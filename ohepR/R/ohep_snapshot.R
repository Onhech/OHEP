#' Prepare OHEP Analytic Snapshot
#'
#' Runs the analytics prep stage once per refresh and returns reusable derived
#' marts used by render functions.
#'
#' @param raw_user_data Respondent-level survey data with at least `company`
#'   and `year` columns plus item response columns.
#' @param index_data Named list with `item_data` and optionally `user_data_key`.
#' @param predictive_data Optional predictive edges table.
#' @param snapshot_id Snapshot identifier. Defaults to UTC timestamp.
#'
#' @return Named list of derived marts and snapshot metadata.
#' @export
prep_ohep_snapshot <- function(
  raw_user_data,
  index_data,
  predictive_data = NULL,
  snapshot_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
) {
  if (!is.data.frame(raw_user_data)) {
    stop("`raw_user_data` must be a data frame.", call. = FALSE)
  }
  if (!is.list(index_data) || !is.data.frame(index_data$item_data)) {
    stop("`index_data$item_data` is required.", call. = FALSE)
  }
  if (length(setdiff(c("company", "year"), names(raw_user_data))) > 0) {
    stop("`raw_user_data` must include `company` and `year`.", call. = FALSE)
  }

  item_meta <- index_data$item_data
  needed_item_cols <- c(
    "item_id", "item_text", "fundamental_id", "facet_id",
    "is_reverse_scored", "response_scale_min", "response_scale_max"
  )
  missing_item_cols <- setdiff(needed_item_cols, names(item_meta))
  if (length(missing_item_cols) > 0) {
    stop(
      sprintf("`index_data$item_data` missing: %s.", paste(missing_item_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  item_meta <- item_meta[item_meta$item_id %in% names(raw_user_data), , drop = FALSE]
  if (nrow(item_meta) < 1L) {
    stop("No `item_id` columns from `index_data$item_data` found in `raw_user_data`.", call. = FALSE)
  }

  item_meta$item_id <- as.character(item_meta$item_id)
  item_meta$item_text <- as.character(item_meta$item_text)
  item_meta$fundamental_id <- as.character(item_meta$fundamental_id)
  item_meta$facet_id <- as.character(item_meta$facet_id)
  item_meta$is_reverse_scored <- as.logical(item_meta$is_reverse_scored)
  item_meta$response_scale_min <- suppressWarnings(as.numeric(item_meta$response_scale_min))
  item_meta$response_scale_max <- suppressWarnings(as.numeric(item_meta$response_scale_max))

  item_order <- suppressWarnings(as.integer(sub(".*_(\\d+)$", "\\1", item_meta$item_id)))
  item_order[is.na(item_order)] <- seq_len(sum(is.na(item_order)))
  item_meta$item_order <- item_order

  fundamental_levels <- unique(item_meta$fundamental_id)
  facet_levels <- unique(item_meta$facet_id)
  item_meta$fundamental_order <- match(item_meta$fundamental_id, fundamental_levels)
  item_meta$facet_order <- match(item_meta$facet_id, facet_levels)

  score_value <- function(value, min_scale, max_scale, reverse) {
    val <- suppressWarnings(as.numeric(value))
    if (is.na(val)) {
      return(NA_real_)
    }
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }
    if (isTRUE(reverse)) {
      return(min_scale + max_scale - val)
    }
    val
  }

  classify_sentiment <- function(value, min_scale, max_scale) {
    if (is.na(value)) {
      return(NA_character_)
    }
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }
    first_cut <- min_scale + (max_scale - min_scale) / 3
    second_cut <- min_scale + 2 * (max_scale - min_scale) / 3
    if (value <= first_cut) {
      "disagree"
    } else if (value >= second_cut) {
      "agree"
    } else {
      "neutral"
    }
  }

  long_rows <- vector("list", nrow(item_meta))
  for (i in seq_len(nrow(item_meta))) {
    item_id <- item_meta$item_id[i]
    raw_vals <- raw_user_data[[item_id]]
    scored <- vapply(
      raw_vals,
      score_value,
      numeric(1),
      min_scale = item_meta$response_scale_min[i],
      max_scale = item_meta$response_scale_max[i],
      reverse = item_meta$is_reverse_scored[i]
    )
    sentiment <- vapply(
      scored,
      classify_sentiment,
      character(1),
      min_scale = item_meta$response_scale_min[i],
      max_scale = item_meta$response_scale_max[i]
    )
    long_rows[[i]] <- data.frame(
      company = as.character(raw_user_data$company),
      year = suppressWarnings(as.integer(raw_user_data$year)),
      item_id = item_id,
      score = scored,
      sentiment = sentiment,
      stringsAsFactors = FALSE
    )
  }
  long_df <- do.call(rbind, long_rows)
  long_df <- long_df[!is.na(long_df$year), , drop = FALSE]
  long_df <- long_df[!is.na(long_df$company) & long_df$company != "", , drop = FALSE]

  agg_item <- stats::aggregate(
    score ~ company + year + item_id,
    data = long_df,
    FUN = function(x) c(mean = mean(x, na.rm = TRUE), n = sum(!is.na(x)))
  )
  agg_item <- cbind(
    agg_item[c("company", "year", "item_id")],
    as.data.frame(agg_item$score, stringsAsFactors = FALSE)
  )
  names(agg_item)[names(agg_item) == "mean"] <- "mean"
  names(agg_item)[names(agg_item) == "n"] <- "n"
  agg_item$mean <- as.numeric(agg_item$mean)
  agg_item$n <- as.numeric(agg_item$n)

  sent_pct <- function(group_df) {
    vals <- group_df$sentiment
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) {
      return(c(disagree_pct = 0, neutral_pct = 100, agree_pct = 0))
    }
    disagree <- round(100 * sum(vals == "disagree") / length(vals))
    neutral <- round(100 * sum(vals == "neutral") / length(vals))
    agree <- round(100 * sum(vals == "agree") / length(vals))
    diff <- 100 - (disagree + neutral + agree)
    max_ix <- which.max(c(disagree, neutral, agree))
    if (max_ix == 1L) disagree <- disagree + diff
    if (max_ix == 2L) neutral <- neutral + diff
    if (max_ix == 3L) agree <- agree + diff
    c(disagree_pct = disagree, neutral_pct = neutral, agree_pct = agree)
  }

  split_key <- interaction(long_df$company, long_df$year, long_df$item_id, drop = TRUE)
  sent_by_group <- lapply(split(long_df, split_key), sent_pct)
  sent_mat <- do.call(rbind, sent_by_group)
  sent_keys <- names(sent_by_group)
  sent_parts <- do.call(rbind, strsplit(sent_keys, "\\."))
  sent_df <- data.frame(
    company = sent_parts[, 1],
    year = as.integer(sent_parts[, 2]),
    item_id = sent_parts[, 3],
    disagree_pct = as.numeric(sent_mat[, "disagree_pct"]),
    neutral_pct = as.numeric(sent_mat[, "neutral_pct"]),
    agree_pct = as.numeric(sent_mat[, "agree_pct"]),
    stringsAsFactors = FALSE
  )

  company_item_year <- merge(
    agg_item,
    sent_df,
    by = c("company", "year", "item_id"),
    all.x = TRUE,
    sort = FALSE
  )

  benchmark_item_year <- stats::aggregate(
    score ~ year + item_id,
    data = long_df,
    FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE), n = sum(!is.na(x)))
  )
  benchmark_item_year <- cbind(
    benchmark_item_year[c("year", "item_id")],
    as.data.frame(benchmark_item_year$score, stringsAsFactors = FALSE)
  )
  names(benchmark_item_year)[names(benchmark_item_year) == "mean"] <- "mean"
  names(benchmark_item_year)[names(benchmark_item_year) == "sd"] <- "sd"
  names(benchmark_item_year)[names(benchmark_item_year) == "n"] <- "n"
  benchmark_item_year$mean <- as.numeric(benchmark_item_year$mean)
  benchmark_item_year$sd <- as.numeric(benchmark_item_year$sd)
  benchmark_item_year$n <- as.numeric(benchmark_item_year$n)

  bench_join <- benchmark_item_year
  names(bench_join)[names(bench_join) == "mean"] <- "benchmark_mean"
  names(bench_join)[names(bench_join) == "sd"] <- "benchmark_sd"

  company_item_year <- merge(
    company_item_year,
    bench_join[c("year", "item_id", "benchmark_mean", "benchmark_sd")],
    by = c("year", "item_id"),
    all.x = TRUE,
    sort = FALSE
  )
  company_item_year$z_score <- with(
    company_item_year,
    ifelse(is.na(benchmark_sd) | benchmark_sd <= 0, 0, (mean - benchmark_mean) / benchmark_sd)
  )
  company_item_year$percentile <- stats::pnorm(company_item_year$z_score) * 100
  company_item_year$vs_benchmark_raw <- company_item_year$mean - company_item_year$benchmark_mean
  company_item_year$vs_benchmark_pctile <- company_item_year$percentile - 50

  company_year_avg <- stats::aggregate(
    mean ~ company + year,
    data = company_item_year,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  names(company_year_avg)[names(company_year_avg) == "mean"] <- "company_year_mean"
  company_item_year <- merge(
    company_item_year,
    company_year_avg,
    by = c("company", "year"),
    all.x = TRUE,
    sort = FALSE
  )
  company_item_year$vs_company_avg_raw <- company_item_year$mean - company_item_year$company_year_mean

  calc_prior_deltas <- function(df, value_col, group_cols) {
    df <- df[order(df$company, df[[group_cols]], df$year), , drop = FALSE]
    key <- paste(df$company, df[[group_cols]], sep = "||")
    df$prior_value <- stats::ave(df[[value_col]], key, FUN = function(v) c(NA, v[-length(v)]))
    df[[paste0("vs_prior_", value_col)]] <- df[[value_col]] - df$prior_value
    df
  }

  company_item_year <- calc_prior_deltas(company_item_year, "mean", "item_id")
  company_item_year <- calc_prior_deltas(company_item_year, "percentile", "item_id")
  company_item_year$vs_prior_raw <- company_item_year$vs_prior_mean
  company_item_year$vs_prior_pctile <- company_item_year$vs_prior_percentile
  company_item_year$vs_prior_raw[is.na(company_item_year$vs_prior_raw)] <- 0
  company_item_year$vs_prior_pctile[is.na(company_item_year$vs_prior_pctile)] <- 0
  company_item_year <- company_item_year[, !names(company_item_year) %in% c(
    "prior_value", "vs_prior_mean", "vs_prior_percentile", "company_year_mean"
  ), drop = FALSE]

  meta_small <- item_meta[, c("item_id", "fundamental_id", "facet_id"), drop = FALSE]
  company_item_enriched <- merge(company_item_year, meta_small, by = "item_id", all.x = TRUE, sort = FALSE)
  benchmark_item_enriched <- merge(benchmark_item_year, meta_small, by = "item_id", all.x = TRUE, sort = FALSE)

  aggregate_group <- function(df, by_col, value_col = "mean", weight_col = "n") {
    split_df <- split(df, interaction(df$company, df$year, df[[by_col]], drop = TRUE))
    rows <- lapply(split_df, function(g) {
      g <- g[!is.na(g[[value_col]]) & !is.na(g[[weight_col]]), , drop = FALSE]
      if (nrow(g) == 0L) {
        return(NULL)
      }
      out <- data.frame(
        company = as.character(g$company[1]),
        year = as.integer(g$year[1]),
        key = as.character(g[[by_col]][1]),
        mean = stats::weighted.mean(g[[value_col]], w = pmax(g[[weight_col]], 1), na.rm = TRUE),
        n = sum(pmax(g[[weight_col]], 0), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      out
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0L) {
      return(data.frame())
    }
    out <- do.call(rbind, rows)
    names(out)[names(out) == "key"] <- by_col
    out
  }

  aggregate_benchmark_group <- function(df, by_col) {
    split_df <- split(df, interaction(df$year, df[[by_col]], drop = TRUE))
    rows <- lapply(split_df, function(g) {
      g <- g[!is.na(g$mean) & !is.na(g$n), , drop = FALSE]
      if (nrow(g) == 0L) {
        return(NULL)
      }
      m <- stats::weighted.mean(g$mean, w = pmax(g$n, 1), na.rm = TRUE)
      s <- stats::sd(g$mean, na.rm = TRUE)
      out <- data.frame(
        year = as.integer(g$year[1]),
        key = as.character(g[[by_col]][1]),
        mean = m,
        sd = ifelse(is.na(s) | s == 0, 1e-9, s),
        n = sum(pmax(g$n, 0), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      out
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0L) {
      return(data.frame())
    }
    out <- do.call(rbind, rows)
    names(out)[names(out) == "key"] <- by_col
    out
  }

  add_group_metrics <- function(company_group, benchmark_group, by_col) {
    names(benchmark_group)[names(benchmark_group) == "mean"] <- "benchmark_mean"
    names(benchmark_group)[names(benchmark_group) == "sd"] <- "benchmark_sd"
    out <- merge(company_group, benchmark_group, by = c("year", by_col), all.x = TRUE, sort = FALSE)
    if ("n.x" %in% names(out)) {
      out$n <- as.numeric(out$n.x)
      out$n.x <- NULL
    }
    if ("n.y" %in% names(out)) {
      out$n.y <- NULL
    }
    out$z_score <- with(out, ifelse(is.na(benchmark_sd) | benchmark_sd <= 0, 0, (mean - benchmark_mean) / benchmark_sd))
    out$percentile <- stats::pnorm(out$z_score) * 100
    out$vs_benchmark_raw <- out$mean - out$benchmark_mean
    out$vs_benchmark_pctile <- out$percentile - 50

    out <- out[order(out$company, out[[by_col]], out$year), , drop = FALSE]
    key <- paste(out$company, out[[by_col]], sep = "||")
    out$prior_mean <- stats::ave(out$mean, key, FUN = function(v) c(NA, v[-length(v)]))
    out$prior_pct <- stats::ave(out$percentile, key, FUN = function(v) c(NA, v[-length(v)]))
    out$vs_prior_raw <- out$mean - out$prior_mean
    out$vs_prior_pctile <- out$percentile - out$prior_pct
    out$vs_prior_raw[is.na(out$vs_prior_raw)] <- 0
    out$vs_prior_pctile[is.na(out$vs_prior_pctile)] <- 0
    out <- out[, !names(out) %in% c("prior_mean", "prior_pct"), drop = FALSE]
    out <- out[, c(
      "company", "year", by_col, "mean", "n",
      "benchmark_mean", "benchmark_sd", "z_score", "percentile",
      "vs_benchmark_raw", "vs_benchmark_pctile",
      "vs_prior_raw", "vs_prior_pctile"
    ), drop = FALSE]
    out
  }

  company_fundamental_year <- add_group_metrics(
    company_group = aggregate_group(company_item_enriched, "fundamental_id"),
    benchmark_group = aggregate_benchmark_group(benchmark_item_enriched, "fundamental_id"),
    by_col = "fundamental_id"
  )
  company_fundamental_year$status_label <- ifelse(
    company_fundamental_year$percentile < 40, "Area for Growth",
    ifelse(
      company_fundamental_year$percentile < 60, "Industry Standard",
      ifelse(company_fundamental_year$percentile < 85, "Above Standard", "Industry Leader")
    )
  )

  company_facet_year <- add_group_metrics(
    company_group = aggregate_group(company_item_enriched, "facet_id"),
    benchmark_group = aggregate_benchmark_group(benchmark_item_enriched, "facet_id"),
    by_col = "facet_id"
  )
  company_facet_year$status_label <- ifelse(
    company_facet_year$percentile < 40, "Area for Growth",
    ifelse(
      company_facet_year$percentile < 60, "Industry Standard",
      ifelse(company_facet_year$percentile < 85, "Above Standard", "Industry Leader")
    )
  )

  predictive_edges <- data.frame()
  if (is.data.frame(predictive_data) && nrow(predictive_data) > 0L) {
    nm <- names(predictive_data)
    lower_nm <- tolower(nm)
    names(predictive_data) <- lower_nm
    required <- c("fundamental", "outcome", "strength")
    if (length(setdiff(required, names(predictive_data))) > 0) {
      stop("`predictive_data` must include at least Fundamental, Outcome, and strength.", call. = FALSE)
    }
    if (!"subset" %in% names(predictive_data)) predictive_data$subset <- "All"
    if (!"type" %in% names(predictive_data)) predictive_data$type <- NA_character_
    if (!"direction" %in% names(predictive_data)) predictive_data$direction <- NA_character_
    if (!"significant" %in% names(predictive_data)) predictive_data$significant <- NA
    if (!"n" %in% names(predictive_data)) predictive_data$n <- NA_real_
    predictive_edges <- predictive_data[, c(
      "fundamental", "outcome", "subset", "strength",
      "type", "direction", "significant", "n"
    ), drop = FALSE]
    predictive_edges$fundamental <- as.character(predictive_edges$fundamental)
    predictive_edges$outcome <- as.character(predictive_edges$outcome)
    predictive_edges$subset <- as.character(predictive_edges$subset)
    predictive_edges$strength <- suppressWarnings(as.numeric(predictive_edges$strength))
    predictive_edges$type <- as.character(predictive_edges$type)
    predictive_edges$direction <- as.character(predictive_edges$direction)
    predictive_edges$significant <- as.logical(predictive_edges$significant)
    predictive_edges$n <- suppressWarnings(as.numeric(predictive_edges$n))
  }

  metadata_lookup <- item_meta[, c(
    "item_id", "item_text", "fundamental_id", "facet_id",
    "is_reverse_scored", "response_scale_min", "response_scale_max",
    "item_order", "facet_order", "fundamental_order"
  ), drop = FALSE]

  list(
    snapshot_meta = data.frame(
      snapshot_id = as.character(snapshot_id),
      generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      stringsAsFactors = FALSE
    ),
    company_item_year = company_item_year,
    company_fundamental_year = company_fundamental_year,
    company_facet_year = company_facet_year,
    benchmark_item_year = benchmark_item_year,
    predictive_edges = predictive_edges,
    metadata_lookup = metadata_lookup
  )
}

#' Persist OHEP Snapshot
#'
#' @param snapshot Output of [prep_ohep_snapshot()].
#' @param path Directory where snapshot artifacts will be written.
#' @param format Storage format: `rds` or `csv`.
#'
#' @return Invisibly returns `path`.
#' @export
write_ohep_snapshot <- function(snapshot, path, format = c("rds", "csv")) {
  format <- match.arg(format)
  if (!is.list(snapshot)) {
    stop("`snapshot` must be a named list.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  if (format == "rds") {
    saveRDS(snapshot, file = file.path(path, "ohep_snapshot.rds"))
  } else {
    for (nm in names(snapshot)) {
      obj <- snapshot[[nm]]
      if (is.data.frame(obj) && ncol(obj) > 0L) {
        utils::write.csv(obj, file = file.path(path, paste0(nm, ".csv")), row.names = FALSE)
      }
    }
  }

  invisible(path)
}

#' Load OHEP Snapshot
#'
#' @param path_or_tables Either a directory path containing snapshot files or
#'   an already-loaded named list of snapshot tables.
#'
#' @return Named list of snapshot tables.
#' @export
load_ohep_snapshot <- function(path_or_tables) {
  if (is.list(path_or_tables)) {
    return(path_or_tables)
  }
  if (!is.character(path_or_tables) || length(path_or_tables) != 1L) {
    stop("`path_or_tables` must be a single directory path or named list.", call. = FALSE)
  }
  path <- path_or_tables[[1]]
  rds_path <- file.path(path, "ohep_snapshot.rds")
  if (file.exists(rds_path)) {
    return(readRDS(rds_path))
  }

  required <- c(
    "company_item_year",
    "company_fundamental_year",
    "company_facet_year",
    "benchmark_item_year",
    "metadata_lookup"
  )
  out <- list()
  for (nm in required) {
    csv_path <- file.path(path, paste0(nm, ".csv"))
    if (!file.exists(csv_path)) {
      stop(sprintf("Missing snapshot table: %s.", nm), call. = FALSE)
    }
    out[[nm]] <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  pred_path <- file.path(path, "predictive_edges.csv")
  if (file.exists(pred_path)) {
    out$predictive_edges <- tryCatch(
      utils::read.csv(pred_path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) data.frame()
    )
  } else {
    out$predictive_edges <- data.frame()
  }
  meta_path <- file.path(path, "snapshot_meta.csv")
  if (file.exists(meta_path)) {
    out$snapshot_meta <- utils::read.csv(meta_path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  out
}

build_dashboard_data_from_marts <- function(company, year, fundamental, marts) {
  needed <- c("company_item_year", "company_fundamental_year", "metadata_lookup")
  missing_needed <- setdiff(needed, names(marts))
  if (length(missing_needed) > 0) {
    stop(sprintf("`marts` missing required table(s): %s.", paste(missing_needed, collapse = ", ")), call. = FALSE)
  }

  cfy <- marts$company_fundamental_year
  if (!all(c("company", "year", "fundamental_id") %in% names(cfy))) {
    stop("`marts$company_fundamental_year` missing required keys.", call. = FALSE)
  }
  row_f <- cfy[
    as.character(cfy$company) == as.character(company) &
      suppressWarnings(as.integer(cfy$year)) == suppressWarnings(as.integer(year)) &
      as.character(cfy$fundamental_id) == as.character(fundamental),
    ,
    drop = FALSE
  ]
  if (nrow(row_f) != 1L) {
    stop("Expected exactly one row in `company_fundamental_year` for company/year/fundamental.", call. = FALSE)
  }

  delta_label <- paste0("vs. ", as.integer(year) - 1L)
  fundamental_df <- data.frame(
    percentile = as.numeric(row_f$percentile[1]),
    percentile_delta = as.numeric(round(row_f$vs_prior_pctile[1])),
    delta_label = delta_label,
    score = as.numeric(round(row_f$mean[1], 2)),
    score_delta = as.numeric(round(row_f$vs_prior_raw[1], 2)),
    stringsAsFactors = FALSE
  )

  pred <- marts$predictive_edges
  outcome_df <- data.frame(rank = integer(0), outcome = character(0), percentile = numeric(0), stringsAsFactors = FALSE)
  if (is.data.frame(pred) && nrow(pred) > 0L && all(c("fundamental", "outcome", "strength") %in% names(pred))) {
    p <- pred[as.character(pred$fundamental) == as.character(fundamental), , drop = FALSE]
    if ("subset" %in% names(p)) {
      p <- p[is.na(p$subset) | as.character(p$subset) %in% c("All", "all"), , drop = FALSE]
    }
    if ("significant" %in% names(p)) {
      keep <- is.na(p$significant) | as.logical(p$significant)
      p <- p[keep, , drop = FALSE]
    }
    p <- p[!as.character(p$outcome) %in% c("Outcomes", "eNPS"), , drop = FALSE]
    if (nrow(p) > 0L) {
      p <- p[order(abs(suppressWarnings(as.numeric(p$strength))), decreasing = TRUE), , drop = FALSE]
      p <- utils::head(p, 3L)
      outcome_df <- data.frame(
        rank = seq_len(nrow(p)),
        outcome = as.character(p$outcome),
        percentile = pmax(0, pmin(100, round(abs(suppressWarnings(as.numeric(p$strength))) * 100))),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(outcome_df) < 1L) {
    outcome_df <- data.frame(
      rank = c(1, 2, 3),
      outcome = c("Turnover Intentions", "Work Satisfaction", "Engagement"),
      percentile = c(40, 55, 60),
      stringsAsFactors = FALSE
    )
  }

  ciy <- marts$company_item_year
  lookup <- marts$metadata_lookup
  item_rows <- ciy[
    as.character(ciy$company) == as.character(company) &
      suppressWarnings(as.integer(ciy$year)) == suppressWarnings(as.integer(year)),
    ,
    drop = FALSE
  ]
  item_rows <- merge(
    item_rows,
    lookup[, c("item_id", "item_text", "fundamental_id", "facet_id", "item_order", "facet_order"), drop = FALSE],
    by = "item_id",
    all.x = TRUE,
    sort = FALSE
  )
  item_rows <- item_rows[as.character(item_rows$fundamental_id) == as.character(fundamental), , drop = FALSE]
  if (nrow(item_rows) < 1L) {
    stop("No item rows found in marts for selected company/year/fundamental.", call. = FALSE)
  }
  item_rows <- item_rows[order(item_rows$facet_order, item_rows$item_order), , drop = FALSE]
  item_rows <- utils::head(item_rows, 8L)

  left_n <- ceiling(nrow(item_rows) / 2)
  item_rows$column <- c(rep("left", left_n), rep("right", nrow(item_rows) - left_n))
  item_rows$section <- ifelse(
    is.na(item_rows$facet_id) | item_rows$facet_id == "",
    as.character(fundamental),
    as.character(item_rows$facet_id)
  )
  item_rows$section_order <- as.numeric(item_rows$facet_order)
  item_rows$item_order <- stats::ave(seq_len(nrow(item_rows)), item_rows$column, FUN = seq_along)
  item_rows$label <- as.character(item_rows$item_text)
  item_rows$mean <- as.numeric(round(item_rows$mean, 2))
  item_rows$disagree_pct <- as.numeric(round(item_rows$disagree_pct))
  item_rows$neutral_pct <- as.numeric(round(item_rows$neutral_pct))
  item_rows$agree_pct <- as.numeric(round(item_rows$agree_pct))
  item_rows$vs_industry <- as.numeric(round(item_rows$vs_benchmark_pctile))
  item_rows$vs_prior <- as.numeric(round(item_rows$vs_prior_pctile))

  list(
    fundamental = fundamental_df,
    outcomes = outcome_df[, c("rank", "outcome", "percentile"), drop = FALSE],
    items = item_rows[, c(
      "column", "section", "section_order", "item_order", "label", "mean",
      "disagree_pct", "neutral_pct", "agree_pct", "vs_industry", "vs_prior"
    ), drop = FALSE]
  )
}

build_dashboard_data_from_filtered_user_data <- function(
  company,
  year,
  fundamental,
  marts,
  filtered_user_data,
  min_n = 3,
  privacy_message = "Not enough responses to display this view."
) {
  if (!is.data.frame(filtered_user_data)) {
    stop("`filtered_user_data` must be a data frame.", call. = FALSE)
  }
  if (length(setdiff(c("company", "year"), names(filtered_user_data))) > 0) {
    stop("`filtered_user_data` must include `company` and `year`.", call. = FALSE)
  }
  needed <- c("benchmark_item_year", "metadata_lookup", "company_item_year")
  missing_needed <- setdiff(needed, names(marts))
  if (length(missing_needed) > 0) {
    stop(sprintf("`marts` missing required table(s): %s.", paste(missing_needed, collapse = ", ")), call. = FALSE)
  }

  lookup <- marts$metadata_lookup
  bmk <- marts$benchmark_item_year
  ciy <- marts$company_item_year
  pred <- marts$predictive_edges

  item_meta <- lookup[as.character(lookup$fundamental_id) == as.character(fundamental), , drop = FALSE]
  if (nrow(item_meta) < 1L) {
    stop("No items found in metadata for selected fundamental.", call. = FALSE)
  }
  item_meta <- item_meta[item_meta$item_id %in% names(filtered_user_data), , drop = FALSE]
  if (nrow(item_meta) < 1L) {
    stop("No matching item columns found in `filtered_user_data` for selected fundamental.", call. = FALSE)
  }
  item_meta <- item_meta[order(item_meta$facet_order, item_meta$item_order), , drop = FALSE]
  item_meta <- utils::head(item_meta, 8L)
  company_item_baseline <- ciy[
    as.character(ciy$company) == as.character(company) &
      suppressWarnings(as.integer(ciy$year)) == as.integer(year) &
      as.character(ciy$item_id) %in% as.character(item_meta$item_id),
    c("item_id", "mean"),
    drop = FALSE
  ]
  company_item_baseline <- company_item_baseline[!duplicated(as.character(company_item_baseline$item_id)), , drop = FALSE]
  names(company_item_baseline)[names(company_item_baseline) == "mean"] <- "company_current_mean"

  score_value <- function(value, min_scale, max_scale, reverse) {
    val <- suppressWarnings(as.numeric(value))
    if (is.na(val)) {
      return(NA_real_)
    }
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }
    if (isTRUE(reverse)) {
      return(min_scale + max_scale - val)
    }
    val
  }

  classify_sentiment <- function(value, min_scale, max_scale) {
    if (is.na(value)) {
      return(NA_character_)
    }
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }
    first_cut <- min_scale + (max_scale - min_scale) / 3
    second_cut <- min_scale + 2 * (max_scale - min_scale) / 3
    if (value <= first_cut) {
      "disagree"
    } else if (value >= second_cut) {
      "agree"
    } else {
      "neutral"
    }
  }

  user_df <- filtered_user_data
  user_df$year <- suppressWarnings(as.integer(user_df$year))
  rows_company <- user_df[as.character(user_df$company) == as.character(company), , drop = FALSE]
  if (nrow(rows_company) < 1L) {
    stop("No rows found in `filtered_user_data` for selected company.", call. = FALSE)
  }
  rows_current <- rows_company[rows_company$year == as.integer(year), , drop = FALSE]
  if (nrow(rows_current) < 1L) {
    stop("No rows found in `filtered_user_data` for selected company/year.", call. = FALSE)
  }
  current_n <- nrow(rows_current)
  if (current_n < as.integer(min_n)) {
    fallback <- build_dashboard_data_from_marts(
      company = company,
      year = as.integer(year),
      fundamental = fundamental,
      marts = marts
    )
    fallback$privacy <- list(
      suppress_page = TRUE,
      current_n = as.integer(current_n),
      min_n = as.integer(min_n),
      message = as.character(privacy_message)
    )
    return(fallback)
  }
  prior_year <- suppressWarnings(max(rows_company$year[rows_company$year < as.integer(year)], na.rm = TRUE))
  has_prior <- is.finite(prior_year)
  rows_prior <- if (has_prior) rows_company[rows_company$year == as.integer(prior_year), , drop = FALSE] else rows_current[0, , drop = FALSE]

  item_metrics <- lapply(seq_len(nrow(item_meta)), function(i) {
    item_id <- as.character(item_meta$item_id[i])
    min_scale <- suppressWarnings(as.numeric(item_meta$response_scale_min[i]))
    max_scale <- suppressWarnings(as.numeric(item_meta$response_scale_max[i]))
    reverse <- as.logical(item_meta$is_reverse_scored[i])

    current_scored <- vapply(rows_current[[item_id]], score_value, numeric(1), min_scale = min_scale, max_scale = max_scale, reverse = reverse)
    current_scored <- current_scored[!is.na(current_scored)]
    if (length(current_scored) < 1L) {
      current_mean <- NA_real_
      current_n <- 0
      disagree <- 0
      neutral <- 100
      agree <- 0
    } else {
      current_mean <- mean(current_scored, na.rm = TRUE)
      current_n <- length(current_scored)
      sent <- vapply(current_scored, classify_sentiment, character(1), min_scale = min_scale, max_scale = max_scale)
      disagree <- round(100 * sum(sent == "disagree") / length(sent))
      neutral <- round(100 * sum(sent == "neutral") / length(sent))
      agree <- round(100 * sum(sent == "agree") / length(sent))
      diff <- 100 - (disagree + neutral + agree)
      max_ix <- which.max(c(disagree, neutral, agree))
      if (max_ix == 1L) disagree <- disagree + diff
      if (max_ix == 2L) neutral <- neutral + diff
      if (max_ix == 3L) agree <- agree + diff
    }

    prior_scored <- if (has_prior) {
      vapply(rows_prior[[item_id]], score_value, numeric(1), min_scale = min_scale, max_scale = max_scale, reverse = reverse)
    } else {
      numeric(0)
    }
    prior_scored <- prior_scored[!is.na(prior_scored)]
    prior_mean <- if (length(prior_scored) > 0L) mean(prior_scored, na.rm = TRUE) else NA_real_
    prior_n <- length(prior_scored)
    company_mean <- NA_real_
    ix_company <- match(item_id, as.character(company_item_baseline$item_id))
    if (!is.na(ix_company)) {
      company_mean <- suppressWarnings(as.numeric(company_item_baseline$company_current_mean[[ix_company]]))
    }

    b_now <- bmk[bmk$item_id == item_id & suppressWarnings(as.integer(bmk$year)) == as.integer(year), , drop = FALSE]
    b_prev <- if (has_prior) bmk[bmk$item_id == item_id & suppressWarnings(as.integer(bmk$year)) == as.integer(prior_year), , drop = FALSE] else b_now[0, , drop = FALSE]
    bm_mean <- if (nrow(b_now) > 0) suppressWarnings(as.numeric(b_now$mean[1])) else NA_real_
    bm_sd <- if (nrow(b_now) > 0) suppressWarnings(as.numeric(b_now$sd[1])) else NA_real_
    bm_prev_mean <- if (nrow(b_prev) > 0) suppressWarnings(as.numeric(b_prev$mean[1])) else NA_real_
    bm_prev_sd <- if (nrow(b_prev) > 0) suppressWarnings(as.numeric(b_prev$sd[1])) else NA_real_
    if (!is.finite(bm_sd) || bm_sd <= 0) bm_sd <- 1e-9
    if (!is.finite(bm_prev_sd) || bm_prev_sd <= 0) bm_prev_sd <- 1e-9

    current_pctile <- if (is.na(current_mean) || is.na(bm_mean)) 50 else stats::pnorm((current_mean - bm_mean) / bm_sd) * 100
    prior_pctile <- if (!has_prior || is.na(prior_mean) || is.na(bm_prev_mean)) 50 else stats::pnorm((prior_mean - bm_prev_mean) / bm_prev_sd) * 100

    data.frame(
      item_id = item_id,
      mean = current_mean,
      n = current_n,
      disagree_pct = disagree,
      neutral_pct = neutral,
      agree_pct = agree,
      benchmark_mean = bm_mean,
      benchmark_sd = bm_sd,
      percentile = current_pctile,
      vs_prior_pctile = current_pctile - prior_pctile,
      item_n = as.integer(current_n),
      prior_n = as.integer(prior_n),
      suppress_row = as.logical(current_n < as.integer(min_n)),
      vs_company = as.numeric(ifelse(is.na(current_mean) || is.na(company_mean), NA_real_, round((current_mean - company_mean) * 100))),
      suppress_vs_industry = as.logical(current_n < as.integer(min_n)),
      suppress_vs_prior = as.logical(prior_n < as.integer(min_n)),
      suppress_vs_company = as.logical(current_n < as.integer(min_n) | is.na(company_mean) | is.na(current_mean)),
      min_n = as.integer(min_n),
      stringsAsFactors = FALSE
    )
  })
  item_df <- do.call(rbind, item_metrics)
  if (all(is.na(item_df$mean))) {
    stop("Selected respondent cut has no valid item responses for this fundamental.", call. = FALSE)
  }

  current_score <- mean(item_df$mean, na.rm = TRUE)
  prior_score <- NA_real_
  if (has_prior) {
    prior_vals <- lapply(seq_len(nrow(item_meta)), function(i) {
      item_id <- as.character(item_meta$item_id[i])
      min_scale <- suppressWarnings(as.numeric(item_meta$response_scale_min[i]))
      max_scale <- suppressWarnings(as.numeric(item_meta$response_scale_max[i]))
      reverse <- as.logical(item_meta$is_reverse_scored[i])
      vals <- vapply(rows_prior[[item_id]], score_value, numeric(1), min_scale = min_scale, max_scale = max_scale, reverse = reverse)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        NA_real_
      } else {
        mean(vals, na.rm = TRUE)
      }
    })
    prior_score <- mean(unlist(prior_vals), na.rm = TRUE)
    if (!is.finite(prior_score)) prior_score <- NA_real_
  }

  bmk_f_now <- bmk[bmk$item_id %in% item_df$item_id & suppressWarnings(as.integer(bmk$year)) == as.integer(year), , drop = FALSE]
  bmk_f_mean <- suppressWarnings(mean(as.numeric(bmk_f_now$mean), na.rm = TRUE))
  bmk_f_sd <- suppressWarnings(stats::sd(as.numeric(bmk_f_now$mean), na.rm = TRUE))
  if (!is.finite(bmk_f_sd) || bmk_f_sd <= 0) bmk_f_sd <- 1e-9
  fundamental_pctile <- if (is.na(current_score) || !is.finite(bmk_f_mean)) 50 else stats::pnorm((current_score - bmk_f_mean) / bmk_f_sd) * 100

  prior_pctile <- 50
  if (has_prior) {
    bmk_f_prev <- bmk[bmk$item_id %in% item_df$item_id & suppressWarnings(as.integer(bmk$year)) == as.integer(prior_year), , drop = FALSE]
    bmk_prev_mean <- suppressWarnings(mean(as.numeric(bmk_f_prev$mean), na.rm = TRUE))
    bmk_prev_sd <- suppressWarnings(stats::sd(as.numeric(bmk_f_prev$mean), na.rm = TRUE))
    if (!is.finite(bmk_prev_sd) || bmk_prev_sd <= 0) bmk_prev_sd <- 1e-9
    if (!is.na(prior_score) && is.finite(bmk_prev_mean)) {
      prior_pctile <- stats::pnorm((prior_score - bmk_prev_mean) / bmk_prev_sd) * 100
    }
  }

  fundamental_df <- data.frame(
    percentile = as.numeric(fundamental_pctile),
    percentile_delta = as.numeric(round(fundamental_pctile - prior_pctile)),
    delta_label = paste0("vs. ", ifelse(has_prior, as.integer(prior_year), as.integer(year) - 1L)),
    score = as.numeric(round(current_score, 2)),
    score_delta = as.numeric(round(ifelse(is.na(prior_score), 0, current_score - prior_score), 2)),
    stringsAsFactors = FALSE
  )

  outcome_df <- data.frame(rank = integer(0), outcome = character(0), percentile = numeric(0), stringsAsFactors = FALSE)
  if (is.data.frame(pred) && nrow(pred) > 0L && all(c("fundamental", "outcome", "strength") %in% names(pred))) {
    p <- pred[as.character(pred$fundamental) == as.character(fundamental), , drop = FALSE]
    if ("subset" %in% names(p)) {
      p <- p[is.na(p$subset) | as.character(p$subset) %in% c("All", "all"), , drop = FALSE]
    }
    if ("significant" %in% names(p)) {
      p <- p[is.na(p$significant) | as.logical(p$significant), , drop = FALSE]
    }
    p <- p[!as.character(p$outcome) %in% c("Outcomes", "eNPS"), , drop = FALSE]
    if (nrow(p) > 0L) {
      p <- p[order(abs(suppressWarnings(as.numeric(p$strength))), decreasing = TRUE), , drop = FALSE]
      p <- utils::head(p, 3L)
      outcome_df <- data.frame(
        rank = seq_len(nrow(p)),
        outcome = as.character(p$outcome),
        percentile = pmax(0, pmin(100, round(abs(suppressWarnings(as.numeric(p$strength))) * 100))),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(outcome_df) < 1L) {
    outcome_df <- data.frame(
      rank = c(1, 2, 3),
      outcome = c("Turnover Intentions", "Work Satisfaction", "Engagement"),
      percentile = c(40, 55, 60),
      stringsAsFactors = FALSE
    )
  }

  item_rows <- merge(
    item_df,
    item_meta[, c("item_id", "item_text", "facet_id", "facet_order", "item_order"), drop = FALSE],
    by = "item_id",
    all.x = TRUE,
    sort = FALSE
  )
  item_rows <- item_rows[order(item_rows$facet_order, item_rows$item_order), , drop = FALSE]

  left_n <- ceiling(nrow(item_rows) / 2)
  item_rows$column <- c(rep("left", left_n), rep("right", nrow(item_rows) - left_n))
  item_rows$section <- ifelse(
    is.na(item_rows$facet_id) | item_rows$facet_id == "",
    as.character(fundamental),
    as.character(item_rows$facet_id)
  )
  item_rows$section_order <- as.numeric(item_rows$facet_order)
  item_rows$item_order <- stats::ave(seq_len(nrow(item_rows)), item_rows$column, FUN = seq_along)
  item_rows$label <- as.character(item_rows$item_text)
  item_rows$vs_industry <- as.numeric(round(item_rows$percentile - 50))
  item_rows$vs_prior <- as.numeric(round(item_rows$vs_prior_pctile))

  out <- list(
    fundamental = fundamental_df,
    outcomes = outcome_df[, c("rank", "outcome", "percentile"), drop = FALSE],
    items = item_rows[, c(
      "column", "section", "section_order", "item_order", "label", "mean",
      "disagree_pct", "neutral_pct", "agree_pct", "vs_company", "vs_industry", "vs_prior",
      "item_n", "prior_n", "suppress_row", "suppress_vs_company", "suppress_vs_industry", "suppress_vs_prior", "min_n"
    ), drop = FALSE]
  )
  out$privacy <- list(
    suppress_page = FALSE,
    current_n = as.integer(current_n),
    min_n = as.integer(min_n),
    message = as.character(privacy_message)
  )
  out
}
