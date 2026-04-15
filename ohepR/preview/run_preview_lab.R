`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

choose_default_target <- function(marts, fundamental) {
  fy <- marts$company_fundamental_year
  fy <- fy[as.character(fy$fundamental_id) == as.character(fundamental), , drop = FALSE]
  if (nrow(fy) < 1L) {
    stop(sprintf("No fundamental rows found for '%s'.", fundamental), call. = FALSE)
  }

  has_prior <- vapply(seq_len(nrow(fy)), function(i) {
    any(
      fy$company == fy$company[i] &
        as.integer(fy$year) < as.integer(fy$year[i]),
      na.rm = TRUE
    )
  }, logical(1))

  fy$has_prior <- has_prior
  fy <- fy[order(!fy$has_prior, -suppressWarnings(as.numeric(fy$n))), , drop = FALSE]
  fy <- fy[1, , drop = FALSE]

  list(
    company = fy$company[[1]],
    year = as.integer(fy$year[[1]])
  )
}

sample_n_rows <- function(df, n, seed = 1L) {
  if (nrow(df) <= n) {
    return(df)
  }
  set.seed(seed)
  ix <- sample.int(nrow(df), size = n, replace = FALSE)
  df[ix, , drop = FALSE]
}

build_scenarios <- function(
  user_df,
  item_ids,
  company,
  year,
  prior_year,
  min_n = 3
) {
  rows_company <- user_df[as.character(user_df$company) == as.character(company), , drop = FALSE]
  rows_company$year <- suppressWarnings(as.integer(rows_company$year))
  rows_current <- rows_company[rows_company$year == as.integer(year), , drop = FALSE]
  rows_prior <- if (is.finite(prior_year)) {
    rows_company[rows_company$year == as.integer(prior_year), , drop = FALSE]
  } else {
    rows_company[0, , drop = FALSE]
  }

  if (nrow(rows_current) < 1L) {
    stop("No current-year rows available for selected company/year.", call. = FALSE)
  }

  focus_item <- item_ids[[1]]

  filtered_baseline <- rbind(
    sample_n_rows(rows_current, min(180L, nrow(rows_current)), seed = 101L),
    sample_n_rows(rows_prior, min(180L, nrow(rows_prior)), seed = 102L)
  )

  tiny_subset <- sample_n_rows(rows_current, min(2L, nrow(rows_current)), seed = 103L)

  row_suppressed <- filtered_baseline
  if (focus_item %in% names(row_suppressed)) {
    current_ix <- which(suppressWarnings(as.integer(row_suppressed$year)) == as.integer(year))
    if (length(current_ix) > min_n) {
      keep_ix <- head(current_ix, min_n - 1L)
      mask_ix <- setdiff(current_ix, keep_ix)
      row_suppressed[mask_ix, focus_item] <- NA
    }
  }

  prior_masked <- filtered_baseline
  if (is.finite(prior_year) && focus_item %in% names(prior_masked)) {
    prior_ix <- which(suppressWarnings(as.integer(prior_masked$year)) == as.integer(prior_year))
    if (length(prior_ix) > min_n) {
      keep_ix <- head(prior_ix, min_n - 1L)
      mask_ix <- setdiff(prior_ix, keep_ix)
      prior_masked[mask_ix, focus_item] <- NA
    }
  }

  shifted <- filtered_baseline
  for (item in item_ids) {
    if (!item %in% names(shifted)) {
      next
    }
    vals <- suppressWarnings(as.numeric(shifted[[item]]))
    current_ix <- which(suppressWarnings(as.integer(shifted$year)) == as.integer(year))
    vals[current_ix] <- ifelse(is.na(vals[current_ix]), NA, pmin(5, vals[current_ix] + 1))
    shifted[[item]] <- vals
  }

  list(
    list(
      name = "01_unfiltered_marts_baseline",
      mode = "unfiltered",
      min_n = min_n,
      description = "Mart-based baseline (no respondent filter).",
      filtered_user_data = NULL
    ),
    list(
      name = "02_filtered_baseline",
      mode = "filtered",
      min_n = min_n,
      description = "Filtered respondent-level baseline (current + prior rows).",
      filtered_user_data = filtered_baseline
    ),
    list(
      name = "03_page_suppressed_n_below_min",
      mode = "filtered",
      min_n = min_n,
      description = "Page-level privacy suppression when subset n is below min_n.",
      filtered_user_data = tiny_subset
    ),
    list(
      name = "04_row_suppressed_item_n_below_min",
      mode = "filtered",
      min_n = min_n,
      description = sprintf("Row-level suppression by masking '%s' item responses.", focus_item),
      filtered_user_data = row_suppressed
    ),
    list(
      name = "05_prior_only_suppressed",
      mode = "filtered",
      min_n = min_n,
      description = sprintf("Comparison-only suppression by reducing prior n for '%s'.", focus_item),
      filtered_user_data = prior_masked
    ),
    list(
      name = "06_data_shifted_current_year",
      mode = "filtered",
      min_n = min_n,
      description = "Current-year item responses shifted upward (+1 capped at 5).",
      filtered_user_data = shifted
    ),
    list(
      name = "07_filtered_baseline_min_n_5",
      mode = "filtered",
      min_n = 5L,
      description = "Same filtered baseline but stricter threshold min_n = 5.",
      filtered_user_data = filtered_baseline
    )
  )
}

scenario_summary_row <- function(name, mode, min_n, description, dashboard_data, filtered_user_data) {
  privacy <- dashboard_data$privacy %||% list()
  items <- dashboard_data$items %||% data.frame()

  page_suppressed <- isTRUE(privacy$suppress_page)
  current_n <- if (!is.null(privacy$current_n)) as.integer(privacy$current_n[[1]]) else NA_integer_
  rows_total <- if (is.data.frame(items)) nrow(items) else 0L

  rows_suppressed <- if (is.data.frame(items) && "suppress_row" %in% names(items)) {
    sum(isTRUE(items$suppress_row) | (!is.na(items$suppress_row) & items$suppress_row), na.rm = TRUE)
  } else {
    0L
  }

  vs_ind_suppressed <- if (is.data.frame(items) && "suppress_vs_industry" %in% names(items)) {
    sum(isTRUE(items$suppress_vs_industry) | (!is.na(items$suppress_vs_industry) & items$suppress_vs_industry), na.rm = TRUE)
  } else {
    0L
  }

  vs_prior_suppressed <- if (is.data.frame(items) && "suppress_vs_prior" %in% names(items)) {
    sum(isTRUE(items$suppress_vs_prior) | (!is.na(items$suppress_vs_prior) & items$suppress_vs_prior), na.rm = TRUE)
  } else {
    0L
  }

  filtered_n <- if (is.data.frame(filtered_user_data)) nrow(filtered_user_data) else NA_integer_

  data.frame(
    scenario = name,
    mode = mode,
    min_n = as.integer(min_n),
    filtered_rows = filtered_n,
    current_n = current_n,
    page_suppressed = page_suppressed,
    total_item_rows = as.integer(rows_total),
    suppressed_rows = as.integer(rows_suppressed),
    suppressed_vs_industry = as.integer(vs_ind_suppressed),
    suppressed_vs_prior = as.integer(vs_prior_suppressed),
    description = description,
    stringsAsFactors = FALSE
  )
}

write_index_page <- function(summary_df, out_dir, run_title) {
  html_path <- file.path(out_dir, "index.html")

  rows_html <- apply(summary_df, 1, function(r) {
    scen <- as.character(r[["scenario"]])
    sprintf(
      paste0(
        "<tr>",
        "<td><a href=\"%s.html\">%s</a></td>",
        "<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
        "<td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
        "</tr>"
      ),
      scen,
      scen,
      r[["mode"]],
      r[["min_n"]],
      r[["filtered_rows"]],
      r[["current_n"]],
      r[["page_suppressed"]],
      r[["suppressed_rows"]],
      r[["suppressed_vs_industry"]],
      r[["suppressed_vs_prior"]],
      r[["description"]]
    )
  })

  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", run_title, "</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
    "margin:24px;background:#f8fafc;color:#0f172a}",
    "h1{margin:0 0 8px 0;font-size:24px} p{margin:0 0 16px 0;color:#475569}",
    "table{border-collapse:collapse;width:100%;background:#fff;border:1px solid #e2e8f0}",
    "th,td{padding:8px 10px;border-bottom:1px solid #e2e8f0;font-size:12px;text-align:left;vertical-align:top}",
    "th{font-size:11px;text-transform:uppercase;letter-spacing:.4px;color:#64748b;background:#f8fafc}",
    "a{color:#0f766e;text-decoration:none} a:hover{text-decoration:underline}",
    "code{background:#f1f5f9;padding:2px 6px;border-radius:4px}",
    "</style></head><body>",
    "<h1>", run_title, "</h1>",
    "<p>Generated scenario outputs for quick HTML/privacy/rendering review.</p>",
    "<p><code>scenario_summary.csv</code> and <code>scenario_results.rds</code> are in this folder.</p>",
    "<table><thead><tr>",
    "<th>Scenario</th><th>Mode</th><th>min_n</th><th>Filtered Rows</th><th>Current n</th>",
    "<th>Page Suppressed</th><th>Suppressed Rows</th><th>Supp. vs Industry</th><th>Supp. vs Prior</th><th>Description</th>",
    "</tr></thead><tbody>",
    paste(rows_html, collapse = "\n"),
    "</tbody></table></body></html>"
  )

  writeLines(html, con = html_path, useBytes = TRUE)
  html_path
}

run_preview_lab <- function(
  out_dir = file.path("preview", "test_examples", "Fundamentals"),
  company = NULL,
  year = NULL,
  fundamental = "Purpose",
  min_n = 3,
  privacy_message = "Not enough responses to display this view.",
  id_prefix = "ohep-preview",
  save_dashboard_data = TRUE,
  clean = TRUE
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is required. Install with install.packages('devtools').", call. = FALSE)
  }

  pkg_root <- normalizePath(
    file.path(dirname(sys.frame(1)$ofile %||% "preview/run_preview_lab.R"), ".."),
    mustWork = TRUE
  )
  devtools::load_all(pkg_root, quiet = TRUE)

  out_dir_abs <- file.path(pkg_root, out_dir)
  if (isTRUE(clean) && dir.exists(out_dir_abs)) {
    unlink(out_dir_abs, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

  ex <- example_fundamental_inputs()
  marts <- prep_ohep_snapshot(
    raw_user_data = ex$user_data$user_data,
    index_data = ex$index_data,
    predictive_data = ex$index_data$predictive_data,
    snapshot_id = "preview-lab"
  )

  target <- if (is.null(company) || is.null(year)) {
    choose_default_target(marts = marts, fundamental = fundamental)
  } else {
    list(company = company, year = as.integer(year))
  }

  company <- target$company
  year <- as.integer(target$year)

  company_rows <- ex$user_data$user_data[
    as.character(ex$user_data$user_data$company) == as.character(company),
    , drop = FALSE
  ]
  company_rows$year <- suppressWarnings(as.integer(company_rows$year))
  prior_year <- suppressWarnings(max(company_rows$year[company_rows$year < as.integer(year)], na.rm = TRUE))
  if (!is.finite(prior_year)) {
    prior_year <- NA_integer_
  }

  item_ids <- marts$metadata_lookup$item_id[
    as.character(marts$metadata_lookup$fundamental_id) == as.character(fundamental)
  ]
  item_ids <- unique(as.character(item_ids))

  scenarios <- build_scenarios(
    user_df = ex$user_data$user_data,
    item_ids = item_ids,
    company = company,
    year = year,
    prior_year = prior_year,
    min_n = as.integer(min_n)
  )

  build_filtered_data <- get("build_dashboard_data_from_filtered_user_data", envir = asNamespace("ohepR"))
  build_from_marts <- get("build_dashboard_data_from_marts", envir = asNamespace("ohepR"))

  scenario_rows <- list()
  dashboard_by_scenario <- list()
  html_paths <- list()

  for (i in seq_along(scenarios)) {
    sc <- scenarios[[i]]
    sc_id <- paste0(id_prefix, "-", sc$name)

    dashboard_data <- if (identical(sc$mode, "filtered")) {
      build_filtered_data(
        company = company,
        year = year,
        fundamental = fundamental,
        marts = marts,
        filtered_user_data = sc$filtered_user_data,
        min_n = as.integer(sc$min_n),
        privacy_message = privacy_message
      )
    } else {
      build_from_marts(
        company = company,
        year = year,
        fundamental = fundamental,
        marts = marts
      )
    }

    html_obj <- if (identical(sc$mode, "filtered")) {
      fundamental_page(
        company = company,
        year = year,
        fundamental = fundamental,
        marts = marts,
        filtered_user_data = sc$filtered_user_data,
        min_n = as.integer(sc$min_n),
        privacy_message = privacy_message,
        id = sc_id
      )
    } else {
      fundamental_page(
        company = company,
        year = year,
        fundamental = fundamental,
        marts = marts,
        id = sc_id
      )
    }

    html_path <- file.path(out_dir_abs, paste0(sc$name, ".html"))
    txt_path <- file.path(out_dir_abs, paste0(sc$name, "_rendered.txt"))

    htmltools::save_html(html_obj, file = html_path)
    writeLines(htmltools::renderTags(html_obj)$html, con = txt_path, useBytes = TRUE)

    scenario_rows[[i]] <- scenario_summary_row(
      name = sc$name,
      mode = sc$mode,
      min_n = sc$min_n,
      description = sc$description,
      dashboard_data = dashboard_data,
      filtered_user_data = sc$filtered_user_data
    )

    dashboard_by_scenario[[sc$name]] <- dashboard_data
    html_paths[[sc$name]] <- html_path
  }

  summary_df <- do.call(rbind, scenario_rows)
  summary_csv <- file.path(out_dir_abs, "scenario_summary.csv")
  utils::write.csv(summary_df, file = summary_csv, row.names = FALSE)

  results <- list(
    config = list(
      company = company,
      year = year,
      prior_year = prior_year,
      fundamental = fundamental,
      base_min_n = as.integer(min_n),
      out_dir = out_dir_abs
    ),
    scenarios = scenarios,
    summary = summary_df,
    html_paths = html_paths
  )

  if (isTRUE(save_dashboard_data)) {
    results$dashboard_data <- dashboard_by_scenario
  }

  results_rds <- file.path(out_dir_abs, "scenario_results.rds")
  saveRDS(results, file = results_rds)

  index_path <- write_index_page(
    summary_df = summary_df,
    out_dir = out_dir_abs,
    run_title = sprintf(
      "OHEP Preview Lab (%s, company %s, year %s)",
      fundamental,
      as.character(company),
      as.integer(year)
    )
  )

  message("Wrote preview lab index: ", normalizePath(index_path, mustWork = TRUE))
  message("Wrote scenario summary: ", normalizePath(summary_csv, mustWork = TRUE))
  message("Wrote scenario results: ", normalizePath(results_rds, mustWork = TRUE))

  invisible(results)
}

decision_matrix_preview_configs <- function(only_favorites = TRUE) {
  all_cfg <- list(
    list(
      order = 1L,
      slug = "change_vs_last_year_by_impact",
      label = "Option 1: Change vs Last Year by Impact",
      x_metric = "prior_delta",
      y_metric = "impact",
      x_split_mode = "zero",
      y_split_mode = "median"
    ),
    list(
      order = 2L,
      slug = "benchmark_by_change_vs_last_year",
      label = "Option 2: Benchmark by Change vs Last Year",
      x_metric = "benchmark_delta",
      y_metric = "prior_delta",
      x_split_mode = "zero",
      y_split_mode = "zero"
    ),
    list(
      order = 3L,
      slug = "impact_by_opportunity",
      label = "Option 3: Impact by Opportunity Score",
      x_metric = "impact",
      y_metric = "opportunity",
      x_split_mode = "median",
      y_split_mode = "zero"
    ),
    list(
      order = 4L,
      slug = "relative_benchmark_by_impact",
      label = "Option 4: Relative Benchmark by Impact",
      x_metric = "benchmark_delta",
      y_metric = "impact",
      x_split_mode = "zero",
      y_split_mode = "median"
    )
  )
  if (!isTRUE(only_favorites)) {
    return(all_cfg)
  }
  keep_orders <- c(1L, 4L)
  all_cfg[vapply(all_cfg, function(cfg) cfg$order %in% keep_orders, logical(1))]
}

build_decision_matrix_export_data <- function(
  company,
  year,
  marts,
  outcomes = NULL,
  include_composite = TRUE,
  subset = "All",
  significant_only = FALSE,
  fallback_to_latest_year = TRUE,
  x_metric = "benchmark_delta",
  y_metric = "impact"
) {
  normalize_fundamental_key <- function(x) {
    key <- tolower(trimws(as.character(x)))
    key <- gsub("&", " and ", key, fixed = TRUE)
    key <- gsub("[^a-z0-9]+", "", key)
    key <- gsub("and", "", key, fixed = TRUE)
    aliases <- c(
      performancedev = "performancedevelopment",
      performancedevelopment = "performancedevelopment",
      learninginnovation = "learninginnovation",
      respectcaretrust = "respectcaretrust"
    )
    has_alias <- key %in% names(aliases)
    key[has_alias] <- unname(aliases[key[has_alias]])
    key
  }

  cfy <- marts$company_fundamental_year
  pred <- marts$predictive_edges

  cfy_sub <- cfy[
    as.character(cfy$company) == as.character(company) &
      suppressWarnings(as.integer(cfy$year)) == as.integer(year),
    c("fundamental_id", "vs_benchmark_raw", "mean"),
    drop = FALSE
  ]
  source_year <- as.integer(year)
  if (nrow(cfy_sub) < 1L && isTRUE(fallback_to_latest_year)) {
    cfy_company <- cfy[as.character(cfy$company) == as.character(company), , drop = FALSE]
    years_available <- sort(unique(suppressWarnings(as.integer(cfy_company$year))))
    years_available <- years_available[is.finite(years_available)]
    if (length(years_available) > 0L) {
      source_year <- max(years_available)
      cfy_sub <- cfy[
        as.character(cfy$company) == as.character(company) &
          suppressWarnings(as.integer(cfy$year)) == as.integer(source_year),
        c("fundamental_id", "vs_benchmark_raw", "mean"),
        drop = FALSE
      ]
    }
  }
  if (nrow(cfy_sub) < 1L) {
    stop(sprintf("No company fundamental scores found for company=%s year=%s.", as.character(company), as.integer(year)), call. = FALSE)
  }

  cfy_sub <- cfy_sub[!duplicated(as.character(cfy_sub$fundamental_id)), , drop = FALSE]
  names(cfy_sub) <- c("fundamental", "benchmark_delta", "company_mean")
  cfy_sub$fundamental <- as.character(cfy_sub$fundamental)
  cfy_sub$benchmark_delta <- suppressWarnings(as.numeric(cfy_sub$benchmark_delta))
  cfy_sub$company_mean <- suppressWarnings(as.numeric(cfy_sub$company_mean))
  cfy_sub$fundamental_key <- normalize_fundamental_key(cfy_sub$fundamental)

  prior_bmk <- rep(NA_real_, nrow(cfy_sub))
  for (i in seq_len(nrow(cfy_sub))) {
    fid <- as.character(cfy_sub$fundamental[[i]])
    cfy_f <- cfy[as.character(cfy$fundamental_id) == fid, , drop = FALSE]
    yr <- suppressWarnings(as.integer(cfy_f$year))
    prior_yrs <- yr[is.finite(yr) & yr < as.integer(source_year)]
    if (length(prior_yrs) < 1L) {
      next
    }
    best_prior <- max(prior_yrs)
    idx <- which(yr == best_prior)
    if (length(idx) < 1L || !"benchmark_mean" %in% names(cfy_f)) {
      next
    }
    bm_vals <- suppressWarnings(as.numeric(cfy_f$benchmark_mean[idx]))
    bm_vals <- bm_vals[is.finite(bm_vals)]
    if (length(bm_vals) > 0L) {
      prior_bmk[[i]] <- bm_vals[[1]]
    }
  }
  use_prior_benchmark <- is.finite(cfy_sub$company_mean) & is.finite(prior_bmk)
  cfy_sub$benchmark_delta[use_prior_benchmark] <- cfy_sub$company_mean[use_prior_benchmark] - prior_bmk[use_prior_benchmark]

  prior_sub <- cfy[
    as.character(cfy$company) == as.character(company) &
      suppressWarnings(as.integer(cfy$year)) == as.integer(source_year),
    c("fundamental_id", "vs_prior_raw"),
    drop = FALSE
  ]
  prior_sub <- prior_sub[!duplicated(as.character(prior_sub$fundamental_id)), , drop = FALSE]
  names(prior_sub) <- c("fundamental", "prior_delta")
  prior_sub$fundamental <- as.character(prior_sub$fundamental)
  prior_sub$prior_delta <- suppressWarnings(as.numeric(prior_sub$prior_delta))
  prior_sub$fundamental_key <- normalize_fundamental_key(prior_sub$fundamental)
  cfy_sub <- merge(
    cfy_sub[, c("fundamental", "fundamental_key", "benchmark_delta"), drop = FALSE],
    prior_sub[, c("fundamental_key", "prior_delta"), drop = FALSE],
    by = "fundamental_key",
    all.x = TRUE,
    sort = FALSE
  )

  pred2 <- pred[, intersect(c("fundamental", "outcome", "subset", "strength", "significant"), names(pred)), drop = FALSE]
  pred2$fundamental <- as.character(pred2$fundamental)
  pred2$fundamental_key <- normalize_fundamental_key(pred2$fundamental)
  pred2$outcome <- as.character(pred2$outcome)
  pred2$strength <- suppressWarnings(as.numeric(pred2$strength))
  pred2 <- pred2[is.finite(pred2$strength) & !is.na(pred2$outcome) & trimws(pred2$outcome) != "", , drop = FALSE]
  if ("subset" %in% names(pred2) && !is.null(subset) && nzchar(as.character(subset))) {
    pred2 <- pred2[tolower(trimws(as.character(pred2$subset))) == tolower(trimws(as.character(subset))), , drop = FALSE]
  }
  if ("significant" %in% names(pred2) && isTRUE(significant_only)) {
    sig <- as.logical(pred2$significant)
    pred2 <- pred2[is.na(sig) | sig, , drop = FALSE]
  }
  pred2 <- pred2[tolower(pred2$outcome) != "outcomes", , drop = FALSE]
  if (nrow(pred2) < 1L) {
    stop("No predictive edges available after filtering.", call. = FALSE)
  }

  outcome_levels <- sort(unique(as.character(pred2$outcome)))
  if (!is.null(outcomes)) {
    keep <- as.character(outcomes)
    outcome_levels <- outcome_levels[outcome_levels %in% keep]
  }
  if (length(outcome_levels) < 1L) {
    stop("No outcomes selected/available for decision matrix generation.", call. = FALSE)
  }

  by_outcome <- stats::aggregate(
    strength ~ fundamental_key + outcome,
    data = pred2[pred2$outcome %in% outcome_levels, , drop = FALSE],
    FUN = mean
  )

  metric_map_fn <- function(pts, x_metric, y_metric) {
    metric_map <- list(
      benchmark_delta = pts$benchmark_delta,
      prior_delta = pts$prior_delta,
      impact = pts$impact,
      opportunity = pts$opportunity
    )
    x_metric_local <- tolower(trimws(as.character(x_metric)))
    y_metric_local <- tolower(trimws(as.character(y_metric)))
    if (!x_metric_local %in% names(metric_map)) {
      stop(sprintf("Unsupported x_metric: %s", x_metric_local), call. = FALSE)
    }
    if (!y_metric_local %in% names(metric_map)) {
      stop(sprintf("Unsupported y_metric: %s", y_metric_local), call. = FALSE)
    }
    list(x = metric_map[[x_metric_local]], y = metric_map[[y_metric_local]])
  }

  make_points <- function(df_strength, matrix_key, matrix_label) {
    names(df_strength)[names(df_strength) == "strength"] <- "impact_raw"
    pts <- merge(
      cfy_sub[, c("fundamental", "fundamental_key", "benchmark_delta", "prior_delta"), drop = FALSE],
      df_strength[, c("fundamental_key", "impact_raw"), drop = FALSE],
      by = "fundamental_key",
      all = FALSE,
      sort = FALSE
    )
    if (nrow(pts) < 1L) {
      return(NULL)
    }
    pts$impact <- suppressWarnings(as.numeric(pts$impact_raw))
    pts$opportunity <- ifelse(
      is.na(pts$impact) | is.na(pts$benchmark_delta),
      NA_real_,
      pts$impact * (-pts$benchmark_delta)
    )
    mapped <- metric_map_fn(pts, x_metric = x_metric, y_metric = y_metric)
    pts$score <- suppressWarnings(as.numeric(mapped$x))
    pts$impact_value <- suppressWarnings(as.numeric(mapped$y))
    pts <- pts[is.finite(pts$score) & is.finite(pts$impact_value), , drop = FALSE]
    if (nrow(pts) < 1L) {
      return(NULL)
    }
    data.frame(
      matrix_key = as.character(matrix_key),
      matrix_label = as.character(matrix_label),
      source_year = as.integer(source_year),
      requested_year = as.integer(year),
      fundamental = as.character(pts$fundamental),
      benchmark_delta = as.numeric(pts$benchmark_delta),
      prior_delta = as.numeric(pts$prior_delta),
      impact = as.numeric(pts$impact),
      opportunity = as.numeric(pts$opportunity),
      score = as.numeric(pts$score),
      impact_value = as.numeric(pts$impact_value),
      x_metric = as.character(tolower(trimws(x_metric))),
      y_metric = as.character(tolower(trimws(y_metric))),
      stringsAsFactors = FALSE
    )
  }

  out_tables <- list()
  for (outcome_name in outcome_levels) {
    out_key <- paste0("outcome_", gsub("[^A-Za-z0-9]+", "_", tolower(outcome_name)))
    pts <- make_points(
      by_outcome[by_outcome$outcome == outcome_name, c("fundamental_key", "strength"), drop = FALSE],
      matrix_key = out_key,
      matrix_label = outcome_name
    )
    if (!is.null(pts)) {
      out_tables[[out_key]] <- pts
    }
  }
  if (isTRUE(include_composite)) {
    comp_strength <- stats::aggregate(strength ~ fundamental_key, data = by_outcome, FUN = mean)
    comp <- make_points(comp_strength, matrix_key = "outcomes_composite", matrix_label = "Outcomes Composite")
    if (!is.null(comp)) {
      out_tables[["outcomes_composite"]] <- comp
    }
  }
  if (length(out_tables) < 1L) {
    stop("No matrix point sets could be built for export.", call. = FALSE)
  }
  do.call(rbind, out_tables)
}

run_decision_matrix_preview <- function(
  out_dir = file.path("preview", "test_examples", "Decision Matrix"),
  company = 2,
  year = 2026,
  id = "ohep-preview-decision-matrix",
  only_favorites = TRUE,
  clean = TRUE
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is required. Install with install.packages('devtools').", call. = FALSE)
  }

  pkg_root <- normalizePath(
    file.path(dirname(sys.frame(1)$ofile %||% "preview/run_preview_lab.R"), ".."),
    mustWork = TRUE
  )
  devtools::load_all(pkg_root, quiet = TRUE)

  out_dir_abs <- file.path(pkg_root, out_dir)
  if (isTRUE(clean) && dir.exists(out_dir_abs)) {
    unlink(out_dir_abs, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

  ex <- example_fundamental_inputs()
  marts <- prep_ohep_snapshot(
    raw_user_data = ex$user_data$user_data,
    index_data = ex$index_data,
    predictive_data = ex$index_data$predictive_data,
    snapshot_id = "preview-lab-decision-matrix"
  )

  cfy <- marts$company_fundamental_year
  years_available <- sort(unique(suppressWarnings(as.integer(cfy$year[as.character(cfy$company) == as.character(company)]))))
  years_available <- years_available[is.finite(years_available)]
  if (length(years_available) < 1L) {
    stop(sprintf("No company_fundamental_year rows for company=%s.", as.character(company)), call. = FALSE)
  }
  source_year <- if (as.integer(year) %in% years_available) as.integer(year) else max(years_available)

  configs <- decision_matrix_preview_configs(only_favorites = only_favorites)
  outputs <- vector("list", length(configs))
  index_rows <- character(length(configs))

  for (i in seq_along(configs)) {
    cfg <- configs[[i]]
    file_stub <- sprintf("%02d_decision_matrix_%s_company_%s_%s", cfg$order, cfg$slug, as.character(company), as.integer(year))
    html_path <- file.path(out_dir_abs, paste0(file_stub, ".html"))
    txt_path <- file.path(out_dir_abs, paste0(file_stub, "_rendered.txt"))
    csv_path <- file.path(out_dir_abs, paste0(file_stub, "_data.csv"))

    html_obj <- decision_matrix_page(
      company = company,
      year = as.integer(year),
      marts = marts,
      id = paste0(id, "-", cfg$slug),
      theme_kicker = "Decision Matrix",
      title = "Health Driver Action Matrix",
      subtitle = "",
      x_metric = cfg$x_metric,
      y_metric = cfg$y_metric,
      x_split_mode = cfg$x_split_mode,
      y_split_mode = cfg$y_split_mode
    )

    htmltools::save_html(html_obj, file = html_path)
    writeLines(htmltools::renderTags(html_obj)$html, con = txt_path, useBytes = TRUE)
    csv_data <- build_decision_matrix_export_data(
      company = company,
      year = as.integer(year),
      marts = marts,
      x_metric = cfg$x_metric,
      y_metric = cfg$y_metric
    )
    utils::write.csv(csv_data, file = csv_path, row.names = FALSE)

    outputs[[i]] <- list(
      html = html_path,
      rendered = txt_path,
      csv = csv_path,
      label = cfg$label,
      slug = cfg$slug
    )
    index_rows[[i]] <- paste0(
      "<li><a href=\"", basename(html_path), "\">", basename(html_path), "</a>",
      " | <a href=\"", basename(csv_path), "\">", basename(csv_path), "</a>",
      " <code>", cfg$label, "</code>",
      " <span style=\"color:#64748b\">(", cfg$x_metric, " x ", cfg$y_metric, "; splits: ", cfg$x_split_mode, "/", cfg$y_split_mode, ")</span></li>"
    )
  }

  options_df <- data.frame(
    order = vapply(configs, `[[`, integer(1), "order"),
    slug = vapply(configs, `[[`, character(1), "slug"),
    label = vapply(configs, `[[`, character(1), "label"),
    x_metric = vapply(configs, `[[`, character(1), "x_metric"),
    y_metric = vapply(configs, `[[`, character(1), "y_metric"),
    x_split_mode = vapply(configs, `[[`, character(1), "x_split_mode"),
    y_split_mode = vapply(configs, `[[`, character(1), "y_split_mode"),
    stringsAsFactors = FALSE
  )
  options_path <- file.path(out_dir_abs, "decision_matrix_options.csv")
  utils::write.csv(options_df, file = options_path, row.names = FALSE)

  year_note <- if (identical(source_year, as.integer(year))) {
    sprintf("Scenario: company <code>%s</code>, year <code>%s</code>.", as.character(company), as.integer(year))
  } else {
    sprintf(
      "Scenario: company <code>%s</code>, requested year <code>%s</code>; rendered with latest available year <code>%s</code>.",
      as.character(company),
      as.integer(year),
      as.integer(source_year)
    )
  }
  index_html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>Decision Matrix Preview</title>",
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:24px;background:#f8fafc;color:#0f172a}",
    "a{color:#0f766e;text-decoration:none} a:hover{text-decoration:underline}",
    "code{background:#f1f5f9;padding:2px 6px;border-radius:4px}",
    "li{margin:8px 0}</style></head><body>",
    "<h1>Decision Matrix Preview</h1>",
    "<p>", year_note, "</p>",
    "<ul>", paste(index_rows, collapse = ""), "</ul>",
    "<p>Supporting file: <code>decision_matrix_options.csv</code></p>",
    "</body></html>"
  )
  writeLines(index_html, con = file.path(out_dir_abs, "index.html"), useBytes = TRUE)

  message("Wrote decision matrix previews: ", normalizePath(out_dir_abs, mustWork = TRUE))
  invisible(outputs)
}

run_item_distribution_preview <- function(
  out_dir = file.path("preview", "test_examples", "Item Distribution"),
  id = "ohep-preview-item-distribution",
  clean = TRUE
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is required. Install with install.packages('devtools').", call. = FALSE)
  }

  pkg_root <- normalizePath(
    file.path(dirname(sys.frame(1)$ofile %||% "preview/run_preview_lab.R"), ".."),
    mustWork = TRUE
  )
  devtools::load_all(pkg_root, quiet = TRUE)

  out_dir_abs <- file.path(pkg_root, out_dir)
  if (isTRUE(clean) && dir.exists(out_dir_abs)) {
    unlink(out_dir_abs, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

  item_distribution_data <- list(
    summary = data.frame(
      title = "Safety",
      subtitle = "Here goes the safety definition.",
      benchmark_label = "vs Industry",
      prior_label = "vs 2025",
      stringsAsFactors = FALSE
    ),
    items = data.frame(
      label = c(
        "I believe my workplace is safe.",
        "We have a strong safety culture at Surge.",
        "I feel empowered and engaged to improve safety.",
        "Leadership prioritizes health and safety.",
        "We have the right safety systems and programs."
      ),
      mean = c(4.82, 4.41, 4.12, 4.48, 4.25),
      disagree_pct = c(1, 1, 2, 1, 1),
      neutral_pct = c(3, 10, 23, 12, 19),
      agree_pct = c(96, 89, 75, 87, 80),
      vs_industry = c(4, 2, -12, 0, -5),
      vs_prior = c(2, -1, 5, -3, 0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )

  html_obj <- item_distribution_page(
    item_distribution_data = item_distribution_data,
    id = id
  )

  html_path <- file.path(out_dir_abs, "01_item_distribution.html")
  txt_path <- file.path(out_dir_abs, "01_item_distribution_rendered.txt")
  csv_path <- file.path(out_dir_abs, "01_item_distribution_data.csv")
  index_path <- file.path(out_dir_abs, "index.html")

  htmltools::save_html(html_obj, file = html_path)
  writeLines(htmltools::renderTags(html_obj)$html, con = txt_path, useBytes = TRUE)
  utils::write.csv(item_distribution_data$items, file = csv_path, row.names = FALSE)

  index_html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>Item Distribution Preview</title>",
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:24px;background:#f8fafc;color:#0f172a}",
    "a{color:#0f766e;text-decoration:none} a:hover{text-decoration:underline}",
    "code{background:#f1f5f9;padding:2px 6px;border-radius:4px}</style></head><body>",
    "<h1>Item Distribution Preview</h1>",
    "<ul>",
    "<li><a href=\"", basename(html_path), "\">", basename(html_path), "</a></li>",
    "<li><a href=\"", basename(csv_path), "\">", basename(csv_path), "</a></li>",
    "<li><code>", basename(txt_path), "</code></li>",
    "</ul></body></html>"
  )
  writeLines(index_html, con = index_path, useBytes = TRUE)

  message("Wrote item distribution preview: ", normalizePath(out_dir_abs, mustWork = TRUE))
  invisible(list(html = html_path, rendered = txt_path, csv = csv_path, index = index_path))
}

run_model_preview <- function(
  out_dir = file.path("preview", "test_examples", "Model Page"),
  id = "ohep-preview-model-page",
  clean = TRUE
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is required. Install with install.packages('devtools').", call. = FALSE)
  }

  pkg_root <- normalizePath(
    file.path(dirname(sys.frame(1)$ofile %||% "preview/run_preview_lab.R"), ".."),
    mustWork = TRUE
  )
  devtools::load_all(pkg_root, quiet = TRUE)

  out_dir_abs <- file.path(pkg_root, out_dir)
  if (isTRUE(clean) && dir.exists(out_dir_abs)) {
    unlink(out_dir_abs, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

  model_data <- list(
    summary = data.frame(
      title = "Health Diagnostics",
      subtitle = "",
      fundamentals_label = "",
      outcomes_label = "Business Outcomes",
      raw_avg_label = "Raw Avg",
      delta_label = "vs '24",
      stringsAsFactors = FALSE
    ),
    fundamentals = data.frame(
      label = c(
        "Purpose",
        "Strategy",
        "Communication",
        "Leadership",
        "Learning & Innovation",
        "Respect, Care & Trust",
        "Performance Development"
      ),
      percentile = c(72, 45, 69, 78, 82, 60, 74),
      prior_percentile = c(60, 32, 58, 71, 76, 55, 66),
      raw_avg = c(4.12, 3.83, 4.03, 4.18, 4.26, 3.99, 4.08),
      delta = c(0.14, -0.08, 0.10, 0.07, 0.06, 0.03, 0.11),
      shape = "circle",
      stringsAsFactors = FALSE
    ),
    outcomes = data.frame(
      label = c("Overall OHEP Score", "Employee Engagement", "Burnout", "Work Satisfaction", "eNPS"),
      percentile = c(64, 68, 38, 70, 76),
      prior_percentile = c(58, 63, 41, 65, 72),
      shape = "diamond",
      stringsAsFactors = FALSE
    )
  )

  html_obj <- model_page(
    model_data = model_data,
    id = id
  )

  html_path <- file.path(out_dir_abs, "01_model_page.html")
  txt_path <- file.path(out_dir_abs, "01_model_page_rendered.txt")
  csv_path <- file.path(out_dir_abs, "01_model_page_data.csv")
  index_path <- file.path(out_dir_abs, "index.html")

  htmltools::save_html(html_obj, file = html_path)
  writeLines(htmltools::renderTags(html_obj)$html, con = txt_path, useBytes = TRUE)
  to_export_rows <- function(section_name, df) {
    out <- as.data.frame(df, stringsAsFactors = FALSE)
    out$section <- section_name
    out
  }
  fundamentals_rows <- to_export_rows("fundamentals", model_data$fundamentals)
  outcomes_rows <- to_export_rows("outcomes", model_data$outcomes)
  all_cols <- unique(c(names(fundamentals_rows), names(outcomes_rows)))
  align_cols <- function(df) {
    for (nm in setdiff(all_cols, names(df))) df[[nm]] <- NA
    df[, all_cols, drop = FALSE]
  }
  csv_data <- rbind(align_cols(fundamentals_rows), align_cols(outcomes_rows))
  utils::write.csv(csv_data, file = csv_path, row.names = FALSE)

  index_html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>Model Page Preview</title>",
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:24px;background:#f8fafc;color:#0f172a}",
    "a{color:#0f766e;text-decoration:none} a:hover{text-decoration:underline}",
    "code{background:#f1f5f9;padding:2px 6px;border-radius:4px}</style></head><body>",
    "<h1>Model Page Preview</h1>",
    "<ul>",
    "<li><a href=\"", basename(html_path), "\">", basename(html_path), "</a></li>",
    "<li><a href=\"", basename(csv_path), "\">", basename(csv_path), "</a></li>",
    "<li><code>", basename(txt_path), "</code></li>",
    "</ul></body></html>"
  )
  writeLines(index_html, con = index_path, useBytes = TRUE)

  message("Wrote model page preview: ", normalizePath(out_dir_abs, mustWork = TRUE))
  invisible(list(html = html_path, rendered = txt_path, csv = csv_path, index = index_path))
}

run_demographics_preview <- function(
  out_dir = file.path("preview", "test_examples", "Demographics"),
  id = "ohep-preview-demographics-page",
  clean = TRUE
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is required. Install with install.packages('devtools').", call. = FALSE)
  }

  pkg_root <- normalizePath(
    file.path(dirname(sys.frame(1)$ofile %||% "preview/run_preview_lab.R"), ".."),
    mustWork = TRUE
  )
  devtools::load_all(pkg_root, quiet = TRUE)

  out_dir_abs <- file.path(pkg_root, out_dir)
  if (isTRUE(clean) && dir.exists(out_dir_abs)) {
    unlink(out_dir_abs, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

  tl <- demo_bipolar_split(
    data = data.frame(
      label = c("Office HQ", "Field Sites"),
      count = c(75, 37),
      pct = c(67, 33),
      stringsAsFactors = FALSE
    ),
    title = "Operational Deployment Split",
    subtitle = "Bipolar Split"
  )
  tr <- demo_categorical_bar(
    data = data.frame(
      category = c("Field Operations", "Operations Accounting", "Land & BD", "Engineering", "Corporate Services", "Geoscience"),
      value = c(35, 20, 17, 15, 14, 11),
      stringsAsFactors = FALSE
    ),
    title = "Department Headcount",
    subtitle = "Categorical Bar"
  )
  bl <- demo_categorical_tree(
    data = data.frame(
      category = c("Field", "Accounting", "Land & BD", "Engineering", "Corporate", "Geoscience"),
      short_label = c("Field", "Accounting", "Land & BD", "Eng.", "Corp.", "Geo."),
      value = c(35, 20, 17, 15, 14, 11),
      stringsAsFactors = FALSE
    ),
    title = "Allocation Footprint",
    subtitle = "Categorical Tree"
  )
  br <- demo_ordinal_bar(
    data = data.frame(
      label = c("< 1 Year", "1 - 3 Years", "3 - 5 Years", "5 - 10 Years", "10+ Years"),
      pct = c(8, 30, 22, 11, 29),
      stringsAsFactors = FALSE
    ),
    title = "Employee Tenure Distribution",
    subtitle = "Ordinal Bar"
  )

  html_obj <- demographics_page(
    tl = tl, tr = tr, bl = bl, br = br,
    id = id,
    title = "Demographics",
    subtitle = "Fixed 2x2 composed page preview."
  )

  html_path <- file.path(out_dir_abs, "01_demographics_page.html")
  txt_path <- file.path(out_dir_abs, "01_demographics_page_rendered.txt")
  csv_path <- file.path(out_dir_abs, "01_demographics_page_data.csv")
  index_path <- file.path(out_dir_abs, "index.html")

  htmltools::save_html(html_obj, file = html_path)
  writeLines(htmltools::renderTags(html_obj)$html, con = txt_path, useBytes = TRUE)

  to_rows <- function(slot_name, panel) {
    df <- as.data.frame(panel$data, stringsAsFactors = FALSE)
    df$slot <- slot_name
    df$panel_type <- panel$type
    df$panel_title <- panel$title %||% ""
    df
  }
  rows <- list(
    to_rows("tl", tl),
    to_rows("tr", tr),
    to_rows("bl", bl),
    to_rows("br", br)
  )
  all_cols <- unique(unlist(lapply(rows, names)))
  align_cols <- function(df) {
    for (nm in setdiff(all_cols, names(df))) df[[nm]] <- NA
    df[, all_cols, drop = FALSE]
  }
  csv_data <- do.call(rbind, lapply(rows, align_cols))
  utils::write.csv(csv_data, file = csv_path, row.names = FALSE)

  index_html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>Demographics Preview</title>",
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:24px;background:#f8fafc;color:#0f172a}",
    "a{color:#0f766e;text-decoration:none} a:hover{text-decoration:underline}",
    "code{background:#f1f5f9;padding:2px 6px;border-radius:4px}</style></head><body>",
    "<h1>Demographics Preview</h1>",
    "<ul>",
    "<li><a href=\"", basename(html_path), "\">", basename(html_path), "</a></li>",
    "<li><a href=\"", basename(csv_path), "\">", basename(csv_path), "</a></li>",
    "<li><code>", basename(txt_path), "</code></li>",
    "</ul></body></html>"
  )
  writeLines(index_html, con = index_path, useBytes = TRUE)

  message("Wrote demographics preview: ", normalizePath(out_dir_abs, mustWork = TRUE))
  invisible(list(html = html_path, rendered = txt_path, csv = csv_path, index = index_path))
}

if (sys.nframe() == 0L) {
  run_preview_lab()
  run_decision_matrix_preview()
  run_item_distribution_preview()
  run_model_preview()
  run_demographics_preview()
  source(file.path("preview", "run_open_ended_preview.R"))
  run_open_ended_preview()
}
