# Single-paste Displayr bootstrap script
# 1) Upload CSVs from this folder.
# 2) Paste this whole script into one Displayr R Output.
# 3) Edit DATASET_MAP only, then run.

# ohepR runtime function bundle for Displayr

build_displayr_payload <-
function(
  raw_user_data,
  static_bundle,
  snapshot_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
) {
  if (!is.data.frame(raw_user_data)) {
    stop("`raw_user_data` must be a data frame.", call. = FALSE)
  }
  if (!is.list(static_bundle) || !is.list(static_bundle$index_data)) {
    stop("`static_bundle` must come from `build_displayr_static_bundle()`.", call. = FALSE)
  }

  marts <- prep_ohep_snapshot(
    raw_user_data = raw_user_data,
    index_data = static_bundle$index_data,
    predictive_data = static_bundle$predictive_data,
    snapshot_id = snapshot_id
  )

  list(
    payload_meta = data.frame(
      snapshot_id = as.character(snapshot_id),
      generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      stringsAsFactors = FALSE
    ),
    raw_user_data = raw_user_data,
    marts = marts,
    index_data = static_bundle$index_data,
    predictive_data = static_bundle$predictive_data,
    colors_table = static_bundle$colors_table
  )
}
build_displayr_static_bundle <-
function(
  index_data,
  predictive_data = NULL,
  colors_table = NULL,
  bundle_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
) {
  if (!is.list(index_data) || !is.data.frame(index_data$item_data)) {
    stop("`index_data$item_data` is required in static bundle.", call. = FALSE)
  }

  pred <- predictive_data
  if (is.null(pred) && "predictive_data" %in% names(index_data)) {
    pred <- index_data$predictive_data
  }
  if (!is.null(pred) && !is.data.frame(pred)) {
    stop("`predictive_data` must be a data frame when supplied.", call. = FALSE)
  }

  cols <- colors_table
  if (is.null(cols)) {
    cols <- load_brand_colors_table()
  }
  if (!is.data.frame(cols) || !("variable" %in% names(cols))) {
    stop("`colors_table` must be a data frame with at least a `variable` column.", call. = FALSE)
  }

  list(
    bundle_meta = data.frame(
      bundle_id = as.character(bundle_id),
      generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      stringsAsFactors = FALSE
    ),
    index_data = index_data,
    predictive_data = pred,
    colors_table = cols
  )
}
decision_matrix_page <-
function(
  points = NULL,
  company = NULL,
  year = NULL,
  marts = NULL,
  outcomes = NULL,
  include_composite = TRUE,
  subset = "All",
  significant_only = FALSE,
  fallback_to_latest_year = TRUE,
  as_list = FALSE,
  id = "ohep-decision-matrix",
  theme_kicker = "Decision Matrix",
  title = "Health Driver Action Matrix",
  subtitle = "",
  x_metric = "benchmark_delta",
  y_metric = "impact",
  x_split_mode = "auto",
  y_split_mode = "auto",
  y_axis_main = NULL,
  x_axis_main = NULL,
  color_overrides = NULL,
  ...
) {
  runtime <- ohepRDisplayr()

  # Mode 1: explicit points => single matrix (backward compatible)
  if (!is.null(points) && is.null(marts)) {
    return(
      runtime$render_decision_matrix_page(
        points = points,
        id = id,
        theme_kicker = theme_kicker,
        title = title,
        subtitle = subtitle,
        y_axis_main = if (is.null(y_axis_main)) "IMPACT ON OUTCOMES" else y_axis_main,
        x_axis_main = if (is.null(x_axis_main)) "PERFORMANCE VS. BENCHMARK" else x_axis_main,
        color_overrides = color_overrides,
        ...
      )
    )
  }

  # Mode 2: marts-based => one per outcome + composite
  if (is.null(marts) || is.null(company) || is.null(year)) {
    stop("Provide either `points`, or `company + year + marts`.", call. = FALSE)
  }
  if (!is.list(marts) || !all(c("company_fundamental_year", "predictive_edges") %in% names(marts))) {
    stop("`marts` must include `company_fundamental_year` and `predictive_edges`.", call. = FALSE)
  }

  metric_defaults <- function(metric, axis = c("x", "y")) {
    axis <- match.arg(axis)
    metric <- tolower(trimws(as.character(metric)))
    switch(
      metric,
      benchmark_delta = list(
        label = "PERFORMANCE VS. BENCHMARK",
        low = "UNDER INDEX",
        high = "ABOVE INDEX",
        split_mode = "zero"
      ),
      prior_delta = list(
        label = "CHANGE VS. LAST YEAR",
        low = "DECLINING",
        high = "IMPROVING",
        split_mode = "zero"
      ),
      impact = list(
        label = "IMPACT ON OUTCOMES",
        low = if (axis == "x") "LOWER" else "LOW",
        high = if (axis == "x") "HIGHER" else "HIGH",
        split_mode = "median"
      ),
      opportunity = list(
        label = "OPPORTUNITY SCORE",
        low = if (axis == "x") "LOWER" else "LOWER",
        high = if (axis == "x") "HIGHER" else "HIGHER",
        split_mode = "zero"
      ),
      stop(sprintf("Unsupported %s_metric: %s", axis, metric), call. = FALSE)
    )
  }

  resolve_split <- function(values, mode, metric) {
    mode <- tolower(trimws(as.character(mode)))
    values <- suppressWarnings(as.numeric(values))
    values <- values[is.finite(values)]
    if (length(values) < 1L) {
      return(0)
    }
    if (identical(mode, "auto")) {
      mode <- metric_defaults(metric)$split_mode
    }
    if (identical(mode, "median")) {
      return(stats::median(values, na.rm = TRUE))
    }
    if (identical(mode, "zero")) {
      return(0)
    }
    stop(sprintf("Unsupported split mode: %s", mode), call. = FALSE)
  }

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
  req_pred <- c("fundamental", "outcome", "strength")
  if (!is.data.frame(pred) || nrow(pred) < 1L || length(setdiff(req_pred, names(pred))) > 0L) {
    stop("`marts$predictive_edges` must be a non-empty data frame with `fundamental`, `outcome`, `strength`.", call. = FALSE)
  }

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

  # Use stagger-stable benchmarking: compare current company performance to the
  # most recent prior-year benchmark mean for each fundamental.
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

  make_points <- function(df_strength, label) {
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

    pts$score <- suppressWarnings(as.numeric(metric_map[[x_metric_local]]))
    pts$impact_value <- suppressWarnings(as.numeric(metric_map[[y_metric_local]]))
    pts <- pts[is.finite(pts$score) & is.finite(pts$impact_value), , drop = FALSE]
    if (nrow(pts) < 1L) {
      return(NULL)
    }
    pts$impact <- pts$impact_value
    pts$label <- as.character(pts$fundamental)
    attr(pts, "x_metric") <- x_metric_local
    attr(pts, "y_metric") <- y_metric_local
    attr(pts, "x_split_value") <- resolve_split(pts$score, x_split_mode, x_metric_local)
    attr(pts, "y_split_value") <- resolve_split(pts$impact, y_split_mode, y_metric_local)
    attr(pts, "x_scale_mode") <- if (metric_defaults(x_metric_local, "x")$split_mode == "zero") "symmetric" else "range"
    attr(pts, "y_scale_mode") <- if (metric_defaults(y_metric_local, "y")$split_mode == "zero") "symmetric" else "range"
    attr(pts, "x_axis_main") <- if (is.null(x_axis_main)) metric_defaults(x_metric_local, "x")$label else x_axis_main
    y_axis_label <- if (is.null(y_axis_main)) {
      if (identical(y_metric_local, "impact")) {
        paste0("IMPACT ON ", toupper(as.character(label)))
      } else {
        metric_defaults(y_metric_local, "y")$label
      }
    } else {
      y_axis_main
    }
    attr(pts, "y_axis_main") <- y_axis_label
    attr(pts, "x_axis_low") <- metric_defaults(x_metric_local, "x")$low
    attr(pts, "x_axis_high") <- metric_defaults(x_metric_local, "x")$high
    attr(pts, "y_axis_low") <- metric_defaults(y_metric_local, "y")$low
    attr(pts, "y_axis_high") <- metric_defaults(y_metric_local, "y")$high
    list(label = label, points = pts)
  }

  matrices <- list()
  for (outcome_name in outcome_levels) {
    pts_obj <- make_points(
      by_outcome[by_outcome$outcome == outcome_name, c("fundamental_key", "strength"), drop = FALSE],
      label = outcome_name
    )
    if (!is.null(pts_obj)) {
      key <- paste0("outcome_", gsub("[^A-Za-z0-9]+", "_", tolower(outcome_name)))
      matrices[[key]] <- pts_obj
    }
  }

  if (isTRUE(include_composite)) {
    comp_strength <- stats::aggregate(strength ~ fundamental_key, data = by_outcome, FUN = mean)
    comp_obj <- make_points(comp_strength, label = "Outcomes Composite")
    if (!is.null(comp_obj)) {
      matrices[["outcomes_composite"]] <- comp_obj
    }
  }

  if (length(matrices) < 1L) {
    stop("No matrix point sets could be built for selected company/year/outcomes.", call. = FALSE)
  }

  render_one <- function(key, mat_obj, idx) {
    subtitle_one <- if (nzchar(subtitle)) subtitle else ""
    runtime$render_decision_matrix_page(
      points = mat_obj$points,
      id = paste0(id, "-", key, "-", idx),
      theme_kicker = theme_kicker,
      title = paste(title, "-", mat_obj$label),
      subtitle = subtitle_one,
      y_axis_main = if (is.null(attr(mat_obj$points, "y_axis_main"))) y_axis_main else attr(mat_obj$points, "y_axis_main"),
      x_axis_main = if (is.null(attr(mat_obj$points, "x_axis_main"))) x_axis_main else attr(mat_obj$points, "x_axis_main"),
      y_axis_low = if (is.null(attr(mat_obj$points, "y_axis_low"))) "LOW" else attr(mat_obj$points, "y_axis_low"),
      y_axis_high = if (is.null(attr(mat_obj$points, "y_axis_high"))) "HIGH" else attr(mat_obj$points, "y_axis_high"),
      x_axis_low = if (is.null(attr(mat_obj$points, "x_axis_low"))) "LAGGING" else attr(mat_obj$points, "x_axis_low"),
      x_axis_high = if (is.null(attr(mat_obj$points, "x_axis_high"))) "LEADING" else attr(mat_obj$points, "x_axis_high"),
      color_overrides = color_overrides,
      ...
    )
  }

  rendered <- lapply(seq_along(matrices), function(i) {
    nm <- names(matrices)[[i]]
    render_one(nm, matrices[[i]], i)
  })
  names(rendered) <- names(matrices)

  if (isTRUE(as_list)) {
    return(rendered)
  }
  do.call(htmltools::tagList, rendered)
}
demo_bipolar_split <-
function(data, title = "", subtitle = "") {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_bipolar_split(data)
  structure(
    list(
      type = "bipolar_split",
      data = data,
      title = title,
      subtitle = subtitle
    ),
    class = c("ohep_demo_panel", "list")
  )
}
demo_categorical_bar <-
function(data, title = "", subtitle = "", sort_desc = TRUE) {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_categorical_bar(data)
  structure(
    list(
      type = "categorical_bar",
      data = data,
      title = title,
      subtitle = subtitle,
      sort_desc = isTRUE(sort_desc)
    ),
    class = c("ohep_demo_panel", "list")
  )
}
demo_categorical_tree <-
function(data, title = "", subtitle = "") {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_categorical_tree(data)
  structure(
    list(
      type = "categorical_tree",
      data = data,
      title = title,
      subtitle = subtitle
    ),
    class = c("ohep_demo_panel", "list")
  )
}
demo_ordinal_bar <-
function(data, title = "", subtitle = "", callout_index = NULL, callout_value = NULL) {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_ordinal_bar(data)
  structure(
    list(
      type = "ordinal_bar",
      data = data,
      title = title,
      subtitle = subtitle,
      callout_index = callout_index,
      callout_value = callout_value
    ),
    class = c("ohep_demo_panel", "list")
  )
}
demographics_page <-
function(
  tl = NULL,
  tr = NULL,
  bl = NULL,
  br = NULL,
  id = "ohep-demographics-page",
  title = "Demographics",
  subtitle = "",
  color_overrides = NULL,
  ...
) {
  is_panel <- function(x) {
    is.null(x) || (is.list(x) && inherits(x, "ohep_demo_panel") && !is.null(x$type) && nzchar(as.character(x$type[[1]])))
  }
  if (!is_panel(tl) || !is_panel(tr) || !is_panel(bl) || !is_panel(br)) {
    stop("`tl`, `tr`, `bl`, and `br` must be NULL or objects from demo_* constructors.", call. = FALSE)
  }

  runtime <- ohepRDisplayr()
  runtime$render_demographics_page(
    tl = tl,
    tr = tr,
    bl = bl,
    br = br,
    id = id,
    title = title,
    subtitle = subtitle,
    color_overrides = color_overrides,
    ...
  )
}
enps_page <-
function(
  enps_data = NULL,
  id = "ohep-enps-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(enps_data)) {
    enps_data <- list(
      summary = data.frame(
        title = "Employee Net Promoter Score (eNPS)",
        subtitle = "Calculated difference between promoters and detractors.",
        score = 34,
        score_delta = 12,
        delta_label = "vs Industry",
        stringsAsFactors = FALSE
      ),
      distribution = data.frame(
        rating = 0:10,
        pct = c(1, 1, 1, 2, 3, 3, 3, 18, 20, 22, 26),
        stringsAsFactors = FALSE
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_enps_page(
    enps_data = enps_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
example_displayr_payload <-
function() {
  inputs <- example_fundamental_inputs()
  static <- build_displayr_static_bundle(
    index_data = inputs$index_data,
    predictive_data = inputs$index_data$predictive_data
  )
  build_displayr_payload(
    raw_user_data = inputs$user_data$user_data,
    static_bundle = static,
    snapshot_id = "example"
  )
}
example_fundamental_inputs <-
function() {
  index_data <- load_example_index_data()
  user_data <- load_example_user_data()
  list(index_data = index_data, user_data = user_data)
}
example_ohep_snapshot <-
function() {
  inputs <- example_fundamental_inputs()
  prep_ohep_snapshot(
    raw_user_data = inputs$user_data$user_data,
    index_data = inputs$index_data,
    predictive_data = inputs$index_data$predictive_data,
    snapshot_id = "example"
  )
}
fundamental_page <-
function(
  company = NULL,
  year = NULL,
  fundamental = NULL,
  marts = NULL,
  filtered_user_data = NULL,
  min_n = 3,
  privacy_message = "Not enough responses to display this view.",
  index_data = NULL,
  user_data = NULL,
  id = "ohep-custom-dashboard",
  color_overrides = NULL,
  benchmark_company = TRUE,
  benchmark_index = TRUE,
  benchmark_prior = TRUE,
  ...
) {
  inline_overrides <- list(...)
  old_mode <- (
    is.null(marts) &&
      is.null(index_data) &&
      is.null(user_data) &&
      is.list(year) &&
      is.list(fundamental)
  )

  if (old_mode) {
    fundamental_expr <- substitute(company)
    fundamental_name <- if (is.symbol(fundamental_expr)) {
      symbol_name <- as.character(fundamental_expr)
      if (exists(symbol_name, envir = parent.frame(), inherits = TRUE)) {
        symbol_value <- get(symbol_name, envir = parent.frame(), inherits = TRUE)
        if (!is.character(symbol_value) || length(symbol_value) != 1L) {
          stop("`fundamental` variable must contain a single string.", call. = FALSE)
        }
        as.character(symbol_value[[1]])
      } else {
        symbol_name
      }
    } else {
      fundamental_value <- eval(fundamental_expr, parent.frame())
      if (!is.character(fundamental_value) || length(fundamental_value) != 1L) {
        stop("`fundamental` must be a single string or unquoted symbol.", call. = FALSE)
      }
      as.character(fundamental_value[[1]])
    }

    runtime <- ohepRDisplayr()
    return(
      runtime$fundamental_page(
        fundamental = fundamental_name,
        index_data = year,
        user_data = fundamental,
        id = id,
        color_overrides = color_overrides,
        benchmark_company = benchmark_company,
        benchmark_index = benchmark_index,
        benchmark_prior = benchmark_prior,
        ...
      )
    )
  }

  if (is.null(marts)) {
    stop("For new usage, provide `marts` from `prep_ohep_snapshot()`.", call. = FALSE)
  }
  if (is.null(company) || length(company) != 1L) {
    stop("`company` must be a single value.", call. = FALSE)
  }
  if (is.null(year) || length(year) != 1L || !is.finite(suppressWarnings(as.numeric(year)))) {
    stop("`year` must be a single numeric year.", call. = FALSE)
  }

  fundamental_expr <- substitute(fundamental)
  fundamental_name <- NULL
  if (is.symbol(fundamental_expr)) {
    symbol_name <- as.character(fundamental_expr)
    if (!is.null(symbol_name) && !identical(symbol_name, "")) {
      if (exists(symbol_name, envir = parent.frame(), inherits = TRUE)) {
        symbol_value <- get(symbol_name, envir = parent.frame(), inherits = TRUE)
        if (!is.character(symbol_value) || length(symbol_value) != 1L) {
          stop("`fundamental` variable must contain a single string.", call. = FALSE)
        }
        fundamental_name <- as.character(symbol_value[[1]])
      } else {
        fundamental_name <- symbol_name
      }
    }
  } else if (is.character(fundamental) && length(fundamental) == 1L) {
    fundamental_name <- as.character(fundamental[[1]])
  }
  if (is.null(fundamental_name) || identical(fundamental_name, "")) {
    stop("`fundamental` must be a single string or unquoted symbol.", call. = FALSE)
  }

  if (is.data.frame(filtered_user_data)) {
    dashboard_data <- build_dashboard_data_from_filtered_user_data(
      company = company,
      year = as.integer(year),
      fundamental = fundamental_name,
      marts = marts,
      filtered_user_data = filtered_user_data,
      min_n = as.integer(min_n),
      privacy_message = privacy_message
    )
  } else {
    dashboard_data <- build_dashboard_data_from_marts(
      company = company,
      year = as.integer(year),
      fundamental = fundamental_name,
      marts = marts
    )
  }
  runtime <- ohepRDisplayr()
  rendered <- runtime$render_fundamental_page(
    dashboard_data = dashboard_data,
    id = id,
    color_overrides = color_overrides,
    benchmark_company = benchmark_company,
    benchmark_index = benchmark_index,
    benchmark_prior = benchmark_prior,
    ...
  )

  if (is.list(dashboard_data$privacy) && isTRUE(dashboard_data$privacy$suppress_page)) {
    msg <- if (!is.null(dashboard_data$privacy$message)) {
      as.character(dashboard_data$privacy$message[[1]])
    } else {
      as.character(privacy_message)
    }
    n_txt <- if (!is.null(dashboard_data$privacy$current_n)) as.integer(dashboard_data$privacy$current_n[[1]]) else NA_integer_
    min_txt <- if (!is.null(dashboard_data$privacy$min_n)) as.integer(dashboard_data$privacy$min_n[[1]]) else as.integer(min_n)
    primary_note <- sprintf("Requires %d responses to protect anonymity.", min_txt)
    disclaimer <- if (is.finite(n_txt)) {
      sprintf("Current filtered subset: %d responses.", n_txt)
    } else {
      "Blurred background does not reflect accurate data."
    }
    colors <- as.list(runtime$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "fundamental"
    ))
    wrap_id <- paste0(id, "-privacy-wrap")
    overlay_css <- sprintf(
      paste0(
        "#%s.privacy-blurred { position: relative; overflow: hidden; }\n",
        "#%s.privacy-blurred > .privacy-content { filter: blur(5px) grayscale(60%%); opacity: 0.35; pointer-events: none; user-select: none; }\n",
        "#%s.privacy-blurred > .privacy-page-overlay { position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; z-index: 9; }\n",
        "#%s.privacy-blurred .privacy-page-card { background: %s; border: 1px solid %s; border-radius: 12px; padding: 24px; max-width: 440px; box-shadow: 0 10px 25px %s; display: flex; gap: 16px; align-items: flex-start; text-align: left; }\n",
        "#%s.privacy-blurred .icon-container { color: %s; background: %s; padding: 10px; border-radius: 8px; display: flex; flex-shrink: 0; }\n",
        "#%s.privacy-blurred .content-stack { display: flex; flex-direction: column; }\n",
        "#%s.privacy-blurred .privacy-eyebrow { font: 700 11px/1 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }\n",
        "#%s.privacy-blurred .privacy-page-card h3 { margin: 0 0 6px 0; font: 800 16px/1.2 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; text-transform: uppercase; letter-spacing: .5px; }\n",
        "#%s.privacy-blurred .privacy-page-card p.primary-text { margin: 0 0 12px 0; font: 600 13px/1.5 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; }\n",
        "#%s.privacy-blurred .privacy-disclaimer { margin: 0; font: 500 11px/1.4 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; display: flex; align-items: center; gap: 6px; }"
      ),
      wrap_id, wrap_id, wrap_id,
      wrap_id, colors$page_privacy_card_bg, colors$page_privacy_card_border, colors$page_privacy_card_shadow,
      wrap_id, colors$page_privacy_icon_color, colors$page_privacy_icon_bg,
      wrap_id,
      wrap_id, colors$page_privacy_eyebrow,
      wrap_id, colors$page_privacy_title,
      wrap_id, colors$page_privacy_body,
      wrap_id, colors$page_privacy_disclaimer
    )
    return(
      htmltools::tagList(
        htmltools::tags$style(htmltools::HTML(overlay_css)),
        htmltools::tags$div(
          id = wrap_id,
          class = "privacy-blurred",
          htmltools::tags$div(class = "privacy-content", rendered),
          htmltools::tags$div(
            class = "privacy-page-overlay",
            htmltools::tags$div(
              class = "privacy-page-card",
              htmltools::tags$div(
                class = "icon-container",
                htmltools::HTML("<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><rect x=\"3\" y=\"11\" width=\"18\" height=\"11\" rx=\"2\" ry=\"2\"></rect><path d=\"M7 11V7a5 5 0 0 1 10 0v4\"></path></svg>")
              ),
              htmltools::tags$div(
                class = "content-stack",
                htmltools::tags$div(class = "privacy-eyebrow", fundamental_name),
                htmltools::tags$h3("Insufficient Responses"),
                htmltools::tags$p(class = "primary-text", primary_note),
                htmltools::tags$div(
                  class = "privacy-disclaimer",
                  htmltools::HTML("<svg width=\"12\" height=\"12\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><circle cx=\"12\" cy=\"12\" r=\"10\"></circle><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"></line><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"></line></svg>"),
                  if (!identical(msg, "")) msg else disclaimer
                )
              )
            )
          )
        )
      )
    )
  }

  rendered
}
get_delta_pill <-
function(delta_value) {
  runtime <- ohepRDisplayr()
  runtime$get_delta_pill(delta_value)
}
get_sentiment_class <-
function(score) {
  runtime <- ohepRDisplayr()
  runtime$get_sentiment_class(score)
}
heatmap_page <-
function(
  heatmap_data = NULL,
  id = "ohep-heatmap-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(heatmap_data)) {
    heatmap_data <- list(
      title = "Internal Comparisons",
      subtitle = "Column-relative view across departments",
      legend_low = "Below Avg",
      legend_high = "Above Avg",
      tables = list(
        "Health Fundamentals" = data.frame(
          Category = c(
            "Purpose", "Strategy", "Communication", "Leadership",
            "Learning & Innovation", "Respect, Care & Trust",
            "Performance Development"
          ),
          `Corp Services` = c(4.38, 3.73, 4.30, 4.12, 4.05, 4.22, 4.02),
          Engineering = c(4.40, 3.91, 4.41, 4.29, 4.11, 4.13, 3.86),
          `Field Ops` = c(4.07, 3.70, 3.96, 3.91, 3.88, 4.03, NA),
          `Geo/Drill/Comp` = c(4.15, 3.47, 3.76, 3.59, 3.73, 4.04, 3.23),
          `Land & BD` = c(4.03, 3.28, 3.82, 3.60, 3.49, 3.71, 3.61),
          `Ops Accounting` = c(4.25, 3.67, 3.91, 4.11, 3.69, 4.02, 3.78),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        "OHEP Overall" = data.frame(
          Category = c("OHEP Score", "Engagement", "Burnout", "Work Satisfaction", "eNPS"),
          Corp = c(4.11, 4.09, 3.82, 4.02, 34),
          Eng = c(4.17, 4.16, 3.95, 4.09, 41),
          Field = c(3.95, 3.88, 3.62, 3.76, 19),
          Geo = c(3.76, 3.72, 3.41, 3.58, 6),
          Land = c(3.84, 3.81, 3.55, 3.69, 12),
          `Ops Acct` = c(4.03, 3.99, 3.73, 3.91, 26),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_heatmap_page(
    heatmap_data = heatmap_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
item_distribution_page <-
function(
  item_distribution_data = NULL,
  id = "ohep-item-distribution-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(item_distribution_data)) {
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
        stringsAsFactors = FALSE
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_item_distribution_page(
    item_distribution_data = item_distribution_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
load_brand_colors_table <-
function(path = NULL) {
  if (is.null(path)) {
    path <- resolve_extdata_path("branding/colors.csv")
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
load_example_index_data <-
function() {
  item_data <- utils::read.csv(
    resolve_extdata_path("index_data/item_data.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  user_data_key <- utils::read.csv(
    resolve_extdata_path("index_data/user_data_key.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  predictive_data <- NULL
  predictive_path <- tryCatch(
    resolve_extdata_path("predictiva_data/predictive_data.csv"),
    error = function(e) NULL
  )
  if (!is.null(predictive_path) && file.exists(predictive_path)) {
    predictive_data <- utils::read.csv(
      normalizePath(predictive_path, mustWork = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  list(
    item_data = item_data,
    user_data_key = user_data_key,
    predictive_data = predictive_data
  )
}
load_example_user_data <-
function() {
  user_data <- utils::read.csv(
    resolve_extdata_path("user_data/user_data.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  list(
    user_data = user_data
  )
}
load_ohep_snapshot <-
function(path_or_tables) {
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
model_page <-
function(
  model_data = NULL,
  id = "ohep-model-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(model_data)) {
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
        label = c(
          "Overall OHEP Score",
          "Employee Engagement",
          "Burnout",
          "Work Satisfaction",
          "eNPS"
        ),
        percentile = c(64, 68, 38, 70, 76),
        prior_percentile = c(58, 63, 41, 65, 72),
        shape = "diamond",
        stringsAsFactors = FALSE
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_model_page(
    model_data = model_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
ohepRDisplayr <-
function() {
  env <- new.env(parent = baseenv())

  env$format_ordinal <- function(x) {
    x <- as.integer(round(x))
    if (is.na(x)) {
      return("")
    }
    if ((x %% 100L) %in% c(11L, 12L, 13L)) {
      suffix <- "th"
    } else {
      suffix <- switch(as.character(x %% 10L), "1" = "st", "2" = "nd", "3" = "rd", "th")
    }
    paste0(x, suffix)
  }

  env$format_signed <- function(x, digits = 0L) {
    if (is.na(x)) {
      return("0")
    }
    fmt <- paste0("%.", digits, "f")
    val <- sprintf(fmt, abs(x))
    if (x > 0) {
      paste0("+", val)
    } else if (x < 0) {
      paste0("-", val)
    } else {
      paste0("0", if (digits > 0) substr(val, 2, nchar(val)) else "")
    }
  }

  env$get_sentiment_class <- function(score) {
    if (is.na(score)) {
      return("bg-neutral")
    }
    if (score >= 67) {
      "bg-agree"
    } else if (score >= 34) {
      "bg-neutral"
    } else {
      "bg-disagree"
    }
  }

  env$get_delta_pill <- function(delta_value) {
    if (is.na(delta_value) || delta_value == 0) {
      "dp-neu"
    } else if (delta_value > 0) {
      "dp-pos"
    } else {
      "dp-neg"
    }
  }

  env$get_outcome_class <- function(percentile) {
    if (is.na(percentile)) {
      return("color-watch")
    }
    if (percentile < 40) {
      "color-risk"
    } else if (percentile < 60) {
      "color-watch"
    } else {
      "color-good"
    }
  }

  env$get_status_text <- function(percentile) {
    if (is.na(percentile)) {
      return("Industry Standard")
    }
    if (percentile < 40) {
      "Area for Growth"
    } else if (percentile < 60) {
      "Industry Standard"
    } else if (percentile < 85) {
      "Above Standard"
    } else {
      "Industry Leader"
    }
  }

  env$get_status_pill_class <- function(percentile) {
    if (is.na(percentile)) {
      return("status-pill status-watch")
    }
    if (percentile < 40) {
      "status-pill status-risk"
    } else if (percentile < 60) {
      "status-pill status-watch"
    } else if (percentile < 85) {
      "status-pill status-good"
    } else {
      "status-pill status-leader"
    }
  }

  env$get_prior_benchmark_label <- function(fundamental_row) {
    "vs '24"
  }

  env$get_active_benchmarks <- function(items, fundamental_row, benchmark_company = TRUE, benchmark_index = TRUE, benchmark_prior = TRUE) {
    candidates <- list(
      list(name = "company", enabled = isTRUE(benchmark_company), value_col = "vs_company", suppress_col = "suppress_vs_company", css_class = "delta-company", label = "vs Co."),
      list(name = "index", enabled = isTRUE(benchmark_index), value_col = "vs_industry", suppress_col = "suppress_vs_industry", css_class = "delta-industry", label = "vs Ind."),
      list(name = "prior", enabled = isTRUE(benchmark_prior), value_col = "vs_prior", suppress_col = "suppress_vs_prior", css_class = "delta-prior", label = env$get_prior_benchmark_label(fundamental_row))
    )
    active <- list()
    for (cand in candidates) {
      if (!cand$enabled || !(cand$value_col %in% names(items))) {
        next
      }
      vals <- suppressWarnings(as.numeric(items[[cand$value_col]]))
      has_available <- any(!is.na(vals))
      if (!has_available) {
        next
      }
      active[[length(active) + 1L]] <- cand
    }
    active
  }

  env$normalize_sentiment <- function(disagree_pct, neutral_pct, agree_pct) {
    vals <- c(disagree_pct, neutral_pct, agree_pct)
    if (any(is.na(vals))) {
      stop("Sentiment percentages cannot be NA.", call. = FALSE)
    }
    if (any(vals < 0 | vals > 100)) {
      stop("Sentiment percentages must be between 0 and 100.", call. = FALSE)
    }
    total <- sum(vals)
    if (total == 100) {
      return(vals)
    }
    if (total %in% c(99, 101)) {
      ix <- which.max(vals)
      vals[ix] <- vals[ix] + (100 - total)
      return(vals)
    }
    stop(
      sprintf("Sentiment percentages must sum to 100 (or 99/101 for rounding), got %s.", total),
      call. = FALSE
    )
  }

  env$escape_text <- function(x) {
    htmltools::htmlEscape(as.character(x), attribute = FALSE)
  }

  env$default_brand_colors <- function() {
    base <- c(
      bg_canvas = "#F8FAFC",
      bg_card = "#FFFFFF",
      bg_secondary = "#F1F5F9",
      bg_overlay = "rgba(255,255,255,0.98)",
      bg_accent_soft = "#F0FDFA",
      border_light = "#F1F5F9",
      border_default = "#E2E8F0",
      border_band = "#E5E7EB",
      border_dark = "#CBD5E1",
      text_primary = "#0F172A",
      text_secondary = "#334155",
      text_tertiary = "#475569",
      text_muted = "#64748B",
      text_faint = "#94A3B8",
      text_decorative = "#CBD5E1",
      text_accent = "#0D9488",
      text_on_dark = "#FFFFFF",
      percentile_z1_core = "#FCD34D",
      percentile_z1_text = "#968548",
      percentile_z1_chip = "#FFEBC4",
      percentile_z2_core = "#B3D6DA",
      percentile_z2_text = "#9DB3B7",
      percentile_z2_chip = "#D3FDFF",
      percentile_z3_core = "#0096A7",
      percentile_z3_text = "#0096A7",
      percentile_z3_chip = "#BBF5FF",
      percentile_z4_core = "#395966",
      percentile_z4_text = "#395966",
      percentile_z4_chip = "#C8F0F1",
      delta_pos_text = "#15803D",
      delta_pos_bg = "#DCFCE7",
      delta_neg_text = "#EB7161",
      delta_neg_bg = "#FEE2E2",
      delta_neu_text = "#64748B",
      delta_neu_bg = "#F1F5F9",
      favorability_agree = "#0096A7",
      favorability_neutral = "#94A3B8",
      favorability_disagree = "#EB7161",
      band_tick = "#0F172A2E",
      band_tick_label = "#64748B",
      band_midpoint = "#94A3B8",
      band_marker = "#111827",
      privacy_item_label = "#94A3B8",
      privacy_overlay_bg = "rgba(255,255,255,0.85)",
      privacy_overlay_border = "#CBD5E1",
      privacy_overlay_text = "#475569",
      privacy_overlay_shadow = "rgba(15,23,42,0.06)",
      page_privacy_card_bg = "rgba(255,255,255,0.98)",
      page_privacy_card_border = "#CBD5E1",
      page_privacy_card_shadow = "rgba(15,23,42,0.10)",
      page_privacy_icon_color = "#0D9488",
      page_privacy_icon_bg = "#F0FDFA",
      page_privacy_eyebrow = "#64748B",
      page_privacy_title = "#0F172A",
      page_privacy_body = "#475569",
      page_privacy_disclaimer = "#94A3B8"
    )

    matrix <- c(
      matrix_shell_bg = base[["border_default"]],
      matrix_slide_bg = base[["bg_canvas"]],
      matrix_header_divider = base[["border_dark"]],
      matrix_theme_kicker = base[["text_tertiary"]],
      matrix_title = base[["text_primary"]],
      matrix_subtitle = base[["text_muted"]],
      matrix_axis_line = base[["text_faint"]],
      matrix_axis_text = base[["text_muted"]],
      matrix_axis_main = base[["text_secondary"]],
      matrix_plot_bg = base[["bg_card"]],
      matrix_quadrant_tl_bg = "#FFF1F2",
      matrix_quadrant_tl_text = "#E11D48",
      matrix_quadrant_tr_bg = "#F0FDFA",
      matrix_quadrant_tr_text = "#0F766E",
      matrix_quadrant_bl_bg = base[["bg_canvas"]],
      matrix_quadrant_bl_text = base[["text_muted"]],
      matrix_quadrant_br_bg = base[["bg_secondary"]],
      matrix_quadrant_br_text = base[["text_faint"]],
      matrix_quadrant_divider = base[["border_dark"]],
      matrix_dot_critical = "#E11D48",
      matrix_dot_leverage = "#0F766E",
      matrix_dot_monitor = base[["text_muted"]],
      matrix_dot_maintain = base[["text_muted"]],
      matrix_point_border = base[["bg_card"]],
      matrix_point_shadow = "rgba(0,0,0,0.15)",
      matrix_label_bg = base[["bg_card"]],
      matrix_label_text = base[["text_secondary"]],
      matrix_label_border = base[["border_default"]],
      matrix_label_shadow = "rgba(0,0,0,0.08)",
      matrix_brand_text = base[["text_secondary"]],
      matrix_brand_dot = "#0EA5E9"
    )

    enps <- c(
      enps_shell_bg = base[["border_default"]],
      enps_slide_bg = base[["bg_canvas"]],
      enps_card_bg = base[["bg_card"]],
      enps_card_border = base[["border_default"]],
      enps_card_shadow = "rgba(15,23,42,0.04)",
      enps_title = base[["text_primary"]],
      enps_subtitle = base[["text_muted"]],
      enps_score_value = base[["text_primary"]],
      enps_score_label = base[["text_muted"]],
      enps_delta_bg = base[["delta_pos_bg"]],
      enps_delta_text = "#16A34A",
      enps_axis_baseline = base[["text_secondary"]],
      enps_axis_baseline_label = base[["text_faint"]],
      enps_tug_detractor_bg = "#E11D48",
      enps_tug_passive_bg = base[["text_faint"]],
      enps_tug_promoter_bg = base[["text_accent"]],
      enps_tug_text_on = base[["text_on_dark"]],
      enps_divider = base[["border_light"]],
      enps_dist_axis = base[["border_default"]],
      enps_dist_label = base[["text_muted"]],
      enps_det_text = "#E11D48",
      enps_pas_text = base[["text_faint"]],
      enps_pro_text = base[["text_accent"]]
    )

    heatmap <- c(
      heatmap_slide_bg = base[["bg_canvas"]],
      heatmap_title = base[["text_primary"]],
      heatmap_subtitle = base[["text_muted"]],
      heatmap_legend_text = base[["text_muted"]],
      heatmap_legend_start = "#EB7161",
      heatmap_legend_mid = base[["bg_secondary"]],
      heatmap_legend_end = "#81BBC2",
      heatmap_card_bg = base[["bg_card"]],
      heatmap_card_border = base[["border_default"]],
      heatmap_card_shadow = "rgba(0,0,0,0.04)",
      heatmap_card_title = base[["text_primary"]],
      heatmap_table_header_text = base[["text_muted"]],
      heatmap_table_header_border = base[["border_default"]],
      heatmap_row_label_text = base[["text_secondary"]],
      heatmap_row_border = base[["border_light"]],
      heatmap_pill_text = "#1E293B",
      heatmap_pill_na_bg = base[["bg_canvas"]],
      heatmap_pill_na_text = base[["text_faint"]],
      heatmap_pill_na_border = base[["border_dark"]],
      heatmap_pill_pos_strong = "#81BBC2",
      heatmap_pill_pos_soft = "#D1E6E9",
      heatmap_pill_neutral = base[["bg_secondary"]],
      heatmap_pill_neg_soft = "#FFD8D1",
      heatmap_pill_neg_strong = "#EB7161"
    )

    itemdist <- c(
      itemdist_slide_bg = base[["bg_canvas"]],
      itemdist_card_bg = base[["bg_card"]],
      itemdist_card_border = base[["border_default"]],
      itemdist_card_shadow = "rgba(0,0,0,0.04)",
      itemdist_title = base[["text_primary"]],
      itemdist_subtitle = base[["text_muted"]],
      itemdist_legend_text = base[["text_muted"]],
      itemdist_header_label = base[["text_faint"]],
      itemdist_row_label = base[["text_secondary"]],
      itemdist_row_border = base[["border_light"]],
      itemdist_mean_text = base[["text_primary"]],
      itemdist_stack_bg = base[["bg_secondary"]]
    )

    model <- c(
      model_slide_bg = base[["bg_canvas"]],
      model_card_bg = base[["bg_card"]],
      model_card_border = base[["border_default"]],
      model_card_shadow = "rgba(0,0,0,0.05)",
      model_header_accent = base[["text_primary"]],
      model_title = base[["text_primary"]],
      model_subtitle = base[["text_muted"]],
      model_section_title = base[["text_primary"]],
      model_section_rule = base[["border_light"]],
      model_metric_header = base[["text_faint"]],
      model_label = base[["text_secondary"]],
      model_track_bg = base[["border_default"]],
      model_stem = base[["border_dark"]],
      model_line = base[["text_faint"]],
      model_line_baseline = base[["text_secondary"]],
      model_line_label = base[["text_muted"]],
      model_metric_raw = base[["text_primary"]],
      model_delta_pos = base[["delta_pos_text"]],
      model_delta_neg = base[["delta_neg_text"]],
      model_delta_neu = base[["delta_neu_text"]],
      model_zone_ni = base[["percentile_z1_core"]],
      model_zone_is = base[["percentile_z2_core"]],
      model_zone_as = base[["percentile_z3_core"]],
      model_zone_il = base[["percentile_z4_core"]],
      model_point_ni = "#D97706",
      model_point_is = "#15803D",
      model_point_as = "#047857",
      model_point_il = "#0F766E",
      model_point_border = base[["text_on_dark"]],
      model_point_ghost_bg = base[["bg_card"]],
      model_point_ghost_border = base[["text_faint"]],
      model_legend_text_on = base[["text_on_dark"]]
    )

    demographics <- c(
      demo_page_bg = base[["bg_canvas"]],
      demo_grid_gap = "#E2E8F0",
      demo_panel_bg = base[["bg_card"]],
      demo_panel_border = base[["border_default"]],
      demo_panel_shadow = "rgba(15,23,42,0.04)",
      demo_title = base[["text_primary"]],
      demo_subtitle = base[["text_muted"]],
      demo_panel_title = base[["text_primary"]],
      demo_panel_subtitle = base[["text_muted"]],
      demo_empty_text = base[["text_faint"]],
      demo_bi_axis_mid = base[["text_secondary"]],
      demo_bi_axis_label = base[["text_faint"]],
      demo_bi_side_label_left = "#0EA5E9",
      demo_bi_side_label_right = "#0F766E",
      demo_bi_side_count = base[["text_muted"]],
      demo_bi_bar_left_bg = "#0EA5E9",
      demo_bi_bar_right_bg = "#0F766E",
      demo_bi_bar_text = base[["text_on_dark"]],
      demo_cb_label = base[["text_secondary"]],
      demo_cb_track_bg = base[["bg_secondary"]],
      demo_cb_value_text = base[["text_on_dark"]],
      demo_cb_bar_1 = "#0F766E",
      demo_cb_bar_2 = "#0EA5E9",
      demo_cb_bar_3 = "#14B8A6",
      demo_cb_bar_4 = "#F59E0B",
      demo_cb_bar_5 = "#F97316",
      demo_cb_bar_6 = "#64748B",
      demo_ct_text_on = base[["text_on_dark"]],
      demo_ct_border = "rgba(255,255,255,0.28)",
      demo_ct_tile_1 = "#0F766E",
      demo_ct_tile_2 = "#0EA5E9",
      demo_ct_tile_3 = "#14B8A6",
      demo_ct_tile_4 = "#F59E0B",
      demo_ct_tile_5 = "#F97316",
      demo_ct_tile_6 = "#64748B",
      demo_ob_axis_label = base[["text_muted"]],
      demo_ob_callout_bg = base[["bg_card"]],
      demo_ob_callout_text = base[["text_primary"]],
      demo_ob_callout_line = base[["text_faint"]],
      demo_ob_seg_1 = "#99F6E4",
      demo_ob_seg_2 = "#5EEAD4",
      demo_ob_seg_3 = "#2DD4BF",
      demo_ob_seg_4 = "#0D9488",
      demo_ob_seg_5 = "#0F766E",
      demo_ob_seg_text_dark = "#0F766E",
      demo_ob_seg_text_light = base[["text_on_dark"]]
    )

    c(base, matrix, enps, heatmap, itemdist, model, demographics)
  }

  env$read_brand_colors <- function(graph = "fundamental") {
    defaults <- env$default_brand_colors()
    color_file <- system.file("extdata", "branding", "colors.csv", package = "ohepR")
    if (!nzchar(color_file) || !file.exists(color_file)) {
      candidates <- c(
        file.path(getwd(), "inst", "extdata", "branding", "colors.csv"),
        file.path(getwd(), "..", "inst", "extdata", "branding", "colors.csv")
      )
      existing <- candidates[file.exists(candidates)]
      if (length(existing) > 0L) {
        color_file <- normalizePath(existing[[1]], mustWork = FALSE)
      }
    }
    if (!nzchar(color_file) || !file.exists(color_file)) {
      return(defaults)
    }

    color_df <- utils::read.csv(color_file, stringsAsFactors = FALSE)
    if (!"variable" %in% names(color_df)) {
      stop("Brand color CSV must include a `variable` column.", call. = FALSE)
    }

    base_col <- if ("color" %in% names(color_df)) {
      "color"
    } else if ("default_color" %in% names(color_df)) {
      "default_color"
    } else {
      NULL
    }
    graph_col <- paste0(tolower(graph), "_color")
    graph_has_col <- graph_col %in% names(color_df)

    to_chr <- function(x) {
      y <- trimws(as.character(x))
      y[is.na(y)] <- ""
      y
    }

    color_df <- color_df[!is.na(color_df$variable) & trimws(color_df$variable) != "", , drop = FALSE]
    if (nrow(color_df) < 1L) {
      return(defaults)
    }

    for (i in seq_len(nrow(color_df))) {
      var_name <- trimws(as.character(color_df$variable[[i]]))
      if (!nzchar(var_name)) {
        next
      }

      base_val <- if (!is.null(base_col)) to_chr(color_df[[base_col]][[i]]) else ""
      graph_val <- if (graph_has_col) to_chr(color_df[[graph_col]][[i]]) else ""
      chosen <- if (nzchar(graph_val)) graph_val else if (nzchar(base_val)) base_val else ""
      if (!nzchar(chosen)) {
        next
      }

      defaults[[var_name]] <- chosen
    }

    defaults
  }

  env$resolve_brand_colors <- function(color_overrides = NULL, extra_overrides = list(), graph = "fundamental") {
    defaults <- env$read_brand_colors(graph = graph)
    merged <- defaults

    from_list <- if (is.null(color_overrides)) list() else color_overrides
    if (!is.list(from_list)) {
      stop("`color_overrides` must be a named list.", call. = FALSE)
    }
    if (length(from_list) > 0L && (is.null(names(from_list)) || any(names(from_list) == ""))) {
      stop("`color_overrides` entries must be named.", call. = FALSE)
    }

    if (length(extra_overrides) > 0L) {
      if (is.null(names(extra_overrides)) || any(names(extra_overrides) == "")) {
        stop("Inline color overrides must be named.", call. = FALSE)
      }
      from_list <- c(from_list, extra_overrides)
    }

    unknown <- setdiff(names(from_list), names(defaults))
    if (length(unknown) > 0L) {
      stop(
        sprintf(
          "Unknown color override(s): %s. See `inst/extdata/branding/colors.csv`.",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    for (nm in names(from_list)) {
      value <- from_list[[nm]]
      if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
        stop(sprintf("Color override `%s` must be a single non-empty string.", nm), call. = FALSE)
      }
      merged[[nm]] <- as.character(value)
    }

    merged
  }

  env$validate_fundamental_page_data <- function(dashboard_data) {
    if (!is.list(dashboard_data)) {
      stop("`dashboard_data` must be a named list.", call. = FALSE)
    }

    required_sections <- c("fundamental", "outcomes", "items")
    missing_sections <- setdiff(required_sections, names(dashboard_data))
    if (length(missing_sections) > 0) {
      stop(
        sprintf("Missing section(s): %s.", paste(missing_sections, collapse = ", ")),
        call. = FALSE
      )
    }

    fundamental <- dashboard_data$fundamental
    outcomes <- dashboard_data$outcomes
    items <- dashboard_data$items

    if (!is.data.frame(fundamental) || nrow(fundamental) != 1L) {
      stop("`dashboard_data$fundamental` must be a data frame with exactly 1 row.", call. = FALSE)
    }
    if (!is.data.frame(outcomes) || nrow(outcomes) < 1L || nrow(outcomes) > 3L) {
      stop("`dashboard_data$outcomes` must have between 1 and 3 rows.", call. = FALSE)
    }
    if (!is.data.frame(items) || nrow(items) < 1L) {
      stop("`dashboard_data$items` must be a non-empty data frame.", call. = FALSE)
    }

    fundamental_cols <- c("percentile", "percentile_delta", "delta_label", "score", "score_delta")
    outcomes_cols <- c("rank", "outcome", "percentile")
    item_base_cols <- c(
      "column", "section", "section_order", "item_order", "label", "mean",
      "disagree_pct", "neutral_pct", "agree_pct"
    )

    if (length(setdiff(fundamental_cols, names(fundamental))) > 0) {
      stop("`fundamental` is missing required columns.", call. = FALSE)
    }
    if (length(setdiff(outcomes_cols, names(outcomes))) > 0) {
      stop("`outcomes` is missing required columns.", call. = FALSE)
    }
    if (length(setdiff(item_base_cols, names(items))) > 0) {
      stop("`items` is missing required columns.", call. = FALSE)
    }

    if (is.na(fundamental$percentile) || fundamental$percentile < 0 || fundamental$percentile > 100) {
      stop("`fundamental$percentile` must be between 0 and 100.", call. = FALSE)
    }
    if (anyNA(outcomes$percentile) || any(outcomes$percentile < 0 | outcomes$percentile > 100)) {
      stop("`outcomes$percentile` must be between 0 and 100.", call. = FALSE)
    }
    if (anyNA(items$column) || !all(items$column %in% c("left", "right"))) {
      stop("`items$column` must contain only 'left' or 'right'.", call. = FALSE)
    }

    left_n <- sum(items$column == "left")
    right_n <- sum(items$column == "right")
    if (left_n > 4L || right_n > 4L) {
      stop("Each items column can contain at most 4 rows in v1.", call. = FALSE)
    }

    pct_mat <- items[, c("disagree_pct", "neutral_pct", "agree_pct")]
    if (anyNA(pct_mat)) {
      stop("`items` sentiment percentages cannot be NA.", call. = FALSE)
    }
    if (any(pct_mat < 0 | pct_mat > 100)) {
      stop("`items` sentiment percentages must be between 0 and 100.", call. = FALSE)
    }

    for (i in seq_len(nrow(items))) {
      env$normalize_sentiment(items$disagree_pct[i], items$neutral_pct[i], items$agree_pct[i])
    }

    dashboard_data
  }

  env$validate_fundamental_inputs <- function(index_data, user_data) {
    if (!is.list(index_data)) {
      stop("`index_data` must be a named list.", call. = FALSE)
    }
    if (!is.list(user_data)) {
      stop("`user_data` must be a named list.", call. = FALSE)
    }

    has_required_cols <- function(df, cols) {
      is.data.frame(df) && length(setdiff(cols, names(df))) == 0
    }

    fm_cols <- c("fundamental", "percentile", "percentile_delta", "delta_label", "score", "score_delta")
    out_cols <- c("fundamental", "rank", "outcome", "percentile")
    key_cols <- c(
      "fundamental", "item", "section", "column", "section_order", "item_order",
      "label", "vs_industry", "vs_prior"
    )
    score_cols <- c("item", "mean")
    sent_cols <- c("item", "disagree_pct", "neutral_pct", "agree_pct")

    if (
      has_required_cols(index_data$fundamental_metrics, fm_cols) &&
      has_required_cols(index_data$outcomes, out_cols) &&
      has_required_cols(index_data$item_key, key_cols) &&
      has_required_cols(user_data$item_scores, score_cols) &&
      has_required_cols(user_data$item_sentiment, sent_cols)
    ) {
      return(invisible("canonical"))
    }

    required_index_new <- c("item_data", "user_data_key")
    required_user_new <- c("user_data")
    missing_index_new <- setdiff(required_index_new, names(index_data))
    missing_user_new <- setdiff(required_user_new, names(user_data))

    if (length(missing_index_new) > 0 || length(missing_user_new) > 0) {
      missing_parts <- c(
        if (length(missing_index_new) > 0) {
          paste0("index_data: ", paste(missing_index_new, collapse = ", "))
        },
        if (length(missing_user_new) > 0) {
          paste0("user_data: ", paste(missing_user_new, collapse = ", "))
        }
      )
      stop(
        sprintf(
          "Input lists do not match supported schemas. Missing %s.",
          paste(missing_parts, collapse = "; ")
        ),
        call. = FALSE
      )
    }

    if (!has_required_cols(index_data$item_data, c("item_id", "item_text", "fundamental_id", "facet_id"))) {
      stop("`index_data$item_data` is missing required columns.", call. = FALSE)
    }
    year_cols <- grep("^[0-9]{4}_mean$", names(index_data$item_data), value = TRUE)
    if (length(year_cols) < 1L) {
      stop("`index_data$item_data` must include at least one `<year>_mean` column.", call. = FALSE)
    }
    if (!has_required_cols(index_data$user_data_key, c("item", "Description", "data_domain"))) {
      stop("`index_data$user_data_key` is missing required columns.", call. = FALSE)
    }
    if (!is.data.frame(user_data$user_data)) {
      stop("`user_data$user_data` must be a data frame.", call. = FALSE)
    }
    if (length(setdiff(c("year", "company"), names(user_data$user_data))) > 0) {
      stop("`user_data$user_data` must include `year` and `company` columns.", call. = FALSE)
    }

    invisible("raw")
  }

  env$build_dashboard_data_from_raw <- function(fundamental, index_data, user_data) {
    clamp_pct <- function(x) {
      as.numeric(pmax(pmin(round(x), 100), 0))
    }
    mean_or_na <- function(df, col) {
      if (!col %in% names(df)) {
        return(NA_real_)
      }
      vals <- suppressWarnings(as.numeric(df[[col]]))
      if (all(is.na(vals))) {
        return(NA_real_)
      }
      mean(vals, na.rm = TRUE)
    }
    sentiment_from_values <- function(values, scale_min = 1, scale_max = 5) {
      vals <- suppressWarnings(as.numeric(values))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        return(c(0, 100, 0))
      }

      first_cut <- scale_min + (scale_max - scale_min) / 3
      second_cut <- scale_min + 2 * (scale_max - scale_min) / 3
      disagree <- 100 * sum(vals <= first_cut) / length(vals)
      neutral <- 100 * sum(vals > first_cut & vals < second_cut) / length(vals)
      agree <- 100 * sum(vals >= second_cut) / length(vals)

      out <- round(c(disagree, neutral, agree))
      diff <- 100 - sum(out)
      out[which.max(out)] <- out[which.max(out)] + diff
      out
    }

    item_data <- index_data$item_data
    user_key <- index_data$user_data_key
    user_df <- user_data$user_data
    pred <- index_data$predictive_data

    year_num <- suppressWarnings(as.numeric(user_df$year))
    latest_year <- suppressWarnings(max(year_num, na.rm = TRUE))
    if (!is.finite(latest_year)) {
      stop("`user_data$user_data$year` does not contain a valid year.", call. = FALSE)
    }

    latest_rows <- user_df[year_num == latest_year, , drop = FALSE]
    if (nrow(latest_rows) < 1L) {
      stop("No rows found in `user_data$user_data` for latest year.", call. = FALSE)
    }

    company_vals <- as.character(latest_rows$company)
    company_vals <- company_vals[!is.na(company_vals) & company_vals != ""]
    if (length(company_vals) > 0L) {
      company_tab <- sort(table(company_vals), decreasing = TRUE)
      focal_company <- names(company_tab)[1]
      current_df <- latest_rows[as.character(latest_rows$company) == focal_company, , drop = FALSE]
      prior_mask <- as.character(user_df$company) == focal_company & year_num < latest_year
      prior_years <- year_num[prior_mask]
      if (length(prior_years) > 0L && any(!is.na(prior_years))) {
        prior_year <- max(prior_years, na.rm = TRUE)
        prior_df <- user_df[as.character(user_df$company) == focal_company & year_num == prior_year, , drop = FALSE]
      } else {
        prior_year <- NA_real_
        prior_df <- user_df[0, , drop = FALSE]
      }
    } else {
      focal_company <- NA_character_
      current_df <- latest_rows
      prior_year <- suppressWarnings(max(year_num[year_num < latest_year], na.rm = TRUE))
      if (is.finite(prior_year)) {
        prior_df <- user_df[year_num == prior_year, , drop = FALSE]
      } else {
        prior_year <- NA_real_
        prior_df <- user_df[0, , drop = FALSE]
      }
    }

    fund_rows <- item_data[as.character(item_data$fundamental_id) == fundamental, , drop = FALSE]
    if (nrow(fund_rows) < 1L) {
      stop("Expected exactly one row in `fundamental_metrics` for the selected fundamental.", call. = FALSE)
    }

    year_cols <- grep("^[0-9]{4}_mean$", names(item_data), value = TRUE)
    year_vals <- suppressWarnings(as.numeric(sub("_mean$", "", year_cols)))
    year_cols <- year_cols[order(year_vals)]
    year_vals <- year_vals[order(year_vals)]

    fund_year_means <- vapply(year_cols, function(col) {
      vals <- suppressWarnings(as.numeric(fund_rows[[col]]))
      if (all(is.na(vals))) {
        return(NA_real_)
      }
      mean(vals, na.rm = TRUE)
    }, numeric(1))
    usable_year_idx <- which(!is.na(fund_year_means))
    latest_index_idx <- if (length(usable_year_idx) > 0L) utils::tail(usable_year_idx, 1) else length(year_cols)
    prior_index_idx <- if (latest_index_idx > 1L) latest_index_idx - 1L else latest_index_idx
    latest_col <- year_cols[latest_index_idx]
    prior_col <- year_cols[prior_index_idx]
    latest_col_year <- year_vals[latest_index_idx]
    prior_col_year <- year_vals[prior_index_idx]

    fund_rows$item_id <- as.character(fund_rows$item_id)
    suffix_num <- suppressWarnings(as.integer(sub(".*_(\\d+)$", "\\1", fund_rows$item_id)))
    ord <- order(ifelse(is.na(suffix_num), Inf, suffix_num), fund_rows$item_id)
    fund_rows <- fund_rows[ord, , drop = FALSE]
    fund_rows <- fund_rows[!duplicated(fund_rows$item_id), , drop = FALSE]
    fund_rows <- utils::head(fund_rows, 8L)

    item_ids <- as.character(fund_rows$item_id)
    item_labels <- as.character(user_key$Description[match(item_ids, as.character(user_key$item))])
    fallback_labels <- as.character(fund_rows$item_text)
    item_labels[is.na(item_labels) | item_labels == ""] <- fallback_labels[is.na(item_labels) | item_labels == ""]

    min_scale <- suppressWarnings(min(as.numeric(fund_rows$response_scale_min), na.rm = TRUE))
    max_scale <- suppressWarnings(max(as.numeric(fund_rows$response_scale_max), na.rm = TRUE))
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }

    user_item_mean <- vapply(item_ids, function(item) mean_or_na(current_df, item), numeric(1))
    prior_item_mean <- vapply(item_ids, function(item) mean_or_na(prior_df, item), numeric(1))
    index_item_mean <- suppressWarnings(as.numeric(fund_rows[[latest_col]]))

    fund_score <- if (all(is.na(user_item_mean))) mean(index_item_mean, na.rm = TRUE) else mean(user_item_mean, na.rm = TRUE)
    if (!is.finite(fund_score)) {
      stop("Could not compute fundamental score from raw inputs.", call. = FALSE)
    }
    prior_score <- if (all(is.na(prior_item_mean))) NA_real_ else mean(prior_item_mean, na.rm = TRUE)
    score_delta <- if (is.na(prior_score)) 0 else fund_score - prior_score

    percentile <- clamp_pct(100 * (fund_score - min_scale) / (max_scale - min_scale))
    percentile_delta <- round(score_delta * 100)
    delta_label <- if (is.finite(prior_year)) {
      paste0("vs. ", as.integer(prior_year))
    } else if (is.finite(prior_col_year)) {
      paste0("vs. ", as.integer(prior_col_year))
    } else {
      "vs. prior"
    }

    key_rows <- nrow(fund_rows)
    left_n <- ceiling(key_rows / 2)
    columns <- c(rep("left", left_n), rep("right", key_rows - left_n))
    item_order <- stats::ave(seq_len(key_rows), columns, FUN = seq_along)
    section_vals <- as.character(fund_rows$facet_id)
    section_vals[is.na(section_vals) | section_vals == ""] <- fundamental

    item_key <- data.frame(
      fundamental = rep(fundamental, key_rows),
      item = item_ids,
      section = section_vals,
      column = columns,
      section_order = 1,
      item_order = as.numeric(item_order),
      label = item_labels,
      vs_industry = ifelse(is.na(user_item_mean) | is.na(index_item_mean), NA_real_, round((user_item_mean - index_item_mean) * 100)),
      vs_prior = ifelse(is.na(user_item_mean) | is.na(prior_item_mean), NA_real_, round((user_item_mean - prior_item_mean) * 100)),
      suppress_vs_industry = as.logical(is.na(user_item_mean) | is.na(index_item_mean)),
      suppress_vs_prior = as.logical(is.na(user_item_mean) | is.na(prior_item_mean)),
      suppress_row = FALSE,
      min_n = 3L,
      stringsAsFactors = FALSE
    )

    item_sent <- t(vapply(seq_along(item_ids), function(i) {
      sent <- sentiment_from_values(
        current_df[[item_ids[i]]],
        scale_min = suppressWarnings(as.numeric(fund_rows$response_scale_min[i])),
        scale_max = suppressWarnings(as.numeric(fund_rows$response_scale_max[i]))
      )
      env$normalize_sentiment(sent[1], sent[2], sent[3])
    }, numeric(3)))
    colnames(item_sent) <- c("disagree_pct", "neutral_pct", "agree_pct")

    build_outcomes_fallback <- function() {
      calc_outcome_pct <- function(cols, reverse = FALSE) {
        cols <- cols[cols %in% names(current_df)]
        if (length(cols) == 0L) {
          return(50)
        }
        vals <- suppressWarnings(as.numeric(unlist(current_df[, cols, drop = FALSE])))
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0L) {
          return(50)
        }
        pct <- clamp_pct(100 * (mean(vals) - 1) / 4)
        if (reverse) {
          pct <- 100 - pct
        }
        pct
      }

      out <- data.frame(
        rank = c(1, 2, 3),
        outcome = c("Turnover Intentions", "Work Satisfaction", "Engagement"),
        percentile = c(
          calc_outcome_pct(c("turnover_1", "turnover_2", "turnover_3"), reverse = TRUE),
          calc_outcome_pct(c("workSat_1", "workSat_2"), reverse = FALSE),
          calc_outcome_pct(c("engagement_1", "engagement_2", "engagement_3", "engagement_4", "engagement_5"), reverse = FALSE)
        ),
        stringsAsFactors = FALSE
      )
      out
    }

    out_rows <- NULL
    if (is.data.frame(pred) && all(c("Fundamental", "Outcome", "strength") %in% names(pred))) {
      sub_pred <- pred[as.character(pred$Fundamental) == fundamental, , drop = FALSE]
      if ("subset" %in% names(sub_pred)) {
        sub_pred <- sub_pred[as.character(sub_pred$subset) == "All", , drop = FALSE]
      }
      if ("significant" %in% names(sub_pred) && any(as.logical(sub_pred$significant), na.rm = TRUE)) {
        sub_pred <- sub_pred[as.logical(sub_pred$significant), , drop = FALSE]
      }
      sub_pred <- sub_pred[!as.character(sub_pred$Outcome) %in% c("Outcomes", "eNPS"), , drop = FALSE]
      if (nrow(sub_pred) > 0L) {
        strength_vals <- suppressWarnings(as.numeric(sub_pred$strength))
        sub_pred <- sub_pred[order(abs(strength_vals), decreasing = TRUE), , drop = FALSE]
        sub_pred <- utils::head(sub_pred, 3L)
        out_rows <- data.frame(
          rank = seq_len(nrow(sub_pred)),
          outcome = as.character(sub_pred$Outcome),
          percentile = clamp_pct(abs(suppressWarnings(as.numeric(sub_pred$strength))) * 100),
          stringsAsFactors = FALSE
        )
      }
    }
    if (!is.data.frame(out_rows) || nrow(out_rows) < 1L) {
      out_rows <- build_outcomes_fallback()
    }

    canonical <- list(
      fundamental = data.frame(
        percentile = as.numeric(percentile),
        percentile_delta = as.numeric(percentile_delta),
        delta_label = as.character(delta_label),
        score = as.numeric(round(fund_score, 2)),
        score_delta = as.numeric(round(score_delta, 2)),
        stringsAsFactors = FALSE
      ),
      outcomes = data.frame(
        rank = as.numeric(out_rows$rank),
        outcome = as.character(out_rows$outcome),
        percentile = as.numeric(out_rows$percentile),
        stringsAsFactors = FALSE
      ),
      items = data.frame(
        column = as.character(item_key$column),
        section = as.character(item_key$section),
        section_order = as.numeric(item_key$section_order),
        item_order = as.numeric(item_key$item_order),
        label = as.character(item_key$label),
        mean = as.numeric(round(user_item_mean, 2)),
        disagree_pct = as.numeric(item_sent[, "disagree_pct"]),
        neutral_pct = as.numeric(item_sent[, "neutral_pct"]),
        agree_pct = as.numeric(item_sent[, "agree_pct"]),
        vs_industry = as.numeric(item_key$vs_industry),
        vs_prior = as.numeric(item_key$vs_prior),
        suppress_row = as.logical(item_key$suppress_row),
        suppress_vs_industry = as.logical(item_key$suppress_vs_industry),
        suppress_vs_prior = as.logical(item_key$suppress_vs_prior),
        min_n = as.integer(item_key$min_n),
        stringsAsFactors = FALSE
      )
    )

    canonical$items$mean[is.na(canonical$items$mean)] <- round(index_item_mean[is.na(canonical$items$mean)], 2)
    canonical$items$mean[is.na(canonical$items$mean)] <- round(fund_score, 2)

    canonical
  }

  env$build_dashboard_data_from_inputs <- function(fundamental, index_data, user_data) {
    input_schema <- env$validate_fundamental_inputs(index_data, user_data)

    if (identical(input_schema, "raw")) {
      canonical <- env$build_dashboard_data_from_raw(
        fundamental = fundamental,
        index_data = index_data,
        user_data = user_data
      )
      return(env$validate_fundamental_page_data(canonical))
    }

    fm <- index_data$fundamental_metrics
    outcomes <- index_data$outcomes
    item_key <- index_data$item_key
    item_scores <- user_data$item_scores
    item_sentiment <- user_data$item_sentiment

    fm_sub <- fm[fm$fundamental == fundamental, , drop = FALSE]
    out_sub <- outcomes[outcomes$fundamental == fundamental, , drop = FALSE]
    key_sub <- item_key[item_key$fundamental == fundamental, , drop = FALSE]

    if (nrow(fm_sub) != 1L) {
      stop("Expected exactly one row in `fundamental_metrics` for the selected fundamental.", call. = FALSE)
    }
    if (nrow(out_sub) < 1L) {
      stop("No outcomes found for selected fundamental.", call. = FALSE)
    }
    if (nrow(key_sub) < 1L) {
      stop("No item key rows found for selected fundamental.", call. = FALSE)
    }

    score_ix <- match(key_sub$item, item_scores$item)
    sent_ix <- match(key_sub$item, item_sentiment$item)
    if (anyNA(score_ix)) {
      missing_items <- unique(key_sub$item[is.na(score_ix)])
      stop(sprintf("Missing user item score(s): %s.", paste(missing_items, collapse = ", ")), call. = FALSE)
    }
    if (anyNA(sent_ix)) {
      missing_items <- unique(key_sub$item[is.na(sent_ix)])
      stop(sprintf("Missing user item sentiment row(s): %s.", paste(missing_items, collapse = ", ")), call. = FALSE)
    }

    canonical <- list(
      fundamental = fm_sub[, c("percentile", "percentile_delta", "delta_label", "score", "score_delta"), drop = FALSE],
      outcomes = out_sub[, c("rank", "outcome", "percentile"), drop = FALSE],
      items = data.frame(
        column = as.character(key_sub$column),
        section = as.character(key_sub$section),
        section_order = as.numeric(key_sub$section_order),
        item_order = as.numeric(key_sub$item_order),
        label = as.character(key_sub$label),
        mean = as.numeric(item_scores$mean[score_ix]),
        disagree_pct = as.numeric(item_sentiment$disagree_pct[sent_ix]),
        neutral_pct = as.numeric(item_sentiment$neutral_pct[sent_ix]),
        agree_pct = as.numeric(item_sentiment$agree_pct[sent_ix]),
        vs_industry = as.numeric(key_sub$vs_industry),
        vs_prior = as.numeric(key_sub$vs_prior),
        suppress_row = FALSE,
        suppress_vs_industry = as.logical(is.na(key_sub$vs_industry)),
        suppress_vs_prior = as.logical(is.na(key_sub$vs_prior)),
        min_n = 3L,
        stringsAsFactors = FALSE
      )
    )

    env$validate_fundamental_page_data(canonical)
  }

  env$fundamental_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "fundamental")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-root {{
  --bg-canvas: {c$bg_canvas};
  --bg-card: {c$bg_card};
  --bg-secondary: {c$bg_secondary};
  --border-light: {c$border_light};
  --border-default: {c$border_default};
  --border-band: {c$border_band};
  --border-dark: {c$border_dark};
  --text-primary: {c$text_primary};
  --text-secondary: {c$text_secondary};
  --text-tertiary: {c$text_tertiary};
  --text-muted: {c$text_muted};
  --text-faint: {c$text_faint};
  --text-decorative: {c$text_decorative};
  --text-accent: {c$text_accent};
  --text-on-dark: {c$text_on_dark};
  --percentile-z1-core: {c$percentile_z1_core};
  --percentile-z1-text: {c$percentile_z1_text};
  --percentile-z1-chip: {c$percentile_z1_chip};
  --percentile-z2-core: {c$percentile_z2_core};
  --percentile-z2-text: {c$percentile_z2_text};
  --percentile-z2-chip: {c$percentile_z2_chip};
  --percentile-z3-core: {c$percentile_z3_core};
  --percentile-z3-text: {c$percentile_z3_text};
  --percentile-z3-chip: {c$percentile_z3_chip};
  --percentile-z4-core: {c$percentile_z4_core};
  --percentile-z4-text: {c$percentile_z4_text};
  --percentile-z4-chip: {c$percentile_z4_chip};
  --delta-pos-text: {c$delta_pos_text};
  --delta-pos-bg: {c$delta_pos_bg};
  --delta-neg-text: {c$delta_neg_text};
  --delta-neg-bg: {c$delta_neg_bg};
  --delta-neu-text: {c$delta_neu_text};
  --delta-neu-bg: {c$delta_neu_bg};
  --favorability-agree: {c$favorability_agree};
  --favorability-neutral: {c$favorability_neutral};
  --favorability-disagree: {c$favorability_disagree};
  --band-tick: {c$band_tick};
  --band-tick-label: {c$band_tick_label};
  --band-midpoint: {c$band_midpoint};
  --band-marker: {c$band_marker};
  --privacy-item-label: {c$privacy_item_label};
  --privacy-overlay-bg: {c$privacy_overlay_bg};
  --privacy-overlay-border: {c$privacy_overlay_border};
  --privacy-overlay-text: {c$privacy_overlay_text};
  --privacy-overlay-shadow: {c$privacy_overlay_shadow};
  width: 1280px;
  height: 720px;
  background: var(--bg-canvas);
  padding: 24px 32px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  flex-direction: column;
  gap: 20px;
  overflow: hidden;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .top-row {{ display: flex; gap: 20px; height: 310px; flex-shrink: 0; }}
{scope} .bottom-row {{ flex: 1; display: flex; min-height: 0; }}
{scope} .card {{
  background: var(--bg-card);
  border: 1px solid var(--border-default);
  border-radius: 12px;
  padding: 24px;
  box-shadow: 0 4px 12px rgba(15,23,42,.03);
  display: flex;
  flex-direction: column;
}}
{scope} .fundamental-card {{ flex: 1.2; }}
{scope} .outcomes-card {{ flex: 1; }}
{scope} .item-card {{ width: 100%; padding: 20px 24px; }}
{scope} .title-bar {{ display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 16px; }}
{scope} .card-title {{ font-size: 22px; font-weight: 900; color: var(--text-primary); margin: 0; text-transform: uppercase; letter-spacing: -0.5px; }}
{scope} .status-pill {{ font-size: 11px; font-weight: 800; padding: 6px 12px; border-radius: 12px; text-transform: uppercase; letter-spacing: 0.5px; }}
{scope} .status-risk {{ background: var(--percentile-z1-chip); color: var(--percentile-z1-text); }}
{scope} .status-watch {{ background: var(--percentile-z2-chip); color: var(--percentile-z2-text); }}
{scope} .status-good {{ background: var(--percentile-z3-chip); color: var(--percentile-z3-text); }}
{scope} .status-leader {{ background: var(--percentile-z4-chip); color: var(--percentile-z4-text); }}
{scope} .color-risk {{ color: var(--percentile-z1-text); }}
{scope} .color-watch {{ color: var(--percentile-z2-text); }}
{scope} .color-good {{ color: var(--percentile-z3-text); }}
{scope} .metrics-container {{ display: flex; flex-direction: column; margin-bottom: 16px; }}
{scope} .hero-row {{ display: flex; align-items: flex-start; gap: 20px; }}
{scope} .hero-label {{ font-size: 11px; font-weight: 800; color: var(--text-accent); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 4px; }}
{scope} .hero-value {{ font-size: 48px; font-weight: 900; color: var(--text-primary); line-height: 1; letter-spacing: -1px; }}
{scope} .delta-block {{ display: flex; align-items: center; gap: 6px; margin-top: 16px; }}
{scope} .delta-block.positive {{ color: var(--delta-pos-text); }}
{scope} .delta-block.negative {{ color: var(--delta-neg-text); }}
{scope} .delta-block.neutral {{ color: var(--text-muted); }}
{scope} .delta-icon {{ font-size: 20px; line-height: 1; }}
{scope} .delta-details {{ display: flex; flex-direction: column; justify-content: center; }}
{scope} .delta-value {{ font-size: 14px; font-weight: 800; line-height: 1.1; }}
{scope} .delta-year {{ font-size: 10px; color: var(--text-faint); font-weight: 700; text-transform: uppercase; margin-top: 2px; }}
{scope} .sub-context {{ font-size: 13px; color: var(--text-muted); font-weight: 600; margin-top: 12px; }}
{scope} .sub-context strong {{ color: var(--text-primary); font-weight: 800; }}
{scope} .sub-delta {{ color: var(--delta-pos-text); font-weight: 800; }}
{scope} .band-wrap {{ position: relative; margin-top: auto; padding-top: 16px; }}
{scope} .band {{ position: relative; height: 26px; border-radius: 999px; overflow: hidden; display: flex; border: 1px solid var(--border-band); }}
{scope} .z1 {{ width: 40%; background: var(--percentile-z1-core); }}
{scope} .z2 {{ width: 20%; background: var(--percentile-z2-core); }}
{scope} .z3 {{ width: 25%; background: var(--percentile-z3-core); }}
{scope} .z4 {{ width: 15%; background: var(--percentile-z4-core); }}
{scope} .tick {{ position: absolute; top: 0; width: 2px; height: 26px; background: var(--band-tick); z-index: 2; transform: translateX(-50%); }}
{scope} .tick-label {{ position: absolute; top: 12px; transform: translate(-50%, -100%); font-size: 11px; font-weight: 800; color: var(--band-tick-label); }}
{scope} .midpoint {{ position: absolute; top: 29px; left: 50%; width: 2px; height: 32px; background: var(--band-midpoint); transform: translate(-50%, -50%); z-index: 3; }}
{scope} .marker {{ position: absolute; top: 29px; width: 4px; height: 38px; background: var(--band-marker); transform: translate(-50%, -50%); border-radius: 4px; z-index: 5; }}
{scope} .labels {{ display: flex; align-items: center; font-size: 11px; font-weight: 600; color: var(--text-muted); margin-top: 10px; }}
{scope} .labels span {{ display: flex; justify-content: center; text-align: center; }}
{scope} .labels span:nth-child(1) {{ width: 40%; }}
{scope} .labels span:nth-child(2) {{ width: 20%; font-weight: 800; color: var(--percentile-z2-text); }}
{scope} .labels span:nth-child(3) {{ width: 25%; }}
{scope} .labels span:nth-child(4) {{ width: 15%; }}
{scope} .table-outcomes {{ width: 100%; border-collapse: collapse; margin-top: auto; }}
{scope} .table-outcomes th {{
  text-align: left; padding-bottom: 10px; font-size: 10px; font-weight: 800; color: var(--text-faint);
  text-transform: uppercase; letter-spacing: 0.5px; border-bottom: 2px solid var(--border-default);
}}
{scope} .table-outcomes th.right-align {{ text-align: right; }}
{scope} .table-outcomes td {{ padding: 14px 0; border-bottom: 1px solid var(--border-light); vertical-align: middle; }}
{scope} .table-outcomes tr:last-child td {{ border-bottom: none; padding-bottom: 0; }}
{scope} .row-rank {{ font-size: 22px; font-weight: 800; color: var(--text-decorative); margin-right: 12px; }}
{scope} .row-name {{ font-size: 15px; font-weight: 800; color: var(--text-primary); }}
{scope} .company-score-stack {{ display: flex; flex-direction: column; align-items: flex-end; }}
{scope} .row-score-val {{ font-size: 15px; font-weight: 800; line-height: 1; }}
{scope} .row-status {{ font-size: 10px; font-weight: 800; text-transform: uppercase; letter-spacing: 0.3px; margin-top: 4px; }}
{scope} .item-title-row {{ display: flex; justify-content: space-between; align-items: center; margin-bottom: 22px; }}
{scope} .legend-group {{ display: flex; gap: 16px; font-size: 11px; font-weight: 700; color: var(--text-muted); text-transform: uppercase; }}
{scope} .leg-item {{ display: flex; align-items: center; gap: 6px; }}
{scope} .leg-dot {{ width: 10px; height: 10px; border-radius: 2px; }}
{scope} .bg-agree {{ background: var(--favorability-agree); }}
{scope} .bg-neutral {{ background: var(--favorability-neutral); }}
{scope} .bg-disagree {{ background: var(--favorability-disagree); }}
{scope} .item-grid-2col {{ display: grid; grid-template-columns: 1fr 1fr; gap: 40px; flex: 1; }}
{scope} .item-column {{ display: flex; flex-direction: column; }}
{scope} .header-row, {scope} .item-row {{ display: grid; grid-template-columns: minmax(180px, 2fr) 40px 3fr 50px 50px; gap: 12px; align-items: center; }}
{scope} .header-row {{ padding-bottom: 8px; border-bottom: 2px solid var(--border-default); }}
{scope} .h-lbl {{ font-size: 9px; font-weight: 800; color: var(--text-faint); text-transform: uppercase; letter-spacing: 0.5px; text-align: center; }}
{scope} .h-lbl:first-child {{ text-align: left; }}
{scope} .sub-cat {{ grid-column: 1 / -1; background: var(--bg-secondary); font-size: 11px; font-weight: 800; color: var(--text-secondary); text-transform: uppercase; letter-spacing: 1px; padding: 6px 12px; border-radius: 4px; margin: 8px 0; }}
{scope} .item-row {{ padding: 6px 0; border-bottom: 1px solid var(--border-light); }}
{scope} .item-row:last-child {{ border-bottom: none; }}
{scope} .item-row > .item-label {{ grid-column: 1; }}
{scope} .item-row > .cell-score {{ grid-column: 2; }}
{scope} .item-row > .sentiment-stack {{ grid-column: 3; }}
{scope} .item-row > .sentiment-privacy-stack {{ grid-column: 3; display: flex; flex-direction: column; align-items: center; gap: 8px; }}
{scope} .item-row > .sentiment-privacy-stack > .sentiment-stack {{ width: 100%; }}
{scope} .item-row > .delta-industry {{ grid-column: 4; }}
{scope} .item-row > .delta-prior {{ grid-column: 5; }}
{scope} .item-label {{ font-size: 12px; font-weight: 600; color: var(--text-secondary); line-height: 1.3; padding-right: 8px; }}
{scope} .cell-score {{ font-size: 13px; font-weight: 800; color: var(--text-primary); text-align: center; }}
{scope} .sentiment-stack {{ height: 22px; display: flex; border-radius: 4px; overflow: hidden; background: var(--bg-secondary); }}
{scope} .segment {{ display: flex; align-items: center; justify-content: center; color: var(--text-on-dark); font-size: 10px; font-weight: 800; }}
{scope} .delta-container {{ display: flex; justify-content: center; }}
{scope} .delta-pill {{ display: inline-flex; align-items: center; justify-content: center; width: 48px; padding: 4px 0; border-radius: 4px; font-size: 10px; font-weight: 800; }}
{scope} .dp-pos {{ background: var(--delta-pos-bg); color: var(--delta-pos-text); }}
{scope} .dp-neg {{ background: var(--delta-neg-bg); color: var(--delta-neg-text); }}
{scope} .dp-neu {{ background: var(--delta-neu-bg); color: var(--delta-neu-text); }}
{scope} .privacy-row {{ position: relative; }}
{scope} .privacy-row .item-label {{ color: var(--privacy-item-label); }}
{scope} .privacy-row > .cell-score,
{scope} .privacy-row > .delta-container,
{scope} .privacy-row > .sentiment-stack,
{scope} .privacy-row > .sentiment-privacy-stack > .sentiment-stack {{ filter: blur(4px) grayscale(60%); opacity: 0.25; pointer-events: none; user-select: none; }}
{scope} .privacy-overlay {{
  position: relative;
  z-index: 3;
  background: var(--privacy-overlay-bg);
  backdrop-filter: blur(2px);
  border: 1px solid var(--privacy-overlay-border);
  border-radius: 6px;
  padding: 6px 14px;
  font-size: 11px;
  font-weight: 700;
  color: var(--privacy-overlay-text);
  letter-spacing: 0.2px;
  box-shadow: 0 2px 6px var(--privacy-overlay-shadow);
  display: flex;
  align-items: center;
  gap: 6px;
}}
"
    )
  }

  env$build_hero_card <- function(fundamental_row) {
    percentile <- as.numeric(fundamental_row$percentile[[1]])
    percentile_delta <- as.numeric(fundamental_row$percentile_delta[[1]])
    score <- as.numeric(fundamental_row$score[[1]])
    score_delta <- as.numeric(fundamental_row$score_delta[[1]])
    delta_label <- env$escape_text(fundamental_row$delta_label[[1]])

    delta_class <- if (percentile_delta > 0) {
      "positive"
    } else if (percentile_delta < 0) {
      "negative"
    } else {
      "neutral"
    }
    delta_icon <- if (percentile_delta > 0) {
      "&#9650;"
    } else if (percentile_delta < 0) {
      "&#9660;"
    } else {
      "&#8226;"
    }

    glue::glue(
      "<div class=\"card fundamental-card\">
        <div class=\"title-bar\">
          <h2 class=\"card-title\">Purpose</h2>
          <span class=\"{env$get_status_pill_class(percentile)}\">{env$get_status_text(percentile)}</span>
        </div>
        <div class=\"metrics-container\">
          <div class=\"hero-row\">
            <div>
              <div class=\"hero-label\">Percentile</div>
              <div class=\"hero-value\">{env$format_ordinal(percentile)}</div>
            </div>
            <div class=\"delta-block {delta_class}\">
              <div class=\"delta-icon\">{delta_icon}</div>
              <div class=\"delta-details\">
                <div class=\"delta-value\">{env$format_signed(percentile_delta)} pts</div>
                <div class=\"delta-year\">{delta_label}</div>
              </div>
            </div>
          </div>
          <div class=\"sub-context\">
            Score: <strong>{sprintf('%.2f', score)}</strong>
            <span class=\"sub-delta\">({env$format_signed(score_delta, digits = 2)})</span>
          </div>
        </div>
        <div class=\"band-wrap\">
          <div class=\"tick-label\" style=\"left:40%;\">40</div>
          <div class=\"tick-label\" style=\"left:50%;\">50</div>
          <div class=\"tick-label\" style=\"left:60%;\">60</div>
          <div class=\"tick-label\" style=\"left:85%;\">85</div>
          <div class=\"band\">
            <div class=\"z1\"></div><div class=\"z2\"></div><div class=\"z3\"></div><div class=\"z4\"></div>
            <div class=\"tick\" style=\"left:40%;\"></div><div class=\"tick\" style=\"left:60%;\"></div><div class=\"tick\" style=\"left:85%;\"></div>
          </div>
          <div class=\"midpoint\"></div>
          <div class=\"marker\" style=\"left:{max(0, min(100, percentile))}%;\"></div>
        </div>
        <div class=\"labels\">
          <span>Area for Growth</span>
          <span>Industry standard</span>
          <span>Above standard</span>
          <span>Industry leader</span>
        </div>
      </div>"
    )
  }

  env$build_outcome_row <- function(outcome_row) {
    pct <- as.numeric(outcome_row$percentile[[1]])
    color_class <- env$get_outcome_class(pct)
    status_text <- env$get_status_text(pct)
    glue::glue(
      "<tr>
        <td>
          <span class=\"row-rank\">{as.integer(outcome_row$rank[[1]])}</span>
          <span class=\"row-name\">{env$escape_text(outcome_row$outcome[[1]])}</span>
        </td>
        <td>
          <div class=\"company-score-stack\">
            <span class=\"row-score-val {color_class}\">{env$format_ordinal(pct)} Percentile</span>
            <span class=\"row-status {color_class}\">{status_text}</span>
          </div>
        </td>
      </tr>"
    )
  }

  env$build_item_row <- function(item_row, active_benchmarks = NULL) {
    suppress_row <- ("suppress_row" %in% names(item_row)) && isTRUE(as.logical(item_row$suppress_row[[1]]))
    min_n <- if ("min_n" %in% names(item_row)) as.integer(item_row$min_n[[1]]) else 3L
    if (is.null(active_benchmarks)) {
      active_benchmarks <- list(
        list(name = "index", value_col = "vs_industry", suppress_col = "suppress_vs_industry", css_class = "delta-industry"),
        list(name = "prior", value_col = "vs_prior", suppress_col = "suppress_vs_prior", css_class = "delta-prior")
      )
    }

    sentiment <- env$normalize_sentiment(
      as.numeric(item_row$disagree_pct[[1]]),
      as.numeric(item_row$neutral_pct[[1]]),
      as.numeric(item_row$agree_pct[[1]])
    )
    disagree <- sentiment[[1]]
    neutral <- sentiment[[2]]
    agree <- sentiment[[3]]

    disagree_lbl <- if (disagree >= 8) paste0(round(disagree), "%") else ""
    neutral_lbl <- if (neutral >= 8) paste0(round(neutral), "%") else ""
    agree_lbl <- if (agree >= 8) paste0(round(agree), "%") else ""

    privacy_tip <- "Insufficient responses"
    n_bm <- length(active_benchmarks)
    row_classes <- c("item-row")
    if (suppress_row) {
      row_classes <- c(row_classes, "privacy-row")
    }
    row_class <- paste(row_classes, collapse = " ")
    row_grid_style <- switch(
      as.character(n_bm),
      "0" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr;\"",
      "1" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px;\"",
      "3" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px 50px 50px;\"",
      ""
    )
    stacked_sentiment <- paste0(
      "<div class=\"sentiment-stack\">
        <div class=\"segment bg-disagree\" style=\"width: ", disagree, "%;\">", disagree_lbl, "</div>
        <div class=\"segment bg-neutral\" style=\"width: ", neutral, "%;\">", neutral_lbl, "</div>
        <div class=\"segment bg-agree\" style=\"width: ", agree, "%;\">", agree_lbl, "</div>
      </div>"
    )
    row_overlay <- if (suppress_row) {
      paste0("<div class=\"sentiment-privacy-stack\">
        <div class=\"privacy-overlay\">
        <svg width=\"12\" height=\"12\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">
          <rect x=\"3\" y=\"11\" width=\"18\" height=\"11\" rx=\"2\" ry=\"2\"></rect>
          <path d=\"M7 11V7a5 5 0 0 1 10 0v4\"></path>
        </svg>
        Insufficient responses
        </div>
        ", stacked_sentiment, "
      </div>")
    } else {
      ""
    }
    score_val <- as.numeric(item_row$mean[[1]])
    score_text <- if (is.na(score_val)) "-" else sprintf("%.2f", score_val)
    benchmark_html <- vapply(seq_along(active_benchmarks), function(i) {
      bm <- active_benchmarks[[i]]
      delta_val <- if (bm$value_col %in% names(item_row)) suppressWarnings(as.numeric(item_row[[bm$value_col]][[1]])) else NA_real_
      suppress_delta <- if (bm$suppress_col %in% names(item_row)) {
        isTRUE(as.logical(item_row[[bm$suppress_col]][[1]]))
      } else {
        is.na(delta_val)
      }
      pill <- env$get_delta_pill(delta_val)
      text <- if (suppress_delta) "-" else paste0(env$format_signed(delta_val), "%")
      title <- if (suppress_delta) glue::glue(" title=\"{privacy_tip}\"") else ""
      pill <- if (suppress_delta) "dp-neu" else pill
      grid_style <- if (n_bm != 2L) glue::glue(" style=\"grid-column:{i + 3L};\"") else ""
      glue::glue("<div class=\"delta-container {bm$css_class}\"{grid_style}><span class=\"delta-pill {pill}\"{title}>{text}</span></div>")
    }, character(1))
    benchmark_html <- paste(benchmark_html, collapse = "")

    glue::glue(
      "<div class=\"{row_class}\"{row_grid_style}>
        <div class=\"item-label\">{env$escape_text(item_row$label[[1]])}</div>
        <div class=\"cell-score\">{score_text}</div>
        {if (suppress_row) row_overlay else stacked_sentiment}
        {benchmark_html}
      </div>"
    )
  }

  env$build_item_column <- function(items_df, column_name, active_benchmarks = list()) {
    col_df <- items_df[items_df$column == column_name, , drop = FALSE]
    col_df <- col_df[order(col_df$section_order, col_df$item_order), , drop = FALSE]
    header_cells <- c(
      "<div class=\"h-lbl\">Survey Item</div>",
      "<div class=\"h-lbl\">Mean</div>",
      "<div class=\"h-lbl\">Sentiment Distribution</div>"
    )
    if (length(active_benchmarks) > 0L) {
      bench_headers <- vapply(active_benchmarks, function(bm) {
        bench_class <- if (length(active_benchmarks) == 2L) "h-lbl" else "h-lbl h-lbl-benchmark"
        glue::glue("<div class=\"{bench_class}\">{env$escape_text(bm$label)}</div>")
      }, character(1))
      header_cells <- c(header_cells, bench_headers)
    }
    n_bm <- length(active_benchmarks)
    header_class <- "header-row"
    header_grid_style <- switch(
      as.character(n_bm),
      "0" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr;\"",
      "1" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px;\"",
      "3" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px 50px 50px;\"",
      ""
    )
    header <- glue::glue(
      "<div class=\"{header_class}\"{header_grid_style}>
      {glue::glue_collapse(header_cells, sep = '')}
    </div>"
    )

    out <- character(0)
    out <- c(out, header)
    sections <- unique(col_df$section)
    for (section in sections) {
      out <- c(out, glue::glue("<div class=\"sub-cat\">{env$escape_text(section)}</div>"))
      section_rows <- col_df[col_df$section == section, , drop = FALSE]
      row_html <- vapply(seq_len(nrow(section_rows)), function(i) {
        env$build_item_row(section_rows[i, , drop = FALSE], active_benchmarks = active_benchmarks)
      }, character(1))
      out <- c(out, row_html)
    }
    glue::glue("<div class=\"item-column\">{glue::glue_collapse(out, sep = '')}</div>")
  }

  env$build_outcomes_table <- function(outcomes_df) {
    outcomes_df <- outcomes_df[order(outcomes_df$rank), , drop = FALSE]
    rows <- vapply(seq_len(nrow(outcomes_df)), function(i) {
      env$build_outcome_row(outcomes_df[i, , drop = FALSE])
    }, character(1))

    glue::glue(
      "<div class=\"card outcomes-card\">
        <div class=\"title-bar\">
          <h2 class=\"card-title\">Purpose drives</h2>
        </div>
        <table class=\"table-outcomes\">
          <thead>
            <tr>
              <th>Outcome</th>
              <th class=\"right-align\">Your Company</th>
            </tr>
          </thead>
          <tbody>{glue::glue_collapse(rows, sep = '')}</tbody>
        </table>
      </div>"
    )
  }

  env$validate_enps_data <- function(enps_data) {
    if (!is.list(enps_data)) {
      stop("`enps_data` must be a named list.", call. = FALSE)
    }
    if (!("distribution" %in% names(enps_data)) || !is.data.frame(enps_data$distribution)) {
      stop("`enps_data$distribution` must be a data frame.", call. = FALSE)
    }
    dist <- enps_data$distribution
    needed_dist <- c("rating", "pct")
    if (length(setdiff(needed_dist, names(dist))) > 0L) {
      stop("`enps_data$distribution` must include `rating` and `pct` columns.", call. = FALSE)
    }
    ratings <- suppressWarnings(as.integer(dist$rating))
    pcts <- suppressWarnings(as.numeric(dist$pct))
    if (anyNA(ratings) || anyNA(pcts)) {
      stop("`distribution$rating` and `distribution$pct` must be numeric/non-missing.", call. = FALSE)
    }
    if (any(ratings < 0L | ratings > 10L)) {
      stop("`distribution$rating` values must be in 0..10.", call. = FALSE)
    }
    if (any(pcts < 0 | pcts > 100)) {
      stop("`distribution$pct` values must be in 0..100.", call. = FALSE)
    }
    total <- sum(pcts)
    if (abs(total - 100) > 1.01) {
      stop("`distribution$pct` must sum to ~100.", call. = FALSE)
    }
    invisible(enps_data)
  }

  env$normalize_enps_data <- function(enps_data) {
    env$validate_enps_data(enps_data)

    dist <- enps_data$distribution[, c("rating", "pct"), drop = FALSE]
    dist$rating <- suppressWarnings(as.integer(dist$rating))
    dist$pct <- suppressWarnings(as.numeric(dist$pct))

    full_ratings <- data.frame(rating = 0:10, stringsAsFactors = FALSE)
    dist <- merge(full_ratings, dist, by = "rating", all.x = TRUE, sort = TRUE)
    dist$pct[is.na(dist$pct)] <- 0
    dist <- dist[order(dist$rating), , drop = FALSE]

    detractors_pct <- sum(dist$pct[dist$rating <= 6], na.rm = TRUE)
    passives_pct <- sum(dist$pct[dist$rating %in% c(7, 8)], na.rm = TRUE)
    promoters_pct <- sum(dist$pct[dist$rating >= 9], na.rm = TRUE)

    summary <- if ("summary" %in% names(enps_data) && is.data.frame(enps_data$summary) && nrow(enps_data$summary) >= 1L) {
      enps_data$summary[1, , drop = FALSE]
    } else {
      data.frame(stringsAsFactors = FALSE)
    }

    score <- if ("score" %in% names(summary)) suppressWarnings(as.numeric(summary$score[[1]])) else NA_real_
    if (!is.finite(score)) {
      score <- promoters_pct - detractors_pct
    }
    score <- max(-100, min(100, score))

    score_delta <- if ("score_delta" %in% names(summary)) suppressWarnings(as.numeric(summary$score_delta[[1]])) else NA_real_
    if (!is.finite(score_delta)) score_delta <- 0

    title <- if ("title" %in% names(summary)) as.character(summary$title[[1]]) else "Employee Net Promoter Score (eNPS)"
    subtitle <- if ("subtitle" %in% names(summary)) as.character(summary$subtitle[[1]]) else "Calculated difference between promoters and detractors."
    delta_label <- if ("delta_label" %in% names(summary)) as.character(summary$delta_label[[1]]) else "vs benchmark"

    list(
      title = title,
      subtitle = subtitle,
      score = score,
      score_delta = score_delta,
      delta_label = delta_label,
      promoters_pct = promoters_pct,
      passives_pct = passives_pct,
      detractors_pct = detractors_pct,
      distribution = dist
    )
  }

  env$enps_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "enps")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-enps-root {{
  --enps-shell-bg: {c$enps_shell_bg};
  --enps-slide-bg: {c$enps_slide_bg};
  --enps-card-bg: {c$enps_card_bg};
  --enps-card-border: {c$enps_card_border};
  --enps-card-shadow: {c$enps_card_shadow};
  --enps-title: {c$enps_title};
  --enps-subtitle: {c$enps_subtitle};
  --enps-score-value: {c$enps_score_value};
  --enps-score-label: {c$enps_score_label};
  --enps-delta-bg: {c$enps_delta_bg};
  --enps-delta-text: {c$enps_delta_text};
  --enps-axis-baseline: {c$enps_axis_baseline};
  --enps-axis-baseline-label: {c$enps_axis_baseline_label};
  --enps-tug-detractor-bg: {c$enps_tug_detractor_bg};
  --enps-tug-passive-bg: {c$enps_tug_passive_bg};
  --enps-tug-promoter-bg: {c$enps_tug_promoter_bg};
  --enps-tug-text-on: {c$enps_tug_text_on};
  --enps-divider: {c$enps_divider};
  --enps-dist-axis: {c$enps_dist_axis};
  --enps-dist-label: {c$enps_dist_label};
  --enps-det-text: {c$enps_det_text};
  --enps-pas-text: {c$enps_pas_text};
  --enps-pro-text: {c$enps_pro_text};
  width: 1280px;
  height: 720px;
  background: var(--enps-slide-bg);
  padding: 34px 42px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .enps-card {{
  width: 100%;
  border: 1px solid var(--enps-card-border);
  border-radius: 16px;
  background: var(--enps-card-bg);
  box-shadow: 0 10px 25px var(--enps-card-shadow);
  padding: 34px 40px;
  display: flex;
  flex-direction: column;
}}
{scope} .card-header {{ display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 34px; }}
{scope} .title-group {{ display: flex; flex-direction: column; gap: 6px; }}
{scope} .card-title {{ margin: 0; font-size: 26px; font-weight: 900; color: var(--enps-title); letter-spacing: -0.3px; }}
{scope} .card-sub {{ margin: 0; font-size: 14px; color: var(--enps-subtitle); font-weight: 600; }}
{scope} .score-display {{ display: flex; flex-direction: column; align-items: flex-end; gap: 5px; }}
{scope} .score-label {{ font-size: 11px; font-weight: 800; text-transform: uppercase; letter-spacing: .8px; color: var(--enps-score-label); }}
{scope} .big-score {{ font-size: 64px; line-height: 1; margin: 0; font-weight: 900; color: var(--enps-score-value); letter-spacing: -2px; }}
{scope} .delta-pill {{ display: inline-flex; align-items: center; gap: 6px; padding: 4px 10px; border-radius: 6px; font-size: 12px; font-weight: 800; letter-spacing: .3px; background: var(--enps-delta-bg); color: var(--enps-delta-text); }}
{scope} .tug-wrap {{ display: flex; flex-direction: column; gap: 14px; margin-bottom: 24px; }}
{scope} .tug-axis {{ position: relative; height: 58px; }}
{scope} .tug-mid {{ position: absolute; top: -8px; left: 50%; width: 2px; height: 74px; transform: translateX(-50%); background: var(--enps-axis-baseline); z-index: 2; }}
{scope} .tug-mid-label {{ position: absolute; top: -20px; left: 50%; transform: translateX(-50%); font-size: 11px; font-weight: 800; letter-spacing: .5px; color: var(--enps-axis-baseline-label); text-transform: uppercase; }}
{scope} .tug-left, {scope} .tug-right {{ position: absolute; top: 5px; height: 48px; display: flex; align-items: center; font-size: 17px; font-weight: 900; color: var(--enps-tug-text-on); }}
{scope} .tug-left {{ right: 50%; justify-content: flex-end; padding-right: 14px; border-radius: 8px 0 0 8px; background: var(--enps-tug-detractor-bg); }}
{scope} .tug-right {{ left: 50%; justify-content: flex-start; padding-left: 14px; border-radius: 0 8px 8px 0; background: var(--enps-tug-promoter-bg); }}
{scope} .tug-labels {{ display: flex; justify-content: space-between; align-items: center; }}
{scope} .t-group {{ display: flex; flex-direction: column; gap: 2px; }}
{scope} .t-group.left {{ align-items: flex-start; }}
{scope} .t-group.right {{ align-items: flex-end; }}
{scope} .t-title {{ font-size: 13px; font-weight: 800; letter-spacing: .5px; text-transform: uppercase; }}
{scope} .t-title.det {{ color: var(--enps-det-text); }}
{scope} .t-title.pro {{ color: var(--enps-pro-text); }}
{scope} .t-math {{ font-size: 20px; font-weight: 900; color: var(--enps-score-value); letter-spacing: -0.4px; }}
{scope} .section-divider {{ border: 0; border-top: 1px solid var(--enps-divider); margin: 10px 0 20px 0; }}
{scope} .dist-title {{ margin: 0 0 4px 0; font-size: 14px; font-weight: 800; color: var(--enps-title); text-transform: uppercase; letter-spacing: .5px; }}
{scope} .dist-sub {{ margin: 0 0 14px 0; font-size: 12px; color: var(--enps-subtitle); }}
{scope} .dist-bars {{ display: flex; align-items: flex-end; justify-content: space-between; height: 140px; border-bottom: 2px solid var(--enps-dist-axis); }}
{scope} .d-group {{ display: flex; align-items: flex-end; height: 100%; }}
{scope} .d-group.det {{ width: 56%; justify-content: space-between; }}
{scope} .d-group.pas {{ width: 18%; justify-content: space-around; }}
{scope} .d-group.pro {{ width: 18%; justify-content: space-around; }}
{scope} .d-bar {{ width: 38px; border-radius: 4px 4px 0 0; }}
{scope} .d-bar.det {{ background: var(--enps-tug-detractor-bg); }}
{scope} .d-bar.pas {{ background: var(--enps-tug-passive-bg); }}
{scope} .d-bar.pro {{ background: var(--enps-tug-promoter-bg); }}
{scope} .dist-labels {{ display: flex; align-items: flex-start; justify-content: space-between; margin-top: 10px; }}
{scope} .l-group {{ display: flex; align-items: flex-start; gap: 6px; }}
{scope} .l-group.det {{ width: 56%; justify-content: space-between; }}
{scope} .l-group.pas {{ width: 18%; justify-content: space-around; }}
{scope} .l-group.pro {{ width: 18%; justify-content: space-around; }}
{scope} .d-label {{ width: 38px; text-align: center; font-size: 12px; font-weight: 800; color: var(--enps-dist-label); }}
{scope} .dist-brackets {{ display: flex; justify-content: space-between; margin-top: 20px; }}
{scope} .bracket {{ font-size: 12px; font-weight: 800; letter-spacing: .5px; text-transform: uppercase; }}
{scope} .bracket.det {{ width: 56%; text-align: left; color: var(--enps-det-text); }}
{scope} .bracket.pas {{ width: 18%; text-align: center; color: var(--enps-pas-text); }}
{scope} .bracket.pro {{ width: 18%; text-align: right; color: var(--enps-pro-text); }}
"
    )
  }

  env$render_enps_page <- function(
    enps_data,
    id = "ohep-enps-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "enps"
    )

    dat <- env$normalize_enps_data(enps_data)
    dist <- dat$distribution

    bars_raw <- dist$pct
    max_bar <- suppressWarnings(max(bars_raw, na.rm = TRUE))
    if (!is.finite(max_bar) || max_bar <= 0) max_bar <- 1
    bar_h <- pmax(0, (bars_raw / max_bar) * 100)

    tug_den <- max(c(dat$promoters_pct, dat$detractors_pct, 1), na.rm = TRUE)
    tug_left <- (dat$detractors_pct / tug_den) * 46
    tug_right <- (dat$promoters_pct / tug_den) * 46

    score_txt <- if (dat$score > 0) paste0("+", round(dat$score)) else as.character(round(dat$score))
    delta_prefix <- if (dat$score_delta >= 0) "+ " else "- "
    delta_txt <- paste0(delta_prefix, abs(round(dat$score_delta)), " ", env$escape_text(dat$delta_label))

    det_idx <- which(dist$rating <= 6)
    pas_idx <- which(dist$rating %in% c(7, 8))
    pro_idx <- which(dist$rating >= 9)

    mk_bars <- function(idxs, cls) {
      paste(vapply(idxs, function(i) {
        glue::glue("<div class=\"d-bar {cls}\" style=\"height:{sprintf('%.1f', bar_h[[i]])}%;\"></div>")
      }, character(1)), collapse = "")
    }
    mk_labels <- function(idxs, cls) {
      paste(vapply(idxs, function(i) {
        glue::glue("<span class=\"d-label {cls}\">{dist$rating[[i]]}</span>")
      }, character(1)), collapse = "")
    }

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-enps-root\">
        <div class=\"enps-card\">
          <div class=\"card-header\">
            <div class=\"title-group\">
              <h2 class=\"card-title\">{env$escape_text(dat$title)}</h2>
              <p class=\"card-sub\">{env$escape_text(dat$subtitle)}</p>
            </div>
            <div class=\"score-display\">
              <span class=\"score-label\">Net Score</span>
              <span class=\"big-score\">{score_txt}</span>
              <span class=\"delta-pill\">{delta_txt}</span>
            </div>
          </div>

          <div class=\"tug-wrap\">
            <div class=\"tug-axis\">
              <div class=\"tug-mid\"><span class=\"tug-mid-label\">Baseline</span></div>
              <div class=\"tug-left\" style=\"width:{sprintf('%.1f', tug_left)}%;\">{sprintf('%.0f%%', dat$detractors_pct)}</div>
              <div class=\"tug-right\" style=\"width:{sprintf('%.1f', tug_right)}%;\">{sprintf('%.0f%%', dat$promoters_pct)}</div>
            </div>
            <div class=\"tug-labels\">
              <div class=\"t-group left\">
                <span class=\"t-title det\">Detractors</span>
                <span class=\"t-math\">- {sprintf('%.0f', dat$detractors_pct)}</span>
              </div>
              <div class=\"t-group right\">
                <span class=\"t-title pro\">Promoters</span>
                <span class=\"t-math\">+ {sprintf('%.0f', dat$promoters_pct)}</span>
              </div>
            </div>
          </div>

          <hr class=\"section-divider\" />

          <h3 class=\"dist-title\">Population Distribution</h3>
          <p class=\"dist-sub\">Detailed breakdown of the 0-10 rating scale</p>

          <div class=\"dist-bars\">
            <div class=\"d-group det\">{mk_bars(det_idx, 'det')}</div>
            <div class=\"d-group pas\">{mk_bars(pas_idx, 'pas')}</div>
            <div class=\"d-group pro\">{mk_bars(pro_idx, 'pro')}</div>
          </div>
          <div class=\"dist-labels\">
            <div class=\"l-group det\">{mk_labels(det_idx, 'det')}</div>
            <div class=\"l-group pas\">{mk_labels(pas_idx, 'pas')}</div>
            <div class=\"l-group pro\">{mk_labels(pro_idx, 'pro')}</div>
          </div>
          <div class=\"dist-brackets\">
            <div class=\"bracket det\">Detractors ({sprintf('%.0f%%', dat$detractors_pct)})</div>
            <div class=\"bracket pas\">Passives ({sprintf('%.0f%%', dat$passives_pct)})</div>
            <div class=\"bracket pro\">Promoters ({sprintf('%.0f%%', dat$promoters_pct)})</div>
          </div>
        </div>
      </div>"
    )

    css <- env$enps_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_heatmap_data <- function(heatmap_data) {
    if (!is.list(heatmap_data)) {
      stop("`heatmap_data` must be a named list.", call. = FALSE)
    }
    if (!("tables" %in% names(heatmap_data)) || !is.list(heatmap_data$tables) || length(heatmap_data$tables) < 1L) {
      stop("`heatmap_data$tables` must be a non-empty named list of data frames.", call. = FALSE)
    }

    tables <- heatmap_data$tables
    if (is.null(names(tables))) {
      stop("`heatmap_data$tables` must be named (card title per table).", call. = FALSE)
    }
    bad_names <- names(tables) == "" | is.na(names(tables))
    if (any(bad_names)) {
      stop("All `heatmap_data$tables` entries must have non-empty names.", call. = FALSE)
    }

    for (i in seq_along(tables)) {
      tbl <- tables[[i]]
      if (!is.data.frame(tbl) || nrow(tbl) < 1L || ncol(tbl) < 2L) {
        stop(
          sprintf("Heatmap table `%s` must be a data frame with >=1 row and >=2 columns.", names(tables)[[i]]),
          call. = FALSE
        )
      }
    }
    invisible(heatmap_data)
  }

  env$normalize_heatmap_data <- function(heatmap_data) {
    env$validate_heatmap_data(heatmap_data)

    title <- if ("title" %in% names(heatmap_data)) as.character(heatmap_data$title[[1]]) else "Internal Comparisons Heatmap"
    subtitle <- if ("subtitle" %in% names(heatmap_data)) as.character(heatmap_data$subtitle[[1]]) else ""
    legend_low <- if ("legend_low" %in% names(heatmap_data)) as.character(heatmap_data$legend_low[[1]]) else "Below Avg"
    legend_high <- if ("legend_high" %in% names(heatmap_data)) as.character(heatmap_data$legend_high[[1]]) else "Above Avg"

    tables <- lapply(seq_along(heatmap_data$tables), function(i) {
      table_name <- names(heatmap_data$tables)[[i]]
      tbl <- heatmap_data$tables[[i]]
      row_label_col <- names(tbl)[[1]]
      group_cols <- names(tbl)[-1]

      out <- tbl
      out[[row_label_col]] <- as.character(out[[row_label_col]])
      for (col in group_cols) {
        out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
      }
      names(out)[[1]] <- "Category"

      list(
        title = table_name,
        data = out
      )
    })

    list(
      title = title,
      subtitle = subtitle,
      legend_low = legend_low,
      legend_high = legend_high,
      tables = tables
    )
  }

  env$heatmap_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "heatmap")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-heatmap-root {{
  --hm-slide-bg: {c$heatmap_slide_bg};
  --hm-title: {c$heatmap_title};
  --hm-subtitle: {c$heatmap_subtitle};
  --hm-legend-text: {c$heatmap_legend_text};
  --hm-legend-start: {c$heatmap_legend_start};
  --hm-legend-mid: {c$heatmap_legend_mid};
  --hm-legend-end: {c$heatmap_legend_end};
  --hm-card-bg: {c$heatmap_card_bg};
  --hm-card-border: {c$heatmap_card_border};
  --hm-card-shadow: {c$heatmap_card_shadow};
  --hm-card-title: {c$heatmap_card_title};
  --hm-header-text: {c$heatmap_table_header_text};
  --hm-header-border: {c$heatmap_table_header_border};
  --hm-row-label: {c$heatmap_row_label_text};
  --hm-row-border: {c$heatmap_row_border};
  --hm-pill-text: {c$heatmap_pill_text};
  --hm-pill-na-bg: {c$heatmap_pill_na_bg};
  --hm-pill-na-text: {c$heatmap_pill_na_text};
  --hm-pill-na-border: {c$heatmap_pill_na_border};
  --hm-pill-pos-strong: {c$heatmap_pill_pos_strong};
  --hm-pill-pos-soft: {c$heatmap_pill_pos_soft};
  --hm-pill-neutral: {c$heatmap_pill_neutral};
  --hm-pill-neg-soft: {c$heatmap_pill_neg_soft};
  --hm-pill-neg-strong: {c$heatmap_pill_neg_strong};
  width: 1400px;
  min-height: 720px;
  background: var(--hm-slide-bg);
  padding: 30px 32px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  flex-direction: column;
  gap: 24px;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .hm-header {{ display: flex; justify-content: space-between; align-items: flex-end; gap: 12px; }}
{scope} .hm-title {{ margin: 0; font-size: 22px; font-weight: 900; color: var(--hm-title); text-transform: uppercase; letter-spacing: 0.4px; }}
{scope} .hm-subtitle {{ margin: 4px 0 0; font-size: 14px; color: var(--hm-subtitle); }}
{scope} .hm-legend {{ display: flex; align-items: center; gap: 12px; font-size: 11px; font-weight: 800; text-transform: uppercase; color: var(--hm-legend-text); }}
{scope} .hm-spectrum {{ width: 150px; height: 8px; border-radius: 5px; background: linear-gradient(to right, var(--hm-legend-start), var(--hm-legend-mid), var(--hm-legend-end)); }}
{scope} .hm-grid {{ display: grid; grid-template-columns: 1fr 0.92fr; gap: 24px; align-items: flex-start; }}
{scope} .hm-col-right {{ display: flex; flex-direction: column; gap: 24px; }}
{scope} .hm-card {{ background: var(--hm-card-bg); border: 1px solid var(--hm-card-border); border-radius: 12px; box-shadow: 0 4px 20px var(--hm-card-shadow); padding: 20px 24px; }}
{scope} .hm-card-title {{ margin: 0 0 14px; padding-bottom: 10px; border-bottom: 2px solid var(--hm-header-border); font-size: 14px; font-weight: 800; text-transform: uppercase; color: var(--hm-card-title); letter-spacing: 0.5px; }}
{scope} .hm-table {{ width: 100%; border-collapse: collapse; table-layout: fixed; }}
{scope} .hm-table th {{ padding: 0 6px 10px; border-bottom: 1px solid var(--hm-header-border); text-transform: uppercase; font-size: 10px; font-weight: 800; color: var(--hm-header-text); text-align: center; }}
{scope} .hm-table th:first-child {{ text-align: left; width: 31%; color: var(--hm-row-label); }}
{scope} .hm-table td {{ padding: 8px 6px; border-bottom: 1px solid var(--hm-row-border); text-align: center; vertical-align: middle; }}
{scope} .hm-table tr:last-child td {{ border-bottom: none; }}
{scope} .hm-table td:first-child {{ text-align: left; font-size: 12px; font-weight: 700; color: var(--hm-row-label); }}
{scope} .hm-pill {{ display: inline-flex; align-items: center; justify-content: center; min-width: 52px; height: 30px; padding: 0 8px; border-radius: 6px; color: var(--hm-pill-text); font-size: 12px; font-weight: 800; }}
{scope} .hm-pill-pos-strong {{ background: var(--hm-pill-pos-strong); }}
{scope} .hm-pill-pos-soft {{ background: var(--hm-pill-pos-soft); }}
{scope} .hm-pill-neutral {{ background: var(--hm-pill-neutral); }}
{scope} .hm-pill-neg-soft {{ background: var(--hm-pill-neg-soft); }}
{scope} .hm-pill-neg-strong {{ background: var(--hm-pill-neg-strong); }}
{scope} .hm-pill-na {{ background: var(--hm-pill-na-bg); color: var(--hm-pill-na-text); border: 1px dashed var(--hm-pill-na-border); font-weight: 700; }}
"
    )
  }

  env$render_heatmap_page <- function(
    heatmap_data,
    id = "ohep-heatmap-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "heatmap"
    )
    dat <- env$normalize_heatmap_data(heatmap_data)

    classify_cell <- function(value, col_mean, max_abs_delta) {
      if (!is.finite(value)) return("hm-pill-na")
      if (!is.finite(col_mean) || !is.finite(max_abs_delta) || max_abs_delta <= 0) return("hm-pill-neutral")
      delta <- value - col_mean
      intensity <- abs(delta) / max_abs_delta
      if (!is.finite(intensity) || intensity < 0.16) return("hm-pill-neutral")
      if (delta > 0) {
        return(if (intensity >= 0.55) "hm-pill-pos-strong" else "hm-pill-pos-soft")
      }
      if (delta < 0) {
        return(if (intensity >= 0.55) "hm-pill-neg-strong" else "hm-pill-neg-soft")
      }
      "hm-pill-neutral"
    }

    build_card <- function(table_obj) {
      df <- table_obj$data
      groups <- names(df)[-1]
      vals <- as.matrix(df[, groups, drop = FALSE])
      col_means <- suppressWarnings(colMeans(vals, na.rm = TRUE))
      col_means[!is.finite(col_means)] <- NA_real_
      centered <- suppressWarnings(sweep(vals, 2, col_means, FUN = "-"))
      max_abs_delta <- suppressWarnings(max(abs(centered), na.rm = TRUE))
      if (!is.finite(max_abs_delta)) max_abs_delta <- 0

      header_html <- glue::glue_collapse(vapply(c("Category", groups), function(h) {
        glue::glue("<th>{env$escape_text(h)}</th>")
      }, character(1)), sep = "")

      row_html <- vapply(seq_len(nrow(df)), function(i) {
        category <- env$escape_text(df$Category[[i]])
        cell_html <- vapply(seq_along(groups), function(j) {
          value <- suppressWarnings(as.numeric(df[[groups[[j]]]][[i]]))
          cls <- classify_cell(value, col_means[[j]], max_abs_delta)
          txt <- if (is.finite(value)) sprintf("%.2f", value) else "NA"
          glue::glue("<td><span class=\"hm-pill {cls}\">{txt}</span></td>")
        }, character(1))
        glue::glue("<tr><td>{category}</td>{glue::glue_collapse(cell_html, sep = '')}</tr>")
      }, character(1))

      glue::glue(
        "<div class=\"hm-card\">
          <h3 class=\"hm-card-title\">{env$escape_text(table_obj$title)}</h3>
          <table class=\"hm-table\">
            <thead><tr>{header_html}</tr></thead>
            <tbody>{glue::glue_collapse(row_html, sep = '')}</tbody>
          </table>
        </div>"
      )
    }

    cards <- vapply(dat$tables, build_card, character(1))
    first_card <- cards[[1]]
    right_cards <- if (length(cards) > 1L) glue::glue_collapse(cards[-1], sep = "") else ""

    grid_html <- if (length(cards) <= 1L) {
      glue::glue("<div class=\"hm-grid\"><div>{first_card}</div></div>")
    } else {
      glue::glue("<div class=\"hm-grid\"><div>{first_card}</div><div class=\"hm-col-right\">{right_cards}</div></div>")
    }

    subtitle_html <- if (nzchar(dat$subtitle)) glue::glue("<p class=\"hm-subtitle\">{env$escape_text(dat$subtitle)}</p>") else ""
    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-heatmap-root\">
        <div class=\"hm-header\">
          <div>
            <h2 class=\"hm-title\">{env$escape_text(dat$title)}</h2>
            {subtitle_html}
          </div>
          <div class=\"hm-legend\">
            <span>{env$escape_text(dat$legend_low)}</span>
            <div class=\"hm-spectrum\"></div>
            <span>{env$escape_text(dat$legend_high)}</span>
          </div>
        </div>
        {grid_html}
      </div>"
    )

    css <- env$heatmap_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_item_distribution_data <- function(item_distribution_data) {
    if (!is.list(item_distribution_data)) {
      stop("`item_distribution_data` must be a named list.", call. = FALSE)
    }
    if (!("items" %in% names(item_distribution_data)) || !is.data.frame(item_distribution_data$items)) {
      stop("`item_distribution_data$items` must be a data frame.", call. = FALSE)
    }
    items <- item_distribution_data$items
    required <- c("label", "mean", "disagree_pct", "neutral_pct", "agree_pct", "vs_industry", "vs_prior")
    missing <- setdiff(required, names(items))
    if (length(missing) > 0L) {
      stop(sprintf("`items` is missing required columns: %s.", paste(missing, collapse = ", ")), call. = FALSE)
    }
    if (nrow(items) < 1L) {
      stop("`items` must contain at least one row.", call. = FALSE)
    }
    for (i in seq_len(nrow(items))) {
      env$normalize_sentiment(
        suppressWarnings(as.numeric(items$disagree_pct[[i]])),
        suppressWarnings(as.numeric(items$neutral_pct[[i]])),
        suppressWarnings(as.numeric(items$agree_pct[[i]]))
      )
    }
    invisible(item_distribution_data)
  }

  env$normalize_item_distribution_data <- function(item_distribution_data) {
    env$validate_item_distribution_data(item_distribution_data)

    summary <- if ("summary" %in% names(item_distribution_data) && is.data.frame(item_distribution_data$summary) && nrow(item_distribution_data$summary) >= 1L) {
      item_distribution_data$summary[1, , drop = FALSE]
    } else {
      data.frame(stringsAsFactors = FALSE)
    }

    title <- if ("title" %in% names(summary)) as.character(summary$title[[1]]) else "Item-Level Sentiment Analysis"
    subtitle <- if ("subtitle" %in% names(summary)) as.character(summary$subtitle[[1]]) else ""
    benchmark_label <- if ("benchmark_label" %in% names(summary)) as.character(summary$benchmark_label[[1]]) else "vs Industry"
    prior_label <- if ("prior_label" %in% names(summary)) as.character(summary$prior_label[[1]]) else "vs Prior"

    items <- item_distribution_data$items
    out <- data.frame(
      label = as.character(items$label),
      mean = suppressWarnings(as.numeric(items$mean)),
      disagree_pct = suppressWarnings(as.numeric(items$disagree_pct)),
      neutral_pct = suppressWarnings(as.numeric(items$neutral_pct)),
      agree_pct = suppressWarnings(as.numeric(items$agree_pct)),
      vs_industry = suppressWarnings(as.numeric(items$vs_industry)),
      vs_prior = suppressWarnings(as.numeric(items$vs_prior)),
      stringsAsFactors = FALSE
    )

    list(
      title = title,
      subtitle = subtitle,
      benchmark_label = benchmark_label,
      prior_label = prior_label,
      items = out
    )
  }

  env$item_distribution_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "item_distribution")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-itemdist-root {{
  --itemdist-slide-bg: {c$itemdist_slide_bg};
  --itemdist-card-bg: {c$itemdist_card_bg};
  --itemdist-card-border: {c$itemdist_card_border};
  --itemdist-card-shadow: {c$itemdist_card_shadow};
  --itemdist-title: {c$itemdist_title};
  --itemdist-subtitle: {c$itemdist_subtitle};
  --itemdist-legend-text: {c$itemdist_legend_text};
  --itemdist-header-label: {c$itemdist_header_label};
  --itemdist-row-label: {c$itemdist_row_label};
  --itemdist-row-border: {c$itemdist_row_border};
  --itemdist-mean-text: {c$itemdist_mean_text};
  --itemdist-stack-bg: {c$itemdist_stack_bg};
  --itemdist-agree: {c$favorability_agree};
  --itemdist-neutral: {c$favorability_neutral};
  --itemdist-disagree: {c$favorability_disagree};
  --itemdist-delta-pos-bg: {c$delta_pos_bg};
  --itemdist-delta-pos-text: {c$delta_pos_text};
  --itemdist-delta-neg-bg: {c$delta_neg_bg};
  --itemdist-delta-neg-text: {c$delta_neg_text};
  --itemdist-delta-neu-bg: {c$delta_neu_bg};
  --itemdist-delta-neu-text: {c$delta_neu_text};
  width: 1280px;
  min-height: 720px;
  background: var(--itemdist-slide-bg);
  padding: 34px 36px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  justify-content: center;
  align-items: center;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .itemdist-card {{
  width: 1150px;
  background: var(--itemdist-card-bg);
  border: 1px solid var(--itemdist-card-border);
  border-radius: 12px;
  box-shadow: 0 4px 20px var(--itemdist-card-shadow);
  padding: 36px 38px;
}}
{scope} .itemdist-header {{ display: flex; justify-content: space-between; align-items: flex-end; margin-bottom: 30px; gap: 10px; }}
{scope} .itemdist-title {{ margin: 0; font-size: 24px; font-weight: 900; text-transform: uppercase; letter-spacing: -0.4px; color: var(--itemdist-title); }}
{scope} .itemdist-subtitle {{ margin: 4px 0 0; font-size: 13px; color: var(--itemdist-subtitle); }}
{scope} .itemdist-legend {{ display: flex; gap: 16px; font-size: 11px; font-weight: 700; color: var(--itemdist-legend-text); text-transform: uppercase; }}
{scope} .itemdist-leg-item {{ display: flex; align-items: center; gap: 6px; }}
{scope} .itemdist-leg-dot {{ width: 10px; height: 10px; border-radius: 2px; }}
{scope} .itemdist-leg-agree {{ background: var(--itemdist-agree); }}
{scope} .itemdist-leg-neutral {{ background: var(--itemdist-neutral); }}
{scope} .itemdist-leg-disagree {{ background: var(--itemdist-disagree); }}
{scope} .itemdist-header-row, {scope} .itemdist-row {{
  display: grid;
  grid-template-columns: 280px 80px 1fr 100px 100px;
  gap: 20px;
  align-items: center;
}}
{scope} .itemdist-header-row {{ padding-bottom: 12px; border-bottom: 2px solid var(--itemdist-card-border); margin-bottom: 8px; }}
{scope} .itemdist-h-lbl {{ font-size: 11px; font-weight: 800; color: var(--itemdist-header-label); text-transform: uppercase; letter-spacing: 1px; text-align: center; }}
{scope} .itemdist-h-lbl:first-child {{ text-align: left; }}
{scope} .itemdist-row {{ padding: 14px 0; border-bottom: 1px solid var(--itemdist-row-border); }}
{scope} .itemdist-row:last-child {{ border-bottom: none; }}
{scope} .itemdist-label {{ font-size: 13px; line-height: 1.35; color: var(--itemdist-row-label); font-weight: 600; }}
{scope} .itemdist-mean {{ font-size: 15px; font-weight: 800; color: var(--itemdist-mean-text); text-align: center; }}
{scope} .itemdist-stack {{ height: 32px; display: flex; border-radius: 6px; overflow: hidden; background: var(--itemdist-stack-bg); }}
{scope} .itemdist-segment {{ display: flex; align-items: center; justify-content: center; color: #FFFFFF; font-size: 11px; font-weight: 800; }}
{scope} .itemdist-disagree {{ background: var(--itemdist-disagree); }}
{scope} .itemdist-neutral {{ background: var(--itemdist-neutral); }}
{scope} .itemdist-agree {{ background: var(--itemdist-agree); }}
{scope} .itemdist-delta-wrap {{ display: flex; justify-content: center; }}
{scope} .itemdist-delta {{ display: inline-flex; align-items: center; justify-content: center; width: 65px; padding: 6px 0; border-radius: 6px; font-size: 11px; font-weight: 800; }}
{scope} .itemdist-dp-pos {{ background: var(--itemdist-delta-pos-bg); color: var(--itemdist-delta-pos-text); }}
{scope} .itemdist-dp-neg {{ background: var(--itemdist-delta-neg-bg); color: var(--itemdist-delta-neg-text); }}
{scope} .itemdist-dp-neu {{ background: var(--itemdist-delta-neu-bg); color: var(--itemdist-delta-neu-text); }}
"
    )
  }

  env$render_item_distribution_page <- function(
    item_distribution_data,
    id = "ohep-item-distribution-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "item_distribution"
    )
    dat <- env$normalize_item_distribution_data(item_distribution_data)

    delta_class <- function(val) {
      if (!is.finite(val) || val == 0) return("itemdist-dp-neu")
      if (val > 0) "itemdist-dp-pos" else "itemdist-dp-neg"
    }
    delta_text <- function(val) {
      if (!is.finite(val)) return("-")
      paste0(env$format_signed(val), "%")
    }

    rows_html <- vapply(seq_len(nrow(dat$items)), function(i) {
      r <- dat$items[i, , drop = FALSE]
      sent <- env$normalize_sentiment(r$disagree_pct[[1]], r$neutral_pct[[1]], r$agree_pct[[1]])
      disagree_lbl <- if (sent[[1]] >= 8) paste0(round(sent[[1]]), "%") else ""
      neutral_lbl <- if (sent[[2]] >= 8) paste0(round(sent[[2]]), "%") else ""
      agree_lbl <- if (sent[[3]] >= 8) paste0(round(sent[[3]]), "%") else ""

      glue::glue(
        "<div class=\"itemdist-row\">
          <div class=\"itemdist-label\">{env$escape_text(r$label[[1]])}</div>
          <div class=\"itemdist-mean\">{sprintf('%.2f', r$mean[[1]])}</div>
          <div class=\"itemdist-stack\">
            <div class=\"itemdist-segment itemdist-disagree\" style=\"width:{sent[[1]]}%;\">{disagree_lbl}</div>
            <div class=\"itemdist-segment itemdist-neutral\" style=\"width:{sent[[2]]}%;\">{neutral_lbl}</div>
            <div class=\"itemdist-segment itemdist-agree\" style=\"width:{sent[[3]]}%;\">{agree_lbl}</div>
          </div>
          <div class=\"itemdist-delta-wrap\"><span class=\"itemdist-delta {delta_class(r$vs_industry[[1]])}\">{delta_text(r$vs_industry[[1]])}</span></div>
          <div class=\"itemdist-delta-wrap\"><span class=\"itemdist-delta {delta_class(r$vs_prior[[1]])}\">{delta_text(r$vs_prior[[1]])}</span></div>
        </div>"
      )
    }, character(1))

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-itemdist-root\">
        <div class=\"itemdist-card\">
          <div class=\"itemdist-header\">
            <div>
              <h2 class=\"itemdist-title\">{env$escape_text(dat$title)}</h2>
              <p class=\"itemdist-subtitle\">{env$escape_text(dat$subtitle)}</p>
            </div>
            <div class=\"itemdist-legend\">
              <div class=\"itemdist-leg-item\"><span class=\"itemdist-leg-dot itemdist-leg-disagree\"></span>Disagree</div>
              <div class=\"itemdist-leg-item\"><span class=\"itemdist-leg-dot itemdist-leg-neutral\"></span>Neutral</div>
              <div class=\"itemdist-leg-item\"><span class=\"itemdist-leg-dot itemdist-leg-agree\"></span>Agree</div>
            </div>
          </div>
          <div class=\"itemdist-header-row\">
            <div class=\"itemdist-h-lbl\">Survey Item</div>
            <div class=\"itemdist-h-lbl\">Mean</div>
            <div class=\"itemdist-h-lbl\">Sentiment Distribution</div>
            <div class=\"itemdist-h-lbl\">{env$escape_text(dat$benchmark_label)}</div>
            <div class=\"itemdist-h-lbl\">{env$escape_text(dat$prior_label)}</div>
          </div>
          {glue::glue_collapse(rows_html, sep = '')}
        </div>
      </div>"
    )

    css <- env$item_distribution_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_model_data <- function(model_data) {
    if (!is.list(model_data)) {
      stop("`model_data` must be a named list.", call. = FALSE)
    }
    if (!("fundamentals" %in% names(model_data)) || !is.data.frame(model_data$fundamentals)) {
      stop("`model_data$fundamentals` must be a data frame.", call. = FALSE)
    }
    if (!("outcomes" %in% names(model_data)) || !is.data.frame(model_data$outcomes)) {
      stop("`model_data$outcomes` must be a data frame.", call. = FALSE)
    }

    check_section <- function(df, section_name) {
      required <- c("label", "percentile")
      missing <- setdiff(required, names(df))
      if (length(missing) > 0L) {
        stop(
          sprintf("`model_data$%s` missing required columns: %s.", section_name, paste(missing, collapse = ", ")),
          call. = FALSE
        )
      }
      if (nrow(df) < 1L) {
        stop(sprintf("`model_data$%s` must have at least one row.", section_name), call. = FALSE)
      }
      pct <- suppressWarnings(as.numeric(df$percentile))
      if (any(!is.finite(pct))) {
        stop(sprintf("`model_data$%s$percentile` must be numeric/non-missing.", section_name), call. = FALSE)
      }
      if (any(pct < 0 | pct > 100)) {
        stop(sprintf("`model_data$%s$percentile` must be in 0..100.", section_name), call. = FALSE)
      }
      invisible(TRUE)
    }

    check_section(model_data$fundamentals, "fundamentals")
    check_section(model_data$outcomes, "outcomes")
    invisible(model_data)
  }

  env$normalize_model_data <- function(model_data) {
    env$validate_model_data(model_data)

    summary <- if ("summary" %in% names(model_data) && is.data.frame(model_data$summary) && nrow(model_data$summary) >= 1L) {
      model_data$summary[1, , drop = FALSE]
    } else {
      data.frame(stringsAsFactors = FALSE)
    }

    to_df <- function(df, default_shape) {
      out <- data.frame(
        label = as.character(df$label),
        percentile = suppressWarnings(as.numeric(df$percentile)),
        prior_percentile = if ("prior_percentile" %in% names(df)) suppressWarnings(as.numeric(df$prior_percentile)) else NA_real_,
        raw_avg = if ("raw_avg" %in% names(df)) suppressWarnings(as.numeric(df$raw_avg)) else NA_real_,
        delta = if ("delta" %in% names(df)) suppressWarnings(as.numeric(df$delta)) else NA_real_,
        shape = if ("shape" %in% names(df)) as.character(df$shape) else default_shape,
        stringsAsFactors = FALSE
      )
      out$shape[is.na(out$shape) | out$shape == ""] <- default_shape
      out
    }

    list(
      title = if ("title" %in% names(summary)) as.character(summary$title[[1]]) else "Health Diagnostics",
      subtitle = if ("subtitle" %in% names(summary)) as.character(summary$subtitle[[1]]) else "",
      fundamentals_label = if ("fundamentals_label" %in% names(summary)) as.character(summary$fundamentals_label[[1]]) else "",
      outcomes_label = if ("outcomes_label" %in% names(summary)) as.character(summary$outcomes_label[[1]]) else "Business Outcomes",
      raw_avg_label = if ("raw_avg_label" %in% names(summary)) as.character(summary$raw_avg_label[[1]]) else "Raw Avg",
      delta_label = if ("delta_label" %in% names(summary)) as.character(summary$delta_label[[1]]) else "vs Prior",
      thresholds = c(35, 50, 65, 90),
      fundamentals = to_df(model_data$fundamentals, default_shape = "circle"),
      outcomes = to_df(model_data$outcomes, default_shape = "diamond")
    )
  }

  env$model_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "model")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-model-root {{
  --model-slide-bg: {c$model_slide_bg};
  --model-card-bg: {c$model_card_bg};
  --model-card-border: {c$model_card_border};
  --model-card-shadow: {c$model_card_shadow};
  --model-header-accent: {c$model_header_accent};
  --model-title: {c$model_title};
  --model-subtitle: {c$model_subtitle};
  --model-section-title: {c$model_section_title};
  --model-section-rule: {c$model_section_rule};
  --model-metric-header: {c$model_metric_header};
  --model-label: {c$model_label};
  --model-track-bg: {c$model_track_bg};
  --model-stem: {c$model_stem};
  --model-line: {c$model_line};
  --model-line-baseline: {c$model_line_baseline};
  --model-line-label: {c$model_line_label};
  --model-metric-raw: {c$model_metric_raw};
  --model-delta-pos: {c$model_delta_pos};
  --model-delta-neg: {c$model_delta_neg};
  --model-delta-neu: {c$model_delta_neu};
  --model-zone-ni: {c$model_zone_ni};
  --model-zone-is: {c$model_zone_is};
  --model-zone-as: {c$model_zone_as};
  --model-zone-il: {c$model_zone_il};
  --model-point-ni: {c$model_point_ni};
  --model-point-is: {c$model_point_is};
  --model-point-as: {c$model_point_as};
  --model-point-il: {c$model_point_il};
  --model-point-border: {c$model_point_border};
  --model-point-ghost-bg: {c$model_point_ghost_bg};
  --model-point-ghost-border: {c$model_point_ghost_border};
  --model-legend-text-on: {c$model_legend_text_on};
  width: 1280px;
  min-height: 760px;
  background: var(--model-slide-bg);
  padding: 28px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  justify-content: center;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .model-card {{
  width: 1150px;
  background: var(--model-card-bg);
  border: 1px solid var(--model-card-border);
  border-radius: 16px;
  box-shadow: 0 10px 25px var(--model-card-shadow);
  padding: 34px;
}}
{scope} .model-header {{ margin-bottom: 12px; border-left: 4px solid var(--model-header-accent); padding-left: 16px; }}
{scope} .model-title {{ margin: 0 0 6px; font-size: 26px; font-weight: 800; color: var(--model-title); }}
{scope} .model-subtitle {{ margin: 0; font-size: 14px; color: var(--model-subtitle); }}
{scope} .model-block {{ position: relative; margin-top: 32px; margin-bottom: 28px; padding-top: 24px; }}
{scope} .model-block:last-of-type {{ margin-bottom: 10px; }}
{scope} .model-section-head {{ display: flex; align-items: flex-end; border-bottom: 2px solid var(--model-section-rule); padding-bottom: 10px; margin-bottom: 0; position: absolute; top: -32px; left: 0; right: 0; }}
{scope} .model-section-title {{ width: 80%; font-size: 15px; font-weight: 800; color: var(--model-section-title); text-transform: uppercase; letter-spacing: 1px; }}
{scope} .model-metric-head {{ width: 20%; display: flex; justify-content: flex-end; gap: 24px; padding-right: 14px; color: var(--model-metric-header); font-size: 10px; font-weight: 800; text-transform: uppercase; letter-spacing: .5px; }}
{scope} .model-metric-head span {{ width: 54px; text-align: right; }}
{scope} .model-zones, {scope} .model-lines {{ position: absolute; left: 25%; width: 55%; }}
{scope} .model-zones {{ top: 24px; bottom: 0; display: grid; grid-template-columns: 35fr 30fr 25fr 10fr; border-radius: 12px; overflow: hidden; z-index: 0; }}
{scope} .model-zone {{ opacity: 1; }}
{scope} .model-zone-ni {{ background: var(--model-zone-ni); }}
{scope} .model-zone-is {{ background: var(--model-zone-is); }}
{scope} .model-zone-as {{ background: var(--model-zone-as); }}
{scope} .model-zone-il {{ background: var(--model-zone-il); }}
{scope} .model-lines {{ top: 0; bottom: 0; z-index: 1; }}
{scope} .model-line {{ position: absolute; top: 0; bottom: 0; width: 2px; transform: translateX(-50%); background: var(--model-line); opacity: .35; }}
{scope} .model-line-baseline {{ background: var(--model-line-baseline); opacity: 1; z-index: 3; }}
{scope} .model-line-label {{ position: absolute; top: 0; transform: translateX(-50%); background: var(--model-card-bg); padding: 0 4px; color: var(--model-line-label); font-size: 11px; font-weight: 800; }}
{scope} .model-line-baseline .model-line-label {{ color: var(--model-line-baseline); }}
{scope} .model-row-wrap {{ position: relative; z-index: 2; padding-top: 14px; padding-bottom: 12px; }}
{scope} .model-row {{ display: flex; align-items: center; margin-bottom: 12px; }}
{scope} .model-row:last-child {{ margin-bottom: 0; }}
{scope} .model-label {{ width: 25%; text-align: right; padding-right: 30px; color: var(--model-label); font-size: 14px; font-weight: 600; }}
{scope} .model-track-wrap {{ width: 55%; position: relative; height: 24px; }}
{scope} .model-track {{ position: absolute; top: 50%; transform: translateY(-50%); width: 100%; height: 4px; border-radius: 2px; background: var(--model-track-bg); }}
{scope} .model-stem {{ position: absolute; top: 50%; transform: translateY(-50%); height: 4px; border-radius: 2px; background: var(--model-stem); }}
{scope} .model-point {{ position: absolute; top: 50%; transform: translate(-50%, -50%); z-index: 4; box-shadow: 0 2px 4px rgba(0,0,0,0.15); }}
{scope} .model-point-circle {{ width: 16px; height: 16px; border-radius: 50%; border: 2px solid var(--model-point-border); }}
{scope} .model-point-diamond {{ width: 14px; height: 14px; transform: translate(-50%, -50%) rotate(45deg); border: 2px solid var(--model-point-border); }}
{scope} .model-point-ghost-circle {{ width: 12px; height: 12px; border-radius: 50%; background: var(--model-point-ghost-bg); border: 2px solid var(--model-point-ghost-border); box-shadow: none; z-index: 3; }}
{scope} .model-point-ghost-diamond {{ width: 10px; height: 10px; transform: translate(-50%, -50%) rotate(45deg); background: var(--model-point-ghost-bg); border: 2px solid var(--model-point-ghost-border); box-shadow: none; z-index: 3; }}
{scope} .model-zone-bg-ni {{ background: var(--model-point-ni); }}
{scope} .model-zone-bg-is {{ background: var(--model-point-is); }}
{scope} .model-zone-bg-as {{ background: var(--model-point-as); }}
{scope} .model-zone-bg-il {{ background: var(--model-point-il); }}
{scope} .model-metrics {{ width: 20%; display: flex; justify-content: flex-end; align-items: center; gap: 24px; padding-right: 14px; }}
{scope} .model-metric-raw {{ width: 54px; text-align: right; color: var(--model-metric-raw); font-size: 14px; font-weight: 800; font-variant-numeric: tabular-nums; }}
{scope} .model-metric-delta {{ width: 54px; text-align: right; font-size: 13px; font-weight: 700; font-variant-numeric: tabular-nums; }}
{scope} .model-delta-pos {{ color: var(--model-delta-pos); }}
{scope} .model-delta-neg {{ color: var(--model-delta-neg); }}
{scope} .model-delta-neu {{ color: var(--model-delta-neu); }}
{scope} .model-legend {{ display: grid; grid-template-columns: 35fr 30fr 25fr 10fr; gap: 4px; margin-left: 25%; width: 55%; margin-top: 12px; }}
{scope} .model-legend-card {{ border-radius: 6px; padding: 10px 4px; text-align: center; color: var(--model-legend-text-on); text-transform: uppercase; letter-spacing: .5px; line-height: 1.2; font-size: 10px; font-weight: 800; }}
{scope} .model-legend-ni {{ background: var(--model-zone-ni); }}
{scope} .model-legend-is {{ background: var(--model-zone-is); }}
{scope} .model-legend-as {{ background: var(--model-zone-as); }}
{scope} .model-legend-il {{ background: var(--model-zone-il); }}
"
    )
  }

  env$render_model_page <- function(
    model_data,
    id = "ohep-model-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "model"
    )
    dat <- env$normalize_model_data(model_data)

    classify_zone <- function(percentile) {
      if (percentile < 35) "ni" else if (percentile < 65) "is" else if (percentile < 90) "as" else "il"
    }
    pct_pos <- function(x) sprintf("%.1f%%", max(0, min(100, as.numeric(x))))
    delta_text <- function(val) {
      if (!is.finite(val)) return("-")
      if (val == 0) return("0.00")
      sprintf("%+.2f", val)
    }
    delta_class <- function(val) {
      if (!is.finite(val) || val == 0) "model-delta-neu" else if (val > 0) "model-delta-pos" else "model-delta-neg"
    }

    render_rows <- function(df, is_outcomes = FALSE) {
      vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        zone <- classify_zone(row$percentile[[1]])
        prior <- suppressWarnings(as.numeric(row$prior_percentile[[1]]))
        has_prior <- is.finite(prior) && prior >= 0 && prior <= 100
        current <- suppressWarnings(as.numeric(row$percentile[[1]]))
        stem_left <- if (has_prior) min(prior, current) else NA_real_
        stem_width <- if (has_prior) abs(current - prior) else NA_real_
        shape <- tolower(trimws(as.character(row$shape[[1]])))
        is_diamond <- identical(shape, "diamond") || is_outcomes
        point_class <- if (is_diamond) "model-point-diamond" else "model-point-circle"
        ghost_class <- if (is_diamond) "model-point-ghost-diamond" else "model-point-ghost-circle"
        stem_html <- if (has_prior && stem_width > 0) {
          glue::glue("<div class=\"model-stem\" style=\"left:{pct_pos(stem_left)}; width:{pct_pos(stem_width)};\"></div>")
        } else ""
        ghost_html <- if (has_prior) {
          glue::glue("<div class=\"model-point {ghost_class}\" style=\"left:{pct_pos(prior)};\"></div>")
        } else ""
        raw_txt <- if (is.finite(row$raw_avg[[1]])) sprintf("%.2f", row$raw_avg[[1]]) else "-"
        d_txt <- delta_text(row$delta[[1]])
        d_class <- delta_class(row$delta[[1]])

        glue::glue(
          "<div class=\"model-row\">
            <div class=\"model-label\">{env$escape_text(row$label[[1]])}</div>
            <div class=\"model-track-wrap\">
              <div class=\"model-track\"></div>
              {stem_html}
              {ghost_html}
              <div class=\"model-point {point_class} model-zone-bg-{zone}\" style=\"left:{pct_pos(current)};\"></div>
            </div>
            <div class=\"model-metrics\">
              <div class=\"model-metric-raw\">{raw_txt}</div>
              <div class=\"model-metric-delta {d_class}\">{d_txt}</div>
            </div>
          </div>"
        )
      }, character(1))
    }

    render_block <- function(section_title, rows_html, show_metrics = TRUE) {
      section_title_html <- if (nzchar(trimws(section_title))) env$escape_text(section_title) else "&nbsp;"
      metric_head <- if (show_metrics) {
        glue::glue("<div class=\"model-metric-head\"><span>{env$escape_text(dat$raw_avg_label)}</span><span>{env$escape_text(dat$delta_label)}</span></div>")
      } else {
        "<div class=\"model-metric-head\"><span></span><span></span></div>"
      }
      glue::glue(
        "<div class=\"model-block\">
          <div class=\"model-section-head\">
            <div class=\"model-section-title\">{section_title_html}</div>
            {metric_head}
          </div>
          <div class=\"model-zones\">
            <div class=\"model-zone model-zone-ni\"></div>
            <div class=\"model-zone model-zone-is\"></div>
            <div class=\"model-zone model-zone-as\"></div>
            <div class=\"model-zone model-zone-il\"></div>
          </div>
          <div class=\"model-lines\">
            <div class=\"model-line\" style=\"left:35%;\"><div class=\"model-line-label\">35th</div></div>
            <div class=\"model-line model-line-baseline\" style=\"left:50%;\"><div class=\"model-line-label\">50th</div></div>
            <div class=\"model-line\" style=\"left:65%;\"><div class=\"model-line-label\">65th</div></div>
            <div class=\"model-line\" style=\"left:90%;\"><div class=\"model-line-label\">90th</div></div>
          </div>
          <div class=\"model-row-wrap\">
            {glue::glue_collapse(rows_html, sep = '')}
          </div>
        </div>"
      )
    }

    f_rows <- render_rows(dat$fundamentals, is_outcomes = FALSE)
    o_rows <- render_rows(dat$outcomes, is_outcomes = TRUE)

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-model-root\">
        <div class=\"model-card\">
          <div class=\"model-header\">
            <h1 class=\"model-title\">{env$escape_text(dat$title)}</h1>
            {if (nzchar(trimws(dat$subtitle))) glue::glue('<p class=\"model-subtitle\">{env$escape_text(dat$subtitle)}</p>') else ''}
          </div>
          {render_block(dat$fundamentals_label, f_rows, show_metrics = TRUE)}
          {render_block(dat$outcomes_label, o_rows, show_metrics = FALSE)}
          <div class=\"model-legend\">
            <div class=\"model-legend-card model-legend-ni\">Needs Improvement</div>
            <div class=\"model-legend-card model-legend-is\">Industry Standard</div>
            <div class=\"model-legend-card model-legend-as\">Above Standard</div>
            <div class=\"model-legend-card model-legend-il\">Industry Leader</div>
          </div>
        </div>
      </div>"
    )

    css <- env$model_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_demo_bipolar_split <- function(data) {
    if (!is.data.frame(data) || nrow(data) != 2L) {
      stop("`demo_bipolar_split$data` must be a data frame with exactly 2 rows.", call. = FALSE)
    }
    req <- c("label", "count", "pct")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_bipolar_split$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    pct <- suppressWarnings(as.numeric(data$pct))
    cnt <- suppressWarnings(as.numeric(data$count))
    if (any(!is.finite(pct)) || any(pct < 0)) stop("`pct` must be non-negative numeric values.", call. = FALSE)
    if (any(!is.finite(cnt)) || any(cnt < 0)) stop("`count` must be non-negative numeric values.", call. = FALSE)
    if (abs(sum(pct) - 100) > 1) stop("`pct` values must sum to 100 (+/- 1 for rounding).", call. = FALSE)
    if (any(trimws(as.character(data$label)) == "")) stop("`label` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$validate_demo_categorical_bar <- function(data) {
    if (!is.data.frame(data) || nrow(data) < 1L) {
      stop("`demo_categorical_bar$data` must be a non-empty data frame.", call. = FALSE)
    }
    req <- c("category", "value")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_categorical_bar$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    val <- suppressWarnings(as.numeric(data$value))
    if (any(!is.finite(val)) || any(val < 0)) stop("`value` must be non-negative numeric values.", call. = FALSE)
    if (any(trimws(as.character(data$category)) == "")) stop("`category` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$validate_demo_categorical_tree <- function(data) {
    if (!is.data.frame(data) || nrow(data) < 1L) {
      stop("`demo_categorical_tree$data` must be a non-empty data frame.", call. = FALSE)
    }
    req <- c("category", "value")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_categorical_tree$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    val <- suppressWarnings(as.numeric(data$value))
    if (any(!is.finite(val)) || any(val < 0)) stop("`value` must be non-negative numeric values.", call. = FALSE)
    if (any(trimws(as.character(data$category)) == "")) stop("`category` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$validate_demo_ordinal_bar <- function(data) {
    if (!is.data.frame(data) || nrow(data) < 1L) {
      stop("`demo_ordinal_bar$data` must be a non-empty data frame.", call. = FALSE)
    }
    req <- c("label", "pct")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_ordinal_bar$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    pct <- suppressWarnings(as.numeric(data$pct))
    if (any(!is.finite(pct)) || any(pct < 0)) stop("`pct` must be non-negative numeric values.", call. = FALSE)
    if (abs(sum(pct) - 100) > 1) stop("`pct` values must sum to 100 (+/- 1 for rounding).", call. = FALSE)
    if (any(trimws(as.character(data$label)) == "")) stop("`label` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$normalize_demo_bipolar_split <- function(panel) {
    env$validate_demo_bipolar_split(panel$data)
    d <- panel$data
    d$label <- as.character(d$label)
    d$count <- suppressWarnings(as.numeric(d$count))
    d$pct <- suppressWarnings(as.numeric(d$pct))
    list(
      type = "bipolar_split",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      data = d
    )
  }

  env$normalize_demo_categorical_bar <- function(panel) {
    env$validate_demo_categorical_bar(panel$data)
    d <- panel$data
    d$category <- as.character(d$category)
    d$value <- suppressWarnings(as.numeric(d$value))
    d$color_key <- if ("color_key" %in% names(d)) as.character(d$color_key) else NA_character_
    sort_desc <- !isFALSE(panel$sort_desc)
    if (sort_desc) {
      d <- d[order(-d$value, d$category), , drop = FALSE]
    }
    max_val <- max(d$value, na.rm = TRUE)
    d$width_pct <- if (is.finite(max_val) && max_val > 0) (d$value / max_val) * 100 else rep(0, nrow(d))
    palette_map <- if (all(is.na(d$color_key) | trimws(d$color_key) == "")) {
      seq_len(nrow(d))
    } else {
      as.integer(as.factor(ifelse(is.na(d$color_key) | trimws(d$color_key) == "", d$category, d$color_key)))
    }
    d$palette_ix <- ((palette_map - 1L) %% 6L) + 1L
    list(
      type = "categorical_bar",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      data = d
    )
  }

  env$normalize_demo_categorical_tree <- function(panel) {
    env$validate_demo_categorical_tree(panel$data)
    d <- panel$data
    d$category <- as.character(d$category)
    d$value <- suppressWarnings(as.numeric(d$value))
    d$short_label <- if ("short_label" %in% names(d)) as.character(d$short_label) else d$category
    d$color_key <- if ("color_key" %in% names(d)) as.character(d$color_key) else NA_character_
    d <- d[order(-d$value, d$category), , drop = FALSE]
    total <- sum(d$value, na.rm = TRUE)
    d$share <- if (is.finite(total) && total > 0) d$value / total else rep(1 / nrow(d), nrow(d))
    palette_map <- if (all(is.na(d$color_key) | trimws(d$color_key) == "")) {
      seq_len(nrow(d))
    } else {
      as.integer(as.factor(ifelse(is.na(d$color_key) | trimws(d$color_key) == "", d$category, d$color_key)))
    }
    d$palette_ix <- ((palette_map - 1L) %% 6L) + 1L
    list(
      type = "categorical_tree",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      data = d
    )
  }

  env$normalize_demo_ordinal_bar <- function(panel) {
    env$validate_demo_ordinal_bar(panel$data)
    d <- panel$data
    d$label <- as.character(d$label)
    d$pct <- suppressWarnings(as.numeric(d$pct))
    d$seg_ix <- ((seq_len(nrow(d)) - 1L) %% 5L) + 1L
    callout_index <- suppressWarnings(as.integer(panel$callout_index))
    if (length(callout_index) != 1L || !is.finite(callout_index) || callout_index < 1L || callout_index > nrow(d)) {
      callout_index <- which.min(d$pct)[[1]]
    }
    callout_value <- panel$callout_value
    if (is.null(callout_value) || !nzchar(trimws(as.character(callout_value)))) {
      callout_value <- paste0(round(d$pct[[callout_index]]), "%")
    }
    list(
      type = "ordinal_bar",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      callout_index = callout_index,
      callout_value = as.character(callout_value),
      data = d
    )
  }

  env$normalize_demographics_panel <- function(panel) {
    if (!is.list(panel) || is.null(panel$type) || !nzchar(as.character(panel$type))) {
      stop("Each demographics slot must be built with demo_* constructors.", call. = FALSE)
    }
    panel$type <- as.character(panel$type[[1]])
    switch(
      panel$type,
      bipolar_split = env$normalize_demo_bipolar_split(panel),
      categorical_bar = env$normalize_demo_categorical_bar(panel),
      categorical_tree = env$normalize_demo_categorical_tree(panel),
      ordinal_bar = env$normalize_demo_ordinal_bar(panel),
      stop(sprintf("Unsupported demographics panel type: %s", panel$type), call. = FALSE)
    )
  }

  env$demographics_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "demographics")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-demographics-root {{
  --demo-page-bg: {c$demo_page_bg};
  --demo-grid-gap: {c$demo_grid_gap};
  --demo-panel-bg: {c$demo_panel_bg};
  --demo-panel-border: {c$demo_panel_border};
  --demo-panel-shadow: {c$demo_panel_shadow};
  --demo-title: {c$demo_title};
  --demo-subtitle: {c$demo_subtitle};
  --demo-panel-title: {c$demo_panel_title};
  --demo-panel-subtitle: {c$demo_panel_subtitle};
  --demo-empty-text: {c$demo_empty_text};
  --demo-bi-axis-mid: {c$demo_bi_axis_mid};
  --demo-bi-axis-label: {c$demo_bi_axis_label};
  --demo-bi-side-label-left: {c$demo_bi_side_label_left};
  --demo-bi-side-label-right: {c$demo_bi_side_label_right};
  --demo-bi-side-count: {c$demo_bi_side_count};
  --demo-bi-bar-left-bg: {c$demo_bi_bar_left_bg};
  --demo-bi-bar-right-bg: {c$demo_bi_bar_right_bg};
  --demo-bi-bar-text: {c$demo_bi_bar_text};
  --demo-cb-label: {c$demo_cb_label};
  --demo-cb-track-bg: {c$demo_cb_track_bg};
  --demo-cb-value-text: {c$demo_cb_value_text};
  --demo-cb-bar-1: {c$demo_cb_bar_1};
  --demo-cb-bar-2: {c$demo_cb_bar_2};
  --demo-cb-bar-3: {c$demo_cb_bar_3};
  --demo-cb-bar-4: {c$demo_cb_bar_4};
  --demo-cb-bar-5: {c$demo_cb_bar_5};
  --demo-cb-bar-6: {c$demo_cb_bar_6};
  --demo-ct-text-on: {c$demo_ct_text_on};
  --demo-ct-border: {c$demo_ct_border};
  --demo-ct-tile-1: {c$demo_ct_tile_1};
  --demo-ct-tile-2: {c$demo_ct_tile_2};
  --demo-ct-tile-3: {c$demo_ct_tile_3};
  --demo-ct-tile-4: {c$demo_ct_tile_4};
  --demo-ct-tile-5: {c$demo_ct_tile_5};
  --demo-ct-tile-6: {c$demo_ct_tile_6};
  --demo-ob-axis-label: {c$demo_ob_axis_label};
  --demo-ob-callout-bg: {c$demo_ob_callout_bg};
  --demo-ob-callout-text: {c$demo_ob_callout_text};
  --demo-ob-callout-line: {c$demo_ob_callout_line};
  --demo-ob-seg-1: {c$demo_ob_seg_1};
  --demo-ob-seg-2: {c$demo_ob_seg_2};
  --demo-ob-seg-3: {c$demo_ob_seg_3};
  --demo-ob-seg-4: {c$demo_ob_seg_4};
  --demo-ob-seg-5: {c$demo_ob_seg_5};
  --demo-ob-seg-text-dark: {c$demo_ob_seg_text_dark};
  --demo-ob-seg-text-light: {c$demo_ob_seg_text_light};
  width: 1280px;
  min-height: 760px;
  background: var(--demo-page-bg);
  padding: 26px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .demo-header {{ margin-bottom: 14px; }}
{scope} .demo-title {{ margin: 0; font-size: 28px; font-weight: 900; color: var(--demo-title); letter-spacing: -0.4px; }}
{scope} .demo-subtitle {{ margin: 6px 0 0; font-size: 13px; color: var(--demo-subtitle); }}
{scope} .demo-grid {{ display: grid; grid-template-columns: 1fr 1fr; grid-template-rows: 1fr 1fr; gap: 2px; background: var(--demo-grid-gap); border-radius: 12px; padding: 2px; }}
{scope} .demo-panel {{ background: var(--demo-panel-bg); border: 1px solid var(--demo-panel-border); border-radius: 12px; box-shadow: 0 4px 16px var(--demo-panel-shadow); padding: 18px; min-height: 310px; }}
{scope} .demo-panel-head {{ margin-bottom: 14px; }}
{scope} .demo-panel-title {{ margin: 0; font-size: 15px; text-transform: uppercase; letter-spacing: .5px; font-weight: 800; color: var(--demo-panel-title); }}
{scope} .demo-panel-subtitle {{ margin: 4px 0 0; font-size: 12px; color: var(--demo-panel-subtitle); }}
{scope} .demo-empty {{ display: flex; height: 100%; align-items: center; justify-content: center; border: 1px dashed var(--demo-panel-border); border-radius: 8px; color: var(--demo-empty-text); font-weight: 700; }}
{scope} .demo-bi-area {{ display: grid; grid-template-columns: 92px 1fr 92px; gap: 10px; align-items: center; margin-top: 10px; }}
{scope} .demo-bi-side {{ display: flex; flex-direction: column; }}
{scope} .demo-bi-side.left {{ text-align: right; align-items: flex-end; }}
{scope} .demo-bi-side.right {{ text-align: left; align-items: flex-start; }}
{scope} .demo-bi-lbl {{ font-size: 12px; font-weight: 800; text-transform: uppercase; letter-spacing: .4px; }}
{scope} .demo-bi-lbl.left {{ color: var(--demo-bi-side-label-left); }}
{scope} .demo-bi-lbl.right {{ color: var(--demo-bi-side-label-right); }}
{scope} .demo-bi-cnt {{ margin-top: 3px; color: var(--demo-bi-side-count); font-size: 11px; font-weight: 600; }}
{scope} .demo-bi-axis {{ height: 54px; position: relative; }}
{scope} .demo-bi-mid {{ position: absolute; left: 50%; top: -4px; bottom: -4px; width: 2px; transform: translateX(-50%); background: var(--demo-bi-axis-mid); }}
{scope} .demo-bi-mid-lbl {{ position: absolute; left: 50%; top: -20px; transform: translateX(-50%); font-size: 10px; color: var(--demo-bi-axis-label); font-weight: 800; text-transform: uppercase; }}
{scope} .demo-bi-left {{ position: absolute; right: 50%; top: 8px; height: 38px; border-radius: 8px 0 0 8px; background: var(--demo-bi-bar-left-bg); display: flex; align-items: center; justify-content: flex-end; padding-right: 12px; }}
{scope} .demo-bi-right {{ position: absolute; left: 50%; top: 8px; height: 38px; border-radius: 0 8px 8px 0; background: var(--demo-bi-bar-right-bg); display: flex; align-items: center; justify-content: flex-start; padding-left: 12px; }}
{scope} .demo-bi-val {{ color: var(--demo-bi-bar-text); font-size: 14px; font-weight: 800; }}
{scope} .demo-cb-row {{ display: grid; grid-template-columns: 124px 1fr; gap: 10px; align-items: center; margin-bottom: 10px; }}
{scope} .demo-cb-label {{ text-align: right; padding-right: 6px; color: var(--demo-cb-label); font-size: 12px; font-weight: 700; }}
{scope} .demo-cb-track {{ height: 28px; border-radius: 4px; background: var(--demo-cb-track-bg); overflow: hidden; }}
{scope} .demo-cb-fill {{ height: 100%; display: flex; align-items: center; justify-content: flex-end; padding-right: 10px; color: var(--demo-cb-value-text); font-size: 12px; font-weight: 800; }}
{scope} .demo-cb-fill.p1 {{ background: var(--demo-cb-bar-1); }}
{scope} .demo-cb-fill.p2 {{ background: var(--demo-cb-bar-2); }}
{scope} .demo-cb-fill.p3 {{ background: var(--demo-cb-bar-3); }}
{scope} .demo-cb-fill.p4 {{ background: var(--demo-cb-bar-4); }}
{scope} .demo-cb-fill.p5 {{ background: var(--demo-cb-bar-5); }}
{scope} .demo-cb-fill.p6 {{ background: var(--demo-cb-bar-6); }}
{scope} .demo-ct-wrap {{ display: flex; gap: 4px; height: 220px; }}
{scope} .demo-ct-col {{ display: flex; flex-direction: column; gap: 4px; }}
{scope} .demo-ct-box {{ border-radius: 4px; border: 1px solid var(--demo-ct-border); color: var(--demo-ct-text-on); padding: 10px; display: flex; flex-direction: column; justify-content: flex-end; min-height: 32px; }}
{scope} .demo-ct-num {{ font-size: 18px; font-weight: 900; line-height: 1; }}
{scope} .demo-ct-lbl {{ margin-top: 4px; font-size: 10px; text-transform: uppercase; font-weight: 700; line-height: 1.1; }}
{scope} .demo-ct-box.p1 {{ background: var(--demo-ct-tile-1); }}
{scope} .demo-ct-box.p2 {{ background: var(--demo-ct-tile-2); }}
{scope} .demo-ct-box.p3 {{ background: var(--demo-ct-tile-3); }}
{scope} .demo-ct-box.p4 {{ background: var(--demo-ct-tile-4); }}
{scope} .demo-ct-box.p5 {{ background: var(--demo-ct-tile-5); }}
{scope} .demo-ct-box.p6 {{ background: var(--demo-ct-tile-6); }}
{scope} .demo-ob-area {{ position: relative; margin-top: 12px; }}
{scope} .demo-ob-callout {{ position: absolute; top: -34px; transform: translateX(-50%); display: flex; flex-direction: column; align-items: center; }}
{scope} .demo-ob-callout-val {{ background: var(--demo-ob-callout-bg); border: 1px solid var(--demo-panel-border); border-radius: 4px; color: var(--demo-ob-callout-text); font-weight: 800; font-size: 11px; padding: 2px 6px; }}
{scope} .demo-ob-callout-line {{ width: 2px; height: 12px; background: var(--demo-ob-callout-line); }}
{scope} .demo-ob-bar {{ display: flex; height: 32px; gap: 4px; }}
{scope} .demo-ob-seg {{ height: 100%; border-radius: 4px; display: flex; align-items: center; justify-content: center; font-size: 11px; font-weight: 800; }}
{scope} .demo-ob-seg.s1 {{ background: var(--demo-ob-seg-1); color: var(--demo-ob-seg-text-dark); }}
{scope} .demo-ob-seg.s2 {{ background: var(--demo-ob-seg-2); color: var(--demo-ob-seg-text-dark); }}
{scope} .demo-ob-seg.s3 {{ background: var(--demo-ob-seg-3); color: var(--demo-ob-seg-text-light); }}
{scope} .demo-ob-seg.s4 {{ background: var(--demo-ob-seg-4); color: var(--demo-ob-seg-text-light); }}
{scope} .demo-ob-seg.s5 {{ background: var(--demo-ob-seg-5); color: var(--demo-ob-seg-text-light); }}
{scope} .demo-ob-axis {{ display: flex; gap: 4px; margin-top: 8px; }}
{scope} .demo-ob-axis-lbl {{ font-size: 10px; color: var(--demo-ob-axis-label); font-weight: 700; text-align: center; line-height: 1.1; }}
"
    )
  }

  env$render_demographics_page <- function(
    tl = NULL,
    tr = NULL,
    bl = NULL,
    br = NULL,
    id = "ohep-demographics-page",
    title = "Demographics",
    subtitle = "",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "demographics"
    )

    normalize_slot <- function(panel) {
      if (is.null(panel)) return(NULL)
      env$normalize_demographics_panel(panel)
    }
    slots <- list(
      tl = normalize_slot(tl),
      tr = normalize_slot(tr),
      bl = normalize_slot(bl),
      br = normalize_slot(br)
    )

    panel_html <- function(panel) {
      if (is.null(panel)) {
        return("<div class=\"demo-panel\"><div class=\"demo-empty\">No panel</div></div>")
      }
      head_html <- glue::glue(
        "<div class=\"demo-panel-head\">
          <h3 class=\"demo-panel-title\">{env$escape_text(panel$title)}</h3>
          {if (nzchar(trimws(panel$subtitle))) glue::glue('<p class=\"demo-panel-subtitle\">{env$escape_text(panel$subtitle)}</p>') else ''}
        </div>"
      )
      body_html <- switch(
        panel$type,
        bipolar_split = {
          d <- panel$data
          glue::glue(
            "<div class=\"demo-bi-area\">
              <div class=\"demo-bi-side left\">
                <span class=\"demo-bi-lbl left\">{env$escape_text(d$label[[1]])}</span>
                <span class=\"demo-bi-cnt\">{sprintf('%.0f', d$count[[1]])}</span>
              </div>
              <div class=\"demo-bi-axis\">
                <div class=\"demo-bi-mid\"><span class=\"demo-bi-mid-lbl\">Center</span></div>
                <div class=\"demo-bi-left\" style=\"width:{sprintf('%.1f', d$pct[[1]] / 2)}%;\"><span class=\"demo-bi-val\">{sprintf('%.0f%%', d$pct[[1]])}</span></div>
                <div class=\"demo-bi-right\" style=\"width:{sprintf('%.1f', d$pct[[2]] / 2)}%;\"><span class=\"demo-bi-val\">{sprintf('%.0f%%', d$pct[[2]])}</span></div>
              </div>
              <div class=\"demo-bi-side right\">
                <span class=\"demo-bi-lbl right\">{env$escape_text(d$label[[2]])}</span>
                <span class=\"demo-bi-cnt\">{sprintf('%.0f', d$count[[2]])}</span>
              </div>
            </div>"
          )
        },
        categorical_bar = {
          d <- panel$data
          rows <- vapply(seq_len(nrow(d)), function(i) {
            glue::glue(
              "<div class=\"demo-cb-row\">
                <div class=\"demo-cb-label\">{env$escape_text(d$category[[i]])}</div>
                <div class=\"demo-cb-track\"><div class=\"demo-cb-fill p{d$palette_ix[[i]]}\" style=\"width:{sprintf('%.1f', d$width_pct[[i]])}%;\">{sprintf('%.0f', d$value[[i]])}</div></div>
              </div>"
            )
          }, character(1))
          glue::glue_collapse(rows, sep = "")
        },
        categorical_tree = {
          d <- panel$data
          n <- nrow(d)
          if (n == 1L) {
            cols <- list(d[1, , drop = FALSE])
          } else if (n == 2L) {
            cols <- list(d[1, , drop = FALSE], d[2, , drop = FALSE])
          } else {
            col1 <- d[1, , drop = FALSE]
            rest <- d[-1, , drop = FALSE]
            a <- cumsum(rest$share)
            split_ix <- which(a >= max(a) / 2)[1]
            if (!is.finite(split_ix) || split_ix < 1) split_ix <- ceiling(nrow(rest) / 2)
            col2 <- rest[seq_len(split_ix), , drop = FALSE]
            col3 <- if (split_ix < nrow(rest)) rest[(split_ix + 1):nrow(rest), , drop = FALSE] else rest[0, , drop = FALSE]
            if (nrow(col3) > 0) {
              col3 <- col3[nrow(col3):1, , drop = FALSE]
            }
            cols <- list(col1, col2, col3)
          }
          col_weights <- vapply(cols, function(df) sum(df$share, na.rm = TRUE), numeric(1))
          col_weights <- if (sum(col_weights) > 0) (col_weights / sum(col_weights)) * 100 else rep(100 / length(cols), length(cols))
          cols_html <- vapply(seq_along(cols), function(ci) {
            col <- cols[[ci]]
            hweights <- if (sum(col$share) > 0) (col$share / sum(col$share)) * 100 else rep(100 / nrow(col), nrow(col))
            boxes <- vapply(seq_len(nrow(col)), function(i) {
              glue::glue(
                "<div class=\"demo-ct-box p{col$palette_ix[[i]]}\" style=\"height:{sprintf('%.1f', hweights[[i]])}%;\">
                  <span class=\"demo-ct-num\">{sprintf('%.0f', col$value[[i]])}</span>
                  <span class=\"demo-ct-lbl\">{env$escape_text(col$short_label[[i]])}</span>
                </div>"
              )
            }, character(1))
            glue::glue("<div class=\"demo-ct-col\" style=\"width:{sprintf('%.1f', col_weights[[ci]])}%;\">{glue::glue_collapse(boxes, sep='')}</div>")
          }, character(1))
          glue::glue("<div class=\"demo-ct-wrap\">{glue::glue_collapse(cols_html, sep='')}</div>")
        },
        ordinal_bar = {
          d <- panel$data
          callout_pos <- sum(d$pct[seq_len(max(1L, panel$callout_index - 1L))], na.rm = TRUE)
          callout_pos <- callout_pos + (d$pct[[panel$callout_index]] / 2)
          segs <- vapply(seq_len(nrow(d)), function(i) {
            txt <- if (d$pct[[i]] >= 11) sprintf("%.0f%%", d$pct[[i]]) else ""
            glue::glue("<div class=\"demo-ob-seg s{d$seg_ix[[i]]}\" style=\"width:{sprintf('%.1f', d$pct[[i]])}%;\">{txt}</div>")
          }, character(1))
          axis <- vapply(seq_len(nrow(d)), function(i) {
            glue::glue("<div class=\"demo-ob-axis-lbl\" style=\"width:{sprintf('%.1f', d$pct[[i]])}%;\">{env$escape_text(d$label[[i]])}</div>")
          }, character(1))
          glue::glue(
            "<div class=\"demo-ob-area\">
              <div class=\"demo-ob-callout\" style=\"left:{sprintf('%.1f', callout_pos)}%;\">
                <div class=\"demo-ob-callout-val\">{env$escape_text(panel$callout_value)}</div>
                <div class=\"demo-ob-callout-line\"></div>
              </div>
              <div class=\"demo-ob-bar\">{glue::glue_collapse(segs, sep='')}</div>
              <div class=\"demo-ob-axis\">{glue::glue_collapse(axis, sep='')}</div>
            </div>"
          )
        },
        "<div class=\"demo-empty\">Unsupported panel</div>"
      )
      glue::glue("<div class=\"demo-panel\">{head_html}{body_html}</div>")
    }

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-demographics-root\">
        <div class=\"demo-header\">
          <h1 class=\"demo-title\">{env$escape_text(title)}</h1>
          {if (nzchar(trimws(subtitle))) glue::glue('<p class=\"demo-subtitle\">{env$escape_text(subtitle)}</p>') else ''}
        </div>
        <div class=\"demo-grid\">
          {panel_html(slots$tl)}
          {panel_html(slots$tr)}
          {panel_html(slots$bl)}
          {panel_html(slots$br)}
        </div>
      </div>"
    )

    css <- env$demographics_page_css(id = id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_decision_matrix_data <- function(points) {
    if (!is.data.frame(points) || nrow(points) < 1L) {
      stop("`points` must be a non-empty data frame.", call. = FALSE)
    }
    required <- c("fundamental", "score", "impact")
    missing <- setdiff(required, names(points))
    if (length(missing) > 0L) {
      stop(sprintf("`points` is missing required columns: %s.", paste(missing, collapse = ", ")), call. = FALSE)
    }
    score <- suppressWarnings(as.numeric(points$score))
    impact <- suppressWarnings(as.numeric(points$impact))
    if (anyNA(score) || anyNA(impact)) {
      stop("`score` and `impact` must be numeric and non-missing.", call. = FALSE)
    }
    points
  }

  env$normalize_decision_matrix_points <- function(points) {
    points <- env$validate_decision_matrix_data(points)
    out <- points
    out$fundamental <- as.character(out$fundamental)
    out$label <- if ("label" %in% names(out)) as.character(out$label) else out$fundamental
    out$score <- suppressWarnings(as.numeric(out$score))
    out$impact <- suppressWarnings(as.numeric(out$impact))

    x_split <- suppressWarnings(as.numeric(attr(points, "x_split_value")))
    y_split <- suppressWarnings(as.numeric(attr(points, "y_split_value")))
    if (length(x_split) != 1L || !is.finite(x_split)) x_split <- 0
    if (length(y_split) != 1L || !is.finite(y_split)) y_split <- stats::median(out$impact, na.rm = TRUE)

    x_scale_mode <- as.character(if (is.null(attr(points, "x_scale_mode"))) "symmetric" else attr(points, "x_scale_mode"))
    y_scale_mode <- as.character(if (is.null(attr(points, "y_scale_mode"))) "range" else attr(points, "y_scale_mode"))

    score_rng <- range(out$score, na.rm = TRUE, finite = TRUE)
    impact_rng <- range(out$impact, na.rm = TRUE, finite = TRUE)
    x_pad <- 8
    y_pad <- 12
    inner_x <- 100 - (2 * x_pad)
    inner_y <- 100 - (2 * y_pad)

    if (identical(x_scale_mode, "symmetric")) {
      max_abs_score <- suppressWarnings(max(abs(out$score - x_split), na.rm = TRUE))
      if (!is.finite(max_abs_score) || max_abs_score == 0) {
        out$x_pct <- rep(50, nrow(out))
      } else {
        out$x_pct <- 50 + ((out$score - x_split) / max_abs_score) * (inner_x / 2)
      }
    } else {
      if (!all(is.finite(score_rng)) || diff(score_rng) == 0) {
        out$x_pct <- rep(50, nrow(out))
      } else {
        out$x_pct <- x_pad + inner_x * (out$score - score_rng[[1]]) / (score_rng[[2]] - score_rng[[1]])
      }
    }

    if (identical(y_scale_mode, "symmetric")) {
      max_abs_impact <- suppressWarnings(max(abs(out$impact - y_split), na.rm = TRUE))
      if (!is.finite(max_abs_impact) || max_abs_impact == 0) {
        out$y_pct <- rep(50, nrow(out))
      } else {
        out$y_pct <- 50 + ((out$impact - y_split) / max_abs_impact) * (inner_y / 2)
      }
    } else {
      if (!all(is.finite(impact_rng)) || diff(impact_rng) == 0) {
        out$y_pct <- rep(50, nrow(out))
      } else {
        out$y_pct <- y_pad + inner_y * (out$impact - impact_rng[[1]]) / (impact_rng[[2]] - impact_rng[[1]])
      }
    }
    out$x_pct <- pmax(x_pad, pmin(100 - x_pad, out$x_pct))
    out$y_pct <- pmax(y_pad, pmin(100 - y_pad, out$y_pct))

    out$quadrant <- ifelse(
      out$score < x_split & out$impact >= y_split, "critical",
      ifelse(
        out$score >= x_split & out$impact >= y_split, "leverage",
        ifelse(out$score < x_split & out$impact < y_split, "monitor", "maintain")
      )
    )

    if ("priority" %in% names(out)) {
      priority <- tolower(trimws(as.character(out$priority)))
      valid <- priority %in% c("critical", "leverage", "monitor", "maintain")
      out$quadrant[valid] <- priority[valid]
    }

    out$dot_class <- ifelse(
      out$quadrant == "critical", "dot-critical",
      ifelse(
        out$quadrant == "leverage", "dot-leverage",
        ifelse(out$quadrant == "maintain", "dot-maintain", "dot-monitor")
      )
    )

    out
  }

  env$decision_matrix_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "decision_matrix")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-dm-root {{
  --matrix-shell-bg: {c$matrix_shell_bg};
  --matrix-slide-bg: {c$matrix_slide_bg};
  --matrix-header-divider: {c$matrix_header_divider};
  --matrix-theme-kicker: {c$matrix_theme_kicker};
  --matrix-title: {c$matrix_title};
  --matrix-subtitle: {c$matrix_subtitle};
  --matrix-axis-line: {c$matrix_axis_line};
  --matrix-axis-text: {c$matrix_axis_text};
  --matrix-axis-main: {c$matrix_axis_main};
  --matrix-plot-bg: {c$matrix_plot_bg};
  --matrix-quadrant-tl-bg: {c$matrix_quadrant_tl_bg};
  --matrix-quadrant-tl-text: {c$matrix_quadrant_tl_text};
  --matrix-quadrant-tr-bg: {c$matrix_quadrant_tr_bg};
  --matrix-quadrant-tr-text: {c$matrix_quadrant_tr_text};
  --matrix-quadrant-bl-bg: {c$matrix_quadrant_bl_bg};
  --matrix-quadrant-bl-text: {c$matrix_quadrant_bl_text};
  --matrix-quadrant-br-bg: {c$matrix_quadrant_br_bg};
  --matrix-quadrant-br-text: {c$matrix_quadrant_br_text};
  --matrix-quadrant-divider: {c$matrix_quadrant_divider};
  --matrix-dot-critical: {c$matrix_dot_critical};
  --matrix-dot-leverage: {c$matrix_dot_leverage};
  --matrix-dot-monitor: {c$matrix_dot_monitor};
  --matrix-dot-maintain: {c$matrix_dot_maintain};
  --matrix-point-border: {c$matrix_point_border};
  --matrix-point-shadow: {c$matrix_point_shadow};
  --matrix-label-bg: {c$matrix_label_bg};
  --matrix-label-text: {c$matrix_label_text};
  --matrix-label-border: {c$matrix_label_border};
  --matrix-label-shadow: {c$matrix_label_shadow};
  --matrix-brand-text: {c$matrix_brand_text};
  --matrix-brand-dot: {c$matrix_brand_dot};
  width: 1280px;
  height: 720px;
  background: var(--matrix-slide-bg);
  padding: 36px 48px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  flex-direction: column;
  overflow: hidden;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .header-section {{ display: flex; flex-direction: column; border-bottom: 2px solid var(--matrix-header-divider); padding-bottom: 12px; margin-bottom: 20px; flex-shrink: 0; }}
{scope} .theme-kicker {{ font-size: 14px; font-weight: 800; color: var(--matrix-theme-kicker); text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 4px; }}
{scope} .title {{ font-size: 32px; font-weight: 900; color: var(--matrix-title); margin: 0 0 6px 0; letter-spacing: -0.5px; }}
{scope} .subtitle {{ font-size: 15px; color: var(--matrix-subtitle); font-weight: 500; margin: 0; }}
{scope} .chart-wrapper {{ display: flex; position: relative; flex: 1; min-height: 0; margin-top: 10px; }}
{scope} .y-axis {{ width: 60px; display: flex; flex-direction: column; justify-content: space-between; align-items: center; padding-bottom: 20px; flex-shrink: 0; }}
{scope} .y-axis-text {{ font-size: 14px; font-weight: 800; color: var(--matrix-axis-text); letter-spacing: 0.5px; }}
{scope} .y-axis-main {{ transform: rotate(-90deg); white-space: nowrap; font-size: 16px; font-weight: 900; letter-spacing: 1.5px; color: var(--matrix-axis-main); }}
{scope} .matrix-container {{ position: relative; flex-grow: 1; border-left: 3px solid var(--matrix-axis-line); border-bottom: 3px solid var(--matrix-axis-line); display: flex; flex-wrap: wrap; background: var(--matrix-plot-bg); border-radius: 0 8px 0 0; overflow: hidden; }}
{scope} .quadrant {{ width: 50%; height: 50%; box-sizing: border-box; padding: 24px; font-size: 16px; font-weight: 900; letter-spacing: 1px; position: relative; }}
{scope} .q-tl {{ background-color: var(--matrix-quadrant-tl-bg); color: var(--matrix-quadrant-tl-text); border-right: 2px dashed var(--matrix-quadrant-divider); border-bottom: 2px dashed var(--matrix-quadrant-divider); }}
{scope} .q-tr {{ background-color: var(--matrix-quadrant-tr-bg); color: var(--matrix-quadrant-tr-text); border-bottom: 2px dashed var(--matrix-quadrant-divider); text-align: right; }}
{scope} .q-bl {{ background-color: var(--matrix-quadrant-bl-bg); color: var(--matrix-quadrant-bl-text); border-right: 2px dashed var(--matrix-quadrant-divider); display: flex; align-items: flex-end; }}
{scope} .q-br {{ background-color: var(--matrix-quadrant-br-bg); color: var(--matrix-quadrant-br-text); display: flex; align-items: flex-end; justify-content: flex-end; }}
{scope} .points-layer {{ position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; }}
{scope} .point {{ position: absolute; transform: translate(-50%, -50%); pointer-events: auto; transition: transform 0.2s ease, z-index 0s; }}
{scope} .point:hover {{ transform: translate(-50%, -50%) scale(1.08); z-index: 20; }}
{scope} .dot {{ width: 22px; height: 22px; border-radius: 50%; border: 3px solid var(--matrix-point-border); box-shadow: 0 4px 8px var(--matrix-point-shadow); }}
{scope} .dot-critical {{ background: var(--matrix-dot-critical); }}
{scope} .dot-leverage {{ background: var(--matrix-dot-leverage); }}
{scope} .dot-monitor {{ background: var(--matrix-dot-monitor); }}
{scope} .dot-maintain {{ background: var(--matrix-dot-maintain); }}
{scope} .label {{ position: absolute; top: 50%; background: var(--matrix-label-bg); padding: 6px 12px; border-radius: 6px; font-size: 13px; font-weight: 800; color: var(--matrix-label-text); box-shadow: 0 4px 10px var(--matrix-label-shadow); border: 1px solid var(--matrix-label-border); white-space: nowrap; line-height: 1; transform: translateY(-50%); }}
{scope} .label-right {{ left: 28px; }}
{scope} .label-left {{ right: 28px; }}
{scope} .x-axis {{ display: flex; justify-content: space-between; align-items: center; padding-left: 60px; margin-top: 20px; flex-shrink: 0; }}
{scope} .x-axis-text {{ font-size: 14px; font-weight: 800; color: var(--matrix-axis-text); letter-spacing: 0.5px; }}
{scope} .x-axis-main {{ font-size: 16px; font-weight: 900; letter-spacing: 1.5px; color: var(--matrix-axis-main); }}
"
    )
  }

  env$render_decision_matrix_page <- function(
    points,
    id = "ohep-decision-matrix",
    theme_kicker = "Decision Matrix",
    title = "Health Driver Action Matrix",
    subtitle = "",
    y_axis_main = "IMPACT ON OUTCOMES",
    x_axis_main = "PERFORMANCE VS. BENCHMARK",
    y_axis_low = "LOW",
    y_axis_high = "HIGH",
    x_axis_low = "LAGGING",
    x_axis_high = "LEADING",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "decision_matrix"
    )
    pts <- env$normalize_decision_matrix_points(points)

    points_html <- vapply(seq_len(nrow(pts)), function(i) {
      label_txt <- as.character(pts$label[[i]])
      chars <- nchar(label_txt)
      est_label_w <- pmax(10, pmin(34, 3 + 0.72 * chars))
      side <- if ((pts$x_pct[[i]] + est_label_w) > 98) "left" else "right"
      if ((pts$x_pct[[i]] - est_label_w) < 2) side <- "right"
      glue::glue(
        "<div class=\"point\" style=\"left: {sprintf('%.1f', pts$x_pct[[i]])}%; top: {sprintf('%.1f', 100 - pts$y_pct[[i]])}%;\">
          <div class=\"dot {pts$dot_class[[i]]}\"></div>
          <div class=\"label label-{side}\">{env$escape_text(label_txt)}</div>
        </div>"
      )
    }, character(1))

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-dm-root\">
        <div class=\"header-section\">
          <div class=\"theme-kicker\">{env$escape_text(theme_kicker)}</div>
          <h1 class=\"title\">{env$escape_text(title)}</h1>
          {if (nzchar(subtitle)) glue::glue('<p class=\"subtitle\">{env$escape_text(subtitle)}</p>') else ''}
        </div>
        <div class=\"chart-wrapper\">
          <div class=\"y-axis\">
            <div class=\"y-axis-text\">{env$escape_text(y_axis_high)}</div>
            <div class=\"y-axis-main\">{env$escape_text(y_axis_main)}</div>
            <div class=\"y-axis-text\">{env$escape_text(y_axis_low)}</div>
          </div>
          <div class=\"matrix-container\">
            <div class=\"quadrant q-tl\">CRITICAL FOCUS</div>
            <div class=\"quadrant q-tr\">LEVERAGE</div>
            <div class=\"quadrant q-bl\">MONITOR</div>
            <div class=\"quadrant q-br\">MAINTAIN</div>
            <div class=\"points-layer\">
              {glue::glue_collapse(points_html, sep = '')}
            </div>
          </div>
        </div>
        <div class=\"x-axis\">
          <div class=\"x-axis-text\">{env$escape_text(x_axis_low)}</div>
          <div class=\"x-axis-main\">{env$escape_text(x_axis_main)}</div>
          <div class=\"x-axis-text\">{env$escape_text(x_axis_high)}</div>
        </div>
      </div>"
    )

    css <- env$decision_matrix_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$render_fundamental_page <- function(
    dashboard_data,
    id = "ohep-custom-dashboard",
    color_overrides = NULL,
    benchmark_company = TRUE,
    benchmark_index = TRUE,
    benchmark_prior = TRUE,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "fundamental"
    )
    dashboard_data <- env$validate_fundamental_page_data(dashboard_data)
    fundamental <- dashboard_data$fundamental
    outcomes <- dashboard_data$outcomes
    items <- dashboard_data$items
    active_benchmarks <- env$get_active_benchmarks(
      items = items,
      fundamental_row = fundamental,
      benchmark_company = benchmark_company,
      benchmark_index = benchmark_index,
      benchmark_prior = benchmark_prior
    )

    hero <- env$build_hero_card(fundamental)
    outcomes_table <- env$build_outcomes_table(outcomes)
    left_col <- env$build_item_column(items, "left", active_benchmarks = active_benchmarks)
    right_col <- env$build_item_column(items, "right", active_benchmarks = active_benchmarks)

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-root\">
        <div class=\"top-row\">
          {hero}
          {outcomes_table}
        </div>
        <div class=\"bottom-row\">
          <div class=\"card item-card\">
            <div class=\"item-title-row\">
              <h2 class=\"card-title\">Item-Level Breakdown</h2>
              <div class=\"legend-group\">
                <div class=\"leg-item\"><div class=\"leg-dot bg-disagree\"></div> Disagree</div>
                <div class=\"leg-item\"><div class=\"leg-dot bg-neutral\"></div> Neutral</div>
                <div class=\"leg-item\"><div class=\"leg-dot bg-agree\"></div> Agree</div>
              </div>
            </div>
            <div class=\"item-grid-2col\">
              {left_col}
              {right_col}
            </div>
          </div>
        </div>
      </div>"
    )

    css <- env$fundamental_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$fundamental_page <- function(
    fundamental,
    index_data,
    user_data,
    id = "ohep-custom-dashboard",
    color_overrides = NULL,
    benchmark_company = TRUE,
    benchmark_index = TRUE,
    benchmark_prior = TRUE,
    ...
  ) {
    dashboard_data <- env$build_dashboard_data_from_inputs(
      fundamental = fundamental,
      index_data = index_data,
      user_data = user_data
    )
    env$render_fundamental_page(
      dashboard_data = dashboard_data,
      id = id,
      color_overrides = color_overrides,
      benchmark_company = benchmark_company,
      benchmark_index = benchmark_index,
      benchmark_prior = benchmark_prior,
      ...
    )
  }

  env
}
outcome_page <-
function(...) {
  stop("`outcome_page()` is not implemented yet.", call. = FALSE)
}
prep_ohep_snapshot <-
function(
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
render_fundamental_page <-
function(
  dashboard_data,
  id = "ohep-custom-dashboard",
  color_overrides = NULL,
  benchmark_company = TRUE,
  benchmark_index = TRUE,
  benchmark_prior = TRUE,
  ...
) {
  runtime <- ohepRDisplayr()
  runtime$render_fundamental_page(
    dashboard_data = dashboard_data,
    id = id,
    color_overrides = color_overrides,
    benchmark_company = benchmark_company,
    benchmark_index = benchmark_index,
    benchmark_prior = benchmark_prior,
    ...
  )
}
validate_displayr_schema <-
function(payload) {
  if (!is.list(payload)) {
    stop("`payload` must be a named list.", call. = FALSE)
  }

  required_top <- c(
    "payload_meta",
    "raw_user_data",
    "marts",
    "index_data",
    "predictive_data",
    "colors_table"
  )
  miss_top <- setdiff(required_top, names(payload))
  if (length(miss_top) > 0) {
    stop(
      sprintf(
        "`payload` is missing required top-level fields: %s.",
        paste(miss_top, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.data.frame(payload$payload_meta)) {
    stop("`payload$payload_meta` must be a data frame.", call. = FALSE)
  }
  req_meta <- c("snapshot_id", "generated_at_utc")
  miss_meta <- setdiff(req_meta, names(payload$payload_meta))
  if (length(miss_meta) > 0) {
    stop(
      sprintf("`payload$payload_meta` missing columns: %s.", paste(miss_meta, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.data.frame(payload$raw_user_data)) {
    stop("`payload$raw_user_data` must be a data frame.", call. = FALSE)
  }

  if (!is.list(payload$marts)) {
    stop("`payload$marts` must be a list from `prep_ohep_snapshot()`.", call. = FALSE)
  }
  if (!is.list(payload$index_data)) {
    stop("`payload$index_data` must be a list.", call. = FALSE)
  }
  if (!is.data.frame(payload$index_data$item_data)) {
    stop("`payload$index_data$item_data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(payload$predictive_data) && !is.data.frame(payload$predictive_data)) {
    stop("`payload$predictive_data` must be NULL or a data frame.", call. = FALSE)
  }
  if (!is.data.frame(payload$colors_table)) {
    stop("`payload$colors_table` must be a data frame.", call. = FALSE)
  }
  if (!("variable" %in% names(payload$colors_table))) {
    stop("`payload$colors_table` must include a `variable` column.", call. = FALSE)
  }

  invisible(TRUE)
}
validate_fundamental_page_data <-
function(dashboard_data) {
  runtime <- ohepRDisplayr()
  runtime$validate_fundamental_page_data(dashboard_data)
}
write_displayr_template_bootstrap <-
function(
  static_bundle,
  out_dir,
  include_rds = TRUE,
  dataset_names = list(
    index_item_data = "index_item_data",
    index_user_data_key = "index_user_data_key",
    predictive_data = "predictive_data",
    colors_table = "colors_table",
    raw_user_data = "raw_user_data"
  )
) {
  if (!is.list(static_bundle) || !is.list(static_bundle$index_data) || !is.data.frame(static_bundle$index_data$item_data)) {
    stop("`static_bundle` must come from `build_displayr_static_bundle()`.", call. = FALSE)
  }
  if (!is.data.frame(static_bundle$colors_table)) {
    stop("`static_bundle$colors_table` must be a data frame.", call. = FALSE)
  }
  if (!is.character(out_dir) || length(out_dir) != 1L || !nzchar(out_dir)) {
    stop("`out_dir` must be a non-empty path string.", call. = FALSE)
  }

  req_names <- c("index_item_data", "index_user_data_key", "predictive_data", "colors_table", "raw_user_data")
  if (!is.list(dataset_names) || length(setdiff(req_names, names(dataset_names))) > 0L) {
    stop("`dataset_names` must include: index_item_data, index_user_data_key, predictive_data, colors_table, raw_user_data.", call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  payload_dir <- file.path(out_dir, "displayr_payload")
  support_dir <- file.path(out_dir, "displayr_support")
  dir.create(payload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(support_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(x, path) utils::write.csv(x, path, row.names = FALSE)

  write_csv(static_bundle$index_data$item_data, file.path(payload_dir, "01_index_item_data.csv"))
  if (!is.null(static_bundle$index_data$user_data_key) && is.data.frame(static_bundle$index_data$user_data_key)) {
    write_csv(static_bundle$index_data$user_data_key, file.path(payload_dir, "02_index_user_data_key.csv"))
  }
  if (!is.null(static_bundle$predictive_data) && is.data.frame(static_bundle$predictive_data)) {
    write_csv(static_bundle$predictive_data, file.path(payload_dir, "03_predictive_data.csv"))
  }
  write_csv(static_bundle$colors_table, file.path(payload_dir, "04_colors_table.csv"))

  mapping_script <- build_displayr_mapping_script(dataset_names = dataset_names)

  # Build a single pasteable script containing all exported ohepR functions
  # followed by the dataset mapping block.
  ns <- asNamespace("ohepR")
  exported <- sort(getNamespaceExports("ohepR"))
  runtime_lines <- character(0)
  tc <- textConnection("runtime_lines", open = "w", local = TRUE)
  dump(
    list = exported,
    file = tc,
    envir = ns
  )
  close(tc)

  single_paste_path <- file.path(payload_dir, "displayr_bootstrap_single_paste.R")
  con <- file(single_paste_path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(
    c(
      "# Single-paste Displayr bootstrap script",
      "# 1) Upload CSVs from this folder.",
      "# 2) Paste this whole script into one Displayr R Output.",
      "# 3) Edit DATASET_MAP only, then run.",
      "",
      "# ohepR runtime function bundle for Displayr",
      ""
    ),
    con = con,
    useBytes = TRUE
  )
  writeLines(runtime_lines, con = con, useBytes = TRUE)
  writeLines(c("", mapping_script), con = con, useBytes = TRUE)
  close(con)
  on.exit(NULL, add = FALSE)

  functions_bundle <- list(
    exported = exported,
    runtime_script_lines = runtime_lines,
    generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
  if (isTRUE(include_rds)) {
    saveRDS(functions_bundle, file.path(support_dir, "template_functions_bundle.rds"))
  }

  github_loader_lines <- c(
    "# Displayr GitHub RDS Bootstrap Script",
    "# PRIMARY FLOW: load ohepR functions + static data from GitHub-hosted RDS files.",
    "# Pin URLs to a commit SHA for reproducibility.",
    "# Edit the CONFIG block and raw_user_data assignment only.",
    "",
    "CONFIG <- list(",
    "  # Example pinned URL:",
    "  # https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_functions_bundle.rds",
    "  functions_rds_url = \"https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_functions_bundle.rds\",",
    "  static_bundle_rds_url = \"https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_static_bundle.rds\"",
    ")",
    "",
    "read_rds_url <- function(u) {",
    "  con <- url(u, open = \"rb\")",
    "  on.exit(close(con), add = TRUE)",
    "  readRDS(con)",
    "}",
    "",
    "fn_bundle <- read_rds_url(CONFIG$functions_rds_url)",
    "if (!is.list(fn_bundle) || !is.character(fn_bundle$runtime_script_lines)) {",
    "  stop(\"functions bundle RDS is not in expected format.\", call. = FALSE)",
    "}",
    "eval(parse(text = fn_bundle$runtime_script_lines), envir = .GlobalEnv)",
    "",
    "static_bundle <- read_rds_url(CONFIG$static_bundle_rds_url)",
    "if (!is.list(static_bundle) || !is.list(static_bundle$index_data)) {",
    "  stop(\"static bundle RDS is not in expected format.\", call. = FALSE)",
    "}",
    "",
    "# IMPORTANT: replace this line with your Displayr respondent table reference.",
    "# Example: raw_user_data <- table.raw_user_data",
    "raw_user_data <- NULL",
    "if (!is.data.frame(raw_user_data)) {",
    "  stop(\"Set `raw_user_data` to a Displayr data-frame reference before running.\", call. = FALSE)",
    "}",
    "",
    "# Preflight schema validation for respondent data",
    "required_base_cols <- c(\"company\", \"year\")",
    "missing_base <- setdiff(required_base_cols, names(raw_user_data))",
    "if (length(missing_base) > 0L) {",
    "  stop(sprintf(\"`raw_user_data` is missing required columns: %s\", paste(missing_base, collapse = \", \")), call. = FALSE)",
    "}",
    "required_item_cols <- unique(as.character(static_bundle$index_data$item_data$item_id))",
    "required_item_cols <- required_item_cols[!is.na(required_item_cols) & nzchar(required_item_cols)]",
    "missing_items <- setdiff(required_item_cols, names(raw_user_data))",
    "if (length(missing_items) > 0L) {",
    "  preview <- utils::head(missing_items, 30)",
    "  suffix <- if (length(missing_items) > 30L) \" ...\" else \"\"",
    "  stop(sprintf(",
    "    \"`raw_user_data` is missing %d required item columns. First missing: %s%s\",",
    "    length(missing_items), paste(preview, collapse = \", \"), suffix",
    "  ), call. = FALSE)",
    "}",
    "if (ncol(raw_user_data) <= 3L) {",
    "  stop(\"Aggregate-only user data is not supported. Provide respondent-level data with item columns.\", call. = FALSE)",
    "}",
    "",
    "index_data <- static_bundle$index_data",
    "colors_table <- static_bundle$colors_table",
    "",
    "snapshot <- prep_ohep_snapshot(",
    "  raw_user_data = raw_user_data,",
    "  index_data = index_data,",
    "  predictive_data = static_bundle$predictive_data,",
    "  snapshot_id = format(Sys.time(), \"%Y%m%dT%H%M%SZ\", tz = \"UTC\")",
    ")",
    "",
    "# Example:",
    "# company <- as.character(snapshot$company_fundamental_year$company[[1]])",
    "# year <- as.integer(snapshot$company_fundamental_year$year[[1]])",
    "# fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])",
    "# fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)"
  )
  writeLines(github_loader_lines, file.path(payload_dir, "displayr_bootstrap_from_github_rds.R"), useBytes = TRUE)

  setup_lines <- c(
    "Displayr Template Bootstrap Setup (Primary: GitHub RDS)",
    "",
    "Primary path:",
    "1) Paste `displayr_payload/displayr_bootstrap_from_github_rds.R` into one Displayr R Output.",
    "2) Set `CONFIG$functions_rds_url` and `CONFIG$static_bundle_rds_url` to pinned commit-SHA raw URLs.",
    "3) Set one line: `raw_user_data <- <Displayr respondent table reference>`.",
    "4) Run script. It creates `index_data`, `colors_table`, and `snapshot`.",
    "",
    "Fallback path (CSV + local single paste):",
    "5) Upload static CSVs from `displayr_payload/` and paste `displayr_payload/displayr_bootstrap_single_paste.R`.",
    sprintf("6) In fallback mode, map names as: %s, %s, %s, %s, %s",
      dataset_names$index_item_data,
      dataset_names$index_user_data_key,
      dataset_names$predictive_data,
      dataset_names$colors_table,
      dataset_names$raw_user_data),
    "",
    "7) Use chart functions with marts = snapshot (e.g., fundamental_page(..., marts = snapshot)).",
    "8) Aggregate-only user data is not supported; respondent-level item columns are required.",
    "9) See function_reference_appendix.txt for syntax and usage of all exported functions."
  )
  writeLines(setup_lines, con = file.path(support_dir, "README_displayr_setup.txt"), useBytes = TRUE)
  writeLines(
    build_displayr_function_reference(),
    con = file.path(support_dir, "function_reference_appendix.txt"),
    useBytes = TRUE
  )

  manifest <- data.frame(
    field = c("bundle_id", "generated_at_utc", "payload_dir", "supports_github_rds_loader"),
    value = c(
      as.character(static_bundle$bundle_meta$bundle_id[[1]]),
      as.character(static_bundle$bundle_meta$generated_at_utc[[1]]),
      "displayr_payload",
      "yes"
    ),
    stringsAsFactors = FALSE
  )
  write_csv(manifest, file.path(support_dir, "manifest.csv"))

  if (isTRUE(include_rds)) {
    saveRDS(static_bundle, file.path(support_dir, "template_static_bundle.rds"))
  }

  invisible(normalizePath(out_dir, winslash = "/", mustWork = FALSE))
}
write_ohep_snapshot <-
function(snapshot, path, format = c("rds", "csv")) {
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

# Paste into a Displayr R Output / R Calculation
# Edit only the DATASET_MAP block below.

DATASET_MAP <- list(
  raw_user_data = "raw_user_data",
  index_item_data = "index_item_data",
  index_user_data_key = "index_user_data_key",
  predictive_data = "predictive_data",
  colors_table = "colors_table"
)

get_required_dataset <- function(obj_name) {
  if (!exists(obj_name, inherits = TRUE)) {
    stop(sprintf("Displayr data set '%s' was not found.", obj_name), call. = FALSE)
  }
  obj <- get(obj_name, inherits = TRUE)
  if (!is.data.frame(obj)) {
    stop(sprintf("Displayr object '%s' exists but is not a data frame.", obj_name), call. = FALSE)
  }
  obj
}

runtime <- ohepRDisplayr()

# Expected Displayr data set objects
index_data <- list(
  item_data = get_required_dataset(DATASET_MAP$index_item_data),
  user_data_key = get_required_dataset(DATASET_MAP$index_user_data_key),
  predictive_data = get_required_dataset(DATASET_MAP$predictive_data)
)
colors_table <- get_required_dataset(DATASET_MAP$colors_table)
raw_user_data <- get_required_dataset(DATASET_MAP$raw_user_data)

snapshot <- prep_ohep_snapshot(
  raw_user_data = raw_user_data,
  index_data = index_data,
  predictive_data = index_data$predictive_data,
  snapshot_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
)

# Example outputs:
# company <- as.character(snapshot$company_fundamental_year$company[[1]])
# year <- as.integer(snapshot$company_fundamental_year$year[[1]])
# fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])
# fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)
