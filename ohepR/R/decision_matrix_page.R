#' Render Decision Matrix Page(s)
#'
#' Render decision matrix chart(s) with fundamentals plotted as:
#' - X-axis: company performance vs. index benchmark
#' - Y-axis: predictive impact (scaled MLM coefficient strength)
#'
#' There are two supported modes:
#' 1) `points` mode: render a single matrix from explicit point data.
#' 2) `marts` mode: render one matrix per outcome plus an equally weighted
#'    outcomes composite.
#'
#' @param points Optional data frame with required columns:
#'   - `fundamental` (character label/id)
#'   - `score` (numeric X-axis value; typically vs-benchmark delta)
#'   - `impact` (numeric Y-axis value)
#'   Optional columns:
#'   - `label` (display label; defaults to `fundamental`)
#'   - `priority` (`critical`, `leverage`, `monitor`, `maintain`) to force dot
#'     color/category.
#' @param company Company identifier (required in `marts` mode).
#' @param year Reporting year (required in `marts` mode).
#' @param marts Snapshot marts from [prep_ohep_snapshot()] (required in
#'   `marts` mode).
#' @param outcomes Optional character vector of outcomes to include in
#'   `marts` mode. Defaults to all available outcomes except `"Outcomes"`.
#' @param include_composite Logical; include an additional equally weighted
#'   composite matrix across outcomes in `marts` mode.
#' @param subset Predictive subset filter in `marts` mode (default `"All"` when
#'   the `subset` column exists).
#' @param significant_only Logical; when `TRUE`, keep only significant
#'   predictive rows if a `significant` column exists. Defaults to `FALSE`.
#' @param fallback_to_latest_year Logical; when `TRUE` and the requested year is
#'   unavailable for the company, use latest available year.
#' @param as_list Logical; in `marts` mode return a named list of html outputs
#'   (one per matrix) instead of a single `tagList`.
#' @param id HTML id used to scope CSS for rendered chart(s).
#' @param theme_kicker Kicker text shown above the title.
#' @param title Title text for the chart.
#' @param subtitle Optional subtitle text.
#' @param x_metric X-axis metric in `marts` mode. One of
#'   `"benchmark_delta"`, `"prior_delta"`, `"impact"`, or `"opportunity"`.
#' @param y_metric Y-axis metric in `marts` mode. One of
#'   `"impact"`, `"benchmark_delta"`, `"prior_delta"`, or `"opportunity"`.
#' @param x_split_mode Split mode for X-axis quadrants in `marts` mode.
#'   One of `"auto"`, `"zero"`, or `"median"`.
#' @param y_split_mode Split mode for Y-axis quadrants in `marts` mode.
#'   One of `"auto"`, `"zero"`, or `"median"`.
#' @param y_axis_main Y-axis title text.
#' @param x_axis_main X-axis title text.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param ... Inline color overrides (e.g. `matrix_dot_critical = "#ff0000"`).
#'
#' @return In `points` mode, a single `htmltools` object. In `marts` mode,
#'   either a `tagList` containing all matrices or a named list if
#'   `as_list = TRUE`.
#' @export
decision_matrix_page <- function(
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
