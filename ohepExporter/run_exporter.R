#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(file_arg) == 0) {
    return(getwd())
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
}

trim_na <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

safe_row_mean <- function(df) {
  out <- rowMeans(df, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

score_dimension <- function(df, item_cols, threshold = 0.5) {
  if (length(item_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  required <- ceiling(length(item_cols) * threshold)
  enough <- rowSums(!is.na(df[, item_cols, drop = FALSE])) >= required
  score <- safe_row_mean(df[, item_cols, drop = FALSE])
  score[!enough] <- NA_real_
  score
}

to_title <- function(x) {
  parts <- strsplit(x, "_", fixed = TRUE)[[1]]
  paste(toupper(substring(parts, 1, 1)), substring(parts, 2), collapse = " ")
}

build_item_dictionary <- function() {
  counts <- c(
    vision = 4,
    purpose = 6,
    strategy = 9,
    communication = 6,
    leadership = 9,
    learning = 7,
    respect = 5,
    performance = 8,
    safety = 9,
    inclusion = 4,
    wellness = 4,
    ethics = 5,
    hybrid = 3,
    engagement = 5,
    burnout = 5,
    workSat = 2,
    turnover = 3
  )

  label_map <- c(
    vision = "Vision",
    purpose = "Purpose",
    strategy = "Strategy",
    communication = "Communication",
    leadership = "Leadership",
    learning = "Learning & innovation",
    respect = "Respect, care, and trust",
    performance = "Performance development",
    safety = "Safety",
    inclusion = "Inclusion & belonging",
    wellness = "Wellness support",
    ethics = "Ethical culture",
    hybrid = "Hybrid work satisfaction",
    engagement = "Engagement",
    burnout = "Burnout",
    workSat = "Work satisfaction",
    turnover = "Turnover intentions"
  )

  outcome_dims <- c("engagement", "burnout", "workSat", "turnover")
  reverse_items <- c(
    paste0("burnout_", 1:5),
    "turnover_3"
  )

  rows <- list()
  idx <- 1L
  for (dim in names(counts)) {
    n_items <- counts[[dim]]
    for (i in seq_len(n_items)) {
      item_id <- paste0(dim, "_", i)
      rows[[idx]] <- data.frame(
        item_id = item_id,
        type = if (dim %in% outcome_dims) "outcome" else "fundamental",
        fundamental_id = label_map[[dim]],
        facet_id = label_map[[dim]],
        is_reverse_scored = item_id %in% reverse_items,
        response_scale_min = 1,
        response_scale_max = 5,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  dict <- do.call(rbind, rows)
  dict
}

classify_domain <- function(item, description, scored_ids) {
  item_l <- tolower(item)
  desc_l <- tolower(ifelse(is.na(description), "", description))

  demographic_exact <- c(
    "demo_department", "workexp", "demo_position", "demo_tenure", "demo_age",
    "demo_employ", "demo_location", "employeegroup", "gender", "gender_other",
    "race_q", "race_other", "disability", "disability_q", "disability_other",
    "org_feedback", "leader", "education", "edu_other", "indigenous", "visminority",
    "visminority_comment"
  )

  if (item %in% scored_ids ||
      grepl("_completed$", item_l) ||
      item_l %in% c("year", "company", "enps", "return", "company_id", "survey_year")) {
    return("scaled_item")
  }

  if (item_l %in% demographic_exact ||
      grepl("^demo_", item_l) ||
      grepl("^race_", item_l) ||
      grepl("^disability_", item_l)) {
    return("demographic")
  }

  if (grepl("comment", item_l) ||
      grepl("comment", desc_l) ||
      grepl("_good$|_bad$|_other$|opportunit", item_l)) {
    return("open_text")
  }

  "demographic"
}

parse_input <- function(file_path) {
  raw <- read.csv(file_path, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(raw) < 3) {
    stop("Input file must include label row, variable-name row, and at least one respondent row.")
  }

  labels <- trim_na(raw[1, ])
  vars <- trim_na(raw[2, ])
  data <- raw[-c(1, 2), , drop = FALSE]

  helper_tokens <- c("Variable Name", "Metric", "X", "Unnamed: 0")
  first_var <- vars[1]
  if (is.na(first_var) || first_var %in% helper_tokens) {
    labels <- labels[-1]
    vars <- vars[-1]
    data <- data[, -1, drop = FALSE]
  }

  vars[is.na(vars)] <- paste0("unnamed_", which(is.na(vars)))
  keep <- !duplicated(vars)

  labels <- labels[keep]
  vars <- vars[keep]
  data <- data[, keep, drop = FALSE]

  colnames(data) <- vars
  data[] <- lapply(data, function(col) {
    col <- as.character(col)
    col <- trimws(col)
    col[col == ""] <- NA_character_
    col
  })

  list(data = data, vars = vars, labels = labels)
}

parse_args <- function(args) {
  input_path <- NULL
  if (length(args) == 0) {
    return(list(input_path = NULL))
  }
  i <- 1L
  while (i <= length(args)) {
    if (args[i] == "--input") {
      if (i == length(args)) stop("Missing value for --input")
      input_path <- args[i + 1]
      i <- i + 2L
    } else {
      stop(paste0("Unknown argument: ", args[i]))
    }
  }
  list(input_path = input_path)
}

script_dir <- get_script_dir()
setwd(script_dir)

args <- parse_args(commandArgs(trailingOnly = TRUE))
input_dir <- file.path(script_dir, "_inputData")
output_dir <- file.path(script_dir, "_outputData")
analytics_dir <- file.path(script_dir, "analytics")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "index_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "user_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "predictiva_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(analytics_dir, "trends"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(analytics_dir, "predictive"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(analytics_dir, "data_quality"), recursive = TRUE, showWarnings = FALSE)

if (is.null(args$input_path)) {
  candidates <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(candidates) != 1) {
    stop("Expected exactly one .csv file in _inputData for v1. Found: ", length(candidates))
  }
  input_file <- candidates[1]
} else {
  input_file <- normalizePath(args$input_path, mustWork = TRUE)
}

parsed <- parse_input(input_file)
user_data <- parsed$data
var_names <- parsed$vars
labels <- parsed$labels

label_map <- setNames(labels, var_names)
item_dict <- build_item_dictionary()
scored_ids <- intersect(item_dict$item_id, names(user_data))

required_cols <- c("year", "company")
missing_required <- setdiff(required_cols, names(user_data))
if (length(missing_required) > 0 || length(scored_ids) < 20) {
  stop(
    paste0(
      "Input validation failed: this is not a respondent-level OHEP long-format file. ",
      "Missing required columns: ", paste(missing_required, collapse = ", "),
      "; scored item columns detected: ", length(scored_ids), "."
    )
  )
}

numeric_targets <- unique(c(
  scored_ids,
  grep("_completed$", names(user_data), value = TRUE),
  c("year", "company", "eNPS", "return")
))

for (nm in intersect(numeric_targets, names(user_data))) {
  user_data[[nm]] <- suppressWarnings(as.numeric(user_data[[nm]]))
}

scored_data <- user_data
for (i in seq_len(nrow(item_dict))) {
  item <- item_dict$item_id[i]
  if (!item %in% names(scored_data)) next
  if (!isTRUE(item_dict$is_reverse_scored[i])) next
  min_v <- item_dict$response_scale_min[i]
  max_v <- item_dict$response_scale_max[i]
  x <- scored_data[[item]]
  scored_data[[item]] <- ifelse(is.na(x), NA_real_, (max_v + min_v) - x)
}

item_groups <- split(item_dict$item_id, item_dict$fundamental_id)
for (fid in names(item_groups)) {
  cols <- intersect(item_groups[[fid]], names(scored_data))
  score_name <- paste0(gsub("[^A-Za-z0-9]", "", fid), "_score")
  scored_data[[score_name]] <- score_dimension(scored_data, cols, threshold = 0.5)
}

score_col_for <- function(label) paste0(gsub("[^A-Za-z0-9]", "", label), "_score")

core_labels <- c(
  "Purpose", "Strategy", "Leadership", "Communication",
  "Learning & innovation", "Performance development", "Respect, care, and trust"
)
core_cols <- intersect(vapply(core_labels, score_col_for, character(1)), names(scored_data))
outcome_labels <- c("Engagement", "Burnout", "Work satisfaction", "Turnover intentions")
outcome_cols <- intersect(vapply(outcome_labels, score_col_for, character(1)), names(scored_data))

scored_data$OHEP_score <- safe_row_mean(scored_data[, core_cols, drop = FALSE])
scored_data$OUTCOMES_score <- safe_row_mean(scored_data[, outcome_cols, drop = FALSE])

user_key <- data.frame(
  item = names(user_data),
  Description = vapply(names(user_data), function(nm) {
    d <- label_map[[nm]]
    if (is.null(d) || is.na(d) || trimws(d) == "") nm else d
  }, character(1)),
  data_domain = vapply(names(user_data), function(nm) {
    classify_domain(nm, label_map[[nm]], scored_ids)
  }, character(1)),
  stringsAsFactors = FALSE
)

write.csv(user_key, file.path(output_dir, "index_data", "user_data_key.csv"), row.names = FALSE, na = "")
write.csv(user_data, file.path(output_dir, "user_data", "user_data.csv"), row.names = FALSE, na = "")

years <- sort(unique(suppressWarnings(as.integer(scored_data$year))))
years <- years[!is.na(years)]
year_cols <- paste0(years, "_mean")

item_rows <- list()
row_idx <- 1L
for (i in seq_len(nrow(item_dict))) {
  item <- item_dict$item_id[i]
  if (!item %in% names(scored_data)) next
  row <- list(
    type = item_dict$type[i],
    item_id = item,
    item_text = ifelse(!is.null(label_map[[item]]) && !is.na(label_map[[item]]), label_map[[item]], item),
    fundamental_id = item_dict$fundamental_id[i],
    facet_id = item_dict$facet_id[i]
  )

  for (yr in years) {
    idx <- !is.na(scored_data$year) & as.integer(scored_data$year) == yr
    mean_val <- mean(scored_data[[item]][idx], na.rm = TRUE)
    if (is.nan(mean_val)) mean_val <- NA_real_
    row[[paste0(yr, "_mean")]] <- mean_val
  }

  row$is_reverse_scored <- item_dict$is_reverse_scored[i]
  row$response_scale_min <- item_dict$response_scale_min[i]
  row$response_scale_max <- item_dict$response_scale_max[i]

  item_rows[[row_idx]] <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  row_idx <- row_idx + 1L
}

item_data <- do.call(rbind, item_rows)
write.csv(item_data, file.path(output_dir, "index_data", "item_data.csv"), row.names = FALSE, na = "")

predictive_outcomes <- list(
  Engagement = score_col_for("Engagement"),
  Burnout = score_col_for("Burnout"),
  `Work Satisfaction` = score_col_for("Work satisfaction"),
  `Turnover Intentions` = score_col_for("Turnover intentions"),
  Outcomes = "OUTCOMES_score"
)
if ("eNPS" %in% names(scored_data)) {
  predictive_outcomes[["eNPS"]] <- "eNPS"
}

core_predictors <- setNames(
  vapply(core_labels, score_col_for, character(1)),
  core_labels
)

fit_standardized <- function(df, outcome_col, predictor_map, subset_name, type_name, outcome_label) {
  predictors <- unname(predictor_map)
  keep <- c(outcome_col, predictors)
  keep <- keep[keep %in% names(df)]
  if (!outcome_col %in% keep) return(list(summary = NULL, detail = NULL))

  m <- df[, keep, drop = FALSE]
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (nrow(m) < 30) return(list(summary = NULL, detail = NULL))
  if (stats::sd(m[[outcome_col]]) == 0 || is.na(stats::sd(m[[outcome_col]]))) return(list(summary = NULL, detail = NULL))

  pred_cols <- setdiff(colnames(m), outcome_col)
  pred_cols <- pred_cols[vapply(pred_cols, function(col) {
    s <- stats::sd(m[[col]])
    !is.na(s) && s > 0
  }, logical(1))]
  if (length(pred_cols) == 0) return(list(summary = NULL, detail = NULL))

  y_std <- as.numeric(scale(m[[outcome_col]]))
  x_std <- as.data.frame(scale(m[, pred_cols, drop = FALSE]))
  model_df <- cbind(y = y_std, x_std)
  fit <- stats::lm(y ~ ., data = model_df)
  s <- summary(fit)
  coefs <- as.data.frame(s$coefficients)
  coefs$term <- rownames(coefs)
  coefs <- coefs[coefs$term != "(Intercept)", , drop = FALSE]
  if (nrow(coefs) == 0) return(list(summary = NULL, detail = NULL))

  predictor_lookup <- setNames(names(predictor_map), predictor_map)
  coefs$predictor_label <- predictor_lookup[coefs$term]
  coefs$predictor_label[is.na(coefs$predictor_label)] <- coefs$term

  ci <- suppressWarnings(stats::confint(fit))
  ci <- ci[rownames(ci) != "(Intercept)", , drop = FALSE]

  summary_rows <- data.frame(
    Fundamental = coefs$predictor_label,
    Outcome = outcome_label,
    subset = subset_name,
    strength = unname(coefs$Estimate),
    strength_ci_low = unname(ci[, 1]),
    strength_ci_high = unname(ci[, 2]),
    type = type_name,
    direction = ifelse(coefs$Estimate > 0, "positive", ifelse(coefs$Estimate < 0, "negative", "neutral")),
    significant = ifelse((coefs$`Pr(>|t|)` < 0.05) & ((ci[, 1] > 0) | (ci[, 2] < 0)), "TRUE", "FALSE"),
    N = nrow(m),
    stringsAsFactors = FALSE
  )

  detail_rows <- data.frame(
    Fundamental = coefs$predictor_label,
    Outcome = outcome_label,
    subset = subset_name,
    type = type_name,
    estimate = unname(coefs$Estimate),
    std_error = unname(coefs$`Std. Error`),
    t_value = unname(coefs$`t value`),
    p_value = unname(coefs$`Pr(>|t|)`),
    ci_low = unname(ci[, 1]),
    ci_high = unname(ci[, 2]),
    strength = unname(coefs$Estimate),
    N = nrow(m),
    stringsAsFactors = FALSE
  )

  list(summary = summary_rows, detail = detail_rows)
}

summary_rows <- list()
detail_rows <- list()
list_idx <- 1L

ohep_outcomes <- predictive_outcomes
for (outcome_name in names(ohep_outcomes)) {
  out_col <- ohep_outcomes[[outcome_name]]
  if (!out_col %in% names(scored_data)) next

  fit_res <- fit_standardized(
    df = scored_data,
    outcome_col = out_col,
    predictor_map = c(OHEP = "OHEP_score"),
    subset_name = "All",
    type_name = "composite",
    outcome_label = outcome_name
  )

  if (!is.null(fit_res$summary)) {
    summary_rows[[list_idx]] <- fit_res$summary
    detail_rows[[list_idx]] <- fit_res$detail
    list_idx <- list_idx + 1L
  }
}

fundamentals_outcome_col <- predictive_outcomes[["Outcomes"]]
if (!is.null(fundamentals_outcome_col) && fundamentals_outcome_col %in% names(scored_data)) {
  fit_res <- fit_standardized(
    df = scored_data,
    outcome_col = fundamentals_outcome_col,
    predictor_map = core_predictors,
    subset_name = "All",
    type_name = "fundamental",
    outcome_label = "Outcomes"
  )

  if (!is.null(fit_res$summary)) {
    summary_rows[[list_idx]] <- fit_res$summary
    detail_rows[[list_idx]] <- fit_res$detail
    list_idx <- list_idx + 1L
  }
}

fundamental_each_outcomes <- predictive_outcomes[names(predictive_outcomes) != "Outcomes"]
for (outcome_name in names(fundamental_each_outcomes)) {
  out_col <- fundamental_each_outcomes[[outcome_name]]
  if (!out_col %in% names(scored_data)) next

  fit_res <- fit_standardized(
    df = scored_data,
    outcome_col = out_col,
    predictor_map = core_predictors,
    subset_name = "All",
    type_name = "fundamental",
    outcome_label = outcome_name
  )

  if (!is.null(fit_res$summary)) {
    summary_rows[[list_idx]] <- fit_res$summary
    detail_rows[[list_idx]] <- fit_res$detail
    list_idx <- list_idx + 1L
  }
}

if (length(summary_rows) == 0) {
  predictive_data <- data.frame(
    Fundamental = character(0),
    Outcome = character(0),
    subset = character(0),
    strength = numeric(0),
    strength_ci_low = numeric(0),
    strength_ci_high = numeric(0),
    type = character(0),
    direction = character(0),
    significant = character(0),
    N = integer(0),
    stringsAsFactors = FALSE
  )
  predictive_detail <- predictive_data
} else {
  predictive_data <- do.call(rbind, summary_rows)
  predictive_detail <- do.call(rbind, detail_rows)
}

write.csv(predictive_data, file.path(output_dir, "predictiva_data", "predictive_data.csv"), row.names = FALSE, na = "")

score_export_cols <- c(core_cols, outcome_cols, "OHEP_score", "OUTCOMES_score")
score_export_cols <- unique(score_export_cols[score_export_cols %in% names(scored_data)])

company_year_scores <- aggregate(
  scored_data[, score_export_cols, drop = FALSE],
  by = list(company = scored_data$company, year = scored_data$year),
  FUN = function(x) {
    m <- mean(x, na.rm = TRUE)
    if (is.nan(m)) NA_real_ else m
  }
)
company_year_scores <- company_year_scores[order(company_year_scores$company, company_year_scores$year), , drop = FALSE]

yearly_means <- aggregate(
  scored_data[, score_export_cols, drop = FALSE],
  by = list(year = scored_data$year),
  FUN = function(x) {
    m <- mean(x, na.rm = TRUE)
    if (is.nan(m)) NA_real_ else m
  }
)
yearly_means <- yearly_means[order(yearly_means$year), , drop = FALSE]

yoy_rows <- list()
yoy_idx <- 1L
for (metric in score_export_cols) {
  vals <- yearly_means[[metric]]
  yrs <- yearly_means$year
  if (length(vals) < 2) next
  for (i in 2:length(vals)) {
    yoy_rows[[yoy_idx]] <- data.frame(
      metric = metric,
      from_year = yrs[i - 1],
      to_year = yrs[i],
      movement = vals[i] - vals[i - 1],
      stringsAsFactors = FALSE
    )
    yoy_idx <- yoy_idx + 1L
  }
}
yoy_data <- if (length(yoy_rows) == 0) data.frame(metric = character(0), from_year = integer(0), to_year = integer(0), movement = numeric(0)) else do.call(rbind, yoy_rows)

trend_long_rows <- list()
trend_idx <- 1L
for (metric in score_export_cols) {
  trend_long_rows[[trend_idx]] <- data.frame(
    year = yearly_means$year,
    metric = metric,
    mean_score = yearly_means[[metric]],
    stringsAsFactors = FALSE
  )
  trend_idx <- trend_idx + 1L
}
index_trend_long <- do.call(rbind, trend_long_rows)

write.csv(company_year_scores, file.path(analytics_dir, "trends", "company_year_scores.csv"), row.names = FALSE, na = "")
write.csv(yoy_data, file.path(analytics_dir, "trends", "year_over_year_movement.csv"), row.names = FALSE, na = "")
write.csv(index_trend_long, file.path(analytics_dir, "trends", "index_trend_long.csv"), row.names = FALSE, na = "")

if (nrow(predictive_detail) > 0) {
  ranked <- predictive_detail[order(-abs(predictive_detail$strength), predictive_detail$p_value), , drop = FALSE]
} else {
  ranked <- predictive_detail
}

write.csv(predictive_detail, file.path(analytics_dir, "predictive", "full_model_coefficients.csv"), row.names = FALSE, na = "")
write.csv(ranked, file.path(analytics_dir, "predictive", "ranked_drivers.csv"), row.names = FALSE, na = "")
write.csv(predictive_data, file.path(analytics_dir, "predictive", "graph_ready_predictive.csv"), row.names = FALSE, na = "")

validation_summary <- data.frame(
  metric = c(
    "input_file", "rows", "columns", "years_detected", "companies_detected",
    "scored_item_columns_detected", "reverse_scored_items_present", "run_timestamp"
  ),
  value = c(
    input_file,
    nrow(user_data),
    ncol(user_data),
    length(unique(na.omit(user_data$year))),
    length(unique(na.omit(user_data$company))),
    length(scored_ids),
    sum(item_dict$is_reverse_scored & item_dict$item_id %in% names(user_data)),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ),
  stringsAsFactors = FALSE
)

coverage_rows <- list()
coverage_idx <- 1L
for (yr in years) {
  y_idx <- !is.na(scored_data$year) & as.integer(scored_data$year) == yr
  n_yr <- sum(y_idx)
  if (n_yr == 0) next
  for (item in scored_ids) {
    coverage_rows[[coverage_idx]] <- data.frame(
      year = yr,
      item_id = item,
      n_respondents = n_yr,
      non_missing_n = sum(!is.na(scored_data[[item]][y_idx])),
      non_missing_pct = mean(!is.na(scored_data[[item]][y_idx])) * 100,
      stringsAsFactors = FALSE
    )
    coverage_idx <- coverage_idx + 1L
  }
}
item_coverage <- if (length(coverage_rows) == 0) data.frame(year = integer(0), item_id = character(0), n_respondents = integer(0), non_missing_n = integer(0), non_missing_pct = numeric(0)) else do.call(rbind, coverage_rows)

write.csv(validation_summary, file.path(analytics_dir, "data_quality", "validation_summary.csv"), row.names = FALSE, na = "")
write.csv(item_coverage, file.path(analytics_dir, "data_quality", "item_coverage_by_year.csv"), row.names = FALSE, na = "")

writeLines(
  c(
    "# Trends Analytics",
    "",
    "This folder contains graph-ready trend outputs generated by run_exporter.R.",
    "",
    "- `company_year_scores.csv`: company-year mean scores for core fundamentals, outcomes, and composites.",
    "- `year_over_year_movement.csv`: yearly movement deltas by score metric.",
    "- `index_trend_long.csv`: long format yearly means for plotting trajectories."
  ),
  file.path(analytics_dir, "trends", "README.md")
)

writeLines(
  c(
    "# Predictive Analytics",
    "",
    "This folder contains model outputs used to rank drivers and create predictive visuals.",
    "",
    "- `full_model_coefficients.csv`: standardized model terms with errors, p-values, and confidence intervals.",
    "- `ranked_drivers.csv`: coefficients ranked by absolute standardized strength.",
    "- `graph_ready_predictive.csv`: concise predictive table aligned with `_outputData/predictiva_data/predictive_data.csv`."
  ),
  file.path(analytics_dir, "predictive", "README.md")
)

writeLines(
  c(
    "# Data Quality",
    "",
    "This folder captures validation and coverage diagnostics for each exporter run.",
    "",
    "- `validation_summary.csv`: high-level run and contract validation facts.",
    "- `item_coverage_by_year.csv`: non-missing item coverage by survey year."
  ),
  file.path(analytics_dir, "data_quality", "README.md")
)

cat("Exporter completed successfully.\\n")
cat("Input:", input_file, "\\n")
cat("Rows:", nrow(user_data), " Columns:", ncol(user_data), "\\n")
