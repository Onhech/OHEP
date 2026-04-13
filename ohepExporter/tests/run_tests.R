#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
root_dir <- dirname(script_dir)
run_script <- "run_exporter.R"

run_cmd <- function(args, wd) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  cmd_args <- c(run_script, vapply(args, shQuote, character(1)))
  out <- system2("Rscript", cmd_args, stdout = TRUE, stderr = TRUE)

  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = paste(out, collapse = "\n"))
}

# Test 1: fail-fast on sparse _inputData file
fail_run <- run_cmd(args = character(0), wd = root_dir)
if (fail_run$status == 0L) {
  stop("Expected fail-fast validation when using sparse _inputData file, but exporter succeeded.")
}
if (!grepl("not a respondent-level OHEP long-format file", fail_run$output, fixed = TRUE)) {
  stop("Fail-fast message did not include expected validation guidance.")
}

# Test 2: golden-path run with detailed source file
golden_input <- normalizePath(file.path(root_dir, "..", "OHEP_Long_5.28.25.csv"), mustWork = TRUE)
golden_run <- run_cmd(args = c("--input", golden_input), wd = root_dir)
if (golden_run$status != 0L) {
  stop("Golden-path run failed. Output:\n", golden_run$output)
}

required_files <- c(
  file.path(root_dir, "_outputData", "index_data", "user_data_key.csv"),
  file.path(root_dir, "_outputData", "index_data", "item_data.csv"),
  file.path(root_dir, "_outputData", "predictiva_data", "predictive_data.csv"),
  file.path(root_dir, "_outputData", "user_data", "user_data.csv"),
  file.path(root_dir, "analytics", "trends", "company_year_scores.csv"),
  file.path(root_dir, "analytics", "predictive", "full_model_coefficients.csv"),
  file.path(root_dir, "analytics", "data_quality", "validation_summary.csv"),
  file.path(root_dir, "analytics", "trends", "README.md"),
  file.path(root_dir, "analytics", "predictive", "README.md"),
  file.path(root_dir, "analytics", "data_quality", "README.md")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required output files:\n", paste(missing_files, collapse = "\n"))
}

# Schema checks
user_data <- read.csv(file.path(root_dir, "_outputData", "user_data", "user_data.csv"), check.names = FALSE)
user_key <- read.csv(file.path(root_dir, "_outputData", "index_data", "user_data_key.csv"), check.names = FALSE)
item_data <- read.csv(file.path(root_dir, "_outputData", "index_data", "item_data.csv"), check.names = FALSE)
predictive_data <- read.csv(file.path(root_dir, "_outputData", "predictiva_data", "predictive_data.csv"), check.names = FALSE)
trends <- read.csv(file.path(root_dir, "analytics", "trends", "company_year_scores.csv"), check.names = FALSE)

if (!identical(colnames(user_data), as.character(user_key$item))) {
  stop("user_data_key item column does not match user_data headers exactly.")
}

if (!all(c("type", "item_id", "item_text", "fundamental_id", "facet_id", "is_reverse_scored", "response_scale_min", "response_scale_max") %in% colnames(item_data))) {
  stop("item_data.csv missing required columns.")
}
if (length(grep("^[0-9]{4}_mean$", colnames(item_data))) == 0) {
  stop("item_data.csv is missing dynamic `YYYY_mean` columns.")
}

if (!all(c("Fundamental", "Outcome", "subset", "strength", "strength_ci_low", "strength_ci_high", "type", "direction", "significant", "N") %in% colnames(predictive_data))) {
  stop("predictive_data.csv missing required columns.")
}

if (nrow(subset(predictive_data, subset == "All")) == 0) {
  stop("predictive_data.csv does not contain estimable subset='All' rows.")
}

if (nrow(subset(predictive_data, Fundamental == "OHEP")) == 0) {
  stop("predictive_data.csv missing OHEP->outcome rows.")
}

if (nrow(subset(predictive_data, type == "fundamental" & Outcome == "Outcomes")) == 0) {
  stop("predictive_data.csv missing Fundamentals->Outcomes rows.")
}

if (nrow(subset(predictive_data, type == "fundamental" & Outcome != "Outcomes")) == 0) {
  stop("predictive_data.csv missing Fundamentals->each-outcome rows.")
}

if (!all(c("OHEP_score", "OUTCOMES_score") %in% colnames(trends))) {
  stop("Trend output missing required composite score columns.")
}

# Reverse-scoring check on burnout_1 yearly mean
source_df <- read.csv(golden_input, skip = 1, check.names = FALSE)
if (names(source_df)[1] %in% c("Variable Name", "Metric", "X", "Unnamed: 0")) {
  source_df[[1]] <- NULL
}
if (anyDuplicated(names(source_df)) > 0) {
  source_df <- source_df[, !duplicated(names(source_df))]
}
source_df$year <- suppressWarnings(as.integer(source_df$year))
source_df$burnout_1 <- suppressWarnings(as.numeric(source_df$burnout_1))

burnout_item <- subset(item_data, item_id == "burnout_1")
if (nrow(burnout_item) == 1) {
  yr_cols <- grep("^[0-9]{4}_mean$", colnames(burnout_item), value = TRUE)
  yr_cols <- yr_cols[!is.na(burnout_item[1, yr_cols])]
  if (length(yr_cols) > 0) {
    test_col <- yr_cols[1]
    test_year <- as.integer(sub("_mean$", "", test_col))
    raw_mean <- mean(source_df$burnout_1[source_df$year == test_year], na.rm = TRUE)
    rev_mean <- 6 - raw_mean
    out_mean <- as.numeric(burnout_item[1, test_col])
    if (!is.na(out_mean) && !is.na(rev_mean) && abs(out_mean - rev_mean) > 1e-6) {
      stop("Reverse-scoring validation failed for burnout_1 year mean.")
    }
  }
}

cat("All tests passed.\n")
