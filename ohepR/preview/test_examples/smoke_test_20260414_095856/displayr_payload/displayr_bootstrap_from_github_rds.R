# Displayr GitHub RDS Bootstrap Script
# PRIMARY FLOW: load ohepR functions + static data from GitHub-hosted RDS files.
# Pin URLs to a commit SHA for reproducibility.
# Edit the CONFIG block and raw_user_data assignment only.

CONFIG <- list(
  # Example pinned URL:
  # https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_functions_bundle.rds
  functions_rds_url = "https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_functions_bundle.rds",
  static_bundle_rds_url = "https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_static_bundle.rds"
)

read_rds_url <- function(u) {
  con <- url(u, open = "rb")
  on.exit(close(con), add = TRUE)
  readRDS(con)
}

fn_bundle <- read_rds_url(CONFIG$functions_rds_url)
if (!is.list(fn_bundle) || !is.character(fn_bundle$runtime_script_lines)) {
  stop("functions bundle RDS is not in expected format.", call. = FALSE)
}
eval(parse(text = fn_bundle$runtime_script_lines), envir = .GlobalEnv)

static_bundle <- read_rds_url(CONFIG$static_bundle_rds_url)
if (!is.list(static_bundle) || !is.list(static_bundle$index_data)) {
  stop("static bundle RDS is not in expected format.", call. = FALSE)
}

# IMPORTANT: replace this line with your Displayr respondent table reference.
# Example: raw_user_data <- table.raw_user_data
raw_user_data <- NULL
if (!is.data.frame(raw_user_data)) {
  stop("Set `raw_user_data` to a Displayr data-frame reference before running.", call. = FALSE)
}

# Preflight schema validation for respondent data
required_base_cols <- c("company", "year")
missing_base <- setdiff(required_base_cols, names(raw_user_data))
if (length(missing_base) > 0L) {
  stop(sprintf("`raw_user_data` is missing required columns: %s", paste(missing_base, collapse = ", ")), call. = FALSE)
}
required_item_cols <- unique(as.character(static_bundle$index_data$item_data$item_id))
required_item_cols <- required_item_cols[!is.na(required_item_cols) & nzchar(required_item_cols)]
missing_items <- setdiff(required_item_cols, names(raw_user_data))
if (length(missing_items) > 0L) {
  preview <- utils::head(missing_items, 30)
  suffix <- if (length(missing_items) > 30L) " ..." else ""
  stop(sprintf(
    "`raw_user_data` is missing %d required item columns. First missing: %s%s",
    length(missing_items), paste(preview, collapse = ", "), suffix
  ), call. = FALSE)
}
if (ncol(raw_user_data) <= 3L) {
  stop("Aggregate-only user data is not supported. Provide respondent-level data with item columns.", call. = FALSE)
}

index_data <- static_bundle$index_data
colors_table <- static_bundle$colors_table

snapshot <- prep_ohep_snapshot(
  raw_user_data = raw_user_data,
  index_data = index_data,
  predictive_data = static_bundle$predictive_data,
  snapshot_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
)

# Example:
# company <- as.character(snapshot$company_fundamental_year$company[[1]])
# year <- as.integer(snapshot$company_fundamental_year$year[[1]])
# fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])
# fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)
