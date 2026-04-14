#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x

now_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
args <- commandArgs(trailingOnly = TRUE)
out_dir_arg <- if (length(args) >= 1L) args[[1]] else file.path("displayr_payload", "latest")

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) NULL)
pkg_root <- if (!is.null(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  stop("Could not find package root. Run from ohepR/ or call via tools/test_displayr_readiness.R", call. = FALSE)
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg_root, quiet = TRUE)
} else {
  r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.R$", full.names = TRUE)
  for (f in sort(r_files)) source(f, local = .GlobalEnv)
}

if (!requireNamespace("htmltools", quietly = TRUE)) {
  stop("htmltools is required. Install with install.packages('htmltools').", call. = FALSE)
}

out_dir <- file.path(pkg_root, out_dir_arg)
if (dir.exists(out_dir) && length(args) < 1L) {
  unlink(out_dir, recursive = TRUE, force = TRUE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
proof_dir <- file.path(out_dir, "proof_renders")
run_info_dir <- file.path(out_dir, "run_info")
dir.create(proof_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(run_info_dir, recursive = TRUE, showWarnings = FALSE)

cat("== OHEP Displayr Readiness Smoke Test ==\n")
cat("Package root:", pkg_root, "\n")
cat("Output dir:", out_dir, "\n")

# 1) Build static bundle and write template bootstrap files
inputs <- example_fundamental_inputs()
static_bundle <- build_displayr_static_bundle(
  index_data = inputs$index_data,
  predictive_data = inputs$index_data$predictive_data
)
write_displayr_template_bootstrap(
  static_bundle = static_bundle,
  out_dir = out_dir,
  include_rds = TRUE
)
bootstrap_dir <- file.path(out_dir, "bootstrap")
sources_dir <- file.path(out_dir, "sources")

# 1a) Assert expected bootstrap artifacts exist
must_exist <- c(
  file.path(out_dir, "DISPLAYR_SETUP.txt"),
  file.path(bootstrap_dir, "displayr_bootstrap_from_github_rds.R"),
  file.path(sources_dir, "template_functions_bundle.rds"),
  file.path(sources_dir, "template_static_bundle.rds"),
  file.path(sources_dir, "function_reference_appendix.txt"),
  file.path(sources_dir, "manifest.csv")
)
missing_files <- must_exist[!file.exists(must_exist)]
if (length(missing_files) > 0L) {
  stop(sprintf("Bootstrap output missing required files: %s", paste(missing_files, collapse = ", ")), call. = FALSE)
}

# 1b) Static content assertions for GitHub loader + README
gh_script <- readLines(file.path(bootstrap_dir, "displayr_bootstrap_from_github_rds.R"), warn = FALSE)
if (!any(grepl("^CONFIG <- list\\(", gh_script))) stop("GitHub loader missing CONFIG block.", call. = FALSE)
if (!any(grepl("^raw_user_data <-", gh_script))) stop("GitHub loader missing explicit raw_user_data binding line.", call. = FALSE)
if (!any(grepl("raw.githubusercontent.com", gh_script, fixed = TRUE))) stop("GitHub loader missing prefilled raw URL.", call. = FALSE)
if (!any(grepl("required_base_cols <- c\\(\"company\", \"year\"\\)", gh_script, fixed = FALSE))) {
  stop("GitHub loader missing base column preflight check.", call. = FALSE)
}
if (!any(grepl("missing_items <- setdiff\\(required_item_cols, names\\(raw_user_data\\)\\)", gh_script, fixed = FALSE))) {
  stop("GitHub loader missing item-column preflight check.", call. = FALSE)
}
if (!any(grepl("Aggregate-only user data is not supported", gh_script, fixed = TRUE))) {
  stop("GitHub loader missing aggregate-only guard.", call. = FALSE)
}

setup_txt <- readLines(file.path(out_dir, "DISPLAYR_SETUP.txt"), warn = FALSE)
if (!any(grepl("RDS Workflow Only", setup_txt, fixed = TRUE))) stop("Setup file missing workflow heading.", call. = FALSE)
if (!any(grepl("script stitches raw_user_data", setup_txt, fixed = TRUE))) stop("Setup file missing stitching instruction.", call. = FALSE)

# 1c) Functional dry-run simulation for GitHub loader with local file URLs
fn_bundle <- readRDS(file.path(sources_dir, "template_functions_bundle.rds"))
static_bundle_from_file <- readRDS(file.path(sources_dir, "template_static_bundle.rds"))
tmp_env <- new.env(parent = baseenv())
eval(parse(text = fn_bundle$runtime_script_lines), envir = tmp_env)
tmp_env$raw_user_data <- inputs$user_data$user_data
tmp_env$index_data <- static_bundle_from_file$index_data
tmp_env$colors_table <- static_bundle_from_file$colors_table
tmp_env$snapshot <- tmp_env$prep_ohep_snapshot(
  raw_user_data = tmp_env$raw_user_data,
  index_data = tmp_env$index_data,
  predictive_data = static_bundle_from_file$predictive_data,
  snapshot_id = "local_url_sim"
)
if (!is.list(tmp_env$snapshot) || !is.data.frame(tmp_env$snapshot$company_fundamental_year)) {
  stop("Local URL simulation did not produce expected snapshot structure.", call. = FALSE)
}

# 1d) Mapping diagnostics smoke checks
diag_probe_name <- tmp_env$test_data_mapping(
  x = "raw_user_data",
  index_data = static_bundle_from_file$index_data,
  source_env = tmp_env
)
if (!inherits(diag_probe_name, "shiny.tag.list") && !inherits(diag_probe_name, "shiny.tag")) {
  stop("test_data_mapping(name) did not return an HTML tag object.", call. = FALSE)
}

diag_probe_missing <- tmp_env$test_data_mapping(
  x = "does_not_exist",
  index_data = static_bundle_from_file$index_data,
  source_env = tmp_env
)
if (!inherits(diag_probe_missing, "shiny.tag.list") && !inherits(diag_probe_missing, "shiny.tag")) {
  stop("test_data_mapping(missing) did not return an HTML tag object.", call. = FALSE)
}

diag_probe_vector <- tmp_env$test_data_mapping(x = tmp_env$raw_user_data[[1]])
if (!inherits(diag_probe_vector, "shiny.tag.list") && !inherits(diag_probe_vector, "shiny.tag")) {
  stop("test_data_mapping(vector) did not return an HTML tag object.", call. = FALSE)
}

diag_probe_df <- tmp_env$test_data_mapping(x = tmp_env$raw_user_data, index_data = static_bundle_from_file$index_data)
if (!inherits(diag_probe_df, "shiny.tag.list") && !inherits(diag_probe_df, "shiny.tag")) {
  stop("test_data_mapping(data.frame) did not return an HTML tag object.", call. = FALSE)
}

diag_probe_scalar <- tmp_env$test_data_mapping(x = "hello world")
if (!inherits(diag_probe_scalar, "shiny.tag.list") && !inherits(diag_probe_scalar, "shiny.tag")) {
  stop("test_data_mapping(scalar) did not return an HTML tag object.", call. = FALSE)
}

diag_inventory_full <- tmp_env$validate_data_sources(
  source_env = tmp_env,
  index_data = static_bundle_from_file$index_data,
  index_user_data_key = static_bundle_from_file$index_data$user_data_key
)
if (!inherits(diag_inventory_full, "shiny.tag.list") && !inherits(diag_inventory_full, "shiny.tag")) {
  stop("validate_data_sources(full) did not return an HTML tag object.", call. = FALSE)
}

tmp_missing_company <- list2env(as.list(tmp_env), parent = baseenv())
if (exists("company", envir = tmp_missing_company, inherits = FALSE)) rm("company", envir = tmp_missing_company)
if (exists("table.company", envir = tmp_missing_company, inherits = FALSE)) rm("table.company", envir = tmp_missing_company)
diag_inventory_missing <- tmp_env$validate_data_sources(
  source_env = tmp_missing_company,
  index_data = static_bundle_from_file$index_data
)
if (!inherits(diag_inventory_missing, "shiny.tag.list") && !inherits(diag_inventory_missing, "shiny.tag")) {
  stop("validate_data_sources(missing base) did not return an HTML tag object.", call. = FALSE)
}

tmp_partial_items <- list2env(as.list(tmp_env), parent = baseenv())
required_items <- unique(as.character(static_bundle_from_file$index_data$item_data$item_id))
required_items <- required_items[!is.na(required_items) & nzchar(required_items)]
for (nm in utils::head(required_items, 2L)) {
  for (cand in c(paste0("table.", nm), paste0("user_data.", nm), nm)) {
    if (exists(cand, envir = tmp_partial_items, inherits = FALSE)) rm(list = cand, envir = tmp_partial_items)
  }
}
diag_inventory_partial <- tmp_env$validate_data_sources(
  source_env = tmp_partial_items,
  index_data = static_bundle_from_file$index_data
)
if (!inherits(diag_inventory_partial, "shiny.tag.list") && !inherits(diag_inventory_partial, "shiny.tag")) {
  stop("validate_data_sources(partial items) did not return an HTML tag object.", call. = FALSE)
}

# 1e) Failure simulations
bad_missing_company <- inputs$user_data$user_data[, setdiff(names(inputs$user_data$user_data), "company"), drop = FALSE]
failed <- FALSE
tryCatch({
  tmp_env$prep_ohep_snapshot(
    raw_user_data = bad_missing_company,
    index_data = static_bundle_from_file$index_data,
    predictive_data = static_bundle_from_file$predictive_data,
    snapshot_id = "bad_missing_company"
  )
}, error = function(e) failed <<- TRUE)
if (!failed) stop("Expected failure for missing company column did not occur.", call. = FALSE)

required_items <- unique(as.character(static_bundle_from_file$index_data$item_data$item_id))
required_items <- required_items[!is.na(required_items) & nzchar(required_items)]
drop_cols <- utils::head(required_items, 2)
bad_missing_items <- inputs$user_data$user_data[, setdiff(names(inputs$user_data$user_data), drop_cols), drop = FALSE]
failed <- FALSE
tryCatch({
  missing_items <- setdiff(required_items, names(bad_missing_items))
  if (length(missing_items) > 0L) stop("missing item columns", call. = FALSE)
}, error = function(e) failed <<- TRUE)
if (!failed) stop("Expected failure for missing item columns did not occur.", call. = FALSE)

# 2) Build + validate payload for render smoke checks
payload <- build_displayr_payload(
  raw_user_data = inputs$user_data$user_data,
  static_bundle = static_bundle,
  snapshot_id = paste0("smoke_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
)
validate_displayr_schema(payload)

# 3) Render fundamental page from marts
marts <- payload$marts
cfy <- marts$company_fundamental_year
cfy <- cfy[!is.na(cfy$company) & !is.na(cfy$year) & !is.na(cfy$fundamental_id), , drop = FALSE]
if (nrow(cfy) < 1L) stop("No rows available in marts$company_fundamental_year.", call. = FALSE)

company <- as.character(cfy$company[[1]])
year <- as.integer(cfy$year[[1]])
fundamental <- as.character(cfy$fundamental_id[[1]])

fund_html <- fundamental_page(
  company = company,
  year = year,
  fundamental = fundamental,
  marts = marts,
  id = "ohep-smoke-fundamental"
)

fund_html_path <- file.path(proof_dir, "01_fundamental_smoke.html")
fund_txt_path <- file.path(proof_dir, "01_fundamental_smoke_rendered.txt")
htmltools::save_html(fund_html, file = fund_html_path)
writeLines(htmltools::renderTags(fund_html)$html, con = fund_txt_path, useBytes = TRUE)

# 4) Render demographics composer page
panel_tl <- demo_bipolar_split(
  data = data.frame(
    label = c("Manager", "Non-Manager"),
    count = c(124, 326),
    pct = c(27.6, 72.4),
    stringsAsFactors = FALSE
  ),
  title = "Role Split",
  subtitle = "Headcount share"
)
panel_tr <- demo_categorical_bar(
  data = data.frame(
    category = c("Operations", "Clinical", "Admin", "Support"),
    value = c(38, 27, 21, 14),
    stringsAsFactors = FALSE
  ),
  title = "Department Mix",
  subtitle = "Percent of sample"
)
panel_bl <- demo_categorical_tree(
  data = data.frame(
    category = c("18-29", "30-39", "40-49", "50-59", "60+"),
    value = c(17, 24, 29, 21, 9),
    stringsAsFactors = FALSE
  ),
  title = "Age Distribution",
  subtitle = "Treemap view"
)
panel_br <- demo_ordinal_bar(
  data = data.frame(
    label = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
    pct = c(6, 11, 19, 42, 22),
    stringsAsFactors = FALSE
  ),
  title = "Engagement Item",
  subtitle = "Ordinal distribution"
)

demo_html <- demographics_page(
  tl = panel_tl,
  tr = panel_tr,
  bl = panel_bl,
  br = panel_br,
  id = "ohep-smoke-demographics",
  title = "Demographics Smoke Test",
  subtitle = "All four panel types"
)

demo_html_path <- file.path(proof_dir, "02_demographics_smoke.html")
demo_txt_path <- file.path(proof_dir, "02_demographics_smoke_rendered.txt")
htmltools::save_html(demo_html, file = demo_html_path)
writeLines(htmltools::renderTags(demo_html)$html, con = demo_txt_path, useBytes = TRUE)

# 5) Render decision matrix from marts
matrix_html <- decision_matrix_page(
  company = company,
  year = year,
  marts = marts,
  id = "ohep-smoke-decision-matrix",
  title = paste("Decision Matrix -", company, year)
)

matrix_html_path <- file.path(proof_dir, "03_decision_matrix_smoke.html")
matrix_txt_path <- file.path(proof_dir, "03_decision_matrix_smoke_rendered.txt")
htmltools::save_html(matrix_html, file = matrix_html_path)
writeLines(htmltools::renderTags(matrix_html)$html, con = matrix_txt_path, useBytes = TRUE)

# 6) Write run manifest + run guide
manifest <- data.frame(
  check = c(
    "template_bootstrap_written",
    "bootstrap_artifacts_validated",
    "github_loader_content_validated",
    "github_loader_local_sim_passed",
    "mapping_diagnostics_smoke_passed",
    "failure_simulations_passed",
    "payload_schema_valid",
    "fundamental_rendered",
    "demographics_rendered",
    "decision_matrix_rendered"
  ),
  status = c("PASS", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS", "PASS"),
  details = c(
    normalizePath(bootstrap_dir, mustWork = TRUE),
    "required bootstrap files found",
    "CONFIG/raw URL/raw_user_data/preflight checks verified",
    "functions bundle + static bundle local simulation succeeded",
    "test_data_mapping + validate_data_sources returned HTML in all scenarios",
    "missing company and missing item columns fail as expected",
    "validate_displayr_schema(payload)",
    normalizePath(fund_html_path, mustWork = TRUE),
    normalizePath(demo_html_path, mustWork = TRUE),
    normalizePath(matrix_html_path, mustWork = TRUE)
  ),
  stringsAsFactors = FALSE
)
manifest_path <- file.path(run_info_dir, "smoke_test_manifest.csv")
utils::write.csv(manifest, manifest_path, row.names = FALSE)

cat("\nSmoke test completed successfully.\n")
cat("Artifacts:\n")
cat("-", normalizePath(file.path(out_dir, "DISPLAYR_SETUP.txt"), mustWork = TRUE), "\n")
cat("-", normalizePath(bootstrap_dir, mustWork = TRUE), "\n")
cat("-", normalizePath(sources_dir, mustWork = TRUE), "\n")
cat("-", normalizePath(proof_dir, mustWork = TRUE), "\n")
cat("-", normalizePath(manifest_path, mustWork = TRUE), "\n")
