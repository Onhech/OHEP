#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x

now_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
args <- commandArgs(trailingOnly = TRUE)
out_dir_arg <- if (length(args) >= 1L) args[[1]] else file.path("preview", "test_examples", paste0("smoke_test_", now_stamp))

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
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
proof_dir <- file.path(out_dir, "render_validation")
dir.create(proof_dir, recursive = TRUE, showWarnings = FALSE)

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
payload_dir <- file.path(out_dir, "displayr_payload")
support_dir <- file.path(out_dir, "displayr_support")

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

# 6) Write run manifest
manifest <- data.frame(
  check = c(
    "template_bootstrap_written",
    "payload_schema_valid",
    "fundamental_rendered",
    "demographics_rendered",
    "decision_matrix_rendered"
  ),
  status = c("PASS", "PASS", "PASS", "PASS", "PASS"),
  details = c(
    normalizePath(payload_dir, mustWork = TRUE),
    "validate_displayr_schema(payload)",
    normalizePath(fund_html_path, mustWork = TRUE),
    normalizePath(demo_html_path, mustWork = TRUE),
    normalizePath(matrix_html_path, mustWork = TRUE)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(out_dir, "smoke_test_manifest.csv"), row.names = FALSE)

cat("\nSmoke test completed successfully.\n")
cat("Artifacts:\n")
cat("-", normalizePath(file.path(out_dir, "smoke_test_manifest.csv"), mustWork = TRUE), "\n")
cat("-", normalizePath(payload_dir, mustWork = TRUE), "\n")
cat("-", normalizePath(support_dir, mustWork = TRUE), "\n")
cat("-", normalizePath(proof_dir, mustWork = TRUE), "\n")
