# Displayr Template Bootstrap Playbook (ohepR)

Use one primary flow: load ohepR runtime functions and static reference data from GitHub-hosted RDS files inside Displayr. Use CSV upload + single-paste script only as fallback/debug.

## 1) Build the Bootstrap Package

Run in `ohepR/`:

```r
library(ohepR)

inputs <- example_fundamental_inputs()

static_bundle <- build_displayr_static_bundle(
  index_data = inputs$index_data,
  predictive_data = inputs$index_data$predictive_data
)

write_displayr_template_bootstrap(
  static_bundle = static_bundle,
  out_dir = "displayr_payload_export"
)
```

This writes:

- `displayr_payload/displayr_bootstrap_from_github_rds.R` (primary Displayr script)
- `displayr_payload/displayr_bootstrap_single_paste.R` (fallback script)
- `displayr_payload/*.csv` (fallback static tables)
- `displayr_support/template_functions_bundle.rds`
- `displayr_support/template_static_bundle.rds`
- `displayr_support/README_displayr_setup.txt`
- `displayr_support/function_reference_appendix.txt`
- `displayr_support/manifest.csv`

## 2) Primary Displayr Setup (GitHub RDS)

1. Commit/push the generated `displayr_support/*.rds` files to GitHub.
2. In Displayr, add one R Output and paste `displayr_payload/displayr_bootstrap_from_github_rds.R`.
3. Edit only:
   - `CONFIG$functions_rds_url` (raw GitHub URL pinned to commit SHA)
   - `CONFIG$static_bundle_rds_url` (raw GitHub URL pinned to commit SHA)
   - `raw_user_data <- <Displayr respondent table reference>`
4. Run once; this creates `index_data`, `colors_table`, and `snapshot`.

Notes:

- `raw_user_data` must be respondent-level and include `company`, `year`, plus required item columns.
- Aggregate-only input is not supported.

## 3) Render Outputs in Displayr

Use page functions with `marts = snapshot`, for example:

```r
company <- as.character(snapshot$company_fundamental_year$company[[1]])
year <- as.integer(snapshot$company_fundamental_year$year[[1]])
fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])

fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)
```

All page functions return HTML tag objects/snippets suitable for Displayr rendering.

## 4) Fallback Path (Only if Needed)

If GitHub RDS loading is blocked in your environment:

1. Upload CSVs from `displayr_payload/`.
2. Paste `displayr_payload/displayr_bootstrap_single_paste.R`.
3. Edit `DATASET_MAP` and run.

Treat this as fallback/debug, not the default workflow.
