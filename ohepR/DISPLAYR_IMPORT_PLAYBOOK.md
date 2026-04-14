# Displayr Bootstrap Playbook (RDS Workflow Only)

This package now uses one primary setup flow: **GitHub-hosted RDS sources + one Displayr bootstrap script**.

## 1) Generate the bundle

```r
library(ohepR)

inputs <- example_fundamental_inputs()
static_bundle <- build_displayr_static_bundle(
  index_data = inputs$index_data,
  predictive_data = inputs$index_data$predictive_data
)

write_displayr_template_bootstrap(
  static_bundle = static_bundle,
  out_dir = "displayr_bootstrap"
)
```

Generated structure:

- `DISPLAYR_SETUP.txt` (single setup instructions file)
- `bootstrap/displayr_bootstrap_from_github_rds.R` (paste this into Displayr)
- `sources/template_functions_bundle.rds`
- `sources/template_static_bundle.rds`
- `sources/function_reference_appendix.txt`
- `sources/manifest.csv`

## 2) Push bundle to GitHub

Commit and push the generated folder so files in `sources/` are reachable by raw GitHub URL.

## 3) In Displayr

1. Paste `bootstrap/displayr_bootstrap_from_github_rds.R` into one R Output.
2. Edit one line only:

```r
raw_user_data <- <Displayr respondent table reference>
```

3. Run the script.

It creates `snapshot`, `index_data`, and `colors_table`.

## 4) Render charts

Use page functions with `marts = snapshot`, for example:

```r
company <- as.character(snapshot$company_fundamental_year$company[[1]])
year <- as.integer(snapshot$company_fundamental_year$year[[1]])
fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])

fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)
```

## Notes

- Input must be respondent-level data.
- Required schema checks are enforced in the bootstrap script.
- Legacy CSV/single-paste assets are optional (`include_fallback = TRUE`) and not part of normal usage.
