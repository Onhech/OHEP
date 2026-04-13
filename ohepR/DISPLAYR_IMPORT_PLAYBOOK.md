# Displayr Import Playbook (ohepR)

This playbook is the operational path for loading a new client refresh into Displayr while keeping static analytics assets stable.

## 1) Build the package and handoff files

From the package root (`ohepR/`):

```r
# install local package build (or devtools::load_all() while developing)
install.packages("ohepR_0.0.0.9000.tar.gz", repos = NULL, type = "source")
library(ohepR)

# Replace with your current client respondent-level data
raw_user_data <- read.csv("/path/to/current_user_data.csv", check.names = FALSE)

# Static index assets (item_data, user_data_key, predictive_data)
index_data <- load_example_index_data()

static_bundle <- build_displayr_static_bundle(index_data = index_data)

payload <- build_displayr_payload(
  raw_user_data = raw_user_data,
  static_bundle = static_bundle,
  snapshot_id = "clientname_2026q2"
)

validate_displayr_schema(payload)

write_displayr_handoff_files(
  payload,
  out_dir = "displayr_payload_handoff/clientname_2026q2"
)
```

Expected output folder:

- `01_raw_user_data.csv`
- `02_index_item_data.csv`
- `03_index_user_data_key.csv`
- `04_predictive_data.csv` (if present)
- `05_colors_table.csv`
- `marts/*.csv`
- `manifest.csv`
- `payload.rds`

## 2) Displayr import mapping

In Displayr, upload the CSVs as separate data sets/tables.

Recommended table names:

1. `raw_user_data` <- `01_raw_user_data.csv`
2. `index_item_data` <- `02_index_item_data.csv`
3. `index_user_data_key` <- `03_index_user_data_key.csv`
4. `predictive_data` <- `04_predictive_data.csv`
5. `colors_table` <- `05_colors_table.csv`
6. `snapshot_meta` <- `marts/01_snapshot_meta.csv`
7. `company_item_year` <- `marts/02_company_item_year.csv`
8. `company_fundamental_year` <- `marts/03_company_fundamental_year.csv`
9. `company_facet_year` <- `marts/04_company_facet_year.csv`
10. `benchmark_item_year` <- `marts/05_benchmark_item_year.csv`
11. `predictive_edges` <- `marts/06_predictive_edges.csv`
12. `metadata_lookup` <- `marts/07_metadata_lookup.csv`

## 3) Runtime object pattern inside Displayr

Use a Displayr R calculation/script to build a single object that all chart renderers read from.

```r
runtime <- ohepRDisplayr()

index_data <- list(
  item_data = index_item_data,
  user_data_key = index_user_data_key,
  predictive_data = predictive_data
)

snapshot <- prep_ohep_snapshot(
  raw_user_data = raw_user_data,
  index_data = index_data,
  predictive_data = predictive_data,
  snapshot_id = snapshot_meta$snapshot_id[[1]]
)
```

Then feed `snapshot` and `colors_table` to your page-level renderers.

## 4) Filtering behavior guidance

To preserve Displayr filtering/cut interactivity:

- Keep `raw_user_data` in Displayr as respondent-level data.
- Recompute filtered marts in Displayr (do not only inject pre-rendered HTML).
- Render HTML from filtered marts in the same project/session.

## 5) Multi-client repeat process

For each new client wave:

1. Keep static `index_data` + `colors_table` stable unless benchmarks/palette changed.
2. Replace only `raw_user_data` (and optionally `predictive_data` if model updates).
3. Regenerate payload and re-import CSVs using the same table names.
4. If input column names remain consistent, mappings remain reusable.

## 6) Troubleshooting

- **Schema errors**: run `validate_displayr_schema(payload)` before export.
- **Mismatched labels**: confirm `user_data_key$item` aligns with user-data columns.
- **Color mismatches**: confirm active token rows are in `05_colors_table.csv`.
- **Benchmark oddities**: verify `company` and `year` values are present and typed consistently.
