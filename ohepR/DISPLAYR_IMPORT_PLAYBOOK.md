# Displayr Template Bootstrap Playbook (ohepR)

Use one Displayr template with static tables uploaded once, then reuse it for nested/client reports.

## Bootstrap Once (Template Setup)

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
  out_dir = "displayr_template_bootstrap"
)
```

This writes:

- `displayr_payload/` (files to upload to Displayr)
- `displayr_support/README_displayr_setup.txt`
- `displayr_support/manifest.csv`
- `displayr_support/template_static_bundle.rds` (optional)

## In Displayr (Template Document)

1. Upload files from `displayr_payload/`.
2. Name the data sets exactly as expected in `displayr_support/README_displayr_setup.txt`.
3. Paste `displayr_payload/displayr_bootstrap_single_paste.R` into one R Output.
4. Edit only the `DATASET_MAP` block.
5. Build chart outputs from `snapshot` and `index_data`.

## New Report Flow

1. Duplicate the Displayr template document.
2. Connect or upload the report's live respondent table as `raw_user_data`.
3. Re-run outputs.

No per-wave marts export is required.

## Annual Maintenance

1. Refresh static benchmark files (`index_item_data`, `index_user_data_key`, `predictive_data`) as needed.
2. Re-run `write_displayr_template_bootstrap()`.
3. Update the Displayr template's static tables once.
