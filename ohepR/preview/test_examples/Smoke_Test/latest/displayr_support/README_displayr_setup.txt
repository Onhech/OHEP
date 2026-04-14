Displayr Template Bootstrap Setup (Primary: GitHub RDS)

Primary path:
1) Paste `displayr_payload/displayr_bootstrap_from_github_rds.R` into one Displayr R Output.
2) Set `CONFIG$functions_rds_url` and `CONFIG$static_bundle_rds_url` to pinned commit-SHA raw URLs.
3) Set one line: `raw_user_data <- <Displayr respondent table reference>`.
4) Run script. It creates `index_data`, `colors_table`, and `snapshot`.

Fallback path (CSV + local single paste):
5) Upload static CSVs from `displayr_payload/` and paste `displayr_payload/displayr_bootstrap_single_paste.R`.
6) In fallback mode, map names as: index_item_data, index_user_data_key, predictive_data, colors_table, raw_user_data

7) Use chart functions with marts = snapshot (e.g., fundamental_page(..., marts = snapshot)).
8) Aggregate-only user data is not supported; respondent-level item columns are required.
9) See function_reference_appendix.txt for syntax and usage of all exported functions.
