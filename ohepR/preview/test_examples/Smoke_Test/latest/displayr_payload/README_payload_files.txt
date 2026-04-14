Displayr Payload Folder

Use this folder directly in Displayr.

Primary script:
- displayr_bootstrap_from_github_rds.R
  Loads ohepR functions + static bundle from GitHub-hosted RDS files.
  Edit CONFIG URLs and one line: raw_user_data <- <Displayr respondent table reference>.

Fallback script:
- displayr_bootstrap_single_paste.R
  Use only when GitHub RDS loading is unavailable.

Fallback CSV files:
- 01_index_item_data.csv
- 02_index_user_data_key.csv
- 03_predictive_data.csv
- 04_colors_table.csv
