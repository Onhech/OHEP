Displayr Template Bootstrap Setup

1) Upload these files from `displayr_payload/` into your Displayr template:
   - 01_index_item_data.csv
   - 02_index_user_data_key.csv
   - 03_predictive_data.csv (if exported)
   - 04_colors_table.csv

2) Name Displayr data sets exactly as: index_item_data, index_user_data_key, predictive_data, colors_table, raw_user_data

3) Paste displayr_bootstrap_single_paste.R into one R Output in Displayr.
4) Edit only DATASET_MAP in that script.
5) Use chart functions with marts = snapshot (e.g., fundamental_page(..., marts = snapshot)).
6) See function_reference_appendix.txt for syntax and usage of all exported functions.
