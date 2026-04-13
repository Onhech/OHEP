# OHEP Preview Lab

This folder is the centralized local sandbox for HTML rendering and privacy behavior checks.

## Primary entrypoint

Run one file:

```r
source("preview/run_preview_lab.R")
run_preview_lab()
run_decision_matrix_preview()
run_item_distribution_preview()
run_model_preview()
run_demographics_preview()
```

Or from shell:

```bash
Rscript preview/run_preview_lab.R
```

Outputs are written to:

- Fundamentals:
  - `preview/test_examples/Fundamentals/index.html`
  - `preview/test_examples/Fundamentals/scenario_summary.csv`
  - `preview/test_examples/Fundamentals/scenario_results.rds`
  - one `*.html` and `*_rendered.txt` per scenario
- Decision Matrix:
  - `preview/test_examples/Decision Matrix/index.html`
  - `preview/test_examples/Decision Matrix/01_decision_matrix_change_vs_last_year_by_impact_company_2_2026.html`
  - `preview/test_examples/Decision Matrix/02_decision_matrix_benchmark_by_change_vs_last_year_company_2_2026.html`
  - `preview/test_examples/Decision Matrix/03_decision_matrix_impact_by_opportunity_company_2_2026.html`
  - `preview/test_examples/Decision Matrix/04_decision_matrix_relative_benchmark_by_impact_company_2_2026.html`
  - rendered/text artifacts alongside each HTML
  - `preview/test_examples/Decision Matrix/decision_matrix_options.csv`
- Item Distribution:
  - `preview/test_examples/Item Distribution/index.html`
  - `preview/test_examples/Item Distribution/01_item_distribution.html`
  - `preview/test_examples/Item Distribution/01_item_distribution_data.csv`
  - `preview/test_examples/Item Distribution/01_item_distribution_rendered.txt`
- Model Page:
  - `preview/test_examples/Model Page/index.html`
  - `preview/test_examples/Model Page/01_model_page.html`
  - `preview/test_examples/Model Page/01_model_page_data.csv`
  - `preview/test_examples/Model Page/01_model_page_rendered.txt`
- Demographics:
  - `preview/test_examples/Demographics/index.html`
  - `preview/test_examples/Demographics/01_demographics_page.html`
  - `preview/test_examples/Demographics/01_demographics_page_data.csv`
  - `preview/test_examples/Demographics/01_demographics_page_rendered.txt`

## Why this exists

This models the Displayr workflow with simple function calls that generate:

- render-ready HTML
- scenario summary diagnostics
- reusable objects in `scenario_results.rds` for inspection

## Key levers you can tune

In `run_preview_lab(...)`:

- `fundamental` to target a different fundamental page
- `company`, `year` to pin the scenario context
- `min_n` to test privacy thresholds
- `privacy_message` to test copy and UX
- `clean` to preserve or reset previous outputs

In `run_decision_matrix_preview(...)`:

- `company` / `year` to set matrix context
- `clean` to preserve or reset previous outputs
- the preview emits four axis/split variants for side-by-side comparison

## Scenario set included

The lab currently generates scenarios for:

- unfiltered marts baseline
- filtered respondent-level baseline
- page suppression (`n < min_n`)
- row-level suppression (`item_n < min_n`)
- comparison-only suppression (`prior_n < min_n`)
- shifted data (to review visual sensitivity)
- stricter threshold (`min_n = 5`)

You can add or edit scenarios in `build_scenarios()` inside `preview/run_preview_lab.R`.
