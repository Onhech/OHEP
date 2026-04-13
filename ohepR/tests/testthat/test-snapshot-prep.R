testthat::test_that("prep_ohep_snapshot builds required marts and key columns", {
  snapshot <- example_ohep_snapshot()

  required_tables <- c(
    "snapshot_meta",
    "company_item_year",
    "company_fundamental_year",
    "company_facet_year",
    "benchmark_item_year",
    "predictive_edges",
    "metadata_lookup"
  )
  testthat::expect_true(all(required_tables %in% names(snapshot)))

  testthat::expect_true(all(c(
    "company", "year", "item_id", "mean", "n",
    "agree_pct", "neutral_pct", "disagree_pct",
    "benchmark_mean", "benchmark_sd", "z_score", "percentile",
    "vs_benchmark_raw", "vs_benchmark_pctile",
    "vs_prior_raw", "vs_prior_pctile", "vs_company_avg_raw"
  ) %in% names(snapshot$company_item_year)))

  testthat::expect_true(all(c(
    "company", "year", "fundamental_id", "mean", "n",
    "z_score", "percentile",
    "vs_benchmark_raw", "vs_benchmark_pctile",
    "vs_prior_raw", "vs_prior_pctile", "status_label"
  ) %in% names(snapshot$company_fundamental_year)))
})

testthat::test_that("prep_ohep_snapshot applies reverse scoring and nearest prior deltas", {
  raw <- data.frame(
    company = c("A", "A", "A", "A", "B", "B"),
    year = c(2023, 2023, 2025, 2025, 2025, 2025),
    purpose_1 = c(1, 2, 4, 5, 4, 4),
    purpose_2 = c(5, 4, 2, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  index_data <- list(
    item_data = data.frame(
      item_id = c("purpose_1", "purpose_2"),
      item_text = c("P1", "P2"),
      fundamental_id = c("Purpose", "Purpose"),
      facet_id = c("Purpose", "Purpose"),
      is_reverse_scored = c(FALSE, TRUE),
      response_scale_min = c(1, 1),
      response_scale_max = c(5, 5),
      stringsAsFactors = FALSE
    ),
    user_data_key = data.frame(item = c("purpose_1", "purpose_2"), Description = c("P1", "P2"), data_domain = "scaled_item")
  )

  snapshot <- prep_ohep_snapshot(raw_user_data = raw, index_data = index_data, snapshot_id = "unit")
  ciy <- snapshot$company_item_year

  p2_2023 <- ciy[ciy$company == "A" & ciy$year == 2023 & ciy$item_id == "purpose_2", , drop = FALSE]
  testthat::expect_equal(round(p2_2023$mean, 6), 1.5)
  testthat::expect_true(p2_2023$disagree_pct >= 50)

  p1_2025 <- ciy[ciy$company == "A" & ciy$year == 2025 & ciy$item_id == "purpose_1", , drop = FALSE]
  testthat::expect_equal(round(p1_2025$vs_prior_raw, 6), 3)
  testthat::expect_true(is.finite(p1_2025$percentile))
})

testthat::test_that("snapshot write/load roundtrip works for rds and csv", {
  snapshot <- example_ohep_snapshot()

  rds_dir <- tempfile("ohep_snapshot_rds_")
  dir.create(rds_dir)
  write_ohep_snapshot(snapshot, rds_dir, format = "rds")
  loaded_rds <- load_ohep_snapshot(rds_dir)
  testthat::expect_true(is.list(loaded_rds))
  testthat::expect_true("company_item_year" %in% names(loaded_rds))

  csv_dir <- tempfile("ohep_snapshot_csv_")
  dir.create(csv_dir)
  write_ohep_snapshot(snapshot, csv_dir, format = "csv")
  loaded_csv <- load_ohep_snapshot(csv_dir)
  testthat::expect_true(is.list(loaded_csv))
  testthat::expect_true("company_fundamental_year" %in% names(loaded_csv))
})

testthat::test_that("fundamental_page renders from marts", {
  snapshot <- example_ohep_snapshot()
  cfy <- snapshot$company_fundamental_year
  purpose_rows <- cfy[cfy$fundamental_id == "Purpose", , drop = FALSE]

  testthat::skip_if(nrow(purpose_rows) < 1L, "No Purpose rows available in example snapshot.")
  row <- purpose_rows[1, , drop = FALSE]

  out <- fundamental_page(
    company = row$company[[1]],
    year = as.integer(row$year[[1]]),
    fundamental = "Purpose",
    marts = snapshot,
    id = "from-marts"
  )
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "from-marts")
})

testthat::test_that("fundamental_page supports respondent-level filtered_user_data cuts", {
  ex <- example_fundamental_inputs()
  snapshot <- example_ohep_snapshot()
  cfy <- snapshot$company_fundamental_year
  purpose_rows <- cfy[cfy$fundamental_id == "Purpose", , drop = FALSE]
  testthat::skip_if(nrow(purpose_rows) < 1L, "No Purpose rows available in example snapshot.")
  row <- purpose_rows[1, , drop = FALSE]

  company_val <- row$company[[1]]
  year_val <- as.integer(row$year[[1]])

  filtered_user_data <- ex$user_data$user_data
  filtered_user_data <- filtered_user_data[
    as.character(filtered_user_data$company) == as.character(company_val) &
      suppressWarnings(as.integer(filtered_user_data$year)) == year_val,
    ,
    drop = FALSE
  ]

  out <- fundamental_page(
    company = company_val,
    year = year_val,
    fundamental = "Purpose",
    marts = snapshot,
    filtered_user_data = filtered_user_data,
    id = "from-filtered-cut"
  )

  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "from-filtered-cut")
})

testthat::test_that("fundamental_page applies page-level privacy suppression when n < min_n", {
  ex <- example_fundamental_inputs()
  snapshot <- example_ohep_snapshot()
  cfy <- snapshot$company_fundamental_year
  purpose_rows <- cfy[cfy$fundamental_id == "Purpose", , drop = FALSE]
  testthat::skip_if(nrow(purpose_rows) < 1L, "No Purpose rows available in example snapshot.")
  row <- purpose_rows[1, , drop = FALSE]

  company_val <- row$company[[1]]
  year_val <- as.integer(row$year[[1]])
  cut <- ex$user_data$user_data[
    as.character(ex$user_data$user_data$company) == as.character(company_val) &
      suppressWarnings(as.integer(ex$user_data$user_data$year)) == year_val,
    ,
    drop = FALSE
  ]
  testthat::skip_if(nrow(cut) < 2L, "Need at least 2 rows for suppression test.")
  cut <- cut[seq_len(min(2L, nrow(cut))), , drop = FALSE]

  out <- fundamental_page(
    company = company_val,
    year = year_val,
    fundamental = "Purpose",
    marts = snapshot,
    filtered_user_data = cut,
    min_n = 3,
    id = "privacy-page"
  )
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "privacy-page-overlay")
  testthat::expect_match(html, "Insufficient Responses")
})

testthat::test_that("fundamental_page applies row-level privacy suppression", {
  ex <- example_fundamental_inputs()
  snapshot <- example_ohep_snapshot()
  cfy <- snapshot$company_fundamental_year
  purpose_rows <- cfy[cfy$fundamental_id == "Purpose", , drop = FALSE]
  testthat::skip_if(nrow(purpose_rows) < 1L, "No Purpose rows available in example snapshot.")
  row <- purpose_rows[1, , drop = FALSE]

  company_val <- row$company[[1]]
  year_val <- as.integer(row$year[[1]])
  cut <- ex$user_data$user_data[
    as.character(ex$user_data$user_data$company) == as.character(company_val) &
      suppressWarnings(as.integer(ex$user_data$user_data$year)) == year_val,
    ,
    drop = FALSE
  ]
  testthat::skip_if(nrow(cut) < 5L, "Need at least 5 rows for row suppression test.")
  cut <- cut[seq_len(5L), , drop = FALSE]
  cut$purpose_1 <- NA
  cut$purpose_1[1:2] <- c(4, 5)

  out <- fundamental_page(
    company = company_val,
    year = year_val,
    fundamental = "Purpose",
    marts = snapshot,
    filtered_user_data = cut,
    min_n = 3,
    id = "privacy-row"
  )
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "privacy-row")
  testthat::expect_match(html, "Insufficient responses")
})

testthat::test_that("fundamental_page applies field-level comparison suppression", {
  snapshot <- example_ohep_snapshot()
  purpose_items <- snapshot$metadata_lookup$item_id[snapshot$metadata_lookup$fundamental_id == "Purpose"]
  purpose_items <- purpose_items[grepl("^purpose_", purpose_items)]
  testthat::skip_if(length(purpose_items) < 2L, "Need Purpose item columns.")

  synth <- data.frame(
    company = c(rep("SYN_COMPANY", 4), rep("SYN_COMPANY", 2)),
    year = c(rep(2025, 4), rep(2024, 2)),
    stringsAsFactors = FALSE
  )
  for (nm in unique(purpose_items)) {
    synth[[nm]] <- c(4, 4, 5, 3, 4, 2)
  }

  out <- fundamental_page(
    company = "SYN_COMPANY",
    year = 2025,
    fundamental = "Purpose",
    marts = snapshot,
    filtered_user_data = synth,
    min_n = 3,
    id = "privacy-compare"
  )
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "privacy-compare")
  testthat::expect_match(html, "title=\"Insufficient responses\"")
})

testthat::test_that("filtered dashboard data carries company benchmark deltas", {
  ex <- example_fundamental_inputs()
  snapshot <- example_ohep_snapshot()
  cfy <- snapshot$company_fundamental_year
  purpose_rows <- cfy[cfy$fundamental_id == "Purpose", , drop = FALSE]
  testthat::skip_if(nrow(purpose_rows) < 1L, "No Purpose rows available in example snapshot.")
  row <- purpose_rows[1, , drop = FALSE]

  company_val <- row$company[[1]]
  year_val <- as.integer(row$year[[1]])
  filtered_user_data <- ex$user_data$user_data[
    as.character(ex$user_data$user_data$company) == as.character(company_val) &
      suppressWarnings(as.integer(ex$user_data$user_data$year)) == year_val,
    ,
    drop = FALSE
  ]

  dashboard <- build_dashboard_data_from_filtered_user_data(
    company = company_val,
    year = year_val,
    fundamental = "Purpose",
    marts = snapshot,
    filtered_user_data = filtered_user_data
  )
  testthat::expect_true("vs_company" %in% names(dashboard$items))
  testthat::expect_true("suppress_vs_company" %in% names(dashboard$items))
})

testthat::test_that("benchmark toggles hide/show comparison columns", {
  data <- sample_dashboard_data()

  html_none <- htmltools::renderTags(
    render_fundamental_page(
      data,
      id = "bench-none",
      benchmark_company = FALSE,
      benchmark_index = FALSE,
      benchmark_prior = FALSE
    )
  )$html
  testthat::expect_no_match(html_none, "vs Ind\\.")
  testthat::expect_no_match(html_none, "vs '24")

  html_index_only <- htmltools::renderTags(
    render_fundamental_page(
      data,
      id = "bench-index-only",
      benchmark_company = FALSE,
      benchmark_index = TRUE,
      benchmark_prior = FALSE
    )
  )$html
  testthat::expect_match(html_index_only, "vs Ind\\.")
  testthat::expect_no_match(html_index_only, "vs '24")
})

testthat::test_that("company benchmark column is rendered for filtered cuts when available", {
  ex <- example_fundamental_inputs()
  snapshot <- example_ohep_snapshot()
  cfy <- snapshot$company_fundamental_year
  purpose_rows <- cfy[cfy$fundamental_id == "Purpose", , drop = FALSE]
  testthat::skip_if(nrow(purpose_rows) < 1L, "No Purpose rows available in example snapshot.")
  row <- purpose_rows[1, , drop = FALSE]

  company_val <- row$company[[1]]
  year_val <- as.integer(row$year[[1]])
  filtered_user_data <- ex$user_data$user_data[
    as.character(ex$user_data$user_data$company) == as.character(company_val) &
      suppressWarnings(as.integer(ex$user_data$user_data$year)) == year_val,
    ,
    drop = FALSE
  ]

  out <- fundamental_page(
    company = company_val,
    year = year_val,
    fundamental = "Purpose",
    marts = snapshot,
    filtered_user_data = filtered_user_data,
    benchmark_company = TRUE,
    benchmark_index = FALSE,
    benchmark_prior = FALSE,
    id = "bench-company-only"
  )
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "vs Co\\.")
  testthat::expect_no_match(html, "vs Ind\\.")
  testthat::expect_no_match(html, "vs '24")
})
