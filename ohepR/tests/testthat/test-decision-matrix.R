testthat::test_that("decision_matrix_page returns htmltools output", {
  pts <- data.frame(
    fundamental = c("Purpose", "Learning", "Leadership"),
    score = c(4.2, 3.1, 3.8),
    impact = c(0.42, 0.65, 0.58),
    stringsAsFactors = FALSE
  )
  out <- decision_matrix_page(pts, id = "dm-basic")
  testthat::expect_true(inherits(out, "shiny.tag.list"))
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "id=\\\"dm-basic\\\"")
  testthat::expect_match(html, "PERFORMANCE VS\\. BENCHMARK")
  testthat::expect_match(html, "IMPACT ON OUTCOMES")
  testthat::expect_false(grepl("<p class=\\\"subtitle\\\">", html))
})

testthat::test_that("decision_matrix_page supports color overrides", {
  pts <- data.frame(
    fundamental = c("Purpose", "Learning"),
    score = c(4.5, 3.0),
    impact = c(0.8, 0.4),
    stringsAsFactors = FALSE
  )
  out <- decision_matrix_page(
    pts,
    id = "dm-colors",
    matrix_dot_critical = "#123456"
  )
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "--matrix-dot-critical: #123456;")
})

testthat::test_that("decision_matrix_page supports marts metric selection", {
  ex <- example_fundamental_inputs()
  marts <- prep_ohep_snapshot(
    raw_user_data = ex$user_data$user_data,
    index_data = ex$index_data,
    predictive_data = ex$index_data$predictive_data,
    snapshot_id = "dm-test"
  )

  out <- decision_matrix_page(
    company = 2,
    year = 2026,
    marts = marts,
    x_metric = "prior_delta",
    y_metric = "impact",
    x_split_mode = "zero",
    y_split_mode = "median",
    id = "dm-marts"
  )

  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "CHANGE VS\\. LAST YEAR")
  testthat::expect_match(html, "IMPACT ON OUTCOMES")
  testthat::expect_match(html, "DECLINING")
  testthat::expect_match(html, "IMPROVING")
})
