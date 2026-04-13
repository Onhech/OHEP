testthat::test_that("renderer returns htmltools output", {
  out <- render_fundamental_page(sample_dashboard_data(), id = "ohep-custom-dashboard")
  testthat::expect_true(inherits(out, "shiny.tag.list"))
})

testthat::test_that("renderer output is snapshot stable", {
  out <- render_fundamental_page(sample_dashboard_data(), id = "ohep-custom-dashboard")
  rendered <- htmltools::renderTags(out)$html
  testthat::expect_match(rendered, "id=\\\"ohep-custom-dashboard\\\"")
  testthat::expect_snapshot_output(cat(rendered))
})
