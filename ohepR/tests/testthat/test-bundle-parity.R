testthat::test_that("Displayr bundle factory matches package renderer output", {
  script_path <- testthat::test_path("../../tools/build_displayr_bundle.R")
  root <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)
  skip_if_not(file.exists(script_path))

  source(script_path, local = TRUE)
  artifacts <- build_displayr_bundle(root = root, out_dir = tempdir())

  bundle_factory <- readRDS(artifacts$latest)
  runtime <- bundle_factory()

  pkg_html <- htmltools::renderTags(
    render_fundamental_page(sample_dashboard_data(), id = "bundle-check")
  )$html
  bundle_html <- htmltools::renderTags(
    runtime$render_fundamental_page(sample_dashboard_data(), id = "bundle-check")
  )$html

  testthat::expect_identical(bundle_html, pkg_html)
})
