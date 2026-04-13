testthat::test_that("item row component renders expected structure", {
  runtime <- ohepRDisplayr()
  data <- sample_dashboard_data()
  row_html <- runtime$build_item_row(data$items[1, , drop = FALSE])
  testthat::expect_snapshot_output(cat(row_html))
})

testthat::test_that("hero card component renders expected structure", {
  runtime <- ohepRDisplayr()
  data <- sample_dashboard_data()
  hero_html <- runtime$build_hero_card(data$fundamental)
  testthat::expect_snapshot_output(cat(hero_html))
})
