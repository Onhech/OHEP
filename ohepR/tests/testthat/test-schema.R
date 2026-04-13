testthat::test_that("schema validation accepts canonical fixture", {
  data <- sample_dashboard_data()
  testthat::expect_silent(validate_fundamental_page_data(data))
})

testthat::test_that("schema validation catches missing section", {
  data <- sample_dashboard_data()
  data$items <- NULL
  testthat::expect_error(validate_fundamental_page_data(data), "Missing section")
})

testthat::test_that("schema validation catches invalid sentiment sums", {
  data <- sample_dashboard_data()
  data$items$agree_pct[1] <- 20
  testthat::expect_error(validate_fundamental_page_data(data), "sum to 100")
})

testthat::test_that("schema validation catches too many rows in a column", {
  data <- sample_dashboard_data()
  data$items <- rbind(
    data$items,
    transform(data$items[1, , drop = FALSE], item_order = 4),
    transform(data$items[1, , drop = FALSE], item_order = 5)
  )
  testthat::expect_error(validate_fundamental_page_data(data), "at most 4 rows")
})
