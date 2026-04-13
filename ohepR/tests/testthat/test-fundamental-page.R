testthat::test_that("fundamental_page supports unquoted and character fundamental names", {
  ex <- example_fundamental_inputs()

  out_symbol <- fundamental_page(Purpose, ex$index_data, ex$user_data, id = "fundamental-symbol")
  out_char <- fundamental_page("Purpose", ex$index_data, ex$user_data, id = "fundamental-char")

  html_symbol <- htmltools::renderTags(out_symbol)$html
  html_char <- htmltools::renderTags(out_char)$html

  testthat::expect_match(html_symbol, "id=\\\"fundamental-symbol\\\"")
  testthat::expect_match(html_char, "id=\\\"fundamental-char\\\"")
})

testthat::test_that("fundamental_page errors when selected fundamental is missing", {
  ex <- example_fundamental_inputs()
  testthat::expect_error(
    fundamental_page("Nonexistent", ex$index_data, ex$user_data),
    "Expected exactly one row"
  )
})

testthat::test_that("CSV-backed static index data can drive rendering", {
  index_data <- load_example_index_data()
  ex <- example_fundamental_inputs()
  out <- fundamental_page("Purpose", index_data, ex$user_data, id = "from-csv-index")
  html <- htmltools::renderTags(out)$html
  testthat::expect_match(html, "from-csv-index")
})
