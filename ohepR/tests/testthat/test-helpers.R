testthat::test_that("sentiment class mapping works", {
  testthat::expect_equal(get_sentiment_class(80), "bg-agree")
  testthat::expect_equal(get_sentiment_class(50), "bg-neutral")
  testthat::expect_equal(get_sentiment_class(20), "bg-disagree")
  testthat::expect_equal(get_sentiment_class(NA_real_), "bg-neutral")
})

testthat::test_that("delta pill mapping works", {
  testthat::expect_equal(get_delta_pill(3), "dp-pos")
  testthat::expect_equal(get_delta_pill(-1), "dp-neg")
  testthat::expect_equal(get_delta_pill(0), "dp-neu")
  testthat::expect_equal(get_delta_pill(NA_real_), "dp-neu")
})
