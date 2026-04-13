#' Get Sentiment Class
#'
#' Convenience helper that maps percentage values to sentiment classes used by
#' the renderer.
#'
#' @param score Numeric percentage score.
#'
#' @return One of `bg-agree`, `bg-neutral`, `bg-disagree`.
#' @export
get_sentiment_class <- function(score) {
  runtime <- ohepRDisplayr()
  runtime$get_sentiment_class(score)
}

#' Get Delta Pill Class
#'
#' Convenience helper that maps a delta value to pill classes.
#'
#' @param delta_value Numeric change value.
#'
#' @return One of `dp-pos`, `dp-neg`, `dp-neu`.
#' @export
get_delta_pill <- function(delta_value) {
  runtime <- ohepRDisplayr()
  runtime$get_delta_pill(delta_value)
}
