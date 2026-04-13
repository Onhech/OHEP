#' Render Item-Level Distribution Page
#'
#' Render an item-level sentiment distribution table with mean score and
#' benchmark deltas.
#'
#' @param item_distribution_data Named list with:
#'   - `summary`: one-row data frame with optional columns
#'     `title`, `subtitle`, `benchmark_label`, `prior_label`
#'   - `items`: data frame with columns `label`, `mean`, `disagree_pct`,
#'     `neutral_pct`, `agree_pct`, `vs_industry`, `vs_prior`.
#' @param id HTML id used to scope CSS for the rendered chart.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param ... Inline color overrides.
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export
item_distribution_page <- function(
  item_distribution_data = NULL,
  id = "ohep-item-distribution-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(item_distribution_data)) {
    item_distribution_data <- list(
      summary = data.frame(
        title = "Safety",
        subtitle = "Here goes the safety definition.",
        benchmark_label = "vs Industry",
        prior_label = "vs 2025",
        stringsAsFactors = FALSE
      ),
      items = data.frame(
        label = c(
          "I believe my workplace is safe.",
          "We have a strong safety culture at Surge.",
          "I feel empowered and engaged to improve safety.",
          "Leadership prioritizes health and safety.",
          "We have the right safety systems and programs."
        ),
        mean = c(4.82, 4.41, 4.12, 4.48, 4.25),
        disagree_pct = c(1, 1, 2, 1, 1),
        neutral_pct = c(3, 10, 23, 12, 19),
        agree_pct = c(96, 89, 75, 87, 80),
        vs_industry = c(4, 2, -12, 0, -5),
        vs_prior = c(2, -1, 5, -3, 0),
        stringsAsFactors = FALSE
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_item_distribution_page(
    item_distribution_data = item_distribution_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
