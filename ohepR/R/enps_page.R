#' Render eNPS Page
#'
#' Render an eNPS card using the same branded visual system as other OHEP pages.
#'
#' @param enps_data Named list with:
#'   - `summary`: one-row data frame with optional columns
#'     `title`, `subtitle`, `score`, `score_delta`, `delta_label`
#'   - `distribution`: data frame with columns `rating` (0-10) and `pct`
#'     (percentage share per rating; should sum to ~100).
#' @param id HTML id used to scope CSS for the rendered chart.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param ... Inline color overrides (for example, `enps_tug_promoter_bg = "#005f56"`).
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export
enps_page <- function(
  enps_data = NULL,
  id = "ohep-enps-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(enps_data)) {
    enps_data <- list(
      summary = data.frame(
        title = "Employee Net Promoter Score (eNPS)",
        subtitle = "Calculated difference between promoters and detractors.",
        score = 34,
        score_delta = 12,
        delta_label = "vs Industry",
        stringsAsFactors = FALSE
      ),
      distribution = data.frame(
        rating = 0:10,
        pct = c(1, 1, 1, 2, 3, 3, 3, 18, 20, 22, 26),
        stringsAsFactors = FALSE
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_enps_page(
    enps_data = enps_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
