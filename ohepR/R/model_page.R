#' Render Model Representation Page
#'
#' Render the model representation chart with percentile tracks for
#' fundamentals and outcomes.
#'
#' @param model_data Named list with:
#'   - `summary`: one-row data frame with optional labels:
#'     `title`, `subtitle`, `fundamentals_label`, `outcomes_label`,
#'     `raw_avg_label`, `delta_label`
#'   - `fundamentals`: data frame with required columns `label`, `percentile`
#'     and optional `prior_percentile`, `raw_avg`, `delta`, `shape`
#'   - `outcomes`: data frame with required columns `label`, `percentile`
#'     and optional `prior_percentile`, `raw_avg`, `delta`, `shape`
#' @param id HTML id used to scope CSS for the rendered chart.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param ... Inline color overrides.
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export
model_page <- function(
  model_data = NULL,
  id = "ohep-model-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(model_data)) {
    model_data <- list(
      summary = data.frame(
        title = "Health Diagnostics",
        subtitle = "",
        fundamentals_label = "",
        outcomes_label = "Business Outcomes",
        raw_avg_label = "Raw Avg",
        delta_label = "vs '24",
        stringsAsFactors = FALSE
      ),
      fundamentals = data.frame(
        label = c(
          "Purpose",
          "Strategy",
          "Communication",
          "Leadership",
          "Learning & Innovation",
          "Respect, Care & Trust",
          "Performance Development"
        ),
        percentile = c(72, 45, 69, 78, 82, 60, 74),
        prior_percentile = c(60, 32, 58, 71, 76, 55, 66),
        raw_avg = c(4.12, 3.83, 4.03, 4.18, 4.26, 3.99, 4.08),
        delta = c(0.14, -0.08, 0.10, 0.07, 0.06, 0.03, 0.11),
        shape = "circle",
        stringsAsFactors = FALSE
      ),
      outcomes = data.frame(
        label = c(
          "Overall OHEP Score",
          "Employee Engagement",
          "Burnout",
          "Work Satisfaction",
          "eNPS"
        ),
        percentile = c(64, 68, 38, 70, 76),
        prior_percentile = c(58, 63, 41, 65, 72),
        shape = "diamond",
        stringsAsFactors = FALSE
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_model_page(
    model_data = model_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
