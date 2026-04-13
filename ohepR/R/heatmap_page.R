#' Render Internal Comparison Heatmap Page
#'
#' Render one or more heatmap tables with color-coded values relative to each
#' column's average.
#'
#' @param heatmap_data Named list with:
#'   - `title` optional chart title
#'   - `subtitle` optional chart subtitle
#'   - `legend_low` optional low-end legend label
#'   - `legend_high` optional high-end legend label
#'   - `tables` named list of data frames; each table's first column is the row
#'     category label and remaining columns are numeric comparison values.
#' @param id HTML id used to scope CSS for the rendered chart.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param ... Inline color overrides (for example,
#'   `heatmap_pill_pos_strong = "#7ED3A6"`).
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export
heatmap_page <- function(
  heatmap_data = NULL,
  id = "ohep-heatmap-page",
  color_overrides = NULL,
  ...
) {
  if (is.null(heatmap_data)) {
    heatmap_data <- list(
      title = "Internal Comparisons",
      subtitle = "Column-relative view across departments",
      legend_low = "Below Avg",
      legend_high = "Above Avg",
      tables = list(
        "Health Fundamentals" = data.frame(
          Category = c(
            "Purpose", "Strategy", "Communication", "Leadership",
            "Learning & Innovation", "Respect, Care & Trust",
            "Performance Development"
          ),
          `Corp Services` = c(4.38, 3.73, 4.30, 4.12, 4.05, 4.22, 4.02),
          Engineering = c(4.40, 3.91, 4.41, 4.29, 4.11, 4.13, 3.86),
          `Field Ops` = c(4.07, 3.70, 3.96, 3.91, 3.88, 4.03, NA),
          `Geo/Drill/Comp` = c(4.15, 3.47, 3.76, 3.59, 3.73, 4.04, 3.23),
          `Land & BD` = c(4.03, 3.28, 3.82, 3.60, 3.49, 3.71, 3.61),
          `Ops Accounting` = c(4.25, 3.67, 3.91, 4.11, 3.69, 4.02, 3.78),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        "OHEP Overall" = data.frame(
          Category = c("OHEP Score", "Engagement", "Burnout", "Work Satisfaction", "eNPS"),
          Corp = c(4.11, 4.09, 3.82, 4.02, 34),
          Eng = c(4.17, 4.16, 3.95, 4.09, 41),
          Field = c(3.95, 3.88, 3.62, 3.76, 19),
          Geo = c(3.76, 3.72, 3.41, 3.58, 6),
          Land = c(3.84, 3.81, 3.55, 3.69, 12),
          `Ops Acct` = c(4.03, 3.99, 3.73, 3.91, 26),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    )
  }

  runtime <- ohepRDisplayr()
  runtime$render_heatmap_page(
    heatmap_data = heatmap_data,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
