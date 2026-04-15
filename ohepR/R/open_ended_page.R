#' Render Open-Ended Comments Page
#'
#' Composes qualitative/open-ended report pages from precomputed tables.
#'
#' @param open_ended_data Named list with `summary`, `takeaways`,
#'   `theme_evidence`, `quotes`, `verbatim`, and optional `meta`.
#' @param page_type One of `"overall_summary"`, `"three_takeaways"`,
#'   `"theme_evidence"`, `"verbatim_first"`, `"verbatim_compact"`.
#' @param page_index 1-based page index for paged views.
#' @param id HTML id used to scope CSS.
#' @param color_overrides Optional named color override list.
#' @param ... Inline color overrides.
#'
#' @return A `htmltools` object ready for rendering.
#' @export
open_ended_page <- function(
  open_ended_data,
  page_type = c("overall_summary", "three_takeaways", "theme_evidence", "verbatim_first", "verbatim_compact"),
  page_index = 1,
  id = "ohep-open-ended-page",
  color_overrides = NULL,
  ...
) {
  page_type <- match.arg(page_type)
  runtime <- ohepRDisplayr()
  runtime$render_open_ended_page(
    open_ended_data = open_ended_data,
    page_type = page_type,
    page_index = page_index,
    id = id,
    color_overrides = color_overrides,
    ...
  )
}
