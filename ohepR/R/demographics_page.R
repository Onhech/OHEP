#' Build Demographics Bipolar Split Panel
#'
#' @param data Data frame with exactly 2 rows and required columns:
#'   `label`, `count`, `pct`.
#' @param title Optional panel title.
#' @param subtitle Optional panel subtitle.
#'
#' @return A typed demographics panel specification for [demographics_page()].
#' @export

demo_bipolar_split <- function(data, title = "", subtitle = "") {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_bipolar_split(data)
  structure(
    list(
      type = "bipolar_split",
      data = data,
      title = title,
      subtitle = subtitle
    ),
    class = c("ohep_demo_panel", "list")
  )
}

#' Build Demographics Categorical Bar Panel
#'
#' @param data Data frame with required columns `category`, `value` and
#'   optional `color_key`.
#' @param title Optional panel title.
#' @param subtitle Optional panel subtitle.
#' @param sort_desc Logical; sort descending by `value` (default `TRUE`).
#'
#' @return A typed demographics panel specification for [demographics_page()].
#' @export

demo_categorical_bar <- function(data, title = "", subtitle = "", sort_desc = TRUE) {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_categorical_bar(data)
  structure(
    list(
      type = "categorical_bar",
      data = data,
      title = title,
      subtitle = subtitle,
      sort_desc = isTRUE(sort_desc)
    ),
    class = c("ohep_demo_panel", "list")
  )
}

#' Build Demographics Categorical Tree Panel
#'
#' @param data Data frame with required columns `category`, `value` and
#'   optional `short_label`, `color_key`.
#' @param title Optional panel title.
#' @param subtitle Optional panel subtitle.
#'
#' @return A typed demographics panel specification for [demographics_page()].
#' @export

demo_categorical_tree <- function(data, title = "", subtitle = "") {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_categorical_tree(data)
  structure(
    list(
      type = "categorical_tree",
      data = data,
      title = title,
      subtitle = subtitle
    ),
    class = c("ohep_demo_panel", "list")
  )
}

#' Build Demographics Ordinal Bar Panel
#'
#' @param data Data frame with required columns `label`, `pct`.
#' @param title Optional panel title.
#' @param subtitle Optional panel subtitle.
#' @param callout_index Optional 1-based segment index to annotate.
#' @param callout_value Optional callout text; defaults to selected segment pct.
#'
#' @return A typed demographics panel specification for [demographics_page()].
#' @export

demo_ordinal_bar <- function(data, title = "", subtitle = "", callout_index = NULL, callout_value = NULL) {
  runtime <- ohepRDisplayr()
  runtime$validate_demo_ordinal_bar(data)
  structure(
    list(
      type = "ordinal_bar",
      data = data,
      title = title,
      subtitle = subtitle,
      callout_index = callout_index,
      callout_value = callout_value
    ),
    class = c("ohep_demo_panel", "list")
  )
}

#' Render Demographics Composer Page
#'
#' Compose up to four demographic graph panels in a fixed 2x2 layout.
#'
#' @param tl Top-left panel spec from one of `demo_*` constructors, or `NULL`.
#' @param tr Top-right panel spec from one of `demo_*` constructors, or `NULL`.
#' @param bl Bottom-left panel spec from one of `demo_*` constructors, or `NULL`.
#' @param br Bottom-right panel spec from one of `demo_*` constructors, or `NULL`.
#' @param id HTML id used to scope CSS for the rendered chart.
#' @param title Optional page title.
#' @param subtitle Optional page subtitle.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param ... Inline color overrides.
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export

demographics_page <- function(
  tl = NULL,
  tr = NULL,
  bl = NULL,
  br = NULL,
  id = "ohep-demographics-page",
  title = "Demographics",
  subtitle = "",
  color_overrides = NULL,
  ...
) {
  is_panel <- function(x) {
    is.null(x) || (is.list(x) && inherits(x, "ohep_demo_panel") && !is.null(x$type) && nzchar(as.character(x$type[[1]])))
  }
  if (!is_panel(tl) || !is_panel(tr) || !is_panel(bl) || !is_panel(br)) {
    stop("`tl`, `tr`, `bl`, and `br` must be NULL or objects from demo_* constructors.", call. = FALSE)
  }

  runtime <- ohepRDisplayr()
  runtime$render_demographics_page(
    tl = tl,
    tr = tr,
    bl = bl,
    br = br,
    id = id,
    title = title,
    subtitle = subtitle,
    color_overrides = color_overrides,
    ...
  )
}
