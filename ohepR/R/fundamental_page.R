#' Render Fundamental Page from Canonical Data
#'
#' Render the Purpose/Fundamental dashboard page using canonical `dashboard_data`.
#'
#' @param dashboard_data Named list with `fundamental`, `outcomes`, and `items`
#'   data frames.
#' @param id HTML id used to scope CSS for the rendered dashboard.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param benchmark_company Logical; include company benchmark column when
#'   available.
#' @param benchmark_index Logical; include index benchmark column when
#'   available.
#' @param benchmark_prior Logical; include prior-year benchmark column when
#'   available.
#' @param ... Inline color overrides (e.g. `favorability_agree = "#0055ff"`).
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export
render_fundamental_page <- function(
  dashboard_data,
  id = "ohep-custom-dashboard",
  color_overrides = NULL,
  benchmark_company = TRUE,
  benchmark_index = TRUE,
  benchmark_prior = TRUE,
  ...
) {
  runtime <- ohepRDisplayr()
  runtime$render_fundamental_page(
    dashboard_data = dashboard_data,
    id = id,
    color_overrides = color_overrides,
    benchmark_company = benchmark_company,
    benchmark_index = benchmark_index,
    benchmark_prior = benchmark_prior,
    ...
  )
}

#' Validate Fundamental Page Data
#'
#' Validates the canonical data contract used by `render_fundamental_page()`.
#'
#' @param dashboard_data Named list with dashboard sections.
#'
#' @return Invisibly returns `dashboard_data` when valid.
#' @export
validate_fundamental_page_data <- function(dashboard_data) {
  runtime <- ohepRDisplayr()
  runtime$validate_fundamental_page_data(dashboard_data)
}

#' Build Fundamental Page from Index and User Data
#'
#' High-level adapter for rendering a fundamental page. Supports:
#' 1) mart-based rendering via `company + year + fundamental + marts`, and
#' 2) backward-compatible raw/index rendering via `fundamental + index_data + user_data`.
#'
#' @param company Company identifier when using mart-based rendering.
#' @param year Reporting year when using mart-based rendering.
#' @param fundamental Fundamental name as string (e.g. `"Purpose"`) or unquoted
#'   symbol in backward-compatible mode.
#' @param marts Named list of derived marts from [prep_ohep_snapshot()].
#' @param filtered_user_data Optional respondent-level filtered data. When
#'   supplied with `marts`, subset metrics are recalculated live for the
#'   selected respondent cut while benchmarks remain fixed from the snapshot.
#' @param min_n Minimum respondent count required to display values in filtered
#'   mode. Defaults to `3`.
#' @param privacy_message Message shown when page-level subset size is below
#'   `min_n`.
#' @param index_data Named list with legacy index inputs.
#' @param user_data Named list with legacy user inputs.
#' @param id HTML id used to scope CSS for the rendered dashboard.
#' @param color_overrides Optional named list of color overrides keyed by
#'   `variable` from `inst/extdata/branding/colors.csv`.
#' @param benchmark_company Logical; include company benchmark column when
#'   available.
#' @param benchmark_index Logical; include index benchmark column when
#'   available.
#' @param benchmark_prior Logical; include prior-year benchmark column when
#'   available.
#' @param ... Inline color overrides (e.g. `favorability_agree = "#0055ff"`).
#'
#' @return A `htmltools` object ready for Displayr rendering.
#' @export
fundamental_page <- function(
  company = NULL,
  year = NULL,
  fundamental = NULL,
  marts = NULL,
  filtered_user_data = NULL,
  min_n = 3,
  privacy_message = "Not enough responses to display this view.",
  index_data = NULL,
  user_data = NULL,
  id = "ohep-custom-dashboard",
  color_overrides = NULL,
  benchmark_company = TRUE,
  benchmark_index = TRUE,
  benchmark_prior = TRUE,
  ...
) {
  inline_overrides <- list(...)
  old_mode <- (
    is.null(marts) &&
      is.null(index_data) &&
      is.null(user_data) &&
      is.list(year) &&
      is.list(fundamental)
  )

  if (old_mode) {
    fundamental_expr <- substitute(company)
    fundamental_name <- if (is.symbol(fundamental_expr)) {
      symbol_name <- as.character(fundamental_expr)
      if (exists(symbol_name, envir = parent.frame(), inherits = TRUE)) {
        symbol_value <- get(symbol_name, envir = parent.frame(), inherits = TRUE)
        if (!is.character(symbol_value) || length(symbol_value) != 1L) {
          stop("`fundamental` variable must contain a single string.", call. = FALSE)
        }
        as.character(symbol_value[[1]])
      } else {
        symbol_name
      }
    } else {
      fundamental_value <- eval(fundamental_expr, parent.frame())
      if (!is.character(fundamental_value) || length(fundamental_value) != 1L) {
        stop("`fundamental` must be a single string or unquoted symbol.", call. = FALSE)
      }
      as.character(fundamental_value[[1]])
    }

    runtime <- ohepRDisplayr()
    return(
      runtime$fundamental_page(
        fundamental = fundamental_name,
        index_data = year,
        user_data = fundamental,
        id = id,
        color_overrides = color_overrides,
        benchmark_company = benchmark_company,
        benchmark_index = benchmark_index,
        benchmark_prior = benchmark_prior,
        ...
      )
    )
  }

  if (is.null(marts)) {
    stop("For new usage, provide `marts` from `prep_ohep_snapshot()`.", call. = FALSE)
  }
  if (is.null(company) || length(company) != 1L) {
    stop("`company` must be a single value.", call. = FALSE)
  }
  if (is.null(year) || length(year) != 1L || !is.finite(suppressWarnings(as.numeric(year)))) {
    stop("`year` must be a single numeric year.", call. = FALSE)
  }

  fundamental_expr <- substitute(fundamental)
  fundamental_name <- NULL
  if (is.symbol(fundamental_expr)) {
    symbol_name <- as.character(fundamental_expr)
    if (!is.null(symbol_name) && !identical(symbol_name, "")) {
      if (exists(symbol_name, envir = parent.frame(), inherits = TRUE)) {
        symbol_value <- get(symbol_name, envir = parent.frame(), inherits = TRUE)
        if (!is.character(symbol_value) || length(symbol_value) != 1L) {
          stop("`fundamental` variable must contain a single string.", call. = FALSE)
        }
        fundamental_name <- as.character(symbol_value[[1]])
      } else {
        fundamental_name <- symbol_name
      }
    }
  } else if (is.character(fundamental) && length(fundamental) == 1L) {
    fundamental_name <- as.character(fundamental[[1]])
  }
  if (is.null(fundamental_name) || identical(fundamental_name, "")) {
    stop("`fundamental` must be a single string or unquoted symbol.", call. = FALSE)
  }

  if (is.data.frame(filtered_user_data)) {
    dashboard_data <- build_dashboard_data_from_filtered_user_data(
      company = company,
      year = as.integer(year),
      fundamental = fundamental_name,
      marts = marts,
      filtered_user_data = filtered_user_data,
      min_n = as.integer(min_n),
      privacy_message = privacy_message
    )
  } else {
    dashboard_data <- build_dashboard_data_from_marts(
      company = company,
      year = as.integer(year),
      fundamental = fundamental_name,
      marts = marts
    )
  }
  runtime <- ohepRDisplayr()
  rendered <- runtime$render_fundamental_page(
    dashboard_data = dashboard_data,
    id = id,
    color_overrides = color_overrides,
    benchmark_company = benchmark_company,
    benchmark_index = benchmark_index,
    benchmark_prior = benchmark_prior,
    ...
  )

  if (is.list(dashboard_data$privacy) && isTRUE(dashboard_data$privacy$suppress_page)) {
    msg <- if (!is.null(dashboard_data$privacy$message)) {
      as.character(dashboard_data$privacy$message[[1]])
    } else {
      as.character(privacy_message)
    }
    n_txt <- if (!is.null(dashboard_data$privacy$current_n)) as.integer(dashboard_data$privacy$current_n[[1]]) else NA_integer_
    min_txt <- if (!is.null(dashboard_data$privacy$min_n)) as.integer(dashboard_data$privacy$min_n[[1]]) else as.integer(min_n)
    primary_note <- sprintf("Requires %d responses to protect anonymity.", min_txt)
    disclaimer <- if (is.finite(n_txt)) {
      sprintf("Current filtered subset: %d responses.", n_txt)
    } else {
      "Blurred background does not reflect accurate data."
    }
    colors <- as.list(runtime$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "fundamental"
    ))
    wrap_id <- paste0(id, "-privacy-wrap")
    overlay_css <- sprintf(
      paste0(
        "#%s.privacy-blurred { position: relative; overflow: hidden; }\n",
        "#%s.privacy-blurred > .privacy-content { filter: blur(5px) grayscale(60%%); opacity: 0.35; pointer-events: none; user-select: none; }\n",
        "#%s.privacy-blurred > .privacy-page-overlay { position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; z-index: 9; }\n",
        "#%s.privacy-blurred .privacy-page-card { background: %s; border: 1px solid %s; border-radius: 12px; padding: 24px; max-width: 440px; box-shadow: 0 10px 25px %s; display: flex; gap: 16px; align-items: flex-start; text-align: left; }\n",
        "#%s.privacy-blurred .icon-container { color: %s; background: %s; padding: 10px; border-radius: 8px; display: flex; flex-shrink: 0; }\n",
        "#%s.privacy-blurred .content-stack { display: flex; flex-direction: column; }\n",
        "#%s.privacy-blurred .privacy-eyebrow { font: 700 11px/1 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }\n",
        "#%s.privacy-blurred .privacy-page-card h3 { margin: 0 0 6px 0; font: 800 16px/1.2 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; text-transform: uppercase; letter-spacing: .5px; }\n",
        "#%s.privacy-blurred .privacy-page-card p.primary-text { margin: 0 0 12px 0; font: 600 13px/1.5 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; }\n",
        "#%s.privacy-blurred .privacy-disclaimer { margin: 0; font: 500 11px/1.4 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: %s; display: flex; align-items: center; gap: 6px; }"
      ),
      wrap_id, wrap_id, wrap_id,
      wrap_id, colors$page_privacy_card_bg, colors$page_privacy_card_border, colors$page_privacy_card_shadow,
      wrap_id, colors$page_privacy_icon_color, colors$page_privacy_icon_bg,
      wrap_id,
      wrap_id, colors$page_privacy_eyebrow,
      wrap_id, colors$page_privacy_title,
      wrap_id, colors$page_privacy_body,
      wrap_id, colors$page_privacy_disclaimer
    )
    return(
      htmltools::tagList(
        htmltools::tags$style(htmltools::HTML(overlay_css)),
        htmltools::tags$div(
          id = wrap_id,
          class = "privacy-blurred",
          htmltools::tags$div(class = "privacy-content", rendered),
          htmltools::tags$div(
            class = "privacy-page-overlay",
            htmltools::tags$div(
              class = "privacy-page-card",
              htmltools::tags$div(
                class = "icon-container",
                htmltools::HTML("<svg width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><rect x=\"3\" y=\"11\" width=\"18\" height=\"11\" rx=\"2\" ry=\"2\"></rect><path d=\"M7 11V7a5 5 0 0 1 10 0v4\"></path></svg>")
              ),
              htmltools::tags$div(
                class = "content-stack",
                htmltools::tags$div(class = "privacy-eyebrow", fundamental_name),
                htmltools::tags$h3("Insufficient Responses"),
                htmltools::tags$p(class = "primary-text", primary_note),
                htmltools::tags$div(
                  class = "privacy-disclaimer",
                  htmltools::HTML("<svg width=\"12\" height=\"12\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><circle cx=\"12\" cy=\"12\" r=\"10\"></circle><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"></line><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"></line></svg>"),
                  if (!identical(msg, "")) msg else disclaimer
                )
              )
            )
          )
        )
      )
    )
  }

  rendered
}
