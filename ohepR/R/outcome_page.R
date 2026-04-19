#' Render Outcome Page
#'
#' Renders a one-pager outcome-focused dashboard. Layout mirrors the
#' fundamental page pattern but inverts the top-right table to show
#' fundamentals that most strongly drive the selected outcome.
#'
#' @param outcome_data Named list with:
#'   - `summary`: one-row data frame with columns
#'     `outcome`, `status_label`, `percentile`, `percentile_delta`,
#'     `delta_label`, `score`, `score_delta`
#'   - `drivers`: data frame with columns
#'     `rank`, `fundamental`, `driver_strength`, `status_label`
#'   - `items`: data frame with columns
#'     `column`, `section`, `section_order`, `item_order`, `label`, `mean`,
#'     `agree_pct`, `neutral_pct`, `disagree_pct`, `vs_industry`, `vs_prior`
#' @param id HTML id used to scope CSS for rendered output.
#'
#' @return `htmltools` tag object.
#' @export
outcome_page <- function(
  outcome_data = NULL,
  id = "ohep-outcome-page"
) {
  if (is.null(outcome_data)) {
    outcome_data <- list(
      summary = data.frame(
        outcome = "Turnover Intent",
        status_label = "Area for Growth",
        percentile = 32,
        percentile_delta = -5,
        delta_label = "vs. 2024",
        score = 3.08,
        score_delta = -0.14,
        stringsAsFactors = FALSE
      ),
      drivers = data.frame(
        rank = 1:5,
        fundamental = c("Leadership", "Communication", "Purpose", "Performance development", "Respect, care, and trust"),
        driver_strength = c(0.61, 0.58, 0.54, 0.49, 0.43),
        status_label = c("High", "High", "High", "Medium", "Medium"),
        stringsAsFactors = FALSE
      ),
      items = data.frame(
        column = c(rep("left", 4), rep("right", 4)),
        section = c("Experience", "Experience", "Experience", "Experience", "Intent", "Intent", "Intent", "Intent"),
        section_order = c(1, 1, 1, 1, 2, 2, 2, 2),
        item_order = c(1, 2, 3, 4, 1, 2, 3, 4),
        label = c(
          "I feel energized when I'm at work.",
          "I feel enthusiastic about my work.",
          "I feel a good level of challenge at work.",
          "When challenged at work, I persevere.",
          "I am satisfied with my job.",
          "I feel fulfilled at this organization.",
          "I expect to still be here in one year.",
          "I am satisfied with actions taken since last year."
        ),
        mean = c(3.2, 3.1, 3.3, 3.4, 3.1, 3.0, 3.2, 3.0),
        agree_pct = c(39, 38, 41, 45, 40, 36, 42, 33),
        neutral_pct = c(31, 29, 30, 28, 32, 33, 31, 35),
        disagree_pct = c(30, 33, 29, 27, 28, 31, 27, 32),
        vs_industry = c(-4, -5, -2, -1, -4, -6, -2, -5),
        vs_prior = c(-3, -2, -1, 0, -4, -3, -1, -2),
        stringsAsFactors = FALSE
      )
    )
  }

  required_summary <- c("outcome", "status_label", "percentile", "percentile_delta", "delta_label", "score", "score_delta")
  required_drivers <- c("rank", "fundamental", "driver_strength", "status_label")
  required_items <- c("column", "section", "section_order", "item_order", "label", "mean", "agree_pct", "neutral_pct", "disagree_pct", "vs_industry", "vs_prior")

  assert_cols <- function(df, req, nm) {
    if (!is.data.frame(df)) stop(sprintf("`outcome_data$%s` must be a data frame.", nm), call. = FALSE)
    miss <- setdiff(req, names(df))
    if (length(miss) > 0L) stop(sprintf("`outcome_data$%s` missing: %s", nm, paste(miss, collapse = ", ")), call. = FALSE)
  }
  assert_cols(outcome_data$summary, required_summary, "summary")
  assert_cols(outcome_data$drivers, required_drivers, "drivers")
  assert_cols(outcome_data$items, required_items, "items")

  summary <- outcome_data$summary[1, , drop = FALSE]
  drivers <- outcome_data$drivers[order(outcome_data$drivers$rank), , drop = FALSE]
  items <- outcome_data$items[order(outcome_data$items$column, outcome_data$items$section_order, outcome_data$items$item_order), , drop = FALSE]

  fmt_pctile <- function(x) {
    n <- suppressWarnings(as.integer(round(as.numeric(x))))
    if (!is.finite(n)) return("NA")
    suffix <- if (n %% 100 %in% c(11, 12, 13)) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
    paste0(n, suffix)
  }
  fmt_delta <- function(x, digits = 0L) {
    n <- suppressWarnings(as.numeric(x))
    if (!is.finite(n)) return("NA")
    if (digits > 0L) sprintf("%+.2f", n) else sprintf("%+d", as.integer(round(n)))
  }
  pct_val <- suppressWarnings(as.numeric(summary$percentile[[1]]))
  pct_left <- if (is.finite(pct_val)) max(5, min(95, pct_val)) else 50
  marker_left <- paste0(sprintf("%.1f", pct_left), "%")

  status_class <- if (pct_left < 40) "status-risk" else if (pct_left < 60) "status-watch" else "status-good"

  driver_rows <- lapply(seq_len(nrow(drivers)), function(i) {
    d <- drivers[i, , drop = FALSE]
    htmltools::tags$tr(
      htmltools::tags$td(htmltools::tags$span(class = "row-rank", as.character(d$rank[[1]])), htmltools::tags$span(class = "row-name", as.character(d$fundamental[[1]]))),
      htmltools::tags$td(
        class = "right-align",
        htmltools::tags$div(
          class = "driver-stack",
          htmltools::tags$div(class = "driver-strength", sprintf("%.2f", as.numeric(d$driver_strength[[1]]))),
          htmltools::tags$div(class = "driver-status", as.character(d$status_label[[1]]))
        )
      )
    )
  })

  render_item_column <- function(col_name) {
    x <- items[items$column == col_name, , drop = FALSE]
    if (nrow(x) < 1L) return(htmltools::tags$div(class = "item-column"))

    rows <- list()
    last_section <- NULL
    for (i in seq_len(nrow(x))) {
      r <- x[i, , drop = FALSE]
      section <- as.character(r$section[[1]])
      if (!identical(section, last_section)) {
        rows[[length(rows) + 1L]] <- htmltools::tags$div(class = "sub-cat", section)
        last_section <- section
      }
      agree <- max(0, as.numeric(r$agree_pct[[1]]))
      neutral <- max(0, as.numeric(r$neutral_pct[[1]]))
      disagree <- max(0, as.numeric(r$disagree_pct[[1]]))
      total <- agree + neutral + disagree
      if (total <= 0) total <- 1
      agree_w <- 100 * agree / total
      neutral_w <- 100 * neutral / total
      disagree_w <- 100 * disagree / total

      mk_delta <- function(v) {
        x <- suppressWarnings(as.numeric(v))
        cls <- if (!is.finite(x) || abs(x) < 0.5) "dp-neu" else if (x > 0) "dp-pos" else "dp-neg"
        txt <- if (is.finite(x)) sprintf("%+d", as.integer(round(x))) else "NA"
        htmltools::tags$span(class = paste("delta-pill", cls), txt)
      }

      rows[[length(rows) + 1L]] <- htmltools::tags$div(
        class = "item-row",
        htmltools::tags$div(class = "item-label", as.character(r$label[[1]])),
        htmltools::tags$div(class = "cell-score", sprintf("%.2f", as.numeric(r$mean[[1]]))),
        htmltools::tags$div(
          class = "sentiment-stack",
          htmltools::tags$span(class = "segment bg-agree", style = paste0("width:", sprintf("%.1f", agree_w), "%"), sprintf("%d%%", as.integer(round(agree)))),
          htmltools::tags$span(class = "segment bg-neutral", style = paste0("width:", sprintf("%.1f", neutral_w), "%"), sprintf("%d%%", as.integer(round(neutral)))),
          htmltools::tags$span(class = "segment bg-disagree", style = paste0("width:", sprintf("%.1f", disagree_w), "%"), sprintf("%d%%", as.integer(round(disagree))))
        ),
        htmltools::tags$div(class = "delta-container", mk_delta(r$vs_industry[[1]])),
        htmltools::tags$div(class = "delta-container", mk_delta(r$vs_prior[[1]]))
      )
    }

    htmltools::tags$div(
      class = "item-column",
      htmltools::tags$div(
        class = "header-row",
        htmltools::tags$div(class = "h-lbl", "Item"),
        htmltools::tags$div(class = "h-lbl", "Mean"),
        htmltools::tags$div(class = "h-lbl", "Sentiment"),
        htmltools::tags$div(class = "h-lbl", "vs Index"),
        htmltools::tags$div(class = "h-lbl", "vs Prior")
      ),
      rows
    )
  }

  css <- paste0(
    "#", id, "{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;background:#F8FAFC;color:#111827;padding:18px;border-radius:10px;}",
    "#", id, " *{box-sizing:border-box;}",
    "#", id, " .top-row{display:flex;gap:16px;min-height:300px;}",
    "#", id, " .card{background:#fff;border:1px solid #e2e8f0;border-radius:12px;padding:20px;display:flex;flex-direction:column;}",
    "#", id, " .fundamental-card{flex:1.2;}#",
    id, " .drivers-card{flex:1;}",
    "#", id, " .title-bar{display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:14px;}",
    "#", id, " .card-title{font-size:22px;font-weight:900;margin:0;text-transform:uppercase;letter-spacing:-.3px;}",
    "#", id, " .status-pill{font-size:11px;font-weight:800;padding:6px 12px;border-radius:999px;text-transform:uppercase;}",
    "#", id, " .status-risk{background:#FEE2E2;color:#B91C1C;}#",
    id, " .status-watch{background:#FEF3C7;color:#B45309;}#",
    id, " .status-good{background:#DCFCE7;color:#166534;}",
    "#", id, " .metrics{display:flex;align-items:flex-start;gap:18px;margin-bottom:12px;}",
    "#", id, " .hero-label{font-size:11px;font-weight:800;text-transform:uppercase;color:#0d9488;letter-spacing:.08em;}",
    "#", id, " .hero-value{font-size:46px;font-weight:900;line-height:1;margin-top:3px;}",
    "#", id, " .delta{margin-top:16px;font-weight:800;color:#DC2626;}",
    "#", id, " .sub-context{font-size:13px;color:#64748b;font-weight:600;}",
    "#", id, " .band-wrap{margin-top:auto;padding-top:14px;position:relative;}",
    "#", id, " .band{height:24px;border-radius:999px;display:flex;overflow:hidden;border:1px solid #e5e7eb;}",
    "#", id, " .z1{width:40%;background:#FBD34C;}#",
    id, " .z2{width:20%;background:#bbf7d0;}#",
    id, " .z3{width:25%;background:#4ADE80;}#",
    id, " .z4{width:15%;background:#2dd4bf;}",
    "#", id, " .tick{position:absolute;top:0;width:2px;height:24px;background:rgba(15,23,42,.2);transform:translateX(-50%);}#",
    id, " .marker{position:absolute;top:-4px;height:32px;width:4px;background:#111827;transform:translateX(-50%);border-radius:3px;left:", marker_left, ";}",
    "#", id, " .labels{display:flex;margin-top:8px;font-size:11px;color:#64748b;font-weight:700;}#",
    id, " .labels span{display:flex;justify-content:center;text-align:center;}#",
    id, " .labels span:nth-child(1){width:40%;}#",
    id, " .labels span:nth-child(2){width:20%;}#",
    id, " .labels span:nth-child(3){width:25%;}#",
    id, " .labels span:nth-child(4){width:15%;}",
    "#", id, " .drivers-table{width:100%;border-collapse:collapse;margin-top:auto;}#",
    id, " .drivers-table th{text-align:left;font-size:10px;font-weight:800;color:#94a3b8;border-bottom:2px solid #e2e8f0;padding:0 0 8px 0;text-transform:uppercase;}#",
    id, " .drivers-table .right-align{text-align:right;}#",
    id, " .drivers-table td{padding:11px 0;border-bottom:1px solid #f1f5f9;}#",
    id, " .drivers-table tr:last-child td{border-bottom:none;}",
    "#", id, " .row-rank{font-size:22px;font-weight:800;color:#cbd5e1;margin-right:10px;}#",
    id, " .row-name{font-size:14px;font-weight:800;color:#111827;}",
    "#", id, " .driver-stack{display:flex;flex-direction:column;align-items:flex-end;}#",
    id, " .driver-strength{font-size:14px;font-weight:800;}#",
    id, " .driver-status{font-size:10px;font-weight:700;color:#64748b;text-transform:uppercase;}",
    "#", id, " .bottom-row{margin-top:16px;}#",
    id, " .item-card{padding:16px;}",
    "#", id, " .item-grid-2col{display:grid;grid-template-columns:1fr 1fr;gap:28px;}",
    "#", id, " .header-row,.item-row{display:grid;grid-template-columns:minmax(170px,2fr) 40px 3fr 56px 56px;gap:10px;align-items:center;}",
    "#", id, " .header-row{padding-bottom:8px;border-bottom:2px solid #E2E8F0;}",
    "#", id, " .h-lbl{font-size:9px;font-weight:800;color:#94A3B8;text-transform:uppercase;letter-spacing:.05em;text-align:center;}#",
    id, " .h-lbl:first-child{text-align:left;}",
    "#", id, " .sub-cat{margin:8px 0 4px;background:#f1f5f9;padding:6px 10px;border-radius:4px;font-size:10px;font-weight:800;text-transform:uppercase;color:#334155;letter-spacing:.07em;}",
    "#", id, " .item-row{padding:6px 0;border-bottom:1px solid #F1F5F9;}#",
    id, " .item-label{font-size:12px;font-weight:600;line-height:1.3;color:#334155;}",
    "#", id, " .cell-score{text-align:center;font-size:13px;font-weight:800;}",
    "#", id, " .sentiment-stack{height:22px;display:flex;border-radius:4px;overflow:hidden;background:#F1F5F9;}",
    "#", id, " .segment{display:flex;align-items:center;justify-content:center;font-size:10px;font-weight:800;}",
    "#", id, " .bg-agree{background:#299D8F;color:#fff;}#",
    id, " .bg-neutral{background:#94A2B9;color:#fff;}#",
    id, " .bg-disagree{background:#EF4444;color:#fff;}",
    "#", id, " .delta-container{display:flex;justify-content:center;}#",
    id, " .delta-pill{display:inline-flex;align-items:center;justify-content:center;width:52px;padding:4px 0;border-radius:4px;font-size:10px;font-weight:800;}",
    "#", id, " .dp-pos{background:#dcfce7;color:#15803d;}#",
    id, " .dp-neg{background:#FEE2E2;color:#B91C1C;}#",
    id, " .dp-neu{background:#f1f5f9;color:#64748B;}"
  )

  htmltools::tags$div(
    id = id,
    class = "outcome-root",
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$div(
      class = "top-row",
      htmltools::tags$div(
        class = "card fundamental-card",
        htmltools::tags$div(
          class = "title-bar",
          htmltools::tags$h2(class = "card-title", as.character(summary$outcome[[1]])),
          htmltools::tags$span(class = paste("status-pill", status_class), as.character(summary$status_label[[1]]))
        ),
        htmltools::tags$div(
          class = "metrics",
          htmltools::tags$div(
            htmltools::tags$div(class = "hero-label", "Percentile"),
            htmltools::tags$div(class = "hero-value", fmt_pctile(summary$percentile[[1]]))
          ),
          htmltools::tags$div(
            class = "delta",
            paste0(fmt_delta(summary$percentile_delta[[1]]), " pts"),
            htmltools::tags$div(style = "font-size:10px;color:#94A3B8;text-transform:uppercase;", as.character(summary$delta_label[[1]]))
          )
        ),
        htmltools::tags$div(
          class = "sub-context",
          paste0("Score: ", sprintf("%.2f", as.numeric(summary$score[[1]])), " (", fmt_delta(summary$score_delta[[1]], digits = 2L), ")")
        ),
        htmltools::tags$div(
          class = "band-wrap",
          htmltools::tags$div(
            class = "band",
            htmltools::tags$span(class = "z1"),
            htmltools::tags$span(class = "z2"),
            htmltools::tags$span(class = "z3"),
            htmltools::tags$span(class = "z4")
          ),
          htmltools::tags$span(class = "tick", style = "left:40%"),
          htmltools::tags$span(class = "tick", style = "left:60%"),
          htmltools::tags$span(class = "tick", style = "left:85%"),
          htmltools::tags$span(class = "marker"),
          htmltools::tags$div(
            class = "labels",
            htmltools::tags$span("RISK ZONE"),
            htmltools::tags$span("WATCH"),
            htmltools::tags$span("HEALTHY"),
            htmltools::tags$span("LEADING")
          )
        )
      ),
      htmltools::tags$div(
        class = "card drivers-card",
        htmltools::tags$div(class = "title-bar", htmltools::tags$h2(class = "card-title", "Fundamental Drivers")),
        htmltools::tags$table(
          class = "drivers-table",
          htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Driver"), htmltools::tags$th(class = "right-align", "Strength"))),
          htmltools::tags$tbody(driver_rows)
        )
      )
    ),
    htmltools::tags$div(
      class = "bottom-row",
      htmltools::tags$div(
        class = "card item-card",
        htmltools::tags$div(
          class = "title-bar",
          htmltools::tags$h2(class = "card-title", "Item Sentiment"),
          htmltools::tags$div(
            style = "display:flex;gap:10px;font-size:11px;font-weight:700;color:#64748b;text-transform:uppercase;",
            htmltools::tags$span("Agree"),
            htmltools::tags$span("Neutral"),
            htmltools::tags$span("Disagree")
          )
        ),
        htmltools::tags$div(
          class = "item-grid-2col",
          render_item_column("left"),
          render_item_column("right")
        )
      )
    )
  )
}
