`%||%` <- function(x, y) if (is.null(x) || length(x) < 1L) y else x

run_open_ended_preview <- function(
  out_dir = NULL,
  clean = FALSE
) {
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(arg) > 0L) normalizePath(sub("^--file=", "", arg[[1]]), winslash = "/", mustWork = FALSE) else ""
  script_dir <- if (nzchar(script_path)) dirname(script_path) else normalizePath(".", winslash = "/", mustWork = TRUE)
  pkg_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) pkg_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  if (file.exists(file.path(pkg_root, "DESCRIPTION")) && requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(pkg_root, quiet = TRUE)
  }

  if (is.null(out_dir) || !nzchar(trimws(as.character(out_dir)))) {
    out_dir <- file.path(pkg_root, "preview", "test_examples", "Open Ended")
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package `htmltools` is required.", call. = FALSE)
  }

  if (clean && dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  open_ended_data <- list(
    summary = data.frame(
      title = "Open-Ended Narrative Summary",
      kicker = "Qualitative Insights",
      narrative = paste(
        "Employees report stronger execution when priorities are sequenced clearly and tradeoffs are explained.",
        "Cross-team handoffs remain uneven in lower-scoring cuts, and workload pacing is a recurring concern",
        "where staffing and scope are misaligned."
      ),
      overall_score = 3.7,
      score_label = "Avg Fundamental Score",
      context_note = "Precomputed qualitative synthesis from workbook comments.",
      stringsAsFactors = FALSE
    ),
    takeaways = data.frame(
      takeaway_id = c("takeaway_1", "takeaway_2", "takeaway_3"),
      title = c("Leadership and direction clarity", "Cross-team operating flow", "Workload sustainability"),
      narrative = c(
        "People ask for clearer rationale behind changing priorities and more explicit sequencing.",
        "Handoff expectations differ by team; respondents request tighter shared workflows.",
        "Pace and capacity concerns rise where role clarity and staffing are less stable."
      ),
      rank = 1:3,
      stringsAsFactors = FALSE
    ),
    theme_evidence = data.frame(
      takeaway_id = c("takeaway_1", "takeaway_2", "takeaway_3"),
      theme_title = c("Leadership and direction clarity", "Cross-team operating flow", "Workload sustainability"),
      context_text = c(
        "Comments emphasize prioritization context and decision transparency.",
        "Respondents highlight process friction at team boundaries.",
        "Sustained delivery is linked to predictable pacing and staffing alignment."
      ),
      metric_label = c("Leadership", "Communication", "Performance development"),
      metric_value = c(3.85, 3.31, 3.18),
      metric_status = c("Above Standard", "Industry Standard", "Area for Growth"),
      stringsAsFactors = FALSE
    ),
    quotes = data.frame(
      quote_id = paste0("q", 1:14),
      takeaway_id = c(rep("takeaway_1", 5), rep("takeaway_2", 4), rep("takeaway_3", 5)),
      theme_title = c(rep("Leadership and direction clarity", 5), rep("Cross-team operating flow", 4), rep("Workload sustainability", 5)),
      quote_text = c(
        "When leaders explain why priorities changed, the team adapts faster.",
        "We understand the direction better when quarterly goals are translated to daily work.",
        "Tradeoff decisions are clearer in some groups than others.",
        "Clarity improves execution speed and reduces rework.",
        "We need earlier signals on shifting priorities.",
        "Handoffs between teams are often where we lose momentum.",
        "Ownership is clear inside teams but less clear between teams.",
        "Shared checklists would reduce duplicate effort.",
        "Cross-functional planning is improving but still inconsistent.",
        "The pace is intense for extended periods without reset time.",
        "Capacity planning is uneven across locations.",
        "Workload is manageable when staffing and scope are aligned.",
        "Competing deadlines make it hard to sustain quality.",
        "More predictable sequencing would reduce burnout risk."
      ),
      source_tag = "Employee comment",
      stringsAsFactors = FALSE
    ),
    verbatim = data.frame(
      comment_id = paste0("c", 1:34),
      takeaway_id = c(rep("takeaway_1", 11), rep("takeaway_2", 10), rep("takeaway_3", 13)),
      fundamental = c(rep("Leadership", 11), rep("Communication", 10), rep("Performance development", 13)),
      comment_text = c(
        "Leadership context helps us execute better.",
        "Priorities are clear when changes are explained.",
        "Better roadmaps reduce churn.",
        "Quarterly planning works when teams see the why.",
        "Decision transparency is uneven.",
        "Forward planning could be more consistent.",
        "Clear tradeoffs improve alignment.",
        "Direction setting has improved this year.",
        "We still need clearer sequencing in peak periods.",
        "Manager communication quality varies.",
        "Earlier communication would help execution.",
        "Handoffs can be a bottleneck.",
        "Cross-team ownership needs clearer guardrails.",
        "Work moves faster with shared process standards.",
        "We lose time on duplicated effort.",
        "Documentation quality is inconsistent.",
        "Inter-team response times vary.",
        "Escalation paths are not always clear.",
        "Collaboration is strongest on planned initiatives.",
        "Unplanned work exposes process gaps.",
        "Shared metrics would improve coordination.",
        "Pace can be difficult to sustain over long stretches.",
        "Workload balance is uneven by team.",
        "Capacity planning should happen earlier.",
        "Resourcing constraints increase stress.",
        "Scope changes create avoidable pressure.",
        "Delivery is smoother when priorities stay stable.",
        "Staffing coverage impacts team wellbeing.",
        "Competing urgent requests are common.",
        "Clearer sequencing would reduce burnout risk.",
        "Recovery time after peak periods is limited.",
        "We need stronger planning discipline.",
        "Sustainable pace should be an explicit goal.",
        "Predictable workflows improve consistency."
      ),
      sort_order = 1:34,
      stringsAsFactors = FALSE
    ),
    meta = data.frame(
      filter_label = "Department: All | Full-Time Status: All | Location: All",
      n_responses = 126L,
      stringsAsFactors = FALSE
    )
  )

  pages <- list(
    list(file = "01_open_ended_overall_summary.html", label = "Overall Summary", type = "overall_summary", page_index = 1L),
    list(file = "02_open_ended_three_takeaways.html", label = "Three Takeaways", type = "three_takeaways", page_index = 1L),
    list(file = "03_open_ended_theme_evidence_01.html", label = "Theme Evidence 1", type = "theme_evidence", page_index = 1L),
    list(file = "04_open_ended_theme_evidence_02.html", label = "Theme Evidence 2", type = "theme_evidence", page_index = 2L),
    list(file = "05_open_ended_verbatim_first_01.html", label = "Verbatim First 1", type = "verbatim_first", page_index = 1L),
    list(file = "06_open_ended_verbatim_compact_01.html", label = "Verbatim Compact 1", type = "verbatim_compact", page_index = 1L)
  )

  for (i in seq_along(pages)) {
    p <- pages[[i]]
    obj <- open_ended_page(
      open_ended_data = open_ended_data,
      page_type = p$type,
      page_index = p$page_index,
      id = paste0("ohep-open-ended-preview-", i)
    )
    htmltools::save_html(
      htmltools::tagList(obj),
      file = file.path(out_dir, p$file),
      background = "#f5f8fb"
    )
    rendered <- htmltools::renderTags(obj)$html
    writeLines(rendered, con = file.path(out_dir, sub("\\.html$", "_rendered.txt", p$file)), useBytes = TRUE)
  }

  index_rows <- vapply(pages, function(p) {
    sprintf("<tr><td><a href=\"%s\">%s</a></td></tr>", p$file, p$label)
  }, character(1))
  index_html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>Open Ended Preview</title><style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
    "margin:24px;background:#f8fafc;color:#0f172a}",
    "h1{margin:0 0 12px 0}",
    "table{border-collapse:collapse;width:min(820px,100%);background:#fff;border:1px solid #e2e8f0}",
    "th,td{padding:10px;border-bottom:1px solid #e2e8f0;text-align:left}",
    "a{color:#0f766e;text-decoration:none}a:hover{text-decoration:underline}",
    "</style></head><body>",
    "<h1>Open Ended Preview</h1>",
    "<table><thead><tr><th>Page</th></tr></thead><tbody>",
    paste(index_rows, collapse = ""),
    "</tbody></table></body></html>"
  )
  writeLines(index_html, con = file.path(out_dir, "index.html"), useBytes = TRUE)

  utils::write.csv(
    data.frame(
      page = vapply(pages, function(p) p$label, character(1)),
      file = vapply(pages, function(p) p$file, character(1)),
      stringsAsFactors = FALSE
    ),
    file = file.path(out_dir, "scenario_summary.csv"),
    row.names = FALSE
  )

  message("Open-ended preview written to: ", normalizePath(out_dir, winslash = "/", mustWork = TRUE))
  invisible(out_dir)
}

if (sys.nframe() == 0L) {
  run_open_ended_preview()
}
