sample_open_ended_data <- function() {
  list(
    summary = data.frame(
      title = "Open-Ended Narrative Summary",
      kicker = "Qualitative Insights",
      narrative = "Employees report clearer priorities where leadership context is explicit.",
      overall_score = 3.7,
      score_label = "Avg Fundamental Score",
      context_note = "Precomputed from workbook comments.",
      stringsAsFactors = FALSE
    ),
    takeaways = data.frame(
      takeaway_id = c("takeaway_1", "takeaway_2", "takeaway_3"),
      title = c("Leadership clarity", "Cross-team flow", "Workload pace"),
      narrative = c("Direction-setting is strongest when tradeoffs are explicit.", "Handoffs remain inconsistent across teams.", "Pace is manageable in some groups and strained in others."),
      rank = 1:3,
      stringsAsFactors = FALSE
    ),
    theme_evidence = data.frame(
      takeaway_id = c("takeaway_1", "takeaway_2"),
      theme_title = c("Leadership clarity", "Cross-team flow"),
      context_text = c("Requests focus on clearer sequencing and rationale.", "Comments emphasize ownership at team boundaries."),
      metric_label = c("Leadership", "Communication"),
      metric_value = c(3.8, 3.3),
      metric_status = c("Above Standard", "Industry Standard"),
      stringsAsFactors = FALSE
    ),
    quotes = data.frame(
      quote_id = paste0("q", 1:8),
      takeaway_id = c(rep("takeaway_1", 5), rep("takeaway_2", 3)),
      theme_title = c(rep("Leadership clarity", 5), rep("Cross-team flow", 3)),
      quote_text = paste("Comment", 1:8),
      score = NA_real_,
      source_tag = "Employee comment",
      stringsAsFactors = FALSE
    ),
    verbatim = data.frame(
      comment_id = paste0("c", 1:40),
      takeaway_id = c(rep("takeaway_1", 22), rep("takeaway_2", 18)),
      fundamental = c(rep("Leadership", 22), rep("Communication", 18)),
      comment_text = paste("Verbatim comment", 1:40),
      sort_order = 1:40,
      stringsAsFactors = FALSE
    ),
    meta = data.frame(
      filter_label = "Department: All | Full-Time Status: All | Location: All",
      n_responses = 120L,
      stringsAsFactors = FALSE
    )
  )
}

testthat::test_that("open_ended_page validates required sections/columns", {
  bad <- sample_open_ended_data()
  bad$quotes <- bad$quotes[, setdiff(names(bad$quotes), "quote_text"), drop = FALSE]
  testthat::expect_error(
    open_ended_page(open_ended_data = bad, page_type = "overall_summary"),
    "missing required column"
  )
})

testthat::test_that("open_ended_page renders all page types", {
  dat <- sample_open_ended_data()
  types <- c("overall_summary", "three_takeaways", "theme_evidence", "verbatim_first", "verbatim_compact")
  for (tp in types) {
    out <- open_ended_page(open_ended_data = dat, page_type = tp, page_index = 1, id = paste0("oe-", tp))
    html <- htmltools::renderTags(out)$html
    testthat::expect_match(html, "ohep-open-ended-root")
  }
})

testthat::test_that("theme evidence and verbatim pagination produce continuation pages", {
  dat <- sample_open_ended_data()

  theme_p2 <- htmltools::renderTags(
    open_ended_page(open_ended_data = dat, page_type = "theme_evidence", page_index = 2, id = "oe-theme-p2")
  )$html
  testthat::expect_match(theme_p2, "Continuation page")

  compact_p1 <- htmltools::renderTags(
    open_ended_page(open_ended_data = dat, page_type = "verbatim_compact", page_index = 1, id = "oe-verbatim-p1")
  )$html
  testthat::expect_match(compact_p1, "oe-compact-item")
})
