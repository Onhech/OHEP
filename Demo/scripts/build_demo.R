#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(devtools)
  library(htmltools)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) < 1L) y else x

script_path <- tryCatch({
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg) < 1L) NA_character_ else normalizePath(sub("^--file=", "", arg[[1]]), winslash = "/", mustWork = TRUE)
}, error = function(e) NA_character_)

script_dir <- if (is.na(script_path)) getwd() else dirname(script_path)
repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
demo_dir <- file.path(repo_root, "Demo")
app_dir <- file.path(demo_dir, "app")
data_dir <- file.path(demo_dir, "data")
build_dir <- file.path(demo_dir, "build")
ohep_dir <- file.path(repo_root, "ohepR")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package `readxl` is required. Install with install.packages('readxl').", call. = FALSE)
}

load_all(ohep_dir, quiet = TRUE)

set.seed(20260414)

workbook_path <- file.path(data_dir, "Example Company Data.xlsx")
if (!file.exists(workbook_path)) {
  stop(sprintf("Missing workbook: %s", workbook_path), call. = FALSE)
}

sheet <- readxl::excel_sheets(workbook_path)[[1]]
raw <- as.data.frame(readxl::read_excel(workbook_path, sheet = sheet))
if (nrow(raw) < 1L) stop("Workbook has no rows.", call. = FALSE)

find_col <- function(patterns, cols) {
  nml <- tolower(cols)
  for (p in patterns) {
    hit <- which(grepl(p, nml, perl = TRUE))
    if (length(hit) > 0L) return(cols[[hit[[1]]]])
  }
  NA_character_
}

year_col <- find_col(c("^year$"), names(raw))
location_col <- find_col(c("^location$"), names(raw))
dept_col <- find_col(c("^department"), names(raw))
tenure_col <- find_col(c("^time at "), names(raw))
position_col <- find_col(c("^position$"), names(raw))
enps_col <- find_col(c("recommend working", "^enps$"), names(raw))

required_cols <- c(year_col, location_col, dept_col, tenure_col, position_col)
if (any(is.na(required_cols))) {
  stop("Workbook is missing one of required columns: Year, Location, Department, Time at..., Position.", call. = FALSE)
}

question_cols <- setdiff(names(raw), c(year_col, location_col, dept_col, tenure_col, position_col))
question_cols <- question_cols[sapply(raw[question_cols], function(x) is.numeric(x) || is.integer(x))]
if (length(question_cols) < 10L) stop("Could not find enough numeric survey question columns.", call. = FALSE)

comment_cols <- setdiff(names(raw), c(year_col, location_col, dept_col, tenure_col, position_col, question_cols, enps_col))
if (length(comment_cols) > 0L) {
  comment_cols <- comment_cols[vapply(raw[comment_cols], function(x) is.character(x) || is.factor(x), logical(1))]
}
if (length(comment_cols) > 0L) {
  non_empty <- vapply(comment_cols, function(col) {
    vals <- trimws(as.character(raw[[col]]))
    mean(nzchar(vals) & !is.na(vals))
  }, numeric(1))
  comment_cols <- comment_cols[non_empty >= 0.1]
}
if (length(comment_cols) > 3L) comment_cols <- comment_cols[seq_len(3L)]

normalize_text <- function(x) {
  out <- as.character(x)
  out <- gsub("<U\\+[0-9A-F]+>", "'", out, perl = TRUE)
  out <- gsub("ARC Resources|ARC", "the organization", out)
  out
}

title_case <- function(x) {
  tools::toTitleCase(tolower(x))
}

raw$year <- suppressWarnings(as.integer(raw[[year_col]]))
raw <- raw[is.finite(raw$year), , drop = FALSE]
if (nrow(raw) < 1L) stop("No valid year rows in workbook.", call. = FALSE)

fmt_cat <- function(x) {
  y <- trimws(as.character(x))
  y[!nzchar(y)] <- "Unknown"
  y
}

raw$location_raw <- fmt_cat(raw[[location_col]])
raw$department_raw <- fmt_cat(raw[[dept_col]])
raw$tenure_raw <- fmt_cat(raw[[tenure_col]])
raw$position_raw <- fmt_cat(raw[[position_col]])

build_label_map <- function(values, pool, fallback_prefix) {
  u <- sort(unique(values))
  if (length(u) <= length(pool)) {
    lbls <- pool[seq_along(u)]
  } else {
    lbls <- c(pool, sprintf("%s %02d", fallback_prefix, seq_len(length(u) - length(pool))))
  }
  setNames(lbls, u)
}

location_pool <- c(
  "North Hub", "Central Hub", "Foothills Hub", "Riverbend Hub", "Prairie Hub",
  "Summit Hub", "West Corridor Hub", "East Corridor Hub", "Valley Hub", "Frontier Hub",
  "Pioneer Hub", "Crescent Hub", "Lakeside Hub", "Ridge Hub", "Harbor Hub"
)
department_pool <- c(
  "Operations Delivery", "Production Engineering", "Commercial Strategy", "Field Operations",
  "Asset Reliability", "Business Development", "Corporate Services", "People & Culture",
  "Finance Operations", "Planning & Performance", "Safety & Assurance", "Data & Insights",
  "Supply Chain", "Infrastructure Services", "Operational Excellence", "Technical Services",
  "Capital Programs", "Stakeholder Relations", "Technology Solutions", "Portfolio Management",
  "Reservoir Development", "Well Performance", "Drilling Execution", "Completions Strategy",
  "Facilities Engineering", "Surface Operations", "Production Optimization", "Operations Planning",
  "Integrated Planning", "Corporate Accounting", "Operations Accounting", "Treasury & Risk",
  "Financial Reporting", "Market Analytics", "Commercial Operations", "Regulatory Affairs",
  "Enterprise Systems", "Information Security", "Digital Product", "Analytics Engineering",
  "Talent Acquisition", "Learning & Development", "Employee Experience", "Compensation & Benefits",
  "Community Relations", "Indigenous Partnerships", "Environment & Sustainability", "Health Services",
  "Quality Assurance", "Maintenance Planning", "Project Controls", "Contract Management"
)

uniq_loc <- sort(unique(raw$location_raw))
loc_map <- build_label_map(uniq_loc, location_pool, "Regional Hub")
raw$location_anon <- unname(loc_map[raw$location_raw])

uniq_dept <- sort(unique(raw$department_raw))
dept_map <- build_label_map(uniq_dept, department_pool, "Department Group")
raw$department_anon <- unname(dept_map[raw$department_raw])

pos_levels <- sort(unique(raw$position_raw))
pos_map <- setNames(sprintf("Job Band %s", LETTERS[seq_along(pos_levels)]), pos_levels)
raw$position_anon <- unname(pos_map[raw$position_raw])

tenure_levels <- sort(unique(raw$tenure_raw))
tenure_map <- setNames(sprintf("Tenure Group %02d", seq_along(tenure_levels)), tenure_levels)
raw$tenure_anon <- unname(tenure_map[raw$tenure_raw])

is_manager <- grepl("manager|supervisor|lead|foreman|executive", tolower(raw$position_raw))
rand_u <- runif(nrow(raw))
raw$full_time_status <- ifelse(is_manager | rand_u < 0.86, "Full-Time", ifelse(rand_u < 0.94, "Contract", "Part-Time"))

loc_top <- names(sort(table(raw$location_anon), decreasing = TRUE))
loc_top <- utils::head(loc_top, 4L)
raw$location_cluster <- ifelse(raw$location_anon %in% loc_top, raw$location_anon, "Regional Support Sites")

dept_top <- names(sort(table(raw$department_anon), decreasing = TRUE))
dept_top <- utils::head(dept_top, 6L)
raw$department_filter <- ifelse(raw$department_anon %in% dept_top, raw$department_anon, "Cross-Functional Groups")

jitter_likert <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  out <- val
  ok <- is.finite(val)
  delta <- sample(c(-1, 0, 1), size = sum(ok), replace = TRUE, prob = c(0.12, 0.76, 0.12))
  out[ok] <- pmin(5, pmax(1, round(val[ok] + delta)))
  out
}

jitter_enps <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  out <- val
  ok <- is.finite(val)
  delta <- sample(c(-1, 0, 1), size = sum(ok), replace = TRUE, prob = c(0.18, 0.64, 0.18))
  out[ok] <- pmin(10, pmax(0, round(val[ok] + delta)))
  out
}

question_map <- function(txt) {
  t <- tolower(normalize_text(txt))
  is_reverse <- grepl("going through the motions|struggle with completing|required deliverables|pessimistic", t)
  if (grepl("energized|enthusiastic|good level of challenge|always persevere", t)) {
    return(list(type = "outcome", fundamental = "Engagement", reverse = FALSE))
  }
  if (grepl("going through the motions|struggle with completing|required deliverables|pessimistic", t)) {
    return(list(type = "outcome", fundamental = "Burnout", reverse = TRUE))
  }
  if (grepl("satisfied with my job|happy and fulfilled|working .*1 year", t)) {
    return(list(type = "outcome", fundamental = "Work satisfaction", reverse = FALSE))
  }
  if (grepl("purpose|meaningful|proud|community|stakeholders", t)) return(list(type = "fundamental", fundamental = "Purpose", reverse = FALSE))
  if (grepl("strategy|priorities|strategic objectives|accountable|decision", t)) return(list(type = "fundamental", fundamental = "Strategy", reverse = FALSE))
  if (grepl("information|communicative|speak openly|communicated", t)) return(list(type = "fundamental", fundamental = "Communication", reverse = FALSE))
  if (grepl("leader|executive team|confidence", t)) return(list(type = "fundamental", fundamental = "Leadership", reverse = FALSE))
  if (grepl("new ideas|reflection|trends|mistakes|discussion|different perspectives", t)) return(list(type = "fundamental", fundamental = "Learning & innovation", reverse = FALSE))
  if (grepl("respect|trust|belonging|be myself|ethic|relationships|collaboration", t)) return(list(type = "fundamental", fundamental = "Respect, care, and trust", reverse = FALSE))
  if (grepl("feedback|performance|achievements|compensation|professional development", t)) return(list(type = "fundamental", fundamental = "Performance development", reverse = FALSE))
  if (grepl("safety|safe workplace|safety incidents|safety culture|training", t)) return(list(type = "fundamental", fundamental = "Safety", reverse = FALSE))
  list(type = "fundamental", fundamental = "Purpose", reverse = FALSE)
}

item_rows <- list()
user_items <- list()

q_no <- 1L
for (col in question_cols) {
  if (!is.na(enps_col) && identical(col, enps_col)) next
  m <- question_map(col)
  item_id <- sprintf("q_%03d", q_no)
  q_no <- q_no + 1L
  values <- suppressWarnings(as.numeric(raw[[col]]))
  values <- jitter_likert(values)
  user_items[[item_id]] <- values
  item_rows[[length(item_rows) + 1L]] <- data.frame(
    type = m$type,
    item_id = item_id,
    item_text = normalize_text(col),
    fundamental_id = m$fundamental,
    facet_id = m$fundamental,
    is_reverse_scored = as.logical(m$reverse),
    response_scale_min = 1,
    response_scale_max = 5,
    stringsAsFactors = FALSE
  )
}

item_data <- do.call(rbind, item_rows)

if (!is.na(enps_col)) {
  enps_values <- jitter_enps(raw[[enps_col]])
} else {
  enps_values <- rep(NA_real_, nrow(raw))
}

user_data <- data.frame(
  year = raw$year,
  company = "Company A",
  demo_department = raw$department_filter,
  demo_location = raw$location_cluster,
  employeeGroup = raw$full_time_status,
  demo_position = raw$position_anon,
  demo_tenure = raw$tenure_anon,
  eNPS = enps_values,
  stringsAsFactors = FALSE
)
for (nm in names(user_items)) user_data[[nm]] <- user_items[[nm]]
if (length(comment_cols) > 0L) {
  for (i in seq_along(comment_cols)) {
    ccol <- comment_cols[[i]]
    user_data[[paste0("comment_", i)]] <- normalize_text(raw[[ccol]])
  }
}

year_counts <- sort(table(user_data$year), decreasing = TRUE)
candidate_years <- as.integer(names(year_counts)[year_counts >= 100])
report_year <- if (length(candidate_years) > 0L) max(candidate_years) else as.integer(names(year_counts)[[1]])

index_data <- list(
  item_data = item_data,
  user_data_key = data.frame(item = item_data$item_id, Description = item_data$item_text, type = item_data$type, stringsAsFactors = FALSE)
)

build_predictive <- function(df, meta, year_val) {
  rows <- df[df$year == year_val, , drop = FALSE]
  if (nrow(rows) < 50L) rows <- df[df$year == max(df$year), , drop = FALSE]
  by_fund <- split(meta$item_id, meta$fundamental_id)
  resp <- data.frame(id = seq_len(nrow(rows)))
  for (f in names(by_fund)) {
    ids <- by_fund[[f]]
    ids <- ids[ids %in% names(rows)]
    if (length(ids) < 1L) next
    resp[[f]] <- rowMeans(as.data.frame(rows[ids]), na.rm = TRUE)
  }
  outcomes <- c("Engagement", "Burnout", "Work satisfaction")
  fundamentals <- setdiff(unique(meta$fundamental_id), outcomes)
  out <- list()
  for (f in fundamentals) {
    if (!f %in% names(resp)) next
    for (o in outcomes) {
      if (!o %in% names(resp)) next
      cxy <- suppressWarnings(cor(resp[[f]], resp[[o]], use = "pairwise.complete.obs"))
      if (!is.finite(cxy)) cxy <- 0
      out[[length(out) + 1L]] <- data.frame(
        Fundamental = f,
        Outcome = o,
        subset = "All",
        strength = as.numeric(cxy),
        strength_ci_low = as.numeric(cxy - 0.05),
        strength_ci_high = as.numeric(cxy + 0.05),
        type = "correlation",
        direction = ifelse(cxy >= 0, "positive", "negative"),
        significant = TRUE,
        N = nrow(rows),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) < 1L) {
    return(data.frame(
      Fundamental = "Purpose", Outcome = "Engagement", subset = "All", strength = 0.25,
      strength_ci_low = 0.2, strength_ci_high = 0.3, type = "synthetic", direction = "positive", significant = TRUE, N = nrow(rows),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, out)
}

predictive_data <- build_predictive(user_data, item_data, report_year)
marts <- prep_ohep_snapshot(user_data, index_data, predictive_data)

slugify <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- gsub("[^a-z0-9]+", "_", out)
  out <- gsub("_+", "_", out)
  out <- gsub("^_|_$", "", out)
  out
}

rows_report <- user_data[user_data$year == report_year, , drop = FALSE]
dim_department <- sort(unique(rows_report$demo_department))
dim_department <- dim_department[nzchar(dim_department)]
dim_department <- c("All", dim_department)
dim_status <- c("All", sort(unique(rows_report$employeeGroup)))
dim_location <- c("All", sort(unique(rows_report$demo_location)))

filter_grid <- expand.grid(
  department = dim_department,
  full_time_status = dim_status,
  location_cluster = dim_location,
  stringsAsFactors = FALSE
)
filter_grid$filter_id <- paste0(
  "department=", slugify(filter_grid$department),
  "|status=", slugify(filter_grid$full_time_status),
  "|location=", slugify(filter_grid$location_cluster)
)

apply_filter <- function(df, row) {
  out <- df
  if (row$department != "All") out <- out[out$demo_department == row$department, , drop = FALSE]
  if (row$full_time_status != "All") out <- out[out$employeeGroup == row$full_time_status, , drop = FALSE]
  if (row$location_cluster != "All") out <- out[out$demo_location == row$location_cluster, , drop = FALSE]
  out
}

fundamentals <- unique(item_data$fundamental_id[item_data$type == "fundamental"])
fundamentals <- fundamentals[fundamentals %in% unique(marts$company_fundamental_year$fundamental_id)]
fundamentals <- fundamentals[nzchar(fundamentals)]

orientation_path <- file.path(data_dir, "system_orientation_text.txt")
orientation_text <- if (file.exists(orientation_path)) {
  paste(readLines(orientation_path, warn = FALSE), collapse = "\n")
} else {
  paste(
    "This report summarizes employee sentiment, strengths, and risk areas across organizational fundamentals and outcomes.",
    "Use the three filters to compare departments, employment status, and location clusters.",
    "Scores are benchmarked and anonymized to preserve confidentiality while showing clear signal for decision-making."
  )
}

html_escape <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

slide_doc <- function(inner, title = "OHEP Demo") {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", html_escape(title), "</title>",
    "<style>",
    "html,body{margin:0;padding:0;background:#f5f8fb;color:#0f172a;font-family:'Avenir Next','Segoe UI',sans-serif;}",
    ".slide{padding:24px 28px 30px 28px;max-width:1220px;margin:0 auto;}",
    ".k{font-size:12px;text-transform:uppercase;letter-spacing:.08em;color:#0f8694;font-weight:700;margin-bottom:8px;}",
    ".h1{font-size:34px;line-height:1.15;margin:0 0 10px 0;}",
    ".sub{font-size:17px;color:#4b5563;line-height:1.45;}",
    ".grid2{display:grid;grid-template-columns:1fr 1fr;gap:14px;}",
    ".grid3{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;}",
    ".card{background:#fff;border:1px solid #d9e2eb;border-radius:12px;padding:14px;}",
    ".m{font-size:30px;font-weight:700;}",
    ".s{font-size:12px;color:#667085;}",
    ".q{font-size:16px;line-height:1.4;font-style:italic;}",
    "</style></head><body>",
    inner,
    "</body></html>"
  )
}

no_data_slide <- function(msg = "Insufficient data for this filter combination.") {
  slide_doc(
    paste0("<div class='slide'><div class='k'>No Data</div><div class='h1'>View unavailable</div><p class='sub'>", html_escape(msg), "</p></div>"),
    "No Data"
  )
}

render_html_obj <- function(obj, title = "Slide") {
  tags <- htmltools::renderTags(obj)
  slide_doc(tags$html, title)
}

kpi_rows <- list()
item_rows_out <- list()
demo_panel_rows <- list()

for (i in seq_len(nrow(filter_grid))) {
  fr <- filter_grid[i, , drop = FALSE]
  rows_sub <- apply_filter(user_data, fr)
  rows_curr <- rows_sub[rows_sub$year == report_year, , drop = FALSE]
  if (nrow(rows_curr) < 1L) next
  fid <- fr$filter_id[[1]]

  if ("eNPS" %in% names(rows_curr)) {
    en <- suppressWarnings(as.numeric(rows_curr$eNPS))
    en <- en[is.finite(en)]
    if (length(en) > 0L) {
      kpi_rows[[length(kpi_rows) + 1L]] <- data.frame(
        filter_id = fid, company = "Company A", year = report_year, metric_group = "outcome",
        metric_id = "eNPS", metric_label = "eNPS", score = 100 * (sum(en >= 9) - sum(en <= 6)) / length(en),
        stringsAsFactors = FALSE
      )
    }
  }

  outcome_meta <- item_data[item_data$type == "outcome", c("item_id", "fundamental_id", "is_reverse_scored", "response_scale_min", "response_scale_max"), drop = FALSE]
  if (nrow(outcome_meta) > 0L) {
    score_value <- function(value, min_scale, max_scale, reverse) {
      val <- suppressWarnings(as.numeric(value))
      if (!is.finite(val)) return(NA_real_)
      if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
        min_scale <- 1
        max_scale <- 5
      }
      if (isTRUE(reverse)) min_scale + max_scale - val else val
    }
    out_vals <- list()
    for (j in seq_len(nrow(outcome_meta))) {
      iid <- as.character(outcome_meta$item_id[[j]])
      if (!iid %in% names(rows_curr)) next
      scored <- vapply(
        rows_curr[[iid]],
        score_value,
        numeric(1),
        min_scale = as.numeric(outcome_meta$response_scale_min[[j]]),
        max_scale = as.numeric(outcome_meta$response_scale_max[[j]]),
        reverse = as.logical(outcome_meta$is_reverse_scored[[j]])
      )
      out_vals[[length(out_vals) + 1L]] <- data.frame(
        outcome = as.character(outcome_meta$fundamental_id[[j]]),
        score = scored,
        stringsAsFactors = FALSE
      )
    }
    if (length(out_vals) > 0L) {
      out_df <- do.call(rbind, out_vals)
      out_agg <- stats::aggregate(score ~ outcome, data = out_df, FUN = function(x) mean(x, na.rm = TRUE))
      for (j in seq_len(nrow(out_agg))) {
        kpi_rows[[length(kpi_rows) + 1L]] <- data.frame(
          filter_id = fid,
          company = "Company A",
          year = report_year,
          metric_group = "outcome",
          metric_id = as.character(out_agg$outcome[[j]]),
          metric_label = as.character(out_agg$outcome[[j]]),
          score = as.numeric(out_agg$score[[j]]),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  for (f in fundamentals) {
    dash <- tryCatch(
      build_dashboard_data_from_filtered_user_data(
        company = "Company A",
        year = report_year,
        fundamental = f,
        marts = marts,
        filtered_user_data = rows_sub,
        min_n = 10
      ),
      error = function(e) NULL
    )
    if (is.null(dash)) next
    fs <- suppressWarnings(as.numeric(dash$fundamental$score[[1]] %||% NA))
    if (is.finite(fs)) {
      kpi_rows[[length(kpi_rows) + 1L]] <- data.frame(
        filter_id = fid, company = "Company A", year = report_year,
        metric_group = "fundamental", metric_id = f, metric_label = f, score = fs,
        stringsAsFactors = FALSE
      )
    }
    if (is.data.frame(dash$items) && nrow(dash$items) > 0L) {
      for (r in seq_len(nrow(dash$items))) {
        item_rows_out[[length(item_rows_out) + 1L]] <- data.frame(
          filter_id = fid,
          fundamental_id = f,
          item_id = paste0("item_", r),
          item_label = as.character(dash$items$label[[r]]),
          mean = as.numeric(dash$items$mean[[r]]),
          agree_pct = as.numeric(dash$items$agree_pct[[r]] %||% NA_real_),
          neutral_pct = as.numeric(dash$items$neutral_pct[[r]] %||% NA_real_),
          disagree_pct = as.numeric(dash$items$disagree_pct[[r]] %||% NA_real_),
          vs_industry = as.numeric(dash$items$vs_industry[[r]] %||% NA_real_),
          vs_prior = as.numeric(dash$items$vs_prior[[r]] %||% NA_real_),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  for (slot in c("tl", "tr", "bl", "br")) {
    col <- switch(slot, tl = "demo_department", tr = "demo_location", bl = "employeeGroup", br = "demo_tenure")
    tab <- sort(table(rows_curr[[col]]), decreasing = TRUE)
    tab <- utils::head(tab, 6L)
    total <- sum(tab)
    for (k in seq_along(tab)) {
      demo_panel_rows[[length(demo_panel_rows) + 1L]] <- data.frame(
        filter_id = fid,
        panel_slot = slot,
        panel_type = "categorical_bar",
        category = names(tab)[[k]],
        label = col,
        count = as.integer(tab[[k]]),
        pct = round(100 * as.numeric(tab[[k]]) / total, 1),
        sort_order = as.integer(k),
        stringsAsFactors = FALSE
      )
    }
  }
}

kpi_scores <- if (length(kpi_rows) > 0L) do.call(rbind, kpi_rows) else data.frame()
item_scores <- if (length(item_rows_out) > 0L) do.call(rbind, item_rows_out) else data.frame()
demographics_panels <- if (length(demo_panel_rows) > 0L) do.call(rbind, demo_panel_rows) else data.frame()

if (nrow(kpi_scores) < 1L || nrow(item_scores) < 1L || nrow(demographics_panels) < 1L) {
  stop("Generated normalized tables are empty after filtering.", call. = FALSE)
}

heatmap_values <- data.frame()
for (f in fundamentals) {
  for (dep in dim_department[dim_department != "All"]) {
    key <- filter_grid$filter_id[filter_grid$department == dep & filter_grid$full_time_status == "All" & filter_grid$location_cluster == "All"][[1]]
    score <- kpi_scores$score[kpi_scores$filter_id == key & kpi_scores$metric_group == "fundamental" & kpi_scores$metric_id == f]
    heatmap_values <- rbind(
      heatmap_values,
      data.frame(
        segment_type_id = "department",
        row_label = f,
        segment_value_id = slugify(dep),
        segment_value_label = dep,
        value = as.numeric(score[[1]] %||% NA_real_),
        stringsAsFactors = FALSE
      )
    )
  }
}

themes_df <- data.frame(
  theme_id = c("theme_1", "theme_2", "theme_3"),
  theme_label = c("Leadership Clarity", "Collaboration Friction", "Workload Sustainability"),
  section = "Thematic Insights",
  summary = c(
    "Employees ask for more explicit priority-setting and forward planning.",
    "Cross-team handoffs are inconsistent across operating groups.",
    "Workload balance and pace are recurring concerns in lower-scoring cuts."
  ),
  mention_count = c(38L, 31L, 27L),
  priority_rank = 1:3,
  stringsAsFactors = FALSE
)

quotes_df <- data.frame(
  quote_id = paste0("quote_", 1:9),
  theme_id = rep(themes_df$theme_id, each = 3),
  section = "Qualitative Insights",
  quote_text = c(
    "Priorities shift quickly and we need better context on tradeoffs.",
    "I can see the strategy, but translation to team-level action is uneven.",
    "When leadership explains the why, execution gets much easier.",
    "Some teams collaborate really well, others still work in silos.",
    "We lose time when handoff expectations are not explicit.",
    "Shared workflows would reduce rework between groups.",
    "The pace can be intense for long stretches without reset time.",
    "Workload planning has improved but not consistently across teams.",
    "Sustained delivery is possible when staffing and scope are aligned."
  ),
  sentiment = "mixed",
  source_col = "synthetic_from_example",
  stringsAsFactors = FALSE
)

cfy <- marts$company_fundamental_year
cfy_curr <- cfy[cfy$company == "Company A" & cfy$year == report_year, , drop = FALSE]
low <- cfy_curr[order(cfy_curr$percentile), , drop = FALSE]
low <- utils::head(low, 3L)
actions_df <- data.frame(
  action_id = paste0("action_", seq_len(nrow(low))),
  priority_rank = seq_len(nrow(low)),
  action_title = paste("Improve", low$fundamental_id),
  rationale = paste("Current percentile:", round(low$percentile), "| score:", round(low$mean, 2)),
  owner_hint = "Business Unit + HR",
  time_horizon = c("0-90 days", "90-180 days", "180+ days")[seq_len(nrow(low))],
  stringsAsFactors = FALSE
)

report_meta <- data.frame(
  report_title = "NorthRiver Energy Organizational Health Report",
  report_subtitle = paste("Annual Workforce Insights | Reporting year", report_year),
  client_label = "NorthRiver Energy",
  reporting_year = report_year,
  summary_text = "Enterprise employee insights across fundamentals, outcomes, and segment comparisons.",
  stringsAsFactors = FALSE
)

segment_options <- rbind(
  data.frame(segment_type_id = "department", segment_value_id = slugify(dim_department), segment_type_label = "Department", segment_value_label = dim_department, sort_order = seq_along(dim_department), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "full_time_status", segment_value_id = slugify(dim_status), segment_type_label = "Full-Time Status", segment_value_label = dim_status, sort_order = seq_along(dim_status), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "location_cluster", segment_value_id = slugify(dim_location), segment_type_label = "Location Cluster", segment_value_label = dim_location, sort_order = seq_along(dim_location), n = NA_integer_, stringsAsFactors = FALSE)
)

validate_csv <- function(df, req, name) {
  miss <- setdiff(req, names(df))
  if (length(miss) > 0L) stop(sprintf("%s missing required columns: %s", name, paste(miss, collapse = ", ")), call. = FALSE)
  if (nrow(df) < 1L) stop(sprintf("%s is empty.", name), call. = FALSE)
}

validate_csv(report_meta, c("report_title", "report_subtitle", "client_label", "reporting_year", "summary_text"), "report_meta.csv")
validate_csv(segment_options, c("segment_type_id", "segment_value_id", "segment_type_label", "segment_value_label"), "segment_options.csv")
validate_csv(kpi_scores, c("filter_id", "metric_group", "metric_id", "score"), "kpi_scores.csv")
validate_csv(item_scores, c("filter_id", "fundamental_id", "item_label", "mean"), "item_scores.csv")
validate_csv(demographics_panels, c("filter_id", "panel_slot", "category", "pct"), "demographics_panels.csv")
validate_csv(heatmap_values, c("segment_type_id", "row_label", "segment_value_label", "value"), "heatmap_values.csv")
validate_csv(themes_df, c("theme_id", "theme_label", "summary"), "themes.csv")
validate_csv(quotes_df, c("quote_id", "quote_text"), "quotes.csv")
validate_csv(actions_df, c("action_id", "action_title", "rationale"), "actions.csv")

utils::write.csv(report_meta, file.path(data_dir, "report_meta.csv"), row.names = FALSE)
utils::write.csv(segment_options, file.path(data_dir, "segment_options.csv"), row.names = FALSE)
utils::write.csv(kpi_scores, file.path(data_dir, "kpi_scores.csv"), row.names = FALSE)
utils::write.csv(item_scores, file.path(data_dir, "item_scores.csv"), row.names = FALSE)
utils::write.csv(demographics_panels, file.path(data_dir, "demographics_panels.csv"), row.names = FALSE)
utils::write.csv(heatmap_values, file.path(data_dir, "heatmap_values.csv"), row.names = FALSE)
utils::write.csv(themes_df, file.path(data_dir, "themes.csv"), row.names = FALSE)
utils::write.csv(quotes_df, file.path(data_dir, "quotes.csv"), row.names = FALSE)
utils::write.csv(actions_df, file.path(data_dir, "actions.csv"), row.names = FALSE)

slides <- data.frame(
  slide_id = c(
    "cover", "orientation_model",
    "survey_overview", "demographics_overview",
    "snapshot", "strengths_risks", "priority_matrix",
    "outcomes_overview", "engagement_deep_dive", "burnout_deep_dive", "work_satisfaction_deep_dive", "enps",
    paste0("fundamental_", slugify(fundamentals)),
    "segment_heatmap", "segment_gaps",
    "theme_overview", "verbatim_quotes", "actions"
  ),
  slide_label = c(
    "Cover & How To Read", "Orientation Model",
    "Survey Overview", "Demographics",
    "Snapshot", "Strengths & Risks", "Priority Matrix",
    "Outcomes Overview", "Engagement", "Burnout", "Work Satisfaction", "eNPS",
    paste("Fundamental:", fundamentals),
    "Segment Heatmap", "Key Segment Gaps",
    "Theme Overview", "Verbatim Quotes", "Action Priorities"
  ),
  section_id = c(
    "system_orientation", "system_orientation",
    "population_context", "population_context",
    "system_summary", "system_summary", "system_summary",
    "outcomes", "outcomes", "outcomes", "outcomes", "outcomes",
    rep("drivers", length(fundamentals)),
    "segmentation", "segmentation",
    "thematic", "qualitative", "actions"
  ),
  stringsAsFactors = FALSE
)
slides$sort_order <- seq_len(nrow(slides))

section_meta <- data.frame(
  section_id = c("system_orientation","population_context","system_summary","outcomes","drivers","segmentation","thematic","qualitative","actions"),
  section_label = c("System Orientation","Population Context","System-Level Summary","Outcomes","Drivers / Fundamentals","Segmentation & Comparisons","Thematic Insights","Qualitative Insights","Action & Prioritization"),
  section_order = seq_len(9),
  stringsAsFactors = FALSE
)

build_demographics_page <- function(fid) {
  x <- demographics_panels[demographics_panels$filter_id == fid, , drop = FALSE]
  if (nrow(x) < 1L) return(no_data_slide())
  mk <- function(slot, ttl) {
    p <- x[x$panel_slot == slot, , drop = FALSE]
    if (nrow(p) < 1L) return(NULL)
    demo_categorical_bar(data = data.frame(category = p$category, value = p$pct, stringsAsFactors = FALSE), title = ttl, subtitle = "Distribution", sort_desc = TRUE)
  }
  render_html_obj(demographics_page(
    tl = mk("tl", "Department"),
    tr = mk("tr", "Location Cluster"),
    bl = mk("bl", "Full-Time Status"),
    br = mk("br", "Tenure"),
    title = "Demographics Overview",
    subtitle = "Current filtered population"
  ), "Demographics")
}

build_model_data <- function(fid) {
  k <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]
  f <- k[k$metric_group == "fundamental", , drop = FALSE]
  o <- k[k$metric_group == "outcome", , drop = FALSE]
  if (nrow(f) < 1L) return(NULL)
  list(
    summary = data.frame(title = "Organizational Health Model", subtitle = "Orientation and score architecture", fundamentals_label = "Fundamentals", outcomes_label = "Outcomes", raw_avg_label = "Score", delta_label = "vs Prior", stringsAsFactors = FALSE),
    fundamentals = data.frame(label = f$metric_label, percentile = pmax(1, pmin(99, round((f$score / 5) * 100))), raw_avg = round(f$score, 2), delta = 0, shape = "circle", stringsAsFactors = FALSE),
    outcomes = data.frame(label = o$metric_label, percentile = pmax(1, pmin(99, ifelse(o$metric_id == "eNPS", 50 + o$score / 2, round((o$score / 5) * 100)))), shape = "diamond", stringsAsFactors = FALSE)
  )
}

build_matrix_points <- function(fid) {
  f <- kpi_scores[kpi_scores$filter_id == fid & kpi_scores$metric_group == "fundamental", , drop = FALSE]
  if (nrow(f) < 1L) return(data.frame())
  pe <- marts$predictive_edges
  pe <- pe[pe$subset == "All", , drop = FALSE]
  imp <- stats::aggregate(abs(strength) ~ fundamental, data = transform(pe, strength = as.numeric(strength)), FUN = function(x) mean(x, na.rm = TRUE))
  names(imp)[2] <- "impact"
  x <- merge(f, imp, by.x = "metric_id", by.y = "fundamental", all.x = TRUE, sort = FALSE)
  x$impact[is.na(x$impact)] <- median(x$impact, na.rm = TRUE)
  x$impact[!is.finite(x$impact)] <- 0.2
  data.frame(fundamental = x$metric_id, label = x$metric_label, score = x$score - 3.5, impact = x$impact, stringsAsFactors = FALSE)
}

build_heatmap <- function() {
  deps <- setdiff(dim_department, "All")
  if (length(deps) < 1L) return(NULL)
  tab <- data.frame(Category = fundamentals, stringsAsFactors = FALSE, check.names = FALSE)
  for (d in deps) {
    key <- filter_grid$filter_id[filter_grid$department == d & filter_grid$full_time_status == "All" & filter_grid$location_cluster == "All"][[1]]
    vals <- sapply(fundamentals, function(fu) {
      v <- kpi_scores$score[kpi_scores$filter_id == key & kpi_scores$metric_group == "fundamental" & kpi_scores$metric_id == fu]
      as.numeric(v[[1]] %||% NA_real_)
    })
    tab[[d]] <- vals
  }
  list(title = "Department Heatmap", subtitle = "Cross-department comparison", legend_low = "Lower", legend_high = "Higher", tables = list("Fundamental Scores" = tab))
}

filter_label <- function(fr) {
  paste0("Department: ", fr$department, " | Full-Time Status: ", fr$full_time_status, " | Location: ", fr$location_cluster)
}

build_outcome_data <- function(outcome_name, rows_sub, fid) {
  rows_curr <- rows_sub[rows_sub$year == report_year, , drop = FALSE]
  if (nrow(rows_curr) < 1L) return(NULL)
  rows_prior <- rows_sub[rows_sub$year == (report_year - 1L), , drop = FALSE]

  m <- item_data[item_data$type == "outcome" & tolower(item_data$fundamental_id) == tolower(outcome_name), , drop = FALSE]
  if (nrow(m) < 1L) return(NULL)
  m <- m[m$item_id %in% names(rows_curr), , drop = FALSE]
  if (nrow(m) < 1L) return(NULL)

  score_value <- function(value, min_scale, max_scale, reverse) {
    val <- suppressWarnings(as.numeric(value))
    if (!is.finite(val)) return(NA_real_)
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }
    if (isTRUE(reverse)) min_scale + max_scale - val else val
  }
  sentiment <- function(value, min_scale, max_scale) {
    if (!is.finite(value)) return(NA_character_)
    cut1 <- min_scale + (max_scale - min_scale) / 3
    cut2 <- min_scale + 2 * (max_scale - min_scale) / 3
    if (value <= cut1) "disagree" else if (value >= cut2) "agree" else "neutral"
  }

  item_rows <- list()
  for (i in seq_len(nrow(m))) {
    iid <- as.character(m$item_id[[i]])
    min_s <- as.numeric(m$response_scale_min[[i]])
    max_s <- as.numeric(m$response_scale_max[[i]])
    rev <- as.logical(m$is_reverse_scored[[i]])

    curr <- vapply(rows_curr[[iid]], score_value, numeric(1), min_scale = min_s, max_scale = max_s, reverse = rev)
    curr_ok <- curr[is.finite(curr)]
    if (length(curr_ok) < 1L) next
    prior <- if (nrow(rows_prior) > 0L) vapply(rows_prior[[iid]], score_value, numeric(1), min_scale = min_s, max_scale = max_s, reverse = rev) else numeric(0)
    prior_ok <- prior[is.finite(prior)]
    sent <- vapply(curr_ok, sentiment, character(1), min_scale = min_s, max_scale = max_s)
    disagree <- round(100 * sum(sent == "disagree") / length(sent))
    neutral <- round(100 * sum(sent == "neutral") / length(sent))
    agree <- round(100 * sum(sent == "agree") / length(sent))
    diff <- 100 - (disagree + neutral + agree)
    if (diff != 0) {
      mx <- which.max(c(disagree, neutral, agree))
      if (mx == 1L) disagree <- disagree + diff
      if (mx == 2L) neutral <- neutral + diff
      if (mx == 3L) agree <- agree + diff
    }
    bm <- marts$benchmark_item_year
    bm_now <- bm[bm$item_id == iid & bm$year == report_year, , drop = FALSE]
    bm_prev <- bm[bm$item_id == iid & bm$year == (report_year - 1L), , drop = FALSE]
    bm_mean <- as.numeric(bm_now$mean[[1]] %||% NA_real_)
    bm_sd <- as.numeric(bm_now$sd[[1]] %||% NA_real_)
    bm_prev_mean <- as.numeric(bm_prev$mean[[1]] %||% NA_real_)
    bm_prev_sd <- as.numeric(bm_prev$sd[[1]] %||% NA_real_)
    if (!is.finite(bm_sd) || bm_sd <= 0) bm_sd <- 1e-9
    if (!is.finite(bm_prev_sd) || bm_prev_sd <= 0) bm_prev_sd <- 1e-9
    cur_mean <- mean(curr_ok, na.rm = TRUE)
    pri_mean <- if (length(prior_ok) > 0L) mean(prior_ok, na.rm = TRUE) else NA_real_
    cur_pct <- if (is.finite(cur_mean) && is.finite(bm_mean)) stats::pnorm((cur_mean - bm_mean) / bm_sd) * 100 else 50
    pri_pct <- if (is.finite(pri_mean) && is.finite(bm_prev_mean)) stats::pnorm((pri_mean - bm_prev_mean) / bm_prev_sd) * 100 else 50

    item_rows[[length(item_rows) + 1L]] <- data.frame(
      column = ifelse(i %% 2 == 1L, "left", "right"),
      section = outcome_name,
      section_order = 1L,
      item_order = i,
      label = as.character(m$item_text[[i]]),
      mean = cur_mean,
      agree_pct = agree,
      neutral_pct = neutral,
      disagree_pct = disagree,
      vs_industry = round(cur_pct - 50),
      vs_prior = round(cur_pct - pri_pct),
      stringsAsFactors = FALSE
    )
  }
  if (length(item_rows) < 1L) return(NULL)
  items_df <- do.call(rbind, item_rows)

  k_now <- kpi_scores[kpi_scores$filter_id == fid & kpi_scores$metric_group == "outcome" & tolower(kpi_scores$metric_id) == tolower(outcome_name), "score", drop = TRUE]
  score_now <- as.numeric(k_now[[1]] %||% mean(items_df$mean, na.rm = TRUE))
  overall_id <- filter_grid$filter_id[filter_grid$department == "All" & filter_grid$full_time_status == "All" & filter_grid$location_cluster == "All"][[1]]
  overall_score <- as.numeric(kpi_scores$score[kpi_scores$filter_id == overall_id & kpi_scores$metric_group == "outcome" & tolower(kpi_scores$metric_id) == tolower(outcome_name)][[1]] %||% score_now)
  score_delta <- score_now - overall_score

  pe <- marts$predictive_edges
  d <- pe[tolower(pe$outcome) == tolower(outcome_name) & tolower(pe$subset) == "all", , drop = FALSE]
  if (nrow(d) < 1L) {
    d <- data.frame(fundamental = fundamentals[seq_len(min(5, length(fundamentals)))], strength = seq(0.4, 0.2, length.out = min(5, length(fundamentals))), stringsAsFactors = FALSE)
  }
  d <- d[order(-abs(as.numeric(d$strength))), , drop = FALSE]
  d <- utils::head(d, 5L)
  drivers_df <- data.frame(
    rank = seq_len(nrow(d)),
    fundamental = as.character(d$fundamental),
    driver_strength = round(abs(as.numeric(d$strength)), 2),
    status_label = ifelse(abs(as.numeric(d$strength)) >= 0.45, "High", ifelse(abs(as.numeric(d$strength)) >= 0.3, "Medium", "Low")),
    stringsAsFactors = FALSE
  )

  status_label <- if (score_now < 3.2) "Area for Growth" else if (score_now < 3.8) "Industry Standard" else "Above Standard"
  list(
    summary = data.frame(
      outcome = outcome_name,
      status_label = status_label,
      percentile = round((score_now / 5) * 100),
      percentile_delta = round(score_delta * 10),
      delta_label = "vs overall",
      score = score_now,
      score_delta = score_delta,
      stringsAsFactors = FALSE
    ),
    drivers = drivers_df,
    items = items_df
  )
}

comment_columns_user <- grep("^comment_[0-9]+$", names(user_data), value = TRUE)

extract_filter_comments <- function(rows_curr) {
  if (length(comment_columns_user) < 1L || nrow(rows_curr) < 1L) return(character(0))
  vals <- unlist(lapply(comment_columns_user, function(col) as.character(rows_curr[[col]])), use.names = FALSE)
  vals <- normalize_text(vals)
  vals <- gsub("\\s+", " ", trimws(vals))
  vals <- vals[nzchar(vals) & nchar(vals) >= 25]
  unique(vals)
}

assign_takeaway_id <- function(comment_text) {
  tx <- tolower(comment_text)
  if (grepl("leader|strategy|priority|clarity|direction|plan", tx, perl = TRUE)) return("takeaway_1")
  if (grepl("team|handoff|collab|cross|silo|communication", tx, perl = TRUE)) return("takeaway_2")
  if (grepl("workload|pace|burnout|capacity|staff|time", tx, perl = TRUE)) return("takeaway_3")
  "takeaway_2"
}

build_open_ended_data <- function(fid, fr, rows_curr) {
  k_now <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]
  fk <- k_now[k_now$metric_group == "fundamental", c("metric_id", "metric_label", "score"), drop = FALSE]
  if (nrow(fk) > 0L) {
    fk <- fk[order(-fk$score), , drop = FALSE]
  }

  comments <- extract_filter_comments(rows_curr)
  if (length(comments) < 1L) {
    top_lbl <- paste(utils::head(fk$metric_label, 2L), collapse = ", ")
    low_lbl <- paste(utils::head(fk$metric_label[order(fk$score)], 2L), collapse = ", ")
    comments <- c(
      paste("People describe strong alignment around", top_lbl, "when priorities are explicit."),
      paste("The most common friction appears around", low_lbl, "when workload and handoffs are unclear."),
      "Employees consistently request clearer sequencing of priorities and faster decision communication.",
      "Cross-team execution is strongest when accountabilities and checkpoints are explicit."
    )
  }

  tak <- data.frame(
    takeaway_id = c("takeaway_1", "takeaway_2", "takeaway_3"),
    title = c("Leadership and direction clarity", "Cross-team operating flow", "Workload sustainability"),
    narrative = c(
      "Comments emphasize clearer decision context and stronger line-of-sight from strategy to team priorities.",
      "Employees describe uneven handoffs across teams and ask for clearer ownership at boundaries.",
      "Pace and capacity concerns are recurring in lower-scoring cuts, especially where prioritization shifts quickly."
    ),
    rank = 1:3,
    stringsAsFactors = FALSE
  )

  quote_df <- data.frame(
    quote_id = paste0("q_", seq_along(comments)),
    quote_text = comments,
    takeaway_id = vapply(comments, assign_takeaway_id, character(1)),
    theme_title = NA_character_,
    source_tag = "Employee comment",
    stringsAsFactors = FALSE
  )
  quote_df$theme_title <- tak$title[match(quote_df$takeaway_id, tak$takeaway_id)]

  t_counts <- table(quote_df$takeaway_id)
  tak$n_mentions <- as.integer(t_counts[match(tak$takeaway_id, names(t_counts))])
  tak$n_mentions[is.na(tak$n_mentions)] <- 0L
  tak$narrative <- ifelse(
    tak$n_mentions > 0L,
    paste0(tak$narrative, " Mention volume: ", tak$n_mentions, "."),
    tak$narrative
  )

  metric_lookup <- c(takeaway_1 = "Leadership", takeaway_2 = "Communication", takeaway_3 = "Performance development")
  metric_title <- function(id) metric_lookup[[id]] %||% "Purpose"
  theme_rows <- lapply(seq_len(nrow(tak)), function(i) {
    tid <- tak$takeaway_id[[i]]
    mid <- metric_title(tid)
    frow <- fk[tolower(fk$metric_id) == tolower(mid) | tolower(fk$metric_label) == tolower(mid), , drop = FALSE]
    score <- if (nrow(frow) > 0L) as.numeric(frow$score[[1]]) else NA_real_
    status <- if (!is.finite(score)) "No score" else if (score < 3.2) "Area for Growth" else if (score < 3.8) "Industry Standard" else "Above Standard"
    data.frame(
      takeaway_id = tid,
      theme_title = tak$title[[i]],
      context_text = tak$narrative[[i]],
      metric_label = mid,
      metric_value = score,
      metric_status = status,
      stringsAsFactors = FALSE
    )
  })
  theme_df <- do.call(rbind, theme_rows)

  ord <- order(match(quote_df$takeaway_id, tak$takeaway_id))
  quote_df <- quote_df[ord, , drop = FALSE]
  quote_df$score <- NA_real_

  verbatim_df <- data.frame(
    comment_id = paste0("c_", seq_len(nrow(quote_df))),
    takeaway_id = quote_df$takeaway_id,
    fundamental = theme_df$metric_label[match(quote_df$takeaway_id, theme_df$takeaway_id)],
    comment_text = quote_df$quote_text,
    sort_order = seq_len(nrow(quote_df)),
    stringsAsFactors = FALSE
  )

  strengths <- if (nrow(fk) > 0L) paste(utils::head(fk$metric_label, 2L), collapse = ", ") else "higher-scoring fundamentals"
  risks <- if (nrow(fk) > 0L) paste(utils::head(fk$metric_label[order(fk$score)], 2L), collapse = ", ") else "emerging risk areas"
  summary_df <- data.frame(
    title = "Open-Ended Narrative Summary",
    kicker = "Qualitative Insights",
    narrative = paste0(
      "Across open-ended responses, employees report strongest experiences in ", strengths,
      ". The most frequent concerns center on ", risks,
      ". The themes below translate these comments into practical priority areas."
    ),
    overall_score = if (nrow(fk) > 0L) mean(fk$score, na.rm = TRUE) else NA_real_,
    score_label = "Avg Fundamental Score",
    context_note = "Narratives are precomputed from available comment text and grouped into three key themes.",
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_df,
    takeaways = tak[, c("takeaway_id", "title", "narrative", "rank"), drop = FALSE],
    theme_evidence = theme_df,
    quotes = quote_df[, c("quote_id", "takeaway_id", "theme_title", "quote_text", "score", "source_tag"), drop = FALSE],
    verbatim = verbatim_df,
    meta = data.frame(
      filter_label = filter_label(fr),
      n_responses = nrow(rows_curr),
      report_year = report_year,
      stringsAsFactors = FALSE
    )
  )
}

theme_page_count <- function(oe_data, quotes_per_page = 4L) {
  te <- oe_data$theme_evidence
  if (!is.data.frame(te) || nrow(te) < 1L) return(1L)
  total <- 0L
  for (i in seq_len(nrow(te))) {
    rr <- te[i, , drop = FALSE]
    q <- oe_data$quotes[oe_data$quotes$takeaway_id == rr$takeaway_id[[1]], , drop = FALSE]
    if ("theme_title" %in% names(q)) {
      qq <- q[q$theme_title == rr$theme_title[[1]], , drop = FALSE]
      if (nrow(qq) > 0L) q <- qq
    }
    total <- total + max(1L, ceiling(nrow(q) / quotes_per_page))
  }
  total
}

verbatim_page_counts <- function(oe_data, first_cap = 10L, compact_cap = 24L) {
  v <- oe_data$verbatim
  if (!is.data.frame(v) || nrow(v) < 1L) return(list(first = 1L, compact = 0L))
  key <- trimws(as.character(v$takeaway_id))
  key[!nzchar(key)] <- trimws(as.character(v$fundamental[!nzchar(key)]))
  key[!nzchar(key)] <- "all_comments"
  n_by <- as.integer(table(key))
  first_n <- length(n_by)
  compact_n <- sum(pmax(0L, ceiling(pmax(0L, n_by - first_cap) / compact_cap)))
  list(first = max(1L, first_n), compact = compact_n)
}

open_ended_by_filter <- list()
oe_summary_rows <- list()
oe_takeaway_rows <- list()
oe_theme_rows <- list()
oe_quote_rows <- list()
oe_verbatim_rows <- list()

for (i in seq_len(nrow(filter_grid))) {
  fr <- filter_grid[i, , drop = FALSE]
  fid <- fr$filter_id[[1]]
  rows_sub <- apply_filter(user_data, fr)
  rows_curr <- rows_sub[rows_sub$year == report_year, , drop = FALSE]
  oe <- build_open_ended_data(fid, fr, rows_curr)
  open_ended_by_filter[[fid]] <- oe

  oe_summary_rows[[length(oe_summary_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$summary, stringsAsFactors = FALSE)
  oe_takeaway_rows[[length(oe_takeaway_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$takeaways, stringsAsFactors = FALSE)
  oe_theme_rows[[length(oe_theme_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$theme_evidence, stringsAsFactors = FALSE)
  oe_quote_rows[[length(oe_quote_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$quotes, stringsAsFactors = FALSE)
  oe_verbatim_rows[[length(oe_verbatim_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$verbatim, stringsAsFactors = FALSE)
}

open_ended_summary <- do.call(rbind, oe_summary_rows)
open_ended_takeaways <- do.call(rbind, oe_takeaway_rows)
open_ended_theme_evidence <- do.call(rbind, oe_theme_rows)
open_ended_quotes <- do.call(rbind, oe_quote_rows)
open_ended_verbatim <- do.call(rbind, oe_verbatim_rows)

overall_filter_id <- filter_grid$filter_id[filter_grid$department == "All" & filter_grid$full_time_status == "All" & filter_grid$location_cluster == "All"][[1]]
overall_oe <- open_ended_by_filter[[overall_filter_id]]

themes_df <- overall_oe$theme_evidence
themes_df$theme_id <- paste0("theme_", seq_len(nrow(themes_df)))
themes_df$theme_label <- themes_df$theme_title
themes_df$section <- "Thematic Insights"
themes_df$summary <- themes_df$context_text
themes_df$mention_count <- as.integer(table(overall_oe$quotes$takeaway_id)[match(themes_df$takeaway_id, names(table(overall_oe$quotes$takeaway_id)))])
themes_df$mention_count[is.na(themes_df$mention_count)] <- 0L
themes_df$priority_rank <- seq_len(nrow(themes_df))
themes_df <- themes_df[, c("theme_id", "theme_label", "section", "summary", "mention_count", "priority_rank"), drop = FALSE]

quotes_df <- overall_oe$quotes
quotes_df$theme_id <- paste0("theme_", match(quotes_df$takeaway_id, overall_oe$theme_evidence$takeaway_id))
quotes_df$section <- "Qualitative Insights"
quotes_df$sentiment <- "mixed"
quotes_df$source_col <- "workbook_open_ended"
quotes_df <- quotes_df[, c("quote_id", "theme_id", "section", "quote_text", "sentiment", "source_col"), drop = FALSE]

oe_theme_pages_n <- theme_page_count(overall_oe, quotes_per_page = 4L)
oe_verbatim_pages <- verbatim_page_counts(overall_oe, first_cap = 10L, compact_cap = 24L)

theme_slide_ids <- paste0("theme_evidence_", sprintf("%02d", seq_len(oe_theme_pages_n)))
theme_slide_labels <- if (oe_theme_pages_n == 1L) "Theme Evidence" else paste("Theme Evidence", seq_len(oe_theme_pages_n))
verb_first_ids <- paste0("verbatim_first_", sprintf("%02d", seq_len(oe_verbatim_pages$first)))
verb_first_labels <- if (oe_verbatim_pages$first == 1L) "Verbatim (First Page)" else paste("Verbatim Intro", seq_len(oe_verbatim_pages$first))
verb_compact_ids <- if (oe_verbatim_pages$compact > 0L) paste0("verbatim_compact_", sprintf("%02d", seq_len(oe_verbatim_pages$compact))) else character(0)
verb_compact_labels <- if (length(verb_compact_ids) > 0L) paste("Verbatim Continued", seq_len(length(verb_compact_ids))) else character(0)

slides <- data.frame(
  slide_id = c(
    "cover", "orientation_model",
    "survey_overview", "demographics_overview",
    "snapshot", "strengths_risks", "priority_matrix",
    "outcomes_overview", "engagement_deep_dive", "burnout_deep_dive", "work_satisfaction_deep_dive", "enps",
    paste0("fundamental_", slugify(fundamentals)),
    "segment_heatmap", "segment_gaps",
    "open_ended_overall_summary", "open_ended_three_takeaways",
    theme_slide_ids,
    verb_first_ids, verb_compact_ids,
    "actions"
  ),
  slide_label = c(
    "Cover & How To Read", "Orientation Model",
    "Survey Overview", "Demographics",
    "Snapshot", "Strengths & Risks", "Priority Matrix",
    "Outcomes Overview", "Engagement", "Burnout", "Work Satisfaction", "eNPS",
    paste("Fundamental:", fundamentals),
    "Segment Heatmap", "Key Segment Gaps",
    "Overall Narrative Summary", "Three Key Takeaways",
    theme_slide_labels,
    verb_first_labels, verb_compact_labels,
    "Action Priorities"
  ),
  section_id = c(
    "system_orientation", "system_orientation",
    "population_context", "population_context",
    "system_summary", "system_summary", "system_summary",
    "outcomes", "outcomes", "outcomes", "outcomes", "outcomes",
    rep("drivers", length(fundamentals)),
    "segmentation", "segmentation",
    "qualitative", "qualitative",
    rep("qualitative", length(theme_slide_ids)),
    rep("qualitative", length(verb_first_ids) + length(verb_compact_ids)),
    "actions"
  ),
  stringsAsFactors = FALSE
)
slides$sort_order <- seq_len(nrow(slides))

validate_csv(open_ended_summary, c("filter_id", "title", "kicker", "narrative"), "open_ended_summary.csv")
validate_csv(open_ended_takeaways, c("filter_id", "takeaway_id", "title", "narrative"), "open_ended_takeaways.csv")
validate_csv(open_ended_theme_evidence, c("filter_id", "takeaway_id", "theme_title", "context_text"), "open_ended_theme_evidence.csv")
validate_csv(open_ended_quotes, c("filter_id", "quote_id", "takeaway_id", "quote_text"), "open_ended_quotes.csv")
validate_csv(open_ended_verbatim, c("filter_id", "comment_id", "comment_text"), "open_ended_verbatim.csv")
validate_csv(themes_df, c("theme_id", "theme_label", "summary"), "themes.csv")
validate_csv(quotes_df, c("quote_id", "quote_text"), "quotes.csv")

utils::write.csv(open_ended_summary, file.path(data_dir, "open_ended_summary.csv"), row.names = FALSE)
utils::write.csv(open_ended_takeaways, file.path(data_dir, "open_ended_takeaways.csv"), row.names = FALSE)
utils::write.csv(open_ended_theme_evidence, file.path(data_dir, "open_ended_theme_evidence.csv"), row.names = FALSE)
utils::write.csv(open_ended_quotes, file.path(data_dir, "open_ended_quotes.csv"), row.names = FALSE)
utils::write.csv(open_ended_verbatim, file.path(data_dir, "open_ended_verbatim.csv"), row.names = FALSE)
utils::write.csv(themes_df, file.path(data_dir, "themes.csv"), row.names = FALSE)
utils::write.csv(quotes_df, file.path(data_dir, "quotes.csv"), row.names = FALSE)

render_slide <- function(slide_id, fr) {
  fid <- fr$filter_id[[1]]
  rows_sub <- apply_filter(user_data, fr)
  rows_curr <- rows_sub[rows_sub$year == report_year, , drop = FALSE]
  if (nrow(rows_curr) < 8L && !slide_id %in% c("cover", "orientation_model")) {
    return(no_data_slide("Too few responses for this filter combination."))
  }

  k_now <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]

  tryCatch({
    if (slide_id == "cover") {
      return(slide_doc(
        paste0(
          "<div class='slide'><div class='k'>System Orientation</div><h1 class='h1'>", html_escape(report_meta$report_title[[1]]), "</h1>",
          "<p class='sub'>", html_escape(report_meta$report_subtitle[[1]]), "</p>",
          "<p class='sub'><strong>How to use:</strong> This web report is filter-first. Adjust Department, Full-Time Status, and Location Cluster independently to inspect different cuts.</p>",
          "<div class='grid2'>",
          "<div class='card'><p class='sub'>Use sidebar navigation to move by narrative section. Slides are designed as full-page web views and can scroll when content is longer.</p></div>",
          "<div class='card'><p class='sub'><strong>Active Filters:</strong><br>", html_escape(filter_label(fr)), "</p></div>",
          "</div></div>"
        ),
        "Cover"
      ))
    }
    if (slide_id == "orientation_model") {
      md <- build_model_data(fid)
      model_html <- if (is.null(md)) "<div class='card'>Model unavailable for this filter.</div>" else htmltools::renderTags(model_page(model_data = md))$html
      return(slide_doc(
        paste0(
          "<div class='slide'><div class='k'>System Orientation</div><h1 class='h1'>Organizational health model</h1>",
          "<p class='sub'>", gsub("\n", "<br>", html_escape(orientation_text)), "</p>",
          "<div class='card' style='margin-top:14px'>", model_html, "</div></div>"
        ),
        "Orientation Model"
      ))
    }
    if (slide_id == "survey_overview") {
      prior_n <- sum(rows_sub$year == (report_year - 1L))
      return(slide_doc(
        paste0(
          "<div class='slide'><div class='k'>Population Context</div><h1 class='h1'>Survey overview</h1>",
          "<div class='grid3'><div class='card'><div class='s'>Responses (Current Year)</div><div class='m'>", nrow(rows_curr), "</div></div>",
          "<div class='card'><div class='s'>Responses (Prior Year)</div><div class='m'>", prior_n, "</div></div>",
          "<div class='card'><div class='s'>Reporting Year</div><div class='m'>", report_year, "</div></div></div></div>"
        ),
        "Survey Overview"
      ))
    }
    if (slide_id == "demographics_overview") return(build_demographics_page(fid))
    if (slide_id == "snapshot") {
      z <- k_now[k_now$metric_group == "fundamental", , drop = FALSE]
      z <- z[order(-z$score), , drop = FALSE]
      z <- utils::head(z, 6L)
      cards <- paste0("<div class='card'><div class='s'>", html_escape(z$metric_label), "</div><div class='m'>", sprintf("%.2f", z$score), "</div></div>", collapse = "")
      return(slide_doc(paste0("<div class='slide'><div class='k'>System Summary</div><h1 class='h1'>Organizational health snapshot</h1><div class='grid3'>", cards, "</div></div>"), "Snapshot"))
    }
    if (slide_id == "strengths_risks") {
      z <- k_now[k_now$metric_group == "fundamental", , drop = FALSE]
      s <- utils::head(z[order(-z$score), "metric_label"], 3)
      r <- utils::head(z[order(z$score), "metric_label"], 3)
      return(slide_doc(
        paste0(
          "<div class='slide'><div class='k'>System Summary</div><h1 class='h1'>Strengths and risks</h1>",
          "<div class='grid2'><div class='card'><div class='s'>Strengths</div><p class='sub'>", html_escape(paste(s, collapse = ", ")), "</p></div>",
          "<div class='card'><div class='s'>Risks</div><p class='sub'>", html_escape(paste(r, collapse = ", ")), "</p></div></div></div>"
        ),
        "Strengths Risks"
      ))
    }
    if (slide_id == "priority_matrix") {
      pts <- build_matrix_points(fid)
      if (nrow(pts) < 1L) return(no_data_slide())
      return(render_html_obj(decision_matrix_page(points = pts, title = "Priority Matrix", subtitle = filter_label(fr)), "Priority Matrix"))
    }
    if (slide_id == "outcomes_overview") {
      o <- k_now[k_now$metric_group == "outcome", , drop = FALSE]
      cards <- paste0("<div class='card'><div class='s'>", html_escape(o$metric_label), "</div><div class='m'>", ifelse(o$metric_id == "eNPS", sprintf("%+.0f", o$score), sprintf("%.2f", o$score)), "</div></div>", collapse = "")
      return(slide_doc(paste0("<div class='slide'><div class='k'>Outcomes</div><h1 class='h1'>Outcomes overview</h1><div class='grid3'>", cards, "</div></div>"), "Outcomes Overview"))
    }
    if (slide_id %in% c("engagement_deep_dive", "burnout_deep_dive", "work_satisfaction_deep_dive")) {
      lookup <- c(engagement_deep_dive = "Engagement", burnout_deep_dive = "Burnout", work_satisfaction_deep_dive = "Work satisfaction")
      out_label <- lookup[[slide_id]]
      od <- build_outcome_data(out_label, rows_sub, fid)
      if (is.null(od)) return(no_data_slide())
      return(render_html_obj(outcome_page(outcome_data = od), paste(out_label, "Deep Dive")))
    }
    if (slide_id == "enps") {
      en <- suppressWarnings(as.numeric(rows_curr$eNPS))
      en <- en[is.finite(en) & en >= 0 & en <= 10]
      if (length(en) < 5L) return(no_data_slide())
      dist <- data.frame(rating = 0:10, pct = sapply(0:10, function(v) round(100 * sum(en == v) / length(en), 1)))
      score <- round(100 * (sum(en >= 9) - sum(en <= 6)) / length(en))
      drv <- marts$predictive_edges
      drv <- drv[tolower(drv$outcome) == "enps" & tolower(drv$subset) == "all", , drop = FALSE]
      if (nrow(drv) < 1L) {
        drv <- data.frame(
          fundamental = c("Purpose", "Leadership", "Performance development"),
          strength = c(0.42, 0.35, 0.31),
          stringsAsFactors = FALSE
        )
      }
      drv <- drv[order(-abs(as.numeric(drv$strength))), , drop = FALSE]
      drv <- utils::head(drv, 3L)
      fund_now <- k_now[k_now$metric_group == "fundamental", c("metric_id", "score"), drop = FALSE]
      drivers <- lapply(seq_len(nrow(drv)), function(i) {
        f <- as.character(drv$fundamental[[i]])
        sc <- as.numeric(fund_now$score[match(f, fund_now$metric_id)][[1]] %||% NA_real_)
        pct <- if (is.finite(sc)) round((sc / 5) * 100) else NA_real_
        status <- if (!is.finite(pct)) "Industry Standard" else if (pct < 40) "Area for Growth" else if (pct < 60) "Industry Standard" else "Above Standard"
        data.frame(rank = i, fundamental = f, percentile = pct, status_label = status, stringsAsFactors = FALSE)
      })
      drivers_df <- do.call(rbind, drivers)
      return(render_html_obj(enps_page(enps_data = list(
        summary = data.frame(title = "Employee Net Promoter Score (eNPS)", subtitle = filter_label(fr), score = score, score_delta = 0, delta_label = "vs Industry", stringsAsFactors = FALSE),
        distribution = dist,
        drivers = drivers_df
      )), "eNPS"))
    }
    if (grepl("^fundamental_", slide_id)) {
      f <- fundamentals[match(slide_id, paste0("fundamental_", slugify(fundamentals)))]
      if (is.na(f)) return(no_data_slide())
      obj <- fundamental_page(company = "Company A", year = report_year, fundamental = f, marts = marts, filtered_user_data = rows_sub, min_n = 10)
      return(render_html_obj(obj, paste("Fundamental", f)))
    }
    if (slide_id == "segment_heatmap") {
      hm <- build_heatmap()
      if (is.null(hm)) return(no_data_slide())
      return(render_html_obj(heatmap_page(heatmap_data = hm), "Heatmap"))
    }
    if (slide_id == "segment_gaps") {
      overall_id <- filter_grid$filter_id[filter_grid$department == "All" & filter_grid$full_time_status == "All" & filter_grid$location_cluster == "All"][[1]]
      a <- kpi_scores[kpi_scores$filter_id == fid & kpi_scores$metric_group == "fundamental", c("metric_id", "metric_label", "score"), drop = FALSE]
      b <- kpi_scores[kpi_scores$filter_id == overall_id & kpi_scores$metric_group == "fundamental", c("metric_id", "score"), drop = FALSE]
      g <- merge(a, b, by = "metric_id", suffixes = c("_seg", "_overall"), all.x = TRUE)
      g$gap <- g$score_seg - g$score_overall
      g <- g[order(g$gap), , drop = FALSE]
      txt <- paste(utils::head(paste(g$metric_label, sprintf("(%+.2f)", g$gap)), 3), collapse = "; ")
      return(slide_doc(paste0("<div class='slide'><div class='k'>Segmentation</div><h1 class='h1'>Key segment gaps</h1><div class='card'><p class='sub'>Largest negative gaps vs overall: ", html_escape(txt), "</p></div></div>"), "Segment Gaps"))
    }
    if (slide_id == "open_ended_overall_summary") {
      if (nrow(rows_curr) < 10L) return(no_data_slide("Insufficient open-ended sample size for this filter (min n = 10)."))
      oe <- open_ended_by_filter[[fid]]
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "overall_summary", page_index = 1), "Open-Ended Summary"))
    }
    if (slide_id == "open_ended_three_takeaways") {
      if (nrow(rows_curr) < 10L) return(no_data_slide("Insufficient open-ended sample size for this filter (min n = 10)."))
      oe <- open_ended_by_filter[[fid]]
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "three_takeaways", page_index = 1), "Three Takeaways"))
    }
    if (grepl("^theme_evidence_[0-9]{2}$", slide_id)) {
      if (nrow(rows_curr) < 10L) return(no_data_slide("Insufficient open-ended sample size for this filter (min n = 10)."))
      oe <- open_ended_by_filter[[fid]]
      idx <- as.integer(sub("^theme_evidence_", "", slide_id))
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "theme_evidence", page_index = idx), paste("Theme Evidence", idx)))
    }
    if (grepl("^verbatim_first_[0-9]{2}$", slide_id)) {
      if (nrow(rows_curr) < 10L) return(no_data_slide("Insufficient open-ended sample size for this filter (min n = 10)."))
      oe <- open_ended_by_filter[[fid]]
      idx <- as.integer(sub("^verbatim_first_", "", slide_id))
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "verbatim_first", page_index = idx), paste("Verbatim Intro", idx)))
    }
    if (grepl("^verbatim_compact_[0-9]{2}$", slide_id)) {
      if (nrow(rows_curr) < 10L) return(no_data_slide("Insufficient open-ended sample size for this filter (min n = 10)."))
      oe <- open_ended_by_filter[[fid]]
      idx <- as.integer(sub("^verbatim_compact_", "", slide_id))
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "verbatim_compact", page_index = idx), paste("Verbatim Continued", idx)))
    }
    if (slide_id == "actions") {
      cards <- paste0("<div class='card'><div class='s'>Priority ", actions_df$priority_rank, "</div><p class='sub'><strong>", html_escape(actions_df$action_title), "</strong><br>", html_escape(actions_df$rationale), "</p></div>", collapse = "")
      return(slide_doc(paste0("<div class='slide'><div class='k'>Action & Prioritization</div><h1 class='h1'>Priority actions</h1>", cards, "</div>"), "Actions"))
    }
    no_data_slide("Slide not found.")
  }, error = function(e) no_data_slide(conditionMessage(e)))
}

slide_html <- list()
for (s in seq_len(nrow(slides))) {
  sid <- slides$slide_id[[s]]
  for (i in seq_len(nrow(filter_grid))) {
    fr <- filter_grid[i, , drop = FALSE]
    key <- paste0(sid, "::", fr$filter_id)
    slide_html[[key]] <- render_slide(sid, fr)
  }
}

sections_payload <- lapply(seq_len(nrow(section_meta)), function(i) {
  sec <- section_meta[i, , drop = FALSE]
  list(
    section_id = sec$section_id[[1]],
    section_label = sec$section_label[[1]],
    section_order = sec$section_order[[1]],
    slide_ids = as.list(slides$slide_id[slides$section_id == sec$section_id[[1]]])
  )
})

bundle <- list(
  meta = list(
    report_title = report_meta$report_title[[1]],
    report_subtitle = report_meta$report_subtitle[[1]],
    client_label = report_meta$client_label[[1]],
    reporting_year = as.integer(report_meta$reporting_year[[1]])
  ),
  filter_dimensions = list(
    list(id = "department", label = "Department", options = as.list(dim_department)),
    list(id = "full_time_status", label = "Full-Time Status", options = as.list(dim_status)),
    list(id = "location_cluster", label = "Location Cluster", options = as.list(dim_location))
  ),
  sections = sections_payload,
  slides = lapply(seq_len(nrow(slides)), function(i) {
    list(slide_id = slides$slide_id[[i]], slide_label = slides$slide_label[[i]], section_id = slides$section_id[[i]], sort_order = slides$sort_order[[i]])
  }),
  slide_order = as.list(slides$slide_id),
  slide_html = slide_html,
  no_data_html = no_data_slide()
)

bundle_json <- jsonlite::toJSON(bundle, auto_unbox = TRUE, pretty = FALSE, null = "null")
writeLines(paste0("window.OHEP_DEMO_DATA = ", bundle_json, ";"), con = file.path(build_dir, "demo-data.js"), useBytes = TRUE)

file.copy(file.path(app_dir, "index.html"), file.path(build_dir, "index.html"), overwrite = TRUE)
file.copy(file.path(app_dir, "styles.css"), file.path(build_dir, "styles.css"), overwrite = TRUE)
file.copy(file.path(app_dir, "app.js"), file.path(build_dir, "app.js"), overwrite = TRUE)

cat(sprintf("Build complete: %s\n", build_dir))
cat(sprintf("Workbook: %s (sheet: %s)\n", basename(workbook_path), sheet))
cat(sprintf("Reporting year: %d | Filter combinations: %d | Slides: %d | Rendered: %d\n", report_year, nrow(filter_grid), nrow(slides), length(slide_html)))
