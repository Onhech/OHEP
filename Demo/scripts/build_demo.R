#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(devtools)
  library(htmltools)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) < 1L) y else x
first_or <- function(x, y) if (is.null(x) || length(x) < 1L) y else x[[1]]

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

read_required_csv <- function(path, req, name) {
  if (!file.exists(path)) {
    stop(sprintf("Missing required demo overlay file: %s", path), call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  miss <- setdiff(req, names(df))
  if (length(miss) > 0L) {
    stop(sprintf("%s missing required columns: %s", name, paste(miss, collapse = ", ")), call. = FALSE)
  }
  if (nrow(df) < 1L) {
    stop(sprintf("%s is empty.", name), call. = FALSE)
  }
  df
}

as_flag <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
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

company_levels <- c(
  "Federal",
  "Industrial Services",
  "Financial Solutions",
  "Petro Star",
  "Energy Services",
  "Construction & North Slope Operations"
)
department_levels <- c(
  "Operations",
  "Engineering & Technical",
  "Maintenance & Reliability",
  "Projects & Construction",
  "Health, Safety & Environment (HSE)",
  "Supply Chain & Logistics",
  "Corporate Functions",
  "Program / Contract Management"
)
identity_levels <- c(
  "I\u00f1upiaq",
  "Other Alaska Native",
  "Other Indigenous (First Nations / Native American)",
  "White",
  "Asian",
  "Black / African American",
  "Hispanic / Latino",
  "Two or more / Other",
  "Prefer not to say"
)
location_levels <- c(
  "North Slope (Prudhoe Bay / Kuparuk)",
  "Utqia\u0121vik (Barrow)",
  "Anchorage",
  "Other Alaska",
  "Lower 48 (U.S.)",
  "Remote / Other"
)
employee_type_levels <- c(
  "Field / Site",
  "Corporate / Office",
  "Federal Contract Staff",
  "Craft / Trades",
  "Manager / Supervisor"
)
tenure_band_levels <- c("<1 year", "1-2 years", "3-5 years", "6-10 years", "10+ years")
work_arrangement_levels <- c("On-site", "Camp / Rotational", "Hybrid", "Remote")

map_by_levels <- function(values, levels) {
  u <- sort(unique(values))
  idx <- ((seq_along(u) - 1L) %% length(levels)) + 1L
  mp <- setNames(levels[idx], u)
  unname(mp[values])
}

raw$company_filter <- map_by_levels(raw$department_anon, company_levels)
raw$department_filter <- map_by_levels(raw$department_anon, department_levels)
raw$location_filter <- map_by_levels(raw$location_anon, location_levels)

is_manager <- grepl("manager|supervisor|lead|foreman|executive", tolower(raw$position_raw))
is_field_like <- grepl("operations|maintenance|reliability|construction|drilling|completions|hse|supply|logistics", tolower(raw$department_anon))
rand_u <- runif(nrow(raw))
raw$employee_type <- ifelse(
  is_manager, "Manager / Supervisor",
  ifelse(
    is_field_like & rand_u < 0.45, "Field / Site",
    ifelse(
      is_field_like & rand_u < 0.75, "Craft / Trades",
      ifelse(rand_u < 0.88, "Corporate / Office", "Federal Contract Staff")
    )
  )
)

raw$tenure_band <- map_by_levels(raw$tenure_anon, tenure_band_levels)

raw$work_arrangement <- ifelse(
  raw$employee_type %in% c("Field / Site", "Craft / Trades"),
  ifelse(runif(nrow(raw)) < 0.58, "Camp / Rotational", "On-site"),
  ifelse(
    raw$employee_type == "Corporate / Office",
    ifelse(runif(nrow(raw)) < 0.55, "Hybrid", "On-site"),
    ifelse(runif(nrow(raw)) < 0.45, "Remote", "On-site")
  )
)

id_probs <- c(0.19, 0.13, 0.08, 0.34, 0.09, 0.05, 0.06, 0.04, 0.02)
raw$identity_filter <- sample(identity_levels, size = nrow(raw), replace = TRUE, prob = id_probs)

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
  demo_company = raw$company_filter,
  demo_department = raw$department_filter,
  demo_identity = raw$identity_filter,
  demo_location = raw$location_filter,
  employeeGroup = raw$employee_type,
  demo_employee_type = raw$employee_type,
  demo_position = raw$position_anon,
  demo_tenure = raw$tenure_band,
  demo_work_arrangement = raw$work_arrangement,
  eNPS = enps_values,
  stringsAsFactors = FALSE
)
for (nm in names(user_items)) user_data[[nm]] <- user_items[[nm]]
if (length(comment_cols) > 0L) {
  comment_question_labels <- setNames(normalize_text(comment_cols), paste0("comment_", seq_along(comment_cols)))
  for (i in seq_along(comment_cols)) {
    ccol <- comment_cols[[i]]
    user_data[[paste0("comment_", i)]] <- normalize_text(raw[[ccol]])
  }
} else {
  comment_question_labels <- character(0)
}

year_counts <- sort(table(user_data$year), decreasing = TRUE)
history_cfg <- read_required_csv(
  file.path(data_dir, "demo_history_config.csv"),
  c("year", "display_label", "include_in_history", "use_as_report_year", "min_n"),
  "demo_history_config.csv"
)
history_cfg$year <- suppressWarnings(as.integer(history_cfg$year))
history_cfg$min_n <- suppressWarnings(as.integer(history_cfg$min_n))
history_cfg$include_in_history <- as_flag(history_cfg$include_in_history)
history_cfg$use_as_report_year <- as_flag(history_cfg$use_as_report_year)
history_cfg <- history_cfg[history_cfg$year %in% as.integer(names(year_counts)), , drop = FALSE]
history_cfg <- history_cfg[order(history_cfg$year), , drop = FALSE]
if (nrow(history_cfg) < 1L) {
  stop("demo_history_config.csv does not match any workbook years.", call. = FALSE)
}
eligible_history <- history_cfg[
  history_cfg$include_in_history &
    year_counts[as.character(history_cfg$year)] >= history_cfg$min_n,
  ,
  drop = FALSE
]
if (nrow(eligible_history) < 1L) {
  stop("No workbook years satisfy demo_history_config.csv inclusion rules.", call. = FALSE)
}
report_candidates <- eligible_history[eligible_history$use_as_report_year, , drop = FALSE]
report_year <- if (nrow(report_candidates) > 0L) {
  max(report_candidates$year, na.rm = TRUE)
} else {
  max(eligible_history$year, na.rm = TRUE)
}
history_years <- eligible_history$year[eligible_history$year <= report_year]
prior_year <- suppressWarnings(max(history_years[history_years < report_year], na.rm = TRUE))
if (!is.finite(prior_year)) prior_year <- NA_integer_
history_year_labels <- setNames(as.character(history_cfg$display_label), as.character(history_cfg$year))

report_copy <- read_required_csv(
  file.path(data_dir, "demo_report_copy.csv"),
  c("report_title", "report_subtitle", "client_label", "summary_text", "orientation_text", "historical_context", "how_to_use_text"),
  "demo_report_copy.csv"
)

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
dim_company <- c("All", company_levels)
dim_department <- c("All", department_levels)
dim_identity <- c("All", identity_levels)
dim_location <- c("All", location_levels)
dim_employee_type <- c("All", employee_type_levels)
dim_tenure <- c("All", tenure_band_levels)
dim_work_arrangement <- c("All", work_arrangement_levels)

filter_ids <- c("company", "department", "identity", "location", "employee_type", "tenure", "work_arrangement")
dim_options <- list(
  company = dim_company,
  department = dim_department,
  identity = dim_identity,
  location = dim_location,
  employee_type = dim_employee_type,
  tenure = dim_tenure,
  work_arrangement = dim_work_arrangement
)

all_row <- as.data.frame(setNames(as.list(rep("All", length(filter_ids))), filter_ids), stringsAsFactors = FALSE)
single_rows <- do.call(
  rbind,
  lapply(filter_ids, function(fid) {
    vals <- setdiff(dim_options[[fid]], "All")
    if (length(vals) < 1L) return(NULL)
    out <- do.call(
      rbind,
      lapply(vals, function(v) {
        rr <- as.list(rep("All", length(filter_ids)))
        names(rr) <- filter_ids
        rr[[fid]] <- v
        as.data.frame(rr, stringsAsFactors = FALSE)
      })
    )
    out
  })
)

observed_rows <- unique(rows_report[, c("demo_company", "demo_department", "demo_identity", "demo_location", "demo_employee_type", "demo_tenure", "demo_work_arrangement"), drop = FALSE])
names(observed_rows) <- filter_ids

filter_grid <- unique(rbind(all_row, single_rows, observed_rows))
filter_grid$filter_id <- apply(filter_grid, 1, function(rw) {
  paste(vapply(filter_ids, function(fid) paste0(fid, "=", slugify(rw[[fid]])), character(1)), collapse = "|")
})

apply_filter <- function(df, row) {
  out <- df
  if (row$company != "All") out <- out[out$demo_company == row$company, , drop = FALSE]
  if (row$department != "All") out <- out[out$demo_department == row$department, , drop = FALSE]
  if (row$identity != "All") out <- out[out$demo_identity == row$identity, , drop = FALSE]
  if (row$location != "All") out <- out[out$demo_location == row$location, , drop = FALSE]
  if (row$employee_type != "All") out <- out[out$demo_employee_type == row$employee_type, , drop = FALSE]
  if (row$tenure != "All") out <- out[out$demo_tenure == row$tenure, , drop = FALSE]
  if (row$work_arrangement != "All") out <- out[out$demo_work_arrangement == row$work_arrangement, , drop = FALSE]
  out
}

fundamentals <- unique(item_data$fundamental_id[item_data$type == "fundamental"])
fundamentals <- fundamentals[nzchar(fundamentals)]

orientation_text <- as.character(report_copy$orientation_text[[1]])
historical_context_text <- as.character(report_copy$historical_context[[1]])
how_to_use_text <- as.character(report_copy$how_to_use_text[[1]])

html_escape <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

slide_doc <- function(inner, title = "OHEP Demo") {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", html_escape(title), "</title>",
    "<style>",
    "html,body{margin:0;padding:0;background:#f1f5f9;color:#0f172a;font-family:'Avenir Next','Segoe UI',sans-serif;}",
    ".slide{padding:24px 20px 30px 20px;max-width:1380px;margin:0 auto;}",
    ".k{font-size:12px;text-transform:uppercase;letter-spacing:.08em;color:#0f8694;font-weight:700;margin-bottom:8px;}",
    ".h1{font-size:34px;line-height:1.15;margin:0 0 10px 0;}",
    ".sub{font-size:17px;color:#4b5563;line-height:1.45;}",
    ".grid2{display:grid;grid-template-columns:1fr 1fr;gap:14px;}",
    ".grid3{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;}",
    ".card{background:#fff;border:1px solid #d9e2eb;border-radius:12px;padding:14px;}",
    ".m{font-size:30px;font-weight:700;}",
    ".s{font-size:12px;color:#667085;}",
    ".q{font-size:16px;line-height:1.4;font-style:italic;}",
    ".widget-wrap{display:flex;justify-content:center;align-items:flex-start;width:100%;}",
    ".page-shell{background:#fff;border:1px solid #d9e2eb;border-radius:16px;padding:28px 30px;box-shadow:0 10px 30px rgba(15,23,42,.08);}",
    ".page-header{margin-bottom:20px;}",
    ".page-header-top{display:flex;justify-content:space-between;align-items:flex-end;gap:16px;}",
    ".page-title-group{display:flex;flex-direction:column;min-width:0;}",
    ".page-header .k{margin-bottom:10px;}",
    ".page-title{font-size:34px;line-height:1.1;font-weight:900;letter-spacing:-.03em;color:#0f172a;margin:0;}",
    ".page-divider{width:100%;height:2px;background:#0f8694;margin:14px 0 14px;}",
    ".page-description{font-size:15px;line-height:1.55;color:#475569;max-width:1080px;}",
    ".page-controls{display:flex;justify-content:flex-end;align-items:center;gap:12px;flex-wrap:wrap;}",
    ".page-control-group{display:inline-flex;align-items:center;gap:10px;}",
    ".page-control-label{font-size:12px;font-weight:800;color:#64748B;text-transform:uppercase;letter-spacing:.5px;}",
    ".page-control-select{appearance:none;border:1px solid #CBD5E1;border-radius:8px;background:#FFFFFF;color:#0F172A;font-size:13px;font-weight:700;padding:8px 34px 8px 12px;min-width:200px;outline:none;cursor:pointer;box-shadow:0 1px 2px rgba(15,23,42,.02);background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 24 24' stroke='%2364748B'%3E%3Cpath stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M19 9l-7 7-7-7'/%3E%3C/svg%3E\");background-repeat:no-repeat;background-position:right 10px center;background-size:14px;}",
    ".page-control-select:focus{border-color:#0D9488;box-shadow:0 0 0 3px rgba(13,148,136,.15);}",
    ".page-widget{width:100%;}",
    ".page-widget > .widget-wrap{justify-content:flex-start;}",
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
  slide_doc(paste0("<div class='widget-wrap'>", tags$html, "</div>"), title)
}

render_html_fragment <- function(obj) {
  tags <- htmltools::renderTags(obj)
  paste0("<div class='widget-wrap'>", tags$html, "</div>")
}

page_header_html <- function(eyebrow, title, description = NULL, controls_html = NULL) {
  desc_html <- if (!is.null(description) && nzchar(trimws(description))) {
    paste0("<p class='page-description'>", html_escape(description), "</p>")
  } else {
    ""
  }
  controls_node <- if (!is.null(controls_html) && nzchar(trimws(controls_html))) {
    paste0("<div class='page-controls'>", controls_html, "</div>")
  } else {
    ""
  }
  paste0(
    "<header class='page-header'>",
    "<div class='page-header-top'>",
    "<div class='page-title-group'>",
    "<div class='k'>", html_escape(eyebrow), "</div>",
    "<h1 class='page-title'>", html_escape(title), "</h1>",
    "</div>",
    controls_node,
    "</div>",
    "<div class='page-divider'></div>",
    desc_html,
    "</header>"
  )
}

widget_shell_doc <- function(obj, title, eyebrow, description = NULL, controls_html = NULL) {
  slide_doc(
    paste0(
      "<div class='slide'><div class='page-shell'>",
      page_header_html(eyebrow, title, description, controls_html),
      "<div class='page-widget'>", render_html_fragment(obj), "</div>",
      "</div></div>"
    ),
    title
  )
}

kpi_history_rows <- list()
fundamental_history_rows <- list()
outcome_history_rows <- list()
item_rows_out <- list()
demo_panel_rows <- list()

score_value <- function(value, min_scale, max_scale, reverse) {
  val <- suppressWarnings(as.numeric(value))
  if (!is.finite(val)) return(NA_real_)
  if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
    min_scale <- 1
    max_scale <- 5
  }
  if (isTRUE(reverse)) min_scale + max_scale - val else val
}

outcome_meta <- item_data[item_data$type == "outcome", c("item_id", "fundamental_id", "is_reverse_scored", "response_scale_min", "response_scale_max"), drop = FALSE]

for (i in seq_len(nrow(filter_grid))) {
  fr <- filter_grid[i, , drop = FALSE]
  rows_sub <- apply_filter(user_data, fr)
  fid <- fr$filter_id[[1]]

  for (yr in history_years) {
    rows_year <- rows_sub[rows_sub$year == yr, , drop = FALSE]
    if (nrow(rows_year) < 1L) next

    if ("eNPS" %in% names(rows_year)) {
      en <- suppressWarnings(as.numeric(rows_year$eNPS))
      en <- en[is.finite(en)]
      if (length(en) > 0L) {
        enps_score <- 100 * (sum(en >= 9) - sum(en <= 6)) / length(en)
        kpi_history_rows[[length(kpi_history_rows) + 1L]] <- data.frame(
          filter_id = fid,
          company = "Company A",
          year = yr,
          metric_group = "outcome",
          metric_id = "eNPS",
          metric_label = "eNPS",
          score = enps_score,
          percentile = pmax(1, pmin(99, round(50 + enps_score / 2))),
          n = length(en),
          stringsAsFactors = FALSE
        )
        outcome_history_rows[[length(outcome_history_rows) + 1L]] <- data.frame(
          filter_id = fid,
          year = yr,
          outcome_id = "eNPS",
          outcome_label = "eNPS",
          score = enps_score,
          percentile = pmax(1, pmin(99, round(50 + enps_score / 2))),
          n = length(en),
          stringsAsFactors = FALSE
        )
      }
    }

    if (nrow(outcome_meta) > 0L) {
      out_vals <- list()
      for (j in seq_len(nrow(outcome_meta))) {
        iid <- as.character(outcome_meta$item_id[[j]])
        if (!iid %in% names(rows_year)) next
        scored <- vapply(
          rows_year[[iid]],
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
        out_df <- out_df[is.finite(suppressWarnings(as.numeric(out_df$score))) & nzchar(trimws(as.character(out_df$outcome))), , drop = FALSE]
        if (!is.data.frame(out_df) || nrow(out_df) < 1L) {
          next
        }
        out_agg <- stats::aggregate(score ~ outcome, data = out_df, FUN = function(x) mean(x, na.rm = TRUE))
        out_n <- stats::aggregate(score ~ outcome, data = out_df, FUN = function(x) sum(is.finite(x)))
        names(out_n)[2] <- "n"
        out_agg <- merge(out_agg, out_n, by = "outcome", all.x = TRUE, sort = FALSE)
        for (j in seq_len(nrow(out_agg))) {
          pct <- pmax(1, pmin(99, round((as.numeric(out_agg$score[[j]]) / 5) * 100)))
          row_out <- data.frame(
            filter_id = fid,
            company = "Company A",
            year = yr,
            metric_group = "outcome",
            metric_id = as.character(out_agg$outcome[[j]]),
            metric_label = as.character(out_agg$outcome[[j]]),
            score = as.numeric(out_agg$score[[j]]),
            percentile = pct,
            n = as.integer(out_agg$n[[j]]),
            stringsAsFactors = FALSE
          )
          kpi_history_rows[[length(kpi_history_rows) + 1L]] <- row_out
          outcome_history_rows[[length(outcome_history_rows) + 1L]] <- data.frame(
            filter_id = fid,
            year = yr,
            outcome_id = row_out$metric_id,
            outcome_label = row_out$metric_label,
            score = row_out$score,
            percentile = row_out$percentile,
            n = row_out$n,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    for (f in fundamentals) {
      dash <- tryCatch(
        build_dashboard_data_from_filtered_user_data(
          company = "Company A",
          year = yr,
          fundamental = f,
          marts = marts,
          filtered_user_data = rows_sub,
          min_n = 10
        ),
        error = function(e) NULL
      )
      if (is.null(dash)) next
      fs <- suppressWarnings(as.numeric(first_or(dash$fundamental$score, NA_real_)))
      fp <- suppressWarnings(as.numeric(first_or(dash$fundamental$percentile, NA_real_)))
      current_n <- if (is.list(dash$privacy) && !is.null(dash$privacy$current_n)) as.integer(dash$privacy$current_n[[1]]) else nrow(rows_year)
      if (is.finite(fs)) {
        row_f <- data.frame(
          filter_id = fid,
          company = "Company A",
          year = yr,
          metric_group = "fundamental",
          metric_id = f,
          metric_label = f,
          score = fs,
          percentile = fp,
          n = current_n,
          stringsAsFactors = FALSE
        )
        kpi_history_rows[[length(kpi_history_rows) + 1L]] <- row_f
        fundamental_history_rows[[length(fundamental_history_rows) + 1L]] <- data.frame(
          filter_id = fid,
          year = yr,
          fundamental_id = f,
          fundamental_label = f,
          score = fs,
          percentile = fp,
          n = current_n,
          stringsAsFactors = FALSE
        )
      }
      if (yr == report_year && is.data.frame(dash$items) && nrow(dash$items) > 0L) {
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
  }

  rows_curr <- rows_sub[rows_sub$year == report_year, , drop = FALSE]
  if (nrow(rows_curr) < 1L) next
  for (slot in c("tl", "tr", "bl", "br")) {
    col <- switch(slot, tl = "demo_company", tr = "demo_location", bl = "demo_employee_type", br = "demo_tenure")
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

kpi_scores_history <- if (length(kpi_history_rows) > 0L) do.call(rbind, kpi_history_rows) else data.frame()
fundamental_history <- if (length(fundamental_history_rows) > 0L) do.call(rbind, fundamental_history_rows) else data.frame()
outcome_history <- if (length(outcome_history_rows) > 0L) do.call(rbind, outcome_history_rows) else data.frame()
kpi_scores <- kpi_scores_history[kpi_scores_history$year == report_year, , drop = FALSE]
item_scores <- if (length(item_rows_out) > 0L) do.call(rbind, item_rows_out) else data.frame()
demographics_panels <- if (length(demo_panel_rows) > 0L) do.call(rbind, demo_panel_rows) else data.frame()

if (nrow(kpi_scores) < 1L || nrow(item_scores) < 1L || nrow(demographics_panels) < 1L) {
  stop("Generated normalized tables are empty after filtering.", call. = FALSE)
}

heatmap_values <- data.frame()
for (f in fundamentals) {
  for (dep in dim_department[dim_department != "All"]) {
    key <- filter_grid$filter_id[filter_grid$department == dep & filter_grid$company == "All" & filter_grid$identity == "All" & filter_grid$location == "All" & filter_grid$employee_type == "All" & filter_grid$tenure == "All" & filter_grid$work_arrangement == "All"][[1]]
    score <- kpi_scores$score[kpi_scores$filter_id == key & kpi_scores$metric_group == "fundamental" & kpi_scores$metric_id == f]
    heatmap_values <- rbind(
      heatmap_values,
      data.frame(
        segment_type_id = "department",
        row_label = f,
        segment_value_id = slugify(dep),
        segment_value_label = dep,
        value = as.numeric(first_or(score, NA_real_)),
        stringsAsFactors = FALSE
      )
    )
  }
}

report_meta <- data.frame(
  report_title = as.character(report_copy$report_title[[1]]),
  report_subtitle = paste0(as.character(report_copy$report_subtitle[[1]]), " | Reporting year ", report_year),
  client_label = as.character(report_copy$client_label[[1]]),
  reporting_year = report_year,
  summary_text = as.character(report_copy$summary_text[[1]]),
  stringsAsFactors = FALSE
)

report_meta$report_title <- gsub("NorthRiver Energy", "Arctic Slope", report_meta$report_title, fixed = TRUE)
report_meta$report_subtitle <- gsub("NorthRiver Energy", "Arctic Slope", report_meta$report_subtitle, fixed = TRUE)
report_meta$client_label <- gsub("NorthRiver Energy", "Arctic Slope", report_meta$client_label, fixed = TRUE)
if (!nzchar(trimws(as.character(report_meta$client_label[[1]])))) {
  report_meta$client_label[[1]] <- "Arctic Slope"
}

segment_options <- rbind(
  data.frame(segment_type_id = "company", segment_value_id = slugify(dim_company), segment_type_label = "Company", segment_value_label = dim_company, sort_order = seq_along(dim_company), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "department", segment_value_id = slugify(dim_department), segment_type_label = "Department", segment_value_label = dim_department, sort_order = seq_along(dim_department), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "identity", segment_value_id = slugify(dim_identity), segment_type_label = "Identity", segment_value_label = dim_identity, sort_order = seq_along(dim_identity), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "location", segment_value_id = slugify(dim_location), segment_type_label = "Location", segment_value_label = dim_location, sort_order = seq_along(dim_location), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "employee_type", segment_value_id = slugify(dim_employee_type), segment_type_label = "Employee Type", segment_value_label = dim_employee_type, sort_order = seq_along(dim_employee_type), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "tenure", segment_value_id = slugify(dim_tenure), segment_type_label = "Tenure (bands)", segment_value_label = dim_tenure, sort_order = seq_along(dim_tenure), n = NA_integer_, stringsAsFactors = FALSE),
  data.frame(segment_type_id = "work_arrangement", segment_value_id = slugify(dim_work_arrangement), segment_type_label = "Work Arrangement", segment_value_label = dim_work_arrangement, sort_order = seq_along(dim_work_arrangement), n = NA_integer_, stringsAsFactors = FALSE)
)

validate_csv <- function(df, req, name) {
  miss <- setdiff(req, names(df))
  if (length(miss) > 0L) stop(sprintf("%s missing required columns: %s", name, paste(miss, collapse = ", ")), call. = FALSE)
  if (nrow(df) < 1L) stop(sprintf("%s is empty.", name), call. = FALSE)
}

validate_csv(report_meta, c("report_title", "report_subtitle", "client_label", "reporting_year", "summary_text"), "report_meta.csv")
validate_csv(segment_options, c("segment_type_id", "segment_value_id", "segment_type_label", "segment_value_label"), "segment_options.csv")
validate_csv(kpi_scores, c("filter_id", "metric_group", "metric_id", "score"), "kpi_scores.csv")
validate_csv(kpi_scores_history, c("filter_id", "metric_group", "metric_id", "year", "score", "percentile", "n"), "kpi_scores_history.csv")
validate_csv(outcome_history, c("filter_id", "outcome_id", "year", "score", "percentile", "n"), "outcome_history.csv")
validate_csv(fundamental_history, c("filter_id", "fundamental_id", "year", "score", "percentile", "n"), "fundamental_history.csv")
validate_csv(item_scores, c("filter_id", "fundamental_id", "item_label", "mean"), "item_scores.csv")
validate_csv(demographics_panels, c("filter_id", "panel_slot", "category", "pct"), "demographics_panels.csv")
validate_csv(heatmap_values, c("segment_type_id", "row_label", "segment_value_label", "value"), "heatmap_values.csv")

utils::write.csv(report_meta, file.path(data_dir, "report_meta.csv"), row.names = FALSE)
utils::write.csv(segment_options, file.path(data_dir, "segment_options.csv"), row.names = FALSE)
utils::write.csv(kpi_scores, file.path(data_dir, "kpi_scores.csv"), row.names = FALSE)
utils::write.csv(kpi_scores_history, file.path(data_dir, "kpi_scores_history.csv"), row.names = FALSE)
utils::write.csv(outcome_history, file.path(data_dir, "outcome_history.csv"), row.names = FALSE)
utils::write.csv(fundamental_history, file.path(data_dir, "fundamental_history.csv"), row.names = FALSE)
utils::write.csv(item_scores, file.path(data_dir, "item_scores.csv"), row.names = FALSE)
utils::write.csv(demographics_panels, file.path(data_dir, "demographics_panels.csv"), row.names = FALSE)
utils::write.csv(heatmap_values, file.path(data_dir, "heatmap_values.csv"), row.names = FALSE)

build_demographics_page <- function(fid) {
  x <- demographics_panels[demographics_panels$filter_id == fid, , drop = FALSE]
  if (nrow(x) < 1L) return(no_data_slide())
  mk <- function(slot, ttl) {
    p <- x[x$panel_slot == slot, , drop = FALSE]
    if (nrow(p) < 1L) return(NULL)
    demo_categorical_bar(data = data.frame(category = p$category, value = p$pct, stringsAsFactors = FALSE), title = ttl, subtitle = "", sort_desc = TRUE)
  }
  widget_shell_doc(demographics_page(
    tl = mk("tl", "Department"),
    tr = mk("tr", "Location Cluster"),
    bl = mk("bl", "Full-Time Status"),
    br = mk("br", "Tenure"),
    title = "",
    subtitle = ""
  ), title = "Demographics", eyebrow = "OHEP Summary", description = NULL)
}

metric_history_pair <- function(df, fid, id_col, metric_id) {
  current_row <- df[df$filter_id == fid & df[[id_col]] == metric_id & df$year == report_year, , drop = FALSE]
  prior_row <- if (is.finite(prior_year)) df[df$filter_id == fid & df[[id_col]] == metric_id & df$year == prior_year, , drop = FALSE] else df[0, , drop = FALSE]
  list(current = current_row, prior = prior_row)
}

fmt_delta_text <- function(delta, digits = 2L) {
  if (!is.finite(delta) || delta == 0) return("Flat vs prior")
  fmt <- if (digits <= 0L) "%+d" else paste0("%+.", digits, "f")
  paste0(sprintf(fmt, if (digits <= 0L) round(delta) else delta), " vs ", if (is.finite(prior_year)) prior_year else "prior")
}

history_year_label <- function(year_value) {
  lbl <- history_year_labels[[as.character(year_value)]]
  if (is.null(lbl) || !nzchar(lbl)) as.character(year_value) else lbl
}

build_model_data <- function(fid) {
  k <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]
  f <- k[k$metric_group == "fundamental", , drop = FALSE]
  o <- k[k$metric_group == "outcome", , drop = FALSE]
  if (nrow(f) < 1L) return(NULL)
  f_delta <- vapply(f$metric_id, function(mid) {
    hist <- metric_history_pair(fundamental_history, fid, "fundamental_id", mid)
    cur <- suppressWarnings(as.numeric(first_or(hist$current$score, NA_real_)))
    prv <- suppressWarnings(as.numeric(first_or(hist$prior$score, NA_real_)))
    if (is.finite(cur) && is.finite(prv)) round(cur - prv, 2) else 0
  }, numeric(1))
  o_prior_pct <- vapply(o$metric_id, function(mid) {
    hist <- metric_history_pair(outcome_history, fid, "outcome_id", mid)
    prv <- suppressWarnings(as.numeric(first_or(hist$prior$score, NA_real_)))
    if (!is.finite(prv)) return(NA_real_)
    if (tolower(mid) == "enps") pmax(1, pmin(99, 50 + prv / 2)) else pmax(1, pmin(99, round((prv / 5) * 100)))
  }, numeric(1))
  o_delta <- vapply(o$metric_id, function(mid) {
    hist <- metric_history_pair(outcome_history, fid, "outcome_id", mid)
    cur <- suppressWarnings(as.numeric(first_or(hist$current$score, NA_real_)))
    prv <- suppressWarnings(as.numeric(first_or(hist$prior$score, NA_real_)))
    if (is.finite(cur) && is.finite(prv)) round(cur - prv, 2) else 0
  }, numeric(1))
  list(
    summary = data.frame(title = "Organizational Health Model", subtitle = "", fundamentals_label = "Fundamentals", outcomes_label = "Outcomes", raw_avg_label = "Score", delta_label = paste("vs", history_year_label(if (is.finite(prior_year)) prior_year else (report_year - 1L))), stringsAsFactors = FALSE),
    fundamentals = data.frame(label = f$metric_label, percentile = pmax(1, pmin(99, round((f$score / 5) * 100))), raw_avg = round(f$score, 2), delta = f_delta, shape = "circle", stringsAsFactors = FALSE),
    outcomes = data.frame(
      label = o$metric_label,
      percentile = pmax(1, pmin(99, ifelse(o$metric_id == "eNPS", 50 + o$score / 2, round((o$score / 5) * 100)))),
      prior_percentile = o_prior_pct,
      raw_avg = round(o$score, 2),
      delta = o_delta,
      shape = "diamond",
      stringsAsFactors = FALSE
    )
  )
}

build_matrix_points <- function(fid) {
  f <- kpi_scores[kpi_scores$filter_id == fid & kpi_scores$metric_group == "fundamental", , drop = FALSE]
  if (nrow(f) < 1L) return(data.frame())

  # Demo-optimized positioning to ensure clear spread across all quadrants.
  # Scores are x-axis coordinates (vs benchmark), impacts are y-axis values by outcome.
  point_map <- data.frame(
    fundamental = c(
      "Purpose",
      "Strategy",
      "Leadership",
      "Communication",
      "Learning & innovation",
      "Respect, care, and trust",
      "Performance development",
      "Safety"
    ),
    score = c(0.42, -0.55, 0.68, -0.70, 0.90, 0.35, -0.35, 0.72),
    impact_overall = c(0.78, 0.85, 0.90, 0.72, 0.30, 0.40, 0.32, 0.25),
    impact_engagement = c(0.82, 0.76, 0.91, 0.68, 0.34, 0.44, 0.35, 0.24),
    impact_burnout = c(0.63, 0.88, 0.74, 0.81, 0.27, 0.36, 0.28, 0.22),
    impact_work_satisfaction = c(0.79, 0.69, 0.86, 0.66, 0.33, 0.38, 0.30, 0.23),
    impact_enps = c(0.84, 0.73, 0.92, 0.70, 0.29, 0.42, 0.31, 0.26),
    stringsAsFactors = FALSE
  )

  x <- merge(
    f,
    point_map,
    by.x = "metric_id",
    by.y = "fundamental",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("_kpi", "")
  )

  # Backfill any unmatched fundamentals into the lower-right "maintain" space.
  x$score[!is.finite(x$score)] <- 0.65
  x$impact_overall[!is.finite(x$impact_overall)] <- 0.30
  x$impact_engagement[!is.finite(x$impact_engagement)] <- 0.30
  x$impact_burnout[!is.finite(x$impact_burnout)] <- 0.30
  x$impact_work_satisfaction[!is.finite(x$impact_work_satisfaction)] <- 0.30
  x$impact_enps[!is.finite(x$impact_enps)] <- 0.30

  data.frame(
    fundamental = x$metric_id,
    label = x$metric_label,
    score = x$score,
    impact = x$impact_overall,
    impact_engagement = x$impact_engagement,
    impact_burnout = x$impact_burnout,
    impact_work_satisfaction = x$impact_work_satisfaction,
    impact_enps = x$impact_enps,
    stringsAsFactors = FALSE
  )
}

build_heatmap <- function() {
  short_label <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("Health, Safety & Environment \\(HSE\\)", "HSE", x, fixed = FALSE)
    x <- gsub("North Slope \\(Prudhoe Bay / Kuparuk\\)", "North Slope", x, fixed = FALSE)
    x <- gsub("Corporate / Office", "Corporate", x, fixed = TRUE)
    x <- gsub("Manager / Supervisor", "Manager", x, fixed = TRUE)
    if (nchar(x) > 22L) x <- paste0(substr(x, 1L, 19L), "...")
    x
  }

  default_all_row <- filter_grid[
    filter_grid$company == "All" &
      filter_grid$department == "All" &
      filter_grid$identity == "All" &
      filter_grid$location == "All" &
      filter_grid$employee_type == "All" &
      filter_grid$tenure == "All" &
      filter_grid$work_arrangement == "All",
    ,
    drop = FALSE
  ]
  overall_key <- if (nrow(default_all_row) > 0L) as.character(default_all_row$filter_id[[1]]) else NA_character_
  overall_scores <- setNames(
    sapply(fundamentals, function(fu) {
      if (!is.character(overall_key) || !nzchar(overall_key)) return(NA_real_)
      v <- kpi_scores$score[
        kpi_scores$filter_id == overall_key &
          kpi_scores$metric_group == "fundamental" &
          kpi_scores$metric_id == fu
      ]
      as.numeric(first_or(v, NA_real_))
    }),
    fundamentals
  )

  fake_score <- function(segment_id, seg_val, fundamental_id) {
    base <- as.numeric(overall_scores[[fundamental_id]])
    if (!is.finite(base)) base <- 4.15
    h <- sum(utf8ToInt(paste(segment_id, seg_val, fundamental_id, sep = "|")))
    jitter <- ((h %% 41L) - 20L) / 100
    round(max(3.2, min(4.9, base + jitter)), 2)
  }

  build_table_for <- function(segment_id, segment_values) {
    vals <- setdiff(segment_values, "All")
    tab <- data.frame(Category = fundamentals, stringsAsFactors = FALSE, check.names = FALSE)
    if (length(vals) < 1L) return(tab)

    for (seg_val in vals) {
      key_rows <- filter_grid[
        filter_grid$company == "All" &
          filter_grid$department == "All" &
          filter_grid$identity == "All" &
          filter_grid$location == "All" &
          filter_grid$employee_type == "All" &
          filter_grid$tenure == "All" &
          filter_grid$work_arrangement == "All",
        ,
        drop = FALSE
      ]
      key_rows <- key_rows[key_rows[[segment_id]] == seg_val, , drop = FALSE]
      key <- if (nrow(key_rows) > 0L) as.character(key_rows$filter_id[[1]]) else NA_character_

      metric_vals <- sapply(fundamentals, function(fu) {
        if (is.character(key) && nzchar(key)) {
          v <- kpi_scores$score[
            kpi_scores$filter_id == key &
              kpi_scores$metric_group == "fundamental" &
              kpi_scores$metric_id == fu
          ]
          vv <- as.numeric(first_or(v, NA_real_))
          if (is.finite(vv)) return(round(vv, 2))
        }
        fake_score(segment_id, seg_val, fu)
      })
      col_name <- short_label(seg_val)
      while (col_name %in% names(tab)) {
        col_name <- paste0(col_name, " *")
      }
      tab[[col_name]] <- metric_vals
    }
    tab
  }

  dimension_defs <- list(
    list(id = "company", label = "Company", values = dim_company),
    list(id = "department", label = "Department", values = dim_department),
    list(id = "identity", label = "Identity", values = dim_identity),
    list(id = "location", label = "Location", values = dim_location),
    list(id = "employee_type", label = "Employee Type", values = dim_employee_type),
    list(id = "tenure", label = "Tenure", values = dim_tenure),
    list(id = "work_arrangement", label = "Work Arrangement", values = dim_work_arrangement)
  )
  compare_sets <- lapply(dimension_defs, function(dd) {
    list("Fundamental Scores" = build_table_for(dd$id, dd$values))
  })
  names(compare_sets) <- vapply(dimension_defs, function(dd) dd$label, character(1))

  list(
    title = "",
    subtitle = "",
    legend_low = "Lower",
    legend_high = "Higher",
    tables = compare_sets[["Department"]],
    compare_sets = compare_sets
  )
}

filter_label <- function(fr) {
  parts <- c(
    if (fr$company != "All") paste0("Company: ", fr$company),
    if (fr$department != "All") paste0("Department: ", fr$department),
    if (fr$identity != "All") paste0("Identity: ", fr$identity),
    if (fr$location != "All") paste0("Location: ", fr$location),
    if (fr$employee_type != "All") paste0("Employee Type: ", fr$employee_type),
    if (fr$tenure != "All") paste0("Tenure: ", fr$tenure),
    if (fr$work_arrangement != "All") paste0("Work Arrangement: ", fr$work_arrangement)
  )
  if (length(parts) < 1L) return("All respondents")
  paste(parts, collapse = " | ")
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
    bm_mean <- as.numeric(first_or(bm_now$mean, NA_real_))
    bm_sd <- as.numeric(first_or(bm_now$sd, NA_real_))
    bm_prev_mean <- as.numeric(first_or(bm_prev$mean, NA_real_))
    bm_prev_sd <- as.numeric(first_or(bm_prev$sd, NA_real_))
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
  score_now <- as.numeric(first_or(k_now, mean(items_df$mean, na.rm = TRUE)))
  hist_pair <- metric_history_pair(outcome_history, fid, "outcome_id", outcome_name)
  score_prior <- suppressWarnings(as.numeric(first_or(hist_pair$prior$score, NA_real_)))
  score_delta <- if (is.finite(score_prior)) score_now - score_prior else 0

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
      percentile_delta = if (is.finite(score_prior)) round((score_now - score_prior) * 20) else 0,
      delta_label = paste0("vs. ", if (is.finite(prior_year)) prior_year else (report_year - 1L)),
      score = score_now,
      score_delta = score_delta,
      stringsAsFactors = FALSE
    ),
    drivers = drivers_df,
    items = items_df
  )
}

build_outcomes_overview_data <- function(rows_sub, fid) {
  outcome_names <- c("Engagement", "Burnout", "Work satisfaction")
  detail <- lapply(outcome_names, function(nm) build_outcome_data(nm, rows_sub, fid))
  ok <- vapply(detail, function(x) !is.null(x), logical(1))
  detail <- detail[ok]
  if (length(detail) < 1L) return(NULL)

  scores <- vapply(detail, function(x) as.numeric(x$summary$score[[1]]), numeric(1))
  deltas <- vapply(detail, function(x) as.numeric(x$summary$score_delta[[1]]), numeric(1))
  score_now <- mean(scores, na.rm = TRUE)
  score_delta <- mean(deltas, na.rm = TRUE)
  percentile <- round((score_now / 5) * 100)

  pe <- marts$predictive_edges
  pe <- pe[tolower(pe$subset) == "all" & tolower(pe$outcome) %in% tolower(outcome_names), , drop = FALSE]
  if (nrow(pe) > 0L) {
    pe$strength <- suppressWarnings(as.numeric(pe$strength))
    drv <- stats::aggregate(abs(strength) ~ fundamental, data = pe, FUN = function(x) mean(x, na.rm = TRUE))
    names(drv)[2] <- "driver_strength"
    drv <- drv[order(-drv$driver_strength), , drop = FALSE]
    drv <- utils::head(drv, 5L)
    drivers <- data.frame(
      rank = seq_len(nrow(drv)),
      fundamental = as.character(drv$fundamental),
      driver_strength = as.numeric(drv$driver_strength),
      status_label = ifelse(drv$driver_strength >= 0.45, "High", ifelse(drv$driver_strength >= 0.3, "Medium", "Low")),
      stringsAsFactors = FALSE
    )
  } else {
    drivers <- data.frame(
      rank = 1:3,
      fundamental = c("Leadership", "Communication", "Purpose"),
      driver_strength = c(0.42, 0.35, 0.31),
      status_label = c("High", "Medium", "Medium"),
      stringsAsFactors = FALSE
    )
  }

  items_all <- do.call(rbind, lapply(seq_along(detail), function(i) {
    d <- detail[[i]]
    nm <- as.character(d$summary$outcome[[1]])
    items <- d$items
    items$section <- nm
    items$section_order <- i
    items
  }))
  items_all <- items_all[order(items_all$column, items_all$section_order, items_all$item_order), , drop = FALSE]

  list(
    summary = data.frame(
      outcome = "Outcomes Overview",
      status_label = if (percentile < 40) "Area for Growth" else if (percentile < 60) "Industry Standard" else "Above Standard",
      percentile = percentile,
      percentile_delta = round(score_delta * 20),
      delta_label = paste0("vs. ", if (is.finite(prior_year)) prior_year else (report_year - 1L)),
      score = score_now,
      score_delta = score_delta,
      stringsAsFactors = FALSE
    ),
    drivers = drivers,
    items = items_all[, c("column", "section", "section_order", "item_order", "label", "mean", "agree_pct", "neutral_pct", "disagree_pct", "vs_industry", "vs_prior"), drop = FALSE]
  )
}

outcome_to_dashboard_data <- function(od) {
  drv <- od$drivers[order(od$drivers$rank), , drop = FALSE]
  drv <- utils::head(drv, 3L)
  out_df <- data.frame(
    rank = seq_len(nrow(drv)),
    outcome = as.character(drv$fundamental),
    percentile = pmax(1, pmin(99, round(abs(as.numeric(drv$driver_strength)) * 100))),
    stringsAsFactors = FALSE
  )
  items <- od$items
  items <- items[order(items$column, items$section_order, items$item_order), , drop = FALSE]
  left_items <- items[items$column == "left", , drop = FALSE]
  right_items <- items[items$column == "right", , drop = FALSE]
  left_items <- utils::head(left_items, 4L)
  right_items <- utils::head(right_items, 4L)
  items <- rbind(left_items, right_items)
  items$item_order <- ave(seq_len(nrow(items)), items$column, FUN = seq_along)
  if (!"vs_company" %in% names(items)) items$vs_company <- NA_real_
  if (!"item_n" %in% names(items)) items$item_n <- NA_integer_
  if (!"prior_n" %in% names(items)) items$prior_n <- NA_integer_
  if (!"suppress_row" %in% names(items)) items$suppress_row <- FALSE
  if (!"suppress_vs_company" %in% names(items)) items$suppress_vs_company <- TRUE
  if (!"suppress_vs_industry" %in% names(items)) items$suppress_vs_industry <- FALSE
  if (!"suppress_vs_prior" %in% names(items)) items$suppress_vs_prior <- FALSE
  if (!"min_n" %in% names(items)) items$min_n <- 3L

  list(
    fundamental = data.frame(
      fundamental_label = as.character(od$summary$outcome[[1]]),
      drivers_table_title = "Key Drivers",
      drivers_row_label = "Driver",
      percentile = as.numeric(od$summary$percentile[[1]]),
      percentile_delta = as.numeric(od$summary$percentile_delta[[1]]),
      delta_label = as.character(od$summary$delta_label[[1]]),
      score = as.numeric(od$summary$score[[1]]),
      score_delta = as.numeric(od$summary$score_delta[[1]]),
      stringsAsFactors = FALSE
    ),
    outcomes = out_df,
    items = items[, c(
      "column", "section", "section_order", "item_order", "label", "mean",
      "disagree_pct", "neutral_pct", "agree_pct", "vs_company", "vs_industry", "vs_prior",
      "item_n", "prior_n", "suppress_row", "suppress_vs_company", "suppress_vs_industry", "suppress_vs_prior", "min_n"
    ), drop = FALSE]
  )
}

comment_columns_user <- grep("^comment_[0-9]+$", names(user_data), value = TRUE)
oe_summary_overlay <- read_required_csv(
  file.path(data_dir, "demo_open_ended_summary.csv"),
  c("scope_id", "title", "kicker", "narrative", "score_label", "context_note"),
  "demo_open_ended_summary.csv"
)
oe_takeaways_overlay <- read_required_csv(
  file.path(data_dir, "demo_open_ended_takeaways.csv"),
  c("scope_id", "takeaway_id", "title", "narrative", "rank", "metric_label"),
  "demo_open_ended_takeaways.csv"
)
oe_theme_overlay <- read_required_csv(
  file.path(data_dir, "demo_open_ended_theme_evidence.csv"),
  c("scope_id", "takeaway_id", "theme_title", "context_text", "metric_label"),
  "demo_open_ended_theme_evidence.csv"
)
oe_quote_tag_overlay <- read_required_csv(
  file.path(data_dir, "demo_open_ended_quote_tags.csv"),
  c("priority", "pattern", "takeaway_id", "theme_title"),
  "demo_open_ended_quote_tags.csv"
)
oe_comments_seed <- read_required_csv(
  file.path(data_dir, "demo_open_ended_comments.csv"),
  c("scope_id", "takeaway_id", "theme_title", "comment_text", "source_tag", "sort_order"),
  "demo_open_ended_comments.csv"
)
oe_quote_tag_overlay$priority <- suppressWarnings(as.integer(oe_quote_tag_overlay$priority))
oe_quote_tag_overlay <- oe_quote_tag_overlay[order(oe_quote_tag_overlay$priority), , drop = FALSE]

lookup_scope_rows <- function(df, fid) {
  exact <- df[df$scope_id == fid, , drop = FALSE]
  if (nrow(exact) > 0L) return(exact)
  df[df$scope_id == "Overall", , drop = FALSE]
}

sanitize_comment_text <- function(x) {
  out <- normalize_text(x)
  out <- gsub("\\b[A-Z]{2,}[[:space:]]+[A-Z][a-z]+\\b", "the organization", out, perl = TRUE)
  out <- gsub("\\b(Mr|Mrs|Ms|Dr)\\.?[[:space:]]+[A-Z][a-z]+\\b", "a colleague", out, perl = TRUE)
  out <- gsub("\\b(manager|supervisor|director|lead)[[:space:]]+[A-Z][a-z]+\\b", "\\1", out, perl = TRUE, ignore.case = TRUE)
  out <- gsub("\\b[A-Z]{2,}-[0-9]{2,}\\b", "[id]", out, perl = TRUE)
  out <- gsub("\\s+", " ", trimws(out))
  out
}

extract_filter_comments <- function(rows_curr) {
  if (length(comment_columns_user) < 1L || nrow(rows_curr) < 1L) {
    return(data.frame(quote_text = character(0), question_label = character(0), stringsAsFactors = FALSE))
  }
  out <- lapply(comment_columns_user, function(col) {
    vals <- sanitize_comment_text(as.character(rows_curr[[col]]))
    vals <- vals[nzchar(vals) & nchar(vals) >= 25]
    if (length(vals) < 1L) return(NULL)
    data.frame(
      quote_text = vals,
      question_label = as.character(first_or(comment_question_labels[[col]], "General comments")),
      stringsAsFactors = FALSE
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) < 1L) {
    return(data.frame(quote_text = character(0), question_label = character(0), stringsAsFactors = FALSE))
  }
  df <- do.call(rbind, out)
  dedup <- !duplicated(tolower(df$quote_text))
  df[dedup, , drop = FALSE]
}

assign_takeaway_id <- function(comment_text) {
  tx <- tolower(comment_text)
  tagged <- oe_quote_tag_overlay[vapply(oe_quote_tag_overlay$pattern, function(pat) {
    grepl(pat, tx, perl = TRUE)
  }, logical(1)), , drop = FALSE]
  if (nrow(tagged) > 0L) return(as.character(tagged$takeaway_id[[1]]))
  if (grepl("leader|strategy|priority|clarity|direction|plan", tx, perl = TRUE)) return("takeaway_1")
  if (grepl("team|handoff|collab|cross|silo|communication", tx, perl = TRUE)) return("takeaway_2")
  if (grepl("workload|pace|burnout|capacity|staff|time", tx, perl = TRUE)) return("takeaway_3")
  "takeaway_2"
}

assign_theme_title <- function(comment_text, takeaway_id, takeaways_df, theme_df) {
  tx <- tolower(comment_text)
  tagged <- oe_quote_tag_overlay[vapply(oe_quote_tag_overlay$pattern, function(pat) {
    grepl(pat, tx, perl = TRUE)
  }, logical(1)), , drop = FALSE]
  if (nrow(tagged) > 0L && nzchar(as.character(tagged$theme_title[[1]]))) {
    return(as.character(tagged$theme_title[[1]]))
  }
  theme_hit <- theme_df$theme_title[match(takeaway_id, theme_df$takeaway_id)]
  if (length(theme_hit) > 0L && nzchar(theme_hit[[1]])) return(as.character(theme_hit[[1]]))
  tak_hit <- takeaways_df$title[match(takeaway_id, takeaways_df$takeaway_id)]
  if (length(tak_hit) > 0L) return(as.character(tak_hit[[1]]))
  "Theme"
}

assign_comment_category <- function(comment_text) {
  tx <- tolower(comment_text)
  if (grepl("purpose|community|meaning|proud|value we create|broader purpose", tx, perl = TRUE)) return("Purpose")
  if (grepl("strategy|priority|direction|objective|plan|focus|sequencing", tx, perl = TRUE)) return("Strategy")
  if (grepl("leader|leadership|manager|management|director|decision context", tx, perl = TRUE)) return("Leadership")
  if (grepl("communicat|transparen|clarity|information|feedback loop|townhall|message", tx, perl = TRUE)) return("Communication")
  if (grepl("learn|innovation|idea|experiment|improve process|new way", tx, perl = TRUE)) return("Learning & innovation")
  if (grepl("respect|care|trust|belong|support|inclusive|psychological", tx, perl = TRUE)) return("Respect, care, and trust")
  if (grepl("career|growth|coaching|develop|recognition|goal|performance", tx, perl = TRUE)) return("Performance development")
  if (grepl("safety|safe|hazard|incident|risk|unsafe", tx, perl = TRUE)) return("Safety")
  "Strategy"
}

build_fallback_open_ended <- function(fid) {
  k_now <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]
  fk <- k_now[k_now$metric_group == "fundamental", c("metric_id", "metric_label", "score"), drop = FALSE]
  if (nrow(fk) > 0L) fk <- fk[order(-fk$score), , drop = FALSE]
  strengths <- if (nrow(fk) > 0L) paste(utils::head(fk$metric_label, 2L), collapse = ", ") else "higher-scoring fundamentals"
  risks <- if (nrow(fk) > 0L) paste(utils::head(fk$metric_label[order(fk$score)], 2L), collapse = ", ") else "emerging risk areas"
  list(
    summary = data.frame(
      scope_id = fid,
      title = "Organizational Narrative Summary",
      kicker = "Thematic Insights",
      narrative = paste0(
        "Across open-ended responses, employees report strongest experiences in ", strengths,
        ". The most frequent concerns center on ", risks,
        ". These themes translate comment-level feedback into practical priorities."
      ),
      score_label = "Average Fundamental Score",
      context_note = "Fallback narrative generated from current scores.",
      stringsAsFactors = FALSE
    ),
    takeaways = data.frame(
      scope_id = fid,
      takeaway_id = c("takeaway_1", "takeaway_2", "takeaway_3"),
      title = c("Leadership and direction clarity", "Cross-team operating flow", "Workload sustainability"),
      narrative = c(
        "Employees respond best when leaders connect enterprise decisions to local work priorities.",
        "Cross-functional execution is strongest when ownership and handoffs are explicit.",
        "Workload concerns increase when pace shifts faster than teams can absorb."
      ),
      rank = 1:3,
      metric_label = c("Leadership", "Communication", "Performance development"),
      stringsAsFactors = FALSE
    ),
    theme = data.frame(
      scope_id = fid,
      takeaway_id = c("takeaway_1", "takeaway_2", "takeaway_3"),
      theme_title = c("Leadership and direction clarity", "Cross-team operating flow", "Workload sustainability"),
      context_text = c(
        "Employees ask for clearer decision context and sequencing.",
        "Comments emphasize execution friction at team boundaries.",
        "Sustainable pace depends on planning discipline and realistic capacity."
      ),
      metric_label = c("Leadership", "Communication", "Performance development"),
      stringsAsFactors = FALSE
    )
  )
}

build_open_ended_data <- function(fid, fr, rows_curr) {
  k_now <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]
  fk <- k_now[k_now$metric_group == "fundamental", c("metric_id", "metric_label", "score"), drop = FALSE]
  fallback_oe <- build_fallback_open_ended(fid)

  summary_src <- lookup_scope_rows(oe_summary_overlay, fid)
  if (nrow(summary_src) < 1L) summary_src <- fallback_oe$summary
  takeaways_src <- lookup_scope_rows(oe_takeaways_overlay, fid)
  if (nrow(takeaways_src) < 1L) takeaways_src <- fallback_oe$takeaways
  theme_src <- lookup_scope_rows(oe_theme_overlay, fid)
  if (nrow(theme_src) < 1L) theme_src <- fallback_oe$theme

  takeaways_src$rank <- suppressWarnings(as.integer(takeaways_src$rank))
  takeaways_src <- takeaways_src[order(takeaways_src$rank, takeaways_src$takeaway_id), , drop = FALSE]
  takeaways_src <- utils::head(takeaways_src, 3L)
  if (nrow(takeaways_src) < 3L) {
    missing_takeaways <- fallback_oe$takeaways[!fallback_oe$takeaways$takeaway_id %in% takeaways_src$takeaway_id, , drop = FALSE]
    takeaways_src <- rbind(takeaways_src, utils::head(missing_takeaways, 3L - nrow(takeaways_src)))
  }

  theme_src <- theme_src[theme_src$takeaway_id %in% takeaways_src$takeaway_id, , drop = FALSE]
  if (nrow(theme_src) < 1L) {
    theme_src <- fallback_oe$theme[fallback_oe$theme$takeaway_id %in% takeaways_src$takeaway_id, , drop = FALSE]
  }

  comments <- extract_filter_comments(rows_curr)
  if (nrow(comments) > 0L) {
    quote_df <- data.frame(
      quote_id = paste0("q_", seq_len(nrow(comments))),
      quote_text = comments$quote_text,
      question_label = comments$question_label,
      takeaway_id = vapply(comments$quote_text, assign_takeaway_id, character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    seed_rows <- lookup_scope_rows(oe_comments_seed, fid)
    if (nrow(seed_rows) < 1L) seed_rows <- oe_comments_seed[oe_comments_seed$scope_id == "Overall", , drop = FALSE]
    quote_df <- data.frame(
      quote_id = paste0("q_", seq_len(nrow(seed_rows))),
      quote_text = as.character(seed_rows$comment_text),
      question_label = "General comments",
      takeaway_id = as.character(seed_rows$takeaway_id),
      theme_title = as.character(seed_rows$theme_title),
      source_tag = as.character(seed_rows$source_tag),
      score = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (nrow(quote_df) > 0L && !"theme_title" %in% names(quote_df)) {
    quote_df$theme_title <- vapply(seq_len(nrow(quote_df)), function(i) {
      assign_theme_title(quote_df$quote_text[[i]], quote_df$takeaway_id[[i]], takeaways_src, theme_src)
    }, character(1))
    quote_df$source_tag <- "Employee comment"
    quote_df$score <- NA_real_
  }
  if (nrow(quote_df) > 0L) {
    quote_df <- quote_df[order(match(quote_df$takeaway_id, takeaways_src$takeaway_id), quote_df$question_label, quote_df$quote_text), , drop = FALSE]
    quote_df$comment_category <- vapply(quote_df$quote_text, assign_comment_category, character(1))
  }

  metric_lookup <- takeaways_src[, c("takeaway_id", "metric_label"), drop = FALSE]
  theme_df <- lapply(seq_len(nrow(theme_src)), function(i) {
    tid <- as.character(theme_src$takeaway_id[[i]])
    metric_label <- as.character(first_or(theme_src$metric_label[i], first_or(metric_lookup$metric_label[match(tid, metric_lookup$takeaway_id)], "Purpose")))
    frow <- fk[tolower(fk$metric_id) == tolower(metric_label) | tolower(fk$metric_label) == tolower(metric_label), , drop = FALSE]
    score <- if (nrow(frow) > 0L) as.numeric(frow$score[[1]]) else NA_real_
    status <- if (!is.finite(score)) "No score" else if (score < 3.2) "Area for Growth" else if (score < 3.8) "Industry Standard" else "Above Standard"
    data.frame(
      takeaway_id = tid,
      theme_title = as.character(theme_src$theme_title[[i]]),
      context_text = as.character(theme_src$context_text[[i]]),
      metric_label = metric_label,
      metric_value = score,
      metric_status = status,
      stringsAsFactors = FALSE
    )
  })
  theme_df <- do.call(rbind, theme_df)

  mention_counts <- if (nrow(quote_df) > 0L) table(quote_df$takeaway_id) else integer(0)
  takeaways_df <- data.frame(
    takeaway_id = as.character(takeaways_src$takeaway_id),
    title = as.character(takeaways_src$title),
    narrative = as.character(takeaways_src$narrative),
    rank = as.integer(takeaways_src$rank),
    stringsAsFactors = FALSE
  )
  takeaways_df$n_mentions <- as.integer(mention_counts[match(takeaways_df$takeaway_id, names(mention_counts))])
  takeaways_df$n_mentions[is.na(takeaways_df$n_mentions)] <- 0L

  summary_df <- data.frame(
    title = as.character(summary_src$title[[1]]),
    kicker = as.character(summary_src$kicker[[1]]),
    narrative = as.character(summary_src$narrative[[1]]),
    overall_score = if (nrow(fk) > 0L) mean(fk$score, na.rm = TRUE) else NA_real_,
    score_label = as.character(summary_src$score_label[[1]]),
    context_note = as.character(summary_src$context_note[[1]]),
    stringsAsFactors = FALSE
  )

  takeaway_outcome_map <- c(
    takeaway_1 = "Engagement",
    takeaway_2 = "Work satisfaction",
    takeaway_3 = "Burnout"
  )
  verbatim_df <- if (nrow(quote_df) > 0L) {
    data.frame(
      comment_id = paste0("c_", seq_len(nrow(quote_df))),
      takeaway_id = quote_df$takeaway_id,
      fundamental = as.character(quote_df$comment_category),
      outcome = as.character(takeaway_outcome_map[quote_df$takeaway_id]),
      question_label = as.character(quote_df$question_label),
      comment_text = quote_df$quote_text,
      sort_order = seq_len(nrow(quote_df)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      comment_id = character(0),
      takeaway_id = character(0),
      fundamental = character(0),
      outcome = character(0),
      question_label = character(0),
      comment_text = character(0),
      sort_order = integer(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    summary = summary_df,
    takeaways = takeaways_df[, c("takeaway_id", "title", "narrative", "rank"), drop = FALSE],
    theme_evidence = theme_df,
    quotes = quote_df[, c("quote_id", "takeaway_id", "theme_title", "quote_text", "score", "source_tag"), drop = FALSE],
    verbatim = verbatim_df,
    meta = data.frame(
      filter_label = filter_label(fr),
      n_responses = nrow(rows_curr),
      report_year = report_year,
      prior_year = if (is.finite(prior_year)) prior_year else NA_integer_,
      comment_n = nrow(verbatim_df),
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
  if (is.data.frame(oe$quotes) && nrow(oe$quotes) > 0L) {
    oe_quote_rows[[length(oe_quote_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$quotes, stringsAsFactors = FALSE)
  }
  if (is.data.frame(oe$verbatim) && nrow(oe$verbatim) > 0L) {
    oe_verbatim_rows[[length(oe_verbatim_rows) + 1L]] <- cbind(data.frame(filter_id = fid, stringsAsFactors = FALSE), oe$verbatim, stringsAsFactors = FALSE)
  }
}

open_ended_summary <- do.call(rbind, oe_summary_rows)
open_ended_takeaways <- do.call(rbind, oe_takeaway_rows)
open_ended_theme_evidence <- do.call(rbind, oe_theme_rows)
open_ended_quotes <- if (length(oe_quote_rows) > 0L) do.call(rbind, oe_quote_rows) else data.frame()
open_ended_verbatim <- if (length(oe_verbatim_rows) > 0L) do.call(rbind, oe_verbatim_rows) else data.frame()

overall_filter_id <- filter_grid$filter_id[
  filter_grid$company == "All" &
    filter_grid$department == "All" &
    filter_grid$identity == "All" &
    filter_grid$location == "All" &
    filter_grid$employee_type == "All" &
    filter_grid$tenure == "All" &
    filter_grid$work_arrangement == "All"
][[1]]
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
quotes_df$section <- "Thematic Insights"
quotes_df$sentiment <- "mixed"
quotes_df$source_col <- "workbook_open_ended"
quotes_df <- quotes_df[, c("quote_id", "theme_id", "section", "quote_text", "sentiment", "source_col"), drop = FALSE]

oe_theme_pages_n <- theme_page_count(overall_oe, quotes_per_page = 4L)
theme_slide_ids <- paste0("theme_evidence_", sprintf("%02d", seq_len(oe_theme_pages_n)))
short_theme_label <- function(x, max_chars = 30L) {
  x <- trimws(as.character(x %||% "Theme"))
  if (!nzchar(x)) x <- "Theme"
  if (nchar(x, type = "chars") <= max_chars) return(x)
  paste0(substr(x, 1L, max_chars - 3L), "...")
}

theme_titles_by_page <- character(0)
for (i in seq_len(nrow(overall_oe$theme_evidence))) {
  rr <- overall_oe$theme_evidence[i, , drop = FALSE]
  q <- overall_oe$quotes[overall_oe$quotes$takeaway_id == rr$takeaway_id[[1]], , drop = FALSE]
  if ("theme_title" %in% names(q)) {
    qq <- q[q$theme_title == rr$theme_title[[1]], , drop = FALSE]
    if (nrow(qq) > 0L) q <- qq
  }
  pages_i <- max(1L, ceiling(nrow(q) / 4L))
  theme_titles_by_page <- c(theme_titles_by_page, rep(rr$theme_title[[1]], pages_i))
}
if (length(theme_titles_by_page) < oe_theme_pages_n) {
  theme_titles_by_page <- c(theme_titles_by_page, rep("Theme", oe_theme_pages_n - length(theme_titles_by_page)))
}
theme_titles_by_page <- theme_titles_by_page[seq_len(oe_theme_pages_n)]
theme_slide_labels <- vapply(theme_titles_by_page, short_theme_label, character(1))

slides <- data.frame(
  slide_id = c(
    "cover", "ohep_model_image", "methodology",
    "orientation_model", "demographics_overview", "priority_matrix", "segment_heatmap",
    paste0("fundamental_", slugify(fundamentals)),
    "outcomes_overview", "engagement_deep_dive", "burnout_deep_dive", "work_satisfaction_deep_dive", "enps",
    "open_ended_overall_summary", "open_ended_three_takeaways",
    theme_slide_ids,
    "comments_explorer"
  ),
  slide_label = c(
    "Introduction", "About the OHEP", "Methodology",
    "OHEP Results", "Demographics", "Priority Matrix", "Segment Heatmap",
    fundamentals,
    "Overall", "Engagement", "Burnout", "Work Satisfaction", "eNPS",
    "Themes Overview", "Three Key Takeaways",
    theme_slide_labels,
    "Comments"
  ),
  section_id = c(
    "system_orientation", "system_orientation", "system_orientation",
    "ohep_summary", "ohep_summary", "ohep_summary", "ohep_summary",
    rep("drivers", length(fundamentals)),
    "outcomes", "outcomes", "outcomes", "outcomes", "outcomes",
    "insights", "insights",
    rep("insights", length(theme_slide_ids)),
    "comments"
  ),
  nav_level = c(
    rep(0L, 7L),
    rep(0L, length(fundamentals)),
    0L, 0L, 0L, 0L, 0L,
    0L, 0L,
    rep(1L, length(theme_slide_ids)),
    0L
  ),
  nav_group = c(
    rep("", 7L),
    rep("", length(fundamentals)),
    "", "", "", "", "",
    "", "",
    rep("Themes", length(theme_slide_ids)),
    ""
  ),
  stringsAsFactors = FALSE
)
slides$sort_order <- seq_len(nrow(slides))

section_meta <- data.frame(
  section_id = c("system_orientation","ohep_summary","drivers","outcomes","insights","comments"),
  section_label = c("Getting Started","OHEP Summary","Drivers","Outcomes","Insights","Comments"),
  section_order = seq_len(6),
  stringsAsFactors = FALSE
)

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
  if (nrow(rows_curr) < 8L && !slide_id %in% c("cover", "methodology", "ohep_model_image", "orientation_model")) {
    return(no_data_slide("Too few responses for this filter combination."))
  }

  k_now <- kpi_scores[kpi_scores$filter_id == fid, , drop = FALSE]

  render_comments_explorer_page <- function(oe) {
    v <- oe$verbatim
    if (!is.data.frame(v) || nrow(v) < 1L) return(no_data_slide("Insufficient open-ended comment volume for this filter."))
    if (!("fundamental" %in% names(v))) v$fundamental <- ""
    if (!("outcome" %in% names(v))) v$outcome <- ""
    vv <- v
    vv$fundamental <- trimws(as.character(vv$fundamental))
    vv$outcome <- trimws(as.character(vv$outcome))
    vv$fundamental[!nzchar(vv$fundamental)] <- NA_character_
    vv$outcome[!nzchar(vv$outcome)] <- NA_character_

    outcome_topics <- c("Engagement", "Burnout", "Work Satisfaction", "eNPS")
    topic_df <- rbind(
      data.frame(topic_type = "Outcome", topic_label = "Overall", stringsAsFactors = FALSE),
      data.frame(topic_type = "Driver", topic_label = fundamentals, stringsAsFactors = FALSE),
      data.frame(topic_type = "Outcome", topic_label = outcome_topics, stringsAsFactors = FALSE)
    )
    topic_df$topic_id <- paste0("topic_", slugify(paste(topic_df$topic_type, topic_df$topic_label)))
    topic_df$display_label <- paste0(topic_df$topic_type, ": ", topic_df$topic_label)
    topic_df$order_key <- seq_len(nrow(topic_df))

    get_topic_comments <- function(topic_type, topic_label) {
      if (identical(topic_type, "Driver")) {
        vv[vv$fundamental == topic_label, , drop = FALSE]
      } else if (identical(topic_label, "Overall")) {
        vv
      } else {
        vv[vv$outcome == topic_label, , drop = FALSE]
      }
    }

    synth_comments <- function(topic_type, topic_label, n = 8L) {
      if (identical(topic_type, "Driver")) {
        row <- k_now[k_now$metric_group == "fundamental" & tolower(k_now$metric_label) == tolower(topic_label), , drop = FALSE]
      } else if (identical(topic_label, "Overall")) {
        row <- k_now[k_now$metric_group %in% c("fundamental", "outcome"), , drop = FALSE]
      } else {
        row <- k_now[k_now$metric_group == "outcome" & tolower(k_now$metric_label) == tolower(topic_label), , drop = FALSE]
      }
      score_txt <- if (nrow(row) > 0L && is.finite(suppressWarnings(as.numeric(row$score[[1]])))) sprintf("%.2f", as.numeric(row$score[[1]])) else "n/a"
      priors <- c(
        paste0("People consistently mention that ", tolower(topic_label), " is most effective when leaders provide clearer context and follow-through."),
        paste0("Several comments suggest that stronger coordination around ", tolower(topic_label), " would reduce friction in day-to-day execution."),
        paste0("A repeated theme is that teams want more predictable communication and decision cadence related to ", tolower(topic_label), "."),
        paste0("Employees highlight that practical support and clearer ownership would materially improve ", tolower(topic_label), "."),
        paste0("Current filtered score for ", topic_label, " is ", score_txt, ", and comments indicate opportunities to tighten operating discipline."),
        paste0("Feedback shows momentum in ", tolower(topic_label), ", but people still ask for clearer priorities and sequencing."),
        paste0("Comments point to uneven experiences in ", tolower(topic_label), " across groups, with execution clarity being the main differentiator."),
        paste0("The strongest recommendation is to make expectations explicit so teams can act faster on ", tolower(topic_label), ".")
      )
      out <- priors[seq_len(min(length(priors), n))]
      if (length(out) < n) out <- rep(out, length.out = n)
      out
    }

    classify_sentiment <- function(txt) {
      x <- tolower(trimws(as.character(txt)))
      if (!nzchar(x)) return(list(cls = "neutral", label = "Neutral"))
      if (grepl("frustrat|burnout|overload|stress|unclear|confus|delay|risk|unsafe|poor|issue|problem|lack", x, perl = TRUE)) {
        return(list(cls = "negative", label = "Negative"))
      }
      if (grepl("good|great|strong|support|clear|improv|better|effective|positive|proud|trust", x, perl = TRUE)) {
        return(list(cls = "positive", label = "Positive"))
      }
      list(cls = "neutral", label = "Neutral")
    }

    esc_attr <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)
    comments_per_page <- 6L

    page_id <- "comments_explorer"
    topic_comment_lists <- lapply(seq_len(nrow(topic_df)), function(i) {
      t_type <- topic_df$topic_type[[i]]
      t_label <- topic_df$topic_label[[i]]
      qv <- get_topic_comments(t_type, t_label)
      comments <- if (nrow(qv) >= 4L) as.character(qv$comment_text) else synth_comments(t_type, t_label, n = 8L)
      comments <- comments[nzchar(trimws(comments))]
      if (length(comments) < 1L) comments <- "No comments available for this segment."
      comments
    })

    # Default view should demonstrate sentiment + paging.
    default_comments <- topic_comment_lists[[1]]
    has_negative <- any(vapply(default_comments, function(txt) classify_sentiment(txt)$cls == "negative", logical(1)))
    if (!has_negative) {
      default_comments <- c(
        default_comments,
        "There is a disconnect between stated priorities and available capacity, which creates frustration and delivery risk."
      )
    }
    while (length(default_comments) < (comments_per_page + 2L)) {
      default_comments <- c(default_comments, synth_comments("Outcome", "Overall", n = 1L))
    }
    topic_comment_lists[[1]] <- default_comments

    topic_df$topic_count <- vapply(topic_comment_lists, length, integer(1))

    select_opts <- paste(vapply(seq_len(nrow(topic_df)), function(i) {
      t_id <- topic_df$topic_id[[i]]
      label <- topic_df$display_label[[i]]
      cnt <- topic_df$topic_count[[i]]
      paste0("<option value='", t_id, "'>", html_escape(label), " (", cnt, ")</option>")
    }, character(1)), collapse = "")

    panel_html <- paste(vapply(seq_len(nrow(topic_df)), function(i) {
      t_id <- topic_df$topic_id[[i]]
      comments <- topic_comment_lists[[i]]
      cards <- paste(vapply(seq_along(comments), function(j) {
        txt <- comments[[j]]
        sent <- classify_sentiment(txt)
        paste0(
          "<div class='comment-card sentiment-", sent$cls, "' data-sent='", sent$cls, "' data-search='", esc_attr(tolower(txt)), "'>",
          "<p class='comment-text'>", html_escape(txt), "</p>",
          "<div class='comment-footer'><span class='sentiment-badge sentiment-", sent$cls, "'>", sent$label, "</span></div>",
          "</div>"
        )
      }, character(1)), collapse = "")
      style <- if (i == 1L) "" else " style='display:none'"
      paste0(
        "<div id='", t_id, "' class='tab-content", if (i == 1L) " active" else "",
        "' data-panel='", t_id, "' data-total='", topic_df$topic_count[[i]], "'", style, ">",
        "<div class='comment-grid'>", cards, "</div></div>"
      )
    }, character(1)), collapse = "")

    intro_text <- "Filter employee comments by driver or outcome"
    slide_doc(
      paste0(
        "<div class='slide'><div id='", page_id, "' class='comments-topic'>",
        "<div class='slide-container'>",
        "<div class='header-section'><div class='title-group'>",
        "<div class='theme-kicker'>Qualitative Feedback</div>",
        "<h1 class='theme-title'>Comment Explorer</h1>",
        "<p class='theme-intro'>", html_escape(intro_text), "</p>",
        "</div></div>",
        "<div class='tab-controls'>",
        "<label for='comments-topic-select' class='topic-label'>Topic</label>",
        "<select id='comments-topic-select' class='topic-select'>", select_opts, "</select>",
        "<label for='comments-search-input' class='topic-label search-label'>Search</label>",
        "<div class='search-wrap'>",
        "<span class='search-icon'>",
        "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='11' cy='11' r='7'></circle><line x1='16.65' y1='16.65' x2='21' y2='21'></line></svg>",
        "</span>",
        "<input id='comments-search-input' class='comments-search' type='search' placeholder='Search comments...' />",
        "</div>",
        "<label for='comments-sentiment-select' class='topic-label search-label'>Sentiment</label>",
        "<select id='comments-sentiment-select' class='topic-select sentiment-select'>",
        "<option value='all' selected>All</option>",
        "<option value='positive'>Positive</option>",
        "<option value='neutral'>Neutral</option>",
        "<option value='negative'>Negative</option>",
        "</select>",
        "</div>",
        "<div class='sentiment-summary'>",
        "<div class='summary-label'>Segment Sentiment</div>",
        "<div class='inline-bar-wrapper'>",
        "<div class='inline-data-bar'>",
        "<div id='sent-pos-seg' class='bar-segment pos' style='width:0%' title='0% Positive (0)'>0% Pos (0)</div>",
        "<div id='sent-neu-seg' class='bar-segment neu' style='width:0%' title='0% Neutral (0)'>0% Neutral (0)</div>",
        "<div id='sent-neg-seg' class='bar-segment neg' style='width:0%' title='0% Negative (0)'>0% Neg (0)</div>",
        "</div></div></div>",
        "<div class='scrollable-area'>", panel_html, "</div>",
        "<div class='pagination-row' id='comments-pagination'>",
        "<span id='comments-page-status' class='page-status'>Page 1 of 1</span>",
        "<div class='page-actions'>",
        "<button id='comments-prev-page' class='page-btn' type='button'>Back</button>",
        "<button id='comments-next-page' class='page-btn' type='button'>Next Page</button>",
        "</div>",
        "</div>",
        "</div></div></div>",
        "<style>",
        "#", page_id, " *{box-sizing:border-box;}",
        "#", page_id, " .slide-container{width:1280px;height:720px;background:#fff;padding:36px 48px;border-radius:8px;box-shadow:0 10px 30px rgba(15,23,42,0.1);display:flex;flex-direction:column;overflow:hidden;}",
        "#", page_id, " .header-section{display:flex;justify-content:space-between;align-items:flex-end;border-bottom:2px solid #0D9488;padding-bottom:12px;margin-bottom:16px;flex-shrink:0;}",
        "#", page_id, " .title-group{display:flex;flex-direction:column;}",
        "#", page_id, " .theme-kicker{font-size:14px;font-weight:800;color:#0D9488;text-transform:uppercase;letter-spacing:1.5px;margin-bottom:4px;}",
        "#", page_id, " .theme-title{font-size:32px;font-weight:900;color:#0F172A;margin:0 0 4px 0;letter-spacing:-0.5px;}",
        "#", page_id, " .theme-intro{font-size:13px;color:#64748b;font-weight:700;margin:0;text-transform:uppercase;letter-spacing:.5px;}",
        "#", page_id, " .tab-controls{display:flex;gap:10px;align-items:center;margin-bottom:16px;flex-shrink:0;border-bottom:1px solid #F1F5F9;padding-bottom:16px;}",
        "#", page_id, " .topic-label{font-size:12px;font-weight:800;text-transform:uppercase;letter-spacing:.08em;color:#64748b;}",
        "#", page_id, " .search-label{margin-left:8px;}",
        "#", page_id, " .topic-select{min-width:320px;background:#F8FAFC;border:1px solid #cbd5e1;border-radius:10px;padding:8px 36px 8px 12px;font-size:13px;font-weight:700;color:#0f172a;outline:none;appearance:none;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 24 24' stroke='%2364748b'%3E%3Cpath stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M19 9l-7 7-7-7'%3E%3C/path%3E%3C/svg%3E\");background-repeat:no-repeat;background-position:right 10px center;background-size:14px;}",
        "#", page_id, " .sentiment-select{min-width:140px;max-width:160px;}",
        "#", page_id, " .topic-select:focus{border-color:#0D9488;box-shadow:0 0 0 3px rgba(13,148,136,.15);}",
        "#", page_id, " .search-wrap{display:flex;align-items:center;min-width:300px;max-width:420px;flex:1;border:1px solid #cbd5e1;border-radius:10px;background:#fff;padding:0 10px;}",
        "#", page_id, " .search-icon{display:inline-flex;align-items:center;justify-content:center;color:#64748b;width:16px;height:16px;}",
        "#", page_id, " .search-icon svg{width:14px;height:14px;}",
        "#", page_id, " .comments-search{width:100%;border:none;outline:none;padding:8px 8px;font-size:13px;font-weight:600;color:#0f172a;background:transparent;}",
        "#", page_id, " {--sent-pos-soft:#10B981;--sent-neu-soft:#E2E8F0;--sent-neg-soft:#F43F5E;}",
        "#", page_id, " .sentiment-summary{display:flex;align-items:center;gap:24px;background:#FFFFFF;border:1px solid #E2E8F0;border-radius:8px;padding:16px 24px;margin-bottom:16px;box-shadow:0 2px 4px rgba(15,23,42,0.02);}",
        "#", page_id, " .summary-label{font-size:11px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:.05em;white-space:nowrap;}",
        "#", page_id, " .inline-bar-wrapper{flex:1;}",
        "#", page_id, " .inline-data-bar{display:flex;height:32px;border-radius:6px;overflow:hidden;}",
        "#", page_id, " .bar-segment{display:flex;align-items:center;justify-content:center;font-size:11px;font-weight:800;color:#FFFFFF;white-space:nowrap;overflow:hidden;}",
        "#", page_id, " .bar-segment:first-child{border-top-left-radius:6px;border-bottom-left-radius:6px;}",
        "#", page_id, " .bar-segment:last-child{border-top-right-radius:6px;border-bottom-right-radius:6px;}",
        "#", page_id, " .bar-segment.pos{background:var(--sent-pos-soft);color:#FFFFFF;}",
        "#", page_id, " .bar-segment.neu{background:var(--sent-neu-soft);color:#0F172A;}",
        "#", page_id, " .bar-segment.neg{background:var(--sent-neg-soft);color:#FFFFFF;}",
        "#", page_id, " .scrollable-area{flex:1;overflow-y:auto;padding-right:12px;}",
        "#", page_id, " .scrollable-area::-webkit-scrollbar{width:6px;}",
        "#", page_id, " .scrollable-area::-webkit-scrollbar-track{background:#F1F5F9;border-radius:4px;}",
        "#", page_id, " .scrollable-area::-webkit-scrollbar-thumb{background:#CBD5E1;border-radius:4px;}",
        "#", page_id, " .pagination-row{margin-top:10px;display:flex;justify-content:space-between;align-items:center;padding-top:10px;border-top:1px solid #E2E8F0;}",
        "#", page_id, " .page-actions{display:inline-flex;align-items:center;gap:8px;}",
        "#", page_id, " .page-status{font-size:12px;font-weight:700;color:#64748B;}",
        "#", page_id, " .page-btn{border:1px solid #CBD5E1;background:#FFFFFF;border-radius:8px;padding:8px 12px;font-size:12px;font-weight:800;color:#0F172A;cursor:pointer;}",
        "#", page_id, " .page-btn:disabled{opacity:.45;cursor:not-allowed;}",
        "#", page_id, " .tab-content{display:none;animation:fadeIn .2s ease-in-out;}",
        "#", page_id, " .tab-content.active{display:block;}",
        "#", page_id, " .comment-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(320px,1fr));gap:16px;width:100%;}",
        "#", page_id, " .comment-card{background:#FFFFFF;padding:18px;border-radius:8px;border:1px solid #E2E8F0;border-left:4px solid #94A3B8;position:relative;display:flex;flex-direction:column;gap:14px;min-height:132px;}",
        "#", page_id, " .comment-card.sentiment-positive{border-left-color:#16A34A;}",
        "#", page_id, " .comment-card.sentiment-negative{border-left-color:#DC2626;}",
        "#", page_id, " .comment-footer{margin-top:auto;display:flex;justify-content:flex-start;}",
        "#", page_id, " .sentiment-badge{font-size:10px;font-weight:800;letter-spacing:.04em;text-transform:uppercase;padding:2px 8px;border-radius:999px;}",
        "#", page_id, " .sentiment-badge.sentiment-positive{background:#DCFCE7;color:#166534;}",
        "#", page_id, " .sentiment-badge.sentiment-neutral{background:#E2E8F0;color:#475569;}",
        "#", page_id, " .sentiment-badge.sentiment-negative{background:#FEE2E2;color:#991B1B;}",
        "#", page_id, " .comment-text{font-size:13px;line-height:1.5;color:#334155;font-weight:500;margin:0;}",
        "#", page_id, " .oe-empty{padding:24px;background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;color:#64748b;}",
        "#", page_id, " @keyframes fadeIn{from{opacity:0;transform:translateY(5px);}to{opacity:1;transform:translateY(0);}}",
        "</style>",
        "<script>(function(){var root=document.getElementById('", page_id, "');if(!root) return;",
        "var PAGE_SIZE=", as.integer(comments_per_page), ";",
        "var sel=root.querySelector('#comments-topic-select');var search=root.querySelector('#comments-search-input');var sentimentSel=root.querySelector('#comments-sentiment-select');var panels=root.querySelectorAll('.tab-content');",
        "var prevBtn=root.querySelector('#comments-prev-page');var nextBtn=root.querySelector('#comments-next-page');var pageStatus=root.querySelector('#comments-page-status');var currentPage=1;",
        "var posSeg=root.querySelector('#sent-pos-seg');var neuSeg=root.querySelector('#sent-neu-seg');var negSeg=root.querySelector('#sent-neg-seg');",
        "if(!sel) return;",
        "function activePanel(){return root.querySelector('.tab-content.active');}",
        "function pct(n,d){return d>0?Math.round((n*100)/d):0;}",
        "function updateSentiment(cards){var pos=0,neu=0,neg=0,total=cards.length;cards.forEach(function(card){var s=(card.getAttribute('data-sent')||'neutral');if(s==='positive')pos+=1;else if(s==='negative')neg+=1;else neu+=1;});var pPos=pct(pos,total),pNeu=pct(neu,total),pNeg=pct(neg,total);if(posSeg){posSeg.style.width=pPos+'%';posSeg.textContent=pPos+'% Pos ('+pos+')';posSeg.title=pPos+'% Positive ('+pos+')';}if(neuSeg){neuSeg.style.width=pNeu+'%';neuSeg.textContent=pNeu+'% Neutral ('+neu+')';neuSeg.title=pNeu+'% Neutral ('+neu+')';}if(negSeg){negSeg.style.width=pNeg+'%';negSeg.textContent=pNeg+'% Neg ('+neg+')';negSeg.title=pNeg+'% Negative ('+neg+')';}}",
        "function applyFilter(resetPage){var panel=activePanel();if(!panel) return;if(resetPage) currentPage=1;var q=(search&&search.value?search.value:'').toLowerCase().trim();var sent=(sentimentSel&&sentimentSel.value?sentimentSel.value:'all');var cards=Array.prototype.slice.call(panel.querySelectorAll('.comment-card'));var filtered=cards.filter(function(card){var hay=(card.getAttribute('data-search')||'');var s=(card.getAttribute('data-sent')||'neutral');var passQ=!q||hay.indexOf(q)!==-1;var passS=(sent==='all')||(s===sent);return passQ&&passS;});cards.forEach(function(card){card.style.display='none';});var totalPages=Math.max(1,Math.ceil(filtered.length/PAGE_SIZE));if(currentPage>totalPages) currentPage=totalPages;if(currentPage<1) currentPage=1;var start=(currentPage-1)*PAGE_SIZE;var end=Math.min(start+PAGE_SIZE,filtered.length);for(var i=start;i<end;i++){filtered[i].style.display='flex';}if(pageStatus) pageStatus.textContent='Page '+currentPage+' of '+totalPages;if(prevBtn) prevBtn.disabled=currentPage<=1;if(nextBtn) nextBtn.disabled=currentPage>=totalPages;updateSentiment(filtered);}",
        "function showTopic(id){panels.forEach(function(p){var on=(p.id===id);p.classList.toggle('active',on);p.style.display=on?'block':'none';});applyFilter(true);}",
        "sel.addEventListener('change',function(){showTopic(sel.value);});",
        "if(search){search.addEventListener('keyup',function(){applyFilter(true);});search.addEventListener('search',function(){applyFilter(true);});search.addEventListener('keydown',function(e){if(e.key==='Enter'){applyFilter(true);}});}",
        "if(sentimentSel){sentimentSel.addEventListener('change',function(){applyFilter(true);});}",
        "if(prevBtn){prevBtn.addEventListener('click',function(){currentPage-=1;applyFilter(false);});}",
        "if(nextBtn){nextBtn.addEventListener('click',function(){currentPage+=1;applyFilter(false);});}",
        "showTopic(sel.value);",
        "})();</script>",
        "</div>"
      ),
      "Comments"
    )
  }

  render_getting_started_page <- function(page_title, body_html, eyebrow = "Getting Started") {
    slide_doc(
      paste0(
        "<div class='slide'><div class='main-card gs-main'>",
        "<header class='gs-header'>",
        "<div class='gs-eyebrow'>", html_escape(eyebrow), "</div>",
        "<h1 class='gs-title'>", html_escape(page_title), "</h1>",
        "</header>",
        body_html,
        "</div></div>",
        "<style>",
        ".gs-main{background:#FFFFFF;border:1px solid #d9e2eb;border-radius:12px;padding:32px 34px;box-shadow:0 10px 25px -5px rgba(15,23,42,.05);display:flex;flex-direction:column;}",
        ".gs-header{margin-bottom:24px;}",
        ".gs-eyebrow{font-size:11px;font-weight:800;color:#0D9488;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;}",
        ".gs-title{font-size:30px;font-weight:900;letter-spacing:-0.4px;color:#0F172A;margin:0;}",
        ".gs-layout{display:grid;grid-template-columns:1fr 1fr;gap:20px;}",
        ".gs-col-span-2{grid-column:span 2;}",
        ".gs-box{background:#F8FAFC;border:1px solid #E2E8F0;border-radius:8px;padding:24px;display:flex;flex-direction:column;gap:14px;}",
        ".gs-box h2{font-size:20px;font-weight:800;color:#0F172A;margin:0;}",
        ".gs-box p{font-size:14px;line-height:1.6;color:#334155;margin:0;}",
        ".gs-box strong{font-weight:800;color:#0F172A;}",
        ".gs-img-wrap{display:flex;justify-content:center;align-items:center;min-height:320px;}",
        ".gs-img-wrap img{max-width:100%;max-height:420px;object-fit:contain;border-radius:8px;}",
        "@media (max-width: 860px){.gs-layout{grid-template-columns:1fr;}.gs-col-span-2{grid-column:span 1;}}",
        "</style>"
      ),
      page_title
    )
  }

  tryCatch({
    if (slide_id == "cover") {
      return(render_getting_started_page(
        "Introduction",
        paste0(
          "<div class='gs-layout' style='grid-template-columns:1fr;'>",
          "<section class='gs-box'>",
          "<h2>", html_escape(report_meta$report_title[[1]]), "</h2>",
          "<p>", html_escape(report_meta$report_subtitle[[1]]), "</p>",
          "<p>Use the left navigation to move through the story by section. Use the filters at the top to explore Company, Department, Identity (voluntary), Location, Employee Type, Tenure, and Work Arrangement. Slides update where sufficient sample size exists.</p>",
          "<p>This report combines quantified organizational health indicators with curated qualitative interpretation to support practical decision making.</p>",
          "</section>",
          "<section class='gs-box'>",
          "<h2>Distribution Policy</h2>",
          "<p>The questionnaire framework and individual scale items used in this Organizational Health and Effectiveness Profile are the exclusive intellectual property of Monark Inc. Materials are confidential and must not be shared externally, reproduced, or redistributed in any format without prior written consent from an authorized Monark representative.</p>",
          "<p><strong>For additional information:</strong><br>Monark Inc.<br>Calgary, Alberta<br>T: 403-604-9854<br>www.leadwithmonark.com</p>",
          "</section>",
          "</div>"
        )
      ))
    }
    if (slide_id == "ohep_model_image") {
      img_path <- file.path(demo_dir, "images", "ohepModel.png")
      if (!file.exists(img_path)) return(no_data_slide("Missing image: Demo/images/ohepModel.png"))
      return(render_getting_started_page(
        "About the OHEP",
        paste0(
          "<div class='gs-layout'>",
          "<section class='gs-box'>",
          "<h2>Why Organizational Health</h2>",
          "<p>Organizational health is a forward-looking driver of sustained performance, yet many critical factors do not appear in retrospective financial statements. Monark's OHEP framework translates these factors into measurable, decision-ready indicators.</p>",
          "<p>Developed in 2015 and continuously refined through peer-reviewed research, executive input, and field application, the OHEP is built around scientifically validated constructs linked to both financial and non-financial outcomes.</p>",
          "<p><strong>Survey Objectives:</strong> Track progress on key health factors and outcomes, benchmark performance to the OHEP Index, identify emerging risk areas early, and prioritize targeted interventions with measurable impact.</p>",
          "</section>",
          "<section class='gs-box gs-img-wrap'><img src='./images/ohepModel.png' alt='OHEP Model'/></section>",
          "<section class='gs-box gs-col-span-2'><p>Upon completion, results are statistically analyzed against outcome measures and benchmarked to the OHEP Index. The final report integrates quantitative results and qualitative themes to support leadership action planning.</p></section>",
          "</div>"
        )
      ))
    }
    if (slide_id == "methodology") {
      return(render_getting_started_page(
        "Methodology",
        paste0(
          "<div class='gs-layout'>",
          "<section class='gs-box'>",
          "<h2>Defining Terminology</h2>",
          "<p><strong>OHEP Index:</strong> Fundamental scores are benchmarked to organizations in energy and investment industries, adjusted for organizational size so no single company skews the index. This provides contextual interpretation beyond raw scores.</p>",
          "<p><strong>Percentage Agreement:</strong> Item-level agreement and disagreement percentages complement mean scores and improve interpretability at the question level.</p>",
          "<p><strong>Anonymity:</strong> Responses are confidential and presented in aggregate. Monark does not release raw respondent-level data to client management or directors.</p>",
          "</section>",
          "<section class='gs-box'>",
          "<h2>Survey Details (Arctic Slope)</h2>",
          "<p>The survey included 85 scale questions and 5 open-ended questions. Scale responses used a 1-5 Likert format (Strongly Disagree to Strongly Agree), with Not Applicable excluded from scale averages.</p>",
          "<p>Data collection window: June 18, 2025 to July 11, 2025.</p>",
          "<p>2025 response profile: 64% overall participation, with stronger office participation than field participation. Average completion time was approximately 18 minutes.</p>",
          "</section>",
          "<section class='gs-box gs-col-span-2'>",
          "<h2>Interpretation Guidance</h2>",
          "<p>Use OHEP summary, drivers, outcomes, and comments together. Summary pages show where to focus; driver and outcome pages show what is moving performance; qualitative pages provide operational context behind the scores.</p>",
          "</section>",
          "</div>"
        )
      ))
    }
    if (slide_id == "orientation_model") {
      md <- build_model_data(fid)
      if (is.null(md)) return(no_data_slide("Model unavailable for this filter."))
      md$summary$title <- ""
      md$summary$subtitle <- ""
      return(widget_shell_doc(model_page(
        model_data = md,
        color_overrides = list(
          model_zone_ni = "#FDD34C",
          model_zone_is = "#B3D6DA",
          model_zone_as = "#0096A6",
          model_zone_il = "#395965",
          model_point_ni = "#FDD34C",
          model_point_is = "#16A34A",
          model_point_as = "#059669",
          model_point_il = "#0D9488"
        )
      ), title = "OHEP Results", eyebrow = "OHEP Summary", description = NULL))
    }
    if (slide_id == "survey_overview") {
      prior_n <- if (is.finite(prior_year)) sum(rows_sub$year == prior_year) else 0L
      return(slide_doc(
        paste0(
          "<div class='slide'><div class='k'>Population Context</div><h1 class='h1'>Survey overview</h1>",
          "<div class='grid3'><div class='card'><div class='s'>Responses (Current Year)</div><div class='m'>", nrow(rows_curr), "</div></div>",
          "<div class='card'><div class='s'>Responses (Prior Year)</div><div class='m'>", prior_n, "</div></div>",
          "<div class='card'><div class='s'>Reporting Year</div><div class='m'>", report_year, "</div><div class='s'>Compared to ", html_escape(history_year_label(if (is.finite(prior_year)) prior_year else (report_year - 1L))), "</div></div></div>",
          "<div class='card' style='margin-top:14px'><p class='sub'>", html_escape(historical_context_text), "</p></div></div>"
        ),
        "Survey Overview"
      ))
    }
    if (slide_id == "demographics_overview") return(build_demographics_page(fid))
    if (slide_id == "priority_matrix") {
      pts <- build_matrix_points(fid)
      if (nrow(pts) < 1L) return(no_data_slide())
      priority_controls <- paste0(
        "<div class='page-control-group'>",
        "<label for='ohep-decision-matrix-axis' class='page-control-label'>Y-Axis:</label>",
        "<select id='ohep-decision-matrix-axis' class='page-control-select'>",
        "<option value='overall' selected>Outcomes (Overall)</option>",
        "<option value='engagement'>Engagement</option>",
        "<option value='burnout'>Burnout</option>",
        "<option value='work_satisfaction'>Work Satisfaction</option>",
        "<option value='enps'>eNPS</option>",
        "</select>",
        "</div>"
      )
      return(widget_shell_doc(
        decision_matrix_page(points = pts, theme_kicker = "", title = "", subtitle = "", show_axis_control = FALSE),
        title = "Priority Matrix",
        eyebrow = "OHEP Summary",
        description = NULL,
        controls_html = priority_controls
      ))
    }
    if (slide_id == "outcomes_overview") {
      ov <- build_outcomes_overview_data(rows_sub, fid)
      if (is.null(ov)) return(no_data_slide())
      dd <- outcome_to_dashboard_data(ov)
      return(render_html_obj(render_fundamental_page(dashboard_data = dd), "Overall"))
    }
    if (slide_id %in% c("engagement_deep_dive", "burnout_deep_dive", "work_satisfaction_deep_dive")) {
      lookup <- c(engagement_deep_dive = "Engagement", burnout_deep_dive = "Burnout", work_satisfaction_deep_dive = "Work satisfaction")
      out_label <- lookup[[slide_id]]
      od <- build_outcome_data(out_label, rows_sub, fid)
      if (is.null(od)) return(no_data_slide())
      dd <- outcome_to_dashboard_data(od)
      return(render_html_obj(render_fundamental_page(dashboard_data = dd), paste(out_label, "Deep Dive")))
    }
    if (slide_id == "enps") {
      en <- suppressWarnings(as.numeric(rows_curr$eNPS))
      en <- en[is.finite(en) & en >= 0 & en <= 10]
      if (length(en) < 5L) return(no_data_slide())
      en_prior <- if (is.finite(prior_year)) suppressWarnings(as.numeric(rows_sub$eNPS[rows_sub$year == prior_year])) else numeric(0)
      en_prior <- en_prior[is.finite(en_prior) & en_prior >= 0 & en_prior <= 10]
      dist <- data.frame(rating = 0:10, pct = sapply(0:10, function(v) round(100 * sum(en == v) / length(en), 1)))
      score <- round(100 * (sum(en >= 9) - sum(en <= 6)) / length(en))
      prior_score <- if (length(en_prior) > 0L) round(100 * (sum(en_prior >= 9) - sum(en_prior <= 6)) / length(en_prior)) else 0
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
        sc <- as.numeric(first_or(fund_now$score[match(f, fund_now$metric_id)], NA_real_))
        pct <- if (is.finite(sc)) round((sc / 5) * 100) else NA_real_
        status <- if (!is.finite(pct)) "Industry Standard" else if (pct < 40) "Area for Growth" else if (pct < 60) "Industry Standard" else "Above Standard"
        data.frame(rank = i, fundamental = f, percentile = pct, status_label = status, stringsAsFactors = FALSE)
      })
      drivers_df <- do.call(rbind, drivers)
      return(render_html_obj(enps_page(enps_data = list(
        summary = data.frame(title = "Employee Net Promoter Score (eNPS)", subtitle = "", score = score, score_delta = if (length(en_prior) > 0L) score - prior_score else 0, delta_label = paste0("vs. ", if (is.finite(prior_year)) prior_year else (report_year - 1L)), stringsAsFactors = FALSE),
        distribution = dist,
        drivers = drivers_df
      )), "eNPS"))
    }
    if (grepl("^fundamental_", slide_id)) {
      f <- fundamentals[match(slide_id, paste0("fundamental_", slugify(fundamentals)))]
      if (is.na(f)) return(no_data_slide())
      dd <- build_dashboard_data_from_filtered_user_data(
        company = "Company A",
        year = report_year,
        fundamental = f,
        marts = marts,
        filtered_user_data = rows_sub,
        min_n = 10
      )
      if (is.list(dd$privacy) && isTRUE(dd$privacy$suppress_page)) return(no_data_slide(dd$privacy$message %||% "Insufficient data"))
      hist <- metric_history_pair(fundamental_history, fid, "fundamental_id", f)
      cur_score <- suppressWarnings(as.numeric(first_or(hist$current$score, dd$fundamental$score[[1]])))
      prev_score <- suppressWarnings(as.numeric(first_or(hist$prior$score, NA_real_)))
      dd$fundamental$fundamental_label <- f
      dd$fundamental$percentile <- pmax(1, pmin(99, round((cur_score / 5) * 100)))
      dd$fundamental$percentile_delta <- if (is.finite(prev_score)) round((cur_score - prev_score) * 20) else 0
      dd$fundamental$score_delta <- if (is.finite(prev_score)) round(cur_score - prev_score, 2) else 0
      dd$fundamental$delta_label <- paste0("vs. ", if (is.finite(prior_year)) prior_year else (report_year - 1L))
      obj <- render_fundamental_page(dashboard_data = dd)
      return(render_html_obj(obj, paste("Fundamental", f)))
    }
    if (slide_id == "segment_heatmap") {
      hm <- build_heatmap()
      if (is.null(hm)) return(no_data_slide())
      compare_keys <- names(hm$compare_sets)
      if (is.null(compare_keys) || length(compare_keys) < 1L) compare_keys <- "Company"
      compare_default <- if ("Company" %in% compare_keys) "Company" else compare_keys[[1]]
      compare_opts <- paste(vapply(compare_keys, function(key) {
        paste0(
          "<option value='", html_escape(key), "'",
          if (identical(key, compare_default)) " selected" else "",
          ">",
          html_escape(key),
          "</option>"
        )
      }, character(1)), collapse = "")
      segment_controls <- paste0(
        "<div class='page-control-group'>",
        "<label for='ohep-heatmap-page-compare' class='page-control-label'>Compare By:</label>",
        "<select id='ohep-heatmap-page-compare' class='page-control-select'>",
        compare_opts,
        "</select>",
        "</div>"
      )
      return(widget_shell_doc(
        heatmap_page(heatmap_data = hm, show_compare_control = FALSE),
        title = "Segment Analysis",
        eyebrow = "OHEP Summary",
        description = NULL,
        controls_html = segment_controls
      ))
    }
    if (slide_id == "open_ended_overall_summary") {
      oe <- open_ended_by_filter[[fid]]
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "overall_summary", page_index = 1), "Open-Ended Summary"))
    }
    if (slide_id == "open_ended_three_takeaways") {
      oe <- open_ended_by_filter[[fid]]
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "three_takeaways", page_index = 1), "Three Takeaways"))
    }
    if (grepl("^theme_evidence_[0-9]{2}$", slide_id)) {
      oe <- open_ended_by_filter[[fid]]
      if (!is.data.frame(oe$quotes) || nrow(oe$quotes) < 4L) return(no_data_slide("Insufficient open-ended comment volume for this filter."))
      idx <- as.integer(sub("^theme_evidence_", "", slide_id))
      page_title <- first_or(slides$slide_label[match(slide_id, slides$slide_id)], paste("Theme", idx))
      return(render_html_obj(open_ended_page(open_ended_data = oe, page_type = "theme_evidence", page_index = idx), page_title))
    }
    if (slide_id == "comments_explorer") {
      oe <- open_ended_by_filter[[fid]]
      return(render_comments_explorer_page(oe))
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

responses_by_filter <- setNames(
  lapply(seq_len(nrow(filter_grid)), function(i) {
    fr <- filter_grid[i, , drop = FALSE]
    rows_sub <- apply_filter(user_data, fr)
    as.integer(sum(rows_sub$year == report_year, na.rm = TRUE))
  }),
  filter_grid$filter_id
)

legacy_demo_data_path <- "/Users/lyndenjensen/Downloads/Demo/build/demo-data.js"
legacy_theme_slide_ids <- c(
  "open_ended_overall_summary",
  "open_ended_three_takeaways"
)

if (file.exists(legacy_demo_data_path)) {
  legacy_lines <- readLines(legacy_demo_data_path, warn = FALSE, encoding = "UTF-8")
  legacy_payload <- paste(legacy_lines, collapse = "\n")
  legacy_payload <- sub("^\\s*window\\.OHEP_DEMO_DATA\\s*=\\s*", "", legacy_payload, perl = TRUE)
  legacy_payload <- sub(";\\s*$", "", legacy_payload, perl = TRUE)
  legacy_bundle <- tryCatch(
    jsonlite::fromJSON(legacy_payload, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.null(legacy_bundle)) {
    legacy_slide_html <- legacy_bundle$slide_html
    if (is.null(legacy_slide_html) && !is.null(legacy_bundle$slide_html_map)) {
      legacy_slide_html <- legacy_bundle$slide_html_map
    }
    if (!is.null(legacy_slide_html)) {
      for (sid in legacy_theme_slide_ids) {
        legacy_key <- names(legacy_slide_html)[grepl(paste0("^", sid, "::"), names(legacy_slide_html))][1]
        if (!is.na(legacy_key) && nzchar(legacy_key)) {
          legacy_html <- as.character(legacy_slide_html[[legacy_key]])
          legacy_html <- gsub("NorthRiver Energy", "Arctic Slope", legacy_html, fixed = TRUE)
          target_keys <- names(slide_html)[grepl(paste0("^", sid, "::"), names(slide_html))]
          for (tk in target_keys) slide_html[[tk]] <- legacy_html
        }
      }
    }
  }
}

bundle <- list(
  meta = list(
    report_title = report_meta$report_title[[1]],
    report_subtitle = report_meta$report_subtitle[[1]],
    client_label = report_meta$client_label[[1]],
    reporting_year = as.integer(report_meta$reporting_year[[1]])
  ),
  filter_dimensions = list(
    list(id = "company", label = "Company", options = as.list(dim_company)),
    list(id = "department", label = "Department", options = as.list(dim_department)),
    list(id = "identity", label = "Identity (voluntary)", options = as.list(dim_identity)),
    list(id = "location", label = "Location", options = as.list(dim_location)),
    list(id = "employee_type", label = "Employee Type", options = as.list(dim_employee_type)),
    list(id = "tenure", label = "Tenure (bands)", options = as.list(dim_tenure)),
    list(id = "work_arrangement", label = "Work Arrangement", options = as.list(dim_work_arrangement))
  ),
  sections = sections_payload,
  top_level_slide_ids = as.list(slides$slide_id[slides$section_id == ""]),
  slides = lapply(seq_len(nrow(slides)), function(i) {
    list(
      slide_id = slides$slide_id[[i]],
      slide_label = slides$slide_label[[i]],
      section_id = slides$section_id[[i]],
      sort_order = slides$sort_order[[i]],
      nav_level = as.integer(first_or(slides$nav_level[i], 0L)),
      nav_group = as.character(first_or(slides$nav_group[i], ""))
    )
  }),
  responses_by_filter = responses_by_filter,
  slide_order = as.list(slides$slide_id),
  slide_html = slide_html,
  no_data_html = no_data_slide()
)

bundle_json <- jsonlite::toJSON(bundle, auto_unbox = TRUE, pretty = FALSE, null = "null")
writeLines(paste0("window.OHEP_DEMO_DATA = ", bundle_json, ";"), con = file.path(build_dir, "demo-data.js"), useBytes = TRUE)

file.copy(file.path(app_dir, "index.html"), file.path(build_dir, "index.html"), overwrite = TRUE)
file.copy(file.path(app_dir, "styles.css"), file.path(build_dir, "styles.css"), overwrite = TRUE)
file.copy(file.path(app_dir, "app.js"), file.path(build_dir, "app.js"), overwrite = TRUE)

build_images_dir <- file.path(build_dir, "images")
dir.create(build_images_dir, recursive = TRUE, showWarnings = FALSE)
for (img_name in c("ASRCLogo.jpg", "ClientLogo.jpg", "ClientLogo.webp", "MonarkLogo.jpg", "ohepModel.png")) {
  src <- file.path(demo_dir, "images", img_name)
  if (file.exists(src)) file.copy(src, file.path(build_images_dir, img_name), overwrite = TRUE)
}

cat(sprintf("Build complete: %s\n", build_dir))
cat(sprintf("Workbook: %s (sheet: %s)\n", basename(workbook_path), sheet))
cat(sprintf("Reporting year: %d | Filter combinations: %d | Slides: %d | Rendered: %d\n", report_year, nrow(filter_grid), nrow(slides), length(slide_html)))
