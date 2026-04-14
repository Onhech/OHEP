#' Render Data Mapping Probe Report
#'
#' Render a lightweight HTML diagnostic report for an arbitrary input object
#' (for example: `"table.company"`, a vector, a data frame, or a scalar string).
#'
#' @param x Object to probe. Supports object names (`character(1)`) that will
#'   be resolved in `source_env`, vectors, data frames, and scalars. If `NULL`,
#'   the function tries `raw_user_data` from `source_env`.
#' @param index_data Optional index list with `item_data$item_id` used for
#'   required-column checks when `x` resolves to a data frame.
#' @param index_user_data_key Optional key table/list for additional reference
#'   context in output.
#' @param source_env Environment used for object-name resolution.
#' @param required_base_cols Character vector of required base columns.
#' @param sample_rows Number of preview rows to render for table-like inputs.
#' @param max_missing_preview Maximum missing columns listed in detail.
#' @param id HTML id for CSS scoping.
#'
#' @return A `htmltools` tag object with probe status, metadata, and previews.
#' @export
#'
#' @examples
#' test_data_mapping("raw_user_data", index_data = load_example_index_data())
#' test_data_mapping(letters[1:5])
test_data_mapping <- function(
  x = NULL,
  index_data = NULL,
  index_user_data_key = NULL,
  source_env = .GlobalEnv,
  required_base_cols = c("company", "year"),
  sample_rows = 5L,
  max_missing_preview = 40L,
  id = "ohep-data-mapping-probe"
) {
  .diag_render_report(
    mode = "probe",
    x = x,
    index_data = index_data,
    index_user_data_key = index_user_data_key,
    source_env = source_env,
    required_base_cols = required_base_cols,
    sample_rows = sample_rows,
    max_missing_preview = max_missing_preview,
    id = id
  )
}

#' Render Data Source Inventory Validator
#'
#' Render an HTML report listing available data sources in `source_env` and
#' validating required coverage (aggregate and per-column) for mapping.
#'
#' @param source_env Environment scanned for data sources.
#' @param index_data Optional index list with `item_data$item_id` used to derive
#'   required item columns.
#' @param index_user_data_key Optional key table/list with expected user columns
#'   (`item` column preferred) for additional reference and coverage stats.
#' @param required_base_cols Character vector of required base columns.
#' @param max_sources Maximum number of source objects displayed.
#' @param max_missing_preview Maximum missing columns listed in detail.
#' @param id HTML id for CSS scoping.
#'
#' @return A `htmltools` tag object with aggregate status, source inventory,
#'   column-level coverage, and diagnostics logs.
#' @export
#'
#' @examples
#' validate_data_sources(index_data = load_example_index_data())
validate_data_sources <- function(
  source_env = .GlobalEnv,
  index_data = NULL,
  index_user_data_key = NULL,
  required_base_cols = c("company", "year"),
  max_sources = 50L,
  max_missing_preview = 60L,
  id = "ohep-data-source-validator"
) {
  .diag_render_report(
    mode = "inventory",
    x = NULL,
    index_data = index_data,
    index_user_data_key = index_user_data_key,
    source_env = source_env,
    required_base_cols = required_base_cols,
    sample_rows = 5L,
    max_missing_preview = max_missing_preview,
    max_sources = max_sources,
    id = id
  )
}

#' Alias for `test_data_mapping()`
#'
#' Convenience alias for users who prefer dotted function naming.
#'
#' @inheritParams test_data_mapping
#'
#' @return A `htmltools` tag object returned by [test_data_mapping()].
#' @export
test.data <- function(
  x = NULL,
  index_data = NULL,
  index_user_data_key = NULL,
  source_env = .GlobalEnv,
  required_base_cols = c("company", "year"),
  sample_rows = 5L,
  max_missing_preview = 40L,
  id = "ohep-data-mapping-probe"
) {
  test_data_mapping(
    x = x,
    index_data = index_data,
    index_user_data_key = index_user_data_key,
    source_env = source_env,
    required_base_cols = required_base_cols,
    sample_rows = sample_rows,
    max_missing_preview = max_missing_preview,
    id = id
  )
}

.diag_render_report <- function(
  mode,
  x,
  index_data,
  index_user_data_key,
  source_env,
  required_base_cols,
  sample_rows,
  max_missing_preview,
  max_sources = 50L,
  id
) {
  if (!is.environment(source_env)) stop("`source_env` must be an environment.", call. = FALSE)

  logs <- data.frame(level = character(0), message = character(0), stringsAsFactors = FALSE)
  add_log <- function(level, message) {
    logs <<- rbind(logs, data.frame(level = as.character(level), message = as.character(message), stringsAsFactors = FALSE))
    invisible(NULL)
  }

  item_required <- character(0)
  if (is.list(index_data) && is.data.frame(index_data$item_data) && ("item_id" %in% names(index_data$item_data))) {
    item_required <- unique(as.character(index_data$item_data$item_id))
    item_required <- item_required[!is.na(item_required) & nzchar(item_required)]
  }

  key_expected <- character(0)
  if (is.data.frame(index_user_data_key) && ("item" %in% names(index_user_data_key))) {
    key_expected <- unique(as.character(index_user_data_key$item))
    key_expected <- key_expected[!is.na(key_expected) & nzchar(key_expected)]
  } else if (is.list(index_user_data_key) && is.data.frame(index_user_data_key$user_data_key) && ("item" %in% names(index_user_data_key$user_data_key))) {
    key_expected <- unique(as.character(index_user_data_key$user_data_key$item))
    key_expected <- key_expected[!is.na(key_expected) & nzchar(key_expected)]
  } else if (is.list(index_data) && is.data.frame(index_data$user_data_key) && ("item" %in% names(index_data$user_data_key))) {
    key_expected <- unique(as.character(index_data$user_data_key$item))
    key_expected <- key_expected[!is.na(key_expected) & nzchar(key_expected)]
  }

  required_cols <- unique(c(required_base_cols, item_required))

  resolve_probe <- function(obj) {
    if (is.null(obj)) {
      if (exists("raw_user_data", envir = source_env, inherits = TRUE)) {
        return(list(name = "raw_user_data", object = get("raw_user_data", envir = source_env, inherits = TRUE), resolved = TRUE))
      }
      return(list(name = NA_character_, object = NULL, resolved = FALSE))
    }

    if (is.character(obj) && length(obj) == 1L && !is.na(obj)) {
      nm <- as.character(obj)
      if (exists(nm, envir = source_env, inherits = TRUE)) {
        return(list(name = nm, object = get(nm, envir = source_env, inherits = TRUE), resolved = TRUE))
      }
      return(list(name = nm, object = NULL, resolved = FALSE))
    }

    list(name = "<inline_object>", object = obj, resolved = TRUE)
  }

  find_vector_source <- function(col) {
    candidates <- c(paste0("table.", col), paste0("user_data.", col), col)
    for (nm in candidates) {
      if (exists(nm, envir = source_env, inherits = TRUE)) {
        obj <- get(nm, envir = source_env, inherits = TRUE)
        if (is.atomic(obj) && is.null(dim(obj))) return(list(name = nm, value = obj))
      }
    }
    list(name = NA_character_, value = NULL)
  }

  make_table <- function(df) {
    if (!is.data.frame(df) || nrow(df) < 1L) return(htmltools::tags$p(class = "diag-empty", "None"))
    htmltools::tags$table(
      class = "diag-table",
      htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), function(nm) htmltools::tags$th(htmltools::htmlEscape(nm))))),
      htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        htmltools::tags$tr(lapply(df[i, , drop = FALSE], function(cell) htmltools::tags$td(htmltools::htmlEscape(as.character(cell)))))
      }))
    )
  }

  preview_missing <- function(x, n = max_missing_preview) {
    x <- as.character(x)
    if (length(x) <= n) return(x)
    c(utils::head(x, n), sprintf("... (%d more)", length(x) - n))
  }

  if (identical(mode, "probe")) {
    probe <- resolve_probe(x)
    obj <- probe$object

    if (!isTRUE(probe$resolved)) {
      add_log("ERROR", sprintf("Object not found in source_env: %s", probe$name))
    } else {
      add_log("INFO", sprintf("Resolved object: %s", probe$name))
    }

    obj_class <- if (!is.null(obj)) paste(class(obj), collapse = ", ") else "<missing>"
    obj_type <- if (!is.null(obj)) typeof(obj) else "<missing>"
    obj_length <- if (!is.null(obj)) length(obj) else NA_integer_
    obj_rows <- if (!is.null(obj) && (is.data.frame(obj) || is.matrix(obj))) nrow(obj) else NA_integer_
    obj_cols <- if (!is.null(obj) && (is.data.frame(obj) || is.matrix(obj))) ncol(obj) else NA_integer_

    present_cols <- if (is.data.frame(obj)) names(obj) else character(0)
    missing_base <- setdiff(required_base_cols, present_cols)
    missing_items <- if (is.data.frame(obj)) setdiff(item_required, present_cols) else item_required

    if (is.data.frame(obj)) {
      add_log("INFO", sprintf("Data frame probe: %d rows x %d cols.", nrow(obj), ncol(obj)))
      if (length(missing_base) > 0L) add_log("ERROR", sprintf("Missing base columns: %s", paste(missing_base, collapse = ", ")))
      if (length(missing_items) > 0L) add_log("WARN", sprintf("Missing %d required item columns.", length(missing_items)))
      if (length(missing_base) == 0L && length(missing_items) == 0L) add_log("PASS", "Required coverage check passed.")
    }

    status <- if (any(logs$level == "ERROR")) "FAIL" else if (any(logs$level == "WARN")) "WARN" else "PASS"

    summary_df <- data.frame(
      metric = c(
        "overall_status", "probe_name", "resolved", "class", "typeof", "length", "nrow", "ncol",
        "required_base_columns", "required_item_columns", "missing_base_columns", "missing_item_columns"
      ),
      value = c(
        status,
        ifelse(is.na(probe$name), "<none>", probe$name),
        ifelse(isTRUE(probe$resolved), "yes", "no"),
        obj_class,
        obj_type,
        as.character(obj_length),
        as.character(obj_rows),
        as.character(obj_cols),
        as.character(length(required_base_cols)),
        as.character(length(item_required)),
        as.character(length(missing_base)),
        as.character(length(missing_items))
      ),
      stringsAsFactors = FALSE
    )

    missing_df <- data.frame(missing_column = preview_missing(unique(c(missing_base, missing_items))), stringsAsFactors = FALSE)
    logs_df <- if (nrow(logs) > 0L) logs else data.frame(level = "INFO", message = "No logs captured.", stringsAsFactors = FALSE)
    preview_df <- if (is.data.frame(obj)) utils::head(obj, as.integer(sample_rows)) else if (is.atomic(obj) && is.null(dim(obj))) data.frame(value = utils::head(obj, as.integer(sample_rows)), stringsAsFactors = FALSE) else NULL

    title_txt <- "Data Mapping Probe"
    badge_status <- status
    primary_cards <- list(
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Summary"), make_table(summary_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Logs"), make_table(logs_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Missing Required Columns"), make_table(missing_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Probe Preview"), make_table(preview_df))
    )

  } else {
    # inventory mode
    env_names <- ls(envir = source_env)
    env_names <- env_names[order(env_names)]
    if (length(env_names) > as.integer(max_sources)) {
      env_names <- utils::head(env_names, as.integer(max_sources))
      add_log("WARN", sprintf("Showing first %d objects only (max_sources cap).", as.integer(max_sources)))
    }

    inventory <- lapply(env_names, function(nm) {
      obj <- tryCatch(get(nm, envir = source_env, inherits = TRUE), error = function(e) NULL)
      is_df <- is.data.frame(obj)
      n_rows <- if (is_df) nrow(obj) else NA_integer_
      n_cols <- if (is_df) ncol(obj) else NA_integer_
      cols <- if (is_df) names(obj) else character(0)
      base_hits <- sum(required_base_cols %in% cols)
      item_hits <- sum(item_required %in% cols)
      data.frame(
        source = nm,
        class = if (is.null(obj)) "<error>" else paste(class(obj), collapse = ", "),
        type = if (is.null(obj)) "<error>" else typeof(obj),
        is_data_frame = is_df,
        nrow = n_rows,
        ncol = n_cols,
        base_hits = base_hits,
        item_hits = item_hits,
        required_item_total = length(item_required),
        stringsAsFactors = FALSE
      )
    })
    inventory_df <- if (length(inventory) > 0L) do.call(rbind, inventory) else data.frame()

    df_only <- if (nrow(inventory_df) > 0L) inventory_df[inventory_df$is_data_frame, , drop = FALSE] else inventory_df
    best_idx <- if (nrow(df_only) > 0L) order(-df_only$base_hits, -df_only$item_hits)[1] else NA_integer_
    best_source <- if (!is.na(best_idx)) as.character(df_only$source[[best_idx]]) else NA_character_

    if (!is.na(best_source)) add_log("INFO", sprintf("Best matching data.frame source: %s", best_source)) else add_log("WARN", "No data.frame sources found in environment scan.")

    full_base <- if (nrow(df_only) > 0L) df_only[df_only$base_hits == length(required_base_cols), , drop = FALSE] else df_only
    full_item <- if (nrow(full_base) > 0L && length(item_required) > 0L) full_base[full_base$item_hits == length(item_required), , drop = FALSE] else full_base

    overall_fail <- FALSE
    if (nrow(full_base) < 1L) {
      add_log("ERROR", "No source contains required base columns company/year.")
      overall_fail <- TRUE
    }
    if (length(item_required) > 0L && nrow(full_item) < 1L) {
      add_log("WARN", "No source has complete required item coverage.")
    }

    if (!overall_fail && (length(item_required) == 0L || nrow(full_item) >= 1L)) add_log("PASS", "Inventory coverage check passed.")

    # Column-level coverage table based on best source when available
    best_cols <- character(0)
    if (!is.na(best_source) && exists(best_source, envir = source_env, inherits = TRUE)) {
      obj <- get(best_source, envir = source_env, inherits = TRUE)
      if (is.data.frame(obj)) best_cols <- names(obj)
    }

    required_df <- data.frame(
      column = required_cols,
      type = ifelse(required_cols %in% required_base_cols, "base", "item"),
      found_in_best_source = required_cols %in% best_cols,
      stringsAsFactors = FALSE
    )

    key_df <- data.frame()
    if (length(key_expected) > 0L) {
      key_df <- data.frame(
        key_item = key_expected,
        found_in_best_source = key_expected %in% best_cols,
        stringsAsFactors = FALSE
      )
    }

    missing_required <- setdiff(required_cols, best_cols)
    missing_df <- data.frame(missing_column = preview_missing(missing_required), stringsAsFactors = FALSE)

    status <- if (any(logs$level == "ERROR")) "FAIL" else if (any(logs$level == "WARN")) "WARN" else "PASS"

    summary_df <- data.frame(
      metric = c(
        "overall_status", "objects_scanned", "data_frames_found", "best_source",
        "required_base_columns", "required_item_columns", "required_total_columns", "missing_required_columns"
      ),
      value = c(
        status,
        as.character(length(env_names)),
        as.character(if (nrow(inventory_df) > 0L) sum(inventory_df$is_data_frame) else 0L),
        ifelse(is.na(best_source), "<none>", best_source),
        as.character(length(required_base_cols)),
        as.character(length(item_required)),
        as.character(length(required_cols)),
        as.character(length(missing_required))
      ),
      stringsAsFactors = FALSE
    )

    logs_df <- if (nrow(logs) > 0L) logs else data.frame(level = "INFO", message = "No logs captured.", stringsAsFactors = FALSE)

    title_txt <- "Data Source Inventory Validator"
    badge_status <- status
    primary_cards <- list(
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Summary"), make_table(summary_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Logs"), make_table(logs_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Source Inventory"), make_table(inventory_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Required Column Coverage"), make_table(required_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Missing Required Columns"), make_table(missing_df)),
      htmltools::tags$div(class = "diag-card", htmltools::tags$h3("Index User Data Key Coverage"), make_table(key_df))
    )
  }

  status_class <- switch(
    badge_status,
    "FAIL" = "diag-fail",
    "WARN" = "diag-warn",
    "diag-pass"
  )

  css <- sprintf(
    "
    #%s {font-family: 'Inter', 'Segoe UI', Arial, sans-serif; color: #1f2a44; background: #f7f9fc; border: 1px solid #d9e2ee; border-radius: 14px; padding: 18px;}
    #%s .diag-head {display:flex; justify-content:space-between; align-items:center; margin-bottom:12px;}
    #%s .diag-title {font-size: 20px; font-weight: 800; margin: 0;}
    #%s .diag-badge {font-size: 12px; font-weight: 800; letter-spacing: .08em; padding: 6px 10px; border-radius: 999px;}
    #%s .diag-pass {background:#daf3e2; color:#0d6b33;}
    #%s .diag-warn {background:#fff2d9; color:#8a5a00;}
    #%s .diag-fail {background:#ffe1e1; color:#8c1d1d;}
    #%s .diag-grid {display:grid; grid-template-columns: 1fr; gap: 12px;}
    #%s .diag-card {background:#ffffff; border:1px solid #d9e2ee; border-radius:12px; padding:12px;}
    #%s .diag-card h3 {margin:0 0 8px 0; font-size:14px; letter-spacing:.04em; text-transform:uppercase;}
    #%s .diag-table {width:100%%; border-collapse: collapse; font-size: 12px;}
    #%s .diag-table th, #%s .diag-table td {border-bottom: 1px solid #edf2f8; padding: 6px 8px; text-align:left; vertical-align: top;}
    #%s .diag-empty {margin:0; color:#6b778c; font-size:12px;}
    ",
    id, id, id, id, id, id, id, id, id, id, id, id, id, id
  )

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$section(
      id = id,
      htmltools::tags$div(
        class = "diag-head",
        htmltools::tags$h2(class = "diag-title", title_txt),
        htmltools::tags$span(class = paste("diag-badge", status_class), badge_status)
      ),
      htmltools::tags$div(class = "diag-grid", primary_cards)
    )
  )
}
