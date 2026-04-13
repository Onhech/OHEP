#' Load Brand Colors Table
#'
#' Load the brand colors token table used by HTML renderers.
#'
#' @param path Optional CSV path. If `NULL`, resolves package extdata.
#'
#' @return Data frame of brand colors.
#' @export
load_brand_colors_table <- function(path = NULL) {
  if (is.null(path)) {
    path <- resolve_extdata_path("branding/colors.csv")
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Build Displayr Static Bundle
#'
#' Build a static in-memory bundle to pair with changing survey/user data in
#' Displayr. This avoids local file-path assumptions inside Displayr.
#'
#' @param index_data Named list with at least `item_data`, and optionally
#'   `user_data_key`, `predictive_data`.
#' @param predictive_data Optional predictive table. Defaults to
#'   `index_data$predictive_data` when present.
#' @param colors_table Optional brand colors table. If `NULL`, loaded from
#'   package extdata.
#' @param bundle_id Optional bundle identifier.
#'
#' @return Named list with `bundle_meta`, `index_data`, `predictive_data`, and
#'   `colors_table`.
#' @export
build_displayr_static_bundle <- function(
  index_data,
  predictive_data = NULL,
  colors_table = NULL,
  bundle_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
) {
  if (!is.list(index_data) || !is.data.frame(index_data$item_data)) {
    stop("`index_data$item_data` is required in static bundle.", call. = FALSE)
  }

  pred <- predictive_data
  if (is.null(pred) && "predictive_data" %in% names(index_data)) {
    pred <- index_data$predictive_data
  }
  if (!is.null(pred) && !is.data.frame(pred)) {
    stop("`predictive_data` must be a data frame when supplied.", call. = FALSE)
  }

  cols <- colors_table
  if (is.null(cols)) {
    cols <- load_brand_colors_table()
  }
  if (!is.data.frame(cols) || !("variable" %in% names(cols))) {
    stop("`colors_table` must be a data frame with at least a `variable` column.", call. = FALSE)
  }

  list(
    bundle_meta = data.frame(
      bundle_id = as.character(bundle_id),
      generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      stringsAsFactors = FALSE
    ),
    index_data = index_data,
    predictive_data = pred,
    colors_table = cols
  )
}

#' Build Displayr Payload
#'
#' Build a Displayr-ready payload from new user data and a static bundle.
#' Produces a fresh analytic snapshot (`marts`) plus attached static artifacts.
#'
#' @param raw_user_data Respondent-level user data for the current refresh.
#' @param static_bundle Output of [build_displayr_static_bundle()].
#' @param snapshot_id Optional snapshot identifier.
#'
#' @return Named list with `payload_meta`, `raw_user_data`, `marts`,
#'   `index_data`, `predictive_data`, and `colors_table`.
#' @export
build_displayr_payload <- function(
  raw_user_data,
  static_bundle,
  snapshot_id = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
) {
  if (!is.data.frame(raw_user_data)) {
    stop("`raw_user_data` must be a data frame.", call. = FALSE)
  }
  if (!is.list(static_bundle) || !is.list(static_bundle$index_data)) {
    stop("`static_bundle` must come from `build_displayr_static_bundle()`.", call. = FALSE)
  }

  marts <- prep_ohep_snapshot(
    raw_user_data = raw_user_data,
    index_data = static_bundle$index_data,
    predictive_data = static_bundle$predictive_data,
    snapshot_id = snapshot_id
  )

  list(
    payload_meta = data.frame(
      snapshot_id = as.character(snapshot_id),
      generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      stringsAsFactors = FALSE
    ),
    raw_user_data = raw_user_data,
    marts = marts,
    index_data = static_bundle$index_data,
    predictive_data = static_bundle$predictive_data,
    colors_table = static_bundle$colors_table
  )
}

#' Build Example Displayr Payload
#'
#' Convenience helper for local/dev testing.
#'
#' @return Named list payload from [build_displayr_payload()].
#' @export
example_displayr_payload <- function() {
  inputs <- example_fundamental_inputs()
  static <- build_displayr_static_bundle(
    index_data = inputs$index_data,
    predictive_data = inputs$index_data$predictive_data
  )
  build_displayr_payload(
    raw_user_data = inputs$user_data$user_data,
    static_bundle = static,
    snapshot_id = "example"
  )
}

#' Validate Displayr Payload Schema
#'
#' Validate a payload object produced by [build_displayr_payload()].
#' Throws clear errors when required top-level objects or key columns are
#' missing.
#'
#' @param payload Named list payload to validate.
#'
#' @return Invisibly `TRUE` when validation passes.
#' @export
validate_displayr_schema <- function(payload) {
  if (!is.list(payload)) {
    stop("`payload` must be a named list.", call. = FALSE)
  }

  required_top <- c(
    "payload_meta",
    "raw_user_data",
    "marts",
    "index_data",
    "predictive_data",
    "colors_table"
  )
  miss_top <- setdiff(required_top, names(payload))
  if (length(miss_top) > 0) {
    stop(
      sprintf(
        "`payload` is missing required top-level fields: %s.",
        paste(miss_top, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.data.frame(payload$payload_meta)) {
    stop("`payload$payload_meta` must be a data frame.", call. = FALSE)
  }
  req_meta <- c("snapshot_id", "generated_at_utc")
  miss_meta <- setdiff(req_meta, names(payload$payload_meta))
  if (length(miss_meta) > 0) {
    stop(
      sprintf("`payload$payload_meta` missing columns: %s.", paste(miss_meta, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.data.frame(payload$raw_user_data)) {
    stop("`payload$raw_user_data` must be a data frame.", call. = FALSE)
  }

  if (!is.list(payload$marts)) {
    stop("`payload$marts` must be a list from `prep_ohep_snapshot()`.", call. = FALSE)
  }
  if (!is.list(payload$index_data)) {
    stop("`payload$index_data` must be a list.", call. = FALSE)
  }
  if (!is.data.frame(payload$index_data$item_data)) {
    stop("`payload$index_data$item_data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(payload$predictive_data) && !is.data.frame(payload$predictive_data)) {
    stop("`payload$predictive_data` must be NULL or a data frame.", call. = FALSE)
  }
  if (!is.data.frame(payload$colors_table)) {
    stop("`payload$colors_table` must be a data frame.", call. = FALSE)
  }
  if (!("variable" %in% names(payload$colors_table))) {
    stop("`payload$colors_table` must include a `variable` column.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Write Displayr Handoff Files
#'
#' Write a validated payload to a folder as CSV/RDS artifacts for Displayr
#' import and debugging.
#'
#' @param payload Output of [build_displayr_payload()].
#' @param out_dir Output directory to create/update.
#' @param include_rds Logical; when `TRUE` also writes `payload.rds`.
#'
#' @return Invisibly returns `out_dir`.
#' @export
write_displayr_handoff_files <- function(payload, out_dir, include_rds = TRUE) {
  validate_displayr_schema(payload)

  if (!is.character(out_dir) || length(out_dir) != 1L || !nzchar(out_dir)) {
    stop("`out_dir` must be a non-empty path string.", call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv <- function(x, path) {
    utils::write.csv(x, path, row.names = FALSE)
  }

  write_csv(payload$raw_user_data, file.path(out_dir, "01_raw_user_data.csv"))
  write_csv(payload$index_data$item_data, file.path(out_dir, "02_index_item_data.csv"))

  if (!is.null(payload$index_data$user_data_key) && is.data.frame(payload$index_data$user_data_key)) {
    write_csv(payload$index_data$user_data_key, file.path(out_dir, "03_index_user_data_key.csv"))
  }
  if (!is.null(payload$predictive_data) && is.data.frame(payload$predictive_data)) {
    write_csv(payload$predictive_data, file.path(out_dir, "04_predictive_data.csv"))
  }
  write_csv(payload$colors_table, file.path(out_dir, "05_colors_table.csv"))

  marts_dir <- file.path(out_dir, "marts")
  dir.create(marts_dir, recursive = TRUE, showWarnings = FALSE)
  marts_names <- names(payload$marts)
  if (is.null(marts_names)) {
    marts_names <- paste0("table_", seq_along(payload$marts))
  }
  for (i in seq_along(payload$marts)) {
    obj <- payload$marts[[i]]
    nm <- marts_names[[i]]
    if (is.data.frame(obj)) {
      write_csv(obj, file.path(marts_dir, sprintf("%02d_%s.csv", i, nm)))
    }
  }

  manifest <- data.frame(
    field = c("snapshot_id", "generated_at_utc", "marts_tables"),
    value = c(
      as.character(payload$payload_meta$snapshot_id[[1]]),
      as.character(payload$payload_meta$generated_at_utc[[1]]),
      paste(marts_names, collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
  write_csv(manifest, file.path(out_dir, "manifest.csv"))

  if (isTRUE(include_rds)) {
    saveRDS(payload, file.path(out_dir, "payload.rds"))
  }

  invisible(normalizePath(out_dir, winslash = "/", mustWork = FALSE))
}
