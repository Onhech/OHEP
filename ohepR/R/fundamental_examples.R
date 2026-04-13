#' Example Inputs for Local Testing
#'
#' Returns example index and user datasets.
#'
#' @return Named list containing `index_data` and `user_data`.
#' @export
example_fundamental_inputs <- function() {
  index_data <- load_example_index_data()
  user_data <- load_example_user_data()
  list(index_data = index_data, user_data = user_data)
}

#' Example Prepared Snapshot for Local Testing
#'
#' Runs [prep_ohep_snapshot()] against package extdata and returns marts.
#'
#' @return Named list of prepared snapshot marts.
#' @export
example_ohep_snapshot <- function() {
  inputs <- example_fundamental_inputs()
  prep_ohep_snapshot(
    raw_user_data = inputs$user_data$user_data,
    index_data = inputs$index_data,
    predictive_data = inputs$index_data$predictive_data,
    snapshot_id = "example"
  )
}

resolve_extdata_path <- function(filename) {
  installed <- system.file("extdata", filename, package = "ohepR")
  if (!identical(installed, "")) {
    return(installed)
  }

  local <- file.path("inst", "extdata", filename)
  if (file.exists(local)) {
    return(normalizePath(local, mustWork = TRUE))
  }

  stop(sprintf("Could not find extdata file: %s.", filename), call. = FALSE)
}

#' Load Example Index Data
#'
#' Loads static index benchmark tables from CSV files in `inst/extdata/index_data`.
#'
#' @return Named list with `item_data`, `user_data_key`, and `predictive_data`.
#' @export
load_example_index_data <- function() {
  item_data <- utils::read.csv(
    resolve_extdata_path("index_data/item_data.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  user_data_key <- utils::read.csv(
    resolve_extdata_path("index_data/user_data_key.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  predictive_data <- NULL
  predictive_path <- tryCatch(
    resolve_extdata_path("predictiva_data/predictive_data.csv"),
    error = function(e) NULL
  )
  if (!is.null(predictive_path) && file.exists(predictive_path)) {
    predictive_data <- utils::read.csv(
      normalizePath(predictive_path, mustWork = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  list(
    item_data = item_data,
    user_data_key = user_data_key,
    predictive_data = predictive_data
  )
}

#' Load Example User Data
#'
#' Loads stable user-data test fixtures from CSV files in
#' `inst/extdata/user_data`.
#'
#' @return Named list with `user_data`.
#' @export
load_example_user_data <- function() {
  user_data <- utils::read.csv(
    resolve_extdata_path("user_data/user_data.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  list(
    user_data = user_data
  )
}
