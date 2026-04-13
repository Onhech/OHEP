build_displayr_bundle <- function(root = getwd(), out_dir = file.path(root, "inst", "displayr")) {
  root <- normalizePath(root, mustWork = TRUE)
  desc_path <- file.path(root, "DESCRIPTION")
  runtime_path <- file.path(root, "R", "displayr_runtime.R")

  if (!file.exists(desc_path)) {
    stop("Could not find DESCRIPTION. Run from the package root or pass `root`.", call. = FALSE)
  }
  if (!file.exists(runtime_path)) {
    stop("Could not find R/displayr_runtime.R in package root.", call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dcf <- read.dcf(desc_path)
  version <- dcf[1, "Version"]

  local_env <- new.env(parent = baseenv())
  sys.source(runtime_path, envir = local_env)
  if (!exists("ohepRDisplayr", envir = local_env, inherits = FALSE)) {
    stop("`ohepRDisplayr` was not found after sourcing runtime file.", call. = FALSE)
  }

  bundle_factory <- get("ohepRDisplayr", envir = local_env, inherits = FALSE)
  versioned <- file.path(out_dir, paste0("ohepR_", version, ".rds"))
  latest <- file.path(out_dir, "ohepR_latest.rds")

  saveRDS(bundle_factory, versioned)
  saveRDS(bundle_factory, latest)

  message("Wrote: ", versioned)
  message("Wrote: ", latest)

  invisible(list(versioned = versioned, latest = latest, version = version))
}

if (sys.nframe() == 0L) {
  build_displayr_bundle()
}
