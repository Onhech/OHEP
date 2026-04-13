run_local_preview <- function(
  out_dir = "preview",
  fundamental = "Purpose",
  id = "ohep-local-preview"
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("`devtools` is required. Install with install.packages('devtools').", call. = FALSE)
  }

  pkg_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "tools/run_local_preview.R"), ".."), mustWork = TRUE)
  devtools::load_all(pkg_root, quiet = TRUE)

  out_dir_abs <- file.path(pkg_root, out_dir)
  dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

  ex <- example_fundamental_inputs()
  html_obj <- fundamental_page(
    fundamental = fundamental,
    index_data = ex$index_data,
    user_data = ex$user_data,
    id = id
  )

  html_path <- file.path(out_dir_abs, "fundamental_page_preview.html")
  htmltools::save_html(html_obj, file = html_path)

  rendered_path <- file.path(out_dir_abs, "fundamental_page_rendered.txt")
  rendered_html <- htmltools::renderTags(html_obj)$html
  writeLines(rendered_html, con = rendered_path, useBytes = TRUE)

  message("Wrote preview HTML: ", normalizePath(html_path, mustWork = TRUE))
  message("Wrote rendered HTML text: ", normalizePath(rendered_path, mustWork = TRUE))

  invisible(list(html = html_path, rendered = rendered_path))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

if (sys.nframe() == 0L) {
  run_local_preview()
}
