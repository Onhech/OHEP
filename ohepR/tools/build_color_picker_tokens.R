#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) == 0) {
  stop("This script should be run with Rscript.")
}

script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

csv_path <- file.path(repo_root, "inst", "extdata", "branding", "colors.csv")
out_path <- file.path(repo_root, "inst", "extdata", "branding", "color_picker_tool", "tokens.js")

if (!file.exists(csv_path)) {
  stop(sprintf("colors.csv not found at: %s", csv_path))
}

df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c("category", "variable", "description", "color", "fundamental_color", "heatmap_color")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(sprintf("colors.csv missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

sanitize <- function(x) {
  x <- ifelse(is.na(x), "", x)
  trimws(as.character(x))
}

esc_js <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x, fixed = TRUE)
  x <- gsub("\r", "", x, fixed = TRUE)
  x
}

rows <- vector("character", nrow(df))
for (i in seq_len(nrow(df))) {
  category <- esc_js(sanitize(df$category[i]))
  variable <- esc_js(sanitize(df$variable[i]))
  description <- esc_js(sanitize(df$description[i]))
  color <- esc_js(sanitize(df$color[i]))
  fundamental_color <- esc_js(sanitize(df$fundamental_color[i]))
  heatmap_color <- esc_js(sanitize(df$heatmap_color[i]))

  rows[i] <- sprintf(
    '  { category: "%s", key: "%s", value: "%s", description: "%s", variable: "%s", fundamental_color: "%s", heatmap_color: "%s" }',
    category, variable, color, description, variable, fundamental_color, heatmap_color
  )
}

lines <- c(
  "/* Auto-generated from inst/extdata/branding/colors.csv by tools/build_color_picker_tokens.R */",
  "window.COLOR_TOKENS = [",
  paste(rows, collapse = ",\n"),
  "];",
  ""
)

writeLines(lines, out_path, useBytes = TRUE)
message(sprintf("Wrote %d tokens to %s", nrow(df), out_path))
