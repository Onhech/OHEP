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

#' Build Displayr Mapping Script
#'
#' Build an R script text block that users can paste into Displayr to stitch
#' uploaded data sets into `index_data` and a computed `snapshot`.
#'
#' @param dataset_names Named list of Displayr data set object names.
#'
#' @return A single character string containing an R script.
build_displayr_mapping_script <- function(dataset_names) {
  sprintf(
    paste(
      "# Paste into a Displayr R Output / R Calculation",
      "# Edit only the DATASET_MAP block below.",
      "",
      "DATASET_MAP <- list(",
      "  raw_user_data = \"%s\",",
      "  index_item_data = \"%s\",",
      "  index_user_data_key = \"%s\",",
      "  predictive_data = \"%s\",",
      "  colors_table = \"%s\"",
      ")",
      "",
      "get_required_dataset <- function(obj_name) {",
      "  if (!exists(obj_name, inherits = TRUE)) {",
      "    stop(sprintf(\"Displayr data set '%%s' was not found.\", obj_name), call. = FALSE)",
      "  }",
      "  obj <- get(obj_name, inherits = TRUE)",
      "  if (!is.data.frame(obj)) {",
      "    stop(sprintf(\"Displayr object '%%s' exists but is not a data frame.\", obj_name), call. = FALSE)",
      "  }",
      "  obj",
      "}",
      "",
      "runtime <- ohepRDisplayr()",
      "",
      "# Expected Displayr data set objects",
      "index_data <- list(",
      "  item_data = get_required_dataset(DATASET_MAP$index_item_data),",
      "  user_data_key = get_required_dataset(DATASET_MAP$index_user_data_key),",
      "  predictive_data = get_required_dataset(DATASET_MAP$predictive_data)",
      ")",
      "colors_table <- get_required_dataset(DATASET_MAP$colors_table)",
      "raw_user_data <- get_required_dataset(DATASET_MAP$raw_user_data)",
      "",
      "snapshot <- prep_ohep_snapshot(",
      "  raw_user_data = raw_user_data,",
      "  index_data = index_data,",
      "  predictive_data = index_data$predictive_data,",
      "  snapshot_id = format(Sys.time(), \"%%Y%%m%%dT%%H%%M%%SZ\", tz = \"UTC\")",
      ")",
      "",
      "# Example outputs:",
      "# company <- as.character(snapshot$company_fundamental_year$company[[1]])",
      "# year <- as.integer(snapshot$company_fundamental_year$year[[1]])",
      "# fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])",
      "# fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)",
      sep = "\n"
    ),
    dataset_names$raw_user_data,
    dataset_names$index_item_data,
    dataset_names$index_user_data_key,
    dataset_names$predictive_data,
    dataset_names$colors_table
  )
}

#' Build Displayr Function Reference Appendix
#'
#' Build a plain-text appendix with syntax and usage notes for exported ohepR
#' functions so users can work without IDE tooltips/help panes.
#'
#' @return Character vector of lines for a `.txt` appendix.
build_displayr_function_reference <- function() {
  ns <- asNamespace("ohepR")
  exports <- sort(getNamespaceExports("ohepR"))

  usage_lines <- function(fn_name) {
    fn <- get(fn_name, envir = ns)
    parts <- deparse(args(fn), width.cutoff = 80)
    parts <- parts[trimws(parts) != "NULL"]
    parts[1] <- sub("^function\\s*", "", parts[1])
    parts[1] <- paste0(fn_name, parts[1])
    paste0("  ", parts)
  }

  render_section <- function(title, body_lines) {
    c(title, body_lines, "")
  }

  render_args <- function(fn_name, arg_docs) {
    fn <- get(fn_name, envir = ns)
    arg_names <- names(formals(fn))
    if (is.null(arg_names) || length(arg_names) == 0L) {
      return(c("  None."))
    }
    out <- character(0)
    for (nm in arg_names) {
      desc <- arg_docs[[nm]]
      if (is.null(desc)) {
        desc <- "See Description."
      }
      out <- c(out, paste0("  - ", nm, ": ", desc))
    }
    out
  }

  render_entry <- function(
    fn_name,
    title,
    purpose,
    description,
    arg_docs,
    value,
    details = NULL,
    examples = NULL,
    see_also = NULL
  ) {
    lines <- c(
      strrep("=", 88),
      paste0("Title: ", title),
      paste0("Function: ", fn_name),
      "",
      render_section("One-sentence purpose", c(paste0("  ", purpose))),
      render_section("Description", paste0("  ", description)),
      render_section("Usage", usage_lines(fn_name)),
      render_section("Arguments", render_args(fn_name, arg_docs))
    )

    if (!is.null(details) && length(details) > 0L) {
      lines <- c(lines, render_section("Details", paste0("  ", details)))
    }

    lines <- c(lines, render_section("Value", paste0("  ", value)))

    if (!is.null(examples) && length(examples) > 0L) {
      lines <- c(lines, render_section("Examples", paste0("  ", examples)))
    }

    if (!is.null(see_also) && length(see_also) > 0L) {
      lines <- c(lines, render_section("See Also", paste0("  ", see_also)))
    }

    c(lines, "")
  }

  color_override_std <- paste(
    "named list; optional; token overrides keyed by `variable` values in",
    "`colors_table` / `inst/extdata/branding/colors.csv`."
  )
  inline_override_std <- "optional inline token overrides passed through `...`."

  docs <- list(
    build_displayr_static_bundle = list(
      title = "Create a Static Bundle for Displayr",
      purpose = "Package stable benchmark/configuration tables into one validated object.",
      description = c(
        "Use this before report rendering to assemble static inputs that change infrequently.",
        "A static bundle usually contains benchmark index tables, predictive edges, and color tokens.",
        "This object is later combined with live respondent data to build a snapshot."
      ),
      arg_docs = list(
        index_data = paste(
          "list, required. Must include `item_data` (data.frame).",
          "May include `user_data_key` and `predictive_data`."
        ),
        predictive_data = paste(
          "data.frame, optional. If NULL, falls back to `index_data$predictive_data` when present."
        ),
        colors_table = "data.frame, optional. Must contain column `variable`. If NULL, loaded from package extdata.",
        bundle_id = "character scalar, optional. Defaults to current UTC timestamp."
      ),
      details = c(
        "`static bundle` in this package means the non-respondent data needed across many reports.",
        "It is the reusable base for template setup in Displayr."
      ),
      value = "Named list with `bundle_meta`, `index_data`, `predictive_data`, and `colors_table`.",
      examples = c(
        "idx <- load_example_index_data()",
        "static <- build_displayr_static_bundle(index_data = idx)"
      ),
      see_also = c("build_displayr_payload()", "write_displayr_template_bootstrap()", "prep_ohep_snapshot()")
    ),

    build_displayr_payload = list(
      title = "Build a Displayr-ready Analytical Payload",
      purpose = "Combine live respondent data with a static bundle and compute a fresh snapshot.",
      description = c(
        "This is a local QA/debug helper for producing one object with both source tables and derived marts.",
        "In template-first Displayr workflows you typically compute marts directly in Displayr instead."
      ),
      arg_docs = list(
        raw_user_data = paste(
          "data.frame, required. Respondent-level table expected by `prep_ohep_snapshot()`.",
          "Must include at least `company`, `year`, and item response columns referenced by index item metadata."
        ),
        static_bundle = "list, required. Output from `build_displayr_static_bundle()`.",
        snapshot_id = "character scalar, optional. Defaults to current UTC timestamp."
      ),
      details = c(
        "`snapshot` means one computed analytics state for a specific data refresh.",
        "`marts` means derived analytics tables optimized for rendering and comparisons."
      ),
      value = paste(
        "Named list with `payload_meta`, `raw_user_data`, `marts`, `index_data`,",
        "`predictive_data`, and `colors_table`."
      ),
      examples = c(
        "inputs <- example_fundamental_inputs()",
        "static <- build_displayr_static_bundle(inputs$index_data)",
        "payload <- build_displayr_payload(inputs$user_data$user_data, static)"
      ),
      see_also = c("prep_ohep_snapshot()", "validate_displayr_schema()")
    ),

    write_displayr_template_bootstrap = list(
      title = "Write Displayr Template Bootstrap Files",
      purpose = "Export the exact files needed to initialize a Displayr template.",
      description = c(
        "Creates two folders under `out_dir`:",
        "- `displayr_payload/`: files to upload or paste into Displayr.",
        "- `displayr_support/`: setup notes, manifest, and optional RDS archive."
      ),
      arg_docs = list(
        static_bundle = "list, required. Output from `build_displayr_static_bundle()`.",
        out_dir = "character scalar path, required. Target directory.",
        include_rds = "logical, optional. If TRUE, writes `displayr_support/template_static_bundle.rds`.",
        dataset_names = paste(
          "named list, optional. Must include `index_item_data`, `index_user_data_key`,",
          "`predictive_data`, `colors_table`, and `raw_user_data`."
        )
      ),
      details = c(
        "Primary script: `displayr_payload/displayr_bootstrap_from_github_rds.R`.",
        "Set commit-SHA pinned raw GitHub URLs for function/static RDS files and edit only `raw_user_data <- ...`.",
        "Fallback script: `displayr_payload/displayr_bootstrap_single_paste.R` for CSV-upload workflows."
      ),
      value = "Returns `out_dir` invisibly.",
      examples = c(
        "idx <- load_example_index_data()",
        "static <- build_displayr_static_bundle(idx)",
        "write_displayr_template_bootstrap(static, out_dir = \"displayr_bootstrap\")"
      ),
      see_also = c("build_displayr_static_bundle()")
    ),

    prep_ohep_snapshot = list(
      title = "Prepare an OHEP Snapshot (Derived Marts)",
      purpose = "Compute derived analytics tables from respondent data and static metadata.",
      description = c(
        "This is the core transformation step that powers downstream renderers.",
        "It calculates item/fundamental/facet metrics, benchmark deltas, prior-year deltas, predictive edges, and metadata lookup tables."
      ),
      arg_docs = list(
        raw_user_data = paste(
          "data.frame, required. Must include `company`, `year`, and item columns.",
          "Item columns are matched using `index_data$item_data$item_id`."
        ),
        index_data = paste(
          "list, required. Must include `item_data` data.frame with required columns:",
          "`item_id`, `item_text`, `fundamental_id`, `facet_id`, `is_reverse_scored`,",
          "`response_scale_min`, `response_scale_max`."
        ),
        predictive_data = "data.frame, optional. Predictive model edges; included in output when provided.",
        snapshot_id = "character scalar, optional. Defaults to current UTC timestamp."
      ),
      details = c(
        "`canonical data` in this package means normalized table structures expected by render functions.",
        "The returned marts are the canonical analytical layer for rendering pages."
      ),
      value = paste(
        "Named list with at least:",
        "`snapshot_meta`, `company_item_year`, `company_fundamental_year`,",
        "`company_facet_year`, `benchmark_item_year`, `predictive_edges`, `metadata_lookup`."
      ),
      examples = c(
        "inputs <- example_fundamental_inputs()",
        "snapshot <- prep_ohep_snapshot(inputs$user_data$user_data, inputs$index_data)"
      ),
      see_also = c("fundamental_page()", "decision_matrix_page()", "write_ohep_snapshot()")
    ),

    fundamental_page = list(
      title = "Render a Fundamental Dashboard Page",
      purpose = "Render the core fundamental page from either marts-mode or legacy raw/index mode.",
      description = c(
        "Supports two modes:",
        "1) Marts mode (recommended): provide `company`, `year`, `fundamental`, and `marts`.",
        "2) Legacy mode: provide `fundamental`, `index_data`, and `user_data`.",
        "The function validates input combinations and raises clear errors when mode requirements are incomplete."
      ),
      arg_docs = list(
        company = "character scalar, required in marts mode; ignored in legacy mode.",
        year = "integer/numeric scalar, required in marts mode; ignored in legacy mode.",
        fundamental = "character scalar or unquoted symbol, required in both modes.",
        marts = "named list, required in marts mode. Expected output from `prep_ohep_snapshot()`.",
        filtered_user_data = paste(
          "data.frame, optional (marts mode only).",
          "If supplied, page metrics are recalculated on this filtered subset while benchmarks remain from snapshot."
        ),
        min_n = "integer, optional. Minimum N threshold for filtered display; default 3.",
        privacy_message = "character scalar, optional. Message when subset N is below threshold.",
        index_data = "list, required in legacy mode only.",
        user_data = "list, required in legacy mode only.",
        id = "character scalar, optional HTML id for CSS scoping.",
        color_overrides = color_override_std,
        benchmark_company = "logical, optional. Show company benchmark column when available.",
        benchmark_index = "logical, optional. Show index benchmark column when available.",
        benchmark_prior = "logical, optional. Show prior-year comparison column when available.",
        "..." = inline_override_std
      ),
      details = c(
        "When `filtered_user_data` is below `min_n`, the page returns a privacy-safe overlay state.",
        "Use marts mode for Displayr workflows."
      ),
      value = "A single `htmltools` tag object for one fundamental page render.",
      examples = c(
        "inputs <- example_fundamental_inputs()",
        "snap <- prep_ohep_snapshot(inputs$user_data$user_data, inputs$index_data)",
        "fundamental_page(company = \"Company2\", year = 2026, fundamental = \"Purpose\", marts = snap)"
      ),
      see_also = c("render_fundamental_page()", "validate_fundamental_page_data()")
    ),

    decision_matrix_page = list(
      title = "Render Decision Matrix Chart(s)",
      purpose = "Render one decision matrix from explicit points or multiple matrices from marts.",
      description = c(
        "Supports two modes:",
        "1) Points mode: provide `points`; returns a single matrix.",
        "2) Marts mode: provide `company`, `year`, and `marts`; returns one per outcome plus optional composite.",
        "Use marts mode for production because it aligns with snapshot tables and benchmark logic."
      ),
      arg_docs = list(
        points = paste(
          "data.frame, optional (points mode). Required columns: `fundamental`, `score`, `impact`.",
          "Optional columns: `label`, `priority`."
        ),
        company = "character scalar, required in marts mode.",
        year = "integer/numeric scalar, required in marts mode.",
        marts = "named list, required in marts mode. Must include `company_fundamental_year` and `predictive_edges`.",
        outcomes = "character vector, optional (marts mode). Defaults to available outcomes excluding `Outcomes`.",
        include_composite = "logical, optional (marts mode). Adds an equally weighted outcomes composite matrix.",
        subset = "character scalar, optional. Predictive subset filter; default `All`.",
        significant_only = "logical, optional. If TRUE, keep only significant predictive rows when available.",
        fallback_to_latest_year = "logical, optional. If requested year missing, uses latest available year.",
        as_list = "logical, optional. In marts mode returns named list instead of single `tagList`.",
        id = "character scalar, optional HTML id.",
        theme_kicker = "character scalar, optional subtitle kicker.",
        title = "character scalar, optional chart title.",
        subtitle = "character scalar, optional chart subtitle.",
        x_metric = "character scalar, optional. One of `benchmark_delta`, `prior_delta`, `impact`, `opportunity`.",
        y_metric = "character scalar, optional. One of `impact`, `benchmark_delta`, `prior_delta`, `opportunity`.",
        x_split_mode = "character scalar, optional. One of `auto`, `zero`, `median`.",
        y_split_mode = "character scalar, optional. One of `auto`, `zero`, `median`.",
        y_axis_main = "character scalar, optional y-axis title override.",
        x_axis_main = "character scalar, optional x-axis title override.",
        color_overrides = color_override_std,
        "..." = inline_override_std
      ),
      details = c(
        "Marts mode uses stagger-stable prior benchmark logic for x-axis benchmark deltas.",
        "Output shape depends on mode and `as_list`."
      ),
      value = paste(
        "Points mode: single `htmltools` matrix object.",
        "Marts mode: `tagList` of matrices, or named list when `as_list = TRUE`."
      ),
      examples = c(
        "inputs <- example_fundamental_inputs()",
        "snap <- prep_ohep_snapshot(inputs$user_data$user_data, inputs$index_data)",
        "decision_matrix_page(company = \"Company2\", year = 2026, marts = snap)"
      ),
      see_also = c("prep_ohep_snapshot()", "fundamental_page()")
    ),

    demographics_page = list(
      title = "Render a Demographics Composer Page (2x2)",
      purpose = "Compose up to four typed demographics panels in a fixed 2x2 layout.",
      description = c(
        "Each slot (`tl`, `tr`, `bl`, `br`) accepts a panel object created by a `demo_*` constructor or NULL.",
        "NULL slots render placeholders; no auto-span behavior is applied."
      ),
      arg_docs = list(
        tl = "panel object or NULL. Use `demo_bipolar_split()`, `demo_categorical_bar()`, `demo_categorical_tree()`, or `demo_ordinal_bar()`.",
        tr = "panel object or NULL.",
        bl = "panel object or NULL.",
        br = "panel object or NULL.",
        id = "character scalar, optional HTML id.",
        title = "character scalar, optional page title.",
        subtitle = "character scalar, optional page subtitle.",
        color_overrides = color_override_std,
        "..." = inline_override_std
      ),
      value = "A single `htmltools` page object containing the 2x2 layout.",
      examples = c(
        "p1 <- demo_bipolar_split(data.frame(label=c(\"A\",\"B\"), count=c(40,60), pct=c(40,60)))",
        "p2 <- demo_categorical_bar(data.frame(category=c(\"X\",\"Y\"), value=c(70,30)))",
        "demographics_page(tl = p1, tr = p2)"
      ),
      see_also = c("demo_bipolar_split()", "demo_categorical_bar()", "demo_categorical_tree()", "demo_ordinal_bar()")
    ),

    demo_bipolar_split = list(
      title = "Create a Bipolar Split Panel Specification",
      purpose = "Construct a validated panel spec for two-group split displays.",
      description = c(
        "Use this constructor for demographics pages where two opposing groups are shown.",
        "Validation enforces exactly two rows and percentages summing to approximately 100."
      ),
      arg_docs = list(
        data = paste(
          "data.frame, required. Must have exactly 2 rows and columns:",
          "`label` (non-empty), `count` (non-negative numeric), `pct` (non-negative numeric, sums to 100 +/- 1)."
        ),
        title = "character scalar, optional panel title.",
        subtitle = "character scalar, optional panel subtitle."
      ),
      value = "Typed panel spec (`ohep_demo_panel`) for use in `demographics_page()`.",
      examples = c(
        "demo_bipolar_split(data.frame(label=c(\"Manager\",\"Staff\"), count=c(40,160), pct=c(20,80)))"
      ),
      see_also = c("demographics_page()")
    ),

    demo_categorical_bar = list(
      title = "Create a Categorical Bar Panel Specification",
      purpose = "Construct a validated categorical bar panel spec.",
      description = c(
        "Use this for category-value comparisons in the demographics composer.",
        "Input rows can be automatically sorted descending by `value`."
      ),
      arg_docs = list(
        data = paste(
          "data.frame, required. Required columns: `category` (non-empty), `value` (non-negative numeric).",
          "Optional column: `color_key`."
        ),
        title = "character scalar, optional panel title.",
        subtitle = "character scalar, optional panel subtitle.",
        sort_desc = "logical, optional. Default TRUE."
      ),
      value = "Typed panel spec (`ohep_demo_panel`) for use in `demographics_page()`.",
      examples = c(
        "demo_categorical_bar(data.frame(category=c(\"Ops\",\"Clinical\"), value=c(62,38)))"
      ),
      see_also = c("demographics_page()")
    ),

    demo_categorical_tree = list(
      title = "Create a Categorical Tree Panel Specification",
      purpose = "Construct a validated treemap-style panel spec from category shares.",
      description = c(
        "Use this for proportional category composition views.",
        "Values are normalized to shares for tile sizing."
      ),
      arg_docs = list(
        data = paste(
          "data.frame, required. Required columns: `category` (non-empty), `value` (non-negative numeric).",
          "Optional columns: `short_label`, `color_key`."
        ),
        title = "character scalar, optional panel title.",
        subtitle = "character scalar, optional panel subtitle."
      ),
      value = "Typed panel spec (`ohep_demo_panel`) for use in `demographics_page()`.",
      examples = c(
        "demo_categorical_tree(data.frame(category=c(\"18-29\",\"30-39\",\"40+\"), value=c(25,35,40)))"
      ),
      see_also = c("demographics_page()")
    ),

    demo_ordinal_bar = list(
      title = "Create an Ordinal Distribution Panel Specification",
      purpose = "Construct a validated panel spec for ordered response bins.",
      description = c(
        "Use this for Likert-style or ordered category distributions.",
        "Validation enforces non-negative percentages summing to approximately 100."
      ),
      arg_docs = list(
        data = paste(
          "data.frame, required. Required columns: `label` (non-empty), `pct` (non-negative numeric, sums to 100 +/- 1)."
        ),
        title = "character scalar, optional panel title.",
        subtitle = "character scalar, optional panel subtitle.",
        callout_index = "integer, optional 1-based segment index. Falls back to smallest segment if invalid.",
        callout_value = "character, optional. Defaults to selected segment percentage text."
      ),
      value = "Typed panel spec (`ohep_demo_panel`) for use in `demographics_page()`.",
      examples = c(
        "demo_ordinal_bar(data.frame(label=c(\"SD\",\"D\",\"N\",\"A\",\"SA\"), pct=c(5,10,15,40,30)))"
      ),
      see_also = c("demographics_page()")
    ),

    render_fundamental_page = list(
      title = "Render Fundamental Page from Canonical Dashboard Data",
      purpose = "Render a fundamental page when canonical dashboard tables are already assembled.",
      description = c(
        "This is a lower-level renderer compared with `fundamental_page()`.",
        "Use it when you have already built `dashboard_data` and only need rendering."
      ),
      arg_docs = list(
        dashboard_data = paste(
          "named list, required. Must include data frames for `fundamental`, `outcomes`, and `items`",
          "matching `validate_fundamental_page_data()` contract."
        ),
        id = "character scalar, optional HTML id.",
        color_overrides = color_override_std,
        benchmark_company = "logical, optional. Include company benchmark column.",
        benchmark_index = "logical, optional. Include index benchmark column.",
        benchmark_prior = "logical, optional. Include prior-year benchmark column.",
        "..." = inline_override_std
      ),
      value = "A single `htmltools` fundamental page object.",
      examples = c(
        "## Typically called indirectly via fundamental_page(...)",
        "## after data prep from marts."
      ),
      see_also = c("fundamental_page()", "validate_fundamental_page_data()")
    ),

    validate_fundamental_page_data = list(
      title = "Validate Canonical Fundamental Dashboard Data",
      purpose = "Check that dashboard tables required by `render_fundamental_page()` are structurally valid.",
      description = c(
        "Use this before rendering when constructing dashboard tables manually."
      ),
      arg_docs = list(
        dashboard_data = "named list, required. Canonical dashboard sections."
      ),
      value = "Returns `dashboard_data` invisibly when valid; otherwise throws an error.",
      examples = c(
        "## validate_fundamental_page_data(dashboard_data)"
      ),
      see_also = c("render_fundamental_page()")
    ),

    enps_page = list(
      title = "Render an eNPS Page",
      purpose = "Render an eNPS card and distribution chart.",
      description = c(
        "If `enps_data` is NULL, a built-in example dataset is used.",
        "Use custom input in production."
      ),
      arg_docs = list(
        enps_data = paste(
          "named list, optional. `summary` one-row data.frame (optional columns: `title`, `subtitle`, `score`, `score_delta`, `delta_label`);",
          "`distribution` data.frame with `rating` (0-10) and `pct` (should sum to ~100)."
        ),
        id = "character scalar, optional HTML id.",
        color_overrides = color_override_std,
        "..." = inline_override_std
      ),
      value = "A single `htmltools` eNPS page/widget object.",
      examples = c(
        "enps_page()"
      ),
      see_also = c("model_page()", "item_distribution_page()")
    ),

    heatmap_page = list(
      title = "Render an Internal Comparison Heatmap Page",
      purpose = "Render one or more heatmap tables with column-relative color encoding.",
      description = c(
        "If `heatmap_data` is NULL, a built-in example is used.",
        "Each table should place the row label in column 1 and numeric comparison values in the remaining columns."
      ),
      arg_docs = list(
        heatmap_data = paste(
          "named list, optional. Keys include `title`, `subtitle`, `legend_low`, `legend_high`, and `tables`.",
          "`tables` must be a named list of data.frames where first column is category label and other columns are numeric."
        ),
        id = "character scalar, optional HTML id.",
        color_overrides = color_override_std,
        "..." = inline_override_std
      ),
      value = "A single `htmltools` heatmap page object.",
      examples = c("heatmap_page()"),
      see_also = c("item_distribution_page()", "model_page()")
    ),

    item_distribution_page = list(
      title = "Render an Item-level Distribution Page",
      purpose = "Render item-level agreement/neutral/disagreement distributions with deltas.",
      description = c(
        "If `item_distribution_data` is NULL, a built-in example is used."
      ),
      arg_docs = list(
        item_distribution_data = paste(
          "named list, optional. `summary` one-row data.frame with optional labels;",
          "`items` data.frame requires columns `label`, `mean`, `disagree_pct`, `neutral_pct`, `agree_pct`, `vs_industry`, `vs_prior`."
        ),
        id = "character scalar, optional HTML id.",
        color_overrides = color_override_std,
        "..." = inline_override_std
      ),
      value = "A single `htmltools` item distribution page object.",
      examples = c("item_distribution_page()"),
      see_also = c("fundamental_page()")
    ),

    model_page = list(
      title = "Render a Model Representation Page",
      purpose = "Render percentile tracks for fundamentals and outcomes on a model page.",
      description = c(
        "If `model_data` is NULL, a built-in example is used.",
        "Designed for side-by-side interpretation of current percentile positions and optional prior deltas."
      ),
      arg_docs = list(
        model_data = paste(
          "named list, optional. `summary` one-row labels table;",
          "`fundamentals` data.frame requires `label`, `percentile` and may include `prior_percentile`, `raw_avg`, `delta`, `shape`;",
          "`outcomes` data.frame requires `label`, `percentile` and may include `prior_percentile`, `raw_avg`, `delta`, `shape`."
        ),
        id = "character scalar, optional HTML id.",
        color_overrides = color_override_std,
        "..." = inline_override_std
      ),
      value = "A single `htmltools` model page object.",
      examples = c("model_page()"),
      see_also = c("enps_page()", "heatmap_page()")
    ),

    outcome_page = list(
      title = "Outcome Page (Not Implemented)",
      purpose = "Reserve a public API placeholder for a future outcomes renderer.",
      description = c(
        "This function currently always errors with `outcome_page() is not implemented yet`.",
        "Do not use in production until an implementation is added."
      ),
      arg_docs = list(
        "..." = "reserved for future inputs."
      ),
      value = "No return value; always raises an error.",
      examples = c(
        "## outcome_page(...)  # currently throws an error"
      ),
      see_also = c("fundamental_page()", "model_page()")
    ),

    validate_displayr_schema = list(
      title = "Validate a Displayr Payload Object",
      purpose = "Check whether a payload object has all required tables and columns.",
      description = c(
        "Use this in QA scripts before writing or consuming payload objects."
      ),
      arg_docs = list(
        payload = "named list, required. Typically output from `build_displayr_payload()`."
      ),
      value = "Returns TRUE invisibly when valid; otherwise throws an error with missing field details.",
      examples = c(
        "p <- example_displayr_payload()",
        "validate_displayr_schema(p)"
      ),
      see_also = c("build_displayr_payload()")
    ),

    write_ohep_snapshot = list(
      title = "Write Snapshot Tables to Disk",
      purpose = "Persist computed snapshot tables as either RDS or CSV files.",
      description = c(
        "Use when you want reproducible snapshot artifacts for audit, handoff, or debugging."
      ),
      arg_docs = list(
        snapshot = "named list, required. Output from `prep_ohep_snapshot()`.",
        path = "character scalar path, required. Target directory.",
        format = "character scalar, optional. One of `rds` or `csv`; default `rds`."
      ),
      value = "Returns `path` invisibly.",
      examples = c(
        "## write_ohep_snapshot(snapshot, path = \"snapshots\", format = \"csv\")"
      ),
      see_also = c("load_ohep_snapshot()", "prep_ohep_snapshot()")
    ),

    load_ohep_snapshot = list(
      title = "Load Snapshot Tables from Disk or In-memory",
      purpose = "Read a saved snapshot from RDS/CSV files or accept an already-loaded list.",
      description = c(
        "This function supports two inputs:",
        "1) a directory path containing snapshot files, or",
        "2) a named list of tables (returned unchanged)."
      ),
      arg_docs = list(
        path_or_tables = "character path or named list, required."
      ),
      value = "Named list of snapshot tables.",
      examples = c(
        "## snap <- load_ohep_snapshot(\"snapshots\")"
      ),
      see_also = c("write_ohep_snapshot()")
    ),

    ohepRDisplayr = list(
      title = "Create the ohepR Runtime Environment",
      purpose = "Instantiate runtime methods used by rendering and utility helpers.",
      description = c(
        "Most users call high-level wrappers and do not need this directly.",
        "Use this when you need access to lower-level runtime helpers."
      ),
      arg_docs = list(),
      value = "Environment containing rendering, validation, and utility functions.",
      examples = c(
        "rt <- ohepRDisplayr()",
        "names(rt)"
      ),
      see_also = c("fundamental_page()", "decision_matrix_page()")
    ),

    load_brand_colors_table = list(
      title = "Load Brand Color Token Table",
      purpose = "Read the color token table used by renderer theme overrides.",
      description = c(
        "When no path is provided, reads package extdata `branding/colors.csv`."
      ),
      arg_docs = list(
        path = "character scalar path, optional. Defaults to packaged colors file."
      ),
      value = "Data frame of color tokens.",
      examples = c("cols <- load_brand_colors_table()"),
      see_also = c("write_displayr_template_bootstrap()")
    ),

    load_example_index_data = list(
      title = "Load Example Index Metadata Tables",
      purpose = "Load packaged benchmark/index input tables for testing.",
      description = c(
        "Returns a list containing `item_data`, `user_data_key`, and optional `predictive_data`."
      ),
      arg_docs = list(),
      value = "Named list of example index tables.",
      examples = c("idx <- load_example_index_data()"),
      see_also = c("example_fundamental_inputs()")
    ),

    load_example_user_data = list(
      title = "Load Example Respondent Data",
      purpose = "Load packaged respondent-level fixture data for local tests.",
      description = c("Useful for smoke tests and example snapshots."),
      arg_docs = list(),
      value = "Named list with `user_data` data.frame.",
      examples = c("u <- load_example_user_data()"),
      see_also = c("example_fundamental_inputs()")
    ),

    example_fundamental_inputs = list(
      title = "Get Example Inputs (Index + User Data)",
      purpose = "Return both example index tables and example respondent data in one object.",
      description = c("Convenience wrapper around `load_example_index_data()` and `load_example_user_data()`."),
      arg_docs = list(),
      value = "Named list with `index_data` and `user_data`.",
      examples = c("inputs <- example_fundamental_inputs()"),
      see_also = c("example_ohep_snapshot()")
    ),

    example_ohep_snapshot = list(
      title = "Build an Example Snapshot",
      purpose = "Run full snapshot prep using packaged example data.",
      description = c("Useful for quick sanity checks without external files."),
      arg_docs = list(),
      value = "Named list of snapshot marts.",
      examples = c("snap <- example_ohep_snapshot()"),
      see_also = c("prep_ohep_snapshot()")
    ),

    example_displayr_payload = list(
      title = "Build an Example Displayr Payload",
      purpose = "Create a complete sample payload from packaged test fixtures.",
      description = c("Primarily useful for local QA and development checks."),
      arg_docs = list(),
      value = "Named payload list from `build_displayr_payload()`.",
      examples = c("payload <- example_displayr_payload()"),
      see_also = c("build_displayr_payload()", "validate_displayr_schema()")
    ),

    get_delta_pill = list(
      title = "Map Delta Values to Pill CSS Class",
      purpose = "Classify numeric change values as positive, negative, or neutral display pills.",
      description = c("Small helper used by renderers; not a primary report-construction function."),
      arg_docs = list(
        delta_value = "numeric scalar, required."
      ),
      value = "Character scalar class: `dp-pos`, `dp-neg`, or `dp-neu`.",
      examples = c("get_delta_pill(3)", "get_delta_pill(-1)"),
      see_also = c("get_sentiment_class()")
    ),

    get_sentiment_class = list(
      title = "Map Percentage to Sentiment CSS Class",
      purpose = "Classify a score into agree/neutral/disagree sentiment class names.",
      description = c("Small helper used by renderers; not a primary report-construction function."),
      arg_docs = list(
        score = "numeric scalar, required."
      ),
      value = "Character scalar class: `bg-agree`, `bg-neutral`, or `bg-disagree`.",
      examples = c("get_sentiment_class(80)", "get_sentiment_class(45)"),
      see_also = c("get_delta_pill()")
    )
  )

  # Make sure every exported function gets an entry. Fallback sections are
  # concise but still use the standard structure.
  missing_docs <- setdiff(exports, names(docs))
  if (length(missing_docs) > 0L) {
    for (nm in missing_docs) {
      docs[[nm]] <- list(
        title = nm,
        purpose = "Public exported function.",
        description = "See package source and examples for usage context.",
        arg_docs = list(),
        value = "See function behavior.",
        examples = c(paste0("## ?", nm))
      )
    }
  }

  header <- c(
    "OHEP Function Reference Appendix",
    "",
    "Workflow overview",
    "  1) Load static inputs (`load_example_index_data()` or your own index tables).",
    "  2) Build static bundle (`build_displayr_static_bundle()`).",
    "  3) In Displayr, map uploaded tables and compute `snapshot` via `prep_ohep_snapshot()`.",
    "  4) Render pages (`fundamental_page()`, `decision_matrix_page()`, `demographics_page()`, etc.).",
    "",
    "Terminology",
    "  - static bundle: reusable non-respondent inputs (index, predictive, colors).",
    "  - snapshot: one computed analytics state for a given data refresh.",
    "  - marts: derived analytic tables in the snapshot used by renderers.",
    "  - canonical data: normalized table structures expected by renderer internals.",
    "",
    "Groups",
    "  - Core workflow: data assembly, snapshot prep, payload/bootstrap writers.",
    "  - Renderers: functions that return HTML chart/page objects.",
    "  - Panel constructors: typed inputs for composed pages.",
    "  - Validation and persistence: schema checks and snapshot I/O.",
    "  - Utilities: small class/color helpers and runtime constructor.",
    ""
  )

  order <- c(
    "build_displayr_static_bundle",
    "write_displayr_template_bootstrap",
    "prep_ohep_snapshot",
    "build_displayr_payload",
    "validate_displayr_schema",
    "fundamental_page",
    "render_fundamental_page",
    "decision_matrix_page",
    "demographics_page",
    "demo_bipolar_split",
    "demo_categorical_bar",
    "demo_categorical_tree",
    "demo_ordinal_bar",
    "enps_page",
    "heatmap_page",
    "item_distribution_page",
    "model_page",
    "outcome_page",
    "write_ohep_snapshot",
    "load_ohep_snapshot",
    "validate_fundamental_page_data",
    "ohepRDisplayr",
    "load_brand_colors_table",
    "load_example_index_data",
    "load_example_user_data",
    "example_fundamental_inputs",
    "example_ohep_snapshot",
    "example_displayr_payload",
    "get_delta_pill",
    "get_sentiment_class"
  )

  order <- unique(c(order, setdiff(exports, order)))

  out <- header
  for (nm in order) {
    meta <- docs[[nm]]
    out <- c(
      out,
      render_entry(
        fn_name = nm,
        title = meta$title,
        purpose = meta$purpose,
        description = meta$description,
        arg_docs = meta$arg_docs,
        value = meta$value,
        details = meta$details,
        examples = meta$examples,
        see_also = meta$see_also
      )
    )
  }

  out
}

#' Write Displayr Template Bootstrap Files
#'
#' Write a streamlined bootstrap payload for template-based Displayr workflows.
#' Exports only the files required for Displayr setup.
#'
#' @param static_bundle Output of [build_displayr_static_bundle()].
#' @param out_dir Output directory to create/update.
#' @param include_rds Logical; when `TRUE` writes `template_static_bundle.rds`.
#' @param dataset_names Named list controlling expected Displayr object names:
#'   `index_item_data`, `index_user_data_key`, `predictive_data`, `colors_table`,
#'   `raw_user_data`.
#'
#' @return Invisibly returns `out_dir`.
#' @export
write_displayr_template_bootstrap <- function(
  static_bundle,
  out_dir,
  include_rds = TRUE,
  dataset_names = list(
    index_item_data = "index_item_data",
    index_user_data_key = "index_user_data_key",
    predictive_data = "predictive_data",
    colors_table = "colors_table",
    raw_user_data = "raw_user_data"
  )
) {
  if (!is.list(static_bundle) || !is.list(static_bundle$index_data) || !is.data.frame(static_bundle$index_data$item_data)) {
    stop("`static_bundle` must come from `build_displayr_static_bundle()`.", call. = FALSE)
  }
  if (!is.data.frame(static_bundle$colors_table)) {
    stop("`static_bundle$colors_table` must be a data frame.", call. = FALSE)
  }
  if (!is.character(out_dir) || length(out_dir) != 1L || !nzchar(out_dir)) {
    stop("`out_dir` must be a non-empty path string.", call. = FALSE)
  }

  req_names <- c("index_item_data", "index_user_data_key", "predictive_data", "colors_table", "raw_user_data")
  if (!is.list(dataset_names) || length(setdiff(req_names, names(dataset_names))) > 0L) {
    stop("`dataset_names` must include: index_item_data, index_user_data_key, predictive_data, colors_table, raw_user_data.", call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  payload_dir <- file.path(out_dir, "displayr_payload")
  support_dir <- file.path(out_dir, "displayr_support")
  dir.create(payload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(support_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(x, path) utils::write.csv(x, path, row.names = FALSE)

  write_csv(static_bundle$index_data$item_data, file.path(payload_dir, "01_index_item_data.csv"))
  if (!is.null(static_bundle$index_data$user_data_key) && is.data.frame(static_bundle$index_data$user_data_key)) {
    write_csv(static_bundle$index_data$user_data_key, file.path(payload_dir, "02_index_user_data_key.csv"))
  }
  if (!is.null(static_bundle$predictive_data) && is.data.frame(static_bundle$predictive_data)) {
    write_csv(static_bundle$predictive_data, file.path(payload_dir, "03_predictive_data.csv"))
  }
  write_csv(static_bundle$colors_table, file.path(payload_dir, "04_colors_table.csv"))

  mapping_script <- build_displayr_mapping_script(dataset_names = dataset_names)

  # Build a single pasteable script containing all exported ohepR functions
  # followed by the dataset mapping block.
  ns <- asNamespace("ohepR")
  exported <- sort(getNamespaceExports("ohepR"))
  runtime_lines <- character(0)
  tc <- textConnection("runtime_lines", open = "w", local = TRUE)
  dump(
    list = exported,
    file = tc,
    envir = ns
  )
  close(tc)

  single_paste_path <- file.path(payload_dir, "displayr_bootstrap_single_paste.R")
  con <- file(single_paste_path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(
    c(
      "# Single-paste Displayr bootstrap script",
      "# 1) Upload CSVs from this folder.",
      "# 2) Paste this whole script into one Displayr R Output.",
      "# 3) Edit DATASET_MAP only, then run.",
      "",
      "# ohepR runtime function bundle for Displayr",
      ""
    ),
    con = con,
    useBytes = TRUE
  )
  writeLines(runtime_lines, con = con, useBytes = TRUE)
  writeLines(c("", mapping_script), con = con, useBytes = TRUE)
  close(con)
  on.exit(NULL, add = FALSE)

  functions_bundle <- list(
    exported = exported,
    runtime_script_lines = runtime_lines,
    generated_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
  if (isTRUE(include_rds)) {
    saveRDS(functions_bundle, file.path(support_dir, "template_functions_bundle.rds"))
  }

  github_loader_lines <- c(
    "# Displayr GitHub RDS Bootstrap Script",
    "# PRIMARY FLOW: load ohepR functions + static data from GitHub-hosted RDS files.",
    "# Pin URLs to a commit SHA for reproducibility.",
    "# Edit the CONFIG block and raw_user_data assignment only.",
    "",
    "CONFIG <- list(",
    "  # Example pinned URL:",
    "  # https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_functions_bundle.rds",
    "  functions_rds_url = \"https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_functions_bundle.rds\",",
    "  static_bundle_rds_url = \"https://raw.githubusercontent.com/<owner>/<repo>/<commit-sha>/<path>/displayr_support/template_static_bundle.rds\"",
    ")",
    "",
    "read_rds_url <- function(u) {",
    "  con <- url(u, open = \"rb\")",
    "  on.exit(close(con), add = TRUE)",
    "  readRDS(con)",
    "}",
    "",
    "fn_bundle <- read_rds_url(CONFIG$functions_rds_url)",
    "if (!is.list(fn_bundle) || !is.character(fn_bundle$runtime_script_lines)) {",
    "  stop(\"functions bundle RDS is not in expected format.\", call. = FALSE)",
    "}",
    "eval(parse(text = fn_bundle$runtime_script_lines), envir = .GlobalEnv)",
    "",
    "static_bundle <- read_rds_url(CONFIG$static_bundle_rds_url)",
    "if (!is.list(static_bundle) || !is.list(static_bundle$index_data)) {",
    "  stop(\"static bundle RDS is not in expected format.\", call. = FALSE)",
    "}",
    "",
    "# IMPORTANT: replace this line with your Displayr respondent table reference.",
    "# Example: raw_user_data <- table.raw_user_data",
    "raw_user_data <- NULL",
    "if (!is.data.frame(raw_user_data)) {",
    "  stop(\"Set `raw_user_data` to a Displayr data-frame reference before running.\", call. = FALSE)",
    "}",
    "",
    "# Preflight schema validation for respondent data",
    "required_base_cols <- c(\"company\", \"year\")",
    "missing_base <- setdiff(required_base_cols, names(raw_user_data))",
    "if (length(missing_base) > 0L) {",
    "  stop(sprintf(\"`raw_user_data` is missing required columns: %s\", paste(missing_base, collapse = \", \")), call. = FALSE)",
    "}",
    "required_item_cols <- unique(as.character(static_bundle$index_data$item_data$item_id))",
    "required_item_cols <- required_item_cols[!is.na(required_item_cols) & nzchar(required_item_cols)]",
    "missing_items <- setdiff(required_item_cols, names(raw_user_data))",
    "if (length(missing_items) > 0L) {",
    "  preview <- utils::head(missing_items, 30)",
    "  suffix <- if (length(missing_items) > 30L) \" ...\" else \"\"",
    "  stop(sprintf(",
    "    \"`raw_user_data` is missing %d required item columns. First missing: %s%s\",",
    "    length(missing_items), paste(preview, collapse = \", \"), suffix",
    "  ), call. = FALSE)",
    "}",
    "if (ncol(raw_user_data) <= 3L) {",
    "  stop(\"Aggregate-only user data is not supported. Provide respondent-level data with item columns.\", call. = FALSE)",
    "}",
    "",
    "index_data <- static_bundle$index_data",
    "colors_table <- static_bundle$colors_table",
    "",
    "snapshot <- prep_ohep_snapshot(",
    "  raw_user_data = raw_user_data,",
    "  index_data = index_data,",
    "  predictive_data = static_bundle$predictive_data,",
    "  snapshot_id = format(Sys.time(), \"%Y%m%dT%H%M%SZ\", tz = \"UTC\")",
    ")",
    "",
    "# Example:",
    "# company <- as.character(snapshot$company_fundamental_year$company[[1]])",
    "# year <- as.integer(snapshot$company_fundamental_year$year[[1]])",
    "# fundamental <- as.character(snapshot$company_fundamental_year$fundamental_id[[1]])",
    "# fundamental_page(company = company, year = year, fundamental = fundamental, marts = snapshot)"
  )
  writeLines(github_loader_lines, file.path(payload_dir, "displayr_bootstrap_from_github_rds.R"), useBytes = TRUE)

  setup_lines <- c(
    "Displayr Template Bootstrap Setup (Primary: GitHub RDS)",
    "",
    "Primary path:",
    "1) Paste `displayr_payload/displayr_bootstrap_from_github_rds.R` into one Displayr R Output.",
    "2) Set `CONFIG$functions_rds_url` and `CONFIG$static_bundle_rds_url` to pinned commit-SHA raw URLs.",
    "3) Set one line: `raw_user_data <- <Displayr respondent table reference>`.",
    "4) Run script. It creates `index_data`, `colors_table`, and `snapshot`.",
    "",
    "Fallback path (CSV + local single paste):",
    "5) Upload static CSVs from `displayr_payload/` and paste `displayr_payload/displayr_bootstrap_single_paste.R`.",
    sprintf("6) In fallback mode, map names as: %s, %s, %s, %s, %s",
      dataset_names$index_item_data,
      dataset_names$index_user_data_key,
      dataset_names$predictive_data,
      dataset_names$colors_table,
      dataset_names$raw_user_data),
    "",
    "7) Use chart functions with marts = snapshot (e.g., fundamental_page(..., marts = snapshot)).",
    "8) Aggregate-only user data is not supported; respondent-level item columns are required.",
    "9) See function_reference_appendix.txt for syntax and usage of all exported functions."
  )
  writeLines(setup_lines, con = file.path(support_dir, "README_displayr_setup.txt"), useBytes = TRUE)
  writeLines(
    build_displayr_function_reference(),
    con = file.path(support_dir, "function_reference_appendix.txt"),
    useBytes = TRUE
  )

  manifest <- data.frame(
    field = c("bundle_id", "generated_at_utc", "payload_dir", "supports_github_rds_loader"),
    value = c(
      as.character(static_bundle$bundle_meta$bundle_id[[1]]),
      as.character(static_bundle$bundle_meta$generated_at_utc[[1]]),
      "displayr_payload",
      "yes"
    ),
    stringsAsFactors = FALSE
  )
  write_csv(manifest, file.path(support_dir, "manifest.csv"))

  if (isTRUE(include_rds)) {
    saveRDS(static_bundle, file.path(support_dir, "template_static_bundle.rds"))
  }

  invisible(normalizePath(out_dir, winslash = "/", mustWork = FALSE))
}
