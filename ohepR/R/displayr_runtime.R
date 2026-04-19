#' Build the Displayr Runtime Environment
#'
#' Creates a self-contained environment with all renderer functions used by
#' Displayr and local package workflows.
#'
#' @return An environment that contains renderer helpers and public entry points.
#' @export
ohepRDisplayr <- function() {
  env <- new.env(parent = baseenv())

  env$format_ordinal <- function(x) {
    x <- as.integer(round(x))
    if (is.na(x)) {
      return("")
    }
    if ((x %% 100L) %in% c(11L, 12L, 13L)) {
      suffix <- "th"
    } else {
      suffix <- switch(as.character(x %% 10L), "1" = "st", "2" = "nd", "3" = "rd", "th")
    }
    paste0(x, suffix)
  }

  env$format_ordinal_html <- function(x) {
    x <- as.integer(round(x))
    if (is.na(x)) {
      return("")
    }
    if ((x %% 100L) %in% c(11L, 12L, 13L)) {
      suffix <- "th"
    } else {
      suffix <- switch(as.character(x %% 10L), "1" = "st", "2" = "nd", "3" = "rd", "th")
    }
    paste0(
      x,
      "<sup style=\"font-size:0.62em;line-height:0;vertical-align:super;\">",
      suffix,
      "</sup>"
    )
  }

  env$format_signed <- function(x, digits = 0L) {
    if (is.na(x)) {
      return("0")
    }
    fmt <- paste0("%.", digits, "f")
    val <- sprintf(fmt, abs(x))
    if (x > 0) {
      paste0("+", val)
    } else if (x < 0) {
      paste0("-", val)
    } else {
      paste0("0", if (digits > 0) substr(val, 2, nchar(val)) else "")
    }
  }

  env$get_sentiment_class <- function(score) {
    if (is.na(score)) {
      return("bg-neutral")
    }
    if (score >= 67) {
      "bg-agree"
    } else if (score >= 34) {
      "bg-neutral"
    } else {
      "bg-disagree"
    }
  }

  env$get_delta_pill <- function(delta_value) {
    if (is.na(delta_value) || delta_value == 0) {
      "dp-neu"
    } else if (delta_value > 0) {
      "dp-pos"
    } else {
      "dp-neg"
    }
  }

  env$get_outcome_class <- function(percentile) {
    if (is.na(percentile)) {
      return("color-watch")
    }
    if (percentile < 40) {
      "color-risk"
    } else if (percentile < 60) {
      "color-watch"
    } else {
      "color-good"
    }
  }

  env$get_status_text <- function(percentile) {
    if (is.na(percentile)) {
      return("Industry Standard")
    }
    if (percentile < 40) {
      "Area for Growth"
    } else if (percentile < 60) {
      "Industry Standard"
    } else if (percentile < 85) {
      "Above Standard"
    } else {
      "Industry Leader"
    }
  }

  env$get_status_pill_class <- function(percentile) {
    if (is.na(percentile)) {
      return("status-pill status-watch")
    }
    if (percentile < 40) {
      "status-pill status-risk"
    } else if (percentile < 60) {
      "status-pill status-watch"
    } else if (percentile < 85) {
      "status-pill status-good"
    } else {
      "status-pill status-leader"
    }
  }

  env$get_prior_benchmark_label <- function(fundamental_row) {
    "vs '24"
  }

  env$get_active_benchmarks <- function(items, fundamental_row, benchmark_company = TRUE, benchmark_index = TRUE, benchmark_prior = TRUE) {
    candidates <- list(
      list(name = "company", enabled = isTRUE(benchmark_company), value_col = "vs_company", suppress_col = "suppress_vs_company", css_class = "delta-company", label = "vs Co."),
      list(name = "index", enabled = isTRUE(benchmark_index), value_col = "vs_industry", suppress_col = "suppress_vs_industry", css_class = "delta-industry", label = "vs Ind."),
      list(name = "prior", enabled = isTRUE(benchmark_prior), value_col = "vs_prior", suppress_col = "suppress_vs_prior", css_class = "delta-prior", label = env$get_prior_benchmark_label(fundamental_row))
    )
    active <- list()
    for (cand in candidates) {
      if (!cand$enabled || !(cand$value_col %in% names(items))) {
        next
      }
      vals <- suppressWarnings(as.numeric(items[[cand$value_col]]))
      has_available <- any(!is.na(vals))
      if (!has_available) {
        next
      }
      active[[length(active) + 1L]] <- cand
    }
    active
  }

  env$normalize_sentiment <- function(disagree_pct, neutral_pct, agree_pct) {
    vals <- c(disagree_pct, neutral_pct, agree_pct)
    if (any(is.na(vals))) {
      stop("Sentiment percentages cannot be NA.", call. = FALSE)
    }
    if (any(vals < 0 | vals > 100)) {
      stop("Sentiment percentages must be between 0 and 100.", call. = FALSE)
    }
    total <- sum(vals)
    if (total == 100) {
      return(vals)
    }
    if (total %in% c(99, 101)) {
      ix <- which.max(vals)
      vals[ix] <- vals[ix] + (100 - total)
      return(vals)
    }
    stop(
      sprintf("Sentiment percentages must sum to 100 (or 99/101 for rounding), got %s.", total),
      call. = FALSE
    )
  }

  env$escape_text <- function(x) {
    htmltools::htmlEscape(as.character(x), attribute = FALSE)
  }

  env$default_brand_colors <- function() {
    base <- c(
      bg_canvas = "#F8FAFC",
      bg_card = "#FFFFFF",
      bg_secondary = "#F1F5F9",
      bg_overlay = "rgba(255,255,255,0.98)",
      bg_accent_soft = "#F0FDFA",
      border_light = "#F1F5F9",
      border_default = "#E2E8F0",
      border_band = "#E5E7EB",
      border_dark = "#CBD5E1",
      text_primary = "#0F172A",
      text_secondary = "#334155",
      text_tertiary = "#475569",
      text_muted = "#64748B",
      text_faint = "#94A3B8",
      text_decorative = "#CBD5E1",
      text_accent = "#0D9488",
      text_on_dark = "#FFFFFF",
      percentile_z1_core = "#FCD34D",
      percentile_z1_text = "#968548",
      percentile_z1_chip = "#FFEBC4",
      percentile_z2_core = "#B3D6DA",
      percentile_z2_text = "#9DB3B7",
      percentile_z2_chip = "#D3FDFF",
      percentile_z3_core = "#0096A7",
      percentile_z3_text = "#0096A7",
      percentile_z3_chip = "#BBF5FF",
      percentile_z4_core = "#395966",
      percentile_z4_text = "#395966",
      percentile_z4_chip = "#C8F0F1",
      delta_pos_text = "#15803D",
      delta_pos_bg = "#DCFCE7",
      delta_neg_text = "#EB7161",
      delta_neg_bg = "#FEE2E2",
      delta_neu_text = "#64748B",
      delta_neu_bg = "#F1F5F9",
      favorability_agree = "#0096A7",
      favorability_neutral = "#94A3B8",
      favorability_disagree = "#EB7161",
      band_tick = "#0F172A2E",
      band_tick_label = "#64748B",
      band_midpoint = "#94A3B8",
      band_marker = "#111827",
      privacy_item_label = "#94A3B8",
      privacy_overlay_bg = "rgba(255,255,255,0.85)",
      privacy_overlay_border = "#CBD5E1",
      privacy_overlay_text = "#475569",
      privacy_overlay_shadow = "rgba(15,23,42,0.06)",
      page_privacy_card_bg = "rgba(255,255,255,0.98)",
      page_privacy_card_border = "#CBD5E1",
      page_privacy_card_shadow = "rgba(15,23,42,0.10)",
      page_privacy_icon_color = "#0D9488",
      page_privacy_icon_bg = "#F0FDFA",
      page_privacy_eyebrow = "#64748B",
      page_privacy_title = "#0F172A",
      page_privacy_body = "#475569",
      page_privacy_disclaimer = "#94A3B8"
    )

    matrix <- c(
      matrix_shell_bg = base[["border_default"]],
      matrix_slide_bg = base[["bg_canvas"]],
      matrix_header_divider = base[["border_dark"]],
      matrix_theme_kicker = base[["text_tertiary"]],
      matrix_title = base[["text_primary"]],
      matrix_subtitle = base[["text_muted"]],
      matrix_axis_line = base[["text_faint"]],
      matrix_axis_text = base[["text_muted"]],
      matrix_axis_main = base[["text_secondary"]],
      matrix_plot_bg = base[["bg_card"]],
      matrix_quadrant_tl_bg = "#FFF1F2",
      matrix_quadrant_tl_text = "#E11D48",
      matrix_quadrant_tr_bg = "#F0FDFA",
      matrix_quadrant_tr_text = "#0F766E",
      matrix_quadrant_bl_bg = base[["bg_canvas"]],
      matrix_quadrant_bl_text = base[["text_muted"]],
      matrix_quadrant_br_bg = base[["bg_secondary"]],
      matrix_quadrant_br_text = base[["text_faint"]],
      matrix_quadrant_divider = base[["border_dark"]],
      matrix_dot_critical = "#E11D48",
      matrix_dot_leverage = "#0F766E",
      matrix_dot_monitor = base[["text_muted"]],
      matrix_dot_maintain = base[["text_muted"]],
      matrix_point_border = base[["bg_card"]],
      matrix_point_shadow = "rgba(0,0,0,0.15)",
      matrix_label_bg = base[["bg_card"]],
      matrix_label_text = base[["text_secondary"]],
      matrix_label_border = base[["border_default"]],
      matrix_label_shadow = "rgba(0,0,0,0.08)",
      matrix_brand_text = base[["text_secondary"]],
      matrix_brand_dot = "#0EA5E9"
    )

    enps <- c(
      enps_shell_bg = base[["border_default"]],
      enps_slide_bg = base[["bg_canvas"]],
      enps_card_bg = base[["bg_card"]],
      enps_card_border = base[["border_default"]],
      enps_card_shadow = "rgba(15,23,42,0.04)",
      enps_title = base[["text_primary"]],
      enps_subtitle = base[["text_muted"]],
      enps_score_value = base[["text_primary"]],
      enps_score_label = base[["text_muted"]],
      enps_delta_bg = base[["delta_pos_bg"]],
      enps_delta_text = "#16A34A",
      enps_axis_baseline = base[["text_secondary"]],
      enps_axis_baseline_label = base[["text_faint"]],
      enps_tug_detractor_bg = "#E11D48",
      enps_tug_passive_bg = base[["text_faint"]],
      enps_tug_promoter_bg = base[["text_accent"]],
      enps_tug_text_on = base[["text_on_dark"]],
      enps_divider = base[["border_light"]],
      enps_dist_axis = base[["border_default"]],
      enps_dist_label = base[["text_muted"]],
      enps_det_text = "#E11D48",
      enps_pas_text = base[["text_faint"]],
      enps_pro_text = base[["text_accent"]]
    )

    heatmap <- c(
      heatmap_slide_bg = base[["bg_canvas"]],
      heatmap_title = base[["text_primary"]],
      heatmap_subtitle = base[["text_muted"]],
      heatmap_legend_text = base[["text_muted"]],
      heatmap_legend_start = "#EB7161",
      heatmap_legend_mid = base[["bg_secondary"]],
      heatmap_legend_end = "#81BBC2",
      heatmap_card_bg = base[["bg_card"]],
      heatmap_card_border = base[["border_default"]],
      heatmap_card_shadow = "rgba(0,0,0,0.04)",
      heatmap_card_title = base[["text_primary"]],
      heatmap_table_header_text = base[["text_muted"]],
      heatmap_table_header_border = base[["border_default"]],
      heatmap_row_label_text = base[["text_secondary"]],
      heatmap_row_border = base[["border_light"]],
      heatmap_pill_text = "#1E293B",
      heatmap_pill_na_bg = base[["bg_canvas"]],
      heatmap_pill_na_text = base[["text_faint"]],
      heatmap_pill_na_border = base[["border_dark"]],
      heatmap_pill_pos_strong = "#81BBC2",
      heatmap_pill_pos_soft = "#D1E6E9",
      heatmap_pill_neutral = base[["bg_secondary"]],
      heatmap_pill_neg_soft = "#FFD8D1",
      heatmap_pill_neg_strong = "#EB7161"
    )

    itemdist <- c(
      itemdist_slide_bg = base[["bg_canvas"]],
      itemdist_card_bg = base[["bg_card"]],
      itemdist_card_border = base[["border_default"]],
      itemdist_card_shadow = "rgba(0,0,0,0.04)",
      itemdist_title = base[["text_primary"]],
      itemdist_subtitle = base[["text_muted"]],
      itemdist_legend_text = base[["text_muted"]],
      itemdist_header_label = base[["text_faint"]],
      itemdist_row_label = base[["text_secondary"]],
      itemdist_row_border = base[["border_light"]],
      itemdist_mean_text = base[["text_primary"]],
      itemdist_stack_bg = base[["bg_secondary"]]
    )

    model <- c(
      model_slide_bg = base[["bg_canvas"]],
      model_card_bg = base[["bg_card"]],
      model_card_border = base[["border_default"]],
      model_card_shadow = "rgba(0,0,0,0.05)",
      model_header_accent = base[["text_primary"]],
      model_title = base[["text_primary"]],
      model_subtitle = base[["text_muted"]],
      model_section_title = base[["text_primary"]],
      model_section_rule = base[["border_light"]],
      model_metric_header = base[["text_faint"]],
      model_label = base[["text_secondary"]],
      model_track_bg = base[["border_default"]],
      model_stem = base[["border_dark"]],
      model_line = base[["text_faint"]],
      model_line_baseline = base[["text_secondary"]],
      model_line_label = base[["text_muted"]],
      model_metric_raw = base[["text_primary"]],
      model_delta_pos = base[["delta_pos_text"]],
      model_delta_neg = base[["delta_neg_text"]],
      model_delta_neu = base[["delta_neu_text"]],
      model_zone_ni = base[["percentile_z1_core"]],
      model_zone_is = base[["percentile_z2_core"]],
      model_zone_as = base[["percentile_z3_core"]],
      model_zone_il = base[["percentile_z4_core"]],
      model_point_ni = "#D97706",
      model_point_is = "#15803D",
      model_point_as = "#047857",
      model_point_il = "#0F766E",
      model_point_border = base[["text_on_dark"]],
      model_point_ghost_bg = base[["bg_card"]],
      model_point_ghost_border = base[["text_faint"]],
      model_legend_text_on = base[["text_on_dark"]]
    )

    openended <- c(
      oe_slide_bg = base[["bg_canvas"]],
      oe_card_bg = base[["bg_card"]],
      oe_border = base[["border_default"]],
      oe_title = base[["text_primary"]],
      oe_sub = base[["text_secondary"]],
      oe_muted = base[["text_muted"]],
      oe_brand = base[["text_accent"]],
      oe_brand_soft = "rgba(15,134,148,0.12)",
      oe_warn = base[["delta_neg_text"]],
      oe_good = base[["delta_pos_text"]]
    )

    demographics <- c(
      demo_page_bg = base[["bg_canvas"]],
      demo_grid_gap = "#E2E8F0",
      demo_panel_bg = base[["bg_card"]],
      demo_panel_border = base[["border_default"]],
      demo_panel_shadow = "rgba(15,23,42,0.04)",
      demo_title = base[["text_primary"]],
      demo_subtitle = base[["text_muted"]],
      demo_panel_title = base[["text_primary"]],
      demo_panel_subtitle = base[["text_muted"]],
      demo_empty_text = base[["text_faint"]],
      demo_bi_axis_mid = base[["text_secondary"]],
      demo_bi_axis_label = base[["text_faint"]],
      demo_bi_side_label_left = "#0EA5E9",
      demo_bi_side_label_right = "#0F766E",
      demo_bi_side_count = base[["text_muted"]],
      demo_bi_bar_left_bg = "#0EA5E9",
      demo_bi_bar_right_bg = "#0F766E",
      demo_bi_bar_text = base[["text_on_dark"]],
      demo_cb_label = base[["text_secondary"]],
      demo_cb_track_bg = base[["bg_secondary"]],
      demo_cb_value_text = base[["text_on_dark"]],
      demo_cb_bar_1 = "#0F766E",
      demo_cb_bar_2 = "#0EA5E9",
      demo_cb_bar_3 = "#14B8A6",
      demo_cb_bar_4 = "#F59E0B",
      demo_cb_bar_5 = "#F97316",
      demo_cb_bar_6 = "#64748B",
      demo_ct_text_on = base[["text_on_dark"]],
      demo_ct_border = "rgba(255,255,255,0.28)",
      demo_ct_tile_1 = "#0F766E",
      demo_ct_tile_2 = "#0EA5E9",
      demo_ct_tile_3 = "#14B8A6",
      demo_ct_tile_4 = "#F59E0B",
      demo_ct_tile_5 = "#F97316",
      demo_ct_tile_6 = "#64748B",
      demo_ob_axis_label = base[["text_muted"]],
      demo_ob_callout_bg = base[["bg_card"]],
      demo_ob_callout_text = base[["text_primary"]],
      demo_ob_callout_line = base[["text_faint"]],
      demo_ob_seg_1 = "#99F6E4",
      demo_ob_seg_2 = "#5EEAD4",
      demo_ob_seg_3 = "#2DD4BF",
      demo_ob_seg_4 = "#0D9488",
      demo_ob_seg_5 = "#0F766E",
      demo_ob_seg_text_dark = "#0F766E",
      demo_ob_seg_text_light = base[["text_on_dark"]]
    )

    c(base, matrix, enps, heatmap, itemdist, model, openended, demographics)
  }

  env$read_brand_colors <- function(graph = "fundamental") {
    defaults <- env$default_brand_colors()
    color_file <- system.file("extdata", "branding", "colors.csv", package = "ohepR")
    if (!nzchar(color_file) || !file.exists(color_file)) {
      candidates <- c(
        file.path(getwd(), "inst", "extdata", "branding", "colors.csv"),
        file.path(getwd(), "..", "inst", "extdata", "branding", "colors.csv")
      )
      existing <- candidates[file.exists(candidates)]
      if (length(existing) > 0L) {
        color_file <- normalizePath(existing[[1]], mustWork = FALSE)
      }
    }
    if (!nzchar(color_file) || !file.exists(color_file)) {
      return(defaults)
    }

    color_df <- utils::read.csv(color_file, stringsAsFactors = FALSE)
    if (!"variable" %in% names(color_df)) {
      stop("Brand color CSV must include a `variable` column.", call. = FALSE)
    }

    base_col <- if ("color" %in% names(color_df)) {
      "color"
    } else if ("default_color" %in% names(color_df)) {
      "default_color"
    } else {
      NULL
    }
    graph_col <- paste0(tolower(graph), "_color")
    graph_has_col <- graph_col %in% names(color_df)

    to_chr <- function(x) {
      y <- trimws(as.character(x))
      y[is.na(y)] <- ""
      y
    }

    color_df <- color_df[!is.na(color_df$variable) & trimws(color_df$variable) != "", , drop = FALSE]
    if (nrow(color_df) < 1L) {
      return(defaults)
    }

    for (i in seq_len(nrow(color_df))) {
      var_name <- trimws(as.character(color_df$variable[[i]]))
      if (!nzchar(var_name)) {
        next
      }

      base_val <- if (!is.null(base_col)) to_chr(color_df[[base_col]][[i]]) else ""
      graph_val <- if (graph_has_col) to_chr(color_df[[graph_col]][[i]]) else ""
      chosen <- if (nzchar(graph_val)) graph_val else if (nzchar(base_val)) base_val else ""
      if (!nzchar(chosen)) {
        next
      }

      defaults[[var_name]] <- chosen
    }

    defaults
  }

  env$resolve_brand_colors <- function(color_overrides = NULL, extra_overrides = list(), graph = "fundamental") {
    defaults <- env$read_brand_colors(graph = graph)
    merged <- defaults

    from_list <- if (is.null(color_overrides)) list() else color_overrides
    if (!is.list(from_list)) {
      stop("`color_overrides` must be a named list.", call. = FALSE)
    }
    if (length(from_list) > 0L && (is.null(names(from_list)) || any(names(from_list) == ""))) {
      stop("`color_overrides` entries must be named.", call. = FALSE)
    }

    if (length(extra_overrides) > 0L) {
      if (is.null(names(extra_overrides)) || any(names(extra_overrides) == "")) {
        stop("Inline color overrides must be named.", call. = FALSE)
      }
      from_list <- c(from_list, extra_overrides)
    }

    unknown <- setdiff(names(from_list), names(defaults))
    if (length(unknown) > 0L) {
      stop(
        sprintf(
          "Unknown color override(s): %s. See `inst/extdata/branding/colors.csv`.",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    for (nm in names(from_list)) {
      value <- from_list[[nm]]
      if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
        stop(sprintf("Color override `%s` must be a single non-empty string.", nm), call. = FALSE)
      }
      merged[[nm]] <- as.character(value)
    }

    merged
  }

  env$validate_fundamental_page_data <- function(dashboard_data) {
    if (!is.list(dashboard_data)) {
      stop("`dashboard_data` must be a named list.", call. = FALSE)
    }

    required_sections <- c("fundamental", "outcomes", "items")
    missing_sections <- setdiff(required_sections, names(dashboard_data))
    if (length(missing_sections) > 0) {
      stop(
        sprintf("Missing section(s): %s.", paste(missing_sections, collapse = ", ")),
        call. = FALSE
      )
    }

    fundamental <- dashboard_data$fundamental
    outcomes <- dashboard_data$outcomes
    items <- dashboard_data$items

    if (!is.data.frame(fundamental) || nrow(fundamental) != 1L) {
      stop("`dashboard_data$fundamental` must be a data frame with exactly 1 row.", call. = FALSE)
    }
    if (!is.data.frame(outcomes) || nrow(outcomes) < 1L || nrow(outcomes) > 3L) {
      stop("`dashboard_data$outcomes` must have between 1 and 3 rows.", call. = FALSE)
    }
    if (!is.data.frame(items) || nrow(items) < 1L) {
      stop("`dashboard_data$items` must be a non-empty data frame.", call. = FALSE)
    }

    fundamental_cols <- c("percentile", "percentile_delta", "delta_label", "score", "score_delta")
    outcomes_cols <- c("rank", "outcome", "percentile")
    item_base_cols <- c(
      "column", "section", "section_order", "item_order", "label", "mean",
      "disagree_pct", "neutral_pct", "agree_pct"
    )

    if (length(setdiff(fundamental_cols, names(fundamental))) > 0) {
      stop("`fundamental` is missing required columns.", call. = FALSE)
    }
    if (length(setdiff(outcomes_cols, names(outcomes))) > 0) {
      stop("`outcomes` is missing required columns.", call. = FALSE)
    }
    if (length(setdiff(item_base_cols, names(items))) > 0) {
      stop("`items` is missing required columns.", call. = FALSE)
    }

    if (is.na(fundamental$percentile) || fundamental$percentile < 0 || fundamental$percentile > 100) {
      stop("`fundamental$percentile` must be between 0 and 100.", call. = FALSE)
    }
    if (anyNA(outcomes$percentile) || any(outcomes$percentile < 0 | outcomes$percentile > 100)) {
      stop("`outcomes$percentile` must be between 0 and 100.", call. = FALSE)
    }
    if (anyNA(items$column) || !all(items$column %in% c("left", "right"))) {
      stop("`items$column` must contain only 'left' or 'right'.", call. = FALSE)
    }

    left_n <- sum(items$column == "left")
    right_n <- sum(items$column == "right")
    if (left_n > 4L || right_n > 4L) {
      stop("Each items column can contain at most 4 rows in v1.", call. = FALSE)
    }

    pct_mat <- items[, c("disagree_pct", "neutral_pct", "agree_pct")]
    if (anyNA(pct_mat)) {
      stop("`items` sentiment percentages cannot be NA.", call. = FALSE)
    }
    if (any(pct_mat < 0 | pct_mat > 100)) {
      stop("`items` sentiment percentages must be between 0 and 100.", call. = FALSE)
    }

    for (i in seq_len(nrow(items))) {
      env$normalize_sentiment(items$disagree_pct[i], items$neutral_pct[i], items$agree_pct[i])
    }

    dashboard_data
  }

  env$validate_fundamental_inputs <- function(index_data, user_data) {
    if (!is.list(index_data)) {
      stop("`index_data` must be a named list.", call. = FALSE)
    }
    if (!is.list(user_data)) {
      stop("`user_data` must be a named list.", call. = FALSE)
    }

    has_required_cols <- function(df, cols) {
      is.data.frame(df) && length(setdiff(cols, names(df))) == 0
    }

    fm_cols <- c("fundamental", "percentile", "percentile_delta", "delta_label", "score", "score_delta")
    out_cols <- c("fundamental", "rank", "outcome", "percentile")
    key_cols <- c(
      "fundamental", "item", "section", "column", "section_order", "item_order",
      "label", "vs_industry", "vs_prior"
    )
    score_cols <- c("item", "mean")
    sent_cols <- c("item", "disagree_pct", "neutral_pct", "agree_pct")

    if (
      has_required_cols(index_data$fundamental_metrics, fm_cols) &&
      has_required_cols(index_data$outcomes, out_cols) &&
      has_required_cols(index_data$item_key, key_cols) &&
      has_required_cols(user_data$item_scores, score_cols) &&
      has_required_cols(user_data$item_sentiment, sent_cols)
    ) {
      return(invisible("canonical"))
    }

    required_index_new <- c("item_data", "user_data_key")
    required_user_new <- c("user_data")
    missing_index_new <- setdiff(required_index_new, names(index_data))
    missing_user_new <- setdiff(required_user_new, names(user_data))

    if (length(missing_index_new) > 0 || length(missing_user_new) > 0) {
      missing_parts <- c(
        if (length(missing_index_new) > 0) {
          paste0("index_data: ", paste(missing_index_new, collapse = ", "))
        },
        if (length(missing_user_new) > 0) {
          paste0("user_data: ", paste(missing_user_new, collapse = ", "))
        }
      )
      stop(
        sprintf(
          "Input lists do not match supported schemas. Missing %s.",
          paste(missing_parts, collapse = "; ")
        ),
        call. = FALSE
      )
    }

    if (!has_required_cols(index_data$item_data, c("item_id", "item_text", "fundamental_id", "facet_id"))) {
      stop("`index_data$item_data` is missing required columns.", call. = FALSE)
    }
    year_cols <- grep("^[0-9]{4}_mean$", names(index_data$item_data), value = TRUE)
    if (length(year_cols) < 1L) {
      stop("`index_data$item_data` must include at least one `<year>_mean` column.", call. = FALSE)
    }
    if (!has_required_cols(index_data$user_data_key, c("item", "Description", "data_domain"))) {
      stop("`index_data$user_data_key` is missing required columns.", call. = FALSE)
    }
    if (!is.data.frame(user_data$user_data)) {
      stop("`user_data$user_data` must be a data frame.", call. = FALSE)
    }
    if (length(setdiff(c("year", "company"), names(user_data$user_data))) > 0) {
      stop("`user_data$user_data` must include `year` and `company` columns.", call. = FALSE)
    }

    invisible("raw")
  }

  env$build_dashboard_data_from_raw <- function(fundamental, index_data, user_data) {
    clamp_pct <- function(x) {
      as.numeric(pmax(pmin(round(x), 100), 0))
    }
    mean_or_na <- function(df, col) {
      if (!col %in% names(df)) {
        return(NA_real_)
      }
      vals <- suppressWarnings(as.numeric(df[[col]]))
      if (all(is.na(vals))) {
        return(NA_real_)
      }
      mean(vals, na.rm = TRUE)
    }
    sentiment_from_values <- function(values, scale_min = 1, scale_max = 5) {
      vals <- suppressWarnings(as.numeric(values))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        return(c(0, 100, 0))
      }

      first_cut <- scale_min + (scale_max - scale_min) / 3
      second_cut <- scale_min + 2 * (scale_max - scale_min) / 3
      disagree <- 100 * sum(vals <= first_cut) / length(vals)
      neutral <- 100 * sum(vals > first_cut & vals < second_cut) / length(vals)
      agree <- 100 * sum(vals >= second_cut) / length(vals)

      out <- round(c(disagree, neutral, agree))
      diff <- 100 - sum(out)
      out[which.max(out)] <- out[which.max(out)] + diff
      out
    }

    item_data <- index_data$item_data
    user_key <- index_data$user_data_key
    user_df <- user_data$user_data
    pred <- index_data$predictive_data

    year_num <- suppressWarnings(as.numeric(user_df$year))
    latest_year <- suppressWarnings(max(year_num, na.rm = TRUE))
    if (!is.finite(latest_year)) {
      stop("`user_data$user_data$year` does not contain a valid year.", call. = FALSE)
    }

    latest_rows <- user_df[year_num == latest_year, , drop = FALSE]
    if (nrow(latest_rows) < 1L) {
      stop("No rows found in `user_data$user_data` for latest year.", call. = FALSE)
    }

    company_vals <- as.character(latest_rows$company)
    company_vals <- company_vals[!is.na(company_vals) & company_vals != ""]
    if (length(company_vals) > 0L) {
      company_tab <- sort(table(company_vals), decreasing = TRUE)
      focal_company <- names(company_tab)[1]
      current_df <- latest_rows[as.character(latest_rows$company) == focal_company, , drop = FALSE]
      prior_mask <- as.character(user_df$company) == focal_company & year_num < latest_year
      prior_years <- year_num[prior_mask]
      if (length(prior_years) > 0L && any(!is.na(prior_years))) {
        prior_year <- max(prior_years, na.rm = TRUE)
        prior_df <- user_df[as.character(user_df$company) == focal_company & year_num == prior_year, , drop = FALSE]
      } else {
        prior_year <- NA_real_
        prior_df <- user_df[0, , drop = FALSE]
      }
    } else {
      focal_company <- NA_character_
      current_df <- latest_rows
      prior_year <- suppressWarnings(max(year_num[year_num < latest_year], na.rm = TRUE))
      if (is.finite(prior_year)) {
        prior_df <- user_df[year_num == prior_year, , drop = FALSE]
      } else {
        prior_year <- NA_real_
        prior_df <- user_df[0, , drop = FALSE]
      }
    }

    fund_rows <- item_data[as.character(item_data$fundamental_id) == fundamental, , drop = FALSE]
    if (nrow(fund_rows) < 1L) {
      stop("Expected exactly one row in `fundamental_metrics` for the selected fundamental.", call. = FALSE)
    }

    year_cols <- grep("^[0-9]{4}_mean$", names(item_data), value = TRUE)
    year_vals <- suppressWarnings(as.numeric(sub("_mean$", "", year_cols)))
    year_cols <- year_cols[order(year_vals)]
    year_vals <- year_vals[order(year_vals)]

    fund_year_means <- vapply(year_cols, function(col) {
      vals <- suppressWarnings(as.numeric(fund_rows[[col]]))
      if (all(is.na(vals))) {
        return(NA_real_)
      }
      mean(vals, na.rm = TRUE)
    }, numeric(1))
    usable_year_idx <- which(!is.na(fund_year_means))
    latest_index_idx <- if (length(usable_year_idx) > 0L) utils::tail(usable_year_idx, 1) else length(year_cols)
    prior_index_idx <- if (latest_index_idx > 1L) latest_index_idx - 1L else latest_index_idx
    latest_col <- year_cols[latest_index_idx]
    prior_col <- year_cols[prior_index_idx]
    latest_col_year <- year_vals[latest_index_idx]
    prior_col_year <- year_vals[prior_index_idx]

    fund_rows$item_id <- as.character(fund_rows$item_id)
    suffix_num <- suppressWarnings(as.integer(sub(".*_(\\d+)$", "\\1", fund_rows$item_id)))
    ord <- order(ifelse(is.na(suffix_num), Inf, suffix_num), fund_rows$item_id)
    fund_rows <- fund_rows[ord, , drop = FALSE]
    fund_rows <- fund_rows[!duplicated(fund_rows$item_id), , drop = FALSE]
    fund_rows <- utils::head(fund_rows, 8L)

    item_ids <- as.character(fund_rows$item_id)
    item_labels <- as.character(user_key$Description[match(item_ids, as.character(user_key$item))])
    fallback_labels <- as.character(fund_rows$item_text)
    item_labels[is.na(item_labels) | item_labels == ""] <- fallback_labels[is.na(item_labels) | item_labels == ""]

    min_scale <- suppressWarnings(min(as.numeric(fund_rows$response_scale_min), na.rm = TRUE))
    max_scale <- suppressWarnings(max(as.numeric(fund_rows$response_scale_max), na.rm = TRUE))
    if (!is.finite(min_scale) || !is.finite(max_scale) || min_scale == max_scale) {
      min_scale <- 1
      max_scale <- 5
    }

    user_item_mean <- vapply(item_ids, function(item) mean_or_na(current_df, item), numeric(1))
    prior_item_mean <- vapply(item_ids, function(item) mean_or_na(prior_df, item), numeric(1))
    index_item_mean <- suppressWarnings(as.numeric(fund_rows[[latest_col]]))

    fund_score <- if (all(is.na(user_item_mean))) mean(index_item_mean, na.rm = TRUE) else mean(user_item_mean, na.rm = TRUE)
    if (!is.finite(fund_score)) {
      stop("Could not compute fundamental score from raw inputs.", call. = FALSE)
    }
    prior_score <- if (all(is.na(prior_item_mean))) NA_real_ else mean(prior_item_mean, na.rm = TRUE)
    score_delta <- if (is.na(prior_score)) 0 else fund_score - prior_score

    percentile <- clamp_pct(100 * (fund_score - min_scale) / (max_scale - min_scale))
    percentile_delta <- round(score_delta * 100)
    delta_label <- if (is.finite(prior_year)) {
      paste0("vs. ", as.integer(prior_year))
    } else if (is.finite(prior_col_year)) {
      paste0("vs. ", as.integer(prior_col_year))
    } else {
      "vs. prior"
    }

    key_rows <- nrow(fund_rows)
    left_n <- ceiling(key_rows / 2)
    columns <- c(rep("left", left_n), rep("right", key_rows - left_n))
    item_order <- stats::ave(seq_len(key_rows), columns, FUN = seq_along)
    section_vals <- as.character(fund_rows$facet_id)
    section_vals[is.na(section_vals) | section_vals == ""] <- fundamental

    item_key <- data.frame(
      fundamental = rep(fundamental, key_rows),
      item = item_ids,
      section = section_vals,
      column = columns,
      section_order = 1,
      item_order = as.numeric(item_order),
      label = item_labels,
      vs_industry = ifelse(is.na(user_item_mean) | is.na(index_item_mean), NA_real_, round((user_item_mean - index_item_mean) * 100)),
      vs_prior = ifelse(is.na(user_item_mean) | is.na(prior_item_mean), NA_real_, round((user_item_mean - prior_item_mean) * 100)),
      suppress_vs_industry = as.logical(is.na(user_item_mean) | is.na(index_item_mean)),
      suppress_vs_prior = as.logical(is.na(user_item_mean) | is.na(prior_item_mean)),
      suppress_row = FALSE,
      min_n = 3L,
      stringsAsFactors = FALSE
    )

    item_sent <- t(vapply(seq_along(item_ids), function(i) {
      sent <- sentiment_from_values(
        current_df[[item_ids[i]]],
        scale_min = suppressWarnings(as.numeric(fund_rows$response_scale_min[i])),
        scale_max = suppressWarnings(as.numeric(fund_rows$response_scale_max[i]))
      )
      env$normalize_sentiment(sent[1], sent[2], sent[3])
    }, numeric(3)))
    colnames(item_sent) <- c("disagree_pct", "neutral_pct", "agree_pct")

    build_outcomes_fallback <- function() {
      calc_outcome_pct <- function(cols, reverse = FALSE) {
        cols <- cols[cols %in% names(current_df)]
        if (length(cols) == 0L) {
          return(50)
        }
        vals <- suppressWarnings(as.numeric(unlist(current_df[, cols, drop = FALSE])))
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0L) {
          return(50)
        }
        pct <- clamp_pct(100 * (mean(vals) - 1) / 4)
        if (reverse) {
          pct <- 100 - pct
        }
        pct
      }

      out <- data.frame(
        rank = c(1, 2, 3),
        outcome = c("Turnover Intentions", "Work Satisfaction", "Engagement"),
        percentile = c(
          calc_outcome_pct(c("turnover_1", "turnover_2", "turnover_3"), reverse = TRUE),
          calc_outcome_pct(c("workSat_1", "workSat_2"), reverse = FALSE),
          calc_outcome_pct(c("engagement_1", "engagement_2", "engagement_3", "engagement_4", "engagement_5"), reverse = FALSE)
        ),
        stringsAsFactors = FALSE
      )
      out
    }

    out_rows <- NULL
    if (is.data.frame(pred) && all(c("Fundamental", "Outcome", "strength") %in% names(pred))) {
      sub_pred <- pred[as.character(pred$Fundamental) == fundamental, , drop = FALSE]
      if ("subset" %in% names(sub_pred)) {
        sub_pred <- sub_pred[as.character(sub_pred$subset) == "All", , drop = FALSE]
      }
      if ("significant" %in% names(sub_pred) && any(as.logical(sub_pred$significant), na.rm = TRUE)) {
        sub_pred <- sub_pred[as.logical(sub_pred$significant), , drop = FALSE]
      }
      sub_pred <- sub_pred[!as.character(sub_pred$Outcome) %in% c("Outcomes", "eNPS"), , drop = FALSE]
      if (nrow(sub_pred) > 0L) {
        strength_vals <- suppressWarnings(as.numeric(sub_pred$strength))
        sub_pred <- sub_pred[order(abs(strength_vals), decreasing = TRUE), , drop = FALSE]
        sub_pred <- utils::head(sub_pred, 3L)
        out_rows <- data.frame(
          rank = seq_len(nrow(sub_pred)),
          outcome = as.character(sub_pred$Outcome),
          percentile = clamp_pct(abs(suppressWarnings(as.numeric(sub_pred$strength))) * 100),
          stringsAsFactors = FALSE
        )
      }
    }
    if (!is.data.frame(out_rows) || nrow(out_rows) < 1L) {
      out_rows <- build_outcomes_fallback()
    }

    canonical <- list(
      fundamental = data.frame(
        percentile = as.numeric(percentile),
        percentile_delta = as.numeric(percentile_delta),
        delta_label = as.character(delta_label),
        score = as.numeric(round(fund_score, 2)),
        score_delta = as.numeric(round(score_delta, 2)),
        stringsAsFactors = FALSE
      ),
      outcomes = data.frame(
        rank = as.numeric(out_rows$rank),
        outcome = as.character(out_rows$outcome),
        percentile = as.numeric(out_rows$percentile),
        stringsAsFactors = FALSE
      ),
      items = data.frame(
        column = as.character(item_key$column),
        section = as.character(item_key$section),
        section_order = as.numeric(item_key$section_order),
        item_order = as.numeric(item_key$item_order),
        label = as.character(item_key$label),
        mean = as.numeric(round(user_item_mean, 2)),
        disagree_pct = as.numeric(item_sent[, "disagree_pct"]),
        neutral_pct = as.numeric(item_sent[, "neutral_pct"]),
        agree_pct = as.numeric(item_sent[, "agree_pct"]),
        vs_industry = as.numeric(item_key$vs_industry),
        vs_prior = as.numeric(item_key$vs_prior),
        suppress_row = as.logical(item_key$suppress_row),
        suppress_vs_industry = as.logical(item_key$suppress_vs_industry),
        suppress_vs_prior = as.logical(item_key$suppress_vs_prior),
        min_n = as.integer(item_key$min_n),
        stringsAsFactors = FALSE
      )
    )

    canonical$items$mean[is.na(canonical$items$mean)] <- round(index_item_mean[is.na(canonical$items$mean)], 2)
    canonical$items$mean[is.na(canonical$items$mean)] <- round(fund_score, 2)

    canonical
  }

  env$build_dashboard_data_from_inputs <- function(fundamental, index_data, user_data) {
    input_schema <- env$validate_fundamental_inputs(index_data, user_data)

    if (identical(input_schema, "raw")) {
      canonical <- env$build_dashboard_data_from_raw(
        fundamental = fundamental,
        index_data = index_data,
        user_data = user_data
      )
      return(env$validate_fundamental_page_data(canonical))
    }

    fm <- index_data$fundamental_metrics
    outcomes <- index_data$outcomes
    item_key <- index_data$item_key
    item_scores <- user_data$item_scores
    item_sentiment <- user_data$item_sentiment

    fm_sub <- fm[fm$fundamental == fundamental, , drop = FALSE]
    out_sub <- outcomes[outcomes$fundamental == fundamental, , drop = FALSE]
    key_sub <- item_key[item_key$fundamental == fundamental, , drop = FALSE]

    if (nrow(fm_sub) != 1L) {
      stop("Expected exactly one row in `fundamental_metrics` for the selected fundamental.", call. = FALSE)
    }
    if (nrow(out_sub) < 1L) {
      stop("No outcomes found for selected fundamental.", call. = FALSE)
    }
    if (nrow(key_sub) < 1L) {
      stop("No item key rows found for selected fundamental.", call. = FALSE)
    }

    score_ix <- match(key_sub$item, item_scores$item)
    sent_ix <- match(key_sub$item, item_sentiment$item)
    if (anyNA(score_ix)) {
      missing_items <- unique(key_sub$item[is.na(score_ix)])
      stop(sprintf("Missing user item score(s): %s.", paste(missing_items, collapse = ", ")), call. = FALSE)
    }
    if (anyNA(sent_ix)) {
      missing_items <- unique(key_sub$item[is.na(sent_ix)])
      stop(sprintf("Missing user item sentiment row(s): %s.", paste(missing_items, collapse = ", ")), call. = FALSE)
    }

    canonical <- list(
      fundamental = fm_sub[, c("percentile", "percentile_delta", "delta_label", "score", "score_delta"), drop = FALSE],
      outcomes = out_sub[, c("rank", "outcome", "percentile"), drop = FALSE],
      items = data.frame(
        column = as.character(key_sub$column),
        section = as.character(key_sub$section),
        section_order = as.numeric(key_sub$section_order),
        item_order = as.numeric(key_sub$item_order),
        label = as.character(key_sub$label),
        mean = as.numeric(item_scores$mean[score_ix]),
        disagree_pct = as.numeric(item_sentiment$disagree_pct[sent_ix]),
        neutral_pct = as.numeric(item_sentiment$neutral_pct[sent_ix]),
        agree_pct = as.numeric(item_sentiment$agree_pct[sent_ix]),
        vs_industry = as.numeric(key_sub$vs_industry),
        vs_prior = as.numeric(key_sub$vs_prior),
        suppress_row = FALSE,
        suppress_vs_industry = as.logical(is.na(key_sub$vs_industry)),
        suppress_vs_prior = as.logical(is.na(key_sub$vs_prior)),
        min_n = 3L,
        stringsAsFactors = FALSE
      )
    )

    env$validate_fundamental_page_data(canonical)
  }

  env$fundamental_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "fundamental")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-root {{
  --bg-canvas: {c$bg_canvas};
  --bg-card: {c$bg_card};
  --bg-secondary: {c$bg_secondary};
  --border-light: {c$border_light};
  --border-default: {c$border_default};
  --border-band: {c$border_band};
  --border-dark: {c$border_dark};
  --text-primary: {c$text_primary};
  --text-secondary: {c$text_secondary};
  --text-tertiary: {c$text_tertiary};
  --text-muted: {c$text_muted};
  --text-faint: {c$text_faint};
  --text-decorative: {c$text_decorative};
  --text-accent: {c$text_accent};
  --text-on-dark: {c$text_on_dark};
  --percentile-z1-core: {c$percentile_z1_core};
  --percentile-z1-text: {c$percentile_z1_text};
  --percentile-z1-chip: {c$percentile_z1_chip};
  --percentile-z2-core: {c$percentile_z2_core};
  --percentile-z2-text: {c$percentile_z2_text};
  --percentile-z2-chip: {c$percentile_z2_chip};
  --percentile-z3-core: {c$percentile_z3_core};
  --percentile-z3-text: {c$percentile_z3_text};
  --percentile-z3-chip: {c$percentile_z3_chip};
  --percentile-z4-core: {c$percentile_z4_core};
  --percentile-z4-text: {c$percentile_z4_text};
  --percentile-z4-chip: {c$percentile_z4_chip};
  --delta-pos-text: {c$delta_pos_text};
  --delta-pos-bg: {c$delta_pos_bg};
  --delta-neg-text: {c$delta_neg_text};
  --delta-neg-bg: {c$delta_neg_bg};
  --delta-neu-text: {c$delta_neu_text};
  --delta-neu-bg: {c$delta_neu_bg};
  --favorability-agree: {c$favorability_agree};
  --favorability-neutral: {c$favorability_neutral};
  --favorability-disagree: {c$favorability_disagree};
  --band-tick: {c$band_tick};
  --band-tick-label: {c$band_tick_label};
  --band-midpoint: {c$band_midpoint};
  --band-marker: {c$band_marker};
  --privacy-item-label: {c$privacy_item_label};
  --privacy-overlay-bg: {c$privacy_overlay_bg};
  --privacy-overlay-border: {c$privacy_overlay_border};
  --privacy-overlay-text: {c$privacy_overlay_text};
  --privacy-overlay-shadow: {c$privacy_overlay_shadow};
  width: 1280px;
  min-height: 720px;
  background: var(--bg-canvas);
  padding: 24px 32px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  flex-direction: column;
  gap: 20px;
  overflow: visible;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .top-row {{ display: flex; gap: 20px; height: 310px; flex-shrink: 0; }}
{scope} .bottom-row {{ flex: 1 1 auto; display: flex; min-height: 0; }}
{scope} .card {{
  background: var(--bg-card);
  border: 1px solid var(--border-default);
  border-radius: 12px;
  padding: 24px;
  box-shadow: 0 4px 12px rgba(15,23,42,.03);
  display: flex;
  flex-direction: column;
}}
{scope} .fundamental-card {{ flex: 1.2; }}
{scope} .outcomes-card {{ flex: 1; }}
{scope} .item-card {{ width: 100%; padding: 20px 24px; }}
{scope} .title-bar {{ display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 16px; }}
{scope} .card-title {{ font-size: 22px; font-weight: 900; color: var(--text-primary); margin: 0; text-transform: uppercase; letter-spacing: -0.5px; }}
{scope} .status-pill {{ font-size: 11px; font-weight: 800; padding: 6px 12px; border-radius: 12px; text-transform: uppercase; letter-spacing: 0.5px; }}
{scope} .status-risk {{ background: var(--percentile-z1-chip); color: var(--percentile-z1-text); }}
{scope} .status-watch {{ background: var(--percentile-z2-chip); color: var(--percentile-z2-text); }}
{scope} .status-good {{ background: var(--percentile-z3-chip); color: var(--percentile-z3-text); }}
{scope} .status-leader {{ background: var(--percentile-z4-chip); color: var(--percentile-z4-text); }}
{scope} .color-risk {{ color: var(--percentile-z1-text); }}
{scope} .color-watch {{ color: var(--percentile-z2-text); }}
{scope} .color-good {{ color: var(--percentile-z3-text); }}
{scope} .metrics-container {{ display: flex; flex-direction: column; margin-bottom: 16px; }}
{scope} .hero-row {{ display: flex; align-items: flex-start; gap: 20px; }}
{scope} .hero-label {{ font-size: 11px; font-weight: 800; color: var(--text-accent); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 4px; }}
{scope} .hero-value {{ font-size: 48px; font-weight: 900; color: var(--text-primary); line-height: 1; letter-spacing: -1px; }}
{scope} .delta-block {{ display: flex; align-items: center; gap: 6px; margin-top: 16px; }}
{scope} .delta-block.positive {{ color: var(--delta-pos-text); }}
{scope} .delta-block.negative {{ color: var(--delta-neg-text); }}
{scope} .delta-block.neutral {{ color: var(--text-muted); }}
{scope} .delta-icon {{ font-size: 20px; line-height: 1; }}
{scope} .delta-details {{ display: flex; flex-direction: column; justify-content: center; }}
{scope} .delta-value {{ font-size: 14px; font-weight: 800; line-height: 1.1; }}
{scope} .delta-year {{ font-size: 10px; color: var(--text-faint); font-weight: 700; text-transform: uppercase; margin-top: 2px; }}
{scope} .sub-context {{ font-size: 13px; color: var(--text-muted); font-weight: 600; margin-top: 12px; }}
{scope} .sub-context strong {{ color: var(--text-primary); font-weight: 800; }}
{scope} .sub-delta {{ color: var(--delta-pos-text); font-weight: 800; }}
{scope} .band-wrap {{ position: relative; margin-top: auto; padding-top: 16px; }}
{scope} .band {{ position: relative; height: 26px; border-radius: 999px; overflow: hidden; display: flex; border: 1px solid var(--border-band); }}
{scope} .z1 {{ width: 40%; background: var(--percentile-z1-core); }}
{scope} .z2 {{ width: 20%; background: var(--percentile-z2-core); }}
{scope} .z3 {{ width: 25%; background: var(--percentile-z3-core); }}
{scope} .z4 {{ width: 15%; background: var(--percentile-z4-core); }}
{scope} .tick {{ position: absolute; top: 0; width: 2px; height: 26px; background: var(--band-tick); z-index: 2; transform: translateX(-50%); }}
{scope} .tick-label {{ position: absolute; top: 12px; transform: translate(-50%, -100%); font-size: 11px; font-weight: 800; color: var(--band-tick-label); }}
{scope} .midpoint {{ position: absolute; top: 29px; left: 50%; width: 2px; height: 32px; background: var(--band-midpoint); transform: translate(-50%, -50%); z-index: 3; }}
{scope} .marker {{ position: absolute; top: 29px; width: 4px; height: 38px; background: var(--band-marker); transform: translate(-50%, -50%); border-radius: 4px; z-index: 5; }}
{scope} .labels {{ display: flex; align-items: center; font-size: 11px; font-weight: 600; color: var(--text-muted); margin-top: 10px; }}
{scope} .labels span {{ display: flex; justify-content: center; text-align: center; }}
{scope} .labels span:nth-child(1) {{ width: 40%; }}
{scope} .labels span:nth-child(2) {{ width: 20%; font-weight: 800; color: var(--percentile-z2-text); }}
{scope} .labels span:nth-child(3) {{ width: 25%; }}
{scope} .labels span:nth-child(4) {{ width: 15%; }}
{scope} .table-outcomes {{ width: 100%; border-collapse: collapse; margin-top: auto; }}
{scope} .table-outcomes th {{
  text-align: left; padding-bottom: 10px; font-size: 10px; font-weight: 800; color: var(--text-faint);
  text-transform: uppercase; letter-spacing: 0.5px; border-bottom: 2px solid var(--border-default);
}}
{scope} .table-outcomes th.right-align {{ text-align: right; }}
{scope} .table-outcomes td {{ padding: 14px 0; border-bottom: 1px solid var(--border-light); vertical-align: middle; }}
{scope} .table-outcomes tr:last-child td {{ border-bottom: none; padding-bottom: 0; }}
{scope} .row-rank {{ font-size: 22px; font-weight: 800; color: var(--text-decorative); margin-right: 12px; }}
{scope} .row-name {{ font-size: 15px; font-weight: 800; color: var(--text-primary); }}
{scope} .company-score-stack {{ display: flex; flex-direction: column; align-items: flex-end; }}
{scope} .row-score-val {{ font-size: 15px; font-weight: 800; line-height: 1; }}
{scope} .row-status {{ font-size: 10px; font-weight: 800; text-transform: uppercase; letter-spacing: 0.3px; margin-top: 4px; }}
{scope} .item-title-row {{ display: flex; justify-content: space-between; align-items: center; margin-bottom: 22px; }}
{scope} .legend-group {{ display: flex; gap: 16px; font-size: 11px; font-weight: 700; color: var(--text-muted); text-transform: uppercase; }}
{scope} .leg-item {{ display: flex; align-items: center; gap: 6px; }}
{scope} .leg-dot {{ width: 10px; height: 10px; border-radius: 2px; }}
{scope} .bg-agree {{ background: var(--favorability-agree); }}
{scope} .bg-neutral {{ background: var(--favorability-neutral); }}
{scope} .bg-disagree {{ background: var(--favorability-disagree); }}
{scope} .item-grid-2col {{ display: grid; grid-template-columns: 1fr 1fr; gap: 40px; flex: 1; align-items: start; }}
{scope} .item-column {{ display: flex; flex-direction: column; }}
{scope} .header-row, {scope} .item-row {{ display: grid; grid-template-columns: minmax(180px, 2fr) 40px 3fr 50px 50px; gap: 12px; align-items: center; }}
{scope} .header-row {{ padding-bottom: 8px; border-bottom: 2px solid var(--border-default); }}
{scope} .h-lbl {{ font-size: 9px; font-weight: 800; color: var(--text-faint); text-transform: uppercase; letter-spacing: 0.5px; text-align: center; }}
{scope} .h-lbl:first-child {{ text-align: left; }}
{scope} .sub-cat {{ grid-column: 1 / -1; background: var(--bg-secondary); font-size: 11px; font-weight: 800; color: var(--text-secondary); text-transform: uppercase; letter-spacing: 1px; padding: 6px 12px; border-radius: 4px; margin: 8px 0; }}
{scope} .item-row {{ padding: 6px 0; border-bottom: 1px solid var(--border-light); }}
{scope} .item-row:last-child {{ border-bottom: none; }}
{scope} .item-row > .item-label {{ grid-column: 1; }}
{scope} .item-row > .cell-score {{ grid-column: 2; }}
{scope} .item-row > .sentiment-stack {{ grid-column: 3; }}
{scope} .item-row > .sentiment-privacy-stack {{ grid-column: 3; display: flex; flex-direction: column; align-items: center; gap: 8px; }}
{scope} .item-row > .sentiment-privacy-stack > .sentiment-stack {{ width: 100%; }}
{scope} .item-row > .delta-industry {{ grid-column: 4; }}
{scope} .item-row > .delta-prior {{ grid-column: 5; }}
{scope} .item-label {{ font-size: 12px; font-weight: 600; color: var(--text-secondary); line-height: 1.3; padding-right: 8px; }}
{scope} .cell-score {{ font-size: 13px; font-weight: 800; color: var(--text-primary); text-align: center; }}
{scope} .sentiment-stack {{ height: 22px; display: flex; border-radius: 4px; overflow: hidden; background: var(--bg-secondary); }}
{scope} .segment {{ display: flex; align-items: center; justify-content: center; color: var(--text-on-dark); font-size: 10px; font-weight: 800; }}
{scope} .delta-container {{ display: flex; justify-content: center; }}
{scope} .delta-pill {{ display: inline-flex; align-items: center; justify-content: center; width: 48px; padding: 4px 0; border-radius: 4px; font-size: 10px; font-weight: 800; }}
{scope} .dp-pos {{ background: var(--delta-pos-bg); color: var(--delta-pos-text); }}
{scope} .dp-neg {{ background: var(--delta-neg-bg); color: var(--delta-neg-text); }}
{scope} .dp-neu {{ background: var(--delta-neu-bg); color: var(--delta-neu-text); }}
{scope} .privacy-row {{ position: relative; }}
{scope} .privacy-row .item-label {{ color: var(--privacy-item-label); }}
{scope} .privacy-row > .cell-score,
{scope} .privacy-row > .delta-container,
{scope} .privacy-row > .sentiment-stack,
{scope} .privacy-row > .sentiment-privacy-stack > .sentiment-stack {{ filter: blur(4px) grayscale(60%); opacity: 0.25; pointer-events: none; user-select: none; }}
{scope} .privacy-overlay {{
  position: relative;
  z-index: 3;
  background: var(--privacy-overlay-bg);
  backdrop-filter: blur(2px);
  border: 1px solid var(--privacy-overlay-border);
  border-radius: 6px;
  padding: 6px 14px;
  font-size: 11px;
  font-weight: 700;
  color: var(--privacy-overlay-text);
  letter-spacing: 0.2px;
  box-shadow: 0 2px 6px var(--privacy-overlay-shadow);
  display: flex;
  align-items: center;
  gap: 6px;
}}
"
    )
  }

  env$build_hero_card <- function(fundamental_row) {
    percentile <- as.numeric(fundamental_row$percentile[[1]])
    percentile_delta <- as.numeric(fundamental_row$percentile_delta[[1]])
    score <- as.numeric(fundamental_row$score[[1]])
    score_delta <- as.numeric(fundamental_row$score_delta[[1]])
    delta_label <- env$escape_text(fundamental_row$delta_label[[1]])

    delta_class <- if (percentile_delta > 0) {
      "positive"
    } else if (percentile_delta < 0) {
      "negative"
    } else {
      "neutral"
    }
    delta_icon <- if (percentile_delta > 0) {
      "&#9650;"
    } else if (percentile_delta < 0) {
      "&#9660;"
    } else {
      "&#8226;"
    }

    glue::glue(
      "<div class=\"card fundamental-card\">
        <div class=\"title-bar\">
          <h2 class=\"card-title\">Purpose</h2>
          <span class=\"{env$get_status_pill_class(percentile)}\">{env$get_status_text(percentile)}</span>
        </div>
        <div class=\"metrics-container\">
          <div class=\"hero-row\">
            <div>
              <div class=\"hero-label\">Percentile</div>
              <div class=\"hero-value\">{env$format_ordinal_html(percentile)}</div>
            </div>
            <div class=\"delta-block {delta_class}\">
              <div class=\"delta-icon\">{delta_icon}</div>
              <div class=\"delta-details\">
                <div class=\"delta-value\">{env$format_signed(percentile_delta)} pts</div>
                <div class=\"delta-year\">{delta_label}</div>
              </div>
            </div>
          </div>
          <div class=\"sub-context\">
            Score: <strong>{sprintf('%.2f', score)}</strong>
            <span class=\"sub-delta\">({env$format_signed(score_delta, digits = 2)})</span>
          </div>
        </div>
        <div class=\"band-wrap\">
          <div class=\"tick-label\" style=\"left:40%;\">40</div>
          <div class=\"tick-label\" style=\"left:50%;\">50</div>
          <div class=\"tick-label\" style=\"left:60%;\">60</div>
          <div class=\"tick-label\" style=\"left:85%;\">85</div>
          <div class=\"band\">
            <div class=\"z1\"></div><div class=\"z2\"></div><div class=\"z3\"></div><div class=\"z4\"></div>
            <div class=\"tick\" style=\"left:40%;\"></div><div class=\"tick\" style=\"left:60%;\"></div><div class=\"tick\" style=\"left:85%;\"></div>
          </div>
          <div class=\"midpoint\"></div>
          <div class=\"marker\" style=\"left:{max(0, min(100, percentile))}%;\"></div>
        </div>
        <div class=\"labels\">
          <span>Area for Growth</span>
          <span>Industry standard</span>
          <span>Above standard</span>
          <span>Industry leader</span>
        </div>
      </div>"
    )
  }

  env$build_outcome_row <- function(outcome_row) {
    pct <- as.numeric(outcome_row$percentile[[1]])
    color_class <- env$get_outcome_class(pct)
    status_text <- env$get_status_text(pct)
    glue::glue(
      "<tr>
        <td>
          <span class=\"row-rank\">{as.integer(outcome_row$rank[[1]])}</span>
          <span class=\"row-name\">{env$escape_text(outcome_row$outcome[[1]])}</span>
        </td>
        <td>
          <div class=\"company-score-stack\">
            <span class=\"row-score-val {color_class}\">{env$format_ordinal_html(pct)} Percentile</span>
            <span class=\"row-status {color_class}\">{status_text}</span>
          </div>
        </td>
      </tr>"
    )
  }

  env$build_item_row <- function(item_row, active_benchmarks = NULL) {
    suppress_row <- ("suppress_row" %in% names(item_row)) && isTRUE(as.logical(item_row$suppress_row[[1]]))
    min_n <- if ("min_n" %in% names(item_row)) as.integer(item_row$min_n[[1]]) else 3L
    if (is.null(active_benchmarks)) {
      active_benchmarks <- list(
        list(name = "index", value_col = "vs_industry", suppress_col = "suppress_vs_industry", css_class = "delta-industry"),
        list(name = "prior", value_col = "vs_prior", suppress_col = "suppress_vs_prior", css_class = "delta-prior")
      )
    }

    sentiment <- env$normalize_sentiment(
      as.numeric(item_row$disagree_pct[[1]]),
      as.numeric(item_row$neutral_pct[[1]]),
      as.numeric(item_row$agree_pct[[1]])
    )
    disagree <- sentiment[[1]]
    neutral <- sentiment[[2]]
    agree <- sentiment[[3]]

    disagree_lbl <- if (disagree >= 8) paste0(round(disagree), "%") else ""
    neutral_lbl <- if (neutral >= 8) paste0(round(neutral), "%") else ""
    agree_lbl <- if (agree >= 8) paste0(round(agree), "%") else ""

    privacy_tip <- "Insufficient responses"
    n_bm <- length(active_benchmarks)
    row_classes <- c("item-row")
    if (suppress_row) {
      row_classes <- c(row_classes, "privacy-row")
    }
    row_class <- paste(row_classes, collapse = " ")
    row_grid_style <- switch(
      as.character(n_bm),
      "0" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr;\"",
      "1" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px;\"",
      "3" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px 50px 50px;\"",
      ""
    )
    stacked_sentiment <- paste0(
      "<div class=\"sentiment-stack\">
        <div class=\"segment bg-disagree\" style=\"width: ", disagree, "%;\">", disagree_lbl, "</div>
        <div class=\"segment bg-neutral\" style=\"width: ", neutral, "%;\">", neutral_lbl, "</div>
        <div class=\"segment bg-agree\" style=\"width: ", agree, "%;\">", agree_lbl, "</div>
      </div>"
    )
    row_overlay <- if (suppress_row) {
      paste0("<div class=\"sentiment-privacy-stack\">
        <div class=\"privacy-overlay\">
        <svg width=\"12\" height=\"12\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">
          <rect x=\"3\" y=\"11\" width=\"18\" height=\"11\" rx=\"2\" ry=\"2\"></rect>
          <path d=\"M7 11V7a5 5 0 0 1 10 0v4\"></path>
        </svg>
        Insufficient responses
        </div>
        ", stacked_sentiment, "
      </div>")
    } else {
      ""
    }
    score_val <- as.numeric(item_row$mean[[1]])
    score_text <- if (is.na(score_val)) "-" else sprintf("%.2f", score_val)
    benchmark_html <- vapply(seq_along(active_benchmarks), function(i) {
      bm <- active_benchmarks[[i]]
      delta_val <- if (bm$value_col %in% names(item_row)) suppressWarnings(as.numeric(item_row[[bm$value_col]][[1]])) else NA_real_
      suppress_delta <- if (bm$suppress_col %in% names(item_row)) {
        isTRUE(as.logical(item_row[[bm$suppress_col]][[1]]))
      } else {
        is.na(delta_val)
      }
      pill <- env$get_delta_pill(delta_val)
      text <- if (suppress_delta) "-" else paste0(env$format_signed(delta_val), "%")
      title <- if (suppress_delta) glue::glue(" title=\"{privacy_tip}\"") else ""
      pill <- if (suppress_delta) "dp-neu" else pill
      grid_style <- if (n_bm != 2L) glue::glue(" style=\"grid-column:{i + 3L};\"") else ""
      glue::glue("<div class=\"delta-container {bm$css_class}\"{grid_style}><span class=\"delta-pill {pill}\"{title}>{text}</span></div>")
    }, character(1))
    benchmark_html <- paste(benchmark_html, collapse = "")

    glue::glue(
      "<div class=\"{row_class}\"{row_grid_style}>
        <div class=\"item-label\">{env$escape_text(item_row$label[[1]])}</div>
        <div class=\"cell-score\">{score_text}</div>
        {if (suppress_row) row_overlay else stacked_sentiment}
        {benchmark_html}
      </div>"
    )
  }

  env$build_item_column <- function(items_df, column_name, active_benchmarks = list()) {
    col_df <- items_df[items_df$column == column_name, , drop = FALSE]
    col_df <- col_df[order(col_df$section_order, col_df$item_order), , drop = FALSE]
    header_cells <- c(
      "<div class=\"h-lbl\">Survey Item</div>",
      "<div class=\"h-lbl\">Mean</div>",
      "<div class=\"h-lbl\">Sentiment Distribution</div>"
    )
    if (length(active_benchmarks) > 0L) {
      bench_headers <- vapply(active_benchmarks, function(bm) {
        bench_class <- if (length(active_benchmarks) == 2L) "h-lbl" else "h-lbl h-lbl-benchmark"
        glue::glue("<div class=\"{bench_class}\">{env$escape_text(bm$label)}</div>")
      }, character(1))
      header_cells <- c(header_cells, bench_headers)
    }
    n_bm <- length(active_benchmarks)
    header_class <- "header-row"
    header_grid_style <- switch(
      as.character(n_bm),
      "0" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr;\"",
      "1" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px;\"",
      "3" = " style=\"grid-template-columns:minmax(180px, 2fr) 40px 3fr 50px 50px 50px;\"",
      ""
    )
    header <- glue::glue(
      "<div class=\"{header_class}\"{header_grid_style}>
      {glue::glue_collapse(header_cells, sep = '')}
    </div>"
    )

    out <- character(0)
    out <- c(out, header)
    sections <- unique(col_df$section)
    for (section in sections) {
      out <- c(out, glue::glue("<div class=\"sub-cat\">{env$escape_text(section)}</div>"))
      section_rows <- col_df[col_df$section == section, , drop = FALSE]
      row_html <- vapply(seq_len(nrow(section_rows)), function(i) {
        env$build_item_row(section_rows[i, , drop = FALSE], active_benchmarks = active_benchmarks)
      }, character(1))
      out <- c(out, row_html)
    }
    glue::glue("<div class=\"item-column\">{glue::glue_collapse(out, sep = '')}</div>")
  }

  env$build_outcomes_table <- function(outcomes_df) {
    outcomes_df <- outcomes_df[order(outcomes_df$rank), , drop = FALSE]
    rows <- vapply(seq_len(nrow(outcomes_df)), function(i) {
      env$build_outcome_row(outcomes_df[i, , drop = FALSE])
    }, character(1))

    glue::glue(
      "<div class=\"card outcomes-card\">
        <div class=\"title-bar\">
          <h2 class=\"card-title\">Purpose drives</h2>
        </div>
        <table class=\"table-outcomes\">
          <thead>
            <tr>
              <th>Outcome</th>
              <th class=\"right-align\">Your Company</th>
            </tr>
          </thead>
          <tbody>{glue::glue_collapse(rows, sep = '')}</tbody>
        </table>
      </div>"
    )
  }

  env$validate_open_ended_data <- function(open_ended_data) {
    if (!is.list(open_ended_data)) {
      stop("`open_ended_data` must be a named list.", call. = FALSE)
    }

    required_sections <- c("summary", "takeaways", "theme_evidence", "quotes", "verbatim")
    missing_sections <- setdiff(required_sections, names(open_ended_data))
    if (length(missing_sections) > 0L) {
      stop(
        sprintf("`open_ended_data` missing section(s): %s.", paste(missing_sections, collapse = ", ")),
        call. = FALSE
      )
    }

    section_cols <- list(
      summary = c("title", "kicker", "narrative"),
      takeaways = c("takeaway_id", "title", "narrative"),
      theme_evidence = c("takeaway_id", "theme_title", "context_text"),
      quotes = c("quote_id", "takeaway_id", "quote_text"),
      verbatim = c("comment_id", "comment_text")
    )

    for (nm in required_sections) {
      part <- open_ended_data[[nm]]
      if (!is.data.frame(part)) {
        stop(sprintf("`open_ended_data$%s` must be a data frame.", nm), call. = FALSE)
      }
      miss <- setdiff(section_cols[[nm]], names(part))
      if (length(miss) > 0L) {
        stop(
          sprintf("`open_ended_data$%s` missing required column(s): %s.", nm, paste(miss, collapse = ", ")),
          call. = FALSE
        )
      }
    }

    if ("meta" %in% names(open_ended_data) && !is.null(open_ended_data$meta) && !is.data.frame(open_ended_data$meta)) {
      stop("`open_ended_data$meta` must be a data frame when provided.", call. = FALSE)
    }

    invisible(open_ended_data)
  }

  env$normalize_open_ended_data <- function(open_ended_data) {
    env$validate_open_ended_data(open_ended_data)

    normalize_chr <- function(df, cols) {
      for (col in cols) {
        if (!col %in% names(df)) next
        df[[col]] <- trimws(as.character(df[[col]]))
      }
      df
    }

    safe_default_df <- function(df, required_cols, defaults) {
      out <- df
      if (nrow(out) < 1L) {
        out <- as.data.frame(defaults, stringsAsFactors = FALSE)
      }
      for (col in required_cols) {
        if (!col %in% names(out)) out[[col]] <- ""
      }
      out[, unique(c(required_cols, names(out))), drop = FALSE]
    }

    summary_df <- safe_default_df(
      normalize_chr(open_ended_data$summary, c("title", "kicker", "narrative", "score_label", "context_note")),
      c("title", "kicker", "narrative"),
      list(
        title = "Open-Ended Insights",
        kicker = "Qualitative Summary",
        narrative = "No summary text is available for this view."
      )
    )
    summary_df <- summary_df[1, , drop = FALSE]

    takeaways_df <- safe_default_df(
      normalize_chr(open_ended_data$takeaways, c("takeaway_id", "title", "narrative", "accent_key")),
      c("takeaway_id", "title", "narrative"),
      list(
        takeaway_id = "takeaway_1",
        title = "Key Takeaway",
        narrative = "No takeaway narrative is available."
      )
    )
    if (!("rank" %in% names(takeaways_df))) {
      takeaways_df$rank <- seq_len(nrow(takeaways_df))
    }
    takeaways_df$rank <- suppressWarnings(as.integer(takeaways_df$rank))
    takeaways_df$rank[!is.finite(takeaways_df$rank)] <- seq_len(sum(!is.finite(takeaways_df$rank)))
    takeaways_df <- takeaways_df[order(takeaways_df$rank, takeaways_df$title), , drop = FALSE]

    theme_df <- normalize_chr(
      open_ended_data$theme_evidence,
      c("takeaway_id", "theme_title", "context_text", "metric_label", "metric_status")
    )
    theme_df$metric_value <- if ("metric_value" %in% names(theme_df)) suppressWarnings(as.numeric(theme_df$metric_value)) else NA_real_
    if (nrow(theme_df) > 0L) {
      theme_df <- theme_df[order(theme_df$takeaway_id, theme_df$theme_title), , drop = FALSE]
      theme_df <- theme_df[!duplicated(paste(theme_df$takeaway_id, theme_df$theme_title, sep = "||")), , drop = FALSE]
    }

    quotes_df <- normalize_chr(
      open_ended_data$quotes,
      c("quote_id", "takeaway_id", "theme_title", "quote_text", "source_tag")
    )
    quotes_df$score <- if ("score" %in% names(quotes_df)) suppressWarnings(as.numeric(quotes_df$score)) else NA_real_
    if (nrow(quotes_df) > 0L) {
      quotes_df <- quotes_df[nzchar(quotes_df$quote_text), , drop = FALSE]
      quotes_df <- quotes_df[order(quotes_df$takeaway_id, quotes_df$theme_title, quotes_df$quote_id), , drop = FALSE]
    }

    verbatim_df <- normalize_chr(
      open_ended_data$verbatim,
      c("comment_id", "takeaway_id", "fundamental", "comment_text")
    )
    if (!("sort_order" %in% names(verbatim_df))) {
      verbatim_df$sort_order <- seq_len(nrow(verbatim_df))
    }
    verbatim_df$sort_order <- suppressWarnings(as.integer(verbatim_df$sort_order))
    verbatim_df$sort_order[!is.finite(verbatim_df$sort_order)] <- seq_len(sum(!is.finite(verbatim_df$sort_order)))
    if (nrow(verbatim_df) > 0L) {
      verbatim_df <- verbatim_df[nzchar(verbatim_df$comment_text), , drop = FALSE]
      verbatim_df <- verbatim_df[order(verbatim_df$takeaway_id, verbatim_df$fundamental, verbatim_df$sort_order, verbatim_df$comment_id), , drop = FALSE]
    }

    meta_df <- if ("meta" %in% names(open_ended_data) && is.data.frame(open_ended_data$meta) && nrow(open_ended_data$meta) > 0L) {
      as.data.frame(open_ended_data$meta[1, , drop = FALSE], stringsAsFactors = FALSE)
    } else {
      data.frame(stringsAsFactors = FALSE)
    }
    if (!("filter_label" %in% names(meta_df))) meta_df$filter_label <- ""
    if (!("n_responses" %in% names(meta_df))) meta_df$n_responses <- NA_integer_
    meta_df$filter_label <- trimws(as.character(meta_df$filter_label))
    meta_df$n_responses <- suppressWarnings(as.integer(meta_df$n_responses))

    list(
      summary = summary_df,
      takeaways = takeaways_df,
      theme_evidence = theme_df,
      quotes = quotes_df,
      verbatim = verbatim_df,
      meta = meta_df
    )
  }

  env$open_ended_theme_pages <- function(dat, quotes_per_page = 4L) {
    themes <- dat$theme_evidence
    if (nrow(themes) < 1L) return(data.frame())

    out <- list()
    page_no <- 1L
    for (i in seq_len(nrow(themes))) {
      t_row <- themes[i, , drop = FALSE]
      t_id <- as.character(t_row$takeaway_id[[1]])
      t_theme <- as.character(t_row$theme_title[[1]])
      q <- dat$quotes[dat$quotes$takeaway_id == t_id, , drop = FALSE]
      if ("theme_title" %in% names(q) && nzchar(t_theme)) {
        q_theme <- q[trimws(as.character(q$theme_title)) == t_theme, , drop = FALSE]
        if (nrow(q_theme) > 0L) q <- q_theme
      }
      n_q <- nrow(q)
      n_pages <- max(1L, ceiling(n_q / as.integer(quotes_per_page)))
      for (p in seq_len(n_pages)) {
        from <- if (n_q < 1L) 1L else ((p - 1L) * as.integer(quotes_per_page) + 1L)
        to <- if (n_q < 1L) 0L else min(n_q, p * as.integer(quotes_per_page))
        out[[length(out) + 1L]] <- data.frame(
          page_index = page_no,
          takeaway_id = t_id,
          theme_title = t_theme,
          quote_from = from,
          quote_to = to,
          is_continuation = p > 1L,
          stringsAsFactors = FALSE
        )
        page_no <- page_no + 1L
      }
    }
    do.call(rbind, out)
  }

  env$open_ended_verbatim_pages <- function(dat, first_page_capacity = 10L, compact_page_capacity = 24L) {
    v <- dat$verbatim
    if (nrow(v) < 1L) {
      empty_first <- data.frame(
        page_index = 1L, group_key = "all_comments", group_label = "All Comments",
        comment_from = 1L, comment_to = 0L, stringsAsFactors = FALSE
      )
      empty_compact <- data.frame(
        page_index = integer(0), group_key = character(0), group_label = character(0),
        comment_from = integer(0), comment_to = integer(0), stringsAsFactors = FALSE
      )
      return(list(first = empty_first, compact = empty_compact))
    }

    key <- if ("takeaway_id" %in% names(v)) trimws(as.character(v$takeaway_id)) else rep("", nrow(v))
    key[!nzchar(key)] <- if ("fundamental" %in% names(v)) trimws(as.character(v$fundamental[!nzchar(key)])) else ""
    key[!nzchar(key)] <- "all_comments"
    v$.__group_key <- key

    groups <- unique(v$.__group_key)
    first_rows <- list()
    compact_rows <- list()
    compact_page_no <- 1L

    for (i in seq_along(groups)) {
      g <- groups[[i]]
      vg <- v[v$.__group_key == g, , drop = FALSE]
      n <- nrow(vg)
      first_to <- min(n, as.integer(first_page_capacity))
      group_label <- if (identical(g, "all_comments")) "All Comments" else g

      first_rows[[length(first_rows) + 1L]] <- data.frame(
        page_index = i,
        group_key = g,
        group_label = group_label,
        comment_from = 1L,
        comment_to = first_to,
        stringsAsFactors = FALSE
      )

      if (n > first_to) {
        rem <- n - first_to
        compact_pages <- ceiling(rem / as.integer(compact_page_capacity))
        for (p in seq_len(compact_pages)) {
          from <- first_to + ((p - 1L) * as.integer(compact_page_capacity)) + 1L
          to <- min(n, first_to + p * as.integer(compact_page_capacity))
          compact_rows[[length(compact_rows) + 1L]] <- data.frame(
            page_index = compact_page_no,
            group_key = g,
            group_label = group_label,
            comment_from = from,
            comment_to = to,
            stringsAsFactors = FALSE
          )
          compact_page_no <- compact_page_no + 1L
        }
      }
    }

    compact_df <- if (length(compact_rows) > 0L) do.call(rbind, compact_rows) else {
      data.frame(
        page_index = integer(0), group_key = character(0), group_label = character(0),
        comment_from = integer(0), comment_to = integer(0), stringsAsFactors = FALSE
      )
    }

    list(first = do.call(rbind, first_rows), compact = compact_df)
  }

  env$open_ended_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "open_ended")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-open-ended-root {{
  --oe-bg: {c$oe_slide_bg};
  --oe-card-bg: {c$oe_card_bg};
  --oe-border: {c$oe_border};
  --oe-title: {c$oe_title};
  --oe-sub: {c$oe_sub};
  --oe-muted: {c$oe_muted};
  --oe-brand: {c$oe_brand};
  --oe-brand-soft: {c$oe_brand_soft};
  --oe-warn: {c$oe_warn};
  --oe-good: {c$oe_good};
  width: 100%;
  min-height: 680px;
  background: var(--oe-bg);
  padding: 20px 24px;
  border-radius: 8px;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .oe-kicker {{ font-size: 12px; text-transform: uppercase; letter-spacing: .08em; color: var(--oe-brand); font-weight: 800; margin-bottom: 6px; }}
{scope} .oe-title {{ margin: 0; font-size: 34px; line-height: 1.14; letter-spacing: -.4px; color: var(--oe-title); }}
{scope} .oe-meta {{ margin-top: 6px; font-size: 13px; color: var(--oe-sub); font-weight: 600; }}
{scope} .oe-grid {{ display: grid; gap: 14px; margin-top: 16px; }}
{scope} .oe-grid.three {{ grid-template-columns: repeat(3, minmax(0, 1fr)); }}
{scope} .oe-grid.two {{ grid-template-columns: repeat(2, minmax(0, 1fr)); }}
{scope} .oe-card {{ background: var(--oe-card-bg); border: 1px solid var(--oe-border); border-radius: 12px; padding: 16px; }}
{scope} .oe-narrative {{ font-size: 17px; line-height: 1.55; color: var(--oe-sub); margin: 0; white-space: pre-wrap; }}
{scope} .oe-score {{ display: inline-flex; align-items: baseline; gap: 8px; border-radius: 999px; padding: 6px 12px; background: var(--oe-brand-soft); color: var(--oe-title); font-weight: 800; font-size: 13px; }}
{scope} .oe-score .val {{ font-size: 18px; color: var(--oe-brand); }}
{scope} .oe-tk-rank {{ font-size: 11px; text-transform: uppercase; letter-spacing: .08em; color: var(--oe-muted); font-weight: 800; }}
{scope} .oe-tk-title {{ margin: 6px 0; font-size: 20px; color: var(--oe-title); line-height: 1.2; }}
{scope} .oe-tk-copy {{ margin: 0; color: var(--oe-sub); line-height: 1.48; font-size: 15px; }}
{scope} .oe-theme-title {{ margin: 0 0 8px 0; font-size: 24px; color: var(--oe-title); }}
{scope} .oe-context {{ margin: 0; color: var(--oe-sub); line-height: 1.5; font-size: 15px; }}
{scope} .oe-pill {{ display: inline-flex; align-items: center; gap: 8px; padding: 6px 10px; border-radius: 999px; border: 1px solid var(--oe-border); background: #f8fafc; font-size: 12px; color: var(--oe-sub); font-weight: 700; }}
{scope} .oe-pill strong {{ color: var(--oe-title); }}
{scope} .oe-quotes {{ margin-top: 14px; display: grid; gap: 10px; }}
{scope} .oe-quote {{ margin: 0; padding: 12px 14px; border-left: 4px solid var(--oe-brand); background: #f8fdff; border-radius: 8px; }}
{scope} .oe-quote p {{ margin: 0; color: var(--oe-title); font-size: 15px; line-height: 1.45; font-style: italic; }}
{scope} .oe-quote .src {{ margin-top: 8px; font-size: 12px; color: var(--oe-muted); font-style: normal; font-weight: 700; }}
{scope} .oe-page-note {{ margin-top: 8px; color: var(--oe-muted); font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: .06em; }}
{scope} .oe-verbatim-feature {{ margin-top: 12px; display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px; }}
{scope} .oe-verbatim-feature .quote {{ background: #fff; border: 1px solid var(--oe-border); border-radius: 12px; padding: 14px; font-size: 15px; line-height: 1.45; color: var(--oe-title); }}
{scope} .oe-verbatim-rest {{ margin-top: 10px; display: grid; gap: 8px; }}
{scope} .oe-verbatim-rest .row {{ border: 1px solid var(--oe-border); border-radius: 10px; padding: 10px 12px; background: #fff; color: var(--oe-sub); font-size: 14px; line-height: 1.4; }}
{scope} .oe-compact-grid {{ margin-top: 12px; columns: 2; column-gap: 10px; }}
{scope} .oe-compact-item {{ break-inside: avoid; margin: 0 0 8px 0; border: 1px solid var(--oe-border); border-radius: 8px; padding: 8px 10px; background: #fff; font-size: 13px; color: var(--oe-sub); line-height: 1.35; }}
{scope} .oe-empty {{ margin-top: 14px; border: 1px dashed var(--oe-border); border-radius: 12px; padding: 16px; background: #fff; color: var(--oe-muted); font-weight: 700; }}
{scope} .theme-shell {{ width: 100%; max-width: 1180px; margin: 0 auto; background: #FFFFFF; border: 1px solid var(--oe-border); border-radius: 12px; box-shadow: 0 10px 30px rgba(15, 23, 42, 0.08); padding: 32px 36px; }}
{scope} .theme-section {{ width: 100%; max-width: 1100px; display: flex; flex-direction: column; margin: 0 auto; }}
{scope} .theme-page-header {{ margin-bottom: 32px; }}
{scope} .theme-page-eyebrow {{ font-size: 11px; font-weight: 800; color: var(--oe-brand); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; }}
{scope} .theme-page-title {{ font-size: 28px; font-weight: 900; color: var(--oe-title); letter-spacing: -0.5px; margin-bottom: 16px; line-height: 1.15; }}
{scope} .theme-divider {{ width: 100%; height: 2px; background-color: var(--oe-brand); margin-bottom: 16px; }}
{scope} .theme-description {{ font-size: 14px; line-height: 1.5; color: var(--oe-sub); max-width: 950px; }}
{scope} .card-grid {{ display: grid; grid-template-columns: repeat(3, 1fr); gap: 24px; }}
{scope} .theme-card {{
  background: #FFFFFF;
  border: 1px solid var(--oe-border);
  border-radius: 8px;
  width: 100%;
  min-height: 420px;
  box-shadow: 0 4px 12px -4px rgba(15, 23, 42, 0.05);
  display: flex;
  flex-direction: column;
  padding: 24px;
}}
{scope} .card-header {{ margin-bottom: 16px; }}
{scope} .card-eyebrow {{ font-size: 10px; font-weight: 800; color: var(--oe-brand); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; }}
{scope} .card-title {{ font-size: 16px; font-weight: 800; color: var(--oe-title); margin: 0; line-height: 1.3; }}
{scope} .card-body {{ font-size: 12px; line-height: 1.5; color: var(--oe-sub); margin-bottom: 24px; }}
{scope} .quote-watermark {{ position: relative; padding: 12px 16px 12px 28px; margin-top: 8px; }}
{scope} .quote-watermark::before {{
  content: \"\\201C\";
  position: absolute;
  top: -12px;
  left: -4px;
  font-size: 80px;
  color: #F1F5F9;
  line-height: 1;
  font-family: Georgia, serif;
  font-weight: 900;
  z-index: 0;
}}
{scope} .quote-watermark .quote-text {{ position: relative; z-index: 1; font-size: 13px; font-weight: 600; color: var(--oe-title); line-height: 1.5; }}
{scope} .card-footer {{ margin-top: auto; padding-top: 24px; border-top: 1px solid #F1F5F9; display: flex; justify-content: center; }}
{scope} .split-pill {{ display: inline-flex; border: 1px solid var(--oe-border); border-radius: 999px; overflow: hidden; background: #fff; }}
{scope} .pill-left {{ background: #FFFFFF; padding: 6px 12px; font-size: 11px; font-weight: 800; color: var(--oe-title); text-transform: uppercase; letter-spacing: 0.5px; }}
{scope} .pill-left sup {{ font-size: 8px; }}
{scope} .pill-right {{ background: #FFFFFF; padding: 6px 12px; border-left: 1px solid var(--oe-border); font-size: 11px; font-weight: 800; color: var(--oe-brand); text-transform: uppercase; letter-spacing: 0.5px; }}
{scope} .nl-main-card {{ background: #FFFFFF; border: 1px solid var(--oe-border); border-radius: 12px; width: 100%; max-width: 1000px; margin: 0 auto; padding: 44px; box-shadow: 0 10px 25px -5px rgba(15,23,42,.05); display: flex; flex-direction: column; }}
{scope} .nl-eyebrow {{ font-size: 11px; font-weight: 800; color: var(--oe-brand); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; }}
{scope} .nl-title {{ font-size: 32px; font-weight: 900; color: var(--oe-title); letter-spacing: -0.5px; margin: 0 0 24px 0; line-height: 1.2; }}
{scope} .nl-divider {{ width: 100%; height: 2px; background-color: var(--oe-brand); margin-bottom: 32px; }}
{scope} .nl-hero {{ margin-bottom: 32px; padding-left: 24px; border-left: 4px solid var(--oe-brand); }}
{scope} .nl-hero-quote {{ font-family: Georgia, serif; font-size: 24px; color: var(--oe-title); font-style: italic; line-height: 1.4; margin-bottom: 8px; }}
{scope} .nl-hero-attribution {{ font-size: 13px; font-weight: 700; color: var(--oe-muted); text-transform: uppercase; letter-spacing: .5px; line-height: 1.5; }}
{scope} .nl-body {{ font-size: 15px; line-height: 1.7; color: var(--oe-sub); display: flex; flex-direction: column; gap: 16px; margin-bottom: 32px; max-width: 900px; }}
{scope} .nl-attrs {{ display: flex; align-items: center; flex-wrap: wrap; gap: 12px; padding: 16px; background: #F0FDFA; border: 1px solid #CCFBF1; border-radius: 8px; margin-bottom: 32px; }}
{scope} .nl-attrs-label {{ font-size: 12px; font-weight: 800; color: var(--oe-brand); text-transform: uppercase; letter-spacing: .5px; margin-right: 6px; }}
{scope} .nl-pill {{ background: #FFFFFF; border: 1px solid #CCFBF1; padding: 6px 12px; border-radius: 6px; font-size: 13px; font-weight: 700; color: var(--oe-title); display: inline-flex; gap: 8px; box-shadow: 0 1px 2px rgba(13,148,136,.05); }}
{scope} .nl-pill .score {{ color: #E11D48; font-weight: 900; }}
{scope} .nl-pill .score.warn {{ color: #F59E0B; }}
{scope} .nl-grid {{ display: grid; grid-template-columns: 1fr 1fr; gap: 24px; margin-bottom: 48px; }}
{scope} .nl-box {{ padding: 24px; border-radius: 8px; border: 1px solid var(--oe-border); background: #F8FAFC; }}
{scope} .nl-box.driver {{ background: #F0F9FF; border-color: #E0F2FE; }}
{scope} .nl-box-title {{ font-size: 13px; font-weight: 800; color: var(--oe-title); text-transform: uppercase; letter-spacing: .5px; margin-bottom: 8px; }}
{scope} .nl-box.driver .nl-box-title {{ color: #0284C7; }}
{scope} .nl-box-text {{ font-size: 14px; line-height: 1.6; color: var(--oe-sub); }}
{scope} .nl-quotes {{ background: #FAFAF9; border: 1px solid #E7E5E4; border-radius: 12px; padding: 40px; display: flex; flex-direction: column; gap: 34px; }}
{scope} .nl-quotes-header {{ font-size: 14px; font-weight: 800; color: var(--oe-brand); text-transform: uppercase; letter-spacing: 1px; border-bottom: 2px solid #E7E5E4; padding-bottom: 12px; margin-bottom: -6px; }}
{scope} .nl-quote-block {{ display: flex; flex-direction: column; position: relative; padding-left: 44px; }}
{scope} .nl-quote-block::before {{ content: \"\\201C\"; position: absolute; left: 0; top: -12px; font-family: Georgia, serif; font-size: 64px; font-weight: 700; color: #D6D3D1; line-height: 1; }}
{scope} .nl-quote-text {{ font-size: 17px; font-weight: 500; color: var(--oe-title); line-height: 1.6; font-style: italic; margin-bottom: 16px; }}
{scope} .nl-tags {{ display: flex; flex-wrap: wrap; gap: 8px; }}
{scope} .nl-tag {{ background: #FFFFFF; border: 1px solid #D6D3D1; border-radius: 4px; padding: 4px 8px; font-size: 10px; font-weight: 800; color: var(--oe-muted); text-transform: uppercase; letter-spacing: .5px; }}
@media (max-width: 1050px) {{
  {scope} .oe-grid.three {{ grid-template-columns: 1fr; }}
  {scope} .oe-grid.two {{ grid-template-columns: 1fr; }}
  {scope} .oe-verbatim-feature {{ grid-template-columns: 1fr; }}
  {scope} .oe-compact-grid {{ columns: 1; }}
  {scope} .card-grid {{ grid-template-columns: repeat(2, 1fr); }}
  {scope} .nl-grid {{ grid-template-columns: 1fr; }}
}}
@media (max-width: 768px) {{
  {scope} .card-grid {{ grid-template-columns: 1fr; }}
  {scope} .nl-main-card {{ padding: 28px 22px; }}
  {scope} .nl-title {{ font-size: 28px; }}
  {scope} .nl-hero-quote {{ font-size: 21px; }}
  {scope} .nl-quotes {{ padding: 26px; }}
}}
"
    )
  }

  env$render_open_ended_page <- function(
    open_ended_data,
    page_type = c("overall_summary", "three_takeaways", "theme_evidence", "verbatim_first", "verbatim_compact"),
    page_index = 1,
    id = "ohep-open-ended-page",
    color_overrides = NULL,
    ...
  ) {
    page_type <- match.arg(page_type)
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "open_ended"
    )
    dat <- env$normalize_open_ended_data(open_ended_data)
    page_index <- suppressWarnings(as.integer(page_index))
    if (!is.finite(page_index) || page_index < 1L) page_index <- 1L

    first_or <- function(x, default = "") {
      if (length(x) < 1L || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]])))) return(default)
      as.character(x[[1]])
    }
    ordinal <- function(x) {
      x <- suppressWarnings(as.integer(round(x)))
      if (!is.finite(x) || is.na(x)) return("n/a")
      suffix <- if ((x %% 100L) %in% c(11L, 12L, 13L)) "th" else switch(as.character(x %% 10L), `1` = "st", `2` = "nd", `3` = "rd", "th")
      paste0(x, suffix)
    }
    ordinal_html <- function(x) {
      x <- suppressWarnings(as.integer(round(x)))
      if (!is.finite(x) || is.na(x)) return("n/a")
      suffix <- if ((x %% 100L) %in% c(11L, 12L, 13L)) "th" else switch(as.character(x %% 10L), `1` = "st", `2` = "nd", `3` = "rd", "th")
      paste0(x, "<sup>", suffix, "</sup>")
    }

    title <- first_or(dat$summary$title, "Open-Ended Insights")
    kicker <- first_or(dat$summary$kicker, "Qualitative Summary")
    meta_line <- ""

    header_html <- function(k = kicker, t = title, m = meta_line) {
      glue::glue(
        "<div class=\"oe-kicker\">{env$escape_text(k)}</div>
         <h1 class=\"oe-title\">{env$escape_text(t)}</h1>
         {if (nzchar(m)) glue::glue('<div class=\"oe-meta\">{env$escape_text(m)}</div>') else ''}"
      )
    }

    body_html <- switch(
      page_type,
      overall_summary = {
        summary_txt <- first_or(dat$summary$narrative, "")
        score_label <- if ("score_label" %in% names(dat$summary)) first_or(dat$summary$score_label, "") else ""
        score_val <- if ("overall_score" %in% names(dat$summary)) suppressWarnings(as.numeric(dat$summary$overall_score[[1]])) else NA_real_
        context_note <- if ("context_note" %in% names(dat$summary)) first_or(dat$summary$context_note, "") else ""
        glue::glue(
          "{header_html()}
           <div class=\"oe-grid two\">
             <div class=\"oe-card\">
               <p class=\"oe-narrative\">{env$escape_text(summary_txt)}</p>
             </div>
             <div class=\"oe-card\">
               {if (is.finite(score_val) || nzchar(score_label)) glue::glue('<div class=\"oe-score\">{if (nzchar(score_label)) env$escape_text(score_label) else \"Score\"} <span class=\"val\">{if (is.finite(score_val)) sprintf(\"%.1f\", score_val) else \"n/a\"}</span></div>') else '<div class=\"oe-score\">Narrative-first summary</div>'}
               <p class=\"oe-narrative\" style=\"margin-top:12px;\">{env$escape_text(if (nzchar(context_note)) context_note else 'This page summarizes recurring themes from open-ended comments for the current filter cut.')}</p>
             </div>
           </div>"
        )
      },
      three_takeaways = {
        tk <- dat$takeaways
        tk <- tk[order(tk$rank), , drop = FALSE]
        if (nrow(tk) < 3L) {
          pad_n <- 3L - nrow(tk)
          if (pad_n > 0L) {
            tk <- rbind(
              tk,
              data.frame(
                takeaway_id = paste0("placeholder_", seq_len(pad_n)),
                title = rep("Additional takeaway", pad_n),
                narrative = rep("No narrative was supplied for this takeaway.", pad_n),
                rank = seq(from = nrow(tk) + 1L, length.out = pad_n),
                stringsAsFactors = FALSE
              )
            )
          }
        }
        tk <- tk[seq_len(min(3L, nrow(tk))), , drop = FALSE]
        cards <- vapply(seq_len(nrow(tk)), function(i) {
          glue::glue(
            "<div class=\"oe-card\">
               <div class=\"oe-tk-rank\">Takeaway {as.integer(tk$rank[[i]])}</div>
               <h2 class=\"oe-tk-title\">{env$escape_text(tk$title[[i]])}</h2>
               <p class=\"oe-tk-copy\">{env$escape_text(tk$narrative[[i]])}</p>
             </div>"
          )
        }, character(1))
        glue::glue(
          "{header_html(k = 'Three Key Takeaways', t = 'What People Are Telling Us')}
           <div class=\"oe-grid three\">{glue::glue_collapse(cards, sep = '')}</div>"
        )
      },
      theme_evidence = {
        themes <- list(
          list(
            title = "Strategy Is Heard, But Not Operationalized",
            hero_quote = "They\u2019re drawing lines on a map we\u2019re standing on.",
            hero_attr = "The friction between Anchorage decision-making and Slope execution, and how strategy gets lost in translation.",
            body = c(
              "One of the clearest patterns in the data is not a lack of strategic awareness, but a breakdown in how strategy is translated into day-to-day work. Employees across the field can generally articulate the company\u2019s major priorities, suggesting that high-level messaging is reaching them. However, lower scores on Strategy and Communication suggest employees do not consistently understand how those priorities should change planning, staffing, coordination, or execution on the ground.",
              "This gap is especially pronounced between office and field populations. Office-based employees report greater confidence in the company\u2019s direction, while field-based employees are less likely to feel that strategy reflects operational reality. In the qualitative feedback, this does not show up as disagreement with the direction itself. It shows up as frustration that strategy has not been translated into the systems where work actually happens."
            ),
            attrs_label = "Relevant Drivers",
            attrs = list(
              list(label = "Strategy", score = "3.42", cls = ""),
              list(label = "Communication", score = "3.36", cls = "")
            ),
            insight1_title = "What this means",
            insight1_text = "The challenge is less about broad awareness and more about executional translation. Employees are hearing the strategy, but not seeing enough evidence of it in the operating rhythms that shape their work.",
            insight2_title = "Driver analysis insight",
            insight2_text = "Driver analysis suggests that lower Purpose and Strategy scores are contributing meaningfully to Burnout. When employees do not feel a clear connection between company direction, day-to-day work, and broader purpose, effort can begin to feel heavier and less sustainable.",
            quotes = list(
              list(
                text = "I\u2019ve heard the plan three times this year. I still don\u2019t know what it means for my Tuesday.",
                tags = c("Field Supervisor", "Construction & North Slope Ops", "6-10 Years")
              ),
              list(
                text = "We put the deck out, we walked the room, we got good questions. I assumed it landed. Talking to the crews now, I\u2019m not sure it ever left the room.",
                tags = c("Operations Director", "Anchorage", "10+ Years")
              )
            )
          ),
          list(
            title = "The Work Is Demanding \u2014 But the Conditions Around the Work Are the Deeper Strain",
            hero_quote = "The job is hard. The conditions around the job are what break people.",
            hero_attr = "What the field is actually fighting, and how much of it is invisible in the office.",
            body = c(
              "The data suggests that field employees are not primarily struggling with the technical demands of the work itself. Rather, strain appears to be shaped by the conditions surrounding the work: travel complexity, weather delays, shifting scope, inconsistent information, camp experience, and variable support from one job or leader to the next.",
              "On the quantitative side, field groups appear to hold relatively stronger scores on accountability and commitment, but lower results on Communication, Respect, Care & Trust, and Leadership. Office employees, by contrast, are more likely to interpret field challenge through the lens of safety and execution quality. Those matter, but the qualitative data suggests employees are reacting most strongly to the instability and friction around the work, not just the work itself."
            ),
            attrs_label = "Relevant Drivers",
            attrs = list(
              list(label = "Communication", score = "3.21", cls = ""),
              list(label = "Respect, Care & Trust", score = "3.29", cls = ""),
              list(label = "Leadership", score = "3.18", cls = "")
            ),
            insight1_title = "What this means",
            insight1_text = "The company may be interpreting stable operational outcomes as a sign that the field experience is healthy. The data suggests otherwise. Employees are pointing to friction in the system and to whether they feel supported, respected, and set up to succeed.",
            insight2_title = "Driver analysis insight",
            insight2_text = "Lower Respect, Care & Trust and Leadership scores appear closely tied to weaker Engagement and Work Satisfaction in the field. In this environment, logistical friction is not separate from culture; it is one of the main ways culture is experienced.",
            quotes = list(
              list(
                text = "I don\u2019t mind the cold. I mind flying out to Deadhorse, sitting 36 hours for weather, and finding out when I land that the scope changed and nobody told my crew.",
                tags = c("Field Hand", "Energy Services", "1-2 Years")
              ),
              list(
                text = "HSE metrics look good. I think we read that as \u2018the field is fine.\u2019 The field is not fine; they\u2019re just not getting hurt.",
                tags = c("Mid-Level Manager", "Industrial Services", "3-5 Years")
              )
            )
          ),
          list(
            title = "Burnout Risk Is Emerging Before Attrition Does",
            hero_quote = "The people who carry the load are quietly running out of room to carry it.",
            hero_attr = "Burnout is showing up before attrition does, and the early signals are already in the data.",
            body = c(
              "This does not appear to be a workforce in active crisis. That is exactly why this theme matters. The warning signs are present, but they are currently being absorbed by a relatively committed group of employees who continue to deliver despite rising strain.",
              "Scores on Purpose, Performance Development, and Communication are softer than would be expected in a top-quartile environment, while the outcome data suggests elevated Burnout and softening Work Satisfaction. The qualitative data reinforces this: employees are not describing disengagement so much as depletion. In both field and office populations, the company appears to be relying on a high-discretionary-effort group to absorb growth, complexity, and gaps in structure."
            ),
            attrs_label = "Relevant Metrics",
            attrs = list(
              list(label = "Purpose", score = "3.19", cls = ""),
              list(label = "Perf. Development", score = "3.14", cls = ""),
              list(label = "Communication", score = "3.09", cls = ""),
              list(label = "Work Sat.", score = "3.28", cls = ""),
              list(label = "Burnout Risk", score = "3.61", cls = " warn")
            ),
            insight1_title = "What this means",
            insight1_text = "The current model is being sustained partly through effort that may not be indefinitely renewable. This is the point at which strong organizations intervene, before burnout turns into turnover, leadership fatigue, or a measurable drop in execution quality.",
            insight2_title = "Driver analysis insight",
            insight2_text = "Driver analysis indicates that Burnout is most strongly influenced by low Purpose, followed by Communication and Performance Development. This suggests employees are more vulnerable to burnout when work feels relentless, developmental support is limited, and the broader why behind the effort is not consistently felt.",
            quotes = list(
              list(
                text = "I used to come home and want to tell my wife about the job. Now I come home and want to not talk for two days.",
                tags = c("Tool Push", "Petro Star", "6-10 Years")
              ),
              list(
                text = "We\u2019re running on the goodwill of about 40 people. If three of them left, I don\u2019t know what we\u2019d do.",
                tags = c("Department Lead", "Administration", "10+ Years")
              )
            )
          )
        )

        if (!is.finite(page_index) || page_index < 1L || page_index > length(themes)) {
          glue::glue("<div class=\"oe-empty\">No theme evidence is available for this page.</div>")
        } else {
          th <- themes[[page_index]]
          body_html <- paste(vapply(th$body, function(p) glue::glue("<p>{env$escape_text(p)}</p>"), character(1)), collapse = "")
          attrs_html <- paste(vapply(th$attrs, function(a) {
            glue::glue("<div class=\"nl-pill\">{env$escape_text(a$label)} <span class=\"score{a$cls}\">{env$escape_text(a$score)}</span></div>")
          }, character(1)), collapse = "")
          quotes_html <- paste(vapply(th$quotes, function(q) {
            glue::glue(
              "<div class=\"nl-quote-block\">
                 <div class=\"nl-quote-text\">{env$escape_text(q$text)}</div>
               </div>"
            )
          }, character(1)), collapse = "")

          glue::glue(
            "<div class=\"nl-main-card\">
               <header>
                 <div class=\"nl-eyebrow\">Theme {page_index}</div>
                 <h1 class=\"nl-title\">{env$escape_text(th$title)}</h1>
                 <div class=\"nl-divider\"></div>
               </header>

               <div class=\"nl-hero\">
                 <div class=\"nl-hero-quote\">\"{env$escape_text(th$hero_quote)}\"</div>
                 <div class=\"nl-hero-attribution\">&mdash; {env$escape_text(th$hero_attr)}</div>
               </div>

               <div class=\"nl-body\">{body_html}</div>

               <div class=\"nl-attrs\">
                 <span class=\"nl-attrs-label\">{env$escape_text(th$attrs_label)}:</span>
                 {attrs_html}
               </div>

               <div class=\"nl-grid\">
                 <div class=\"nl-box\">
                   <div class=\"nl-box-title\">{env$escape_text(th$insight1_title)}</div>
                   <div class=\"nl-box-text\">{env$escape_text(th$insight1_text)}</div>
                 </div>
                 <div class=\"nl-box driver\">
                   <div class=\"nl-box-title\">{env$escape_text(th$insight2_title)}</div>
                   <div class=\"nl-box-text\">{env$escape_text(th$insight2_text)}</div>
                 </div>
               </div>

               <div class=\"nl-quotes\">
                 <div class=\"nl-quotes-header\">What We\u2019re Hearing</div>
                 {quotes_html}
               </div>
             </div>"
          )
        }
      },
      verbatim_first = {
        maps <- env$open_ended_verbatim_pages(dat = dat, first_page_capacity = 10L, compact_page_capacity = 24L)
        m <- maps$first
        if (nrow(m) < 1L || page_index > nrow(m)) {
          glue::glue("{header_html(k = 'Verbatim Appendix', t = 'Verbatim Comments')}<div class=\"oe-empty\">No verbatim comments are available for this page.</div>")
        } else {
          p_row <- m[page_index, , drop = FALSE]
          vv <- dat$verbatim
          gk <- as.character(p_row$group_key[[1]])
          key <- trimws(as.character(vv$takeaway_id))
          key[!nzchar(key)] <- trimws(as.character(vv$fundamental[!nzchar(key)]))
          key[!nzchar(key)] <- "all_comments"
          vg <- vv[key == gk, , drop = FALSE]
          if (nrow(vg) > 0L && p_row$comment_to[[1]] >= p_row$comment_from[[1]]) {
            vg <- vg[seq.int(p_row$comment_from[[1]], p_row$comment_to[[1]]), , drop = FALSE]
          } else {
            vg <- vg[0, , drop = FALSE]
          }
          featured <- utils::head(vg$comment_text, 2L)
          rest <- if (length(vg$comment_text) > 2L) vg$comment_text[3:length(vg$comment_text)] else character(0)
          feat_html <- if (length(featured) > 0L) {
            paste(vapply(featured, function(txt) {
              glue::glue("<div class=\"quote\">\"{env$escape_text(txt)}\"</div>")
            }, character(1)), collapse = "")
          } else {
            "<div class=\"oe-empty\">No comments in this verbatim group.</div>"
          }
          rest_html <- if (length(rest) > 0L) {
            paste(vapply(rest, function(txt) {
              glue::glue("<div class=\"row\">{env$escape_text(txt)}</div>")
            }, character(1)), collapse = "")
          } else {
            ""
          }
          glue::glue(
            "{header_html(k = 'Verbatim Appendix', t = as.character(p_row$group_label[[1]]))}
             <div class=\"oe-verbatim-feature\">{feat_html}</div>
             {if (nzchar(rest_html)) glue::glue('<div class=\"oe-verbatim-rest\">{rest_html}</div>') else ''}"
          )
        }
      },
      verbatim_compact = {
        maps <- env$open_ended_verbatim_pages(dat = dat, first_page_capacity = 10L, compact_page_capacity = 24L)
        m <- maps$compact
        if (nrow(m) < 1L || page_index > nrow(m)) {
          glue::glue("{header_html(k = 'Verbatim Appendix', t = 'Verbatim Continuation')}<div class=\"oe-empty\">No continuation pages are required for this filter.</div>")
        } else {
          p_row <- m[page_index, , drop = FALSE]
          vv <- dat$verbatim
          key <- trimws(as.character(vv$takeaway_id))
          key[!nzchar(key)] <- trimws(as.character(vv$fundamental[!nzchar(key)]))
          key[!nzchar(key)] <- "all_comments"
          vg <- vv[key == as.character(p_row$group_key[[1]]), , drop = FALSE]
          if (nrow(vg) > 0L && p_row$comment_to[[1]] >= p_row$comment_from[[1]]) {
            vg <- vg[seq.int(p_row$comment_from[[1]], p_row$comment_to[[1]]), , drop = FALSE]
          } else {
            vg <- vg[0, , drop = FALSE]
          }
          items <- if (nrow(vg) > 0L) paste(vapply(vg$comment_text, function(txt) {
            glue::glue("<div class=\"oe-compact-item\">{env$escape_text(txt)}</div>")
          }, character(1)), collapse = "") else "<div class=\"oe-empty\">No comments available.</div>"
          glue::glue(
            "{header_html(k = 'Verbatim Appendix', t = paste0(as.character(p_row$group_label[[1]]), ' (Continuation)'))}
             <div class=\"oe-compact-grid\">{items}</div>"
          )
        }
      }
    )

    css <- env$open_ended_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(glue::glue("<div id=\"{env$escape_text(id)}\" class=\"ohep-open-ended-root\">{body_html}</div>"))
    )
  }

  env$validate_enps_data <- function(enps_data) {
    if (!is.list(enps_data)) {
      stop("`enps_data` must be a named list.", call. = FALSE)
    }
    if (!("distribution" %in% names(enps_data)) || !is.data.frame(enps_data$distribution)) {
      stop("`enps_data$distribution` must be a data frame.", call. = FALSE)
    }
    dist <- enps_data$distribution
    needed_dist <- c("rating", "pct")
    if (length(setdiff(needed_dist, names(dist))) > 0L) {
      stop("`enps_data$distribution` must include `rating` and `pct` columns.", call. = FALSE)
    }
    ratings <- suppressWarnings(as.integer(dist$rating))
    pcts <- suppressWarnings(as.numeric(dist$pct))
    if (anyNA(ratings) || anyNA(pcts)) {
      stop("`distribution$rating` and `distribution$pct` must be numeric/non-missing.", call. = FALSE)
    }
    if (any(ratings < 0L | ratings > 10L)) {
      stop("`distribution$rating` values must be in 0..10.", call. = FALSE)
    }
    if (any(pcts < 0 | pcts > 100)) {
      stop("`distribution$pct` values must be in 0..100.", call. = FALSE)
    }
    total <- sum(pcts)
    if (abs(total - 100) > 1.01) {
      stop("`distribution$pct` must sum to ~100.", call. = FALSE)
    }
    if ("drivers" %in% names(enps_data) && !is.null(enps_data$drivers)) {
      if (!is.data.frame(enps_data$drivers)) {
        stop("`enps_data$drivers` must be a data frame when provided.", call. = FALSE)
      }
      if (nrow(enps_data$drivers) > 0L) {
        needed_drv <- c("fundamental")
        miss_drv <- setdiff(needed_drv, names(enps_data$drivers))
        if (length(miss_drv) > 0L) {
          stop(sprintf("`enps_data$drivers` missing: %s", paste(miss_drv, collapse = ", ")), call. = FALSE)
        }
      }
    }
    invisible(enps_data)
  }

  env$normalize_enps_data <- function(enps_data) {
    env$validate_enps_data(enps_data)

    dist <- enps_data$distribution[, c("rating", "pct"), drop = FALSE]
    dist$rating <- suppressWarnings(as.integer(dist$rating))
    dist$pct <- suppressWarnings(as.numeric(dist$pct))

    full_ratings <- data.frame(rating = 0:10, stringsAsFactors = FALSE)
    dist <- merge(full_ratings, dist, by = "rating", all.x = TRUE, sort = TRUE)
    dist$pct[is.na(dist$pct)] <- 0
    dist <- dist[order(dist$rating), , drop = FALSE]

    detractors_pct <- sum(dist$pct[dist$rating <= 6], na.rm = TRUE)
    passives_pct <- sum(dist$pct[dist$rating %in% c(7, 8)], na.rm = TRUE)
    promoters_pct <- sum(dist$pct[dist$rating >= 9], na.rm = TRUE)

    summary <- if ("summary" %in% names(enps_data) && is.data.frame(enps_data$summary) && nrow(enps_data$summary) >= 1L) {
      enps_data$summary[1, , drop = FALSE]
    } else {
      data.frame(stringsAsFactors = FALSE)
    }

    score <- if ("score" %in% names(summary)) suppressWarnings(as.numeric(summary$score[[1]])) else NA_real_
    if (!is.finite(score)) {
      score <- promoters_pct - detractors_pct
    }
    score <- max(-100, min(100, score))

    score_delta <- if ("score_delta" %in% names(summary)) suppressWarnings(as.numeric(summary$score_delta[[1]])) else NA_real_
    if (!is.finite(score_delta)) score_delta <- 0

    title <- if ("title" %in% names(summary)) as.character(summary$title[[1]]) else "Employee Net Promoter Score (eNPS)"
    subtitle <- if ("subtitle" %in% names(summary)) as.character(summary$subtitle[[1]]) else "Calculated difference between promoters and detractors."
    delta_label <- if ("delta_label" %in% names(summary)) as.character(summary$delta_label[[1]]) else "vs benchmark"

    drivers <- if ("drivers" %in% names(enps_data) && is.data.frame(enps_data$drivers) && nrow(enps_data$drivers) > 0L) {
      d <- enps_data$drivers
      d$fundamental <- as.character(d$fundamental)
      d$rank <- if ("rank" %in% names(d)) suppressWarnings(as.integer(d$rank)) else seq_len(nrow(d))
      d$percentile <- if ("percentile" %in% names(d)) suppressWarnings(as.numeric(d$percentile)) else NA_real_
      d$status_label <- if ("status_label" %in% names(d)) as.character(d$status_label) else ifelse(
        is.finite(d$percentile) & d$percentile < 40, "Area for Growth",
        ifelse(is.finite(d$percentile) & d$percentile < 60, "Industry Standard", "Above Standard")
      )
      d <- d[order(d$rank), c("rank", "fundamental", "percentile", "status_label"), drop = FALSE]
      utils::head(d, 5L)
    } else {
      data.frame(
        rank = 1:3,
        fundamental = c("Purpose", "Leadership", "Performance development"),
        percentile = c(42, 32, 68),
        status_label = c("Industry Standard", "Area for Growth", "Above Standard"),
        stringsAsFactors = FALSE
      )
    }

    list(
      title = title,
      subtitle = subtitle,
      score = score,
      score_delta = score_delta,
      delta_label = delta_label,
      promoters_pct = promoters_pct,
      passives_pct = passives_pct,
      detractors_pct = detractors_pct,
      distribution = dist,
      drivers = drivers
    )
  }

  env$enps_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "enps")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-enps-root {{
  --enps-shell-bg: {c$enps_shell_bg};
  --enps-slide-bg: {c$enps_slide_bg};
  --enps-card-bg: {c$enps_card_bg};
  --enps-card-border: {c$enps_card_border};
  --enps-card-shadow: {c$enps_card_shadow};
  --enps-title: {c$enps_title};
  --enps-subtitle: {c$enps_subtitle};
  --enps-score-value: {c$enps_score_value};
  --enps-score-label: {c$enps_score_label};
  --enps-delta-bg: {c$enps_delta_bg};
  --enps-delta-text: {c$enps_delta_text};
  --enps-axis-baseline: {c$enps_axis_baseline};
  --enps-axis-baseline-label: {c$enps_axis_baseline_label};
  --enps-tug-detractor-bg: {c$enps_tug_detractor_bg};
  --enps-tug-passive-bg: {c$enps_tug_passive_bg};
  --enps-tug-promoter-bg: {c$enps_tug_promoter_bg};
  --enps-tug-text-on: {c$enps_tug_text_on};
  --enps-divider: {c$enps_divider};
  --enps-dist-axis: {c$enps_dist_axis};
  --enps-dist-label: {c$enps_dist_label};
  --enps-det-text: {c$enps_det_text};
  --enps-pas-text: {c$enps_pas_text};
  --enps-pro-text: {c$enps_pro_text};
  width: 100%;
  min-height: 680px;
  background: var(--enps-slide-bg);
  padding: 20px 24px;
  border-radius: 8px;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .top-row {{ display:flex; gap:20px; min-height:280px; }}
{scope} .bottom-row {{ margin-top:18px; }}
{scope} .card {{
  background: var(--enps-card-bg);
  border: 1px solid var(--enps-card-border);
  border-radius: 12px;
  padding: 20px;
  box-shadow: 0 4px 12px var(--enps-card-shadow);
  display: flex;
  flex-direction: column;
}}
{scope} .enps-card {{ flex: 1.2; }}
{scope} .drivers-card {{ flex: 1; }}
{scope} .title-bar {{ display:flex; justify-content:space-between; align-items:flex-start; margin-bottom: 10px; }}
{scope} .card-title {{ margin:0; font-size:22px; font-weight:900; color:var(--enps-title); letter-spacing:-.4px; text-transform:uppercase; }}
{scope} .card-sub {{ margin:4px 0 0 0; font-size:13px; color:var(--enps-subtitle); font-weight:500; }}
{scope} .hero-label {{ font-size:12px; font-weight:800; color:var(--enps-score-label); text-transform:uppercase; letter-spacing:.06em; margin-bottom:4px; }}
{scope} .hero-value {{ font-size:56px; font-weight:900; line-height:1; color:var(--enps-score-value); letter-spacing:-2px; }}
{scope} .delta-pill {{ display:inline-flex; align-items:center; gap:6px; padding:6px 12px; border-radius:6px; font-size:13px; font-weight:800; background:var(--enps-delta-bg); color:var(--enps-delta-text); }}
{scope} .tug-container {{ margin-top:auto; padding-bottom: 6px; }}
{scope} .tug-axis {{ position:relative; height:42px; display:flex; align-items:center; margin-bottom: 18px; }}
{scope} .tug-mid {{ position:absolute; left:50%; height:56px; width:2px; background:var(--enps-axis-baseline); z-index:2; transform:translateX(-50%); }}
{scope} .tug-mid-lbl {{ position:absolute; left:50%; top:-20px; transform:translateX(-50%); font-size:11px; font-weight:800; color:var(--enps-axis-baseline-label); text-transform:uppercase; }}
{scope} .tug-left, {scope} .tug-right {{ position:absolute; height:100%; display:flex; align-items:center; font-size:14px; font-weight:800; color:var(--enps-tug-text-on); }}
{scope} .tug-left {{ right:50%; justify-content:flex-end; padding-right:12px; border-radius:6px 0 0 6px; background:var(--enps-tug-detractor-bg); }}
{scope} .tug-right {{ left:50%; justify-content:flex-start; padding-left:12px; border-radius:0 6px 6px 0; background:var(--enps-tug-promoter-bg); }}
{scope} .tug-labels {{ display:flex; justify-content:space-between; align-items:center; }}
{scope} .t-leg-group {{ display:flex; flex-direction:column; gap:2px; }}
{scope} .t-leg-group.left {{ align-items:flex-start; }}
{scope} .t-leg-group.right {{ align-items:flex-end; }}
{scope} .t-title {{ font-size:12px; font-weight:800; text-transform:uppercase; letter-spacing:.04em; }}
{scope} .t-title.det {{ color:var(--enps-det-text); }}
{scope} .t-title.pro {{ color:var(--enps-pro-text); }}
{scope} .t-math {{ font-size:18px; font-weight:900; color:var(--enps-score-value); }}
{scope} .table-outcomes {{ width:100%; border-collapse:collapse; margin-top:auto; }}
{scope} .table-outcomes th {{ text-align:left; padding-bottom:10px; font-size:10px; font-weight:800; color:#94a3b8; text-transform:uppercase; letter-spacing:.05em; border-bottom:2px solid #e2e8f0; }}
{scope} .table-outcomes th.right-align {{ text-align:right; }}
{scope} .table-outcomes td {{ padding:12px 0; border-bottom:1px solid #f1f5f9; vertical-align:middle; }}
{scope} .table-outcomes tr:last-child td {{ border-bottom:none; padding-bottom:0; }}
{scope} .row-rank {{ font-size:22px; font-weight:800; color:#cbd5e1; margin-right:12px; font-variant-numeric:tabular-nums; }}
{scope} .row-name {{ font-size:15px; font-weight:800; color:#111827; }}
{scope} .company-score-stack {{ display:flex; flex-direction:column; align-items:flex-end; }}
{scope} .row-score-val {{ font-size:15px; font-weight:800; line-height:1; }}
{scope} .row-status {{ font-size:10px; font-weight:800; text-transform:uppercase; letter-spacing:.03em; margin-top:4px; }}
{scope} .color-risk {{ color:#f97316; }}
{scope} .color-watch {{ color:#D97706; }}
{scope} .color-good {{ color:#16A34A; }}
{scope} .dist-wrapper {{ display:flex; flex-direction:column; width:100%; max-width:950px; margin:8px auto 0 auto; }}
{scope} .dist-bars {{ display:flex; align-items:flex-end; justify-content:center; gap:40px; border-bottom:2px solid var(--enps-dist-axis); height:120px; }}
{scope} .d-bar-group {{ display:flex; align-items:flex-end; gap:12px; height:100%; }}
{scope} .d-bar {{ width:56px; border-radius:4px 4px 0 0; }}
{scope} .d-bar.det {{ background:var(--enps-tug-detractor-bg); }}
{scope} .d-bar.pas {{ background:var(--enps-tug-passive-bg); }}
{scope} .d-bar.pro {{ background:var(--enps-tug-promoter-bg); }}
{scope} .dist-labels {{ display:flex; align-items:flex-start; justify-content:center; gap:40px; margin-top:12px; }}
{scope} .l-group {{ display:flex; align-items:flex-start; gap:12px; }}
{scope} .d-label {{ width:56px; text-align:center; font-size:13px; font-weight:800; color:var(--enps-dist-label); }}
{scope} .dist-brackets {{ display:flex; justify-content:center; gap:40px; margin-top:24px; border-top:1px dashed #E2E8F0; padding-top:16px; }}
{scope} .bracket {{ text-align:center; font-size:13px; font-weight:800; text-transform:uppercase; letter-spacing:.05em; }}
{scope} .bracket.det {{ width:464px; color:var(--enps-det-text); }}
{scope} .bracket.pas {{ width:124px; color:var(--enps-pas-text); }}
{scope} .bracket.pro {{ width:124px; color:var(--enps-pro-text); }}
"
    )
  }

  env$render_enps_page <- function(
    enps_data,
    id = "ohep-enps-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "enps"
    )

    dat <- env$normalize_enps_data(enps_data)
    dist <- dat$distribution

    bars_raw <- dist$pct
    max_bar <- suppressWarnings(max(bars_raw, na.rm = TRUE))
    if (!is.finite(max_bar) || max_bar <= 0) max_bar <- 1
    bar_h <- pmax(0, (bars_raw / max_bar) * 100)

    tug_den <- max(c(dat$promoters_pct, dat$detractors_pct, 1), na.rm = TRUE)
    tug_left <- (dat$detractors_pct / tug_den) * 46
    tug_right <- (dat$promoters_pct / tug_den) * 46

    score_txt <- if (dat$score > 0) paste0("+", round(dat$score)) else as.character(round(dat$score))
    delta_sign <- if (dat$score_delta >= 0) "▲" else "▼"
    delta_prefix <- if (dat$score_delta >= 0) "+ " else "- "
    delta_txt <- paste0(delta_sign, " ", delta_prefix, abs(round(dat$score_delta)), " ", env$escape_text(dat$delta_label))

    det_idx <- which(dist$rating <= 6)
    pas_idx <- which(dist$rating %in% c(7, 8))
    pro_idx <- which(dist$rating >= 9)

    mk_bars <- function(idxs, cls) {
      paste(vapply(idxs, function(i) {
        glue::glue("<div class=\"d-bar {cls}\" style=\"height:{sprintf('%.1f', bar_h[[i]])}%;\"></div>")
      }, character(1)), collapse = "")
    }
    mk_labels <- function(idxs, cls) {
      paste(vapply(idxs, function(i) {
        glue::glue("<span class=\"d-label {cls}\">{dist$rating[[i]]}</span>")
      }, character(1)), collapse = "")
    }

    status_class <- function(p) {
      if (!is.finite(p)) return("color-watch")
      if (p < 40) return("color-risk")
      if (p < 60) return("color-watch")
      "color-good"
    }
    status_text <- function(p) {
      if (!is.finite(p)) return("Industry Standard")
      if (p < 40) return("Area for Growth")
      if (p < 60) return("Industry Standard")
      "Above Standard"
    }
    driver_rows <- paste(vapply(seq_len(nrow(dat$drivers)), function(i) {
      d <- dat$drivers[i, , drop = FALSE]
      pct <- suppressWarnings(as.numeric(d$percentile[[1]]))
      cls <- status_class(pct)
      st <- as.character(d$status_label[[1]])
      if (!nzchar(st) || is.na(st)) st <- status_text(pct)
      pct_txt <- if (is.finite(pct)) paste0(env$format_ordinal_html(pct), " Percentile") else "n/a"
      glue::glue(
        "<tr>
          <td>
            <span class=\"row-rank\">{as.integer(d$rank[[1]])}</span>
            <span class=\"row-name\">{env$escape_text(d$fundamental[[1]])}</span>
          </td>
          <td class=\"right-align\">
            <div class=\"company-score-stack\">
              <span class=\"row-score-val {cls}\">{pct_txt}</span>
              <span class=\"row-status {cls}\">{env$escape_text(st)}</span>
            </div>
          </td>
        </tr>"
      )
    }, character(1)), collapse = "")

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-enps-root\">
        <div class=\"top-row\">
          <div class=\"card enps-card\">
            <div class=\"title-bar\" style=\"margin-bottom:0;\">
              <div style=\"display:flex;flex-direction:column;\">
                <h2 class=\"card-title\">{env$escape_text(dat$title)}</h2>
                <p class=\"card-sub\">{env$escape_text(dat$subtitle)}</p>
              </div>
            </div>
            <div class=\"hero-row\" style=\"display:flex;align-items:flex-end;gap:20px;margin:16px 0 20px 0;\">
              <div>
                <div class=\"hero-label\">Net Score</div>
                <div class=\"hero-value\">{score_txt}</div>
              </div>
              <div style=\"padding-bottom:6px;\"><span class=\"delta-pill\">{delta_txt}</span></div>
            </div>
            <div class=\"tug-container\">
              <div class=\"tug-axis\">
                <div class=\"tug-mid\"><span class=\"tug-mid-lbl\">Baseline</span></div>
                <div class=\"tug-left\" style=\"width:{sprintf('%.1f', tug_left)}%;\">{sprintf('%.0f%%', dat$detractors_pct)}</div>
                <div class=\"tug-right\" style=\"width:{sprintf('%.1f', tug_right)}%;\">{sprintf('%.0f%%', dat$promoters_pct)}</div>
              </div>
              <div class=\"tug-labels\">
                <div class=\"t-leg-group left\">
                  <span class=\"t-title det\">Detractors</span>
                  <span class=\"t-math\">- {sprintf('%.0f', dat$detractors_pct)}</span>
                </div>
                <div class=\"t-leg-group right\">
                  <span class=\"t-title pro\">Promoters</span>
                  <span class=\"t-math\">+ {sprintf('%.0f', dat$promoters_pct)}</span>
                </div>
              </div>
            </div>
          </div>
          <div class=\"card drivers-card\">
            <div class=\"title-bar\" style=\"margin-bottom:0;\"><h2 class=\"card-title\">Key Drivers</h2></div>
            <p class=\"card-sub\">The fundamentals driving <strong>eNPS</strong> most.</p>
            <table class=\"table-outcomes\">
              <thead>
                <tr><th>Fundamental</th><th class=\"right-align\">Your Company</th></tr>
              </thead>
              <tbody>{driver_rows}</tbody>
            </table>
          </div>
        </div>
        <div class=\"bottom-row\">
          <div class=\"card\" style=\"width:100%;\">
            <div class=\"title-bar\" style=\"margin-bottom:0;\"><h2 class=\"card-title\">Population Distribution</h2></div>
            <p class=\"card-sub\">Detailed breakdown of the 0-10 rating scale across the organization.</p>
            <div class=\"dist-wrapper\">
              <div class=\"dist-bars\">
                <div class=\"d-bar-group\">{mk_bars(det_idx, 'det')}</div>
                <div class=\"d-bar-group\">{mk_bars(pas_idx, 'pas')}</div>
                <div class=\"d-bar-group\">{mk_bars(pro_idx, 'pro')}</div>
              </div>
              <div class=\"dist-labels\">
                <div class=\"l-group\">{mk_labels(det_idx, 'det')}</div>
                <div class=\"l-group\">{mk_labels(pas_idx, 'pas')}</div>
                <div class=\"l-group\">{mk_labels(pro_idx, 'pro')}</div>
              </div>
              <div class=\"dist-brackets\">
                <div class=\"bracket det\">Detractors ({sprintf('%.0f%%', dat$detractors_pct)})</div>
                <div class=\"bracket pas\">Passives ({sprintf('%.0f%%', dat$passives_pct)})</div>
                <div class=\"bracket pro\">Promoters ({sprintf('%.0f%%', dat$promoters_pct)})</div>
              </div>
            </div>
          </div>
        </div>
      </div>"
    )

    css <- env$enps_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_heatmap_data <- function(heatmap_data) {
    if (!is.list(heatmap_data)) {
      stop("`heatmap_data` must be a named list.", call. = FALSE)
    }
    if (!("tables" %in% names(heatmap_data)) || !is.list(heatmap_data$tables) || length(heatmap_data$tables) < 1L) {
      stop("`heatmap_data$tables` must be a non-empty named list of data frames.", call. = FALSE)
    }

    tables <- heatmap_data$tables
    if (is.null(names(tables))) {
      stop("`heatmap_data$tables` must be named (card title per table).", call. = FALSE)
    }
    bad_names <- names(tables) == "" | is.na(names(tables))
    if (any(bad_names)) {
      stop("All `heatmap_data$tables` entries must have non-empty names.", call. = FALSE)
    }

    for (i in seq_along(tables)) {
      tbl <- tables[[i]]
      if (!is.data.frame(tbl) || nrow(tbl) < 1L || ncol(tbl) < 2L) {
        stop(
          sprintf("Heatmap table `%s` must be a data frame with >=1 row and >=2 columns.", names(tables)[[i]]),
          call. = FALSE
        )
      }
    }
    invisible(heatmap_data)
  }

  env$normalize_heatmap_data <- function(heatmap_data) {
    env$validate_heatmap_data(heatmap_data)

    title <- if ("title" %in% names(heatmap_data)) as.character(heatmap_data$title[[1]]) else "Internal Comparisons Heatmap"
    subtitle <- if ("subtitle" %in% names(heatmap_data)) as.character(heatmap_data$subtitle[[1]]) else ""
    legend_low <- if ("legend_low" %in% names(heatmap_data)) as.character(heatmap_data$legend_low[[1]]) else "Below Avg"
    legend_high <- if ("legend_high" %in% names(heatmap_data)) as.character(heatmap_data$legend_high[[1]]) else "Above Avg"

    normalize_tables <- function(tbl_list) {
      lapply(seq_along(tbl_list), function(i) {
        table_name <- names(tbl_list)[[i]]
        tbl <- tbl_list[[i]]
        row_label_col <- names(tbl)[[1]]
        group_cols <- names(tbl)[-1]

        out <- tbl
        out[[row_label_col]] <- as.character(out[[row_label_col]])
        for (col in group_cols) {
          out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
        }
        names(out)[[1]] <- "Category"

        list(
          title = table_name,
          data = out
        )
      })
    }

    tables <- normalize_tables(heatmap_data$tables)
    compare_sets <- list(Department = tables)
    compare_sets_outcomes <- compare_sets
    if ("compare_sets" %in% names(heatmap_data) && is.list(heatmap_data$compare_sets) && length(heatmap_data$compare_sets) > 0L) {
      compare_sets <- lapply(heatmap_data$compare_sets, function(set_tables) {
        if (!is.list(set_tables) || length(set_tables) < 1L) return(NULL)
        normalize_tables(set_tables)
      })
      compare_sets <- compare_sets[!vapply(compare_sets, is.null, logical(1))]
      if (length(compare_sets) < 1L) compare_sets <- list(Department = tables)
    }
    if ("compare_sets_outcomes" %in% names(heatmap_data) && is.list(heatmap_data$compare_sets_outcomes) && length(heatmap_data$compare_sets_outcomes) > 0L) {
      compare_sets_outcomes <- lapply(heatmap_data$compare_sets_outcomes, function(set_tables) {
        if (!is.list(set_tables) || length(set_tables) < 1L) return(NULL)
        normalize_tables(set_tables)
      })
      compare_sets_outcomes <- compare_sets_outcomes[!vapply(compare_sets_outcomes, is.null, logical(1))]
      if (length(compare_sets_outcomes) < 1L) compare_sets_outcomes <- compare_sets
    }

    list(
      title = title,
      subtitle = subtitle,
      legend_low = legend_low,
      legend_high = legend_high,
      tables = tables,
      compare_sets = compare_sets,
      compare_sets_outcomes = compare_sets_outcomes
    )
  }

  env$heatmap_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "heatmap")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-heatmap-root {{
  --hm-slide-bg: {c$heatmap_slide_bg};
  --hm-title: {c$heatmap_title};
  --hm-subtitle: {c$heatmap_subtitle};
  --hm-legend-text: {c$heatmap_legend_text};
  --hm-legend-start: {c$heatmap_legend_start};
  --hm-legend-mid: {c$heatmap_legend_mid};
  --hm-legend-end: #16A34A;
  --hm-card-bg: {c$heatmap_card_bg};
  --hm-card-border: {c$heatmap_card_border};
  --hm-card-shadow: {c$heatmap_card_shadow};
  --hm-card-title: {c$heatmap_card_title};
  --hm-header-text: {c$heatmap_table_header_text};
  --hm-header-border: {c$heatmap_table_header_border};
  --hm-row-label: {c$heatmap_row_label_text};
  --hm-row-border: {c$heatmap_row_border};
  --hm-pill-text: {c$heatmap_pill_text};
  --hm-pill-na-bg: {c$heatmap_pill_na_bg};
  --hm-pill-na-text: {c$heatmap_pill_na_text};
  --hm-pill-na-border: {c$heatmap_pill_na_border};
  --hm-pill-pos-strong: #6FCF97;
  --hm-pill-pos-soft: #E4F7EC;
  --hm-pill-neutral: {c$heatmap_pill_neutral};
  --hm-pill-neg-soft: #F9E3E7;
  --hm-pill-neg-strong: #F2B8C2;
  width: 100%;
  background: transparent;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 14px;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .hm-header {{ display: flex; flex-direction: column; gap: 6px; }}
{scope} .hm-title {{ margin: 0; font-size: 22px; font-weight: 900; color: var(--hm-title); text-transform: uppercase; letter-spacing: 0.4px; }}
{scope} .hm-subtitle {{ margin: 0; font-size: 14px; color: var(--hm-subtitle); }}
{scope} .hm-controls {{
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 12px;
  flex-wrap: wrap;
}}
{scope} .hm-view-toggle {{
  display: inline-flex;
  background: #F1F5F9;
  border: 1px solid var(--hm-header-border);
  border-radius: 8px;
  padding: 4px;
}}
{scope} .hm-toggle-btn {{
  border: none;
  background: transparent;
  color: var(--hm-header-text);
  opacity: 0.72;
  padding: 8px 18px;
  border-radius: 6px;
  font-size: 12px;
  font-weight: 800;
  letter-spacing: 0.5px;
  text-transform: uppercase;
  cursor: pointer;
}}
{scope} .hm-toggle-btn.active {{
  background: #FFFFFF;
  color: var(--hm-title);
  opacity: 1;
  box-shadow: 0 2px 4px rgba(15, 23, 42, 0.06);
}}
{scope} .hm-legend {{
  display: inline-flex;
  align-items: center;
  gap: 10px;
  font-size: 10px;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: var(--hm-legend-text);
  margin-left: auto;
}}
{scope} .hm-spectrum {{
  width: 120px;
  height: 6px;
  border-radius: 99px;
  background: linear-gradient(to right, var(--hm-legend-start), var(--hm-legend-mid), var(--hm-legend-end));
}}
{scope} .hm-compare {{
  display: inline-flex;
  align-items: center;
  gap: 10px;
}}
{scope} .hm-compare-label {{
  font-size: 12px;
  font-weight: 700;
  color: var(--hm-legend-text);
}}
{scope} .hm-compare-select {{
  appearance: none;
  border: 1px solid var(--hm-header-border);
  border-radius: 6px;
  padding: 8px 30px 8px 12px;
  background-color: #F8FAFC;
  color: var(--hm-title);
  font-size: 13px;
  font-weight: 600;
  line-height: 1;
  min-width: 180px;
  cursor: pointer;
  background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 24 24' stroke='%2364748B'%3E%3Cpath stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M19 9l-7 7-7-7'/%3E%3C/svg%3E\");
  background-repeat: no-repeat;
  background-position: right 10px center;
  background-size: 14px;
}}
{scope} .hm-table-wrap {{
  border: 1px solid var(--hm-card-border);
  border-radius: 8px;
  background: #FFFFFF;
  overflow-x: auto;
  -webkit-overflow-scrolling: touch;
}}
{scope} .hm-table-wrap::-webkit-scrollbar {{ height: 8px; }}
{scope} .hm-table-wrap::-webkit-scrollbar-track {{ background: #F8FAFC; }}
{scope} .hm-table-wrap::-webkit-scrollbar-thumb {{ background: #CBD5E1; border-radius: 4px; }}
{scope} .hm-table {{ width: 100%; min-width: 900px; border-collapse: collapse; }}
{scope} .hm-table th {{
  padding: 12px 10px;
  border-bottom: 1px solid var(--hm-header-border);
  text-transform: uppercase;
  font-size: 10px;
  font-weight: 800;
  color: var(--hm-header-text);
  text-align: center;
  vertical-align: bottom;
  min-width: 120px;
  line-height: 1.35;
}}
{scope} .hm-table th:first-child {{
  text-align: left;
  min-width: 180px;
  color: var(--hm-row-label);
}}
{scope} .hm-table td {{ padding: 10px 8px; border-bottom: 1px solid var(--hm-row-border); text-align: center; vertical-align: middle; }}
{scope} .hm-table tr:last-child td {{ border-bottom: none; }}
{scope} .hm-table td:first-child {{ text-align: left; font-size: 12px; font-weight: 700; color: var(--hm-row-label); background: #FFFFFF; }}
{scope} .hm-pill {{ display: inline-flex; align-items: center; justify-content: center; width: 40px; height: 40px; padding: 0; border-radius: 6px; color: var(--hm-pill-text); font-size: 12px; font-weight: 800; line-height: 1; }}
{scope} .hm-pill-pos-strong {{ background: var(--hm-pill-pos-strong); }}
{scope} .hm-pill-pos-soft {{ background: var(--hm-pill-pos-soft); }}
{scope} .hm-pill-neutral {{ background: var(--hm-pill-neutral); }}
{scope} .hm-pill-neg-soft {{ background: var(--hm-pill-neg-soft); }}
{scope} .hm-pill-neg-strong {{ background: var(--hm-pill-neg-strong); }}
{scope} .hm-pill-na {{ background: var(--hm-pill-na-bg); color: var(--hm-pill-na-text); border: 1px dashed var(--hm-pill-na-border); font-weight: 700; }}
"
    )
  }

  env$render_heatmap_page <- function(
    heatmap_data,
    id = "ohep-heatmap-page",
    show_compare_control = TRUE,
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "heatmap"
    )
    dat <- env$normalize_heatmap_data(heatmap_data)

    classify_cell <- function(value, row_mean, row_max_abs_delta) {
      if (!is.finite(value)) return("hm-pill-na")
      if (!is.finite(row_mean) || !is.finite(row_max_abs_delta) || row_max_abs_delta <= 0) return("hm-pill-neutral")
      delta <- value - row_mean
      intensity <- abs(delta) / row_max_abs_delta
      if (!is.finite(intensity) || intensity < 0.16) return("hm-pill-neutral")
      if (delta > 0) {
        return(if (intensity >= 0.55) "hm-pill-pos-strong" else "hm-pill-pos-soft")
      }
      if (delta < 0) {
        return(if (intensity >= 0.55) "hm-pill-neg-strong" else "hm-pill-neg-soft")
      }
      "hm-pill-neutral"
    }

    build_table_html <- function(tables) {
      all_groups <- unique(unlist(lapply(tables, function(table_obj) names(table_obj$data)[-1]), use.names = FALSE))
      header_html <- glue::glue_collapse(vapply(c("Category", all_groups), function(h) {
        glue::glue("<th>{env$escape_text(h)}</th>")
      }, character(1)), sep = "")

      section_rows <- vapply(seq_along(tables), function(idx) {
        table_obj <- tables[[idx]]
        df <- table_obj$data
        groups <- names(df)[-1]

        row_html <- vapply(seq_len(nrow(df)), function(i) {
          category <- env$escape_text(df$Category[[i]])
          row_values <- vapply(all_groups, function(group_name) {
            if (!(group_name %in% groups)) return(NA_real_)
            suppressWarnings(as.numeric(df[[group_name]][[i]]))
          }, numeric(1))
          row_mean <- suppressWarnings(mean(row_values, na.rm = TRUE))
          if (!is.finite(row_mean)) row_mean <- NA_real_
          row_centered <- row_values - row_mean
          row_max_abs_delta <- suppressWarnings(max(abs(row_centered), na.rm = TRUE))
          if (!is.finite(row_max_abs_delta)) row_max_abs_delta <- 0
          cell_html <- vapply(all_groups, function(group_name) {
            value <- if (group_name %in% groups) suppressWarnings(as.numeric(df[[group_name]][[i]])) else NA_real_
            cls <- classify_cell(value, row_mean, row_max_abs_delta)
            txt <- if (is.finite(value)) sprintf("%.2f", value) else "NA"
            glue::glue("<td><span class=\"hm-pill {cls}\">{txt}</span></td>")
          }, character(1))
          glue::glue("<tr><td>{category}</td>{glue::glue_collapse(cell_html, sep = '')}</tr>")
        }, character(1))

        glue::glue("{glue::glue_collapse(row_html, sep = '')}")
      }, character(1))

      glue::glue(
        "<div class=\"hm-table-wrap\">
          <table class=\"hm-table\">
            <thead><tr>{header_html}</tr></thead>
            <tbody>{glue::glue_collapse(section_rows, sep = '')}</tbody>
          </table>
        </div>"
      )
    }

    compare_keys <- names(dat$compare_sets)
    compare_options_html <- paste(vapply(compare_keys, function(key) {
      glue::glue("<option value=\"{env$escape_text(key)}\">{env$escape_text(key)}</option>")
    }, character(1)), collapse = "")
    compare_default <- if ("Company" %in% compare_keys) "Company" else compare_keys[[1]]
    compare_table_map_drivers <- setNames(lapply(dat$compare_sets, build_table_html), compare_keys)
    outcome_sets <- dat$compare_sets_outcomes[names(dat$compare_sets_outcomes) %in% compare_keys]
    compare_table_map_outcomes <- setNames(lapply(outcome_sets, build_table_html), names(outcome_sets))
    if (length(compare_table_map_outcomes) < 1L) compare_table_map_outcomes <- compare_table_map_drivers
    compare_view_map <- list(
      drivers = compare_table_map_drivers,
      outcomes = compare_table_map_outcomes
    )
    compare_table_json <- jsonlite::toJSON(compare_view_map, auto_unbox = TRUE)

    header_html <- if (nzchar(trimws(dat$title)) || nzchar(trimws(dat$subtitle))) {
      glue::glue(
        "<div class=\"hm-header\">
          {if (nzchar(trimws(dat$title))) glue::glue('<h2 class=\"hm-title\">{env$escape_text(dat$title)}</h2>') else ''}
          {if (nzchar(trimws(dat$subtitle))) glue::glue('<p class=\"hm-subtitle\">{env$escape_text(dat$subtitle)}</p>') else ''}
        </div>"
      )
    } else ""

    compare_html <- if (isTRUE(show_compare_control)) {
      glue::glue(
        "<div class=\"hm-compare\">
          <label class=\"hm-compare-label\" for=\"{env$escape_text(id)}-compare\">Compare By:</label>
          <select id=\"{env$escape_text(id)}-compare\" class=\"hm-compare-select\">{compare_options_html}</select>
        </div>"
      )
    } else {
      ""
    }

    controls_html <- glue::glue(
      "<div class=\"hm-controls\">
        <div class=\"hm-view-toggle\" role=\"tablist\" aria-label=\"Heatmap View Toggle\">
          <button type=\"button\" class=\"hm-toggle-btn active\" data-view=\"drivers\">Drivers</button>
          <button type=\"button\" class=\"hm-toggle-btn\" data-view=\"outcomes\">Outcomes</button>
        </div>
        <div class=\"hm-legend\">
          <span>{env$escape_text(dat$legend_low)}</span>
          <div class=\"hm-spectrum\"></div>
          <span>{env$escape_text(dat$legend_high)}</span>
        </div>
        {compare_html}
      </div>"
    )

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-heatmap-root\">
        {header_html}
        {controls_html}
        <div id=\"{env$escape_text(id)}-table-host\"></div>
        <script>
          (function() {{
            var root = document.getElementById('{env$escape_text(id)}');
            if (!root) return;
            var sel = root.querySelector('.hm-compare-select') || document.getElementById('{env$escape_text(id)}-compare');
            var toggleBtns = root.querySelectorAll('.hm-toggle-btn');
            var host = document.getElementById('{env$escape_text(id)}-table-host');
            var tableMap = {compare_table_json};
            var currentView = 'drivers';
            function renderTable(key) {{
              if (!host) return;
              var viewMap = tableMap[currentView] || tableMap['drivers'] || {{}};
              host.innerHTML = viewMap[key] || viewMap['{env$escape_text(compare_default)}'] || '';
            }}
            if (sel) {{
              sel.value = '{env$escape_text(compare_default)}';
              sel.addEventListener('change', function() {{ renderTable(sel.value); }});
            }}
            toggleBtns.forEach(function(btn) {{
              btn.addEventListener('click', function() {{
                currentView = btn.getAttribute('data-view') || 'drivers';
                toggleBtns.forEach(function(other) {{ other.classList.toggle('active', other === btn); }});
                renderTable(sel ? sel.value : '{env$escape_text(compare_default)}');
              }});
            }});
            renderTable('{env$escape_text(compare_default)}');
          }})();
        </script>
      </div>"
    )

    css <- env$heatmap_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_item_distribution_data <- function(item_distribution_data) {
    if (!is.list(item_distribution_data)) {
      stop("`item_distribution_data` must be a named list.", call. = FALSE)
    }
    if (!("items" %in% names(item_distribution_data)) || !is.data.frame(item_distribution_data$items)) {
      stop("`item_distribution_data$items` must be a data frame.", call. = FALSE)
    }
    items <- item_distribution_data$items
    required <- c("label", "mean", "disagree_pct", "neutral_pct", "agree_pct", "vs_industry", "vs_prior")
    missing <- setdiff(required, names(items))
    if (length(missing) > 0L) {
      stop(sprintf("`items` is missing required columns: %s.", paste(missing, collapse = ", ")), call. = FALSE)
    }
    if (nrow(items) < 1L) {
      stop("`items` must contain at least one row.", call. = FALSE)
    }
    for (i in seq_len(nrow(items))) {
      env$normalize_sentiment(
        suppressWarnings(as.numeric(items$disagree_pct[[i]])),
        suppressWarnings(as.numeric(items$neutral_pct[[i]])),
        suppressWarnings(as.numeric(items$agree_pct[[i]]))
      )
    }
    invisible(item_distribution_data)
  }

  env$normalize_item_distribution_data <- function(item_distribution_data) {
    env$validate_item_distribution_data(item_distribution_data)

    summary <- if ("summary" %in% names(item_distribution_data) && is.data.frame(item_distribution_data$summary) && nrow(item_distribution_data$summary) >= 1L) {
      item_distribution_data$summary[1, , drop = FALSE]
    } else {
      data.frame(stringsAsFactors = FALSE)
    }

    title <- if ("title" %in% names(summary)) as.character(summary$title[[1]]) else "Item-Level Sentiment Analysis"
    subtitle <- if ("subtitle" %in% names(summary)) as.character(summary$subtitle[[1]]) else ""
    benchmark_label <- if ("benchmark_label" %in% names(summary)) as.character(summary$benchmark_label[[1]]) else "vs Industry"
    prior_label <- if ("prior_label" %in% names(summary)) as.character(summary$prior_label[[1]]) else "vs Prior"

    items <- item_distribution_data$items
    out <- data.frame(
      label = as.character(items$label),
      mean = suppressWarnings(as.numeric(items$mean)),
      disagree_pct = suppressWarnings(as.numeric(items$disagree_pct)),
      neutral_pct = suppressWarnings(as.numeric(items$neutral_pct)),
      agree_pct = suppressWarnings(as.numeric(items$agree_pct)),
      vs_industry = suppressWarnings(as.numeric(items$vs_industry)),
      vs_prior = suppressWarnings(as.numeric(items$vs_prior)),
      stringsAsFactors = FALSE
    )

    list(
      title = title,
      subtitle = subtitle,
      benchmark_label = benchmark_label,
      prior_label = prior_label,
      items = out
    )
  }

  env$item_distribution_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "item_distribution")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-itemdist-root {{
  --itemdist-slide-bg: {c$itemdist_slide_bg};
  --itemdist-card-bg: {c$itemdist_card_bg};
  --itemdist-card-border: {c$itemdist_card_border};
  --itemdist-card-shadow: {c$itemdist_card_shadow};
  --itemdist-title: {c$itemdist_title};
  --itemdist-subtitle: {c$itemdist_subtitle};
  --itemdist-legend-text: {c$itemdist_legend_text};
  --itemdist-header-label: {c$itemdist_header_label};
  --itemdist-row-label: {c$itemdist_row_label};
  --itemdist-row-border: {c$itemdist_row_border};
  --itemdist-mean-text: {c$itemdist_mean_text};
  --itemdist-stack-bg: {c$itemdist_stack_bg};
  --itemdist-agree: {c$favorability_agree};
  --itemdist-neutral: {c$favorability_neutral};
  --itemdist-disagree: {c$favorability_disagree};
  --itemdist-delta-pos-bg: {c$delta_pos_bg};
  --itemdist-delta-pos-text: {c$delta_pos_text};
  --itemdist-delta-neg-bg: {c$delta_neg_bg};
  --itemdist-delta-neg-text: {c$delta_neg_text};
  --itemdist-delta-neu-bg: {c$delta_neu_bg};
  --itemdist-delta-neu-text: {c$delta_neu_text};
  width: 1280px;
  min-height: 720px;
  background: var(--itemdist-slide-bg);
  padding: 34px 36px;
  border-radius: 8px;
  box-shadow: 0 10px 30px rgba(15, 23, 42, 0.1);
  display: flex;
  justify-content: center;
  align-items: center;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .itemdist-card {{
  width: 1150px;
  background: var(--itemdist-card-bg);
  border: 1px solid var(--itemdist-card-border);
  border-radius: 12px;
  box-shadow: 0 4px 20px var(--itemdist-card-shadow);
  padding: 36px 38px;
}}
{scope} .itemdist-header {{ display: flex; justify-content: space-between; align-items: flex-end; margin-bottom: 30px; gap: 10px; }}
{scope} .itemdist-title {{ margin: 0; font-size: 24px; font-weight: 900; text-transform: uppercase; letter-spacing: -0.4px; color: var(--itemdist-title); }}
{scope} .itemdist-subtitle {{ margin: 4px 0 0; font-size: 13px; color: var(--itemdist-subtitle); }}
{scope} .itemdist-legend {{ display: flex; gap: 16px; font-size: 11px; font-weight: 700; color: var(--itemdist-legend-text); text-transform: uppercase; }}
{scope} .itemdist-leg-item {{ display: flex; align-items: center; gap: 6px; }}
{scope} .itemdist-leg-dot {{ width: 10px; height: 10px; border-radius: 2px; }}
{scope} .itemdist-leg-agree {{ background: var(--itemdist-agree); }}
{scope} .itemdist-leg-neutral {{ background: var(--itemdist-neutral); }}
{scope} .itemdist-leg-disagree {{ background: var(--itemdist-disagree); }}
{scope} .itemdist-header-row, {scope} .itemdist-row {{
  display: grid;
  grid-template-columns: 280px 80px 1fr 100px 100px;
  gap: 20px;
  align-items: center;
}}
{scope} .itemdist-header-row {{ padding-bottom: 12px; border-bottom: 2px solid var(--itemdist-card-border); margin-bottom: 8px; }}
{scope} .itemdist-h-lbl {{ font-size: 11px; font-weight: 800; color: var(--itemdist-header-label); text-transform: uppercase; letter-spacing: 1px; text-align: center; }}
{scope} .itemdist-h-lbl:first-child {{ text-align: left; }}
{scope} .itemdist-row {{ padding: 14px 0; border-bottom: 1px solid var(--itemdist-row-border); }}
{scope} .itemdist-row:last-child {{ border-bottom: none; }}
{scope} .itemdist-label {{ font-size: 13px; line-height: 1.35; color: var(--itemdist-row-label); font-weight: 600; }}
{scope} .itemdist-mean {{ font-size: 15px; font-weight: 800; color: var(--itemdist-mean-text); text-align: center; }}
{scope} .itemdist-stack {{ height: 32px; display: flex; border-radius: 6px; overflow: hidden; background: var(--itemdist-stack-bg); }}
{scope} .itemdist-segment {{ display: flex; align-items: center; justify-content: center; color: #FFFFFF; font-size: 11px; font-weight: 800; }}
{scope} .itemdist-disagree {{ background: var(--itemdist-disagree); }}
{scope} .itemdist-neutral {{ background: var(--itemdist-neutral); }}
{scope} .itemdist-agree {{ background: var(--itemdist-agree); }}
{scope} .itemdist-delta-wrap {{ display: flex; justify-content: center; }}
{scope} .itemdist-delta {{ display: inline-flex; align-items: center; justify-content: center; width: 65px; padding: 6px 0; border-radius: 6px; font-size: 11px; font-weight: 800; }}
{scope} .itemdist-dp-pos {{ background: var(--itemdist-delta-pos-bg); color: var(--itemdist-delta-pos-text); }}
{scope} .itemdist-dp-neg {{ background: var(--itemdist-delta-neg-bg); color: var(--itemdist-delta-neg-text); }}
{scope} .itemdist-dp-neu {{ background: var(--itemdist-delta-neu-bg); color: var(--itemdist-delta-neu-text); }}
"
    )
  }

  env$render_item_distribution_page <- function(
    item_distribution_data,
    id = "ohep-item-distribution-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "item_distribution"
    )
    dat <- env$normalize_item_distribution_data(item_distribution_data)

    delta_class <- function(val) {
      if (!is.finite(val) || val == 0) return("itemdist-dp-neu")
      if (val > 0) "itemdist-dp-pos" else "itemdist-dp-neg"
    }
    delta_text <- function(val) {
      if (!is.finite(val)) return("-")
      paste0(env$format_signed(val), "%")
    }

    rows_html <- vapply(seq_len(nrow(dat$items)), function(i) {
      r <- dat$items[i, , drop = FALSE]
      sent <- env$normalize_sentiment(r$disagree_pct[[1]], r$neutral_pct[[1]], r$agree_pct[[1]])
      disagree_lbl <- if (sent[[1]] >= 8) paste0(round(sent[[1]]), "%") else ""
      neutral_lbl <- if (sent[[2]] >= 8) paste0(round(sent[[2]]), "%") else ""
      agree_lbl <- if (sent[[3]] >= 8) paste0(round(sent[[3]]), "%") else ""

      glue::glue(
        "<div class=\"itemdist-row\">
          <div class=\"itemdist-label\">{env$escape_text(r$label[[1]])}</div>
          <div class=\"itemdist-mean\">{sprintf('%.2f', r$mean[[1]])}</div>
          <div class=\"itemdist-stack\">
            <div class=\"itemdist-segment itemdist-disagree\" style=\"width:{sent[[1]]}%;\">{disagree_lbl}</div>
            <div class=\"itemdist-segment itemdist-neutral\" style=\"width:{sent[[2]]}%;\">{neutral_lbl}</div>
            <div class=\"itemdist-segment itemdist-agree\" style=\"width:{sent[[3]]}%;\">{agree_lbl}</div>
          </div>
          <div class=\"itemdist-delta-wrap\"><span class=\"itemdist-delta {delta_class(r$vs_industry[[1]])}\">{delta_text(r$vs_industry[[1]])}</span></div>
          <div class=\"itemdist-delta-wrap\"><span class=\"itemdist-delta {delta_class(r$vs_prior[[1]])}\">{delta_text(r$vs_prior[[1]])}</span></div>
        </div>"
      )
    }, character(1))

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-itemdist-root\">
        <div class=\"itemdist-card\">
          <div class=\"itemdist-header\">
            <div>
              <h2 class=\"itemdist-title\">{env$escape_text(dat$title)}</h2>
              <p class=\"itemdist-subtitle\">{env$escape_text(dat$subtitle)}</p>
            </div>
            <div class=\"itemdist-legend\">
              <div class=\"itemdist-leg-item\"><span class=\"itemdist-leg-dot itemdist-leg-disagree\"></span>Disagree</div>
              <div class=\"itemdist-leg-item\"><span class=\"itemdist-leg-dot itemdist-leg-neutral\"></span>Neutral</div>
              <div class=\"itemdist-leg-item\"><span class=\"itemdist-leg-dot itemdist-leg-agree\"></span>Agree</div>
            </div>
          </div>
          <div class=\"itemdist-header-row\">
            <div class=\"itemdist-h-lbl\">Survey Item</div>
            <div class=\"itemdist-h-lbl\">Mean</div>
            <div class=\"itemdist-h-lbl\">Sentiment Distribution</div>
            <div class=\"itemdist-h-lbl\">{env$escape_text(dat$benchmark_label)}</div>
            <div class=\"itemdist-h-lbl\">{env$escape_text(dat$prior_label)}</div>
          </div>
          {glue::glue_collapse(rows_html, sep = '')}
        </div>
      </div>"
    )

    css <- env$item_distribution_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_model_data <- function(model_data) {
    if (!is.list(model_data)) {
      stop("`model_data` must be a named list.", call. = FALSE)
    }
    if (!("fundamentals" %in% names(model_data)) || !is.data.frame(model_data$fundamentals)) {
      stop("`model_data$fundamentals` must be a data frame.", call. = FALSE)
    }
    if (!("outcomes" %in% names(model_data)) || !is.data.frame(model_data$outcomes)) {
      stop("`model_data$outcomes` must be a data frame.", call. = FALSE)
    }

    check_section <- function(df, section_name) {
      required <- c("label", "percentile")
      missing <- setdiff(required, names(df))
      if (length(missing) > 0L) {
        stop(
          sprintf("`model_data$%s` missing required columns: %s.", section_name, paste(missing, collapse = ", ")),
          call. = FALSE
        )
      }
      if (nrow(df) < 1L) {
        stop(sprintf("`model_data$%s` must have at least one row.", section_name), call. = FALSE)
      }
      pct <- suppressWarnings(as.numeric(df$percentile))
      if (any(!is.finite(pct))) {
        stop(sprintf("`model_data$%s$percentile` must be numeric/non-missing.", section_name), call. = FALSE)
      }
      if (any(pct < 0 | pct > 100)) {
        stop(sprintf("`model_data$%s$percentile` must be in 0..100.", section_name), call. = FALSE)
      }
      invisible(TRUE)
    }

    check_section(model_data$fundamentals, "fundamentals")
    check_section(model_data$outcomes, "outcomes")
    invisible(model_data)
  }

  env$normalize_model_data <- function(model_data) {
    env$validate_model_data(model_data)

    summary <- if ("summary" %in% names(model_data) && is.data.frame(model_data$summary) && nrow(model_data$summary) >= 1L) {
      model_data$summary[1, , drop = FALSE]
    } else {
      data.frame(stringsAsFactors = FALSE)
    }

    to_df <- function(df, default_shape) {
      out <- data.frame(
        label = as.character(df$label),
        percentile = suppressWarnings(as.numeric(df$percentile)),
        prior_percentile = if ("prior_percentile" %in% names(df)) suppressWarnings(as.numeric(df$prior_percentile)) else NA_real_,
        raw_avg = if ("raw_avg" %in% names(df)) suppressWarnings(as.numeric(df$raw_avg)) else NA_real_,
        delta = if ("delta" %in% names(df)) suppressWarnings(as.numeric(df$delta)) else NA_real_,
        shape = if ("shape" %in% names(df)) as.character(df$shape) else default_shape,
        stringsAsFactors = FALSE
      )
      out$shape[is.na(out$shape) | out$shape == ""] <- default_shape
      out
    }

    list(
      title = if ("title" %in% names(summary)) as.character(summary$title[[1]]) else "Health Diagnostics",
      subtitle = if ("subtitle" %in% names(summary)) as.character(summary$subtitle[[1]]) else "",
      fundamentals_label = if ("fundamentals_label" %in% names(summary)) as.character(summary$fundamentals_label[[1]]) else "",
      outcomes_label = if ("outcomes_label" %in% names(summary)) as.character(summary$outcomes_label[[1]]) else "Business Outcomes",
      raw_avg_label = if ("raw_avg_label" %in% names(summary)) as.character(summary$raw_avg_label[[1]]) else "Raw Avg",
      delta_label = if ("delta_label" %in% names(summary)) as.character(summary$delta_label[[1]]) else "vs Prior",
      thresholds = c(35, 50, 65, 90),
      fundamentals = to_df(model_data$fundamentals, default_shape = "circle"),
      outcomes = to_df(model_data$outcomes, default_shape = "diamond")
    )
  }

  env$model_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "model")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-model-root {{
  --model-slide-bg: {c$model_slide_bg};
  --model-card-bg: {c$model_card_bg};
  --model-card-border: {c$model_card_border};
  --model-card-shadow: {c$model_card_shadow};
  --model-header-accent: {c$model_header_accent};
  --model-title: {c$model_title};
  --model-subtitle: {c$model_subtitle};
  --model-section-title: {c$model_section_title};
  --model-section-rule: {c$model_section_rule};
  --model-metric-header: {c$model_metric_header};
  --model-label: {c$model_label};
  --model-track-bg: {c$model_track_bg};
  --model-stem: {c$model_stem};
  --model-line: {c$model_line};
  --model-line-baseline: {c$model_line_baseline};
  --model-line-label: {c$model_line_label};
  --model-metric-raw: {c$model_metric_raw};
  --model-delta-pos: {c$model_delta_pos};
  --model-delta-neg: {c$model_delta_neg};
  --model-delta-neu: {c$model_delta_neu};
  --model-zone-ni: {c$model_zone_ni};
  --model-zone-is: {c$model_zone_is};
  --model-zone-as: {c$model_zone_as};
  --model-zone-il: {c$model_zone_il};
  --model-point-ni: {c$model_point_ni};
  --model-point-is: {c$model_point_is};
  --model-point-as: {c$model_point_as};
  --model-point-il: {c$model_point_il};
  --model-point-border: {c$model_point_border};
  --model-point-ghost-bg: {c$model_point_ghost_bg};
  --model-point-ghost-border: {c$model_point_ghost_border};
  --model-legend-text-on: {c$model_legend_text_on};
  width: 1280px;
  min-height: 0;
  background: transparent;
  padding: 0;
  border-radius: 0;
  box-shadow: none;
  display: block;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .model-card {{
  width: 100%;
  background: transparent;
  border: none;
  border-radius: 0;
  box-shadow: none;
  padding: 4px 0 0;
}}
{scope} .model-header {{ margin-bottom: 12px; border-left: 4px solid var(--model-header-accent); padding-left: 16px; }}
{scope} .model-title {{ margin: 0 0 6px; font-size: 26px; font-weight: 800; color: var(--model-title); }}
{scope} .model-subtitle {{ margin: 0; font-size: 14px; color: var(--model-subtitle); }}
{scope} .model-block {{ position: relative; margin-top: 32px; margin-bottom: 28px; padding-top: 24px; }}
{scope} .model-block:last-of-type {{ margin-bottom: 10px; }}
{scope} .model-section-head {{ display: flex; align-items: flex-end; border-bottom: 2px solid var(--model-section-rule); padding-bottom: 10px; margin-bottom: 0; position: absolute; top: -32px; left: 0; right: 0; }}
{scope} .model-section-title {{ width: 80%; font-size: 15px; font-weight: 800; color: var(--model-section-title); text-transform: uppercase; letter-spacing: 1px; }}
{scope} .model-metric-head {{ width: 20%; display: flex; justify-content: flex-end; gap: 24px; padding-right: 14px; color: var(--model-metric-header); font-size: 10px; font-weight: 800; text-transform: uppercase; letter-spacing: .5px; }}
{scope} .model-metric-head span {{ width: 54px; text-align: right; }}
{scope} .model-zones, {scope} .model-lines {{ position: absolute; left: 25%; width: 55%; }}
{scope} .model-zones {{ top: 24px; bottom: 0; display: grid; grid-template-columns: 35fr 30fr 25fr 10fr; border-radius: 12px; overflow: hidden; z-index: 0; }}
{scope} .model-zone {{ opacity: 1; }}
{scope} .model-zone-ni {{ background: var(--model-zone-ni); }}
{scope} .model-zone-is {{ background: var(--model-zone-is); }}
{scope} .model-zone-as {{ background: var(--model-zone-as); }}
{scope} .model-zone-il {{ background: var(--model-zone-il); }}
{scope} .model-lines {{ top: 0; bottom: 0; z-index: 1; }}
{scope} .model-line {{ position: absolute; top: 0; bottom: 0; width: 2px; transform: translateX(-50%); background: var(--model-line); opacity: .35; }}
{scope} .model-line-baseline {{ background: var(--model-line-baseline); opacity: 1; z-index: 3; }}
{scope} .model-line-label {{ position: absolute; top: 0; transform: translateX(-50%); background: var(--model-card-bg); padding: 0 4px; color: var(--model-line-label); font-size: 11px; font-weight: 800; }}
{scope} .model-line-baseline .model-line-label {{ color: var(--model-line-baseline); }}
{scope} .model-row-wrap {{ position: relative; z-index: 2; padding-top: 14px; padding-bottom: 12px; }}
{scope} .model-row {{ display: flex; align-items: center; margin-bottom: 12px; }}
{scope} .model-row:last-child {{ margin-bottom: 0; }}
{scope} .model-label {{ width: 25%; text-align: right; padding-right: 30px; color: var(--model-label); font-size: 14px; font-weight: 600; }}
{scope} .model-track-wrap {{ width: 55%; position: relative; height: 24px; }}
{scope} .model-track {{ position: absolute; top: 50%; transform: translateY(-50%); width: 100%; height: 4px; border-radius: 2px; background: var(--model-track-bg); }}
{scope} .model-stem {{ position: absolute; top: 50%; transform: translateY(-50%); height: 4px; border-radius: 2px; background: var(--model-stem); }}
{scope} .model-point {{ position: absolute; top: 50%; transform: translate(-50%, -50%); z-index: 4; box-shadow: 0 2px 4px rgba(0,0,0,0.15); }}
{scope} .model-point-circle {{ width: 16px; height: 16px; border-radius: 50%; border: 2px solid var(--model-point-border); }}
{scope} .model-point-diamond {{ width: 14px; height: 14px; transform: translate(-50%, -50%) rotate(45deg); border: 2px solid var(--model-point-border); }}
{scope} .model-point-ghost-circle {{ width: 12px; height: 12px; border-radius: 50%; background: var(--model-point-ghost-bg); border: 2px solid var(--model-point-ghost-border); box-shadow: none; z-index: 3; }}
{scope} .model-point-ghost-diamond {{ width: 10px; height: 10px; transform: translate(-50%, -50%) rotate(45deg); background: var(--model-point-ghost-bg); border: 2px solid var(--model-point-ghost-border); box-shadow: none; z-index: 3; }}
{scope} .model-zone-bg-ni {{ background: var(--model-point-ni); }}
{scope} .model-zone-bg-is {{ background: var(--model-point-is); }}
{scope} .model-zone-bg-as {{ background: var(--model-point-as); }}
{scope} .model-zone-bg-il {{ background: var(--model-point-il); }}
{scope} .model-metrics {{ width: 20%; display: flex; justify-content: flex-end; align-items: center; gap: 24px; padding-right: 14px; }}
{scope} .model-metric-raw {{ width: 54px; text-align: right; color: var(--model-metric-raw); font-size: 14px; font-weight: 800; font-variant-numeric: tabular-nums; }}
{scope} .model-metric-delta {{ width: 54px; text-align: right; font-size: 13px; font-weight: 700; font-variant-numeric: tabular-nums; }}
{scope} .model-delta-pos {{ color: var(--model-delta-pos); }}
{scope} .model-delta-neg {{ color: var(--model-delta-neg); }}
{scope} .model-delta-neu {{ color: var(--model-delta-neu); }}
{scope} .model-legend {{ display: grid; grid-template-columns: 35fr 30fr 25fr 10fr; gap: 4px; margin-left: 25%; width: 55%; margin-top: 12px; }}
{scope} .model-legend-card {{
  border-radius: 6px;
  min-height: 38px;
  padding: 8px 6px;
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  color: var(--model-legend-text-on);
  text-transform: uppercase;
  letter-spacing: .45px;
  line-height: 1.15;
  font-size: 11px;
  font-weight: 900;
  text-shadow: 0 1px 0 rgba(255,255,255,0.18);
}}
{scope} .model-legend-ni {{ background: var(--model-zone-ni); color: #1F2937; text-shadow: none; }}
{scope} .model-legend-is {{ background: var(--model-zone-is); color: #1F2937; }}
{scope} .model-legend-as {{ background: var(--model-zone-as); color: #FFFFFF; }}
{scope} .model-legend-il {{ background: var(--model-zone-il); color: #FFFFFF; }}
"
    )
  }

  env$render_model_page <- function(
    model_data,
    id = "ohep-model-page",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "model"
    )
    dat <- env$normalize_model_data(model_data)

    classify_zone <- function(percentile) {
      if (percentile < 35) "ni" else if (percentile < 65) "is" else if (percentile < 90) "as" else "il"
    }
    pct_pos <- function(x) sprintf("%.1f%%", max(0, min(100, as.numeric(x))))
    delta_text <- function(val) {
      if (!is.finite(val)) return("-")
      if (val == 0) return("0.00")
      sprintf("%+.2f", val)
    }
    delta_class <- function(val) {
      if (!is.finite(val) || val == 0) "model-delta-neu" else if (val > 0) "model-delta-pos" else "model-delta-neg"
    }

    render_rows <- function(df, is_outcomes = FALSE) {
      vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        zone <- classify_zone(row$percentile[[1]])
        prior <- suppressWarnings(as.numeric(row$prior_percentile[[1]]))
        has_prior <- is.finite(prior) && prior >= 0 && prior <= 100
        current <- suppressWarnings(as.numeric(row$percentile[[1]]))
        stem_left <- if (has_prior) min(prior, current) else NA_real_
        stem_width <- if (has_prior) abs(current - prior) else NA_real_
        shape <- tolower(trimws(as.character(row$shape[[1]])))
        is_diamond <- identical(shape, "diamond") || is_outcomes
        point_class <- if (is_diamond) "model-point-diamond" else "model-point-circle"
        ghost_class <- if (is_diamond) "model-point-ghost-diamond" else "model-point-ghost-circle"
        stem_html <- if (has_prior && stem_width > 0) {
          glue::glue("<div class=\"model-stem\" style=\"left:{pct_pos(stem_left)}; width:{pct_pos(stem_width)};\"></div>")
        } else ""
        ghost_html <- if (has_prior) {
          glue::glue("<div class=\"model-point {ghost_class}\" style=\"left:{pct_pos(prior)};\"></div>")
        } else ""
        raw_txt <- if (is.finite(row$raw_avg[[1]])) sprintf("%.2f", row$raw_avg[[1]]) else "-"
        d_txt <- delta_text(row$delta[[1]])
        d_class <- delta_class(row$delta[[1]])

        glue::glue(
          "<div class=\"model-row\">
            <div class=\"model-label\">{env$escape_text(row$label[[1]])}</div>
            <div class=\"model-track-wrap\">
              <div class=\"model-track\"></div>
              {stem_html}
              {ghost_html}
              <div class=\"model-point {point_class} model-zone-bg-{zone}\" style=\"left:{pct_pos(current)};\"></div>
            </div>
            <div class=\"model-metrics\">
              <div class=\"model-metric-raw\">{raw_txt}</div>
              <div class=\"model-metric-delta {d_class}\">{d_txt}</div>
            </div>
          </div>"
        )
      }, character(1))
    }

    render_block <- function(section_title, rows_html, show_metrics = TRUE) {
      section_title_html <- if (nzchar(trimws(section_title))) env$escape_text(section_title) else "&nbsp;"
      metric_head <- if (show_metrics) {
        glue::glue("<div class=\"model-metric-head\"><span>{env$escape_text(dat$raw_avg_label)}</span><span>{env$escape_text(dat$delta_label)}</span></div>")
      } else {
        "<div class=\"model-metric-head\"><span></span><span></span></div>"
      }
      glue::glue(
        "<div class=\"model-block\">
          <div class=\"model-section-head\">
            <div class=\"model-section-title\">{section_title_html}</div>
            {metric_head}
          </div>
          <div class=\"model-zones\">
            <div class=\"model-zone model-zone-ni\"></div>
            <div class=\"model-zone model-zone-is\"></div>
            <div class=\"model-zone model-zone-as\"></div>
            <div class=\"model-zone model-zone-il\"></div>
          </div>
          <div class=\"model-lines\">
            <div class=\"model-line\" style=\"left:35%;\"><div class=\"model-line-label\">35th</div></div>
            <div class=\"model-line model-line-baseline\" style=\"left:50%;\"><div class=\"model-line-label\">50th</div></div>
            <div class=\"model-line\" style=\"left:65%;\"><div class=\"model-line-label\">65th</div></div>
            <div class=\"model-line\" style=\"left:90%;\"><div class=\"model-line-label\">90th</div></div>
          </div>
          <div class=\"model-row-wrap\">
            {glue::glue_collapse(rows_html, sep = '')}
          </div>
        </div>"
      )
    }

    f_rows <- render_rows(dat$fundamentals, is_outcomes = FALSE)
    o_rows <- render_rows(dat$outcomes, is_outcomes = TRUE)

    model_header_html <- if (nzchar(trimws(dat$title)) || nzchar(trimws(dat$subtitle))) {
      glue::glue(
        "<div class=\"model-header\">
          {if (nzchar(trimws(dat$title))) glue::glue('<h1 class=\"model-title\">{env$escape_text(dat$title)}</h1>') else ''}
          {if (nzchar(trimws(dat$subtitle))) glue::glue('<p class=\"model-subtitle\">{env$escape_text(dat$subtitle)}</p>') else ''}
        </div>"
      )
    } else {
      ""
    }

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-model-root\">
        <div class=\"model-card\">
          {model_header_html}
          {render_block(dat$fundamentals_label, f_rows, show_metrics = TRUE)}
          {render_block(dat$outcomes_label, o_rows, show_metrics = FALSE)}
          <div class=\"model-legend\">
            <div class=\"model-legend-card model-legend-ni\">Needs Improvement</div>
            <div class=\"model-legend-card model-legend-is\">Industry Standard</div>
            <div class=\"model-legend-card model-legend-as\">Above Standard</div>
            <div class=\"model-legend-card model-legend-il\">Industry Leader</div>
          </div>
        </div>
      </div>"
    )

    css <- env$model_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_demo_bipolar_split <- function(data) {
    if (!is.data.frame(data) || nrow(data) != 2L) {
      stop("`demo_bipolar_split$data` must be a data frame with exactly 2 rows.", call. = FALSE)
    }
    req <- c("label", "count", "pct")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_bipolar_split$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    pct <- suppressWarnings(as.numeric(data$pct))
    cnt <- suppressWarnings(as.numeric(data$count))
    if (any(!is.finite(pct)) || any(pct < 0)) stop("`pct` must be non-negative numeric values.", call. = FALSE)
    if (any(!is.finite(cnt)) || any(cnt < 0)) stop("`count` must be non-negative numeric values.", call. = FALSE)
    if (abs(sum(pct) - 100) > 1) stop("`pct` values must sum to 100 (+/- 1 for rounding).", call. = FALSE)
    if (any(trimws(as.character(data$label)) == "")) stop("`label` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$validate_demo_categorical_bar <- function(data) {
    if (!is.data.frame(data) || nrow(data) < 1L) {
      stop("`demo_categorical_bar$data` must be a non-empty data frame.", call. = FALSE)
    }
    req <- c("category", "value")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_categorical_bar$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    val <- suppressWarnings(as.numeric(data$value))
    if (any(!is.finite(val)) || any(val < 0)) stop("`value` must be non-negative numeric values.", call. = FALSE)
    if (any(trimws(as.character(data$category)) == "")) stop("`category` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$validate_demo_categorical_tree <- function(data) {
    if (!is.data.frame(data) || nrow(data) < 1L) {
      stop("`demo_categorical_tree$data` must be a non-empty data frame.", call. = FALSE)
    }
    req <- c("category", "value")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_categorical_tree$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    val <- suppressWarnings(as.numeric(data$value))
    if (any(!is.finite(val)) || any(val < 0)) stop("`value` must be non-negative numeric values.", call. = FALSE)
    if (any(trimws(as.character(data$category)) == "")) stop("`category` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$validate_demo_ordinal_bar <- function(data) {
    if (!is.data.frame(data) || nrow(data) < 1L) {
      stop("`demo_ordinal_bar$data` must be a non-empty data frame.", call. = FALSE)
    }
    req <- c("label", "pct")
    miss <- setdiff(req, names(data))
    if (length(miss) > 0L) {
      stop(sprintf("`demo_ordinal_bar$data` missing required columns: %s.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    pct <- suppressWarnings(as.numeric(data$pct))
    if (any(!is.finite(pct)) || any(pct < 0)) stop("`pct` must be non-negative numeric values.", call. = FALSE)
    if (abs(sum(pct) - 100) > 1) stop("`pct` values must sum to 100 (+/- 1 for rounding).", call. = FALSE)
    if (any(trimws(as.character(data$label)) == "")) stop("`label` values must be non-empty.", call. = FALSE)
    invisible(data)
  }

  env$normalize_demo_bipolar_split <- function(panel) {
    env$validate_demo_bipolar_split(panel$data)
    d <- panel$data
    d$label <- as.character(d$label)
    d$count <- suppressWarnings(as.numeric(d$count))
    d$pct <- suppressWarnings(as.numeric(d$pct))
    list(
      type = "bipolar_split",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      data = d
    )
  }

  env$normalize_demo_categorical_bar <- function(panel) {
    env$validate_demo_categorical_bar(panel$data)
    d <- panel$data
    d$category <- as.character(d$category)
    d$value <- suppressWarnings(as.numeric(d$value))
    d$color_key <- if ("color_key" %in% names(d)) as.character(d$color_key) else NA_character_
    sort_desc <- !isFALSE(panel$sort_desc)
    if (sort_desc) {
      d <- d[order(-d$value, d$category), , drop = FALSE]
    }
    max_val <- max(d$value, na.rm = TRUE)
    d$width_pct <- if (is.finite(max_val) && max_val > 0) (d$value / max_val) * 100 else rep(0, nrow(d))
    palette_map <- if (all(is.na(d$color_key) | trimws(d$color_key) == "")) {
      seq_len(nrow(d))
    } else {
      as.integer(as.factor(ifelse(is.na(d$color_key) | trimws(d$color_key) == "", d$category, d$color_key)))
    }
    d$palette_ix <- ((palette_map - 1L) %% 6L) + 1L
    list(
      type = "categorical_bar",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      data = d
    )
  }

  env$normalize_demo_categorical_tree <- function(panel) {
    env$validate_demo_categorical_tree(panel$data)
    d <- panel$data
    d$category <- as.character(d$category)
    d$value <- suppressWarnings(as.numeric(d$value))
    d$short_label <- if ("short_label" %in% names(d)) as.character(d$short_label) else d$category
    d$color_key <- if ("color_key" %in% names(d)) as.character(d$color_key) else NA_character_
    d <- d[order(-d$value, d$category), , drop = FALSE]
    total <- sum(d$value, na.rm = TRUE)
    d$share <- if (is.finite(total) && total > 0) d$value / total else rep(1 / nrow(d), nrow(d))
    palette_map <- if (all(is.na(d$color_key) | trimws(d$color_key) == "")) {
      seq_len(nrow(d))
    } else {
      as.integer(as.factor(ifelse(is.na(d$color_key) | trimws(d$color_key) == "", d$category, d$color_key)))
    }
    d$palette_ix <- ((palette_map - 1L) %% 6L) + 1L
    list(
      type = "categorical_tree",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      data = d
    )
  }

  env$normalize_demo_ordinal_bar <- function(panel) {
    env$validate_demo_ordinal_bar(panel$data)
    d <- panel$data
    d$label <- as.character(d$label)
    d$pct <- suppressWarnings(as.numeric(d$pct))
    d$seg_ix <- ((seq_len(nrow(d)) - 1L) %% 5L) + 1L
    callout_index <- suppressWarnings(as.integer(panel$callout_index))
    if (length(callout_index) != 1L || !is.finite(callout_index) || callout_index < 1L || callout_index > nrow(d)) {
      callout_index <- which.min(d$pct)[[1]]
    }
    callout_value <- panel$callout_value
    if (is.null(callout_value) || !nzchar(trimws(as.character(callout_value)))) {
      callout_value <- paste0(round(d$pct[[callout_index]]), "%")
    }
    list(
      type = "ordinal_bar",
      title = if (is.null(panel$title)) "" else as.character(panel$title[[1]]),
      subtitle = if (is.null(panel$subtitle)) "" else as.character(panel$subtitle[[1]]),
      callout_index = callout_index,
      callout_value = as.character(callout_value),
      data = d
    )
  }

  env$normalize_demographics_panel <- function(panel) {
    if (!is.list(panel) || is.null(panel$type) || !nzchar(as.character(panel$type))) {
      stop("Each demographics slot must be built with demo_* constructors.", call. = FALSE)
    }
    panel$type <- as.character(panel$type[[1]])
    switch(
      panel$type,
      bipolar_split = env$normalize_demo_bipolar_split(panel),
      categorical_bar = env$normalize_demo_categorical_bar(panel),
      categorical_tree = env$normalize_demo_categorical_tree(panel),
      ordinal_bar = env$normalize_demo_ordinal_bar(panel),
      stop(sprintf("Unsupported demographics panel type: %s", panel$type), call. = FALSE)
    )
  }

  env$demographics_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "demographics")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-demographics-root {{
  --demo-page-bg: {c$demo_page_bg};
  --demo-grid-gap: {c$demo_grid_gap};
  --demo-panel-bg: {c$demo_panel_bg};
  --demo-panel-border: {c$demo_panel_border};
  --demo-panel-shadow: {c$demo_panel_shadow};
  --demo-title: {c$demo_title};
  --demo-subtitle: {c$demo_subtitle};
  --demo-panel-title: {c$demo_panel_title};
  --demo-panel-subtitle: {c$demo_panel_subtitle};
  --demo-empty-text: {c$demo_empty_text};
  --demo-bi-axis-mid: {c$demo_bi_axis_mid};
  --demo-bi-axis-label: {c$demo_bi_axis_label};
  --demo-bi-side-label-left: {c$demo_bi_side_label_left};
  --demo-bi-side-label-right: {c$demo_bi_side_label_right};
  --demo-bi-side-count: {c$demo_bi_side_count};
  --demo-bi-bar-left-bg: {c$demo_bi_bar_left_bg};
  --demo-bi-bar-right-bg: {c$demo_bi_bar_right_bg};
  --demo-bi-bar-text: {c$demo_bi_bar_text};
  --demo-cb-label: {c$demo_cb_label};
  --demo-cb-track-bg: {c$demo_cb_track_bg};
  --demo-cb-value-text: {c$demo_cb_value_text};
  --demo-cb-bar-1: {c$demo_cb_bar_1};
  --demo-cb-bar-2: {c$demo_cb_bar_2};
  --demo-cb-bar-3: {c$demo_cb_bar_3};
  --demo-cb-bar-4: {c$demo_cb_bar_4};
  --demo-cb-bar-5: {c$demo_cb_bar_5};
  --demo-cb-bar-6: {c$demo_cb_bar_6};
  --demo-ct-text-on: {c$demo_ct_text_on};
  --demo-ct-border: {c$demo_ct_border};
  --demo-ct-tile-1: {c$demo_ct_tile_1};
  --demo-ct-tile-2: {c$demo_ct_tile_2};
  --demo-ct-tile-3: {c$demo_ct_tile_3};
  --demo-ct-tile-4: {c$demo_ct_tile_4};
  --demo-ct-tile-5: {c$demo_ct_tile_5};
  --demo-ct-tile-6: {c$demo_ct_tile_6};
  --demo-ob-axis-label: {c$demo_ob_axis_label};
  --demo-ob-callout-bg: {c$demo_ob_callout_bg};
  --demo-ob-callout-text: {c$demo_ob_callout_text};
  --demo-ob-callout-line: {c$demo_ob_callout_line};
  --demo-ob-seg-1: {c$demo_ob_seg_1};
  --demo-ob-seg-2: {c$demo_ob_seg_2};
  --demo-ob-seg-3: {c$demo_ob_seg_3};
  --demo-ob-seg-4: {c$demo_ob_seg_4};
  --demo-ob-seg-5: {c$demo_ob_seg_5};
  --demo-ob-seg-text-dark: {c$demo_ob_seg_text_dark};
  --demo-ob-seg-text-light: {c$demo_ob_seg_text_light};
  width: 1280px;
  min-height: 0;
  background: transparent;
  padding: 0;
  border-radius: 0;
  box-shadow: none;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .demo-header {{ margin-bottom: 14px; }}
{scope} .demo-title {{ margin: 0; font-size: 28px; font-weight: 900; color: var(--demo-title); letter-spacing: -0.4px; }}
{scope} .demo-subtitle {{ margin: 6px 0 0; font-size: 13px; color: var(--demo-subtitle); }}
{scope} .demo-grid {{ display: grid; grid-template-columns: 1fr 1fr; grid-template-rows: 1fr 1fr; gap: 2px; background: var(--demo-grid-gap); border-radius: 12px; padding: 2px; }}
{scope} .demo-panel {{ background: var(--demo-panel-bg); border: 1px solid var(--demo-panel-border); border-radius: 12px; box-shadow: 0 4px 16px var(--demo-panel-shadow); padding: 18px; min-height: 310px; }}
{scope} .demo-panel-head {{ margin-bottom: 14px; }}
{scope} .demo-panel-title {{ margin: 0; font-size: 15px; text-transform: uppercase; letter-spacing: .5px; font-weight: 800; color: var(--demo-panel-title); }}
{scope} .demo-panel-subtitle {{ margin: 4px 0 0; font-size: 12px; color: var(--demo-panel-subtitle); }}
{scope} .demo-empty {{ display: flex; height: 100%; align-items: center; justify-content: center; border: 1px dashed var(--demo-panel-border); border-radius: 8px; color: var(--demo-empty-text); font-weight: 700; }}
{scope} .demo-bi-area {{ display: grid; grid-template-columns: 92px 1fr 92px; gap: 10px; align-items: center; margin-top: 10px; }}
{scope} .demo-bi-side {{ display: flex; flex-direction: column; }}
{scope} .demo-bi-side.left {{ text-align: right; align-items: flex-end; }}
{scope} .demo-bi-side.right {{ text-align: left; align-items: flex-start; }}
{scope} .demo-bi-lbl {{ font-size: 12px; font-weight: 800; text-transform: uppercase; letter-spacing: .4px; }}
{scope} .demo-bi-lbl.left {{ color: var(--demo-bi-side-label-left); }}
{scope} .demo-bi-lbl.right {{ color: var(--demo-bi-side-label-right); }}
{scope} .demo-bi-cnt {{ margin-top: 3px; color: var(--demo-bi-side-count); font-size: 11px; font-weight: 600; }}
{scope} .demo-bi-axis {{ height: 54px; position: relative; }}
{scope} .demo-bi-mid {{ position: absolute; left: 50%; top: -4px; bottom: -4px; width: 2px; transform: translateX(-50%); background: var(--demo-bi-axis-mid); }}
{scope} .demo-bi-mid-lbl {{ position: absolute; left: 50%; top: -20px; transform: translateX(-50%); font-size: 10px; color: var(--demo-bi-axis-label); font-weight: 800; text-transform: uppercase; }}
{scope} .demo-bi-left {{ position: absolute; right: 50%; top: 8px; height: 38px; border-radius: 8px 0 0 8px; background: var(--demo-bi-bar-left-bg); display: flex; align-items: center; justify-content: flex-end; padding-right: 12px; }}
{scope} .demo-bi-right {{ position: absolute; left: 50%; top: 8px; height: 38px; border-radius: 0 8px 8px 0; background: var(--demo-bi-bar-right-bg); display: flex; align-items: center; justify-content: flex-start; padding-left: 12px; }}
{scope} .demo-bi-val {{ color: var(--demo-bi-bar-text); font-size: 14px; font-weight: 800; }}
{scope} .demo-cb-row {{ display: grid; grid-template-columns: 124px 1fr; gap: 10px; align-items: center; margin-bottom: 10px; }}
{scope} .demo-cb-label {{ text-align: right; padding-right: 6px; color: var(--demo-cb-label); font-size: 12px; font-weight: 700; }}
{scope} .demo-cb-track {{ height: 28px; border-radius: 4px; background: var(--demo-cb-track-bg); overflow: hidden; }}
{scope} .demo-cb-fill {{ height: 100%; display: flex; align-items: center; justify-content: flex-end; padding-right: 10px; color: var(--demo-cb-value-text); font-size: 12px; font-weight: 800; }}
{scope} .demo-cb-fill.p1 {{ background: var(--demo-cb-bar-1); }}
{scope} .demo-cb-fill.p2 {{ background: var(--demo-cb-bar-2); }}
{scope} .demo-cb-fill.p3 {{ background: var(--demo-cb-bar-3); }}
{scope} .demo-cb-fill.p4 {{ background: var(--demo-cb-bar-4); }}
{scope} .demo-cb-fill.p5 {{ background: var(--demo-cb-bar-5); }}
{scope} .demo-cb-fill.p6 {{ background: var(--demo-cb-bar-6); }}
{scope} .demo-ct-wrap {{ display: flex; gap: 4px; height: 220px; }}
{scope} .demo-ct-col {{ display: flex; flex-direction: column; gap: 4px; }}
{scope} .demo-ct-box {{ border-radius: 4px; border: 1px solid var(--demo-ct-border); color: var(--demo-ct-text-on); padding: 10px; display: flex; flex-direction: column; justify-content: flex-end; min-height: 32px; }}
{scope} .demo-ct-num {{ font-size: 18px; font-weight: 900; line-height: 1; }}
{scope} .demo-ct-lbl {{ margin-top: 4px; font-size: 10px; text-transform: uppercase; font-weight: 700; line-height: 1.1; }}
{scope} .demo-ct-box.p1 {{ background: var(--demo-ct-tile-1); }}
{scope} .demo-ct-box.p2 {{ background: var(--demo-ct-tile-2); }}
{scope} .demo-ct-box.p3 {{ background: var(--demo-ct-tile-3); }}
{scope} .demo-ct-box.p4 {{ background: var(--demo-ct-tile-4); }}
{scope} .demo-ct-box.p5 {{ background: var(--demo-ct-tile-5); }}
{scope} .demo-ct-box.p6 {{ background: var(--demo-ct-tile-6); }}
{scope} .demo-ob-area {{ position: relative; margin-top: 12px; }}
{scope} .demo-ob-callout {{ position: absolute; top: -34px; transform: translateX(-50%); display: flex; flex-direction: column; align-items: center; }}
{scope} .demo-ob-callout-val {{ background: var(--demo-ob-callout-bg); border: 1px solid var(--demo-panel-border); border-radius: 4px; color: var(--demo-ob-callout-text); font-weight: 800; font-size: 11px; padding: 2px 6px; }}
{scope} .demo-ob-callout-line {{ width: 2px; height: 12px; background: var(--demo-ob-callout-line); }}
{scope} .demo-ob-bar {{ display: flex; height: 32px; gap: 4px; }}
{scope} .demo-ob-seg {{ height: 100%; border-radius: 4px; display: flex; align-items: center; justify-content: center; font-size: 11px; font-weight: 800; }}
{scope} .demo-ob-seg.s1 {{ background: var(--demo-ob-seg-1); color: var(--demo-ob-seg-text-dark); }}
{scope} .demo-ob-seg.s2 {{ background: var(--demo-ob-seg-2); color: var(--demo-ob-seg-text-dark); }}
{scope} .demo-ob-seg.s3 {{ background: var(--demo-ob-seg-3); color: var(--demo-ob-seg-text-light); }}
{scope} .demo-ob-seg.s4 {{ background: var(--demo-ob-seg-4); color: var(--demo-ob-seg-text-light); }}
{scope} .demo-ob-seg.s5 {{ background: var(--demo-ob-seg-5); color: var(--demo-ob-seg-text-light); }}
{scope} .demo-ob-axis {{ display: flex; gap: 4px; margin-top: 8px; }}
{scope} .demo-ob-axis-lbl {{ font-size: 10px; color: var(--demo-ob-axis-label); font-weight: 700; text-align: center; line-height: 1.1; }}
"
    )
  }

  env$render_demographics_page <- function(
    tl = NULL,
    tr = NULL,
    bl = NULL,
    br = NULL,
    id = "ohep-demographics-page",
    title = "Demographics",
    subtitle = "",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "demographics"
    )

    normalize_slot <- function(panel) {
      if (is.null(panel)) return(NULL)
      env$normalize_demographics_panel(panel)
    }
    slots <- list(
      tl = normalize_slot(tl),
      tr = normalize_slot(tr),
      bl = normalize_slot(bl),
      br = normalize_slot(br)
    )

    panel_html <- function(panel) {
      if (is.null(panel)) {
        return("<div class=\"demo-panel\"><div class=\"demo-empty\">No panel</div></div>")
      }
      head_html <- glue::glue(
        "<div class=\"demo-panel-head\">
          <h3 class=\"demo-panel-title\">{env$escape_text(panel$title)}</h3>
          {if (nzchar(trimws(panel$subtitle))) glue::glue('<p class=\"demo-panel-subtitle\">{env$escape_text(panel$subtitle)}</p>') else ''}
        </div>"
      )
      body_html <- switch(
        panel$type,
        bipolar_split = {
          d <- panel$data
          glue::glue(
            "<div class=\"demo-bi-area\">
              <div class=\"demo-bi-side left\">
                <span class=\"demo-bi-lbl left\">{env$escape_text(d$label[[1]])}</span>
                <span class=\"demo-bi-cnt\">{sprintf('%.0f', d$count[[1]])}</span>
              </div>
              <div class=\"demo-bi-axis\">
                <div class=\"demo-bi-mid\"><span class=\"demo-bi-mid-lbl\">Center</span></div>
                <div class=\"demo-bi-left\" style=\"width:{sprintf('%.1f', d$pct[[1]] / 2)}%;\"><span class=\"demo-bi-val\">{sprintf('%.0f%%', d$pct[[1]])}</span></div>
                <div class=\"demo-bi-right\" style=\"width:{sprintf('%.1f', d$pct[[2]] / 2)}%;\"><span class=\"demo-bi-val\">{sprintf('%.0f%%', d$pct[[2]])}</span></div>
              </div>
              <div class=\"demo-bi-side right\">
                <span class=\"demo-bi-lbl right\">{env$escape_text(d$label[[2]])}</span>
                <span class=\"demo-bi-cnt\">{sprintf('%.0f', d$count[[2]])}</span>
              </div>
            </div>"
          )
        },
        categorical_bar = {
          d <- panel$data
          rows <- vapply(seq_len(nrow(d)), function(i) {
            glue::glue(
              "<div class=\"demo-cb-row\">
                <div class=\"demo-cb-label\">{env$escape_text(d$category[[i]])}</div>
                <div class=\"demo-cb-track\"><div class=\"demo-cb-fill p{d$palette_ix[[i]]}\" style=\"width:{sprintf('%.1f', d$width_pct[[i]])}%;\">{sprintf('%.0f', d$value[[i]])}</div></div>
              </div>"
            )
          }, character(1))
          glue::glue_collapse(rows, sep = "")
        },
        categorical_tree = {
          d <- panel$data
          n <- nrow(d)
          if (n == 1L) {
            cols <- list(d[1, , drop = FALSE])
          } else if (n == 2L) {
            cols <- list(d[1, , drop = FALSE], d[2, , drop = FALSE])
          } else {
            col1 <- d[1, , drop = FALSE]
            rest <- d[-1, , drop = FALSE]
            a <- cumsum(rest$share)
            split_ix <- which(a >= max(a) / 2)[1]
            if (!is.finite(split_ix) || split_ix < 1) split_ix <- ceiling(nrow(rest) / 2)
            col2 <- rest[seq_len(split_ix), , drop = FALSE]
            col3 <- if (split_ix < nrow(rest)) rest[(split_ix + 1):nrow(rest), , drop = FALSE] else rest[0, , drop = FALSE]
            if (nrow(col3) > 0) {
              col3 <- col3[nrow(col3):1, , drop = FALSE]
            }
            cols <- list(col1, col2, col3)
          }
          col_weights <- vapply(cols, function(df) sum(df$share, na.rm = TRUE), numeric(1))
          col_weights <- if (sum(col_weights) > 0) (col_weights / sum(col_weights)) * 100 else rep(100 / length(cols), length(cols))
          cols_html <- vapply(seq_along(cols), function(ci) {
            col <- cols[[ci]]
            hweights <- if (sum(col$share) > 0) (col$share / sum(col$share)) * 100 else rep(100 / nrow(col), nrow(col))
            boxes <- vapply(seq_len(nrow(col)), function(i) {
              glue::glue(
                "<div class=\"demo-ct-box p{col$palette_ix[[i]]}\" style=\"height:{sprintf('%.1f', hweights[[i]])}%;\">
                  <span class=\"demo-ct-num\">{sprintf('%.0f', col$value[[i]])}</span>
                  <span class=\"demo-ct-lbl\">{env$escape_text(col$short_label[[i]])}</span>
                </div>"
              )
            }, character(1))
            glue::glue("<div class=\"demo-ct-col\" style=\"width:{sprintf('%.1f', col_weights[[ci]])}%;\">{glue::glue_collapse(boxes, sep='')}</div>")
          }, character(1))
          glue::glue("<div class=\"demo-ct-wrap\">{glue::glue_collapse(cols_html, sep='')}</div>")
        },
        ordinal_bar = {
          d <- panel$data
          callout_pos <- sum(d$pct[seq_len(max(1L, panel$callout_index - 1L))], na.rm = TRUE)
          callout_pos <- callout_pos + (d$pct[[panel$callout_index]] / 2)
          segs <- vapply(seq_len(nrow(d)), function(i) {
            txt <- if (d$pct[[i]] >= 11) sprintf("%.0f%%", d$pct[[i]]) else ""
            glue::glue("<div class=\"demo-ob-seg s{d$seg_ix[[i]]}\" style=\"width:{sprintf('%.1f', d$pct[[i]])}%;\">{txt}</div>")
          }, character(1))
          axis <- vapply(seq_len(nrow(d)), function(i) {
            glue::glue("<div class=\"demo-ob-axis-lbl\" style=\"width:{sprintf('%.1f', d$pct[[i]])}%;\">{env$escape_text(d$label[[i]])}</div>")
          }, character(1))
          glue::glue(
            "<div class=\"demo-ob-area\">
              <div class=\"demo-ob-callout\" style=\"left:{sprintf('%.1f', callout_pos)}%;\">
                <div class=\"demo-ob-callout-val\">{env$escape_text(panel$callout_value)}</div>
                <div class=\"demo-ob-callout-line\"></div>
              </div>
              <div class=\"demo-ob-bar\">{glue::glue_collapse(segs, sep='')}</div>
              <div class=\"demo-ob-axis\">{glue::glue_collapse(axis, sep='')}</div>
            </div>"
          )
        },
        "<div class=\"demo-empty\">Unsupported panel</div>"
      )
      glue::glue("<div class=\"demo-panel\">{head_html}{body_html}</div>")
    }

    demo_header_html <- if (nzchar(trimws(title)) || nzchar(trimws(subtitle))) {
      glue::glue(
        "<div class=\"demo-header\">
          {if (nzchar(trimws(title))) glue::glue('<h1 class=\"demo-title\">{env$escape_text(title)}</h1>') else ''}
          {if (nzchar(trimws(subtitle))) glue::glue('<p class=\"demo-subtitle\">{env$escape_text(subtitle)}</p>') else ''}
        </div>"
      )
    } else {
      ""
    }

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-demographics-root\">
        {demo_header_html}
        <div class=\"demo-grid\">
          {panel_html(slots$tl)}
          {panel_html(slots$tr)}
          {panel_html(slots$bl)}
          {panel_html(slots$br)}
        </div>
      </div>"
    )

    css <- env$demographics_page_css(id = id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$validate_decision_matrix_data <- function(points) {
    if (!is.data.frame(points) || nrow(points) < 1L) {
      stop("`points` must be a non-empty data frame.", call. = FALSE)
    }
    required <- c("fundamental", "score", "impact")
    missing <- setdiff(required, names(points))
    if (length(missing) > 0L) {
      stop(sprintf("`points` is missing required columns: %s.", paste(missing, collapse = ", ")), call. = FALSE)
    }
    score <- suppressWarnings(as.numeric(points$score))
    impact <- suppressWarnings(as.numeric(points$impact))
    if (anyNA(score) || anyNA(impact)) {
      stop("`score` and `impact` must be numeric and non-missing.", call. = FALSE)
    }
    points
  }

  env$normalize_decision_matrix_points <- function(points) {
    points <- env$validate_decision_matrix_data(points)
    out <- points
    out$fundamental <- as.character(out$fundamental)
    out$label <- if ("label" %in% names(out)) as.character(out$label) else out$fundamental
    out$score <- suppressWarnings(as.numeric(out$score))
    out$impact <- suppressWarnings(as.numeric(out$impact))

    x_split <- suppressWarnings(as.numeric(attr(points, "x_split_value")))
    y_split <- suppressWarnings(as.numeric(attr(points, "y_split_value")))
    if (length(x_split) != 1L || !is.finite(x_split)) x_split <- 0
    if (length(y_split) != 1L || !is.finite(y_split)) y_split <- stats::median(out$impact, na.rm = TRUE)

    x_scale_mode <- as.character(if (is.null(attr(points, "x_scale_mode"))) "symmetric" else attr(points, "x_scale_mode"))
    y_scale_mode <- as.character(if (is.null(attr(points, "y_scale_mode"))) "range" else attr(points, "y_scale_mode"))

    score_rng <- range(out$score, na.rm = TRUE, finite = TRUE)
    impact_rng <- range(out$impact, na.rm = TRUE, finite = TRUE)
    x_pad <- 8
    y_pad <- 12
    inner_x <- 100 - (2 * x_pad)
    inner_y <- 100 - (2 * y_pad)

    if (identical(x_scale_mode, "symmetric")) {
      max_abs_score <- suppressWarnings(max(abs(out$score - x_split), na.rm = TRUE))
      if (!is.finite(max_abs_score) || max_abs_score == 0) {
        out$x_pct <- rep(50, nrow(out))
      } else {
        out$x_pct <- 50 + ((out$score - x_split) / max_abs_score) * (inner_x / 2)
      }
    } else {
      if (!all(is.finite(score_rng)) || diff(score_rng) == 0) {
        out$x_pct <- rep(50, nrow(out))
      } else {
        out$x_pct <- x_pad + inner_x * (out$score - score_rng[[1]]) / (score_rng[[2]] - score_rng[[1]])
      }
    }

    if (identical(y_scale_mode, "symmetric")) {
      max_abs_impact <- suppressWarnings(max(abs(out$impact - y_split), na.rm = TRUE))
      if (!is.finite(max_abs_impact) || max_abs_impact == 0) {
        out$y_pct <- rep(50, nrow(out))
      } else {
        out$y_pct <- 50 + ((out$impact - y_split) / max_abs_impact) * (inner_y / 2)
      }
    } else {
      if (!all(is.finite(impact_rng)) || diff(impact_rng) == 0) {
        out$y_pct <- rep(50, nrow(out))
      } else {
        out$y_pct <- y_pad + inner_y * (out$impact - impact_rng[[1]]) / (impact_rng[[2]] - impact_rng[[1]])
      }
    }
    out$x_pct <- pmax(x_pad, pmin(100 - x_pad, out$x_pct))
    out$y_pct <- pmax(y_pad, pmin(100 - y_pad, out$y_pct))

    out$quadrant <- ifelse(
      out$score < x_split & out$impact >= y_split, "critical",
      ifelse(
        out$score >= x_split & out$impact >= y_split, "leverage",
        ifelse(out$score < x_split & out$impact < y_split, "monitor", "maintain")
      )
    )

    if ("priority" %in% names(out)) {
      priority <- tolower(trimws(as.character(out$priority)))
      valid <- priority %in% c("critical", "leverage", "monitor", "maintain")
      out$quadrant[valid] <- priority[valid]
    }

    out$dot_class <- ifelse(
      out$quadrant == "critical", "dot-critical",
      ifelse(
        out$quadrant == "leverage", "dot-leverage",
        ifelse(out$quadrant == "maintain", "dot-maintain", "dot-monitor")
      )
    )

    out
  }

  env$decision_matrix_page_css <- function(id, colors = NULL) {
    scope <- paste0("#", id)
    if (is.null(colors)) {
      colors <- env$resolve_brand_colors(graph = "decision_matrix")
    }
    c <- as.list(colors)
    glue::glue(
      "
{scope}, {scope} * {{ box-sizing: border-box; }}
{scope}.ohep-dm-root {{
  --matrix-shell-bg: {c$matrix_shell_bg};
  --matrix-slide-bg: {c$matrix_slide_bg};
  --matrix-header-divider: {c$matrix_header_divider};
  --matrix-theme-kicker: {c$matrix_theme_kicker};
  --matrix-title: {c$matrix_title};
  --matrix-subtitle: {c$matrix_subtitle};
  --matrix-axis-line: {c$matrix_axis_line};
  --matrix-axis-text: {c$matrix_axis_text};
  --matrix-axis-main: {c$matrix_axis_main};
  --matrix-plot-bg: {c$matrix_plot_bg};
  --matrix-quadrant-tl-bg: {c$matrix_quadrant_tl_bg};
  --matrix-quadrant-tl-text: {c$matrix_quadrant_tl_text};
  --matrix-quadrant-tr-bg: {c$matrix_quadrant_tr_bg};
  --matrix-quadrant-tr-text: {c$matrix_quadrant_tr_text};
  --matrix-quadrant-bl-bg: {c$matrix_quadrant_bl_bg};
  --matrix-quadrant-bl-text: {c$matrix_quadrant_bl_text};
  --matrix-quadrant-br-bg: {c$matrix_quadrant_br_bg};
  --matrix-quadrant-br-text: {c$matrix_quadrant_br_text};
  --matrix-quadrant-divider: {c$matrix_quadrant_divider};
  --matrix-dot-critical: {c$matrix_dot_critical};
  --matrix-dot-leverage: {c$matrix_dot_leverage};
  --matrix-dot-monitor: {c$matrix_dot_monitor};
  --matrix-dot-maintain: {c$matrix_dot_maintain};
  --matrix-point-border: {c$matrix_point_border};
  --matrix-point-shadow: {c$matrix_point_shadow};
  --matrix-label-bg: {c$matrix_label_bg};
  --matrix-label-text: {c$matrix_label_text};
  --matrix-label-border: {c$matrix_label_border};
  --matrix-label-shadow: {c$matrix_label_shadow};
  --matrix-brand-text: {c$matrix_brand_text};
  --matrix-brand-dot: {c$matrix_brand_dot};
  width: 1280px;
  min-height: 720px;
  aspect-ratio: 16 / 9;
  background: transparent;
  padding: 8px 0 0;
  border-radius: 0;
  box-shadow: none;
  display: flex;
  flex-direction: column;
  overflow: visible;
  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
}}
{scope} .header-section {{ display: flex; flex-direction: column; border-bottom: 2px solid var(--matrix-header-divider); padding-bottom: 12px; margin-bottom: 20px; flex-shrink: 0; }}
{scope} .theme-kicker {{ font-size: 14px; font-weight: 800; color: var(--matrix-theme-kicker); text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 4px; }}
{scope} .title {{ font-size: 32px; font-weight: 900; color: var(--matrix-title); margin: 0 0 6px 0; letter-spacing: -0.5px; }}
{scope} .subtitle {{ font-size: 15px; color: var(--matrix-subtitle); font-weight: 500; margin: 0; }}
{scope} .controls-bar {{ display: flex; justify-content: flex-end; align-items: center; margin-bottom: 14px; }}
{scope} .axis-filter {{ display: inline-flex; align-items: center; gap: 10px; }}
{scope} .axis-filter-label {{ font-size: 12px; font-weight: 800; color: var(--matrix-axis-text); text-transform: uppercase; letter-spacing: 0.5px; }}
{scope} .axis-select {{
  appearance: none;
  border: 1px solid #CBD5E1;
  border-radius: 8px;
  background: #F8FAFC;
  color: var(--matrix-title);
  font-size: 13px;
  font-weight: 700;
  padding: 8px 34px 8px 12px;
  min-width: 220px;
  outline: none;
  cursor: pointer;
  background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 24 24' stroke='%2364748B'%3E%3Cpath stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M19 9l-7 7-7-7'/%3E%3C/svg%3E\");
  background-repeat: no-repeat;
  background-position: right 10px center;
  background-size: 14px;
}}
{scope} .axis-select:focus {{ border-color: #0D9488; box-shadow: 0 0 0 3px rgba(13,148,136,.15); }}
{scope} .chart-wrapper {{ display: flex; position: relative; flex: 1; min-height: 0; margin-top: 10px; }}
{scope} .y-axis {{ width: 60px; display: flex; flex-direction: column; justify-content: space-between; align-items: center; padding-bottom: 20px; flex-shrink: 0; }}
{scope} .y-axis-text {{ font-size: 14px; font-weight: 800; color: var(--matrix-axis-text); letter-spacing: 0.5px; }}
{scope} .y-axis-main {{ transform: rotate(-90deg); white-space: nowrap; font-size: 16px; font-weight: 900; letter-spacing: 1.5px; color: var(--matrix-axis-main); }}
{scope} .matrix-container {{ position: relative; flex-grow: 1; border-left: 3px solid var(--matrix-axis-line); border-bottom: 3px solid var(--matrix-axis-line); display: flex; flex-wrap: wrap; background: var(--matrix-plot-bg); border-radius: 0 8px 0 0; overflow: hidden; }}
{scope} .quadrant {{ width: 50%; height: 50%; box-sizing: border-box; padding: 24px; font-size: 16px; font-weight: 900; letter-spacing: 1px; position: relative; }}
{scope} .q-tl {{ background-color: var(--matrix-quadrant-tl-bg); color: var(--matrix-quadrant-tl-text); border-right: 2px dashed var(--matrix-quadrant-divider); border-bottom: 2px dashed var(--matrix-quadrant-divider); }}
{scope} .q-tr {{ background-color: var(--matrix-quadrant-tr-bg); color: var(--matrix-quadrant-tr-text); border-bottom: 2px dashed var(--matrix-quadrant-divider); text-align: right; }}
{scope} .q-bl {{ background-color: var(--matrix-quadrant-bl-bg); color: var(--matrix-quadrant-bl-text); border-right: 2px dashed var(--matrix-quadrant-divider); display: flex; align-items: flex-end; }}
{scope} .q-br {{ background-color: var(--matrix-quadrant-br-bg); color: var(--matrix-quadrant-br-text); display: flex; align-items: flex-end; justify-content: flex-end; }}
{scope} .points-layer {{ position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; }}
{scope} .point {{ position: absolute; transform: translate(-50%, -50%); pointer-events: auto; transition: transform 0.2s ease, z-index 0s; }}
{scope} .point:hover {{ transform: translate(-50%, -50%) scale(1.08); z-index: 20; }}
{scope} .dot {{ width: 22px; height: 22px; border-radius: 50%; border: 3px solid var(--matrix-point-border); box-shadow: 0 4px 8px var(--matrix-point-shadow); }}
{scope} .dot-critical {{ background: var(--matrix-dot-critical); }}
{scope} .dot-leverage {{ background: var(--matrix-dot-leverage); }}
{scope} .dot-monitor {{ background: var(--matrix-dot-monitor); }}
{scope} .dot-maintain {{ background: var(--matrix-dot-maintain); }}
{scope} .label {{ position: absolute; top: 50%; background: var(--matrix-label-bg); padding: 6px 12px; border-radius: 6px; font-size: 13px; font-weight: 800; color: var(--matrix-label-text); box-shadow: 0 4px 10px var(--matrix-label-shadow); border: 1px solid var(--matrix-label-border); white-space: nowrap; line-height: 1; transform: translateY(-50%); }}
{scope} .label-right {{ left: 28px; }}
{scope} .label-left {{ right: 28px; }}
{scope} .x-axis {{ display: flex; justify-content: space-between; align-items: center; padding-left: 60px; margin-top: 20px; flex-shrink: 0; }}
{scope} .x-axis-text {{ font-size: 14px; font-weight: 800; color: var(--matrix-axis-text); letter-spacing: 0.5px; }}
{scope} .x-axis-main {{ font-size: 16px; font-weight: 900; letter-spacing: 1.5px; color: var(--matrix-axis-main); }}
"
    )
  }

  env$render_decision_matrix_page <- function(
    points,
    id = "ohep-decision-matrix",
    theme_kicker = "Decision Matrix",
    title = "Health Driver Action Matrix",
    subtitle = "",
    show_axis_control = TRUE,
    y_axis_main = "IMPACT ON OUTCOMES",
    x_axis_main = "PERFORMANCE VS. BENCHMARK",
    y_axis_low = "LOW",
    y_axis_high = "HIGH",
    x_axis_low = "LAGGING",
    x_axis_high = "LEADING",
    color_overrides = NULL,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "decision_matrix"
    )
    matrix_variants <- list(
      overall = points
    )
    if ("impact_engagement" %in% names(points)) {
      p <- points
      p$impact <- suppressWarnings(as.numeric(p$impact_engagement))
      matrix_variants$engagement <- p
    }
    if ("impact_burnout" %in% names(points)) {
      p <- points
      p$impact <- suppressWarnings(as.numeric(p$impact_burnout))
      matrix_variants$burnout <- p
    }
    if ("impact_work_satisfaction" %in% names(points)) {
      p <- points
      p$impact <- suppressWarnings(as.numeric(p$impact_work_satisfaction))
      matrix_variants$work_satisfaction <- p
    }
    if ("impact_enps" %in% names(points)) {
      p <- points
      p$impact <- suppressWarnings(as.numeric(p$impact_enps))
      matrix_variants$enps <- p
    }

    matrix_variants <- lapply(matrix_variants, env$normalize_decision_matrix_points)
    pts <- matrix_variants$overall

    points_html <- vapply(seq_len(nrow(pts)), function(i) {
      label_txt <- as.character(pts$label[[i]])
      chars <- nchar(label_txt)
      est_label_w <- pmax(10, pmin(34, 3 + 0.72 * chars))
      side <- if ((pts$x_pct[[i]] + est_label_w) > 98) "left" else "right"
      if ((pts$x_pct[[i]] - est_label_w) < 2) side <- "right"
      metric_attrs <- paste(
        vapply(names(matrix_variants), function(metric_key) {
          pts_metric <- matrix_variants[[metric_key]]
          paste0(
            " data-top-", metric_key, "=\"", sprintf('%.1f', 100 - pts_metric$y_pct[[i]]), "\"",
            " data-dot-", metric_key, "=\"", pts_metric$dot_class[[i]], "\""
          )
        }, character(1)),
        collapse = ""
      )
      glue::glue(
        "<div class=\"point\"{metric_attrs} style=\"left: {sprintf('%.1f', pts$x_pct[[i]])}%; top: {sprintf('%.1f', 100 - pts$y_pct[[i]])}%;\">
          <div class=\"dot {pts$dot_class[[i]]}\"></div>
          <div class=\"label label-{side}\">{env$escape_text(label_txt)}</div>
        </div>"
      )
    }, character(1))

    axis_options <- c(
      overall = "Outcomes (Overall)",
      engagement = "Engagement",
      burnout = "Burnout",
      work_satisfaction = "Work Satisfaction",
      enps = "eNPS"
    )
    axis_options <- axis_options[names(axis_options) %in% names(matrix_variants)]
    if (!("overall" %in% names(axis_options))) {
      axis_options <- c(overall = "Outcomes (Overall)", axis_options)
    }
    select_options_html <- paste(vapply(names(axis_options), function(k) {
      paste0("<option value=\"", k, "\"", if (identical(k, "overall")) " selected" else "", ">", env$escape_text(axis_options[[k]]), "</option>")
    }, character(1)), collapse = "")

    header_html <- if (nzchar(trimws(theme_kicker)) || nzchar(trimws(title)) || nzchar(trimws(subtitle))) {
      glue::glue(
        "<div class=\"header-section\">
          {if (nzchar(trimws(theme_kicker))) glue::glue('<div class=\"theme-kicker\">{env$escape_text(theme_kicker)}</div>') else ''}
          {if (nzchar(trimws(title))) glue::glue('<h1 class=\"title\">{env$escape_text(title)}</h1>') else ''}
          {if (nzchar(trimws(subtitle))) glue::glue('<p class=\"subtitle\">{env$escape_text(subtitle)}</p>') else ''}
        </div>"
      )
    } else {
      ""
    }

    controls_html <- if (isTRUE(show_axis_control)) {
      glue::glue(
        "<div class=\"controls-bar\">
          <div class=\"axis-filter\">
            <label for=\"{env$escape_text(id)}-axis\" class=\"axis-filter-label\">Y-Axis:</label>
            <select id=\"{env$escape_text(id)}-axis\" class=\"axis-select\">{select_options_html}</select>
          </div>
        </div>"
      )
    } else {
      ""
    }

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-dm-root\">
        {header_html}
        {controls_html}
        <div class=\"chart-wrapper\">
          <div class=\"y-axis\">
            <div class=\"y-axis-text\">{env$escape_text(y_axis_high)}</div>
            <div class=\"y-axis-main\">{env$escape_text(y_axis_main)}</div>
            <div class=\"y-axis-text\">{env$escape_text(y_axis_low)}</div>
          </div>
          <div class=\"matrix-container\">
            <div class=\"quadrant q-tl\">CRITICAL FOCUS</div>
            <div class=\"quadrant q-tr\">LEVERAGE</div>
            <div class=\"quadrant q-bl\">MONITOR</div>
            <div class=\"quadrant q-br\">MAINTAIN</div>
            <div class=\"points-layer\">
              {glue::glue_collapse(points_html, sep = '')}
            </div>
          </div>
        </div>
        <div class=\"x-axis\">
          <div class=\"x-axis-text\">{env$escape_text(x_axis_low)}</div>
          <div class=\"x-axis-main\">{env$escape_text(x_axis_main)}</div>
          <div class=\"x-axis-text\">{env$escape_text(x_axis_high)}</div>
        </div>
        <script>
          (function() {{
            var root = document.getElementById('{env$escape_text(id)}');
            if (!root) return;
            var axisSel = root.querySelector('.axis-select') || document.getElementById('{env$escape_text(id)}-axis');
            if (!axisSel) return;
            var points = root.querySelectorAll('.point');
            function applyAxis(key) {{
              points.forEach(function(pt) {{
                var top = pt.getAttribute('data-top-' + key);
                var dotCls = pt.getAttribute('data-dot-' + key);
                if (top) pt.style.top = top + '%';
                var dot = pt.querySelector('.dot');
                if (dot && dotCls) dot.className = 'dot ' + dotCls;
              }});
            }}
            axisSel.addEventListener('change', function() {{ applyAxis(axisSel.value); }});
            applyAxis(axisSel.value || 'overall');
          }})();
        </script>
      </div>"
    )

    css <- env$decision_matrix_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$render_fundamental_page <- function(
    dashboard_data,
    id = "ohep-custom-dashboard",
    color_overrides = NULL,
    benchmark_company = TRUE,
    benchmark_index = TRUE,
    benchmark_prior = TRUE,
    ...
  ) {
    inline_overrides <- list(...)
    colors <- env$resolve_brand_colors(
      color_overrides = color_overrides,
      extra_overrides = inline_overrides,
      graph = "fundamental"
    )
    dashboard_data <- env$validate_fundamental_page_data(dashboard_data)
    fundamental <- dashboard_data$fundamental
    outcomes <- dashboard_data$outcomes
    items <- dashboard_data$items
    active_benchmarks <- env$get_active_benchmarks(
      items = items,
      fundamental_row = fundamental,
      benchmark_company = benchmark_company,
      benchmark_index = benchmark_index,
      benchmark_prior = benchmark_prior
    )

    hero <- env$build_hero_card(fundamental)
    outcomes_table <- env$build_outcomes_table(outcomes)
    left_col <- env$build_item_column(items, "left", active_benchmarks = active_benchmarks)
    right_col <- env$build_item_column(items, "right", active_benchmarks = active_benchmarks)
    count_rows <- function(col_name) {
      sub <- items[as.character(items$column) == col_name, , drop = FALSE]
      if (nrow(sub) < 1L) return(0L)
      section_keys <- paste0(sub$section_order, "::", sub$section)
      nrow(sub) + length(unique(section_keys))
    }
    max_item_rows <- max(count_rows("left"), count_rows("right"))
    dynamic_min_height <- max(720L, 420L + as.integer(max_item_rows) * 44L)

    body_html <- glue::glue(
      "<div id=\"{env$escape_text(id)}\" class=\"ohep-root\" style=\"min-height:{as.integer(dynamic_min_height)}px;\">
        <div class=\"top-row\">
          {hero}
          {outcomes_table}
        </div>
        <div class=\"bottom-row\">
          <div class=\"card item-card\">
            <div class=\"item-title-row\">
              <h2 class=\"card-title\">Item-Level Breakdown</h2>
              <div class=\"legend-group\">
                <div class=\"leg-item\"><div class=\"leg-dot bg-disagree\"></div> Disagree</div>
                <div class=\"leg-item\"><div class=\"leg-dot bg-neutral\"></div> Neutral</div>
                <div class=\"leg-item\"><div class=\"leg-dot bg-agree\"></div> Agree</div>
              </div>
            </div>
            <div class=\"item-grid-2col\">
              {left_col}
              {right_col}
            </div>
          </div>
        </div>
      </div>"
    )

    css <- env$fundamental_page_css(id, colors = colors)
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::HTML(body_html)
    )
  }

  env$fundamental_page <- function(
    fundamental,
    index_data,
    user_data,
    id = "ohep-custom-dashboard",
    color_overrides = NULL,
    benchmark_company = TRUE,
    benchmark_index = TRUE,
    benchmark_prior = TRUE,
    ...
  ) {
    dashboard_data <- env$build_dashboard_data_from_inputs(
      fundamental = fundamental,
      index_data = index_data,
      user_data = user_data
    )
    env$render_fundamental_page(
      dashboard_data = dashboard_data,
      id = id,
      color_overrides = color_overrides,
      benchmark_company = benchmark_company,
      benchmark_index = benchmark_index,
      benchmark_prior = benchmark_prior,
      ...
    )
  }

  env
}
