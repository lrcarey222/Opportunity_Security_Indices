# Competitive-policy alignment: a supporting diagnostic of the strategic posture
# (docs/strategic_position.md, sections 5 and 7). It compares Competitive Position with Revealed Policy
# Priority within a country (aligned_strength, latent_advantage, policy_led_bet, low_priority) and
# reports the priority gap. It explains a posture; it is not a framework of its own, and the
# recommended posture (posture_engine.R) never reads it.
# Pure functions: no IO. Needs R/strategic_position/sp_helpers.R and R/utils/supply_chain_stages.R.

SP_CP_ALIGNMENT_CLASSES <- c("aligned_strength", "latent_advantage", "policy_led_bet", "low_priority", "insufficient_data")

sp_alignment_check_cols <- function(tbl, cols, label) {
  missing <- setdiff(cols, names(tbl))
  if (length(missing) > 0) stop(label, ": missing column(s) ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

# Class from the two percentiles; insufficient_data when either is missing.
sp_alignment_class <- function(cp_pct, pp_pct, cp_threshold = 0.5, pp_threshold = 0.5) {
  hi_cp <- cp_pct >= cp_threshold
  hi_pp <- pp_pct >= pp_threshold
  dplyr::case_when(
    is.na(cp_pct) | is.na(pp_pct) ~ "insufficient_data",
    hi_cp & hi_pp ~ "aligned_strength",
    hi_cp & !hi_pp ~ "latent_advantage",
    !hi_cp & hi_pp ~ "policy_led_bet",
    TRUE ~ "low_priority"
  )
}

# Within-country priority gap over the cells where both position and priority are observed: the
# within-country percentile of the (cross-country) position percentile minus the within-country
# percentile of the policy priority. Re-ranking both over the same cells keeps the two distributions
# comparable. Also returns the z-score, rank and cross-country versions for the diagnostics.
sp_priority_gap <- function(tbl, cp_col = "competitive_position_percentile",
                            pp_col = "policy_priority_percentile", country_col = "Country") {
  sp_alignment_check_cols(tbl, c(country_col, cp_col, pp_col), "sp_priority_gap()")
  z <- function(x) {
    s <- stats::sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(ifelse(is.na(x), NA_real_, 0))
    (x - mean(x, na.rm = TRUE)) / s
  }
  rank_share <- function(x) {
    n <- sum(!is.na(x))
    out <- rep(NA_real_, length(x))
    if (n == 0) return(out)
    out[!is.na(x)] <- rank(x[!is.na(x)], ties.method = "average") / n
    out
  }
  tbl %>%
    dplyr::mutate(.both = !is.na(.data[[cp_col]]) & !is.na(.data[[pp_col]])) %>%
    dplyr::group_by(.data[[country_col]]) %>%
    dplyr::mutate(
      .cp = dplyr::if_else(.data$.both, .data[[cp_col]], NA_real_),
      .pp = dplyr::if_else(.data$.both, .data[[pp_col]], NA_real_),
      priority_gap = sp_midrank_pct(.data$.cp) - sp_midrank_pct(.data$.pp),
      priority_gap_cross = .data$.cp - .data$.pp,
      priority_gap_z = z(.data$.cp) - z(.data$.pp),
      priority_gap_rank = rank_share(.data$.cp) - rank_share(.data$.pp)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-".both", -".cp", -".pp")
}

# Adds cp_within_country_pct, competitive_policy_alignment, alignment_boundary and the priority gaps.
# cfg = the alignment list of config/strategic_position.yml. With competitive_position_basis
# within_country (the default), position is the within-country percentile of the cross-country
# position percentile: across ~218 economies nearly every cell of a large economy sits above the
# median, so only the within-country view separates its strengths.
sp_alignment <- function(tbl, cfg = list()) {
  cp_t <- cfg$competitive_position_threshold %||% 0.5
  pp_t <- cfg$policy_priority_threshold %||% 0.5
  sn_t <- cfg$strategic_need_threshold %||% 0.5
  band <- cfg$boundary_band %||% 0.1
  basis <- cfg$competitive_position_basis %||% "within_country"
  if (!basis %in% c("within_country", "cross_country")) stop("sp_alignment(): unknown competitive_position_basis ", basis)
  sp_alignment_check_cols(
    tbl, c("Country", "competitive_position_percentile", "policy_priority_percentile", "strategic_need_percentile"),
    "sp_alignment()"
  )
  near <- function(x, t) !is.na(x) & abs(x - t) < band
  out <- tbl %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(cp_within_country_pct = sp_midrank_pct(.data$competitive_position_percentile)) %>%
    dplyr::ungroup()
  cp_axis <- if (basis == "within_country") out$cp_within_country_pct else out$competitive_position_percentile
  out <- out %>%
    dplyr::mutate(
      competitive_policy_alignment = sp_alignment_class(cp_axis, .data$policy_priority_percentile, cp_t, pp_t),
      competitive_policy_basis = basis,
      alignment_boundary = near(cp_axis, cp_t) | near(.data$policy_priority_percentile, pp_t) |
        near(.data$strategic_need_percentile, sn_t)
    )
  sp_priority_gap(out)
}

# Roll six-stage cells up to Upstream / Midstream / Downstream for compatibility views.
# mean_cols: scores and percentiles (mean of the child stages that have a value);
# sum_cols: additive quantities whose stage values partition the source (attribution shares that
# sum to 1 per record), so summing child stages never counts a record twice;
# once_cols: values repeated across child stages (tech-wide or shared evidence). These are taken
# once; the function stops if they differ across the children, because summing them would
# double-count one observation.
sp_legacy_rollup <- function(tbl, mean_cols = character(), sum_cols = character(), once_cols = character(),
                             by = c("Country", "tech"), stage_col = "supply_chain") {
  sp_alignment_check_cols(tbl, c(by, stage_col, mean_cols, sum_cols, once_cols), "sp_legacy_rollup()")
  if (length(intersect(mean_cols, sum_cols)) > 0 || length(intersect(mean_cols, once_cols)) > 0 ||
    length(intersect(sum_cols, once_cols)) > 0) {
    stop("sp_legacy_rollup(): a column is given more than one rule.")
  }
  assert_supply_chain_stages(tbl[[stage_col]], label = "sp_legacy_rollup() stages")
  grouped <- tbl %>%
    dplyr::mutate(legacy_stage = supply_chain_stage_rollup(.data[[stage_col]])) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "legacy_stage"))))

  if (length(once_cols) > 0) {
    clash <- grouped %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(once_cols), ~ length(unique(.x[!is.na(.x)])) > 1),
        .groups = "drop"
      )
    bad <- vapply(once_cols, function(col) any(clash[[col]]), logical(1))
    if (any(bad)) {
      stop(
        "sp_legacy_rollup(): once_cols differ across child stages (not a repeated value): ",
        paste(once_cols[bad], collapse = ", ")
      )
    }
  }
  grouped %>%
    dplyr::summarise(
      n_stages = dplyr::n_distinct(.data[[stage_col]]),
      dplyr::across(dplyr::all_of(mean_cols), sp_mean_available),
      dplyr::across(dplyr::all_of(sum_cols), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(once_cols), ~ if (all(is.na(.x))) .x[1] else .x[!is.na(.x)][1]),
      .groups = "drop"
    )
}
