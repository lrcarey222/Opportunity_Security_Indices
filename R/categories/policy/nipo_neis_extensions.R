# ==============================================================================
# NIPO -> NEIS FRAMEWORK EXTENSIONS
# ------------------------------------------------------------------------------
# Companion module to nipo_policy_index.R. Nothing here replaces DIS; it adds the
# NEIS framework layers that DIS structurally cannot express, plus the de-biasing
# fixes discussed in review.
#
# Source AFTER nipo_policy_index.R:
#   source("R/categories/policy/nipo_policy_index.R")
#   source("R/categories/policy/nipo_neis_extensions.R")
#
# Design rules followed here:
#   1. Confidence is epistemic. It never multiplies strength. It filters, weights
#      robustness runs, and is reported. See neis_strength_variants().
#   2. Every metric keeps its own column. No new composites. The framework's own
#      warning about false precision in composite scores applies to this module.
#   3. Directionality (who is affected) is separated from intensity (how hard the
#      state is pushing). Affected Jurisdiction is used for exposure and
#      rival-direction, not as a strength multiplier.
#   4. Motive and policy-layer fields are read by name via a config list, because
#      NIPO export headers vary by vintage. Set NEIS_COLS once, up front.
#
# NOT YET RUN. R was unavailable in the authoring environment, so this has been
# written carefully but not executed. Run neis_selftest() against a small slice
# of policy_asof before wiring into the pipeline.
# ==============================================================================

# ---- Column configuration ----------------------------------------------------
# Adjust the right-hand side to match your NIPO export headers, then everything
# downstream resolves through neis_col().
# VERIFIED against the July 2026 and February 2026 exports (52 columns each,
# identical headers). Two names in the original draft were wrong, and one
# assumption was structurally wrong:
#
#   - National security and geopolitical concern are ONE COMBINED FIELD,
#     "Motive: National Security or Geopolitical Concern", not two. Pointing two
#     keys at it would make n_motives count 2 and halve motive_unit_weight on
#     every national-security measure. MOTIVE_KEYS is therefore five, not six.
#   - Resilience carries a "(Non-Food)" qualifier.
#
# All five are logical columns, 100% populated. There are three further
# "Mentions ..." fields (Food Security, Public Health Concerns, Other) which are
# NOT motives in the IMF sense and are deliberately not included here.
NEIS_COLS <- list(
  motive_security          = "Motive: National Security or Geopolitical Concern",
  motive_resilience        = "Motive: Resilience/Security of Supply (Non-Food)",
  motive_competitiveness   = "Motive: Strategic Competitiveness",
  motive_climate           = "Motive: Climate Change Mitigation",
  motive_digital           = "Motive: Digital Transformation",
  layer_field              = "Levels of Policy Intervention",
  affected_jurisdiction    = "Affected Jurisdiction",
  implementing_jurisdiction = "Implementing Jurisdiction"
)

MOTIVE_KEYS <- c(
  "motive_security", "motive_resilience",
  "motive_competitiveness", "motive_climate", "motive_digital"
)

# NIPO's layer field is clean categorical data with exactly three values, so it
# is mapped directly rather than pattern-matched. The regex approach in the
# original draft misfired: its "action" pattern was tested before "strategy",
# and it also treated a named Firm: Beneficiary as decisive, which would
# reclassify a "Policy or regulation" row carrying a beneficiary as a firm-level
# action - inflating delivery_ratio's numerator and deflating its denominator at
# the same time.
NEIS_LAYER_MAP <- c(
  "industrial strategy or plan" = "strategy",
  "policy or regulation"        = "policy",
  "firm-specific"               = "action"
)

# Fallback patterns, used only for a value not present in NEIS_LAYER_MAP, so a
# future vintage with richer labels degrades rather than failing. Order matters:
# strategy is tested before policy because "industrial strategy or plan"
# contains both "strateg" and "plan".
NEIS_LAYER_PATTERNS <- list(
  strategy = "strateg|plan|framework|vision|roadmap",
  policy   = "polic|regulat|law|decree|act\\b|scheme|programme|program",
  action   = "firm|award|grant to|authoris|authoriz|licence decision|license decision|case|determination"
)

# Instrument families classified for the Fence Posture index. "Restrictive"
# means the measure operates by denying or conditioning access rather than by
# funding capacity.
NEIS_RESTRICTIVE_FAMILIES <- c(
  "fam_export_policy", "fam_import_policy", "fam_trade_defence"
)
NEIS_CONDITIONAL_FAMILIES <- c(
  "fam_localisation_policy", "fam_procurement_policy"
)
NEIS_FUNDING_FAMILIES <- c(
  "fam_subsidy", "fam_export_incentive"
)
NEIS_ALL_FAMILIES <- c(
  NEIS_RESTRICTIVE_FAMILIES, NEIS_CONDITIONAL_FAMILIES, NEIS_FUNDING_FAMILIES,
  "fam_fdi_policy", "fam_other_policy"
)

# ==============================================================================
# 0) Small helpers
# ==============================================================================

neis_col <- function(tbl, key, required = FALSE) {
  nm <- NEIS_COLS[[key]]
  if (is.null(nm) || !nm %in% names(tbl)) {
    if (required) {
      stop(sprintf("neis_col(): column for '%s' (expected '%s') not found. Update NEIS_COLS.",
                   key, if (is.null(nm)) "<unset>" else nm))
    }
    return(NULL)
  }
  nm
}

neis_bool <- function(x) {
  if (is.logical(x)) return(dplyr::coalesce(x, FALSE))
  v <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::coalesce(v %in% c("true", "t", "yes", "y", "1"), FALSE)
}

# Shannon entropy normalised to [0,1]. Used for instrument diversity.
neis_entropy <- function(counts) {
  counts <- counts[is.finite(counts) & counts > 0]
  k <- length(counts)
  if (k <= 1) return(0)
  p <- counts / sum(counts)
  -sum(p * log(p)) / log(k)
}

# Herfindahl over shares, rescaled so 0 = perfectly even, 1 = fully concentrated.
neis_hhi <- function(counts) {
  counts <- counts[is.finite(counts) & counts >= 0]
  k <- length(counts)
  if (k <= 1 || sum(counts) <= 0) return(NA_real_)
  p <- counts / sum(counts)
  (sum(p^2) - 1 / k) / (1 - 1 / k)
}

# ==============================================================================
# 1) Word-boundary keyword matching - NOW LIVES IN nipo_policy_index.R
# ------------------------------------------------------------------------------
# This module originally defined neis_bound_kws(), neis_bind_dictionary(),
# neis_audit_keywords() and NEIS_KEYWORD_BLOCKLIST. They have been DELETED here
# rather than kept, because this file is sourced AFTER nipo_policy_index.R and
# these definitions would have silently overwritten the canonical ones with
# different behaviour: no allow_plural argument, no handling of the \bstem\w*
# prefix terms, and a `blocklisted` column that the canonical audit does not
# emit.
#
# The canonical implementations are in nipo_policy_index.R, where the fix is
# applied to TECH_KEYWORDS and SUPPLY_CHAIN_KEYWORDS at load time. Call them
# from there; they are in scope by the time this file is sourced.
# ==============================================================================
# 2) FIX: separate confidence from strength
# ------------------------------------------------------------------------------
# Produces four strength variants from the same allocation table so you can see
# how much of the cross-country ranking is documentation quality rather than
# policy intensity. If variant rankings diverge materially, the confidence
# multiplier was doing the work.
# ==============================================================================

#' @param alloc_long Output of allocate_policy_to_tech_sc(), post cross-cutting
#'   handling. Must contain scale_strength_pkg, alloc, mapping_confidence,
#'   m_geo (if present), and mapped_share.
#' @param conf_threshold Minimum mapping_confidence for the "filtered" variant.
neis_strength_variants <- function(alloc_long,
                                   conf_threshold = 0.75,
                                   drop_geo_multiplier = TRUE) {
  need <- c("scale_strength_pkg", "alloc", "mapping_confidence")
  miss <- setdiff(need, names(alloc_long))
  if (length(miss) > 0) {
    stop("neis_strength_variants(): missing columns: ", paste(miss, collapse = ", "))
  }

  out <- alloc_long %>%
    dplyr::mutate(
      mapping_confidence = dplyr::coalesce(.data$mapping_confidence, 1),

      # Remove the affected-jurisdiction breadth multiplier from intensity.
      # m_geo is a directionality property; it is used in neis_rival_direction().
      #
      # IMPORTANT: divide by m_geo_APPLIED, not m_geo. Since Task 3,
      # build_policy_base() excludes m_geo from the product by default and
      # records what it actually used in m_geo_applied (1 when excluded). Naively
      # dividing by m_geo would remove the term a SECOND time on any table built
      # with the current defaults, silently deflating every strength by a factor
      # of up to 3. Falling back to m_geo only when m_geo_applied is absent keeps
      # this correct for a legacy allocation table too.
      .geo_adj = if (isTRUE(drop_geo_multiplier)) {
        if ("m_geo_applied" %in% names(alloc_long)) {
          dplyr::if_else(is.finite(.data$m_geo_applied) & .data$m_geo_applied > 0,
                         1 / .data$m_geo_applied, 1)
        } else if ("m_geo" %in% names(alloc_long)) {
          dplyr::if_else(is.finite(.data$m_geo) & .data$m_geo > 0, 1 / .data$m_geo, 1)
        } else {
          1
        }
      } else {
        1
      },

      # v0: current behaviour, for backward comparison.
      dis_v0_current = .data$scale_strength_pkg * .data$alloc * .data$mapping_confidence,

      # v1: recommended default. Confidence removed, geo removed.
      dis_v1_neutral = .data$scale_strength_pkg * .data$alloc * .data$.geo_adj,

      # v2: confidence as a hard filter rather than a weight.
      dis_v2_filtered = dplyr::if_else(
        .data$mapping_confidence >= conf_threshold,
        .data$scale_strength_pkg * .data$alloc * .data$.geo_adj,
        0
      ),

      # v3: confidence as a bounded [0,1] reliability weight. Never amplifies.
      dis_v3_downweight_only = .data$scale_strength_pkg * .data$alloc * .data$.geo_adj *
        pmin(1, .data$mapping_confidence)
    ) %>%
    dplyr::select(-".geo_adj")

  out
}

#' Rank-stability diagnostic across the strength variants.
#'
#' Returns Spearman correlations between variants within each tech x stage cell.
#' Low correlation between v0 and v1 means the published index was substantially
#' a documentation-quality index.
neis_variant_stability <- function(strength_variants_tbl,
                                   group_cols = c("tech", "supply_chain")) {
  vars <- c("dis_v0_current", "dis_v1_neutral", "dis_v2_filtered", "dis_v3_downweight_only")
  vars <- intersect(vars, names(strength_variants_tbl))

  strength_variants_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", group_cols)))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_countries = dplyr::n(),
      rho_v0_v1 = suppressWarnings(stats::cor(.data$dis_v0_current, .data$dis_v1_neutral,
                                              method = "spearman", use = "pairwise.complete.obs")),
      rho_v0_v3 = suppressWarnings(stats::cor(.data$dis_v0_current, .data$dis_v3_downweight_only,
                                              method = "spearman", use = "pairwise.complete.obs")),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$rho_v0_v1)
}

# ==============================================================================
# 3) WHY LAYER: Objective Structure
# ------------------------------------------------------------------------------
# Motive shares, motive concentration, and the motive x instrument profile.
# Multi-motive measures are split equally, matching the IMF convention.
# ==============================================================================

neis_attach_motives <- function(policy_tbl, motive_source_year_cutoff = 2023) {
  present <- MOTIVE_KEYS[vapply(MOTIVE_KEYS, function(k) !is.null(neis_col(policy_tbl, k)),
                                logical(1))]
  if (length(present) == 0) {
    warning("neis_attach_motives(): no motive columns found. Update NEIS_COLS. ",
            "Objective Structure metrics will be all-NA.")
  }

  out <- policy_tbl
  for (k in present) {
    out[[k]] <- neis_bool(out[[NEIS_COLS[[k]]]])
  }
  for (k in setdiff(MOTIVE_KEYS, present)) {
    out[[k]] <- NA
  }

  out %>%
    dplyr::mutate(
      n_motives = rowSums(
        dplyr::across(dplyr::all_of(MOTIVE_KEYS), ~ as.integer(dplyr::coalesce(.x, FALSE))),
        na.rm = TRUE
      ),
      has_motive = .data$n_motives > 0,
      # Equal split across motives, per the IMF's own weighting convention.
      motive_unit_weight = dplyr::if_else(.data$n_motives > 0, 1 / .data$n_motives, NA_real_),
      # Pre-cutoff motives in the historical extension are LLM-inferred at a 60%
      # probability threshold. Do not compare levels across this boundary without
      # surfacing the flag.
      announce_year_for_motive = suppressWarnings(
        as.integer(format(as_date_safe(.data$announce_date), "%Y"))
      ),
      motive_source = dplyr::case_when(
        is.na(.data$announce_year_for_motive) ~ "unknown",
        .data$announce_year_for_motive >= motive_source_year_cutoff ~ "manual",
        TRUE ~ "llm_inferred"
      )
    )
}

neis_objective_structure <- function(alloc_long_with_motives,
                                     strength_col = "dis_v1_neutral",
                                     group_cols = c("iso3", "country", "tech", "supply_chain")) {
  if (!strength_col %in% names(alloc_long_with_motives)) {
    stop("neis_objective_structure(): strength column '", strength_col, "' not found.")
  }

  long <- alloc_long_with_motives %>%
    dplyr::filter(dplyr::coalesce(.data$has_motive, FALSE)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(MOTIVE_KEYS),
      names_to = "motive",
      values_to = "motive_flag"
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$motive_flag, FALSE)) %>%
    dplyr::mutate(
      motive_strength = .data[[strength_col]] * .data$motive_unit_weight
    )

  shares <- long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "motive")))) %>%
    dplyr::summarise(
      motive_strength = sum(.data$motive_strength, na.rm = TRUE),
      motive_n = dplyr::n_distinct(.data$policy_id),
      .groups = "drop"
    )

  totals <- shares %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      total_motive_strength = sum(.data$motive_strength, na.rm = TRUE),
      objective_hhi = neis_hhi(.data$motive_strength),
      .groups = "drop"
    )

  wide <- shares %>%
    dplyr::left_join(totals, by = group_cols) %>%
    dplyr::mutate(
      share = dplyr::if_else(.data$total_motive_strength > 0,
                             .data$motive_strength / .data$total_motive_strength,
                             NA_real_)
    ) %>%
    dplyr::select(dplyr::all_of(c(group_cols, "motive", "share"))) %>%
    tidyr::pivot_wider(names_from = "motive", values_from = "share",
                       names_prefix = "share_", values_fill = 0)

  # Motive coverage: what fraction of strength carries any stated motive at all.
  coverage <- alloc_long_with_motives %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      motive_coverage = {
        tot <- sum(.data[[strength_col]], na.rm = TRUE)
        tagged <- sum(.data[[strength_col]][dplyr::coalesce(.data$has_motive, FALSE)], na.rm = TRUE)
        if (tot > 0) tagged / tot else NA_real_
      },
      share_llm_inferred = mean(.data$motive_source == "llm_inferred", na.rm = TRUE),
      .groups = "drop"
    )

  totals %>%
    dplyr::left_join(wide, by = group_cols) %>%
    dplyr::left_join(coverage, by = group_cols)
}

# ==============================================================================
# 4) WHEN / HOW B LAYER: Delivery Conversion and Taper Profile
# ------------------------------------------------------------------------------
# The two metrics that operationalise "policy architecture does not equal
# industrial delivery". Both use fields already ingested; neither is currently
# surfaced, because m_duration saturates at 1.0 for every in-force measure and
# m_duration = 0 discards announced-but-unimplemented measures entirely.
# ==============================================================================

#' Classify a measure into NIPO's three layers.
#'
#' The field is clean categorical data with exactly three values on the current
#' export, so it is mapped directly. Two deliberate departures from the original
#' draft:
#'
#'   1. has_beneficiary is NOT consulted. It was tested first, which reclassified
#'      any "Policy or regulation" row naming a beneficiary as a firm-level
#'      action. That inflates delivery_ratio's numerator and deflates its
#'      denominator simultaneously. On the July 2026 export 11,990 rows name a
#'      beneficiary and every one of them is already "Firm-specific", so
#'      consulting it adds nothing and can only mislead.
#'   2. Strategy is tested before action in the regex fallback, because
#'      "industrial strategy or plan" would otherwise never be reached.
neis_classify_layer <- function(policy_tbl) {
  lf <- neis_col(policy_tbl, "layer_field")
  if (is.null(lf)) {
    warning("neis_classify_layer(): layer field not found. Delivery Conversion will be NA.")
    return(dplyr::mutate(policy_tbl, nipo_layer = NA_character_))
  }

  txt <- stringr::str_squish(stringr::str_to_lower(
    dplyr::coalesce(as.character(policy_tbl[[lf]]), "")
  ))

  mapped <- unname(NEIS_LAYER_MAP[txt])

  policy_tbl %>%
    dplyr::mutate(
      nipo_layer = dplyr::case_when(
        !is.na(mapped) ~ mapped,
        # Fallback for a label this vintage does not carry.
        stringr::str_detect(txt, NEIS_LAYER_PATTERNS$strategy) ~ "strategy",
        stringr::str_detect(txt, NEIS_LAYER_PATTERNS$action)   ~ "action",
        stringr::str_detect(txt, NEIS_LAYER_PATTERNS$policy)   ~ "policy",
        TRUE ~ "unclassified"
      )
    )
}

#' Delivery Conversion index.
#'
#' Three components, reported separately:
#'   delivery_ratio  - firm/sector-level actions per strategy-or-policy entry
#'   impl_lag_median - median days from announcement to implementation
#'   impl_rate       - share of announced measures reaching implementation,
#'                     right-censored to exclude records too recent to judge
neis_delivery_conversion <- function(alloc_long,
                                     as_of_date,
                                     censor_days = 540,
                                     group_cols = c("iso3", "country", "tech", "supply_chain")) {
  as_of_date <- as_date_safe(as_of_date)
  if (is.na(as_of_date)) stop("neis_delivery_conversion(): as_of_date must be a valid date.")

  d <- alloc_long %>%
    neis_classify_layer() %>%
    dplyr::mutate(
      announce_date = as_date_safe(.data$announce_date),
      impl_date = as_date_safe(.data$impl_date),
      impl_lag_days = dplyr::if_else(
        !is.na(.data$announce_date) & !is.na(.data$impl_date) &
          .data$impl_date >= .data$announce_date,
        as.numeric(.data$impl_date - .data$announce_date),
        NA_real_
      ),
      # A measure announced within censor_days of as_of_date has not had time to
      # be implemented or recorded. Exclude it from impl_rate rather than count
      # it as a failure to deliver. GTA notes recording lags explicitly.
      censored = !is.na(.data$announce_date) &
        as.numeric(as_of_date - .data$announce_date) < censor_days,
      implemented = !is.na(.data$impl_date)
    )

  # Collapse to policy level first so multi-HS6 policies are not counted twice.
  pol <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "policy_id")))) %>%
    dplyr::summarise(
      nipo_layer = dplyr::first(.data$nipo_layer),
      impl_lag_days = dplyr::first(.data$impl_lag_days),
      censored = dplyr::first(.data$censored),
      implemented = dplyr::first(.data$implemented),
      .groups = "drop"
    )

  pol %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_policies = dplyr::n(),
      n_strategy = sum(.data$nipo_layer == "strategy", na.rm = TRUE),
      n_policy_layer = sum(.data$nipo_layer == "policy", na.rm = TRUE),
      n_action = sum(.data$nipo_layer == "action", na.rm = TRUE),
      n_unclassified = sum(.data$nipo_layer == "unclassified", na.rm = TRUE),
      # Ratio is NA rather than Inf when there is no architecture layer to
      # convert from. An absent denominator is not high conversion.
      delivery_ratio = dplyr::if_else(
        (.data$n_strategy + .data$n_policy_layer) > 0,
        .data$n_action / (.data$n_strategy + .data$n_policy_layer),
        NA_real_
      ),
      impl_lag_median = suppressWarnings(
        stats::median(.data$impl_lag_days, na.rm = TRUE)
      ),
      impl_lag_p75 = suppressWarnings(
        as.numeric(stats::quantile(.data$impl_lag_days, 0.75, na.rm = TRUE, names = FALSE))
      ),
      n_uncensored = sum(!.data$censored, na.rm = TRUE),
      impl_rate = dplyr::if_else(
        .data$n_uncensored > 0,
        sum(.data$implemented & !.data$censored, na.rm = TRUE) / .data$n_uncensored,
        NA_real_
      ),
      .groups = "drop"
    )
}

#' Taper Profile.
#'
#' Answers the exit-criteria question the framework flags as unresolved: do
#' measures in this capability actually end, and after how long. Uses the
#' removal date directly rather than m_duration, which saturates.
#'
#' Right-censoring is handled explicitly: in-force measures contribute exposure
#' time but not a removal event, so median_duration is a Kaplan-Meier estimate
#' when the survival package is available and a naive median otherwise.
neis_taper_profile <- function(alloc_long,
                               as_of_date,
                               group_cols = c("iso3", "country", "tech", "supply_chain"),
                               by_instrument = FALSE) {
  as_of_date <- as_date_safe(as_of_date)

  keys <- if (isTRUE(by_instrument)) c(group_cols, "primary_family") else group_cols

  # Computed up front rather than as neis_primary_family(.) inside mutate(). The
  # magrittr dot resolves to the pre-mutate table there, which happens to work
  # but silently depends on that ordering.
  alloc_long$primary_family <- neis_primary_family(alloc_long)

  d <- alloc_long %>%
    dplyr::mutate(
      impl_date = as_date_safe(.data$impl_date),
      removal_date = as_date_safe(.data$removal_date),
      event_removed = !is.na(.data$removal_date) & .data$removal_date <= as_of_date,

      # Prefer the pipeline's own exposure_days (added in Task 4), which already
      # censors at as_of_date and returns NA for a removal recorded BEFORE
      # implementation. Recomputing it here would reintroduce that defect: the
      # local formula below has no removal >= impl guard on its final branch, so
      # an incoherent record would yield a negative exposure. Only fall back
      # when the column is absent.
      exposure_days = if ("exposure_days" %in% names(alloc_long)) {
        suppressWarnings(as.numeric(.data$exposure_days))
      } else {
        dplyr::case_when(
          is.na(.data$impl_date) ~ NA_real_,
          !is.na(.data$removal_date) & .data$removal_date < .data$impl_date ~ NA_real_,
          !is.na(.data$removal_date) ~
            as.numeric(pmin(.data$removal_date, as_of_date) - .data$impl_date),
          TRUE ~ as.numeric(as_of_date - .data$impl_date)
        )
      }
    ) %>%
    dplyr::filter(!is.na(.data$exposure_days), .data$exposure_days >= 0)

  pol <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "policy_id")))) %>%
    dplyr::summarise(
      event_removed = dplyr::first(.data$event_removed),
      exposure_days = dplyr::first(.data$exposure_days),
      .groups = "drop"
    )

  pol %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      n_policies = dplyr::n(),
      n_removed = sum(.data$event_removed, na.rm = TRUE),
      taper_rate = .data$n_removed / .data$n_policies,
      # Naive median of observed durations among removed measures only. Biased
      # short; reported alongside the KM estimate below for contrast.
      median_days_removed_only = suppressWarnings(
        stats::median(.data$exposure_days[.data$event_removed], na.rm = TRUE)
      ),
      median_days_km = neis_km_median(.data$exposure_days, .data$event_removed),
      retention_12m = neis_retention_at(.data$exposure_days, .data$event_removed, 365),
      retention_36m = neis_retention_at(.data$exposure_days, .data$event_removed, 1095),
      .groups = "drop"
    )
}

neis_primary_family <- function(tbl) {
  fams <- intersect(NEIS_ALL_FAMILIES, names(tbl))
  if (length(fams) == 0) return(rep(NA_character_, nrow(tbl)))
  # Deterministic priority: funding, then conditional, then restrictive.
  priority <- c(NEIS_FUNDING_FAMILIES, NEIS_CONDITIONAL_FAMILIES,
                "fam_fdi_policy", NEIS_RESTRICTIVE_FAMILIES, "fam_other_policy")
  priority <- intersect(priority, fams)
  out <- rep(NA_character_, nrow(tbl))
  for (f in rev(priority)) {
    hit <- neis_bool(tbl[[f]])
    out[hit] <- f
  }
  out
}

# Kaplan-Meier median with right censoring. Falls back gracefully.
neis_km_median <- function(time, event) {
  ok <- is.finite(time) & !is.na(event)
  time <- time[ok]; event <- as.integer(event[ok])
  if (length(time) == 0) return(NA_real_)
  if (sum(event) == 0) return(NA_real_)  # nothing ever ended; median undefined

  ord <- order(time)
  time <- time[ord]; event <- event[ord]
  n_at_risk <- length(time)
  surv <- 1
  for (i in seq_along(time)) {
    if (event[i] == 1L) {
      surv <- surv * (1 - 1 / n_at_risk)
      if (surv <= 0.5) return(time[i])
    }
    n_at_risk <- n_at_risk - 1
    if (n_at_risk <= 0) break
  }
  NA_real_  # survival never reached 0.5 within observed follow-up
}

# Share still in force at horizon_days, censoring-aware.
neis_retention_at <- function(time, event, horizon_days) {
  ok <- is.finite(time) & !is.na(event)
  time <- time[ok]; event <- as.integer(event[ok])
  if (length(time) == 0) return(NA_real_)
  # Only measures observed for at least horizon_days, or removed before it,
  # are informative about retention at that horizon.
  informative <- (time >= horizon_days) | (event == 1L & time < horizon_days)
  if (!any(informative)) return(NA_real_)
  removed_before <- sum(event == 1L & time < horizon_days, na.rm = TRUE)
  1 - removed_before / sum(informative)
}

# ==============================================================================
# 5) WHO LAYER: Fence Posture
# ------------------------------------------------------------------------------
# Note the structural limit: GTA records only unilateral action. Bilateral,
# plurilateral, and multilateral agreed measures are out of scope, so the "gate"
# side of Gates & Fences is not observable here. This function measures fences.
# Keep the gate register as a separate hand-maintained table.
# ==============================================================================

neis_fence_posture <- function(alloc_long,
                               strength_col = "dis_v1_neutral",
                               group_cols = c("iso3", "country", "tech", "supply_chain")) {
  fams <- intersect(NEIS_ALL_FAMILIES, names(alloc_long))
  if (length(fams) == 0) stop("neis_fence_posture(): no fam_* columns found.")

  restrictive <- intersect(NEIS_RESTRICTIVE_FAMILIES, fams)
  conditional <- intersect(NEIS_CONDITIONAL_FAMILIES, fams)
  funding <- intersect(NEIS_FUNDING_FAMILIES, fams)

  # Build the group flags as plain vectors first. Doing this inside mutate()
  # with .data[[f]] under lapply() is fragile across dplyr versions.
  any_flag <- function(tbl, cols) {
    if (length(cols) == 0) return(rep(FALSE, nrow(tbl)))
    m <- vapply(cols, function(f) neis_bool(tbl[[f]]), logical(nrow(tbl)))
    if (is.matrix(m)) apply(m, 1, any) else as.logical(m)
  }

  d <- alloc_long
  d$.s <- dplyr::coalesce(as.numeric(d[[strength_col]]), 0)
  d$.restrictive  <- any_flag(d, restrictive)
  d$.conditional  <- any_flag(d, conditional)
  d$.funding      <- any_flag(d, funding)
  d$.localisation <- any_flag(d, intersect("fam_localisation_policy", fams))

  fam_counts <- d %>%
    tidyr::pivot_longer(dplyr::all_of(fams), names_to = "family", values_to = "flag") %>%
    dplyr::filter(neis_bool(.data$flag)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "family")))) %>%
    dplyr::summarise(fam_strength = sum(.data[[strength_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      instrument_entropy = neis_entropy(.data$fam_strength),
      instrument_hhi = neis_hhi(.data$fam_strength),
      n_families_used = dplyr::n(),
      .groups = "drop"
    )

  shares <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      total_strength = sum(.data$.s, na.rm = TRUE),
      fence_share = {
        t <- sum(.data$.s, na.rm = TRUE)
        if (t > 0) sum(.data$.s[.data$.restrictive], na.rm = TRUE) / t else NA_real_
      },
      conditional_share = {
        t <- sum(.data$.s, na.rm = TRUE)
        if (t > 0) sum(.data$.s[.data$.conditional], na.rm = TRUE) / t else NA_real_
      },
      funding_share = {
        t <- sum(.data$.s, na.rm = TRUE)
        if (t > 0) sum(.data$.s[.data$.funding], na.rm = TRUE) / t else NA_real_
      },
      localisation_share = {
        t <- sum(.data$.s, na.rm = TRUE)
        if (t > 0) sum(.data$.s[.data$.localisation], na.rm = TRUE) / t else NA_real_
      },
      .groups = "drop"
    )

  shares %>% dplyr::left_join(fam_counts, by = group_cols)
}

# ==============================================================================
# 6) PARTNER SYMMETRY: Inbound Exposure and Rival Direction
# ------------------------------------------------------------------------------
# Same dataset, inverted. For country c and capability k, how much of *other*
# jurisdictions' distortive strength lands on c. This is the framework's
# partner-side symmetry requirement as a data operation. It belongs in the
# security dimension, not the policy pillar.
# ==============================================================================

neis_inbound_exposure <- function(alloc_long,
                                  strength_col = "dis_v1_neutral",
                                  tech_cols = c("tech", "supply_chain"),
                                  distortive_only = TRUE) {
  aj <- neis_col(alloc_long, "affected_jurisdiction", required = TRUE)

  d <- alloc_long
  if (isTRUE(distortive_only) && "status_norm" %in% names(d)) {
    d <- dplyr::filter(d, .data$status_norm == "Distortive")
  }

  d %>%
    dplyr::mutate(
      affected_list = stringr::str_split(dplyr::coalesce(as.character(.data[[aj]]), ""),
                                         "\\s*,\\s*")
    ) %>%
    tidyr::unnest("affected_list") %>%
    dplyr::mutate(
      # Affected Jurisdiction carries RAW GTA names while `country` has already
      # been through standardize_country_names(). Without standardising here the
      # self-exclusion filter below silently fails to match, and so does the
      # downstream join onto by_tech_sc, which is keyed on the standardised name.
      affected_jurisdiction = standardize_country_names(
        stringr::str_squish(.data$affected_list)
      )
    ) %>%
    dplyr::filter(!is.na(.data$affected_jurisdiction),
                  nzchar(.data$affected_jurisdiction)) %>%
    # A measure's own implementer is not "exposure" for that implementer.
    dplyr::filter(.data$affected_jurisdiction != dplyr::coalesce(.data$country, "")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("affected_jurisdiction", tech_cols)))) %>%
    dplyr::summarise(
      inbound_strength = sum(.data[[strength_col]], na.rm = TRUE),
      inbound_n_measures = dplyr::n_distinct(.data$policy_id),
      inbound_n_sources = dplyr::n_distinct(.data$country),
      # Concentration of the exposure across source jurisdictions. High values
      # mean the exposure is a single-partner chokepoint rather than diffuse.
      inbound_source_hhi = {
        by_src <- tapply(.data[[strength_col]], .data$country, sum, na.rm = TRUE)
        neis_hhi(as.numeric(by_src))
      },
      .groups = "drop"
    ) %>%
    dplyr::rename(country = "affected_jurisdiction")
}

#' Rival-directedness: how concentrated a country's outbound restrictive
#' measures are on a small set of partners, rather than applied diffusely.
neis_rival_direction <- function(alloc_long,
                                 strength_col = "dis_v1_neutral",
                                 group_cols = c("iso3", "country", "tech", "supply_chain"),
                                 restrictive_only = TRUE) {
  aj <- neis_col(alloc_long, "affected_jurisdiction", required = TRUE)

  d <- alloc_long
  if (isTRUE(restrictive_only)) {
    restrictive <- intersect(NEIS_RESTRICTIVE_FAMILIES, names(d))
    if (length(restrictive) > 0) {
      m <- vapply(restrictive, function(f) neis_bool(d[[f]]), logical(nrow(d)))
      keep <- if (is.matrix(m)) apply(m, 1, any) else as.logical(m)
      d <- d[keep, , drop = FALSE]
    }
  }

  d %>%
    dplyr::mutate(
      affected_list = stringr::str_split(dplyr::coalesce(as.character(.data[[aj]]), ""),
                                         "\\s*,\\s*")
    ) %>%
    tidyr::unnest("affected_list") %>%
    # Standardised for the same reason as in neis_inbound_exposure(): partner
    # concentration would otherwise be computed over raw GTA spellings, counting
    # the same jurisdiction more than once under different names.
    dplyr::mutate(partner = standardize_country_names(
      stringr::str_squish(.data$affected_list)
    )) %>%
    dplyr::filter(!is.na(.data$partner), nzchar(.data$partner)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_partners_targeted = dplyr::n_distinct(.data$partner),
      partner_concentration_hhi = {
        by_p <- tapply(.data[[strength_col]], .data$partner, sum, na.rm = TRUE)
        neis_hhi(as.numeric(by_p))
      },
      top_partner = {
        by_p <- tapply(.data[[strength_col]], .data$partner, sum, na.rm = TRUE)
        if (length(by_p) == 0) NA_character_ else names(by_p)[which.max(by_p)]
      },
      .groups = "drop"
    )
}

# ==============================================================================
# 7) WHEN: Sequencing Position (leader vs follower)
# ------------------------------------------------------------------------------
# Share of a country's measures that arrive within window_days of another
# jurisdiction's measure on the same HS6 line. High values indicate reactive
# rather than autonomous strategy. Requires an exploded HS6 table.
# ==============================================================================

neis_sequencing_position <- function(policy_hs6_long,
                                     window_days = 365,
                                     hs6_col = "hs6",
                                     group_cols = c("iso3", "country")) {
  need <- c(hs6_col, "country", "impl_date", "policy_id")
  miss <- setdiff(need, names(policy_hs6_long))
  if (length(miss) > 0) {
    stop("neis_sequencing_position(): missing columns: ", paste(miss, collapse = ", "))
  }

  d <- policy_hs6_long %>%
    dplyr::mutate(impl_date = as_date_safe(.data$impl_date)) %>%
    dplyr::filter(!is.na(.data$impl_date), !is.na(.data[[hs6_col]]))

  # For each measure, the earliest foreign measure on the same HS6 within the
  # preceding window. Self-join is bounded by HS6 so it stays tractable.
  first_foreign <- d %>%
    dplyr::select(dplyr::all_of(c(hs6_col, "country", "impl_date", "policy_id"))) %>%
    dplyr::rename(other_country = "country",
                  other_date = "impl_date",
                  other_policy = "policy_id")

  joined <- d %>%
    dplyr::inner_join(first_foreign, by = hs6_col,
                      relationship = "many-to-many") %>%
    dplyr::filter(
      .data$other_country != .data$country,
      .data$other_date < .data$impl_date,
      as.numeric(.data$impl_date - .data$other_date) <= window_days
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "policy_id"))))

  totals <- d %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "policy_id")))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_cols)), name = "n_measures")

  followers <- joined %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_cols)), name = "n_following")

  totals %>%
    dplyr::left_join(followers, by = group_cols) %>%
    dplyr::mutate(
      n_following = dplyr::coalesce(.data$n_following, 0L),
      follow_rate = .data$n_following / .data$n_measures,
      lead_rate = 1 - .data$follow_rate,
      sequencing_window_days = window_days
    )
}

# ==============================================================================
# 8) EU consolidation
# ------------------------------------------------------------------------------
# EU-wide measures are recorded once; individual member-state measures are
# counted separately. Summing both double-counts. Choose one view explicitly.
# ==============================================================================

EU_MEMBER_ISO3 <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC",
  "HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK",
  "SVN","ESP","SWE"
)

#' @param mode "member_only" drops EU-wide rows; "eu_only" drops member rows;
#'   "both_flagged" keeps everything and adds eu_view for downstream filtering.
neis_consolidate_eu <- function(tbl, mode = c("both_flagged", "member_only", "eu_only"),
                                eu_iso3 = "EUU") {
  mode <- match.arg(mode)
  if (!"iso3" %in% names(tbl)) {
    warning("neis_consolidate_eu(): no iso3 column; returning input unchanged.")
    return(tbl)
  }

  out <- tbl %>%
    dplyr::mutate(
      eu_view = dplyr::case_when(
        .data$iso3 == eu_iso3 ~ "eu_wide",
        .data$iso3 %in% EU_MEMBER_ISO3 ~ "eu_member",
        TRUE ~ "non_eu"
      )
    )

  switch(
    mode,
    both_flagged = out,
    member_only = dplyr::filter(out, .data$eu_view != "eu_wide"),
    eu_only = dplyr::filter(out, .data$eu_view != "eu_member")
  )
}

# ==============================================================================
# 9) Panel assembly
# ------------------------------------------------------------------------------
# Joins the metric families side by side. Deliberately does NOT build a
# composite. Each index keeps its own column so a reader can see which layer is
# driving a posture judgement.
# ==============================================================================

neis_panel <- function(nipo_out,
                       as_of_date = NULL,
                       strength_col = "dis_v1_neutral",
                       conf_threshold = 0.75,
                       eu_mode = "both_flagged") {
  alloc <- nipo_out$internals$policy_alloc_tech_sc
  if (is.null(alloc)) {
    stop("neis_panel(): nipo_out$internals$policy_alloc_tech_sc not found. ",
         "Ensure nipo_policy_outputs() returned internals.")
  }

  if (is.null(as_of_date)) {
    as_of_date <- suppressWarnings(max(as_date_safe(alloc$as_of_date), na.rm = TRUE))
    if (!is.finite(as_of_date)) {
      stop("neis_panel(): could not infer as_of_date. Pass it explicitly.")
    }
  }

  alloc2 <- alloc %>%
    neis_attach_motives() %>%
    neis_strength_variants(conf_threshold = conf_threshold) %>%
    neis_consolidate_eu(mode = eu_mode)

  gc_ <- c("iso3", "country", "tech", "supply_chain")

  objective <- neis_objective_structure(alloc2, strength_col = strength_col, group_cols = gc_)
  delivery  <- neis_delivery_conversion(alloc2, as_of_date = as_of_date, group_cols = gc_)
  taper     <- neis_taper_profile(alloc2, as_of_date = as_of_date, group_cols = gc_)
  fence     <- neis_fence_posture(alloc2, strength_col = strength_col, group_cols = gc_)
  rival     <- tryCatch(
    neis_rival_direction(alloc2, strength_col = strength_col, group_cols = gc_),
    error = function(e) { warning("rival_direction skipped: ", conditionMessage(e)); NULL }
  )
  inbound   <- tryCatch(
    neis_inbound_exposure(alloc2, strength_col = strength_col),
    error = function(e) { warning("inbound_exposure skipped: ", conditionMessage(e)); NULL }
  )

  panel <- nipo_out$by_tech_sc %>%
    dplyr::left_join(objective, by = gc_) %>%
    dplyr::left_join(delivery, by = gc_) %>%
    dplyr::left_join(taper, by = gc_) %>%
    dplyr::left_join(fence, by = gc_)

  if (!is.null(rival)) panel <- dplyr::left_join(panel, rival, by = gc_)
  if (!is.null(inbound)) {
    panel <- dplyr::left_join(panel, inbound, by = c("country", "tech", "supply_chain"))
  }

  panel <- panel %>%
    dplyr::mutate(
      neis_as_of_date = as_of_date,
      neis_strength_variant = strength_col,
      # Structural caveats travel with the data, not the memo.
      caveat_no_standards = TRUE,   # GTA excludes TBT/SPS and product standards
      caveat_no_gates = TRUE,       # GTA excludes agreed bi/pluri/multilateral measures
      caveat_subsidy_value_coverage = "systematic from 2023 only"
    )

  list(
    panel = panel,
    stability = neis_variant_stability(alloc2),
    # TECH_KEYWORDS is already bounded at load time by nipo_policy_index.R, so
    # it is passed straight through. Re-binding would be a no-op (the helper
    # leaves regex-carrying terms alone) but implies it is not already fixed.
    keyword_audit = tryCatch(
      neis_audit_keywords(
        TECH_KEYWORDS,
        paste(dplyr::coalesce(alloc2$Title, ""),
              dplyr::coalesce(alloc2$source_text, ""), sep = " | ")
      ),
      error = function(e) NULL
    ),
    alloc_with_variants = alloc2
  )
}

# ==============================================================================
# 10) Self-test
# ==============================================================================

neis_selftest <- function() {
  stopifnot(abs(neis_entropy(c(1, 1, 1, 1)) - 1) < 1e-9)
  stopifnot(neis_entropy(c(10, 0, 0)) == 0)
  stopifnot(is.na(neis_hhi(c(5))))
  stopifnot(abs(neis_hhi(c(1, 1, 1, 1)) - 0) < 1e-9)
  stopifnot(abs(neis_hhi(c(1, 0, 0, 0)) - 1) < 1e-9)

  # Censoring-aware retention: 2 removed at 100d, 3 in force at 800d.
  t <- c(100, 100, 800, 800, 800); e <- c(1, 1, 0, 0, 0)
  stopifnot(abs(neis_retention_at(t, e, 365) - 3 / 5) < 1e-9)
  # Nothing removed -> KM median undefined, not 0.
  stopifnot(is.na(neis_km_median(c(500, 600), c(0, 0))))

  bd <- neis_bound_kws(c("ai", "\\bpv\\b", "cells?"))
  stopifnot(bd[1] == "\\bai\\b")
  stopifnot(bd[2] == "\\bpv\\b")   # already bounded, untouched
  stopifnot(bd[3] == "cells?")     # carries regex syntax, untouched

  message("neis_selftest(): all checks passed.")
  invisible(TRUE)
}
