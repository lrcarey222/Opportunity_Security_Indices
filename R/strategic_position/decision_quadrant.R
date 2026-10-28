# The decision quadrant inside the medium yard (docs/strategic_position.md, section 7.3). After the
# Secure override, the recommended posture of a producing-stage node comes from two axes:
#
#   Strategic Need          strategic_need_percentile (across countries within the cell)
#   Industrial Opportunity  the EO index placed among the node's contenders (opportunity_percentile)
#
#                        opportunity high        opportunity low
#     need high          COMPETE catch_up        COMPETE partner
#     need low           COMPETE preserve        DEPLOY  (quadrant watch)
#
# There is no new score: the quadrant is a pair of threshold tests on the two existing axes, and the
# thresholds are config (strategic_posture.decision_quadrant), tested at 0.60 / 0.67 / 0.70. Cells
# within the borderline band of a threshold are flagged and lose confidence. Deployment keeps the
# engine's posture (Deploy unless Secure): its quadrant is reported for information only.
# Competitive Position is a consistency check, never an override: catch_up where the country already
# leads the world, preserve without a capability and partner despite a strong one are flagged.
#
# Pure: no IO. Needs posture_engine.R (sp_delivery_mode, SP_POSTURES) and sp_helpers.R.

SP_QUADRANTS <- c("catch_up", "preserve", "partner", "watch", "insufficient_data")
SP_COMPETE_MODES <- c("catch_up", "preserve", "partner")
SP_QUADRANT_FLAGS <- c("catch_up_already_leading", "preserve_without_capability", "partner_despite_capability")

# Rank of each value among a reference set: (reference values below + half those equal) / n.
sp_ref_pct <- function(v, ref) {
  ref <- ref[!is.na(ref)]
  n <- length(ref)
  if (n == 0) return(rep(NA_real_, length(v)))
  vapply(v, function(z) if (is.na(z)) NA_real_ else (sum(ref < z) + 0.5 * sum(ref == z)) / n, numeric(1))
}

# Industrial Opportunity, placed among the node's contenders. The EO index is compressed and its
# category mix differs by technology, so neither its absolute value nor a percentile across all
# countries and technologies says whether an opportunity is high. Within a cell, the reference set is
# the economies with a real presence in the node (home_share >= contender_min_share); where fewer than
# min_contenders exist (and at Deployment, which has no supply share), the peer set (coalition members
# and large economies). Technologies whose world market is declining (market_outlook in cap_outlooks)
# are capped below the thresholds: a shrinking market is not an industrial opportunity.
#
# eo: iso3, tech, supply_chain, industrial_opportunity (the EO index; NA = not rankable).
# presence: iso3, tech, supply_chain, home_share. peers: ISO3 codes. outlook: named character, tech ->
# market outlook. Returns iso3 x tech x supply_chain with industrial_opportunity,
# opportunity_percentile, opportunity_basis (contenders / peers / none), opportunity_n_ref,
# opportunity_capped.
sp_opportunity_axis <- function(eo, presence, peers, cfg = list(), outlook = NULL) {
  min_share <- cfg$contender_min_share %||% 0.005
  min_n <- as.integer(cfg$min_contenders %||% 5L)
  caps <- unlist(cfg$cap_outlooks %||% "declining")
  cap_pct <- cfg$cap_percentile %||% 0.5
  x <- eo %>%
    dplyr::left_join(presence[, c("iso3", "tech", "supply_chain", "home_share")], by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::mutate(.contender = !is.na(.data$home_share) & .data$home_share >= min_share & !is.na(.data$industrial_opportunity),
                  .peer = .data$iso3 %in% peers & !is.na(.data$industrial_opportunity))
  x <- x %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      opportunity_basis = if (sum(.data$.contender) >= min_n) "contenders" else if (sum(.data$.peer) >= min_n) "peers" else "none",
      opportunity_n_ref = dplyr::case_when(
        dplyr::first(.data$opportunity_basis) == "contenders" ~ sum(.data$.contender),
        dplyr::first(.data$opportunity_basis) == "peers" ~ sum(.data$.peer),
        TRUE ~ 0L
      ),
      opportunity_percentile = {
        b <- dplyr::first(.data$opportunity_basis)
        ref <- if (b == "contenders") .data$industrial_opportunity[.data$.contender] else if (b == "peers") .data$industrial_opportunity[.data$.peer] else numeric()
        if (b == "none") rep(NA_real_, dplyr::n()) else pmin(pmax(sp_ref_pct(.data$industrial_opportunity, ref), 0), 1)
      }
    ) %>%
    dplyr::ungroup()
  out_look <- if (is.null(outlook)) rep(NA_character_, nrow(x)) else unname(outlook[x$tech])
  x$opportunity_capped <- out_look %in% caps & !is.na(x$opportunity_percentile) & x$opportunity_percentile > cap_pct
  x$opportunity_percentile <- ifelse(x$opportunity_capped, cap_pct, x$opportunity_percentile)
  x$opportunity_basis[is.na(x$industrial_opportunity)] <- "none"
  x[, c("iso3", "tech", "supply_chain", "industrial_opportunity", "opportunity_percentile", "opportunity_basis",
        "opportunity_n_ref", "opportunity_capped")]
}

# The quadrant of each node from the two axes (no posture logic).
sp_quadrant_of <- function(need, opp, need_t, opp_t) {
  dplyr::case_when(
    is.na(need) | is.na(opp) ~ "insufficient_data",
    need >= need_t & opp >= opp_t ~ "catch_up",
    need < need_t & opp >= opp_t ~ "preserve",
    need >= need_t & opp < opp_t ~ "partner",
    TRUE ~ "watch"
  )
}
sp_quadrant_posture <- function(q) ifelse(q %in% SP_COMPETE_MODES, "compete", "deploy")

# Apply the quadrant to the cell table after the engine. tbl: one row per country x tech x six-stage
# cell with the engine columns (strategic_posture, posture_variant, posture_possible, pivotal_inputs,
# posture_confidence, decision_trace, delivery_mode, posture_primary_rationale, compete_strategy,
# posture_exclusion), strategic_need_percentile, opportunity_percentile, opportunity_basis,
# competitive_position_percentile, home_share, trusted_world_share, coercive_world_share,
# coercive_top_share, import_hhi, imports_usd and stage_evidence_quality_cp. cfg: the strategic_posture
# block (decision_quadrant, delivery, concentration_watch).
#
# Adds decision_quadrant, compete_mode, need_high, opportunity_high, quadrant_borderline,
# quadrant_consistency_flag, engine_posture and engine_variant (the engine's own Compete reading, kept
# for comparison), and rewrites the posture columns of the nodes the quadrant decides.
sp_decision_quadrant <- function(tbl, cfg = list()) {
  qc <- cfg$decision_quadrant %||% list()
  need_t <- qc$need_threshold %||% 0.67
  opp_t <- qc$opportunity_threshold %||% 0.67
  band <- qc$borderline_band %||% 0.05
  stages <- unlist(qc$apply_stages %||% setdiff(SUPPLY_CHAIN_STAGES, "Deployment"))
  cons <- qc$consistency %||% list()
  lead_pct <- cons$leading_cp_pct %||% 0.9
  pres_pct <- cons$preserve_min_cp_pct %||% 0.25
  pres_share <- cons$preserve_min_share %||% 0.005
  part_pct <- cons$partner_strong_cp_pct %||% 0.75
  watch <- cfg$concentration_watch %||% list(coercive_share = 0.4, import_hhi = 0.18)
  col <- function(n, default = NA) if (n %in% names(tbl)) tbl[[n]] else rep(default, nrow(tbl))

  need <- col("strategic_need_percentile", NA_real_)
  opp <- col("opportunity_percentile", NA_real_)
  cp <- col("competitive_position_percentile", NA_real_)
  home <- col("home_share", NA_real_)
  q <- sp_quadrant_of(need, opp, need_t, opp_t)
  tbl$decision_quadrant <- ifelse(is.na(tbl$strategic_posture), NA_character_, q)
  tbl$need_high <- ifelse(is.na(need), NA, need >= need_t)
  tbl$opportunity_high <- ifelse(is.na(opp), NA, opp >= opp_t)
  near_need <- !is.na(need) & abs(need - need_t) < band
  near_opp <- !is.na(opp) & abs(opp - opp_t) < band
  tbl$quadrant_borderline <- dplyr::case_when(
    near_need & near_opp ~ "both", near_need ~ "need", near_opp ~ "opportunity", TRUE ~ NA_character_
  )
  tbl$engine_posture <- tbl$strategic_posture
  tbl$engine_variant <- tbl$posture_variant

  decides <- !is.na(tbl$strategic_posture) & tbl$strategic_posture != "secure" & tbl$supply_chain %in% stages &
    is.na(col("posture_exclusion", NA_character_))
  new_posture <- sp_quadrant_posture(q)
  mode <- ifelse(decides & new_posture == "compete", q, NA_character_)
  # Deploy variants: the engine's when it already read Deploy; otherwise the concentration watch.
  material <- !is.na(col("imports_usd", NA_real_)) & col("imports_usd", NA_real_) >= (cfg$exposure_min_import_usd %||% 1e6)
  diversify <- (!is.na(col("coercive_top_share", NA_real_)) & col("coercive_top_share", NA_real_) >= watch$coercive_share) |
    (!is.na(col("import_hhi", NA_real_)) & col("import_hhi", NA_real_) >= watch$import_hhi & material)
  deploy_variant <- ifelse(grepl("^deploy", tbl$posture_variant), tbl$posture_variant, ifelse(diversify, "deploy_diversify", "deploy"))
  variant <- ifelse(new_posture == "compete", paste0("compete_", q), deploy_variant)

  # Competitive Position as a consistency check.
  flag <- dplyr::case_when(
    mode %in% "catch_up" & !is.na(cp) & cp >= lead_pct ~ "catch_up_already_leading",
    mode %in% "preserve" & ((!is.na(cp) & cp < pres_pct) | (!is.na(home) & home < pres_share)) ~ "preserve_without_capability",
    mode %in% "partner" & !is.na(cp) & cp >= part_pct ~ "partner_despite_capability",
    TRUE ~ NA_character_
  )
  tbl$quadrant_consistency_flag <- ifelse(decides, flag, NA_character_)

  # What the unknowns and the borderline allow: Secure where the engine still allows it, and the
  # posture of the neighbouring quadrant across a borderline threshold.
  eng_possible <- col("posture_possible", NA_character_)
  alt_need <- ifelse(near_need, sp_quadrant_posture(sp_quadrant_of(ifelse(need >= need_t, need_t - 1, need_t), opp, need_t, opp_t)), NA_character_)
  alt_opp <- ifelse(near_opp, sp_quadrant_posture(sp_quadrant_of(need, ifelse(opp >= opp_t, opp_t - 1, opp_t), need_t, opp_t)), NA_character_)
  possible <- vapply(seq_len(nrow(tbl)), function(i) {
    p <- c(new_posture[i], alt_need[i], alt_opp[i], if (grepl("secure", eng_possible[i])) "secure")
    paste(SP_POSTURES[SP_POSTURES %in% p], collapse = "|")
  }, character(1))
  piv_add <- paste0(ifelse(!is.na(alt_need) & alt_need != new_posture, "strategic_need_percentile;", ""),
                    ifelse(!is.na(alt_opp) & alt_opp != new_posture, "opportunity_percentile;", ""))
  piv_add <- sub(";$", "", piv_add)
  eng_piv <- col("pivotal_inputs", NA_character_)
  # security inputs stay pivotal only if they could still open Secure
  sec_piv <- ifelse(grepl("secure", eng_possible), eng_piv, NA_character_)
  pivotal <- ifelse(nzchar(piv_add), ifelse(is.na(sec_piv), piv_add, paste(sec_piv, piv_add, sep = ";")), sec_piv)

  ev_low <- col("stage_evidence_quality_cp", NA_character_) %in% c("low", "none")
  issues <- (near_need + near_opp) + grepl("secure", eng_possible) + (col("opportunity_basis", NA_character_) %in% "peers") +
    !is.na(flag) + ev_low
  confidence <- dplyr::case_when(q == "insufficient_data" ~ "low", issues == 0 ~ "high", issues == 1 ~ "moderate", TRUE ~ "low")

  tbl$strategic_posture <- ifelse(decides, new_posture, tbl$strategic_posture)
  tbl$posture_variant <- ifelse(decides, variant, tbl$posture_variant)
  tbl$compete_mode <- mode
  tbl$compete_strategy <- ifelse(decides, mode, tbl$compete_strategy)
  tbl$posture_possible <- ifelse(decides, possible, tbl$posture_possible)
  tbl$pivotal_inputs <- ifelse(decides, pivotal, tbl$pivotal_inputs)
  tbl$posture_confidence <- ifelse(decides, confidence, tbl$posture_confidence)
  tbl$posture_primary_rationale <- ifelse(
    decides, c(catch_up = "capability", preserve = "capability", partner = "need", watch = "openness", insufficient_data = "openness")[q],
    tbl$posture_primary_rationale
  )
  if ("posture_secondary_rationale" %in% names(tbl)) {
    tbl$posture_secondary_rationale <- ifelse(decides & tbl$engine_posture != tbl$strategic_posture, NA_character_, tbl$posture_secondary_rationale)
  }
  dm <- sp_delivery_mode(tbl, tbl$strategic_posture, tbl$posture_variant, tbl$compete_strategy, cfg)
  tbl$delivery_mode <- ifelse(decides, dm, tbl$delivery_mode)
  if ("decision_trace" %in% names(tbl)) {
    tbl$decision_trace <- sp_quadrant_trace(tbl$decision_trace, decides, q, need, opp, need_t, opp_t, tbl$quadrant_borderline,
                                            flag, tbl$strategic_posture, tbl$posture_variant, tbl$posture_possible, tbl$pivotal_inputs,
                                            col("opportunity_basis", NA_character_))
  }
  tbl
}

# The final posture without corners, pivotal inputs or trace (for sensitivity runs): the engine's
# strict Secure, else the quadrant at the producing stages. x: the engine input frame with
# strategic_need_percentile and opportunity_percentile.
sp_final_posture_strict <- function(x, cfg = list()) {
  p <- sp_posture_strict(x, cfg)
  qc <- cfg$decision_quadrant %||% list()
  stages <- unlist(qc$apply_stages %||% setdiff(SUPPLY_CHAIN_STAGES, "Deployment"))
  need <- if ("strategic_need_percentile" %in% names(x)) x$strategic_need_percentile else rep(NA_real_, nrow(x))
  opp <- if ("opportunity_percentile" %in% names(x)) x$opportunity_percentile else rep(NA_real_, nrow(x))
  q <- sp_quadrant_of(need, opp, qc$need_threshold %||% 0.67, qc$opportunity_threshold %||% 0.67)
  ifelse(is.na(p) | p == "secure" | !x$supply_chain %in% stages, p, sp_quadrant_posture(q))
}

# Add the quadrant test to the engine's trace and restate the posture it ends on.
sp_quadrant_trace <- function(trace, decides, q, need, opp, need_t, opp_t, borderline, flag, posture, variant, possible, pivotal, basis) {
  num <- function(v) ifelse(is.na(v), "null", format(round(v, 3), nsmall = 3, trim = TRUE))
  arr <- function(s, sep) vapply(strsplit(ifelse(is.na(s), "", s), sep, fixed = TRUE), function(p) {
    if (length(p) == 0) "[]" else paste0("[", paste0("\"", p, "\"", collapse = ","), "]")
  }, character(1))
  detail <- dplyr::case_when(
    !decides ~ "The quadrant does not decide this node (Secure, Deployment or not classified); shown for information.",
    q == "insufficient_data" ~ "Strategic Need or Industrial Opportunity is not measured: no case for intervention is established.",
    q == "catch_up" ~ "High need and high opportunity: compete to catch up.",
    q == "preserve" ~ "High opportunity without high need: compete to preserve and grow an advantage.",
    q == "partner" ~ "High need without high opportunity: compete by partnering with trusted suppliers.",
    TRUE ~ "Neither need nor opportunity is high: deploy openly and watch."
  )
  test <- paste0(
    "{\"id\":\"quadrant\",\"state\":", sp_json_str(q), ",\"marginal\":", ifelse(is.na(borderline), "false", "true"),
    ",\"detail\":", sp_json_str(detail),
    ",\"inputs\":[{\"id\":\"strategic_need_percentile\",\"value\":", num(need), ",\"threshold\":", num(need_t), ",\"provenance\":\"data\"},",
    "{\"id\":\"opportunity_percentile\",\"value\":", num(opp), ",\"threshold\":", num(opp_t), ",\"provenance\":", sp_json_str(paste0("data:", basis)), "}]",
    ",\"consistency_flag\":", sp_json_str(flag), "}"
  )
  trace <- ifelse(is.na(trace), NA_character_, trace)
  head <- sub("\\],\"posture\":\"[^\"]*\",\"variant\":\"[^\"]*\",\"possible\":\\[[^]]*\\],\"pivotal\":\\[[^]]*\\]\\}$", "", trace)
  ifelse(is.na(trace), NA_character_, paste0(
    head, ",", test, "],\"posture\":", sp_json_str(posture), ",\"variant\":", sp_json_str(variant),
    ",\"possible\":", arr(possible, "|"), ",\"pivotal\":", arr(pivotal, ";"), "}"
  ))
}
