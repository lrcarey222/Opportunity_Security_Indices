# Validation diagnostics for the trade refresh, the Secure routes, the decision quadrant, policy
# alignment and the partner recommendations (docs/strategic_position_readout.md). Each returns a
# table; scripts/26 writes them to diagnostics/strategic_position/. Pure: no IO.

# Counts of the momentum quality classes, overall and per country (for `countries`).
sp_diag_momentum_availability <- function(tbl, countries = NULL) {
  x <- tbl[tbl$supply_chain %in% c("Extraction", "Refining", "Components", "Assembly"), , drop = FALSE]
  overall <- x %>% dplyr::count(.data$trade_momentum_quality, name = "n_cells") %>% dplyr::mutate(Country = "(all)")
  per <- if (length(countries)) {
    x %>% dplyr::filter(.data$Country %in% countries) %>% dplyr::count(.data$Country, .data$trade_momentum_quality, name = "n_cells")
  } else NULL
  dplyr::bind_rows(overall, per) %>% dplyr::select("Country", "trade_momentum_quality", "n_cells")
}

# The cells whose Competitive Position percentile moves most when momentum enters (structural only vs
# blended), with the momentum behind the move.
sp_diag_momentum_movers <- function(structural, blended, tm_cells, iso3_of, n = 50) {
  s <- structural %>% dplyr::select("Country", "tech", "supply_chain", cp_structural_pct = "competitive_position_percentile")
  b <- blended %>% dplyr::select("Country", "tech", "supply_chain", cp_blended_pct = "competitive_position_percentile", "trade_momentum_weight")
  x <- dplyr::inner_join(s, b, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::mutate(iso3 = iso3_of(.data$Country), change = .data$cp_blended_pct - .data$cp_structural_pct) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::left_join(tm_cells %>% dplyr::select("iso3", "tech", "supply_chain", "trade_momentum_raw", "trade_momentum_specialization",
                                                "trade_momentum_score", "trade_exports_previous", "trade_exports_current"),
                     by = c("iso3", "tech", "supply_chain"))
  x %>% dplyr::arrange(dplyr::desc(abs(.data$change))) %>% utils::head(n)
}

# Momentum weight sensitivity: for each weight, Competitive Position against structural only and
# against the configured weight. rank_corr is the median over cells of the Spearman correlation of the
# percentiles; movers_5 / movers_10 count cells moving more than 5 / 10 percentile points. With tbl
# (the final cell table) and cfg (strategic_posture), the quadrant is re-run on each weight's
# Competitive Position: posture_changes, flag_changes and confidence_changes against tbl.
sp_diag_momentum_weights <- function(structural, tm_cells, iso3_of, families, weights, base_weight, tbl = NULL, cfg = list()) {
  rerun <- function(bl) {
    if (is.null(tbl)) return(c(NA, NA, NA))
    alt <- tbl %>%
      dplyr::select(-"competitive_position_percentile") %>%
      dplyr::left_join(bl %>% dplyr::select("Country", "tech", "supply_chain", "competitive_position_percentile"), by = c("Country", "tech", "supply_chain"))
    alt <- sp_decision_quadrant(alt, cfg)
    c(sum(!mapply(identical, alt$strategic_posture, tbl$strategic_posture)),
      sum(!mapply(identical, alt$quadrant_consistency_flag, tbl$quadrant_consistency_flag)),
      sum(!mapply(identical, alt$posture_confidence, tbl$posture_confidence)))
  }
  base <- sp_cp_blend_momentum(structural, tm_cells, iso3_of, families, weight = base_weight)
  cmp <- function(a, b) {
    x <- dplyr::inner_join(a %>% dplyr::select("Country", "tech", "supply_chain", pa = "competitive_position_percentile"),
                           b %>% dplyr::select("Country", "tech", "supply_chain", pb = "competitive_position_percentile"),
                           by = c("Country", "tech", "supply_chain")) %>% dplyr::filter(!is.na(.data$pa), !is.na(.data$pb))
    rc <- x %>% dplyr::group_by(.data$tech, .data$supply_chain) %>%
      dplyr::summarise(r = if (dplyr::n() >= 5) suppressWarnings(stats::cor(.data$pa, .data$pb, method = "spearman")) else NA_real_, .groups = "drop")
    list(rank_corr = stats::median(rc$r, na.rm = TRUE), movers_5 = sum(abs(x$pa - x$pb) > 0.05), movers_10 = sum(abs(x$pa - x$pb) > 0.10), n = nrow(x))
  }
  dplyr::bind_rows(lapply(weights, function(w) {
    bl <- sp_cp_blend_momentum(structural, tm_cells, iso3_of, families, weight = w)
    v0 <- cmp(structural, bl)
    vb <- cmp(base, bl)
    ch <- rerun(bl)
    data.frame(weight = w, n_cells = v0$n, n_blended = sum(bl$trade_momentum_weight > 0),
               rank_corr_vs_structural = v0$rank_corr, movers_5_vs_structural = v0$movers_5, movers_10_vs_structural = v0$movers_10,
               rank_corr_vs_base = vb$rank_corr, movers_5_vs_base = vb$movers_5, movers_10_vs_base = vb$movers_10,
               posture_changes_vs_base = ch[1], flag_changes_vs_base = ch[2], confidence_changes_vs_base = ch[3])
  }))
}

# Price effects in mineral and fuel cells: the common (median) log export growth across exporters and
# the gap between median value growth and median net-weight growth. A within-cell rank ignores a
# common move, so a large common component explains why raw growth is not used directly.
sp_diag_momentum_prices <- function(tm_cells, fuel_techs = c("Coal", "Gas", "Oil"), mineral_stages = c("Extraction", "Refining")) {
  tm_cells %>%
    dplyr::filter(.data$tech %in% fuel_techs | .data$supply_chain %in% mineral_stages) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_valid = sum(!is.na(.data$trade_momentum_raw)),
      median_log_growth = stats::median(.data$trade_momentum_raw, na.rm = TRUE),
      iqr_log_growth = stats::IQR(.data$trade_momentum_raw, na.rm = TRUE),
      value_volume_gap = dplyr::first(stats::na.omit(.data$trade_value_volume_gap)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(price_driven = !is.na(.data$value_volume_gap) & abs(.data$value_volume_gap) > 0.2)
}

# Secure before and after the routes, by `by` (old and new are cell tables with strategic_posture;
# new carries security_rationale).
sp_diag_secure_compare <- function(old, new, by, countries = NULL) {
  key <- c("iso3", "tech", "supply_chain")
  x <- dplyr::full_join(old %>% dplyr::transmute(!!!rlang::syms(key), Country_old = .data$Country, secure_old = .data$strategic_posture %in% "secure"),
                        new %>% dplyr::transmute(!!!rlang::syms(key), .data$Country, secure_new = .data$strategic_posture %in% "secure",
                                                 .data$security_rationale), by = key) %>%
    dplyr::mutate(Country = dplyr::coalesce(.data$Country, .data$Country_old),
                  secure_old = dplyr::coalesce(.data$secure_old, FALSE), secure_new = dplyr::coalesce(.data$secure_new, FALSE))
  if (length(countries)) x <- x[x$Country %in% countries, , drop = FALSE]
  # flags first: summarise() would otherwise read the counts it has just made
  x %>% dplyr::mutate(.added = .data$secure_new & !.data$secure_old, .removed = .data$secure_old & !.data$secure_new,
                      .oc = .data$secure_new & .data$security_rationale %in% "operational_control",
                      .cc = .data$secure_new & .data$security_rationale %in% "coercive_chokepoint",
                      .sr = .data$secure_new & .data$security_rationale %in% "systemic_resilience") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(secure_old = sum(.data$secure_old), secure_new = sum(.data$secure_new), added = sum(.data$.added),
                     removed = sum(.data$.removed), operational_control = sum(.data$.oc), coercive_chokepoint = sum(.data$.cc),
                     systemic_resilience = sum(.data$.sr), .groups = "drop")
}

# Postures, quadrants and Compete modes by `by`.
sp_diag_quadrant_distribution <- function(tbl, by) {
  tbl %>% dplyr::filter(!is.na(.data$strategic_posture)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      n_cells = dplyr::n(), secure = sum(.data$strategic_posture == "secure"),
      catch_up = sum(.data$compete_mode %in% "catch_up"), preserve = sum(.data$compete_mode %in% "preserve"),
      partner = sum(.data$compete_mode %in% "partner"),
      deploy_watch = sum(.data$strategic_posture == "deploy" & .data$decision_quadrant %in% "watch"),
      deploy_other = sum(.data$strategic_posture == "deploy" & !.data$decision_quadrant %in% "watch"),
      borderline = sum(!is.na(.data$quadrant_borderline) & .data$strategic_posture != "secure"),
      consistency_flags = sum(!is.na(.data$quadrant_consistency_flag)),
      partner_gaps = sum(.data$partner_gap %in% TRUE),
      .groups = "drop"
    )
}

# Up to n examples per country and quadrant (the clearest: farthest from both thresholds).
sp_diag_quadrant_examples <- function(tbl, countries, need_t = 0.67, opp_t = 0.67, n = 3) {
  tbl %>%
    dplyr::filter(.data$Country %in% countries, !is.na(.data$compete_mode) | .data$decision_quadrant %in% "watch" | .data$strategic_posture %in% "secure") %>%
    dplyr::mutate(group = ifelse(.data$strategic_posture == "secure", paste0("secure_", .data$security_rationale), .data$decision_quadrant),
                  clarity = abs(.data$strategic_need_percentile - need_t) + abs(.data$opportunity_percentile - opp_t)) %>%
    dplyr::group_by(.data$Country, .data$group) %>%
    dplyr::arrange(dplyr::desc(.data$clarity), .by_group = TRUE) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::ungroup() %>%
    dplyr::select("Country", "group", "tech", "supply_chain", "strategic_posture", "posture_variant", "strategic_need_percentile",
                  "opportunity_percentile", "competitive_position_percentile", "home_share", "quadrant_consistency_flag",
                  "posture_confidence", "recommended_partner_1", "partner_gap", "policy_alignment")
}

# The quadrant at alternative thresholds (both axes at each value): cells per posture and quadrant and
# how many change posture against the configured thresholds.
sp_diag_quadrant_thresholds <- function(tbl, thresholds, base_need = 0.67, base_opp = 0.67, stages = setdiff(SUPPLY_CHAIN_STAGES, "Deployment")) {
  x <- tbl[!is.na(tbl$strategic_posture) & tbl$strategic_posture != "secure" & tbl$supply_chain %in% stages & is.na(tbl$posture_exclusion), , drop = FALSE]
  base <- sp_quadrant_posture(sp_quadrant_of(x$strategic_need_percentile, x$opportunity_percentile, base_need, base_opp))
  dplyr::bind_rows(lapply(thresholds, function(t) {
    q <- sp_quadrant_of(x$strategic_need_percentile, x$opportunity_percentile, t, t)
    p <- sp_quadrant_posture(q)
    data.frame(threshold = t, n_cells = nrow(x), catch_up = sum(q == "catch_up"), preserve = sum(q == "preserve"),
               partner = sum(q == "partner"), watch = sum(q == "watch"), insufficient_data = sum(q == "insufficient_data"),
               compete = sum(p == "compete"), deploy = sum(p == "deploy"), posture_changed_vs_base = sum(p != base))
  }))
}

# Policy alignment by posture and Compete mode, with the rule flags.
sp_diag_alignment_summary <- function(tbl) {
  tbl %>% dplyr::filter(!is.na(.data$strategic_posture)) %>%
    dplyr::mutate(mode = dplyr::coalesce(.data$compete_mode, ifelse(.data$decision_quadrant %in% "watch" & .data$strategic_posture == "deploy", "watch", NA_character_))) %>%
    dplyr::count(.data$strategic_posture, .data$mode, .data$policy_alignment, .data$policy_alignment_flag, name = "n_cells")
}

# Partner recommendation sanity checks. pairs: sp_coercive_pairs(). Returns one row per check with the
# number of violations (all should be 0 except the descriptive rows).
sp_diag_partner_sanity <- function(tbl, pairs, members) {
  p <- tbl[tbl$compete_mode %in% "partner", , drop = FALSE]
  long <- dplyr::bind_rows(lapply(1:3, function(k) {
    data.frame(iso3 = p$iso3, tech = p$tech, supply_chain = p$supply_chain, k = k, partner = p[[paste0("recommended_partner_", k)]],
               capability = p[[paste0("recommended_partner_", k, "_capability")]], stringsAsFactors = FALSE)
  })) %>% dplyr::filter(!is.na(.data$partner))
  coer <- paste(pairs$reporter_iso[pairs$coercive %in% TRUE], pairs$partner_iso[pairs$coercive %in% TRUE])
  untrusted <- paste(pairs$reporter_iso[!pairs$trusted %in% TRUE], pairs$partner_iso[!pairs$trusted %in% TRUE])
  data.frame(
    check = c("partner cells", "partner cells with a gap", "recommendations", "recommended partner is the country itself",
              "recommended partner is coercive to the country", "recommended partner is not trusted by the country",
              "recommended partner below the presence line", "first recommendation is a coalition member", "distinct first recommendations"),
    value = c(nrow(p), sum(p$partner_gap %in% TRUE), nrow(long), sum(long$partner == long$iso3),
              sum(paste(long$iso3, long$partner) %in% coer), sum(paste(long$iso3, long$partner) %in% untrusted),
              sum(long$capability < 0.02, na.rm = TRUE), sum(p$recommended_partner_1 %in% members, na.rm = TRUE),
              length(unique(stats::na.omit(p$recommended_partner_1))))
  )
}

# Old (committed engine) vs new posture transitions.
sp_diag_posture_transitions <- function(old, new) {
  key <- c("iso3", "tech", "supply_chain")
  dplyr::inner_join(old %>% dplyr::select(dplyr::all_of(key), old_posture = "strategic_posture", old_variant = "posture_variant"),
                    new %>% dplyr::select(dplyr::all_of(key), new_posture = "strategic_posture", new_variant = "posture_variant",
                                          "decision_quadrant"), by = key) %>%
    dplyr::count(.data$old_posture, .data$new_posture, .data$decision_quadrant, name = "n_cells")
}
