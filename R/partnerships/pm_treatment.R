# Partnership Memos: treatment, constraint type, instrument choice and the separate decision fields
# (docs/partnership_memos.md, "Treatments and instruments" and "Fields").
#
# No single partnership score. Each field is 0-1, built from named components by a documented rule (the
# weaker leg of a pairing = min; a mean only where the components are interchangeable signals). Priority is
# lexicographic over the fields, never a weighted sum.
#
# Pure functions.

PM_TREATMENT <- c(capacity_constrained_trusted_supply = "BUILD", shared_vulnerability = "BUILD",
                  a_supplies_b_needs = "COORDINATE", b_supplies_a_needs = "COORDINATE",
                  complementary_specialization = "COORDINATE", direct_competition = "MANAGE SPECIALIZATION",
                  low_strategic_case = "LOW INTERVENTION", deployment_demand = "DEMAND CONTEXT", not_assessed = "INSUFFICIENT DATA")

PM_FAILURE <- c(
  geopolitical_chokepoint = "Security externality: a coercive supplier holds most world supply; buyers do not price the risk of a cut-off, so trusted capacity is under-built.",
  trusted_partner_capacity_constraint = "Coordination failure: trusted producers will not add capacity without long-term demand certainty, and buyers will not commit without capacity.",
  strategic_manufacturing_gap = "Capability gap: the buyer has a strategic need but no domestic or trusted base, and the market prize alone does not attract one.",
  emerging_bottleneck = "Lead-time risk: deployment demand is growing faster than trusted capacity is being added.",
  no_material_intervention = "No market failure identified: trusted supply is adequate or the two countries are commercial competitors.")
PM_FAILURE_SCORE <- c(geopolitical_chokepoint = 1, trusted_partner_capacity_constraint = 0.75, strategic_manufacturing_gap = 0.5,
                      emerging_bottleneck = 0.5, no_material_intervention = 0)

pm_constraint <- function(d, cfg) {
  dn_hi <- cfg$thresholds$deployment_need_high
  emerging <- (pmax(d$deployment_need_a, d$deployment_need_b, na.rm = TRUE) >= dn_hi) & !is.na(d$world_top_share) & d$world_top_share >= 0.4
  emerging[is.na(emerging)] <- FALSE
  supply_state <- d$relationship_state %in% c("a_supplies_b_needs", "b_supplies_a_needs")
  manufacturing <- d$stage %in% c("Components", "Assembly") | (pmax(d$prize_a, d$prize_b, na.rm = TRUE) >= cfg$thresholds$prize_high) %in% TRUE
  primary <- ifelse(d$relationship_state %in% c("not_assessed", "deployment_demand"), NA_character_,
              ifelse(supply_state & !d$chokepoint & !d$allied_short & !emerging & !manufacturing, "no_material_intervention",
              ifelse(d$relationship_state == "low_strategic_case" | (d$relationship_state == "direct_competition" & !(d$chokepoint | d$allied_short)),
                     "no_material_intervention",
              ifelse(d$chokepoint, "geopolitical_chokepoint",
              ifelse(d$relationship_state == "capacity_constrained_trusted_supply" | d$allied_short, "trusted_partner_capacity_constraint",
              ifelse(emerging, "emerging_bottleneck", "strategic_manufacturing_gap"))))))
  secondary <- ifelse(primary == "geopolitical_chokepoint" & d$allied_short, "trusted_partner_capacity_constraint",
               ifelse(primary %in% c("geopolitical_chokepoint", "trusted_partner_capacity_constraint", "strategic_manufacturing_gap") & emerging, "emerging_bottleneck", NA_character_))
  d$constraint_type <- primary
  d$constraint_secondary <- secondary
  d$market_failure <- ifelse(is.na(primary), NA_character_, unname(PM_FAILURE[primary]))
  d
}

# Evidence rows touching a unit: the pair's countries, then the node (exact) or compact + stage (near).
pm_match_evidence <- function(d, ev, countries) {
  ev$cty <- strsplit(ev$countries, "\\|")
  both <- vapply(ev$cty, function(z) all(countries %in% z), logical(1))
  lapply(seq_len(nrow(d)), function(i) {
    hit_node <- !is.na(d$node_id[i]) & nzchar(ev$aib_node) & ev$aib_node == d$node_id[i]
    if (d$unit_type[i] == "cell") hit_node <- nzchar(ev$aib_node) & ev$aib_node %in% d$nodes_of_cell[[i]]
    cmp <- if (d$unit_type[i] == "node") d$compact[i] else d$primary_compact[i]
    hit_cmp <- !is.na(cmp) & ev$compact %in% cmp & ev$stage == d$stage[i]
    ev$evidence_id[both & (hit_node | hit_cmp)]
  })
}

pm_commitment <- function(ids, ev) {
  vapply(ids, function(z) {
    s <- ev$status[ev$evidence_id %in% z & ev$item_type %in% c("project", "financing", "corporate")]
    if (any(s %in% c("under_construction", "operating", "signed", "in_force"))) 1 else if (any(s %in% c("announced", "proposed"))) 0.5 else 0
  }, numeric(1))
}

pm_restriction <- function(ids, ev) {
  vapply(ids, function(z) any(ev$evidence_id %in% z & ev$instrument == "export_control" & ev$status == "in_force"), logical(1))
}

# Select the lightest effective instruments (intensity ascending) whose state, constraint and condition hold.
pm_instruments <- function(d, inst, cfg) {
  pick <- lapply(seq_len(nrow(d)), function(i) {
    st <- d$relationship_state[i]; ct <- d$constraint_type[i]
    if (is.na(ct) || st %in% c("low_strategic_case", "not_assessed")) return(inst[0, ])
    ok <- vapply(seq_len(nrow(inst)), function(j) {
      if (!st %in% strsplit(inst$states[j], "\\|")[[1]]) return(FALSE)
      if (!ct %in% strsplit(inst$constraints[j], "\\|")[[1]]) return(FALSE)
      cond <- inst$condition[j]
      switch(cond,
             none = TRUE,
             buyer_protects = isTRUE(d$buyer_protection[i] >= 0.67),
             more_capacity_needed = isTRUE(d$chokepoint[i] | d$allied_short[i]),
             material_node = d$stage[i] %in% c("Extraction", "Refining", "Recovery"),
             third_country = isTRUE(d$third_country_n[i] > 0) || isTRUE(nzchar(d$aib_third_hosts[i])),
             severe_chokepoint = isTRUE(d$severe_chokepoint[i]) && isTRUE(d$allied_short[i]) && isTRUE(d$evidence_quality[i] >= cfg$evidence$low_evidence_below),
             FALSE)
    }, logical(1))
    x <- inst[ok, , drop = FALSE]
    x <- x[order(x$intensity, x$instrument_id), , drop = FALSE]
    eff <- unlist(cfg$effective_families[[st]])
    main <- which(x$family %in% eff)
    if (!length(main)) return(x[0, ])
    rbind(x[main[1], , drop = FALSE], x[-main[1], , drop = FALSE])
  })
  d$instruments <- vapply(pick, function(x) paste(utils::head(x$instrument_id, 3), collapse = ";"), character(1))
  d$main_instrument <- vapply(pick, function(x) if (nrow(x)) x$instrument_id[1] else NA_character_, character(1))
  d$main_instrument_family <- vapply(pick, function(x) if (nrow(x)) x$family[1] else NA_character_, character(1))
  d$heaviest_instrument_intensity <- vapply(pick, function(x) if (nrow(x)) max(utils::head(x$intensity, 3)) else NA_real_, numeric(1))
  d
}

pm_band <- function(x, hi = 0.67, mid = 0.4) ifelse(is.na(x), "unknown", ifelse(x >= hi, "high", ifelse(x >= mid, "medium", "low")))

# The separate decision fields and the priority tier.
pm_fields <- function(d, cfg, ev, mech, devpart_n, invest_pairs, roles) {
  th <- cfg$thresholds
  a <- d$country_a[1]; b <- d$country_b[1]
  sup_is_a <- !is.na(d$supplier) & d$supplier == a
  cp_sup <- ifelse(sup_is_a, d$competitive_position_a, d$competitive_position_b)
  cp_buy <- ifelse(sup_is_a, d$competitive_position_b, d$competitive_position_a)
  dcap <- ifelse(d$chokepoint, 1, cfg$demand_importance_cap)
  need_score <- function(nband, need, dem) ifelse(nband == "high", need, ifelse(nband == "demand", pmin(dcap, dem / th$node$demand_scale_share), 0))
  nsa <- need_score(d$need_band_a, d$strategic_need_a, ifelse(is.na(d$demand_share_a), 0, d$demand_share_a))
  nsb <- need_score(d$need_band_b, d$strategic_need_b, ifelse(is.na(d$demand_share_b), 0, d$demand_share_b))
  nsa[is.na(nsa)] <- 0; nsb[is.na(nsb)] <- 0
  need_buy <- ifelse(sup_is_a, nsb, nsa)
  st <- d$relationship_state
  supply_state <- st %in% c("a_supplies_b_needs", "b_supplies_a_needs", "capacity_constrained_trusted_supply")
  d$strategic_need_a_score <- round(nsa, 3); d$strategic_need_b_score <- round(nsb, 3)
  # importance: the larger need; for a chain specialisation, the need of the party that is weak at this unit
  weak_need <- ifelse(!is.na(d$supplier) & d$supplier == a, nsb, nsa)
  d$strategic_importance <- round(ifelse(st == "complementary_specialization", weak_need, ifelse(st == "deployment_demand", 0, pmax(nsa, nsb))), 3)
  # complementarity: supplier advantage, bounded by the buyer's need; chain specialisation by the weaker strength
  comp <- ifelse(supply_state, pmin(pmax(0, cp_sup - cp_buy), need_buy),
                 ifelse(st == "complementary_specialization", pmin(cp_sup, 0.75), 0))
  d$bilateral_complementarity <- round(ifelse(is.na(comp), 0, comp), 3)
  d$shared_vulnerability <- round(ifelse(d$chokepoint | d$both_dependent, pmin(nsa, nsb), 0), 3)
  d$trade_fit <- round(ifelse(supply_state, pmin(cp_sup, need_buy), 0), 3)
  # investment: does the AIB model select the supplier as investor with the buyer as host at this node?
  host_sig <- vapply(seq_len(nrow(d)), function(i) {
    if (is.na(d$supplier[i])) return(0)
    ip <- invest_pairs[invest_pairs$investor_iso == d$supplier[i] & invest_pairs$host_iso == d$buyer[i], , drop = FALSE]
    nodes <- if (d$unit_type[i] == "node") d$node_id[i] else d$nodes_of_cell[[i]]
    if (any(ip$node_id %in% nodes)) 1 else if (isTRUE(d[[paste0("buyer_", ifelse(d$buyer[i] == a, "a", "b"))]][i])) 0.67 else 0.33
  }, numeric(1))
  d$investment_host_signal <- host_sig
  d$investment_fit <- round(ifelse(st %in% c(PM_ACTIVE_STATES), pmin(ifelse(is.na(cp_sup), 0, cp_sup), host_sig), 0), 3)
  d$third_country_fit <- round(pmin(1, d$third_country_n / 5), 3)
  # compact fit: both countries hold a compact role in the unit's compact(s)
  rk <- roles[roles$iso3 %in% c(a, b), c("strategic_compact_id", "iso3")]
  cfit <- vapply(seq_len(nrow(d)), function(i) {
    cm <- if (d$unit_type[i] == "node") d$compact[i] else d$compacts_of_cell[i]
    if (is.na(cm)) return(0)
    cms <- strsplit(cm, ";")[[1]]
    n <- length(unique(rk$iso3[rk$strategic_compact_id %in% cms]))
    if (!st[i] %in% PM_ACTIVE_STATES) return(0)
    c(0, 0.5, 1)[n + 1]
  }, numeric(1))
  d$compact_fit <- cfit
  # urgency: the chokepoint share, the allied residual gap, an in-force export control in the evidence layer, a high deployment need
  restr <- pm_restriction(d$evidence_ids, ev)
  d$active_restriction <- restr
  urg <- pmax(ifelse(d$chokepoint, d$coercive_world_share, 0), ifelse(is.na(d$residual_gap), 0, d$residual_gap),
              ifelse(restr, 0.8, 0), ifelse(pmax(d$deployment_need_a, d$deployment_need_b, na.rm = TRUE) >= th$deployment_need_high,
                                            pmax(d$deployment_need_a, d$deployment_need_b, na.rm = TRUE), 0), na.rm = TRUE)
  d$urgency <- round(ifelse(st %in% c("not_assessed"), NA, urg), 3)
  # tractability: mean of A's and B's revealed policy priority and the existing-mechanism score
  mech_score <- vapply(seq_len(nrow(d)), function(i) {
    cm <- if (d$unit_type[i] == "node") d$compact[i] else d$compacts_of_cell[i]
    cms <- if (is.na(cm)) character() else strsplit(cm, ";")[[1]]
    m <- mech[vapply(strsplit(mech$scope_compacts, "\\|"), function(z) any(z %in% cms), logical(1)), , drop = FALSE]
    if (!nrow(m)) 0 else if (any(m$status %in% c("in_force", "signed") & m$has_finance %in% c("TRUE", "partial"))) 1 else 0.5
  }, numeric(1))
  d$mechanism_score <- mech_score
  d$tractability <- round(rowMeans(cbind(d$policy_priority_a, d$policy_priority_b, mech_score), na.rm = TRUE), 3)
  # additionality: market failure, existing commitment, market already working
  mf <- ifelse(is.na(d$constraint_type), NA_real_, unname(PM_FAILURE_SCORE[d$constraint_type]))
  commit <- pm_commitment(d$evidence_ids, ev)
  share_buy <- ifelse(sup_is_a, d$b_imports_from_a_share, d$a_imports_from_b_share)
  working <- ifelse(is.na(share_buy), 0, ifelse(share_buy >= th$bilateral_share_working, 1, ifelse(share_buy >= th$bilateral_share_partial, 0.5, 0)))
  working[!supply_state] <- 0
  d$market_failure_score <- mf; d$existing_commitment <- commit; d$market_already_working <- working
  d$additionality <- round(rowMeans(cbind(mf, 1 - commit, 1 - working)), 3)
  d$additionality_label <- ifelse(is.na(mf), "unknown", ifelse(mf == 0 | commit == 1, "low", ifelse(mf >= 0.75 & commit == 0 & working < 0.5, "high", "medium")))
  # execution speed and value-chain interdependency
  sp <- cfg$speed; pen <- cfg$stage_speed_penalty
  spd <- vapply(seq_len(nrow(d)), function(i) {
    f <- d$main_instrument_family[i]; if (is.na(f)) return(NA_real_)
    v <- if (is.null(sp[[f]])) 0.5 else as.numeric(sp[[f]])
    if (f %in% c("investment", "development") && !is.null(pen[[d$stage[i]]])) v <- min(v, as.numeric(pen[[d$stage[i]]]))
    v
  }, numeric(1))
  d$execution_speed <- round(spd, 2)
  order6 <- c(Extraction = 1, Refining = 2, Components = 3, Assembly = 4, Deployment = 5, Recovery = 1)
  d$interdependency <- round(ifelse(d$unit_type == "node" & !is.na(d$n_steps) & d$n_steps > 1,
                                    (d$n_steps - d$step_order) / (d$n_steps - 1), (5 - order6[d$stage]) / 4), 2)
  # evidence quality: the weaker of the two countries' unit evidence
  d$evidence_quality <- round(pmin(d$evidence_quality_a, d$evidence_quality_b, na.rm = TRUE), 2)
  d$low_evidence <- is.na(d$evidence_quality) | d$evidence_quality < cfg$evidence$low_evidence_below
  d$evidence_basis <- ifelse(d$unit_type == "node", "node trade data; need inherited from cell (proxy)", "engine cell (stage-specific where evidence_specificity is high)")
  # priority tier (lexicographic, no weights)
  tier <- ifelse(st == "not_assessed", 0L, ifelse(st == "deployment_demand", 5L,
          ifelse(st %in% PM_ACTIVE_STATES & d$strategic_importance >= th$need_high & !d$low_evidence & d$additionality_label != "low", 1L,
          ifelse(st %in% PM_ACTIVE_STATES, 2L, ifelse(st == "direct_competition", 3L, 4L)))))
  d$priority_tier <- tier
  d$priority_label <- c("not assessed", "joint bet", "coordinate", "manage competition", "leave open", "demand context")[tier + 1]
  lab <- function(x) match(x, c("high", "medium", "low", "unknown"))
  o <- order(d$priority_tier == 0, d$priority_tier == 5, d$priority_tier, -d$urgency, lab(d$additionality_label), -d$strategic_importance,
             -d$tractability, -d$evidence_quality, d$unit_type, d$unit_id)
  d <- d[o, ]
  d$priority_rank <- seq_len(nrow(d))
  d
}

