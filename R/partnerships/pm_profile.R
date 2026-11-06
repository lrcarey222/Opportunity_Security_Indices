# Partnership Memos: country profiles at two grains (docs/partnership_memos.md).
#
#   cell: technology x six-stage node, read from the strategic-position table (scripts/26). Competitive
#         Position, supply-chain or deployment need, the prize, the choice and the policy columns are the
#         engine's own; nothing is recomputed.
#   node: Allied Industrial Base electro-industrial node (scripts/31-33): world export / production share,
#         RCA, allied demand and net-import shares, host rank. Node need is INHERITED from the node's
#         technology x stage cells (the engine scores need per cell only) and is labelled a proxy.
#
# Pure functions: data frames and config lists in, data frames out. No IO.

PM_POSITION_RANK <- c(not_measured = 0L, weak = 1L, moderate = 2L, strong = 3L)

pm_num <- function(x) suppressWarnings(as.numeric(x))
pm_lgl <- function(x) {
  if (is.logical(x)) return(x)
  x <- toupper(trimws(as.character(x)))
  ifelse(x %in% c("TRUE", "T", "1", "YES"), TRUE, ifelse(x %in% c("FALSE", "F", "0", "NO"), FALSE, NA))
}
pm_cfg <- function(cfg, ...) {
  v <- cfg
  for (k in c(...)) { if (is.null(v[[k]])) return(NULL); v <- v[[k]] }
  v
}
pm_quality_score <- function(q, map) {
  q <- tolower(as.character(q))
  out <- vapply(q, function(z) if (is.na(z) || is.null(map[[z]])) NA_real_ else as.numeric(map[[z]]), numeric(1))
  unname(out)
}

# Stage roll-up: the canonical helper when R/utils/supply_chain_stages.R is loaded, else the config mapping.
pm_rollup <- function(stage) {
  if (exists("supply_chain_stage_rollup", mode = "function")) return(supply_chain_stage_rollup(stage))
  map <- c(Extraction = "Upstream", Refining = "Upstream", Recovery = "Upstream",
           Components = "Midstream", Assembly = "Midstream", Deployment = "Downstream")
  unname(map[as.character(stage)])
}

# ---- Cell profile -----------------------------------------------------------------------------------------

# sp: the strategic-position table (any economies); iso: one ISO3. Returns one row per tech x stage.
pm_cell_profile <- function(sp, iso, cfg) {
  th <- cfg$thresholds
  x <- sp[sp$iso3 == iso, , drop = FALSE]
  if (!nrow(x)) stop("pm_cell_profile: no strategic-position rows for ", iso)
  stage <- as.character(x$supply_chain)
  is_dep <- stage == "Deployment"
  cp_contender <- pm_num(x$cp_contender_pct)
  cp_cross <- pm_num(x$competitive_position_percentile)
  cp <- ifelse(is.na(cp_contender), cp_cross, cp_contender)
  cp_high <- pm_lgl(x$cp_high)
  contender <- pm_lgl(x$cp_is_contender)
  band <- ifelse(is_dep,
                 ifelse(is.na(cp_cross), "not_measured", ifelse(cp_cross >= 0.75, "strong", ifelse(cp_cross >= 0.5, "moderate", "weak"))),
                 ifelse(is.na(cp), "not_measured",
                        ifelse(!is.na(cp_high) & cp_high, "strong",
                               ifelse(!is.na(contender) & contender, "moderate", "weak"))))
  need <- ifelse(is_dep, pm_num(x$deployment_need), pm_num(x$supply_chain_need))
  need_line <- ifelse(is_dep, th$deployment_need_high, th$need_high)
  home <- pm_num(x$home_share_exposure)
  buyer <- (pm_lgl(x$sn_import_material) %in% TRUE) & (is.na(home) | home < 0.10)
  need_band <- ifelse(is.na(need), "not_measured", ifelse(need >= need_line, "high", "low"))
  q <- cfg$evidence$quality_score
  ev_cp <- pm_quality_score(x$stage_evidence_quality_cp, q)
  ev_sn <- pm_quality_score(x$stage_evidence_quality_sn, q)
  ev <- ifelse(is_dep, ev_cp, pmin(ev_cp, ifelse(is.na(ev_sn), ev_cp, ev_sn), na.rm = TRUE))
  data.frame(
    iso3 = iso, unit_type = "cell", unit_id = paste(x$tech, stage, sep = "|"),
    tech = as.character(x$tech), stage = stage, rollup = pm_rollup(stage), node_id = NA_character_,
    node_label = paste(x$tech, stage), compact = NA_character_, step = NA_character_, step_order = NA_integer_,
    competitive_position = round(cp, 3), cp_cross_country = round(cp_cross, 3), position_band = band,
    position_basis = ifelse(is_dep, "market_deployment_percentile", ifelse(is.na(cp_contender), "cross_country_percentile", "contender_percentile")),
    world_share = round(home, 4), export_share = round(pm_num(x$trade_benchmark_share), 4),
    rca = round(pm_num(x$trade_benchmark_rca), 2),
    production_position = round(pm_num(x$cp_production_position), 3),
    manufacturing_capacity = round(pm_num(x$cp_manufacturing_capacity), 3),
    investment_position = round(pm_num(x$cp_investment_position), 3),
    trade_momentum = round(pm_num(x$trade_momentum_score), 3),
    largest_trusted = NA, host_rank = NA_integer_,
    strategic_need = round(need, 3), need_type = ifelse(is_dep, "deployment", "supply_chain"),
    need_basis = ifelse(is_dep, "deployment_need", "supply_chain_need"), need_band = need_band, buyer = buyer,
    demand_share = NA_real_, net_import_share = NA_real_,
    imports_usd = pm_num(x$imports_usd), imports_to_gdp = pm_num(x$imports_to_gdp),
    china_share = round(pm_num(x$china_share), 3), coercive_import_share = round(pm_num(x$coercive_import_share), 3),
    top_supplier_iso3 = as.character(x$top_supplier_iso3), top_supplier_share = round(pm_num(x$top_supplier_share), 3),
    import_hhi = round(pm_num(x$import_hhi), 3),
    prize = round(pm_num(x$strategic_industrial_value), 3), large_yard = pm_lgl(x$large_yard_case),
    strategic_choice = as.character(x$strategic_choice), security_rationale = as.character(x$security_rationale),
    delivery_mode = as.character(x$delivery_mode), posture_confidence = as.character(x$posture_confidence),
    policy_priority = round(pm_num(x$policy_priority_percentile), 3),
    policy_support_priority = round(pm_num(x$policy_support_priority), 3),
    policy_protection_priority = round(pm_num(x$policy_protection_priority), 3),
    policy_momentum = round(pm_num(x$policy_momentum), 3),
    policy_alignment_label = as.character(x$policy_alignment),
    evidence_specificity = round(pm_num(x$evidence_specificity_cp), 3),
    stage_evidence_quality = as.character(x$stage_evidence_quality_cp),
    evidence_quality = round(ev, 2),
    data_year = as.character(x$trade_benchmark_period),
    recommended_partners = trimws(paste(ifelse(is.na(x$recommended_partner_1), "", x$recommended_partner_1),
                                        ifelse(is.na(x$recommended_partner_2), "", x$recommended_partner_2),
                                        ifelse(is.na(x$recommended_partner_3), "", x$recommended_partner_3))),
    partner_aib_status = as.character(x$partner_aib_status),
    world_top_iso3 = as.character(x$world_top_iso3), world_top_share = round(pm_num(x$world_top_share), 3),
    coercive_top_iso3 = as.character(x$coercive_top_iso3), coercive_world_share = round(pm_num(x$coercive_world_share), 3),
    trusted_world_share = round(pm_num(x$trusted_world_share), 3),
    aib_class = NA_character_, residual_gap = NA_real_, node_demand_usd = NA_real_, largest_trusted_supplier = NA_character_,
    stringsAsFactors = FALSE
  )
}

# ---- Node profile -----------------------------------------------------------------------------------------

# nodes: node_id, supply_chain (six-stage), score_cells ("Tech|Stage; ..."), from aib_inputs.rds$nodes.
# spine: aib_strategic_spine.csv (one scenario); dol: aib_compact_division_of_labour.csv;
# nclass: aib_node_classification.csv; cells: pm_cell_profile() of the same country.
pm_node_frame <- function(nodes, spine, nclass, dol, coercive) {
  sp1 <- spine[!duplicated(spine$node_id), c("node_id", "node_label", "strategic_compact_id", "value_chain_step",
                                              "value_chain_order", "demand_usd", "largest_supplier", "largest_share"), drop = FALSE]
  n_steps <- tapply(pm_num(spine$value_chain_order), spine$strategic_compact_id, max, na.rm = TRUE)
  fr <- merge(nodes[, c("node_id", "supply_chain", "score_cells")], sp1, by = "node_id", all.x = FALSE)
  nc <- nclass[, intersect(c("node_id", "node_classification", "policy_needs", "residual_gap", "static_coverage", "data_quality",
                              "optimized_largest_supplier", "priority_investment_hosts", "specialty_hosts", "development_hosts"), names(nclass)), drop = FALSE]
  fr <- merge(fr, nc, by = "node_id", all.x = TRUE)
  # coercive share of world supply: summed positions of the coercive suppliers listed at the node (export or production)
  cs <- dol[dol$iso3 %in% coercive, , drop = FALSE]
  cs <- cs[!duplicated(paste(cs$node_id, cs$iso3)), , drop = FALSE]
  csum <- tapply(pm_num(cs$position_share), cs$node_id, sum, na.rm = TRUE)
  ctop <- vapply(split(cs, cs$node_id), function(d) d$iso3[which.max(pm_num(d$position_share))], character(1))
  fr$coercive_world_share <- round(pmin(1, unname(csum[fr$node_id])), 3)
  fr$coercive_world_share[is.na(fr$coercive_world_share)] <- 0
  fr$node_label <- ifelse(is.na(fr$node_label) | fr$node_label == "", gsub("-", " ", fr$node_id), fr$node_label)
  fr$coercive_top_iso3 <- unname(ctop[fr$node_id])
  fr$n_steps <- unname(n_steps[fr$strategic_compact_id])
  fr$primary_tech <- vapply(strsplit(as.character(fr$score_cells), ";"), function(z) {
    z <- trimws(z); z <- z[nzchar(z)]; if (!length(z)) NA_character_ else sub("\\|.*$", "", z[1]) }, character(1))
  fr
}

pm_node_profile <- function(frame, dol, iso, cells, cfg) {
  th <- cfg$thresholds$node
  d <- dol[dol$iso3 == iso, , drop = FALSE]
  d <- d[!duplicated(d$node_id), , drop = FALSE]
  x <- merge(frame, d[, c("node_id", "position_share", "world_share", "production_share", "export_rca", "scope_demand_share",
                          "net_import_share", "world_import_share", "best_host_rank", "capable_host", "momentum",
                          "anchor_position", "specialist_position")], by = "node_id", all.x = TRUE)
  listed <- !is.na(x$position_share)
  ps <- ifelse(listed, pm_num(x$position_share), 0)
  rca <- pm_num(x$export_rca)
  largest_trusted <- !is.na(x$optimized_largest_supplier) & x$optimized_largest_supplier == iso
  anchor <- pm_lgl(x$anchor_position) %in% TRUE
  specialist <- pm_lgl(x$specialist_position) %in% TRUE
  host_rank <- pm_num(x$best_host_rank)
  capable_host <- pm_lgl(x$capable_host) %in% TRUE
  band <- ifelse(anchor | specialist | ps >= th$strong_share | (largest_trusted & ps >= th$presence_share), "strong",
                 ifelse(ps >= th$presence_share | capable_host, "moderate", "weak"))
  dem <- ifelse(listed, pm_num(x$scope_demand_share), 0)
  nis <- ifelse(listed, pm_num(x$net_import_share), 0)
  buyer <- nis >= th$material_net_import_share | (dem >= th$material_demand_share & nis > 0)
  # need inherited from the node's cells: the highest producing-stage supply-chain need among them
  cell_need <- setNames(cells$strategic_need, cells$unit_id)
  cell_row <- setNames(seq_len(nrow(cells)), cells$unit_id)
  sc <- strsplit(as.character(x$score_cells), ";")
  inh <- vapply(sc, function(z) { z <- trimws(z); v <- cell_need[z[z %in% names(cell_need)]]; if (!length(v) || all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) }, numeric(1))
  first_cell <- vapply(sc, function(z) { z <- trimws(z); z <- z[z %in% names(cell_row)]; if (!length(z)) NA_integer_ else unname(cell_row[z[1]]) }, integer(1))
  pick <- function(col) { v <- cells[[col]]; ifelse(is.na(first_cell), NA, v[first_cell]) }
  need_line <- cfg$thresholds$need_high
  need_band <- ifelse(!is.na(inh) & inh >= need_line & buyer, "high",
                      ifelse(buyer, "demand", ifelse(is.na(inh) & !listed, "not_measured", "low")))
  aq <- cfg$evidence$aib_quality_score
  ev_aib <- vapply(as.character(x$data_quality), function(z) if (is.na(z) || is.null(aq[[z]])) NA_real_ else as.numeric(aq[[z]]), numeric(1))
  ev <- pmin(unname(ev_aib), cfg$evidence$proxy_cap, na.rm = TRUE)
  data.frame(
    iso3 = iso, unit_type = "node", unit_id = x$node_id, tech = x$primary_tech, stage = as.character(x$supply_chain),
    rollup = pm_rollup(as.character(x$supply_chain)), node_id = x$node_id, node_label = x$node_label,
    compact = x$strategic_compact_id, step = x$value_chain_step, step_order = as.integer(pm_num(x$value_chain_order)),
    competitive_position = round(pmin(1, ps / th$strong_share), 3), cp_cross_country = NA_real_, position_band = band,
    position_basis = ifelse(listed, "aib_position_share", "aib_unlisted_below_1pct"),
    world_share = round(ps, 4), export_share = round(ifelse(listed, pm_num(x$world_share), NA), 4), rca = round(rca, 2),
    production_position = round(pm_num(x$production_share), 3), manufacturing_capacity = NA_real_, investment_position = NA_real_,
    trade_momentum = ifelse(pm_lgl(x$momentum) %in% TRUE, 1, ifelse(listed, 0, NA)),
    largest_trusted = largest_trusted, host_rank = as.integer(host_rank),
    strategic_need = round(inh, 3), need_type = "supply_chain", need_basis = "inherited_from_cell", need_band = need_band, buyer = buyer,
    demand_share = round(dem, 3), net_import_share = round(nis, 3),
    imports_usd = NA_real_, imports_to_gdp = NA_real_, china_share = pick("china_share"), coercive_import_share = pick("coercive_import_share"),
    top_supplier_iso3 = NA_character_, top_supplier_share = NA_real_, import_hhi = NA_real_,
    prize = pick("prize"), large_yard = pick("large_yard"), strategic_choice = pick("strategic_choice"),
    security_rationale = pick("security_rationale"), delivery_mode = pick("delivery_mode"), posture_confidence = pick("posture_confidence"),
    policy_priority = pick("policy_priority"), policy_support_priority = pick("policy_support_priority"),
    policy_protection_priority = pick("policy_protection_priority"), policy_momentum = pick("policy_momentum"),
    policy_alignment_label = pick("policy_alignment_label"),
    evidence_specificity = NA_real_, stage_evidence_quality = as.character(x$data_quality), evidence_quality = round(ev, 2),
    data_year = "2024", recommended_partners = NA_character_, partner_aib_status = NA_character_,
    world_top_iso3 = x$largest_supplier, world_top_share = round(pm_num(x$largest_share), 3),
    coercive_top_iso3 = x$coercive_top_iso3, coercive_world_share = x$coercive_world_share, trusted_world_share = NA_real_,
    aib_class = x$node_classification, residual_gap = round(pm_num(x$residual_gap), 3), node_demand_usd = pm_num(x$demand_usd),
    largest_trusted_supplier = x$optimized_largest_supplier,
    stringsAsFactors = FALSE
  )
}

# Both countries, both grains: the country_profiles.csv table.
pm_country_profiles <- function(sp, frame, dol, a, b, cfg) {
  ca <- pm_cell_profile(sp, a, cfg); cb <- pm_cell_profile(sp, b, cfg)
  na <- pm_node_profile(frame, dol, a, ca, cfg); nb <- pm_node_profile(frame, dol, b, cb, cfg)
  out <- rbind(ca, cb, na, nb)
  out[order(out$unit_type, out$iso3, out$tech, match(out$stage, c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")), out$unit_id), ]
}
