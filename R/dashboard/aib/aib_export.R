# Allied Industrial Base (AIB): the Sectoral Compacts dashboard bundle (scripts/84_export_aib_data.R,
# dashboard/src/aib). Pure: builds the JSON-ready list from scripts/32's aib_results.rds. Every
# array-typed field is wrapped in I() so jsonlite's auto_unbox cannot collapse a one-item list into
# a scalar (a Y&F export broke that way on 2026-10-14).

AIB_DASH_SCHEMA <- "osi-aib"
AIB_DASH_VERSION <- 1L

aib_dash_arr <- function(x) I(if (is.null(x)) character() else as.character(x))
aib_dash_num_arr <- function(x) I(if (is.null(x)) numeric() else as.numeric(x))
aib_dash_split <- function(x, sep = " ") {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(aib_dash_arr(character()))
  aib_dash_arr(strsplit(x, sep, fixed = TRUE)[[1]])
}
aib_dash_round <- function(x, d = 4) ifelse(is.na(x), NA_real_, round(as.numeric(x), d))

# Node records (every classified node, with its frontier) keyed for the view.
aib_dash_nodes <- function(res) {
  cl <- res$classification
  nt <- res$compact_nodes
  q <- res$quality
  fr <- res$frontier
  fr <- fr[fr$level == "subsector" & fr$shock_id == res$meta$headline_shock & fr$scenario == res$meta$headline_scenario, , drop = FALSE]
  if ("target_iso" %in% names(fr)) {
    # A shared node can carry one run per target; the headline (country) shock has one.
    fr <- fr[!duplicated(fr[, c("node_id", "max_supplier_share")]), , drop = FALSE]
  }
  lapply(seq_len(nrow(cl)), function(i) {
    r <- cl[i, ]
    t <- nt[nt$node_id == r$node_id, , drop = FALSE]
    qq <- q[q$node_id == r$node_id & is.na(q$aggregated_into), , drop = FALSE]
    f <- fr[fr$node_id == r$node_id, , drop = FALSE]
    f <- f[order(f$max_supplier_share), , drop = FALSE]
    list(
      id = r$node_id,
      label = r$sub_sector,
      techs = aib_dash_split(r$techs, "; "),
      stage = r$supply_chain,
      cls = r$node_classification,
      reason = r$classification_reason,
      quality = if (nrow(qq)) qq$data_quality[1] else NA_character_,
      flags = if (nrow(qq) && !is.na(qq$flags[1]) && nzchar(qq$flags[1])) aib_dash_split(qq$flags[1], "; ") else aib_dash_arr(character()),
      exposure = aib_dash_round(r$shock_exposure),
      static = aib_dash_round(r$static_coverage),
      existing = aib_dash_round(r$existing_coverage),
      optimized = aib_dash_round(r$optimized_coverage),
      dividend = aib_dash_round(r$coordination_dividend),
      residual = aib_dash_round(r$residual_gap),
      demandUsd = aib_dash_round(r$demand_usd, 0),
      lostUsd = aib_dash_round(r$lost_usd, 0),
      baseLargest = r$baseline_largest_supplier,
      baseLargestShare = aib_dash_round(r$baseline_largest_share),
      largest = r$optimized_largest_supplier,
      largestShare = aib_dash_round(r$optimized_largest_share),
      hhi = aib_dash_round(r$optimized_hhi),
      hosts = aib_dash_split(r$priority_investment_hosts, "; "),
      bestGain = aib_dash_round(r$best_marginal_gain),
      partners = if (nrow(t)) t$key_trade_partners[1] else NA_character_,
      treatment = if (nrow(t)) t$recommended_policy_treatment[1] else NA_character_,
      compact = if (nrow(t) && !is.na(t$compact_id[1]) && nzchar(t$compact_id[1])) t$compact_id[1] else NA_character_,
      yfCells = aib_dash_arr(unique(t$yf_cell)),
      yfSubcomponents = if (nrow(t)) aib_dash_split(t$yf_subcomponent_id[1], "; ") else aib_dash_arr(character()),
      tightestCostless = if (nrow(f)) aib_dash_round(f$tightest_costless_max_share[1]) else NA_real_,
      minRequiredDiversification = if (nrow(f)) aib_dash_round(f$min_required_diversification[1]) else NA_real_,
      frontier = lapply(seq_len(nrow(f)), function(k) list(
        cap = f$max_supplier_share[k], coverage = aib_dash_round(f$optimized_coverage[k]),
        largestShare = aib_dash_round(f$optimized_largest_share[k]), largest = f$optimized_largest_supplier[k],
        secondShock = aib_dash_round(f$second_shock_coverage[k]), specialization = aib_dash_round(f$specialization_score[k])
      ))
    )
  })
}

# One compact record: overview, roles, coverage comparison, node list, the trade and investment
# relationships behind it (the largest `top_edges` of each) and the coalition path.
aib_dash_compact <- function(cp, res, top_edges = 40) {
  ids <- res$compact_nodes$node_id[!is.na(res$compact_nodes$compact_id) & res$compact_nodes$compact_id == cp$compact_id]
  ids <- unique(ids)
  mem <- res$compact_members[res$compact_members$compact_id == cp$compact_id, , drop = FALSE]
  pth <- res$coalition_paths[res$coalition_paths$compact_id == cp$compact_id, , drop = FALSE]
  pth <- pth[order(pth$step), , drop = FALSE]
  hs <- res$meta$headline_shock
  sc <- res$meta$headline_scenario
  tr <- res$partnerships[res$partnerships$node_id %in% ids & res$partnerships$shock_id == hs & res$partnerships$scenario == sc &
                           res$partnerships$relationship != "domestic retention", , drop = FALSE]
  tr <- tr[order(-tr$delta_usd), , drop = FALSE]
  tr <- utils::head(tr, top_edges)
  iv <- res$investment_pairs
  iv <- if (is.null(iv) || !nrow(iv)) iv else iv[iv$node_id %in% ids & iv$shock_id == hs & iv$scenario == sc & iv$relationship != "domestic" & dplyr::coalesce(iv$host_rank, 99L) <= 5L, , drop = FALSE]
  if (!is.null(iv) && nrow(iv)) iv <- utils::head(iv[order(-iv$supply_from_new_capacity_usd), , drop = FALSE], top_edges)
  roles <- lapply(seq_len(nrow(mem)), function(k) list(
    iso3 = mem$iso3[k], role = mem$role[k], basis = mem$basis[k], member = isTRUE(mem$in_minimum_coalition[k])
  ))
  parse_cov <- function(s) {
    if (is.na(s) || !nzchar(s)) return(list())
    kv <- strsplit(strsplit(s, "; ", fixed = TRUE)[[1]], "=", fixed = TRUE)
    stats::setNames(lapply(kv, function(p) as.numeric(p[2])), vapply(kv, `[`, "", 1))
  }
  list(
    id = cp$compact_id,
    name = cp$compact_name,
    technologies = aib_dash_split(cp$technology, "; "),
    stages = aib_dash_split(cp$stages, "; "),
    archetype = cp$compact_archetype,
    type = cp$compact_type,
    nodes = aib_dash_arr(ids),
    coalition = list(
      members = aib_dash_split(cp$minimum_coalition),
      t90 = aib_dash_split(cp$minimum_coalition_90),
      t95 = aib_dash_split(cp$minimum_coalition_95),
      t99 = aib_dash_split(cp$minimum_coalition_99),
      reached99 = isTRUE(as.logical(cp$target_reached_99)),
      maxCoverage = aib_dash_round(cp$max_anchor_coverage)
    ),
    anchors = aib_dash_split(cp$demand_anchors),
    suppliers = aib_dash_split(cp$specialized_suppliers),
    hosts = aib_dash_split(cp$investment_hosts),
    technologyPartners = aib_dash_split(cp$technology_partners),
    bridges = aib_dash_split(cp$bridge_partners),
    associates = aib_dash_split(cp$associate_partners),
    roles = roles,
    metrics = list(
      demandUsd = aib_dash_round(cp$demand_usd, 0), lostUsd = aib_dash_round(cp$lost_usd, 0),
      exposure = aib_dash_round(cp$baseline_shock_exposure),
      static = aib_dash_round(cp$coverage_static),
      existing = aib_dash_round(cp$coverage_without_coordination),
      trade = aib_dash_round(cp$coverage_with_trade_coordination),
      investment = aib_dash_round(cp$coverage_after_investment_simulation),
      dividend = aib_dash_round(cp$coordination_dividend),
      residual = aib_dash_round(cp$residual_gap),
      residualAfterInvestment = aib_dash_round(cp$residual_gap_after_investment),
      globalOptimum = aib_dash_round(cp$coverage_global_optimum),
      anchorFragmented = aib_dash_round(cp$anchor_coverage_fragmented),
      anchorCompact = aib_dash_round(cp$anchor_coverage_compact),
      compactGain = aib_dash_round(cp$compact_gain_over_fragmented)
    ),
    classMix = cp$class_mix,
    secondaryClass = if (is.na(cp$secondary_class)) NA_character_ else cp$secondary_class,
    policyNeeds = aib_dash_split(cp$key_policy_needs, "; "),
    path = lapply(seq_len(nrow(pth)), function(k) list(
      step = pth$step[k], added = if (is.na(pth$added[k])) NA_character_ else pth$added[k],
      members = aib_dash_split(pth$members[k]), minCoverage = aib_dash_round(pth$min_coverage[k]),
      meanCoverage = aib_dash_round(pth$mean_coverage[k]), nodeCoverage = parse_cov(pth$node_coverage[k])
    )),
    trade = lapply(seq_len(nrow(tr)), function(k) list(
      from = tr$exporter_iso[k], to = tr$importer_iso[k], node = tr$node_id[k], usd = aib_dash_round(tr$delta_usd[k], 0),
      share = aib_dash_round(tr$delta_share_of_demand[k]), relationship = tr$relationship[k],
      opportunity = aib_dash_round(tr$export_opportunity[k])
    )),
    investment = if (is.null(iv) || !nrow(iv)) list() else lapply(seq_len(nrow(iv)), function(k) list(
      investor = iv$investor_iso[k], host = iv$host_iso[k], node = iv$node_id[k], usd = aib_dash_round(iv$supply_from_new_capacity_usd[k], 0),
      share = aib_dash_round(iv$supply_from_new_capacity[k]), friendshore = aib_dash_round(iv$friendshore[k])
    ))
  )
}

# The whole document: meta, country names for every code used, compacts and nodes.
aib_dash_bundle <- function(res, top_edges = 40) {
  cfg <- res$meta$config
  nodes <- aib_dash_nodes(res)
  comps <- lapply(seq_len(nrow(res$compacts)), function(i) aib_dash_compact(res$compacts[i, ], res, top_edges))
  codes <- unique(c(
    unlist(lapply(comps, function(c) c(unlist(c$coalition$t99), unlist(c$coalition$members), unlist(lapply(c$roles, `[[`, "iso3")),
                                         unlist(lapply(c$trade, function(t) c(t$from, t$to))), unlist(lapply(c$investment, function(t) c(t$investor, t$host)))))),
    unlist(lapply(nodes, function(n) c(n$baseLargest, n$largest, unlist(n$hosts))))
  ))
  codes <- sort(codes[!is.na(codes) & nzchar(codes)])
  nm <- res$country_names
  names_out <- stats::setNames(as.list(ifelse(is.na(nm$country[match(codes, nm$iso3)]), codes, nm$country[match(codes, nm$iso3)])), codes)
  shocks <- lapply(cfg$shocks, function(s) list(id = s$id, label = s$label))
  list(
    schema = AIB_DASH_SCHEMA, version = AIB_DASH_VERSION,
    meta = list(
      universe = res$meta$universe, headlineShock = res$meta$headline_shock, headlineScenario = res$meta$headline_scenario,
      tradeYear = res$meta$trade_year, built = res$meta$built,
      shocks = shocks, scenarios = aib_dash_arr(unlist(cfg$scenarios)),
      classes = aib_dash_arr(aib_node_classes()),
      targets = aib_dash_num_arr(unlist(cfg$compacts$coverage_targets)),
      thresholds = cfg$classification
    ),
    countries = names_out,
    compacts = comps,
    nodes = nodes
  )
}
