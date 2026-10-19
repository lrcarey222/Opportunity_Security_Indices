# Allied Industrial Base (AIB): the Sectoral Compacts dashboard bundle (scripts/84_export_aib_data.R,
# dashboard/src/aib). Pure: builds the JSON-ready list from scripts/32's aib_results.rds. Every
# array-typed field is wrapped in I() so jsonlite's auto_unbox cannot collapse a one-item list into
# a scalar (a Y&F export broke that way on 2026-10-14).
#
# Version 1 fields are unchanged except frontier[].secondShock, now two readings of N-1:
# instantaneousN1 (coverage x (1 - largest share)) and n1Reoptimized (largest supplier removed, trade
# re-optimized). Added (optional in the contract): meta demand scope / candidate universe / shock
# semantics / supply constraint / strategic scope; per node the multi-label policy needs, the
# physical validation and warning, the class under the other variants and the growth-bound
# sensitivity; per compact the exact coalition method, the greedy sufficient coalition and the
# coalition tiers (critical supply partners, critical (and marginal) and other external dependencies),
# per-variant physical warnings (variantWarnings, growth[].warning) and the members' leave-one-out flag
# and substitutes. Added 2026-10-17 (optional): `network`, the emergent AIB network (country roles and
# supplier -> buyer edges, aib_dash_network) and `investmentFlows`, the investor -> host flows of the
# marginal investment simulation with the ranked hosts (aib_dash_investment_flows); both the headline
# variant, with its variant fields. aib_check.R checks them as the TypeScript contract does.

AIB_DASH_SCHEMA <- "osi-aib"
AIB_DASH_VERSION <- 1L

aib_dash_arr <- function(x) I(if (is.null(x)) character() else as.character(x))
aib_dash_num_arr <- function(x) I(if (is.null(x)) numeric() else as.numeric(x))
aib_dash_split <- function(x, sep = " ") {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(aib_dash_arr(character()))
  aib_dash_arr(strsplit(x, sep, fixed = TRUE)[[1]])
}
aib_dash_round <- function(x, d = 4) ifelse(is.na(x), NA_real_, round(as.numeric(x), d))
aib_dash_chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) NA_character_ else as.character(x)
aib_dash_col <- function(r, col) if (col %in% names(r)) r[[col]][1] else NA

# Node records (every classified node, with its frontier) keyed for the view.
aib_dash_nodes <- function(res) {
  cl <- res$classification
  nt <- res$compact_nodes
  q <- res$quality
  fr <- res$frontier
  fr <- fr[fr$level == "subsector" & fr$shock_id == res$meta$headline_shock & fr$scenario == res$meta$headline_scenario, , drop = FALSE]
  # One variant only: the headline shock semantics and supply constraint.
  if ("supply_constraint" %in% names(fr) && !is.null(res$meta$headline_constraint)) fr <- fr[fr$supply_constraint == res$meta$headline_constraint, , drop = FALSE]
  if ("shock_semantics" %in% names(fr) && !is.null(res$meta$headline_semantics)) fr <- fr[fr$shock_semantics == res$meta$headline_semantics, , drop = FALSE]
  if ("target_iso" %in% names(fr)) {
    # A shared node can carry one run per target; the headline (country) shock has one.
    fr <- fr[!duplicated(fr[, c("node_id", "max_supplier_share")]), , drop = FALSE]
  }
  gr <- res$growth
  if (!is.null(gr) && nrow(gr)) gr <- gr[gr$scenario == res$meta$headline_scenario, , drop = FALSE]
  other_cls <- grep("^class_", names(cl), value = TRUE)
  other_cls <- setdiff(other_cls, grep("^class_growth_", other_cls, value = TRUE))
  # The class under each supply-constraint mode (trade_only, production_constrained).
  mode_cls <- if (is.null(res$layers)) list() else lapply(res$layers, function(L) stats::setNames(L$classified$node_classification, L$classified$node_id))
  mode_warn <- if (is.null(res$layers)) list() else lapply(res$layers, function(L) stats::setNames(!is.na(L$classified$classification_warning), L$classified$node_id))
  lapply(seq_len(nrow(cl)), function(i) {
    r <- cl[i, ]
    g <- if (!is.null(gr) && nrow(gr)) gr[gr$node_id == r$node_id, , drop = FALSE] else NULL
    if (!is.null(g) && nrow(g)) g <- g[order(g$existing_growth), , drop = FALSE]
    needs <- c(COORDINATE = isTRUE(as.logical(aib_dash_col(r, "needs_coordinate"))), BUILD = isTRUE(as.logical(aib_dash_col(r, "needs_build"))),
               `MANAGE SPECIALIZATION` = isTRUE(as.logical(aib_dash_col(r, "needs_manage_specialization"))))
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
      policyNeeds = aib_dash_arr(names(needs)[needs]),
      warning = aib_dash_chr(aib_dash_col(r, "classification_warning")),
      physicalValidation = aib_dash_chr(aib_dash_col(r, "physical_validation")),
      physicalData = aib_dash_chr(aib_dash_col(r, "physical_data")),
      physicalDivergence = aib_dash_round(aib_dash_col(r, "physical_divergence")),
      inStrategicScope = isTRUE(as.logical(aib_dash_col(r, "in_strategic_scope"))),
      variants = c(stats::setNames(lapply(other_cls, function(k) aib_dash_chr(r[[k]])), sub("^class_", "", other_cls)),
                   lapply(mode_cls, function(v) aib_dash_chr(unname(v[r$node_id])))),
      # TRUE where that variant's class carries PHYSICAL CAPABILITY NOT VALIDATED.
      variantWarnings = c(stats::setNames(lapply(sub("^class_", "", other_cls), function(k) {
        w <- aib_dash_col(r, paste0("classification_warning_", k))
        !is.na(w) && nzchar(w)
      }), sub("^class_", "", other_cls)), lapply(mode_warn, function(v) isTRUE(unname(v[r$node_id])))),
      growth = if (is.null(g) || !nrow(g)) list() else lapply(seq_len(nrow(g)), function(k) list(
        label = g$existing_growth_label[k], existing = aib_dash_round(g$existing_coverage[k]),
        dividend = aib_dash_round(g$coordination_dividend[k]), cls = aib_dash_chr(g$node_classification[k]),
        warning = {
          w <- aib_dash_col(r, paste0("classification_warning_growth_", g$existing_growth_label[k]))
          !is.na(w) && nzchar(w)
        }
      )),
      coordinateSensitiveToGrowth = isTRUE(as.logical(aib_dash_col(r, "coordinate_sensitive_to_growth"))),
      n1Reoptimized = aib_dash_round(aib_dash_col(r, "optimized_n1_reoptimized_coverage")),
      instantaneousN1 = aib_dash_round(aib_dash_col(r, "optimized_instantaneous_n1_retained_coverage")),
      diversion = aib_dash_round(aib_dash_col(r, "optimized_diversion")),
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
        instantaneousN1 = aib_dash_round(f$instantaneous_n1_retained_coverage[k]), n1Reoptimized = aib_dash_round(f$n1_reoptimized_coverage[k]),
        n1Removed = aib_dash_chr(f$n1_removed_supplier[k]), specialization = aib_dash_round(f$specialization_score[k])
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
  if (!is.null(iv) && nrow(iv) && "supply_constraint" %in% names(iv) && !is.null(res$meta$headline_constraint)) {
    iv <- iv[iv$supply_constraint == res$meta$headline_constraint, , drop = FALSE]
  }
  iv <- if (is.null(iv) || !nrow(iv)) iv else iv[iv$node_id %in% ids & iv$shock_id == hs & iv$scenario == sc & iv$relationship != "domestic" & dplyr::coalesce(iv$host_rank, 99L) <= 5L, , drop = FALSE]
  if (!is.null(iv) && nrow(iv)) iv <- utils::head(iv[order(-iv$supply_from_new_capacity_usd), , drop = FALSE], top_edges)
  roles <- lapply(seq_len(nrow(mem)), function(k) list(
    iso3 = mem$iso3[k], role = mem$role[k], basis = mem$basis[k], member = isTRUE(mem$in_minimum_coalition[k]),
    tier = aib_dash_chr(aib_dash_col(mem[k, ], "coalition_tier")), critical = isTRUE(as.logical(aib_dash_col(mem[k, ], "critical_supplier"))),
    looCritical = isTRUE(as.logical(aib_dash_col(mem[k, ], "loo_critical"))),
    substitutes = aib_dash_split(aib_dash_col(mem[k, ], "substitutable_by")),
    substitutesNeeded = suppressWarnings(as.integer(aib_dash_col(mem[k, ], "substitutes_needed")))
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
      # The exact 95% coalition; where the target is out of reach, the widest greedy set (see method).
      members = if (!is.na(cp$minimum_coalition) && nzchar(cp$minimum_coalition)) aib_dash_split(cp$minimum_coalition) else aib_dash_split(aib_dash_col(cp, "core_coordinating_members")),
      robustTieBreakSolved = isTRUE(as.logical(aib_dash_col(cp, "robust_tie_break_solved"))),
      t90 = aib_dash_split(cp$minimum_coalition_90),
      t95 = aib_dash_split(cp$minimum_coalition_95),
      t99 = aib_dash_split(cp$minimum_coalition_99),
      reached99 = isTRUE(as.logical(cp$target_reached_99)),
      maxCoverage = aib_dash_round(cp$max_anchor_coverage),
      method = aib_dash_chr(aib_dash_col(cp, "coalition_method")),
      greedy = aib_dash_split(aib_dash_col(cp, "greedy_sufficient_coalition")),
      criticalPartners = aib_dash_split(aib_dash_col(cp, "critical_supply_partners")),
      criticalExternal = aib_dash_split(aib_dash_col(cp, "critical_external_dependencies")),
      criticalExternalMarginal = aib_dash_split(aib_dash_col(cp, "critical_external_dependencies_marginal")),
      associatesReach99 = isTRUE(as.logical(aib_dash_col(cp, "associates_reach_99"))),
      external = aib_dash_split(aib_dash_col(cp, "external_dependencies"))
    ),
    nodePolicyNeeds = aib_dash_chr(aib_dash_col(cp, "node_policy_needs")),
    physicalWarnings = aib_dash_split(aib_dash_col(cp, "physical_warnings"), "; "),
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

# The emergent AIB network of the headline variant (scripts/32: emergent_aib_country_summary.csv and
# emergent_aib_edges.csv; strategic scope, production-constrained supply, hold semantics). Both files
# carry both scenarios; only the headline scenario is exported, and the variant is written once.
# Countries keep their four network roles apart (trade supplier, trade buyer, investment host,
# investor) with the summary's own metrics; edges run as goods move (supplier -> buyer: the exporter,
# or the host whose new capacity serves the investor). "headline" = the headline shock, "cumulative" =
# summed over every configured shock. Contributions are shares of a node's demand, summed over the
# nodes (and partners) of the role, so they can exceed 1.
aib_dash_network <- function(res) {
  co <- res$countries
  ed <- res$edges
  if (is.null(co) || !nrow(co) || is.null(ed)) return(NULL)
  sc <- res$meta$headline_scenario
  co <- co[co$scenario == sc, , drop = FALSE]
  ed <- ed[ed$scenario == sc, , drop = FALSE]
  num <- function(r, k, d = 5) if (k %in% names(r)) aib_dash_round(dplyr::coalesce(as.numeric(r[[k]][1]), 0), d) else 0
  int <- function(r, k) if (k %in% names(r)) as.integer(dplyr::coalesce(as.numeric(r[[k]][1]), 0)) else 0L
  side <- function(r, p, trade) {
    out <- list(
      partners = int(r, paste0(p, "_n_partners")), selected = int(r, paste0(p, "_n_selected")),
      headline = num(r, paste0(p, "_headline_contribution")), cumulative = num(r, paste0(p, "_cumulative_contribution")),
      upstream = int(r, paste0(p, "_n_upstream_partners")), midstream = int(r, paste0(p, "_n_midstream_partners")),
      downstream = int(r, paste0(p, "_n_downstream_partners"))
    )
    if (trade) {
      out$dividend <- num(r, paste0(p, "_cumulative_coordination_dividend"))
      out$volumeUsd <- num(r, paste0(p, "_headline_volume_usd"), 0)
    }
    out
  }
  countries <- lapply(seq_len(nrow(co)), function(i) {
    r <- co[i, ]
    list(
      iso3 = r$iso3, roles = aib_dash_split(r$roles, "; "), technologies = aib_dash_split(r$technologies, "; "),
      lostUsd = if ("headline_lost_usd" %in% names(r) && !is.na(r$headline_lost_usd)) aib_dash_round(r$headline_lost_usd, 0) else NA_real_,
      exposedNodes = if ("n_exposed_subsectors" %in% names(r) && !is.na(r$n_exposed_subsectors)) as.integer(r$n_exposed_subsectors) else NA_integer_,
      supplier = side(r, "supplier", TRUE), buyer = side(r, "buyer", TRUE),
      host = side(r, "investment_host", FALSE), investor = side(r, "investor", FALSE)
    )
  })
  edges <- lapply(seq_len(nrow(ed)), function(i) {
    r <- ed[i, ]
    stg <- c(Upstream = isTRUE(r$upstream), Midstream = isTRUE(r$midstream), Downstream = isTRUE(r$downstream))
    list(
      from = r$supplier_iso, to = r$buyer_iso,
      nTrade = as.integer(r$n_trade_selected), nInvestment = as.integer(r$n_investment_selected), nShocks = as.integer(r$n_shocks),
      nodes = aib_dash_split(r$subsectors, "; "), techs = aib_dash_split(r$technologies, "; "), stages = aib_dash_arr(names(stg)[stg]),
      nHighSeverity = as.integer(r$n_high_severity),
      tradeUsd = aib_dash_round(r$headline_trade_volume_usd, 0), trade = aib_dash_round(r$headline_trade_contribution, 5),
      investment = aib_dash_round(r$headline_investment_contribution, 5),
      cumTrade = aib_dash_round(r$cumulative_trade_resilience_contribution, 5), cumDividend = aib_dash_round(r$cumulative_coordination_dividend, 5),
      cumInvestment = aib_dash_round(r$cumulative_investment_resilience_contribution, 5),
      opportunity = aib_dash_round(aib_dash_col(r, "export_opportunity")), friendshore = aib_dash_round(aib_dash_col(r, "friendshore"))
    )
  })
  list(
    variant = aib_dash_variant(res, shocks = TRUE),
    countries = countries,
    edges = edges
  )
}

# The variant fields every network / investment section carries (the headline run).
aib_dash_variant <- function(res, shocks = FALSE) {
  m <- res$meta
  out <- list(shock = m$headline_shock, scenario = m$headline_scenario, shockSemantics = aib_dash_chr(m$headline_semantics),
              supplyConstraint = aib_dash_chr(m$headline_constraint), strategicScope = aib_dash_chr(m$strategic_scope),
              demandScope = aib_dash_chr(m$demand_scope), candidateUniverse = aib_dash_chr(m$candidate_universe), universe = aib_dash_chr(m$universe))
  if (shocks) out$shocks <- aib_dash_arr(vapply(m$config$shocks, function(s) s$id, ""))
  out
}

# Investor -> host flows of the marginal investment simulation, headline variant (scripts/32:
# allied_investment_pairs.csv and the ranked hosts of allied_investment_partnerships.csv). Each ranked
# host of a node is a SEPARATE simulation of the same increment: only rank 1 is the selected host, and
# the flows of different ranks are alternatives that must not be added up. Investor = the economy whose
# lost supply the new capacity serves; host = where the capacity is added. Domestic rows (an economy
# serving its own gap) are left out of the flows. A modelled result: it does not follow the Perspective.
# Only nodes of the strategic scope (classification in_strategic_scope, the flag behind
# nodes[].inStrategicScope) are kept, like the emergent country summary and the compacts.
aib_dash_investment_flows <- function(res, max_rank = 5L) {
  ip <- res$investment_pairs
  iv <- res$investment
  if (is.null(ip) || !nrow(ip)) return(NULL)
  m <- res$meta
  inc <- m$config$investment$headline_increment
  inc <- if (is.null(inc)) 0.1 else as.numeric(inc)
  cl <- res$classification
  scope_ids <- if (!is.null(cl) && "in_strategic_scope" %in% names(cl)) cl$node_id[dplyr::coalesce(as.logical(cl$in_strategic_scope), FALSE)] else NULL
  keep <- function(d) {
    if (!is.null(scope_ids)) d <- d[d$node_id %in% scope_ids, , drop = FALSE]
    d <- d[d$shock_id == m$headline_shock & d$scenario == m$headline_scenario, , drop = FALSE]
    if ("increment" %in% names(d)) d <- d[abs(d$increment - inc) < 1e-9, , drop = FALSE]
    if ("supply_constraint" %in% names(d) && !is.null(m$headline_constraint)) d <- d[d$supply_constraint == m$headline_constraint, , drop = FALSE]
    d[!is.na(d$host_rank) & d$host_rank <= max_rank, , drop = FALSE]
  }
  ip <- keep(ip)
  ip <- ip[ip$relationship != "domestic" & ip$investor_iso != ip$host_iso & dplyr::coalesce(ip$supply_from_new_capacity_usd, 0) > 0, , drop = FALSE]
  ip <- ip[order(ip$node_id, ip$host_rank, -ip$supply_from_new_capacity_usd), , drop = FALSE]
  hosts <- if (is.null(iv) || !nrow(iv)) iv else keep(iv)
  if (!is.null(hosts) && nrow(hosts)) hosts <- hosts[order(hosts$node_id, hosts$host_rank), , drop = FALSE]
  list(
    variant = c(aib_dash_variant(res), list(increment = inc, maxRank = as.integer(max_rank))),
    pairs = lapply(seq_len(nrow(ip)), function(k) list(
      investor = ip$investor_iso[k], host = ip$host_iso[k], node = ip$node_id[k], rank = as.integer(ip$host_rank[k]),
      usd = aib_dash_round(ip$supply_from_new_capacity_usd[k], 0), share = aib_dash_round(ip$supply_from_new_capacity[k], 6),
      lostShare = aib_dash_round(aib_dash_col(ip[k, ], "investor_lost_share"), 6),
      friendshore = aib_dash_round(ip$friendshore[k]), opportunity = aib_dash_round(aib_dash_col(ip[k, ], "export_opportunity"))
    )),
    hosts = if (is.null(hosts) || !nrow(hosts)) list() else lapply(seq_len(nrow(hosts)), function(k) list(
      node = hosts$node_id[k], host = hosts$host_iso[k], rank = as.integer(hosts$host_rank[k]),
      gain = aib_dash_round(hosts$marginal_resilience_gain[k], 5), before = aib_dash_round(hosts$coverage_before[k]),
      after = aib_dash_round(hosts$coverage_after[k]), newCapacityUsed = aib_dash_round(aib_dash_col(hosts[k, ], "new_capacity_used"), 5),
      servedUsd = aib_dash_round(aib_dash_col(hosts[k, ], "additional_demand_served_usd"), 0),
      capabilityTier = suppressWarnings(as.integer(aib_dash_col(hosts[k, ], "capability_tier"))),
      capabilityBasis = aib_dash_chr(aib_dash_col(hosts[k, ], "capability_basis"))
    ))
  )
}

# The whole document: meta, country names for every code used, compacts and nodes; additive: the
# emergent network (`network`) and the investor -> host flows (`investmentFlows`).
aib_dash_bundle <- function(res, top_edges = 40) {
  cfg <- res$meta$config
  nodes <- aib_dash_nodes(res)
  comps <- lapply(seq_len(nrow(res$compacts)), function(i) aib_dash_compact(res$compacts[i, ], res, top_edges))
  network <- aib_dash_network(res)
  invest <- aib_dash_investment_flows(res)
  codes <- unique(c(
    unlist(lapply(network$countries, `[[`, "iso3")), unlist(lapply(network$edges, function(e) c(e$from, e$to))),
    unlist(lapply(invest$pairs, function(p) c(p$investor, p$host))), unlist(lapply(invest$hosts, `[[`, "host")),
    unlist(lapply(comps, function(c) c(unlist(c$coalition$t99), unlist(c$coalition$members), unlist(c$coalition$greedy),
                                         unlist(c$coalition$criticalExternal), unlist(c$coalition$criticalExternalMarginal),
                                         unlist(c$coalition$external), unlist(lapply(c$roles, `[[`, "iso3")),
                                         unlist(lapply(c$trade, function(t) c(t$from, t$to))), unlist(lapply(c$investment, function(t) c(t$investor, t$host)))))),
    unlist(lapply(nodes, function(n) c(n$baseLargest, n$largest, unlist(n$hosts))))
  ))
  codes <- sort(codes[!is.na(codes) & nzchar(codes)])
  nm <- res$country_names
  names_out <- stats::setNames(as.list(ifelse(is.na(nm$country[match(codes, nm$iso3)]), codes, nm$country[match(codes, nm$iso3)])), codes)
  shocks <- lapply(cfg$shocks, function(s) list(id = s$id, label = s$label))
  doc <- list(
    schema = AIB_DASH_SCHEMA, version = AIB_DASH_VERSION,
    meta = list(
      universe = res$meta$universe, headlineShock = res$meta$headline_shock, headlineScenario = res$meta$headline_scenario,
      demandScope = aib_dash_chr(res$meta$demand_scope), candidateUniverse = aib_dash_chr(res$meta$candidate_universe),
      nDemandScope = if (is.null(res$meta$n_demand_scope)) NA_integer_ else res$meta$n_demand_scope,
      shockSemantics = aib_dash_chr(res$meta$headline_semantics), supplyConstraint = aib_dash_chr(res$meta$headline_constraint),
      semantics = aib_dash_arr(unlist(cfg$shocked_demand)), constraints = aib_dash_arr(unlist(cfg$supply_constraint$modes)),
      strategicScope = aib_dash_chr(res$meta$strategic_scope), scopeTechs = aib_dash_arr(res$meta$scope_techs),
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
  # additive sections: absent (not null) when the run has none
  if (!is.null(network)) doc$network <- network
  if (!is.null(invest)) doc$investmentFlows <- invest
  doc
}
