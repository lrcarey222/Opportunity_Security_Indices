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
# variant, with its variant fields. aib_check.R checks them as the TypeScript contract does. Added
# 2026-10-19 (the scenario toggle; optional): the document is the Decoupling scenario, with meta.scenarios
# (meta.supplyScenarios then holds the supply scenarios), node.baseline, the run fields of node.diversify,
# compact.managed and compact.baseline; the Managed Interdependence and Baseline scenarios are separate
# documents (aib_dash_scenario_managed(), aib_dash_scenario_baseline(); schema osi-aib-scenario). Added
# 2026-10-20 (optional): coalition.prospectiveHosts (members in only for the simulated capacity they would
# host; their roles carry the tier "prospective host") and, per ranked host, hostIncrement (the increment
# simulated there, capped at the host's scale) and rankBasis (the criterion that placed it); the host types:
# node / compact / node.diversify specialtyHosts and developmentHosts beside hosts (the scaling hosts), the
# ranked hosts' type (scaling, specialty, development or none), roles "Scaling host" (was "Capacity-building /
# investment host"), "Specialty host", "Development host" and the tier "development host"; node.screenedHost
# (host, gain, cost), the best host the alignment screen keeps out and what the screen costs.

AIB_DASH_SCHEMA <- "osi-aib"
# Version 2 (2026-10-20): meta.scenarios became the scenario list (objects) and the supply scenarios moved to
# meta.supplyScenarios; an app reading version 1 would reject the objects, so it asks for a reload instead.
AIB_DASH_VERSION <- 2L

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
# The EIS index of each technology (sector) when the run used the electro-industrial dictionary
# (res$taxonomy: tech, index_code, index_name); empty for the OSI basket.
aib_dash_index_of <- function(res) {
  tx <- res$taxonomy
  if (is.null(tx) || !nrow(tx) || all(is.na(tx$index_code))) return(NULL)
  tx
}

aib_dash_nodes <- function(res) {
  idx <- aib_dash_index_of(res)
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
  # A screened host is shown only where it would add something: a gain of at least the ranking's tie margin.
  ti <- res$meta$config$investment$tie_resilience
  screen_min_gain <- if (is.null(ti)) 0.005 else as.numeric(ti)
  # The observed (Baseline) supply of each optimized node (res$baseline_supply, scripts/32).
  bs <- res$baseline_supply
  # The diversification scenario (res$diversification, scripts/32): the headline scenario's row per node.
  dv <- res$diversification
  if (!is.null(dv) && nrow(dv)) {
    dv <- dv[is.na(dv$scenario) | dv$scenario == res$meta$headline_scenario, , drop = FALSE]
    dv <- dv[!duplicated(dv$node_id), , drop = FALSE]
  }
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
      indexCode = if (is.null(idx)) NA_character_ else aib_dash_chr(idx$index_code[match(strsplit(r$techs, "; ")[[1]][1], idx$tech)]),
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
      # Absent (not null) when the run has no diversification scenario.
      diversify = {
        d <- if (is.null(dv) || !nrow(dv)) NULL else dv[dv$node_id == r$node_id, , drop = FALSE]
        if (is.null(d) || !nrow(d)) NULL else list(
          status = aib_dash_chr(d$status), currentHhi = aib_dash_round(d$current_hhi), benchmarkHhi = aib_dash_round(d$benchmark_hhi),
          basis = aib_dash_chr(d$benchmark_basis), cap = aib_dash_round(d$cap),
          capped = if (is.na(d$target_iso)) aib_dash_arr(character()) else aib_dash_split(d$target_iso, "+"),
          excess = aib_dash_round(aib_dash_col(d, "excess_share")),
          static = aib_dash_round(aib_dash_col(d, "static_coverage")), existing = aib_dash_round(aib_dash_col(d, "existing_coverage")),
          optimized = aib_dash_round(aib_dash_col(d, "optimized_coverage")), residual = aib_dash_round(aib_dash_col(d, "residual_gap")),
          residualUsd = aib_dash_round(aib_dash_col(d, "residual_gap_usd"), 0),
          hosts = aib_dash_split(aib_dash_col(d, "priority_investment_hosts"), "; "),
          specialtyHosts = aib_dash_split(aib_dash_col(d, "specialty_hosts"), "; "),
          developmentHosts = aib_dash_split(aib_dash_col(d, "development_hosts"), "; "),
          partners = aib_dash_chr(aib_dash_col(d, "key_trade_partners")),
          # A class only where the scenario ran (the node is above its benchmark).
          cls = if (identical(d$status, "diversified to the benchmark")) aib_dash_chr(aib_dash_col(d, "diversify_class")) else NA_character_,
          # The scenario toggle (2026-10-19): the run's figures where it ran, null otherwise. lostUsd: the
          # supply that shifts away from the capped suppliers (excess x demandUsd); hhi / largest / shares:
          # the optimized state under the cap, on node.hhi's basis (share of served supply).
          lostUsd = aib_dash_ran(d, "lost_usd", 0), dividend = aib_dash_ran(d, "coordination_dividend"),
          hhi = aib_dash_ran(d, "optimized_hhi"),
          largest = if (identical(d$status, "diversified to the benchmark")) aib_dash_chr(aib_dash_col(d, "optimized_largest_supplier")) else NA_character_,
          largestShare = aib_dash_ran(d, "optimized_largest_share"), top3Share = aib_dash_ran(d, "optimized_top3_share"),
          newLinks = aib_dash_ran_int(d, "n_new_links"), expandedLinks = aib_dash_ran_int(d, "n_expanded_links"))
      },
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
      # The leading hosts by type (2026-10-20): scaling (hosts), specialty and development.
      hosts = aib_dash_split(r$priority_investment_hosts, "; "),
      specialtyHosts = aib_dash_split(aib_dash_col(r, "specialty_hosts"), "; "),
      developmentHosts = aib_dash_split(aib_dash_col(r, "development_hosts"), "; "),
      bestGain = aib_dash_round(r$best_marginal_gain),
      # The best host the alignment screen keeps out, its gain and the alignment cost (the gain the
      # eligible hosts fall short of it by, 0 when one does as well); absent where no host was screened
      # or the screened host would add less than the tie margin (aib_node_classification.csv has them all).
      screenedHost = {
        sh <- aib_dash_chr(aib_dash_col(r, "screened_host"))
        sg <- suppressWarnings(as.numeric(aib_dash_col(r, "screened_host_gain")))
        if (is.na(sh) || is.na(sg) || sg < screen_min_gain - 1e-12) NULL else
          list(host = sh, gain = aib_dash_round(sg, 5), cost = aib_dash_round(aib_dash_col(r, "alignment_cost"), 5))
      },
      partners = if (nrow(t)) t$key_trade_partners[1] else NA_character_,
      treatment = if (nrow(t)) t$recommended_policy_treatment[1] else NA_character_,
      compact = if (nrow(t) && !is.na(t$compact_id[1]) && nzchar(t$compact_id[1])) t$compact_id[1] else NA_character_,
      # OSI technology x stage cells (the dictionary's nodes reach the Sector and Country views
      # through the crosswalk of scripts/31; yf_cells lists them all).
      yfCells = aib_dash_arr(unique(stats::na.omit(c(t$yf_cell, if ("yf_cells" %in% names(t) && nrow(t)) unlist(strsplit(stats::na.omit(t$yf_cells), "; ")))))),
      yfSubcomponents = if (nrow(t)) aib_dash_split(t$yf_subcomponent_id[1], "; ") else aib_dash_arr(character()),
      tightestCostless = if (nrow(f)) aib_dash_round(f$tightest_costless_max_share[1]) else NA_real_,
      minRequiredDiversification = if (nrow(f)) aib_dash_round(f$min_required_diversification[1]) else NA_real_,
      frontier = lapply(seq_len(nrow(f)), function(k) list(
        cap = f$max_supplier_share[k], coverage = aib_dash_round(f$optimized_coverage[k]),
        largestShare = aib_dash_round(f$optimized_largest_share[k]), largest = f$optimized_largest_supplier[k],
        instantaneousN1 = aib_dash_round(f$instantaneous_n1_retained_coverage[k]), n1Reoptimized = aib_dash_round(f$n1_reoptimized_coverage[k]),
        n1Removed = aib_dash_chr(f$n1_removed_supplier[k]), specialization = aib_dash_round(f$specialization_score[k])
      )),
      # The node as it is today (2026-10-19; absent for a node that was not optimized or a run without the
      # observed supply): observed, before any shock, on the scope-supply basis.
      baseline = if (is.null(bs) || !r$node_id %in% bs$node_id) NULL else list(
        hhi = aib_dash_round(aib_dash_col(r, "baseline_hhi")), top3Share = aib_dash_round(aib_dash_col(r, "baseline_top3_share")),
        largest = aib_dash_chr(aib_dash_col(r, "baseline_largest_supplier")),
        largestShare = aib_dash_round(aib_dash_col(r, "baseline_largest_share")),
        suppliers = aib_dash_supplier_tuples(aib_dash_top_suppliers(bs, r$node_id, 5L)$top)
      )
    )
  })
}

# A diversification-scenario figure where the scenario ran (status "diversified to the benchmark"), else NA.
aib_dash_ran <- function(d, col, digits = 4) {
  if (!identical(d$status, "diversified to the benchmark")) return(NA_real_)
  aib_dash_round(aib_dash_col(d, col), digits)
}
aib_dash_ran_int <- function(d, col) {
  if (!identical(d$status, "diversified to the benchmark")) return(NA_integer_)
  suppressWarnings(as.integer(aib_dash_col(d, col)))
}

# The observed suppliers of a set of nodes (supply: res$baseline_supply, one row per node and exporter with
# supply_usd and the node's demand_usd): each exporter's supply summed over the nodes, as a share of the
# nodes' summed demand; `top` the largest `n` (ties by ISO3 code) and `hhi` the HHI over every exporter
# (the shares sum to 1). NULL when none of the nodes has observed supply.
aib_dash_top_suppliers <- function(supply, ids, n = 5L) {
  if (is.null(supply) || !nrow(supply)) return(NULL)
  s <- supply[supply$node_id %in% ids, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  dem <- sum(s$demand_usd[!duplicated(s$node_id)])
  by <- tapply(s$supply_usd, s$exporter_iso, sum)
  o <- order(-as.numeric(by), names(by))
  share <- as.numeric(by[o]) / dem
  list(demand_usd = dem, hhi = sum(share^2),
       top = utils::head(tibble::tibble(exporter_iso = names(by)[o], supply_usd = as.numeric(by[o]), share = share), n))
}

# [ISO3, share] tuples (usd = FALSE) or [ISO3, USD, share] tuples (usd = TRUE): JSON arrays of mixed type.
aib_dash_supplier_tuples <- function(top, usd = FALSE) {
  if (is.null(top) || !nrow(top)) return(list())
  lapply(seq_len(nrow(top)), function(k) {
    if (usd) list(top$exporter_iso[k], aib_dash_round(top$supply_usd[k], 0), aib_dash_round(top$share[k])) else
      list(top$exporter_iso[k], aib_dash_round(top$share[k]))
  })
}

# A compact's coverage metrics (one row of aib_compact_candidates[_managed].csv).
aib_dash_metrics <- function(cp) {
  list(
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
    anchorFragmented = aib_dash_round(aib_dash_col(cp, "anchor_coverage_fragmented")),
    anchorCompact = aib_dash_round(aib_dash_col(cp, "anchor_coverage_compact")),
    compactGain = aib_dash_round(aib_dash_col(cp, "compact_gain_over_fragmented"))
  )
}

# One compact record: overview, roles, coverage comparison, node list, the trade and investment
# relationships behind it (the largest `top_edges` of each) and the coalition path. `shock`, `members` and
# `paths` select the scenario (default: the headline shock's compact layer; the Managed Interdependence
# file passes the diversification shock with res$compact_members_managed / res$coalition_paths_managed).
aib_dash_compact <- function(cp, res, top_edges = 40, shock = res$meta$headline_shock, members = res$compact_members,
                             paths = res$coalition_paths) {
  ids <- res$compact_nodes$node_id[!is.na(res$compact_nodes$compact_id) & res$compact_nodes$compact_id == cp$compact_id]
  ids <- unique(ids)
  mem <- members[members$compact_id == cp$compact_id, , drop = FALSE]
  pth <- paths[paths$compact_id == cp$compact_id, , drop = FALSE]
  pth <- pth[order(pth$step), , drop = FALSE]
  hs <- shock
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
    indices = {
      idx <- aib_dash_index_of(res)
      if (is.null(idx)) aib_dash_arr(character()) else
        aib_dash_arr(sort(unique(stats::na.omit(idx$index_code[match(strsplit(cp$technology, "; ")[[1]], idx$tech)]))))
    },
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
      # Members in only for the simulated capacity they would host (their tier: "prospective host").
      prospectiveHosts = aib_dash_split(aib_dash_col(cp, "prospective_hosts")),
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
    specialtyHosts = aib_dash_split(aib_dash_col(cp, "specialty_hosts")),
    developmentHosts = aib_dash_split(aib_dash_col(cp, "development_hosts")),
    technologyPartners = aib_dash_split(cp$technology_partners),
    bridges = aib_dash_split(cp$bridge_partners),
    associates = aib_dash_split(cp$associate_partners),
    roles = roles,
    metrics = aib_dash_metrics(cp),
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
# `countries`, `edges`, `variant` and `scenario` select another scenario's network (the Managed
# Interdependence network of the diversification shock alone; the observed Baseline network, scenario
# "observed").
aib_dash_network <- function(res, countries = res$countries, edges = res$edges, variant = aib_dash_variant(res, shocks = TRUE),
                             scenario = res$meta$headline_scenario) {
  co <- countries
  ed <- edges
  if (is.null(co) || !nrow(co) || is.null(ed)) return(NULL)
  sc <- scenario
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
    variant = variant,
    countries = countries,
    edges = edges
  )
}

# The variant fields every network / investment section carries (the headline run). `shock` and
# `shock_ids` name another scenario's shock and the shocks its network sums over (the Managed
# Interdependence network: the diversification shock alone).
aib_dash_variant <- function(res, shocks = FALSE, shock = res$meta$headline_shock, shock_ids = NULL) {
  m <- res$meta
  out <- list(shock = shock, scenario = m$headline_scenario, shockSemantics = aib_dash_chr(m$headline_semantics),
              supplyConstraint = aib_dash_chr(m$headline_constraint), strategicScope = aib_dash_chr(m$strategic_scope),
              demandScope = aib_dash_chr(m$demand_scope), candidateUniverse = aib_dash_chr(m$candidate_universe), universe = aib_dash_chr(m$universe))
  # The supply shocks the network sums over (scripts/32 leaves the diversification scenario out).
  if (shocks) {
    out$shocks <- if (is.null(shock_ids)) aib_dash_arr(vapply(Filter(function(s) !identical(s$type, "diversify"), m$config$shocks), function(s) s$id, "")) else
      aib_dash_arr(shock_ids)
  }
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
aib_dash_investment_flows <- function(res, max_rank = 5L, shock = res$meta$headline_shock) {
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
    d <- d[d$shock_id == shock & d$scenario == m$headline_scenario, , drop = FALSE]
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
    variant = c(aib_dash_variant(res, shock = shock), list(increment = inc, maxRank = as.integer(max_rank))),
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
      capabilityBasis = aib_dash_chr(aib_dash_col(hosts[k, ], "capability_basis")),
      hostIncrement = aib_dash_round(aib_dash_col(hosts[k, ], "host_increment"), 5),
      type = aib_dash_chr(aib_dash_col(hosts[k, ], "host_type")),
      rankBasis = aib_dash_chr(aib_dash_col(hosts[k, ], "rank_basis"))
    ))
  )
}

# The whole document: meta, country names for every code used, compacts and nodes; additive: the
# emergent network (`network`) and the investor -> host flows (`investmentFlows`).
aib_dash_bundle <- function(res, top_edges = 40) {
  cfg <- res$meta$config
  nodes <- aib_dash_nodes(res)
  drop_null <- function(x) x[!vapply(x, is.null, logical(1))]
  nodes <- lapply(nodes, drop_null)
  comps <- lapply(seq_len(nrow(res$compacts)), function(i) aib_dash_compact(res$compacts[i, ], res, top_edges))
  # The scenario toggle (2026-10-19): each compact under Managed Interdependence (its nodes' figures) and as
  # it is today (observed suppliers); absent when the run has no diversification scenario / observed supply.
  dv_run <- aib_dash_scenario_rows(res)
  node_demand <- stats::setNames(as.numeric(res$classification$demand_usd), res$classification$node_id)
  comps <- lapply(comps, function(c) {
    ids <- as.character(c$nodes)
    if (!is.null(dv_run)) c$managed <- aib_dash_compact_managed(ids, dv_run, node_demand)
    b <- aib_dash_top_suppliers(res$baseline_supply, ids, 5L)
    if (!is.null(b)) c$baseline <- list(hhi = aib_dash_round(b$hhi), suppliers = aib_dash_supplier_tuples(b$top))
    c
  })
  network <- aib_dash_network(res)
  invest <- aib_dash_investment_flows(res)
  codes <- unique(c(
    unlist(lapply(network$countries, `[[`, "iso3")), unlist(lapply(network$edges, function(e) c(e$from, e$to))),
    unlist(lapply(invest$pairs, function(p) c(p$investor, p$host))), unlist(lapply(invest$hosts, `[[`, "host")),
    unlist(lapply(comps, function(c) c(unlist(c$coalition$t99), unlist(c$coalition$members), unlist(c$coalition$greedy),
                                         unlist(c$coalition$criticalExternal), unlist(c$coalition$criticalExternalMarginal),
                                         unlist(c$coalition$external), unlist(lapply(c$roles, `[[`, "iso3")),
                                         unlist(lapply(c$trade, function(t) c(t$from, t$to))), unlist(lapply(c$investment, function(t) c(t$investor, t$host))),
                                         aib_dash_tuple_codes(c$baseline$suppliers)))),
    unlist(lapply(nodes, function(n) c(n$baseLargest, n$largest, unlist(n$hosts), n$baseline$largest, aib_dash_tuple_codes(n$baseline$suppliers),
                                       if (is.list(n$diversify)) n$diversify$largest, if (is.list(n$screenedHost)) n$screenedHost$host)))
  ))
  names_out <- aib_dash_country_names(res, codes)
  shocks <- lapply(cfg$shocks, function(s) list(id = s$id, label = s$label))
  defs <- aib_dash_scenario_defs(res)
  doc <- list(
    schema = AIB_DASH_SCHEMA, version = AIB_DASH_VERSION,
    meta = list(
      universe = res$meta$universe, headlineShock = res$meta$headline_shock, headlineScenario = res$meta$headline_scenario,
      demandScope = aib_dash_chr(res$meta$demand_scope), candidateUniverse = aib_dash_chr(res$meta$candidate_universe),
      nDemandScope = if (is.null(res$meta$n_demand_scope)) NA_integer_ else res$meta$n_demand_scope,
      shockSemantics = aib_dash_chr(res$meta$headline_semantics), supplyConstraint = aib_dash_chr(res$meta$headline_constraint),
      semantics = aib_dash_arr(unlist(cfg$shocked_demand)), constraints = aib_dash_arr(unlist(cfg$supply_constraint$modes)),
      strategicScope = aib_dash_chr(res$meta$strategic_scope), scopeTechs = aib_dash_arr(res$meta$scope_techs),
      taxonomy = aib_dash_chr(if (is.null(res$meta$taxonomy)) "master" else res$meta$taxonomy), dictionaryScope = aib_dash_chr(res$meta$dictionary_scope),
      indices = {
        idx <- aib_dash_index_of(res)
        if (is.null(idx)) list() else unname(lapply(split(idx, idx$index_code), function(t) list(
          code = t$index_code[1], name = t$index_name[1], sectors = aib_dash_arr(sort(unique(t$tech))))))
      },
      tradeYear = res$meta$trade_year, built = res$meta$built,
      # meta.scenarios: the scenario list of the toggle (2026-10-19) when the run has the other scenarios, the
      # supply scenarios moving to meta.supplyScenarios (contract.ts: supplyScenariosOf(), scenarioDefsOf());
      # else the supply scenarios, as before.
      shocks = shocks, scenarios = if (is.null(defs)) aib_dash_arr(unlist(cfg$scenarios)) else defs,
      classes = aib_dash_arr(aib_node_classes()),
      targets = aib_dash_num_arr(unlist(cfg$compacts$coverage_targets)),
      thresholds = cfg$classification,
      diversify = {
        ds <- Filter(function(s) identical(s$type, "diversify"), cfg$shocks)
        if (!length(ds)) NULL else list(shockId = ds[[1]]$id, label = aib_dash_chr(ds[[1]]$label),
                                        benchmark = aib_dash_chr(if (is.null(ds[[1]]$benchmark)) "hs4" else ds[[1]]$benchmark),
                                        minExcess = aib_dash_round(if (is.null(ds[[1]]$min_excess)) 0 else ds[[1]]$min_excess))
      },
      supplyScenarios = if (is.null(defs)) NULL else aib_dash_arr(unlist(cfg$scenarios))
    ),
    countries = names_out,
    compacts = comps,
    nodes = nodes
  )
  doc$meta <- drop_null(doc$meta)
  # additive sections: absent (not null) when the run has none
  if (!is.null(network)) doc$network <- network
  if (!is.null(invest)) doc$investmentFlows <- invest
  doc
}

# ISO3 -> name for every code (the code itself where res$country_names has no name), sorted.
aib_dash_country_names <- function(res, codes) {
  codes <- sort(unique(codes[!is.na(codes) & nzchar(codes)]))
  nm <- res$country_names
  stats::setNames(as.list(ifelse(is.na(nm$country[match(codes, nm$iso3)]), codes, nm$country[match(codes, nm$iso3)])), codes)
}

# The ISO3 codes of [ISO3, ...] supplier tuples.
aib_dash_tuple_codes <- function(tuples) {
  if (is.null(tuples) || !length(tuples)) return(character())
  vapply(tuples, function(t) as.character(t[[1]]), "")
}

# ---- The scenario toggle (2026-10-19) -------------------------------------------------------------------
# Three scenarios: Baseline (observed reference-year trade, before any shock), Managed Interdependence (the
# diversification shock: no supplier is removed; each supplier above its node's benchmark cap gives up the
# excess) and Decoupling (the headline shock under hold: the main document). The main document carries
# meta.scenarios, node.baseline, the run fields of node.diversify, compact.managed and compact.baseline; the
# other two scenarios have lazily loaded files of their own (aib_dash_scenario_managed(),
# aib_dash_scenario_baseline()), written by scripts/84 and named in manifest.files.scenarios.

AIB_DASH_SCENARIO_SCHEMA <- "osi-aib-scenario"
AIB_DASH_SCENARIO_VERSION <- 1L

# The diversification shock id (res$scenario_shock, else the configured shock of type "diversify"); NULL when none.
aib_dash_div_id <- function(res) {
  if (!is.null(res$scenario_shock) && !is.na(res$scenario_shock)) return(res$scenario_shock)
  ds <- Filter(function(s) identical(s$type, "diversify"), res$meta$config$shocks)
  if (length(ds)) ds[[1]]$id else NULL
}

# meta.scenarios: {id, label, shockId, words} for Baseline, Managed Interdependence and Decoupling, in that
# order; NULL (meta.scenarios keeps the supply scenarios) when the run has neither the observed supply
# nor the Managed Interdependence results.
aib_dash_scenario_defs <- function(res) {
  div <- aib_dash_div_id(res)
  has_base <- !is.null(res$baseline_supply)
  has_managed <- !is.null(div) && !is.null(res$compacts_managed)
  if (!has_base && !has_managed) return(NULL)
  yr <- if (is.null(res$meta$trade_year)) "the reference year" else res$meta$trade_year
  out <- list()
  if (has_base) out[[length(out) + 1]] <- list(id = "baseline", label = "Baseline", shockId = NA_character_,
                                              words = sprintf("Trade as it is today (%s), before any shock.", yr))
  if (has_managed) out[[length(out) + 1]] <- list(
    id = "managed", label = "Managed Interdependence", shockId = div,
    words = paste("No supplier is removed. Where a supply-chain step is more concentrated than comparable goods, no supplier may",
                  "hold more than the share that would bring it to that benchmark; trade and new capacity replace the excess."))
  out[[length(out) + 1]] <- list(id = "decoupling", label = "Decoupling", shockId = res$meta$headline_shock,
                                 words = "China stops supplying the allied market (it keeps buying); trade and new capacity replace what it supplied.")
  out
}

# The diversification scenario's node rows where it ran (res$diversification, headline scenario, status
# "diversified to the benchmark"); NULL when the run has none.
aib_dash_scenario_rows <- function(res) {
  dv <- res$diversification
  if (is.null(dv) || !nrow(dv) || !"status" %in% names(dv)) return(NULL)
  dv <- dv[dv$status == "diversified to the benchmark" & !is.na(dv$scenario) & dv$scenario == res$meta$headline_scenario, , drop = FALSE]
  dv[!duplicated(dv$node_id), , drop = FALSE]
}

# compact.managed: a compact's nodes under the diversification scenario, demand-weighted over ALL its nodes
# (`ids`). dv: the rows where the scenario ran (node_id, demand_usd, lost_usd, static_coverage,
# existing_coverage, optimized_coverage, residual_gap_usd, diversify_class); a node without one counts as
# unchanged (its demand from `node_demand`, coverage 1, nothing lost). classMix: the scenario classes'
# shares of its lost supply, every class counted ("BUILD 78%; LOW INTERVENTION 22%"); null when no node ran.
aib_dash_compact_managed <- function(ids, dv, node_demand) {
  d <- dv[dv$node_id %in% ids, , drop = FALSE]
  d <- d[!duplicated(d$node_id), , drop = FALSE]
  u <- setdiff(ids, d$node_id)
  dem_u <- sum(as.numeric(node_demand[u]), na.rm = TRUE)
  D <- sum(d$demand_usd) + dem_u
  w <- function(x) if (D > 0) (sum(x * d$demand_usd) + dem_u) / D else NA_real_
  res_usd <- sum(d$residual_gap_usd)
  mix <- if (nrow(d)) aib_class_mix(d$diversify_class, d$lost_usd, keep = aib_node_classes()) else NULL
  list(
    nodesRun = nrow(d),
    demandUsd = aib_dash_round(D, 0), lostUsd = aib_dash_round(sum(d$lost_usd), 0),
    static = aib_dash_round(w(d$static_coverage)), existing = aib_dash_round(w(d$existing_coverage)),
    optimized = aib_dash_round(w(d$optimized_coverage)),
    residual = aib_dash_round(if (D > 0) res_usd / D else NA_real_), residualUsd = aib_dash_round(res_usd, 0),
    classMix = if (nrow(d)) aib_class_mix_text(mix) else NA_character_
  )
}

# scenario-managed.<h8>.json: the headline compacts (same ids and nodes) solved under the diversification
# shock (res$compacts_managed, compact_members_managed, coalition_paths_managed; scripts/32), the network of
# that shock alone (res$edges_managed, countries_managed: its headline fields are the scenario's and cum* =
# headline) and its investor -> host flows (same increment and ranks as the main document). A compact the
# scenario did not run on has coalition null with the reason, no roles, path or relationships, and its
# metrics as unchanged. NULL when the run has no Managed Interdependence results.
aib_dash_scenario_managed <- function(res, top_edges = 40) {
  div <- aib_dash_div_id(res)
  cm <- res$compacts_managed
  if (is.null(div) || is.null(cm) || !nrow(cm)) return(NULL)
  empty <- aib_dash_arr(character())
  comps <- lapply(res$compacts$compact_id, function(cid) {
    row <- cm[cm$compact_id == cid, , drop = FALSE]
    n_run <- if (nrow(row)) as.integer(dplyr::coalesce(row$n_nodes_run[1], 0L)) else 0L
    if (!nrow(row) || n_run == 0L || is.na(row$coalition_method[1])) {
      return(list(id = cid, nodesRun = n_run, coalition = NULL,
                  coalitionReason = if (nrow(row)) aib_dash_chr(aib_dash_col(row, "coalition_reason")) else "the scenario did not run on any node of this compact",
                  type = NA_character_, archetype = NA_character_, policyNeeds = empty, secondaryClass = NA_character_,
                  anchors = empty, suppliers = empty, hosts = empty, technologyPartners = empty, bridges = empty, associates = empty,
                  roles = list(), metrics = if (nrow(row)) aib_dash_metrics(row[1, ]) else NULL, path = list(), trade = list(), investment = list()))
    }
    full <- aib_dash_compact(row[1, ], res, top_edges, shock = div, members = res$compact_members_managed, paths = res$coalition_paths_managed)
    # The compact's type, archetype and policy needs under the scenario (its classes, compact gain and coalition size).
    list(id = cid, nodesRun = n_run, coalition = full$coalition, coalitionReason = NA_character_,
         type = full$type, archetype = full$archetype, policyNeeds = full$policyNeeds, secondaryClass = full$secondaryClass,
         anchors = full$anchors, suppliers = full$suppliers, hosts = full$hosts, technologyPartners = full$technologyPartners,
         bridges = full$bridges, associates = full$associates, roles = full$roles, metrics = full$metrics, path = full$path,
         trade = full$trade, investment = full$investment)
  })
  network <- aib_dash_network(res, countries = res$countries_managed, edges = res$edges_managed,
                              variant = aib_dash_variant(res, shocks = TRUE, shock = div, shock_ids = div))
  invest <- aib_dash_investment_flows(res, shock = div)
  codes <- c(
    unlist(lapply(network$countries, `[[`, "iso3")), unlist(lapply(network$edges, function(e) c(e$from, e$to))),
    unlist(lapply(invest$pairs, function(p) c(p$investor, p$host))), unlist(lapply(invest$hosts, `[[`, "host")),
    unlist(lapply(comps, function(c) c(unlist(c$coalition$t90), unlist(c$coalition$t95), unlist(c$coalition$t99), unlist(c$coalition$members),
                                         unlist(c$coalition$greedy), unlist(c$coalition$criticalPartners), unlist(c$coalition$criticalExternal),
                                         unlist(c$coalition$criticalExternalMarginal), unlist(c$coalition$external),
                                         unlist(c$anchors), unlist(c$suppliers), unlist(c$hosts), unlist(c$technologyPartners), unlist(c$bridges),
                                         unlist(c$associates), unlist(lapply(c$roles, `[[`, "iso3")), unlist(lapply(c$roles, function(r) unlist(r$substitutes))),
                                         unlist(lapply(c$path, function(s) c(s$added, unlist(s$members)))),
                                         unlist(lapply(c$trade, function(t) c(t$from, t$to))), unlist(lapply(c$investment, function(t) c(t$investor, t$host))))))
  )
  doc <- list(schema = AIB_DASH_SCENARIO_SCHEMA, version = AIB_DASH_SCENARIO_VERSION, scenario = "managed", shockId = div,
              created = res$meta$built, countries = aib_dash_country_names(res, codes), compacts = comps)
  if (!is.null(network)) doc$network <- network
  if (!is.null(invest)) doc$investmentFlows <- invest
  doc
}

# scenario-baseline.<h8>.json: trade as it is today. network: the observed supplier -> buyer pairs of the
# strategic-scope nodes, demand-scope buyers, pairs with at least res$baseline_min_share of node demand
# summed over the nodes (res$edges_baseline, countries_baseline; scripts/32): trade = the observed share,
# tradeUsd = the observed USD, investment 0, cum* = headline, nShocks 0, no importer exposure, variant
# shock "baseline" / scenario "observed" / no shocks. nodes and compacts: their top `top` observed suppliers
# [ISO3, USD, share of the node's / compact's allied demand]. NULL when the run has no observed supply.
aib_dash_scenario_baseline <- function(res, top = 10L) {
  bs <- res$baseline_supply
  if (is.null(bs) || !nrow(bs)) return(NULL)
  m <- res$meta
  variant <- list(shock = "baseline", scenario = "observed", shockSemantics = NA_character_, supplyConstraint = NA_character_,
                  strategicScope = aib_dash_chr(m$strategic_scope), demandScope = aib_dash_chr(m$demand_scope),
                  candidateUniverse = aib_dash_chr(m$candidate_universe), universe = aib_dash_chr(m$universe), shocks = aib_dash_arr(character()))
  ed <- res$edges_baseline
  # The pseudo-shock is not a shock: no edge counts one.
  if (!is.null(ed) && nrow(ed)) ed$n_shocks <- 0L
  network <- aib_dash_network(res, countries = res$countries_baseline, edges = ed, variant = variant, scenario = "observed")
  ids <- res$classification$node_id[res$classification$node_id %in% bs$node_id]
  nodes <- lapply(ids, function(id) list(id = id, suppliers = aib_dash_supplier_tuples(aib_dash_top_suppliers(bs, id, top)$top, usd = TRUE)))
  comps <- lapply(seq_len(nrow(res$compacts)), function(i) {
    cid <- res$compacts$compact_id[i]
    cids <- unique(res$compact_nodes$node_id[!is.na(res$compact_nodes$compact_id) & res$compact_nodes$compact_id == cid])
    b <- aib_dash_top_suppliers(bs, cids, top)
    list(id = cid, suppliers = aib_dash_supplier_tuples(if (is.null(b)) NULL else b$top, usd = TRUE))
  })
  codes <- c(unlist(lapply(network$countries, `[[`, "iso3")), unlist(lapply(network$edges, function(e) c(e$from, e$to))),
             unlist(lapply(nodes, function(n) aib_dash_tuple_codes(n$suppliers))), unlist(lapply(comps, function(c) aib_dash_tuple_codes(c$suppliers))))
  doc <- list(schema = AIB_DASH_SCENARIO_SCHEMA, version = AIB_DASH_SCENARIO_VERSION, scenario = "baseline", shockId = NA_character_,
              created = m$built, countries = aib_dash_country_names(res, codes))
  if (!is.null(network)) doc$network <- network
  doc$nodes <- nodes
  doc$compacts <- comps
  doc
}
