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
# 2026-10-20 (optional): `strategic`, the strategic layer of scripts/33 (Strategic Sectoral Compacts, their
# workstreams, Strategic AIB Roles, compact-specific country roles and bilateral theses; aib_dash_strategic()).
# 2026-10-28 (version 3, the dashboard's stage migration; optional): node.yfCellsLegacy and
# yfSubcomponentsLegacy, each node's three-stage cells (aib_dash_legacy_cells()), for the joins with the
# three-stage Country, Sector and Strategic bundles; and the aggregated nodes' labels name their six-stage
# stage (aib_dash_node_label()).

AIB_DASH_SCHEMA <- "osi-aib"
# Version 3 (2026-10-28): the six supply-chain stages (below), which the dashboard's contract reads since then;
# an app reading version 2 would reject the six-stage document, so it asks for a reload instead. Version 2
# (2026-10-20): meta.scenarios became the scenario list (objects) and the supply scenarios moved to
# meta.supplyScenarios.
AIB_DASH_VERSION <- 3L

# Stages (2026-10-27): node.stage, compact.stages and network edge stages are the six supply-chain
# stages (SUPPLY_CHAIN_STAGES, R/utils/supply_chain_stages.R, which the caller sources) in chain order,
# and the network's per-side partner counts are keyed by them in lower case (extraction, refining,
# components, assembly, deployment, recovery) instead of upstream / midstream / downstream.
# aib_check_stages() (R/dashboard/aib/aib_check.R) checks node.stage and compact.stages; aib_check_network()
# checks the edge stages and, through aib_check_side(), the per-side stage counts.
aib_dash_stage_keys <- function() tolower(SUPPLY_CHAIN_STAGES)
aib_dash_stage_order <- function(x) {
  x <- as.character(x)
  aib_dash_arr(x[order(match(x, SUPPLY_CHAIN_STAGES, nomatch = length(SUPPLY_CHAIN_STAGES) + 1L))])
}

aib_dash_arr <- function(x) I(if (is.null(x)) character() else as.character(x))
aib_dash_num_arr <- function(x) I(if (is.null(x)) numeric() else as.numeric(x))
aib_dash_split <- function(x, sep = " ") {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(aib_dash_arr(character()))
  aib_dash_arr(strsplit(x, sep, fixed = TRUE)[[1]])
}
aib_dash_round <- function(x, d = 4) ifelse(is.na(x), NA_real_, round(as.numeric(x), d))
aib_dash_chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) NA_character_ else as.character(x)
aib_dash_col <- function(r, col) if (col %in% names(r)) r[[col]][1] else NA

# A node's label as the bundle shows it. A data-quality aggregate built before 2026-10-28 is labelled with its
# source stage ("Uranium/fuel Upstream: aggregated low-coverage sub-sectors (...)", aib_quality_merge_map());
# the label names the node's six-stage stage instead ("Uranium/fuel Extraction: aggregated ..."). Its id
# keeps the source stage ("uranium-fuel-upstream--aggregated"): the strategic configs are keyed by it. Every
# other label, and a stage that is not one of the six, leaves the label as it is.
aib_dash_node_label <- function(label, stage) {
  if (is.na(label) || !is_supply_chain_stage(stage)) return(label)
  sub("^(.*) (Upstream|Midstream|Downstream): aggregated ", paste0("\\1 ", stage, ": aggregated "), label)
}

# Each node's three-stage cells (node.yfCellsLegacy / yfSubcomponentsLegacy, 2026-10-28): a table node_id,
# yf_cells ("batteries-midstream; ...") and osi_subcomponents ("batteries-midstream::Anode Materials; ..."),
# the cells of the three-stage Country, Sector and Strategic bundles of 2026-10-22, which the six-stage cells
# do not give back by roll-up (the paper filed refined materials under Midstream; Refining rolls up to
# Upstream). taxonomy "dictionary": the crosswalk of each node's codes (node_codes: node_id, hs6) to the OSI
# master's three-stage supply_chain column (aib_osi_crosswalk(stage_col = "supply_chain"), the same crosswalk
# scripts/31 builds at six stages), weighted by code_trade (hs6, value) when given, else by counting codes:
# the set of cells is the same either way, only their order (largest first) depends on the weights. Any other
# taxonomy (the master): every node is its own cells, each technology at the node's source stage
# (node_cells: node_id, tech, source_stage; node_cells_detail adds sub_sector), as scripts/32 builds the
# six-stage ones. A node with no basket code is absent (the exporter writes []). The result's attribute
# "known_nodes" lists every node the inputs hold, so the exporter tells a known node with no basket code
# (it writes []) from a node the inputs do not know, as in inputs of another run (it writes nothing, and the
# dashboard falls back to the roll-up). Pure.
aib_dash_legacy_cells <- function(node_codes, osi_master, code_trade = NULL, taxonomy = "dictionary", node_cells = NULL, node_cells_detail = NULL) {
  known <- unique(as.character(c(node_codes$node_id, if (!is.null(node_cells)) node_cells$node_id)))
  if (identical(taxonomy, "dictionary")) {
    xw <- aib_osi_crosswalk(node_codes, osi_master, code_trade, stage_col = "supply_chain")
    out <- xw[!is.na(xw$yf_cells), c("node_id", "yf_cells", "osi_subcomponents"), drop = FALSE]
    attr(out, "known_nodes") <- known
    return(out)
  }
  require_columns(node_cells, c("node_id", "tech", "source_stage"), label = "node_cells")
  cell_id <- function(t) paste0(aib_slug(t$tech), "-", aib_slug(t$source_stage))
  nc <- tibble::as_tibble(node_cells)
  cells <- tibble::tibble(node_id = nc$node_id, cell = cell_id(nc)) %>% dplyr::distinct() %>%
    dplyr::group_by(node_id) %>% dplyr::summarise(yf_cells = paste(cell, collapse = "; "), .groups = "drop")
  det <- if (is.null(node_cells_detail)) nc else tibble::as_tibble(node_cells_detail)
  subs <- tibble::tibble(node_id = det$node_id, id = paste0(cell_id(det), "::", det$sub_sector)) %>% dplyr::distinct() %>%
    dplyr::group_by(node_id) %>% dplyr::summarise(osi_subcomponents = paste(id, collapse = "; "), .groups = "drop")
  out <- dplyr::left_join(cells, subs, by = "node_id")
  attr(out, "known_nodes") <- known
  out
}

# One node's three-stage cells as the bundle writes them (list(cells, subcomponents), I() arrays): NULL (the
# fields are left out) without legacy_cells or for a node outside its "known_nodes"; empty arrays for a known
# node with no basket code. Pure.
aib_dash_node_legacy <- function(legacy_cells, node_id) {
  if (is.null(legacy_cells)) return(NULL)
  known <- attr(legacy_cells, "known_nodes")
  if (!is.null(known) && !node_id %in% known) return(NULL)
  lg <- legacy_cells[legacy_cells$node_id == node_id, , drop = FALSE]
  if (!nrow(lg)) return(list(cells = aib_dash_arr(character()), subcomponents = aib_dash_arr(character())))
  list(cells = aib_dash_split(lg$yf_cells[1], "; "), subcomponents = aib_dash_split(lg$osi_subcomponents[1], "; "))
}

# Node records (every classified node, with its frontier) keyed for the view.
# The EIS index of each technology (sector) when the run used the electro-industrial dictionary
# (res$taxonomy: tech, index_code, index_name); empty for the OSI basket.
aib_dash_index_of <- function(res) {
  tx <- res$taxonomy
  if (is.null(tx) || !nrow(tx) || all(is.na(tx$index_code))) return(NULL)
  tx
}

# legacy_cells: aib_dash_legacy_cells() (node_id, yf_cells, osi_subcomponents), or NULL: the nodes then carry no
# yfCellsLegacy / yfSubcomponentsLegacy (an export before 2026-10-28, which the contract still accepts). A node
# outside its "known_nodes" attribute (when set) carries none either, so the dashboard uses the roll-up.
aib_dash_nodes <- function(res, legacy_cells = NULL) {
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
    lg <- aib_dash_node_legacy(legacy_cells, r$node_id)
    list(
      id = r$node_id,
      label = aib_dash_node_label(r$sub_sector, r$supply_chain),
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
      # OSI technology x six-stage cells ("batteries-components"; the dictionary's nodes reach the Sector
      # and Country views through the crosswalk of scripts/31; yf_cells lists them all).
      yfCells = aib_dash_arr(unique(stats::na.omit(c(t$yf_cell, if ("yf_cells" %in% names(t) && nrow(t)) unlist(strsplit(stats::na.omit(t$yf_cells), "; ")))))),
      yfSubcomponents = if (nrow(t)) aib_dash_split(t$yf_subcomponent_id[1], "; ") else aib_dash_arr(character()),
      # The same node's three-stage cells and sub-components ("batteries-midstream", 2026-10-28), for the joins
      # with the three-stage bundles; [] for a node with no basket code; absent (NULL, dropped) without legacy_cells
      # or for a node the inputs do not know.
      yfCellsLegacy = lg$cells,
      yfSubcomponentsLegacy = lg$subcomponents,
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
    stages = aib_dash_stage_order(aib_dash_split(cp$stages, "; ")),
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
    out <- c(list(
      partners = int(r, paste0(p, "_n_partners")), selected = int(r, paste0(p, "_n_selected")),
      headline = num(r, paste0(p, "_headline_contribution")), cumulative = num(r, paste0(p, "_cumulative_contribution"))),
      # Partners by stage, one count per six-stage stage (extraction ... recovery; aib_network_countries()).
      stats::setNames(lapply(aib_dash_stage_keys(), function(s) int(r, paste0(p, "_n_", s, "_partners"))), aib_dash_stage_keys()))
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
    stg <- stats::setNames(vapply(aib_dash_stage_keys(), function(s) isTRUE(r[[s]][1]), logical(1)), SUPPLY_CHAIN_STAGES)
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
# emergent network (`network`), the investor -> host flows (`investmentFlows`) and, given the strategic layer
# (`strategic`: aib_dash_strategic() of scripts/33's aib_strategic.rds; 2026-10-20), the `strategic` section;
# given legacy_cells (aib_dash_legacy_cells(); 2026-10-28), node.yfCellsLegacy and yfSubcomponentsLegacy.
aib_dash_bundle <- function(res, top_edges = 40, strategic = NULL, legacy_cells = NULL) {
  cfg <- res$meta$config
  nodes <- aib_dash_nodes(res, legacy_cells)
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
                                       if (is.list(n$diversify)) n$diversify$largest, if (is.list(n$screenedHost)) n$screenedHost$host))),
    aib_dash_strategic_codes(strategic)
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
  if (!is.null(strategic)) doc$strategic <- strategic
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

# ---- The strategic layer (2026-10-20) ------------------------------------------------------------------
# `strategic`: an optional section of the main document, built from scripts/33's aib_strategic.rds (R/aib/
# aib_strategic.R; docs/aib_methodology.md section 16): the Strategic Sectoral Compacts (persistent: the same
# ids, steps and nodes in every scenario) with their value-chain spine, per-scenario portfolio, missions,
# policy menu, coverage and partners, and the country x node division of labour; the compact candidates of
# the main document as their workstreams; the Strategic AIB Roles of countries (every gate value and
# percentile, the narrative slots); the compact-specific country roles; and the bilateral theses as tuples
# [a, b, [priority compact ids], [logic labels]] (the complementarity text is the two countries' compact
# roles). Absent when there is no strategic layer. Arrays are I()-wrapped; NA is JSON null. The shape is in
# docs/aib_methodology.md 16.9 and checked by aib_check_strategic() (aib_check.R).

AIB_DASH_STRATEGIC_VERSION <- 1L

aib_dash_strategic_scenario_ids <- function() c("baseline", "managed", "partial", "decoupling")
aib_dash_strategic_role_codes <- function() c("RPA", "STM", "SMI", "DMA", "CIA", "DSP", "CBH")
aib_dash_strategic_supply_roles <- function() c("RPA", "STM", "SMI")
aib_dash_strategic_role_names <- function() {
  c(RPA = "Resource & Processing Anchor", STM = "Specialized Technology & Manufacturing Anchor",
    SMI = "Scale Manufacturing & Integration Platform", DMA = "Demand & Market Anchor",
    CIA = "Capital & Investment Anchor", DSP = "Development & Scale-Up Platform", CBH = "Coordination / Bridge Hub")
}
aib_dash_strategic_role_nouns <- function() {
  c(RPA = "resource & processing", STM = "specialized technology", SMI = "scale manufacturing", DMA = "demand & market",
    CIA = "capital & investment", DSP = "development & scale-up", CBH = "coordination / bridge")
}
# The spine statuses: the five AIB classes plus the three scenario statuses that are not classes (NOT TESTED,
# 2026-10-21: a Managed Interdependence node left alone only because its benchmark is concentrated too).
aib_dash_strategic_statuses <- function() c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION", "LOW INTERVENTION", "INSUFFICIENT DATA", "NOT STRESSED", "NOT TESTED", "NO SHOCK")
aib_dash_strategic_functions <- function() c("resource", "processing", "components", "systems")

# Gate and supporting metrics exported per country (the evidence of every role).
aib_dash_strategic_metric_names <- function() {
  c("rp_positions", "rp_mean_share", "spec_value_share", "spec_compacts", "spec_positions", "mfg_value_share",
    "integration_compacts", "mfg_positions", "demand_mean_share", "net_import_mf", "net_import_rp", "net_import_mean",
    "net_import_end", "demand_positions", "cmfdi_out_usd_bn", "cmfdi_destinations", "fdi_out_usd_bn", "fdi_year", "host_gap_usd_bn",
    "scaleup_rate", "host_top_scaled", "momentum_nodes", "presence_nodes", "host_capable_gap_nodes",
    "ws_bridge_vertical", "sc_span", "ws_member", "ws_bridge", "sc_span_supply", "ws_specialized", "ws_resource",
    "ws_demand_anchor", "ws_critical")
}

aib_dash_signif <- function(x, d = 4) ifelse(is.na(x), NA_real_, signif(as.numeric(x), d))
aib_dash_split_codes <- function(x) {
  if (is.null(x) || !length(x) || is.na(x) || !nzchar(trimws(x))) return(aib_dash_arr(character()))
  aib_dash_arr(strsplit(trimws(x), "\\s+")[[1]])
}

# st: readRDS(aib_strategic.rds). gate_labels: named character (gate metric -> plain label), e.g.
# aib_strategic_gate_labels(); the metric name is used where it has none.
aib_dash_strategic <- function(st, gate_labels = NULL) {
  if (is.null(st) || is.null(st$compacts) || !nrow(st$compacts)) return(NULL)
  sids <- intersect(aib_dash_strategic_scenario_ids(), st$scenarios$scenario)
  defs <- st$compact_defs
  map <- st$map
  # a configured compact with no node in the run has no spine: left out
  defs <- defs[defs$strategic_compact_id %in% map$strategic_compact_id, , drop = FALSE]
  P <- st$params
  ws <- st$workstreams
  gates <- st$roles[!duplicated(st$roles$role_code), c("role_code", "gate1", "gate2")]
  num <- function(x, d = 4) aib_dash_round(x, d)
  coalition_scenario <- function(s) isTRUE(st$scenarios$coalitions[st$scenarios$scenario == s])
  # A scenario without coalitions (Baseline, partial disruption) has no partners: null, not [].
  codes_or_null <- function(x, s) if (!coalition_scenario(s)) NULL else aib_dash_split_codes(x)
  core_long <- st$core_long
  dl <- st$division
  scen <- lapply(sids, function(s) {
    r <- st$scenarios[st$scenarios$scenario == s, ]
    list(id = s, label = r$label, shockId = aib_dash_chr(r$shock_id), words = aib_dash_chr(r$words),
         investment = isTRUE(r$investment), coalitions = isTRUE(r$coalitions))
  })
  comps <- lapply(seq_len(nrow(defs)), function(k) {
    sc <- defs$strategic_compact_id[k]
    m <- map[map$strategic_compact_id == sc, , drop = FALSE]
    m <- m[order(m$value_chain_order, match(m$node_id, map$node_id)), , drop = FALSE]
    steps <- lapply(unique(m$value_chain_order), function(o) {
      x <- m[m$value_chain_order == o, , drop = FALSE]
      list(order = as.integer(o), label = x$value_chain_step[1], description = aib_dash_chr(x$step_description[1]),
           nodes = aib_dash_arr(x$node_id), functions = aib_dash_arr(x$node_function))
    })
    core <- if (is.null(core_long)) NULL else core_long[core_long$strategic_compact_id == sc, , drop = FALSE]
    core_obj <- function(d) if (is.null(d) || !nrow(d)) list() else lapply(seq_len(nrow(d)), function(i) list(iso3 = d$iso3[i], role = d$compact_role[i], step = aib_dash_chr(d$lead_step[i])))
    ws_k <- ws[ws$strategic_compact_id %in% sc, , drop = FALSE]
    also <- ws$workstream_id[!ws$strategic_compact_id %in% sc &
                               vapply(strsplit(dplyr::coalesce(ws$also_spans, ""), " ", fixed = TRUE), function(a) sc %in% a, logical(1))]
    by_s <- lapply(sids, function(s) {
      r <- st$compacts[st$compacts$strategic_compact_id == sc & st$compacts$scenario == s, , drop = FALSE]
      sp <- st$spine[st$spine$strategic_compact_id == sc & st$spine$scenario == s, , drop = FALSE]
      sp <- sp[match(m$node_id, sp$node_id), , drop = FALSE]
      ms <- Filter(Negate(is.null), lapply(c("primary", "secondary", "supporting"), function(slot) {
        txt <- r[[paste0(slot, "_mission")]]
        if (is.null(txt) || is.na(txt)) return(NULL)
        mat <- r[[paste0(slot, "_mission_material")]]
        # targeted: below the materiality floor (a targeted action, not a mission that leads the compact)
        list(slot = slot, cls = r[[paste0(slot, "_mission_class")]], text = txt, basis = aib_dash_chr(r[[paste0(slot, "_mission_basis")]]),
             targeted = !is.null(mat) && isFALSE(as.logical(mat)))
      }))
      menu <- st$policy_menus[[paste(sc, s)]]
      mix <- if (is.na(r$policy_mix) || identical(r$policy_mix, "NO SHOCK")) character() else strsplit(r$policy_mix, " + ", fixed = TRUE)[[1]]
      list(
        counts = list(build = as.integer(r$build_node_count), coordinate = as.integer(r$coordinate_node_count), manage = as.integer(r$manage_node_count),
                      low = as.integer(r$low_node_count), insufficient = as.integer(r$insufficient_node_count),
                      notStressed = as.integer(r$not_stressed_node_count), notTested = as.integer(dplyr::coalesce(r$not_tested_node_count, 0L)),
                      noShock = as.integer(r$no_shock_node_count)),
        needs = list(build = as.integer(r$needs_build_nodes), coordinate = as.integer(r$needs_coordinate_nodes), manage = as.integer(r$needs_manage_nodes)),
        missions = ms, statement = r$mission_statement, policyMix = aib_dash_arr(mix),
        menu = if (is.null(menu)) list() else lapply(unname(menu), function(x) list(cls = x$cls, instruments = aib_dash_arr(x$instruments), nodes = aib_dash_arr(unique(x$nodes)))),
        demandUsd = aib_dash_round(r$demand_usd, 0), lostUsd = aib_dash_round(r$lost_usd, 0), exposure = num(r$exposure),
        coverage = list(static = num(r$coverage_static), existing = num(r$coverage_existing),
                        coordinated = num(r$coverage_coordinated), postInvestment = num(r$coverage_post_investment)),
        activeWorkstreams = aib_dash_split_codes(r$active_workstreams),
        # allied economies only (2026-10-21): the non-allied ones are criticalExternal and nonAlliedReliedOn
        criticalPartners = codes_or_null(r$critical_supply_partners, s), criticalExternal = codes_or_null(r$critical_external_dependencies, s),
        alliedOutsideCore = codes_or_null(r$allied_outside_core, s), coalition = codes_or_null(r$coalition_members, s),
        nonAlliedReliedOn = codes_or_null(r$non_allied_relied_on, s),
        # hosts that beat every other host on a BUILD node (a tie names nobody: buildHostTies counts those nodes)
        buildHosts = aib_dash_split_codes(r$build_hosts), buildHostsNonAllied = aib_dash_split_codes(r$build_hosts_non_allied),
        buildHostTies = if (is.null(r$build_host_ties) || is.na(r$build_host_ties)) NA_integer_ else as.integer(r$build_host_ties),
        classMix = aib_dash_chr(r$class_mix), headline = r$headline_result, stressNote = aib_dash_chr(r$stress_note),
        # the spine: [node id, status, coordinated coverage or null, named host or null, hosts tied with the
        # model's rank 1 or null], in spine order (a host is named only when no other host ties with it)
        nodes = lapply(seq_len(nrow(sp)), function(i) list(m$node_id[i], sp$node_status[i], num(sp$coverage_coordinated[i]), aib_dash_chr(sp$top_host[i]),
                                                           if (is.null(sp$host_tie_n) || is.na(sp$host_tie_n[i])) NA_integer_ else as.integer(sp$host_tie_n[i])))
      )
    })
    names(by_s) <- sids
    d <- dl[dl$strategic_compact_id == sc, , drop = FALSE]
    # flags: a = anchor position, s = specialist position, h = capable top host, m = momentum
    flags <- paste0(ifelse(d$anchor_position %in% TRUE, "a", ""), ifelse(d$specialist_position %in% TRUE, "s", ""),
                    ifelse(!is.na(d$best_host_rank) & d$host_scaled %in% TRUE, "h", ""), ifelse(d$momentum %in% TRUE, "m", ""))
    list(
      id = sc, name = defs$strategic_compact_name[k], family = aib_dash_chr(defs$family[k]), purpose = aib_dash_chr(defs$purpose[k]),
      steps = steps, workstreams = aib_dash_arr(ws_k$workstream_id), alsoTouching = aib_dash_arr(also),
      core = core_obj(core[core$allied, , drop = FALSE]), nonAllied = core_obj(core[!core$allied, , drop = FALSE]),
      scenarios = by_s,
      # [ISO3, node id, share of world supply, share of allied demand, best host rank or null, flags]
      division = lapply(seq_len(nrow(d)), function(i) list(d$iso3[i], d$node_id[i], num(d$position_share[i]), num(d$scope_demand_share[i]),
                                                          if (is.na(d$best_host_rank[i])) NA_integer_ else as.integer(d$best_host_rank[i]), flags[i]))
    )
  })
  wso <- lapply(seq_len(nrow(ws)), function(i) {
    w <- ws[i, ]
    split <- if (is.na(w$demand_split) || !nzchar(w$demand_split)) list() else lapply(strsplit(w$demand_split, "; ", fixed = TRUE)[[1]], function(x) {
      p <- strsplit(x, " ", fixed = TRUE)[[1]]
      list(p[1], num(as.numeric(sub("%", "", p[2], fixed = TRUE)) / 100))
    })
    list(id = w$workstream_id, compact = aib_dash_chr(w$strategic_compact_id),
         basis = if (grepl("^override", w$assignment_basis)) "override" else if (is.na(w$strategic_compact_id)) "none" else "demand",
         share = num(w$primary_demand_share), split = split, alsoSpans = aib_dash_split_codes(w$also_spans),
         straddles = isTRUE(w$straddles), overrideReason = aib_dash_chr(w$override_reason),
         active = aib_dash_arr(sids[vapply(sids, function(s) isTRUE(w[[paste0("active_", s)]]), logical(1))]))
  })
  mt <- st$metrics
  narr <- st$narratives
  slot_names <- c(headline = "slot_headline", secondary = "slot_secondary", firstOn = "slot_first_on", contributes = "slot_contributes",
                  focus = "slot_focus", needs = "slot_needs", dependency = "slot_dependency", compacts = "slot_compacts",
                  partners = "slot_partners", capacity = "slot_capacity")
  mnames <- intersect(aib_dash_strategic_metric_names(), names(mt))
  shown <- c("primary", "secondary", "qualifies beyond the cap")
  # a list slot's items as [code, number] pairs (codes and numbers space-separated in the narrative table)
  item_pairs <- function(ids, vals, d = 4) {
    id <- if (is.null(ids) || is.na(ids) || !nzchar(trimws(ids))) character() else strsplit(trimws(ids), "\\s+")[[1]]
    v <- if (is.null(vals) || is.na(vals) || !nzchar(trimws(vals))) numeric() else suppressWarnings(as.numeric(strsplit(trimws(vals), "\\s+")[[1]]))
    lapply(seq_along(id), function(j) list(id[j], if (j <= length(v)) num(v[j], d) else NA_real_))
  }
  items_of <- function(n) {
    if (!nrow(n) || !"items_contributes" %in% names(n)) return(NULL)
    list(firstOn = aib_dash_split_codes(n$items_first_on[1]), contributes = item_pairs(n$items_contributes[1], n$items_contributes_share[1]),
         focus = aib_dash_split_codes(n$items_focus[1]), needs = item_pairs(n$items_needs[1], n$items_needs_share[1]),
         compacts = aib_dash_split_codes(n$items_compacts[1]), partners = aib_dash_split_codes(n$items_partners[1]),
         capacity = item_pairs(n$items_capacity[1], n$items_capacity_usd_bn[1], 3), capacityUsdBn = num(n$capacity_usd_bn[1], 3))
  }
  countries <- lapply(seq_len(nrow(mt)), function(i) {
    iso <- mt$iso3[i]
    r <- st$roles[st$roles$iso3 == iso, , drop = FALSE]
    r <- r[order(is.na(r$role_rank), r$role_rank, match(r$role_code, aib_dash_strategic_role_codes())), , drop = FALSE]
    n <- narr[narr$iso3 == iso, , drop = FALSE]
    pos <- function(k) aib_dash_arr(r$role_code[r$primary_or_secondary %in% k])
    list(
      iso3 = iso, allied = isTRUE(mt$allied[i]), majorSupplier = isTRUE(mt$major_supplier[i]),
      primary = pos("primary"), secondary = pos("secondary"), beyondCap = pos("qualifies beyond the cap"),
      # [code, status or null, rank or null, role strength, gate 1 value, gate 1 percentile, gate 2 value, gate 2 percentile]
      roles = lapply(seq_len(nrow(r)), function(j) list(
        r$role_code[j], if (r$primary_or_secondary[j] %in% shown) r$primary_or_secondary[j] else NA_character_,
        if (is.na(r$role_rank[j])) NA_integer_ else as.integer(r$role_rank[j]),
        num(r$role_strength[j], 3), aib_dash_signif(r$gate1_value[j]), num(r$gate1_pct[j], 3),
        aib_dash_signif(r$gate2_value[j]), num(r$gate2_pct[j], 3))),
      metrics = stats::setNames(lapply(mnames, function(k) aib_dash_signif(mt[[k]][i])), mnames),
      slots = if (!nrow(n)) NULL else stats::setNames(lapply(slot_names, function(k) aib_dash_chr(n[[k]][1])), names(slot_names)),
      # the list slots' items as codes (2026-10-21): readers join them to the bundle's nodes, compacts and
      # countries instead of splitting the prose back apart
      items = items_of(n),
      narrative = if (!nrow(n)) NA_character_ else n$narrative[1]
    )
  })
  cr <- st$compact_roles
  croles <- lapply(seq_len(nrow(cr)), function(i) {
    x <- cr[i, ]
    list(compact = x$strategic_compact_id, iso3 = x$iso3, allied = isTRUE(x$allied), role = x$compact_role,
         detail = aib_dash_chr(x$compact_role_detail), functions = aib_dash_split(x$compact_functions, "; "),
         leadNode = aib_dash_chr(x$lead_node), leadStep = aib_dash_chr(x$lead_step), leadShare = num(x$lead_share),
         leadBasis = aib_dash_chr(x$lead_share_basis), keyNodes = aib_dash_split(x$key_nodes, "; "),
         supplier = num(x$supplier_contribution), demand = num(x$demand_contribution), investment = num(x$investment_contribution),
         resilienceManaged = num(x$resilience_contribution_managed), resilienceDecoupling = num(x$resilience_contribution_decoupling),
         criticality = aib_dash_chr(x$criticality), relevance = num(x$compact_relevance))
  })
  th <- st$theses
  theses <- if (is.null(th) || !nrow(th)) list() else lapply(seq_len(nrow(th)), function(i) list(
    th$a[i], th$b[i], aib_dash_split_codes(th$priority_compacts[i]), aib_dash_split(th$logic[i], "; ")))
  names_r <- aib_dash_strategic_role_names()
  nouns <- aib_dash_strategic_role_nouns()
  list(
    version = AIB_DASH_STRATEGIC_VERSION,
    defaultScenario = if (is.null(st$meta$default_scenario)) "managed" else st$meta$default_scenario,
    framing = "Strategic compact = persistent architecture; scenario = the stress applied to it.",
    scenarios = scen,
    roleDefs = lapply(aib_dash_strategic_role_codes(), function(code) {
      g <- gates[gates$role_code == code, , drop = FALSE]
      gg <- if (nrow(g)) c(g$gate1[1], g$gate2[1]) else character()
      lab <- if (is.null(gate_labels)) gg else ifelse(is.na(gate_labels[gg]), gg, gate_labels[gg])
      list(code = code, name = names_r[[code]], noun = nouns[[code]], gates = aib_dash_arr(gg), gateLabels = aib_dash_arr(unname(lab)),
           supplyRole = code %in% aib_dash_strategic_supply_roles())
    }),
    thresholds = list(pPrimary = P$p_primary, pSecondary = P$p_secondary, maxPrimary = as.integer(P$max_primary), maxSecondary = as.integer(P$max_secondary)),
    compacts = comps, workstreams = wso, countries = countries, compactRoles = croles, theses = theses
  )
}

# Every ISO3 code the strategic section names (for the document's country names).
aib_dash_strategic_codes <- function(s) {
  if (is.null(s)) return(character())
  c(unlist(lapply(s$countries, `[[`, "iso3")), unlist(lapply(s$compactRoles, `[[`, "iso3")),
    unlist(lapply(s$theses, function(t) c(t[[1]], t[[2]]))),
    unlist(lapply(s$compacts, function(k) c(unlist(lapply(k$core, `[[`, "iso3")), unlist(lapply(k$nonAllied, `[[`, "iso3")),
                                             unlist(lapply(k$division, `[[`, 1)),
                                             unlist(lapply(k$scenarios, function(x) c(unlist(x$criticalPartners), unlist(x$criticalExternal),
                                                                                       unlist(x$alliedOutsideCore), unlist(x$nonAlliedReliedOn),
                                                                                       unlist(x$buildHostsNonAllied),
                                                                                       unlist(x$coalition), unlist(x$buildHosts),
                                                                                       unlist(lapply(x$nodes, `[[`, 4)))))))))
}
