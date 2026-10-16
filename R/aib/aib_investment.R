# Allied Industrial Base (AIB): Investment Partnership Prioritization, a marginal capacity
# simulation (docs/aib_methodology.md, 7). It runs only after the optimized trade reallocation and
# only on nodes whose post-trade residual gap is material. For each candidate host it adds a
# standardized synthetic increment of capacity (a share of the node's universe demand), reruns the
# trade optimizer and measures the change in coverage. The increment is a modelling device, not an
# investment project: it says where new capability would restore supply, not what a plant costs.
#
# Hosts are ordered lexicographically: (1) resilience contribution, (2) diversification
# contribution, (3) host specialization/capability, (4) Friendshore, (5) revealed policy/climate
# compatibility. Only economies with revealed capability are hosts at all (aib_eligible_hosts()),
# so a high Friendshore score cannot make an industrially irrelevant economy look optimal.

# Economies with revealed capability in a node, at material scale. RCA counts only where the
# economy's exports of the node are material: at least min_exports_usd or min_material_share of the
# node's world trade, whichever bar is lower (the repo's RCA facts use a USD 50m floor for the same
# reason: an economy with tiny total exports gets a huge RCA from a few shipments). Bases:
#   tier 3  RCA >= min_rca with material exports, or a production share >= min_production_share
#   tier 2  world export share >= min_share
#   tier 1  RCA >= min_rca with material exports in a node linked in the supply chain
# Within a tier hosts are ordered by capability at scale (capability_value: world export share or
# production share in the node).
# Shocked economies and those outside the universe are not hosts. Friendshore plays no part here.
aib_eligible_hosts <- function(node_id, exporter_metrics, edges, exclude = character(), universe = NULL,
                               min_rca = 1, min_share = 0.005, min_production_share = 0.01, rca_cap = 4,
                               min_exports_usd = 5e7, min_material_share = 0.01) {
  world <- tapply(exporter_metrics$exports, exporter_metrics$node_id, sum)
  material <- exporter_metrics$exports >= pmin(min_exports_usd, min_material_share * world[exporter_metrics$node_id])
  em <- exporter_metrics
  em$rca_material <- ifelse(material, em$export_rca, NA_real_)
  ex <- em[em$node_id == node_id, , drop = FALSE]
  linked_ids <- unique(c(edges$node_b[edges$node_a == node_id], edges$node_a[edges$node_b == node_id]))
  lk <- em[em$node_id %in% linked_ids & dplyr::coalesce(em$rca_material, 0) >= min_rca, , drop = FALSE]
  lk <- if (!nrow(lk)) tibble::tibble(exporter_iso = character(), linked_rca = numeric(), linked_node = character()) else lk %>%
    dplyr::group_by(exporter_iso) %>%
    dplyr::summarise(linked_rca = max(rca_material), linked_node = node_id[which.max(rca_material)], .groups = "drop")
  prod <- if ("production_share" %in% names(ex)) ex$production_share else rep(NA_real_, nrow(ex))
  cand <- tibble::tibble(
    host_iso = ex$exporter_iso,
    node_exports_usd = ex$exports,
    node_rca = ex$export_rca,
    node_rca_material = ex$rca_material,
    node_world_share = ex$world_share,
    production_share = prod
  )
  cand <- dplyr::full_join(cand, dplyr::rename(lk, host_iso = exporter_iso), by = "host_iso")
  ok_rca <- dplyr::coalesce(cand$node_rca_material, 0) >= min_rca
  ok_prod <- dplyr::coalesce(cand$production_share, 0) >= min_production_share
  ok_share <- dplyr::coalesce(cand$node_world_share, 0) >= min_share
  ok_link <- !is.na(cand$linked_rca)
  cand$capability_tier <- dplyr::case_when(ok_rca | ok_prod ~ 3L, ok_share ~ 2L, ok_link ~ 1L, TRUE ~ 0L)
  cand$capability_basis <- dplyr::case_when(
    ok_rca & ok_prod ~ "RCA and production share in the node",
    ok_rca ~ "RCA in the node",
    ok_prod ~ "production share in the node",
    ok_share ~ "world export share in the node",
    ok_link ~ paste0("RCA in linked node ", cand$linked_node),
    TRUE ~ NA_character_
  )
  # Capability at scale ranks hosts within a tier: the larger of the economy's world export share and
  # production share in the node (RCA is the gate, not the measure: it saturates for small
  # specialists); a linked-node host by its world export share in the linked node.
  linked_share <- em$world_share[match(paste(cand$linked_node, cand$host_iso), paste(em$node_id, em$exporter_iso))]
  cand$capability_value <- dplyr::case_when(
    ok_rca | ok_prod | ok_share ~ pmax(dplyr::coalesce(cand$node_world_share, 0), dplyr::coalesce(cand$production_share, 0)),
    ok_link ~ dplyr::coalesce(linked_share, 0),
    TRUE ~ 0
  )
  cand <- cand[cand$capability_tier > 0 & !cand$host_iso %in% exclude, , drop = FALSE]
  if (!is.null(universe)) cand <- cand[cand$host_iso %in% universe, , drop = FALSE]
  cand
}

# The simulation for one node: hosts x increments, each a rerun of the optimized state (C) with
# the host's synthetic capacity, at the headline supplier-share limit and at the diversified one.
# Coverage and concentration come from the closed-form optimized state (aib_optimized_analytic());
# the importer allocation of the new capacity (who it serves, the investment partnerships) is worked
# out for the `pair_hosts` best-ranked hosts at the headline increment, on top of the optimized
# allocation. Returns list(hosts = one row per host x increment, pairs = investor -> host rows).
aib_investment_node <- function(prob, scenario, hosts, increments, headline_increment, cap, diversified_cap,
                                scores, fs_lookup, tolerances, neutral = 0.5, pair_hosts = 5,
                                tie_resilience = 0.005, tie_hhi = 0.005, base_flows = NULL) {
  if (is.null(prob) || !nrow(hosts)) return(list(hosts = NULL, pairs = NULL))
  solve <- function(cp, extra = NULL) {
    if (isTRUE(prob$allow_domestic)) aib_optimized_analytic(prob, scenario, cap = cp, scores = scores, extra = extra) else
      aib_solve_state(prob, scenario, "C", cap = cp, scores = scores, tolerances = tolerances, neutral = neutral, extra = extra, steps = 3)
  }
  before <- solve(cap)
  before_div <- solve(diversified_cap)
  need <- pmax(prob$lost, 0)
  w_need <- if (sum(need) > 0) need / sum(need) else rep(1 / length(need), length(need))
  rows <- list()
  for (k in seq_len(nrow(hosts))) {
    h <- hosts$host_iso[k]
    fs_h <- fs_lookup(prob$scope, rep(h, length(prob$scope)))
    fs_w <- sum(w_need * ifelse(is.na(fs_h), neutral, fs_h))
    cp_w <- sum(w_need * scores$compat(prob$scope, rep(h, length(prob$scope))))
    for (inc in increments) {
      extra <- stats::setNames(inc, h)
      after <- solve(cap, extra)
      after_div <- solve(diversified_cap, extra)
      used <- unname(after$totals[h]) - dplyr::coalesce(unname(before$totals[h]), 0)
      rows[[length(rows) + 1]] <- tibble::tibble(
        host_iso = h, increment = inc,
        coverage_before = before$coverage, coverage_after = after$coverage,
        marginal_resilience_gain = after$coverage - before$coverage,
        residual_gap_before = 1 - before$coverage, residual_gap_after = 1 - after$coverage,
        additional_demand_served_usd = (after$coverage - before$coverage) * prob$D,
        hhi_before = before$hhi, hhi_after = after$hhi, delta_hhi = after$hhi - before$hhi,
        largest_supplier_before = before$largest, largest_share_before = before$largest_share,
        largest_supplier_after = after$largest, largest_share_after = after$largest_share,
        delta_largest_share = after$largest_share - before$largest_share,
        coverage_before_diversified = before_div$coverage, coverage_after_diversified = after_div$coverage,
        marginal_resilience_gain_diversified = after_div$coverage - before_div$coverage,
        new_capacity_used = if (length(used) != 1 || is.na(used)) 0 else used,
        friendshore_need_weighted = fs_w,
        compatibility_need_weighted = cp_w
      )
    }
  }
  host_rows <- dplyr::bind_rows(rows) %>% dplyr::left_join(hosts, by = "host_iso")
  host_rows$node_key <- "n"
  host_rows <- aib_rank_hosts(host_rows, tie_resilience, tie_hhi, keys = c("node_key", "increment"))
  host_rows$node_key <- NULL

  # Investment partnerships: which importers the best hosts' new capacity serves. Every other flow is
  # fixed at the optimized allocation (base_flows: the headline LP's state-C flows, recomputed when
  # not given), so the lexicographic optimum for the increment is greedy: the importers with unmet
  # demand in descending Export Opportunity of the host to them (objective 3), then compatibility
  # (objective 4); a host keeping supply at home counts as neutral opportunity, full compatibility.
  pairs <- list()
  top <- host_rows[abs(host_rows$increment - headline_increment) < 1e-9 & host_rows$host_rank <= pair_hosts, , drop = FALSE]
  if (nrow(top)) {
    if (is.null(base_flows)) {
      base_flows <- aib_solve_state(prob, scenario, "C", cap = cap, scores = scores, tolerances = tolerances, neutral = neutral)$flows
    }
    served_m <- if (is.null(base_flows) || !nrow(base_flows)) numeric() else tapply(base_flows$f, base_flows$m, sum)
    cap_m <- pmax(as.numeric(prob$demand - prob$fixed_m), 0)
    names(cap_m) <- prob$scope
    got <- unname(served_m[prob$scope])
    got[is.na(got)] <- 0
    unmet <- pmax(cap_m - got, 0)
    for (k in seq_len(nrow(top))) {
      h <- top$host_iso[k]
      u <- top$new_capacity_used[k]
      if (!(u > 1e-9)) next
      ms <- prob$scope[unmet > 1e-12]
      if (!length(ms)) next
      op <- ifelse(ms == h, neutral, scores$op(rep(h, length(ms)), ms))
      cp <- ifelse(ms == h, 1, scores$compat(rep(h, length(ms)), ms))
      ord <- order(-op, -cp, ms)
      alloc <- numeric(length(ms))
      left <- u
      for (j in ord) {
        a <- min(unmet[match(ms[j], prob$scope)], left)
        alloc[j] <- a
        left <- left - a
        if (left <= 1e-12) break
      }
      keep <- alloc > 1e-9
      if (!any(keep)) next
      m <- ms[keep]
      pairs[[length(pairs) + 1]] <- tibble::tibble(
        investor_iso = m, host_iso = h, increment = headline_increment,
        supply_from_new_capacity = alloc[keep],
        supply_from_new_capacity_usd = alloc[keep] * prob$D,
        investor_lost_share = unname(prob$lost[m]),
        export_opportunity = scores$op_raw(rep(h, length(m)), m),
        friendshore = fs_lookup(m, rep(h, length(m))),
        relationship = ifelse(m == h, "domestic", "investment partnership")
      )
    }
  }
  list(hosts = host_rows, pairs = dplyr::bind_rows(pairs))
}

# Lexicographic host order per node (and increment): (1) resilience contribution, (2) the
# diversification contribution (fall in HHI), (3) capability tier and value, (4) Friendshore,
# (5) compatibility. The resilience contribution is the gain with the diversified supplier limit
# in force (`marginal_resilience_gain_diversified`): without a limit every host's increment adds
# the same coverage (min(increment, gap)), so only the limited gain tells hosts apart (capacity at
# an already dominant supplier adds little). Ties are measured from the group's leader: a gain
# within `tie_resilience` of the best, an HHI change within `tie_hhi` of the lowest.
aib_rank_hosts <- function(host_rows, tie_resilience = 0.005, tie_hhi = 0.005, keys = c("node_id", "shock_id", "scenario", "increment"),
                           gain_col = "marginal_resilience_gain_diversified") {
  if (is.null(host_rows) || !nrow(host_rows)) return(host_rows)
  if (!gain_col %in% names(host_rows)) gain_col <- "marginal_resilience_gain"
  host_rows$rank_gain <- dplyr::coalesce(host_rows[[gain_col]], host_rows$marginal_resilience_gain)
  host_rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::mutate(
      gain_tier = floor((max(rank_gain) - rank_gain) / tie_resilience + 1e-9),
      hhi_tier = floor((dplyr::coalesce(delta_hhi, 0) - min(dplyr::coalesce(delta_hhi, 0))) / tie_hhi + 1e-9)
    ) %>%
    dplyr::arrange(
      gain_tier,
      hhi_tier,
      dplyr::desc(capability_tier),
      dplyr::desc(capability_value),
      dplyr::desc(friendshore_need_weighted),
      dplyr::desc(compatibility_need_weighted),
      .by_group = TRUE
    ) %>%
    dplyr::mutate(host_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gain_tier, -hhi_tier, -rank_gain)
}
