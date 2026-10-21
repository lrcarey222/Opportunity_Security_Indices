# Allied Industrial Base (AIB): Investment Partnership Prioritization, a marginal capacity
# simulation (docs/aib_methodology.md, 7). It runs only after the optimized trade reallocation and
# only on nodes whose post-trade residual gap is material. For each candidate host it adds a
# synthetic increment of capacity (a share of the node's universe demand, capped at a multiple of the
# host's current scale in the node: aib_host_increment()), reruns the trade optimizer and measures the
# change in coverage. The increment is a modelling device, not an investment project: it says where
# new capability would restore supply, not what a plant costs.
#
# Hosts are ordered lexicographically: (1) resilience contribution, (2) diversification
# contribution, (3) host specialization/capability, (4) Friendshore, (5) revealed policy/climate
# compatibility. Only economies with revealed capability are hosts at all (aib_eligible_hosts()),
# so a high Friendshore score cannot make an industrially irrelevant economy look optimal. With one
# standardized increment at every host the resilience contribution ties everywhere and the fall in
# HHI, which favours the smallest supplier, decides; the host-scale cap makes the contribution
# differ where a host cannot credibly carry the increment.

# Economies with revealed capability in a node, at material scale. RCA counts only where the
# economy's exports of the node are material: at least min_exports_usd or min_material_share of the
# node's world trade, whichever bar is lower (the repo's RCA facts use a USD 50m floor for the same
# reason: an economy with tiny total exports gets a huge RCA from a few shipments). Bases:
#   tier 3  RCA >= min_rca with material exports, or a production share >= min_production_share
#   tier 2  world export share >= min_share
#   tier 1  RCA >= min_rca with material exports in a node linked in the supply chain
# Within a tier hosts are ordered by capability at scale (capability_value: world export share or
# production share in the node). host_scale_usd is the host's current scale in the node: the larger of
# its world exports and its production share x the node's world exports (0 for a host qualified only
# by a linked node); aib_host_increment() caps its synthetic increment with it.
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
  node_world <- if (node_id %in% names(world)) as.numeric(world[[node_id]]) else 0
  cand$host_scale_usd <- pmax(dplyr::coalesce(cand$node_exports_usd, 0), dplyr::coalesce(cand$production_share, 0) * node_world)
  cand <- cand[cand$capability_tier > 0 & !cand$host_iso %in% exclude, , drop = FALSE]
  if (!is.null(universe)) cand <- cand[cand$host_iso %in% universe, , drop = FALSE]
  cand
}

# The synthetic increment simulated at a host: the nominal increment (a share of the node's
# demand-scope demand), capped at `multiple` x the host's current scale in the node (scale_share:
# host_scale_usd / that demand), so a host can at most grow its output by that multiple (1: double
# it). A host with no current scale in the node (one qualified by a linked node) gets none. An
# infinite or missing multiple leaves the nominal increment (one standardized increment at every host).
aib_host_increment <- function(increment, scale_share, multiple = Inf) {
  if (is.null(multiple) || !length(multiple) || is.na(multiple[1]) || !is.finite(multiple[1])) {
    return(rep_len(as.numeric(increment), max(length(increment), length(scale_share))))
  }
  pmin(as.numeric(increment), multiple[1] * pmax(dplyr::coalesce(as.numeric(scale_share), 0), 0))
}

# Scaling hosts: established producers, the hosts that build volume. A host is one when it can carry the
# full headline increment (aib_host_increment() leaves it whole) or holds at least `min_share` of world
# exports or production in the node (capability tier 2 or 3; a host qualified only by a linked node has
# no scale here). hosts: capability_tier, capability_value; scale_share: host scale / demand.
aib_scaling_hosts <- function(hosts, scale_share, headline_increment, multiple = Inf, min_share = 0.05) {
  full <- aib_host_increment(headline_increment, scale_share, multiple) >= headline_increment - 1e-12
  big <- dplyr::coalesce(hosts$capability_tier, 0L) >= 2L & dplyr::coalesce(hosts$capability_value, 0) >= min_share
  full | big
}

# Specialty and development hosts beside the scaling ranking (docs/aib_methodology.md, 7), after the
# simulation. rows: host rows (host_iso, node_rca, node_exports_usd, capability_tier, capability_value,
# delta_hhi; is_scaling when the run flagged scaling hosts) of the groups `keys`; income: named
# character, ISO3 -> World Bank income group; dev_index: per row, the host's Development Potential Index
# for the node (aib_host_dev_index()).
#   specialty    an economy of `specialty_income` with RCA >= specialty_min_rca on at least
#                specialty_min_exports (USD) of the node's exports; ranked by RCA, then the fall in HHI
#   development  an economy of `development_income` (every host is eligible, so it shows revealed
#                capability in the node or a linked node); ranked by the Development Potential Index
#                (unscored last), then capability tier and value
# The types exclude each other in that order: a scaling host is neither (a middle-income economy at
# scale is a scaling host, its income_group says the rest), so the specialty and development hosts are
# economies below scale. Adds income_group, development_index, is_specialty, specialty_rank,
# is_development, development_rank and host_type ("scaling", "specialty", "development" or "" for an
# eligible host of none). The ranks are NA outside the type.
aib_host_types <- function(rows, income, dev_index, keys = c("node_id", "shock_id", "scenario", "increment"),
                           specialty_min_rca = 2, specialty_min_exports = 1e8, specialty_income = "High income",
                           development_income = c("Lower middle income", "Upper middle income")) {
  if (is.null(rows) || !nrow(rows)) return(rows)
  inc <- if (is.null(income) || !length(income)) rep(NA_character_, nrow(rows)) else unname(income[rows$host_iso])
  rows$income_group <- inc
  rows$development_index <- if (is.null(dev_index)) NA_real_ else as.numeric(dev_index)
  scaling <- if ("is_scaling" %in% names(rows)) dplyr::coalesce(rows$is_scaling, FALSE) else rep(FALSE, nrow(rows))
  rows$is_specialty <- !scaling & !is.na(inc) & inc %in% specialty_income & dplyr::coalesce(rows$node_exports_usd, 0) >= specialty_min_exports &
    dplyr::coalesce(rows$node_rca, 0) >= specialty_min_rca
  rows$is_development <- !scaling & !is.na(inc) & inc %in% development_income
  rows$host_type <- dplyr::case_when(scaling ~ "scaling", rows$is_specialty ~ "specialty", rows$is_development ~ "development", TRUE ~ "")
  rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::mutate(
      specialty_rank = aib_type_rank(is_specialty, -dplyr::coalesce(node_rca, 0), dplyr::coalesce(delta_hhi, 0), host_iso),
      development_rank = aib_type_rank(is_development, -dplyr::coalesce(development_index, -1), -dplyr::coalesce(capability_tier, 0L),
                                       -dplyr::coalesce(capability_value, 0), host_iso)
    ) %>%
    dplyr::ungroup()
}

# The rank within a type: 1.. over the rows where `flag` holds, in the order of the keys `...`; NA elsewhere.
aib_type_rank <- function(flag, ...) {
  o <- order(...)
  o <- o[flag[o] %in% TRUE]
  r <- rep(NA_integer_, length(flag))
  r[o] <- seq_along(o)
  r
}

# A host's Development Potential Index for a node: the mean over the node's OSI technology x stage cells
# (`techs`, `stage`), else over every cell the economy is scored in; NA where it is not scored (the index
# covers non-high-income partners). dev: partner_iso, tech, supply_chain, value.
aib_host_dev_index <- function(hosts, techs, stage, dev) {
  if (is.null(dev) || !nrow(dev)) return(rep(NA_real_, length(hosts)))
  vapply(hosts, function(h) {
    d <- dev$value[dev$partner_iso == h & dev$tech %in% techs & dev$supply_chain %in% stage]
    if (!length(d)) d <- dev$value[dev$partner_iso == h]
    if (!length(d) || all(is.na(d))) NA_real_ else mean(d, na.rm = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

# The simulation for one node: hosts x increments, each a rerun of the optimized state (C) with
# the host's synthetic capacity, at the headline supplier-share limit and at the diversified one.
# Each host's increment is the nominal one capped at `scale_multiple` x its current scale
# (aib_host_increment(); `increment` keeps the nominal value, `host_increment` the one simulated).
# Coverage and concentration come from the closed-form optimized state (aib_optimized_analytic());
# the importer allocation of the new capacity (who it serves, the investment partnerships) is worked
# out for the `pair_hosts` best-ranked hosts at the headline increment, on top of the optimized
# allocation. Returns list(hosts = one row per host x increment, pairs = investor -> host rows).
# With `scaling_min_share` each host is flagged a scaling host (`is_scaling`, aib_scaling_hosts()) and
# the scaling hosts are ranked first (aib_rank_hosts()), so the investment partnerships of the best
# hosts are those of the best scaling hosts.
aib_investment_node <- function(prob, scenario, hosts, increments, headline_increment, cap, diversified_cap,
                                scores, fs_lookup, tolerances, neutral = 0.5, pair_hosts = 5,
                                tie_resilience = 0.005, tie_hhi = 0.005, base_flows = NULL, scale_multiple = Inf,
                                scaling_min_share = NULL) {
  if (is.null(prob) || !nrow(hosts)) return(list(hosts = NULL, pairs = NULL))
  solve <- function(cp, extra = NULL) {
    if (isTRUE(prob$allow_domestic)) aib_optimized_analytic(prob, scenario, cap = cp, scores = scores, extra = extra) else
      aib_solve_state(prob, scenario, "C", cap = cp, scores = scores, tolerances = tolerances, neutral = neutral, extra = extra, steps = 3)
  }
  before <- solve(cap)
  before_div <- solve(diversified_cap)
  need <- pmax(prob$lost, 0)
  w_need <- if (sum(need) > 0) need / sum(need) else rep(1 / length(need), length(need))
  scale_share <- if ("host_scale_usd" %in% names(hosts) && isTRUE(prob$D > 0)) hosts$host_scale_usd / prob$D else rep(Inf, nrow(hosts))
  if (!is.null(scaling_min_share)) {
    hosts$is_scaling <- aib_scaling_hosts(hosts, scale_share, headline_increment, scale_multiple, scaling_min_share)
  }
  rows <- list()
  for (k in seq_len(nrow(hosts))) {
    h <- hosts$host_iso[k]
    fs_h <- fs_lookup(prob$scope, rep(h, length(prob$scope)))
    fs_w <- sum(w_need * ifelse(is.na(fs_h), neutral, fs_h))
    cp_w <- sum(w_need * scores$compat(prob$scope, rep(h, length(prob$scope))))
    for (inc in increments) {
      inc_h <- aib_host_increment(inc, scale_share[k], scale_multiple)
      # No capacity to add: the states before stand (no rerun).
      none <- !(inc_h > 1e-12)
      extra <- if (none) NULL else stats::setNames(inc_h, h)
      after <- if (none) before else solve(cap, extra)
      after_div <- if (none) before_div else solve(diversified_cap, extra)
      used <- unname(after$totals[h]) - dplyr::coalesce(unname(before$totals[h]), 0)
      rows[[length(rows) + 1]] <- tibble::tibble(
        host_iso = h, increment = inc, host_increment = inc_h, host_scale_share = scale_share[k],
        increment_capped = inc_h < inc - 1e-12,
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
# Two columns say how the order was reached: `gain_shortlist`, how many of the group's hosts tie
# with the best on the resilience contribution, and `rank_basis`, the first criterion on which a
# host differs from the next one up (rank 1: from rank 2), "tied" when none does.
# Rows flagged `is_scaling` (aib_scaling_hosts()) put the scaling hosts first, each type ranked on its
# own (ties measured from its own leader); `scaling_rank` is then the rank among scaling hosts.
aib_rank_hosts <- function(host_rows, tie_resilience = 0.005, tie_hhi = 0.005, keys = c("node_id", "shock_id", "scenario", "increment"),
                           gain_col = "marginal_resilience_gain_diversified") {
  if (is.null(host_rows) || !nrow(host_rows)) return(host_rows)
  if (!gain_col %in% names(host_rows)) gain_col <- "marginal_resilience_gain"
  host_rows$rank_gain <- dplyr::coalesce(host_rows[[gain_col]], host_rows$marginal_resilience_gain)
  typed <- "is_scaling" %in% names(host_rows)
  host_rows$scaling_key <- if (typed) dplyr::coalesce(host_rows$is_scaling, FALSE) else TRUE
  out <- host_rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "scaling_key")))) %>%
    dplyr::mutate(
      gain_tier = floor((max(rank_gain) - rank_gain) / tie_resilience + 1e-9),
      hhi_tier = floor((dplyr::coalesce(delta_hhi, 0) - min(dplyr::coalesce(delta_hhi, 0))) / tie_hhi + 1e-9),
      gain_shortlist = sum(gain_tier == 0)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::arrange(
      dplyr::desc(scaling_key),
      gain_tier,
      hhi_tier,
      dplyr::desc(capability_tier),
      dplyr::desc(capability_value),
      dplyr::desc(friendshore_need_weighted),
      dplyr::desc(compatibility_need_weighted),
      .by_group = TRUE
    ) %>%
    dplyr::mutate(
      host_rank = dplyr::row_number(),
      rank_basis = aib_rank_basis(gain_tier, hhi_tier, capability_tier, capability_value, friendshore_need_weighted,
                                  compatibility_need_weighted, scaling = if (typed) scaling_key else NULL)
    ) %>%
    dplyr::ungroup()
  if (typed) out$scaling_rank <- ifelse(out$scaling_key, out$host_rank, NA_integer_)
  dplyr::select(out, -gain_tier, -hhi_tier, -rank_gain, -scaling_key)
}

# The criterion that separates each host from the next one up in an ordered group (the first host
# from the second): the first of the ranking keys on which the two differ, "tied" when none does;
# with `scaling` (the scaling-host flags), "scaling host first" where one is a scaling host and the
# other is not.
aib_rank_basis <- function(gain_tier, hhi_tier, capability_tier, capability_value, friendshore, compatibility, scaling = NULL) {
  n <- length(gain_tier)
  if (n < 2) return(rep(NA_character_, n))
  keys <- list(`resilience gain` = gain_tier, `diversification (HHI change)` = hhi_tier, `capability tier` = capability_tier,
               `capability at scale` = capability_value, Friendshore = friendshore, compatibility = compatibility)
  if (!is.null(scaling)) keys <- c(list(`scaling host first` = as.numeric(scaling)), keys)
  other <- c(2L, seq_len(n - 1L))
  vapply(seq_len(n), function(i) {
    j <- other[i]
    for (k in names(keys)) {
      a <- keys[[k]][i]
      b <- keys[[k]][j]
      if (!identical(is.na(a), is.na(b)) || (!is.na(a) && abs(a - b) > 1e-12)) return(k)
    }
    "tied"
  }, character(1))
}
