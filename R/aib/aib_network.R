# Allied Industrial Base (AIB): the emergent AIB network (docs/aib_methodology.md, 8). It is the
# aggregate of the bilateral trade and investment relationships the optimizer selects across
# nodes and shocks; no alliance is fixed in advance and no country ranking is produced. Edges keep
# their direction: trade edges run exporter -> importer, investment edges investor (the importer
# whose gap the new capacity serves) -> host.

# Country view of the network: each economy's roles kept apart (as trade supplier, trade buyer,
# investment host, investor), not collapsed into one rank. Built from aib_network_pairs().
aib_network_countries <- function(pairs, node_shock_importer = NULL) {
  if (is.null(pairs) || !nrow(pairs)) return(NULL)
  side <- function(col, other, kind) {
    sel <- if (kind == "trade") pairs$n_trade_selected > 0 else pairs$n_investment_selected > 0
    p <- pairs[sel, , drop = FALSE]
    if (!nrow(p)) return(NULL)
    p %>%
      dplyr::group_by(iso3 = .data[[col]], scenario) %>%
      dplyr::summarise(
        n_partners = dplyr::n_distinct(.data[[other]]),
        n_selected = sum(if (kind == "trade") n_trade_selected else n_investment_selected),
        headline_contribution = sum(if (kind == "trade") headline_trade_contribution else headline_investment_contribution),
        cumulative_contribution = sum(if (kind == "trade") cumulative_trade_resilience_contribution else cumulative_investment_resilience_contribution),
        cumulative_coordination_dividend = if (kind == "trade") sum(cumulative_coordination_dividend) else 0,
        headline_volume_usd = if (kind == "trade") sum(headline_trade_volume_usd) else NA_real_,
        n_upstream_partners = dplyr::n_distinct(.data[[other]][upstream]),
        n_midstream_partners = dplyr::n_distinct(.data[[other]][midstream]),
        n_downstream_partners = dplyr::n_distinct(.data[[other]][downstream]),
        technologies = paste(sort(unique(unlist(strsplit(technologies, "; ")))), collapse = "; "),
        .groups = "drop"
      )
  }
  parts <- list(
    supplier = side("supplier_iso", "buyer_iso", "trade"),
    buyer = side("buyer_iso", "supplier_iso", "trade"),
    investment_host = side("supplier_iso", "buyer_iso", "investment"),
    investor = side("buyer_iso", "supplier_iso", "investment")
  )
  long <- dplyr::bind_rows(lapply(names(parts), function(k) if (is.null(parts[[k]])) NULL else dplyr::mutate(parts[[k]], role = k)))
  value_cols <- c("n_partners", "n_selected", "headline_contribution", "cumulative_contribution", "cumulative_coordination_dividend",
                  "headline_volume_usd", "n_upstream_partners", "n_midstream_partners", "n_downstream_partners")
  wide <- long %>%
    dplyr::select(iso3, scenario, role, dplyr::all_of(value_cols)) %>%
    tidyr::pivot_wider(names_from = role, values_from = dplyr::all_of(value_cols), names_glue = "{role}_{.value}")
  # Drop the combinations that do not exist (coordination dividend and volume belong to trade).
  wide <- wide[, !grepl("^(investment_host|investor)_(cumulative_coordination_dividend|headline_volume_usd)$", names(wide)), drop = FALSE]
  num <- vapply(wide, is.numeric, logical(1))
  wide[num] <- lapply(wide[num], function(v) dplyr::coalesce(v, 0))
  techs <- long %>%
    dplyr::group_by(iso3, scenario) %>%
    dplyr::summarise(
      technologies = paste(sort(unique(unlist(strsplit(technologies, "; ")))), collapse = "; "),
      n_technologies = length(unique(unlist(strsplit(technologies, "; ")))),
      roles = paste(sort(unique(role)), collapse = "; "),
      .groups = "drop"
    )
  out <- wide %>% dplyr::left_join(techs, by = c("iso3", "scenario"))
  if (!is.null(node_shock_importer)) {
    out <- out %>% dplyr::left_join(node_shock_importer, by = c("iso3", "scenario"))
  }
  out
}

# One record per bilateral relationship, oriented as goods move (supplier -> buyer): trade edges as
# they are (exporter -> importer) and investment edges turned round (host -> investor, the host's
# new capacity serving the investor). Only each node's first-ranked host counts as a selected
# investment relationship, so alternative hosts are not summed. Columns: trade and investment
# selections, technologies, sub-sectors, stage participation, high-severity participation, the
# optimized trade volume, resilience contributions (trade, investment), the coordination dividend,
# Export Opportunity (supplier -> buyer) and Friendshore (buyer -> supplier, the investor rating the
# host).
aib_network_pairs <- function(trade, invest_top, node_info, node_shock, headline_shock, high_severity = 0.4,
                              node_cells = NULL, op_index = NULL, fs_index = NULL) {
  sev <- node_shock %>% dplyr::group_by(node_id, shock_id, scenario) %>%
    dplyr::summarise(shock_exposure = max(shock_exposure), .groups = "drop")
  tr <- trade %>% dplyr::filter(relationship != "domestic retention") %>%
    dplyr::transmute(supplier_iso = exporter_iso, buyer_iso = importer_iso, scenario, shock_id, node_id, kind = "trade",
                     usd = delta_usd, contribution = delta_share_of_demand, new_link = relationship == "new")
  iv <- if (is.null(invest_top) || !nrow(invest_top)) NULL else invest_top %>% dplyr::filter(relationship != "domestic") %>%
    dplyr::transmute(supplier_iso = host_iso, buyer_iso = investor_iso, scenario, shock_id, node_id, kind = "investment",
                     usd = supply_from_new_capacity_usd, contribution = supply_from_new_capacity, new_link = NA)
  all <- dplyr::bind_rows(tr, iv) %>% dplyr::left_join(sev, by = c("node_id", "shock_id", "scenario"))
  if (!nrow(all)) return(NULL)
  all$stage <- node_info$supply_chain[match(all$node_id, node_info$node_id)]
  all$techs <- node_info$techs[match(all$node_id, node_info$node_id)]
  out <- all %>%
    dplyr::group_by(supplier_iso, buyer_iso, scenario) %>%
    dplyr::summarise(
      n_trade_selected = dplyr::n_distinct(paste(node_id, shock_id)[kind == "trade"]),
      n_investment_selected = dplyr::n_distinct(paste(node_id, shock_id)[kind == "investment"]),
      n_shocks = dplyr::n_distinct(shock_id),
      n_subsectors = dplyr::n_distinct(node_id),
      n_technologies = length(unique(unlist(strsplit(techs, "; ")))),
      technologies = paste(sort(unique(unlist(strsplit(techs, "; ")))), collapse = "; "),
      subsectors = paste(sort(unique(node_id)), collapse = "; "),
      n_high_severity = dplyr::n_distinct(paste(node_id, shock_id)[dplyr::coalesce(shock_exposure, 0) >= high_severity]),
      upstream = any(stage == "Upstream"), midstream = any(stage == "Midstream"), downstream = any(stage == "Downstream"),
      headline_trade_volume_usd = sum(usd[kind == "trade" & shock_id == headline_shock]),
      headline_trade_contribution = sum(contribution[kind == "trade" & shock_id == headline_shock]),
      headline_investment_contribution = sum(contribution[kind == "investment" & shock_id == headline_shock]),
      cumulative_trade_resilience_contribution = sum(contribution[kind == "trade"]),
      cumulative_coordination_dividend = sum(contribution[kind == "trade" & dplyr::coalesce(new_link, FALSE)]),
      cumulative_investment_resilience_contribution = sum(contribution[kind == "investment"]),
      .groups = "drop"
    )
  if (!is.null(node_cells) && (!is.null(op_index) || !is.null(fs_index))) {
    pairs <- all %>% dplyr::distinct(supplier_iso, buyer_iso, scenario, node_id) %>%
      dplyr::inner_join(dplyr::distinct(node_cells, node_id, tech, supply_chain), by = "node_id", relationship = "many-to-many")
    if (!is.null(op_index)) {
      op <- pairs %>% dplyr::inner_join(op_index, by = c("supplier_iso" = "reporter_iso", "buyer_iso" = "partner_iso", "tech", "supply_chain")) %>%
        dplyr::group_by(supplier_iso, buyer_iso, scenario) %>% dplyr::summarise(export_opportunity = mean(value), .groups = "drop")
      out <- out %>% dplyr::left_join(op, by = c("supplier_iso", "buyer_iso", "scenario"))
    }
    if (!is.null(fs_index)) {
      fs <- pairs %>% dplyr::inner_join(fs_index, by = c("buyer_iso" = "reporter_iso", "supplier_iso" = "partner_iso", "tech", "supply_chain")) %>%
        dplyr::group_by(supplier_iso, buyer_iso, scenario) %>% dplyr::summarise(friendshore = mean(value), .groups = "drop")
      out <- out %>% dplyr::left_join(fs, by = c("supplier_iso", "buyer_iso", "scenario"))
    }
  }
  out %>% dplyr::arrange(scenario, dplyr::desc(cumulative_trade_resilience_contribution + cumulative_investment_resilience_contribution))
}

# ---- The observed (Baseline) network: trade as it is in the reference year, before any shock ----------

# Each node's observed supply to the demand scope by exporter: the reference-year shipments into the
# scope's importers (flows: node_id, exporter_iso, importer_iso, value in USD) summed by exporter, with
# the node's demand (the scope's imports of the node, the D of aib_node_problem()) and each exporter's
# share of it. Every exporter is kept, so any top-n list and the exact HHI (sum of the squared shares)
# can be read from it; `node_ids` restricts the nodes. Rows are ordered by node, then supply (rank 1 =
# the largest supplier; ties by ISO3 code).
aib_observed_supply <- function(flows, demand_scope, node_ids = NULL) {
  f <- flows[flows$value > 0 & flows$importer_iso %in% demand_scope, c("node_id", "exporter_iso", "importer_iso", "value"), drop = FALSE]
  if (!is.null(node_ids)) f <- f[f$node_id %in% node_ids, , drop = FALSE]
  if (!nrow(f)) {
    return(tibble::tibble(node_id = character(), exporter_iso = character(), supply_usd = numeric(), demand_usd = numeric(),
                          share = numeric(), rank = integer()))
  }
  f %>%
    dplyr::group_by(node_id, exporter_iso) %>%
    dplyr::summarise(supply_usd = sum(value), .groups = "drop") %>%
    dplyr::group_by(node_id) %>%
    dplyr::mutate(demand_usd = sum(supply_usd), share = supply_usd / demand_usd) %>%
    dplyr::arrange(dplyr::desc(supply_usd), exporter_iso, .by_group = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()
}

# The observed network as a pseudo-shock for aib_network_pairs(): every reference-year shipment of a
# node into the demand scope becomes an "existing" trade relationship whose USD is its value and whose
# contribution is its share of the node's demand; every node's exposure is 0 (nothing is lost). Passed
# with `headline_shock = shock_id`, the pairs' headline and cumulative trade fields are then the observed
# USD and the summed shares of node demand; the coordination dividend, investment and high-severity
# counts are 0 by construction. Returns list(trade, node_shock).
aib_observed_network_inputs <- function(flows, demand_scope, node_ids = NULL, shock_id = "baseline", scenario = "observed") {
  f <- flows[flows$value > 0 & flows$importer_iso %in% demand_scope, c("node_id", "exporter_iso", "importer_iso", "value"), drop = FALSE]
  if (!is.null(node_ids)) f <- f[f$node_id %in% node_ids, , drop = FALSE]
  D <- tapply(f$value, f$node_id, sum)
  trade <- tibble::tibble(node_id = f$node_id, shock_id = rep(shock_id, nrow(f)), scenario = rep(scenario, nrow(f)),
                          exporter_iso = f$exporter_iso, importer_iso = f$importer_iso,
                          delta_usd = f$value, delta_share_of_demand = f$value / as.numeric(D[f$node_id]),
                          relationship = rep("existing", nrow(f)))
  ids <- sort(unique(f$node_id))
  list(trade = trade,
       node_shock = tibble::tibble(node_id = ids, shock_id = rep(shock_id, length(ids)), scenario = rep(scenario, length(ids)),
                                   shock_exposure = rep(0, length(ids))))
}

# Keep the supplier -> buyer pairs whose summed contribution (trade plus investment, shares of node
# demand summed over the nodes) is at least `min_share`: the observed network has thousands of small
# pairs, and the 0.5% default keeps about 96% of its USD in about a fifth of them.
aib_network_min_share <- function(pairs, min_share = 0.005) {
  if (is.null(pairs) || !nrow(pairs)) return(pairs)
  tot <- dplyr::coalesce(pairs$cumulative_trade_resilience_contribution, 0) + dplyr::coalesce(pairs$cumulative_investment_resilience_contribution, 0)
  pairs[tot >= min_share - 1e-12, , drop = FALSE]
}

# Importer exposure of a scenario for aib_network_countries(): each importer's lost supply (USD) summed
# over the strategic-scope nodes and the number of nodes where it loses any, repeated for every trade
# scenario. `loss`: node_id, importer_iso, lost_share, lost_usd (scripts/32 `importer_loss`).
aib_importer_exposure <- function(loss, scope_nodes, scenarios) {
  loss %>% dplyr::filter(node_id %in% scope_nodes) %>% dplyr::group_by(iso3 = importer_iso) %>%
    dplyr::summarise(headline_lost_usd = sum(lost_usd), n_exposed_subsectors = sum(lost_share > 0), .groups = "drop") %>%
    tidyr::expand_grid(scenario = scenarios)
}
