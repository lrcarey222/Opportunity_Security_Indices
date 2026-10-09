# --- FILE: R/indices/allied_network_design.R ---------------------------------
# Allied network design: choose a coalition-wide division of labor (specialization)
# and implied trade/relationship flows for each (tech x supply_chain) stage.
#
# Conventions (asserted, not assumed):
# - Nodes are one row per (iso3c, tech, supply_chain). The EO/ES/Policy inputs can carry two spellings
#   of one country (e.g. "Viet Nam"/"Vietnam") and the development table is dyadic (reporter x
#   partner), so every node input is collapsed to one row per key before it is joined.
# - Edges are one row per (producer, consumer, tech, supply_chain). In the edge and flow tables
#   `reporter_iso` is the PRODUCER (exporter) and `partner_iso` the CONSUMER (importer). The partner
#   theme tables are oriented differently: the Opportunity Index is reported by the exporter (it rates
#   the reporter selling to the partner), while the Friendshore Index is reported by the importer (it
#   rates the partner as a supplier to the reporter). Hence
#   edge(p -> c) = w_fs * FS(reporter = c, partner = p) + w_op * OP(reporter = p, partner = c).
# - A node is scored on the components it has (EO, policy, ES; weights renormalized). A coalition
#   member without a policy index gets the within-stage coalition median, flagged as policy_imputed.
#   node_policy = FALSE (scripts/30 since Oct 2026) drops the policy term from the node score: the
#   score is then EO and ES only (node_weights$eo and $resilience, renormalized), the policy index is
#   not read and nothing is imputed. The default TRUE keeps the three-component score.
# - The coalition (iso3c_network) is passed by the caller. scripts/30 derives it with
#   coalition_members() (R/indices/build_indices.R): the two-tier coalition of coalition_tbl.rds
#   (scripts/13_build_coalition.R), or the alignment coalition of geo_coalition_tbl.rds (scripts/12)
#   until that exists; allied_network_default_iso3c() is only a legacy/test default.
# - Network nodes. Without network_members (or when every member's network_node is its own ISO3
#   code) every member is its own node, as before. When members share a network_node (the EU-27 as
#   one node "EUU", Oct 2026) the design is solved over nodes, aggregated from the members before
#   anything is ranked (see "Network nodes" below for the rules): producer inputs (EO, ES, policy,
#   development) are the members' means under the stage weights node_member_weights
#   (eu_stage_weights_tbl.rds); demand need is the GDP-weighted mean member need and size the summed
#   GDP; an edge to or from a bundled node is the mean of its members' edges weighted by their basket
#   trade on that edge (node_trade_tbl) or, without trade, by the stage weights; edges inside a node
#   are dropped; the portfolio cap uses the summed GDP. Every output table is at node level
#   (iso3c = the node code); member_shares splits each node's production share back to its members
#   by stage weight (derived, not designed).
# - Objective per stage (maximised): w_node * sum_p a_p x_p + w_edge * sum_pc W_pc f_pc
#   + w_dev * sum_p dev_p z_p - diversity_lambda * HHI, with x_p the production share and
#   HHI = sum_p x_p^2. a_p, W_pc and dev_p lie in [0, 1] and the shares sum to one, so the node and
#   edge terms are in score units on [0, w_node] and [0, w_edge], and diversity_lambda is in score
#   units per unit of HHI. Without the HHI term (diversity_lambda = 0, the legacy design) the
#   objective is linear and the solution is bang-bang: producers sit at max_share. At an interior
#   optimum two producers' shares differ by (difference in marginal score) / (2 * diversity_lambda).
#   The MILP carries the HHI as a tangent (outer) approximation; see allied_network_hhi_breakpoints.
# - supply_chains restricts the stages solved; the portfolio caps count top-K appearances over the
#   solved stages only.

# Dependencies expected elsewhere in repo: dplyr, tidyr, tibble, purrr, stringr, rlang
# Optional solver deps for MILP: ompr, ompr.roi, ROI, ROI.plugin.glpk

if (!exists("median_scurve", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "utils", "scurve.R"))
}
if (!exists("require_columns", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "utils", "schema.R"))
}
if (!exists("partnership_strength_country_to_iso", mode = "function")) {
  source(file.path(
    dirname(sys.frame(1)$ofile),
    "..", "themes", "partnership_strength", "partnership_strength_helpers.R"
  ))
}

# ---- Defaults ---------------------------------------------------------------

# LEGACY / TEST DEFAULT ONLY. The hand-picked 23-member coalition used until Oct 2026. Production
# no longer uses it: scripts/30 derives the coalition with coalition_members() in
# R/indices/build_indices.R (coalition_tbl.rds from scripts/13, or geo_coalition_tbl.rds from
# scripts/12 until that exists) and passes it as iso3c_network. It remains the default of the
# iso3c_network arguments below so that direct calls and tests keep working; do not use it for
# reported results.
allied_network_default_iso3c <- function() {
  # ARE is the ISO3 code for the United Arab Emirates; the earlier "UAE" matched no table.
  c(
    "USA","CAN","JPN","AUS","IND","MEX","KOR","GBR","DEU","FRA","ITA","BRA","SAU",
    "ZAF","IDN","NOR","ARE","VNM","KEN","DNK","ARG","MAR","CHL"
  )
}

# Node columns carried through the solvers into the specialization table.
allied_network_node_extra_cols <- function() {
  c(
    "gdp_usd", "gdp_imputed", "size_idx", "need_idx", "demand_need_component", "demand_size_component",
    "policy_imputed", "node_components_n", "demand_need_missing"
  )
}

allied_network_normalize_weights <- function(w) {
  nm <- names(w)
  w <- suppressWarnings(as.numeric(w))
  if (!is.null(nm)) names(w) <- nm
  if (length(w) == 0 || all(is.na(w)) || sum(w, na.rm = TRUE) <= 0) {
    out <- rep(NA_real_, length(w))
    if (!is.null(nm)) names(out) <- nm
    return(out)
  }
  w / sum(w, na.rm = TRUE)
}

allied_network_hhi <- function(shares) {
  shares <- suppressWarnings(as.numeric(shares))
  shares <- shares[is.finite(shares)]
  if (!length(shares)) return(NA_real_)
  sum(shares^2)
}

# node_policy switches the policy term of the producer score on (TRUE, the legacy score) or off.
allied_network_check_node_policy <- function(node_policy) {
  if (!is.logical(node_policy) || length(node_policy) != 1 || is.na(node_policy)) {
    stop("node_policy must be TRUE or FALSE.")
  }
  node_policy
}

# network_members: NULL, or one row per coalition member with its ISO3 code (column iso3c or iso3),
# tier and network_node (coalition_members() in R/indices/build_indices.R, plus any countries
# allied_network_add_node_members() adds to a shared node). It must list exactly the iso3c_network
# members. Returns a tibble (iso3c, tier, network_node, node_tier) in iso3c_network order; a missing
# or empty network_node is the member's own iso3c, and node_tier (the tier of the member's node, as
# coalition_members() carries it) defaults to the member's tier. Members that share a network_node
# are aggregated into one node by allied_network_design().
allied_network_check_members <- function(network_members, iso3c_network) {
  if (is.null(network_members)) return(NULL)
  if (!is.data.frame(network_members)) {
    stop("network_members must be NULL or a data frame with iso3c (or iso3), tier and network_node.")
  }
  key <- intersect(c("iso3c", "iso3"), names(network_members))[1]
  if (is.na(key)) stop("network_members needs an iso3c (or iso3) column.")
  require_columns(network_members, c("tier", "network_node"), label = "network_members")
  iso3c <- toupper(trimws(as.character(network_members[[key]])))
  if (anyNA(iso3c) || any(!nzchar(iso3c)) || anyDuplicated(iso3c)) {
    stop("network_members must have one row per member (no missing or duplicate ISO3 codes).")
  }
  missing_members <- setdiff(iso3c_network, iso3c)
  extra_members <- setdiff(iso3c, iso3c_network)
  if (length(missing_members) || length(extra_members)) {
    stop(
      "network_members must list exactly the iso3c_network members; missing: ",
      if (length(missing_members)) paste(missing_members, collapse = ", ") else "none",
      "; not in iso3c_network: ",
      if (length(extra_members)) paste(extra_members, collapse = ", ") else "none", "."
    )
  }
  node <- toupper(trimws(as.character(network_members$network_node)))
  tier <- as.character(network_members$tier)
  node_tier <- if ("node_tier" %in% names(network_members)) as.character(network_members$node_tier) else rep(NA_character_, length(iso3c))
  out <- tibble::tibble(
    iso3c = iso3c,
    tier = tier,
    network_node = ifelse(is.na(node) | !nzchar(node), iso3c, node),
    node_tier = dplyr::coalesce(node_tier, tier)
  )
  # A node code that is also a member's ISO3 code must be that member's own node, so a country code
  # never names a node the country is not in.
  clash <- out$network_node %in% out$iso3c & out$network_node != out$iso3c &
    out$network_node[match(out$network_node, out$iso3c)] != out$network_node
  if (any(clash)) {
    stop(
      "network_members: network_node ", paste(unique(out$network_node[clash]), collapse = ", "),
      " is the ISO3 code of a member that is not in that node."
    )
  }
  out[match(iso3c_network, out$iso3c), ]
}

# ---- Network nodes (member aggregation) ----------------------------------------
#
# Members that share a network_node (the EU-27, node "EUU") are solved as one node. Every input is
# aggregated from the members before anything is ranked or scaled, so a bundled node is one point in
# each within-stage S-curve, exactly like a single-country node. Rules (multi-member nodes only; a
# single-member node passes through unchanged):
# - Member weights per (node, tech, stage): node_member_weights (eu_stage_weights_tbl.rds from
#   scripts/13: the members' production or export shares, else GDP shares) restricted to the node's
#   members and renormalised to sum to one; a listed member outside network_members is dropped, an
#   unlisted member weighs 0. Every solved stage needs weights for every multi-member node.
# - Producer inputs (EO, ES, policy, development potential): weighted mean of the members' raw
#   values over the members that have a value, the weights renormalised over those members. A member
#   without a value drops out; a member with weight 0 (e.g. a non-producer under a production basis)
#   never counts; with no positive-weight member left the node value is missing (and the node is
#   scored on its other components, as any node is).
# - Demand: size = the members' summed GDP (gdp_imputed if any member's GDP is imputed); need =
#   1 - S(es_need_raw) with es_need_raw the GDP-weighted mean of the members' raw ES, i.e. the
#   GDP-weighted mean of the members' raw need (1 - ES), put on the node-level S-curve like every
#   other node's need. For a single-member node es_need_raw = es_raw and need_idx = 1 - es_idx.
# - Edges, per component (Friendshore, Opportunity) before the missing-component 0 fill: the edge
#   between nodes P and C is the weighted mean of the member edges p -> c (p in P, c in C) with a
#   value, weighted by the basket trade p -> c (node_trade_tbl, the reconciled last-year matrix:
#   for the EU as consumer, its members' imports from the producer; for the EU as producer, its
#   members' exports to the consumer). Where those members have no trade on the edge the weights are
#   the stage weights (product of both sides'), and if those are all 0, equal. Edges between two
#   members of one node are dropped (the node's self-supply edge is treated like any node's).
# - Portfolio cap: from the node's summed GDP.
# - member_shares: each node's designed production share split back to its members by the stage
#   weights (production_share_derived; derived, not designed; sums to the node share).

# TRUE when network_members (checked) bundles some member into a node other than its own code.
allied_network_aggregates <- function(network_members) {
  !is.null(network_members) && any(network_members$network_node != network_members$iso3c)
}

# Countries that belong to a coalition member's shared node without being coalition members
# themselves (the EU members below the coalition's own tier rules), added so that a bundled node is
# the whole bloc. members: coalition_members() output (iso3, tier, network_node, ...). node_tbl: a
# table listing node membership (iso3 or iso3c, network_node; optionally country, tier, node_tier),
# e.g. coalition_tbl.rds. Only nodes whose code is not a member's own code (bundles such as "EUU")
# are expanded. The added rows keep their own tier (e.g. "none") and get node_tier from node_tbl
# (else the tier of the node's coalition members); node_only flags them. Returns members (with
# node_only = FALSE) plus the added rows, sorted by iso3.
allied_network_add_node_members <- function(members, node_tbl) {
  require_columns(members, c("iso3", "tier", "network_node"), label = "members")
  key <- intersect(c("iso3", "iso3c"), names(node_tbl))[1]
  if (is.na(key)) stop("node_tbl needs an iso3 (or iso3c) column.")
  require_columns(node_tbl, "network_node", label = "node_tbl")
  members <- tibble::as_tibble(members)
  members$node_only <- FALSE
  own <- toupper(trimws(as.character(members$iso3)))
  m_node <- toupper(trimws(as.character(members$network_node)))
  bundles <- unique(m_node[!is.na(m_node) & m_node != own])
  if (!length(bundles)) return(members)
  cand <- tibble::tibble(
    iso3 = toupper(trimws(as.character(node_tbl[[key]]))),
    network_node = toupper(trimws(as.character(node_tbl$network_node))),
    country = if ("country" %in% names(node_tbl)) as.character(node_tbl$country) else NA_character_,
    tier = if ("tier" %in% names(node_tbl)) as.character(node_tbl$tier) else NA_character_,
    node_tier = if ("node_tier" %in% names(node_tbl)) as.character(node_tbl$node_tier) else NA_character_
  ) %>%
    dplyr::filter(.data$network_node %in% bundles, !.data$iso3 %in% own, !is.na(.data$iso3)) %>%
    dplyr::distinct(.data$iso3, .keep_all = TRUE)
  if (!nrow(cand)) return(members)
  member_node_tier <- if ("node_tier" %in% names(members)) as.character(members$node_tier) else as.character(members$tier)
  cand$node_tier <- dplyr::coalesce(cand$node_tier, member_node_tier[match(cand$network_node, m_node)])
  cand$node_only <- TRUE
  if ("source" %in% names(members)) cand$source <- "node_member"
  dplyr::bind_rows(members, cand) %>% dplyr::arrange(.data$iso3)
}

# One row per network node: network_node, tier (the members' node_tier when they agree, else
# "mixed"), n_members, members (space-separated ISO3 codes) and node_order (first appearance in
# iso3c_network order).
allied_network_node_table <- function(network_members) {
  network_members %>%
    dplyr::mutate(.order = dplyr::row_number()) %>%
    dplyr::group_by(.data$network_node) %>%
    dplyr::summarise(
      tier = if (dplyr::n_distinct(.data$node_tier) == 1) dplyr::first(.data$node_tier) else "mixed",
      n_members = dplyr::n(),
      members = paste(sort(.data$iso3c), collapse = " "),
      node_order = min(.data$.order),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$node_order)
}

# Member weights of every member in every stage of `stages` (tech, supply_chain): network_node,
# iso3c, tech, supply_chain, member_weight (sums to one per node and stage) and weight_basis
# ("own" for a single-member node, else node_member_weights' basis column, or "stage_weight").
# Stops when a multi-member node has no positive weight for one of the stages.
allied_network_member_weights <- function(network_members, node_member_weights, stages) {
  stages <- dplyr::distinct(tibble::as_tibble(stages)[c("tech", "supply_chain")])
  sizes <- dplyr::count(network_members, .data$network_node, name = "n_members")
  multi_nodes <- sizes$network_node[sizes$n_members > 1]
  base <- network_members %>%
    dplyr::select("network_node", "iso3c") %>%
    tidyr::crossing(stages)
  single <- base %>%
    dplyr::filter(!.data$network_node %in% multi_nodes) %>%
    dplyr::mutate(member_weight = 1, weight_basis = "own")
  if (!length(multi_nodes)) return(single)

  if (is.null(node_member_weights)) {
    stop(
      "network_members bundles members into node(s) ", paste(multi_nodes, collapse = ", "),
      ": node_member_weights (e.g. eu_stage_weights_tbl.rds) is required to aggregate them."
    )
  }
  key <- intersect(c("iso3c", "iso3"), names(node_member_weights))[1]
  if (is.na(key)) stop("node_member_weights needs an iso3c (or iso3) column.")
  require_columns(node_member_weights, c("network_node", "tech", "supply_chain", "weight"), label = "node_member_weights")
  w <- tibble::tibble(
    network_node = toupper(trimws(as.character(node_member_weights$network_node))),
    iso3c = toupper(trimws(as.character(node_member_weights[[key]]))),
    tech = as.character(node_member_weights$tech),
    supply_chain = as.character(node_member_weights$supply_chain),
    weight = suppressWarnings(as.numeric(node_member_weights$weight)),
    weight_basis = if ("basis" %in% names(node_member_weights)) as.character(node_member_weights$basis) else "stage_weight"
  )
  if (any(!is.finite(w$weight) | w$weight < 0)) stop("node_member_weights$weight must be finite and >= 0.")
  if (anyDuplicated(w[c("network_node", "iso3c", "tech", "supply_chain")])) {
    stop("node_member_weights must have one row per (network_node, member, tech, supply_chain).")
  }
  multi <- base %>%
    dplyr::filter(.data$network_node %in% multi_nodes) %>%
    dplyr::left_join(w, by = c("network_node", "iso3c", "tech", "supply_chain")) %>%
    dplyr::group_by(.data$network_node, .data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      weight = dplyr::coalesce(.data$weight, 0),
      weight_basis = dplyr::coalesce(.data$weight_basis, dplyr::first(stats::na.omit(.data$weight_basis)), "stage_weight"),
      .total = sum(.data$weight)
    ) %>%
    dplyr::ungroup()
  bad <- multi %>% dplyr::filter(.data$.total <= 0) %>% dplyr::distinct(.data$network_node, .data$tech, .data$supply_chain)
  if (nrow(bad)) {
    stop(
      "node_member_weights has no positive weight for node ", paste(unique(bad$network_node), collapse = ", "),
      " in ", nrow(bad), " stage(s), e.g. ", bad$tech[1], " / ", bad$supply_chain[1], "."
    )
  }
  multi <- multi %>%
    dplyr::mutate(member_weight = .data$weight / .data$.total) %>%
    dplyr::select("network_node", "iso3c", "tech", "supply_chain", "member_weight", "weight_basis")
  dplyr::bind_rows(single, multi)
}

# Weighted mean of x over the elements with a finite x and a positive weight (weights renormalised);
# NA if there are none.
allied_network_wmean <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Member-level node inputs (allied_network_node_inputs()) aggregated to network nodes by the rules
# above. Returns the same columns at node level (iso3c = the node code) plus es_need_raw and
# n_members_inputs (members with an input row in the stage), one row per (node, tech, supply_chain)
# with at least one member row. node_labels names a bundled node's `country` (else country_info's
# name for the code, else the code). node_size_tbl (allied_network_node_size_tbl()) gives a bundled
# node the GDP of all its members, also in a stage where some member has no input row; without it
# the GDP is summed over the members with a row.
allied_network_aggregate_node_inputs <- function(raw, network_members, member_weights, node_labels = NULL, country_info = NULL,
                                                 node_size_tbl = NULL) {
  require_columns(raw, c("iso3c", "country", "tech", "supply_chain", "eo_raw", "es_raw", "policy_raw", "dev_potential", "gdp_usd", "gdp_imputed"), label = "node inputs")
  node_of <- stats::setNames(network_members$network_node, network_members$iso3c)
  sizes <- table(network_members$network_node)
  multi_nodes <- names(sizes)[sizes > 1]
  raw <- raw %>% dplyr::mutate(network_node = unname(node_of[.data$iso3c]))
  single <- raw %>%
    dplyr::filter(!.data$network_node %in% multi_nodes) %>%
    dplyr::mutate(iso3c = .data$network_node, es_need_raw = .data$es_raw, n_members_inputs = 1L) %>%
    dplyr::select(-"network_node")
  label_of <- function(node) {
    lab <- if (!is.null(node_labels)) unname(node_labels[node]) else NA_character_
    if (is.na(lab) && !is.null(country_info)) lab <- as.character(country_info$country[match(node, country_info$iso3c)])
    if (is.na(lab)) lab <- node
    lab
  }
  multi <- raw %>%
    dplyr::filter(.data$network_node %in% multi_nodes) %>%
    dplyr::left_join(
      member_weights %>% dplyr::select("network_node", "iso3c", "tech", "supply_chain", "member_weight"),
      by = c("network_node", "iso3c", "tech", "supply_chain"),
      relationship = "one-to-one"
    )
  if (anyNA(multi$member_weight)) {
    miss <- multi %>% dplyr::filter(is.na(.data$member_weight)) %>% dplyr::distinct(.data$network_node, .data$tech, .data$supply_chain)
    stop("No member weights for node ", miss$network_node[1], " in ", miss$tech[1], " / ", miss$supply_chain[1], ".")
  }
  multi <- multi %>%
    dplyr::group_by(.data$network_node, .data$tech, .data$supply_chain) %>%
    # summarise() sees each new column in the expressions after it, so es_need_raw (members' ES and
    # GDP) comes before es_raw and gdp_usd are replaced by their node values
    dplyr::summarise(
      # need: GDP weights (a member without GDP drops out; with no GDP at all, equal weights)
      es_need_raw = if (any(is.finite(.data$gdp_usd) & .data$gdp_usd > 0)) {
        allied_network_wmean(.data$es_raw, .data$gdp_usd)
      } else {
        allied_network_wmean(.data$es_raw, rep(1, dplyr::n()))
      },
      eo_raw = allied_network_wmean(.data$eo_raw, .data$member_weight),
      es_raw = allied_network_wmean(.data$es_raw, .data$member_weight),
      policy_raw = allied_network_wmean(.data$policy_raw, .data$member_weight),
      dev_potential = allied_network_wmean(.data$dev_potential, .data$member_weight),
      gdp_imputed = any(.data$gdp_imputed %in% TRUE),
      gdp_usd = if (any(is.finite(.data$gdp_usd))) sum(.data$gdp_usd, na.rm = TRUE) else NA_real_,
      n_members_inputs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(iso3c = "network_node") %>%
    dplyr::mutate(country = vapply(.data$iso3c, label_of, character(1), USE.NAMES = FALSE))
  if (!is.null(node_size_tbl) && nrow(multi)) {
    require_columns(node_size_tbl, c("iso3c", "gdp_usd", "gdp_imputed"), label = "node_size_tbl")
    k <- match(multi$iso3c, node_size_tbl$iso3c)
    multi$gdp_usd <- ifelse(is.na(k), multi$gdp_usd, node_size_tbl$gdp_usd[k])
    multi$gdp_imputed <- ifelse(is.na(k), multi$gdp_imputed, node_size_tbl$gdp_imputed[k] %in% TRUE)
  }
  out <- dplyr::bind_rows(single, multi) %>%
    dplyr::arrange(.data$iso3c, .data$tech, .data$supply_chain)
  if (anyDuplicated(out[c("iso3c", "tech", "supply_chain")])) {
    stop("allied_network_aggregate_node_inputs: nodes are not unique per (node, tech, supply_chain).")
  }
  out
}

# Node-level size table: the members' GDP summed per node (gdp_imputed if any member's GDP is
# imputed). country_size_tbl is member-level (iso3c, gdp_usd, optional gdp_imputed).
allied_network_node_size_tbl <- function(country_size_tbl, network_members) {
  require_columns(country_size_tbl, c("iso3c", "gdp_usd"), label = "country_size_tbl")
  tbl <- tibble::as_tibble(country_size_tbl) %>%
    dplyr::transmute(
      iso3c = as.character(.data$iso3c),
      gdp_usd = suppressWarnings(as.numeric(.data$gdp_usd)),
      gdp_imputed = if ("gdp_imputed" %in% names(country_size_tbl)) dplyr::coalesce(as.logical(.data$gdp_imputed), FALSE) else FALSE
    ) %>%
    dplyr::distinct(.data$iso3c, .keep_all = TRUE)
  network_members %>%
    dplyr::select("iso3c", "network_node") %>%
    dplyr::left_join(tbl, by = "iso3c") %>%
    dplyr::group_by(.data$network_node) %>%
    dplyr::summarise(
      gdp_usd = if (any(is.finite(.data$gdp_usd))) sum(.data$gdp_usd, na.rm = TRUE) else NA_real_,
      gdp_imputed = any(.data$gdp_imputed %in% TRUE),
      n_members = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(iso3c = "network_node")
}

# Member-level basket trade for the edge weights, from a reconciled flow matrix
# (partnership_strength_build_trade_cells(): exporter_iso, importer_iso, tech, supply_chain and
# value_last, or value): the flows between network members, unobserved (NA) flows dropped.
allied_network_member_trade <- function(trade_cells, iso3c_network) {
  require_columns(trade_cells, c("exporter_iso", "importer_iso", "tech", "supply_chain"), label = "trade_cells")
  value_col <- intersect(c("value_last", "value"), names(trade_cells))[1]
  if (is.na(value_col)) stop("trade_cells needs a value_last (or value) column.")
  out <- tibble::tibble(
    exporter_iso = as.character(trade_cells$exporter_iso),
    importer_iso = as.character(trade_cells$importer_iso),
    tech = as.character(trade_cells$tech),
    supply_chain = as.character(trade_cells$supply_chain),
    value = suppressWarnings(as.numeric(trade_cells[[value_col]]))
  ) %>%
    dplyr::filter(
      .data$exporter_iso %in% iso3c_network, .data$importer_iso %in% iso3c_network,
      .data$exporter_iso != .data$importer_iso, is.finite(.data$value)
    )
  if (anyDuplicated(out[c("exporter_iso", "importer_iso", "tech", "supply_chain")])) {
    stop("trade_cells must have one row per (exporter_iso, importer_iso, tech, supply_chain).")
  }
  out
}

# One edge component (producer in reporter_iso, consumer in partner_iso, tech, supply_chain, value),
# member-level, aggregated to node pairs by the edge rule above. Returns reporter_iso / partner_iso
# as node codes, tech, supply_chain, value, weight_basis ("member" for two single-member nodes,
# else "trade", "stage" or "equal") and n_member_edges; attr "intra_node_dropped" counts the member
# edges dropped inside a node.
allied_network_aggregate_edge_component <- function(comp, network_members, member_weights, node_trade_tbl = NULL) {
  node_of <- stats::setNames(network_members$network_node, network_members$iso3c)
  sizes <- table(network_members$network_node)
  multi_nodes <- names(sizes)[sizes > 1]
  comp <- comp %>%
    dplyr::mutate(
      .p_node = unname(node_of[.data$reporter_iso]),
      .c_node = unname(node_of[.data$partner_iso])
    )
  intra <- comp$.p_node == comp$.c_node
  n_intra <- sum(intra)
  comp <- comp[!intra, , drop = FALSE]
  is_multi <- comp$.p_node %in% multi_nodes | comp$.c_node %in% multi_nodes
  passthrough <- comp[!is_multi, , drop = FALSE] %>%
    dplyr::mutate(reporter_iso = .data$.p_node, partner_iso = .data$.c_node, weight_basis = "member", n_member_edges = 1L) %>%
    dplyr::select("reporter_iso", "partner_iso", "tech", "supply_chain", "value", "weight_basis", "n_member_edges")
  agg <- comp[is_multi, , drop = FALSE]
  if (nrow(agg)) {
    mw <- member_weights %>% dplyr::select("iso3c", "tech", "supply_chain", "member_weight")
    agg <- agg %>%
      dplyr::filter(is.finite(.data$value)) %>%
      # stages without member weights are not solved; their bundled edges are not needed
      dplyr::inner_join(mw %>% dplyr::rename(reporter_iso = "iso3c", .w_p = "member_weight"),
                        by = c("reporter_iso", "tech", "supply_chain"), relationship = "many-to-one") %>%
      dplyr::inner_join(mw %>% dplyr::rename(partner_iso = "iso3c", .w_c = "member_weight"),
                        by = c("partner_iso", "tech", "supply_chain"), relationship = "many-to-one")
    if (!is.null(node_trade_tbl)) {
      require_columns(node_trade_tbl, c("exporter_iso", "importer_iso", "tech", "supply_chain", "value"), label = "node_trade_tbl")
      trade <- node_trade_tbl %>%
        dplyr::transmute(reporter_iso = .data$exporter_iso, partner_iso = .data$importer_iso,
                         tech = .data$tech, supply_chain = .data$supply_chain, .trade = as.numeric(.data$value))
      agg <- agg %>% dplyr::left_join(trade, by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), relationship = "many-to-one")
    } else {
      agg$.trade <- NA_real_
    }
    agg <- agg %>%
      dplyr::mutate(.trade = pmax(0, dplyr::coalesce(.data$.trade, 0))) %>%
      dplyr::group_by(.data$.p_node, .data$.c_node, .data$tech, .data$supply_chain) %>%
      dplyr::summarise(
        weight_basis = if (sum(.data$.trade) > 0) "trade" else if (sum(.data$.w_p * .data$.w_c) > 0) "stage" else "equal",
        value = switch(
          weight_basis,
          trade = allied_network_wmean(.data$value, .data$.trade),
          stage = allied_network_wmean(.data$value, .data$.w_p * .data$.w_c),
          equal = mean(.data$value)
        ),
        n_member_edges = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::rename(reporter_iso = ".p_node", partner_iso = ".c_node") %>%
      dplyr::select("reporter_iso", "partner_iso", "tech", "supply_chain", "value", "weight_basis", "n_member_edges")
  } else {
    agg <- passthrough[0, ]
  }
  out <- dplyr::bind_rows(passthrough, agg)
  attr(out, "intra_node_dropped") <- n_intra
  out
}

# Each node's production share split back to its members by stage weight: one row per member and
# solved stage with tech, supply_chain, iso3c (member), country, tier (the member's), network_node,
# selected (the node's), node_production_share, member_weight, weight_basis,
# production_share_derived (= node share x member weight; sums to the node share) and
# share_is_derived (TRUE for members of a bundled node: the split is not designed).
allied_network_disaggregate_shares <- function(specialization_tbl, network_members, member_weights, country_info = NULL) {
  if (is.null(specialization_tbl) || !nrow(specialization_tbl)) return(NULL)
  sizes <- table(network_members$network_node)
  node_rows <- specialization_tbl %>%
    dplyr::select("tech", "supply_chain", network_node = "iso3c", "selected", node_production_share = "production_share")
  member_names <- if (!is.null(country_info)) {
    as.character(country_info$country[match(network_members$iso3c, country_info$iso3c)])
  } else {
    rep(NA_character_, nrow(network_members))
  }
  members <- network_members %>%
    dplyr::transmute(.data$iso3c, country = member_names, .data$tier, .data$network_node)
  node_rows %>%
    dplyr::inner_join(members, by = "network_node", relationship = "many-to-many") %>%
    dplyr::left_join(
      member_weights %>% dplyr::select("iso3c", "tech", "supply_chain", "member_weight", "weight_basis"),
      by = c("iso3c", "tech", "supply_chain"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      production_share_derived = .data$node_production_share * .data$member_weight,
      share_is_derived = as.integer(sizes[.data$network_node]) > 1
    ) %>%
    dplyr::select(
      "tech", "supply_chain", "iso3c", "country", "tier", "network_node", "selected",
      "node_production_share", "member_weight", "weight_basis", "production_share_derived", "share_is_derived"
    ) %>%
    dplyr::arrange(.data$tech, .data$supply_chain, dplyr::desc(.data$production_share_derived), .data$iso3c)
}

# Observed export concentration of the network's nodes (the lambda calibration target): per stage,
# the Herfindahl of the nodes' basket exports, shares taken within the nodes. A bundled node exports
# only to importers outside it (the EU's extra-EU exports); every other node exports to every other
# economy. trade_cells: the reconciled matrix (exporter_iso, importer_iso, tech, supply_chain,
# value_last or value; NA = unobserved, counted as 0). Stages with no positive node export (no HS6
# basket) are left out. Returns tech, supply_chain, n_nodes_exporting, total_exports, hhi,
# effective_exporters, top_node, top_share.
allied_network_observed_export_hhi <- function(trade_cells, network_members, stages = NULL) {
  require_columns(network_members, c("iso3c", "network_node"), label = "network_members")
  value_col <- intersect(c("value_last", "value"), names(trade_cells))[1]
  if (is.na(value_col)) stop("trade_cells needs a value_last (or value) column.")
  node_of <- stats::setNames(as.character(network_members$network_node), as.character(network_members$iso3c))
  x <- tibble::tibble(
    exporter_iso = as.character(trade_cells$exporter_iso),
    importer_iso = as.character(trade_cells$importer_iso),
    tech = as.character(trade_cells$tech),
    supply_chain = as.character(trade_cells$supply_chain),
    value = dplyr::coalesce(suppressWarnings(as.numeric(trade_cells[[value_col]])), 0)
  ) %>%
    dplyr::filter(.data$exporter_iso %in% names(node_of), .data$exporter_iso != .data$importer_iso)
  if (!is.null(stages)) x <- x %>% dplyr::semi_join(tibble::as_tibble(stages)[c("tech", "supply_chain")], by = c("tech", "supply_chain"))
  x %>%
    dplyr::mutate(
      node = unname(node_of[.data$exporter_iso]),
      importer_node = unname(node_of[.data$importer_iso])
    ) %>%
    dplyr::filter(is.na(.data$importer_node) | .data$importer_node != .data$node) %>%
    dplyr::group_by(.data$tech, .data$supply_chain, .data$node) %>%
    dplyr::summarise(exports = sum(.data$value), .groups = "drop") %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::filter(sum(.data$exports) > 0) %>%
    dplyr::mutate(share = .data$exports / sum(.data$exports)) %>%
    dplyr::summarise(
      n_nodes_exporting = sum(.data$exports > 0),
      total_exports = sum(.data$exports),
      hhi = sum(.data$share^2),
      top_node = .data$node[which.max(.data$share)],
      top_share = max(.data$share),
      .groups = "drop"
    ) %>%
    dplyr::mutate(effective_exporters = 1 / .data$hhi) %>%
    dplyr::arrange(.data$tech, .data$supply_chain)
}

# ---- Diversification term (HHI penalty) ----------------------------------------

# Breakpoints for the tangent approximation of x^2 on [0, max_share]: a grid of `step` plus
# max_share itself. The MILP maximises, so with diversity_lambda > 0 each auxiliary t_p settles on the
# upper envelope of the tangents t >= 2 a_k x - a_k^2. The envelope touches x^2 at every breakpoint
# and has its kinks where neighbouring tangents meet, at the midpoints, where it is below x^2 by
# (step / 2)^2; so the error is at most step^2 / 4 per producer. An interior optimal share sits on a
# kink (0.005, 0.015, ...) unless another constraint binds, so the bound is usually attained. The
# default step 0.01 (41 cuts per producer at max_share 0.40) keeps the error at 2.5e-5 per producer,
# at most 7.75e-4 of HHI for a 31-member stage (under 0.8% of an HHI of 0.10), and it moves a share
# by about half a step from the exact quadratic optimum; a finer step adds rows to every stage MILP.
allied_network_check_lambda <- function(diversity_lambda) {
  if (!is.numeric(diversity_lambda) || length(diversity_lambda) != 1 || !is.finite(diversity_lambda) || diversity_lambda < 0) {
    stop("diversity_lambda must be one finite number >= 0.")
  }
  as.numeric(diversity_lambda)
}

allied_network_hhi_breakpoints <- function(max_share, step = 0.01) {
  if (!is.finite(step) || step <= 0) stop("diversity_breakpoint_step must be a positive number.")
  hi <- max(0, min(1, max_share))
  bp <- seq(0, hi, by = step)
  unique(c(bp[bp < hi - 1e-12], hi))
}

# Shares of a fixed producer set that maximise sum v_p x_p - lambda * sum x_p^2 subject to
# sum x_p = 1 and lo <= x_p <= hi (water-filling): x_p = clip((v_p - mu) / (2 lambda), lo, hi), with
# the level mu found by bisection. NULL if the bounds cannot sum to one.
allied_network_waterfill <- function(v, lambda, lo, hi) {
  v <- as.numeric(v)
  k <- length(v)
  if (!k || !is.finite(lambda) || lambda <= 0 || k * lo > 1 + 1e-12 || k * hi < 1 - 1e-12) return(NULL)
  share_at <- function(mu) pmin(hi, pmax(lo, (v - mu) / (2 * lambda)))
  # The sum is non-increasing in mu: k * hi >= 1 at lo_mu and k * lo <= 1 at hi_mu
  lo_mu <- min(v) - 2 * lambda * hi
  hi_mu <- max(v) - 2 * lambda * lo
  for (iter in seq_len(100)) {
    mid <- (lo_mu + hi_mu) / 2
    if (sum(share_at(mid)) > 1) lo_mu <- mid else hi_mu <- mid
  }
  x <- share_at((lo_mu + hi_mu) / 2)
  x / sum(x)
}

# Greedy counterpart of the diversified MILP objective. Candidates come in the greedy's preference
# order with value v_p per unit of share and a fixed bonus per selected producer (w_dev * dev). For
# each k >= n_min the first k candidates get water-filled shares in [min_share, max_share], and the k
# with the highest sum v x + bonus - lambda * sum x^2 wins. With a common share range and no bonus the
# best set of k producers is the k highest values, so for candidates in value order this is the exact
# optimum of the node part; flows and edges enter only through v.
allied_network_diversified_shares <- function(v, bonus = 0, lambda, min_share, max_share, n_min) {
  v <- as.numeric(v)
  bonus <- rep_len(as.numeric(bonus), length(v))
  best <- NULL
  for (k in seq(max(1L, n_min), length(v))) {
    x <- allied_network_waterfill(v[seq_len(k)], lambda, lo = min_share, hi = max_share)
    if (is.null(x)) next
    val <- sum(v[seq_len(k)] * x) + sum(bonus[seq_len(k)]) - lambda * sum(x^2)
    if (is.null(best) || val > best$value + 1e-12) best <- list(k = k, shares = x, value = val)
  }
  best
}

# Objective of a solved stage split into its terms, from the realized shares and flows (a missing
# producer score or development value counts as 0, as in the solvers). diversity_penalty is the exact
# lambda * HHI; the MILP's own objective uses the tangent approximation instead.
allied_network_objective_terms <- function(specialization, flows, w_node, w_edge, w_dev, diversity_lambda = 0) {
  sel <- specialization$selected %in% TRUE
  x <- dplyr::coalesce(as.numeric(specialization$production_share), 0)
  a <- dplyr::coalesce(as.numeric(specialization$producer_score), 0)
  dev <- dplyr::coalesce(as.numeric(specialization$dev_potential), 0)
  edge <- if (is.null(flows) || !nrow(flows)) 0 else sum(flows$flow_share * dplyr::coalesce(flows$edge_weight, 0))
  list(
    node = w_node * sum(a * x),
    edge = w_edge * edge,
    dev = w_dev * sum(dev[sel]),
    diversity_penalty = diversity_lambda * sum(x^2)
  )
}

# ---- Input collapsing (one row per key) ---------------------------------------

# Mean of the distinct non-missing values (NA if none): exact copies collapse to their value and
# conflicting duplicates (split country records, upstream join fan-out) count once each.
allied_network_mean_distinct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- unique(x[is.finite(x)])
  if (!length(x)) return(NA_real_)
  mean(x)
}

# Collapse a long table to one row per `keys` with the mean of distinct values. Rows with a missing
# key are dropped, and with a Year column only each key's latest year is kept, so a multi-year table
# cannot fan out a join. n_source_rows / n_distinct_values record what was collapsed.
allied_network_collapse_keys <- function(tbl, keys, value_col = "value") {
  require_columns(tbl, c(keys, value_col), label = "allied_network_collapse_keys input")
  out <- tibble::as_tibble(tbl) %>%
    dplyr::mutate(.value = suppressWarnings(as.numeric(.data[[value_col]]))) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(keys), ~ !is.na(.x)))
  if ("Year" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(.year = suppressWarnings(as.numeric(.data$Year))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::mutate(.year_max = if (all(is.na(.year))) NA_real_ else max(.year, na.rm = TRUE)) %>%
      dplyr::filter(is.na(.year_max) | (!is.na(.year) & .year == .year_max)) %>%
      dplyr::ungroup()
  }
  out %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      n_source_rows = dplyr::n(),
      n_distinct_values = dplyr::n_distinct(.value[is.finite(.value)]),
      value = allied_network_mean_distinct(.value),
      .groups = "drop"
    )
}

# Collapse a Country-keyed index (EO, ES or Policy) to one row per coalition (iso3c, tech,
# supply_chain). Two spellings of one country map to one ISO3; keep the record with a value, then
# the one spelled as in country_info (the name the index builders join on), then the first. Values
# are not averaged: the variant record comes from an upstream name-mapping error, not a second
# measurement.
allied_network_collapse_country_index <- function(tbl, value_col, country_info, iso3c_network = NULL) {
  require_columns(tbl, c("Country", "tech", "supply_chain", value_col), label = "country index")
  canonical <- country_info %>%
    dplyr::transmute(iso3c = as.character(iso3c), canonical = as.character(country)) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)

  out <- tibble::as_tibble(tbl) %>%
    dplyr::transmute(
      Country_raw = as.character(Country),
      Country = partnership_strength_standardize_countries(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      raw = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    dplyr::mutate(iso3c = partnership_strength_country_to_iso(Country, country_info)) %>%
    dplyr::filter(!is.na(iso3c), !is.na(tech), !is.na(supply_chain))
  if (!is.null(iso3c_network)) out <- out %>% dplyr::filter(iso3c %in% iso3c_network)

  out %>%
    dplyr::left_join(canonical, by = "iso3c") %>%
    dplyr::mutate(is_canonical = dplyr::coalesce(Country_raw == canonical, FALSE)) %>%
    dplyr::group_by(iso3c, tech, supply_chain) %>%
    dplyr::arrange(dplyr::desc(is.finite(raw)), dplyr::desc(is_canonical), .by_group = TRUE) %>%
    dplyr::summarise(
      n_source_rows = dplyr::n(),
      n_distinct_values = dplyr::n_distinct(raw[is.finite(raw)]),
      country = dplyr::first(Country),
      value = dplyr::first(raw),
      .groups = "drop"
    )
}

allied_network_input_check_row <- function(input, collapsed) {
  tibble::tibble(
    input = input,
    rows_in = sum(collapsed$n_source_rows),
    keys_out = nrow(collapsed),
    dup_keys = sum(collapsed$n_source_rows > 1),
    multivalued_keys = sum(collapsed$n_distinct_values > 1)
  )
}

allied_network_compute_portfolio_caps <- function(country_size_tbl,
                                                  iso3c_network,
                                                  min_cap = 2,
                                                  max_cap = 10,
                                                  transform = c("log", "linear"),
                                                  clamp = TRUE) {
  transform <- match.arg(transform)
  require_columns(country_size_tbl, c("iso3c", "gdp_usd"), label = "country_size_tbl")

  tbl <- tibble::tibble(iso3c = as.character(iso3c_network)) %>%
    dplyr::left_join(
      country_size_tbl %>%
        dplyr::transmute(
          iso3c = as.character(iso3c),
          gdp_usd = suppressWarnings(as.numeric(gdp_usd)),
          gdp_imputed = if ("gdp_imputed" %in% names(country_size_tbl)) dplyr::coalesce(as.logical(.data$gdp_imputed), FALSE) else FALSE
        ) %>%
        dplyr::group_by(iso3c) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup(),
      by = "iso3c"
    )

  med <- stats::median(tbl$gdp_usd[is.finite(tbl$gdp_usd)], na.rm = TRUE)
  if (!is.finite(med)) med <- 1
  tbl <- tbl %>%
    dplyr::mutate(
      gdp_usd = dplyr::if_else(is.finite(gdp_usd), gdp_usd, med),
      gdp_imputed = dplyr::coalesce(gdp_imputed, FALSE)
    )

  x <- if (transform == "log") log1p(tbl$gdp_usd) else tbl$gdp_usd
  rng <- range(x, na.rm = TRUE)
  denom <- rng[[2]] - rng[[1]]
  norm <- if (!is.finite(denom) || denom <= 0) rep(0, length(x)) else (x - rng[[1]]) / denom

  caps <- round(min_cap + norm * (max_cap - min_cap))
  if (isTRUE(clamp)) caps <- pmax(min_cap, pmin(max_cap, caps))
  if (length(caps) > 0) caps[which.max(tbl$gdp_usd)] <- max_cap
  stats::setNames(as.integer(caps), tbl$iso3c)
}

# Coalition GDP (current US$) from a WDI extract already read into memory (country, iso3c, year and
# NY.GDP.MKTP.CD or another numeric GDP column). The latest year is taken among rows with a finite
# value: WDI publishes the newest year with gaps (ARE 2025 is NA), and taking the newest row first
# sent those members to the median imputation. Members still without GDP get the coalition median
# and gdp_imputed = TRUE.
allied_network_country_size_tbl <- function(gdp_raw, country_info, iso3c_network) {
  lower_names <- tolower(names(gdp_raw))
  year_col <- names(gdp_raw)[match(TRUE, lower_names %in% c("year"), nomatch = 0)]
  country_col <- names(gdp_raw)[match(TRUE, lower_names %in% c("country", "country_name"), nomatch = 0)]
  iso_col <- names(gdp_raw)[match(TRUE, lower_names %in% c("iso3c", "iso3", "iso_code"), nomatch = 0)]

  gdp_col <- if ("NY.GDP.MKTP.CD" %in% names(gdp_raw)) {
    "NY.GDP.MKTP.CD"
  } else {
    candidate <- names(gdp_raw)[vapply(gdp_raw, is.numeric, logical(1))]
    candidate <- setdiff(candidate, c(year_col, "iso2c", "country", "Country", "iso3c"))
    if (!length(candidate)) stop("No usable GDP numeric column found in the GDP table")
    candidate[[1]]
  }
  if (!length(country_col) || !nzchar(country_col)) stop("Could not identify a country column in the GDP table")

  gdp_tbl <- gdp_raw %>%
    dplyr::transmute(
      country = standardize_country_names(.data[[country_col]]),
      iso3c = if (length(iso_col) && nzchar(iso_col)) toupper(as.character(.data[[iso_col]])) else NA_character_,
      year = if (length(year_col) && nzchar(year_col)) suppressWarnings(as.numeric(.data[[year_col]])) else NA_real_,
      gdp_usd = suppressWarnings(as.numeric(.data[[gdp_col]]))
    ) %>%
    dplyr::left_join(
      country_info %>%
        dplyr::transmute(country = standardize_country_names(country), iso3c_ref = toupper(as.character(iso3c))) %>%
        dplyr::distinct(country, .keep_all = TRUE),
      by = "country"
    ) %>%
    dplyr::mutate(iso3c = dplyr::coalesce(iso3c, iso3c_ref)) %>%
    dplyr::select(-iso3c_ref) %>%
    dplyr::filter(iso3c %in% iso3c_network, is.finite(gdp_usd)) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::arrange(dplyr::desc(year), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  med <- stats::median(gdp_tbl$gdp_usd, na.rm = TRUE)
  if (!is.finite(med)) med <- 1

  tibble::tibble(iso3c = as.character(iso3c_network)) %>%
    dplyr::left_join(gdp_tbl, by = "iso3c") %>%
    dplyr::left_join(
      country_info %>%
        dplyr::transmute(iso3c = toupper(as.character(iso3c)), country_ref = country) %>%
        dplyr::distinct(iso3c, .keep_all = TRUE),
      by = "iso3c"
    ) %>%
    dplyr::mutate(
      country = dplyr::coalesce(country, country_ref),
      year = suppressWarnings(as.numeric(year)),
      gdp_imputed = !is.finite(gdp_usd),
      gdp_usd = dplyr::if_else(is.finite(gdp_usd), gdp_usd, med)
    ) %>%
    dplyr::select(iso3c, country, year, gdp_usd, gdp_imputed)
}

allied_network_get_stage_target <- function(stage_targets_tbl, tech, supply_chain) {
  if (is.null(stage_targets_tbl)) return(NA_real_)
  require_columns(stage_targets_tbl, c("tech", "supply_chain", "target_exports_usd"), label = "stage_targets_tbl")

  target_tbl <- stage_targets_tbl %>%
    dplyr::filter(tech == !!tech, supply_chain == !!supply_chain) %>%
    dplyr::mutate(
      Year = suppressWarnings(as.numeric(.data$Year)),
      target_exports_usd = suppressWarnings(as.numeric(target_exports_usd))
    )

  if (nrow(target_tbl) == 0) return(NA_real_)
  if ("Year" %in% names(target_tbl) && any(is.finite(target_tbl$Year))) {
    yr <- max(target_tbl$Year[is.finite(target_tbl$Year)], na.rm = TRUE)
    target_tbl <- target_tbl %>% dplyr::filter(.data$Year == yr)
  }

  out <- target_tbl$target_exports_usd[[1]]
  if (!is.finite(out)) return(NA_real_)
  out
}

allied_network_get_scale_caps <- function(scale_caps_tbl, tech, supply_chain, iso_vec) {
  iso_vec <- as.character(iso_vec)
  if (is.null(scale_caps_tbl)) {
    return(list(
      cap0 = stats::setNames(rep(0, length(iso_vec)), iso_vec),
      capMax = stats::setNames(rep(Inf, length(iso_vec)), iso_vec),
      build_max = stats::setNames(rep(Inf, length(iso_vec)), iso_vec)
    ))
  }

  require_columns(
    scale_caps_tbl,
    c("iso3c", "tech", "supply_chain", "cap0_exports_usd", "capMax_exports_usd", "build_max_usd"),
    label = "scale_caps_tbl"
  )

  caps <- scale_caps_tbl %>%
    dplyr::filter(tech == !!tech, supply_chain == !!supply_chain, iso3c %in% iso_vec) %>%
    dplyr::mutate(
      Year = suppressWarnings(as.numeric(.data$Year)),
      cap0_exports_usd = suppressWarnings(as.numeric(cap0_exports_usd)),
      capMax_exports_usd = suppressWarnings(as.numeric(capMax_exports_usd)),
      build_max_usd = suppressWarnings(as.numeric(build_max_usd))
    )

  if ("Year" %in% names(caps) && any(is.finite(caps$Year))) {
    caps <- caps %>%
      dplyr::group_by(iso3c) %>%
      dplyr::filter(.data$Year == max(.data$Year, na.rm = TRUE)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {
    caps <- caps %>% dplyr::group_by(iso3c) %>% dplyr::slice(1) %>% dplyr::ungroup()
  }

  caps <- tibble::tibble(iso3c = iso_vec) %>%
    dplyr::left_join(caps, by = "iso3c") %>%
    dplyr::mutate(
      cap0_exports_usd = dplyr::coalesce(cap0_exports_usd, 0),
      capMax_exports_usd = dplyr::coalesce(capMax_exports_usd, 0),
      build_max_usd = dplyr::coalesce(build_max_usd, 0)
    )

  list(
    cap0 = stats::setNames(caps$cap0_exports_usd, caps$iso3c),
    capMax = stats::setNames(caps$capMax_exports_usd, caps$iso3c),
    build_max = stats::setNames(caps$build_max_usd, caps$iso3c)
  )
}

# ---- Extract edges from partner dyad theme tables ----------------------------

# Expects the standardized dyad tables written by scripts/15_build_partner_themes.R:
# - partner_friendshore_tbl (category == "friendshore", variable == "Friendshore Index"); reporter =
#   importer, i.e. FS rates the partner as a supplier to the reporter
# - partner_opportunity_tbl (category == "opportunity", variable == "Opportunity Index"); reporter =
#   exporter, i.e. OP rates the reporter selling to the partner
# Output: one row per (producer, consumer, tech, supply_chain), producer in `reporter_iso` and consumer
# in `partner_iso`; friendshore_index is FS(reporter = consumer, partner = producer). Each input is
# collapsed to one row per dyad first (the friendshore table carries exact duplicate rows). A dyad
# present in only one table gets 0 for the missing component, i.e. no observed relationship.
# friendshore_orientation = "reporter_is_producer" reproduces the legacy (transposed) reading and is
# kept only for sensitivity runs. The collapse counts are attached as attr "input_checks".
# With network_members bundling members into nodes (and their member_weights from
# allied_network_member_weights(), plus the optional member-level basket trade node_trade_tbl from
# allied_network_member_trade()), each component is aggregated to node pairs before the 0 fill
# (allied_network_aggregate_edge_component()); the edges are then keyed by node codes, and attr
# "edge_aggregation" counts the node edges by component, stage and weight basis (with the member
# edges dropped inside a node).
allied_network_extract_edges <- function(partner_friendshore_tbl,
                                         partner_opportunity_tbl,
                                         iso3c_network = allied_network_default_iso3c(),
                                         edge_weights = list(friendshore = 0.5, opportunity = 0.5),
                                         keep_components = TRUE,
                                         friendshore_orientation = c("reporter_is_consumer", "reporter_is_producer"),
                                         network_members = NULL,
                                         member_weights = NULL,
                                         node_trade_tbl = NULL) {
  friendshore_orientation <- match.arg(friendshore_orientation)
  require_columns(
    partner_friendshore_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable", "data_type", "value"),
    label = "partner_friendshore_tbl"
  )
  require_columns(
    partner_opportunity_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable", "data_type", "value"),
    label = "partner_opportunity_tbl"
  )
  
  w <- c(friendshore = edge_weights$friendshore, opportunity = edge_weights$opportunity)
  w <- allied_network_normalize_weights(w)
  if (any(is.na(w))) {
    stop("edge_weights must be positive; got: ", paste(names(edge_weights), unlist(edge_weights), collapse = ", "))
  }
  
  keys <- c("reporter_iso", "partner_iso", "tech", "supply_chain")
  # Coalition dyads of one index, re-keyed as (producer, consumer) and collapsed to one row per dyad.
  pick_edges <- function(tbl, category_name, variable_name, producer_col, consumer_col) {
    tbl %>%
      dplyr::filter(
        category == category_name,
        variable == variable_name,
        data_type == "index",
        reporter_iso %in% iso3c_network,
        partner_iso %in% iso3c_network
      ) %>%
      dplyr::mutate(
        .producer = as.character(.data[[producer_col]]),
        .consumer = as.character(.data[[consumer_col]]),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain)
      ) %>%
      dplyr::select(reporter_iso = .producer, partner_iso = .consumer, tech, supply_chain, value, dplyr::any_of("Year")) %>%
      allied_network_collapse_keys(keys = keys)
  }

  fs_producer_col <- if (friendshore_orientation == "reporter_is_consumer") "partner_iso" else "reporter_iso"
  fs_consumer_col <- setdiff(c("reporter_iso", "partner_iso"), fs_producer_col)
  fs <- pick_edges(partner_friendshore_tbl, "friendshore", "Friendshore Index", fs_producer_col, fs_consumer_col)
  op <- pick_edges(partner_opportunity_tbl, "opportunity", "Opportunity Index", "reporter_iso", "partner_iso")
  input_checks <- dplyr::bind_rows(
    allied_network_input_check_row("partner_friendshore_tbl", fs),
    allied_network_input_check_row("partner_opportunity_tbl", op)
  )

  edge_aggregation <- NULL
  network_members <- allied_network_check_members(network_members, unique(as.character(iso3c_network)))
  if (allied_network_aggregates(network_members)) {
    if (is.null(member_weights)) stop("allied_network_extract_edges: member_weights are required to aggregate edges to nodes.")
    fs <- allied_network_aggregate_edge_component(fs, network_members, member_weights, node_trade_tbl)
    op <- allied_network_aggregate_edge_component(op, network_members, member_weights, node_trade_tbl)
    count_basis <- function(x, component) {
      x %>%
        dplyr::count(tech, supply_chain, weight_basis, name = "n_edges") %>%
        dplyr::mutate(component = component, intra_node_member_edges_dropped = attr(x, "intra_node_dropped"))
    }
    edge_aggregation <- dplyr::bind_rows(count_basis(fs, "friendshore"), count_basis(op, "opportunity")) %>%
      dplyr::relocate(component)
  }

  edges <- dplyr::full_join(
    fs %>% dplyr::select(dplyr::all_of(keys), friendshore_index = value),
    op %>% dplyr::select(dplyr::all_of(keys), opportunity_index = value),
    by = keys,
    relationship = "one-to-one"
  ) %>%
    dplyr::mutate(
      friendshore_index  = dplyr::coalesce(friendshore_index, 0),
      opportunity_index  = dplyr::coalesce(opportunity_index, 0),
      edge_weight = w[["friendshore"]] * friendshore_index + w[["opportunity"]] * opportunity_index
    )
  if (anyDuplicated(edges[keys])) stop("allied_network_extract_edges: edges are not unique per (producer, consumer, tech, supply_chain).")

  if (!isTRUE(keep_components)) {
    edges <- edges %>%
      dplyr::select(reporter_iso, partner_iso, tech, supply_chain, edge_weight)
  }

  attr(edges, "input_checks") <- input_checks
  attr(edges, "edge_aggregation") <- edge_aggregation
  edges
}

allied_network_complete_edges_stage <- function(edges_stage,
                                                iso3c_vec,
                                                allow_self = TRUE,
                                                fill_missing = 0,
                                                self_weight = NULL) {
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  # Duplicate pairs would be summed by the greedy connectivity and overwritten by the MILP matrix.
  if (anyDuplicated(edges_stage[c("reporter_iso", "partner_iso")])) {
    stop("allied_network_complete_edges_stage: duplicate (reporter_iso, partner_iso) edges; collapse edges first.")
  }

  base <- tidyr::crossing(
    reporter_iso = as.character(iso3c_vec),
    partner_iso  = as.character(iso3c_vec)
  )
  
  out <- base %>%
    dplyr::left_join(
      edges_stage %>% dplyr::select(reporter_iso, partner_iso, dplyr::any_of(c("edge_weight", "friendshore_index", "opportunity_index"))),
      by = c("reporter_iso", "partner_iso")
    )
  
  if (!isTRUE(allow_self)) {
    out <- out %>% dplyr::filter(reporter_iso != partner_iso)
  }
  
  if (is.null(self_weight)) {
    med <- suppressWarnings(stats::median(out$edge_weight, na.rm = TRUE))
    if (!is.finite(med)) med <- 0.5
    self_weight <- med
  }
  
  out <- out %>%
    dplyr::mutate(
      edge_weight = dplyr::case_when(
        !is.na(edge_weight) ~ edge_weight,
        reporter_iso == partner_iso ~ self_weight,
        TRUE ~ fill_missing
      )
    )

  out
}

# Demand-weighted outbound edge quality of each potential producer, c_i = sum_j W_ij d_j. With one
# edge per pair and demand summing to one this is a weighted mean of edge weights, so it cannot exceed
# the largest weight (1 for index-valued edges); a larger value means duplicated edges or nodes.
allied_network_connectivity_out <- function(edges_full, nodes_stage) {
  d_tbl <- nodes_stage %>% dplyr::select(iso3c, demand_weight)
  conn <- edges_full %>%
    dplyr::left_join(d_tbl, by = c("partner_iso" = "iso3c"), relationship = "many-to-one") %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::summarize(connectivity_out = sum(edge_weight * demand_weight, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(iso3c = reporter_iso)
  max_w <- max(c(0, edges_full$edge_weight), na.rm = TRUE)
  if (any(conn$connectivity_out > max_w * sum(d_tbl$demand_weight, na.rm = TRUE) + 1e-9)) {
    stop("allied_network_connectivity_out: connectivity exceeds the largest edge weight; edges or demand are duplicated.")
  }
  conn
}

# ---- Prepare coalition nodes (capability + demand weights) -------------------

# Inputs:
# - economic_opportunity_index: must contain Country, tech, supply_chain, Economic_Opportunity_Index
# - energy_security_index: must contain Country, tech, supply_chain, Energy_Security_Index
# - policy_index: must contain Country, tech, supply_chain, value  (your policy_index table)
# - partner_development_country_tbl (optional): Development Potential Index, either country-level
#   (Country/country column) or dyadic (reporter x partner, as scripts/15 writes it). Development
#   potential is a partner-level quantity, so a dyadic table is keyed by partner and collapsed.
# - country_info: must contain iso3c, country (WDI country info standardized)
# - policy_missing: "coalition_median" gives a coalition member without a policy value the
#   within-(tech, stage) coalition median and sets policy_imputed; "none" leaves it missing.
# - missing_components: "renormalize" scores a node on the components it has (weights renormalized)
#   and, for mixed demand, uses the size term alone when need (ES) is missing; "require_all" is the
#   legacy rule (any missing component makes producer_score NA and mixed demand 0).
# - node_policy: TRUE (default, the legacy score) puts the policy index in the producer score with
#   weight node_weights$policy. FALSE leaves it out: the policy weight is set to 0 (so EO and ES carry
#   node_weights$eo and $resilience, renormalized), policy_index is not read (it may be NULL),
#   policy_raw / pol_idx are NA and no member is policy_imputed; policy_missing has no effect.
# - network_members, node_member_weights, node_labels (optional): members that share a network_node
#   are aggregated into one node before scoring ("Network nodes" above); policy imputation, the
#   S-curves and demand are then computed over nodes.
#
# Output: one row per coalition (iso3c, tech, supply_chain) with producer_score, demand_weight,
# dev_potential and the flags above (iso3c is the node code when members are aggregated). Collapse
# counts are attached as attr "input_checks". The work is split in two steps that
# allied_network_design() calls separately (so that the stage filter and the node aggregation sit
# between them): allied_network_node_inputs() (member-level raw values) and
# allied_network_score_nodes() (imputation, S-curves, producer score and demand).
allied_network_prepare_nodes <- function(economic_opportunity_index,
                                         energy_security_index,
                                         policy_index,
                                         country_info,
                                         iso3c_network = allied_network_default_iso3c(),
                                         partner_development_country_tbl = NULL,
                                         node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
                                         demand_from = NULL,
                                         country_size_tbl = NULL,
                                         demand_mode = c("need", "size", "mixed", "equal"),
                                         demand_weights = list(need = 0.5, size = 0.5),
                                         size_transform = c("log_scurve", "scurve", "rank"),
                                         policy_missing = c("coalition_median", "none"),
                                         missing_components = c("renormalize", "require_all"),
                                         node_policy = TRUE,
                                         network_members = NULL,
                                         node_member_weights = NULL,
                                         node_labels = NULL) {
  demand_mode <- match.arg(demand_mode)
  size_transform <- match.arg(size_transform)
  policy_missing <- match.arg(policy_missing)
  missing_components <- match.arg(missing_components)
  node_policy <- allied_network_check_node_policy(node_policy)
  if (!is.null(demand_from)) {
    demand_from <- match.arg(demand_from, c("need", "equal"))
    demand_mode <- demand_from
  }
  iso3c_network <- unique(as.character(iso3c_network))

  raw <- allied_network_node_inputs(
    economic_opportunity_index = economic_opportunity_index,
    energy_security_index = energy_security_index,
    policy_index = policy_index,
    country_info = country_info,
    iso3c_network = iso3c_network,
    partner_development_country_tbl = partner_development_country_tbl,
    country_size_tbl = country_size_tbl,
    node_policy = node_policy
  )
  input_checks <- attr(raw, "input_checks")
  network_members <- allied_network_check_members(network_members, iso3c_network)
  if (allied_network_aggregates(network_members)) {
    member_weights <- allied_network_member_weights(network_members, node_member_weights, dplyr::distinct(raw, tech, supply_chain))
    node_size_tbl <- if (is.null(country_size_tbl)) NULL else allied_network_node_size_tbl(country_size_tbl, network_members)
    raw <- allied_network_aggregate_node_inputs(raw, network_members, member_weights, node_labels, country_info, node_size_tbl)
  }
  nodes <- allied_network_score_nodes(
    raw,
    node_weights = node_weights,
    demand_mode = demand_mode,
    demand_weights = demand_weights,
    size_transform = size_transform,
    policy_missing = policy_missing,
    missing_components = missing_components,
    node_policy = node_policy
  )
  attr(nodes, "input_checks") <- input_checks
  nodes
}

# Member-level raw node inputs: one row per coalition (iso3c, tech, supply_chain) present in the EO
# index, with country, eo_raw, es_raw, policy_raw (NA when node_policy = FALSE; not imputed here),
# dev_potential (NA where the development table has no value), gdp_usd and gdp_imputed. Collapse
# counts are attached as attr "input_checks".
allied_network_node_inputs <- function(economic_opportunity_index,
                                       energy_security_index,
                                       policy_index,
                                       country_info,
                                       iso3c_network = allied_network_default_iso3c(),
                                       partner_development_country_tbl = NULL,
                                       country_size_tbl = NULL,
                                       node_policy = TRUE) {
  node_policy <- allied_network_check_node_policy(node_policy)
  require_columns(
    economic_opportunity_index,
    c("Country", "tech", "supply_chain", "Economic_Opportunity_Index"),
    label = "economic_opportunity_index"
  )
  require_columns(
    energy_security_index,
    c("Country", "tech", "supply_chain", "Energy_Security_Index"),
    label = "energy_security_index"
  )
  if (node_policy) {
    require_columns(
      policy_index,
      c("Country", "tech", "supply_chain", "value"),
      label = "policy_index"
    )
  }
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  normalize_country_col <- function(tbl) {
    if (!inherits(tbl, "data.frame") || "Country" %in% names(tbl)) {
      return(tbl)
    }

    if ("country" %in% names(tbl)) {
      return(dplyr::rename(tbl, Country = country))
    }

    if ("Partner" %in% names(tbl)) {
      return(dplyr::rename(tbl, Country = Partner))
    }

    if ("partner" %in% names(tbl)) {
      return(dplyr::rename(tbl, Country = partner))
    }

    tbl
  }

  # Map Country -> iso3c and collapse to one row per coalition (iso3c, tech, supply_chain)
  eo <- allied_network_collapse_country_index(economic_opportunity_index, "Economic_Opportunity_Index", country_info, iso3c_network)
  es <- allied_network_collapse_country_index(energy_security_index, "Energy_Security_Index", country_info, iso3c_network)
  input_checks <- dplyr::bind_rows(
    allied_network_input_check_row("economic_opportunity_index", eo),
    allied_network_input_check_row("energy_security_index", es)
  )

  node_keys <- c("iso3c", "tech", "supply_chain")
  nodes <- eo %>%
    dplyr::select(iso3c, country, tech, supply_chain, eo_raw = value) %>%
    dplyr::left_join(es %>% dplyr::select(dplyr::all_of(node_keys), es_raw = value), by = node_keys, relationship = "one-to-one")

  if (node_policy) {
    # The policy table can carry component rows; only the overall index is a node input.
    if ("variable" %in% names(policy_index) && "Overall Policy Index" %in% policy_index$variable) {
      policy_index <- policy_index %>% dplyr::filter(variable == "Overall Policy Index")
    }
    pol <- allied_network_collapse_country_index(policy_index, "value", country_info, iso3c_network)
    input_checks <- dplyr::bind_rows(input_checks, allied_network_input_check_row("policy_index", pol))
    nodes <- nodes %>%
      dplyr::left_join(pol %>% dplyr::select(dplyr::all_of(node_keys), policy_raw = value), by = node_keys, relationship = "one-to-one")
  } else {
    nodes <- nodes %>% dplyr::mutate(policy_raw = NA_real_)
  }

  # Development potential (optional; only populated for EMEs in the current build)
  if (!is.null(partner_development_country_tbl)) {
    dev_src <- tibble::as_tibble(partner_development_country_tbl)
    if ("partner_iso" %in% names(dev_src)) {
      dev_src <- dev_src %>% dplyr::mutate(iso3c = toupper(as.character(partner_iso)))
    } else {
      dev_src <- normalize_country_col(dev_src)
      require_columns(dev_src, "Country", label = "partner_development_country_tbl")
      dev_src <- dev_src %>%
        dplyr::mutate(iso3c = partnership_strength_country_to_iso(partnership_strength_standardize_countries(Country), country_info))
    }
    require_columns(
      dev_src,
      c("iso3c", "tech", "supply_chain", "category", "variable", "data_type", "value"),
      label = "partner_development_country_tbl"
    )
    # A dyadic table repeats each partner's value for every reporter (plus an upstream fan-out
    # within dyads); collapsing by partner keeps one value per node.
    dev <- dev_src %>%
      dplyr::filter(
        category == "development",
        variable == "Development Potential Index",
        data_type == "index",
        iso3c %in% iso3c_network
      ) %>%
      dplyr::mutate(tech = as.character(tech), supply_chain = as.character(supply_chain)) %>%
      dplyr::select(iso3c, tech, supply_chain, value, dplyr::any_of("Year")) %>%
      allied_network_collapse_keys(keys = node_keys)
    input_checks <- dplyr::bind_rows(input_checks, allied_network_input_check_row("partner_development_country_tbl", dev))

    nodes <- nodes %>%
      dplyr::left_join(dev %>% dplyr::select(dplyr::all_of(node_keys), dev_potential = value), by = node_keys, relationship = "one-to-one")
  } else {
    nodes$dev_potential <- NA_real_
  }

  if (!is.null(country_size_tbl)) {
    require_columns(country_size_tbl, c("iso3c", "gdp_usd"), label = "country_size_tbl")
    nodes <- nodes %>%
      dplyr::left_join(
        country_size_tbl %>%
          dplyr::transmute(
            iso3c = as.character(iso3c),
            gdp_usd = suppressWarnings(as.numeric(gdp_usd)),
            gdp_imputed = if ("gdp_imputed" %in% names(country_size_tbl)) dplyr::coalesce(as.logical(.data$gdp_imputed), FALSE) else FALSE
          ) %>%
          dplyr::group_by(iso3c) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup(),
        by = "iso3c",
        relationship = "many-to-one"
      )
  } else {
    nodes <- nodes %>% dplyr::mutate(gdp_usd = NA_real_, gdp_imputed = FALSE)
  }

  attr(nodes, "input_checks") <- input_checks
  nodes
}

# Scoring step of allied_network_prepare_nodes() on (member- or node-level) raw inputs from
# allied_network_node_inputs() or allied_network_aggregate_node_inputs(): policy imputation, the
# within-stage S-curves, producer score and demand weights (arguments as in prepare_nodes). Need is
# 1 - S(es_need_raw) when the input carries es_need_raw (aggregated nodes), else 1 - es_idx.
allied_network_score_nodes <- function(raw,
                                       node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
                                       demand_mode = c("need", "size", "mixed", "equal"),
                                       demand_weights = list(need = 0.5, size = 0.5),
                                       size_transform = c("log_scurve", "scurve", "rank"),
                                       policy_missing = c("coalition_median", "none"),
                                       missing_components = c("renormalize", "require_all"),
                                       node_policy = TRUE) {
  demand_mode <- match.arg(demand_mode)
  size_transform <- match.arg(size_transform)
  policy_missing <- match.arg(policy_missing)
  missing_components <- match.arg(missing_components)
  node_policy <- allied_network_check_node_policy(node_policy)
  require_columns(raw, c("iso3c", "tech", "supply_chain", "eo_raw", "es_raw", "policy_raw", "dev_potential", "gdp_usd"), label = "node inputs")

  w_node <- c(
    eo = node_weights$eo,
    # node_policy = FALSE: the policy term carries no weight (EO and ES renormalized)
    policy = if (node_policy) node_weights$policy else 0,
    resilience = node_weights$resilience
  )
  if (length(w_node) != 3) stop("node_weights must name eo, policy and resilience.")
  w_node <- allied_network_normalize_weights(w_node)
  if (any(is.na(w_node))) stop("node_weights must be positive (eo/policy/resilience).")

  nodes <- raw
  if (!"es_need_raw" %in% names(nodes)) nodes$es_need_raw <- nodes$es_raw

  if (node_policy) {
    # Policy coverage is narrower than the coalition (the index keeps only countries with every
    # component). Without imputation those members can never be greedy producers, so the producer
    # map would reflect coverage rather than capability.
    nodes <- nodes %>%
      dplyr::group_by(tech, supply_chain) %>%
      dplyr::mutate(
        policy_imputed = policy_missing == "coalition_median" & is.na(policy_raw) & any(!is.na(policy_raw)),
        policy_raw = dplyr::if_else(policy_imputed, stats::median(policy_raw, na.rm = TRUE), policy_raw)
      ) %>%
      dplyr::ungroup()
  } else {
    nodes <- nodes %>% dplyr::mutate(policy_raw = NA_real_, policy_imputed = FALSE)
  }

  nodes <- nodes %>%
    dplyr::mutate(
      dev_potential = dplyr::coalesce(dev_potential, 0.5)
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      eo_idx  = median_scurve(eo_raw),
      es_idx  = median_scurve(es_raw),
      pol_idx = median_scurve(policy_raw),
      # "need" = vulnerability proxy (higher means more vulnerable / more demand for reliable supply);
      # es_need_raw = es_raw except for an aggregated node (GDP-weighted members' ES)
      need_idx = 1 - median_scurve(es_need_raw),
      size_idx = dplyr::case_when(
        size_transform == "log_scurve" ~ median_scurve(log1p(gdp_usd)),
        size_transform == "scurve" ~ median_scurve(gdp_usd),
        size_transform == "rank" ~ dplyr::percent_rank(gdp_usd),
        TRUE ~ median_scurve(log1p(gdp_usd))
      ),
      size_idx = dplyr::coalesce(size_idx, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-es_need_raw)

  # Producer score: weighted mean of the available S-curved components (the policy index's own
  # per-cell renormalization rule). A node with no component at all keeps NA and cannot be ranked.
  w_avail <- w_node[["eo"]] * (!is.na(nodes$eo_idx)) + w_node[["policy"]] * (!is.na(nodes$pol_idx)) +
    w_node[["resilience"]] * (!is.na(nodes$es_idx))
  score_sum <- w_node[["eo"]] * dplyr::coalesce(nodes$eo_idx, 0) + w_node[["policy"]] * dplyr::coalesce(nodes$pol_idx, 0) +
    w_node[["resilience"]] * dplyr::coalesce(nodes$es_idx, 0)
  nodes$node_components_n <- as.integer(!is.na(nodes$eo_idx)) + as.integer(!is.na(nodes$pol_idx)) + as.integer(!is.na(nodes$es_idx))
  nodes$producer_score <- if (missing_components == "renormalize") {
    dplyr::if_else(w_avail > 0, score_sum / w_avail, NA_real_)
  } else {
    # node_policy = FALSE: the policy term is absent, not missing (0 * NA would be NA)
    pol_term <- if (node_policy) w_node[["policy"]] * nodes$pol_idx else 0
    w_node[["eo"]] * nodes$eo_idx + pol_term + w_node[["resilience"]] * nodes$es_idx
  }
  nodes$demand_need_missing <- is.na(nodes$need_idx)

  w_need <- suppressWarnings(as.numeric(demand_weights$need))
  w_size <- suppressWarnings(as.numeric(demand_weights$size))
  if (!is.finite(w_need)) w_need <- 0.5
  if (!is.finite(w_size)) w_size <- 0.5
  # Mixed demand averages need and size; a node without an ES value (so no need) is sized by GDP
  # alone rather than given zero demand.
  mixed_complete <- if ((w_need + w_size) > 0) (w_need * nodes$need_idx + w_size * nodes$size_idx) / (w_need + w_size) else NA_real_
  mixed_size_only <- if (w_size > 0) nodes$size_idx else 0
  nodes$demand_mixed <- if (missing_components == "renormalize") {
    dplyr::if_else(is.na(nodes$need_idx), mixed_size_only, mixed_complete)
  } else {
    w_need * nodes$need_idx + w_size * nodes$size_idx
  }

  nodes <- nodes %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      demand_weight = dplyr::case_when(
        demand_mode == "equal" ~ 1,
        demand_mode == "need" ~ dplyr::coalesce(need_idx, 0),
        demand_mode == "size" ~ dplyr::coalesce(size_idx, 0),
        demand_mode == "mixed" ~ dplyr::coalesce(demand_mixed, 0),
        TRUE ~ dplyr::coalesce(need_idx, 0)
      ),
      demand_need_component = dplyr::case_when(
        demand_mode == "mixed" ~ dplyr::coalesce(w_need * need_idx, 0),
        demand_mode == "need" ~ dplyr::coalesce(need_idx, 0),
        TRUE ~ 0
      ),
      demand_size_component = dplyr::case_when(
        demand_mode == "mixed" ~ dplyr::coalesce(w_size * size_idx, 0),
        demand_mode == "size" ~ dplyr::coalesce(size_idx, 0),
        TRUE ~ 0
      ),
      demand_weight = if (all(is.na(demand_weight)) || sum(demand_weight, na.rm = TRUE) <= 0) {
        rep(1, dplyr::n())
      } else {
        demand_weight
      },
      demand_need_component = if (sum(demand_need_component, na.rm = TRUE) > 0) demand_need_component / sum(demand_need_component, na.rm = TRUE) else 0,
      demand_size_component = if (sum(demand_size_component, na.rm = TRUE) > 0) demand_size_component / sum(demand_size_component, na.rm = TRUE) else 0,
      demand_weight = demand_weight / sum(demand_weight, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-demand_mixed)

  if (anyDuplicated(nodes[c("iso3c", "tech", "supply_chain")])) {
    stop("allied_network_prepare_nodes: nodes are not unique per (iso3c, tech, supply_chain).")
  }
  nodes
}

# ---- Solver: greedy fallback ------------------------------------------------

allied_network_cap_shares <- function(shares, min_share, max_share) {
  shares <- as.numeric(shares)
  n <- length(shares)
  if (n == 0) return(shares)
  
  # Feasibility guards
  min_share <- max(0, min(min_share, 1 / n))
  max_share <- max(min_share, min(max_share, 1))
  
  # Start normalized
  if (sum(shares, na.rm = TRUE) <= 0) shares <- rep(1 / n, n)
  shares <- shares / sum(shares)
  
  # Iteratively clip to max_share, then enforce min_share
  for (iter in 1:50) {
    over <- which(shares > max_share + 1e-10)
    if (!length(over)) break
    excess <- sum(shares[over] - max_share)
    shares[over] <- max_share
    under_idx <- setdiff(seq_len(n), over)
    if (!length(under_idx)) break
    shares[under_idx] <- shares[under_idx] + excess * (shares[under_idx] / sum(shares[under_idx]))
  }
  
  for (iter in 1:50) {
    under <- which(shares < min_share - 1e-10)
    if (!length(under)) break
    deficit <- sum(min_share - shares[under])
    shares[under] <- min_share
    over_idx <- setdiff(seq_len(n), under)
    if (!length(over_idx)) break
    shares[over_idx] <- pmax(0, shares[over_idx] - deficit * (shares[over_idx] / sum(shares[over_idx])))
    if (sum(shares, na.rm = TRUE) <= 0) {
      shares <- rep(1 / n, n)
      break
    }
    shares <- shares / sum(shares)
  }
  
  shares / sum(shares)
}

# Greedy heuristic (the fallback when the MILP is unavailable, too large or fails): pick the top
# distinct countries by combined score, set capped softmax shares, then fit flows to those shares and
# to demand under the MILP's supplier-redundancy rules. Not an optimum; diagnostics say which ran.
# With diversity_lambda > 0 the producer count and shares instead maximise the MILP's node objective
# with the HHI penalty in closed form (allied_network_diversified_shares), valuing a unit of share at
# w_node * a_p + w_edge * connectivity_out_p (the edge term if p served every consumer in proportion
# to demand) plus the w_dev * dev_p bonus per selected producer. diversity_lambda = 0 keeps the
# softmax shares on exactly min_producers producers.
allied_network_solve_stage_greedy <- function(nodes_stage,
                                              edges_stage,
                                              producer_forbidden = character(),
                                              min_producers = 3,
                                              max_share = 0.40,
                                              min_share = 0.05,
                                              min_suppliers_per_consumer = 2,
                                              epsilon_supplier_share = 0.10,
                                              w_edge = 0.5,
                                              w_node = 1.0,
                                              w_dev = 0.0,
                                              allow_self = TRUE,
                                              diversity_lambda = 0) {
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  diversity_lambda <- allied_network_check_lambda(diversity_lambda)

  iso <- as.character(nodes_stage$iso3c)
  if (anyDuplicated(iso)) stop("allied_network_solve_stage_greedy: nodes_stage must have one row per iso3c.")
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  min_required <- ceiling(1 / max_share)
  # As in the MILP, a consumer needing k suppliers implies at least k producers
  target_prod <- max(min_producers, min_required, min_suppliers_per_consumer)
  n_prod <- min(target_prod, length(eligible_iso))
  if (length(eligible_iso) < target_prod) warning("Producer forbiddance reduced feasible producers for stage; using eligible maximum.")

  # Complete edges for this stage (self pairs stay in the matrix; flows drop them if allow_self=FALSE)
  edges_full <- allied_network_complete_edges_stage(
    edges_stage,
    iso3c_vec = iso,
    allow_self = TRUE,
    fill_missing = 0,
    self_weight = NULL
  )
  conn <- allied_network_connectivity_out(edges_full, nodes_stage)

  # Rank on the combined score. A node without a producer score competes on its edges only and ranks
  # after every scored node; ties break on producer score, connectivity, then ISO3, so the choice
  # never depends on input row order.
  candidates <- nodes_stage %>%
    dplyr::left_join(conn, by = "iso3c", relationship = "one-to-one") %>%
    dplyr::mutate(
      connectivity_out = dplyr::coalesce(connectivity_out, 0),
      has_score = !is.na(producer_score),
      combined_score = w_node * dplyr::coalesce(producer_score, 0) + w_edge * connectivity_out +
        w_dev * dplyr::coalesce(dev_potential, 0)
    ) %>%
    dplyr::arrange(dplyr::desc(has_score), dplyr::desc(combined_score), dplyr::desc(producer_score),
                   dplyr::desc(connectivity_out), iso3c)

  # A producer needs a positive edge to some consumer with demand (itself only if self-supply is
  # allowed); otherwise the flow fit gives it nothing and the others absorb its share past max_share.
  # Such nodes are picked only if too few others remain.
  d_pos <- stats::setNames(dplyr::coalesce(suppressWarnings(as.numeric(nodes_stage$demand_weight)), 0) > 0, iso)
  if (!any(d_pos)) d_pos[] <- TRUE
  reach_iso <- unique(edges_full$reporter_iso[
    dplyr::coalesce(edges_full$edge_weight, 0) > 0 & d_pos[edges_full$partner_iso] &
      (isTRUE(allow_self) | edges_full$reporter_iso != edges_full$partner_iso)
  ])
  pool <- unique(candidates$iso3c[candidates$iso3c %in% eligible_iso])
  pool <- c(pool[pool %in% reach_iso], pool[!pool %in% reach_iso])
  diversified <- NULL
  if (diversity_lambda > 0) {
    cand_pool <- candidates[match(pool, candidates$iso3c), ]
    diversified <- allied_network_diversified_shares(
      v = w_node * dplyr::coalesce(cand_pool$producer_score, 0) + w_edge * cand_pool$connectivity_out,
      bonus = w_dev * dplyr::coalesce(cand_pool$dev_potential, 0),
      lambda = diversity_lambda, min_share = min_share, max_share = max_share, n_min = n_prod
    )
  }

  if (!is.null(diversified)) {
    selected_iso <- pool[seq_len(diversified$k)]
    target <- stats::setNames(diversified$shares, selected_iso)
  } else {
    selected_iso <- utils::head(pool, n_prod)
    # Target shares: softmax of the combined score among selected producers, then min/max capped
    sel_scores <- candidates %>% dplyr::filter(iso3c %in% selected_iso)
    s <- sel_scores$combined_score
    raw <- exp(s - max(s))
    shares <- allied_network_cap_shares(raw / sum(raw), min_share = min_share, max_share = max_share)
    target <- stats::setNames(shares, sel_scores$iso3c)[selected_iso]
  }

  # Flows: biproportional (RAS) fit of the edge weights to the target shares (row sums) and demand
  # (column sums), so each producer's realized supply equals its capped share. The earlier rule
  # (split each consumer's demand in proportion to W_ij * s_i) let supply drift from s_i and past
  # max_share. A consumer with no positive edge to any selected producer is served pro rata.
  d <- suppressWarnings(as.numeric(nodes_stage$demand_weight))
  d[!is.finite(d)] <- 0
  d <- if (sum(d) > 0) d / sum(d) else rep(1 / length(iso), length(iso))
  K <- matrix(0, nrow = length(selected_iso), ncol = length(iso), dimnames = list(selected_iso, iso))
  e_sel <- edges_full %>% dplyr::filter(reporter_iso %in% selected_iso)
  K[cbind(match(e_sel$reporter_iso, selected_iso), match(e_sel$partner_iso, iso))] <- pmax(0, dplyr::coalesce(e_sel$edge_weight, 0))
  self_idx <- cbind(seq_along(selected_iso), match(selected_iso, iso))
  if (!isTRUE(allow_self)) K[self_idx] <- 0
  dead <- colSums(K) <= 0 & d > 0
  if (any(dead)) {
    K[, dead] <- 1
    if (!isTRUE(allow_self)) K[self_idx[dead[self_idx[, 2]], , drop = FALSE]] <- 0
  }
  f <- allied_network_ras(K, row_target = as.numeric(target), col_target = d)
  f <- allied_network_prune_small_flows(
    f, K, row_target = as.numeric(target), col_target = d,
    min_suppliers = min_suppliers_per_consumer, epsilon = epsilon_supplier_share
  )
  # Diversified shares put many producers near min_share: finish the prune one flow at a time
  # (diversity_lambda = 0 keeps the legacy greedy flows exactly)
  if (diversity_lambda > 0) {
    f <- allied_network_prune_small_flows_stepwise(
      f, K, row_target = as.numeric(target), col_target = d,
      min_suppliers = min_suppliers_per_consumer, epsilon = epsilon_supplier_share
    )
  }
  if (max(abs(rowSums(f) - as.numeric(target))) > 1e-6) {
    warning("Greedy flow fit could not meet the target production shares for stage; realized shares are reported (see max_production_share).")
  }

  flows <- tibble::tibble(
    reporter_iso = rep(selected_iso, times = length(iso)),
    partner_iso = rep(iso, each = length(selected_iso)),
    flow_share = as.vector(f)
  ) %>%
    dplyr::filter(flow_share > 0) %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"), relationship = "many-to-one")

  # Production share = realized supply (equal to the target whenever the RAS fit converges)
  prod_tbl <- tibble::tibble(iso3c = selected_iso, production_share = as.numeric(rowSums(f)))
  specialization <- candidates %>%
    dplyr::mutate(selected = iso3c %in% selected_iso) %>%
    dplyr::left_join(prod_tbl, by = "iso3c", relationship = "one-to-one") %>%
    dplyr::mutate(production_share = dplyr::coalesce(production_share, 0)) %>%
    dplyr::select(iso3c, country, producer_score, connectivity_out, dev_potential, demand_weight, selected, production_share, dplyr::any_of(allied_network_node_extra_cols()))

  # Same objective as the MILP (missing producer score counts as 0), so the two are comparable; the
  # HHI penalty is exact here (no tangent approximation)
  a_by_iso <- stats::setNames(dplyr::coalesce(specialization$producer_score, 0), specialization$iso3c)
  dev_by_iso <- stats::setNames(dplyr::coalesce(specialization$dev_potential, 0), specialization$iso3c)
  objective <- sum(flows$flow_share * (w_node * a_by_iso[flows$reporter_iso] + w_edge * flows$edge_weight), na.rm = TRUE) +
    w_dev * sum(dev_by_iso[selected_iso])
  if (diversity_lambda > 0) objective <- objective - diversity_lambda * sum(prod_tbl$production_share^2)

  list(
    specialization = specialization,
    flows = flows %>%
      dplyr::select(reporter_iso, partner_iso, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index")), flow_share),
    objective = objective,
    hhi = allied_network_hhi(prod_tbl$production_share),
    n_producers = length(selected_iso),
    hhi_tangent_max_error = NA_real_
  )
}

# Biproportional (RAS) fitting: rescale the rows and columns of a non-negative seed matrix until its
# row sums match row_target and its column sums match col_target (both summing to one). The column
# step runs last, so demand is always met exactly; rows match whenever the seed's zero pattern allows.
allied_network_ras <- function(K, row_target, col_target, max_iter = 2000, tol = 1e-12) {
  f <- K
  for (iter in seq_len(max_iter)) {
    rs <- rowSums(f)
    f <- f * ifelse(rs > 0, row_target / rs, 0)
    cs <- colSums(f)
    f <- sweep(f, 2, ifelse(cs > 0, col_target / cs, 0), `*`)
    if (max(abs(rowSums(f) - row_target)) < tol) break
  }
  f
}

# The MILP's redundancy rules applied to a RAS flow matrix: a supplier serving less than `epsilon` of a
# consumer's demand is dropped (never leaving that consumer with fewer than `min_suppliers`) and the fit
# is repeated. A refit that can no longer meet the producer shares is rejected, because the share caps
# are hard constraints and the redundancy rules are not.
allied_network_prune_small_flows <- function(f, K, row_target, col_target, min_suppliers = 2, epsilon = 0.10,
                                             max_passes = 5, tol = 1e-6) {
  if (!is.finite(epsilon) || epsilon <= 0) return(f)
  for (pass in seq_len(max_passes)) {
    rel <- sweep(f, 2, ifelse(col_target > 0, col_target, 1), `/`)
    small <- f > 0 & rel < epsilon - 1e-9
    if (!any(small)) break
    K_new <- K
    for (j in which(colSums(small) > 0)) {
      n_drop <- min(sum(small[, j]), sum(f[, j] > 0) - min_suppliers)
      if (n_drop <= 0) next
      cand <- which(small[, j])
      K_new[cand[order(rel[cand, j])][seq_len(n_drop)], j] <- 0
    }
    if (identical(K_new, K)) break
    f_new <- allied_network_ras(K_new, row_target = row_target, col_target = col_target)
    if (max(abs(rowSums(f_new) - row_target)) > tol || max(abs(colSums(f_new) - col_target)) > tol) break
    K <- K_new
    f <- f_new
  }
  f
}

# Stepwise version of the same rule, for the diversified greedy: drop one sub-epsilon flow at a time
# (smallest share of its consumer's demand first, never leaving a consumer with fewer than
# min_suppliers) and keep the drop only if the refit still meets the producer shares; if it does not,
# try the next candidate. With diversity_lambda > 0 the greedy selects many producers near min_share
# and RAS spreads each over every consumer, so nearly every flow is a sliver; dropping all of a
# consumer's slivers at once (allied_network_prune_small_flows) then cannot be refit and the 10% rule
# was left unmet for every consumer (Nuclear Upstream at lambda 1.5 on the 2026-10 inputs: smallest
# active supplier 3.6% of demand; stepwise: none below 10%, in 91 refits). max_refits bounds the work.
allied_network_prune_small_flows_stepwise <- function(f, K, row_target, col_target, min_suppliers = 2, epsilon = 0.10,
                                                      tol = 1e-6, max_refits = 2000) {
  if (!is.finite(epsilon) || epsilon <= 0) return(f)
  K <- K * (f > 0)
  refits <- 0
  repeat {
    rel <- sweep(f, 2, ifelse(col_target > 0, col_target, 1), `/`)
    small <- which(f > 0 & rel < epsilon - 1e-9, arr.ind = TRUE)
    small <- small[colSums(f > 0)[small[, 2]] > min_suppliers, , drop = FALSE]
    if (!nrow(small)) break
    small <- small[order(rel[small]), , drop = FALSE]
    dropped <- FALSE
    for (k in seq_len(nrow(small))) {
      if (refits >= max_refits) break
      K_try <- K
      K_try[small[k, 1], small[k, 2]] <- 0
      f_try <- allied_network_ras(K_try, row_target = row_target, col_target = col_target)
      refits <- refits + 1
      if (max(abs(rowSums(f_try) - row_target)) <= tol && max(abs(colSums(f_try) - col_target)) <= tol) {
        K <- K_try
        f <- f_try
        dropped <- TRUE
        break
      }
    }
    if (!dropped) break
  }
  f
}

# Realized redundancy of a stage's flows: the fewest suppliers any consumer with positive demand has,
# and the smallest share of a consumer's demand that any active supplier provides.
allied_network_supplier_stats <- function(flows, specialization) {
  na_out <- list(min_suppliers = NA_integer_, min_supplier_share = NA_real_)
  if (is.null(flows) || !nrow(flows) || is.null(specialization)) return(na_out)
  d <- specialization %>%
    dplyr::filter(dplyr::coalesce(demand_weight, 0) > 0) %>%
    dplyr::select(partner_iso = iso3c, demand_weight)
  per <- flows %>%
    dplyr::filter(flow_share > 1e-12) %>%
    dplyr::inner_join(d, by = "partner_iso", relationship = "many-to-one") %>%
    dplyr::group_by(partner_iso) %>%
    dplyr::summarise(n = dplyr::n(), rel = min(flow_share / demand_weight), .groups = "drop")
  if (!nrow(per)) return(na_out)
  n_sup <- d %>% dplyr::left_join(per, by = "partner_iso") %>% dplyr::mutate(n = dplyr::coalesce(n, 0L))
  list(min_suppliers = as.integer(min(n_sup$n)), min_supplier_share = min(per$rel))
}

# ---- Solver: MILP (optional) ------------------------------------------------

allied_network_has_milp <- function() {
  requireNamespace("ompr", quietly = TRUE) &&
    requireNamespace("ompr.roi", quietly = TRUE) &&
    requireNamespace("ROI", quietly = TRUE) &&
    requireNamespace("ROI.plugin.glpk", quietly = TRUE)
}

allied_network_solve_stage_milp <- function(nodes_stage,
                                            edges_stage,
                                            producer_forbidden = character(),
                                            min_producers = 3,
                                            max_share = 0.40,
                                            min_share = 0.05,
                                            min_suppliers_per_consumer = 2,
                                            epsilon_supplier_share = 0.10,
                                            w_edge = 0.5,
                                            w_node = 1.0,
                                            w_dev = 0.0,
                                            allow_self = TRUE,
                                            solver = c("glpk"),
                                            time_limit_sec = NULL,
                                            diversity_lambda = 0,
                                            diversity_breakpoint_step = 0.01) {
  solver <- match.arg(solver)
  if (!allied_network_has_milp()) {
    stop("MILP solver packages not available. Install ompr, ompr.roi, ROI, ROI.plugin.glpk or use method='greedy'.")
  }

  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  diversity_lambda <- allied_network_check_lambda(diversity_lambda)

  iso <- as.character(nodes_stage$iso3c)
  if (anyDuplicated(iso)) stop("allied_network_solve_stage_milp: nodes_stage must have one row per iso3c.")
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  # A node without a producer score is not a producer candidate (scoring it 0 let the MILP pick
  # members for their edges alone) unless too few scored nodes remain for a feasible stage.
  scored_iso <- intersect(eligible_iso, iso[is.finite(nodes_stage$producer_score)])
  if (length(scored_iso) >= max(min_producers, ceiling(1 / max_share), min_suppliers_per_consumer)) eligible_iso <- scored_iso
  n <- length(iso)
  if (n == 0) stop("No nodes in stage.")
  
  # Complete edge matrix for this stage
  edges_full <- allied_network_complete_edges_stage(
    edges_stage,
    iso3c_vec = iso,
    allow_self = TRUE,
    fill_missing = 0,
    self_weight = NULL
  )
  
  # Index mapping iso3c -> i
  idx <- stats::setNames(seq_len(n), iso)
  
  # Vectors/matrix (sanitize to avoid NA coefficients in MILP objective/constraints)
  a <- suppressWarnings(as.numeric(nodes_stage$producer_score))
  d <- suppressWarnings(as.numeric(nodes_stage$demand_weight))
  dev <- suppressWarnings(as.numeric(nodes_stage$dev_potential))

  a[!is.finite(a)] <- 0
  dev[!is.finite(dev)] <- 0
  d[!is.finite(d)] <- 0
  if (sum(d, na.rm = TRUE) <= 0) {
    d <- rep(1 / n, n)
  } else {
    d <- d / sum(d, na.rm = TRUE)
  }
  
  # Edges are unique per pair (asserted in complete_edges_stage), so each cell is written once
  W <- matrix(0, nrow = n, ncol = n, dimnames = list(iso, iso))
  in_stage <- edges_full$reporter_iso %in% iso & edges_full$partner_iso %in% iso
  ew <- suppressWarnings(as.numeric(edges_full$edge_weight[in_stage]))
  ew[!is.finite(ew)] <- 0
  W[cbind(match(edges_full$reporter_iso[in_stage], iso), match(edges_full$partner_iso[in_stage], iso))] <- ew

  # ompr model
  # Variables:
  # f[i,j] flow share from i to j
  # z[i]   producer selected
  # y[i,j] supplier assignment (binary) for redundancy per consumer
  suppressPackageStartupMessages(library(ompr))
  suppressPackageStartupMessages(library(ompr.roi))
  suppressPackageStartupMessages(library(ROI))
  suppressPackageStartupMessages(library(ROI.plugin.glpk))
  
  model <- ompr::MIPModel() %>%
    ompr::add_variable(f[i, j], i = 1:n, j = 1:n, type = "continuous", lb = 0) %>%
    ompr::add_variable(z[i], i = 1:n, type = "binary") %>%
    ompr::add_variable(y[i, j], i = 1:n, j = 1:n, type = "binary") %>%
    # Objective: node capability + edge feasibility + dev "buildability" signal
    ompr::set_objective(
      ompr::sum_expr(f[i, j] * (w_node * a[i] + w_edge * W[iso[i], iso[j]]), i = 1:n, j = 1:n) +
        ompr::sum_expr(w_dev * dev[i] * z[i], i = 1:n),
      "max"
    ) %>%
    # Meet each consumer's demand weight
    ompr::add_constraint(ompr::sum_expr(f[i, j], i = 1:n) == d[j], j = 1:n) %>%
    # Production share caps and selection linkage
    ompr::add_constraint(ompr::sum_expr(f[i, j], j = 1:n) <= max_share * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(f[i, j], j = 1:n) >= min_share * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(z[i], i = 1:n) >= min(min_producers, length(eligible_iso))) %>%
    # Supplier assignment constraints (redundancy per consumer)
    ompr::add_constraint(f[i, j] <= d[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(f[i, j] >= epsilon_supplier_share * d[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(y[i, j] <= z[i], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(y[i, j], i = 1:n) >= min_suppliers_per_consumer, j = 1:n)

  if (length(setdiff(iso, eligible_iso)) > 0) {
    forbidden_idx <- which(iso %in% setdiff(iso, eligible_iso))
    model <- model %>% ompr::add_constraint(z[i] == 0, i = forbidden_idx)
  }

  if (!isTRUE(allow_self)) {
    # Disallow self-supply by forcing y[i,i]=0 and f[i,i]=0
    model <- model %>%
      ompr::add_constraint(f[i, i] == 0, i = 1:n) %>%
      ompr::add_constraint(y[i, i] == 0, i = 1:n)
  }

  # Diversification: subtract diversity_lambda * sum_i tt_i, where x_i is producer i's share and tt_i
  # lies above every tangent of x^2 at the breakpoints bp. Maximising pushes tt_i down onto the tangent
  # envelope, which under-approximates x_i^2 by at most step^2 / 4. The cuts are written in perspective
  # form, tt_i >= 2 bp_k x_i - bp_k^2 z_i: with z_i = 1 they are the plain tangents and with z_i = 0
  # (so x_i = 0) they reduce to tt_i >= 0, so the integer solutions and their objective values are
  # unchanged, but the LP relaxation no longer lets a fractional z_i hold a share below min_share at
  # almost no penalty. On a Nuclear Upstream stage with near-tied candidates this cut GLPK's time from
  # 875 s to 51 s with the same solution. The objective already holds the linear part, so it is
  # replaced with the penalised version. With diversity_lambda = 0 the model is left exactly as before
  # (no extra variables or rows).
  if (diversity_lambda > 0) {
    bp <- allied_network_hhi_breakpoints(max_share, diversity_breakpoint_step)
    n_bp <- length(bp)
    model <- model %>%
      ompr::add_variable(x[i], i = 1:n, type = "continuous", lb = 0) %>%
      ompr::add_variable(tt[i], i = 1:n, type = "continuous", lb = 0) %>%
      ompr::add_constraint(x[i] == ompr::sum_expr(f[i, j], j = 1:n), i = 1:n) %>%
      ompr::add_constraint(tt[i] >= 2 * bp[k] * x[i] - bp[k]^2 * z[i], i = 1:n, k = 1:n_bp) %>%
      ompr::set_objective(
        ompr::sum_expr(f[i, j] * (w_node * a[i] + w_edge * W[iso[i], iso[j]]), i = 1:n, j = 1:n) +
          ompr::sum_expr(w_dev * dev[i] * z[i], i = 1:n) -
          ompr::sum_expr(diversity_lambda * tt[i], i = 1:n),
        "max"
      )
  }

  # GLPK's own time limit (ms): setTimeLimit() in the caller cannot interrupt compiled solver code
  solver_args <- list(solver = solver)
  if (!is.null(time_limit_sec) && is.finite(time_limit_sec)) {
    solver_args$control <- list(tm_limit = as.integer(round(1000 * time_limit_sec)))
  }
  result <- ompr::solve_model(model, do.call(ompr.roi::with_ROI, solver_args))
  # A time-limited or infeasible run still returns a (non-optimal) solution vector; refuse it so the
  # caller falls back to greedy and records why.
  if (!identical(ompr::solver_status(result), "success")) {
    roi_status <- tryCatch(ompr::additional_solver_output(result)$ROI$status$msg$message, error = function(e) NULL)
    stop("MILP did not return an optimal solution", if (length(roi_status)) paste0(": ", roi_status) else "")
  }

  flows <- ompr::get_solution(result, f[i, j]) %>%
    dplyr::mutate(
      reporter_iso = iso[i],
      partner_iso = iso[j],
      flow_share = value
    ) %>%
    dplyr::select(reporter_iso, partner_iso, flow_share) %>%
    dplyr::filter(flow_share > 1e-12)

  selected <- ompr::get_solution(result, z[i]) %>%
    dplyr::mutate(iso3c = iso[i], selected = round(value) == 1) %>%
    dplyr::select(iso3c, selected)

  # connectivity_out is reported for both solvers (build_candidates uses it)
  conn <- allied_network_connectivity_out(edges_full, nodes_stage)
  specialization <- nodes_stage %>%
    dplyr::left_join(selected, by = "iso3c", relationship = "one-to-one") %>%
    dplyr::mutate(selected = dplyr::coalesce(selected, FALSE)) %>%
    dplyr::left_join(conn, by = "iso3c", relationship = "one-to-one") %>%
    dplyr::left_join(
      flows %>%
        dplyr::group_by(reporter_iso) %>%
        dplyr::summarize(production_share = sum(flow_share, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(iso3c = reporter_iso),
      by = "iso3c"
    ) %>%
    dplyr::mutate(
      production_share = dplyr::coalesce(production_share, 0),
      connectivity_out = dplyr::coalesce(connectivity_out, 0)
    ) %>%
    dplyr::select(iso3c, country, producer_score, connectivity_out, dev_potential, demand_weight, selected, production_share, dplyr::any_of(allied_network_node_extra_cols()))

  flows <- flows %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"), relationship = "many-to-one") %>%
    dplyr::select(reporter_iso, partner_iso, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index")), flow_share)

  # Realized gap between the tangent envelope and the true square (bounded by step^2 / 4)
  tangent_error <- NA_real_
  if (diversity_lambda > 0) {
    x_val <- ompr::get_solution(result, x[i]) %>% dplyr::arrange(i) %>% dplyr::pull(value)
    t_val <- ompr::get_solution(result, tt[i]) %>% dplyr::arrange(i) %>% dplyr::pull(value)
    tangent_error <- max(abs(t_val - x_val^2))
  }

  list(
    specialization = specialization,
    flows = flows,
    objective = ompr::objective_value(result),
    hhi = allied_network_hhi(specialization$production_share[specialization$production_share > 0]),
    n_producers = sum(specialization$selected, na.rm = TRUE),
    hhi_tangent_max_error = tangent_error
  )
}

allied_network_build_cost <- function(dev_potential, build_cost_mode = c("none", "flat", "dev_potential")) {
  build_cost_mode <- match.arg(build_cost_mode)
  dev <- suppressWarnings(as.numeric(dev_potential))
  dev[!is.finite(dev)] <- 0.5
  dev <- pmax(0, pmin(1, dev))
  if (build_cost_mode == "none") return(rep(0, length(dev)))
  if (build_cost_mode == "flat") return(rep(1, length(dev)))
  1 - dev
}

allied_network_solve_stage_milp_scaled <- function(nodes_stage,
                                                   edges_stage,
                                                   target_total_usd,
                                                   producer_forbidden = character(),
                                                   cap0_usd,
                                                   capMax_usd,
                                                   build_max_usd,
                                                   build_cost_mode = c("none", "flat", "dev_potential"),
                                                   build_cost_weight = 0,
                                                   min_producers = 3,
                                                   max_share = 0.40,
                                                   min_share = 0.05,
                                                   min_suppliers_per_consumer = 2,
                                                   epsilon_supplier_share = 0.10,
                                                   w_edge = 0.5,
                                                   w_node = 1.0,
                                                   w_dev = 0.0,
                                                   allow_self = TRUE,
                                                   solver = c("glpk")) {
  solver <- match.arg(solver)
  build_cost_mode <- match.arg(build_cost_mode)
  if (!allied_network_has_milp()) {
    stop("MILP solver packages not available. Install ompr, ompr.roi, ROI, ROI.plugin.glpk or use method='greedy'.")
  }
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")

  iso <- as.character(nodes_stage$iso3c)
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  n <- length(iso)
  if (n == 0) stop("No nodes in stage.")
  target_total_usd <- suppressWarnings(as.numeric(target_total_usd))
  if (!is.finite(target_total_usd) || target_total_usd <= 0) stop("target_total_usd must be positive.")

  cap0 <- suppressWarnings(as.numeric(cap0_usd[iso]))
  capMax <- suppressWarnings(as.numeric(capMax_usd[iso]))
  build_max <- suppressWarnings(as.numeric(build_max_usd[iso]))
  cap0[!is.finite(cap0)] <- 0
  capMax[!is.finite(capMax)] <- 0
  build_max[!is.finite(build_max)] <- 0
  cap0 <- pmax(0, cap0)
  capMax <- pmax(0, capMax)
  build_max <- pmax(0, build_max)

  edges_full <- allied_network_complete_edges_stage(edges_stage, iso3c_vec = iso, allow_self = TRUE, fill_missing = 0, self_weight = NULL)

  a <- suppressWarnings(as.numeric(nodes_stage$producer_score)); a[!is.finite(a)] <- 0
  d <- suppressWarnings(as.numeric(nodes_stage$demand_weight)); d[!is.finite(d)] <- 0
  dev <- suppressWarnings(as.numeric(nodes_stage$dev_potential)); dev[!is.finite(dev)] <- 0.5
  if (sum(d, na.rm = TRUE) <= 0) d <- rep(1 / n, n) else d <- d / sum(d, na.rm = TRUE)
  D <- d * target_total_usd
  min_lb <- pmin(min_share * target_total_usd, capMax)
  max_prod <- pmin(max_share * target_total_usd, capMax)
  build_cost <- allied_network_build_cost(dev, build_cost_mode = build_cost_mode)

  W <- matrix(0, nrow = n, ncol = n, dimnames = list(iso, iso))
  for (k in seq_len(nrow(edges_full))) {
    i <- edges_full$reporter_iso[[k]]
    j <- edges_full$partner_iso[[k]]
    ew <- suppressWarnings(as.numeric(edges_full$edge_weight[[k]]))
    if (!is.na(i) && !is.na(j) && i %in% iso && j %in% iso) W[i, j] <- if (is.finite(ew)) ew else 0
  }

  suppressPackageStartupMessages(library(ompr))
  suppressPackageStartupMessages(library(ompr.roi))
  suppressPackageStartupMessages(library(ROI))
  suppressPackageStartupMessages(library(ROI.plugin.glpk))

  model <- ompr::MIPModel() %>%
    ompr::add_variable(q[i, j], i = 1:n, j = 1:n, type = "continuous", lb = 0) %>%
    ompr::add_variable(z[i], i = 1:n, type = "binary") %>%
    ompr::add_variable(y[i, j], i = 1:n, j = 1:n, type = "binary") %>%
    ompr::add_variable(build[i], i = 1:n, type = "continuous", lb = 0) %>%
    ompr::set_objective(
      ompr::sum_expr(q[i, j] * (w_node * a[i] + w_edge * W[iso[i], iso[j]]), i = 1:n, j = 1:n) +
        ompr::sum_expr(w_dev * dev[i] * z[i], i = 1:n) -
        build_cost_weight * ompr::sum_expr(build[i] * build_cost[i], i = 1:n),
      "max"
    ) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], i = 1:n) == D[j], j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) <= cap0[i] + build[i], i = 1:n) %>%
    ompr::add_constraint(build[i] <= build_max[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) <= capMax[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) <= max_prod[i] * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) >= min_lb[i] * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(z[i], i = 1:n) >= min(min_producers, length(eligible_iso))) %>%
    ompr::add_constraint(q[i, j] <= D[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(q[i, j] >= epsilon_supplier_share * D[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(y[i, j] <= z[i], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(y[i, j], i = 1:n) >= min_suppliers_per_consumer, j = 1:n)

  if (length(setdiff(iso, eligible_iso)) > 0) {
    forbidden_idx <- which(iso %in% setdiff(iso, eligible_iso))
    model <- model %>% ompr::add_constraint(z[i] == 0, i = forbidden_idx)
  }

  if (!isTRUE(allow_self)) {
    model <- model %>% ompr::add_constraint(q[i, i] == 0, i = 1:n) %>% ompr::add_constraint(y[i, i] == 0, i = 1:n)
  }

  result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = solver))
  status <- ROI::solution(result, "status")$code
  if (!identical(status, 0L)) stop("Scaled MILP did not converge to optimal solution.")

  flows <- ompr::get_solution(result, q[i, j]) %>%
    dplyr::mutate(reporter_iso = iso[i], partner_iso = iso[j], flow_usd = value) %>%
    dplyr::select(reporter_iso, partner_iso, flow_usd) %>%
    dplyr::filter(flow_usd > 1e-9) %>%
    dplyr::mutate(flow_share = flow_usd / target_total_usd) %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"))

  selected <- ompr::get_solution(result, z[i]) %>%
    dplyr::mutate(iso3c = iso[i], selected = as.integer(value) == 1) %>%
    dplyr::select(iso3c, selected)

  build_tbl <- ompr::get_solution(result, build[i]) %>%
    dplyr::transmute(iso3c = iso[i], build_usd = value)

  specialization <- nodes_stage %>%
    dplyr::left_join(selected, by = "iso3c") %>%
    dplyr::mutate(selected = dplyr::coalesce(selected, FALSE)) %>%
    dplyr::left_join(
      flows %>% dplyr::group_by(reporter_iso) %>% dplyr::summarize(production_usd = sum(flow_usd, na.rm = TRUE), .groups = "drop") %>% dplyr::rename(iso3c = reporter_iso),
      by = "iso3c"
    ) %>%
    dplyr::left_join(build_tbl, by = "iso3c") %>%
    dplyr::mutate(
      production_usd = dplyr::coalesce(production_usd, 0),
      production_share = production_usd / target_total_usd,
      cap0_exports_usd = cap0,
      capMax_exports_usd = capMax,
      build_max_usd = build_max,
      build_needed_usd = pmax(0, production_usd - cap0_exports_usd),
      build_usd = dplyr::coalesce(build_usd, 0)
    ) %>%
    dplyr::select(iso3c, country, selected, production_usd, production_share, cap0_exports_usd, capMax_exports_usd, build_max_usd, build_needed_usd, demand_weight, producer_score, dev_potential, dplyr::any_of(c("connectivity_out", allied_network_node_extra_cols())))

  list(
    specialization = specialization,
    flows = flows %>% dplyr::select(reporter_iso, partner_iso, flow_usd, flow_share, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index"))),
    objective = ompr::objective_value(result),
    hhi = allied_network_hhi(specialization$production_share),
    n_producers = sum(specialization$selected, na.rm = TRUE),
    unmet_demand_usd = 0
  )
}

allied_network_solve_stage_greedy_scaled <- function(nodes_stage,
                                                     edges_stage,
                                                     target_total_usd,
                                                     producer_forbidden = character(),
                                                     cap0_usd,
                                                     capMax_usd,
                                                     build_max_usd,
                                                     min_producers = 3,
                                                     max_share = 0.40,
                                                     min_share = 0.05,
                                                     min_suppliers_per_consumer = 2,
                                                     epsilon_supplier_share = 0.10,
                                                     w_edge = 0.5,
                                                     w_node = 1.0,
                                                     w_dev = 0.0,
                                                     allow_self = TRUE) {
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  iso <- as.character(nodes_stage$iso3c)
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  target_total_usd <- suppressWarnings(as.numeric(target_total_usd))
  if (!is.finite(target_total_usd) || target_total_usd <= 0) stop("target_total_usd must be positive.")

  edges_full <- allied_network_complete_edges_stage(edges_stage, iso3c_vec = iso, allow_self = TRUE, fill_missing = 0, self_weight = NULL)
  d_tbl <- nodes_stage %>% dplyr::select(iso3c, demand_weight)
  d <- suppressWarnings(as.numeric(d_tbl$demand_weight)); d[!is.finite(d)] <- 0
  if (sum(d) <= 0) d <- rep(1 / length(d), length(d)) else d <- d / sum(d)
  D <- stats::setNames(d * target_total_usd, d_tbl$iso3c)

  conn <- edges_full %>%
    dplyr::left_join(d_tbl, by = c("partner_iso" = "iso3c")) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::summarize(connectivity_out = sum(edge_weight * demand_weight, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(iso3c = reporter_iso)

  candidates <- nodes_stage %>%
    dplyr::left_join(conn, by = "iso3c") %>%
    dplyr::mutate(
      connectivity_out = dplyr::coalesce(connectivity_out, 0),
      combined_score = w_node * producer_score + w_edge * connectivity_out + w_dev * dev_potential
    ) %>%
    dplyr::arrange(dplyr::desc(combined_score))

  cap0 <- stats::setNames(pmax(0, suppressWarnings(as.numeric(cap0_usd[iso]))), iso); cap0[!is.finite(cap0)] <- 0
  capMax <- stats::setNames(pmax(0, suppressWarnings(as.numeric(capMax_usd[iso]))), iso); capMax[!is.finite(capMax)] <- 0
  build_max <- stats::setNames(pmax(0, suppressWarnings(as.numeric(build_max_usd[iso]))), iso); build_max[!is.finite(build_max)] <- 0
  remaining_cap <- capMax

  min_required <- max(min_producers, ceiling(1 / max_share))
  if (length(eligible_iso) < min_required) warning("Producer forbiddance reduced feasible producers for stage; using eligible maximum.")
  selected_iso <- candidates %>% dplyr::filter(iso3c %in% eligible_iso) %>% dplyr::pull(iso3c) %>% utils::head(min(min_required, length(eligible_iso)))

  allocate_once <- function(selected_iso, remaining_cap, unmet_by_consumer) {
    flow_rows <- list()
    for (j in names(unmet_by_consumer)) {
      remaining_j <- unmet_by_consumer[[j]]
      if (remaining_j <= 1e-9) next
      while (remaining_j > 1e-9) {
        cand <- selected_iso[remaining_cap[selected_iso] > 1e-9]
        if (!length(cand)) break
        wj <- edges_full %>%
          dplyr::filter(reporter_iso %in% cand, partner_iso == j) %>%
          dplyr::left_join(candidates %>% dplyr::select(iso3c, producer_score), by = c("reporter_iso" = "iso3c")) %>%
          dplyr::mutate(weight = pmax(0, edge_weight * producer_score)) %>%
          dplyr::select(reporter_iso, weight)
        if (!isTRUE(allow_self)) wj <- wj %>% dplyr::filter(reporter_iso != j)
        if (!nrow(wj)) break
        if (sum(wj$weight, na.rm = TRUE) <= 0) wj$weight <- 1
        wj <- wj %>% dplyr::mutate(weight = weight / sum(weight, na.rm = TRUE))
        allocated_any <- FALSE
        for (k in seq_len(nrow(wj))) {
          i <- wj$reporter_iso[[k]]
          alloc <- min(remaining_j * wj$weight[[k]], remaining_cap[[i]])
          if (alloc <= 1e-9) next
          flow_rows[[length(flow_rows) + 1]] <- tibble::tibble(reporter_iso = i, partner_iso = j, flow_usd = alloc)
          remaining_j <- remaining_j - alloc
          remaining_cap[[i]] <- remaining_cap[[i]] - alloc
          allocated_any <- TRUE
        }
        if (!allocated_any) break
      }
      unmet_by_consumer[[j]] <- remaining_j
    }
    list(flows = dplyr::bind_rows(flow_rows), remaining_cap = remaining_cap, unmet = unmet_by_consumer)
  }

  unmet <- D
  all_flows <- tibble::tibble(reporter_iso = character(), partner_iso = character(), flow_usd = numeric())
  cursor <- length(selected_iso)
  repeat {
    alloc <- allocate_once(selected_iso, remaining_cap, unmet)
    if (nrow(alloc$flows) > 0) all_flows <- dplyr::bind_rows(all_flows, alloc$flows)
    remaining_cap <- alloc$remaining_cap
    unmet <- alloc$unmet
    if (sum(unmet, na.rm = TRUE) <= 1e-6) break
    if (sum(remaining_cap, na.rm = TRUE) <= 1e-9 || cursor >= nrow(candidates)) break
    cursor <- cursor + 1
    next_iso <- candidates$iso3c[[cursor]]
    if (next_iso %in% eligible_iso) selected_iso <- unique(c(selected_iso, next_iso))
  }

  flows <- all_flows %>%
    dplyr::group_by(reporter_iso, partner_iso) %>%
    dplyr::summarize(flow_usd = sum(flow_usd, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(flow_share = flow_usd / target_total_usd) %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"))

  prod <- flows %>% dplyr::group_by(reporter_iso) %>% dplyr::summarize(production_usd = sum(flow_usd, na.rm = TRUE), .groups = "drop") %>% dplyr::rename(iso3c = reporter_iso)
  specialization <- candidates %>%
    dplyr::left_join(prod, by = "iso3c") %>%
    dplyr::mutate(
      selected = iso3c %in% selected_iso,
      production_usd = dplyr::coalesce(production_usd, 0),
      production_share = production_usd / target_total_usd,
      cap0_exports_usd = as.numeric(cap0[iso3c]),
      capMax_exports_usd = as.numeric(capMax[iso3c]),
      build_max_usd = as.numeric(build_max[iso3c]),
      build_needed_usd = pmax(0, production_usd - cap0_exports_usd)
    ) %>%
    dplyr::select(iso3c, country, selected, production_usd, production_share, cap0_exports_usd, capMax_exports_usd, build_max_usd, build_needed_usd, demand_weight, producer_score, dev_potential, connectivity_out, dplyr::any_of(allied_network_node_extra_cols()))

  objective <- sum(flows$flow_usd * (w_node * specialization$producer_score[match(flows$reporter_iso, specialization$iso3c)] + w_edge * dplyr::coalesce(flows$edge_weight, 0)), na.rm = TRUE) +
    sum(w_dev * specialization$dev_potential * as.numeric(specialization$selected), na.rm = TRUE)

  list(
    specialization = specialization,
    flows = flows %>% dplyr::select(reporter_iso, partner_iso, flow_usd, flow_share, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index"))),
    objective = objective,
    hhi = allied_network_hhi(specialization$production_share),
    n_producers = sum(specialization$selected, na.rm = TRUE),
    unmet_demand_usd = sum(unmet, na.rm = TRUE)
  )
}

# ---- Main orchestration ------------------------------------------------------

# Top-K producers of each stage. rank_in_stage is the dense rank of the share (tied shares share a
# rank, as reported before); membership of the top K uses a strict rank (ties broken by producer
# score, then ISO3) so that a stage contributes at most K appearances. With interior shares (on the
# MILP's 0.01 tangent grid) ties are common and the dense rank would count every tied producer.
allied_network_build_topk_tbl <- function(specialization_tbl, portfolio_top_k = 5) {
  if (is.null(specialization_tbl) || !nrow(specialization_tbl)) return(tibble::tibble())
  strict_rank <- function(share, score, iso) {
    o <- order(-dplyr::coalesce(share, 0), -dplyr::coalesce(score, -Inf), iso)
    r <- integer(length(o))
    r[o] <- seq_along(o)
    r
  }
  specialization_tbl %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      rank_in_stage = dplyr::dense_rank(dplyr::desc(dplyr::coalesce(production_share, 0))),
      topk_rank = strict_rank(production_share, producer_score, iso3c),
      in_top_k = topk_rank <= portfolio_top_k & dplyr::coalesce(production_share, 0) > 0
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(tech, supply_chain, iso3c, production_share, dplyr::any_of(c('production_usd', 'producer_score')), rank_in_stage, topk_rank, in_top_k)
}

# Number of stages in which each coalition member is a top-K producer. Counts distinct stages, not
# rows, so a duplicated specialization row can never inflate a count.
allied_network_count_topk <- function(topk_tbl, iso3c_network, cap_by_iso) {
  counted <- if (nrow(topk_tbl)) {
    topk_tbl %>%
      dplyr::filter(in_top_k) %>%
      dplyr::distinct(tech, supply_chain, iso3c) %>%
      dplyr::count(iso3c, name = 'topk_count')
  } else {
    tibble::tibble(iso3c = character(), topk_count = integer())
  }
  counted %>%
    dplyr::right_join(tibble::tibble(iso3c = as.character(iso3c_network)), by = 'iso3c') %>%
    dplyr::mutate(
      topk_count = dplyr::coalesce(topk_count, 0L),
      portfolio_cap = as.integer(cap_by_iso[iso3c]),
      over_by = pmax(0L, topk_count - portfolio_cap)
    )
}

allied_network_design <- function(economic_opportunity_index,
                                  energy_security_index,
                                  policy_index,
                                  partner_friendshore_tbl,
                                  partner_opportunity_tbl,
                                  country_info,
                                  iso3c_network = allied_network_default_iso3c(),
                                  partner_development_country_tbl = NULL,
                                  techs = NULL,
                                  node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
                                  edge_weights = list(friendshore = 0.5, opportunity = 0.5),
                                  method = c("auto", "milp", "greedy"),
                                  stage_targets_tbl = NULL,
                                  scale_caps_tbl = NULL,
                                  scale_mode = c("shares", "usd_target_match_china"),
                                  build_cost_mode = c("none", "flat", "dev_potential"),
                                  build_cost_weight = 0,
                                  country_size_tbl = NULL,
                                  demand_mode = c("need", "size", "mixed", "equal"),
                                  demand_weights = list(need = 0.5, size = 0.5),
                                  size_transform = c("log_scurve", "scurve", "rank"),
                                  portfolio_enable = FALSE,
                                  portfolio_top_k = 5,
                                  portfolio_min_cap = 2,
                                  portfolio_max_cap = 10,
                                  portfolio_transform = c("log", "linear"),
                                  portfolio_max_iters = 10,
                                  portfolio_verbose = FALSE,
                                  portfolio_caps_tbl = NULL,
                                  # constraints / knobs
                                  min_producers = 3,
                                  max_share = 0.40,
                                  min_share = 0.05,
                                  min_suppliers_per_consumer = 2,
                                  epsilon_supplier_share = 0.10,
                                  allow_self = TRUE,
                                  # objective weights
                                  w_node = 1.0,
                                  w_edge = 0.5,
                                  w_dev = 0.0,
                                  progress_callback = NULL,
                                  # "auto" uses the MILP for stages with at most this many members and
                                  # greedy above it (and as the fallback). The default 30 dates from the
                                  # 23-member coalition; scripts/30 passes 60 so that every stage of the
                                  # derived coalition (31 alignment members, 57 in the draft two-tier
                                  # coalition) is solved by the MILP.
                                  auto_milp_max_nodes = 30,
                                  milp_stage_time_limit_sec = 120,
                                  # policy term in the producer (node) score: TRUE (default) is the legacy
                                  # EO/policy/ES score; FALSE scores nodes on EO and ES only and does not
                                  # read policy_index (see allied_network_prepare_nodes)
                                  node_policy = TRUE,
                                  # missing-data rules (see allied_network_prepare_nodes)
                                  policy_missing = c("coalition_median", "none"),
                                  missing_components = c("renormalize", "require_all"),
                                  friendshore_orientation = c("reporter_is_consumer", "reporter_is_producer"),
                                  # stages solved, matched case-insensitively (e.g. c("Upstream",
                                  # "Midstream")); NULL solves every stage in the inputs (Upstream,
                                  # Midstream and Downstream in the shipped data, and any other label a
                                  # caller uses). The portfolio caps count appearances over the solved
                                  # stages only.
                                  supply_chains = NULL,
                                  # diversification: subtract diversity_lambda * HHI per stage (score
                                  # units per unit of HHI; 0 = the linear legacy design). The MILP uses
                                  # tangent cuts at this share step (error <= step^2 / 4 per producer).
                                  diversity_lambda = 0,
                                  diversity_breakpoint_step = 0.01,
                                  # optional per-member tier and network_node (coalition_members(), plus
                                  # allied_network_add_node_members()). Members sharing a network_node
                                  # are solved as one node ("Network nodes" above); without it, or with
                                  # every member its own node, nothing is aggregated.
                                  network_members = NULL,
                                  # member weights of the bundled nodes per (tech, stage): network_node,
                                  # iso3 (or iso3c), tech, supply_chain, weight (eu_stage_weights_tbl.rds);
                                  # required when network_members bundles members
                                  node_member_weights = NULL,
                                  # optional member-level basket trade for the bundled nodes' edge
                                  # weights: exporter_iso, importer_iso, tech, supply_chain, value
                                  # (allied_network_member_trade()); without it the stage weights apply
                                  node_trade_tbl = NULL,
                                  # optional display names of bundled nodes, e.g. c(EUU = "European Union")
                                  node_labels = NULL) {
  method <- match.arg(method)
  scale_mode <- match.arg(scale_mode)
  build_cost_mode <- match.arg(build_cost_mode)
  demand_mode <- match.arg(demand_mode)
  size_transform <- match.arg(size_transform)
  portfolio_transform <- match.arg(portfolio_transform)
  policy_missing <- match.arg(policy_missing)
  missing_components <- match.arg(missing_components)
  friendshore_orientation <- match.arg(friendshore_orientation)
  iso3c_network <- unique(as.character(iso3c_network))
  node_policy <- allied_network_check_node_policy(node_policy)
  network_members <- allied_network_check_members(network_members, iso3c_network)
  diversity_lambda <- allied_network_check_lambda(diversity_lambda)
  if (diversity_lambda > 0) allied_network_hhi_breakpoints(max_share, diversity_breakpoint_step)  # validates the step early
  if (!is.null(supply_chains)) {
    supply_chains <- unique(as.character(supply_chains))
    if (!length(supply_chains) || anyNA(supply_chains)) stop("supply_chains must be NULL or name at least one supply-chain stage.")
  }

  # Member-level raw inputs; the stage filter, the node aggregation and the scoring follow. Node
  # scores and demand are built within each (tech, stage), so dropping stages first leaves the
  # remaining stages unchanged; it only removes the dropped stages from the portfolio counts.
  raw_nodes <- allied_network_node_inputs(
    economic_opportunity_index = economic_opportunity_index,
    energy_security_index = energy_security_index,
    policy_index = policy_index,
    country_info = country_info,
    iso3c_network = iso3c_network,
    partner_development_country_tbl = partner_development_country_tbl,
    country_size_tbl = country_size_tbl,
    node_policy = node_policy
  )
  node_input_checks <- attr(raw_nodes, "input_checks")

  if (!is.null(techs)) {
    raw_nodes <- raw_nodes %>% dplyr::filter(tech %in% techs)
  }
  stages_excluded <- tibble::as_tibble(raw_nodes)[0, c("tech", "supply_chain")]
  if (!is.null(supply_chains)) {
    keep_stage <- tolower(raw_nodes$supply_chain) %in% tolower(supply_chains)
    stages_excluded <- raw_nodes[!keep_stage, ] %>%
      dplyr::distinct(tech, supply_chain) %>%
      dplyr::arrange(tech, supply_chain)
    raw_nodes <- raw_nodes[keep_stage, ]
  }

  # Network nodes: every member its own node, or members bundled into shared nodes
  aggregate_nodes <- allied_network_aggregates(network_members)
  member_weights <- NULL
  node_size_tbl <- NULL
  if (aggregate_nodes) {
    member_weights <- allied_network_member_weights(network_members, node_member_weights, dplyr::distinct(raw_nodes, tech, supply_chain))
    if (!is.null(country_size_tbl)) node_size_tbl <- allied_network_node_size_tbl(country_size_tbl, network_members)
    raw_nodes <- allied_network_aggregate_node_inputs(raw_nodes, network_members, member_weights, node_labels, country_info, node_size_tbl)
    node_tbl <- allied_network_node_table(network_members)
    iso3c_nodes <- node_tbl$network_node
  } else {
    iso3c_nodes <- iso3c_network
  }

  nodes <- allied_network_score_nodes(
    raw_nodes,
    node_weights = node_weights,
    demand_mode = demand_mode,
    demand_weights = demand_weights,
    size_transform = size_transform,
    policy_missing = policy_missing,
    missing_components = missing_components,
    node_policy = node_policy
  )
  rm(raw_nodes)

  edges <- allied_network_extract_edges(
    partner_friendshore_tbl = partner_friendshore_tbl,
    partner_opportunity_tbl = partner_opportunity_tbl,
    iso3c_network = iso3c_network,
    edge_weights = edge_weights,
    keep_components = TRUE,
    friendshore_orientation = friendshore_orientation,
    network_members = if (aggregate_nodes) network_members else NULL,
    member_weights = member_weights,
    node_trade_tbl = if (aggregate_nodes && !is.null(node_trade_tbl)) allied_network_member_trade(node_trade_tbl, iso3c_network) else NULL
  )
  input_checks <- dplyr::bind_rows(node_input_checks, attr(edges, "input_checks"))

  stages <- nodes %>%
    dplyr::distinct(tech, supply_chain) %>%
    dplyr::arrange(tech, supply_chain)

  if (nrow(stages) == 0) stop("No (tech, supply_chain) stages available after filtering.")
  if (diversity_lambda > 0 && identical(scale_mode, "usd_target_match_china") && !is.null(stage_targets_tbl) && !is.null(scale_caps_tbl)) {
    stop("diversity_lambda > 0 is implemented for the share-based solvers only (scale_mode = 'shares').")
  }

  has_milp <- allied_network_has_milp()
  use_method <- if (method == 'auto') if (has_milp) 'milp' else 'greedy' else method

  nodes_selected <- nodes %>%
    dplyr::select(
      tech, supply_chain, iso3c, country, producer_score, demand_weight, dev_potential,
      dplyr::any_of(allied_network_node_extra_cols())
    )
  edges_selected <- edges %>%
    dplyr::select(tech, supply_chain, reporter_iso, partner_iso, edge_weight, dplyr::any_of(c('friendshore_index', 'opportunity_index')))

  stage_keys <- paste(stages$tech, stages$supply_chain, sep = '||')
  names(stage_keys) <- stage_keys
  node_keys <- paste(nodes_selected$tech, nodes_selected$supply_chain, sep = '||')
  edge_keys <- paste(edges_selected$tech, edges_selected$supply_chain, sep = '||')
  nodes_by_stage <- split(nodes_selected, node_keys, drop = TRUE)
  edges_by_stage <- split(edges_selected, edge_keys, drop = TRUE)
  stage_count <- length(stage_keys)
  start_time <- Sys.time()

  producer_forbidden_by_stage <- stats::setNames(vector('list', length(stage_keys)), stage_keys)
  for (k in names(producer_forbidden_by_stage)) producer_forbidden_by_stage[[k]] <- character(0)

  cap_by_iso <- NULL
  caps_tbl <- NULL
  if (isTRUE(portfolio_enable)) {
    if (!is.null(portfolio_caps_tbl)) {
      require_columns(portfolio_caps_tbl, c('iso3c', 'portfolio_cap'), label = 'portfolio_caps_tbl')
      # keyed by node code when members are bundled
      cap_by_iso <- portfolio_caps_tbl %>%
        dplyr::transmute(iso3c = as.character(iso3c), portfolio_cap = as.integer(portfolio_cap)) %>%
        dplyr::filter(iso3c %in% iso3c_nodes) %>%
        dplyr::distinct(iso3c, .keep_all = TRUE)
      cap_by_iso <- stats::setNames(cap_by_iso$portfolio_cap, cap_by_iso$iso3c)
    } else {
      if (is.null(country_size_tbl)) stop('portfolio_enable=TRUE requires country_size_tbl or portfolio_caps_tbl.')
      # a bundled node's cap comes from its members' summed GDP
      cap_by_iso <- allied_network_compute_portfolio_caps(
        country_size_tbl = if (aggregate_nodes) node_size_tbl else country_size_tbl,
        iso3c_network = iso3c_nodes,
        min_cap = portfolio_min_cap,
        max_cap = portfolio_max_cap,
        transform = portfolio_transform,
        clamp = TRUE
      )
    }

    gdp_tbl <- nodes_selected %>%
      dplyr::distinct(iso3c, gdp_usd, gdp_imputed)
    caps_tbl <- tibble::tibble(iso3c = iso3c_nodes) %>%
      dplyr::mutate(portfolio_cap = as.integer(cap_by_iso[iso3c])) %>%
      dplyr::left_join(gdp_tbl, by = 'iso3c')
  }

  solve_one_stage <- function(stage_idx) {
    t <- stages$tech[[stage_idx]]
    sc <- stages$supply_chain[[stage_idx]]
    key <- stage_keys[[stage_idx]]

    nodes_stage <- nodes_by_stage[[key]]
    skipped <- function(reason) {
      list(
        tech = t, supply_chain = sc,
        specialization = NULL, flows = NULL,
        objective = NA_real_, hhi = NA_real_, n_producers = NA_integer_, method_used = reason,
        fallback_reason = NA_character_, max_production_share = NA_real_,
        target_exports_usd = NA_real_, allies_cap0_usd = NA_real_, allies_capMax_usd = NA_real_,
        feasible_caps = NA, unmet_demand_usd = NA_real_
      )
    }
    n_stage_nodes <- if (is.null(nodes_stage)) 0L else dplyr::n_distinct(nodes_stage$iso3c)
    if (n_stage_nodes < 3) return(skipped('skipped_too_few_nodes'))
    # No member has any node component: any producer set would be arbitrary (it used to be the first
    # three ISO3 codes alphabetically), so the stage is reported as skipped instead.
    if (!any(is.finite(nodes_stage$producer_score))) return(skipped('skipped_no_node_signal'))

    edges_stage <- edges_by_stage[[key]]
    if (is.null(edges_stage)) edges_stage <- edges_selected[0, , drop = FALSE]

    nodes_stage_core <- nodes_stage %>% dplyr::select(iso3c, country, producer_score, demand_weight, dev_potential, dplyr::any_of(allied_network_node_extra_cols()))
    edges_stage_core <- edges_stage %>% dplyr::select(reporter_iso, partner_iso, edge_weight, dplyr::any_of(c('friendshore_index', 'opportunity_index')))

    stage_method <- use_method
    target_total_usd <- NA_real_
    allies_cap0_usd <- NA_real_
    allies_capMax_usd <- NA_real_
    feasible_caps <- NA

    scaled_enabled <- identical(scale_mode, 'usd_target_match_china') && !is.null(stage_targets_tbl) && !is.null(scale_caps_tbl)
    caps <- NULL
    if (scaled_enabled) {
      target_total_usd <- allied_network_get_stage_target(stage_targets_tbl, t, sc)
      caps <- allied_network_get_scale_caps(scale_caps_tbl, t, sc, nodes_stage_core$iso3c)
      allies_cap0_usd <- sum(caps$cap0, na.rm = TRUE)
      allies_capMax_usd <- sum(caps$capMax, na.rm = TRUE)
      feasible_caps <- allies_capMax_usd >= target_total_usd
      if (!is.finite(target_total_usd) || target_total_usd <= 0) {
        return(list(
          tech = t, supply_chain = sc,
          specialization = NULL, flows = NULL,
          objective = NA_real_, hhi = NA_real_, n_producers = NA_integer_, method_used = 'skipped_missing_target',
          target_exports_usd = target_total_usd, allies_cap0_usd = allies_cap0_usd,
          allies_capMax_usd = allies_capMax_usd, feasible_caps = feasible_caps, unmet_demand_usd = NA_real_
        ))
      }
    }

    if (method == 'auto') stage_method <- if (has_milp && n_stage_nodes <= auto_milp_max_nodes) 'milp' else 'greedy'
    forbidden <- producer_forbidden_by_stage[[key]]
    fallback_reason <- NA_character_

    solve_greedy <- function() {
      if (scaled_enabled) {
        allied_network_solve_stage_greedy_scaled(
          nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
          target_total_usd = target_total_usd, cap0_usd = caps$cap0, capMax_usd = caps$capMax,
          build_max_usd = caps$build_max, min_producers = min_producers, max_share = max_share,
          min_share = min_share, min_suppliers_per_consumer = min_suppliers_per_consumer,
          epsilon_supplier_share = epsilon_supplier_share, w_edge = w_edge, w_node = w_node,
          w_dev = w_dev, allow_self = allow_self, producer_forbidden = forbidden
        )
      } else {
        allied_network_solve_stage_greedy(
          nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
          min_producers = min_producers, max_share = max_share, min_share = min_share,
          min_suppliers_per_consumer = min_suppliers_per_consumer,
          epsilon_supplier_share = epsilon_supplier_share,
          w_edge = w_edge, w_node = w_node, w_dev = w_dev, allow_self = allow_self,
          producer_forbidden = forbidden, diversity_lambda = diversity_lambda
        )
      }
    }

    if (stage_method == 'milp') {
      setTimeLimit(cpu = milp_stage_time_limit_sec, elapsed = milp_stage_time_limit_sec, transient = TRUE)
      sol <- tryCatch({
        if (scaled_enabled) {
          allied_network_solve_stage_milp_scaled(
            nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
            target_total_usd = target_total_usd, cap0_usd = caps$cap0, capMax_usd = caps$capMax,
            build_max_usd = caps$build_max, build_cost_mode = build_cost_mode,
            build_cost_weight = build_cost_weight, min_producers = min_producers, max_share = max_share,
            min_share = min_share, min_suppliers_per_consumer = min_suppliers_per_consumer,
            epsilon_supplier_share = epsilon_supplier_share, w_edge = w_edge, w_node = w_node,
            w_dev = w_dev, allow_self = allow_self, producer_forbidden = forbidden
          )
        } else {
          allied_network_solve_stage_milp(
            nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
            min_producers = min_producers, max_share = max_share, min_share = min_share,
            min_suppliers_per_consumer = min_suppliers_per_consumer,
            epsilon_supplier_share = epsilon_supplier_share, w_edge = w_edge,
            w_node = w_node, w_dev = w_dev, allow_self = allow_self,
            producer_forbidden = forbidden, time_limit_sec = milp_stage_time_limit_sec,
            diversity_lambda = diversity_lambda, diversity_breakpoint_step = diversity_breakpoint_step
          )
        }
      }, error = function(e) {
        # Lift the limit first: once elapsed, it would also abort the greedy fallback
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
        stage_method <<- 'greedy'
        fallback_reason <<- conditionMessage(e)
        if (is.function(progress_callback)) {
          progress_callback(list(event = 'fallback_greedy', tech = t, supply_chain = sc, reason = fallback_reason))
        }
        solve_greedy()
      })
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    } else {
      sol <- solve_greedy()
    }

    share_vec <- sol$specialization$production_share
    # Realized redundancy, so a greedy stage can be checked against the MILP's supplier rules
    redundancy <- allied_network_supplier_stats(sol$flows, sol$specialization)
    terms <- if (scaled_enabled) NULL else allied_network_objective_terms(sol$specialization, sol$flows, w_node, w_edge, w_dev, diversity_lambda)
    list(
      tech = t,
      supply_chain = sc,
      specialization = sol$specialization,
      flows = sol$flows,
      objective = sol$objective,
      hhi = sol$hhi,
      n_producers = sol$n_producers,
      method_used = stage_method,
      fallback_reason = fallback_reason,
      max_production_share = if (length(share_vec)) max(share_vec, na.rm = TRUE) else NA_real_,
      n_at_max_share = sum(share_vec >= max_share - 1e-6, na.rm = TRUE),
      objective_node = if (is.null(terms)) NA_real_ else terms$node,
      objective_edge = if (is.null(terms)) NA_real_ else terms$edge,
      objective_dev = if (is.null(terms)) NA_real_ else terms$dev,
      diversity_penalty = if (is.null(terms)) NA_real_ else terms$diversity_penalty,
      hhi_tangent_max_error = if (is.null(sol$hhi_tangent_max_error)) NA_real_ else sol$hhi_tangent_max_error,
      min_suppliers_realized = redundancy$min_suppliers,
      min_supplier_share_realized = redundancy$min_supplier_share,
      # members the portfolio-cap loop barred from producing in this stage (the instance solved)
      producers_forbidden = paste(sort(forbidden), collapse = " "),
      target_exports_usd = target_total_usd,
      allies_cap0_usd = allies_cap0_usd,
      allies_capMax_usd = allies_capMax_usd,
      feasible_caps = feasible_caps,
      unmet_demand_usd = dplyr::coalesce(sol$unmet_demand_usd, NA_real_)
    )
  }

  run_all_stages <- function(indices) {
    out <- vector('list', stage_count)
    if (is.null(indices)) indices <- seq_len(stage_count)
    for (i in indices) {
      if (is.function(progress_callback)) {
        progress_callback(list(event = 'start_stage', current = i, total = stage_count, tech = stages$tech[[i]], supply_chain = stages$supply_chain[[i]], method = use_method))
      }
      out[[i]] <- solve_one_stage(i)
      if (is.function(progress_callback)) {
        elapsed_done <- as.numeric(difftime(Sys.time(), start_time, units = 'secs'))
        done_count <- i
        eta_done <- if (done_count <= 0) NA_real_ else (elapsed_done / done_count) * (stage_count - done_count)
        progress_callback(list(event = 'end_stage', current = i, total = stage_count, tech = stages$tech[[i]], supply_chain = stages$supply_chain[[i]], elapsed_sec = elapsed_done, eta_sec = eta_done, pct_complete = done_count / stage_count, pct_remaining = 1 - (done_count / stage_count), method = out[[i]]$method_used))
      }
    }
    out
  }

  results <- run_all_stages(seq_len(stage_count))

  portfolio_converged <- TRUE
  portfolio_iters <- 0
  portfolio_violations_remaining <- 0
  counts_tbl <- NULL
  topk_tbl <- tibble::tibble()

  if (isTRUE(portfolio_enable)) {
    for (iter in seq_len(portfolio_max_iters)) {
      portfolio_iters <- iter
      specialization_tbl_iter <- purrr::map_dfr(results, function(x) {
        if (is.null(x$specialization)) return(NULL)
        x$specialization %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = iso3c)
      })
      topk_tbl <- allied_network_build_topk_tbl(specialization_tbl_iter, portfolio_top_k = portfolio_top_k)
      counts_tbl <- allied_network_count_topk(topk_tbl, iso3c_nodes, cap_by_iso)

      offenders <- counts_tbl %>% dplyr::filter(over_by > 0)
      if (!nrow(offenders)) {
        portfolio_converged <- TRUE
        break
      }
      portfolio_converged <- FALSE

      impacted <- integer(0)
      for (r in seq_len(nrow(offenders))) {
        offender_iso <- offenders$iso3c[[r]]
        over <- offenders$over_by[[r]]
        cand <- topk_tbl %>%
          dplyr::filter(in_top_k, iso3c == offender_iso) %>%
          dplyr::distinct(tech, supply_chain, .keep_all = TRUE)
        priority_base <- if ('production_usd' %in% names(cand)) dplyr::coalesce(cand$production_usd, cand$production_share) else cand$production_share
        cand <- cand %>%
          dplyr::mutate(
            producer_score = dplyr::coalesce(producer_score, 1),
            drop_priority = priority_base * producer_score
          ) %>%
          dplyr::arrange(drop_priority)
        if (!nrow(cand)) next
        drops <- utils::head(cand, over)
        for (k in seq_len(nrow(drops))) {
          stg_key <- paste(drops$tech[[k]], drops$supply_chain[[k]], sep = '||')
          producer_forbidden_by_stage[[stg_key]] <- unique(c(producer_forbidden_by_stage[[stg_key]], offender_iso))
          impacted <- unique(c(impacted, match(stg_key, stage_keys)))
        }
      }

      if (!length(impacted)) break
      for (idx in impacted) results[[idx]] <- solve_one_stage(idx)
      if (isTRUE(portfolio_verbose)) message('portfolio iteration ', iter, ': re-solved ', length(impacted), ' stages')
    }

    topk_tbl <- allied_network_build_topk_tbl(
      purrr::map_dfr(results, function(x) if (is.null(x$specialization)) NULL else x$specialization %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = iso3c)),
      portfolio_top_k = portfolio_top_k
    )
    counts_tbl <- allied_network_count_topk(topk_tbl, iso3c_nodes, cap_by_iso)
    portfolio_violations_remaining <- sum(counts_tbl$over_by > 0, na.rm = TRUE)
    if (portfolio_violations_remaining == 0) portfolio_converged <- TRUE
  }

  specialization_tbl <- purrr::map_dfr(results, function(x) {
    if (is.null(x$specialization)) return(NULL)
    x$specialization %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = iso3c)
  })

  flows_tbl <- purrr::map_dfr(results, function(x) {
    if (is.null(x$flows)) return(NULL)
    x$flows %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = reporter_iso)
  })

  or_na <- function(v, na) if (is.null(v) || !length(v)) na else v
  method_requested <- method
  # Upper bound on the MILP's tangent error per producer; the realized error is reported per stage
  # (NA when the stage was not solved by the MILP with diversity_lambda > 0)
  tangent_bound <- if (diversity_lambda > 0) max(diff(allied_network_hhi_breakpoints(max_share, diversity_breakpoint_step)))^2 / 4 else NA_real_
  diagnostics_tbl <- purrr::map_dfr(results, function(x) {
    out <- tibble::tibble(
      tech = x$tech, supply_chain = x$supply_chain,
      method = dplyr::coalesce(x$method_used, use_method), method_requested = method_requested,
      fallback_reason = or_na(x$fallback_reason, NA_character_),
      producers_forbidden = or_na(x$producers_forbidden, NA_character_),
      n_producers = x$n_producers, hhi = x$hhi, objective = x$objective,
      max_production_share = or_na(x$max_production_share, NA_real_),
      min_suppliers_realized = or_na(x$min_suppliers_realized, NA_integer_),
      min_supplier_share_realized = or_na(x$min_supplier_share_realized, NA_real_),
      diversity_lambda = diversity_lambda,
      node_policy = node_policy,
      effective_producers = 1 / x$hhi,
      n_at_max_share = or_na(x$n_at_max_share, NA_integer_),
      objective_node = or_na(x$objective_node, NA_real_),
      objective_edge = or_na(x$objective_edge, NA_real_),
      objective_dev = or_na(x$objective_dev, NA_real_),
      diversity_penalty = or_na(x$diversity_penalty, NA_real_),
      hhi_tangent_max_error = or_na(x$hhi_tangent_max_error, NA_real_),
      hhi_tangent_error_bound = if (identical(x$method_used, "milp")) tangent_bound else NA_real_
    )
    if (identical(scale_mode, 'usd_target_match_china')) {
      out <- out %>% dplyr::mutate(target_exports_usd = x$target_exports_usd, allies_cap0_usd = x$allies_cap0_usd, allies_capMax_usd = x$allies_capMax_usd, feasible_caps = x$feasible_caps, unmet_demand_usd = x$unmet_demand_usd)
    }
    out
  })

  # Node-data flags per stage. Policy imputation changes the producer set in a large share of stages
  # (critic G7), so the imputed count and the production share held by imputed members are reported.
  stage_flags <- nodes_selected %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarise(
      n_nodes = dplyr::n_distinct(iso3c),
      n_nodes_no_score = sum(is.na(producer_score)),
      n_nodes_partial_score = sum(dplyr::between(node_components_n, 1L, 2L)),
      n_policy_imputed = sum(policy_imputed),
      n_demand_need_missing = sum(demand_need_missing),
      .groups = 'drop'
    )
  diagnostics_tbl <- diagnostics_tbl %>% dplyr::left_join(stage_flags, by = c('tech', 'supply_chain'))
  if (!is.null(specialization_tbl) && nrow(specialization_tbl)) {
    selection_flags <- specialization_tbl %>%
      dplyr::group_by(tech, supply_chain) %>%
      dplyr::summarise(
        n_selected_policy_imputed = sum(selected & policy_imputed, na.rm = TRUE),
        share_policy_imputed = sum(production_share[policy_imputed], na.rm = TRUE),
        .groups = 'drop'
      )
    diagnostics_tbl <- diagnostics_tbl %>% dplyr::left_join(selection_flags, by = c('tech', 'supply_chain'))
  }

  if (isTRUE(portfolio_enable) && nrow(specialization_tbl)) {
    specialization_tbl <- specialization_tbl %>%
      dplyr::left_join(counts_tbl %>% dplyr::select(iso3c, portfolio_topk_count = topk_count, portfolio_cap), by = 'iso3c')
    diagnostics_tbl <- diagnostics_tbl %>%
      dplyr::mutate(
        portfolio_converged = portfolio_converged,
        portfolio_iters = portfolio_iters,
        portfolio_violations_remaining = portfolio_violations_remaining
      )
  }

  build_candidates_tbl <- NULL
  if (!is.null(partner_development_country_tbl) && NROW(specialization_tbl) > 0) {
    build_candidates_tbl <- specialization_tbl %>%
      dplyr::group_by(tech, supply_chain) %>%
      dplyr::mutate(
        build_priority = dev_potential * (0.6 * producer_score + 0.4 * dplyr::coalesce(connectivity_out, 0)),
        build_priority = dplyr::if_else(is.na(build_priority), 0, build_priority)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(build_priority)) %>%
      dplyr::select(tech, supply_chain, iso3c, country, dev_potential, producer_score, dplyr::any_of('connectivity_out'), selected, production_share, build_priority)
  }

  # Network node of every row (tier, network_node, n_members after iso3c): with bundled nodes each
  # row is a node (iso3c = network_node, tier = the node's tier); otherwise each member's own tier.
  # member_shares splits the node shares back to the members (derived for bundled nodes).
  member_shares_tbl <- NULL
  if (!is.null(network_members) && NROW(specialization_tbl) > 0) {
    node_info <- if (aggregate_nodes) {
      allied_network_node_table(network_members) %>%
        dplyr::transmute(iso3c = network_node, tier, network_node, n_members)
    } else {
      network_members %>% dplyr::transmute(iso3c, tier, network_node, n_members = 1L)
    }
    specialization_tbl <- specialization_tbl %>%
      dplyr::select(-dplyr::any_of(c("tier", "network_node", "n_members"))) %>%
      dplyr::left_join(node_info, by = "iso3c", relationship = "many-to-one") %>%
      dplyr::relocate(tier, network_node, n_members, .after = iso3c)
    share_weights <- if (aggregate_nodes) {
      member_weights
    } else {
      allied_network_member_weights(network_members, NULL, stages)
    }
    member_shares_tbl <- allied_network_disaggregate_shares(specialization_tbl, network_members, share_weights, country_info)
  }

  out <- list(
    params = list(
      # the coalition members whose inputs were read
      iso3c_network = iso3c_network,
      # tier, network_node and node_tier per member as passed (NULL when not given)
      network_members = network_members,
      # the nodes solved (the members themselves unless some share a network_node) and whether
      # members were aggregated into nodes
      network_nodes = iso3c_nodes,
      node_aggregation = aggregate_nodes,
      node_weights = node_weights,
      # policy term in the producer score, and the node weights as applied (normalized; policy 0
      # when node_policy = FALSE)
      node_policy = node_policy,
      node_weights_effective = as.list(allied_network_normalize_weights(c(
        eo = node_weights$eo,
        policy = if (node_policy) node_weights$policy else 0,
        resilience = node_weights$resilience
      ))),
      edge_weights = edge_weights,
      # `method` is what was requested; what each stage actually ran is diagnostics$method
      method = method,
      methods_used = sort(unique(diagnostics_tbl$method)),
      milp_available = has_milp,
      policy_missing = policy_missing,
      missing_components = missing_components,
      friendshore_orientation = friendshore_orientation,
      scale_mode = scale_mode,
      build_cost_mode = build_cost_mode,
      build_cost_weight = build_cost_weight,
      demand_mode = demand_mode,
      demand_weights = demand_weights,
      size_transform = size_transform,
      portfolio_enable = portfolio_enable,
      portfolio_top_k = portfolio_top_k,
      portfolio_min_cap = portfolio_min_cap,
      portfolio_max_cap = portfolio_max_cap,
      portfolio_transform = portfolio_transform,
      min_producers = min_producers,
      max_share = max_share,
      min_share = min_share,
      min_suppliers_per_consumer = min_suppliers_per_consumer,
      epsilon_supplier_share = epsilon_supplier_share,
      allow_self = allow_self,
      w_node = w_node,
      w_edge = w_edge,
      w_dev = w_dev,
      auto_milp_max_nodes = auto_milp_max_nodes,
      milp_stage_time_limit_sec = milp_stage_time_limit_sec,
      diversity_lambda = diversity_lambda,
      diversity_breakpoint_step = diversity_breakpoint_step,
      hhi_tangent_error_bound = tangent_bound,
      supply_chains = supply_chains,
      # (tech, supply_chain) stages with nodes that the supply_chains filter left out
      stages_excluded = stages_excluded
    ),
    specialization = specialization_tbl,
    flows = flows_tbl,
    diagnostics = diagnostics_tbl,
    build_candidates = build_candidates_tbl,
    # rows in and keys out for each collapsed input (duplicates and conflicting duplicates)
    input_checks = input_checks,
    # one row per member and stage: the node's production share split by stage weight (NULL without
    # network_members); production_share_derived is derived, not designed, for bundled nodes
    member_shares = member_shares_tbl
  )

  if (aggregate_nodes) {
    out$node_aggregation <- list(
      # one row per node: tier, members, summed GDP
      nodes = allied_network_node_table(network_members) %>%
        dplyr::select(-node_order) %>%
        dplyr::left_join(
          if (is.null(node_size_tbl)) tibble::tibble(network_node = character(), gdp_usd = numeric(), gdp_imputed = logical()) else
            node_size_tbl %>% dplyr::select(network_node = iso3c, gdp_usd, gdp_imputed),
          by = "network_node"
        ),
      # member weights per stage (renormalised over the node's members)
      member_weights = member_weights,
      # node edges by component, stage and weight basis (trade / stage / equal / member), with the
      # member edges dropped inside a node
      edges = attr(edges, "edge_aggregation"),
      trade_supplied = !is.null(node_trade_tbl)
    )
  }

  if (isTRUE(portfolio_enable)) {
    out$portfolio <- list(
      caps = caps_tbl,
      counts = counts_tbl,
      topk = topk_tbl,
      converged = portfolio_converged,
      iters = portfolio_iters,
      violations_remaining = portfolio_violations_remaining
    )
  }

  out
}
