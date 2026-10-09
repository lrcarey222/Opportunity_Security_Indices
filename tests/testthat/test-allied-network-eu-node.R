repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

# Six members, four network nodes: USA, CAN and MEX are their own nodes; DEU, FRA and ITA form "EUU".
eu_iso <- c("DEU", "FRA", "ITA")
all_iso <- c("USA", "CAN", "MEX", eu_iso)
eu_country_info <- tibble::tibble(
  iso3c = all_iso,
  country = c("United States", "Canada", "Mexico", "Germany", "France", "Italy")
)
eu_members <- tibble::tibble(
  iso3 = all_iso,
  tier = c("core", "core", "partner", "core", "core", "core"),
  network_node = c("USA", "CAN", "MEX", "EUU", "EUU", "EUU")
)
stages <- c("Upstream", "Midstream")
eo_val <- c(USA = 0.9, CAN = 0.7, MEX = 0.5, DEU = 0.8, FRA = 0.6, ITA = 0.4)
es_val <- c(USA = 0.8, CAN = 0.6, MEX = 0.4, DEU = 0.5, FRA = 0.7, ITA = 0.3)
gdp_val <- c(USA = 3e13, CAN = 2e12, MEX = 1.8e12, DEU = 4.5e12, FRA = 3e12, ITA = 2.2e12)

# Stage weights as eu_stage_weights_tbl.rds carries them; FRA weighs 0 upstream (a non-producer).
eu_weights <- tibble::tibble(
  network_node = "EUU",
  iso3 = rep(eu_iso, 2),
  tech = "Solar",
  supply_chain = rep(stages, each = 3),
  weight = c(0.6, 0, 0.4, 0.5, 0.3, 0.2),
  basis = rep(c("production", "exports"), each = 3)
)

make_eu_inputs <- function() {
  grid <- tidyr::crossing(iso3c = all_iso, tech = "Solar", supply_chain = stages) %>%
    dplyr::mutate(Country = eu_country_info$country[match(iso3c, eu_country_info$iso3c)])
  eo <- grid %>% dplyr::transmute(Country, tech, supply_chain, Economic_Opportunity_Index = unname(eo_val[iso3c]))
  # ITA has no ES upstream: its value drops out of the EU mean
  es <- grid %>%
    dplyr::filter(!(iso3c == "ITA" & supply_chain == "Upstream")) %>%
    dplyr::transmute(Country, tech, supply_chain, Energy_Security_Index = unname(es_val[iso3c]))
  dyads <- tidyr::crossing(reporter_iso = all_iso, partner_iso = all_iso, tech = "Solar", supply_chain = stages) %>%
    dplyr::filter(reporter_iso != partner_iso) %>%
    dplyr::mutate(r = match(reporter_iso, all_iso), p = match(partner_iso, all_iso), s = match(supply_chain, stages))
  fs <- dyads %>%
    dplyr::mutate(category = "friendshore", variable = "Friendshore Index", data_type = "index",
                  value = 0.2 + 0.1 * ((r + 2 * p + s) %% 7), Year = 2024L) %>%
    dplyr::select(-r, -p, -s)
  op <- dyads %>%
    dplyr::mutate(category = "opportunity", variable = "Opportunity Index", data_type = "index",
                  value = 0.25 + 0.1 * ((2 * r + p + s) %% 6), Year = 2024L) %>%
    dplyr::select(-r, -p, -s)
  size <- tibble::tibble(iso3c = all_iso, gdp_usd = unname(gdp_val[all_iso]), gdp_imputed = FALSE)
  # Midstream basket trade (exporter -> importer); nothing upstream, so the stage weights apply there
  trade <- tibble::tribble(
    ~exporter_iso, ~importer_iso, ~value,
    "USA", "DEU", 10, "USA", "FRA", 30, "USA", "ITA", 0,
    "DEU", "CAN", 5, "FRA", "CAN", 0, "ITA", "CAN", 15,
    "DEU", "FRA", 99, "FRA", "ITA", 99
  ) %>% dplyr::mutate(tech = "Solar", supply_chain = "Midstream")
  list(eo = eo, es = es, fs = fs, op = op, size = size, trade = trade)
}

run_eu_design <- function(x, members = eu_members, weights = eu_weights, trade = x$trade, ...) {
  suppressWarnings(allied_network_design(
    economic_opportunity_index = x$eo,
    energy_security_index = x$es,
    policy_index = NULL,
    partner_friendshore_tbl = x$fs,
    partner_opportunity_tbl = x$op,
    country_info = eu_country_info,
    iso3c_network = all_iso,
    country_size_tbl = x$size,
    node_policy = FALSE,
    demand_mode = "mixed",
    network_members = members,
    node_member_weights = weights,
    node_trade_tbl = trade,
    node_labels = c(EUU = "European Union"),
    min_producers = 3,
    max_share = 0.40,
    min_share = 0.05,
    ...
  ))
}

lookup <- function(tbl, reporter, partner, stage) {
  tbl$value[tbl$reporter_iso == reporter & tbl$partner_iso == partner & tbl$supply_chain == stage]
}

test_that("bundled node inputs are stage-weighted means, need is GDP-weighted and GDP is summed", {
  x <- make_eu_inputs()
  nodes <- allied_network_prepare_nodes(
    x$eo, x$es, NULL, eu_country_info,
    iso3c_network = all_iso, country_size_tbl = x$size, demand_mode = "mixed", node_policy = FALSE,
    network_members = eu_members, node_member_weights = eu_weights, node_labels = c(EUU = "European Union")
  )
  expect_setequal(unique(nodes$iso3c), c("USA", "CAN", "MEX", "EUU"))
  expect_equal(nrow(nodes), 8)
  eu_mid <- nodes[nodes$iso3c == "EUU" & nodes$supply_chain == "Midstream", ]
  eu_up <- nodes[nodes$iso3c == "EUU" & nodes$supply_chain == "Upstream", ]
  expect_equal(eu_mid$country, "European Union")
  # Midstream: weights 0.5 / 0.3 / 0.2
  expect_equal(eu_mid$eo_raw, 0.5 * 0.8 + 0.3 * 0.6 + 0.2 * 0.4)
  expect_equal(eu_mid$es_raw, 0.5 * 0.5 + 0.3 * 0.7 + 0.2 * 0.3)
  # Upstream: FRA weighs 0; ITA has no ES, so the ES mean renormalises onto DEU alone
  expect_equal(eu_up$eo_raw, (0.6 * 0.8 + 0.4 * 0.4) / 1)
  expect_equal(eu_up$es_raw, 0.5)
  # GDP: summed over all members; need from the GDP-weighted members' ES (FRA counts: its GDP is not 0)
  expect_equal(eu_mid$gdp_usd, sum(gdp_val[eu_iso]))
  expect_equal(eu_up$gdp_usd, sum(gdp_val[eu_iso]))
  es_need_mid <- sum(gdp_val[eu_iso] * es_val[eu_iso]) / sum(gdp_val[eu_iso])
  es_need_up <- sum(gdp_val[c("DEU", "FRA")] * es_val[c("DEU", "FRA")]) / sum(gdp_val[c("DEU", "FRA")])
  # The S-curves run over the four nodes; need = 1 - S(es_need_raw) with the EU at its GDP-weighted ES
  mid <- nodes[nodes$supply_chain == "Midstream", ]
  es_need_vec <- ifelse(mid$iso3c == "EUU", es_need_mid, mid$es_raw)
  expect_equal(mid$need_idx, 1 - median_scurve(es_need_vec))
  expect_equal(mid$es_idx, median_scurve(mid$es_raw))
  expect_equal(mid$eo_idx, median_scurve(mid$eo_raw))
  expect_equal(mid$size_idx, median_scurve(log1p(mid$gdp_usd)))
  up <- nodes[nodes$supply_chain == "Upstream", ]
  expect_equal(up$need_idx, 1 - median_scurve(ifelse(up$iso3c == "EUU", es_need_up, up$es_raw)))
  # single-member nodes keep need = 1 - es_idx
  single <- nodes$iso3c != "EUU"
  expect_equal(nodes$need_idx[single], 1 - nodes$es_idx[single])
  expect_equal(sum(mid$demand_weight), 1)

  # The member weights are renormalised over the node's members; a member outside network_members drops out
  mw <- allied_network_member_weights(allied_network_check_members(eu_members, all_iso), eu_weights, tidyr::crossing(tech = "Solar", supply_chain = stages))
  expect_equal(mw$member_weight[mw$iso3c == "DEU" & mw$supply_chain == "Midstream"], 0.5)
  expect_equal(mw$member_weight[mw$iso3c == "USA"], c(1, 1))
  expect_equal(unique(mw$weight_basis[mw$iso3c == "USA"]), "own")
  extra <- dplyr::bind_rows(eu_weights, tibble::tibble(network_node = "EUU", iso3 = "ESP", tech = "Solar", supply_chain = "Midstream", weight = 1, basis = "exports"))
  mw2 <- allied_network_member_weights(allied_network_check_members(eu_members, all_iso), extra, tidyr::crossing(tech = "Solar", supply_chain = stages))
  expect_equal(mw2, mw)
})

test_that("a bundled node needs member weights for every solved stage", {
  x <- make_eu_inputs()
  expect_error(run_eu_design(x, weights = NULL, method = "greedy"), "node_member_weights .* is required")
  expect_error(
    run_eu_design(x, weights = eu_weights[eu_weights$supply_chain == "Midstream", ], method = "greedy"),
    "no positive weight for node EUU"
  )
  # a weights table limited to the solved stages is enough
  out <- run_eu_design(x, weights = eu_weights[eu_weights$supply_chain == "Midstream", ], method = "greedy", supply_chains = "Midstream")
  expect_equal(unique(out$specialization$supply_chain), "Midstream")
  bad <- eu_weights
  bad$weight[1] <- -1
  expect_error(run_eu_design(x, weights = bad, method = "greedy"), "finite and >= 0")
})

test_that("edges to and from the bundled node are trade-weighted means of member edges; intra-EU edges are dropped", {
  x <- make_eu_inputs()
  members <- allied_network_check_members(eu_members, all_iso)
  mw <- allied_network_member_weights(members, eu_weights, tidyr::crossing(tech = "Solar", supply_chain = stages))
  edges <- allied_network_extract_edges(
    x$fs, x$op, iso3c_network = all_iso, network_members = members, member_weights = mw,
    node_trade_tbl = allied_network_member_trade(x$trade, all_iso)
  )
  nodes <- c("USA", "CAN", "MEX", "EUU")
  expect_setequal(unique(c(edges$reporter_iso, edges$partner_iso)), nodes)
  # every ordered pair of distinct nodes once per stage; no EU member and no EUU -> EUU edge
  expect_equal(nrow(edges), 4 * 3 * 2)
  expect_false(any(edges$reporter_iso == edges$partner_iso))
  expect_false(any(c(edges$reporter_iso, edges$partner_iso) %in% eu_iso))
  expect_equal(anyDuplicated(edges[c("reporter_iso", "partner_iso", "tech", "supply_chain")]), 0)
  get_edge <- function(p, c, stage) edges[edges$reporter_iso == p & edges$partner_iso == c & edges$supply_chain == stage, ]

  # EU as consumer of USA (Midstream): FS(member, USA) weighted by the members' imports from USA (10, 30, 0)
  e <- get_edge("USA", "EUU", "Midstream")
  expect_equal(e$friendshore_index, (10 * lookup(x$fs, "DEU", "USA", "Midstream") + 30 * lookup(x$fs, "FRA", "USA", "Midstream")) / 40)
  # ... and OP(USA, member) with the same trade weights
  expect_equal(e$opportunity_index, (10 * lookup(x$op, "USA", "DEU", "Midstream") + 30 * lookup(x$op, "USA", "FRA", "Midstream")) / 40)
  expect_equal(e$edge_weight, 0.5 * e$friendshore_index + 0.5 * e$opportunity_index)
  # EU as producer for CAN (Midstream): weighted by the members' exports to CAN (5, 0, 15)
  e <- get_edge("EUU", "CAN", "Midstream")
  expect_equal(e$opportunity_index, (5 * lookup(x$op, "DEU", "CAN", "Midstream") + 15 * lookup(x$op, "ITA", "CAN", "Midstream")) / 20)
  expect_equal(e$friendshore_index, (5 * lookup(x$fs, "CAN", "DEU", "Midstream") + 15 * lookup(x$fs, "CAN", "ITA", "Midstream")) / 20)
  # No basket trade on the edge: the stage weights (Midstream 0.5 / 0.3 / 0.2; Upstream 0.6 / 0 / 0.4)
  e <- get_edge("EUU", "MEX", "Midstream")
  expect_equal(e$opportunity_index, sum(c(0.5, 0.3, 0.2) * sapply(eu_iso, lookup, tbl = x$op, partner = "MEX", stage = "Midstream")))
  e <- get_edge("EUU", "MEX", "Upstream")
  expect_equal(e$opportunity_index, (0.6 * lookup(x$op, "DEU", "MEX", "Upstream") + 0.4 * lookup(x$op, "ITA", "MEX", "Upstream")) / 1)
  # Edges between single-member nodes are unchanged
  e <- get_edge("USA", "CAN", "Upstream")
  expect_equal(e$friendshore_index, lookup(x$fs, "CAN", "USA", "Upstream"))
  expect_equal(e$opportunity_index, lookup(x$op, "USA", "CAN", "Upstream"))

  agg <- attr(edges, "edge_aggregation")
  expect_equal(unique(agg$intra_node_member_edges_dropped), 3 * 2 * 2)
  mid_fs <- agg[agg$component == "friendshore" & agg$supply_chain == "Midstream", ]
  expect_equal(mid_fs$n_edges[mid_fs$weight_basis == "trade"], 2)  # USA -> EUU and EUU -> CAN
  expect_equal(mid_fs$n_edges[mid_fs$weight_basis == "stage"], 4)
  expect_equal(mid_fs$n_edges[mid_fs$weight_basis == "member"], 6)

  # Without trade every bundled edge uses the stage weights
  plain <- allied_network_extract_edges(x$fs, x$op, iso3c_network = all_iso, network_members = members, member_weights = mw)
  e <- plain[plain$reporter_iso == "USA" & plain$partner_iso == "EUU" & plain$supply_chain == "Midstream", ]
  expect_equal(e$friendshore_index, sum(c(0.5, 0.3, 0.2) * sapply(eu_iso, function(m) lookup(x$fs, m, "USA", "Midstream"))))

  # Weight fallbacks inside one component: a member without a value drops out; all-zero weights -> equal
  comp <- tibble::tibble(reporter_iso = c("DEU", "FRA", "ITA"), partner_iso = "MEX", tech = "Solar", supply_chain = "Upstream",
                         value = c(NA, 0.9, 0.5))
  out <- allied_network_aggregate_edge_component(comp, members, mw)
  expect_equal(out$value, 0.5)  # FRA weighs 0 upstream, DEU has no value: ITA alone
  expect_equal(out$weight_basis, "stage")
  comp$value <- c(NA, 0.9, NA)
  out <- allied_network_aggregate_edge_component(comp, members, mw)
  expect_equal(out$value, 0.9)
  expect_equal(out$weight_basis, "equal")
})

test_that("the design solves nodes: caps from summed GDP, member shares derived by stage weight", {
  x <- make_eu_inputs()
  out <- run_eu_design(x, method = "greedy", portfolio_enable = TRUE, portfolio_max_cap = 10, portfolio_min_cap = 2)
  spec <- out$specialization
  expect_setequal(unique(spec$iso3c), c("USA", "CAN", "MEX", "EUU"))
  expect_equal(out$params$network_nodes, c("USA", "CAN", "MEX", "EUU"))
  expect_true(out$params$node_aggregation)
  expect_equal(out$params$iso3c_network, all_iso)
  expect_equal(unique(spec$country[spec$iso3c == "EUU"]), "European Union")
  expect_equal(unique(spec$n_members[spec$iso3c == "EUU"]), 3L)
  expect_equal(unique(spec$tier[spec$iso3c == "EUU"]), "core")
  expect_equal(unique(spec$network_node), unique(spec$iso3c))
  expect_equal(which(names(spec) %in% c("tier", "network_node", "n_members")), which(names(spec) == "iso3c") + 1:3)
  expect_false(any(c(out$flows$reporter_iso, out$flows$partner_iso) %in% eu_iso))
  expect_equal(unique(out$diagnostics$n_nodes), 4L)
  share_sums <- tapply(spec$production_share, spec$supply_chain, sum)
  expect_equal(as.numeric(share_sums), c(1, 1))

  # Portfolio caps from the node GDP (EU = sum of its members); the largest node gets the maximum
  caps <- out$portfolio$caps
  expect_equal(caps$gdp_usd[caps$iso3c == "EUU"], sum(gdp_val[eu_iso]))
  node_size <- tibble::tibble(iso3c = c("USA", "CAN", "MEX", "EUU"), gdp_usd = c(gdp_val[c("USA", "CAN", "MEX")], sum(gdp_val[eu_iso])))
  expect_equal(
    stats::setNames(caps$portfolio_cap, caps$iso3c),
    allied_network_compute_portfolio_caps(node_size, c("USA", "CAN", "MEX", "EUU"), min_cap = 2, max_cap = 10)
  )
  expect_equal(caps$portfolio_cap[caps$iso3c == "USA"], 10L)
  expect_setequal(out$portfolio$counts$iso3c, c("USA", "CAN", "MEX", "EUU"))
  # with the EU the largest node, it is the one at the maximum
  x_big <- x
  x_big$size$gdp_usd[x_big$size$iso3c == "DEU"] <- 4e13
  big <- run_eu_design(x_big, method = "greedy", portfolio_enable = TRUE)
  expect_equal(big$portfolio$caps$portfolio_cap[big$portfolio$caps$iso3c == "EUU"], 10L)

  # Disaggregation: one row per member and stage, summing to the node share
  ms <- out$member_shares
  expect_equal(nrow(ms), length(all_iso) * 2)
  expect_setequal(unique(ms$iso3c), all_iso)
  node_share <- spec$production_share[match(paste(ms$network_node, ms$supply_chain), paste(spec$iso3c, spec$supply_chain))]
  expect_equal(ms$node_production_share, node_share)
  expect_equal(ms$production_share_derived, ms$node_production_share * ms$member_weight)
  by_node <- ms %>% dplyr::group_by(supply_chain, network_node) %>% dplyr::summarise(s = sum(production_share_derived), w = sum(member_weight), .groups = "drop")
  expect_equal(by_node$s, spec$production_share[match(paste(by_node$network_node, by_node$supply_chain), paste(spec$iso3c, spec$supply_chain))])
  expect_equal(by_node$w, rep(1, nrow(by_node)))
  expect_equal(as.numeric(tapply(ms$production_share_derived, ms$supply_chain, sum)), c(1, 1))
  expect_true(all(ms$share_is_derived[ms$network_node == "EUU"]))
  expect_false(any(ms$share_is_derived[ms$network_node != "EUU"]))
  eu_up <- ms[ms$network_node == "EUU" & ms$supply_chain == "Upstream", ]
  expect_equal(eu_up$member_weight[eu_up$iso3c == "FRA"], 0)
  expect_equal(eu_up$production_share_derived[eu_up$iso3c == "DEU"], 0.6 * eu_up$node_production_share[1])
  expect_equal(unique(eu_up$weight_basis), "production")
  expect_equal(ms$tier[ms$iso3c == "MEX"][1], "partner")

  # Aggregation record
  agg <- out$node_aggregation
  expect_equal(agg$nodes$n_members[agg$nodes$network_node == "EUU"], 3L)
  expect_equal(agg$nodes$gdp_usd[agg$nodes$network_node == "EUU"], sum(gdp_val[eu_iso]))
  expect_true(agg$trade_supplied)
  expect_true(all(c("component", "weight_basis", "n_edges") %in% names(agg$edges)))
})

test_that("the bundled design also runs on the MILP", {
  skip_if_not(allied_network_has_milp(), "MILP solver packages not installed")
  x <- make_eu_inputs()
  out <- run_eu_design(x, method = "milp", diversity_lambda = 0.5)
  expect_true(all(out$diagnostics$method == "milp"))
  expect_setequal(unique(out$specialization$iso3c), c("USA", "CAN", "MEX", "EUU"))
  expect_false(any(c(out$flows$reporter_iso, out$flows$partner_iso) %in% eu_iso))
  expect_equal(as.numeric(tapply(out$member_shares$production_share_derived, out$member_shares$supply_chain, sum)), c(1, 1))
  expect_true(all(out$specialization$production_share <= 0.40 + 1e-6))
})

test_that("without a shared node nothing is aggregated (backward compatible)", {
  x <- make_eu_inputs()
  own <- eu_members %>% dplyr::mutate(network_node = iso3)
  base <- suppressWarnings(allied_network_design(
    economic_opportunity_index = x$eo, energy_security_index = x$es, policy_index = NULL,
    partner_friendshore_tbl = x$fs, partner_opportunity_tbl = x$op, country_info = eu_country_info,
    iso3c_network = all_iso, country_size_tbl = x$size, node_policy = FALSE, demand_mode = "mixed",
    method = "greedy", portfolio_enable = TRUE
  ))
  out <- run_eu_design(x, members = own, weights = NULL, method = "greedy", portfolio_enable = TRUE)
  expect_false(out$params$node_aggregation)
  expect_equal(out$params$network_nodes, all_iso)
  expect_null(out$node_aggregation)
  expect_equal(dplyr::select(out$specialization, -"tier", -"network_node", -"n_members"), base$specialization)
  expect_equal(out$flows, base$flows)
  expect_equal(out$diagnostics, base$diagnostics)
  expect_equal(out$portfolio, base$portfolio)
  expect_null(base$member_shares)
  expect_equal(out$member_shares$production_share_derived, out$member_shares$node_production_share)
  expect_false(any(out$member_shares$share_is_derived))
  # trade and weights are ignored when nothing is bundled
  out2 <- run_eu_design(x, members = own, method = "greedy", portfolio_enable = TRUE)
  expect_equal(out2$specialization, out$specialization)
})

test_that("allied_network_add_node_members adds the rest of a bundled node only", {
  members <- tibble::tibble(
    iso3 = c("DEU", "FRA", "USA"), country = c("Germany", "France", "United States"),
    tier = "core", network_node = c("EUU", "EUU", "USA"), node_tier = "core", source = "coalition_tbl"
  )
  coalition_tbl <- tibble::tibble(
    iso3 = c("DEU", "FRA", "USA", "MLT", "EST", "CHN", "PRI"),
    country = c("Germany", "France", "United States", "Malta", "Estonia", "China", "Puerto Rico"),
    tier = c("core", "core", "core", "none", "none", "none", "none"),
    network_node = c("EUU", "EUU", "USA", "EUU", "EUU", "CHN", "USA"),
    node_tier = c("core", "core", "core", "core", NA, "none", "none")
  )
  out <- allied_network_add_node_members(members, coalition_tbl)
  expect_equal(out$iso3, c("DEU", "EST", "FRA", "MLT", "USA"))
  expect_equal(out$node_only, c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(out$tier[out$iso3 == "MLT"], "none")
  expect_equal(out$network_node[out$node_only], c("EUU", "EUU"))
  # node_tier from the table, else from the node's coalition members
  expect_equal(out$node_tier[out$iso3 == "EST"], "core")
  expect_equal(out$source[out$node_only], c("node_member", "node_member"))
  # nothing bundled -> unchanged (plus node_only)
  solo <- allied_network_add_node_members(members[3, ], coalition_tbl)
  expect_equal(solo$iso3, "USA")
  expect_false(solo$node_only)
  # a node code that is another member's ISO3 must contain that member
  bad <- eu_members
  bad$network_node[bad$iso3 == "CAN"] <- "MEX"
  bad$network_node[bad$iso3 == "MEX"] <- "EUU"
  expect_error(allied_network_check_members(bad, all_iso), "network_node MEX is the ISO3 code of a member")
})

test_that("observed export HHI counts a bundled node's exports outside the node only", {
  members <- allied_network_check_members(eu_members, all_iso)
  cells <- tibble::tribble(
    ~exporter_iso, ~importer_iso, ~value_last,
    "DEU", "FRA", 100,   # intra-EU: not an EU export
    "DEU", "USA", 30,
    "FRA", "CHN", 10,
    "ITA", "CAN", NA,    # unobserved: 0
    "USA", "DEU", 20,
    "USA", "CHN", 20,
    "CAN", "USA", 40,
    "CHN", "USA", 500    # not a node
  ) %>% dplyr::mutate(tech = "Solar", supply_chain = "Midstream")
  cells <- dplyr::bind_rows(cells, tibble::tibble(exporter_iso = "USA", importer_iso = "CAN", value_last = 0, tech = "Wind", supply_chain = "Upstream"))
  out <- allied_network_observed_export_hhi(cells, members)
  expect_equal(nrow(out), 1)  # the stage without any positive export is left out
  x <- c(EUU = 40, USA = 40, CAN = 40)
  expect_equal(out$hhi, sum((x / sum(x))^2))
  expect_equal(out$effective_exporters, 3)
  expect_equal(out$n_nodes_exporting, 3L)
  expect_equal(out$total_exports, 120)
  # members as their own nodes: intra-EU exports count
  own <- allied_network_check_members(eu_members %>% dplyr::mutate(network_node = iso3), all_iso)
  out2 <- allied_network_observed_export_hhi(cells, own)
  y <- c(DEU = 130, FRA = 10, USA = 40, CAN = 40)
  expect_equal(out2$hhi, sum((y / sum(y))^2))
})
