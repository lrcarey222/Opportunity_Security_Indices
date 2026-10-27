repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "utils/supply_chain_stages.R")) source(file.path(repo_root, "R", f))
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
source(file.path(repo_root, "R", "dashboard", "aib", "aib_export.R"))
source(file.path(repo_root, "R", "dashboard", "aib", "aib_check.R"))
source(file.path(repo_root, "R", "dashboard", "country", "country_util.R"))
suppressPackageStartupMessages(library(dplyr))

# The scenario toggle (2026-10-19): the observed (Baseline) supply and network, the Managed Interdependence
# compact aggregates and the two scenario files.

# Two nodes; the demand scope is USA, DEU and JPN (CAN is outside it).
scn_flows <- function() {
  tibble::tibble(
    node_id = c("cells", "cells", "cells", "cells", "cells", "anodes", "anodes", "anodes"),
    exporter_iso = c("CHN", "KOR", "CHN", "JPN", "CHN", "CHN", "KOR", "CHN"),
    importer_iso = c("USA", "USA", "DEU", "DEU", "CAN", "USA", "JPN", "JPN"),
    value = c(60, 20, 10, 10, 500, 30, 2, 0)
  )
}

test_that("observed supply sums each exporter's shipments into the demand scope, as shares of the node's demand", {
  s <- aib_observed_supply(scn_flows(), c("USA", "DEU", "JPN"))
  cells <- s[s$node_id == "cells", ]
  # CAN is outside the scope; the node's demand is 60 + 20 + 10 + 10 = 100.
  expect_equal(cells$exporter_iso, c("CHN", "KOR", "JPN"))
  expect_equal(cells$supply_usd, c(70, 20, 10))
  expect_equal(unique(cells$demand_usd), 100)
  expect_equal(cells$share, c(0.7, 0.2, 0.1))
  expect_equal(cells$rank, 1:3)
  anodes <- s[s$node_id == "anodes", ]
  # A zero flow is no supply.
  expect_equal(anodes$supply_usd, c(30, 2))
  expect_equal(sum(anodes$share), 1)
  expect_equal(nrow(aib_observed_supply(scn_flows(), c("USA", "DEU", "JPN"), node_ids = "anodes")), 2)
  expect_equal(nrow(aib_observed_supply(scn_flows(), "FRA")), 0)
})

test_that("ties in observed supply are ranked by ISO3 code", {
  f <- tibble::tibble(node_id = "n", exporter_iso = c("KOR", "DEU", "JPN"), importer_iso = "USA", value = c(5, 5, 10))
  s <- aib_observed_supply(f, "USA")
  expect_equal(s$exporter_iso, c("JPN", "DEU", "KOR"))
})

test_that("the observed network is a pseudo-shock whose fields are observed USD and shares of node demand", {
  nodes <- tibble::tibble(node_id = c("cells", "anodes"), supply_chain = c("Assembly", "Refining"), techs = c("Batteries", "Batteries"))
  inp <- aib_observed_network_inputs(scn_flows(), c("USA", "DEU", "JPN"))
  expect_true(all(inp$trade$relationship == "existing"))
  expect_true(all(inp$trade$shock_id == "baseline" & inp$trade$scenario == "observed"))
  expect_equal(inp$node_shock$shock_exposure, c(0, 0))
  pairs <- aib_network_pairs(inp$trade, NULL, nodes, inp$node_shock, "baseline", 0.4)
  chn_usa <- pairs[pairs$supplier_iso == "CHN" & pairs$buyer_iso == "USA", ]
  # CHN -> USA: 60 of the cells' demand of 100 and 30 of the anodes' demand of 32.
  expect_equal(chn_usa$headline_trade_volume_usd, 90)
  expect_equal(chn_usa$headline_trade_contribution, 0.6 + 30 / 32)
  expect_equal(chn_usa$cumulative_trade_resilience_contribution, chn_usa$headline_trade_contribution)
  expect_equal(chn_usa$n_trade_selected, 2)
  expect_equal(chn_usa$n_high_severity, 0)
  # One stage flag per six-stage stage: cells are Assembly, anodes Refining.
  expect_equal(unlist(chn_usa[, tolower(SUPPLY_CHAIN_STAGES)], use.names = FALSE), c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_false(any(c("upstream", "midstream", "downstream") %in% names(pairs)))
  expect_equal(sum(pairs$cumulative_coordination_dividend), 0)
  expect_equal(sum(pairs$headline_investment_contribution), 0)
  # Shares summed over the pairs give one per node.
  expect_equal(sum(pairs$headline_trade_contribution), 2)
  expect_equal(sum(pairs$headline_trade_volume_usd), 132)
  kept <- aib_network_min_share(pairs, 0.1)
  expect_false(any(kept$supplier_iso == "KOR" & kept$buyer_iso == "JPN"))   # 2 / 32 < 0.1
  expect_true(all(kept$cumulative_trade_resilience_contribution >= 0.1))
  co <- aib_network_countries(kept)
  expect_equal(sort(co$iso3), c("CHN", "DEU", "JPN", "KOR", "USA"))
  expect_equal(co$roles[co$iso3 == "CHN"], "supplier")
  expect_true(all(paste0("supplier_n_", tolower(SUPPLY_CHAIN_STAGES), "_partners") %in% names(co)))
  expect_equal(co$supplier_n_deployment_partners[co$iso3 == "CHN"], 0)
  expect_false("headline_lost_usd" %in% names(co))
})

test_that("importer exposure sums lost USD over the scope nodes for every trade scenario", {
  loss <- tibble::tibble(node_id = c("a", "a", "b", "z"), importer_iso = c("USA", "DEU", "USA", "USA"),
                         lost_share = c(0.2, 0, 0.1, 0.5), lost_usd = c(20, 0, 5, 100))
  ex <- aib_importer_exposure(loss, c("a", "b"), c("conservative", "reallocation"))
  expect_equal(nrow(ex), 4)
  usa <- ex[ex$iso3 == "USA" & ex$scenario == "reallocation", ]
  expect_equal(usa$headline_lost_usd, 25)
  expect_equal(usa$n_exposed_subsectors, 2)
  expect_equal(ex$n_exposed_subsectors[ex$iso3 == "DEU"][1], 0)
})

test_that("a scenario's class mix can count every class, and the policy needs survive an all-LOW mix", {
  cls <- c("BUILD", "LOW INTERVENTION", "BUILD")
  lost <- c(30, 20, 50)
  expect_equal(names(aib_class_mix(cls, lost)), "BUILD")
  mix <- aib_class_mix(cls, lost, keep = aib_node_classes())
  expect_equal(aib_class_mix_text(mix), "BUILD 80%; LOW INTERVENTION 20%")
  expect_true(is.na(aib_class_mix_text(aib_class_mix("LOW INTERVENTION", 10))))
  expect_equal(aib_policy_needs(aib_class_mix("LOW INTERVENTION", 10)), aib_archetype_tools()[["Integrated Compact"]])
})

test_that("a compact the scenario did not run on says why", {
  expect_match(aib_scenario_coalition_reason(c("already as diversified as its benchmark", NA)), "above its benchmark by more than a small tolerance")
  expect_match(aib_scenario_coalition_reason(c("within the cap in the allied market", "already as diversified as its benchmark")),
               "above its benchmark cap in the allied market")
  expect_match(aib_scenario_coalition_reason("no benchmark"), "no concentration benchmark|has a concentration benchmark")
  expect_match(aib_scenario_coalition_reason(character()), "did not assess")
})

test_that("compact.managed weights the run nodes by demand and counts the others as unchanged", {
  dv <- tibble::tibble(node_id = c("a", "b"), demand_usd = c(100, 300), lost_usd = c(40, 30),
                       static_coverage = c(0.6, 0.9), existing_coverage = c(0.8, 0.95), optimized_coverage = c(0.9, 1),
                       residual_gap_usd = c(10, 0), diversify_class = c("BUILD", "LOW INTERVENTION"))
  m <- aib_dash_compact_managed(c("a", "c"), dv, c(a = 100, b = 300, c = 300))
  expect_equal(m$nodesRun, 1)
  expect_equal(m$demandUsd, 400)
  expect_equal(m$lostUsd, 40)
  expect_equal(m$static, (0.6 * 100 + 300) / 400)
  expect_equal(m$optimized, (0.9 * 100 + 300) / 400)
  expect_equal(m$residualUsd, 10)
  expect_equal(m$residual, 10 / 400)
  expect_equal(m$classMix, "BUILD 100%")
  both <- aib_dash_compact_managed(c("a", "b"), dv, c(a = 100, b = 300))
  expect_equal(both$classMix, "BUILD 57%; LOW INTERVENTION 43%")
  none <- aib_dash_compact_managed("c", dv, c(c = 300))
  expect_equal(none$nodesRun, 0)
  expect_equal(c(none$static, none$existing, none$optimized, none$residual, none$lostUsd), c(1, 1, 1, 0, 0))
  expect_true(is.na(none$classMix))
})

test_that("a compact's observed suppliers are summed over its nodes, with the HHI over every exporter", {
  s <- aib_observed_supply(scn_flows(), c("USA", "DEU", "JPN"))
  b <- aib_dash_top_suppliers(s, c("cells", "anodes"), 2L)
  # Demand 100 + 32; CHN 70 + 30, KOR 20 + 2, JPN 10.
  expect_equal(b$demand_usd, 132)
  expect_equal(b$top$exporter_iso, c("CHN", "KOR"))
  expect_equal(b$top$share, c(100, 22) / 132)
  expect_equal(b$hhi, sum((c(100, 22, 10) / 132)^2))
  expect_null(aib_dash_top_suppliers(s, "none"))
  js <- as.character(country_json(list(a = aib_dash_supplier_tuples(b$top), b = aib_dash_supplier_tuples(b$top, usd = TRUE),
                                       e = aib_dash_supplier_tuples(NULL))))
  expect_equal(js, "{\"a\":[[\"CHN\",0.7576],[\"KOR\",0.1667]],\"b\":[[\"CHN\",100,0.7576],[\"KOR\",22,0.1667]],\"e\":[]}")
  expect_equal(aib_dash_tuple_codes(aib_dash_supplier_tuples(b$top)), c("CHN", "KOR"))
})

# A run with the scenario results scripts/32 saves (only the columns the export reads).
scn_res <- function() {
  flows <- scn_flows()
  scope <- c("USA", "DEU", "JPN")
  nodes <- tibble::tibble(node_id = c("cells", "anodes"), supply_chain = c("Assembly", "Refining"), techs = c("Batteries", "Batteries"))
  inp <- aib_observed_network_inputs(flows, scope)
  eb <- aib_network_min_share(aib_network_pairs(inp$trade, NULL, nodes, inp$node_shock, "baseline", 0.4), 0.05)
  cp <- function(id, method, members) tibble::tibble(
    compact_id = id, compact_name = "Batteries: cells, anodes", technology = "Batteries", stages = "Assembly; Refining",
    compact_archetype = "Investment / Capacity Compact", compact_type = "candidate compact", coalition_method = method,
    minimum_coalition = members, minimum_coalition_90 = members, minimum_coalition_95 = members, minimum_coalition_99 = members,
    target_reached_99 = !is.na(members), robust_tie_break_solved = TRUE, max_anchor_coverage = 0.99,
    core_coordinating_members = members, demand_anchors = if (is.na(members)) NA_character_ else "USA",
    specialized_suppliers = if (is.na(members)) NA_character_ else "KOR", investment_hosts = "", technology_partners = "", bridge_partners = "",
    associate_partners = "", demand_usd = 132, lost_usd = if (is.na(members)) 0 else 50, baseline_shock_exposure = if (is.na(members)) 0 else 50 / 132,
    coverage_static = 0.7, coverage_without_coordination = 0.8, coverage_with_trade_coordination = 0.95,
    coverage_after_investment_simulation = 0.97, coverage_global_optimum = 0.98, coordination_dividend = 0.15, residual_gap = 0.05,
    residual_gap_after_investment = 0.03, anchor_coverage_fragmented = 0.8, anchor_coverage_compact = 0.96, compact_gain_over_fragmented = 0.16,
    class_mix = "BUILD 100%", secondary_class = NA_character_, key_policy_needs = "coordinated finance"
  )
  list(
    meta = list(headline_shock = "china_100", headline_scenario = "reallocation", headline_semantics = "hold", headline_constraint = "production_constrained",
                strategic_scope = "all", demand_scope = "coalition", candidate_universe = "global", universe = "coalition demand / global partners",
                trade_year = 2024, built = "2026-10-19T12:00:00",
                config = list(shocks = list(list(id = "china_100", type = "country"), list(id = "diversify_benchmark", type = "diversify")),
                              investment = list(headline_increment = 0.1))),
    scenario_shock = "diversify_benchmark",
    classification = tibble::tibble(node_id = c("cells", "anodes"), demand_usd = c(100, 32), in_strategic_scope = TRUE),
    compacts = bind_rows(cp("AIB-C01", "exact", "KOR USA"), cp("AIB-C02", "exact", "KOR USA")),
    compact_nodes = tibble::tibble(node_id = c("cells", "anodes"), compact_id = c("AIB-C01", "AIB-C02")),
    compacts_managed = bind_rows(cp("AIB-C01", "exact", "KOR USA") %>% mutate(n_nodes_run = 1L, coalition_reason = NA_character_),
                                 cp("AIB-C02", NA_character_, NA_character_) %>%
                                   mutate(n_nodes_run = 0L, coalition_reason = "no node of this compact is more concentrated than its benchmark")),
    compact_members_managed = tibble::tibble(compact_id = "AIB-C01", iso3 = c("USA", "KOR"), role = c("Demand anchor", "Specialized supplier"),
                                             basis = "b", in_minimum_coalition = TRUE, coalition_tier = "core coordinating member",
                                             critical_supplier = FALSE, loo_critical = FALSE, substitutable_by = NA_character_, substitutes_needed = 0L),
    coalition_paths_managed = tibble::tibble(compact_id = "AIB-C01", step = 0:1, added = c(NA, "KOR"), members = c("USA", "KOR USA"),
                                             min_coverage = c(0.8, 0.96), mean_coverage = c(0.8, 0.96), node_coverage = c("cells=0.800", "cells=0.960")),
    partnerships = tibble::tibble(node_id = "cells", shock_id = c("diversify_benchmark", "china_100"), scenario = "reallocation",
                                  exporter_iso = c("KOR", "JPN"), importer_iso = "USA", relationship = "expanded",
                                  delta_usd = c(15, 40), delta_share_of_demand = c(0.15, 0.4), export_opportunity = 0.5),
    investment_pairs = NULL, investment = NULL,
    baseline_supply = aib_observed_supply(flows, scope), edges_baseline = eb, countries_baseline = aib_network_countries(eb),
    country_names = tibble::tibble(iso3 = c("CHN", "KOR", "USA", "DEU", "JPN"), country = c("China", "Korea", "United States", "Germany", "Japan"))
  )
}

test_that("the meta scenario list runs Baseline, Managed Interdependence, Decoupling, and is absent without scenario results", {
  defs <- aib_dash_scenario_defs(scn_res())
  expect_equal(vapply(defs, `[[`, "", "id"), c("baseline", "managed", "decoupling"))
  expect_true(is.na(defs[[1]]$shockId))
  expect_equal(defs[[2]]$shockId, "diversify_benchmark")
  expect_equal(defs[[3]]$shockId, "china_100")
  expect_match(defs[[1]]$words, "2024")
  r0 <- scn_res()
  r0$baseline_supply <- NULL
  r0$compacts_managed <- NULL
  expect_null(aib_dash_scenario_defs(r0))
  expect_match(as.character(country_json(list(s = defs))), "\"shockId\":null")
})

test_that("the Baseline file carries the observed network, with no shock, and the top suppliers", {
  doc <- aib_dash_scenario_baseline(scn_res(), top = 2L)
  expect_equal(doc$schema, "osi-aib-scenario")
  expect_equal(doc$scenario, "baseline")
  expect_true(is.na(doc$shockId))
  expect_equal(doc$network$variant$shock, "baseline")
  expect_equal(doc$network$variant$scenario, "observed")
  expect_length(doc$network$variant$shocks, 0)
  e <- doc$network$edges[[which(vapply(doc$network$edges, function(x) x$from == "CHN" && x$to == "USA", logical(1)))]]
  expect_equal(e$nShocks, 0L)
  expect_equal(e$tradeUsd, 90)
  expect_equal(e$cumTrade, e$trade)
  expect_equal(c(e$investment, e$cumDividend, e$cumInvestment, e$nHighSeverity), c(0, 0, 0, 0))
  chn <- doc$network$countries[[which(vapply(doc$network$countries, `[[`, "", "iso3") == "CHN")]]
  expect_true(is.na(chn$lostUsd))
  expect_equal(chn$host$headline, 0)
  expect_equal(vapply(doc$nodes, `[[`, "", "id"), c("cells", "anodes"))
  expect_equal(doc$nodes[[1]]$suppliers[[1]], list("CHN", 70, 0.7))
  expect_length(doc$compacts[[1]]$suppliers, 2)
  expect_equal(doc$countries$KOR, "Korea")
  expect_length(aib_check_scenario_doc(doc, expect = "baseline"), 0)
})

test_that("the Managed Interdependence file solves each headline compact, or says why it has no coalition", {
  doc <- aib_dash_scenario_managed(scn_res())
  expect_equal(doc$scenario, "managed")
  expect_equal(doc$shockId, "diversify_benchmark")
  expect_equal(vapply(doc$compacts, `[[`, "", "id"), c("AIB-C01", "AIB-C02"))
  c1 <- doc$compacts[[1]]
  expect_equal(c1$nodesRun, 1L)
  expect_equal(as.character(c1$coalition$members), c("KOR", "USA"))
  expect_true(is.na(c1$coalitionReason))
  # Only the scenario's relationships.
  expect_equal(length(c1$trade), 1)
  expect_equal(c1$trade[[1]]$from, "KOR")
  expect_equal(length(c1$path), 2)
  c2 <- doc$compacts[[2]]
  expect_null(c2$coalition)
  expect_match(c2$coalitionReason, "benchmark")
  expect_equal(c2$metrics$lostUsd, 0)
  expect_length(c2$roles, 0)
  # The type, archetype and policy needs under the scenario: its own, none without a coalition.
  expect_equal(c1$type, "candidate compact")
  expect_equal(c1$archetype, "Investment / Capacity Compact")
  expect_equal(as.character(c1$policyNeeds), "coordinated finance")
  expect_true(is.na(c2$type) && is.na(c2$archetype))
  expect_length(c2$policyNeeds, 0)
  js <- as.character(country_json(doc))
  expect_match(js, "\"coalition\":null")
  expect_match(js, "\"anchors\":\\[\\]")
  expect_match(js, "\"policyNeeds\":\\[\"coordinated finance\"\\]")
  bad <- doc
  bad$compacts[[1]]$type <- "a compact"
  expect_match(paste(aib_check_scenario_doc(bad, expect = "managed"), collapse = " "), "compacts\\[0\\]\\.type")
  expect_null(doc$network)
  r0 <- scn_res()
  r0$compacts_managed <- NULL
  expect_null(aib_dash_scenario_managed(r0))
  expect_length(aib_check_scenario_doc(doc, expect = "managed"), 0)
})
