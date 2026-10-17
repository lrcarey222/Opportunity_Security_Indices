repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "aib", "aib_export.R"))
source(file.path(repo_root, "R", "dashboard", "aib", "aib_check.R"))
source(file.path(repo_root, "R", "dashboard", "country", "country_util.R"))
suppressPackageStartupMessages(library(dplyr))

# A two-scenario run with the columns scripts/32 writes into aib_results.rds.
aib_net_res <- function() {
  countries <- tibble::tibble(
    iso3 = c("DEU", "USA", "DEU", "USA"), scenario = c("reallocation", "reallocation", "conservative", "conservative"),
    supplier_n_partners = c(3, 0, 2, 0), buyer_n_partners = c(0, 2, 0, 1), investment_host_n_partners = c(1, 0, 1, 0), investor_n_partners = c(0, 1, 0, 1),
    supplier_n_selected = c(9, 0, 4, 0), buyer_n_selected = c(0, 9, 0, 4), investment_host_n_selected = c(1, 0, 1, 0), investor_n_selected = c(0, 1, 0, 1),
    supplier_headline_contribution = c(0.25, 0, 0.1, 0), buyer_headline_contribution = c(0, 0.25, 0, 0.1),
    investment_host_headline_contribution = c(0.1, 0, 0.1, 0), investor_headline_contribution = c(0, 0.1, 0, 0.1),
    supplier_cumulative_contribution = c(1.2, 0, 0.5, 0), buyer_cumulative_contribution = c(0, 1.2, 0, 0.5),
    investment_host_cumulative_contribution = c(0.2, 0, 0.2, 0), investor_cumulative_contribution = c(0, 0.2, 0, 0.2),
    supplier_cumulative_coordination_dividend = c(0.05, 0, 0, 0), buyer_cumulative_coordination_dividend = c(0, 0.05, 0, 0),
    supplier_headline_volume_usd = c(5e8, 0, 2e8, 0), buyer_headline_volume_usd = c(0, 5e8, 0, 2e8),
    supplier_n_upstream_partners = c(1, 0, 1, 0), buyer_n_upstream_partners = 0, investment_host_n_upstream_partners = 0, investor_n_upstream_partners = 0,
    supplier_n_midstream_partners = c(2, 0, 1, 0), buyer_n_midstream_partners = c(0, 1, 0, 1), investment_host_n_midstream_partners = c(1, 0, 1, 0), investor_n_midstream_partners = c(0, 1, 0, 1),
    supplier_n_downstream_partners = 0, buyer_n_downstream_partners = 0, investment_host_n_downstream_partners = 0, investor_n_downstream_partners = 0,
    technologies = "Solar", n_technologies = 1, roles = c("investment_host; supplier", "buyer; investor", "investment_host; supplier", "buyer; investor"),
    headline_lost_usd = c(NA, 3e9, NA, 3e9), n_exposed_subsectors = c(NA, 2, NA, 2)
  )
  edges <- tibble::tibble(
    supplier_iso = c("DEU", "DEU"), buyer_iso = c("USA", "USA"), scenario = c("reallocation", "conservative"),
    n_trade_selected = c(9, 4), n_investment_selected = c(1, 1), n_shocks = c(2, 2), n_subsectors = 1, n_technologies = 1,
    technologies = "Solar", subsectors = "solar-cells", n_high_severity = 1, upstream = FALSE, midstream = TRUE, downstream = FALSE,
    headline_trade_volume_usd = c(5e8, 2e8), headline_trade_contribution = c(0.25, 0.1), headline_investment_contribution = 0.1,
    cumulative_trade_resilience_contribution = c(1.2, 0.5), cumulative_coordination_dividend = c(0.05, 0),
    cumulative_investment_resilience_contribution = 0.2, export_opportunity = 0.9, friendshore = NA_real_
  )
  pairs <- tibble::tibble(
    node_id = "solar-cells", shock_id = c("china_100", "china_100", "china_100", "china_50"), scenario = "reallocation",
    supply_constraint = "production_constrained", investor_iso = c("USA", "USA", "DEU", "USA"), host_iso = c("DEU", "KOR", "DEU", "DEU"),
    increment = 0.1, supply_from_new_capacity = c(0.08, 0.07, 0.01, 0.05), supply_from_new_capacity_usd = c(8e8, 7e8, 1e8, 5e8),
    investor_lost_share = 0.4, export_opportunity = 0.9, friendshore = 0.7,
    relationship = c("investment partnership", "investment partnership", "domestic", "investment partnership"), host_rank = c(1L, 2L, 1L, 1L)
  )
  hosts <- tibble::tibble(
    node_id = "solar-cells", shock_id = "china_100", scenario = "reallocation", supply_constraint = "production_constrained",
    increment = c(0.1, 0.1, 0.05, 0.1), host_iso = c("DEU", "KOR", "DEU", "JPN"), host_rank = c(1L, 2L, 1L, NA),
    marginal_resilience_gain = c(0.09, 0.08, 0.05, 0.01), coverage_before = 0.5, coverage_after = c(0.59, 0.58, 0.55, 0.51),
    new_capacity_used = 0.09, additional_demand_served_usd = 9e8, capability_tier = 3L, capability_basis = "production share in the node"
  )
  list(
    meta = list(headline_shock = "china_100", headline_scenario = "reallocation", headline_semantics = "hold", headline_constraint = "production_constrained",
                strategic_scope = "electro_industrial", demand_scope = "coalition", candidate_universe = "global", universe = "coalition demand / global partners",
                config = list(shocks = list(list(id = "china_100"), list(id = "china_50")), investment = list(headline_increment = 0.1))),
    countries = countries, edges = edges, investment_pairs = pairs, investment = hosts
  )
}

test_that("the network section keeps the headline scenario, four sides and the variant", {
  n <- aib_dash_network(aib_net_res())
  expect_length(n$countries, 2)
  deu <- n$countries[[1]]
  expect_equal(deu$iso3, "DEU")
  expect_equal(deu$supplier$headline, 0.25)
  expect_equal(deu$supplier$cumulative, 1.2)
  expect_equal(deu$host$partners, 1L)
  expect_null(deu$host$volumeUsd)
  expect_true(is.na(deu$lostUsd))
  expect_equal(n$countries[[2]]$lostUsd, 3e9)
  expect_length(n$edges, 1)
  expect_equal(n$edges[[1]]$trade, 0.25)
  expect_equal(as.character(n$edges[[1]]$stages), "Midstream")
  expect_equal(n$variant$scenario, "reallocation")
  expect_equal(as.character(n$variant$shocks), c("china_100", "china_50"))
  j <- as.character(country_json(n$edges[[1]]))
  expect_match(j, '"nodes":\\["solar-cells"\\]')
  expect_match(j, '"friendshore":null')
})

test_that("investment flows keep the headline shock and increment, ranked hosts, no domestic rows", {
  f <- aib_dash_investment_flows(aib_net_res())
  expect_length(f$pairs, 2)
  expect_equal(vapply(f$pairs, `[[`, "", "host"), c("DEU", "KOR"))
  expect_equal(vapply(f$pairs, `[[`, 1L, "rank"), c(1L, 2L))
  expect_length(f$hosts, 2)
  expect_equal(f$variant$increment, 0.1)
  expect_equal(f$variant$maxRank, 5L)
})

test_that("investment flows keep only nodes of the strategic scope", {
  res <- aib_net_res()
  res$classification <- tibble::tibble(node_id = "solar-cells", in_strategic_scope = TRUE)
  expect_length(aib_dash_investment_flows(res)$pairs, 2)
  res$classification$in_strategic_scope <- FALSE
  f <- aib_dash_investment_flows(res)
  expect_length(f$pairs, 0)
  expect_length(f$hosts, 0)
})

test_that("the R mirror accepts the exported sections and names each broken field", {
  res <- aib_net_res()
  doc <- list(nodes = list(list(id = "solar-cells")), network = aib_dash_network(res), investmentFlows = aib_dash_investment_flows(res))
  expect_equal(aib_check_bundle_additions(doc), character())
  expect_equal(aib_check_bundle_additions(list(nodes = list())), character())
  bad <- doc
  bad$network$edges[[1]]$nodes <- "solar-cells"
  bad$network$countries[[2]]$iso3 <- "DEU"
  bad$investmentFlows$pairs[[1]]$host <- "USA"
  bad$investmentFlows$pairs[[2]]$rank <- 7L
  bad$investmentFlows$hosts[[1]]$node <- "wafers"
  p <- aib_check_bundle_additions(bad)
  expect_true(any(grepl("^network.edges\\[0\\].nodes: expected an array", p)))
  expect_true(any(grepl("^network.countries\\[1\\].iso3: duplicate 'DEU'", p)))
  expect_true(any(grepl("^investmentFlows.pairs\\[0\\]: investor and host are the same economy", p)))
  expect_true(any(grepl("^investmentFlows.pairs\\[1\\].rank: 7 above 5", p)))
  expect_true(any(grepl("^investmentFlows.hosts\\[0\\].node: 'wafers' is not a node", p)))
})
