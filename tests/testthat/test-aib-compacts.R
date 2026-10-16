repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
source(file.path(repo_root, "R", "dashboard", "aib", "aib_export.R"))
suppressPackageStartupMessages(library(dplyr))

cls <- list(low_exposure = 0.15, adequate_coverage = 0.9, build_gap = 0.1, coordinate_dividend = 0.05,
            manage_largest_share = 0.4, manage_cap = 0.4, manage_coverage_loss = 0.05)

test_that("each class follows its rule, in order", {
  nr <- tibble::tibble(
    node_id = c("low", "fine", "build", "manage", "coord", "near", "thin"),
    optimize = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    shock_exposure = c(0.05, 0.5, 0.6, 0.5, 0.5, 0.5, 0.5),
    existing_coverage = c(0.95, 0.95, 0.4, 0.8, 0.8, 0.85, 0.5),
    optimized_coverage = c(1, 1, 0.5, 0.95, 0.95, 0.92, 0.5),
    residual_gap = c(0, 0, 0.5, 0.05, 0.05, 0.08, 0.5),
    coordination_dividend = c(0.05, 0.05, 0.1, 0.15, 0.15, 0.03, 0),
    optimized_largest_share = c(0.2, 0.2, 0.6, 0.55, 0.2, 0.2, 0.2),
    optimized_largest_supplier = "JPN",
    coverage_loss_at_manage_cap = c(0, 0, 0, 0, 0, 0, 0)
  )
  out <- aib_classify_nodes(nr, cls)
  expect_equal(out$node_classification,
               c("LOW INTERVENTION", "LOW INTERVENTION", "BUILD", "MANAGE SPECIALIZATION", "COORDINATE", "LOW INTERVENTION", "INSUFFICIENT DATA"))
  expect_match(out$classification_reason[2], "existing relationships restore")
  expect_gt(out$need_build[3], out$need_build[5])
})

test_that("grouping needs coherence, policy similarity and partner overlap at once", {
  classified <- tibble::tibble(
    node_id = c("anode", "cathode", "cells", "magnet"),
    node_classification = c("BUILD", "COORDINATE", "BUILD", "BUILD"),
    need_coordinate = c(0, 0.3, 0, 0), need_build = c(0.5, 0, 0.6, 0.5), need_manage = c(0, 0, 0, 0)
  )
  v <- list(
    anode = list(supply = c(JPN = 0.5, KOR = 0.5), demand = c(USA = 1)),
    cathode = list(supply = c(JPN = 0.5, KOR = 0.5), demand = c(USA = 1)),
    cells = list(supply = c(JPN = 0.6, KOR = 0.4), demand = c(DEU = 1)),
    magnet = list(supply = c(JPN = 0.5, KOR = 0.5), demand = c(USA = 1))
  )
  coh <- matrix(TRUE, 4, 4, dimnames = list(classified$node_id, classified$node_id))
  coh["magnet", c("anode", "cathode", "cells")] <- FALSE
  coh[c("anode", "cathode", "cells"), "magnet"] <- FALSE
  g <- aib_group_nodes(classified, v, coh, min_partner = 0.35, min_policy = 0.5)
  grp <- stats::setNames(g$group, g$node_id)
  expect_equal(grp[["anode"]], grp[["cells"]])          # same suppliers, same need
  expect_false(grp[["anode"]] == grp[["cathode"]])      # same suppliers, different policy need
  expect_false(grp[["anode"]] == grp[["magnet"]])       # not coherent in the supply chain
})

test_that("the greedy coalition adds only what the anchors' coverage needs", {
  flows <- tibble::tibble(
    exporter_iso = c("CHN", "CHN", "JPN", "KOR", "AUS", "AUS"),
    importer_iso = c("USA", "DEU", "USA", "DEU", "CHN", "NZL"),
    value = c(50, 30, 10, 10, 40, 5)
  )
  prob <- aib_node_problem(flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1, candidate_min_share = 0)
  problems <- list(n1 = prob)
  path <- aib_coalition_path(problems, "n1", anchors = c("USA", "DEU"), candidates = c("JPN", "KOR", "AUS"),
                             scenario = "reallocation", target = 0.99)
  # Anchors alone keep the non-members' surviving 20; Australia's 45 of redirected exports lift the
  # anchors to 65%; Japan and Korea add nothing they do not already ship, so the search stops.
  expect_equal(path$min_coverage[1], 0.20, tolerance = 1e-9)
  expect_equal(path$added[2], "AUS")
  expect_equal(nrow(path), 2)
  expect_equal(utils::tail(path$min_coverage, 1), 0.65, tolerance = 1e-9)
  at <- aib_coalition_at(path, 0.5)
  expect_true(at$reached)
  expect_false(grepl("KOR", aib_coalition_at(path, 0.2)$members))
})

test_that("archetypes follow the lost-supply-weighted class mix", {
  expect_equal(aib_archetype(c("BUILD", "BUILD", "COORDINATE"), c(50, 40, 10)), "Investment / Capacity Compact")
  expect_equal(aib_archetype(c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION"), c(40, 30, 30)), "Integrated Compact")
  expect_equal(aib_archetype(c("COORDINATE"), c(5)), "Trade / Coordination Compact")
})

test_that("host eligibility needs material capability; Friendshore plays no part", {
  em <- tibble::tibble(
    node_id = "n", exporter_iso = c("JPN", "CYM", "IND", "ARM"),
    exports = c(5e8, 2e6, 6e7, 3e6), world_share = c(0.5, 0.002, 0.06, 0.003),
    export_rca = c(2, 40, 0.8, 5), production_share = NA_real_
  )
  h <- aib_eligible_hosts("n", em, tibble::tibble(node_a = character(), node_b = character(), basis = character()),
                          min_exports_usd = 5e7, min_material_share = 0.01)
  expect_setequal(h$host_iso, c("JPN", "IND"))
  expect_equal(h$capability_tier[h$host_iso == "JPN"], 3L)
  expect_equal(h$capability_tier[h$host_iso == "IND"], 2L)
})

test_that("host ranking puts resilience first, then diversification, then capability", {
  rows <- tibble::tibble(
    node_id = "n", shock_id = "s", scenario = "r", increment = 0.1,
    host_iso = c("A", "B", "C"),
    marginal_resilience_gain = c(0.1, 0.1, 0.05),
    delta_hhi = c(0.01, -0.02, -0.05),
    capability_tier = c(3L, 2L, 3L), capability_value = c(1, 0.5, 1),
    friendshore_need_weighted = c(0.1, 0.9, 0.9), compatibility_need_weighted = 0.5
  )
  r <- aib_rank_hosts(rows, 0.005, 0.005)
  expect_equal(r$host_iso[order(r$host_rank)], c("B", "A", "C"))
})

test_that("dashboard arrays stay arrays when they have one element", {
  x <- list(a = aib_dash_split("USA"), b = aib_dash_split(""), c = aib_dash_split("DEU; FRA", "; "))
  j <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  expect_equal(j, '{"a":["USA"],"b":[],"c":["DEU","FRA"]}')
})

test_that("concentration in a few suppliers is MANAGE SPECIALIZATION even when existing trade copes", {
  nr <- tibble::tibble(node_id = "wafers", optimize = TRUE, shock_exposure = 0.26, existing_coverage = 0.91,
                       optimized_coverage = 0.91, residual_gap = 0.09, coordination_dividend = 0,
                       optimized_largest_share = 0.37, optimized_top3_share = 0.8, optimized_largest_supplier = "JPN",
                       coverage_loss_at_manage_cap = 0)
  out <- aib_classify_nodes(nr, c(cls, manage_top3_share = 0.75))
  expect_equal(out$node_classification, "MANAGE SPECIALIZATION")
  expect_match(out$classification_reason, "three suppliers")
  out2 <- aib_classify_nodes(dplyr::mutate(nr, optimized_top3_share = 0.6), c(cls, manage_top3_share = 0.75))
  expect_equal(out2$node_classification, "LOW INTERVENTION")
})

test_that("restricting to members moves non-members' shipments into the fixed flows", {
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "KOR", "AUS"), importer_iso = c("USA", "USA", "USA", "CHN"),
                          value = c(50, 20, 10, 30))
  prob <- aib_node_problem(flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1, candidate_min_share = 0)
  r <- aib_restrict_problem(prob, c("USA", "AUS"))
  expect_setequal(r$cand, "AUS")
  expect_equal(r$fixed_total, prob$fixed_total + 30 / 80)
  flat <- list(spec = function(e) rep(0.5, length(e)))
  # All members: Japan and Korea keep 30, Australia's 30 freed -> 60 of 80. Members USA and AUS only:
  # the same 60 (non-members' shipments continue, Australia redirects).
  expect_equal(aib_optimized_analytic(r, "reallocation", 1, flat)$coverage, 60 / 80, tolerance = 1e-9)
  expect_equal(aib_anchor_existing_coverage(prob, "USA", tibble::tibble(e = c("JPN", "KOR"), m = "USA", f = c(20, 10) / 80)), 30 / 80, tolerance = 1e-9)
})

test_that("policy needs keep every class above the floor", {
  mix <- aib_class_mix(c("BUILD", "COORDINATE", "LOW INTERVENTION"), c(65, 35, 100))
  expect_equal(names(mix), c("BUILD", "COORDINATE"))
  needs <- aib_policy_needs(mix, 0.2)
  expect_match(needs, "coordinated finance")
  expect_match(needs, "rules of origin")
  expect_false(grepl("rules of origin", aib_policy_needs(aib_class_mix(c("BUILD", "COORDINATE"), c(90, 10)), 0.2)))
})

test_that("the network pairs trade and investment along the direction goods move", {
  trade <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "r", exporter_iso = "JPN", importer_iso = "USA",
                          delta_usd = 10, delta_share_of_demand = 0.1, relationship = "new")
  inv <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "r", investor_iso = "USA", host_iso = "JPN",
                        supply_from_new_capacity = 0.05, supply_from_new_capacity_usd = 5, relationship = "investment partnership")
  ni <- tibble::tibble(node_id = "n", supply_chain = "Midstream", techs = "Batteries")
  ns <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "r", shock_exposure = 0.5)
  p <- aib_network_pairs(trade, inv, ni, ns, "s")
  expect_equal(nrow(p), 1)
  expect_equal(p$supplier_iso, "JPN")
  expect_equal(p$n_trade_selected, 1)
  expect_equal(p$n_investment_selected, 1)
  expect_equal(p$cumulative_coordination_dividend, 0.1)
  expect_equal(p$cumulative_investment_resilience_contribution, 0.05)
  cs <- aib_network_countries(p)
  expect_equal(cs$supplier_n_selected[cs$iso3 == "JPN"], 1)
  expect_equal(cs$investor_n_selected[cs$iso3 == "USA"], 1)
})
