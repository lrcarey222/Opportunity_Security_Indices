repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "utils/supply_chain_stages.R", "categories/policy/cat_policy_index.R")) {
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
  ni <- tibble::tibble(node_id = "n", supply_chain = "Components", techs = "Batteries")
  ns <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "r", shock_exposure = 0.5)
  p <- aib_network_pairs(trade, inv, ni, ns, "s")
  expect_equal(nrow(p), 1)
  expect_equal(p$supplier_iso, "JPN")
  expect_true(p$components)
  expect_false(any(unlist(p[, c("extraction", "refining", "assembly", "deployment", "recovery")])))
  expect_equal(p$n_trade_selected, 1)
  expect_equal(p$n_investment_selected, 1)
  expect_equal(p$cumulative_coordination_dividend, 0.1)
  expect_equal(p$cumulative_investment_resilience_contribution, 0.05)
  cs <- aib_network_countries(p)
  expect_equal(cs$supplier_n_selected[cs$iso3 == "JPN"], 1)
  expect_equal(cs$investor_n_selected[cs$iso3 == "USA"], 1)
  expect_equal(cs$supplier_n_components_partners[cs$iso3 == "JPN"], 1)
  expect_equal(cs$investment_host_n_components_partners[cs$iso3 == "JPN"], 1)
  expect_equal(cs$supplier_n_assembly_partners[cs$iso3 == "JPN"], 0)
})

test_that("the exact coalition is smaller than the greedy one when the greedy first pick is a trap", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  # Two nodes. C1 covers 60% of the anchor's need in both; C2 covers all of node 1, C3 all of node 2.
  mk <- function(other) tibble::tibble(exporter_iso = c("CHN", "CC1", other), importer_iso = c("USA", "ZZZ", "ZZZ"), value = c(100, 60, 100))
  p1 <- aib_node_problem(mk("CC2"), c(CHN = 1), shocked_demand = "hold", candidate_min_exports = 1, candidate_min_share = 0)
  p2 <- aib_node_problem(mk("CC3"), c(CHN = 1), shocked_demand = "hold", candidate_min_exports = 1, candidate_min_share = 0)
  probs <- list(n1 = p1, n2 = p2)
  cands <- c("CC1", "CC2", "CC3")
  path <- aib_coalition_path(probs, c("n1", "n2"), "USA", cands, "reallocation", target = 0.99)
  greedy <- strsplit(utils::tail(path$members, 1), " ")[[1]]
  exact <- aib_coalition_exact(probs, c("n1", "n2"), "USA", cands, "reallocation", target = 0.99)
  expect_true(exact$reached)
  expect_setequal(exact$members, c("USA", "CC2", "CC3"))
  expect_equal(length(greedy), 4)
  expect_lt(exact$size, length(greedy))
  # An unreachable target is reported, not forced.
  far <- aib_coalition_exact(probs, c("n1", "n2"), "USA", "CC1", "reallocation", target = 0.99)
  expect_false(far$reached)
  expect_equal(far$best_min_coverage, 0.6, tolerance = 1e-9)
})

test_that("leave-one-out marks critical members and critical external dependencies", {
  flows <- tibble::tibble(exporter_iso = c("CHN", "KOR", "JPN"), importer_iso = c("USA", "USA", "ZZZ"), value = c(50, 50, 50))
  # Korea is outside the partner universe, so it is not coordinating but still ships to the anchor.
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", partner_universe = c("USA", "JPN", "ZZZ"),
                        candidate_min_exports = 1, candidate_min_share = 0)
  probs <- list(n = p)
  expect_equal(aib_anchor_coverage(p, "USA", c("USA", "JPN"), "reallocation"), 1)
  crit <- aib_coalition_critical(probs, "n", "USA", c("USA", "JPN"), "reallocation", target = 0.95)
  expect_true(crit$critical[crit$iso3 == "KOR"])
  expect_false(crit$member[crit$iso3 == "KOR"])
  expect_true(crit$critical[crit$iso3 == "JPN"])
  expect_true(crit$member[crit$iso3 == "JPN"])
  # Japan is also irreplaceable: no other candidate can make up its supply.
  expect_true(crit$irreplaceable[crit$iso3 == "JPN"])
})

test_that("a member another candidate could replace is not irreplaceable", {
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "DEU"), importer_iso = c("USA", "ZZZ", "ZZZ"), value = c(100, 100, 100))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", candidate_min_exports = 1, candidate_min_share = 0)
  crit <- aib_coalition_critical(list(n = p), "n", "USA", c("USA", "JPN"), "reallocation", target = 0.95, candidates = "DEU")
  expect_true(crit$critical[crit$iso3 == "JPN"])        # needed within the coalition as chosen
  expect_false(crit$irreplaceable[crit$iso3 == "JPN"])  # Germany could take its place
})

test_that("compact coherence follows curated physical chains, not shared stages", {
  cells <- tibble::tibble(
    node_id = c("ree", "metal", "magnet", "motor", "turbine", "blade", "foundation"),
    sub_sector = c("Rare-Earth Compounds", "Rare-Earth Metals", "Permanent Magnets & Magnet Parts",
                   "Electric Drive Units & Motors", "Wind Turbines & Generators", "Wind Blades", "Wind Foundations")
  )
  links <- list(c("Rare-Earth Compounds", "Rare-Earth Metals"), c("Rare-Earth Metals", "Permanent Magnets & Magnet Parts"),
                c("Permanent Magnets & Magnet Parts", "Electric Drive Units & Motors"),
                c("Permanent Magnets & Magnet Parts", "Wind Turbines & Generators"), c("Wind Blades", "Wind Turbines & Generators"))
  e <- aib_chain_edges(cells, links)
  coh <- aib_chain_coherence(e, cells$node_id, max_hops = 3, allow_co_inputs = TRUE)
  expect_true(coh["ree", "motor"])            # oxide -> metal -> magnet -> motor
  expect_true(coh["magnet", "blade"])         # both feed the turbine
  expect_false(coh["magnet", "foundation"])   # same broad stage, no physical link
  expect_false(aib_chain_coherence(e, cells$node_id, allow_co_inputs = FALSE)["magnet", "blade"])
  expect_false(aib_chain_coherence(e, cells$node_id, max_hops = 2)["ree", "motor"])
})

test_that("nodes keep every policy need, not only the headline class", {
  nr <- tibble::tibble(node_id = "x", optimize = TRUE, shock_exposure = 0.5, existing_coverage = 0.6,
                       optimized_coverage = 0.75, residual_gap = 0.25, coordination_dividend = 0.15,
                       optimized_largest_share = 0.5, optimized_top3_share = 0.9, optimized_largest_supplier = "JPN",
                       coverage_loss_at_manage_cap = 0)
  out <- aib_classify_nodes(nr, c(cls, manage_top3_share = 0.75))
  expect_equal(out$node_classification, "BUILD")
  expect_true(out$needs_build && out$needs_coordinate && out$needs_manage_specialization)
  expect_equal(out$policy_needs, "COORDINATE + BUILD + MANAGE SPECIALIZATION")
})

test_that("a member is a critical supply partner only when one substitute cannot replace it", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "DEU", "FRA"), importer_iso = c("USA", "ZZZ", "ZZZ", "ZZZ"),
                          value = c(150, 100, 60, 60))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", candidate_min_exports = 1, candidate_min_share = 0)
  probs <- list(n = p)
  ex <- aib_coalition_exact(probs, "n", "USA", c("JPN", "DEU", "FRA"), "reallocation", target = 0.95)
  expect_equal(ex$size, 3)
  expect_true("JPN" %in% ex$members)
  sub <- aib_coalition_substitution(probs, "n", "USA", ex$members, c("JPN", "DEU", "FRA"), "reallocation", target = 0.95)
  # Without Japan, Germany and France reach only 120 of 150: Japan cannot be replaced at all.
  expect_true(sub$critical[sub$iso3 == "JPN"])
  expect_true(is.na(sub$substitutes_needed[sub$iso3 == "JPN"]))
  # Germany (or France) is replaced one for one.
  other <- setdiff(ex$members, c("USA", "JPN"))
  expect_false(sub$critical[sub$iso3 == other])
  expect_equal(sub$single_substitutes[sub$iso3 == other], setdiff(c("DEU", "FRA"), other))
  # The anchor supplies nothing, so losing its supply changes nothing.
  expect_false(sub$needed[sub$iso3 == "USA"])
  # Dropping an economy removes its current shipments as well as its additions.
  t0 <- aib_anchor_terms(p, "USA", "reallocation")
  t1 <- aib_anchor_terms(p, "USA", "reallocation", drop = "JPN")
  expect_equal(unname(t1$g["JPN"]), 0)
  expect_equal(t1$F, t0$F)
})

test_that("an exporting anchor replaced one for one is not a critical supply partner", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  flows <- tibble::tibble(exporter_iso = c("CHN", "CHN", "CAN", "MEX", "DEU", "FRA"),
                          importer_iso = c("USA", "CAN", "ZZZ", "ZZZ", "ZZZ", "ZZZ"),
                          value = c(100, 50, 60, 70, 20, 65))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", candidate_min_exports = 1, candidate_min_share = 0)
  probs <- list(n = p)
  anchors <- c("USA", "CAN")
  ex <- aib_coalition_exact(probs, "n", anchors, c("CAN", "MEX", "DEU", "FRA"), "reallocation", target = 0.95)
  # Two additions are needed; of the size-2 sets, MEX+FRA and MEX+DEU clear the target by the full
  # margin (FRA+DEU only by 1.7 points), and MEX+FRA adds the most supply.
  expect_setequal(ex$members, c("USA", "CAN", "MEX", "FRA"))
  sub <- aib_coalition_substitution(probs, "n", anchors, ex$members, c("CAN", "MEX", "DEU", "FRA"), "reallocation", target = 0.95)
  expect_true(sub$needed[sub$iso3 == "CAN"])
  expect_false(sub$critical[sub$iso3 == "CAN"])          # Germany takes Canada's place
  expect_equal(sub$single_substitutes[sub$iso3 == "CAN"], "DEU")
  # Held-in members: the 99% coalition built on the 95% one only adds to it.
  nested <- aib_coalition_exact(probs, "n", anchors, c("CAN", "MEX", "DEU", "FRA"), "reallocation", target = 0.99,
                                fixed = ex$members)
  expect_true(all(ex$members %in% nested$members))
})

test_that("leave-one-out flags a material loss where the target is out of reach", {
  flows <- tibble::tibble(exporter_iso = c("CHN", "KOR", "JPN"), importer_iso = c("USA", "USA", "ZZZ"), value = c(80, 20, 50))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", partner_universe = c("USA", "JPN", "ZZZ"),
                        candidate_min_exports = 1, candidate_min_share = 0)
  crit <- aib_coalition_critical(list(n = p), "n", "USA", c("USA", "JPN"), "reallocation", target = 0.95)
  # Coverage with Japan coordinating is 70%: below target, but Korea's 20 points still matter.
  expect_equal(crit$coverage_with[crit$iso3 == "KOR"], 0.7)
  expect_true(crit$critical[crit$iso3 == "KOR"])
})

test_that("a compact linked only through another compact's node is split", {
  cells <- tibble::tibble(node_id = c("a", "b", "c", "d"), sub_sector = c("A", "B", "C", "D"))
  e <- aib_chain_edges(cells, list(c("A", "B"), c("B", "C"), c("C", "D")))
  # a and d are three hops apart through b and c.
  expect_true(aib_chain_coherence(e, cells$node_id, max_hops = 3)["a", "d"])
  expect_false(aib_chain_coherence(e, cells$node_id, max_hops = 3, through = c("a", "d"))["a", "d"])
  split <- aib_split_disconnected(tibble::tibble(node_id = c("a", "d", "b", "c"), group = c(1L, 1L, 2L, 2L)), e, cells$node_id)
  expect_equal(dplyr::n_distinct(split$group), 3)
  expect_false(split$group[split$node_id == "a"] == split$group[split$node_id == "d"])
  # The same chain with b and c in no compact stays together.
  kept <- aib_split_disconnected(tibble::tibble(node_id = c("a", "d"), group = c(1L, 1L)), e, cells$node_id)
  expect_equal(dplyr::n_distinct(kept$group), 1)
})

test_that("under a supplier-share cap the compact never passes the scenario's own optimum", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  flat <- list(spec = function(e) rep(0.5, length(e)), op = function(e, m) rep(0.5, length(e)),
               compat = function(e, m) rep(0.5, length(e)))
  # China holds 80% of the scope's supply and is capped at 30% (it gives up 50); Japan and Korea ship 10 each
  # to the scope and 50 each elsewhere, so uncapped they could replace all of it, but capped only 20 each.
  mk <- function(cap, other = 50) {
    fl <- tibble::tibble(exporter_iso = c("CHN", "CHN", "JPN", "KOR", "JPN", "KOR"), importer_iso = c("USA", "DEU", "USA", "DEU", "ZZZ", "ZZZ"),
                         value = c(60, 20, 10, 10, other, other))
    p <- aib_node_problem(fl, c(CHN = 1 - 0.3 / 0.8), shocked_demand = "hold", demand_scope = c("USA", "DEU"),
                          candidate_min_exports = 1, candidate_min_share = 0, cap_basis = "demand")
    p$share_cap <- cap
    p
  }
  probs <- list(n1 = mk(0.3), n2 = mk(0.3, other = 30))
  anchors <- c("USA", "DEU")
  members <- c("USA", "DEU", "JPN", "KOR")
  for (n in names(probs)) {
    p <- probs[[n]]
    optimum <- aib_solve_state(p, "reallocation", "C", cap = 0.3, scores = flat)$coverage
    existing <- aib_solve_state(p, "reallocation", "B", cap = 0.3, scores = flat, existing_growth = 1)$coverage
    trade <- aib_compact_trade_coverage(p, "reallocation", members, scores = flat, existing_growth = 1)
    expect_equal(optimum, 0.9, tolerance = 1e-6, info = n)
    expect_lte(trade, optimum + 1e-6)
    expect_gte(trade, existing - 1e-6)
    expect_equal(trade, 0.9, tolerance = 1e-6, info = n)
    # The anchors are the whole scope here, so their coverage cannot pass the optimum either; each member adds 20.
    expect_equal(aib_anchor_coverage(p, anchors, members, "reallocation"), 0.9, tolerance = 1e-9)
    expect_equal(unname(aib_anchor_terms(p, anchors, "reallocation")$g[c("JPN", "KOR")]), c(0.2, 0.2), tolerance = 1e-9)
    # Synthetic capacity at a member host is capped too.
    expect_equal(unname(aib_anchor_terms(p, anchors, "reallocation", host_extra = c(JPN = 0.5))$g["JPN"]), 0.2, tolerance = 1e-9)
    # Without the cap the same members would restore everything.
    u <- p
    u$share_cap <- NULL
    expect_equal(aib_compact_trade_coverage(u, "reallocation", members, scores = flat, existing_growth = 1), 1, tolerance = 1e-6)
    expect_equal(aib_anchor_coverage(u, anchors, members, "reallocation"), 1)
  }
  # The 95% target is out of reach under the cap: no coalition is forced onto it.
  ex <- aib_coalition_exact(probs, names(probs), anchors, c("JPN", "KOR"), "reallocation", target = 0.95)
  expect_false(ex$reached)
  expect_lte(ex$best_min_coverage, 0.9 + 1e-9)
  path <- aib_coalition_path(probs, names(probs), anchors, c("JPN", "KOR"), "reallocation", target = 0.99)
  expect_lte(max(path$min_coverage), 0.9 + 1e-9)
  # A served-basis problem ignores share_cap in the coalition terms (its cap is not linear in the membership).
  s <- probs$n1
  s$cap_basis <- "served"
  expect_equal(aib_compact_share_cap(s), 1)
})
