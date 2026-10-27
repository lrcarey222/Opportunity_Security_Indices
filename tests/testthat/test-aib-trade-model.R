repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "utils/supply_chain_stages.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
suppressPackageStartupMessages(library(dplyr))

# A toy node. China supplies X and Y; A sells only to Z (which loses nothing) and to China; B sells
# to Y. Removing China (both directions) frees A's 15 of exports to China, but A's existing
# customers do not need it: only a new A -> X or A -> Y relationship puts it to use.
toy_flows <- tibble::tibble(
  exporter_iso = c("CHN", "CHN", "AAA", "AAA", "BBB"),
  importer_iso = c("XXX", "YYY", "ZZZ", "CHN", "YYY"),
  value = c(60, 20, 5, 15, 10) * 1e6
)
flat_scores <- list(
  spec = function(e) rep(0.5, length(e)),
  op = function(e, m) rep(0.5, length(e)),
  op_raw = function(e, m) rep(NA_real_, length(e)),
  compat = function(a, b) rep(0.5, length(a))
)

test_that("the node problem measures exposure on the universe's demand", {
  prob <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  expect_equal(sort(prob$scope), c("XXX", "YYY", "ZZZ"))
  expect_equal(prob$D, 95e6)
  expect_equal(prob$L, 80e6)
  expect_equal(unname(prob$freed["AAA"]), 15 / 95)
  expect_equal(aib_static_state(prob)$coverage, 15 / 95)
  hold <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "hold", candidate_min_exports = 1e6, candidate_min_share = 0)
  expect_equal(sum(hold$freed), 0)
})

test_that("existing relationships cannot use freed supply that new ones can (the coordination dividend)", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  prob <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  r <- aib_run_node_shock(prob, c("conservative", "reallocation"), c(0.4, 1), 1, flat_scores, c(0.01, 0.02, 0.02))
  s <- r$summary[r$summary$scenario == "conservative", ]
  expect_equal(s$static_coverage, 15 / 95, tolerance = 1e-9)
  expect_equal(s$existing_coverage, 15 / 95, tolerance = 1e-9)
  expect_equal(s$optimized_coverage, 30 / 95, tolerance = 1e-9)
  expect_equal(s$coordination_dividend, 15 / 95, tolerance = 1e-9)
  expect_equal(s$residual_gap, 65 / 95, tolerance = 1e-9)
  p <- r$partnerships[r$partnerships$scenario == "conservative", ]
  expect_true(all(p$exporter_iso == "AAA"))
  expect_true(all(p$relationship == "new"))
  expect_equal(sum(p$delta_usd), 15e6, tolerance = 1)
})

test_that("a supplier-share limit binds and the closed form matches the LP", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  prob <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  # Conservative, limit 0.4: B's locked 10 already exceeds 0.4 of the static 15, so B may not
  # expand; A may reach 0.4 of the total: T = 10 + 0.4 T -> T = 50/3.
  lp <- aib_solve_state(prob, "conservative", "C", cap = 0.4, scores = flat_scores, steps = 3)
  an <- aib_optimized_analytic(prob, "conservative", cap = 0.4, scores = flat_scores)
  expect_equal(lp$coverage, (50 / 3) / 95, tolerance = 1e-7)
  expect_equal(an$coverage, lp$coverage, tolerance = 1e-7)
  expect_equal(an$hhi, lp$hhi, tolerance = 1e-6)
  for (sc in c("conservative", "reallocation")) for (cp in c(0.25, 0.6, 1)) {
    lp <- aib_solve_state(prob, sc, "C", cap = cp, scores = flat_scores, steps = 3)
    an <- aib_optimized_analytic(prob, sc, cap = cp, scores = flat_scores)
    expect_equal(an$coverage, lp$coverage, tolerance = 1e-6, info = paste(sc, cp))
  }
})

test_that("reallocation never reshuffles shipments the shock did not touch", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  flows <- dplyr::bind_rows(toy_flows, tibble::tibble(exporter_iso = "BBB", importer_iso = "WWW", value = 40e6))
  prob <- aib_node_problem(flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  # A strong Export Opportunity pull toward YYY must not move B's shipments away from WWW.
  sc <- flat_scores
  sc$op <- function(e, m) ifelse(m == "YYY", 1, 0)
  s <- aib_solve_state(prob, "reallocation", "C", cap = 1, scores = sc)
  bw <- s$flows$f[s$flows$e == "BBB" & s$flows$m == "WWW"]
  expect_equal(bw, 40 / 135, tolerance = 1e-9)
  expect_equal(s$surviving_cut, 0, tolerance = 1e-12)
})

test_that("a half shock frees only half of the sales to the shocked economy", {
  prob <- aib_node_problem(toy_flows, c(CHN = 0.5), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  cons <- aib_optimized_analytic(prob, "conservative", 1, flat_scores)
  real <- aib_optimized_analytic(prob, "reallocation", 1, flat_scores)
  expect_equal(prob$L, 40e6)
  expect_equal(cons$coverage, (55 + 7.5) / 95, tolerance = 1e-9)
  # A sells only to Z (in scope) and China: the half of its China sales the shock frees is all it can move.
  expect_equal(real$coverage, (55 + 7.5) / 95, tolerance = 1e-9)
  extra <- dplyr::bind_rows(toy_flows, tibble::tibble(exporter_iso = "AAA", importer_iso = "WWW", value = 4e6))
  p2 <- aib_node_problem(extra, c(CHN = 0.5), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0,
                         demand_scope = c("XXX", "YYY", "ZZZ"))
  # Reallocation adds A's sales outside the scope (4 to W).
  expect_equal(aib_optimized_analytic(p2, "reallocation", 1, flat_scores)$coverage, (55 + 7.5 + 4) / 95, tolerance = 1e-9)
})

test_that("shock targets follow the level and carry re-export hubs", {
  node_cells <- tibble::tibble(node_id = c("n1", "n2", "n3"), tech = c("T1", "T1", "T2"), supply_chain = "Components",
                               sub_sector = c("S1", "S2", "S3"))
  em <- tibble::tibble(
    node_id = c("n1", "n1", "n2", "n2", "n3", "n3"),
    exporter_iso = c("CHN", "JPN", "CHN", "JPN", "KOR", "CHN"),
    exports = c(10, 20, 50, 5, 9, 1)
  )
  shocks <- list(
    list(id = "china_100", type = "country", countries = list("CHN"), fraction = 1),
    list(id = "largest_subsector", type = "largest_supplier", level = "subsector", fraction = 1),
    list(id = "largest_stage", type = "largest_supplier", level = "stage", fraction = 1)
  )
  t <- aib_shock_targets(shocks, node_cells, em, reexport_hubs = list(CHN = c("HKG")))
  ch <- t[t$shock_id == "china_100", ]
  expect_true(all(ch$shock_key == "CHN:1,HKG:1"))
  sub <- t[t$shock_id == "largest_subsector", ]
  expect_equal(sub$target_iso[sub$node_id == "n1"], "JPN")
  expect_equal(sub$shock_key[sub$node_id == "n2"], "CHN:1,HKG:1")
  st <- t[t$shock_id == "largest_stage", ]
  # T1 stage: CHN 60 vs JPN 25 -> China for both T1 nodes, even n1 whose own leader is Japan.
  expect_equal(st$target_iso[st$node_id == "n1"], "CHN")
  expect_equal(st$target_iso[st$node_id == "n3"], "KOR")
})

test_that("the frontier gives the costless limit and the minimum required diversification", {
  fr <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "reallocation", max_supplier_share = c(0.25, 0.4, 0.6, 1),
                       optimized_coverage = c(0.6, 0.79, 0.8, 0.8), optimized_largest_share = c(0.25, 0.3, 0.55, 0.7),
                       specialization_score = c(0.5, 0.6, 0.8, 1))
  fr$coverage_loss_vs_unconstrained <- 0.8 - fr$optimized_coverage
  fr$instantaneous_n1_retained_coverage <- fr$optimized_coverage * (1 - fr$optimized_largest_share)
  fr$n1_reoptimized_coverage <- c(0.58, 0.7, 0.6, 0.45)
  m <- aib_frontier_minimum(fr, tolerance = 0.02, acceptable_coverage = 0.5)
  expect_equal(m$tightest_costless_max_share, 0.4)
  # True N-1 (re-optimized): 0.58, 0.70, 0.60, 0.45 -> the loosest limit keeping 50% is 60%.
  expect_equal(m$min_required_diversification, 0.6)
  # Instantaneous N-1: 0.45, 0.553, 0.36, 0.24 -> only the 40% limit keeps 50%.
  expect_equal(m$min_required_diversification_instantaneous, 0.4)
  expect_equal(m$specialization_retained, 0.8)
  none <- aib_frontier_minimum(fr, acceptable_coverage = 0.9)
  expect_true(is.na(none$min_required_diversification))
  na_run <- dplyr::mutate(fr, coverage_loss_vs_unconstrained = NA_real_)
  expect_silent(aib_frontier_minimum(na_run))
})

test_that("in state B an existing relationship grows only up to the configured bound", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  # B sells 10 to Y, which loses China's 20; removing China frees B's 30 of exports to China. With
  # unbounded growth the existing B -> Y relationship absorbs all 20; bounded at +50% it can carry
  # only 5 more, and the rest of the restoration needs coordination (state C).
  flows <- tibble::tibble(exporter_iso = c("CHN", "BBB", "BBB"), importer_iso = c("YYY", "YYY", "CHN"), value = c(20, 10, 30) * 1e6)
  prob <- aib_node_problem(flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  unbounded <- aib_solve_state(prob, "conservative", "B", scores = flat_scores, existing_growth = Inf)
  bounded <- aib_solve_state(prob, "conservative", "B", scores = flat_scores, existing_growth = 0.5)
  expect_equal(unbounded$coverage, 1, tolerance = 1e-9)
  expect_equal(bounded$coverage, 15 / 30, tolerance = 1e-9)
  r <- aib_run_node_shock(prob, "conservative", 1, 1, flat_scores, c(0.01, 0.02, 0.02), existing_growth = 0.5)
  expect_equal(r$summary$coordination_dividend, 0.5, tolerance = 1e-9)
})

test_that("the trade-compact state lets members open relationships while others keep the existing response", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  prob <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  b <- aib_solve_state(prob, "conservative", "B", scores = flat_scores, existing_growth = 1, steps = 1)
  with_a <- aib_solve_state(prob, "conservative", "B", scores = flat_scores, existing_growth = 1, steps = 1, coordinating = "AAA")
  with_b <- aib_solve_state(prob, "conservative", "B", scores = flat_scores, existing_growth = 1, steps = 1, coordinating = "BBB")
  expect_equal(b$coverage, 15 / 95, tolerance = 1e-9)
  expect_equal(with_a$coverage, 30 / 95, tolerance = 1e-9)  # A's freed 15 reaches X and Y
  expect_equal(with_b$coverage, 15 / 95, tolerance = 1e-9)  # B has nothing to redirect
})


test_that("hold keeps the shocked economy's purchases; release frees them", {
  hold <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "hold", candidate_min_exports = 1e6, candidate_min_share = 0)
  rel <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  expect_equal(sum(hold$freed), 0)
  expect_equal(unname(rel$freed["AAA"]), 15 / 95)
  # Conservative + hold: nothing the shock frees, so nothing moves (the no-adaptation bound).
  expect_equal(aib_optimized_analytic(hold, "conservative", 1, flat_scores)$coverage, 15 / 95, tolerance = 1e-9)
  # Reallocation + hold: China keeps buying, so A's sales to China stay with it; A has no other market.
  expect_equal(aib_optimized_analytic(hold, "reallocation", 1, flat_scores)$coverage, 15 / 95, tolerance = 1e-9)
  # Reallocation + release: China's purchases fall, so A's 15 move to the scope.
  expect_equal(aib_optimized_analytic(rel, "reallocation", 1, flat_scores)$coverage, 30 / 95, tolerance = 1e-9)
})

test_that("the demand scope and the partner universe are separate", {
  flows <- dplyr::bind_rows(toy_flows, tibble::tibble(exporter_iso = "AAA", importer_iso = "WWW", value = 20e6))
  # Protect only X and Y; anyone may supply. A can divert its 5 to Z and 20 to W (outside the scope);
  # under hold its 15 to China stay with China.
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", demand_scope = c("XXX", "YYY"),
                        candidate_min_exports = 1e6, candidate_min_share = 0)
  expect_setequal(p$scope, c("XXX", "YYY"))
  expect_true("AAA" %in% p$cand)
  expect_equal(unname(p$outside_scope["AAA"]), 25 / 90)  # 5 to Z and 20 to W, outside the scope
  r <- aib_optimized_analytic(p, "reallocation", 1, flat_scores)
  expect_equal(r$coverage, (10 + 25) / 90, tolerance = 1e-9)  # B's 10 stays; A's 25 diverted
  expect_equal(r$diversion, 25 / 90, tolerance = 1e-9)
  # The same scope with A outside the partner universe: A's shipments stay where they are.
  q <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", demand_scope = c("XXX", "YYY"),
                        partner_universe = c("BBB"), candidate_min_exports = 1e6, candidate_min_share = 0)
  expect_false("AAA" %in% q$cand)
  expect_equal(aib_optimized_analytic(q, "reallocation", 1, flat_scores)$coverage, 10 / 90, tolerance = 1e-9)
})

test_that("physical evidence decides who can supply, not how much", {
  # Release: A's 15 sales to China are freed. A exports 20 in all (5 to Z, 15 to China).
  free <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  unc <- aib_optimized_analytic(free, "reallocation", 1, flat_scores)$coverage
  expect_equal(unc, 30 / 95, tolerance = 1e-9)
  # A listed as a producer, however small its share: its observed exports count in full.
  listed <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0,
                             physical_share = c(CHN = 0.9, AAA = 0.02, BBB = 0.08))
  expect_true(listed$physical_producer[["AAA"]])
  expect_equal(unname(listed$supply_cap["AAA"]), 20 / 95, tolerance = 1e-12)
  expect_equal(aib_optimized_analytic(listed, "reallocation", 1, flat_scores)$coverage, unc, tolerance = 1e-9)
  # A absent from the physical data (a re-exporter, or another product under the code): it keeps its
  # 5 to Z but cannot redirect its 15.
  absent <- aib_node_problem(toy_flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0,
                             physical_share = c(CHN = 0.9, BBB = 0.1))
  expect_false(absent$physical_producer[["AAA"]])
  expect_equal(unname(absent$supply_cap["AAA"]), 5 / 95, tolerance = 1e-12)
  con <- aib_optimized_analytic(absent, "reallocation", 1, flat_scores)$coverage
  expect_equal(con, 15 / 95, tolerance = 1e-9)
  expect_equal(aib_optimized_analytic(absent, "conservative", 1, flat_scores)$coverage, 15 / 95, tolerance = 1e-9)
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  expect_equal(aib_solve_state(absent, "reallocation", "C", scores = flat_scores, steps = 1)$coverage, con, tolerance = 1e-7)
})

test_that("the true N-1 removes the largest remaining supplier and re-optimizes", {
  flows <- dplyr::bind_rows(toy_flows, tibble::tibble(exporter_iso = "BBB", importer_iso = "WWW", value = 40e6))
  p <- aib_node_problem(flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  c0 <- aib_optimized_analytic(p, "reallocation", 1, flat_scores)
  expect_equal(c0$largest, "BBB")
  n1 <- aib_n1_reoptimized(p, "reallocation", 1, "BBB", flat_scores)
  # Without B: Y and W lose B's 50; A's 20 is all that can be redirected.
  expect_equal(n1, 20 / 135, tolerance = 1e-9)  # only A's 20 of exports remain to redirect
  p1 <- aib_remove_supplier(p, "BBB")
  expect_false("BBB" %in% p1$cand)
  expect_equal(p1$L, p$L + 50e6)
  expect_equal(aib_instantaneous_n1(c0$coverage, c0$largest_share), c0$coverage * (1 - c0$largest_share))
})

test_that("existing-network coverage rises with the growth bound", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  flows <- tibble::tibble(exporter_iso = c("CHN", "BBB", "BBB"), importer_iso = c("YYY", "YYY", "CHN"), value = c(20, 10, 30) * 1e6)
  p <- aib_node_problem(flows, c(CHN = 1), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  g <- aib_existing_growth_sensitivity(p, "conservative", c(0.25, 1, 3, Inf), flat_scores)
  expect_equal(g$existing_coverage, c(12.5, 20, 30, 30) / 30, tolerance = 1e-9)
})
