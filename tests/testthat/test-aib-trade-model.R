repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "categories/policy/cat_policy_index.R")) {
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

test_that("a half shock frees only half, and reallocation can take the rest", {
  prob <- aib_node_problem(toy_flows, c(CHN = 0.5), NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  cons <- aib_optimized_analytic(prob, "conservative", 1, flat_scores)
  real <- aib_optimized_analytic(prob, "reallocation", 1, flat_scores)
  expect_equal(prob$L, 40e6)
  expect_equal(cons$coverage, (55 + 7.5) / 95, tolerance = 1e-9)
  expect_equal(real$coverage, (55 + 15) / 95, tolerance = 1e-9)
})

test_that("shock targets follow the level and carry re-export hubs", {
  node_cells <- tibble::tibble(node_id = c("n1", "n2", "n3"), tech = c("T1", "T1", "T2"), supply_chain = "Midstream",
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
  fr$second_shock_coverage <- fr$optimized_coverage * (1 - fr$optimized_largest_share)
  m <- aib_frontier_minimum(fr, tolerance = 0.02, acceptable_coverage = 0.5)
  expect_equal(m$tightest_costless_max_share, 0.4)
  # Second-shock coverage: 0.45, 0.553, 0.36, 0.24 -> only the 40% limit keeps 50%.
  expect_equal(m$min_required_diversification, 0.4)
  expect_equal(m$specialization_retained, 0.6)
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
