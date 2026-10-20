repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R")) source(file.path(repo_root, "R", f))
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
suppressPackageStartupMessages(library(dplyr))

test_that("capping shares moves the excess to the others in proportion, repeatedly", {
  s <- c(A = 0.6, B = 0.3, C = 0.1)
  out <- aib_capped_shares(s, 0.4)
  expect_equal(sum(out), 1)
  expect_equal(unname(out["A"]), 0.4)
  # The 0.2 excess goes 3:1 to B and C: B 0.45 > 0.4, so B is capped too and C takes the rest.
  expect_equal(unname(out["B"]), 0.4)
  expect_equal(unname(out["C"]), 0.2, tolerance = 1e-12)
  expect_equal(aib_hhi(c(1, 1, 2)), (0.25^2) * 2 + 0.5^2)
})

test_that("the cap brings the HHI to the benchmark, or reports that it cannot", {
  s <- c(CHN = 0.6, DEU = 0.2, JPN = 0.1, KOR = 0.1)
  res <- aib_diversify_cap(s, 0.3)
  expect_true(res$reached)
  expect_equal(sum(aib_capped_shares(s, res$cap)^2), 0.3, tolerance = 1e-6)
  expect_true(is.na(aib_diversify_cap(s, 0.5)$cap))                     # already below 0.5
  low <- aib_diversify_cap(s, 0.2)                                       # four suppliers cannot go below 0.25
  expect_false(low$reached)
  expect_equal(low$cap, 0.25)
})

test_that("the parent-heading benchmark leaves the node's own lines out; the sector one uses the other nodes", {
  node_codes <- tibble::tibble(node_id = c("ev", "ev", "cells"), hs6 = c("870380", "870360", "850760"))
  ph <- tibble::tibble(hs6 = c("870321", "870323", "870390", "850710", "850780"),
                       world = c(100, 300, 400, 50, 200), hhi = c(0.06, 0.10, 0.12, 0.07, 0.30))
  own <- tibble::tibble(node_id = c("ev", "cells"), hs6 = c("870390", "850780"))   # HS92 lines holding the node's codes
  nh <- tibble::tibble(node_id = c("ev", "bus", "cells"), sector = c("EVs", "EVs", "Cells"), hhi = c(0.2, 0.1, 0.4), world_trade = c(10, 30, 5))
  b <- aib_benchmark_hhi(node_codes, ph, own, nh)
  expect_equal(b$benchmark_hs4[b$node_id == "ev"], (0.06 * 100 + 0.10 * 300) / 400)   # 870390 left out
  expect_equal(b$n_benchmark_lines[b$node_id == "ev"], 2)
  expect_equal(b$benchmark_hs4[b$node_id == "cells"], 0.07)
  expect_equal(b$benchmark_sector[b$node_id == "ev"], 0.1)                             # the bus node
  expect_true(is.na(b$benchmark_sector[b$node_id == "cells"]))                         # alone in its sector
})

test_that("diversification targets fold re-export hubs into their origin and cap the dominant suppliers", {
  em <- tibble::tibble(node_id = c("n", "n", "n", "n", "m", "m"),
                       exporter_iso = c("CHN", "HKG", "DEU", "JPN", "DEU", "JPN"),
                       exports = c(50, 10, 20, 20, 50, 50))
  bm <- tibble::tibble(node_id = c("n", "m"), benchmark_hs4 = c(0.3, 0.6), benchmark_hs4_headings = c("8507", "8507"),
                       benchmark_sector = NA_real_)
  t <- aib_diversify_targets(em, bm, "hs4", reexport_hubs = list(CHN = c("HKG", "MAC")))
  n <- t[t$node_id == "n", ]
  expect_equal(n$current_hhi, 0.6^2 + 0.2^2 + 0.2^2)                                  # China with Hong Kong: 60%
  expect_equal(n$target_iso, "CHN")
  key <- aib_parse_shock_key(n$shock_key)
  expect_equal(unname(key["CHN"]), 1 - n$cap / 0.6, tolerance = 1e-6)
  expect_equal(unname(key["HKG"]), unname(key["CHN"]))                                 # the hub loses the same share
  expect_true(is.na(t$cap[t$node_id == "m"]))                                          # 0.5 HHI is below its 0.6 benchmark
  st <- aib_shock_targets(list(list(id = "div", type = "diversify")), tibble::tibble(node_id = c("n", "m"), tech = "T",
                          supply_chain = "Midstream", sub_sector = c("N", "M")), em, diversify = t)
  expect_equal(st$node_id, "n")
  expect_equal(st$cap, n$cap)
})

test_that("the excess is measured where the cap binds: the demand scope's supply", {
  em <- tibble::tibble(node_id = "n", exporter_iso = c("CHN", "DEU", "JPN", "KOR"), exports = c(40, 20, 20, 20))
  bm <- tibble::tibble(node_id = "n", benchmark_hs4 = 0.25, benchmark_hs4_headings = "8507", benchmark_sector = NA_real_)
  # Worldwide China has 40%; in the allied market Japan has 60%.
  sc <- tibble::tibble(node_id = "n", exporter_iso = c("CHN", "JPN", "KOR"), value = c(10, 60, 30))
  t <- aib_diversify_targets(em, bm, "hs4", scope_supply = sc)
  key <- aib_parse_shock_key(t$shock_key)
  expect_setequal(names(key), c("JPN", "KOR"))
  expect_equal(unname(key["JPN"]), 1 - t$cap / 0.6, tolerance = 1e-6)
  expect_equal(t$excess_share, (0.6 - t$cap) + (0.3 - t$cap), tolerance = 1e-9)
  expect_equal(t$target_iso, "JPN+KOR")
})

div_scores <- list(
  spec = function(e) rep(0.5, length(e)),
  op = function(e, m) rep(0.5, length(e)),
  op_raw = function(e, m) rep(NA_real_, length(e)),
  compat = function(a, b) rep(0.5, length(a))
)

test_that("the diversification cap is a share of demand, so trade never takes coverage below the static state", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  # China has 80% of the scope's 100 and is capped at 30%: 50 lost, static 50%. Japan cannot expand.
  fl <- tibble::tibble(exporter_iso = c("CHN", "CHN", "JPN"), importer_iso = c("XXX", "YYY", "YYY"), value = c(70, 10, 20) * 1e6)
  key <- c(CHN = 1 - 0.3 / 0.8)
  served <- aib_node_problem(fl, key, NULL, "hold", candidate_min_exports = 1e6, candidate_min_share = 0)
  demand <- aib_node_problem(fl, key, NULL, "hold", candidate_min_exports = 1e6, candidate_min_share = 0, cap_basis = "demand")
  expect_equal(aib_static_state(demand)$coverage, 0.5)
  # On the served basis Japan's 20 is above 30% of the shrunken market and is cut: coverage falls.
  expect_lt(aib_solve_state(served, "reallocation", "C", cap = 0.3, scores = div_scores)$coverage, 0.5 - 0.05)
  for (st in c("B", "C")) {
    expect_equal(aib_solve_state(demand, "reallocation", st, cap = 0.3, scores = div_scores, existing_growth = 1)$coverage, 0.5, tolerance = 1e-7)
  }
  r <- aib_run_node_shock(demand, c("conservative", "reallocation"), c(0.3, 1), 0.3, div_scores, c(0.0005, 0.02, 0.02), existing_cap = 0.3)
  expect_true(all(r$summary$existing_coverage >= r$summary$static_coverage - 1e-9))
  expect_true(all(r$summary$optimized_coverage >= r$summary$existing_coverage - 1e-9))
})

test_that("on the demand basis the closed form matches the LP and no exporter passes cap x demand", {
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  fl <- tibble::tibble(exporter_iso = c("CHN", "CHN", "JPN", "KOR", "KOR", "DEU"),
                       importer_iso = c("XXX", "YYY", "YYY", "XXX", "ZZZ", "ZZZ"), value = c(60, 20, 10, 5, 40, 15) * 1e6)
  prob <- aib_node_problem(fl, c(CHN = 0.5), NULL, "hold", candidate_min_exports = 1e6, candidate_min_share = 0, cap_basis = "demand")
  for (sc in c("conservative", "reallocation")) for (cp in c(0.2, 0.35, 0.6)) {
    lp <- aib_solve_state(prob, sc, "C", cap = cp, scores = div_scores, steps = 3)
    an <- aib_optimized_analytic(prob, sc, cap = cp, scores = div_scores)
    expect_equal(an$coverage, lp$coverage, tolerance = 1e-6, info = paste(sc, cp))
    tot <- tapply(lp$flows$f, lp$flows$e, sum)
    expect_true(all(tot <= pmax(cp, tapply(prob$links$x, prob$links$e, sum)[names(tot)] * (sc == "conservative")) + 1e-7), info = paste(sc, cp))
  }
})

test_that("a capped ally's own purchases stay in the protected demand", {
  fl <- tibble::tibble(exporter_iso = c("CHN", "DEU", "JPN"), importer_iso = c("XXX", "XXX", "DEU"), value = c(50, 40, 10) * 1e6)
  key <- c(DEU = 0.25)
  shocked <- aib_node_problem(fl, key, NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0)
  ally <- aib_node_problem(fl, key, NULL, "release", candidate_min_exports = 1e6, candidate_min_share = 0, protect_shocked = TRUE)
  expect_equal(sort(shocked$scope), "XXX")
  expect_equal(sort(ally$scope), c("DEU", "XXX"))
  expect_equal(ally$D, 100e6)
  expect_equal(ally$L / ally$D, 0.1)
  expect_equal(sum(ally$freed), 0)                       # its purchases are demand to serve, not freed supply
  expect_equal(unname(ally$to_scope["JPN"]), 0.1)
})
