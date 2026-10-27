repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
source(file.path(repo_root, "R", "dashboard", "aib", "aib_check.R"))
suppressPackageStartupMessages(library(dplyr))

bloc <- c(USA = "U.S.-leaning", DEU = "U.S.-leaning", VNM = "China-leaning", RUS = "China-leaning", MMR = "China-leaning",
          CHE = "Nonaligned", MDG = "Nonaligned", TWN = "Nonaligned")
partners <- c("USA", "DEU", "VNM", "RUS", "MMR", "CHE", "MDG", "TWN", "XKX")
scope <- c("USA", "DEU", "VNM")

test_that("the bloc screen keeps the demand scope and every economy outside the excluded blocs", {
  s <- aib_host_screen(partners, scope, bloc, mode = "bloc")
  expect_equal(sort(s$iso3[s$screened]), c("MMR", "RUS"))
  # A China-leaning economy in the demand scope stays a host: the AIB restores its demand.
  expect_false(s$screened[s$iso3 == "VNM"])
  # Nonaligned economies outside the coalition tiers (Taiwan, Switzerland, Madagascar) stay hosts.
  expect_false(any(s$screened[s$iso3 %in% c("CHE", "MDG", "TWN")]))
  # An economy without a bloc is kept.
  expect_false(s$screened[s$iso3 == "XKX"])
  expect_true(is.na(s$bloc[s$iso3 == "XKX"]))
  expect_match(s$screen_reason[s$iso3 == "RUS"], "China-leaning")
  expect_true(all(is.na(s$screen_reason[!s$screened])))
})

test_that("the screen modes: none keeps everyone, demand_scope keeps only the demand scope", {
  expect_false(any(aib_host_screen(partners, scope, bloc, mode = "none")$screened))
  d <- aib_host_screen(partners, scope, bloc, mode = "demand_scope")
  expect_equal(sort(d$iso3[!d$screened]), sort(scope))
  # Several excluded blocs, and no bloc data at all (nothing screened by bloc).
  s2 <- aib_host_screen(partners, scope, bloc, mode = "bloc", exclude_blocs = c("China-leaning", "Nonaligned"))
  expect_equal(sort(s2$iso3[s2$screened]), c("CHE", "MDG", "MMR", "RUS", "TWN"))
  expect_false(any(aib_host_screen(partners, scope, NULL, mode = "bloc")$screened))
  expect_error(aib_host_screen(partners, scope, bloc, mode = "friendshore"))
})

test_that("the shortlist keeps the largest hosts by scale, whatever their capability tier", {
  h <- tibble::tibble(host_iso = c("DEU", "SVK", "CZE", "LTU", "AUT"), capability_tier = c(2L, 3L, 3L, 3L, 1L),
                      capability_value = c(0.068, 0.004, 0.02, 0.002, 0.05), host_scale_usd = c(6.8e8, 4e7, 2e8, 2e7, 0))
  s <- aib_host_shortlist(h, 3)
  # Germany (tier 2, the largest exporter) is in; the tier-1 host with no scale in the node is not.
  expect_equal(s$host_iso, c("DEU", "CZE", "SVK"))
  expect_equal(nrow(aib_host_shortlist(h, 20)), 5)
  # Ties on scale: capability tier, then value, then ISO3.
  t <- tibble::tibble(host_iso = c("B", "A", "C"), capability_tier = c(1L, 3L, 3L), capability_value = c(0.1, 0.01, 0.02), host_scale_usd = 0)
  expect_equal(aib_host_shortlist(t, 3)$host_iso, c("C", "A", "B"))
})

test_that("the alignment cost is the best screened gain over the best eligible one, floored at 0", {
  k <- c("node_id", "shock_id", "scenario", "increment")
  base <- tibble::tibble(shock_id = "s", scenario = "r", increment = 0.1)
  ranked <- dplyr::bind_cols(tibble::tibble(node_id = c("a", "a", "b"), host_iso = c("USA", "JPN", "DEU"),
                                            marginal_resilience_gain_diversified = c(0.10, 0.08, 0.053)), base[rep(1, 3), ])
  screened <- dplyr::bind_cols(tibble::tibble(node_id = c("a", "b", "b"), host_iso = c("MMR", "RUS", "IRN"),
                                              marginal_resilience_gain_diversified = c(0.10, 0.10, 0.07)), base[rep(1, 3), ])
  ac <- aib_alignment_cost(ranked, screened, k) %>% arrange(node_id)
  expect_equal(ac$best_screened_host, c("MMR", "RUS"))
  expect_equal(ac$alignment_cost, c(0, 0.047))
  expect_equal(ac$best_eligible_gain, c(0.10, 0.053))
  # A node whose every host is screened: the cost is the screened gain.
  only <- aib_alignment_cost(ranked[ranked$node_id == "a", ], screened, k) %>% arrange(node_id)
  expect_equal(only$alignment_cost[only$node_id == "b"], 0.10)
  # Screened hosts tied on the gain: the larger one is named (Myanmar over Laos in rare-earth oxides).
  tie <- dplyr::bind_cols(tibble::tibble(node_id = "a", host_iso = c("LAO", "MMR"), marginal_resilience_gain_diversified = 0.10,
                                         host_scale_usd = c(1e8, 3e8)), base[rep(1, 2), ])
  expect_equal(aib_alignment_cost(ranked, tie, k)$best_screened_host, "MMR")
  # Nothing screened: no rows, the keys still there.
  none <- aib_alignment_cost(ranked, screened[0, ], k)
  expect_equal(nrow(none), 0)
  expect_true(all(c(k, "alignment_cost") %in% names(none)))
  # ... typed as in the ranked rows, so it still joins onto them.
  expect_type(none$increment, "double")
  expect_equal(nrow(dplyr::left_join(ranked, none, by = k)), nrow(ranked))
})

test_that("the bundle check reads node.screenedHost as contract.ts does", {
  ok <- list(nodes = list(list(id = "a", screenedHost = list(host = "MMR", gain = 0.1, cost = 0)),
                          list(id = "b", screenedHost = list(host = "RUS", gain = NA_real_, cost = NA_real_)),
                          list(id = "c")))
  expect_equal(aib_check_screened_hosts(ok), character())
  bad <- list(nodes = list(list(id = "a", screenedHost = list(host = "MMR", gain = 0.1, cost = -0.2)),
                           list(id = "b", screenedHost = "RUS"),
                           list(id = "c", screenedHost = list(gain = 0.1, cost = 0))))
  p <- aib_check_screened_hosts(bad)
  expect_true("nodes[0].screenedHost.cost: -0.2 below 0" %in% p)
  expect_true("nodes[1].screenedHost: not an object" %in% p)
  expect_true("nodes[2].screenedHost.host: expected a string" %in% p)
  expect_true(all(p %in% aib_check_bundle_additions(bad)))
})
