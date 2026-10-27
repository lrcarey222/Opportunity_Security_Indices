repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("utils/scurve.R", "utils/schema.R", "utils/hs6_crosswalk.R", "categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, "R", f))
}
for (f in sort(list.files(file.path(repo_root, "R", "aib"), pattern = "\\.R$", full.names = TRUE))) source(f)
suppressPackageStartupMessages(library(dplyr))

flat <- list(spec = function(e) rep(0.5, length(e)), op = function(e, m) rep(0.5, length(e)),
             op_raw = function(e, m) rep(0.5, length(e)), compat = function(e, m) rep(0.5, length(e)))

test_that("a host's increment is capped at a multiple of its current scale", {
  expect_equal(aib_host_increment(0.1, c(0.5, 0.04, 0, NA), 1), c(0.1, 0.04, 0, 0))
  expect_equal(aib_host_increment(0.1, c(0.5, 0.04), 2), c(0.1, 0.08))
  # No multiple (or an infinite one): the standardized increment everywhere, whatever the scale.
  expect_equal(aib_host_increment(0.1, c(0.5, 0, NA), Inf), c(0.1, 0.1, 0.1))
  expect_equal(aib_host_increment(0.1, c(0.5, 0), NULL), c(0.1, 0.1))
})

test_that("host scale is the larger of node exports and production share x node world exports", {
  em <- tibble::tibble(
    node_id = c("n", "n", "n", "up"), exporter_iso = c("JPN", "AUT", "CHL", "KOR"),
    exports = c(8e8, 1.5e8, 5e7, 9e8), world_share = c(0.8, 0.15, 0.05, 1),
    export_rca = c(2, 1.5, 1.2, 3), production_share = c(NA, NA, 0.3, NA)
  )
  edges <- tibble::tibble(node_a = "up", node_b = "n", basis = "material link")
  h <- aib_eligible_hosts("n", em, edges, min_exports_usd = 1e7, min_material_share = 0.01)
  sc <- stats::setNames(h$host_scale_usd, h$host_iso)
  expect_equal(sc[["JPN"]], 8e8)
  expect_equal(sc[["AUT"]], 1.5e8)
  expect_equal(sc[["CHL"]], 0.3 * 1e9)  # production share x the node's world exports beats its own exports
  expect_equal(sc[["KOR"]], 0)          # qualified only by the linked node: no scale in this one
})

test_that("the smallest qualified supplier no longer wins a node on the HHI tie-break", {
  # China (lost) supplied the whole demand scope (USA, 200); Japan, Korea, Germany and France ship 30
  # each elsewhere, Austria 2, so trade restores 61%. A 10% increment (20) at each host: uncapped,
  # every host adds the same 10 points (none passes the 40% limit), and Austria, where the increment
  # raises concentration least, comes first; capped at its own scale (multiple 1), Austria can add
  # only 1 point and Japan or Korea leads.
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "KOR", "DEU", "FRA", "AUT"), importer_iso = c("USA", rep("ZZZ", 5)),
                          value = c(200, 30, 30, 30, 30, 2))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", demand_scope = "USA",
                        candidate_min_exports = 1, candidate_min_share = 0)
  hosts <- tibble::tibble(host_iso = c("JPN", "KOR", "AUT"), capability_tier = 3L, capability_value = c(0.246, 0.245, 0.016),
                          host_scale_usd = c(30, 30, 2))
  fs <- function(a, b) rep(0.5, length(a))
  run <- function(m) aib_investment_node(p, "reallocation", hosts, 0.1, 0.1, 1, 0.4, flat, fs, c(5e-4, 0.02, 0.02),
                                         pair_hosts = 0, scale_multiple = m)$hosts
  old <- run(Inf)
  expect_equal(old$host_iso[old$host_rank == 1], "AUT")
  expect_equal(unique(round(old$marginal_resilience_gain, 6)), 0.1)   # every gain ties
  expect_equal(old$rank_basis[old$host_rank == 1], "diversification (HHI change)")
  expect_equal(unique(old$gain_shortlist), 3L)
  new <- run(1)
  a <- new[new$host_iso == "AUT", ]
  expect_equal(a$host_increment, 0.01)
  expect_true(a$increment_capped)
  expect_equal(a$marginal_resilience_gain, 0.01, tolerance = 1e-6)
  expect_equal(a$host_rank, 3L)
  expect_equal(new$host_iso[new$host_rank == 1], "JPN")                 # Japan and Korea tie; capability decides
  expect_equal(new$rank_basis[new$host_rank == 1], "capability at scale")
  expect_equal(new$rank_basis[new$host_rank == 3], "resilience gain")
  expect_equal(unique(new$gain_shortlist), 2L)
  expect_equal(unique(new$increment), 0.1)                              # the nominal increment is kept
})

test_that("a host's simulated capacity is kept apart from its current supply in the coalition terms", {
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "AUT"), importer_iso = c("USA", "ZZZ", "ZZZ"), value = c(100, 60, 1))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", candidate_min_exports = 1, candidate_min_share = 0)
  t <- aib_anchor_terms(p, "USA", "reallocation", host_extra = c(AUT = 0.2, DEU = 0.1))
  expect_equal(unname(t$h[c("AUT", "DEU", "JPN")]), c(0.2, 0.1, 0))
  expect_equal(unname(t$g["AUT"] - t$h["AUT"]), unname(aib_anchor_terms(p, "USA", "reallocation")$g["AUT"]))
  # Losing only its current supply leaves a host its simulated capacity; losing everything leaves nothing.
  tc <- aib_anchor_terms(p, "USA", "reallocation", host_extra = c(AUT = 0.2), drop_current = "AUT")
  expect_equal(unname(tc$g["AUT"]), 0.2)
  td <- aib_anchor_terms(p, "USA", "reallocation", host_extra = c(AUT = 0.2), drop = "AUT")
  expect_equal(unname(td$g["AUT"]), 0)
  expect_equal(unname(td$h["AUT"]), 0)
})

test_that("under a supplier-share cap current supply fills the room before simulated capacity", {
  fl <- tibble::tibble(exporter_iso = c("CHN", "CHN", "JPN", "JPN"), importer_iso = c("USA", "DEU", "USA", "ZZZ"),
                       value = c(60, 20, 10, 50))
  p <- aib_node_problem(fl, c(CHN = 1 - 0.3 / 0.8), shocked_demand = "hold", demand_scope = c("USA", "DEU"),
                        candidate_min_exports = 1, candidate_min_share = 0, cap_basis = "demand")
  p$share_cap <- 0.3
  t <- aib_anchor_terms(p, c("USA", "DEU"), "reallocation", host_extra = c(JPN = 0.5))
  # Japan already ships 10 of the scope's 90: the 30% cap leaves it 30% - 10/90 of the scope's demand,
  # all of it filled from its current supply (50 shipped elsewhere), none from the increment.
  expect_equal(unname(t$g["JPN"]), 0.3 - 10 / 90, tolerance = 1e-9)
  expect_equal(unname(t$h["JPN"]), 0, tolerance = 1e-9)
})

test_that("a member in only for the capacity it would host is a prospective host, not critical", {
  # The anchor (USA) lost China's 100. Japan can redirect 60 of its exports; Austria ships 1 elsewhere.
  # With a 40-point synthetic increment at Austria (a leading host), Japan and Austria together reach
  # 95%; Austria's current supply (1 point) matters nowhere, its simulated capacity everywhere.
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "AUT"), importer_iso = c("USA", "ZZZ", "ZZZ"), value = c(100, 60, 1))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", demand_scope = "USA", candidate_min_exports = 1, candidate_min_share = 0)
  probs <- list(n = p)
  hx <- list(n = c(AUT = 0.4))
  members <- c("USA", "JPN", "AUT")
  expect_gte(aib_anchor_coverage(p, "USA", members, "reallocation", hx$n), 0.95)
  crit <- aib_coalition_critical(probs, "n", "USA", members, "reallocation", hx, target = 0.95)
  aut <- crit[crit$iso3 == "AUT", ]
  expect_false(aut$critical)                    # its current supply is not what the coalition needs
  expect_true(aut$critical_simulated)           # its hypothetical plant is
  expect_equal(aut$added_share_simulated, 0.4)
  expect_true(crit$critical[crit$iso3 == "JPN"])
  basis <- aib_membership_basis(members, "USA", crit)
  expect_equal(basis$membership_basis[match(c("USA", "JPN", "AUT"), basis$iso3)], c("demand anchor", "current supply", "simulated capacity"))
  skip_if_not(aib_has_lp(), "Rglpk/slam not installed")
  sub <- aib_coalition_substitution(probs, "n", "USA", members, c("JPN", "AUT"), "reallocation", hx, target = 0.95)
  expect_false(sub$needed[sub$iso3 == "AUT"])   # never a critical supply partner on hypothetical capacity
  expect_true(sub$critical[sub$iso3 == "JPN"])
})

test_that("a large supplier that is also a host keeps its current-supply membership", {
  flows <- tibble::tibble(exporter_iso = c("CHN", "JPN", "KOR"), importer_iso = c("USA", "ZZZ", "ZZZ"), value = c(100, 60, 30))
  p <- aib_node_problem(flows, c(CHN = 1), shocked_demand = "hold", demand_scope = "USA", candidate_min_exports = 1, candidate_min_share = 0)
  hx <- list(n = c(JPN = 0.1))
  members <- c("USA", "JPN", "KOR")
  crit <- aib_coalition_critical(list(n = p), "n", "USA", members, "reallocation", hx, target = 0.95)
  expect_true(crit$critical[crit$iso3 == "JPN"])
  expect_equal(aib_membership_basis(members, "USA", crit)$membership_basis, c("demand anchor", "current supply", "current supply"))
})

test_that("the rank basis names the first criterion that separates two hosts", {
  b <- aib_rank_basis(gain_tier = c(0, 0, 0, 1), hhi_tier = c(0, 0, 1, 0), capability_tier = c(3L, 3L, 3L, 3L),
                      capability_value = c(0.2, 0.2, 0.1, 0.5), friendshore = c(0.6, 0.6, 0.5, 0.5), compatibility = c(0.5, 0.5, 0.5, 0.5))
  expect_equal(b, c("tied", "tied", "diversification (HHI change)", "resilience gain"))
  expect_equal(aib_rank_basis(0, 0, 3L, 0.1, 0.5, 0.5), NA_character_)
})

test_that("scaling hosts carry the full increment or hold a large world share; they rank first", {
  hosts <- tibble::tibble(host_iso = c("BIG", "FULL", "SMALL", "LINK"), capability_tier = c(3L, 3L, 3L, 1L),
                          capability_value = c(0.08, 0.02, 0.01, 0.30))
  # Scale as a share of demand: BIG and SMALL cannot carry a 10% increment, FULL can; LINK has none.
  s <- aib_scaling_hosts(hosts, c(0.05, 0.12, 0.01, 0), 0.1, multiple = 1, min_share = 0.05)
  expect_equal(s, c(TRUE, TRUE, FALSE, FALSE))
  rows <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "r", increment = 0.1,
                         host_iso = c("SMALL", "BIG", "FULL"), is_scaling = c(FALSE, TRUE, TRUE),
                         marginal_resilience_gain = c(0.1, 0.05, 0.1), marginal_resilience_gain_diversified = c(0.1, 0.05, 0.1),
                         delta_hhi = c(-0.03, 0.01, 0), capability_tier = 3L, capability_value = c(0.01, 0.08, 0.02),
                         friendshore_need_weighted = 0.5, compatibility_need_weighted = 0.5)
  r <- aib_rank_hosts(rows, 0.005, 0.005)
  r <- r[order(r$host_rank), ]
  expect_equal(r$host_iso, c("FULL", "BIG", "SMALL"))     # the small host comes after every scaling host
  expect_equal(r$scaling_rank, c(1L, 2L, NA))
  expect_equal(r$rank_basis, c("resilience gain", "resilience gain", "scaling host first"))
  expect_equal(r$gain_shortlist, c(1L, 1L, 1L))           # each type has its own leader
})

test_that("specialty and development hosts are typed and ranked beside the scaling ranking", {
  rows <- tibble::tibble(node_id = "n", shock_id = "s", scenario = "r", increment = 0.1,
                         host_iso = c("JPN", "AUT", "LUX", "VNM", "IND", "TWN", "MYS"),
                         is_scaling = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                         node_rca = c(2.5, 3, 40, 2.5, 1.2, 4, 1.1), node_exports_usd = c(5e9, 2e8, 6e7, 4e8, 1e9, 3e8, 2e8),
                         capability_tier = 3L, capability_value = c(0.2, 0.02, 0.005, 0.03, 0.06, 0.02, 0.01),
                         delta_hhi = c(0.02, -0.01, -0.02, -0.01, 0, 0, -0.01))
  income <- c(JPN = "High income", AUT = "High income", LUX = "High income", VNM = "Lower middle income", IND = "Lower middle income",
              TWN = "High income", MYS = "Upper middle income")
  t <- aib_host_types(rows, income, dev_index = c(NA, NA, NA, 0.6, 0.8, NA, 0.7))
  tt <- stats::setNames(t$host_type, t$host_iso)
  # The types exclude each other, scaling first: Japan (RCA 2.5) and India (middle income) are scaling hosts only.
  expect_equal(unname(tt[c("JPN", "AUT", "LUX", "VNM", "IND", "TWN", "MYS")]),
               c("scaling", "specialty", "", "development", "scaling", "specialty", "development"))
  expect_equal(t$income_group[t$host_iso == "IND"], "Lower middle income")
  # Luxembourg's RCA of 40 rests on USD 60m of exports: under the USD 100m floor, no specialty host.
  sp <- t[!is.na(t$specialty_rank), ]
  expect_equal(sp$host_iso[order(sp$specialty_rank)], c("TWN", "AUT"))   # by RCA
  dv <- t[!is.na(t$development_rank), ]
  expect_equal(dv$host_iso[order(dv$development_rank)], c("MYS", "VNM")) # by the Development Potential Index
  expect_true(all(is.na(t$development_rank[t$host_iso %in% c("JPN", "AUT", "TWN", "IND")])))
  # Without income groups nothing is typed beyond scaling.
  t0 <- aib_host_types(rows, NULL, NULL)
  expect_false(any(t0$is_specialty | t0$is_development))
})

test_that("a host's Development Potential Index is its node cells' mean, else its own mean", {
  dev <- tibble::tibble(partner_iso = c("VNM", "VNM", "VNM", "IND"), tech = c("Solar", "Solar", "Wind", "Wind"),
                        supply_chain = c("Midstream", "Upstream", "Midstream", "Upstream"), value = c(0.8, 0.4, 0.6, 0.5))
  expect_equal(aib_host_dev_index(c("VNM", "IND", "JPN"), "Solar", "Midstream", dev), c(0.8, 0.5, NA))
  expect_equal(aib_host_dev_index("VNM", character(), "Midstream", dev), 0.6)
  expect_equal(aib_host_dev_index("VNM", "Solar", "Midstream", NULL), NA_real_)
})
