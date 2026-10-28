# Guards the partner recommendations and the allied division of labour
# (R/strategic_position/partner_recs.R; docs/strategic_position.md section 7.4).

pr_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/partner_recs.R",
            "R/strategic_position/decision_quadrant.R", "R/strategic_position/posture_engine.R", "R/strategic_position/sp_diag_framework.R")) {
  source(file.path(pr_repo_root(), f))
}

# Reporter USA; suppliers CHN (coercive), JPN, KOR, DEU (trusted), IND (nonaligned, not trusted), and USA itself.
pr_pairs <- data.frame(
  reporter_iso = "USA", partner_iso = c("CHN", "JPN", "KOR", "DEU", "IND", "MEX"),
  coercive = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), trusted = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), stringsAsFactors = FALSE
)
pr_supply <- data.frame(iso3 = c("CHN", "JPN", "KOR", "DEU", "IND", "USA", "MEX"), tech = "Solar", supply_chain = "Components",
                        home_share = c(0.60, 0.15, 0.10, 0.05, 0.08, 0.01, 0.01))
pr_compat <- data.frame(reporter_iso = "USA", partner_iso = c("JPN", "KOR", "DEU", "MEX"), tech = "Solar", supply_chain = "Components",
                        friendshore = c(0.9, 0.8, 0.95, 0.7))
pr_cp <- data.frame(iso3 = c("JPN", "KOR", "DEU", "MEX"), tech = "Solar", supply_chain = "Components",
                    competitive_position_percentile = c(0.9, 0.8, 0.7, 0.3))
pr_imports <- data.frame(reporter_iso = "USA", partner_iso = c("CHN", "KOR", "MEX"), tech = "Solar", supply_chain = "Components",
                         imports_usd = c(600, 300, 100))
pr_cells <- data.frame(iso3 = "USA", tech = "Solar", supply_chain = "Components")
pr_recs <- function(supply = pr_supply, aib = NULL, imports = pr_imports, cfg = list()) {
  sp_partner_recs(pr_cells, sp_cell_capability(supply, aib), pr_pairs, pr_compat, pr_cp, imports = imports,
                  aib_cells = aib$cells, members = c("JPN", "KOR", "DEU"), cfg = cfg)
}

test_that("recommended partners are trusted, never coercive and never the country itself", {
  r <- pr_recs()
  recs <- unlist(r[, c("recommended_partner_1", "recommended_partner_2", "recommended_partner_3")])
  expect_setequal(stats::na.omit(recs), c("JPN", "KOR", "DEU"))
  expect_false(any(recs %in% c("CHN", "IND", "USA")))
  expect_equal(r$partner_n_credible, 3)
  expect_false(r$partner_gap)
  expect_equal(r$recommended_partner_1_cp, pr_cp$competitive_position_percentile[pr_cp$iso3 == r$recommended_partner_1])
  expect_true(nzchar(r$partner_reason))
  expect_equal(r$partner_capability_source, "world_share")
  expect_equal(r$partner_evidence_quality, "low")
})

test_that("ranking combines capability, relationship and diversification", {
  r <- pr_recs()
  # JPN: largest capability and no current imports from it (diversifies); KOR already supplies 30%
  expect_equal(r$recommended_partner_1, "JPN")
  expect_gt(r$recommended_partner_1_diversification, 0)
  expect_equal(r$recommended_partner_1_relationship, 0.9)
  # MEX holds 1%: below the presence line, so not credible and not recommended
  expect_false("MEX" %in% unlist(r[, grep("^recommended_partner_[123]$", names(r))]))
})

test_that("moving imports from coercive suppliers to a new partner lowers concentration", {
  s <- c(CHN = 0.6, KOR = 0.3, MEX = 0.1)
  expect_gt(sp_hhi_drop(s, c(TRUE, FALSE, FALSE), "JPN", 0.1), 0)
  # adding to the country's dominant supplier raises concentration
  s2 <- c(KOR = 0.8, MEX = 0.2)
  expect_lt(sp_hhi_drop(s2, c(FALSE, FALSE), "KOR", 0.1), 0.0001)
  expect_true(is.na(sp_hhi_drop(numeric(), logical(), "JPN")))
})

test_that("no credible trusted supplier is a partner gap", {
  sup <- pr_supply
  sup$home_share[sup$iso3 %in% c("JPN", "KOR", "DEU")] <- 0.01
  r <- pr_recs(supply = sup)
  expect_true(r$partner_gap)
  expect_equal(r$partner_n_credible, 0)
  expect_true(is.na(r$recommended_partner_1))
  expect_match(r$partner_reason, "partner gap")
})

test_that("the AIB position share is used where the AIB covers the cell", {
  nodes <- data.frame(node_id = c("n1", "n2"), score_cells = c("Solar|Components", "Solar|Components; Semiconductors|Components"),
                      osi_coverage = c(1, 0.5))
  ev <- data.frame(node_id = rep(c("n1", "n2"), each = 3), iso3 = rep(c("KOR", "JPN", "DEU"), 2),
                   position_share = c(0.5, 0.1, 0.05, 0.2, 0.2, 0.2), world_trade = rep(c(100, 200), each = 3))
  q <- data.frame(node_id = c("n1", "n2"), data_quality = c("High confidence", "Usable with caution"), stage_mixed = FALSE)
  loo <- data.frame(node_id = "n1", iso3 = "KOR", irreplaceable = TRUE)
  aib <- sp_aib_cell_capability(nodes, ev, q, loo, techs = "Solar")
  expect_equal(nrow(aib$cells), 1)
  expect_equal(aib$cells$aib_n_nodes, 2)
  expect_equal(aib$cells$aib_high_quality_share, 0.5)
  expect_equal(aib$cells$aib_irreplaceable, "KOR")
  k <- aib$capability$aib_position_share[aib$capability$iso3 == "KOR"]
  expect_equal(k, (100 * 0.5 + 100 * 0.2) / 200)
  r <- pr_recs(aib = aib)
  expect_equal(r$partner_capability_source, "aib_position_share")
  expect_equal(r$recommended_partner_1, "KOR")
  expect_equal(r$partner_evidence_quality, "high")
})

test_that("the division matrix names the allied holder, backups and gaps", {
  cap <- sp_cell_capability(rbind(transform(pr_supply, home_share = ifelse(iso3 == "DEU", 0.08, home_share)), data.frame(iso3 = c("CHN", "AUS"), tech = "Solar", supply_chain = "Refining", home_share = c(0.9, 0.03)),
                                  data.frame(iso3 = c("CHN", "JPN"), tech = "Wind", supply_chain = "Refining", home_share = c(0.9, 0.0))))
  div <- sp_aib_division(cap, allied = c("JPN", "KOR", "DEU", "AUS"), coercive_powers = "CHN",
                         demand = data.frame(tech = "Solar", supply_chain = "Components", partner_demand = 4L))
  comp <- div[div$tech == "Solar" & div$supply_chain == "Components", ]
  expect_equal(c(comp$primary_holder, comp$backup_1, comp$backup_2), c("JPN", "KOR", "DEU"))
  expect_equal(comp$redundancy, 3)
  expect_equal(comp$partner_demand, 4)
  expect_equal(comp$aib_gap, "weak_evidence")          # no AIB node
  ref <- div[div$tech == "Solar" & div$supply_chain == "Refining", ]
  expect_equal(ref$aib_gap, "single_point_of_failure")  # one allied holder
  expect_match(ref$aib_gaps, "insufficient_redundancy")
  wr <- div[div$tech == "Wind", ]
  expect_equal(wr$aib_gap, "no_trusted_supplier")
  expect_equal(wr$coercive_share, 0.9)
})

test_that("the partner sanity checks find no violations on clean recommendations", {
  r <- pr_recs()
  tbl <- cbind(data.frame(Country = "United States", compete_mode = "partner"), r)
  s <- sp_diag_partner_sanity(tbl, pr_pairs, c("JPN", "KOR", "DEU"))
  v <- setNames(s$value, s$check)
  expect_equal(unname(v[c("recommended partner is the country itself", "recommended partner is coercive to the country",
                          "recommended partner is not trusted by the country", "recommended partner below the presence line")]), c(0, 0, 0, 0))
})
