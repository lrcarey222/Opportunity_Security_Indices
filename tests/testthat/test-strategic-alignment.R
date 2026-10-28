# Guards the competitive-policy alignment diagnostic (R/strategic_position/alignment.R): the four
# classes, the within-country position basis, the priority gap and the legacy-stage roll-up.

alignment_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(alignment_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(alignment_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(alignment_repo_root(), "R", "strategic_position", "alignment.R"))

library(dplyr)

test_that("sp_midrank_pct gives mid-rank percentiles and 0.5 for a singleton", {
  expect_equal(sp_midrank_pct(c(10, 20, 30, 40)), c(0.125, 0.375, 0.625, 0.875))
  expect_equal(sp_midrank_pct(5), 0.5)
  expect_equal(sp_midrank_pct(c(NA, 1, 1)), c(NA, 0.5, 0.5))
  expect_true(all(is.na(sp_midrank_pct(c(NA_real_, NA_real_)))))
})

test_that("the four classes follow the two thresholds", {
  cls <- sp_alignment_class(c(0.9, 0.9, 0.1, 0.1, NA, 0.5), c(0.9, 0.1, 0.9, 0.1, 0.5, NA))
  expect_equal(cls, c(
    "aligned_strength", "latent_advantage", "policy_led_bet", "low_priority",
    "insufficient_data", "insufficient_data"
  ))
  expect_equal(sp_alignment_class(0.5, 0.5), "aligned_strength")
})

alignment_fixture <- function() {
  tibble::tibble(
    Country = rep(c("A", "B"), each = 4),
    tech = rep(c("Solar", "Wind", "Nuclear", "Batteries"), 2),
    supply_chain = "Assembly",
    # A large economy: every cell is above the world median.
    competitive_position_percentile = c(0.99, 0.95, 0.7, 0.6, 0.3, NA, 0.6, 0.7),
    policy_priority_percentile = c(0.125, 0.875, 0.625, 0.375, 0.9, 0.1, 0.5, 0.2),
    strategic_need_percentile = c(0.7, 0.2, 0.55, 0.1, 0.9, 0.5, NA, 0.3)
  )
}

test_that("within-country position separates a large economy's strengths", {
  out <- sp_alignment(alignment_fixture(), list(boundary_band = 0.1))
  a <- out[out$Country == "A", ]
  expect_equal(a$cp_within_country_pct, c(0.875, 0.625, 0.375, 0.125))
  expect_equal(a$competitive_policy_alignment, c("latent_advantage", "aligned_strength", "policy_led_bet", "low_priority"))
  # Cross-country, every cell of A is a strength.
  cross <- sp_alignment(alignment_fixture(), list(competitive_position_basis = "cross_country"))
  expect_true(all(cross$competitive_policy_alignment[cross$Country == "A"] %in% c("aligned_strength", "latent_advantage")))
  expect_error(sp_alignment(alignment_fixture(), list(competitive_position_basis = "peer")), "unknown")
})

test_that("missing position gives insufficient data and no gap; the boundary flag fires near thresholds", {
  out <- sp_alignment(alignment_fixture(), list(boundary_band = 0.1))
  bw <- out[out$Country == "B" & out$tech == "Wind", ]
  expect_equal(bw$competitive_policy_alignment, "insufficient_data")
  expect_true(is.na(bw$priority_gap))
  # A Nuclear: need 0.55 is within 0.1 of 0.5.
  expect_true(out$alignment_boundary[out$Country == "A" & out$tech == "Nuclear"])
})

test_that("priority gap is positive where capability runs ahead of policy attention", {
  out <- sp_alignment(alignment_fixture())
  a <- out[out$Country == "A", ]
  expect_gt(a$priority_gap[a$tech == "Solar"], 0)
  expect_lt(a$priority_gap[a$tech == "Nuclear"], 0)
  expect_equal(sum(a$priority_gap), 0)
  expect_equal(sign(a$priority_gap), sign(a$priority_gap_rank))
  expect_equal(sign(a$priority_gap), sign(a$priority_gap_z))
})

test_that("priority gap re-ranks over the cells observed on both axes only", {
  out <- sp_alignment(alignment_fixture())
  b <- out[out$Country == "B" & !is.na(out$priority_gap), ]
  expect_equal(nrow(b), 3)
})

test_that("legacy roll-up averages scores, sums partitioned counts and takes repeated values once", {
  six <- tibble::tibble(
    Country = "A", tech = "Batteries",
    supply_chain = SUPPLY_CHAIN_STAGES,
    score = c(0.2, 0.4, 0.6, 0.8, 1.0, NA),
    attributed = c(1, 0.5, 0.25, 0.25, 0, 2),
    tech_wide_count = 7
  )
  up <- sp_legacy_rollup(six, mean_cols = "score", sum_cols = "attributed", once_cols = "tech_wide_count")
  expect_equal(up$legacy_stage, c("Downstream", "Midstream", "Upstream"))
  expect_equal(up$score, c(1.0, 0.7, 0.3))
  expect_equal(up$attributed, c(0, 0.5, 3.5))
  expect_equal(sum(up$attributed), sum(six$attributed))
  expect_equal(up$tech_wide_count, c(7, 7, 7))
})

test_that("legacy roll-up refuses to treat stage-varying values as shared", {
  six <- tibble::tibble(Country = "A", tech = "Wind", supply_chain = c("Components", "Assembly"), shared_val = c(1, 2))
  expect_error(sp_legacy_rollup(six, once_cols = "shared_val"), "differ across child stages")
  expect_error(sp_legacy_rollup(six, mean_cols = "shared_val", sum_cols = "shared_val"), "more than one rule")
})
