# Guards the decision quadrant inside the medium yard (R/strategic_position/decision_quadrant.R;
# docs/strategic_position.md section 7.3) and the policy alignment of its modes.

dq_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/decision_quadrant.R", "R/strategic_position/sp_diag_framework.R")) {
  source(file.path(dq_repo_root(), f))
}
dq_full_cfg <- function() yaml::read_yaml(file.path(dq_repo_root(), "config", "strategic_position.yml"))
dq_cfg <- function() dq_full_cfg()$strategic_posture

# Cell rows as they leave the engine (here all engine Deploy unless set).
dq_cells <- function(...) {
  base <- list(
    Country = "Testland", iso3 = "TST", tech = "Solar", supply_chain = "Components",
    strategic_posture = "deploy", posture_variant = "deploy", posture_possible = "deploy", pivotal_inputs = NA_character_,
    posture_confidence = "high", posture_primary_rationale = "openness", posture_secondary_rationale = NA_character_,
    compete_strategy = NA_character_, delivery_mode = "diversified_global", posture_exclusion = NA_character_,
    decision_trace = "{\"tests\":[{\"id\":\"t1_consequence\",\"state\":\"no\"}],\"posture\":\"deploy\",\"variant\":\"deploy\",\"possible\":[\"deploy\"],\"pivotal\":[]}",
    strategic_need_percentile = 0.9, opportunity_percentile = 0.9, opportunity_basis = "contenders",
    competitive_position_percentile = 0.6, home_share = 0.05, trusted_world_share = 0.6, coercive_world_share = 0.3,
    coercive_top_share = 0.2, import_hhi = 0.1, imports_usd = 1e9, stage_evidence_quality_cp = "high"
  )
  args <- list(...)
  base[names(args)] <- args
  as.data.frame(base, stringsAsFactors = FALSE)
}
dq <- function(...) sp_decision_quadrant(dq_cells(...), dq_cfg())

test_that("the quadrant maps need x opportunity to the three Compete modes and Deploy / watch", {
  expect_equal(dq(strategic_need_percentile = 0.9, opportunity_percentile = 0.9)$compete_mode, "catch_up")
  expect_equal(dq(strategic_need_percentile = 0.2, opportunity_percentile = 0.9)$compete_mode, "preserve")
  expect_equal(dq(strategic_need_percentile = 0.9, opportunity_percentile = 0.2)$compete_mode, "partner")
  w <- dq(strategic_need_percentile = 0.2, opportunity_percentile = 0.2)
  expect_equal(w$strategic_posture, "deploy")
  expect_equal(w$decision_quadrant, "watch")
  expect_true(is.na(w$compete_mode))
  for (q in c("catch_up", "preserve", "partner")) {
    r <- dq(strategic_need_percentile = ifelse(q == "preserve", 0.2, 0.9), opportunity_percentile = ifelse(q == "partner", 0.2, 0.9))
    expect_equal(r$strategic_posture, "compete")
    expect_equal(r$posture_variant, paste0("compete_", q))
  }
})

test_that("watch is not a fourth top-level posture", {
  x <- do.call(rbind, lapply(seq(0.05, 0.95, by = 0.1), function(n) do.call(rbind, lapply(seq(0.05, 0.95, by = 0.1), function(o)
    dq_cells(iso3 = paste0("T", round(n * 100), round(o * 100)), strategic_need_percentile = n, opportunity_percentile = o)))))
  out <- sp_decision_quadrant(x, dq_cfg())
  expect_true(all(out$strategic_posture %in% SP_POSTURES))
  expect_true(all(out$decision_quadrant %in% SP_QUADRANTS))
  expect_true(all(out$compete_mode %in% c(SP_COMPETE_MODES, NA)))
})

test_that("the quadrant comes after the Secure override", {
  s <- dq(strategic_posture = "secure", posture_variant = "secure_input", strategic_need_percentile = 0.1, opportunity_percentile = 0.1)
  expect_equal(s$strategic_posture, "secure")
  expect_equal(s$posture_variant, "secure_input")
  expect_true(is.na(s$compete_mode))
  expect_equal(s$decision_quadrant, "watch")   # reported for information
})

test_that("Deployment keeps the engine's posture; its quadrant is information only", {
  d <- dq(supply_chain = "Deployment", strategic_need_percentile = 0.9, opportunity_percentile = 0.9)
  expect_equal(d$strategic_posture, "deploy")
  expect_equal(d$decision_quadrant, "catch_up")
  expect_true(is.na(d$compete_mode))
})

test_that("thresholds come from config and are the documented 0.67, tested at 0.60 / 0.67 / 0.70", {
  q <- dq_cfg()$decision_quadrant
  expect_equal(c(q$need_threshold, q$opportunity_threshold), c(0.67, 0.67))
  expect_equal(unlist(q$thresholds_sensitivity), c(0.60, 0.67, 0.70))
  cfg <- dq_cfg()
  cfg$decision_quadrant$opportunity_threshold <- 0.95
  expect_equal(sp_decision_quadrant(dq_cells(), cfg)$compete_mode, "partner")
  x <- do.call(rbind, lapply(c(0.62, 0.65, 0.69, 0.72), function(v) dq_cells(iso3 = paste0("T", v * 100), strategic_need_percentile = v)))
  sens <- sp_diag_quadrant_thresholds(sp_decision_quadrant(x, dq_cfg()), c(0.60, 0.67, 0.70))
  expect_equal(sens$catch_up, c(4, 2, 1))
  expect_equal(sens$posture_changed_vs_base, c(0, 0, 0))   # partner and catch_up are both Compete
})

test_that("borderline cells are flagged, lose confidence and widen the possible postures", {
  b <- dq(strategic_need_percentile = 0.2, opportunity_percentile = 0.69)
  expect_equal(b$quadrant_borderline, "opportunity")
  expect_equal(b$strategic_posture, "compete")
  expect_equal(b$posture_possible, "compete|deploy")
  expect_match(b$pivotal_inputs, "opportunity_percentile")
  expect_false(b$posture_confidence == "high")
  clear <- dq(strategic_need_percentile = 0.9, opportunity_percentile = 0.9)
  expect_true(is.na(clear$quadrant_borderline))
  expect_equal(clear$posture_confidence, "high")
  expect_equal(clear$posture_possible, "compete")
})

test_that("a still-possible Secure stays in the possible postures and lowers confidence", {
  r <- dq(posture_possible = "secure|deploy", pivotal_inputs = "substitutability")
  expect_equal(r$posture_possible, "secure|compete")
  expect_equal(r$pivotal_inputs, "substitutability")
  expect_equal(r$posture_confidence, "moderate")
})

test_that("a missing axis is insufficient data and no case for intervention", {
  r <- dq(opportunity_percentile = NA)
  expect_equal(r$decision_quadrant, "insufficient_data")
  expect_equal(r$strategic_posture, "deploy")
  expect_equal(r$posture_confidence, "low")
})

test_that("Competitive Position is a consistency check, never an override", {
  lead <- dq(competitive_position_percentile = 0.95)
  expect_equal(lead$compete_mode, "catch_up")
  expect_equal(lead$quadrant_consistency_flag, "catch_up_already_leading")
  pres <- dq(strategic_need_percentile = 0.2, competitive_position_percentile = 0.1)
  expect_equal(pres$compete_mode, "preserve")
  expect_equal(pres$quadrant_consistency_flag, "preserve_without_capability")
  part <- dq(opportunity_percentile = 0.2, competitive_position_percentile = 0.9)
  expect_equal(part$compete_mode, "partner")
  expect_equal(part$quadrant_consistency_flag, "partner_despite_capability")
  expect_true(is.na(dq()$quadrant_consistency_flag))
})

test_that("Industrial Opportunity is placed among the node's contenders, not within the country", {
  eo <- data.frame(iso3 = c("A", "B", "C", "D", "E", "F", "G"), tech = "Solar", supply_chain = "Components",
                   industrial_opportunity = c(0.30, 0.40, 0.50, 0.60, 0.70, 0.45, 0.80))
  pres <- data.frame(iso3 = eo$iso3, tech = "Solar", supply_chain = "Components", home_share = c(0.1, 0.1, 0.1, 0.1, 0.1, 0, 0.001))
  o <- sp_opportunity_axis(eo, pres, peers = character(), list(contender_min_share = 0.005, min_contenders = 5))
  expect_equal(unique(o$opportunity_basis), "contenders")
  expect_equal(o$opportunity_percentile[o$iso3 == "A"], 0.1)
  expect_equal(o$opportunity_percentile[o$iso3 == "E"], 0.9)
  # non-contenders are placed against the contenders
  expect_equal(o$opportunity_percentile[o$iso3 == "F"], 0.4)
  expect_equal(o$opportunity_percentile[o$iso3 == "G"], 1)
  # another technology of the same country does not move this cell
  eo2 <- rbind(eo, data.frame(iso3 = "A", tech = "Wind", supply_chain = "Components", industrial_opportunity = 0.99))
  pres2 <- rbind(pres, data.frame(iso3 = "A", tech = "Wind", supply_chain = "Components", home_share = 0.5))
  o2 <- sp_opportunity_axis(eo2, pres2, peers = character(), list(contender_min_share = 0.005, min_contenders = 5))
  expect_equal(o2$opportunity_percentile[o2$tech == "Solar"], o$opportunity_percentile)
})

test_that("too few contenders fall back to the peers, and a declining market is capped below the thresholds", {
  eo <- data.frame(iso3 = c("A", "B", "C", "D", "E", "F"), tech = "Coal", supply_chain = "Extraction",
                   industrial_opportunity = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8))
  pres <- data.frame(iso3 = eo$iso3, tech = "Coal", supply_chain = "Extraction", home_share = c(0.3, 0.3, 0, 0, 0, 0))
  o <- sp_opportunity_axis(eo, pres, peers = eo$iso3, list(contender_min_share = 0.005, min_contenders = 5, cap_outlooks = "declining",
                                                          cap_percentile = 0.5), outlook = c(Coal = "declining"))
  expect_equal(unique(o$opportunity_basis), "peers")
  expect_true(all(o$opportunity_percentile <= 0.5))
  expect_true(o$opportunity_capped[o$iso3 == "F"])
})

test_that("the trace records the quadrant test and stays valid JSON", {
  r <- dq(strategic_need_percentile = 0.9, opportunity_percentile = 0.2)
  tr <- jsonlite::fromJSON(r$decision_trace)
  expect_true("quadrant" %in% tr$tests$id)
  expect_equal(tr$posture, "compete")
  expect_equal(tr$variant, "compete_partner")
  expect_equal(tr$possible, "compete")
})

test_that("no overall score is added", {
  out <- dq()
  added <- setdiff(names(out), names(dq_cells()))
  expect_setequal(added, c("decision_quadrant", "need_high", "opportunity_high", "quadrant_borderline", "engine_posture", "engine_variant",
                           "compete_mode", "quadrant_consistency_flag"))
  expect_false(any(grepl("score|index", added)))
})

test_that("Partner is delivered with trusted partners, Preserve at home", {
  expect_equal(dq(opportunity_percentile = 0.2, trusted_world_share = 0.6, home_share = 0.0)$delivery_mode, "allied")
  expect_equal(dq(opportunity_percentile = 0.2, trusted_world_share = 0.1, home_share = 0.0)$delivery_mode, "diversified_global")
  expect_equal(dq(strategic_need_percentile = 0.2)$delivery_mode, "domestic")
})

test_that("policy alignment: weak support under Preserve / Catch Up, heavy policy on Watch, autarky under Partner", {
  a_cfg <- dq_full_cfg()$policy_alignment
  base <- dq_cells(pams_priority = 0.6, nipo_priority = 0.6)
  pres <- sp_decision_quadrant(transform(base, strategic_need_percentile = 0.2), dq_cfg())
  pres$policy_priority_percentile <- 0.7
  pres$policy_support_priority <- 0.2
  pres$policy_protection_priority <- 0.4
  expect_equal(sp_policy_alignment(pres, a_cfg), "under_prioritized")
  pres$policy_support_priority <- 0.8
  expect_equal(sp_policy_alignment(pres, a_cfg), "aligned")
  w <- sp_decision_quadrant(transform(base, strategic_need_percentile = 0.2, opportunity_percentile = 0.2), dq_cfg())
  w$policy_priority_percentile <- 0.9
  expect_equal(sp_policy_alignment(w, a_cfg, has_record = TRUE), "over_prioritized")
  p <- sp_decision_quadrant(transform(base, opportunity_percentile = 0.2), dq_cfg())
  p$policy_priority_percentile <- 0.5
  p$policy_protection_priority <- 0.9
  p$policy_support_priority <- 0.5
  # spec X / design D5: autarky under Partner is the wrong treatment (misaligned), no longer "mixed"
  expect_equal(sp_policy_alignment(p, a_cfg), "misaligned")
  expect_equal(sp_policy_alignment_flag(p, a_cfg), "partner_autarkic")
  p$policy_protection_priority <- 0.3
  expect_equal(sp_policy_alignment(p, a_cfg), "aligned")
  expect_true(is.na(sp_policy_alignment_flag(p, a_cfg)))
})

test_that("the final strict posture used in the sensitivity runs agrees with the quadrant", {
  x <- data.frame(supply_chain = c("Components", "Components", "Deployment"), strategic_need_percentile = c(0.9, 0.1, 0.9),
                  opportunity_percentile = c(0.1, 0.1, 0.9))
  p <- c("deploy", "deploy", "deploy")
  q <- sp_quadrant_posture(sp_quadrant_of(x$strategic_need_percentile, x$opportunity_percentile, 0.67, 0.67))
  expect_equal(ifelse(x$supply_chain == "Deployment", p, q), c("compete", "deploy", "deploy"))
})
