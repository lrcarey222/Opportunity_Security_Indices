# Guards the Explorer's six-stage posture bundle, schema 2 (R/dashboard/posture/posture_export.R): the
# columnar core document, the design dictionary, the detail document and the JSON shapes the TypeScript
# contract (dashboard/src/posture/contract.ts, detail.ts) reads.

pe_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/dashboard/posture/posture_export.R")) source(file.path(pe_repo_root(), f))

pe_tbl <- function() {
  g <- expand.grid(iso3 = c("AAA", "BBB"), tech = c("Batteries", "Solar"), supply_chain = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
  g$Country <- ifelse(g$iso3 == "AAA", "Aland", "Bland")
  g$strategic_posture <- "deploy"
  g$strategic_choice <- "watch"
  g$posture_variant <- "deploy"
  g$delivery_mode <- "diversified_global"
  g$compete_mode <- NA_character_
  g$deploy_mode <- "watch"
  g$need_type <- ifelse(g$supply_chain == "Deployment", "deployment", "supply_chain")
  g$supply_chain_need <- ifelse(g$supply_chain == "Deployment", NA_real_, 0.2)
  g$deployment_need <- ifelse(g$supply_chain == "Deployment", 0.1234, NA_real_)
  g$industrial_opportunity <- ifelse(g$supply_chain == "Deployment", NA_real_, 0.3)
  g$cp_contender_pct <- 0.4
  g$competitive_position_percentile <- 0.4
  g$stage_evidence_quality_cp <- "medium"
  g$policy_alignment <- "aligned"
  g$policy_alignment_confidence <- "medium"
  g$choice_reason <- "Low supply-chain need (0.20) and a low industrial prize (0.30): Watch."
  g$fence_posture <- "A low fence."
  g$fence_level <- "low"
  g$gate_posture <- "Wide open."
  g$build_posture <- "Build what enables deployment."
  g$benchmark <- "Delivered cost."
  g$exit_condition <- "Re-test."
  g$trade_benchmark_period <- "2022-2024"
  g$trade_momentum_period <- "Sep 2025 - Aug 2026"
  g$trade_momentum_previous_period <- "Sep 2024 - Aug 2025"
  g$trade_momentum_raw <- NA_real_
  g$decision_trace <- paste0("[{\"step\":\"secure\",\"test\":\"secure_override\",\"result\":\"no\"},",
                             "{\"step\":\"need\",\"test\":\"supply_chain_need\",\"value\":0.2,\"threshold\":0.67,\"result\":\"low\"},",
                             "{\"step\":\"choice\",\"test\":\"choice\",\"result\":\"watch\"}]")
  i <- which(g$iso3 == "AAA" & g$tech == "Batteries" & g$supply_chain == "Components")
  g$strategic_posture[i] <- "compete"
  g$strategic_choice[i] <- "partner"
  g$posture_variant[i] <- "compete_partner"
  g$compete_mode[i] <- "partner"
  g$deploy_mode[i] <- NA_character_
  g$supply_chain_need[i] <- 0.9
  g$choice_reason[i] <- "High supply-chain need (0.90) and a low industrial prize (0.30): Partner with trusted suppliers."
  g$delivery_mode[i] <- "allied"
  g$trade_momentum_raw[i] <- 0.3
  g$recommended_partner_1 <- NA_character_
  g$recommended_partner_1[i] <- "BBB"
  g$partner_gap <- NA
  g$partner_gap[i] <- FALSE
  g$partner_gap_reasons <- NA_character_
  g$partner_gap_reasons[i] <- "single_credible_supplier"
  g$partner_aib_status <- NA_character_
  g$partner_aib_status[i] <- "gap"
  g
}
pe_div <- function() {
  data.frame(tech = "Batteries", supply_chain = "Components", primary_holder = "BBB", primary_share = 0.2, backup_1 = NA_character_,
             backup_1_share = NA_real_, backup_2 = NA_character_, backup_2_share = NA_real_, redundancy = 1L, allied_share = 0.2,
             coercive_share = 0.7, world_leader = "XXX", world_leader_share = 0.7, aib_gap = "single_point_of_failure",
             aib_gaps = "single_point_of_failure;insufficient_redundancy", capability_source = "world_share", aib_n_nodes = NA_integer_,
             partner_demand = 1L, secure_demand = 0L, partner_gap_countries = 0L, partner_demand_members = 1L, stringsAsFactors = FALSE)
}
pe_cfg <- list(headline_techs = c("Batteries", "Solar"),
               decision_framework = list(need_threshold = 0.67, prize_threshold = 0.6, cp_preserve_pct = 0.75, deployment_threshold = 0.67,
                                         borderline_band = 0.05),
               node_phrases = list(`Batteries|Refining` = "battery materials refining", `Coal|Refining` = "coke and coal processing"))
pe_countries <- data.frame(iso3 = c("AAA", "BBB"), name = c("Aland", "Bland"), tier = c("core", "partner"))

test_that("the core document is columnar, six-stage and carries the freshness meta", {
  core <- posture_core_doc(pe_tbl(), pe_div(), pe_countries, pe_cfg, created = "2026-10-28T00:00:00Z")
  n <- length(core$cells$p)
  expect_equal(n, 24)
  expect_true(all(vapply(core$cells, length, 1L) == n))
  expect_equal(as.character(core$meta$stages), SUPPLY_CHAIN_STAGES)
  expect_equal(core$meta$benchmark_period, "2022-2024")
  expect_equal(core$meta$momentum_through, "Aug 2026")
  expect_equal(core$schema_version, 2L)
  expect_setequal(unique(core$cells$p), c("C", "D"))
  i <- which(core$cells$m == "partner")
  expect_equal(as.character(core$cells$mom)[i], "up")
  expect_true(is.na(core$cells$dm[i]))
  expect_true(all(core$cells$dm[core$cells$p == "D"] == "watch"))
  # the relevant Need: supply_chain_need at the producing stages, deployment_need at Deployment & Use
  dep <- core$cells$s == match("Deployment", SUPPLY_CHAIN_STAGES) - 1L
  expect_true(all(core$cells$need[dep] == 0.123))
  expect_true(all(core$cells$need[!dep] %in% c(0.2, 0.9)))
  expect_true(all(is.na(core$cells$prize[dep])))
  expect_true(all(core$cells$ev == "medium"))     # evidence written as the engine's raw word (contract.ts normalises)
  expect_true(all(posture_evidence_word(core$cells$ev) == "moderate"))
  expect_equal(core$meta$thresholds$prize, 0.6)
  expect_equal(core$meta$thresholds$cp, 0.75)
  expect_equal(names(core$meta$phrases), "Batteries|Refining")   # only the bundle's technologies
  expect_equal(unname(unlist(core$meta$stage_display)), SUPPLY_CHAIN_STAGES)
  expect_equal(core$aib_header$priority_nodes, 1)
  expect_equal(core$aib_header$covered_share, 0)
  expect_setequal(names(core$design), c("deploy|diversified_global", "compete_partner|allied"))
})

test_that("one-item arrays stay arrays in the JSON (jsonlite auto_unbox)", {
  core <- posture_core_doc(pe_tbl(), pe_div(), pe_countries, pe_cfg, created = "x")
  j <- jsonlite::fromJSON(as.character(jsonlite::toJSON(core, auto_unbox = TRUE, na = "null", null = "null", digits = NA)), simplifyVector = FALSE)
  expect_type(j$division$t, "list")
  expect_length(j$division$t, 1)
  expect_type(j$division$gap, "list")
  expect_type(j$meta$techs, "list")
  expect_type(j$cells$dm, "list")
  expect_type(j$meta$thresholds, "list")
  expect_equal(j$meta$thresholds$need, 0.67)
})

test_that("momentum direction and the through label", {
  expect_equal(posture_momentum_arrow(c(0.2, 0.05, -0.2, NA)), c("up", "flat", "down", NA))
  expect_equal(posture_through(c(NA, "Sep 2025 - Aug 2026")), "Aug 2026")
  expect_true(is.na(posture_through(NA)))
})

test_that("the detail document carries the drawer fields, the deciding words, and no repeated design text", {
  d <- posture_detail_doc(pe_tbl(), "AAA", pe_cfg, created = "x")
  expect_equal(d$country, "Aland")
  expect_equal(d$schema_version, 2L)
  expect_length(d$cells, 12)
  p <- Filter(function(c) identical(c$mode, "partner"), d$cells)[[1]]
  expect_equal(p$stage, "Components")
  expect_equal(p$delivery, "allied")
  expect_null(p$fence)
  expect_null(p$tests)
  expect_match(p$reason, "Partner with trusted suppliers")
  expect_equal(p$need, 0.9)
  expect_equal(p$partner_gap_reasons, "single_credible_supplier")
  expect_equal(p$partner_aib_status, "gap")
  expect_length(p$trace, 3)
  expect_equal(p$trace[[1]]$step, "secure")
  expect_equal(p$trade$momentum_raw, 0.3)
  expect_false("momentum_period" %in% names(p$trade))
  w <- d$cells[[1]]
  expect_false(any(vapply(w$trade %||% list(), is.null, logical(1))))
  dep <- Filter(function(c) identical(c$stage, "Deployment"), d$cells)[[1]]
  expect_equal(dep$need, 0.123)
  expect_null(dep$prize)
  expect_null(dep$pathway_credible)
  expect_null(dep$import_dependence)
})

test_that("the evidence words are the Explorer's strong / moderate / weak", {
  expect_equal(posture_evidence_word(c("high", "medium", "low", "none", NA)), c("strong", "moderate", "weak", "weak", NA))
})

test_that("a cell that breaks the framework invariants stops the export", {
  bad <- pe_tbl()
  bad$deploy_mode[1] <- NA_character_
  expect_error(posture_core_doc(bad, pe_div(), pe_countries, pe_cfg, created = "x"), "invariants")
  acc <- pe_tbl()
  acc$deploy_mode[acc$supply_chain == "Components" & acc$strategic_posture == "deploy"] <- "accelerate"
  expect_error(posture_core_doc(acc, pe_div(), pe_countries, pe_cfg, created = "x"), "accelerate_off_deployment")
  cmp <- pe_tbl()
  j <- which(cmp$supply_chain == "Deployment")[1]
  cmp$strategic_posture[j] <- "compete"
  cmp$compete_mode[j] <- "partner"
  cmp$deploy_mode[j] <- NA_character_
  expect_error(posture_core_doc(cmp, pe_div(), pe_countries, pe_cfg, created = "x"), "compete_at_deployment")
})

test_that("not assessed cells are coded N with no mode and carry their reason in the detail", {
  x <- pe_tbl()
  k <- which(x$iso3 == "AAA" & x$tech == "Solar" & x$supply_chain == "Recovery")
  x$strategic_posture[k] <- "not_assessed"
  x$strategic_choice[k] <- "not_assessed"
  x$deploy_mode[k] <- NA_character_
  x$posture_variant[k] <- NA_character_
  x$choice_reason[k] <- "Supply-chain need is not measured: not assessed."
  core <- posture_core_doc(x, pe_div(), pe_countries, pe_cfg, created = "x")
  expect_equal(sum(core$cells$p == "N"), 1)
  d <- posture_detail_doc(x, "AAA", pe_cfg, created = "x")
  n <- Filter(function(c) identical(c$posture, "not_assessed"), d$cells)[[1]]
  expect_match(n$not_assessed_reason, "not measured")
  expect_null(n$reason)
})

test_that("the detail policy says whether NIPO records any protection, and leaves it out where NIPO does not observe the cell", {
  x <- pe_tbl()
  x$policy_priority_percentile <- 0.5
  x$policy_protection_stock <- NA_real_
  a <- which(x$iso3 == "AAA" & x$tech == "Batteries" & x$supply_chain == "Refining")
  b <- which(x$iso3 == "AAA" & x$tech == "Batteries" & x$supply_chain == "Assembly")
  x$policy_protection_stock[a] <- 3
  x$policy_protection_stock[b] <- 0
  # a Deployment row never reads the producing stock (only a localisation stock, which this table lacks)
  x$policy_protection_stock[x$supply_chain == "Deployment"] <- 5
  d <- posture_detail_doc(x, "AAA", pe_cfg, created = "x")
  cell <- function(tech, stage) Filter(function(c) identical(c$tech, tech) && identical(c$stage, stage), d$cells)[[1]]
  expect_identical(cell("Batteries", "Refining")$policy$protection_recorded, TRUE)
  expect_identical(cell("Batteries", "Assembly")$policy$protection_recorded, FALSE)
  expect_false("protection_recorded" %in% names(cell("Batteries", "Extraction")$policy))
  expect_false("protection_recorded" %in% names(cell("Batteries", "Deployment")$policy))
  # with a localisation stock column the Deployment row reads it
  x$deployment_localisation_stock <- ifelse(x$supply_chain == "Deployment", 0, NA_real_)
  d2 <- posture_detail_doc(x, "AAA", pe_cfg, created = "x")
  dep <- Filter(function(c) identical(c$tech, "Batteries") && identical(c$stage, "Deployment"), d2$cells)[[1]]
  expect_identical(dep$policy$protection_recorded, FALSE)
  # a JSON boolean, not an array
  j <- jsonlite::fromJSON(as.character(jsonlite::toJSON(d, auto_unbox = TRUE, na = "null", null = "null", digits = NA)), simplifyVector = FALSE)
  rj <- Filter(function(c) identical(c$tech, "Batteries") && identical(c$stage, "Refining"), j$cells)[[1]]
  expect_identical(rj$policy$protection_recorded, TRUE)
  expect_null(posture_protection_recorded(NULL))
  expect_null(posture_protection_recorded(NA_real_))
})

test_that("the bundle's commit stamp says -dirty when R, scripts or config have uncommitted changes", {
  expect_equal(posture_commit_stamp("abc1234", character()), "abc1234")
  expect_equal(posture_commit_stamp("abc1234", ""), "abc1234")
  expect_equal(posture_commit_stamp("abc1234", c(" M R/strategic_position/decision_engine.R", "?? scripts/x.R")), "abc1234-dirty")
  expect_true(is.na(posture_commit_stamp(NA_character_, character())))
  expect_true(is.na(posture_commit_stamp(character(), character())))
})

test_that("the yards and the strategic value travel in the core and the detail, additive to schema 2", {
  g <- pe_tbl()
  prod <- g$supply_chain != "Deployment"
  g$choice_prize <- ifelse(prod, 0.72, NA_real_)
  g$prize_measure <- ifelse(prod, "strategic_value", NA_character_)
  g$strategic_industrial_value <- g$choice_prize
  g$siv_market <- ifelse(prod, 0.3, NA_real_)
  g$siv_commons <- ifelse(prod, 0.5, NA_real_)
  g$siv_option <- ifelse(prod, 0, NA_real_)
  g$siv_cap <- NA_character_
  g$large_yard_case <- ifelse(prod, FALSE, NA)
  g$large_yard_n <- ifelse(prod, 1L, NA_integer_)
  g$large_yard_signals <- ifelse(prod, "passive", NA_character_)
  g$large_yard_unknown <- NA_character_
  g$large_yard_reason <- ifelse(prod, "Not a large yard: not mature (scaling).", NA_character_)
  g$yard <- "large"
  g$yard_rollup <- "large"
  g$yard_children <- NA_character_
  i <- which(g$iso3 == "AAA" & g$tech == "Batteries" & g$supply_chain == "Components")
  g$yard[i] <- "medium"
  g$yard_rollup[i] <- "mixed_high_medium"
  g$yard_children[i] <- "[{\"layer_id\":\"bess-control\",\"label\":\"BMS\",\"yard\":\"small\",\"choice\":\"secure\",\"variant\":\"secure_integrated\",\"reason\":\"An acute vulnerability.\"}]"
  g$yard_fence_scope <- NA_character_
  g$yard_fence_scope[i] <- "extends_to_hardware"
  g$siv_commons_n <- ifelse(prod, 2L, NA_integer_)
  g$siv_option_n <- ifelse(prod, 0L, NA_integer_)
  g$large_yard_floor <- ifelse(prod, FALSE, NA)
  core <- posture_core_doc(g, pe_div(), pe_countries, pe_cfg, created = "x")
  dep <- core$cells$s == match("Deployment", SUPPLY_CHAIN_STAGES) - 1L
  expect_equal(core$meta$prize_measure, "strategic_value")
  expect_true(all(core$cells$prize[!dep] == 0.72))        # the prize the choice read
  expect_true(all(core$cells$io[!dep] == 0.3))            # the market prize beside it
  expect_true(all(is.na(core$cells$prize[dep])))
  k <- which(core$cells$m %in% "partner")
  expect_equal(as.character(core$cells$yd)[k], "SM")
  expect_true(all(as.character(core$cells$yd)[-k] == "L"))
  expect_equal(core$meta$yard_codes$mixed_medium_low, "ML")
  j <- jsonlite::fromJSON(as.character(jsonlite::toJSON(core, auto_unbox = TRUE, na = "null", null = "null", digits = NA)), simplifyVector = FALSE)
  expect_type(j$cells$yd, "list")
  d <- posture_detail_doc(g, "AAA", pe_cfg, created = "x")
  jd <- jsonlite::fromJSON(as.character(jsonlite::toJSON(d, auto_unbox = TRUE, na = "null", null = "null", digits = NA)), simplifyVector = FALSE)
  p <- Filter(function(c) identical(c$mode, "partner"), jd$cells)[[1]]
  expect_equal(p$prize$value, 0.72)
  expect_equal(p$prize$measure, "strategic_value")
  expect_equal(p$prize$market, 0.3)
  expect_equal(p$prize$strategic_value$commons, 0.5)
  expect_identical(p$large_yard$case, FALSE)
  expect_equal(p$large_yard$signals, list("passive"))     # a one-item list stays an array
  expect_null(p$large_yard$reason)                        # decided: its words are in the choice reason
  expect_equal(p$yard, "medium")
  expect_equal(p$yard_rollup, "mixed_high_medium")
  expect_length(p$yard_children, 1)                       # one child stays a one-item array
  expect_equal(p$yard_children[[1]]$layer_id, "bess-control")
  expect_equal(p$yard_children[[1]]$yard, "small")
  expect_equal(p$yard_children[[1]]$variant, "secure_integrated")
  expect_equal(p$yard_fence_scope, "extends_to_hardware")
  expect_equal(p$prize$strategic_value$commons_observed, 2)
  expect_null(p$large_yard$floor)                         # only a large yard with a capability floor carries it
  depc <- Filter(function(c) identical(c$stage, "Deployment"), jd$cells)[[1]]
  expect_null(depc$large_yard)
  expect_null(depc$prize)
  expect_null(Filter(function(c) identical(c$stage, "Extraction"), jd$cells)[[1]]$yard_children)
  # an undecided case carries its reason
  g$large_yard_case[g$iso3 == "AAA" & g$supply_chain == "Refining"] <- NA
  d2 <- posture_detail_doc(g, "AAA", pe_cfg, created = "x")
  expect_match(Filter(function(c) identical(c$stage, "Refining"), d2$cells)[[1]]$large_yard$reason, "large yard")
  g$large_yard_case[i] <- TRUE
  g$large_yard_floor[i] <- TRUE
  d3 <- posture_detail_doc(g, "AAA", pe_cfg, created = "x")
  expect_true(Filter(function(c) identical(c$mode, "partner"), d3$cells)[[1]]$large_yard$floor)
  # a table without them exports as before
  core0 <- posture_core_doc(pe_tbl(), pe_div(), pe_countries, pe_cfg, created = "x")
  expect_null(core0$cells$yd)
  expect_null(core0$cells$io)
  expect_equal(core0$meta$prize_measure, "market")
  expect_equal(POSTURE_BUDGET_DETAIL, 160 * 1024)
})
