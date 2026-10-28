# Guards the Explorer's six-stage posture bundle (R/dashboard/posture/posture_export.R): the columnar
# core document, the design dictionary, the detail document and the JSON shapes the TypeScript contract
# (dashboard/src/posture/contract.ts) reads.

pe_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/dashboard/posture/posture_export.R")) source(file.path(pe_repo_root(), f))

pe_tbl <- function() {
  g <- expand.grid(iso3 = c("AAA", "BBB"), tech = c("Batteries", "Solar"), supply_chain = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
  g$Country <- ifelse(g$iso3 == "AAA", "Aland", "Bland")
  g$strategic_posture <- "deploy"
  g$posture_variant <- "deploy"
  g$delivery_mode <- "diversified_global"
  g$compete_mode <- NA_character_
  g$decision_quadrant <- "watch"
  g$strategic_need_percentile <- 0.2
  g$opportunity_percentile <- 0.3
  g$competitive_position_percentile <- 0.4
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
  g$decision_trace <- "{\"tests\":[{\"id\":\"quadrant\",\"state\":\"watch\",\"detail\":\"Neither is high.\"}],\"posture\":\"deploy\",\"variant\":\"deploy\",\"possible\":[\"deploy\"],\"pivotal\":[]}"
  i <- which(g$iso3 == "AAA" & g$tech == "Batteries" & g$supply_chain == "Components")
  g$strategic_posture[i] <- "compete"
  g$posture_variant[i] <- "compete_partner"
  g$compete_mode[i] <- "partner"
  g$delivery_mode[i] <- "allied"
  g$trade_momentum_raw[i] <- 0.3
  g$recommended_partner_1 <- NA_character_
  g$recommended_partner_1[i] <- "BBB"
  g$partner_gap <- NA
  g$partner_gap[i] <- FALSE
  g
}
pe_div <- function() {
  data.frame(tech = "Batteries", supply_chain = "Components", primary_holder = "BBB", primary_share = 0.2, backup_1 = NA_character_,
             backup_1_share = NA_real_, backup_2 = NA_character_, backup_2_share = NA_real_, redundancy = 1L, allied_share = 0.2,
             coercive_share = 0.7, world_leader = "XXX", world_leader_share = 0.7, aib_gap = "single_point_of_failure",
             aib_gaps = "single_point_of_failure;insufficient_redundancy", capability_source = "world_share", aib_n_nodes = NA_integer_,
             partner_demand = 1L, secure_demand = 0L, partner_gap_countries = 0L, partner_demand_members = 1L, stringsAsFactors = FALSE)
}
pe_cfg <- list(headline_techs = c("Batteries", "Solar"), strategic_posture = list(decision_quadrant = list(need_threshold = 0.67, opportunity_threshold = 0.67)))
pe_countries <- data.frame(iso3 = c("AAA", "BBB"), name = c("Aland", "Bland"), tier = c("core", "partner"))

test_that("the core document is columnar, six-stage and carries the freshness meta", {
  core <- posture_core_doc(pe_tbl(), pe_div(), pe_countries, pe_cfg, created = "2026-10-28T00:00:00Z")
  n <- length(core$cells$p)
  expect_equal(n, 24)
  expect_true(all(vapply(core$cells, length, 1L) == n))
  expect_equal(as.character(core$meta$stages), SUPPLY_CHAIN_STAGES)
  expect_equal(core$meta$benchmark_period, "2022-2024")
  expect_equal(core$meta$momentum_through, "Aug 2026")
  expect_setequal(unique(core$cells$p), c("C", "D"))
  i <- which(core$cells$m == "partner")
  expect_equal(as.character(core$cells$mom)[i], "up")
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
})

test_that("momentum direction and the through label", {
  expect_equal(posture_momentum_arrow(c(0.2, 0.05, -0.2, NA)), c("up", "flat", "down", NA))
  expect_equal(posture_through(c(NA, "Sep 2025 - Aug 2026")), "Aug 2026")
  expect_true(is.na(posture_through(NA)))
})

test_that("the detail document carries the drawer fields, the deciding words, and no repeated design text", {
  d <- posture_detail_doc(pe_tbl(), "AAA", pe_cfg, created = "x")
  expect_equal(d$country, "Aland")
  expect_length(d$cells, 12)
  p <- Filter(function(c) identical(c$mode, "partner"), d$cells)[[1]]
  expect_equal(p$stage, "Components")
  expect_equal(p$delivery, "allied")
  expect_null(p$fence)
  expect_null(p$tests)
  expect_equal(p$reason, "Neither is high.")
  expect_equal(p$trade$momentum_raw, 0.3)
  expect_false("momentum_period" %in% names(p$trade))
  w <- d$cells[[1]]
  expect_false(any(vapply(w$trade %||% list(), is.null, logical(1))))
})
