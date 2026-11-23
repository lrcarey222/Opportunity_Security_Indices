# Guards the trade-is-not-production diagnostics (R/qc/qc_trade_production.R; docs/QC_SYSTEM_DESIGN.md 6.2): high
# exports without production where the stage is measured, exports far above measured production, the entrepot
# (re-export) signature, positions and Catch Up resting on trade alone, the explicit "no production data for this
# stage" gap, and that a projection is never read as production.

qt_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/qc/qc_core.R", "R/qc/qc_trade_production.R")) source(file.path(qt_root(), f))

qt_sp_row <- function(iso3, tech = "Solar", stage = "Assembly", ...) {
  base <- list(iso3 = iso3, tech = tech, supply_chain = stage, strategic_choice = "watch", posture_exclusion = NA_character_,
               competitive_position_score = 0.3, cp_contender_pct = 0.3, trade_benchmark_share = 0.001, trade_benchmark_rca = 0.5,
               trade_benchmark_exports = 1e8, trade_benchmark_source = "comtrade", trade_benchmark_period = "2022-2024", imports_usd = 1e7,
               catch_up_anchor_type = NA_character_, cp_export_position = 0.3, cp_production_position = NA_real_,
               cp_manufacturing_capacity = NA_real_, cp_investment_position = NA_real_, cp_cost_position = NA_real_,
               cp_resource_endowment = NA_real_, cp_deployment_scale = NA_real_, cp_innovation_position = NA_real_)
  a <- list(...)
  base[names(a)] <- a
  as.data.frame(base, stringsAsFactors = FALSE)
}

# Solar Assembly is measured (IEA manufacturing capacity for P1-P5 and BBB); Wind Components is not measured.
qt_fixture <- function() {
  sp <- rbind(
    qt_sp_row("AAA", trade_benchmark_share = 0.10),                                                   # exports, not listed
    qt_sp_row("BBB", trade_benchmark_share = 0.30),                                                   # exports 6x production
    qt_sp_row("SGP", trade_benchmark_share = 0.08, imports_usd = 0.9e8),                              # hub, re-exports
    qt_sp_row("CCC", trade_benchmark_share = 0.05, imports_usd = 0.8e8),                              # non-hub re-export pattern
    qt_sp_row("CCC", tech = "Wind", stage = "Components", trade_benchmark_share = 0.05, imports_usd = 0.8e8),  # same, stage unmeasured
    qt_sp_row("DDD", tech = "Wind", stage = "Components", strategic_choice = "preserve", cp_contender_pct = 0.9,
              cp_export_position = 0.9, trade_benchmark_share = 0.12),                                 # Preserve on exports alone
    qt_sp_row("EEE", tech = "Wind", stage = "Components", strategic_choice = "catch_up", catch_up_anchor_type = "direct_export_share",
              trade_benchmark_share = 0.03),                                                          # Catch Up anchored on exports
    qt_sp_row("FFF", trade_benchmark_share = 0.04, cp_export_position = 0.4),                         # projection only
    qt_sp_row("GGG", tech = "Wind", stage = "Components", strategic_choice = "preserve", cp_contender_pct = 0.9,
              cp_export_position = 0.5, cp_manufacturing_capacity = 0.9, cp_investment_position = 0.8, trade_benchmark_share = 0.15),
    qt_sp_row("P1", trade_benchmark_share = 0.001), qt_sp_row("ZZZ", stage = "Deployment", trade_benchmark_share = 0.5)
  )
  mss <- data.frame(iso3 = c("P1", "P2", "P3", "P4", "P5", "BBB", "WLD"), country = NA, tech = "Solar", supply_chain = "Assembly",
                    measure = "iea_manufacturing_capacity", mineral = NA_character_, world_share = c(0.3, 0.2, 0.1, 0.1, 0.05, 0.05, 1),
                    year = 2024L, value = 1, unit = "GW", is_aggregate = c(rep(FALSE, 6), TRUE), source = "IEA Global Supply Chains",
                    stringsAsFactors = FALSE)
  msm <- data.frame(Country = "F", iso3c = "FFF", tech = "Solar", supply_chain = "Assembly", category = "Market Share",
                    variable = "Overall Market Share", data_type = "raw", value = 0.2, Year = 2035L, source = "IEA ETP 2024",
                    stringsAsFactors = FALSE)
  list(sp = sp, tables = list(sp = sp, major_supplier_shares_tbl = mss, market_share_manufacturing_tbl = msm))
}

qt_flags <- function(fx = qt_fixture(), cfg = list()) {
  ev <- qc_production_evidence(fx$tables, as_of_year = 2026L)
  qc_trade_production(fx$sp, ev, cfg = cfg)
}
qt_has <- function(fl, iso3, flag, tech = NULL) {
  any(fl$iso3 == iso3 & fl$flag == flag & (is.null(tech) | fl$tech %in% tech))
}

test_that("production evidence reads shares, keeps projections apart and never lists the aggregate", {
  ev <- qc_production_evidence(qt_fixture()$tables, as_of_year = 2026L)
  expect_false("WLD" %in% ev$iso3)
  expect_equal(ev$kind[ev$iso3 == "FFF"], "projected_share")
  s <- qc_production_summary(ev, 5)
  expect_true(s$cover$stage_covered[s$cover$tech == "Solar" & s$cover$supply_chain == "Assembly"])
  expect_false(s$cell$production_positive[s$cell$iso3 == "FFF"])
})

test_that("high exports with no production where the stage is measured are flagged, with the evidence", {
  fl <- qt_flags()
  expect_true(qt_has(fl, "AAA", "trade_without_production"))
  r <- fl[fl$iso3 == "AAA" & fl$flag == "trade_without_production", ]
  expect_equal(r$production_status, "not_listed")
  expect_equal(r$status, "warn")
  expect_match(r$stage_sources, "IEA Global Supply Chains")
  expect_match(r$detail, "export share 10.0%")
  # a projection is not production
  expect_true(qt_has(fl, "FFF", "trade_without_production"))
  expect_equal(fl$projected_share[fl$iso3 == "FFF" & fl$flag == "trade_without_production"], 0.2)
  # Deployment is never read and a small exporter is not flagged
  expect_false(any(fl$iso3 %in% c("ZZZ", "P1")))
})

test_that("exports far above measured production are flagged with the ratio", {
  fl <- qt_flags()
  r <- fl[fl$iso3 == "BBB" & fl$flag == "export_exceeds_production", ]
  expect_equal(nrow(r), 1)
  expect_equal(r$export_to_production, 6)
  expect_false(qt_has(qt_flags(cfg = list(ratio = 8)), "BBB", "export_exceeds_production"))
})

test_that("the entrepot signature needs re-exports plus absent production, or a known hub", {
  fl <- qt_flags()
  expect_true(qt_has(fl, "SGP", "entrepot_signature"))
  expect_true(fl$known_hub[fl$iso3 == "SGP" & fl$flag == "entrepot_signature"])
  expect_true(fl$needs_human_explanation[fl$iso3 == "SGP" & fl$flag == "entrepot_signature"])
  expect_true(qt_has(fl, "CCC", "entrepot_signature", tech = "Solar"))
  # the same trade pattern where no production source measures the stage is a gap, not an entrepot
  expect_false(qt_has(fl, "CCC", "entrepot_signature", tech = "Wind"))
  expect_true(qt_has(fl, "CCC", "no_production_data_stage", tech = "Wind"))
  expect_false(qt_has(qt_flags(cfg = list(hubs = character(), reexport_ratio = 2)), "SGP", "entrepot_signature"))
})

test_that("a Preserve or Catch Up resting on trade alone is flagged; one backed by capacity is not", {
  fl <- qt_flags()
  d <- fl[fl$iso3 == "DDD" & fl$flag == "trade_driven_position", ]
  expect_equal(nrow(d), 1)
  expect_equal(d$export_weight, 1)
  expect_equal(d$production_status, "stage_not_covered")
  expect_true(d$needs_human_explanation)
  e <- fl[fl$iso3 == "EEE" & fl$flag == "trade_driven_position", ]
  expect_true(e$catch_up_trade_anchored)
  expect_match(e$detail, "Catch Up anchored only on trade")
  expect_false(qt_has(fl, "GGG", "trade_driven_position"))
})

test_that("the no-production-data gap is info and summary rows are produced per flag", {
  fl <- qt_flags()
  gap <- fl[fl$flag == "no_production_data_stage", ]
  expect_true(all(gap$status == "info"))
  expect_true(all(gap$production_status == "stage_not_covered"))
  ch <- qc_trade_production_checks(fl)
  expect_equal(nrow(ch), 5)
  expect_equal(ch$status[ch$check_id == "tp_no_production_data_stage"], "info")
  expect_equal(ch$status[ch$check_id == "tp_trade_driven_position"], "warn")
  empty <- qc_trade_production(qt_fixture()$sp[0, ], qc_production_evidence(list()))
  expect_equal(nrow(empty), 0)
})

test_that("a node whose only supply share is the Atlas export share makes direct_supply_share a trade anchor", {
  mss <- data.frame(tech = c("Nuclear", "Nuclear", "Solar"), supply_chain = c("Refining", "Refining", "Assembly"),
                    measure = c("atlas_export_share", "usgs_reserves", "iea_manufacturing_capacity"))
  expect_equal(qc_trade_share_nodes(mss), "Nuclear|Refining")
  fx <- qt_fixture()
  fx$sp <- rbind(fx$sp, qt_sp_row("NLD", tech = "Nuclear", stage = "Refining", strategic_choice = "catch_up",
                                  catch_up_anchor_type = "direct_supply_share;direct_export_share", trade_benchmark_share = 0.3))
  ev <- qc_production_evidence(fx$tables, as_of_year = 2026L)
  expect_true(qt_has(qc_trade_production(fx$sp, ev, trade_share_nodes = "Nuclear|Refining"), "NLD", "trade_driven_position"))
  expect_false(qt_has(qc_trade_production(fx$sp, ev), "NLD", "trade_driven_position"))
})

test_that("a Catch Up at Recovery anchored on the export share is flagged with the scrap-export caveat", {
  fx <- qt_fixture()
  fx$sp <- rbind(fx$sp, qt_sp_row("USA", tech = "Batteries", stage = "Recovery", strategic_choice = "catch_up",
                                  catch_up_anchor_type = "direct_export_share", cp_contender_pct = NA_real_,
                                  competitive_position_score = NA_real_, cp_export_position = NA_real_, trade_benchmark_share = 0.12))
  fl <- qc_trade_production(fx$sp, qc_production_evidence(fx$tables, as_of_year = 2026L))
  r <- fl[fl$iso3 == "USA" & fl$flag == "trade_driven_position", ]
  expect_equal(nrow(r), 1)
  expect_true(r$catch_up_trade_anchored)
  expect_match(r$detail, "shipped abroad for processing")
  expect_true(qt_has(fl, "USA", "no_production_data_stage"))
})
