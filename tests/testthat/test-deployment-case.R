# The deployment case tag (R/strategic_position/deployment_need.R dn_cases() and its case-only measures;
# decision_engine.R deploy_case and strong_deployment_case). A descriptive tag: it never changes
# deployment_need or the choice. Synthetic fixtures; the committed config/deployment_need.yml.

dct_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

suppressPackageStartupMessages(library(dplyr))
source(file.path(dct_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(dct_root(), "R", "strategic_position", "deployment_need.R"))

dct_cfg <- function() yaml::read_yaml(file.path(dct_root(), "config", "deployment_need.yml"))

# A scored measure table (dn_score_measures() shape: iso3, tech, measure, value, pct) from a compact spec.
dct_scored <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) data.frame(iso3 = r[[1]], tech = r[[2]], measure = r[[3]], value = as.numeric(r[[4]]),
                                                     pct = as.numeric(r[[5]]), stringsAsFactors = FALSE)))
}
dct_grid <- function(iso3, tech) data.frame(iso3 = iso3, tech = tech, Country = iso3, stringsAsFactors = FALSE)

test_that("the strongest material case wins; a close second is named too", {
  cfg <- dct_cfg()
  scored <- dct_scored(
    # AAA: high fuel imports (security), a small target shortfall, flat demand
    list("AAA", "Solar", "import_displacement", 0.5, 0.95), list("AAA", "Solar", "renewables_target_shortfall", 2, 0.6),
    list("AAA", "Solar", "demand_growth", 0.5, 0.2),
    # BBB: a big target shortfall and fast demand growth, no fuel imports
    list("BBB", "Solar", "import_displacement", 0, 0.2), list("BBB", "Solar", "renewables_target_shortfall", 20, 0.9),
    list("BBB", "Solar", "demand_growth", 5, 0.85),
    # CCC: middling everywhere, but its fuel imports are below the material gate
    list("CCC", "Solar", "import_displacement", 0.05, 0.6), list("CCC", "Solar", "renewables_target_shortfall", -1, 0.3),
    list("CCC", "Solar", "demand_growth", -0.5, 0.5)
  )
  out <- dn_cases(scored, dct_grid(c("AAA", "BBB", "CCC"), "Solar"), cfg)
  expect_equal(out$dn_case[out$iso3 == "AAA"], "security")
  # within the technology, the case percentile re-ranks: BBB is top on climate (its targets) and demand, within the margin
  expect_true(out$dn_case[out$iso3 == "BBB"] %in% c("climate+demand", "demand+climate"))
  # relatively high but not material (imports below 10% of generation, a target already met, falling demand): none
  expect_equal(out$dn_case[out$iso3 == "CCC"], "none")
  expect_false(out$dn_case_security_material[out$iso3 == "CCC"])
  expect_equal(out$dn_case_basis, rep("multi_case", 3))
})

test_that("fossil technologies have no climate case; missing data is not assessed, never weak", {
  cfg <- dct_cfg()
  scored <- dct_scored(
    list("AAA", "Coal", "demand_growth", 3, 0.9), list("AAA", "Coal", "target_shortfall", 5, 0.8),
    list("AAA", "Electric Vehicles", "ev_share_growth", 2, 0.7), list("AAA", "Oil", "oil_demand_growth", 1, 0.6)
  )
  out <- dn_cases(scored, dct_grid(c("AAA", "AAA", "AAA", "ZZZ"), c("Coal", "Electric Vehicles", "Oil", "Solar")), cfg)
  coal <- out[out$tech == "Coal", ]
  expect_equal(coal$dn_case_climate_status, "not_applicable")
  # a Coal national-plan shortfall is demand, not climate
  expect_equal(coal$dn_case, "demand")
  expect_false(grepl("climate", dplyr::coalesce(coal$dn_case_unassessed, "")))
  ev <- out[out$tech == "Electric Vehicles", ]
  # climate and security measures are attributed to EVs but have no data here: not assessed
  expect_equal(ev$dn_case_climate_status, "not_assessed")
  expect_equal(ev$dn_case_security_status, "not_assessed")
  expect_true(all(c("security", "climate") %in% strsplit(ev$dn_case_unassessed, ";")[[1]]))
  expect_equal(ev$dn_case_basis, "single_case")
  # no security measure is attributed to Oil: not measured, which is not a data gap for this country
  oil <- out[out$tech == "Oil", ]
  expect_equal(oil$dn_case_security_status, "not_measured")
  expect_false(grepl("security", dplyr::coalesce(oil$dn_case_unassessed, "")))
  zzz <- out[out$iso3 == "ZZZ", ]
  expect_true(is.na(zzz$dn_case))
  expect_true(is.na(zzz$dn_case_basis))
})

test_that("include keeps a mostly-zero measure out of a case where it is absent", {
  cfg <- dct_cfg()
  scored <- dct_scored(
    list("AAA", "Solar", "demand_growth", 2, 0.8), list("AAA", "Solar", "access_gap", 0, 0.3),
    list("BBB", "Solar", "demand_growth", 2, 0.8), list("BBB", "Solar", "access_gap", 40, 0.95)
  )
  out <- dn_cases(scored, dct_grid(c("AAA", "BBB"), "Solar"), cfg)
  expect_equal(out$dn_case_demand_score[out$iso3 == "AAA"], 0.8)
  expect_equal(out$dn_case_demand_score[out$iso3 == "BBB"], mean(c(0.8, 0.95)))
})

test_that("measures sharing a group count once in a case", {
  cfg <- dct_cfg()
  scored <- dct_scored(
    list("AAA", "Solar", "import_displacement", 0.4, 0.9), list("AAA", "Solar", "fuel_supplier_risk", 0.2, 0.7),
    list("AAA", "Solar", "electricity_import_share", 20, 0.3)
  )
  out <- dn_cases(scored, dct_grid("AAA", "Solar"), cfg)
  # (mean(0.9, 0.7) + 0.3) / 2, not mean(0.9, 0.7, 0.3)
  expect_equal(out$dn_case_security_score, (0.8 + 0.3) / 2)
})

test_that("grid integration is a case of its own: a material wind and solar share", {
  cfg <- dct_cfg()
  scored <- dct_scored(list("AAA", "Batteries", "flexibility_need", 40, 0.9), list("BBB", "Batteries", "flexibility_need", 5, 0.1))
  out <- dn_cases(scored, dct_grid(c("AAA", "BBB"), "Batteries"), cfg)
  expect_equal(out$dn_case, c("integration", "none"))
  expect_equal(out$dn_case_integration_material, c(TRUE, FALSE))
})

test_that("the climate case reads a climate-aligned pathway as well as targets, and anchors EVs", {
  cfg <- dct_cfg()
  scored <- dct_scored(
    # a country without targets still has a climate case from the pathway
    list("AAA", "Solar", "climate_pathway", 80, 0.9), list("AAA", "Solar", "demand_growth", 1, 0.3),
    # an EV leader: low on distance to all-electric sales, high on oil imports and demand; climate is named first
    list("BBB", "Electric Vehicles", "ev_sales_gap", 40, 0.1), list("BBB", "Electric Vehicles", "climate_pathway", 50, 0.2),
    list("BBB", "Electric Vehicles", "import_displacement", 0.9, 0.95), list("BBB", "Electric Vehicles", "ev_share_growth", 5, 0.9),
    # targets and the pathway count as one group each
    list("CCC", "Wind", "renewables_target_shortfall", 10, 0.6), list("CCC", "Wind", "target_shortfall", 5, 0.8),
    list("CCC", "Wind", "climate_pathway", 40, 0.4), list("CCC", "Wind", "fossil_power_share", 60, 0.9)
  )
  out <- dn_cases(scored, dct_grid(c("AAA", "BBB", "CCC"), c("Solar", "Electric Vehicles", "Wind")), cfg)
  # one country per technology: every case ranks 0.5, so climate (tie order) leads and demand follows
  expect_equal(out$dn_case[out$iso3 == "AAA"], "climate+demand")
  expect_equal(out$dn_case[out$iso3 == "BBB"], "climate+security")
  expect_equal(out$dn_case_climate_score[out$iso3 == "CCC"], mean(c(mean(c(0.6, 0.8)), mean(c(0.4, 0.9)))))
})

test_that("case-only measures never enter deployment_need", {
  cfg <- dct_cfg()
  m <- rbind(
    dn_finish_measure(data.frame(iso3 = c("AAA", "BBB"), tech = "Solar", value = c(1, 2), basis = "national", stringsAsFactors = FALSE), cfg, "demand_growth"),
    dn_finish_measure(data.frame(iso3 = c("AAA", "BBB"), tech = "Solar", value = c(50, 0), basis = "national", stringsAsFactors = FALSE), cfg, "access_gap")
  )
  u <- data.frame(iso3 = c("AAA", "BBB"), Country = c("A", "B"), stringsAsFactors = FALSE)
  with_case <- dn_assemble(m, u, cfg)
  without <- dn_assemble(m[m$measure != "access_gap", ], u, cfg)
  expect_equal(with_case$deployment_need, without$deployment_need)
  expect_equal(unique(m$family[m$measure == "access_gap"]), DN_CASE_ONLY)
})

test_that("fuel supplier risk weights displaced imported fuel by the coercive share of its imports", {
  cfg <- dct_cfg()
  ember <- data.frame(iso3 = "AAA", year = 2024L, source = c("Total generation", "Coal", "Gas", "Other fossil"),
                      generation_twh = c(100, 40, 20, 0), capacity_gw = NA_real_, share_pct = NA_real_, stringsAsFactors = FALSE)
  import_dep <- data.frame(iso3 = "AAA", fuel = c("Coal", "Gas", "Oil"), import_dependence = c(1, 0.5, 0), year = 2025L, stringsAsFactors = FALSE)
  risk <- dn_fuel_supplier_risk(data.frame(importer_iso = "AAA", tech = c("Coal", "Gas", "Oil"), supply_chain = "Extraction", observed = TRUE,
                                           coercive_import_share = c(0.5, 0.1, 0.9), import_hhi = 0.2, top_supplier_iso3 = "XXX",
                                           stringsAsFactors = FALSE))
  spec <- list(Solar = list(displaces = list("Coal", "Gas", "Other fossil")))
  plain <- dn_measure_import_displacement(ember, import_dep, spec, cfg)
  risky <- dn_measure_import_displacement(ember, import_dep, spec, cfg, risk = risk, measure = "fuel_supplier_risk")
  expect_equal(plain$value, 0.4 * 1 + 0.2 * 0.5)
  expect_equal(risky$value, 0.4 * 1 * 0.5 + 0.2 * 0.5 * 0.1)
  expect_equal(risky$family, DN_CASE_ONLY)
})

test_that("electricity imports and the access gap read Ember and WDI", {
  cfg <- dct_cfg()
  ember <- data.frame(iso3 = c("AAA", "AAA", "BBB", "BBB"), year = 2024L, source = c("Net imports", "Demand", "Net imports", "Demand"),
                      generation_twh = c(20, 100, -5, 50), capacity_gw = NA_real_, share_pct = NA_real_, stringsAsFactors = FALSE)
  ei <- dn_measure_electricity_imports(ember, "Solar", cfg)
  expect_equal(ei$value[ei$iso3 == "AAA"], 20)
  expect_equal(ei$value[ei$iso3 == "BBB"], 0)
  wdi <- data.frame(Country.Name = c("A", "B", "World"), Country.Code = c("AAA", "BBB", "WLD"), Series.Name = "x",
                    Series.Code = "EG.ELC.ACCS.ZS", X2022..YR2022. = c("80", "100", "90"), X2023..YR2023. = c("85", "..", "91"),
                    stringsAsFactors = FALSE)
  acc <- dn_wdi_latest(wdi, "EG.ELC.ACCS.ZS", is_country = function(i) i != "WLD")
  expect_equal(acc$value[acc$iso3 == "AAA"], 85)
  expect_equal(acc$year[acc$iso3 == "BBB"], 2022L)
  expect_false("WLD" %in% acc$iso3)
  gap <- dn_measure_access_gap(acc, "Solar", cfg)
  expect_equal(gap$value[gap$iso3 == "AAA"], 15)
})

test_that("the BNEF growth sensitivity: ETS compound growth, national unless regional proxies are asked for", {
  neo <- data.frame(scenario = "ETS", region = c("Japan", "Other Europe"), sector = "Power & heat",
                    indicator = "Installed electric capacity", fuel = "Battery storage", y2024 = c(10, 4), y2035 = c(40, 8),
                    stringsAsFactors = FALSE)
  ro <- data.frame(iso3 = c("JPN", "POL"), region = c("Japan", "Other Europe"), basis = c("national", "regional_proxy"),
                   stringsAsFactors = FALSE)
  spec <- list(family = "dn_demand", direction = 1, base_year = 2024, target_year = 2035,
               techs = list(Batteries = list(sector = "Power & heat", indicator = "Installed electric capacity", fuels = list("Battery storage"))))
  cfg <- dct_cfg()
  cfg$measures$neo_growth <- spec
  nat <- dn_measure_neo_growth(neo, ro, cfg)
  expect_equal(nat$iso3, "JPN")
  expect_equal(nat$value, 100 * ((40 / 10)^(1 / 11) - 1))
  expect_equal(nat$family, "dn_demand")
  cfg$measures$neo_growth$regional_proxy <- TRUE
  both <- dn_measure_neo_growth(neo, ro, cfg)
  expect_equal(sort(both$iso3), c("JPN", "POL"))
  expect_equal(both$basis[both$iso3 == "POL"], "regional_proxy")
  # the committed config never names it (D13)
  expect_null(dct_cfg()$measures$neo_growth)
})

test_that("the kind of Watch: phase-down (fossil, pathway), momentum, declining, low need; evidence only", {
  cfg <- dct_cfg()
  scored <- dct_scored(list("AAA", "Coal", "climate_pathway", -70, 0.1), list("BBB", "Green Hydrogen", "climate_pathway", -35, 0.1))
  dn <- data.frame(iso3 = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
                   tech = c("Coal", "Green Hydrogen", "Solar", "Wind", "Nuclear", "Solar", "Gas"),
                   deployment_need = c(0.3, 0.3, 0.4, 0.4, 0.4, NA, 0.3),
                   deployment_trend_growth = c(5, NA, 25, -8, 1, 40, 30),
                   dn_target_met = c(NA, NA, FALSE, NA, TRUE, NA, TRUE), stringsAsFactors = FALSE)
  out <- dn_watch_kind(scored, dn, cfg)
  k <- stats::setNames(out$dn_watch_kind, out$iso3)
  expect_equal(unname(k["AAA"]), "phase_down")      # a fossil technology the pathway deploys far less of, though it grows
  expect_equal(unname(k["BBB"]), "low_need")        # a lower hydrogen series is not a call to phase green hydrogen down
  expect_equal(unname(k["CCC"]), "momentum")        # +25% a year
  expect_equal(unname(k["DDD"]), "declining")       # -8% a year
  expect_equal(unname(k["EEE"]), "momentum")        # the national target met at the recent build rate
  expect_true(is.na(k["FFF"]))                      # not assessed: no kind
  expect_equal(unname(k["GGG"]), "momentum")        # gas growing fast with no pathway reading: momentum (a met plan never counts for fossil)
})
