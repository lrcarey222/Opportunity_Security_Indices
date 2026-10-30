# Deployment Need (R/strategic_position/deployment_need.R; spec Parts V.2, VI and XXII "Deployment Need";
# design D12). Synthetic fixtures; the committed config/deployment_need.yml.

dnt_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path(dnt_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(dnt_root(), "R", "strategic_position", "deployment_need.R"))
source(file.path(dnt_root(), "R", "strategic_position", "home_market.R"))

dnt_cfg <- function() yaml::read_yaml(file.path(dnt_root(), "config", "deployment_need.yml"))

DNT_CAPF <- c(Solar = 0.15, Wind = 0.3, Coal = 0.6, Gas = 0.5, "Other fossil" = 0.3, Nuclear = 0.9, Hydro = 0.4,
              Bioenergy = 0.6, "Other renewables" = 0.7)

# dn_ember_prepare()-shaped rows for one economy: each source's generation starts at gen0 in the first
# year and grows by its factor in `g` a year (1 when not named); capacity = generation / (8.76 x a
# capacity factor); Demand = Total generation.
dnt_country <- function(iso, gen0, g = NULL, years = 2019:2025) {
  gg <- stats::setNames(rep(1, length(gen0)), names(gen0))
  if (!is.null(g)) gg[names(g)] <- g
  re <- intersect(names(gen0), c("Solar", "Wind", "Hydro", "Bioenergy", "Other renewables"))
  do.call(rbind, lapply(years, function(y) {
    gen <- gen0 * gg^(y - years[1])
    cap <- gen / (8.76 * DNT_CAPF[names(gen0)])
    tot <- sum(gen)
    data.frame(iso3 = iso, year = y,
               source = c(names(gen0), "Renewables", "Total generation", "Demand"),
               generation_twh = c(unname(gen), sum(gen[re]), tot, tot),
               capacity_gw = c(unname(cap), sum(cap[re]), sum(cap), NA),
               share_pct = c(unname(100 * gen / tot), 100 * sum(gen[re]) / tot, 100, 100),
               stringsAsFactors = FALSE)
  }))
}

# AAA: fast-growing system on imported coal and gas, a single-country WEO region with a solar target.
# BBB: flat, nuclear-heavy, imports all its gas, no targets. CCC: shrinking coal on domestic coal, a
# coal phase-down plan. EEE: a tiny system with no import-dependence data. DDD: in the universe, no data.
dnt_ember <- function() {
  rbind(
    dnt_country("AAA", c(Solar = 10, Wind = 10, Coal = 50, Gas = 30), g = c(Solar = 1.3, Wind = 1.2, Coal = 1.05)),
    dnt_country("BBB", c(Solar = 5, Wind = 5, Gas = 20, Nuclear = 70)),
    dnt_country("CCC", c(Solar = 1, Coal = 40, Gas = 10, Hydro = 50), g = c(Coal = 0.9, Solar = 1.5)),
    dnt_country("EEE", c(Solar = 0.1, Gas = 0.5))
  )
}

dnt_universe <- function() {
  data.frame(iso3 = c("AAA", "BBB", "CCC", "DDD", "EEE"),
             Country = c("Aland", "Bland", "Cland", "Dland", "Eland"), stringsAsFactors = FALSE)
}

dnt_import_dep <- function() {
  data.frame(iso3 = c("AAA", "AAA", "AAA", "BBB", "BBB", "CCC", "CCC", "CCC"),
             fuel = c("Coal", "Gas", "Oil", "Gas", "Oil", "Coal", "Gas", "Oil"),
             year = 2025L, import_dependence = c(1, 1, 1, 1, 0.5, 0, 0.5, 0.2), stringsAsFactors = FALSE)
}

# WEO 2025 single-country region rows for AAA (dn_weo_national() shape).
dnt_weo <- function() {
  s <- data.frame(
    series = rep(c("gen_total", "gen_solar", "gen_wind", "gen_coal", "gen_gas", "gen_nuclear"), each = 3),
    scenario = rep(c("historical", "current", "stated"), times = 6),
    year = rep(c(2024L, 2035L, 2035L), times = 6),
    value = c(100, 150, 140, 20, 60, 70, 15, 30, 35, 40, 35, 30, 25, 25, 20, 0, 0, 0),
    stringsAsFactors = FALSE)
  cbind(iso3 = "AAA", region = "Aland", s, stringsAsFactors = FALSE)
}

dnt_targets <- function() {
  data.frame(iso3 = c("AAA", "AAA", "CCC"), target_key = c("Solar", "Renewables", "Coal"),
             target_gw = c(120, 200, 1), target_year = 2030L,
             target_kind = c("renewable_target", "renewable_target", "national_plan"),
             target_source = "test", target_note = "test", stringsAsFactors = FALSE)
}

dnt_gevo_share <- function() {
  rbind(
    data.frame(iso3 = "AAA", category = "Historical", year = 2019:2025, value = c(1, 2, 3, 5, 8, 12, 16)),
    data.frame(iso3 = "AAA", category = "Projection-CPS", year = 2035L, value = 46),
    data.frame(iso3 = "BBB", category = "Historical", year = 2019:2025, value = c(1, 1, 2, 2, 3, 3, 4))
  )
}

dnt_oil <- function() {
  rbind(data.frame(iso3 = "AAA", year = 2019:2025, value = 2 * 1.04^(0:6)),
        data.frame(iso3 = "BBB", year = 2019:2025, value = rep(1, 7)),
        data.frame(iso3 = "CCC", year = 2019:2025, value = 3 * 0.97^(0:6)))
}

dnt_bess <- function() {
  data.frame(iso3 = "AAA", year = 2020:2025, capacity_gw = c(0.1, 0.3, 0.6, 1, 2, 4))
}

dnt_inputs <- function() {
  list(ember = dnt_ember(), weo_national = dnt_weo(), import_dep = dnt_import_dep(), gevo_share = dnt_gevo_share(),
       oil = dnt_oil(), targets = dnt_targets(), bess = dnt_bess())
}

dnt_build <- function(cfg = dnt_cfg(), inp = dnt_inputs()) {
  built <- dn_build_measures(inp, cfg)
  list(built = built, tbl = dn_assemble(built$measures, dnt_universe(), cfg), cfg = cfg)
}

test_that("Deployment Need is defined for the Deployment & Use stage only, one row per country and technology", {
  r <- dnt_build()
  out <- r$tbl
  techs <- unlist(r$cfg$techs)
  expect_setequal(techs, c("Batteries", "Electric Grid", "Electric Vehicles", "Green Hydrogen", "Nuclear", "Solar",
                           "Wind", "Geothermal", "Coal", "Gas", "Oil"))
  expect_equal(nrow(out), nrow(dnt_universe()) * length(techs))
  expect_true(all(out$supply_chain == "Deployment"))
  expect_true(all(out$need_type == "deployment"))
  expect_false(anyDuplicated(out[, c("iso3", "tech")]) > 0)
  dn <- out$deployment_need[!is.na(out$deployment_need)]
  expect_true(length(dn) > 0 && all(dn >= 0 & dn <= 1))
  # never combined with supply-chain need (spec VI)
  expect_false(any(grepl("supply_chain_need|strategic_need", names(out))))
  # a technology outside the configured eleven is ignored
  extra <- r$built$measures[1, ]
  extra$tech <- "Semiconductors"
  out2 <- dn_assemble(rbind(r$built$measures, extra), dnt_universe(), r$cfg)
  expect_false("Semiconductors" %in% out2$tech)
})

test_that("deployment_need is the mean of the observed family percentiles", {
  out <- dnt_build()$tbl
  fam <- as.matrix(out[, DN_FAMILIES])
  n <- unname(rowSums(!is.na(fam)))
  expect_equal(out$dn_families_observed, as.integer(n))
  expected <- ifelse(n > 0, unname(rowSums(fam, na.rm = TRUE)) / pmax(n, 1), NA_real_)
  expect_equal(out$deployment_need, expected)
  # the family percentile is the within-technology mid-rank of the family value
  for (t in unique(out$tech)) {
    s <- out[out$tech == t, ]
    expect_equal(s$dn_demand, sp_midrank_pct(s$dn_demand_score))
  }
  expect_equal(out$dn_confidence[out$dn_families_observed == 3], rep("high", sum(out$dn_families_observed == 3)))
  expect_true(all(out$dn_confidence[out$dn_families_observed == 0] == "none"))
})

test_that("scenarios are labelled per family and the climate-aligned value is a separate alternative", {
  cfg <- dnt_cfg()
  expect_equal(cfg$families$dn_demand$scenario, "current_policy")
  expect_equal(cfg$families$dn_policy_gap$scenario, "national_commitments")
  out <- dnt_build(cfg)$tbl
  expect_true(all(out$dn_demand_scenario == "current_policy"))
  expect_true(all(out$dn_policy_gap_scenario == "national_commitments"))
  expect_true(all(out$dn_system_scenario %in% DN_SCENARIOS))
  # the scenario list of a row names only the families that were observed
  aaa_solar <- out[out$iso3 == "AAA" & out$tech == "Solar", ]
  expect_equal(aaa_solar$dn_scenarios, "current_policy; national_commitments")
  bbb_solar <- out[out$iso3 == "BBB" & out$tech == "Solar", ]
  expect_equal(bbb_solar$dn_scenarios, "current_policy")
  # no climate-aligned input reaches the score
  expect_false(any(grepl("climate", names(out))))
  pw <- data.frame(series = rep(c("cap_solar", "cap_coal"), each = 2), scenario = rep(c("nze", "current"), 2),
                   year = 2035L, value = c(300, 200, 50, 100), stringsAsFactors = FALSE)
  ca <- dn_climate_aligned(pw, cfg)
  expect_equal(ca$dn_climate_aligned[ca$tech == "Solar"], 0.5)
  expect_equal(ca$dn_climate_aligned[ca$tech == "Coal"], -0.5)
  expect_true(all(ca$dn_climate_aligned_scenario == "climate_aligned"))
  expect_true(all(ca$dn_climate_aligned_basis[!is.na(ca$dn_climate_aligned)] == "global_context"))
  # a series with no data stays NA (not_assessed), never zero
  expect_true(is.na(ca$dn_climate_aligned[ca$tech == "Wind"]))
  expect_equal(ca$dn_climate_aligned_basis[ca$tech == "Wind"], "not_assessed")
})

test_that("demand adequacy uses WEO Current Policies where national and the observed trend elsewhere, consistently across power technologies", {
  cfg <- dnt_cfg()
  m <- dnt_build(cfg)$built$measures
  d <- m[m$measure == "demand_growth", ]
  aaa <- d[d$iso3 == "AAA", ]
  expect_true(all(aaa$basis == "national"))
  expect_equal(unique(aaa$value), 100 * ((150 / 100)^(1 / 11) - 1))
  bbb <- d[d$iso3 == "BBB", ]
  expect_true(all(bbb$basis == "observed_trend"))
  expect_equal(unique(bbb$value), 0)
  ccc <- d[d$iso3 == "CCC", ]
  tot <- function(y) sum(c(1 * 1.5^(y - 2019), 40 * 0.9^(y - 2019), 10, 50))
  expect_equal(unique(ccc$value), 100 * ((tot(2025) / tot(2020))^(1 / 5) - 1))
  # system-level attribution: every attributed technology carries the same value
  expect_setequal(unique(d$tech), unlist(cfg$measures$demand_growth$techs))
  expect_equal(nrow(unique(d[, c("iso3", "value")])), length(unique(d$iso3)))
  # a system below min_system_twh gets no growth measure (not a zero)
  expect_false("EEE" %in% d$iso3)
  # the technology's own output growth (WEO for AAA): percentage points of the 2024 system a year
  og <- m[m$measure == "output_growth" & m$iso3 == "AAA", ]
  expect_equal(og$value[og$tech == "Solar"], 100 * (60 - 20) / (11 * 100))
  expect_equal(og$value[og$tech == "Batteries"], 100 * ((60 + 30) - (20 + 15)) / (11 * 100))
  expect_equal(og$value[og$tech == "Coal"], 100 * (35 - 40) / (11 * 100))
})

test_that("target gaps are computed consistently from targets, installed capacity and the recent build rate", {
  targets <- data.frame(iso3 = c("XXX", "XXX", "YYY"), target_key = c("Solar", "Nuclear", "Solar"),
                        target_gw = c(30, 4, 8), target_year = 2030L, target_kind = c("renewable_target", "national_plan", "renewable_target"),
                        target_source = "t", target_note = "n", stringsAsFactors = FALSE)
  installed <- data.frame(iso3 = c("XXX", "XXX", "YYY"), key = c("Solar", "Nuclear", "Solar"), installed_year = 2025L,
                          installed_gw = c(10, 5, 10), capacity_from = c(4, 5, 7), build_rate_gw = c(2, 0, 1))
  fleet <- data.frame(iso3 = c("XXX", "YYY"), year = 2025L, fleet_gw = c(100, 50))
  sf <- dn_target_shortfall(targets, installed, fleet, list(Solar = "Solar", Nuclear = "Nuclear"))
  x <- sf[sf$iso3 == "XXX" & sf$target_key == "Solar", ]
  expect_equal(x$projected_gw, 20)
  expect_equal(x$shortfall_gw, 10)
  expect_equal(x$shortfall_share, 10)
  expect_equal(x$required_rate_gw, 4)
  expect_equal(x$acceleration, 2)
  expect_false(x$target_met)
  # ahead of the target: a negative shortfall (less need), not zero
  y <- sf[sf$iso3 == "YYY", ]
  expect_equal(y$shortfall_gw, 8 - 15)
  expect_true(y$target_met)
  # the same arithmetic on the full build
  sf2 <- dnt_build()$built$shortfall
  expect_equal(sf2$projected_gw, sf2$installed_gw + sf2$build_rate_gw * sf2$years_left)
  expect_equal(sf2$shortfall_gw, sf2$target_gw - sf2$projected_gw)
  expect_equal(sf2$shortfall_share, 100 * sf2$shortfall_gw / sf2$fleet_gw)
  # the policy-gap measure is exactly the shortfall share, and Batteries / Electric Grid read the
  # renewables commitment
  m <- dnt_build()$built$measures
  ts <- m[m$measure == "target_shortfall" & m$iso3 == "AAA" & m$tech == "Solar", ]
  expect_equal(ts$value, sf2$shortfall_share[sf2$iso3 == "AAA" & sf2$target_key == "Solar"])
  re <- m[m$measure == "renewables_target_shortfall" & m$iso3 == "AAA", ]
  expect_setequal(re$tech, c("Solar", "Wind", "Batteries", "Electric Grid"))
  expect_equal(unique(re$value), sf2$shortfall_share[sf2$iso3 == "AAA" & sf2$target_key == "Renewables"])
  expect_true(all(re$scenario == "national_commitments"))
})

test_that("missing deployment evidence stays NA, never zero", {
  out <- dnt_build()$tbl
  # no target: the policy gap is not assessed
  bbb <- out[out$iso3 == "BBB" & out$tech == "Solar", ]
  expect_true(is.na(bbb$dn_policy_gap))
  expect_equal(bbb$dn_policy_gap_basis, "not_assessed")
  expect_match(bbb$dn_policy_gap_note, "no data for this country")
  expect_false(is.na(bbb$deployment_need))
  # a technology with no attributed measure in a family
  ev <- out[out$iso3 == "AAA" & out$tech == "Electric Vehicles", ]
  expect_true(is.na(ev$dn_policy_gap))
  expect_match(ev$dn_policy_gap_note, "no measure attributed")
  # a country with no data at all
  ddd <- out[out$iso3 == "DDD", ]
  expect_true(all(is.na(ddd$deployment_need)))
  expect_true(all(ddd$dn_families_observed == 0))
  expect_true(all(ddd$dn_basis == "not_assessed"))
  expect_true(all(ddd$dn_confidence == "none"))
  # dropping the targets input leaves the policy gap NA everywhere and other families untouched
  inp <- dnt_inputs()
  inp$targets <- inp$targets[0, ]
  out2 <- dnt_build(inp = inp)$tbl
  expect_true(all(is.na(out2$dn_policy_gap)))
  expect_equal(out2$dn_demand, out$dn_demand)
  expect_equal(out2$dn_system, out$dn_system)
})

test_that("unknown import dependence of a fuel that matters makes import displacement unknown, not zero", {
  cfg <- dnt_cfg()
  m <- dnt_build(cfg)$built$measures
  imp <- m[m$measure == "import_displacement", ]
  # EEE burns gas (83% of generation) with no import-dependence data
  expect_false(any(imp$iso3 == "EEE"))
  # BBB burns no coal, so its missing coal import dependence does not matter
  expect_true(any(imp$iso3 == "BBB" & imp$tech == "Solar"))
  bbb_solar <- imp$value[imp$iso3 == "BBB" & imp$tech == "Solar"]
  expect_equal(bbb_solar, 20 / 100 * 1)
})

test_that("fossil technologies are assessed by the same families and code path", {
  r <- dnt_build()
  out <- r$tbl
  fossil <- out[out$tech %in% c("Coal", "Gas", "Oil"), ]
  expect_true(all(c(DN_FAMILIES, paste0(DN_FAMILIES, "_basis")) %in% names(fossil)))
  expect_true(any(!is.na(fossil$deployment_need[fossil$tech == "Coal"])))
  expect_true(any(!is.na(fossil$deployment_need[fossil$tech == "Gas"])))
  expect_true(any(!is.na(fossil$deployment_need[fossil$tech == "Oil"])))
  imp <- r$built$measures[r$built$measures$measure == "import_displacement", ]
  # a coal plant burning imported coal displaces no imports
  expect_equal(imp$value[imp$iso3 == "AAA" & imp$tech == "Coal"], 0)
  # on domestic coal it displaces the imported gas it replaces: gas share x gas import dependence
  ccc_gas_share <- 10 / sum(c(1 * 1.5^6, 40 * 0.9^6, 10, 50))
  expect_equal(imp$value[imp$iso3 == "CCC" & imp$tech == "Coal"], ccc_gas_share * 0.5)
  # Solar in the same country displaces imported coal (none) and imported gas
  expect_equal(imp$value[imp$iso3 == "CCC" & imp$tech == "Solar"], ccc_gas_share * 0.5)
  # a coal phase-down plan below the projected fleet is a negative gap (least need), still assessed
  ccc_coal <- r$built$measures[r$built$measures$measure == "target_shortfall" & r$built$measures$iso3 == "CCC", ]
  expect_equal(ccc_coal$tech, "Coal")
  expect_lt(ccc_coal$value, 0)
  expect_false(is.na(out$dn_policy_gap[out$iso3 == "CCC" & out$tech == "Coal"]))
})

test_that("Ember targets: only commitments count, wind adds onshore and offshore, the EU row is dropped", {
  tcfg <- dnt_cfg()$targets
  cap_wide <- data.frame(country_code = c("AAA", "BBB", "EU"), target_year = 2030, Solar = c(10, NA, 500),
                         Wind = c(NA, 7, 300), "Onshore Wind" = c(4, NA, NA), "Offshore Wind" = c(2, NA, NA),
                         res_capacity_target = c(20, 9, 900), check.names = FALSE)
  sources <- data.frame(SOURCE_ID = c("1", "2", "3"), COUNTRY_CODE = c("AAA", "AAA", "BBB"),
                        SOURCE_NAME = c("Plan A", "Study B", "Law C"), PUBLISHER = "x",
                        SOURCE_TYPE = c("National strategy or plans (approved)", "Third-party study", "Written into law"),
                        ANNOUNCEMENT_DATE = c("2024-01-01", "2025-01-01", "2023-06-01"), INCLUDE_IN_TOOL = c(TRUE, FALSE, TRUE))
  raw_long <- data.frame(COUNTRY_CODE = c("AAA", "AAA", "BBB"), TARGET_YEAR = 2030, FUEL_CATEGORY = c("Nuclear", "Nuclear", "Gas"),
                         METRIC = "capacity_total_gw", VALUE = c(3, 9, 12), SOURCE_ID = c("1", "2", "3"))
  t <- dn_targets_prepare(cap_wide, raw_long, sources, tcfg)
  expect_false("EU" %in% t$iso3)
  expect_equal(t$target_gw[t$iso3 == "AAA" & t$target_key == "Wind"], 6)
  expect_equal(t$target_gw[t$iso3 == "BBB" & t$target_key == "Wind"], 7)
  # the third-party study (9 GW) is not a commitment; the approved plan (3 GW) is
  expect_equal(t$target_gw[t$iso3 == "AAA" & t$target_key == "Nuclear"], 3)
  expect_equal(t$target_kind[t$iso3 == "BBB" & t$target_key == "Gas"], "national_plan")
  expect_false(any(t$iso3 == "BBB" & t$target_key == "Solar"))
})

test_that("input parsers keep countries only and convert units", {
  sheet <- data.frame(
    a = c("Battery Energy Storage System (BESS) Capacity", NA, "Installed Capacity (Megawatts)", "Canada",
          "Behind-the-meter", "Front-of-the-meter", "Total North America", "Rest of Europe", "Atlantis"),
    b = c(NA, NA, "2024", "1000", "400", "600", "5000", "70", "1"),
    c = c(NA, NA, "2025", "2000", "800", "1200", "9000", "80", "2"),
    stringsAsFactors = FALSE)
  to_iso <- function(x) c(Canada = "CAN")[x]
  b <- dn_ei_bess_prepare(sheet, to_iso)
  expect_setequal(unique(b$iso3), "CAN")
  expect_equal(b$capacity_gw[b$year == 2025], 2)
  gevo <- data.frame(region_country = c("Korea", "World", "Korea"), category = "Historical",
                     parameter = c("EV sales share", "EV sales share", "EV stock"), mode = "Cars", powertrain = "EV",
                     year = 2025L, unit = "percent", value = c(10, 20, 5), stringsAsFactors = FALSE)
  g <- dn_gevo_prepare(gevo, "EV sales share", to_iso3 = function(x) c(Korea = "KOR", World = NA)[x])
  expect_equal(g$iso3, "KOR")
  expect_equal(g$value, 10)
  ei <- data.frame(Country = c("Japan", "Total World"), Year = 2025L, ISO3166_alpha3 = c("JPN", ""),
                   Var = "oil_tes_ej", Value = c(6.5, 190))
  o <- dn_ei_series(ei, "oil_tes_ej")
  expect_equal(o$iso3, "JPN")
})

test_that("the measure attribution in the config covers the eleven technologies with defensible roles", {
  cfg <- dnt_cfg()
  att <- lapply(cfg$measures, dn_measure_techs)
  # every technology has at least one attributed measure
  expect_true(all(unlist(cfg$techs) %in% unlist(att)))
  # every measure belongs to one of the three families
  expect_true(all(vapply(cfg$measures, function(m) m$family %in% DN_FAMILIES, logical(1))))
  # Oil (which burns oil) and Electric Grid are never credited with displacing imported fuel
  expect_false(any(c("Oil", "Electric Grid") %in% att$import_displacement))
  # coal and gas plants displace imports only net of their own fuel's import dependence
  expect_equal(cfg$measures$import_displacement$techs$Coal$own_fuel, "Coal")
  expect_equal(cfg$measures$import_displacement$techs$Gas$own_fuel, "Gas")
  # no measure reads a subscription source (D13)
  expect_false(any(grepl("bnef|bloomberg", tolower(unlist(cfg)))))
})
