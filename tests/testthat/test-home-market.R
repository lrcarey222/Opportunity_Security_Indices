# National home-market growth (R/strategic_position/home_market.R; design section 2 "Prize", D13): the
# io_home_market input to industrial_opportunity. Synthetic fixtures; the committed config.

hmt_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path(hmt_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(hmt_root(), "R", "strategic_position", "deployment_need.R"))
source(file.path(hmt_root(), "R", "strategic_position", "home_market.R"))

hmt_cfg <- function() yaml::read_yaml(file.path(hmt_root(), "config", "deployment_need.yml"))

# Capacity series (iso3, key, year, capacity_gw): linear growth from `from` by `step` GW a year.
hmt_cap <- function(iso, key, from, step, years = 2019:2025) {
  data.frame(iso3 = iso, key = key, year = years, capacity_gw = from + step * (years - years[1]), stringsAsFactors = FALSE)
}

hmt_universe <- function() {
  data.frame(iso3 = c("AAA", "BBB", "CCC", "DDD"), Country = c("Aland", "Bland", "Cland", "Dland"), stringsAsFactors = FALSE)
}

hmt_cap_series <- function() {
  rbind(hmt_cap("AAA", "Solar", 10, 5), hmt_cap("BBB", "Solar", 10, 1), hmt_cap("CCC", "Solar", 10, 0.5),
        hmt_cap("AAA", "Nuclear", 30, 0), hmt_cap("BBB", "Nuclear", 20, -1),
        hmt_cap("AAA", "Battery", 0, 1, years = 2020:2025))
}

# WEO single-country region rows for AAA only (dn_weo_national() shape).
hmt_weo <- function() {
  data.frame(iso3 = "AAA", region = "Aland", series = rep(c("gen_solar", "gen_nuclear"), each = 2),
             scenario = rep(c("historical", "stated"), 2), year = rep(c(2024L, 2035L), 2),
             value = c(40, 120, 200, 200), stringsAsFactors = FALSE)
}

hmt_targets <- function() {
  data.frame(iso3 = c("AAA", "CCC"), target_key = "Solar", target_gw = c(100, 20), target_year = 2030L,
             target_kind = "renewable_target", target_source = "test", target_note = "n", stringsAsFactors = FALSE)
}

hmt_build <- function(cfg = hmt_cfg()) {
  cap <- hmt_cap_series()
  installed <- dn_installed_capacity(cap, as.integer(cfg$build_rate_years))
  fleet <- data.frame(iso3 = c("AAA", "BBB", "CCC"), year = 2025L, fleet_gw = 100)
  shortfall <- dn_target_shortfall(hmt_targets(), installed, fleet, cfg$targets$installed_source)
  built <- list(installed = installed, shortfall = shortfall, cap_series = cap)
  gevo_stock <- rbind(data.frame(iso3 = "AAA", category = "Historical", year = 2023:2025, value = c(1e5, 2e5, 4e5)),
                      data.frame(iso3 = "AAA", category = "Projection-STEPS", year = 2035L, value = 2.4e6),
                      data.frame(iso3 = "AAA", category = "Projection-CPS", year = 2035L, value = 1.4e6))
  gevo_sales <- rbind(data.frame(iso3 = "AAA", category = "Historical", year = 2023:2025, value = c(1e5, 1e5, 2e5)),
                      data.frame(iso3 = "BBB", category = "Historical", year = 2024:2025, value = c(5e4, 6e4)))
  oil <- rbind(data.frame(iso3 = "AAA", year = 2020:2025, value = 2 + 0.1 * (0:5)),
               data.frame(iso3 = "BBB", year = 2020:2025, value = 1))
  inp <- list(weo_national = hmt_weo(), gevo_stock = gevo_stock, gevo_sales = gevo_sales, oil = oil)
  m <- hm_build_measures(inp, built, cfg)
  list(measures = m, tbl = hm_assemble(m, hmt_universe(), cfg), built = built, cfg = cfg)
}

test_that("home market is one row per country and technology with the engine's column names", {
  r <- hmt_build()
  out <- r$tbl
  expect_equal(nrow(out), nrow(hmt_universe()) * length(unlist(r$cfg$techs)))
  expect_true(all(c("iso3", "tech", "io_home_market", "io_home_market_basis") %in% names(out)))
  v <- out$io_home_market[!is.na(out$io_home_market)]
  expect_true(length(v) > 0 && all(v >= 0 & v <= 1))
  expect_equal(out$io_home_market, out$home_market)
  # national evidence only: the basis is national wherever there is a value
  expect_true(all(out$io_home_market_basis[!is.na(out$io_home_market)] == "national"))
  expect_true(all(out$io_home_market_basis[is.na(out$io_home_market)] == "not_assessed"))
})

test_that("each kind is computed from its own evidence: scenario, target and observed trend", {
  r <- hmt_build()
  m <- r$measures
  # observed trend: mean annual capacity additions over trend_years
  tr <- m[m$kind == "observed_trend" & m$tech == "Solar", ]
  expect_equal(tr$additions[tr$iso3 == "AAA"], 5)
  expect_equal(tr$additions[tr$iso3 == "CCC"], 0.5)
  expect_equal(tr$scenario[1], "observed")
  # target: the additions a year the target requires from today's installed capacity
  tg <- m[m$kind == "target" & m$tech == "Solar", ]
  expect_equal(tg$additions[tg$iso3 == "AAA"], (100 - 40) / 5)
  expect_equal(tg$additions[tg$iso3 == "CCC"], (20 - 13) / 5)
  expect_equal(tg$scenario[1], "national_commitments")
  # scenario: WEO Stated Policies generation growth applied to installed capacity
  sc <- m[m$kind == "scenario" & m$tech == "Solar", ]
  expect_equal(sc$iso3, "AAA")
  expect_equal(sc$additions, (40 * 120 / 40 - 40) / (2035 - 2025))
  expect_equal(sc$scenario, "stated_policies")
  # EV: GEVO Stated Policies stock, not Current Policies
  ev <- m[m$kind == "scenario" & m$tech == "Electric Vehicles", ]
  expect_equal(ev$additions, (2.4e6 - 4e5) / 10)
  # a shrinking fleet is negative additions (the smallest home market), not missing
  nu <- m[m$kind == "observed_trend" & m$tech == "Nuclear", ]
  expect_equal(nu$additions[nu$iso3 == "BBB"], -1)
  # a short EV sales history (fewer years than ev_sales_years) gives no trend value
  evs <- m[m$kind == "observed_trend" & m$tech == "Electric Vehicles", ]
  expect_equal(evs$iso3, "AAA")
})

test_that("home market is the mean of the observed kinds' percentiles and missing evidence stays NA", {
  out <- hmt_build()$tbl
  k <- as.matrix(out[, c("hm_scenario_pct", "hm_target_pct", "hm_observed_trend_pct")])
  n <- unname(rowSums(!is.na(k)))
  expect_equal(out$hm_kinds_observed, as.integer(n))
  expect_equal(out$home_market, ifelse(n > 0, unname(rowSums(k, na.rm = TRUE)) / pmax(n, 1), NA_real_))
  aaa <- out[out$iso3 == "AAA" & out$tech == "Solar", ]
  expect_equal(aaa$hm_kinds_observed, 3L)
  expect_equal(aaa$hm_primary_kind, "scenario")
  # a country with no evidence, and technologies with none anywhere, are NA (never zero)
  expect_true(all(is.na(out$io_home_market[out$iso3 == "DDD"])))
  expect_true(all(is.na(out$io_home_market[out$tech %in% c("Electric Grid", "Green Hydrogen", "Geothermal")])))
  expect_true(all(out$hm_kinds_observed[out$iso3 == "DDD"] == 0))
})

test_that("regional WEO values never enter the home market", {
  weo_values <- data.frame(region = c("Aland", "Bigregion"), series = "gen_solar", scenario = "stated", year = 2035L, value = c(1, 2))
  weo_geo <- data.frame(iso3 = c("AAA", "BBB", "CCC"), region = c("Aland", "Bigregion", "Bigregion"),
                        match = c("own", "member", "by_subregion"))
  nat <- dn_weo_national(weo_values, weo_geo)
  expect_equal(unique(nat$iso3), "AAA")
  m <- hmt_build()$measures
  expect_false(any(m$kind == "scenario" & m$iso3 %in% c("BBB", "CCC")))
})

test_that("no subscription (BNEF) value enters the home market", {
  m <- hmt_build()$measures
  expect_false(any(grepl("bnef|bloomberg", tolower(m$source))))
  src <- readLines(file.path(hmt_root(), "R", "strategic_position", "home_market.R"))
  code <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("bnef|bloomberg", tolower(code))))
})
