# Resolve the repo root by walking up to .git rather than assuming getwd() is the root, so
# this file works under both test_dir("tests/testthat") and test_file().
repo_root <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }
  d
})

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
})

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "indices", "index_vintages.R"))

test_that("parse_index_years accepts the CLI and env spellings and falls back to the default", {
  expect_equal(parse_index_years("2020,2025"), c(2020L, 2025L))
  expect_equal(parse_index_years("2025 2020 2020"), c(2020L, 2025L))
  expect_equal(parse_index_years(c(2015, 2020)), c(2015L, 2020L))
  expect_equal(parse_index_years(""), c(2020L, 2025L))
  expect_equal(parse_index_years(NULL, default = 2019L), 2019L)

  expect_error(parse_index_years("not-a-year"), "Could not parse index years")
  expect_error(parse_index_years("1820"), "outside the supported")
})

test_that("resolve_available_year snaps back to the newest year at or before the request", {
  atlas <- 1995:2023

  # The Atlas stops in 2023, so a 2025 vintage takes 2023 rather than failing.
  expect_equal(resolve_available_year(2025, atlas), 2023L)
  expect_equal(resolve_available_year(2020, atlas), 2020L)

  # A request that predates the source falls forward to its first year; the builder
  # records that so the substitution is visible rather than silent.
  expect_equal(resolve_available_year(1990, atlas), 1995L)
})

test_that("vintage_slice_imf_wide drops later periods and reports the year it reached", {
  imf <- data.frame(
    INDICATOR = "Crude Oil",
    X2019 = 1,
    X2019.M01 = 2,
    X2020.Q1 = 3,
    X2020.M06 = 4,
    X2021.M01 = 5,
    check.names = FALSE
  )

  sliced <- vintage_slice_imf_wide(imf, 2020)
  expect_false("X2021.M01" %in% names(sliced))
  expect_true(all(c("INDICATOR", "X2019", "X2019.M01", "X2020.Q1", "X2020.M06") %in% names(sliced)))
  expect_equal(vintage_year_of(sliced), 2020L)

  # A panel that starts after the requested vintage must not be emptied - the builders
  # would fail on a table with no period columns at all.
  late <- data.frame(COUNTRY = "France", X2021.M01 = 1, X2022.M01 = 2, check.names = FALSE)
  late_sliced <- vintage_slice_imf_wide(late, 2020)
  expect_equal(vintage_year_of(late_sliced), 2021L)
  expect_true("X2021.M01" %in% names(late_sliced))
  expect_false("X2022.M01" %in% names(late_sliced))
})

test_that("vintage_slice_annual trims the tail and records the effective year", {
  tbl <- data.frame(Country = "Chile", Year = 2018:2025, Investment = 1:8)

  sliced <- vintage_slice_annual(tbl, 2020)
  expect_equal(max(sliced$Year), 2020)
  expect_equal(nrow(sliced), 3)
  expect_equal(vintage_year_of(sliced), 2020L)

  ilo <- data.frame(ref_area.label = "Chile", time = c(2015, 2019, 2024), obs_value = 1:3)
  expect_equal(nrow(vintage_slice_ilo(ilo, 2020)), 2)
})

test_that("atlas_pad_hs_codes restores the leading zeros Atlas drops", {
  atlas <- tibble::tibble(product_hs92_code = c(10111L, 854140L), export_value = c(1, 2))
  padded <- atlas_pad_hs_codes(atlas, width = 6)

  expect_equal(padded$product_hs92_code, c("010111", "854140"))
  expect_type(padded$product_hs92_code, "character")
})

test_that("index_vintage_comparison reports level and rank movement between the end years", {
  index_by_year <- tibble::tibble(
    index_year = c(2020L, 2025L, 2020L, 2025L),
    Country = c("Chile", "Chile", "Peru", "Peru"),
    tech = "Solar",
    supply_chain = "Upstream",
    Economic_Opportunity_Index = c(0.4, 0.7, 0.6, 0.5)
  )

  out <- index_vintage_comparison(index_by_year, index_col = "Economic_Opportunity_Index")

  chile <- out[out$Country == "Chile", ]
  peru <- out[out$Country == "Peru", ]

  expect_equal(chile$index_change, 0.3)
  expect_equal(peru$index_change, -0.1)

  # Chile overtakes Peru: rank 2 -> 1 is reported as +1 so that, like index_change,
  # positive always means the country gained ground.
  expect_equal(chile$rank_change, 1)
  expect_equal(peru$rank_change, -1)
  expect_equal(unique(out$comparison), "2020_to_2025")
})

test_that("index_vintage_comparison needs at least two vintages", {
  one_year <- tibble::tibble(
    index_year = 2025L,
    Country = "Chile",
    tech = "Solar",
    supply_chain = "Upstream",
    Economic_Opportunity_Index = 0.5
  )

  expect_error(
    index_vintage_comparison(one_year, index_col = "Economic_Opportunity_Index"),
    "at least two vintage years"
  )
})

test_that("weight coverage counts a mixed category as varying and honours what actually scored", {
  weights <- list(
    "Energy Imports" = 4,
    "Reserves" = 4,
    "Production" = 6,
    "Energy Prices" = 2
  )
  themes <- c("import_dependence", "reserves", "production_depth_momentum",
              "critical_minerals_production", "energy_prices")

  full <- index_vintage_weight_coverage(weights, themes)
  expect_equal(full$weight_total, 16)
  # Energy Imports (4) + Production (6, mixed but the EI half moves) + Energy Prices (2).
  expect_equal(full$weight_varying, 12)

  # Energy Prices carries a weight but scores nothing in the current config; excluding it
  # is what the index builder effectively does.
  scored <- index_vintage_weight_coverage(
    weights,
    themes,
    categories_present = c("Energy Imports", "Reserves", "Production")
  )
  expect_equal(scored$weight_total, 14)
  expect_equal(scored$weight_varying, 10)
})

test_that("the theme spec covers every theme the vintage builder feeds to a pillar", {
  spec <- index_vintage_theme_spec()

  energy_security_themes <- c(
    "energy_access_consumption", "solar_pv_potential", "wind_potential", "geothermal_potential",
    "import_dependence", "reserves", "foreign_dependency", "critical_minerals_processing",
    "critical_minerals_production", "critical_minerals_trade", "energy_consumption",
    "trade_concentration", "energy_prices", "investment_momentum"
  )
  economic_opportunity_themes <- c(
    "energy_access_consumption", "solar_pv_potential", "wind_potential", "geothermal_potential",
    "energy_consumption", "energy_prices", "export_feasibility", "future_demand",
    "lcoe_competitiveness", "market_share_manufacturing", "cost_competitiveness",
    "production_depth_momentum", "overcapacity_premium", "technological_readiness",
    "investment_momentum"
  )

  expect_setequal(
    setdiff(union(energy_security_themes, economic_opportunity_themes), spec$theme),
    character(0)
  )
})
