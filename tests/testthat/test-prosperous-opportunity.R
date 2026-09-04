# testthat runs with the working directory set to tests/testthat, so the repo root is
# resolved from this file rather than from getwd().
repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

test_that("opportunity trade_index is scaled within each reporter country", {
  ds_export <- tibble::tibble(
    reporter_iso = c("USA", "USA", "CHN", "CHN"),
    partner_iso = c("JPN", "DEU", "JPN", "DEU"),
    tech = "Solar",
    supply_chain = "Upstream",
    level_last = c(100, 200, 300, 400),
    level_first = c(80, 120, 250, 500),
    growth = c(0.25, 0.67, 0.2, -0.2)
  )

  trade_indices <- partnership_strength_build_export_indices(ds_export)

  by_reporter <- trade_indices %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::summarize(
      min_trade_index = min(trade_index, na.rm = TRUE),
      max_trade_index = max(trade_index, na.rm = TRUE),
      .groups = "drop"
    )

  expect_equal(by_reporter$min_trade_index, rep(0, nrow(by_reporter)))
  expect_equal(by_reporter$max_trade_index, rep(1, nrow(by_reporter)))
})

test_that("friendshore imp_trade_index is scaled within each reporter country", {
  ds_import <- tibble::tibble(
    reporter_iso = c("USA", "USA", "CHN", "CHN"),
    partner_iso = c("JPN", "DEU", "JPN", "DEU"),
    tech = "Solar",
    supply_chain = "Upstream",
    level_last = c(140, 80, 500, 250),
    level_first = c(100, 100, 300, 400),
    growth = c(0.4, -0.2, 0.67, -0.38)
  )

  import_indices <- partnership_strength_build_import_indices(ds_import)

  by_reporter <- import_indices %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::summarize(
      min_imp_trade_index = min(imp_trade_index, na.rm = TRUE),
      max_imp_trade_index = max(imp_trade_index, na.rm = TRUE),
      .groups = "drop"
    )

  expect_equal(by_reporter$min_imp_trade_index, rep(0, nrow(by_reporter)))
  expect_equal(by_reporter$max_imp_trade_index, rep(1, nrow(by_reporter)))
})

test_that("opportunity assigns tech/supply-chain average trade_index when trade data is missing", {
  trade_indices <- tibble::tibble(
    reporter_iso = "USA",
    partner_iso = "JPN",
    tech = "Solar",
    supply_chain = "Upstream",
    trade_index = 0.8
  )

  econ_opp_index <- tibble::tibble(
    Country = c("United States", "China"),
    tech = "Solar",
    supply_chain = "Upstream",
    Economic_Opportunity_Index = c(0.4, 0.6)
  )

  energy_security_index <- tibble::tibble(
    Country = c("Japan", "Germany"),
    tech = "Solar",
    supply_chain = "Upstream",
    Energy_Security_Index = c(0.3, 0.7)
  )

  tech_ghg <- tibble::tibble(tech = "Solar", ghg_index = 0.5)
  policy <- tibble::tibble(Country = c("Japan", "Germany"), climate_policy_index = c(0.5, 0.5))
  country_info <- tibble::tibble(
    country = c("United States", "China", "Japan", "Germany"),
    iso3c = c("USA", "CHN", "JPN", "DEU")
  )

  out <- partnership_strength_build_opportunity_dyads(
    trade_indices = trade_indices,
    econ_opp_index = econ_opp_index,
    energy_security_index = energy_security_index,
    tech_ghg = tech_ghg,
    policy = policy,
    country_info = country_info
  )

  expect_equal(nrow(out), 4)
  expect_false(any(is.na(out$trade_index)))
})

test_that("friendshore assigns tech/supply-chain average imp_trade_index when trade data is missing", {
  import_indices <- tibble::tibble(
    reporter_iso = "USA",
    partner_iso = "JPN",
    tech = "Solar",
    supply_chain = "Upstream",
    imp_trade_index = 0.7
  )

  econ_opp_index <- tibble::tibble(
    Country = c("United States", "China", "Japan", "Germany"),
    tech = "Solar",
    supply_chain = "Upstream",
    Economic_Opportunity_Index = c(0.4, 0.6, 0.5, 0.55)
  )

  energy_security_index <- tibble::tibble(
    Country = c("United States", "China"),
    tech = "Solar",
    supply_chain = "Upstream",
    Energy_Security_Index = c(0.3, 0.7)
  )

  outbound_edges <- tibble::tibble(
    reporter_iso = character(),
    partner_iso = character(),
    outbound_index = numeric()
  )

  tech_ghg <- tibble::tibble(tech = "Solar", ghg_index = 0.5)
  policy <- tibble::tibble(Country = c("United States", "China", "Japan", "Germany"), climate_policy_index = 0.5)
  country_info <- tibble::tibble(
    country = c("United States", "China", "Japan", "Germany"),
    iso3c = c("USA", "CHN", "JPN", "DEU")
  )

  out <- partnership_strength_build_friendshore_dyads(
    import_indices = import_indices,
    econ_opp_index = econ_opp_index,
    energy_security_index = energy_security_index,
    outbound_edges = outbound_edges,
    tech_ghg = tech_ghg,
    policy = policy,
    country_info = country_info
  )

  expect_equal(nrow(out), 4)
  expect_false(any(is.na(out$imp_trade_index)))
})
