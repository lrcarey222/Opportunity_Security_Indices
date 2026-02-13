repo_root <- getwd()

source(file.path(repo_root, "R", "categories", "energy_prices", "energy_prices.R"))
source(file.path(repo_root, "tests", "testthat", "helper-fixtures.R"))

test_that("energy_prices_imf_clean captures expanded PCPS transition set without oil false positives", {
  imf_monthly_long <- read_fixture_csv("imf_pcps_energy_prices_minimal.csv") |>
    dplyr::mutate(
      date = as.Date(date),
      value = as.numeric(value)
    )

  cleaned <- energy_prices_imf_clean(imf_monthly_long)

  expect_true(all(c("Lithium", "Vanadium", "Iron_Ore", "Diammonium_Phosphate", "Propane") %in% cleaned$clean))

  expect_false(any(cleaned$INDICATOR == "Sunflower Oil, US dollars per metric tonne, Unit prices"))
  expect_false(any(cleaned$INDICATOR == "US dollar, SDR per US dollar, Period average"))

  sunflower_rows <- cleaned |>
    dplyr::filter(stringr::str_detect(INDICATOR, stringr::regex("sunflower oil", ignore_case = TRUE)))
  expect_false(any(stringr::str_detect(sunflower_rows$clean, "^Oil_"), na.rm = TRUE))
})

test_that("energy_prices_build_volatility keeps Electric Grid mapping", {
  imf_monthly_long <- tibble::tibble(
    INDICATOR = rep("Copper, US dollars per metric tonne, Unit prices", 3),
    date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")),
    value = c(100, 110, 120)
  )

  imf_monthly <- energy_prices_imf_clean(imf_monthly_long)

  mineral_demand_clean <- tibble::tibble(
    Mineral = "Copper",
    tech = "Electric Grid"
  )

  out <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = mineral_demand_clean,
    years_back = c(5),
    min_months = 2
  )

  expect_true(any(out$tech == "Electric Grid"))
})

test_that("energy_prices_build_volatility keeps rare earth mappings from mineral demand aliases", {
  imf_monthly_long <- tibble::tibble(
    INDICATOR = rep("Rare Earth Elements, Rare earth carbonate REO 42-45 Dom, Unit prices", 3),
    date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")),
    value = c(50, 55, 53)
  )

  imf_monthly <- energy_prices_imf_clean(imf_monthly_long)

  mineral_demand_clean <- tibble::tibble(
    Mineral = "Rare Earth Elements",
    tech = "Wind"
  )

  out <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = mineral_demand_clean,
    years_back = c(5),
    min_months = 2
  )

  expect_true(any(out$tech == "Wind"))
  expect_true(any(out$sub_sector == "Rare_Earths"))
})

test_that("energy_prices_build_volatility maps rare earths to Wind via PCPS extension map", {
  imf_monthly_long <- tibble::tibble(
    INDICATOR = rep("Rare Earth Elements, Rare earth carbonate REO 42-45 Dom, Unit prices", 3),
    date = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")),
    value = c(50, 55, 53)
  )

  imf_monthly <- energy_prices_imf_clean(imf_monthly_long)

  out <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    years_back = c(5),
    min_months = 2
  )

  expect_true(any(out$tech == "Wind"))
  expect_true(any(out$sub_sector == "Rare_Earths"))
})

test_that("energy_prices_build_table includes latest price and yoy change without affecting overall volatility index", {
  volatility_by_tech <- tibble::tibble(
    tech = "Gas",
    sub_sector = "Natural_Gas_Henry_Hub",
    vol_logret_annualized = 0.2,
    vol_level_sd = 0.1,
    vol_level_cv = 0.05,
    latest_price = 3.4,
    yoy_price_change_pct = 12.5,
    unit = "USD per MMBtu",
    n_series = 1
  )

  tbl <- energy_prices_build_table(
    volatility_by_tech = volatility_by_tech,
    as_of_year = 2024,
    gamma = 0.5
  )

  expect_true(all(c("price_volatility", "latest_price", "yoy_price_change_pct") %in% tbl$variable))
  expect_true(any(tbl$variable == "latest_price" & stringr::str_detect(tbl$explanation, "Unit: USD per MMBtu")))
  expect_true(any(tbl$variable == "latest_price" & stringr::str_detect(tbl$explanation, "Natural_Gas_Henry_Hub")))

  overall_tbl <- energy_prices_add_overall_fallback(tbl)
  overall_rows <- overall_tbl |>
    dplyr::filter(variable == "Overall Energy Prices Index")

  expect_true(nrow(overall_rows) > 0)
  expect_true(all(overall_rows$data_type == "index"))
})

test_that("energy_prices_latest_and_yoy uses year-on-year change in trailing 12-month averages", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "month")
  values <- c(rep(100, 12), rep(110, 12))

  out <- energy_prices_latest_and_yoy(
    tibble::tibble(
      date = dates,
      value = values
    )
  )

  expect_equal(out$latest_price[[1]], 110)
  expect_equal(out$yoy_price_change_pct[[1]], 10)
})

test_that("energy_prices_sub_sector_unit_lookup returns joinable sub_sector to unit_description tibble", {
  imf_price <- tibble::tibble(
    INDICATOR = c(
      "Natural Gas, US Henry Hub, US dollars per MMBtu, Unit prices",
      "Natural Gas, US Henry Hub, US dollars per MMBtu, Unit prices",
      "Coal, US dollars per metric tonne, Unit prices"
    ),
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-01-01")),
    value = c(2.5, 2.7, 150)
  )

  lookup <- energy_prices_sub_sector_unit_lookup(imf_price)

  expect_true(all(c("sub_sector", "unit_description") %in% names(lookup)))
  expect_true(any(lookup$sub_sector == "Natural_Gas_Henry_Hub"))
  expect_true(any(lookup$sub_sector == "Coal"))
  expect_true(any(lookup$unit_description == "USD per MMBtu"))
  expect_true(any(lookup$unit_description == "USD per metric tonne"))
})

test_that("energy_prices_imf_annual_yoy_lookup reads IMF annual yoy transformation", {
  imf_price <- tibble::tibble(
    INDICATOR = c("Natural Gas, US Henry Hub, US dollars per MMBtu, Unit prices"),
    FREQUENCY = "Annual",
    DATA_TRANSFORMATION = "Index, percent change from a year ago",
    X2024 = 8.0,
    X2025 = 10.0
  )

  yoy_lookup <- energy_prices_imf_annual_yoy_lookup(imf_price)

  expect_equal(nrow(yoy_lookup), 1)
  expect_equal(yoy_lookup$clean[[1]], "Natural_Gas_Henry_Hub")
  expect_equal(yoy_lookup$yoy_price_change_pct_annual[[1]], 10)
})

test_that("energy_prices_build_volatility prefers IMF annual yoy and avoids NaN aggregation", {
  imf_monthly <- tibble::tibble(
    INDICATOR = rep("Natural Gas, US Henry Hub, US dollars per MMBtu, Unit prices", 3),
    clean = rep("Natural_Gas_Henry_Hub", 3),
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    value = c(1, 1.1, 1.2)
  )

  out <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    years_back = c(5),
    min_months = 24,
    annual_yoy_lookup = tibble::tibble(
      INDICATOR = "Natural Gas, US Henry Hub, US dollars per MMBtu, Unit prices",
      clean = "Natural_Gas_Henry_Hub",
      yoy_price_change_pct_annual = 7.5
    )
  )

  expect_true(any(out$sub_sector == "Natural_Gas_Henry_Hub"))
  expect_equal(out$yoy_price_change_pct[out$sub_sector == "Natural_Gas_Henry_Hub"][[1]], 7.5)
  expect_false(any(is.nan(out$yoy_price_change_pct)))
})

pcps_gas_indicator <- "Natural Gas, EU, US dollars per million metric British thermal units of gas, Unit prices"

build_pcps_wide_monthly_and_yoy <- function(include_yoy = TRUE) {
  monthly_row <- tibble::tibble(
    INDICATOR = pcps_gas_indicator,
    FREQUENCY = "Monthly",
    DATA_TRANSFORMATION = "US dollars",
    X2024.M12 = 13.757,
    X2025.M12 = 9.460
  )

  if (!include_yoy) {
    return(monthly_row)
  }

  annual_row <- tibble::tibble(
    INDICATOR = pcps_gas_indicator,
    FREQUENCY = "Annual",
    DATA_TRANSFORMATION = "Index, percent change from a year ago",
    X2025 = 10.0
  )

  dplyr::bind_rows(monthly_row, annual_row)
}

test_that("energy_prices latest_price uses latest monthly level", {
  imf_price <- build_pcps_wide_monthly_and_yoy(include_yoy = TRUE)

  out <- energy_prices(
    imf_price,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    years_back = c(5),
    min_months = 2
  )

  latest_row <- out |>
    dplyr::filter(variable == "latest_price", sub_sector == "Natural_Gas_EU", data_type == "raw")

  expect_equal(nrow(latest_row), 1)
  expect_equal(latest_row$value[[1]], 9.460, tolerance = 1e-8)
  expect_equal(latest_row$Year[[1]], 2025)
})

test_that("energy_prices yoy_price_change_pct uses IMF annual YoY value", {
  imf_price <- build_pcps_wide_monthly_and_yoy(include_yoy = TRUE)

  out <- energy_prices(
    imf_price,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    years_back = c(5),
    min_months = 2
  )

  yoy_row <- out |>
    dplyr::filter(variable == "yoy_price_change_pct", sub_sector == "Natural_Gas_EU", data_type == "raw")

  expect_equal(nrow(yoy_row), 1)
  expect_equal(yoy_row$value[[1]], 10.0, tolerance = 1e-8)
})

test_that("adding annual YoY rows does not affect price_volatility or price_volatility_index", {
  imf_price_monthly_only <- build_pcps_wide_monthly_and_yoy(include_yoy = FALSE)
  imf_price_with_yoy <- build_pcps_wide_monthly_and_yoy(include_yoy = TRUE)

  out_monthly_only <- energy_prices(
    imf_price_monthly_only,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    years_back = c(5),
    min_months = 2
  )

  out_with_yoy <- energy_prices(
    imf_price_with_yoy,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    years_back = c(5),
    min_months = 2
  )

  vol_monthly_only <- out_monthly_only |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "price_volatility", data_type == "raw") |>
    dplyr::pull(value)
  vol_with_yoy <- out_with_yoy |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "price_volatility", data_type == "raw") |>
    dplyr::pull(value)

  vol_index_monthly_only <- out_monthly_only |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "price_volatility", data_type == "index") |>
    dplyr::pull(value)
  vol_index_with_yoy <- out_with_yoy |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "price_volatility", data_type == "index") |>
    dplyr::pull(value)

  expect_equal(vol_monthly_only, vol_with_yoy)
  expect_equal(vol_index_monthly_only, vol_index_with_yoy)
})

test_that("Overall Energy Prices Index remains index-only fallback from price_volatility index", {
  imf_price <- build_pcps_wide_monthly_and_yoy(include_yoy = TRUE)

  out <- energy_prices(
    imf_price,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    years_back = c(5),
    min_months = 2
  )

  overall_rows <- out |>
    dplyr::filter(variable == "Overall Energy Prices Index")

  expect_true(nrow(overall_rows) > 0)
  expect_true(all(overall_rows$data_type == "index"))
  expect_false(any(stringr::str_detect(overall_rows$source, "latest_price|yoy_price_change_pct"), na.rm = TRUE))
})
