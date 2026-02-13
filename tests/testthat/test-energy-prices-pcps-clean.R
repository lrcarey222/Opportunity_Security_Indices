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

  overall_tbl <- energy_prices_add_overall_fallback(tbl)
  overall_rows <- overall_tbl |>
    dplyr::filter(variable == "Overall Energy Prices Index")

  expect_true(nrow(overall_rows) > 0)
  expect_true(all(overall_rows$data_type == "index"))
})
