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
