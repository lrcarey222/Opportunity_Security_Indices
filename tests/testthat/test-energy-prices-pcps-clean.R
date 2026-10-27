repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

# energy_prices() relies on helpers that scripts/10_build_themes.R sources ahead of it:
# median_scurve(), energy_security_add_overall_index() and, through it,
# apply_overall_definitions().
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
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
    window_years = 5,
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

  expect_true(all(c("price_volatility_5yr", "mean_price_volatility", "latest_price", "yoy_price_change_pct") %in% tbl$variable))
  expect_true(any(tbl$variable == "latest_price" & stringr::str_detect(tbl$explanation, "Unit: USD per MMBtu")))
  expect_true(any(tbl$variable == "latest_price" & stringr::str_detect(tbl$explanation, "Natural_Gas_Henry_Hub")))
  # No stage column in the input: the series name gives it (a gas hub price is Extraction).
  expect_true(all(tbl$supply_chain == "Extraction"))

  overall_rows <- tbl |>
    dplyr::filter(variable == "Overall Price Volatility Index")

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



test_that("monthly parsers distinguish generic monthly_long vs monthly_usd_long", {
  imf_price <- tibble::tibble(
    INDICATOR = c(
      "Natural Gas, EU, US dollars per million metric British thermal units of gas, Unit prices",
      "Natural Gas, EU, index row"
    ),
    FREQUENCY = c("Monthly", "Monthly"),
    DATA_TRANSFORMATION = c("US dollars", "Index"),
    X2025.M12 = c(9.460, 105)
  )

  monthly_all <- energy_prices_imf_monthly_long(imf_price)
  monthly_usd <- energy_prices_imf_monthly_usd_long(imf_price)

  expect_equal(nrow(monthly_all), 2)
  expect_equal(nrow(monthly_usd), 1)
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
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "price_volatility_5yr", data_type == "raw") |>
    dplyr::pull(value)
  vol_with_yoy <- out_with_yoy |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "price_volatility_5yr", data_type == "raw") |>
    dplyr::pull(value)

  mean_vol_monthly_only <- out_monthly_only |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "mean_price_volatility", data_type == "raw") |>
    dplyr::pull(value)
  mean_vol_with_yoy <- out_with_yoy |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", variable == "mean_price_volatility", data_type == "raw") |>
    dplyr::pull(value)

  expect_equal(vol_monthly_only, vol_with_yoy)
  expect_equal(mean_vol_monthly_only, mean_vol_with_yoy)
})

test_that("Overall Price Volatility Index exists per tech", {
  imf_price <- build_pcps_wide_monthly_and_yoy(include_yoy = TRUE)

  out <- energy_prices(
    imf_price,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    years_back = c(5),
    min_months = 2
  )

  overall_rows <- out |>
    dplyr::filter(variable == "Overall Price Volatility Index")

  expect_true(nrow(overall_rows) > 0)
  expect_true(all(overall_rows$data_type == "index"))
  expect_false(any(stringr::str_detect(overall_rows$source, "latest_price|yoy_price_change_pct"), na.rm = TRUE))
})

test_that("energy_prices outputs window-specific volatility variables and unsplit latest/yoy", {
  imf_price <- tibble::tibble(
    INDICATOR = "Natural Gas, EU, US dollars per million metric British thermal units of gas, Unit prices",
    FREQUENCY = "Monthly",
    DATA_TRANSFORMATION = "US dollars",
    X2023.M12 = 12.0,
    X2024.M12 = 13.757,
    X2025.M12 = 9.460
  )

  out <- energy_prices(
    imf_price,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character()),
    country_info = NULL,
    min_months = 2
  )

  expect_true(all(c("price_volatility_1yr", "price_volatility_5yr", "price_volatility_10yr", "price_volatility_20yr", "mean_price_volatility") %in% out$variable))
  expect_equal(sum(out$variable == "latest_price" & out$sub_sector == "Natural_Gas_EU"), 1)
  expect_equal(sum(out$variable == "yoy_price_change_pct" & out$sub_sector == "Natural_Gas_EU"), 1)
})


test_that("1-year window computes non-NA volatility with default min_months", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "month")
  imf_monthly_long <- tibble::tibble(
    INDICATOR = rep("Natural Gas, EU, US dollars per million metric British thermal units of gas, Unit prices", length(dates)),
    date = dates,
    value = seq(8, 12, length.out = length(dates))
  )

  imf_monthly <- energy_prices_imf_clean(imf_monthly_long)

  out <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = tibble::tibble(Mineral = character(), tech = character())
  )

  vol_1y <- out |>
    dplyr::filter(sub_sector == "Natural_Gas_EU", window_years == 1) |>
    dplyr::pull(vol_logret_annualized)

  expect_equal(length(vol_1y), 1)
  expect_true(is.finite(vol_1y[[1]]))
})

# --- price units ------------------------------------------------------------------------

test_that("price units come from the IMF series metadata, not the label text", {
  # The IMF labels cobalt "US dollars per pound" and propane "US cents per gallon"; the values
  # are USD per tonne and USD per gallon. Lithium, vanadium, chromium and rare earths carry
  # no unit in the label at all.
  expect_equal(
    energy_prices_unit(c("Cobalt", "Propane", "Lithium", "Vanadium", "Chromium", "Rare_Earths")),
    c("USD per metric tonne", "USD per gallon", "USD per metric tonne", "USD per metric tonne",
      "USD per metric tonne", "USD per metric tonne")
  )
  expect_equal(energy_prices_unit(c("Natural_Gas_Henry_Hub", "LNG", "Uranium", "Oil_APSP")),
               c("USD per MMBtu", "USD per MMBtu", "USD per pound", "USD per barrel"))
  # Unverified or unknown: flagged, never guessed from the label.
  expect_equal(energy_prices_unit(c("Urea", "Energy_Transition_Metal_Index", "Not_A_Commodity")),
               rep("unit to confirm", 3))
})

test_that("every commodity the patterns can match has a unit unless deliberately held back", {
  units <- energy_prices_pcps_units()
  expect_false(anyDuplicated(units$clean) > 0)
  expect_true(all(units$clean %in% names(energy_prices_imf_patterns)))
  expect_setequal(
    setdiff(names(energy_prices_imf_patterns), units$clean),
    c("Urea", "Energy_Transition_Metal_Index")
  )
})

test_that("the theme states each price in its true unit and the yoy change in percent", {
  months <- sprintf("X2025.M%02d", 1:3)
  wide <- function(indicator, values) {
    row <- tibble::tibble(INDICATOR = indicator, FREQUENCY = "Monthly", DATA_TRANSFORMATION = "US dollars")
    for (i in seq_along(months)) row[[months[[i]]]] <- values[[i]]
    row
  }
  imf_price <- dplyr::bind_rows(
    wide("Cobalt, US dollars per pound, Unit prices", c(48000, 50000, 51873)),
    wide("Propane, US cents per gallon, Unit prices", c(0.70, 0.66, 0.646)),
    wide("Lithium, 99%, Battery Grade, Unit prices", c(90000, 93000, 95257))
  )

  out <- energy_prices(
    imf_price,
    mineral_demand_clean = tibble::tibble(Mineral = c("Cobalt", "Lithium"), tech = "Batteries"),
    country_info = NULL,
    min_months = 2
  )
  explanation_of <- function(sub, var) {
    unique(out$explanation[out$sub_sector == sub & out$variable == var])
  }

  expect_match(explanation_of("Cobalt", "latest_price"), "Unit: USD per metric tonne\\.$")
  expect_match(explanation_of("Propane", "latest_price"), "Unit: USD per gallon\\.$")
  expect_match(explanation_of("Lithium", "latest_price"), "Unit: USD per metric tonne\\.$")
  expect_match(explanation_of("Cobalt", "yoy_price_change_pct"), "Unit: percent \\(price quoted in USD per metric tonne\\)\\.$")
  expect_false(any(grepl("per pound|US cents|Indicator-native", out$explanation[out$sub_sector %in% c("Cobalt", "Propane")])))
})

# --- six-stage supply chain ---------------------------------------------------------------

test_that("every PCPS series the patterns can match is staged Extraction or Refining", {
  stages <- energy_prices_pcps_stages()
  expect_false(anyDuplicated(stages$clean) > 0)
  expect_setequal(stages$clean, names(energy_prices_imf_patterns))
  expect_true(assert_supply_chain_stages(stages$supply_chain, "energy_prices_pcps_stages()"))
  expect_setequal(unique(stages$supply_chain), c("Extraction", "Refining"))

  # Ores, crude, gas hubs (LNG included), coal and U3O8 are Extraction; refined metals,
  # chemicals and the NGL are Refining.
  expect_identical(
    energy_prices_stage(c("Oil_Brent", "Dubai_Crude", "Natural_Gas_EU", "LNG", "Coal", "Iron_Ore", "Uranium")),
    rep("Extraction", 7)
  )
  expect_identical(
    energy_prices_stage(c("Copper", "Nickel", "Lithium", "Cobalt", "Silicon", "Rare_Earths", "Propane", "Diammonium_Phosphate")),
    rep("Refining", 8)
  )
  # A series with no stage stops the build instead of leaving the table.
  expect_error(energy_prices_stage(c("Copper", "Not_A_Commodity")), "Not_A_Commodity")
})

test_that("a tech priced by ores and refined metals gets an Extraction and a Refining cell", {
  months <- c(sprintf("X2024.M%02d", 1:12), sprintf("X2025.M%02d", 1:12))
  wide <- function(indicator, values) {
    row <- tibble::tibble(INDICATOR = indicator, FREQUENCY = "Monthly", DATA_TRANSFORMATION = "US dollars")
    for (i in seq_along(months)) row[[months[[i]]]] <- values[[i]]
    row
  }
  i <- seq_along(months)
  imf_price <- dplyr::bind_rows(
    wide("Copper, US dollars per metric tonne, Unit prices", 9000 + 400 * sin(i)),
    wide("Iron Ore, US dollars per metric tonne, Unit prices", 110 + 25 * cos(i)),
    wide("Natural Gas, US Henry Hub, US dollars per MMBtu, Unit prices", 3 + sin(i / 2)),
    wide("Propane, US cents per gallon, Unit prices", 0.8 + 0.05 * cos(i / 3))
  )

  out <- withr::with_options(
    list(opportunity_security.index_definition = yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))),
    energy_prices(
      imf_price,
      mineral_demand_clean = tibble::tibble(Mineral = "Copper", tech = "Electric Grid"),
      country_info = NULL,
      years_back = c(1, 5),
      min_months = 2
    )
  )

  # Every row carries a stage, none the old Upstream.
  expect_true(assert_supply_chain_stages(out$supply_chain, "energy_prices"))
  stage_of <- function(sub) unique(out$supply_chain[out$sub_sector == sub])
  expect_identical(stage_of("Iron_Ore"), "Extraction")
  expect_identical(stage_of("Copper"), "Refining")
  expect_identical(stage_of("Natural_Gas_Henry_Hub"), "Extraction")
  expect_identical(stage_of("Propane"), "Refining")

  # Electric Grid (iron ore, copper) and Gas (Henry Hub, propane) each split into two cells,
  # and each cell has its own volatility index and category score.
  overall <- out %>%
    dplyr::filter(variable == "Overall Price Volatility Index") %>%
    dplyr::select(tech, supply_chain, value)
  for (t in c("Electric Grid", "Gas")) {
    expect_setequal(overall$supply_chain[overall$tech == t], c("Extraction", "Refining"))
  }
  scored <- out %>% dplyr::filter(variable == "Overall Energy Prices Index", tech %in% c("Electric Grid", "Gas"))
  expect_equal(nrow(dplyr::distinct(scored, tech, supply_chain)), 4)

  # The index is the s-curve of the mean over that cell's series only.
  expected <- out %>%
    dplyr::filter(variable == "mean_price_volatility") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarize(mean_vol = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(expected = median_scurve(-mean_vol, gamma = 0.5))
  joined <- dplyr::inner_join(overall, expected, by = c("tech", "supply_chain"))
  expect_equal(nrow(joined), nrow(overall))
  expect_equal(joined$value, joined$expected)
  expect_true(all(grepl("Extraction series", out$explanation[out$variable == "Overall Price Volatility Index" & out$supply_chain == "Extraction"])))

  # The Extraction cell's mean volatility is the iron ore series alone.
  grid_extraction <- out$value[out$tech == "Electric Grid" & out$supply_chain == "Extraction" & out$variable == "mean_price_volatility"]
  iron_ore <- out$value[out$sub_sector == "Iron_Ore" & out$tech == "Electric Grid" & out$variable == "mean_price_volatility"]
  expect_equal(grid_extraction, iron_ore)
})
