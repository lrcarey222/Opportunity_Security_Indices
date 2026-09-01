# Guards the shape changes that arrived with IEA Global EV Outlook 2026: the extract
# became a CSV, projections moved from 2030 to 2035, a second scenario appeared beside
# Stated Policies, and stock/sales gained an "EV" powertrain total that sums the
# BEV/PHEV/FCEV rows published next to it. Summing blindly over either double counts.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "iea_ev.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "technology_demand", "future_demand.R"))

# Two countries, the three parameters the theme reads, and the years it needs.
ev_fixture <- function() {
  rows <- function(country, parameter, powertrain, category, years, values) {
    data.frame(
      region_country = country,
      category = category,
      parameter = parameter,
      mode = "Cars",
      powertrain = powertrain,
      year = years,
      unit = if (parameter == "EV sales share") "percent" else "Vehicles",
      value = values,
      stringsAsFactors = FALSE
    )
  }

  hist_years <- c(2022L, 2025L)

  do.call(rbind, list(
    # Stock and sales publish components plus an "EV" total that sums them.
    rows("USA", "EV stock", "BEV", "Historical", hist_years, c(2000, 6000)),
    rows("USA", "EV stock", "PHEV", "Historical", hist_years, c(1000, 2000)),
    rows("USA", "EV stock", "EV", "Historical", hist_years, c(3000, 8000)),
    rows("USA", "EV stock", "BEV", "Projection-STEPS", 2035L, 20000),
    rows("USA", "EV stock", "PHEV", "Projection-STEPS", 2035L, 4000),
    rows("USA", "EV stock", "EV", "Projection-STEPS", 2035L, 24000),
    # Current Policies sits beside Stated Policies from the 2026 release.
    rows("USA", "EV stock", "BEV", "Projection-CPS", 2035L, 12000),
    rows("USA", "EV stock", "PHEV", "Projection-CPS", 2035L, 3000),
    rows("USA", "EV sales", "BEV", "Historical", hist_years, c(500, 1500)),
    rows("USA", "EV sales", "EV", "Historical", hist_years, c(500, 1500)),
    rows("USA", "EV sales", "BEV", "Projection-STEPS", 2035L, 4000),
    # Sales share is published only as the "EV" aggregate.
    rows("USA", "EV sales share", "EV", "Historical", hist_years, c(5, 12)),
    rows("USA", "EV sales share", "EV", "Projection-STEPS", 2035L, 30),
    rows("China", "EV stock", "BEV", "Historical", hist_years, c(9000, 30000)),
    rows("China", "EV stock", "PHEV", "Historical", hist_years, c(1000, 10000)),
    rows("China", "EV stock", "EV", "Historical", hist_years, c(10000, 40000)),
    rows("China", "EV stock", "BEV", "Projection-STEPS", 2035L, 200000),
    rows("China", "EV sales", "BEV", "Historical", hist_years, c(3000, 12000)),
    rows("China", "EV sales", "BEV", "Projection-STEPS", 2035L, 25000),
    rows("China", "EV sales share", "EV", "Historical", hist_years, c(25, 50)),
    rows("China", "EV sales share", "EV", "Projection-STEPS", 2035L, 90),
    # Modes other than Cars stay out of the theme.
    rows("USA", "EV stock", "BEV", "Historical", hist_years, c(9e6, 9e6)) |>
      transform(mode = "Buses")
  ))
}

country_fixture <- function() {
  data.frame(
    iso3c = c("USA", "CHN"),
    country = c("United States", "China"),
    stringsAsFactors = FALSE
  )
}

ev_value <- function(tbl, country, variable) {
  hit <- tbl[tbl$Country == country & tbl$variable == variable, ]
  if (nrow(hit) != 1) stop("expected one row for ", country, "/", variable)
  hit$value
}

test_that("the reader accepts the 2026 CSV and the legacy workbook layout", {
  csv_path <- file.path(tempdir(), "EV data by country 2026.csv")
  on.exit(unlink(csv_path), add = TRUE)

  fixture <- ev_fixture()
  fixture[["Aggregate group"]] <- "Other"
  write.csv(fixture, csv_path, row.names = FALSE)

  ev <- read_iea_ev(csv_path)

  expect_true(all(IEA_EV_COLUMNS %in% names(ev)))
  expect_type(ev$year, "integer")
  expect_type(ev$value, "double")
  expect_equal(nrow(ev), nrow(fixture))
})

test_that("the reader rejects a file missing the long-format columns", {
  bad_path <- file.path(tempdir(), "EV data by country 2099.csv")
  on.exit(unlink(bad_path), add = TRUE)

  write.csv(data.frame(region = "USA", value = 1), bad_path, row.names = FALSE)

  expect_error(read_iea_ev(bad_path), "missing columns")
})

test_that("the 'EV' powertrain total is not summed on top of its components", {
  ev_tbl <- future_demand_build_ev(ev_fixture(), country_fixture())

  # BEV 6000 + PHEV 2000, not 8000 more from the "EV" row.
  expect_equal(ev_value(ev_tbl, "United States", "stock_2025"), 8000)
  expect_equal(ev_value(ev_tbl, "United States", "stock_2035"), 24000)
  # Sales share exists only as the aggregate, so it must survive the same filter.
  expect_equal(ev_value(ev_tbl, "United States", "share_2025"), 12)
})

test_that("only the Stated Policies projection reaches the theme", {
  ev_tbl <- future_demand_build_ev(ev_fixture(), country_fixture())

  # STEPS BEV 20000 + PHEV 4000; the CPS rows would add 15000 more.
  expect_equal(ev_value(ev_tbl, "United States", "stock_2035"), 24000)
  expect_equal(ev_value(ev_tbl, "China", "stock_2035"), 200000)
})

test_that("growth runs 2022-2025 and the forecast horizon is 2035", {
  ev_tbl <- future_demand_build_ev(ev_fixture(), country_fixture())

  expect_equal(ev_value(ev_tbl, "United States", "stock_growth_2225"), 8000 / 3000 - 1)
  expect_equal(ev_value(ev_tbl, "United States", "stock_forecast_growth"), 24000 / 8000 - 1)
  expect_equal(unique(ev_tbl$Year), 2035L)
  expect_equal(unique(ev_tbl$source), "IEA Global EV Outlook 2026")
})

test_that("a release without the required years fails loudly", {
  stale <- ev_fixture()
  stale$year[stale$year == 2035L] <- 2030L

  expect_error(
    future_demand_build_ev(stale, country_fixture()),
    "missing year columns: 2035"
  )
})
