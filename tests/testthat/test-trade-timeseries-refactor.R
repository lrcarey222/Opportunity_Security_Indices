test_that("parse_years_arg parses range string", {
  source(test_path("..", "..", "scripts", "96_pull_trade_timeseries.R"), local = FALSE)
  expect_equal(parse_years_arg("2024:2026"), 2024:2026)
})

test_that("parse_years_arg parses comma-separated string", {
  source(test_path("..", "..", "scripts", "96_pull_trade_timeseries.R"), local = FALSE)
  expect_equal(parse_years_arg("2024,2025,2026"), 2024:2026)
})

test_that("monthly chunking uses single-year windows", {
  source(test_path("..", "..", "R", "charts", "trade_timeseries.R"), local = FALSE)
  windows <- trade_chunk_years(2024:2026, year_chunk_size = 1)
  expect_equal(nrow(windows), 3)
  expect_true(all(windows$ys == windows$ye))
})

test_that("annual chunking creates valid non-overlapping windows", {
  source(test_path("..", "..", "R", "charts", "trade_timeseries.R"), local = FALSE)
  windows <- trade_chunk_years(2020:2026, year_chunk_size = 3)
  expect_true(all(windows$ys <= windows$ye))
  expect_equal(windows$ys, c(2020, 2023, 2026))
  expect_equal(windows$ye, c(2022, 2025, 2026))
})

test_that("validate_trade_timeseries_requests rejects monthly multi-year windows", {
  source(test_path("..", "..", "R", "charts", "trade_timeseries.R"), local = FALSE)

  req <- data.frame(
    request_id = 1,
    reporter = "VNM",
    partner = "USA",
    commodity_code = "850760",
    start_date = 2024,
    end_date = 2025,
    flow_direction = "export",
    frequency = "M",
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_trade_timeseries_requests(req),
    "Monthly requests must have start_date == end_date"
  )
})

test_that("request grid never produces ys > ye", {
  source(test_path("..", "..", "R", "charts", "trade_timeseries.R"), local = FALSE)

  catalog <- data.frame(HS6 = "850760", tech = "Solar", supply_chain = "Midstream", stringsAsFactors = FALSE)
  grid <- build_trade_timeseries_request_grid(
    country = "VNM",
    tech = "Solar",
    supply_chain = "Midstream",
    years = 2024:2026,
    hs6_catalog = catalog,
    partner = "USA",
    flow_direction = "export",
    year_chunk_size = 2
  )

  expect_true(all(grid$ys <= grid$ye))
})

test_that("R/charts timeseries script is a compatibility shim", {
  script_text <- paste(
    readLines(test_path("..", "..", "R", "charts", "96_pull_trade_timeseries.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(script_text, "scripts/96_pull_trade_timeseries\\.R")
})
