test_that("05 ingest script has no hard-coded Comtrade key literals", {
  ingest_lines <- readLines(test_path("..", "..", "scripts", "05_ingest_sources.R"), warn = FALSE)
  ingest_text <- paste(ingest_lines, collapse = "\n")

  expect_false(grepl("set_primary_comtrade_key\\s*\\(", ingest_text, perl = TRUE))
  expect_false(grepl("[A-Fa-f0-9]{32}", ingest_text, perl = TRUE))
})

test_that("trade wrappers resolve the same comtrade year via shared option", {
  old_opt <- getOption("opportunity_security.comtrade_year")
  on.exit(options(opportunity_security.comtrade_year = old_opt), add = TRUE)

  source(test_path("..", "..", "R", "categories", "trade", "trade_concentration.R"), local = FALSE)
  source(test_path("..", "..", "R", "categories", "trade", "export_feasibility.R"), local = FALSE)

  assign("trade_category_build_trade", function(..., year_comtrade) list(year_comtrade = year_comtrade), envir = .GlobalEnv)
  on.exit(rm("trade_category_build_trade", envir = .GlobalEnv), add = TRUE)

  options(opportunity_security.comtrade_year = 2023)
  dummy_trade <- data.frame(ref_year = c(2022, 2023), stringsAsFactors = FALSE)

  tc <- trade_concentration(
    subcat = data.frame(),
    aec_4_data = data.frame(),
    aec_6_data = data.frame(),
    comtrade_trade = dummy_trade,
    comtrade_total_export = data.frame(),
    country_info = data.frame()
  )
  ef <- export_feasibility(
    aec_4_data = data.frame(),
    aec_6_data = data.frame(),
    subcat = data.frame(),
    country_info = data.frame(),
    gdp_data = data.frame(),
    comtrade_trade = dummy_trade,
    comtrade_total_export = data.frame()
  )

  expect_equal(tc$year_comtrade, ef$year_comtrade)
  expect_equal(tc$year_comtrade, 2023)
})

test_that("timeseries warns when requested end year is missing", {
  source(test_path("..", "..", "R", "charts", "trade_timeseries.R"), local = FALSE)
  source(test_path("..", "..", "scripts", "96_pull_trade_timeseries.R"), local = FALSE)

  repo_root <- normalizePath(test_path("..", ".."), winslash = "/")
  options(
    opportunity_security.repo_root = repo_root,
    opportunity_security.config = list(raw_data_dir = "data/raw")
  )

  assign("comtrade_set_key_from_env", function() invisible("ok"), envir = .GlobalEnv)
  assign(
    "comtrade_fetch_requests",
    function(request_df, ...) {
      list(
        data = data.frame(request_id = 1, ref_year = 2024, trade_flow = "export", stringsAsFactors = FALSE),
        failed = character(),
        no_data = character(),
        request_count = nrow(request_df)
      )
    },
    envir = .GlobalEnv
  )
  on.exit({
    rm("comtrade_set_key_from_env", envir = .GlobalEnv)
    rm("comtrade_fetch_requests", envir = .GlobalEnv)
  }, add = TRUE)

  hs_path <- tempfile(fileext = ".csv")
  write.csv(data.frame(HS6 = "850760", tech = "Batteries", supply_chain = "Midstream"), hs_path, row.names = FALSE)

  expect_warning(
    run_trade_timeseries_pull(
      country = "USA",
      tech = "Batteries",
      supply_chain = "Midstream",
      partners = "World",
      years = 2024:2025,
      flow_direction = "export",
      hs6_catalog_path = hs_path,
      write_output = FALSE,
      show_progress = FALSE
    ),
    "Requested end year"
  )
})
