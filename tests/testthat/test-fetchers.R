# Fetcher framework behaviour. These run offline: the contract, pivot and period
# helpers are pure, and the network paths are exercised only through injected stubs.
# The guarantee under test is that a bad fetch never replaces a good local file.

opsi_load_fetchers <- function() {
  root <- normalizePath(test_path("..", ".."), winslash = "/")
  source(file.path(root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source_fetcher_files(root)
}

opsi_fetch_tmpdir <- function() {
  path <- file.path(tempdir(), paste0("opsi-fetch-", basename(tempfile(""))))
  dir.create(path, recursive = TRUE)
  path
}

# These tests must behave the same whether or not the caller has SKIP_DATA_DOWNLOADS
# set (CI does). withr ships with testthat, so it is always available here.
opsi_fetching_enabled <- function(.local_envir = parent.frame()) {
  withr::local_envvar(
    c(OPSI_SKIP_FETCH = NA, SKIP_DATA_DOWNLOADS = NA, OPSI_REQUIRE_FETCH = NA),
    .local_envir = .local_envir
  )
}

test_that("period normalisation produces the spellings the parsers expect", {
  opsi_load_fetchers()

  expect_equal(opsi_normalize_period("2024"), "2024")
  expect_equal(opsi_normalize_period("2024-Q1"), "2024-Q1")
  expect_equal(opsi_normalize_period("2024Q3"), "2024-Q3")
  # SDMX emits ISO monthly; the exports use the M-form.
  expect_equal(opsi_normalize_period("2024-01"), "2024-M01")
  expect_equal(opsi_normalize_period("2024-M1"), "2024-M01")
  expect_equal(opsi_normalize_period("2024-M12"), "2024-M12")

  # read.csv turns these into X2024.M01, which cost_competitiveness.R matches on.
  cols <- opsi_normalize_period(c("2024-01", "2024-Q1", "2024"))
  mangled <- names(read.csv(text = paste0(paste(cols, collapse = ","), "\n1,2,3"), check.names = TRUE))
  expect_true(all(grepl("^X\\d{4}(\\.[MQ]\\d+)?$", mangled)))
})

test_that("periods interleave quarters with their months, as the staged exports do", {
  opsi_load_fetchers()

  ordered <- opsi_order_periods(c("2025-M02", "2024-Q2", "2024", "2024-M01", "2025"))
  expect_equal(ordered, c("2024", "2024-M01", "2024-Q2", "2025", "2025-M02"))

  # Full-year layout: annual, then each quarter followed by its three months.
  year <- c("2021", paste0("2021-Q", 1:4), sprintf("2021-M%02d", 1:12))
  expect_equal(
    opsi_order_periods(sample(year)),
    c("2021",
      "2021-Q1", "2021-M01", "2021-M02", "2021-M03",
      "2021-Q2", "2021-M04", "2021-M05", "2021-M06",
      "2021-Q3", "2021-M07", "2021-M08", "2021-M09",
      "2021-Q4", "2021-M10", "2021-M11", "2021-M12")
  )
})

test_that("wide pivot keeps one row per key and fills the right cells", {
  opsi_load_fetchers()

  long <- data.frame(
    country = c("USA", "USA", "BRA"),
    period = c("2024-M01", "2024-M02", "2024-M01"),
    value = c(1.5, 2.5, 9.0),
    stringsAsFactors = FALSE
  )
  wide <- opsi_pivot_periods_wide(long, id_cols = "country", period_order = c("2024-M01", "2024-M02"))

  expect_equal(nrow(wide), 2)
  expect_equal(names(wide), c("country", "2024-M01", "2024-M02"))
  expect_equal(wide$`2024-M01`[wide$country == "USA"], 1.5)
  expect_equal(wide$`2024-M02`[wide$country == "USA"], 2.5)
  expect_true(is.na(wide$`2024-M02`[wide$country == "BRA"]))
})

test_that("contract rejects missing columns, thin results and duplicate keys", {
  opsi_load_fetchers()

  contract <- list(
    required_columns = c("COUNTRY", "INDICATOR"),
    min_rows = 2,
    unique_key = c("COUNTRY", "INDICATOR"),
    time_column_pattern = "^\\d{4}$",
    min_time_columns = 1
  )

  good <- data.frame(
    COUNTRY = c("USA", "BRA"), INDICATOR = c("PPI", "PPI"),
    `2024` = c(1, 2), check.names = FALSE, stringsAsFactors = FALSE
  )
  expect_null(opsi_validate_contract(good, contract, "probe"))

  missing_col <- good[, c("COUNTRY", "2024")]
  expect_match(opsi_validate_contract(missing_col, contract, "probe"), "missing column")

  too_few <- good[1, , drop = FALSE]
  expect_match(opsi_validate_contract(too_few, contract, "probe"), "expected at least 2")

  # The failure mode that would corrupt a downstream join.
  dupes <- rbind(good, good[1, , drop = FALSE])
  expect_match(opsi_validate_contract(dupes, contract, "probe"), "duplicate row")

  no_time <- good[, c("COUNTRY", "INDICATOR")]
  expect_match(opsi_validate_contract(no_time, contract, "probe"), "time column")
})

test_that("a contract-violating fetch never overwrites an existing good file", {
  opsi_load_fetchers()

  dir <- opsi_fetch_tmpdir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  dest <- file.path(dir, "good.csv")
  writeLines(c("COUNTRY,INDICATOR,2024", "USA,PPI,1"), dest)
  before <- readLines(dest)

  contract <- list(
    required_columns = c("COUNTRY", "INDICATOR"), min_rows = 5,
    time_column_pattern = "^\\d{4}$", min_time_columns = 1
  )
  garbage <- data.frame(COUNTRY = "USA", INDICATOR = "PPI", `2024` = 1,
                        check.names = FALSE, stringsAsFactors = FALSE)

  expect_error(opsi_write_validated(garbage, dest, contract, "probe"), "expected at least 5")
  expect_equal(readLines(dest), before)
  expect_false(file.exists(paste0(dest, ".tmp-fetch")))
})

test_that("run_fetcher keeps the local file and warns when the fetch errors", {
  opsi_load_fetchers()
  opsi_fetching_enabled()

  dir <- opsi_fetch_tmpdir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  dest <- file.path(dir, "keepme.csv")
  writeLines(c("COUNTRY,INDICATOR,2024", "USA,PPI,1"), dest)
  before <- readLines(dest)

  register_fetcher(
    id = "probe_exploding",
    fn = function() stop("upstream is down"),
    contract = list(required_columns = "COUNTRY", min_rows = 1)
  )

  expect_warning(
    outcome <- run_fetcher("probe_exploding", dest, cadence = "per-run", force = TRUE, quiet = TRUE),
    "upstream is down"
  )
  expect_equal(outcome, "failed")
  expect_equal(readLines(dest), before)

  # Opt-in strict mode turns the same failure into a hard error.
  withr::with_envvar(
    c(OPSI_REQUIRE_FETCH = "true"),
    expect_error(
      run_fetcher("probe_exploding", dest, cadence = "per-run", force = TRUE, quiet = TRUE),
      "upstream is down"
    )
  )
})

test_that("SKIP_DATA_DOWNLOADS disables fetchers, not just OPSI_SKIP_FETCH", {
  opsi_load_fetchers()
  opsi_fetching_enabled()

  expect_true(opsi_fetch_enabled())

  # CI sets SKIP_DATA_DOWNLOADS; fetchers must not reach the network there.
  withr::with_envvar(c(SKIP_DATA_DOWNLOADS = "1"), expect_false(opsi_fetch_enabled()))
  withr::with_envvar(c(OPSI_SKIP_FETCH = "true"), expect_false(opsi_fetch_enabled()))
})

test_that("run_fetcher does no work while fetching is disabled", {
  opsi_load_fetchers()
  opsi_fetching_enabled()

  dir <- opsi_fetch_tmpdir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  withr::local_envvar(c(SKIP_DATA_DOWNLOADS = "1"))

  touched <- new.env(parent = emptyenv())
  touched$n <- 0L
  register_fetcher(
    id = "probe_network",
    fn = function() {
      touched$n <- touched$n + 1L
      data.frame(COUNTRY = "USA", stringsAsFactors = FALSE)
    },
    contract = list(required_columns = "COUNTRY", min_rows = 1)
  )

  expect_equal(
    run_fetcher("probe_network", file.path(dir, "none.csv"), cadence = "per-run", force = TRUE, quiet = TRUE),
    "skipped"
  )
  expect_equal(touched$n, 0L)
})

test_that("run_fetcher skips work when the local copy is within its cadence", {
  opsi_load_fetchers()
  opsi_fetching_enabled()

  dir <- opsi_fetch_tmpdir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  dest <- file.path(dir, "fresh.csv")
  writeLines("COUNTRY\nUSA", dest)

  called <- new.env(parent = emptyenv())
  called$n <- 0L
  register_fetcher(
    id = "probe_counting",
    fn = function() {
      called$n <- called$n + 1L
      data.frame(COUNTRY = "USA", stringsAsFactors = FALSE)
    },
    contract = list(required_columns = "COUNTRY", min_rows = 1)
  )

  expect_equal(run_fetcher("probe_counting", dest, cadence = "annual", quiet = TRUE), "fresh")
  expect_equal(called$n, 0L)

  expect_equal(run_fetcher("probe_counting", dest, cadence = "annual", force = TRUE, quiet = TRUE), "fetched")
  expect_equal(called$n, 1L)
})

test_that("PCPS indicator codes are composed into the Data Explorer label form", {
  root <- normalizePath(test_path("..", ".."), winslash = "/")
  withr::local_options(opportunity_security.repo_root = root)
  opsi_load_fetchers()

  # The API's short name carries no unit; energy_prices_extract_unit() and the Aluminum
  # pattern both read the unit out of the label, so the fetcher has to restore it.
  expect_equal(
    imf_pcps_compose_indicator("PALUM: Aluminum"),
    "Aluminum, US dollars per metric tonne, Unit prices"
  )

  # "Coal" is the coal index, not the Australian coal price - the codes disambiguate.
  expect_equal(
    imf_pcps_compose_indicator("PCOAL: Coal"),
    "Coal index, Commodity price index, Index, 2016=100"
  )
  expect_equal(
    imf_pcps_compose_indicator("PCOALAU: Coal, Australia"),
    "Coal, Australia, US dollars per metric tonne, Unit prices"
  )

  # An unmapped code degrades to its plain label rather than failing the fetch.
  expect_equal(imf_pcps_compose_indicator("PNEWTHING: Some New Commodity"), "Some New Commodity")

  # Vectorised, because it is applied to a whole column.
  expect_equal(
    imf_pcps_compose_indicator(c("PZINC: Zinc", "PNEWTHING: Some New Commodity")),
    c("Zinc, US dollars per metric tonne, Unit prices", "Some New Commodity")
  )
})

test_that("the PCPS label map covers every commodity the Energy Prices theme scores", {
  root <- normalizePath(test_path("..", ".."), winslash = "/")
  withr::local_options(opportunity_security.repo_root = root)
  opsi_load_fetchers()
  source(file.path(root, "R", "categories", "energy_prices", "energy_prices.R"), local = FALSE)

  labels <- unname(imf_pcps_label_map())

  # Every pattern the theme scores must still find a label in the map, or the swap from
  # the hand-staged export silently drops a commodity.
  scored <- setdiff(
    names(energy_prices_imf_patterns),
    # Index series the theme only reads when include_optional_indices is on.
    c("Energy_Index", "Energy_Transition_Metal_Index", "All_Metals_Index", "Base_Metals_Index")
  )

  unmatched <- Filter(
    function(nm) !any(grepl(energy_prices_imf_patterns[[nm]], tolower(labels))),
    scored
  )

  expect_equal(unmatched, character(0))
})

test_that("registered fetchers cover the manifest entries that declare them", {
  root <- normalizePath(test_path("..", ".."), winslash = "/")
  opsi_load_fetchers()
  source(file.path(root, "scripts", "utils", "raw_inputs.R"), local = FALSE)

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(root))
  ids <- setdiff(list_fetchers(), c("probe_exploding", "probe_counting", "probe_network"))

  expect_true(all(c("wb_wdi", "wb_doingbusiness", "imf_ppi", "imf_lending_rates",
                    "imf_commodity_prices") %in% ids))

  # Every fetcher must correspond to a real input, or it writes a file nothing reads.
  for (id in ids) {
    expect_false(
      is.null(manifest[[id]]),
      info = paste0("fetcher '", id, "' has no matching manifest entry")
    )
  }

  # And every fetcher must declare a usable contract.
  for (id in ids) {
    contract <- get_fetcher(id)$contract
    expect_true(length(contract$required_columns) > 0, info = id)
    expect_true(is.numeric(contract$min_rows) && contract$min_rows > 0, info = id)
  }
})
