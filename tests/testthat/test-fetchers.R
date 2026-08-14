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

  old <- Sys.getenv("OPSI_REQUIRE_FETCH", unset = NA)
  on.exit(
    if (is.na(old)) Sys.unsetenv("OPSI_REQUIRE_FETCH") else Sys.setenv(OPSI_REQUIRE_FETCH = old),
    add = TRUE
  )
  Sys.unsetenv("OPSI_REQUIRE_FETCH")

  expect_warning(
    outcome <- run_fetcher("probe_exploding", dest, cadence = "per-run", force = TRUE, quiet = TRUE),
    "upstream is down"
  )
  expect_equal(outcome, "failed")
  expect_equal(readLines(dest), before)

  # Opt-in strict mode turns the same failure into a hard error.
  Sys.setenv(OPSI_REQUIRE_FETCH = "true")
  expect_error(
    run_fetcher("probe_exploding", dest, cadence = "per-run", force = TRUE, quiet = TRUE),
    "upstream is down"
  )
})

test_that("run_fetcher skips work when the local copy is within its cadence", {
  opsi_load_fetchers()

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

test_that("registered fetchers cover the manifest entries that declare them", {
  root <- normalizePath(test_path("..", ".."), winslash = "/")
  opsi_load_fetchers()
  source(file.path(root, "scripts", "utils", "raw_inputs.R"), local = FALSE)

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(root))
  ids <- setdiff(list_fetchers(), c("probe_exploding", "probe_counting"))

  expect_true(all(c("wb_wdi", "wb_doingbusiness", "imf_ppi", "imf_lending_rates") %in% ids))

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
