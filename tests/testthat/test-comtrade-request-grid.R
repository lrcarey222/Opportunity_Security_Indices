# The ingest script builds its own Comtrade request grid, separate from the one in
# scripts/96_pull_trade_timeseries.R. When the shared client gained a validated column
# contract, this builder was not updated and every ingest run failed with
# "request_df is missing required columns: request_id, frequency".
# These tests pin the builder to the contract the client actually enforces.

opsi_ingest_path <- function() {
  file.path(normalizePath(test_path("..", ".."), winslash = "/"), "scripts", "05_ingest_sources.R")
}

# Pull a single top-level function out of a script without running its IO.
opsi_extract_fn <- function(path, name) {
  exprs <- parse(path)
  env <- new.env(parent = globalenv())
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 &&
        identical(as.character(e[[1]]), "<-") &&
        identical(as.character(e[[2]]), name)) {
      eval(e, envir = env)
      return(get(name, envir = env))
    }
  }
  stop("could not find ", name, " in ", path)
}

opsi_client_required_cols <- function() {
  client <- readLines(
    file.path(normalizePath(test_path("..", ".."), winslash = "/"), "scripts", "utils", "comtrade_client.R"),
    warn = FALSE
  )
  line <- grep("required_cols\\s*<-", client, value = TRUE)[1]
  expect_false(is.na(line))
  eval(parse(text = sub(".*required_cols\\s*<-\\s*", "", line)))
}

opsi_build_requests <- function() {
  root <- normalizePath(test_path("..", ".."), winslash = "/")
  source(file.path(root, "scripts", "utils", "comtrade_ingest_utils.R"), local = FALSE)
  opsi_extract_fn(opsi_ingest_path(), "build_requests")
}

test_that("the ingest request grid satisfies the shared client's column contract", {
  build_requests <- opsi_build_requests()
  required <- opsi_client_required_cols()

  req <- build_requests(
    reporters = c("USA", "BRA"),
    partners = "World",
    commodity_codes = list("850760"),
    years = 2024,
    flows = c("export", "import"),
    partner_chunk_size = 1
  )

  expect_true(all(required %in% names(req)))
  expect_equal(nrow(req), 4)
  expect_equal(unique(req$frequency), "A")
  expect_equal(anyDuplicated(req$request_id), 0)

  # The year probe passes a single row straight through to the client.
  expect_true(all(required %in% names(req[1, , drop = FALSE])))
})

test_that("multi-year requests pair start and end rather than crossing them", {
  build_requests <- opsi_build_requests()

  years <- 2021:2025
  req <- build_requests(
    reporters = "USA",
    partners = "World",
    commodity_codes = list("850760"),
    years = years,
    flows = "export",
    partner_chunk_size = 1
  )

  # One request per year, not every (start, end) combination.
  expect_equal(nrow(req), length(years))
  expect_equal(sort(unique(req$start_date)), years)
  expect_true(all(req$start_date == req$end_date))
  expect_false(any(req$start_date > req$end_date))
})

test_that("request ids stay unique and stable across chunked runs", {
  build_requests <- opsi_build_requests()
  subset_request_chunk <- opsi_extract_fn(opsi_ingest_path(), "subset_request_chunk")

  req <- build_requests(
    reporters = c("USA", "BRA", "IND", "CHN"),
    partners = "World",
    commodity_codes = list("850760"),
    years = 2023:2024,
    flows = c("export", "import"),
    partner_chunk_size = 1
  )

  chunk_count <- 4L
  chunks <- lapply(seq_len(chunk_count), function(i) subset_request_chunk(req, i, chunk_count))

  # Chunks partition the grid: every request runs exactly once across all chunk runs.
  ids <- sort(unlist(lapply(chunks, function(x) x$request_id)))
  expect_equal(ids, sort(req$request_id))
  expect_equal(anyDuplicated(ids), 0)

  for (chunk in chunks) {
    expect_true(all(opsi_client_required_cols() %in% names(chunk)))
  }
})

test_that("frequency is configurable but defaults to annual", {
  build_requests <- opsi_build_requests()

  args <- list(
    reporters = "USA", partners = "World", commodity_codes = list("TOTAL"),
    years = 2024, flows = "export", partner_chunk_size = 1
  )

  expect_equal(unique(do.call(build_requests, args)$frequency), "A")
  expect_equal(unique(do.call(build_requests, c(args, list(frequency = "M")))$frequency), "M")
})
