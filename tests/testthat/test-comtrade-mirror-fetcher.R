# The Comtrade mirror fetcher (scripts/fetchers/comtrade_mirror_critmin.R): partner codes, the
# record-cap split and the response parser, with the HTTP layer replaced by canned responses.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "scripts", "utils", "fetchers.R"))
source(file.path(repo_root, "scripts", "fetchers", "comtrade_mirror_critmin.R"))

# The fetcher's functions live in the global environment (sourced, not a package), so a stand-in
# replaces the global binding for the rest of the calling test.
override_global <- function(..., .frame = parent.frame()) {
  stand_ins <- list(...)
  originals <- mget(names(stand_ins), envir = globalenv())
  list2env(stand_ins, envir = globalenv())
  withr::defer(list2env(originals, envir = globalenv()), envir = .frame)
}

partner_ref_fixture <- function() {
  data.frame(
    id = c(180L, 729L, 736L, 643L, 830L, 412L, 0L),
    country = c("Dem. Rep. of the Congo", "Sudan", "Sudan (...2011)", "Russian Federation",
                "Channel Islands", "Kosovo", "World"),
    iso_3 = c("COD", "SDN", "SDN", "RUS", "_CI", "_KS", "World"),
    exit_year = c(NA, NA, 2011, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

record <- function(reporter, partner, code, cif, flow = "M") {
  list(
    refYear = 2024L, reporterCode = reporter, reporterISO = "CHN", reporterDesc = "China",
    flowCode = flow, partnerCode = partner, partnerISO = "COD", partnerDesc = "DR Congo",
    partner2Code = 0L, classificationCode = "H6", cmdCode = code, cmdDesc = "desc",
    customsCode = "C00", motCode = 0L, qty = 1, netWgt = 1, cifvalue = cif, fobvalue = NA,
    primaryValue = cif, isReported = TRUE, isAggregate = FALSE
  )
}

test_that("partners map to current Comtrade codes, with the WDI-only codes overridden", {
  codes <- critmin_mirror_partner_codes(c("COD", "SDN", "CHI", "XKX", "VIR"), partner_ref_fixture())

  expect_equal(codes$partner_code, c(180L, 729L, 830L, 412L, NA))
})

test_that("the request asks for every reporter's totals over modes, procedures and consignment", {
  url <- critmin_mirror_url(180L, c("260500", "810520"), 2024)

  expect_match(url, "^https://comtradeapi.un.org/public/v1/preview/C/A/HS\\?")
  expect_match(url, "period=2024&partnerCode=180&partner2Code=0&flowCode=M&customsCode=C00&motCode=0")
  expect_match(url, "cmdCode=260500,810520")
  expect_false(grepl("reporterCode", url))

  # Their own exports: the economies as reporters, World as partner.
  own <- critmin_mirror_url(c(180L, 643L), "TOTAL", 2024, flow = "X")
  expect_match(own, "period=2024&reporterCode=180,643&partnerCode=0&partner2Code=0&flowCode=X&customsCode=C00&motCode=0")
  expect_match(own, "cmdCode=TOTAL")
})

# A stand-in for the preview endpoint: one record per requested (partner, code), answered "full"
# (at the cap) when a call asks for `cap` records or more. A call for `fail_on` partners only
# raises an error (the first `fail_times` times). Own-export calls (flow X, the economies in
# reporterCode) are logged with an "X:" prefix and answered only for the `reporting` economies.
fake_comtrade <- function(cap = 2L, fail_on = integer(), fail_times = Inf, reporting = integer()) {
  env <- new.env()
  env$calls <- character()
  env$failures <- 0L
  env$fn <- function(url, ...) {
    own <- grepl("flowCode=X", url, fixed = TRUE)
    key <- if (own) "reporterCode" else "partnerCode"
    partners <- as.integer(strsplit(sub(paste0(".*", key, "=([^&]*).*"), "\\1", url), ",")[[1]])
    codes <- strsplit(sub(".*cmdCode=([^&]*).*", "\\1", url), ",")[[1]]
    env$calls <- c(env$calls, paste0(if (own) "X:", paste(partners, collapse = ","), "|", paste(codes, collapse = ",")))
    if (!own && all(partners %in% fail_on) && env$failures < fail_times) {
      env$failures <- env$failures + 1L
      stop("HTTP 500")
    }
    if (own) partners <- intersect(partners, reporting)
    grid <- expand.grid(partner = partners, code = codes, stringsAsFactors = FALSE)
    if (nrow(grid) >= cap) return(list(count = cap, data = list()))
    if (nrow(grid) == 0) return(list(count = 0L, data = list()))
    list(count = nrow(grid), data = do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
      if (own) {
        as.data.frame(record(grid$partner[i], 0L, grid$code[i], 100, flow = "X"))
      } else {
        as.data.frame(record(156L, grid$partner[i], grid$code[i], 100))
      }
    })))
  }
  env
}

test_that("a response at the record cap is discarded and split by partner, then by code", {
  api <- fake_comtrade(cap = 2L)
  override_global(opsi_http_get_json_simple = api$fn, CRITMIN_MIRROR_MAX_RECORDS = 2L)

  out <- critmin_mirror_fetch_partner(c(180L, 643L), c("260500", "810520"), 2024, pause_seconds = 0)

  expect_equal(nrow(out), 4)
  expect_setequal(paste(out$partner_code, out$cmd_code), c(
    "180 260500", "180 810520", "643 260500", "643 810520"
  ))
  # Both partners and codes, then one partner with both codes, then one call per code.
  expect_equal(api$calls[1:3], c("180,643|260500,810520", "180|260500,810520", "180|260500"))
  expect_equal(length(api$calls), 7)
})

test_that("the parser keeps Comtrade's values and returns nothing for an empty response", {
  parsed <- critmin_mirror_parse(list(count = 1L, data = as.data.frame(record(156L, 180L, "810520", 3.02e9))))

  expect_equal(parsed$primary_value, 3.02e9)
  expect_equal(parsed$reporter_iso, "CHN")
  expect_true(is.na(parsed$fobvalue))
  expect_null(critmin_mirror_parse(list(count = 0L, data = list())))
  expect_error(critmin_mirror_parse(list(error = "Too many requests")), "Too many requests")
})

test_that("a partner that keeps failing fails the whole fetch rather than leaving a hole", {
  api <- fake_comtrade(cap = 2L, fail_on = 643L)
  override_global(opsi_http_get_json_simple = api$fn, CRITMIN_MIRROR_MAX_RECORDS = 2L)
  partners <- data.frame(iso3c = c("COD", "RUS"), partner_code = c(180L, 643L))

  expect_error(
    critmin_mirror_fetch(partners, "260500", 2024, pause_seconds = 0, retry_pause_seconds = 0, quiet = TRUE),
    "Mirror fetch failed \\(commodity flows\\)"
  )
  # Russia's call was made, then made again by the retry pass.
  expect_equal(sum(api$calls == "643|260500"), 2)
})

test_that("the retry pass recovers a failed call and reuses the cached answers", {
  api <- fake_comtrade(cap = 2L, fail_on = 643L, fail_times = 1L)
  override_global(opsi_http_get_json_simple = api$fn, CRITMIN_MIRROR_MAX_RECORDS = 2L)
  partners <- data.frame(iso3c = c("COD", "RUS"), partner_code = c(180L, 643L))
  cache <- withr::local_tempdir()

  out <- critmin_mirror_fetch(
    partners, "260500", 2024,
    pause_seconds = 0, retry_pause_seconds = 0, cache_dir = cache, quiet = TRUE
  )

  # Commodity flows and totals for both partners; neither reports its own exports.
  expect_setequal(paste(out$partner_code, out$cmd_code), c("180 260500", "643 260500", "180 TOTAL", "643 TOTAL"))
  expect_true(all(out$flow_code == "M"))
  expect_true(all(out$fetched_on == as.character(Sys.Date())))
  # The DR Congo's answer came from the cache on the retry pass, not from a second call.
  expect_equal(sum(api$calls == "180|260500"), 1)
  expect_equal(sum(api$calls == "643|260500"), 2)
})

test_that("the fetch adds the own exports of partners that have since reported", {
  # The DR Congo now reports its exports; Russia does not.
  api <- fake_comtrade(cap = 3L, reporting = 180L)
  override_global(opsi_http_get_json_simple = api$fn, CRITMIN_MIRROR_MAX_RECORDS = 3L)
  partners <- data.frame(iso3c = c("COD", "RUS"), partner_code = c(180L, 643L))

  out <- critmin_mirror_fetch(partners, "260500", 2024, pause_seconds = 0, retry_pause_seconds = 0, quiet = TRUE)

  own <- out[out$flow_code == "X", ]
  expect_setequal(paste(own$reporter_code, own$partner_code, own$cmd_code), c("180 0 260500", "180 0 TOTAL"))
  expect_equal(sum(out$flow_code == "M"), 4)
  expect_true(all(c("X:180,643|260500", "X:180,643|TOTAL") %in% api$calls))
  # One row per (year, flow, reporter, partner, code), as the fetcher contract requires.
  expect_false(any(duplicated(out[, c("ref_year", "flow_code", "reporter_code", "partner_code", "cmd_code")])))
})
