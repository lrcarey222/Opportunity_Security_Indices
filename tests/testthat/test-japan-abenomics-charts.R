# Japan Abenomics expansion chart helpers.
#
# The IMF and BoJ pulls are live network calls and are not exercised here. What is
# covered is the handling around them: which WEO years count as projections, how the
# monthly monetary base is averaged, and what the index is allowed to be based on.

opsi_jae_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap or its runner.
opsi_load_jae <- function() {
  path <- file.path(opsi_jae_root(), "R", "charts", "japan_abenomics_expansion.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c("JAE_SERIES", "jae_parse_weo", "jae_annualise_monthly", "jae_rebase", "jae_wide")
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

test_that("WEO parse flags years past each indicator's latest actual as projections", {
  opsi_load_jae()

  raw <- data.frame(
    INDICATOR = c("GGXWDG", "GGXWDG", "NGDP", "NGDP"),
    TIME_PERIOD = c("2024", "2025", "2024", "2025"),
    OBS_VALUE = c("10", "11", "5", ""),
    LATEST_ACTUAL_ANNUAL_DATA = c("2024", "2024", "2025", "2025"),
    PUBLICATION_DATE = "2026-04-14T13:00:00Z",
    stringsAsFactors = FALSE
  )
  out <- jae_parse_weo(raw)

  expect_equal(out$projection, c(FALSE, TRUE, FALSE))  # blank NGDP 2025 dropped
  expect_equal(out$vintage[1], "2026-04-14")
  expect_error(jae_parse_weo(raw[, c("INDICATOR", "OBS_VALUE")]), "missing column")
})

test_that("monthly series are averaged to full calendar years only", {
  opsi_load_jae()

  monthly <- data.frame(
    period = c(201201:201212, 201301:201308),
    value = c(rep(10, 12), rep(20, 8))
  )
  annual <- jae_annualise_monthly(monthly)

  expect_equal(annual$year, 2012L)
  expect_equal(annual$value, 10)
})

test_that("rebase indexes to the base year and refuses a projected base", {
  opsi_load_jae()

  df <- data.frame(year = 2011:2013, value = c(50, 80, 120), projection = FALSE)
  expect_equal(jae_rebase(df, 2012)$index, c(62.5, 100, 150))

  df$projection[2] <- TRUE
  expect_error(jae_rebase(df, 2012), "projection")
  expect_error(jae_rebase(df, 1999), "No base-year")
})

test_that("wide table uses legend labels and blanks projection years per series", {
  opsi_load_jae()

  panel <- data.frame(
    slug = c("gg_gross_debt", "gg_gross_debt", "real_gdp", "real_gdp"),
    kind = "index",
    year = c(2024L, 2025L, 2024L, 2025L),
    index = c(136.54, 140, 108.8, 110.1),
    projection = c(FALSE, TRUE, FALSE, FALSE)
  )
  wide <- jae_wide(panel)

  expect_equal(names(wide), c("year", "Government gross debt", "Real GDP"))
  expect_equal(wide[["Government gross debt"]], c(136.5, NA))
  expect_equal(wide[["Real GDP"]], c(108.8, 110.1))
  expect_equal(nrow(jae_wide(panel, include_projections = TRUE)), 2L)
})
