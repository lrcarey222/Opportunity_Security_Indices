repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "scripts", "utils", "bnef_lcoe.R"))
source(file.path(repo_root, "R", "categories", "energy_prices", "lcoe_competitiveness.R"))

# Minimal stand-in for the BNEF "Raw LCOE data" sheet: the columns the theme reads, two
# technologies and enough regions for the s-curve to spread.
bnef_fixture <- function(current_year = 2025L, battery_label = "Utility-scale battery (2h)") {
  regions <- c("Australia", "Brazil", "Canada", "Chile", "India")
  current_col <- paste0("X", current_year)

  base <- tidyr::expand_grid(
    Region = regions,
    Technology = c("PV fixed-axis", battery_label, "Wind offshore")
  )

  out <- tibble::tibble(
    Metric = "LCOE",
    Region = base$Region,
    Technology = base$Technology,
    Scenario = "Mid",
    Unit = "$ per megawatt-hour"
  )
  out[[current_col]] <- seq_len(nrow(out)) * 10
  out$X2050 <- seq_len(nrow(out)) * 5

  # Rows the theme must drop: wrong scenario and a non-LCOE metric.
  noise <- out
  noise$Scenario <- "Low"
  capex <- out
  capex$Metric <- "Capex"

  dplyr::bind_rows(out, noise, capex)
}

test_that("the release's reference year drives the emitted variable names", {
  out_2025 <- lcoe_competitiveness(bnef_fixture(2025L), current_year = 2025L)
  out_2024 <- lcoe_competitiveness(
    bnef_fixture(2024L, battery_label = "Utility-scale battery (1h)"),
    current_year = 2024L
  )

  expect_setequal(unique(out_2025$variable), c("lcoe_25", "lcoe_50", "Overall input cost index"))
  expect_setequal(unique(out_2024$variable), c("lcoe_24", "lcoe_50", "Overall input cost index"))
})

test_that("the current year is taken from the reader's reference_year attribute", {
  fixture <- bnef_fixture(2025L)
  attr(fixture, "reference_year") <- 2025L

  expect_true("lcoe_25" %in% lcoe_competitiveness(fixture)$variable)

  attr(fixture, "reference_year") <- NA_integer_
  expect_error(lcoe_competitiveness(fixture), "current year")
})

test_that("both utility-scale battery durations map to Batteries", {
  years <- c(current = 2025L, horizon = 2050L)

  for (label in c("Utility-scale battery (1h)", "Utility-scale battery (2h)", "Utility-scale battery (4h)")) {
    cleaned <- lcoe_competitiveness_clean_bnef(bnef_fixture(2025L, battery_label = label), years = years)
    expect_true("Batteries" %in% cleaned$Technology, info = label)
  }
})

test_that("only Mid-scenario LCOE rows are scored", {
  years <- c(current = 2025L, horizon = 2050L)
  cleaned <- lcoe_competitiveness_clean_bnef(bnef_fixture(2025L), years = years)

  # 5 regions x 2 mapped technologies (PV fixed-axis -> Solar, battery -> Batteries);
  # Wind offshore is not in the tech list.
  expect_equal(nrow(cleaned), 10)
  expect_setequal(unique(cleaned$Technology), c("Solar", "Batteries"))
})

test_that("indices stay bounded and cheaper LCOE scores higher", {
  out <- lcoe_competitiveness(bnef_fixture(2025L), current_year = 2025L)
  index_values <- out$value[out$data_type == "index"]

  expect_true(all(index_values >= 0 & index_values <= 1, na.rm = TRUE))

  solar <- out %>%
    dplyr::filter(tech == "Solar", variable == "lcoe_25")
  cheapest <- solar$Country[solar$data_type == "raw"][which.min(solar$value[solar$data_type == "raw"])]
  best_scored <- solar$Country[solar$data_type == "index"][which.max(solar$value[solar$data_type == "index"])]

  expect_identical(cheapest, best_scored)
})

test_that("bnef_lcoe_reference_year reads the workbook banner", {
  banner <- tempfile(fileext = ".csv")
  on.exit(unlink(banner), add = TRUE)

  writeLines(
    c(
      ",,,,,Raw LCOE data", "", ",Note", ",note one", ",note two", ",note three", "",
      ",Key cost metrics (2025 real)",
      ",Metric,Region,Technology,Scenario,Unit,2014"
    ),
    banner
  )

  expect_identical(bnef_lcoe_reference_year(banner), 2025L)
})

test_that("a csv input is read directly rather than sent through the converter", {
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  writeLines("x", csv)

  expect_identical(
    bnef_lcoe_resolve_csv(csv, cache_dir = tempdir(), converter = "does-not-exist.ps1"),
    csv
  )
})
