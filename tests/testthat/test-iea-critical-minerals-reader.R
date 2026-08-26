# Guards the contract the theme builders depend on: the IEA publishes this dataset as a
# human-readable workbook, and every builder expects the flattened long frame the older
# hand-made CSV provided.

# Resolved from the test file rather than getwd() so the suite runs from either the repo
# root or tests/testthat.
repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))

fixture_workbook <- function() {
  file.path(repo_root, "tests", "fixtures", "iea_critical_minerals_fixture.xlsx")
}

test_that("the workbook flattens to the canonical long frame", {
  critical <- read_iea_critical_minerals(fixture_workbook(), quiet = TRUE)

  expect_true(all(c("Pillar", "Mineral", "Sector.Country", "Scenario") %in% names(critical)))
  expect_equal(names(iea_critical_minerals_year_cols(critical)), c("X2025", "X2030", "X2035"))
  expect_true(all(vapply(critical[c("X2025", "X2030", "X2035")], is.numeric, logical(1))))

  # One pillar per sheet the index reads; the 4.x technology sheets stay out.
  expect_setequal(
    unique(critical$Pillar),
    c(
      "1 Total demand for key minerals",
      "2 Total supply for key minerals",
      "3.1 Energy demand by tech",
      "3.2 Energy demand by mineral"
    )
  )
})

test_that("demand rows carry their mineral header and the shared base year", {
  critical <- read_iea_critical_minerals(fixture_workbook(), quiet = TRUE)

  by_tech <- critical[critical$Pillar == "3.1 Energy demand by tech", ]
  expect_setequal(unique(by_tech$Mineral), c("Copper", "Lithium"))

  solar <- by_tech[by_tech$Mineral == "Copper" & by_tech$Sector.Country == "Solar PV", ]
  expect_equal(nrow(solar), 1)
  # The base-year column sits left of the scenario blocks and is shared by all of them.
  expect_equal(solar$X2025, 1970)
  expect_equal(solar$X2035, 2164)
})

test_that("the supply sheet's side-by-side mining and refining blocks both parse", {
  critical <- read_iea_critical_minerals(fixture_workbook(), quiet = TRUE)
  supply <- critical[critical$Pillar == "2 Total supply for key minerals", ]

  expect_setequal(unique(supply$Mineral), c("Copper - Mining", "Copper - Refining"))
  expect_equal(unique(supply$Scenario), "Base case")

  refining_china <- supply[supply$Mineral == "Copper - Refining" & supply$Sector.Country == "China", ]
  expect_equal(refining_china$X2025, 13069)
  expect_equal(refining_china$X2035, 17221)
})

test_that("scenario selection keeps Stated Policies and the supply base case", {
  default_read <- read_iea_critical_minerals(fixture_workbook(), quiet = TRUE)
  expect_setequal(unique(default_read$Scenario), c("Stated Policies scenario", "Base case"))

  every_scenario <- read_iea_critical_minerals(fixture_workbook(), scenario = NULL, quiet = TRUE)
  expect_true("Current Policies scenario" %in% every_scenario$Scenario)

  expect_error(
    read_iea_critical_minerals(fixture_workbook(), scenario = "Announced Pledges", quiet = TRUE),
    "No demand rows"
  )
})

test_that("footnote rows below the data are not read as minerals", {
  critical <- read_iea_critical_minerals(fixture_workbook(), scenario = NULL, quiet = TRUE)
  expect_false(any(grepl("^Notes", critical$Mineral)))
  expect_false(any(grepl("^Notes", critical$Sector.Country)))
})

test_that("value columns are addressed by year, not by a pinned name", {
  critical <- read_iea_critical_minerals(fixture_workbook(), quiet = TRUE)

  expect_equal(iea_critical_minerals_base_year(critical), 2025L)
  expect_equal(iea_critical_minerals_year_col(critical, 2035), "X2035")

  # A vintage that no longer publishes the requested year falls back to the nearest one
  # rather than failing on a column name that only differs by release.
  expect_message(
    resolved <- iea_critical_minerals_year_col(critical, 2024),
    "no X2024 column"
  )
  expect_equal(resolved, "X2025")
})

test_that("the flattened frame drives mineral_demand_clean the same way the CSV did", {
  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  })
  source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))

  critical <- read_iea_critical_minerals(fixture_workbook(), quiet = TRUE)
  mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)

  expect_true(all(c("Mineral", "tech", "share_24", "share_35") %in% names(mineral_demand_clean)))

  # "Total <mineral>" and "Other ..." rows are aggregates, so only real sectors survive.
  expect_setequal(
    mineral_demand_clean$Sector.Country,
    c("Solar PV", "Wind", "Electric vehicles", "Grid battery storage")
  )
  expect_setequal(
    unique(mineral_demand_clean$tech),
    c("Solar", "Wind", "Electric Vehicles", "Batteries")
  )

  # Shares are within-mineral, so each mineral's shares sum to one.
  share_totals <- tapply(mineral_demand_clean$share_24, mineral_demand_clean$Mineral, sum)
  expect_equal(as.numeric(share_totals), rep(1, length(share_totals)))
})
