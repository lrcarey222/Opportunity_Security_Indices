# Label helpers (R/utils/schema.R) and the builders that use them.
#
# Guards two labels that leaked into every downstream table:
#   * the EI source label, hard-coded as "(2025)", the data year, while the staged files are
#     the 2026 edition of the Statistical Review;
#   * the technology "Heat.Pumps", read.csv()'s syntactic name for the IEA header
#     "Heat Pumps - Current", which the clean-tech midstream builders pivoted into `tech`.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "categories", "consumption", "energy_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_imports", "import_dependence.R"))
source(file.path(repo_root, "R", "categories", "production", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "foreign_dependency.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "market_share_manufacturing.R"))

withr::local_options(list(opportunity_security.index_definition = NULL))

# --- EI edition --------------------------------------------------------------------------

test_that("the EI label names the edition, one year after the newest data year", {
  expect_equal(ei_stat_review_edition(2025), 2026L)
  expect_equal(ei_stat_review_source(2025), "EI Statistical Review of World Energy (2026 edition)")
  expect_equal(ei_stat_review_source("2025"), ei_stat_review_source(2025L))
  expect_error(ei_stat_review_source(c(2024, 2025)), "one data year")
  expect_error(ei_stat_review_source(NA), "one data year")
})

ei_panel_fixture <- function() {
  countries <- c("US", "Germany", "Japan")
  vars <- c("pop", "coal_tes_ej", "oil_tes_ej", "gas_tes_ej", "solar_tes_ej", "wind_tes_ej",
            "nuclear_tes_ej", "oilprod_kbd", "oilcons_kbd", "gasprod_ej", "coalprod_ej")
  grid <- expand.grid(Country = countries, Year = c(2020, 2025), Var = vars, stringsAsFactors = FALSE)
  grid$Value <- seq_len(nrow(grid)) %% 13 + 1
  tibble::as_tibble(grid)
}

test_that("the EI builders label rows with the edition of the panel they read", {
  ei <- ei_panel_fixture()
  expected <- "EI Statistical Review of World Energy (2026 edition)"

  access <- energy_access_consumption(ei, base_year = 2020, target_year = 2025)
  consumption_level <- energy_consumption_build_ei_indices(energy_consumption_clean_ei(ei, 2025), 2025)
  consumption_growth <- energy_consumption_build_ei_growth(
    energy_consumption_build_ei_indices(energy_consumption_clean_ei(ei, 2020), 2020),
    consumption_level,
    base_year = 2020,
    target_year = 2025
  )
  imports <- import_dependence(ei, year = 2025)

  for (tbl in list(access, consumption_level, consumption_growth, imports)) {
    ei_rows <- tbl$source[tbl$source != "Author calculation"]
    expect_true(length(ei_rows) > 0)
    expect_true(all(ei_rows == expected))
  }
})

test_that("mineral production and fuel reserves take the edition from the panel year", {
  country_reference <- tibble::tibble(
    ISO3166_alpha3 = c("CHL", "AUS"),
    Country = c("Chile", "Australia")
  )
  sheet <- tibble::tibble(Country = c("Chile", "Australia", "Total World"), `2025` = c(60, 90, 150))

  production <- critical_minerals_production_build_table(
    sheet_data = sheet, nm_col = "Country", tech_name = "Lithium", unit_desc = "kt",
    sheet_id = "Lithium P-R", country_reference = country_reference, year = 2025L, val_col = "2025"
  )
  expect_true(all(production$source == "EI Statistical Review of World Energy (2026 edition)"))

  # The reserves sheets of the 2026 workbook hold end-2020 stocks: the label still names the
  # edition read, which follows the panel year, not the sheet's vintage.
  reserves_sheet <- tibble::tibble(Country = c("Chile", "Australia", "Total World"), Total = c(1, 2, 3))
  fuel <- reserves_build_reserve_table(
    sheet_data = reserves_sheet, nm_col = "Country", val_col = "Total", tech_name = "Coal",
    unit_desc = "Mt", sheet_id = "Coal - Reserves", country_reference = country_reference,
    year = 2025, data_year = 2020L
  )
  expect_true(all(fuel$source == "EI Statistical Review of World Energy (2026 edition)"))
  expect_match(unique(fuel$explanation[fuel$data_type == "raw"]), "at end-2020")
})

# --- Heat Pumps --------------------------------------------------------------------------

test_that("CSV header labels get their word separators back", {
  expect_equal(csv_header_label(c("Heat.Pumps", "Solar", "Electrolyzers")), c("Heat Pumps", "Solar", "Electrolyzers"))
  expect_equal(csv_header_label("Heat..Pumps"), "Heat Pumps")
})

# The IEA extract as read.csv() delivers it: "Heat Pumps - Current" becomes "Heat.Pumps...Current".
cleantech_fixture <- function() {
  utils::read.csv(text = paste(
    ",Solar - Current,Solar - Pipeline,Electrolyzers - Current,Electrolyzers - Pipeline,Heat Pumps - Current,Heat Pumps - Pipeline",
    "China,79,80,41,26,36,28",
    "United States,5,6,10,12,9,10",
    "Germany,2,3,8,9,20,21",
    "Other,14,11,41,53,35,41",
    sep = "\n"
  ))
}

cleantech_ei_fixture <- function() {
  tibble::tibble(Country = c("China", "United States", "Germany"), EU = c(0L, 0L, 1L))
}

test_that("the clean-tech midstream builders label heat pumps 'Heat Pumps'", {
  expect_true("Heat.Pumps...Current" %in% names(cleantech_fixture()))

  market_share <- market_share_manufacturing_build_cleantech_midstream(
    ei = cleantech_ei_fixture(),
    cleantech_midstream = cleantech_fixture()
  )
  dependency <- foreign_dependency_build_cleantech_midstream(
    ei = cleantech_ei_fixture(),
    cleantech_midstream = cleantech_fixture()
  )

  for (tbl in list(market_share, dependency)) {
    expect_true("Heat Pumps" %in% tbl$tech)
    expect_false(any(grepl(".", tbl$tech, fixed = TRUE)))
    expect_false(any(grepl("Heat.Pumps", tbl$explanation, fixed = TRUE)))
  }
  # Electrolyser capacity still maps to Green Hydrogen in the market-share theme.
  expect_true("Green Hydrogen" %in% market_share$tech)
})
