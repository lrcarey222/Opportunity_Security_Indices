# Guards the failure this file was written for: critical_minerals_production_specs()
# addressed the EI workbook by sheet *position* and pinned the readxl position suffix on the
# value column ("2024...31"). The 2026 edition inserted two sheets ahead of the mineral
# block, so every spec silently pointed four sheets away from its mineral.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "production", "critical_minerals_production.R"))

ei_workbook <- function() {
  file.path(repo_root, "data", "raw", "ei_stat_review_world_energy_wide.xlsx")
}

# A miniature P-R sheet: production years, then the latest year repeated as growth rate and
# share, then the reserves block. readxl disambiguates the repeats by column position.
pr_sheet_fixture <- function(latest = 2025) {
  cols <- list(
    `Thousand tonnes` = c("Australia", "Chile", "China"),
    `2023` = c(778, 5250, 1820),
    `2024` = c(765, 5510, 1840)
  )
  cols[[as.character(latest)]] <- c(730, 5300, 1800)            # production level
  out <- tibble::as_tibble(cols)
  out[[paste0(latest, "...5")]] <- c(-0.043, -0.035, -0.019)    # growth rate per annum
  out[["2015-25"]] <- c(-0.028, -0.008, 0.005)
  out[[paste0(latest, "...7")]] <- c(0.032, 0.230, 0.078)       # share
  out[["At end of 2025"]] <- c(1e5, 18e4, 41e3)
  # readxl would name the three repeats by position; mirror that here.
  names(out)[names(out) == as.character(latest)] <- paste0(latest, "...4")
  out
}

test_that("the production column is the leftmost of a year's repeated columns", {
  sheet <- pr_sheet_fixture()

  year_cols <- critical_minerals_production_year_columns(sheet)
  expect_equal(year_cols$year, c(2023L, 2024L, 2025L, 2025L, 2025L))

  # 2025 appears three times: production, growth rate, share. Only the first is a level.
  expect_equal(
    critical_minerals_production_resolve_val_col(sheet, year = 2025, sheet_id = "fixture"),
    "2025...4"
  )
  expect_equal(sheet[["2025...4"]], c(730, 5300, 1800))
})

test_that("a year the sheet does not publish falls back to its latest, and says so", {
  sheet <- pr_sheet_fixture()

  expect_message(
    resolved <- critical_minerals_production_resolve_val_col(sheet, year = 2030, sheet_id = "fixture"),
    "no 2030 column; using 2025"
  )
  expect_equal(resolved, "2025...4")

  # "2015-25" is a growth window, not a year column, so it must not be mistaken for one.
  expect_false("2015-25" %in% critical_minerals_production_year_columns(sheet)$column)
})

test_that("a sheet with no year columns fails loudly rather than reading a stray column", {
  expect_error(
    critical_minerals_production_resolve_val_col(
      tibble::tibble(`Thousand tonnes` = "Chile", Share = 0.2),
      sheet_id = "fixture"
    ),
    "no year columns"
  )
})

test_that("the theme year is the newest production year across the mineral sheets", {
  inputs <- list(
    list(data = pr_sheet_fixture(2025)),
    list(data = pr_sheet_fixture(2024))
  )
  expect_equal(critical_minerals_production_latest_year(inputs), 2025L)
})

test_that("every spec names a sheet the EI workbook actually has, with a usable value column", {
  skip_if_not(file.exists(ei_workbook()), "EI workbook not staged in data/raw")

  sheets <- readxl::excel_sheets(ei_workbook())
  specs <- critical_minerals_production_specs()
  expect_gt(length(specs), 0)

  for (spec in specs) {
    # Sheet names, never positions: positions shift when EI adds a sheet.
    expect_false(is.numeric(spec$sheet), info = spec$tech_name)
    expect_true(spec$sheet %in% sheets, info = paste(spec$tech_name, "->", spec$sheet))

    sheet_data <- suppressMessages(
      readxl::read_excel(ei_workbook(), sheet = spec$sheet, skip = spec$skip)
    )
    expect_true(spec$nm_col %in% names(sheet_data), info = paste(spec$tech_name, spec$nm_col))

    val_col <- critical_minerals_production_resolve_val_col(sheet_data, sheet_id = spec$sheet)
    expect_true(val_col %in% names(sheet_data), info = paste(spec$tech_name, val_col))
    expect_true(is.numeric(sheet_data[[val_col]]), info = paste(spec$tech_name, val_col))
  }
})

test_that("footnote markers on country names do not drop the country", {
  # The EI graphite sheet labels footnotes by appending a digit, which silently zeroed
  # Brazil, India and Türkiye out of every name-based join.
  expect_equal(standardize_country_names(c("Brazil1", "India2")), c("Brazil", "India"))
  expect_equal(standardize_country_names("Türkiye"), "Turkiye")

  # Only a digit directly after a letter is a footnote marker; numbers elsewhere stay put.
  expect_equal(standardize_country_names("^ less than 0.05"), "^ less than 0.05")
  expect_equal(standardize_country_names("Timor-Leste"), "Timor-Leste")
  expect_equal(standardize_country_names("US"), "United States")
})
