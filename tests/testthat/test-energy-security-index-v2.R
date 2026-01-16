repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))

test_that("normalize_year handles character and integer years", {
  set_test_index_definition()

  theme_tables <- list(
    energy_access_consumption = tibble::tibble(
      Country = c("A", "A"),
      tech = c("Solar", "Solar"),
      supply_chain = c("Upstream", "Upstream"),
      category = c("Energy Access", "Energy Access"),
      variable = c("Overall Energy Access Index", "Overall Energy Access Index"),
      value = c(0.2, 0.8),
      Year = c(2021, "2022-01-01"),
      data_type = c("index", "index"),
      source = c("Test", "Test"),
      explanation = c("Example", "Example")
    )
  )

  outputs <- build_energy_security_index_v2(
    theme_tables = theme_tables,
    weights = list(`Energy Access` = 1),
    missing_data = list(`Overall Energy Access Index` = "global_average"),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  year_selected <- outputs$diagnostics$year_provenance$Year_selected
  expect_equal(year_selected, 2022)
})

test_that("missing-data policy applies global average vs zero", {
  set_test_index_definition()

  theme_tbl <- read_fixture_csv("energy_security_theme.csv") |>
    dplyr::filter(category == "Energy Access", sub_sector == "Manufacturing") |>
    dplyr::select(-sub_sector)

  missing_global <- read_fixture_yaml("missing_data_global_average.yml")
  missing_zero <- read_fixture_yaml("missing_data_zero.yml")

  outputs_global <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 1),
    missing_data = missing_global,
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  outputs_zero <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 1),
    missing_data = missing_zero,
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  index_global <- outputs_global$energy_security_index |>
    dplyr::filter(Country == "B") |>
    dplyr::pull(Energy_Security_Index)

  index_zero <- outputs_zero$energy_security_index |>
    dplyr::filter(Country == "B") |>
    dplyr::pull(Energy_Security_Index)

  expect_equal(index_global, 0.2)
  expect_equal(index_zero, 0)
})

test_that("build_energy_security_index_v2 returns one row per key and toggles sub_sector", {
  set_test_index_definition()

  theme_tbl <- read_fixture_csv("energy_security_theme.csv") |>
    dplyr::filter(!is.na(value))

  outputs <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 0.5, Reliability = 0.5),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  expected_rows <- theme_tbl |>
    dplyr::distinct(Country, tech, supply_chain) |>
    nrow()

  expect_equal(nrow(outputs$energy_security_index), expected_rows)
  expect_false("sub_sector" %in% names(outputs$energy_security_index))

  outputs_sub <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 0.5, Reliability = 0.5),
    allow_partial_categories = TRUE,
    include_sub_sector = TRUE
  )

  expected_sub_rows <- theme_tbl |>
    dplyr::distinct(Country, tech, supply_chain, sub_sector) |>
    nrow()

  expect_equal(nrow(outputs_sub$energy_security_index), expected_sub_rows)
  expect_true("sub_sector" %in% names(outputs_sub$energy_security_index))
})

test_that("score_variable selection in index_definition controls category scores", {
  set_test_index_definition()

  theme_tbl <- read_fixture_csv("energy_security_theme.csv") |>
    dplyr::filter(Country == "A", category == "Energy Access")

  outputs <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 1),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  category_score <- outputs$category_scores |>
    dplyr::filter(Country == "A", category == "Energy Access") |>
    dplyr::pull(category_score)

  expect_equal(category_score, 0.3)
})

test_that("weights normalize when allow_partial_categories is TRUE", {
  set_test_index_definition()

  theme_tbl <- read_fixture_csv("energy_security_theme.csv") |>
    dplyr::filter(category == "Energy Access", !is.na(value))

  outputs <- suppressWarnings(build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 0.2, Reliability = 0.8),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  ))

  index_value <- outputs$energy_security_index |>
    dplyr::filter(Country == "A") |>
    dplyr::pull(Energy_Security_Index)

  category_score <- outputs$category_scores |>
    dplyr::filter(Country == "A", category == "Energy Access") |>
    dplyr::pull(category_score)

  expect_equal(index_value, category_score)
})

test_that("final indices omit Year columns", {
  set_test_index_definition()

  theme_tbl <- read_fixture_csv("energy_security_theme.csv") |>
    dplyr::filter(!is.na(value))

  outputs <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 0.5, Reliability = 0.5),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  expect_false("Year" %in% names(outputs$energy_security_index))
})
