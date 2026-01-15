repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))

test_that("normalize_year handles character and integer years", {
  index_definition <- list(
    pillars = list(
      energy_security = list(
        categories = list(
          `Energy Access` = list(score_variable = "Overall Energy Access Index")
        )
      )
    ),
    overall_variables = list(),
    variable_levels = list()
  )
  options(opportunity_security.index_definition = index_definition)

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
    missing_data = list(energy_access_consumption = "global_average"),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  expect_equal(outputs$energy_security_index$Year_used, 2022)
})

test_that("missing-data policy applies global average vs zero", {
  index_definition <- list(
    pillars = list(
      energy_security = list(
        categories = list(
          `Energy Access` = list(score_variable = "Overall Energy Access Index")
        )
      )
    ),
    overall_variables = list(),
    variable_levels = list()
  )
  options(opportunity_security.index_definition = index_definition)

  theme_tbl <- tibble::tibble(
    Country = c("A", "B"),
    tech = c("Solar", "Solar"),
    supply_chain = c("Upstream", "Upstream"),
    category = c("Energy Access", "Energy Access"),
    variable = c("Overall Energy Access Index", "Overall Energy Access Index"),
    value = c(1, NA_real_),
    Year = c(2022, 2022),
    data_type = c("index", "index"),
    source = c("Test", "Test"),
    explanation = c("Example", "Example")
  )

  outputs_global <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 1),
    missing_data = list(energy_access_consumption = "global_average"),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  outputs_zero <- build_energy_security_index_v2(
    theme_tables = list(energy_access_consumption = theme_tbl),
    weights = list(`Energy Access` = 1),
    missing_data = list(energy_access_consumption = "zero"),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  index_global <- outputs_global$energy_security_index |>
    dplyr::filter(Country == "B") |>
    dplyr::pull(Energy_Security_Index)

  index_zero <- outputs_zero$energy_security_index |>
    dplyr::filter(Country == "B") |>
    dplyr::pull(Energy_Security_Index)

  expect_equal(index_global, 1)
  expect_equal(index_zero, 0)
})
