repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index_v2.R"))

build_eo_theme_tbl <- function() {
  tibble::tibble(
    Country = c("A", "A", "A", "B", "B", "B"),
    tech = "Solar",
    supply_chain = "Upstream",
    category = c(
      "Investment",
      "Investment",
      "Innovation",
      "Investment",
      "Investment",
      "Innovation"
    ),
    variable = c(
      "Capital Availability",
      "Policy Support",
      "Innovation Score",
      "Capital Availability",
      "Policy Support",
      "Innovation Score"
    ),
    value = c(0.2, 0.4, 0.5, 0.6, 0.8, 0.3),
    Year = c(2021, 2021, 2021, 2020, 2020, 2020),
    data_type = "index",
    source = "Test",
    explanation = "Example"
  )
}

test_that("economic opportunity v2 omits Year columns and returns one row per key", {
  set_test_index_definition()

  theme_tbl <- build_eo_theme_tbl()

  outputs <- build_economic_opportunity_index_v2(
    theme_tables = list(economic_opportunity_theme = theme_tbl),
    weights = list(Investment = 0.6, Innovation = 0.4),
    missing_data = list(),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  expected_rows <- theme_tbl |>
    dplyr::distinct(Country, tech, supply_chain) |>
    nrow()

  expect_equal(nrow(outputs$economic_opportunity_index), expected_rows)
  expect_false("Year" %in% names(outputs$economic_opportunity_index))
  expect_false("Year" %in% names(outputs$category_contributions))
  expect_false("Year" %in% names(outputs$variable_contributions))
})

test_that("economic opportunity v2 indices are bounded when inputs are bounded", {
  set_test_index_definition()

  theme_tbl <- build_eo_theme_tbl()

  outputs <- build_economic_opportunity_index_v2(
    theme_tables = list(economic_opportunity_theme = theme_tbl),
    weights = list(Investment = 0.6, Innovation = 0.4),
    missing_data = list(),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  index_values <- outputs$economic_opportunity_index$Economic_Opportunity_Index

  expect_true(all(index_values >= 0 & index_values <= 1))
})

test_that("economic opportunity v2 fails when score variables exceed bounds", {
  set_test_index_definition()

  theme_tbl <- build_eo_theme_tbl() |>
    dplyr::mutate(value = dplyr::if_else(variable == "Innovation Score", 1.2, value))

  expect_error(
    build_economic_opportunity_index_v2(
      theme_tables = list(economic_opportunity_theme = theme_tbl),
      weights = list(Investment = 0.6, Innovation = 0.4),
      missing_data = list(),
      allow_partial_categories = TRUE,
      include_sub_sector = FALSE
    ),
    "bounded in \[0, 1\]"
  )
})

test_that("economic opportunity v2 contributions reconcile to index", {
  set_test_index_definition()

  theme_tbl <- build_eo_theme_tbl()

  outputs <- build_economic_opportunity_index_v2(
    theme_tables = list(economic_opportunity_theme = theme_tbl),
    weights = list(Investment = 0.6, Innovation = 0.4),
    missing_data = list(),
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )

  category_sums <- outputs$category_contributions |>
    dplyr::group_by(Country, tech, supply_chain) |>
    dplyr::summarize(total = sum(weighted_category_contribution), .groups = "drop")

  variable_sums <- outputs$variable_contributions |>
    dplyr::group_by(Country, tech, supply_chain) |>
    dplyr::summarize(total = sum(weighted_variable_contribution), .groups = "drop")

  combined <- outputs$economic_opportunity_index |>
    dplyr::select(Country, tech, supply_chain, Economic_Opportunity_Index) |>
    dplyr::left_join(category_sums, by = c("Country", "tech", "supply_chain")) |>
    dplyr::left_join(variable_sums, by = c("Country", "tech", "supply_chain"), suffix = c("_category", "_variable"))

  expect_equal(combined$total_category, combined$Economic_Opportunity_Index, tolerance = 1e-8)
  expect_equal(combined$total_variable, combined$Economic_Opportunity_Index, tolerance = 1e-8)
})
