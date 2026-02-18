repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))

test_that("energy_access_build_growth computes growth index across countries within each tech", {
  ec_base <- tibble::tibble(
    Country = c("A", "A", "B", "B"),
    tech = c("Solar", "Wind", "Solar", "Wind"),
    supply_chain = "Downstream",
    category = "Energy Access",
    variable = "Energy consumption per capita",
    data_type = "raw",
    value = c(10, 10, 10, 10),
    Year = "2019",
    source = "Test",
    explanation = "Test"
  )

  ec_target <- tibble::tibble(
    Country = c("A", "A", "B", "B"),
    tech = c("Solar", "Wind", "Solar", "Wind"),
    supply_chain = "Downstream",
    category = "Energy Access",
    variable = "Energy consumption per capita",
    data_type = "raw",
    value = c(11, 12, 15, 14),
    Year = "2024",
    source = "Test",
    explanation = "Test"
  )

  out <- energy_access_build_growth(ec_base, ec_target, 2019, 2024)

  growth_index <- out |>
    dplyr::filter(variable == "Energy consumption per capita growth", data_type == "index") |>
    dplyr::select(Country, tech, value)

  expect_equal(
    growth_index |> dplyr::filter(tech == "Solar") |> dplyr::arrange(Country) |> dplyr::pull(value),
    c(0, 1)
  )
  expect_equal(
    growth_index |> dplyr::filter(tech == "Wind") |> dplyr::arrange(Country) |> dplyr::pull(value),
    c(0, 1)
  )
})

test_that("energy_access_build_growth treats zero baseline as missing growth", {
  ec_base <- tibble::tibble(
    Country = c("A", "B"),
    tech = c("Solar", "Solar"),
    supply_chain = "Downstream",
    category = "Energy Access",
    variable = "Energy consumption per capita",
    data_type = "raw",
    value = c(0, 10),
    Year = "2019",
    source = "Test",
    explanation = "Test"
  )

  ec_target <- tibble::tibble(
    Country = c("A", "B"),
    tech = c("Solar", "Solar"),
    supply_chain = "Downstream",
    category = "Energy Access",
    variable = "Energy consumption per capita",
    data_type = "raw",
    value = c(5, 11),
    Year = "2024",
    source = "Test",
    explanation = "Test"
  )

  out <- energy_access_build_growth(ec_base, ec_target, 2019, 2024)

  growth_raw <- out |>
    dplyr::filter(variable == "Energy consumption per capita growth", data_type == "raw") |>
    dplyr::arrange(Country) |>
    dplyr::pull(value)

  expect_true(is.na(growth_raw[[1]]))
  expect_equal(growth_raw[[2]], 0.1)
})
