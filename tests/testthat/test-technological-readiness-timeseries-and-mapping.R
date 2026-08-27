repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R"))

# read_iea_tech_map_rules() finds config/ from the repo root; without this it would
# resolve against tests/testthat.
withr::local_options(opportunity_security.repo_root = repo_root, .local_envir = teardown_env())

iea_timeseries_fixture <- function() {
  read_fixture_csv("iea_clean_tech_guide_subset_timeseries.csv")
}

test_that("token-based mapping covers batteries, EVs, fossil techs and duplication", {
  clean_tbl <- technological_readiness_clean(iea_timeseries_fixture())
  assigned <- technological_readiness_assign_tech(clean_tbl)

  sodium <- assigned |> dplyr::filter(name == "Sodium-ion battery") |> dplyr::pull(tech) |> unique()
  bev <- assigned |> dplyr::filter(name == "Battery electric passenger car") |> dplyr::pull(tech) |> unique()
  coal <- assigned |> dplyr::filter(name == "Coal mine methane use for energy supply") |> dplyr::pull(tech) |> unique()

  vapour <- assigned |> dplyr::filter(name == "Vapour recovery units") |> dplyr::pull(tech) |> unique()
  fossil_h2 <- assigned |>
    dplyr::filter(name == "Integrating electrolytic hydrogen into the refinery") |>
    dplyr::pull(tech) |>
    unique()

  expect_identical(sodium, "Batteries")
  expect_identical(bev, "Electric Vehicles")
  expect_identical(coal, "Coal")
  expect_setequal(vapour, c("Oil", "Gas"))
  expect_setequal(fossil_h2, c("Oil", "Gas"))
  expect_false("Green Hydrogen" %in% fossil_h2)
})

test_that("TRL momentum and blended index are computed in output schema", {
  techs <- c("Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind", "Oil", "Solar", "Gas", "Geothermal", "Electric Grid")
  out <- technological_readiness(iea_timeseries_fixture(), techs = techs)

  battery_delta <- out |>
    dplyr::filter(tech == "Batteries", variable == "TRL Δ 2020–2023") |>
    dplyr::pull(value) |>
    unique()
  battery_momentum <- out |>
    dplyr::filter(tech == "Batteries", variable == "TRL Momentum Index") |>
    dplyr::pull(value) |>
    unique()

  coal_momentum <- out |>
    dplyr::filter(tech == "Coal", variable == "TRL Momentum Index") |>
    dplyr::pull(value) |>
    unique()

  expect_true(all(battery_delta > 0, na.rm = TRUE))
  expect_true(all(battery_momentum > 0, na.rm = TRUE))
  expect_true(all(abs(coal_momentum) < 1e-8, na.rm = TRUE))

  expect_named(
    out,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value", "Year", "source", "explanation")
  )

  expect_true("TRL Index" %in% unique(out$variable))
  expect_true("TRL 2023" %in% unique(out$variable))
  expect_true("TRL Level Index" %in% unique(out$variable))
  expect_true("TRL Momentum Index" %in% unique(out$variable))
  expect_true("TRL Δ 2020–2023" %in% unique(out$variable))

  nuclear_2023 <- out |>
    dplyr::filter(tech == "Nuclear", variable == "TRL 2023") |>
    dplyr::pull(value) |>
    unique()

  expect_true(all(is.na(nuclear_2023)))
})
