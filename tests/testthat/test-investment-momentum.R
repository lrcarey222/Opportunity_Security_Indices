repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "investment", "investment_momentum.R"))

test_that("investment_momentum returns schema-valid Investment category output", {
  annual_tbl <- tibble::tibble(
    Country = c("US", "US", "Canada", "Canada"),
    Segment = c("Manufacturing", "Electric Power", "Manufacturing", "Electric Power"),
    Technology = c("Solar PV", "Solar PV", "Solar PV", "Solar PV"),
    Year = c(2020, 2020, 2020, 2020),
    Investment = c(10, 0, 0, 5)
  )

  capacity_tbl <- tibble::tibble(
    Country = c("US", "US", "Canada", "Canada"),
    Segment = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV", "Solar PV", "Solar PV"),
    Product = c("Module", "Module", "Module", "Module"),
    End_use_application = c("Grid", "Grid", "Grid", "Grid"),
    Facility_Type = c("Plant", "Plant", "Plant", "Plant"),
    Category = c(
      "Current operational capacity",
      "Under construction - anticipated capacity",
      "Current operational capacity",
      "Under construction - anticipated capacity"
    ),
    Value = c(100, 20, 0, 0)
  )

  out <- investment_momentum(
    annual_tbl = annual_tbl,
    capacity_tbl = capacity_tbl,
    momentum_window_years = 2
  )

  expect_invisible(validate_schema(out))
  expect_true(all(out$category == "Investment"))

  component_vars <- c(
    "Annual Investment Index",
    "Investment Momentum Index",
    "Operating Capacity Index",
    "Pipeline Capacity Index"
  )

  component_out <- out %>%
    dplyr::filter(variable %in% component_vars, data_type == "index")

  expect_true(all(component_out$value >= 0 & component_out$value <= 1, na.rm = TRUE))

  dupes <- out %>%
    dplyr::count(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      Year,
      name = "n"
    ) %>%
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0)
})

test_that("all-zero annual groups normalize to zero instead of 0.5", {
  annual_tbl <- tibble::tibble(
    Country = c("US", "Canada"),
    Segment = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV"),
    Year = c(2024, 2024),
    Investment = c(0, 0)
  )

  capacity_tbl <- tibble::tibble(
    Country = c("US", "Canada"),
    Segment = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV"),
    Product = c("Module", "Module"),
    End_use_application = c("Grid", "Grid"),
    Facility_Type = c("Plant", "Plant"),
    Category = c("Current operational capacity", "Current operational capacity"),
    Value = c(0, 0)
  )

  out <- investment_momentum(
    annual_tbl = annual_tbl,
    capacity_tbl = capacity_tbl
  )

  annual_idx <- out %>%
    dplyr::filter(variable == "Annual Investment Index", supply_chain == "Midstream")

  expect_true(all(annual_idx$value == 0))
})
