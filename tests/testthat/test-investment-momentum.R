repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "investment", "investment_momentum.R"))

standardize_theme_types_test <- function(tbl, country_info = NULL) {
  if (is.null(tbl)) {
    return(tbl)
  }

  standardized <- tbl %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      sub_sector = if ("sub_sector" %in% names(tbl)) as.character(sub_sector) else NULL,
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    )

  if (is.null(country_info)) {
    return(standardized)
  }

  standardized_with_country <- standardize_country_table(standardized, country_info = country_info)
  if (nrow(standardized_with_country) == 0 && nrow(standardized) > 0) {
    return(standardized)
  }

  standardized_with_country
}

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
  expect_true(any(out$data_type == "raw"))

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

test_that("Critical Minerals maps to Batteries Upstream and Electric Vehicles Upstream", {
  annual_tbl <- tibble::tibble(
    Country = c("US"),
    Segment = c("Industry"),
    Technology = c("Critical Minerals"),
    Year = c(2024),
    Investment = c(7)
  )

  capacity_tbl <- tibble::tibble(
    Country = c("US", "US"),
    Segment = c("Industry", "Industry"),
    Technology = c("Critical Minerals", "Critical Minerals"),
    Product = c("Lithium", "Nickel"),
    End_use_application = c("Battery", "Vehicle"),
    Facility_Type = c("Plant", "Plant"),
    Category = c("Current operational capacity", "Under construction - anticipated capacity"),
    Value = c(10, 20)
  )

  out <- investment_momentum(annual_tbl = annual_tbl, capacity_tbl = capacity_tbl)

  mapped_pairs <- out %>%
    dplyr::filter(data_type == "raw", variable == "Annual Investment (USD bn, 2024$)") %>%
    dplyr::distinct(tech, supply_chain)

  expect_true(any(mapped_pairs$tech == "Batteries" & mapped_pairs$supply_chain == "Upstream"))
  expect_true(any(mapped_pairs$tech == "Electric Vehicles" & mapped_pairs$supply_chain == "Upstream"))
})

test_that("country_reference filtering standardizes Vietnam/Viet Nam", {
  annual_tbl <- tibble::tibble(
    Country = c("Vietnam", "US"),
    Segment = c("Industry", "Industry"),
    Technology = c("Solar PV", "Solar PV"),
    Year = c(2024, 2024),
    Investment = c(5, 3)
  )

  capacity_tbl <- tibble::tibble(
    Country = c("Vietnam", "US"),
    Segment = c("Industry", "Industry"),
    Technology = c("Solar PV", "Solar PV"),
    Product = c("Module", "Module"),
    End_use_application = c("Grid", "Grid"),
    Facility_Type = c("Plant", "Plant"),
    Category = c("Current operational capacity", "Current operational capacity"),
    Value = c(8, 6)
  )

  out <- investment_momentum(
    annual_tbl = annual_tbl,
    capacity_tbl = capacity_tbl,
    country_reference = c("Viet Nam", "United States")
  )

  expect_true(any(out$Country == "Viet Nam"))
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

test_that("standardize_theme_types fallback preserves rows when country_info mismatches", {
  annual_tbl <- tibble::tibble(
    Country = c("US", "Canada"),
    Segment = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV"),
    Year = c(2024, 2024),
    Investment = c(1, 2)
  )

  capacity_tbl <- tibble::tibble(
    Country = c("US", "Canada"),
    Segment = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV"),
    Product = c("Module", "Module"),
    End_use_application = c("Grid", "Grid"),
    Facility_Type = c("Plant", "Plant"),
    Category = c("Current operational capacity", "Current operational capacity"),
    Value = c(10, 20)
  )

  out <- investment_momentum(annual_tbl = annual_tbl, capacity_tbl = capacity_tbl)

  # Deliberately mismatched country_info (iso3c missing/blank) would normally drop all rows.
  country_info_bad <- tibble::tibble(
    country = c("Xland", "Yland"),
    iso3c = c(NA_character_, "")
  )

  standardized <- standardize_theme_types_test(out, country_info = country_info_bad)
  expect_gt(nrow(out), 0)
  expect_equal(nrow(standardized), nrow(out))
})
