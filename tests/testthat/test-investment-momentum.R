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

test_that("Viet Nam is retained when country_reference does not include it (default non-enforcing)", {
  annual_tbl <- tibble::tibble(
    Country = c("Vietnam"),
    Segment = c("Industry"),
    Technology = c("Solar PV"),
    Year = c(2024),
    Investment = c(5)
  )

  capacity_tbl <- tibble::tibble(
    Country = c("Vietnam"),
    Segment = c("Industry"),
    Technology = c("Solar PV"),
    Product = c("Module"),
    End_use_application = c("Grid"),
    Facility_Type = c("Plant"),
    Category = c("Current operational capacity"),
    Value = c(8)
  )

  out <- investment_momentum(
    annual_tbl = annual_tbl,
    capacity_tbl = capacity_tbl,
    country_reference = c("United States")
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

test_that("GCIM 'Sector'/'Region' column labels are accepted in place of 'Segment'", {
  annual_tbl <- tibble::tibble(
    Region = c("US", "Rest of the World"),
    Country = c("US", "Canada"),
    Sector = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV"),
    Year = c(2024, 2024),
    Investment = c(10, 4)
  )

  # The 2026Q1 download drops End_use_application / Facility_Type entirely.
  capacity_tbl <- tibble::tibble(
    Region = c("US", "Rest of the World"),
    Country = c("US", "Canada"),
    Sector = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar", "Solar"),
    Product = c("Modules", "Modules"),
    Category = c("Current operational capacity", "Current operational capacity"),
    Value = c(100, 20)
  )

  out <- investment_momentum(annual_tbl = annual_tbl, capacity_tbl = capacity_tbl)

  expect_invisible(validate_schema(out))
  expect_true(all(out$supply_chain %in% c("Upstream", "Midstream", "Downstream")))
  expect_true(any(out$variable == "Operating Capacity Index"))
})

test_that("investment_momentum_from_excel finds sheets and header rows by content", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  path <- withr::local_tempfile(fileext = ".xlsx")

  pad <- function(tbl, notes) {
    # Mimic the workbook's title/notes rows above the header row.
    header <- names(tbl)
    body <- lapply(tbl, as.character)
    blanks <- rep(NA_character_, length(header))
    rows <- c(
      lapply(notes, function(note) c(note, blanks[-1])),
      list(header),
      lapply(seq_len(nrow(tbl)), function(i) vapply(body, function(col) col[[i]], character(1)))
    )
    out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(out) <- paste0("V", seq_along(out))
    out
  }

  annual <- tibble::tibble(
    Region = c("US", "Rest of the World"),
    Country = c("US", "Canada"),
    Sector = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV"),
    Year = c(2022, 2022),
    Investment = c(1, 2)
  )
  annual2 <- annual
  annual2$Year <- c(2025, 2025)
  annual2$Investment <- c(9, 3)

  capacity <- tibble::tibble(
    Region = c("US", "Rest of the World"),
    Country = c("US", "Canada"),
    Sector = c("Manufacturing", "Manufacturing"),
    Technology = c("Solar", "Solar"),
    Product = c("Modules", "Modules"),
    Category = c("Current operational capacity", "Announced - anticipated capacity"),
    Value = c(50, 10)
  )

  quarterly <- tibble::tibble(
    Region = c("US"),
    Country = c("US"),
    Sector = c("Manufacturing"),
    Technology = c("Solar"),
    Quarter = c("2025-Q1"),
    Investment = c(0.5)
  )

  writexl::write_xlsx(
    list(
      README = data.frame(V1 = "Notes tab that must be ignored."),
      annual_actual_investment = pad(dplyr::bind_rows(annual, annual2), c("Capital investment", NA)),
      mfg_ind_capacity = pad(capacity, c("Country-level capacity : 2025", rep(NA, 8))),
      mfg_ind_quarterly_actual_inv = pad(quarterly, c("Quarterly investment", NA, NA))
    ),
    path,
    col_names = FALSE
  )

  out <- investment_momentum_from_excel(path, momentum_window_years = 3)

  expect_invisible(validate_schema(out))
  expect_true(any(out$variable == "Investment Momentum Index"))
  expect_true(any(out$variable == "Operating Capacity Index"))
  expect_true(any(out$variable == "Pipeline Capacity Index"))
  expect_setequal(unique(out$Country), c("United States", "Canada"))
})

test_that("investment_momentum_from_excel errors informatively on an unusable workbook", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(annual_actual_investment = data.frame(x = 1)), path)

  expect_error(
    investment_momentum_from_excel(path),
    "Could not locate the manufacturing/industry capacity sheet",
    fixed = TRUE
  )
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
