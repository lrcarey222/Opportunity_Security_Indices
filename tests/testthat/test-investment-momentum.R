repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
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

test_that("Critical Minerals maps to Batteries Refining and Electric Vehicles Refining", {
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

  expect_setequal(paste(mapped_pairs$tech, mapped_pairs$supply_chain), c("Batteries Refining", "Electric Vehicles Refining"))

  # Capacity of the critical-minerals products (refined chemicals and metals) is Refining too.
  capacity_pairs <- out %>%
    dplyr::filter(variable %in% c("Operating Capacity Index", "Pipeline Capacity Index")) %>%
    dplyr::distinct(tech, supply_chain)
  expect_setequal(unique(capacity_pairs$supply_chain), "Refining")
  expect_setequal(unique(capacity_pairs$tech), c("Batteries", "Electric Vehicles"))
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
    dplyr::filter(variable == "Annual Investment Index", supply_chain %in% c("Components", "Assembly"))

  expect_gt(nrow(annual_idx), 0)
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
  expect_true(all(out$supply_chain %in% SUPPLY_CHAIN_STAGES))
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

# ---- Critical-minerals split, stage/year completion, zero scoring --------------------------

panel_fixture <- function() {
  annual_tbl <- tibble::tibble(
    Country = c("US", "US", "Canada", "Mexico", "Chile", "US", "Canada"),
    Segment = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Industry", "Electric Power", "Manufacturing"),
    Technology = c("Solar PV", "Solar PV", "Solar PV", "Solar PV", "Critical Minerals", "Solar PV", "Solar PV"),
    Year = c(2022, 2025, 2022, 2025, 2025, 2025, 2025),
    Investment = c(4, 10, 3, 1, 8, 6, 0)
  )
  capacity_tbl <- tibble::tibble(
    Country = c("US", "Chile"),
    Segment = c("Manufacturing", "Industry"),
    Technology = c("Solar PV", "Critical Minerals"),
    Product = c("Module", "Lithium"),
    Category = c("Current operational capacity", "Current operational capacity"),
    Value = c(100, 40)
  )
  list(annual = annual_tbl, capacity = capacity_tbl)
}

test_that("critical-minerals investment is split between Batteries and EV, not copied to both", {
  fx <- panel_fixture()
  split <- c("Batteries" = 0.25, "Electric Vehicles" = 0.75)

  out <- investment_momentum(fx$annual, fx$capacity, critical_minerals_split = split)
  raw <- out %>%
    dplyr::filter(variable == "Annual Investment (USD bn, 2024$)", supply_chain == "Refining", Country == "Chile")

  expect_equal(raw$value[raw$tech == "Batteries"], 8 * 0.25)
  expect_equal(raw$value[raw$tech == "Electric Vehicles"], 8 * 0.75)
  expect_equal(sum(raw$value), 8)
  expect_true(all(grepl("split between Batteries and Electric Vehicles", raw$explanation, fixed = TRUE)))

  default_raw <- investment_momentum(fx$annual, fx$capacity) %>%
    dplyr::filter(variable == "Annual Investment (USD bn, 2024$)", supply_chain == "Refining", Country == "Chile")
  expect_equal(sum(default_raw$value), 8)

  expect_error(
    investment_momentum(fx$annual, fx$capacity, critical_minerals_split = c("Batteries" = 1, "Electric Vehicles" = 1)),
    "summing to 1"
  )
})

test_that("the demand-based split averages each battery mineral's Batteries/EV shares", {
  demand <- tibble::tribble(
    ~Mineral,   ~tech,               ~share_24,
    "Lithium",  "Electric Vehicles",      0.8,
    "Lithium",  "Batteries",              0.2,
    "Graphite", "Electric Vehicles",      0.6,
    "Graphite", "Batteries",              0.4,
    "Copper",   "Electric Grid",          0.6
  )

  split <- investment_critical_minerals_split(demand)

  expect_equal(unname(split[["Batteries"]]), mean(c(0.2, 0.4)))
  expect_equal(unname(split[["Electric Vehicles"]]), mean(c(0.8, 0.6)))
  expect_equal(sum(split), 1)
})

test_that("only the stages the monitor covers are completed, over every year", {
  fx <- panel_fixture()
  out <- investment_momentum(fx$annual, fx$capacity)

  annual_idx <- out %>% dplyr::filter(variable == "Annual Investment Index")

  # Solar investment is manufacturing (Components/Assembly) and deployment only: no
  # manufactured all-zero Solar Extraction, Refining or Recovery.
  expect_false(any(annual_idx$tech == "Solar" & annual_idx$supply_chain %in% c("Extraction", "Refining", "Recovery")))
  expect_setequal(
    unique(annual_idx$supply_chain[annual_idx$tech == "Solar"]),
    c("Components", "Assembly", "Deployment")
  )
  # Critical-minerals capex is the only Batteries/EV investment in the fixture: Refining only.
  expect_setequal(unique(annual_idx$supply_chain[annual_idx$tech == "Batteries"]), "Refining")
  # Every country's panel runs to the latest year, so latest values compare like with like.
  latest <- annual_idx %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarize(latest = max(Year), .groups = "drop")
  expect_true(all(latest$latest == 2025L))
})

test_that("zero investment scores 0 and the smallest investor scores above it", {
  fx <- panel_fixture()
  out <- investment_momentum(fx$annual, fx$capacity)

  solar_comp_2025 <- out %>%
    dplyr::filter(variable == "Annual Investment Index", tech == "Solar", supply_chain == "Components", Year == 2025L)
  value_of <- function(country) solar_comp_2025$value[solar_comp_2025$Country == country]

  expect_equal(value_of("Canada"), 0)
  expect_gt(value_of("Mexico"), 0)
  expect_gt(value_of("United States"), value_of("Mexico"))

  # Canada invested in 2022 but not 2025, so it has a (negative) momentum; a country with
  # nothing in either window year has none and is left to the missing-data rule.
  momentum <- out %>%
    dplyr::filter(variable == "Investment Momentum Index", tech == "Solar", supply_chain == "Components")
  expect_true(all(c("United States", "Canada", "Mexico") %in% momentum$Country))
  expect_lt(momentum$value[momentum$Country == "Canada"], momentum$value[momentum$Country == "Mexico"])

  deployment_momentum <- out %>%
    dplyr::filter(variable == "Investment Momentum Index", tech == "Solar", supply_chain == "Deployment")
  expect_setequal(deployment_momentum$Country, "United States")
})

# ---- Six-stage mapping -----------------------------------------------------------------------

test_that("GCIM capacity products map to stages relative to their technology", {
  products <- c(
    "Polysilicon", "Wafers", "Cells", "Modules", "Blades", "Nacelles", "EAM", "ZEVs",
    "Module", "Lithium", "Lithium Carbonate", "Lithium Hydroxide", "Lithium Chloride",
    "Lithium Sulfate", "Nickel Sulfate", "Cobalt Sulfate", "Nickel", "Cobalt", "Graphite",
    "Carbon capture", NA
  )
  expect_equal(
    investment_product_to_stage(products),
    c(
      "Refining", "Components", "Components", "Assembly", "Components", "Assembly", "Components", "Assembly",
      "Assembly", rep("Refining", 10),
      NA, NA
    )
  )
  # A mineral-led name for a part or finished good is staged by the part, not the mineral.
  expect_equal(
    investment_product_to_stage(c("Lithium-ion Cells", "Lithium-ion battery modules")),
    c("Components", "Assembly")
  )
})

test_that("GCIM sectors map to Deployment, product stages, or the replicated Midstream roll-up", {
  expect_equal(
    investment_segment_to_supply_chain(c("Electric Power", "Transport", NA, "")),
    rep("Deployment", 4)
  )
  # No product (annual capex): manufacturing comes back as the roll-up the caller replicates.
  expect_equal(investment_segment_to_supply_chain(c("Manufacturing", "Industry")), c("Midstream", "Midstream"))
  expect_equal(
    investment_segment_to_supply_chain(
      c("Manufacturing", "Manufacturing", "Manufacturing", "Electric Power"),
      c("Polysilicon", "Cells", "Modules", "Modules")
    ),
    c("Refining", "Components", "Assembly", "Deployment")
  )
  expect_warning(
    expect_equal(investment_segment_to_supply_chain("Manufacturing", "Frames"), "Midstream"),
    "Replicating these rows onto Components and Assembly"
  )
  expect_warning(
    expect_equal(investment_segment_to_supply_chain("Buildings"), "Deployment"),
    "Defaulting these rows to Deployment"
  )
})

stage_fixture <- function() {
  annual_tbl <- tibble::tibble(
    Country = c("US", "Canada", "US", "Mexico"),
    Segment = c("Manufacturing", "Manufacturing", "Electric Power", "Transport"),
    Technology = c("Solar PV", "Solar PV", "Solar PV", "Electric Vehicles (LDV)"),
    Year = c(2025, 2025, 2025, 2025),
    Investment = c(10, 4, 6, 3)
  )
  op <- "Current operational capacity"
  # Batteries EAM is in kilotons and cells in GWh: US and Canada mirror each other across the
  # two products, so a raw sum (US 1001, Mexico 550, Canada 110) ranks them apart while the
  # per-product indices tie them.
  capacity_tbl <- tibble::tribble(
    ~Country, ~Segment,        ~Technology,              ~Product,      ~Category, ~Value,
    "US",     "Manufacturing", "Solar",                  "Polysilicon", op,          50,
    "US",     "Manufacturing", "Solar",                  "Wafers",      op,          20,
    "Canada", "Manufacturing", "Solar",                  "Cells",       op,          10,
    "US",     "Manufacturing", "Solar",                  "Modules",     op,          30,
    "US",     "Manufacturing", "Wind",                   "Blades",      op,           5,
    "Canada", "Manufacturing", "Wind",                   "Nacelles",    op,           4,
    "US",     "Manufacturing", "Zero Emission Vehicles", "ZEVs",        op,        1000,
    "US",     "Manufacturing", "Batteries",              "EAM",         op,        1000,
    "US",     "Manufacturing", "Batteries",              "Cells",       op,           1,
    "Canada", "Manufacturing", "Batteries",              "EAM",         op,          10,
    "Canada", "Manufacturing", "Batteries",              "Cells",       op,         100,
    "Mexico", "Manufacturing", "Batteries",              "EAM",         op,         500,
    "Mexico", "Manufacturing", "Batteries",              "Cells",       op,          50,
    "US",     "Manufacturing", "Batteries",              "Modules",     op,          40
  )
  list(annual = annual_tbl, capacity = capacity_tbl)
}

test_that("every output row carries one of the six stages", {
  fx <- stage_fixture()
  out <- investment_momentum(fx$annual, fx$capacity)

  expect_true(all(out$supply_chain %in% SUPPLY_CHAIN_STAGES))
  expect_invisible(assert_supply_chain_stages(out$supply_chain))
})

test_that("manufacturing capacity is split into Refining, Components and Assembly by product", {
  fx <- stage_fixture()
  out <- investment_momentum(fx$annual, fx$capacity)

  operating <- out %>%
    dplyr::filter(variable == "Operating Capacity Index") %>%
    dplyr::distinct(tech, supply_chain)
  expect_setequal(
    paste(operating$tech, operating$supply_chain),
    c(
      "Solar Refining", "Solar Components", "Solar Assembly",
      "Wind Components", "Wind Assembly",
      "Electric Vehicles Assembly",
      "Batteries Components", "Batteries Assembly"
    )
  )

  # The split keeps every unit of capacity: the stage sums add back to the old Midstream total.
  raw <- out %>%
    dplyr::filter(variable == "Operating Capacity (raw stage sum)", tech == "Solar")
  expect_equal(sum(raw$value), 50 + 20 + 10 + 30)
  expect_equal(raw$value[raw$supply_chain == "Refining"], 50)
  expect_equal(sum(raw$value[raw$supply_chain == "Components"]), 20 + 10)
})

test_that("a stage's capacity index averages per-product indices instead of summing units", {
  fx <- stage_fixture()
  out <- investment_momentum(fx$annual, fx$capacity)

  comp <- out %>%
    dplyr::filter(variable == "Operating Capacity Index", tech == "Batteries", supply_chain == "Components")
  value_of <- function(country) comp$value[comp$Country == country]

  expect_equal(value_of("United States"), value_of("Canada"))
  expect_gt(value_of("Canada"), value_of("Mexico"))
  expect_true(all(grepl("each product indexed on its own", comp$explanation, fixed = TRUE)))
})

test_that("manufacturing capex is replicated onto Components and Assembly; power and transport are Deployment", {
  fx <- stage_fixture()
  out <- investment_momentum(fx$annual, fx$capacity)

  raw <- out %>%
    dplyr::filter(variable == "Annual Investment (USD bn, 2024$)", Year == 2025L)
  solar_value <- function(country, stage) {
    raw$value[raw$tech == "Solar" & raw$Country == country & raw$supply_chain == stage]
  }

  expect_equal(solar_value("United States", "Components"), 10)
  expect_equal(solar_value("United States", "Assembly"), 10)
  expect_equal(solar_value("Canada", "Assembly"), 4)
  expect_equal(solar_value("United States", "Deployment"), 6)
  expect_equal(raw$value[raw$tech == "Electric Vehicles" & raw$supply_chain == "Deployment"], 3)
  expect_false(any(raw$supply_chain %in% SUPPLY_CHAIN_LEGACY_STAGES))

  replicated <- raw$explanation[raw$supply_chain %in% c("Components", "Assembly")]
  expect_true(all(grepl("replicated onto Components and Assembly", replicated, fixed = TRUE)))
  expect_false(any(grepl("replicated", raw$explanation[raw$supply_chain == "Deployment"], fixed = TRUE)))

  # Identical inputs, identical ranks: the replicated stages score the same.
  idx <- out %>% dplyr::filter(variable == "Annual Investment Index", tech == "Solar", Year == 2025L)
  comp <- idx %>% dplyr::filter(supply_chain == "Components") %>% dplyr::arrange(Country)
  asm <- idx %>% dplyr::filter(supply_chain == "Assembly") %>% dplyr::arrange(Country)
  expect_equal(comp$value, asm$value)
})

test_that("capacity shares are per product of world operating capacity, by status, the stage taking its most material product", {
  cap <- data.frame(
    Country = c("Japan", "Japan", "Japan", "Korea, Rep.", "Korea, Rep.", "Japan", "Korea, Rep.", "Japan"),
    Segment = "Manufacturing", Technology = "Solar",
    Product = c("Cells", "Cells", "Cells", "Cells", "Wafers", "Wafers", "Wafers", "Modules"),
    Category = c("Current operational capacity", "Under construction - anticipated capacity", "Announced - anticipated capacity",
                 "Current operational capacity", "Current operational capacity", "Operational yet to come online",
                 "Announced - anticipated capacity", "Announced - anticipated capacity"),
    Value = c(10, 5, 40, 90, 50, 25, 10, 7),
    stringsAsFactors = FALSE
  )
  s <- investment_capacity_shares(investment_capacity_clean(cap))
  jp <- s[s$Country == "Japan" & s$supply_chain == "Components", ]
  # cells: operating 10 / 100, committed 5 / 100, announced 40 / 100; wafers: committed 25 / 50 -> max 0.5
  expect_equal(jp$operating_share, 0.1)
  expect_equal(jp$committed_share, 0.5)
  expect_equal(jp$announced_share, 0.4)
  kr <- s[s$Country != "Japan" & s$supply_chain == "Components", ]
  expect_equal(kr$operating_share, 1)          # wafers: 50 of 50
  expect_equal(kr$announced_share, 0.2)
  # modules have no operating capacity anywhere: no world denominator, so no share
  expect_false(any(s$supply_chain == "Assembly"))
})
