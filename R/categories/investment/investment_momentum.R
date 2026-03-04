# Investment theme builder functions.

investment_segment_to_supply_chain <- function(segment) {
  segment_clean <- stringr::str_trim(as.character(segment))

  mapped <- dplyr::case_when(
    segment_clean %in% c("Manufacturing", "Industry") ~ "Midstream",
    segment_clean %in% c("Electric Power", "Transport") ~ "Downstream",
    is.na(segment_clean) | segment_clean == "" ~ "Downstream",
    TRUE ~ "Downstream"
  )

  unexpected_segments <- sort(unique(segment_clean[!segment_clean %in% c(
    "Manufacturing", "Industry", "Electric Power", "Transport", "", NA_character_
  )]))
  if (length(unexpected_segments) > 0) {
    warning(
      "Unexpected Segment value(s): ",
      paste(unexpected_segments, collapse = ", "),
      ". Defaulting these rows to Downstream."
    )
  }

  mapped
}

investment_map_tech <- function(technology) {
  tech_clean <- stringr::str_squish(stringr::str_to_lower(as.character(technology)))

  dplyr::case_when(
    stringr::str_detect(tech_clean, "solar") ~ "Solar",
    stringr::str_detect(tech_clean, "wind") ~ "Wind",
    stringr::str_detect(tech_clean, "batter(y|ies)|battery storage|bess") ~ "Batteries",
    stringr::str_detect(tech_clean, "zero emission vehicles|electric vehicles|\bzev\b|\bev\b") ~ "Electric Vehicles",
    stringr::str_detect(tech_clean, "nuclear") ~ "Nuclear",
    stringr::str_detect(tech_clean, "geothermal") ~ "Geothermal",
    TRUE ~ NA_character_
  )
}

investment_is_critical_minerals <- function(technology) {
  tech_clean <- stringr::str_squish(stringr::str_to_lower(as.character(technology)))
  stringr::str_detect(tech_clean, "critical minerals")
}

investment_safe_index <- function(x, gamma = 0.5) {
  if (!is.numeric(x)) {
    stop("investment_safe_index() expects numeric input.")
  }

  if (length(x) == 0) {
    return(numeric())
  }

  all_zero <- all(dplyr::coalesce(x, 0) == 0)
  if (all_zero) {
    return(rep(0, length(x)))
  }

  median_scurve(x, gamma = gamma)
}

investment_assert_columns <- function(tbl, expected, label) {
  require_columns(tbl, expected, label = label)
  invisible(tbl)
}

investment_momentum <- function(annual_tbl,
                                capacity_tbl,
                                quarterly_tbl = NULL,
                                momentum_window_years = 3,
                                capacity_year = 2025L,
                                gamma = 0.5,
                                country_reference = NULL) {
  country_reference_std <- NULL
  if (!is.null(country_reference)) {
    country_reference_std <- standardize_country_names(country_reference)
    country_reference_std <- unique(country_reference_std[!is.na(country_reference_std) & nzchar(country_reference_std)])
  }

  investment_assert_columns(
    annual_tbl,
    c("Country", "Segment", "Technology", "Year", "Investment"),
    label = "annual_tbl"
  )
  investment_assert_columns(
    capacity_tbl,
    c(
      "Country", "Segment", "Technology", "Product", "Category", "Value"
    ),
    label = "capacity_tbl"
  )

  # Mapping assumptions:
  # Segment -> supply_chain: Manufacturing/Industry=>Midstream, Electric Power/Transport=>Downstream.
  # Unexpected Segment values default to Downstream with warning.
  # Technology values are mapped to the model tech universe; unsupported technologies are dropped.

  annual_clean <- annual_tbl %>%
    dplyr::transmute(
      Country = standardize_country_names(Country),
      Technology = as.character(Technology),
      tech = investment_map_tech(Technology),
      supply_chain_segment = investment_segment_to_supply_chain(Segment),
      Year = suppressWarnings(as.integer(Year)),
      investment_value = suppressWarnings(as.numeric(Investment))
    ) %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::mutate(investment_value = dplyr::coalesce(investment_value, 0))

  annual_regular <- annual_clean %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::transmute(Country, tech, supply_chain = supply_chain_segment, Year, investment_value)

  # Explicit special-case mapping requested by user:
  # raw Technology == "Critical Minerals" contributes to both
  # Batteries Upstream and Electric Vehicles Upstream.
  annual_critical_minerals <- annual_clean %>%
    dplyr::filter(investment_is_critical_minerals(Technology)) %>%
    dplyr::select(Country, Year, investment_value) %>%
    tidyr::crossing(tech = c("Batteries", "Electric Vehicles")) %>%
    dplyr::mutate(supply_chain = "Upstream")

  annual_clean <- dplyr::bind_rows(annual_regular, annual_critical_minerals)

  dropped_techs <- annual_tbl %>%
    dplyr::mutate(mapped_tech = investment_map_tech(Technology)) %>%
    dplyr::filter(is.na(mapped_tech), !is.na(Technology), Technology != "") %>%
    dplyr::distinct(Technology) %>%
    dplyr::pull(Technology)
  if (length(dropped_techs) > 0) {
    message(
      "Dropping unmapped technologies from investment input: ",
      paste(sort(dropped_techs), collapse = ", ")
    )
  }

  if (nrow(annual_clean) == 0) {
    stop(
      "No mappable annual investment rows after applying Segment/Technology mappings. ",
      "Expected at least one technology mapping into: Solar, Wind, Batteries, Electric Vehicles, Nuclear, Geothermal."
    )
  }

  if (!is.null(country_reference_std)) {
    annual_clean <- annual_clean %>%
      dplyr::filter(Country %in% country_reference_std)
  }

  if (nrow(annual_clean) == 0) {
    stop(
      "No annual investment rows remain after applying country_reference filter. ",
      "Check country naming alignment between workbook and EI country reference."
    )
  }

  annual_agg <- annual_clean %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::summarise(investment_value = sum(investment_value, na.rm = TRUE), .groups = "drop")

  annual_raw <- annual_agg %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Annual Investment (USD bn, 2024$)",
      data_type = "raw",
      value = investment_value,
      Year,
      source = "GCIM / Rhodium",
      explanation = "Annual investment amount in 2024 USD billions, summed within Country-tech-supply_chain-year"
    )

  annual_complete <- annual_agg %>%
    dplyr::group_by(Country, tech, Year) %>%
    tidyr::complete(
      supply_chain = c("Upstream", "Midstream", "Downstream"),
      fill = list(investment_value = 0)
    ) %>%
    dplyr::ungroup()

  annual_index <- annual_complete %>%
    dplyr::group_by(tech, supply_chain, Year) %>%
    dplyr::mutate(value = investment_safe_index(log1p(investment_value), gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Annual Investment Index",
      data_type = "index",
      value,
      Year,
      source = "GCIM / Rhodium + author normalization",
      explanation = "Within tech-supply-chain-year median_scurve(log1p(annual investment)); all-zero groups set to 0"
    )

  annual_latest <- annual_complete %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::mutate(latest_year = max(Year, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Year == latest_year) %>%
    dplyr::rename(invest_latest = investment_value) %>%
    dplyr::mutate(baseline_year = latest_year - as.integer(momentum_window_years)) %>%
    dplyr::select(Country, tech, supply_chain, latest_year, baseline_year, invest_latest)

  annual_baseline <- annual_complete %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      baseline_year = Year,
      invest_baseline = investment_value
    )

  momentum_tbl <- annual_latest %>%
    dplyr::left_join(
      annual_baseline,
      by = c("Country", "tech", "supply_chain", "baseline_year")
    ) %>%
    dplyr::mutate(
      invest_baseline = dplyr::coalesce(invest_baseline, 0),
      momentum_raw = log1p(invest_latest) - log1p(invest_baseline),
      Year = as.integer(latest_year)
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = investment_safe_index(momentum_raw, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Investment Momentum Index",
      data_type = "index",
      value,
      Year,
      source = "GCIM / Rhodium + author normalization",
      explanation = "Within tech-supply-chain median_scurve(log1p(latest)-log1p(baseline)); all-zero groups set to 0"
    )

  momentum_raw_tbl <- annual_latest %>%
    dplyr::left_join(
      annual_baseline,
      by = c("Country", "tech", "supply_chain", "baseline_year")
    ) %>%
    dplyr::mutate(
      invest_baseline = dplyr::coalesce(invest_baseline, 0),
      value = log1p(invest_latest) - log1p(invest_baseline),
      Year = as.integer(latest_year)
    ) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Investment Momentum (log1p latest-baseline)",
      data_type = "raw",
      value,
      Year,
      source = "GCIM / Rhodium + author transform",
      explanation = "Raw momentum signal before normalization"
    )

  capacity_clean <- capacity_tbl %>%
    dplyr::transmute(
      Country = standardize_country_names(Country),
      Technology = as.character(Technology),
      tech = investment_map_tech(Technology),
      supply_chain_segment = investment_segment_to_supply_chain(Segment),
      Product = as.character(Product),
      capacity_stage = as.character(Category),
      Value = suppressWarnings(as.numeric(Value))
    ) %>%
    dplyr::mutate(Value = dplyr::coalesce(Value, 0))

  capacity_regular <- capacity_clean %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::transmute(Country, tech, supply_chain = supply_chain_segment, Product, capacity_stage, Value)

  capacity_critical_minerals <- capacity_clean %>%
    dplyr::filter(investment_is_critical_minerals(Technology)) %>%
    dplyr::select(Country, Product, capacity_stage, Value) %>%
    tidyr::crossing(tech = c("Batteries", "Electric Vehicles")) %>%
    dplyr::mutate(supply_chain = "Upstream")

  capacity_clean <- dplyr::bind_rows(capacity_regular, capacity_critical_minerals)

  if (!is.null(country_reference_std)) {
    capacity_clean <- capacity_clean %>%
      dplyr::filter(Country %in% country_reference_std)
  }

  if (nrow(capacity_clean) == 0) {
    stop(
      "No mappable capacity rows remain after Technology/Segment mapping and country filtering."
    )
  }

  capacity_stage_levels <- c(
    "Current operational capacity",
    "Operational yet to come online",
    "Under construction - anticipated capacity",
    "Announced - anticipated capacity",
    "Paused - anticipated capacity"
  )

  capacity_product_stage <- capacity_clean %>%
    dplyr::filter(capacity_stage %in% capacity_stage_levels) %>%
    dplyr::group_by(Country, tech, supply_chain, Product, capacity_stage) %>%
    dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain, Product, capacity_stage) %>%
    dplyr::mutate(product_stage_index = investment_safe_index(log1p(Value), gamma = gamma)) %>%
    dplyr::ungroup()

  capacity_stage_raw <- capacity_clean %>%
    dplyr::filter(capacity_stage %in% capacity_stage_levels) %>%
    dplyr::mutate(stage_group = dplyr::case_when(
      capacity_stage == "Current operational capacity" ~ "Operating",
      capacity_stage %in% c(
        "Operational yet to come online",
        "Under construction - anticipated capacity",
        "Announced - anticipated capacity",
        "Paused - anticipated capacity"
      ) ~ "Pipeline",
      TRUE ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(stage_group)) %>%
    dplyr::group_by(Country, tech, supply_chain, stage_group) %>%
    dplyr::summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = dplyr::if_else(
        stage_group == "Operating",
        "Operating Capacity (raw stage sum)",
        "Pipeline Capacity (raw stage sum)"
      ),
      data_type = "raw",
      value,
      Year = as.integer(capacity_year),
      source = "GCIM / Rhodium",
      explanation = "Raw summed capacity by stage group (units may differ across products)"
    )

  operating_capacity <- capacity_product_stage %>%
    dplyr::filter(capacity_stage == "Current operational capacity") %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(raw_stage_value = mean(product_stage_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = investment_safe_index(raw_stage_value, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Operating Capacity Index",
      data_type = "index",
      value,
      Year = as.integer(capacity_year),
      source = "GCIM / Rhodium + author normalization",
      explanation = "Mean of product-level operating capacity indices, renormalized by tech-supply-chain"
    )

  pipeline_capacity <- capacity_product_stage %>%
    dplyr::filter(capacity_stage %in% c(
      "Operational yet to come online",
      "Under construction - anticipated capacity",
      "Announced - anticipated capacity",
      "Paused - anticipated capacity"
    )) %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(raw_stage_value = mean(product_stage_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = investment_safe_index(raw_stage_value, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Investment",
      variable = "Pipeline Capacity Index",
      data_type = "index",
      value,
      Year = as.integer(capacity_year),
      source = "GCIM / Rhodium + author normalization",
      explanation = "Mean of product-level pipeline capacity indices, renormalized by tech-supply-chain"
    )

  out <- dplyr::bind_rows(
    annual_raw,
    momentum_raw_tbl,
    capacity_stage_raw,
    annual_index,
    momentum_tbl,
    operating_capacity,
    pipeline_capacity
  ) %>%
    standardize_theme_table()

  validate_schema(out)
  out
}

investment_momentum_from_excel <- function(path,
                                           momentum_window_years = 3,
                                           capacity_year = 2025L,
                                           gamma = 0.5,
                                           country_reference = NULL) {
  if (!file.exists(path)) {
    stop(
      "Missing GCIM investment monitor file at ",
      path,
      ". Expected file name: data/raw/GCIM_Investment_Capacity_aggregated.xlsx"
    )
  }

  expected_sheets <- c(
    "annual_investment",
    "quarterly_investment",
    "operating_and_planned_capacity"
  )
  sheets <- readxl::excel_sheets(path)
  missing_sheets <- setdiff(expected_sheets, sheets)
  if (length(missing_sheets) > 0) {
    stop(
      "Unexpected sheet layout in ", path,
      ". Missing sheets: ", paste(missing_sheets, collapse = ", "),
      ". Expected sheets: ", paste(expected_sheets, collapse = ", "),
      ". Found: ", paste(sheets, collapse = ", ")
    )
  }

  annual_tbl <- readxl::read_excel(path, sheet = "annual_investment", skip = 2)
  quarterly_tbl <- readxl::read_excel(path, sheet = "quarterly_investment", skip = 3)
  capacity_tbl <- readxl::read_excel(path, sheet = "operating_and_planned_capacity", skip = 8)

  expected_annual <- c("Country", "Segment", "Technology", "Year", "Investment")
  expected_quarterly <- c("Country", "Segment", "Technology", "Quarter", "Investment")
  expected_capacity <- c(
    "Country", "Segment", "Technology", "Product", "End_use_application",
    "Facility_Type", "Category", "Value"
  )

  missing_annual <- setdiff(expected_annual, names(annual_tbl))
  missing_quarterly <- setdiff(expected_quarterly, names(quarterly_tbl))
  missing_capacity <- setdiff(expected_capacity, names(capacity_tbl))

  if (length(missing_annual) > 0 || length(missing_quarterly) > 0 || length(missing_capacity) > 0) {
    stop(
      "Unexpected column layout in GCIM investment workbook. ",
      "Expected annual columns: ", paste(expected_annual, collapse = ", "), "; ",
      "found: ", paste(names(annual_tbl), collapse = ", "), ". ",
      "Expected quarterly columns: ", paste(expected_quarterly, collapse = ", "), "; ",
      "found: ", paste(names(quarterly_tbl), collapse = ", "), ". ",
      "Expected capacity columns: ", paste(expected_capacity, collapse = ", "), "; ",
      "found: ", paste(names(capacity_tbl), collapse = ", "), "."
    )
  }

  investment_momentum(
    annual_tbl = annual_tbl,
    capacity_tbl = capacity_tbl,
    quarterly_tbl = quarterly_tbl,
    momentum_window_years = momentum_window_years,
    capacity_year = capacity_year,
    gamma = gamma,
    country_reference = country_reference
  )
}
