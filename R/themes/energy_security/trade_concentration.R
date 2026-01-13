# Trade concentration theme builder functions.

# === Data preparation helpers ===
# These functions accept already-loaded data frames and apply the legacy
# transformations so that IO stays in scripts/ only.

# ---- Build energy trade code mapping ----
# The HS6 code categories are cleaned into a compact lookup table so that
# Comtrade (HS6) and Atlas (HS92) trade files can be matched to energy industries.
trade_concentration_build_energy_codes <- function(subcat) {
  pick_col <- function(tbl, candidates, label) {
    match <- intersect(candidates, names(tbl))
    if (length(match) == 0) {
      stop("Missing ", label, " column in trade codes file.")
    }
    match[[1]]
  }

  tech_col <- pick_col(subcat, c("tech", "Technology", "technology"), "tech")
  supply_chain_col <- pick_col(
    subcat,
    c("supply_chain", "Value.Chain", "value_chain", "Supply.Chain", "supply chain"),
    "supply_chain"
  )
  sub_sector_col <- pick_col(subcat, c("sub_sector", "Sub.Sector", "subsector"), "sub_sector")
  hs6_col <- pick_col(subcat, c("HS6", "hs6", "HS_6"), "HS6")

  subcat %>%
    dplyr::transmute(
      tech = as.character(.data[[tech_col]]),
      supply_chain = as.character(.data[[supply_chain_col]]),
      sub_sector = as.character(.data[[sub_sector_col]]),
      code6 = stringr::str_pad(as.character(.data[[hs6_col]]), width = 6, side = "left", pad = "0"),
      code4 = substr(code6, 1, 4)
    ) %>%
    dplyr::filter(!is.na(code6), nzchar(code6)) %>%
    dplyr::distinct()
}

trade_concentration_detect_year_column <- function(tbl, label = "trade data") {
  year_col <- intersect(c("period", "ref_year", "year", "Year"), names(tbl))
  if (length(year_col) == 0) {
    stop("Unable to locate year column in ", label, ".")
  }
  year_col[[1]]
}

trade_concentration_filter_year <- function(tbl, year, label = "trade data") {
  year_col <- trade_concentration_detect_year_column(tbl, label = label)
  tbl %>%
    dplyr::filter(as.character(.data[[year_col]]) == as.character(year))
}

# ---- Atlas market share (6-digit) ----
# Use the Atlas data only for market share values.
trade_concentration_build_aec_market_share <- function(aec_6_data, energy_codes, year = 2023) {
  aec_6_data %>%
    dplyr::filter(year == as.character(year)) %>%
    dplyr::left_join(energy_codes, by = c("product_hs92_code" = "code6")) %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(country_iso3_code, tech, supply_chain, sub_sector) %>%
    dplyr::summarize(
      market_share = stats::weighted.mean(global_market_share, w = export_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Atlas feasibility (4-digit) ----
# Use the Atlas data only for feasibility values derived from distance.
trade_concentration_build_aec_feasibility <- function(aec_4_data, energy_codes, year = 2022) {
  aec_4_data %>%
    dplyr::filter(year == as.character(year)) %>%
    dplyr::left_join(energy_codes, by = c("product_hs92_code" = "code4")) %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(country_iso3_code, tech, supply_chain, sub_sector) %>%
    dplyr::summarize(
      feasibility = 1 - mean(distance, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Comtrade trade aggregation ----
# Aggregate Comtrade exports/imports to the technology level.
trade_concentration_build_comtrade_trade <- function(comtrade_data,
                                                     energy_codes,
                                                     year = 2024) {
  comtrade_data <- trade_concentration_filter_year(
    comtrade_data,
    year = year,
    label = "Comtrade energy trade data"
  )

  if ("partner_desc" %in% names(comtrade_data)) {
    comtrade_data <- comtrade_data %>%
      dplyr::filter(partner_desc == "World")
  } else if ("partner_iso" %in% names(comtrade_data)) {
    comtrade_data <- comtrade_data %>%
      dplyr::filter(partner_iso == "World")
  }

  comtrade_data %>%
    dplyr::mutate(
      cmd_code = stringr::str_pad(as.character(cmd_code), width = 6, side = "left", pad = "0"),
      flow_direction = stringr::str_to_lower(flow_direction)
    ) %>%
    dplyr::filter(flow_direction %in% c("export", "import")) %>%
    dplyr::left_join(energy_codes, by = c("cmd_code" = "code6")) %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(reporter_iso, tech, supply_chain, sub_sector, flow_direction) %>%
    dplyr::summarize(value = sum(primary_value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = flow_direction,
      values_from = value,
      values_fill = 0
    ) %>%
    dplyr::rename(exports = export, imports = import)
}

# ---- Comtrade RCA ----
trade_concentration_build_comtrade_rca <- function(comtrade_trade,
                                                   comtrade_total_export,
                                                   year = 2024) {
  total_export <- trade_concentration_filter_year(
    comtrade_total_export,
    year = year,
    label = "Comtrade total export data"
  )

  total_export <- total_export %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::summarize(total_exports = sum(primary_value, na.rm = TRUE), .groups = "drop")

  total_world <- total_export %>%
    dplyr::summarize(total_world_exports = sum(total_exports, na.rm = TRUE)) %>%
    dplyr::pull(total_world_exports)

  world_exports <- comtrade_trade %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::summarize(world_exports = sum(exports, na.rm = TRUE), .groups = "drop")

  comtrade_trade %>%
    dplyr::left_join(total_export, by = "reporter_iso") %>%
    dplyr::left_join(world_exports, by = c("tech", "supply_chain", "sub_sector")) %>%
    dplyr::mutate(
      share = exports / total_exports,
      world_share = world_exports / total_world,
      rca = share / world_share
    ) %>%
    dplyr::rename(export_rca = rca)
}

# ---- Comtrade HHI concentration ----
trade_concentration_build_comtrade_hhi <- function(comtrade_trade) {
  comtrade_trade %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::mutate(share_frac = exports / sum(exports, na.rm = TRUE)) %>%
    dplyr::summarize(HHI = sum(share_frac^2, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(HHI_index = median_scurve(-HHI))
}

# ---- Country-level trade aggregates ----
trade_concentration_build_country_trade <- function(comtrade_trade,
                                                    comtrade_total_export,
                                                    market_share,
                                                    feasibility,
                                                    gdp_data,
                                                    year = 2024) {
  comtrade_rca <- trade_concentration_build_comtrade_rca(
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    year = year
  )

  comtrade_rca %>%
    dplyr::mutate(
      exports = dplyr::coalesce(exports, 0),
      imports = dplyr::coalesce(imports, 0)
    ) %>%
    dplyr::left_join(
      market_share,
      by = c("reporter_iso" = "country_iso3_code", "tech", "supply_chain", "sub_sector")
    ) %>%
    dplyr::left_join(
      feasibility,
      by = c("reporter_iso" = "country_iso3_code", "tech", "supply_chain", "sub_sector")
    ) %>%
    dplyr::mutate(deficit = exports - imports) %>%
    dplyr::left_join(
      gdp_data %>%
        dplyr::filter(year == "2024") %>%
        dplyr::rename(gdp = NY.GDP.MKTP.CD) %>%
        dplyr::select(iso3c, gdp),
      by = c("reporter_iso" = "iso3c")
    ) %>%
    dplyr::mutate(deficit_gdp = deficit / gdp) %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::mutate(
      market_share_index = median_scurve(market_share),
      rca_index = median_scurve(export_rca),
      deficit_gdp_index = median_scurve(deficit_gdp)
    ) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(
      export_size_index = median_scurve(exports),
      feas_index = median_scurve(feasibility)
    ) %>%
    dplyr::ungroup()
}

# ---- Trade indices and HHI concentration ----
trade_concentration_build_indices <- function(country_trade, hhi_tbl, country_info) {
  trade_indices <- country_info %>%
    dplyr::select(country, iso3c) %>%
    dplyr::left_join(country_trade, by = c("iso3c" = "reporter_iso")) %>%
    dplyr::distinct()

  trade_indices <- trade_indices %>%
    dplyr::left_join(hhi_tbl, by = c("tech", "supply_chain", "sub_sector")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      overall_trade_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE),
      overall_trade_risk_index = mean(c(HHI_index, market_share_index, deficit_gdp_index), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(overall_trade_index))

  list(trade_indices = trade_indices, trade_indices_hhi = hhi_tbl)
}

# ---- Canonical tidy output ----
# Pivot raw values and indices into the shared schema used across themes.
trade_concentration_build_tidy <- function(trade_indices, year = 2023) {
  trade_indices %>%
    dplyr::rename(Country = country) %>%
    dplyr::mutate(tech = dplyr::if_else(tech == "Natural Gas", "Gas", tech)) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(c(
        "exports",
        "imports",
        "export_rca",
        "feasibility",
        "market_share",
        "deficit",
        "gdp",
        "deficit_gdp",
        "market_share_index",
        "rca_index",
        "export_size_index",
        "feas_index",
        "deficit_gdp_index",
        "HHI",
        "HHI_index",
        "overall_trade_index",
        "overall_trade_risk_index"
      )),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Trade",
      Year = year,
      source = dplyr::case_when(
        variable %in% c("market_share", "feasibility", "market_share_index", "feas_index") ~
          "Harvard Atlas of Economic Complexity",
        variable %in% c("gdp", "deficit_gdp", "deficit_gdp_index") ~
          "World Bank WDI; UN Comtrade",
        variable %in% c("overall_trade", "overall_trade_risk") ~ "Author calculation",
        TRUE ~ "UN Comtrade"
      ),
      variable = dplyr::case_when(
        variable == "overall_trade" ~ "Overall Trade Index",
        variable == "overall_trade_risk" ~ "Overall Trade Risk Index",
        TRUE ~ variable
      ),
      explanation = dplyr::case_when(
        variable %in% c("Overall Trade Index", "Overall Trade Risk Index") ~
          "Author calculation across trade indices",
        variable %in% c("market_share", "feasibility") ~
          "Atlas of Economic Complexity market share or feasibility value",
        data_type == "raw" ~ stringr::str_glue("{variable}: raw value from Comtrade or WDI"),
        data_type == "index" ~ stringr::str_glue("{variable}: percent-rank or HHI index"),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      sub_sector,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )
}

# === Public theme entrypoint ===
trade_concentration <- function(subcat,
                                aec_4_data,
                                aec_6_data,
                                comtrade_trade,
                                comtrade_total_export,
                                country_info,
                                gdp_data,
                                year_4 = 2022,
                                year_6 = 2023,
                                year_comtrade = 2024,
                                include_sub_sector = FALSE) {
  energy_codes <- trade_concentration_build_energy_codes(subcat)

  market_share <- trade_concentration_build_aec_market_share(
    aec_6_data,
    energy_codes,
    year = year_6
  )

  feasibility <- trade_concentration_build_aec_feasibility(
    aec_4_data,
    energy_codes,
    year = year_4
  )

  comtrade_trade <- trade_concentration_build_comtrade_trade(
    comtrade_trade,
    energy_codes,
    year = year_comtrade
  )

  country_trade <- trade_concentration_build_country_trade(
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    market_share = market_share,
    feasibility = feasibility,
    gdp_data = gdp_data,
    year = year_comtrade
  )

  hhi_tbl <- trade_concentration_build_comtrade_hhi(comtrade_trade)

  indices <- trade_concentration_build_indices(country_trade, hhi_tbl, country_info)

  trade_concentration_build_tidy(
    indices$trade_indices,
    year = year_comtrade
  ) %>%
    energy_security_add_overall_index(include_sub_sector = include_sub_sector)
}
