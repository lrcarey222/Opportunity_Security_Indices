# Trade category core computations (Comtrade + Atlas hybrid pipeline).

trade_core_build_energy_codes <- function(subcat, include_sub_sector = FALSE) {
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
    dplyr::mutate(
      sub_sector = if (isTRUE(include_sub_sector)) dplyr::coalesce(sub_sector, "All") else "All"
    ) %>%
    dplyr::distinct()
}

trade_core_detect_year_column <- function(tbl, label = "trade data") {
  year_col <- intersect(c("period", "ref_year", "year", "Year"), names(tbl))
  if (length(year_col) == 0) {
    stop("Unable to locate year column in ", label, ".")
  }
  year_col[[1]]
}

trade_core_filter_year <- function(tbl, year, label = "trade data") {
  year_col <- trade_core_detect_year_column(tbl, label = label)
  tbl %>%
    dplyr::filter(as.character(.data[[year_col]]) == as.character(year))
}

trade_core_build_aec_market_share <- function(aec_6_data, energy_codes, year = 2023) {
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

trade_core_build_aec_feasibility <- function(aec_4_data, energy_codes, year = 2022) {
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

trade_core_build_comtrade_trade <- function(comtrade_data,
                                            energy_codes,
                                            year = 2024) {
  comtrade_data <- trade_core_filter_year(
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

trade_core_build_comtrade_rca <- function(comtrade_trade,
                                          comtrade_total_export,
                                          year = 2024) {
  total_export <- trade_core_filter_year(
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

trade_core_build_comtrade_hhi <- function(comtrade_trade) {
  comtrade_trade %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::mutate(share_frac = exports / sum(exports, na.rm = TRUE)) %>%
    dplyr::summarize(HHI = sum(share_frac^2, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(HHI_index = median_scurve(-HHI))
}

trade_core_build_country_trade <- function(comtrade_trade,
                                           comtrade_total_export,
                                           market_share,
                                           feasibility,
                                           gdp_data,
                                           year = 2024) {
  comtrade_rca <- trade_core_build_comtrade_rca(
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

trade_core_build_indices <- function(country_trade, hhi_tbl, country_info) {
  trade_indices <- country_info %>%
    dplyr::select(country, iso3c) %>%
    dplyr::left_join(country_trade, by = c("iso3c" = "reporter_iso")) %>%
    dplyr::distinct()

  trade_indices <- trade_indices %>%
    dplyr::left_join(hhi_tbl, by = c("tech", "supply_chain", "sub_sector")) %>%
    dplyr::arrange(dplyr::desc(export_size_index))

  list(trade_indices = trade_indices, trade_indices_hhi = hhi_tbl)
}

trade_core_build_tidy <- function(trade_indices, year = 2023, include_sub_sector = FALSE) {
  trade_indices %>%
    dplyr::rename(Country = country) %>%
    dplyr::mutate(
      tech = dplyr::if_else(tech == "Natural Gas", "Gas", tech),
      sub_sector = if (isTRUE(include_sub_sector)) {
        dplyr::coalesce(as.character(sub_sector), "All")
      } else {
        "All"
      }
    ) %>%
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
        "HHI_index"
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
        TRUE ~ "UN Comtrade"
      ),
      explanation = dplyr::case_when(
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

trade_category_build_trade <- function(subcat,
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
  energy_codes <- trade_core_build_energy_codes(subcat, include_sub_sector = include_sub_sector)

  market_share <- trade_core_build_aec_market_share(
    aec_6_data,
    energy_codes,
    year = year_6
  )

  feasibility <- trade_core_build_aec_feasibility(
    aec_4_data,
    energy_codes,
    year = year_4
  )

  comtrade_trade <- trade_core_build_comtrade_trade(
    comtrade_trade,
    energy_codes,
    year = year_comtrade
  )

  country_trade <- trade_core_build_country_trade(
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    market_share = market_share,
    feasibility = feasibility,
    gdp_data = gdp_data,
    year = year_comtrade
  )

  hhi_tbl <- trade_core_build_comtrade_hhi(comtrade_trade)

  indices <- trade_core_build_indices(country_trade, hhi_tbl, country_info)

  trade_core_build_tidy(
    indices$trade_indices,
    year = year_comtrade,
    include_sub_sector = include_sub_sector
  ) %>%
    energy_security_add_overall_index(include_sub_sector = include_sub_sector)
}
