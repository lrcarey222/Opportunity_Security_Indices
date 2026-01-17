# Prosperous opportunity theme (partner-level opportunity index).

partnership_strength_clean_trade_data <- function(comtrade_dyads, subcat, years = 2020:2024) {
  require_columns(
    comtrade_dyads,
    c("reporter_iso", "partner_iso"),
    label = "comtrade_dyads"
  )

  flow_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("flow_direction", "trade_flow", "flow"),
    "flow"
  )
  period_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("period", "year", "Year", "ref_year"),
    "period"
  )
  value_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("primary_value", "trade_value", "value"),
    "trade value"
  )
  code_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("cmd_code", "code", "hs6"),
    "HS6 code"
  )

  mapping <- partnership_strength_build_trade_mapping(subcat)

  comtrade_dyads %>%
    dplyr::transmute(
      reporter_iso = as.character(reporter_iso),
      partner_iso = as.character(partner_iso),
      period = suppressWarnings(as.integer(.data[[period_col]])),
      flow_direction = stringr::str_to_lower(as.character(.data[[flow_col]])),
      cmd_code = stringr::str_pad(as.character(.data[[code_col]]), width = 6, side = "left", pad = "0"),
      primary_value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    dplyr::left_join(mapping, by = c("cmd_code" = "code"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(period), period %in% years) %>%
    dplyr::select(reporter_iso, partner_iso, period, flow_direction, tech, supply_chain, primary_value)
}

partnership_strength_build_dyad_series <- function(res_tech, flow = c("export", "import"), years = 2020:2024) {
  flow <- match.arg(flow)

  res_tech %>%
    dplyr::filter(flow_direction == flow, !is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(reporter_iso, partner_iso, tech, supply_chain, period) %>%
    dplyr::summarise(val = sum(primary_value, na.rm = TRUE), .groups = "drop_last") %>%
    tidyr::complete(period = c(min(years), max(years)), fill = list(val = 0)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = period, values_from = val, names_prefix = "y", values_fill = 0) %>%
    dplyr::transmute(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      level_last = .data[[paste0("y", max(years))]],
      level_first = .data[[paste0("y", min(years))]],
      growth = dplyr::if_else(level_first > 0, (level_last / level_first) - 1, NA_real_)
    )
}

partnership_strength_build_export_indices <- function(ds_export) {
  ds_export %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      export_index = partnership_strength_safe_scurve(level_last),
      export_growth_index = partnership_strength_safe_scurve(growth),
      ti_num = 2 * export_index + 1 * export_growth_index,
      ti_den = 2 * (!is.na(export_index)) + 1 * (!is.na(export_growth_index)),
      trade_index_raw = dplyr::if_else(ti_den > 0, ti_num / ti_den, NA_real_)
    ) %>%
    dplyr::mutate(trade_index = partnership_strength_safe_scurve(trade_index_raw)) %>%
    dplyr::ungroup() %>%
    dplyr::select(reporter_iso, partner_iso, tech, supply_chain, trade_index)
}

partnership_strength_build_opportunity_dyads <- function(trade_indices,
                                                         econ_opp_index,
                                                         energy_security_index,
                                                         tech_ghg,
                                                         policy,
                                                         country_info) {
  econ_opp_iso <- econ_opp_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      exporter_iso = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(grepl("Overall\\s*Economic\\s*Opportunity", variable, ignore.case = TRUE)) %>%
    dplyr::transmute(exporter_iso, tech, supply_chain, econ_opp_raw = value)

  energy_sec_iso <- energy_security_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      partner_iso = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(grepl("Overall\\s*Energy\\s*Security", variable, ignore.case = TRUE)) %>%
    dplyr::transmute(partner_iso, tech, supply_chain, energy_sec_raw = 1 - value)

  ghg_iso <- tech_ghg %>%
    dplyr::transmute(tech, ghg_index = as.numeric(ghg_index))

  policy_iso <- policy %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::transmute(iso3c, climate_policy_index)

  default_ghg <- if (nrow(ghg_iso)) median(ghg_iso$ghg_index, na.rm = TRUE) else 0.5
  default_policy <- if (nrow(policy_iso)) median(policy_iso$climate_policy_index, na.rm = TRUE) else 0.5

  trade_indices %>%
    dplyr::left_join(econ_opp_iso, by = c("reporter_iso" = "exporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(energy_sec_iso, by = c("partner_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(ghg_iso, by = "tech") %>%
    dplyr::left_join(policy_iso, by = c("partner_iso" = "iso3c")) %>%
    dplyr::filter(!is.na(econ_opp_raw), !is.na(energy_sec_raw)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      econ_opp_index = partnership_strength_safe_scurve(econ_opp_raw),
      energy_security_index = partnership_strength_safe_scurve(energy_sec_raw),
      ghg_index = dplyr::coalesce(ghg_index, default_ghg),
      climate_policy_index = dplyr::coalesce(climate_policy_index, default_policy)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      o_num = 2 * trade_index + 2 * econ_opp_index + energy_security_index,
      o_den = 2 * (!is.na(trade_index)) + 2 * (!is.na(econ_opp_index)) + 1 * (!is.na(energy_security_index)),
      opportunity_raw = dplyr::if_else(o_den > 0, o_num / o_den, NA_real_),
      penalty = (1 - ghg_index) * climate_policy_index * 0.20,
      opportunity_index_raw = pmax(0, opportunity_raw - penalty)
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(opportunity_index = partnership_strength_safe_scurve(opportunity_index_raw)) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(opportunity_index_country = partnership_strength_safe_scurve(opportunity_index_raw)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      trade_index,
      econ_opp_index,
      energy_security_index,
      ghg_index,
      climate_policy_index,
      penalty,
      opportunity_raw,
      opportunity_index_raw,
      opportunity_index,
      opportunity_index_country
    )
}

partnership_strength_build_opportunity_country <- function(opportunity_all,
                                                           country_info,
                                                           top_n = 3,
                                                           year = 2024L) {
  opportunity_all %>%
    dplyr::filter(!is.na(opportunity_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = opportunity_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(value = mean(opportunity_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "opportunity",
      variable = "Opportunity Index",
      data_type = "index",
      value,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average of the top opportunity dyads by partner country."
    )
}

prosperous_opportunity <- function(comtrade_dyads,
                                   subcat,
                                   econ_opp_index,
                                   energy_security_index,
                                   tech_ghg,
                                   policy,
                                   country_info,
                                   years = 2020:2024,
                                   top_n = 3) {
  res_tech <- partnership_strength_clean_trade_data(comtrade_dyads, subcat, years = years)
  ds_export <- partnership_strength_build_dyad_series(res_tech, flow = "export", years = years)
  trade_indices <- partnership_strength_build_export_indices(ds_export)
  opportunity_all <- partnership_strength_build_opportunity_dyads(
    trade_indices,
    econ_opp_index,
    energy_security_index,
    tech_ghg,
    policy,
    country_info
  )

  opportunity_country <- partnership_strength_build_opportunity_country(
    opportunity_all,
    country_info,
    top_n = top_n,
    year = max(years)
  )

  standardized <- partnership_strength_standardize_bind_rows(opportunity_country)
  partnership_strength_validate_schema(standardized, label = "prosperous_opportunity")
  standardized
}
