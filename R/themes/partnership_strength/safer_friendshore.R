# Safer friendshore theme (partner-level friendshoring index).

partnership_strength_build_import_indices <- function(ds_import) {
  ds_import %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      import_index = partnership_strength_safe_scurve(level_last),
      import_growth_idx = partnership_strength_safe_scurve(growth),
      imp_num = 2 * import_index + 1 * import_growth_idx,
      imp_den = 2 * (!is.na(import_index)) + 1 * (!is.na(import_growth_idx)),
      imp_trade_index = dplyr::if_else(imp_den > 0, imp_num / imp_den, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::mutate(imp_trade_index = partnership_strength_safe_scurve(imp_trade_index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(reporter_iso, partner_iso, tech, supply_chain, imp_trade_index)
}

partnership_strength_build_outbound_edges <- function(fdi_raw,
                                                      country_info,
                                                      gdp_data,
                                                      gdp_year = 2023) {
  require_columns(fdi_raw, c("COUNTRY", "COUNTERPART_COUNTRY"), label = "fdi_raw")
  require_columns(gdp_data, c("iso3c", "year", "NY.GDP.MKTP.CD"), label = "gdp_data")

  fdi_clean <- fdi_raw %>%
    dplyr::mutate(
      COUNTRY = partnership_strength_clean_outbound_countries(COUNTRY),
      COUNTERPART_COUNTRY = partnership_strength_clean_outbound_countries(COUNTERPART_COUNTRY)
    ) %>%
    dplyr::filter(
      !COUNTERPART_COUNTRY %in% c(
        "World",
        "Not Specified (including Confidential)",
        "Europe",
        "North and Central America",
        "Oceania and Polar Regions",
        "Other Near and Middle East Economies",
        "Economies of Persian Gulf",
        "Central and South Asia",
        "East Asia",
        "British Virgin Islands",
        "Cayman Islands",
        "Bermuda",
        "Luxembourg",
        "Bahamas",
        "Bahamas, The",
        "Barbados",
        "Turks and Caicos Islands",
        "Ireland",
        "Isle of Man",
        "Guernsey",
        "Cyprus",
        "Mauritius"
      ),
      DV_TYPE == "Reported official data",
      INDICATOR == "Outward Direct investment, Assets (gross), Debt instruments, All entities"
    ) %>%
    dplyr::select(COUNTRY, COUNTERPART_COUNTRY, dplyr::starts_with("X")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::starts_with("X")), na.rm = TRUE)) %>%
    dplyr::ungroup()

  gdp_latest <- gdp_data %>%
    dplyr::filter(as.character(year) == as.character(gdp_year)) %>%
    dplyr::rename(GDP = NY.GDP.MKTP.CD)

  fdi_clean %>%
    dplyr::left_join(country_info %>% dplyr::select(country, iso3c), by = c("COUNTRY" = "country")) %>%
    dplyr::rename(reporter_iso = iso3c) %>%
    dplyr::left_join(
      country_info %>% dplyr::select(country, iso3c),
      by = c("COUNTERPART_COUNTRY" = "country"),
      relationship = "many-to-many"
    ) %>%
    dplyr::rename(partner_iso = iso3c) %>%
    dplyr::filter(!is.na(reporter_iso), !is.na(partner_iso)) %>%
    dplyr::left_join(gdp_latest, by = c("reporter_iso" = "iso3c")) %>%
    dplyr::rename(GDP_reporter = GDP) %>%
    dplyr::left_join(gdp_latest, by = c("partner_iso" = "iso3c")) %>%
    dplyr::rename(GDP_partner = GDP) %>%
    dplyr::filter(!is.na(GDP_reporter), !is.na(GDP_partner)) %>%
    dplyr::mutate(
      GDP_cp = GDP_partner / 1e6,
      GDP = GDP_reporter / 1e6,
      total_counterpart_share = total / GDP_cp,
      outbound_relative = (total / GDP) / GDP_cp
    ) %>%
    dplyr::mutate(
      total_index = median_scurve(scales::rescale(total, to = c(0, 1))),
      total_share_index = median_scurve(scales::rescale(total_counterpart_share, to = c(0, 1))),
      outbound_relative_index = median_scurve(scales::rescale(outbound_relative, to = c(0, 1)))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(outbound_index = mean(total_index:outbound_relative_index)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(outbound_index = median_scurve(outbound_index)) %>%
    dplyr::ungroup() %>%
    dplyr::select(reporter_iso, partner_iso, outbound_index) %>%
    dplyr::distinct()
}

partnership_strength_build_friendshore_dyads <- function(import_indices,
                                                         econ_opp_index,
                                                         energy_security_index,
                                                         outbound_edges,
                                                         tech_ghg,
                                                         policy,
                                                         country_info,
                                                         component_weights = NULL) {
  default_weights <- list(
    imp_trade_index = 1,
    econ_opp_raw = 1,
    es_need = 2,
    eo_partner = 1.5,
    outbound_index = 1.5
  )
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    default_weights,
    "Friendshore component"
  )
  es_iso <- energy_security_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)) %>%
    dplyr::filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
      mutate(value=median_scurve(Energy_Security_Index)
    ) %>%
    dplyr::transmute(reporter_iso = iso3c, tech, supply_chain, es_need = 1 - value)

  eo_partner_iso <- econ_opp_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)) %>%
    dplyr::filter(!is.na(iso3c)) %>%
    group_by(tech,supply_chain) %>%
    mutate(value=median_scurve(Economic_Opportunity_Index)
    ) %>%
    dplyr::transmute(partner_iso = iso3c, tech, supply_chain, eo_partner = value)

  econ_opp_iso <- econ_opp_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      exporter_iso = partnership_strength_country_to_iso(Country, country_info)) %>%
    dplyr::filter(!is.na(exporter_iso)) %>%
    group_by(exporter_iso) %>%
    mutate(value=median_scurve(Economic_Opportunity_Index)
    ) %>%
    dplyr::transmute(exporter_iso, tech, supply_chain, econ_opp_raw = value)

  ghg_iso <- tech_ghg %>%
    dplyr::transmute(tech, ghg_index = as.numeric(ghg_index))

  policy_iso <- policy %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(!is.na(iso3c)) %>%
    dplyr::transmute(iso3c, climate_policy_index) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarize(climate_policy_index = mean(climate_policy_index, na.rm = TRUE), .groups = "drop")

  default_ghg <- if (nrow(ghg_iso)) median(ghg_iso$ghg_index, na.rm = TRUE) else 0.5
  default_policy <- if (nrow(policy_iso)) median(policy_iso$climate_policy_index, na.rm = TRUE) else 0.5

  import_indices %>%
    dplyr::left_join(es_iso, by = c("tech", "supply_chain", "reporter_iso")) %>%
    dplyr::left_join(econ_opp_iso, by = c("reporter_iso" = "exporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(eo_partner_iso, by = c("tech", "supply_chain", "partner_iso")) %>%
    dplyr::left_join(outbound_edges, by = c("reporter_iso", "partner_iso")) %>%
    dplyr::left_join(dplyr::rename(policy_iso, reporter_iso = iso3c, cpi_r = climate_policy_index), by = "reporter_iso") %>%
    dplyr::left_join(dplyr::rename(policy_iso, partner_iso = iso3c, cpi_p = climate_policy_index), by = "partner_iso") %>%
    dplyr::left_join(ghg_iso, by = "tech") %>%
    dplyr::mutate(
      cpi_r = dplyr::coalesce(cpi_r, default_policy),
      cpi_p = dplyr::coalesce(cpi_p, default_policy),
      gh_entry = dplyr::coalesce(ghg_index, default_ghg),
      outbound_index = dplyr::coalesce(outbound_index, 0)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      fsi_raw = partnership_strength_weighted_mean(
        c(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index),
        c(
          component_weights$imp_trade_index,
          component_weights$econ_opp_raw,
          component_weights$es_need,
          component_weights$eo_partner,
          component_weights$outbound_index
        )
      ),
      penalty_r = (1 - gh_entry) * cpi_r * 0.10,
      penalty_p = (1 - gh_entry) * cpi_p * 0.10,
      fsi_adj = pmax(0, fsi_raw - (penalty_r + penalty_p))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(friendshore_index = partnership_strength_safe_scurve(fsi_adj)) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(friendshore_index_country = partnership_strength_safe_scurve(fsi_adj)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      penalty_r,
      penalty_p,
      fsi_raw,
      fsi_adj,
      friendshore_index,
      friendshore_index_country
    )
}

partnership_strength_build_friendshore_country <- function(friendshore_all,
                                                           country_info,
                                                           top_n = 3,
                                                           year = 2025L) {
  friendshore_all %>%
    dplyr::filter(!is.na(friendshore_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = friendshore_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(value = mean(friendshore_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "friendshore",
      variable = "Friendshore Index",
      data_type = "index",
      value,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average of the top friendshore dyads by partner country."
    )
}

partnership_strength_build_friendshore_inputs_country <- function(friendshore_all,
                                                                  country_info,
                                                                  component_weights,
                                                                  top_n = 3,
                                                                  year = 2025L) {
  component_weights_tbl <- tibble::tibble(
    component_key = c("imp_trade_index", "econ_opp_raw", "es_need", "eo_partner", "outbound_index"),
    component_weight = c(
      component_weights$imp_trade_index,
      component_weights$econ_opp_raw,
      component_weights$es_need,
      component_weights$eo_partner,
      component_weights$outbound_index
    )
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  top_dyads <- friendshore_all %>%
    dplyr::filter(!is.na(friendshore_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = friendshore_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(partner_iso, reporter_iso, tech, supply_chain) %>%
    dplyr::distinct()

  if (nrow(top_dyads) == 0) {
    return(tibble::tibble(
      Country = character(),
      tech = character(),
      supply_chain = character(),
      category = character(),
      variable = character(),
      data_type = character(),
      value = numeric(),
      component_weight = numeric(),
      component_weight_share = numeric(),
      weighted_component = numeric(),
      Year = integer(),
      source = character(),
      explanation = character()
    ))
  }

  friendshore_all %>%
    dplyr::distinct(
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      .keep_all = TRUE
    ) %>%
    dplyr::inner_join(top_dyads, by = c("partner_iso", "reporter_iso", "tech", "supply_chain")) %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::select(
      Country,
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(weighted_component = value * component_weight_share) %>%
    dplyr::group_by(
      Country,
      tech,
      supply_chain,
      component_key,
      component_weight,
      component_weight_share
    ) %>%
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      weighted_component = mean(weighted_component, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      category = "friendshore",
      variable = component_key,
      data_type = "index",
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average component contribution across top friendshore dyads."
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      component_weight,
      component_weight_share,
      weighted_component,
      Year,
      source,
      explanation
    )
}

partnership_strength_build_friendshore_dyads_table <- function(friendshore_all,
                                                               country_info,
                                                               component_weights,
                                                               year = 2025L) {
  component_weights_tbl <- tibble::tibble(
    component_key = c("imp_trade_index", "econ_opp_raw", "es_need", "eo_partner", "outbound_index"),
    component_weight = c(
      component_weights$imp_trade_index,
      component_weights$econ_opp_raw,
      component_weights$es_need,
      component_weights$eo_partner,
      component_weights$outbound_index
    )
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  friendshore_all %>%
    dplyr::mutate(
      Reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      Partner = partnership_strength_iso_to_country(partner_iso, country_info),
      Country = Partner
    ) %>%
    dplyr::select(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      friendshore_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index, friendshore_index),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(
      variable = dplyr::if_else(component_key == "friendshore_index", "Friendshore Index", component_key),
      category = "friendshore",
      data_type = "index",
      weighted_component = dplyr::if_else(
        !is.na(component_weight_share),
        value * component_weight_share,
        NA_real_
      ),
      Year = as.integer(year),
      source = "Author calculation",
      explanation = dplyr::if_else(
        component_key == "friendshore_index",
        "Friendshore index per reporter-partner dyad.",
        "Component of the friendshore index per reporter-partner dyad."
      )
    ) %>%
    dplyr::select(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      component_weight,
      component_weight_share,
      weighted_component,
      Year,
      source,
      explanation
    )
}

safer_friendshore <- function(comtrade_dyads,
                              subcat,
                              econ_opp_index,
                              energy_security_index,
                              tech_ghg,
                              policy,
                              fdi_raw,
                              country_info,
                              gdp_data,
                              component_weights = NULL,
                              years = 2021:2025,
                              top_n = 3,
                              gdp_year = 2023) {
  res_tech <- partnership_strength_clean_trade_data(comtrade_dyads, subcat, years = years)
  ds_import <- partnership_strength_build_dyad_series(res_tech, flow = "import", years = years)
  import_indices <- partnership_strength_build_import_indices(ds_import)

  outbound_edges <- partnership_strength_build_outbound_edges(
    fdi_raw,
    country_info,
    gdp_data,
    gdp_year = gdp_year
  )

  friendshore_all <- partnership_strength_build_friendshore_dyads(
    import_indices,
    econ_opp_index,
    energy_security_index,
    outbound_edges,
    tech_ghg,
    policy,
    country_info,
    component_weights = component_weights
  )

  friendshore_dyads <- partnership_strength_build_friendshore_dyads_table(
    friendshore_all,
    country_info,
    component_weights = component_weights,
    year = max(years)
  )

  variable_tbl <- friendshore_dyads %>%
    dplyr::mutate(
      reporter = Reporter,
      partner = Partner
    ) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )

  contributions_tbl <- friendshore_dyads %>%
    dplyr::mutate(
      reporter = Reporter,
      partner = Partner,
      data_type = "contribution",
      contribution = dplyr::coalesce(weighted_component, value)
    ) %>%
    dplyr::filter(!is.na(contribution)) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      contribution,
      Year,
      source,
      explanation
    )

  top5_tbl <- variable_tbl %>%
    dplyr::group_by(reporter, partner, reporter_iso, partner_iso, category, variable) %>%
    dplyr::slice_max(order_by = value, n = 5, with_ties = FALSE) %>%
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      source = dplyr::first(source),
      explanation = dplyr::first(explanation),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      data_type = "index",
      Year = as.integer(max(years))
    ) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )

  list(
    variable = variable_tbl,
    contributions = contributions_tbl,
    top5 = top5_tbl
  )
}
