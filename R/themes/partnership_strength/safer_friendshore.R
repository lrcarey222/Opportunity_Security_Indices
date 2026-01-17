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
                                                         country_info) {
  es_iso <- energy_security_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(grepl("Overall\\s*Energy\\s*Security", variable, ignore.case = TRUE)) %>%
    dplyr::transmute(reporter_iso = iso3c, tech, supply_chain, es_need = 1 - value)

  eo_partner_iso <- econ_opp_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(grepl("Overall\\s*Economic\\s*Opportunity", variable, ignore.case = TRUE)) %>%
    dplyr::transmute(partner_iso = iso3c, tech, supply_chain, eo_partner = value)

  econ_opp_iso <- econ_opp_index %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      exporter_iso = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(grepl("Overall\\s*Economic\\s*Opportunity", variable, ignore.case = TRUE)) %>%
    dplyr::transmute(exporter_iso, tech, supply_chain, econ_opp_raw = value)

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
    dplyr::mutate(
      fsi_raw = imp_trade_index + econ_opp_raw + 2 * es_need + 1.5 * eo_partner + 1.5 * outbound_index,
      penalty_r = (1 - gh_entry) * cpi_r * 0.10,
      penalty_p = (1 - gh_entry) * cpi_p * 0.10,
      fsi_adj = pmax(0, fsi_raw - (penalty_r + penalty_p))
    ) %>%
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
                                                           year = 2024L) {
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

safer_friendshore <- function(comtrade_dyads,
                              subcat,
                              econ_opp_index,
                              energy_security_index,
                              tech_ghg,
                              policy,
                              fdi_raw,
                              country_info,
                              gdp_data,
                              years = 2020:2024,
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
    country_info
  )

  friendshore_country <- partnership_strength_build_friendshore_country(
    friendshore_all,
    country_info,
    top_n = top_n,
    year = max(years)
  )

  standardized <- partnership_strength_standardize_bind_rows(friendshore_country)
  partnership_strength_validate_schema(standardized, label = "safer_friendshore")
  standardized
}
