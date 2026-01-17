# Stronger development theme (outbound investment ties).

partnership_strength_build_development_country <- function(outbound_edges,
                                                           trade_pairs,
                                                           country_info,
                                                           top_n = 3,
                                                           year = 2024L) {
  expanded <- outbound_edges %>%
    tidyr::crossing(trade_pairs)

  expanded %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = outbound_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(value = mean(outbound_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "development",
      variable = "Outbound Investment Index",
      data_type = "index",
      value,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average outbound investment ties across top counterparties."
    )
}

stronger_development <- function(comtrade_dyads,
                                 subcat,
                                 fdi_raw,
                                 country_info,
                                 gdp_data,
                                 years = 2020:2024,
                                 top_n = 3,
                                 gdp_year = 2023) {
  res_tech <- partnership_strength_clean_trade_data(comtrade_dyads, subcat, years = years)
  trade_pairs <- res_tech %>%
    dplyr::distinct(tech, supply_chain)

  outbound_edges <- partnership_strength_build_outbound_edges(
    fdi_raw,
    country_info,
    gdp_data,
    gdp_year = gdp_year
  )

  development_country <- partnership_strength_build_development_country(
    outbound_edges,
    trade_pairs,
    country_info,
    top_n = top_n,
    year = max(years)
  )

  standardized <- partnership_strength_standardize_bind_rows(development_country)
  partnership_strength_validate_schema(standardized, label = "stronger_development")
  standardized
}
