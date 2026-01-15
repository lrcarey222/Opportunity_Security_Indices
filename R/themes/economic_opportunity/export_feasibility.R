# Export feasibility (trade) theme builder functions.

export_feasibility_build_energy_codes <- function(subcat) {
  energy_codes <- trade_concentration_build_energy_codes(subcat)
  require_columns(energy_codes, c("code6", "code4", "tech", "supply_chain"), label = "energy_codes")

  energy_codes <- energy_codes %>%
    dplyr::mutate(
      supply_chain = stringr::str_to_title(as.character(supply_chain)),
      industry = paste(tech, supply_chain)
    ) %>%
    dplyr::rename(code = code4) %>%
    dplyr::select(code6, code, tech, supply_chain, industry) %>%
    dplyr::distinct()

  duplicate_code6 <- energy_codes %>%
    dplyr::count(code6, name = "n") %>%
    dplyr::filter(n > 1)
  if (nrow(duplicate_code6) > 0) {
    warning(
      "Energy trade code mapping has multiple industries for HS6 codes; ",
      "using the first industry per code6."
    )
    energy_codes <- energy_codes %>%
      dplyr::arrange(code6, tech, supply_chain) %>%
      dplyr::group_by(code6) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  }

  assert_unique_keys(energy_codes, "code6", label = "energy_codes")

  duplicate_codes <- energy_codes %>%
    dplyr::count(code, name = "n") %>%
    dplyr::filter(n > 1)
  if (nrow(duplicate_codes) > 0) {
    stop("Energy trade code mapping has multiple industries for 4-digit codes; review subcat.")
  }

  energy_codes
}

export_feasibility_build_aec_4 <- function(aec_4_data,
                                           energy_codes,
                                           year_4 = 2022L) {
  require_columns(
    aec_4_data,
    c("year", "country_iso3_code", "product_hs92_code", "export_value", "import_value", "export_rca", "distance"),
    label = "aec_4_data"
  )

  energy_codes_4 <- energy_codes %>% dplyr::select(code, industry)
  assert_unique_keys(energy_codes_4, "code", label = "energy_codes_4")

  aec_4_clean <- aec_4_data %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(year == as.integer(year_4))

  if (nrow(aec_4_clean) == 0) {
    stop("AEC 4-digit data missing year: ", year_4)
  }

  aec_4_clean %>%
    dplyr::inner_join(energy_codes_4, by = c("product_hs92_code" = "code")) %>%
    dplyr::group_by(country_iso3_code, industry) %>%
    dplyr::summarize(
      exports = sum(export_value, na.rm = TRUE),
      imports = sum(import_value, na.rm = TRUE),
      export_rca = stats::weighted.mean(export_rca, w = export_value, na.rm = TRUE),
      feasibility = 1 - mean(distance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(market_share = exports / sum(exports, na.rm = TRUE) * 100) %>%
    dplyr::ungroup()
}

export_feasibility_build_aec_6 <- function(aec_6_data,
                                           energy_codes,
                                           year_6 = 2023L) {
  require_columns(
    aec_6_data,
    c("year", "country_iso3_code", "product_hs92_code", "export_value", "import_value", "global_market_share"),
    label = "aec_6_data"
  )

  energy_codes_6 <- energy_codes %>% dplyr::select(code6, industry)
  assert_unique_keys(energy_codes_6, "code6", label = "energy_codes_6")

  aec_6_clean <- aec_6_data %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(year == as.integer(year_6)) %>%
    dplyr::left_join(energy_codes_6, by = c("product_hs92_code" = "code6")) %>%
    dplyr::filter(!is.na(industry)) %>%
    dplyr::mutate(
      industry = stringr::str_replace_all(industry, " NA", " Downstream"),
      industry = stringr::str_replace_all(industry, "Battery", "Batteries")
    )

  if (nrow(aec_6_clean) == 0) {
    stop("AEC 6-digit data missing year: ", year_6)
  }

  aec_6_world <- aec_6_clean %>%
    dplyr::group_by(industry) %>%
    dplyr::summarize(
      exports = sum(export_value, na.rm = TRUE),
      imports = sum(import_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(world_share = exports / sum(exports, na.rm = TRUE) * 100)

  aec_6_all <- aec_6_clean %>%
    dplyr::group_by(country_iso3_code, industry) %>%
    dplyr::summarize(
      exports = sum(export_value, na.rm = TRUE),
      imports = sum(import_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(country_iso3_code) %>%
    dplyr::mutate(country_share = exports / sum(exports, na.rm = TRUE) * 100) %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(market_share = exports / sum(exports, na.rm = TRUE) * 100) %>%
    dplyr::left_join(aec_6_world %>% dplyr::select(industry, world_share), by = "industry") %>%
    dplyr::mutate(rca = country_share / world_share)

  list(aec_6_all = aec_6_all, aec_6_world = aec_6_world)
}

export_feasibility_build_country_rca <- function(aec_6_all,
                                                 aec_4_all,
                                                 gdp_data,
                                                 gdp_year = 2024L) {
  require_columns(aec_6_all, c("country_iso3_code", "industry", "exports", "imports", "rca", "market_share"))
  require_columns(aec_4_all, c("country_iso3_code", "industry", "exports", "imports", "export_rca", "feasibility"))
  require_columns(gdp_data, c("iso3c", "year", "NY.GDP.MKTP.CD"), label = "gdp_data")

  gdp_clean <- gdp_data %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(year == as.integer(gdp_year)) %>%
    dplyr::select(iso3c, gdp = NY.GDP.MKTP.CD)

  assert_unique_keys(gdp_clean, "iso3c", label = "gdp_clean")

  aec_6_bind <- aec_6_all %>%
    dplyr::select(-country_share, -world_share) %>%
    dplyr::rename(export_rca = rca)

  aec_4_bind <- aec_4_all %>%
    dplyr::filter(!industry %in% aec_6_all$industry) %>%
    dplyr::select(-feasibility)

  bind_inputs <- lapply(list(aec_6_bind, aec_4_bind), standardize_bind_rows_inputs)

  dplyr::bind_rows(bind_inputs) %>%
    dplyr::left_join(
      aec_4_all %>% dplyr::select(country_iso3_code, industry, feasibility),
      by = c("country_iso3_code", "industry")
    ) %>%
    dplyr::mutate(deficit = exports - imports) %>%
    dplyr::left_join(gdp_clean, by = c("country_iso3_code" = "iso3c")) %>%
    dplyr::mutate(deficit_gdp = deficit / gdp) %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(
      market_share_index = median_scurve(market_share),
      rca_index = median_scurve(export_rca)
    ) %>%
    dplyr::group_by(country_iso3_code) %>%
    dplyr::mutate(
      export_size_index = median_scurve(exports),
      feas_index = median_scurve(feasibility)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(overall_trade_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(overall_trade_index = median_scurve(overall_trade_index)) %>%
    dplyr::ungroup()
}

export_feasibility_build_trade_indices <- function(country_rca, country_info) {
  require_columns(country_rca, c("country_iso3_code", "industry"))
  require_columns(country_info, c("iso3c", "country"), label = "country_info")
  assert_unique_keys(country_info, "iso3c", label = "country_info")

  trade_indices <- country_info %>%
    dplyr::select(country, iso3c) %>%
    dplyr::left_join(country_rca, by = c("iso3c" = "country_iso3_code")) %>%
    dplyr::arrange(dplyr::desc(overall_trade_index)) %>%
    tidyr::separate(
      col = industry,
      into = c("tech", "supply_chain"),
      sep = " (?=Downstream|Midstream|Upstream$)"
    ) %>%
    dplyr::distinct()

  trade_indices_hhi <- trade_indices %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      share_frac = market_share / 100,
      HHI = sum(share_frac^2, na.rm = TRUE)
    ) %>%
    dplyr::select(-share_frac) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(HHI_index = median_scurve(-HHI)) %>%
    dplyr::distinct(tech, supply_chain, HHI, HHI_index)

  assert_unique_keys(trade_indices_hhi, c("tech", "supply_chain"), label = "trade_indices_hhi")

  list(trade_indices = trade_indices, trade_indices_hhi = trade_indices_hhi)
}

export_feasibility_build_trade_tidy <- function(trade_indices,
                                                trade_indices_hhi,
                                                year = 2023L) {
  trade_indices %>%
    dplyr::rename(Country = country) %>%
    dplyr::mutate(tech = dplyr::if_else(tech == "Natural Gas", "Gas", tech)) %>%
    dplyr::left_join(trade_indices_hhi, by = c("tech", "supply_chain")) %>%
    tidyr::pivot_longer(
      cols = exports:HHI_index,
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Trade",
      source = "Harvard Atlas of Economic Complexity",
      variable = dplyr::if_else(variable == "overall_trade", "Overall Trade Index", variable),
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue("{variable}: raw value from Atlas"),
        data_type == "index" ~ stringr::str_glue("{variable}: percent-rank or HHI index"),
        TRUE ~ variable
      ),
      Year = as.integer(year),
      supply_chain = as.character(supply_chain)
    ) %>%
    dplyr::select(
      Country,
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
}

export_feasibility <- function(aec_4_data,
                               aec_6_data,
                               subcat,
                               country_info,
                               gdp_data,
                               year_4 = 2022L,
                               year_6 = 2023L,
                               gdp_year = 2024L) {
  energy_codes <- export_feasibility_build_energy_codes(subcat)
  aec_4_all <- export_feasibility_build_aec_4(aec_4_data, energy_codes, year_4 = year_4)
  aec_6_results <- export_feasibility_build_aec_6(aec_6_data, energy_codes, year_6 = year_6)
  country_rca <- export_feasibility_build_country_rca(
    aec_6_results$aec_6_all,
    aec_4_all,
    gdp_data,
    gdp_year = gdp_year
  )
  trade_indices <- export_feasibility_build_trade_indices(country_rca, country_info)

  trade_tidy <- export_feasibility_build_trade_tidy(
    trade_indices = trade_indices$trade_indices,
    trade_indices_hhi = trade_indices$trade_indices_hhi,
    year = max(as.integer(year_4), as.integer(year_6))
  )

  output <- standardize_theme_table(trade_tidy)
  validate_schema(output)
  output
}
