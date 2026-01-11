# Trade concentration theme builder functions.

# === Data preparation helpers ===
# These functions accept already-loaded data frames and apply the legacy
# transformations so that IO stays in scripts/ only.

# ---- Build energy trade code mapping ----
# The raw HS code categories are cleaned into a compact lookup table so that
# 4-digit and 6-digit trade files can be matched to energy industries.
trade_concentration_build_energy_codes <- function(subcat) {
  subcat %>%
    dplyr::mutate(
      industry = paste(Technology, `Value.Chain`),
      code = substr(HS6, 1, 4),
      code6 = stringr::str_extract(HS6, "^\\d{6}")
    ) %>%
    dplyr::select(industry, HS6, code, code6)
}

# ---- 4-digit trade aggregation ----
# Aggregate 4-digit HS trade flows to the industry level and compute market
# shares, RCA, and feasibility from the Atlas data.
trade_concentration_build_aec_4 <- function(aec_4_data, energy_codes, year = 2022) {
  aec_4_data %>%
    dplyr::filter(year == as.character(year)) %>%
    dplyr::inner_join(energy_codes, by = c("product_hs92_code" = "code")) %>%
    dplyr::distinct(
      country_iso3_code,
      industry,
      product_hs92_code,
      export_value,
      import_value,
      export_rca,
      distance
    ) %>%
    dplyr::group_by(country_iso3_code, industry) %>%
    dplyr::summarize(
      exports = sum(export_value, na.rm = TRUE),
      imports = sum(import_value, na.rm = TRUE),
      export_rca = stats::weighted.mean(export_rca, w = export_value, na.rm = TRUE),
      feasibility = 1 - mean(distance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(market_share = exports / sum(exports) * 100)
}

# ---- 6-digit trade aggregation ----
# Normalize 6-digit HS data to the same industry taxonomy and compute
# market-share-derived RCA metrics at the country level.
trade_concentration_build_aec_6 <- function(aec_6_data, energy_codes, year = 2023) {
  aec_6_data %>%
    dplyr::filter(year == as.character(year)) %>%
    dplyr::left_join(energy_codes, by = c("product_hs92_code" = "code6")) %>%
    dplyr::distinct(
      country_iso3_code,
      industry,
      product_hs92_code,
      export_value,
      import_value,
      global_market_share
    ) %>%
    dplyr::mutate(
      industry = stringr::str_replace_all(industry, " NA", " Downstream"),
      industry = stringr::str_replace_all(industry, "Battery", "Batteries")
    )
}

# ---- Global trade shares (6-digit) ----
trade_concentration_build_aec_6_world <- function(aec_6_data, energy_codes, year = 2023) {
  trade_concentration_build_aec_6(aec_6_data, energy_codes, year = year) %>%
    dplyr::group_by(industry) %>%
    dplyr::summarize(
      exports = sum(export_value, na.rm = TRUE),
      imports = sum(import_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(world_share = exports / sum(exports) * 100)
}

# ---- Country-level trade RCA (6-digit + 4-digit backfill) ----
# Merge 6-digit RCA with 4-digit industries that are not covered in the
# 6-digit file, and compute overall trade indices.
trade_concentration_build_country_rca <- function(aec_4_all,
                                                  aec_6_data,
                                                  energy_codes,
                                                  gdp_data,
                                                  year = 2023) {
  aec_6_world <- trade_concentration_build_aec_6_world(aec_6_data, energy_codes, year = year)

  aec_6_all <- trade_concentration_build_aec_6(aec_6_data, energy_codes, year = year) %>%
    dplyr::group_by(country_iso3_code, industry) %>%
    dplyr::summarize(
      exports = sum(export_value, na.rm = TRUE),
      imports = sum(import_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(country_iso3_code) %>%
    dplyr::mutate(country_share = exports / sum(exports) * 100) %>%
    dplyr::group_by(industry) %>%
    dplyr::mutate(market_share = exports / sum(exports) * 100) %>%
    dplyr::left_join(
      aec_6_world %>% dplyr::select(industry, world_share),
      by = "industry"
    ) %>%
    dplyr::mutate(rca = country_share / world_share)

  aec_6_all %>%
    dplyr::select(-country_share, -world_share) %>%
    dplyr::rename(export_rca = rca) %>%
    dplyr::bind_rows(
      aec_4_all %>%
        dplyr::filter(!industry %in% aec_6_all$industry) %>%
        dplyr::select(-feasibility)
    ) %>%
    dplyr::left_join(
      aec_4_all %>% dplyr::select(country_iso3_code, industry, feasibility),
      by = c("country_iso3_code", "industry")
    ) %>%
    dplyr::mutate(deficit = exports - imports) %>%
    dplyr::filter(!is.na(industry)) %>%
    dplyr::left_join(
      gdp_data %>%
        dplyr::filter(year == "2024") %>%
        dplyr::rename(gdp = NY.GDP.MKTP.CD) %>%
        dplyr::select(iso3c, gdp),
      by = c("country_iso3_code" = "iso3c")
    ) %>%
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
    dplyr::group_by(industry) %>%
    dplyr::mutate(overall_trade_index = median_scurve(overall_trade_index))
}

# ---- Trade indices and HHI concentration ----
trade_concentration_build_indices <- function(country_rca, country_info) {
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

  list(trade_indices = trade_indices, trade_indices_hhi = trade_indices_hhi)
}

# ---- Canonical tidy output ----
# Pivot raw values and indices into the shared schema used across themes.
trade_concentration_build_tidy <- function(trade_indices, trade_indices_hhi, year = 2023) {
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
      Year = year,
      source = "Harvard Atlas of Economic Complexity",
      variable = dplyr::if_else(variable == "overall_trade", "Overall Trade Index", variable),
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue("{variable}: raw value from Atlas"),
        data_type == "index" ~ stringr::str_glue("{variable}: percent-rank or HHI index"),
        TRUE ~ NA_character_
      )
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

# === Public theme entrypoint ===
trade_concentration <- function(subcat,
                                aec_4_data,
                                aec_6_data,
                                country_info,
                                gdp_data,
                                year_4 = 2022,
                                year_6 = 2023) {
  energy_codes <- trade_concentration_build_energy_codes(subcat)

  aec_4_all <- trade_concentration_build_aec_4(
    aec_4_data,
    energy_codes,
    year = year_4
  )

  country_rca <- trade_concentration_build_country_rca(
    aec_4_all,
    aec_6_data,
    energy_codes,
    gdp_data,
    year = year_6
  )

  indices <- trade_concentration_build_indices(country_rca, country_info)

  trade_concentration_build_tidy(
    indices$trade_indices,
    indices$trade_indices_hhi,
    year = year_6
  ) %>%
    energy_security_add_overall_index()
}
