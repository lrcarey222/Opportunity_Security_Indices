# Critical minerals trade theme builder functions.

# === Critical minerals trade (UN Comtrade) ===
# These builders mirror the legacy UN Comtrade pipeline for critical minerals
# and return the tidy, canonical schema.

# ---- Build mineral regex for commodity parsing ----
# The legacy pipeline extracts mineral names from Comtrade descriptions using a
# regex built from the IEA mineral demand list.
critical_minerals_trade_build_minerals_pattern <- function(mineral_demand_clean) {
  minerals <- unique(mineral_demand_clean$Mineral)
  paste0(paste0(minerals, collapse = "|"), "|", "Graphite")
}

# ---- Merge import/export flows ----
# Combine import/export values, map reporter ISO3 to country names, and derive
# the mineral label from the commodity description.
critical_minerals_trade_build_trade <- function(critmin_import,
                                                critmin_export,
                                                country_info,
                                                minerals_pattern) {
  critmin_import %>%
    dplyr::select(reporter_iso, reporter_desc, cmd_code, cmd_desc, primary_value) %>%
    dplyr::rename(imports = primary_value) %>%
    dplyr::left_join(
      critmin_export %>%
        dplyr::select(reporter_iso, reporter_desc, cmd_code, cmd_desc, primary_value) %>%
        dplyr::rename(exports = primary_value),
      by = c("reporter_iso", "reporter_desc", "cmd_code", "cmd_desc")
    ) %>%
    dplyr::left_join(country_info %>% dplyr::select(iso3c, country), by = c("reporter_iso" = "iso3c")) %>%
    dplyr::select(-reporter_desc) %>%
    dplyr::mutate(
      imports = dplyr::if_else(is.na(imports), 0, imports),
      exports = dplyr::if_else(is.na(exports), 0, exports),
      trade_balance = (exports - imports) / (exports + imports),
      mineral = stringr::str_to_sentence(
        stringr::str_extract(cmd_desc, stringr::regex(minerals_pattern, ignore_case = TRUE))
      ),
      supply_chain = "Upstream"
    )
}

# ---- Compute market shares and HHI ----
# Build the mineral-level trade indices (market share, export share, HHI).
critical_minerals_trade_build_hhi <- function(critmin_trade, total_export, gamma = 0.5) {
  critmin_trade %>%
    dplyr::group_by(reporter_iso, country, mineral, supply_chain) %>%
    dplyr::summarize(exports = sum(exports, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(
      total_export %>% dplyr::select(reporter_iso, total_exports = primary_value),
      by = "reporter_iso"
    ) %>%
    dplyr::mutate(exp_share = exports / total_exports) %>%
    dplyr::group_by(mineral, supply_chain) %>%
    dplyr::mutate(
      market_share = exports / sum(exports, na.rm = TRUE),
      share_frac_24 = exports / sum(exports, na.rm = TRUE),
      HHI_24 = sum(share_frac_24^2, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hhi_index = median_scurve(-HHI_24, gamma = gamma))
}

# ---- Roll up to technology-level trade indices ----
critical_minerals_trade_build_tech <- function(critmin_hhi, mineral_demand_clean, gamma = 0.5) {
  critmin_hhi %>%
    dplyr::group_by(mineral, supply_chain) %>%
    dplyr::mutate(
      criticalmineral_marketshare_index = median_scurve(market_share, gamma = gamma),
      criticalmineral_exportshare_index = median_scurve(exp_share, gamma = gamma),
      criticalmineral_hhi_index = hhi_index
    ) %>%
    dplyr::left_join(
      mineral_demand_clean %>%
        dplyr::mutate(Mineral = dplyr::if_else(stringr::str_detect(Mineral, "graphite", ignore_case = TRUE), "Graphite", Mineral)) %>%
        dplyr::ungroup() %>%
        dplyr::select(Mineral, tech, share_24),
      by = c("mineral" = "Mineral")
    ) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::mutate(share_24 = share_24 / sum(share_24)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::summarize(
      criticalmineral_marketshare = stats::weighted.mean(market_share, share_24, na.rm = TRUE),
      criticalmineral_marketshare_index = stats::weighted.mean(
        criticalmineral_marketshare_index,
        share_24,
        na.rm = TRUE
      ),
      criticalmineral_exportshare_index = stats::weighted.mean(
        criticalmineral_exportshare_index,
        share_24,
        na.rm = TRUE
      ),
      criticalmineral_hhi_index = stats::weighted.mean(hhi_index, share_24, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      critmin_trade_index = criticalmineral_marketshare_index
    )
}

# ---- Canonical tidy output ----
critical_minerals_trade_build_tidy <- function(critmin_trade_tech, year = 2024) {
  critmin_trade_tech %>%
    dplyr::select(country, tech, supply_chain, critmin_trade_index) %>%
    tidyr::pivot_longer(
      cols = -c(country, tech, supply_chain),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      category = "Trade",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = year,
      source = "UN Comtrade",
      explanation = "UN Comtrade export data"
    ) %>%
    dplyr::rename(Country = country) %>%
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
critical_minerals_trade <- function(critmin_import,
                                    critmin_export,
                                    total_export,
                                    mineral_demand_clean,
                                    country_info,
                                    year = 2024,
                                    gamma = 0.5) {
  minerals_pattern <- critical_minerals_trade_build_minerals_pattern(mineral_demand_clean)

  critmin_trade <- critical_minerals_trade_build_trade(
    critmin_import = critmin_import,
    critmin_export = critmin_export,
    country_info = country_info,
    minerals_pattern = minerals_pattern
  )

  critmin_hhi <- critical_minerals_trade_build_hhi(
    critmin_trade = critmin_trade,
    total_export = total_export,
    gamma = gamma
  )

  critmin_trade_tech <- critical_minerals_trade_build_tech(
    critmin_hhi = critmin_hhi,
    mineral_demand_clean = mineral_demand_clean,
    gamma = gamma
  )

  critical_minerals_trade_build_tidy(
    critmin_trade_tech = critmin_trade_tech,
    year = year
  )
}
