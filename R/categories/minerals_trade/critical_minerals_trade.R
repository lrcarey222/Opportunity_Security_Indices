# Critical minerals trade theme builder functions.

# === Critical minerals trade (UN Comtrade) ===
# These builders mirror the legacy UN Comtrade pipeline for critical minerals
# and return the tidy, canonical schema.
#
# Construction (Technical Appendix, "Critical-mineral trade": security sub-index =
# mean(market-share, HHI), demand-weighted across minerals). For reporter i, mineral m and
# technology t, with w_mt the IEA base-year demand shares renormalized over the minerals that
# have trade data:
#   critmin_market_share_it = sum_m w_mt * S_m(X_im / sum_j X_jm)   (S_m: S-curve within m
#                             over the exporters, above a zero anchor; non-exporters score 0)
#   critmin_hhi_t           = sum_m w_mt * S(-HHI_m)                 (same for every country)
# config/index_definition.yml averages the two into "Overall Minerals Trade Index". The HHI
# term is a property of the world market for the technology's minerals, so it only shifts the
# level of a technology; it is emitted for every country in `country_info`, not only the
# Comtrade reporters, so that a non-reporter is not scored below a reporter on it.

# ---- Build mineral regex for commodity parsing ----
# The legacy pipeline extracts mineral names from Comtrade descriptions using a
# regex built from the IEA mineral demand list. Canonical names that Comtrade spells
# differently ("rare-earth metals", "platinum") are searched by their Comtrade wording.
critical_minerals_trade_build_minerals_pattern <- function(mineral_demand_clean) {
  minerals <- unique(harmonize_mineral_names(mineral_demand_clean$Mineral))
  minerals <- minerals[!is.na(minerals)]

  comtrade_terms <- c(
    "Graphite" = "graphite",
    "Rare Earths" = "rare.?earth",
    "PGMs" = "platinum|palladium|rhodium"
  )
  terms <- ifelse(minerals %in% names(comtrade_terms), comtrade_terms[minerals], minerals)

  paste0(paste0(unique(terms), collapse = "|"), "|", "Graphite")
}

# ---- Filter HS codes to critical minerals ----
# Match the Comtrade critical-minerals dashboard to the minerals regex so the
# API pulls only the relevant commodity codes.
critical_minerals_trade_filter_hs <- function(crit_hs, mineral_demand_clean) {
  minerals_pattern <- critical_minerals_trade_build_minerals_pattern(mineral_demand_clean)

  crit_hs %>%
    dplyr::filter(
      stringr::str_detect(
        comtradeDescription,
        stringr::regex(minerals_pattern, ignore_case = TRUE)
      )
    )
}

# ---- Merge import/export flows ----
# Combine import/export values, map reporter ISO3 to country names, and derive
# the mineral label from the commodity description.
#
# Imports and exports are full-joined on (reporter, HS code): the import-led left join
# dropped every reporter-code with exports but no imports (2.5% of 2024 export value).
critical_minerals_trade_build_trade <- function(critmin_import,
                                                critmin_export,
                                                country_info,
                                                minerals_pattern) {
  flows <- function(tbl, value_name) {
    tbl %>%
      dplyr::group_by(reporter_iso, cmd_code) %>%
      dplyr::summarize(
        cmd_desc = dplyr::first(cmd_desc),
        "{value_name}" := sum(primary_value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  dplyr::full_join(
    flows(critmin_import, "imports"),
    flows(critmin_export, "exports"),
    by = c("reporter_iso", "cmd_code"),
    suffix = c("_imp", "_exp")
  ) %>%
    dplyr::mutate(cmd_desc = dplyr::coalesce(cmd_desc_exp, cmd_desc_imp)) %>%
    dplyr::select(-cmd_desc_imp, -cmd_desc_exp) %>%
    dplyr::left_join(country_info %>% dplyr::select(iso3c, country), by = c("reporter_iso" = "iso3c")) %>%
    dplyr::mutate(
      imports = dplyr::if_else(is.na(imports), 0, imports),
      exports = dplyr::if_else(is.na(exports), 0, exports),
      trade_balance = (exports - imports) / (exports + imports),
      mineral = harmonize_mineral_names(
        stringr::str_extract(cmd_desc, stringr::regex(minerals_pattern, ignore_case = TRUE))
      ),
      supply_chain = "Upstream"
    )
}

# ---- Compute market shares and HHI ----
# Mineral-level exports, world market shares and export-market HHIs over the Comtrade
# reporters. Every reporter is completed with zero exports for the minerals it does not
# export, so the tech rollup below weights every mineral the technology needs instead of only
# the ones a country happens to trade.
critical_minerals_trade_build_hhi <- function(critmin_trade, total_export, gamma = 0.5) {
  critmin_trade %>%
    dplyr::filter(!is.na(mineral)) %>%
    dplyr::group_by(reporter_iso, country, mineral, supply_chain) %>%
    dplyr::summarize(exports = sum(exports, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      tidyr::nesting(reporter_iso, country),
      tidyr::nesting(mineral, supply_chain),
      fill = list(exports = 0)
    ) %>%
    dplyr::left_join(
      total_export %>% dplyr::select(reporter_iso, total_exports = primary_value),
      by = "reporter_iso"
    ) %>%
    dplyr::mutate(exp_share = exports / total_exports) %>%
    dplyr::group_by(mineral, supply_chain) %>%
    dplyr::mutate(
      market_share = exports / sum(exports, na.rm = TRUE),
      HHI = sum(market_share^2, na.rm = TRUE),
      # A reporter exporting none of a mineral scores 0, the same as a country the zero
      # missing-data rule fills in; exporters are ranked within the mineral above it.
      market_share_index = iea_critical_minerals_share_index(market_share, gamma = gamma)
    ) %>%
    dplyr::ungroup()
}

# World export concentration by mineral: one row per mineral, S-curve of -HHI across minerals
# (lower concentration = more secure).
critical_minerals_trade_build_mineral_hhi <- function(critmin_hhi, gamma = 0.5) {
  critmin_hhi %>%
    dplyr::distinct(mineral, supply_chain, HHI) %>%
    dplyr::mutate(hhi_index = median_scurve(-HHI, gamma = gamma))
}

# Demand weights over the minerals that have trade data, identical for every country.
critical_minerals_trade_build_weights <- function(mineral_demand_clean, traded_minerals) {
  mineral_demand_clean %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::transmute(mineral = harmonize_mineral_names(Mineral), tech, share_24) %>%
    dplyr::filter(mineral %in% traded_minerals) %>%
    dplyr::group_by(mineral, tech) %>%
    dplyr::summarize(share_24 = sum(share_24, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech) %>%
    dplyr::filter(sum(share_24) > 0) %>%
    dplyr::mutate(weight = share_24 / sum(share_24)) %>%
    dplyr::ungroup() %>%
    dplyr::select(mineral, tech, weight)
}

# ---- Roll up to technology-level trade indices ----
critical_minerals_trade_build_tech <- function(critmin_hhi, mineral_demand_clean, gamma = 0.5) {
  weights <- critical_minerals_trade_build_weights(
    mineral_demand_clean,
    traded_minerals = unique(critmin_hhi$mineral)
  )

  critmin_hhi %>%
    dplyr::inner_join(weights, by = "mineral", relationship = "many-to-many") %>%
    dplyr::group_by(reporter_iso, country, tech, supply_chain) %>%
    dplyr::summarize(
      critmin_market_share = sum(weight * market_share, na.rm = TRUE),
      critmin_market_share_index = sum(weight * market_share_index, na.rm = TRUE),
      critmin_export_share = sum(weight * exp_share, na.rm = TRUE),
      .groups = "drop"
    )
}

# Technology-level HHI component: one value per technology.
critical_minerals_trade_build_tech_hhi <- function(critmin_hhi, mineral_demand_clean, gamma = 0.5) {
  weights <- critical_minerals_trade_build_weights(
    mineral_demand_clean,
    traded_minerals = unique(critmin_hhi$mineral)
  )

  critical_minerals_trade_build_mineral_hhi(critmin_hhi, gamma = gamma) %>%
    dplyr::inner_join(weights, by = "mineral", relationship = "many-to-many") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarize(
      critmin_hhi = sum(weight * HHI),
      critmin_hhi_index = sum(weight * hhi_index),
      .groups = "drop"
    )
}

# ---- Canonical tidy output ----
critical_minerals_trade_build_tidy <- function(critmin_trade_tech,
                                               critmin_tech_hhi,
                                               countries,
                                               year = 2024) {
  share_tbl <- critmin_trade_tech %>%
    dplyr::filter(!is.na(country)) %>%
    dplyr::select(
      country, tech, supply_chain,
      critmin_market_share, critmin_market_share_index, critmin_export_share
    ) %>%
    tidyr::pivot_longer(
      cols = -c(country, tech, supply_chain),
      names_to = "variable",
      values_to = "value"
    )

  hhi_tbl <- tidyr::crossing(country = unique(countries[!is.na(countries)]), critmin_tech_hhi) %>%
    tidyr::pivot_longer(
      cols = c(critmin_hhi, critmin_hhi_index),
      names_to = "variable",
      values_to = "value"
    )

  dplyr::bind_rows(share_tbl, hhi_tbl) %>%
    dplyr::mutate(
      category = "Minerals Trade",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = year,
      source = "UN Comtrade",
      explanation = dplyr::case_when(
        variable == "critmin_market_share" & data_type == "raw" ~
          "Demand-weighted share of world exports of the technology's minerals",
        variable == "critmin_market_share" ~
          "Demand-weighted within-mineral S-curve of world export market share (non-exporters = 0)",
        variable == "critmin_export_share" ~
          "Demand-weighted share of the technology's minerals in the country's total exports",
        variable == "critmin_hhi" & data_type == "raw" ~
          "Demand-weighted HHI of world exports of the technology's minerals",
        variable == "critmin_hhi" ~
          "Demand-weighted S-curve of -HHI of world mineral exports (same for every country)",
        TRUE ~ "UN Comtrade export data"
      )
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
# `year` defaults to the reference year the Comtrade extract itself carries, so the label
# follows the file rather than a pinned 2024.
critical_minerals_trade <- function(critmin_import,
                                    critmin_export,
                                    total_export,
                                    mineral_demand_clean,
                                    country_info,
                                    year = NULL,
                                    gamma = 0.5) {
  year <- year %||% {
    ref_years <- suppressWarnings(as.integer(c(critmin_export$ref_year, critmin_import$ref_year)))
    if (all(is.na(ref_years))) 2024L else max(ref_years, na.rm = TRUE)
  }

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

  critmin_tech_hhi <- critical_minerals_trade_build_tech_hhi(
    critmin_hhi = critmin_hhi,
    mineral_demand_clean = mineral_demand_clean,
    gamma = gamma
  )

  critical_minerals_trade_build_tidy(
    critmin_trade_tech = critmin_trade_tech,
    critmin_tech_hhi = critmin_tech_hhi,
    countries = country_info$country,
    year = year
  ) %>%
    energy_security_add_overall_index()
}
