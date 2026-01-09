# Critical minerals processing theme builder functions.

# === Critical minerals processing (IEA supply) ===
# These builders reshape the IEA critical minerals dataset into a clean table
# that mirrors the legacy “mineral_supply” pipeline.

# ---- Build mineral supply inputs ----
# Filter to total supply rows, split mineral names into mineral + supply-chain,
# and ensure the resulting table includes all countries and minerals.
critical_minerals_processing_build_supply <- function(critical,
                                                      mineral_demand_clean,
                                                      country_info,
                                                      gamma = 0.5) {
  minerals <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::distinct(mineral)

  countries <- critical %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::inner_join(
      country_info %>% dplyr::select(country),
      by = c("Sector.Country" = "country")
    )

  critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    dplyr::rename(country = `Sector.Country`) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::mutate(
      supply_chain = dplyr::if_else(
        stringr::str_detect(supply_chain_raw, stringr::regex("Refining|Chemical", ignore_case = TRUE)),
        "Midstream",
        "Upstream"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(mineral, country, supply_chain, X2024, X2035) %>%
    tidyr::complete(
      mineral = minerals$mineral,
      supply_chain = c("Upstream", "Midstream"),
      country = countries$`Sector.Country`,
      fill = list(X2024 = 0, X2035 = 0)
    ) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::filter(!is.na(tech)) %>%
        dplyr::ungroup() %>%
        dplyr::select(Mineral, tech, share_24, share_35),
      by = c("mineral" = "Mineral")
    ) %>%
    dplyr::mutate(
      supply_24 = share_24 * X2024,
      supply_35 = share_35 * X2035
    ) %>%
    dplyr::group_by(mineral, tech, supply_chain) %>%
    dplyr::mutate(
      share_frac_24 = supply_24 / sum(supply_24, na.rm = TRUE),
      HHI_24 = sum(share_frac_24^2, na.rm = TRUE),
      share_frac_35 = supply_35 / sum(supply_35, na.rm = TRUE),
      HHI_35 = sum(share_frac_35^2, na.rm = TRUE)
    ) %>%
    dplyr::filter(
      country != "Rest of world",
      !is.na(share_frac_24),
      !is.na(share_frac_35)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      market_share24_index = median_scurve(share_frac_24, gamma = gamma),
      market_share35_index = median_scurve(share_frac_35, gamma = gamma),
      hhi24_index = median_scurve(1 - HHI_24, gamma = gamma),
      hhi35_index = median_scurve(1 - HHI_35, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      security_index = mean(dplyr::c_across(c(market_share24_index:hhi35_index)), na.rm = TRUE)
    ) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::arrange(dplyr::desc(security_index))
}

# ---- Build canonical tidy output ----
# Aggregate the mineral-level indices into technology-level scores and return
# the standard schema with completed country coverage.
critical_minerals_processing_build_tidy <- function(mineral_supply,
                                                    country_reference,
                                                    year = 2024) {
  mineral_supply %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::summarize(
      security_index = stats::weighted.mean(security_index, w = share_35, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    tidyr::pivot_longer(
      cols = c(security_index),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      category = "Foreign Dependency",
      variable = "Mineral Supply",
      data_type = "index",
      Year = year,
      source = "IEA Critical Minerals Database",
      explanation = dplyr::case_when(
        data_type == "index" ~ stringr::str_glue(
          "Percent-rank of {tech} market share across countries, and HHI across critical minerals"
        )
      )
    ) %>%
    dplyr::select(
      country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    ) %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(
      country = country_reference$Country,
      fill = list(value = 0)
    ) %>%
    dplyr::rename(Country = country)
}

# === Public theme entrypoint ===
critical_minerals_processing <- function(critical,
                                         mineral_demand_clean,
                                         country_info,
                                         country_reference,
                                         year = 2024,
                                         gamma = 0.5) {
  mineral_supply <- critical_minerals_processing_build_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    gamma = gamma
  )

  critical_minerals_processing_build_tidy(
    mineral_supply = mineral_supply,
    country_reference = country_reference,
    year = year
  )
}
