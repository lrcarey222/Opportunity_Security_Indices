# Critical minerals processing theme builder functions.

# === Critical minerals processing (IEA supply) ===
# These builders reshape the IEA critical minerals dataset into a clean table
# that mirrors the legacy “mineral_supply” pipeline.

# ---- Build mineral supply inputs ----
# Filter to total supply rows, split mineral names into mineral + supply-chain stage
# (mining -> Extraction, refining/processing -> Refining), and ensure the resulting table
# includes all countries for every (mineral, stage) block.
#
# `roster` (country names), when given, is completed as well: a roster country the IEA does
# not list is a non-supplier and is scored like a listed country with zero supply (see
# foreign_dependency_build_mineral_supply()), not left to a flat 0 in the tidy step.
critical_minerals_processing_build_supply <- function(critical,
                                                      mineral_demand_clean,
                                                      country_info,
                                                      gamma = 0.5,
                                                      base_year = iea_critical_minerals_base_year(critical),
                                                      horizon_year = 2035,
                                                      roster = NULL) {
  # The IEA restates its base year each release, so address the supply columns by year.
  base_col <- iea_critical_minerals_year_col(critical, base_year, label = "mineral supply")
  horizon_col <- iea_critical_minerals_year_col(critical, horizon_year, label = "mineral supply")

  critical <- critical %>%
    dplyr::mutate(`Sector.Country` = standardize_country_names(`Sector.Country`))

  countries <- critical %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::inner_join(
      country_info %>% dplyr::select(country),
      by = c("Sector.Country" = "country")
    ) %>%
    dplyr::bind_rows(tibble::tibble(`Sector.Country` = as.character(roster))) %>%
    dplyr::distinct(`Sector.Country`)

  critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    dplyr::rename(country = `Sector.Country`) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    # Mining scores Extraction, refining/processing Refining (the old recode sent both to one
    # stage), and minerals carry the canonical names their demand shares are keyed on.
    dplyr::mutate(
      mineral = harmonize_mineral_names(mineral),
      supply_chain = iea_critical_minerals_supply_stage(supply_chain_raw, legacy = FALSE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      mineral, country, supply_chain,
      supply_base = dplyr::all_of(base_col),
      supply_horizon = dplyr::all_of(horizon_col)
    ) %>%
    tidyr::complete(
      tidyr::nesting(mineral, supply_chain),
      country = countries$`Sector.Country`,
      fill = list(supply_base = 0, supply_horizon = 0)
    ) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::filter(!is.na(tech)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(Mineral = harmonize_mineral_names(Mineral), tech, share_24, share_35),
      by = c("mineral" = "Mineral"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      supply_24 = share_24 * supply_base,
      supply_35 = share_35 * supply_horizon
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
    # Ranked within stage, so refining shares are compared with refining shares; a zero share
    # scores 0 and suppliers are ranked above it (iea_critical_minerals_share_index()).
    dplyr::group_by(supply_chain) %>%
    dplyr::mutate(
      market_share24_index = iea_critical_minerals_share_index(share_frac_24, gamma = gamma),
      market_share35_index = iea_critical_minerals_share_index(share_frac_35, gamma = gamma),
      hhi24_index = median_scurve(1 - HHI_24, gamma = gamma),
      hhi35_index = median_scurve(1 - HHI_35, gamma = gamma)
    ) %>%
    dplyr::ungroup() %>%
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
          "Percent-rank of {tech} market share across countries, and HHI across critical minerals, ",
          "of {dplyr::if_else(supply_chain == 'Refining', 'refined', 'mined')} supply"
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
                                         year = iea_critical_minerals_base_year(critical),
                                         gamma = 0.5) {
  mineral_supply <- critical_minerals_processing_build_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    gamma = gamma,
    base_year = year,
    roster = country_reference$Country
  )

  critical_minerals_processing_build_tidy(
    mineral_supply = mineral_supply,
    country_reference = country_reference,
    year = year
  ) %>%
    energy_security_add_overall_index()
}
