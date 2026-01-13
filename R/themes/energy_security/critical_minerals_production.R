# Critical minerals production theme builder functions.

# === Critical minerals production (EI data) ===
# These builders translate the EI Statistical Review mineral production tables
# into the canonical schema with demand-weighted rollups.

# ---- Production sheet specifications ----
# Each spec indicates where a mineral’s production series lives in the EI Excel
# workbook so the IO layer can load the correct sheet.
critical_minerals_production_specs <- function() {
  list(
    list(sheet = 83, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...31", tech_name = "Cobalt", unit_desc = "Thousand tonnes"),
    list(sheet = 84, skip = 2, nm_col = "Thousand tonnes of Lithium content", val_col = "2024...31", tech_name = "Lithium", unit_desc = "Thousand tonnes"),
    list(sheet = 85, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...31", tech_name = "Graphite", unit_desc = "Thousand tonnes"),
    list(sheet = 86, skip = 2, nm_col = "Thousand tonnes1", val_col = "2024...31", tech_name = "Rare Earths", unit_desc = "Thousand tonnes"),
    list(sheet = 87, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...12", tech_name = "Copper", unit_desc = "Thousand tonnes"),
    list(sheet = 88, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...13", tech_name = "Manganese", unit_desc = "Thousand tonnes"),
    list(sheet = 89, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...13", tech_name = "Nickel", unit_desc = "Thousand tonnes"),
    list(sheet = 90, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...13", tech_name = "Zinc", unit_desc = "Thousand tonnes"),
    list(sheet = 91, skip = 2, nm_col = "Thousand tonnes", val_col = "2024...12", tech_name = "PGMs", unit_desc = "Thousand tonnes")
  )
}

# ---- Build a single mineral production table ----
# Convert a raw EI sheet into raw + index values for one mineral.
critical_minerals_production_build_table <- function(sheet_data,
                                                     nm_col,
                                                     val_col,
                                                     tech_name,
                                                     unit_desc,
                                                     sheet_id,
                                                     country_reference,
                                                     year = 2024,
                                                     gamma = 0.5) {
  country_names <- country_reference$Country

  raw <- sheet_data %>%
    dplyr::rename(
      Country = dplyr::all_of(nm_col),
      raw_value = dplyr::all_of(val_col)
    ) %>%
    dplyr::mutate(
      Country = standardize_country_names(Country),
      Country = dplyr::case_when(
        Country %in% c("Rest of World", "Rest of world", "Rest of World^") ~ "Rest of World",
        Country == "US" ~ "United States",
        Country == "DR Congo" ~ "Democratic Republic of Congo",
        Country == "Russia Federation" ~ "Russia",
        TRUE ~ Country
      )
    ) %>%
    dplyr::filter(
      Country %in% country_names,
      !is.na(Country),
      !grepl("Total World|Other|OECD|OPEC|Orinoco", Country)
    ) %>%
    dplyr::mutate(raw_value = as.numeric(raw_value) %>% tidyr::replace_na(0))

  dummy_zero <- tibble::tibble(Country = "_ZERO_", raw_value = 0)

  dplyr::bind_rows(raw, dummy_zero) %>%
    dplyr::mutate(index_value = median_scurve(raw_value, gamma = gamma)) %>%
    dplyr::filter(Country != "_ZERO_") %>%
    tidyr::complete(
      Country = country_names,
      fill = list(raw_value = 0, index_value = 0)
    ) %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = tech_name,
      supply_chain = "Upstream",
      category = "Production",
      variable = stringr::str_glue("{tech_name} Production"),
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = year,
      source = "EI Statistical Review of World Energy (2024)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue("{tech_name} production ({unit_desc}) from sheet {sheet_id}"),
        data_type == "index" ~ "Percent-rank of production across reporting entities (countries + RoW)"
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

# ---- Demand-weighted rollup ----
# Combine mineral-level indices into technology-level production scores.
critical_minerals_production_build_weighted <- function(critical_min_production,
                                                        mineral_demand_clean,
                                                        country_reference,
                                                        gamma = 0.5) {
  weighted_inputs <- critical_min_production %>%
    dplyr::rename(Mineral = tech) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::ungroup() %>%
        dplyr::select(Mineral, tech, share_24) %>%
        dplyr::mutate(
          Mineral = dplyr::case_when(
            stringr::str_detect(Mineral, stringr::regex("rare", ignore_case = TRUE)) ~ "Rareearths",
            Mineral == "Battery-grade graphite" ~ "Graphite",
            TRUE ~ Mineral
          )
        ),
      by = "Mineral"
    ) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::group_by(Country, tech, data_type) %>%
    dplyr::mutate(share_24 = share_24 / sum(share_24)) %>%
    dplyr::ungroup()

  critical_min_prod <- weighted_inputs %>%
    dplyr::filter(data_type == "index") %>%
    dplyr::group_by(Country, tech, supply_chain, category, data_type, source, explanation, Year) %>%
    dplyr::summarize(value = stats::weighted.mean(value, w = share_24, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(variable = stringr::str_glue("{tech} Production")) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(tech) %>%
    dplyr::mutate(value = median_scurve(value, gamma = gamma)) %>%
    dplyr::bind_rows(
      weighted_inputs %>%
        dplyr::select(-tech) %>%
        dplyr::rename(tech = Mineral) %>%
        dplyr::distinct(
          Country,
          tech,
          supply_chain,
          category,
          data_type,
          variable,
          value,
          Year,
          source,
          explanation
        )
    ) %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(
      Country = country_reference$Country,
      fill = list(value = 0)
    )

  critical_min_prod
}

# === Public theme entrypoint ===
critical_minerals_production <- function(production_inputs,
                                         mineral_demand_clean,
                                         country_reference,
                                         year = 2024,
                                         gamma = 0.5) {
  production_tables <- lapply(production_inputs, function(spec) {
    critical_minerals_production_build_table(
      sheet_data = spec$data,
      nm_col = spec$nm_col,
      val_col = spec$val_col,
      tech_name = spec$tech_name,
      unit_desc = spec$unit_desc,
      sheet_id = spec$sheet,
      country_reference = country_reference,
      year = year,
      gamma = gamma
    )
  })

  critical_min_production <- dplyr::bind_rows(production_tables)

  critical_minerals_production_build_weighted(
    critical_min_production = critical_min_production,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference,
    gamma = gamma
  ) %>%
    energy_security_add_overall_index()
}
