# Energy import dependence theme builder functions.
import_dependence_clean_raw <- function(ei, year = 2024) {
  ei %>%
    dplyr::select(Country, Year, Var, Value) %>%
    dplyr::filter(
      !grepl("Other|Total|OECD|OPEC", Country),
      Year == as.character(year),
      Var %in% c(
        "oilcons_ej",
        "oilcons_kbd",
        "oilprod_kbd",
        "primary_ej_pc",
        "gasprod_ej",
        "gascons_ej",
        "coalcons_ej",
        "coalprod_ej",
        "ren_power_ej",
        "ren_power_twh",
        "primary_ej",
        "electbyfuel_ren_power",
        "electbyfuel_total"
      )
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
    dplyr::filter(oilcons_ej != 0) %>%
    dplyr::mutate(
      dplyr::across(c(coalcons_ej:coalprod_ej), ~tidyr::replace_na(.x, 0))
    )
}

import_dependence_build_imports <- function(imports_raw) {
  imports_raw %>%
    dplyr::mutate(
      oil_calc = oilcons_ej / oilcons_kbd,
      oilprod_ej = oil_calc * oilprod_kbd,
      oil_imports_ej = oilprod_ej - oilcons_ej,
      oil_imports_kbd = oilprod_kbd - oilcons_kbd,
      gas_imports_ej = gasprod_ej - gascons_ej,
      coal_imports_ej = coalprod_ej - coalcons_ej,
      fossil_imports_ej = oil_imports_ej + gas_imports_ej + coal_imports_ej,
      oil_imports_share = 100 * oil_imports_ej / oilcons_ej,
      gas_imports_share = 100 * gas_imports_ej / gascons_ej,
      coal_imports_share = 100 * coal_imports_ej / coalcons_ej,
      fossil_import_share = 100 * fossil_imports_ej / (oilcons_ej + gascons_ej + coalcons_ej)
    ) %>%
    dplyr::mutate(
      oil_import_index = median_scurve(oil_imports_share),
      gas_import_index = median_scurve(gas_imports_share),
      coal_import_index = median_scurve(coal_imports_share),
      fossil_import_index = median_scurve(fossil_import_share)
    ) %>%
    dplyr::ungroup()
}

import_dependence_build_tidy <- function(imports_indexed, year = 2024) {
  imports_indexed %>%
    dplyr::select(
      Country,
      oil_imports_ej,
      oil_import_index,
      gas_imports_ej,
      gas_import_index,
      coal_imports_ej,
      coal_import_index,
      fossil_imports_ej,
      fossil_import_index
    ) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "oil_imports_ej", "Oil_raw"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "oil_import_index", "Oil_index"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "gas_imports_ej", "Gas_raw"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "gas_import_index", "Gas_index"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "coal_imports_ej", "Coal_raw"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "coal_import_index", "Coal_index"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "fossil_imports_ej", "Fossil_raw"), dplyr::everything()) %>%
    dplyr::rename_with(~ stringr::str_replace(.x, "fossil_import_index", "Fossil_index"), dplyr::everything()) %>%
    tidyr::pivot_longer(
      cols = -Country,
      names_to = c("tech", "data_type"),
      names_pattern = "^(.*)_(raw|index)$",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      supply_chain = "Upstream",
      category = "Energy Imports",
      variable = "Production surplus/deficit",
      Year = year,
      source = "EI Statistical Review of World Energy (2024)",
      explanation = dplyr::case_when(
        data_type == "raw" & tech == "Oil" ~
          "Oil import share (%) = oil production minus consumption as a share of consumption",
        data_type == "raw" & tech == "Gas" ~
          "Gas import share (%) = gas production minus consumption as a share of consumption",
        data_type == "raw" & tech == "Coal" ~
          "Coal import share (%) = coal production minus consumption as a share of consumption",
        data_type == "raw" & tech == "Fossil" ~
          "Fossil import share (%) = sum of oil, gas, and coal imports as a share of consumption",
        data_type == "raw" & tech == "Renewables" ~
          "Renewable generation share (%) = electbyfuel_ren_power ÷ electbyfuel_total × 100",
        data_type == "index" & tech %in% c("Oil", "Gas", "Coal", "Fossil") ~
          stringr::str_glue("Mean of percent-ranked import share & absolute imports for {tech}"),
        data_type == "index" & tech == "Renewables" ~
          "Percent-rank of renewable-generation share across countries",
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
    ) %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))
}

import_dependence <- function(ei, year = 2024) {
  energy_security_add_overall_index(
    import_dependence_clean_raw(ei, year = year) %>%
      import_dependence_build_imports() %>%
      import_dependence_build_tidy(year = year)
  )
}
