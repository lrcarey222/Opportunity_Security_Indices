# Energy import dependence theme builder functions.
import_dependence_clean_raw <- function(ei, year = 2025) {
  ei %>%
    dplyr::select(Country, Year, Var, Value) %>%
    dplyr::filter(
      !grepl("Other|Total|OECD|OPEC", Country),
      Year == as.character(year),
      Var %in% c(
        "oil_tes_ej",
        "oilcons_kbd",
        "oilprod_kbd",
        "tes_gj_pc",
        "gasprod_ej",
        "gas_tes_ej",
        "coal_tes_ej",
        "coalprod_ej",
        "ren_power_tes_ej",
        "ren_power_twh",
        "tes_ej",
        "electbyfuel_ren_power",
        "electbyfuel_total"
      )
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
    dplyr::filter(oil_tes_ej != 0) %>%
    # EI drops a country's row entirely when it produces or consumes none of a fuel, so an
    # absent value means zero, not unknown. Without this fill the production side is NA for
    # every non-producer and the whole surplus/deficit chain collapses to NA for them.
    #
    # Named explicitly rather than as a column range: the pivoted column order follows the
    # order Vars happen to appear in the extract, so a positional range is not stable across
    # EI releases.
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "oilprod_kbd", "gasprod_ej", "coalprod_ej",
          "oil_tes_ej", "gas_tes_ej", "coal_tes_ej"
        )),
        ~tidyr::replace_na(.x, 0)
      )
    )
}

import_dependence_build_imports <- function(imports_raw) {
  imports_raw %>%
    dplyr::mutate(
      oil_calc = oil_tes_ej / oilcons_kbd,
      oilprod_ej = oil_calc * oilprod_kbd,
      oil_imports_ej = oilprod_ej - oil_tes_ej,
      oil_imports_kbd = oilprod_kbd - oilcons_kbd,
      gas_imports_ej = gasprod_ej - gas_tes_ej,
      coal_imports_ej = coalprod_ej - coal_tes_ej,
      fossil_imports_ej = oil_imports_ej + gas_imports_ej + coal_imports_ej,
      # An import *share* is undefined where a country consumes none of the fuel, so guard
      # the denominators rather than emitting 0/0. The absolute surplus/deficit above is
      # still 0 and still meaningful, and the index below averages over whichever of the two
      # components is available.
      oil_imports_share = dplyr::if_else(oil_tes_ej > 0, 100 * oil_imports_ej / oil_tes_ej, NA_real_),
      gas_imports_share = dplyr::if_else(gas_tes_ej > 0, 100 * gas_imports_ej / gas_tes_ej, NA_real_),
      coal_imports_share = dplyr::if_else(coal_tes_ej > 0, 100 * coal_imports_ej / coal_tes_ej, NA_real_),
      fossil_import_share = dplyr::if_else(
        (oil_tes_ej + gas_tes_ej + coal_tes_ej) > 0,
        100 * fossil_imports_ej / (oil_tes_ej + gas_tes_ej + coal_tes_ej),
        NA_real_
      )
    ) %>%
    dplyr::mutate(
      oil_import_index = rowMeans(cbind(median_scurve(oil_imports_share), median_scurve(oil_imports_ej)), na.rm = TRUE),
      gas_import_index = rowMeans(cbind(median_scurve(gas_imports_share), median_scurve(gas_imports_ej)), na.rm = TRUE),
      coal_import_index = rowMeans(cbind(median_scurve(coal_imports_share), median_scurve(coal_imports_ej)), na.rm = TRUE),
      fossil_import_index = rowMeans(cbind(median_scurve(fossil_import_share), median_scurve(fossil_imports_ej)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

import_dependence_build_tidy <- function(imports_indexed, year = 2025) {
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
      source = "EI Statistical Review of World Energy (2025)",
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

import_dependence <- function(ei, year = 2025) {
  energy_security_add_overall_index(
    import_dependence_clean_raw(ei, year = year) %>%
      import_dependence_build_imports() %>%
      import_dependence_build_tidy(year = year)
  )
}
