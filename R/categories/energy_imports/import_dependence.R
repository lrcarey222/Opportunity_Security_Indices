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
      fossil_imports_share = dplyr::if_else(
        (oil_tes_ej + gas_tes_ej + coal_tes_ej) > 0,
        100 * fossil_imports_ej / (oil_tes_ej + gas_tes_ej + coal_tes_ej),
        NA_real_
      )
    ) %>%
    dplyr::mutate(
      oil_import_index = rowMeans(cbind(median_scurve(oil_imports_share), median_scurve(oil_imports_ej)), na.rm = TRUE),
      gas_import_index = rowMeans(cbind(median_scurve(gas_imports_share), median_scurve(gas_imports_ej)), na.rm = TRUE),
      coal_import_index = rowMeans(cbind(median_scurve(coal_imports_share), median_scurve(coal_imports_ej)), na.rm = TRUE),
      fossil_import_index = rowMeans(cbind(median_scurve(fossil_imports_share), median_scurve(fossil_imports_ej)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

# The index is the mean of two percent-ranked components: the absolute balance and the
# import share. Publish both as raw rows so the composite can be audited, and keep them on
# separate `variable` names so (Country, tech, variable, data_type) stays unique.
#
# "Production surplus/deficit" must stay the EJ balance: config/index_definition.yml lists
# it as the Overall Energy Imports Index component, and R/charts/import_exposure_chart.R
# reads its raw row as production_surplus_deficit_ej.
import_dependence_component_specs <- function() {
  list(
    list(
      suffix = "_imports_ej",
      variable = "Production surplus/deficit",
      data_type = "raw",
      explanation = paste(
        "{tech} production surplus/deficit (EJ) = production minus consumption;",
        "negative means net importer"
      )
    ),
    list(
      suffix = "_imports_share",
      variable = "Import share",
      data_type = "raw",
      explanation = paste(
        "{tech} import share (%) = production minus consumption as a share of consumption;",
        "negative means net importer. NA where the country consumes none of the fuel"
      )
    ),
    list(
      suffix = "_import_index",
      variable = "Production surplus/deficit",
      data_type = "index",
      explanation = "Mean of the percent-ranked import share and percent-ranked absolute balance for {tech}"
    )
  )
}

import_dependence_build_tidy <- function(imports_indexed, year = 2025) {
  techs <- c(oil = "Oil", gas = "Gas", coal = "Coal", fossil = "Fossil")

  components <- lapply(import_dependence_component_specs(), function(spec) {
    cols <- paste0(names(techs), spec$suffix)
    require_columns(imports_indexed, cols, label = "imports_indexed")

    imports_indexed %>%
      dplyr::select(Country, dplyr::all_of(cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols),
        names_to = "tech",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        tech = unname(techs[sub(paste0(spec$suffix, "$"), "", tech)]),
        variable = spec$variable,
        data_type = spec$data_type,
        explanation = as.character(stringr::str_glue(spec$explanation))
      )
  })

  dplyr::bind_rows(components) %>%
    dplyr::mutate(
      supply_chain = "Upstream",
      category = "Energy Imports",
      Year = year,
      source = "EI Statistical Review of World Energy (2025)"
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
