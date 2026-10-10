# Energy import dependence theme builder functions.
import_dependence_clean_raw <- function(ei, year = 2025) {
  ei %>%
    dplyr::select(Country, Year, Var, Value) %>%
    # Standardized before ranking, so each economy enters the percent ranks once; "USSR"
    # (EI's CIS aggregate) is dropped with the other aggregates.
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::filter(
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country),
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
      fossilprod_ej = oilprod_ej + gasprod_ej + coalprod_ej,
      fossil_tes_ej = oil_tes_ej + gas_tes_ej + coal_tes_ej,
      # Net balance, production minus consumption (EJ): positive for a net exporter. These
      # columns were named *_imports_ej although a net importer has a negative value.
      oil_surplus_ej = oilprod_ej - oil_tes_ej,
      oil_surplus_kbd = oilprod_kbd - oilcons_kbd,
      gas_surplus_ej = gasprod_ej - gas_tes_ej,
      coal_surplus_ej = coalprod_ej - coal_tes_ej,
      fossil_surplus_ej = oil_surplus_ej + gas_surplus_ej + coal_surplus_ej,
      # Self-sufficiency, production as a percent of consumption: 100 is balanced, 0 fully
      # imported, above 100 a net exporter. It replaces the former "Import share",
      # (production - consumption) / consumption x 100, which is this minus 100 and was
      # positive for exporters despite its name. The index was always oriented higher = less
      # import dependent, and a shift by 100 leaves the percent ranks as they were, with one
      # exception that this form removes: 100 * (0 - c) / c is not exactly -100 for every c,
      # so economies that produce none of a fuel were split by rounding noise instead of
      # tying (2025 oil: Spain -100.00000000000001 ranked below 38 exact -100s, Israel,
      # Netherlands, Poland and Singapore -99.999999999999986 above them). 100 * 0 / c is 0.
      # Undefined where a country consumes none of the fuel, so the denominators are guarded
      # rather than emitting 0/0. The absolute balance above is still 0 and still
      # meaningful, and the index below averages over whichever of the two is available.
      oil_self_sufficiency = dplyr::if_else(oil_tes_ej > 0, 100 * oilprod_ej / oil_tes_ej, NA_real_),
      gas_self_sufficiency = dplyr::if_else(gas_tes_ej > 0, 100 * gasprod_ej / gas_tes_ej, NA_real_),
      coal_self_sufficiency = dplyr::if_else(coal_tes_ej > 0, 100 * coalprod_ej / coal_tes_ej, NA_real_),
      fossil_self_sufficiency = dplyr::if_else(
        fossil_tes_ej > 0,
        100 * fossilprod_ej / fossil_tes_ej,
        NA_real_
      )
    ) %>%
    dplyr::mutate(
      oil_import_index = rowMeans(cbind(median_scurve(oil_self_sufficiency), median_scurve(oil_surplus_ej)), na.rm = TRUE),
      gas_import_index = rowMeans(cbind(median_scurve(gas_self_sufficiency), median_scurve(gas_surplus_ej)), na.rm = TRUE),
      coal_import_index = rowMeans(cbind(median_scurve(coal_self_sufficiency), median_scurve(coal_surplus_ej)), na.rm = TRUE),
      fossil_import_index = rowMeans(cbind(median_scurve(fossil_self_sufficiency), median_scurve(fossil_surplus_ej)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

# The index is the mean of two percent-ranked components: the absolute balance and
# self-sufficiency. Publish both as raw rows so the composite can be audited, and keep them
# on separate `variable` names so (Country, tech, variable, data_type) stays unique.
#
# "Production surplus/deficit" must stay the EJ balance: config/index_definition.yml lists
# it as the Overall Energy Imports Index component, and R/charts/import_exposure_chart.R
# reads its raw row as production_surplus_deficit_ej.
import_dependence_component_specs <- function() {
  list(
    list(
      suffix = "_surplus_ej",
      variable = "Production surplus/deficit",
      data_type = "raw",
      explanation = paste(
        "{tech} production surplus/deficit (EJ) = production minus consumption;",
        "negative means net importer"
      )
    ),
    list(
      suffix = "_self_sufficiency",
      variable = "Self-sufficiency",
      data_type = "raw",
      explanation = paste(
        "{tech} self-sufficiency (%) = production as a percent of consumption; below 100 means",
        "net importer, 0 fully imported. NA where the country consumes none of the fuel"
      )
    ),
    list(
      suffix = "_import_index",
      variable = "Production surplus/deficit",
      data_type = "index",
      explanation = paste(
        "Mean of the percent-ranked self-sufficiency and percent-ranked absolute balance for",
        "{tech}; higher means less dependent on imports"
      )
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
      source = ei_stat_review_source(year)
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

import_dependence <- function(ei, year = 2025) {
  energy_security_add_overall_index(
    import_dependence_clean_raw(ei, year = year) %>%
      import_dependence_build_imports() %>%
      import_dependence_build_tidy(year = year)
  )
}
