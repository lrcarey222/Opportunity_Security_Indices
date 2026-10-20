# Prosperous opportunity theme (dyadic export-opportunity index).
#
# Direction convention. In every opportunity table the reporter (reporter_iso) is the exporting
# country and the partner (partner_iso) is the candidate export market: Opportunity(i, j, t, s)
# rates market j for reporter i's exports of (t, s), from i's reported exports to j, i's own
# economic opportunity and j's energy-security need, less a climate penalty on j. Friendshore
# uses the import flow (reporter = importer, partner = supplier; safer_friendshore.R), so both
# rate the partner from the reporter's side. The tables are not transposed here.
#
# Universe: reporters with an EO record x partners with an ES record, one row per ISO3, no
# self-dyads. The trade component reads the reconciled flow matrix of
# partnership_strength_build_trade_cells() (partnership_strength_helpers.R): the market's import
# declaration where it reports the cell, else the reporter's own export declaration (mirror). A
# dyad cell with no observed flow in the last theme year takes the (tech, stage) mean
# (trade_status "imputed"); one observed with no trade is a recorded zero. A (tech, stage) the HS6
# crosswalk maps no code to ("no_data": the Downstream stages and Green Hydrogen) takes the dyad's
# own trade index in its observed cells, the same technology first (no_data_fill "dyad";
# partnership_strength_fill_no_data_trade(), partnership_strength_helpers.R).
#
# Each dyad cell also carries imputed_weight_share, the share of its partner- and pair-specific
# component weight (trade, partner_es_need) resting on imputed inputs
# (partnership_strength_imputed_weight_share(), partnership_strength_helpers.R); the top-5 table
# carries the dyad's mean over its top-5 cells on the Opportunity Index rows (PSI rankability).

# Code defaults; the pipeline passes config/weights.yml partnership_opportunity_components.
partnership_strength_opportunity_default_weights <- function() {
  list(
    trade_index = 2,
    econ_opp_index = 2,
    partner_es_need = 1,
    climate_policy_weight = 0.2
  )
}

# Reporter-only view of a Comtrade dyad table at (tech, stage) level: each reporter's own rows,
# no mirror flows and no recorded zeros. The themes no longer use it (they read
# partnership_strength_build_trade_cells()); it is kept with partnership_strength_build_dyad_series()
# for the comparisons in scripts/paper/fig_bilateral.R.
partnership_strength_clean_trade_data <- function(comtrade_dyads, subcat, years = 2020:2024) {
  require_columns(
    comtrade_dyads,
    c("reporter_iso", "partner_iso"),
    label = "comtrade_dyads"
  )

  flow_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("flow_direction", "trade_flow", "flow"),
    "flow"
  )
  period_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("period", "year", "Year", "ref_year"),
    "period"
  )
  value_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("primary_value", "trade_value", "value"),
    "trade value"
  )
  code_col <- partnership_strength_pick_column(
    comtrade_dyads,
    c("cmd_code", "code", "hs6"),
    "HS6 code"
  )

  mapping <- partnership_strength_build_trade_mapping(subcat)

  comtrade_dyads %>%
    dplyr::transmute(
      reporter_iso = as.character(reporter_iso),
      partner_iso = as.character(partner_iso),
      period = suppressWarnings(as.integer(.data[[period_col]])),
      flow_direction = stringr::str_to_lower(as.character(.data[[flow_col]])),
      cmd_code = stringr::str_pad(as.character(.data[[code_col]]), width = 6, side = "left", pad = "0"),
      primary_value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    dplyr::left_join(mapping, by = c("cmd_code" = "code"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(period), period %in% years) %>%
    dplyr::select(reporter_iso, partner_iso, period, flow_direction, tech, supply_chain, primary_value)
}

partnership_strength_build_dyad_series <- function(res_tech, flow = c("export", "import"), years = 2020:2024) {
  flow <- match.arg(flow)

  res_tech %>%
    dplyr::filter(flow_direction == flow, !is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(reporter_iso, partner_iso, tech, supply_chain, period) %>%
    dplyr::summarise(val = sum(primary_value, na.rm = TRUE), .groups = "drop_last") %>%
    tidyr::complete(period = c(min(years), max(years)), fill = list(val = 0)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = period, values_from = val, names_prefix = "y", values_fill = 0) %>%
    dplyr::transmute(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      level_last = .data[[paste0("y", max(years))]],
      level_first = .data[[paste0("y", min(years))]],
      growth = dplyr::if_else(level_first > 0, (level_last / level_first) - 1, NA_real_)
    )
}

# Reporter i's exports to market j: the two-stage trade index of
# partnership_strength_two_stage_trade_index() (partnership_strength_helpers.R; growth scored from
# a minimum first-year base, `trade_growth`). Returns reporter_iso, partner_iso, tech,
# supply_chain, trade_index and trade_status.
partnership_strength_build_export_indices <- function(ds_export, trade_growth = partnership_strength_trade_growth_settings()) {
  partnership_strength_two_stage_trade_index(ds_export, "trade_index", trade_growth = trade_growth)
}

partnership_strength_build_opportunity_dyads <- function(trade_indices,
                                                         econ_opp_index,
                                                         energy_security_index,
                                                         tech_ghg,
                                                         policy,
                                                         country_info,
                                                         component_weights = NULL,
                                                         techs = NULL,
                                                         no_data_fill = "dyad") {
  component_weights <- partnership_strength_resolve_opportunity_weights(component_weights)
  climate_policy_weight <- component_weights$climate_policy_weight

  # One value per (ISO3, tech, stage) before any ranking; the ES/EO tables file some economies
  # under several spellings (partnership_strength_country_iso_table()).
  econ_opp_iso <- partnership_strength_country_iso_table(
    econ_opp_index,
    "Economic_Opportunity_Index",
    country_info,
    techs = techs
  ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = median_scurve(Economic_Opportunity_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(exporter_iso = iso3c, tech, supply_chain, econ_opp_raw = value)

  # Partner (market) need: 1 - the partner's cross-country ES rank in (tech, stage). Higher = a
  # less secure market = more room for the reporter's exports. Not a security score.
  energy_sec_iso <- partnership_strength_country_iso_table(
    energy_security_index,
    "Energy_Security_Index",
    country_info,
    techs = techs
  ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = median_scurve(Energy_Security_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(partner_iso = iso3c, tech, supply_chain, energy_sec_raw = 1 - value)
  # The partner's ES cell imputed share, for the imputed weight share below (NULL when the ES
  # table predates the rankability rule).
  es_partner_share <- partnership_strength_pillar_imputed_share(
    energy_security_index, country_info, "es_partner_imputed", techs = techs
  )

  ghg_iso <- tech_ghg %>%
    dplyr::transmute(tech, ghg_index = as.numeric(ghg_index))

  policy_iso <- policy %>%
    dplyr::mutate(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info)
    ) %>%
    dplyr::filter(!is.na(iso3c)) %>%
    dplyr::transmute(iso3c, climate_policy_index) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarize(climate_policy_index = mean(climate_policy_index, na.rm = TRUE), .groups = "drop")

  default_ghg <- if (nrow(ghg_iso)) median(ghg_iso$ghg_index, na.rm = TRUE) else 0.5
  default_policy <- if (nrow(policy_iso)) median(policy_iso$climate_policy_index, na.rm = TRUE) else 0.5

  reporter_keys <- econ_opp_iso %>%
    dplyr::distinct(reporter_iso = exporter_iso, tech, supply_chain)
  partner_keys <- energy_sec_iso %>%
    dplyr::distinct(partner_iso, tech, supply_chain)

  dyad_backbone <- reporter_keys %>%
    dplyr::inner_join(partner_keys, by = c("tech", "supply_chain"), relationship = "many-to-many") %>%
    # A country is not its own export market; keep self-dyads out of every ranking.
    dplyr::filter(reporter_iso != partner_iso)

  trade_indices <- partnership_strength_with_trade_status(trade_indices, "trade_index")

  out <- dyad_backbone %>%
    dplyr::left_join(trade_indices, by = c("reporter_iso", "partner_iso", "tech", "supply_chain")) %>%
    # A dyad cell without an observed flow in the last theme year takes the (tech, stage) mean; a
    # (tech, stage) with no observed flow at all (no HS6 code mapped) is "no_data": 0.5 here, then
    # the dyad's own trade in its other cells (no_data_fill; partnership_strength_fill_no_data_trade()).
    dplyr::mutate(trade_status = dplyr::if_else(is.na(trade_index), "imputed", trade_status)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      trade_status = if (all(trade_status %in% "imputed")) rep("no_data", dplyr::n()) else trade_status,
      trade_index = dplyr::coalesce(
        trade_index,
        dplyr::if_else(
          is.nan(mean(trade_index, na.rm = TRUE)),
          0.5,
          mean(trade_index, na.rm = TRUE)
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    partnership_strength_fill_no_data_trade("trade_index", fill = no_data_fill) %>%
    dplyr::left_join(econ_opp_iso, by = c("reporter_iso" = "exporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(energy_sec_iso, by = c("partner_iso", "tech", "supply_chain")) %>%
    partnership_strength_join_pillar_share(es_partner_share, "partner_iso", "es_partner_imputed") %>%
    dplyr::left_join(ghg_iso, by = "tech") %>%
    dplyr::left_join(policy_iso, by = c("partner_iso" = "iso3c")) %>%
    dplyr::filter(!is.na(econ_opp_raw), !is.na(energy_sec_raw)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      econ_opp_index = partnership_strength_safe_scurve(econ_opp_raw),
      # Called energy_security_index until 2026-09; it is the partner's INsecurity (need).
      partner_es_need = partnership_strength_safe_scurve(energy_sec_raw),
      ghg_index = dplyr::coalesce(ghg_index, default_ghg),
      climate_policy_index = dplyr::coalesce(climate_policy_index, default_policy)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      opportunity_raw = partnership_strength_row_weighted_mean(
        list(trade_index, econ_opp_index, partner_es_need),
        c(
          component_weights$trade_index,
          component_weights$econ_opp_index,
          component_weights$partner_es_need
        )
      ),
      penalty = (1 - ghg_index) * climate_policy_index * climate_policy_weight,
      opportunity_index_raw = pmax(0, opportunity_raw - penalty),
      # Share of the cell's partner- and pair-specific component weight resting on imputed
      # inputs (partnership_strength_imputed_weight_share(), partnership_strength_helpers.R): trade
      # imputed at the cell mean, the partner's ES cell's own imputed share. The reporter's
      # econ_opp_index is left out, as is the trade component of a no_data (tech, stage).
      imputed_weight_share = partnership_strength_imputed_weight_share(
        list(
          partnership_strength_trade_unobserved(trade_status),
          partnership_strength_pillar_unobserved(partner_es_need, es_partner_imputed)
        ),
        c(component_weights$trade_index, component_weights$partner_es_need)
      )
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(opportunity_index = partnership_strength_safe_scurve(opportunity_index_raw)) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(opportunity_index_country = partnership_strength_safe_scurve(opportunity_index_raw)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      trade_index,
      econ_opp_index,
      partner_es_need,
      ghg_index,
      climate_policy_index,
      penalty,
      opportunity_raw,
      opportunity_index_raw,
      opportunity_index,
      opportunity_index_country,
      trade_status,
      trade_fill,
      imputed_weight_share
    )

  partnership_strength_assert_unique(
    out,
    c("reporter_iso", "partner_iso", "tech", "supply_chain"),
    label = "opportunity dyads"
  )
}

# Resolve the opportunity weights: the three component weights plus climate_policy_weight.
# The partner-need component was keyed energy_security_index before it was renamed; an old
# config using that key is still read, with a warning.
partnership_strength_resolve_opportunity_weights <- function(component_weights = NULL) {
  defaults <- partnership_strength_opportunity_default_weights()
  if (is.null(component_weights) || length(component_weights) == 0) {
    return(defaults)
  }
  component_weights <- as.list(component_weights)
  if (!is.null(component_weights$energy_security_index) && is.null(component_weights$partner_es_need)) {
    warning(
      "Opportunity weight 'energy_security_index' is now 'partner_es_need' (the partner's energy-security need); ",
      "reading it under the new name.",
      call. = FALSE
    )
    component_weights$partner_es_need <- component_weights$energy_security_index
  }
  component_weights$energy_security_index <- NULL
  if (is.null(component_weights$climate_policy_weight)) {
    component_weights$climate_policy_weight <- defaults$climate_policy_weight
  }
  partnership_strength_resolve_weights(
    component_weights[intersect(names(defaults), names(component_weights))],
    defaults,
    "Opportunity component"
  )
}

partnership_strength_build_opportunity_country <- function(opportunity_all,
                                                           country_info,
                                                           top_n = 3,
                                                           year = 2024L) {
  opportunity_all %>%
    dplyr::filter(!is.na(opportunity_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = opportunity_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(value = mean(opportunity_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "opportunity",
      variable = "Opportunity Index",
      data_type = "index",
      value,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average of the top opportunity dyads by partner country."
    )
}

partnership_strength_build_opportunity_inputs_country <- function(opportunity_all,
                                                                  country_info,
                                                                  component_weights,
                                                                  top_n = 3,
                                                                  year = 2024L) {
  component_weights_tbl <- tibble::tibble(
    component_key = c("trade_index", "econ_opp_index", "partner_es_need"),
    component_weight = c(
      component_weights$trade_index,
      component_weights$econ_opp_index,
      component_weights$partner_es_need
    )
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  top_dyads <- opportunity_all %>%
    dplyr::filter(!is.na(opportunity_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = opportunity_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(partner_iso, reporter_iso, tech, supply_chain) %>%
    dplyr::distinct()

  if (nrow(top_dyads) == 0) {
    return(tibble::tibble(
      Country = character(),
      tech = character(),
      supply_chain = character(),
      category = character(),
      variable = character(),
      data_type = character(),
      value = numeric(),
      component_weight = numeric(),
      component_weight_share = numeric(),
      weighted_component = numeric(),
      Year = integer(),
      source = character(),
      explanation = character()
    ))
  }

  opportunity_all %>%
    dplyr::distinct(
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      trade_index,
      econ_opp_index,
      partner_es_need,
      .keep_all = TRUE
    ) %>%
    dplyr::inner_join(top_dyads, by = c("partner_iso", "reporter_iso", "tech", "supply_chain")) %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::select(
      Country,
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      trade_index,
      econ_opp_index,
      partner_es_need
    ) %>%
    tidyr::pivot_longer(
      cols = c(trade_index, econ_opp_index, partner_es_need),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(weighted_component = value * component_weight_share) %>%
    dplyr::group_by(
      Country,
      tech,
      supply_chain,
      component_key,
      component_weight,
      component_weight_share
    ) %>%
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      weighted_component = mean(weighted_component, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      category = "opportunity",
      variable = component_key,
      data_type = "index",
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average component contribution across top opportunity dyads."
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      component_weight,
      component_weight_share,
      weighted_component,
      Year,
      source,
      explanation
    )
}

partnership_strength_build_opportunity_dyads_table <- function(opportunity_all,
                                                               country_info,
                                                               component_weights,
                                                               year = 2024L) {
  component_weights_tbl <- tibble::tibble(
    component_key = c("trade_index", "econ_opp_index", "partner_es_need"),
    component_weight = c(
      component_weights$trade_index,
      component_weights$econ_opp_index,
      component_weights$partner_es_need
    )
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  opportunity_all %>%
    dplyr::mutate(
      Reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      Partner = partnership_strength_iso_to_country(partner_iso, country_info),
      Country = Partner,
      # Weights of the components this dyad has; opportunity_raw renormalizes over them, so the
      # contributions below do too and sum to opportunity_raw. The negations are parenthesized:
      # R's `!` binds looser than `*` and `+`.
      weight_available = component_weights$trade_index * (!is.na(trade_index)) +
        component_weights$econ_opp_index * (!is.na(econ_opp_index)) +
        component_weights$partner_es_need * (!is.na(partner_es_need))
    ) %>%
    dplyr::select(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      weight_available,
      trade_index,
      econ_opp_index,
      partner_es_need,
      opportunity_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(trade_index, econ_opp_index, partner_es_need, opportunity_index),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(
      variable = dplyr::if_else(component_key == "opportunity_index", "Opportunity Index", component_key),
      category = "opportunity",
      data_type = "index",
      weighted_component = dplyr::if_else(
        !is.na(component_weight_share) & weight_available > 0,
        value * component_weight / weight_available,
        NA_real_
      ),
      Year = as.integer(year),
      source = "Author calculation",
      explanation = dplyr::if_else(
        component_key == "opportunity_index",
        "Opportunity index: partner rated as an export market for the reporter (reporter exports to partner).",
        "Opportunity component: partner rated as an export market for the reporter (reporter exports to partner)."
      )
    ) %>%
    dplyr::select(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      component_weight,
      component_weight_share,
      weighted_component,
      Year,
      source,
      explanation
    )
}

prosperous_opportunity <- function(comtrade_dyads,
                                   subcat,
                                   econ_opp_index,
                                   energy_security_index,
                                   tech_ghg,
                                   policy,
                                   country_info,
                                   component_weights = NULL,
                                   years = 2020:2024,
                                   top_n = 3,
                                   techs = NULL,
                                   trade_cells = NULL,
                                   cif_fob_factor = PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR,
                                   trade_growth = partnership_strength_trade_growth_settings(),
                                   no_data_fill = "dyad") {
  component_weights <- partnership_strength_resolve_opportunity_weights(component_weights)

  # The export side of the reconciled flow matrix Friendshore reads (importer's report, else the
  # exporter's own as a mirror; partnership_strength_build_trade_cells()).
  if (is.null(trade_cells)) {
    trade_cells <- partnership_strength_build_trade_cells(comtrade_dyads, subcat, years = years, cif_fob_factor = cif_fob_factor)
  }
  ds_export <- partnership_strength_trade_view(trade_cells, view = "export")
  rm(trade_cells)
  trade_indices <- partnership_strength_build_export_indices(ds_export, trade_growth = trade_growth)
  rm(ds_export)
  opportunity_all <- partnership_strength_build_opportunity_dyads(
    trade_indices,
    econ_opp_index,
    energy_security_index,
    tech_ghg,
    policy,
    country_info,
    component_weights = component_weights,
    techs = techs,
    no_data_fill = no_data_fill
  )

  opportunity_dyads <- partnership_strength_build_opportunity_dyads_table(
    opportunity_all,
    country_info,
    component_weights = component_weights,
    year = max(years)
  )

  variable_tbl <- opportunity_dyads %>%
    dplyr::mutate(
      reporter = Reporter,
      partner = Partner
    ) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
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

  contributions_tbl <- opportunity_dyads %>%
    dplyr::mutate(
      reporter = Reporter,
      partner = Partner,
      data_type = "contribution",
      contribution = dplyr::coalesce(weighted_component, value)
    ) %>%
    dplyr::filter(!is.na(contribution)) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      contribution,
      Year,
      source,
      explanation
    )
  # Trade coverage of the dyad cells (observed trade, recorded zero or imputed), by (tech, stage).
  trade_coverage <- partnership_strength_trade_coverage(opportunity_all, theme = "opportunity")
  # Each dyad's imputed weight share over the cells its top-5 Opportunity Index averages; the
  # PSI's rankability rule reads it off the top-5 table.
  dyad_share <- partnership_strength_top_k_share(
    opportunity_all,
    c("reporter_iso", "partner_iso"),
    "opportunity_index",
    k = 5L
  )
  # The long tables run to ~5M rows on the full build; release the intermediates before the
  # summaries.
  rm(opportunity_all, opportunity_dyads)

  partnership_strength_assert_unique(
    variable_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable"),
    label = "partner_opportunity_tbl"
  )

  top5_tbl <- partnership_strength_build_top_k(
    variable_tbl,
    c("reporter_iso", "partner_iso"),
    k = 5L,
    year = max(years)
  ) %>%
    dplyr::mutate(
      reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      partner = partnership_strength_iso_to_country(partner_iso, country_info),
      explanation = "Mean of the dyad's five highest (tech, stage) values; partner rated as an export market for the reporter."
    ) %>%
    partnership_strength_add_top5_share(dyad_share, c("reporter_iso", "partner_iso"), "Opportunity Index") %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      category,
      variable,
      data_type,
      value,
      imputed_weight_share,
      Year,
      source,
      explanation
    )

  country_tbl <- partnership_strength_build_partner_rollup(
    variable_tbl,
    index_variable = "Opportunity Index",
    year = max(years),
    explanation = "Mean over reporters of the dyad Opportunity Index: partner as an export market."
  )

  list(
    variable = variable_tbl,
    contributions = contributions_tbl,
    top5 = top5_tbl,
    country = country_tbl,
    trade_coverage = trade_coverage
  )
}
