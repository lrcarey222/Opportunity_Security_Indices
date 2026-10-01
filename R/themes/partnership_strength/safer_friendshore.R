# Safer friendshore theme (dyadic friendshoring index).
#
# Direction convention. In every friendshore table the reporter (reporter_iso) is the country
# that sources a technology and the partner (partner_iso) is the candidate supplier:
# Friendshore(i, j, t, s) rates partner j as a supplier of (t, s) to reporter i, from i's
# reported imports from j, i's own energy-security need, j's economic opportunity (its
# capability as a producer) and i's outward direct-investment position in j, less a climate
# penalty. The Opportunity theme uses the other trade flow (reporter = exporter, partner =
# market; prosperous_opportunity.R), so FS(i, j) and OP(i, j) both rate j from i's side. The
# tables are not transposed here: a consumer that reads an edge as producer -> consumer must use
# FS(j, i) for the producer j supplying consumer i.
#
# Universe: reporters are the countries with an ES or EO record, partners those with an EO
# record, both collapsed to one row per ISO3; self-dyads are excluded. The trade component is
# observed only for the Comtrade reporters; other dyads take the (tech, stage) mean.

# Code defaults; the pipeline passes config/weights.yml partnership_friendshore_components.
partnership_strength_friendshore_default_weights <- function() {
  list(
    imp_trade_index = 1,
    econ_opp_raw = 1,
    es_need = 2,
    eo_partner = 1.5,
    outbound_index = 1.5
  )
}

partnership_strength_build_import_indices <- function(ds_import) {
  ds_import %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      import_index = partnership_strength_safe_scurve(level_last),
      import_growth_idx = partnership_strength_safe_scurve(growth),
      imp_num = 2 * import_index + 1 * import_growth_idx,
      imp_den = 2 * (!is.na(import_index)) + 1 * (!is.na(import_growth_idx)),
      imp_trade_index = dplyr::if_else(imp_den > 0, imp_num / imp_den, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::mutate(imp_trade_index = partnership_strength_safe_scurve(imp_trade_index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(reporter_iso, partner_iso, tech, supply_chain, imp_trade_index)
}

# Reporter i's outward direct-investment (debt instrument) position in partner j, from IMF
# Direct Investment Positions (DIP). One row per (reporter_iso, partner_iso) with a reported
# position; dyads without one are absent (the friendshore composite then drops the component
# rather than scoring it 0).
#
# - The position is a stock, so it is the mean of the reported annual positions over `years`
#   (all year columns when NULL). Summing the stock over 16 years double-counted it and grew with
#   the length of a reporter's history.
# - A row with no reported year is missing. DIP reporters file explicit zeros (about half of the
#   outward-debt rows), so a reported zero is a real zero; an absent row is not evidence of one.
# - outbound_index = within-reporter S-curve of the mean of three S-curved measures: the position,
#   the position relative to partner GDP, and the position relative to both GDPs. A measure that
#   needs a missing GDP (e.g. Taiwan) drops out of that mean. A zero (or negative) position
#   enters that ranking as one tie block below every positive position, so it takes the block's
#   mid-rank like any tied value in the index: 0 where it is the reporter's only zero, ~0.5 where
#   the reporter files zeros for nearly every counterpart.
partnership_strength_build_outbound_edges <- function(fdi_raw,
                                                      country_info,
                                                      gdp_data,
                                                      gdp_year = 2023,
                                                      years = NULL) {
  require_columns(
    fdi_raw,
    c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"),
    label = "fdi_raw"
  )
  require_columns(gdp_data, c("iso3c", "year", "NY.GDP.MKTP.CD"), label = "gdp_data")

  # Year columns are "X2009" from read.csv() and "2009" from fread().
  year_cols <- grep("^X?[0-9]{4}$", names(fdi_raw), value = TRUE)
  if (!is.null(years)) {
    year_cols <- year_cols[as.integer(sub("^X", "", year_cols)) %in% years]
  }
  if (length(year_cols) == 0) {
    stop("fdi_raw has no year columns for the requested years.")
  }

  # Regional aggregates and unallocated positions are not counterpart countries.
  aggregates <- c(
    "World",
    "Not Specified (including Confidential)",
    "Europe",
    "North and Central America",
    "Oceania and Polar Regions",
    "Other Near and Middle East Economies",
    "Economies of Persian Gulf",
    "Central and South Asia",
    "East Asia",
    "North Africa",
    "North Atlantic and Caribbean",
    "South America",
    "Sub-Saharan Africa",
    "US Pacific Islands"
  )
  # Financial centres whose positions are pass-through, excluded as counterparts (the same set as
  # before, now matched on ISO3 so IMF spellings cannot slip through).
  tax_haven_iso <- c("VGB", "CYM", "BMU", "LUX", "BHS", "BRB", "TCA", "IRL", "IMN", "GGY", "CYP", "MUS")

  fdi <- fdi_raw %>%
    dplyr::filter(
      DV_TYPE == "Reported official data",
      INDICATOR == "Outward Direct investment, Assets (gross), Debt instruments, All entities"
    )

  positions <- vapply(
    year_cols,
    function(col) suppressWarnings(as.numeric(fdi[[col]])),
    numeric(nrow(fdi))
  )
  positions <- matrix(positions, nrow = nrow(fdi))
  n_reported <- rowSums(!is.na(positions))
  total <- ifelse(n_reported > 0, rowSums(positions, na.rm = TRUE) / pmax(n_reported, 1), NA_real_)

  # IMF names are matched through the shared name -> ISO3 mapping (WDI names plus countrycode);
  # the former exact join on WDI names dropped ~100 IMF spellings, among them China, the
  # Netherlands, Poland and Tuerkiye, which then scored 0 for every reporter.
  imf_names <- unique(c(fdi$COUNTRY, fdi$COUNTERPART_COUNTRY))
  imf_clean <- partnership_strength_clean_outbound_countries(imf_names)
  imf_iso <- partnership_strength_country_to_iso(imf_clean, country_info)
  imf_iso[imf_clean %in% aggregates] <- NA_character_

  gdp_latest <- gdp_data %>%
    dplyr::filter(as.character(year) == as.character(gdp_year)) %>%
    dplyr::transmute(iso3c = as.character(iso3c), GDP = suppressWarnings(as.numeric(NY.GDP.MKTP.CD))) %>%
    dplyr::filter(!is.na(GDP), GDP > 0) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)

  tibble::tibble(
    reporter_iso = imf_iso[match(fdi$COUNTRY, imf_names)],
    partner_iso = imf_iso[match(fdi$COUNTERPART_COUNTRY, imf_names)],
    total = total
  ) %>%
    dplyr::filter(
      !is.na(reporter_iso),
      !is.na(partner_iso),
      !is.na(total),
      reporter_iso != partner_iso,
      !partner_iso %in% tax_haven_iso
    ) %>%
    # Two IMF spellings of one economy would otherwise give a dyad two positions.
    dplyr::group_by(reporter_iso, partner_iso) %>%
    dplyr::summarise(total = mean(total), .groups = "drop") %>%
    dplyr::left_join(dplyr::rename(gdp_latest, GDP_reporter = GDP), by = c("reporter_iso" = "iso3c")) %>%
    dplyr::left_join(dplyr::rename(gdp_latest, GDP_partner = GDP), by = c("partner_iso" = "iso3c")) %>%
    dplyr::mutate(
      GDP_cp = GDP_partner / 1e6,
      GDP = GDP_reporter / 1e6,
      total_counterpart_share = total / GDP_cp,
      outbound_relative = (total / GDP) / GDP_cp
    ) %>%
    dplyr::mutate(
      total_index = partnership_strength_safe_scurve(total),
      total_share_index = partnership_strength_safe_scurve(total_counterpart_share),
      outbound_relative_index = partnership_strength_safe_scurve(outbound_relative),
      # Mean of the three measures. This read mean(total_index:outbound_relative_index), R's
      # sequence operator on two scalars, which returned total_index alone.
      outbound_index = partnership_strength_row_weighted_mean(
        list(total_index, total_share_index, outbound_relative_index),
        c(1, 1, 1)
      ),
      # Zero and negative positions are one tie block below every positive one. Their three-measure
      # means differed slightly (a measure dropped for a missing GDP is ranked over fewer rows),
      # which split identical zeros into sub-blocks: Algeria's zeros scored 0.46 or 0.76.
      outbound_index = dplyr::if_else(total <= 0, 0, outbound_index)
    ) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(outbound_index = partnership_strength_safe_scurve(outbound_index)) %>%
    dplyr::ungroup() %>%
    dplyr::select(reporter_iso, partner_iso, outbound_index)
}

partnership_strength_build_friendshore_dyads <- function(import_indices,
                                                         econ_opp_index,
                                                         energy_security_index,
                                                         outbound_edges,
                                                         tech_ghg,
                                                         policy,
                                                         country_info,
                                                         component_weights = NULL,
                                                         techs = NULL) {
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    partnership_strength_friendshore_default_weights(),
    "Friendshore component"
  )

  # One value per (ISO3, tech, stage) before any ranking; the ES/EO tables file some economies
  # under several spellings (partnership_strength_country_iso_table()).
  es_cells <- partnership_strength_country_iso_table(
    energy_security_index,
    "Energy_Security_Index",
    country_info,
    techs = techs
  )
  eo_cells <- partnership_strength_country_iso_table(
    econ_opp_index,
    "Economic_Opportunity_Index",
    country_info,
    techs = techs
  )

  # Reporter need: 1 - S-curve of the reporter's ES across its own (tech, stage) cells, i.e. which
  # of i's technologies are least secure relative to i's others (not relative to other countries).
  es_iso <- es_cells %>%
    dplyr::group_by(iso3c) %>%
    dplyr::mutate(value = median_scurve(Energy_Security_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(reporter_iso = iso3c, tech, supply_chain, es_need = 1 - value)

  eo_partner_iso <- eo_cells %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = median_scurve(Economic_Opportunity_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(partner_iso = iso3c, tech, supply_chain, eo_partner = value)

  econ_opp_iso <- eo_cells %>%
    dplyr::group_by(iso3c) %>%
    dplyr::mutate(value = median_scurve(Economic_Opportunity_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(exporter_iso = iso3c, tech, supply_chain, econ_opp_raw = value)

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

  reporter_keys <- dplyr::bind_rows(
    es_iso %>% dplyr::distinct(reporter_iso, tech, supply_chain),
    econ_opp_iso %>% dplyr::distinct(reporter_iso = exporter_iso, tech, supply_chain)
  ) %>%
    dplyr::distinct(reporter_iso, tech, supply_chain)
  partner_keys <- eo_partner_iso %>%
    dplyr::distinct(partner_iso, tech, supply_chain)

  dyad_backbone <- reporter_keys %>%
    dplyr::inner_join(partner_keys, by = c("tech", "supply_chain"), relationship = "many-to-many") %>%
    # A country is not its own friendshore partner; keep self-dyads out of every ranking.
    dplyr::filter(reporter_iso != partner_iso)

  out <- dyad_backbone %>%
    dplyr::left_join(import_indices, by = c("reporter_iso", "partner_iso", "tech", "supply_chain")) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      imp_trade_index = dplyr::coalesce(
        imp_trade_index,
        dplyr::if_else(
          is.nan(mean(imp_trade_index, na.rm = TRUE)),
          0.5,
          mean(imp_trade_index, na.rm = TRUE)
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(es_iso, by = c("tech", "supply_chain", "reporter_iso")) %>%
    dplyr::left_join(econ_opp_iso, by = c("reporter_iso" = "exporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(eo_partner_iso, by = c("tech", "supply_chain", "partner_iso")) %>%
    dplyr::left_join(outbound_edges, by = c("reporter_iso", "partner_iso")) %>%
    dplyr::left_join(dplyr::rename(policy_iso, reporter_iso = iso3c, cpi_r = climate_policy_index), by = "reporter_iso") %>%
    dplyr::left_join(dplyr::rename(policy_iso, partner_iso = iso3c, cpi_p = climate_policy_index), by = "partner_iso") %>%
    dplyr::left_join(ghg_iso, by = "tech") %>%
    dplyr::mutate(
      cpi_r = dplyr::coalesce(cpi_r, default_policy),
      cpi_p = dplyr::coalesce(cpi_p, default_policy),
      gh_entry = dplyr::coalesce(ghg_index, default_ghg),
      # outbound_index stays NA where the dyad has no IMF DIP position (most dyads), so the
      # weighted mean drops it and renormalizes. It used to be coalesced to 0, which scored a
      # missing record below a reported zero position (~0.3-0.4 after ranking).
      fsi_raw = partnership_strength_row_weighted_mean(
        list(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index),
        c(
          component_weights$imp_trade_index,
          component_weights$econ_opp_raw,
          component_weights$es_need,
          component_weights$eo_partner,
          component_weights$outbound_index
        )
      ),
      penalty_r = (1 - gh_entry) * cpi_r * 0.10,
      penalty_p = (1 - gh_entry) * cpi_p * 0.10,
      fsi_adj = pmax(0, fsi_raw - (penalty_r + penalty_p))
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(friendshore_index = partnership_strength_safe_scurve(fsi_adj)) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(friendshore_index_country = partnership_strength_safe_scurve(fsi_adj)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      penalty_r,
      penalty_p,
      fsi_raw,
      fsi_adj,
      friendshore_index,
      friendshore_index_country
    )

  partnership_strength_assert_unique(
    out,
    c("reporter_iso", "partner_iso", "tech", "supply_chain"),
    label = "friendshore dyads"
  )
}

# Top-3 reporter roll-up by partner; not called by the pipeline (scripts/15 writes the
# mean-over-reporters roll-up from partnership_strength_build_partner_rollup() instead).
partnership_strength_build_friendshore_country <- function(friendshore_all,
                                                           country_info,
                                                           top_n = 3,
                                                           year = 2024L) {
  friendshore_all %>%
    dplyr::filter(!is.na(friendshore_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = friendshore_index, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(value = mean(friendshore_index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(partner_iso, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "friendshore",
      variable = "Friendshore Index",
      data_type = "index",
      value,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average of the top friendshore dyads by partner country."
    )
}

partnership_strength_build_friendshore_inputs_country <- function(friendshore_all,
                                                                  country_info,
                                                                  component_weights,
                                                                  top_n = 3,
                                                                  year = 2024L) {
  component_weights_tbl <- tibble::tibble(
    component_key = c("imp_trade_index", "econ_opp_raw", "es_need", "eo_partner", "outbound_index"),
    component_weight = c(
      component_weights$imp_trade_index,
      component_weights$econ_opp_raw,
      component_weights$es_need,
      component_weights$eo_partner,
      component_weights$outbound_index
    )
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  top_dyads <- friendshore_all %>%
    dplyr::filter(!is.na(friendshore_index)) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::slice_max(order_by = friendshore_index, n = top_n, with_ties = FALSE) %>%
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

  friendshore_all %>%
    dplyr::distinct(
      partner_iso,
      reporter_iso,
      tech,
      supply_chain,
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
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
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index),
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
      category = "friendshore",
      variable = component_key,
      data_type = "index",
      Year = as.integer(year),
      source = "Author calculation",
      explanation = "Average component contribution across top friendshore dyads."
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

partnership_strength_build_friendshore_dyads_table <- function(friendshore_all,
                                                               country_info,
                                                               component_weights,
                                                               year = 2024L) {
  component_weights_tbl <- tibble::tibble(
    component_key = c("imp_trade_index", "econ_opp_raw", "es_need", "eo_partner", "outbound_index"),
    component_weight = c(
      component_weights$imp_trade_index,
      component_weights$econ_opp_raw,
      component_weights$es_need,
      component_weights$eo_partner,
      component_weights$outbound_index
    )
  ) %>%
    dplyr::mutate(component_weight_share = component_weight / sum(component_weight))

  friendshore_all %>%
    dplyr::mutate(
      Reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      Partner = partnership_strength_iso_to_country(partner_iso, country_info),
      Country = Partner,
      # Weights of the components this dyad has; fsi_raw renormalizes over them, so the
      # contributions below do too and sum to fsi_raw. The negations are parenthesized: R's `!`
      # binds looser than `*` and `+`.
      weight_available = component_weights$imp_trade_index * (!is.na(imp_trade_index)) +
        component_weights$econ_opp_raw * (!is.na(econ_opp_raw)) +
        component_weights$es_need * (!is.na(es_need)) +
        component_weights$eo_partner * (!is.na(eo_partner)) +
        component_weights$outbound_index * (!is.na(outbound_index))
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
      imp_trade_index,
      econ_opp_raw,
      es_need,
      eo_partner,
      outbound_index,
      friendshore_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(imp_trade_index, econ_opp_raw, es_need, eo_partner, outbound_index, friendshore_index),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(component_weights_tbl, by = "component_key") %>%
    dplyr::mutate(
      variable = dplyr::if_else(component_key == "friendshore_index", "Friendshore Index", component_key),
      category = "friendshore",
      data_type = "index",
      weighted_component = dplyr::if_else(
        !is.na(component_weight_share) & weight_available > 0,
        value * component_weight / weight_available,
        NA_real_
      ),
      Year = as.integer(year),
      source = "Author calculation",
      explanation = dplyr::if_else(
        component_key == "friendshore_index",
        "Friendshore index: partner rated as a supplier to the reporter (reporter imports from partner).",
        "Friendshore component: partner rated as a supplier to the reporter (reporter imports from partner)."
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

safer_friendshore <- function(comtrade_dyads,
                              subcat,
                              econ_opp_index,
                              energy_security_index,
                              tech_ghg,
                              policy,
                              fdi_raw,
                              country_info,
                              gdp_data,
                              component_weights = NULL,
                              years = 2020:2024,
                              top_n = 3,
                              gdp_year = 2023,
                              techs = NULL) {
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    partnership_strength_friendshore_default_weights(),
    "Friendshore component"
  )

  res_tech <- partnership_strength_clean_trade_data(comtrade_dyads, subcat, years = years)
  ds_import <- partnership_strength_build_dyad_series(res_tech, flow = "import", years = years)
  import_indices <- partnership_strength_build_import_indices(ds_import)

  # DIP positions averaged over the same window as the trade data.
  outbound_edges <- partnership_strength_build_outbound_edges(
    fdi_raw,
    country_info,
    gdp_data,
    gdp_year = gdp_year,
    years = years
  )

  friendshore_all <- partnership_strength_build_friendshore_dyads(
    import_indices,
    econ_opp_index,
    energy_security_index,
    outbound_edges,
    tech_ghg,
    policy,
    country_info,
    component_weights = component_weights,
    techs = techs
  )

  friendshore_dyads <- partnership_strength_build_friendshore_dyads_table(
    friendshore_all,
    country_info,
    component_weights = component_weights,
    year = max(years)
  )

  variable_tbl <- friendshore_dyads %>%
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

  contributions_tbl <- friendshore_dyads %>%
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
  # The long tables run to ~10M rows on the full build; release the intermediates before the
  # summaries.
  rm(friendshore_all, friendshore_dyads)

  partnership_strength_assert_unique(
    variable_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable"),
    label = "partner_friendshore_tbl"
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
      explanation = "Mean of the dyad's five highest (tech, stage) values; partner rated as a supplier to the reporter."
    ) %>%
    dplyr::select(
      reporter,
      partner,
      reporter_iso,
      partner_iso,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )

  country_tbl <- partnership_strength_build_partner_rollup(
    variable_tbl,
    index_variable = "Friendshore Index",
    year = max(years),
    explanation = "Mean over reporters of the dyad Friendshore Index: partner as a supplier."
  )

  list(
    variable = variable_tbl,
    contributions = contributions_tbl,
    top5 = top5_tbl,
    country = country_tbl
  )
}
