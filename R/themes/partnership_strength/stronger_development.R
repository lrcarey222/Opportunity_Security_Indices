# Stronger development theme (development potential index).
#
# Level: partner country x (tech, stage). The index describes partner j alone (its enabling
# environment from WDI and Doing Business, its energy-security need, its economic opportunity
# and the ODA it receives); no reporter characteristic enters. It is therefore built and written
# once per (partner_iso, tech, supply_chain). It used to be copied to every reporter with any
# Comtrade trade with j and fanned out once per OECD donor (up to 38 rows per dyad cell), which
# is what corrupted the allied-network node join.
#
# Defined only for partners that are not World Bank high-income economies: the development pillar
# scores emerging and developing markets. High-income partners have no development value; the
# PSI composite averages over the categories a partner has, so they are not scored as 0.
#
# Development Potential Index = S-curve, within (tech, stage) across partners, of the weighted
# mean of the components a partner has (as the Technical Appendix specifies and as Friendshore
# and Opportunity are built). The raw weighted mean was used directly before; its scale
# (partner means ~0.26-0.60) sat well below the S-curved Friendshore and Opportunity top-5
# scores, so once the PSI stopped scoring the missing category as 0 it pulled every
# non-high-income partner down instead.
#
# Each partner cell also carries imputed_weight_share: the share of the component weight resting
# on missing indicators (counted in full) and on the partner's ES and EO cells at their own
# pillar imputed share (partnership_strength_imputed_weight_share(), partnership_strength_helpers.R).
# The top-5 table carries the partner's mean over its top-5 cells on the Development Potential
# Index rows (PSI rankability).

# Code defaults for the development components (config/weights.yml
# partnership_development_components overrides them).
partnership_strength_development_default_weights <- function() {
  list(
    development_index = 1,
    doingbusiness_index = 1,
    investment_enviro_index = 1,
    energy_security = 1,
    economic_opportunity = 1.5,
    aid_index_supplychain = 1.5,
    aidgdp_index_supplychain = 2,
    energy_use_index = 1
  )
}

partnership_strength_clean_index_country <- function(index_tbl,
                                                     index_candidates,
                                                     value_name,
                                                     country_info,
                                                     invert = FALSE,
                                                     techs = NULL) {
  require_columns(index_tbl, c("Country", "tech", "supply_chain"), label = value_name)
  index_col <- partnership_strength_pick_column(index_tbl, index_candidates, value_name)

  # One value per (ISO3, tech, stage): the ES/EO tables file some economies under several
  # spellings (partnership_strength_country_iso_table()).
  partnership_strength_country_iso_table(index_tbl, index_col, country_info, techs = techs) %>%
    dplyr::mutate(value = .data[[index_col]]) %>%
    dplyr::mutate(value = if (isTRUE(invert)) 1 - value else value) %>%
    dplyr::transmute(
      iso3c,
      tech,
      supply_chain,
      !!rlang::sym(value_name) := value
    )
}

partnership_strength_build_wdi_long <- function(wb_wdi, valid_iso = NULL) {
  country_name_col <- partnership_strength_pick_column(
    wb_wdi,
    c("Country.Name", "Country Name", "country", "country_name"),
    "country name"
  )
  country_code_col <- partnership_strength_pick_column(
    wb_wdi,
    c("Country.Code", "Country Code", "iso3c", "iso3", "country_code"),
    "country code"
  )
  series_name_col <- partnership_strength_pick_column(
    wb_wdi,
    c("Series.Name", "Series Name", "series_name"),
    "series name"
  )
  series_code_col <- partnership_strength_pick_column(
    wb_wdi,
    c("Series.Code", "Series Code", "series_code"),
    "series code"
  )

  value_cols <- setdiff(
    names(wb_wdi),
    c(country_name_col, country_code_col, series_name_col, series_code_col)
  )

  if (length(value_cols) == 0) {
    stop("WDI data missing value columns.")
  }

  # The WDI extract also carries regional and income-group aggregates (WLD, HIC, EAS, ...);
  # rank countries only.
  if (!is.null(valid_iso)) {
    wb_wdi <- wb_wdi[wb_wdi[[country_code_col]] %in% valid_iso, , drop = FALSE]
  }

  wb_wdi %>%
    dplyr::select(
      dplyr::all_of(c(country_name_col, country_code_col, series_name_col, series_code_col)),
      dplyr::all_of(value_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::group_by(
      .data[[country_name_col]],
      .data[[country_code_col]],
      .data[[series_name_col]],
      .data[[series_code_col]]
    ) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data[[series_name_col]], .data[[series_code_col]]) %>%
    dplyr::mutate(index = partnership_strength_safe_scurve(value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      Country.Name = !!rlang::sym(country_name_col),
      Country.Code = !!rlang::sym(country_code_col),
      Series.Name = !!rlang::sym(series_name_col),
      Series.Code = !!rlang::sym(series_code_col)
    )
}

partnership_strength_build_doing_business <- function(wb_doingbusiness, valid_iso = NULL) {
  country_name_col <- partnership_strength_pick_column(
    wb_doingbusiness,
    c("Country.Name", "Country Name", "country", "country_name"),
    "country name"
  )
  country_code_col <- partnership_strength_pick_column(
    wb_doingbusiness,
    c("Country.Code", "Country Code", "iso3c", "iso3", "country_code"),
    "country code"
  )
  series_code_col <- partnership_strength_pick_column(
    wb_doingbusiness,
    c("Series.Code", "Series Code", "series_code"),
    "series code"
  )

  value_cols <- setdiff(names(wb_doingbusiness), c(country_name_col, country_code_col, series_code_col))
  if (length(value_cols) == 0) {
    stop("Doing Business data missing value columns.")
  }

  if (!is.null(valid_iso)) {
    wb_doingbusiness <- wb_doingbusiness[wb_doingbusiness[[country_code_col]] %in% valid_iso, , drop = FALSE]
  }

  wb_doingbusiness %>%
    dplyr::filter(.data[[series_code_col]] == "IC.BUS.EASE.DFRN.XQ.DB1719") %>%
    dplyr::select(
      dplyr::all_of(c(country_name_col, country_code_col, series_code_col)),
      dplyr::all_of(value_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "year",
      values_to = "value"
    ) %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    dplyr::group_by(.data[[country_name_col]], .data[[country_code_col]]) %>%
    dplyr::summarize(doingbusiness = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(doingbusiness_index = partnership_strength_safe_scurve(doingbusiness)) %>%
    dplyr::rename(
      Country.Name = !!rlang::sym(country_name_col),
      Country.Code = !!rlang::sym(country_code_col)
    )
}

partnership_strength_build_development_indicators <- function(wb_wdi,
                                                              wb_doingbusiness,
                                                              valid_iso = NULL) {
  wb_wdi_long <- partnership_strength_build_wdi_long(wb_wdi, valid_iso = valid_iso)

  # Series where a higher value is a worse enabling environment.
  inverted_codes <- c(
    "FD.RES.LIQU.AS.ZS",
    "FB.BNK.CAPA.ZS",
    "FR.INR.DPST",
    "EG.IMP.CONS.ZS",
    "DT.DOD.DECT.GN.ZS",
    "IC.ELC.OUTG.ZS",
    "DC.DAC.USAL.CD",
    "DT.ODA.ODAT.PC.ZS",
    "TM.TAX.MRCH.WM.AR.ZS",
    "TM.TAX.MANF.WM.AR.ZS",
    "DT.TDS.DECT.GN.ZS",
    # Central government debt (% of GDP) and public debt service (% of exports) were scored
    # higher-is-better, unlike the external-debt series above.
    "GC.DOD.TOTL.GD.ZS",
    "DT.TDS.DPPF.XP.ZS"
  )

  # Series left out of the development basket:
  # - FP.CPI.TOTL is a price level indexed to 2010 = 100 in each country, and FM.AST.NFRG.CN is
  #   net foreign assets in local currency; neither is comparable across countries.
  # - BN.GSR.MRCH.CD (net goods trade, current US$) is a level that scales with the economy and
  #   has no development orientation (a surplus is not a better enabling environment).
  # - EG.USE.PCAP.KG.OE (energy use per capita) enters the index separately as
  #   energy_use_index; keeping it here counted it twice.
  excluded_codes <- c("FP.CPI.TOTL", "FM.AST.NFRG.CN", "BN.GSR.MRCH.CD", "EG.USE.PCAP.KG.OE")

  wb_wdi_long <- wb_wdi_long %>%
    dplyr::mutate(index = dplyr::if_else(Series.Code %in% inverted_codes, 1 - index, index))

  development_index <- wb_wdi_long %>%
    dplyr::filter(!Series.Code %in% excluded_codes) %>%
    dplyr::group_by(Country.Name, Country.Code) %>%
    dplyr::summarize(development_index = mean(index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(development_index = partnership_strength_safe_scurve(development_index)) %>%
    dplyr::rename(iso3c = Country.Code, country = Country.Name)

  doingbusiness_index <- partnership_strength_build_doing_business(wb_doingbusiness, valid_iso = valid_iso) %>%
    dplyr::transmute(
      iso3c = Country.Code,
      doingbusiness_index = doingbusiness_index
    )

  energy_use_index <- wb_wdi_long %>%
    dplyr::filter(Series.Code == "EG.USE.PCAP.KG.OE") %>%
    dplyr::transmute(
      iso3c = Country.Code,
      energy_use_index = index
    )

  development_index %>%
    dplyr::left_join(doingbusiness_index, by = "iso3c") %>%
    dplyr::mutate(
      investment_enviro_index = partnership_strength_safe_scurve(
        (development_index + doingbusiness_index) / 2
      )
    ) %>%
    dplyr::left_join(energy_use_index, by = "iso3c")
}

partnership_strength_build_oecd_sector <- function(oecd_raw, gdp_data, gdp_year = 2023) {
  donor_col <- partnership_strength_pick_column(
    oecd_raw,
    c("Donor", "donor"),
    "oecd donor"
  )
  recipient_iso_col <- partnership_strength_pick_column(
    oecd_raw,
    c("RECIPIENT", "RecipientISO", "recipient_iso", "recipient_iso3"),
    "oecd recipient iso"
  )
  recipient_col <- partnership_strength_pick_column(
    oecd_raw,
    c("Recipient", "recipient"),
    "oecd recipient"
  )
  sector_col <- partnership_strength_pick_column(
    oecd_raw,
    c("Sector", "sector"),
    "oecd sector"
  )
  value_col <- partnership_strength_pick_column(
    oecd_raw,
    c("OBS_VALUE", "Value", "value", "aid"),
    "oecd value"
  )
  measure_col <- intersect(c("Measure", "measure"), names(oecd_raw))

  group_vars <- c(donor_col, recipient_iso_col, recipient_col, sector_col)
  if (length(measure_col) > 0) {
    group_vars <- c(group_vars, measure_col[[1]])
  }

  oecd <- oecd_raw %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarize(aid = sum(as.numeric(.data[[value_col]]), na.rm = TRUE), .groups = "drop")

  oecd_sector <- oecd %>%
    dplyr::filter(.data[[sector_col]] != "Energy") %>%
    dplyr::mutate(
      sector_clean = dplyr::case_when(
        stringr::str_detect(.data[[sector_col]], "Nuclear") ~ "Nuclear",
        stringr::str_detect(.data[[sector_col]], "Solar") ~ "Solar",
        stringr::str_detect(.data[[sector_col]], "Wind") ~ "Wind",
        stringr::str_detect(.data[[sector_col]], "Geothermal") ~ "Geothermal",
        stringr::str_detect(.data[[sector_col]], "Coal") ~ "Coal",
        TRUE ~ .data[[sector_col]]
      )
    ) %>%
    dplyr::mutate(
      sector_clean = dplyr::recode(
        sector_clean,
        "Energy distribution" = "Electric Grid",
        "Energy generation, non-renewable sources" = "Fossil Generation",
        "Energy generation, renewable sources" = "Renewable Generation",
        "Electric power transmission and distribution (centralised grids)" = "Electric Grid",
        "Energy generation, renewable sources - multiple technologies" = "Renewable Generation",
        "Natural gas-fired electric power plants" = "Gas",
        "Electric power transmission and distribution (isolated mini-grids)" = "Electric Grid",
        "Hydro-electric power plants" = "Renewable Generation"
      )
    ) %>%
    dplyr::mutate(
      tech = dplyr::case_when(
        stringr::str_detect(sector_clean, stringr::regex("^Fossil Generation$", TRUE)) ~ "Coal,Gas",
        stringr::str_detect(sector_clean, stringr::regex("^Renewable Generation$", TRUE)) ~ "Solar,Wind,Batteries",
        stringr::str_detect(sector_clean, stringr::regex("^Electric Grid$", TRUE)) ~ "Electric Grid",
        stringr::str_detect(sector_clean, stringr::regex("non[- ]renewable", TRUE)) ~ "Coal,Gas",
        stringr::str_detect(sector_clean, stringr::regex("^Solar$", TRUE)) ~ "Solar",
        stringr::str_detect(sector_clean, stringr::regex("^Wind$", TRUE)) ~ "Wind",
        stringr::str_detect(sector_clean, stringr::regex("^Nuclear$", TRUE)) ~ "Nuclear",
        stringr::str_detect(sector_clean, stringr::regex("^Gas$", TRUE)) ~ "Gas",
        stringr::str_detect(sector_clean, stringr::regex("^Coal$", TRUE)) ~ "Coal",
        TRUE ~ NA_character_
      )
    ) %>%
    tidyr::separate_rows(tech, sep = ",") %>%
    dplyr::mutate(supply_chain = "Downstream")

  iso_col <- partnership_strength_pick_column(
    gdp_data,
    c("iso3c", "iso3", "Country.Code", "country_code"),
    "gdp iso"
  )
  year_col <- partnership_strength_pick_column(
    gdp_data,
    c("year", "Year"),
    "gdp year"
  )
  gdp_col <- partnership_strength_pick_column(
    gdp_data,
    c("NY.GDP.MKTP.CD", "gdp", "GDP"),
    "gdp value"
  )

  gdp_tbl <- gdp_data %>%
    dplyr::filter(.data[[year_col]] == gdp_year) %>%
    dplyr::transmute(iso3c = .data[[iso_col]], gdp = .data[[gdp_col]])

  oecd_sector %>%
    dplyr::left_join(
      gdp_tbl,
      by = stats::setNames("iso3c", recipient_iso_col)
    ) %>%
    dplyr::mutate(aid_gdp = aid / gdp) %>%
    dplyr::mutate(
      aid_index_sector = partnership_strength_safe_scurve(aid),
      aidgdp_index_sector = partnership_strength_safe_scurve(aid_gdp)
    ) %>%
    dplyr::transmute(
      iso3c = .data[[recipient_iso_col]],
      tech,
      supply_chain,
      aid_index_sector,
      aidgdp_index_sector
    )
}

# ODA received by partner j per supply-chain stage, from OECD CRS disbursements summed over the
# extract's years (2013-2024, the same window for every recipient) and over all donors: Energy ->
# Downstream, Industry -> Midstream, Mineral resources and mining less its coal and oil-and-gas
# sub-rows -> Upstream. aid_index_supplychain and aidgdp_index_supplychain are the
# S-curves of that total and of total / GDP across recipient countries within each stage.
#
# The donor dimension is summed here. It used to be dropped without aggregating, leaving one row
# per donor that the partner join then multiplied; the donor was never matched to the reporter,
# so the index was never dyadic.
partnership_strength_build_oecd_supplychain <- function(oecd_api_raw,
                                                        gdp_data,
                                                        gdp_year = 2023,
                                                        valid_iso = NULL) {
  oecd_api <- oecd_api_raw
  required_cols <- c("DonorISO", "RecipientISO", "Recipient", "Sector", "Year", "Value")
  if (!all(required_cols %in% names(oecd_api))) {
    if (ncol(oecd_api) < 29) {
      stop("OECD API data missing expected columns.")
    }
    names(oecd_api)[c(5, 7, 8, 10, 27, 29)] <- required_cols
  }

  oecd_api <- oecd_api %>%
    dplyr::select(dplyr::all_of(required_cols))

  iso_col <- partnership_strength_pick_column(
    gdp_data,
    c("iso3c", "iso3", "Country.Code", "country_code"),
    "gdp iso"
  )
  year_col <- partnership_strength_pick_column(
    gdp_data,
    c("year", "Year"),
    "gdp year"
  )
  gdp_col <- partnership_strength_pick_column(
    gdp_data,
    c("NY.GDP.MKTP.CD", "gdp", "GDP"),
    "gdp value"
  )

  gdp_tbl <- gdp_data %>%
    dplyr::filter(.data[[year_col]] == gdp_year) %>%
    dplyr::transmute(iso3c = .data[[iso_col]], gdp = .data[[gdp_col]])

  gdp_tbl <- gdp_tbl %>%
    dplyr::mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)

  sector <- stringr::str_to_lower(stringr::str_squish(as.character(oecd_api$Sector)))

  oecd_api %>%
    dplyr::mutate(
      # Matched case-insensitively: the extract spells it "Oil and gas (upstream)", which the old
      # "(Upstream)" filter missed, so fossil upstream aid fell through to Upstream a second time.
      # Coal and oil-and-gas (upstream) are sub-rows of "Mineral resources and mining" (the total
      # is at least their sum in every donor-recipient-year of the 2026 extract, and oil and gas
      # is ~80% of it), so they are subtracted: Upstream is the non-fossil mineral-resources aid
      # the old exclusion list intended.
      supply_chain = dplyr::case_when(
        sector == "energy" ~ "Downstream",
        sector == "industry" ~ "Midstream",
        sector %in% c("mineral resources and mining", "coal", "oil and gas (upstream)") ~ "Upstream",
        TRUE ~ NA_character_
      ),
      sign = dplyr::if_else(sector %in% c("coal", "oil and gas (upstream)"), -1, 1),
      Value = suppressWarnings(as.numeric(Value))
    ) %>%
    dplyr::filter(!is.na(supply_chain)) %>%
    # Regional recipients ("LDC", "... regional") are not countries.
    dplyr::filter(if (is.null(valid_iso)) TRUE else RecipientISO %in% valid_iso) %>%
    dplyr::group_by(RecipientISO, supply_chain) %>%
    dplyr::summarize(aid = pmax(0, sum(sign * Value, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::left_join(gdp_tbl, by = c("RecipientISO" = "iso3c")) %>%
    dplyr::mutate(aid_gdp = dplyr::if_else(!is.na(gdp) & gdp > 0, aid / gdp, NA_real_)) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::mutate(
      aid_index_supplychain = partnership_strength_safe_scurve(aid),
      aidgdp_index_supplychain = partnership_strength_safe_scurve(aid_gdp)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      iso3c = RecipientISO,
      supply_chain,
      aid_index_supplychain,
      aidgdp_index_supplychain
    )
}

partnership_strength_build_development_index <- function(energy_security_index,
                                                         economic_opportunity_index,
                                                         oecd_api_raw,
                                                         wb_wdi,
                                                         wb_doingbusiness,
                                                         country_info,
                                                         gdp_data,
                                                         gdp_year = 2023,
                                                         component_weights = NULL,
                                                         techs = NULL) {
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    partnership_strength_development_default_weights(),
    "Development component"
  )

  energy_security_clean <- partnership_strength_clean_index_country(
    energy_security_index,
    c("Energy_Security_Index", "energy_security_index", "value"),
    "energy_security",
    country_info,
    invert = TRUE,
    techs = techs
  )

  econ_opp_clean <- partnership_strength_clean_index_country(
    economic_opportunity_index,
    c("Economic_Opportunity_Index", "economic_opportunity_index", "value"),
    "economic_opportunity",
    country_info,
    techs = techs
  )

  # The United States' own EO for the (tech, stage), once a component here, is dropped: it is
  # the same for every partner in a cell, so it said nothing about the partner, only shifted
  # levels across cells (and it fixed a US perspective for all reporters).

  valid_iso <- country_info$iso3c

  oecd_supplychain <- partnership_strength_build_oecd_supplychain(
    oecd_api_raw,
    gdp_data,
    gdp_year = gdp_year,
    valid_iso = valid_iso
  )

  development_indicators <- partnership_strength_build_development_indicators(
    wb_wdi,
    wb_doingbusiness,
    valid_iso = valid_iso
  ) %>%
    dplyr::select(-dplyr::any_of("country"))

  eme_iso <- country_info %>%
    dplyr::filter(income != "High income") %>%
    dplyr::distinct(iso3c)

  base_tbl <- dplyr::full_join(
    energy_security_clean,
    econ_opp_clean,
    by = c("iso3c", "tech", "supply_chain")
  )

  component_names <- names(partnership_strength_development_default_weights())

  dev_potential_index <- base_tbl %>%
    dplyr::left_join(oecd_supplychain, by = c("iso3c", "supply_chain")) %>%
    dplyr::left_join(development_indicators, by = "iso3c") %>%
    dplyr::filter(iso3c %in% eme_iso$iso3c)
  require_columns(dev_potential_index, component_names, label = "development components")

  # Weighted mean over the components the partner has (missing ones dropped, not scored 0),
  # then S-curved across partners within (tech, stage).
  component_w <- vapply(component_names, function(nm) as.numeric(component_weights[[nm]]), numeric(1))
  dev_potential_index$dev_potential_raw <- partnership_strength_row_weighted_mean(
    as.list(dev_potential_index[component_names]),
    component_w
  )

  # Share of the component weight resting on missing or imputed inputs (the PSI rankability
  # measure, partnership_strength_imputed_weight_share()): a missing component counts in full,
  # the partner's ES and EO components at their pillar cell's own imputed share. Every component
  # describes the partner, so all of them count.
  pillar_share <- list(
    energy_security = partnership_strength_pillar_imputed_share(energy_security_index, country_info, "share", techs = techs),
    economic_opportunity = partnership_strength_pillar_imputed_share(economic_opportunity_index, country_info, "share", techs = techs)
  )
  unobserved <- lapply(component_names, function(nm) {
    if (nm %in% names(pillar_share)) {
      keyed <- partnership_strength_join_pillar_share(
        dev_potential_index[c("iso3c", "tech", "supply_chain")], pillar_share[[nm]], "iso3c", "share"
      )
      partnership_strength_pillar_unobserved(dev_potential_index[[nm]], keyed$share)
    } else {
      as.numeric(is.na(dev_potential_index[[nm]]))
    }
  })
  dev_potential_index$imputed_weight_share <- partnership_strength_imputed_weight_share(unobserved, component_w)
  dev_potential_index <- dev_potential_index %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(dev_potential_index = partnership_strength_safe_scurve(dev_potential_raw)) %>%
    dplyr::ungroup()

  partnership_strength_assert_unique(
    dev_potential_index,
    c("iso3c", "tech", "supply_chain"),
    label = "development potential index"
  )
}

# Development tables at the partner level. `variable` has one row per (partner, tech, stage,
# variable): the Development Potential Index and its components. `contributions` carries each
# component's weighted share of the pre-ranking composite (weights renormalized over the
# components present, so they sum to it; the index is its S-curve), as for Friendshore and
# Opportunity. `top5` is the partner's mean of its five highest (tech, stage) values per variable.
stronger_development <- function(country_info,
                                 gdp_data,
                                 economic_opportunity_index,
                                 energy_security_index,
                                 wb_wdi,
                                 wb_doingbusiness,
                                 oecd_api_raw,
                                 component_weights = NULL,
                                 year = 2024L,
                                 gdp_year = 2023,
                                 techs = NULL) {
  component_weights <- partnership_strength_resolve_weights(
    component_weights,
    partnership_strength_development_default_weights(),
    "Development component"
  )
  component_names <- names(component_weights)

  dev_potential_index <- partnership_strength_build_development_index(
    energy_security_index = energy_security_index,
    economic_opportunity_index = economic_opportunity_index,
    oecd_api_raw = oecd_api_raw,
    wb_wdi = wb_wdi,
    wb_doingbusiness = wb_doingbusiness,
    country_info = country_info,
    gdp_data = gdp_data,
    gdp_year = gdp_year,
    component_weights = component_weights,
    techs = techs
  )

  weights_tbl <- tibble::tibble(
    component_key = component_names,
    component_weight = vapply(component_names, function(nm) as.numeric(component_weights[[nm]]), numeric(1))
  )

  dev_long <- dev_potential_index %>%
    dplyr::mutate(
      partner_iso = iso3c,
      partner = partnership_strength_iso_to_country(iso3c, country_info)
    ) %>%
    dplyr::select(partner, partner_iso, tech, supply_chain, dplyr::all_of(component_names), dev_potential_index) %>%
    tidyr::pivot_longer(
      cols = c(dplyr::all_of(component_names), dev_potential_index),
      names_to = "component_key",
      values_to = "value"
    ) %>%
    dplyr::left_join(weights_tbl, by = "component_key") %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::mutate(
      weight_available = sum(component_weight[!is.na(value)], na.rm = TRUE),
      contribution = dplyr::if_else(
        !is.na(component_weight) & weight_available > 0,
        value * component_weight / weight_available,
        value
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Country = partner,
      category = "development",
      variable = dplyr::if_else(component_key == "dev_potential_index", "Development Potential Index", component_key),
      data_type = "index",
      Year = as.integer(year),
      source = "Author calculation",
      explanation = dplyr::if_else(
        component_key == "dev_potential_index",
        "Development potential of the partner: S-curve within (tech, stage) of its weighted component mean (non-high-income partners only; not reporter-specific).",
        "Component of the partner's development potential (non-high-income partners only)."
      )
    )

  variable_tbl <- dev_long %>%
    dplyr::select(
      Country,
      partner,
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

  partnership_strength_assert_unique(
    variable_tbl,
    c("partner_iso", "tech", "supply_chain", "variable"),
    label = "partner_development_tbl"
  )

  contributions_tbl <- dev_long %>%
    dplyr::filter(!is.na(contribution)) %>%
    dplyr::mutate(data_type = "contribution") %>%
    dplyr::select(
      Country,
      partner,
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

  # The partner's imputed weight share over the cells its top-5 Development Potential Index
  # averages; the PSI's rankability rule reads it off the top-5 table.
  partner_share <- partnership_strength_top_k_share(
    dplyr::rename(dev_potential_index, partner_iso = iso3c),
    "partner_iso",
    "dev_potential_index",
    k = 5L
  )

  top5_tbl <- partnership_strength_build_top_k(
    variable_tbl,
    c("partner", "partner_iso"),
    k = 5L,
    year = year
  ) %>%
    dplyr::mutate(
      Country = partner,
      explanation = "Mean of the partner's five highest (tech, stage) values."
    ) %>%
    partnership_strength_add_top5_share(partner_share, "partner_iso", "Development Potential Index") %>%
    dplyr::select(
      Country,
      partner,
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

  list(
    variable = variable_tbl,
    contributions = contributions_tbl,
    top5 = top5_tbl
  )
}
