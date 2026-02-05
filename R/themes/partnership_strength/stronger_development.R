# Stronger development theme (development potential index).

partnership_strength_clean_index_country <- function(index_tbl,
                                                     index_candidates,
                                                     value_name,
                                                     country_info,
                                                     invert = FALSE) {
  require_columns(index_tbl, c("Country", "tech", "supply_chain"), label = value_name)
  index_col <- partnership_strength_pick_column(index_tbl, index_candidates, value_name)

  index_tbl %>%
    dplyr::mutate(
      iso3c = partnership_strength_country_to_iso(Country, country_info),
      country = partnership_strength_standardize_countries(Country),
      value = .data[[index_col]]
    ) %>%
    dplyr::filter(!is.na(iso3c)) %>%
    dplyr::mutate(value = if (isTRUE(invert)) 1 - value else value) %>%
    dplyr::transmute(
      iso3c,
      country,
      tech,
      supply_chain,
      !!rlang::sym(value_name) := value
    )
}

partnership_strength_build_wdi_long <- function(wb_wdi) {
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

partnership_strength_build_doing_business <- function(wb_doingbusiness) {
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
                                                              wb_doingbusiness) {
  wb_wdi_long <- partnership_strength_build_wdi_long(wb_wdi)

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
    "DT.TDS.DECT.GN.ZS"
  )

  wb_wdi_long <- wb_wdi_long %>%
    dplyr::mutate(index = dplyr::if_else(Series.Code %in% inverted_codes, 1 - index, index))

  development_index <- wb_wdi_long %>%
    dplyr::group_by(Country.Name, Country.Code) %>%
    dplyr::summarize(development_index = mean(index, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(development_index = partnership_strength_safe_scurve(development_index)) %>%
    dplyr::rename(iso3c = Country.Code, country = Country.Name)

  doingbusiness_index <- partnership_strength_build_doing_business(wb_doingbusiness) %>%
    dplyr::transmute(
      iso3c = Country.Code,
      country = Country.Name,
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

partnership_strength_build_oecd_supplychain <- function(oecd_api_raw,
                                                        gdp_data,
                                                        gdp_year = 2023) {
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

  oecd_api %>%
    dplyr::filter(!Sector %in% c("All sectors", "Coal", "Oil and gas (Upstream)")) %>%
    dplyr::mutate(
      supply_chain = dplyr::case_when(
        Sector == "Energy" ~ "Downstream",
        Sector == "Industry" ~ "Midstream",
        Sector == "Mineral resources and mining" ~ "Upstream",
        TRUE ~ "Upstream"
      ),
      Value = as.numeric(Value)
    ) %>%
    dplyr::group_by(DonorISO, RecipientISO, Recipient, supply_chain) %>%
    dplyr::summarize(aid = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(gdp_tbl, by = c("RecipientISO" = "iso3c")) %>%
    dplyr::mutate(aid_gdp = aid / gdp) %>%
    dplyr::mutate(
      aid_index_supplychain = partnership_strength_safe_scurve(aid),
      aidgdp_index_supplychain = partnership_strength_safe_scurve(aid_gdp)
    ) %>%
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
                                                         trade_pairs,
                                                         gdp_year = 2023) {
  energy_security_clean <- partnership_strength_clean_index_country(
    energy_security_index,
    c("Energy_Security_Index", "energy_security_index", "value"),
    "energy_security",
    country_info,
    invert = TRUE
  )

  econ_opp_clean <- partnership_strength_clean_index_country(
    economic_opportunity_index,
    c("Economic_Opportunity_Index", "economic_opportunity_index", "value"),
    "economic_opportunity",
    country_info
  )

  us_opp_index <- econ_opp_clean %>%
    dplyr::filter(iso3c == "USA") %>%
    dplyr::transmute(
      tech,
      supply_chain,
      us_economic_opportunity = economic_opportunity
    )

  oecd_supplychain <- partnership_strength_build_oecd_supplychain(
    oecd_api_raw,
    gdp_data,
    gdp_year = gdp_year
  )

  development_indicators <- partnership_strength_build_development_indicators(
    wb_wdi,
    wb_doingbusiness
  )

  eme_iso <- country_info %>%
    dplyr::filter(income != "High income") %>%
    dplyr::distinct(iso3c)

  base_tbl <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = c("iso3c", "tech", "supply_chain")),
    list(energy_security_clean, econ_opp_clean)
  )

  dev_potential_index <- base_tbl %>%
    dplyr::left_join(us_opp_index, by = c("tech", "supply_chain")) %>%
    dplyr::left_join(oecd_supplychain, by = c("iso3c", "supply_chain")) %>%
    dplyr::left_join(development_indicators, by = "iso3c") %>%
    dplyr::filter(iso3c %in% eme_iso$iso3c) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dev_potential_index = partnership_strength_weighted_mean(
        c(
          development_index,
          doingbusiness_index,
          investment_enviro_index,
          energy_security,
          economic_opportunity,
          us_economic_opportunity,
          aid_index_supplychain,
          aidgdp_index_supplychain,
          energy_use_index
        ),
        c(1, 1, 1, 1, 1.5, 1.5, 1.5, 2, 1)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::semi_join(trade_pairs, by = c("tech", "supply_chain"))

  dev_potential_index
}

stronger_development <- function(comtrade_dyads,
                                 subcat,
                                 country_info,
                                 gdp_data,
                                 economic_opportunity_index,
                                 energy_security_index,
                                 wb_wdi,
                                 wb_doingbusiness,
                                 oecd_api_raw,
                                 years = 2020:2024,
                                 gdp_year = 2023) {
  res_tech <- partnership_strength_clean_trade_data(comtrade_dyads, subcat, years = years)
  trade_pairs <- res_tech %>%
    dplyr::distinct(tech, supply_chain)

  dev_potential_index <- partnership_strength_build_development_index(
    energy_security_index = energy_security_index,
    economic_opportunity_index = economic_opportunity_index,
    oecd_api_raw = oecd_api_raw,
    wb_wdi = wb_wdi,
    wb_doingbusiness = wb_doingbusiness,
    country_info = country_info,
    gdp_data = gdp_data,
    trade_pairs = trade_pairs,
    gdp_year = gdp_year
  )

  development_dyads <- res_tech %>%
    dplyr::distinct(reporter_iso, partner_iso, tech, supply_chain) %>%
    dplyr::left_join(
      dev_potential_index,
      by = c("partner_iso" = "iso3c", "tech", "supply_chain")
    ) %>%
    dplyr::mutate(
      Reporter = partnership_strength_iso_to_country(reporter_iso, country_info),
      Partner = partnership_strength_iso_to_country(partner_iso, country_info),
      Country = Partner
    ) %>%
    dplyr::transmute(
      Country,
      Reporter,
      Partner,
      reporter_iso,
      partner_iso,
      tech,
      supply_chain,
      category = "development",
      variable = "Development Potential Index",
      data_type = "index",
      value = dev_potential_index,
      Year = as.integer(max(years)),
      source = "Author calculation",
      explanation = "Development potential per reporter-partner dyad."
    )

  development_country <- dev_potential_index %>%
    dplyr::mutate(Country = partnership_strength_iso_to_country(iso3c, country_info)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "development",
      variable = "Development Potential Index",
      data_type = "index",
      value = dev_potential_index,
      Year = as.integer(max(years)),
      source = "Author calculation",
      explanation = "Weighted composite of OECD aid, WDI development indicators, energy security, and economic opportunity."
    )

  standardized_country <- partnership_strength_standardize_bind_rows(development_country)
  partnership_strength_validate_schema(standardized_country, label = "stronger_development_country")

  standardized_dyads <- partnership_strength_standardize_bind_rows(development_dyads)
  partnership_strength_validate_schema(standardized_dyads, label = "stronger_development_dyads")

  list(
    dyads = standardized_dyads,
    country = standardized_country
  )
}
