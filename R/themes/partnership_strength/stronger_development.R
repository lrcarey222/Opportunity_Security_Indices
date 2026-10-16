# Stronger development theme (development potential index).
#
# Level: partner country x (tech, stage). The index describes partner j alone (its enabling
# environment from WDI and Doing Business, its energy-security need, its economic opportunity
# and the official development finance it receives for the technology and stage); no reporter
# characteristic enters. It is therefore built and written once per (partner_iso, tech,
# supply_chain). It used to be copied to every reporter with any Comtrade trade with j and fanned
# out once per OECD donor (up to 38 rows per dyad cell), which is what corrupted the
# allied-network node join.
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

# Settings of the aid components (config/partnership.yml development_aid; the code defaults are
# used where it is silent): the window of CRS years averaged, the CRS measures counted and the
# flow (gross disbursements or commitments). Returns list(years = integer vector, measures, flow).
partnership_strength_development_aid_settings <- function(cfg = NULL) {
  years <- if (is.null(cfg$years)) c(2020L, 2024L) else as.integer(unlist(cfg$years))
  measures <- if (is.null(cfg$measures)) c("ODA", "OOF", "PSI") else as.character(unlist(cfg$measures))
  flow <- if (is.null(cfg$flow)) "disbursement" else as.character(cfg$flow)[1]
  if (length(years) != 2 || anyNA(years) || years[1] > years[2]) {
    stop("development_aid years must be [first, last] with first <= last.")
  }
  bad <- setdiff(measures, c("ODA", "OOF", "PSI"))
  if (!length(measures) || length(bad)) {
    stop("development_aid measures must be among ODA, OOF, PSI; got ", paste(measures, collapse = ", "))
  }
  if (!flow %in% c("disbursement", "commitment")) {
    stop("development_aid flow must be disbursement or commitment; got ", flow)
  }
  list(years = seq.int(years[1], years[2]), measures = unique(measures), flow = flow)
}

# CRS amounts per recipient and purpose code -> (tech, stage), with the purpose crosswalk
# (data/reference/oecd_crs_purpose_crosswalk.csv: purpose_code, supply_chain, tech, method):
#   - direct codes name the technologies they fund; a code naming several splits equally;
#   - pooled (cross-cutting) codes, such as energy policy or renewables "multiple technologies",
#     are spread over the technologies they list pro rata to the recipient's direct aid to them,
#     else pro rata to every recipient's direct aid to them, else equally;
#   - a (tech, stage) with no direct code anywhere in the crosswalk (Batteries Downstream, Green
#     Hydrogen, Geothermal / Nuclear / Solar / Wind Upstream, ...) takes the recipient's stage
#     proxy: everything the stage allocates, less the codes that fund only coal, oil and gas.
#     aid_basis says which: "purpose" or "stage".
# `amounts`: recipient_iso3, purpose_code, amount (codes outside the crosswalk are ignored);
# `cells`: tibble(tech, supply_chain) to return; `recipients`: the recipients to return, every
# cell filled (0 where the recipient has no aid there). Returns recipient_iso3, tech,
# supply_chain, aid, aid_basis.
partnership_strength_allocate_crs_purposes <- function(amounts,
                                                       crosswalk,
                                                       cells,
                                                       recipients = unique(amounts$recipient_iso3),
                                                       fossil_techs = c("Coal", "Oil", "Gas")) {
  require_columns(amounts, c("recipient_iso3", "purpose_code", "amount"), label = "CRS amounts")
  # Evaluated before `amounts` is filtered below (a lazy default would see the filtered table).
  recipients <- unique(as.character(recipients))
  require_columns(crosswalk, c("purpose_code", "supply_chain", "tech", "method"), label = "CRS purpose crosswalk")
  require_columns(cells, c("tech", "supply_chain"), label = "development cells")

  cw <- tibble::as_tibble(crosswalk[c("purpose_code", "supply_chain", "tech", "method")])
  cw$purpose_code <- as.integer(cw$purpose_code)
  bad_method <- setdiff(unique(cw$method), c("direct", "pooled"))
  if (length(bad_method)) stop("CRS purpose crosswalk: unknown method(s) ", paste(bad_method, collapse = ", "))
  if (anyDuplicated(cw[c("purpose_code", "supply_chain", "tech")])) {
    stop("CRS purpose crosswalk: duplicate (purpose_code, supply_chain, tech) rows.")
  }
  mixed <- dplyr::distinct(cw, purpose_code, method)
  if (anyDuplicated(mixed$purpose_code)) {
    stop("CRS purpose crosswalk: purpose code(s) with both direct and pooled rows: ",
         paste(unique(mixed$purpose_code[duplicated(mixed$purpose_code)]), collapse = ", "))
  }

  amounts <- tibble::tibble(
    recipient_iso3 = as.character(amounts$recipient_iso3),
    purpose_code = as.integer(amounts$purpose_code),
    amount = as.numeric(amounts$amount)
  )
  amounts <- amounts[amounts$purpose_code %in% cw$purpose_code, , drop = FALSE]

  direct_cw <- cw %>%
    dplyr::filter(method == "direct") %>%
    dplyr::group_by(purpose_code) %>%
    dplyr::mutate(share = 1 / dplyr::n()) %>%
    dplyr::ungroup()
  direct <- amounts %>%
    dplyr::inner_join(direct_cw, by = "purpose_code", relationship = "many-to-many") %>%
    dplyr::transmute(recipient_iso3, purpose_code, supply_chain, tech, aid = amount * share)

  direct_tot <- direct %>%
    dplyr::group_by(recipient_iso3, supply_chain, tech) %>%
    dplyr::summarise(d = sum(aid), .groups = "drop")
  global_tot <- direct_tot %>%
    dplyr::group_by(supply_chain, tech) %>%
    dplyr::summarise(g = sum(d), .groups = "drop")

  pooled <- amounts %>%
    dplyr::inner_join(dplyr::filter(cw, method == "pooled"), by = "purpose_code", relationship = "many-to-many") %>%
    dplyr::left_join(direct_tot, by = c("recipient_iso3", "supply_chain", "tech")) %>%
    dplyr::left_join(global_tot, by = c("supply_chain", "tech")) %>%
    dplyr::mutate(d = dplyr::coalesce(pmax(d, 0), 0), g = dplyr::coalesce(pmax(g, 0), 0)) %>%
    dplyr::group_by(recipient_iso3, purpose_code) %>%
    dplyr::mutate(
      w = if (sum(d) > 0) d / sum(d) else if (sum(g) > 0) g / sum(g) else 1 / dplyr::n(),
      aid = amount * w
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(recipient_iso3, purpose_code, supply_chain, tech, aid)

  allocated <- dplyr::bind_rows(direct, pooled)
  fossil_only <- cw %>%
    dplyr::group_by(purpose_code) %>%
    dplyr::summarise(fossil = all(tech %in% fossil_techs), .groups = "drop") %>%
    dplyr::filter(fossil) %>%
    dplyr::pull(purpose_code)

  tech_aid <- allocated %>%
    dplyr::group_by(recipient_iso3, supply_chain, tech) %>%
    dplyr::summarise(aid = sum(aid), .groups = "drop")
  stage_proxy <- allocated %>%
    dplyr::filter(!purpose_code %in% fossil_only) %>%
    dplyr::group_by(recipient_iso3, supply_chain) %>%
    dplyr::summarise(proxy = sum(aid), .groups = "drop")
  coded <- dplyr::distinct(direct_cw, supply_chain, tech) %>% dplyr::mutate(coded = TRUE)

  tidyr::expand_grid(
    recipient_iso3 = recipients,
    dplyr::distinct(tibble::as_tibble(cells[c("tech", "supply_chain")]))
  ) %>%
    dplyr::left_join(coded, by = c("supply_chain", "tech")) %>%
    dplyr::left_join(tech_aid, by = c("recipient_iso3", "supply_chain", "tech")) %>%
    dplyr::left_join(stage_proxy, by = c("recipient_iso3", "supply_chain")) %>%
    dplyr::mutate(
      coded = dplyr::coalesce(coded, FALSE),
      aid_basis = dplyr::if_else(coded, "purpose", "stage"),
      aid = pmax(0, dplyr::if_else(coded, dplyr::coalesce(aid, 0), dplyr::coalesce(proxy, 0)))
    ) %>%
    dplyr::select(recipient_iso3, tech, supply_chain, aid, aid_basis)
}

# Official development finance received by partner j per (tech, stage), from the CRS extract
# data/raw/oecd_crs_purpose.csv (scripts/fetchers/oecd_crs.R: bilateral flows of country donors,
# by purpose code, USD millions at constant prices) and the purpose crosswalk
# (partnership_strength_allocate_crs_purposes()). The settings
# (partnership_strength_development_aid_settings()) pick the CRS measures, the flow and the window
# of years; aid is the annual mean over the window's years in the extract, summed over donors, and
# aid_gdp that mean / the recipient's mean GDP over the years of the window it has (USD m / USD).
# aid_index_supplychain and aidgdp_index_supplychain are their S-curves across recipients within
# each (tech, stage); the names predate the tech dimension and are kept for the weights, the index
# definition and the dashboard.
#
# Every recipient in the window gets every cell, 0 where it received nothing there: the CRS is
# the census of its reporting donors' activities, so no recorded aid is an observed zero. A
# partner that received none of the extract's official finance in the window is missing (NA).
#
# This replaces the 3-digit stage totals of partnership_strength_build_oecd_supplychain(), which
# gave all eleven technologies of a stage the same value (median rank correlation 0.99 of the
# Development Potential Index across technologies within a stage in the October 2026 build).
partnership_strength_build_oecd_tech <- function(oecd_crs,
                                                 crosswalk,
                                                 gdp_data,
                                                 cells,
                                                 settings = partnership_strength_development_aid_settings(),
                                                 valid_iso = NULL) {
  require_columns(
    oecd_crs,
    c("year", "recipient_iso3", "purpose_code", "measure", "usd_commitment", "usd_disbursement"),
    label = "OECD CRS extract"
  )
  amount_col <- if (identical(settings$flow, "commitment")) "usd_commitment" else "usd_disbursement"

  crs <- oecd_crs[oecd_crs$year %in% settings$years & oecd_crs$measure %in% settings$measures, , drop = FALSE]
  if (!is.null(valid_iso)) crs <- crs[crs$recipient_iso3 %in% valid_iso, , drop = FALSE]
  years_present <- sort(unique(as.integer(crs$year)))
  if (!length(years_present)) {
    stop("OECD CRS extract has no ", paste(settings$measures, collapse = "/"), " rows in ",
         min(settings$years), "-", max(settings$years), ".")
  }
  absent <- setdiff(settings$years, years_present)
  if (length(absent)) {
    warning("OECD CRS extract has no rows for ", paste(absent, collapse = ", "),
            "; aid is the annual mean over ", paste(range(years_present), collapse = "-"), ".", call. = FALSE)
  }

  amounts <- crs %>%
    dplyr::group_by(recipient_iso3, purpose_code) %>%
    dplyr::summarise(amount = pmax(0, sum(.data[[amount_col]], na.rm = TRUE)) / length(years_present), .groups = "drop")
  aid <- partnership_strength_allocate_crs_purposes(
    amounts,
    crosswalk,
    cells,
    recipients = unique(as.character(crs$recipient_iso3))
  )

  iso_col <- partnership_strength_pick_column(gdp_data, c("iso3c", "iso3", "Country.Code", "country_code"), "gdp iso")
  year_col <- partnership_strength_pick_column(gdp_data, c("year", "Year"), "gdp year")
  gdp_col <- partnership_strength_pick_column(gdp_data, c("NY.GDP.MKTP.CD", "gdp", "GDP"), "gdp value")
  gdp_tbl <- tibble::tibble(
    iso3c = as.character(gdp_data[[iso_col]]),
    year = suppressWarnings(as.integer(gdp_data[[year_col]])),
    gdp = suppressWarnings(as.numeric(gdp_data[[gdp_col]]))
  ) %>%
    dplyr::filter(year %in% settings$years, !is.na(gdp), gdp > 0) %>%
    dplyr::distinct(iso3c, year, .keep_all = TRUE) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarise(gdp = mean(gdp), .groups = "drop")

  aid %>%
    dplyr::left_join(gdp_tbl, by = c("recipient_iso3" = "iso3c")) %>%
    dplyr::mutate(aid_gdp = dplyr::if_else(!is.na(gdp), aid / gdp, NA_real_)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      aid_index_supplychain = partnership_strength_safe_scurve(aid),
      aidgdp_index_supplychain = partnership_strength_safe_scurve(aid_gdp)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      iso3c = recipient_iso3,
      tech,
      supply_chain,
      aid,
      aid_gdp,
      aid_basis,
      aid_index_supplychain,
      aidgdp_index_supplychain
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
#
# Since October 2026 the index reads partnership_strength_build_oecd_tech() instead. This builder
# stays for the Development & finance chunk of the dashboard export (scripts/87,
# R/dashboard/sector/sector_dev.R), which ships the 3-digit CRS flows of oecd_crs_api.csv and
# checks its stage totals against it.
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
                                                         oecd_crs,
                                                         oecd_crosswalk,
                                                         wb_wdi,
                                                         wb_doingbusiness,
                                                         country_info,
                                                         gdp_data,
                                                         aid_settings = NULL,
                                                         component_weights = NULL,
                                                         techs = NULL) {
  if (is.null(aid_settings)) aid_settings <- partnership_strength_development_aid_settings()
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

  oecd_tech <- partnership_strength_build_oecd_tech(
    oecd_crs,
    oecd_crosswalk,
    gdp_data,
    cells = dplyr::distinct(base_tbl, tech, supply_chain),
    settings = aid_settings,
    valid_iso = valid_iso
  ) %>%
    dplyr::select(iso3c, tech, supply_chain, aid_basis, aid_index_supplychain, aidgdp_index_supplychain)

  component_names <- names(partnership_strength_development_default_weights())

  dev_potential_index <- base_tbl %>%
    dplyr::left_join(oecd_tech, by = c("iso3c", "tech", "supply_chain")) %>%
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
                                 oecd_crs,
                                 oecd_crosswalk,
                                 component_weights = NULL,
                                 aid_settings = NULL,
                                 year = 2024L,
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
    oecd_crs = oecd_crs,
    oecd_crosswalk = oecd_crosswalk,
    wb_wdi = wb_wdi,
    wb_doingbusiness = wb_doingbusiness,
    country_info = country_info,
    gdp_data = gdp_data,
    aid_settings = aid_settings,
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
