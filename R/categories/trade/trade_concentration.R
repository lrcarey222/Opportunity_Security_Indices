# Trade concentration theme builder functions.

resolve_trade_comtrade_year <- function(year_comtrade, comtrade_trade) {
  if (!is.null(year_comtrade) && !is.na(year_comtrade)) {
    return(as.integer(year_comtrade))
  }

  option_year <- getOption("opportunity_security.comtrade_year")
  if (!is.null(option_year) && !is.na(option_year)) {
    return(as.integer(option_year))
  }

  year_col <- intersect(c("ref_year", "period", "year", "Year"), names(comtrade_trade))
  if (length(year_col) > 0 && nrow(comtrade_trade) > 0) {
    vals <- suppressWarnings(as.integer(stringr::str_extract(as.character(comtrade_trade[[year_col[[1]]]]), "\\d{4}")))
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      return(max(vals))
    }
  }

  NA_integer_
}

# ES Trade: the exposure components of "Overall Trade Risk Index" - world export
# concentration (a cell-level constant), own export market share, and net imports of the
# basket relative to GDP. `gdp_data` is the WDI GDP table (iso3c, year, NY.GDP.MKTP.CD);
# without it the deficit term is absent and the category rests on the other two.
# `hhi_rank_techs` (the headline technologies) is the ranking universe of the HHI S-curve and
# `reexport_hubs` (ISO3) the reporters left out of the Comtrade measures; see
# trade_core_build_comtrade_hhi() and trade_core_build_comtrade_trade(). NULL keeps every cell
# in the ranking and every reporter.
# The cells are tech x six-stage supply chain, from the crosswalk's Stage column; Deployment has
# no HS6 basket and so no rows. `legacy_stages = TRUE` builds the old Upstream / Midstream
# baskets instead, for readers of this table not yet moved to six stages
# (trade_core_build_energy_codes()).
trade_concentration <- function(subcat,
                                aec_4_data,
                                aec_6_data,
                                comtrade_trade,
                                comtrade_total_export,
                                country_info,
                                year_4 = 2022,
                                year_6 = 2023,
                                year_comtrade = NULL,
                                include_sub_sector = FALSE,
                                gdp_data = NULL,
                                hhi_rank_techs = NULL,
                                reexport_hubs = NULL,
                                legacy_stages = FALSE) {
  year_comtrade <- resolve_trade_comtrade_year(year_comtrade, comtrade_trade)

  trade_category_build_trade(
    subcat = subcat,
    aec_4_data = aec_4_data,
    aec_6_data = aec_6_data,
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    country_info = country_info,
    year_4 = year_4,
    year_6 = year_6,
    year_comtrade = year_comtrade,
    include_sub_sector = include_sub_sector,
    gdp_data = gdp_data,
    theme = "trade_concentration",
    hhi_rank_techs = hhi_rank_techs,
    reexport_hubs = reexport_hubs,
    legacy_stages = legacy_stages
  )
}
