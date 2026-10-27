# Export feasibility (trade) theme builder functions.

if (!exists("resolve_trade_comtrade_year")) {
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
}

# EO Trade: the capability components of "Overall Trade Index" - market share, Balassa
# RCA, export size, Atlas feasibility and world export concentration. The net-import term
# belongs to the ES exposure reading (trade_concentration()), so `gdp_data` is accepted
# for call compatibility but not used. `hhi_rank_techs`, `reexport_hubs` and `legacy_stages`
# (six-stage cells unless TRUE; no Deployment rows) as in trade_concentration().
export_feasibility <- function(aec_4_data,
                               aec_6_data,
                               subcat,
                               country_info,
                               gdp_data = NULL,
                               comtrade_trade,
                               comtrade_total_export,
                               year_4 = 2022,
                               year_6 = 2023,
                               year_comtrade = NULL,
                               include_sub_sector = FALSE,
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
    theme = "export_feasibility",
    hhi_rank_techs = hhi_rank_techs,
    reexport_hubs = reexport_hubs,
    legacy_stages = legacy_stages
  )
}
