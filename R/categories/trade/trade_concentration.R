# Trade concentration theme builder functions.

trade_concentration <- function(subcat,
                                aec_4_data,
                                aec_6_data,
                                comtrade_trade,
                                comtrade_total_export,
                                country_info,
                                gdp_data,
                                year_4 = 2022,
                                year_6 = 2023,
                                year_comtrade = 2024,
                                include_sub_sector = FALSE) {
  trade_category_build_trade(
    subcat = subcat,
    aec_4_data = aec_4_data,
    aec_6_data = aec_6_data,
    comtrade_trade = comtrade_trade,
    comtrade_total_export = comtrade_total_export,
    country_info = country_info,
    gdp_data = gdp_data,
    year_4 = year_4,
    year_6 = year_6,
    year_comtrade = year_comtrade,
    include_sub_sector = include_sub_sector
  )
}
