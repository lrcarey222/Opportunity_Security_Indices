# The R files scripts/88_export_country_data.R and the tests source, relative to the repository root,
# in order (pure helpers of Country Insights; spec_country.md A7.2). The OSB decoder is the Strategic
# export's (R/dashboard/dash_osb.R), read only.
country_helper_files <- function() {
  c(
    "R/dashboard/dash_osb.R",
    "R/dashboard/country/country_format.R",
    "R/dashboard/country/country_util.R",
    "R/dashboard/country/country_cells.R",
    "R/dashboard/country/country_si.R",
    "R/dashboard/country/country_policy.R",
    "R/dashboard/country/country_drivers.R",
    "R/dashboard/country/country_trade.R",
    "R/dashboard/country/country_fdi.R",
    "R/dashboard/country/country_ties.R",
    "R/dashboard/country/country_partners.R",
    "R/dashboard/country/country_network.R",
    "R/dashboard/country/country_nipo.R",
    "R/dashboard/country/country_sheet.R",
    "R/dashboard/country/country_check.R",
    "R/dashboard/country/country_memo.R",
    "R/dashboard/country/country_memo_lint.R",
    "R/dashboard/country/country_synthetic.R"
  )
}
