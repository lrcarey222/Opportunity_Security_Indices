# The R files scripts/87_export_sector_data.R and the tests source, relative to the repository root,
# in order (pure). The OSB codec is the Strategic export's, unchanged (no new node types). The
# six supply-chain stages (SUPPLY_CHAIN_STAGES) come first: every sector constant is built on them.
sector_files <- function() {
  c(
    "R/utils/supply_chain_stages.R",
    "R/dashboard/dash_osb.R",
    "R/dashboard/sector/sector_dictionary.R",
    "R/dashboard/sector/sector_summary.R",
    "R/dashboard/sector/sector_trade.R",
    "R/dashboard/sector/sector_policy.R",
    "R/dashboard/sector/sector_chunks.R",
    "R/dashboard/sector/sector_check.R",
    "R/dashboard/sector/sector_bundle.R",
    "R/dashboard/sector/sector_dims.R",
    "R/dashboard/sector/sector_facts.R",
    "R/dashboard/sector/sector_raw.R",
    "R/dashboard/sector/sector_series.R",
    "R/dashboard/sector/sector_fdi.R",
    "R/dashboard/sector/sector_dev.R",
    "R/dashboard/sector/sector_climate.R",
    "R/dashboard/sector/sector_synthetic.R"
  )
}

# Pipeline files the real export reuses (read only): the trade matrix, the PAMS cleaners, the IEA
# critical-minerals reader, the USGS reserves, the coalition mineral links, the IMF units and the
# EIA International Energy Statistics lookup (the EIA years and comparability of filled production),
# and the Development Potential and Friendshore outbound builders (the WDI, Doing Business, CRS and
# IMF DIP parsers behind Development & finance).
sector_pipeline_files <- function() {
  c(
    "R/utils/schema.R",
    "R/utils/country.R",
    "R/utils/iea_critical_minerals.R",
    "R/utils/eia_intl.R",
    "R/themes/partnership_strength/partnership_strength_helpers.R",
    "R/themes/partnership_strength/stronger_development.R",
    "R/themes/partnership_strength/safer_friendshore.R",
    "R/categories/policy/iea_policy_index.R",
    "R/categories/reserves/usgs_mcs.R",
    "R/categories/reserves/reserves.R",
    "R/categories/energy_prices/energy_prices.R",
    "R/categories/investment/investment_momentum.R",
    "R/indices/coalition.R"
  )
}
