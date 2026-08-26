# Build the Economic Opportunity and Energy Security indices for a chosen set of years.
#
# The annual pipeline (00 -> 05 -> 07 -> 10 -> 20) produces one snapshot: each theme builder
# reaches for the newest observation its source carries, so data/processed holds a single
# vintage and there is no way to ask it what 2020 looked like. This script rebuilds both
# pillars at each requested year by slicing the *raw inputs* to that year and then running
# the same theme builders and the same v2 index builders the annual pipeline uses.
#
# Usage (Rscript, from anywhere):
#
#   Rscript scripts/40_build_index_vintages.R --years=2020,2025
#   OPSI_INDEX_YEARS=2015,2020,2025 Rscript scripts/40_build_index_vintages.R
#
# Defaults to 2020,2025. Any number of years can be requested; the comparison table is
# built between the earliest and latest of them.
#
# Prerequisite: scripts/10_build_themes.R must have been run at least once, because the
# themes with no time dimension (resource potential, reserves, IEA critical minerals, BNEF
# demand and LCOE, overcapacity, TRL) are read from data/processed and reused unchanged
# across every vintage. Reusing rather than rebuilding them is deliberate: it guarantees the
# fixed components are bit-identical between years, so the whole of the year-on-year delta
# comes from the components that genuinely moved.
#
# How to read the output
# ----------------------
# Every component is a cross-sectional percent rank (median_scurve), so an index value is a
# country's standing against its peers in that year, not an absolute level. A rise from 2020
# to 2025 means the country gained ground on the field. Both the level change and the rank
# change are written out for that reason.
#
# Half the Energy Security weight and about three quarters of the Economic Opportunity
# weight re-derives per year; the rest is held fixed and contributes zero to the delta. The
# exact split for the run is written to index_vintage_weight_coverage.csv, and the per-theme
# reasoning to index_vintage_theme_provenance.csv.
#
# Note on sign: `Energy_Security_Index` is the builder's own orientation, where HIGH means
# better placed (more reserves, a stronger import balance, less concentrated trade) - the
# same orientation as `energy_security_index` in scripts/20_build_indices.R before the
# 1 - x flip that script applies. `Energy_Security_Risk` is published alongside it as that
# complement, for reading the pillar as exposure rather than capability.

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "theme_standardize.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_core.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "categories", "consumption", "energy_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_imports", "import_dependence.R"))
source(file.path(repo_root, "R", "categories", "energy_prices", "energy_prices.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "production", "production_depth_momentum.R"))
source(file.path(repo_root, "R", "categories", "investment", "investment_momentum.R"))
source(file.path(repo_root, "R", "categories", "economic opportunity", "cost_competitiveness.R"))
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index_v2.R"))
source(file.path(repo_root, "R", "indices", "index_vintages.R"))

# Reload runtime configs from repository YAML so an interactive session's option overrides
# cannot leak into a vintage run (mirrors scripts/20_build_indices.R).
options(
  opportunity_security.config = yaml::read_yaml(
    Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
  ),
  opportunity_security.weights = yaml::read_yaml(
    Sys.getenv("OPSI_WEIGHTS", file.path(repo_root, "config", "weights.yml"))
  ),
  opportunity_security.missing_data = yaml::read_yaml(
    Sys.getenv("OPSI_MISSING_DATA", file.path(repo_root, "config", "missing_data.yml"))
  ),
  opportunity_security.index_definition = yaml::read_yaml(
    Sys.getenv("OPSI_INDEX_DEFINITION", file.path(repo_root, "config", "index_definition.yml"))
  )
)

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
missing_data <- getOption("opportunity_security.missing_data")

allow_partial_categories <- isTRUE(config$allow_partial_categories)
include_sub_sector <- isTRUE(if (!is.null(config$include_sub_sector)) {
  config$include_sub_sector
} else {
  config$energy_security_include_sub_sector
})

techs <- c(
  "Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind",
  "Oil", "Solar", "Gas", "Geothermal", "Electric Grid"
)

# ---------------------------------------------------------------------------
# Requested years
# ---------------------------------------------------------------------------

cli_args <- commandArgs(trailingOnly = TRUE)
years_arg <- sub("^--years=", "", grep("^--years=", cli_args, value = TRUE))
if (length(years_arg) == 0) {
  years_arg <- Sys.getenv("OPSI_INDEX_YEARS", "")
}

index_years <- parse_index_years(years_arg, default = c(2020L, 2025L))
message("Building index vintages for: ", paste(index_years, collapse = ", "))

# ---------------------------------------------------------------------------
# Paths and static (non-time-varying) theme tables
# ---------------------------------------------------------------------------

raw_data_path <- file.path(repo_root, config$raw_data_dir)
if (!dir.exists(raw_data_path)) {
  stop("Raw data directory not found: ", raw_data_path)
}

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found: ", processed_dir, ". Run scripts/10_build_themes.R first.")
}

vintage_dir <- file.path(processed_dir, "vintages")
if (!dir.exists(vintage_dir)) {
  dir.create(vintage_dir, recursive = TRUE)
}

read_processed_theme <- function(name) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop(
      "Processed theme table not found: ", path, "\n",
      "The vintage builder reuses the non-time-varying themes from the annual pipeline; ",
      "run scripts/10_build_themes.R first."
    )
  }
  readRDS(path)
}

static_themes <- list(
  solar_pv_potential = read_processed_theme("solar_pv_potential_tbl"),
  wind_potential = read_processed_theme("wind_potential_tbl"),
  geothermal_potential = read_processed_theme("geothermal_potential_tbl"),
  reserves = read_processed_theme("reserves_tbl"),
  foreign_dependency = read_processed_theme("foreign_dependency_tbl"),
  critical_minerals_processing = read_processed_theme("critical_minerals_processing_tbl"),
  critical_minerals_production = read_processed_theme("critical_minerals_production_tbl"),
  critical_minerals_trade = read_processed_theme("critical_minerals_trade_tbl"),
  future_demand = read_processed_theme("future_demand_tbl"),
  lcoe_competitiveness = read_processed_theme("lcoe_competitiveness_tbl"),
  market_share_manufacturing = read_processed_theme("market_share_manufacturing_tbl"),
  overcapacity_premium = read_processed_theme("overcapacity_premium_tbl"),
  technological_readiness = read_processed_theme("technological_readiness_tbl")
)

# ---------------------------------------------------------------------------
# Raw inputs for the time-varying themes (read once, sliced per vintage)
# ---------------------------------------------------------------------------

message("Reading raw inputs for time-varying themes ...")

country_info <- standardize_country_info(read.csv(file.path(raw_data_path, "wdi_country_info.csv")))

ei <- read.csv(file.path(raw_data_path, "ei_stat_review_world_energy.csv"))
ei_years <- sort(unique(suppressWarnings(as.integer(ei$Year))))
ei_years <- ei_years[!is.na(ei_years)]

bnef_neo_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^\\d{4}-\\d{2}-\\d{2} - New Energy Outlook \\d{4}\\.csv$",
  fallback = "2024-10-29 - New Energy Outlook 2024.csv",
  label = "BNEF New Energy Outlook"
)
bnef_neo <- read.csv(bnef_neo_path, skip = 2)

critical_minerals_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
  fallback = "IEA Critical Minerals Dataset 2026.xlsx",
  label = "IEA Critical Minerals Dataset"
)
critical <- read_iea_critical_minerals(critical_minerals_path)
mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)

# Atlas of Economic Complexity: the multi-year trade source. Both files are tens of
# millions of rows, so only the needed columns are read and only the years this run needs
# are kept.
read_atlas <- function(path, columns) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(tibble::as_tibble(data.table::fread(path, select = columns)))
  }
  readr::read_csv(path, col_select = dplyr::all_of(columns), show_col_types = FALSE)
}

message("Reading Atlas trade data (large files) ...")
atlas_hs6 <- read_atlas(
  file.path(raw_data_path, "hs92_country_product_year_6.csv"),
  c("country_iso3_code", "product_hs92_code", "year", "export_value", "import_value", "global_market_share")
)
atlas_hs4 <- read_atlas(
  file.path(raw_data_path, "hs92_country_product_year_4.csv"),
  c("country_iso3_code", "product_hs92_code", "year", "export_value", "distance")
)

atlas_years <- sort(unique(as.integer(atlas_hs6$year)))
atlas_target_years <- unique(vapply(
  index_years,
  function(y) resolve_available_year(y, atlas_years),
  integer(1)
))

# Atlas stores HS codes as integers; pad them back to the zero-filled HS6/HS4 strings the
# crosswalk uses before any join happens.
atlas_hs6 <- atlas_pad_hs_codes(
  dplyr::filter(atlas_hs6, .data$year %in% atlas_target_years),
  width = 6
)
atlas_hs4 <- atlas_pad_hs_codes(
  dplyr::filter(atlas_hs4, .data$year %in% atlas_target_years),
  width = 4
)

subcat <- readr::read_csv(
  file.path(raw_data_path, "hts_codes_categories_bolstered_final.csv"),
  show_col_types = FALSE
) %>%
  dplyr::rename("tech" = "Technology", "supply_chain" = "Value Chain")

imf_commodity_prices <- read.csv(file.path(raw_data_path, "imf_commodity_prices.csv"))
imf_lending_rates <- read.csv(file.path(raw_data_path, "imf_lending_rates.csv"))
imf_ppi <- read.csv(file.path(raw_data_path, "imf_ppi.csv"))
iea_relative_costs <- read.csv(file.path(raw_data_path, "Relative_Costs_IEA.csv"))

# ILO average monthly earnings is fetched live by the annual pipeline. Cache it under the
# vintage directory so repeated vintage runs (and offline runs) do not depend on the API.
ilo_cache_path <- file.path(vintage_dir, "_cache_ilo_earnings.csv")
if (file.exists(ilo_cache_path)) {
  ilo_raw <- read.csv(ilo_cache_path)
} else {
  ilo_url <- paste0(
    "https://rplumber.ilo.org/data/indicator/?id=EAR_EMTA_SEX_ECO_CUR_NB_A&lang=en",
    "&type=label&format=.csv&channel=ilostat",
    "&title=average-monthly-earnings-of-employees-by-sex-economic-activity-and-currency-annual"
  )
  message("Downloading ILO earnings (cached to ", ilo_cache_path, ") ...")
  ilo_raw <- read.csv(ilo_url)
  utils::write.csv(ilo_raw, ilo_cache_path, row.names = FALSE)
}

gcim_path <- file.path(raw_data_path, "GCIM_Investment_Capacity_aggregated.xlsx")
gcim_sheets <- readxl::excel_sheets(gcim_path)
gcim_annual <- investment_read_gcim_sheet(
  gcim_path,
  investment_resolve_gcim_sheet(gcim_sheets, investment_gcim_sheet_patterns$annual, "annual investment"),
  c("Country", "Segment", "Technology", "Year", "Investment"),
  label = "annual investment"
)
gcim_capacity <- investment_read_gcim_sheet(
  gcim_path,
  investment_resolve_gcim_sheet(gcim_sheets, investment_gcim_sheet_patterns$capacity, "manufacturing/industry capacity"),
  c("Country", "Segment", "Technology", "Product", "Category", "Value"),
  label = "manufacturing/industry capacity"
)
gcim_years <- sort(unique(suppressWarnings(as.integer(gcim_annual$Year))))
gcim_years <- gcim_years[!is.na(gcim_years)]

# Country reference for the minerals half of production_depth_momentum. Pinned to the
# latest requested year so it is identical across vintages and cannot inject spurious
# differences through a changing country list.
country_reference <- reserves_build_country_reference(ei, year = max(index_years))

# ---------------------------------------------------------------------------
# Build the time-varying themes for one vintage year
# ---------------------------------------------------------------------------

build_varying_themes <- function(year) {
  ei_year <- resolve_available_year(year, ei_years)
  atlas_year <- resolve_available_year(year, atlas_years)
  gcim_year <- resolve_available_year(year, gcim_years)

  # Each slice reports the year it could actually reach; IMF PPI in particular only starts
  # in 2021, so an early vintage gets the first panel the source publishes.
  imf_prices_slice <- vintage_slice_imf_wide(imf_commodity_prices, year)
  imf_rates_slice <- vintage_slice_imf_wide(imf_lending_rates, year)
  imf_ppi_slice <- vintage_slice_imf_wide(imf_ppi, year)
  ilo_slice <- vintage_slice_ilo(ilo_raw, year)
  gcim_slice <- vintage_slice_annual(gcim_annual, gcim_year, year_col = "Year")

  message(sprintf(
    "  vintage %d source years: EI=%d Atlas=%d GCIM=%d IMFprices=%d IMFrates=%d IMFppi=%d ILO=%d",
    year, ei_year, atlas_year,
    vintage_year_of(gcim_slice), vintage_year_of(imf_prices_slice),
    vintage_year_of(imf_rates_slice), vintage_year_of(imf_ppi_slice),
    vintage_year_of(ilo_slice)
  ))

  energy_access_tbl <- energy_access_consumption(
    ei,
    country_info = country_info,
    base_year = ei_year - 5L,
    target_year = ei_year
  ) %>%
    standardize_theme_types(country_info = country_info)

  energy_consumption_tbl <- energy_consumption(
    ei = ei,
    bnef_neo = bnef_neo,
    country_info = country_info,
    base_year = ei_year - 5L,
    target_year = ei_year
  ) %>%
    standardize_theme_types(country_info = country_info)

  import_dependence_tbl <- import_dependence(ei, year = ei_year) %>%
    standardize_theme_types(country_info = country_info)

  production_depth_momentum_tbl <- production_depth_momentum(
    ei = ei,
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    year = ei_year
  ) %>%
    standardize_theme_types(country_info = country_info)

  trade_tbl <- atlas_trade_theme(
    aec_4_data = atlas_hs4,
    aec_6_data = atlas_hs6,
    subcat = subcat,
    country_info = country_info,
    year = atlas_year,
    include_sub_sector = include_sub_sector
  ) %>%
    standardize_theme_types(country_info = country_info)

  energy_prices_tbl <- energy_prices(
    imf_price = imf_prices_slice,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info
  ) %>%
    standardize_theme_types(country_info = country_info)

  cost_competitiveness_tbl <- cost_competitiveness(
    iea_cost_raw = iea_relative_costs,
    ei = ei,
    country_info = country_info,
    ilo_raw = ilo_slice,
    imf_lending_rates = imf_rates_slice,
    imf_ppi = imf_ppi_slice,
    year = year
  ) %>%
    standardize_theme_types(country_info = country_info)

  investment_momentum_tbl <- investment_momentum(
    annual_tbl = gcim_slice,
    capacity_tbl = gcim_capacity,
    country_reference = country_reference$Country
  ) %>%
    standardize_theme_types(country_info = country_info)

  list(
    themes = list(
      energy_access_consumption = energy_access_tbl,
      energy_consumption = energy_consumption_tbl,
      import_dependence = import_dependence_tbl,
      production_depth_momentum = production_depth_momentum_tbl,
      trade_concentration = trade_tbl,
      export_feasibility = trade_tbl,
      energy_prices = energy_prices_tbl,
      cost_competitiveness = cost_competitiveness_tbl,
      investment_momentum = investment_momentum_tbl
    ),
    source_years = tibble::tibble(
      index_year = as.integer(year),
      source = c(
        "EI Statistical Review",
        "Atlas of Economic Complexity",
        "GCIM annual investment",
        "IMF commodity prices",
        "IMF lending rates",
        "IMF producer prices",
        "ILO average earnings"
      ),
      year_used = c(
        ei_year,
        atlas_year,
        vintage_year_of(gcim_slice),
        vintage_year_of(imf_prices_slice),
        vintage_year_of(imf_rates_slice),
        vintage_year_of(imf_ppi_slice),
        vintage_year_of(ilo_slice)
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Run every vintage
# ---------------------------------------------------------------------------

energy_security_theme_names <- c(
  "energy_access_consumption", "solar_pv_potential", "wind_potential", "geothermal_potential",
  "import_dependence", "reserves", "foreign_dependency", "critical_minerals_processing",
  "critical_minerals_production", "critical_minerals_trade", "energy_consumption",
  "trade_concentration", "energy_prices", "investment_momentum"
)

economic_opportunity_theme_names <- c(
  "energy_access_consumption", "solar_pv_potential", "wind_potential", "geothermal_potential",
  "energy_consumption", "energy_prices", "export_feasibility", "future_demand",
  "lcoe_competitiveness", "market_share_manufacturing", "cost_competitiveness",
  "production_depth_momentum", "overcapacity_premium", "technological_readiness",
  "investment_momentum"
)

vintages <- list()
source_year_log <- list()

for (year in index_years) {
  message("Building vintage ", year, " ...")
  built <- build_varying_themes(year)
  all_themes <- c(built$themes, static_themes)
  source_year_log[[as.character(year)]] <- built$source_years

  energy_security_outputs <- build_energy_security_index_v2(
    theme_tables = all_themes[energy_security_theme_names],
    weights = weights$energy_security,
    missing_data = missing_data$energy_security,
    allow_partial_categories = allow_partial_categories,
    include_sub_sector = include_sub_sector
  )

  economic_opportunity_outputs <- build_economic_opportunity_index_v2(
    theme_tables = all_themes[economic_opportunity_theme_names],
    weights = weights$economic_opportunity,
    missing_data = missing_data$economic_opportunity,
    allow_partial_categories = allow_partial_categories,
    include_sub_sector = include_sub_sector
  )

  vintages[[as.character(year)]] <- list(
    index_year = as.integer(year),
    energy_security = energy_security_outputs,
    economic_opportunity = economic_opportunity_outputs
  )
}

# ---------------------------------------------------------------------------
# Stack, compare, write
# ---------------------------------------------------------------------------

stack_by_year <- function(extract_fn) {
  dplyr::bind_rows(lapply(vintages, function(v) {
    tbl <- extract_fn(v)
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(NULL)
    }
    dplyr::mutate(tibble::as_tibble(tbl), index_year = v$index_year, .before = 1)
  }))
}

energy_security_by_year <- stack_by_year(function(v) v$energy_security$index) %>%
  dplyr::filter(.data$tech %in% techs) %>%
  # The builder's Energy_Security_Index scores *security*: its components (reserves, import
  # balance, unconcentrated trade) all rank higher for a better-placed country. The risk
  # reading is its complement, which is the flip scripts/20_build_indices.R applies before
  # folding the pillar into the strategic index. Both are published so neither orientation
  # has to be inferred. Risk change is exactly the negative of index change, so the
  # comparison table below is built on the security orientation only.
  dplyr::mutate(Energy_Security_Risk = 1 - .data$Energy_Security_Index)

economic_opportunity_by_year <- stack_by_year(function(v) v$economic_opportunity$index) %>%
  dplyr::filter(.data$tech %in% techs)

energy_security_categories_by_year <- stack_by_year(function(v) v$energy_security$category_scores) %>%
  dplyr::filter(.data$tech %in% techs) %>%
  dplyr::mutate(pillar = "energy_security", .after = "index_year")

economic_opportunity_categories_by_year <- stack_by_year(function(v) v$economic_opportunity$category_scores) %>%
  dplyr::filter(.data$tech %in% techs) %>%
  dplyr::mutate(pillar = "economic_opportunity", .after = "index_year")

category_scores_by_year <- dplyr::bind_rows(
  energy_security_categories_by_year,
  economic_opportunity_categories_by_year
)

# Comparison tables are only meaningful with two or more vintages.
comparisons <- list()
if (length(index_years) >= 2) {
  comparisons$energy_security <- index_vintage_comparison(
    energy_security_by_year,
    index_col = "Energy_Security_Index"
  )
  comparisons$economic_opportunity <- index_vintage_comparison(
    economic_opportunity_by_year,
    index_col = "Economic_Opportunity_Index"
  )
}

# Provenance: which themes moved with the year, and how much pillar weight that covers.
theme_provenance <- dplyr::bind_rows(
  index_vintage_theme_spec() %>%
    dplyr::filter(.data$theme %in% energy_security_theme_names) %>%
    dplyr::mutate(pillar = "energy_security", .before = 1),
  index_vintage_theme_spec() %>%
    dplyr::filter(.data$theme %in% economic_opportunity_theme_names) %>%
    dplyr::mutate(pillar = "economic_opportunity", .before = 1)
)

# Anchored to the categories that actually scored, not to the configured weight list: the
# builders drop any category whose score variable they cannot find and renormalize over the
# rest, and that renormalization is what the index is really made of.
energy_security_categories_scored <- sort(unique(energy_security_categories_by_year$category))
economic_opportunity_categories_scored <- sort(unique(economic_opportunity_categories_by_year$category))

weight_coverage <- dplyr::bind_rows(
  index_vintage_weight_coverage(
    weights$energy_security,
    energy_security_theme_names,
    categories_present = energy_security_categories_scored
  ) %>%
    dplyr::mutate(pillar = "energy_security", .before = 1),
  index_vintage_weight_coverage(
    weights$economic_opportunity,
    economic_opportunity_theme_names,
    categories_present = economic_opportunity_categories_scored
  ) %>%
    dplyr::mutate(pillar = "economic_opportunity", .before = 1)
)

# Configured-but-unscored categories are a live defect in the shared config, not a vintage
# artefact: they behave the same way in scripts/20_build_indices.R. Surface them here so a
# vintage run does not quietly present a weight scheme it did not apply.
describe_dropped <- function(pillar, configured, scored) {
  dropped <- setdiff(configured, scored)
  if (length(dropped) == 0) {
    return(character(0))
  }
  paste0(pillar, ": ", paste(dropped, collapse = ", "))
}

dropped_categories <- c(
  describe_dropped("energy_security", names(weights$energy_security), energy_security_categories_scored),
  describe_dropped("economic_opportunity", names(weights$economic_opportunity), economic_opportunity_categories_scored)
)
if (length(dropped_categories) > 0) {
  warning(
    "Categories carry a weight in config/weights.yml but produced no score, so they are ",
    "excluded from the index and the remaining weights are renormalized over the rest ",
    "(same behaviour as the annual pipeline): ",
    paste(dropped_categories, collapse = " | "),
    call. = FALSE
  )
}

source_years <- dplyr::bind_rows(source_year_log)

write_vintage_csv <- function(tbl, name) {
  path <- file.path(vintage_dir, paste0(name, ".csv"))
  utils::write.csv(tbl, path, row.names = FALSE)
  path
}

write_vintage_csv(energy_security_by_year, "energy_security_index_by_year")
write_vintage_csv(economic_opportunity_by_year, "economic_opportunity_index_by_year")
write_vintage_csv(category_scores_by_year, "index_category_scores_by_year")
write_vintage_csv(theme_provenance, "index_vintage_theme_provenance")
write_vintage_csv(weight_coverage, "index_vintage_weight_coverage")
write_vintage_csv(source_years, "index_vintage_source_years")

if (length(comparisons) > 0) {
  label <- paste0(min(index_years), "_vs_", max(index_years))
  write_vintage_csv(comparisons$energy_security, paste0("energy_security_comparison_", label))
  write_vintage_csv(comparisons$economic_opportunity, paste0("economic_opportunity_comparison_", label))
}

saveRDS(
  list(
    index_years = index_years,
    vintages = vintages,
    energy_security_by_year = energy_security_by_year,
    economic_opportunity_by_year = economic_opportunity_by_year,
    category_scores_by_year = category_scores_by_year,
    comparisons = comparisons,
    theme_provenance = theme_provenance,
    weight_coverage = weight_coverage,
    source_years = source_years
  ),
  file.path(vintage_dir, "index_vintages.rds")
)

message("Wrote vintage outputs to: ", vintage_dir)
for (i in seq_len(nrow(weight_coverage))) {
  message(sprintf(
    "  %-22s %.0f%% of category weight re-derives per vintage (%g of %g)",
    weight_coverage$pillar[i],
    100 * weight_coverage$share_varying[i],
    weight_coverage$weight_varying[i],
    weight_coverage$weight_total[i]
  ))
}
