# Pure helpers for building the Economic Opportunity and Energy Security indices at a
# chosen year ("vintage") rather than only at the latest release.
#
# The annual pipeline builds one snapshot: every theme builder reaches for the newest
# observation its source carries, and R/indices/index_builder_core.R::latest_by_group()
# then keeps the max Year per group. Nothing in that path takes an "as of" year, so the
# processed theme tables in data/processed hold a single vintage and cannot be re-cut into
# a 2020 index after the fact.
#
# These helpers close that gap by slicing each *raw input* to the requested year before the
# existing theme builders see it. The builders themselves are untouched, so a vintage index
# is computed by exactly the same code as the headline index - only the input window moves.
#
# Two constraints are worth stating up front, because they shape how a vintage comparison
# should be read:
#
#   1. Every component is a cross-sectional percent rank (median_scurve). An index value is
#      a country's position relative to its peers in that year, not an absolute level. A
#      2020-to-2025 change therefore says "moved up/down the field", not "got better/worse"
#      in physical terms.
#   2. Only sources that carry a genuine time dimension can be re-cut. Sources staged as a
#      single snapshot or as forward projections (resource potential, reserves, IEA
#      critical minerals, BNEF demand/LCOE, overcapacity, TRL) are held fixed across
#      vintages and contribute nothing to the year-on-year delta. index_vintage_theme_spec()
#      is the registry of which is which, and the builder writes it out alongside the
#      indices so the split is visible in the outputs.

# ---------------------------------------------------------------------------
# Year handling
# ---------------------------------------------------------------------------

# Parse a "2020,2025" style specification into a sorted integer vector.
parse_index_years <- function(x, default = c(2020L, 2025L)) {
  if (is.null(x) || length(x) == 0) {
    return(as.integer(default))
  }

  if (is.numeric(x)) {
    years <- as.integer(x)
  } else {
    text <- paste(as.character(x), collapse = ",")
    if (!nzchar(trimws(text))) {
      return(as.integer(default))
    }
    pieces <- unlist(strsplit(text, "[,;[:space:]]+"))
    pieces <- pieces[nzchar(pieces)]
    years <- suppressWarnings(as.integer(pieces))
  }

  if (length(years) == 0 || any(is.na(years))) {
    stop("Could not parse index years from: ", paste(as.character(x), collapse = " "))
  }

  years <- sort(unique(years))
  if (any(years < 1900L | years > 2100L)) {
    stop("Index years outside the supported 1900-2100 range: ", paste(years, collapse = ", "))
  }

  years
}

# Snap a requested year onto the years a source actually publishes.
#
# Sources end at different points (the Atlas of Economic Complexity stops at 2023, the EI
# Statistical Review runs to 2025), so a requested 2025 vintage takes the newest year at or
# before 2025 that the source has. Falling *forward* to the earliest available year is only
# used when the request predates the source entirely, and the caller is expected to record
# the substitution.
resolve_available_year <- function(requested, available) {
  available <- sort(unique(as.integer(available[!is.na(available)])))
  if (length(available) == 0) {
    stop("No available years to resolve against.")
  }

  requested <- as.integer(requested)
  at_or_before <- available[available <= requested]
  if (length(at_or_before) > 0) {
    return(max(at_or_before))
  }

  min(available)
}

# ---------------------------------------------------------------------------
# Raw-input slicing
# ---------------------------------------------------------------------------

# Every slice below snaps the request onto the years the source actually publishes and
# records the year it landed on in a "vintage_year" attribute, so the builder can report
# when a source could not reach back as far as the request. IMF PPI, for example, starts in
# 2021: a 2020 vintage gets the 2021 panel, and the run says so rather than silently
# emptying the table.
vintage_year_of <- function(x) {
  year <- attr(x, "vintage_year", exact = TRUE)
  if (is.null(year)) NA_integer_ else as.integer(year)
}

# Period years present in a wide IMF panel (`X2024`, `X2024.Q1`, `X2024.M03` after
# read.csv name repair).
imf_wide_period_years <- function(imf_price) {
  if (is.null(imf_price) || ncol(imf_price) == 0) {
    return(integer(0))
  }

  cols <- names(imf_price)
  is_period <- grepl("^X\\d{4}(\\.(M\\d{2}|Q\\d))?$", cols)
  years <- suppressWarnings(as.integer(substr(cols[is_period], 2, 5)))
  sort(unique(years[!is.na(years)]))
}

# Dropping the period columns after the vintage year makes max(date) land on that year,
# which is what energy_prices() uses as its as-of date and as the right edge of every
# volatility window, and what the cost builders use when they take the newest observation
# per country.
vintage_slice_imf_wide <- function(imf_price, year) {
  period_years <- imf_wide_period_years(imf_price)
  if (length(period_years) == 0) {
    return(imf_price)
  }

  effective_year <- resolve_available_year(year, period_years)

  cols <- names(imf_price)
  is_period <- grepl("^X\\d{4}(\\.(M\\d{2}|Q\\d))?$", cols)
  col_year <- suppressWarnings(as.integer(substr(cols, 2, 5)))
  drop <- is_period & !is.na(col_year) & col_year > effective_year

  out <- imf_price[, cols[!drop], drop = FALSE]
  attr(out, "vintage_year") <- effective_year
  out
}

# ILO earnings are long with a `time` year column; the builder takes the newest year per
# country, so trimming the tail is enough to move the vintage.
vintage_slice_ilo <- function(ilo_raw, year) {
  vintage_slice_annual(ilo_raw, year, year_col = "time")
}

# Tail trim for a long table with a year column (GCIM annual investment, ILO earnings).
vintage_slice_annual <- function(tbl, year, year_col = "Year") {
  if (is.null(tbl) || !year_col %in% names(tbl)) {
    return(tbl)
  }

  observed <- suppressWarnings(as.integer(substr(as.character(tbl[[year_col]]), 1, 4)))
  available <- observed[!is.na(observed)]
  if (length(available) == 0) {
    return(tbl)
  }

  effective_year <- resolve_available_year(year, available)
  out <- tbl[!is.na(observed) & observed <= effective_year, , drop = FALSE]
  attr(out, "vintage_year") <- effective_year
  out
}

# ---------------------------------------------------------------------------
# Trade themes from the Atlas of Economic Complexity
# ---------------------------------------------------------------------------
#
# The annual pipeline's trade themes are Comtrade-led, and the staged Comtrade extract is a
# single year. The Atlas HS92 files carry 1995 onward for the same countries and products,
# so the vintage builder runs the trade themes off the Atlas for *every* year it builds.
# Using one source across all vintages matters more than matching the headline pipeline's
# source: a Comtrade-2024 numerator against an Atlas-2020 denominator would show source
# differences as if they were real change.
#
# Everything downstream of the two frames below is the existing trade_core code, so the
# index composition (market share, RCA, export size, feasibility, HHI) is unchanged.

# Atlas stores HS codes as integers, dropping the leading zero that HS chapters 01-09 need.
atlas_pad_hs_codes <- function(tbl, width, col = "product_hs92_code") {
  require_columns(tbl, col, label = "Atlas trade data")

  tbl[[col]] <- stringr::str_pad(
    as.character(tbl[[col]]),
    width = width,
    side = "left",
    pad = "0"
  )
  tbl
}

# Country x tech x supply chain energy trade, shaped like trade_core_build_comtrade_trade()
# so the rest of the trade pipeline can consume it unchanged.
trade_core_build_atlas_trade <- function(aec_6_data, energy_codes, year) {
  aec_6_data %>%
    trade_core_filter_year(year = year, label = "Atlas HS6 trade data") %>%
    dplyr::left_join(
      energy_codes,
      by = c("product_hs92_code" = "code6"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(tech), !is.na(supply_chain)) %>%
    dplyr::group_by(
      reporter_iso = .data$country_iso3_code,
      .data$tech,
      .data$supply_chain,
      .data$sub_sector
    ) %>%
    dplyr::summarize(
      exports = sum(as.numeric(.data$export_value), na.rm = TRUE),
      imports = sum(as.numeric(.data$import_value), na.rm = TRUE),
      .groups = "drop"
    )
}

# All-product exports per reporter, shaped like the Comtrade total-export extract that
# trade_core_build_comtrade_rca() divides by.
trade_core_build_atlas_total_export <- function(aec_6_data, year) {
  aec_6_data %>%
    trade_core_filter_year(year = year, label = "Atlas HS6 total export data") %>%
    dplyr::group_by(reporter_iso = .data$country_iso3_code) %>%
    dplyr::summarize(
      primary_value = sum(as.numeric(.data$export_value), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(year = as.integer(year))
}

# Full Trade-category theme table for one vintage year, Atlas end to end.
#
# `aec_6_data` and `aec_4_data` must already carry zero-padded character HS codes
# (atlas_pad_hs_codes()) and may be pre-filtered to the years of interest.
atlas_trade_theme <- function(aec_4_data,
                              aec_6_data,
                              subcat,
                              country_info,
                              year,
                              include_sub_sector = FALSE) {
  energy_codes <- trade_core_build_energy_codes(subcat, include_sub_sector = include_sub_sector)

  market_share <- trade_core_build_aec_market_share(aec_6_data, energy_codes, year = year)
  feasibility <- trade_core_build_aec_feasibility(aec_4_data, energy_codes, year = year)

  atlas_trade <- trade_core_build_atlas_trade(aec_6_data, energy_codes, year = year)
  if (nrow(atlas_trade) == 0) {
    stop("Atlas trade data produced no energy-tech rows for year ", year, ".")
  }

  total_export <- trade_core_build_atlas_total_export(aec_6_data, year = year)

  country_trade <- trade_core_build_country_trade(
    comtrade_trade = atlas_trade,
    comtrade_total_export = total_export,
    market_share = market_share,
    feasibility = feasibility,
    year = year
  )

  hhi_tbl <- trade_core_build_comtrade_hhi(atlas_trade)
  indices <- trade_core_build_indices(country_trade, hhi_tbl, country_info)

  trade_core_build_tidy(
    indices$trade_indices,
    year = year,
    include_sub_sector = include_sub_sector
  ) %>%
    # trade_core_build_tidy() attributes values to Comtrade; this build is Atlas-only.
    dplyr::mutate(
      source = dplyr::if_else(
        .data$source == "UN Comtrade",
        "Harvard Atlas of Economic Complexity",
        .data$source
      )
    ) %>%
    energy_security_add_overall_index(include_sub_sector = include_sub_sector)
}

# ---------------------------------------------------------------------------
# Which themes move with the vintage year, and which are held fixed
# ---------------------------------------------------------------------------
#
# `varies` is the honest answer to "does a 2020 run differ from a 2025 run here?", given
# the raw inputs staged in data/raw. `category` is the index category the theme scores into,
# so the builder can report what share of each pillar's weight actually re-derives.
index_vintage_theme_spec <- function() {
  tibble::tribble(
    ~theme,                            ~category,                 ~varies, ~basis,
    "energy_access_consumption",       "Energy Access",              TRUE,  "EI Statistical Review, annual 1965-2025",
    "energy_consumption",              "Consumption",                TRUE,  "EI Statistical Review annual; BNEF NEO projections held fixed",
    "import_dependence",               "Energy Imports",             TRUE,  "EI Statistical Review, annual 1965-2025",
    "production_depth_momentum",       "Production",                 TRUE,  "EI Statistical Review annual; IEA minerals supply held fixed",
    "trade_concentration",             "Trade",                      TRUE,  "Atlas of Economic Complexity HS92, annual 1995-2023",
    "export_feasibility",              "Trade",                      TRUE,  "Atlas of Economic Complexity HS92, annual 1995-2023",
    "energy_prices",                   "Energy Prices",              TRUE,  "IMF monthly commodity prices, volatility window ending in the vintage year",
    "cost_competitiveness",            "Cost Competitiveness",       TRUE,  "ILO earnings and IMF rates/PPI annual; IEA relative costs held fixed",
    "investment_momentum",             "Investment",                 TRUE,  "GCIM annual investment 2018-2025; capacity pipeline held fixed",
    "solar_pv_potential",              "Reserves",                   FALSE, "Global Solar Atlas resource potential, no time dimension",
    "wind_potential",                  "Reserves",                   FALSE, "Global Wind Atlas resource potential, no time dimension",
    "geothermal_potential",            "Reserves",                   FALSE, "Geothermal LCOE/potential snapshot, no time dimension",
    "reserves",                        "Reserves",                   FALSE, "EI reserves sheets publish end-of-latest-year stocks only",
    "foreign_dependency",              "Foreign Dependency",         FALSE, "IEA critical minerals and clean-tech shares, single release",
    "critical_minerals_processing",    "Foreign Dependency",         FALSE, "IEA critical minerals, single release",
    "critical_minerals_production",    "Production",                 FALSE, "EI production sheets, single release",
    "critical_minerals_trade",         "Minerals Trade",             FALSE, "Comtrade critical-minerals extract, single year staged",
    "future_demand",                   "Technology Demand",          FALSE, "IEA WEO / BNEF NEO / BCG forward projections",
    "overcapacity_premium",            "Technology Demand",          FALSE, "BNEF supply-chain snapshot",
    "market_share_manufacturing",      "Foreign Dependency",         FALSE, "IEA clean-tech manufacturing shares, single release",
    "lcoe_competitiveness",            "Cost Competitiveness",       FALSE, "BNEF LCOE projection to 2050",
    "technological_readiness",         "Technological Readiness",    FALSE, "IEA Clean Tech Guide TRL columns, single release"
  )
}

# Share of a pillar's category weight that re-derives with the vintage year.
#
# A category counts as varying when at least one theme feeding it varies, which is the right
# reading for a mixed category such as Production (EI output moves, the minerals half does
# not).
#
# `categories_present` should be the categories that actually produced a score in the run.
# It matters because a configured weight is not necessarily a live weight: the pipeline
# drops any category whose score variable it cannot find, and normalizes the remaining
# weights over what is left. Passing the observed categories keeps the reported share
# anchored to the index that was really built rather than to the config's intent.
index_vintage_weight_coverage <- function(weights,
                                          themes,
                                          categories_present = NULL,
                                          spec = index_vintage_theme_spec()) {
  weights_tbl <- tibble::tibble(
    category = names(weights),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  if (!is.null(categories_present)) {
    weights_tbl <- weights_tbl %>%
      dplyr::filter(.data$category %in% categories_present)
  }

  spec %>%
    dplyr::filter(.data$theme %in% themes) %>%
    dplyr::group_by(.data$category) %>%
    dplyr::summarize(varies = any(.data$varies), .groups = "drop") %>%
    dplyr::inner_join(weights_tbl, by = "category") %>%
    dplyr::summarize(
      weight_total = sum(.data$weight),
      weight_varying = sum(.data$weight[.data$varies]),
      share_varying = sum(.data$weight[.data$varies]) / sum(.data$weight),
      categories_scored = paste(sort(.data$category), collapse = "; "),
      categories_varying = paste(sort(.data$category[.data$varies]), collapse = "; ")
    )
}

# ---------------------------------------------------------------------------
# Cross-vintage comparison
# ---------------------------------------------------------------------------

# Turn a stacked index (one row per key per index_year) into a wide comparison carrying the
# level in each year, the change between the first and last year, and the same for rank.
#
# Rank is within-year and within tech x supply chain, ascending on 1 = best. It is reported
# alongside the level because the level is itself a relative measure: a country can hold a
# flat index value while the field moves around it.
index_vintage_comparison <- function(index_by_year,
                                     index_col,
                                     key_cols = c("Country", "tech", "supply_chain"),
                                     rank_within = c("tech", "supply_chain"),
                                     higher_is_better = TRUE) {
  require_columns(index_by_year, c("index_year", key_cols, index_col), label = "index_by_year")

  years <- sort(unique(as.integer(index_by_year$index_year)))
  if (length(years) < 2) {
    stop("index_vintage_comparison() needs at least two vintage years; got ", length(years), ".")
  }
  base_year <- years[1]
  final_year <- years[length(years)]

  rank_within <- intersect(rank_within, names(index_by_year))
  rank_sign <- if (isTRUE(higher_is_better)) -1 else 1

  ranked <- index_by_year %>%
    dplyr::mutate(index_year = as.integer(.data$index_year)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("index_year", rank_within)))) %>%
    dplyr::mutate(
      .rank = dplyr::if_else(
        is.na(.data[[index_col]]),
        NA_real_,
        rank(rank_sign * .data[[index_col]], na.last = "keep", ties.method = "min")
      )
    ) %>%
    dplyr::ungroup()

  levels_wide <- ranked %>%
    dplyr::select(dplyr::all_of(c(key_cols, "index_year", index_col))) %>%
    tidyr::pivot_wider(
      names_from = "index_year",
      values_from = dplyr::all_of(index_col),
      names_prefix = "index_"
    )

  ranks_wide <- ranked %>%
    dplyr::select(dplyr::all_of(c(key_cols, "index_year")), .rank) %>%
    tidyr::pivot_wider(
      names_from = "index_year",
      values_from = ".rank",
      names_prefix = "rank_"
    )

  base_col <- paste0("index_", base_year)
  final_col <- paste0("index_", final_year)
  base_rank_col <- paste0("rank_", base_year)
  final_rank_col <- paste0("rank_", final_year)

  levels_wide %>%
    dplyr::left_join(ranks_wide, by = key_cols) %>%
    dplyr::mutate(
      index_change = .data[[final_col]] - .data[[base_col]],
      # Rank 1 is best, so a fall in rank number is an improvement; flip the sign to keep
      # "positive means better" consistent with index_change.
      rank_change = .data[[base_rank_col]] - .data[[final_rank_col]],
      comparison = paste0(base_year, "_to_", final_year)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$index_change))
}
