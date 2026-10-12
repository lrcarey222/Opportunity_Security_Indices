# Production depth + momentum theme builder functions.

# EI fuel production variables and the theme's cell labels; EIA fills these (see below).
PRODUCTION_EI_FUEL_VARS <- c(
  "Oil Upstream" = "oilprod_kbd",
  "Gas Upstream" = "gasprod_ej",
  "Coal Upstream" = "coalprod_ej"
)

# Every economy EI lists for any of `vars` in any of `years` is scored on every cell.
#
# Fuel production (Oil, Gas, Coal Upstream) EI does not report for one of them: with `eia` (the
# staged EIA International Energy Statistics extract, R/utils/eia_intl.R) the cell takes the
# EIA value of the same year in EI's units (gas and coal, which EIA publishes a year after oil,
# the latest EIA year within `eia_max_lag` years), recorded in `eia_note` and `eia_used`; a cell
# EIA has no value for, or one whose fuel is not EI's (EIA_INTL_COMPARABILITY: Estonia coal,
# where EI counts oil shale), is NA and the pillar's missing-data rule fills its index. EI's production
# tables list fewer economies than its consumption and generation tables and fold the rest into
# "Other <region>" residuals, so an absent row is not a zero. Without `eia` (fixtures, older
# callers) such a cell is 0, as before 2026-10-11.
#
# Generation (the Downstream cells) EI does not report is still 0, with or without `eia`: EI's
# electricity-by-fuel table lists 31 of the 99 economies, and on 2026-10-11 EIA had positive net
# generation for 35 (coal), 60 (gas) and 63 (oil) of the other 68 (scratch notes, fixes8).
production_depth_momentum_clean_energy_production <- function(ei,
                                                              years = c(2015, 2020, 2025),
                                                              vars = c(
                                                                "oilprod_kbd",
                                                                "gasprod_ej",
                                                                "coalprod_ej",
                                                                "solar_twh",
                                                                "wind_twh",
                                                                "nuclear_twh",
                                                                "electbyfuel_coal",
                                                                "electbyfuel_gas",
                                                                "electbyfuel_oil"
                                                              ),
                                                              eia = NULL,
                                                              eia_max_lag = 1L) {
  require_columns(ei, c("Country", "Year", "Var", "Value"), label = "ei")

  # Reference the observation years by value rather than by literal column name so a new
  # EI release only needs the `years` argument moved.
  years <- sort(unique(as.integer(years)))
  yr_base10 <- as.character(years[1])
  yr_base5 <- as.character(years[2])
  yr_latest <- as.character(years[length(years)])

  long <- ei %>%
    dplyr::select(Country, Year, Var, Value) %>%
    # Standardized before ranking; "USSR" (EI's CIS aggregate) goes with the aggregates.
    dplyr::mutate(
      Country = standardize_country_names(Country),
      Year = as.integer(Year),
      Value = as.numeric(Value)
    ) %>%
    dplyr::filter(
      !grepl("Other|Total|OECD|OPEC|World|USSR", Country),
      Year %in% years,
      Var %in% vars
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      Var = dplyr::case_when(
        Var == "oilprod_kbd" ~ "Oil Upstream",
        Var == "gasprod_ej" ~ "Gas Upstream",
        Var == "coalprod_ej" ~ "Coal Upstream",
        Var == "solar_twh" ~ "Solar Downstream",
        Var == "wind_twh" ~ "Wind Downstream",
        Var == "nuclear_twh" ~ "Nuclear Downstream",
        Var == "electbyfuel_coal" ~ "Coal Downstream",
        Var == "electbyfuel_gas" ~ "Gas Downstream",
        Var == "electbyfuel_oil" ~ "Oil Downstream",
        TRUE ~ Var
      )
    ) %>%
    dplyr::group_by(Country, Year, Var) %>%
    dplyr::summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  eia_notes <- NULL
  fuel_vars <- PRODUCTION_EI_FUEL_VARS[PRODUCTION_EI_FUEL_VARS %in% vars]
  if (!is.null(eia) && length(fuel_vars) > 0) {
    filled <- production_depth_momentum_fill_eia(long, ei, eia, fuel_vars, years, eia_max_lag)
    # Explicit rows, NA where EIA has no value either, so complete() below leaves them alone.
    long <- dplyr::bind_rows(long, dplyr::transmute(filled, Country, Year, Var, Value = value))
    eia_notes <- production_depth_momentum_eia_notes(filled, yr_latest)
  }

  production <- long %>%
    tidyr::complete(
      Country,
      Year,
      Var,
      fill = list(Value = 0),
      explicit = FALSE
    ) %>%
    tidyr::pivot_wider(names_from = "Year", values_from = "Value") %>%
    dplyr::mutate(
      change_5yr = (.data[[yr_latest]] / .data[[yr_base5]] - 1) * 100,
      change_10yr = (.data[[yr_latest]] / .data[[yr_base10]] - 1) * 100,
      change_5yr_abs = .data[[yr_latest]] - .data[[yr_base5]]
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        dplyr::all_of(c(yr_base10, yr_base5, yr_latest)),
        change_5yr,
        change_10yr,
        change_5yr_abs
      ),
      names_to = "year",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = year, values_from = value)

  if (!is.null(eia_notes)) {
    production <- dplyr::left_join(production, eia_notes, by = c("Country", "Var"))
  }
  production
}

# The fuel cells (economy x year x Var label) of `long` EI does not report, with the EIA value
# (eia_intl_lookup(): value, eia_year, series_id, eia_basis, eia_consumption_coverage). ISO3 codes
# come from EI's own ISO3166_alpha3 column. Each value is checked against EI's consumption of the
# fuel in the same year where EI has it (EIA_INTL_COMPARABILITY: Estonia coal fails, because EI
# counts oil shale as coal).
production_depth_momentum_fill_eia <- function(long, ei, eia, fuel_vars, years, max_lag = 1L) {
  absent <- tidyr::crossing(Country = unique(long$Country), Year = as.integer(years), Var = names(fuel_vars)) %>%
    dplyr::anti_join(dplyr::distinct(long, Country, Year, Var), by = c("Country", "Year", "Var"))
  iso3 <- ei_iso3_lookup(ei)
  consumption <- ei %>%
    dplyr::filter(Var %in% EIA_INTL_CONSUMPTION_OF) %>%
    dplyr::transmute(
      Country = standardize_country_names(Country),
      Year = as.integer(Year),
      cons_var = Var,
      ei_consumption = as.numeric(Value)
    ) %>%
    dplyr::filter(Year %in% as.integer(years), !is.na(Country)) %>%
    dplyr::group_by(Country, Year, cons_var) %>%
    dplyr::summarize(ei_consumption = sum(ei_consumption, na.rm = TRUE), .groups = "drop")
  cells <- absent %>%
    dplyr::mutate(
      iso3 = iso3$iso3[match(Country, iso3$Country)],
      ei_var = unname(fuel_vars[Var]),
      year = Year,
      cons_var = unname(EIA_INTL_CONSUMPTION_OF[ei_var])
    ) %>%
    dplyr::left_join(consumption, by = c("Country", "Year", "cons_var"))
  eia_intl_lookup(cells, eia, max_lag = max_lag, ei_consumption = cells$ei_consumption) %>%
    dplyr::select(Country, Year, Var, ei_var, value, eia_year, eia_basis, series_id, eia_consumption_coverage)
}

# One note per economy and fuel cell EI does not report in some year, for the explanation of its
# rows: the years EIA supplies (from which EIA year, how converted) and the years left NA (no EIA
# value, or one that fails the comparability check). `eia_used`: some year has an EIA value, so
# the rows' source names EIA beside EI.
production_depth_momentum_eia_notes <- function(filled, yr_latest) {
  if (nrow(filled) == 0) {
    return(tibble::tibble(Country = character(), Var = character(), eia_note = character(), eia_used = logical()))
  }
  filled %>%
    dplyr::mutate(
      fuel = sub(" Upstream$", "", Var),
      used = !is.na(value),
      not_comparable = eia_basis %in% "not_comparable",
      year_txt = dplyr::case_when(
        !used ~ as.character(Year),
        eia_basis == "no_series" ~ paste0(Year, " (no EIA production series: 0)"),
        eia_year != Year ~ paste0(Year, " (EIA ", eia_year, ", its latest year)"),
        TRUE ~ as.character(Year)
      )
    ) %>%
    dplyr::arrange(Country, Var, Year) %>%
    dplyr::group_by(Country, Var) %>%
    dplyr::summarize(
      eia_used = any(used),
      eia_note = paste0(
        "EI does not report ", tolower(dplyr::first(fuel)), " production for this economy separately",
        if (any(used)) {
          paste0(
            "; ", EIA_INTL_SOURCE, " for ", paste(year_txt[used], collapse = ", "), " (",
            eia_intl_conversion_text(dplyr::first(ei_var)), ")"
          )
        } else "",
        if (any(not_comparable)) {
          paste0(
            "; NA in ", paste(year_txt[not_comparable], collapse = ", "), ": ",
            eia_intl_not_comparable_text(dplyr::first(fuel), eia_consumption_coverage[not_comparable])
          )
        } else "",
        if (any(!used & !not_comparable)) {
          paste0("; NA in ", paste(year_txt[!used & !not_comparable], collapse = ", "), " (no EIA value)")
        } else ""
      ),
      .groups = "drop"
    )
}

production_depth_momentum_build_energy_indices <- function(production_clean,
                                                           latest_year = 2025,
                                                           gamma = 0.5) {
  yr_latest <- as.character(latest_year)

  require_columns(
    production_clean,
    c("Country", "Var", yr_latest, "change_5yr", "change_10yr", "change_5yr_abs"),
    label = "production_clean"
  )

  production_clean %>%
    tidyr::separate(
      Var,
      into = c("tech", "supply_chain"),
      sep = " (?=Upstream|Downstream)",
      remove = TRUE
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      size_index = median_scurve(.data[[yr_latest]], gamma = gamma),
      growth_index = median_scurve(change_5yr, gamma = gamma),
      growth_abs_index = median_scurve(change_5yr_abs, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      overall_production_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(overall_production_index = median_scurve(overall_production_index, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))
}

production_depth_momentum_build_mineral_supply <- function(critical,
                                                           mineral_demand_clean,
                                                           country_info,
                                                           gamma = 0.5,
                                                           base_year = iea_critical_minerals_base_year(critical),
                                                           horizon_year = 2035) {
  require_columns(
    critical,
    c("Pillar", "Sector.Country", "Mineral"),
    label = "critical"
  )
  # The IEA restates its base year each release, so address the supply columns by year.
  base_col <- iea_critical_minerals_year_col(critical, base_year, label = "mineral supply")
  horizon_col <- iea_critical_minerals_year_col(critical, horizon_year, label = "mineral supply")
  require_columns(
    mineral_demand_clean,
    c("Mineral", "tech", "share_24", "share_35"),
    label = "mineral_demand_clean"
  )
  require_columns(country_info, "country", label = "country_info")

  # Standardized so "Korea" and "Lao People's Democratic Republic" meet country_info.
  critical <- critical %>%
    dplyr::mutate(`Sector.Country` = standardize_country_names(`Sector.Country`))

  minerals <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::distinct(mineral)

  if (nrow(minerals) == 0) {
    stop("No minerals found in critical supply data.")
  }

  countries <- critical %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::inner_join(
      country_info %>% dplyr::select(country),
      by = c("Sector.Country" = "country")
    )

  if (nrow(countries) == 0) {
    stop("No matching countries found in critical supply data.")
  }

  critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    dplyr::rename(country = `Sector.Country`) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    # Canonical mineral names, so graphite and rare earths meet their demand shares; stage
    # from the shared IEA stage map (battery-grade graphite is processing, i.e. Midstream).
    dplyr::mutate(
      mineral = harmonize_mineral_names(mineral),
      supply_chain = iea_critical_minerals_supply_stage(supply_chain_raw)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      mineral, country, supply_chain,
      supply_base = dplyr::all_of(base_col),
      supply_horizon = dplyr::all_of(horizon_col)
    ) %>%
    tidyr::complete(
      tidyr::nesting(mineral, supply_chain),
      country = countries$`Sector.Country`,
      fill = list(supply_base = 0, supply_horizon = 0)
    ) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::filter(!is.na(tech)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(Mineral = harmonize_mineral_names(Mineral), tech, share_24, share_35),
      by = c("mineral" = "Mineral"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      supply_24 = share_24 * supply_base,
      supply_35 = share_35 * supply_horizon
    ) %>%
    dplyr::group_by(mineral, tech, supply_chain) %>%
    dplyr::mutate(
      share_frac_24 = supply_24 / sum(supply_24, na.rm = TRUE),
      HHI_24 = sum(share_frac_24^2, na.rm = TRUE),
      share_frac_35 = supply_35 / sum(supply_35, na.rm = TRUE),
      HHI_35 = sum(share_frac_35^2, na.rm = TRUE),
      # World supply (the named producers plus "Rest of world"), kept before that row is
      # dropped so production can be put in shares of world supply of each mineral.
      world_base = sum(supply_base, na.rm = TRUE),
      horizon_year = as.integer(horizon_year)
    ) %>%
    dplyr::filter(
      country != "Rest of world",
      !is.na(share_frac_24),
      !is.na(share_frac_35)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      market_share24_index = median_scurve(share_frac_24, gamma = gamma),
      market_share35_index = median_scurve(share_frac_35, gamma = gamma),
      hhi24_index = median_scurve(1 - HHI_24, gamma = gamma),
      hhi35_index = median_scurve(1 - HHI_35, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      security_index = mean(dplyr::c_across(market_share24_index:hhi35_index), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::arrange(dplyr::desc(security_index))
}

# One production level per country and technology from the IEA mined supply of the
# technology's minerals.
#
# Tonnes of different minerals cannot be averaged or added: copper is mined in tens of
# millions of tonnes, cobalt, lithium and magnet rare earths in tens to hundreds of thousands,
# so a mean of a country's kilotonnes was copper's tonnage diluted by however many other
# minerals the technology uses, and ranking all country-mineral rows together scored a country
# on how many of the minerals it mines rather than how much (Russia, a mid-sized producer of
# most of them, ranked in the top two on size in five of the six technologies).
# Each mineral therefore enters as the country's share of world supply, and the technology's
# minerals are combined with its share of each mineral's demand (share_24, renormalised over
# the minerals the supply sheet reports), the weights of the reserves and critical-minerals
# production rollups:
#
#   supply_base    = sum_m w_m * S_m,base / W_m,base     (the raw production level)
#   supply_horizon = sum_m w_m * S_m,horizon / W_m,base
#
# The horizon level is divided by the base-year world supply too, so the change between the
# two is a change in quantity in base-year world-share units, not a change in market share.
# Growth is that absolute change, as for oil, gas, coal and uranium: the relative change was
# undefined for a country mining none of the minerals, and those NAs were then filled with the
# cell's average, ranking non-producers above the smallest producers. A country the supply
# sheet does not name for a mineral counts as 0 there, as it already did for the level.
production_depth_momentum_build_mineral_production <- function(mineral_supply, gamma = 0.5) {
  require_columns(
    mineral_supply,
    c(
      "mineral", "country", "tech", "supply_chain", "supply_base", "supply_horizon",
      "world_base", "share_24", "HHI_24"
    ),
    label = "mineral_supply"
  )
  if (!"horizon_year" %in% names(mineral_supply)) {
    mineral_supply$horizon_year <- NA_integer_
  }

  mineral_supply %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(share_24), share_24 > 0, !is.na(world_base), world_base > 0) %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::mutate(weight = share_24 / sum(share_24)) %>%
    dplyr::summarize(
      supply_base = sum(weight * dplyr::coalesce(supply_base, 0) / world_base),
      supply_horizon = sum(weight * dplyr::coalesce(supply_horizon, 0) / world_base),
      # A world-market term, the same for every country of the technology.
      HHI_24 = sum(weight * HHI_24),
      horizon_year = dplyr::first(horizon_year),
      .groups = "drop"
    ) %>%
    dplyr::mutate(supply_growth = supply_horizon - supply_base) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      size_index = median_scurve(supply_base, gamma = gamma),
      growth_abs_index = median_scurve(supply_growth, gamma = gamma),
      HHI_index = median_scurve(-HHI_24, gamma = gamma),
      overall_production_index = median_scurve(
        2 * size_index + 1 * growth_abs_index + 1 * HHI_index,
        gamma = gamma
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Country = dplyr::if_else(country == "US", "United States", country)) %>%
    dplyr::select(-country)
}

production_depth_momentum_build_combined <- function(production_growth,
                                                     mineral_production,
                                                     energy_year = 2025,
                                                     mineral_year = 2025,
                                                     energy_source = "EI Statistical Review World Energy",
                                                     mineral_source = "IEA Critical Minerals Database") {
  energy_year_col <- as.character(energy_year)

  require_columns(
    production_growth,
    c("Country", "tech", "supply_chain", energy_year_col, "size_index", "growth_abs_index", "overall_production_index"),
    label = "production_growth"
  )
  require_columns(
    mineral_production,
    c("Country", "tech", "supply_chain", "supply_base", "size_index", "growth_abs_index", "overall_production_index"),
    label = "mineral_production"
  )

  # EI energy production and IEA critical mineral production carry different latest
  # years, so the raw level is unified under one year-neutral column and the source
  # year is kept alongside it for the explanation text. Each block carries its own source
  # and measure: the mineral rows were labelled EI although they are IEA mined supply.
  production_tbl <- production_growth %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      production_latest = .data[[energy_year_col]],
      raw_year = energy_year,
      size_index,
      growth_abs_index,
      overall_production_index,
      block = "energy",
      source = energy_source,
      # Set where EI does not report a fuel value (production_depth_momentum_clean_energy_production());
      # eia_used where EIA supplied one.
      eia_note = if ("eia_note" %in% names(production_growth)) eia_note else NA_character_,
      eia_used = if ("eia_used" %in% names(production_growth)) eia_used %in% TRUE else FALSE
    )
  mineral_tbl <- mineral_production %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      production_latest = supply_base,
      raw_year = mineral_year,
      horizon_year = if ("horizon_year" %in% names(mineral_production)) horizon_year else NA_integer_,
      size_index,
      growth_abs_index,
      overall_production_index,
      block = "mineral",
      source = mineral_source
    )

  production_tbl <- standardize_bind_rows_inputs(production_tbl)
  mineral_tbl <- standardize_bind_rows_inputs(mineral_tbl)

  dplyr::bind_rows(production_tbl, mineral_tbl)
}

production_depth_momentum_build_table <- function(production_combined,
                                                  year = 2025,
                                                  source = "EI Statistical Review World Energy") {
  require_columns(
    production_combined,
    c("Country", "tech", "supply_chain", "production_latest", "raw_year", "size_index", "growth_abs_index", "overall_production_index"),
    label = "production_combined"
  )
  # Rows from production_depth_momentum_build_combined() carry their block and source; the
  # uranium rows come without them and take `source`.
  if (!"source" %in% names(production_combined)) {
    production_combined$source <- source
  }
  if (!"block" %in% names(production_combined)) {
    production_combined$block <- "energy"
  }
  if (!"horizon_year" %in% names(production_combined)) {
    production_combined$horizon_year <- NA_integer_
  }
  # A fuel cell with an EIA value names EIA beside EI in its source and says which years came
  # from it; an EIA value is observed data, not an imputation.
  if (!"eia_note" %in% names(production_combined)) {
    production_combined$eia_note <- NA_character_
  }
  if (!"eia_used" %in% names(production_combined)) {
    production_combined$eia_used <- !is.na(production_combined$eia_note)
  }
  from_eia <- production_combined$eia_used %in% TRUE
  production_combined$source[from_eia] <- paste0(
    production_combined$source[from_eia], "; EIA International Energy Statistics"
  )

  production_combined %>%
    dplyr::filter(supply_chain != "Midstream") %>%
    tidyr::pivot_longer(
      cols = c(production_latest, size_index, growth_abs_index, overall_production_index),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(metric, "_index"), "index", "raw"),
      metric_clean = stringr::str_remove(metric, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Production",
      explanation = dplyr::case_when(
        block == "mineral" & metric_clean == "production_latest" ~
          glue::glue(
            "Share of world mined supply of the minerals {tech} uses in {raw_year}, each mineral ",
            "weighted by {tech}'s share of its demand (0-1)"
          ),
        block == "mineral" & metric_clean == "size" ~
          glue::glue("Percent-rank of {tech}'s demand-weighted share of world mined supply in {raw_year}"),
        block == "mineral" & metric_clean == "growth_abs" ~
          glue::glue(
            "Percent-rank of the projected change in that supply, {raw_year}-{horizon_year} ",
            "(IEA base case), in {raw_year} world-supply units"
          ),
        block == "mineral" & metric_clean == "overall_production" ~
          glue::glue(
            "Composite index: percent-rank of 2 x size + growth (plus a concentration term common ",
            "to every country) for {tech}"
          ),
        metric_clean == "production_latest" ~
          glue::glue("Raw production of {tech} ({supply_chain}) in {raw_year}"),
        metric_clean == "size" ~
          glue::glue("Min-max scaled production in {raw_year} for {tech}"),
        metric_clean == "growth_abs" ~
          glue::glue("Min-max scaled absolute growth index for {tech}"),
        metric_clean == "overall_production" ~
          glue::glue("Composite index: mean of size & growth indices, then min-max scaled for {tech}"),
        TRUE ~ metric_clean
      ),
      explanation = dplyr::if_else(
        is.na(eia_note),
        as.character(explanation),
        paste0(explanation, ". ", eia_note)
      )
    ) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category,
      variable = dplyr::if_else(metric_clean == "overall_production", "Overall Production", metric_clean),
      data_type,
      value,
      Year = as.integer(year),
      source,
      explanation
    )
}

# Nuclear Upstream production: uranium mine output scored like oil, gas and coal.
#
# The Red Book series (R/categories/reserves/uranium.R) ends a year before the EI one, so it
# is scored on its own latest year not after `year` and the 5 years before it; the raw row's
# explanation records its year. `countries` is every economy to score: Table 1.18 lists
# every producer, so an economy it omits produced nothing (an observed zero, as on the
# Energy Security side), not an unknown for the missing-data rule to fill.
production_depth_momentum_build_uranium <- function(uranium_production,
                                                    countries,
                                                    year,
                                                    gamma = 0.5) {
  uranium_year <- uranium_production_latest_year(uranium_production, year = year)
  uranium_clean <- uranium_production_clean(
    uranium_production,
    countries = countries,
    latest_year = uranium_year
  )

  uranium_combined <- production_depth_momentum_build_energy_indices(
    uranium_clean,
    latest_year = uranium_year,
    gamma = gamma
  ) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      production_latest = .data[[as.character(uranium_year)]],
      raw_year = uranium_year,
      size_index,
      growth_abs_index,
      overall_production_index
    )

  production_depth_momentum_build_table(
    production_combined = uranium_combined,
    year = year,
    source = uranium_source_label(uranium_production$source)
  )
}

# `uranium_production` (data/reference/uranium_production.csv) adds Nuclear Upstream; without
# it the theme has no Nuclear Upstream rows, as before.
production_depth_momentum <- function(ei,
                                      critical,
                                      mineral_demand_clean,
                                      country_info,
                                      year = 2025,
                                      mineral_year = iea_critical_minerals_base_year(critical),
                                      gamma = 0.5,
                                      uranium_production = NULL,
                                      eia = NULL,
                                      eia_max_lag = 1L) {
  # `eia`: the staged EIA International Energy Statistics extract, which supplies the fuel
  # production EI does not report (production_depth_momentum_clean_energy_production()).
  production_clean <- production_depth_momentum_clean_energy_production(
    ei,
    years = c(year - 10L, year - 5L, year),
    eia = eia,
    eia_max_lag = eia_max_lag
  )
  production_growth <- production_depth_momentum_build_energy_indices(
    production_clean,
    latest_year = year,
    gamma = gamma
  )
  mineral_supply <- production_depth_momentum_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    gamma = gamma,
    base_year = mineral_year
  )
  mineral_production <- production_depth_momentum_build_mineral_production(mineral_supply, gamma = gamma)
  production_combined <- production_depth_momentum_build_combined(
    production_growth = production_growth,
    mineral_production = mineral_production,
    energy_year = year,
    mineral_year = mineral_year,
    # The edition label the other EI themes carry (R/utils/schema.R), derived from the EI
    # data year read rather than written out, so the themes name the same edition.
    energy_source = ei_stat_review_source(year)
  )

  output <- production_depth_momentum_build_table(
    production_combined = production_combined,
    year = year
  )

  if (!is.null(uranium_production)) {
    # The EI fuel roster plus every country_info economy: the EI roster alone left ~110
    # economies with a global-average uranium output although the Red Book says they had none.
    output <- uranium_bind_theme_rows(
      output,
      production_depth_momentum_build_uranium(
        uranium_production,
        countries = unique(c(production_clean$Country, country_info$country)),
        year = year,
        gamma = gamma
      )
    )
  }

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
