# Uranium (Nuclear - Upstream) theme inputs: identified recoverable resources and mine
# production by country, from the OECD-NEA/IAEA "Red Book" (Uranium: Resources, Production
# and Demand).
#
# Until these were added, the only Nuclear - Upstream input was the world uranium price, so
# Energy Security was identical for every economy in that sector and the Strategic Index had
# to drop it. The Red Book tables are transcribed in data/reference/uranium_resources.csv and
# data/reference/uranium_production.csv, one row per country, variable and year, each with
# its source table, URL and access date. The builders below turn them into the same variables
# the reserves and production themes build for the other fuels and minerals:
#
#   * "Nuclear Reserves" (Reserves theme, Energy Security): identified recoverable resources
#     in one Red Book cost category, ranked like the EI reserve tables in
#     reserves_build_reserve_table();
#   * "Nuclear Production" (Energy Security production theme): latest-year mine production,
#     ranked like the EI mineral production tables in
#     critical_minerals_production_build_table();
#   * Nuclear Upstream size / growth (production_depth_momentum, Economic Opportunity): mine
#     production run through the same depth-and-momentum indices as oil, gas and coal.
#
# All functions are pure; scripts/10_build_themes.R reads the CSVs.

# The cost category the Red Book headlines: the 2026 edition defines total identified
# resources as RAR plus inferred resources recoverable at < USD 260/kgU, maps that category
# (Figure 1.1) and leads Table 1.1 with it. It is also the only category in which countries
# that supply no cost data (India, Jordan, Bulgaria) report anything (Table 1.2a, note d), so
# a lower category would score them as holding nothing.
URANIUM_DEFAULT_COST_CATEGORY <- "lt_usd260"

uranium_reference_columns <- function() {
  c("country", "iso3c", "variable", "value", "unit", "year", "source", "url", "accessed")
}

uranium_resources_variable <- function(cost_category = URANIUM_DEFAULT_COST_CATEGORY) {
  paste0("identified_recoverable_resources_", cost_category, "_kgu")
}

uranium_cost_category_label <- function(cost_category = URANIUM_DEFAULT_COST_CATEGORY) {
  usd <- sub("^lt_usd", "", cost_category)
  paste0("<USD ", usd, "/kgU")
}

# Check a uranium reference table and return it with typed columns. One value per country,
# variable and year, numeric and non-negative, and every row carrying its source.
uranium_validate_reference <- function(tbl, label = "uranium reference") {
  require_columns(tbl, uranium_reference_columns(), label = label)

  out <- tibble::as_tibble(tbl) %>%
    dplyr::mutate(
      iso3c = normalize_iso3c(iso3c),
      variable = as.character(variable),
      value = suppressWarnings(as.numeric(value)),
      year = suppressWarnings(as.integer(year)),
      source = as.character(source)
    )

  if (any(is.na(out$iso3c))) {
    stop(label, ": rows without an ISO3 code: ", paste(unique(out$country[is.na(out$iso3c)]), collapse = ", "))
  }
  if (any(is.na(out$value)) || any(out$value < 0)) {
    stop(label, ": values must be numeric and non-negative.")
  }
  if (any(is.na(out$source) | !nzchar(out$source))) {
    stop(label, ": every row needs a source.")
  }
  dupes <- out %>% dplyr::count(iso3c, variable, year) %>% dplyr::filter(n > 1)
  if (nrow(dupes) > 0) {
    stop(
      label, ": more than one value per country, variable and year (",
      paste(utils::head(paste(dupes$iso3c, dupes$variable, dupes$year), 5), collapse = "; "), ")."
    )
  }
  out
}

# Publication label without the table detail ("OECD-NEA/IAEA (2026), Uranium 2026: ...,
# NEA No. 7770"), for the theme's source column.
uranium_source_label <- function(source) {
  labels <- unique(trimws(sub(",\\s*Table.*$", "", source)))
  paste(sort(labels), collapse = "; ")
}

# Economies a uranium variable is completed over: the theme's own country reference plus
# every economy the Red Book lists. The EI rosters the themes use omit Namibia, Niger and
# most other African producers, and an inner join on them would drop those producers.
#
# `country_reference` needs a Country column; an ISO3166_alpha3 or iso3c code is used where a
# row carries one and the name is matched otherwise.
uranium_country_roster <- function(country_reference, iso3c) {
  code_col <- intersect(c("ISO3166_alpha3", "iso3c"), names(country_reference))[1]
  ref_iso <- if (is.na(code_col)) {
    rep(NA_character_, nrow(country_reference))
  } else {
    normalize_iso3c(country_reference[[code_col]])
  }
  ref_iso <- dplyr::coalesce(ref_iso, country_name_to_iso3(country_reference$Country))

  reference <- tibble::tibble(
    Country = as.character(country_reference$Country),
    ISO3166_alpha3 = ref_iso
  ) %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, .keep_all = TRUE)

  extra <- setdiff(unique(normalize_iso3c(iso3c)), reference$ISO3166_alpha3)
  extra <- extra[!is.na(extra)]

  dplyr::bind_rows(
    reference,
    tibble::tibble(Country = country_name_from_iso3(extra), ISO3166_alpha3 = extra)
  )
}

# Percent-rank among positive holders plus a zero anchor, as the reserve and production
# tables do. A reported zero gets the anchor's value, 0, directly: ranked with ties it shared
# the anchor's averaged rank and scored above an economy the table does not list, which put
# a historical producer with no current output (France, Gabon) above a non-producer.
uranium_anchor_index <- function(value, gamma = 0.5) {
  index <- rep(NA_real_, length(value))
  index[!is.na(value) & value <= 0] <- 0

  positive <- !is.na(value) & value > 0
  if (any(positive)) {
    index[positive] <- median_scurve(c(0, value[positive]), gamma = gamma)[-1]
  }
  index
}

# "Nuclear Reserves": identified recoverable uranium resources in one cost category.
#
# Countries the Red Book does not list are completed as NA, as in the EI reserve tables; the
# reserves missing-data rule scores them 0.
uranium_reserves_table <- function(uranium_resources,
                                   country_reference,
                                   cost_category = URANIUM_DEFAULT_COST_CATEGORY,
                                   year = 2025,
                                   gamma = 0.5) {
  resources <- uranium_validate_reference(uranium_resources, label = "uranium_resources")
  target <- uranium_resources_variable(cost_category)
  resources <- resources %>% dplyr::filter(variable == target)

  if (nrow(resources) == 0) {
    stop(
      "uranium_resources has no rows for ", target, "; available: ",
      paste(sort(unique(uranium_resources$variable)), collapse = ", ")
    )
  }
  if (dplyr::n_distinct(resources$year) != 1) {
    stop("uranium_resources mixes reference years for ", target, ".")
  }

  data_year <- resources$year[[1]]
  unit <- unique(resources$unit)
  roster <- uranium_country_roster(country_reference, resources$iso3c)

  resources %>%
    dplyr::transmute(
      ISO3166_alpha3 = iso3c,
      raw_value = value,
      index_value = uranium_anchor_index(value, gamma = gamma)
    ) %>%
    tidyr::complete(ISO3166_alpha3 = roster$ISO3166_alpha3) %>%
    dplyr::left_join(roster, by = "ISO3166_alpha3") %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = "Nuclear",
      supply_chain = "Upstream",
      category = "Reserves",
      variable = "Nuclear Reserves",
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = as.integer(year),
      source = uranium_source_label(resources$source),
      explanation = dplyr::if_else(
        data_type == "raw",
        paste0(
          "Identified recoverable uranium resources (reasonably assured plus inferred, ",
          uranium_cost_category_label(cost_category), ", ", paste(unit, collapse = "/"),
          ") as of 1 January ", data_year, ", Red Book Table 1.2a"
        ),
        "Percent-rank of identified uranium resources across reporting countries (plus a zero anchor)"
      )
    ) %>%
    dplyr::select(
      Country, ISO3166_alpha3, tech, supply_chain, category, variable,
      data_type, value, Year, source, explanation
    )
}

uranium_production_rows <- function(uranium_production) {
  production <- uranium_validate_reference(uranium_production, label = "uranium_production")
  production <- production %>% dplyr::filter(variable == "mine_production")
  if (nrow(production) == 0) {
    stop("uranium_production has no mine_production rows.")
  }
  production
}

# Latest production year, or the latest not after `year`: a theme scored for an earlier
# vintage (scripts/40) must not read output from later years.
uranium_production_latest_year <- function(uranium_production, year = NULL) {
  years <- uranium_production_rows(uranium_production)$year
  if (!is.null(year)) {
    years <- years[years <= year]
  }
  if (length(years) == 0) {
    stop("uranium_production has no mine_production rows up to ", year, ".")
  }
  max(years)
}

# "Nuclear Production": latest-year uranium mine production, for the Energy Security
# production theme.
#
# Red Book Table 1.18 lists every country that has ever mined uranium and its rows sum to the
# world total, so an economy it does not list produced none: an observed zero, completed as 0
# the way critical_minerals_production_build_table() completes the EI mineral sheets. Unlike
# those sheets it has no "Rest of World" residual, so pass every economy the index scores as
# `country_reference` (scripts/10 passes the EI roster plus the WDI economies); an economy
# left out would be zero-filled by the missing-data rule and counted as imputed instead.
uranium_production_table <- function(uranium_production,
                                     country_reference,
                                     year = NULL,
                                     gamma = 0.5) {
  production <- uranium_production_rows(uranium_production)
  year <- as.integer(year %||% max(production$year))

  latest <- production %>% dplyr::filter(year == !!year)
  if (nrow(latest) == 0) {
    stop("uranium_production has no mine_production rows for ", year, ".")
  }

  roster <- uranium_country_roster(country_reference, latest$iso3c)

  latest %>%
    dplyr::transmute(
      ISO3166_alpha3 = iso3c,
      raw_value = value,
      index_value = uranium_anchor_index(value, gamma = gamma)
    ) %>%
    tidyr::complete(
      ISO3166_alpha3 = roster$ISO3166_alpha3,
      fill = list(raw_value = 0, index_value = 0)
    ) %>%
    dplyr::left_join(roster, by = "ISO3166_alpha3") %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = "Nuclear",
      supply_chain = "Upstream",
      category = "Production",
      variable = "Nuclear Production",
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = year,
      source = uranium_source_label(latest$source),
      explanation = dplyr::if_else(
        data_type == "raw",
        paste0("Uranium mine production in ", year, " (", paste(unique(latest$unit), collapse = "/"), "), Red Book Table 1.18"),
        "Percent-rank of production across reporting countries (plus a zero anchor)"
      )
    ) %>%
    dplyr::select(
      Country, tech, supply_chain, category, variable,
      data_type, value, Year, source, explanation
    )
}

# Append uranium rows to a theme table. The IEA demand map can route minerals to a "Nuclear"
# technology (reserves_build_mineral_demand_clean() recognises the label), in which case the
# mineral rollups would emit their own "Nuclear Reserves" / "Nuclear Production" and the two
# would be averaged as one variable without anyone deciding to; stop instead.
uranium_bind_theme_rows <- function(theme_tbl, uranium_tbl) {
  theme_tbl <- dplyr::ungroup(theme_tbl)
  clash <- intersect(
    unique(paste(theme_tbl$tech, theme_tbl$supply_chain, theme_tbl$variable)),
    unique(paste(uranium_tbl$tech, uranium_tbl$supply_chain, uranium_tbl$variable))
  )
  if (length(clash) > 0) {
    stop(
      "The theme already carries ", paste(clash, collapse = ", "),
      " from another input; decide which source scores it before adding the Red Book uranium rows."
    )
  }

  dplyr::bind_rows(
    standardize_bind_rows_inputs(theme_tbl),
    standardize_bind_rows_inputs(dplyr::ungroup(uranium_tbl))
  )
}

# Uranium mine production in the shape production_depth_momentum_clean_energy_production()
# returns: one "Nuclear Upstream" row per country with the base and latest levels and the
# 5- and 10-year changes, so production_depth_momentum_build_energy_indices() scores it the
# way it scores oil, gas and coal.
#
# `countries` is every economy to score (production_depth_momentum() passes the EI fuel
# roster plus country_info); Red Book producers outside it are added and every economy
# without a listed value is an observed zero, as in uranium_production_table(). The
# change over ten years is NA when the table does not reach back that far; the indices use
# the 5-year change only.
uranium_production_clean <- function(uranium_production,
                                     countries,
                                     latest_year = NULL,
                                     span = 5L) {
  production <- uranium_production_rows(uranium_production)
  latest_year <- as.integer(latest_year %||% max(production$year))
  base_year <- latest_year - as.integer(span)
  base10_year <- latest_year - 10L

  missing_years <- setdiff(c(base_year, latest_year), production$year)
  if (length(missing_years) > 0) {
    stop(
      "uranium_production has no mine_production rows for ",
      paste(missing_years, collapse = ", "),
      "; the production momentum needs the latest year and the year ", span, " years before."
    )
  }

  roster <- uranium_country_roster(
    tibble::tibble(Country = unique(as.character(countries))),
    production$iso3c
  )

  level <- function(yr) {
    production %>%
      dplyr::filter(year == yr) %>%
      dplyr::select(ISO3166_alpha3 = iso3c, value)
  }

  out <- roster %>%
    dplyr::left_join(level(latest_year) %>% dplyr::rename(latest = value), by = "ISO3166_alpha3") %>%
    dplyr::left_join(level(base_year) %>% dplyr::rename(base5 = value), by = "ISO3166_alpha3") %>%
    dplyr::mutate(
      latest = dplyr::coalesce(latest, 0),
      base5 = dplyr::coalesce(base5, 0)
    )

  if (base10_year %in% production$year) {
    out <- out %>%
      dplyr::left_join(level(base10_year) %>% dplyr::rename(base10 = value), by = "ISO3166_alpha3") %>%
      dplyr::mutate(base10 = dplyr::coalesce(base10, 0))
  } else {
    out <- out %>% dplyr::mutate(base10 = NA_real_)
  }

  out <- out %>%
    dplyr::transmute(
      Country,
      Var = "Nuclear Upstream",
      base5,
      latest,
      change_5yr = (latest / base5 - 1) * 100,
      change_10yr = (latest / base10 - 1) * 100,
      change_5yr_abs = latest - base5
    )

  names(out)[names(out) == "base5"] <- as.character(base_year)
  names(out)[names(out) == "latest"] <- as.character(latest_year)
  out
}
