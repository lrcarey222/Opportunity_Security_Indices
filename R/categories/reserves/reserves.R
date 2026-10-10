# Reserves theme builder functions.
#
# Sources (user decision, 2026-10-09): oil, gas and coal from the EI Statistical Review sheets
# of reserves_specs(); the nine critical minerals of RESERVES_MINERALS from USGS Mineral
# Commodity Summaries (usgs_mcs.R: reserves_usgs_specs(), usgs_mcs_reserves()); uranium from
# the Red Book (uranium.R). EI's mineral sheets copy older USGS data and fold smaller holders
# into "Rest of World", so the theme reads USGS directly; reserves() still builds a mineral
# from an EI sheet when one is passed in `reserve_inputs` (the layout the tests and older
# workbooks use), but never the same mineral from both.
#
# Sheets are addressed by name rather than by position: the EI workbook adds and drops
# sheets between releases (the 2026 edition inserted "Data Centre Demand" and "SAF prices"
# ahead of the mineral block), so positional indices silently read the wrong sheet.
#
# `data_year` records the vintage of the values where the column name does not carry it. EI
# stopped updating fossil proved reserves after end-2020, so oil, gas and coal are end-2020
# even in the 2026 edition; the explanation text says so rather than implying the theme year.

# The minerals whose reserves are rolled up to technologies through their demand shares
# (reserves_build_critical_mineral_reserves()); every other reserve table is a fuel's own.
RESERVES_MINERALS <- c(
  "Cobalt", "Lithium", "Graphite", "Rare Earths", "Copper", "Manganese", "Nickel", "Zinc", "PGMs"
)

# The EI sheets the theme reads: the fuels' proved reserves. The mineral "P-R" sheets left this
# list for USGS MCS (above); critical_minerals_production_specs() still reads their production
# columns.
reserves_specs <- function() {
  list(
    list(
      sheet = "Oil - Proved reserves history",
      skip = 4,
      nm_col = "Thousand million barrels",
      val_col = "2020...42",
      tech_name = "Oil",
      unit_desc = "Thousand million barrels",
      data_year = 2020
    ),
    list(
      sheet = "Gas - Proved reserves history",
      skip = 4,
      nm_col = "Trillion cubic metres",
      val_col = "2020...42",
      tech_name = "Gas",
      unit_desc = "Trillion cubic metres",
      data_year = 2020
    ),
    list(
      sheet = "Coal - Reserves",
      skip = 5,
      nm_col = "Million tonnes",
      val_col = "Total",
      tech_name = "Coal",
      unit_desc = "Million tonnes",
      data_year = 2020
    )
  )
}

# Resolve the reserves value column on a mineral P-R sheet.
#
# The EI mineral sheets (when passed in the older layout) label reserves "At end of <year>",
# so a pinned column name goes stale every release. When the pinned column is absent, fall back to
# the latest "At end of <year>" column actually present and say so, rather than erroring on
# a column name that only differs by vintage.
reserves_resolve_val_col <- function(sheet_data, val_col, sheet_id = NULL) {
  if (val_col %in% names(sheet_data)) {
    return(val_col)
  }

  candidates <- grep("^At end of \\d{4}$", names(sheet_data), value = TRUE)
  if (length(candidates) == 0) {
    return(val_col)
  }

  resolved <- candidates[which.max(as.integer(sub("^At end of ", "", candidates)))]
  message(
    "Reserves sheet ", sheet_id %||% "<unnamed>", ": '", val_col,
    "' not found; using '", resolved, "'."
  )
  resolved
}

reserves_build_country_reference <- function(ei, year = 2025) {
  ei %>%
    dplyr::filter(
      Year == year,
      # "USSR" is EI's CIS aggregate, still published for the latest year; left in, it is
      # ranked as a country and later collapses onto Russia's ISO3.
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)
}

reserves_country_names <- function(country_reference) {
  country_reference$Country
}

# Demand-by-technology shares from the IEA critical minerals dataset.
#
# The pillar label carries the release's own wording ("3.1 Cleantech demand by tech" in the
# 2025 vintage, "3.1 Energy demand by tech" in 2026), so only the section number is matched.
# Likewise the base year moves with each release, so the base and 2035 columns are resolved
# from the data. `share_24`/`share_35` keep their names — they are the base-year and 2035
# shares, and every downstream weighting step joins on them.
reserves_build_mineral_demand_clean <- function(critical,
                                               base_year = iea_critical_minerals_base_year(critical),
                                               horizon_year = 2035) {
  base_col <- iea_critical_minerals_year_col(critical, base_year, label = "mineral demand")
  horizon_col <- iea_critical_minerals_year_col(critical, horizon_year, label = "mineral demand")

  mineral_demand <- critical %>%
    dplyr::filter(
      grepl("^3\\.1", Pillar),
      !grepl("Other|Total", `Sector.Country`)
    ) %>%
    dplyr::mutate(
      demand_base = as.numeric(.data[[base_col]]),
      demand_horizon = as.numeric(.data[[horizon_col]]),
      growth = demand_horizon / demand_base - 1
    ) %>%
    dplyr::group_by(Pillar, Mineral) %>%
    dplyr::mutate(
      share_24 = demand_base / sum(demand_base),
      share_35 = demand_horizon / sum(demand_horizon)
    ) %>%
    dplyr::select(
      Pillar, Mineral, `Sector.Country`,
      demand_base, demand_horizon, growth, share_24, share_35
    ) %>%
    dplyr::ungroup()

  mineral_demand %>%
    dplyr::mutate(
      tech = dplyr::case_when(
        stringr::str_detect(`Sector.Country`, stringr::regex("^Solar", ignore_case = TRUE)) ~ "Solar",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Wind", ignore_case = TRUE)) ~ "Wind",
        stringr::str_detect(`Sector.Country`, stringr::regex("Electric vehicles?", ignore_case = TRUE)) ~ "Electric Vehicles",
        stringr::str_detect(`Sector.Country`, stringr::regex("Grid battery storage", ignore_case = TRUE)) ~ "Batteries",
        stringr::str_detect(`Sector.Country`, stringr::regex("Electricity networks?", ignore_case = TRUE)) ~ "Electric Grid",
        stringr::str_detect(`Sector.Country`, stringr::regex("Hydrogen", ignore_case = TRUE)) ~ "Green Hydrogen",
        stringr::str_detect(`Sector.Country`, stringr::regex("Heat pumps?", ignore_case = TRUE)) ~ "Heat Pumps",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Coal$", ignore_case = TRUE)) ~ "Coal",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Nuclear$", ignore_case = TRUE)) ~ "Nuclear",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Oil$", ignore_case = TRUE)) ~ "Oil",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Gas$", ignore_case = TRUE)) ~ "Gas",
        stringr::str_detect(`Sector.Country`, stringr::regex("Hydroelectric", ignore_case = TRUE)) ~ "Hydroelectric Power",
        stringr::str_detect(`Sector.Country`, stringr::regex("Geothermal", ignore_case = TRUE)) ~ "Geothermal",
        TRUE ~ NA_character_
      ),
      # Shares are computed on the IEA's own labels above; the canonical name is what every
      # downstream join keys on ("Battery-grade graphite" -> "Graphite", "Neodymium" ->
      # "Rare Earths").
      Mineral = harmonize_mineral_names(Mineral)
    )
}

reserves_build_reserve_table <- function(sheet_data,
                                         nm_col,
                                         val_col,
                                         tech_name,
                                         unit_desc,
                                         sheet_id,
                                         country_reference,
                                         year = 2025,
                                         gamma = 0.5,
                                         data_year = NULL) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  val_col <- reserves_resolve_val_col(sheet_data, val_col, sheet_id = sheet_id)
  # "At end of 2025" / "2020...42" carry their own vintage; the coal "Total" column does not.
  data_year <- data_year %||% suppressWarnings(
    as.integer(stringr::str_extract(val_col, "(19|20)\\d{2}"))
  )
  vintage_note <- if (is.na(data_year)) "" else paste0(" at end-", data_year)

  raw_inputs <- sheet_data %>%
    dplyr::rename(
      Country = dplyr::all_of(nm_col),
      raw_value = dplyr::all_of(val_col)
    ) %>%
    # Sub-rows and footnotes go before the names are standardized: name matching reads
    # "Venezuela: Orinoco Belt" (a component of Venezuela's total) and the oil-sheet footnote
    # about "Russian reserves" as those countries, which gave both a second, conflicting row.
    # Rows without a value carry nothing; a country missing here is completed as NA below.
    dplyr::filter(
      !grepl("Orinoco|of which", Country, ignore.case = TRUE),
      !is.na(suppressWarnings(as.numeric(raw_value)))
    ) %>%
    dplyr::mutate(
      Country = standardize_country_names(Country),
      Country = dplyr::case_when(
        Country %in% c("Rest of World", "Rest of world", "Rest of World^") ~ "Rest of World",
        Country == "US" ~ "United States",
        Country == "DR Congo" ~ "Democratic Republic of Congo",
        Country == "Russia Federation" ~ "Russia",
        TRUE ~ Country
      ),
      raw_value = as.numeric(raw_value)
    ) %>%
    dplyr::filter(!is.na(Country))

  unmapped <- raw_inputs %>%
    dplyr::filter(
      !grepl("Total World|Other|OECD|OPEC|Orinoco", Country),
      Country != "Rest of World"
    ) %>%
    dplyr::distinct(Country) %>%
    dplyr::anti_join(country_reference, by = "Country")

  if (nrow(unmapped) > 0) {
    warning(
      "Reserves sheet ",
      sheet_id,
      " contains countries without ISO3 mappings after standardization: ",
      paste(sort(unmapped$Country), collapse = ", ")
    )
  }

  raw <- raw_inputs %>%
    dplyr::filter(
      !grepl("Total World|Other|OECD|OPEC|Orinoco", Country),
      Country != "Rest of World"
    ) %>%
    dplyr::inner_join(country_reference, by = "Country") %>%
    dplyr::select(ISO3166_alpha3, raw_value)

  dummy_zero <- tibble::tibble(ISO3166_alpha3 = "_ZERO_", raw_value = 0)

  dplyr::bind_rows(
    lapply(list(raw, dummy_zero), standardize_bind_rows_inputs)
  ) %>%
    dplyr::mutate(index_value = median_scurve(raw_value, gamma = gamma)) %>%
    dplyr::filter(ISO3166_alpha3 != "_ZERO_") %>%
    tidyr::complete(
      ISO3166_alpha3 = country_reference$ISO3166_alpha3
    ) %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3") %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = tech_name,
      supply_chain = "Upstream",
      category = "Reserves",
      variable = stringr::str_glue("{tech_name} Reserves"),
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = year,
      source = "EI Statistical Review of World Energy (2025)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue("{tech_name} reserves ({unit_desc}){vintage_note} from sheet {sheet_id}"),
        data_type == "index" ~ "Percent-rank of reserves across reporting countries (plus a zero anchor)",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      Country,
      ISO3166_alpha3,
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
}

# `country_reference` plus every economy in `iso3` it lacks, named by country_name_from_iso3().
# USGS lists holders the EI roster omits (MCS 2026: Ghana's manganese, Mali's lithium,
# Greenland's rare earths, North Korea's graphite); an inner join on the EI roster, as
# reserves_build_reserve_table() does, would score them as holding nothing.
reserves_extend_roster <- function(country_reference, iso3) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)
  extra <- setdiff(unique(normalize_iso3c(iso3)), country_reference$ISO3166_alpha3)
  extra <- extra[!is.na(extra)]
  dplyr::bind_rows(
    country_reference,
    tibble::tibble(ISO3166_alpha3 = extra, Country = country_name_from_iso3(extra))
  )
}

# One mineral's theme rows from usgs_mcs_reserves(): the variable, columns and zero-anchored
# S-curve reserves_build_reserve_table() builds from an EI sheet, completed over
# `country_reference` (reserves_extend_roster()). Only country rows are holders; "Other
# countries" and "World total" are not ranked.
#
# A reported zero ("—": Germany, Japan and the Republic of Korea refine copper but hold no
# copper reserves) scores 0, the anchor's value: ranked with the anchor it would share its
# averaged rank and score above an unlisted economy. Unquantified holders ("NA", "W", "s"; see
# usgs_mcs_parse_value()) stay NA, so the reserves missing-data rule scores them like an
# unlisted economy and counts them as imputed rather than as observed zeros.
reserves_build_usgs_table <- function(usgs_rows,
                                      country_reference,
                                      year = 2025,
                                      gamma = 0.5) {
  tech_name <- unique(usgs_rows$tech)
  if (length(tech_name) != 1) {
    stop("reserves_build_usgs_table() takes the rows of one mineral; got ", paste(tech_name, collapse = ", "))
  }
  describe <- function(x) paste(unique(x), collapse = "/")
  holders <- usgs_rows[usgs_rows$row_type == "country", , drop = FALSE]

  index <- rep(NA_real_, nrow(holders))
  index[!is.na(holders$value) & holders$value <= 0] <- 0
  positive <- !is.na(holders$value) & holders$value > 0
  if (any(positive)) {
    index[positive] <- median_scurve(c(0, holders$value[positive]), gamma = gamma)[-1]
  }

  source_label <- paste0("USGS Mineral Commodity Summaries ", describe(usgs_rows$edition))
  raw_explanation <- paste0(
    tech_name, " reserves (", describe(usgs_rows$unit), ", ", describe(usgs_rows$basis), ") at end-",
    describe(usgs_rows$data_year), ", ", source_label, " (World Mine Production and Reserves)"
  )

  tibble::tibble(
    ISO3166_alpha3 = holders$ISO3166_alpha3,
    raw_value = holders$value,
    index_value = index
  ) %>%
    tidyr::complete(ISO3166_alpha3 = country_reference$ISO3166_alpha3) %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3") %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = tech_name,
      supply_chain = "Upstream",
      category = "Reserves",
      variable = paste(tech_name, "Reserves"),
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = year,
      source = source_label,
      explanation = dplyr::if_else(
        data_type == "raw",
        raw_explanation,
        "Percent-rank of reserves across reporting countries (plus a zero anchor)"
      )
    ) %>%
    dplyr::select(
      Country,
      ISO3166_alpha3,
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
}

reserves_build_critical_mineral_reserves <- function(mineral_reserves,
                                                     mineral_demand_clean,
                                                     country_reference,
                                                     year = 2025) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  critical_min_reserves <- dplyr::bind_rows(
    lapply(mineral_reserves, standardize_bind_rows_inputs)
  ) %>%
    dplyr::rename(Mineral = "tech") %>%
    dplyr::select(-Country) %>%
    # Both sides on the canonical names: the old ad hoc recode never matched "Rare Earths"
    # (sheet) to "Neodymium" (IEA demand), so rare earths never reached Wind or EV reserves.
    dplyr::mutate(join_mineral = harmonize_mineral_names(Mineral)) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::ungroup() %>%
        dplyr::transmute(join_mineral = harmonize_mineral_names(Mineral), tech, share_24),
      by = "join_mineral",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-join_mineral) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::group_by(ISO3166_alpha3, tech, data_type) %>%
    dplyr::mutate(share_24 = share_24 / sum(share_24)) %>%
    dplyr::ungroup()

  # A mineral a country is not listed for counts as 0 (the bottom of that mineral's index,
  # as in the production rollup) rather than as missing: with na.rm the mean ran over the
  # listed minerals only, so a country on one sheet scored that mineral's rank for the whole
  # technology (Chile topped Wind Reserves on copper alone).
  critical_min_reserves %>%
    dplyr::filter(data_type == "index") %>%
    dplyr::group_by(ISO3166_alpha3, tech, supply_chain, category, data_type, source) %>%
    dplyr::summarize(
      value = stats::weighted.mean(dplyr::coalesce(value, 0), w = share_24, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      variable = stringr::str_glue("{tech} Reserves"),
      explanation = "Weighted average of reserve availability by share of demand in technology",
      Year = year
    ) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(tech, supply_chain) %>%
    # Same zero anchor as the per-mineral step: without it the smallest reporter re-scores to
    # exactly 0, i.e. the same as a country with no reserves at all.
    dplyr::mutate(value = median_scurve(c(0, value))[-1]) %>%
    {
      dplyr::bind_rows(
        standardize_bind_rows_inputs(.),
        standardize_bind_rows_inputs(
          critical_min_reserves %>%
            dplyr::select(-tech) %>%
            dplyr::rename(tech = "Mineral") %>%
            dplyr::distinct(
              ISO3166_alpha3,
              tech,
              supply_chain,
              category,
              data_type,
              variable,
              value,
              Year,
              source,
              explanation
            )
        )
      )
    } %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(ISO3166_alpha3 = country_reference$ISO3166_alpha3) %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3")
}

reserves_build_clean_table <- function(reserve_tables, country_reference) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  dplyr::bind_rows(
    lapply(reserve_tables, standardize_bind_rows_inputs)
  ) %>%
    dplyr::select(-Country) %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(ISO3166_alpha3 = country_reference$ISO3166_alpha3) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3") %>%
    dplyr::select(Country, ISO3166_alpha3, dplyr::everything())
}

# `reserve_inputs`: EI sheets (reserves_specs() with each sheet's `data`), fuels and, in the
# older layout, minerals. `usgs_reserves` (usgs_mcs_reserves()) supplies minerals from USGS;
# a mineral both supply stops (decide which source scores it). The mineral tables and their
# technology rollups are completed over the EI roster plus every economy USGS lists
# (reserves_extend_roster()), and so are the fuel tables, which only gain NA rows.
#
# `uranium_resources` (data/reference/uranium_resources.csv, see R/categories/reserves/uranium.R)
# adds "Nuclear Reserves". It is bound after the clean table rather than completed with it:
# the Red Book lists producers the EI roster omits (Namibia, Niger), and completing every
# other reserve variable over them would change nothing but the row count.
reserves <- function(ei,
                     reserve_inputs,
                     mineral_demand_clean,
                     year = 2025,
                     gamma = 0.5,
                     uranium_resources = NULL,
                     uranium_cost_category = URANIUM_DEFAULT_COST_CATEGORY,
                     usgs_reserves = NULL) {
  country_reference <- reserves_build_country_reference(ei, year = year)
  roster <- country_reference

  usgs_tables <- list()
  if (!is.null(usgs_reserves)) {
    ei_minerals <- intersect(
      vapply(reserve_inputs, function(spec) as.character(spec$tech_name), character(1)),
      unique(usgs_reserves$tech)
    )
    if (length(ei_minerals) > 0) {
      stop(
        "Reserves of ", paste(ei_minerals, collapse = ", "),
        " come from both an EI sheet and USGS; pass each mineral from one source."
      )
    }
    usgs_reserves <- tibble::as_tibble(usgs_reserves)
    holders <- usgs_reserves$row_type == "country"
    roster <- reserves_extend_roster(country_reference, usgs_reserves$ISO3166_alpha3[holders])
    usgs_tables <- lapply(
      split(usgs_reserves, usgs_reserves$tech),
      reserves_build_usgs_table,
      country_reference = roster,
      year = year,
      gamma = gamma
    )
    names(usgs_tables) <- NULL
  }

  reserve_tables <- lapply(reserve_inputs, function(spec) {
    reserves_build_reserve_table(
      sheet_data = spec$data,
      nm_col = spec$nm_col,
      val_col = spec$val_col,
      tech_name = spec$tech_name,
      unit_desc = spec$unit_desc,
      sheet_id = spec$sheet,
      country_reference = country_reference,
      year = year,
      gamma = gamma,
      data_year = spec$data_year
    )
  })

  mineral_reserves <- lapply(
    reserve_tables,
    function(tbl) if (tbl$tech[[1]] %in% RESERVES_MINERALS) tbl else NULL
  )
  mineral_reserves <- c(Filter(Negate(is.null), mineral_reserves), usgs_tables)
  if (length(mineral_reserves) == 0) {
    stop("reserves() has no mineral reserves: pass usgs_reserves (usgs_mcs_reserves()) or EI mineral sheets.")
  }

  energy_reserves <- lapply(
    reserve_tables,
    function(tbl) if (tbl$tech[[1]] %in% c("Oil", "Gas", "Coal")) tbl else NULL
  )
  energy_reserves <- Filter(Negate(is.null), energy_reserves)

  critical_min_reserves <- reserves_build_critical_mineral_reserves(
    mineral_reserves = mineral_reserves,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = roster,
    year = year
  )

  clean_table <- reserves_build_clean_table(
    reserve_tables = c(list(critical_min_reserves), energy_reserves),
    country_reference = roster
  )

  if (!is.null(uranium_resources)) {
    clean_table <- uranium_bind_theme_rows(
      clean_table,
      uranium_reserves_table(
        uranium_resources,
        country_reference = country_reference,
        cost_category = uranium_cost_category,
        year = year,
        gamma = gamma
      )
    )
  }

  clean_table %>%
    energy_security_add_overall_index()
}
