# Critical minerals production theme builder functions.

# === Critical minerals production (EI data) ===
# These builders translate the EI Statistical Review mineral production tables
# into the canonical schema with demand-weighted rollups.

# ---- Production sheet specifications ----
# Each spec says where a mineral's production series lives in the EI Excel workbook so the
# IO layer can load the correct sheet.
#
# Sheets are addressed by name, not by position: the EI workbook adds and drops sheets
# between releases (the 2026 edition inserted "Data Centre Demand" and "SAF prices" ahead of
# the mineral block, shifting every mineral sheet four places), so positional indices
# silently read the wrong sheet. The value column is resolved from the sheet for the same
# reason — see critical_minerals_production_resolve_val_col().
#
# These are the same "P-R" (production and reserves) sheets reserves_specs() reads; the two
# take different columns off them, so a rename upstream needs fixing in both places.
critical_minerals_production_specs <- function() {
  list(
    list(sheet = "Cobalt P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Cobalt", unit_desc = "Thousand tonnes"),
    list(sheet = "Lithium P-R", skip = 2, nm_col = "Thousand tonnes of Lithium content", tech_name = "Lithium", unit_desc = "Thousand tonnes"),
    list(sheet = "Natural Graphite P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Graphite", unit_desc = "Thousand tonnes"),
    list(sheet = "Rare Earth metals P-R", skip = 2, nm_col = "Thousand tonnes1", tech_name = "Rare Earths", unit_desc = "Thousand tonnes"),
    list(sheet = "Copper P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Copper", unit_desc = "Thousand tonnes"),
    list(sheet = "Manganese P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Manganese", unit_desc = "Thousand tonnes"),
    list(sheet = "Nickel P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Nickel", unit_desc = "Thousand tonnes"),
    list(sheet = "Zinc P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Zinc", unit_desc = "Thousand tonnes"),
    list(
      # Renamed from "Platinum Group Metals P-R" and restated in kilograms in the 2026
      # edition. Units differ across minerals, which is harmless: each mineral's series is
      # percent-ranked within itself before the techs are rolled up.
      sheet = "PGM", skip = 2, nm_col = "Kilogram", tech_name = "PGMs", unit_desc = "Kilograms"
    )
  )
}

# ---- Resolve the production column on a P-R sheet ----
# Year columns on a mineral P-R sheet, as sheet name / year / position.
#
# The latest year appears three times — production level, growth rate, share — so readxl
# disambiguates the duplicates by column position ("2025...12", "2025...13", "2025...15").
# The leftmost column for a year is the production level; the others are derived from it.
critical_minerals_production_year_columns <- function(sheet_data) {
  matched <- stringr::str_match(names(sheet_data), "^((?:19|20)\\d{2})(?:\\.\\.\\.\\d+)?$")
  keep <- which(!is.na(matched[, 1]))

  tibble::tibble(
    column = names(sheet_data)[keep],
    year = as.integer(matched[keep, 2]),
    position = keep
  )
}

# Production column for `year`, falling back to the sheet's own latest year.
#
# Pinning the readxl position suffix is what went stale before: it moves whenever the
# workbook gains a year or a column, so the name is derived from the sheet every run.
critical_minerals_production_resolve_val_col <- function(sheet_data, year = NULL, sheet_id = NULL) {
  year_cols <- critical_minerals_production_year_columns(sheet_data)
  if (nrow(year_cols) == 0) {
    stop(
      "Production sheet ", sheet_id %||% "<unnamed>",
      " has no year columns; the sheet layout or `skip` has changed."
    )
  }

  target <- if (is.null(year)) max(year_cols$year) else as.integer(year)
  hits <- year_cols[year_cols$year == target, ]

  if (nrow(hits) == 0) {
    target <- max(year_cols$year)
    hits <- year_cols[year_cols$year == target, ]
    message(
      "Production sheet ", sheet_id %||% "<unnamed>", ": no ", year,
      " column; using ", target, "."
    )
  }

  hits$column[which.min(hits$position)]
}

# Latest production year published across the mineral sheets, so every mineral in the
# theme is stamped with one year rather than drifting apart sheet by sheet.
critical_minerals_production_latest_year <- function(production_inputs) {
  years <- vapply(production_inputs, function(spec) {
    year_cols <- critical_minerals_production_year_columns(spec$data)
    if (nrow(year_cols) == 0) NA_integer_ else max(year_cols$year)
  }, integer(1))

  years <- years[!is.na(years)]
  if (length(years) == 0) {
    stop("No year columns found on any EI mineral production sheet.")
  }
  max(years)
}

# ---- Build a single mineral production table ----
# Convert a raw EI sheet into raw + index values for one mineral.
critical_minerals_production_build_table <- function(sheet_data,
                                                     nm_col,
                                                     tech_name,
                                                     unit_desc,
                                                     sheet_id,
                                                     country_reference,
                                                     year = NULL,
                                                     val_col = NULL,
                                                     gamma = 0.5) {
  country_names <- country_reference$Country

  val_col <- val_col %||% critical_minerals_production_resolve_val_col(
    sheet_data,
    year = year,
    sheet_id = sheet_id
  )
  # A sheet that lags the rest of the workbook resolves to its own latest year. The theme
  # keeps one `Year` so the demand-weighted rollup does not fragment, and the raw row's
  # explanation records the year the value actually came from.
  value_year <- as.integer(stringr::str_extract(val_col, "^(19|20)\\d{2}"))
  year <- year %||% value_year

  raw <- sheet_data %>%
    dplyr::rename(
      Country = dplyr::all_of(nm_col),
      raw_value = dplyr::all_of(val_col)
    ) %>%
    dplyr::mutate(
      Country = standardize_country_names(Country),
      Country = dplyr::case_when(
        Country %in% c("Rest of World", "Rest of world", "Rest of World^") ~ "Rest of World",
        Country == "US" ~ "United States",
        Country == "DR Congo" ~ "Democratic Republic of Congo",
        Country == "Russia Federation" ~ "Russia",
        TRUE ~ Country
      )
    ) %>%
    dplyr::filter(
      Country %in% country_names,
      !is.na(Country),
      !grepl("Total World|Other|OECD|OPEC|Orinoco", Country)
    ) %>%
    dplyr::mutate(raw_value = as.numeric(raw_value) %>% tidyr::replace_na(0))

  dummy_zero <- tibble::tibble(Country = "_ZERO_", raw_value = 0)

  dplyr::bind_rows(
    lapply(list(raw, dummy_zero), standardize_bind_rows_inputs)
  ) %>%
    dplyr::mutate(index_value = median_scurve(raw_value, gamma = gamma)) %>%
    dplyr::filter(Country != "_ZERO_") %>%
    tidyr::complete(
      Country = country_names,
      fill = list(raw_value = 0, index_value = 0)
    ) %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = tech_name,
      supply_chain = "Upstream",
      category = "Production",
      variable = stringr::str_glue("{tech_name} Production"),
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = year,
      source = stringr::str_glue("EI Statistical Review of World Energy ({year})"),
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue(
          "{tech_name} production in {value_year} ({unit_desc}) from sheet '{sheet_id}'"
        ),
        data_type == "index" ~ "Percent-rank of production across reporting entities (countries + RoW)"
      )
    ) %>%
    dplyr::select(
      Country,
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

# ---- Demand-weighted rollup ----
# Combine mineral-level indices into technology-level production scores.
critical_minerals_production_build_weighted <- function(critical_min_production,
                                                        mineral_demand_clean,
                                                        country_reference,
                                                        gamma = 0.5) {
  weighted_inputs <- critical_min_production %>%
    dplyr::rename(Mineral = tech) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::ungroup() %>%
        dplyr::select(Mineral, tech, share_24) %>%
        dplyr::mutate(
          Mineral = dplyr::case_when(
            stringr::str_detect(Mineral, stringr::regex("rare", ignore_case = TRUE)) ~ "Rareearths",
            Mineral == "Battery-grade graphite" ~ "Graphite",
            TRUE ~ Mineral
          )
        ),
      by = "Mineral"
    ) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::group_by(Country, tech, data_type) %>%
    dplyr::mutate(share_24 = share_24 / sum(share_24)) %>%
    dplyr::ungroup()

  critical_min_prod <- weighted_inputs %>%
    dplyr::filter(data_type == "index") %>%
    dplyr::group_by(Country, tech, supply_chain, category, data_type, source, explanation, Year) %>%
    dplyr::summarize(value = stats::weighted.mean(value, w = share_24, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(variable = stringr::str_glue("{tech} Production")) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(tech) %>%
    dplyr::mutate(value = median_scurve(value, gamma = gamma)) %>%
    {
      dplyr::bind_rows(
        standardize_bind_rows_inputs(.),
        standardize_bind_rows_inputs(
          weighted_inputs %>%
            dplyr::select(-tech) %>%
            dplyr::rename(tech = Mineral) %>%
            dplyr::distinct(
              Country,
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
    tidyr::complete(
      Country = country_reference$Country,
      fill = list(value = 0)
    )

  critical_min_prod
}

# === Public theme entrypoint ===
# `year` defaults to the newest production year the workbook publishes, so a new EI release
# flows through without a code edit. A sheet that lags behind falls back to its own latest
# year and says so.
critical_minerals_production <- function(production_inputs,
                                         mineral_demand_clean,
                                         country_reference,
                                         year = critical_minerals_production_latest_year(production_inputs),
                                         gamma = 0.5) {
  production_tables <- lapply(production_inputs, function(spec) {
    critical_minerals_production_build_table(
      sheet_data = spec$data,
      nm_col = spec$nm_col,
      tech_name = spec$tech_name,
      unit_desc = spec$unit_desc,
      sheet_id = spec$sheet,
      country_reference = country_reference,
      year = year,
      gamma = gamma
    )
  })

  critical_min_production <- dplyr::bind_rows(
    lapply(production_tables, standardize_bind_rows_inputs)
  )

  critical_minerals_production_build_weighted(
    critical_min_production = critical_min_production,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference,
    gamma = gamma
  ) %>%
    energy_security_add_overall_index()
}
