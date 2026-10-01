# Helper utilities for partnership strength themes.

partnership_strength_pick_column <- function(tbl, candidates, label) {
  match <- intersect(candidates, names(tbl))
  if (length(match) == 0) {
    stop("Missing ", label, " column.")
  }
  match[[1]]
}

partnership_strength_safe_scurve <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  median_scurve(x)
}

partnership_strength_min_max_index <- function(x) {
  finite_x <- x[is.finite(x)]
  if (!length(finite_x)) {
    return(rep(NA_real_, length(x)))
  }
  min_x <- min(finite_x, na.rm = TRUE)
  max_x <- max(finite_x, na.rm = TRUE)
  if (isTRUE(all.equal(min_x, max_x))) {
    return(rep(0, length(x)))
  }
  x2 <- x
  x2[x == Inf] <- max_x
  x2[x == -Inf] <- min_x
  (x2 - min_x) / (max_x - min_x)
}

partnership_strength_standardize_bind_rows <- function(tbl) {
  standardize_bind_rows_inputs(tbl)
}

partnership_strength_weighted_mean <- function(values, weights) {
  if (length(values) != length(weights)) {
    stop("Weights must align with values for weighted mean.")
  }
  if (length(values) == 0) {
    return(NA_real_)
  }
  valid <- !is.na(values)
  if (!any(valid)) {
    return(NA_real_)
  }
  sum(values[valid] * weights[valid]) / sum(weights[valid])
}

# Row-wise version of partnership_strength_weighted_mean() over a list of equal-length
# component vectors: each row averages the components it has, with the remaining weights
# renormalized (a missing component is dropped, not scored 0). NA when no weighted
# component is present. Vectorized because the dyad tables run to ~1.5M rows, where
# rowwise() took minutes.
partnership_strength_row_weighted_mean <- function(values, weights) {
  if (length(values) != length(weights)) {
    stop("Weights must align with values for weighted mean.")
  }
  num <- 0
  den <- 0
  for (k in seq_along(values)) {
    x <- suppressWarnings(as.numeric(values[[k]]))
    ok <- !is.na(x)
    num <- num + ifelse(ok, x * weights[[k]], 0)
    den <- den + ok * weights[[k]]
  }
  ifelse(den > 0, num / den, NA_real_)
}

# Mean of the k largest non-missing values; NA when there are none.
partnership_strength_top_k_mean <- function(x, k = 5L) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  mean(sort(x, decreasing = TRUE)[seq_len(min(k, length(x)))])
}

# The 11 headline technologies the ES, EO and Policy pillars report (the `techs` vector in
# scripts/20_build_indices.R). The partner themes rank over the same set, so within-reporter
# ranks and the top-k summaries are not driven by the sparse extra rows the all-tech ES/EO
# tables carry (minerals, Aluminium, Steel, Heat pumps, NA-tech rows ...). A test checks the
# two lists agree.
partnership_strength_headline_techs <- function() {
  c(
    "Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind", "Oil",
    "Solar", "Gas", "Geothermal", "Electric Grid"
  )
}

partnership_strength_filter_techs <- function(tbl, techs = NULL) {
  if (is.null(techs)) {
    return(tbl)
  }
  tbl %>%
    dplyr::filter(tech %in% techs, !is.na(supply_chain))
}

# Fail before writing if a partner table has more than one row per key. The Friendshore table
# once shipped ~2x duplicated (a grouped-tibble leak) and Opportunity with conflicting copies
# (country-name variants); both passed silently into the allied network.
partnership_strength_assert_unique <- function(tbl, keys, label = "partnership table") {
  require_columns(tbl, keys, label = label)
  if (!vctrs::vec_duplicate_any(tbl[keys])) {
    return(invisible(tbl))
  }
  dup_keys <- tbl[keys][vctrs::vec_duplicate_detect(tbl[keys]), , drop = FALSE]
  examples <- utils::head(unique(do.call(paste, c(dup_keys, sep = "|"))), 3)
  stop(
    "Duplicate rows in ", label, " for key (", paste(keys, collapse = ", "), "): ",
    nrow(dup_keys), " rows, e.g. ", paste(examples, collapse = "; ")
  )
}

# Best-k summary ("top5" tables): for each unit (dyad, or partner for development) and
# variable, the mean of the k largest values across its (tech, supply_chain) cells. Each
# variable picks its own k cells, so a component's summary need not come from the cells that
# lead the index. This is an optimistic best-sector summary, not an average relationship.
partnership_strength_build_top_k <- function(variable_tbl, id_cols, k = 5L, year) {
  variable_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "category", "variable")))) %>%
    dplyr::summarise(
      value = partnership_strength_top_k_mean(value, k),
      source = dplyr::first(source),
      explanation = dplyr::first(explanation),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      data_type = "index",
      Year = as.integer(year)
    )
}

# Partner-level roll-up of a dyadic index, in the standard Country schema: for each
# (partner, tech, stage), the mean of the dyad index over reporters (self-dyads are excluded
# upstream), with the number of reporters behind it. Written as partner_<theme>_country_tbl,
# which used to be a byte copy of the dyad table.
partnership_strength_build_partner_rollup <- function(variable_tbl,
                                                      index_variable,
                                                      year,
                                                      explanation) {
  variable_tbl %>%
    dplyr::filter(variable == index_variable) %>%
    dplyr::group_by(partner, partner_iso, tech, supply_chain, category, variable) %>%
    dplyr::summarise(
      n_reporters = sum(!is.na(value)),
      value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      Country = partner,
      partner,
      partner_iso,
      tech,
      supply_chain,
      category,
      variable,
      data_type = "index",
      value,
      n_reporters,
      Year = as.integer(year),
      source = "Author calculation",
      explanation = explanation
    )
}

partnership_strength_resolve_weights <- function(weights, defaults, label) {
  if (is.null(weights) || length(weights) == 0) {
    return(defaults)
  }

  # Allow a scalar numeric shorthand (e.g., w = 1) by broadcasting it to all
  # expected components.
  if (is.atomic(weights) && is.null(names(weights)) && length(weights) == 1) {
    return(stats::setNames(as.list(rep(as.numeric(weights), length(defaults))), names(defaults)))
  }

  # Allow unnamed numeric vectors with the same arity as defaults by assigning
  # names positionally.
  if (is.atomic(weights) && is.null(names(weights)) && length(weights) == length(defaults)) {
    weights <- stats::setNames(as.list(as.numeric(weights)), names(defaults))
  }

  # Accept numeric vectors and coerce to list for `$`/`[[` access downstream.
  if (is.atomic(weights) && !is.list(weights)) {
    weights <- as.list(weights)
  }

  if (is.null(names(weights)) || any(names(weights) == "")) {
    stop(label, " weights must be a named list or a scalar numeric value.")
  }

  expected <- names(defaults)
  missing <- setdiff(expected, names(weights))
  extra <- setdiff(names(weights), expected)
  if (length(missing) > 0) {
    stop(
      label,
      " weights missing: ",
      paste(missing, collapse = ", "),
      ". Expected names: ",
      paste(expected, collapse = ", ")
    )
  }
  if (length(extra) > 0) {
    stop(
      label,
      " weights include unexpected entries: ",
      paste(extra, collapse = ", "),
      ". Expected names: ",
      paste(expected, collapse = ", ")
    )
  }

  weights
}

partnership_strength_validate_schema <- function(tbl, label = "partnership theme") {
  validate_schema(tbl)

  invalid_types <- tbl %>%
    dplyr::filter(!data_type %in% c("raw", "index"))
  if (nrow(invalid_types) > 0) {
    stop("Invalid data_type values in ", label, "; expected raw or index only.")
  }

  if (!is.integer(tbl$Year) || any(is.na(tbl$Year))) {
    stop("Year must be integer and non-missing in ", label, ".")
  }

  invisible(tbl)
}

partnership_strength_standardize_countries <- function(country) {
  standardize_country_names(country)
}

partnership_strength_country_to_iso <- function(
    country,
    country_info,
    unresolved_action = c("ignore", "warn", "error")) {
  unresolved_action <- match.arg(unresolved_action)

  if (is.null(country_info) || nrow(country_info) == 0) {
    stop("country_info is required to map country names to ISO3 codes.")
  }

  cleaned <- partnership_strength_standardize_countries(country)
  lookup <- country_info %>%
    dplyr::mutate(country = partnership_strength_standardize_countries(country)) %>%
    dplyr::distinct(country, .keep_all = TRUE)

  # match() rather than a join: a name listed twice in the lookup must not add rows.
  mapped <- tibble::tibble(
    country = cleaned,
    iso3c = lookup$iso3c[match(cleaned, lookup$country)]
  )

  # Keep only ISO3 values present in country_info to ensure downstream joins
  # stay within the modeled country universe.
  valid_iso3c <- unique(country_info$iso3c)
  mapped$iso3c <- dplyr::if_else(mapped$iso3c %in% valid_iso3c, mapped$iso3c, NA_character_)

  aggregates <- c(
    "EU",
    "European Union",
    "World",
    "OECD",
    "OPEC",
    "Aggregates"
  )

  # Names outside country_info go through the shared resolver (R/utils/country.R), which returns
  # an ISO3 code for Taiwan and the territories WDI does not list, and NA for aggregates and
  # historical entities. The bare countrycode call used here before read "USSR" as Russia and
  # "Northern Cyprus" as Cyprus, so those rows were pooled with the present-day economy.
  update_idx <- which(is.na(mapped$iso3c) & !is.na(mapped$country) & !mapped$country %in% aggregates)
  if (length(update_idx) > 0) {
    mapped$iso3c[update_idx] <- country_name_to_iso3(mapped$country[update_idx])
  }

  unresolved <- mapped$country[is.na(mapped$iso3c) & !mapped$country %in% aggregates]
  if (length(unresolved) > 0) {
    sample_vals <- paste(utils::head(unique(unresolved), 5), collapse = ", ")
    message <- paste0(
      "Unmapped country names in partnership_strength_country_to_iso: ",
      sample_vals
    )

    if (identical(unresolved_action, "error")) {
      stop(message)
    }
    if (identical(unresolved_action, "warn")) {
      warning(message, call. = FALSE)
    }
  }

  mapped$iso3c
}

partnership_strength_iso_to_country <- function(iso, country_info) {
  if (is.null(country_info) || nrow(country_info) == 0) {
    stop("country_info is required to map ISO3 codes to country names.")
  }
  lookup <- country_info %>%
    dplyr::mutate(country = partnership_strength_standardize_countries(country)) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)

  iso <- as.character(iso)
  out <- lookup$country[match(iso, lookup$iso3c)]

  # country_to_iso() accepts ISO3 codes outside country_info (Taiwan and ~20 territories the
  # ES/EO tables carry). WDI has no name for them, so they came back NA and the PSI, which keys
  # on the partner name, pooled them all into one "NA" partner. Name them with the shared
  # canonical name ("Taiwan"), else countrycode's; the name maps back to the same ISO3.
  missing <- is.na(out) & !is.na(iso)
  if (any(missing)) {
    missing_iso <- unique(iso[missing])
    fallback <- partnership_strength_standardize_countries(country_name_from_iso3(missing_iso))
    out[missing] <- fallback[match(iso[missing], missing_iso)]
  }
  out
}

# Collapse a Country-keyed ES/EO index table to one value per (iso3c, tech, supply_chain).
#
# The upstream country indices carried some economies under two or three spellings ("Viet Nam" /
# "Vietnam", "Slovak Republic" / "Slovakia", "Taiwan" / "Taiwan, Province of China", ...), each
# with its own value. Joined on ISO3 they fanned every dyad cell into 2-36 conflicting rows.
# Rule: drop missing values; prefer the record filed under the canonical spelling (the
# country_info name, else the shared canonical / countrycode name), first as written and then
# after standardize_country_names(); average only among records at the best available level.
# Historical and unrecognised entities ("USSR", "Northern Cyprus") get no ISO3 and are dropped.
# Once the upstream tables carry one spelling per economy this is a no-op safety net.
partnership_strength_country_iso_table <- function(tbl,
                                                   value_col,
                                                   country_info,
                                                   techs = NULL) {
  require_columns(tbl, c("Country", "tech", "supply_chain", value_col), label = value_col)
  tbl <- partnership_strength_filter_techs(tbl, techs)

  raw_name <- stringr::str_squish(as.character(tbl$Country))
  std_name <- partnership_strength_standardize_countries(raw_name)
  keyed <- tibble::tibble(
    raw_name = raw_name,
    std_name = std_name,
    iso3c = partnership_strength_country_to_iso(std_name, country_info),
    tech = as.character(tbl$tech),
    supply_chain = as.character(tbl$supply_chain),
    value = suppressWarnings(as.numeric(tbl[[value_col]]))
  ) %>%
    dplyr::filter(!is.na(iso3c))

  canonical <- partnership_strength_iso_to_country(keyed$iso3c, country_info)
  keyed$priority <- dplyr::case_when(
    keyed$raw_name == canonical ~ 2L,
    keyed$std_name == canonical ~ 1L,
    TRUE ~ 0L
  )
  keyed$priority[is.na(keyed$value)] <- -1L

  keyed %>%
    dplyr::group_by(iso3c, tech, supply_chain) %>%
    dplyr::filter(priority == max(priority)) %>%
    dplyr::summarise(
      !!rlang::sym(value_col) := if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}

partnership_strength_clean_outbound_countries <- function(country) {
  recode_map <- c(
    "Curacao" = "Curaçao",
    "Cura ao" = "Curaçao",
    "Turkey" = "Turkiye",
    "United kingdom" = "United Kingdom",
    "Korea, Republic of" = "South Korea",
    "Egypt, Arab Republic of" = "Egypt",
    "Lao People's Democratic Republic" = "Laos",
    "Russian Federation" = "Russia",
    "Czechia" = "Czech Republic",
    "Venezuela, República Bolivariana de" = "Venezuela",
    # IMF DIP spellings that neither WDI nor countrycode recognise.
    "Hong Kong Special Administrative Region, People's Republic of China" = "Hong Kong",
    "Macao Special Administrative Region, People's Republic of China" = "Macau",
    "Aruba, Kingdom of the Netherlands" = "Aruba",
    "Curaçao, Kingdom of the Netherlands" = "Curaçao",
    "Sint Maarten, Kingdom of the Netherlands" = "Sint Maarten (Dutch part)",
    "Kosovo, Republic of" = "Kosovo",
    "Anguilla, United Kingdom-British Overseas Territory" = "Anguilla",
    "Montserrat, United Kingdom-British Overseas Territory" = "Montserrat"
  )

  partnership_strength_standardize_countries(dplyr::recode(country, !!!recode_map, .default = country))
}

partnership_strength_build_trade_mapping <- function(subcat, include_sub_sector = FALSE) {
  tech_col <- partnership_strength_pick_column(
    subcat,
    c("tech", "Technology", "technology"),
    "technology"
  )
  supply_chain_col <- partnership_strength_pick_column(
    subcat,
    c("supply_chain", "Value.Chain", "value_chain", "Supply.Chain", "supply chain"),
    "supply_chain"
  )
  sub_sector_col <- intersect(c("sub_sector", "Sub.Sector", "subsector"), names(subcat))
  code_col <- partnership_strength_pick_column(subcat, c("code", "HS6", "hs6", "HS_6", "cmd_code"), "HS6")
  # Resolved outside transmute(): the inline `if (...) .data[[sub_sector_col[[1]]]]` it replaces
  # errored (subscript out of bounds) on a crosswalk without a sub_sector column.
  sub_sector_values <- if (length(sub_sector_col) > 0) as.character(subcat[[sub_sector_col[[1]]]]) else rep(NA_character_, nrow(subcat))

  subcat %>%
    dplyr::transmute(
      tech = as.character(.data[[tech_col]]),
      supply_chain = as.character(.data[[supply_chain_col]]),
      sub_sector = sub_sector_values,
      code = stringr::str_pad(as.character(.data[[code_col]]), width = 6, side = "left", pad = "0")
    ) %>%
    dplyr::filter(!is.na(code), nzchar(code)) %>%
    dplyr::mutate(
      sub_sector = if (isTRUE(include_sub_sector)) dplyr::coalesce(sub_sector, "All") else "All"
    ) %>%
    dplyr::distinct()
}

partnership_strength_clean_ghg <- function(tech_ghg_raw) {
  require_columns(tech_ghg_raw, c("Tech", "ghg_intensity"), label = "tech_ghg_raw")
  
  base <- tech_ghg_raw %>%
    dplyr::mutate(
      # normalize unicode dashes to plain hyphen
      Tech = stringr::str_replace_all(Tech, "[\u2013\u2014]", "-"),
      Tech = stringr::str_squish(Tech),
      Tech = dplyr::case_when(
        Tech == "Coal (pulverised)" ~ "Coal",
        Tech == "Natural-gas combined cycle" ~ "Gas",
        Tech == "Oil-fired steam" ~ "Oil",
        stringr::str_detect(Tech, "^Solar PV") ~ "Solar",
        stringr::str_detect(Tech, "Concentrated solar power") ~ "Solar",
        stringr::str_detect(Tech, "^Wind") ~ "Wind",
        Tech == "Nuclear (light-water)" ~ "Nuclear",
        TRUE ~ Tech
      )
    ) %>%
    # Geothermal has its own IPCC row (38 gCO2e/kWh); it used to be filtered out here and
    # imputed with the Solar value below.
    dplyr::filter(Tech %in% c("Coal","Gas","Oil","Solar","Nuclear","Wind","Geothermal")) %>%
    dplyr::mutate(ghg_index = partnership_strength_min_max_index(-ghg_intensity)) %>%
    dplyr::transmute(tech = Tech, ghg_index) %>%
    dplyr::group_by(tech) %>%
    dplyr::summarise(ghg_index = mean(ghg_index, na.rm = TRUE), .groups = "drop")

  get_idx <- function(df, t) {
    x <- df$ghg_index[df$tech == t]
    if (length(x) == 0 || all(is.na(x))) NA_real_ else x[1]
  }

  safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

  extra <- tibble::tibble(
    tech = c("Batteries", "Electric Vehicles", "Green Hydrogen", "Geothermal", "Electric Grid"),
    ghg_index = c(
      get_idx(base, "Solar"),
      safe_mean(base$ghg_index[base$tech %in% c("Coal","Oil","Gas","Solar","Wind","Nuclear")]),
      safe_mean(base$ghg_index[base$tech %in% c("Gas","Solar")]),
      get_idx(base, "Solar"),
      safe_mean(base$ghg_index[base$tech %in% c("Solar","Wind")])
    )
  ) %>%
    # Proxies only for technologies without their own IPCC row.
    dplyr::filter(!tech %in% base$tech)

  dplyr::bind_rows(base, extra)
}


partnership_strength_clean_policy <- function(cat_raw, country_info = NULL) {
  
  cleaned <- cat_raw %>%
    dplyr::mutate(
      climate_policy_index = dplyr::case_when(
        Overall.rating == "Critically insufficient" ~ 0.25,
        Overall.rating == "Highly insufficient"     ~ 0.5,
        Overall.rating == "Insufficient"            ~ 0.75,
        Overall.rating == "Almost Sufficient"       ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::transmute(Country = Country, climate_policy_index = climate_policy_index)
  
  # standardize country names + (optionally) attach iso3c and filter to valid iso3c
  standardize_country_table(cleaned, country_info = country_info)
}
