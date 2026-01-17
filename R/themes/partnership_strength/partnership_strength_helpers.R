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

partnership_strength_country_to_iso <- function(country, country_info) {
  if (is.null(country_info) || nrow(country_info) == 0) {
    stop("country_info is required to map country names to ISO3 codes.")
  }

  cleaned <- partnership_strength_standardize_countries(country)
  lookup <- country_info %>%
    dplyr::mutate(country = partnership_strength_standardize_countries(country)) %>%
    dplyr::distinct(country, iso3c)

  mapped <- tibble::tibble(country = cleaned) %>%
    dplyr::left_join(lookup, by = "country")

  unmapped <- mapped$country[is.na(mapped$iso3c)]
  if (length(unmapped) > 0) {
    fallback <- countrycode::countrycode(
      unmapped,
      "country.name",
      "iso3c",
      custom_match = c(
        "Viet Nam" = "VNM",
        "Turkiye" = "TUR",
        "South Korea" = "KOR",
        "Curaçao" = "CUW",
        "Cura ao" = "CUW",
        "Laos" = "LAO",
        "Czech Republic" = "CZE"
      )
    )
    mapped$iso3c[is.na(mapped$iso3c)] <- fallback
  }

  mapped$iso3c
}

partnership_strength_iso_to_country <- function(iso, country_info) {
  if (is.null(country_info) || nrow(country_info) == 0) {
    stop("country_info is required to map ISO3 codes to country names.")
  }
  lookup <- country_info %>%
    dplyr::mutate(country = partnership_strength_standardize_countries(country)) %>%
    dplyr::distinct(iso3c, country)

  tibble::tibble(iso3c = iso) %>%
    dplyr::left_join(lookup, by = "iso3c") %>%
    dplyr::pull(country)
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
    "Venezuela, República Bolivariana de" = "Venezuela"
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

  subcat %>%
    dplyr::transmute(
      tech = as.character(.data[[tech_col]]),
      supply_chain = as.character(.data[[supply_chain_col]]),
      sub_sector = if (length(sub_sector_col) > 0) as.character(.data[[sub_sector_col[[1]]]]) else NA_character_,
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

  tech_ghg_raw %>%
    dplyr::mutate(
      Tech = dplyr::recode(
        Tech,
        "Coal (pulverised)" = "Coal",
        "Natural-gas combined cycle" = "Gas",
        "Oil-fired steam" = "Oil",
        "Solar PV - utility scale" = "Solar",
        "Nuclear (light-water)" = "Nuclear",
        "Wind - onshore" = "Wind"
      )
    ) %>%
    dplyr::mutate(ghg_index = partnership_strength_min_max_index(-ghg_intensity)) %>%
    dplyr::transmute(tech = Tech, ghg_index = ghg_index)
}

partnership_strength_clean_policy <- function(cat_raw) {
  require_columns(cat_raw, c("Country", "Overall.rating"), label = "cat_raw")

  cat_raw %>%
    dplyr::mutate(
      climate_policy_index = dplyr::case_when(
        Overall.rating == "Critically insufficient" ~ 0.25,
        Overall.rating == "Highly insufficient" ~ 0.5,
        Overall.rating == "Insufficient" ~ 0.75,
        Overall.rating == "Almost Sufficient" ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::transmute(Country = Country, climate_policy_index = climate_policy_index)
}
