# Country standardization helpers.
country_recode_map <- function() {
  c(
    "United States of America" = "United States",
    "US" = "United States",
    "USA" = "United States",
    "UK" = "United Kingdom",
    "United kingdom" = "United Kingdom",
    "Korea" = "South Korea",
    "Korea, Rep." = "South Korea",
    "Republic of Korea" = "South Korea",
    "Korea, Dem. People's Rep." = "North Korea",
    "Iran, Islamic Rep." = "Iran",
    "Turkey" = "Turkiye",
    "Curacao" = "Curaçao",
    "Saudi arabia" = "Saudi Arabia",
    "Russian Federation" = "Russia",
    "Czechia" = "Czech Republic",
    "Yemen, Rep." = "Yemen",
    "Venezuela, RB" = "Venezuela",
    "Egypt, Arab Rep." = "Egypt",
    "Gambia, The" = "Gambia",
    "Bahamas, The" = "Bahamas",
    "Hong Kong SAR, China" = "Hong Kong",
    "Macao SAR, China" = "Macau",
    "Micronesia, Fed. Sts." = "Micronesia",
    "Congo, Dem. Rep." = "Democratic Republic of Congo",
    "Congo, Rep." = "Congo"
  )
}

standardize_country_names <- function(country) {
  if (is.null(country)) {
    return(country)
  }

  cleaned <- stringr::str_trim(as.character(country))
  cleaned <- stringr::str_replace_all(cleaned, "\\s+", " ")

  dplyr::recode(cleaned, !!!country_recode_map(), .default = cleaned)
}

normalize_iso3c <- function(x) {
  cleaned <- toupper(stringr::str_trim(as.character(x)))
  cleaned[cleaned %in% c("", "NA", "N/A", "NULL", "<NA>")] <- NA_character_
  cleaned
}

standardize_country_info <- function(country_info) {
  if (is.null(country_info)) {
    return(country_info)
  }
  if (!"country" %in% names(country_info)) {
    stop("country_info must include a 'country' column.")
  }

  standardized <- country_info %>%
    dplyr::mutate(
      country = standardize_country_names(country),
      iso3c = normalize_iso3c(iso3c)
    ) %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c))

  if ("region" %in% names(standardized)) {
    standardized <- standardized %>% dplyr::filter(region != "Aggregates")
  }

  standardized %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)
}

standardize_country_table <- function(tbl, country_info = NULL) {
  if (is.null(tbl) || !"Country" %in% names(tbl)) {
    return(tbl)
  }

  standardized <- tbl %>%
    dplyr::select(-dplyr::any_of(c(
      "country",
      "country_std",
      "country_std.x",
      "country_std.y",
      "iso3c.x",
      "iso3c.y"
    ))) %>%
    dplyr::mutate(
      Country = standardize_country_names(Country),
      iso3c = if ("iso3c" %in% names(tbl)) normalize_iso3c(iso3c) else NA_character_
    )

  if (is.null(country_info)) {
    return(standardized)
  }

  country_ref <- country_info %>%
    dplyr::transmute(
      ref_iso3c = normalize_iso3c(iso3c),
      ref_country = as.character(country),
      ref_country_std = standardize_country_names(country)
    ) %>%
    dplyr::distinct(ref_iso3c, .keep_all = TRUE)

  with_iso <- standardized %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c)) %>%
    dplyr::left_join(country_ref, by = c("iso3c" = "ref_iso3c")) %>%
    dplyr::mutate(Country = dplyr::coalesce(ref_country, Country))

  without_iso <- standardized %>%
    dplyr::filter(is.na(iso3c) | !nzchar(iso3c)) %>%
    dplyr::left_join(country_ref, by = c("Country" = "ref_country_std")) %>%
    dplyr::mutate(
      iso3c = dplyr::coalesce(iso3c, ref_iso3c),
      Country = dplyr::coalesce(ref_country, Country)
    )

  standardized_with_ref <- dplyr::bind_rows(with_iso, without_iso) %>%
    dplyr::mutate(
      iso3c = normalize_iso3c(iso3c),
      iso3c = dplyr::coalesce(
        iso3c,
        countrycode::countrycode(Country, origin = "country.name", destination = "iso3c")
      ),
      iso3c = normalize_iso3c(iso3c)
    ) %>%
    dplyr::select(-dplyr::any_of(c("ref_iso3c", "ref_country", "ref_country_std")))

  standardized_filtered <- standardized_with_ref %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c))

  if (nrow(standardized_filtered) == 0 && nrow(standardized_with_ref) > 0) {
    warning(
      "Country standardization dropped all rows in standardize_country_table(); ",
      "returning rows without iso3c filtering."
    )
    return(standardized_with_ref)
  }

  standardized_filtered
}

