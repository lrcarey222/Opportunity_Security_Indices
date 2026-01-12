# Country standardization helpers.
country_recode_map <- function() {
  c(
    "US" = "United States",
    "USA" = "United States",
    "UK" = "United Kingdom",
    "United kingdom" = "United Kingdom",
    "Korea" = "South Korea",
    "Korea, Rep." = "South Korea",
    "Korea, Dem. People's Rep." = "North Korea",
    "Vietnam" = "Viet Nam",
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

standardize_country_info <- function(country_info) {
  if (is.null(country_info)) {
    return(country_info)
  }
  if (!"country" %in% names(country_info)) {
    stop("country_info must include a 'country' column.")
  }

  standardized <- country_info %>%
    dplyr::mutate(country = standardize_country_names(country)) %>%
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
    dplyr::mutate(Country = standardize_country_names(Country))

  if (is.null(country_info)) {
    return(standardized)
  }

  standardized %>%
    dplyr::left_join(
      country_info %>% dplyr::select(iso3c, country),
      by = c("Country" = "country")
    ) %>%
    dplyr::filter(!is.na(iso3c))
}
