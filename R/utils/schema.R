# Schema validation helpers.
validate_schema <- function(tbl) {
  if (!inherits(tbl, "data.frame")) {
    stop("validate_schema() expects a data.frame or tibble.")
  }

  required_cols <- c(
    "Country",
    "tech",
    "supply_chain",
    "category",
    "variable",
    "data_type",
    "value",
    "Year",
    "source",
    "explanation"
  )

  missing_cols <- setdiff(required_cols, names(tbl))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  character_cols <- c(
    "Country",
    "tech",
    "supply_chain",
    "category",
    "variable",
    "data_type",
    "source",
    "explanation"
  )

  non_character <- character_cols[!vapply(character_cols, function(col) is.character(tbl[[col]]), logical(1))]
  if (length(non_character) > 0) {
    stop("Columns with incorrect types (expected character): ", paste(non_character, collapse = ", "))
  }

  if (!is.numeric(tbl$value)) {
    stop("Column 'value' must be numeric.")
  }

  if (!is.integer(tbl$Year)) {
    stop("Column 'Year' must be integer.")
  }

  if (any(is.na(tbl$Year))) {
    stop("Column 'Year' must not contain missing values.")
  }

  if (!all(tbl$data_type %in% c("raw", "index", "weight", "contribution"))) {
    stop("Column 'data_type' must be one of: raw, index, weight, contribution.")
  }

  if (any(is.na(tbl$data_type))) {
    stop("Column 'data_type' must not contain missing values.")
  }

  invisible(tbl)
}

require_columns <- function(tbl, columns, label = "table") {
  if (!inherits(tbl, "data.frame")) {
    stop("require_columns() expects a data.frame for ", label, ".")
  }

  missing_cols <- setdiff(columns, names(tbl))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ",
      label,
      ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(tbl)
}

assert_unique_keys <- function(tbl, keys, label = "table") {
  require_columns(tbl, keys, label = label)
  key_counts <- tbl %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(key_counts) > 0) {
    stop("Duplicate key(s) detected in ", label, " for: ", paste(keys, collapse = ", "))
  }

  invisible(tbl)
}

# The index tables key on Country while the partner and allied-network code keys on iso3c,
# so the two must name the same economies one-to-one. Reports Country strings carrying more
# than one code and codes carrying more than one Country string (NA codes are ignored).
# action = "warn" returns the offending pairs invisibly; "error" stops.
assert_country_iso3_consistent <- function(tbl, label = "table", action = c("warn", "error")) {
  action <- match.arg(action)
  if (!inherits(tbl, "data.frame") || !all(c("Country", "iso3c") %in% names(tbl))) {
    return(invisible(NULL))
  }

  pairs <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c), !is.na(Country)) %>%
    dplyr::distinct(Country, iso3c)

  conflicts <- dplyr::bind_rows(
    pairs %>% dplyr::group_by(Country) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::ungroup(),
    pairs %>% dplyr::group_by(iso3c) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::ungroup()
  ) %>%
    dplyr::distinct(Country, iso3c) %>%
    dplyr::arrange(iso3c, Country)

  if (nrow(conflicts) > 0) {
    msg <- paste0(
      "Country and iso3c are not one-to-one in ", label, ": ",
      paste0(conflicts$Country, " = ", conflicts$iso3c, collapse = "; ")
    )
    if (identical(action, "error")) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  invisible(conflicts)
}

# Year as integer from a year, a date, or a label. ISO dates ("2022-01-01", "2022-01")
# take their leading year; anything else takes a trailing four-digit year, so a range
# such as "2020-2022" resolves to its end year.
extract_year_int <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.POSIXlt(x)$year + 1900L)
  }
  values <- as.character(x)
  iso_year <- stringr::str_match(values, "^(\\d{4})-\\d{2}(-\\d{2})?([ T].*)?$")[, 2]
  year_text <- dplyr::coalesce(iso_year, stringr::str_extract(values, "\\d{4}$"))
  suppressWarnings(as.integer(year_text))
}

standardize_theme_table <- function(tbl) {
  if (is.null(tbl)) {
    return(tbl)
  }

  if (!inherits(tbl, "data.frame")) {
    stop("standardize_theme_table() expects a data.frame.")
  }

  tbl %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      Year = extract_year_int(Year),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    )
}

standardize_bind_rows_inputs <- function(tbl) {
  if (is.null(tbl)) {
    return(tbl)
  }

  if (!inherits(tbl, "data.frame")) {
    stop("standardize_bind_rows_inputs() expects a data.frame.")
  }

  if ("Year" %in% names(tbl)) {
    tbl$Year <- extract_year_int(tbl$Year)
  }

  if ("value" %in% names(tbl)) {
    tbl$value <- suppressWarnings(as.numeric(tbl$value))
  }

  tbl
}
# schema (placeholder).
# TODO: implement.
schema_stub <- function() {
  NULL
}
