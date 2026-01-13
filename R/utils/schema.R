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

  if (!all(tbl$data_type %in% c("raw", "index"))) {
    stop("Column 'data_type' must be either 'raw' or 'index'.")
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
      Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    )
}
# schema (placeholder).
# TODO: implement.
schema_stub <- function() {
  NULL
}
