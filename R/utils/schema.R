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

  if (!(is.numeric(tbl$Year) || is.integer(tbl$Year))) {
    stop("Column 'Year' must be numeric or integer.")
  }

  invisible(tbl)
}
