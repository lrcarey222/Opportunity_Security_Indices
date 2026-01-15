# Shared helper to append Overall category indices.
energy_security_add_overall_index <- function(tbl, include_sub_sector = FALSE) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(tbl)
  }

  tbl <- tbl %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      sub_sector = if (isTRUE(include_sub_sector) && "sub_sector" %in% names(tbl)) {
        as.character(sub_sector)
      } else {
        "All"
      },
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    )

  apply_overall_definitions(tbl, include_sub_sector = include_sub_sector)
}
