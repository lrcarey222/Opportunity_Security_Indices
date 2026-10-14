# Shared theme-table standardization helpers.
#
# These were previously defined inline in scripts/10_build_themes.R. They are pure
# transformations, so they live in R/ and are sourced by every script that has to bring a
# theme table onto the common schema - the annual pipeline and the vintage builder in
# scripts/40_build_index_vintages.R alike. The bodies are kept as they were so the annual
# pipeline's output is unchanged by the move.

rebuild_theme_overall_indices <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0 || !"variable" %in% names(tbl) || !"data_type" %in% names(tbl)) {
    return(tbl)
  }

  index_definition <- getOption("opportunity_security.index_definition")
  if (is.null(index_definition)) {
    return(tbl)
  }

  overall_defs <- index_definition$overall_variables
  overall_names <- names(overall_defs)

  if (is.null(overall_defs) || length(overall_names) == 0) {
    return(tbl)
  }

  include_sub_sector <- "sub_sector" %in% names(tbl)
  tbl_without_overall <- tbl %>%
    dplyr::filter(!(data_type == "index" & variable %in% overall_names))

  apply_overall_definitions(tbl_without_overall, include_sub_sector = include_sub_sector)
}

standardize_theme_types <- function(tbl, country_info = NULL) {
  if (is.null(tbl)) {
    return(tbl)
  }

  core_cols <- c(
    "Country", "iso3c", "tech", "supply_chain", "sub_sector",
    "category", "variable", "data_type", "value", "Year", "source", "explanation"
  )

  keep_theme_schema <- function(x) {
    x %>%
      dplyr::select(dplyr::any_of(core_cols))
  }

  standardized <- tbl %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      sub_sector = if ("sub_sector" %in% names(tbl)) as.character(sub_sector) else NULL,
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    ) %>%
    keep_theme_schema()

  if (is.null(country_info)) {
    return(rebuild_theme_overall_indices(standardized))
  }

  standardized_with_country <- standardize_country_table(
    standardized,
    country_info = country_info
  ) %>%
    keep_theme_schema()

  # Guard against full row-loss when country matching fails for a dataset.
  # In that case, preserve the standardized rows and allow downstream missing-data
  # handling to proceed rather than returning an empty table.
  if (nrow(standardized_with_country) == 0 && nrow(standardized) > 0) {
    warning(
      "Country standardization dropped all rows in standardize_theme_types(); ",
      "returning unfiltered standardized rows before rebuilding overall indices."
    )
    return(rebuild_theme_overall_indices(standardized))
  }

  standardized_with_country <- standardize_country_table(
    standardized,
    country_info = country_info
  )

  # Guard against full row-loss when country matching fails for a dataset.
  # In that case, preserve the standardized rows and allow downstream missing-data
  # handling to proceed rather than returning an empty table.
  if (nrow(standardized_with_country) == 0 && nrow(standardized) > 0) {
    warning(
      "Country standardization dropped all rows in standardize_theme_types(); ",
      "returning unfiltered standardized rows instead."
    )
    return(standardized)
  }

  standardized_with_country
}
