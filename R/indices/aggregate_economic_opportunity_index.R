# Aggregate Economic Opportunity index into tidy schema.

aggregate_economic_opportunity_index <- function(economic_opportunity_outputs,
                                                 variable = "Overall Economic Opportunity Index",
                                                 category = "Economic Opportunity",
                                                 source = "Author calculation",
                                                 explanation = "Economic opportunity index from weighted category scores.") {
  if (is.null(economic_opportunity_outputs)) {
    stop("economic_opportunity_outputs is required.")
  }

  index_tbl <- economic_opportunity_outputs$index
  if (is.null(index_tbl)) {
    index_tbl <- economic_opportunity_outputs$economic_opportunity_index
  }
  if (is.null(index_tbl)) {
    stop("economic_opportunity_outputs is missing index data.")
  }

  require_columns(index_tbl, c("Country", "tech", "supply_chain", "Economic_Opportunity_Index"), label = "economic_opportunity_index")

  diagnostics <- economic_opportunity_outputs$diagnostics
  if (is.null(diagnostics) || is.null(diagnostics$year_provenance)) {
    stop("economic_opportunity_outputs is missing year provenance diagnostics.")
  }

  year_provenance <- diagnostics$year_provenance
  year_cols <- intersect(names(index_tbl), names(year_provenance))
  join_cols <- intersect(year_cols, c("Country", "tech", "supply_chain", "sub_sector"))
  if (length(join_cols) == 0) {
    stop("Unable to join year provenance; missing key columns.")
  }

  year_provenance <- year_provenance %>%
    dplyr::select(dplyr::all_of(c(join_cols, "Year_selected"))) %>%
    dplyr::distinct()

  assert_unique_keys(year_provenance, join_cols, label = "economic opportunity year provenance")

  if (any(is.na(year_provenance$Year_selected))) {
    stop("Year provenance includes missing values; check Year normalization.")
  }

  index_tbl %>%
    dplyr::left_join(year_provenance, by = join_cols) %>%
    dplyr::transmute(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      category = category,
      variable = variable,
      data_type = "index",
      value = Economic_Opportunity_Index,
      Year = as.integer(Year_selected),
      source = source,
      explanation = explanation
    )
}

aggregate_energy_security_index <- function(energy_security_outputs,
                                            variable = "Overall Energy Security Index",
                                            category = "Energy Security",
                                            source = "Author calculation",
                                            explanation = "Energy security index from weighted category scores.") {
  if (is.null(energy_security_outputs)) {
    stop("energy_security_outputs is required.")
  }

  index_tbl <- energy_security_outputs$index
  if (is.null(index_tbl)) {
    index_tbl <- energy_security_outputs$energy_security_index
  }
  if (is.null(index_tbl)) {
    stop("energy_security_outputs is missing index data.")
  }

  require_columns(index_tbl, c("Country", "tech", "supply_chain", "Energy_Security_Index"), label = "energy_security_index")

  diagnostics <- energy_security_outputs$diagnostics
  if (is.null(diagnostics) || is.null(diagnostics$year_provenance)) {
    stop("energy_security_outputs is missing year provenance diagnostics.")
  }

  year_provenance <- diagnostics$year_provenance
  year_cols <- intersect(names(index_tbl), names(year_provenance))
  join_cols <- intersect(year_cols, c("Country", "tech", "supply_chain", "sub_sector"))
  if (length(join_cols) == 0) {
    stop("Unable to join year provenance; missing key columns.")
  }

  year_provenance <- year_provenance %>%
    dplyr::select(dplyr::all_of(c(join_cols, "Year_selected"))) %>%
    dplyr::distinct()

  assert_unique_keys(year_provenance, join_cols, label = "energy security year provenance")

  if (any(is.na(year_provenance$Year_selected))) {
    stop("Year provenance includes missing values; check Year normalization.")
  }

  index_tbl %>%
    dplyr::left_join(year_provenance, by = join_cols) %>%
    dplyr::transmute(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      category = category,
      variable = variable,
      data_type = "index",
      value = Energy_Security_Index,
      Year = as.integer(Year_selected),
      source = source,
      explanation = explanation
    )
}
