# Aggregate Economic Opportunity index into tidy schema.

resolve_year_provenance <- function(index_tbl, year_provenance, label) {
  year_cols <- intersect(names(index_tbl), names(year_provenance))
  join_cols <- intersect(year_cols, c("Country", "tech", "supply_chain", "sub_sector"))
  if (length(join_cols) == 0) {
    stop("Unable to join year provenance; missing key columns.")
  }

  selected_year_provenance <- year_provenance %>%
    dplyr::select(dplyr::all_of(c(join_cols, "Year_selected"))) %>%
    dplyr::distinct()

  keys_for_year_resolution <- intersect(join_cols, c("Country", "tech", "supply_chain"))
  if (length(keys_for_year_resolution) > 0 && !identical(keys_for_year_resolution, join_cols)) {
    conflicting_years <- selected_year_provenance %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keys_for_year_resolution))) %>%
      dplyr::summarise(
        distinct_years = dplyr::n_distinct(Year_selected, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(distinct_years > 1)

    if (nrow(conflicting_years) > 0) {
      stop(
        "Year provenance has multiple Year_selected values within ", label,
        " for the same Country/tech/supply_chain combination."
      )
    }

    selected_year_provenance <- selected_year_provenance %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keys_for_year_resolution))) %>%
      dplyr::summarise(Year_selected = dplyr::first(Year_selected), .groups = "drop")

    join_cols <- keys_for_year_resolution
  }

  assert_unique_keys(selected_year_provenance, join_cols, label = label)

  list(year_provenance = selected_year_provenance, join_cols = join_cols)
}

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

  index_col <- intersect(
    c("Economic_Opportunity_Index", "Economic Opportunity Index", "economic_opportunity_index"),
    names(index_tbl)
  )
  if (length(index_col) == 0) {
    stop("economic_opportunity_index is missing an Economic Opportunity index column.")
  }
  index_col <- index_col[[1]]
  require_columns(index_tbl, c("Country", "tech", "supply_chain", index_col), label = "economic_opportunity_index")

  diagnostics <- economic_opportunity_outputs$diagnostics
  if (is.null(diagnostics) || is.null(diagnostics$year_provenance)) {
    stop("economic_opportunity_outputs is missing year provenance diagnostics.")
  }

  year_provenance <- diagnostics$year_provenance
  year_resolution <- resolve_year_provenance(
    index_tbl = index_tbl,
    year_provenance = year_provenance,
    label = "economic opportunity year provenance"
  )
  year_provenance <- year_resolution$year_provenance
  join_cols <- year_resolution$join_cols

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
      value = .data[[index_col]],
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

  index_col <- intersect(
    c("Energy_Security_Index", "Energy Security Index", "energy_security_index"),
    names(index_tbl)
  )
  if (length(index_col) == 0) {
    stop("energy_security_index is missing an Energy Security index column.")
  }
  index_col <- index_col[[1]]
  require_columns(index_tbl, c("Country", "tech", "supply_chain", index_col), label = "energy_security_index")

  diagnostics <- energy_security_outputs$diagnostics
  if (is.null(diagnostics) || is.null(diagnostics$year_provenance)) {
    stop("energy_security_outputs is missing year provenance diagnostics.")
  }

  year_provenance <- diagnostics$year_provenance
  year_resolution <- resolve_year_provenance(
    index_tbl = index_tbl,
    year_provenance = year_provenance,
    label = "energy security year provenance"
  )
  year_provenance <- year_resolution$year_provenance
  join_cols <- year_resolution$join_cols

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
      value = .data[[index_col]],
      Year = as.integer(Year_selected),
      source = source,
      explanation = explanation
    )
}
