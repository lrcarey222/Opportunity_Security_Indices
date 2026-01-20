# Partnership Strength composite (PSI) tidy output helper.

psi_composite <- function(index_tbl,
                          year,
                          variable = "Partnership Strength Index",
                          category = "psi",
                          source = "Author calculation",
                          explanation = "Weighted composite of friendshore, opportunity, and development indices.") {
  require_columns(index_tbl, c("Country", "tech", "supply_chain", "Partnership_Strength_Index"), label = "psi_index")

  composite <- index_tbl %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = category,
      variable = variable,
      data_type = "index",
      value = Partnership_Strength_Index,
      Year = as.integer(year),
      source = source,
      explanation = explanation
    )

  standardized <- partnership_strength_standardize_bind_rows(composite)
  partnership_strength_validate_schema(standardized, label = "psi_composite")
  standardized
}
