# Shared helper to append Overall category indices.
energy_security_add_overall_index <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(tbl)
  }

  existing_overall_categories <- tbl %>%
    dplyr::filter(data_type == "index", grepl("Overall", variable)) %>%
    dplyr::distinct(category) %>%
    dplyr::pull(category)

  overall_candidates <- tbl %>%
    dplyr::filter(data_type == "index", !grepl("Overall", variable))

  if (nrow(overall_candidates) == 0) {
    return(tbl)
  }

  overall_indices <- overall_candidates %>%
    dplyr::group_by(Country, tech, supply_chain, Year, category) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!category %in% existing_overall_categories) %>%
    dplyr::mutate(
      variable = paste("Overall", category, "Index"),
      data_type = "index",
      source = "Author calculation",
      explanation = "Mean across category index variables"
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )

  dplyr::bind_rows(tbl, overall_indices)
}
