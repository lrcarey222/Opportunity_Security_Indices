# Shared helper to append Overall category indices.
energy_security_add_overall_index <- function(tbl, include_sub_sector = FALSE) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(tbl)
  }

  if (include_sub_sector && !"sub_sector" %in% names(tbl)) {
    tbl$sub_sector <- "All"
  }

  tbl <- tbl %>%
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
    )

  existing_overall_categories <- tbl %>%
    dplyr::filter(data_type == "index", grepl("Overall", variable)) %>%
    dplyr::distinct(category) %>%
    dplyr::pull(category)

  overall_candidates <- tbl %>%
    dplyr::filter(data_type == "index", !grepl("Overall", variable))

  if (nrow(overall_candidates) == 0) {
    return(tbl)
  }

  group_cols <- c("Country", "tech", "supply_chain", "Year", "category")
  if (include_sub_sector) {
    group_cols <- c(group_cols, "sub_sector")
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  latest_year <- overall_candidates %>%
    dplyr::filter(!is.na(Year), Year <= current_year) %>%
    dplyr::summarize(latest_year = max(Year, na.rm = TRUE)) %>%
    dplyr::pull(latest_year)

  overall_indices <- overall_candidates %>%
    dplyr::filter(Year == latest_year) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!category %in% existing_overall_categories) %>%
    dplyr::mutate(
      variable = paste("Overall", category, "Index"),
      data_type = "index",
      source = "Author calculation",
      explanation = "Mean across category index variables"
    )

  select_cols <- c("Country", "tech", "supply_chain")
  if ("sub_sector" %in% names(overall_indices)) {
    select_cols <- c(select_cols, "sub_sector")
  }
  select_cols <- c(
    select_cols,
    "category",
    "variable",
    "data_type",
    "value",
    "Year",
    "source",
    "explanation"
  )

  overall_indices <- overall_indices %>%
    dplyr::select(dplyr::all_of(select_cols))

  dplyr::bind_rows(tbl, overall_indices)
}
