# Build Partnership Strength index outputs (friendshore + opportunity + development).

if (!exists("normalize_sub_sector_or_keys", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "index_builder_core.R"))
}

standardize_partnership_strength_inputs <- function(theme_tables) {
  theme_names <- names(theme_tables)
  if (is.null(theme_names)) {
    theme_names <- rep("unknown_theme", length(theme_tables))
  }

  standardized <- Map(function(theme_name, tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }

    standardized_tbl <- standardize_theme_table(tbl)
    standardized_tbl <- standardize_bind_rows_inputs(standardized_tbl)
    validate_schema(standardized_tbl)

    standardized_tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        category = stringr::str_to_lower(as.character(category)),
        theme = as.character(theme_name)
      )
  }, theme_names, theme_tables)

  dplyr::bind_rows(standardized)
}

build_partnership_strength_index <- function(theme_tables,
                                             weights,
                                             allow_partial_categories = TRUE) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Partnership strength weights are missing or empty.")
  }

  psi_data <- standardize_partnership_strength_inputs(theme_tables) %>%
    dplyr::filter(data_type == "index")

  if (nrow(psi_data) == 0) {
    stop("Partnership strength inputs are empty after filtering to index values.")
  }

  if (!is.integer(psi_data$Year) || any(is.na(psi_data$Year))) {
    stop("Year must be integer and non-missing for partnership strength inputs.")
  }

  group_cols <- c("Country", "tech", "supply_chain", "category")

  category_scores <- psi_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      category_score = mean(value, na.rm = TRUE),
      .groups = "drop"
    )

  weights_tbl <- tibble::tibble(
    category = stringr::str_to_lower(names(weights)),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  index_outputs <- compute_index_and_contributions(
    category_scores,
    variable_contributions = category_scores %>%
      dplyr::mutate(
        variable = category,
        component_value = category_score,
        component_weight_within_overall = 1,
        missing_rule_applied = NA_character_,
        imputed = FALSE
      ) %>%
      dplyr::select(
        Country,
        tech,
        supply_chain,
        category,
        variable,
        component_value,
        component_weight_within_overall,
        missing_rule_applied,
        imputed
      ),
    weights_tbl = weights_tbl,
    allow_partial_categories = allow_partial_categories,
    include_sub_sector = FALSE,
    pillar_label = "Partnership strength",
    index_col = "Partnership_Strength_Index"
  )

  index_tbl <- index_outputs$index_tbl %>%
    dplyr::select(-complete_categories)

  list(
    partnership_strength_index = index_tbl,
    category_contributions = index_outputs$category_contributions,
    variable_contributions = index_outputs$variable_contributions,
    index = index_tbl
  )
}
