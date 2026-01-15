# Build Economic Opportunity index outputs (category scores + overall index).
standardize_economic_opportunity_inputs <- function(theme_tables, include_sub_sector = FALSE) {
  standardized <- lapply(theme_tables, function(tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }
    standardized_tbl <- standardize_theme_table(tbl)
    standardized_tbl <- standardize_bind_rows_inputs(standardized_tbl)
    validate_schema(standardized_tbl)
    standardized_tbl <- standardized_tbl %>%
      dplyr::mutate(
        sub_sector = if (isTRUE(include_sub_sector) && "sub_sector" %in% names(standardized_tbl)) {
          as.character(sub_sector)
        } else {
          "All"
        }
      )
    standardized_tbl
  })

  dplyr::bind_rows(standardized)
}

build_economic_opportunity_index <- function(theme_tables,
                                             weights,
                                             allow_partial_categories = TRUE,
                                             include_sub_sector = FALSE,
                                             techs = c(
                                               "Electric Vehicles",
                                               "Nuclear",
                                               "Coal",
                                               "Batteries",
                                               "Green Hydrogen",
                                               "Wind",
                                               "Oil",
                                               "Solar",
                                               "Gas",
                                               "Geothermal",
                                               "Electric Grid"
                                             )) {
  message("Building economic opportunity index: standardizing theme inputs.")
  if (is.null(weights) || length(weights) == 0) {
    stop("Economic opportunity weights are missing or empty.")
  }

  index_definition <- resolve_index_definition()
  score_variables <- index_definition$pillars$economic_opportunity$categories
  if (is.null(score_variables) || length(score_variables) == 0) {
    stop("Index definition missing economic opportunity category definitions.")
  }

  score_variables_tbl <- tibble::tibble(
    category = names(score_variables),
    score_variable = vapply(score_variables, function(x) x$score_variable, character(1))
  )

  economic_opportunity_data <- standardize_economic_opportunity_inputs(
    theme_tables,
    include_sub_sector = include_sub_sector
  ) %>%
    dplyr::filter(data_type == "index", tech %in% techs) %>%
    apply_overall_definitions(
      index_definition = index_definition,
      include_sub_sector = include_sub_sector
    )

  validate_variable_levels(
    economic_opportunity_data,
    index_definition = index_definition,
    include_sub_sector = include_sub_sector
  )

  require_columns(
    economic_opportunity_data,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "value"),
    label = "economic_opportunity_data"
  )
  group_cols <- c("Country", "tech", "supply_chain")
  if (isTRUE(include_sub_sector)) {
    group_cols <- c(group_cols, "sub_sector")
  }
  assert_unique_keys(
    economic_opportunity_data,
    c(group_cols, "category", "variable", "Year"),
    label = "economic_opportunity_data"
  )

  message("Filtering economic opportunity data to configured score variables only.")
  economic_opportunity_overall <- economic_opportunity_data %>%
    dplyr::inner_join(score_variables_tbl, by = c("category", "variable" = "score_variable"))

  require_columns(
    economic_opportunity_overall,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "value"),
    label = "economic_opportunity_overall"
  )
  assert_unique_keys(
    economic_opportunity_overall,
    c(group_cols, "category", "variable", "Year"),
    label = "economic_opportunity_overall"
  )

  weights_tbl <- tibble::tibble(
    category = names(weights),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )
  assert_unique_keys(weights_tbl, "category", label = "economic_opportunity_weights")

  message("Computing category-level scores.")
  category_scores <- economic_opportunity_overall %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "category")))) %>%
    dplyr::summarize(category_score = mean(value, na.rm = TRUE), .groups = "drop")
  assert_unique_keys(
    category_scores,
    c(group_cols, "category"),
    label = "economic_opportunity_category_scores"
  )

  latest_years <- economic_opportunity_overall %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(Year = as.integer(max(Year, na.rm = TRUE)), .groups = "drop")
  assert_unique_keys(
    latest_years,
    group_cols,
    label = "economic_opportunity_latest_years"
  )

  categories_in_data <- sort(unique(category_scores$category))
  categories_in_weights <- sort(unique(weights_tbl$category))

  extra_config_categories <- setdiff(categories_in_weights, categories_in_data)
  if (length(extra_config_categories) > 0) {
    warning(
      "Economic opportunity weights include categories not present in theme output: ",
      paste(extra_config_categories, collapse = ", ")
    )
  }

  missing_weight_categories <- setdiff(categories_in_data, categories_in_weights)
  if (length(missing_weight_categories) > 0) {
    msg <- paste(
      "Theme categories are missing weights and will be excluded:",
      paste(missing_weight_categories, collapse = ", ")
    )
    if (isTRUE(allow_partial_categories)) {
      warning(msg)
    } else {
      stop(msg)
    }
  }

  weights_tbl <- weights_tbl %>%
    dplyr::filter(category %in% categories_in_data)

  available_categories <- category_scores %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols[-1], "category"))))
  assert_unique_keys(
    available_categories,
    c(group_cols[-1], "category"),
    label = "economic_opportunity_available_categories"
  )

  available_by_group <- available_categories %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols[-1]))) %>%
    dplyr::summarize(
      available_categories = list(unique(category)),
      .groups = "drop"
    )

  group_missing_categories <- category_scores %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      present_categories = list(unique(category)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(available_by_group, by = group_cols[-1]) %>%
    dplyr::mutate(
      missing_categories = Map(setdiff, available_categories, present_categories)
    ) %>%
    dplyr::filter(lengths(missing_categories) > 0) %>%
    dplyr::select(-present_categories, -available_categories)

  if (nrow(group_missing_categories) > 0) {
    missing_preview <- group_missing_categories %>%
      dplyr::mutate(
        missing_categories = vapply(
          missing_categories,
          function(items) paste(items, collapse = ", "),
          character(1)
        ),
        group_key = apply(
          dplyr::select(., dplyr::all_of(group_cols)),
          1,
          function(row) paste(row, collapse = " | ")
        )
      ) %>%
      dplyr::select(group_key, missing_categories) %>%
      head(10)

    warning(
      paste(
        "Category scores missing for",
        nrow(group_missing_categories),
        "group(s); filling with global category averages.",
        "Examples (Country | tech | supply_chain -> missing categories):",
        paste(
          paste0(missing_preview$group_key, " -> ", missing_preview$missing_categories),
          collapse = "; "
        )
      )
    )
  }

  global_category_averages <- category_scores %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols[-1], "category")))) %>%
    dplyr::summarize(global_avg = mean(category_score, na.rm = TRUE), .groups = "drop")

  category_scores_complete <- category_scores %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::left_join(
      available_categories,
      by = group_cols[-1]
    ) %>%
    dplyr::left_join(
      category_scores,
      by = c(group_cols, "category")
    ) %>%
    dplyr::left_join(
      global_category_averages,
      by = c(group_cols[-1], "category")
    ) %>%
    dplyr::mutate(category_score = dplyr::coalesce(category_score, global_avg)) %>%
    dplyr::select(-global_avg) %>%
    dplyr::left_join(latest_years, by = group_cols)

  message("Computing overall economic opportunity index from weighted categories.")
  assert_unique_keys(
    category_scores_complete,
    c(group_cols, "category"),
    label = "economic_opportunity_category_scores_complete"
  )

  category_contributions <- category_scores_complete %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_contribution = category_score * weight / weight_sum)

  overall_variables <- names(index_definition$overall_variables)
  variable_contributions <- economic_opportunity_data %>%
    dplyr::filter(!variable %in% overall_variables) %>%
    dplyr::inner_join(
      category_scores_complete %>%
        dplyr::select(dplyr::all_of(c(group_cols, "category", "category_score"))),
      by = c(group_cols, "category")
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "category")))) %>%
    dplyr::mutate(variable_count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      category_weight = weight / weight_sum,
      variable_weight = category_weight / variable_count,
      weighted_contribution = value * variable_weight
    ) %>%
    dplyr::left_join(latest_years, by = group_cols) %>%
    dplyr::select(
      dplyr::all_of(group_cols),
      Year,
      category,
      variable,
      value,
      category_score,
      variable_count,
      category_weight,
      variable_weight,
      weighted_contribution
    )

  economic_opportunity_index <- category_scores_complete %>%
    dplyr::inner_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      economic_opportunity_index = sum(category_score * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(latest_years, by = group_cols) %>%
    dplyr::mutate(Year = as.integer(Year))

  list(
    category_scores = category_scores_complete,
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    index = economic_opportunity_index %>%
      dplyr::select(dplyr::all_of(c(group_cols, "Year", "economic_opportunity_index")))
  )
}
