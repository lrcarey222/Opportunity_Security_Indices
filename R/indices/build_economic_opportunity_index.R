# Build Economic Opportunity index outputs (category scores + overall index).
standardize_economic_opportunity_inputs <- function(theme_tables) {
  standardized <- lapply(theme_tables, function(tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }
    standardized_tbl <- standardize_theme_table(tbl)
    standardized_tbl <- standardize_bind_rows_inputs(standardized_tbl)
    validate_schema(standardized_tbl)
    standardized_tbl
  })

  dplyr::bind_rows(standardized)
}

build_economic_opportunity_index <- function(theme_tables,
                                             weights,
                                             allow_partial_categories = TRUE,
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

  economic_opportunity_data <- standardize_economic_opportunity_inputs(theme_tables) %>%
    dplyr::filter(data_type == "index", tech %in% techs)

  message("Filtering economic opportunity data to Overall variables only.")
  economic_opportunity_overall <- economic_opportunity_data %>%
    dplyr::filter(grepl("Overall", variable))

  require_columns(
    economic_opportunity_overall,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "value"),
    label = "economic_opportunity_overall"
  )
  assert_unique_keys(
    economic_opportunity_overall,
    c("Country", "tech", "supply_chain", "category", "variable", "Year"),
    label = "economic_opportunity_overall"
  )

  weights_tbl <- tibble::tibble(
    category = names(weights),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )
  assert_unique_keys(weights_tbl, "category", label = "economic_opportunity_weights")

  message("Computing category-level scores.")
  category_scores <- economic_opportunity_overall %>%
    dplyr::group_by(Country, tech, supply_chain, category) %>%
    dplyr::summarize(category_score = mean(value, na.rm = TRUE), .groups = "drop")
  assert_unique_keys(
    category_scores,
    c("Country", "tech", "supply_chain", "category"),
    label = "economic_opportunity_category_scores"
  )

  latest_years <- economic_opportunity_overall %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarize(Year = as.integer(max(Year, na.rm = TRUE)), .groups = "drop")
  assert_unique_keys(
    latest_years,
    c("Country", "tech", "supply_chain"),
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
    dplyr::distinct(tech, supply_chain, category)
  assert_unique_keys(
    available_categories,
    c("tech", "supply_chain", "category"),
    label = "economic_opportunity_available_categories"
  )

  available_by_group <- available_categories %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarize(
      available_categories = list(unique(category)),
      .groups = "drop"
    )

  group_missing_categories <- category_scores %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarize(
      present_categories = list(unique(category)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(available_by_group, by = c("tech", "supply_chain")) %>%
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
        group_key = paste(Country, tech, supply_chain, sep = " | ")
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
    dplyr::group_by(tech, supply_chain, category) %>%
    dplyr::summarize(global_avg = mean(category_score, na.rm = TRUE), .groups = "drop")

  category_scores_complete <- category_scores %>%
    dplyr::distinct(Country, tech, supply_chain) %>%
    dplyr::left_join(
      available_categories,
      by = c("tech", "supply_chain")
    ) %>%
    dplyr::left_join(
      category_scores,
      by = c("Country", "tech", "supply_chain", "category")
    ) %>%
    dplyr::left_join(
      global_category_averages,
      by = c("tech", "supply_chain", "category")
    ) %>%
    dplyr::mutate(category_score = dplyr::coalesce(category_score, global_avg)) %>%
    dplyr::select(-global_avg) %>%
    dplyr::left_join(latest_years, by = c("Country", "tech", "supply_chain"))

  message("Computing overall economic opportunity index from weighted categories.")
  assert_unique_keys(
    category_scores_complete,
    c("Country", "tech", "supply_chain", "category"),
    label = "economic_opportunity_category_scores_complete"
  )

  category_contributions <- category_scores_complete %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_contribution = category_score * weight / weight_sum)

  variable_contributions <- economic_opportunity_data %>%
    dplyr::filter(!grepl("Overall", variable)) %>%
    dplyr::inner_join(
      category_scores_complete %>%
        dplyr::select(Country, tech, supply_chain, category, category_score),
      by = c("Country", "tech", "supply_chain", "category")
    ) %>%
    dplyr::group_by(Country, tech, supply_chain, category) %>%
    dplyr::mutate(variable_count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      category_weight = weight / weight_sum,
      variable_weight = category_weight / variable_count,
      weighted_contribution = value * variable_weight
    ) %>%
    dplyr::left_join(latest_years, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
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
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarize(
      economic_opportunity_index = sum(category_score * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(latest_years, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::mutate(Year = as.integer(Year))

  list(
    category_scores = category_scores_complete,
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    index = economic_opportunity_index %>%
      dplyr::select(Country, tech, supply_chain, Year, economic_opportunity_index)
  )
}
