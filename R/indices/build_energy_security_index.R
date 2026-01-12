# Build Energy Security index outputs (category scores + overall index).
standardize_energy_security_inputs <- function(theme_tables) {
  standardized <- lapply(theme_tables, function(tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }
    tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        category = as.character(category),
        Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
        value = suppressWarnings(as.numeric(value))
      )
  })

  dplyr::bind_rows(standardized)
}

build_energy_security_index <- function(theme_tables,
                                        weights,
                                        allow_partial_categories = FALSE,
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
  message("Building energy security index: standardizing theme inputs.")
  if (is.null(weights) || length(weights) == 0) {
    stop("Energy security weights are missing or empty.")
  }

  energy_security_data <- standardize_energy_security_inputs(theme_tables) %>%
    dplyr::filter(data_type == "index", tech %in% techs)

  message("Filtering energy security data to Overall variables only.")
  energy_security_overall <- energy_security_data %>%
    dplyr::filter(grepl("Overall", variable))

  weights_tbl <- tibble::tibble(
    category = names(weights),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  message("Computing category-level scores.")
  category_scores <- energy_security_overall %>%
    dplyr::group_by(Country, tech, supply_chain, Year, category) %>%
    dplyr::summarize(category_score = mean(value, na.rm = TRUE), .groups = "drop")

  latest_years <- category_scores %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarize(Year = max(Year, na.rm = TRUE), .groups = "drop")

  category_scores_latest <- category_scores %>%
    dplyr::inner_join(
      latest_years,
      by = c("Country", "tech", "supply_chain", "Year")
    )

  categories_in_data <- sort(unique(category_scores_latest$category))
  categories_in_weights <- sort(unique(weights_tbl$category))

  extra_config_categories <- setdiff(categories_in_weights, categories_in_data)
  if (length(extra_config_categories) > 0) {
    warning(
      "Energy security weights include categories not present in theme output: ",
      paste(extra_config_categories, collapse = ", ")
    )
  }

  missing_weight_categories <- setdiff(categories_in_data, categories_in_weights)
  if (length(missing_weight_categories) > 0) {
    warning(
      "Theme categories are missing weights and will be excluded: ",
      paste(missing_weight_categories, collapse = ", ")
    )
  }

  weights_tbl <- weights_tbl %>%
    dplyr::filter(category %in% categories_in_data)

  expected_categories <- weights_tbl$category
  group_missing_categories <- category_scores_latest %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::summarize(
      missing_categories = list(setdiff(expected_categories, unique(category))),
      .groups = "drop"
    ) %>%
    dplyr::filter(lengths(missing_categories) > 0)

  if (nrow(group_missing_categories) > 0) {
    missing_preview <- group_missing_categories %>%
      dplyr::mutate(
        missing_categories = vapply(
          missing_categories,
          function(items) paste(items, collapse = ", "),
          character(1)
        )
      ) %>%
      dplyr::mutate(
        group_key = paste(Country, tech, supply_chain, Year, sep = " | ")
      ) %>%
      dplyr::select(group_key, missing_categories) %>%
      head(10)

    missing_message <- paste(
      "Category scores missing for",
      nrow(group_missing_categories),
      "group(s).",
      "Examples (Country | tech | supply_chain | Year -> missing categories):",
      paste(
        paste0(missing_preview$group_key, " -> ", missing_preview$missing_categories),
        collapse = "; "
      )
    )

    if (isTRUE(allow_partial_categories)) {
      warning(missing_message)
    } else {
      stop(
        missing_message,
        " Set allow_partial_categories: true in config to permit partial weighting."
      )
    }
  }

  message("Computing overall energy security index from weighted categories.")
  category_contributions <- category_scores_latest %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_contribution = category_score * weight / weight_sum)

  variable_contributions <- energy_security_data %>%
    dplyr::filter(!grepl("Overall", variable)) %>%
    dplyr::inner_join(
      category_scores_latest %>%
        dplyr::select(Country, tech, supply_chain, Year, category, category_score),
      by = c("Country", "tech", "supply_chain", "Year", "category")
    ) %>%
    dplyr::group_by(Country, tech, supply_chain, Year, category) %>%
    dplyr::mutate(variable_count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      category_weight = weight / weight_sum,
      variable_weight = category_weight / variable_count,
      weighted_contribution = value * variable_weight
    ) %>%
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

  energy_security_index <- category_scores_latest %>%
    dplyr::inner_join(weights_tbl, by = "category") %>%
    dplyr::group_by(Country, tech, supply_chain, Year) %>%
    dplyr::summarize(
      energy_security_index = sum(category_score * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    category_scores = category_scores_latest %>%
      dplyr::select(Country, tech, supply_chain, Year, category, category_score),
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    index = energy_security_index %>%
      dplyr::select(Country, tech, supply_chain, Year, energy_security_index)
  )
}
