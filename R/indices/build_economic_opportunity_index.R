# Build Economic Opportunity index outputs (category scores + overall index).
standardize_economic_opportunity_inputs <- function(theme_tables, include_sub_sector = FALSE) {
  theme_names <- names(theme_tables)
  if (is.null(theme_names)) {
    warning("Economic opportunity inputs missing theme names; assigning 'unknown_theme'.")
    theme_names <- rep("unknown_theme", length(theme_tables))
  } else if (any(!nzchar(theme_names))) {
    warning("Economic opportunity inputs include blank theme names; assigning 'unknown_theme'.")
    theme_names <- ifelse(nzchar(theme_names), theme_names, "unknown_theme")
  }

  standardized <- Map(function(theme_name, tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }
    standardized_tbl <- standardize_theme_table(tbl)
    standardized_tbl <- standardize_bind_rows_inputs(standardized_tbl)
    if ("Year" %in% names(standardized_tbl)) {
      standardized_tbl <- standardized_tbl %>%
        dplyr::mutate(Year = dplyr::if_else(is.na(Year), 0L, Year))
    }
    validate_schema(standardized_tbl)
    standardized_tbl <- standardized_tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        category = as.character(category),
        theme = as.character(theme_name)
      )
    if (!"sub_sector" %in% names(standardized_tbl)) {
      standardized_tbl$sub_sector <- NA_character_
    }
    standardized_tbl <- standardized_tbl %>%
      dplyr::mutate(
        sub_sector = as.character(sub_sector),
        sub_sector = if (!isTRUE(include_sub_sector)) {
          "All"
        } else {
          dplyr::if_else(is.na(sub_sector) | sub_sector == "", "All", sub_sector)
        }
      )
    if (!isTRUE(include_sub_sector) && nrow(standardized_tbl) > 0) {
      standardized_tbl <- standardized_tbl %>%
        dplyr::group_by(
          Country,
          tech,
          supply_chain,
          sub_sector,
          category,
          theme,
          variable,
          data_type,
          Year,
          source,
          explanation
        ) %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
    }
    standardized_tbl
  }, theme_names, theme_tables)

  dplyr::bind_rows(standardized)
}

if (!exists("normalize_year", mode = "function")) {
  normalize_year <- function(x) {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      return(as.POSIXlt(x)$year + 1900L)
    }

    if (inherits(x, "labelled")) {
      x <- as.numeric(x)
    }

    if (is.numeric(x)) {
      return(as.integer(x))
    }

    values <- as.character(x)
    match_pos <- regexpr("\\d{4}", values)
    year_text <- ifelse(match_pos > 0, regmatches(values, match_pos), NA_character_)
    suppressWarnings(as.integer(year_text))
  }
}

if (!exists("latest_by_group", mode = "function")) {
  latest_by_group <- function(tbl, group_cols) {
    latest_years <- tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(
        Year = if (all(is.na(Year))) NA_integer_ else max(Year, na.rm = TRUE),
        .groups = "drop"
      )

    tbl %>%
      dplyr::inner_join(latest_years, by = c(group_cols, "Year"))
  }
}

if (!exists("append_sub_sector", mode = "function")) {
  append_sub_sector <- function(base_cols, include_sub_sector) {
    if (isTRUE(include_sub_sector)) {
      c(base_cols, "sub_sector")
    } else {
      base_cols
    }
  }
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
  assert_unique_keys(
    score_variables_tbl,
    c("category", "score_variable"),
    label = "economic_opportunity_score_variables"
  )

  economic_opportunity_data <- standardize_economic_opportunity_inputs(
    theme_tables,
    include_sub_sector = include_sub_sector
  )

  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::mutate(
      Year_raw = Year,
      Year = normalize_year(Year),
      value = suppressWarnings(as.numeric(value))
    )

  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::mutate(Year = dplyr::if_else(is.na(Year), 0L, Year))

  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::filter(data_type == "index")

  if (!isTRUE(include_sub_sector)) {
    if ("sub_sector" %in% names(economic_opportunity_data)) {
      unique_sub_sectors <- unique(economic_opportunity_data$sub_sector)
      if (length(unique_sub_sectors) != 1 || !identical(unique_sub_sectors, "All")) {
        stop("Expected sub_sector to be 'All' when include_sub_sector is FALSE.")
      }
    }
  }

  if (!missing(techs) && !is.null(techs)) {
    economic_opportunity_data <- economic_opportunity_data %>%
      dplyr::filter(tech %in% techs)
  }

  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::select(-Year_raw) %>%
    apply_overall_definitions(
      index_definition = index_definition,
      include_sub_sector = include_sub_sector
    )
  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::group_by(
      Country,
      tech,
      supply_chain,
      sub_sector,
      category,
      variable,
      data_type,
      Year,
      theme
    ) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
  assert_unique_keys(
    economic_opportunity_data,
    c("Country", "tech", "supply_chain", "sub_sector", "category", "variable", "data_type", "Year", "theme"),
    label = "economic_opportunity_inputs"
  )

  if (nrow(economic_opportunity_data) == 0) {
    stop(
      "Economic opportunity inputs are empty after Year normalization.",
      " Year class: ", paste(class(economic_opportunity_data$Year), collapse = ", ")
    )
  }

  validation_tbl <- economic_opportunity_data %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category, variable, data_type, Year, theme) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

  if (nrow(economic_opportunity_data) == 0) {
    stop(
      "Economic opportunity inputs are empty after Year normalization.",
      " Year class: ", paste(class(economic_opportunity_data$Year), collapse = ", ")
    )
  }

  validation_tbl <- economic_opportunity_data %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category, variable, data_type, Year) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

  validate_variable_levels(
    validation_tbl,
    index_definition = index_definition,
    include_sub_sector = include_sub_sector
  )

  unexpected_categories <- setdiff(
    unique(economic_opportunity_data$category),
    score_variables_tbl$category
  )
  if (length(unexpected_categories) > 0) {
    warning(
      "Economic opportunity inputs include categories not in index_definition: ",
      paste(sort(unexpected_categories), collapse = ", ")
    )
  }

  require_columns(
    economic_opportunity_data,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "theme", "value"),
    label = "economic_opportunity_data"
  )
  group_cols <- append_sub_sector(c("Country", "tech", "supply_chain"), include_sub_sector)

  latest_group_cols <- c(group_cols, "category", "variable", "theme")
  economic_opportunity_data <- latest_by_group(
    economic_opportunity_data,
    group_cols = latest_group_cols
  )

  if (nrow(economic_opportunity_data) == 0) {
    year_samples <- unique(economic_opportunity_data$Year)
    year_samples <- year_samples[seq_len(min(5, length(year_samples)))]
    year_sample_text <- if (length(year_samples) == 0) "none" else paste(year_samples, collapse = ", ")
    stop(
      "Latest-year filter returned 0 rows; check Year type/coercion. ",
      "Year classes: ", paste(class(economic_opportunity_data$Year), collapse = ", "),
      ". Sample Year values: ", year_sample_text, "."
    )
  }

  message("Filtering economic opportunity data to configured score variables only.")
  available_pairs <- economic_opportunity_data %>%
    dplyr::distinct(category, variable)
  missing_pairs <- score_variables_tbl %>%
    dplyr::left_join(available_pairs, by = c("category" = "category", "score_variable" = "variable")) %>%
    dplyr::filter(is.na(variable))
  if (nrow(missing_pairs) > 0) {
    available_summary <- economic_opportunity_data %>%
      dplyr::distinct(category, variable) %>%
      dplyr::group_by(category) %>%
      dplyr::summarize(
        variables = paste(sort(unique(variable)), collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::mutate(summary = paste0(category, ": ", variables)) %>%
      dplyr::pull(summary) %>%
      head(10)
    available_text <- if (length(available_summary) == 0) {
      "none"
    } else {
      paste(available_summary, collapse = "; ")
    }
    stop(
      "Score-variable filter returned missing category/variable pairs; check index_definition score_variable names ",
      "vs available variables. Available variables per category: ",
      available_text
    )
  }
  economic_opportunity_overall <- economic_opportunity_data %>%
    dplyr::inner_join(score_variables_tbl, by = c("category", "variable" = "score_variable"))

  require_columns(
    economic_opportunity_overall,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "theme", "value"),
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
    dplyr::summarize(category_score = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(category_score = dplyr::if_else(is.nan(category_score), NA_real_, category_score))
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

  if (nrow(weights_tbl) == 0) {
    stop("No economic opportunity categories remain after filtering to available data and weights.")
  }

  expected_categories <- sort(unique(weights_tbl$category))
  category_completeness <- category_scores %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "category")))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      present_categories = list(unique(category)),
      present_count = dplyr::n_distinct(category),
      .groups = "drop"
    ) %>%
    dplyr::mutate(complete_categories = present_count == length(expected_categories))

  category_scores_complete <- category_scores %>%
    dplyr::left_join(latest_years, by = group_cols)

  message("Computing overall economic opportunity index from weighted categories.")
  assert_unique_keys(
    category_scores_complete,
    c(group_cols, "category"),
    label = "economic_opportunity_category_scores_complete"
  )

  category_contributions <- category_scores_complete %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::left_join(category_completeness, by = group_cols) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(
      weight_sum = sum(weight, na.rm = TRUE),
      category_weight = weight / weight_sum
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_contribution = category_score * category_weight) %>%
    dplyr::select(-present_categories, -present_count)

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
    dplyr::left_join(category_completeness, by = group_cols) %>%
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
      weighted_contribution,
      complete_categories
    )

  if (!isTRUE(allow_partial_categories)) {
    category_contributions <- category_contributions %>%
      dplyr::mutate(
        category_weight = dplyr::if_else(complete_categories, category_weight, NA_real_),
        weighted_contribution = dplyr::if_else(complete_categories, weighted_contribution, NA_real_)
      )

    variable_contributions <- variable_contributions %>%
      dplyr::mutate(
        category_weight = dplyr::if_else(complete_categories, category_weight, NA_real_),
        weighted_contribution = dplyr::if_else(complete_categories, weighted_contribution, NA_real_)
      )
  }

  economic_opportunity_index <- category_contributions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      complete_categories = all(complete_categories),
      economic_opportunity_index = dplyr::if_else(
        !isTRUE(allow_partial_categories) & !complete_categories,
        NA_real_,
        sum(weighted_contribution, na.rm = TRUE)
      ),
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
