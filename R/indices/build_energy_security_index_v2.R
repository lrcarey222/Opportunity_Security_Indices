# Build Energy Security index outputs (category scores + overall index) - v2.

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

standardize_energy_security_inputs_v2 <- function(theme_tables, include_sub_sector = FALSE) {
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
    if ("Year" %in% names(standardized_tbl)) {
      standardized_tbl <- standardized_tbl %>%
        dplyr::mutate(Year = dplyr::if_else(is.na(Year), 0L, Year))
    }
    validate_schema(standardized_tbl)

    standardized_tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        sub_sector = if (isTRUE(include_sub_sector) && "sub_sector" %in% names(standardized_tbl)) {
          as.character(sub_sector)
        } else {
          "All"
        },
        category = as.character(category),
        theme = as.character(theme_name)
      )
  }, theme_names, theme_tables)

  standardized <- dplyr::bind_rows(standardized)

  if (!isTRUE(include_sub_sector) && nrow(standardized) > 0) {
    standardized <- standardized %>%
      dplyr::group_by(
        Country,
        tech,
        supply_chain,
        sub_sector,
        category,
        variable,
        data_type,
        Year,
        theme,
        source,
        explanation
      ) %>%
      dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
  }

  standardized
}

parse_missing_policy <- function(missing_data, data_tbl) {
  if (is.null(missing_data) || length(missing_data) == 0) {
    return(tibble::tibble())
  }

  rules_tbl <- tibble::tibble(
    key = names(missing_data),
    method = as.character(unlist(missing_data, use.names = FALSE))
  )

  invalid_methods <- rules_tbl %>%
    dplyr::filter(!method %in% c("zero", "global_average"))
  if (nrow(invalid_methods) > 0) {
    stop(
      "Unknown missing data methods: ",
      paste(unique(invalid_methods$method), collapse = ", ")
    )
  }

  available_variables <- unique(data_tbl$variable)
  available_categories <- unique(data_tbl$category)
  available_themes <- unique(data_tbl$theme)

  rules_tbl <- rules_tbl %>%
    dplyr::mutate(
      scope = dplyr::case_when(
        key %in% available_variables ~ "variable",
        key %in% available_categories ~ "category",
        key %in% available_themes ~ "theme",
        TRUE ~ "unknown"
      )
    )

  unknown_rules <- rules_tbl %>%
    dplyr::filter(scope == "unknown")
  if (nrow(unknown_rules) > 0) {
    warning(
      "Missing data rules referenced unknown variables/categories/themes: ",
      paste(unique(unknown_rules$key), collapse = ", ")
    )
  }

  rules_tbl
}

apply_missing_policy <- function(tbl, rules_tbl) {
  group_cols <- c("tech", "supply_chain", "sub_sector", "category", "variable", "theme")

  country_groups <- tbl %>%
    dplyr::distinct(Country, tech, supply_chain, sub_sector)

  variable_groups <- tbl %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))

  grid <- variable_groups %>%
    dplyr::left_join(country_groups, by = c("tech", "supply_chain", "sub_sector"))

  completed <- grid %>%
    dplyr::left_join(tbl, by = c("Country", group_cols))

  if (nrow(rules_tbl) > 0) {
    variable_rules <- rules_tbl %>%
      dplyr::filter(scope == "variable") %>%
      dplyr::select(key, method)
    category_rules <- rules_tbl %>%
      dplyr::filter(scope == "category") %>%
      dplyr::select(key, method)
    theme_rules <- rules_tbl %>%
      dplyr::filter(scope == "theme") %>%
      dplyr::select(key, method)

    completed <- completed %>%
      dplyr::left_join(variable_rules, by = c("variable" = "key")) %>%
      dplyr::rename(method_variable = method) %>%
      dplyr::left_join(category_rules, by = c("category" = "key")) %>%
      dplyr::rename(method_category = method) %>%
      dplyr::left_join(theme_rules, by = c("theme" = "key")) %>%
      dplyr::rename(method_theme = method) %>%
      dplyr::mutate(
        missing_method = dplyr::coalesce(method_variable, method_category, method_theme)
      ) %>%
      dplyr::select(-method_variable, -method_category, -method_theme)
  } else {
    completed <- completed %>%
      dplyr::mutate(missing_method = NA_character_)
  }

  global_stats <- completed %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      global_avg = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(global_avg = dplyr::if_else(is.nan(global_avg), NA_real_, global_avg))

  completed %>%
    dplyr::left_join(global_stats, by = group_cols) %>%
    dplyr::mutate(value_raw = value) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        !is.na(value_raw) ~ value_raw,
        missing_method == "zero" ~ 0,
        missing_method == "global_average" ~ global_avg,
        TRUE ~ value_raw
      ),
      imputed = is.na(value_raw) & !is.na(missing_method) & !is.na(value),
      missing_rule_applied = dplyr::if_else(imputed, missing_method, NA_character_)
    ) %>%
    dplyr::select(-global_avg, -value_raw)
}

compute_overall_scores <- function(tbl, index_definition, include_sub_sector = FALSE) {
  if (is.null(index_definition) || is.null(index_definition$overall_variables)) {
    return(list(overall_scores = tibble::tibble(), component_contributions = tibble::tibble()))
  }

  overall_defs <- index_definition$overall_variables
  overall_names <- names(overall_defs)
  if (is.null(overall_names) || length(overall_names) == 0) {
    return(list(overall_scores = tibble::tibble(), component_contributions = tibble::tibble()))
  }

  variable_levels <- index_definition$variable_levels

  overall_scores <- list()
  component_contributions <- list()

  for (overall_name in overall_names) {
    def <- overall_defs[[overall_name]]
    if (is.null(def$category)) {
      next
    }

    level <- NULL
    if (!is.null(variable_levels) && !is.null(variable_levels[[overall_name]])) {
      level <- variable_levels[[overall_name]]
    }
    group_cols <- variable_level_columns(level, include_sub_sector = include_sub_sector)
    group_cols <- group_cols[group_cols %in% names(tbl)]
    group_cols <- setdiff(group_cols, "Year")

    components <- def$components
    if (is.null(components)) {
      next
    }

    component_tbl <- tbl %>%
      dplyr::filter(category == def$category)

    if (length(components) == 1 && identical(components[[1]], "*")) {
      component_names <- setdiff(unique(component_tbl$variable), overall_names)
    } else {
      component_names <- components
    }

    if (length(component_names) == 0) {
      next
    }

    component_tbl <- component_tbl %>%
      dplyr::filter(variable %in% component_names)

    if (nrow(component_tbl) == 0) {
      next
    }

    aggregation <- if (!is.null(def$aggregation)) def$aggregation else "mean"
    summary_fn <- switch(
      aggregation,
      mean = function(x) mean(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      stop("Unknown aggregation rule: ", aggregation)
    )

    component_tbl <- component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "variable")))) %>%
      dplyr::summarize(
        value = mean(value, na.rm = TRUE),
        imputed = any(isTRUE(imputed)),
        missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
        .groups = "drop"
      ) %>%
      dplyr::mutate(missing_rule_applied = dplyr::na_if(missing_rule_applied, ""))

    component_counts <- component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(
        component_count = dplyr::n(),
        .groups = "drop"
      )

    overall_tbl <- component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(
        value = summary_fn(value),
        imputed = any(isTRUE(imputed)),
        missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
        .groups = "drop"
      ) %>%
      dplyr::left_join(component_counts, by = group_cols) %>%
      dplyr::mutate(
        variable = overall_name,
        category = def$category,
        theme = "overall_definition",
        data_type = "index",
        missing_rule_applied = dplyr::na_if(missing_rule_applied, "")
      ) %>%
      dplyr::mutate(value = dplyr::if_else(is.nan(value), NA_real_, value))

    component_tbl <- component_tbl %>%
      dplyr::left_join(component_counts, by = group_cols) %>%
      dplyr::mutate(
        component_weight_within_overall = 1 / component_count,
        value = dplyr::if_else(is.nan(value), NA_real_, value)
      )

    overall_scores[[overall_name]] <- overall_tbl
    component_contributions[[overall_name]] <- component_tbl
  }

  list(
    overall_scores = dplyr::bind_rows(overall_scores),
    component_contributions = dplyr::bind_rows(component_contributions)
  )
}

compute_category_scores <- function(score_values_tbl, score_variables_tbl) {
  score_values_tbl %>%
    dplyr::inner_join(score_variables_tbl, by = c("category", "variable" = "score_variable")) %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category) %>%
    dplyr::summarize(
      category_score = mean(value, na.rm = TRUE),
      imputed = any(isTRUE(imputed)),
      missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      category_score = dplyr::if_else(is.nan(category_score), NA_real_, category_score),
      missing_rule_applied = dplyr::na_if(missing_rule_applied, "")
    )
}

compute_index_and_contributions <- function(category_scores,
                                            variable_contributions,
                                            weights_tbl,
                                            allow_partial_categories = TRUE) {
  categories_in_data <- sort(unique(category_scores$category))
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
    missing_message <- paste(
      "Theme categories are missing weights and will be excluded:",
      paste(missing_weight_categories, collapse = ", ")
    )
    if (isTRUE(allow_partial_categories)) {
      warning(missing_message)
    } else {
      stop(missing_message)
    }
  }

  weights_tbl <- weights_tbl %>%
    dplyr::filter(category %in% categories_in_data)

  if (nrow(weights_tbl) == 0) {
    stop("No energy security categories remain after filtering to available data and weights.")
  }

  expected_categories <- sort(unique(weights_tbl$category))

  category_completeness <- category_scores %>%
    dplyr::distinct(Country, tech, supply_chain, sub_sector, category) %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector) %>%
    dplyr::summarize(
      present_categories = list(unique(category)),
      present_count = dplyr::n_distinct(category),
      .groups = "drop"
    ) %>%
    dplyr::mutate(complete_categories = present_count == length(expected_categories))

  category_contributions <- category_scores %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::left_join(category_completeness, by = c("Country", "tech", "supply_chain", "sub_sector")) %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector) %>%
    dplyr::mutate(
      weight_sum = sum(weight, na.rm = TRUE),
      category_weight = weight / weight_sum
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_category_contribution = category_score * category_weight) %>%
    dplyr::select(-present_categories, -present_count)

  variable_contributions <- variable_contributions %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::left_join(category_completeness, by = c("Country", "tech", "supply_chain", "sub_sector")) %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector) %>%
    dplyr::mutate(
      weight_sum = sum(weight, na.rm = TRUE),
      category_weight = weight / weight_sum,
      weighted_variable_contribution = component_value * component_weight_within_overall * category_weight
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      sub_sector,
      category,
      variable,
      component_value,
      component_weight_within_overall,
      category_weight,
      weighted_variable_contribution,
      missing_rule_applied,
      imputed
    )

  if (!isTRUE(allow_partial_categories)) {
    category_contributions <- category_contributions %>%
      dplyr::mutate(
        category_weight = dplyr::if_else(complete_categories, category_weight, NA_real_),
        weighted_category_contribution = dplyr::if_else(complete_categories, weighted_category_contribution, NA_real_)
      )

    variable_contributions <- variable_contributions %>%
      dplyr::mutate(
        category_weight = dplyr::if_else(complete_categories, category_weight, NA_real_),
        weighted_variable_contribution = dplyr::if_else(complete_categories, weighted_variable_contribution, NA_real_)
      )
  }

  energy_security_index <- category_contributions %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector) %>%
    dplyr::summarize(
      complete_categories = all(complete_categories),
      Energy_Security_Index = dplyr::if_else(
        !isTRUE(allow_partial_categories) & !complete_categories,
        NA_real_,
        sum(weighted_category_contribution, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  list(
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    energy_security_index = energy_security_index
  )
}

build_energy_security_index_v2 <- function(theme_tables,
                                           weights,
                                           missing_data = NULL,
                                           allow_partial_categories = TRUE,
                                           include_sub_sector = FALSE,
                                           techs = NULL) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Energy security weights are missing or empty.")
  }

  index_definition <- resolve_index_definition()
  score_variables <- index_definition$pillars$energy_security$categories
  if (is.null(score_variables) || length(score_variables) == 0) {
    stop("Index definition missing energy security category definitions.")
  }

  score_variables_tbl <- tibble::tibble(
    category = names(score_variables),
    score_variable = vapply(score_variables, function(x) x$score_variable, character(1))
  )

  energy_security_data <- standardize_energy_security_inputs_v2(
    theme_tables,
    include_sub_sector = include_sub_sector
  ) %>%
    dplyr::mutate(
      Year_raw = Year,
      Year = normalize_year(Year),
      value = suppressWarnings(as.numeric(value))
    )

  energy_security_data <- energy_security_data %>%
    dplyr::mutate(Year = dplyr::if_else(is.na(Year), 0L, Year))

  energy_security_data <- energy_security_data %>%
    dplyr::filter(data_type == "index")

  if (!missing(techs) && !is.null(techs)) {
    energy_security_data <- energy_security_data %>%
      dplyr::filter(tech %in% techs)
  }

  energy_security_data <- energy_security_data %>%
    dplyr::select(-Year_raw)

  if (nrow(energy_security_data) == 0) {
    stop(
      "Energy security inputs are empty after Year normalization.",
      " Year class: ", paste(class(energy_security_data$Year), collapse = ", ")
    )
  }

  validation_tbl <- energy_security_data %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category, variable, data_type, Year) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

  validate_variable_levels(
    validation_tbl,
    index_definition = index_definition,
    include_sub_sector = include_sub_sector
  )

  require_columns(
    energy_security_data,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "theme", "value"),
    label = "energy_security_data"
  )

  latest_tbl <- latest_by_group(
    energy_security_data,
    group_cols = c("Country", "tech", "supply_chain", "sub_sector", "category", "variable", "theme")
  )

  year_provenance <- latest_tbl %>%
    dplyr::select(Country, tech, supply_chain, sub_sector, category, variable, theme, Year) %>%
    dplyr::rename(Year_selected = Year)

  latest_tbl <- latest_tbl %>%
    dplyr::select(-Year)

  if (nrow(latest_tbl) == 0) {
    year_samples <- unique(energy_security_data$Year)
    year_samples <- year_samples[seq_len(min(5, length(year_samples)))]
    year_sample_text <- if (length(year_samples) == 0) "none" else paste(year_samples, collapse = ", ")
    stop(
      "Latest-year filter returned 0 rows; check Year type/coercion. ",
      "Year classes: ", paste(class(energy_security_data$Year), collapse = ", "),
      ". Sample Year values: ", year_sample_text, "."
    )
  }

  rules_tbl <- parse_missing_policy(missing_data, latest_tbl)

  imputed_tbl <- apply_missing_policy(latest_tbl, rules_tbl)

  overall_outputs <- compute_overall_scores(
    imputed_tbl,
    index_definition = index_definition,
    include_sub_sector = include_sub_sector
  )

  overall_scores <- overall_outputs$overall_scores
  component_contributions <- overall_outputs$component_contributions

  overall_names <- unique(overall_scores$variable)

  score_variable_data <- imputed_tbl %>%
    dplyr::filter(!variable %in% overall_names)

  scored_data <- dplyr::bind_rows(score_variable_data, overall_scores)

  available_pairs <- scored_data %>%
    dplyr::distinct(category, variable)
  missing_pairs <- score_variables_tbl %>%
    dplyr::left_join(available_pairs, by = c("category" = "category", "score_variable" = "variable")) %>%
    dplyr::filter(is.na(variable))
  if (nrow(missing_pairs) > 0) {
    available_summary <- scored_data %>%
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

  category_scores <- compute_category_scores(scored_data, score_variables_tbl)

  if (nrow(category_scores) == 0) {
    stop("Energy security category scores are empty after filtering to score variables.")
  }

  direct_score_variables <- score_variables_tbl %>%
    dplyr::filter(!score_variable %in% overall_names)

  direct_variable_contributions <- scored_data %>%
    dplyr::inner_join(direct_score_variables, by = c("category", "variable" = "score_variable")) %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category, variable) %>%
    dplyr::summarize(
      component_value = mean(value, na.rm = TRUE),
      imputed = any(isTRUE(imputed)),
      missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      component_weight_within_overall = 1,
      component_value = dplyr::if_else(is.nan(component_value), NA_real_, component_value),
      missing_rule_applied = dplyr::na_if(missing_rule_applied, "")
    )

  component_contributions <- component_contributions %>%
    dplyr::rename(component_value = value) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      sub_sector,
      category,
      variable,
      component_value,
      component_weight_within_overall,
      missing_rule_applied,
      imputed
    )

  variable_contributions <- dplyr::bind_rows(
    component_contributions,
    direct_variable_contributions
  )

  weights_tbl <- tibble::tibble(
    category = names(weights),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  index_outputs <- compute_index_and_contributions(
    category_scores,
    variable_contributions,
    weights_tbl,
    allow_partial_categories = allow_partial_categories
  )

  diagnostics <- list(
    categories_configured = score_variables_tbl$category,
    categories_in_data = sort(unique(category_scores$category)),
    imputed_share = mean(category_scores$imputed, na.rm = TRUE),
    year_provenance = year_provenance
  )

  energy_security_index <- index_outputs$energy_security_index %>%
    dplyr::select(-complete_categories)

  list(
    energy_security_index = energy_security_index,
    category_contributions = index_outputs$category_contributions,
    variable_contributions = index_outputs$variable_contributions,
    diagnostics = diagnostics,
    category_scores = category_scores,
    index = energy_security_index
  )
}
