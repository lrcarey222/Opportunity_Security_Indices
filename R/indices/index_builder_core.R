# Shared helpers for index builders (v2).

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

normalize_sub_sector_or_keys <- function(base_cols, include_sub_sector) {
  if (isTRUE(include_sub_sector)) {
    c(base_cols, "sub_sector")
  } else {
    base_cols
  }
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

apply_missing_policy <- function(tbl, rules_tbl, include_sub_sector = FALSE) {
  group_cols <- normalize_sub_sector_or_keys(c("tech", "supply_chain"), include_sub_sector)
  group_cols <- c(group_cols, "category", "variable", "theme")

  country_groups <- tbl %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(
      normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)
    )))

  variable_groups <- tbl %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))

  grid <- variable_groups %>%
    dplyr::left_join(
      country_groups,
      by = normalize_sub_sector_or_keys(c("tech", "supply_chain"), include_sub_sector)
    )

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

compute_category_scores <- function(score_values_tbl,
                                    score_variables_tbl,
                                    include_sub_sector = FALSE) {
  group_cols <- normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)

  score_values_tbl %>%
    dplyr::inner_join(score_variables_tbl, by = c("category", "variable" = "score_variable")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "category")))) %>%
    dplyr::summarize(
      category_score = mean(value, na.rm = TRUE),
      imputed = any(isTRUE(imputed)),
      missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      category_score = dplyr::if_else(is.nan(category_score), NA_real_, category_score),
      missing_rule_applied = dplyr::na_if(missing_rule_applied, ""),
      sub_sector = if (!isTRUE(include_sub_sector)) "All" else sub_sector
    )
}

compute_index_and_contributions <- function(category_scores,
                                            variable_contributions,
                                            weights_tbl,
                                            allow_partial_categories = TRUE,
                                            include_sub_sector = FALSE,
                                            pillar_label = "Index",
                                            index_col = "Index") {
  key_cols <- normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)

  categories_in_data <- sort(unique(category_scores$category))
  categories_in_weights <- sort(unique(weights_tbl$category))

  extra_config_categories <- setdiff(categories_in_weights, categories_in_data)
  if (length(extra_config_categories) > 0) {
    warning(
      pillar_label, " weights include categories not present in theme output: ",
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
    stop("No ", tolower(pillar_label), " categories remain after filtering to available data and weights.")
  }

  expected_categories <- sort(unique(weights_tbl$category))

  category_completeness <- category_scores %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key_cols, "category")))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarize(
      present_categories = list(unique(category)),
      present_count = dplyr::n_distinct(category),
      .groups = "drop"
    ) %>%
    dplyr::mutate(complete_categories = present_count == length(expected_categories))

  category_contributions <- category_scores %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::left_join(category_completeness, by = key_cols) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::mutate(
      weight_sum = sum(weight, na.rm = TRUE),
      category_weight = weight / weight_sum
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_category_contribution = category_score * category_weight) %>%
    dplyr::select(-present_categories, -present_count)

  variable_contributions <- variable_contributions %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::left_join(category_completeness, by = key_cols) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::mutate(
      weight_sum = sum(weight, na.rm = TRUE),
      category_weight = weight / weight_sum,
      weighted_variable_contribution = component_value * component_weight_within_overall * category_weight
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::all_of(key_cols),
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

  index_tbl <- category_contributions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarize(
      complete_categories = all(complete_categories),
      index_value = dplyr::if_else(
        !isTRUE(allow_partial_categories) & !complete_categories,
        NA_real_,
        sum(weighted_category_contribution, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(!!index_col := index_value)

  list(
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    index_tbl = index_tbl
  )
}
