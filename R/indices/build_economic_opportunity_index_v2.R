# Build Economic Opportunity index outputs (category scores + overall index) - v2.

if (!exists("normalize_sub_sector_or_keys", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "index_builder_core.R"))
}

standardize_economic_opportunity_inputs_v2 <- function(theme_tables, include_sub_sector = FALSE) {
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
        category = as.character(category),
        theme = as.character(theme_name)
      )
  }, theme_names, theme_tables)

  standardized <- dplyr::bind_rows(standardized)

  if (!"sub_sector" %in% names(standardized)) {
    standardized$sub_sector <- NA_character_
  }

  standardized <- standardized %>%
    dplyr::mutate(
      sub_sector = as.character(sub_sector),
      sub_sector = if (!isTRUE(include_sub_sector)) {
        "All"
      } else {
        dplyr::if_else(is.na(sub_sector) | sub_sector == "", "All", sub_sector)
      }
    )

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

validate_score_bounds <- function(scored_data, score_variables_tbl, include_sub_sector = FALSE) {
  score_values <- scored_data %>%
    dplyr::inner_join(score_variables_tbl, by = c("category", "variable" = "score_variable"))

  if (nrow(score_values) == 0) {
    return(invisible(NULL))
  }

  bounds_summary <- score_values %>%
    dplyr::group_by(category, variable) %>%
    dplyr::summarize(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      n_non_na = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      min = dplyr::if_else(is.infinite(min), NA_real_, min),
      max = dplyr::if_else(is.infinite(max), NA_real_, max)
    )

  out_of_bounds <- bounds_summary %>%
    dplyr::filter((!is.na(min) & min < 0) | (!is.na(max) & max > 1))

  if (nrow(out_of_bounds) == 0) {
    return(invisible(NULL))
  }

  example_rows <- score_values %>%
    dplyr::filter(
      (!is.na(value) & value < 0) | (!is.na(value) & value > 1)
    ) %>%
    dplyr::mutate(sub_sector = if (!"sub_sector" %in% names(score_values)) "All" else sub_sector) %>%
    dplyr::select(Country, tech, supply_chain, sub_sector, value) %>%
    dplyr::distinct() %>%
    dplyr::slice_head(n = 8)

  summary_text <- paste(capture.output(print(out_of_bounds)), collapse = "\n")
  example_text <- paste(capture.output(print(example_rows)), collapse = "\n")

  stop(
    "Economic opportunity score variables must be bounded in [0, 1].\n",
    "Summary:\n", summary_text, "\n",
    "Examples:\n", example_text,
    call. = FALSE
  )
}

build_economic_opportunity_index_v2 <- function(theme_tables,
                                                weights,
                                                missing_data = NULL,
                                                allow_partial_categories = TRUE,
                                                include_sub_sector = FALSE,
                                                index_definition = resolve_index_definition()) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Economic opportunity weights are missing or empty.")
  }

  score_variables <- index_definition$pillars$economic_opportunity$categories
  if (is.null(score_variables) || length(score_variables) == 0) {
    stop("Index definition missing economic opportunity category definitions.")
  }

  score_variables_tbl <- tibble::tibble(
    category = names(score_variables),
    score_variable = vapply(score_variables, function(x) x$score_variable, character(1))
  )

  economic_opportunity_data <- standardize_economic_opportunity_inputs_v2(
    theme_tables,
    include_sub_sector = include_sub_sector
  ) %>%
    dplyr::mutate(
      Year_raw = Year,
      Year = normalize_year(Year),
      value = suppressWarnings(as.numeric(value))
    )

  invalid_years <- economic_opportunity_data %>%
    dplyr::filter(is.na(Year) & !is.na(Year_raw))
  if (nrow(invalid_years) > 0) {
    examples <- invalid_years %>%
      dplyr::select(Year_raw) %>%
      dplyr::distinct() %>%
      dplyr::slice_head(n = 5)
    stop(
      "Economic opportunity Year normalization failed for some inputs. Examples:\n",
      paste(capture.output(print(examples)), collapse = "\n")
    )
  }

  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::mutate(Year = dplyr::if_else(is.na(Year), 0L, as.integer(Year)))

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

  economic_opportunity_data <- economic_opportunity_data %>%
    dplyr::select(-Year_raw)

  if (nrow(economic_opportunity_data) == 0) {
    stop(
      "Economic opportunity inputs are empty after Year normalization.",
      " Year class: ", paste(class(economic_opportunity_data$Year), collapse = ", ")
    )
  }

  validation_tbl <- economic_opportunity_data %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category, variable, data_type, Year) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

  extra_categories <- setdiff(unique(economic_opportunity_data$category), score_variables_tbl$category)
  if (length(extra_categories) > 0) {
    stop(
      "Economic opportunity inputs include unexpected categories: ",
      paste(extra_categories, collapse = ", ")
    )
  }

  validate_variable_levels(
    validation_tbl,
    index_definition = index_definition,
    include_sub_sector = include_sub_sector
  )

  require_columns(
    economic_opportunity_data,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "theme", "value"),
    label = "economic_opportunity_data"
  )

  group_cols <- normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)
  group_cols <- c(group_cols, "category", "variable", "theme")

  latest_tbl <- latest_by_group(economic_opportunity_data, group_cols = group_cols)

  year_provenance <- latest_tbl %>%
    dplyr::select(dplyr::all_of(c(group_cols, "Year"))) %>%
    dplyr::rename(Year_selected = Year)

  latest_tbl <- latest_tbl %>%
    dplyr::select(-Year)

  if (nrow(latest_tbl) == 0) {
    year_samples <- unique(economic_opportunity_data$Year)
    year_samples <- year_samples[seq_len(min(5, length(year_samples)))]
    year_sample_text <- if (length(year_samples) == 0) "none" else paste(year_samples, collapse = ", ")
    stop(
      "Latest-year filter returned 0 rows; check Year type/coercion. ",
      "Year classes: ", paste(class(economic_opportunity_data$Year), collapse = ", "),
      ". Sample Year values: ", year_sample_text, "."
    )
  }

  rules_tbl <- parse_missing_policy(missing_data, latest_tbl)

  imputed_tbl <- apply_missing_policy(
    latest_tbl,
    rules_tbl,
    include_sub_sector = include_sub_sector
  )

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
    dplyr::filter(is.na(score_variable))
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

  validate_score_bounds(scored_data, score_variables_tbl, include_sub_sector = include_sub_sector)

  category_scores <- compute_category_scores(
    scored_data,
    score_variables_tbl,
    include_sub_sector = include_sub_sector
  )

  if (nrow(category_scores) == 0) {
    stop("Economic opportunity category scores are empty after filtering to score variables.")
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
    allow_partial_categories = allow_partial_categories,
    include_sub_sector = include_sub_sector,
    pillar_label = "Economic opportunity",
    index_col = "Economic_Opportunity_Index"
  )

  diagnostics <- list(
    categories_configured = score_variables_tbl$category,
    categories_in_data = sort(unique(category_scores$category)),
    imputed_share_by_category = category_scores %>%
      dplyr::group_by(category) %>%
      dplyr::summarize(
        imputed_share = mean(imputed, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      ),
    year_provenance = year_provenance
  )

  economic_opportunity_index <- index_outputs$index_tbl %>%
    dplyr::select(-complete_categories)

  list(
    economic_opportunity_index = economic_opportunity_index,
    category_contributions = index_outputs$category_contributions,
    variable_contributions = index_outputs$variable_contributions,
    diagnostics = diagnostics,
    category_scores = category_scores,
    index = economic_opportunity_index
  )
}
