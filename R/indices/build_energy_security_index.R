# Build Energy Security index outputs (category scores + overall index).
standardize_energy_security_inputs <- function(theme_tables, include_sub_sector = FALSE) {
  theme_names <- names(theme_tables)
  if (is.null(theme_names)) {
    theme_names <- rep("unknown_theme", length(theme_tables))
  }

  standardized <- Map(function(theme_name, tbl) {
    if (is.null(tbl)) {
      return(NULL)
    }

    standardized_tbl <- standardize_theme_table(tbl) %>%
      dplyr::mutate(Year = 0L)
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

  dplyr::bind_rows(standardized)
}

parse_energy_security_years <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.POSIXlt(x)$year + 1900L)
  }

  values <- as.character(x)
  match_pos <- regexpr("\\d{4}", values)
  year_text <- ifelse(match_pos > 0, regmatches(values, match_pos), NA_character_)
  suppressWarnings(as.integer(year_text))
}

build_energy_security_index <- function(theme_tables,
                                        weights,
                                        missing_data = NULL,
                                        allow_partial_categories = T,
                                        include_sub_sector = FALSE,
                                        techs = NULL) {
  message("Building energy security index: standardizing theme inputs.")
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

  energy_security_data <- standardize_energy_security_inputs(
    theme_tables,
    include_sub_sector = include_sub_sector
  ) %>%
    apply_overall_definitions(
      index_definition = index_definition,
      include_sub_sector = include_sub_sector
    )

  energy_security_data <- energy_security_data %>%
    dplyr::filter(data_type == "index")

  if (!missing(techs) && !is.null(techs)) {
    pre_filter_count <- nrow(energy_security_data)
    energy_security_data <- energy_security_data %>%
      dplyr::filter(tech %in% techs)
    dropped_count <- pre_filter_count - nrow(energy_security_data)
    if (pre_filter_count > 0 && dropped_count > 0) {
      dropped_pct <- round(100 * dropped_count / pre_filter_count, 1)
      warning(
        paste0(
          "Tech filtering removed ", dropped_count, " rows (",
          dropped_pct, "%) from energy security inputs."
        )
      )
    }
  }

  energy_security_data <- energy_security_data %>%
    dplyr::mutate(
      Year_raw = Year,
      Year = parse_energy_security_years(Year_raw)
    )

  dropped_years <- sum(is.na(energy_security_data$Year))
  if (dropped_years > 0) {
    sample_values <- energy_security_data %>%
      dplyr::filter(is.na(Year)) %>%
      dplyr::pull(Year_raw) %>%
      unique() %>%
      head(5)
    sample_text <- if (length(sample_values) == 0) {
      "none"
    } else {
      paste(sample_values, collapse = ", ")
    }
    warning(
      paste(
        "Dropping", dropped_years,
        "energy security rows with invalid Year values after coercion.",
        "Sample Year values:", sample_text
      )
    )
  }

  energy_security_data <- energy_security_data %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::select(-Year_raw)

  validate_variable_levels(
    energy_security_data,
    index_definition = index_definition,
    include_sub_sector = include_sub_sector
  )

  message("Filtering energy security data to configured score variables.")
  expected_pairs <- score_variables_tbl %>%
    dplyr::distinct(category, score_variable)
  available_pairs <- energy_security_data %>%
    dplyr::distinct(category, variable)
  missing_pairs <- expected_pairs %>%
    dplyr::left_join(
      available_pairs,
      by = c("category" = "category", "score_variable" = "variable")
    ) %>%
    dplyr::filter(is.na(variable))
  if (nrow(missing_pairs) > 0) {
    missing_preview <- missing_pairs %>%
      dplyr::mutate(summary = paste(category, score_variable, sep = " | ")) %>%
      dplyr::pull(summary) %>%
      head(10)
    warning(
      paste(
        "Missing expected category/variable pairs in energy security inputs.",
        "Examples (category | score_variable):",
        paste(missing_preview, collapse = "; ")
      )
    )
  }
  energy_security_overall <- energy_security_data %>%
    dplyr::inner_join(score_variables_tbl, by = c("category", "variable" = "score_variable"))
  if (nrow(energy_security_overall) == 0) {
    available_summary <- energy_security_data %>%
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
      "Score-variable filter returned 0 rows; check index_definition score_variable names ",
      "vs available variables. Available variables per category: ",
      available_text
    )
  }

  weights_tbl <- tibble::tibble(
    category = names(weights),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  missing_data_tbl <- tibble::tibble(
    theme = names(missing_data),
    method = as.character(unlist(missing_data, use.names = FALSE))
  )

  message("Computing category-level scores.")
  group_cols <- c("Country", "tech", "supply_chain")
  if (include_sub_sector) {
    group_cols <- c(group_cols, "sub_sector")
  }
  category_group_cols <- c(group_cols, "Year", "category")
  theme_group_cols <- c(group_cols[-1], "Year", "category", "theme")

  require_columns(
    energy_security_overall,
    c("Country", "tech", "supply_chain", "category", "variable", "data_type", "Year", "theme", "value"),
    label = "energy_security_overall"
  )
  assert_unique_keys(
    energy_security_overall,
    c(theme_group_cols, "Country"),
    label = "energy_security_overall"
  )

  missing_data_tbl <- energy_security_overall %>%
    dplyr::distinct(theme) %>%
    dplyr::left_join(missing_data_tbl, by = "theme") %>%
    dplyr::mutate(method = dplyr::coalesce(method, "global_average"))

  invalid_methods <- missing_data_tbl %>%
    dplyr::filter(!method %in% c("zero", "global_average"))
  if (nrow(invalid_methods) > 0) {
    stop(
      "Unknown missing data methods: ",
      paste(unique(invalid_methods$method), collapse = ", ")
    )
  }

  theme_groups <- energy_security_overall %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(theme_group_cols)))

  country_groups <- energy_security_overall %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "Year"))))

  theme_country_grid <- theme_groups %>%
    dplyr::left_join(
      country_groups,
      by = c(group_cols[-1], "Year")
    )

  theme_global_averages <- energy_security_overall %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(theme_group_cols))) %>%
    dplyr::summarize(global_avg = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(global_avg = dplyr::if_else(is.nan(global_avg), NA_real_, global_avg))

  theme_overall_completed <- theme_country_grid %>%
    dplyr::left_join(
      energy_security_overall %>%
        dplyr::select(dplyr::all_of(c(theme_group_cols, "Country", "value"))),
      by = c(theme_group_cols, "Country")
    ) %>%
    dplyr::left_join(
      theme_global_averages,
      by = theme_group_cols
    ) %>%
    dplyr::left_join(missing_data_tbl, by = "theme") %>%
    dplyr::mutate(
      category_score = dplyr::case_when(
        !is.na(value) ~ value,
        method == "zero" ~ 0,
        method == "global_average" ~ global_avg,
        TRUE ~ global_avg
      )
    )

  missing_summary <- theme_overall_completed %>%
    dplyr::filter(is.na(value)) %>%
    dplyr::count(theme, method, category)

  if (nrow(missing_summary) > 0) {
    missing_preview <- missing_summary %>%
      dplyr::mutate(summary = paste(theme, category, method, sep = " | ")) %>%
      dplyr::pull(summary) %>%
      head(10)

    missing_message <- paste(
      "Missing theme scores detected; imputing using configured methods.",
      "Examples (theme | category | method):",
      paste(missing_preview, collapse = "; ")
    )
    if (isTRUE(allow_partial_categories)) {
      warning(missing_message)
    } else {
      stop(missing_message)
    }
  }

  category_scores <- theme_overall_completed %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(category_group_cols))) %>%
    dplyr::summarize(category_score = mean(category_score, na.rm = TRUE), .groups = "drop")

  latest_years <- category_scores %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      Year = if (all(is.na(Year))) NA_integer_ else max(Year, na.rm = TRUE),
      .groups = "drop"
    )

  category_scores_latest <- category_scores %>%
    dplyr::inner_join(
      latest_years %>% dplyr::filter(!is.na(Year)),
      by = c(group_cols, "Year")
    )

  if (nrow(category_scores_latest) == 0) {
    year_samples <- unique(category_scores$Year)
    year_samples <- year_samples[seq_len(min(5, length(year_samples)))]
    year_sample_text <- if (length(year_samples) == 0) "none" else paste(year_samples, collapse = ", ")
    stop(
      "Latest-year filter returned 0 rows; check Year type/coercion. ",
      "Year classes: ", paste(class(category_scores$Year), collapse = ", "),
      ". Sample Year values: ", year_sample_text, "."
    )
  }

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

  message("Computing overall energy security index from weighted categories.")
  category_contributions <- category_scores_latest %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Year")))) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_contribution = category_score * weight / weight_sum)

  overall_variables <- names(index_definition$overall_variables)
  variable_contributions <- energy_security_data %>%
    dplyr::filter(!variable %in% overall_variables) %>%
    dplyr::inner_join(
      category_scores_latest %>%
        dplyr::select(dplyr::all_of(c(group_cols, "Year", "category", "category_score"))),
      by = c(group_cols, "Year", "category")
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Year", "category")))) %>%
    dplyr::mutate(variable_count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Year")))) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      category_weight = weight / weight_sum,
      variable_weight = category_weight / variable_count,
      weighted_contribution = value * variable_weight
    ) %>%
    dplyr::select(
      dplyr::all_of(c(
        group_cols,
        "Year",
        "category",
        "variable",
        "value",
        "category_score",
        "variable_count",
        "category_weight",
        "variable_weight",
        "weighted_contribution"
      ))
    )

  energy_security_index <- category_scores_latest %>%
    dplyr::inner_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Year")))) %>%
    dplyr::summarize(
      energy_security_index = sum(category_score * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols[-1]))) %>%
    dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols[-1]))) %>%
    dplyr::mutate(energy_security_index = median_scurve(energy_security_index)) %>%
    dplyr::ungroup()

  list(
    category_scores = category_scores_latest %>%
      dplyr::select(dplyr::all_of(c(group_cols, "Year", "category", "category_score"))),
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    index = energy_security_index %>%
      dplyr::select(dplyr::all_of(c(group_cols, "Year", "energy_security_index")))
  )
}
