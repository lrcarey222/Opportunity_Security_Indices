# Build Energy Security index outputs (category scores + overall index) - v2.

if (!exists("normalize_sub_sector_or_keys", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "index_builder_core.R"))
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
        category = as.character(category),
        theme = as.character(theme_name)
      )
  }, theme_names, theme_tables)

  standardized <- dplyr::bind_rows(standardized)

  if (nrow(standardized) > 0 && "theme" %in% names(standardized)) {
    global_energy_prices <- standardized %>%
      dplyr::filter(theme == "energy_prices", Country == "Global")

    if (nrow(global_energy_prices) > 0) {
      country_reference <- standardized %>%
        dplyr::filter(Country != "Global") %>%
        dplyr::distinct(Country)

      if (nrow(country_reference) > 0) {
        expanded_energy_prices <- global_energy_prices %>%
          dplyr::select(-Country) %>%
          tidyr::crossing(country_reference)

        standardized <- standardized %>%
          dplyr::filter(!(theme == "energy_prices" & Country == "Global")) %>%
          dplyr::bind_rows(expanded_energy_prices)
      }
    }
  }

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

  invalid_years <- energy_security_data %>%
    dplyr::filter(is.na(Year) & !is.na(Year_raw))
  if (nrow(invalid_years) > 0) {
    examples <- invalid_years %>%
      dplyr::select(Year_raw) %>%
      dplyr::distinct() %>%
      dplyr::slice_head(n = 5)
    stop(
      "Energy security Year normalization failed for some inputs. Examples:\n",
      paste(capture.output(print(examples)), collapse = "\n")
    )
  }

  energy_security_data <- energy_security_data %>%
    dplyr::mutate(Year = dplyr::if_else(is.na(Year), 0L, as.integer(Year)))

  energy_security_data <- energy_security_data %>%
    dplyr::filter(data_type == "index")

  if (!isTRUE(include_sub_sector)) {
    if ("sub_sector" %in% names(energy_security_data)) {
      unique_sub_sectors <- unique(energy_security_data$sub_sector)
      if (length(unique_sub_sectors) != 1 || !identical(unique_sub_sectors, "All")) {
        stop("Expected sub_sector to be 'All' when include_sub_sector is FALSE.")
      }
    }
  }

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

  extra_categories <- setdiff(unique(energy_security_data$category), score_variables_tbl$category)
  if (length(extra_categories) > 0) {
    stop(
      "Energy security inputs include unexpected categories: ",
      paste(extra_categories, collapse = ", ")
    )
  }

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

  group_cols <- normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)
  group_cols <- c(group_cols, "category", "variable", "theme")

  latest_tbl <- latest_by_group(energy_security_data, group_cols = group_cols)

  year_provenance <- latest_tbl %>%
    dplyr::select(dplyr::all_of(c(group_cols, "Year"))) %>%
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

  # A configured category whose score variable was never built (an overall whose components
  # name variables the theme does not emit) silently scored nothing and dropped out of the
  # index. The former check joined on its own key and could never fire; this one warns,
  # naming the category, the missing variable and what the category's inputs do carry. The
  # second check catches the same defect when a theme's same-named composite stands in.
  unproduced_score_variables <- find_unproduced_score_variables(score_variables_tbl, scored_data)
  warn_unproduced_score_variables(unproduced_score_variables, pillar_label = "Energy security")
  substituted_score_variables <- find_substituted_score_variables(
    score_variables_tbl, index_definition, overall_names, scored_data
  )
  warn_substituted_score_variables(substituted_score_variables, pillar_label = "Energy security")

  category_scores <- compute_category_scores(
    scored_data,
    score_variables_tbl,
    include_sub_sector = include_sub_sector
  )

  if (nrow(category_scores) == 0) {
    stop("Energy security category scores are empty after filtering to score variables.")
  }

  direct_variable_contributions <- direct_score_contributions(
    scored_data,
    score_variables_tbl,
    overall_names
  )

  component_contributions <- normalize_component_contributions(
    component_contributions,
    include_sub_sector = include_sub_sector,
    score_variables_tbl = score_variables_tbl
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
    pillar_label = "Energy security",
    index_col = "Energy_Security_Index",
    absent_categories_reported = unproduced_score_variables$category
  )

  # imputed_share: share of category scores with at least one imputed component.
  # unscored_groups / index_cells: country-cells and tech x stage cells left without any
  # observed category (NA index), and cells whose index is constant across countries.
  # index_imputation: share of each index value's weight on imputed components.
  # unobserved_components: component groups no country is observed in, left NA.
  diagnostics <- list(
    categories_configured = score_variables_tbl$category,
    categories_in_data = sort(unique(category_scores$category[!is.na(category_scores$category_score)])),
    categories_unproduced = unproduced_score_variables,
    categories_substituted = substituted_score_variables,
    imputed_share = mean(category_scores$imputed, na.rm = TRUE),
    imputed_share_by_category = category_scores %>%
      dplyr::group_by(category) %>%
      dplyr::summarize(
        imputed_share = mean(imputed, na.rm = TRUE),
        observed_share = mean(!is.na(category_score)),
        n = dplyr::n(),
        .groups = "drop"
      ),
    unscored_groups = index_outputs$unscored_groups,
    index_cells = summarize_index_cells(
      index_outputs$index_tbl,
      index_col = "Energy_Security_Index",
      include_sub_sector = include_sub_sector
    ),
    index_imputation = summarize_index_imputation(
      index_outputs$variable_contributions,
      include_sub_sector = include_sub_sector
    ),
    unobserved_components = unobserved_component_groups(imputed_tbl, include_sub_sector = include_sub_sector),
    year_provenance = year_provenance
  )

  energy_security_index <- index_outputs$index_tbl %>%
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
