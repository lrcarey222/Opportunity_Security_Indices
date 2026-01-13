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
    if (include_sub_sector && !"sub_sector" %in% names(tbl)) {
      tbl$sub_sector <- "All"
    }
    tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        sub_sector = if ("sub_sector" %in% names(tbl)) as.character(sub_sector) else "All",
        category = as.character(category),
        theme = as.character(theme_name),
        Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
        value = suppressWarnings(as.numeric(value))
      )
  }, theme_names, theme_tables)

  dplyr::bind_rows(standardized)
}

build_energy_security_index <- function(theme_tables,
                                        weights,
                                        missing_data = NULL,
                                        allow_partial_categories = T,
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
  message("Building energy security index: standardizing theme inputs.")
  if (is.null(weights) || length(weights) == 0) {
    stop("Energy security weights are missing or empty.")
  }

  energy_security_data <- standardize_energy_security_inputs(
    theme_tables,
    include_sub_sector = include_sub_sector
  ) %>%
    dplyr::filter(data_type == "index", tech %in% techs)

  message("Filtering energy security data to Overall variables only.")
  energy_security_overall <- energy_security_data %>%
    dplyr::filter(grepl("Overall", variable)) %>%
    dplyr::filter(!(category == "Trade" & variable != "Overall Trade Risk Index"))

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
    dplyr::summarize(global_avg = mean(value, na.rm = TRUE), .groups = "drop")

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

    warning(
      paste(
        "Missing theme scores detected; imputing using configured methods.",
        "Examples (theme | category | method):",
        paste(missing_preview, collapse = "; ")
      )
    )
  }

  category_scores <- theme_overall_completed %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(category_group_cols))) %>%
    dplyr::summarize(category_score = mean(category_score, na.rm = TRUE), .groups = "drop")

  latest_years <- category_scores %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(Year = max(Year, na.rm = TRUE), .groups = "drop")

  category_scores_latest <- category_scores %>%
    dplyr::inner_join(
      latest_years,
      by = c(group_cols, "Year")
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

  message("Computing overall energy security index from weighted categories.")
  category_contributions <- category_scores_latest %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Year")))) %>%
    dplyr::mutate(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_contribution = category_score * weight / weight_sum)

  variable_contributions <- energy_security_data %>%
    dplyr::filter(!grepl("Overall", variable)) %>%
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
