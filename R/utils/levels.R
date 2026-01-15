# Helpers for variable level validation and config-driven overall formulas.

resolve_index_definition <- function(index_definition = NULL, allow_null = FALSE) {
  if (is.null(index_definition)) {
    index_definition <- getOption("opportunity_security.index_definition")
  }
  if (is.null(index_definition) && !allow_null) {
    stop("Index definition not loaded; run scripts/00_setup.R first.")
  }
  index_definition
}

variable_level_columns <- function(level, include_sub_sector = FALSE) {
  base_cols <- c("Country", "Year", "category")
  if (is.null(level)) {
    return(c(base_cols, "tech", "supply_chain"))
  }

  if (level == "country") {
    return(base_cols)
  }
  if (level == "country_tech") {
    return(c(base_cols, "tech"))
  }
  if (level == "country_tech_supply_chain") {
    return(c(base_cols, "tech", "supply_chain"))
  }
  if (level == "country_tech_supply_chain_sub_sector") {
    return(c(base_cols, "tech", "supply_chain", "sub_sector"))
  }

  stop("Unknown variable level: ", level)
}

apply_overall_definitions <- function(tbl,
                                      index_definition = NULL,
                                      include_sub_sector = FALSE) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(tbl)
  }

  index_definition <- resolve_index_definition(index_definition, allow_null = TRUE)
  if (is.null(index_definition) || is.null(index_definition$overall_variables)) {
    return(tbl)
  }

  overall_defs <- index_definition$overall_variables
  overall_names <- names(overall_defs)
  if (is.null(overall_names) || length(overall_names) == 0) {
    return(tbl)
  }

  variable_levels <- index_definition$variable_levels

  new_overalls <- lapply(overall_names, function(overall_name) {
    def <- overall_defs[[overall_name]]
    if (is.null(def$category)) {
      return(NULL)
    }

    level <- NULL
    if (!is.null(variable_levels) && !is.null(variable_levels[[overall_name]])) {
      level <- variable_levels[[overall_name]]
    }
    group_cols <- variable_level_columns(level, include_sub_sector = include_sub_sector)
    group_cols <- group_cols[group_cols %in% names(tbl)]

    components <- def$components
    if (is.null(components)) {
      return(NULL)
    }

    component_tbl <- tbl %>%
      dplyr::filter(category == def$category, data_type == "index")

    if (length(components) == 1 && identical(components[[1]], "*")) {
      component_names <- setdiff(unique(component_tbl$variable), overall_names)
    } else {
      component_names <- components
    }

    if (length(component_names) == 0) {
      return(NULL)
    }

    component_tbl <- component_tbl %>%
      dplyr::filter(variable %in% component_names)

    if (nrow(component_tbl) == 0) {
      return(NULL)
    }

    aggregation <- if (!is.null(def$aggregation)) def$aggregation else "mean"
    summary_fn <- switch(
      aggregation,
      mean = function(x) mean(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      stop("Unknown aggregation rule: ", aggregation)
    )

    component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(value = summary_fn(value), .groups = "drop") %>%
      dplyr::mutate(
        variable = overall_name,
        data_type = "index",
        source = dplyr::coalesce(def$source, "Author calculation"),
        explanation = dplyr::coalesce(def$explanation, "Author calculation across category indices")
      ) %>%
      dplyr::select(dplyr::all_of(c(group_cols, "variable", "data_type", "value", "source", "explanation")))
  })

  new_overalls <- dplyr::bind_rows(new_overalls)
  if (nrow(new_overalls) == 0) {
    return(tbl)
  }

  if (!"tech" %in% names(new_overalls) && "tech" %in% names(tbl)) {
    new_overalls$tech <- "All"
  }
  if (!"supply_chain" %in% names(new_overalls) && "supply_chain" %in% names(tbl)) {
    new_overalls$supply_chain <- "All"
  }
  if (!"sub_sector" %in% names(new_overalls) && "sub_sector" %in% names(tbl)) {
    new_overalls$sub_sector <- "All"
  }

  computed_overalls <- unique(new_overalls$variable)
  base_tbl <- tbl %>%
    dplyr::filter(!(data_type == "index" & variable %in% computed_overalls))

  dplyr::bind_rows(
    standardize_bind_rows_inputs(base_tbl),
    standardize_bind_rows_inputs(new_overalls)
  )
}

validate_variable_levels <- function(tbl,
                                     index_definition = NULL,
                                     include_sub_sector = FALSE) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(invisible(tbl))
  }

  index_definition <- resolve_index_definition(index_definition, allow_null = TRUE)
  if (is.null(index_definition) || is.null(index_definition$variable_levels)) {
    return(invisible(tbl))
  }

  variable_levels <- index_definition$variable_levels
  variables_to_check <- intersect(unique(tbl$variable), names(variable_levels))

  for (var_name in variables_to_check) {
    level <- variable_levels[[var_name]]
    key_cols <- variable_level_columns(level, include_sub_sector = include_sub_sector)
    key_cols <- intersect(key_cols, names(tbl))
    key_cols <- c(key_cols, "variable", "data_type")

    assert_unique_keys(
      tbl %>% dplyr::filter(variable == var_name),
      key_cols,
      label = paste("variable level", var_name)
    )
  }

  invisible(tbl)
}
