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

parse_missing_policy <- function(missing_data, data_tbl, known_keys = NULL) {
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
  known_keys <- unique(c(known_keys, available_variables, available_categories, available_themes))

  rules_tbl <- rules_tbl %>%
    dplyr::mutate(
      scope = dplyr::case_when(
        key %in% available_variables ~ "variable",
        key %in% available_categories ~ "category",
        key %in% available_themes ~ "theme",
        key %in% known_keys ~ "known_but_absent",
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

  assert_unique_keys(
    country_groups,
    normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector),
    label = "missing policy country groups"
  )
  assert_unique_keys(
    variable_groups,
    group_cols,
    label = "missing policy variable groups"
  )

  grid <- variable_groups %>%
    dplyr::left_join(
      country_groups,
      by = normalize_sub_sector_or_keys(c("tech", "supply_chain"), include_sub_sector),
      relationship = "many-to-many"
    )

  completed <- grid %>%
    dplyr::left_join(tbl, by = c("Country", group_cols))

  # Grid-created rows have no sub_sector when it is not a key. The Trade overalls group by
  # sub_sector, so an NA there would split one country's components across two groups;
  # give them the same "All" placeholder the builders use for observed rows.
  if (!isTRUE(include_sub_sector) && "sub_sector" %in% names(completed)) {
    completed <- completed %>%
      dplyr::mutate(sub_sector = dplyr::coalesce(sub_sector, "All"))
  }

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
      n_observed = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(global_avg = dplyr::if_else(is.nan(global_avg), NA_real_, global_avg))

  # A rule fills a country the source did not cover, judged against the countries it did
  # cover in the same (tech, stage, category, variable, theme) group. When the source covers
  # no country in the group there is nothing to judge against: a "zero" rule would score
  # every country 0 and manufacture a component with no information in it. Such groups stay
  # NA, so the available-case rule in compute_index_and_contributions() drops them.
  completed %>%
    dplyr::left_join(global_stats, by = group_cols) %>%
    dplyr::mutate(value_raw = value) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        !is.na(value_raw) ~ value_raw,
        n_observed == 0 ~ value_raw,
        missing_method == "zero" ~ 0,
        missing_method == "global_average" ~ global_avg,
        TRUE ~ value_raw
      ),
      imputed = is.na(value_raw) & !is.na(missing_method) & !is.na(value),
      missing_rule_applied = dplyr::if_else(imputed, missing_method, NA_character_)
    ) %>%
    dplyr::select(-global_avg, -n_observed, -value_raw)
}

# (tech, stage, category, variable, theme) groups in which no country carries a value after
# the missing-data policy ran, i.e. the source covers nobody there. apply_missing_policy()
# leaves them NA rather than inventing a value; this lists them for the diagnostics.
unobserved_component_groups <- function(imputed_tbl, include_sub_sector = FALSE) {
  group_cols <- c(
    normalize_sub_sector_or_keys(c("tech", "supply_chain"), include_sub_sector),
    "category", "variable", "theme"
  )

  imputed_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      n_countries = dplyr::n_distinct(Country),
      n_scored = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_scored == 0) %>%
    dplyr::select(-n_scored)
}

# Zero-row outputs carry the columns the v2 builders select and rename, so an index
# definition with no matching overall variables yields no contributions rather than
# a "column doesn't exist" error downstream.
empty_overall_outputs <- function() {
  keys <- tibble::tibble(
    Country = character(),
    tech = character(),
    supply_chain = character(),
    sub_sector = character(),
    category = character(),
    variable = character(),
    value = numeric(),
    imputed = logical(),
    missing_rule_applied = character(),
    component_count = integer()
  )
  list(
    overall_scores = keys %>%
      dplyr::mutate(theme = character(), data_type = character()),
    component_contributions = keys %>%
      dplyr::mutate(component_weight_within_overall = numeric(), overall_variable = character())
  )
}

# The weight each component carries in its overall, chosen so that
# sum(weight * value) reproduces the overall exactly. NA components carry 0: the overall is
# taken over the components that are present, so they do not enter it.
#   mean   - 1 / (number of present components) each
#   median - the middle order statistic (odd count) or the two middle ones at 1/2 each
overall_component_weights <- function(value, aggregation = "mean") {
  observed <- !is.na(value)
  n_observed <- sum(observed)
  weights <- numeric(length(value))
  if (n_observed == 0) {
    return(weights)
  }

  if (identical(aggregation, "mean")) {
    weights[observed] <- 1 / n_observed
    return(weights)
  }

  ordered <- which(observed)[order(value[observed])]
  middle <- if (n_observed %% 2 == 1) {
    ordered[(n_observed + 1) / 2]
  } else {
    ordered[c(n_observed / 2, n_observed / 2 + 1)]
  }
  weights[middle] <- 1 / length(middle)
  weights
}

# Bring compute_overall_scores()'s component_contributions onto the shape the index builders
# bind into variable_contributions.
#
# That table arrives shaped by compute_overall_scores()'s own grouping, and both v2 builders
# used to rename `value` and select `sub_sector` from it unconditionally. Two shapes broke
# that, and either aborted the whole index build:
#
#   * a zero-column tibble, returned whenever the index definition carries no overall
#     variables or none of them matched any data - "Column `value` doesn't exist"
#   * no sub_sector column, because variable_level_columns() only groups by it when
#     include_sub_sector is TRUE - "Column `sub_sector` doesn't exist"
#
# The "All" placeholder is the same one compute_category_scores() uses when sub_sector is
# not being scored, so the two sides of the later bind_rows() agree.
#
# compute_overall_scores() builds every overall variable whose category has data, including
# overalls that are not the pillar's score variable (the ES pillar also builds EO's
# "Overall Trade Index" from the shared Trade rows; EO builds "Overall Reserves Index" from
# the potential themes). Their components are not part of the index, and keeping them made
# variable_contributions sum to more than it. Passing score_variables_tbl keeps only the
# components of each category's configured score variable, recorded as `score_variable`.
COMPONENT_CONTRIBUTION_COLUMNS <- c(
  "Country", "tech", "supply_chain", "sub_sector", "category", "score_variable", "variable",
  "component_value", "component_weight_within_overall", "missing_rule_applied", "imputed"
)

normalize_component_contributions <- function(component_contributions,
                                              include_sub_sector = FALSE,
                                              score_variables_tbl = NULL) {
  if (is.null(component_contributions) || nrow(component_contributions) == 0) {
    return(tibble::tibble(
      Country = character(),
      tech = character(),
      supply_chain = character(),
      sub_sector = character(),
      category = character(),
      score_variable = character(),
      variable = character(),
      component_value = numeric(),
      component_weight_within_overall = numeric(),
      missing_rule_applied = character(),
      imputed = logical()
    ))
  }

  if (!"component_value" %in% names(component_contributions) &&
      "value" %in% names(component_contributions)) {
    component_contributions <- dplyr::rename(component_contributions, component_value = value)
  }

  if (!"sub_sector" %in% names(component_contributions) && !isTRUE(include_sub_sector)) {
    component_contributions$sub_sector <- "All"
  }

  if (!"score_variable" %in% names(component_contributions)) {
    component_contributions$score_variable <- if ("overall_variable" %in% names(component_contributions)) {
      component_contributions$overall_variable
    } else {
      NA_character_
    }
  }

  if (!is.null(score_variables_tbl)) {
    component_contributions <- component_contributions %>%
      dplyr::semi_join(score_variables_tbl, by = c("category", "score_variable"))
  }

  # Anything still missing is a real defect upstream, so let all_of() report it.
  component_contributions %>%
    dplyr::select(dplyr::all_of(COMPONENT_CONTRIBUTION_COLUMNS))
}

# Contribution rows for score variables that are theme variables rather than configured
# overalls (weight 1 within their category when present, 0 when NA). Shared by both v2
# builders so their variable_contributions are built the same way.
direct_score_contributions <- function(scored_data, score_variables_tbl, overall_names) {
  direct_score_variables <- score_variables_tbl %>%
    dplyr::filter(!score_variable %in% overall_names)

  scored_data %>%
    dplyr::inner_join(direct_score_variables, by = c("category", "variable" = "score_variable")) %>%
    dplyr::group_by(Country, tech, supply_chain, sub_sector, category, variable) %>%
    dplyr::summarize(
      component_value = mean(value, na.rm = TRUE),
      imputed = any(imputed %in% TRUE),
      missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      score_variable = variable,
      component_value = dplyr::if_else(is.nan(component_value), NA_real_, component_value),
      component_weight_within_overall = dplyr::if_else(is.na(component_value), 0, 1),
      missing_rule_applied = dplyr::na_if(missing_rule_applied, "")
    )
}

# Configured categories whose score variable never materialized, typically because an
# overall's components name a variable the theme does not emit. Returns one row per such
# category with the variables its inputs do carry, so the warning can point at the fix.
find_unproduced_score_variables <- function(score_variables_tbl, scored_data, max_inputs = 8) {
  missing <- score_variables_tbl %>%
    dplyr::anti_join(
      dplyr::distinct(scored_data, category, variable),
      by = c("category", "score_variable" = "variable")
    )

  if (nrow(missing) == 0) {
    return(missing %>% dplyr::mutate(category_inputs = character()))
  }

  inputs <- scored_data %>%
    dplyr::filter(category %in% missing$category) %>%
    dplyr::distinct(category, variable) %>%
    dplyr::group_by(category) %>%
    dplyr::summarize(
      category_inputs = {
        vars <- sort(unique(variable))
        text <- paste(utils::head(vars, max_inputs), collapse = ", ")
        if (length(vars) > max_inputs) paste0(text, ", ... (", length(vars), " in all)") else text
      },
      .groups = "drop"
    )

  missing %>%
    dplyr::left_join(inputs, by = "category")
}

warn_unproduced_score_variables <- function(missing_tbl, pillar_label) {
  if (is.null(missing_tbl) || nrow(missing_tbl) == 0) {
    return(invisible(character()))
  }

  details <- paste0(
    missing_tbl$category,
    " (score variable '", missing_tbl$score_variable, "' was not built; ",
    dplyr::if_else(
      is.na(missing_tbl$category_inputs),
      "no input rows carry this category",
      paste0("its inputs carry: ", missing_tbl$category_inputs)
    ),
    ")"
  )
  warning(
    pillar_label, ": ", nrow(missing_tbl), " configured ",
    if (nrow(missing_tbl) == 1) "category is NOT scored and drops" else "categories are NOT scored and drop",
    " out of the index - check the overall's components in ",
    "config/index_definition.yml against the variable names the theme emits: ",
    paste(details, collapse = "; "),
    call. = FALSE
  )
  invisible(missing_tbl$category)
}

# The same defect, masked. Most themes also emit their own "Overall ... Index" rows under the
# configured overall's name (rebuild_theme_overall_indices()). When the configured overall is
# not built - its components match no input variable - those theme rows survive as a direct
# score variable, so the category still scores, but from the theme's composite rather than
# the configured definition, and find_unproduced_score_variables() sees nothing wrong.
find_substituted_score_variables <- function(score_variables_tbl, index_definition, built_overalls, scored_data) {
  configured_overalls <- names(index_definition$overall_variables)

  score_variables_tbl %>%
    dplyr::filter(score_variable %in% configured_overalls, !score_variable %in% built_overalls) %>%
    dplyr::semi_join(
      dplyr::distinct(scored_data, category, variable),
      by = c("category", "score_variable" = "variable")
    )
}

warn_substituted_score_variables <- function(substituted_tbl, pillar_label) {
  if (is.null(substituted_tbl) || nrow(substituted_tbl) == 0) {
    return(invisible(character()))
  }

  warning(
    pillar_label, ": configured overall not built from its components in ",
    "config/index_definition.yml (none of them is in the inputs), so the theme's own variable ",
    "of the same name is scored instead - check the components against the variable names ",
    "the theme emits: ",
    paste0(substituted_tbl$category, " ('", substituted_tbl$score_variable, "')", collapse = "; "),
    call. = FALSE
  )
  invisible(substituted_tbl$category)
}

compute_overall_scores <- function(tbl, index_definition, include_sub_sector = FALSE) {
  if (is.null(index_definition) || is.null(index_definition$overall_variables)) {
    return(empty_overall_outputs())
  }

  overall_defs <- index_definition$overall_variables
  overall_names <- names(overall_defs)
  if (is.null(overall_names) || length(overall_names) == 0) {
    return(empty_overall_outputs())
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
    required_group_cols <- setdiff(group_cols, "Year")
    missing_group_cols <- setdiff(required_group_cols, names(tbl))
    if (length(missing_group_cols) > 0) {
      stop(
        "Missing required grouping columns for overall variable '",
        overall_name,
        "': ",
        paste(missing_group_cols, collapse = ", ")
      )
    }
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

    # `imputed` is a vector here, and isTRUE() of anything longer than one element is FALSE,
    # so the former any(isTRUE(imputed)) flagged only single-row groups.
    component_tbl <- component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "variable")))) %>%
      dplyr::summarize(
        value = mean(value, na.rm = TRUE),
        imputed = any(imputed %in% TRUE),
        missing_rule_applied = paste(sort(unique(stats::na.omit(missing_rule_applied))), collapse = "; "),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        value = dplyr::if_else(is.nan(value), NA_real_, value),
        missing_rule_applied = dplyr::na_if(missing_rule_applied, "")
      )

    # component_count is the number of components actually present (non-NA), i.e. the ones
    # the overall is taken over; it used to count NA components too, which understated
    # every present component's share.
    component_counts <- component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(
        component_count = sum(!is.na(value)),
        .groups = "drop"
      )

    overall_tbl <- component_tbl %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarize(
        imputed = any(imputed %in% TRUE),
        value = summary_fn(value),
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
      dplyr::mutate(overall_variable = overall_name)

    component_tbl <- if (identical(aggregation, "mean")) {
      component_tbl %>%
        dplyr::mutate(
          component_weight_within_overall = dplyr::if_else(is.na(value), 0, 1 / component_count)
        )
    } else {
      component_tbl %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::mutate(
          component_weight_within_overall = overall_component_weights(value, aggregation)
        ) %>%
        dplyr::ungroup()
    }

    # Grouping drops sub_sector when it is not a key; restore it as "All", matching
    # compute_category_scores(), so the builders can select it.
    if (!"sub_sector" %in% group_cols) {
      overall_tbl <- overall_tbl %>% dplyr::mutate(sub_sector = "All")
      component_tbl <- component_tbl %>% dplyr::mutate(sub_sector = "All")
    }

    overall_scores[[overall_name]] <- overall_tbl
    component_contributions[[overall_name]] <- component_tbl
  }

  # The per-overall tables are collected in a *named* list. bind_rows(tbl, named_list) reads
  # such a list as one data frame whose columns are the tibbles (a single garbage row per
  # overall, every key NA), so the lists must be spliced in as separate data frames.
  empty <- empty_overall_outputs()
  list(
    overall_scores = dplyr::bind_rows(c(list(empty$overall_scores), unname(overall_scores))),
    component_contributions = dplyr::bind_rows(
      c(list(empty$component_contributions), unname(component_contributions))
    )
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
      imputed = any(imputed %in% TRUE),
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
                                            index_col = "Index",
                                            absent_categories_reported = character()) {
  key_cols <- normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)

  # A category is present only if it is observed somewhere: one whose every score is NA
  # carries no weight anywhere under the available-case rule below, exactly like one with no
  # rows at all.
  categories_in_data <- sort(unique(category_scores$category[!is.na(category_scores$category_score)]))
  categories_in_weights <- sort(unique(weights_tbl$category))

  # One warning per build, naming every weighted category that scores nowhere. Categories
  # the caller has already reported (a configured score variable that was never built, see
  # warn_unproduced_score_variables()) are left out so one defect is not reported twice.
  extra_config_categories <- setdiff(
    setdiff(categories_in_weights, categories_in_data),
    absent_categories_reported
  )
  if (length(extra_config_categories) > 0) {
    warning(
      pillar_label,
      ": config/weights.yml gives weight to categories with no observed category score ",
      "(not defined for this pillar in config/index_definition.yml, or none of their inputs ",
      "carry a value); their weight is dropped and the rest renormalized: ",
      paste(extra_config_categories, collapse = ", "),
      call. = FALSE
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

  # Available-case aggregation: within each key group the index is
  #
  #   sum_{k observed} w_k c_k / sum_{k observed} w_k
  #
  # A category whose score is NA there carries no weight. It used to keep its weight in the
  # denominator while adding nothing to the numerator, which scored an unobserved category
  # as 0 (it pulled ES Upstream fossil cells down by 0.10-0.14 through NA Reserves rows).
  # A key group with no observed weighted category gets an NA index - no value is invented.
  #
  # The denominator is derived once here and shared by both contribution tables. They used
  # to sum `weight` down their own rows, which for variable_contributions (one row per
  # variable) counted each category's weight once per variable and inflated the denominator.
  observed_categories <- category_scores %>%
    dplyr::filter(!is.na(category_score)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key_cols, "category")))) %>%
    dplyr::inner_join(weights_tbl, by = "category")

  category_weight_sums <- observed_categories %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarize(
      observed_count = dplyr::n_distinct(category),
      weight_sum = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(complete_categories = observed_count == length(expected_categories))

  # In a scored group an unobserved (or unweighted) category gets weight 0 and contributes 0,
  # so both tables sum to the index with a plain sum(). In a group with nothing scored, the
  # weights are undefined and everything is NA, like the index.
  category_contributions <- category_scores %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::left_join(category_weight_sums, by = key_cols) %>%
    dplyr::mutate(
      complete_categories = dplyr::coalesce(complete_categories, FALSE),
      scored_group = !is.na(weight_sum) & weight_sum > 0,
      category_weight = dplyr::case_when(
        !scored_group ~ NA_real_,
        is.na(category_score) | is.na(weight) ~ 0,
        TRUE ~ weight / weight_sum
      ),
      weighted_category_contribution = dplyr::case_when(
        !scored_group ~ NA_real_,
        category_weight == 0 ~ 0,
        TRUE ~ category_score * category_weight
      )
    ) %>%
    dplyr::select(-observed_count, -scored_group)

  if (!isTRUE(allow_partial_categories)) {
    category_contributions <- category_contributions %>%
      dplyr::mutate(
        category_weight = dplyr::if_else(complete_categories, category_weight, NA_real_),
        weighted_category_contribution = dplyr::if_else(complete_categories, weighted_category_contribution, NA_real_)
      )
  }

  # Variable rows take the category weight from category_contributions itself, so the two
  # decompositions cannot drift apart. Rows of a category with no category score have no
  # place in the index and are dropped.
  category_weight_lookup <- category_contributions %>%
    dplyr::select(dplyr::all_of(c(key_cols, "category")), category_weight)

  variable_contributions <- variable_contributions %>%
    dplyr::inner_join(
      category_weight_lookup,
      by = c(key_cols, "category"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      weighted_variable_contribution = dplyr::case_when(
        is.na(category_weight) ~ NA_real_,
        is.na(component_value) | component_weight_within_overall == 0 | category_weight == 0 ~ 0,
        TRUE ~ component_value * component_weight_within_overall * category_weight
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(key_cols),
      category,
      dplyr::any_of("score_variable"),
      variable,
      component_value,
      component_weight_within_overall,
      category_weight,
      weighted_variable_contribution,
      missing_rule_applied,
      imputed
    )

  # sum() without na.rm: NA exactly when the group has nothing scored (or, with
  # allow_partial_categories = FALSE, is missing an expected category).
  index_tbl <- category_contributions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarize(
      complete_categories = all(complete_categories),
      index_value = sum(weighted_category_contribution),
      .groups = "drop"
    ) %>%
    dplyr::rename(!!index_col := index_value)

  unscored_groups <- category_scores %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::anti_join(
      dplyr::filter(category_weight_sums, weight_sum > 0),
      by = key_cols
    )

  list(
    category_contributions = category_contributions,
    variable_contributions = variable_contributions,
    index_tbl = index_tbl,
    unscored_groups = unscored_groups
  )
}

# Share of each index value's weight that rests on imputed components, read off the
# variable decomposition (whose weights sum to 1 in every scored group). 1 means the value is
# built entirely from imputed components: the sources cover the country in none of the
# cell's categories. NA for groups with nothing scored.
summarize_index_imputation <- function(variable_contributions, include_sub_sector = FALSE) {
  key_cols <- normalize_sub_sector_or_keys(c("Country", "tech", "supply_chain"), include_sub_sector)

  variable_contributions %>%
    dplyr::mutate(.weight = component_weight_within_overall * category_weight) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarize(
      imputed_weight_share = if (all(is.na(.weight))) {
        NA_real_
      } else {
        sum(.weight[imputed %in% TRUE], na.rm = TRUE) / sum(.weight, na.rm = TRUE)
      },
      .groups = "drop"
    )
}

# Tech x stage summary of a pillar index for the builders' diagnostics. A cell is
# "unscored" when no country in it has an observed category (every index NA), and
# "constant" when the scored countries all share one value, so the cell cannot rank
# countries (e.g. its only categories are world-level series copied to every country).
summarize_index_cells <- function(index_tbl, index_col, include_sub_sector = FALSE, tolerance = 1e-12) {
  cell_cols <- normalize_sub_sector_or_keys(c("tech", "supply_chain"), include_sub_sector)

  index_tbl %>%
    dplyr::mutate(.index_value = .data[[index_col]]) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(cell_cols))) %>%
    dplyr::summarize(
      n_countries = dplyr::n(),
      n_unscored = sum(is.na(.index_value)),
      score_range = if (all(is.na(.index_value))) NA_real_ else diff(range(.index_value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cell_status = dplyr::case_when(
        n_unscored == n_countries ~ "unscored",
        n_countries - n_unscored >= 2 & score_range <= tolerance ~ "constant",
        TRUE ~ "ok"
      )
    )
}
