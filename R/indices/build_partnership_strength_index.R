# Build Partnership Strength index outputs (friendshore + opportunity + development).
#
# Level. The PSI is a partner-country score, collapsed over technology and stage and over
# reporters (scripts/25 passes tech = supply_chain = "All"):
#   C^k_j = mean over the rows for partner j of pillar k's index: for Friendshore and Opportunity
#           one row per reporter i != j (the dyad's top-5 (tech, stage) mean), for Development
#           the partner's own top-5 mean (it has no reporter);
#   PSI_j = sum_k w_k C^k_j / sum over the categories j has of w_k   (w = config/weights.yml psi).
# A category a partner lacks is dropped and the weights renormalized, not scored 0: Development
# is defined only for non-high-income partners, so high-income partners get
# PSI = (0.4 FS + 0.4 OP) / 0.8 rather than a score capped at 0.8. Each category uses its pillar
# index only (psi_index_variables()); the components, which include reporter attributes and a
# zero-weight term, are no longer averaged in alongside the index.
# build_partnership_strength_dyads() gives the same composite per (reporter, partner).

psi_index_variables <- function() {
  c(
    friendshore = "Friendshore Index",
    opportunity = "Opportunity Index",
    development = "Development Potential Index"
  )
}

if (!exists("normalize_sub_sector_or_keys", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "index_builder_core.R"))
}

standardize_partnership_strength_inputs <- function(theme_tables) {
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
    validate_schema(standardized_tbl)

    standardized_tbl %>%
      dplyr::mutate(
        Country = as.character(Country),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        category = stringr::str_to_lower(as.character(category)),
        theme = as.character(theme_name)
      )
  }, theme_names, theme_tables)

  dplyr::bind_rows(standardized)
}

build_partnership_strength_index <- function(theme_tables,
                                             weights,
                                             allow_partial_categories = TRUE,
                                             index_variables = psi_index_variables()) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Partnership strength weights are missing or empty.")
  }

  psi_data <- standardize_partnership_strength_inputs(theme_tables) %>%
    dplyr::filter(data_type == "index")

  # One variable per category: the pillar index. NULL keeps every index-typed variable.
  if (!is.null(index_variables)) {
    names(index_variables) <- stringr::str_to_lower(names(index_variables))
    psi_data <- psi_data %>%
      dplyr::filter(variable == unname(index_variables[category]))
  }

  if (nrow(psi_data) == 0) {
    stop("Partnership strength inputs are empty after filtering to index values.")
  }

  if (!is.integer(psi_data$Year) || any(is.na(psi_data$Year))) {
    stop("Year must be integer and non-missing for partnership strength inputs.")
  }

  group_cols <- c("Country", "tech", "supply_chain", "category")

  category_scores <- psi_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      category_score = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      n_obs = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    # A category with no value for the partner (development for high-income partners) is
    # absent, so it takes no weight in the composite instead of counting as 0.
    dplyr::filter(!is.na(category_score))

  weights_tbl <- tibble::tibble(
    category = stringr::str_to_lower(names(weights)),
    weight = as.numeric(unlist(weights, use.names = FALSE))
  )

  index_outputs <- compute_index_and_contributions(
    category_scores,
    variable_contributions = category_scores %>%
      dplyr::mutate(
        variable = category,
        component_value = category_score,
        component_weight_within_overall = 1,
        missing_rule_applied = NA_character_,
        imputed = FALSE
      ) %>%
      dplyr::select(
        Country,
        tech,
        supply_chain,
        category,
        variable,
        component_value,
        component_weight_within_overall,
        missing_rule_applied,
        imputed
      ),
    weights_tbl = weights_tbl,
    allow_partial_categories = allow_partial_categories,
    include_sub_sector = FALSE,
    pillar_label = "Partnership strength",
    index_col = "Partnership_Strength_Index"
  )

  index_tbl <- index_outputs$index_tbl %>%
    dplyr::select(-dplyr::any_of("complete_categories"))

  list(
    partnership_strength_index = index_tbl,
    category_contributions = index_outputs$category_contributions,
    variable_contributions = index_outputs$variable_contributions,
    index = index_tbl
  )
}

# Dyadic PSI: the same composite for each (reporter, partner) pair, from the dyad top-5
# Friendshore and Opportunity scores and the partner's top-5 Development score, averaging over
# the categories the pair has (weights renormalized). Both dyad pillars rate the partner from the
# reporter's side (Friendshore: partner as a supplier to the reporter; Opportunity: partner as a
# market for the reporter's exports), so PSI(i, j) is partner j's value to reporter i.
build_partnership_strength_dyads <- function(friendshore_top5,
                                             opportunity_top5,
                                             development_top5,
                                             weights,
                                             index_variables = psi_index_variables()) {
  if (is.null(weights) || length(weights) == 0) {
    stop("Partnership strength weights are missing or empty.")
  }
  require_columns(friendshore_top5, c("reporter_iso", "partner_iso", "variable", "value"), label = "friendshore_top5")
  require_columns(opportunity_top5, c("reporter_iso", "partner_iso", "variable", "value"), label = "opportunity_top5")
  require_columns(development_top5, c("partner_iso", "variable", "value"), label = "development_top5")

  w <- stats::setNames(as.numeric(unlist(weights, use.names = FALSE)), stringr::str_to_lower(names(weights)))
  missing_w <- setdiff(c("friendshore", "opportunity", "development"), names(w))
  if (length(missing_w) > 0) {
    stop("Partnership strength weights missing: ", paste(missing_w, collapse = ", "))
  }

  # The target variable is resolved outside filter(): the top5 tables have their own `category`
  # column, which would mask the argument inside the data mask.
  pick_index <- function(tbl, pillar, keys) {
    target <- index_variables[[pillar]]
    tbl %>%
      dplyr::filter(variable == !!target) %>%
      dplyr::select(dplyr::all_of(keys), value) %>%
      dplyr::rename(!!pillar := value)
  }

  fs <- pick_index(friendshore_top5, "friendshore", c("reporter_iso", "partner_iso"))
  op <- pick_index(opportunity_top5, "opportunity", c("reporter_iso", "partner_iso"))
  dv <- pick_index(development_top5, "development", "partner_iso")

  out <- dplyr::full_join(fs, op, by = c("reporter_iso", "partner_iso")) %>%
    dplyr::left_join(dv, by = "partner_iso") %>%
    dplyr::filter(reporter_iso != partner_iso) %>%
    dplyr::mutate(
      n_categories = (!is.na(friendshore)) + (!is.na(opportunity)) + (!is.na(development)),
      Partnership_Strength_Index = partnership_strength_row_weighted_mean(
        list(friendshore, opportunity, development),
        c(w[["friendshore"]], w[["opportunity"]], w[["development"]])
      )
    )

  partnership_strength_assert_unique(out, c("reporter_iso", "partner_iso"), label = "psi dyads")
}
