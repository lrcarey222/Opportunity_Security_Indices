# Composite Policy index (pure; scripts/20_build_indices.R does the IO).
#
# Combines policy components built in scripts/10_build_themes.R into one score per
# Country x tech x supply_chain:
#
#   P_its = sum_v w_v x^v_its / sum_v w_v, over the components v observed for the cell,
#   w = IEA PAMS 3, CAT 2 (POLICY_COMPONENT_WEIGHTS, the default since October 2026).
#
# scripts/20_build_indices.R passes the same weights explicitly and records them in
# policy_index_provenance. NIPO (GTA; weight 4 until October 2026, when NIPO was 4, IEA
# PAMS 3, CAT 2) is excluded from the composite for now: scripts/10 still builds it and its
# rows stay in policy_component_tbl, so it can return by adding "NIPO Policy Index" = 4 to
# the weights. The default follows the pipeline so that a caller relying on it (the paper
# scripts) rebuilds the stored index.
#
# Eligibility: a country is scored only if each weighted component is observed for at least
# one of its cells (with IEA PAMS + CAT: 65 countries in the Oct 2026 build; CAT, which rates
# 39 countries and the EU, is the binding source). Within an eligible country the weights are
# renormalised over the components present in each cell, so with NIPO included the effective
# weights differed by stage (NIPO had no Downstream cells). `n_components` records how many
# components each cell has.
#
# The dual-use score (weight 1 until October 2026) is no longer a component. It is an
# LLM-elicited judgement per technology x stage (R/categories/policy/dual_use_scores.R),
# the same for every country, so it could not rank countries; it only shifted sector levels,
# which the within-country S-curve of the Strategic Index then read as policy support. Its
# rows still travel in policy_component_tbl for reference and are ignored here, because
# only the components named in `component_weights` are read.
#
# This file used to hold an unused near-copy of the IEA PAMS builder
# (R/categories/policy/iea_policy_index.R) with a different jurisdiction weight, a
# within-country S-curve and a POLICY_TYPE_WEIGHTS table that clashed with the NIPO one.
# scripts/20 sourced it after the real builder, so its build_policy_index() silently replaced
# the IEA one in that session.

POLICY_COMPONENT_WEIGHTS <- c(
  "IEA PAMS Policy Index" = 3,
  "CAT Policy Index" = 2
)

# Returns list(index, eligibility):
#   index        Country, tech, supply_chain, category, variable, data_type, value, Year,
#                source, explanation, n_components (eligible countries, non-NA cells only)
#   eligibility  one row per country: n_components, eligible, missing_components and a
#                has_<component> flag per component
build_composite_policy_index <- function(policy_component_tbl,
                                         techs = NULL,
                                         component_weights = POLICY_COMPONENT_WEIGHTS) {
  require_columns(
    policy_component_tbl,
    c("Country", "tech", "supply_chain", "variable", "value"),
    label = "policy_component_tbl"
  )
  if (is.null(names(component_weights)) || any(!nzchar(names(component_weights)))) {
    stop("component_weights must be a named numeric vector keyed by component variable.")
  }
  required_vars <- names(component_weights)

  components <- policy_component_tbl %>%
    dplyr::filter(.data$variable %in% required_vars)
  if (!is.null(techs)) {
    components <- components %>% dplyr::filter(.data$tech %in% techs)
  }

  # One value per Country x tech x supply_chain x component (mean of duplicates).
  components <- components %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain, .data$variable) %>%
    dplyr::summarise(
      value = if (all(is.na(.data$value))) NA_real_ else mean(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(w = unname(component_weights[.data$variable]))

  availability <- components %>%
    dplyr::group_by(.data$Country, .data$variable) %>%
    dplyr::summarise(has = any(!is.na(.data$value)), .groups = "drop") %>%
    tidyr::complete(Country, variable = required_vars, fill = list(has = FALSE))

  flags <- availability %>%
    dplyr::mutate(variable = paste0("has_", policy_component_slug(.data$variable))) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "has")

  eligibility <- availability %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(
      n_components = sum(.data$has),
      eligible = all(.data$has),
      missing_components = if (all(.data$has)) NA_character_ else
        paste(.data$variable[!.data$has], collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::left_join(flags, by = "Country") %>%
    dplyr::arrange(dplyr::desc(.data$eligible), .data$Country)

  eligible_countries <- eligibility$Country[eligibility$eligible]

  weights_text <- paste(
    paste0(sub(" (Policy Index|policy score)$", "", required_vars), " (", component_weights, ")"),
    collapse = ", "
  )

  index <- components %>%
    dplyr::filter(.data$Country %in% eligible_countries) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_components = sum(!is.na(.data$value)),
      value = {
        denom <- sum(.data$w[!is.na(.data$value)])
        if (denom == 0) NA_real_ else sum(.data$value * .data$w, na.rm = TRUE) / denom
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(
      category = "Policy",
      variable = "Overall Policy Index",
      data_type = "index",
      Year = 0L,
      source = "Author calculation",
      explanation = paste0(
        "Weighted mean of ", weights_text, " over the components present in the cell; ",
        "countries kept only if all ", length(required_vars), " components exist somewhere"
      )
    ) %>%
    dplyr::select(
      "Country", "tech", "supply_chain",
      "category", "variable", "data_type",
      "value", "Year", "source", "explanation", "n_components"
    )

  list(index = index, eligibility = eligibility)
}

policy_component_slug <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}
