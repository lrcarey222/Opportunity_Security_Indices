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
# Eligibility: a country is scored if each of `required_components` is observed for at least
# one of its cells. IEA PAMS is the backbone and the only required component by default
# (POLICY_REQUIRED_COMPONENTS, user decision of 9 Oct 2026): it covers 215 economies at
# technology x stage resolution, while CAT rates 39 countries and the EU. A country with IEA
# PAMS but no CAT rating is scored from IEA PAMS alone and flagged policy_basis "IEA PAMS only"
# (150 countries in the Oct 2026 build, Israel, Ukraine and nine coalition partners among
# them); one with both is "IEA PAMS + CAT" (65). Until then every weighted component was
# required, so CAT was the binding source and those 150 had no Policy index and no Strategic
# Index; required_components = names(component_weights) restores that rule (scripts/20:
# OPSI_POLICY_REQUIRE_ALL=1, for robustness runs).
#
# A country with a CAT rating but no IEA PAMS is not scored (none in the Oct 2026 build). CAT
# is one economy-wide rating that differs across a country's cells only through the fixed
# technology GHG index (c*g + (1-c)*(1-g), R/categories/policy/cat_policy_index.R), so on its
# own the Strategic Index's within-country S-curve of policy would rank sectors by technology
# emissions: a technology attribute the SI already carries in its climate term, not a reading
# of the country's sector policy (the reason the dual-use score was dropped, below).
#
# Within an eligible country the weights are renormalised over the components present in each
# cell, so with NIPO included the effective weights differed by stage (NIPO had no Downstream
# cells). `n_components` records how many components each cell has. The Strategic Index ranks
# policy within a country, so a country's basis does not move its values against another
# country's, but the two bases are not the same reading: CAT adds its technology-GHG tilt. For
# the 65 countries with both, ranking their cells on IEA PAMS alone agrees with the composite
# at a median within-country Spearman of 0.48 (Oct 2026 build), and 6 of the 45 coalition
# members with CAT would get another allied pick. Hence the flag; a cross-country comparison
# of the policy level should also keep the two bases apart.
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

POLICY_REQUIRED_COMPONENTS <- "IEA PAMS Policy Index"

# Returns list(index, eligibility):
#   index        Country, tech, supply_chain, category, variable, data_type, value, Year,
#                source, explanation, n_components, policy_basis (eligible countries, non-NA
#                cells only; policy_basis is the country's, n_components the cell's)
#   eligibility  one row per country: n_components (weighted components observed anywhere),
#                eligible, policy_basis ("IEA PAMS + CAT", "IEA PAMS only"; NA when not
#                eligible), missing_components (weighted components not observed),
#                missing_required (required ones not observed; NA when eligible) and a
#                has_<component> flag per component
build_composite_policy_index <- function(policy_component_tbl,
                                         techs = NULL,
                                         component_weights = POLICY_COMPONENT_WEIGHTS,
                                         required_components = POLICY_REQUIRED_COMPONENTS) {
  require_columns(
    policy_component_tbl,
    c("Country", "tech", "supply_chain", "variable", "value"),
    label = "policy_component_tbl"
  )
  if (is.null(names(component_weights)) || any(!nzchar(names(component_weights)))) {
    stop("component_weights must be a named numeric vector keyed by component variable.")
  }
  component_vars <- names(component_weights)
  if (!is.character(required_components) || length(required_components) == 0 ||
      anyNA(required_components) || !all(required_components %in% component_vars)) {
    stop(
      "required_components must name one or more of the weighted components: ",
      paste(component_vars, collapse = ", "), "."
    )
  }

  components <- policy_component_tbl %>%
    dplyr::filter(.data$variable %in% component_vars)
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
    tidyr::complete(Country, variable = component_vars, fill = list(has = FALSE)) %>%
    # component order as in component_weights, so policy_basis reads "IEA PAMS + CAT"
    dplyr::arrange(.data$Country, match(.data$variable, component_vars))

  flags <- availability %>%
    dplyr::mutate(variable = paste0("has_", policy_component_slug(.data$variable))) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "has")

  eligibility <- availability %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(
      n_components = sum(.data$has),
      eligible = all(.data$has[.data$variable %in% required_components]),
      policy_basis = if (eligible) policy_basis_label(.data$variable[.data$has]) else NA_character_,
      missing_components = if (all(.data$has)) NA_character_ else
        paste(.data$variable[!.data$has], collapse = "; "),
      missing_required = if (eligible) NA_character_ else
        paste(.data$variable[!.data$has & .data$variable %in% required_components], collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::left_join(flags, by = "Country") %>%
    dplyr::arrange(dplyr::desc(.data$eligible), .data$Country)

  weights_text <- paste(
    paste0(policy_component_label(component_vars), " (", component_weights, ")"),
    collapse = ", "
  )
  rule_text <- if (all(component_vars %in% required_components)) {
    paste0("countries kept only if all ", length(component_vars), " components exist somewhere")
  } else {
    paste0(
      "countries kept if ", paste(policy_component_label(required_components), collapse = " and "),
      " exist", if (length(required_components) == 1) "s", " somewhere"
    )
  }

  index <- components %>%
    dplyr::inner_join(
      eligibility %>%
        dplyr::filter(.data$eligible) %>%
        dplyr::select("Country", "policy_basis"),
      by = "Country"
    ) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain, .data$policy_basis) %>%
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
        rule_text, "; basis ", .data$policy_basis
      )
    ) %>%
    dplyr::select(
      "Country", "tech", "supply_chain",
      "category", "variable", "data_type",
      "value", "Year", "source", "explanation", "n_components", "policy_basis"
    )

  list(index = index, eligibility = eligibility)
}

# "IEA PAMS Policy Index" -> "IEA PAMS"; the short names used in labels and messages.
policy_component_label <- function(x) {
  sub(" (Policy Index|policy score)$", "", x)
}

# The components a country's index rests on, in weight order: "IEA PAMS + CAT", "IEA PAMS only".
policy_basis_label <- function(components) {
  labels <- policy_component_label(components)
  if (length(labels) == 1) paste(labels, "only") else paste(labels, collapse = " + ")
}

policy_component_slug <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}
