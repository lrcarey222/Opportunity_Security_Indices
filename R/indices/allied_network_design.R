# Allied network helpers.

allied_network_resolve_node_weights <- function(node_weights,
                                                defaults = c(eo = 0.5, policy = 0.3, resilience = 0.2)) {
  # Accept list, named vector, or scalar shorthand.
  if (is.null(node_weights) || length(node_weights) == 0) {
    w <- defaults
  } else if (is.atomic(node_weights) && length(node_weights) == 1 && is.null(names(node_weights))) {
    w <- stats::setNames(rep(as.numeric(node_weights), length(defaults)), names(defaults))
  } else {
    if (is.list(node_weights)) {
      node_weights <- unlist(node_weights, use.names = TRUE)
    }

    if (is.null(names(node_weights))) {
      if (length(node_weights) != length(defaults)) {
        stop("node_weights must be named (eo/policy/resilience), a scalar numeric, or length-3 numeric.")
      }
      names(node_weights) <- names(defaults)
    }

    w <- c(
      eo = suppressWarnings(as.numeric(node_weights[["eo"]])),
      policy = suppressWarnings(as.numeric(node_weights[["policy"]])),
      resilience = suppressWarnings(as.numeric(node_weights[["resilience"]]))
    )
  }

  w <- allied_network_normalize_weights(w)

  # Some normalizers may drop names. Restore/reorder deterministically.
  if (is.null(names(w)) || !all(names(defaults) %in% names(w))) {
    if (length(w) != length(defaults)) {
      stop("node_weights must resolve to three values (eo/policy/resilience).")
    }
    names(w) <- names(defaults)
  }
  w <- w[names(defaults)]

  if (any(is.na(w)) || any(!is.finite(w)) || sum(w) <= 0) {
    stop("node_weights must be finite and positive for eo/policy/resilience.")
  }

  w
}

allied_network_prepare_nodes <- function(economic_opportunity_index,
                                         energy_security_index,
                                         policy_index,
                                         country_info,
                                         iso3c_network = allied_network_default_iso3c(),
                                         partner_development_country_tbl = NULL,
                                         node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
                                         demand_from = c("need", "equal")) {
  demand_from <- match.arg(demand_from)

  require_columns(
    economic_opportunity_index,
    c("Country", "tech", "supply_chain", "Economic_Opportunity_Index"),
    label = "economic_opportunity_index"
  )
  require_columns(
    energy_security_index,
    c("Country", "tech", "supply_chain", "Energy_Security_Index"),
    label = "energy_security_index"
  )
  require_columns(
    policy_index,
    c("Country", "tech", "supply_chain", "value"),
    label = "policy_index"
  )
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  w_node <- allied_network_resolve_node_weights(node_weights)

  eo <- economic_opportunity_index %>%
    dplyr::transmute(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info),
      country = Country,
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      eo_raw = suppressWarnings(as.numeric(Economic_Opportunity_Index))
    )

  es <- energy_security_index %>%
    dplyr::transmute(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      es_raw = suppressWarnings(as.numeric(Energy_Security_Index))
    )

  pol <- policy_index %>%
    dplyr::transmute(
      Country = partnership_strength_standardize_countries(Country),
      iso3c = partnership_strength_country_to_iso(Country, country_info),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      policy_raw = suppressWarnings(as.numeric(value))
    )

  nodes <- eo %>%
    dplyr::left_join(es, by = c("iso3c", "tech", "supply_chain")) %>%
    dplyr::left_join(pol, by = c("iso3c", "tech", "supply_chain")) %>%
    dplyr::filter(!is.na(iso3c), iso3c %in% iso3c_network)

  if (!is.null(partner_development_country_tbl)) {
    require_columns(
      partner_development_country_tbl,
      c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value"),
      label = "partner_development_country_tbl"
    )

    dev <- partner_development_country_tbl %>%
      dplyr::filter(
        category == "development",
        variable == "Development Potential Index",
        data_type == "index"
      ) %>%
      dplyr::transmute(
        Country = partnership_strength_standardize_countries(Country),
        iso3c = partnership_strength_country_to_iso(Country, country_info),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        dev_potential = suppressWarnings(as.numeric(value))
      )

    nodes <- nodes %>%
      dplyr::left_join(dev, by = c("iso3c", "tech", "supply_chain"))
  } else {
    nodes$dev_potential <- NA_real_
  }

  nodes <- nodes %>%
    dplyr::mutate(dev_potential = dplyr::coalesce(dev_potential, 0.5)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      eo_idx = median_scurve(eo_raw),
      es_idx = median_scurve(es_raw),
      pol_idx = median_scurve(policy_raw),
      need_idx = 1 - es_idx,
      producer_score = w_node[["eo"]] * eo_idx +
        w_node[["policy"]] * pol_idx +
        w_node[["resilience"]] * es_idx
    ) %>%
    dplyr::ungroup()

  nodes %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      demand_weight = dplyr::case_when(
        demand_from == "equal" ~ 1,
        TRUE ~ dplyr::coalesce(need_idx, 0)
      ),
      demand_weight = if (all(is.na(demand_weight)) || sum(demand_weight, na.rm = TRUE) <= 0) {
        rep(1, dplyr::n())
      } else {
        demand_weight
      },
      demand_weight = demand_weight / sum(demand_weight, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}
