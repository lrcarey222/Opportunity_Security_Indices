# Strategic Index and allied sector selection (pure; scripts/20_build_indices.R does the IO).
#
# SI_its = 0.2 * pol_its + 0.3 * EO_its + 0.3 * (1 - ES_its) + 0.1 * lambda_s + 0.1 * g_t
#
#   pol     within-country median S-curve of the composite Policy index (value), taken over
#           the country's in-sample sectors only
#   EO      Economic Opportunity pillar, as built (cross-country level; not re-ranked)
#   1 - ES  energy-security NEED: the Energy Security pillar is oriented higher = more
#           secure, so the SI rewards sectors where the country is least secure. The column
#           that enters the formula is `energy_security_need`; `Energy_Security_Index` keeps
#           its security meaning in the output.
#   lambda  supply-chain stage weight (Midstream 0.75 > Upstream 0.50 > Downstream 0.25)
#   g_t     technology climate weight, ghg_index (1 = lowest lifecycle GHG intensity)
#
# Sample rule: a cell enters the SI only if EO, ES and the Policy index are all observed.
# Nothing is imputed. The pipeline used to fill a missing pillar with the country's mean over
# the other technologies in the same stage (1,041 of 6,757 cells lacked ES in the Sep 2026
# build, 51 of the 571 scored ones), which puts an invented pillar value into a composite
# that is then ranked. Excluded cells stay in the table with strategic_index = NA and the
# reason in `si_exclusion`. `excluded_sectors` (Geothermal - Midstream) is removed before
# the within-country S-curves, so the ranks are taken over the sectors that are reported:
# pol over the scored sectors; eo and es_need (chart inputs, not in the SI) over the sectors
# where that pillar is observed, so they stay defined for countries without a Policy index.

STRATEGIC_INDEX_WEIGHTS <- c(
  policy = 0.2,
  economic_opportunity = 0.3,
  energy_security_need = 0.3,
  supply_chain = 0.1,
  technology = 0.1
)

STRATEGIC_SUPPLY_CHAIN_WEIGHTS <- c(Upstream = 0.50, Midstream = 0.75, Downstream = 0.25)

STRATEGIC_EXCLUDED_SECTORS <- c("Geothermal - Midstream")

# Allies for the allied strategic selection (ISO3). "ARE" is the United Arab Emirates; the
# list used to carry the invalid code "UAE", which matched nothing.
STRATEGIC_ALLY_ISO3 <- c(
  "USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA", "ITA", "BRA", "SAU",
  "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK", "ARG", "MAR", "CHL"
)

strategic_index_key_check <- function(tbl, label) {
  dup <- tbl %>%
    dplyr::count(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop(
      label, " has ", nrow(dup), " duplicated Country x tech x supply_chain keys (e.g. ",
      paste(utils::head(paste(dup$Country, dup$tech, dup$supply_chain, sep = " / "), 3), collapse = "; "),
      "); joining it would duplicate strategic index rows."
    )
  }
  invisible(tbl)
}

build_strategic_index <- function(economic_opportunity_index,
                                  energy_security_index,
                                  policy_index,
                                  tech_ghg,
                                  technological_readiness_index = NULL,
                                  techs = NULL,
                                  weights = STRATEGIC_INDEX_WEIGHTS,
                                  supply_chain_weights = STRATEGIC_SUPPLY_CHAIN_WEIGHTS,
                                  excluded_sectors = STRATEGIC_EXCLUDED_SECTORS) {
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
  require_columns(policy_index, c("Country", "tech", "supply_chain", "value"), label = "policy_index")
  require_columns(tech_ghg, c("tech", "ghg_index"), label = "tech_ghg")

  missing_weights <- setdiff(names(STRATEGIC_INDEX_WEIGHTS), names(weights))
  if (length(missing_weights) > 0) {
    stop("weights must name: ", paste(missing_weights, collapse = ", "))
  }

  eo <- economic_opportunity_index %>%
    dplyr::select("Country", "tech", "supply_chain", "Economic_Opportunity_Index")
  es <- energy_security_index %>%
    dplyr::select("Country", "tech", "supply_chain", "Energy_Security_Index")
  pol <- policy_index %>%
    dplyr::select("Country", "tech", "supply_chain", "value")
  if (!is.null(techs)) {
    eo <- eo %>% dplyr::filter(.data$tech %in% techs)
    es <- es %>% dplyr::filter(.data$tech %in% techs)
    pol <- pol %>% dplyr::filter(.data$tech %in% techs)
  }
  strategic_index_key_check(eo, "economic_opportunity_index")
  strategic_index_key_check(es, "energy_security_index")
  strategic_index_key_check(pol, "policy_index")

  ghg <- tech_ghg %>%
    dplyr::distinct(.data$tech, .keep_all = TRUE) %>%
    dplyr::select("tech", "ghg_index")

  base <- eo %>%
    dplyr::left_join(es, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::left_join(pol, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::left_join(ghg, by = "tech")

  if (!is.null(technological_readiness_index)) {
    trl <- technological_readiness_index %>%
      dplyr::select("Country", "tech", "supply_chain", "trl_index")
    strategic_index_key_check(trl, "technological_readiness_index")
    base <- base %>% dplyr::left_join(trl, by = c("Country", "tech", "supply_chain"))
  } else {
    base$trl_index <- NA_real_
  }

  policy_countries <- unique(pol$Country[!is.na(pol$value)])

  base %>%
    dplyr::mutate(sector = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
    dplyr::filter(!.data$sector %in% excluded_sectors) %>%
    dplyr::mutate(
      energy_security_need = 1 - .data$Energy_Security_Index,
      sc_weight = unname(supply_chain_weights[.data$supply_chain]),
      # A fixed technology attribute, not a country measurement; the global mean only fills a
      # technology absent from tech_ghg (none of the 11 headline technologies is).
      tech_weight = dplyr::coalesce(.data$ghg_index, mean(ghg$ghg_index, na.rm = TRUE)),
      si_exclusion = paste_reasons(
        dplyr::if_else(!.data$Country %in% policy_countries, "country not policy-eligible", NA_character_),
        dplyr::if_else(.data$Country %in% policy_countries & is.na(.data$value), "missing policy index", NA_character_),
        dplyr::if_else(is.na(.data$Economic_Opportunity_Index), "missing economic opportunity", NA_character_),
        dplyr::if_else(is.na(.data$Energy_Security_Index), "missing energy security", NA_character_),
        dplyr::if_else(is.na(.data$sc_weight), "unknown supply chain stage", NA_character_)
      ),
      in_sample = is.na(.data$si_exclusion)
    ) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(
      # pol, the only S-curve in the SI, is ranked over the scored sectors. eo and es_need
      # feed the charts' EO vs ES-risk views (package_selection_viz.R, the megatable's
      # strategic path) and do not depend on the Policy index, so they are ranked over the
      # sectors where that pillar is observed, as before the policy sample rule.
      pol = scurve_in_sample(.data$value, .data$in_sample),
      eo = scurve_in_sample(.data$Economic_Opportunity_Index, TRUE),
      es_need = scurve_in_sample(.data$energy_security_need, TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Deprecated alias of es_need, kept so R/charts consumers that read `es` as the
      # S-curved energy-security risk keep working; use es_need.
      es = .data$es_need,
      strategic_index = dplyr::if_else(
        .data$in_sample,
        weights[["policy"]] * .data$pol +
          weights[["economic_opportunity"]] * .data$Economic_Opportunity_Index +
          weights[["energy_security_need"]] * .data$energy_security_need +
          weights[["supply_chain"]] * .data$sc_weight +
          weights[["technology"]] * .data$tech_weight,
        NA_real_
      )
    ) %>%
    dplyr::select(
      "Country", "tech", "supply_chain", "sector",
      "Economic_Opportunity_Index", "Energy_Security_Index", "energy_security_need",
      "value", "ghg_index", "trl_index",
      "eo", "es_need", "es", "pol",
      "sc_weight", "tech_weight",
      "strategic_index", "si_exclusion"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index))
}

# Weighted contribution of each SI term (they sum to strategic_index), for decomposition
# charts and tables.
strategic_index_contributions <- function(strategic_index, weights = STRATEGIC_INDEX_WEIGHTS) {
  require_columns(
    strategic_index,
    c("pol", "Economic_Opportunity_Index", "energy_security_need", "sc_weight", "tech_weight"),
    label = "strategic_index"
  )
  strategic_index %>%
    dplyr::mutate(
      contrib_policy = weights[["policy"]] * .data$pol,
      contrib_eo = weights[["economic_opportunity"]] * .data$Economic_Opportunity_Index,
      contrib_es_need = weights[["energy_security_need"]] * .data$energy_security_need,
      contrib_sc = weights[["supply_chain"]] * .data$sc_weight,
      contrib_ghg = weights[["technology"]] * .data$tech_weight
    )
}

# Sectors in which a pillar has no cross-country variation among the scored cells (standard
# deviation 0 over two or more countries). There the pillar term is a sector constant, not a
# measurement: in the Sep 2026 build ES was identically 0 in Nuclear - Upstream and
# Geothermal - Upstream (zero-filled cells), so energy_security_need = 1 for every country
# and Nuclear - Upstream was the top pick of 8 of the 18 allies scored. scripts/20 reports
# any sector listed here and rebuilds the index with it excluded.
strategic_index_degenerate_sectors <- function(strategic_index, tol = 1e-12) {
  require_columns(
    strategic_index,
    c("Country", "sector", "Economic_Opportunity_Index", "Energy_Security_Index", "strategic_index"),
    label = "strategic_index"
  )
  strategic_index %>%
    dplyr::filter(!is.na(.data$strategic_index)) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::summarise(
      n_countries = dplyr::n_distinct(.data$Country),
      es_sd = stats::sd(.data$Energy_Security_Index),
      eo_sd = stats::sd(.data$Economic_Opportunity_Index),
      es_first = .data$Energy_Security_Index[[1]],
      eo_first = .data$Economic_Opportunity_Index[[1]],
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      es_constant = .data$n_countries > 1 & .data$es_sd <= tol,
      eo_constant = .data$n_countries > 1 & .data$eo_sd <= tol,
      es_value = dplyr::if_else(.data$es_constant, .data$es_first, NA_real_),
      eo_value = dplyr::if_else(.data$eo_constant, .data$eo_first, NA_real_)
    ) %>%
    dplyr::filter(.data$es_constant | .data$eo_constant) %>%
    dplyr::select(
      "sector", "n_countries", "es_constant", "es_value", "eo_constant", "eo_value",
      "es_sd", "eo_sd"
    ) %>%
    dplyr::arrange(.data$sector)
}

# Top Upstream/Midstream sector per ally (ties kept), excluding the technologies the allied
# network does not model.
select_allied_strategic_sectors <- function(strategic_index,
                                            ally_countries,
                                            stages = c("Midstream", "Upstream"),
                                            excluded_techs = c("Geothermal", "Green Hydrogen"),
                                            n = 1) {
  require_columns(strategic_index, c("Country", "tech", "supply_chain", "strategic_index"), label = "strategic_index")
  strategic_index %>%
    dplyr::filter(
      .data$Country %in% ally_countries,
      .data$supply_chain %in% stages,
      !is.na(.data$strategic_index),
      !.data$tech %in% excluded_techs
    ) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::slice_max(order_by = .data$strategic_index, n = n) %>%
    dplyr::ungroup()
}

# Median S-curve over the non-NA rows flagged in_sample (TRUE: every non-NA row); NA elsewhere.
scurve_in_sample <- function(x, in_sample) {
  out <- rep(NA_real_, length(x))
  keep <- in_sample & !is.na(x)
  if (any(keep)) {
    out[keep] <- median_scurve(x[keep])
  }
  out
}

# Row-wise "; "-join of the non-NA reason strings; NA when there is none.
paste_reasons <- function(...) {
  parts <- list(...)
  out <- apply(do.call(cbind, parts), 1, function(r) {
    r <- r[!is.na(r)]
    if (length(r) == 0) NA_character_ else paste(r, collapse = "; ")
  })
  as.character(out)
}
