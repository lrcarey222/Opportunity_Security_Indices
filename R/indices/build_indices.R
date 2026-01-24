# Index helpers shared across scripts.

build_strategic_index <- function(economic_opportunity_index,
                                  energy_security_index,
                                  policy_index,
                                  technological_readiness_index,
                                  techs) {
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
  require_columns(
    technological_readiness_index,
    c("Country", "tech", "supply_chain", "trl_index"),
    label = "technological_readiness_index"
  )

  left_join(economic_opportunity_index, energy_security_index,
            by = c("Country", "tech", "supply_chain")) %>%
    left_join(
      policy_index %>% dplyr::select(Country, tech, supply_chain, value),
      by = c("Country", "tech", "supply_chain")
    ) %>%
    left_join(
      technological_readiness_index,
      by = c("Country", "tech", "supply_chain")
    ) %>%
    dplyr::filter(.data$tech %in% techs) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(
      eo = median_scurve(.data$Economic_Opportunity_Index),
      es = 1 - median_scurve(.data$Energy_Security_Index),
      pol = median_scurve(.data$value),
      trl = median_scurve(.data$trl_index),
      eo = dplyr::if_else(is.na(eo), mean(eo, na.rm = TRUE), eo),
      es = dplyr::if_else(is.na(es), mean(es, na.rm = TRUE), es),
      pol = dplyr::if_else(is.na(pol), mean(pol, na.rm = TRUE), pol),
      trl = dplyr::if_else(is.na(trl), mean(trl, na.rm = TRUE), trl),
      strategic_index = eo + es + pol + trl
    ) %>%
    dplyr::ungroup()
}
