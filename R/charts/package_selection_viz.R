# Package selection visualization builders (pure functions; no file IO).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(scales)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

resolve_country_name <- function(tbl, country) {
  countries <- unique(tbl$Country)
  exact <- countries[countries == country]
  if (length(exact) == 1) {
    return(exact)
  }
  idx <- which(tolower(countries) == tolower(country))
  if (length(idx) == 1) {
    return(countries[idx])
  }
  stop("Country not found in input data: ", country)
}

build_country_strategic_tbl <- function(index_outputs,
                                        country,
                                        techs = NULL,
                                        include_sub_sector = FALSE) {
  if (is.null(index_outputs$economic_opportunity_index) ||
      is.null(index_outputs$energy_security_index)) {
    stop("index_outputs must include economic_opportunity_index and energy_security_index.")
  }

  eo_tbl <- index_outputs$economic_opportunity_index
  es_tbl <- index_outputs$energy_security_index

  country_name <- resolve_country_name(eo_tbl, country)

  eo_sel <- eo_tbl %>%
    dplyr::filter(.data$Country == country_name) %>%
    dplyr::select("Country", "tech", "supply_chain", "Economic_Opportunity_Index") %>%
    dplyr::distinct()

  es_sel <- es_tbl %>%
    dplyr::filter(.data$Country == country_name) %>%
    dplyr::select("Country", "tech", "supply_chain", "Energy_Security_Index") %>%
    dplyr::distinct()

  policy_tbl <- index_outputs$policy_index
  if (is.null(policy_tbl) && !is.null(index_outputs$policy_component_tbl)) {
    policy_tbl <- index_outputs$policy_component_tbl %>%
      dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
      dplyr::summarise(value = if (all(is.na(.data$value))) NA_real_ else mean(.data$value, na.rm = TRUE),
                       .groups = "drop")
  }
  if (is.null(policy_tbl)) {
    policy_tbl <- eo_sel %>% dplyr::transmute(Country, tech, supply_chain, value = NA_real_)
  }

  ghg_tbl <- tibble::tibble(tech = character(), ghg_index = numeric())
  if (!is.null(index_outputs$tech_ghg)) {
    ghg_tbl <- index_outputs$tech_ghg %>% dplyr::select("tech", "ghg_index")
  }

  trl_tbl <- tibble::tibble(Country = character(), tech = character(), supply_chain = character(), trl_index = numeric())
  if (!is.null(index_outputs$economic_opportunity_category_scores)) {
    trl_tbl <- index_outputs$economic_opportunity_category_scores %>%
      dplyr::filter(.data$category == "Technological Readiness") %>%
      dplyr::transmute(.data$Country, .data$tech, .data$supply_chain, trl_index = .data$category_score)
  }

  strategic_tbl <- eo_sel %>%
    dplyr::inner_join(es_sel, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::left_join(
      policy_tbl %>% dplyr::select("Country", "tech", "supply_chain", "value"),
      by = c("Country", "tech", "supply_chain")
    ) %>%
    dplyr::left_join(ghg_tbl, by = "tech") %>%
    dplyr::left_join(trl_tbl, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::mutate(
      Economic_Opportunity_Index = ifelse(
        is.na(.data$Economic_Opportunity_Index),
        mean(.data$Economic_Opportunity_Index, na.rm = TRUE),
        .data$Economic_Opportunity_Index
      ),
      Energy_Security_Index = ifelse(
        is.na(.data$Energy_Security_Index),
        mean(.data$Energy_Security_Index, na.rm = TRUE),
        .data$Energy_Security_Index
      )
    ) %>%
    dplyr::mutate(
      policy_fill = ifelse(
        is.na(.data$value),
        ifelse(all(is.na(.data$value)), 0.5, mean(.data$value, na.rm = TRUE)),
        .data$value
      ),
      eo = median_scurve(.data$Economic_Opportunity_Index),
      es_risk = 1 - median_scurve(.data$Energy_Security_Index),
      pol = median_scurve(.data$policy_fill),
      sc_weight = dplyr::case_when(
        .data$supply_chain == "Upstream" ~ 0.50,
        .data$supply_chain == "Midstream" ~ 0.75,
        .data$supply_chain == "Downstream" ~ 0.25,
        TRUE ~ NA_real_
      ),
      tech_weight = dplyr::coalesce(.data$ghg_index, mean(.data$ghg_index, na.rm = TRUE), 0.5),
      strategic_index =
        0.2 * .data$pol +
        0.25 * .data$Economic_Opportunity_Index +
        0.25 * .data$Energy_Security_Index +
        0.15 * .data$sc_weight +
        0.15 * .data$tech_weight,
      sector_label = paste(.data$tech, .data$supply_chain, sep = " - ")
    ) %>%
    dplyr::select(
      "Country",
      "tech",
      "supply_chain",
      "eo",
      "es_risk",
      "pol",
      "trl_index",
      "sc_weight",
      "tech_weight",
      "Economic_Opportunity_Index",
      "Energy_Security_Index",
      "strategic_index",
      "sector_label"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index))

  if (!is.null(techs)) {
    strategic_tbl <- strategic_tbl %>% dplyr::filter(.data$tech %in% techs)
  }

  if (isTRUE(include_sub_sector) && "sub_sector" %in% colnames(eo_tbl)) {
    strategic_tbl <- strategic_tbl %>%
      dplyr::left_join(
        eo_tbl %>%
          dplyr::filter(.data$Country == country_name) %>%
          dplyr::select("Country", "tech", "supply_chain", "sub_sector") %>%
          dplyr::distinct(),
        by = c("Country", "tech", "supply_chain")
      )
  }

  strategic_tbl
}

plot_country_heatmap <- function(strategic_tbl, selected_sector_labels = NULL) {
  plot_tbl <- strategic_tbl %>%
    dplyr::mutate(
      supply_chain = factor(.data$supply_chain, levels = c("Upstream", "Midstream", "Downstream")),
      selected = .data$sector_label %in% (selected_sector_labels %||% character())
    )

  p <- ggplot(plot_tbl, aes(x = .data$supply_chain, y = .data$tech, fill = .data$strategic_index)) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradientn(
      colours = c("#eff3ff", "#6baed6", "#084594"),
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Strategic index",
      title = "Strategic index heatmap"
    ) +
    theme_minimal(base_size = 11)

  if (!is.null(selected_sector_labels) && length(selected_sector_labels) > 0) {
    p <- p +
      geom_text(
        data = plot_tbl %>% dplyr::filter(.data$selected),
        aes(label = "Selected"),
        size = 3,
        color = "black"
      )
  }

  p
}

plot_country_scatter <- function(strategic_tbl, selected_sector_labels = NULL) {
  ggplot(
    strategic_tbl %>%
      dplyr::mutate(selected = .data$sector_label %in% (selected_sector_labels %||% character())),
    aes(x = .data$eo, y = .data$es_risk, color = .data$supply_chain)
  ) +
    geom_point(size = 2.8, alpha = 0.9) +
    geom_text(
      aes(label = ifelse(.data$selected, .data$sector_label, "")),
      nudge_y = 0.02,
      check_overlap = TRUE,
      size = 3,
      show.legend = FALSE
    ) +
    scale_color_brewer(palette = "Set2") +
    labs(
      x = "Economic opportunity (S-curve)",
      y = "Energy security risk (1 - S-curve)",
      color = "Supply chain",
      title = "EO vs ES-risk by sector"
    ) +
    theme_minimal(base_size = 11)
}

build_topn_contrib_tbl <- function(strategic_tbl, top_n = 10) {
  topn_wide <- strategic_tbl %>%
    dplyr::mutate(
      contrib_eo = 0.25 * .data$Economic_Opportunity_Index,
      contrib_es = 0.25 * .data$Energy_Security_Index,
      contrib_policy = 0.2 * .data$pol,
      contrib_sc = 0.15 * .data$sc_weight,
      contrib_climate = 0.15 * .data$tech_weight
    ) %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(
      "Country",
      "sector_label",
      "strategic_index",
      "contrib_eo",
      "contrib_es",
      "contrib_policy",
      "contrib_sc",
      "contrib_climate"
    )

  topn_long <- topn_wide %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("contrib_"),
      names_to = "component",
      values_to = "contribution"
    )

  attr(topn_wide, "topn_long_tbl") <- topn_long
  topn_wide
}

plot_topn_contrib <- function(topn_long_tbl) {
  ggplot(topn_long_tbl, aes(x = reorder(.data$sector_label, .data$contribution, FUN = sum), y = .data$contribution, fill = .data$component)) +
    geom_col() +
    coord_flip() +
    scale_fill_brewer(palette = "Set3") +
    labs(
      x = NULL,
      y = "Weighted contribution",
      fill = NULL,
      title = "Top sectors strategic-index decomposition"
    ) +
    theme_minimal(base_size = 11)
}

build_category_contrib_wide <- function(contrib_tbl,
                                        country,
                                        selected_sector_labels,
                                        pillar = c("ES", "EO")) {
  pillar <- match.arg(pillar)

  if (is.null(contrib_tbl) || nrow(contrib_tbl) == 0) {
    return(tibble::tibble())
  }

  country_name <- resolve_country_name(contrib_tbl, country)

  score_col <- if ("weighted_contribution" %in% colnames(contrib_tbl)) {
    "weighted_contribution"
  } else if ("contribution" %in% colnames(contrib_tbl)) {
    "contribution"
  } else if ("category_score" %in% colnames(contrib_tbl)) {
    "category_score"
  } else if ("value" %in% colnames(contrib_tbl)) {
    "value"
  } else {
    stop("Contribution table does not include a known contribution column.")
  }

  weights_tbl <- if ("weight" %in% colnames(contrib_tbl)) {
    contrib_tbl %>%
      dplyr::filter(.data$Country == country_name) %>%
      dplyr::group_by(.data$category) %>%
      dplyr::summarise(weight = dplyr::first(.data$weight), .groups = "drop")
  } else {
    NULL
  }

  long_tbl <- contrib_tbl %>%
    dplyr::filter(.data$Country == country_name) %>%
    dplyr::mutate(sector_label = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
    dplyr::filter(.data$sector_label %in% selected_sector_labels) %>%
    dplyr::select("category", "sector_label", value = dplyr::all_of(score_col))

  if (nrow(long_tbl) == 0) {
    return(tibble::tibble())
  }

  wide_tbl <- long_tbl %>%
    tidyr::pivot_wider(names_from = .data$sector_label, values_from = .data$value)

  if (!is.null(weights_tbl)) {
    wide_tbl <- wide_tbl %>% dplyr::left_join(weights_tbl, by = "category")
  }

  wide_tbl
}

plot_category_contrib <- function(category_wide_tbl, pillar = c("ES", "EO")) {
  pillar <- match.arg(pillar)
  if (nrow(category_wide_tbl) == 0) {
    stop("category_wide_tbl is empty.")
  }

  long_tbl <- category_wide_tbl %>%
    tidyr::pivot_longer(
      cols = -dplyr::any_of(c("category", "weight")),
      names_to = "sector_label",
      values_to = "value"
    )

  ggplot(long_tbl, aes(x = .data$sector_label, y = .data$value, fill = .data$category)) +
    geom_col(position = "stack") +
    coord_flip() +
    labs(
      x = NULL,
      y = paste0(pillar, " category contribution"),
      fill = "Category",
      title = paste0(pillar, " category contributions by selected sector")
    ) +
    theme_minimal(base_size = 11)
}
