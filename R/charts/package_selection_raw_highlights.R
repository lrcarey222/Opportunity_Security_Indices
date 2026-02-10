# Package selection raw-highlights builders (pure functions; no file IO).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

resolve_variable_contrib_tbl <- function(index_outputs, pillar = c("EO", "ES")) {
  pillar <- match.arg(pillar)

  candidates <- if (pillar == "EO") {
    list(
      index_outputs$economic_opportunity_variable_contributions,
      index_outputs$economic_opportunity_outputs$variable_contributions,
      index_outputs$economic_opportunity_outputs$outputs$variable_contributions
    )
  } else {
    list(
      index_outputs$energy_security_variable_contributions,
      index_outputs$energy_security_outputs$variable_contributions,
      index_outputs$energy_security_outputs$outputs$variable_contributions
    )
  }

  for (cand in candidates) {
    if (!is.null(cand) && is.data.frame(cand) && nrow(cand) > 0) {
      return(tibble::as_tibble(cand))
    }
  }

  tibble::tibble()
}

resolve_contrib_col <- function(var_contrib_tbl) {
  if ("weighted_variable_contribution" %in% names(var_contrib_tbl)) {
    return("weighted_variable_contribution")
  }
  if ("weighted_contribution" %in% names(var_contrib_tbl)) {
    return("weighted_contribution")
  }
  if ("component_value" %in% names(var_contrib_tbl)) {
    return("component_value")
  }
  stop("Variable contribution table missing weighted contribution column.")
}


ensure_cols <- function(tbl, cols) {
  for (nm in cols) {
    if (!nm %in% names(tbl)) {
      tbl[[nm]] <- NA
    }
  }
  tbl
}

build_top_variable_contrib_long <- function(var_contrib_tbl, country, selected_sector_labels, top_k = 5) {
  if (is.null(var_contrib_tbl) || nrow(var_contrib_tbl) == 0) {
    return(tibble::tibble())
  }

  contrib_col <- resolve_contrib_col(var_contrib_tbl)
  country_name <- resolve_country_name(var_contrib_tbl, country)

  out <- var_contrib_tbl %>%
    ensure_cols(c("component_value", "category_weight", "component_weight_within_overall", "imputed", "missing_rule_applied")) %>%
    dplyr::filter(.data$Country == country_name) %>%
    dplyr::mutate(
      sector_label = paste(.data$tech, .data$supply_chain, sep = " - "),
      weighted_variable_contribution = .data[[contrib_col]],
      variable_label = paste(.data$category, .data$variable, sep = " :: "),
      abs_weighted_variable_contribution = abs(.data$weighted_variable_contribution)
    ) %>%
    dplyr::filter(.data$sector_label %in% selected_sector_labels) %>%
    dplyr::group_by(.data$sector_label) %>%
    dplyr::arrange(dplyr::desc(.data$abs_weighted_variable_contribution), .by_group = TRUE) %>%
    dplyr::slice_head(n = top_k) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      country = .data$Country,
      .data$tech,
      .data$supply_chain,
      .data$sector_label,
      .data$category,
      .data$variable,
      .data$variable_label,
      .data$weighted_variable_contribution,
      .data$component_value,
      .data$category_weight,
      .data$component_weight_within_overall,
      .data$imputed,
      .data$missing_rule_applied
    )

  out
}

build_top_variable_contrib_wide <- function(top_long_tbl) {
  if (is.null(top_long_tbl) || nrow(top_long_tbl) == 0) {
    return(tibble::tibble())
  }

  top_long_tbl %>%
    dplyr::select(.data$variable_label, .data$sector_label, .data$weighted_variable_contribution) %>%
    tidyr::pivot_wider(names_from = .data$sector_label, values_from = .data$weighted_variable_contribution)
}

normalize_year_like_themes <- function(Year) {
  year_chr <- as.character(Year)
  year_chr[is.na(year_chr)] <- ""
  out <- stringr::str_extract(year_chr, "(\\d{4})\\s*$")
  out <- as.integer(out)
  out[is.na(out)] <- 0L
  out
}

extract_raw_values_from_theme_tbls <- function(theme_tbls, country, top_long_tbl) {
  if (is.null(top_long_tbl) || nrow(top_long_tbl) == 0) {
    return(tibble::tibble())
  }
  if (is.null(theme_tbls) || length(theme_tbls) == 0) {
    return(top_long_tbl %>%
      dplyr::distinct(.data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable) %>%
      dplyr::mutate(Year = NA_character_, source = NA_character_, explanation = NA_character_, country_value = NA_real_, matched_table_file = NA_character_))
  }

  key_tbl <- top_long_tbl %>%
    dplyr::distinct(.data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable)

  country_rows <- purrr::imap_dfr(theme_tbls, function(tbl, nm) {
    if (!is.data.frame(tbl) || nrow(tbl) == 0) return(tibble::tibble())
    required <- c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value")
    if (!all(required %in% colnames(tbl))) return(tibble::tibble())

    tbl %>%
      dplyr::filter(.data$data_type == "raw") %>%
      dplyr::inner_join(key_tbl, by = c("tech", "supply_chain", "category", "variable")) %>%
      dplyr::filter(tolower(.data$Country) == tolower(country)) %>%
      dplyr::mutate(
        year_norm = normalize_year_like_themes(.data$Year),
        matched_table_file = nm
      ) %>%
      dplyr::group_by(.data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable) %>%
      dplyr::arrange(dplyr::desc(.data$year_norm), .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      ensure_cols(c("source", "explanation", "Year")) %>%
      dplyr::transmute(
        .data$sector_label,
        .data$tech,
        .data$supply_chain,
        .data$category,
        .data$variable,
        Year = as.character(.data$Year),
        source = as.character(.data$source),
        explanation = as.character(.data$explanation),
        country_value = as.numeric(.data$value),
        matched_table_file = .data$matched_table_file
      )
  })

  key_tbl %>%
    dplyr::left_join(country_rows, by = c("sector_label", "tech", "supply_chain", "category", "variable"))
}

compute_global_raw_stats <- function(theme_tbls, top_long_tbl) {
  if (is.null(top_long_tbl) || nrow(top_long_tbl) == 0 || is.null(theme_tbls) || length(theme_tbls) == 0) {
    return(tibble::tibble())
  }

  key_tbl <- top_long_tbl %>%
    dplyr::distinct(.data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable)

  all_rows <- purrr::imap_dfr(theme_tbls, function(tbl, nm) {
    if (!is.data.frame(tbl) || nrow(tbl) == 0) return(tibble::tibble())
    required <- c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value")
    if (!all(required %in% colnames(tbl))) return(tibble::tibble())

    tbl %>%
      dplyr::filter(.data$data_type == "raw") %>%
      dplyr::inner_join(key_tbl, by = c("tech", "supply_chain", "category", "variable")) %>%
      dplyr::mutate(year_norm = normalize_year_like_themes(.data$Year))
  })

  if (nrow(all_rows) == 0) {
    return(tibble::tibble())
  }

  latest_by_metric <- all_rows %>%
    dplyr::group_by(.data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable) %>%
    dplyr::summarise(Year_used = max(.data$year_norm, na.rm = TRUE), .groups = "drop")

  all_rows %>%
    dplyr::inner_join(latest_by_metric,
      by = c("sector_label", "tech", "supply_chain", "category", "variable")
    ) %>%
    dplyr::filter(.data$year_norm == .data$Year_used) %>%
    dplyr::group_by(.data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable, .data$Year_used) %>%
    dplyr::summarise(
      global_p25 = stats::quantile(.data$value, probs = 0.25, na.rm = TRUE, names = FALSE),
      global_p50 = stats::quantile(.data$value, probs = 0.50, na.rm = TRUE, names = FALSE),
      global_p75 = stats::quantile(.data$value, probs = 0.75, na.rm = TRUE, names = FALSE),
      global_min = suppressWarnings(min(.data$value, na.rm = TRUE)),
      global_max = suppressWarnings(max(.data$value, na.rm = TRUE)),
      .groups = "drop"
    )
}

build_raw_highlights_tbl <- function(theme_tbls, country, top_long_tbl) {
  if (is.null(top_long_tbl) || nrow(top_long_tbl) == 0) {
    return(tibble::tibble())
  }

  country_raw <- extract_raw_values_from_theme_tbls(theme_tbls = theme_tbls, country = country, top_long_tbl = top_long_tbl)
  global_stats <- compute_global_raw_stats(theme_tbls = theme_tbls, top_long_tbl = top_long_tbl)

  top_long_tbl %>%
    dplyr::distinct(.data$country, .data$sector_label, .data$tech, .data$supply_chain, .data$category, .data$variable, .data$variable_label) %>%
    dplyr::left_join(country_raw, by = c("sector_label", "tech", "supply_chain", "category", "variable")) %>%
    dplyr::left_join(global_stats, by = c("sector_label", "tech", "supply_chain", "category", "variable")) %>%
    dplyr::mutate(
      Year = dplyr::coalesce(.data$Year, as.character(.data$Year_used)),
      unmatched = is.na(.data$country_value)
    ) %>%
    dplyr::select(
      .data$country,
      .data$sector_label,
      .data$tech,
      .data$supply_chain,
      .data$category,
      .data$variable,
      .data$variable_label,
      .data$Year,
      .data$country_value,
      .data$global_p25,
      .data$global_p50,
      .data$global_p75,
      .data$global_min,
      .data$global_max,
      .data$source,
      .data$explanation,
      .data$matched_table_file,
      .data$unmatched
    )
}

plot_top_variable_contrib <- function(top_long_tbl, pillar = c("EO", "ES")) {
  pillar <- match.arg(pillar)

  ggplot(
    top_long_tbl,
    aes(x = .data$weighted_variable_contribution, y = reorder(.data$variable_label, .data$weighted_variable_contribution))
  ) +
    geom_col(fill = "#2c7fb8") +
    facet_wrap(~sector_label, scales = "free_y") +
    labs(
      x = "Weighted variable contribution",
      y = NULL,
      title = paste0(pillar, " top variable contributions (selected sectors)")
    ) +
    theme_minimal(base_size = 11)
}

plot_raw_highlights <- function(raw_highlights_tbl, pillar = c("EO", "ES")) {
  pillar <- match.arg(pillar)

  base <- raw_highlights_tbl %>% dplyr::filter(!is.na(.data$country_value))

  ggplot(base, aes(y = reorder(.data$variable_label, .data$country_value), x = .data$country_value)) +
    geom_segment(aes(x = .data$global_p25, xend = .data$global_p75, yend = reorder(.data$variable_label, .data$country_value)), linewidth = 1.2, color = "#9ecae1") +
    geom_point(aes(x = .data$global_p50), color = "#3182bd", size = 2, alpha = 0.9) +
    geom_point(color = "#cb181d", size = 2.2) +
    facet_wrap(~sector_label, scales = "free_x") +
    labs(
      x = "Country raw value (with global IQR and median)",
      y = NULL,
      title = paste0(pillar, " raw indicator highlights (selected sectors)")
    ) +
    theme_minimal(base_size = 11)
}
