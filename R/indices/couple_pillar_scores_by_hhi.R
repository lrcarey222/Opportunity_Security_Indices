# Coupling helper to shrink pillar scores toward a tech-level chain score using interdependence edges.
couple_pillar_scores_by_hhi <- function(pillar_tbl,
                                        interdependence_tbl,
                                        score_col,
                                        include_sub_sector = FALSE,
                                        lambda_min = 0.15,
                                        lambda_max = 0.65,
                                        eps = 1e-6) {
  if (!inherits(pillar_tbl, "data.frame")) {
    stop("pillar_tbl must be a data.frame or tibble.")
  }
  if (!inherits(interdependence_tbl, "data.frame")) {
    stop("interdependence_tbl must be a data.frame or tibble.")
  }

  score_col_quo <- rlang::enquo(score_col)
  score_col_name <- rlang::as_name(score_col_quo)

  if (!score_col_name %in% names(pillar_tbl)) {
    stop("Score column not found in pillar_tbl: ", score_col_name)
  }

  pillar_tbl <- pillar_tbl %>%
    dplyr::mutate(
      Year = as.integer(Year),
      "{score_col_name}" := suppressWarnings(as.numeric(!!score_col_quo))
    )

  if (!(is.numeric(pillar_tbl[[score_col_name]]))) {
    stop("Score column must be numeric: ", score_col_name)
  }

  group_cols <- c("Country", "tech", "Year")
  if (include_sub_sector && "sub_sector" %in% names(pillar_tbl)) {
    group_cols <- c("Country", "tech", "sub_sector", "Year")
  }

  expected_stages <- c("Upstream", "Midstream", "Downstream")
  stage_counts <- pillar_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      stage_count = dplyr::n_distinct(supply_chain),
      missing_stages = list(setdiff(expected_stages, unique(supply_chain))),
      .groups = "drop"
    ) %>%
    dplyr::filter(stage_count < length(expected_stages))

  if (nrow(stage_counts) > 0) {
    missing_preview <- stage_counts %>%
      dplyr::mutate(
        missing_stages = vapply(
          missing_stages,
          function(items) paste(items, collapse = ", "),
          character(1)
        ),
        group_key = if (include_sub_sector && "sub_sector" %in% names(stage_counts)) {
          paste(Country, tech, sub_sector, Year, sep = " | ")
        } else {
          paste(Country, tech, Year, sep = " | ")
        }
      ) %>%
      dplyr::select(group_key, missing_stages) %>%
      head(10)

    warning(
      paste(
        "Supply-chain stages missing for",
        nrow(stage_counts),
        "group(s); computing chain score with available stages.",
        "Examples (Country | tech | Year -> missing stages):",
        paste(
          paste0(missing_preview$group_key, " -> ", missing_preview$missing_stages),
          collapse = "; "
        )
      )
    )
  }

  weight_candidates <- c("weight", "strength", "edge_weight", "value")
  weight_col <- weight_candidates[weight_candidates %in% names(interdependence_tbl)][1]
  if (is.na(weight_col)) {
    weight_col <- NULL
  }

  if (all(c("from", "to") %in% names(interdependence_tbl))) {
    edges <- interdependence_tbl %>%
      dplyr::transmute(
        tech = if ("tech" %in% names(interdependence_tbl)) .data$tech else NA_character_,
        from_stage = .data$from,
        to_stage = .data$to,
        weight = if (is.null(weight_col)) 1 else suppressWarnings(as.numeric(.data[[weight_col]]))
      )
  } else if (all(c("source", "target") %in% names(interdependence_tbl))) {
    edges <- interdependence_tbl %>%
      dplyr::transmute(
        tech = if ("tech" %in% names(interdependence_tbl)) .data$tech else NA_character_,
        from_stage = .data$source,
        to_stage = .data$target,
        weight = if (is.null(weight_col)) 1 else suppressWarnings(as.numeric(.data[[weight_col]]))
      )
  } else if (all(c("primary", "secondary", "tertiary") %in% names(interdependence_tbl))) {
    edges <- dplyr::bind_rows(
      interdependence_tbl %>%
        dplyr::transmute(
          tech = if ("tech" %in% names(interdependence_tbl)) .data$tech else NA_character_,
          from_stage = .data$primary,
          to_stage = .data$secondary,
          weight = if (is.null(weight_col)) 1 else suppressWarnings(as.numeric(.data[[weight_col]]))
        ),
      interdependence_tbl %>%
        dplyr::transmute(
          tech = if ("tech" %in% names(interdependence_tbl)) .data$tech else NA_character_,
          from_stage = .data$primary,
          to_stage = .data$tertiary,
          weight = if (is.null(weight_col)) 1 else suppressWarnings(as.numeric(.data[[weight_col]]))
        ),
      interdependence_tbl %>%
        dplyr::transmute(
          tech = if ("tech" %in% names(interdependence_tbl)) .data$tech else NA_character_,
          from_stage = .data$secondary,
          to_stage = .data$tertiary,
          weight = if (is.null(weight_col)) 1 else suppressWarnings(as.numeric(.data[[weight_col]]))
        )
    )
  } else {
    stop("interdependence_tbl must include columns (from, to), (source, target), or (primary, secondary, tertiary).")
  }

  edges <- edges %>%
    dplyr::mutate(
      weight = dplyr::if_else(is.na(weight), 1, weight),
      from_stage = as.character(from_stage),
      to_stage = as.character(to_stage)
    ) %>%
    dplyr::filter(!is.na(from_stage), !is.na(to_stage))

  edge_group_cols <- "supply_chain"
  if ("tech" %in% names(edges) && "tech" %in% names(pillar_tbl)) {
    edge_group_cols <- c("tech", "supply_chain")
  }

  interdependence_strength <- edges %>%
    tidyr::pivot_longer(
      cols = c("from_stage", "to_stage"),
      names_to = "endpoint",
      values_to = "supply_chain"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(edge_group_cols))) %>%
    dplyr::summarize(
      strength = sum(weight, na.rm = TRUE),
      .groups = "drop"
    )

  strength_group_cols <- setdiff(edge_group_cols, "supply_chain")
  if (length(strength_group_cols) > 0) {
    interdependence_strength <- interdependence_strength %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(strength_group_cols))) %>%
      dplyr::mutate(
        max_strength = max(strength, na.rm = TRUE),
        interdependence_norm = dplyr::if_else(max_strength > 0, strength / max_strength, 0)
      ) %>%
      dplyr::ungroup()
  } else {
    max_strength <- max(interdependence_strength$strength, na.rm = TRUE)
    if (!is.finite(max_strength)) {
      max_strength <- 0
    }
    interdependence_strength <- interdependence_strength %>%
      dplyr::mutate(
        max_strength = max_strength,
        interdependence_norm = dplyr::if_else(max_strength > 0, strength / max_strength, 0)
      )
  }

  interdependence_strength <- interdependence_strength %>%
    dplyr::mutate(
      interdependence_norm = pmin(pmax(interdependence_norm, 0), 1),
      lambda = lambda_min + (lambda_max - lambda_min) * interdependence_norm
    ) %>%
    dplyr::select(-max_strength, -interdependence_norm)

  chain_scores <- pillar_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      chain_score = {
        scores <- pmax(!!score_col_quo, eps)
        if (all(is.na(scores))) {
          NA_real_
        } else {
          exp(mean(log(scores), na.rm = TRUE))
        }
      },
      .groups = "drop"
    )

  score_coupled_col <- paste0(score_col_name, "_coupled")

  coupled <- pillar_tbl %>%
    dplyr::left_join(interdependence_strength, by = edge_group_cols) %>%
    dplyr::left_join(chain_scores, by = group_cols)

  coupled <- coupled %>%
    dplyr::mutate(
      lambda = dplyr::if_else(is.na(lambda), lambda_min, lambda),
      "{score_coupled_col}" := (1 - lambda) * !!score_col_quo + lambda * chain_score,
      "{score_coupled_col}" := pmin(pmax(.data[[score_coupled_col]], 0), 1)
    ) %>%
    dplyr::select(-lambda, -chain_score)

  coupled
}
