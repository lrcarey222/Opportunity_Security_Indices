# Coupling helper to shrink pillar scores toward a tech-level chain score using HHI.
couple_pillar_scores_by_hhi <- function(pillar_tbl,
                                        hhi_tbl,
                                        score_col,
                                        hhi_col = NULL,
                                        lambda_min = 0.15,
                                        lambda_max = 0.65,
                                        h0 = 0.25,
                                        k = 10,
                                        eps = 1e-6) {
  if (!inherits(pillar_tbl, "data.frame")) {
    stop("pillar_tbl must be a data.frame or tibble.")
  }
  if (!inherits(hhi_tbl, "data.frame")) {
    stop("hhi_tbl must be a data.frame or tibble.")
  }

  score_col_quo <- rlang::enquo(score_col)
  score_col_name <- rlang::as_name(score_col_quo)

  if (!score_col_name %in% names(pillar_tbl)) {
    stop("Score column not found in pillar_tbl: ", score_col_name)
  }

  if (is.null(hhi_col)) {
    if ("HHI" %in% names(hhi_tbl)) {
      hhi_col <- rlang::sym("HHI")
    } else if ("hhi" %in% names(hhi_tbl)) {
      hhi_col <- rlang::sym("hhi")
    } else if ("value" %in% names(hhi_tbl)) {
      hhi_col <- rlang::sym("value")
    } else {
      stop("Unable to infer HHI column in hhi_tbl; provide hhi_col.")
    }
  } else {
    hhi_col <- rlang::ensym(hhi_col)
  }

  pillar_tbl <- pillar_tbl %>%
    dplyr::mutate(
      Year = as.integer(Year),
      "{score_col_name}" := suppressWarnings(as.numeric(!!score_col_quo))
    )

  if (!(is.numeric(pillar_tbl[[score_col_name]]))) {
    stop("Score column must be numeric: ", score_col_name)
  }

  expected_stages <- c("Upstream", "Midstream", "Downstream")
  stage_counts <- pillar_tbl %>%
    dplyr::group_by(Country, tech, Year) %>%
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
        group_key = paste(Country, tech, Year, sep = " | ")
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

  hhi_latest <- hhi_tbl %>%
    dplyr::filter(.data$supply_chain %in% c("Upstream", "Midstream")) %>%
    dplyr::mutate(Year = as.integer(Year))

  latest_year <- max(hhi_latest$Year, na.rm = TRUE)
  if (!is.finite(latest_year)) {
    warning("HHI table has no usable Year values; using lambda_min for all techs.")
    hhi_latest <- hhi_latest %>% dplyr::slice(0)
  } else {
    hhi_latest <- hhi_latest %>% dplyr::filter(Year == latest_year)
  }

  hhi_values <- hhi_latest %>%
    dplyr::mutate(hhi_value = suppressWarnings(as.numeric(!!hhi_col)))

  max_hhi <- max(hhi_values$hhi_value, na.rm = TRUE)
  hhi_values <- hhi_values %>%
    dplyr::mutate(
      hhi_norm = pmin(pmax(hhi_value, 0), 1)
      )
    

  hhi_by_tech <- hhi_values %>%
    dplyr::group_by(tech) %>%
    dplyr::summarize(
      hhi_tech = mean(hhi_norm, na.rm = TRUE),
      non_missing = sum(!is.na(hhi_norm)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(hhi_tech = dplyr::if_else(non_missing > 0, hhi_tech, NA_real_)) %>%
    dplyr::select(-non_missing)

  hhi_by_tech <- hhi_by_tech %>%
    dplyr::mutate(
      hhi_tech = pmin(pmax(hhi_tech, 0), 1),
      lambda = lambda_min + (lambda_max - lambda_min) * (1 / (1 + exp(-k * (hhi_tech - h0))))
    )

  chain_scores <- pillar_tbl %>%
    dplyr::group_by(Country, tech, Year) %>%
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
    dplyr::left_join(hhi_by_tech, by = "tech") %>%
    dplyr::left_join(chain_scores, by = c("Country", "tech", "Year"))

  missing_lambda <- coupled %>%
    dplyr::filter(is.na(lambda)) %>%
    dplyr::distinct(tech) %>%
    dplyr::pull(tech)

  coupled <- coupled %>%
    dplyr::mutate(
      lambda = dplyr::if_else(is.na(lambda), lambda_min, lambda),
      "{score_coupled_col}" := (1 - lambda) * !!score_col_quo + lambda * chain_score,
      "{score_coupled_col}" := pmin(pmax(.data[[score_coupled_col]], 0), 1)
    ) %>%
    dplyr::select(-lambda, -chain_score)

  if (length(missing_lambda) > 0) {
    warning(
      "Missing HHI for tech(s); using lambda_min: ",
      paste(missing_lambda, collapse = ", ")
    )
  }

  coupled
}
