parse_trl_value <- function(x) {
  x_chr <- as.character(x)

  dplyr::case_when(
    is.na(x_chr) ~ NA_real_,
    stringr::str_detect(x_chr, "-") ~ {
      parts <- stringr::str_split(x_chr, "-")
      purrr::map_dbl(parts, function(values) {
        numeric_values <- readr::parse_number(values)
        numeric_values <- numeric_values[!is.na(numeric_values)]
        if (length(numeric_values) == 0) NA_real_ else mean(numeric_values)
      })
    },
    TRUE ~ readr::parse_number(x_chr)
  )
}

technological_readiness_clean <- function(iea_cleantech_all) {
  require_columns(iea_cleantech_all, c("sector", "trl2023", "name"), label = "iea_cleantech_all")

  trl_cols <- names(iea_cleantech_all)[stringr::str_detect(names(iea_cleantech_all), "^trl20\\d{2}$")]

  iea_cleantech_all %>%
    dplyr::mutate(
      sector_raw = sector,
      dplyr::across(dplyr::all_of(trl_cols), parse_trl_value)
    ) %>%
    tidyr::separate(
      sector,
      into = c("sector1", "sector2", "sector3", "sector4"),
      sep = ",",
      fill = "right",
      extra = "merge"
    )
}

taxonomy_has_token <- function(sector_raw, token, sector4 = NULL) {
  pattern <- stringr::regex(
    paste0("(^|,)\\s*", stringr::str_replace_all(token, "([\\W])", "\\\\\\1"), "\\s*(,|$)"),
    ignore_case = TRUE
  )

  sector_raw_hit <- stringr::str_detect(dplyr::coalesce(as.character(sector_raw), ""), pattern)
  sector4_hit <- if (is.null(sector4)) {
    FALSE
  } else {
    stringr::str_detect(dplyr::coalesce(as.character(sector4), ""), pattern)
  }

  sector_raw_hit | sector4_hit
}

null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

resolve_repo_root <- function() {
  option_root <- getOption("opportunity_security.repo_root", default = NULL)
  if (!is.null(option_root) && file.exists(file.path(option_root, ".git"))) {
    return(option_root)
  }

  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

read_iea_tech_map_rules <- function(path = NULL) {
  map_path <- if (is.null(path)) {
    file.path(resolve_repo_root(), "config", "iea_clean_tech_guide_tech_map.yml")
  } else {
    path
  }

  if (!file.exists(map_path)) {
    stop("IEA clean tech mapping config not found: ", map_path)
  }

  map_cfg <- yaml::read_yaml(map_path)
  if (is.null(map_cfg$rules) || length(map_cfg$rules) == 0) {
    stop("IEA clean tech mapping config has no rules: ", map_path)
  }

  map_cfg$rules
}

rule_matches <- function(row, rule) {
  sector_raw <- row$sector_raw
  sector4 <- row$sector4
  name <- dplyr::coalesce(as.character(row$name), "")
  supply_chain <- dplyr::coalesce(as.character(row$supplyChain), "")

  has_all_tokens <- all(vapply(null_coalesce(rule$all_tokens, character(0)), function(token) {
    taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token)
  }, logical(1)))

  has_any_tokens <- {
    any_tokens <- null_coalesce(rule$any_tokens, character(0))
    if (length(any_tokens) == 0) TRUE else any(vapply(any_tokens, function(token) {
      taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token)
    }, logical(1)))
  }

  has_any_supplychain <- {
    supply_tokens <- null_coalesce(rule$any_supplychain_tokens, character(0))
    if (length(supply_tokens) == 0) {
      TRUE
    } else {
      any(vapply(supply_tokens, function(token) {
        stringr::str_detect(supply_chain, stringr::regex(token, ignore_case = TRUE))
      }, logical(1)))
    }
  }

  has_any_name_regex <- {
    regex_patterns <- null_coalesce(rule$any_name_regex, character(0))
    if (length(regex_patterns) == 0) {
      TRUE
    } else {
      any(vapply(regex_patterns, function(pattern) {
        stringr::str_detect(name, stringr::regex(pattern, ignore_case = TRUE))
      }, logical(1)))
    }
  }

  has_excluded <- any(vapply(null_coalesce(rule$exclude_tokens, character(0)), function(token) {
    taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token) ||
      stringr::str_detect(name, stringr::regex(token, ignore_case = TRUE)) ||
      stringr::str_detect(supply_chain, stringr::regex(token, ignore_case = TRUE))
  }, logical(1)))

  has_all_tokens && has_any_tokens && has_any_supplychain && has_any_name_regex && !has_excluded
}

assign_tech_from_iea <- function(row, map_rules) {
  for (rule in map_rules) {
    if (rule_matches(row, rule)) {
      tech <- rule$tech
      return(as.character(unlist(tech, use.names = FALSE)))
    }
  }
  NA_character_
}

technological_readiness_assign_tech <- function(clean_tbl, map_rules = read_iea_tech_map_rules()) {
  require_columns(clean_tbl, c("sector_raw", "name"), label = "clean_tbl")

  if (!"supplyChain" %in% names(clean_tbl)) {
    clean_tbl <- dplyr::mutate(clean_tbl, supplyChain = NA_character_)
  }

  clean_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tech = list(assign_tech_from_iea(dplyr::cur_data(), map_rules = map_rules))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(tech) %>%
    dplyr::mutate(tech = dplyr::na_if(tech, "NA"))
}

find_trl_start <- function(df, start_candidates = c("trl2020", "trl2021", "trl2022")) {
  candidates <- intersect(start_candidates, names(df))
  if (length(candidates) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  mat <- as.matrix(df[, candidates, drop = FALSE])
  apply(mat, 1, function(values) {
    numeric_values <- suppressWarnings(as.numeric(values))
    numeric_values <- numeric_values[!is.na(numeric_values)]
    if (length(numeric_values) == 0) NA_real_ else numeric_values[1]
  })
}

technological_readiness_mapping_diagnostics <- function(clean_tbl, techs, top_n = 10) {
  assigned_counts <- clean_tbl %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::count(tech, sort = TRUE)

  unmapped <- clean_tbl %>%
    dplyr::filter(is.na(tech))

  list(
    counts_by_assigned_tech = assigned_counts,
    pct_unmapped = if (nrow(clean_tbl) == 0) NA_real_ else nrow(unmapped) / nrow(clean_tbl),
    top_unmapped_items = unmapped %>%
      dplyr::select(name, sector_raw, supplyChain) %>%
      dplyr::distinct() %>%
      dplyr::slice_head(n = top_n),
    requested_techs = techs
  )
}

trl_bell_hard <- function(x, min_trl = 2, mu = 6, max_trl = 11) {
  left_w <- mu - min_trl
  right_w <- max_trl - mu

  y <- dplyr::case_when(
    is.na(x) ~ NA_real_,
    x <= min_trl | x >= max_trl ~ 0,
    x <= mu ~ cos((pi / 2) * (mu - x) / left_w),
    TRUE ~ cos((pi / 2) * (x - mu) / right_w)
  )

  pmax(0, pmin(1, y))
}

technological_readiness_build_tech <- function(iea_cleantech,
                                               techs = c(
                                                 "Electric Vehicles",
                                                 "Nuclear",
                                                 "Coal",
                                                 "Batteries",
                                                 "Green Hydrogen",
                                                 "Wind",
                                                 "Oil",
                                                 "Solar",
                                                 "Gas",
                                                 "Geothermal",
                                                 "Electric Grid"
                                               ),
                                               min_trl = 2,
                                               mu = 6,
                                               max_trl = 11,
                                               momentum_weight = 0.3,
                                               delta_cap = 3,
                                               gamma_momentum = 0.5) {
  iea_cleantech %>%
    dplyr::filter(.data$tech %in% techs) %>%
    dplyr::mutate(
      trl_end = .data$trl2023,
      trl_start = find_trl_start(dplyr::pick(dplyr::everything())),
      trl_delta = .data$trl_end - .data$trl_start,
      trl_level_index_item = trl_bell_hard(.data$trl_end, min_trl = min_trl, mu = mu, max_trl = max_trl),
      delta_pos = pmax(0, .data$trl_delta),
      momentum_raw = pmin(.data$delta_pos, delta_cap),
      trl_momentum_index_item = dplyr::if_else(
        is.na(.data$momentum_raw),
        NA_real_,
        (.data$momentum_raw / delta_cap)^gamma_momentum
      ),
      trl_index_item = (1 - momentum_weight) * .data$trl_level_index_item + momentum_weight * .data$trl_momentum_index_item
    ) %>%
    dplyr::group_by(.data$tech) %>%
    dplyr::summarize(
      trl2023 = {
        values <- trl_end[!is.na(trl_end)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      `trl_delta_2020_2023` = {
        values <- trl_delta[!is.na(trl_delta)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      trl_level_index = {
        values <- trl_level_index_item[!is.na(trl_level_index_item)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      trl_momentum_index = {
        values <- trl_momentum_index_item[!is.na(trl_momentum_index_item)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      trl_index = {
        values <- trl_index_item[!is.na(trl_index_item)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      n_items = sum(!is.na(trl_end)),
      .groups = "drop"
    ) %>%
    dplyr::right_join(tibble::tibble(tech = techs), by = "tech")
}

technological_readiness_build_indices <- function(iea_tech,
                                                  year = 2023L,
                                                  year_start = 2020L,
                                                  year_end = 2023L) {
  supply_chain_levels <- c("Upstream", "Midstream", "Downstream")
  delta_label <- paste0("TRL Δ ", year_start, "–", year_end)

  iea_tech %>%
    tidyr::crossing(supply_chain = supply_chain_levels) %>%
    tidyr::pivot_longer(
      cols = c(trl2023, trl_delta_2020_2023, trl_level_index, trl_momentum_index, trl_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::case_when(
        variable %in% c("trl2023", "trl_delta_2020_2023") ~ "raw",
        variable %in% c("trl_level_index", "trl_momentum_index", "trl_index") ~ "index",
        TRUE ~ "raw"
      ),
      variable = dplyr::case_when(
        variable == "trl2023" ~ "TRL 2023",
        variable == "trl_delta_2020_2023" ~ delta_label,
        variable == "trl_level_index" ~ "TRL Level Index",
        variable == "trl_momentum_index" ~ "TRL Momentum Index",
        variable == "trl_index" ~ "Overall Technology Readiness Index",
        TRUE ~ variable
      )
    ) %>%
    dplyr::transmute(
      tech,
      supply_chain,
      category = "Technological Readiness",
      variable,
      data_type,
      value,
      Year = as.integer(year),
      source = "IEA Clean Tech Guide",
      explanation = dplyr::case_when(
        variable == "TRL 2023" ~ "Mean end-year technology readiness level (TRL) from IEA Clean Tech Guide items mapped to each technology.",
        variable == delta_label ~ "Mean change in TRL from start-year to end-year across mapped IEA Clean Tech Guide items.",
        variable == "TRL Level Index" ~ "Goldilocks bell-curve score applied at item level to end-year TRL, then averaged by technology.",
        variable == "TRL Momentum Index" ~ "Scaled positive TRL change from start-year to end-year, capped and transformed, then averaged by technology.",
        variable == "Overall Technology Readiness Index" ~ "Weighted blend of TRL Level Index and TRL Momentum Index averaged across mapped items.",
        TRUE ~ NA_character_
      )
    )
}

technological_readiness <- function(iea_cleantech_all,
                                    techs = c(
                                      "Electric Vehicles",
                                      "Nuclear",
                                      "Coal",
                                      "Batteries",
                                      "Green Hydrogen",
                                      "Wind",
                                      "Oil",
                                      "Solar",
                                      "Gas",
                                      "Geothermal",
                                      "Electric Grid"
                                    ),
                                    year_end = 2023L,
                                    year_start = 2020L,
                                    min_trl = 2,
                                    mu = 6,
                                    max_trl = 11,
                                    momentum_weight = 0.3,
                                    delta_cap = 3,
                                    gamma_momentum = 0.5,
                                    verbose = FALSE,
                                    tech_map_rules = NULL) {
  iea_cleantech <- technological_readiness_clean(iea_cleantech_all)
  map_rules <- null_coalesce(tech_map_rules, read_iea_tech_map_rules())

  iea_with_tech <- technological_readiness_assign_tech(iea_cleantech, map_rules = map_rules)
  diagnostics <- technological_readiness_mapping_diagnostics(iea_with_tech, techs = techs)

  if (isTRUE(verbose)) {
    message(sprintf("Technological Readiness mapping: %.1f%% unmapped", diagnostics$pct_unmapped * 100))
    if (nrow(diagnostics$top_unmapped_items) > 0) {
      message("Top unmapped items:")
      print(diagnostics$top_unmapped_items %>% dplyr::slice_head(n = 10))
    }
  }

  iea_tech <- technological_readiness_build_tech(
    iea_with_tech,
    techs = techs,
    min_trl = min_trl,
    mu = mu,
    max_trl = max_trl,
    momentum_weight = momentum_weight,
    delta_cap = delta_cap,
    gamma_momentum = gamma_momentum
  )

  readiness_tbl <- technological_readiness_build_indices(
    iea_tech,
    year = year_end,
    year_start = year_start,
    year_end = year_end
  )

  out <- readiness_tbl %>%
    dplyr::mutate(
      Country = "Global",
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      value = suppressWarnings(as.numeric(value)),
      Year = as.integer(year_end),
      source = as.character(source),
      explanation = as.character(explanation)
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation
    )

  attr(out, "mapping_diagnostics") <- diagnostics
  out
}
