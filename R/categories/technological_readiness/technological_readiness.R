# Technological readiness theme builder functions.
#
# Input is the IEA ETP Clean Energy Technology Guide. The 2026 public dataset changed shape
# from the extract the pipeline was originally written against:
#
#   legacy  name, sector (comma-joined taxonomy path), supplyChain, trl2020..trl2023
#   2026    tech.final.name, category.1..category.4, <theme>.cc cross-cutting columns,
#           TRL.2020..TRL.2025
#
# technological_readiness_normalize_iea() folds the newer layout back onto the legacy column
# names so one rule engine and one set of mapping rules serve both. Everything downstream is
# driven by whichever TRL years are actually present, so a release that adds TRL.2026 moves
# the theme forward without a code edit.

IEA_CLEAN_TECH_TRL_PATTERN <- "^trl20\\d{2}$"

# TRL columns present, ascending by year.
technological_readiness_trl_columns <- function(df) {
  cols <- names(df)[stringr::str_detect(tolower(names(df)), IEA_CLEAN_TECH_TRL_PATTERN)]
  cols[order(readr::parse_number(cols))]
}

technological_readiness_trl_years <- function(df) {
  as.integer(readr::parse_number(technological_readiness_trl_columns(df)))
}

# TRUE for the 2026-style public dataset (wide categories, TRL.YYYY, no supplyChain).
is_iea_clean_tech_public_dataset <- function(df) {
  any(c("tech.final.name", "tech final name") %in% names(df)) ||
    any(stringr::str_detect(names(df), "^category[.]\\d+$"))
}

# Join the non-empty values of a set of columns with commas, row-wise. Empty strings are
# dropped rather than kept as blanks so taxonomy_has_token() never sees an empty token.
iea_paste_columns <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  parts <- lapply(cols, function(col) trimws(as.character(df[[col]])))
  out <- vapply(seq_len(nrow(df)), function(i) {
    values <- vapply(parts, function(p) p[[i]], character(1))
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) NA_character_ else paste(values, collapse = ",")
  }, character(1))

  out
}

technological_readiness_normalize_iea <- function(iea_cleantech_all) {
  if (!is_iea_clean_tech_public_dataset(iea_cleantech_all)) {
    return(iea_cleantech_all)
  }

  df <- iea_cleantech_all
  names(df) <- gsub(" ", ".", names(df), fixed = TRUE)

  category_cols <- grep("^category[.]\\d+$", names(df), value = TRUE)
  category_cols <- category_cols[order(readr::parse_number(category_cols))]

  # The ".cc" columns are the 2026 dataset's cross-cutting classification (Power.cc =
  # "Generation", Energy.Storage.cc = "Batteries", ...). They carry the same signal the
  # legacy `supplyChain` column did, so the mapping rules read them the same way.
  cc_cols <- grep("[.]cc$", names(df), value = TRUE)

  normalized <- tibble::tibble(
    name = as.character(df[["tech.final.name"]]),
    sector = iea_paste_columns(df, category_cols),
    supplyChain = iea_paste_columns(df, cc_cols)
  )

  trl_cols <- grep("^TRL[.]20\\d{2}$", names(df), value = TRUE)
  for (col in trl_cols) {
    normalized[[paste0("trl", sub("^TRL[.]", "", col))]] <- df[[col]]
  }

  if ("tech.description" %in% names(df)) {
    normalized$description <- as.character(df[["tech.description"]])
  }
  if ("tech.nze.rationale" %in% names(df)) {
    normalized$NZErationale <- as.character(df[["tech.nze.rationale"]])
  }

  normalized
}

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
  iea_cleantech_all <- technological_readiness_normalize_iea(iea_cleantech_all)

  require_columns(iea_cleantech_all, c("sector", "name"), label = "iea_cleantech_all")

  trl_cols <- technological_readiness_trl_columns(iea_cleantech_all)
  if (length(trl_cols) == 0) {
    stop(
      "iea_cleantech_all carries no TRL columns. Expected trl20XX (legacy extract) or ",
      "TRL.20XX (IEA public dataset); found: ",
      paste(utils::head(names(iea_cleantech_all), 20), collapse = ", ")
    )
  }

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

  any_tokens <- null_coalesce(rule$any_tokens, character(0))
  supply_tokens <- null_coalesce(rule$any_supplychain_tokens, character(0))
  regex_patterns <- null_coalesce(rule$any_name_regex, character(0))

  has_any_tokens <- if (length(any_tokens) == 0) {
    NA
  } else {
    any(vapply(any_tokens, function(token) {
      taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token)
    }, logical(1)))
  }

  has_any_supplychain <- if (length(supply_tokens) == 0) {
    NA
  } else {
    any(vapply(supply_tokens, function(token) {
      stringr::str_detect(supply_chain, stringr::regex(token, ignore_case = TRUE))
    }, logical(1)))
  }

  has_any_name_regex <- if (length(regex_patterns) == 0) {
    NA
  } else {
    any(vapply(regex_patterns, function(pattern) {
      stringr::str_detect(name, stringr::regex(pattern, ignore_case = TRUE))
    }, logical(1)))
  }

  any_match_checks <- c(has_any_tokens, has_any_supplychain, has_any_name_regex)
  any_match_checks <- any_match_checks[!is.na(any_match_checks)]
  has_any_match <- if (length(any_match_checks) == 0) TRUE else any(any_match_checks)

  has_excluded <- any(vapply(null_coalesce(rule$exclude_tokens, character(0)), function(token) {
    taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token) ||
      stringr::str_detect(name, stringr::regex(token, ignore_case = TRUE)) ||
      stringr::str_detect(supply_chain, stringr::regex(token, ignore_case = TRUE))
  }, logical(1)))

  has_all_tokens && has_any_match && !has_excluded
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

# Baseline TRL for the momentum term: the earliest reading an item actually has inside the
# opening window of the release. The window is the first three TRL years present (trl2020
# through trl2022 in every release so far), which keeps items that the Guide only started
# tracking mid-series from scoring a spurious zero delta.
technological_readiness_start_candidates <- function(df, window = 3L) {
  utils::head(technological_readiness_trl_columns(df), window)
}

find_trl_start <- function(df, start_candidates = technological_readiness_start_candidates(df)) {
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

# End year of the release: the newest TRL column the data actually carries, so a Guide
# release that adds a year moves the theme forward on its own. An explicit year still wins,
# and is checked against the data rather than trusted.
technological_readiness_resolve_year_end <- function(df, year_end = NULL) {
  years <- technological_readiness_trl_years(df)
  if (length(years) == 0) {
    stop("No TRL columns found; cannot resolve the technological readiness end year.")
  }

  if (is.null(year_end)) {
    return(max(years))
  }

  year_end <- as.integer(year_end)
  if (!year_end %in% years) {
    stop(
      "Requested TRL end year ", year_end, " is not in the data. Available: ",
      paste(years, collapse = ", ")
    )
  }
  year_end
}

technological_readiness_resolve_year_start <- function(df, year_start = NULL) {
  years <- technological_readiness_trl_years(df)
  if (is.null(year_start)) {
    return(min(years))
  }
  as.integer(year_start)
}

# Start/end years carried by an aggregated tech table's trl_delta_<start>_<end> column.
technological_readiness_delta_window <- function(iea_tech) {
  delta_col <- grep("^trl_delta_\\d{4}_\\d{4}$", names(iea_tech), value = TRUE)
  if (length(delta_col) != 1) {
    stop(
      "Expected exactly one trl_delta_<start>_<end> column in the aggregated tech table; ",
      "found: ", if (length(delta_col) == 0) "none" else paste(delta_col, collapse = ", ")
    )
  }

  years <- as.integer(stringr::str_match(delta_col, "^trl_delta_(\\d{4})_(\\d{4})$")[1, 2:3])
  c(start = years[[1]], end = years[[2]])
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
                                               year_end = NULL,
                                               year_start = NULL,
                                               min_trl = 2,
                                               mu = 6,
                                               max_trl = 11,
                                               momentum_weight = 0.3,
                                               delta_cap = 3,
                                               gamma_momentum = 0.5) {
  year_end <- technological_readiness_resolve_year_end(iea_cleantech, year_end)
  year_start <- technological_readiness_resolve_year_start(iea_cleantech, year_start)
  end_col <- paste0("trl", year_end)
  delta_col <- paste0("trl_delta_", year_start, "_", year_end)

  iea_cleantech %>%
    dplyr::filter(.data$tech %in% techs) %>%
    dplyr::mutate(
      trl_end = .data[[end_col]],
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
      "{end_col}" := {
        values <- trl_end[!is.na(trl_end)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      "{delta_col}" := {
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
                                                  year = NULL,
                                                  year_start = NULL,
                                                  year_end = NULL) {
  # technological_readiness_build_tech() names its delta column trl_delta_<start>_<end>,
  # which is the only place the aggregated table still records the window it used.
  window <- technological_readiness_delta_window(iea_tech)
  year_end <- if (is.null(year_end)) window[["end"]] else as.integer(year_end)
  year_start <- if (is.null(year_start)) window[["start"]] else as.integer(year_start)
  year <- if (is.null(year)) year_end else as.integer(year)

  supply_chain_levels <- c("Upstream", "Midstream", "Downstream")
  end_col <- paste0("trl", year_end)
  delta_col <- paste0("trl_delta_", year_start, "_", year_end)
  end_label <- paste0("TRL ", year_end)
  delta_label <- paste0("TRL Δ ", year_start, "–", year_end)

  iea_tech %>%
    tidyr::crossing(supply_chain = supply_chain_levels) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(
        end_col, delta_col,
        "trl_level_index", "trl_momentum_index", "trl_index"
      )),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::case_when(
        variable %in% c(end_col, delta_col) ~ "raw",
        variable %in% c("trl_level_index", "trl_momentum_index", "trl_index") ~ "index",
        TRUE ~ "raw"
      ),
      variable = dplyr::case_when(
        variable == end_col ~ end_label,
        variable == delta_col ~ delta_label,
        variable == "trl_level_index" ~ "TRL Level Index",
        variable == "trl_momentum_index" ~ "TRL Momentum Index",
        variable == "trl_index" ~ "TRL Index",
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
        variable == end_label ~ "Mean end-year technology readiness level (TRL) from IEA Clean Tech Guide items mapped to each technology.",
        variable == delta_label ~ "Mean change in TRL from start-year to end-year across mapped IEA Clean Tech Guide items.",
        variable == "TRL Level Index" ~ "Goldilocks bell-curve score applied at item level to end-year TRL, then averaged by technology.",
        variable == "TRL Momentum Index" ~ "Scaled positive TRL change from start-year to end-year, capped and transformed, then averaged by technology.",
        variable == "TRL Index" ~ "Weighted blend of TRL Level Index and TRL Momentum Index averaged across mapped items.",
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
                                    year_end = NULL,
                                    year_start = NULL,
                                    min_trl = 2,
                                    mu = 6,
                                    max_trl = 11,
                                    momentum_weight = 0.3,
                                    delta_cap = 3,
                                    gamma_momentum = 0.5,
                                    verbose = FALSE,
                                    tech_map_rules = NULL) {
  iea_cleantech <- technological_readiness_clean(iea_cleantech_all)

  # Both years follow the release unless pinned: the newest TRL column is the end year and
  # the oldest is the start of the momentum window.
  year_end <- technological_readiness_resolve_year_end(iea_cleantech, year_end)
  year_start <- technological_readiness_resolve_year_start(iea_cleantech, year_start)

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
    year_end = year_end,
    year_start = year_start,
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
