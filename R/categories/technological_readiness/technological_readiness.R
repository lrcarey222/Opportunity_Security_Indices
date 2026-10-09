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
# names, and also keeps its taxonomy levels (category1..category4) and its cross-cutting tags
# as "<theme>=<value>" pairs, which the 2026 mapping rules read. Everything downstream is
# driven by whichever TRL years are actually present, so a release that adds TRL.2026 moves
# the theme forward without a code edit.
#
# The two layouts also use different TRL scales. The legacy extract runs TRL 1-11 (10 =
# integration needed at scale, 11 = proof of stability), the scale the level score's bell is
# built on. The 2026 release stops at TRL 9 in every year and grades TRL 9 items instead by a
# post-TRL-9 adoption stage (Post.index.2025: Awaiting adoption / Building momentum /
# Commercial in many markets). technological_readiness_extended_trl() reads that stage back
# onto the 1-11 scale for the end-year level; the TRL change behind the momentum term stays on
# the scale the release reports for every year.
#
# Each Guide item is mapped to a technology and a supply-chain stage by the ordered rules in
# config/iea_clean_tech_guide_tech_map.yml (or classified as outside the index technologies).
# A technology-stage cell averages the items mapped to it; a stage no item maps to takes the
# technology-wide value. The legacy extract has no stage information, so its items count for
# all three stages.
#
# TRL is a property of the technology, not of a country: every index here is one value per
# technology and stage, identical across countries. technological_readiness() copies it to
# each country in `country_reference`, so the pillar reads it as an observed input rather
# than imputing a single "Global" row to every country (99.5% of its values were flagged
# imputed). Within a technology-stage cell it is a constant: it moves the cell's level
# relative to other cells and cannot rank countries inside it.

IEA_CLEAN_TECH_TRL_PATTERN <- "^trl20\\d{2}$"

TECHNOLOGICAL_READINESS_STAGES <- c("Upstream", "Midstream", "Downstream")

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

# Cross-cutting tags of the 2026 dataset as "<theme>=<value>" pairs joined by ";", e.g.
# "Hydrogen=Direct use;Power=Generation". Unlike `supplyChain`, which joins the bare values,
# the theme is kept: "Storage" means different things under Power.cc, Heat.cc and Hydrogen.cc.
iea_cross_cutting_tags <- function(df, cc_cols) {
  cc_cols <- intersect(cc_cols, names(df))
  if (length(cc_cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  themes <- gsub("[.]", " ", sub("[.]cc$", "", cc_cols))
  parts <- lapply(seq_along(cc_cols), function(j) {
    values <- trimws(as.character(df[[cc_cols[[j]]]]))
    ifelse(is.na(values) | !nzchar(values), NA_character_, paste0(themes[[j]], "=", values))
  })

  vapply(seq_len(nrow(df)), function(i) {
    tags <- vapply(parts, function(p) p[[i]], character(1))
    tags <- tags[!is.na(tags)]
    if (length(tags) == 0) NA_character_ else paste(tags, collapse = ";")
  }, character(1))
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
  # legacy `supplyChain` column did, so the legacy rules read them the same way.
  cc_cols <- grep("[.]cc$", names(df), value = TRUE)

  normalized <- tibble::tibble(
    name = as.character(df[["tech.final.name"]]),
    sector = iea_paste_columns(df, category_cols),
    supplyChain = iea_paste_columns(df, cc_cols),
    cross_cutting = iea_cross_cutting_tags(df, cc_cols)
  )

  # The taxonomy levels one by one: several of them contain commas ("Heating, cooling and
  # ventilation technologies"), so they cannot be recovered from the joined `sector`.
  for (i in seq_along(category_cols)) {
    normalized[[paste0("category", i)]] <- trimws(as.character(df[[category_cols[[i]]]]))
  }

  trl_cols <- grep("^TRL[.]20\\d{2}$", names(df), value = TRUE)
  for (col in trl_cols) {
    normalized[[paste0("trl", sub("^TRL[.]", "", col))]] <- df[[col]]
  }

  # Post-TRL-9 adoption stage by year ("Post.index.2025"), blank below TRL 9.
  post_cols <- grep("^Post[.]index[.]20\\d{2}$", names(df), value = TRUE)
  for (col in post_cols) {
    values <- trimws(as.character(df[[col]]))
    normalized[[paste0("post_trl9_stage_", sub("^Post[.]index[.]", "", col))]] <-
      dplyr::if_else(is.na(values) | !nzchar(values), NA_character_, values)
  }

  if ("tech.description" %in% names(df)) {
    normalized$description <- as.character(df[["tech.description"]])
  }
  if ("tech.nze.rationale" %in% names(df)) {
    normalized$NZErationale <- as.character(df[["tech.nze.rationale"]])
  }

  normalized$guide_layout <- "public_dataset"
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

  if (!"guide_layout" %in% names(iea_cleantech_all)) {
    iea_cleantech_all$guide_layout <- "legacy"
  }
  if (!"cross_cutting" %in% names(iea_cleantech_all)) {
    iea_cleantech_all$cross_cutting <- NA_character_
  }

  # The legacy extract has no separate taxonomy columns; its comma-joined path is the best
  # available stand-in for path rules.
  if (!"category1" %in% names(iea_cleantech_all)) {
    levels <- stringr::str_split(dplyr::coalesce(as.character(iea_cleantech_all$sector), ""), ",")
    for (k in 1:4) {
      iea_cleantech_all[[paste0("category", k)]] <- vapply(levels, function(v) {
        value <- if (length(v) >= k) trimws(v[[k]]) else NA_character_
        if (is.na(value) || !nzchar(value)) NA_character_ else value
      }, character(1))
    }
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

# The mapping config: `rules` for the 2026 public dataset and `legacy_rules` for the
# hand-built extract. technological_readiness_select_rules() picks the set that fits the
# layout it is handed.
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

  technological_readiness_validate_rules(map_cfg$rules, label = "rules")
  if (!is.null(map_cfg$legacy_rules)) {
    technological_readiness_validate_rules(map_cfg$legacy_rules, label = "legacy_rules")
  }

  map_cfg[intersect(c("rules", "legacy_rules"), names(map_cfg))]
}

TECHNOLOGICAL_READINESS_RULE_CONDITIONS <- c(
  "path", "names", "all_tokens", "any_tokens", "any_supplychain_tokens", "any_cc", "any_name_regex"
)

# A rule needs a tech ("none" for an out-of-scope class) and at least one condition: a rule
# without conditions would claim every item that reaches it.
technological_readiness_validate_rules <- function(rules, label = "rules") {
  for (j in seq_along(rules)) {
    rule <- rules[[j]]
    id <- null_coalesce(rule$id, paste0(label, "[", j, "]"))
    if (is.null(rule$tech) || length(unlist(rule$tech)) == 0) {
      stop("IEA tech map rule ", id, " has no tech.")
    }
    if (!any(TECHNOLOGICAL_READINESS_RULE_CONDITIONS %in% names(rule))) {
      stop("IEA tech map rule ", id, " has no matching condition.")
    }
    stages <- unlist(rule$stage)
    if (!is.null(stages) && !all(stages %in% TECHNOLOGICAL_READINESS_STAGES)) {
      stop(
        "IEA tech map rule ", id, " has an unknown stage: ",
        paste(setdiff(stages, TECHNOLOGICAL_READINESS_STAGES), collapse = ", ")
      )
    }
  }
  invisible(rules)
}

# A config (named list with `rules` / `legacy_rules`) yields the set for the table's layout;
# a bare list of rules is used as given.
technological_readiness_select_rules <- function(map_rules, clean_tbl) {
  is_config <- is.list(map_rules) && !is.null(names(map_rules)) && "rules" %in% names(map_rules)
  if (!is_config) {
    return(map_rules)
  }

  legacy <- "guide_layout" %in% names(clean_tbl) && all(clean_tbl$guide_layout == "legacy")
  if (legacy && !is.null(map_rules$legacy_rules)) {
    return(map_rules$legacy_rules)
  }
  map_rules$rules
}

# Case- and space-insensitive label; the Guide writes "H₂" with a subscript two.
iea_normalize_label <- function(x) {
  x <- tolower(stringr::str_squish(as.character(x)))
  gsub("₂", "2", x, fixed = TRUE)
}

# `path` as a list of prefixes: a character vector is one prefix, a list holds alternatives.
rule_path_list <- function(path) {
  if (is.null(path)) {
    return(list())
  }
  if (is.list(path)) {
    return(lapply(path, function(p) as.character(unlist(p))))
  }
  list(as.character(path))
}

# TRUE where the item's category1, category2, ... start with `prefix`.
iea_path_matches <- function(tbl, prefix) {
  prefix <- iea_normalize_label(prefix)
  hit <- rep(TRUE, nrow(tbl))
  for (k in seq_along(prefix)) {
    col <- paste0("category", k)
    value <- if (col %in% names(tbl)) iea_normalize_label(tbl[[col]]) else rep(NA_character_, nrow(tbl))
    hit <- hit & !is.na(value) & value == prefix[[k]]
  }
  hit
}

# TRUE where any of the item's "<theme>=<value>" tags is one of `tags`.
iea_cc_has_tag <- function(cross_cutting, tags) {
  wanted <- iea_normalize_label(tags)
  item_tags <- strsplit(dplyr::coalesce(as.character(cross_cutting), ""), ";", fixed = TRUE)
  vapply(item_tags, function(t) any(iea_normalize_label(t) %in% wanted), logical(1))
}

# Rule evaluated on every row of the cleaned table at once. `path`, `names` and `all_tokens`
# must all hold; the "any" conditions are pooled so one hit among them is enough; any
# `exclude_tokens` hit vetoes the match.
rule_matches <- function(tbl, rule) {
  n <- nrow(tbl)
  sector_raw <- tbl$sector_raw
  sector4 <- tbl$sector4
  name <- dplyr::coalesce(as.character(tbl$name), "")
  supply_chain <- dplyr::coalesce(as.character(tbl$supplyChain), "")
  cross_cutting <- if ("cross_cutting" %in% names(tbl)) tbl$cross_cutting else rep(NA_character_, n)
  any_of <- function(hits) Reduce(`|`, hits, rep(FALSE, n))

  hit <- rep(TRUE, n)

  for (token in null_coalesce(rule$all_tokens, character(0))) {
    hit <- hit & taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token)
  }

  paths <- rule_path_list(rule$path)
  if (length(paths) > 0) {
    hit <- hit & any_of(lapply(paths, function(p) iea_path_matches(tbl, p)))
  }

  rule_names <- unlist(rule$names)
  if (length(rule_names) > 0) {
    hit <- hit & iea_normalize_label(name) %in% iea_normalize_label(rule_names)
  }

  any_checks <- list()
  any_tokens <- unlist(rule$any_tokens)
  if (length(any_tokens) > 0) {
    any_checks$tokens <- any_of(lapply(any_tokens, function(token) {
      taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token)
    }))
  }
  supply_tokens <- unlist(rule$any_supplychain_tokens)
  if (length(supply_tokens) > 0) {
    any_checks$supply_chain <- any_of(lapply(supply_tokens, function(token) {
      stringr::str_detect(supply_chain, stringr::regex(token, ignore_case = TRUE))
    }))
  }
  cc_tags <- unlist(rule$any_cc)
  if (length(cc_tags) > 0) {
    any_checks$cross_cutting <- iea_cc_has_tag(cross_cutting, cc_tags)
  }
  regex_patterns <- unlist(rule$any_name_regex)
  if (length(regex_patterns) > 0) {
    any_checks$name <- any_of(lapply(regex_patterns, function(pattern) {
      stringr::str_detect(name, stringr::regex(pattern, ignore_case = TRUE))
    }))
  }
  if (length(any_checks) > 0) {
    hit <- hit & any_of(any_checks)
  }

  for (token in null_coalesce(rule$exclude_tokens, character(0))) {
    excluded <- taxonomy_has_token(sector_raw = sector_raw, sector4 = sector4, token = token) |
      stringr::str_detect(name, stringr::regex(token, ignore_case = TRUE)) |
      stringr::str_detect(supply_chain, stringr::regex(token, ignore_case = TRUE))
    hit <- hit & !excluded
  }

  hit
}

technological_readiness_rule_id <- function(rule, j) {
  null_coalesce(rule$id, paste0("rule_", j))
}

# One row per item x technology x stage. Items no index technology claims stay as a single
# row with tech = NA and mapping_status "out_of_scope" (a rule classified them, with a
# reason) or "unclassified" (no rule matched).
technological_readiness_assign_tech <- function(clean_tbl, map_rules = read_iea_tech_map_rules()) {
  require_columns(clean_tbl, c("sector_raw", "name"), label = "clean_tbl")

  if (!"supplyChain" %in% names(clean_tbl)) {
    clean_tbl <- dplyr::mutate(clean_tbl, supplyChain = NA_character_)
  }

  rules <- technological_readiness_select_rules(map_rules, clean_tbl)
  n <- nrow(clean_tbl)
  rule_index <- rep(NA_integer_, n)
  for (j in seq_along(rules)) {
    open <- is.na(rule_index)
    if (!any(open)) {
      break
    }
    rule_index[open & rule_matches(clean_tbl, rules[[j]])] <- j
  }

  tech <- vector("list", n)
  stage <- vector("list", n)
  status <- rep("unclassified", n)
  rule_id <- rep(NA_character_, n)
  reason <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    j <- rule_index[[i]]
    if (is.na(j)) {
      tech[[i]] <- NA_character_
      stage[[i]] <- NA_character_
      next
    }
    rule <- rules[[j]]
    rule_id[[i]] <- technological_readiness_rule_id(rule, j)
    rule_tech <- as.character(unlist(rule$tech, use.names = FALSE))
    if (identical(tolower(rule_tech), "none")) {
      status[[i]] <- "out_of_scope"
      reason[[i]] <- null_coalesce(rule$reason, NA_character_)
      tech[[i]] <- NA_character_
      stage[[i]] <- NA_character_
    } else {
      status[[i]] <- "mapped"
      tech[[i]] <- rule_tech
      # A rule without a stage (the legacy rules) counts its items for every stage.
      stage[[i]] <- as.character(null_coalesce(unlist(rule$stage), TECHNOLOGICAL_READINESS_STAGES))
    }
  }

  clean_tbl %>%
    dplyr::mutate(
      tech = tech,
      stage = stage,
      mapping_status = status,
      mapping_rule = rule_id,
      mapping_reason = reason
    ) %>%
    tidyr::unnest(tech) %>%
    tidyr::unnest(stage) %>%
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

# Item-level mapping audit. `clean_tbl` is the output of technological_readiness_assign_tech().
technological_readiness_mapping_diagnostics <- function(clean_tbl, techs, top_n = 10) {
  if (!"stage" %in% names(clean_tbl)) {
    clean_tbl$stage <- NA_character_
  }
  if (!"mapping_status" %in% names(clean_tbl)) {
    clean_tbl$mapping_status <- dplyr::if_else(is.na(clean_tbl$tech), "unclassified", "mapped")
  }
  for (col in c("mapping_rule", "mapping_reason", "supplyChain", "cross_cutting")) {
    if (!col %in% names(clean_tbl)) {
      clean_tbl[[col]] <- NA_character_
    }
  }

  index_rows <- !is.na(clean_tbl$tech) & clean_tbl$tech %in% techs
  items <- clean_tbl %>%
    dplyr::mutate(in_index = index_rows) %>%
    dplyr::group_by(name, sector_raw) %>%
    dplyr::summarize(
      mapping_status = dplyr::first(mapping_status),
      mapping_rule = dplyr::first(mapping_rule),
      mapping_reason = dplyr::first(mapping_reason),
      tech = paste(sort(unique(stats::na.omit(tech))), collapse = " + "),
      stage = paste(intersect(TECHNOLOGICAL_READINESS_STAGES, stage), collapse = " + "),
      in_index = any(in_index),
      supplyChain = dplyr::first(supplyChain),
      cross_cutting = dplyr::first(cross_cutting),
      .groups = "drop"
    )

  mapped_rows <- clean_tbl[index_rows, , drop = FALSE]
  n_items <- nrow(items)
  unclassified <- items %>% dplyr::filter(.data$mapping_status == "unclassified")

  list(
    counts_by_assigned_tech = mapped_rows %>%
      dplyr::distinct(tech, name, sector_raw) %>%
      dplyr::count(tech, sort = TRUE),
    counts_by_tech_stage = mapped_rows %>%
      dplyr::distinct(tech, stage, name, sector_raw) %>%
      dplyr::count(tech, stage),
    # Share of Guide items that feed no index technology, whether deliberately (out of scope)
    # or because no rule matched them.
    pct_unmapped = if (n_items == 0) NA_real_ else mean(!items$in_index),
    pct_out_of_scope = if (n_items == 0) NA_real_ else mean(items$mapping_status == "out_of_scope"),
    pct_unclassified = if (n_items == 0) NA_real_ else nrow(unclassified) / n_items,
    out_of_scope_by_rule = items %>%
      dplyr::filter(.data$mapping_status == "out_of_scope") %>%
      dplyr::count(mapping_rule, mapping_reason, sort = TRUE),
    unclassified_items = unclassified %>% dplyr::select(name, sector_raw, supplyChain, cross_cutting),
    top_unmapped_items = items %>%
      dplyr::filter(!.data$in_index) %>%
      dplyr::arrange(.data$mapping_status != "unclassified") %>%
      dplyr::select(name, sector_raw, supplyChain) %>%
      dplyr::slice_head(n = top_n),
    item_table = items,
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

trl_mean_available <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

# TRL on the 1-11 scale for each post-TRL-9 adoption stage of the 2026 Guide. The IEA assesses
# a TRL 9 technology first for growing deployment in key markets and then for wide deployment
# in several regions under market forces (IEA commentary "Energy innovation has a
# commercialisation problem", 23 July 2026), the ground the retired TRL 10 and 11 covered. Of the
# Guide items that appear in both the legacy extract and the 2026 file, those the extract rated
# TRL 11 in 2023 are mostly "Commercial in many markets" (18 of 19), those rated TRL 10 mostly
# "Building momentum" (13 of 17), and those rated TRL 9 mostly "Awaiting adoption" (39 of 63);
# the 2026 file itself reads 9 for every one of the 37 rated 10 or 11.
IEA_POST_TRL9_SCALE <- c(
  "Awaiting adoption" = 9,
  "Building momentum" = 10,
  "Commercial in many markets" = 11
)

technological_readiness_post_trl9_value <- function(post_stage, scale = IEA_POST_TRL9_SCALE) {
  unname(scale[match(iea_normalize_label(post_stage), iea_normalize_label(names(scale)))])
}

# End-year TRL on the 1-11 scale the level score is built for. An item at TRL 9 takes the TRL its
# adoption stage stands for; so does an item whose TRL is blank but which carries a stage, since
# the Guide grades adoption only from TRL 9 on. Items below TRL 9, and releases without the
# stage column, keep their reported TRL.
technological_readiness_extended_trl <- function(trl, post_stage, scale = IEA_POST_TRL9_SCALE) {
  stage_trl <- technological_readiness_post_trl9_value(post_stage, scale)
  graduated <- !is.na(stage_trl) & (is.na(trl) | trl >= min(scale))
  dplyr::if_else(graduated, pmax(dplyr::coalesce(trl, stage_trl), stage_trl), as.numeric(trl))
}

# End-year TRL on the scale the release reports for every year, for the change behind the
# momentum term: the adoption stage has no earlier readings to compare with. A blank TRL with an
# adoption stage reads as TRL 9, the level from which the Guide grades adoption.
technological_readiness_series_trl <- function(trl, post_stage, scale = IEA_POST_TRL9_SCALE) {
  staged <- !is.na(technological_readiness_post_trl9_value(post_stage, scale))
  dplyr::if_else(is.na(trl) & staged, min(scale), as.numeric(trl))
}

# Per-item scores, then one row per technology x stage.
#
#   level     trl_bell_hard() of the item's end-year TRL on the 1-11 scale
#             (technological_readiness_extended_trl())
#   momentum  ((min(max(TRL_end - TRL_start, 0), delta_cap)) / delta_cap)^gamma_momentum, with
#             both TRLs on the scale the release reports for every year
#             (technological_readiness_series_trl()); missing for an item with no TRL in the
#             opening window (see find_trl_start())
#
# A cell's Level and Momentum indices are the means over its items that observe them, and its
# TRL Index is (1 - momentum_weight) * Level + momentum_weight * Momentum, so the published
# index is exactly the published blend of its two components. (Averaging item-level blends
# instead would drop every item without a start-year TRL from the index while keeping it in
# the Level index.) When no item of a stage observes a component, the cell takes the
# technology-wide value of that component, the mean over all of the technology's items:
# `basis` / `momentum_basis` record which one a cell carries.
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
                                               gamma_momentum = 0.5,
                                               post_trl9_scale = IEA_POST_TRL9_SCALE) {
  year_end <- technological_readiness_resolve_year_end(iea_cleantech, year_end)
  year_start <- technological_readiness_resolve_year_start(iea_cleantech, year_start)
  end_col <- paste0("trl", year_end)
  delta_col <- paste0("trl_delta_", year_start, "_", year_end)
  post_col <- paste0("post_trl9_stage_", year_end)
  if (!post_col %in% names(iea_cleantech)) {
    iea_cleantech[[post_col]] <- NA_character_
  }

  # An assignment without stages (an older caller) counts every item for every stage.
  if (!"stage" %in% names(iea_cleantech)) {
    iea_cleantech <- tidyr::crossing(iea_cleantech, stage = TECHNOLOGICAL_READINESS_STAGES)
  }
  item_key <- intersect(c("name", "sector_raw"), names(iea_cleantech))

  items <- iea_cleantech %>%
    dplyr::filter(.data$tech %in% techs) %>%
    dplyr::mutate(
      trl_end = technological_readiness_extended_trl(.data[[end_col]], .data[[post_col]], post_trl9_scale),
      trl_start = find_trl_start(dplyr::pick(dplyr::everything())),
      trl_delta = technological_readiness_series_trl(.data[[end_col]], .data[[post_col]], post_trl9_scale) -
        .data$trl_start,
      trl_level_index_item = trl_bell_hard(.data$trl_end, min_trl = min_trl, mu = mu, max_trl = max_trl),
      delta_pos = pmax(0, .data$trl_delta),
      momentum_raw = pmin(.data$delta_pos, delta_cap),
      trl_momentum_index_item = dplyr::if_else(
        is.na(.data$momentum_raw),
        NA_real_,
        (.data$momentum_raw / delta_cap)^gamma_momentum
      )
    )

  summarise_items <- function(df) {
    # The counts come first: summarize() evaluates in order, so after `trl_end = ...` the
    # name refers to the summary rather than the items.
    dplyr::summarize(
      df,
      n_items = sum(!is.na(.data$trl_end)),
      n_momentum = sum(!is.na(.data$trl_momentum_index_item)),
      trl_end = trl_mean_available(.data$trl_end),
      trl_delta = trl_mean_available(.data$trl_delta),
      trl_level_index = trl_mean_available(.data$trl_level_index_item),
      trl_momentum_index = trl_mean_available(.data$trl_momentum_index_item),
      .groups = "drop"
    )
  }

  stage_tbl <- items %>%
    dplyr::group_by(tech, supply_chain = stage) %>%
    summarise_items()

  # Technology-wide: each item once, however many stages it counts for.
  tech_tbl <- items[!duplicated(items[, c("tech", item_key), drop = FALSE]), , drop = FALSE] %>%
    dplyr::group_by(tech) %>%
    summarise_items()

  value_cols <- c("trl_end", "trl_delta", "trl_level_index", "trl_momentum_index", "n_items", "n_momentum")

  tidyr::crossing(tech = techs, supply_chain = TECHNOLOGICAL_READINESS_STAGES) %>%
    dplyr::left_join(stage_tbl, by = c("tech", "supply_chain")) %>%
    dplyr::left_join(
      dplyr::rename_with(tech_tbl, ~ paste0(.x, "_tech"), dplyr::all_of(value_cols)),
      by = "tech"
    ) %>%
    dplyr::mutate(
      n_items = dplyr::coalesce(.data$n_items, 0L),
      n_momentum = dplyr::coalesce(.data$n_momentum, 0L),
      n_items_tech = dplyr::coalesce(.data$n_items_tech, 0L),
      basis = dplyr::case_when(
        .data$n_items > 0 ~ "stage",
        .data$n_items_tech > 0 ~ "technology",
        TRUE ~ NA_character_
      ),
      momentum_basis = dplyr::case_when(
        .data$n_momentum > 0 ~ "stage",
        dplyr::coalesce(.data$n_momentum_tech, 0L) > 0 ~ "technology",
        TRUE ~ NA_character_
      ),
      trl_end = dplyr::if_else(.data$n_items > 0, .data$trl_end, .data$trl_end_tech),
      trl_level_index = dplyr::if_else(.data$n_items > 0, .data$trl_level_index, .data$trl_level_index_tech),
      trl_delta = dplyr::if_else(.data$n_momentum > 0, .data$trl_delta, .data$trl_delta_tech),
      trl_momentum_index = dplyr::if_else(
        .data$n_momentum > 0, .data$trl_momentum_index, .data$trl_momentum_index_tech
      ),
      trl_index = (1 - momentum_weight) * .data$trl_level_index + momentum_weight * .data$trl_momentum_index,
      momentum_weight = momentum_weight
    ) %>%
    dplyr::transmute(
      tech = .data$tech,
      supply_chain = .data$supply_chain,
      "{end_col}" := .data$trl_end,
      "{delta_col}" := .data$trl_delta,
      trl_level_index = .data$trl_level_index,
      trl_momentum_index = .data$trl_momentum_index,
      trl_index = .data$trl_index,
      n_items = .data$n_items,
      n_items_tech = .data$n_items_tech,
      basis = .data$basis,
      momentum_basis = .data$momentum_basis,
      momentum_weight = .data$momentum_weight
    ) %>%
    dplyr::mutate(supply_chain = factor(.data$supply_chain, levels = TECHNOLOGICAL_READINESS_STAGES)) %>%
    dplyr::arrange(match(.data$tech, techs), .data$supply_chain) %>%
    dplyr::mutate(supply_chain = as.character(.data$supply_chain))
}

# Where a cell's values come from, appended to every explanation.
technological_readiness_basis_note <- function(basis, momentum_basis, n_items, n_items_tech, variable_kind) {
  base <- dplyr::case_when(
    basis == "stage" ~ paste0("Stage-specific: ", n_items, " Guide item(s) mapped to this technology and stage."),
    basis == "technology" ~ paste0(
      "No Guide item maps to this stage: technology-wide value over ", n_items_tech, " item(s)."
    ),
    TRUE ~ "No Guide item maps to this technology."
  )
  momentum_note <- dplyr::if_else(
    variable_kind == "momentum" & basis == "stage" & dplyr::coalesce(momentum_basis, "") == "technology",
    " No item at this stage has a start-year TRL, so the momentum term is the technology-wide value.",
    ""
  )
  paste0(base, momentum_note)
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

  end_col <- paste0("trl", year_end)
  delta_col <- paste0("trl_delta_", year_start, "_", year_end)
  end_label <- paste0("TRL ", year_end)
  delta_label <- paste0("TRL Δ ", year_start, "–", year_end)

  # A technology-level table (no stages) is copied to every stage, as before stages existed.
  if (!"supply_chain" %in% names(iea_tech)) {
    iea_tech <- tidyr::crossing(iea_tech, supply_chain = TECHNOLOGICAL_READINESS_STAGES)
  }
  defaults <- list(
    n_items = NA_integer_, n_items_tech = NA_integer_, basis = "technology",
    momentum_basis = "technology", momentum_weight = 0.3
  )
  for (col in names(defaults)) {
    if (!col %in% names(iea_tech)) {
      iea_tech[[col]] <- defaults[[col]]
    }
  }

  iea_tech %>%
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
      variable_kind = dplyr::if_else(variable %in% c(delta_col, "trl_momentum_index"), "momentum", "level"),
      base_explanation = dplyr::case_when(
        variable == end_col ~ "Mean end-year technology readiness level (TRL, 1-11) of the IEA Clean Tech Guide items mapped to the technology and stage; where the Guide grades a TRL 9 item by its post-TRL-9 adoption stage, Awaiting adoption / Building momentum / Commercial in many markets read as TRL 9 / 10 / 11.",
        variable == delta_col ~ "Mean change in TRL from start-year to end-year across those items, on the scale the Guide reports for every year.",
        variable == "trl_level_index" ~ "Goldilocks bell-curve score of each item's end-year TRL, averaged over the items.",
        variable == "trl_momentum_index" ~ "Scaled positive TRL change from start-year to end-year, capped and transformed, averaged over the items with a start-year TRL.",
        variable == "trl_index" ~ paste0(
          "Weighted blend of the TRL Level Index (", 1 - momentum_weight,
          ") and the TRL Momentum Index (", momentum_weight, ")."
        ),
        TRUE ~ NA_character_
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
      explanation = paste(
        base_explanation,
        technological_readiness_basis_note(basis, momentum_basis, n_items, n_items_tech, variable_kind)
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
                                    post_trl9_scale = IEA_POST_TRL9_SCALE,
                                    verbose = FALSE,
                                    tech_map_rules = NULL,
                                    country_reference = NULL) {
  iea_cleantech <- technological_readiness_clean(iea_cleantech_all)

  # Both years follow the release unless pinned: the newest TRL column is the end year and
  # the oldest is the start of the momentum window.
  year_end <- technological_readiness_resolve_year_end(iea_cleantech, year_end)
  year_start <- technological_readiness_resolve_year_start(iea_cleantech, year_start)

  map_rules <- null_coalesce(tech_map_rules, read_iea_tech_map_rules())

  iea_with_tech <- technological_readiness_assign_tech(iea_cleantech, map_rules = map_rules)
  diagnostics <- technological_readiness_mapping_diagnostics(iea_with_tech, techs = techs)

  # The 2026 rules classify every item of the release they were written against; an item they
  # do not know is new in the Guide and needs a mapping decision.
  if (nrow(diagnostics$unclassified_items) > 0 && all(iea_cleantech$guide_layout == "public_dataset")) {
    warning(
      "technological_readiness(): ", nrow(diagnostics$unclassified_items),
      " IEA Clean Tech Guide item(s) match no rule in the tech map and are left out: ",
      paste(utils::head(diagnostics$unclassified_items$name, 10), collapse = "; "),
      call. = FALSE
    )
  }

  # An adoption stage the scale does not know would leave its items at TRL 9 unnoticed.
  post_values <- unlist(iea_cleantech[grep("^post_trl9_stage_\\d{4}$", names(iea_cleantech))], use.names = FALSE)
  post_values <- unique(stats::na.omit(post_values))
  unknown_stages <- post_values[is.na(technological_readiness_post_trl9_value(post_values, post_trl9_scale))]
  if (length(unknown_stages) > 0) {
    warning(
      "technological_readiness(): unknown post-TRL-9 adoption stage(s) in the IEA Clean Tech Guide, ",
      "read as the reported TRL: ", paste(unknown_stages, collapse = "; "),
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message(sprintf(
      "Technological Readiness mapping: %.1f%% of items feed no index technology (%.1f%% out of scope, %.1f%% unclassified)",
      diagnostics$pct_unmapped * 100, diagnostics$pct_out_of_scope * 100, diagnostics$pct_unclassified * 100
    ))
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
    gamma_momentum = gamma_momentum,
    post_trl9_scale = post_trl9_scale
  )
  diagnostics$tech_stage_table <- iea_tech

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

  out <- technological_readiness_broadcast(out, country_reference)

  attr(out, "mapping_diagnostics") <- diagnostics
  out
}

# Copy the technology-stage rows to every country in `country_reference` (a character vector
# or a table with a Country column). NULL keeps the single Country = "Global" row per key.
technological_readiness_broadcast <- function(tbl, country_reference = NULL) {
  if (is.null(country_reference)) {
    return(tbl)
  }

  countries <- if (is.data.frame(country_reference)) {
    require_columns(country_reference, "Country", label = "country_reference")
    country_reference$Country
  } else {
    country_reference
  }
  countries <- sort(unique(as.character(countries)))
  countries <- countries[!is.na(countries) & nzchar(countries) & countries != "Global"]
  if (length(countries) == 0) {
    stop("technological_readiness(): country_reference holds no country names.")
  }

  tidyr::crossing(Country = countries, dplyr::select(tbl, -Country)) %>%
    dplyr::mutate(
      explanation = stringr::str_squish(paste(
        dplyr::coalesce(explanation, ""),
        "Global value, identical for every country."
      ))
    ) %>%
    dplyr::select(dplyr::all_of(names(tbl)))
}
