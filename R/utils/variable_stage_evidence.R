# Evidence specificity (docs/strategic_position.md, section 1): how much of a cell's score rests on
# observations that identify its six-stage cell, as opposed to figures shared between stages, proxies,
# technology-wide values or fills. The metadata live in data/reference/index_variable_stage_evidence.csv
# (one row per pillar, category, variable, tech and stage; tech or stage may be "*"), each row citing
# the map_id of the documenting row in data/reference/index_variable_stage_map.csv. Scripts read the
# CSV; these helpers are pure. Evidence is reported next to a score and never changes it.
#
# Depends on R/utils/supply_chain_stages.R (SUPPLY_CHAIN_STAGES, supply_chain_stage_rollup()).

VSE_STATUSES <- c("exact", "split", "shared", "proxy", "tech_wide", "no_data")
VSE_COUNTRY_VARIATION <- c("country", "regional", "global", "pair", "single_country")
# SI documents the terms of the deprecated Strategic Index; CP, SN and PP are the new dimensions.
VSE_PILLARS <- c("EO", "ES", "Policy", "PSI", "SI", "CP", "SN", "PP")
VSE_COLUMNS <- c(
  "map_id", "pillar", "category", "variable", "tech", "stage", "legacy_stage",
  "evidence_status", "allocation_method", "country_variation", "source", "notes"
)
VSE_KEY <- c("pillar", "category", "variable", "tech", "stage")
VSE_ANY <- "*"

# The lookup precedence: a row naming both the technology and the stage beats one naming only the
# technology, which beats one naming only the stage, which beats the (*, *) default.
VSE_MATCH_LEVELS <- c("tech+stage", "tech+*", "*+stage", "*+*")

vse_stop_values <- function(fn, what, bad) {
  bad <- unique(as.character(bad))
  stop(
    fn, ": ", what, ": ", paste(utils::head(bad, 12), collapse = ", "),
    if (length(bad) > 12) paste0(" (and ", length(bad) - 12, " more)") else "", ".",
    call. = FALSE
  )
}

# Vocabulary and completeness checks of the evidence table; returns it with character columns.
# `map_ids` (optional): the map_id column of index_variable_stage_map.csv, so every cited id must
# exist there. `pillars`: the pillar vocabulary.
vse_read_check <- function(tbl, map_ids = NULL, pillars = VSE_PILLARS) {
  fn <- "vse_read_check()"
  missing <- setdiff(VSE_COLUMNS, names(tbl))
  if (length(missing) > 0) vse_stop_values(fn, "missing column(s)", missing)
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  for (col in VSE_COLUMNS) {
    tbl[[col]] <- trimws(as.character(tbl[[col]]))
  }
  # notes may be empty; every other field says something a reader needs.
  required <- setdiff(VSE_COLUMNS, "notes")
  for (col in required) {
    empty <- is.na(tbl[[col]]) | tbl[[col]] == ""
    if (any(empty)) vse_stop_values(fn, paste0("empty ", col, " in row(s)"), which(empty))
  }
  for (col in c("map_id", "pillar", "category", "variable")) {
    if (any(tbl[[col]] == VSE_ANY)) vse_stop_values(fn, paste0("'*' is allowed only in tech and stage, not in ", col, "; row(s)"), which(tbl[[col]] == VSE_ANY))
  }
  bad <- setdiff(tbl$pillar, pillars)
  if (length(bad)) vse_stop_values(fn, "unknown pillar(s)", bad)
  bad <- setdiff(tbl$evidence_status, VSE_STATUSES)
  if (length(bad)) vse_stop_values(fn, paste0("unknown evidence_status (expected ", paste(VSE_STATUSES, collapse = ", "), ")"), bad)
  bad <- setdiff(tbl$country_variation, VSE_COUNTRY_VARIATION)
  if (length(bad)) vse_stop_values(fn, paste0("unknown country_variation (expected ", paste(VSE_COUNTRY_VARIATION, collapse = ", "), ")"), bad)
  bad <- setdiff(tbl$stage, c(SUPPLY_CHAIN_STAGES, VSE_ANY))
  if (length(bad)) vse_stop_values(fn, "stage holds values that are neither a six-stage name nor '*'", bad)
  expected <- ifelse(tbl$stage == VSE_ANY, VSE_ANY, supply_chain_stage_rollup(tbl$stage))
  wrong <- which(tbl$legacy_stage != expected)
  if (length(wrong)) {
    vse_stop_values(fn, "legacy_stage is not supply_chain_stage_rollup(stage) ('*' for '*') in row(s)", wrong)
  }
  key <- do.call(paste, c(tbl[VSE_KEY], sep = " | "))
  dup <- duplicated(key)
  if (any(dup)) vse_stop_values(fn, "duplicate (pillar, category, variable, tech, stage) key(s)", key[dup])
  if (!is.null(map_ids)) {
    bad <- setdiff(tbl$map_id, as.character(map_ids))
    if (length(bad)) vse_stop_values(fn, "map_id(s) not in index_variable_stage_map.csv", bad)
  }
  tbl
}

# Attach evidence_status, country_variation and map_id (plus evidence_match, the precedence level
# that matched) to `rows`, which name a pillar, category, variable, tech and stage in the given
# columns. Precedence: exact tech + exact stage > exact tech + '*' > '*' + exact stage > '*' + '*'.
# Unmatched rows get NA. Existing columns of those names are replaced. Row order is kept. A stage
# value outside the six (a legacy Upstream / Midstream label) can only match a '*' stage row.
vse_lookup <- function(rows,
                       evidence,
                       pillar_col = "pillar",
                       category_col = "category",
                       variable_col = "variable",
                       tech_col = "tech",
                       stage_col = "supply_chain") {
  cols <- c(pillar_col, category_col, variable_col, tech_col, stage_col)
  missing <- setdiff(cols, names(rows))
  if (length(missing) > 0) vse_stop_values("vse_lookup()", "rows lack column(s)", missing)
  missing <- setdiff(c(VSE_KEY, "evidence_status", "country_variation", "map_id"), names(evidence))
  if (length(missing) > 0) vse_stop_values("vse_lookup()", "evidence lacks column(s)", missing)

  chr <- function(x) as.character(x)
  sep <- "\r"
  r_base <- paste(chr(rows[[pillar_col]]), chr(rows[[category_col]]), chr(rows[[variable_col]]), sep = sep)
  r_tech <- chr(rows[[tech_col]])
  r_stage <- chr(rows[[stage_col]])
  e_base <- paste(chr(evidence$pillar), chr(evidence$category), chr(evidence$variable), sep = sep)
  e_tech <- chr(evidence$tech)
  e_stage <- chr(evidence$stage)
  any_tech <- e_tech == VSE_ANY
  any_stage <- e_stage == VSE_ANY

  # One candidate key per precedence level; evidence keys are unique, so match() finds at most one.
  r_keys <- list(
    paste(r_base, r_tech, r_stage, sep = sep),
    paste(r_base, r_tech, sep = sep),
    paste(r_base, r_stage, sep = sep),
    r_base
  )
  e_keys <- list(
    ifelse(!any_tech & !any_stage, paste(e_base, e_tech, e_stage, sep = sep), NA_character_),
    ifelse(!any_tech & any_stage, paste(e_base, e_tech, sep = sep), NA_character_),
    ifelse(any_tech & !any_stage, paste(e_base, e_stage, sep = sep), NA_character_),
    ifelse(any_tech & any_stage, e_base, NA_character_)
  )
  hit <- rep(NA_integer_, nrow(rows))
  level <- rep(NA_character_, nrow(rows))
  for (i in seq_along(VSE_MATCH_LEVELS)) {
    todo <- is.na(hit)
    if (!any(todo)) break
    m <- match(r_keys[[i]][todo], e_keys[[i]], incomparables = NA)
    found <- !is.na(m)
    idx <- which(todo)[found]
    hit[idx] <- m[found]
    level[idx] <- VSE_MATCH_LEVELS[i]
  }
  rows$evidence_status <- chr(evidence$evidence_status)[hit]
  rows$country_variation <- chr(evidence$country_variation)[hit]
  rows$map_id <- chr(evidence$map_id)[hit]
  rows$evidence_match <- level
  rows
}

vse_check_bands <- function(bands) {
  bands <- unlist(bands)
  if (!all(c("high", "medium") %in% names(bands))) {
    stop("vse_cell_specificity(): bands needs named values high and medium.", call. = FALSE)
  }
  high <- as.numeric(bands[["high"]])
  medium <- as.numeric(bands[["medium"]])
  if (!is.finite(high) || !is.finite(medium) || medium <= 0 || high < medium || high > 1) {
    stop("vse_cell_specificity(): bands must satisfy 0 < medium <= high <= 1.", call. = FALSE)
  }
  c(high = high, medium = medium)
}

# stage_evidence_quality from evidence_specificity: high (>= high), medium (>= medium), low (> 0),
# none (0); NA when the cell has no weight. The tolerance keeps a share computed as 0.66999...
# from falling a band below 0.67.
vse_quality_band <- function(specificity, bands) {
  tol <- 1e-9
  dplyr::case_when(
    is.na(specificity) ~ NA_character_,
    specificity + tol >= bands[["high"]] ~ "high",
    specificity + tol >= bands[["medium"]] ~ "medium",
    specificity > tol ~ "low",
    TRUE ~ "none"
  )
}

# The cell measure of section 1. `inputs`: one row per input entering a cell's score, with the `by`
# columns, `weight` (the input's share of the cell's score) and `observed` (TRUE when the value is
# from the source, FALSE when filled). The evidence status comes from an `evidence_status` column
# when inputs carry one (a module that sets status per record), and otherwise, or where that column
# is NA, from `evidence` through vse_lookup() on pillar / category / variable / tech / supply_chain.
# Rows with weight NA or 0 do not enter a score and are left out; an NA `observed` counts as not
# observed. Stops, listing them, when inputs have no status and no evidence row.
# Per group:
#   evidence_specificity      sum(weight * observed * status in `specific`) / sum(weight)
#   <status>_evidence_share   weight share of exact, split, shared, proxy and tech_wide over observed weight
#   no_data_share             weight share of no_data rows over all weight
#   observed_share            weight share of observed rows over all weight
#   global_share              weight share of observed rows whose country_variation is global (a world
#                             constant that cannot rank countries), over observed weight; NA when the
#                             inputs carry no country_variation and no lookup supplies one
#   n_inputs                  rows entering the score
#   stage_evidence_quality    high / medium / low / none on evidence_specificity (`bands`)
# `count_global = FALSE` leaves rows whose country_variation is global out of the specificity
# numerator: a world constant filed at the right stage (trade HHI, TRL, price volatility) is
# stage-specific but cannot tell countries apart. The default follows the contract (status only);
# Competitive Position and Strategic Need carry no global variable, so it matters only for the legacy
# pillars.
vse_cell_specificity <- function(inputs,
                                 evidence = NULL,
                                 specific = c("exact", "split"),
                                 bands = list(high = 0.67, medium = 0.33),
                                 by = c("Country", "tech", "supply_chain"),
                                 count_global = TRUE) {
  fn <- "vse_cell_specificity()"
  missing <- setdiff(c(by, "weight", "observed"), names(inputs))
  if (length(missing) > 0) vse_stop_values(fn, "inputs lack column(s)", missing)
  bad <- setdiff(specific, VSE_STATUSES)
  if (length(bad)) vse_stop_values(fn, "unknown status(es) in `specific`", bad)
  bands <- vse_check_bands(bands)

  inputs <- as.data.frame(inputs, stringsAsFactors = FALSE)
  if (!"evidence_status" %in% names(inputs)) inputs$evidence_status <- rep(NA_character_, nrow(inputs))
  inputs$evidence_status <- as.character(inputs$evidence_status)
  has_cv <- "country_variation" %in% names(inputs)
  if (!has_cv) inputs$country_variation <- rep(NA_character_, nrow(inputs))

  todo <- is.na(inputs$evidence_status)
  if (any(todo)) {
    lookup_cols <- c("pillar", "category", "variable", "tech", "supply_chain")
    if (is.null(evidence) || !all(lookup_cols %in% names(inputs))) {
      stop(
        fn, ": ", sum(todo), " input row(s) have no evidence_status; give inputs an evidence_status ",
        "column or pass `evidence` with inputs carrying ", paste(lookup_cols, collapse = ", "), ".",
        call. = FALSE
      )
    }
    found <- vse_lookup(inputs[todo, lookup_cols, drop = FALSE], evidence)
    inputs$evidence_status[todo] <- found$evidence_status
    fill_cv <- todo & is.na(inputs$country_variation)
    inputs$country_variation[fill_cv] <- found$country_variation[fill_cv[todo]]
    has_cv <- TRUE
    unmatched <- is.na(inputs$evidence_status)
    if (any(unmatched)) {
      miss <- unique(inputs[unmatched, lookup_cols, drop = FALSE])
      labels <- unique(paste0(miss$pillar, " / ", miss$category, " / ", miss$variable,
                              " (e.g. ", miss$tech, " - ", miss$supply_chain, ")")[!duplicated(miss[c("pillar", "category", "variable")])])
      vse_stop_values(fn, paste0("no evidence row for ", sum(unmatched), " input row(s); unmatched variables"), labels)
    }
  }
  bad <- setdiff(inputs$evidence_status, VSE_STATUSES)
  if (length(bad)) vse_stop_values(fn, "unknown evidence_status in inputs", bad)

  weight <- suppressWarnings(as.numeric(inputs$weight))
  if (any(weight < 0, na.rm = TRUE)) stop(fn, ": negative weight(s) in inputs.", call. = FALSE)
  inputs$weight <- weight
  inputs <- inputs[!is.na(weight) & weight > 0, , drop = FALSE]
  inputs$observed <- !is.na(inputs$observed) & as.logical(inputs$observed)
  if (!isTRUE(count_global) && !has_cv) {
    stop(fn, ": count_global = FALSE needs country_variation (from inputs or the evidence lookup).", call. = FALSE)
  }
  inputs$.specific <- inputs$evidence_status %in% specific &
    (isTRUE(count_global) | !(inputs$country_variation %in% "global"))

  share_of <- function(num, den) ifelse(den > 0, num / den, NA_real_)
  out <- inputs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      .w = sum(.data$weight),
      .w_obs = sum(.data$weight * .data$observed),
      .w_spec = sum(.data$weight * .data$observed * .data$.specific),
      .w_exact = sum(.data$weight * .data$observed * (.data$evidence_status == "exact")),
      .w_split = sum(.data$weight * .data$observed * (.data$evidence_status == "split")),
      .w_shared = sum(.data$weight * .data$observed * (.data$evidence_status == "shared")),
      .w_proxy = sum(.data$weight * .data$observed * (.data$evidence_status == "proxy")),
      .w_tech_wide = sum(.data$weight * .data$observed * (.data$evidence_status == "tech_wide")),
      .w_no_data = sum(.data$weight * (.data$evidence_status == "no_data")),
      .w_global = if (has_cv) sum(.data$weight * .data$observed * (.data$country_variation %in% "global")) else NA_real_,
      n_inputs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      evidence_specificity = share_of(.data$.w_spec, .data$.w),
      exact_evidence_share = share_of(.data$.w_exact, .data$.w_obs),
      split_evidence_share = share_of(.data$.w_split, .data$.w_obs),
      shared_evidence_share = share_of(.data$.w_shared, .data$.w_obs),
      proxy_evidence_share = share_of(.data$.w_proxy, .data$.w_obs),
      tech_wide_evidence_share = share_of(.data$.w_tech_wide, .data$.w_obs),
      no_data_share = share_of(.data$.w_no_data, .data$.w),
      observed_share = share_of(.data$.w_obs, .data$.w),
      global_share = share_of(.data$.w_global, .data$.w_obs),
      stage_evidence_quality = vse_quality_band(.data$evidence_specificity, bands)
    )
  out[, c(
    by, "evidence_specificity", "exact_evidence_share", "split_evidence_share", "shared_evidence_share",
    "proxy_evidence_share", "tech_wide_evidence_share", "no_data_share", "observed_share", "global_share",
    "n_inputs", "stage_evidence_quality"
  )]
}

# Coverage of a vse_cell_specificity() table (or one whose columns carry a common suffix, such as the
# _cp columns of strategic_position_tbl: pass `suffix = "_cp"`) by `by` (for example "supply_chain",
# "tech", "Country", or character(0) for one overall row): the number of cells, the mean of each
# share over cells (cells with an NA share are left out of that mean) and the number of cells in each
# stage_evidence_quality band.
vse_coverage_summary <- function(cell_tbl, by = character(0), suffix = "") {
  shares <- c(
    "evidence_specificity", "exact_evidence_share", "split_evidence_share", "shared_evidence_share",
    "proxy_evidence_share", "tech_wide_evidence_share", "no_data_share", "observed_share", "global_share"
  )
  shares <- paste0(shares, suffix)
  quality <- paste0("stage_evidence_quality", suffix)
  missing <- setdiff(c(by, shares[1], quality), names(cell_tbl))
  if (length(missing) > 0) vse_stop_values("vse_coverage_summary()", "cell_tbl lacks column(s)", missing)
  shares <- intersect(shares, names(cell_tbl))
  mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  q <- as.character(cell_tbl[[quality]])
  cell_tbl$.q_high <- q %in% "high"
  cell_tbl$.q_medium <- q %in% "medium"
  cell_tbl$.q_low <- q %in% "low"
  cell_tbl$.q_none <- q %in% "none"
  cell_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      n_cells = dplyr::n(),
      dplyr::across(dplyr::all_of(shares), mean_or_na, .names = "mean_{.col}"),
      n_high = sum(.data$.q_high),
      n_medium = sum(.data$.q_medium),
      n_low = sum(.data$.q_low),
      n_none = sum(.data$.q_none),
      .groups = "drop"
    )
}
