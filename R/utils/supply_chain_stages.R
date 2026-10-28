# Six-stage supply chain (config/supply_chain_stages.yml, docs/supply_chain_stages.md).
#
# Since 2026-10-26 the `supply_chain` column of every Energy Security and Economic Opportunity
# theme table holds one of six stages instead of Upstream / Midstream / Downstream. The old three
# survive as roll-ups: Upstream = extraction, refining and recovery (materials, refining
# included); Midstream = component manufacturing and final assembly; Downstream = deployment.
# These helpers are pure; scripts read the YAML. The constants below must equal the YAML, and
# tests/testthat/test-supply-chain-stages.R checks that they do.

SUPPLY_CHAIN_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")

SUPPLY_CHAIN_STAGE_ROLLUP <- c(
  Extraction = "Upstream",
  Refining = "Upstream",
  Components = "Midstream",
  Assembly = "Midstream",
  Deployment = "Downstream",
  Recovery = "Upstream"
)

SUPPLY_CHAIN_LEGACY_STAGES <- c("Upstream", "Midstream", "Downstream")

# The stages that make or supply something (every stage but Deployment): the old Upstream and
# Midstream. Coalition supplier measures, the allied network and the AIB partner scores use them.
SUPPLY_CHAIN_SUPPLY_STAGES <- setdiff(SUPPLY_CHAIN_STAGES, "Deployment")

# Manufacturing (the old Midstream): where a source reports plants or capex without a product,
# its value is copied onto both.
SUPPLY_CHAIN_MANUFACTURING_STAGES <- c("Components", "Assembly")

# Components and Assembly carry the same sources (docs/supply_chain_stages.md, "The manufacturing
# pair"). A source with a figure for only one of a technology's two manufacturing stages (an IEA
# ETP capacity share, the IEA cost relative to China, a BNEF overcapacity ratio) is copied onto
# the other, as manufacturing capex and the ILO manufacturing wage are; a source with its own
# figure for each stage (BNEF solar wafers and cells, then modules) keeps both. Without the copy
# the bare stage escaped the source: Batteries - Assembly lacked the battery overcapacity, cost and
# capacity share filed under Components and topped 41 countries' Strategic Index (2026-10-27).
# `by` names the columns that identify one figure apart from its stage; the copies' explanation
# says where they came from (SUPPLY_CHAIN_PAIR_COPY_MARKER). Pure.
SUPPLY_CHAIN_PAIR_COPY_MARKER <- "; copied from "

supply_chain_fill_manufacturing_pair <- function(tbl, by, stage_col = "supply_chain") {
  missing <- setdiff(c(by, stage_col), names(tbl))
  if (length(missing) > 0) {
    stop("supply_chain_fill_manufacturing_pair(): missing column(s) ", paste(missing, collapse = ", "))
  }
  pair <- SUPPLY_CHAIN_MANUFACTURING_STAGES
  stage <- as.character(tbl[[stage_col]])
  in_pair <- stage %in% pair
  if (!any(in_pair)) return(tbl)
  key <- do.call(paste, c(lapply(tbl[in_pair, by, drop = FALSE], as.character), sep = "\r"))
  n_stages <- tapply(stage[in_pair], key, function(s) length(unique(s)))
  lone <- key %in% names(n_stages)[n_stages == 1]
  if (!any(lone)) return(tbl)
  # glue explanations (str_glue) would not bind to the copies' plain text
  if ("explanation" %in% names(tbl)) tbl$explanation <- as.character(tbl$explanation)
  add <- tbl[in_pair, , drop = FALSE][lone, , drop = FALSE]
  from <- as.character(add[[stage_col]])
  add[[stage_col]] <- ifelse(from == pair[1], pair[2], pair[1])
  if ("explanation" %in% names(add)) {
    add$explanation <- paste0(
      add$explanation, SUPPLY_CHAIN_PAIR_COPY_MARKER, from,
      ": the source has no separate figure for this manufacturing stage (do not sum across stages)"
    )
  }
  dplyr::bind_rows(tbl, add)
}

# The cells (tech, supply_chain), or with `by_category` the (tech, supply_chain, category)
# entries, whose every row across `tables` (a list of theme tables) is a manufacturing-pair copy.
# A copy says nothing about its stage, so the Strategic Index rules do not count such a cell as
# measured (strategic_unmeasured_sectors()) or such a category as stage evidence
# (strategic_unevidenced_sectors()). A table without the columns, or without explanations,
# contributes rows that count as the stage's own. Pure.
supply_chain_pair_copy_cells <- function(tables, by_category = FALSE) {
  keys <- c("tech", "supply_chain", if (by_category) "category")
  rows <- lapply(tables, function(t) {
    if (is.null(t) || !all(keys %in% names(t))) return(NULL)
    copy <- if ("explanation" %in% names(t)) grepl(SUPPLY_CHAIN_PAIR_COPY_MARKER, as.character(t$explanation), fixed = TRUE) else FALSE
    out <- as.data.frame(lapply(t[keys], as.character), stringsAsFactors = FALSE)
    out$copy <- rep_len(copy, nrow(out))
    unique(out)
  })
  rows <- dplyr::bind_rows(rows)
  if (!nrow(rows)) return(stats::setNames(data.frame(matrix(character(), 0, length(keys))), keys))
  rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(copy_only = all(.data$copy), .groups = "drop") %>%
    dplyr::filter(.data$copy_only) %>%
    dplyr::select(dplyr::all_of(keys)) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

# Strategic Index supply-chain term: each stage takes an equal share of its roll-up's weight
# (Upstream 0.50, Midstream 0.75, Downstream 0.25; user decision 2026-10-26).
SUPPLY_CHAIN_STAGE_SI_WEIGHTS <- c(
  Extraction = 0.50 / 3,
  Refining = 0.50 / 3,
  Components = 0.75 / 2,
  Assembly = 0.75 / 2,
  Deployment = 0.25,
  Recovery = 0.50 / 3
)

# TRUE for the six stage names.
is_supply_chain_stage <- function(x) {
  as.character(x) %in% SUPPLY_CHAIN_STAGES
}

# Stop unless every value is a stage name; `label` names the table in the error.
assert_supply_chain_stages <- function(x, label = "supply_chain") {
  bad <- unique(as.character(x)[!is_supply_chain_stage(x)])
  if (length(bad) > 0) {
    stop(
      label, " holds values that are not supply-chain stages: ",
      paste(utils::head(bad, 10), collapse = ", "),
      ". Expected one of ", paste(SUPPLY_CHAIN_STAGES, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

# Roll a stage up to Upstream / Midstream / Downstream (NA for anything else).
supply_chain_stage_rollup <- function(stage) {
  unname(SUPPLY_CHAIN_STAGE_ROLLUP[as.character(stage)])
}

# The stages an old Upstream / Midstream / Downstream label covers, in stage order.
supply_chain_stages_of <- function(legacy) {
  legacy <- as.character(legacy)
  if (length(legacy) != 1 || !legacy %in% SUPPLY_CHAIN_LEGACY_STAGES) {
    stop("supply_chain_stages_of() takes one of ", paste(SUPPLY_CHAIN_LEGACY_STAGES, collapse = ", "), ".")
  }
  SUPPLY_CHAIN_STAGES[SUPPLY_CHAIN_STAGE_ROLLUP[SUPPLY_CHAIN_STAGES] == legacy]
}

# Replicate rows labelled with an old stage onto each stage that label covers. For measures
# whose source only reports Upstream / Midstream / Downstream (one value cannot be split), so
# every covered stage carries the same value. Rows already labelled with a stage pass through.
supply_chain_expand_legacy <- function(tbl, col = "supply_chain") {
  if (nrow(tbl) == 0) {
    return(tbl)
  }
  values <- as.character(tbl[[col]])
  unknown <- unique(values[!values %in% c(SUPPLY_CHAIN_LEGACY_STAGES, SUPPLY_CHAIN_STAGES)])
  if (length(unknown) > 0) {
    stop("supply_chain_expand_legacy(): unknown ", col, " value(s) ", paste(unknown, collapse = ", "), ".")
  }

  map <- tibble::tibble(
    .legacy = c(unname(SUPPLY_CHAIN_STAGE_ROLLUP[SUPPLY_CHAIN_STAGES]), SUPPLY_CHAIN_STAGES),
    .stage = c(SUPPLY_CHAIN_STAGES, SUPPLY_CHAIN_STAGES)
  )
  tbl$.legacy <- values
  out <- dplyr::inner_join(tbl, map, by = ".legacy", relationship = "many-to-many")
  out[[col]] <- out$.stage
  out$.legacy <- NULL
  out$.stage <- NULL
  out
}

# Copy every row onto all six stages, for measures with no stage at all (a country-level
# price, a technology-wide demand projection). Any existing `col` is replaced.
supply_chain_expand_all <- function(tbl, col = "supply_chain", stages = SUPPLY_CHAIN_STAGES) {
  tbl[[col]] <- NULL
  stage_tbl <- tibble::tibble(.stage = stages)
  out <- tidyr::crossing(tbl, stage_tbl)
  out[[col]] <- out$.stage
  out$.stage <- NULL
  out
}

# Order a stage vector (or factor levels) along the chain.
supply_chain_stage_factor <- function(x) {
  factor(as.character(x), levels = SUPPLY_CHAIN_STAGES)
}
