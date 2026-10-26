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
