# HS6 crosswalk: one master table, several named views.
#
# data/reference/energy_hs6_master.csv is the single source of truth for the
# HS6 -> technology / supply-chain / sub-sector mapping. Consumers historically read
# three differently-shaped files, which had drifted apart: on the codes they shared,
# only 60-81% agreed on the technology assignment, and ES/EO was scored on a different
# basket of codes than PSI.
#
# Rather than rewrite every consumer, the legacy file names are regenerated from the
# master as views. These functions are pure; scripts/04_build_hs6_views.R does the IO.
#
# Conventions the master follows (its `note` column records the reason on each row that
# departs from the original export):
#
#   * Stage follows the product, not the technology's shopping list. Ores, concentrates,
#     metal-bearing slag and waste/scrap are raw materials, so they sit in Upstream even
#     when the technology that consumes them is scored at Midstream. Filed under Batteries
#     Midstream they made Chile, Peru and Zimbabwe the leading battery manufacturers.
#   * `legacy_hs` rows carry an older HS code whose successors are all in the basket
#     (e.g. 8541.40 for the HS2022 PV codes 8541.42/43, 2710.00 for the refined-oil codes).
#     The Atlas panel is in HS92 and some Comtrade reporters still file in older
#     revisions, so without them those sources silently lose the headline product. A code
#     whose older scope was wider than the successors (8541.40 also holds LEDs) is an
#     upper bound, and says so in `note`. Residual "n.e.s." headings are not bridged:
#     HS92 3823.90 (chemical products n.e.s.) would be a fifth of the Atlas battery
#     midstream basket and make Eswatini a leading battery-materials exporter.
#   * One row per (tech, supply_chain, sub_sector, hs6). The source export carried some
#     keys twice with conflicting essential flags; a code is essential if any row flagged
#     it, because the flag is a positive designation and an unflagged row only means the
#     source file that contributed it did not assess essentiality.
#   * A code belongs in a technology's basket only if that technology is a dominant use of
#     the product. Rows that fail the test stay in the master with `excluded = TRUE` and
#     the reason in `note`, so the decision is visible and reversible; they are dropped
#     from every view and from the copy of the master staged in data/raw
#     (hs6_master_in_basket()). The October 2026 review excluded, among others, the
#     residual chemicals heading 3824.99 and polyethylene/polypropylene resins from
#     Batteries Midstream, aircraft engines from Coal and Gas Midstream, general
#     machinery n.e.s. from Oil and Gas Midstream, copper ore from Batteries Upstream and
#     electrical energy (2716.00) from Electric Grid Upstream. Codes kept although the
#     technology is not their only use say so in `note` ("kept with caveat"). Electric Grid
#     Upstream now holds the grid's copper and aluminium inputs (ore, blister, cathode;
#     bauxite, alumina, unwrought aluminium), kept with that caveat; the Batteries rows of
#     copper ore, bauxite and alumina stay excluded, so each code is in one basket.
#   * No code sits in two stages of one technology. The Upstream baskets of Electric
#     Vehicles, Solar, Wind, Nuclear and Semiconductors (user decision 2026-10-11) therefore
#     moved the first traded form of a raw material out of Midstream rather than copying it:
#     rare-earth compounds and metals (EV and Wind; the HS has no rare-earth ore code apart
#     from monazite, 2612.20), natural uranium compounds (2844.10, how most mines ship
#     yellowcake) and unwrought gallium, germanium and indium (8112.92, bridged to HS92
#     8112.91, the same named minor metals). EV Upstream shares Batteries Upstream's ores
#     and concentrates (a code in two technologies counts fully in each); Solar Upstream is
#     silicon metal (2804.69).

#   * `stage` is the six-stage supply chain (R/utils/supply_chain_stages.R): what the code
#     physically is, relative to the technology (a lithium-ion pack is Assembly for Batteries
#     and Components for Electric Vehicles). Every in-basket row has one; excluded rows leave it
#     blank. `supply_chain` keeps the three-stage label for the consumers not yet moved to six
#     stages (the partner themes, NIPO, the allied network, the dashboard exports); it is NOT
#     the roll-up of `stage` for the 55 refining rows filed Midstream before 2026-10-26.
#     `enabling_equipment` flags capital goods that make or move a stage's output (drill rigs,
#     fab tools); they stay in Assembly, with the stage they serve in `serves_stage`.

HS6_MASTER_COLUMNS <- c("tech", "supply_chain", "sub_sector", "hs6", "essential")

# Optional provenance columns; a master without them reads as all-current, in-basket codes.
HS6_MASTER_OPTIONAL_COLUMNS <- c("legacy_hs", "excluded", "note", "stage", "enabling_equipment", "serves_stage")

# Zero-pad to six characters, dropping anything that is not a usable code.
hs6_clean_code <- function(x) {
  digits <- gsub("\\D", "", as.character(x))
  ifelse(nzchar(digits), formatC(as.numeric(digits), width = 6, format = "d", flag = "0"), NA_character_)
}

hs6_validate_master <- function(master) {
  missing <- setdiff(HS6_MASTER_COLUMNS, names(master))
  if (length(missing) > 0) {
    stop("HS6 master is missing column(s): ", paste(missing, collapse = ", "))
  }
  if (nrow(master) == 0) {
    stop("HS6 master is empty.")
  }
  invisible(TRUE)
}

# Rows of the master that are in a basket, with the master's own columns minus
# `excluded`. scripts/04 stages this as data/raw/energy_hs6_master.csv, which scripts/15
# and the trade charts read directly rather than through a view.
hs6_master_in_basket <- function(master) {
  hs6_validate_master(master)
  if (!"excluded" %in% names(master)) return(master)

  excluded <- as.logical(master$excluded)
  excluded[is.na(excluded)] <- FALSE
  out <- master[!excluded, setdiff(names(master), "excluded"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Normalise the master into the canonical long form every view is built from.
# Excluded rows are dropped first, so no view or match report ever sees them.
hs6_normalize_master <- function(master) {
  master <- hs6_master_in_basket(master)

  legacy <- if ("legacy_hs" %in% names(master)) as.logical(master$legacy_hs) else FALSE
  stage <- if ("stage" %in% names(master)) trimws(as.character(master$stage)) else NA_character_

  out <- data.frame(
    tech = trimws(as.character(master$tech)),
    supply_chain = trimws(as.character(master$supply_chain)),
    stage = stage,
    sub_sector = trimws(as.character(master$sub_sector)),
    hs6 = hs6_clean_code(master$hs6),
    essential = as.logical(master$essential),
    legacy_hs = legacy,
    stringsAsFactors = FALSE
  )
  out$stage[!is.na(out$stage) & !nzchar(out$stage)] <- NA_character_

  out <- out[!is.na(out$hs6) & nzchar(out$tech), , drop = FALSE]
  out$essential[is.na(out$essential)] <- FALSE
  out$legacy_hs[is.na(out$legacy_hs)] <- FALSE

  # Sub-sector text in the master contains embedded newlines from its source export;
  # collapse them so downstream CSVs stay one row per record.
  out$sub_sector <- gsub("[\r\n]+", " ", out$sub_sector)
  out$sub_sector <- gsub("\\s{2,}", " ", trimws(out$sub_sector))

  # One row per key, essential if any row flagged it (see the header). unique() alone
  # kept both halves of a conflicting pair, so NIPO counted those codes twice.
  key <- paste(out$tech, out$supply_chain, out$sub_sector, out$hs6, sep = "\r")
  keep <- !duplicated(key)
  out$essential <- as.logical(stats::ave(out$essential, key, FUN = any))
  out$legacy_hs <- as.logical(stats::ave(out$legacy_hs, key, FUN = any))
  out <- out[keep, , drop = FALSE]

  out <- out[order(out$tech, out$supply_chain, out$sub_sector, out$hs6), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Which master codes a trade source can see. `available_codes` is the product list of the
# source (e.g. the HS92 codes in the Atlas panel); unmatched codes drop out of every
# variable built from it, so the count is reported rather than absorbed.
hs6_match_report <- function(master_long, available_codes) {
  codes <- unique(master_long$hs6)
  available <- hs6_clean_code(available_codes)
  unmatched <- sort(setdiff(codes, available))

  cells <- unique(master_long[master_long$hs6 %in% unmatched, c("hs6", "tech", "supply_chain")])
  cells$cell <- paste(cells$tech, cells$supply_chain, sep = " ")
  by_code <- if (nrow(cells) == 0) {
    data.frame(hs6 = character(), cells = character(), stringsAsFactors = FALSE)
  } else {
    stats::aggregate(cell ~ hs6, data = cells, FUN = function(x) paste(sort(unique(x)), collapse = "; "))
  }
  names(by_code) <- c("hs6", "cells")

  list(
    n_codes = length(codes),
    n_unmatched = length(unmatched),
    n_legacy = length(unique(master_long$hs6[master_long$legacy_hs])),
    unmatched = by_code
  )
}

## Views ---------------------------------------------------------------------

# Legacy name: hts_codes_categories_bolstered_final.csv
# Read by scripts/05_ingest_sources.R (Comtrade code list) and 10_build_themes.R. The trade
# themes score `Stage` (six stages); "Value Chain" is the three-stage label other readers use.
hs6_view_bolstered <- function(master_long) {
  out <- data.frame(
    Technology = master_long$tech,
    `Value Chain` = master_long$supply_chain,
    Stage = master_long$stage,
    Sub.Sector = master_long$sub_sector,
    HS6 = master_long$hs6,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  unique(out)
}

# Legacy name: consolidated_hs6_energy_tech_long.csv
# Read by R/charts/35_trade_bloc_counterfactual.R.
hs6_view_consolidated <- function(master_long) {
  out <- data.frame(
    tech = master_long$tech,
    supply_chain = master_long$supply_chain,
    stage = master_long$stage,
    sub_sector = master_long$sub_sector,
    HS6 = master_long$hs6,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  unique(out)
}

# Legacy name: hs6_categories_with_essential.csv
# The staged copy was a corrupt Excel pivot fragment with neither an HS6 nor an
# essential column. 10_build_themes.R renames "Value Chain" -> Value.Chain, and
# prepare_subcat_mapping() in R/categories/policy/nipo_policy_index.R needs an HS6
# column plus an essential column before it will apply the essential-goods override.
hs6_view_essential <- function(master_long) {
  out <- data.frame(
    Technology = master_long$tech,
    `Value Chain` = master_long$supply_chain,
    Sub.Sector = master_long$sub_sector,
    HS6 = master_long$hs6,
    essential = master_long$essential,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  unique(out)
}

# name -> builder. Keys are the file names written into data/raw.
hs6_view_builders <- function() {
  list(
    `hts_codes_categories_bolstered_final.csv` = hs6_view_bolstered,
    `consolidated_hs6_energy_tech_long.csv` = hs6_view_consolidated,
    `hs6_categories_with_essential.csv` = hs6_view_essential
  )
}

hs6_build_views <- function(master) {
  master_long <- hs6_normalize_master(master)
  builders <- hs6_view_builders()
  stats::setNames(lapply(builders, function(f) f(master_long)), names(builders))
}
