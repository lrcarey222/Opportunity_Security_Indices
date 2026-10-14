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

HS6_MASTER_COLUMNS <- c("tech", "supply_chain", "sub_sector", "hs6", "essential")

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

# Normalise the master into the canonical long form every view is built from.
hs6_normalize_master <- function(master) {
  hs6_validate_master(master)

  out <- data.frame(
    tech = trimws(as.character(master$tech)),
    supply_chain = trimws(as.character(master$supply_chain)),
    sub_sector = trimws(as.character(master$sub_sector)),
    hs6 = hs6_clean_code(master$hs6),
    essential = as.logical(master$essential),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$hs6) & nzchar(out$tech), , drop = FALSE]
  out$essential[is.na(out$essential)] <- FALSE

  # Sub-sector text in the master contains embedded newlines from its source export;
  # collapse them so downstream CSVs stay one row per record.
  out$sub_sector <- gsub("[\r\n]+", " ", out$sub_sector)
  out$sub_sector <- gsub("\\s{2,}", " ", trimws(out$sub_sector))

  out <- unique(out)
  out[order(out$tech, out$supply_chain, out$sub_sector, out$hs6), , drop = FALSE]
}

## Views ---------------------------------------------------------------------

# Legacy name: hts_codes_categories_bolstered_final.csv
# Read by scripts/05_ingest_sources.R (Comtrade code list) and 10_build_themes.R,
# which renames Technology -> tech and "Value Chain" -> supply_chain.
hs6_view_bolstered <- function(master_long) {
  out <- data.frame(
    Technology = master_long$tech,
    `Value Chain` = master_long$supply_chain,
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
