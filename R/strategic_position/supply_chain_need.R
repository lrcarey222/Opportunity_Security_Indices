# NEED at the producing stages: supply_chain_need (docs/strategic_position.md, "Need"). How costly or
# dangerous is dependence on external supply for this node? It keeps the cleaned Strategic Need families
# (strategic_need.R: net import dependence, the country's own import concentration (HHI), China and
# opposite-bloc exposure) and adds a fourth, trusted alternatives (the world-supply share of trusted
# suppliers other than the country itself, the Secure engine's Test 5 input), when it is observed for
# at least trusted_alternatives_min_coverage of the assessed producing cells; otherwise trusted
# alternatives is reported as a drawer subcomponent only. Substitutability, reconstitution time,
# inventories, essentiality and operational control stay Secure-engine inputs.
#
# Need is stage-specific (spec V-VI): supply_chain_need exists at Extraction, Refining, Components,
# Assembly and Recovery only. Deployment & Use has deployment_need instead (scripts/95,
# deployment_need.R); the Assembly-equipment proxy that strategic_need.R puts on the Deployment row is
# not a Need there and is never read by the decision. No combined need score exists.
#
# Pure functions: no IO. Needs sp_helpers.R and R/utils/supply_chain_stages.R.

SP_SCN_FAMILIES <- c("sn_import_dependence", "sn_supplier_concentration", "sn_risky_supplier_exposure")
SP_NEED_TYPES <- c("supply_chain", "deployment")

# The need type of a stage: deployment at Deployment & Use, supply_chain elsewhere.
sp_need_type <- function(stage) ifelse(stage == "Deployment", "deployment", "supply_chain")

# tbl: the cell table after strategic_need.R and the posture engine (supply_chain, the three sn_ family
# scores, strategic_need_percentile, sn_import_material, trusted_world_share, home_share). cfg:
# decision_framework (supply_chain_need$trusted_alternatives_min_coverage, self_supply_share).
# Adds trusted_alternatives_share (trusted partners' share of world supply), sn_trusted_alternatives (the
# family score: within-cell mid-rank percentile of 1 - that share; 0 need when the country is
# self-supplied or its imports are immaterial), sn_trusted_in_score, supply_chain_need_score,
# supply_chain_need (0-1, within-cell percentile; NA at Deployment and wherever strategic_need.R found
# the cell thin or excluded), supply_chain_need_n_families and need_type.
sp_supply_chain_need <- function(tbl, cfg = list()) {
  min_cov <- (cfg$supply_chain_need %||% list())$trusted_alternatives_min_coverage %||% 0.8
  self_line <- cfg$self_supply_share %||% 0.10
  col <- function(n, default = NA) if (n %in% names(tbl)) tbl[[n]] else rep(default, nrow(tbl))
  prod <- tbl$supply_chain != "Deployment"
  assessed <- prod & !is.na(col("strategic_need_percentile", NA_real_))
  trusted <- col("trusted_world_share", NA_real_)
  home <- dplyr::coalesce(col("home_share", NA_real_), 0)
  tbl$trusted_alternatives_share <- ifelse(prod & !is.na(trusted), pmax(trusted - home, 0), NA_real_)
  m <- 1 - pmin(tbl$trusted_alternatives_share, 1)
  self <- !is.na(col("home_share", NA_real_)) & col("home_share", NA_real_) >= self_line
  immaterial <- col("sn_import_material", NA) %in% FALSE
  m[!assessed] <- NA_real_
  cell <- paste(tbl$tech, tbl$supply_chain, sep = "|")
  # ranked over every observed country (as strategic_need.R ranks its measures), then need 0 where the
  # country supplies itself or its imports are immaterial
  pct <- stats::ave(m, cell, FUN = sp_midrank_pct)
  pct[!is.na(pct) & (self | immaterial)] <- 0
  tbl$sn_trusted_alternatives <- pct
  coverage <- if (any(assessed)) mean(!is.na(tbl$sn_trusted_alternatives[assessed])) else 0
  use_trusted <- coverage >= min_cov
  fams <- c(SP_SCN_FAMILIES, if (use_trusted) "sn_trusted_alternatives")
  fm <- sapply(fams, function(f) col(f, NA_real_))
  if (is.null(dim(fm))) fm <- matrix(fm, nrow = nrow(tbl))
  colnames(fm) <- fams
  # Self-supply guard (the Secure engine's self_supply_share line): a country holding that share of world
  # supply is not dependent on the composition of its imports, so the import-composition families
  # (concentration, risky suppliers, trusted alternatives) carry no need there; import dependence stays.
  comp <- intersect(c("sn_supplier_concentration", "sn_risky_supplier_exposure", "sn_trusted_alternatives"), fams)
  for (f in comp) fm[self & !is.na(fm[, f]), f] <- 0
  tbl$sn_self_supplied <- ifelse(prod, self, NA)
  score <- sp_row_mean_available(fm)
  score[!assessed] <- NA_real_
  tbl$supply_chain_need_score <- score
  tbl$supply_chain_need <- stats::ave(score, cell, FUN = sp_midrank_pct)
  tbl$supply_chain_need_n_families <- ifelse(assessed, rowSums(!is.na(fm)), NA_integer_)
  tbl$sn_trusted_in_score <- use_trusted
  attr(tbl, "sn_trusted_alternatives_coverage") <- coverage
  tbl$need_type <- sp_need_type(tbl$supply_chain)
  tbl
}
