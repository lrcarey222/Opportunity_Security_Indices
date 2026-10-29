# Policy of the Sector Insights bundle (spec_sector.md 4.3 S10, 8.6): IEA PAMS records as bit masks
# and the Climate Action Tracker ratings. Pure.
#
# policy.pams has one row per policy x jurisdiction (the unit build_policy_table() works on, before
# expand_cross_cutting()): tech_mask bit i = policy.tags[i] (the 11 technologies, then Critical
# Minerals), stage_mask bit i = the six stages in chain order (Extraction, Refining, Components,
# Assembly, Deployment, Recovery; build_policy_table(stages = "six"), schema 2); cross_cutting = no
# technology tag (tech_mask 0); stage_mask 0 = no stage tag (any stage). tech_src: "iea" when the tag
# comes from the IEA's own technology list, "text" when from keyword rules on the text, NA when none.

SECTOR_POLICY_TAGS <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal",
                        "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind", "Critical Minerals")
SECTOR_POLICY_STAGES <- SUPPLY_CHAIN_STAGES

# policy_tbl: build_policy_table() output (one row per policy x country x tech x supply_chain);
# columns policy_id, iso3, year, status, jurisdiction, policy_type_bucket, tech, tech_source,
# supply_chain, title_text, learnMore. eu_from / eu_to: the EU code mapping. Returns list(pams,
# text, tags) with ids 0..n-1 in (policy_id, iso3) order; pams$record (policy_id) is not shipped.
sector_policy_compact <- function(policy_tbl, eu_from = "EUR", eu_to = "EUU") {
  p <- policy_tbl
  p$iso3 <- ifelse(p$iso3 == eu_from, eu_to, p$iso3)
  key <- paste(p$policy_id, p$iso3, sep = "\r")
  bit_t <- match(p$tech, SECTOR_POLICY_TAGS) - 1L
  # A former three-stage tag means the table was built with build_policy_table(stages = "legacy").
  if (any(p$supply_chain %in% SUPPLY_CHAIN_LEGACY_STAGES)) stop("sector_policy_compact(): three-stage policy tags; build the table with build_policy_table(..., stages = \"six\")")
  bit_s <- match(p$supply_chain, SECTOR_POLICY_STAGES) - 1L
  tmask <- tapply(ifelse(is.na(bit_t), 0L, bitwShiftL(1L, pmax(bit_t, 0L))), key, function(v) Reduce(bitwOr, v, 0L))
  smask <- tapply(ifelse(is.na(bit_s), 0L, bitwShiftL(1L, pmax(bit_s, 0L))), key, function(v) Reduce(bitwOr, v, 0L))
  src <- tapply(p$tech_source, key, function(v) {
    v <- unique(v[!is.na(v) & v != "none"])
    if ("list" %in% v) "iea" else if ("text" %in% v) "text" else NA_character_
  })
  first <- !duplicated(key)
  base <- p[first, , drop = FALSE]
  bkey <- key[first]
  ord <- order(base$policy_id, base$iso3)
  base <- base[ord, , drop = FALSE]
  bkey <- bkey[ord]
  n <- nrow(base)
  tm <- as.integer(tmask[bkey])
  pams <- tibble::tibble(
    id = seq_len(n) - 1L, geo = base$iso3, year = as.integer(base$year), status = as.character(base$status),
    jurisdiction = as.character(base$jurisdiction), type = as.character(base$policy_type_bucket),
    tech_mask = tm, tech_src = as.character(src[bkey]), stage_mask = as.integer(smask[bkey]),
    cross_cutting = tm == 0L,
    # The PAMS record before the country expansion (not shipped: the headline counts records once).
    record = as.character(base$policy_id)
  )
  text <- tibble::tibble(id = pams$id, title = as.character(base$title_text),
                         url = ifelse(is.na(base$learnMore) | !nzchar(base$learnMore), NA_character_, as.character(base$learnMore)))
  list(pams = pams, text = text, tags = SECTOR_POLICY_TAGS)
}

# policy.records counts per (geo, tag): the number of records whose tech_mask has the tag's bit
# (cross-cutting records excluded unless include_cross_cutting). Used by the headline and tests.
sector_policy_counts <- function(pams, tags = SECTOR_POLICY_TAGS, years = NULL, include_cross_cutting = FALSE) {
  p <- pams
  if (!is.null(years)) p <- p[!is.na(p$year) & p$year %in% years, , drop = FALSE]
  out <- lapply(seq_along(tags), function(i) {
    hit <- bitwAnd(p$tech_mask, bitwShiftL(1L, i - 1L)) != 0L
    if (include_cross_cutting) hit <- hit | p$cross_cutting
    if (!any(hit)) return(NULL)
    t <- table(p$geo[hit])
    tibble::tibble(geo = names(t), tag = tags[i], n = as.integer(t))
  })
  dplyr::bind_rows(out)
}

# The Climate Action Tracker table (country, overall rating, five sub-ratings, update date) keyed by
# ISO3. iso3: the ISO3 of each row (the EU -> EUU), computed by the caller.
sector_cat <- function(cat_raw, iso3) {
  nm <- names(cat_raw)
  col <- function(pattern) {
    j <- grep(pattern, nm, ignore.case = TRUE)
    if (!length(j)) stop("sector_cat(): no column matching ", pattern)
    v <- trimws(as.character(cat_raw[[j[1]]]))
    ifelse(nzchar(v), v, NA_character_)
  }
  out <- tibble::tibble(
    geo = iso3, overall = col("^overall"), policies_action = col("^policies"), domestic_target = col("^domestic"),
    fair_share = col("^fair"), climate_finance = col("^climate"), ndc = col("^net.?zero"), updated = col("^update")
  )
  if (anyNA(out$geo)) stop("sector_cat(): rows without an ISO3 code: ", paste(cat_raw[[1]][is.na(out$geo)], collapse = ", "))
  if (anyDuplicated(out$geo)) stop("sector_cat(): duplicate economies")
  out
}
