# Small shared helpers of Country Insights (pure): cell keys, labels, JSON arrays, hashing, medians.

COUNTRY_TECHS <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal", "Green Hydrogen",
                   "Nuclear", "Oil", "Solar", "Wind")
# The six supply-chain stages in chain order (2026-10-28; they replace Upstream, Midstream and Downstream):
# SUPPLY_CHAIN_STAGES of R/utils/supply_chain_stages.R, which country_helper_files() sources first. Written
# out here so that this file still stands alone for scripts/84 and the AIB tests, which source it by itself
# for country_json(); tests/testthat/test-country-check.R checks that the two are equal.
COUNTRY_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
# A technology in running text, as the modifier of a stage ("battery refining", "electric-vehicle
# assembly"): label_prose of the cells.
COUNTRY_TECH_PROSE <- c(Batteries = "battery", Coal = "coal", `Electric Grid` = "electric-grid", `Electric Vehicles` = "electric-vehicle",
                        Gas = "gas", Geothermal = "geothermal", `Green Hydrogen` = "green-hydrogen", Nuclear = "nuclear", Oil = "oil",
                        Solar = "solar", Wind = "wind")
# The sectors with an Economic Opportunity score but no Energy Security score for any economy: the pipeline
# computes none there (index_outputs.rds of 2026-10-27: 58 of the 66 sectors have Energy Security rows), so
# at most 58 sectors can be placed (57: Oil - Recovery's Energy Security is the same for every economy, and a
# sector with a constant pillar is not placed either, country_constant_pillars()). scripts/88 derives the
# list from the data (country_no_es_cells()), stops when it differs from this one, and ships it in every sheet
# and the manifest (no_es_cells); the dashboard's NO_ES_CELLS (dashboard/src/country/contract.ts) is the same
# list.
COUNTRY_NO_ES_CELLS <- c("coal-recovery", "electric-grid-deployment", "gas-recovery", "geothermal-extraction", "geothermal-refining",
                         "geothermal-components", "geothermal-assembly", "geothermal-recovery")
COUNTRY_CHINA_HOMES <- c("CHN", "HKG", "MAC")
# 2 since 2026-10-28: the sheets are keyed on the 66 six-stage sectors (version 1: the 33 three-stage ones).
COUNTRY_SCHEMA_VERSION <- 2L

country_slug <- function(x) gsub("[^a-z0-9]+", "-", tolower(x))

# "Oil", "Extraction" -> "oil-extraction" (the Sector cellSlug() rule: the lower-case stage id).
country_cell_key <- function(tech, stage) paste0(country_slug(tech), "-", tolower(stage))

# The 66 cells in the Sector cell order: tech (alphabetical), then the six stages in chain order
# (Extraction, Refining, Components, Assembly, Deployment, Recovery). label "Batteries — Refining" (the
# stage id as its label); label_prose, the sector in running text: the technology as the modifier of the
# lower-case stage ("battery refining", "solar components", "electric-vehicle deployment").
country_cells_tbl <- function() {
  g <- expand.grid(stage = COUNTRY_STAGES, tech = COUNTRY_TECHS, stringsAsFactors = FALSE)[, c("tech", "stage")]
  g$key <- country_cell_key(g$tech, g$stage)
  g$label <- paste0(g$tech, " — ", g$stage)
  g$label_prose <- paste(unname(COUNTRY_TECH_PROSE[g$tech]), tolower(g$stage))
  g$order <- seq_len(nrow(g))
  g
}

# The sectors (cell keys, in the Sector cell order) where no economy has a finite Energy Security score:
# es as in country_cell_stats() (iso3, key, value). The data side of COUNTRY_NO_ES_CELLS.
country_no_es_cells <- function(es, keys = country_cells_tbl()$key) {
  have <- unique(es$key[is.finite(es$value)])
  keys[!keys %in% have]
}

# A character vector that stays a JSON array (jsonlite unboxes length-1 vectors otherwise).
country_arr <- function(x) I(as.character(if (is.null(x)) character() else x))

# An empty JSON object.
country_obj <- function() structure(list(), names = character())

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

country_chr_or_null <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) NULL else as.character(x[1])

# sha256 of a string (digest, else openssl); NA when neither package is installed.
country_sha256 <- function(text) {
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(enc2utf8(text), algo = "sha256", serialize = FALSE))
  if (requireNamespace("openssl", quietly = TRUE)) return(as.character(openssl::sha256(enc2utf8(text))))
  NA_character_
}

# The shortest decimal text that reads back as the same double ("%.15g", else 16, else 17 digits) under
# both R's reader and jsonlite's (the read-back check), which can differ in the last bit at 15 digits.
country_num_text <- function(x) {
  json_back <- function(s) jsonlite::fromJSON(paste0("[", paste(s, collapse = ","), "]"))
  ok_fin <- is.finite(x)
  x0 <- ifelse(ok_fin, x, 0)
  s <- sprintf("%.15g", x0)
  for (dg in c(16, 17)) {
    bad <- ok_fin & (as.numeric(s) != x0 | json_back(s) != x0)
    if (!any(bad)) break
    s[bad] <- sprintf(paste0("%.", dg, "g"), x[bad])
  }
  s[!ok_fin] <- "null"
  s[ok_fin & x == 0] <- "0"
  s
}

# Replace every double of a nested list by its exact JSON text (class "json", inserted verbatim);
# one vectorised conversion per call.
country_json_nums <- function(x) {
  if (is.double(x)) x <- list(x)
  if (!is.list(x)) return(x)
  vals <- rapply(x, function(z) z, classes = "numeric", how = "unlist")
  if (!length(vals)) return(x)
  txt <- country_num_text(unname(vals))
  pos <- 0L
  rapply(x, function(z) {
    n <- length(z)
    t <- txt[pos + seq_len(n)]
    pos <<- pos + n
    if (n == 1 && !inherits(z, "AsIs")) structure(t, class = "json") else lapply(t, function(v) structure(v, class = "json"))
  }, classes = "numeric", how = "replace")
}

# The JSON text of a sheet / manifest / memo: every double written with the shortest text that reads
# back identical (round trip exact), NA -> null.
country_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(country_json_nums(x), auto_unbox = TRUE, json_verbatim = TRUE, na = "null", null = "null", pretty = pretty)
}

# Percentile of x among `others` (the other economies of S_c): 100 * (#lower + 0.5 * #equal) / n.
country_percentile <- function(others, x) {
  others <- others[is.finite(others)]
  if (!length(others) || !is.finite(x)) return(NA_real_)
  100 * (sum(others < x) + 0.5 * sum(others == x)) / length(others)
}

# The plotted position: the percentile pinned to the side of the median its value is on (A9.1).
country_pos <- function(pct, x, med) {
  if (!is.finite(pct) || !is.finite(x) || !is.finite(med)) return(NA_real_)
  if (x >= med) max(pct, 50) else min(pct, 49.5)
}

# The corner from two positions and the split on each axis: high = at or above the split (ties go high).
# Grow = high EO, high ES; Upgrade = high EO, low ES; Exposed (key "secure") = low EO, low ES; Stable (key
# "watch") = low EO, high ES. The keys stay; the display words (config words.quadrant) were Secure and Watch
# until 2026-10-28, renamed so they do not clash with the six-stage posture words.
country_quadrant <- function(es, eo, es_med, eo_med) {
  high_eo <- eo >= eo_med
  high_es <- es >= es_med
  ifelse(high_eo, ifelse(high_es, "grow", "upgrade"), ifelse(high_es, "watch", "secure"))
}

# The quadrant rules (A9.2). "country_median" (the user's rule of 2026-10-15): a sector is high on an axis
# when its plotted position (es.pos / eo.pos, the share of economies the country beats in that sector) is
# at or above the country's median sector, the median of those positions over its placed sectors; ties
# go high. "sector_median" (the rule until 2026-10-14, kept only to compare memos written under it):
# high = the score at or above the sector's median economy (equivalently, position >= 50).
COUNTRY_QUADRANT_RULES <- c("country_median", "sector_median")

# The country's median sector: list(es, eo), the medians of the plotted positions of its placed sectors
# (NA when none is placed).
country_median_sector <- function(cells) {
  placed <- Filter(function(c) isTRUE(c$placed), cells)
  m <- function(pl) {
    x <- vapply(placed, function(c) as.numeric(c[[pl]]$pos %||% NA_real_), 1)
    x <- x[is.finite(x)]
    if (length(x)) stats::median(x) else NA_real_
  }
  list(es = m("es"), eo = m("eo"))
}

# Set the corner of every cell (NULL when not placed) under a rule. Returns list(cells, median).
country_set_quadrants <- function(cells, rule = "country_median") {
  rule <- match.arg(rule, COUNTRY_QUADRANT_RULES)
  med <- country_median_sector(cells)
  for (i in seq_along(cells)) {
    c <- cells[[i]]
    q <- if (!isTRUE(c$placed)) NULL else if (rule == "sector_median") country_quadrant(c$es$v, c$eo$v, c$es$med$v, c$eo$med$v)
         else country_quadrant(c$es$pos, c$eo$pos, med$es, med$eo)
    cells[[i]]["quadrant"] <- list(q)
  }
  list(cells = cells, median = med)
}

# Thirds over n ranked items (A9.4): k = round(n / 3); high = rank <= k; low = rank > n - k.
country_thirds <- function(rank, n) {
  k <- round(n / 3)
  ifelse(is.na(rank), NA_character_, ifelse(rank <= k, "high", ifelse(rank > n - k, "low", "middle")))
}

country_agreement_class <- function(prio_third, pol_third) {
  ifelse(is.na(prio_third) | is.na(pol_third), NA_character_,
    ifelse(prio_third == "high" & pol_third == "high", "aligned_high",
      ifelse(prio_third == "high" & pol_third == "low", "gap",
        ifelse(prio_third == "low" & pol_third == "high", "policy_ahead",
          ifelse(prio_third == "low" & pol_third == "low", "aligned_low", "partial")))))
}

# Fill {placeholders} from a named list.
country_fill <- function(text, vals) {
  for (k in names(vals)) text <- gsub(paste0("{", k, "}"), as.character(vals[[k]] %||% ""), text, fixed = TRUE)
  text
}

# An economy's name in running text, the R mirror of proseName() in dashboard/src/country/derive/text.ts:
# "the United States", "the Netherlands", "Japan".
country_prose_name <- function(name) {
  if (is.null(name) || length(name) != 1 || is.na(name) || !nzchar(name) || grepl("^(the|its) ", name, ignore.case = TRUE)) return(name)
  the <- grepl(paste0("^(United |Russian Federation$|European Union$|Holy See$|Gambia$|Bahamas$|Netherlands$|Philippines$|",
                      "Maldives$|Comoros$|Seychelles$|Czech Republic$|Dominican Republic$|Central African Republic$|",
                      "Kyrgyz Republic$|Slovak Republic$|Democratic Republic|Republic of )"), name) || grepl(" Islands$", name)
  if (the) paste("the", name) else name
}

# The possessive of a name in running text (poss() in derive/text.ts): "Japan's", "the Netherlands'".
country_poss <- function(name) if (endsWith(name, "s")) paste0(name, "'") else paste0(name, "'s")

# Fill a caveat sentence (2026-10-14): {country} and {sovereign} print as running-text names, their
# possessives ("{country}'s") drop the extra s after a final s, and the sentence starts with a capital
# ("The Netherlands has ..."). Other placeholders as country_fill().
country_fill_caveat <- function(text, vals) {
  for (k in c("country", "sovereign")) {
    if (is.null(vals[[k]])) next
    nm <- country_prose_name(as.character(vals[[k]]))
    text <- gsub(paste0("{", k, "}'s"), country_poss(nm), text, fixed = TRUE)
    text <- gsub(paste0("{", k, "}"), nm, text, fixed = TRUE)
  }
  text <- country_fill(text, vals[setdiff(names(vals), c("country", "sovereign"))])
  if (nzchar(text)) substr(text, 1, 1) <- toupper(substr(text, 1, 1))
  text
}

# name, group (china | core | partner | rest), tier and income group of an economy, for partner and trade
# rows. roster: iso3, name, tier, income_group; extra_names: named character (ISO3 -> name) for
# economies outside the roster (Sector dims.geo).
country_party_fn <- function(roster, extra_names = character()) {
  force(roster)
  force(extra_names)
  function(iso) {
    i <- match(iso, roster$iso3)
    t <- if (is.na(i)) NA_character_ else roster$tier[i]
    list(
      name = if (!is.na(i)) roster$name[i] else if (!is.na(extra_names[iso])) unname(extra_names[iso]) else iso,
      group = if (identical(iso, "CHN")) "china" else if (is.na(t) || t == "none") "rest" else t,
      tier = if (is.na(t)) "none" else t,
      income_group = if (is.na(i)) NA_character_ else roster$income_group[i]
    )
  }
}

# Notes of the Sector facts chunk by (var, item, geo, year): f(var, item, iso3, year) -> note or NA.
country_note_fn <- function(notes_by) {
  force(notes_by)
  function(var, item, iso3, year) {
    n <- notes_by[paste(var, item, iso3, year, sep = "\r")]
    if (length(n) && !is.na(n)) unname(n) else NA_character_
  }
}
