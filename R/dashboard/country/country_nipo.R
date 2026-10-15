# The strongest industrial-policy measures in force, by sector, from the GTA New Industrial Policy
# Observatory (NIPO) policy table of the latest completed NIPO run (data/processed/nipo_policy_all.rds).
# Context only: NIPO is not part of the composite Policy index (IEA Policies and Measures + Climate
# Action Tracker since October 2026). Pure.
#
# nipo: one row per GTA intervention of the economy: iso3, title, impl_date, removal_date (Date),
# strength (policy_strength_pkg: the measure's strength counting its package), strength_base
# (policy_strength), confidence (mapping_confidence_mean), techs ("Batteries|Solar"), stages
# ("Midstream|Upstream"), url, source.
#
# Strength (R/categories/policy/nipo_policy_index.R build_policy_base()): instrument bite (w_tool: the
# instrument family or GTA type, e.g. a financial grant 0.90, an import ban 1.00, import monitoring
# 0.35) x GTA's assessment (w_status: discriminatory 1.00, neutral 0.50, unclear 0.30, liberalising
# lower) x breadth of beneficiaries (m_scope: sector-wide or horizontal 1.00, named beneficiary 0.60,
# firm-specific 0.40) x the money or trade covered (m_scale: log-scaled against the 95th percentile of
# the extract, capped at 3), times the package multiplier (1 + 0.15 per extra instrument family in the
# same State Act, capped at 1.6).

# A date column of the NIPO policy table as Date, whatever it arrived as: Date; POSIXct (read in its own
# time zone, so a midnight-UTC timestamp never slips to the previous day); ISO "2025-12-18" or
# "18/12/2025" text; or an Excel serial day number. Anything else -> NA.
country_nipo_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) {
    tz <- attr(as.POSIXlt(x), "tzone")[1]
    if (is.null(tz) || is.na(tz) || !nzchar(tz)) tz <- "UTC"
    return(as.Date(format(x, "%Y-%m-%d", tz = tz)))
  }
  if (is.numeric(x)) return(as.Date(ifelse(is.finite(x) & x > 20000 & x < 80000, x, NA_real_), origin = "1899-12-30"))
  s <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(s)))
  iso <- !is.na(s) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)
  out[iso] <- as.Date(substr(s[iso], 1, 10), format = "%Y-%m-%d")
  dmy <- !is.na(s) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", s)
  out[dmy] <- as.Date(s[dmy], format = "%d/%m/%Y")
  out
}

# Which measures count as active on as_of (config nipo.active_rule):
#   not_removed  (the user's rule, 2026-10-12, the config default) no removal date or a later one,
#                whatever the implementation date; measures without an implementation date on or before
#                as_of (announced or scheduled) are listed with in_force = FALSE and counted
#                (n_not_in_force).
#   in_force     implemented on or before as_of, and no removal date or a later one: the NIPO pipeline's
#                own is_active_asof rule (R/categories/policy/nipo_policy_index.R add_asof_flags());
#                announced or scheduled measures are counted (n_not_in_force), never listed.
country_nipo_active <- function(nipo, as_of, rule = "in_force") {
  as_of <- as.Date(as_of)
  kept <- is.na(nipo$removal_date) | nipo$removal_date > as_of
  if (identical(rule, "not_removed")) return(kept)
  kept & !is.na(nipo$impl_date) & nipo$impl_date <= as_of
}

country_nipo_split <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  lapply(strsplit(x, "|", fixed = TRUE), function(v) trimws(v[nzchar(trimws(v))]))
}

# Where Global Trade Alert records each measure (2026-10-14): n_juris = the number of implementing
# jurisdictions (economies) the run records the same title under, and scope = "eu" (an EU-level measure:
# a part of the title prefix before ":", split on "-", ",", "&" and " and ", is one of eu_prefixes, and at
# least eu_min jurisdictions record it), "multi" (recorded under more than one jurisdiction otherwise) or
# "national" (recorded under one jurisdiction only, whatever its prefix: an "EC:" approval of a national
# state-aid scheme is national). Computed over the whole run (all economies), once. Pure.
country_nipo_scope <- function(nipo, eu_prefixes = character(), eu_min = 5L) {
  if (is.null(nipo) || !nrow(nipo)) return(nipo)
  t <- as.character(nipo$title)
  t[is.na(t)] <- ""
  nj <- tapply(nipo$iso3, t, function(x) length(unique(x[!is.na(x)])))
  nipo$n_juris <- as.integer(nj[t])
  pre <- ifelse(grepl("^[^:]{2,60}:", t), sub(":.*$", "", t), "")
  parts <- strsplit(pre, "\\s*(-|,|&|\\band\\b)\\s*", perl = TRUE)
  eu_pre <- vapply(parts, function(p) any(trimws(p) %in% eu_prefixes), TRUE)
  nipo$scope <- ifelse(nipo$n_juris > 1, ifelse(eu_pre & nipo$n_juris >= eu_min, "eu", "multi"), "national")
  nipo
}

# The top-n measures in force per cell. Measures with the same title and implementation date are one
# measure (GTA lists one entry per affected product group or partner); the strongest entry is kept.
# Cell membership: the cell's technology is in the measure's technology list and its stage in the
# stage list (the policy table carries the two lists, not the pairs). Measures without product codes
# are mapped to "Cross-cutting" by the NIPO pipeline and are counted (n_crosscut), not listed by cell.
# Economy-wide measures (2026-10-14): a measure mapped to `broad_min` or more technologies (sanctions,
# most-favoured-nation revocations, export controls) is listed in a cell only after the cell's
# sector-specific measures, strength order kept within each group, and flagged economy_wide. The counts
# (n_active, n_by_cell, ...) do not depend on this order.
country_nipo_top <- function(nipo, iso3, as_of, n = 5, cells = country_cells_tbl(), meta = list(), rule = "in_force", broad_min = 9L) {
  if (is.null(nipo$scope)) nipo <- country_nipo_scope(nipo, unlist(meta$eu_prefixes), meta$eu_min_jurisdictions %||% 5L)
  x <- nipo[nipo$iso3 == iso3, , drop = FALSE]
  covered <- nrow(x) > 0
  dedup <- function(d) d[!duplicated(paste(d$title, d$impl_date)), , drop = FALSE]
  # measures not removed but not yet in force (announced or scheduled: no implementation date on or
  # before as_of): under in_force they are counted and never listed; under not_removed they are listed,
  # each marked in_force = FALSE, and counted
  pending <- x[country_nipo_active(x, as_of, "not_removed") & !country_nipo_active(x, as_of, "in_force") & is.finite(x$strength) & x$strength > 0, , drop = FALSE]
  n_pending <- nrow(dedup(pending))
  x <- x[country_nipo_active(x, as_of, rule) & is.finite(x$strength) & x$strength > 0, , drop = FALSE]
  if (nrow(x)) {
    x <- x[order(-x$strength, -x$confidence, x$title), , drop = FALSE]
    x <- dedup(x)
  }
  techs <- country_nipo_split(x$techs)
  n_cross <- sum(vapply(techs, function(t) "Cross-cutting" %in% t, TRUE))
  broad <- vapply(techs, function(t) length(setdiff(t, "Cross-cutting")) >= broad_min, TRUE)
  stages <- country_nipo_split(x$stages)
  chosen <- integer()
  by_cell <- list()
  n_cell <- list()
  for (i in seq_len(nrow(cells))) {
    hit <- which(vapply(techs, function(t) cells$tech[i] %in% t, TRUE) & vapply(stages, function(s) cells$stage[i] %in% s, TRUE))
    n_cell[[cells$key[i]]] <- country_fv(length(hit), "count")
    if (!length(hit)) next
    hit <- hit[order(broad[hit])]
    top <- utils::head(hit, n)
    chosen <- union(chosen, top)
    by_cell[[cells$key[i]]] <- top
  }
  chosen <- sort(chosen)
  ids <- seq_along(chosen) - 1L
  policies <- lapply(seq_along(chosen), function(j) {
    r <- x[chosen[j], , drop = FALSE]
    list(
      id = ids[j], title = sub("^[^:]{2,60}:[[:space:]]+", "", r$title),
      date = if (is.na(r$impl_date)) NULL else format(r$impl_date), date_label = if (is.na(r$impl_date)) "date not recorded" else country_date_label(r$impl_date),
      in_force = !is.na(r$impl_date) && r$impl_date <= as.Date(as_of),
      removal = if (is.na(r$removal_date)) NULL else format(r$removal_date),
      # additive 2026-10-14: the removal date in words, as date_label
      removal_label = if (is.na(r$removal_date)) NULL else country_date_label(r$removal_date),
      strength = country_fv(r$strength, "index", digits = 4),
      strength_base = country_fv(r$strength_base, "index", digits = 4),
      confidence = country_fv(r$confidence, "index", digits = 4),
      techs = country_arr(techs[[chosen[j]]]), stages = country_arr(stages[[chosen[j]]]),
      url = country_chr_or_null(r$url), source = country_chr_or_null(r$source),
      # additive 2026-10-14: EU-wide, shared with other jurisdictions, or national; and how many record it
      scope = r$scope, n_jurisdictions = country_fv(r$n_juris, "count"),
      # additive 2026-10-14: mapped to broad_min or more technologies (listed after sector-specific measures)
      economy_wide = isTRUE(broad[chosen[j]])
    )
  })
  sc <- x$scope[chosen]
  cells_out <- lapply(by_cell, function(rows) I(ids[match(rows, chosen)]))
  list(
    covered = covered,
    vintage = meta$vintage %||% NULL, vintage_label = meta$vintage_label %||% NULL, as_of = format(as.Date(as_of)),
    measure = "policy_strength_pkg", active_rule = rule,
    economy_wide_min_techs = as.integer(broad_min),
    jurisdictions = country_fv(meta$jurisdictions %||% NA, "count"),
    strength_words = meta$strength_words %||% NULL, confidence_words = meta$confidence_words %||% NULL,
    n_active = country_fv(nrow(x), "count"),
    # additive 2026-10-14: EU-wide measures (Global Trade Alert records them under each EU member) and
    # measures recorded under more than one jurisdiction (EU-wide included), among the active and the listed
    n_active_eu = country_fv(sum(x$scope == "eu"), "count"),
    n_active_shared = country_fv(sum(x$scope != "national"), "count"),
    n_listed = country_fv(length(chosen), "count"),
    n_listed_eu = country_fv(sum(sc == "eu"), "count"),
    n_listed_shared = country_fv(sum(sc != "national"), "count"),
    n_not_in_force = country_fv(n_pending, "count"),
    n_crosscut = country_fv(n_cross, "count"),
    n_by_cell = n_cell,
    cells = if (length(cells_out)) cells_out else country_obj(),
    policies = policies
  )
}
