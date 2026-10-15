# Policy posture of one economy (spec_country.md A9.4): IEA Policies and Measures record counts by
# technology and stage, the Climate Action Tracker rating (national or the EU's), and the coverage
# table. Pure.
#
# policy_clean (policy_outputs$policy_clean): one row per record x country x technology x stage after
# the cross-cutting expansion; columns policy_id, iso3, tech, supply_chain, tech_crosscut, sc_crosscut,
# status_norm, year. tech_crosscut marks rows created by expanding a record that names no technology;
# sc_crosscut likewise for stage. "Critical Minerals" is a tag outside the 33 sectors.

COUNTRY_PAMS_STATUSES <- c("In force", "Ended", "Unknown")
COUNTRY_CM_TAG <- "Critical Minerals"

# Counts for every economy at once (one pass over policy_clean).
country_pams_counts_all <- function(pc) {
  pc <- pc[, c("policy_id", "iso3", "tech", "supply_chain", "tech_crosscut", "sc_crosscut", "status_norm", "year")]
  pc$tech_crosscut <- pc$tech_crosscut %in% TRUE
  pc$sc_crosscut <- pc$sc_crosscut %in% TRUE
  cnt <- function(d, by) {
    d <- d[!duplicated(d[, c(by, "policy_id")]), , drop = FALSE]
    if (!nrow(d)) {
      e <- d[0, by, drop = FALSE]
      e$n <- integer()
      e$latest_year <- numeric()
      return(e)
    }
    k <- do.call(paste, c(d[by], sep = "\r"))
    n <- tapply(d$policy_id, k, length)
    yr <- tapply(suppressWarnings(as.numeric(d$year)), k, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
    u <- d[!duplicated(k), by, drop = FALSE]
    uk <- do.call(paste, c(u[by], sep = "\r"))
    u$n <- as.integer(n[uk])
    u$latest_year <- as.numeric(yr[uk])
    u
  }
  spec <- cnt(pc[!pc$tech_crosscut & !pc$sc_crosscut, ], c("iso3", "tech", "supply_chain"))
  all <- cnt(pc, c("iso3", "tech", "supply_chain"))
  tech <- cnt(pc[!pc$tech_crosscut, ], c("iso3", "tech"))
  rec <- pc[!duplicated(pc[, c("iso3", "policy_id")]), c("iso3", "policy_id", "status_norm")]
  tot <- tapply(rec$policy_id, rec$iso3, length)
  ts <- pc[!pc$tech_crosscut, c("iso3", "policy_id")]
  ts <- ts[!duplicated(ts), ]
  nt <- pc[pc$tech_crosscut, c("iso3", "policy_id")]
  nt <- nt[!duplicated(nt), ]
  cm <- pc[!pc$tech_crosscut & pc$tech == COUNTRY_CM_TAG, c("iso3", "policy_id")]
  cm <- cm[!duplicated(cm), ]
  isos <- names(tot)
  totals <- data.frame(
    iso3 = isos, total = as.integer(tot[isos]),
    tech_specific = as.integer(table(factor(ts$iso3, levels = isos))),
    no_tech = as.integer(table(factor(nt$iso3, levels = isos))),
    critical_minerals = as.integer(table(factor(cm$iso3, levels = isos))),
    stringsAsFactors = FALSE
  )
  st <- as.data.frame(table(iso3 = rec$iso3, status = rec$status_norm), stringsAsFactors = FALSE)
  names(st)[3] <- "n"
  list(spec = spec, all = all, tech = tech, totals = totals, status = st)
}

# One economy's counts: list(cells = data.frame(key, records_specific, records_tech, records_all,
# latest_year), facts = the PolicyFacts count fields and the coverage table).
country_pams_counts <- function(counts, iso3, cells = country_cells_tbl(), eu_records = NULL) {
  pick <- function(d) d[d$iso3 == iso3, , drop = FALSE]
  sp <- pick(counts$spec)
  al <- pick(counts$all)
  te <- pick(counts$tech)
  key_of <- function(d) if (nrow(d)) country_cell_key(d$tech, d$supply_chain) else character()
  cell_tbl <- data.frame(
    key = cells$key,
    records_specific = as.integer(sp$n[match(cells$key, key_of(sp))]),
    records_tech = as.integer(te$n[match(cells$tech, te$tech)]),
    records_all = as.integer(al$n[match(cells$key, key_of(al))]),
    latest_year = sp$latest_year[match(cells$key, key_of(sp))],
    stringsAsFactors = FALSE
  )
  for (k in c("records_specific", "records_tech", "records_all")) cell_tbl[[k]][is.na(cell_tbl[[k]])] <- 0L
  tot <- counts$totals[counts$totals$iso3 == iso3, , drop = FALSE]
  g <- function(col) if (nrow(tot)) tot[[col]][1] else 0L
  st <- counts$status[counts$status$iso3 == iso3, , drop = FALSE]
  by_status <- lapply(COUNTRY_PAMS_STATUSES, function(s) country_fv(sum(st$n[st$status == s]), "count"))
  names(by_status) <- COUNTRY_PAMS_STATUSES
  techs <- unique(cells$tech)
  spec_m <- lapply(techs, function(t) lapply(COUNTRY_STAGES, function(s) country_fv(cell_tbl$records_specific[cell_tbl$key == country_cell_key(t, s)], "count")))
  cm_stage <- lapply(COUNTRY_STAGES, function(s) {
    v <- sp$n[sp$tech == COUNTRY_CM_TAG & sp$supply_chain == s]
    country_fv(if (length(v)) v[1] else 0L, "count")
  })
  facts <- list(
    pams_total = country_fv(g("total"), "count"),
    pams_tech_specific = country_fv(g("tech_specific"), "count"),
    pams_no_tech = country_fv(g("no_tech"), "count"),
    pams_critical_minerals = country_fv(g("critical_minerals"), "count"),
    pams_by_status = by_status,
    pams_eu_records = if (is.null(eu_records)) NULL else country_fv(eu_records, "count"),
    coverage = list(
      rows = country_arr(techs), cols = country_arr(COUNTRY_STAGES), specific = spec_m,
      tech_any_stage = lapply(techs, function(t) country_fv(te$n[te$tech == t][1] %||% 0L, "count")),
      critical_minerals = cm_stage
    )
  )
  list(cells = cell_tbl, facts = facts)
}

COUNTRY_MONTHS <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# "Nov-24" -> "November 2024" (the CAT file's update column).
country_cat_date_label <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
  m <- regmatches(x, regexec("^([A-Za-z]{3})[- ]?(\\d{2,4})$", trimws(x)))[[1]]
  if (length(m) != 3) return(x)
  mo <- match(tolower(m[2]), tolower(substr(COUNTRY_MONTHS, 1, 3)))
  yr <- as.integer(m[3])
  if (yr < 100) yr <- 2000L + yr
  if (is.na(mo)) x else paste(COUNTRY_MONTHS[mo], yr)
}

# The economy's Climate Action Tracker rating: its own row, else the EU's for an EU member (scope
# eu_bloc), else NULL. cat_tbl: geo, overall, policies_action, domestic_target, fair_share,
# climate_finance, ndc (the file's net-zero column), updated.
country_cat <- function(cat_tbl, iso3, eu_member = FALSE, eu_code = "EUU") {
  row <- cat_tbl[cat_tbl$geo == iso3, , drop = FALSE]
  scope <- "national"
  if (!nrow(row) && isTRUE(eu_member)) {
    row <- cat_tbl[cat_tbl$geo == eu_code, , drop = FALSE]
    scope <- "eu_bloc"
  }
  if (!nrow(row) || is.na(row$overall[1])) return(NULL)
  s <- function(col) country_chr_or_null(row[[col]][1])
  list(
    overall = row$overall[1], date = s("updated"), date_label = country_chr_or_null(country_cat_date_label(row$updated[1])), scope = scope,
    sub = list(policies_action = s("policies_action"), domestic_target = s("domestic_target"), fair_share = s("fair_share"),
               climate_finance = s("climate_finance"), net_zero = s("ndc"))
  )
}
