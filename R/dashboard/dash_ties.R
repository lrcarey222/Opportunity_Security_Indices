# Bilateral ties for the Partners view (Strategic Insights) and the Country partners section (pure).
#
# The raw values behind Friendshore's investment tie, and per-sector partner lists at the paper's
# baseline, shipped as auxiliary chunks of the Strategic bundle (scripts/85; read in the browser on
# demand, never by the engine):
#
#   ties          IMF direct investment positions by partner (both directions, latest year, and the
#                 2020-2024 mean the index ranks), the conduit financial centres, the CM-FDI factory
#                 projects behind the investment tie in Solar, Wind, Batteries and Electric Vehicles
#                 Midstream, Development Potential by sector, shared agreements, UN-vote alignment and
#                 each economy's top partners by Friendshore / Partnership Opportunity (best five)
#   pcell-<slug>  one per sector: each economy's top partners as suppliers (Friendshore) and as
#                 export markets (Partnership Opportunity) at the baseline, with their Partnership
#                 Strength rank
#
# The positions are read with the same name -> ISO3 mapping and filters as
# partnership_strength_build_outbound_edges() (R/themes/partnership_strength/safer_friendshore.R),
# and dash_dip_index() ranks them the way that builder does, so scripts/85 can check that the shipped
# positions reproduce the investment tie in the bundle bit for bit.

# The IMF Direct Investment Positions series shown. out_debt is the one Friendshore ranks.
DASH_DIP_MEASURES <- c(
  out_debt = "Outward Direct investment, Assets (gross), Debt instruments, All entities",
  in_debt = "Inward Direct investment, Liabilities (gross), Debt instruments, All entities",
  out_all = "Outward Direct investment, Net (assets less liabilities), All financial instruments, All entities",
  in_all = "Inward Direct investment, Net (liabilities less assets), All financial instruments, All entities"
)

# Regional aggregates and unallocated positions (the list in partnership_strength_build_outbound_edges();
# scripts/85 checks that the two give the same dyads).
DASH_DIP_AGGREGATES <- c(
  "World", "Not Specified (including Confidential)", "Europe", "North and Central America", "Oceania and Polar Regions",
  "Other Near and Middle East Economies", "Economies of Persian Gulf", "Central and South Asia", "East Asia", "North Africa",
  "North Atlantic and Caribbean", "South America", "Sub-Saharan Africa", "US Pacific Islands"
)

# Same rule as the TypeScript stageSlug(): "Electric Vehicles", "Midstream" -> "electric-vehicles-midstream".
dash_cell_slug <- function(tech, stage) {
  s <- tolower(paste(tech, stage, sep = "-"))
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-|-$", "", s)
}

# IMF DIP rows -> one row per (reporter_iso, partner_iso, measure) with
#   year      the reporter's latest year with any reported position in that measure
#   value     the position in that year (NA when the dyad has none that year)
#   mean_idx  the mean of the reported positions over index_years (the builder's `total`; out_debt only)
# Values are in the file's unit (US$ millions). Only "Reported official data" rows, as in the index.
dash_dip_positions <- function(fdi_raw, country_info, index_years = 2020:2024) {
  need <- c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR")
  miss <- setdiff(need, names(fdi_raw))
  if (length(miss)) stop("dash_dip_positions(): fdi_raw lacks ", paste(miss, collapse = ", "))
  year_cols <- grep("^X?[0-9]{4}$", names(fdi_raw), value = TRUE)
  yrs <- as.integer(sub("^X", "", year_cols))
  fdi <- fdi_raw[fdi_raw$DV_TYPE == "Reported official data" & fdi_raw$INDICATOR %in% DASH_DIP_MEASURES, , drop = FALSE]
  vals <- vapply(year_cols, function(col) suppressWarnings(as.numeric(fdi[[col]])), numeric(nrow(fdi)))
  vals <- matrix(vals, nrow = nrow(fdi))
  nm <- unique(c(fdi$COUNTRY, fdi$COUNTERPART_COUNTRY))
  cl <- partnership_strength_clean_outbound_countries(nm)
  iso <- partnership_strength_country_to_iso(cl, country_info)
  iso[cl %in% DASH_DIP_AGGREGATES] <- NA_character_
  rep_iso <- iso[match(fdi$COUNTRY, nm)]
  par_iso <- iso[match(fdi$COUNTERPART_COUNTRY, nm)]
  measure <- names(DASH_DIP_MEASURES)[match(fdi$INDICATOR, DASH_DIP_MEASURES)]
  ok <- !is.na(rep_iso) & !is.na(par_iso) & rep_iso != par_iso
  rep_iso <- rep_iso[ok]
  par_iso <- par_iso[ok]
  measure <- measure[ok]
  vals <- vals[ok, , drop = FALSE]
  iy <- yrs %in% index_years
  n_idx <- rowSums(!is.na(vals[, iy, drop = FALSE]))
  mean_idx <- ifelse(n_idx > 0, rowSums(vals[, iy, drop = FALSE], na.rm = TRUE) / pmax(n_idx, 1), NA_real_)
  # the reporter's latest year per measure
  any_yr <- !is.na(vals)
  last_of <- apply(any_yr, 1, function(z) if (any(z)) max(which(z)) else NA_integer_)
  key_rm <- paste(rep_iso, measure, sep = "\r")
  last_rm <- tapply(last_of, key_rm, function(z) if (all(is.na(z))) NA_integer_ else max(z, na.rm = TRUE))
  j <- as.integer(last_rm[key_rm])
  latest <- ifelse(is.na(j), NA_real_, vals[cbind(seq_len(nrow(vals)), ifelse(is.na(j), 1L, j))])
  d <- data.frame(reporter_iso = rep_iso, partner_iso = par_iso, measure = measure, year = ifelse(is.na(j), NA_integer_, yrs[ifelse(is.na(j), 1L, j)]),
                  value = latest, mean_idx = ifelse(measure == "out_debt", mean_idx, NA_real_), stringsAsFactors = FALSE)
  # two IMF spellings of one economy: the mean, as in the builder (NA values do not count)
  g <- paste(d$reporter_iso, d$partner_iso, d$measure, sep = "\r")
  mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x[!is.na(x)])
  first <- !duplicated(g)
  out <- d[first, , drop = FALSE]
  if (any(!first)) {
    out$value <- as.numeric(tapply(d$value, g, mean_na)[g[first]])
    out$mean_idx <- as.numeric(tapply(d$mean_idx, g, mean_na)[g[first]])
  }
  out <- out[!is.na(out$value) | !is.na(out$mean_idx), , drop = FALSE]
  out <- out[order(out$reporter_iso, out$measure, out$partner_iso, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# The investment tie from the shipped positions (out_debt mean_idx), ranked as in
# partnership_strength_build_outbound_edges(): the position, the position relative to the partner's
# GDP and relative to both GDPs, each S-curved, averaged; zeros and negatives at 0; S-curved again
# within the reporter. Conduit centres dropped as counterparts and (scope "both") as reporters.
dash_dip_index <- function(pos, gdp_data, gdp_year = 2023, conduit_iso = character(), conduit_scope = "both") {
  x <- pos[pos$measure == "out_debt" & !is.na(pos$mean_idx), , drop = FALSE]
  x <- x[!x$partner_iso %in% conduit_iso & (conduit_scope == "counterpart" | !x$reporter_iso %in% conduit_iso), , drop = FALSE]
  gdp <- gdp_data[as.character(gdp_data$year) == as.character(gdp_year), , drop = FALSE]
  g <- suppressWarnings(as.numeric(gdp$NY.GDP.MKTP.CD))
  gi <- as.character(gdp$iso3c)
  keep <- !is.na(g) & g > 0
  g <- g[keep]
  gi <- gi[keep]
  dup <- duplicated(gi)
  g <- g[!dup]
  gi <- gi[!dup]
  total <- x$mean_idx
  gdp_cp <- g[match(x$partner_iso, gi)] / 1e6
  gdp_r <- g[match(x$reporter_iso, gi)] / 1e6
  ti <- partnership_strength_safe_scurve(total)
  tsi <- partnership_strength_safe_scurve(total / gdp_cp)
  ori <- partnership_strength_safe_scurve((total / gdp_r) / gdp_cp)
  ob <- partnership_strength_row_weighted_mean(list(ti, tsi, ori), c(1, 1, 1))
  ob <- ifelse(total <= 0, 0, ob)
  out <- stats::ave(ob, x$reporter_iso, FUN = partnership_strength_safe_scurve)
  tibble::tibble(reporter_iso = x$reporter_iso, partner_iso = x$partner_iso, outbound_index = out)
}

# A (reporter_iso, partner_iso, value) table as a [U, U] matrix (NA elsewhere).
dash_dyad_matrix <- function(tbl, U, value = "outbound_index") {
  m <- matrix(NA_real_, length(U), length(U), dimnames = list(U, U))
  r <- match(tbl$reporter_iso, U)
  p <- match(tbl$partner_iso, U)
  ok <- !is.na(r) & !is.na(p)
  m[cbind(r[ok], p[ok])] <- as.numeric(tbl[[value]][ok])
  m
}

# Each reporter's top k partners of a [U (reporter), U (partner)] matrix: partners with a value, not
# the reporter, sovereign economies only (as the Partnership Strength ranking); highest first, ties
# in U order; rank = min rank among them; n = how many were ranked.
dash_top_partners <- function(M, U, sovereign, k = 10L) {
  out <- vector("list", length(U))
  for (r in seq_along(U)) {
    v <- M[r, ]
    cand <- which(!is.na(v) & seq_along(U) != r & sovereign %in% TRUE)
    if (!length(cand)) next
    o <- cand[order(-v[cand], cand)]
    rk <- rank(-v[cand], ties.method = "min")[match(o, cand)]
    n <- min(k, length(o))
    out[[r]] <- data.frame(reporter_iso = U[r], k = seq_len(n), partner_iso = U[o[seq_len(n)]], value = unname(v[o[seq_len(n)]]),
                           rank = as.integer(rk[seq_len(n)]), n = length(o), stringsAsFactors = FALSE)
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) {
    return(tibble::tibble(reporter_iso = character(), k = integer(), partner_iso = character(), value = numeric(), rank = integer(), n = integer()))
  }
  tibble::as_tibble(do.call(rbind, out))
}

# The Partnership Strength rank of each (reporter, partner) of a top table (NA: not ranked).
dash_psi_rank_of <- function(top, psi_dyad) {
  i <- match(paste(top$reporter_iso, top$partner_iso), paste(psi_dyad$reporter_iso, psi_dyad$partner_iso))
  as.integer(psi_dyad$rank[i])
}

# The per-sector chunks: list named pcell-<slug>, each list(cell = list(tech, supply_chain, slug),
# top = tibble(reporter_iso, side ("fs" | "po"), k, partner_iso, value, rank, n, psi_rank)).
# cells_res: list(fs_index, po_index) [U, U, C] at the baseline; cells: tibble(cell_id, tech, supply_chain).
dash_cell_chunks <- function(cells_res, psi_dyad, U, sovereign, cells, k = 10L) {
  out <- list()
  for (ci in seq_len(nrow(cells))) {
    sides <- list(fs = cells_res$fs_index[, , ci], po = cells_res$po_index[, , ci])
    top <- do.call(rbind, lapply(names(sides), function(s) {
      t <- dash_top_partners(sides[[s]], U, sovereign, k)
      if (!nrow(t)) return(NULL)
      tibble::add_column(t, side = s, .after = "reporter_iso")
    }))
    if (is.null(top)) {
      top <- tibble::tibble(reporter_iso = character(), side = character(), k = integer(), partner_iso = character(), value = numeric(),
                            rank = integer(), n = integer())
    }
    top$psi_rank <- dash_psi_rank_of(top, psi_dyad)
    slug <- dash_cell_slug(cells$tech[ci], cells$supply_chain[ci])
    out[[paste0("pcell-", slug)]] <- list(cell = list(tech = cells$tech[ci], supply_chain = cells$supply_chain[ci], slug = slug), top = top)
  }
  out
}

# Development Potential by sector: partner_development_tbl rows of "Development Potential Index" ->
# tibble(partner_iso, cell_id, value) over the bundle's cells.
dash_dp_cells <- function(dev_tbl, cells) {
  d <- dev_tbl[dev_tbl$variable == "Development Potential Index", , drop = FALSE]
  cid <- cells$cell_id[match(paste(d$tech, d$supply_chain, sep = "\r"), paste(cells$tech, cells$supply_chain, sep = "\r"))]
  out <- tibble::tibble(partner_iso = as.character(d$partner_iso), cell_id = as.integer(cid), value = as.numeric(d$value))
  out <- out[!is.na(out$cell_id) & !is.na(out$partner_iso), , drop = FALSE]
  out <- out[!duplicated(paste(out$partner_iso, out$cell_id)), , drop = FALSE]
  out[order(out$cell_id, out$partner_iso, method = "radix"), , drop = FALSE]
}

# The CM-FDI ties behind the investment tie (partner_cm_fdi_ties_tbl): investor -> destination by sector.
dash_cmfdi_flows <- function(ties_tbl) {
  t <- tibble::tibble(
    investor_iso = as.character(ties_tbl$investor_iso), destination_iso = as.character(ties_tbl$destination_iso),
    tech = as.character(ties_tbl$tech), supply_chain = as.character(ties_tbl$supply_chain),
    n_projects = as.numeric(ties_tbl$n_projects), n_disclosed = as.numeric(ties_tbl$n_disclosed), value_usd_m = as.numeric(ties_tbl$value_usd_m)
  )
  t[order(t$tech, t$investor_iso, t$destination_iso, method = "radix"), , drop = FALSE]
}

# The ties chunk. dip: dash_dip_positions() (rows of reporters and partners outside U are dropped);
# conduits: partnership_strength_conduit_centres() with in_rule (the centres the configured rule drops);
# cmfdi: dash_cmfdi_flows(); dp: dash_dp_cells(); agreements: data frame iso3, agreement; geo: [U, U]
# UN-vote alignment or NULL; top_all: dash_top_partners() of the best-five Friendshore / Partnership
# Opportunity with side and psi_rank; meta: list of scalars / short texts (years, rule, sources);
# U_names: the economies' names in U order (shipped with U, so a page without the core chunk, the
# Country partners section, can name partners and read geo).
dash_ties_chunk <- function(dip, conduits, cmfdi, dp, agreements, geo, top_all, U, meta, U_names = U) {
  dip <- dip[dip$reporter_iso %in% U & dip$partner_iso %in% U, , drop = FALSE]
  ag <- tibble::tibble(iso3 = as.character(agreements$iso3), agreement = as.character(agreements$agreement))
  ag <- ag[!duplicated(paste(ag$iso3, ag$agreement)), , drop = FALSE]
  out <- list(
    ties = list(
      meta = dash_osb_json(meta),
      U = as.character(U),
      U_names = as.character(U_names),
      dip = tibble::tibble(reporter_iso = dip$reporter_iso, partner_iso = dip$partner_iso, measure = dip$measure,
                           year = as.integer(dip$year), value = as.numeric(dip$value), mean_idx = as.numeric(dip$mean_idx)),
      conduits = tibble::tibble(iso3 = as.character(conduits$iso3), name = as.character(conduits$name),
                                dej_top10 = as.logical(conduits$dej_top10), in_rule = as.logical(conduits$in_rule)),
      cmfdi = cmfdi,
      dp = dp,
      agreements = ag,
      top_all = top_all
    )
  )
  if (!is.null(geo)) out$ties$geo <- unname(geo)
  out
}
