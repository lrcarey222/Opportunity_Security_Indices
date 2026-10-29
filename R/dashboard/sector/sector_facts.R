# Facts from data/processed (spec_sector.md 8.1, 8.9). Pure: every function takes a processed table
# and returns list(facts, ledger, excluded):
#   facts     var, item_key, geo, year, value, status, obs_year (true data years from the dictionary,
#             never the processed `Year`)
#   ledger    the parity ledger (not shipped): var, item_key, geo, year, prov, table, variable, key,
#             source_value, formula_value. prov P: the shipped value must be identical() to
#             source_value; D: identical() to formula_value = f(source values); X: within 1e-12 of
#             formula_value (a cross-check with processed).
#   excluded  (table, variable, reason, n): processed raw rows not shipped, by reason.
# and, where a fact needs one, `notes` (var, item_key, geo, year, note): per-fact notes shipped in
# facts.notes, `note` = a dict.caveats id (EIA provenance, production assumed 0, Estonia's oil shale).
# Only data_type == "raw" rows are read; "Overall ..." variables never.

sector_raw_rows <- function(tbl) {
  tbl[tbl$data_type == "raw" & !grepl("^Overall", tbl$variable), , drop = FALSE]
}

sector_result <- function(facts = NULL, ledger = NULL, excluded = NULL) {
  list(facts = if (is.null(facts)) sector_facts_empty() else facts,
       ledger = if (is.null(ledger)) sector_ledger_empty() else ledger,
       excluded = if (is.null(excluded)) sector_excluded_empty() else excluded)
}
sector_facts_empty <- function() {
  tibble::tibble(var = character(), item_key = character(), geo = character(), year = integer(), value = double(),
                 status = character(), obs_year = integer())
}
sector_ledger_empty <- function() {
  tibble::tibble(var = character(), item_key = character(), geo = character(), year = integer(), prov = character(),
                 table = character(), variable = character(), key = character(), source_value = double(), formula_value = double())
}
sector_excluded_empty <- function() tibble::tibble(table = character(), variable = character(), reason = character(), n = integer())
sector_notes_empty <- function() {
  tibble::tibble(var = character(), item_key = character(), geo = character(), year = integer(), note = character())
}

# Bind a list of sector_result()s (and their per-fact notes, where any).
sector_bind_results <- function(rs) {
  list(facts = dplyr::bind_rows(lapply(rs, `[[`, "facts")),
       ledger = dplyr::bind_rows(lapply(rs, `[[`, "ledger")),
       excluded = dplyr::bind_rows(lapply(rs, `[[`, "excluded")),
       notes = dplyr::bind_rows(c(list(sector_notes_empty()), lapply(rs, `[[`, "notes"))))
}

# Per-fact notes: one row per (fact, note) where `note` (a caveat id, or NA for none) applies.
sector_fact_notes <- function(facts, note) {
  note <- rep_len(as.character(note), nrow(facts))
  hit <- !is.na(note) & nzchar(note)
  if (!any(hit)) return(sector_notes_empty())
  tibble::tibble(var = facts$var[hit], item_key = facts$item_key[hit], geo = facts$geo[hit], year = facts$year[hit],
                 note = note[hit])
}

# Facts and their ledger rows at once. src: the processed value (P) shipped verbatim.
sector_fx <- function(var, item_key, geo, year, value, status, table, variable, prov = "P", obs_year = NA_integer_,
                      formula_value = NA_real_, key = NA_character_) {
  n <- length(value)
  # as.character(): an empty input (ifelse() on zero rows gives logical(0)) keeps the column types.
  f <- tibble::tibble(var = as.character(rep_len(var, n)), item_key = as.character(rep_len(item_key, n)), geo = as.character(rep_len(geo, n)),
                      year = as.integer(rep_len(year, n)), value = as.double(value), status = as.character(rep_len(status, n)),
                      obs_year = as.integer(rep_len(obs_year, n)))
  shipped <- !is.na(f$value)
  l <- tibble::tibble(var = f$var, item_key = f$item_key, geo = f$geo, year = f$year, prov = as.character(rep_len(prov, n)),
                      table = as.character(rep_len(table, n)), variable = as.character(rep_len(variable, n)), key = as.character(rep_len(key, n)), source_value = f$value,
                      formula_value = as.double(rep_len(formula_value, n)))
  list(facts = f, ledger = l[shipped, , drop = FALSE])
}

sector_excl <- function(table, variable, reason, n) {
  if (!n) return(NULL)
  tibble::tibble(table = table, variable = variable, reason = reason, n = as.integer(n))
}

# Collapse copies: one row per `keys`; stops when a copy group holds two values (or two years).
sector_debroadcast <- function(rows, keys, label, value_cols = "value") {
  key <- do.call(paste, c(unname(as.list(rows[keys])), sep = "\r"))
  for (vc in value_cols) {
    v <- rows[[vc]]
    first <- !duplicated(key)
    ref <- v[first][match(key, key[first])]
    same <- (is.na(v) & is.na(ref)) | (!is.na(v) & !is.na(ref) & v == ref)
    if (!all(same)) {
      bad <- unique(key[!same])
      stop("sector_debroadcast(", label, "): copies differ in ", vc, " for ", length(bad), " group(s), e.g. ",
           gsub("\r", " / ", paste(utils::head(bad, 3), collapse = "; ")))
    }
  }
  rows[!duplicated(key), , drop = FALSE]
}

# major_supplier_shares_tbl repeats each mineral measure once per technology that uses the mineral:
# collapse the copies on (measure, mineral, iso3), asserting identical value and world_share.
sector_supplier_dedupe <- function(mss, measures = c("iea_mineral_mining", "iea_mineral_refining", "ei_mineral_mining")) {
  s <- mss[mss$measure %in% measures, , drop = FALSE]
  sector_debroadcast(s, c("measure", "mineral", "iso3"), "major_supplier_shares_tbl (technology copies)", c("value", "world_share"))
}

sector_check_iso3 <- function(x, label) {
  bad <- unique(x[!grepl("^[A-Z]{3}$", x)])
  if (length(bad)) stop(label, ": codes that are not ISO3: ", paste(utils::head(bad, 8), collapse = ", "))
  invisible(TRUE)
}

# ---- Trade (processed): Atlas shares, RCA, exporter-reported totals -----------------------------------

sector_facts_atlas <- function(tc) {
  r <- sector_raw_rows(tc)
  r <- r[r$variable == "market_share" & !is.na(r$value), , drop = FALSE]
  sector_check_iso3(r$iso3c, "trade_concentration_tbl market_share")
  x <- sector_fx("trade.atlas_share", paste0("cell:", sector_cell_slug(r$tech, r$supply_chain)), r$iso3c, 2023L,
                 r$value, ifelse(r$value == 0, "zero", "obs"), "trade_concentration_tbl", "market_share",
                 key = paste(r$iso3c, r$tech, r$supply_chain))
  w <- unique(x$facts[c("var", "item_key", "year")])
  w$value <- 1
  w$basis <- "one"
  c(sector_result(x$facts, x$ledger), list(world = w))
}

# RCA (exporter-reported): rows whose exporter-reported basket exports are below min_exports read
# "too small to compare" (status nq, value NaN; not in the ledger).
sector_facts_rca <- function(ef, min_exports = 5e7) {
  r <- sector_raw_rows(ef)
  rca <- r[r$variable == "export_rca" & !is.na(r$value), , drop = FALSE]
  ex <- r[r$variable == "exports", , drop = FALSE]
  base <- ex$value[match(paste(rca$iso3c, rca$tech, rca$supply_chain), paste(ex$iso3c, ex$tech, ex$supply_chain))]
  small <- is.na(base) | base < min_exports
  sector_check_iso3(rca$iso3c, "export_feasibility_tbl export_rca")
  x <- sector_fx("trade.rca", paste0("cell:", sector_cell_slug(rca$tech, rca$supply_chain)), rca$iso3c, 2024L,
                 ifelse(small, NaN, rca$value), ifelse(small, "nq", "obs"), "export_feasibility_tbl", "export_rca",
                 key = paste(rca$iso3c, rca$tech, rca$supply_chain))
  sector_result(x$facts, x$ledger, sector_excl("export_feasibility_tbl", "export_rca", "rca_below_min_exports (shipped as nq)", sum(small)))
}

# Export feasibility (Harvard Atlas relatedness): the processed raw `feasibility` of
# export_feasibility_tbl verbatim, 1 - the mean Atlas product-space distance over the basket's HS4
# headings (trade_core_build_aec_feasibility(); each heading once). `year` is the Atlas HS4 data year
# that the rows' explanation names in brackets ("... [2023]"; the table's Year column is the Comtrade
# year): the caller passes sector_feasibility_year().
sector_facts_feasibility <- function(ef, year) {
  r <- sector_raw_rows(ef)
  r <- r[r$variable == "feasibility", , drop = FALSE]
  na <- is.na(r$value)
  r <- r[!na, , drop = FALSE]
  sector_check_iso3(r$iso3c, "export_feasibility_tbl feasibility")
  x <- sector_fx("trade.feasibility", paste0("cell:", sector_cell_slug(r$tech, r$supply_chain)), r$iso3c, as.integer(year),
                 r$value, ifelse(r$value == 0, "zero", "obs"), "export_feasibility_tbl", "feasibility",
                 key = paste(r$iso3c, r$tech, r$supply_chain))
  sector_result(x$facts, x$ledger, sector_excl("export_feasibility_tbl", "feasibility", "NA", sum(na)))
}

# The Atlas HS4 data year of the feasibility rows: the single bracketed year their explanation names
# ("1 - mean Atlas product-space distance over the basket's HS4 headings [2023]"). Stops when the rows
# name none or more than one.
sector_feasibility_year <- function(ef) {
  r <- sector_raw_rows(ef)
  ex <- unique(r$explanation[r$variable == "feasibility"])
  y <- unique(regmatches(ex, regexpr("\\[(19|20)[0-9]{2}\\]", ex)))
  if (length(y) != 1) {
    stop("export_feasibility_tbl feasibility: expected one bracketed data year in the explanation, found ",
         if (length(y)) paste(y, collapse = ", ") else "none")
  }
  as.integer(gsub("[^0-9]", "", y))
}

# Exporter-reported totals per cell for the reconciliation table: total, China's share, the
# processed HHI (de-broadcast; one value per cell) and the number of reporters.
sector_reported_trade <- function(tc, china = "CHN") {
  r <- sector_raw_rows(tc)
  ex <- r[r$variable == "exports" & !is.na(r$value), , drop = FALSE]
  hhi <- sector_debroadcast(r[r$variable == "HHI", c("tech", "supply_chain", "value")], c("tech", "supply_chain"), "trade HHI")
  cells <- unique(ex[c("tech", "supply_chain")])
  out <- lapply(seq_len(nrow(cells)), function(i) {
    e <- ex[ex$tech == cells$tech[i] & ex$supply_chain == cells$supply_chain[i], , drop = FALSE]
    tot <- sum(e$value)
    h <- hhi$value[hhi$tech == cells$tech[i] & hhi$supply_chain == cells$supply_chain[i]]
    tibble::tibble(item_key = paste0("cell:", sector_cell_slug(cells$tech[i], cells$supply_chain[i])), year = 2024L,
                   total = tot, china_reported = sum(e$value[e$iso3c == china]) / tot,
                   hhi_reported = if (length(h)) h[1] else NA_real_, n_reporters = sum(e$value > 0))
  })
  dplyr::bind_rows(out)
}

# Mineral exports and export dependence (critical_minerals_trade_flows_tbl, one row per exporter x
# mineral x stage since 2026-10-27: ores and concentrates at Extraction, refined metal and chemicals at
# Refining, scrap at Recovery). The item carries the stage (axis mineral_stage, "lithium-refining"):
# each stage is its own basket with its own world total. Status: mirror (partner-reported), zero, obs.
# World = the sum over every exporter of the mineral and stage, cross-checked against the processed
# market shares. Returns also `pairs`: the (mineral slug, stage) pairs (the mineral_stage items).
sector_facts_critmin <- function(fl) {
  sector_check_iso3(fl$iso3c, "critical_minerals_trade_flows_tbl")
  slug <- sector_mineral_slug(fl$mineral)
  if (anyNA(slug)) stop("critical minerals flows: unknown minerals ", paste(unique(fl$mineral[is.na(slug)]), collapse = ", "))
  if (!"supply_chain" %in% names(fl)) stop("critical_minerals_trade_flows_tbl has no supply_chain (built before the six-stage minerals trade): rerun scripts/10")
  stage <- as.character(fl$supply_chain)
  bad <- !stage %in% SECTOR_CRITMIN_STAGES
  if (any(bad)) stop("critical minerals flows: stages outside ", paste(SECTOR_CRITMIN_STAGES, collapse = ", "), ": ", paste(unique(stage[bad]), collapse = ", "))
  year <- as.integer(fl$Year)
  item <- paste0("mineral_stage:", slug, "-", tolower(stage))
  key <- paste(fl$iso3c, fl$mineral, stage)
  # A mirror row is from partners' declarations even when they declare nothing (a 0 there is no
  # report of zero by the economy itself): mirror, never zero.
  st <- ifelse(fl$export_source == "mirror", "mirror", ifelse(fl$exports == 0, "zero", "obs"))
  a <- sector_fx("trade.critmin_exports", item, fl$iso3c, year, fl$exports, st, "critical_minerals_trade_flows_tbl", "exports", key = key)
  st2 <- ifelse(fl$export_source == "mirror", "mirror", ifelse(fl$exp_share == 0, "zero", "obs"))
  ok2 <- !is.na(fl$exp_share)
  b <- sector_fx("trade.critmin_dependence", item[ok2], fl$iso3c[ok2], year[ok2], fl$exp_share[ok2], st2[ok2],
                 "critical_minerals_trade_flows_tbl", "exp_share", key = key[ok2])
  w <- stats::aggregate(list(value = fl$exports), list(item_key = item, year = year), sum)
  world <- tibble::tibble(var = "trade.critmin_exports", item_key = w$item_key, year = as.integer(w$year), value = w$value, basis = "sum_all")
  # Cross-check: exports / W = the processed market share (per mineral and stage).
  W <- w$value[match(paste(item, year), paste(w$item_key, w$year))]
  rel <- abs(fl$exports / W - fl$market_share) / pmax(abs(fl$market_share), 1e-300)
  rel[fl$exports == 0 & fl$market_share == 0] <- 0
  out <- sector_result(dplyr::bind_rows(a$facts, b$facts), dplyr::bind_rows(a$ledger, b$ledger),
                       sector_excl("critical_minerals_trade_flows_tbl", "exp_share", "NA", sum(!ok2)))
  c(out, list(world = world, check = list(max_rel = max(rel, na.rm = TRUE)), pairs = unique(tibble::tibble(mineral = slug, stage = stage))))
}

# ---- Reserves and potentials ---------------------------------------------------------------------------

# Uranium (resources, mine production and the Red Book series) is the Nuclear Extraction cell.
SECTOR_URANIUM_CELL <- "nuclear-extraction"

# reserves_tbl: fuels (end-2020), minerals (USGS end-2025) and uranium resources (1 Jan 2025).
# NA rows: listed-without-a-figure holders (unquantified: tibble(tech, ISO3166_alpha3)) ship as nq
# with NaN; the rest are the roster completion and are not shipped.
sector_facts_reserves <- function(res, unquantified) {
  r <- sector_raw_rows(res)
  r$tech_name <- sub(" Reserves$", "", r$variable)
  fuel <- r$tech_name %in% c("Oil", "Gas", "Coal")
  uran <- r$tech_name == "Nuclear"
  mineral <- !fuel & !uran
  out <- list()
  na <- is.na(r$value)
  nq <- mineral & na & paste(r$tech_name, r$iso3c) %in% paste(unquantified$tech, unquantified$ISO3166_alpha3)
  keep <- !na | nq
  st <- ifelse(nq, "nq", ifelse(r$value == 0, "zero", "obs"))
  sector_check_iso3(r$iso3c[keep], "reserves_tbl")
  sel <- keep & fuel
  out[[1]] <- sector_fx("resources.fuel_reserves", paste0("fuel:", tolower(r$tech_name[sel])), r$iso3c[sel], 2020L,
                        r$value[sel], st[sel], "reserves_tbl", r$variable[sel], key = r$iso3c[sel])
  sel <- keep & mineral
  out[[2]] <- sector_fx("resources.mineral_reserves", paste0("mineral:", sector_mineral_slug(r$tech_name[sel])), r$iso3c[sel], 2025L,
                        ifelse(nq[sel], NaN, r$value[sel]), st[sel], "reserves_tbl", r$variable[sel], key = r$iso3c[sel])
  sel <- keep & uran
  out[[3]] <- sector_fx("resources.uranium_resources", paste0("cell:", SECTOR_URANIUM_CELL), r$iso3c[sel], 2025L,
                        r$value[sel], st[sel], "reserves_tbl", r$variable[sel], key = r$iso3c[sel])
  ex <- dplyr::bind_rows(
    sector_excl("reserves_tbl", "*", "NA (roster completion: economy not listed by the source)", sum(na & !nq)),
    sector_excl("reserves_tbl", "<Mineral> Reserves", "NA listed without a figure (shipped as nq)", sum(nq))
  )
  sector_result(dplyr::bind_rows(lapply(out, `[[`, "facts")), dplyr::bind_rows(lapply(out, `[[`, "ledger")), ex)
}

# Solar, wind and geothermal potentials (static GIS layers; true year: the dataset label). The site
# resource is the Deployment cell of its technology (where the plants are built); a processed table
# filed at another stage stops.
sector_facts_potential <- function(solar, wind, geo) {
  spec <- tibble::tribble(
    ~tbl, ~variable, ~var, ~cell,
    "solar_pv_potential_tbl", "Solar PV power potential per sq km", "resources.pv_quality", "solar-deployment",
    "solar_pv_potential_tbl", "Solar PV power potential", "resources.pv_area", "solar-deployment",
    "wind_potential_tbl", "Wind power potential per sq km", "resources.wind_density", "wind-deployment",
    "wind_potential_tbl", "Wind power potential", "resources.wind_area", "wind-deployment",
    "wind_potential_tbl", "Wind power potential share over threshold", "resources.wind_share", "wind-deployment",
    "geothermal_potential_tbl", "Geothermal total potential (MW)", "resources.geo_potential", "geothermal-deployment",
    "geothermal_potential_tbl", "Geothermal LCOE (500 MW)", "resources.geo_lcoe_500", "geothermal-deployment",
    "geothermal_potential_tbl", "Geothermal LCOE (5 GW)", "resources.geo_lcoe_5000", "geothermal-deployment"
  )
  for (t in list(solar, wind, geo)) {
    if ("supply_chain" %in% names(t)) {
      st <- unique(as.character(sector_raw_rows(t)$supply_chain))
      if (length(setdiff(st, c("Deployment", NA)))) stop("resource potentials filed at ", paste(setdiff(st, "Deployment"), collapse = ", "), ", not Deployment")
    }
  }
  src <- list(solar_pv_potential_tbl = sector_raw_rows(solar), wind_potential_tbl = sector_raw_rows(wind),
              geothermal_potential_tbl = sector_raw_rows(geo))
  rs <- lapply(seq_len(nrow(spec)), function(i) {
    r <- src[[spec$tbl[i]]]
    r <- r[r$variable == spec$variable[i], , drop = FALSE]
    na <- is.na(r$value)
    r <- r[!na, , drop = FALSE]
    sector_check_iso3(r$iso3c, spec$tbl[i])
    x <- sector_fx(spec$var[i], paste0("cell:", spec$cell[i]), r$iso3c, 2023L, r$value, ifelse(r$value == 0, "zero", "obs"),
                   spec$tbl[i], spec$variable[i], key = r$iso3c)
    sector_result(x$facts, x$ledger, sector_excl(spec$tbl[i], spec$variable[i], "NA (no tranche or not covered)", sum(na)))
  })
  sector_bind_results(rs)
}

# ---- EIA International Energy Statistics (fossil production EI does not report) ----------------------
#
# Since 2026-10-11 the pipeline takes the oil, gas and coal production EI does not report for an
# economy (none, or inside an EI "Other <region>" residual) from the EIA International Energy
# Statistics (R/utils/eia_intl.R; import_dependence_tbl and production_depth_momentum_tbl), converted
# to EI's units, and names EIA in the row's `source`. Such a value is observed data: the exporter
# ships it as obs (or zero), with the EIA year as obs_year and a per-fact note (facts.notes:
# eia_production). Where neither EI nor EIA gives a comparable value (2025: Estonia coal, where EI
# counts oil shale as coal; note ei_oil_shale), the production fact is nsr with NaN, and the balance
# and self-sufficiency follow the user rule of 2026-10-11: production is assumed to be 0 and the
# fact says so (status nsr, note production_assumed_zero), so the net balance reads minus
# consumption and self-sufficiency 0%. Tables built before the EIA fill (production zero-filled,
# no EIA in `source`) take the same path: their zero-filled balance is already minus consumption.

SECTOR_EIA_SOURCE <- "EIA International Energy Statistics"
SECTOR_EI_PRODUCTION_VARS <- c(Oil = "oilprod_kbd", Gas = "gasprod_ej", Coal = "coalprod_ej")
SECTOR_EI_CONSUMPTION_VARS <- c(Oil = "oil_tes_ej", Gas = "gas_tes_ej", Coal = "coal_tes_ej")
# Per-fact notes (dict.caveats ids) whose var-level caveat is shown only where the build has such a
# fact (sector_assemble()): the EIA and assumed-zero notes, a figure the source gives once for
# Components and Assembly (same_figure_pair) and the critical-minerals investment split
# (cim_minerals_split).
SECTOR_DATA_CAVEATS <- c("eia_production", "eia_consumption", "production_assumed_zero", "fossil_assumed_zero", "ei_oil_shale",
                         "same_figure_pair", "cim_minerals_split")

# Rows of a processed table whose source names the EIA International Energy Statistics.
sector_cites_eia <- function(rows) {
  if (!"source" %in% names(rows)) return(rep(FALSE, nrow(rows)))
  !is.na(rows$source) & grepl(SECTOR_EIA_SOURCE, rows$source, fixed = TRUE)
}

# The cells to look up in the staged EIA extract: every (iso3, EI production variable) of `iso3`
# without an EI row in `year`, with EI's consumption of the fuel (EJ, NA without an EI row) for the
# comparability check of the pipeline's eia_intl_lookup(). ei_values: tibble(ei_var, iso3, value) of
# EI's rows in `year` (value not NA).
sector_eia_gap_cells <- function(iso3, ei_values, year) {
  iso3 <- sort(unique(iso3[grepl("^[A-Z]{3}$", iso3)]))
  cells <- tidyr::expand_grid(iso3 = iso3, ei_var = unname(SECTOR_EI_PRODUCTION_VARS))
  have <- paste(ei_values$ei_var, ei_values$iso3)
  cells <- cells[!paste(cells$ei_var, cells$iso3) %in% have, , drop = FALSE]
  cons_var <- unname(SECTOR_EI_CONSUMPTION_VARS[match(cells$ei_var, SECTOR_EI_PRODUCTION_VARS)])
  cells$year <- as.integer(year)
  cells$ei_consumption <- ei_values$value[match(paste(cons_var, cells$iso3), have)]
  cells
}

# EI's consumption of oil, gas, coal and fossil (their sum, a missing gas or coal row counting as 0,
# as the pipeline does) in EJ: tibble(iso3, tech, value), for the balance of a fuel whose production
# neither EI nor EIA reports (minus consumption). ei_values as for sector_eia_gap_cells(); economies
# without an EI oil consumption row are left out (their consumption is EIA's, not EI's).
sector_ei_consumption <- function(ei_values) {
  get <- function(v) ei_values[ei_values$ei_var == v, c("iso3", "value"), drop = FALSE]
  oil <- get("oil_tes_ej")
  out <- list(tibble::tibble(iso3 = oil$iso3, tech = "Oil", value = as.double(oil$value)))
  fossil <- as.double(oil$value)
  for (f in c("Gas", "Coal")) {
    x <- get(SECTOR_EI_CONSUMPTION_VARS[[f]])
    v <- as.double(x$value[match(oil$iso3, x$iso3)])
    v[is.na(v)] <- 0
    out[[length(out) + 1]] <- tibble::tibble(iso3 = oil$iso3, tech = f, value = v)
    fossil <- fossil + v
  }
  dplyr::bind_rows(c(out, list(tibble::tibble(iso3 = oil$iso3, tech = "Fossil", value = fossil))))
}

# The EIA-filled production facts against the staged extract: each fact the processed table took
# from EIA must equal eia_intl_lookup()'s value (1e-12 relative). facts: the production.fuels facts
# with their notes (sector_facts_production()); eia_cells: the lookup. Returns list(n, n_missing,
# max_rel, not_comparable (keys "ISO3 fuel")).
sector_eia_check <- function(prod, eia_cells) {
  f <- prod$facts[prod$facts$var == "production.fuels", , drop = FALSE]
  nk <- prod$notes[prod$notes$note == "eia_production", , drop = FALSE]
  f <- f[paste(f$item_key, f$geo) %in% paste(nk$item_key, nk$geo), , drop = FALSE]
  ev <- unname(SECTOR_EI_PRODUCTION_VARS[match(sub("^fuel:", "", f$item_key), tolower(names(SECTOR_EI_PRODUCTION_VARS)))])
  ref <- eia_cells$value[match(paste(f$geo, ev), paste(eia_cells$iso3, eia_cells$ei_var))]
  rel <- abs(f$value - ref) / pmax(abs(ref), 1e-300)
  rel[f$value == 0 & ref %in% 0] <- 0
  nc <- eia_cells[eia_cells$eia_basis %in% "not_comparable", , drop = FALSE]
  list(n = nrow(f), n_missing = sum(is.na(ref)), max_rel = if (any(!is.na(rel))) max(rel, na.rm = TRUE) else 0,
       not_comparable = paste(nc$iso3, names(SECTOR_EI_PRODUCTION_VARS)[match(nc$ei_var, SECTOR_EI_PRODUCTION_VARS)]))
}

# ---- Production -------------------------------------------------------------------------------------

# production_depth_momentum_tbl production_latest: fuels (Extraction), generation (Deployment) and
# uranium (Nuclear Extraction, 2024). ei_present: tibble(ei_var, iso3) of EI rows present in the raw
# CSV for the latest year. A fuel value without an EI row is EIA's when the row's source names EIA
# (obs or zero; obs_year = the EIA year from eia_cells, note eia_production), NA when neither gives
# one (nsr, NaN), else the zero-fill of a table built before the EIA fill (nsr, 0); note ei_oil_shale
# where EIA's value fails the comparability check (eia_cells eia_basis not_comparable). Generation EI does not report is still
# the zero-fill (nsr). eia_cells: eia_intl_lookup() of sector_eia_gap_cells() (iso3, ei_var,
# value, eia_year, eia_basis), or NULL. The clean-tech Extraction and Refining cells hold a
# cross-mineral roll-up and are not shipped.
sector_facts_production <- function(pdm, ei_present, latest_year = 2025L, uranium_year = 2024L, eia_cells = NULL) {
  r <- sector_raw_rows(pdm)
  r <- r[r$variable == "production_latest", , drop = FALSE]
  cell <- paste(r$tech, r$supply_chain)
  fuel_map <- c("Oil Extraction" = "oilprod_kbd", "Gas Extraction" = "gasprod_ej", "Coal Extraction" = "coalprod_ej")
  gen_map <- c("Solar Deployment" = "solar_twh", "Wind Deployment" = "wind_twh", "Nuclear Deployment" = "nuclear_twh",
               "Coal Deployment" = "electbyfuel_coal", "Gas Deployment" = "electbyfuel_gas", "Oil Deployment" = "electbyfuel_oil")
  pres <- paste(ei_present$ei_var, ei_present$iso3)
  out <- list()
  f <- r[cell %in% names(fuel_map), , drop = FALSE]
  sector_check_iso3(f$iso3c, "production_depth_momentum_tbl")
  ev <- unname(fuel_map[paste(f$tech, f$supply_chain)])
  present <- paste(ev, f$iso3c) %in% pres
  eia <- !present & !is.na(f$value) & sector_cites_eia(f)
  ek <- if (is.null(eia_cells)) rep(NA_integer_, nrow(f)) else match(paste(f$iso3c, ev), paste(eia_cells$iso3, eia_cells$ei_var))
  eia_year <- if (is.null(eia_cells)) rep(NA_integer_, nrow(f)) else as.integer(eia_cells$eia_year[ek])
  not_comparable <- !present & !eia & (if (is.null(eia_cells)) FALSE else eia_cells$eia_basis[ek] %in% "not_comparable")
  st <- ifelse(is.na(f$value), "nsr", ifelse(f$value > 0, "obs", ifelse(present | eia, "zero", "nsr")))
  out[[1]] <- sector_fx("production.fuels", paste0("fuel:", tolower(f$tech)), f$iso3c, latest_year,
                        ifelse(is.na(f$value), NaN, f$value), st,
                        "production_depth_momentum_tbl", "production_latest", obs_year = ifelse(eia, eia_year, NA_integer_),
                        key = paste(f$iso3c, f$tech, f$supply_chain))
  notes <- sector_fact_notes(out[[1]]$facts, ifelse(eia, "eia_production", ifelse(not_comparable, "ei_oil_shale", NA_character_)))
  g <- r[cell %in% names(gen_map), , drop = FALSE]
  ev <- gen_map[paste(g$tech, g$supply_chain)]
  gp <- paste(ev, g$iso3c) %in% pres
  out[[2]] <- sector_fx("production.generation", paste0("fuel:", tolower(g$tech)), g$iso3c, latest_year,
                        ifelse(is.na(g$value), NaN, g$value),
                        ifelse(is.na(g$value), "nsr", ifelse(g$value > 0, "obs", ifelse(gp, "zero", "nsr"))),
                        "production_depth_momentum_tbl", "production_latest", key = paste(g$iso3c, g$tech, g$supply_chain))
  u <- r[cell == "Nuclear Extraction", , drop = FALSE]
  sector_check_iso3(u$iso3c, "production_depth_momentum_tbl uranium")
  out[[3]] <- sector_fx("production.uranium", paste0("cell:", SECTOR_URANIUM_CELL), u$iso3c, uranium_year, u$value,
                        ifelse(u$value > 0, "obs", "zero"), "production_depth_momentum_tbl", "production_latest", key = u$iso3c)
  roll <- !cell %in% c(names(fuel_map), names(gen_map), "Nuclear Extraction")
  # Anything else must be the clean-tech mineral roll-up (Extraction or Refining): a new cell stops.
  odd <- unique(cell[roll & !r$supply_chain %in% SECTOR_MINERAL_STAGES])
  if (length(odd)) stop("production_depth_momentum_tbl production_latest: cells without a rule: ", paste(odd, collapse = ", "))
  c(sector_result(dplyr::bind_rows(lapply(out, `[[`, "facts")), dplyr::bind_rows(lapply(out, `[[`, "ledger")),
                  dplyr::bind_rows(
                    sector_excl("production_depth_momentum_tbl", "production_latest", "mineral Extraction / Refining roll-up (not a quantity of one mineral)", sum(roll)),
                    sector_excl("production_depth_momentum_tbl", "production_latest (fuels)",
                                "NA: neither EI nor EIA reports a comparable value (shipped as nsr, NaN)", sum(is.na(f$value))))),
    list(notes = notes))
}

# critical_minerals_production_tbl: EI mine production of manganese, zinc and PGMs (2025). Zeros
# are the roster completion (nsr). World from major_supplier_shares_tbl (value / world share of the
# largest producer). The other EI minerals are covered by IEA and USGS and are not shipped.
sector_facts_mining_ei <- function(cmp, mss, minerals = c("Manganese", "Zinc", "PGMs"), year = 2025L) {
  r <- sector_raw_rows(cmp)
  r$tech_name <- sub(" Production$", "", r$variable)
  sel <- r$tech_name %in% minerals
  m <- r[sel, , drop = FALSE]
  sector_check_iso3(m$iso3c, "critical_minerals_production_tbl")
  x <- sector_fx("production.mining_ei", paste0("mineral:", sector_mineral_slug(m$tech_name)), m$iso3c, year, m$value,
                 ifelse(m$value > 0, "obs", "nsr"), "critical_minerals_production_tbl", m$variable, key = m$iso3c)
  s <- sector_supplier_dedupe(mss, "ei_mineral_mining")
  s <- s[s$mineral %in% minerals & s$world_share > 0, , drop = FALSE]
  s <- s[order(s$mineral, -s$value), , drop = FALSE]
  s <- s[!duplicated(s$mineral), , drop = FALSE]
  world <- tibble::tibble(var = "production.mining_ei", item_key = paste0("mineral:", sector_mineral_slug(s$mineral)), year = year,
                          value = s$value / s$world_share, basis = "source_world")
  other <- !sel & r$tech_name != "Nuclear"
  ex <- dplyr::bind_rows(
    sector_excl("critical_minerals_production_tbl", "<Mineral> Production", "minerals covered by IEA and USGS", sum(other)),
    sector_excl("critical_minerals_production_tbl", "Nuclear Production", "duplicate of production_depth_momentum_tbl (uranium)", sum(r$tech_name == "Nuclear"))
  )
  c(sector_result(x$facts, x$ledger, ex), list(world = world))
}

# ---- Consumption -------------------------------------------------------------------------------------

# energy_access_tbl: consumption per person and its 2020-2025 change (ship once; the EI rows of
# energy_consumption_tbl must be identical). ei_present as for production (<fuel>_tes_ej).
sector_facts_consumption <- function(ea, ec, ei_present, year = 2025L) {
  r <- sector_raw_rows(ea)
  k <- paste(r$iso3c, r$tech, r$variable)
  e <- sector_raw_rows(ec)
  e <- e[e$variable %in% unique(r$variable), , drop = FALSE]
  ke <- paste(e$iso3c, e$tech, e$variable)
  if (!setequal(k, ke) || !identical(e$value[match(k, ke)], r$value)) {
    stop("energy_consumption_tbl EI rows differ from energy_access_tbl: ship-once rule broken")
  }
  sector_check_iso3(r$iso3c, "energy_access_tbl")
  pres <- paste(ei_present$ei_var, ei_present$iso3)
  pc <- r[r$variable == "Energy consumption per capita", , drop = FALSE]
  present <- paste(paste0(tolower(pc$tech), "_tes_ej"), pc$iso3c) %in% pres
  st <- ifelse(pc$value > 0, "obs", ifelse(present, "zero", "nsr"))
  a <- sector_fx("consumption.per_capita", paste0("fuel:", tolower(pc$tech)), pc$iso3c, year, pc$value, st,
                 "energy_access_tbl", "Energy consumption per capita", key = paste(pc$iso3c, pc$tech))
  g <- r[r$variable == "Energy consumption per capita growth", , drop = FALSE]
  bad <- !is.finite(g$value)
  b <- sector_fx("consumption.pc_change", paste0("fuel:", tolower(g$tech)), g$iso3c, year, ifelse(bad, NaN, g$value),
                 ifelse(bad, "na_growth", "obs"), "energy_access_tbl", "Energy consumption per capita growth",
                 key = paste(g$iso3c, g$tech))
  sector_result(dplyr::bind_rows(a$facts, b$facts), dplyr::bind_rows(a$ledger, b$ledger),
                dplyr::bind_rows(sector_excl("energy_access_tbl", "Energy consumption per capita growth", "Inf or NaN (shipped as na_growth)", sum(bad)),
                                 sector_excl("energy_consumption_tbl", "Energy consumption per capita (+ growth)", "duplicate of energy_access_tbl", nrow(e))))
}

# import_dependence_tbl: net balance (EJ) and self-sufficiency (percent of consumption). Where the
# production of a fuel is reported (an EI row in ei_present, or a value the pipeline took from EIA:
# the row's source names EIA) both ship verbatim (obs / zero; notes eia_production where the
# production, and eia_consumption where the consumption, is EIA's). Where neither EI nor EIA
# reports it (inside an EI "Other" residual; Estonia coal), production is ASSUMED TO BE 0 and the
# fact says so (user rule, 2026-10-11): status nsr ("not reported separately") with the value that
# assumption gives, the balance minus consumption and self-sufficiency 0%, note
# production_assumed_zero (+ ei_oil_shale where EIA's value failed the comparability check). A
# table built before the EIA fill holds that value already (its production is a zero-fill: shipped
# verbatim, P); a table built with it holds NA there, and the value is derived (D) from EI's
# consumption (`consumption`: sector_ei_consumption(); left NaN when EI has no consumption row).
# Fossil sums the fuels: nsr when none of oil, gas and coal has a production value, else obs with
# the note fossil_assumed_zero where one of them counts as 0. Self-sufficiency NA because the
# economy consumes none of the fuel is not shipped. ei_present = NULL ships every value as reported.
sector_facts_balance <- function(idp, ei_present = NULL, year = 2025L, eia_cells = NULL, consumption = NULL) {
  r <- sector_raw_rows(idp)
  sector_check_iso3(r$iso3c, "import_dependence_tbl")
  r <- r[r$variable %in% c("Production surplus/deficit", "Self-sufficiency"), , drop = FALSE]
  if (!nrow(r)) return(c(sector_result(), list(notes = sector_notes_empty())))
  fuels <- names(SECTOR_EI_PRODUCTION_VARS)
  # Production source per (economy, fuel): ei, eia or none (neither reports it).
  bal <- r[r$variable == "Production surplus/deficit", , drop = FALSE]
  cites <- sector_cites_eia(bal)
  src_of <- function(iso3, tech) {
    if (is.null(ei_present)) return(rep("ei", length(iso3)))
    pres <- paste(ei_present$ei_var, ei_present$iso3)
    ei <- paste(SECTOR_EI_PRODUCTION_VARS[tech], iso3) %in% pres
    k <- match(paste(iso3, tech), paste(bal$iso3c, bal$tech))
    eia <- !ei & !is.na(k) & cites[k] & !is.na(bal$value[k])
    ifelse(ei, "ei", ifelse(eia, "eia", "none"))
  }
  nc_of <- function(iso3, tech) {
    if (is.null(eia_cells)) return(rep(FALSE, length(iso3)))
    paste(iso3, SECTOR_EI_PRODUCTION_VARS[tech]) %in%
      paste(eia_cells$iso3, eia_cells$ei_var)[eia_cells$eia_basis %in% "not_comparable"]
  }
  fuel <- r$tech %in% fuels
  fossil <- r$tech == "Fossil"
  src <- matrix(vapply(fuels, function(f) src_of(r$iso3c, rep(f, nrow(r))), character(nrow(r))), nrow = nrow(r))
  colnames(src) <- fuels
  nc <- matrix(vapply(fuels, function(f) nc_of(r$iso3c, rep(f, nrow(r))), logical(nrow(r))), nrow = nrow(r))
  own <- cbind(seq_len(nrow(r)), match(r$tech, fuels))
  own_src <- ifelse(fuel, src[cbind(own[, 1], ifelse(fuel, own[, 2], 1L))], NA_character_)
  none <- (fuel & own_src == "none") | (fossil & rowSums(src == "none") == length(fuels))
  partial <- fossil & !none & rowSums(src == "none") > 0
  eia_prod <- (fuel & own_src == "eia") | (fossil & rowSums(src == "eia") > 0)
  shale <- ifelse(fuel, nc[cbind(own[, 1], ifelse(fuel, own[, 2], 1L))], fossil & rowSums(nc & src == "none") > 0)
  eia_cons <- if (is.null(ei_present)) rep(FALSE, nrow(r)) else {
    !paste("oil_tes_ej", r$iso3c) %in% paste(ei_present$ei_var, ei_present$iso3) & sector_cites_eia(r)
  }
  # The assumed-zero value where the table holds NA (its balance of the economy and fuel is NA:
  # production unreported): minus consumption / 0%.
  cons <- if (is.null(consumption)) rep(NA_real_, nrow(r)) else consumption$value[match(paste(r$iso3c, r$tech), paste(consumption$iso3, consumption$tech))]
  is_bal <- r$variable == "Production surplus/deficit"
  kb <- match(paste(r$iso3c, r$tech), paste(bal$iso3c, bal$tech))
  prod_na <- !is.na(kb) & is.na(bal$value[kb])
  derive <- none & prod_na & !is.na(cons) & (is_bal | cons > 0)
  value <- ifelse(derive, ifelse(is_bal, -cons, 0), r$value)
  # Not shipped: self-sufficiency of an economy that consumes none of the fuel (NA beside a balance,
  # or an assumed-zero production with zero consumption). An assumed-zero value without a
  # consumption figure ships as nsr NaN.
  no_cons <- !is_bal & is.na(value) & (!prod_na | (!is.na(cons) & cons <= 0))
  keep <- !no_cons
  na_none <- keep & is.na(value) & none
  na_other <- keep & is.na(value) & !none
  status <- ifelse(none, "nsr", ifelse(value == 0, "zero", "obs"))
  status[is.na(value)] <- "nsr"
  var <- ifelse(is_bal, "consumption.balance", "consumption.self_sufficiency")
  x <- sector_fx(var[keep], paste0("fuel:", tolower(r$tech[keep])), r$iso3c[keep], year,
                 ifelse(is.na(value[keep]), NaN, value[keep]), status[keep], "import_dependence_tbl", r$variable[keep],
                 prov = ifelse(derive[keep], "D", "P"), formula_value = ifelse(derive[keep], value[keep], NA_real_),
                 key = paste(r$iso3c[keep], r$tech[keep]))
  fx <- x$facts
  notes <- dplyr::bind_rows(
    sector_fact_notes(fx, ifelse(none[keep], "production_assumed_zero", NA_character_)),
    sector_fact_notes(fx, ifelse(partial[keep], "fossil_assumed_zero", NA_character_)),
    sector_fact_notes(fx, ifelse(shale[keep] & (none[keep] | partial[keep]), "ei_oil_shale", NA_character_)),
    sector_fact_notes(fx, ifelse(eia_prod[keep], "eia_production", NA_character_)),
    sector_fact_notes(fx, ifelse(eia_cons[keep], "eia_consumption", NA_character_))
  )
  n_by <- function(sel, v) sum(sel & r$variable == v)
  ex <- list()
  for (v in c("Production surplus/deficit", "Self-sufficiency")) {
    what <- if (v == "Production surplus/deficit") "minus consumption" else "0%"
    ex[[length(ex) + 1]] <- sector_excl("import_dependence_tbl", v,
                                        paste0("production not reported separately: assumed 0, shipped as nsr (", what, "; verbatim zero-fill)"),
                                        n_by(keep & none & !derive & !is.na(value), v))
    ex[[length(ex) + 1]] <- sector_excl("import_dependence_tbl", v,
                                        paste0("production not reported separately: assumed 0, shipped as nsr (", what, "; D from EI consumption)"),
                                        n_by(derive, v))
    ex[[length(ex) + 1]] <- sector_excl("import_dependence_tbl", v,
                                        "production not reported separately and no EI consumption figure (shipped as nsr, NaN)", n_by(na_none, v))
    ex[[length(ex) + 1]] <- sector_excl("import_dependence_tbl", v, "NA although production is reported (shipped as nsr, NaN)", n_by(na_other, v))
  }
  ex[[length(ex) + 1]] <- sector_excl("import_dependence_tbl", "Self-sufficiency", "NA (no consumption)", sum(no_cons))
  c(sector_result(fx, x$ledger, dplyr::bind_rows(ex)), list(notes = notes))
}

# ---- Manufacturing and EV regions --------------------------------------------------------------------

# IEA ETP technologies -> tech slugs; the stage is the row's (the IEA files battery capacity as
# Components, solar, wind and electrolyser capacity as Assembly).
SECTOR_ETP_TECH <- c(Solar = "solar", Wind = "wind", Batteries = "batteries", Electrolyzers = "green-hydrogen", `Green Hydrogen` = "green-hydrogen")

# The per-fact note of a value the source gives once for a technology's manufacturing (Components and
# Assembly carry the same figure: manufacturing capex, the IEA ETP capacity shares; user decision C2,
# 2026-10-28): shown on both cells, never added across them (headline sums count it once,
# sector_count_once()).
SECTOR_PAIR_NOTE <- "same_figure_pair"

# Rows of `rows` (columns tech_col, stage_col, by) at one manufacturing stage only, copied onto the other
# (the pipeline's rule, supply_chain_fill_manufacturing_pair()); a pair present at both stages must hold
# the same value (stops otherwise). Returns rows with `pair` TRUE on every row of a Components /
# Assembly pair.
sector_pair_fill <- function(rows, by, value_col = "value", stage_col = "supply_chain", label = "manufacturing pair") {
  pair <- SUPPLY_CHAIN_MANUFACTURING_STAGES
  rows$pair <- rows[[stage_col]] %in% pair
  if (!any(rows$pair)) return(rows)
  key <- do.call(paste, c(unname(as.list(rows[by])), sep = "\r"))
  p <- rows$pair
  n_st <- tapply(rows[[stage_col]][p], key[p], function(s) length(unique(s)))
  both <- key %in% names(n_st)[n_st == 2]
  if (any(both & p)) {
    v <- tapply(rows[[value_col]][both & p], key[both & p], function(x) length(unique(x)))
    if (any(v > 1)) stop("sector_pair_fill(", label, "): Components and Assembly differ for ", sum(v > 1), " figure(s), e.g. ",
                         gsub("\r", " / ", names(v)[v > 1][1]))
  }
  lone <- p & !both
  if (any(lone)) {
    add <- rows[lone, , drop = FALSE]
    add[[stage_col]] <- ifelse(add[[stage_col]] == pair[1], pair[2], pair[1])
    rows <- dplyr::bind_rows(rows, add)
  }
  rows
}

sector_etp_entity <- function(iso3, eu_members) {
  ifelse(iso3 %in% c(eu_members, "EUU"), "E:iea_etp:eu",
         ifelse(iso3 == "CHN", "E:iea_etp:china", ifelse(iso3 == "USA", "E:iea_etp:us",
         ifelse(iso3 == "IND", "E:iea_etp:india", ifelse(iso3 == "VNM", "E:iea_etp:viet-nam", NA_character_)))))
}

# IEA ETP shares: current (major_supplier_shares_tbl iea_manufacturing_capacity world_share) and
# announced 2035 (foreign_dependency_tbl Market Share, EU members carry the EU value). A 0 means
# "under 1% or not reported" (status nq, value kept). The IEA reports one share per technology's
# manufacturing: the pipeline files it at one manufacturing stage and copies it onto the other
# (supply_chain_fill_manufacturing_pair(): foreign_dependency_tbl holds the copies, explanation
# "; copied from"; major_supplier_shares_tbl only the filed rows, which sector_pair_fill() copies the
# same way). Both cells ship the figure, each with the note same_figure_pair.
sector_facts_mfg <- function(fd, mss, eu_members) {
  s <- mss[mss$measure == "iea_manufacturing_capacity", , drop = FALSE]
  ent <- sector_etp_entity(s$iso3, eu_members)
  if (anyNA(ent)) stop("IEA manufacturing capacity: unknown entities ", paste(unique(s$iso3[is.na(ent)]), collapse = ", "))
  tslug <- SECTOR_ETP_TECH[s$tech]
  if (anyNA(tslug)) stop("IEA manufacturing capacity: unknown techs ", paste(unique(s$tech[is.na(tslug)]), collapse = ", "))
  if (!all(s$supply_chain %in% SUPPLY_CHAIN_MANUFACTURING_STAGES)) stop("IEA manufacturing capacity filed outside Components and Assembly")
  s <- tibble::tibble(iso3 = s$iso3, entity = ent, tslug = unname(tslug), tech = s$tech, supply_chain = as.character(s$supply_chain),
                      year = as.integer(s$year), value = s$world_share)
  s <- sector_pair_fill(s, c("entity", "tslug", "year"), label = "IEA ETP current shares")
  a <- sector_fx("manufacturing.share_now", paste0("cell:", s$tslug, "-", tolower(s$supply_chain)), s$entity, s$year, s$value,
                 ifelse(s$value == 0, "nq", "obs"), "major_supplier_shares_tbl", "iea_manufacturing_capacity",
                 key = paste(s$iso3, s$tech, s$supply_chain))
  na <- sector_fact_notes(a$facts, ifelse(s$pair, SECTOR_PAIR_NOTE, NA_character_))
  r <- sector_raw_rows(fd)
  r <- r[r$variable == "Market Share", , drop = FALSE]
  heat <- !r$tech %in% names(SECTOR_ETP_TECH)
  r <- r[!heat, , drop = FALSE]
  if (!all(r$supply_chain %in% SUPPLY_CHAIN_MANUFACTURING_STAGES)) stop("IEA 2035 shares filed outside Components and Assembly")
  r$entity <- sector_etp_entity(r$iso3c, eu_members)
  if (anyNA(r$entity)) stop("IEA 2035 shares: unknown economies ", paste(unique(r$iso3c[is.na(r$entity)]), collapse = ", "))
  r$copy <- grepl("; copied from ", as.character(r$explanation), fixed = TRUE)
  d <- sector_debroadcast(r[c("entity", "tech", "supply_chain", "value", "iso3c", "copy")], c("entity", "tech", "supply_chain"), "IEA 2035 shares (EU members)")
  d$tslug <- unname(SECTOR_ETP_TECH[d$tech])
  n_eu_copies <- nrow(r) - nrow(d)
  d <- sector_pair_fill(d, c("entity", "tslug"), label = "IEA ETP 2035 shares")
  b <- sector_fx("manufacturing.share_2035", paste0("cell:", d$tslug, "-", tolower(d$supply_chain)), d$entity, 2035L, d$value,
                 ifelse(d$value == 0, "nq", "obs"), "foreign_dependency_tbl", "Market Share", key = paste(d$entity, d$tech, d$supply_chain))
  nb <- sector_fact_notes(b$facts, ifelse(d$pair, SECTOR_PAIR_NOTE, NA_character_))
  world <- unique(dplyr::bind_rows(a$facts, b$facts)[c("var", "item_key", "year")])
  world$value <- 1
  world$basis <- "one"
  c(sector_result(dplyr::bind_rows(a$facts, b$facts), dplyr::bind_rows(a$ledger, b$ledger),
                  dplyr::bind_rows(sector_excl("foreign_dependency_tbl", "Market Share", "Heat Pumps (no cell)", sum(heat)),
                                   sector_excl("foreign_dependency_tbl", "Market Share", "EU member copies of the EU value", n_eu_copies))),
    list(world = world, notes = dplyr::bind_rows(na, nb)))
}

# The IEA EV region measures and the cell each is filed at in market_share_manufacturing_tbl:
# production and the net-export ratio at Assembly (vehicle plants), sales at Deployment (the series
# of sector_iea_ev_series() follow the same cells; scripts/87 checks facts and series agree).
SECTOR_EV_REGION_CELLS <- c(Domestic.Production = "electric-vehicles-assembly", net_export_ratio = "electric-vehicles-assembly",
                            Domestic.sales = "electric-vehicles-deployment")

# IEA EV regions (market_share_manufacturing_tbl): production, sales and the net-export ratio, one
# value per region (the region from the row's explanation; copies asserted identical), on the cell
# the processed table files each at (SECTOR_EV_REGION_CELLS; any other stops).
sector_facts_ev_regions <- function(msm, year = 2024L) {
  r <- sector_raw_rows(msm)
  r <- r[r$tech == "Electric Vehicles" & r$variable %in% c("Domestic.Production", "Domestic.sales", "net_export_ratio"), , drop = FALSE]
  r$region <- sub("^.*IEA region: ([^(]+?) \\(.*$", "\\1", r$explanation)
  bad <- !r$region %in% names(SECTOR_EV_REGIONS)
  if (any(bad)) stop("EV regions: unknown region text ", paste(utils::head(unique(r$region[bad]), 3), collapse = "; "))
  r$cell <- sector_cell_slug(r$tech, r$supply_chain)
  off <- r$cell != SECTOR_EV_REGION_CELLS[r$variable]
  if (any(off)) stop("EV regions: market_share_manufacturing_tbl files ", paste(unique(paste(r$variable[off], r$cell[off])), collapse = ", "),
                     "; SECTOR_EV_REGION_CELLS expects ", paste(names(SECTOR_EV_REGION_CELLS), SECTOR_EV_REGION_CELLS, collapse = ", "))
  d <- sector_debroadcast(r[c("region", "variable", "cell", "value")], c("region", "variable"), "IEA EV regions")
  vmap <- c(Domestic.Production = "production.ev_region", Domestic.sales = "production.ev_region_sales",
            net_export_ratio = "manufacturing.ev_net_export_ratio")
  x <- sector_fx(unname(vmap[d$variable]), paste0("cell:", d$cell), paste0("R:iea_ev:", SECTOR_EV_REGIONS[d$region]),
                 year, d$value, ifelse(d$value == 0, "zero", "obs"), "market_share_manufacturing_tbl", d$variable, key = d$region)
  region_of <- stats::setNames(r$region, r$iso3c)
  region_of <- region_of[!duplicated(names(region_of))]
  c(sector_result(x$facts, x$ledger, sector_excl("market_share_manufacturing_tbl", "EV regional values", "copies of the region's value", nrow(r) - nrow(d))),
    list(region_of = region_of))
}

# ---- Investment -------------------------------------------------------------------------------------

# investment_momentum_tbl annual investment, 2018-2025. 2025 includes estimated Q4 construction
# starts for wind, solar and critical minerals (CIM README): those rows are `est`; the
# critical-minerals capex is the Refining cells of Batteries and Electric Vehicles (the monitor's
# processing plants, split between the two by their demand shares: per-fact note cim_minerals_split).
# Manufacturing capex has no product detail, so the pipeline replicates it onto Components and
# Assembly (its explanation says so): both cells ship it with the note same_figure_pair, and
# investment@1 counts it once (sector_count_once()). A pair whose two values differ stops.
sector_facts_investment <- function(im, est_year = 2025L) {
  r <- sector_raw_rows(im)
  a <- r[r$variable == "Annual Investment (USD bn, 2024$)", , drop = FALSE]
  sector_check_iso3(a$iso3c, "investment_momentum_tbl")
  est <- a$Year == est_year & (a$tech %in% c("Solar", "Wind") | a$supply_chain == "Refining")
  x <- sector_fx("investment.annual", paste0("cell:", sector_cell_slug(a$tech, a$supply_chain)), a$iso3c, as.integer(a$Year), a$value,
                 ifelse(est, "est", ifelse(a$value == 0, "zero", "obs")), "investment_momentum_tbl", "Annual Investment (USD bn, 2024$)",
                 key = paste(a$iso3c, a$tech, a$supply_chain, a$Year))
  ex <- as.character(a$explanation)
  pair <- a$supply_chain %in% SUPPLY_CHAIN_MANUFACTURING_STAGES & grepl("replicated onto Components and Assembly", ex, fixed = TRUE)
  if (any(pair)) {
    k <- paste(a$iso3c, a$tech, a$Year)[pair]
    n_v <- tapply(a$value[pair], k, function(v) length(unique(v)))
    n_s <- tapply(a$supply_chain[pair], k, function(s) length(unique(s)))
    if (any(n_v > 1) || any(n_s != 2)) stop("investment_momentum_tbl: replicated manufacturing capex differs between Components and Assembly, or one stage is missing (",
                                            paste(utils::head(names(n_v)[n_v > 1 | n_s != 2], 3), collapse = "; "), ")")
  }
  split <- a$supply_chain == "Refining" & grepl("split between Batteries and Electric Vehicles", ex, fixed = TRUE)
  notes <- dplyr::bind_rows(sector_fact_notes(x$facts, ifelse(pair, SECTOR_PAIR_NOTE, NA_character_)),
                            sector_fact_notes(x$facts, ifelse(split, "cim_minerals_split", NA_character_)))
  other <- r$variable != "Annual Investment (USD bn, 2024$)"
  c(sector_result(x$facts, x$ledger, sector_excl("investment_momentum_tbl", "momentum and raw stage sums", "a transform / mixed units", sum(other))),
    list(notes = notes))
}

# The facts a headline sum may add: every row except the Assembly copy of a figure the source gives once
# for Components and Assembly (note same_figure_pair), so it is counted once. notes: facts.notes rows.
sector_count_once <- function(facts, notes) {
  if (is.null(notes) || !nrow(notes) || !nrow(facts)) return(facts)
  n <- notes[notes$note == SECTOR_PAIR_NOTE, , drop = FALSE]
  copy <- paste(facts$var, facts$item_key, facts$geo, facts$year) %in% paste(n$var, n$item_key, n$geo, n$year) &
    endsWith(facts$item_key, "-assembly")
  facts[!copy, , drop = FALSE]
}

# The slices (var, item_key, year) that are wholly the Assembly copy of a same_figure_pair figure: every
# fact of the slice is dropped by sector_count_once(). A headline count of measures counts such a slice
# once, at Components. Returns paste(var, item_key, year, sep = "\r") keys.
sector_pair_copy_slices <- function(facts, notes) {
  if (is.null(notes) || !nrow(notes) || !nrow(facts)) return(character())
  key <- function(f) paste(f$var, f$item_key, f$year, sep = "\r")
  kept <- key(sector_count_once(facts, notes))
  setdiff(unique(key(facts)), kept)
}

# ---- Future demand: WEO world supply, IEA GEVO EV cars ---------------------------------------------

SECTOR_WEO_FUEL <- c(Coal = "coal", Gas = "gas", `Green Hydrogen` = "hydrogen", Nuclear = "nuclear", Oil = "oil", Solar = "solar", Wind = "wind")

sector_facts_future <- function(fdm, ev_min_base = 1000) {
  r <- sector_raw_rows(fdm)
  weo <- r[r$source == "IEA World Energy Outlook" & r$variable %in% c("2035", "global_growth"), , drop = FALSE]
  d <- sector_debroadcast(weo[c("tech", "variable", "value")], c("tech", "variable"), "WEO world values")
  v35 <- d[d$variable == "2035", , drop = FALSE]
  gr <- d[d$variable == "global_growth", , drop = FALSE]
  g <- gr$value[match(v35$tech, gr$tech)]
  fuel <- paste0("fuel:", SECTOR_WEO_FUEL[v35$tech])
  a <- sector_fx("consumption.weo", fuel, "WLD", 2035L, v35$value, "obs", "future_demand_tbl", "2035", key = v35$tech)
  v24 <- v35$value - g
  b <- sector_fx("consumption.weo", fuel, "WLD", 2024L, v24, "obs", "future_demand_tbl", "2035 - global_growth", prov = "D",
                 formula_value = v24, key = v35$tech)
  ev <- r[r$source == "IEA Global EV Outlook 2026" & r$tech == "Electric Vehicles", , drop = FALSE]
  sector_check_iso3(ev$iso3c, "future_demand_tbl GEVO")
  # Car sales and stock: the Deployment cell (as the processed table files them).
  if (length(setdiff(unique(ev$supply_chain), "Deployment"))) stop("future_demand_tbl GEVO rows filed outside Electric Vehicles Deployment")
  cell <- "cell:electric-vehicles-deployment"
  pick <- function(vn) ev[ev$variable == vn & !is.na(ev$value), , drop = FALSE]
  lvl <- list()
  for (spec in list(c("sales_2025", "consumption.ev_sales", 2025), c("sales_2035", "consumption.ev_sales", 2035),
                    c("stock_2025", "consumption.ev_stock", 2025), c("stock_2035", "consumption.ev_stock", 2035),
                    c("share_2025", "consumption.ev_sales_share", 2025), c("share_2035", "consumption.ev_sales_share", 2035))) {
    p <- pick(spec[1])
    lvl[[length(lvl) + 1]] <- sector_fx(spec[2], cell, p$iso3c, as.integer(spec[3]), p$value, ifelse(p$value == 0, "zero", "obs"),
                                        "future_demand_tbl", spec[1], key = p$iso3c)
  }
  # Growth 2022-2025 only where the 2022 level (level_2025 / (1 + growth)) is at least ev_min_base.
  grow <- list()
  n_small <- 0L
  for (spec in list(c("sales_growth_2225", "sales_2025", "consumption.ev_sales_growth"),
                    c("stock_growth_2225", "stock_2025", "consumption.ev_stock_growth"))) {
    p <- pick(spec[1])
    lv <- ev$value[ev$variable == spec[2]][match(p$iso3c, ev$iso3c[ev$variable == spec[2]])]
    base <- lv / (1 + p$value)
    small <- is.na(base) | !is.finite(p$value) | base < ev_min_base
    n_small <- n_small + sum(small)
    grow[[length(grow) + 1]] <- sector_fx(spec[3], cell, p$iso3c, 2025L, ifelse(small, NaN, p$value), ifelse(small, "na_growth", "obs"),
                                          "future_demand_tbl", spec[1], key = p$iso3c)
  }
  all <- c(list(a, b), lvl, grow)
  other <- r$source %in% c("BNEF New Energy Outlook", "BCG") | (r$source == "IEA Global EV Outlook 2026" & grepl("forecast_growth|share_growth", r$variable))
  sector_result(dplyr::bind_rows(lapply(all, `[[`, "facts")), dplyr::bind_rows(lapply(all, `[[`, "ledger")),
                dplyr::bind_rows(sector_excl("future_demand_tbl", "BNEF / BCG", "licensed (never shipped)", sum(r$source %in% c("BNEF New Energy Outlook", "BCG"))),
                                 sector_excl("future_demand_tbl", "GEVO growth", "2022 base below the floor (shipped as na_growth)", n_small),
                                 sector_excl("future_demand_tbl", "GEVO forecast and share growth", "not in the measure catalogue",
                                             sum(r$source == "IEA Global EV Outlook 2026" & grepl("forecast_growth|share_growth", r$variable))),
                                 sector_excl("future_demand_tbl", "WEO", "copies of the world value", nrow(weo) - nrow(d))))
}

# ---- Prices, costs, TRL, alignment ----------------------------------------------------------------

sector_facts_prices <- function(ep, commodities, year = 2025L) {
  r <- sector_raw_rows(ep)
  vmap <- c(latest_price = "prices.latest", yoy_price_change_pct = "prices.yoy", price_volatility_1yr = "prices.vol_1y",
            price_volatility_5yr = "prices.vol_5y", price_volatility_10yr = "prices.vol_10y", price_volatility_20yr = "prices.vol_20y")
  r <- r[r$variable %in% names(vmap), , drop = FALSE]
  d <- sector_debroadcast(r[c("sub_sector", "variable", "value")], c("sub_sector", "variable"), "IMF prices (countries x techs)")
  d <- d[!is.na(d$value), , drop = FALSE]
  slug <- commodities$slug[match(d$sub_sector, commodities$sub_sector)]
  x <- sector_fx(unname(vmap[d$variable]), paste0("commodity:", slug), "WLD", year, d$value, "obs", "energy_prices_tbl", d$variable,
                 key = d$sub_sector)
  sector_result(x$facts, x$ledger, dplyr::bind_rows(
    sector_excl("energy_prices_tbl", "*", "copies of world values (countries x techs)", nrow(r) - nrow(d)),
    sector_excl("energy_prices_tbl", "mean_price_volatility", "a mean of the four windows (not shipped)", sum(sector_raw_rows(ep)$variable == "mean_price_volatility"))
  ))
}

# cost_competitiveness_tbl: wages per (economy, ILO activity), the lending rate and PPI y/y per
# economy; copies across technologies and stages collapse (the six stages read three ILO activities:
# SECTOR_ACTIVITY_OF_STAGE; the stages of one activity must agree); obs_year = the observation year.
sector_facts_costs <- function(cc, nominal_year = 2025L) {
  r <- sector_raw_rows(cc)
  sector_check_iso3(r$iso3c[!is.na(r$value)], "cost_competitiveness_tbl")
  w <- r[r$variable == "earnings_usd" & !is.na(r$value), c("iso3c", "supply_chain", "value", "Year")]
  w$activity <- unname(SECTOR_ACTIVITY_OF_STAGE[as.character(w$supply_chain)])
  if (anyNA(w$activity)) stop("cost_competitiveness_tbl earnings_usd: stages without an ILO activity: ", paste(unique(w$supply_chain[is.na(w$activity)]), collapse = ", "))
  w <- sector_debroadcast(w, c("iso3c", "activity"), "wages (techs and the stages of one activity)", c("value", "Year"))
  a <- sector_fx("prices.wages", paste0("activity:", w$activity), w$iso3c, nominal_year, w$value, "obs",
                 "cost_competitiveness_tbl", "earnings_usd", obs_year = as.integer(w$Year), key = paste(w$iso3c, w$activity))
  one <- function(vn, var) {
    x <- r[r$variable == vn & !is.na(r$value), c("iso3c", "value", "Year")]
    x <- sector_debroadcast(x, "iso3c", paste(vn, "(techs x stages)"), c("value", "Year"))
    sector_fx(var, "none:none", x$iso3c, nominal_year, x$value, ifelse(x$value == 0, "zero", "obs"), "cost_competitiveness_tbl", vn,
              obs_year = as.integer(x$Year), key = x$iso3c)
  }
  b <- one("nominal_rate", "prices.lending_rate")
  c3 <- one("ppi_yoy", "prices.ppi_yoy")
  sector_result(dplyr::bind_rows(a$facts, b$facts, c3$facts), dplyr::bind_rows(a$ledger, b$ledger, c3$ledger),
                dplyr::bind_rows(sector_excl("cost_competitiveness_tbl", "ppi", "PPI levels are not comparable across economies", sum(r$variable == "ppi")),
                                 sector_excl("cost_competitiveness_tbl", "earnings_usd, nominal_rate, ppi_yoy", "copies across technologies and stages",
                                             sum(r$variable %in% c("earnings_usd", "nominal_rate", "ppi_yoy") & !is.na(r$value)) - nrow(a$facts) - nrow(b$facts) - nrow(c3$facts))))
}

# technological_readiness_tbl: one value per cell (copies across economies collapse); fallback
# cells (the technology-wide value) from the explanation text.
sector_facts_trl <- function(tr, year = 2025L) {
  r <- sector_raw_rows(tr)
  vmap <- c("TRL 2025" = "technology.trl")
  dvar <- unique(r$variable[grepl("^TRL .*2020", r$variable)])
  if (length(dvar) != 1) stop("TRL change variable not found")
  vmap[dvar] <- "technology.trl_change"
  r <- r[r$variable %in% names(vmap), , drop = FALSE]
  d <- sector_debroadcast(r[c("tech", "supply_chain", "variable", "value", "explanation")], c("tech", "supply_chain", "variable"), "TRL (economies)")
  x <- sector_fx(unname(vmap[d$variable]), paste0("cell:", sector_cell_slug(d$tech, d$supply_chain)), "WLD", year, d$value, "obs",
                 "technological_readiness_tbl", d$variable, key = paste(d$tech, d$supply_chain))
  fb <- d[d$variable == "TRL 2025" & !grepl("Stage-specific", d$explanation), , drop = FALSE]
  fb <- fb[order(match(fb$tech, SECTOR_TECH_LABELS), match(fb$supply_chain, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
  c(sector_result(x$facts, x$ledger, sector_excl("technological_readiness_tbl", "*", "copies across economies", nrow(r) - nrow(d))),
    list(fallback = paste(fb$tech, fb$supply_chain)))
}

sector_facts_alignment <- function(ideal, year = 2023L) {
  x <- ideal[!is.na(ideal$s_axis) & grepl("^[A-Z]{3}$", ideal$iso3), , drop = FALSE]
  f <- sector_fx("development.alignment", "none:none", x$iso3, year, x$s_axis, "obs", "geo_ideal_points_tbl", "s_axis", key = x$iso3)
  sector_result(f$facts, f$ledger, sector_excl("geo_ideal_points_tbl", "s_axis", "NA (no votes, no sovereign)", sum(is.na(ideal$s_axis))))
}
