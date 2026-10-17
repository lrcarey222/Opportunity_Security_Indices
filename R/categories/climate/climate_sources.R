# Climate sources (user request of 2026-10-17: seven files added to data/raw for the Climate section):
# the Global Carbon Project's national fossil CO2 (territorial, consumption-based, transfers) and World
# fossil CO2 by fuel, the Energy Institute's Country Transition Tracker, the IEA's WEO 2025 regional free
# dataset, its Net Zero by 2050 (2021) Annex A and its World Energy Investment 2026 data file, parsed and
# mapped to economies, WEO regions and technology x stage (config/climate_sources_map.yml). The IEA
# Energy and Emissions per Value Added database is licence-restricted and is not read here.
#
# Pure (no IO). scripts/18_build_climate_sources.R reads the files (workbook sheets as data frames read
# with col_names = FALSE, CSVs as published) and the map, and writes data/processed/climate_src_*_tbl.rds
# with the functions below; scripts/87 ships them in the lazy sector-climate-src chunk.
# Every parser stops when a sheet, title, unit, scenario, region or row label it relies on has changed.

CLIMATE_SRC_ROLES <- c("own", "demand", "context")
CLIMATE_SRC_MATCH <- c("direct", "partial", "proxy", "none")
CLIMATE_SRC_GCP_MEASURES <- c("territorial", "consumption", "transfers")
CLIMATE_SRC_WEO_SCENARIOS <- c("historical", "stated", "current", "nze")
CLIMATE_SRC_REGION_KINDS <- c("country", "union", "region", "world")
CLIMATE_SRC_GEO_MATCH <- c("own", "member", "by_subregion")
CLIMATE_SRC_WEI_KINDS <- c("world", "group", "country", "region")
CLIMATE_SRC_BETTER <- c("lower", "higher", "none")

# The text of one cell of a sheet read with col_names = FALSE ("" for NA). Columns may be vectors or the
# list columns readxl returns with col_types = "list" (each cell in its own type, numbers unrounded).
.cs_txt <- function(d, i, j) {
  if (i < 1L || i > nrow(d) || j > ncol(d)) return("")
  v <- d[[j]][[i]]
  if (is.null(v) || length(v) != 1L || is.na(v)) "" else trimws(as.character(v))
}

# A column (vector or list) as doubles; text that is not a number and empty cells are NA.
.cs_num <- function(x) {
  if (!is.list(x)) return(suppressWarnings(as.double(x)))
  vapply(x, function(v) if (is.null(v) || length(v) != 1L || is.na(v)) NA_real_ else suppressWarnings(as.double(v)), double(1))
}

# ---- Map ------------------------------------------------------------------------------------------------

# Every problem of the parsed config/climate_sources_map.yml (empty when valid). cells: the cell slugs the
# map must list (sector dictionary `cells`). weo_series: the WEO 2025 World series ids of
# config/iea_pathways_map.yml (NULL skips that check).
climate_sources_map_validate <- function(map, cells, weo_series = NULL) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  for (k in c("gcp", "ei", "weo_regions", "nze2021", "wei", "cells")) if (is.null(map[[k]])) add("no ", k, " section")
  if (length(p)) return(p)
  g <- map$gcp
  if (!setequal(names(g$national_sheets), CLIMATE_SRC_GCP_MEASURES)) add("gcp.national_sheets must name ", paste(CLIMATE_SRC_GCP_MEASURES, collapse = ", "))
  if (!is.numeric(g$c_to_co2) || length(g$c_to_co2) != 1L) add("gcp.c_to_co2 must be one number")
  if (!all(unlist(g$aggregate_geo) %in% c("EUU", "WLD")) || !all(names(g$aggregate_geo) %in% unlist(g$aggregates))) add("gcp.aggregate_geo must map listed aggregates to EUU or WLD")
  fuels <- unlist(g$fuel_columns)
  if (anyDuplicated(fuels) || !"total" %in% fuels) add("gcp.fuel_columns: duplicate keys or no total")
  ids <- vapply(map$ei$indicators, function(x) x$id %||% NA_character_, character(1))
  if (anyNA(ids) || anyDuplicated(ids)) add("ei.indicators: missing or duplicate id")
  for (x in map$ei$indicators) {
    for (k in c("sheet", "title", "label", "unit")) if (is.null(x[[k]]) || !nzchar(x[[k]])) add("ei ", x$id %||% "?", ": no ", k)
    if (!isTRUE(x$better %in% CLIMATE_SRC_BETTER)) add("ei ", x$id %||% "?", ": better must be lower, higher or none")
  }
  pw <- map$ei$power
  if (!length(pw$low_carbon) || !all(unlist(pw$low_carbon) %in% unlist(pw$columns))) add("ei.power.low_carbon must name power columns")
  w <- map$weo_regions
  if (!setequal(unname(unlist(w$scenarios)), CLIMATE_SRC_WEO_SCENARIOS)) add("weo_regions.scenarios must map to ", paste(CLIMATE_SRC_WEO_SCENARIOS, collapse = ", "))
  if (!all(unlist(w$regions) %in% CLIMATE_SRC_REGION_KINDS)) add("weo_regions.regions: kind must be one of ", paste(CLIMATE_SRC_REGION_KINDS, collapse = ", "))
  regs <- names(w$regions)
  sid <- vapply(w$series, function(s) s$id %||% NA_character_, character(1))
  if (anyNA(sid) || anyDuplicated(sid)) add("weo_regions.series: missing or duplicate id")
  if (!all(unlist(w$own) %in% regs)) add("weo_regions.own names a region that is not read")
  if (!all(names(w$members) %in% regs)) add("weo_regions.members names a region that is not read")
  if (!all(unlist(w$by_subregion) %in% regs)) add("weo_regions.by_subregion names a region that is not read")
  own_iso <- names(w$own)
  mem <- unlist(w$members)
  if (length(intersect(own_iso, mem))) add("weo_regions: ", paste(intersect(own_iso, mem), collapse = ", "), " is both own and a member")
  # An economy may be listed under the EU or Southeast Asia and nowhere else among the broad regions.
  broad <- setdiff(names(w$members), c("European Union", "Southeast Asia", "China"))
  bm <- unlist(w$members[broad])
  if (anyDuplicated(bm)) add("weo_regions.members: ", paste(unique(bm[duplicated(bm)]), collapse = ", "), " listed in two broad regions")
  n <- map$nze2021
  nid <- vapply(n$series, function(s) s$id %||% NA_character_, character(1))
  if (anyNA(nid) || anyDuplicated(nid)) add("nze2021.series: missing or duplicate id")
  for (s in n$series) {
    if (!isTRUE(s$match %in% c("direct", "partial"))) add("nze2021 ", s$id, ": match must be direct or partial")
    if (identical(s$match, "partial") && (is.null(s$note) || !nzchar(s$note))) add("nze2021 ", s$id, ": a partial match needs a note")
    if (!is.null(weo_series) && !isTRUE(s$weo %in% weo_series)) add("nze2021 ", s$id, ": weo ", s$weo %||% "?", " is not a series of config/iea_pathways_map.yml")
  }
  wr <- vapply(map$wei$rows, function(r) r$id %||% NA_character_, character(1))
  if (anyNA(wr) || anyDuplicated(wr)) add("wei.rows: missing or duplicate id")
  par <- unlist(lapply(map$wei$rows, function(r) r$parent))
  if (!all(par %in% wr)) add("wei.rows: a parent is not a row id")
  if (!all(unlist(map$wei$regions) %in% CLIMATE_SRC_WEI_KINDS)) add("wei.regions: kind must be one of ", paste(CLIMATE_SRC_WEI_KINDS, collapse = ", "))
  if (!all(unlist(map$wei$context) %in% wr)) add("wei.context names an unknown row")
  missing <- setdiff(cells, names(map$cells))
  extra <- setdiff(names(map$cells), cells)
  if (length(missing)) add("cells missing from the map: ", paste(missing, collapse = ", "))
  if (length(extra)) add("cells not in the sector dictionary: ", paste(extra, collapse = ", "))
  for (cell in names(map$cells)) {
    e <- map$cells[[cell]]
    if (is.null(e$note) || !nzchar(e$note)) add(cell, ": no note")
    used <- character()
    for (r in e$wei %||% list()) {
      wh <- paste0(cell, " wei ", r$id %||% "?", ": ")
      if (!isTRUE(r$id %in% wr)) add(wh, "unknown row")
      if (isTRUE(r$id %in% used)) add(wh, "listed twice")
      used <- c(used, r$id %||% "")
      if (!isTRUE(r$role %in% c("own", "demand"))) add(wh, "role must be own or demand")
      if (!isTRUE(r$match %in% c("direct", "partial", "proxy"))) add(wh, "match must be direct, partial or proxy")
    }
    gf <- unlist(e$gcp %||% list())
    if (length(setdiff(gf, setdiff(fuels, "total")))) add(cell, ": unknown GCP fuel ", paste(setdiff(gf, fuels), collapse = ", "))
  }
  p
}

# One row per (cell, source, key): cell, src (wei / gcp), key (WEI row id / GCP fuel), role, match (NA for
# context), order (within the cell and source). Cells with no row have none.
climate_sources_map_tbl <- function(map) {
  rows <- lapply(names(map$cells), function(cell) {
    e <- map$cells[[cell]]
    w <- e$wei %||% list()
    g <- unlist(e$gcp %||% list())
    dplyr::bind_rows(
      if (length(w)) tibble::tibble(cell = cell, src = "wei", key = vapply(w, function(r) r$id, character(1)), role = vapply(w, function(r) r$role, character(1)),
                                    match = vapply(w, function(r) r$match, character(1)), order = seq_along(w)),
      if (length(g)) tibble::tibble(cell = cell, src = "gcp", key = g, role = "context", match = NA_character_, order = seq_along(g))
    )
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) out <- tibble::tibble(cell = character(), src = character(), key = character(), role = character(), match = character(), order = integer())
  out$order <- as.integer(out$order)
  out
}

# One row per cell: cell, match (the best of its WEI rows; none without one), note.
climate_sources_cells_tbl <- function(map, map_tbl, cells) {
  rank <- match(map_tbl$match, CLIMATE_SRC_MATCH)
  best <- vapply(cells, function(s) {
    r <- rank[map_tbl$cell == s & map_tbl$src == "wei"]
    if (length(r)) CLIMATE_SRC_MATCH[min(r)] else "none"
  }, character(1), USE.NAMES = FALSE)
  tibble::tibble(cell = cells, match = best, note = vapply(cells, function(s) map$cells[[s]]$note, character(1), USE.NAMES = FALSE))
}

# ---- Global Carbon Project --------------------------------------------------------------------------------

# One national sheet (territorial, consumption or transfers) read with col_names = FALSE: the row whose
# first cell is the unit ("MtC/yr"), then upper-case names, then the names as published, then one row
# per year. Returns list(names, years, values = matrix years x names) in MtC.
.cs_gcp_sheet <- function(d, unit, label) {
  r <- which(vapply(seq_len(nrow(d)), function(i) .cs_txt(d, i, 1L), character(1)) == unit)
  if (length(r) != 1L) stop("GCP national workbook, sheet ", label, ": no single '", unit, "' unit row (renamed sheet or new unit)")
  nm <- vapply(2:ncol(d), function(j) .cs_txt(d, r + 2L, j), character(1))
  keep <- nzchar(nm)
  yrs <- as.integer(.cs_num(d[[1]][(r + 3L):nrow(d)]))
  ok <- !is.na(yrs)
  if (!any(ok) || any(diff(yrs[ok]) != 1L)) stop("GCP national workbook, sheet ", label, ": years are not consecutive")
  vals <- vapply(which(keep) + 1L, function(j) .cs_num(d[[j]][(r + 3L):nrow(d)])[ok], double(sum(ok)))
  if (is.null(dim(vals))) vals <- matrix(vals, nrow = sum(ok))
  list(names = nm[keep], years = yrs[ok], values = vals)
}

# National fossil CO2 from the three national sheets (`sheets`: named list territorial / consumption /
# transfers of data frames read with col_names = FALSE). to_iso3: a function from GCP names to ISO3 (NA
# when unknown). Returns iso3 (EUU for EU27, WLD for World), name (as published), measure, year, value
# (Mt CO2 = MtC x c_to_co2), from national_first_year, without missing values. Stops on a name that is
# neither an economy nor a listed aggregate, on two names with one ISO3, and when the sheets disagree on
# their columns.
climate_sources_gcp_national <- function(sheets, map, to_iso3) {
  g <- map$gcp
  miss <- setdiff(CLIMATE_SRC_GCP_MEASURES, names(sheets))
  if (length(miss)) stop("GCP national workbook: missing sheet(s) ", paste(miss, collapse = ", "))
  parsed <- lapply(CLIMATE_SRC_GCP_MEASURES, function(m) .cs_gcp_sheet(sheets[[m]], g$national_unit, g$national_sheets[[m]]))
  names(parsed) <- CLIMATE_SRC_GCP_MEASURES
  nm <- parsed$territorial$names
  for (m in CLIMATE_SRC_GCP_MEASURES[-1]) if (!identical(parsed[[m]]$names, nm)) stop("GCP national workbook: sheet ", g$national_sheets[[m]], " has other columns than the territorial sheet")
  agg <- unlist(g$aggregates)
  agg_geo <- unlist(g$aggregate_geo)
  custom <- unlist(g$names)
  iso <- ifelse(nm %in% names(custom), custom[nm], NA_character_)
  todo <- is.na(iso) & !nm %in% agg
  iso[todo] <- to_iso3(nm[todo])
  iso[nm %in% names(agg_geo)] <- agg_geo[nm[nm %in% names(agg_geo)]]
  bad <- nm[is.na(iso) & !nm %in% agg]
  if (length(bad)) stop("GCP national workbook: no ISO3 for ", paste(bad, collapse = ", "), " (add it to gcp.names in config/climate_sources_map.yml)")
  dup <- iso[!is.na(iso) & duplicated(iso)]
  if (length(dup)) stop("GCP national workbook: two columns map to ", paste(unique(dup), collapse = ", "))
  out <- list()
  for (m in CLIMATE_SRC_GCP_MEASURES) {
    p <- parsed[[m]]
    for (j in which(!is.na(iso))) {
      v <- p$values[, j]
      k <- !is.na(v) & p$years >= g$national_first_year
      if (!any(k)) next
      out[[length(out) + 1L]] <- tibble::tibble(iso3 = iso[j], name = nm[j], measure = m, year = p$years[k], value = v[k] * g$c_to_co2)
    }
  }
  res <- dplyr::bind_rows(out)
  res[order(res$iso3, match(res$measure, CLIMATE_SRC_GCP_MEASURES), res$year, method = "radix"), , drop = FALSE]
}

# World fossil CO2 by fuel from the "Fossil Emissions by Category" sheet (col_names = FALSE): the unit row
# ("GtC/yr"), then the header row starting "Year". Returns fuel (key), year, value (Gt CO2) from
# fuel_first_year. Stops on a missing or renamed fuel column.
climate_sources_gcp_fuel <- function(d, map) {
  g <- map$gcp
  col1 <- vapply(seq_len(nrow(d)), function(i) .cs_txt(d, i, 1L), character(1))
  if (!g$fuel_unit %in% col1) stop("GCP budget workbook, sheet ", g$fuel_sheet, ": no '", g$fuel_unit, "' unit row")
  h <- which(col1 == "Year")
  if (length(h) != 1L) stop("GCP budget workbook, sheet ", g$fuel_sheet, ": no single 'Year' header row")
  hdr <- vapply(seq_len(ncol(d)), function(j) .cs_txt(d, h, j), character(1))
  fc <- unlist(g$fuel_columns)
  miss <- setdiff(names(fc), hdr)
  if (length(miss)) stop("GCP budget workbook, sheet ", g$fuel_sheet, ": missing column(s) ", paste(miss, collapse = ", "), " (renamed?)")
  yrs <- as.integer(.cs_num(d[[1]][(h + 1L):nrow(d)]))
  ok <- !is.na(yrs) & yrs >= g$fuel_first_year
  out <- lapply(names(fc), function(col) {
    v <- .cs_num(d[[match(col, hdr)]][(h + 1L):nrow(d)])[ok]
    tibble::tibble(fuel = unname(fc[col]), year = yrs[ok], value = v * g$c_to_co2)
  })
  res <- dplyr::bind_rows(out)
  if (anyNA(res$value)) stop("GCP budget workbook: a fuel has a missing value after ", g$fuel_first_year)
  res[order(match(res$fuel, fc), res$year), , drop = FALSE]
}

# ---- Energy Institute Country Transition Tracker -----------------------------------------------------------

# One indicator sheet (col_names = FALSE): title in A1, unit text in A2, header row "Country" with years.
.cs_ei_sheet <- function(d, title, label) {
  if (.cs_txt(d, 1L, 1L) != title) stop("EI Country Transition Tracker, sheet ", label, ": title is '", .cs_txt(d, 1L, 1L), "', expected '", title, "' (renamed?)")
  h <- which(vapply(seq_len(nrow(d)), function(i) .cs_txt(d, i, 1L), character(1)) == "Country")
  if (length(h) != 1L) stop("EI Country Transition Tracker, sheet ", label, ": no single 'Country' header row")
  list(h = h, unit = .cs_txt(d, 2L, 1L), hdr = vapply(seq_len(ncol(d)), function(j) .cs_txt(d, h, j), character(1)))
}

# The tracker's indicators (`sheets`: named list of data frames read with col_names = FALSE, by sheet
# name). Returns list(values = iso3, name, ind, year, value; units = sheet, unit as published). Year
# columns are the header cells that are four-digit years (growth columns are left out); the power sheet's
# year is the single year above its header. Stops on a missing or renamed sheet, an unknown economy or a
# duplicate.
climate_sources_ei <- function(sheets, map, to_iso3) {
  e <- map$ei
  custom <- unlist(e$names)
  iso_of <- function(nm) {
    iso <- ifelse(nm %in% names(custom), custom[nm], NA_character_)
    iso[is.na(iso)] <- to_iso3(nm[is.na(iso)])
    bad <- nm[is.na(iso)]
    if (length(bad)) stop("EI Country Transition Tracker: no ISO3 for ", paste(bad, collapse = ", "), " (add it to ei.names)")
    iso
  }
  out <- list()
  units <- list()
  for (x in e$indicators) {
    d <- sheets[[x$sheet]]
    if (is.null(d)) stop("EI Country Transition Tracker: no sheet '", x$sheet, "' (renamed?)")
    s <- .cs_ei_sheet(d, x$title, x$sheet)
    yc <- which(grepl("^(19|20)\\d{2}$", s$hdr))
    if (!length(yc)) stop("EI Country Transition Tracker, sheet ", x$sheet, ": no year columns")
    rows <- (s$h + 1L):nrow(d)
    nm <- vapply(rows, function(i) .cs_txt(d, i, 1L), character(1))
    rows <- rows[nzchar(nm)]
    nm <- nm[nzchar(nm)]
    iso <- iso_of(nm)
    for (j in yc) {
      v <- .cs_num(d[[j]][rows])
      out[[length(out) + 1L]] <- tibble::tibble(iso3 = iso, name = nm, ind = x$id, year = as.integer(s$hdr[j]), value = v)
    }
    units[[length(units) + 1L]] <- tibble::tibble(sheet = x$sheet, unit = s$unit)
  }
  pw <- e$power
  d <- sheets[[pw$sheet]]
  if (is.null(d)) stop("EI Country Transition Tracker: no sheet '", pw$sheet, "' (renamed?)")
  s <- .cs_ei_sheet(d, pw$title, pw$sheet)
  yr <- suppressWarnings(as.integer(.cs_txt(d, s$h - 1L, 2L)))
  if (is.na(yr)) stop("EI Country Transition Tracker, sheet ", pw$sheet, ": no year above the header")
  pc <- unlist(pw$columns)
  miss <- setdiff(names(pc), s$hdr)
  if (length(miss)) stop("EI Country Transition Tracker, sheet ", pw$sheet, ": missing column(s) ", paste(miss, collapse = ", "))
  rows <- (s$h + 1L):nrow(d)
  nm <- vapply(rows, function(i) .cs_txt(d, i, 1L), character(1))
  rows <- rows[nzchar(nm)]
  nm <- nm[nzchar(nm)]
  iso <- iso_of(nm)
  for (col in names(pc)) {
    v <- .cs_num(d[[match(col, s$hdr)]][rows])
    out[[length(out) + 1L]] <- tibble::tibble(iso3 = iso, name = nm, ind = paste0("power_", pc[[col]]), year = yr, value = v)
  }
  units[[length(units) + 1L]] <- tibble::tibble(sheet = pw$sheet, unit = s$unit)
  vals <- dplyr::bind_rows(out)
  vals <- vals[!is.na(vals$value), , drop = FALSE]
  if (anyDuplicated(vals[c("iso3", "ind", "year")])) stop("EI Country Transition Tracker: duplicate (economy, indicator, year)")
  vals <- vals[order(vals$ind, vals$year, vals$iso3, method = "radix"), , drop = FALSE]
  list(values = vals, units = dplyr::bind_rows(units))
}

# The indicators as a table: id, label, unit (the explorer's), better, sheet. Power columns get
# power_<key> rows with unit share; low_carbon marks the columns the views add up.
climate_sources_ei_indicators <- function(map) {
  e <- map$ei
  a <- tibble::tibble(id = vapply(e$indicators, function(x) x$id, character(1)), label = vapply(e$indicators, function(x) x$label, character(1)),
                      unit = vapply(e$indicators, function(x) x$unit, character(1)), better = vapply(e$indicators, function(x) x$better, character(1)),
                      sheet = vapply(e$indicators, function(x) x$sheet, character(1)), low_carbon = NA)
  pc <- unlist(e$power$columns)
  b <- tibble::tibble(id = paste0("power_", unname(pc)), label = paste0(names(pc), "'s share of power generation"), unit = "share", better = "none",
                      sheet = e$power$sheet, low_carbon = unname(pc) %in% unlist(e$power$low_carbon))
  dplyr::bind_rows(a, b)
}

# The median economy per (indicator, year) over every economy the tracker lists: ind, year, value, n.
climate_sources_ei_median <- function(values) {
  out <- dplyr::summarise(dplyr::group_by(values, .data$ind, .data$year), value = stats::median(.data$value), n = dplyr::n(), .groups = "drop")
  out$n <- as.integer(out$n)
  out[order(out$ind, out$year, method = "radix"), , drop = FALSE]
}

# ---- IEA WEO 2025 regions ------------------------------------------------------------------------------------

# The mapped series of the regions read, long: region, series, scenario (key), year, value; plus the series
# table (id, label, unit, category, product, flow). Stops on a missing column, another publication, an
# unknown scenario or region (a new edition renamed one), a series with two units, or a region read with
# none of the series.
climate_sources_weo_regions <- function(weo, map) {
  w <- map$weo_regions
  cols <- c("PUBLICATION", "SCENARIO", "CATEGORY", "PRODUCT", "FLOW", "UNIT", "REGION", "YEAR", "VALUE")
  miss <- setdiff(cols, names(weo))
  if (length(miss)) stop("WEO regions: missing column(s) ", paste(miss, collapse = ", "))
  pub <- unique(weo$PUBLICATION)
  if (!identical(pub, w$publication)) stop("WEO regions: publication ", paste(pub, collapse = ", "), ", expected ", w$publication)
  sc <- unlist(w$scenarios)
  unknown <- setdiff(unique(weo$SCENARIO), names(sc))
  if (length(unknown)) stop("WEO regions: unknown scenario label(s) ", paste(unknown, collapse = ", "), " (update weo_regions.scenarios)")
  regs <- names(w$regions)
  ur <- setdiff(unique(weo$REGION), c(regs, unlist(w$ignored_regions)))
  if (length(ur)) stop("WEO regions: unknown region(s) ", paste(ur, collapse = ", "), " (add to weo_regions.regions or ignored_regions)")
  gone <- setdiff(regs, unique(weo$REGION))
  if (length(gone)) stop("WEO regions: region(s) ", paste(gone, collapse = ", "), " not in the file (renamed?)")
  key <- paste(weo$CATEGORY, weo$PRODUCT, weo$FLOW, sep = "|")
  out <- list()
  ser <- list()
  for (s in w$series) {
    r <- weo[key == paste(s$category, s$product, s$flow, sep = "|") & weo$REGION %in% regs, , drop = FALSE]
    if (!nrow(r)) stop("WEO regions: no rows for series ", s$id)
    u <- unique(r$UNIT)
    if (length(u) != 1L) stop("WEO regions: series ", s$id, " has ", length(u), " units")
    ser[[length(ser) + 1L]] <- tibble::tibble(id = s$id, label = s$label, unit = u, category = s$category, product = s$product, flow = s$flow)
    out[[length(out) + 1L]] <- tibble::tibble(region = r$REGION, series = s$id, scenario = unname(sc[r$SCENARIO]), year = as.integer(r$YEAR), value = as.double(r$VALUE))
  }
  values <- dplyr::bind_rows(out)
  values <- values[!is.na(values$value), , drop = FALSE]
  none <- setdiff(regs, unique(values$region))
  if (length(none)) stop("WEO regions: no mapped series for ", paste(none, collapse = ", "))
  if (anyDuplicated(values[c("region", "series", "scenario", "year")])) stop("WEO regions: duplicate (region, series, scenario, year)")
  sid <- vapply(w$series, function(s) s$id, character(1))
  values <- values[order(match(values$region, regs), match(values$series, sid), match(values$scenario, CLIMATE_SRC_WEO_SCENARIOS), values$year), , drop = FALSE]
  list(series = dplyr::bind_rows(ser), values = values,
       regions = tibble::tibble(region = regs, kind = unname(unlist(w$regions))))
}

# Economy -> WEO region for `iso3` (the explorer's economies): iso3, region, match (own / member /
# by_subregion), in the order own, European Union, Southeast Asia, the other listed regions, then the UN
# sub-region (`subregion`: a function from ISO3 to the UN sub-region name, NA when unknown). Economies with
# none are left out.
climate_sources_weo_geo <- function(iso3, map, subregion) {
  w <- map$weo_regions
  own <- unlist(w$own)
  first <- c("China", "European Union", "Southeast Asia")
  order_r <- c(intersect(first, names(w$members)), setdiff(names(w$members), first))
  region <- rep(NA_character_, length(iso3))
  how <- rep(NA_character_, length(iso3))
  k <- iso3 %in% names(own)
  region[k] <- own[iso3[k]]
  how[k] <- "own"
  for (r in order_r) {
    k <- is.na(region) & iso3 %in% unlist(w$members[[r]])
    region[k] <- r
    how[k] <- "member"
  }
  bs <- unlist(w$by_subregion)
  sub <- subregion(iso3)
  k <- is.na(region) & !is.na(sub) & sub %in% names(bs)
  region[k] <- bs[sub[k]]
  how[k] <- "by_subregion"
  out <- tibble::tibble(iso3 = iso3, region = unname(region), match = how)
  out <- out[!is.na(out$region), , drop = FALSE]
  out[order(out$iso3), , drop = FALSE]
}

# ---- IEA Net Zero by 2050 (2021) ---------------------------------------------------------------------------

# The paired series of NZE2021_AnnexA.csv: list(series = id, label, unit, category, product, flow, weo,
# match, note; values = series, year, value). Stops on a missing column, another publication or scenario,
# or a series without exactly one row per year.
climate_sources_nze2021 <- function(nze, map) {
  n <- map$nze2021
  cols <- c("Publication", "Scenario", "Category", "Product", "Flow", "Unit", "Year", "Value")
  miss <- setdiff(cols, names(nze))
  if (length(miss)) stop("NZE 2021 Annex A: missing column(s) ", paste(miss, collapse = ", "))
  if (!identical(unique(nze$Publication), n$publication)) stop("NZE 2021 Annex A: publication ", paste(unique(nze$Publication), collapse = ", "), ", expected ", n$publication)
  if (!identical(unique(nze$Scenario), n$scenario)) stop("NZE 2021 Annex A: scenario ", paste(unique(nze$Scenario), collapse = ", "), ", expected ", n$scenario, " (renamed?)")
  key <- paste(nze$Category, nze$Product, nze$Flow, sep = "|")
  years <- sort(unique(as.integer(nze$Year)))
  if (!n$milestone_year %in% years) stop("NZE 2021 Annex A: no ", n$milestone_year, " values")
  ser <- list()
  val <- list()
  for (s in n$series) {
    r <- nze[key == paste(s$category, s$product, s$flow, sep = "|"), , drop = FALSE]
    if (nrow(r) != length(years) || anyDuplicated(r$Year)) stop("NZE 2021 Annex A: series ", s$id, " has ", nrow(r), " rows, expected one per year (", length(years), ")")
    u <- unique(r$Unit)
    if (length(u) != 1L) stop("NZE 2021 Annex A: series ", s$id, " has ", length(u), " units")
    ser[[length(ser) + 1L]] <- tibble::tibble(id = s$id, label = s$label, unit = u, category = s$category, product = s$product, flow = s$flow,
                                              weo = s$weo, match = s$match, note = s$note %||% NA_character_)
    val[[length(val) + 1L]] <- tibble::tibble(series = s$id, year = as.integer(r$Year), value = as.double(r$Value))
  }
  values <- dplyr::bind_rows(val)
  if (anyNA(values$value)) stop("NZE 2021 Annex A: a paired series has a missing value")
  values <- values[order(match(values$series, vapply(n$series, function(s) s$id, character(1))), values$year), , drop = FALSE]
  list(series = dplyr::bind_rows(ser), values = values)
}

# ---- IEA World Energy Investment 2026 ------------------------------------------------------------------------

# The region sheets (`sheets`: named list by region of data frames read with col_names = FALSE): the
# region label in A3 with the years beside it, then the row labels of wei.rows in order. Returns
# list(rows = id, label, name, parent, unit; values = row, region, year, value, estimate). Stops on a
# missing sheet, a renamed, added or dropped row (rows with `only` are expected on those regions' sheets
# alone), or a region label that is not the sheet's.
climate_sources_wei <- function(sheets, map) {
  w <- map$wei
  rows <- w$rows
  labels <- vapply(rows, function(r) r$label, character(1))
  ids <- vapply(rows, function(r) r$id, character(1))
  est <- as.integer(unlist(w$estimate_years))
  out <- list()
  for (reg in names(w$regions)) {
    d <- sheets[[reg]]
    if (is.null(d)) stop("World Energy Investment: no sheet '", reg, "' (renamed?)")
    hl <- w$region_labels[[reg]] %||% reg
    hr <- which(vapply(seq_len(min(nrow(d), 10L)), function(i) .cs_txt(d, i, 1L), character(1)) == hl)
    if (length(hr) != 1L) stop("World Energy Investment, sheet ", reg, ": no header row labelled '", hl, "'")
    yrs <- suppressWarnings(as.integer(vapply(2:ncol(d), function(j) .cs_txt(d, hr, j), character(1))))
    yc <- which(!is.na(yrs)) + 1L
    yrs <- yrs[!is.na(yrs)]
    col1 <- vapply((hr + 1L):nrow(d), function(i) .cs_txt(d, i, 1L), character(1))
    ri <- (hr + 1L):nrow(d)
    # Data rows: a label with at least one number beside it (the trailing note row has none).
    has_num <- vapply(ri, function(i) any(!is.na(vapply(yc, function(j) .cs_num(d[[j]][i]), double(1)))), logical(1))
    lab <- col1[nzchar(col1) & has_num]
    ri <- ri[nzchar(col1) & has_num]
    here <- vapply(rows, function(r) is.null(r$only) || reg %in% unlist(r$only), logical(1))
    if (!identical(lab, labels[here])) {
      dif <- setdiff(union(lab, labels[here]), intersect(lab, labels[here]))
      stop("World Energy Investment, sheet ", reg, ": row labels differ from wei.rows (", if (length(dif)) paste(dif, collapse = ", ") else "order", ")")
    }
    for (k in seq_along(ri)) {
      v <- vapply(yc, function(j) .cs_num(d[[j]][ri[k]]), double(1))
      out[[length(out) + 1L]] <- tibble::tibble(row = ids[here][k], region = reg, year = yrs, value = v, estimate = yrs %in% est)
    }
  }
  values <- dplyr::bind_rows(out)
  values <- values[!is.na(values$value), , drop = FALSE]
  rt <- tibble::tibble(id = ids, label = labels, name = vapply(rows, function(r) r$name, character(1)),
                       parent = vapply(rows, function(r) r$parent %||% NA_character_, character(1)),
                       unit = vapply(rows, function(r) r$unit %||% w$unit, character(1)))
  values <- values[order(match(values$row, ids), match(values$region, names(w$regions)), values$year), , drop = FALSE]
  regs <- names(w$regions)
  list(rows = rt, values = values,
       regions = tibble::tibble(region = regs, label = vapply(regs, function(r) w$region_labels[[r]] %||% r, character(1), USE.NAMES = FALSE), kind = unname(unlist(w$regions))))
}
