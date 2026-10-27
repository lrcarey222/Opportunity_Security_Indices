# IEA scenario pathways (World) mapped to technology x stage (config/iea_pathways_map.yml).
#
# Why: the Sector Profile's Climate lens and section showed what is emitted today (Climate TRACE) but not
# where the IEA's scenarios say the sector is heading (user request of 2026-10-17: "IEA Net Zero report
# data for the Climate section"). The IEA's World Energy Outlook 2025 Annex A free dataset (CC BY 4.0)
# carries, for the World only, installed capacity by technology, electricity generation and fuel supply,
# and CO2 by sector, for Historical years and the Stated Policies, Current Policies and Net Zero Emissions
# by 2050 scenarios; the IEA Critical Minerals Dataset 2026 (CC BY 4.0) carries mineral demand by clean
# technology in the Current Policies, Stated Policies and High Demand scenarios (no Net Zero scenario).
# The map states, per cell, which series stand for its own activity, which for the demand it supplies
# and which are emissions context, each with a match quality.
#
# Pure (no IO). scripts/17_build_iea_pathways.R reads the WEO CSV (and the previous vintage, for a
# vintage check only), the Critical Minerals workbook through read_iea_critical_minerals()
# (R/utils/iea_critical_minerals.R) and the map, and writes data/processed/iea_pathways_*_tbl.rds with
# the functions below; scripts/87 ships them as the climate.nze_* tables of the sector-climate chunk.

IEA_PATHWAYS_ROLES <- c("own", "demand", "context")
IEA_PATHWAYS_MATCH <- c("direct", "partial", "proxy", "none")
IEA_PATHWAYS_GROUPS <- c("deployment", "power", "fuel", "emissions")
IEA_PATHWAYS_WEO_SCENARIOS <- c("historical", "stated", "current", "nze")
IEA_PATHWAYS_MINERAL_SCENARIOS <- c("current", "stated", "high_demand")
IEA_PATHWAYS_WEO_COLUMNS <- c("SCENARIO", "CATEGORY", "PRODUCT", "FLOW", "UNIT", "REGION", "YEAR", "VALUE")

# ---- Map --------------------------------------------------------------------------------------------

# Every problem of the parsed config/iea_pathways_map.yml (empty when valid). cells: the cell slugs the
# map must cover (sector dictionary `cells`). mineral_techs: the Critical Minerals technology labels
# (sheet 3.1) a cell may name; NULL skips that check.
iea_pathways_map_validate <- function(map, cells, mineral_techs = NULL) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  sw <- unlist(map$scenarios$weo %||% list())
  sm <- unlist(map$scenarios$minerals %||% list())
  if (!setequal(unname(sw), IEA_PATHWAYS_WEO_SCENARIOS)) add("scenarios.weo must map to ", paste(IEA_PATHWAYS_WEO_SCENARIOS, collapse = ", "))
  if (!setequal(unname(sm), IEA_PATHWAYS_MINERAL_SCENARIOS)) add("scenarios.minerals must map to ", paste(IEA_PATHWAYS_MINERAL_SCENARIOS, collapse = ", "))
  ids <- vapply(map$series %||% list(), function(s) s$id %||% NA_character_, character(1))
  if (!length(ids) || anyNA(ids) || anyDuplicated(ids)) add("series: missing or duplicate id")
  for (s in map$series %||% list()) {
    w <- paste0("series ", s$id %||% "?", ": ")
    for (k in c("category", "product", "flow", "label")) if (is.null(s[[k]]) || !nzchar(s[[k]])) add(w, "no ", k)
    if (!isTRUE(s$group %in% IEA_PATHWAYS_GROUPS)) add(w, "group must be one of ", paste(IEA_PATHWAYS_GROUPS, collapse = ", "))
  }
  key <- vapply(map$series %||% list(), function(s) paste(s$category, s$product, s$flow, sep = "|"), character(1))
  if (anyDuplicated(key)) add("series: two ids read the same category, product and flow")
  missing <- setdiff(cells, names(map$cells))
  extra <- setdiff(names(map$cells), cells)
  if (length(missing)) add("cells missing from the map: ", paste(missing, collapse = ", "))
  if (length(extra)) add("cells not in the sector dictionary: ", paste(extra, collapse = ", "))
  for (cell in names(map$cells)) {
    e <- map$cells[[cell]]
    if (is.null(e$note) || !nzchar(e$note)) add(cell, ": no note")
    used <- character()
    for (s in e$series %||% list()) {
      w <- paste0(cell, " ", s$id %||% "?", ": ")
      if (!isTRUE(s$id %in% ids)) add(w, "unknown series")
      if (isTRUE(s$id %in% used)) add(w, "listed twice")
      used <- c(used, s$id %||% "")
      if (!isTRUE(s$role %in% IEA_PATHWAYS_ROLES)) add(w, "role must be own, demand or context")
      if (isTRUE(s$role %in% c("own", "demand")) && !isTRUE(s$match %in% setdiff(IEA_PATHWAYS_MATCH, "none"))) add(w, "an own or demand series needs match direct, partial or proxy")
      if (identical(s$role, "context") && !is.null(s$match)) add(w, "a context series has no match quality")
    }
    mt <- unlist(e$minerals %||% list())
    if (length(mt) && !is.null(mineral_techs) && length(setdiff(mt, mineral_techs))) add(cell, ": unknown mineral technology ", paste(setdiff(mt, mineral_techs), collapse = ", "))
  }
  wc <- map$world_context
  if (!is.null(wc) && !isTRUE(wc %in% ids)) add("world_context names an unknown series")
  p
}

# The series as a table: id, group, label, category, product, flow (unit comes from the data).
iea_pathways_series_specs <- function(map) {
  ss <- map$series
  tibble::tibble(
    id = vapply(ss, function(s) s$id, character(1)), group = vapply(ss, function(s) s$group, character(1)),
    label = vapply(ss, function(s) s$label, character(1)), category = vapply(ss, function(s) s$category, character(1)),
    product = vapply(ss, function(s) s$product, character(1)), flow = vapply(ss, function(s) s$flow, character(1))
  )
}

# One row per (cell, series): cell, series, role, match (NA for context), order. Cells with no series
# have no row.
iea_pathways_map_tbl <- function(map) {
  rows <- lapply(names(map$cells), function(cell) {
    ss <- map$cells[[cell]]$series %||% list()
    if (!length(ss)) return(NULL)
    tibble::tibble(cell = cell, series = vapply(ss, function(s) s$id, character(1)), role = vapply(ss, function(s) s$role, character(1)),
                   match = vapply(ss, function(s) s$match %||% NA_character_, character(1)), order = seq_along(ss))
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) out <- tibble::tibble(cell = character(), series = character(), role = character(), match = character(), order = integer())
  out$order <- as.integer(out$order)
  out
}

# One row per cell: cell, match (the best of its own and demand series; none without one), note,
# mineral_techs (the Critical Minerals technologies, "|"-separated; NA for none).
iea_pathways_cells_tbl <- function(map, map_tbl, cells) {
  rank <- match(map_tbl$match, IEA_PATHWAYS_MATCH)
  best <- vapply(cells, function(s) {
    r <- rank[map_tbl$cell == s & map_tbl$role %in% c("own", "demand")]
    if (length(r)) IEA_PATHWAYS_MATCH[min(r)] else "none"
  }, character(1), USE.NAMES = FALSE)
  mt <- vapply(cells, function(s) {
    m <- unlist(map$cells[[s]]$minerals %||% list())
    if (length(m)) paste(m, collapse = "|") else NA_character_
  }, character(1), USE.NAMES = FALSE)
  tibble::tibble(cell = cells, match = best, note = vapply(cells, function(s) map$cells[[s]]$note, character(1), USE.NAMES = FALSE), mineral_techs = mt)
}

# ---- WEO Annex A -------------------------------------------------------------------------------------

# The WEO rows of the mapped series (World), long: series, scenario (key), year, value, plus the series
# table with its unit. Stops when a series has no row, more than one unit, a duplicate key, or lacks a
# scenario; a scenario label the map does not know stops too (a new edition renamed one).
iea_pathways_weo <- function(weo, map) {
  miss <- setdiff(IEA_PATHWAYS_WEO_COLUMNS, names(weo))
  if (length(miss)) stop("WEO Annex A: missing column(s) ", paste(miss, collapse = ", "))
  sc <- unlist(map$scenarios$weo)
  unknown <- setdiff(unique(weo$SCENARIO), names(sc))
  if (length(unknown)) stop("WEO Annex A: unknown scenario label(s) ", paste(unknown, collapse = ", "), " (update scenarios.weo in config/iea_pathways_map.yml)")
  if (any(weo$REGION != "World")) stop("WEO Annex A: rows for a region other than World (", paste(utils::head(unique(weo$REGION[weo$REGION != "World"]), 3), collapse = ", "), ")")
  specs <- iea_pathways_series_specs(map)
  key <- paste(weo$CATEGORY, weo$PRODUCT, weo$FLOW, sep = "|")
  out <- list()
  unit <- character(nrow(specs))
  for (i in seq_len(nrow(specs))) {
    r <- weo[key == paste(specs$category[i], specs$product[i], specs$flow[i], sep = "|"), , drop = FALSE]
    if (!nrow(r)) stop("WEO Annex A: no rows for series ", specs$id[i], " (", specs$category[i], " / ", specs$product[i], " / ", specs$flow[i], ")")
    u <- unique(r$UNIT)
    if (length(u) != 1L) stop("WEO Annex A: series ", specs$id[i], " has ", length(u), " units")
    unit[i] <- u
    s <- unname(sc[r$SCENARIO])
    have <- unique(s)
    if (length(setdiff(IEA_PATHWAYS_WEO_SCENARIOS, have))) stop("WEO Annex A: series ", specs$id[i], " lacks scenario(s) ", paste(setdiff(IEA_PATHWAYS_WEO_SCENARIOS, have), collapse = ", "))
    out[[i]] <- tibble::tibble(series = specs$id[i], scenario = s, year = as.integer(r$YEAR), value = as.double(r$VALUE))
  }
  values <- dplyr::bind_rows(out)
  if (anyNA(values$value) || anyNA(values$year)) stop("WEO Annex A: a mapped series has a missing year or value")
  if (anyDuplicated(values[c("series", "scenario", "year")])) stop("WEO Annex A: duplicate (series, scenario, year)")
  values <- values[order(match(values$series, specs$id), match(values$scenario, IEA_PATHWAYS_WEO_SCENARIOS), values$year), , drop = FALSE]
  specs$unit <- unit
  list(series = specs[c("id", "group", "label", "unit", "category", "product", "flow")], values = values)
}

# The edition of a WEO Annex A frame ("World Energy Outlook 2025"), from PUBLICATION.
iea_pathways_weo_edition <- function(weo) {
  p <- unique(weo$PUBLICATION %||% NA_character_)
  if (length(p) != 1L || is.na(p)) stop("WEO Annex A: expected one PUBLICATION, got ", paste(p, collapse = ", "))
  p
}

# Vintage check: the mapped series of two WEO editions on the (series, scenario, year) both publish for
# the stated and nze scenarios. `old` may label the stated scenario differently ("Stated Policies
# Scenario" in both 2024 and 2025) and lacks current; historical years differ, so only projection years
# are compared. Returns series, scenario, year, new, old, rel (new / old - 1).
iea_pathways_vintage <- function(new_values, old_weo, map) {
  sc <- unlist(map$scenarios$weo)
  old <- old_weo[old_weo$SCENARIO %in% names(sc)[sc %in% c("stated", "nze")], , drop = FALSE]
  specs <- iea_pathways_series_specs(map)
  key <- paste(old$CATEGORY, old$PRODUCT, old$FLOW, sep = "|")
  old$series <- specs$id[match(key, paste(specs$category, specs$product, specs$flow, sep = "|"))]
  old <- old[!is.na(old$series), , drop = FALSE]
  o <- tibble::tibble(series = old$series, scenario = unname(sc[old$SCENARIO]), year = as.integer(old$YEAR), old = as.double(old$VALUE))
  n <- new_values[new_values$scenario %in% c("stated", "nze"), , drop = FALSE]
  out <- dplyr::inner_join(dplyr::rename(n, new = "value"), o, by = c("series", "scenario", "year"))
  out$rel <- ifelse(out$old != 0, out$new / out$old - 1, NA_real_)
  out[order(match(out$series, specs$id), out$scenario, out$year), c("series", "scenario", "year", "new", "old", "rel")]
}

# ---- Critical Minerals ----------------------------------------------------------------------------------

# Mineral demand by scenario from read_iea_critical_minerals(path, scenario = NULL): sheet 1 (total demand
# for the key minerals, by use, incl. other uses), 3.1 (clean-energy demand by technology) and 3.2
# (clean-energy demand by mineral), long: sheet ("1", "3.1", "3.2"), mineral, tech (the row label as
# published; "Total" on sheet 3.2), scenario (key), year, kt. The base year (shared by the scenarios) is
# kept once per scenario, as published. Sheet 1's share rows (a ratio, not kt) are left out.
iea_pathways_minerals <- function(critical, map) {
  sm <- unlist(map$scenarios$minerals)
  d <- critical[critical$Scenario != "Base case", , drop = FALSE]
  unknown <- setdiff(unique(d$Scenario), names(sm))
  if (length(unknown)) stop("IEA Critical Minerals: unknown scenario label(s) ", paste(unknown, collapse = ", "), " (update scenarios.minerals in config/iea_pathways_map.yml)")
  sheet <- ifelse(startsWith(d$Pillar, "3.1"), "3.1", ifelse(startsWith(d$Pillar, "3.2"), "3.2", ifelse(startsWith(d$Pillar, "1 "), "1", NA_character_)))
  d <- d[!is.na(sheet), , drop = FALSE]
  sheet <- sheet[!is.na(sheet)]
  keep <- !grepl("^Share of", d$Sector.Country)
  d <- d[keep, , drop = FALSE]
  sheet <- sheet[keep]
  ycols <- grep("^X(19|20)\\d{2}$", names(d), value = TRUE)
  rows <- lapply(ycols, function(yc) tibble::tibble(sheet = sheet, mineral = trimws(d$Mineral), tech = trimws(d$Sector.Country),
                                                   scenario = unname(sm[d$Scenario]), year = as.integer(sub("^X", "", yc)), kt = as.double(d[[yc]])))
  out <- dplyr::bind_rows(rows)
  out <- out[!is.na(out$kt), , drop = FALSE]
  if (anyDuplicated(out[c("sheet", "mineral", "tech", "scenario", "year")])) stop("IEA Critical Minerals: duplicate (sheet, mineral, technology, scenario, year)")
  out[order(out$sheet, out$mineral, out$tech, match(out$scenario, IEA_PATHWAYS_MINERAL_SCENARIOS), out$year, method = "radix"), , drop = FALSE]
}

# The technologies named on sheet 3.1 (row labels other than the "Total <mineral>" rows).
iea_pathways_mineral_techs <- function(minerals) {
  t <- unique(minerals$tech[minerals$sheet == "3.1"])
  sort(t[!grepl("^Total", t)])
}
