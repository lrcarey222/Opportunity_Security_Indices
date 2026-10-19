# Climate TRACE sector emissions mapped to technology x stage (config/climate_trace_map.yml).
#
# Why: the Sector Profile's Climate lens and section and the Country Profile's Policy & climate
# section had no emissions figures (user request of 2026-10-17). Climate TRACE publishes country x
# subsector emissions (CO2e, 100-year GWP) for every country under CC BY 4.0, plus asset-level
# emissions and activity (power plants by fuel, smelters, mines). Its subsectors match some
# technology x stage cells cleanly (coal mining), some only partly (oil and gas production is one
# subsector) and many not at all (no battery or solar manufacturing subsector); the map states a
# match quality per cell, and cells with no match fall back to the IPCC lifecycle intensity of the
# technology (tech_ghg_tbl, data/raw/ipcc_ghg_intensity.csv), said so where shown.
#
# Pure (no IO). scripts/fetchers/climate_trace.R calls the API and stages
#   data/raw/climate_trace_country_emissions.csv  (climate_trace_parse_country())
#   data/raw/climate_trace_asset_activity.csv     (climate_trace_aggregate_assets())
# scripts/16_build_climate_trace.R builds data/processed/climate_trace_{country,assets,map,cells,
# lifecycle}_tbl.rds with the functions below; scripts/87 ships them in the sector-climate chunk.

CLIMATE_TRACE_API <- "https://api.climatetrace.org/v6"
CLIMATE_TRACE_MATCH <- c("direct", "partial", "proxy", "none")
CLIMATE_TRACE_ROLES <- c("own", "context")
CLIMATE_TRACE_BASES <- c("country", "assets")
CLIMATE_TRACE_GROUP_RULES <- c("primary_fuel", "asset_type", "all")
# /v6/assets answers at most this many assets per call; the fetch pages with `offset`.
CLIMATE_TRACE_ASSET_PAGE <- 10000L
CLIMATE_TRACE_COUNTRY_COLUMNS <- c("iso3", "sector", "subsector", "year", "co2e_100yr", "co2", "ch4", "n2o", "co2e_20yr")
CLIMATE_TRACE_ASSET_COLUMNS <- c("iso3", "subsector", "asset_group", "year", "n_assets", "n_activity", "co2e_100yr",
                                 "co2e_100yr_activity", "activity", "activity_unit", "capacity", "capacity_unit")

# ---- URLs ------------------------------------------------------------------------------------

# Country totals of one subsector in one year (since = to = year: a longer range is summed).
climate_trace_country_url <- function(subsector, year, countries, api = CLIMATE_TRACE_API) {
  paste0(api, "/country/emissions?since=", year, "&to=", year, "&subsectors=", subsector,
         "&countries=", paste(countries, collapse = ","))
}

# One page of the tracked assets of a subsector, with their emissions summary of `year`.
climate_trace_assets_url <- function(subsector, year, offset = 0L, limit = CLIMATE_TRACE_ASSET_PAGE, api = CLIMATE_TRACE_API) {
  paste0(api, "/assets?subsectors=", subsector, "&year=", year, "&limit=", limit, "&offset=", offset)
}

# ---- Parsers -----------------------------------------------------------------------------------

ct_num <- function(x) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) NA_real_ else as.numeric(x)

# /v6/country/emissions answer (jsonlite::fromJSON(simplifyVector = FALSE)) -> one row per country
# plus a WLD row from `worldEmissions` (every country, the same in every row of one answer).
climate_trace_parse_country <- function(x, subsector, sector, year) {
  if (!is.list(x) || !length(x)) stop("Climate TRACE ", subsector, " ", year, ": empty answer")
  if (!is.null(x$country)) x <- list(x)
  gases <- c("co2e_100yr", "co2", "ch4", "n2o", "co2e_20yr")
  row <- function(iso3, em) {
    v <- lapply(gases, function(g) ct_num(em[[g]]))
    names(v) <- gases
    tibble::as_tibble(c(list(iso3 = iso3), v))
  }
  rows <- lapply(x, function(r) row(as.character(r$country), r$emissions))
  worlds <- unique(vapply(x, function(r) ct_num(r$worldEmissions$co2e_100yr), numeric(1)))
  if (length(worlds) != 1L) stop("Climate TRACE ", subsector, " ", year, ": rows disagree on the world total")
  out <- dplyr::bind_rows(c(rows, list(row("WLD", x[[1]]$worldEmissions))))
  if (any(out$iso3 == "all")) stop("Climate TRACE ", subsector, " ", year, ": world aggregate without countries (no `countries` parameter?)")
  out$sector <- sector
  out$subsector <- subsector
  out$year <- as.integer(year)
  out[CLIMATE_TRACE_COUNTRY_COLUMNS]
}

# An asset's group (config fetch.asset_subsectors `group`).
climate_trace_asset_group <- function(asset_type, rule) {
  if (!rule %in% CLIMATE_TRACE_GROUP_RULES) stop("Unknown asset group rule: ", rule)
  t <- trimws(ifelse(is.na(asset_type), "", as.character(asset_type)))
  g <- switch(rule,
    all = rep("all", length(t)),
    asset_type = t,
    primary_fuel = tolower(trimws(sub(",.*$", "", t)))
  )
  ifelse(nzchar(g), g, "unknown")
}

# The assets of one /v6/assets page as rows: id, iso3, asset_type, co2e_100yr, activity,
# activity_unit, capacity, capacity_unit (the EmissionsSummary entry of `gas`). `assets` is the
# page's `assets` as jsonlite::fromJSON() simplifies it: a data frame whose EmissionsSummary column is
# a list of data frames (one row per gas).
climate_trace_asset_rows <- function(assets, gas = "co2e_100yr") {
  n <- if (is.data.frame(assets)) nrow(assets) else 0L
  if (!n) {
    return(tibble::tibble(id = character(), iso3 = character(), asset_type = character(), co2e_100yr = double(),
                          activity = double(), activity_unit = character(), capacity = double(), capacity_unit = character()))
  }
  pick <- function(field, num) {
    vapply(assets$EmissionsSummary, function(e) {
      if (!is.data.frame(e) || !nrow(e) || !"Gas" %in% names(e) || !field %in% names(e)) return(if (num) NA_real_ else NA_character_)
      v <- e[[field]][which(e$Gas == gas)[1]]
      if (num) ct_num(v) else if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
    }, if (num) numeric(1) else character(1))
  }
  col <- function(nm, default) if (nm %in% names(assets)) as.character(assets[[nm]]) else rep(default, n)
  tibble::tibble(
    id = col("Id", NA_character_), iso3 = col("Country", NA_character_), asset_type = col("AssetType", ""),
    co2e_100yr = pick("EmissionsQuantity", TRUE), activity = pick("Activity", TRUE), activity_unit = pick("ActivityUnits", FALSE),
    capacity = pick("Capacity", TRUE), capacity_unit = pick("CapacityUnits", FALSE)
  )
}

# Asset rows of one subsector and year -> one row per (iso3, asset group): the number of assets, the
# assets with an activity, their emissions (all, and those with an activity) and activity and
# capacity summed. An asset with no emissions value counts as an asset but adds nothing.
climate_trace_aggregate_assets <- function(rows, subsector, year, rule) {
  if (anyDuplicated(rows$id)) stop("Climate TRACE assets ", subsector, " ", year, ": an asset listed twice (paging overlap)")
  rows <- rows[!is.na(rows$iso3) & nzchar(rows$iso3), , drop = FALSE]
  rows$asset_group <- climate_trace_asset_group(rows$asset_type, rule)
  rows$has_activity <- is.finite(rows$activity) & rows$activity > 0
  units <- unique(rows$activity_unit[rows$has_activity])
  if (length(units) > 1L) stop("Climate TRACE assets ", subsector, " ", year, ": more than one activity unit (", paste(units, collapse = ", "), ")")
  cap_units <- unique(rows$capacity_unit[is.finite(rows$capacity)])
  sum0 <- function(x) if (any(is.finite(x))) sum(x[is.finite(x)]) else NA_real_
  # Helper columns first: inside summarise() a new column masks the input column of the same name.
  rows$em_act <- ifelse(rows$has_activity, rows$co2e_100yr, NA_real_)
  rows$act <- ifelse(rows$has_activity, rows$activity, NA_real_)
  out <- dplyr::summarise(
    dplyr::group_by(rows, .data$iso3, .data$asset_group),
    n_assets = dplyr::n(),
    n_activity = sum(.data$has_activity),
    co2e_100yr_activity = sum0(.data$em_act),
    activity = sum0(.data$act),
    capacity = sum0(.data$capacity),
    co2e_100yr = sum0(.data$co2e_100yr),
    .groups = "drop"
  )
  out$subsector <- subsector
  out$year <- as.integer(year)
  out$activity_unit <- if (length(units)) units else NA_character_
  out$capacity_unit <- if (length(cap_units) == 1L) cap_units else NA_character_
  out$n_assets <- as.integer(out$n_assets)
  out$n_activity <- as.integer(out$n_activity)
  out[CLIMATE_TRACE_ASSET_COLUMNS]
}

# ---- Map ------------------------------------------------------------------------------------------

# Every problem of the parsed config/climate_trace_map.yml (empty when valid). cells: the 32 cell
# slugs the map must cover (sector dictionary `cells`).
climate_trace_map_validate <- function(map, cells) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  f <- map$fetch
  if (is.null(f$years) || !length(f$years)) add("fetch.years is empty")
  cs <- vapply(f$country_subsectors %||% list(), function(s) s$subsector %||% NA_character_, character(1))
  as <- vapply(f$asset_subsectors %||% list(), function(s) s$subsector %||% NA_character_, character(1))
  rules <- vapply(f$asset_subsectors %||% list(), function(s) s$group %||% NA_character_, character(1))
  if (!length(cs) || anyNA(cs) || anyDuplicated(cs)) add("fetch.country_subsectors: missing or duplicate subsector")
  if (anyNA(as) || anyDuplicated(as)) add("fetch.asset_subsectors: missing or duplicate subsector")
  if (any(!rules %in% CLIMATE_TRACE_GROUP_RULES)) add("fetch.asset_subsectors: unknown group rule")
  if (length(setdiff(as, cs))) add("fetch.asset_subsectors not in country_subsectors: ", paste(setdiff(as, cs), collapse = ", "))
  missing <- setdiff(cells, names(map$cells))
  extra <- setdiff(names(map$cells), cells)
  if (length(missing)) add("cells missing from the map: ", paste(missing, collapse = ", "))
  if (length(extra)) add("cells not in the sector dictionary: ", paste(extra, collapse = ", "))
  for (cell in names(map$cells)) {
    e <- map$cells[[cell]]
    if (is.null(e$note) || !nzchar(e$note)) add(cell, ": no note")
    for (s in e$sources %||% list()) {
      w <- paste0(cell, " ", s$subsector %||% "?", ": ")
      if (!isTRUE(s$subsector %in% cs)) add(w, "subsector is not fetched")
      if (!isTRUE(s$role %in% CLIMATE_TRACE_ROLES)) add(w, "role must be own or context")
      basis <- s$basis %||% "country"
      if (!basis %in% CLIMATE_TRACE_BASES) add(w, "basis must be country or assets")
      if (identical(s$role, "own") && !isTRUE(s$match %in% setdiff(CLIMATE_TRACE_MATCH, "none"))) add(w, "an own source needs match direct, partial or proxy")
      if (identical(s$role, "context") && !is.null(s$match)) add(w, "a context source has no match quality")
      if (!is.null(s$asset_group)) {
        if (basis != "assets") add(w, "an asset group needs basis assets")
        if (!s$subsector %in% as) add(w, "asset group of a subsector whose assets are not fetched")
      }
      if (basis == "assets" && is.null(s$asset_group)) add(w, "basis assets needs an asset_group")
    }
  }
  p
}

# Source id of a (subsector, asset group): "coal-mining", "electricity-generation:coal".
climate_trace_src_id <- function(subsector, asset_group) ifelse(is.na(asset_group), subsector, paste0(subsector, ":", asset_group))

# The map as one row per (cell, source): cell, subsector, asset_group (NA = the whole subsector),
# basis, role, match (NA for context), src_id. Cells with no source have no row.
climate_trace_map_tbl <- function(map) {
  rows <- lapply(names(map$cells), function(cell) {
    ss <- map$cells[[cell]]$sources %||% list()
    if (!length(ss)) return(NULL)
    tibble::tibble(
      cell = cell,
      subsector = vapply(ss, function(s) s$subsector, character(1)),
      asset_group = vapply(ss, function(s) s$asset_group %||% NA_character_, character(1)),
      basis = vapply(ss, function(s) s$basis %||% "country", character(1)),
      role = vapply(ss, function(s) s$role, character(1)),
      match = vapply(ss, function(s) s$match %||% NA_character_, character(1)),
      order = seq_along(ss)
    )
  })
  out <- dplyr::bind_rows(rows)
  out$src_id <- climate_trace_src_id(out$subsector, out$asset_group)
  out
}

# The sources the map uses, one row each: src_id, subsector, sector, label (fetch.country_subsectors
# label, with the group label for an asset group), asset_group, basis.
climate_trace_sources_tbl <- function(map, map_tbl) {
  cs <- map$fetch$country_subsectors
  lab <- stats::setNames(vapply(cs, function(s) s$label, character(1)), vapply(cs, function(s) s$subsector, character(1)))
  sec <- stats::setNames(vapply(cs, function(s) s$sector, character(1)), names(lab))
  u <- unique(map_tbl[c("src_id", "subsector", "asset_group", "basis")])
  gl <- unlist(map$fetch$group_labels %||% list())
  u$sector <- unname(sec[u$subsector])
  u$label <- ifelse(is.na(u$asset_group), unname(lab[u$subsector]),
                    paste0(unname(lab[u$subsector]), ", ", ifelse(u$asset_group %in% names(gl), gl[u$asset_group], u$asset_group)))
  u[order(match(u$subsector, names(lab)), u$asset_group, na.last = FALSE), c("src_id", "subsector", "sector", "label", "asset_group", "basis")]
}

# One row per cell: cell, tech, stage, match (the best of its own sources; none without one), note.
climate_trace_cells_tbl <- function(map, map_tbl, cells) {
  rank <- match(map_tbl$match, CLIMATE_TRACE_MATCH)
  best <- vapply(cells$slug, function(s) {
    r <- rank[map_tbl$cell == s & map_tbl$role == "own"]
    if (length(r)) CLIMATE_TRACE_MATCH[min(r)] else "none"
  }, character(1), USE.NAMES = FALSE)
  tibble::tibble(cell = cells$slug, tech = cells$tech, stage = cells$stage, match = best,
                 note = vapply(cells$slug, function(s) map$cells[[s]]$note, character(1), USE.NAMES = FALSE))
}

# ---- Emissions and intensity ------------------------------------------------------------------

# Emissions per (src_id, iso3, year): country totals for basis country, the summed assets of the
# group for basis assets. WLD rows: Climate TRACE's world total (country basis) or the sum over every
# country's assets (assets basis).
climate_trace_emissions_tbl <- function(sources, country, assets) {
  out <- lapply(seq_len(nrow(sources)), function(i) {
    s <- sources[i, ]
    if (s$basis == "country") {
      d <- country[country$subsector == s$subsector, c("iso3", "year", "co2e_100yr")]
    } else {
      a <- assets[assets$subsector == s$subsector & assets$asset_group == s$asset_group & assets$iso3 != "WLD", c("iso3", "year", "co2e_100yr")]
      w <- dplyr::summarise(dplyr::group_by(a, .data$year), co2e_100yr = sum(.data$co2e_100yr, na.rm = TRUE), .groups = "drop")
      d <- dplyr::bind_rows(a, tibble::tibble(iso3 = "WLD", year = w$year, co2e_100yr = w$co2e_100yr))
    }
    d <- d[is.finite(d$co2e_100yr), , drop = FALSE]
    tibble::tibble(src_id = s$src_id, iso3 = d$iso3, year = as.integer(d$year), co2e_100yr = as.double(d$co2e_100yr))
  })
  out <- dplyr::bind_rows(out)
  out[order(out$src_id, out$year, out$iso3, method = "radix"), , drop = FALSE]
}

# Emissions intensity of the tracked assets per (subsector, asset group, iso3, year), WLD = every
# country's assets: their emissions over their activity, only assets that report an activity.
# intensity_unit: "t CO2e per MWh", "t CO2e per t of steel".
climate_trace_intensity_tbl <- function(assets) {
  a <- assets[assets$iso3 != "WLD" & is.finite(assets$activity) & assets$activity > 0 & is.finite(assets$co2e_100yr_activity), , drop = FALSE]
  w <- dplyr::summarise(dplyr::group_by(a, .data$subsector, .data$asset_group, .data$year, .data$activity_unit),
                        n_activity = sum(.data$n_activity), co2e_100yr_activity = sum(.data$co2e_100yr_activity),
                        activity = sum(.data$activity), .groups = "drop")
  w$iso3 <- "WLD"
  d <- dplyr::bind_rows(a[c("subsector", "asset_group", "iso3", "year", "activity_unit", "n_activity", "co2e_100yr_activity", "activity")], w)
  d$intensity <- d$co2e_100yr_activity / d$activity
  d$intensity_unit <- paste0("t CO2e per ", ifelse(d$activity_unit == "BBL", "barrel", d$activity_unit))
  d$year <- as.integer(d$year)
  d <- d[order(d$subsector, d$asset_group, d$year, d$iso3, method = "radix"), , drop = FALSE]
  d[c("subsector", "asset_group", "iso3", "year", "n_activity", "co2e_100yr_activity", "activity", "activity_unit", "intensity", "intensity_unit")]
}

# Subsector-years whose world total repeats the previous year's to within 1e-6 (relative; a measured
# change is never that small): Climate TRACE carried an estimate forward rather than re-estimating it
# (2025 copper, bauxite and iron-ore mining in the API of October 2026, copper off by 2 t of 87 Mt).
# One row per (subsector, year) with repeated = TRUE / FALSE.
climate_trace_repeats <- function(country) {
  w <- country[country$iso3 == "WLD", c("subsector", "year", "co2e_100yr")]
  w <- w[order(w$subsector, w$year), , drop = FALSE]
  prev <- stats::ave(w$co2e_100yr, w$subsector, FUN = function(x) c(NA, utils::head(x, -1)))
  w$repeated <- !is.na(prev) & abs(w$co2e_100yr - prev) <= 1e-6 * pmax(abs(prev), 1)
  w[c("subsector", "year", "repeated")]
}

# ---- IPCC lifecycle --------------------------------------------------------------------------------

# The IPCC rows behind each technology's ghg_index (the steps of partnership_strength_clean_ghg():
# its tech mapping, its oil correction `oil_g_per_kwh`, its min-max index over the seven technologies
# with their own rows and its proxies for the other four) as one row per technology: tech,
# ghg_index (from tech_ghg_tbl, the processed value), basis ("ipcc": own rows; "proxy"), g_min,
# g_max (gCO2e/kWh over the technology's rows; NA for a proxy), rows ("Coal (pulverised) 820"),
# proxy_of (the technologies a proxy averages). Stops when the rows do not reproduce tech_ghg_tbl.
climate_trace_lifecycle <- function(ipcc_raw, tech_ghg, oil_g_per_kwh) {
  require_columns(ipcc_raw, c("Tech", "ghg_intensity"), label = "ipcc_raw")
  require_columns(tech_ghg, c("tech", "ghg_index"), label = "tech_ghg")
  raw <- stringr::str_squish(stringr::str_replace_all(ipcc_raw$Tech, "[–—]", "-"))
  tech <- dplyr::case_when(
    raw == "Coal (pulverised)" ~ "Coal",
    raw == "Natural-gas combined cycle" ~ "Gas",
    raw == "Oil-fired steam" ~ "Oil",
    stringr::str_detect(raw, "^Solar PV") ~ "Solar",
    stringr::str_detect(raw, "Concentrated solar power") ~ "Solar",
    stringr::str_detect(raw, "^Wind") ~ "Wind",
    raw == "Nuclear (light-water)" ~ "Nuclear",
    TRUE ~ raw
  )
  g <- ifelse(tech == "Oil", oil_g_per_kwh, as.numeric(ipcc_raw$ghg_intensity))
  own <- c("Coal", "Gas", "Oil", "Solar", "Nuclear", "Wind", "Geothermal")
  keep <- tech %in% own
  rows <- tibble::tibble(tech = tech[keep], raw = raw[keep], g = g[keep])
  rows$index <- (max(rows$g) - rows$g) / (max(rows$g) - min(rows$g))
  idx <- tapply(rows$index, rows$tech, mean)
  proxy_of <- list(Batteries = "Solar", `Electric Vehicles` = c("Coal", "Oil", "Gas", "Solar", "Wind", "Nuclear"),
                   `Green Hydrogen` = c("Gas", "Solar"), `Electric Grid` = c("Solar", "Wind"))
  techs <- tech_ghg$tech
  out <- tibble::tibble(
    tech = techs,
    ghg_index = as.double(tech_ghg$ghg_index),
    basis = ifelse(techs %in% own, "ipcc", "proxy"),
    g_min = vapply(techs, function(t) if (t %in% own) min(rows$g[rows$tech == t]) else NA_real_, numeric(1), USE.NAMES = FALSE),
    g_max = vapply(techs, function(t) if (t %in% own) max(rows$g[rows$tech == t]) else NA_real_, numeric(1), USE.NAMES = FALSE),
    rows = vapply(techs, function(t) if (t %in% own) paste(paste(rows$raw[rows$tech == t], format(rows$g[rows$tech == t], trim = TRUE)), collapse = "; ") else NA_character_, character(1), USE.NAMES = FALSE),
    proxy_of = vapply(techs, function(t) if (!is.null(proxy_of[[t]])) paste(proxy_of[[t]], collapse = "|") else NA_character_, character(1), USE.NAMES = FALSE)
  )
  want <- vapply(techs, function(t) if (t %in% own) unname(idx[t]) else mean(idx[proxy_of[[t]]]), numeric(1), USE.NAMES = FALSE)
  if (any(is.na(out$basis[out$basis == "proxy"])) || !isTRUE(all.equal(want, out$ghg_index, tolerance = 1e-12))) {
    stop("climate_trace_lifecycle(): the IPCC rows do not reproduce tech_ghg_tbl's ghg_index")
  }
  out
}
