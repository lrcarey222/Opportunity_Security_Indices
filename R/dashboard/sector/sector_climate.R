# sector-climate (user request of 2026-10-17): Climate TRACE sector emissions mapped to technology x
# stage, with the IPCC lifecycle intensity for the technologies, for the Sector Profile's Climate lens
# and section and the Country Profile's Policy & climate section. Optional, lazily loaded chunk
# (manifest flags.climate; contract CLIMATE_SPEC in dashboard/src/sector/contract.ts). Pure.
#
# scripts/87 reads data/processed/climate_trace_*_tbl.rds (scripts/16_build_climate_trace.R; rules in
# R/categories/climate/climate_trace.R; map in config/climate_trace_map.yml) and ships them as they
# are, restricted to the economies of dims.geo (WLD is Climate TRACE's world total, so the economies
# outside dims.geo stay in it). After the decode every shipped value is checked identical() to the
# processed table (sector_climate_parity()). The lifecycle table's ghg_index column (the paper's
# technology weight) is not shipped: the views show the IPCC gCO2e/kWh, a fact, and name the proxy
# technologies where the paper has no IPCC row.
#
# Tables (text-keyed part -> chunk):
#   climate.src        one row per source the map uses: id ("coal-mining", "electricity-generation:coal"),
#                      subsector, sector, label, asset_group (NA = the whole subsector), basis (country:
#                      Climate TRACE's country totals; assets: the summed tracked assets of the group),
#                      repeated (the latest year's world total equals the previous year's: an estimate
#                      carried forward)
#   climate.map        (cell item, src row, role own / context, match direct / partial / proxy (NA for
#                      context), order)
#   climate.cells      one row per cell: match (direct / partial / proxy / none), note
#   climate.lifecycle  one row per technology: basis (ipcc / proxy), g_min, g_max (gCO2e/kWh; NA for a
#                      proxy), rows (the IPCC rows), proxy_of ("Solar|Wind")
#   climate.em         t CO2e (100-year GWP) per (src row, geo, year)
#   climate.intensity  the tracked assets' emissions over their activity per (subsector, asset group,
#                      geo, year): n (assets with an activity), emissions, activity, value, unit,
#                      activity_unit

SECTOR_CLIMATE_V <- list(
  basis = c("country", "assets"),
  role = c("own", "context"),
  match = c("direct", "partial", "proxy"),
  cell_match = c("direct", "partial", "proxy", "none"),
  lifecycle_basis = c("ipcc", "proxy")
)

# The processed tables scripts/87 reads (names without _tbl.rds).
SECTOR_CLIMATE_PROCESSED <- c(sources = "climate_trace_sources_tbl", map = "climate_trace_map_tbl", cells = "climate_trace_cells_tbl",
                              lifecycle = "climate_trace_lifecycle_tbl", emissions = "climate_trace_emissions_tbl",
                              intensity = "climate_trace_intensity_tbl", country = "climate_trace_country_tbl")

# The text-keyed part from the processed tables (`p`, named as SECTOR_CLIMATE_PROCESSED) and the
# dims.geo codes. Also returns `outside`: per (src, year), the share of the world total from economies
# outside dims.geo (a validation note), and `year`: the latest year.
sector_climate_part <- function(p, geo_codes) {
  for (nm in names(SECTOR_CLIMATE_PROCESSED)) if (is.null(p[[nm]])) stop("sector_climate_part(): missing ", nm)
  latest <- max(p$emissions$year)
  rep <- p$country[p$country$iso3 == "WLD" & p$country$year == latest, c("subsector", "repeated")]
  src <- tibble::tibble(id = p$sources$src_id, subsector = p$sources$subsector, sector = p$sources$sector, label = p$sources$label,
                        asset_group = p$sources$asset_group, basis = p$sources$basis,
                        repeated = rep$repeated[match(p$sources$subsector, rep$subsector)] %in% TRUE)
  em <- p$emissions[p$emissions$iso3 %in% geo_codes, , drop = FALSE]
  it <- p$intensity[p$intensity$iso3 %in% geo_codes, , drop = FALSE]
  w <- p$emissions[p$emissions$iso3 == "WLD", c("src_id", "year", "co2e_100yr")]
  s <- dplyr::summarise(dplyr::group_by(em[em$iso3 != "WLD", ], .data$src_id, .data$year), shipped = sum(.data$co2e_100yr), .groups = "drop")
  outside <- dplyr::left_join(w, s, by = c("src_id", "year"))
  outside$share_outside <- ifelse(outside$co2e_100yr > 0, 1 - dplyr::coalesce(outside$shipped, 0) / outside$co2e_100yr, NA_real_)
  list(
    src = src,
    map = tibble::tibble(item_key = paste0("cell:", p$map$cell), src_id = p$map$src_id, role = p$map$role, match = p$map$match,
                         order = as.integer(p$map$order)),
    cells = tibble::tibble(item_key = paste0("cell:", p$cells$cell), match = p$cells$match, note = p$cells$note),
    lifecycle = tibble::tibble(tech = p$lifecycle$tech, basis = p$lifecycle$basis, g_min = p$lifecycle$g_min, g_max = p$lifecycle$g_max,
                               rows = p$lifecycle$rows, proxy_of = p$lifecycle$proxy_of),
    em = tibble::tibble(src_id = em$src_id, geo = em$iso3, year = as.integer(em$year), value = as.double(em$co2e_100yr)),
    intensity = tibble::tibble(subsector = it$subsector, asset_group = it$asset_group, geo = it$iso3, year = as.integer(it$year),
                               n = as.integer(it$n_activity), emissions = as.double(it$co2e_100yr_activity), activity = as.double(it$activity),
                               value = as.double(it$intensity), unit = it$intensity_unit, activity_unit = it$activity_unit),
    outside = outside,
    year = latest
  )
}

# The sector-climate chunk (contract CLIMATE_SPEC) from the text-keyed part. gi / ii: geo / item code
# -> 0-based index.
sector_climate_chunk <- function(cl, gi, ii) {
  si <- function(id) {
    i <- match(id, cl$src$id)
    if (anyNA(i)) stop("sector_climate_chunk(): unknown source ", paste(utils::head(unique(id[is.na(i)]), 5), collapse = ", "))
    as.integer(i - 1L)
  }
  src <- tibble::tibble(id = as.character(cl$src$id), subsector = as.character(cl$src$subsector), sector = as.character(cl$src$sector),
                        label = as.character(cl$src$label), asset_group = as.character(cl$src$asset_group), basis = as.character(cl$src$basis),
                        repeated = as.logical(cl$src$repeated))
  m <- tibble::tibble(item = ii(cl$map$item_key), src = si(cl$map$src_id), role = as.character(cl$map$role), match = as.character(cl$map$match),
                      order = as.integer(cl$map$order))
  m <- m[order(m$item, m$order, method = "radix"), , drop = FALSE]
  cc <- tibble::tibble(item = ii(cl$cells$item_key), match = as.character(cl$cells$match), note = as.character(cl$cells$note))
  cc <- cc[order(cc$item, method = "radix"), , drop = FALSE]
  lc <- tibble::tibble(tech = as.character(cl$lifecycle$tech), basis = as.character(cl$lifecycle$basis), g_min = as.double(cl$lifecycle$g_min),
                       g_max = as.double(cl$lifecycle$g_max), rows = as.character(cl$lifecycle$rows), proxy_of = as.character(cl$lifecycle$proxy_of))
  em <- tibble::tibble(src = si(cl$em$src_id), geo = gi(cl$em$geo), year = as.integer(cl$em$year), value = as.double(cl$em$value))
  em <- em[order(em$src, em$year, em$geo, method = "radix"), , drop = FALSE]
  it <- cl$intensity
  int <- tibble::tibble(subsector = as.character(it$subsector), asset_group = as.character(it$asset_group), geo = gi(it$geo), year = as.integer(it$year),
                        n = as.integer(it$n), emissions = as.double(it$emissions), activity = as.double(it$activity), value = as.double(it$value),
                        unit = as.character(it$unit), activity_unit = as.character(it$activity_unit))
  int <- int[order(int$subsector, int$asset_group, int$year, int$geo, method = "radix"), , drop = FALSE]
  out <- list(src = src, map = m, cells = cc, lifecycle = lc, em = em, intensity = int)
  # The IEA pathways (optional: scripts/87 sets cl$nze when the iea_pathways_*_tbl.rds of scripts/17 exist).
  if (!is.null(cl$nze)) out <- c(out, sector_nze_tables(cl$nze, ii))
  list(climate = out)
}

# The R mirror of the contract's CLIMATE_SPEC (sector_chunk_specs() adds it).
sector_climate_spec <- function() {
  c(list(
    `climate.src` = sector_tbl_spec(c(id = "s!", subsector = "s!", sector = "s!", label = "s!", asset_group = "s", basis = "s!", repeated = "b!"),
                                    unique = "id", values = list(basis = SECTOR_CLIMATE_V$basis)),
    `climate.map` = sector_tbl_spec(c(item = "i!@items", src = "i!", role = "s!", match = "s", order = "i!"),
                                    sorted = c("item", "order"), unique = c("item", "src"),
                                    values = list(role = SECTOR_CLIMATE_V$role, match = SECTOR_CLIMATE_V$match)),
    `climate.cells` = sector_tbl_spec(c(item = "i!@items", match = "s!", note = "s!"), sorted = "item", unique = "item",
                                      values = list(match = SECTOR_CLIMATE_V$cell_match)),
    `climate.lifecycle` = sector_tbl_spec(c(tech = "s!", basis = "s!", g_min = "f", g_max = "f", rows = "s", proxy_of = "s"), unique = "tech",
                                          values = list(tech = SECTOR_V$tech_label, basis = SECTOR_CLIMATE_V$lifecycle_basis)),
    `climate.em` = sector_tbl_spec(c(src = "i!", geo = "i!@geo", year = "i!", value = "f!"), sorted = c("src", "year", "geo"),
                                   unique = c("src", "year", "geo")),
    `climate.intensity` = sector_tbl_spec(c(subsector = "s!", asset_group = "s!", geo = "i!@geo", year = "i!", n = "i!", emissions = "f!",
                                            activity = "f!", value = "f!", unit = "s!", activity_unit = "s!"),
                                          unique = c("subsector", "asset_group", "year", "geo"))
  ), sector_nze_spec())
}

# Cross-table rules of sector-climate (contract checkClimate()): source rows in range; a context row has
# no match, an own row has one; a cell's match is the best of its own rows (none without one); an
# assets source names its group; a proxy lifecycle row has proxy_of and no gCO2e range; intensity =
# emissions / activity with activity > 0.
sector_check_climate <- function(d) {
  e <- character()
  ns <- nrow(d$src)
  if (any(d$map$src >= ns) || any(d$em$src >= ns)) e <- c(e, "climate: a src index past climate.src")
  m <- d$map
  if (any(is.na(m$match) != (m$role == "context"))) e <- c(e, "climate.map: match must be set exactly on own rows")
  rank <- match(m$match, SECTOR_CLIMATE_V$cell_match)
  want <- vapply(d$cells$item, function(i) {
    r <- rank[m$item == i & m$role == "own"]
    if (length(r)) SECTOR_CLIMATE_V$cell_match[min(r)] else "none"
  }, character(1))
  if (!identical(unname(want), d$cells$match)) e <- c(e, "climate.cells.match is not the best match of the cell's own sources")
  if (!all(m$item %in% d$cells$item)) e <- c(e, "climate.map has a cell without a climate.cells row")
  s <- d$src
  if (any((s$basis == "assets") != !is.na(s$asset_group))) e <- c(e, "climate.src: an assets source needs an asset_group, a country source none")
  l <- d$lifecycle
  if (any((l$basis == "proxy") != !is.na(l$proxy_of)) || any(l$basis == "proxy" & !is.na(l$g_min))) e <- c(e, "climate.lifecycle: proxy rows need proxy_of and no gCO2e range")
  if (any(l$basis == "ipcc" & (is.na(l$g_min) | is.na(l$g_max) | l$g_min > l$g_max))) e <- c(e, "climate.lifecycle: ipcc rows need g_min <= g_max")
  if (any(!is.finite(d$em$value))) e <- c(e, "climate.em.value has non-finite values")
  it <- d$intensity
  if (any(!(it$activity > 0)) || any(abs(it$value - it$emissions / it$activity) > 1e-12 * pmax(abs(it$value), 1e-300))) e <- c(e, "climate.intensity: value is not emissions / activity")
  c(e, sector_check_nze(d))
}

# Decoded sector-climate against the processed tables (`p`, as sector_climate_part() takes them):
# every shipped value identical() to the processed row of the same key, and every processed row of a
# dims.geo economy shipped. Returns check rows (check, status, n, detail).
sector_climate_parity <- function(decoded, meta, p) {
  d <- decoded$climate
  geo <- meta$dims$geo$code
  it <- meta$dims$items
  key <- paste0(it$axis, ":", it$slug)
  src_id <- d$src$id
  rows <- list()
  chk <- function(check, ok, n, detail = NA_character_) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(check = check, status = if (ok) "PASS" else "FAIL", n = as.integer(n), detail = as.character(detail))
  }
  # Emissions.
  got <- tibble::tibble(src_id = src_id[d$em$src + 1L], iso3 = geo[d$em$geo + 1L], year = d$em$year, co2e_100yr = d$em$value)
  ref <- p$emissions[p$emissions$iso3 %in% geo, c("src_id", "iso3", "year", "co2e_100yr")]
  ord <- function(x) x[order(x$src_id, x$year, x$iso3, method = "radix"), , drop = FALSE]
  a <- as.data.frame(ord(got))
  b <- as.data.frame(ord(tibble::tibble(src_id = ref$src_id, iso3 = ref$iso3, year = as.integer(ref$year), co2e_100yr = as.double(ref$co2e_100yr))))
  rownames(a) <- rownames(b) <- NULL
  chk("climate (decoded): emissions identical() to climate_trace_emissions_tbl (dims.geo economies and WLD)", identical(a, b), nrow(a))
  # Intensity.
  gi <- tibble::tibble(subsector = d$intensity$subsector, asset_group = d$intensity$asset_group, iso3 = geo[d$intensity$geo + 1L],
                       year = d$intensity$year, n_activity = d$intensity$n, co2e_100yr_activity = d$intensity$emissions,
                       activity = d$intensity$activity, activity_unit = d$intensity$activity_unit, intensity = d$intensity$value,
                       intensity_unit = d$intensity$unit)
  ri <- p$intensity[p$intensity$iso3 %in% geo, names(gi)]
  ordi <- function(x) as.data.frame(x[order(x$subsector, x$asset_group, x$year, x$iso3, method = "radix"), , drop = FALSE])
  a <- ordi(gi)
  b <- ordi(tibble::tibble(subsector = ri$subsector, asset_group = ri$asset_group, iso3 = ri$iso3, year = as.integer(ri$year),
                           n_activity = as.integer(ri$n_activity), co2e_100yr_activity = as.double(ri$co2e_100yr_activity),
                           activity = as.double(ri$activity), activity_unit = ri$activity_unit, intensity = as.double(ri$intensity),
                           intensity_unit = ri$intensity_unit))
  rownames(a) <- rownames(b) <- NULL
  chk("climate (decoded): intensity identical() to climate_trace_intensity_tbl", identical(a, b), nrow(a))
  # Map, cells, lifecycle, sources.
  gm <- tibble::tibble(cell = sub("^cell:", "", key[d$map$item + 1L]), src_id = src_id[d$map$src + 1L], role = d$map$role, match = d$map$match, order = d$map$order)
  om <- function(x) as.data.frame(x[order(x$cell, x$order, method = "radix"), c("cell", "src_id", "role", "match", "order"), drop = FALSE])
  a <- om(gm)
  b <- om(tibble::tibble(cell = p$map$cell, src_id = p$map$src_id, role = p$map$role, match = p$map$match, order = as.integer(p$map$order)))
  rownames(a) <- rownames(b) <- NULL
  chk("climate (decoded): cell map identical() to climate_trace_map_tbl", identical(a, b), nrow(a))
  gc <- tibble::tibble(cell = sub("^cell:", "", key[d$cells$item + 1L]), match = d$cells$match, note = d$cells$note)
  rc <- p$cells[match(gc$cell, p$cells$cell), c("cell", "match", "note")]
  chk("climate (decoded): cell match quality and notes identical() to climate_trace_cells_tbl",
      nrow(gc) == nrow(p$cells) && identical(as.data.frame(gc), as.data.frame(tibble::tibble(cell = rc$cell, match = rc$match, note = rc$note))), nrow(gc))
  lc <- p$lifecycle
  chk("climate (decoded): IPCC lifecycle gCO2e/kWh identical() to climate_trace_lifecycle_tbl",
      identical(d$lifecycle$tech, lc$tech) && identical(d$lifecycle$g_min, as.double(lc$g_min)) && identical(d$lifecycle$g_max, as.double(lc$g_max)) &&
        identical(d$lifecycle$basis, lc$basis) && identical(d$lifecycle$proxy_of, lc$proxy_of), nrow(lc))
  chk("climate (decoded): sources identical() to climate_trace_sources_tbl",
      identical(d$src$id, p$sources$src_id) && identical(d$src$label, p$sources$label) && identical(d$src$basis, p$sources$basis) &&
        identical(d$src$asset_group, as.character(p$sources$asset_group)), nrow(d$src))
  dplyr::bind_rows(rows)
}

# ---- IEA pathways (climate.nze_*; user request of 2026-10-17: IEA Net Zero data) -----------------------
#
# Optional tables of sector-climate: the IEA's World pathways (WEO Annex A free dataset: Historical, Stated
# Policies, Current Policies, Net Zero Emissions by 2050) and mineral demand by scenario (IEA Critical
# Minerals Dataset: Current Policies, Stated Policies, High Demand; no Net Zero scenario), mapped to
# technology x stage. scripts/87 reads data/processed/iea_pathways_*_tbl.rds (scripts/17_build_iea_pathways.R;
# rules in R/categories/climate/iea_pathways.R; map in config/iea_pathways_map.yml) and ships them as they
# are; after the decode every value is checked identical() to the processed table (sector_nze_parity()).
# World only: no country value. Every value is an IEA scenario projection or the IEA's historical estimate.
#
#   climate.nze_series    one row per series: id, grp (deployment / power / fuel / emissions), label, unit,
#                         category, product, flow (the WEO Annex A row it reads)
#   climate.nze_values    (series row, scenario historical / stated / current / nze, year, value), World
#   climate.nze_map       (cell item, series row, role own / demand / context, match (NA for context), order)
#   climate.nze_cells     one row per cell: match (direct / partial / proxy / none), note, mineral_techs
#                         (the Critical Minerals technologies the cell's mineral demand sums, "|"-separated)
#   climate.nze_minerals  (sheet 1 / 3.1 / 3.2, mineral, tech, scenario current / stated / high_demand, year,
#                         kt) as published; slug: the explorer's mineral item where one matches (NA otherwise)
#   climate.nze_meta      key, value: weo (edition), weo_file, weo_previous (the vintage checked against),
#                         minerals_file, minerals_base_year

SECTOR_NZE_V <- list(
  grp = c("deployment", "power", "fuel", "emissions"),
  scenario = c("historical", "stated", "current", "nze"),
  mineral_scenario = c("current", "stated", "high_demand"),
  role = c("own", "demand", "context"),
  match = c("direct", "partial", "proxy"),
  cell_match = c("direct", "partial", "proxy", "none"),
  sheet = c("1", "3.1", "3.2")
)

SECTOR_NZE_PROCESSED <- c(series = "iea_pathways_series_tbl", values = "iea_pathways_values_tbl", map = "iea_pathways_map_tbl",
                          cells = "iea_pathways_cells_tbl", minerals = "iea_pathways_minerals_tbl", meta = "iea_pathways_meta_tbl")

# The text-keyed nze part from the processed tables (`p`, named as SECTOR_NZE_PROCESSED).
sector_nze_part <- function(p) {
  for (nm in names(SECTOR_NZE_PROCESSED)) if (is.null(p[[nm]])) stop("sector_nze_part(): missing ", nm)
  list(
    series = p$series, values = p$values,
    map = tibble::tibble(item_key = paste0("cell:", p$map$cell), series = p$map$series, role = p$map$role, match = p$map$match, order = as.integer(p$map$order)),
    cells = tibble::tibble(item_key = paste0("cell:", p$cells$cell), match = p$cells$match, note = p$cells$note, mineral_techs = p$cells$mineral_techs),
    minerals = p$minerals, meta = p$meta
  )
}

# The climate.nze_* tables (contract CLIMATE_SPEC) from the nze part. ii: item code -> 0-based index.
sector_nze_tables <- function(n, ii) {
  ser <- tibble::tibble(id = as.character(n$series$id), grp = as.character(n$series$group), label = as.character(n$series$label),
                        unit = as.character(n$series$unit), category = as.character(n$series$category), product = as.character(n$series$product),
                        flow = as.character(n$series$flow))
  si <- function(id) {
    i <- match(id, ser$id)
    if (anyNA(i)) stop("sector_nze_tables(): unknown series ", paste(utils::head(unique(id[is.na(i)]), 5), collapse = ", "))
    as.integer(i - 1L)
  }
  val <- tibble::tibble(series = si(n$values$series), scenario = as.character(n$values$scenario), year = as.integer(n$values$year),
                        value = as.double(n$values$value))
  val <- val[order(val$series, method = "radix"), , drop = FALSE]
  m <- tibble::tibble(item = ii(n$map$item_key), series = si(n$map$series), role = as.character(n$map$role), match = as.character(n$map$match),
                      order = as.integer(n$map$order))
  m <- m[order(m$item, m$order, method = "radix"), , drop = FALSE]
  cc <- tibble::tibble(item = ii(n$cells$item_key), match = as.character(n$cells$match), note = as.character(n$cells$note),
                       mineral_techs = as.character(n$cells$mineral_techs))
  cc <- cc[order(cc$item, method = "radix"), , drop = FALSE]
  mi <- n$minerals
  mn <- tibble::tibble(sheet = as.character(mi$sheet), mineral = as.character(mi$mineral), tech = as.character(mi$tech),
                       scenario = as.character(mi$scenario), year = as.integer(mi$year), kt = as.double(mi$kt),
                       slug = as.character(sector_mineral_slug(mi$mineral)))
  meta <- tibble::tibble(key = as.character(n$meta$key), value = as.character(n$meta$value))
  list(nze_series = ser, nze_values = val, nze_map = m, nze_cells = cc, nze_minerals = mn, nze_meta = meta)
}

# The R mirror of the contract's climate.nze_* specs (optional tables of CLIMATE_SPEC).
sector_nze_spec <- function() {
  list(
    `climate.nze_series` = sector_tbl_spec(c(id = "s!", grp = "s!", label = "s!", unit = "s!", category = "s!", product = "s!", flow = "s!"),
                                           unique = "id", values = list(grp = SECTOR_NZE_V$grp), optional = TRUE),
    `climate.nze_values` = sector_tbl_spec(c(series = "i!", scenario = "s!", year = "i!", value = "f!"), sorted = "series",
                                           unique = c("series", "scenario", "year"), values = list(scenario = SECTOR_NZE_V$scenario), optional = TRUE),
    `climate.nze_map` = sector_tbl_spec(c(item = "i!@items", series = "i!", role = "s!", match = "s", order = "i!"), sorted = c("item", "order"),
                                        unique = c("item", "series"), values = list(role = SECTOR_NZE_V$role, match = SECTOR_NZE_V$match), optional = TRUE),
    `climate.nze_cells` = sector_tbl_spec(c(item = "i!@items", match = "s!", note = "s!", mineral_techs = "s"), sorted = "item", unique = "item",
                                          values = list(match = SECTOR_NZE_V$cell_match), optional = TRUE),
    `climate.nze_minerals` = sector_tbl_spec(c(sheet = "s!", mineral = "s!", tech = "s!", scenario = "s!", year = "i!", kt = "f!", slug = "s"),
                                             unique = c("sheet", "mineral", "tech", "scenario", "year"),
                                             values = list(sheet = SECTOR_NZE_V$sheet, scenario = SECTOR_NZE_V$mineral_scenario), optional = TRUE),
    `climate.nze_meta` = sector_tbl_spec(c(key = "s!", value = "s"), unique = "key", optional = TRUE)
  )
}

# Cross-table rules of the nze tables (contract checkClimate()): all or none present; series rows in range;
# match set exactly on own and demand rows; a cell's match is the best of its own and demand rows (none
# without one); the WEO values finite.
sector_check_nze <- function(d) {
  nm <- c("nze_series", "nze_values", "nze_map", "nze_cells", "nze_minerals", "nze_meta")
  have <- vapply(nm, function(x) !is.null(d[[x]]), logical(1))
  if (!any(have)) return(character())
  if (!all(have)) return("climate.nze_*: either every nze table or none")
  e <- character()
  ns <- nrow(d$nze_series)
  if (any(d$nze_values$series >= ns) || any(d$nze_map$series >= ns)) e <- c(e, "climate.nze: a series index past climate.nze_series")
  m <- d$nze_map
  if (any(is.na(m$match) != (m$role == "context"))) e <- c(e, "climate.nze_map: match must be set exactly on own and demand rows")
  rank <- match(m$match, SECTOR_NZE_V$cell_match)
  want <- vapply(d$nze_cells$item, function(i) {
    r <- rank[m$item == i & m$role != "context"]
    if (length(r)) SECTOR_NZE_V$cell_match[min(r)] else "none"
  }, character(1))
  if (!identical(unname(want), d$nze_cells$match)) e <- c(e, "climate.nze_cells.match is not the best match of the cell's own and demand series")
  if (!all(m$item %in% d$nze_cells$item)) e <- c(e, "climate.nze_map has a cell without a climate.nze_cells row")
  if (any(!is.finite(d$nze_values$value))) e <- c(e, "climate.nze_values.value has non-finite values")
  e
}

# Decoded nze tables against the processed iea_pathways_*_tbl.rds (`p`, as sector_nze_part() takes them):
# every shipped value identical() to the processed row of the same key. Returns check rows.
sector_nze_parity <- function(decoded, meta, p) {
  d <- decoded$climate
  rows <- list()
  chk <- function(check, ok, n) rows[[length(rows) + 1L]] <<- tibble::tibble(check = check, status = if (ok) "PASS" else "FAIL", n = as.integer(n), detail = NA_character_)
  if (is.null(d$nze_values)) return(tibble::tibble(check = character(), status = character(), n = integer(), detail = character()))
  it <- meta$dims$items
  key <- paste0(it$axis, ":", it$slug)
  sid <- d$nze_series$id
  df <- function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    rownames(x) <- NULL
    x
  }
  a <- tibble::tibble(series = sid[d$nze_values$series + 1L], scenario = d$nze_values$scenario, year = d$nze_values$year, value = d$nze_values$value)
  o <- function(x) df(x[order(x$series, x$scenario, x$year, method = "radix"), , drop = FALSE])
  b <- tibble::tibble(series = p$values$series, scenario = p$values$scenario, year = as.integer(p$values$year), value = as.double(p$values$value))
  chk("climate (decoded): IEA pathway values identical() to iea_pathways_values_tbl (World)", identical(o(a), o(b)), nrow(a))
  s <- p$series
  chk("climate (decoded): IEA pathway series identical() to iea_pathways_series_tbl",
      identical(df(tibble::tibble(id = sid, group = d$nze_series$grp, label = d$nze_series$label, unit = d$nze_series$unit, category = d$nze_series$category,
                                  product = d$nze_series$product, flow = d$nze_series$flow)),
                df(s[c("id", "group", "label", "unit", "category", "product", "flow")])), nrow(s))
  gm <- tibble::tibble(cell = sub("^cell:", "", key[d$nze_map$item + 1L]), series = sid[d$nze_map$series + 1L], role = d$nze_map$role, match = d$nze_map$match, order = d$nze_map$order)
  om <- function(x) df(x[order(x$cell, x$order, method = "radix"), c("cell", "series", "role", "match", "order"), drop = FALSE])
  chk("climate (decoded): IEA pathway cell map identical() to iea_pathways_map_tbl",
      identical(om(gm), om(tibble::tibble(cell = p$map$cell, series = p$map$series, role = p$map$role, match = p$map$match, order = as.integer(p$map$order)))), nrow(gm))
  gc <- tibble::tibble(cell = sub("^cell:", "", key[d$nze_cells$item + 1L]), match = d$nze_cells$match, note = d$nze_cells$note, mineral_techs = d$nze_cells$mineral_techs)
  rc <- p$cells[match(gc$cell, p$cells$cell), c("cell", "match", "note", "mineral_techs")]
  chk("climate (decoded): IEA pathway cell match quality, notes and mineral technologies identical() to iea_pathways_cells_tbl",
      nrow(gc) == nrow(p$cells) && identical(df(gc), df(rc)), nrow(gc))
  mm <- d$nze_minerals
  om2 <- function(x) df(x[order(x$sheet, x$mineral, x$tech, x$scenario, x$year, method = "radix"), c("sheet", "mineral", "tech", "scenario", "year", "kt"), drop = FALSE])
  pm <- p$minerals
  chk("climate (decoded): mineral demand by scenario identical() to iea_pathways_minerals_tbl",
      identical(om2(mm), om2(tibble::tibble(sheet = pm$sheet, mineral = pm$mineral, tech = pm$tech, scenario = pm$scenario, year = as.integer(pm$year), kt = as.double(pm$kt)))), nrow(mm))
  chk("climate (decoded): IEA pathway editions identical() to iea_pathways_meta_tbl",
      identical(d$nze_meta$key, p$meta$key) && identical(d$nze_meta$value, as.character(p$meta$value)), nrow(d$nze_meta))
  dplyr::bind_rows(rows)
}
