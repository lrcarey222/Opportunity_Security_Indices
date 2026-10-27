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

# ---- sector-climate-src: climate sources (user request of 2026-10-17: seven files added to data/raw) -------
#
# A lazy chunk of its own (manifest flags.climate_src; contract CLIMATE_SRC_SPEC; too large to join
# sector-climate within its 300 kB budget): the Global Carbon Project's national fossil CO2
# (territorial, consumption-based, transfers; Mt CO2) and World fossil CO2 by fuel (Gt CO2), the Energy
# Institute's Country Transition Tracker with the median economy, the IEA's WEO 2025 regional pathways
# (Historical, Stated Policies, Current Policies; regions have no Net Zero scenario) with the economy ->
# region map, the IEA's Net Zero by 2050 (2021) series paired with the WEO 2025 World series, and the IEA's
# World Energy Investment 2026 by region, with the cell map (WEI rows: own / demand with a match quality;
# GCP fuels: context). scripts/87 reads data/processed/climate_src_*_tbl.rds (scripts/18_build_climate_sources.R;
# rules in R/categories/climate/climate_sources.R; map in config/climate_sources_map.yml), keeps the economies
# of dims.geo, and ships the rest as they are; after the decode every value is checked identical() to the
# processed table (sector_src_parity()).
#
#   csrc.gcp            (geo, measure territorial / consumption / transfers, year, value Mt CO2)
#   csrc.gcp_fuel       (fuel, year, value Gt CO2), World
#   csrc.ei             (geo, ind, year, value) as published
#   csrc.ei_ind         id, label, unit, better (lower / higher / none), sheet, low_carbon
#   csrc.ei_median      (ind, year, value, n): the median of every economy the tracker lists
#   csrc.weo_series     id, label, unit, category, product, flow
#   csrc.weo_values     (region, series row, scenario, year, value)
#   csrc.weo_regions    region, kind (country / union / region / world)
#   csrc.weo_geo        (geo, region, match own / member / by_subregion)
#   csrc.nze21_series   id, label, unit, category, product, flow, weo (paired WEO 2025 series id), match, note
#   csrc.nze21_values   (series row, year, value), World, Net Zero Emissions by 2050 (2021 edition)
#   csrc.wei_rows       id, label (as published), name, parent, unit
#   csrc.wei_values     (row, region, year, value, estimate)
#   csrc.wei_regions    region, label, kind (world / group / country / region)
#   csrc.map            (cell item, src wei / gcp, key, role own / demand / context, match, order)
#   csrc.cells          one row per cell: match (best WEI row; none), note
#   csrc.meta           key, value: editions and files

SECTOR_SRC_V <- list(
  measure = c("territorial", "consumption", "transfers"),
  fuel = c("total", "coal", "oil", "gas", "cement", "flaring", "other"),
  better = c("lower", "higher", "none"),
  scenario = c("historical", "stated", "current", "nze"),
  region_kind = c("country", "union", "region", "world"),
  geo_match = c("own", "member", "by_subregion"),
  wei_kind = c("world", "group", "country", "region"),
  nze21_match = c("direct", "partial"),
  src = c("wei", "gcp"),
  role = c("own", "demand", "context"),
  match = c("direct", "partial", "proxy"),
  cell_match = c("direct", "partial", "proxy", "none")
)

SECTOR_SRC_PROCESSED <- c(gcp_national = "climate_src_gcp_national_tbl", gcp_fuel = "climate_src_gcp_fuel_tbl", ei = "climate_src_ei_tbl",
                          ei_ind = "climate_src_ei_ind_tbl", ei_median = "climate_src_ei_median_tbl", weo_series = "climate_src_weo_series_tbl",
                          weo_values = "climate_src_weo_values_tbl", weo_regions = "climate_src_weo_regions_tbl", weo_geo = "climate_src_weo_geo_tbl",
                          nze21_series = "climate_src_nze21_series_tbl", nze21_values = "climate_src_nze21_values_tbl", wei_rows = "climate_src_wei_rows_tbl",
                          wei_values = "climate_src_wei_values_tbl", wei_regions = "climate_src_wei_regions_tbl", map = "climate_src_map_tbl",
                          cells = "climate_src_cells_tbl", meta = "climate_src_meta_tbl")

# The text-keyed part: the processed tables (`p`, named as SECTOR_SRC_PROCESSED), geo-keyed ones kept to the
# dims.geo codes. Also `dropped`: the GCP and EI economies outside dims.geo (a validation note).
sector_src_part <- function(p, geo_codes) {
  for (nm in names(SECTOR_SRC_PROCESSED)) if (is.null(p[[nm]])) stop("sector_src_part(): missing ", nm)
  out <- p
  out$gcp_national <- p$gcp_national[p$gcp_national$iso3 %in% geo_codes, , drop = FALSE]
  out$ei <- p$ei[p$ei$iso3 %in% geo_codes, , drop = FALSE]
  out$weo_geo <- p$weo_geo[p$weo_geo$iso3 %in% geo_codes, , drop = FALSE]
  out$map <- tibble::tibble(item_key = paste0("cell:", p$map$cell), src = p$map$src, key = p$map$key, role = p$map$role, match = p$map$match,
                            order = as.integer(p$map$order))
  out$cells <- tibble::tibble(item_key = paste0("cell:", p$cells$cell), match = p$cells$match, note = p$cells$note)
  out$dropped <- list(gcp = setdiff(unique(p$gcp_national$iso3), geo_codes), ei = setdiff(unique(p$ei$iso3), geo_codes))
  out
}

# The sector-climate-src chunk (contract CLIMATE_SRC_SPEC) from the part. gi / ii: geo / item code -> 0-based index.
sector_src_chunk <- function(s, gi, ii) {
  idx <- function(x, ids, what) {
    i <- match(x, ids)
    if (anyNA(i)) stop("sector_src_chunk(): unknown ", what, " ", paste(utils::head(unique(x[is.na(i)]), 5), collapse = ", "))
    as.integer(i - 1L)
  }
  ch <- function(x) as.character(x)
  srt <- function(d, ...) d[do.call(order, c(list(...), list(method = "radix"))), , drop = FALSE]
  g <- s$gcp_national
  gg <- gi(g$iso3)
  gcp <- srt(tibble::tibble(geo = gg, measure = ch(g$measure), year = as.integer(g$year), value = as.double(g$value)), gg, g$measure, g$year)
  f <- s$gcp_fuel
  fuel <- tibble::tibble(fuel = ch(f$fuel), year = as.integer(f$year), value = as.double(f$value))
  e <- s$ei
  eg <- gi(e$iso3)
  ei <- srt(tibble::tibble(geo = eg, ind = ch(e$ind), year = as.integer(e$year), value = as.double(e$value)), e$ind, e$year, eg)
  ind <- tibble::tibble(id = ch(s$ei_ind$id), label = ch(s$ei_ind$label), unit = ch(s$ei_ind$unit), better = ch(s$ei_ind$better), sheet = ch(s$ei_ind$sheet),
                        low_carbon = as.logical(s$ei_ind$low_carbon))
  med <- tibble::tibble(ind = ch(s$ei_median$ind), year = as.integer(s$ei_median$year), value = as.double(s$ei_median$value), n = as.integer(s$ei_median$n))
  ws <- s$weo_series
  wser <- tibble::tibble(id = ch(ws$id), label = ch(ws$label), unit = ch(ws$unit), category = ch(ws$category), product = ch(ws$product), flow = ch(ws$flow))
  wv <- s$weo_values
  wval <- tibble::tibble(region = ch(wv$region), series = idx(wv$series, wser$id, "WEO series"), scenario = ch(wv$scenario), year = as.integer(wv$year),
                         value = as.double(wv$value))
  wreg <- tibble::tibble(region = ch(s$weo_regions$region), kind = ch(s$weo_regions$kind))
  wg <- s$weo_geo
  wgg <- gi(wg$iso3)
  wgeo <- srt(tibble::tibble(geo = wgg, region = ch(wg$region), match = ch(wg$match)), wgg)
  ns <- s$nze21_series
  nser <- tibble::tibble(id = ch(ns$id), label = ch(ns$label), unit = ch(ns$unit), category = ch(ns$category), product = ch(ns$product), flow = ch(ns$flow),
                         weo = ch(ns$weo), match = ch(ns$match), note = ch(ns$note))
  nv <- s$nze21_values
  ni <- idx(nv$series, nser$id, "NZE 2021 series")
  nval <- srt(tibble::tibble(series = ni, year = as.integer(nv$year), value = as.double(nv$value)), ni, nv$year)
  wr <- s$wei_rows
  wrow <- tibble::tibble(id = ch(wr$id), label = ch(wr$label), name = ch(wr$name), parent = ch(wr$parent), unit = ch(wr$unit))
  iv <- s$wei_values
  ri <- idx(iv$row, wrow$id, "WEI row")
  ival <- srt(tibble::tibble(row = ri, region = ch(iv$region), year = as.integer(iv$year), value = as.double(iv$value), estimate = as.logical(iv$estimate)),
              ri, iv$region, iv$year)
  ireg <- tibble::tibble(region = ch(s$wei_regions$region), label = ch(s$wei_regions$label), kind = ch(s$wei_regions$kind))
  m <- s$map
  mi <- ii(m$item_key)
  mp <- srt(tibble::tibble(item = mi, src = ch(m$src), key = ch(m$key), role = ch(m$role), match = ch(m$match), order = as.integer(m$order)), mi, m$src, m$order)
  ci <- ii(s$cells$item_key)
  cc <- srt(tibble::tibble(item = ci, match = ch(s$cells$match), note = ch(s$cells$note)), ci)
  meta <- tibble::tibble(key = ch(s$meta$key), value = ch(s$meta$value))
  list(csrc = list(gcp = gcp, gcp_fuel = fuel, ei = ei, ei_ind = ind, ei_median = med, weo_series = wser, weo_values = wval,
                   weo_regions = wreg, weo_geo = wgeo, nze21_series = nser, nze21_values = nval, wei_rows = wrow, wei_values = ival,
                   wei_regions = ireg, map = mp, cells = cc, meta = meta))
}

# The R mirror of the contract's CLIMATE_SRC_SPEC (sector_chunk_specs() adds it).
sector_src_spec <- function() {
  V <- SECTOR_SRC_V
  t <- function(cols, ...) sector_tbl_spec(cols, ...)
  list(
    `csrc.gcp` = t(c(geo = "i!@geo", measure = "s!", year = "i!", value = "f!"), sorted = "geo", unique = c("geo", "measure", "year"),
                          values = list(measure = V$measure)),
    `csrc.gcp_fuel` = t(c(fuel = "s!", year = "i!", value = "f!"), unique = c("fuel", "year"), values = list(fuel = V$fuel)),
    `csrc.ei` = t(c(geo = "i!@geo", ind = "s!", year = "i!", value = "f!"), unique = c("ind", "year", "geo")),
    `csrc.ei_ind` = t(c(id = "s!", label = "s!", unit = "s!", better = "s!", sheet = "s!", low_carbon = "b"), unique = "id", values = list(better = V$better)),
    `csrc.ei_median` = t(c(ind = "s!", year = "i!", value = "f!", n = "i!"), unique = c("ind", "year")),
    `csrc.weo_series` = t(c(id = "s!", label = "s!", unit = "s!", category = "s!", product = "s!", flow = "s!"), unique = "id"),
    `csrc.weo_values` = t(c(region = "s!", series = "i!", scenario = "s!", year = "i!", value = "f!"), unique = c("region", "series", "scenario", "year"),
                                 values = list(scenario = V$scenario)),
    `csrc.weo_regions` = t(c(region = "s!", kind = "s!"), unique = "region", values = list(kind = V$region_kind)),
    `csrc.weo_geo` = t(c(geo = "i!@geo", region = "s!", match = "s!"), sorted = "geo", unique = "geo", values = list(match = V$geo_match)),
    `csrc.nze21_series` = t(c(id = "s!", label = "s!", unit = "s!", category = "s!", product = "s!", flow = "s!", weo = "s!", match = "s!", note = "s"),
                                   unique = "id", values = list(match = V$nze21_match)),
    `csrc.nze21_values` = t(c(series = "i!", year = "i!", value = "f!"), sorted = c("series", "year"), unique = c("series", "year")),
    `csrc.wei_rows` = t(c(id = "s!", label = "s!", name = "s!", parent = "s", unit = "s!"), unique = "id"),
    `csrc.wei_values` = t(c(row = "i!", region = "s!", year = "i!", value = "f!", estimate = "b!"), sorted = "row",
                                 unique = c("row", "region", "year")),
    `csrc.wei_regions` = t(c(region = "s!", label = "s!", kind = "s!"), unique = "region", values = list(kind = V$wei_kind)),
    `csrc.map` = t(c(item = "i!@items", src = "s!", key = "s!", role = "s!", match = "s", order = "i!"), sorted = "item",
                          unique = c("item", "src", "key"), values = list(src = V$src, role = V$role, match = V$match)),
    `csrc.cells` = t(c(item = "i!@items", match = "s!", note = "s!"), sorted = "item", unique = "item", values = list(match = V$cell_match)),
    `csrc.meta` = t(c(key = "s!", value = "s"), unique = "key")
  )
}

# Cross-table rules of sector-climate-src (contract checkClimateSrc()); `d` is the decoded csrc list: row indices in range;
# WEO values and the region map name regions of the region table; every EI indicator listed; a wei key of
# the map is a WEI row id and a gcp key a fuel; wei rows have a match and role own or demand, gcp rows are
# context without one; a cell's match is the best of its WEI rows (none without one); n is positive.
sector_check_src <- function(d) {
  e <- character()
  if (any(d$weo_values$series >= nrow(d$weo_series))) e <- c(e, "csrc.weo_values: a series index past csrc.weo_series")
  if (!all(d$weo_values$region %in% d$weo_regions$region) || !all(d$weo_geo$region %in% d$weo_regions$region)) e <- c(e, "csrc.weo: a region not in csrc.weo_regions")
  if (any(d$nze21_values$series >= nrow(d$nze21_series))) e <- c(e, "csrc.nze21_values: a series index past csrc.nze21_series")
  if (any(d$wei_values$row >= nrow(d$wei_rows))) e <- c(e, "csrc.wei_values: a row index past csrc.wei_rows")
  if (!all(d$wei_values$region %in% d$wei_regions$region)) e <- c(e, "csrc.wei_values: a region not in csrc.wei_regions")
  if (!all(d$ei$ind %in% d$ei_ind$id) || !all(d$ei_median$ind %in% d$ei_ind$id)) e <- c(e, "csrc.ei: an indicator not in csrc.ei_ind")
  if (any(d$ei_median$n < 1L)) e <- c(e, "csrc.ei_median: n must be positive")
  m <- d$map
  wei <- m$src == "wei"
  if (!all(m$key[wei] %in% d$wei_rows$id)) e <- c(e, "csrc.map: a wei key that is not a WEI row id")
  if (!all(m$key[!wei] %in% SECTOR_SRC_V$fuel)) e <- c(e, "csrc.map: a gcp key that is not a fuel")
  if (any(wei & (is.na(m$match) | m$role == "context")) || any(!wei & (!is.na(m$match) | m$role != "context"))) e <- c(e, "csrc.map: wei rows need a match and role own or demand; gcp rows are context without one")
  rank <- match(m$match, SECTOR_SRC_V$cell_match)
  want <- vapply(d$cells$item, function(i) {
    r <- rank[m$item == i & wei]
    if (length(r)) SECTOR_SRC_V$cell_match[min(r)] else "none"
  }, character(1))
  if (!identical(unname(want), d$cells$match)) e <- c(e, "csrc.cells.match is not the best match of the cell's WEI rows")
  e
}

# The median economy per (indicator, year), recomputed from the processed tracker table (parity check).
sector_src_median <- function(ei) {
  out <- dplyr::summarise(dplyr::group_by(ei, .data$ind, .data$year), value = stats::median(.data$value), n = dplyr::n(), .groups = "drop")
  tibble::tibble(ind = out$ind, year = as.integer(out$year), value = as.double(out$value), n = as.integer(out$n))
}

# The decoded sector-climate-src chunk against the processed climate_src_*_tbl.rds (`p`, as sector_src_part() takes them):
# every shipped value identical() to the processed row of the same key, and every processed row of a
# dims.geo economy shipped. Returns check rows (check, status, n, detail).
sector_src_parity <- function(decoded, meta, p) {
  d <- decoded$csrc
  if (is.null(d$gcp)) return(tibble::tibble(check = character(), status = character(), n = integer(), detail = character()))
  geo <- meta$dims$geo$code
  it <- meta$dims$items
  key <- paste0(it$axis, ":", it$slug)
  rows <- list()
  df <- function(x, by) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x <- x[do.call(order, c(unname(as.list(x[by])), list(method = "radix"))), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  chk <- function(label, got, ref, by) {
    ok <- identical(df(got, by), df(ref, by))
    rows[[length(rows) + 1L]] <<- tibble::tibble(check = paste0("climate (decoded): ", label), status = if (ok) "PASS" else "FAIL", n = nrow(got), detail = NA_character_)
  }
  pg <- p$gcp_national[p$gcp_national$iso3 %in% geo, , drop = FALSE]
  chk("GCP national fossil CO2 identical() to climate_src_gcp_national_tbl (dims.geo economies, EU27 and World)",
      tibble::tibble(iso3 = geo[d$gcp$geo + 1L], measure = d$gcp$measure, year = d$gcp$year, value = d$gcp$value),
      tibble::tibble(iso3 = pg$iso3, measure = pg$measure, year = as.integer(pg$year), value = as.double(pg$value)), c("iso3", "measure", "year"))
  chk("GCP World fossil CO2 by fuel identical() to climate_src_gcp_fuel_tbl",
      tibble::tibble(fuel = d$gcp_fuel$fuel, year = d$gcp_fuel$year, value = d$gcp_fuel$value),
      tibble::tibble(fuel = p$gcp_fuel$fuel, year = as.integer(p$gcp_fuel$year), value = as.double(p$gcp_fuel$value)), c("fuel", "year"))
  pe <- p$ei[p$ei$iso3 %in% geo, , drop = FALSE]
  chk("EI Country Transition Tracker identical() to climate_src_ei_tbl (dims.geo economies)",
      tibble::tibble(iso3 = geo[d$ei$geo + 1L], ind = d$ei$ind, year = d$ei$year, value = d$ei$value),
      tibble::tibble(iso3 = pe$iso3, ind = pe$ind, year = as.integer(pe$year), value = as.double(pe$value)), c("ind", "year", "iso3"))
  chk("EI indicators identical() to climate_src_ei_ind_tbl", d$ei_ind,
      tibble::tibble(id = p$ei_ind$id, label = p$ei_ind$label, unit = p$ei_ind$unit, better = p$ei_ind$better, sheet = p$ei_ind$sheet,
                     low_carbon = as.logical(p$ei_ind$low_carbon)), "id")
  chk("EI median economy identical() to climate_src_ei_median_tbl", d$ei_median,
      tibble::tibble(ind = p$ei_median$ind, year = as.integer(p$ei_median$year), value = as.double(p$ei_median$value), n = as.integer(p$ei_median$n)), c("ind", "year"))
  chk("EI median economy identical() to the median recomputed from climate_src_ei_tbl (every economy the tracker lists)", d$ei_median,
      sector_src_median(p$ei), c("ind", "year"))
  sid <- d$weo_series$id
  chk("WEO regional series identical() to climate_src_weo_series_tbl", d$weo_series, p$weo_series[c("id", "label", "unit", "category", "product", "flow")], "id")
  chk("WEO regional pathways identical() to climate_src_weo_values_tbl",
      tibble::tibble(region = d$weo_values$region, series = sid[d$weo_values$series + 1L], scenario = d$weo_values$scenario,
                     year = d$weo_values$year, value = d$weo_values$value),
      tibble::tibble(region = p$weo_values$region, series = p$weo_values$series, scenario = p$weo_values$scenario, year = as.integer(p$weo_values$year),
                     value = as.double(p$weo_values$value)), c("region", "series", "scenario", "year"))
  chk("WEO regions identical() to climate_src_weo_regions_tbl", d$weo_regions, p$weo_regions[c("region", "kind")], "region")
  pw <- p$weo_geo[p$weo_geo$iso3 %in% geo, , drop = FALSE]
  chk("economy -> WEO region identical() to climate_src_weo_geo_tbl (dims.geo economies)",
      tibble::tibble(iso3 = geo[d$weo_geo$geo + 1L], region = d$weo_geo$region, match = d$weo_geo$match), pw[c("iso3", "region", "match")], "iso3")
  nid <- d$nze21_series$id
  chk("NZE 2021 series identical() to climate_src_nze21_series_tbl", d$nze21_series,
      p$nze21_series[c("id", "label", "unit", "category", "product", "flow", "weo", "match", "note")], "id")
  chk("NZE 2021 values identical() to climate_src_nze21_values_tbl",
      tibble::tibble(series = nid[d$nze21_values$series + 1L], year = d$nze21_values$year, value = d$nze21_values$value),
      tibble::tibble(series = p$nze21_values$series, year = as.integer(p$nze21_values$year), value = as.double(p$nze21_values$value)), c("series", "year"))
  rid <- d$wei_rows$id
  chk("WEI rows identical() to climate_src_wei_rows_tbl", d$wei_rows, p$wei_rows[c("id", "label", "name", "parent", "unit")], "id")
  chk("WEI investment identical() to climate_src_wei_values_tbl",
      tibble::tibble(row = rid[d$wei_values$row + 1L], region = d$wei_values$region, year = d$wei_values$year, value = d$wei_values$value,
                     estimate = d$wei_values$estimate),
      tibble::tibble(row = p$wei_values$row, region = p$wei_values$region, year = as.integer(p$wei_values$year), value = as.double(p$wei_values$value),
                     estimate = as.logical(p$wei_values$estimate)), c("row", "region", "year"))
  chk("WEI regions identical() to climate_src_wei_regions_tbl", d$wei_regions, p$wei_regions[c("region", "label", "kind")], "region")
  chk("climate sources cell map identical() to climate_src_map_tbl",
      tibble::tibble(cell = sub("^cell:", "", key[d$map$item + 1L]), src = d$map$src, key = d$map$key, role = d$map$role,
                     match = d$map$match, order = d$map$order),
      tibble::tibble(cell = p$map$cell, src = p$map$src, key = p$map$key, role = p$map$role, match = p$map$match, order = as.integer(p$map$order)),
      c("cell", "src", "order"))
  chk("climate sources cell match and notes identical() to climate_src_cells_tbl",
      tibble::tibble(cell = sub("^cell:", "", key[d$cells$item + 1L]), match = d$cells$match, note = d$cells$note),
      p$cells[c("cell", "match", "note")], "cell")
  chk("climate sources editions identical() to climate_src_meta_tbl", d$meta, tibble::tibble(key = p$meta$key, value = as.character(p$meta$value)), "key")
  dplyr::bind_rows(rows)
}
