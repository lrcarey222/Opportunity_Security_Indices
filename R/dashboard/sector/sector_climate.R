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
  list(climate = list(src = src, map = m, cells = cc, lifecycle = lc, em = em, intensity = int))
}

# The R mirror of the contract's CLIMATE_SPEC (sector_chunk_specs() adds it).
sector_climate_spec <- function() {
  list(
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
  )
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
  e
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
