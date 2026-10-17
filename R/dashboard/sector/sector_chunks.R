# The Sector Insights chunks (spec_sector.md 8.2, 8.6; dashboard/src/sector/contract.ts): long
# tables in, the nested lists dash_osb_encode() writes out. Pure.
#
# The exporter works on long tables keyed by text: var = dictionary id, item_key = "<axis>:<slug>",
# geo = dims.geo code. sector_chunks() turns the keys into the 0-based i32 indices of the contract,
# sorts and slices the observation tables and casts every column to its contract type (i32 =
# integer, f64 = double, bool = logical, str = character), even when a column is all NA.

SECTOR_SCHEMA <- "osi-sector"
SECTOR_SCHEMA_VERSION <- 1L
SECTOR_TRADE_YEARS <- c(2020L, 2024L)
SECTOR_CHUNK_FILE_RE <- "^sector-[a-z-]+\\.[0-9a-f]{8}\\.osb$"
SECTOR_BUDGET_GZ <- list(
  `sector-meta` = 150 * 1024, `sector-facts` = 600 * 1024, `sector-trade` = 450 * 1024,
  `sector-policy` = 250 * 1024, `sector-policy-text` = 700 * 1024, `sector-series` = 900 * 1024,
  `sector-atlas` = 1.3 * 1024^2, `sector-dev` = 1.2 * 1024^2, `sector-fdi` = 300 * 1024,
  `sector-climate` = 300 * 1024,
  sector_total = 9 * 1024^2, combined_total = 25 * 1024^2
)
SECTOR_DICT_VAR_COLS <- c("id", "theme", "family", "label", "short", "definition", "unit", "data_year", "years",
                          "year_field", "native_level", "item_axis", "agg", "world_basis", "norms", "scale",
                          "higher_is", "sign_note", "display_scale", "source", "roster", "licence", "caveats",
                          "primary_for", "section", "stale_years", "min_base", "feeds", "prov", "prov_detail",
                          "phase", "projection_from", "series_window")

sector_item_key <- function(axis, slug) paste0(axis, ":", slug)

# 0-based row indices of `x` in `table`; stops on any value not found.
sector_index <- function(x, table, what) {
  i <- match(x, table)
  if (anyNA(i)) stop("sector_chunks(): ", what, " not found: ", paste(utils::head(unique(x[is.na(i)]), 8), collapse = ", "))
  as.integer(i - 1L)
}

# An observation table (facts.obs / series.obs) and its slice index, sorted by (var, item, year,
# geo) indices. obs: var, item_key, geo, year, value, status, obs_year (and, optionally, obs_from: the
# first year of a value averaged over years, development.wdi; NA elsewhere).
sector_obs_tables <- function(obs, var_ids, item_keys, geo_codes) {
  if (is.null(obs) || !nrow(obs)) {
    empty_obs <- tibble::tibble(var = integer(), item = integer(), geo = integer(), year = integer(), value = double(),
                                status = character(), obs_year = integer(), obs_from = integer())
    return(list(obs = empty_obs, index = tibble::tibble(var = integer(), item = integer(), year = integer(), start = integer(), n = integer())))
  }
  t <- tibble::tibble(
    var = sector_index(obs$var, var_ids, "var"),
    item = sector_index(obs$item_key, item_keys, "item"),
    geo = sector_index(obs$geo, geo_codes, "geo"),
    year = as.integer(obs$year),
    value = as.double(obs$value),
    status = as.character(obs$status),
    obs_year = as.integer(if ("obs_year" %in% names(obs)) obs$obs_year else NA_integer_),
    obs_from = as.integer(if ("obs_from" %in% names(obs)) obs$obs_from else NA_integer_)
  )
  if (anyNA(t$year)) stop("sector_obs_tables(): NA year")
  if (anyNA(t$status) || length(setdiff(unique(t$status), SECTOR_STATUS_CODES))) stop("sector_obs_tables(): unknown status ", paste(setdiff(unique(t$status), SECTOR_STATUS_CODES), collapse = ", "))
  t <- t[order(t$var, t$item, t$year, t$geo), , drop = FALSE]
  dup <- duplicated(t[c("var", "item", "year", "geo")])
  if (any(dup)) stop("sector_obs_tables(): duplicate (var, item, year, geo) keys: ", sum(dup))
  key <- paste(t$var, t$item, t$year, sep = "\r")
  first <- !duplicated(key)
  starts <- which(first)
  n <- diff(c(starts, nrow(t) + 1L))
  index <- tibble::tibble(var = t$var[starts], item = t$item[starts], year = t$year[starts],
                          start = as.integer(starts - 1L), n = as.integer(n))
  list(obs = t, index = index)
}

# facts.notes (contract FACTS_SPEC, optional): per-fact notes, one row per (fact, note), sorted by
# (var, item, year, geo) indices like facts.obs; NULL when there are none. Stops on a note whose fact
# is not in `facts` or whose id is not in `caveat_ids` (dict.caveats).
sector_notes_table <- function(notes, facts, var_ids, item_keys, geo_codes, caveat_ids) {
  if (is.null(notes) || !nrow(notes)) return(NULL)
  fk <- paste(facts$var, facts$item_key, facts$geo, facts$year, sep = "\r")
  nk <- paste(notes$var, notes$item_key, notes$geo, notes$year, sep = "\r")
  if (!all(nk %in% fk)) stop("sector_notes_table(): notes for facts that are not shipped: ", paste(utils::head(unique(nk[!nk %in% fk]), 3), collapse = "; "))
  bad <- setdiff(unique(notes$note), caveat_ids)
  if (length(bad)) stop("sector_notes_table(): note ids not in dict.caveats: ", paste(bad, collapse = ", "))
  t <- tibble::tibble(var = sector_index(notes$var, var_ids, "var"), item = sector_index(notes$item_key, item_keys, "item"),
                      geo = sector_index(notes$geo, geo_codes, "geo"), year = as.integer(notes$year), note = as.character(notes$note))
  t <- t[order(t$var, t$item, t$year, t$geo, t$note, method = "radix"), , drop = FALSE]
  if (anyDuplicated(t[c("var", "item", "year", "geo", "note")])) stop("sector_notes_table(): duplicate notes")
  t
}

sector_world_table <- function(world, var_ids, item_keys) {
  if (is.null(world) || !nrow(world)) {
    return(tibble::tibble(var = integer(), item = integer(), year = integer(), value = double(), basis = character()))
  }
  t <- tibble::tibble(
    var = sector_index(world$var, var_ids, "var"), item = sector_index(world$item_key, item_keys, "item"),
    year = as.integer(world$year), value = as.double(world$value), basis = as.character(world$basis)
  )
  t <- t[order(t$var, t$item, t$year), , drop = FALSE]
  if (anyDuplicated(t[c("var", "item", "year")])) stop("sector_world_table(): duplicate keys")
  t
}

# The dims.geo table with contract types.
sector_geo_table <- function(geo) {
  tibble::tibble(
    code = as.character(geo$code), name = as.character(geo$name), kind = as.character(geo$kind),
    iso_n3 = as.integer(geo$iso_n3), tier = as.character(geo$tier), group = as.character(geo$group),
    eu_member = as.logical(geo$eu_member), re_export_hub = as.logical(geo$re_export_hub),
    bloc = as.character(geo$bloc), s_axis = as.double(geo$s_axis), alignment_basis = as.character(geo$alignment_basis),
    wb_region = as.character(geo$wb_region), income = as.character(geo$income), lon = as.double(geo$lon),
    lat = as.double(geo$lat), reason = as.character(geo$reason)
  )
}

# Every chunk as a nested list (names without dots: dash_osb_flatten() joins them with "."),
# keyed by chunk name. b: the bundle of long tables (see scripts/87 and sector_synthetic_bundle()).
sector_chunks <- function(b) {
  geo <- sector_geo_table(b$geo)
  geo_codes <- geo$code
  items <- b$items
  item_keys <- sector_item_key(items$axis, items$slug)
  vars <- b$dict$vars
  var_ids <- vars$id
  gi <- function(x, what = "geo") sector_index(x, geo_codes, what)
  ii <- function(x, what = "item") sector_index(x, item_keys, what)
  vi <- function(x, what = "var") sector_index(x, var_ids, what)

  dims <- list(
    geo = geo,
    region_members = tibble::tibble(geo = gi(b$region_members$geo), member = gi(b$region_members$member)),
    items = tibble::tibble(
      axis = as.character(items$axis), slug = as.character(items$slug), label = as.character(items$label),
      tech = as.character(items$tech), stage = as.character(items$stage), headline = as.logical(items$headline),
      sort = as.integer(items$sort), has_trade = as.logical(items$has_trade)
    ),
    item_links = tibble::tibble(item = ii(b$item_links$item_key), cell = ii(sector_item_key("cell", b$item_links$cell)),
                                weight = as.double(b$item_links$weight)),
    mineral_demand = tibble::tibble(
      mineral = ii(sector_item_key("mineral", b$mineral_demand$mineral)), tech = as.character(b$mineral_demand$tech),
      label = as.character(b$mineral_demand$label), scenario = as.character(b$mineral_demand$scenario),
      year = as.integer(b$mineral_demand$year), kt = as.double(b$mineral_demand$kt)
    ),
    eu_outside_coalition = as.character(b$eu_outside_coalition)
  )
  dv <- vars[SECTOR_DICT_VAR_COLS]
  dv$year_field <- as.logical(dv$year_field)
  dv$display_scale <- as.double(dv$display_scale)
  dv$stale_years <- as.integer(dv$stale_years)
  dv$min_base <- as.double(dv$min_base)
  dv$projection_from <- as.integer(dv$projection_from)
  d <- b$dict
  dict <- list(
    vars = dv,
    units = d$units[c("id", "short", "long", "unit_kind")],
    sources = d$sources[c("id", "short", "citation", "publisher", "edition", "url", "licence", "attribution", "accessed", "transform", "issues")],
    caveats = d$caveats[c("id", "text")],
    themes = d$themes[c("slug", "label", "description", "order", "default_var")],
    families = d$families[c("id", "label", "section")],
    cells = d$cells[c("slug", "cell_blurb", "section_cards")],
    techs = d$techs[c("slug", "label", "tech_blurb")],
    item_units = tibble::tibble(var = vi(b$item_units$var), item = ii(b$item_units$item_key), unit = as.character(b$item_units$unit))
  )
  # Optional (sector_data change 2026-10-12): the cells' basket notes and the HHI reference thresholds.
  cc <- d$cell_caveats
  if (!is.null(cc) && nrow(cc)) {
    if (length(setdiff(cc$slug, d$cells$slug))) stop("sector_chunks(): cell caveats for cells not in dict.cells")
    dict$cell_caveats <- tibble::tibble(slug = as.character(cc$slug), caveat = as.character(cc$caveat))
  }
  hr <- d$hhi_refs
  if (!is.null(hr) && nrow(hr)) {
    dict$hhi_refs <- tibble::tibble(id = as.character(hr$id), value = as.double(hr$value), points = as.integer(hr$points),
                                    side = as.character(hr$side), label = as.character(hr$label), authority = as.character(hr$authority),
                                    document = as.character(hr$document), year = as.integer(hr$year))
  }
  sm <- b$summary
  meta <- list(
    dims = dims,
    dict = dict,
    rosters = tibble::tibble(roster = as.character(b$rosters$roster), geo = gi(b$rosters$geo)),
    coverage = tibble::tibble(item = ii(b$coverage$item_key), family = b$coverage$family, status = b$coverage$status,
                              n_geo = as.integer(b$coverage$n_geo), reason = as.character(b$coverage$reason),
                              sources = as.character(b$coverage$sources)),
    avail = tibble::tibble(var = vi(b$avail$var), item = ii(b$avail$item_key), chunk = as.character(b$avail$chunk),
                           year_min = as.integer(b$avail$year_min), year_max = as.integer(b$avail$year_max),
                           n_years = as.integer(b$avail$n_years), n_geo = as.integer(b$avail$n_geo)),
    summary = tibble::tibble(
      item = ii(sm$item_key), var = vi(sm$var), year = as.integer(sm$year), eu_mode = as.character(sm$eu_mode),
      W = as.double(sm$W), basis = as.character(sm$basis), n = as.integer(sm$n),
      top1_geo = as.integer(sm$top1_geo - 1L), top1_share = as.double(sm$top1_share), top3_share = as.double(sm$top3_share),
      hhi = as.double(sm$hhi), hhi_lower_bound = as.logical(sm$hhi_lower_bound), eff_n = as.double(sm$eff_n),
      china = as.double(sm$china), core = as.double(sm$core), partner = as.double(sm$partner), rest = as.double(sm$rest),
      unattributed = as.double(sm$unattributed), growth = as.double(sm$growth), growth_kind = as.character(sm$growth_kind)
    ),
    reconcile = tibble::tibble(
      item = ii(b$reconcile$item_key), year = as.integer(b$reconcile$year),
      matrix_total = as.double(b$reconcile$matrix_total), matrix_china = as.double(b$reconcile$matrix_china),
      matrix_hhi = as.double(b$reconcile$matrix_hhi), reported_total = as.double(b$reconcile$reported_total),
      reported_china = as.double(b$reconcile$reported_china), reported_hhi = as.double(b$reconcile$reported_hhi),
      n_reporters = as.integer(b$reconcile$n_reporters), warn = as.logical(b$reconcile$warn)
    )
  )
  out <- list(`sector-meta` = meta)

  fo <- sector_obs_tables(b$facts, var_ids, item_keys, geo_codes)
  out$`sector-facts` <- list(facts = list(obs = fo$obs, index = fo$index, world = sector_world_table(b$world, var_ids, item_keys)))
  fnt <- sector_notes_table(b$fact_notes, b$facts, var_ids, item_keys, geo_codes, b$dict$caveats$id)
  if (!is.null(fnt)) out$`sector-facts`$facts$notes <- fnt

  for (tech in names(b$trade)) {
    tr <- b$trade[[tech]]
    fl <- tibble::tibble(
      item = ii(tr$flows$item_key), exporter = gi(tr$flows$exporter), importer = gi(tr$flows$importer),
      v2020 = as.double(tr$flows$v2020), v2024 = as.double(tr$flows$v2024),
      s2020 = as.character(tr$flows$s2020), s2024 = as.character(tr$flows$s2024)
    )
    fl <- fl[order(fl$item, fl$exporter, fl$importer), , drop = FALSE]
    out[[paste0("sector-trade-", tech)]] <- list(trade = list(
      flows = fl,
      declared = tibble::tibble(item = ii(tr$declared$item_key), geo = gi(tr$declared$geo), year = as.integer(tr$declared$year),
                                imports_declared = as.logical(tr$declared$imports_declared),
                                exports_declared = as.logical(tr$declared$exports_declared)),
      codes = tibble::tibble(item = ii(tr$codes$item_key), sub_sector = as.character(tr$codes$sub_sector), hs6 = as.character(tr$codes$hs6),
                             description = as.character(tr$codes$description), shared_n = as.integer(tr$codes$shared_n),
                             caveat = as.character(tr$codes$caveat)),
      excluded = tibble::tibble(item = ii(tr$excluded$item_key), sub_sector = as.character(tr$excluded$sub_sector),
                                hs6 = as.character(tr$excluded$hs6), reason = as.character(tr$excluded$reason))
    ))
    # Trade by sub-component (optional tables; sector_data change 2026-10-12).
    tt <- out[[paste0("sector-trade-", tech)]]$trade
    if (!is.null(tr$cty_sub) && nrow(tr$cty_sub)) {
      cs <- tibble::tibble(item = ii(tr$cty_sub$item_key), sub_sector = as.character(tr$cty_sub$sub_sector), geo = gi(tr$cty_sub$geo),
                           year = as.integer(tr$cty_sub$year), exports = as.double(tr$cty_sub$exports), imports = as.double(tr$cty_sub$imports))
      tt$cty_sub <- cs[order(cs$item, cs$sub_sector, cs$geo, cs$year, method = "radix"), , drop = FALSE]
    }
    if (!is.null(tr$sub_intra) && nrow(tr$sub_intra)) {
      si <- tibble::tibble(item = ii(tr$sub_intra$item_key), sub_sector = as.character(tr$sub_intra$sub_sector),
                           year = as.integer(tr$sub_intra$year), intra = as.double(tr$sub_intra$intra))
      tt$sub_intra <- si[order(si$item, si$sub_sector, si$year, method = "radix"), , drop = FALSE]
    }
    ss <- tr$sub_summary
    if (!is.null(ss) && nrow(ss)) {
      st <- tibble::tibble(
        item = ii(ss$item_key), sub_sector = as.character(ss$sub_sector), var = vi(ss$var), year = as.integer(ss$year),
        eu_mode = as.character(ss$eu_mode), W = as.double(ss$W), n = as.integer(ss$n), top1_geo = as.integer(ss$top1_geo - 1L),
        top1_share = as.double(ss$top1_share), top3_share = as.double(ss$top3_share), hhi = as.double(ss$hhi),
        hhi_lower_bound = as.logical(ss$hhi_lower_bound), eff_n = as.double(ss$eff_n), china = as.double(ss$china),
        core = as.double(ss$core), partner = as.double(ss$partner), rest = as.double(ss$rest),
        unattributed = as.double(ss$unattributed), growth = as.double(ss$growth)
      )
      tt$sub_summary <- st[order(st$item, st$sub_sector, st$var, st$year, match(st$eu_mode, c("members", "bloc")), method = "radix"), , drop = FALSE]
    }
    out[[paste0("sector-trade-", tech)]]$trade <- tt
  }

  pl <- b$policy
  out$`sector-policy` <- list(policy = list(
    pams = tibble::tibble(
      id = as.integer(pl$pams$id), geo = gi(pl$pams$geo), year = as.integer(pl$pams$year), status = as.character(pl$pams$status),
      jurisdiction = as.character(pl$pams$jurisdiction), type = as.character(pl$pams$type), tech_mask = as.integer(pl$pams$tech_mask),
      tech_src = as.character(pl$pams$tech_src), stage_mask = as.integer(pl$pams$stage_mask), cross_cutting = as.logical(pl$pams$cross_cutting)
    ),
    tags = as.character(pl$tags),
    cat = tibble::tibble(
      geo = gi(pl$cat$geo), overall = as.character(pl$cat$overall), policies_action = as.character(pl$cat$policies_action),
      domestic_target = as.character(pl$cat$domestic_target), fair_share = as.character(pl$cat$fair_share),
      climate_finance = as.character(pl$cat$climate_finance), ndc = as.character(pl$cat$ndc), updated = as.character(pl$cat$updated)
    )
  ))
  if (!is.null(pl$text)) {
    out$`sector-policy-text` <- list(policy = list(text = tibble::tibble(
      id = as.integer(pl$text$id), title = as.character(pl$text$title), url = as.character(pl$text$url)
    )))
  }

  if (!is.null(b$series)) {
    so <- sector_obs_tables(b$series$obs, var_ids, item_keys, geo_codes)
    ser <- list(obs = so$obs, index = so$index, world = sector_world_table(b$series$world, var_ids, item_keys))
    mo <- b$series$monthly
    if (!is.null(mo)) {
      m <- tibble::tibble(var = vi(mo$var), item = ii(mo$item_key), geo = gi(mo$geo), year = as.integer(mo$year),
                          month = as.integer(mo$month), value = as.double(mo$value), status = as.character(mo$status))
      ser$monthly <- m[order(m$var, m$item, m$geo, m$year, m$month), , drop = FALSE]
    }
    ca <- b$series$capacity
    if (!is.null(ca) && nrow(ca)) {
      ser$capacity <- tibble::tibble(item = ii(ca$item_key), product = as.character(ca$product), unit = as.character(ca$unit),
                                     geo = gi(ca$geo), year = as.integer(ca$year), cap_status = as.character(ca$cap_status),
                                     value = as.double(ca$value), status = as.character(ca$status))
    }
    out$`sector-series` <- list(series = ser)
  }

  if (!is.null(b$fdi)) out$`sector-fdi` <- sector_fdi_chunk(b$fdi, gi, ii)
  if (!is.null(b$dev)) out$`sector-dev` <- sector_dev_chunk(b$dev, gi, ii)
  if (!is.null(b$climate)) out$`sector-climate` <- sector_climate_chunk(b$climate, gi, ii)
  out
}

# The sector-fdi chunk (contract FDI_SPEC; R/dashboard/sector/sector_fdi.R) from the text-keyed part.
# gi / ii: geo / item code -> 0-based index. Projects sorted by (item, year announced, project id);
# fdi.sources.project is a 0-based row of fdi.projects.
sector_fdi_chunk <- function(fd, gi, ii) {
  pr <- fd$projects
  pr <- pr[order(ii(pr$item_key), pr$year_announced, pr$pid, method = "radix"), , drop = FALSE]
  projects <- tibble::tibble(
    pid = as.character(pr$pid), item = ii(pr$item_key), technology = as.character(pr$technology),
    parent = as.character(pr$parent), home = gi(pr$home), dest = gi(pr$dest), city = as.character(pr$city),
    status = as.character(pr$status), operational = as.character(pr$operational), activity = as.character(pr$activity),
    year_announced = as.integer(pr$year_announced), year_completed = as.integer(pr$year_completed),
    value = as.double(pr$value), value_status = as.character(pr$value_status), expansion = as.logical(pr$expansion),
    phase = as.character(pr$phase), jv = as.logical(pr$jv), jv_local = as.logical(pr$jv_local),
    jv_local_state = as.logical(pr$jv_local_state), jv_company = as.character(pr$jv_company),
    manufacturing = as.logical(pr$manufacturing), active = as.logical(pr$active), index_use = as.logical(pr$index_use),
    home_group = as.character(pr$home_group)
  )
  src <- fd$sources
  row <- match(src$pid, pr$pid)
  if (anyNA(row)) stop("sector_fdi_chunk(): sources of unknown projects")
  sources <- tibble::tibble(project = as.integer(row - 1L), field = as.character(src$field), text = as.character(src$text),
                            url = as.character(src$url), unverified = as.logical(src$unverified))
  sources <- sources[order(sources$project, match(sources$field, names(SECTOR_FDI_SOURCE_FIELDS)), method = "radix"), , drop = FALSE]
  fl <- fd$flows
  flows <- tibble::tibble(item = ii(fl$item_key), home = gi(fl$home), dest = gi(fl$dest), basis = as.character(fl$basis),
                          n = as.integer(fl$n), n_value = as.integer(fl$n_value), value = as.double(fl$value),
                          year_min = as.integer(fl$year_min), year_max = as.integer(fl$year_max))
  flows <- flows[order(flows$item, flows$home, flows$dest, match(flows$basis, SECTOR_FDI_BASES), method = "radix"), , drop = FALSE]
  gr <- fd$groups
  groups <- tibble::tibble(item = ii(gr$item_key), dest = gi(gr$dest), basis = as.character(gr$basis), group = as.character(gr$group),
                           n = as.integer(gr$n), n_value = as.integer(gr$n_value), value = as.double(gr$value),
                           share_n = as.double(gr$share_n), share_value = as.double(gr$share_value))
  groups <- groups[order(groups$item, groups$dest, match(groups$basis, SECTOR_FDI_BASES), match(groups$group, SECTOR_FDI_GROUPS),
                         method = "radix"), , drop = FALSE]
  yr <- fd$years
  years <- tibble::tibble(item = ii(yr$item_key), year = as.integer(yr$year), basis = as.character(yr$basis), group = as.character(yr$group),
                          n = as.integer(yr$n), n_value = as.integer(yr$n_value), value = as.double(yr$value))
  years <- years[order(years$item, years$year, match(years$basis, SECTOR_FDI_BASES), match(years$group, SECTOR_FDI_GROUPS),
                       method = "radix"), , drop = FALSE]
  list(fdi = list(projects = projects, sources = sources, flows = flows, groups = groups, years = years))
}

# The trade tech slug of a chunk name, or NA.
sector_chunk_tech <- function(name) ifelse(startsWith(name, "sector-trade-"), sub("^sector-trade-", "", name), NA_character_)

# The budget key of a chunk ("sector-trade" for every trade chunk).
sector_budget_key <- function(name) ifelse(startsWith(name, "sector-trade-"), "sector-trade", name)

# Chunks over their per-chunk gzip budget (names), from (name, bytes_gz).
sector_over_budget <- function(chunk_info) {
  lim <- unlist(SECTOR_BUDGET_GZ[sector_budget_key(chunk_info$name)])
  chunk_info$name[chunk_info$bytes_gz > lim]
}

# Stale chunk files in the sector directory: those matching the sector pattern that the new
# manifest does not list. Nothing else is ever deleted.
sector_stale_files <- function(files, keep) {
  files <- basename(files)
  files[grepl(SECTOR_CHUNK_FILE_RE, files) & !files %in% keep]
}

# manifest.json as a list (serialise with dash_json_text()). chunk_info: name, file, bytes,
# bytes_gz, md5. headlines: list of lists. glyph: list of lists. validation: (check, status, n,
# detail). sources: (path, mtime, bytes, md5).
sector_manifest <- function(bundle_id, created, git_commit, phase, strategic_bundle_id, processed_mtime_max,
                            pipeline_marker, flags, chunk_info, headlines, glyph, notes, validation, sources,
                            budget) {
  na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
  rows <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(list())
    lapply(seq_len(nrow(tbl)), function(i) lapply(as.list(tbl[i, , drop = FALSE]), function(v) {
      v <- v[[1]]
      if (length(v) == 1 && is.na(v)) NULL else v
    }))
  }
  ci <- chunk_info
  chunks <- lapply(seq_len(nrow(ci)), function(i) list(
    name = ci$name[i], file = ci$file[i], bytes = as.numeric(ci$bytes[i]), bytes_gz = as.numeric(ci$bytes_gz[i]),
    md5 = ci$md5[i], tech = na_null(sector_chunk_tech(ci$name[i]))
  ))
  list(
    schema = SECTOR_SCHEMA,
    schema_version = SECTOR_SCHEMA_VERSION,
    bundle_id = bundle_id,
    created = created,
    git_commit = na_null(git_commit),
    generator = "scripts/87_export_sector_data.R",
    phase = as.integer(phase),
    strategic_bundle_id = na_null(strategic_bundle_id),
    processed_mtime_max = processed_mtime_max,
    pipeline_marker = na_null(pipeline_marker),
    flags = flags,
    chunks = chunks,
    headlines = headlines,
    coverage_glyph = glyph,
    notes = I(as.character(notes)),
    validation = rows(validation),
    sources = rows(sources),
    budget = budget,
    encoding = list(lossy = FALSE)
  )
}

# Serialise the manifest (pretty JSON; nulls written as null, empty lists as []).
sector_manifest_json <- function(m) {
  as.character(jsonlite::toJSON(m, auto_unbox = TRUE, digits = I(17), na = "null", null = "null", pretty = TRUE))
}

# ---- Parity -----------------------------------------------------------------------------------------

# Compare decoded facts with the parity ledger. decoded: data frame (var, item_key, geo, year, value)
# from the decoded chunk with keys translated back; ledger: (var, item_key, geo, year, prov, table,
# variable, key, source_value, formula_value) where formula_value is f(source_value) for D rows
# and the cross-check value for X rows with processed overlap. Returns (check, status, n, detail).
sector_parity <- function(decoded, ledger, tol_x = 1e-12) {
  k_d <- paste(decoded$var, decoded$item_key, decoded$geo, decoded$year, sep = "\r")
  k_l <- paste(ledger$var, ledger$item_key, ledger$geo, ledger$year, sep = "\r")
  m <- match(k_l, k_d)
  out <- list()
  add <- function(check, ok, n, detail = NA_character_) {
    out[[length(out) + 1]] <<- tibble::tibble(check = check, status = if (ok) "PASS" else "FAIL", n = as.integer(n), detail = detail)
  }
  missing <- is.na(m)
  add("parity: ledger rows present in the decoded chunk", !any(missing), sum(!missing),
      if (any(missing)) paste("missing:", paste(utils::head(k_l[missing], 5), collapse = "; ")) else NA_character_)
  v <- decoded$value[m]
  for (p in c("P", "D")) {
    sel <- ledger$prov == p & !missing
    if (!any(sel)) next
    ref <- if (p == "P") ledger$source_value[sel] else ledger$formula_value[sel]
    ok <- vapply(seq_along(ref), function(i) identical(v[sel][i], ref[i]), logical(1))
    by_var <- split(ok, ledger$var[sel])
    for (vv in names(by_var)) {
      bad <- !by_var[[vv]]
      add(paste0("parity ", p, ": ", vv, " identical() to data/processed"), !any(bad), length(bad),
          if (any(bad)) paste(sum(bad), "rows differ") else NA_character_)
    }
  }
  sel <- ledger$prov == "X" & !missing & !is.na(ledger$formula_value)
  if (any(sel)) {
    a <- v[sel]
    r <- ledger$formula_value[sel]
    rel <- abs(a - r) / pmax(abs(r), .Machine$double.xmin)
    by_var <- split(rel, ledger$var[sel])
    for (vv in names(by_var)) {
      worst <- max(by_var[[vv]])
      add(paste0("parity X: ", vv, " cross-check with data/processed (1e-12 relative)"), worst <= tol_x, length(by_var[[vv]]),
          sprintf("max relative difference %.3g", worst))
    }
  }
  dplyr::bind_rows(out)
}

# Decoded obs (facts.obs / series.obs) with text keys again: var id, item_key, geo code.
sector_decoded_obs <- function(meta, obs) {
  it <- meta$dims$items
  tibble::tibble(var = meta$dict$vars$id[obs$var + 1L], item_key = paste0(it$axis, ":", it$slug)[obs$item + 1L],
                 geo = meta$dims$geo$code[obs$geo + 1L], year = obs$year, value = obs$value, status = obs$status,
                 obs_year = obs$obs_year, obs_from = if ("obs_from" %in% names(obs)) obs$obs_from else NA_integer_)
}

sector_decoded_world <- function(meta, world) {
  it <- meta$dims$items
  tibble::tibble(var = meta$dict$vars$id[world$var + 1L], item_key = paste0(it$axis, ":", it$slug)[world$item + 1L],
                 year = world$year, value = world$value, basis = world$basis)
}

# The largest difference between two summary tables over the numeric columns, matched on
# (item_key, var, year, eu_mode); Inf when the keys differ.
sector_summary_diff <- function(a, b) {
  ka <- paste(a$item_key, a$var, a$year, a$eu_mode)
  kb <- paste(b$item_key, b$var, b$year, b$eu_mode)
  if (!setequal(ka, kb)) return(Inf)
  b <- b[match(ka, kb), , drop = FALSE]
  cols <- c("W", "top1_share", "top3_share", "hhi", "eff_n", "china", "core", "partner", "rest", "unattributed", "growth")
  d <- vapply(cols, function(cn) {
    x <- a[[cn]]
    y <- b[[cn]]
    both_na <- is.na(x) & is.na(y)
    if (any(xor(is.na(x), is.na(y)))) return(Inf)
    rel <- abs(x - y) / pmax(abs(y), 1)
    max(c(0, rel[!both_na]))
  }, numeric(1))
  if (any(a$n != b$n) || any(a$top1_geo != b$top1_geo, na.rm = TRUE)) return(Inf)
  max(d)
}
