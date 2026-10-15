# Assembly of the Sector Insights bundle from its parts (spec_sector.md 8.3 step 10): the derived
# meta tables (section cards, summary, coverage, availability, rosters, reconcile), the headlines and
# the coverage glyph. Pure; shared by the real export and the synthetic fixture so both follow one
# set of rules.
#
# parts (long tables keyed by text; see sector_chunks.R):
#   geo, region_members, items (+ text: running-text name), item_links (item_key, cell, weight),
#   mineral_demand, eu_outside_coalition, item_units (var, item_key, unit), roster_extra (roster, geo),
#   facts, fact_notes (var, item_key, geo, year, note: per-fact notes, a dict.caveats id each; or NULL),
#   world, series (list(obs, world, monthly, capacity) or NULL),
#   trade (named by tech: list(flows, declared, codes, excluded, and optionally cty_sub
#   (sector_trade_sub_table()) and sub_intra; sector_assemble() adds sub_summary)),
#   eu_intra (item_key, year, intra),
#   policy (list(pams, text, tags, cat)), fdi (sector_fdi_part(): CM-FDI projects, sources, flows,
#   groups, years; or NULL), reported (exporter-reported trade per cell, or NULL),
#   licensed (item_key, family, sources) or NULL, headline_sources (named list), fill (placeholders).

# Distinct (var, item_key, chunk, year, geo, geo_kind) of every chunk's data.
sector_data_keys <- function(parts, geo) {
  kind <- function(g) geo$kind[match(g, geo$code)]
  pick <- function(x, chunk) {
    if (is.null(x) || !nrow(x)) return(NULL)
    tibble::tibble(var = x$var, item_key = x$item_key, chunk = chunk, year = as.integer(x$year), geo = x$geo, geo_kind = kind(x$geo))
  }
  out <- list(pick(parts$facts, "facts"))
  if (!is.null(parts$series)) {
    out <- c(out, list(pick(parts$series$obs, "series"), pick(parts$series$monthly, "series")))
    ca <- parts$series$capacity
    if (!is.null(ca) && nrow(ca)) {
      out <- c(out, list(tibble::tibble(var = "manufacturing.capacity", item_key = ca$item_key, chunk = "series",
                                        year = as.integer(ca$year), geo = ca$geo, geo_kind = kind(ca$geo))))
    }
  }
  for (tech in names(parts$trade)) {
    fl <- parts$trade[[tech]]$flows
    if (is.null(fl) || !nrow(fl)) next
    yrs <- lapply(SECTOR_TRADE_YEARS, function(y) {
      v <- fl[[paste0("v", y)]]
      ok <- !is.na(v)
      if (!any(ok)) return(NULL)
      tibble::tibble(var = "trade.flow", item_key = fl$item_key[ok], chunk = "trade", year = y, geo = fl$exporter[ok],
                     geo_kind = kind(fl$exporter[ok]))
    })
    out <- c(out, yrs)
    # Trade by sub-component (trade.cty_sub): economies with a value on the side (WLD rows excluded).
    cs <- parts$trade[[tech]]$cty_sub
    if (!is.null(cs) && nrow(cs)) {
      for (side in c("exports", "imports")) {
        ok <- cs$geo != "WLD" & !is.na(cs[[side]])
        if (!any(ok)) next
        out <- c(out, list(tibble::tibble(var = paste0("trade.sub_", side), item_key = cs$item_key[ok], chunk = "trade",
                                          year = as.integer(cs$year[ok]), geo = cs$geo[ok], geo_kind = kind(cs$geo[ok]))))
      }
    }
  }
  pl <- parts$policy
  if (!is.null(pl$pams) && nrow(pl$pams)) {
    tag_slugs <- sector_policy_tag_slugs(pl$tags)
    rows <- lapply(seq_along(pl$tags), function(i) {
      hit <- bitwAnd(pl$pams$tech_mask, bitwShiftL(1L, i - 1L)) != 0L
      if (!any(hit)) return(NULL)
      tibble::tibble(var = "policy.records", item_key = paste0("policy_tag:", tag_slugs[i]), chunk = "policy",
                     year = pl$pams$year[hit], geo = pl$pams$geo[hit], geo_kind = kind(pl$pams$geo[hit]))
    })
    out <- c(out, rows)
    xc <- pl$pams$cross_cutting
    if (any(xc)) {
      out <- c(out, list(tibble::tibble(var = "policy.records", item_key = "policy_tag:cross-cutting", chunk = "policy",
                                        year = pl$pams$year[xc], geo = pl$pams$geo[xc], geo_kind = kind(pl$pams$geo[xc]))))
    }
  }
  if (!is.null(pl$cat) && nrow(pl$cat)) {
    out <- c(out, list(tibble::tibble(var = "policy.cat", item_key = "none:none", chunk = "policy", year = NA_integer_,
                                      geo = pl$cat$geo, geo_kind = kind(pl$cat$geo))))
  }
  if (!is.null(parts$fdi)) out <- c(out, list(sector_fdi_data_keys(parts$fdi, kind)))
  md <- parts$mineral_demand
  if (!is.null(md) && nrow(md)) {
    out <- c(out, list(tibble::tibble(var = "consumption.mineral_demand", item_key = paste0("mineral:", md$mineral), chunk = "meta",
                                      year = as.integer(md$year), geo = "WLD", geo_kind = "world")))
  }
  dplyr::bind_rows(out)
}

# Policy tag labels -> item slugs ("Electric Grid" -> "electric-grid").
sector_policy_tag_slugs <- function(tags) gsub(" ", "-", tolower(tags))

# rosters: per dictionary roster id, the economies with a row of a measure that names it, plus
# roster_extra (e.g. every economy in the trade matrix). Geos of kind country, eu, region or entity.
sector_rosters <- function(data_keys, vars, geo, roster_extra = NULL) {
  r <- vars[!is.na(vars$roster), c("id", "roster")]
  d <- data_keys[data_keys$var %in% r$id & data_keys$geo_kind %in% c("country", "eu", "region", "entity"), c("var", "geo")]
  d$roster <- r$roster[match(d$var, r$id)]
  out <- unique(rbind(as.data.frame(d[c("roster", "geo")]), if (!is.null(roster_extra)) as.data.frame(roster_extra[c("roster", "geo")])))
  out <- out[out$geo %in% geo$code, , drop = FALSE]
  out <- out[order(out$roster, match(out$geo, geo$code)), , drop = FALSE]
  tibble::as_tibble(out)
}

# The bundle sector_chunks() encodes. dict: sector_dictionary_tables(); ship_vars: the var ids to
# ship (default: every dictionary var).
sector_assemble <- function(parts, dict, ship_vars = NULL) {
  geo <- parts$geo
  items <- parts$items
  items$item_key <- sector_item_key(items$axis, items$slug)
  if (anyDuplicated(items$item_key)) stop("sector_assemble(): duplicate items")
  vars <- dict$vars
  if (!is.null(ship_vars)) vars <- vars[vars$id %in% ship_vars, , drop = FALSE]
  item_units <- parts$item_units[parts$item_units$var %in% vars$id & parts$item_units$item_key %in% items$item_key, , drop = FALSE]

  data <- sector_data_keys(parts, geo)
  unknown_var <- setdiff(unique(data$var), vars$id)
  if (length(unknown_var)) stop("sector_assemble(): data for measures not in the dictionary: ", paste(unknown_var, collapse = ", "))
  unknown_item <- setdiff(unique(data$item_key), items$item_key)
  if (length(unknown_item)) stop("sector_assemble(): data for unknown items: ", paste(utils::head(unknown_item, 8), collapse = ", "))
  unknown_geo <- setdiff(unique(data$geo), geo$code)
  if (length(unknown_geo)) stop("sector_assemble(): data for economies not in dims.geo: ", paste(utils::head(unknown_geo, 8), collapse = ", "))

  # Item axes agree with the dictionary.
  ax <- sub(":.*$", "", data$item_key)
  bad_ax <- unique(data$var[ax != vars$item_axis[match(data$var, vars$id)]])
  if (length(bad_ax)) stop("sector_assemble(): item axis differs from the dictionary for ", paste(bad_ax, collapse = ", "))

  # Section cards per cell, the cell's measures in dictionary order.
  cells <- items[items$axis == "cell", , drop = FALSE]
  dkeys <- unique(data[c("var", "item_key")])
  cell_vars <- sector_cell_vars(cells, vars, dkeys, parts$item_links)
  dc <- dict$cells
  dc$section_cards <- vapply(dc$slug, function(s) {
    v <- cell_vars$var[cell_vars$cell == s]
    if (length(v)) paste(v, collapse = "|") else NA_character_
  }, character(1), USE.NAMES = FALSE)
  dict$cells <- dc

  # Series windows.
  sw <- data[data$chunk == "series" & !is.na(data$year), , drop = FALSE]
  if (nrow(sw)) {
    rng <- tapply(sw$year, sw$var, function(y) paste0(min(y), "-", max(y)))
    vars$series_window <- unname(rng[vars$id])
  }
  # Measures that live only in sector-fdi: `years` = the announcement years with data (the
  # dictionary cannot know them for a new CM-FDI edition).
  fy <- data[data$chunk == "fdi" & !is.na(data$year), , drop = FALSE]
  if (nrow(fy)) {
    yrs <- tapply(fy$year, fy$var, function(y) paste(sort(unique(y)), collapse = "|"))
    hit <- vars$id %in% names(yrs) & !vars$id %in% data$var[data$chunk != "fdi"]
    vars$years[hit] <- unname(yrs[vars$id[hit]])
  }
  # Caveats that describe per-fact notes (SECTOR_DATA_CAVEATS: EIA provenance, production assumed 0,
  # Estonia's oil shale) stay on a measure only when this build has such a fact for it.
  fn <- parts$fact_notes
  if (!is.null(vars$caveats)) {
    vars$caveats <- vapply(seq_len(nrow(vars)), function(i) {
      cv <- if (is.na(vars$caveats[i])) character() else strsplit(vars$caveats[i], "|", fixed = TRUE)[[1]]
      used <- if (is.null(fn)) character() else unique(fn$note[fn$var == vars$id[i]])
      cv <- cv[!cv %in% SECTOR_DATA_CAVEATS | cv %in% used]
      if (length(cv)) paste(cv, collapse = "|") else NA_character_
    }, character(1))
  }
  dict$vars <- vars

  # A world row for every summarised slice (sum_all / sum_listed: the holders' sum; one: 1).
  cw <- sector_complete_world(parts$facts, parts$world, vars, geo)
  parts$world <- cw$world
  world_missing <- cw$missing
  if (!is.null(parts$series) && !is.null(parts$series$obs)) {
    sw <- sector_complete_world(parts$series$obs, parts$series$world %||% parts$world[0, ], vars, geo)
    parts$series$world <- sw$world
    world_missing <- c(world_missing, sw$missing)
  }
  summary <- sector_summary(parts$facts, parts$world, geo, vars, parts$eu_intra)
  # trade.sub_summary: the same rules per sub-component of each trade cell.
  for (tech in names(parts$trade)) {
    tr <- parts$trade[[tech]]
    if (!is.null(tr$cty_sub) && nrow(tr$cty_sub)) parts$trade[[tech]]$sub_summary <- sector_trade_sub_summary(tr$cty_sub, tr$sub_intra, geo, vars)
  }
  # top1_geo stays a dims.geo row number here; sector_chunks() makes it 0-based.
  coverage <- sector_coverage(items, vars, cell_vars, data, rbind(parts$item_links[c("item_key", "cell")],
                                                                  tibble::tibble(item_key = "none:none", cell = cells$slug)),
                              dict$sources, parts$licensed)
  avail <- sector_avail(data)
  rosters <- sector_rosters(data, vars, geo, parts$roster_extra)
  reconcile <- if (!is.null(parts$reported)) {
    sector_trade_reconcile(summary, parts$reported)
  } else {
    tibble::tibble(item_key = character(), year = integer(), matrix_total = double(), matrix_china = double(), matrix_hhi = double(),
                   reported_total = double(), reported_china = double(), reported_hhi = double(), n_reporters = integer(), warn = logical())
  }
  headlines <- sector_headlines(summary, parts$facts, geo, items, vars, parts$policy$pams, parts$headline_sources %||% list())
  glyph <- sector_coverage_glyph(items, coverage)

  list(
    geo = geo, region_members = parts$region_members, items = items, item_links = parts$item_links,
    mineral_demand = parts$mineral_demand, eu_outside_coalition = parts$eu_outside_coalition,
    dict = dict, item_units = item_units, rosters = rosters, coverage = coverage, avail = avail,
    summary = summary, reconcile = reconcile, facts = parts$facts, world = parts$world, series = parts$series,
    trade = parts$trade, policy = parts$policy, fdi = parts$fdi, fact_notes = parts$fact_notes, headlines = headlines, glyph = glyph,
    cell_vars = cell_vars,
    world_missing = world_missing
  )
}
