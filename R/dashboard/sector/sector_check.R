# R mirror of the chunk checks of dashboard/src/sector/contract.ts (checkChunk()): every decoded
# chunk is checked before it is written, so a contract break stops the export instead of the app.
# Pure. Column codes: f = f64 (double), i = i32 (integer), b = bool (logical), s = str (character);
# "!" = no NA; "@geo" / "@items" / "@vars" = a 0-based index into dims.geo / dims.items / dict.vars.

SECTOR_V <- list(
  status = c("obs", "zero", "nsr", "nq", "mirror", "est", "na_growth"),
  kind = c("country", "eu", "world", "region", "entity", "residual"),
  tier = c("core", "partner", "none"),
  group = c("china", "core", "partner", "rest", "na"),
  axis = c("cell", "mineral", "fuel", "commodity", "activity", "policy_tag", "none"),
  level = c("country", "eu", "region", "entity", "world", "cell_global", "dyad"),
  agg = c("sum", "ratio", "none"),
  basis = c("source_world", "sum_all", "one", "sum_listed", "none"),
  eu_mode = c("members", "bloc"),
  licence = c("open", "attribution"),
  prov = c("P", "D", "X"),
  phase = c("1", "2", "hold"),
  theme = c("trade", "resources", "production", "consumption", "investment", "manufacturing", "prices", "technology", "policy", "development"),
  family = c("exports", "production", "capacity", "reserves", "investment", "consumption", "prices", "policy", "technology", "context", "development"),
  section = c("snapshot", "who", "leaders", "trade", "investment", "production", "resources", "demand", "prices", "policy", "technology", "context", "countries"),
  cov = c("country", "region", "global", "licensed", "none"),
  chunk = c("meta", "facts", "trade", "policy", "series", "atlas", "dev"),
  flow = c("importer", "mirror", "none"),
  tech_src = c("iea", "text"),
  tech_label = c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal", "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind", "Semiconductors"),
  stage_label = c("Upstream", "Midstream", "Downstream"),
  cap_status = c("operational", "construction", "announced", "paused")
)

sector_tbl_spec <- function(cols, sorted = NULL, unique = NULL, values = list(), optional = FALSE) {
  list(kind = "table", cols = cols, sorted = sorted, unique = unique, values = values, optional = optional)
}

sector_chunk_specs <- function() {
  obs <- sector_tbl_spec(c(var = "i!@vars", item = "i!@items", geo = "i!@geo", year = "i!", value = "f", status = "s!", obs_year = "i"),
                         sorted = c("var", "item", "year", "geo"), unique = c("var", "item", "year", "geo"),
                         values = list(status = SECTOR_V$status))
  index <- sector_tbl_spec(c(var = "i!@vars", item = "i!@items", year = "i!", start = "i!", n = "i!"),
                           sorted = c("var", "item", "year"), unique = c("var", "item", "year"))
  world <- sector_tbl_spec(c(var = "i!@vars", item = "i!@items", year = "i!", value = "f!", basis = "s!"),
                           sorted = c("var", "item", "year"), unique = c("var", "item", "year"), values = list(basis = SECTOR_V$basis))
  list(
    meta = list(
      `dims.geo` = sector_tbl_spec(c(code = "s!", name = "s!", kind = "s!", iso_n3 = "i", tier = "s!", group = "s!", eu_member = "b!",
                                     re_export_hub = "b!", bloc = "s", s_axis = "f", alignment_basis = "s", wb_region = "s", income = "s",
                                     lon = "f", lat = "f", reason = "s"), unique = "code",
                                   values = list(kind = SECTOR_V$kind, tier = SECTOR_V$tier, group = SECTOR_V$group)),
      `dims.region_members` = sector_tbl_spec(c(geo = "i!@geo", member = "i!@geo"), unique = c("geo", "member")),
      `dims.items` = sector_tbl_spec(c(axis = "s!", slug = "s!", label = "s!", tech = "s", stage = "s", headline = "b!", sort = "i!", has_trade = "b!"),
                                     unique = c("axis", "slug"), values = list(axis = SECTOR_V$axis, tech = SECTOR_V$tech_label, stage = SECTOR_V$stage_label)),
      `dims.item_links` = sector_tbl_spec(c(item = "i!@items", cell = "i!@items", weight = "f"), unique = c("item", "cell")),
      `dims.mineral_demand` = sector_tbl_spec(c(mineral = "i!@items", tech = "s", label = "s!", scenario = "s!", year = "i!", kt = "f"),
                                              unique = c("mineral", "label", "scenario", "year"), values = list(tech = SECTOR_V$tech_label)),
      `dims.eu_outside_coalition` = list(kind = "strv"),
      `dict.vars` = sector_tbl_spec(c(id = "s!", theme = "s!", family = "s!", label = "s!", short = "s!", definition = "s", unit = "s!",
                                      data_year = "s!", years = "s", year_field = "b!", native_level = "s!", item_axis = "s!", agg = "s!",
                                      world_basis = "s!", norms = "s", scale = "s", higher_is = "s", sign_note = "s", display_scale = "f!",
                                      source = "s!", roster = "s", licence = "s!", caveats = "s", primary_for = "s", section = "s",
                                      stale_years = "i", min_base = "f", feeds = "s", prov = "s!", prov_detail = "s", phase = "s!",
                                      projection_from = "i", series_window = "s"), unique = "id",
                                    values = list(theme = SECTOR_V$theme, family = SECTOR_V$family, native_level = SECTOR_V$level,
                                                  item_axis = SECTOR_V$axis, agg = SECTOR_V$agg, world_basis = SECTOR_V$basis,
                                                  scale = c("linear", "log"), licence = SECTOR_V$licence, section = SECTOR_V$section,
                                                  prov = SECTOR_V$prov, phase = SECTOR_V$phase)),
      `dict.units` = sector_tbl_spec(c(id = "s!", short = "s!", long = "s!", unit_kind = "s!"), unique = "id"),
      `dict.sources` = sector_tbl_spec(c(id = "s!", short = "s!", citation = "s!", publisher = "s", edition = "s", url = "s", licence = "s!",
                                         attribution = "s", accessed = "s", transform = "s", issues = "s"), unique = "id",
                                       values = list(licence = SECTOR_V$licence)),
      `dict.caveats` = sector_tbl_spec(c(id = "s!", text = "s!"), unique = "id"),
      `dict.themes` = sector_tbl_spec(c(slug = "s!", label = "s!", description = "s", order = "i!", default_var = "s!"), unique = "slug",
                                      values = list(slug = SECTOR_V$theme)),
      `dict.families` = sector_tbl_spec(c(id = "s!", label = "s!", section = "s"), unique = "id",
                                        values = list(id = SECTOR_V$family, section = SECTOR_V$section)),
      `dict.cells` = sector_tbl_spec(c(slug = "s!", cell_blurb = "s", section_cards = "s"), unique = "slug"),
      `dict.techs` = sector_tbl_spec(c(slug = "s!", label = "s!", tech_blurb = "s"), unique = "slug"),
      `dict.item_units` = sector_tbl_spec(c(var = "i!@vars", item = "i!@items", unit = "s!"), unique = c("var", "item")),
      rosters = sector_tbl_spec(c(roster = "s!", geo = "i!@geo"), unique = c("roster", "geo")),
      coverage = sector_tbl_spec(c(item = "i!@items", family = "s!", status = "s!", n_geo = "i!", reason = "s", sources = "s"),
                                 unique = c("item", "family"), values = list(family = SECTOR_V$family, status = SECTOR_V$cov)),
      avail = sector_tbl_spec(c(var = "i!@vars", item = "i!@items", chunk = "s!", year_min = "i", year_max = "i", n_years = "i!", n_geo = "i!"),
                              unique = c("var", "item", "chunk"), values = list(chunk = SECTOR_V$chunk)),
      summary = sector_tbl_spec(c(item = "i!@items", var = "i!@vars", year = "i!", eu_mode = "s!", W = "f", basis = "s!", n = "i!",
                                  top1_geo = "i@geo", top1_share = "f", top3_share = "f", hhi = "f", hhi_lower_bound = "b", eff_n = "f",
                                  china = "f", core = "f", partner = "f", rest = "f", unattributed = "f", growth = "f", growth_kind = "s"),
                                unique = c("item", "var", "year", "eu_mode"), values = list(eu_mode = SECTOR_V$eu_mode, basis = SECTOR_V$basis)),
      reconcile = sector_tbl_spec(c(item = "i!@items", year = "i!", matrix_total = "f", matrix_china = "f", matrix_hhi = "f",
                                    reported_total = "f", reported_china = "f", reported_hhi = "f", n_reporters = "i", warn = "b!"),
                                  unique = c("item", "year"))
    ),
    facts = list(`facts.obs` = obs, `facts.index` = index, `facts.world` = world),
    trade = list(
      `trade.flows` = sector_tbl_spec(c(item = "i!@items", exporter = "i!@geo", importer = "i!@geo", v2020 = "f", v2024 = "f", s2020 = "s!", s2024 = "s!"),
                                      sorted = c("item", "exporter"), unique = c("item", "exporter", "importer"),
                                      values = list(s2020 = SECTOR_V$flow, s2024 = SECTOR_V$flow)),
      `trade.declared` = sector_tbl_spec(c(item = "i!@items", geo = "i!@geo", year = "i!", imports_declared = "b!", exports_declared = "b!"),
                                         unique = c("item", "geo", "year")),
      `trade.codes` = sector_tbl_spec(c(item = "i!@items", sub_sector = "s!", hs6 = "s!", description = "s", shared_n = "i!", caveat = "s"),
                                      unique = c("item", "hs6")),
      `trade.excluded` = sector_tbl_spec(c(item = "i!@items", sub_sector = "s", hs6 = "s!", reason = "s!"))
    ),
    policy = list(
      `policy.pams` = sector_tbl_spec(c(id = "i!", geo = "i!@geo", year = "i", status = "s", jurisdiction = "s", type = "s", tech_mask = "i!",
                                        tech_src = "s", stage_mask = "i!", cross_cutting = "b!"), unique = "id",
                                      values = list(tech_src = SECTOR_V$tech_src)),
      `policy.tags` = list(kind = "strv"),
      `policy.cat` = sector_tbl_spec(c(geo = "i!@geo", overall = "s", policies_action = "s", domestic_target = "s", fair_share = "s",
                                       climate_finance = "s", ndc = "s", updated = "s"), unique = "geo")
    ),
    policy_text = list(`policy.text` = sector_tbl_spec(c(id = "i!", title = "s", url = "s"), unique = "id")),
    series = list(
      `series.obs` = obs, `series.index` = index, `series.world` = world,
      `series.monthly` = sector_tbl_spec(c(var = "i!@vars", item = "i!@items", geo = "i!@geo", year = "i!", month = "i!", value = "f", status = "s!"),
                                         sorted = c("var", "item", "geo", "year", "month"), unique = c("var", "item", "geo", "year", "month"),
                                         values = list(status = SECTOR_V$status)),
      `series.capacity` = sector_tbl_spec(c(item = "i!@items", product = "s!", unit = "s!", geo = "i!@geo", year = "i!", cap_status = "s!",
                                            value = "f", status = "s!"), unique = c("item", "product", "geo", "year", "cap_status"),
                                          values = list(status = SECTOR_V$status, cap_status = SECTOR_V$cap_status), optional = TRUE)
    )
  )
}

sector_spec_of <- function(name) {
  s <- sector_chunk_specs()
  if (startsWith(name, "sector-trade-")) return(s$trade)
  switch(name, `sector-meta` = s$meta, `sector-facts` = s$facts, `sector-policy` = s$policy,
         `sector-policy-text` = s$policy_text, `sector-series` = s$series, stop("no spec for ", name))
}

# A dotted path of a nested list.
sector_pick <- function(x, path) {
  for (p in strsplit(path, ".", fixed = TRUE)[[1]]) {
    if (!is.list(x) || is.null(x[[p]])) return(NULL)
    x <- x[[p]]
  }
  x
}

sector_check_table <- function(path, spec, t, sizes) {
  e <- character()
  if (!is.data.frame(t)) return(paste(path, "is not a table"))
  n <- nrow(t)
  for (cn in names(spec$cols)) {
    code <- spec$cols[[cn]]
    where <- paste0(path, ".", cn)
    v <- t[[cn]]
    if (is.null(v)) { e <- c(e, paste(where, "is missing")); next }
    type <- substr(code, 1, 1)
    ok_type <- switch(type, f = is.double(v), i = is.integer(v), b = is.logical(v), s = is.character(v))
    if (!ok_type) { e <- c(e, paste0(where, " has R type ", typeof(v), ", expected ", type)); next }
    req <- grepl("!", code, fixed = TRUE)
    na <- is.na(v)
    if (req && any(na)) e <- c(e, sprintf("%s has %d NA values", where, sum(na)))
    if (type == "f" && any(is.infinite(v))) e <- c(e, paste(where, "has infinite values"))
    ref <- regmatches(code, regexpr("@[a-z]+$", code))
    if (length(ref)) {
      size <- sizes[[sub("@", "", ref)]]
      bad <- !na & (v < 0L | (if (is.null(size)) FALSE else v >= size))
      if (any(bad)) e <- c(e, sprintf("%s has %d indices outside %s", where, sum(bad), ref))
    }
    allowed <- spec$values[[cn]]
    if (!is.null(allowed)) {
      bad <- !na & !v %in% allowed
      if (any(bad)) e <- c(e, sprintf("%s has values outside the vocabulary: %s", where, paste(utils::head(unique(v[bad]), 5), collapse = ", ")))
    }
  }
  if (length(e)) return(e)
  if (!is.null(spec$sorted) && n > 1) {
    o <- do.call(order, unname(as.list(t[spec$sorted])))
    if (!identical(o, seq_len(n))) {
      # Equal keys may come in any order; only a decrease is an error.
      d <- t[spec$sorted]
      dec <- FALSE
      for (i in 2:n) {
        for (c in spec$sorted) {
          if (d[[c]][i] > d[[c]][i - 1]) break
          if (d[[c]][i] < d[[c]][i - 1]) { dec <- TRUE; break }
        }
        if (dec) break
      }
      if (dec) e <- c(e, paste0(path, " is not sorted by (", paste(spec$sorted, collapse = ", "), ")"))
    }
  }
  if (!is.null(spec$unique) && anyDuplicated(t[spec$unique])) e <- c(e, paste0(path, " repeats the key (", paste(spec$unique, collapse = ", "), ")"))
  e
}

sector_check_obs_index <- function(prefix, obs, index) {
  e <- character()
  # NaN by design: nq, na_growth, and nsr where the source's zero-fill would read as a figure
  # (balance and self-sufficiency without EI production, EI successor years; integrator 2026-10-11).
  nan <- is.na(obs$value) & !obs$status %in% c("nq", "na_growth", "nsr")
  if (any(nan)) e <- c(e, sprintf("%s.obs has %d NA values with a status other than nq / na_growth / nsr", prefix, sum(nan)))
  if (nrow(index)) {
    expect_start <- c(0L, cumsum(index$n)[-nrow(index)])
    if (!identical(as.integer(index$start), as.integer(expect_start)) || any(index$n < 1) || sum(index$n) != nrow(obs)) {
      e <- c(e, paste0(prefix, ".index slices do not tile ", prefix, ".obs"))
    } else {
      r <- rep(seq_len(nrow(index)), index$n)
      if (any(obs$var != index$var[r] | obs$item != index$item[r] | obs$year != index$year[r])) {
        e <- c(e, paste0(prefix, ".index slices hold rows of another (var, item, year)"))
      }
    }
  } else if (nrow(obs)) e <- c(e, paste0(prefix, ".index is empty"))
  e
}

# Every error of a decoded chunk (objects = dash_osb_decode()$objects). sizes: list(geo, items,
# vars) row counts of the meta tables (taken from the chunk itself for sector-meta). var_ids: the
# measure catalogue (contract VAR_IDS) or NULL.
sector_check_chunk <- function(name, objects, sizes = list(), var_ids = NULL) {
  spec <- sector_spec_of(name)
  if (name == "sector-meta") {
    sizes <- list(geo = nrow(objects$dims$geo), items = nrow(objects$dims$items), vars = nrow(objects$dict$vars))
  }
  e <- character()
  for (path in names(spec)) {
    node <- spec[[path]]
    v <- sector_pick(objects, path)
    if (is.null(v)) {
      if (!isTRUE(node$optional)) e <- c(e, paste(path, "is missing"))
      next
    }
    if (identical(node$kind, "strv")) {
      if (!is.character(v) || anyNA(v)) e <- c(e, paste(path, "must be a character vector without NA"))
    } else {
      e <- c(e, sector_check_table(path, node, v, sizes))
    }
  }
  if (length(e)) return(e)
  if (name == "sector-facts") e <- c(e, sector_check_obs_index("facts", objects$facts$obs, objects$facts$index))
  if (name == "sector-series") e <- c(e, sector_check_obs_index("series", objects$series$obs, objects$series$index))
  if (startsWith(name, "sector-trade-")) {
    f <- objects$trade$flows
    for (y in SECTOR_TRADE_YEARS) {
      v <- f[[paste0("v", y)]]
      s <- f[[paste0("s", y)]]
      if (any(is.na(v) != (s == "none"))) e <- c(e, paste0("trade.flows: v", y, " NaN <=> s", y, " none is violated"))
      if (any(!is.na(v) & v <= 0)) e <- c(e, paste0("trade.flows.v", y, " has zero or negative values"))
    }
    if (any(f$s2020 == "none" & f$s2024 == "none")) e <- c(e, "trade.flows has rows with no observed value")
  }
  if (name == "sector-meta") {
    g <- objects$dims$geo
    re <- c(country = "^[A-Z]{3}$", eu = "^EUU$", world = "^WLD$", region = "^R:[^:]+:.+$", entity = "^E:[^:]+:.+$", residual = "^RES:.+$")
    bad <- !mapply(function(k, c) grepl(re[[k]], c), g$kind, g$code)
    if (any(bad)) e <- c(e, paste("dims.geo.code does not match its kind:", paste(utils::head(g$code[bad], 5), collapse = ", ")))
    if (!"WLD" %in% g$code) e <- c(e, "dims.geo has no WLD row")
    if (!"CHN" %in% g$code || g$group[g$code == "CHN"] != "china") e <- c(e, "dims.geo: CHN must be in group china")
    if (any(g$kind == "country" & (g$group == "na" | (g$group == "china" & g$code != "CHN")))) e <- c(e, "dims.geo: a country has group na or a second china")
    it <- objects$dims$items
    if (sum(it$axis == "none") != 1) e <- c(e, "dims.items needs exactly one axis none")
    cells <- it[it$axis == "cell", , drop = FALSE]
    tech_slug <- names(SECTOR_TECH_LABELS)[match(cells$tech, SECTOR_TECH_LABELS)]
    want <- paste0(tech_slug, "-", tolower(cells$stage))
    if (any(is.na(tech_slug)) || any(cells$slug != want) || any(cells$headline != (tech_slug != "semiconductors"))) {
      e <- c(e, "dims.items cell rows need slug = <tech>-<stage> of known labels, headline FALSE only for Semiconductors")
    }
    dv <- objects$dict$vars
    if (!is.null(var_ids) && length(setdiff(dv$id, var_ids))) e <- c(e, paste("dict.vars ids outside the catalogue:", paste(setdiff(dv$id, var_ids), collapse = ", ")))
    if (any(sub("\\..*$", "", dv$id) != dv$theme)) e <- c(e, "dict.vars.theme differs from the id prefix")
    if (length(setdiff(dv$unit, objects$dict$units$id))) e <- c(e, "dict.vars.unit not in dict.units")
    if (length(setdiff(dv$source, objects$dict$sources$id))) e <- c(e, "dict.vars.source not in dict.sources")
    cv <- unlist(strsplit(stats::na.omit(dv$caveats), "|", fixed = TRUE))
    if (length(setdiff(cv, objects$dict$caveats$id))) e <- c(e, "dict.vars.caveats not in dict.caveats")
    if (length(setdiff(objects$dict$item_units$unit, objects$dict$units$id))) e <- c(e, "dict.item_units.unit not in dict.units")
    if (!setequal(objects$dict$themes$slug, SECTOR_V$theme)) e <- c(e, "dict.themes must hold the 10 themes")
    if (length(setdiff(objects$dict$themes$default_var, dv$id))) e <- c(e, "dict.themes.default_var not in dict.vars")
    if (!setequal(objects$dict$techs$slug, SECTOR_TECH_SLUGS)) e <- c(e, "dict.techs must hold the 12 tech slugs")
  }
  e
}
