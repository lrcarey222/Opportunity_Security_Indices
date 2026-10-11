# Sector Insights dictionary (config/sector_dictionary.yml): validation and the tables of
# sector-meta (spec_sector.md 8.5). Pure: the YAML arrives as a parsed list (yaml::read_yaml()).
#
# The dictionary holds one entry per measure (`vars`) plus the shared tables `units`, `sources`,
# `caveats`, `themes`, `families`, `techs`, `cells` and `item_units`. Caveat, source and data-year
# texts may carry {placeholders} that the exporter fills from the data (sector_fill_placeholders()).

SECTOR_THEMES <- c("trade", "resources", "production", "consumption", "investment", "manufacturing",
                   "prices", "technology", "policy", "development")
SECTOR_FAMILIES <- c("exports", "production", "capacity", "reserves", "investment", "consumption",
                     "prices", "policy", "technology", "context", "development")
# The lens families (spec 3.3 "Families with country data"): the ones the coverage glyph and the thin
# rule count. Policy (PAMS records) and context (UN alignment) are country-level for every cell, and
# technology is global, so they never make a cell rich.
SECTOR_LENS_FAMILIES <- c("exports", "production", "capacity", "reserves", "investment", "consumption", "prices")
SECTOR_SECTIONS <- c("snapshot", "who", "leaders", "trade", "investment", "production", "resources",
                     "demand", "prices", "policy", "technology", "context", "countries")
SECTOR_NATIVE_LEVELS <- c("country", "eu", "region", "entity", "world", "cell_global", "dyad")
SECTOR_ITEM_AXES <- c("cell", "mineral", "fuel", "commodity", "activity", "policy_tag", "none")
SECTOR_AGG_RULES <- c("sum", "ratio", "none")
SECTOR_WORLD_BASES <- c("source_world", "sum_all", "one", "sum_listed", "none")
SECTOR_LICENCES <- c("open", "attribution")
SECTOR_PROVENANCES <- c("P", "D", "X")
SECTOR_PHASES <- c("1", "2", "hold")
SECTOR_NORMS <- c("value", "share", "gdp")
SECTOR_LINK_STAGES <- c("Upstream", "Midstream", "Downstream", "any")
SECTOR_STATUS_CODES <- c("obs", "zero", "nsr", "nq", "mirror", "est", "na_growth")
SECTOR_NAN_STATUS <- c("nq", "na_growth")
SECTOR_TECH_SLUGS <- c("batteries", "coal", "electric-grid", "electric-vehicles", "gas", "geothermal",
                       "green-hydrogen", "nuclear", "oil", "solar", "wind", "semiconductors")
SECTOR_TECH_LABELS <- c(batteries = "Batteries", coal = "Coal", `electric-grid` = "Electric Grid",
                        `electric-vehicles` = "Electric Vehicles", gas = "Gas", geothermal = "Geothermal",
                        `green-hydrogen` = "Green Hydrogen", nuclear = "Nuclear", oil = "Oil", solar = "Solar",
                        wind = "Wind", semiconductors = "Semiconductors")
SECTOR_STAGE_SLUGS <- c("upstream", "midstream", "downstream")
SECTOR_STAGE_LABELS <- c(upstream = "Upstream", midstream = "Midstream", downstream = "Downstream")

# Fields every var entry must carry (others default to NULL / NA).
sector_dictionary_required_fields <- function() {
  c("id", "theme", "family", "label", "short", "unit", "data_year", "native_level", "item_axis", "agg",
    "world_basis", "source", "licence", "prov", "phase")
}

# Words that mark normative content; no var id, label or processed variable may carry them.
SECTOR_NORMATIVE_RE <- "(\\bindex\\b|\\bweight(s|ed)?\\b|contribution|\\boverall\\b|s-curve)"

sector_null_to_na <- function(x, na = NA_character_) if (is.null(x) || length(x) == 0) na else x

# Text in {placeholders}: list them, and fill them from a named list (values pasted as text).
sector_placeholders <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(character())
  unique(unlist(regmatches(x, gregexpr("\\{[a-z0-9_]+\\}", x))))
}

sector_fill_placeholders <- function(x, fill) {
  if (!length(fill) || all(is.na(x))) return(x)
  for (k in names(fill)) x <- gsub(paste0("{", k, "}"), as.character(fill[[k]]), x, fixed = TRUE)
  x
}

# Every problem of the parsed dictionary `d` (empty when valid). `var_ids`: the measure catalogue
# (contract.ts VAR_IDS) the ids must belong to; NULL skips that check.
sector_dictionary_validate <- function(d, var_ids = NULL) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  for (k in c("units", "sources", "caveats", "themes", "families", "techs", "cells", "vars")) {
    if (is.null(d[[k]]) || !length(d[[k]])) add("dictionary: missing or empty `", k, "`")
  }
  if (length(p)) return(p)
  ids_of <- function(tbl, key = "id") vapply(tbl, function(e) sector_null_to_na(e[[key]]), character(1))
  unit_ids <- ids_of(d$units)
  source_ids <- ids_of(d$sources)
  caveat_ids <- ids_of(d$caveats)
  family_ids <- ids_of(d$families)
  theme_ids <- ids_of(d$themes, "slug")
  for (nm in c("units", "sources", "caveats", "families")) {
    ids <- ids_of(d[[nm]])
    if (anyNA(ids)) add(nm, ": an entry without id")
    if (anyDuplicated(ids)) add(nm, ": duplicate id ", paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }
  for (u in d$units) {
    for (f in c("short", "long", "unit_kind")) if (is.null(u[[f]]) || !nzchar(u[[f]])) add("units ", u$id, ": no ", f)
  }
  for (s in d$sources) {
    for (f in c("short", "citation", "licence")) if (is.null(s[[f]]) || !nzchar(s[[f]])) add("sources ", s$id, ": no ", f)
    if (!is.null(s$licence) && !s$licence %in% SECTOR_LICENCES) add("sources ", s$id, ": licence ", s$licence, " is not public (open / attribution)")
  }
  for (cv in d$caveats) if (is.null(cv$text) || !nzchar(cv$text)) add("caveats ", cv$id, ": no text")
  if (!setequal(theme_ids, SECTOR_THEMES)) add("themes: expected exactly ", paste(SECTOR_THEMES, collapse = ", "))
  if (!setequal(family_ids, SECTOR_FAMILIES)) add("families: expected exactly ", paste(SECTOR_FAMILIES, collapse = ", "))
  tech_slugs <- ids_of(d$techs, "slug")
  if (!setequal(tech_slugs, SECTOR_TECH_SLUGS)) add("techs: expected the 12 tech slugs")

  var_ids_seen <- ids_of(d$vars)
  if (anyNA(var_ids_seen)) add("vars: an entry without id")
  if (anyDuplicated(var_ids_seen)) add("vars: duplicate id ", paste(unique(var_ids_seen[duplicated(var_ids_seen)]), collapse = ", "))
  for (v in d$vars) {
    id <- sector_null_to_na(v$id)
    where <- paste0("var ", id, ": ")
    for (f in sector_dictionary_required_fields()) {
      if (is.null(v[[f]]) || (is.character(v[[f]]) && !nzchar(v[[f]][1]))) add(where, "no ", f)
    }
    if (!is.null(var_ids) && !is.na(id) && !id %in% var_ids) add(where, "not in the measure catalogue (contract.ts VAR_IDS)")
    if (!is.na(id) && !identical(sub("\\..*$", "", id), v$theme)) add(where, "theme ", v$theme, " differs from the id prefix")
    if (!is.null(v$theme) && !v$theme %in% SECTOR_THEMES) add(where, "unknown theme ", v$theme)
    if (!is.null(v$family) && !v$family %in% family_ids) add(where, "unknown family ", v$family)
    if (!is.null(v$unit) && !v$unit %in% unit_ids) add(where, "unit ", v$unit, " is not in units")
    if (!is.null(v$source) && !v$source %in% source_ids) add(where, "source ", v$source, " is not in sources")
    if (!is.null(v$roster) && !is.character(v$roster)) add(where, "roster must be text or null")
    bad_cv <- setdiff(unlist(v$caveats), caveat_ids)
    if (length(bad_cv)) add(where, "caveats not in caveats: ", paste(bad_cv, collapse = ", "))
    chk <- function(field, allowed) {
      x <- v[[field]]
      if (!is.null(x) && !as.character(x) %in% allowed) add(where, field, " ", x, " is not one of ", paste(allowed, collapse = " | "))
    }
    chk("native_level", SECTOR_NATIVE_LEVELS)
    chk("item_axis", SECTOR_ITEM_AXES)
    chk("agg", SECTOR_AGG_RULES)
    chk("world_basis", SECTOR_WORLD_BASES)
    chk("licence", SECTOR_LICENCES)
    chk("prov", SECTOR_PROVENANCES)
    chk("phase", SECTOR_PHASES)
    if (!is.null(v$section)) chk("section", SECTOR_SECTIONS)
    if (!is.null(v$link_stage)) chk("link_stage", SECTOR_LINK_STAGES)
    if (!is.null(v$scale)) chk("scale", c("linear", "log"))
    norms <- unlist(v$norms)
    if (length(setdiff(norms, SECTOR_NORMS))) add(where, "unknown norms ", paste(setdiff(norms, SECTOR_NORMS), collapse = ", "))
    # Legal normalisations: a share needs a world basis and an additive measure; % of GDP is for
    # additive country measures only.
    if ("share" %in% norms && (identical(v$world_basis, "none") || !identical(v$agg, "sum"))) add(where, "norm share needs agg sum and a world basis")
    if ("gdp" %in% norms && (!identical(v$agg, "sum") || !identical(v$native_level, "country"))) add(where, "norm gdp needs an additive country measure")
    if (identical(v$agg, "ratio") && !identical(v$world_basis, "none")) add(where, "a ratio measure has no world basis")
    if (!identical(v$item_axis, "cell") && !identical(v$item_axis, "none") && !identical(v$native_level, "world") &&
        !identical(v$native_level, "dyad") && is.null(v$link_stage)) {
      add(where, "item axis ", v$item_axis, " needs link_stage")
    }
    if (!is.null(v$display_scale) && (!is.numeric(v$display_scale) || v$display_scale <= 0)) add(where, "display_scale must be a positive number")
    if (isTRUE(v$year_field) && !is.null(v$years) && length(unlist(v$years)) != 1) add(where, "a year_field measure has one nominal year")
    if (is.null(v$years) || !length(unlist(v$years))) add(where, "no years")
    pr <- v$parity
    if (is.null(pr)) add(where, "no parity rule")
    else if (identical(v$prov, "P") && is.null(pr$table)) add(where, "prov P needs parity table and variable")
    else if (identical(v$prov, "X") && is.null(pr$none) && is.null(pr$cross_check)) add(where, "prov X needs parity none (with a reason) or cross_check")
    # No normative content.
    probe <- c(id, v$label, v$short, if (!is.null(pr)) c(pr$table, pr$variable))
    probe <- probe[!is.na(probe)]
    if (any(grepl(SECTOR_NORMATIVE_RE, probe, ignore.case = TRUE))) add(where, "names an index, weight or contribution")
  }
  default_vars <- vapply(d$themes, function(t) sector_null_to_na(t$default_var), character(1))
  if (length(setdiff(default_vars, var_ids_seen))) add("themes: default_var not in vars: ", paste(setdiff(default_vars, var_ids_seen), collapse = ", "))
  for (iu in d$item_units %||% list()) {
    if (!iu$var %in% var_ids_seen) add("item_units: unknown var ", iu$var)
    if (!iu$unit %in% unit_ids) add("item_units: unknown unit ", iu$unit)
  }
  for (u in unlist(d$price_unit_ids)) if (!u %in% unit_ids) add("price_unit_ids: unknown unit ", u)
  # Each cell has at most one primary measure.
  prim <- unlist(lapply(d$vars, function(v) unlist(v$primary_for)))
  if (anyDuplicated(prim)) add("primary_for: cells with two primary measures: ", paste(unique(prim[duplicated(prim)]), collapse = ", "))
  p
}

# Pipe-joined text of a YAML list field ("2020|2024"), NA when empty.
sector_pipe <- function(x) {
  x <- unlist(x)
  if (is.null(x) || !length(x)) NA_character_ else paste(as.character(x), collapse = "|")
}

# The dictionary as tables. `fill`: named values for {placeholders}. Returns list(vars, units,
# sources, caveats, themes, families, techs, cells, item_units, price_unit_ids). `vars` carries the
# contract columns of dict.vars plus internal ones (link_stage, parity_*), which the chunk builder
# drops.
sector_dictionary_tables <- function(d, fill = list()) {
  chr <- function(x) sector_null_to_na(x)
  num <- function(x) if (is.null(x)) NA_real_ else as.double(x)
  int <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
  vars <- dplyr::bind_rows(lapply(d$vars, function(v) {
    pr <- v$parity %||% list()
    tibble::tibble(
      id = v$id, theme = v$theme, family = v$family, label = v$label, short = v$short,
      definition = chr(v$definition), unit = v$unit, data_year = as.character(v$data_year),
      years = sector_pipe(v$years), year_field = isTRUE(v$year_field), native_level = v$native_level,
      item_axis = v$item_axis, agg = v$agg, world_basis = v$world_basis, norms = sector_pipe(v$norms),
      scale = chr(v$scale), higher_is = chr(v$higher_is), sign_note = chr(v$sign_note),
      display_scale = if (is.null(v$display_scale)) 1 else as.double(v$display_scale),
      source = v$source, roster = chr(v$roster), licence = v$licence, caveats = sector_pipe(v$caveats),
      primary_for = sector_pipe(v$primary_for), section = chr(v$section), stale_years = int(v$stale_years),
      min_base = num(v$min_base), feeds = sector_pipe(v$feeds), prov = v$prov, prov_detail = chr(v$prov_detail),
      phase = as.character(v$phase),
      projection_from = int(v$projection_from),
      series_window = NA_character_,
      link_stage = chr(v$link_stage),
      parity_table = chr(pr$table), parity_variable = chr(pr$variable), parity_formula = chr(pr$formula),
      parity_cross_check = chr(pr$cross_check), parity_none = chr(pr$none)
    )
  }))
  vars$data_year <- sector_fill_placeholders(vars$data_year, fill)
  units <- dplyr::bind_rows(lapply(d$units, function(u) tibble::tibble(id = u$id, short = u$short, long = u$long, unit_kind = u$unit_kind)))
  sources <- dplyr::bind_rows(lapply(d$sources, function(s) tibble::tibble(
    id = s$id, short = s$short, citation = s$citation, publisher = chr(s$publisher), edition = chr(s$edition),
    url = chr(s$url), licence = s$licence, attribution = chr(s$attribution), accessed = chr(s$accessed),
    transform = chr(s$transform), issues = chr(s$issues)
  )))
  for (f in c("citation", "edition", "accessed", "transform", "issues")) sources[[f]] <- sector_fill_placeholders(sources[[f]], fill)
  caveats <- dplyr::bind_rows(lapply(d$caveats, function(c) tibble::tibble(id = c$id, text = c$text)))
  caveats$text <- sector_fill_placeholders(caveats$text, fill)
  themes <- dplyr::bind_rows(lapply(d$themes, function(t) tibble::tibble(
    slug = t$slug, label = t$label, description = chr(t$description), order = as.integer(t$order), default_var = t$default_var
  )))
  themes <- themes[order(themes$order), , drop = FALSE]
  families <- dplyr::bind_rows(lapply(d$families, function(f) tibble::tibble(id = f$id, label = f$label, section = chr(f$section))))
  techs <- dplyr::bind_rows(lapply(d$techs, function(t) tibble::tibble(slug = t$slug, label = unname(SECTOR_TECH_LABELS[t$slug]), tech_blurb = chr(t$tech_blurb))))
  techs <- techs[match(SECTOR_TECH_SLUGS, techs$slug), , drop = FALSE]
  cells <- dplyr::bind_rows(lapply(d$cells, function(c) tibble::tibble(slug = c$slug, cell_blurb = chr(c$cell_blurb), section_cards = chr(c$section_cards))))
  item_units <- dplyr::bind_rows(lapply(d$item_units %||% list(), function(u) tibble::tibble(var = u$var, axis = u$axis, item = u$item, unit = u$unit)))
  if (!nrow(item_units)) item_units <- tibble::tibble(var = character(), axis = character(), item = character(), unit = character())
  price_unit_ids <- unlist(d$price_unit_ids %||% list())
  list(vars = vars, units = units, sources = sources, caveats = caveats, themes = themes, families = families,
       techs = techs, cells = cells, item_units = item_units, price_unit_ids = price_unit_ids)
}

# Placeholders still unfilled in the tables (the export stops on any).
sector_dictionary_unfilled <- function(tabs) {
  sector_placeholders(c(tabs$vars$data_year, tabs$caveats$text, tabs$sources$citation, tabs$sources$edition,
                        tabs$sources$accessed, tabs$sources$transform, tabs$sources$issues))
}
