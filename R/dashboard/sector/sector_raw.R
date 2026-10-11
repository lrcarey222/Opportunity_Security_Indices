# Cross-sections read from data/raw through the pipeline's readers (spec_sector.md 3.2, 8.3 step 4):
# IEA Critical Minerals supply and demand, USGS mine production, the Red Book uranium series. Pure:
# the parsed tables come in; facts (and their parity rows) come out.

# IEA Critical Minerals Dataset sheet 2 (read_iea_critical_minerals()): mining and refining by
# producing economy, the base case. country_iso3: function(names) -> ISO3. Returns list(facts,
# world (the "Total" rows), residual (Rest of world), top3 (IEA's "Top 3 share" rows), unmatched).
sector_iea_cm_supply <- function(critical, country_iso3, years = c(2025L, 2030L, 2035L, 2040L)) {
  s <- critical[grepl("^2 ", critical$Pillar), , drop = FALSE]
  if (!nrow(s)) stop("IEA Critical Minerals: no sheet 2 (Total supply) rows")
  step <- ifelse(grepl("Mining", s$Mineral, ignore.case = TRUE), "production.mining",
                 ifelse(grepl("Refining|Battery grade", s$Mineral, ignore.case = TRUE), "production.refining", NA_character_))
  if (anyNA(step)) stop("IEA Critical Minerals: unknown blocks ", paste(unique(s$Mineral[is.na(step)]), collapse = ", "))
  slug <- sector_mineral_slug(sub(" - .*$", "", s$Mineral))
  if (anyNA(slug)) stop("IEA Critical Minerals: unknown minerals ", paste(unique(s$Mineral[is.na(slug)]), collapse = ", "))
  label <- trimws(s$Sector.Country)
  kind <- ifelse(grepl("^Total", label), "total", ifelse(grepl("^Top 3", label), "top3",
                 ifelse(grepl("^Rest of world", label, ignore.case = TRUE), "rest", "country")))
  long <- dplyr::bind_rows(lapply(years, function(y) {
    col <- paste0("X", y)
    if (!col %in% names(s)) return(NULL)
    tibble::tibble(var = step, item_key = paste0("mineral:", slug), block = s$Mineral, label = label, kind = kind,
                   year = as.integer(y), value = as.numeric(s[[col]]))
  }))
  long <- long[!is.na(long$value), , drop = FALSE]
  blocks <- unique(long[c("var", "item_key", "block")])
  if (anyDuplicated(blocks[c("var", "item_key")])) stop("IEA Critical Minerals: two blocks for one mineral and step")
  for (i in seq_len(nrow(blocks))) {
    b <- long[long$block == blocks$block[i], , drop = FALSE]
    for (y in unique(b$year)) {
      if (sum(b$kind == "total" & b$year == y) != 1) stop("IEA Critical Minerals: block '", blocks$block[i], "' ", y, " needs one ^Total row")
    }
  }
  cty <- long[long$kind == "country", , drop = FALSE]
  iso <- country_iso3(unique(cty$label))
  names(iso) <- unique(cty$label)
  unmatched <- names(iso)[is.na(iso)]
  if (length(unmatched)) stop("IEA Critical Minerals: no ISO3 for ", paste(unmatched, collapse = ", "))
  cty$geo <- unname(iso[cty$label])
  facts <- tibble::tibble(var = cty$var, item_key = cty$item_key, geo = cty$geo, year = cty$year, value = cty$value,
                          status = ifelse(cty$value == 0, "zero", "obs"), obs_year = NA_integer_)
  tot <- long[long$kind == "total", , drop = FALSE]
  world <- tibble::tibble(var = tot$var, item_key = tot$item_key, year = tot$year, value = tot$value, basis = "source_world")
  list(facts = facts, world = world, residual = long[long$kind == "rest", , drop = FALSE],
       top3 = long[long$kind == "top3", , drop = FALSE])
}

# The IEA guard: the top-3 share recomputed from the country rows over the block's sum (country
# rows plus Rest of world; IEA's "Total" row is rounded to three significant figures and IEA's own
# top-3 share is not taken over it) equals IEA's "Top 3 share" within tol for every block and year.
# Returns the comparison table.
sector_iea_cm_top3_check <- function(sup, tol = 0.001) {
  f <- sup$facts
  key <- paste(f$var, f$item_key, f$year)
  rec <- tapply(f$value, key, function(v) sum(sort(v, decreasing = TRUE)[seq_len(min(3, length(v)))]))
  listed <- tapply(f$value, key, sum)
  r <- sup$residual
  rest <- stats::setNames(r$value, paste(r$var, r$item_key, r$year))
  t3 <- sup$top3
  k3 <- paste(t3$var, t3$item_key, t3$year)
  W <- unname(listed[k3]) + ifelse(is.na(rest[k3]), 0, unname(rest[k3]))
  out <- tibble::tibble(key = k3, iea = t3$value, recomputed = unname(rec[k3]) / W)
  out$diff <- abs(out$iea - out$recomputed)
  out$ok <- !is.na(out$diff) & out$diff <= tol
  out
}

# IEA Critical Minerals sheet 3.1: clean-energy demand by technology. Observed base year (the same
# in every scenario) plus the Stated Policies projections. tech_of: function(IEA labels) ->
# Strategic tech labels (NA outside the techs and for totals).
sector_iea_cm_demand <- function(critical, tech_of, base_year = 2025L, proj_years = c(2030L, 2035L, 2040L)) {
  d <- critical[grepl("^3\\.1", critical$Pillar), , drop = FALSE]
  if (!nrow(d)) stop("IEA Critical Minerals: no sheet 3.1 rows")
  steps <- d[grepl("Stated Policies", d$Scenario, ignore.case = TRUE), , drop = FALSE]
  slug <- sector_mineral_slug(steps$Mineral)
  keep <- !is.na(slug)
  steps <- steps[keep, , drop = FALSE]
  slug <- slug[keep]
  label <- trimws(steps$Sector.Country)
  is_total <- grepl("^Total", label)
  label[is_total] <- "Total clean technologies"
  base <- tibble::tibble(mineral = slug, tech = tech_of(steps$Sector.Country), label = label, scenario = "observed",
                         year = base_year, kt = as.numeric(steps[[paste0("X", base_year)]]))
  proj <- dplyr::bind_rows(lapply(proj_years, function(y) tibble::tibble(
    mineral = slug, tech = tech_of(steps$Sector.Country), label = label, scenario = "STEPS", year = y,
    kt = as.numeric(steps[[paste0("X", y)]]))))
  out <- dplyr::bind_rows(base, proj)
  out <- out[!is.na(out$kt), , drop = FALSE]
  out[!duplicated(out[c("mineral", "label", "scenario", "year")]), , drop = FALSE]
}

# IEA demand-sheet technology labels -> Strategic tech labels.
sector_iea_demand_tech <- function(x) {
  lx <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    grepl("^total", lx) ~ NA_character_,
    grepl("electric vehicle", lx) ~ "Electric Vehicles",
    grepl("battery storage|grid battery", lx) ~ "Batteries",
    grepl("solar", lx) ~ "Solar",
    grepl("wind", lx) ~ "Wind",
    grepl("network|grid", lx) ~ "Electric Grid",
    grepl("hydrogen|electroly", lx) ~ "Green Hydrogen",
    grepl("nuclear", lx) ~ "Nuclear",
    grepl("geothermal", lx) ~ "Geothermal",
    TRUE ~ NA_character_
  )
}

# USGS MCS mine production (the World Mine Production table of each chapter), one year. mcs: the
# long commodity table (all columns character); parse_value: usgs_mcs_parse_value; country_iso3:
# names -> ISO3; is_residual: names -> TRUE for aggregates and "Other countries". Returns
# list(facts, world, units (item_key -> USGS unit)).
sector_usgs_production <- function(mcs, parse_value, country_iso3, is_residual, year = 2025L) {
  spec <- tibble::tribble(
    ~slug, ~commodity,
    "lithium", "Lithium", "cobalt", "Cobalt", "graphite", "Graphite (Natural)", "rare-earths", "Rare Earths",
    "copper", "Copper", "manganese", "Manganese", "nickel", "Nickel", "zinc", "Zinc", "silver", "Silver",
    "chromium", "Chromium", "molybdenum", "Molybdenum"
  )
  rows <- mcs[mcs$Commodity %in% spec$commodity & mcs$Statistics == "Production" &
                grepl("^Mine production(: rounded)?$", mcs$Statistics_detail) &
                suppressWarnings(as.integer(mcs$Year)) == year & !grepl("United States", mcs$Section %||% ""), , drop = FALSE]
  if (!nrow(rows)) stop("USGS: no mine production rows for ", year)
  label <- trimws(rows$Country)
  world <- grepl("^world total", label, ignore.case = TRUE)
  resid <- !world & is_residual(label)
  parsed <- parse_value(rows$Value, rows$Notes)
  slug <- spec$slug[match(rows$Commodity, spec$commodity)]
  cty <- !world & !resid
  iso <- country_iso3(label[cty])
  if (anyNA(iso)) stop("USGS mine production: no ISO3 for ", paste(unique(label[cty][is.na(iso)]), collapse = ", "))
  st <- ifelse(is.na(parsed$value[cty]), "nq", ifelse(parsed$value[cty] == 0, "zero", "obs"))
  facts <- tibble::tibble(var = "production.mine_usgs", item_key = paste0("mineral:", slug[cty]), geo = iso, year = year,
                          value = ifelse(is.na(parsed$value[cty]), NaN, parsed$value[cty]), status = st, obs_year = NA_integer_)
  dup <- duplicated(facts[c("item_key", "geo")])
  if (any(dup)) stop("USGS mine production lists one economy twice: ", paste(facts$geo[dup], collapse = ", "))
  wv <- parsed$value[world]
  w <- tibble::tibble(var = "production.mine_usgs", item_key = paste0("mineral:", slug[world]), year = year, value = wv, basis = "source_world")
  # The world is the larger of the reported total and the listed rows (as usgs_mcs_world_reserves()).
  listed <- tapply(ifelse(is.na(parsed$value[!world]), 0, parsed$value[!world]), slug[!world], sum)
  w$value <- pmax(w$value, unname(listed[sub("^mineral:", "", w$item_key)]), na.rm = TRUE)
  units <- unique(tibble::tibble(item_key = paste0("mineral:", slug), unit = rows$Unit))
  if (anyDuplicated(units$item_key)) stop("USGS mine production: a mineral in two units")
  list(facts = facts, world = w, units = units)
}

# Red Book uranium mine production series (data/reference/uranium_production.csv): every year but
# the latest (which comes from data/processed). Zeros are true zeros.
sector_uranium_series <- function(ref, exclude_year) {
  u <- ref[ref$variable == "mine_production" & ref$year != exclude_year, , drop = FALSE]
  tibble::tibble(var = "production.uranium", item_key = "cell:nuclear-upstream", geo = u$iso3c, year = as.integer(u$year),
                 value = as.double(u$value), status = ifelse(u$value > 0, "obs", "zero"), obs_year = NA_integer_)
}
