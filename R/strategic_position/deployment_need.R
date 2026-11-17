# Deployment Need (spec Part V.2 and VI; design D12; docs/strategic_position.md, "Deployment Need").
#
# How much additional deployment does a country need, at the Deployment & Use stage only, to meet expected
# energy-system requirements or its own stated goals? Three families, each from measures attributed to
# technologies in config/deployment_need.yml:
#   dn_demand      demand adequacy (scenario current_policy): electricity-demand growth, the technology's
#                  generation growth, EV sales-share growth, oil demand growth;
#   dn_policy_gap  climate / policy gap (scenario national_commitments): projected 2030 shortfall against
#                  the country's own targets and approved plans (Ember 2030 target tracker);
#   dn_system      system need: imported fuel the technology displaces, flexibility need (variable-renewable
#                  share), firm-capacity need (dispatchable capacity per unit of average load).
# Each measure is ranked within the technology across countries (mid-rank percentile, higher = more need);
# a family's value (dn_<family>_score) is the mean of its observed measure percentiles and the family
# percentile (dn_<family>) is the mid-rank of that value within the technology; deployment_need is the
# mean of the observed family percentiles. Missing evidence stays NA (never zero), with basis =
# not_assessed. A climate-aligned value is carried only as a separately labelled column
# (dn_climate_aligned, World, global_context) and never enters the score. Deployment Need is never
# combined with supply_chain_need (spec VI).
#
# Fossil technologies (Coal, Gas, Oil) go through exactly the same code path; what each measure says
# about them is set by the attribution in config/deployment_need.yml (e.g. coal and gas plants displace
# imported fuel only in proportion to how much of their own fuel is produced at home).
#
# Pure functions: no IO. Needs sp_midrank_pct() and sp_row_mean_available() from
# R/strategic_position/sp_helpers.R and dplyr.

DN_FAMILIES <- c("dn_demand", "dn_policy_gap", "dn_system")
# The family of measures that feed only the deployment case tag (dn_cases()), never deployment_need.
DN_CASE_ONLY <- "case_only"
# The deployment cases, in tie-breaking order (dn_cases()).
DN_CASES <- c("security", "climate", "demand", "integration")
# Strongest first. A family's basis is the weakest basis among its observed measures (conservative), and
# the row's dn_basis the weakest among its observed families.
DN_BASIS_LEVELS <- c("national", "regional_proxy", "observed_trend", "global_context", "not_assessed")
DN_SCENARIOS <- c("current_policy", "national_commitments", "climate_aligned")
DN_MEASURE_COLS <- c("iso3", "tech", "family", "measure", "value", "basis", "scenario", "source",
                     "year_from", "year_to", "context")

dn_or <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# data.frame() that also works with zero rows: length-1 arguments are recycled to the length of the first
# argument (so a scalar label beside an empty column gives an empty frame, not an error).
dn_df <- function(..., stringsAsFactors = FALSE) {
  a <- list(...)
  n <- length(a[[1]])
  a <- lapply(a, function(x) if (length(x) == 1L && n != 1L) rep(x, n) else x)
  as.data.frame(a, stringsAsFactors = FALSE, optional = TRUE)
}

# An empty measure table with the standard columns.
dn_empty_measures <- function() {
  dn_df(iso3 = character(), tech = character(), family = character(), measure = character(),
             value = numeric(), basis = character(), scenario = character(), source = character(),
             year_from = integer(), year_to = integer(), context = character(), stringsAsFactors = FALSE)
}

# The family a measure belongs to and its scenario label, from the config.
dn_measure_meta <- function(cfg, measure) {
  m <- cfg$measures[[measure]]
  if (is.null(m)) stop("deployment_need: measure '", measure, "' is not defined in the config.")
  fam <- m$family
  if (!fam %in% c(DN_FAMILIES, DN_CASE_ONLY)) stop("deployment_need: measure '", measure, "' has unknown family '", fam, "'.")
  # a case-only measure (the deployment case tag, dn_cases()) never enters deployment_need
  scen <- if (fam == DN_CASE_ONLY) dn_or(m$scenario, "current_policy") else cfg$families[[fam]]$scenario
  list(family = fam, scenario = scen, direction = as.numeric(dn_or(m$direction, 1)))
}

# The technologies a measure is attributed to: the names of a mapping (tech -> details) or the values of
# a plain list.
dn_measure_techs <- function(m) {
  t <- m$techs
  if (is.null(t)) return(character())
  if (!is.null(names(t))) names(t) else as.character(unlist(t))
}

# Repeat per-country rows (iso3 + measure columns) once for each technology in `techs`.
dn_expand_techs <- function(rows, techs) {
  if (nrow(rows) == 0 || length(techs) == 0) return(rows[0, , drop = FALSE])
  out <- rows[rep(seq_len(nrow(rows)), times = length(techs)), , drop = FALSE]
  out$tech <- rep(techs, each = nrow(rows))
  rownames(out) <- NULL
  out
}

# Fill the standard columns of a measure table from its config entry.
dn_finish_measure <- function(rows, cfg, measure) {
  if (nrow(rows) == 0) return(dn_empty_measures())
  meta <- dn_measure_meta(cfg, measure)
  rows$measure <- measure
  rows$family <- meta$family
  rows$scenario <- meta$scenario
  for (col in setdiff(DN_MEASURE_COLS, names(rows))) rows[[col]] <- NA
  rows$year_from <- as.integer(rows$year_from)
  rows$year_to <- as.integer(rows$year_to)
  as.data.frame(rows[, DN_MEASURE_COLS], stringsAsFactors = FALSE)
}

dn_cagr_pct <- function(v_to, v_from, years) {
  out <- rep(NA_real_, length(v_to))
  if (length(out) == 0) return(out)
  years <- rep_len(as.numeric(years), length(out))
  ok <- !is.na(v_to) & !is.na(v_from) & !is.na(years) & v_from > 0 & v_to >= 0 & years > 0
  out[ok] <- 100 * ((v_to[ok] / v_from[ok])^(1 / years[ok]) - 1)
  out
}

# ---- Inputs --------------------------------------------------------------------------------------------

# Ember yearly electricity data (July-2026 layout) -> iso3, year, source, generation_twh, capacity_gw,
# share_pct for countries and economies only (regions and aggregates dropped).
dn_ember_prepare <- function(raw) {
  need <- c("ISO 3 code", "Year", "Area type", "Electricity source", "Generation (TWh)", "Capacity (GW)",
            "Share of generation (%)")
  miss <- setdiff(need, names(raw))
  if (length(miss) > 0) stop("Ember yearly data is missing columns: ", paste(miss, collapse = ", "))
  out <- data.frame(
    iso3 = toupper(trimws(as.character(raw[["ISO 3 code"]]))),
    year = as.integer(raw[["Year"]]),
    area_type = as.character(raw[["Area type"]]),
    source = as.character(raw[["Electricity source"]]),
    generation_twh = suppressWarnings(as.numeric(raw[["Generation (TWh)"]])),
    capacity_gw = suppressWarnings(as.numeric(raw[["Capacity (GW)"]])),
    share_pct = suppressWarnings(as.numeric(raw[["Share of generation (%)"]])),
    stringsAsFactors = FALSE
  )
  out <- out[out$area_type %in% "Country or economy" & !is.na(out$iso3) & nzchar(out$iso3) & !is.na(out$year), ]
  out$area_type <- NULL
  out <- out[!duplicated(out[, c("iso3", "year", "source")]), ]
  rownames(out) <- NULL
  out
}

# Sum of an Ember column over `sources` per iso3 x year, for every country-year with a "Total generation"
# row. Ember's generation by source is a complete breakdown, so a source with no row in such a year is a
# structural zero. A present row with a missing value makes the sum missing; for capacity, a missing value
# on a row that generated nothing counts as zero.
dn_ember_sum <- function(ember, sources, col = c("generation_twh", "capacity_gw")) {
  col <- match.arg(col)
  frame <- ember[ember$source == "Total generation", c("iso3", "year")]
  sub <- ember[ember$source %in% sources, c("iso3", "year", "generation_twh", "capacity_gw")]
  if (nrow(sub) > 0) {
    v <- sub[[col]]
    if (col == "capacity_gw") {
      v[is.na(v) & !is.na(sub$generation_twh) & sub$generation_twh == 0] <- 0
    }
    sub$v <- v
    agg <- dplyr::summarise(dplyr::group_by(sub, .data$iso3, .data$year),
                            value = if (any(is.na(.data$v))) NA_real_ else sum(.data$v), n_rows = dplyr::n(),
                            .groups = "drop")
  } else {
    agg <- dn_df(iso3 = character(), year = integer(), value = numeric(), n_rows = integer())
  }
  out <- dplyr::left_join(frame, agg, by = c("iso3", "year"))
  out$value[is.na(out$n_rows)] <- 0
  out$n_rows <- NULL
  as.data.frame(out)
}

# For each iso3: the latest year where every column in `cols` is observed (year_to), those values
# (<col>_to) and the values `k` years earlier (<col>_from, NA when absent).
dn_window <- function(df, k, cols = "value") {
  ok <- stats::complete.cases(df[, cols, drop = FALSE])
  d <- df[ok, c("iso3", "year", cols), drop = FALSE]
  if (nrow(d) == 0) {
    out <- dn_df(iso3 = character(), year_to = integer(), year_from = integer())
    for (cc in cols) out[[paste0(cc, "_to")]] <- numeric()
    for (cc in cols) out[[paste0(cc, "_from")]] <- numeric()
    return(out)
  }
  last <- stats::aggregate(year ~ iso3, data = d, FUN = max)
  to <- merge(last, d, by = c("iso3", "year"))
  names(to)[names(to) == "year"] <- "year_to"
  for (cc in cols) names(to)[names(to) == cc] <- paste0(cc, "_to")
  to$year_from <- to$year_to - as.integer(k)
  from <- d
  names(from)[names(from) == "year"] <- "year_from"
  for (cc in cols) names(from)[names(from) == cc] <- paste0(cc, "_from")
  out <- merge(to, from, by = c("iso3", "year_from"), all.x = TRUE)
  out[order(out$iso3), c("iso3", "year_to", "year_from", paste0(cols, "_to"), paste0(cols, "_from")), drop = FALSE]
}

# WEO 2025 Regions values (climate_src_weo_values_tbl: region, series, scenario, year, value) for the
# single-country regions, keyed by iso3 (climate_src_weo_geo_tbl rows with match == "own").
dn_weo_national <- function(weo_values, weo_geo) {
  own <- weo_geo[weo_geo$match %in% "own", c("iso3", "region")]
  out <- merge(own, weo_values, by = "region")
  out[, c("iso3", "region", "series", "scenario", "year", "value")]
}

# The WEO region each economy belongs to when it is not a single-country region (regional proxies: shown
# as context, never scored).
dn_weo_regional <- function(weo_values, weo_geo) {
  mem <- weo_geo[!weo_geo$match %in% "own", c("iso3", "region")]
  out <- merge(mem, weo_values, by = "region")
  out[, c("iso3", "region", "series", "scenario", "year", "value")]
}

dn_weo_value <- function(weo, series, scenario, year) {
  s <- weo[weo$series %in% series & weo$scenario %in% scenario & weo$year %in% year, , drop = FALSE]
  if (nrow(s) == 0) return(dn_df(iso3 = character(), value = numeric()))
  agg <- stats::aggregate(value ~ iso3, data = s, FUN = sum)
  # every requested series must be present
  n <- stats::aggregate(series ~ iso3, data = s, FUN = function(x) length(unique(x)))
  agg[agg$iso3 %in% n$iso3[n$series == length(unique(series))], , drop = FALSE]
}

# import_dependence_tbl (EI / EIA self-sufficiency, %) -> iso3, fuel, import_dependence in [0, 1]:
# 1 - self-sufficiency, floored at 0 (net exporters) and capped at 1.
dn_import_dependence <- function(import_dependence_tbl, fuels = c("Coal", "Gas", "Oil")) {
  d <- import_dependence_tbl[import_dependence_tbl$variable %in% "Self-sufficiency" &
                               import_dependence_tbl$tech %in% fuels, , drop = FALSE]
  iso <- if ("iso3c" %in% names(d)) d$iso3c else d$iso3
  out <- dn_df(iso3 = iso, fuel = d$tech, year = as.integer(d$Year),
                    import_dependence = pmin(1, pmax(0, 1 - d$value / 100)), stringsAsFactors = FALSE)
  out <- out[!is.na(out$iso3), , drop = FALSE]
  out[!duplicated(out[, c("iso3", "fuel")]), , drop = FALSE]
}

# One variable of the Energy Institute Statistical Review long file (Country, Year, ISO3166_alpha3, Var,
# Value) -> iso3, year, value, for rows with a three-letter ISO code (regional aggregates dropped).
dn_ei_series <- function(ei_long, var) {
  d <- ei_long[ei_long$Var %in% var, , drop = FALSE]
  iso <- toupper(trimws(as.character(d$ISO3166_alpha3)))
  out <- dn_df(iso3 = iso, year = as.integer(d$Year), value = suppressWarnings(as.numeric(d$Value)))
  out <- out[grepl("^[A-Z]{3}$", out$iso3) & !is.na(out$year) & !is.na(out$value), , drop = FALSE]
  out <- out[!duplicated(out[, c("iso3", "year")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Energy Institute Statistical Review wide workbook, sheet "Grid Scale BESS Capacity" (read with
# col_names = FALSE) -> iso3, year, capacity_gw (behind- plus front-of-the-meter, MW / 1000). Country
# rows only: the behind/front sub-rows, regional totals and "Rest of"/"Other" rows are dropped, as is any
# label `to_iso3` cannot map. `to_iso3`: a function from country names to ISO3 codes (NA when unknown).
dn_ei_bess_prepare <- function(sheet, to_iso3) {
  s <- as.data.frame(sheet, stringsAsFactors = FALSE)
  lab <- trimws(as.character(s[[1]]))
  hdr <- which(grepl("^Installed Capacity", lab, ignore.case = TRUE))
  if (length(hdr) == 0) stop("Grid Scale BESS Capacity sheet: no 'Installed Capacity' header row.")
  hdr <- hdr[1]
  yrs <- suppressWarnings(as.integer(round(as.numeric(unlist(s[hdr, -1])))))
  ycol <- which(!is.na(yrs) & yrs >= 1990 & yrs <= 2100) + 1L
  body <- s[seq.int(hdr + 1L, nrow(s)), , drop = FALSE]
  blab <- trimws(as.character(body[[1]]))
  skip <- is.na(blab) | !nzchar(blab) |
    grepl("^(Behind|Front)|^Total|^Rest of|^Other|^Source|^Note|^\\*|^World|^of which|^Growth", blab, ignore.case = TRUE)
  body <- body[!skip, , drop = FALSE]
  blab <- blab[!skip]
  iso <- to_iso3(blab)
  keep <- !is.na(iso)
  if (!any(keep)) return(dn_df(iso3 = character(), year = integer(), capacity_gw = numeric()))
  out <- lapply(ycol, function(j) {
    dn_df(iso3 = iso[keep], year = yrs[j - 1L],
          capacity_gw = suppressWarnings(as.numeric(as.character(body[[j]][keep]))) / 1000)
  })
  out <- do.call(rbind, out)
  out <- out[!is.na(out$capacity_gw), , drop = FALSE]
  out <- out[!duplicated(out[, c("iso3", "year")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# IEA Global EV Outlook country file (region_country, category, parameter, mode, powertrain, year, unit,
# value) -> iso3, category, year, value for one parameter x mode x powertrain. Regions and aggregates
# (World, Europe, ...) are dropped: `to_iso3` returns NA for them.
dn_gevo_prepare <- function(gevo, parameter, mode = "Cars", powertrain = "EV", to_iso3) {
  d <- gevo[gevo$parameter %in% parameter & gevo$mode %in% mode & gevo$powertrain %in% powertrain, , drop = FALSE]
  if (nrow(d) == 0) return(dn_df(iso3 = character(), category = character(), year = integer(), value = numeric()))
  reg <- as.character(d$region_country)
  iso <- rep(NA_character_, length(reg))
  ok <- !is.na(reg) & nzchar(reg)
  iso[ok] <- to_iso3(reg[ok])
  out <- dn_df(iso3 = iso, category = as.character(d$category), year = as.integer(d$year),
               value = suppressWarnings(as.numeric(d$value)))
  out <- out[!is.na(out$iso3) & !is.na(out$year) & !is.na(out$value), , drop = FALSE]
  out <- out[!duplicated(out[, c("iso3", "category", "year")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Capacity series per iso3 x key for the policy gap and the home market: every Ember source in
# `sources` (incl. aggregates such as Renewables) from the yearly data, plus key "Battery" from the EI
# grid-scale BESS series. Returns iso3, key, year, capacity_gw.
dn_capacity_series <- function(ember, sources, bess = NULL) {
  out <- lapply(sources, function(s) {
    d <- dn_ember_sum(ember, s, "capacity_gw")
    dn_df(iso3 = d$iso3, key = s, year = as.integer(d$year), capacity_gw = d$value)
  })
  if (!is.null(bess) && nrow(bess) > 0) {
    out[[length(out) + 1L]] <- dn_df(iso3 = bess$iso3, key = "Battery", year = as.integer(bess$year),
                                     capacity_gw = bess$capacity_gw)
  }
  out <- do.call(rbind, out)
  out <- out[!is.na(out$capacity_gw), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The installed power fleet (all sources) per iso3 x year: Ember "Total generation" capacity.
dn_fleet <- function(ember) {
  f <- ember[ember$source == "Total generation" & !is.na(ember$capacity_gw), c("iso3", "year", "capacity_gw")]
  names(f)[3] <- "fleet_gw"
  rownames(f) <- NULL
  f
}

# ---- Family A: demand adequacy -------------------------------------------------------------------------

# Electricity demand growth, % a year: WEO 2025 Current Policies generation 2024 -> 2035 for the
# single-country WEO regions (national), else Ember demand over the last trend_years (observed_trend).
dn_measure_demand_growth <- function(ember, weo_national, techs, cfg) {
  k <- as.integer(cfg$trend_years)
  dem <- ember[ember$source == "Demand", c("iso3", "year")]
  dem$value <- ember$generation_twh[ember$source == "Demand"]
  w <- dn_window(dem, k)
  w <- w[!is.na(w$value_from) & w$value_from >= cfg$min_system_twh, , drop = FALSE]
  obs <- dn_df(iso3 = w$iso3, value = dn_cagr_pct(w$value_to, w$value_from, k), basis = "observed_trend",
                    source = "Ember yearly electricity data: electricity demand", year_from = w$year_from,
                    year_to = w$year_to, stringsAsFactors = FALSE)
  obs <- obs[!is.na(obs$value), , drop = FALSE]
  obs$context <- sprintf("observed %d-%d: %.2f%%/yr", obs$year_from, obs$year_to, obs$value)

  base_y <- as.integer(cfg$scenario_base_year)
  scen_y <- as.integer(cfg$scenario_year)
  series <- cfg$weo_series$total
  cps <- cfg$weo_scenarios$current_policy
  g0 <- dn_weo_value(weo_national, series, "historical", base_y)
  g1 <- dn_weo_value(weo_national, series, cps, scen_y)
  nat <- merge(g0, g1, by = "iso3", suffixes = c("_from", "_to"))
  nat <- dn_df(iso3 = nat$iso3, value = dn_cagr_pct(nat$value_to, nat$value_from, scen_y - base_y),
                    basis = "national", source = "IEA WEO 2025 Current Policies (single-country region): electricity generation",
                    year_from = base_y, year_to = scen_y, stringsAsFactors = FALSE)
  nat <- nat[!is.na(nat$value), , drop = FALSE]
  if (nrow(nat) > 0) {
    ctx <- obs$context[match(nat$iso3, obs$iso3)]
    nat$context <- ifelse(is.na(ctx), sprintf("WEO Current Policies %d-%d: %.2f%%/yr", base_y, scen_y, nat$value),
                          sprintf("WEO Current Policies %d-%d: %.2f%%/yr; %s", base_y, scen_y, nat$value, ctx))
  } else {
    nat$context <- character()
  }
  rows <- rbind(nat, obs[!obs$iso3 %in% nat$iso3, , drop = FALSE])
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "demand_growth")
}

# Growth of the technology's generation as percentage points of total generation per year: WEO 2025
# Current Policies 2024 -> 2035 for the single-country regions (national; every source must have a WEO
# series), else Ember over the last trend_years (observed_trend). `tech_sources`: tech -> Ember sources.
dn_measure_output_growth <- function(ember, weo_national, tech_sources, cfg) {
  k <- as.integer(cfg$trend_years)
  base_y <- as.integer(cfg$scenario_base_year)
  scen_y <- as.integer(cfg$scenario_year)
  cps <- cfg$weo_scenarios$current_policy
  total <- ember[ember$source == "Total generation", c("iso3", "year", "generation_twh")]
  names(total)[3] <- "total"
  t0 <- dn_weo_value(weo_national, cfg$weo_series$total, "historical", base_y)
  out <- list()
  for (tech in names(tech_sources)) {
    src <- unlist(tech_sources[[tech]])
    g <- dn_ember_sum(ember, src, "generation_twh")
    names(g)[names(g) == "value"] <- "gen"
    d <- merge(g, total, by = c("iso3", "year"))
    w <- dn_window(d, k, c("gen", "total"))
    w <- w[!is.na(w$total_from) & w$total_from >= cfg$min_system_twh & !is.na(w$gen_from), , drop = FALSE]
    obs <- dn_df(iso3 = w$iso3, value = 100 * (w$gen_to - w$gen_from) / (k * w$total_from),
                      basis = "observed_trend", source = paste0("Ember yearly electricity data: generation (", paste(src, collapse = " + "), ")"),
                      year_from = w$year_from, year_to = w$year_to, stringsAsFactors = FALSE)
    obs$context <- sprintf("%s %.1f -> %.1f TWh (%d-%d) in a %.0f TWh system",
                           paste(src, collapse = "+"), w$gen_from, w$gen_to, w$year_from, w$year_to, w$total_from)

    series <- vapply(src, function(s) dn_or(cfg$weo_series[[s]], NA_character_), character(1))
    nat <- obs[0, , drop = FALSE]
    if (!anyNA(series)) {
      s0 <- dn_weo_value(weo_national, series, "historical", base_y)
      s1 <- dn_weo_value(weo_national, series, cps, scen_y)
      n <- merge(merge(s0, s1, by = "iso3", suffixes = c("_from", "_to")), t0, by = "iso3")
      if (nrow(n) > 0) {
        nat <- dn_df(iso3 = n$iso3, value = 100 * (n$value_to - n$value_from) / ((scen_y - base_y) * n$value),
                          basis = "national",
                          source = paste0("IEA WEO 2025 Current Policies (single-country region): generation (", paste(series, collapse = " + "), ")"),
                          year_from = base_y, year_to = scen_y, stringsAsFactors = FALSE)
        nat$context <- sprintf("WEO Current Policies %.0f -> %.0f TWh (%d-%d) in a %.0f TWh system",
                               n$value_from, n$value_to, base_y, scen_y, n$value)
        nat <- nat[!is.na(nat$value), , drop = FALSE]
      }
    }
    rows <- rbind(nat, obs[!obs$iso3 %in% nat$iso3, , drop = FALSE])
    if (nrow(rows) > 0) {
      rows$tech <- tech
      out[[tech]] <- rows
    }
  }
  if (length(out) == 0) return(dn_empty_measures())
  dn_finish_measure(do.call(rbind, out), cfg, "output_growth")
}

# EV share of new-car sales, change in percentage points a year. `gevo_share`: iso3, category
# ("Historical", "Projection-CPS", ...), year, value (percent) for Cars / EV. National where GEVO projects
# the country under Current Policies (latest historical year -> scenario_year), else the observed trend.
dn_measure_ev_share_growth <- function(gevo_share, cfg, techs = "Electric Vehicles", cps_category = "Projection-CPS") {
  k <- as.integer(cfg$trend_years)
  hist <- gevo_share[gevo_share$category %in% "Historical", c("iso3", "year", "value")]
  w <- dn_window(hist, k)
  w <- w[!is.na(w$value_from), , drop = FALSE]
  obs <- dn_df(iso3 = w$iso3, value = (w$value_to - w$value_from) / k, basis = "observed_trend",
                    source = "IEA Global EV Outlook 2026: EV share of new-car sales", year_from = w$year_from,
                    year_to = w$year_to, stringsAsFactors = FALSE)
  obs$context <- sprintf("EV sales share %.1f%% (%d) -> %.1f%% (%d)", w$value_from, w$year_from, w$value_to, w$year_to)
  latest <- dn_window(hist, 0)
  proj <- gevo_share[gevo_share$category %in% cps_category & gevo_share$year %in% as.integer(cfg$scenario_year),
                     c("iso3", "year", "value")]
  n <- merge(latest[, c("iso3", "year_to", "value_to")], proj, by = "iso3")
  nat <- dn_df(iso3 = n$iso3, value = (n$value - n$value_to) / (n$year - n$year_to), basis = "national",
                    source = "IEA Global EV Outlook 2026 Current Policies: EV share of new-car sales",
                    year_from = n$year_to, year_to = n$year, stringsAsFactors = FALSE)
  nat$context <- sprintf("EV sales share %.1f%% (%d) -> %.1f%% (%d, Current Policies)", n$value_to, n$year_to, n$value, n$year)
  rows <- rbind(nat, obs[!obs$iso3 %in% nat$iso3, , drop = FALSE])
  rows <- rows[!is.na(rows$value), , drop = FALSE]
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "ev_share_growth")
}

# Oil consumption growth, % a year, observed over trend_years. `oil`: iso3, year, value (EJ).
dn_measure_oil_demand_growth <- function(oil, cfg, techs = "Oil") {
  k <- as.integer(cfg$trend_years)
  w <- dn_window(oil, k)
  w <- w[!is.na(w$value_from) & w$value_from >= cfg$min_oil_ej, , drop = FALSE]
  rows <- dn_df(iso3 = w$iso3, value = dn_cagr_pct(w$value_to, w$value_from, k), basis = "observed_trend",
                     source = "Energy Institute Statistical Review of World Energy 2026: oil consumption",
                     year_from = w$year_from, year_to = w$year_to, stringsAsFactors = FALSE)
  rows$context <- sprintf("oil use %.2f -> %.2f EJ (%d-%d)", w$value_from, w$value_to, w$year_from, w$year_to)
  rows <- rows[!is.na(rows$value), , drop = FALSE]
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "oil_demand_growth")
}

# ---- Family B: climate / policy gap -------------------------------------------------------------------

dn_flag_true <- function(x) toupper(trimws(as.character(x))) %in% c("TRUE", "T", "1", "YES")

# National 2030 targets from the Ember tracker -> iso3, target_key, target_gw, target_year, target_kind,
# target_source. `cap_wide` = sheet capacity_target_wide (the tool's targets: Solar, Wind, total
# renewables); `raw_long` + `sources` = sheets raw_data_long and sources (approved national-plan capacities
# for the plan fuels). The EU row is not a country and is dropped.
dn_targets_prepare <- function(cap_wide, raw_long, sources, tcfg) {
  ty <- as.integer(tcfg$target_year)
  src <- as.data.frame(sources, stringsAsFactors = FALSE)
  src$SOURCE_ID <- as.character(src$SOURCE_ID)
  tool_src <- src[dn_flag_true(src$INCLUDE_IN_TOOL), , drop = FALSE]
  tool_label <- tapply(as.character(tool_src$SOURCE_NAME), tool_src$COUNTRY_CODE,
                       function(x) paste(unique(x[!is.na(x)]), collapse = "; "))

  w <- as.data.frame(cap_wide, check.names = FALSE, stringsAsFactors = FALSE)
  w <- w[!is.na(w$country_code) & nchar(w$country_code) == 3 & as.integer(w$target_year) %in% ty, , drop = FALSE]
  num <- function(col) if (col %in% names(w)) suppressWarnings(as.numeric(w[[col]])) else rep(NA_real_, nrow(w))
  onshore <- num("Onshore Wind")
  offshore <- num("Offshore Wind")
  wind <- num("Wind")
  wind_combined <- ifelse(!is.na(onshore), onshore + ifelse(is.na(offshore), 0, offshore), NA_real_)
  wind_note <- ifelse(!is.na(wind), "Wind target",
                      ifelse(!is.na(onshore) & !is.na(offshore), "onshore + offshore targets",
                             ifelse(!is.na(onshore), "onshore target (no offshore target)", NA_character_)))
  wind <- ifelse(is.na(wind), wind_combined, wind)
  label <- unname(tool_label[w$country_code])
  label <- ifelse(is.na(label) | !nzchar(label), "Ember 2030 Global Renewable Target Tracker",
                  paste0("Ember 2030 Global Renewable Target Tracker: ", label))
  mk <- function(key, value, note) {
    dn_df(iso3 = w$country_code, target_key = key, target_gw = value, target_year = ty,
               target_kind = "renewable_target", target_source = label, target_note = note, stringsAsFactors = FALSE)
  }
  tool <- rbind(mk("Solar", num("Solar"), "Solar target"),
                mk("Wind", wind, wind_note),
                mk("Renewables", num(tcfg$renewables_column), "total renewable-capacity target"))
  tool <- tool[!is.na(tool$target_gw), , drop = FALSE]

  rl <- as.data.frame(raw_long, stringsAsFactors = FALSE)
  rl$SOURCE_ID <- as.character(rl$SOURCE_ID)
  plan_map <- unlist(tcfg$plan_fuels)
  rl <- rl[rl$METRIC %in% "capacity_total_gw" & as.integer(rl$TARGET_YEAR) %in% ty &
             rl$FUEL_CATEGORY %in% names(plan_map) & !is.na(suppressWarnings(as.numeric(rl$VALUE))) &
             !is.na(rl$COUNTRY_CODE) & nchar(rl$COUNTRY_CODE) == 3, , drop = FALSE]
  rl <- merge(rl, src[, intersect(c("SOURCE_ID", "SOURCE_NAME", "PUBLISHER", "SOURCE_TYPE", "ANNOUNCEMENT_DATE"), names(src))],
              by = "SOURCE_ID")
  rl <- rl[rl$SOURCE_TYPE %in% unlist(tcfg$plan_source_types), , drop = FALSE]
  plan <- tool[0, , drop = FALSE]
  if (nrow(rl) > 0) {
    rl$announced <- as.Date(substr(as.character(rl$ANNOUNCEMENT_DATE), 1, 10))
    rl <- rl[order(rl$COUNTRY_CODE, rl$FUEL_CATEGORY, -as.numeric(rl$announced), -suppressWarnings(as.numeric(rl$SOURCE_ID))), ]
    rl <- rl[!duplicated(rl[, c("COUNTRY_CODE", "FUEL_CATEGORY")]), , drop = FALSE]
    plan <- dn_df(iso3 = rl$COUNTRY_CODE, target_key = unname(plan_map[rl$FUEL_CATEGORY]),
                       target_gw = as.numeric(rl$VALUE), target_year = ty, target_kind = "national_plan",
                       target_source = paste0("Ember target tracker, national plan: ", rl$SOURCE_NAME,
                                              ifelse(is.na(rl$announced), "", paste0(" (", format(rl$announced, "%Y-%m"), ")"))),
                       target_note = paste0(rl$FUEL_CATEGORY, " capacity in ", tolower(rl$SOURCE_TYPE)),
                       stringsAsFactors = FALSE)
  }
  out <- rbind(tool, plan)
  rownames(out) <- NULL
  out
}

# Installed capacity per iso3 x key: latest year with capacity (installed_year, installed_gw) and the
# recent build rate (GW a year over the last `k` years; NA when the earlier year is missing).
# `cap_series`: iso3, key, year, capacity_gw.
dn_installed_capacity <- function(cap_series, k) {
  out <- list()
  for (key in unique(cap_series$key)) {
    d <- cap_series[cap_series$key == key, c("iso3", "year", "capacity_gw")]
    names(d)[3] <- "value"
    w <- dn_window(d, k)
    if (nrow(w) == 0) next
    out[[key]] <- dn_df(iso3 = w$iso3, key = key, installed_year = w$year_to, installed_gw = w$value_to,
                             capacity_from = w$value_from, build_rate_gw = (w$value_to - w$value_from) / k,
                             stringsAsFactors = FALSE)
  }
  if (length(out) == 0) {
    return(dn_df(iso3 = character(), key = character(), installed_year = integer(), installed_gw = numeric(),
                      capacity_from = numeric(), build_rate_gw = numeric()))
  }
  do.call(rbind, out)
}

# Projected 2030 shortfall against each target at the recent build rate:
#   projected   = installed + build_rate x (target_year - installed_year)
#   shortfall   = target - projected (GW; negative = ahead of the target)
#   shortfall_share = 100 x shortfall / installed power fleet (all sources, same year), the measure;
#   required_rate = (target - installed) / years left; acceleration = required_rate / build_rate.
# `installed`: dn_installed_capacity(); `fleet`: iso3, year, fleet_gw; `installed_source`: target key ->
# installed key.
dn_target_shortfall <- function(targets, installed, fleet, installed_source) {
  key_map <- unlist(installed_source)
  t <- targets
  t$key <- unname(key_map[t$target_key])
  t <- t[!is.na(t$key), , drop = FALSE]
  d <- merge(t, installed, by = c("iso3", "key"))
  if (nrow(d) == 0) {
    return(cbind(targets[0, , drop = FALSE], data.frame(installed_year = integer(), installed_gw = numeric(),
      build_rate_gw = numeric(), fleet_gw = numeric(), years_left = numeric(), projected_gw = numeric(),
      shortfall_gw = numeric(), shortfall_share = numeric(), required_rate_gw = numeric(),
      acceleration = numeric(), target_met = logical())))
  }
  # fleet at the installed year, else the latest earlier year
  d$fleet_gw <- vapply(seq_len(nrow(d)), function(i) {
    f <- fleet[fleet$iso3 == d$iso3[i] & fleet$year <= d$installed_year[i] & !is.na(fleet$fleet_gw), , drop = FALSE]
    if (nrow(f) == 0) NA_real_ else f$fleet_gw[which.max(f$year)]
  }, numeric(1))
  d$years_left <- d$target_year - d$installed_year
  d$years_left[d$years_left <= 0] <- NA
  d$projected_gw <- d$installed_gw + d$build_rate_gw * d$years_left
  d$shortfall_gw <- d$target_gw - d$projected_gw
  d$shortfall_share <- ifelse(!is.na(d$fleet_gw) & d$fleet_gw > 0, 100 * d$shortfall_gw / d$fleet_gw, NA_real_)
  d$required_rate_gw <- (d$target_gw - d$installed_gw) / d$years_left
  d$acceleration <- ifelse(!is.na(d$build_rate_gw) & d$build_rate_gw > 0, d$required_rate_gw / d$build_rate_gw, NA_real_)
  d$target_met <- d$installed_gw >= d$target_gw
  d$key <- NULL
  d$capacity_from <- NULL
  rownames(d) <- NULL
  d
}

# A policy-gap measure from dn_target_shortfall(). `tech_keys`: named list tech -> target key, or a
# character vector of techs that all read `default_key`.
dn_measure_target <- function(shortfall, tech_keys, cfg, measure, default_key = NULL) {
  if (is.null(names(tech_keys))) {
    tech_keys <- stats::setNames(as.list(rep(default_key, length(tech_keys))), unlist(tech_keys))
  }
  out <- list()
  for (tech in names(tech_keys)) {
    s <- shortfall[shortfall$target_key %in% tech_keys[[tech]] & !is.na(shortfall$shortfall_share), , drop = FALSE]
    if (nrow(s) == 0) next
    out[[tech]] <- data.frame(
      iso3 = s$iso3, tech = tech, value = s$shortfall_share, basis = "national", source = s$target_source,
      year_from = s$installed_year, year_to = s$target_year,
      context = sprintf("%s %s %.1f GW by %d vs %.1f GW installed (%d), recent build %.2f GW/yr -> %.1f GW projected; shortfall %.1f GW",
                        s$target_key, ifelse(s$target_kind == "national_plan", "plan", "target"), s$target_gw,
                        s$target_year, s$installed_gw, s$installed_year, s$build_rate_gw, s$projected_gw, s$shortfall_gw),
      stringsAsFactors = FALSE)
  }
  if (length(out) == 0) return(dn_empty_measures())
  dn_finish_measure(do.call(rbind, out), cfg, measure)
}

# ---- Family C: system need ---------------------------------------------------------------------------

# Generation shares (0-1) per iso3 x Ember source at each country's latest year with total generation.
dn_generation_shares <- function(ember, sources) {
  total <- ember[ember$source == "Total generation" & !is.na(ember$generation_twh) & ember$generation_twh > 0,
                 c("iso3", "year", "generation_twh")]
  last <- stats::aggregate(year ~ iso3, data = total, FUN = max)
  total <- merge(last, total, by = c("iso3", "year"))
  out <- list()
  for (s in sources) {
    g <- dn_ember_sum(ember, s, "generation_twh")
    g <- merge(total, g, by = c("iso3", "year"))
    out[[s]] <- dn_df(iso3 = g$iso3, year = g$year, source = s, share = g$value / g$generation_twh,
                           stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

# Imported fuel a technology's deployment can displace (0-1). For tech specs with `displaces`:
#   sum over displaced sources of share of generation x import dependence of the source's fuel, times
#   (1 - import dependence of own_fuel) when the technology burns a fuel itself;
# with `fuel`: that fuel's import dependence (EVs displace oil; green hydrogen displaces gas).
# A source below min_fuel_share contributes nothing; at or above it an unknown import dependence makes the
# measure unknown (NA), as does an unknown own-fuel import dependence.
#
# With `risk` (iso3, fuel, risk in [0, 1]: dn_fuel_supplier_risk()) each displaced fuel's import dependence is
# multiplied by the risk of its suppliers, giving the imported fuel from risky suppliers that deployment can
# displace (measure fuel_supplier_risk); an imported fuel (at or above min_fuel_share) with no supplier risk
# measured makes the measure unknown. A fuel with no imports (import dependence 0) needs no supplier risk.
dn_measure_import_displacement <- function(ember, import_dep, spec, cfg, risk = NULL, measure = "import_displacement") {
  fuel_of <- unlist(cfg$fuel_of_source)
  srcs <- unique(unlist(lapply(spec, function(s) c(s$displaces))))
  shares <- if (length(srcs) > 0) dn_generation_shares(ember, srcs) else NULL
  id_of <- function(iso, fuel) {
    v <- import_dep$import_dependence[import_dep$iso3 == iso & import_dep$fuel == fuel]
    if (length(v) == 0) NA_real_ else v[1]
  }
  rk_of <- function(iso, fuel, idv) {
    if (is.null(risk)) return(1)
    if (!is.na(idv) && idv == 0) return(0)
    v <- risk$risk[risk$iso3 == iso & risk$fuel == fuel]
    if (length(v) == 0) NA_real_ else v[1]
  }
  id_year <- suppressWarnings(max(import_dep$year, na.rm = TRUE))
  src_w <- if (is.null(risk)) "" else " x supplier risk (coercive share of fuel imports, UN Comtrade)"
  out <- list()
  for (tech in names(spec)) {
    s <- spec[[tech]]
    if (!is.null(s$fuel)) {
      d <- import_dep[import_dep$fuel == s$fuel & !is.na(import_dep$import_dependence), , drop = FALSE]
      if (nrow(d) == 0) next
      rk <- vapply(seq_len(nrow(d)), function(j) rk_of(d$iso3[j], s$fuel, d$import_dependence[j]), numeric(1))
      ok <- !is.na(rk)
      if (!any(ok)) next
      d <- d[ok, , drop = FALSE]
      rk <- rk[ok]
      out[[tech]] <- dn_df(iso3 = d$iso3, tech = tech, value = d$import_dependence * rk, basis = "national",
                                source = paste0("EI Statistical Review / EIA: ", tolower(s$fuel), " import dependence (1 - self-sufficiency)", src_w),
                                year_from = d$year, year_to = d$year,
                                context = if (is.null(risk)) sprintf("%s import dependence %.2f", tolower(s$fuel), d$import_dependence)
                                          else sprintf("%s import dependence %.2f x supplier risk %.2f", tolower(s$fuel), d$import_dependence, rk),
                                stringsAsFactors = FALSE)
      next
    }
    disp <- unlist(s$displaces)
    sh <- shares[shares$source %in% disp, , drop = FALSE]
    isos <- unique(sh$iso3)
    rows <- lapply(isos, function(iso) {
      r <- sh[sh$iso3 == iso, , drop = FALSE]
      parts <- character()
      total <- 0
      for (j in seq_len(nrow(r))) {
        share <- r$share[j]
        if (is.na(share)) return(NULL)
        if (share < cfg$min_fuel_share) next
        idv <- id_of(iso, fuel_of[[r$source[j]]])
        if (is.na(idv)) return(NULL)
        rk <- rk_of(iso, fuel_of[[r$source[j]]], idv)
        if (is.na(rk)) return(NULL)
        total <- total + share * idv * rk
        parts <- c(parts, if (is.null(risk)) sprintf("%s %.0f%% x %.2f", tolower(r$source[j]), 100 * share, idv)
                          else sprintf("%s %.0f%% x %.2f x risk %.2f", tolower(r$source[j]), 100 * share, idv, rk))
      }
      if (!is.null(s$own_fuel)) {
        own <- id_of(iso, s$own_fuel)
        if (is.na(own)) return(NULL)
        total <- total * (1 - own)
        parts <- c(parts, sprintf("x (1 - own %s import dependence %.2f)", tolower(s$own_fuel), own))
      }
      dn_df(iso3 = iso, tech = tech, value = total, basis = "national",
                 source = paste0("Ember generation shares x EI Statistical Review / EIA import dependence", src_w),
                 year_from = min(r$year), year_to = max(r$year),
                 context = if (length(parts)) paste(parts, collapse = "; ") else "no imported-fuel generation",
                 stringsAsFactors = FALSE)
    })
    rows <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
    if (!is.null(rows) && nrow(rows) > 0) out[[tech]] <- rows
  }
  if (length(out) == 0) return(dn_empty_measures())
  res <- dn_finish_measure(do.call(rbind, out), cfg, measure)
  attr(res, "import_dependence_year") <- id_year
  res
}

# Supplier risk of each fuel's imports per importer (0-1): the coercive share of the fuel's imports at the
# fuel stage (import_exposure_tbl, UN Comtrade; Coal, Gas and Oil at Extraction: coal, natural gas incl.
# LNG, crude). Returns iso3, fuel, risk, import_hhi, top_supplier_iso3.
dn_fuel_supplier_risk <- function(import_exposure, fuels = c("Coal", "Gas", "Oil"), stage = "Extraction") {
  x <- as.data.frame(import_exposure, stringsAsFactors = FALSE)
  x <- x[x$tech %in% fuels & x$supply_chain %in% stage & x$observed %in% TRUE & !is.na(x$coercive_import_share), , drop = FALSE]
  if (nrow(x) == 0) return(dn_df(iso3 = character(), fuel = character(), risk = numeric(), import_hhi = numeric(),
                                 top_supplier_iso3 = character()))
  x <- x[!duplicated(x[, c("importer_iso", "tech")]), , drop = FALSE]
  dn_df(iso3 = toupper(x$importer_iso), fuel = x$tech, risk = pmin(pmax(x$coercive_import_share, 0), 1),
        import_hhi = x$import_hhi, top_supplier_iso3 = x$top_supplier_iso3, stringsAsFactors = FALSE)
}

# Net electricity imports as % of demand (0 for net exporters) at the latest year with both (Ember).
dn_measure_electricity_imports <- function(ember, techs, cfg) {
  ni <- ember[ember$source == "Net imports", c("iso3", "year", "generation_twh")]
  names(ni)[3] <- "net"
  dem <- ember[ember$source == "Demand", c("iso3", "year", "generation_twh")]
  names(dem)[3] <- "demand"
  d <- merge(ni, dem, by = c("iso3", "year"))
  w <- dn_window(d, 0, c("net", "demand"))
  w <- w[w$demand_to >= cfg$min_system_twh, , drop = FALSE]
  if (nrow(w) == 0) return(dn_empty_measures())
  rows <- dn_df(iso3 = w$iso3, value = 100 * pmax(w$net_to, 0) / w$demand_to, basis = "national",
                source = "Ember yearly electricity data: net imports and demand",
                year_from = w$year_to, year_to = w$year_to, stringsAsFactors = FALSE)
  rows$context <- sprintf("net electricity imports %.1f TWh, %.1f%% of demand (%d)", w$net_to, rows$value, w$year_to)
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "electricity_import_share")
}

# One World Development Indicators series from the WDI download (Country.Code, Series.Code, X2022..YR2022.,
# ...; ".." is missing) -> iso3, year, value at each economy's latest observed year. Aggregates (codes that
# `is_country` rejects) are dropped.
dn_wdi_latest <- function(wdi, code, is_country = function(iso3) rep(TRUE, length(iso3))) {
  w <- wdi[wdi$Series.Code %in% code, , drop = FALSE]
  ycols <- grep("^X[0-9]{4}", names(w), value = TRUE)
  if (nrow(w) == 0 || length(ycols) == 0) return(dn_df(iso3 = character(), year = integer(), value = numeric()))
  long <- do.call(rbind, lapply(ycols, function(yc) {
    dn_df(iso3 = toupper(trimws(w$Country.Code)), year = as.integer(substr(yc, 2, 5)),
          value = suppressWarnings(as.numeric(as.character(w[[yc]]))), stringsAsFactors = FALSE)
  }))
  long <- long[!is.na(long$value) & nchar(long$iso3) == 3 & is_country(long$iso3), , drop = FALSE]
  if (nrow(long) == 0) return(dn_df(iso3 = character(), year = integer(), value = numeric()))
  long <- long[order(long$iso3, -long$year), , drop = FALSE]
  long <- long[!duplicated(long$iso3), , drop = FALSE]
  rownames(long) <- NULL
  long
}

# Electricity access gap: % of the population without electricity (100 - WDI EG.ELC.ACCS.ZS), unmet demand.
# `access`: dn_wdi_latest() output.
dn_measure_access_gap <- function(access, techs, cfg) {
  if (is.null(access) || nrow(access) == 0) return(dn_empty_measures())
  rows <- dn_df(iso3 = access$iso3, value = pmax(100 - access$value, 0), basis = "national",
                source = "World Bank WDI: access to electricity (% of population), EG.ELC.ACCS.ZS",
                year_from = access$year, year_to = access$year, stringsAsFactors = FALSE)
  rows$context <- sprintf("%.1f%% of the population without electricity (%d)", rows$value, rows$year_to)
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "access_gap")
}

# ---- Climate case: climate-aligned projections (case-only; user decision 2026-11-17) ----------------------

# BloombergNEF New Energy Outlook long file (read with skip = 2: Report, Scenario, Region, Macro sector, Sector,
# Indicator, Fuel/type, Unit, Note, 2000 ... 2050) -> one row per series with numeric year columns y<year>.
dn_neo_prepare <- function(raw) {
  need <- c("Report", "Scenario", "Region", "Sector", "Indicator", "Fuel/type")
  miss <- setdiff(need, names(raw))
  if (length(miss)) stop("BNEF NEO file is missing columns: ", paste(miss, collapse = ", "))
  ycols <- grep("^[0-9]{4}$", names(raw), value = TRUE)
  out <- data.frame(scenario = as.character(raw$Scenario), region = as.character(raw$Region), sector = as.character(raw$Sector),
                    indicator = as.character(raw$Indicator), fuel = as.character(raw[["Fuel/type"]]), stringsAsFactors = FALSE)
  for (y in ycols) out[[paste0("y", y)]] <- suppressWarnings(as.numeric(raw[[y]]))
  out[grepl("^NEO", raw$Report) & nzchar(out$scenario), , drop = FALSE]
}

# One NEO series per region for a scenario: the sum over `fuels` of `indicator` in `sector`, at `year` or summed
# over `years` (a vector). Returns region, value.
dn_neo_value <- function(neo, scenario, sector, indicator, fuels, years) {
  d <- neo[neo$scenario == scenario & neo$sector == sector & neo$indicator == indicator & neo$fuel %in% fuels, , drop = FALSE]
  cols <- paste0("y", years)
  if (!nrow(d) || !all(cols %in% names(d))) return(dn_df(region = character(), value = numeric()))
  v <- rowSums(as.matrix(d[, cols, drop = FALSE]))
  a <- stats::aggregate(list(value = v), by = list(region = d$region), FUN = function(z) if (any(is.na(z))) NA_real_ else sum(z))
  a[!is.na(a$value), , drop = FALSE]
}

# How much more of a technology a climate-aligned pathway deploys than the economics-led baseline: BNEF NEO
# Net Zero Scenario over Economic Transition Scenario, minus 1, in % (installed capacity at the target year,
# or a quantity summed over a span of years for investment). National for the economies NEO models, else
# the NEO region (basis regional_proxy). `region_of`: iso3, region, basis. Spec per technology:
# {sector, indicator, fuels, year} or {..., years: [from, to]}. Values at or below the reference are kept
# (the pathway asks for less: no climate case).
dn_measure_climate_pathway <- function(neo, region_of, cfg) {
  m <- cfg$measures$climate_pathway
  if (is.null(neo) || !nrow(neo) || is.null(m)) return(dn_empty_measures())
  sc <- dn_or(m$scenarios, list(aligned = "NZS", reference = "ETS"))
  out <- list()
  for (tech in names(m$techs)) {
    s <- m$techs[[tech]]
    yrs <- if (!is.null(s$years)) seq(s$years[[1]], s$years[[2]]) else s$year
    a <- dn_neo_value(neo, sc$aligned, s$sector, s$indicator, unlist(s$fuels), yrs)
    r <- dn_neo_value(neo, sc$reference, s$sector, s$indicator, unlist(s$fuels), yrs)
    j <- merge(a, r, by = "region", suffixes = c("_a", "_r"))
    j <- j[!is.na(j$value_r) & j$value_r > 0, , drop = FALSE]
    if (!nrow(j)) next
    j$ratio <- 100 * (j$value_a / j$value_r - 1)
    k <- match(region_of$region, j$region)
    ok <- !is.na(k)
    if (!any(ok)) next
    yl <- if (length(yrs) > 1) paste0(min(yrs), "-", max(yrs)) else as.character(yrs)
    out[[tech]] <- dn_df(iso3 = region_of$iso3[ok], tech = tech, value = j$ratio[k[ok]], basis = region_of$basis[ok],
                         source = "BloombergNEF New Energy Outlook (subscription): Net Zero vs Economic Transition Scenario",
                         year_from = min(yrs), year_to = max(yrs),
                         context = sprintf("NEO %s: net zero %+.0f%% vs economic transition (%s, %s)", region_of$region[ok],
                                           j$ratio[k[ok]], yl, tolower(s$indicator)),
                         stringsAsFactors = FALSE)
  }
  if (!length(out)) return(dn_empty_measures())
  dn_finish_measure(do.call(rbind, out), cfg, "climate_pathway")
}

# Expected growth under BloombergNEF's Economic Transition Scenario (NEO): the compound annual growth, % a year,
# of the technology's series between base_year and target_year (spec per technology as climate_pathway's).
# National for the modelled economies; regional proxies only where the measure's regional_proxy is true.
# Not in the committed config (D13): a sensitivity of deployment_need (config/deployment_need_bnef_*.yml).
dn_measure_neo_growth <- function(neo, region_of, cfg, measure = "neo_growth") {
  m <- cfg$measures[[measure]]
  if (is.null(neo) || !nrow(neo) || is.null(m)) return(dn_empty_measures())
  y0 <- as.integer(dn_or(m$base_year, 2024))
  y1 <- as.integer(dn_or(m$target_year, 2035))
  sc <- dn_or(m$scenario_code, "ETS")
  ro <- if (isTRUE(m$regional_proxy)) region_of else region_of[region_of$basis == "national", , drop = FALSE]
  out <- list()
  for (tech in names(m$techs)) {
    s <- m$techs[[tech]]
    a <- dn_neo_value(neo, sc, s$sector, s$indicator, unlist(s$fuels), y0)
    b <- dn_neo_value(neo, sc, s$sector, s$indicator, unlist(s$fuels), y1)
    j <- merge(a, b, by = "region", suffixes = c("_0", "_1"))
    j$g <- dn_cagr_pct(j$value_1, j$value_0, y1 - y0)
    j <- j[!is.na(j$g), , drop = FALSE]
    k <- match(ro$region, j$region)
    ok <- !is.na(k)
    if (!any(ok)) next
    out[[tech]] <- dn_df(iso3 = ro$iso3[ok], tech = tech, value = j$g[k[ok]], basis = ro$basis[ok],
                         source = "BloombergNEF New Energy Outlook (subscription): Economic Transition Scenario",
                         year_from = y0, year_to = y1,
                         context = sprintf("NEO %s ETS %+.1f%% a year %d-%d", ro$region[ok], j$g[k[ok]], y0, y1),
                         stringsAsFactors = FALSE)
  }
  if (!length(out)) return(dn_empty_measures())
  dn_finish_measure(do.call(rbind, out), cfg, measure)
}

# Distance to the climate-aligned milestone for cars (all new cars electric by 2035 in net-zero pathways):
# 100 - the latest observed EV share of new-car sales (IEA GEVO "Historical").
dn_measure_ev_sales_gap <- function(gevo_share, techs, cfg) {
  if (is.null(gevo_share) || !nrow(gevo_share)) return(dn_empty_measures())
  h <- gevo_share[gevo_share$category == "Historical" & !is.na(gevo_share$value), , drop = FALSE]
  if (!nrow(h)) return(dn_empty_measures())
  h <- h[order(h$iso3, -h$year), , drop = FALSE]
  h <- h[!duplicated(h$iso3), , drop = FALSE]
  rows <- dn_df(iso3 = h$iso3, value = pmax(100 - h$value, 0), basis = "national",
                source = "IEA Global EV Outlook: EV share of new-car sales (historical)", year_from = h$year, year_to = h$year,
                stringsAsFactors = FALSE)
  rows$context <- sprintf("EV share of new-car sales %.1f%% (%d): %.1f points from all-electric sales", h$value, h$year, rows$value)
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "ev_sales_gap")
}

# Distance to a decarbonised power system: the fossil share of generation (%) at the latest year (Ember).
dn_measure_fossil_power_share <- function(ember, techs, cfg) {
  sh <- dn_generation_shares(ember, "Fossil")
  if (is.null(sh) || !nrow(sh)) return(dn_empty_measures())
  sh <- sh[!is.na(sh$share), , drop = FALSE]
  rows <- dn_df(iso3 = sh$iso3, value = 100 * sh$share, basis = "national",
                source = "Ember yearly electricity data: fossil share of generation", year_from = sh$year, year_to = sh$year,
                stringsAsFactors = FALSE)
  rows$context <- sprintf("fossil fuels %.0f%% of generation (%d)", rows$value, rows$year_to)
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "fossil_power_share")
}

# The NEO region of each economy: the economy itself where NEO models it (config climate_pathway$national,
# iso3 -> NEO name), else the regional rule on the World Bank region (and UN sub-region for South-eastern Asia).
# `regions`: iso3, wb_region, un_subregion (from countrycode, in the script). Returns iso3, region, basis.
dn_neo_region_of <- function(regions, cfg) {
  m <- cfg$measures$climate_pathway
  nat <- unlist(m$national)
  rr <- m$regional
  reg <- vapply(seq_len(nrow(regions)), function(i) {
    iso <- regions$iso3[i]
    if (iso %in% names(rr$overrides)) return(rr$overrides[[iso]])
    if (!is.na(regions$un_subregion[i]) && regions$un_subregion[i] %in% names(rr$by_un_subregion)) return(rr$by_un_subregion[[regions$un_subregion[i]]])
    w <- regions$wb_region[i]
    if (!is.na(w) && w %in% names(rr$by_wb_region)) return(rr$by_wb_region[[w]])
    dn_or(rr$default, NA_character_)
  }, character(1))
  is_nat <- regions$iso3 %in% names(nat)
  reg[is_nat] <- unname(nat[regions$iso3[is_nat]])
  out <- dn_df(iso3 = regions$iso3, region = reg, basis = ifelse(is_nat, "national", "regional_proxy"), stringsAsFactors = FALSE)
  out[!is.na(out$region), , drop = FALSE]
}

# Variable-renewable (solar + wind) share of generation, %, at the latest year.
dn_measure_flexibility <- function(ember, techs, cfg) {
  vre <- dn_ember_sum(ember, c("Solar", "Wind"), "generation_twh")
  names(vre)[names(vre) == "value"] <- "vre"
  total <- ember[ember$source == "Total generation", c("iso3", "year", "generation_twh")]
  names(total)[3] <- "total"
  d <- merge(vre, total, by = c("iso3", "year"))
  d <- d[!is.na(d$total) & d$total > 0, , drop = FALSE]
  w <- dn_window(d, 0, c("vre", "total"))
  agg <- data.frame(iso3 = w$iso3, year = w$year_to, share = w$vre_to / w$total_to)
  if (nrow(agg) == 0) return(dn_empty_measures())
  rows <- dn_df(iso3 = agg$iso3, value = 100 * agg$share, basis = "national",
                     source = "Ember yearly electricity data: solar + wind share of generation",
                     year_from = agg$year, year_to = agg$year, stringsAsFactors = FALSE)
  rows$context <- sprintf("solar + wind %.1f%% of generation (%d)", rows$value, rows$year_to)
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "flexibility_need")
}

# Dispatchable capacity (GW) per GW of average load (demand TWh / 8.76) at the latest year with both.
# Lower = tighter firm-capacity margin = more need (the measure's direction is -1).
dn_measure_firm_capacity <- function(ember, techs, cfg) {
  srcs <- unlist(cfg$measures$firm_capacity_need$dispatchable_sources)
  cap <- dn_ember_sum(ember, srcs, "capacity_gw")
  names(cap)[names(cap) == "value"] <- "cap"
  dem <- ember[ember$source == "Demand", c("iso3", "year", "generation_twh")]
  names(dem)[3] <- "demand"
  d <- merge(cap, dem, by = c("iso3", "year"))
  w <- dn_window(d, 0, c("cap", "demand"))
  w <- w[w$demand_to >= cfg$min_system_twh, , drop = FALSE]
  avg <- w$demand_to / cfg$twh_to_avg_gw
  rows <- dn_df(iso3 = w$iso3, value = w$cap_to / avg, basis = "national",
                     source = "Ember yearly electricity data: dispatchable capacity and demand",
                     year_from = w$year_to, year_to = w$year_to, stringsAsFactors = FALSE)
  rows$context <- sprintf("%.1f GW dispatchable for %.1f GW average load (%d)", w$cap_to, avg, w$year_to)
  rows <- rows[is.finite(rows$value), , drop = FALSE]
  dn_finish_measure(dn_expand_techs(rows, techs), cfg, "firm_capacity_need")
}

# ---- Context columns (never scored) ------------------------------------------------------------------

# Climate-aligned alternative per technology (World, country-invariant): aligned / reference - 1 at the
# configured year. `pathways`: iea_pathways_values_tbl (series, scenario, year, value).
dn_climate_aligned <- function(pathways, cfg) {
  ca <- cfg$climate_aligned
  out <- lapply(names(ca$series), function(tech) {
    s <- ca$series[[tech]]
    a <- pathways$value[pathways$series == s & pathways$scenario == ca$aligned & pathways$year == ca$year]
    r <- pathways$value[pathways$series == s & pathways$scenario == ca$reference & pathways$year == ca$year]
    v <- if (length(a) == 1 && length(r) == 1 && !is.na(r) && r > 0) a / r - 1 else NA_real_
    note <- if (is.na(v)) NA_character_ else
      sprintf("World %d: Net Zero Emissions %s Current Policies by %.0f%% (%s); a climate-aligned alternative, not part of Deployment Need",
              as.integer(ca$year), if (v >= 0) "above" else "below", abs(100 * v), s)
    data.frame(tech = tech, dn_climate_aligned = v, dn_climate_aligned_series = s,
               dn_climate_aligned_scenario = ca$scenario,
               dn_climate_aligned_basis = if (is.na(v)) "not_assessed" else "global_context",
               dn_climate_aligned_note = note,
               dn_climate_aligned_source = sprintf("IEA WEO 2025 World: %s, Net Zero Emissions vs Current Policies, %d (context only; never in the score)", s, as.integer(ca$year)),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Current deployment trend per country x technology (spec XIV, Deploy - Accelerate: "current deployment
# trend"), from the home-market observed-trend rows (hm_trend_capacity / hm_trend_ev / hm_trend_oil):
# deployment_trend (text), deployment_trend_additions (a year, in deployment_trend_unit),
# deployment_trend_growth (% a year) and deployment_trend_years. Context only; never in the score.
dn_deployment_trend <- function(hm_measures) {
  t <- hm_measures[hm_measures$kind %in% "observed_trend", , drop = FALSE]
  if (nrow(t) == 0) {
    return(dn_df(iso3 = character(), tech = character(), deployment_trend = character(),
                 deployment_trend_additions = numeric(), deployment_trend_unit = character(),
                 deployment_trend_growth = numeric(), deployment_trend_years = character()))
  }
  dn_df(iso3 = t$iso3, tech = t$tech, deployment_trend = t$context, deployment_trend_additions = t$additions,
        deployment_trend_unit = t$unit, deployment_trend_growth = t$growth_rate,
        deployment_trend_years = paste0(t$year_from, "-", t$year_to))
}

# Regional-proxy electricity growth (WEO region Current Policies 2024 -> 2035) for economies that are not
# single-country WEO regions: shown as context, never scored.
dn_regional_proxy <- function(weo_regional, cfg) {
  base_y <- as.integer(cfg$scenario_base_year)
  scen_y <- as.integer(cfg$scenario_year)
  s <- cfg$weo_series$total
  a <- weo_regional[weo_regional$series == s & weo_regional$scenario == "historical" & weo_regional$year == base_y, ]
  b <- weo_regional[weo_regional$series == s & weo_regional$scenario == cfg$weo_scenarios$current_policy & weo_regional$year == scen_y, ]
  m <- merge(a[, c("iso3", "region", "value")], b[, c("iso3", "value")], by = "iso3", suffixes = c("_from", "_to"))
  dn_df(iso3 = m$iso3, dn_regional_proxy_growth = dn_cagr_pct(m$value_to, m$value_from, scen_y - base_y),
             dn_regional_proxy_region = m$region, dn_regional_proxy_basis = "regional_proxy",
             dn_regional_proxy_years = sprintf("%d-%d", base_y, scen_y), stringsAsFactors = FALSE)
}

# ---- Assembly ------------------------------------------------------------------------------------------

# The weakest (most conservative) basis among observed evidence; not_assessed when there is none.
dn_weakest_basis <- function(x) {
  x <- x[!is.na(x) & x != "not_assessed"]
  if (length(x) == 0) return("not_assessed")
  i <- match(x, DN_BASIS_LEVELS)
  if (anyNA(i)) stop("deployment_need: unknown basis '", paste(unique(x[is.na(i)]), collapse = "', '"), "'.")
  DN_BASIS_LEVELS[max(i)]
}

dn_confidence <- function(n, ccfg) {
  out <- rep("none", length(n))
  out[n >= ccfg$low] <- "low"
  out[n >= ccfg$moderate] <- "moderate"
  out[n >= ccfg$high] <- "high"
  out
}

# Measure percentiles: each measure is oriented (direction) and ranked within the technology across the
# universe countries that have it. Returns the measure rows with `pct` and `n_countries`.
dn_score_measures <- function(measures, universe_iso3, cfg) {
  m <- measures[measures$iso3 %in% universe_iso3 & measures$tech %in% unlist(cfg$techs) & !is.na(measures$value), , drop = FALSE]
  if (nrow(m) == 0) {
    m$pct <- numeric()
    m$n_countries <- integer()
    return(m)
  }
  dir <- vapply(m$measure, function(x) dn_measure_meta(cfg, x)$direction, numeric(1))
  m$oriented <- m$value * dir
  m <- dplyr::mutate(dplyr::group_by(m, .data$tech, .data$measure),
                     pct = sp_midrank_pct(.data$oriented), n_countries = dplyr::n())
  m <- as.data.frame(dplyr::ungroup(m))
  m$oriented <- NULL
  m
}

# Why a family has no value for a technology: no measure is attributed to it at all (by design), or the
# measures attributed to it are not observed for this country.
dn_family_gap_reason <- function(cfg, fam, tech) {
  attributed <- vapply(names(cfg$measures), function(m) {
    mm <- cfg$measures[[m]]
    mm$family == fam && tech %in% dn_measure_techs(mm)
  }, logical(1))
  if (any(attributed)) "no data for this country" else "no measure attributed to this technology"
}

# deployment_need per country x technology. `measures`: the standard measure table (DN_MEASURE_COLS);
# `universe`: iso3 + Country. Every universe country x technology gets a row; unassessed families stay NA
# with basis not_assessed. Per family: dn_<family> (the family percentile within the technology, which
# enters deployment_need), dn_<family>_score (the mean of its measure percentiles), _basis, _scenario,
# _measures, _source, _note (the measures' raw values in words).
dn_assemble <- function(measures, universe, cfg) {
  techs <- unlist(cfg$techs)
  scored <- dn_score_measures(measures, universe$iso3, cfg)
  grid <- dn_df(iso3 = rep(universe$iso3, times = length(techs)),
                     Country = rep(universe$Country, times = length(techs)),
                     tech = rep(techs, each = nrow(universe)), stringsAsFactors = FALSE)
  key <- paste(grid$iso3, grid$tech)
  for (fam in DN_FAMILIES) {
    f <- scored[scored$family == fam, , drop = FALSE]
    score <- pct <- rep(NA_real_, nrow(grid))
    basis <- rep("not_assessed", nrow(grid))
    meas <- src <- note <- rep(NA_character_, nrow(grid))
    if (nrow(f) > 0) {
      f$note <- ifelse(is.na(f$context), f$measure, paste0(f$measure, ": ", f$context))
      agg <- dplyr::summarise(dplyr::group_by(f, .data$iso3, .data$tech),
                              score = mean(.data$pct), basis = dn_weakest_basis(.data$basis),
                              measures = paste(sort(unique(.data$measure)), collapse = "; "),
                              source = paste(unique(.data$source), collapse = " | "),
                              note = paste(.data$note[order(.data$measure)], collapse = " | "), .groups = "drop")
      agg <- dplyr::mutate(dplyr::group_by(agg, .data$tech), pct = sp_midrank_pct(.data$score))
      agg <- as.data.frame(dplyr::ungroup(agg))
      i <- match(key, paste(agg$iso3, agg$tech))
      score <- agg$score[i]
      pct <- agg$pct[i]
      basis <- ifelse(is.na(i), "not_assessed", agg$basis[i])
      meas <- agg$measures[i]
      src <- agg$source[i]
      note <- agg$note[i]
    }
    gap <- vapply(techs, function(t) dn_family_gap_reason(cfg, fam, t), character(1))
    note[is.na(pct)] <- paste0("not assessed: ", unname(gap[grid$tech[is.na(pct)]]))
    grid[[fam]] <- pct
    grid[[paste0(fam, "_score")]] <- score
    grid[[paste0(fam, "_basis")]] <- basis
    grid[[paste0(fam, "_scenario")]] <- cfg$families[[fam]]$scenario
    grid[[paste0(fam, "_measures")]] <- meas
    grid[[paste0(fam, "_source")]] <- src
    grid[[paste0(fam, "_note")]] <- note
  }
  fam_pct <- as.matrix(grid[, DN_FAMILIES])
  grid$dn_families_observed <- as.integer(rowSums(!is.na(fam_pct)))
  grid$deployment_need <- sp_row_mean_available(fam_pct)
  grid$deployment_need_pct <- NA_real_
  for (t in techs) {
    i <- grid$tech == t
    grid$deployment_need_pct[i] <- sp_midrank_pct(grid$deployment_need[i])
  }
  fam_basis <- as.matrix(grid[, paste0(DN_FAMILIES, "_basis")])
  grid$dn_basis <- apply(fam_basis, 1, dn_weakest_basis)
  grid$dn_confidence <- dn_confidence(grid$dn_families_observed, cfg$confidence)
  grid$dn_scenarios <- apply(fam_pct, 1, function(x) {
    s <- unique(vapply(DN_FAMILIES[!is.na(x)], function(f) cfg$families[[f]]$scenario, character(1)))
    if (length(s) == 0) NA_character_ else paste(s, collapse = "; ")
  })
  grid$supply_chain <- dn_or(cfg$stage, "Deployment")
  grid$need_type <- "deployment"
  lead <- c("Country", "iso3", "tech", "supply_chain", "need_type", "deployment_need", "deployment_need_pct",
            "dn_families_observed", "dn_confidence", "dn_basis", "dn_scenarios")
  grid[, c(lead, setdiff(names(grid), lead))]
}

# ---- Deployment case (a descriptive tag; never changes deployment_need or the choice) -------------------
#
# Why a country needs more of a technology, as evidence: energy security, climate (national targets and a
# climate-aligned pathway), demand growth or grid integration (a high wind and solar share that storage, grids
# and flexible gas must firm). Each case is built like a family from the measures config `cases` attributes
# to it (a measure may serve different cases for different technologies: approved national-plan capacities
# for Coal, Gas and Nuclear are demand, renewable and storage targets are climate). Measures that share a
# `group` are averaged first, so several measures of one thing (fuel imports) count once. Per case:
#   dn_case_<c>          mid-rank within the technology of the case score (mean of group means of the
#                        measure percentiles); NA where not assessed
#   dn_case_<c>_score    the case score
#   dn_case_<c>_material the absolute gate: at least one of its measures shows the need is real (the
#                        displaced fuel is imported, a target has a shortfall, demand grows), not just
#                        relatively high
#   dn_case_<c>_status   assessed, not_assessed (measures attributed, no data for this country), not_measured
#                        (no measure of the case is attributed to the technology yet) or not_applicable (the
#                        case cannot hold for the technology: no climate case for fossil technologies)
# A case is strong at or above min_pct and material. dn_case is the strongest strong case, or the two
# strongest joined with "+" (higher first) where the second is within dual_margin; "none" where cases were
# assessed and none is strong; NA where no case is assessed. Exact ties follow DN_CASES order. A technology's
# anchor case (config cases$anchor) is named first wherever it is material, whatever its rank, with the
# strongest other strong case after it.
# dn_case_unassessed lists the cases with no data (missing evidence is uncertainty, never a weak case);
# dn_case_basis is multi_case (two or more cases assessed) or single_case.
dn_cases <- function(scored, dn_tbl, cfg) {
  cc <- cfg$cases
  if (is.null(cc)) return(dn_tbl)
  min_pct <- as.numeric(dn_or(cc$min_pct, 0.5))
  margin <- as.numeric(dn_or(cc$dual_margin, 0.1))
  key <- paste(dn_tbl$iso3, dn_tbl$tech)
  passes <- function(v, g) {
    if (is.null(g)) return(rep(TRUE, length(v)))
    ok <- !is.na(v)
    if (!is.null(g$gt)) ok <- ok & v > g$gt
    if (!is.null(g$ge)) ok <- ok & v >= g$ge
    if (!is.null(g$le)) ok <- ok & v <= g$le
    ok
  }
  pct_m <- matrix(NA_real_, nrow(dn_tbl), length(DN_CASES), dimnames = list(NULL, DN_CASES))
  mat_m <- matrix(FALSE, nrow(dn_tbl), length(DN_CASES), dimnames = list(NULL, DN_CASES))
  na_m <- matrix(FALSE, nrow(dn_tbl), length(DN_CASES), dimnames = list(NULL, DN_CASES))
  meas_m <- matrix(NA_character_, nrow(dn_tbl), length(DN_CASES), dimnames = list(NULL, DN_CASES))
  nm_m <- matrix(FALSE, nrow(dn_tbl), length(DN_CASES), dimnames = list(NULL, DN_CASES))
  for (cs in DN_CASES) {
    def <- cc$cases[[cs]]
    na_m[, cs] <- dn_tbl$tech %in% unlist(def$not_applicable_techs)
    rows <- list()
    for (m in names(def$measures)) {
      md <- def$measures[[m]]
      s <- scored[scored$measure == m & !is.na(scored$pct), c("iso3", "tech", "measure", "value", "pct"), drop = FALSE]
      if (!is.null(md$techs)) s <- s[s$tech %in% unlist(md$techs), , drop = FALSE]
      # include: a measure that is zero for most countries (electricity imports, the access gap) enters a case
      # only where present; it is still ranked against every country, zeros included
      if (!is.null(md$include)) s <- s[passes(s$value, md$include), , drop = FALSE]
      if (nrow(s) == 0) next
      s$group <- dn_or(md$group, m)
      s$material <- passes(s$value, md$material)
      rows[[m]] <- s
    }
    if (!length(rows)) next
    s <- do.call(rbind, rows)
    s <- s[!s$tech %in% unlist(def$not_applicable_techs), , drop = FALSE]
    if (nrow(s) == 0) next
    g <- dplyr::summarise(dplyr::group_by(s, .data$iso3, .data$tech, .data$group), gp = mean(.data$pct),
                          mat = any(.data$material), ms = paste(sort(unique(.data$measure)), collapse = ", "), .groups = "drop")
    a <- dplyr::summarise(dplyr::group_by(g, .data$iso3, .data$tech), score = mean(.data$gp), mat = any(.data$mat),
                          ms = paste(.data$ms, collapse = ", "), .groups = "drop")
    a <- dplyr::mutate(dplyr::group_by(a, .data$tech), pct = sp_midrank_pct(.data$score))
    a <- as.data.frame(dplyr::ungroup(a))
    i <- match(key, paste(a$iso3, a$tech))
    pct_m[, cs] <- a$pct[i]
    mat_m[, cs] <- dplyr::coalesce(a$mat[i], FALSE)
    meas_m[, cs] <- a$ms[i]
    dn_tbl[[paste0("dn_case_", cs)]] <- a$pct[i]
    dn_tbl[[paste0("dn_case_", cs, "_score")]] <- a$score[i]
  }
  for (cs in DN_CASES) {
    if (is.null(dn_tbl[[paste0("dn_case_", cs)]])) {
      dn_tbl[[paste0("dn_case_", cs)]] <- NA_real_
      dn_tbl[[paste0("dn_case_", cs, "_score")]] <- NA_real_
    }
    dn_tbl[[paste0("dn_case_", cs, "_material")]] <- ifelse(is.na(pct_m[, cs]), NA, mat_m[, cs])
    # not_measured: no measure of the case is attributed to the technology (by design, not a data gap)
    att <- vapply(dn_tbl$tech, function(t) any(vapply(names(cc$cases[[cs]]$measures), function(m) {
      md <- cc$cases[[cs]]$measures[[m]]
      t %in% dn_measure_techs(cfg$measures[[m]]) && (is.null(md$techs) || t %in% unlist(md$techs))
    }, logical(1))), logical(1))
    nm_m[, cs] <- !na_m[, cs] & !att
    dn_tbl[[paste0("dn_case_", cs, "_status")]] <- ifelse(na_m[, cs], "not_applicable", ifelse(!is.na(pct_m[, cs]), "assessed",
                                                         ifelse(nm_m[, cs], "not_measured", "not_assessed")))
  }
  strong <- !is.na(pct_m) & pct_m >= min_pct & mat_m
  n_ass <- rowSums(!is.na(pct_m))
  anchor <- unlist(cc$anchor)
  dn_tbl$dn_case <- vapply(seq_len(nrow(dn_tbl)), function(r) {
    if (n_ass[r] == 0) return(NA_character_)
    a <- if (dn_tbl$tech[r] %in% names(anchor)) anchor[[dn_tbl$tech[r]]] else NA_character_
    # an anchor case that is material is named first, with the strongest other strong case after it
    if (!is.na(a) && a %in% DN_CASES && !is.na(pct_m[r, a]) && mat_m[r, a]) {
      o <- pct_m[r, strong[r, ] & DN_CASES != a]
      if (!length(o)) return(a)
      return(paste(a, names(o)[order(-o, match(names(o), DN_CASES))][1], sep = "+"))
    }
    v <- pct_m[r, strong[r, ]]
    if (!length(v)) return("none")
    o <- order(-v, match(names(v), DN_CASES))
    v <- v[o]
    if (length(v) >= 2 && v[1] - v[2] <= margin + 1e-9) paste(names(v)[1:2], collapse = "+") else names(v)[1]
  }, character(1))
  dn_tbl$dn_case_basis <- ifelse(n_ass == 0, NA_character_, ifelse(n_ass >= 2, "multi_case", "single_case"))
  dn_tbl$dn_case_unassessed <- vapply(seq_len(nrow(dn_tbl)), function(r) {
    u <- DN_CASES[is.na(pct_m[r, ]) & !na_m[r, ] & !nm_m[r, ]]
    if (length(u)) paste(u, collapse = ";") else NA_character_
  }, character(1))
  p2 <- function(v) format(round(v, 2), nsmall = 2)
  dn_tbl$dn_case_note <- vapply(seq_len(nrow(dn_tbl)), function(r) {
    parts <- vapply(DN_CASES, function(cs) {
      if (na_m[r, cs]) return(paste0(cs, " n/a"))
      if (nm_m[r, cs]) return(paste0(cs, " not measured for this technology"))
      if (is.na(pct_m[r, cs])) return(paste0(cs, " not assessed"))
      paste0(cs, " ", p2(pct_m[r, cs]), if (!mat_m[r, cs]) " (not material)" else "", " [", meas_m[r, cs], "]")
    }, character(1))
    if (n_ass[r] == 0) NA_character_ else paste(parts, collapse = "; ")
  }, character(1))
  dn_tbl
}

# What kind of Watch a cell would be (config watch_kind): phase_down, momentum, declining or low_need, from the
# climate pathway (scored measures, climate_pathway), the observed deployment trend (deployment_trend_growth,
# joined by scripts/95 before this runs) and the national target (dn_target_met). Adds dn_watch_kind,
# dn_phase_down (a fossil technology the pathway asks for less of), dn_momentum and dn_declining (logicals; NA
# where unobserved) and dn_trend_pct (the observed growth's percentile within the technology, context only). Computed for every assessed cell; the
# engine shows it on Watch cells only. dn_phase_down is also read beside Accelerate for the fossil technologies.
dn_watch_kind <- function(scored, dn_tbl, cfg) {
  wk <- cfg$watch_kind
  if (is.null(wk)) return(dn_tbl)
  key <- paste(dn_tbl$iso3, dn_tbl$tech)
  pw <- scored[scored$measure == "climate_pathway" & !is.na(scored$value), , drop = FALSE]
  pv <- pw$value[match(key, paste(pw$iso3, pw$tech))]
  fossil <- dn_tbl$tech %in% unlist(wk$fossil_techs)
  # fossil technologies only (config watch_kind: the hydrogen series is not a call to phase green hydrogen down)
  phase <- ifelse(!fossil, FALSE, ifelse(is.na(pv), NA, pv <= as.numeric(dn_or(wk$phase_down_max, -20))))
  g <- if ("deployment_trend_growth" %in% names(dn_tbl)) as.numeric(dn_tbl$deployment_trend_growth) else rep(NA_real_, nrow(dn_tbl))
  gp <- rep(NA_real_, nrow(dn_tbl))
  for (t in unique(dn_tbl$tech)) {
    i <- dn_tbl$tech == t & !is.na(g)
    if (any(i)) gp[i] <- sp_midrank_pct(g[i])
  }
  met <- if ("dn_target_met" %in% names(dn_tbl)) dn_tbl$dn_target_met %in% TRUE else rep(FALSE, nrow(dn_tbl))
  fast <- !is.na(g) & g >= as.numeric(dn_or(wk$momentum_min_growth, 15))
  mom <- ifelse(is.na(g) & !met, NA, fast | (met & !fossil))
  dec <- ifelse(is.na(g), NA, g < as.numeric(dn_or(wk$decline_max, -2)))
  assessed <- !is.na(dn_tbl$deployment_need)
  kind <- ifelse(phase %in% TRUE, "phase_down", ifelse(mom %in% TRUE, "momentum", ifelse(dec %in% TRUE, "declining", "low_need")))
  dn_tbl$dn_watch_kind <- ifelse(assessed, kind, NA_character_)
  dn_tbl$dn_phase_down <- phase
  dn_tbl$dn_momentum <- mom
  dn_tbl$dn_declining <- dec
  dn_tbl$dn_trend_pct <- gp
  dn_tbl
}

# Measure-level columns, wide: dnm_<measure> (raw value), _pct, _basis, _years, _source, _context.
dn_measures_wide <- function(scored, cfg) {
  base <- unique(scored[, c("iso3", "tech")])
  for (m in names(cfg$measures)) {
    s <- scored[scored$measure == m, , drop = FALSE]
    i <- match(paste(base$iso3, base$tech), paste(s$iso3, s$tech))
    p <- paste0("dnm_", m)
    base[[p]] <- s$value[i]
    base[[paste0(p, "_pct")]] <- s$pct[i]
    base[[paste0(p, "_basis")]] <- s$basis[i]
    base[[paste0(p, "_years")]] <- ifelse(is.na(i), NA_character_,
                                          ifelse(s$year_from[i] == s$year_to[i], as.character(s$year_to[i]),
                                                 paste0(s$year_from[i], "-", s$year_to[i])))
    base[[paste0(p, "_source")]] <- s$source[i]
    base[[paste0(p, "_context")]] <- s$context[i]
  }
  base
}

# Target detail per country x technology (the technology's own target or plan, and the total-renewables
# target where attributed), from dn_target_shortfall().
dn_target_detail <- function(shortfall, cfg) {
  own <- cfg$measures$target_shortfall$techs
  out <- list()
  for (tech in names(own)) {
    s <- shortfall[shortfall$target_key == own[[tech]], , drop = FALSE]
    if (nrow(s) == 0) next
    out[[tech]] <- dn_df(iso3 = s$iso3, tech = tech, dn_target_key = s$target_key, dn_target_kind = s$target_kind,
                              dn_target_gw = s$target_gw, dn_target_year = s$target_year,
                              dn_target_source = s$target_source, dn_target_note = s$target_note,
                              dn_installed_gw = s$installed_gw, dn_installed_year = s$installed_year,
                              dn_build_rate_gw = s$build_rate_gw, dn_projected_gw = s$projected_gw,
                              dn_shortfall_gw = s$shortfall_gw, dn_required_rate_gw = s$required_rate_gw,
                              dn_acceleration = s$acceleration, dn_target_met = s$target_met, stringsAsFactors = FALSE)
  }
  own_tbl <- if (length(out)) do.call(rbind, out) else NULL
  re <- shortfall[shortfall$target_key == "Renewables", , drop = FALSE]
  re_techs <- unlist(cfg$measures$renewables_target_shortfall$techs)
  re_tbl <- NULL
  if (nrow(re) > 0 && length(re_techs) > 0) {
    re_tbl <- do.call(rbind, lapply(re_techs, function(tech) {
      dn_df(iso3 = re$iso3, tech = tech, dn_re_target_gw = re$target_gw, dn_re_installed_gw = re$installed_gw,
                 dn_re_projected_gw = re$projected_gw, dn_re_shortfall_gw = re$shortfall_gw, stringsAsFactors = FALSE)
    }))
  }
  list(own = own_tbl, renewables = re_tbl)
}

# ---- Orchestration (pure) -----------------------------------------------------------------------------

# Every measure from prepared inputs. `inp`: list of
#   ember        dn_ember_prepare() output
#   weo_national dn_weo_national() output (may have zero rows)
#   import_dep   dn_import_dependence() output
#   gevo_share   dn_gevo_prepare(.., "EV sales share") output (iso3, category, year, value in percent)
#   oil          iso3, year, value (EJ): dn_ei_series(.., "oil_tes_ej")
#   targets      dn_targets_prepare() output (may have zero rows)
#   bess         dn_ei_bess_prepare() output (may be NULL)
#   fuel_risk    dn_fuel_supplier_risk() output (may be NULL; case tag only)
#   access       dn_wdi_latest(.., "EG.ELC.ACCS.ZS") output (may be NULL; case tag only)
#   neo          dn_neo_prepare() output and neo_region dn_neo_region_of() output (may be NULL; case tag only)
# Returns list(measures, shortfall, installed, cap_series).
dn_build_measures <- function(inp, cfg) {
  ms <- cfg$measures
  cap_keys <- unique(c(unlist(cfg$targets$installed_source), unlist(cfg$home_market$capacity_source)))
  cap_series <- dn_capacity_series(inp$ember, setdiff(cap_keys, "Battery"), inp$bess)
  installed <- dn_installed_capacity(cap_series, as.integer(cfg$build_rate_years))
  targets <- inp$targets
  shortfall <- if (!is.null(targets) && nrow(targets) > 0) {
    dn_target_shortfall(targets, installed, dn_fleet(inp$ember), cfg$targets$installed_source)
  } else {
    NULL
  }
  parts <- list(
    dn_measure_demand_growth(inp$ember, inp$weo_national, dn_measure_techs(ms$demand_growth), cfg),
    dn_measure_output_growth(inp$ember, inp$weo_national, ms$output_growth$techs, cfg),
    if (!is.null(inp$gevo_share) && nrow(inp$gevo_share) > 0)
      dn_measure_ev_share_growth(inp$gevo_share, cfg, dn_measure_techs(ms$ev_share_growth)) else NULL,
    if (!is.null(inp$oil) && nrow(inp$oil) > 0)
      dn_measure_oil_demand_growth(inp$oil, cfg, dn_measure_techs(ms$oil_demand_growth)) else NULL,
    if (!is.null(shortfall)) dn_measure_target(shortfall, ms$target_shortfall$techs, cfg, "target_shortfall") else NULL,
    if (!is.null(shortfall)) dn_measure_target(shortfall, dn_measure_techs(ms$renewables_target_shortfall), cfg,
                                               "renewables_target_shortfall", default_key = "Renewables") else NULL,
    dn_measure_import_displacement(inp$ember, inp$import_dep, ms$import_displacement$techs, cfg),
    dn_measure_flexibility(inp$ember, dn_measure_techs(ms$flexibility_need), cfg),
    dn_measure_firm_capacity(inp$ember, dn_measure_techs(ms$firm_capacity_need), cfg),
    # case-only measures (the deployment case tag; never in deployment_need)
    if (!is.null(ms$fuel_supplier_risk) && !is.null(inp$fuel_risk) && nrow(inp$fuel_risk) > 0)
      dn_measure_import_displacement(inp$ember, inp$import_dep, ms$fuel_supplier_risk$techs, cfg, risk = inp$fuel_risk,
                                     measure = "fuel_supplier_risk") else NULL,
    if (!is.null(ms$electricity_import_share)) dn_measure_electricity_imports(inp$ember, dn_measure_techs(ms$electricity_import_share), cfg) else NULL,
    if (!is.null(ms$access_gap) && !is.null(inp$access)) dn_measure_access_gap(inp$access, dn_measure_techs(ms$access_gap), cfg) else NULL,
    if (!is.null(ms$climate_pathway) && !is.null(inp$neo) && !is.null(inp$neo_region))
      dn_measure_climate_pathway(inp$neo, inp$neo_region, cfg) else NULL,
    if (!is.null(ms$ev_sales_gap) && !is.null(inp$gevo_share)) dn_measure_ev_sales_gap(inp$gevo_share, dn_measure_techs(ms$ev_sales_gap), cfg) else NULL,
    if (!is.null(ms$fossil_power_share)) dn_measure_fossil_power_share(inp$ember, dn_measure_techs(ms$fossil_power_share), cfg) else NULL,
    # the BNEF growth sensitivity (only where a sensitivity config names neo_growth)
    if (!is.null(ms$neo_growth) && !is.null(inp$neo) && !is.null(inp$neo_region)) dn_measure_neo_growth(inp$neo, inp$neo_region, cfg) else NULL
  )
  parts <- parts[!vapply(parts, is.null, logical(1))]
  measures <- do.call(rbind, c(list(dn_empty_measures()), parts))
  rownames(measures) <- NULL
  list(measures = measures, shortfall = shortfall, installed = installed, cap_series = cap_series)
}

# Coverage per technology x family (and per measure): countries with a value, out of the universe.
dn_coverage <- function(dn_tbl, scored) {
  fam <- do.call(rbind, lapply(c(DN_FAMILIES, "deployment_need"), function(f) {
    v <- dn_tbl[[f]]
    a <- stats::aggregate(list(n_observed = !is.na(v)), by = list(tech = dn_tbl$tech), FUN = sum)
    a$n_countries <- as.integer(table(dn_tbl$tech)[a$tech])
    a$level <- "family"
    a$item <- f
    a
  }))
  if (nrow(scored) > 0) {
    m <- stats::aggregate(list(n_observed = rep(1L, nrow(scored))), by = list(tech = scored$tech, item = scored$measure), FUN = sum)
    m$n_countries <- as.integer(table(dn_tbl$tech)[m$tech])
    m$level <- "measure"
    nat <- stats::aggregate(list(n_national = scored$basis == "national"), by = list(tech = scored$tech, item = scored$measure), FUN = sum)
    m <- merge(m, nat, by = c("tech", "item"))
    fam$n_national <- NA_integer_
    fam <- rbind(fam[, c("level", "item", "tech", "n_observed", "n_national", "n_countries")],
                 m[, c("level", "item", "tech", "n_observed", "n_national", "n_countries")])
  } else {
    fam$n_national <- NA_integer_
    fam <- fam[, c("level", "item", "tech", "n_observed", "n_national", "n_countries")]
  }
  fam$share_observed <- round(fam$n_observed / fam$n_countries, 3)
  fam[order(fam$level, fam$item, fam$tech), , drop = FALSE]
}
