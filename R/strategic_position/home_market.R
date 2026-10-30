# National home-market growth (design section 2 "Prize", D13): the expected growth of domestic deployment
# of each technology, per country, from national evidence only. It is the io_home_market input that lifts
# a node's industrial prize where real national data exist; regional values are never used.
#
# Three kinds of evidence, each an expected volume of additions per year in the technology's own unit
# (GW a year for power, storage; vehicles a year for EVs; EJ a year for oil):
#   scenario        IEA WEO 2025 Stated Policies for the single-country WEO regions (the generation growth
#                   factor 2024 -> 2035 applied to Ember installed capacity) and IEA GEVO 2026 Stated
#                   Policies EV stock;
#   target          Ember 2030 targets and approved national-plan capacities: (target - installed) / years;
#   observed_trend  Ember installed-capacity additions over the trend window, EI grid-scale BESS additions,
#                   GEVO EV car sales, EI oil consumption change.
# Each kind is ranked within the technology across countries (mid-rank percentile of the additions: volume,
# so a tiny base growing fast does not outrank a large market); home_market is the mean of the observed
# kinds' percentiles. BNEF (subscription) values never enter this table (D13).
#
# Pure functions: no IO. Uses dn_* helpers from R/strategic_position/deployment_need.R and sp_midrank_pct()
# from R/strategic_position/sp_helpers.R.

HM_KINDS <- c("scenario", "target", "observed_trend")
HM_KIND_SCENARIO <- c(scenario = "stated_policies", target = "national_commitments", observed_trend = "observed")
HM_MEASURE_COLS <- c("iso3", "tech", "kind", "additions", "growth_rate", "unit", "basis", "scenario", "source",
                     "year_from", "year_to", "context")

hm_empty <- function() {
  dn_df(iso3 = character(), tech = character(), kind = character(), additions = numeric(), growth_rate = numeric(),
        unit = character(), basis = character(), scenario = character(), source = character(),
        year_from = integer(), year_to = integer(), context = character())
}

hm_finish <- function(rows, kind, hcfg) {
  if (is.null(rows) || nrow(rows) == 0) return(hm_empty())
  rows$kind <- kind
  rows$basis <- "national"
  rows$scenario <- unname(HM_KIND_SCENARIO[kind])
  units <- unlist(hcfg$units)
  rows$unit <- unname(units[rows$tech])
  rows$year_from <- as.integer(rows$year_from)
  rows$year_to <- as.integer(rows$year_to)
  for (col in setdiff(HM_MEASURE_COLS, names(rows))) rows[[col]] <- NA
  rows <- rows[!is.na(rows$additions), HM_MEASURE_COLS, drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# Scenario kind for power technologies: installed capacity (Ember, latest year) scaled by the WEO Stated
# Policies generation growth factor of the technology (constant capacity factor), spread over the years to
# the scenario year. `installed`: dn_installed_capacity() rows (iso3, key, installed_year, installed_gw).
hm_scenario_power <- function(installed, weo_national, cfg) {
  hcfg <- cfg$home_market
  base_y <- as.integer(cfg$scenario_base_year)
  scen_y <- as.integer(cfg$scenario_year)
  steps <- cfg$weo_scenarios[[hcfg$scenario]]
  out <- list()
  for (tech in names(hcfg$capacity_source)) {
    key <- hcfg$capacity_source[[tech]]
    series <- cfg$weo_series[[key]]
    if (is.null(series)) next
    g0 <- dn_weo_value(weo_national, series, "historical", base_y)
    g1 <- dn_weo_value(weo_national, series, steps, scen_y)
    g <- merge(g0, g1, by = "iso3", suffixes = c("_from", "_to"))
    g <- g[!is.na(g$value_from) & g$value_from > 0, , drop = FALSE]
    inst <- installed[installed$key == key, c("iso3", "installed_year", "installed_gw")]
    d <- merge(g, inst, by = "iso3")
    if (nrow(d) == 0) next
    proj <- d$installed_gw * d$value_to / d$value_from
    out[[tech]] <- dn_df(iso3 = d$iso3, tech = tech, additions = (proj - d$installed_gw) / (scen_y - d$installed_year),
                         growth_rate = dn_cagr_pct(d$value_to, d$value_from, scen_y - base_y),
                         source = sprintf("IEA WEO 2025 Stated Policies (single-country region): %s growth %d-%d, applied to Ember installed capacity", series, base_y, scen_y),
                         year_from = d$installed_year, year_to = scen_y,
                         context = sprintf("%.1f GW (%d) x %.0f/%.0f TWh -> %.1f GW by %d", d$installed_gw, d$installed_year,
                                           d$value_to, d$value_from, proj, scen_y))
  }
  hm_finish(if (length(out)) do.call(rbind, out) else NULL, "scenario", hcfg)
}

# Scenario kind for EVs: GEVO Stated Policies car EV stock in the scenario year vs the latest historical
# stock. `gevo_stock`: iso3, category, year, value (vehicles).
hm_scenario_ev <- function(gevo_stock, cfg, tech = "Electric Vehicles") {
  hcfg <- cfg$home_market
  scen_y <- as.integer(cfg$scenario_year)
  hist <- gevo_stock[gevo_stock$category %in% "Historical", c("iso3", "year", "value")]
  latest <- dn_window(hist, 0)
  proj <- gevo_stock[gevo_stock$category %in% hcfg$ev_scenario & gevo_stock$year %in% scen_y, c("iso3", "value")]
  d <- merge(latest[, c("iso3", "year_to", "value_to")], proj, by = "iso3")
  if (nrow(d) == 0) return(hm_empty())
  rows <- dn_df(iso3 = d$iso3, tech = tech, additions = (d$value - d$value_to) / (scen_y - d$year_to),
                growth_rate = dn_cagr_pct(d$value, d$value_to, scen_y - d$year_to),
                source = "IEA Global EV Outlook 2026 Stated Policies: car EV stock",
                year_from = d$year_to, year_to = scen_y,
                context = sprintf("EV car stock %.2f m (%d) -> %.2f m (%d)", d$value_to / 1e6, d$year_to, d$value / 1e6, scen_y))
  hm_finish(rows, "scenario", hcfg)
}

# Target kind: the additions a year the national target or approved plan requires from today's installed
# capacity. `shortfall`: dn_target_shortfall().
hm_target <- function(shortfall, cfg) {
  hcfg <- cfg$home_market
  out <- list()
  for (tech in names(hcfg$target_key)) {
    s <- shortfall[shortfall$target_key == hcfg$target_key[[tech]] & !is.na(shortfall$required_rate_gw), , drop = FALSE]
    if (nrow(s) == 0) next
    out[[tech]] <- dn_df(iso3 = s$iso3, tech = tech, additions = s$required_rate_gw,
                         growth_rate = dn_cagr_pct(s$target_gw, s$installed_gw, s$years_left),
                         source = s$target_source, year_from = s$installed_year, year_to = s$target_year,
                         context = sprintf("%.1f GW (%d) -> %s %.1f GW by %d", s$installed_gw, s$installed_year,
                                           ifelse(s$target_kind == "national_plan", "plan", "target"), s$target_gw, s$target_year))
  }
  hm_finish(if (length(out)) do.call(rbind, out) else NULL, "target", hcfg)
}

# Observed-trend kind for capacity technologies: mean annual capacity additions over the trend window.
# `cap_series`: iso3, key, year, capacity_gw (Ember sources; EI BESS as key Battery).
hm_trend_capacity <- function(cap_series, cfg) {
  hcfg <- cfg$home_market
  k <- as.integer(cfg$trend_years)
  inst <- dn_installed_capacity(cap_series, k)
  out <- list()
  for (tech in names(hcfg$capacity_source)) {
    key <- hcfg$capacity_source[[tech]]
    s <- inst[inst$key == key & !is.na(inst$build_rate_gw), , drop = FALSE]
    if (nrow(s) == 0) next
    src <- if (key == "Battery") "Energy Institute Statistical Review 2026: grid-scale battery storage capacity" else
      paste0("Ember yearly electricity data: ", tolower(key), " capacity")
    out[[tech]] <- dn_df(iso3 = s$iso3, tech = tech, additions = s$build_rate_gw,
                         growth_rate = dn_cagr_pct(s$installed_gw, s$capacity_from, k), source = src,
                         year_from = s$installed_year - k, year_to = s$installed_year,
                         context = sprintf("%.1f -> %.1f GW (%d-%d)", s$capacity_from, s$installed_gw, s$installed_year - k, s$installed_year))
  }
  hm_finish(if (length(out)) do.call(rbind, out) else NULL, "observed_trend", hcfg)
}

# Observed-trend kind for EVs: mean annual car EV sales over the last ev_sales_years historical years.
# `gevo_sales`: iso3, category, year, value (vehicles).
hm_trend_ev <- function(gevo_sales, cfg, tech = "Electric Vehicles") {
  hcfg <- cfg$home_market
  n <- as.integer(hcfg$ev_sales_years)
  hist <- gevo_sales[gevo_sales$category %in% "Historical" & !is.na(gevo_sales$value), c("iso3", "year", "value")]
  if (nrow(hist) == 0) return(hm_empty())
  rows <- lapply(split(hist, hist$iso3), function(h) {
    h <- h[order(h$year), , drop = FALSE]
    last <- max(h$year)
    win <- h[h$year > last - n, , drop = FALSE]
    if (nrow(win) < n) return(NULL)
    first <- win$value[1]
    dn_df(iso3 = h$iso3[1], tech = tech, additions = mean(win$value),
          growth_rate = dn_cagr_pct(win$value[nrow(win)], first, n - 1),
          source = "IEA Global EV Outlook 2026: car EV sales", year_from = min(win$year), year_to = last,
          context = sprintf("mean %.0f EV car sales a year (%d-%d)", mean(win$value), min(win$year), last))
  })
  rows <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  hm_finish(rows, "observed_trend", hcfg)
}

# Observed-trend kind for oil: change in oil consumption, EJ a year, over the trend window.
hm_trend_oil <- function(oil, cfg, tech = "Oil") {
  hcfg <- cfg$home_market
  k <- as.integer(cfg$trend_years)
  w <- dn_window(oil, k)
  w <- w[!is.na(w$value_from) & w$value_from >= cfg$min_oil_ej, , drop = FALSE]
  rows <- dn_df(iso3 = w$iso3, tech = tech, additions = (w$value_to - w$value_from) / k,
                growth_rate = dn_cagr_pct(w$value_to, w$value_from, k),
                source = "Energy Institute Statistical Review of World Energy 2026: oil consumption",
                year_from = w$year_from, year_to = w$year_to,
                context = sprintf("oil use %.2f -> %.2f EJ (%d-%d)", w$value_from, w$value_to, w$year_from, w$year_to))
  hm_finish(rows, "observed_trend", hcfg)
}

# home_market per universe country x technology. Every row of the grid is kept; a technology with no
# national evidence stays NA (hm_basis not_assessed). The primary columns show the highest-priority kind
# observed (scenario, then target, then observed trend) for display.
hm_assemble <- function(measures, universe, cfg) {
  techs <- unlist(cfg$techs)
  m <- measures[measures$iso3 %in% universe$iso3 & measures$tech %in% techs & !is.na(measures$additions), , drop = FALSE]
  if (nrow(m) > 0) {
    m <- dplyr::mutate(dplyr::group_by(m, .data$tech, .data$kind), pct = sp_midrank_pct(.data$additions))
    m <- as.data.frame(dplyr::ungroup(m))
  } else {
    m$pct <- numeric()
  }
  grid <- dn_df(iso3 = rep(universe$iso3, times = length(techs)), Country = rep(universe$Country, times = length(techs)),
                tech = rep(techs, each = nrow(universe)))
  key <- paste(grid$iso3, grid$tech)
  pcts <- matrix(NA_real_, nrow(grid), length(HM_KINDS), dimnames = list(NULL, HM_KINDS))
  for (kind in HM_KINDS) {
    s <- m[m$kind == kind, , drop = FALSE]
    i <- match(key, paste(s$iso3, s$tech))
    p <- paste0("hm_", kind)
    grid[[paste0(p, "_additions")]] <- s$additions[i]
    grid[[paste0(p, "_growth_rate")]] <- s$growth_rate[i]
    grid[[paste0(p, "_pct")]] <- s$pct[i]
    grid[[paste0(p, "_scenario")]] <- ifelse(is.na(i), NA_character_, unname(HM_KIND_SCENARIO[kind]))
    grid[[paste0(p, "_source")]] <- s$source[i]
    grid[[paste0(p, "_years")]] <- ifelse(is.na(i), NA_character_, paste0(s$year_from[i], "-", s$year_to[i]))
    grid[[paste0(p, "_context")]] <- s$context[i]
    pcts[, kind] <- s$pct[i]
  }
  grid$hm_kinds_observed <- as.integer(rowSums(!is.na(pcts)))
  grid$home_market <- sp_row_mean_available(pcts)
  grid$home_market_pct <- NA_real_
  for (t in techs) {
    i <- grid$tech == t
    grid$home_market_pct[i] <- sp_midrank_pct(grid$home_market[i])
  }
  grid$hm_basis <- ifelse(grid$hm_kinds_observed > 0, "national", "not_assessed")
  grid$hm_unit <- unname(unlist(cfg$home_market$units)[grid$tech])
  prim <- rep(NA_character_, nrow(grid))
  for (kind in rev(HM_KINDS)) prim[!is.na(pcts[, kind])] <- kind
  grid$hm_primary_kind <- prim
  pick <- function(suffix) {
    out <- rep(NA, nrow(grid))
    for (kind in HM_KINDS) {
      i <- which(prim == kind)
      out[i] <- grid[[paste0("hm_", kind, "_", suffix)]][i]
    }
    out
  }
  grid$hm_primary_additions <- as.numeric(pick("additions"))
  grid$hm_primary_growth_rate <- as.numeric(pick("growth_rate"))
  grid$hm_primary_source <- as.character(pick("source"))
  grid$hm_primary_years <- as.character(pick("years"))
  # The engine's names (R/strategic_position/industrial_opportunity.R reads io_home_market first).
  grid$io_home_market <- grid$home_market
  grid$io_home_market_basis <- grid$hm_basis
  lead <- c("Country", "iso3", "tech", "io_home_market", "io_home_market_basis", "home_market", "home_market_pct",
            "hm_kinds_observed", "hm_basis", "hm_unit", "hm_primary_kind", "hm_primary_additions",
            "hm_primary_growth_rate", "hm_primary_source", "hm_primary_years")
  grid[, c(lead, setdiff(names(grid), lead))]
}

# Every home-market measure from prepared inputs. `inp`: as for dn_build_measures(), plus gevo_stock and
# gevo_sales (dn_gevo_prepare() for "EV stock" and "EV sales", Cars, EV); `built`: dn_build_measures()
# output (installed capacity, target shortfall, capacity series). BNEF is never an input.
hm_build_measures <- function(inp, built, cfg) {
  parts <- list(
    hm_scenario_power(built$installed, inp$weo_national, cfg),
    if (!is.null(inp$gevo_stock) && nrow(inp$gevo_stock) > 0) hm_scenario_ev(inp$gevo_stock, cfg) else NULL,
    if (!is.null(built$shortfall) && nrow(built$shortfall) > 0) hm_target(built$shortfall, cfg) else NULL,
    hm_trend_capacity(built$cap_series, cfg),
    if (!is.null(inp$gevo_sales) && nrow(inp$gevo_sales) > 0) hm_trend_ev(inp$gevo_sales, cfg) else NULL,
    if (!is.null(inp$oil) && nrow(inp$oil) > 0) hm_trend_oil(inp$oil, cfg) else NULL
  )
  parts <- parts[!vapply(parts, is.null, logical(1))]
  out <- do.call(rbind, c(list(hm_empty()), parts))
  rownames(out) <- NULL
  out
}
