# PRIZE: the clean Industrial Opportunity of the canonical decision framework (docs/strategic_position.md,
# "Prize"). It answers one question: is there an economic or technological prize worth pursuing in this
# technology x six-stage node? It is deliberately separate from POSITION (Competitive Position) and from
# NEED (supply_chain_need / deployment_need):
#
#   io_node_prize           the node's market prize, the same for every country: the expected growth of
#                           world demand 2024 -> 2035 under current policy (IEA WEO 2025 World, Stated
#                           Policies; IEA Critical Minerals Outlook, Stated Policies, for the mineral
#                           Extraction and Refining nodes), as a percentile across the producing nodes,
#                           averaged with the node's global manufacturing investment momentum (Clean
#                           Investment Monitor capex trend) where that is observed. A declining world
#                           market caps it (and an oversupplied one, from a shippable source only).
#   io_home_market          national expected growth of domestic deployment (data/processed/
#                           home_market_tbl.rds, scripts/95): lifts the prize only where the value is
#                           national (regional proxies are shown, never scored).
#   industrial_opportunity  io_node_prize, lifted to the mean of io_node_prize and io_home_market where the
#                           home market is national and above the node prize (home_market_mode lift, the
#                           user's "lifted by national home-market growth"): a fast-growing home market adds
#                           to the prize, a small one does not take a world-market prize away from an
#                           export-oriented producer. home_market_mode mean averages both ways.
#   io_maturity / io_maturity_class  TRL 2025 (IEA Clean Technology Guide) and the curated maturity:
#                           technology context, shown, never scored (country-invariant).
#
# Nothing that measures current capability (export share, RCA, production, operating capacity, annual
# investment, reserves, deployment scale) and nothing that measures need (HHI, import dependence, risky
# suppliers) enters it: SP_IO_FORBIDDEN lists them and sp_io_check_inputs() refuses them. Feasibility,
# pipeline, adjacency, learning and input cost are not in the prize: they are the credible-pathway test
# for Catch Up (pathway.R).
#
# Pure functions: no IO. Needs sp_helpers.R and R/utils/supply_chain_stages.R.

SP_IO_PRODUCING_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Recovery")
SP_IO_OUTLOOKS <- c("growing", "flat", "declining")
# Variables that measure Competitive Position or Strategic Need and must never be a prize component.
SP_IO_FORBIDDEN <- c(
  "market_share", "rca", "export_size", "size", "Overall Production", "growth_abs", "Operating Capacity Index",
  "Annual Investment Index", "Reserves", "Potential Index", "LCOE Index", "HHI", "import_hhi", "deficit_gdp",
  "Production surplus/deficit", "china_share", "opposite_bloc_share", "coercive_import_share", "imports_usd",
  "competitive_position_percentile", "competitive_position_score", "strategic_need_percentile", "supply_chain_need",
  "home_share", "trusted_world_share", "Input Cost Index", "feas"
)
# The prize components a cell may carry (the drawer's PRIZE question).
SP_IO_COMPONENTS <- c("io_growth", "io_growth_pct", "io_investment_momentum", "io_investment_momentum_pct",
                      "io_node_prize", "io_home_market")

# "Electric Grid", "Refining" -> "electric-grid-refining" (the iea_pathways cell key, scripts/17).
sp_io_cell_key <- function(tech, stage) paste0(gsub("[^a-z0-9]+", "-", tolower(tech)), "-", tolower(stage))

# Refuse a component list that contains a capability or need variable.
sp_io_check_inputs <- function(components) {
  bad <- intersect(components, SP_IO_FORBIDDEN)
  if (length(bad)) stop("sp_io_check_inputs(): capability or need variable(s) in the prize: ", paste(bad, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Outlook class of a growth rate.
sp_io_outlook <- function(growth, cfg = list()) {
  o <- cfg$outlook %||% list(declining_below = -0.05, growing_from = 0.05)
  ifelse(is.na(growth), NA_character_,
         ifelse(growth < o$declining_below, "declining", ifelse(growth >= o$growing_from, "growing", "flat")))
}

# World demand growth of every node. map: iea_pathways_map_tbl (cell, series, role, match, order);
# values: iea_pathways_values_tbl (series, scenario, year, value); cells_meta: iea_pathways_cells_tbl (cell,
# match, mineral_techs); minerals: iea_pathways_minerals_tbl (sheet, mineral, tech, scenario, year, kt);
# techs: the headline technologies. cfg: decision_framework$industrial_opportunity.
#
# Growth = value(scenario, horizon) / value(historical, base) - 1 of the node's primary series (the
# lowest `order` with role own or demand); for the mineral stages of a technology with mineral_techs, the
# total key-mineral demand (sheet 1, kt) of those technologies, minerals_base_year -> horizon. A node with
# no series (Recovery, Geothermal Refining) takes its technology's Deployment series as a proxy: what is
# recovered follows the installed base. Returns tech, supply_chain, io_growth, io_growth_current_policies,
# io_growth_series, io_growth_basis (weo_world_stated | iea_minerals_stated | proxy_deployment), io_growth_match,
# io_outlook.
sp_io_node_growth <- function(map, values, cells_meta, minerals = NULL, techs, cfg = list()) {
  base <- as.integer(cfg$base_year %||% 2024L)
  horizon <- as.integer(cfg$horizon_year %||% 2035L)
  scen <- cfg$scenario %||% "stated"
  mbase <- as.integer(cfg$minerals_base_year %||% 2025L)
  mstages <- unlist(cfg$mineral_stages %||% c("Extraction", "Refining"))
  val <- function(series, scenario, year) {
    v <- values$value[values$series == series & values$scenario == scenario & values$year == year]
    if (length(v)) v[1] else NA_real_
  }
  growth_of <- function(series, scenario) {
    b <- val(series, "historical", base)
    h <- val(series, scenario, horizon)
    if (is.na(b) || is.na(h) || b <= 0) NA_real_ else h / b - 1
  }
  primary <- map[!is.na(map$role) & map$role %in% c("own", "demand"), , drop = FALSE]
  primary <- primary[order(primary$cell, primary$order), , drop = FALSE]
  primary <- primary[!duplicated(primary$cell), , drop = FALSE]
  mineral_growth <- function(mt, scenario) {
    if (is.null(minerals) || is.na(mt) || !nzchar(mt)) return(NA_real_)
    tt <- strsplit(mt, "|", fixed = TRUE)[[1]]
    m <- minerals[minerals$sheet == "1" & minerals$tech %in% tt & minerals$scenario == scenario, , drop = FALSE]
    b <- sum(m$kt[m$year == mbase], na.rm = TRUE)
    h <- sum(m$kt[m$year == horizon], na.rm = TRUE)
    if (!is.finite(b) || b <= 0) NA_real_ else h / b - 1
  }
  g <- expand.grid(tech = techs, supply_chain = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
  g$cell <- sp_io_cell_key(g$tech, g$supply_chain)
  out <- lapply(seq_len(nrow(g)), function(i) {
    cell <- g$cell[i]
    meta <- cells_meta[cells_meta$cell == cell, , drop = FALSE]
    mt <- if (nrow(meta) && "mineral_techs" %in% names(meta)) meta$mineral_techs[1] else NA_character_
    match_q <- if (nrow(meta)) meta$match[1] else NA_character_
    if (g$supply_chain[i] %in% mstages && !is.na(mt) && nzchar(mt)) {
      gr <- mineral_growth(mt, scen)
      if (!is.na(gr)) {
        return(data.frame(io_growth = gr, io_growth_current_policies = mineral_growth(mt, "current"),
                          io_growth_series = paste0("minerals:", mt), io_growth_basis = "iea_minerals_stated",
                          io_growth_match = match_q, stringsAsFactors = FALSE))
      }
    }
    p <- primary[primary$cell == cell, , drop = FALSE]
    basis <- "weo_world_stated"
    if (!nrow(p)) {
      p <- primary[primary$cell == sp_io_cell_key(g$tech[i], "Deployment"), , drop = FALSE]
      basis <- "proxy_deployment"
      match_q <- "proxy"
    }
    if (!nrow(p)) {
      return(data.frame(io_growth = NA_real_, io_growth_current_policies = NA_real_, io_growth_series = NA_character_,
                        io_growth_basis = "not_assessed", io_growth_match = NA_character_, stringsAsFactors = FALSE))
    }
    data.frame(io_growth = growth_of(p$series[1], scen), io_growth_current_policies = growth_of(p$series[1], "current"),
               io_growth_series = p$series[1], io_growth_basis = basis, io_growth_match = match_q, stringsAsFactors = FALSE)
  })
  out <- cbind(g[, c("tech", "supply_chain")], do.call(rbind, out))
  out$io_outlook <- sp_io_outlook(out$io_growth, cfg)
  out
}

# Global manufacturing investment momentum per node: the log change of world capex (Clean Investment
# Monitor annual actual investment summed over countries) from the baseline years to the latest years.
# inv: investment_momentum_tbl rows of the annual-investment variable (tech, supply_chain, Year, value,
# USD bn). Nodes whose baseline capex is below min_baseline_usd_bn, or that the monitor does not cover,
# are NA (not observed). Returns tech, supply_chain, io_investment_momentum, io_investment_latest_usd_bn,
# io_investment_baseline_usd_bn.
sp_io_investment_momentum <- function(inv, cfg = list()) {
  latest <- as.integer(unlist(cfg$latest_years %||% c(2024L, 2025L)))
  baseline <- as.integer(unlist(cfg$baseline_years %||% c(2021L, 2022L)))
  min_base <- cfg$min_baseline_usd_bn %||% 0.5
  empty <- data.frame(tech = character(), supply_chain = character(), io_investment_momentum = numeric(),
                      io_investment_latest_usd_bn = numeric(), io_investment_baseline_usd_bn = numeric())
  if (is.null(inv) || !nrow(inv)) return(empty)
  agg <- inv %>%
    dplyr::filter(!is.na(.data$value), .data$Year %in% c(latest, baseline)) %>%
    dplyr::group_by(.data$tech, .data$supply_chain, .data$Year) %>%
    dplyr::summarise(v = sum(.data$value), .groups = "drop") %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(io_investment_latest_usd_bn = sum(.data$v[.data$Year %in% latest]) / length(latest),
                     io_investment_baseline_usd_bn = sum(.data$v[.data$Year %in% baseline]) / length(baseline),
                     .groups = "drop")
  agg$io_investment_momentum <- ifelse(agg$io_investment_baseline_usd_bn >= min_base & agg$io_investment_latest_usd_bn > 0,
                                       log(agg$io_investment_latest_usd_bn / agg$io_investment_baseline_usd_bn), NA_real_)
  as.data.frame(agg[, names(empty)])
}

# The node prize. growth: sp_io_node_growth(); momentum: sp_io_investment_momentum(); maturity
# (optional): tech, supply_chain, io_maturity (TRL), io_maturity_class. cfg:
# decision_framework$industrial_opportunity. Percentiles are mid-rank across the producing nodes
# (SP_IO_PRODUCING_STAGES) of the headline technologies; Deployment nodes get their growth for context
# but no prize (Deployment is decided on deployment_need). Returns one row per node with io_growth_pct,
# io_investment_momentum_pct, io_node_prize, io_node_prize_capped, io_oversupplied and the inputs.
sp_io_node_prize <- function(growth, momentum = NULL, maturity = NULL, cfg = list()) {
  w_mom <- (cfg$investment_momentum %||% list())$weight %||% 0.25
  caps <- cfg$caps %||% list(declining = 0.33, oversupplied = 0.5)
  over <- unlist(cfg$oversupplied_nodes %||% character())
  x <- growth
  if (!is.null(momentum) && nrow(momentum)) {
    x <- dplyr::left_join(x, momentum, by = c("tech", "supply_chain"))
  } else {
    x$io_investment_momentum <- NA_real_
  }
  prod <- x$supply_chain %in% SP_IO_PRODUCING_STAGES
  x$io_growth_pct <- NA_real_
  x$io_growth_pct[prod] <- sp_midrank_pct(x$io_growth[prod])
  x$io_investment_momentum_pct <- NA_real_
  x$io_investment_momentum[!prod] <- NA_real_
  x$io_investment_momentum_pct[prod] <- sp_midrank_pct(x$io_investment_momentum[prod])
  has_m <- !is.na(x$io_investment_momentum_pct)
  raw <- ifelse(has_m, (1 - w_mom) * x$io_growth_pct + w_mom * x$io_investment_momentum_pct, x$io_growth_pct)
  key <- paste(x$tech, x$supply_chain, sep = "|")
  x$io_oversupplied <- key %in% over
  cap <- ifelse(x$io_outlook %in% "declining", caps$declining %||% 0.33, 1)
  cap <- pmin(cap, ifelse(x$io_oversupplied, caps$oversupplied %||% 0.5, 1))
  x$io_node_prize_capped <- !is.na(raw) & raw > cap
  x$io_node_prize <- ifelse(prod, pmin(raw, cap), NA_real_)
  x$io_prize_basis <- ifelse(!prod, "deployment_stage", ifelse(is.na(raw), "not_assessed",
                             ifelse(has_m, paste0(x$io_growth_basis, "+investment_momentum"), x$io_growth_basis)))
  if (!is.null(maturity) && nrow(maturity)) x <- dplyr::left_join(x, maturity, by = c("tech", "supply_chain"))
  for (cl in c("io_maturity", "io_maturity_class")) if (!cl %in% names(x)) x[[cl]] <- NA
  x
}

# The technology-level market outlook the engine's catch-up test and the legacy opportunity cap read:
# the outlook of the technology's Deployment node (its own demand series), with documented manual
# overrides (config market_outlook_override) on top. Returns a named character vector tech -> outlook.
sp_io_tech_outlook <- function(growth, override = list()) {
  d <- growth[growth$supply_chain == "Deployment", , drop = FALSE]
  out <- stats::setNames(d$io_outlook, d$tech)
  ov <- unlist(override %||% list())
  if (length(ov)) {
    bad <- setdiff(unique(ov), SP_IO_OUTLOOKS)
    if (length(bad)) stop("sp_io_tech_outlook(): unknown outlook(s) in the override: ", paste(bad, collapse = ", "), call. = FALSE)
    out[names(ov)] <- ov
  }
  out[!is.na(out)]
}

# Industrial Opportunity per country x technology x stage. cells: iso3, tech, supply_chain (any further
# columns pass through). node_prize: sp_io_node_prize(). home: optional national home-market table (iso3,
# tech, optional supply_chain, io_home_market 0-1, io_home_market_basis). cfg:
# decision_framework$industrial_opportunity (home_market_bases: the bases that enter the score).
# Deployment rows get no industrial_opportunity (their Need is deployment_need, and they have no prize
# test). Adds the node prize columns, io_home_market, io_home_market_basis, io_home_market_scored,
# io_home_market_lift (the home market raised the prize), industrial_opportunity and io_basis (node |
# node_and_home_market | not_assessed).
sp_industrial_opportunity <- function(cells, node_prize, home = NULL, cfg = list()) {
  bases <- unlist(cfg$home_market_bases %||% "national")
  mode <- cfg$home_market_mode %||% "lift"
  if (!mode %in% c("lift", "mean")) stop("sp_industrial_opportunity(): home_market_mode must be lift or mean", call. = FALSE)
  keep <- intersect(c("tech", "supply_chain", "io_growth", "io_growth_current_policies", "io_growth_series", "io_growth_basis",
                      "io_growth_match", "io_outlook", "io_growth_pct", "io_investment_momentum", "io_investment_momentum_pct",
                      "io_investment_latest_usd_bn", "io_investment_baseline_usd_bn", "io_node_prize", "io_node_prize_capped",
                      "io_oversupplied", "io_prize_basis", "io_maturity", "io_maturity_class"), names(node_prize))
  drop <- setdiff(c(keep, "io_home_market", "io_home_market_basis", "io_home_market_scored", "io_home_market_lift", "industrial_opportunity", "io_basis"),
                  c("tech", "supply_chain"))
  x <- cells[, setdiff(names(cells), drop), drop = FALSE]
  x <- dplyr::left_join(x, node_prize[, keep, drop = FALSE], by = c("tech", "supply_chain"))
  x$io_home_market <- NA_real_
  x$io_home_market_basis <- NA_character_
  if (!is.null(home) && nrow(home)) {
    h <- sp_io_home_market_std(home)
    by <- intersect(c("iso3", "tech", "supply_chain"), names(h))
    h <- h[!duplicated(h[, by]), c(by, "io_home_market", "io_home_market_basis"), drop = FALSE]
    names(h)[names(h) %in% c("io_home_market", "io_home_market_basis")] <- c(".hm", ".hmb")
    x <- dplyr::left_join(x, h, by = by)
    x$io_home_market <- x$.hm
    x$io_home_market_basis <- x$.hmb
    x$.hm <- NULL
    x$.hmb <- NULL
  }
  prod <- x$supply_chain %in% SP_IO_PRODUCING_STAGES
  x$io_home_market[!prod] <- NA_real_
  x$io_home_market_scored <- prod & !is.na(x$io_home_market) & x$io_home_market_basis %in% bases
  avg <- (x$io_node_prize + x$io_home_market) / 2
  with_home <- if (mode == "lift") pmax(x$io_node_prize, avg) else avg
  x$industrial_opportunity <- ifelse(!prod, NA_real_, ifelse(x$io_home_market_scored, with_home, x$io_node_prize))
  # a declining (or oversupplied) world market stays capped whatever the home market does
  cap_n <- x$io_node_prize_capped %in% TRUE | x$io_outlook %in% "declining"
  x$industrial_opportunity <- ifelse(cap_n & !is.na(x$industrial_opportunity), pmin(x$industrial_opportunity, pmax(x$io_node_prize, 0)),
                                     x$industrial_opportunity)
  x$io_home_market_lift <- x$io_home_market_scored & !is.na(x$industrial_opportunity) & !is.na(x$io_node_prize) &
    abs(x$industrial_opportunity - x$io_node_prize) > 1e-12
  x$io_basis <- ifelse(!prod, NA_character_, ifelse(is.na(x$industrial_opportunity), "not_assessed",
                       ifelse(x$io_home_market_lift, "node_and_home_market", "node")))
  x
}

# The home-market table with standard column names: io_home_market (0-1) and io_home_market_basis.
# Accepts the names scripts/95 may use (home_market, home_market_pct; basis, home_market_basis).
sp_io_home_market_std <- function(home) {
  h <- as.data.frame(home, stringsAsFactors = FALSE)
  pick <- function(cands) { hit <- intersect(cands, names(h)); if (length(hit)) hit[1] else NA_character_ }
  v <- pick(c("io_home_market", "home_market", "home_market_pct", "home_market_score"))
  b <- pick(c("io_home_market_basis", "home_market_basis", "hm_basis", "basis"))
  if (is.na(v)) stop("sp_io_home_market_std(): no home-market value column (io_home_market)", call. = FALSE)
  if (!"iso3" %in% names(h) || !"tech" %in% names(h)) stop("sp_io_home_market_std(): needs iso3 and tech", call. = FALSE)
  h$io_home_market <- suppressWarnings(as.numeric(h[[v]]))
  h$io_home_market_basis <- if (is.na(b)) "national" else as.character(h[[b]])
  if (any(!is.na(h$io_home_market) & (h$io_home_market < 0 | h$io_home_market > 1))) {
    stop("sp_io_home_market_std(): io_home_market must be in [0, 1]", call. = FALSE)
  }
  h
}

# ---- Validation (spec XXI.2) ------------------------------------------------------------------------
# Is Industrial Opportunity still a disguised capability score? tbl: the cell table with
# industrial_opportunity, competitive_position_percentile, supply_chain_need and the legacy
# opportunity_percentile (legacy_eo_opportunity_pct). Returns a list of tables:
#   correlations  Spearman of the prize with CP and with need, pooled over producing cells and the median
#                 within nodes (within a node only the home market can vary the prize)
#   weights       the share of the prize from each component, over producing cells
#   movers        the cells whose percentile moves most from the legacy EO opportunity
#   disguise      per node: within-node Spearman with CP and the share of countries with a home market
sp_io_validation <- function(tbl, n_movers = 40, momentum_weight = 0.25) {
  x <- tbl[tbl$supply_chain %in% SP_IO_PRODUCING_STAGES & !is.na(tbl$industrial_opportunity), , drop = FALSE]
  sp <- function(a, b) { ok <- !is.na(a) & !is.na(b); if (sum(ok) < 5 || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) NA_real_ else stats::cor(a[ok], b[ok], method = "spearman") }
  col <- function(n) if (n %in% names(x)) x[[n]] else rep(NA_real_, nrow(x))
  cp <- col("competitive_position_percentile")
  need <- col("supply_chain_need")
  legacy <- col("legacy_eo_opportunity_pct")
  node <- paste(x$tech, x$supply_chain, sep = "|")
  within <- function(v) {
    r <- vapply(split(seq_len(nrow(x)), node), function(i) sp(x$industrial_opportunity[i], v[i]), numeric(1))
    stats::median(r, na.rm = TRUE)
  }
  correlations <- data.frame(
    measure = c("competitive_position_percentile", "supply_chain_need", "legacy_eo_opportunity_pct"),
    pooled_spearman = c(sp(x$industrial_opportunity, cp), sp(x$industrial_opportunity, need), sp(x$industrial_opportunity, legacy)),
    within_node_median_spearman = c(within(cp), within(need), within(legacy)),
    n_cells = c(sum(!is.na(cp)), sum(!is.na(need)), sum(!is.na(legacy)))
  )
  hm <- if ("io_home_market_lift" %in% names(x)) x$io_home_market_lift %in% TRUE else x$io_home_market_scored %in% TRUE
  has_m <- !is.na(col("io_investment_momentum_pct"))
  weights <- data.frame(
    component = c("io_growth_pct (world demand growth)", "io_investment_momentum_pct (global capex trend)", "io_home_market (national, where it lifts the prize)"),
    share_of_prize = c(
      mean(ifelse(hm, 0.5, 1) * ifelse(has_m, 1 - momentum_weight, 1)),
      mean(ifelse(hm, 0.5, 1) * ifelse(has_m, momentum_weight, 0)),
      mean(ifelse(hm, 0.5, 0))
    ),
    cells_with_component = c(sum(!is.na(col("io_growth_pct"))), sum(!is.na(col("io_investment_momentum_pct"))), sum(hm)),
    country_varying = c(FALSE, FALSE, TRUE)
  )
  movers <- x
  movers$legacy_eo_opportunity_pct <- legacy
  movers$change <- movers$industrial_opportunity - legacy
  movers <- movers[!is.na(movers$change), , drop = FALSE]
  movers <- movers[order(-abs(movers$change)), intersect(c("Country", "iso3", "tech", "supply_chain", "legacy_eo_opportunity_pct",
                                                           "industrial_opportunity", "change", "io_node_prize", "io_home_market",
                                                           "competitive_position_percentile"), names(movers)), drop = FALSE]
  movers <- utils::head(movers, n_movers)
  disguise <- do.call(rbind, lapply(split(seq_len(nrow(x)), node), function(i) {
    data.frame(tech = x$tech[i[1]], supply_chain = x$supply_chain[i[1]], n_cells = length(i),
               io_node_prize = x$io_node_prize[i[1]], home_market_share = mean(hm[i]),
               prize_sd_within_node = if (length(i) > 1) stats::sd(x$industrial_opportunity[i]) else NA_real_,
               spearman_with_cp = sp(x$industrial_opportunity[i], cp[i]))
  }))
  list(correlations = correlations, weights = weights, movers = movers, disguise = disguise)
}
