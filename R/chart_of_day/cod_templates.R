# Chart of the Day templates (pure). Each template has
#   cod_cand_<template>(d, cfg)  -> data.frame of candidates: template, variable, measure, tech, stage, iso3, interest
#   cod_build_<template>(c, d, cfg) -> chart spec (list), see cod_spec()
# where `d` is cod_prepare() output and `c` one candidate row. Interest scores (about 0.5-2) favour charts with a
# clear story: a dominant supplier, a top-three rank, a stage that flips from surplus to deficit.

cod_excluded <- function(tech, stage, cfg) {
  ex <- cfg$exclude_cells
  if (!length(ex)) return(rep(FALSE, length(tech)))
  k <- paste(tech, stage, sep = "|")
  k %in% vapply(ex, function(e) paste(e$tech, e$stage, sep = "|"), "")
}

cod_cand <- function(template, variable, measure, tech, stage, iso3, interest, current = FALSE) {
  if (!length(iso3)) return(NULL)
  data.frame(template = template, variable = variable, measure = measure, tech = tech, stage = stage, iso3 = iso3,
             interest = interest, current = current, stringsAsFactors = FALSE)
}

cod_rank_interest <- function(rank) ifelse(rank <= 3, 2, ifelse(rank <= 10, 1.3, ifelse(rank <= 15, 0.7, 0)))

#' A chart spec. `chart$type` is one of bar, grouped_bar, lines, scatter, tiles; `rows` is a data frame.
cod_spec <- function(c, title, subtitle, chart, rows, notes, source, d) {
  list(schema = "osi-chart-of-day", version = 1L,
       key = paste(c$template, c$variable, c$measure, c$tech, c$stage, c$iso3, sep = ":"),
       template = c$template, variable = c$variable, measure = c$measure, tech = c$tech, stage = c$stage,
       focus = list(iso3 = c$iso3, name = cod_country_label(c$iso3, d$countries)),
       title = title, subtitle = subtitle, chart = chart, rows = rows, notes = notes, source = source)
}

# ---- leaderboard -------------------------------------------------------------------------------------------

COD_LEADER_VARS <- list(
  trade_exports_level = list(src = "trade", col = "exports", measure = "level", desc = FALSE),
  trade_exports_share = list(src = "trade", col = "share", measure = "share_of_world", desc = FALSE),
  trade_imports_level = list(src = "trade", col = "imports", measure = "level", desc = FALSE),
  trade_net_imports_gdp = list(src = "trade", col = "deficit_gdp", measure = "per_gdp", desc = FALSE),
  mineral_exports_share = list(src = "metric", measure = "share_of_world", desc = FALSE),
  mineral_production = list(src = "metric", measure = "level", desc = FALSE),
  mineral_reserves = list(src = "metric", measure = "level", desc = FALSE),
  investment_latest = list(src = "metric", measure = "level", desc = FALSE),
  lcoe_2025 = list(src = "metric", measure = "level", asc = TRUE))

cod_leader_table <- function(d, var) {
  v <- COD_LEADER_VARS[[var]]
  if (v$src == "trade") {
    t <- d$trade
    out <- data.frame(iso3 = t$iso3, tech = t$tech, stage = t$stage, value = t[[v$col]], stringsAsFactors = FALSE)
  } else {
    m <- d$metric[d$metric$var == var, ]
    out <- data.frame(iso3 = m$iso3, tech = m$tech, stage = m$stage, value = m$value, stringsAsFactors = FALSE)
  }
  out <- out[!is.na(out$value) & out$value > 0 & !out$iso3 %in% c("WLD", "EUU"), ]
  # rank within cell
  asc <- isTRUE(v$asc)
  out <- out[order(out$tech, out$stage, if (asc) out$value else -out$value), ]
  out$rank <- stats::ave(out$value, out$tech, out$stage, FUN = seq_along)
  out$n <- stats::ave(out$value, out$tech, out$stage, FUN = length)
  out
}

cod_cand_leaderboard <- function(d, cfg) {
  vars <- intersect(names(cfg$leaderboard_variables), names(COD_LEADER_VARS))
  out <- lapply(vars, function(var) {
    t <- cod_leader_table(d, var)
    t <- t[t$n >= cfg$thresholds$leaderboard_min_countries & !cod_excluded(t$tech, t$stage, cfg) &
             t$rank <= cfg$thresholds$leaderboard_max_rank, ]
    # storage has a levelised cost of storage, not of electricity: leave it out of the power-cost ranking
    if (var == "lcoe_2025") t <- t[t$tech != "Batteries", ]
    # a share of world exports under the floor makes a weak headline ("supplies 0.2% of ...")
    if (COD_LEADER_VARS[[var]]$measure == "share_of_world") t <- t[t$value >= cfg$thresholds$leaderboard_min_share, ]
    cod_cand("leaderboard", var, COD_LEADER_VARS[[var]]$measure, t$tech, t$stage, t$iso3, cod_rank_interest(t$rank),
             !(var %in% c("trade_exports_level", "trade_exports_share", "trade_imports_level", "trade_net_imports_gdp", "mineral_exports_share")))
  })
  do.call(rbind, out)
}

cod_mineral_phrase <- function(mineral, stage) {
  m <- tolower(mineral); if (mineral %in% c("PGMs")) m <- "platinum-group metals"
  switch(stage, Extraction = sprintf("%s ores and concentrates", m), Refining = sprintf("refined %s", m),
         Recovery = sprintf("%s scrap", m), m)
}

cod_build_leaderboard <- function(c, d, cfg) {
  var <- c$variable
  t <- cod_leader_table(d, var)
  t <- t[t$tech == c$tech & t$stage == c$stage, ]
  f <- t[t$iso3 == c$iso3, ]
  top <- utils::head(t, 10)
  rows <- if (f$rank > 10) rbind(top, f) else top
  who <- cod_country_name(c$iso3, d$countries)
  Who <- cod_cap1(who)
  ord <- cod_ordinal(f$rank)
  lg <- if (f$rank == 1) "largest" else paste0(ord, "-largest")
  yr <- d$trade_year
  m <- d$metric[d$metric$var == var & d$metric$tech == c$tech & d$metric$stage == c$stage, ]
  unit <- if (nrow(m)) m$unit[1] else ""
  year <- if (nrow(m)) m$year[1] else yr
  src <- if (nrow(m)) m$source[1] else "UN Comtrade; Harvard Atlas of Economic Complexity"
  fmt <- "usd"; notes <- ""
  if (var == "trade_exports_level") {
    cell <- cod_cell_phrase(c$tech, c$stage)
    title <- sprintf("%s is the world's %s exporter of %s", Who, lg, cell)
    sub <- sprintf("Exports of %s, %s, top 10 countries%s", cell, yr, if (f$rank > 10) sprintf(" and %s", who) else "")
  } else if (var == "trade_exports_share") {
    cell <- cod_cell_phrase(c$tech, c$stage); fmt <- "pct"
    title <- sprintf("%s supplies %s of world exports of %s", Who, cod_pct(f$value, if (f$value < 0.01) 1 else 0), cell)
    sub <- sprintf("Share of world exports of %s, %s; %s ranks %s of %d", cell, yr, who, ord, f$n)
  } else if (var == "trade_imports_level") {
    cell <- cod_cell_phrase(c$tech, c$stage)
    title <- sprintf("%s is the world's %s importer of %s", Who, lg, cell)
    sub <- sprintf("Imports of %s, %s, top 10 countries%s", cell, yr, if (f$rank > 10) sprintf(" and %s", who) else "")
  } else if (var == "trade_net_imports_gdp") {
    cell <- cod_cell_phrase(c$tech, c$stage); fmt <- "pct2"
    title <- sprintf("%s net imports of %s are worth %s of its GDP, the %s-highest share", cod_cap1(cod_poss(who)), cell,
                     cod_pct(f$value, 2), if (f$rank == 1) "" else ord)
    title <- sub("the -highest", "the highest", title, fixed = TRUE)
    sub <- sprintf("Imports minus exports of %s as a share of GDP, %s; countries that export more than they import are not shown", cell, yr)
    src <- "UN Comtrade; Harvard Atlas of Economic Complexity; World Bank WDI"
  } else if (var == "mineral_exports_share") {
    cell <- cod_mineral_phrase(c$tech, c$stage); fmt <- "pct"
    title <- sprintf("%s supplies %s of world exports of %s", Who, cod_pct(f$value, if (f$value < 0.01) 1 else 0), cell)
    sub <- sprintf("Share of world exports of %s, %s; %s ranks %s of %d", cell, year, who, ord, f$n)
  } else if (var == "mineral_production") {
    cell <- tolower(c$tech); fmt <- "num"
    title <- sprintf("%s is the world's %s %s producer", Who, lg, cell)
    sub <- sprintf("Mine production of %s, %s (%s)", cell, year, unit)
  } else if (var == "mineral_reserves") {
    cell <- tolower(c$tech); fmt <- "num"
    title <- sprintf("%s holds the world's %s %s reserves", Who, lg, cell)
    sub <- sprintf("Known %s reserves (%s); countries the source reports", cell, unit)
  } else if (var == "investment_latest") {
    cell <- cod_cell_phrase(c$tech, c$stage)
    title <- sprintf("%s invested %s in %s in %s, %s", Who, cod_usd(f$value), cell, year,
                     if (f$rank == 1) "the most of any country" else paste(ord, "in the world"))
    sub <- sprintf("Investment in %s by country, %s, 2024 dollars", cell, year)
    notes <- "Announced and operating projects tracked by the Clean Investment Monitor; manufacturing is one total for components and final assembly."
  } else if (var == "lcoe_2025") {
    cell <- unname(COD_TECH_OBJECT[c$tech]); if (is.na(cell) || !grepl("power$", cell)) cell <- paste(tolower(c$tech), "power")
    fmt <- "usd_mwh"
    title <- sprintf("New %s in %s costs $%s/MWh, the %s of %d countries", cell, who, round(f$value),
                     if (f$rank == 1) "lowest" else paste0(ord, "-lowest"), f$n)
    sub <- sprintf("Levelised cost of electricity from new %s plants, 2025, $/MWh (lower is cheaper)", sub(" power$", "", cell))
  }
  rows <- data.frame(label = paste0(rows$rank, ". ", cod_country_label(rows$iso3, d$countries)), iso3 = rows$iso3,
                     value = rows$value, highlight = rows$iso3 == c$iso3, stringsAsFactors = FALSE)
  if (f$rank > 11) notes <- trimws(paste(notes, sprintf("Countries ranked 11th to %s are not shown.", cod_ordinal(f$rank - 1))))
  cod_spec(c, title, sub, list(type = "bar", format = fmt, value_label = if (nzchar(unit) && fmt == "num") unit else ""), rows, notes, src, d)
}

# ---- suppliers / customers ---------------------------------------------------------------------------------
# Sources: d$partners (imports by supplier: each country's latest 12 months of its own monthly reports, 2024 where
# it does not report monthly) and d$exports (exports by buyer: own monthly reports, else the buyers' 2024 records).

cod_is_current <- function(period) !is.na(period) & period != "2024"

cod_side_table <- function(d, side) {
  if (side == "suppliers") {
    p <- d$partners
    data.frame(focus = p$reporter, other = p$partner, tech = p$tech, stage = p$stage, usd = p$usd, period = p$period,
               basis = "own", stringsAsFactors = FALSE)
  } else {
    e <- d$exports
    data.frame(focus = e$exporter, other = e$buyer, tech = e$tech, stage = e$stage, usd = e$usd, period = e$period,
               basis = e$basis, stringsAsFactors = FALSE)
  }
}

cod_partner_shares <- function(d, iso3, tech, stage, side = c("suppliers", "customers")) {
  side <- match.arg(side)
  p <- cod_side_table(d, side)
  p <- p[p$focus == iso3 & p$tech == tech & p$stage == stage, ]
  if (!nrow(p)) return(NULL)
  a <- stats::aggregate(usd ~ other, p, sum)
  a <- a[order(-a$usd), ]
  a$share <- a$usd / sum(a$usd)
  attr(a, "period") <- p$period[1]
  attr(a, "basis") <- p$basis[1]
  a
}

cod_cand_partners <- function(d, cfg, side) {
  p <- cod_side_table(d, side)
  p <- p[!cod_excluded(p$tech, p$stage, cfg), ]
  tot <- stats::aggregate(usd ~ focus + tech + stage + period, p, sum)
  mx <- stats::aggregate(usd ~ focus + tech + stage, p, max)
  tot$top_share <- mx$usd[match(paste(tot$focus, tot$tech, tot$stage), paste(mx$focus, mx$tech, mx$stage))] / tot$usd
  thr <- if (side == "suppliers") cfg$thresholds$suppliers_min_imports_usd else cfg$thresholds$customers_min_exports_usd
  tot <- tot[tot$usd >= thr, ]
  interest <- ifelse(tot$top_share >= 0.5, 2, ifelse(tot$top_share >= 0.3, 1.3, 0.8))
  cod_cand(side, if (side == "suppliers") "imports_by_partner" else "exports_by_partner", "share_of_total",
           tot$tech, tot$stage, tot$focus, interest, cod_is_current(tot$period))
}
cod_cand_suppliers <- function(d, cfg) cod_cand_partners(d, cfg, "suppliers")
cod_cand_customers <- function(d, cfg) cod_cand_partners(d, cfg, "customers")

cod_period_note <- function(period) {
  if (cod_is_current(period)) sprintf("Latest 12 months of the country's own monthly customs reports to UN Comtrade (%s).",
                                      sub("^12 months to ", "to ", period))
  else "Annual 2024 data: this country does not report monthly trade to UN Comtrade."
}

cod_build_partners <- function(c, d, cfg, side) {
  a <- cod_partner_shares(d, c$iso3, c$tech, c$stage, side)
  period <- attr(a, "period")
  top <- utils::head(a, 7)
  rest <- a[-seq_len(nrow(top)), ]
  who <- cod_country_name(c$iso3, d$countries)
  cell <- cod_cell_phrase(c$tech, c$stage)
  bloc <- d$countries$bloc[match(top$other, d$countries$iso3)]
  bloc[is.na(bloc)] <- "Nonaligned"
  rows <- data.frame(label = cod_country_label(top$other, d$countries), iso3 = top$other, value = top$share,
                     highlight = FALSE, group = bloc, stringsAsFactors = FALSE)
  if (nrow(rest)) rows <- rbind(rows, data.frame(label = sprintf("All others (%d)", nrow(rest)), iso3 = "", value = sum(rest$share),
                                                 highlight = FALSE, group = "Others", stringsAsFactors = FALSE))
  lead <- cod_country_name(top$other[1], d$countries)
  colour <- "Colour shows each country's alignment in UN General Assembly voting (U.S.-leaning, China-leaning, nonaligned)."
  if (side == "suppliers" && top$share[1] < 0.25) {
    title <- sprintf("No single country supplies more than %s of %s imports of %s", cod_pct(top$share[1]), cod_poss(who), cell)
  } else if (side == "suppliers") {
    title <- sprintf("%s supplies %s of %s imports of %s", cod_cap1(lead), cod_pct(top$share[1]), cod_poss(who), cell)
  } else {
    title <- sprintf("%s buys %s of %s exports of %s", cod_cap1(lead), cod_pct(top$share[1]), cod_poss(who), cell)
  }
  if (side == "suppliers") {
    sub <- sprintf("Where %s buys %s: share of its imports by supplier, %s (%s in total)", who, cell, period, cod_usd(sum(a$usd)))
    notes <- paste(cod_period_note(period), colour)
  } else {
    sub <- sprintf("Who buys %s %s: share of its exports by destination, %s (%s in total)", cod_poss(who), cell, period, cod_usd(sum(a$usd)))
    notes <- paste(if (identical(attr(a, "basis"), "mirror")) "Measured from the buyers' 2024 customs records." else cod_period_note(period), colour)
  }
  src <- if (cod_is_current(period)) "UN Comtrade monthly data (HS6 basket for each supply-chain step)" else "UN Comtrade (HS6 basket for each supply-chain step)"
  s <- cod_spec(c, title, sub, list(type = "bar", format = "pct", color_by = "group"), rows, notes, src, d)
  s$period <- period
  s
}
cod_build_suppliers <- function(c, d, cfg) cod_build_partners(c, d, cfg, "suppliers")
cod_build_customers <- function(c, d, cfg) cod_build_partners(c, d, cfg, "customers")

# ---- stage balance -----------------------------------------------------------------------------------------

cod_stage_rows <- function(d, iso3, tech, cfg) {
  t <- d$balance[d$balance$iso3 == iso3 & d$balance$tech == tech & d$balance$stage %in% COD_TRADE_STAGES, ]
  t$exports[is.na(t$exports)] <- 0; t$imports[is.na(t$imports)] <- 0
  t <- t[(t$exports + t$imports) >= cfg$thresholds$stage_min_cell_usd & !cod_excluded(t$tech, t$stage, cfg), ]
  t[order(match(t$stage, COD_TRADE_STAGES)), ]
}

cod_cand_stage_balance <- function(d, cfg) {
  t <- d$balance[d$balance$stage %in% COD_TRADE_STAGES & !cod_excluded(d$balance$tech, d$balance$stage, cfg), ]
  t$exports[is.na(t$exports)] <- 0; t$imports[is.na(t$imports)] <- 0
  t <- t[(t$exports + t$imports) >= cfg$thresholds$stage_min_cell_usd, ]
  t$sur <- as.numeric(t$exports > t$imports); t$n <- 1; t$gross <- t$exports + t$imports
  g <- stats::aggregate(cbind(n, gross, sur) ~ iso3 + tech + period, t, sum)
  g <- g[g$n >= 3 & g$gross >= cfg$thresholds$stage_min_total_usd, ]
  mixed <- g$sur > 0 & g$sur < g$n
  cod_cand("stage_balance", "exports_imports", "level", g$tech, "All", g$iso3, ifelse(mixed, 1.6, 1.0), cod_is_current(g$period))
}

cod_build_stage_balance <- function(c, d, cfg) {
  t <- cod_stage_rows(d, c$iso3, c$tech, cfg)
  period <- t$period[1]
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  net <- t$exports - t$imports
  noun <- if (!is.na(COD_TECH_NOUN[c$tech])) COD_TECH_NOUN[[c$tech]] else tolower(c$tech)
  if (any(net > 0) && any(net < 0)) {
    s <- which.max(net); dd <- which.min(net)
    title <- sprintf("%s runs a %s surplus in %s but a %s deficit in %s", Who, cod_usd(net[s]), cod_cell_phrase(c$tech, t$stage[s]),
                     cod_usd(-net[dd]), cod_cell_phrase(c$tech, t$stage[dd]))
  } else if (all(net < 0)) {
    title <- sprintf("%s imports more than it exports at every traded step of the %s supply chain", Who, noun)
  } else {
    title <- sprintf("%s exports more than it imports at every traded step of the %s supply chain", Who, noun)
  }
  rows <- rbind(data.frame(category = cod_stage_label(t$stage), stage = t$stage, series = "Exports", value = t$exports, stringsAsFactors = FALSE),
                data.frame(category = cod_stage_label(t$stage), stage = t$stage, series = "Imports", value = t$imports, stringsAsFactors = FALSE))
  sub <- sprintf("%s exports and imports at each step of the %s supply chain, %s", cod_poss(Who), noun, period)
  s <- cod_spec(c, title, sub, list(type = "grouped_bar", format = "usd", series = I(c("Exports", "Imports"))), rows,
                paste(sprintf("Steps with less than %s of trade are left out. Each step is a basket of HS6 product codes.",
                              cod_usd(cfg$thresholds$stage_min_cell_usd)), cod_period_note(period)),
                if (cod_is_current(period)) "UN Comtrade monthly data" else "UN Comtrade; Harvard Atlas of Economic Complexity", d)
  s$period <- period
  s
}

# ---- China share tiles -------------------------------------------------------------------------------------

#' China's share of each country's imports by tech x stage, from a partners table (reporter, partner, ..., usd, period).
cod_china_table <- function(p) {
  p <- p[p$stage %in% COD_TRADE_STAGES, ]
  if (!nrow(p)) return(data.frame(iso3 = character(), tech = character(), stage = character(), imports_usd = numeric(),
                                  china_share = numeric(), period = character()))
  tot <- stats::aggregate(usd ~ reporter + tech + stage + period, p, sum)
  chn <- p[p$partner == "CHN", ]
  cv <- chn$usd[match(paste(tot$reporter, tot$tech, tot$stage), paste(chn$reporter, chn$tech, chn$stage))]
  data.frame(iso3 = tot$reporter, tech = tot$tech, stage = tot$stage, imports_usd = tot$usd, china_share = ifelse(is.na(cv), 0, cv) / tot$usd,
             period = tot$period, stringsAsFactors = FALSE)
}

cod_china_cells <- function(d, iso3, cfg) {
  e <- cod_china_table(d$partners[d$partners$reporter == iso3, ])
  e[e$imports_usd >= cfg$thresholds$china_share_min_cell_usd & !cod_excluded(e$tech, e$stage, cfg), ]
}

cod_cand_china_share <- function(d, cfg) {
  e <- cod_china_table(d$partners)
  e <- e[e$imports_usd >= cfg$thresholds$china_share_min_cell_usd & !cod_excluded(e$tech, e$stage, cfg), ]
  # one candidate per headline cell (a big import with China at 25%+), so technology weights and cooldowns vary the
  # story told over the same grid
  n <- stats::aggregate(china_share ~ iso3, e, length)
  ok <- n$iso3[n$china_share >= cfg$thresholds$china_share_min_cells]
  big <- e[e$iso3 %in% ok & e$imports_usd >= cfg$thresholds$china_share_headline_usd & e$china_share >= 0.25, ]
  cod_cand("china_share", "china_import_share", "share_of_total", big$tech, big$stage, big$iso3,
           ifelse(big$china_share >= 0.6, 2, ifelse(big$china_share >= 0.4, 1.4, 0.8)), cod_is_current(big$period))
}

cod_build_china_share <- function(c, d, cfg) {
  e <- cod_china_cells(d, c$iso3, cfg)
  period <- e$period[1]
  who <- cod_country_name(c$iso3, d$countries)
  m <- e[e$tech == c$tech & e$stage == c$stage, ]
  title <- sprintf("%s buys %s of its imports of %s from China", cod_cap1(who), cod_pct(m$china_share), cod_cell_phrase(m$tech, m$stage))
  techs <- names(sort(tapply(e$imports_usd, e$tech, sum), decreasing = TRUE))
  rows <- data.frame(row = e$tech, col = cod_stage_label(e$stage), stage = e$stage, value = e$china_share, imports = e$imports_usd,
                     stringsAsFactors = FALSE)
  rows <- rows[order(match(rows$row, techs), match(rows$stage, COD_TRADE_STAGES)), ]
  sub <- sprintf("Share of %s imports that come from China, by technology and supply-chain step, %s", cod_poss(who), period)
  s <- cod_spec(c, title, sub, list(type = "tiles", format = "pct", rows_order = I(techs),
                     cols_order = I(cod_stage_label(intersect(COD_TRADE_STAGES, e$stage)))),
                rows, paste(sprintf("Blank cells: imports below %s. China excludes Hong Kong and Macao. Each step is a basket of HS6 product codes.",
                                    cod_usd(cfg$thresholds$china_share_min_cell_usd)), cod_period_note(period)),
                if (cod_is_current(period)) "UN Comtrade monthly data" else "UN Comtrade", d)
  s$period <- period
  s
}

# ---- China share shift (2024 -> latest 12 months) ----------------------------------------------------------

cod_china_shift_table <- function(d, cfg) {
  now <- cod_china_table(d$partners[cod_is_current(d$partners$period), ])
  then <- cod_china_table(d$partners_2024[d$partners_2024$reporter %in% now$iso3, ])
  k <- paste(now$iso3, now$tech, now$stage)
  now$share_2024 <- then$china_share[match(k, paste(then$iso3, then$tech, then$stage))]
  now$imports_2024 <- then$imports_usd[match(k, paste(then$iso3, then$tech, then$stage))]
  thr <- cfg$thresholds$china_shift_min_usd
  now <- now[!is.na(now$share_2024) & now$imports_usd >= thr & now$imports_2024 >= thr & !cod_excluded(now$tech, now$stage, cfg), ]
  now$change <- now$china_share - now$share_2024
  now
}

cod_cand_china_shift <- function(d, cfg) {
  if (!isTRUE(d$has_current)) return(NULL)
  t <- cod_china_shift_table(d, cfg)
  t <- t[abs(t$change) >= cfg$thresholds$china_shift_min_points, ]
  if (!nrow(t)) return(NULL)
  n <- stats::aggregate(change ~ iso3, t, length)
  mx <- stats::aggregate(change ~ iso3, t, function(x) max(abs(x)))
  g <- n[n$change >= 3, ]
  m <- mx$change[match(g$iso3, mx$iso3)]
  cod_cand("china_shift", "china_import_share_change", "change", "All", "All", g$iso3, ifelse(m >= 0.25, 2, 1.3), TRUE)
}

cod_build_china_shift <- function(c, d, cfg) {
  t <- cod_china_shift_table(d, cfg)
  t <- t[t$iso3 == c$iso3 & abs(t$change) >= cfg$thresholds$china_shift_min_points, ]
  t <- utils::head(t[order(-abs(t$change)), ], 8)
  period <- t$period[1]
  who <- cod_country_name(c$iso3, d$countries)
  m <- t[1, ]
  cell <- cod_cell_phrase(m$tech, m$stage)
  title <- sprintf("China's share of %s imports of %s %s from %s to %s", cod_poss(who), cell,
                   if (m$change > 0) "rose" else "fell", cod_pct(m$share_2024), cod_pct(m$china_share))
  rows <- data.frame(label = cod_cap1(vapply(seq_len(nrow(t)), function(i) cod_cell_phrase(t$tech[i], t$stage[i]), "")),
                     from = t$share_2024, to = t$china_share, tech = t$tech, stage = t$stage, imports = t$imports_usd,
                     stringsAsFactors = FALSE)
  sub <- sprintf("Share of %s imports that come from China: 2024 vs the latest %s; the steps that moved most", cod_poss(who), period)
  s <- cod_spec(c, title, sub, list(type = "dumbbell", format = "pct", from_label = "2024", to_label = sub("^12 months to ", "", period)), rows,
                paste(sprintf("Steps with at least %s of imports in both periods and a move of %s points or more.",
                              cod_usd(cfg$thresholds$china_shift_min_usd), round(100 * cfg$thresholds$china_shift_min_points)),
                      cod_period_note(period), "China excludes Hong Kong and Macao."),
                "UN Comtrade (annual 2024 and monthly data)", d)
  s$period <- period
  s
}

# ---- investment trend --------------------------------------------------------------------------------------

cod_cand_investment_trend <- function(d, cfg) {
  iv <- d$invest[d$invest$usd > 0, ]
  iv$yrs <- 1
  g <- stats::aggregate(cbind(usd, yrs) ~ iso3 + tech + stage, iv, sum)
  names(g)[names(g) == "usd"] <- "tot"
  g <- g[g$tot / 1e9 >= cfg$thresholds$investment_min_total_bn & g$yrs >= cfg$thresholds$investment_min_years, ]
  cod_cand("investment_trend", "annual_investment", "level", g$tech, g$stage, g$iso3, 1, TRUE)
}

cod_build_investment_trend <- function(c, d, cfg) {
  iv <- d$invest[d$invest$tech == c$tech & d$invest$stage == c$stage, ]
  s <- data.frame(iso3 = iv$iso3, x = iv$year, value = iv$usd, stringsAsFactors = FALSE)
  peers <- cod_pick_peers(c$iso3, cod_series_size(s), cfg, n = (if (is.null(cfg$peers$n)) 3 else cfg$peers$n) + 2)
  base_year <- if (is.null(cfg$peers$base_year$investment)) 2020L else cfg$peers$base_year$investment
  tr <- cod_trend_rows(s, c$iso3, peers, base_year, d, cfg)
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  cell <- cod_cell_phrase(c$tech, c$stage)
  f <- tr$rows[tr$rows$iso3 == c$iso3, ]; f <- f[order(f$x), ]
  last <- f[nrow(f), ]
  shown <- setdiff(tr$shown, c$iso3)
  if (tr$indexed) {
    title <- cod_index_title(sprintf("%s investment in %s", cod_poss(Who), cell), c$iso3, tr, d)
    sub <- sprintf("Annual investment in %s, index %s = 100: %s, neighbours and comparable countries", cell, tr$base, who)
    notes <- sprintf("Indexed to %s = 100 so countries of different size can be compared; %s invested %s in %s. Investment in 2024 dollars.",
                     tr$base, who, cod_usd(last$level), last$x)
  } else {
    first <- f[which(f$level > 0)[1], ]; peak <- f[which.max(f$level), ]
    title <- if (peak$x < last$x && last$level < 0.7 * peak$level)
      sprintf("%s investment in %s peaked at %s in %s and was %s in %s", cod_poss(Who), cell, cod_usd(peak$level), peak$x, cod_usd(last$level), last$x)
    else sprintf("%s investment in %s %s from %s in %s to %s in %s", cod_poss(Who), cell, if (last$level >= first$level) "grew" else "fell",
                 cod_usd(first$level), first$x, cod_usd(last$level), last$x)
    sub <- sprintf("Annual investment in %s, %s-%s: %s, neighbours and comparable countries", cell, min(f$x), last$x, who)
    notes <- "Annual investment in 2024 dollars."
  }
  notes <- paste(notes, sprintf("Compared with %s: neighbours and countries of similar size in this market.", cod_and(cod_country_label(shown, d$countries))))
  s <- cod_spec(c, title, sub, list(type = "lines", format = if (tr$indexed) "num" else "usd",
                                    y_label = if (tr$indexed) sprintf("Index, %s = 100", tr$base) else "Investment, 2024 dollars",
                                    level_format = "usd"), tr$rows, notes, "Rhodium Group and MIT CEEPR, Clean Investment Monitor", d)
  s$period <- paste0(min(tr$rows$x), "-", last$x)
  s
}

# ---- trade map (scatter) -----------------------------------------------------------------------------------

cod_cand_trade_map <- function(d, cfg) {
  t <- d$trade[d$trade$stage %in% COD_TRADE_STAGES & !is.na(d$trade$exports) & !is.na(d$trade$imports) &
                 d$trade$exports > 1e6 & d$trade$imports > 1e6 & !cod_excluded(d$trade$tech, d$trade$stage, cfg), ]
  n <- stats::ave(t$exports, t$tech, t$stage, FUN = length)
  t <- t[n >= cfg$thresholds$trade_map_min_countries & (t$exports + t$imports) >= 1e9, ]
  cod_cand("trade_map", "exports_vs_imports", "level", t$tech, t$stage, t$iso3, 1)
}

cod_build_trade_map <- function(c, d, cfg) {
  t <- d$trade[d$trade$tech == c$tech & d$trade$stage == c$stage & !is.na(d$trade$exports) & !is.na(d$trade$imports) &
                 d$trade$exports > 1e6 & d$trade$imports > 1e6, ]
  f <- t[t$iso3 == c$iso3, ]
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  cell <- cod_cell_phrase(c$tech, c$stage)
  lab <- unique(c(c$iso3, utils::head(t$iso3[order(-t$exports)], 4), utils::head(t$iso3[order(-t$imports)], 3)))
  ratio <- f$exports / f$imports
  title <- if (ratio >= 1.5) sprintf("%s exports %s of %s and imports %s: a net seller", Who, cod_usd(f$exports), cell, cod_usd(f$imports))
           else if (ratio <= 1 / 1.5) sprintf("%s imports %s of %s and exports %s: a net buyer", Who, cod_usd(f$imports), cell, cod_usd(f$exports))
           else sprintf("%s trades %s of %s in both directions", Who, cod_usd((f$imports + f$exports) / 2), cell)
  rows <- data.frame(label = cod_country_label(t$iso3, d$countries), iso3 = t$iso3, x = t$imports, y = t$exports,
                     highlight = t$iso3 == c$iso3, show_label = t$iso3 %in% lab, stringsAsFactors = FALSE)
  sub <- sprintf("Exports and imports of %s by country, %s; above the diagonal = exports more than it imports", cell, d$trade_year)
  cod_spec(c, title, sub, list(type = "scatter", format = "usd", x_label = "Imports", y_label = "Exports", log = TRUE), rows,
           "Log scales. Countries with under $1m of exports or imports are left out.", "UN Comtrade; Harvard Atlas of Economic Complexity", d)
}

COD_TEMPLATES <- c("suppliers", "customers", "china_share", "china_shift", "leaderboard", "stage_balance", "investment_trend", "trade_map")
