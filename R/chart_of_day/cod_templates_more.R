# Chart of the Day templates on non-trade data (pure): Energy Institute (power mix, production and generation
# trends), Clean Investment Monitor (investment by sector, new power capacity), the Atlas of Economic Complexity
# (share of world exports over time), GTA industrial-policy measures (what a country uses; who is most active), and
# clean-tech factory FDI (who builds where). Same contract as cod_templates.R.

COD_FUEL_COL <- c(Coal = "#6b4f3a", Gas = "#9ca3af", Oil = "#4b5563", Nuclear = "#c084fc", Hydro = "#1889b0",
                  Wind = "#55c4c5", Solar = "#f5b829", `Other renewables` = "#8b9d17", Other = "#374151")
COD_SECTOR_COL <- c(Manufacturing = "#f58229", `Electric Power` = "#1889b0", Transport = "#55c4c5", Industry = "#8b9d17")
COD_POWER_COL <- c(Solar = "#f5b829", Wind = "#55c4c5", `Nuclear Power` = "#c084fc", `Advanced Nuclear` = "#a855f7",
                   `Other Renewables` = "#8b9d17", `Other Renewables (Hydro/Geo/Bio)` = "#8b9d17", `Stationary Battery Storage` = "#f58229",
                   `Solar PV` = "#f5b829", `Enhanced Geothermal` = "#b45309")
COD_POLICY_COL <- c(`Support` = "#8b9d17", `Protection` = "#e2574c")

COD_FAMILY_LABEL <- c(grant = "Grants", public_finance = "State loans and guarantees", export_support = "Export finance",
                      tax_incentive = "Tax breaks", state_aid = "Other state aid", production_subsidy = "Production subsidies",
                      price_support = "Price support", localisation_incentive = "Local-content incentives", fdi_incentive = "Investment incentives",
                      import_support = "Import support", localisation_unspecified = "Local-content measures",
                      tariff = "Tariffs", trade_defence = "Anti-dumping and anti-subsidy duties", procurement_preference = "Buy-national procurement",
                      export_restriction = "Export restrictions", import_restriction = "Import restrictions", investment_control = "Investment screening",
                      fdi_restriction = "Foreign-investment limits", localisation_requirement = "Local-content requirements", other = "Other")

#' A series-by-year spec row set: data frame (x, series, value).
cod_series_rows <- function(x, series, value) data.frame(x = as.integer(x), series = series, value = value, stringsAsFactors = FALSE)

# ---- EI: power mix -----------------------------------------------------------------------------------------

cod_ei_mix_table <- function(d, iso3) {
  e <- d$ei[d$ei$iso3 == iso3, ]
  v <- function(var) { s <- e[e$var == var, ]; stats::setNames(s$value, s$year) }
  years <- sort(unique(e$year[e$var == "electbyfuel_total" & e$value > 0]))
  get <- function(var) { x <- v(var); out <- x[as.character(years)]; out[is.na(out)] <- 0; unname(out) }
  tot <- get("electbyfuel_total")
  parts <- list(Coal = get("electbyfuel_coal"), Gas = get("electbyfuel_gas"), Oil = get("electbyfuel_oil"),
                Nuclear = get("electbyfuel_nuclear"), Hydro = get("electbyfuel_hydro"), Wind = get("wind_twh"),
                Solar = get("solar_twh"), `Other renewables` = get("biogeo_twh"), Other = get("electbyfuel_other"))
  data.frame(year = years, total = tot, as.data.frame(parts, check.names = FALSE), check.names = FALSE)
}

cod_cand_ei_power_mix <- function(d, cfg) {
  if (is.null(d$ei)) return(NULL)
  e <- d$ei[d$ei$var %in% c("electbyfuel_total", "wind_twh", "solar_twh") & d$ei$year %in% c(2015, 2025) & d$ei$iso3 != "WLD", ]
  w <- stats::reshape(stats::aggregate(value ~ iso3 + year + var, e, sum), idvar = c("iso3", "year"), timevar = "var", direction = "wide")
  names(w) <- sub("^value\\.", "", names(w))
  w$ws <- (ifelse(is.na(w$wind_twh), 0, w$wind_twh) + ifelse(is.na(w$solar_twh), 0, w$solar_twh)) / w$electbyfuel_total
  a <- w[w$year == 2025 & w$electbyfuel_total >= 20, ]
  b <- w[w$year == 2015, ]
  a$chg <- a$ws - b$ws[match(a$iso3, b$iso3)]
  a <- a[!is.na(a$chg), ]
  cod_cand("ei_power_mix", "electricity_by_source", "share_of_total", "All", "Deployment", a$iso3,
           ifelse(a$chg >= 0.10, 2, ifelse(a$chg >= 0.05, 1.4, 0.9)), TRUE)
}

cod_build_ei_power_mix <- function(c, d, cfg) {
  t <- cod_ei_mix_table(d, c$iso3)
  t <- t[t$year >= 2005, ]
  src <- setdiff(names(COD_FUEL_COL), character())
  rows <- do.call(rbind, lapply(src, function(s) cod_series_rows(t$year, s, t[[s]] / t$total)))
  rows <- rows[rows$value > 0, ]
  keep <- stats::aggregate(value ~ series, rows, max); keep <- intersect(names(COD_FUEL_COL), keep$series[keep$value >= 0.005])
  rows <- rows[rows$series %in% keep, ]
  who <- cod_country_name(c$iso3, d$countries)
  ws <- function(y) { r <- t[t$year == y, ]; (r$Wind + r$Solar) / r$total }
  last <- max(t$year)
  w25 <- ws(last); w15 <- ws(2015)
  r <- t[t$year == last, ]
  big <- names(which.max(unlist(r[src])))
  title <- if (w25 - w15 >= 0.03) sprintf("Wind and solar made %s of %s electricity in %s, up from %s in 2015", cod_pct(w25), cod_poss(who), last, cod_pct(w15))
           else sprintf("%s made %s of %s electricity in %s", big, cod_pct(r[[big]] / r$total), cod_poss(who), last)
  s <- cod_spec(c, title, sprintf("Share of %s electricity generation by source, 2005-%s", cod_poss(who), last),
                list(type = "stacked_bar", format = "pct", y_label = "Share of generation", colors = as.list(COD_FUEL_COL[keep]),
                     series = I(keep)), rows,
                "Gross generation. Other renewables: bioenergy and geothermal. Other: pumped hydro, non-renewable waste and other sources.",
                "Energy Institute, Statistical Review of World Energy 2026", d)
  s$period <- as.character(last)
  s
}

# ---- EI: production and generation trends ------------------------------------------------------------------

COD_EI_TREND <- list(
  solar_twh = list(label = "solar generation", unit = "TWh", tech = "Solar"),
  wind_twh = list(label = "wind generation", unit = "TWh", tech = "Wind"),
  nuclear_twh = list(label = "nuclear generation", unit = "TWh", tech = "Nuclear"),
  lithium_kt = list(label = "lithium mine output", unit = "thousand tonnes", tech = "Lithium"),
  cobalt_kt = list(label = "cobalt mine output", unit = "thousand tonnes", tech = "Cobalt"),
  graphite_kt = list(label = "graphite mine output", unit = "thousand tonnes", tech = "Graphite"),
  rareearths_kt = list(label = "rare-earth mine output", unit = "thousand tonnes", tech = "Rare Earths"),
  coalprod_mt = list(label = "coal production", unit = "million tonnes", tech = "Coal"),
  gasprod_bcm = list(label = "gas production", unit = "billion cubic metres", tech = "Gas"),
  oilprod_kbd = list(label = "oil production", unit = "thousand barrels a day", tech = "Oil"),
  co2_mtco2 = list(label = "CO2 emissions from energy", unit = "million tonnes CO2", tech = "All"))

cod_cand_ei_trend <- function(d, cfg) {
  if (is.null(d$ei)) return(NULL)
  out <- lapply(names(COD_EI_TREND), function(v) {
    e <- d$ei[d$ei$var == v, ]
    last <- max(e$year)
    world <- e$value[e$iso3 == "WLD" & e$year == last]
    e <- e[e$iso3 != "WLD", ]
    a <- e[e$year == last & e$value > 0, ]
    if (!length(world)) world <- sum(a$value)
    b <- e[e$year == last - 10, ]
    a$then <- b$value[match(a$iso3, b$iso3)]
    a$ratio <- a$value / a$then
    a <- a[a$value / world >= 0.01 & !is.na(a$then) & a$then > 0, ]
    rk <- rank(-a$value)
    # growth from a tiny base (2015 under 5% of today) is not a story in itself: no bonus for the ratio
    tiny <- a$then < 0.05 * a$value
    int <- ifelse(!tiny & (a$ratio >= 3 | a$ratio <= 0.5), 2, ifelse(rk <= 5, 1.4, 0.9))
    cod_cand("ei_trend", v, "level", COD_EI_TREND[[v]]$tech, "All", a$iso3, int, TRUE)
  })
  do.call(rbind, out)
}

cod_build_ei_trend <- function(c, d, cfg) {
  v <- COD_EI_TREND[[c$variable]]
  e <- d$ei[d$ei$var == c$variable & d$ei$year >= 2005, ]
  last <- max(e$year)
  world <- e$value[e$iso3 == "WLD" & e$year == last]
  e <- e[e$iso3 != "WLD", ]
  if (!length(world)) world <- sum(e$value[e$year == last])
  s <- data.frame(iso3 = e$iso3, x = e$year, value = e$value, stringsAsFactors = FALSE)
  peers <- cod_pick_peers(c$iso3, cod_series_size(s), cfg, n = (if (is.null(cfg$peers$n)) 3 else cfg$peers$n) + 2)
  base_year <- if (is.null(cfg$peers$base_year$ei)) 2015L else cfg$peers$base_year$ei
  tr <- cod_trend_rows(s, c$iso3, peers, base_year, d, cfg)
  f <- s[s$iso3 == c$iso3, ]; now <- f$value[f$x == last]; then <- f$value[f$x == last - 10]
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  share <- now / world
  num <- function(z) if (z >= 100) format(round(z), big.mark = ",") else format(signif(z, 2))
  shown <- setdiff(tr$shown, c$iso3)
  if (tr$indexed) {
    title <- cod_index_title(sprintf("%s %s", cod_poss(Who), v$label), c$iso3, tr, d)
    sub <- sprintf("%s, index %s = 100: %s, neighbours and comparable countries", cod_cap1(v$label), tr$base, who)
    notes <- sprintf("Indexed to %s = 100 so countries of different size can be compared. In %s %s produced %s %s, %s of the world total.",
                     tr$base, last, who, num(now), v$unit, cod_pct(share, if (share < 0.01) 1 else 0))
  } else {
    r <- now / then
    move <- if (then < 0.05 * now) sprintf("rose from almost nothing (%s %s)", num(then), v$unit)
            else if (r >= 2) sprintf("grew %s-fold", if (r >= 10) round(r) else format(round(r, 1)))
            else if (r >= 1.1) sprintf("grew %s%%", round(100 * (r - 1)))
            else if (r <= 0.9) sprintf("fell %s%%", round(100 * (1 - r)))
            else "held steady"
    title <- sprintf("%s %s %s between %s and %s, to %s %s (%s of the world total)", cod_poss(Who), v$label, move, last - 10, last,
                     num(now), v$unit, cod_pct(share, if (share < 0.01) 1 else 0))
    sub <- sprintf("%s, %s, %s-%s: %s, neighbours and comparable countries", cod_cap1(v$label), v$unit, min(tr$rows$x), last, who)
    notes <- "Share of the world total as the Statistical Review reports it."
  }
  notes <- paste(notes, sprintf("Compared with %s: neighbours and countries of similar size.", cod_and(cod_country_label(shown, d$countries))))
  s <- cod_spec(c, title, sub, list(type = "lines", format = "num",
                                    y_label = if (tr$indexed) sprintf("Index, %s = 100", tr$base) else cod_cap1(v$unit),
                                    level_format = "num", level_unit = v$unit), tr$rows, notes,
                "Energy Institute, Statistical Review of World Energy 2026", d)
  s$period <- paste0(min(tr$rows$x), "-", last)
  s
}

# ---- GCIM: clean investment by sector; new power capacity ----------------------------------------------------

cod_cand_gcim_mix <- function(d, cfg) {
  if (is.null(d$gcim_invest)) return(NULL)
  g <- d$gcim_invest
  last <- max(g$year)
  tot <- stats::aggregate(usd ~ iso3 + year, g, sum)
  a <- tot[tot$year == last & tot$usd >= 1e9, ]
  b <- tot[tot$year == last - 3, ]
  a$r <- a$usd / b$usd[match(a$iso3, b$iso3)]
  cod_cand("gcim_mix", "clean_investment_by_sector", "level", "All", "All", a$iso3,
           ifelse(!is.na(a$r) & (a$r >= 2 | a$r <= 0.6), 1.8, 1), TRUE)
}

cod_build_gcim_mix <- function(c, d, cfg) {
  g <- d$gcim_invest[d$gcim_invest$iso3 == c$iso3, ]
  a <- stats::aggregate(usd ~ sector + year, g, sum)
  last <- max(d$gcim_invest$year)
  rows <- cod_series_rows(a$year, a$sector, a$usd)
  ser <- intersect(names(COD_SECTOR_COL), unique(rows$series))
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  tl <- sum(a$usd[a$year == last]); t0 <- sum(a$usd[a$year == min(a$year)])
  lead <- a[a$year == last, ]; lead <- lead[which.max(lead$usd), ]
  title <- sprintf("%s invested %s in clean technology in %s, %s on %s", Who, cod_usd(tl), last, cod_pct(lead$usd / tl),
                   c(Manufacturing = "factories", `Electric Power` = "clean power", Transport = "zero-emission vehicles", Industry = "clean industry")[[lead$sector]])
  s <- cod_spec(c, title, sprintf("%s clean-technology investment by sector, %s-%s, 2024 dollars", cod_poss(Who), min(a$year), last),
                list(type = "stacked_bar", format = "usd", y_label = "Investment, 2024 dollars", colors = as.list(COD_SECTOR_COL[ser]), series = I(ser)),
                rows, "Actual capital spending on projects operating or under construction, spread over construction. Transport: purchases of zero-emission vehicles.",
                "Rhodium Group and MIT CEEPR, Clean Investment Monitor", d)
  s$period <- as.character(last)
  s
}

cod_cand_gcim_power <- function(d, cfg) {
  if (is.null(d$gcim_power)) return(NULL)
  p <- d$gcim_power
  last <- max(p$year)
  tot <- stats::aggregate(gw ~ iso3 + year, p, sum)
  a <- tot[tot$year == last & tot$gw >= 1, ]
  cod_cand("gcim_power", "clean_power_additions", "level", "All", "Deployment", a$iso3, 1.2, TRUE)
}

cod_build_gcim_power <- function(c, d, cfg) {
  p <- d$gcim_power[d$gcim_power$iso3 == c$iso3, ]
  last <- max(d$gcim_power$year)
  p$technology <- sub("^Other Renewables.*", "Other renewables", p$technology)
  p$technology[p$technology == "Solar PV"] <- "Solar"
  a <- stats::aggregate(gw ~ technology + year, p, sum)
  cols <- c(Solar = "#f5b829", Wind = "#55c4c5", `Nuclear Power` = "#c084fc", `Advanced Nuclear` = "#a855f7",
            `Other renewables` = "#8b9d17", `Stationary Battery Storage` = "#f58229", `Enhanced Geothermal` = "#b45309")
  ser <- intersect(names(cols), unique(a$technology))
  ser <- c(ser, setdiff(unique(a$technology), ser))
  cols <- c(cols, stats::setNames(rep("#6b7280", length(setdiff(ser, names(cols)))), setdiff(ser, names(cols))))
  rows <- cod_series_rows(a$year, a$technology, a$gw)
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  l <- a[a$year == last, ]; tl <- sum(l$gw); top <- l[which.max(l$gw), ]
  title <- sprintf("%s was %s of the %s GW of clean power %s added in %s", sub("Nuclear Power", "Nuclear", top$technology),
                   cod_pct(top$gw / tl), format(signif(tl, if (tl < 10) 2 else 3)), who, last)
  s <- cod_spec(c, title, sprintf("Clean power capacity added in %s by technology, GW, %s-%s", who, min(a$year), last),
                list(type = "stacked_bar", format = "num", y_label = "GW added", colors = as.list(cols[ser]), series = I(ser)), rows,
                "Capacity coming online each year; storage in GW of power.", "Rhodium Group and MIT CEEPR, Clean Investment Monitor", d)
  s$period <- as.character(last)
  s
}

# ---- Atlas: share of world exports over time -----------------------------------------------------------------

cod_cand_export_share_trend <- function(d, cfg) {
  if (is.null(d$atlas)) return(NULL)
  a <- d$atlas[d$atlas$stage %in% COD_TRADE_STAGES & !cod_excluded(d$atlas$tech, d$atlas$stage, cfg), ]
  last <- max(a$year)
  n <- a[a$year == last, ]; o <- a[a$year == last - 10, ]
  n$then <- o$share[match(paste(n$iso3, n$tech, n$stage), paste(o$iso3, o$tech, o$stage))]
  # a real move: at least 2 points and at least a quarter of the earlier share
  n <- n[!is.na(n$then) & pmax(n$share, n$then) >= 0.02 & abs(n$share - n$then) >= 0.02 &
           abs(n$share - n$then) >= 0.25 * pmax(n$then, 0.001), ]
  cod_cand("export_share_trend", "world_export_share", "share_of_world", n$tech, n$stage, n$iso3,
           ifelse(abs(n$share - n$then) >= 0.05, 2, 1.2), TRUE)
}

cod_build_export_share_trend <- function(c, d, cfg) {
  a <- d$atlas[d$atlas$tech == c$tech & d$atlas$stage == c$stage & d$atlas$year >= 2005, ]
  last <- max(a$year)
  s <- data.frame(iso3 = a$iso3, x = a$year, value = a$share, stringsAsFactors = FALSE)
  peers <- cod_pick_peers(c$iso3, cod_series_size(s), cfg, n = (if (is.null(cfg$peers$n)) 3 else cfg$peers$n) + 2)
  # shares are already comparable across countries: never indexed
  cfg_n <- cfg; cfg_n$peers$index_ratio <- Inf
  tr <- cod_trend_rows(s, c$iso3, peers, 2005L, d, cfg_n)
  f <- s[s$iso3 == c$iso3, ]; now <- f$value[f$x == last]; then <- f$value[f$x == last - 10]
  who <- cod_country_name(c$iso3, d$countries)
  cell <- cod_cell_phrase(c$tech, c$stage)
  shown <- setdiff(tr$shown, c$iso3)
  title <- sprintf("%s share of world exports of %s %s from %s in %s to %s in %s", cod_cap1(cod_poss(who)), cell,
                   if (now > then) "rose" else "fell", cod_pct(then), last - 10, cod_pct(now), last)
  s <- cod_spec(c, title, sprintf("Share of world exports of %s, %s-%s: %s, neighbours and comparable exporters", cell, min(tr$rows$x), last, who),
                list(type = "lines", format = "pct", y_label = "Share of world exports"), tr$rows,
                paste(sprintf("Compared with %s: neighbours and exporters of similar size.", cod_and(cod_country_label(shown, d$countries))),
                      "Exports as reported in the Atlas of Economic Complexity (HS 1992 codes, so products created after 1992 are missing from the basket). Re-exports through hubs such as Hong Kong are included."),
                "Harvard Growth Lab, Atlas of Economic Complexity", d)
  s$period <- paste0(min(tr$rows$x), "-", last)
  s
}

# ---- Policy: what a country uses; who is most active -----------------------------------------------------------

COD_POLICY_SINCE <- 2020L

cod_cand_policy_mix <- function(d, cfg) {
  if (is.null(d$nipo)) return(NULL)
  n <- unique(d$nipo[d$nipo$year >= COD_POLICY_SINCE, c("intervention_id", "iso3", "tech")])
  g <- stats::aggregate(intervention_id ~ iso3 + tech, n, length)
  g <- g[g$intervention_id >= 12, ]
  cod_cand("policy_mix", "policy_measures_by_type", "count", g$tech, "All", g$iso3, ifelse(g$intervention_id >= 40, 1.5, 1), TRUE)
}

cod_build_policy_mix <- function(c, d, cfg) {
  n <- unique(d$nipo[d$nipo$iso3 == c$iso3 & d$nipo$tech == c$tech & d$nipo$year >= COD_POLICY_SINCE, c("intervention_id", "family", "support")])
  a <- stats::aggregate(intervention_id ~ family + support, n, length)
  a <- a[order(-a$intervention_id), ]
  a <- utils::head(a, 9)
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  noun <- if (!is.na(COD_TECH_NOUN[c$tech])) COD_TECH_NOUN[[c$tech]] else tolower(c$tech)
  lab <- unname(COD_FAMILY_LABEL[a$family]); lab[is.na(lab)] <- a$family[is.na(lab)]
  rows <- data.frame(label = lab, iso3 = "", value = a$intervention_id, highlight = FALSE,
                     group = ifelse(a$support == 1, "Support", "Protection"), stringsAsFactors = FALSE)
  last <- max(d$nipo$year)
  ns <- sum(n$support == 1); np <- sum(n$support == 0)
  title <- sprintf("%s has announced %d measures touching its %s supply chain since %d; the most common: %s",
                   Who, nrow(n), noun, COD_POLICY_SINCE, tolower(lab[1]))
  s <- cod_spec(c, title, sprintf("Industrial-policy measures announced by %s since %d that touch %s supply-chain products, by type (%d support, %d protection)",
                                  who, COD_POLICY_SINCE, noun, ns, np),
                list(type = "bar", format = "count", color_by = "group", colors = as.list(COD_POLICY_COL)), rows,
                sprintf("Each measure counted once, through %s. Measures recorded by Global Trade Alert and judged distortive; sanctions excluded. A measure counts here if it covers at least one product in the %s basket.", last, noun),
                "Global Trade Alert; NEIS industrial-policy (NIPO) coding", d)
  s$period <- sprintf("%d-%d", COD_POLICY_SINCE, last)
  s
}

COD_LEADERS_SINCE <- 2023L

cod_policy_leader_table <- function(d, tech) {
  n <- unique(d$nipo[d$nipo$tech == tech & d$nipo$support == 1 & d$nipo$year >= COD_LEADERS_SINCE, c("intervention_id", "iso3")])
  a <- stats::aggregate(intervention_id ~ iso3, n, length)
  names(a)[2] <- "value"
  a <- a[order(-a$value), ]
  a$rank <- rank(-a$value, ties.method = "min")
  a
}

cod_cand_policy_leaders <- function(d, cfg) {
  if (is.null(d$nipo)) return(NULL)
  out <- lapply(unique(d$nipo$tech), function(t) {
    a <- cod_policy_leader_table(d, t)
    a <- a[a$rank <= 12 & a$value >= 5, ]
    cod_cand("policy_leaders", "support_measures_count", "count", rep(t, nrow(a)), "All", a$iso3, cod_rank_interest(a$rank), TRUE)
  })
  do.call(rbind, out)
}

cod_build_policy_leaders <- function(c, d, cfg) {
  a <- cod_policy_leader_table(d, c$tech)
  f <- a[a$iso3 == c$iso3, ]
  top <- utils::head(a, 10)
  rows <- if (!c$iso3 %in% top$iso3) rbind(top, f) else top
  lab <- ifelse(rows$iso3 == "EUU", "EU institutions", cod_country_label(rows$iso3, d$countries))
  out <- data.frame(label = paste0(rows$rank, ". ", lab), iso3 = rows$iso3, value = rows$value, highlight = rows$iso3 == c$iso3,
                    stringsAsFactors = FALSE)
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  noun <- if (!is.na(COD_TECH_NOUN[c$tech])) COD_TECH_NOUN[[c$tech]] else tolower(c$tech)
  last <- max(d$nipo$year)
  title <- sprintf("%s has announced %d measures supporting its %s supply chain since %d, %s", Who, f$value, noun, COD_LEADERS_SINCE,
                   if (f$rank == 1) "more than any other government" else sprintf("the %s-most of any government", cod_ordinal(f$rank)))
  s <- cod_spec(c, title, sprintf("Support measures (grants, loans, tax breaks, subsidies, local-content incentives) touching %s supply-chain products, announced %d-%d",
                                  noun, COD_LEADERS_SINCE, last),
                list(type = "bar", format = "count"), out,
                "Each measure counted once. Measures recorded by Global Trade Alert and judged distortive; counts measure activity, not money.",
                "Global Trade Alert; NEIS industrial-policy (NIPO) coding", d)
  s$period <- sprintf("%d-%d", COD_LEADERS_SINCE, last)
  s
}

# ---- FDI: who builds clean-tech factories where ----------------------------------------------------------------

COD_FDI_SECTOR <- c(Battery = "battery", EV = "EV", Solar = "solar", Wind = "wind")

cod_cand_fdi_projects <- function(d, cfg) {
  if (is.null(d$fdi)) return(NULL)
  f <- d$fdi
  h <- stats::aggregate(project ~ host, f, length); h <- h[h$project >= 8, ]
  b <- stats::aggregate(project ~ home, f, length); b <- b[b$project >= 8, ]
  rbind(cod_cand("fdi_projects", "factories_hosted", "count", "All", "All", h$host, ifelse(h$project >= 25, 1.6, 1.1), TRUE),
        cod_cand("fdi_projects", "factories_abroad", "count", "All", "All", b$home, ifelse(b$project >= 25, 1.6, 1.1), TRUE))
}

cod_build_fdi_projects <- function(c, d, cfg) {
  f <- d$fdi
  hosted <- c$variable == "factories_hosted"
  x <- if (hosted) f[f$host == c$iso3, ] else f[f$home == c$iso3, ]
  by <- if (hosted) x$home else x$host
  a <- sort(table(by), decreasing = TRUE)
  top <- utils::head(a, 8); rest <- sum(a) - sum(top)
  bloc <- d$countries$bloc[match(names(top), d$countries$iso3)]; bloc[is.na(bloc)] <- "Nonaligned"
  rows <- data.frame(label = cod_country_label(names(top), d$countries), iso3 = names(top), value = as.numeric(top), highlight = FALSE,
                     group = bloc, stringsAsFactors = FALSE)
  if (rest > 0) rows <- rbind(rows, data.frame(label = sprintf("Others (%d countries)", length(a) - length(top)), iso3 = "", value = rest,
                                               highlight = FALSE, group = "Others", stringsAsFactors = FALSE))
  who <- cod_country_name(c$iso3, d$countries); Who <- cod_cap1(who)
  lead <- cod_country_name(names(a)[1], d$countries)
  adj <- function(iso) {
    # demonym for "<X> companies"
    dem <- c(CHN = "Chinese", USA = "American", KOR = "Korean", JPN = "Japanese", DEU = "German", TWN = "Taiwanese", IND = "Indian",
             FRA = "French", GBR = "British", CAN = "Canadian", ESP = "Spanish", SWE = "Swedish", DNK = "Danish", NLD = "Dutch",
             ITA = "Italian", NOR = "Norwegian", AUS = "Australian", BRA = "Brazilian", VNM = "Vietnamese", THA = "Thai",
             SGP = "Singaporean", HKG = "Hong Kong", BEL = "Belgian", CHE = "Swiss", AUT = "Austrian", FIN = "Finnish", ISR = "Israeli",
             TUR = "Turkish", MEX = "Mexican", IDN = "Indonesian", MYS = "Malaysian", POL = "Polish", SAU = "Saudi", ARE = "Emirati")
    if (!is.na(dem[iso])) unname(dem[iso]) else paste0(cod_country_label(iso, d$countries), "-based")
  }
  sectors <- paste(unique(COD_FDI_SECTOR[x$sector]), collapse = ", ")
  yrs <- range(x$year, na.rm = TRUE)
  if (hosted) {
    title <- sprintf("%s companies are behind %d of the %d foreign-owned battery, EV, solar and wind factory projects in %s", cod_cap1(adj(names(a)[1])),
                     as.integer(a[1]), sum(a), who)
    sub <- sprintf("Foreign-owned clean-tech factory projects in %s by the owner's home country, announced %s-%s", who, yrs[1], yrs[2])
  } else {
    title <- sprintf("%s companies have announced %d battery, EV, solar and wind factory projects abroad; %d are in %s", cod_cap1(adj(c$iso3)), sum(a),
                     as.integer(a[1]), lead)
    sub <- sprintf("Clean-tech factory projects abroad by %s companies, by host country, announced %s-%s", adj(c$iso3), yrs[1], yrs[2])
  }
  s <- cod_spec(c, title, sub, list(type = "bar", format = "count", color_by = "group"), rows,
                paste("Projects announced, under construction or operating (cancelled and closed projects left out); each project counted once.",
                      "Colour shows each country's alignment in UN General Assembly voting (U.S.-leaning, China-leaning, nonaligned)."),
                "NEIS clean-tech manufacturing FDI tracker", d)
  s$period <- sprintf("%s-%s", yrs[1], yrs[2])
  s
}

COD_TEMPLATES_MORE <- c("ei_power_mix", "ei_trend", "gcim_mix", "gcim_power", "export_share_trend", "policy_mix", "policy_leaders", "fdi_projects")
