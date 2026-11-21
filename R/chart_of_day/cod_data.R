# Chart of the Day: turn the pipeline's processed tables into the tidy raw-data inputs the templates use (pure).
#
# `raw` is a named list of the processed tables (the script reads them from data/processed/*_tbl.rds):
# trade_concentration, import_partner_cells, import_exposure, investment_momentum, critical_minerals_trade_flows,
# critical_minerals_production, reserves, lcoe_competitiveness, coalition. Only rows flagged data_type == "raw"
# (or tables that hold raw values only) are used: no index scores reach a chart.

COD_MINERALS <- c("Cobalt", "Copper", "Graphite", "Lithium", "Manganese", "Nickel", "PGMs", "Rare Earths", "Zinc",
                  "Chromium", "Molybdenum", "Silicon", "Silver", "Uranium")

cod_raw_rows <- function(x, variables = NULL) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- x[x$data_type %in% "raw" & !is.na(x$value), , drop = FALSE]
  if (!is.null(variables)) x <- x[x$variable %in% variables, , drop = FALSE]
  x
}

#' Unit in brackets at the start of an explanation ("... in 2025 (Thousand tonnes) from ..."), lower-cased.
cod_unit_from_explanation <- function(expl) {
  u <- regmatches(expl, regexpr("\\(([^)]+)\\)", expl))
  u <- if (length(u)) gsub("[()]", "", u) else ""
  u <- sub(", .*$", "", u)
  tolower(u)
}

#' Month label for a YYYYMM integer: 202608 -> "Aug 2026".
cod_month_label <- function(yyyymm) paste(month.abb[as.integer(yyyymm) %% 100], as.integer(yyyymm) %/% 100)

#' Latest-12-month trade by reporter x partner x flow x tech x stage, from the T12M HS6 dyads that
#' scripts/38_build_current_import_exposure.R saves (current_trade_dyads_tbl.rds: each reporter's own monthly
#' reports, partial months dropped, scaled to 12 months) and the HS6 stage crosswalk (energy_hs6_master.csv).
#' A code in several cells counts in each (as in the 2024 baskets); duplicate crosswalk rows are dropped.
cod_current_cells <- function(dyads, master) {
  m <- unique(data.frame(hs6 = formatC(as.integer(master$hs6), width = 6, flag = "0", format = "d"), tech = as.character(master$tech),
                         stage = as.character(master$stage), stringsAsFactors = FALSE))
  m <- m[!is.na(m$stage) & nzchar(m$stage) & !is.na(m$tech), ]
  x <- data.frame(reporter = dyads$reporter_iso, partner = dyads$partner_iso, flow = dyads$flow_direction,
                  hs6 = formatC(as.integer(dyads$cmd_code), width = 6, flag = "0", format = "d"), usd = dyads$primary_value,
                  window_end = dyads$window_end, stringsAsFactors = FALSE)
  x <- x[x$reporter != x$partner & !is.na(x$usd) & x$usd > 0, ]
  j <- merge(x, m, by = "hs6")
  key <- paste(j$reporter, j$partner, j$flow, j$tech, j$stage, sep = "")
  s <- rowsum(j$usd, key)
  parts <- do.call(rbind, strsplit(rownames(s), "", fixed = TRUE))
  cells <- data.frame(reporter = parts[, 1], partner = parts[, 2], flow = parts[, 3], tech = parts[, 4], stage = parts[, 5],
                      usd = as.numeric(s), stringsAsFactors = FALSE)
  w <- unique(data.frame(reporter = j$reporter, flow = j$flow, window_end = j$window_end))
  w <- stats::aggregate(window_end ~ reporter + flow, w, max)
  w$period <- paste("12 months to", cod_month_label(w$window_end))
  list(cells = cells, windows = w)
}

#' Tidy inputs for the templates. `current` (optional) is cod_current_cells() output: when given, every chart
#' about one country's own trade uses its latest 12 months of monthly reports, and 2024 only where the country
#' does not report monthly. World rankings stay on 2024: China has published no monthly data since 2024-12.
cod_prepare <- function(raw, current = NULL) {
  # trade: one row per country x tech x stage (2024, HS6 basket)
  tc <- cod_raw_rows(raw$trade_concentration, c("exports", "imports", "market_share", "gdp", "deficit_gdp"))
  tc <- tc[is.na(tc$sub_sector) | tc$sub_sector == "All", , drop = FALSE]
  key <- paste(tc$iso3c, tc$tech, tc$supply_chain, sep = "|")
  cells <- unique(data.frame(key = key, iso3 = tc$iso3c, tech = tc$tech, stage = tc$supply_chain, stringsAsFactors = FALSE))
  pick <- function(v) { s <- tc[tc$variable == v, ]; s$value[match(cells$key, paste(s$iso3c, s$tech, s$supply_chain, sep = "|"))] }
  trade <- data.frame(iso3 = cells$iso3, tech = cells$tech, stage = cells$stage, exports = pick("exports"), imports = pick("imports"),
                      share = pick("market_share"), gdp = pick("gdp"), deficit_gdp = pick("deficit_gdp"), stringsAsFactors = FALSE)
  trade <- trade[trade$iso3 %in% unique(tc$iso3c[tc$variable %in% c("exports", "imports")]), ]
  trade_year <- if ("Year" %in% names(tc)) max(tc$Year, na.rm = TRUE) else NA

  pc <- as.data.frame(raw$import_partner_cells, stringsAsFactors = FALSE)
  partners <- data.frame(reporter = pc$reporter_iso, partner = pc$partner_iso, tech = pc$tech, stage = pc$supply_chain,
                         usd = pc$imports_usd, stringsAsFactors = FALSE)
  partners <- partners[partners$reporter != partners$partner & !is.na(partners$usd) & partners$usd > 0, ]
  partners$period <- "2024"
  partners_2024 <- partners
  # exports by buyer, 2024: the buyers' records (mirror)
  exports <- data.frame(exporter = partners$partner, buyer = partners$reporter, tech = partners$tech, stage = partners$stage,
                        usd = partners$usd, period = "2024", basis = "mirror", stringsAsFactors = FALSE)
  balance <- data.frame(iso3 = trade$iso3, tech = trade$tech, stage = trade$stage, exports = trade$exports, imports = trade$imports,
                        period = "2024", stringsAsFactors = FALSE)
  if (!is.null(current)) {
    cc <- current$cells; w <- current$windows
    per <- function(iso, flow) w$period[match(paste(iso, flow), paste(w$reporter, w$flow))]
    ci <- cc[cc$flow == "import", ]
    ce <- cc[cc$flow == "export", ]
    rep_i <- unique(ci$reporter); rep_e <- unique(ce$reporter)
    partners <- rbind(partners[!partners$reporter %in% rep_i, ],
                      data.frame(reporter = ci$reporter, partner = ci$partner, tech = ci$tech, stage = ci$stage, usd = ci$usd,
                                 period = per(ci$reporter, "import"), stringsAsFactors = FALSE))
    exports <- rbind(exports[!exports$exporter %in% rep_e, ],
                     data.frame(exporter = ce$reporter, buyer = ce$partner, tech = ce$tech, stage = ce$stage, usd = ce$usd,
                                period = per(ce$reporter, "export"), basis = "own", stringsAsFactors = FALSE))
    both <- intersect(rep_i, rep_e)
    si <- stats::aggregate(usd ~ reporter + tech + stage, ci[ci$reporter %in% both, ], sum)
    se <- stats::aggregate(usd ~ reporter + tech + stage, ce[ce$reporter %in% both, ], sum)
    k <- unique(rbind(si[1:3], se[1:3]))
    kk <- paste(k$reporter, k$tech, k$stage)
    cur_bal <- data.frame(iso3 = k$reporter, tech = k$tech, stage = k$stage,
                          exports = se$usd[match(kk, paste(se$reporter, se$tech, se$stage))],
                          imports = si$usd[match(kk, paste(si$reporter, si$tech, si$stage))],
                          period = per(k$reporter, "import"), stringsAsFactors = FALSE)
    balance <- rbind(balance[!balance$iso3 %in% both, ], cur_bal)
  }

  ie <- as.data.frame(raw$import_exposure, stringsAsFactors = FALSE)
  exposure <- data.frame(iso3 = ie$importer_iso, tech = ie$tech, stage = ie$supply_chain, imports_usd = ie$imports_usd,
                         china_share = ie$china_share, stringsAsFactors = FALSE)

  # investment: manufacturing capex is replicated onto Components and Assembly in the source table, so keep one
  # copy and call it Manufacturing; Refining (critical minerals) is split between Batteries and EVs already.
  im <- cod_raw_rows(raw$investment_momentum)
  im <- im[grepl("^Annual Investment", im$variable), , drop = FALSE]
  im <- im[im$supply_chain != "Components", , drop = FALSE]
  invest <- data.frame(iso3 = im$iso3c, tech = im$tech, stage = ifelse(im$supply_chain == "Assembly", "Manufacturing", im$supply_chain),
                       year = as.integer(im$Year), usd = im$value * 1e9, stringsAsFactors = FALSE)
  invest <- stats::aggregate(usd ~ iso3 + tech + stage + year, invest, sum)

  # leaderboard metrics that are not trade: long table
  metric <- list()
  cm <- as.data.frame(raw$critical_minerals_trade_flows, stringsAsFactors = FALSE)
  cm <- cm[!is.na(cm$market_share), ]
  metric$mineral_exports_share <- data.frame(var = "mineral_exports_share", iso3 = cm$iso3c, tech = cm$mineral, stage = cm$supply_chain,
                                             value = cm$market_share, aux = cm$exports, unit = "share of world exports", year = cm$Year,
                                             source = "UN Comtrade (mirror data where a country does not report)", stringsAsFactors = FALSE)
  mp <- cod_raw_rows(raw$critical_minerals_production)
  mp$mineral <- sub(" Production$", "", mp$variable)
  mp$mineral[mp$mineral == "Nuclear"] <- "Uranium"
  mp <- mp[mp$mineral %in% COD_MINERALS, ]
  metric$mineral_production <- data.frame(var = "mineral_production", iso3 = mp$iso3c, tech = mp$mineral, stage = "Extraction",
                                          value = mp$value, aux = NA_real_, unit = vapply(mp$explanation, cod_unit_from_explanation, ""),
                                          year = mp$Year, source = mp$source, stringsAsFactors = FALSE)
  rs <- cod_raw_rows(raw$reserves)
  rs$mineral <- sub(" Reserves$", "", rs$variable)
  rs$mineral[rs$mineral == "Nuclear"] <- "Uranium"
  rs <- rs[rs$mineral %in% COD_MINERALS, ]
  metric$mineral_reserves <- data.frame(var = "mineral_reserves", iso3 = rs$iso3c, tech = rs$mineral, stage = "Extraction",
                                        value = rs$value, aux = NA_real_, unit = vapply(rs$explanation, cod_unit_from_explanation, ""),
                                        year = rs$Year, source = rs$source, stringsAsFactors = FALSE)
  lc <- cod_raw_rows(raw$lcoe_competitiveness, "lcoe_25")
  metric$lcoe_2025 <- data.frame(var = "lcoe_2025", iso3 = lc$iso3c, tech = lc$tech, stage = "Deployment", value = lc$value,
                                 aux = NA_real_, unit = "$/MWh", year = lc$Year, source = "BloombergNEF (Mid scenario)", stringsAsFactors = FALSE)
  if (nrow(invest)) {
    y <- max(invest$year)
    iv <- invest[invest$year == y, ]
    metric$investment_latest <- data.frame(var = "investment_latest", iso3 = iv$iso3, tech = iv$tech, stage = iv$stage, value = iv$usd,
                                           aux = NA_real_, unit = "USD", year = y, source = "Rhodium Group (GCIM)", stringsAsFactors = FALSE)
  }
  metric <- do.call(rbind, metric)
  metric <- metric[!is.na(metric$value) & metric$value > 0, ]

  co <- as.data.frame(raw$coalition, stringsAsFactors = FALSE)
  countries <- data.frame(iso3 = co$iso3, country = co$country, tier = co$tier, bloc = co$bloc, gdp_usd = co$gdp_usd,
                          stringsAsFactors = FALSE)
  list(trade = trade, trade_year = trade_year, partners = partners, partners_2024 = partners_2024, exports = exports,
       balance = balance, has_current = !is.null(current), exposure = exposure, invest = invest, metric = metric,
       countries = countries)
}
