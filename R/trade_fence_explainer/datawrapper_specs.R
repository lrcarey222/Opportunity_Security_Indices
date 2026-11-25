# Datawrapper versions of the trade-fence explainer charts (scripts/trade_fence_explainer/04_datawrapper.R).
# Pure: takes the chart CSVs (outputs/trade_fence_explainer/data/, read by the script into a named list `d`) and
# returns one spec per chart: key, title, Datawrapper type, data frame, and metadata (describe, annotate, visualize,
# axes). The script creates or updates the charts through the Datawrapper API in the NEIS theme.
#
# Settings use Datawrapper's documented metadata keys and otherwise lean on the NEIS theme defaults. Categorical
# cell grids (charts 5 and 6 of the article) are tables whose cells carry coloured-square glyphs, because a table
# heatmap colours numbers, not categories.

TFE_DW_SOURCE <- "NEIS analysis of US Census Bureau data (via the Trade War Tracker), UN Comtrade and Federal Register notices"
TFE_DW_BYLINE <- "NEIS / RMI"
TFE_DW_CAUSAL <- "Policy events are shown for context; changes in trade flows cannot be attributed to individual measures from these descriptive charts alone."
TFE_DW_TRI <- "Trade Restrictiveness Index (Waugh): the uniform tariff with the same welfare cost as the actual tariffs, on 2024 import weights; it is not an average tariff."
TFE_DW_VERDICT_SQ <- c(Aligned = "\U0001F7E9", `Taxes trusted partners` = "\U0001F7EA", `Fence above recommendation` = "\U0001F7E7",
                       `Fence below recommendation / missing` = "\U0001F7E5")
TFE_DW_TECH <- c(Batteries = "Batteries", `Electric Vehicles` = "Electric vehicles", Solar = "Solar", Wind = "Wind",
                 `Electric Grid` = "Electric grid", Nuclear = "Nuclear", Gas = "Gas", Oil = "Oil", Coal = "Coal")

tfe_dw_spec <- function(key, title, type, data, intro, notes, source = TFE_DW_SOURCE, visualize = list(), axes = NULL,
                        source_url = "") {
  meta <- list(describe = list(intro = intro, `source-name` = source, `source-url` = source_url, byline = TFE_DW_BYLINE),
               annotate = list(notes = notes), visualize = visualize)
  if (!is.null(axes)) meta$axes <- axes
  list(key = key, title = title, type = type, data = data, metadata = meta)
}
r1 <- function(x, d = 1) round(x, d)
bn <- function(x) round(x / 1e9, 1)

tfe_dw_specs <- function(d, period = "January-August", data_month = "August 2026", snapshot = "4 December 2026") {
  out <- list()
  add <- function(s) out[[length(out) + 1]] <<- s

  # 1a-1d: what America trades
  bt <- d$tech[!is.na(d$tech$imports_2026), ]
  lab <- function(x) sprintf("%s (net %s$%sbn)", TFE_DW_TECH[x$tech], ifelse(x$balance_2026 < 0, "−", "+"), formatC(abs(x$balance_2026) / 1e9, format = "f", digits = 1))
  for (g in c("Clean energy and grid", "Fuels (oil, gas, coal)")) {
    x <- bt[bt$group == g, ]
    x <- x[order(-x$imports_2026), ]
    add(tfe_dw_spec(
      if (g == "Clean energy and grid") "00a_trade_by_tech_clean" else "00a_trade_by_tech_fuels",
      if (g == "Clean energy and grid") "America imports far more clean-energy and grid goods than it exports" else "The US is a net exporter of fuels",
      "d3-bars-grouped",
      data.frame(Technology = lab(x), `Imports ($bn)` = bn(x$imports_2026), `Exports ($bn)` = bn(x$exports_2026), check.names = FALSE),
      sprintf("US imports and exports, %s 2026, $bn; net balance in brackets.", period),
      "Technology totals are attributed: a product in several technologies (static converters, switchgear, cable) counts in each, so rows overlap. Producing stages only (Deployment repeats Assembly).",
      visualize = list(`custom-colors` = list(`Imports ($bn)` = "#016787", `Exports ($bn)` = "#a3b53a"))))
  }
  st <- d$stage
  for (b in unique(st$basket)) {
    x <- st[st$basket == b, ]
    clean <- grepl("^Clean", b)
    add(tfe_dw_spec(
      if (clean) "00a_trade_by_stage_clean" else "00a_trade_by_stage_all",
      if (clean) sprintf("%s%% of clean-energy and grid imports are assembled goods", round(100 * x$import_share_of_basket[x$stage == "Assembly"])) else "Across all electro-industrial goods, the US exports raw materials and fuels and imports equipment",
      "d3-bars-grouped",
      data.frame(Stage = x$stage, `Imports ($bn)` = bn(x$imports_2026), `Exports ($bn)` = bn(x$exports_2026), check.names = FALSE),
      sprintf("US imports and exports by supply-chain stage, %s 2026, $bn (%s).", period, tolower(b)),
      "Each HS6 product is counted once; a product in different stages of different technologies is split equally between them. Deployment is not a traded stage.",
      visualize = list(`custom-colors` = list(`Imports ($bn)` = "#016787", `Exports ($bn)` = "#a3b53a"))))
  }

  # 2: rewiring scatter and timeline table
  rw <- d$rewiring[!d$rewiring$tech %in% c("Oil", "Gas", "Coal", "Refined copper cathodes"), ]
  add(tfe_dw_spec("00b_trade_rewiring", "US electro-industrial trade is being rewired, not simply reduced", "d3-scatter-plot",
    stats::setNames(data.frame(ifelse(is.na(rw$label), rw$tech, rw$label), r1(rw$imports_change_pct), r1(100 * (rw$china_share_2026 - rw$china_share_2024)),
                               bn(rw$imports_2026), rw$kind),
                    c("Label", "Change in imports (%)", "Change in China's share (pts)", sprintf("Imports %s 2026 ($bn)", period), "Kind")),
    sprintf("Change in US imports and in China's share of them, %s 2024 to 2026; bubble size is 2026 imports. Left/below zero = falling imports / falling China share.", period),
    paste("A falling China share is not by itself a success: it can reflect rerouting, Chinese-owned plants abroad or a smaller market. China = imports recorded from China (Hong Kong and Macao not separable).", TFE_DW_CAUSAL),
    axes = list(x = "Change in imports (%)", y = "Change in China's share (pts)", size = sprintf("Imports %s 2026 ($bn)", period), color = "Kind", labels = "Label"),
    visualize = list(`color-category` = list(map = list(Technology = "#1189b1", `Product group` = "#55c3c4")), `show-color-key` = TRUE,
                     size = "dynamic", `max-size` = 40)))
  tl <- d$timeline[toupper(d$timeline$key_event) == "TRUE", ]
  add(tfe_dw_spec("00c_policy_timeline", "The policy environment changed fast between the two trade windows", "tables",
    data.frame(Date = tl$date, Measure = tl$short_label, Authority = tl$authority, Products = tl$products_technologies,
               Countries = tl$countries, check.names = FALSE),
    sprintf("Key US measures for the electro-industrial stack, January 2024 to the %s snapshot.", snapshot),
    paste(TFE_DW_CAUSAL, "Full list with primary-source links: data/00_policy_timeline.csv."), source = "Federal Register; HTSUS; Trade War Tracker registry (2025 IEEPA steps)"))

  # 3: fence rising
  f1 <- d$rising[d$rising$supplier_group %in% c("China", "Coercive", "Nonaligned", "Trusted", "All suppliers"), ]
  f1 <- f1[match(c("China", "Coercive", "Nonaligned", "Trusted", "All suppliers"), f1$supplier_group), ]
  add(tfe_dw_spec("01_fence_rising", "The US raised the fence against China, and sharply raised the floor under its allies", "d3-range-plot",
    data.frame(`Supplier group` = c("China", "Coercive suppliers", "Nonaligned suppliers", "Trusted suppliers", "All suppliers"),
               `2024` = r1(f1$tri_2024), `August 2026` = r1(f1$tri_latest), check.names = FALSE),
    sprintf("Trade Restrictiveness Index on all US goods imports, 2024 and %s, %%. Trusted suppliers +%s points (×%s); China +%s points.", data_month,
            r1(f1$change_points[f1$supplier_group == "Trusted"]), r1(f1$ratio[f1$supplier_group == "Trusted"]), r1(f1$change_points[f1$supplier_group == "China"])),
    paste("Census calculated duty; AD/CVD excluded.", TFE_DW_TRI, "Trusted = US-leaning UN-voting bloc; coercive = China, Hong Kong, Macao, Russia and the China-leaning bloc (incl. Vietnam, Indonesia)."),
    visualize = list(`range-value-labels` = "both", `custom-colors` = list(`2024` = "#919aa1", `August 2026` = "#001f3e"))))

  # 4: tariff map (table heatmap)
  tm <- d$map
  techs <- c("Batteries", "Electric Vehicles", "Solar", "Wind", "Electric Grid", "Nuclear", "Gas", "Oil", "Coal")
  stages <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
  grid <- do.call(rbind, lapply(techs, function(t) {
    row <- tm[tm$tech == t, ]
    v <- vapply(stages, function(s) { x <- row[row$stage == s, ]; if (nrow(x) && isTRUE(as.logical(x$in_basket))) r1(x$tri_all, 0) else NA_real_ }, numeric(1))
    data.frame(Technology = TFE_DW_TECH[[t]], t(v), check.names = FALSE)
  }))
  names(grid)[names(grid) == "Deployment"] <- "Deployment*"
  add(tfe_dw_spec("02_tariff_map", "Where has America actually built tariff walls?", "tables", grid,
    sprintf("Trade Restrictiveness Index by technology and supply-chain stage, all suppliers, %s Census duty plus AD/CVD deposits, %%. Blank: no traded goods.", data_month),
    paste("*Deployment has no traded goods of its own; it repeats the Assembly basket. AD/CVD lines capped at 100%.", TFE_DW_TRI),
    visualize = list(heatmap = list(enabled = TRUE, mode = "continuous", stops = "equidistant", rangeMin = "0", rangeMax = "40",
                                    colors = list(list(color = "#ecf1f4", position = 0), list(color = "#88c6d9", position = 0.35), list(color = "#1189b1", position = 0.7),
                                                  list(color = "#001f3e", position = 1))),
                     columns = stats::setNames(lapply(setdiff(names(grid), "Technology"), function(x) list(heatmap = TRUE)), setdiff(names(grid), "Technology")))))

  # 5: policy intensity (table with intensity bars)
  ft <- d$tools
  bar <- c(Low = "▰", Medium = "▰▰", High = "▰▰▰")
  ft$cell <- paste0(bar[ft$band], " ", ft$band, ifelse(as.logical(ft$single_measure), "†", ""))
  ft$row <- paste0(c(`Electric Grid` = "Grid", Solar = "Solar", Batteries = "Batteries")[ft$tech], ": ", ft$stage)
  tools <- unique(ft$tool)
  tg <- do.call(rbind, lapply(unique(ft$row), function(r) {
    x <- ft[ft$row == r, ]
    data.frame(`Chain and stage` = r, t(stats::setNames(x$cell[match(tools, x$tool)], tools)), check.names = FALSE)
  }))
  add(tfe_dw_spec("04_multi_tool_fence", "America's new trade regime is not really just a tariff regime", "tables", tg,
    "Relative intensity of each US policy tool by stage: how often the tool falls on a stage's products compared with the same tool across all US imports (High = top 10% of products, Medium = next 30%, Low = the rest).",
    "Bands compare each tool with itself, not tools with each other: a High procurement cell is not market closure equal to a High tariff. † = band rests on a single measure. Non-tariff tools count Global Trade Alert NIPO measures in force on 31 Oct 2026 naming each HS6 code. From 4 Dec 2026 Section 232 adds a 15% tariff and price floors on solar wafers, cells and modules.",
    source = "NEIS analysis of Global Trade Alert NIPO and US Census Bureau data"))

  # 6: Yards & Fences audit (table with verdict squares) + summary bars + dollars
  au <- d$audit
  ag <- do.call(rbind, lapply(techs, function(t) {
    row <- au[au$tech == t, ]
    v <- vapply(stages, function(s) {
      x <- row[row$stage == s, ]
      if (!nrow(x)) return("")
      if (is.na(x$verdict4) || !nzchar(x$verdict4)) return(if (isTRUE(as.logical(x$in_basket)) || identical(x$strategic_choice, "not_assessed")) "not assessed" else "")
      paste(TFE_DW_VERDICT_SQ[[x$verdict4]], x$choice_label)
    }, character(1))
    data.frame(Technology = TFE_DW_TECH[[t]], t(v), check.names = FALSE)
  }))
  vs <- d$summary
  th <- d$threshold
  add(tfe_dw_spec("05_yards_fences_audit", sprintf("Only %d of %d electro-industrial tariff fences match the strategic posture Yards & Fences recommends", vs$cells_observed[vs$verdict == "Aligned"], sum(vs$cells_observed)),
    "tables", ag,
    paste0(paste(sprintf("%s %s: %d", TFE_DW_VERDICT_SQ[vs$verdict], vs$verdict, vs$cells_observed), collapse = " · "),
           ". Each cell names the recommended choice. Aligned cells under other open-gate lines: ",
           paste(sprintf("%s%% → %d of %d", th$open_threshold, th$Aligned, th$assessed_cells), collapse = "; "), "."),
    sprintf("Tariff fence = %s Census duty plus AD/CVD (TRI). Secure needs 15%%+ on coercive and under 5%% on trusted suppliers; Preserve, Catch Up, Partner and Accelerate need trusted partners under 5%%; Watch needs all suppliers under 5%%. Unchanged on the %s statutory regime.", data_month, snapshot)))
  add(tfe_dw_spec("05b_verdict_summary", sprintf("%d of %d US electro-industrial tariff fences match the strategy", vs$cells_observed[vs$verdict == "Aligned"], sum(vs$cells_observed)), "d3-bars",
    data.frame(Verdict = vs$verdict, Cells = vs$cells_observed),
    "Technology x supply-chain stage cells by verdict: US tariffs against the Yards & Fences posture.",
    sprintf("Observed %s Census duty plus AD/CVD; open gate under 5%%, high fence 15%% and over.", data_month),
    visualize = list(`custom-colors` = list(Aligned = "#0e8a3a", `Taxes trusted partners` = "#7b4fbf", `Fence above recommendation` = "#c98500",
                                           `Fence below recommendation / missing` = "#d03b3b"))))
  dw <- d$dollars[d$dollars$basis == "dedup_equal_split" & grepl("^observed", d$dollars$verdict_basis), ]
  bars <- c(`All suppliers` = "All imports", `Trusted partners` = "From trusted partners", `All suppliers (excl. oil, gas, coal)` = "Clean energy and grid: all imports",
            `Trusted partners (excl. oil, gas, coal)` = "Clean energy and grid: from trusted partners")
  verd <- c("Aligned", "Taxes trusted partners", "Fence above recommendation", "Fence below recommendation / missing")
  dd <- do.call(rbind, lapply(names(bars), function(b) {
    x <- dw[dw$suppliers == b, ]
    data.frame(Imports = sprintf("%s ($%.0fbn)", bars[[b]], x$basis_total_usd[1] / 1e9), t(stats::setNames(r1(100 * x$share[match(verd, x$verdict)]), verd)), check.names = FALSE)
  }))
  cl <- dw[dw$suppliers == "All suppliers (excl. oil, gas, coal)", ]
  add(tfe_dw_spec("06_dollar_weighted_misalignment", sprintf("Outside oil, gas and coal, only %s%% of the imports behind these fences meet an aligned one", round(100 * cl$share[cl$verdict == "Aligned"])),
    "d3-bars-stacked", dd, "2024 US imports in the assessed basket by the verdict of the cells they sit in, %, each HS6 product counted once.",
    "A product in several technologies is split equally across the producing-stage cells it sits in; Deployment cells, which repeat Assembly, are left out. Aligned dollars across all technologies are mostly crude oil and refined products.",
    visualize = list(`custom-colors` = list(Aligned = "#0e8a3a", `Taxes trusted partners` = "#7b4fbf", `Fence above recommendation` = "#c98500",
                                           `Fence below recommendation / missing` = "#d03b3b"), `stack-percentages` = FALSE)))

  # 7: why allies pay (dot plot)
  wa <- d$allies
  sc <- c(`Dec 4 statutory (all measures)` = "All measures, 4 Dec 2026", `Without Section 232` = "Without Section 232", `Without Section 301` = "Without Section 301",
          `Without Section 338` = "Without Section 338", `Without AD/CVD` = "Without AD/CVD", `2024 rates (pre-2025 baseline)` = "2024 duty rates")
  cells <- unique(wa$key)
  dp <- do.call(rbind, lapply(cells, function(k) {
    x <- wa[wa$key == k, ]
    data.frame(Cell = paste(TFE_DW_TECH[[x$tech[1]]], tolower(x$stage[1])), t(stats::setNames(r1(x$tri[match(names(sc), x$scenario)]), unname(sc))),
               `Observed August 2026` = r1(x$observed_aug2026_trusted_tri[1]), check.names = FALSE)
  }))
  add(tfe_dw_spec("07_why_allies_pay", "Steel, aluminium and copper tariffs explain most of what trusted partners pay for clean-energy equipment", "d3-dot-plot", dp,
    sprintf("Trusted-partner Trade Restrictiveness Index on the %s statutory regime, and with one authority switched off at a time, %%.", snapshot),
    paste("Each scenario starts from observed August 2026 duty and adds the statutory change to 4 December modelled with the Trade War Tracker engine, with every action of one authority removed. Section 338 (Canada) coverage checked against HTSUS Revision 23, chapter 99 U.S. note 51.", TFE_DW_TRI),
    visualize = list(`custom-colors` = list(`All measures, 4 Dec 2026` = "#001f3e", `Without Section 232` = "#f58229", `Without Section 301` = "#1189b1",
                                           `Without Section 338` = "#646e77", `Without AD/CVD` = "#7b4fbf", `2024 duty rates` = "#bfc5ca", `Observed August 2026` = "#a3b53a"))))

  # 8: trade response
  so <- d$solar
  origins <- c("Vietnam", "Thailand", "Malaysia", "Cambodia", "India", "Indonesia", "Laos", "Philippines", "Africa", "China", "All other")
  so <- so[so$origin %in% origins, ]
  wide <- stats::reshape(so[c("time", "origin", "value")], idvar = "time", timevar = "origin", direction = "wide")
  names(wide) <- sub("^value\\.", "", names(wide))
  wide <- wide[order(wide$time), c("time", intersect(origins, names(wide)))]
  wide[-1] <- lapply(wide[-1], function(v) round(ifelse(is.na(v), 0, v) / 1e6, 0))
  est <- c("Cambodia", "Laos", "Philippines", "Africa")
  names(wide) <- ifelse(names(wide) %in% est, paste0(names(wide), " (est.)"), names(wide))
  names(wide)[1] <- "Month"
  pal <- c(Vietnam = "#001f3e", Thailand = "#1189b1", Malaysia = "#4ca7c5", Cambodia = "#88c6d9", India = "#f58229", Indonesia = "#f6a05f", Laos = "#f6bd95",
           Philippines = "#55c3c4", Africa = "#a3b53a", China = "#646e77", `All other` = "#d3dce2")
  names(pal) <- ifelse(names(pal) %in% est, paste0(names(pal), " (est.)"), names(pal))
  add(tfe_dw_spec("08a_solar_trade_response", "Solar module imports collapsed and moved to new sources", "stacked-column-chart", wide,
    "US imports of crystalline-silicon modules (HTS 8541.43.0010) by origin, $m per month.",
    "Origins outside the Trade War Tracker's 30 partners (est.) split the Census remainder by each country's monthly share of US imports of HS 8541.43 in UN Comtrade; the monthly totals are exact. Africa: mainly Ethiopia, Kenya and Nigeria.",
    visualize = list(`custom-colors` = as.list(pal))))
  cap <- d$capacity
  add(tfe_dw_spec("08a_us_module_capacity", "US module factories grew, but cell capacity stayed small", "column-chart",
    data.frame(Period = cap$label, `Operating module capacity (GW)` = cap$value, check.names = FALSE),
    "Operating nameplate module manufacturing capacity in the US, GW. Crystalline-silicon cell capacity was about 3 GW in mid-2026.",
    "Capacity is not production: output was about 12 GW in 2024 and 31 GW in 2025 (Wood Mackenzie via NREL; vintages differ). Announced capacity is excluded.",
    source = "SEIA/Wood Mackenzie US Solar Market Insight (secondary source)", visualize = list(`base-color` = "#1189b1")))
  bo <- d$battery[d$battery$origin %in% c("China", "Korea", "Japan", "Malaysia", "Thailand", "Vietnam", "All other"), ]
  bo <- bo[match(c("China", "Japan", "Korea", "Malaysia", "Thailand", "Vietnam", "All other"), bo$origin), ]
  ow <- d$ownership
  o <- ow[ow$status_group == "All projects" & ow$destination_iso3 %in% c("MYS", "THA"), ]
  add(tfe_dw_spec("08b_battery_trade_response", "Battery imports diversified away from China by origin more than by ownership", "d3-bars-grouped",
    data.frame(Origin = sprintf("%s (%s%% → %s%%)", bo$origin, round(100 * bo$share_2024), round(100 * bo$share_2026)),
               x2024 = bn(bo$value_jan_aug_2024), x2026 = bn(bo$value_jan_aug_2026)) |>
      stats::setNames(c("Origin", sprintf("%s 2024 ($bn)", period), sprintf("%s 2026 ($bn)", period))),
    sprintf("US imports of lithium-ion batteries (HTS 8507.60) by origin, %s, $bn; share of the total in brackets.", period),
    paste0("Country of origin is not ownership: ", paste(sprintf("in %s, %d of %d battery-manufacturing FDI projects have a Chinese parent (%s%% of disclosed capital)",
           c(MYS = "Malaysia", THA = "Thailand")[o$destination_iso3], o$n_chinese_parent, o$n_projects, round(100 * o$chinese_parent_share_capital)), collapse = "; "),
           ". NEIS CM-FDI project database (projects since about 2017, not installed capacity). Cells and packs cannot be separated in trade data."),
    visualize = list(`custom-colors` = stats::setNames(list("#88c6d9", "#001f3e"), c(sprintf("%s 2024 ($bn)", period), sprintf("%s 2026 ($bn)", period))))))

  # sidebar and analysis scatters
  pg <- d$partners[as.logical(d$partners$plotted), ]
  add(tfe_dw_spec("03_who_gets_through_gate", "The US has a China gate. It does not yet have a coherent trusted-partner gate", "d3-scatter-plot",
    data.frame(Partner = pg$name, `UN-voting position (0 = China, 1 = US)` = round(pg$s_partner, 3), `Effective tariff, Jun-Aug 2026 (%)` = r1(pg$effective_rate),
               `Two-way goods trade 2024 ($bn)` = bn(pg$two_way_trade_2024_usd), Group = pg$group, check.names = FALSE),
    "Effective tariff (duties collected / imports, all goods, June-August 2026) against each partner's position on the US-China axis of UN votes; bubble size is 2024 two-way trade.",
    "Sidebar: all goods, not the electro-industrial basket. Coercive = China, Russia and the China-leaning UN-voting bloc (incl. Vietnam, Indonesia); nonaligned includes Mexico, India, Thailand, Malaysia. Taiwan has no UN vote. Rates exclude AD/CVD.",
    axes = list(x = "UN-voting position (0 = China, 1 = US)", y = "Effective tariff, Jun-Aug 2026 (%)", size = "Two-way goods trade 2024 ($bn)", color = "Group", labels = "Partner"),
    visualize = list(`color-category` = list(map = list(Trusted = "#1189b1", Nonaligned = "#919aa1", Coercive = "#f58229")), `show-color-key` = TRUE,
                     size = "dynamic", `max-size` = 45)))
  fv <- d$fence
  add(tfe_dw_spec("analysis_fence_change_vs_diversification", "Analysis: higher fences and lower coercive dependence rarely coincide", "d3-scatter-plot",
    data.frame(Cell = paste(TFE_DW_TECH[fv$tech], tolower(fv$stage)), `Change in TRI (pts)` = r1(fv$tri_change_pts),
               `Change in coercive-supplier share (pts)` = r1(fv$coercive_share_change_pts), `Imports 2024 ($bn)` = bn(fv$imports_2024),
               Archetype = fv$archetype, check.names = FALSE),
    "Technology x stage cells: change in all-supplier Trade Restrictiveness Index (2024 rates to August 2026) against change in coercive-supplier share (January-August 2024 to 2026).",
    paste("Analytical exploration, not for publication. Archetype lines: +10 points of TRI, -5 points of coercive share.", TFE_DW_CAUSAL),
    axes = list(x = "Change in TRI (pts)", y = "Change in coercive-supplier share (pts)", size = "Imports 2024 ($bn)", color = "Archetype", labels = "Cell"),
    visualize = list(`show-color-key` = TRUE, size = "dynamic", `max-size` = 35)))
  out
}
