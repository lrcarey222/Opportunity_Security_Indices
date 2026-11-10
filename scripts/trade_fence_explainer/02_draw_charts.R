source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))
    candidate_starts <- unique(c(candidate_starts, getwd()))
    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))

# Draws the eight explainer charts (PNG at 300 dpi and SVG, 6.5 inches wide) from the CSVs that
# 01_build_data.R writes, plus the editorial contact sheet (contact_sheet.html). Titles and subtitles quote the
# numbers in the CSVs, so a refresh updates them; the headline wording is editorial and is checked against the
# data in TRADE_FENCE_EXPLAINER_CHARTS.md.

source(file.path(repo_root, "R", "trade_fence_explainer", "explainer_data.R"))
source(file.path(repo_root, "R", "trade_fence_explainer", "explainer_charts.R"))
source(file.path(repo_root, "R", "trade_fence_explainer", "trade_overview_charts.R"))
config <- getOption("opportunity_security.config")
cfg <- yaml::read_yaml(Sys.getenv("OPSI_TRADE_FENCE_EXPLAINER", file.path(repo_root, "config", "trade_fence_explainer.yml")))
abs_path <- function(p) if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(repo_root, p)
out_dir <- abs_path(cfg$output_dir)
data_dir <- file.path(out_dir, "data")
fig_dir <- out_dir
rd <- function(f) utils::read.csv(file.path(data_dir, f), check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
has <- function(f) file.exists(file.path(data_dir, f))
info <- rd("build_info.csv")
I <- stats::setNames(info$value, info$item)

# --- fonts: Roboto (the NEIS dashboard face) when available, else Arial ---------------------------------------
font_dir <- file.path(repo_root, config$raw_data_dir, "fonts", "roboto")
roboto <- file.path(font_dir, paste0("Roboto-", c("Regular", "Bold", "Italic"), ".ttf"))
if (!all(file.exists(roboto)) && tolower(Sys.getenv("OPSI_SKIP_FETCH", "false")) != "true") {
  dir.create(font_dir, recursive = TRUE, showWarnings = FALSE)
  for (w in c("Regular", "Bold", "Italic")) {
    try(utils::download.file(sprintf("https://raw.githubusercontent.com/googlefonts/roboto/main/src/hinted/Roboto-%s.ttf", w),
                             file.path(font_dir, paste0("Roboto-", w, ".ttf")), mode = "wb", quiet = TRUE), silent = TRUE)
  }
}
fam <- "Arial"
if (all(file.exists(roboto))) {
  systemfonts::register_font("Roboto", plain = roboto[1], bold = roboto[2], italic = roboto[3], bolditalic = roboto[2])
  fam <- "Roboto"
}
sym <- if ("Segoe UI Symbol" %in% systemfonts::system_fonts()$family) "Segoe UI Symbol" else fam

W <- 6.5
save_chart <- function(p, name, h, svg = TRUE) {
  ggplot2::ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = W, height = h, dpi = 300, device = ragg::agg_png, bg = "#ffffff")
  if (svg) ggplot2::ggsave(file.path(fig_dir, paste0(name, ".svg")), p, width = W, height = h, device = svglite::svglite, bg = "#ffffff")
  invisible(name)
}
pc <- function(x, d = 1) formatC(x, format = "f", digits = d)
src_tri <- "Source: NEIS analysis of US Census Bureau imports for consumption and calculated duty by HTS-10 line and partner (via the Trade War Tracker, github.com/tradewartracker), Commerce AD/CVD notices in the Federal Register."
note_tri <- "Trade Restrictiveness Index (Waugh): the uniform tariff with the same welfare cost as the actual tariff structure, sqrt(sum w*t^2 / sum w) on 2024 import weights. It rises with dispersion as well as level; it is not an average tariff and should not be multiplied by imports to estimate revenue."
blocs <- "Supplier groups (NEIS posture engine): trusted = US-leaning UN-voting bloc (incl. EU, UK, Japan, Korea, Canada, Australia); coercive = China, Hong Kong, Macao, Russia and the China-leaning UN-voting bloc (incl. Vietnam and Indonesia); nonaligned = the rest (incl. Mexico, India, Taiwan, Thailand, Malaysia, Singapore, Switzerland)."
charts <- list()
reg <- function(file, title, h, svg = TRUE, section = "publication", order = 99) {
  charts[[length(charts) + 1]] <<- list(file = file, title = title, h = h, svg = svg, section = section, order = order)
}
save_grob <- function(g, name, h, dir = fig_dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (ext in c("png", "svg")) ggplot2::ggsave(file.path(dir, paste0(name, ".", ext)), g, width = W, height = h, dpi = 300,
                                               device = if (ext == "png") ragg::agg_png else svglite::svglite, bg = "#ffffff")
}
txt_grob <- function(x, size, face = "plain", col = TFE_INK[["primary"]], width = 98)
  grid::textGrob(paste(vapply(x, tfe_wrap, character(1), width = width), collapse = "\n"), x = 0.025, hjust = 0,
                 gp = grid::gpar(fontfamily = fam, fontface = face, fontsize = size, col = col, lineheight = 1.2))
src_trade <- "Sources: US Census Bureau imports for consumption by HTS-10 line and partner (via the Trade War Tracker); US exports by HS6 (Census data as reported to UN Comtrade); NEIS electro-industrial HS6 basket (data/reference/energy_hs6_master.csv)."


# --- chart 00a: what America trades -------------------------------------------------------------------------------
if (has("00a_trade_by_technology.csv")) {
  bt <- rd("00a_trade_by_technology.csv")
  bt <- bt[!is.na(bt$imports_2026), ]
  bs0 <- rd("00a_trade_by_stage.csv")
  ovt <- rd("00a_electro_industrial_trade_overview.csv")
  tt <- ovt[ovt$level == "total", ]
  ta <- tt[grepl("^All", tt$key), ]; tc <- tt[grepl("^Clean", tt$key), ]
  cl <- bt[bt$group == "Clean energy and grid", ]
  cl <- cl[order(-cl$imports_2026), ]
  dep <- cl[order(-cl$import_export_ratio_2026), ]
  stc <- bs0[grepl("^Clean", bs0$basket), ]
  asm <- stc$import_share_of_basket[stc$stage == "Assembly"]; cmpn <- stc$import_share_of_basket[stc$stage == "Components"]
  pa0 <- tfe_chart_00a_tech(bt, fam) + ggplot2::labs(title = "A. Imports and exports by technology, January-August 2026") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 11))
  pb0 <- tfe_chart_00a_stage(bs0, fam) + ggplot2::labs(title = "B. Imports and exports by supply-chain stage, January-August 2026") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 11))
  h0 <- txt_grob("America's electro-industrial trade deficit sits in a few technologies, and mostly at assembly", 14.3, "bold", width = 58)
  s0 <- txt_grob(sprintf("In January-August 2026 the US imported $%sbn of clean-energy and grid goods and exported $%sbn (%s to 1). Imports are largest in electric vehicles, grid and solar; relative to exports they are highest in batteries (%s to 1) and nuclear (%s to 1). %s%% of clean-energy and grid imports are assembled goods and %s%% components; the US is a net exporter of fuels and of raw materials.",
                         pc(tc$imports_2026 / 1e9, 0), pc(tc$exports_2026 / 1e9, 0), pc(tc$imports_2026 / tc$exports_2026, 1),
                         pc(bt$import_export_ratio_2026[bt$tech == "Batteries"], 1), pc(bt$import_export_ratio_2026[bt$tech == "Nuclear"], 1),
                         pc(100 * asm, 0), pc(100 * cmpn, 0)), 9.6, col = TFE_INK[["secondary"]])
  c0 <- txt_grob(c(
    "Technology totals are attributed: a product in several technologies (static converters, switchgear, cable) counts in each, so technology rows overlap; stage totals count each HS6 product once (split equally where it sits in different stages) and exclude Deployment, which repeats Assembly. Net balance (bold) = exports - imports. Geothermal and green hydrogen have no defensible traded HS6 basket and are omitted.",
    sprintf("All technologies together: imports $%sbn (%s%% vs January-August 2024), exports $%sbn (%s%%). A trade deficit is not by itself a strategic weakness.",
            pc(ta$imports_2026 / 1e9, 0), sprintf("%+.1f", ta$imports_change_pct), pc(ta$exports_2026 / 1e9, 0), sprintf("%+.1f", ta$exports_change_pct)),
    src_trade), 7.1, col = TFE_INK[["muted"]], width = 134)
  g0a <- gridExtra::arrangeGrob(h0, s0, ggplot2::ggplotGrob(pa0), ggplot2::ggplotGrob(pb0), c0, ncol = 1,
                                heights = grid::unit(c(0.66, 0.95, 4.5, 2.5, 1.25), "in"))
  save_grob(g0a, "00a_electro_industrial_trade_overview", 9.9)
  reg("00a_electro_industrial_trade_overview", "1. What America trades in the electro-industrial stack", 9.9, order = 1)
}

# --- chart 00b: how sourcing changed ---------------------------------------------------------------------------------
if (has("00b_trade_rewiring.csv")) {
  b0 <- rd("00b_trade_rewiring.csv")
  b0p <- b0[!b0$tech %in% c("Oil", "Gas", "Coal", "Refined copper cathodes"), ]
  cnb <- b0[b0$tech == "Batteries", ]
  n_down <- sum(b0p$china_share_change_pts < 0 & b0p$kind == "Technology")
  p0b <- tfe_chart_00b(b0p, fam) + tfe_labs(
    "US electro-industrial trade is being rewired, not simply reduced",
    sprintf("Change in US imports and in China's share of them, January-August 2024 to 2026; bubble size is 2026 imports. Imports grew in five of six clean-energy and grid technologies while China's share fell in all %d; batteries are the exception on volume, with China's share down from %s%% to %s%% as imports fell %s%%. Rare earths and magnets remain about 70%% Chinese.",
            n_down, pc(100 * cnb$china_share_2024, 0), pc(100 * cnb$china_share_2026, 0), pc(abs(cnb$imports_change_pct), 0)),
    c("A falling China share is not by itself a success: it can reflect rerouting through third countries, Chinese-owned plants abroad, or a smaller market. Technologies are their producing-stage HS6 baskets (overlapping); product groups are HS6 sets (squares). Oil, gas, coal and refined copper are left off the chart (China shares near zero; copper imports +225%) and are in 00b_trade_rewiring.csv.",
      "China = imports recorded from China; Hong Kong and Macao are not separately available and sit in the rest of world. Technology x stage detail, coercive, trusted, nonaligned, North American and Southeast Asian shares and concentration (HHI, top-three share) are in 00b_trade_diversification_metrics.csv.",
      TFE_CAUSAL_NOTE, src_trade))
  save_chart(p0b, "00b_trade_rewiring", 6.2)
  reg("00b_trade_rewiring", "2. How US electro-industrial sourcing is changing", 6.2, order = 2)
}

# --- chart 00c: policy timeline ---------------------------------------------------------------------------------------
if (has("00_policy_timeline.csv")) {
  tl0 <- rd("00_policy_timeline.csv")
  win <- list(start = c("2024-01-01", "2026-01-01"), end = c("2024-08-31", "2026-08-31"), label = c("Jan-Aug 2024", "Jan-Aug 2026"),
              latest = paste0(I[["data_month"]], "-28"))
  p0c <- tfe_chart_00c(tl0, win, fam) + tfe_labs(
    "The policy environment changed fast between the two trade windows",
    "Key US measures for the electro-industrial stack, January 2024 to the 4 December 2026 snapshot; shading marks the January-August windows compared in the trade charts. Dashed stems: after the latest Census month.",
    c(TFE_CAUSAL_NOTE, "Full list with sources (Federal Register, HTSUS; 2025 IEEPA steps from the Trade War Tracker registry): data/00_policy_timeline.csv."), ws = 100)
  save_chart(p0c, "00c_policy_timeline", 3.9)
  reg("00c_policy_timeline", "2b. Policy timeline (pairs with chart 2)", 3.9, order = 3)
}

# --- analysis: fence change vs diversification ------------------------------------------------------------------------
if (has("fence_change_vs_diversification.csv")) {
  fv <- rd("fence_change_vs_diversification.csv")
  fc <- rd("fence_change_vs_diversification_correlation.csv")
  ov_cfg <- cfg$trade_overview
  ar <- table(fv$archetype)
  pf <- tfe_chart_fence_vs_div(fv, ov_cfg$fence_line, ov_cfg$dependence_line, fam) + tfe_labs(
    "Higher fences and lower coercive dependence rarely coincide",
    sprintf("Technology x stage cells (Deployment excluded), bubble size 2024 imports. %d cells raised the fence by %d points or more; %d of them also cut the coercive-supplier share by %d points or more. Correlation %s (unweighted), %s (import-weighted).",
            sum(fv$tri_change_pts >= ov_cfg$fence_line), ov_cfg$fence_line, sum(ar[grepl("^Higher fence \\+ lower", names(ar))]), abs(ov_cfg$dependence_line),
            pc(fc$correlation[fc$measure == "unweighted"], 2), pc(fc$correlation[fc$measure == "weighted_2024_imports"], 2)),
    c("Analytical exploration, not a publication chart. Dashed lines: the archetype cut-offs (fence change 10 points; coercive share -5 points). Fence change: all-supplier TRI at 2024 rates vs August 2026 Census duty (both excl. AD/CVD).", TFE_CAUSAL_NOTE, src_trade))
  dir.create(file.path(out_dir, "analysis"), showWarnings = FALSE)
  ggplot2::ggsave(file.path(out_dir, "analysis", "fence_change_vs_diversification.png"), pf, width = W, height = 5.6, dpi = 300, device = ragg::agg_png, bg = "#ffffff")
  reg("analysis/fence_change_vs_diversification", "Analysis: fence change vs coercive dependence", 5.6, svg = FALSE, section = "appendix", order = 30)
}

# --- chart 1 ----------------------------------------------------------------------------------------------------
d1 <- rd("01_fence_rising.csv")
g <- function(x, col) d1[[col]][d1$supplier_group == x]
mth <- format(as.Date(paste0(I[["data_month"]], "-01")), "%B %Y")
snap_txt <- sub("^0", "", format(as.Date(I[["snapshot_date"]]), "%d %B %Y"))
p1 <- tfe_chart_01(d1, fam) + tfe_labs(
  "The US raised the fence against China, and sharply raised the floor under its allies",
  sprintf("Trade Restrictiveness Index on all US goods imports, 2024 (open circle) and %s (filled). Trusted suppliers went from %s%% to %s%% (+%s points); China from %s%% to %s%% (+%s points). Trusted suppliers now face a slightly higher index than nonaligned ones.",
          mth, pc(g("Trusted", "tri_2024")), pc(g("Trusted", "tri_latest")), pc(g("Trusted", "change_points")),
          pc(g("China", "tri_2024")), pc(g("China", "tri_latest")), pc(g("China", "change_points"))),
  c(if ("tri_dec4_modeled" %in% names(d1)) sprintf("Statutory regime of %s (observed rates plus statutory changes since mid-August, modelled): trusted %s%%, nonaligned %s%%, coercive %s%%, China %s%%, all suppliers %s%%.",
                                                   snap_txt, pc(g("Trusted", "tri_dec4_modeled")), pc(g("Nonaligned", "tri_dec4_modeled")), pc(g("Coercive", "tri_dec4_modeled")),
                                                   pc(g("China", "tri_dec4_modeled")), pc(g("All suppliers", "tri_dec4_modeled"))),
    paste("Observed rates are Census calculated duty / customs value; AD/CVD deposits are not in Census duty and are excluded here.", note_tri), blocs, src_tri))
save_chart(p1, "01_fence_rising", 4.7)
reg("01_fence_rising", "3. The fence has risen sharply, including against allies", 4.7, order = 4)

# --- chart 2 ----------------------------------------------------------------------------------------------------
d2 <- rd("02_tariff_map.csv")
techs <- unlist(cfg$techs)
hi <- d2[d2$in_basket %in% TRUE & d2$stage != "Deployment", ]
top <- hi[order(-hi$tri_all), ][1:3, ]
p2 <- tfe_chart_02(d2, techs, fam) + tfe_labs(
  "Where has America actually built tariff walls?",
  sprintf("Trade Restrictiveness Index by technology and supply-chain stage, all suppliers, %s Census duty plus AD/CVD cash deposits. The highest walls stand at %s; extraction and fuel refining are mostly open.",
          mth, paste(sprintf("%s %s (%s%%)", tolower(TFE_TECH_LABEL[top$tech]), tolower(top$stage), pc(top$tri_all, 0)), collapse = ", ")),
  c("* Deployment has no traded goods of its own: it is scored on the technology's Assembly basket (the goods the build-out imports), so it repeats the Assembly value.",
    "Italic value with a dot: thin cell (under $0.5bn of 2024 imports). –: no traded basket goods at this stage. AD/CVD lines are capped at 100%.",
    note_tri, src_tri))
save_chart(p2, "02_tariff_map", 5.9)
reg("02_tariff_map", "4. Where the tariff wall sits", 5.9, order = 5)

# --- chart 3 ----------------------------------------------------------------------------------------------------
d3 <- rd("03_who_gets_through_gate.csv")
g3 <- rd("03_group_rates.csv")
gr <- function(x) g3$effective_rate[g3$group == x]
cn <- d3$effective_rate[d3$iso3 == "CHN"]
gap_tn <- gr("Trusted") - gr("Nonaligned")
headline3 <- if (cn > 2 * max(gr("Trusted"), gr("Nonaligned")) && gap_tn > -3) {
  "The US has a China gate. It does not yet have a coherent trusted-partner gate"
} else "Does US tariff treatment follow strategic alignment?"
p3 <- tfe_chart_03(d3, g3, fam) + tfe_labs(
  headline3,
  sprintf("Effective tariff (duties collected / imports, all goods, June-August 2026) against each partner's position on the US-China axis of UN votes; bubble size is 2024 two-way goods trade. China pays %s%%. Trusted partners pay %s%% on average, more than nonaligned partners (%s%%).",
          pc(cn), pc(gr("Trusted")), pc(gr("Nonaligned"))),
  c(sprintf("Averages are import-weighted over the %d partners plotted (the Trade War Tracker's 30 largest suppliers less Taiwan, which has no UN vote). Coercive excluding China (Vietnam, Indonesia, Russia): %s%%. Effective rates exclude AD/CVD deposits.",
            sum(d3$plotted %in% TRUE), pc(gr("Coercive excl. China"))), blocs,
    "Sources: US Census Bureau (via the Trade War Tracker); UN Comtrade 2024 US goods trade by partner; UN General Assembly voting ideal points (Bailey, Strezhnev and Voeten), 2019-2023 sessions, as used by the NEIS alignment measure."))
save_chart(p3, "03_who_gets_through_gate", 5.5)
reg("03_who_gets_through_gate", "Sidebar: who gets through the gate (all goods, UN-voting blocs)", 5.5, section = "appendix", order = 20)

# --- chart 4 ----------------------------------------------------------------------------------------------------
d4 <- rd("04_multi_tool_fence.csv")
flags <- d4[d4$dec4_price_floor %in% TRUE, c("tech", "stage", "tool")]
p4 <- tfe_chart_04(d4, fam, flags) + tfe_labs(
  "America's new trade regime is not really just a tariff regime",
  "Relative intensity of each US policy tool at each stage of three supply chains: how often the tool falls on the stage's products compared with the same tool across all US imports (High = top 10% of traded products, Medium = next 30%, Low = the rest). At assembly, tariffs rank Low or Medium while domestic-market rules rank High in all three chains.",
  c("Bands compare each tool with itself, not tools with each other: a High public-purchasing cell means products unusually often named by procurement rules, not market closure equal to a High tariff cell. For non-tariff tools the bands count NIPO measures, not their economic restrictiveness.",
    "† The band rests on a single NIPO measure. Red triangle: from 4 December 2026 a Section 232 minimum import price and 15% tariff (Proclamation 11052) also apply to polysilicon, wafers, cells and modules; they are not in the tariff band, which uses August 2026 Census duty (excluding AD/CVD).",
    "Tariffs: rank of the stage's import-weighted applied rate. Other tools: import-weighted mean percentile of the stage's products by the number of NIPO measures in force on 31 October 2026 naming each HS6 code; measures naming over 500 codes (sanctions lists) are excluded and trade-defence scopes are checked against the orders. A measure named on a product may bite on part of its trade only (one firm, partner or end use).",
    "Assembly and Deployment share one basket. Sources: Global Trade Alert New Industrial Policy Observatory (October 2026 extract); US Census Bureau; NEIS US_Fence_Tools_by_Stage.xlsx."), ws = 96)
save_chart(p4, "04_multi_tool_fence", 8.3)
reg("04_multi_tool_fence", "5. The fence uses more than tariffs", 8.3, order = 6)

# --- chart 5 and 5b -----------------------------------------------------------------------------------------------
d5 <- rd("05_yards_fences_audit.csv")
d5b <- rd("05b_verdict_summary.csv")
n <- stats::setNames(d5b$cells_observed, d5b$verdict)
tot <- sum(n)
dec4_line <- if ("cells_dec4" %in% names(d5b)) {
  ch <- sum(d5$verdict_changed_dec4 %in% TRUE)
  if (ch == 0) sprintf("Re-run on the statutory regime of %s (incl. Proclamation 11052 and the new AD/CVD deposits): no verdict changes.", snap_txt)
  else sprintf("On the statutory regime of %s, %d cell(s) change verdict (outlined).", snap_txt, ch)
} else ""
thr <- if (has("05_threshold_sensitivity.csv")) rd("05_threshold_sensitivity.csv") else NULL
thr_line <- if (!is.null(thr)) paste0(" Aligned cells under other open-gate lines: ", paste(sprintf("%s%% → %d of %d", thr$open_threshold, thr$Aligned, thr$assessed_cells), collapse = "; "),
                                      ". Misalignment remains substantial, but the count depends on where “open” is drawn.") else ""
p5 <- tfe_chart_05(d5, techs, fam, sym) + tfe_labs(
  sprintf("Only %d of %d electro-industrial tariff fences match the strategic posture Yards & Fences recommends", n[["Aligned"]], tot),
  sprintf("Each cell compares the tariff fence (%s Census duty plus AD/CVD) with the recommended choice written in it: %d aligned, %d tax trusted partners, %d sit above the recommended height and %d is below it. %s",
          mth, n[["Aligned"]], n[["Taxes trusted partners"]], n[["Fence above recommendation"]], n[["Fence below recommendation / missing"]], paste0(dec4_line, thr_line)),
  c("Rules (Trade Restrictiveness Index): Secure needs 15% or more on coercive suppliers and under 5% on trusted ones. Preserve, Catch Up, Partner and Accelerate keep the gate open: trusted partners under 5%, so a cell that fences coercive suppliers is still misaligned if trusted partners pay 5% or more. Watch implies a low fence: all suppliers under 5%.",
    "Not assessed: Wind extraction (no posture) and Nuclear recovery (no trade). –: no traded basket goods. Deployment is scored on the Assembly basket.", src_tri))
save_chart(p5, "05_yards_fences_audit", 6.5)
reg("05_yards_fences_audit", "6. Does the fence match the strategy? (cells)", 6.5, order = 7)
p5b <- tfe_chart_05b(d5b, fam, sym) + tfe_labs(
  sprintf("%d of %d US electro-industrial tariff fences match the strategy", n[["Aligned"]], tot),
  "Technology x supply-chain stage cells by verdict: US tariffs (Trade Restrictiveness Index incl. AD/CVD) against the Yards & Fences posture.",
  c(sprintf("Observed %s Census duty plus AD/CVD cash deposits; open gate under 5%%, high fence 15%% and over. Unchanged on the %s statutory regime.", mth, snap_txt),
    "Source: NEIS analysis of US Census Bureau and Federal Register data."), wt = 44, ws = 80)
save_chart(p5b, "05b_verdict_summary", 4.3, svg = TRUE)
reg("05b_verdict_summary", "Social: verdict summary", 4.3, section = "appendix", order = 21)

# --- chart 6 ----------------------------------------------------------------------------------------------------
d6 <- rd("06_dollar_weighted_misalignment.csv")
d6o <- d6[d6$basis == "dedup_equal_split" & grepl("^observed", d6$verdict_basis), ]
sh <- function(s, v) d6o$share[d6o$suppliers == s & d6o$verdict == v]
gross <- d6[d6$basis == "gross" & grepl("^observed", d6$verdict_basis) & d6$suppliers == "All suppliers", ]
mis_all <- 1 - sh("All suppliers", "Aligned")
mis_tr <- 1 - sh("Trusted partners", "Aligned")
cl_al <- sh("All suppliers (excl. oil, gas, coal)", "Aligned")
p6 <- tfe_chart_06(d6o, fam) + tfe_labs(
  sprintf("Outside oil, gas and coal, only %s%% of the imports behind these fences meet an aligned one", pc(100 * cl_al, 0)),
  sprintf("2024 US imports in the assessed basket by the verdict of the cells they sit in, each HS6 product counted once. Across all technologies %s%% sit behind a misaligned fence (%s%% of trusted-partner imports); the aligned dollars are mostly crude oil and refined products. In clean-energy and grid technologies %s%% sit in cells that tax trusted partners, and %s%% of what those technologies import from trusted partners.",
          pc(100 * mis_all, 0), pc(100 * mis_tr, 0), pc(100 * sh("All suppliers (excl. oil, gas, coal)", "Taxes trusted partners"), 0),
          pc(100 * sh("Trusted partners (excl. oil, gas, coal)", "Taxes trusted partners"), 0)),
  c("A product in several technology baskets is split equally across the producing-stage cells it sits in; Deployment cells, which repeat the Assembly basket, are left out of the dollar totals. 06_dollar_weighted_misalignment.csv also gives the gross cell sum, the sum without Deployment and a variant that gives Assembly goods the Deployment verdict.",
    sprintf("Gross cell-level exposure, counting overlaps and Deployment: %s. Verdicts are the observed %s fences of chart 5.", tfe_bn(sum(gross$imports_usd)), mth), src_tri))
save_chart(p6, "06_dollar_weighted_misalignment", 7.3)
reg("06_dollar_weighted_misalignment", "6b. Does the fence match the strategy? (dollars)", 7.3, order = 8)

# --- chart 7 ----------------------------------------------------------------------------------------------------
if (has("07_why_allies_pay.csv")) {
  d7 <- rd("07_why_allies_pay.csv")
  f7 <- d7[d7$scenario == "Dec 4 statutory (all measures)", ]
  dropf <- function(sc) { x <- d7[d7$scenario == sc, ]; f7$tri - x$tri[match(f7$key, x$key)] }
  half232 <- sum(dropf("Without Section 232") >= 0.5 * f7$tri)
  d338 <- dropf("Without Section 338")
  p7 <- tfe_chart_07(d7, fam) + tfe_labs(
    "Steel, aluminium and copper tariffs explain most of what trusted partners pay for clean-energy equipment",
    sprintf("Trusted-partner Trade Restrictiveness Index on the statutory regime of %s, and with one authority switched off at a time. Without the Section 232 metals, autos and polysilicon duties the index would fall by half or more in %d of %d cells. Section 338 duties on Canada add a further %s-%s points in wind, solar and grid assembly since September.",
            snap_txt, half232, nrow(f7), pc(min(d338[d338 > 1]), 0), pc(max(d338), 0)),
    c("Each scenario starts from observed August 2026 duty and adds the statutory change to 4 December modelled by the Trade War Tracker tariff-action engine, re-run with every action of one authority removed (without Section 232, those goods become liable to the Section 301 forced-labour duty, as they would be). No line falls below its 2024 rate. AD/CVD: all-others deposits in force on 4 December, capped at 100%. 2024 duty rates exclude AD/CVD.",
      "Grey tick: observed August 2026 (Census duty plus AD/CVD). Section 338: the tracker's Canada basket was checked line by line against HTSUS Revision 23 (2026), chapter 99 U.S. note 51(b)(3) (all 591 codes listed, incl. 7308.90 structures, 8537.10.91 switchgear, 8544.49.30 conductors); note 51(c) makes the duty stack on Section 232. The 338 effect is modelled (after the latest Census month), so it is shown as a secondary result. A pure-engine version is in the CSV.",
      "Trade Restrictiveness Index: the uniform tariff with the same welfare cost as the actual tariffs (not an average tariff). Source: NEIS analysis using the Trade War Tracker engine (github.com/tradewartracker/trade-war-redux-2025), US Census Bureau, Federal Register."))
  save_chart(p7, "07_why_allies_pay", 6.3)
  reg("07_why_allies_pay", "7. Why trusted partners are getting caught", 6.3, order = 9)
}

# --- chart 8 ----------------------------------------------------------------------------------------------------
d8a <- rd("08a_solar_trade_response.csv")
ev <- if (has("08_policy_events.csv")) rd("08_policy_events.csv") else data.frame()
ev <- ev[ev$panel == "solar", ]
ev$future <- toupper(ev$future) == "TRUE"
ev <- ev[order(ev$date), ]
pa <- tfe_chart_08a(d8a, ev, fam) + ggplot2::labs(title = "A. Solar: crystalline-silicon module imports by origin")
dd <- if (has("D_domestic_response.csv")) rd("D_domestic_response.csv") else data.frame()
cap <- dd[dd$topic %in% "solar_module_capacity_operating" & grepl("SEIA/Wood Mackenzie, US Solar Market Insight", dd$source_name) &
            !grepl("as first", dd$period), ]
cap$p <- sub(" \\(.*", "", cap$period)
cap <- cap[!duplicated(cap$p), ]
cap$label <- c(`end-2023` = "End\n2023", `end-2024` = "End\n2024", `end-2025` = "End\n2025", `end-Q2 2026` = "Mid-\n2026")[cap$p]
cap <- cap[!is.na(cap$label), ]
pd <- tfe_chart_08_domestic(cap, fam) + ggplot2::labs(title = "US module factories", subtitle = "Operating capacity
at year end, GW")
d8b <- rd("08b_battery_trade_response.csv")
own <- if (has("08b_battery_ownership.csv")) rd("08b_battery_ownership.csv") else NULL
own_txt <- ""
if (!is.null(own)) {
  o <- own[own$status_group == "All projects" & own$destination_iso3 %in% c("MYS", "THA"), ]
  own_txt <- paste0("Country of origin is not ownership: ", paste(sprintf("in %s, %d of %d battery-manufacturing FDI projects have a Chinese parent (%s%% of disclosed capital)",
                           c(MYS = "Malaysia", THA = "Thailand")[o$destination_iso3], o$n_chinese_parent, o$n_projects,
                           pc(100 * o$chinese_parent_share_capital, 0)), collapse = "; "), ".")
}
pb <- tfe_chart_08b(d8b, fam) + ggplot2::labs(title = "B. Batteries: lithium-ion imports by origin", subtitle = tfe_wrap(own_txt, 100)) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8.6))
cn24 <- d8b$share_2024[d8b$origin == "China"]; cn26 <- d8b$share_2026[d8b$origin == "China"]
ytd <- function(y) d8a$value[substr(d8a$time, 1, 4) == y & substr(d8a$time, 6, 7) %in% unlist(cfg$response_months)]
mod24 <- sum(ytd("2024")); mod26 <- sum(ytd("2026"))
o26 <- stats::aggregate(value ~ origin, data = d8a[substr(d8a$time, 1, 4) == "2026", ], FUN = sum)
o26 <- o26[order(-o26$value), ]
lead26 <- paste(head(o26$origin[o26$origin != "All other"], 3), collapse = ", ")
head8 <- grid::textGrob(tfe_wrap("The same fence can produce very different forms of diversification", 58), x = 0.025, hjust = 0,
                        gp = grid::gpar(fontfamily = fam, fontface = "bold", fontsize = 14.3, col = TFE_INK[["primary"]], lineheight = 1.05))
sub8 <- grid::textGrob(tfe_wrap(sprintf("Module imports fell %s%% (January-August 2026 vs 2024) and moved from the four countries hit by the 2025 orders to new sources, led in 2026 by %s (estimated split); US module capacity (not output) rose, but almost no cells are made at home. China's share of lithium-ion imports fell from %s%% to %s%%, mostly to Japan and Korea; Malaysia and Thailand, where most recent battery FDI has Chinese parents, also gained. Origin diversified more than ownership.",
                                         pc(100 * (1 - mod26 / mod24), 0), lead26, pc(100 * cn24, 0), pc(100 * cn26, 0)), 98), x = 0.025, hjust = 0,
                       gp = grid::gpar(fontfamily = fam, fontsize = 9.6, col = TFE_INK[["secondary"]], lineheight = 1.2))
evkey <- paste(sprintf("(%d) %s, %s", seq_len(nrow(ev)), gsub("\n", " ", ev$label), format(as.Date(ev$date), "%d %b %Y")), collapse = "; ")
cap8 <- grid::textGrob(paste(vapply(c(
  paste0("Panel A events: ", evkey, " (dashed: after the latest Census month, August 2026)."),
  "Panel A: US imports for consumption of HTS 8541.43.0010 (crystalline-silicon modules) from Census; imports from countries outside the Trade War Tracker's 30 partners (Cambodia, Laos, the Philippines, Africa) are the Census remainder split by each country's monthly share of US imports of HS 8541.43 in UN Comtrade. Africa: mainly Ethiopia, Kenya and Nigeria. Capacity: operating nameplate module capacity (SEIA/Wood Mackenzie, secondary source), not output; production was about 12 GW in 2024 and 31 GW in 2025 (Wood Mackenzie via NREL); crystalline-silicon cell capacity was about 3 GW in mid-2026.",
  "Panel B: HTS 8507.60 (cells and packs cannot be separated in trade data). Ownership: NEIS CM-FDI project database (battery manufacturing projects, all statuses, capital weighted by project attribution); it covers projects announced since about 2017, not the whole installed base.",
  "Sources: US Census Bureau (via the Trade War Tracker); UN Comtrade; SEIA/Wood Mackenzie US Solar Market Insight; NREL Solar Industry Updates; Federal Register; NEIS CM-FDI database."), tfe_wrap, character(1), width = 134), collapse = "\n"),
  x = 0.025, hjust = 0, gp = grid::gpar(fontfamily = fam, fontsize = 7.1, col = TFE_INK[["muted"]], lineheight = 1.2))
top_row <- gridExtra::arrangeGrob(ggplot2::ggplotGrob(pa), ggplot2::ggplotGrob(pd), ncol = 2, widths = c(3.25, 1))
fig8 <- gridExtra::arrangeGrob(head8, sub8, top_row, ggplot2::ggplotGrob(pb), cap8, ncol = 1,
                               heights = grid::unit(c(0.42, 0.78, 3.9, 3.3, 1.85), "in"))
for (ext in c("png", "svg")) {
  ggplot2::ggsave(file.path(fig_dir, paste0("08_trade_response.", ext)), fig8, width = W, height = 10.4, dpi = 300,
                  device = if (ext == "png") ragg::agg_png else svglite::svglite, bg = "#ffffff")
}
reg("08_trade_response", "8. What happens when the fence rises", 10.4, order = 10)

# --- contact sheet ------------------------------------------------------------------------------------------------
charts <- charts[order(vapply(charts, function(x) x$section != "publication", logical(1)), vapply(charts, function(x) x$order, numeric(1)))]
pub <- charts[vapply(charts, function(x) x$section == "publication", logical(1))]
app <- charts[vapply(charts, function(x) x$section != "publication", logical(1))]
card <- function(x) sprintf(
  '<figure><figcaption>%s</figcaption><img src="%s.png" alt="%s" loading="lazy"><p class="links"><a href="%s.png">PNG</a>%s<a href="data/">CSV folder</a></p></figure>',
  x$title, x$file, x$title, x$file, if (x$svg) sprintf(' <a href="%s.svg">SVG</a> ', x$file) else " ")
cards_pub <- vapply(pub, card, character(1))
cards_app <- vapply(app, card, character(1))
cards <- vapply(charts, function(x) sprintf(
  '<figure><figcaption>%s</figcaption><img src="%s.png" alt="%s" loading="lazy"><p class="links"><a href="%s.png">PNG</a>%s<a href="data/">CSV folder</a></p></figure>',
  x$title, x$file, x$title, x$file, if (x$svg) sprintf(' <a href="%s.svg">SVG</a> ', x$file) else " "), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
          '<title>Trade fence explainer charts</title><style>',
          ':root{--ink:#111827;--ink2:#374151;--mute:#6b7280;--line:#e5e7eb;--bg:#ffffff;--card:#f9fafb}',
          '@media (prefers-color-scheme: dark){:root:not([data-theme="light"]){--ink:#f9fafb;--ink2:#d1d5db;--mute:#9ca3af;--line:#374151;--bg:#111827;--card:#1f2937}}',
          'body{margin:0;background:var(--bg);color:var(--ink);font:15px/1.5 Roboto,system-ui,sans-serif}',
          'main{max-width:1500px;margin:0 auto;padding:24px 16px}h1{font-size:24px;margin:0 0 4px}h2{font-size:18px}p.lede{color:var(--ink2);margin:0 0 20px;max-width:900px}',
          '.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(min(100%,640px),1fr));gap:20px}',
          'figure{margin:0;background:var(--card);border:1px solid var(--line);border-radius:8px;padding:12px}',
          'figcaption{font-weight:700;margin-bottom:8px}img{width:100%;height:auto;background:#fff;border-radius:4px;display:block}',
          '.links{font-size:13px;color:var(--mute);margin:6px 0 0}.links a{color:inherit;margin-right:10px}',
          '.mobile .grid{grid-template-columns:repeat(auto-fill,375px)}</style></head><body><main>',
          '<h1>US trade fences across the electro-industrial stack: chart contact sheet</h1>',
          sprintf('<p class="lede">Editorial review copy. Observed tariffs: Census duty through %s. Statutory snapshot: %s. Built %s by scripts/trade_fence_explainer/ (see README.md and TRADE_FENCE_EXPLAINER_CHARTS.md).</p>',
                  mth, snap_txt, format(Sys.Date())),
          '<h2>Recommended publication sequence</h2><div class="grid">', cards_pub, '</div>',
          '<h2>Appendix, sidebar and social graphics</h2><div class="grid">', cards_app, '</div>',
          '<section class="mobile"><h2>At phone width (375 px)</h2><div class="grid">',
          vapply(charts, function(x) sprintf('<figure><figcaption>%s</figcaption><img src="%s.png" alt="%s" loading="lazy"></figure>', x$file, x$file, x$title), character(1)),
          '</div></section></main></body></html>')
writeLines(html, file.path(out_dir, "contact_sheet.html"), useBytes = TRUE)
message("Drew ", length(charts), " charts into ", out_dir, " (font: ", fam, ").")
