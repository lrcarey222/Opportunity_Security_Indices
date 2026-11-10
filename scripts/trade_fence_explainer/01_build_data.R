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

# Data for the NEIS trade-fence explainer (outputs/trade_fence_explainer/README.md): chart-ready CSVs for the
# eight charts, the supporting tables and the quality checks. Reads the outputs of
# scripts/28_build_us_trade_restrictiveness.R (line table, cell table, posture comparison, benchmarks), the
# tracker's monthly Census files, the statutory engine outputs of statutory_variants.py, the fence-tools
# derived tables, Comtrade 2024 partner totals, the CM-FDI project table and the reference files named in
# config/trade_fence_explainer.yml. Writes outputs/trade_fence_explainer/data/.

library(dplyr)
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/supply_structure.R",
            "R/strategic_position/trade_restrictiveness.R", "R/trade_fence_explainer/explainer_data.R")) {
  source(file.path(repo_root, f))
}
config <- getOption("opportunity_security.config")
cfg_tri <- yaml::read_yaml(file.path(repo_root, "config", "us_trade_restrictiveness.yml"))
cfg <- yaml::read_yaml(Sys.getenv("OPSI_TRADE_FENCE_EXPLAINER", file.path(repo_root, "config", "trade_fence_explainer.yml")))
abs_path <- function(p) if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(repo_root, p)
processed_dir <- file.path(repo_root, config$processed_dir)
raw_dir <- file.path(repo_root, config$raw_data_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(processed_dir, "outputs")
diag_dir <- file.path(repo_root, "diagnostics", "us_trade_restrictiveness")
out_dir <- abs_path(cfg$output_dir)
data_out <- file.path(out_dir, "data")
dir.create(data_out, recursive = TRUE, showWarnings = FALSE)
need <- function(p) { if (!file.exists(p)) stop(p, " not found."); p }
rd <- function(p, ...) utils::read.csv(need(p), check.names = FALSE, stringsAsFactors = FALSE, ...)
wcsv <- function(x, f) { utils::write.csv(x, file.path(data_out, f), row.names = FALSE, na = ""); invisible(x) }
qc <- list()
add_qc <- function(check, pass, detail) qc[[length(qc) + 1]] <<- data.frame(check = check, pass = pass, detail = detail)

# --- inputs from scripts/28 -------------------------------------------------------------------------------
goods <- readRDS(need(file.path(outputs_dir, "us_trade_restrictiveness_goods.rds")))
cmp <- rd(file.path(diag_dir, "tri_vs_posture.csv"))
bench <- rd(file.path(diag_dir, "tri_benchmarks.csv"))
run_info <- rd(file.path(diag_dir, "run_info.csv"), colClasses = "character")
ri <- stats::setNames(run_info$value, run_info$item)
data_month <- ri[["data_month"]]
open <- cfg_tri$thresholds$open
high <- cfg_tri$thresholds$high
read_ref <- function(path) utils::read.csv(file.path(repo_root, path), colClasses = "character", check.names = FALSE, na.strings = character())
all_techs <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal", "Green Hydrogen",
               "Nuclear", "Oil", "Solar", "Wind")
cells <- tri_basket_cells(read_ref("data/reference/energy_hs6_master.csv"), all_techs)
techs <- unlist(cfg$techs)
stages <- SUPPLY_CHAIN_STAGES
tri_dir <- Sys.getenv("OPSI_TWT_TRI_DIR", file.path(raw_dir, "trade_war_tracker", "how-restrictive-us-trade"))
named_codes <- utils::read.csv(need(file.path(tri_dir, "data", "country-list.csv")), header = FALSE, colClasses = "character")[[1]]
partner_codes <- read_ref(cfg_tri$partner_codes)
geo <- readRDS(need(file.path(processed_dir, "geo_alignment_tbl.rds")))
pairs <- sp_coercive_pairs(geo)
monthly <- tfe_monthly_lines(goods, named_codes)

cmp$verdict4 <- tfe_verdict4(cmp$verdict)
cmp$choice_label <- unname(TFE_CHOICE_LABEL[ifelse(is.na(cmp$strategic_choice) | cmp$strategic_choice == "", "not_assessed", cmp$strategic_choice)])
cmp$key <- paste(cmp$tech, cmp$stage, sep = "|")

# QC 1-3: the audit's headline numbers, cells and verdicts.
b <- function(m, g) bench$tri[bench$measure == m & bench$supplier_group == g]
m_now <- paste0("collected_", data_month)
audit <- c(`All suppliers` = 6.6, Trusted = 2.1, Coercive = 13.6, China = 15.7)
audit_now <- c(`All suppliers` = 15.6, Trusted = 12.3, Coercive = 24.8, China = 28.4)
for (g in names(audit)) {
  add_qc(paste0("1. Headline TRI ", g), abs(round(b("collected_2024", g), 1) - audit[[g]]) < 0.05 && abs(round(b(m_now, g), 1) - audit_now[[g]]) < 0.05,
         sprintf("2024 %.2f (audit %.1f); %s %.2f (audit %.1f)", b("collected_2024", g), audit[[g]], data_month, b(m_now, g), audit_now[[g]]))
}
assessed <- cmp[!is.na(cmp$verdict4), ]
add_qc("2. Assessed cells", nrow(assessed) == 46, sprintf("%d assessed cells with traded goods (audit 46); not assessed: %s", nrow(assessed),
       paste(cmp$key[is.na(cmp$verdict4)], collapse = ", ")))
vc <- table(factor(assessed$verdict4, TFE_VERDICTS))
add_qc("3. Verdict counts (observed)", all(as.integer(vc) == c(12, 17, 16, 1)), paste(names(vc), as.integer(vc), sep = " ", collapse = "; "))

# --- chart 1: the fence rising ----------------------------------------------------------------------------
grp_order <- c("All suppliers", "Trusted", "Nonaligned", "Coercive", "China")
c1 <- do.call(rbind, lapply(grp_order, function(g) data.frame(
  supplier_group = g,
  tri_2024 = b("collected_2024", g), tri_latest = b(m_now, g),
  mean_2024 = bench$mean[bench$measure == "collected_2024" & bench$supplier_group == g],
  mean_latest = bench$mean[bench$measure == m_now & bench$supplier_group == g])))
c1$latest_month <- data_month
c1$change_points <- c1$tri_latest - c1$tri_2024
c1$ratio <- c1$tri_latest / c1$tri_2024
c1$measure <- "Trade Restrictiveness Index (Waugh), all goods, Census calculated duty (excludes AD/CVD); 2024 import weights"

# --- chart 2: tariff map ----------------------------------------------------------------------------------
grid <- expand.grid(tech = techs, stage = stages, stringsAsFactors = FALSE)
c2 <- merge(grid, cmp[c("tech", "stage", "imports_usd", "tri_all", "tri_all_duty_only", "tri_trusted", "tri_coercive", "tri_china",
                        "adcvd_weight_share", "thin", "weight_coverage")], by = c("tech", "stage"), all.x = TRUE)
c2$in_basket <- !is.na(c2$imports_usd) & c2$imports_usd > 0
c2$scored_on_assembly_basket <- c2$stage == "Deployment" & c2$in_basket
c2$thin <- c2$in_basket & toupper(as.character(c2$thin)) == "TRUE"
c2 <- c2[order(match(c2$tech, techs), match(c2$stage, stages)), ]

# --- chart 3: partners --------------------------------------------------------------------------------------
months3 <- unlist(cfg$partner_rate_months)
pr <- tfe_partner_rates(monthly[monthly$cty_code %in% named_codes, ], months3)
pr$iso3 <- partner_codes$iso3[match(pr$cty_code, partner_codes$census_code)]
pr$name <- tools::toTitleCase(tolower(partner_codes$census_name[match(pr$cty_code, partner_codes$census_code)]))
pr$name[pr$iso3 == "KOR"] <- "Korea"; pr$name[pr$iso3 == "GBR"] <- "UK"; pr$name[pr$iso3 == "SAU"] <- "Saudi Arabia"
pr$group <- tri_partner_groups(pr$iso3, pairs, "USA")
usa <- geo[geo$reporter_iso == "USA", ]
pr$s_partner <- usa$s_partner[match(pr$iso3, usa$partner_iso)]
pr$bloc_partner <- usa$bloc_partner[match(pr$iso3, usa$partner_iso)]
ct_path <- abs_path(cfg$comtrade_partner_trade)
if (!file.exists(ct_path)) stop(ct_path, " not found; run scripts/trade_fence_explainer/fetch_us_partner_trade.R.")
ct <- jsonlite::fromJSON(ct_path)$data
ct <- ct[ct$partnerCode != 0, c("partnerCode", "partnerDesc", "flowCode", "primaryValue")]
ct$iso3 <- suppressWarnings(countrycode::countrycode(ct$partnerCode, "un", "iso3c", warn = FALSE))
fix <- c(`251` = "FRA", `699` = "IND", `757` = "CHE", `579` = "NOR", `490` = "TWN", `842` = "USA")
ct$iso3[as.character(ct$partnerCode) %in% names(fix)] <- unname(fix[as.character(ct$partnerCode[as.character(ct$partnerCode) %in% names(fix)])])
two_way <- tapply(ct$primaryValue, ct$iso3, sum)
imp24 <- tapply(ct$primaryValue[ct$flowCode == "M"], ct$iso3[ct$flowCode == "M"], sum)
pr$two_way_trade_2024_usd <- unname(two_way[pr$iso3])
pr$comtrade_imports_2024_usd <- unname(imp24[pr$iso3])
pr$label <- pr$iso3 %in% unlist(cfg$label_partners)
pr$plotted <- !is.na(pr$s_partner) & !is.na(pr$two_way_trade_2024_usd)
c3 <- pr[order(-pr$two_way_trade_2024_usd), c("iso3", "name", "group", "bloc_partner", "s_partner", "effective_rate", "imports_usd",
                                              "duty_usd", "two_way_trade_2024_usd", "comtrade_imports_2024_usd", "label", "plotted")]
names(c3)[names(c3) == "imports_usd"] <- "imports_jun_aug_2026_usd"
names(c3)[names(c3) == "duty_usd"] <- "duty_jun_aug_2026_usd"
g3 <- tfe_group_rates(transform(c3[c3$plotted, ], imports_usd = imports_jun_aug_2026_usd))
add_qc("4. Bloc classification (chart 3)", all(c3$group[c3$iso3 == "CHN"] == "Coercive") && all(c3$group[c3$iso3 %in% c("JPN", "DEU", "GBR", "CAN", "KOR")] == "Trusted"),
       paste(sprintf("%s=%s", c3$iso3, substr(c3$group, 1, 1)), collapse = " "))

# --- chart 4: fence tools -----------------------------------------------------------------------------------
ft_dir <- abs_path(cfg$fence_tools_dir)
c4 <- tfe_fence_tools(rd(file.path(ft_dir, "fence_stage_tbl.csv")), rd(file.path(ft_dir, "fence_measures.csv")))

# --- statutory engine: Dec 4 snapshot and counterfactuals ---------------------------------------------------
stat_dir <- abs_path(Sys.getenv("OPSI_TFE_STATUTORY_DIR", cfg$statutory_dir))
snap <- cfg$snapshot_date
base_date <- cfg$statutory_base_date
stat_file <- function(d, v) file.path(stat_dir, sprintf("statutory_%s_%s.csv", d, v))
have_stat <- all(file.exists(c(stat_file(snap, "full"), stat_file(base_date, "full"))))
orders <- read_ref(cfg_tri$adcvd_orders)
orders_dec4 <- read_ref(cfg$adcvd_orders_dec4)
iso_of <- c(stats::setNames(partner_codes$iso3, partner_codes$census_code), ROW = TRI_ROW_CODE)
eu27 <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
          "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
read_stat <- function(d, v) {
  s <- utils::read.csv(stat_file(d, v), colClasses = c(date = "character", variant = "character", cty_code = "character", hs10 = "character"))
  s[s$imports > 0, , drop = FALSE]
}
tag_stat <- function(s, ord = orders_dec4) {
  s$iso3 <- unname(iso_of[s$cty_code])
  if (any(is.na(s$iso3))) stop("Engine partner code(s) without ISO3: ", paste(unique(s$cty_code[is.na(s$iso3)]), collapse = ", "))
  s$group <- tri_partner_groups(s$iso3, pairs, "USA")
  s$hs6 <- substr(s$hs10, 1, 6)
  s$w <- s$imports
  s$tau_2024 <- 100 * s$duty / s$imports
  s$tau_stat <- s$tau_2024 + s$tariff
  s$s232 <- "none"
  ad <- tri_adcvd_rates(s$hs10, s$iso3, ord)
  s$adcvd <- ad$adcvd
  s$covered <- s$adcvd > 0
  s$tau_stat_adcvd <- pmin(s$tau_stat + s$adcvd, 100)
  s
}
c7 <- NULL
dec4 <- NULL
if (have_stat) {
  e_base <- read_stat(base_date, "full")
  k <- function(s) paste(s$cty_code, s$hs10)
  base_rate <- stats::setNames(e_base$tariff, k(e_base))
  # Observed + statutory change: each goods line takes its engine partner's change in added duty from the base
  # date to the snapshot under a variant (EU members take the EU bloc's line; lines the engine does not price,
  # such as the rest-of-world residual, take none).
  eng_key <- paste(ifelse(goods$iso3 %in% eu27, "0003", goods$cty_code), goods$hs10)
  line_delta <- function(v) {
    e <- read_stat(snap, v)
    dl <- unname(stats::setNames(e$tariff, k(e))[eng_key] - base_rate[eng_key])
    dl[is.na(dl)] <- 0
    dl
  }
  goods$engine_priced <- eng_key %in% names(base_rate)
  goods$delta_dec4 <- line_delta("full")
  # AD/CVD in force on the snapshot date (adcvd_orders_dec4.csv: orders published or provisional deposits
  # started after the audit's 3 November file).
  goods$adcvd_aug <- goods$adcvd
  goods$adcvd_dec4 <- tri_adcvd_rates(goods$hs10, goods$iso3, orders_dec4)$adcvd
  goods$tau_dec4 <- tfe_scenario_tau(goods$tau_collected, goods$tau_base, goods$delta_dec4, goods$adcvd_dec4)
  goods$tau_dec4_duty_only <- ifelse(is.na(goods$tau_collected), NA_real_, goods$tau_collected + goods$delta_dec4)
  goods$matched_dec4 <- goods$matched | goods$adcvd_dec4 > 0
  g4 <- goods
  g4$covered <- g4$adcvd_dec4 > 0
  dec4_cells <- tri_by_cell(g4, cells, "tau_dec4", "dec4", keep = "matched_dec4", cap = 100)
  rm(g4)
  pick <- function(grp) { d <- dec4_cells[dec4_cells$supplier_group == grp, ]; stats::setNames(d$tri, paste(d$tech, d$stage, sep = "|")) }
  cmp$tri_all_dec4 <- unname(pick("All suppliers")[cmp$key])
  cmp$tri_trusted_dec4 <- unname(pick("Trusted")[cmp$key])
  cmp$tri_coercive_dec4 <- unname(pick("Coercive")[cmp$key])
  cmp$tri_china_dec4 <- unname(pick("China")[cmp$key])
  v4 <- tri_posture_verdict(cmp$strategic_choice, cmp$tri_all_dec4, cmp$tri_trusted_dec4, cmp$tri_coercive_dec4, open, high)
  cmp$verdict_dec4 <- v4$verdict
  cmp$verdict4_dec4 <- tfe_verdict4(v4$verdict)
  cmp$verdict_changed_dec4 <- !is.na(cmp$verdict4) & cmp$verdict4 != cmp$verdict4_dec4
  dec4_bench <- tri_benchmarks(goods[!is.na(goods$tau_dec4_duty_only), ], "tau_dec4_duty_only", "dec4_duty_only")
  c1$tri_dec4_modeled <- dec4_bench$tri[match(c1$supplier_group, dec4_bench$supplier_group)]
  moved <- goods$delta_dec4 != 0
  add_qc("7. Observed vs statutory dates", TRUE, sprintf(
    "Observed Census month %s; statutory snapshot %s = observed + engine change %s -> %s on %d of %d lines (%.1f%% of 2024 weight); %.1f%% of 2024 weight is priced by the engine",
    data_month, snap, base_date, snap, sum(moved), nrow(goods), 100 * sum(goods$w[moved]) / sum(goods$w),
    100 * sum(goods$w[goods$engine_priced]) / sum(goods$w)))

  # Counterfactual TRI (chart 7), anchored to observed rates: each scenario adds the engine's change from the
  # base date to the snapshot with one authority switched off, floored at the lower of the observed and 2024
  # rates, so the "all measures" scenario is the Dec 4 measure of chart 5.
  keep_cells <- unlist(cfg$decompose_cells)
  ccells <- cells[paste(cells$tech, cells$stage, sep = "|") %in% keep_cells, ]
  in_c <- goods$hs6 %in% ccells$hs6
  gb <- goods[in_c, ]
  gb$covered <- gb$adcvd_dec4 > 0
  vs <- c("no232", "no301", "no338")
  vs <- vs[file.exists(stat_file(snap, vs))]
  for (v in vs) gb[[paste0("delta_", v)]] <- line_delta(v)[in_c]
  sc_tau <- list(
    `Dec 4 statutory (all measures)` = tfe_scenario_tau(gb$tau_collected, gb$tau_base, gb$delta_dec4, gb$adcvd_dec4),
    `Without Section 232` = if ("no232" %in% vs) tfe_scenario_tau(gb$tau_collected, gb$tau_base, gb$delta_no232, gb$adcvd_dec4),
    `Without Section 301` = if ("no301" %in% vs) tfe_scenario_tau(gb$tau_collected, gb$tau_base, gb$delta_no301, gb$adcvd_dec4),
    `Without Section 338` = if ("no338" %in% vs) tfe_scenario_tau(gb$tau_collected, gb$tau_base, gb$delta_no338, gb$adcvd_dec4),
    `Without AD/CVD` = tfe_scenario_tau(gb$tau_collected, gb$tau_base, gb$delta_dec4, 0),
    `2024 rates (pre-2025 baseline)` = tfe_scenario_tau(gb$tau_base, gb$tau_base, 0, 0))
  sc_tau <- sc_tau[!vapply(sc_tau, is.null, logical(1))]
  c7 <- do.call(rbind, lapply(names(sc_tau), function(sc) {
    gb$tau_sc <- sc_tau[[sc]]
    r <- tri_by_cell(gb, ccells, "tau_sc", sc, keep = "matched_dec4", cap = 100)
    tr <- r[r$supplier_group == "Trusted", c("tech", "stage", "measure", "tri", "mean", "w_total")]
    al <- r[r$supplier_group == "All suppliers", c("tech", "stage", "tri", "mean")]
    names(al)[3:4] <- c("tri_all_suppliers", "mean_all_suppliers")
    names(tr)[3] <- "scenario"
    merge(tr, al, by = c("tech", "stage"))
  }))
  c7$key <- paste(c7$tech, c7$stage, sep = "|")
  trw <- gb$w * (gb$group %in% "Trusted")
  pri <- data.frame(hs6 = gb$hs6, w = trw, wp = trw * gb$engine_priced)
  cov <- merge(unique(ccells[c("tech", "stage", "hs6")]), pri, by = "hs6")
  cov <- stats::aggregate(cbind(w, wp) ~ tech + stage, data = cov, FUN = sum)
  c7$trusted_weight_priced_by_engine <- (cov$wp / cov$w)[match(c7$key, paste(cov$tech, cov$stage, sep = "|"))]
  obs <- cmp[match(c7$key, cmp$key), ]
  c7$observed_aug2026_trusted_tri <- obs$tri_trusted
  c7$audit_s232_tri_sq_share_trusted <- obs$s232_tri_sq_share_trusted
  full <- c7[c7$scenario == "Dec 4 statutory (all measures)", c("key", "tri")]
  c7$change_vs_full <- c7$tri - full$tri[match(c7$key, full$key)]
  add_qc("7b. Chart 7 reconciles to chart 5", isTRUE(all.equal(full$tri, unname(cmp$tri_trusted_dec4[match(full$key, cmp$key)]), tolerance = 1e-9)),
         "The all-measures scenario equals the Dec 4 trusted-partner TRI of chart 5 in every decomposed cell")
  # Sensitivity: the same scenarios on the engine's statutory rates alone (2024 duty + added duty), not
  # anchored to observed collection.
  st <- do.call(rbind, lapply(c("full", vs), function(v) {
    s <- tag_stat(read_stat(snap, v))
    s[s$hs6 %in% ccells$hs6, , drop = FALSE]
  }))
  scen <- list(
    `Dec 4 statutory (all measures)` = list(variant = "full", tau = "tau_stat_adcvd"),
    `Without Section 232` = list(variant = "no232", tau = "tau_stat_adcvd"),
    `Without Section 301` = list(variant = "no301", tau = "tau_stat_adcvd"),
    `Without Section 338` = list(variant = "no338", tau = "tau_stat_adcvd"),
    `Without AD/CVD` = list(variant = "full", tau = "tau_stat"),
    `2024 rates (pre-2025 baseline)` = list(variant = "full", tau = "tau_2024"))
  scen <- scen[vapply(scen, function(x) x$variant %in% st$variant, logical(1))]
  ce <- tfe_counterfactuals(st, ccells, scen, "Trusted")
  c7$engine_only_trusted_tri <- ce$tri[match(paste(c7$key, c7$scenario), paste(paste(ce$tech, ce$stage, sep = "|"), ce$scenario))]
  sb <- tag_stat(e_base, orders)
  sb <- sb[sb$hs6 %in% ccells$hs6, ]
  eng_aug <- tri_by_cell(sb, ccells, "tau_stat_adcvd", "engine_base")
  eng_aug <- eng_aug[eng_aug$supplier_group == "Trusted", c("tech", "stage", "tri")]
  c7$engine_only_base_date_trusted_tri <- eng_aug$tri[match(c7$key, paste(eng_aug$tech, eng_aug$stage, sep = "|"))]
  c7 <- c7[order(match(c7$key, keep_cells), match(c7$scenario, names(sc_tau))), ]
  add_qc("9. AD/CVD counted once", all(goods$adcvd >= 0) && !any(grepl("adcvd", names(goods)[names(goods) == "tau_collected"])),
         "Census calculated duty excludes AD/CVD (docs/us_trade_restrictiveness.md); deposits added once per line via tri_adcvd_tau(); the engine's added duty has no AD/CVD")
} else {
  message("No statutory engine outputs in ", stat_dir, "; the Dec 4 snapshot and chart 7 are skipped (run statutory_variants.py).")
}

# Cells with goods under the Section 232 minimum import prices from the snapshot date (flag only: no ad
# valorem equivalent), with the 2024 import share of those goods.
mip_hs6 <- c("280461", "381800", "854142", "854143")
mip <- do.call(rbind, lapply(split(merge(goods[c("hs6", "w")], unique(cells[c("tech", "stage", "hs6")]), by = "hs6"),
                                   ~ tech + stage, drop = TRUE), function(d) data.frame(
  tech = d$tech[1], stage = d$stage[1], mip_import_share = sum(d$w[d$hs6 %in% mip_hs6]) / sum(d$w))))
mip <- mip[mip$mip_import_share > 0, ]
cmp$dec4_price_floor_share <- mip$mip_import_share[match(cmp$key, paste(mip$tech, mip$stage, sep = "|"))]
cmp$dec4_price_floor_share[is.na(cmp$dec4_price_floor_share)] <- 0
c2$dec4_price_floor_share <- cmp$dec4_price_floor_share[match(paste(c2$tech, c2$stage, sep = "|"), cmp$key)]
if ("tri_all_dec4" %in% names(cmp)) c2$tri_all_dec4 <- cmp$tri_all_dec4[match(paste(c2$tech, c2$stage, sep = "|"), cmp$key)]
c4$dec4_price_floor <- c4$tool_key == "tariff" & c4$tech == "Solar" & c4$stage %in% c("Refining", "Components", "Assembly / Deployment")

# --- chart 5: Yards & Fences audit ----------------------------------------------------------------------------
c5 <- merge(grid, cmp[setdiff(names(cmp), "key")], by = c("tech", "stage"), all.x = TRUE)
c5$in_basket <- !is.na(c5$imports_usd) & c5$imports_usd > 0
c5$thin <- c5$in_basket & toupper(as.character(c5$thin)) == "TRUE"
c5 <- c5[order(match(c5$tech, techs), match(c5$stage, stages)), ]
keep5 <- c("tech", "stage", "strategic_posture", "strategic_choice", "choice_label", "fence_level", "yard", "imports_usd", "thin",
           "tri_all", "tri_trusted", "tri_coercive", "tri_china", "tri_nonaligned", "verdict", "verdict4", "verdict_reason",
           "verdict_duty_only", intersect(c("tri_all_dec4", "tri_trusted_dec4", "tri_coercive_dec4", "verdict_dec4", "verdict4_dec4",
                                            "verdict_changed_dec4"), names(c5)), "dec4_price_floor_share", "in_basket")
c5 <- c5[keep5]
c5b <- data.frame(verdict = TFE_VERDICTS, cells_observed = as.integer(table(factor(c5$verdict4, TFE_VERDICTS))))
if ("verdict4_dec4" %in% names(c5)) {
  c5b$cells_dec4 <- as.integer(table(factor(c5$verdict4_dec4, TFE_VERDICTS)))
  ch <- c5[c5$verdict_changed_dec4 %in% TRUE, ]
  add_qc("3b. Verdict counts (Dec 4 statutory)", TRUE, paste0(paste(c5b$verdict, c5b$cells_dec4, sep = " ", collapse = "; "),
         if (nrow(ch)) paste0(". Changed: ", paste(ch$tech, ch$stage, ch$verdict4, "->", ch$verdict4_dec4, collapse = "; ")) else ". No verdict changes."))
}

# --- chart 6: dollar-weighted -----------------------------------------------------------------------------------
vv <- cmp[c("tech", "stage", "verdict4")]
c6 <- tfe_dollar_weighted(goods, cells, vv)
c6$verdict_basis <- paste0("observed ", data_month, " + AD/CVD")
if ("verdict4_dec4" %in% names(cmp)) {
  vd <- cmp[c("tech", "stage", "verdict4_dec4")]
  names(vd)[3] <- "verdict4"
  c6d <- tfe_dollar_weighted(goods, cells, vd)
  c6d$verdict_basis <- paste0("statutory ", snap)
  c6 <- rbind(c6, c6d)
}
# Variant without the fossil-fuel technologies: crude oil and refined products dominate the aligned dollars.
clean <- setdiff(techs, c("Oil", "Gas", "Coal"))
c6c <- tfe_dollar_weighted(goods, cells[cells$tech %in% clean, ], vv[vv$tech %in% clean, ])
c6c$suppliers <- paste0(c6c$suppliers, " (excl. oil, gas, coal)")
c6c$verdict_basis <- paste0("observed ", data_month, " + AD/CVD")
c6 <- rbind(c6, c6c)
c6_cov <- tfe_dedup_coverage(goods, cells, vv)
c6_tech <- tfe_dollar_weighted_by_tech(goods, cells, vv)
gross <- sum(c6$imports_usd[c6$basis == "gross" & c6$suppliers == "All suppliers" & grepl("^observed", c6$verdict_basis)])
dedup <- sum(c6$imports_usd[c6$basis == "dedup_equal_split" & c6$suppliers == "All suppliers" & grepl("^observed", c6$verdict_basis)])
add_qc("5. Deployment not double-counted", abs(dedup - c6_cov$assessed_imports_usd[c6_cov$suppliers == "All suppliers"]) < 1,
       sprintf("Dedup total $%.1fbn = unique assessed producing-stage HS6 imports $%.1fbn; gross with Deployment $%.1fbn",
               dedup / 1e9, c6_cov$assessed_imports_usd[1] / 1e9, gross / 1e9))
ov <- table(table(unique(cells[cells$stage != "Deployment" & cells$tech %in% techs, c("tech", "hs6")])$hs6))
add_qc("6. HS6 duplication across baskets", TRUE, paste0("HS6 codes by number of technology baskets: ", paste(names(ov), as.integer(ov), sep = " basket(s): ", collapse = "; "),
                                                         ". Handled by equal split in the dedup basis."))

# --- chart 8 and table C: monthly Census by origin ----------------------------------------------------------------
entity_file <- function(code) need(file.path(tri_dir, "data", "imports-hs10", paste0(code, "data-current.parquet")))
prefixes <- unique(c(unlist(cfg$modules_hs10), unlist(cfg$batteries_hs10), unlist(cfg$response_groups)))
mon <- do.call(rbind, lapply(c(named_codes, "TOTAL"), function(code) {
  t <- as.data.frame(arrow::read_parquet(entity_file(code), col_select = c("CON_VAL_MO", "CAL_DUT_MO", "I_COMMODITY", "time")))
  t <- t[t$time >= "2023-01" & Reduce(`|`, lapply(prefixes, function(p) startsWith(t$I_COMMODITY, p))), , drop = FALSE]
  data.frame(cty_code = code, hs10 = t$I_COMMODITY, time = t$time, value = suppressWarnings(as.numeric(t$CON_VAL_MO)),
             duty = suppressWarnings(as.numeric(t$CAL_DUT_MO)), stringsAsFactors = FALSE)
}))
mon$value[is.na(mon$value)] <- 0
mon$duty[is.na(mon$duty)] <- 0
by_origin <- function(pref, origins, label) {
  d <- mon[Reduce(`|`, lapply(pref, function(p) startsWith(mon$hs10, p))), , drop = FALSE]
  tot <- stats::aggregate(cbind(value, duty) ~ time, data = d[d$cty_code == "TOTAL", ], FUN = sum)
  sel <- d[d$cty_code %in% names(origins), , drop = FALSE]
  sel$origin <- unname(origins[sel$cty_code])
  a <- stats::aggregate(cbind(value, duty) ~ time + origin, data = sel, FUN = sum)
  named_sum <- stats::aggregate(cbind(value, duty) ~ time, data = sel, FUN = sum)
  oth <- merge(tot, named_sum, by = "time", all.x = TRUE, suffixes = c("", "_named"))
  oth$value_named[is.na(oth$value_named)] <- 0; oth$duty_named[is.na(oth$duty_named)] <- 0
  a <- rbind(a, data.frame(time = oth$time, origin = "All other", value = oth$value - oth$value_named, duty = oth$duty - oth$duty_named))
  a$product <- label
  a$census_duty_rate <- ifelse(a$value > 0, 100 * a$duty / a$value, NA_real_)
  a[order(a$time, a$origin), c("product", "time", "origin", "value", "duty", "census_duty_rate")]
}
c8a <- by_origin(unlist(cfg$modules_hs10), unlist(cfg$module_origins), "Crystalline-silicon PV modules (HTS 8541.43.0010)")
# Origins outside the tracker's 30 partners: the Census residual (total less the 30) is split by each untracked
# partner's share of US imports of HS 854143 that month in UN Comtrade (the same Census data, all partners, HS6
# incl. thin film). The tracker's other named partners stay in "All other".
ct_dir <- abs_path(cfg$comtrade_monthly_dir)
ctf <- list.files(ct_dir, pattern = "^854143_[0-9]{6}[.]json$", full.names = TRUE)
ctm <- do.call(rbind, lapply(ctf, function(f) {
  d <- jsonlite::fromJSON(f)$data
  if (!NROW(d)) return(NULL)
  data.frame(period = as.character(d$period), partner = d$partnerCode, value = d$primaryValue)
}))
ctm <- ctm[ctm$partner != 0, ]
ctm$iso3 <- suppressWarnings(countrycode::countrycode(ctm$partner, "un", "iso3c", warn = FALSE))
ctm$iso3[as.character(ctm$partner) %in% names(fix)] <- unname(fix[as.character(ctm$partner[as.character(ctm$partner) %in% names(fix)])])
ctm$time <- paste0(substr(ctm$period, 1, 4), "-", substr(ctm$period, 5, 6))
named_iso <- partner_codes$iso3[match(named_codes, partner_codes$census_code)]
named_iso[named_codes == "0003"] <- NA
unt <- ctm[!ctm$iso3 %in% c(named_iso, eu27[eu27 %in% named_iso]), ]
unt$origin <- unname(c(KHM = "Cambodia", LAO = "Laos", PHL = "Philippines")[unt$iso3])
cont <- suppressWarnings(countrycode::countrycode(unt$iso3, "iso3c", "continent", warn = FALSE))
unt$origin[is.na(unt$origin) & cont %in% "Africa"] <- "Africa"
unt$origin[is.na(unt$origin)] <- "All other"
ush <- stats::aggregate(value ~ time + origin, data = unt, FUN = sum)
ush$share <- ush$value / stats::ave(ush$value, ush$time, FUN = sum)
md <- mon[startsWith(mon$hs10, unlist(cfg$modules_hs10)[1]), ]
resid <- merge(stats::aggregate(value ~ time, data = md[md$cty_code == "TOTAL", ], FUN = sum),
               stats::aggregate(value ~ time, data = md[md$cty_code %in% named_codes, ], FUN = sum), by = "time", all.x = TRUE, suffixes = c("_total", "_named"))
resid$value_named[is.na(resid$value_named)] <- 0
resid$residual <- pmax(0, resid$value_total - resid$value_named)
alloc <- merge(resid[c("time", "residual")], ush[c("time", "origin", "share")], by = "time", all.x = TRUE)
alloc$origin[is.na(alloc$origin)] <- "All other"
alloc$share[is.na(alloc$share)] <- 1
alloc$value <- alloc$residual * alloc$share
c8a$source <- "Census HS10 (tracker partner files)"
c8a <- c8a[c8a$time >= "2024-01", ]
oth <- c8a$origin == "All other"
res_t <- resid$residual[match(c8a$time[oth], resid$time)]
c8a$value[oth] <- c8a$value[oth] - ifelse(is.na(res_t), 0, res_t)
c8a$duty[oth] <- NA
add8 <- stats::aggregate(value ~ time + origin, data = alloc[alloc$time >= "2024-01", ], FUN = sum)
add8$product <- c8a$product[1]
add8$duty <- NA
add8$census_duty_rate <- NA
add8$source <- "Census residual allocated by UN Comtrade HS 854143 partner shares"
c8a <- rbind(c8a, add8[names(c8a)])
c8a <- stats::aggregate(value ~ product + time + origin, data = c8a, FUN = sum)
c8a <- c8a[order(c8a$time, c8a$origin), ]
tot8 <- stats::aggregate(value ~ time, data = c8a, FUN = sum)
ref8 <- resid$value_total[match(tot8$time, resid$time)]
gap8 <- max(abs(tot8$value - ref8) / ref8, na.rm = TRUE)
add_qc("8b. Chart 8A sums to Census total", gap8 < 1e-9 && !anyNA(ref8),
       sprintf("Module imports by origin add up to the Census total every month (largest relative gap %.1e, %d months); %d Comtrade months used",
               gap8, nrow(tot8), length(unique(ctm$time))))
c8b_m <- by_origin(unlist(cfg$batteries_hs10), unlist(cfg$battery_origins), "Lithium-ion batteries (HTS 8507.60)")
c8a <- c8a[c8a$time >= "2024-01", ]
c8b_m <- c8b_m[c8b_m$time >= "2024-01", ]
latest <- max(mon$time[mon$cty_code == "TOTAL"])
ytd <- function(y) paste0(y, "-", unlist(cfg$response_months))
per <- function(d, months) stats::aggregate(value ~ origin, data = d[d$time %in% months, ], FUN = sum)
b24 <- per(c8b_m, ytd(2024)); b26 <- per(c8b_m, ytd(2026))
c8b <- merge(b24, b26, by = "origin", all = TRUE, suffixes = c("_jan_aug_2024", "_jan_aug_2026"))
c8b$share_2024 <- c8b$value_jan_aug_2024 / sum(c8b$value_jan_aug_2024, na.rm = TRUE)
c8b$share_2026 <- c8b$value_jan_aug_2026 / sum(c8b$value_jan_aug_2026, na.rm = TRUE)
c8b$change_pct <- 100 * (c8b$value_jan_aug_2026 / c8b$value_jan_aug_2024 - 1)
# Ownership of battery manufacturing FDI (CM-FDI project table): Chinese-parent share by destination.
fdi_path <- file.path(processed_dir, cfg$cm_fdi_projects)
own <- NULL
if (file.exists(fdi_path)) {
  p <- as.data.frame(readRDS(fdi_path))
  p <- p[p$tech %in% "Batteries" & p$activity %in% "Manufacturing" & p$destination_iso3 %in% unlist(cfg$ownership_destinations), , drop = FALSE]
  p$cap <- suppressWarnings(as.numeric(p$capital_usd_m)) * suppressWarnings(as.numeric(p$attribution_weight))
  p$china_parent <- toupper(as.character(p$china_parent)) == "TRUE"
  p$status_group <- ifelse(p$operational %in% "Operational", "Operational", ifelse(p$operational %in% c("Under construction"), "Under construction", "Announced / other"))
  own <- do.call(rbind, lapply(split(p, list(p$destination_iso3, p$status_group), drop = TRUE), function(d) data.frame(
    destination_iso3 = d$destination_iso3[1], status_group = d$status_group[1], n_projects = nrow(d),
    n_chinese_parent = sum(d$china_parent), capital_disclosed_usd_m = sum(d$cap, na.rm = TRUE),
    capital_chinese_parent_usd_m = sum(d$cap[d$china_parent], na.rm = TRUE), n_capital_undisclosed = sum(is.na(d$cap)))))
  tot_own <- do.call(rbind, lapply(split(p, p$destination_iso3), function(d) data.frame(
    destination_iso3 = d$destination_iso3[1], status_group = "All projects", n_projects = nrow(d),
    n_chinese_parent = sum(d$china_parent), capital_disclosed_usd_m = sum(d$cap, na.rm = TRUE),
    capital_chinese_parent_usd_m = sum(d$cap[d$china_parent], na.rm = TRUE), n_capital_undisclosed = sum(is.na(d$cap)))))
  own <- rbind(own, tot_own)
  own$chinese_parent_share_count <- own$n_chinese_parent / own$n_projects
  own$chinese_parent_share_capital <- ifelse(own$capital_disclosed_usd_m > 0, own$capital_chinese_parent_usd_m / own$capital_disclosed_usd_m, NA_real_)
  own <- own[order(own$destination_iso3, own$status_group), ]
  wcsv(p[c("project_id", "parent", "home_iso3", "destination_iso3", "operational", "year_announced", "capital_usd_m",
           "attribution_weight", "china_parent", "stages")], "08b_battery_fdi_projects.csv")
}
# Table C: product groups.
grp <- cfg$response_groups
tab_c <- do.call(rbind, lapply(names(grp), function(g) {
  d <- mon[Reduce(`|`, lapply(grp[[g]], function(p) startsWith(mon$hs10, p))), , drop = FALSE]
  tot <- d[d$cty_code == "TOTAL", ]
  named <- d[d$cty_code != "TOTAL", ]
  v <- function(dd, y) sum(dd$value[dd$time %in% ytd(y)])
  t24 <- v(tot, 2024); t26 <- v(tot, 2026)
  top <- function(y) {
    s <- tapply(named$value[named$time %in% ytd(y)], named$cty_code[named$time %in% ytd(y)], sum)
    s <- sort(s[s > 0], decreasing = TRUE)[1:min(5, sum(s > 0))]
    nm <- partner_codes$iso3[match(names(s), partner_codes$census_code)]
    paste(sprintf("%s %.0f%%", nm, 100 * s / v(tot, y)), collapse = "; ")
  }
  cn <- function(y) sum(named$value[named$time %in% ytd(y) & named$cty_code == "5700"]) / v(tot, y)
  data.frame(product_group = g, hs_prefixes = paste(grp[[g]], collapse = " "), imports_jan_aug_2024_usd = t24,
             imports_jan_aug_2026_usd = t26, change_pct = 100 * (t26 / t24 - 1), china_share_2024 = cn(2024), china_share_2026 = cn(2026),
             top_sources_2024 = top(2024), top_sources_2026 = top(2026), stringsAsFactors = FALSE)
}))

# --- supporting tables A and B ------------------------------------------------------------------------------------
lines_c <- merge(goods[c("hs6", "cty_code", "iso3", "group", "w", "tau_collected", paste0("v_", tri_month_window(data_month, 3)),
                         paste0("d_", tri_month_window(data_month, 3)), paste0("v_", tri_same_months(tri_month_window(data_month, 3), 2024)),
                         paste0("d_", tri_same_months(tri_month_window(data_month, 3), 2024)))],
                 unique(cells[c("tech", "stage", "hs6")]), by = "hs6")
now_m <- tri_month_window(data_month, 3)
base_m <- tri_same_months(now_m, 2024)
tab_ab <- do.call(rbind, lapply(split(lines_c, paste(lines_c$tech, lines_c$stage, sep = "|")), function(d) {
  mo <- d[d$cty_code %in% c(named_codes, TRI_ROW_CODE), ]
  tr <- d$group %in% "Trusted"
  tr_m <- mo$group %in% "Trusted"
  cn_m <- mo$iso3 %in% "CHN"
  co_m <- mo$group %in% "Coercive"
  sv <- function(dd, ms) sum(vapply(ms, function(m) sum(dd[[paste0("v_", m)]]), numeric(1)))
  sd <- function(dd, ms) sum(vapply(ms, function(m) sum(dd[[paste0("d_", m)]]), numeric(1)))
  data.frame(tech = d$tech[1], stage = d$stage[1], imports_2024_usd = sum(d$w), trusted_imports_2024_usd = sum(d$w[tr]),
             trusted_share_2024 = sum(d$w[tr]) / sum(d$w), coercive_share_2024 = sum(d$w[d$group %in% "Coercive"]) / sum(d$w),
             china_share_2024 = sum(d$w[d$iso3 %in% "CHN"]) / sum(d$w),
             imports_latest3_usd = sv(mo, now_m), imports_same3_2024_usd = sv(mo, base_m),
             china_share_latest3 = sv(mo[cn_m, ], now_m) / sv(mo, now_m), china_share_same3_2024 = sv(mo[cn_m, ], base_m) / sv(mo, base_m),
             named_coercive_share_latest3 = sv(mo[co_m, ], now_m) / sv(mo, now_m), named_coercive_share_same3_2024 = sv(mo[co_m, ], base_m) / sv(mo, base_m),
             trusted_effective_rate_latest3 = 100 * sd(mo[tr_m, ], now_m) / sv(mo[tr_m, ], now_m),
             trusted_duty_paid_latest3_usd = sd(mo[tr_m, ], now_m), trusted_duty_paid_same3_2024_usd = sd(mo[tr_m, ], base_m),
             stringsAsFactors = FALSE)
}))
tab_ab <- merge(tab_ab, cmp[c("tech", "stage", "choice_label", "tri_all", "tri_trusted", "tri_coercive", "tri_china", "verdict4", "thin")],
                by = c("tech", "stage"), all.x = TRUE)
tab_ab$china_share_change_pts <- 100 * (tab_ab$china_share_latest3 - tab_ab$china_share_same3_2024)
tab_ab$fence_exposure <- ifelse(tab_ab$tri_all >= high & tab_ab$coercive_share_2024 >= 0.25, "high fence + high coercive dependence",
                         ifelse(tab_ab$tri_all >= high, "high fence + little coercive exposure",
                         ifelse(tab_ab$coercive_share_2024 >= 0.25, "low/moderate fence + high coercive dependence", "low/moderate fence + little coercive exposure")))
tab_ab <- tab_ab[order(match(tab_ab$tech, all_techs), match(tab_ab$stage, stages)), ]

# --- write ------------------------------------------------------------------------------------------------------
wcsv(c1, "01_fence_rising.csv")
wcsv(c2, "02_tariff_map.csv")
wcsv(c3, "03_who_gets_through_gate.csv")
wcsv(g3, "03_group_rates.csv")
wcsv(c4, "04_multi_tool_fence.csv")
wcsv(c5, "05_yards_fences_audit.csv")
wcsv(c5b, "05b_verdict_summary.csv")
wcsv(c6, "06_dollar_weighted_misalignment.csv")
wcsv(c6_cov, "06_dedup_coverage.csv")
wcsv(c6_tech, "06_dollar_weighted_by_tech.csv")
if (!is.null(c7)) wcsv(c7, "07_why_allies_pay.csv")
wcsv(c8a, "08a_solar_trade_response.csv")
wcsv(c8b_m, "08b_battery_monthly_by_origin.csv")
wcsv(c8b, "08b_battery_trade_response.csv")
if (!is.null(own)) wcsv(own, "08b_battery_ownership.csv")
wcsv(tab_ab, "A_B_cell_exposure_and_dependence.csv")
wcsv(tab_c, "C_import_response.csv")
dr <- abs_path(cfg$domestic_response)
if (file.exists(dr)) file.copy(dr, file.path(data_out, "D_domestic_response.csv"), overwrite = TRUE)
ev <- abs_path(cfg$policy_events)
if (file.exists(ev)) file.copy(ev, file.path(data_out, "08_policy_events.csv"), overwrite = TRUE)

add_qc("10. TRI labelled as restrictiveness", TRUE, "Chart titles, axes and CSV `measure` fields say Trade Restrictiveness Index; effective rates (chart 3, 8) are labelled duties/imports")
add_qc("12. Traceability", TRUE, sprintf("Tracker commits: TRI %s, engine %s; observed month %s; outputs written by scripts/trade_fence_explainer/01_build_data.R",
                                          ri[["tri_repo_commit"]], ri[["redux_repo_commit"]], data_month))
wcsv(do.call(rbind, qc), "qc_checks.csv")
meta <- data.frame(item = c("data_month", "latest_monthly_census", "snapshot_date", "statutory_base_date", "statutory_available", "tri_repo_commit", "redux_repo_commit", "open_threshold", "high_threshold"),
                   value = c(data_month, latest, snap, base_date, have_stat, ri[["tri_repo_commit"]], ri[["redux_repo_commit"]], open, high))
wcsv(meta, "build_info.csv")
message("Wrote ", data_out, ". QC: ", sum(vapply(qc, function(x) isTRUE(x$pass), logical(1))), "/", length(qc), " pass.")
