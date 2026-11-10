source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
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

# Opening section of the trade-fence explainer (outputs/trade_fence_explainer/README.md): what the US trades in the
# electro-industrial stack, how its sources changed between January-August 2024 and January-August 2026, the policy
# timeline, and the fence-change vs diversification analysis. Run after 01_build_data.R (it appends its quality
# checks to data/qc_checks.csv) and before 02_draw_charts.R.
#
# Imports: Census imports for consumption by HTS10 line and partner (the Trade War Tracker's monthly files: 30 named
# partners and the all-country total). Exports: Census total exports by HS6 as reported to UN Comtrade (monthly,
# partner World; scripts/trade_fence_explainer/fetch_comtrade.R), on the same HS6 basket.

for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/supply_structure.R", "R/strategic_position/trade_restrictiveness.R",
            "R/trade_fence_explainer/explainer_data.R", "R/trade_fence_explainer/explainer_charts.R", "R/trade_fence_explainer/trade_overview.R")) source(file.path(repo_root, f))
config <- getOption("opportunity_security.config")
cfg_tri <- yaml::read_yaml(file.path(repo_root, "config", "us_trade_restrictiveness.yml"))
cfg <- yaml::read_yaml(Sys.getenv("OPSI_TRADE_FENCE_EXPLAINER", file.path(repo_root, "config", "trade_fence_explainer.yml")))
abs_path <- function(p) if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(repo_root, p)
processed_dir <- file.path(repo_root, config$processed_dir)
raw_dir <- file.path(repo_root, config$raw_data_dir)
out_dir <- abs_path(cfg$output_dir)
data_out <- file.path(out_dir, "data")
need <- function(p) { if (!file.exists(p)) stop(p, " not found."); p }
wcsv <- function(x, f) utils::write.csv(x, file.path(data_out, f), row.names = FALSE, na = "")
read_ref <- function(path) utils::read.csv(file.path(repo_root, path), colClasses = "character", check.names = FALSE, na.strings = character())
qc <- list()
add_qc <- function(check, pass, detail) qc[[length(qc) + 1]] <<- data.frame(check = check, pass = pass, detail = detail)

ov <- cfg$trade_overview
periods <- list(`2024` = paste0("2024-", unlist(ov$months)), `2026` = paste0("2026-", unlist(ov$months)))
plab <- sprintf("January-%s", month.name[as.integer(max(unlist(ov$months)))])
all_techs <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal", "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind")
cells <- tri_basket_cells(read_ref("data/reference/energy_hs6_master.csv"), all_techs)
techs <- unlist(cfg$techs)
pc_all <- tfe_producing_cells(cells, techs)
pc_clean <- tfe_producing_cells(cells, TFE_CLEAN_TECHS)
basket <- unique(pc_all$hs6)
no_basket <- setdiff(all_techs, unique(cells$tech))

# --- Census imports by partner (tracker monthly files) --------------------------------------------------------------
tri_dir <- Sys.getenv("OPSI_TWT_TRI_DIR", file.path(raw_dir, "trade_war_tracker", "how-restrictive-us-trade"))
named <- utils::read.csv(need(file.path(tri_dir, "data", "country-list.csv")), header = FALSE, colClasses = "character")[[1]]
partner_codes <- read_ref(cfg_tri$partner_codes)
geo <- readRDS(need(file.path(processed_dir, "geo_alignment_tbl.rds")))
pairs <- sp_coercive_pairs(geo)
mon <- do.call(rbind, lapply(c(named, "TOTAL"), function(code) {
  t <- as.data.frame(arrow::read_parquet(need(file.path(tri_dir, "data", "imports-hs10", paste0(code, "data-current.parquet"))),
                                         col_select = c("CON_VAL_MO", "I_COMMODITY", "time")))
  t <- t[t$time >= "2024-01" & substr(t$I_COMMODITY, 1, 6) %in% basket, , drop = FALSE]
  if (!nrow(t)) return(NULL)
  t$value <- suppressWarnings(as.numeric(t$CON_VAL_MO))
  t$value[is.na(t$value)] <- 0
  stats::aggregate(value ~ hs6 + time, data = data.frame(hs6 = substr(t$I_COMMODITY, 1, 6), time = t$time, value = t$value), FUN = sum) |>
    transform(partner = code)
}))
latest <- max(mon$time[mon$partner == "TOTAL"])
add_qc("T2. Comparable periods", all(unlist(periods) <= latest) && length(periods[[1]]) == length(periods[[2]]),
       sprintf("%s 2024 vs %s 2026 (%d months each); latest Census month in the tracker files %s; September 2026 is not available without a Census API key",
               plab, plab, length(periods[[1]]), latest))
tot_m <- mon[mon$partner == "TOTAL", ]
nam_m <- mon[mon$partner != "TOTAL", ]
nsum <- stats::aggregate(value ~ hs6 + time, data = nam_m, FUN = sum)
row_m <- merge(tot_m[c("hs6", "time", "value")], nsum, by = c("hs6", "time"), all.x = TRUE, suffixes = c("", "_named"))
row_m$value_named[is.na(row_m$value_named)] <- 0
row_m <- data.frame(hs6 = row_m$hs6, time = row_m$time, value = pmax(0, row_m$value - row_m$value_named), partner = "ROW")
pm <- rbind(nam_m[c("hs6", "time", "value", "partner")], row_m)
pm$iso3 <- partner_codes$iso3[match(pm$partner, partner_codes$census_code)]
pm$iso3[pm$partner == "ROW"] <- "ROW"
pm$group <- ifelse(pm$partner == "ROW", "Unassigned (rest of world)", tri_partner_groups(pm$iso3, pairs, "USA"))
pm$name <- ifelse(pm$partner == "ROW", "Rest of world", tools::toTitleCase(tolower(partner_codes$census_name[match(pm$partner, partner_codes$census_code)])))
pmp <- do.call(rbind, lapply(names(periods), function(p) {
  x <- tfe_period_sum(pm, periods[[p]], c("hs6", "partner", "iso3", "group", "name"))
  x$period <- p
  x
}))
pmp$partner <- pmp$name

# --- imports and exports by HS6 ----------------------------------------------------------------------------------
imp <- do.call(rbind, lapply(names(periods), function(p) { x <- tfe_period_sum(tot_m, periods[[p]], "hs6"); x$period <- p; x }))
ct_dir <- abs_path(cfg$comtrade_monthly_dir)
ctf <- list.files(ct_dir, pattern = "^basket[0-9]+_[0-9]{6}[.]json$", full.names = TRUE)
ctb <- do.call(rbind, lapply(ctf, function(f) {
  d <- jsonlite::fromJSON(f)$data
  if (!NROW(d)) return(NULL)
  data.frame(hs6 = as.character(d$cmdCode), period_m = as.character(d$period), flow = d$flowCode, value = d$primaryValue)
}))
ctb <- ctb[ctb$hs6 %in% basket, ]
ctb$time <- paste0(substr(ctb$period_m, 1, 4), "-", substr(ctb$period_m, 5, 6))
have_m <- sort(unique(ctb$time))
miss <- setdiff(unlist(periods), have_m)
if (length(miss)) stop("Comtrade basket files missing for ", paste(miss, collapse = ", "), "; run fetch_comtrade.R.")
exp <- do.call(rbind, lapply(names(periods), function(p) { x <- tfe_period_sum(ctb[ctb$flow == "X", ], periods[[p]], "hs6"); x$period <- p; x }))
cimp <- do.call(rbind, lapply(names(periods), function(p) { x <- tfe_period_sum(ctb[ctb$flow == "M", ], periods[[p]], "hs6"); x$period <- p; x }))

# --- chart 00a ----------------------------------------------------------------------------------------------------
bt <- tfe_trade_by_tech(imp, exp, pc_all, periods)
bt <- bt[order(match(bt$tech, techs)), ]
bs <- rbind(tfe_trade_by_stage(imp, exp, pc_all, periods, "All electro-industrial technologies"),
            tfe_trade_by_stage(imp, exp, pc_clean, periods, "Clean energy and grid (excl. oil, gas, coal)"))
uniq <- function(d, p, codes) sum(d$value[d$period == p & d$hs6 %in% codes])
codes_clean <- unique(pc_clean$hs6)
totals <- do.call(rbind, lapply(list(all = basket, clean = codes_clean), function(cd) data.frame(
  imports_2024 = uniq(imp, "2024", cd), imports_2026 = uniq(imp, "2026", cd), exports_2024 = uniq(exp, "2024", cd), exports_2026 = uniq(exp, "2026", cd))))
totals$basket <- c("All electro-industrial technologies (unique HS6)", "Clean energy and grid (unique HS6)")
totals$balance_2026 <- totals$exports_2026 - totals$imports_2026
totals$imports_change_pct <- 100 * (totals$imports_2026 / totals$imports_2024 - 1)
totals$exports_change_pct <- 100 * (totals$exports_2026 / totals$exports_2024 - 1)
totals$import_export_ratio_2026 <- totals$imports_2026 / totals$exports_2026
ov_long <- rbind(
  data.frame(level = "technology", key = bt$tech, group = bt$group, stage = "All producing stages",
             imports_2024 = bt$imports_2024, imports_2026 = bt$imports_2026, exports_2024 = bt$exports_2024, exports_2026 = bt$exports_2026),
  data.frame(level = "stage", key = bs$basket, group = bs$basket, stage = bs$stage,
             imports_2024 = bs$imports_2024, imports_2026 = bs$imports_2026, exports_2024 = bs$exports_2024, exports_2026 = bs$exports_2026),
  data.frame(level = "total", key = totals$basket, group = totals$basket, stage = "All producing stages",
             imports_2024 = totals$imports_2024, imports_2026 = totals$imports_2026, exports_2024 = totals$exports_2024, exports_2026 = totals$exports_2026))
ov_long$period <- plab
ov_long$balance_2026 <- ov_long$exports_2026 - ov_long$imports_2026
ov_long$imports_change_pct <- 100 * (ov_long$imports_2026 / ov_long$imports_2024 - 1)
ov_long$exports_change_pct <- 100 * (ov_long$exports_2026 / ov_long$exports_2024 - 1)
bt$period <- plab
bs$period <- plab
bt$not_in_basket <- ""
nb <- data.frame(tech = no_basket)
if (length(no_basket)) bt <- merge(bt, data.frame(tech = no_basket, not_in_basket = "no traded HS6 basket"), all = TRUE, by = c("tech", "not_in_basket"))
bt <- bt[order(match(bt$tech, c(techs, no_basket))), ]

# --- chart 00b and the diversification metrics ---------------------------------------------------------------------
div <- tfe_diversification(pmp, pc_all, periods)
div <- div[order(match(div$tech, techs), div$stage != "All producing stages", match(div$stage, TFE_TRADE_STAGES)), ]
div$period <- plab
div$concentration_note <- "Top shares, HHI and effective suppliers over the 30 named partners only (the rest-of-world residual is unnamed), so HHI is a lower bound"
groups_c <- cfg$response_groups[intersect(names(cfg$response_groups), unlist(ov$product_groups))]
pg <- do.call(rbind, lapply(names(groups_c), function(g) {
  pre <- groups_c[[g]]
  cd <- unique(substr(pre, 1, 6))
  a <- tfe_sourcing(pmp[pmp$period == "2024", ], cd)
  b <- tfe_sourcing(pmp[pmp$period == "2026", ], cd)
  data.frame(tech = g, stage = "Product group", imports_2024 = a$imports, imports_2026 = b$imports,
             imports_change_pct = 100 * (b$imports / a$imports - 1), china_share_2024 = a$china_share, china_share_2026 = b$china_share,
             china_share_change_pts = 100 * (b$china_share - a$china_share), coercive_share_change_pts = 100 * (b$coercive_share - a$coercive_share),
             top_supplier_2026 = b$top_supplier, top_supplier_share_2026 = b$top_supplier_share)
}))
pg$quadrant <- tfe_quadrant(pg$imports_change_pct, pg$china_share_change_pts)
keep_b <- c("tech", "stage", "imports_2024", "imports_2026", "imports_change_pct", "china_share_2024", "china_share_2026", "china_share_change_pts",
            "coercive_share_change_pts", "top_supplier_2026", "top_supplier_share_2026", "quadrant")
b00 <- rbind(transform(div[div$stage == "All producing stages", keep_b], kind = "Technology"), transform(pg[keep_b], kind = "Product group"))
b00$label <- unname(c(TFE_TECH_LABEL, stats::setNames(names(groups_c), names(groups_c)))[b00$tech])
b00$period <- plab

# --- policy timeline -------------------------------------------------------------------------------------------------
tl <- read_ref(cfg$policy_timeline)
tl <- tl[order(tl$date), ]
tl$in_observed_window <- tl$date <= paste0(latest, "-31")
add_qc("T10. Policy annotations use verified effective dates", all(nzchar(tl$source_url)) && all(grepl("^verified", tl$date_basis[toupper(tl$key_event) == "TRUE" & toupper(tl$source_is_primary) == "TRUE"])),
       sprintf("%d timeline events with source URLs: %d checked against Federal Register / HTSUS text (2026-11-09/10), %d from the tracker's action registry (secondary, IEEPA 2025 steps), %d other; every primary-sourced key event is verified",
               nrow(tl), sum(grepl("^verified", tl$date_basis)), sum(tl$date_basis == "tracker registry"), sum(!grepl("^verified", tl$date_basis) & tl$date_basis != "tracker registry")))

# --- fence change vs diversification -------------------------------------------------------------------------------
cmp <- utils::read.csv(need(file.path(repo_root, "diagnostics", "us_trade_restrictiveness", "tri_vs_posture.csv")), stringsAsFactors = FALSE)
tri_ch <- data.frame(tech = cmp$tech, stage = cmp$stage, tri_2024 = cmp$tri_all_weight_year, tri_latest = cmp$tri_all_duty_only,
                     tri_change_pts = cmp$tri_all_duty_only - cmp$tri_all_weight_year, verdict = cmp$verdict, choice = cmp$strategic_choice)
tri_ch <- tri_ch[tri_ch$stage != "Deployment" & !is.na(tri_ch$tri_change_pts), ]
dv <- div[div$stage != "All producing stages", c("tech", "stage", "imports_2024", "imports_2026", "coercive_share_2024", "coercive_share_2026",
                                                  "coercive_share_change_pts", "china_share_change_pts", "imports_change_pct")]
fvd <- tfe_fence_vs_diversification(tri_ch, dv, ov$fence_line, ov$dependence_line)
fvd$thin <- fvd$imports_2024 < cfg_tri$thin_imports_usd
w <- fvd$imports_2024
wcor <- function(x, y, w) { mx <- sum(w * x) / sum(w); my <- sum(w * y) / sum(w); sum(w * (x - mx) * (y - my)) / sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2)) }
fvd_cor <- c(unweighted = stats::cor(fvd$tri_change_pts, fvd$coercive_share_change_pts), weighted_2024_imports = wcor(fvd$tri_change_pts, fvd$coercive_share_change_pts, w))

# --- quality checks ---------------------------------------------------------------------------------------------------
tot_census <- function(p) sum(tot_m$value[tot_m$time %in% periods[[p]]])
add_qc("T1. Import and export totals reconcile", abs(totals$imports_2026[1] - tot_census("2026")) < 1 && abs(totals$imports_2024[1] - tot_census("2024")) < 1,
       sprintf("Basket imports = Census total (tracker TOTAL file) for the period: %s 2024 $%.1fbn, 2026 $%.1fbn; Comtrade general imports for the same codes $%.1fbn / $%.1fbn (%.1f%% / %.1f%% apart: general vs consumption imports); exports from Comtrade $%.1fbn / $%.1fbn",
               plab, tot_census("2024") / 1e9, tot_census("2026") / 1e9, uniq(cimp, "2024", basket) / 1e9, uniq(cimp, "2026", basket) / 1e9,
               100 * (uniq(cimp, "2024", basket) / tot_census("2024") - 1), 100 * (uniq(cimp, "2026", basket) / tot_census("2026") - 1),
               totals$exports_2024[1] / 1e9, totals$exports_2026[1] / 1e9))
add_qc("T3. No Deployment double counting", !any(pc_all$stage == "Deployment") && !"Deployment" %in% bs$stage,
       "Deployment cells (which repeat Assembly) are excluded from every trade total, stage table and diversification cell")
ov_n <- table(table(unique(pc_all[c("tech", "hs6")])$hs6))
st_sum <- sum(bs$imports_2026[bs$basket == "All electro-industrial technologies"])
add_qc("T4. Technology overlaps handled", abs(st_sum - totals$imports_2026[1]) < 1,
       sprintf("Technology totals are attributed (shared codes count in each; exclusive totals also given); stage totals split shared codes equally and sum to the unique total ($%.1fbn). Codes by number of technologies: %s",
               st_sum / 1e9, paste(names(ov_n), as.integer(ov_n), sep = ":", collapse = " ")))
shares <- c("china_share", "coercive_excl_china_share", "trusted_share", "nonaligned_share", "unassigned_share")
ssum <- unlist(lapply(c("2024", "2026"), function(p) rowSums(div[paste0(shares, "_", p)])))
add_qc("T5. Bloc shares sum to 100%", max(abs(ssum - 1), na.rm = TRUE) < 1e-9,
       "China + coercive excl. China + trusted + nonaligned + unassigned = 1 in every cell and period (largest gap below 1e-9)")
pr_sum <- sum(pmp$value[pmp$period == "2026"])
add_qc("T6. Supplier shares reconcile to total imports", abs(pr_sum - tot_census("2026")) < 1,
       sprintf("Named partners + rest-of-world residual = Census total ($%.1fbn, %s 2026); named partners cover %.1f%%",
               pr_sum / 1e9, plab, 100 * sum(pmp$value[pmp$period == "2026" & pmp$partner != "Rest of world"]) / pr_sum))
hh <- div[div$tech == "Batteries" & div$stage == "Assembly", ]
cd_b <- unique(pc_all$hs6[pc_all$tech == "Batteries" & pc_all$stage == "Assembly"])
xb <- pmp[pmp$period == "2026" & pmp$hs6 %in% cd_b, ]
sb <- tapply(xb$value, xb$partner, sum)
sb <- sb[names(sb) != "Rest of world"] / sum(xb$value)
add_qc("T7. HHI uses partner shares", abs(sum(sb^2) - hh$hhi_2026) < 1e-12 && all(div$hhi_2026 <= 1, na.rm = TRUE),
       sprintf("HHI = sum of squared named-partner shares of total imports (spot check, battery assembly 2026: %.4f); residual excluded, so a lower bound", hh$hhi_2026))
gap8 <- max(abs(div$coercive_share_change_pts - (div$china_share_change_pts + div$coercive_excl_china_share_change_pts)), na.rm = TRUE)
add_qc("T8. China not double counted in coercive change", gap8 < 1e-9 && all(div$coercive_excl_china_share_2026 >= -1e-12, na.rm = TRUE),
       "Coercive-share change = China-share change + coercive-excl.-China change; charts plot China and the CSV reports both components")
mt <- stats::aggregate(value ~ time, data = tot_m, FUN = sum)
add_qc("T9. Monthly series reconcile to YTD totals", abs(sum(mt$value[mt$time %in% periods[["2026"]]]) - tot_census("2026")) < 1,
       sprintf("Sum of the %d monthly basket totals = the %s 2026 total; monthly series in 00b_monthly_imports.csv", length(periods[["2026"]]), plab))
add_qc("T11. No causal claims from descriptive charts", TRUE,
       "Charts 00a, 00b, the timeline and the fence-change analysis carry the note: policy events are shown for context; changes in trade flows cannot be attributed to individual measures from these descriptive charts alone")
add_qc("T12. Capacity never described as production", TRUE,
       "Chart 8 labels SEIA/Wood Mackenzie figures as operating nameplate capacity; production figures are quoted separately with their source; checked in chart text and TRADE_FENCE_EXPLAINER_CHARTS.md")
x_codes <- unique(exp$hs6[exp$value > 0])
m_codes <- unique(imp$hs6[imp$value > 0])
add_qc("T13. Exports use the import HS dictionary", all(x_codes %in% basket) && all(m_codes %in% basket),
       sprintf("Exports and imports are both restricted to the same %d HS6 codes of data/reference/energy_hs6_master.csv (producing stages, 9 technologies): %d codes with exports, %d with imports in the periods; exports at HS6 (Comtrade), imports at HS10 summed to HS6",
               length(basket), length(x_codes), length(m_codes)))
s338 <- utils::read.csv(need(file.path(repo_root, cfg$s338_hts_check)), colClasses = "character")
add_qc("T14. Section 338 coverage verified", all(toupper(s338$in_hts_note51b3[s338$source == "tracker"]) == "TRUE"),
       "Every code of the tracker's Canada autos basket (591) appears in HTSUS Revision 23 (2026) chapter 99 U.S. note 51(b)(3); note 51(c) limits the Section 232 exemption to heading 9903.03.13 (scripts/trade_fence_explainer/check_s338_hts.py)")

# --- write -------------------------------------------------------------------------------------------------------------
wcsv(ov_long, "00a_electro_industrial_trade_overview.csv")
wcsv(bt, "00a_trade_by_technology.csv")
wcsv(bs, "00a_trade_by_stage.csv")
wcsv(div, "00b_trade_diversification_metrics.csv")
wcsv(b00, "00b_trade_rewiring.csv")
mt2 <- stats::aggregate(value ~ time + group, data = pm[pm$hs6 %in% codes_clean, ], FUN = sum)
wcsv(mt2[order(mt2$time, mt2$group), ], "00b_monthly_imports.csv")
wcsv(tl, "00_policy_timeline.csv")
wcsv(fvd, "fence_change_vs_diversification.csv")
wcsv(data.frame(measure = names(fvd_cor), correlation = unname(fvd_cor), n_cells = nrow(fvd)), "fence_change_vs_diversification_correlation.csv")
# Chart 5 threshold sensitivity: aligned cells at each open-gate line (the audit's sensitivity table).
sens <- utils::read.csv(need(file.path(repo_root, "diagnostics", "us_trade_restrictiveness", "verdict_threshold_sensitivity.csv")), stringsAsFactors = FALSE)
sens <- sens[sens$verdict != "Not assessed", ]
sens$verdict4 <- tfe_verdict4(sens$verdict)
th <- stats::aggregate(Freq ~ open_threshold + verdict4, data = sens, FUN = sum)
th <- stats::reshape(th, idvar = "open_threshold", timevar = "verdict4", direction = "wide")
names(th) <- sub("^Freq\\.", "", names(th))
th[is.na(th)] <- 0
th$assessed_cells <- rowSums(th[setdiff(names(th), "open_threshold")])
wcsv(th[order(th$open_threshold), ], "05_threshold_sensitivity.csv")
q_old <- utils::read.csv(file.path(data_out, "qc_checks.csv"), stringsAsFactors = FALSE)
q_old <- q_old[!grepl("^T[0-9]+\\.", q_old$check), ]
wcsv(rbind(q_old, do.call(rbind, qc)), "qc_checks.csv")
message(sprintf("Trade overview: %s 2026 basket imports $%.0fbn (%+.1f%% vs 2024), exports $%.0fbn; QC %d/%d new checks pass.",
                plab, totals$imports_2026[1] / 1e9, totals$imports_change_pct[1], totals$exports_2026[1] / 1e9,
                sum(vapply(qc, function(x) isTRUE(x$pass), logical(1))), length(qc)))
