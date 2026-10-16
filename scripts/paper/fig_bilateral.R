# Figures and numbers for the bilateral partnership indices (Section 9, Results).
#
# Builds five figures and one table from the partner tables of the final rerun of 2026-10-12
# (data/processed/partner_*_tbl.rds, psi_*.rds, coalition_tbl.rds, geo_alignment_tbl.rds) and the
# raw inputs the partner themes read (data/raw/):
#   bilateral_fs_heatmap    Friendshore matrix among the 31 core coalition members (plus China as a
#                           supplier column), Batteries and Solar Midstream
#   bilateral_fs_components contributions of the six positively weighted Friendshore components
#                           (imports, reporter need, partner capability, outward investment, UNGA
#                           alignment, Chinese-parent exposure) and the climate penalty, top-10
#                           suppliers of the United States and Japan (mean over each dyad's five best cells)
#   bilateral_cmfdi_exposure Chinese-parent ownership exposure of suppliers (CM-FDI) in Solar, Wind,
#                           Batteries and EV Midstream, and its effect on the core members' ratings
#   bilateral_psi_rank      headline (core-member view) PSI and its three category scores for the 57
#                           coalition members, the highest-ranked and selected non-members, and the
#                           highest-scoring partners the rankability rule leaves unranked
#   bilateral_fs_po         FS(consumer <- supplier) against PO(supplier -> consumer), coalition dyads
#   paper/tables/bilateral_psi_views.tex  top-15 partners in the core, full-coalition and world views
# and writes every number the text quotes to paper/results/bilateral.json.
#
# Weights are read from config/weights.yml (partnership_friendshore_components: seven components,
# one with weight zero; partnership_opportunity_components; psi), the CIF/FOB factor and the CM-FDI
# settings from config/partnership.yml and the rankability threshold from config/rankability.yml.
# The risk component chinese_parent_exposure (stored as the Chinese-parent share) is scored 1 - share,
# as partnership_strength_friendshore_risk_components() prescribes.
#
# Quantities the pipeline does not store are rebuilt with its own functions (sourced from R/;
# nothing is written there) and checked against the stored tables before use:
#   * the reconciled trade matrix (partnership_strength_build_trade_cells(), importer's declaration
#     else the exporter's mirror x CIF/FOB), which gives each dyad cell's trade status (observed
#     trade, recorded zero, imputed at the cell mean, or no HS6 code) and, where observed, whether
#     the last-year flow is the importer's own declaration or a mirror; the rebuilt trade
#     components reproduce the stored ones and the status counts reproduce
#     partner_trade_coverage_tbl.rds;
#   * the Friendshore climate penalty 0.10 (1 - g_t)(CPI_i + CPI_j) and the Opportunity penalty
#     w (1 - g_t) CPI_j, from the IPCC intensities and the CAT ratings as scripts/15 prepares them;
#     the rebuilt indices reproduce the stored ones;
#   * the best-five summaries and the core-view PSI, which reproduce partner_*_top5_tbl.rds and
#     psi_tbl.rds.
# Three counterfactuals reuse that machinery: the PSI without mirror fill (trade components from
# importers' own declarations only), the PSI without the UNGA alignment component and the PSI without
# the Chinese-parent exposure component (weight set to zero). Both change one input and re-run every later step (S-curve within cell, best five,
# mean over the 31 core reporters, PSI).
#
# Rankings use the pipeline's rankability columns (psi_tbl$rank / rankable). Supplier lists rank a
# partner for a reporter only when the partner is sovereign (psi_tbl rank_exclusion) and the
# Friendshore dyad's imputed weight share (partner_friendshore_top5_tbl) is at most tau, the same
# rule applied to the Friendshore pillar alone. Correlations are Spearman rank correlations.
#
# Run from the repository root:  Rscript scripts/paper/fig_bilateral.R

find_repo_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "paper", "_paper_theme.R"))) {
    if (dirname(d) == d) stop("Run from inside the repository.")
    d <- dirname(d)
  }
  d
}
repo_root <- find_repo_root()
source(file.path(repo_root, "scripts", "paper", "_paper_theme.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(cowplot)
  library(ggrepel)
})
for (f in c("R/utils/scurve.R", "R/utils/schema.R", "R/utils/country.R",
            "R/themes/partnership_strength/partnership_strength_helpers.R",
            "R/themes/partnership_strength/prosperous_opportunity.R",
            "R/themes/partnership_strength/safer_friendshore.R",
            "R/categories/policy/cat_policy_index.R")) {
  source(file.path(repo_root, f))
}
data_path <- function(...) file.path(repo_root, "data", ...)
cfg_path <- function(...) file.path(repo_root, "config", ...)
read_proc <- function(name) as.data.table(readRDS(data_path("processed", paste0(name, ".rds"))))

# ---------------------------------------------------------------------------------------------
# Configuration (weights, CIF/FOB factor, rankability threshold)
# ---------------------------------------------------------------------------------------------
wcfg <- yaml::read_yaml(cfg_path("weights.yml"))
fs_comp <- partnership_strength_friendshore_components()
w_fs <- unlist(wcfg$partnership_friendshore_components)[fs_comp]
stopifnot(!anyNA(w_fs), length(w_fs) == length(fs_comp))
po_cfg <- wcfg$partnership_opportunity_components
w_po <- unlist(po_cfg[c("trade_index", "econ_opp_index", "partner_es_need")])
po_climate_weight <- po_cfg$climate_policy_weight
w_psi <- unlist(wcfg$psi)[c("friendshore", "opportunity", "development")]
stopifnot(!anyNA(w_po), !anyNA(w_psi), is.numeric(po_climate_weight))
pcfg <- yaml::read_yaml(cfg_path("partnership.yml"))
cif_fob <- if (is.null(pcfg$trade$cif_fob_factor)) PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR else as.numeric(pcfg$trade$cif_fob_factor)
rcfg <- yaml::read_yaml(cfg_path("rankability.yml"))
tau <- if (!is.null(rcfg$psi$max_imputed_weight_share)) rcfg$psi$max_imputed_weight_share else rcfg$max_imputed_weight_share
# The Friendshore penalty coefficient is fixed in code (safer_friendshore.R: penalty_r / penalty_p).
fs_penalty_coef <- 0.10
trade_years <- 2020:2024

# Labels of the positively weighted Friendshore components (econ_opp_raw has weight zero).
fs_comp_lab <- c(imp_trade_index = "Imports from j", es_need = "Reporter need",
                 eo_partner = "Partner opportunity (EO rank)", outbound_index = "Outward investment in j",
                 geo_alignment = "UNGA bloc alignment",
                 chinese_parent_exposure = "Chinese-parent exposure (1 - share)")
stopifnot(identical(partnership_strength_friendshore_risk_components(), "chinese_parent_exposure"))
stopifnot(setequal(names(fs_comp_lab), names(w_fs)[w_fs > 0]))

sc <- function(x) median_scurve(x)
spearman <- function(x, y) suppressWarnings(stats::cor(x, y, method = "spearman", use = "complete.obs"))
r2 <- function(x) round(x, 2)
r3 <- function(x) round(x, 3)
r4 <- function(x) round(x, 4)
pct <- function(x) round(100 * x, 1)
qsum <- function(x) {
  x <- x[!is.na(x)]
  list(n = length(x), mean = r4(mean(x)), sd = r4(sd(x)), p10 = r4(quantile(x, 0.10, names = FALSE)),
       p25 = r4(quantile(x, 0.25, names = FALSE)), median = r4(median(x)),
       p75 = r4(quantile(x, 0.75, names = FALSE)), p90 = r4(quantile(x, 0.90, names = FALSE)),
       min = r4(min(x)), max = r4(max(x)))
}
top5_mean <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) NA_real_ else mean(sort(v, decreasing = TRUE)[seq_len(min(5L, length(v)))])
}
res <- list()

# ---------------------------------------------------------------------------------------------
# Coalition and partner attributes
# ---------------------------------------------------------------------------------------------
coal_tbl <- read_proc("coalition_tbl")
core <- sort(coal_tbl[tier == "core", iso3])
partners_tier <- sort(coal_tbl[tier == "partner", iso3])
coalition <- c(core, partners_tier)
stopifnot(length(core) == 31, length(coalition) == 57)

psi <- read_proc("psi_tbl")[, .(partner_iso, name = Country, psi = value, rank, rankable, rank_exclusion,
                                iws = imputed_weight_share, fs_iws = friendshore_imputed_share,
                                po_iws = opportunity_imputed_share, dp_iws = development_imputed_share)]
stopifnot(!anyDuplicated(psi$partner_iso))
psi[, sovereign := is.na(rank_exclusion) | !grepl("not a sovereign", rank_exclusion)]
iso_name <- setNames(psi$name, psi$partner_iso)
short_names <- c("Dominican Republic" = "Dominican Rep.", "Congo, Dem. Rep." = "DR Congo",
                 "Democratic Republic of the Congo" = "DR Congo", "Bosnia and Herzegovina" = "Bosnia-Herz.",
                 "United Arab Emirates" = "UAE", "Slovak Republic" = "Slovakia",
                 "Czech Republic" = "Czechia", "St. Martin (French part)" = "St. Martin",
                 "Turks and Caicos Islands" = "Turks and Caicos", "British Virgin Islands" = "Br. Virgin Is.",
                 "Northern Mariana Islands" = "N. Mariana Is.")
nm <- function(iso) {
  out <- unname(ifelse(is.na(iso_name[iso]), iso, iso_name[iso]))
  ifelse(out %in% names(short_names), short_names[out], out)
}
income_tbl <- as.data.table(read.csv(data_path("raw", "wdi_country_info.csv")))[
  !income %in% c("Aggregates", ""), .(iso3c = as.character(iso3c), income = as.character(income))]

# Climate-policy ratings and technology emissions index, prepared as scripts/15 prepares them.
country_info <- standardize_country_info(read.csv(data_path("raw", "wdi_country_info.csv")))
tech_ghg <- as.data.table(partnership_strength_clean_ghg(read.csv(data_path("raw", "ipcc_ghg_intensity.csv"))))[,
  .(tech, ghg_index = as.numeric(ghg_index))]
policy_tbl <- partnership_strength_clean_policy(
  cat_prepare_ratings(read.csv(data_path("raw", "CAT_country ratings data.csv")), country_info = country_info),
  country_info = country_info
)
policy_iso <- as.data.table(policy_tbl %>%
  mutate(Country = partnership_strength_standardize_countries(Country),
         iso3c = partnership_strength_country_to_iso(Country, country_info)) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarize(cpi = mean(climate_policy_index, na.rm = TRUE), .groups = "drop"))
cpi_default <- median(policy_iso$cpi)
ghg_default <- median(tech_ghg$ghg_index, na.rm = TRUE)
res$climate_penalty <- list(n_rated_economies = nrow(policy_iso), cpi_default_unrated = cpi_default)

# ---------------------------------------------------------------------------------------------
# Reconciled trade matrix (all-reporter Comtrade dyads, mirror fill at the CIF/FOB factor)
# ---------------------------------------------------------------------------------------------
ct <- fread(data_path("raw", "comtrade_energy_dyads_all.csv"),
            select = c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value"),
            colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")),
            na.strings = c("", "NA"), showProgress = FALSE)
subcat <- read.csv(data_path("raw", "energy_hs6_master.csv"))
std <- as.data.table(partnership_strength_standardize_comtrade(as.data.frame(ct), years = range(trade_years)))
res$comtrade <- list(
  file = "data/raw/comtrade_energy_dyads_all.csv",
  rows = nrow(ct),
  years = sort(unique(std$period)),
  reporters_by_year = as.list(std[, .(n = uniqueN(reporter_iso)), by = period][order(period), setNames(n, period)]),
  economies_in_file = uniqueN(c(std$reporter_iso, std$partner_iso)),
  cif_fob_factor = cif_fob
)
rm(std); invisible(gc())
trade_cells <- as.data.table(partnership_strength_build_trade_cells(as.data.frame(ct), subcat, years = trade_years,
                                                                    cif_fob_factor = cif_fob))
rm(ct); invisible(gc())
# The pipeline's message: share of last-year observed (exporter, importer, cell) flows from mirrors.
res$comtrade$observed_flow_cells <- nrow(trade_cells)
res$comtrade$share_last_year_observed_from_mirror <- r4(trade_cells[!is.na(source_last), mean(source_last == "mirror")])
res$comtrade$share_last_year_observed_positive_from_mirror <-
  r4(trade_cells[!is.na(source_last) & value_last > 0, mean(source_last == "mirror")])
res$comtrade$cells_with_trade_codes <- trade_cells[, uniqueN(paste(tech, supply_chain))]
res$comtrade$importers_reporting_last_year <- trade_cells[source_last == "importer", uniqueN(importer_iso)]

# Trade indices with and without mirror fill. Without mirrors = importers' own declarations only
# (partnership_strength_build_trade_cells(mirror = FALSE) chooses the source per importer, cell and
# year, so dropping the mirror-sourced year values gives the same matrix).
trade_idx <- function(tc, view) {
  ds <- partnership_strength_trade_view(as.data.frame(tc), view = view)
  out <- if (view == "import") partnership_strength_build_import_indices(ds) else partnership_strength_build_export_indices(ds)
  out <- as.data.table(out)
  setnames(out, intersect(c("imp_trade_index", "trade_index"), names(out)), "tidx")
  out
}
imp_idx <- trade_idx(trade_cells, "import")
exp_idx <- trade_idx(trade_cells, "export")
tc_nm <- copy(trade_cells)
tc_nm[source_first %in% "mirror", value_first := NA_real_]
tc_nm[source_last %in% "mirror", value_last := NA_real_]
imp_nm <- trade_idx(tc_nm, "import")
exp_nm <- trade_idx(tc_nm, "export")
rm(tc_nm); invisible(gc())
src_last <- trade_cells[!is.na(source_last), .(exporter_iso, importer_iso, tech, supply_chain, src = source_last)]
trade_data_cells <- unique(trade_cells[, paste(tech, supply_chain)])
rm(trade_cells); invisible(gc())

# ---------------------------------------------------------------------------------------------
# Friendshore cells: components, trade status, penalty; reproduction of the stored index
# ---------------------------------------------------------------------------------------------
fs_long <- read_proc("partner_friendshore_tbl")[, .(reporter_iso, partner_iso, tech, supply_chain, variable, value)]
invisible(gc())
fs_n_long <- nrow(fs_long)
fs_dups <- sum(duplicated(fs_long, by = c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable")))
fsw <- dcast(fs_long, reporter_iso + partner_iso + tech + supply_chain ~ variable, value.var = "value")
rm(fs_long); invisible(gc())
setnames(fsw, "Friendshore Index", "fs")
stopifnot(fs_dups == 0, all(fs_comp %in% names(fsw)))
# Risk component: keep the stored share and score it as 1 - share, as the pipeline does.
fsw[, cpe_share := chinese_parent_exposure]
fsw[, chinese_parent_exposure := 1 - chinese_parent_exposure]
# Cells whose outward-investment tie comes from CM-FDI projects (the rest from IMF DIP positions).
cmfdi_keys <- read_proc("partner_cm_fdi_coverage_tbl")[, paste(tech, supply_chain)]
fsw[, cmfdi_cell := paste(tech, supply_chain) %in% cmfdi_keys]

# Trade status and source of the import component.
fsw <- merge(fsw, imp_idx[, .(reporter_iso, partner_iso, tech, supply_chain, imp_rb = tidx, status = trade_status)],
             by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), all.x = TRUE)
fsw <- merge(fsw, imp_nm[, .(reporter_iso, partner_iso, tech, supply_chain, imp_nm = tidx)],
             by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), all.x = TRUE)
fsw <- merge(fsw, src_last[, .(reporter_iso = importer_iso, partner_iso = exporter_iso, tech, supply_chain, src)],
             by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), all.x = TRUE)
fsw[is.na(status), status := "imputed"]
fsw[, has_trade_data := any(status != "imputed"), by = .(tech, supply_chain)]
fsw[has_trade_data == FALSE, status := "no_data"]
fsw[status %in% c("imputed", "no_data"), src := NA_character_]
fsw[, obs_imp := status %in% c("trade", "zero")]
imp_dev <- fsw[obs_imp == TRUE, max(abs(imp_rb - imp_trade_index))]
fsw[, cell_mean_obs := if (any(obs_imp)) mean(imp_trade_index[obs_imp]) else 0.5, by = .(tech, supply_chain)]
imp_impute_dev <- fsw[obs_imp == FALSE, max(abs(imp_trade_index - cell_mean_obs))]
stopifnot(imp_dev < 1e-9, imp_impute_dev < 1e-9)
# Without mirrors: observed only where the importer's own declaration exists; else the cell mean.
fsw[, imp_nm_obs := !is.na(imp_nm)]
fsw[, imp_trade_nm := fifelse(imp_nm_obs, imp_nm, if (any(imp_nm_obs)) mean(imp_nm[imp_nm_obs]) else 0.5),
    by = .(tech, supply_chain)]
fsw[, c("imp_rb", "imp_nm", "cell_mean_obs") := NULL]

# Coverage reproduction (partner_trade_coverage_tbl).
cov_tbl <- read_proc("partner_trade_coverage_tbl")
cov_fs <- fsw[, .(n_cells = .N, n_trade = sum(status == "trade"), n_zero = sum(status == "zero"),
                  n_imputed = sum(status == "imputed"), n_no_data = sum(status == "no_data")),
              by = .(tech, supply_chain)]
cmp_cov <- merge(cov_fs, cov_tbl[theme == "friendshore"], by = c("tech", "supply_chain"), suffixes = c("", ".p"))
stopifnot(nrow(cmp_cov) == nrow(cov_fs),
          all(cmp_cov$n_cells == cmp_cov$n_cells.p), all(cmp_cov$n_trade == cmp_cov$n_trade.p),
          all(cmp_cov$n_zero == cmp_cov$n_zero.p), all(cmp_cov$n_imputed == cmp_cov$n_imputed.p))

# Penalty and composite.
fsw <- merge(fsw, tech_ghg, by = "tech", all.x = TRUE)
fsw[is.na(ghg_index), ghg_index := ghg_default]
fsw[, cpi_r := policy_iso$cpi[match(reporter_iso, policy_iso$iso3c)]]
fsw[, cpi_p := policy_iso$cpi[match(partner_iso, policy_iso$iso3c)]]
fsw[is.na(cpi_r), cpi_r := cpi_default]
fsw[is.na(cpi_p), cpi_p := cpi_default]
fsw[, pen := fs_penalty_coef * (1 - ghg_index) * (cpi_r + cpi_p)]
fsw[, c("cpi_r", "cpi_p") := NULL]
# Sum of the weights of the components present (the denominator of the available-case mean).
fsw[, wsum := Reduce(`+`, lapply(fs_comp, function(k) (!is.na(get(k))) * w_fs[[k]]))]
for (k in names(fs_comp_lab)) {
  set(fsw, j = paste0("c_", k), value = fifelse(is.na(fsw[[k]]), 0, w_fs[[k]] * fsw[[k]]) / fsw$wsum)
}
fsw[, fsi_raw := Reduce(`+`, lapply(names(fs_comp_lab), function(k) get(paste0("c_", k))))]
fs_index <- function(d, trade_col = "imp_trade_index", w = w_fs) {
  raw <- partnership_strength_row_weighted_mean(
    lapply(fs_comp, function(k) if (k == "imp_trade_index") d[[trade_col]] else d[[k]]), unname(w[fs_comp]))
  x <- pmax(0, raw - d$pen)
  ave(x, d$tech, d$supply_chain, FUN = sc)
}
fsw[, fs_rb := fs_index(fsw)]
fs_dev <- abs(fsw$fs_rb - fsw$fs)
res$checks <- list(
  fs_long_rows = fs_n_long, fs_duplicate_keys = fs_dups,
  fs_rebuilt_max_abs_dev = signif(max(fs_dev), 3), fs_rebuilt_mean_abs_dev = signif(mean(fs_dev), 3),
  fs_rebuilt_rows_dev_gt_1e9 = sum(fs_dev > 1e-9),
  fs_trade_observed_max_abs_dev = signif(imp_dev, 3),
  fs_trade_imputed_equals_cell_mean_max_abs_dev = signif(imp_impute_dev, 3),
  fs_trade_status_counts_match_coverage_tbl = TRUE
)
stopifnot(mean(fs_dev) < 1e-6)
fsw[, c("fs_rb") := NULL]
invisible(gc())

# ---------------------------------------------------------------------------------------------
# Opportunity cells
# ---------------------------------------------------------------------------------------------
po_long <- read_proc("partner_opportunity_tbl")[, .(reporter_iso, partner_iso, tech, supply_chain, variable, value)]
po_dups <- sum(duplicated(po_long, by = c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable")))
pow <- dcast(po_long, reporter_iso + partner_iso + tech + supply_chain ~ variable, value.var = "value")
rm(po_long); invisible(gc())
setnames(pow, "Opportunity Index", "po")
pow <- merge(pow, exp_idx[, .(reporter_iso, partner_iso, tech, supply_chain, exp_rb = tidx, status = trade_status)],
             by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), all.x = TRUE)
pow <- merge(pow, exp_nm[, .(reporter_iso, partner_iso, tech, supply_chain, exp_nm = tidx)],
             by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), all.x = TRUE)
pow <- merge(pow, src_last[, .(reporter_iso = exporter_iso, partner_iso = importer_iso, tech, supply_chain, src)],
             by = c("reporter_iso", "partner_iso", "tech", "supply_chain"), all.x = TRUE)
rm(src_last); invisible(gc())
pow[is.na(status), status := "imputed"]
pow[, has_trade_data := any(status != "imputed"), by = .(tech, supply_chain)]
pow[has_trade_data == FALSE, status := "no_data"]
pow[status %in% c("imputed", "no_data"), src := NA_character_]
pow[, obs_exp := status %in% c("trade", "zero")]
exp_dev <- pow[obs_exp == TRUE, max(abs(exp_rb - trade_index))]
# The pipeline imputes at the mean over the dyad backbone before dropping dyads without a reporter
# EO or partner ES value, so the mean over the kept dyads can differ slightly.
pow[, cell_mean_obs := if (any(obs_exp)) mean(trade_index[obs_exp]) else 0.5, by = .(tech, supply_chain)]
exp_impute_dev <- pow[obs_exp == FALSE, max(abs(trade_index - cell_mean_obs))]
stopifnot(exp_dev < 1e-9)
pow[, exp_nm_obs := !is.na(exp_nm)]
pow[, trade_nm := fifelse(exp_nm_obs, exp_nm, if (any(exp_nm_obs)) mean(exp_nm[exp_nm_obs]) else 0.5),
    by = .(tech, supply_chain)]
pow[, c("exp_rb", "exp_nm", "cell_mean_obs") := NULL]
cov_po <- pow[, .(n_cells = .N, n_trade = sum(status == "trade"), n_zero = sum(status == "zero"),
                  n_imputed = sum(status == "imputed")), by = .(tech, supply_chain)]
cmp_cov <- merge(cov_po, cov_tbl[theme == "opportunity"], by = c("tech", "supply_chain"), suffixes = c("", ".p"))
stopifnot(nrow(cmp_cov) == nrow(cov_po), all(cmp_cov$n_cells == cmp_cov$n_cells.p),
          all(cmp_cov$n_trade == cmp_cov$n_trade.p), all(cmp_cov$n_imputed == cmp_cov$n_imputed.p))
pow <- merge(pow, tech_ghg, by = "tech", all.x = TRUE)
pow[is.na(ghg_index), ghg_index := ghg_default]
pow[, cpi_p := policy_iso$cpi[match(partner_iso, policy_iso$iso3c)]]
pow[is.na(cpi_p), cpi_p := cpi_default]
pow[, pen := po_climate_weight * (1 - ghg_index) * cpi_p]
pow[, cpi_p := NULL]
po_index <- function(d, trade_col = "trade_index") {
  raw <- (w_po[["trade_index"]] * d[[trade_col]] + w_po[["econ_opp_index"]] * d$econ_opp_index +
            w_po[["partner_es_need"]] * d$partner_es_need) / sum(w_po)
  ave(pmax(0, raw - d$pen), d$tech, d$supply_chain, FUN = sc)
}
po_dev <- abs(po_index(pow) - pow$po)
res$checks$po_duplicate_keys <- po_dups
res$checks$po_rebuilt_max_abs_dev <- signif(max(po_dev), 3)
res$checks$po_rebuilt_mean_abs_dev <- signif(mean(po_dev), 3)
res$checks$po_trade_observed_max_abs_dev <- signif(exp_dev, 3)
res$checks$po_trade_imputed_vs_mean_of_kept_observed_max_abs_dev <- signif(exp_impute_dev, 3)
stopifnot(po_dups == 0, mean(po_dev) < 1e-6)
rm(po_dev); invisible(gc())

# ---------------------------------------------------------------------------------------------
# Development, best-five summaries and the PSI views; reproduction of the stored PSI
# ---------------------------------------------------------------------------------------------
dp_long <- read_proc("partner_development_tbl")[, .(partner_iso, tech, supply_chain, variable, value)]
stopifnot(!anyDuplicated(dp_long[, .(partner_iso, tech, supply_chain, variable)]))
dpw <- dcast(dp_long, partner_iso + tech + supply_chain ~ variable, value.var = "value")
rm(dp_long)
setnames(dpw, "Development Potential Index", "dp")
dp_top <- read_proc("partner_development_top5_tbl")[variable == "Development Potential Index",
                                                     .(partner_iso, tdp = value, dp_share = imputed_weight_share)]
fs_top <- read_proc("partner_friendshore_top5_tbl")[variable == "Friendshore Index",
                                                    .(reporter_iso, partner_iso, tfs = value, fs_share = imputed_weight_share)]
po_top <- read_proc("partner_opportunity_top5_tbl")[variable == "Opportunity Index",
                                                    .(reporter_iso, partner_iso, tpo = value, po_share = imputed_weight_share)]
t5 <- function(d, col) d[, .(t = top5_mean(get(col))), by = .(reporter_iso, partner_iso)]
chk_fs <- merge(t5(fsw, "fs"), fs_top, by = c("reporter_iso", "partner_iso"))
chk_po <- merge(t5(pow, "po"), po_top, by = c("reporter_iso", "partner_iso"))
res$checks$best5_T_FS_rebuilt_max_abs_dev <- signif(max(abs(chk_fs$t - chk_fs$tfs)), 3)
res$checks$best5_T_PO_rebuilt_max_abs_dev <- signif(max(abs(chk_po$t - chk_po$tpo)), 3)
stopifnot(nrow(chk_fs) == nrow(fs_top), nrow(chk_po) == nrow(po_top),
          max(abs(chk_fs$t - chk_fs$tfs)) < 1e-9, max(abs(chk_po$t - chk_po$tpo)) < 1e-9)
rm(chk_fs, chk_po)

# Partner-level PSI of a view from dyad summaries (tf: reporter, partner, t for FS; tp for PO):
# mean over the view's reporters, Development from the partner's own summary, weights
# renormalized over the categories present.
psi_view <- function(tf, tp, reporters = NULL) {
  if (!is.null(reporters)) {
    tf <- tf[reporter_iso %in% reporters]
    tp <- tp[reporter_iso %in% reporters]
  }
  z <- merge(tf[!is.na(t), .(cfs = mean(t)), by = partner_iso], tp[!is.na(t), .(cpo = mean(t)), by = partner_iso],
             by = "partner_iso", all = TRUE)
  z <- merge(z, dp_top[, .(partner_iso, cdp = tdp)], by = "partner_iso", all.x = TRUE)
  m <- as.matrix(z[, .(cfs, cpo, cdp)])
  w <- matrix(w_psi, nrow(m), 3, byrow = TRUE)
  z[, psi_x := rowSums(ifelse(is.na(m), 0, m * w)) / rowSums(ifelse(is.na(m), 0, w))]
  z
}
base_tf <- fs_top[, .(reporter_iso, partner_iso, t = tfs)]
base_tp <- po_top[, .(reporter_iso, partner_iso, t = tpo)]
v_core <- psi_view(base_tf, base_tp, core)
chk <- merge(v_core, psi[, .(partner_iso, psi)], by = "partner_iso")
res$checks$psi_core_view_rebuilt_max_abs_dev <- signif(max(abs(chk$psi_x - chk$psi)), 3)
stopifnot(nrow(chk) == nrow(psi), max(abs(chk$psi_x - chk$psi)) < 1e-9)
psi <- merge(psi, v_core[, .(partner_iso, friendshore = cfs, opportunity = cpo, development = cdp)], by = "partner_iso")
psi[, is_member := partner_iso %in% coalition]
psi[, tier := fifelse(partner_iso %in% core, "core", fifelse(partner_iso %in% partners_tier, "partner", "none"))]
psi[, income := income_tbl$income[match(partner_iso, income_tbl$iso3c)]]
psi[, income_group := fifelse(is.na(income), "Not on WB list",
                              fifelse(income == "High income", "High income", "Other WB income group"))]
rk <- psi[rankable == TRUE]
rm(chk)

# ---------------------------------------------------------------------------------------------
# Coverage of the three pillars
# ---------------------------------------------------------------------------------------------
fs_td <- fsw[has_trade_data == TRUE]
po_td <- pow[has_trade_data == TRUE]
dip_reporters <- fsw[!is.na(outbound_index) & cmfdi_cell == FALSE, unique(reporter_iso)]
conduits <- partnership_strength_conduit_centres()$iso3
stopifnot(!any(fsw[!is.na(outbound_index) & cmfdi_cell == FALSE, partner_iso %in% conduits | reporter_iso %in% conduits]))
geo <- read_proc("geo_alignment_tbl")[, .(reporter_iso, partner_iso, basis_reporter, basis_partner, bloc_reporter, bloc_partner)]
assigned <- unique(c(geo[basis_reporter != "own_votes", reporter_iso], geo[basis_partner != "own_votes", partner_iso]))
fsw[, geo_assigned := reporter_iso %in% assigned | partner_iso %in% assigned]
res$coverage <- list(
  friendshore = list(
    reporters = uniqueN(fsw$reporter_iso), partners = uniqueN(fsw$partner_iso),
    pairs = uniqueN(fsw[, .(reporter_iso, partner_iso)]), cells = nrow(fsw),
    tech_stage_cells = uniqueN(fsw[, .(tech, supply_chain)]),
    tech_stage_cells_with_trade_codes = uniqueN(fs_td[, .(tech, supply_chain)]),
    trade_cells_by_stage = as.list(table(factor(unique(fs_td[, .(tech, supply_chain)])$supply_chain,
                                                c("Upstream", "Midstream", "Downstream")))),
    trade_cells = sort(unique(fs_td[, paste(tech, supply_chain)])),
    cells_in_trade_cells = nrow(fs_td),
    share_trade_observed = r4(mean(fs_td$obs_imp)),
    share_trade_positive = r4(mean(fs_td$status == "trade")),
    share_trade_recorded_zero = r4(mean(fs_td$status == "zero")),
    share_trade_imputed = r4(mean(fs_td$status == "imputed")),
    share_observed_from_mirror = r4(fs_td[obs_imp == TRUE, mean(src == "mirror")]),
    share_trade_imputed_without_mirror = r4(mean(!fs_td$imp_nm_obs)),
    core_reporters = list(
      share_trade_imputed = r4(fs_td[reporter_iso %in% core, mean(status == "imputed")]),
      share_observed_from_mirror = r4(fs_td[reporter_iso %in% core & obs_imp == TRUE, mean(src == "mirror")]),
      share_trade_imputed_without_mirror = r4(fs_td[reporter_iso %in% core, mean(!imp_nm_obs)])),
    noncore_reporters = list(
      share_trade_imputed = r4(fs_td[!reporter_iso %in% core, mean(status == "imputed")]),
      share_trade_imputed_without_mirror = r4(fs_td[!reporter_iso %in% core, mean(!imp_nm_obs)])),
    reporters_with_observed_trade = uniqueN(fs_td[obs_imp == TRUE, reporter_iso]),
    reporters_with_own_import_declarations = uniqueN(fs_td[src %in% "importer", reporter_iso]),
    reporters_filing_dip = length(dip_reporters),
    pairs_with_dip_position = uniqueN(fsw[!is.na(outbound_index) & cmfdi_cell == FALSE, .(reporter_iso, partner_iso)]),
    share_cells_with_investment = r4(mean(!is.na(fsw$outbound_index))),
    share_dip_cells_with_investment = r4(fsw[cmfdi_cell == FALSE, mean(!is.na(outbound_index))]),
    share_cmfdi_cells_with_investment_tie = r4(fsw[cmfdi_cell == TRUE, mean(!is.na(outbound_index))]),
    share_cmfdi_cells_with_chinese_parent_exposure = r4(fsw[cmfdi_cell == TRUE, mean(!is.na(cpe_share))]),
    cmfdi_cells = sort(cmfdi_keys),
    core_reporters_filing_dip = sort(intersect(core, dip_reporters)),
    core_reporters_not_filing_dip = sort(setdiff(core, dip_reporters)),
    share_cells_need_missing = r4(mean(is.na(fsw$es_need))),
    cells_need_missing_for_every_reporter = sort(fsw[, .(m = all(is.na(es_need))), by = .(tech, supply_chain)][m == TRUE, paste(tech, supply_chain)]),
    share_cells_alignment_missing = r4(mean(is.na(fsw$geo_alignment))),
    economies_with_assigned_alignment = length(assigned),
    share_cells_alignment_assigned = r4(mean(fsw$geo_assigned)),
    conduit_centres_excluded = conduits
  ),
  opportunity = list(
    reporters = uniqueN(pow$reporter_iso), partners = uniqueN(pow$partner_iso),
    pairs = uniqueN(pow[, .(reporter_iso, partner_iso)]), cells = nrow(pow),
    tech_stage_cells = uniqueN(pow[, .(tech, supply_chain)]),
    tech_stage_cells_with_trade_codes = uniqueN(po_td[, .(tech, supply_chain)]),
    share_trade_observed = r4(mean(po_td$obs_exp)),
    share_trade_imputed = r4(mean(po_td$status == "imputed")),
    share_observed_from_mirror = r4(po_td[obs_exp == TRUE, mean(src == "mirror")]),
    share_trade_imputed_without_mirror = r4(mean(!po_td$exp_nm_obs)),
    core_reporters = list(
      share_trade_imputed = r4(po_td[reporter_iso %in% core, mean(status == "imputed")]),
      share_observed_from_mirror = r4(po_td[reporter_iso %in% core & obs_exp == TRUE, mean(src == "mirror")]),
      share_trade_imputed_without_mirror = r4(po_td[reporter_iso %in% core, mean(!exp_nm_obs)])),
    share_fewer_cells_than_fs = r3(1 - nrow(pow) / nrow(fsw))
  ),
  development = list(
    partners = uniqueN(dpw$partner_iso), cells = nrow(dpw),
    partners_with_stage_aid = as.list(dpw[, .(n = uniqueN(partner_iso[!is.na(aid_index_supplychain)])),
                                          by = supply_chain][, setNames(n, supply_chain)])
  ),
  psi = list(partners = nrow(psi), dyads = nrow(read_proc("psi_dyad_tbl")))
)
rm(fs_td, po_td); invisible(gc())

# Imputation behind the rankings.
res$imputation <- list(
  tau = tau,
  fs_dyad_share_all = qsum(fs_top$fs_share),
  fs_dyad_share_core_reporters = qsum(fs_top[reporter_iso %in% core, fs_share]),
  po_dyad_share_core_reporters = qsum(po_top[reporter_iso %in% core, po_share]),
  dp_partner_share = qsum(dp_top$dp_share),
  psi_core_view_share_rankable = qsum(rk$iws),
  psi_core_view_ranked = nrow(rk), psi_core_view_scored = nrow(psi),
  psi_core_view_not_sovereign = psi[sovereign == FALSE, .N],
  psi_core_view_sovereign_over_tau = psi[sovereign == TRUE & rankable == FALSE, sort(partner_iso)],
  psi_core_view_unranked_top_by_value = lapply(seq_len(5), function(k) with(psi[rankable == FALSE][order(-psi)][k],
    list(iso3 = partner_iso, name = name, psi = r3(psi), share = r3(iws), reason = rank_exclusion))),
  psi_core_view_rank_if_ranked_highest_unranked = {
    z <- psi[rankable == FALSE][order(-psi)][1]
    sum(rk$psi > z$psi) + 1L
  }
)

# ---------------------------------------------------------------------------------------------
# Counterfactual PSI (core view): without mirror fill, and without the alignment component
# ---------------------------------------------------------------------------------------------
core_t5 <- function(d, v) {
  d <- data.table(reporter_iso = d$reporter_iso, partner_iso = d$partner_iso, v = v)[reporter_iso %in% core]
  d[, .(t = top5_mean(v)), by = .(reporter_iso, partner_iso)]
}
fs_nomirror <- fs_index(fsw, trade_col = "imp_trade_nm")
po_nomirror <- po_index(pow, trade_col = "trade_nm")
w_fs_noalign <- w_fs; w_fs_noalign[["geo_alignment"]] <- 0
fs_noalign <- fs_index(fsw, w = w_fs_noalign)
tf_nm <- core_t5(fsw, fs_nomirror); tp_nm <- core_t5(pow, po_nomirror)
tf_na <- core_t5(fsw, fs_noalign)
v_nm <- psi_view(tf_nm, tp_nm)
v_na <- psi_view(tf_na, base_tp, core)
cf_compare <- function(v, label) {
  z <- merge(rk[, .(partner_iso, psi, rank, friendshore, opportunity, is_member, tier)],
             v[, .(partner_iso, psi_x, cfs_x = cfs, cpo_x = cpo)], by = "partner_iso")
  z[, rank_x := frank(-psi_x, ties.method = "min")]
  z[, rk_fs := frank(-friendshore, ties.method = "min")][, rk_fs_x := frank(-cfs_x, ties.method = "min")]
  z[, d := rank_x - rank]
  top15 <- z[order(rank)][1:15, partner_iso]; top15_x <- z[order(rank_x)][1:15, partner_iso]
  list(
    variant = label, n = nrow(z),
    spearman_psi = r3(spearman(z$psi, z$psi_x)),
    spearman_C_FS = r3(spearman(z$friendshore, z$cfs_x)),
    spearman_C_PO = r3(spearman(z$opportunity, z$cpo_x)),
    median_abs_rank_change = median(abs(z$d)), max_abs_rank_change = max(abs(z$d)),
    top15 = top15_x, top15_overlap_with_headline = length(intersect(top15, top15_x)),
    top15_entering = setdiff(top15_x, top15), top15_leaving = setdiff(top15, top15_x),
    largest_gains = lapply(seq_len(6), function(k) with(z[order(d)][k], list(iso3 = partner_iso, name = nm(partner_iso),
                                                                          rank = rank, rank_x = rank_x))),
    largest_losses = lapply(seq_len(6), function(k) with(z[order(-d)][k], list(iso3 = partner_iso, name = nm(partner_iso),
                                                                            rank = rank, rank_x = rank_x))),
    members_median_rank = median(z[is_member == TRUE, rank]), members_median_rank_x = median(z[is_member == TRUE, rank_x]),
    core_median_rank = median(z[tier == "core", rank]), core_median_rank_x = median(z[tier == "core", rank_x]),
    core_in_top25 = z[tier == "core" & rank <= 25, .N], core_in_top25_x = z[tier == "core" & rank_x <= 25, .N],
    key = lapply(setNames(c("CHN", "TWN", "CHE", "IND", "VNM", "IDN", "RUS"), c("CHN", "TWN", "CHE", "IND", "VNM", "IDN", "RUS")),
                 function(k) with(z[partner_iso == k], list(rank = rank, rank_x = rank_x, rank_C_FS = rk_fs, rank_C_FS_x = rk_fs_x)))
  )
}
res$counterfactual_no_mirror <- cf_compare(v_nm, "trade components from importers' own declarations only")
res$counterfactual_no_alignment <- cf_compare(v_na, "geo_alignment weight set to 0")
# Cell level: how much the alignment term moves FS within cells (all dyads, and core reporters).
res$counterfactual_no_alignment$cell_level_spearman_mean_within_cell <-
  r3(mean(fsw[, .(r = spearman(fs, fs_noalign[.I])), by = .(tech, supply_chain)]$r))
res$counterfactual_no_mirror$cell_level_fs_spearman_mean_within_cell_noncore_reporters <-
  r3(mean(data.table(fsw[, .(tech, supply_chain, reporter_iso, fs)], x = fs_nomirror)[!reporter_iso %in% core,
    .(r = spearman(fs, x)), by = .(tech, supply_chain)]$r))
res$counterfactual_no_mirror$cell_level_po_spearman_mean_within_cell_core_reporters <-
  r3(mean(data.table(pow[, .(tech, supply_chain, reporter_iso, po)], x = po_nomirror)[reporter_iso %in% core,
    .(r = spearman(po, x)), by = .(tech, supply_chain)]$r))

# Sensitivities of the core-view PSI (review of 2026-10-15): (i) an absent outward-investment record
# (IMF position or CM-FDI tie) scored as the lowest rank (0) instead of dropping out of the
# available-case mean; (ii) continuous alignment p_i = s_i (A_ij = s_i s_j + (1 - s_i)(1 - s_j));
# (iii) tercile blocs instead of quartile blocs. Each swaps one column of fsw, rebuilds FS and the
# core view with the pipeline's rule, and restores the column.
us_top10 <- function(tf_r) {
  z <- merge(tf_r[reporter_iso == "USA"], psi[, .(partner_iso, sovereign)], by = "partner_iso")
  z <- merge(z, fs_top[reporter_iso == "USA", .(partner_iso, fs_share)], by = "partner_iso")
  z[sovereign == TRUE & fs_share <= tau + 1e-9][order(-t)][1:10, partner_iso]
}
orig_out <- copy(fsw$outbound_index)   # copy(): := below must not alter the saved vector
fsw[, outbound_index := fifelse(is.na(orig_out), 0, orig_out)]
fs_inv0 <- fs_index(fsw)
fsw[, outbound_index := orig_out]; rm(orig_out)
tf_inv0 <- core_t5(fsw, fs_inv0)
v_inv0 <- psi_view(tf_inv0, base_tp, core)
res$counterfactual_investment_missing_lowest <- cf_compare(v_inv0, "absent outward-investment record scored as the lowest rank (0)")
res$counterfactual_investment_missing_lowest$us_top10_headline <- us_top10(base_tf)
res$counterfactual_investment_missing_lowest$us_top10_variant <- us_top10(tf_inv0)
res$counterfactual_investment_missing_lowest$us_top10_headline_without_investment_in_best5 <-
  us_top10(base_tf)[vapply(us_top10(base_tf), function(j) fsw[reporter_iso == "USA" & partner_iso == j][order(-fs)][
    1:5, all(is.na(outbound_index))], logical(1))]
rm(fs_inv0, tf_inv0, v_inv0); invisible(gc())

geo_s <- read_proc("geo_alignment_tbl")[, .(reporter_iso, partner_iso, s_reporter, s_partner, p_reporter, p_partner,
                                             basis_reporter, basis_partner, geo_alignment_tbl = geo_alignment)]
s_econ <- unique(rbind(geo_s[, .(iso = reporter_iso, s = s_reporter, p = p_reporter, basis = basis_reporter)],
                       geo_s[, .(iso = partner_iso, s = s_partner, p = p_partner, basis = basis_partner)]))
s_econ <- unique(s_econ, by = "iso")
s_own <- s_econ[basis == "own_votes" & !is.na(s), s]
terc <- unname(stats::quantile(s_own, c(1 / 3, 2 / 3)))
s_econ[, s_cont := fifelse(is.na(s), p, pmin(1, pmax(0, s)))]
s_econ[, p_terc := fifelse(is.na(s), p, fifelse(s >= terc[2], 1, fifelse(s <= terc[1], 0, 0.5)))]
idx_r <- match(fsw$reporter_iso, s_econ$iso); idx_p <- match(fsw$partner_iso, s_econ$iso)
orig_geo <- copy(fsw$geo_alignment)
a_cont <- s_econ$s_cont[idx_r] * s_econ$s_cont[idx_p] + (1 - s_econ$s_cont[idx_r]) * (1 - s_econ$s_cont[idx_p])
a_terc <- s_econ$p_terc[idx_r] * s_econ$p_terc[idx_p] + (1 - s_econ$p_terc[idx_r]) * (1 - s_econ$p_terc[idx_p])
a_quart <- s_econ$p[idx_r] * s_econ$p[idx_p] + (1 - s_econ$p[idx_r]) * (1 - s_econ$p[idx_p])
ok_geo <- !is.na(orig_geo)
stopifnot(max(abs(a_quart[ok_geo] - orig_geo[ok_geo])) < 1e-9)
fsw[, geo_alignment := fifelse(is.na(orig_geo), NA_real_, a_cont)]
v_cont <- psi_view(core_t5(fsw, fs_index(fsw)), base_tp, core)
fsw[, geo_alignment := fifelse(is.na(orig_geo), NA_real_, a_terc)]
v_terc <- psi_view(core_t5(fsw, fs_index(fsw)), base_tp, core)
fsw[, geo_alignment := orig_geo]
rm(orig_geo, a_cont, a_terc, a_quart, idx_r, idx_p, geo_s); invisible(gc())
res$counterfactual_alignment_continuous <- cf_compare(v_cont, "continuous alignment p_i = s_i")
res$counterfactual_alignment_terciles <- cf_compare(v_terc, "tercile blocs (p_i = 1 above the upper tercile of s, 0 below the lower)")
res$counterfactual_alignment_terciles$tercile_cuts_s <- r3(terc)
res$counterfactual_alignment_terciles$blocs_own_votes <- list(
  us_leaning = sum(s_own >= terc[2]), china_leaning = sum(s_own <= terc[1]),
  nonaligned = sum(s_own > terc[1] & s_own < terc[2]))
rm(v_cont, v_terc, s_econ, s_own); invisible(gc())

# ---------------------------------------------------------------------------------------------
# The alignment component
# ---------------------------------------------------------------------------------------------
core_bloc <- unique(geo[reporter_iso %in% core, .(reporter_iso, bloc_reporter)])
stopifnot(all(core_bloc$bloc_reporter == "U.S.-leaning"))
part_bloc <- unique(geo[, .(partner_iso, bloc = bloc_partner)])
res$alignment <- list(
  weight = w_fs[["geo_alignment"]], weight_share_when_all_present = r3(w_fs[["geo_alignment"]] / sum(w_fs)),
  nominal_components_positive = names(w_fs)[w_fs > 0],
  mean_contribution_all_cells = r3(mean(fsw$c_geo_alignment)),
  mean_share_of_composite_all_cells = r3(mean(fsw$c_geo_alignment / fsw$fsi_raw)),
  mean_contribution_core_reporters = r3(fsw[reporter_iso %in% core, mean(c_geo_alignment)]),
  core_reporters_bloc = "U.S.-leaning (all 31)",
  value_distribution_core_reporters = as.list(fsw[reporter_iso %in% core, .N, by = geo_alignment][order(geo_alignment), setNames(N, geo_alignment)]),
  partners_by_bloc = as.list(part_bloc[partner_iso %in% psi$partner_iso, .N, by = bloc][, setNames(N, bloc)]),
  coalition_partner_tier_by_bloc = as.list(part_bloc[partner_iso %in% partners_tier, .N, by = bloc][, setNames(N, bloc)]),
  mean_contribution_core_reporters_by_partner_bloc = as.list(
    merge(fsw[reporter_iso %in% core, .(partner_iso, c_geo_alignment)], part_bloc, by = "partner_iso")[,
      .(m = r3(mean(c_geo_alignment))), by = bloc][, setNames(m, bloc)])
)

# ---------------------------------------------------------------------------------------------
# Figure 1: Friendshore matrix among the core members (with China as a supplier column)
# ---------------------------------------------------------------------------------------------
cells_heatmap <- list(c("Batteries", "Midstream"), c("Solar", "Midstream"))
# Every dyad shown has an observed import component (checked below), so no imputation marker is
# needed; the scale starts at 0.4 because every value shown lies above it.
heat_lims <- c(0.4, 1)
heat_panel <- function(t, s) {
  d <- fsw[tech == t & supply_chain == s & reporter_iso %in% core & partner_iso %in% c(core, "CHN"),
           .(i = reporter_iso, j = partner_iso, fs, obs_imp)]
  stopifnot(all(d$obs_imp), min(d$fs) >= heat_lims[1])
  ord <- d[j %in% core, .(m = mean(fs)), by = j][order(-m), j]
  xlev <- c(ord, " ", "CHN")
  d[, `:=`(x = factor(j, levels = xlev), y = factor(i, levels = rev(ord)))]
  ggplot(d, aes(x = x, y = y)) +
    geom_tile(aes(fill = fs), colour = "white", linewidth = 0.3) +
    scale_x_discrete(drop = FALSE, position = "top") +
    scale_fill_gradientn(colours = paper_seq_blue, limits = heat_lims, breaks = seq(0.4, 1, 0.1),
                         name = expression(FS[ijts]),
                         guide = guide_colourbar(title.position = "left", title.vjust = 0.9)) +
    coord_equal() +
    labs(x = paste0(t, " ", s, ": partner j"), y = "Economy i") +
    theme_paper(base_size = 8, grid = "none") +
    theme(axis.text.x.top = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 6.2),
          axis.text.y = element_text(size = 6.2),
          axis.line.x = element_blank(),
          axis.title.x.top = element_text(margin = margin(b = 3)),
          legend.position = "bottom",
          legend.key.width = unit(1.6, "cm"), legend.key.height = unit(0.22, "cm"),
          legend.title = element_text(margin = margin(r = 8)),
          plot.margin = margin(2, 4, 2, 2))
}
hm_plots <- lapply(cells_heatmap, function(cl) heat_panel(cl[1], cl[2]))
legend_hm <- cowplot::get_plot_component(hm_plots[[1]], "guide-box-bottom", return_all = TRUE)
if (is.list(legend_hm) && !inherits(legend_hm, "grob")) legend_hm <- legend_hm[[1]]
hm_row <- cowplot::plot_grid(plotlist = lapply(hm_plots, function(p) p + theme(legend.position = "none")),
                             nrow = 1, labels = c("(a)", "(b)"), label_size = 8.5,
                             label_fontface = "bold", align = "hv", hjust = 0, label_x = 0.005)
fig_hm <- cowplot::plot_grid(hm_row, legend_hm, ncol = 1, rel_heights = c(1, 0.08))
save_paper_fig(fig_hm, "bilateral_fs_heatmap", width = paper_width_full, height = 3.75)

heat_stats <- lapply(cells_heatmap, function(cl) {
  d <- fsw[tech == cl[1] & supply_chain == cl[2] & reporter_iso %in% core & partner_iso %in% core]
  sup <- d[, .(mean_fs = mean(fs)), by = partner_iso][order(-mean_fs)]
  imp <- d[, .(mean_fs = mean(fs)), by = reporter_iso][order(-mean_fs)]
  a <- merge(d[, .(i = reporter_iso, j = partner_iso, fs_ij = fs)], d[, .(i = partner_iso, j = reporter_iso, fs_ji = fs)],
             by = c("i", "j"))[i < j]
  chn <- fsw[tech == cl[1] & supply_chain == cl[2] & reporter_iso %in% core & partner_iso == "CHN"]
  z <- fsw[tech == cl[1] & supply_chain == cl[2] & reporter_iso %in% core & partner_iso %in% c(core, "CHN"),
           .(m = mean(fs)), by = partner_iso][, rk := frank(-m, ties.method = "min")]
  list(
    cell = paste(cl, collapse = " "), n_dyads = nrow(d),
    share_observed_trade = r4(mean(d$obs_imp)), n_imputed_trade = sum(!d$obs_imp),
    supplier_mean_fs = setNames(as.list(r3(sup$mean_fs)), sup$partner_iso),
    importer_mean_fs = setNames(as.list(r3(imp$mean_fs)), imp$reporter_iso),
    sd_supplier_means = r3(sd(sup$mean_fs)), sd_importer_means = r3(sd(imp$mean_fs)),
    spearman_fs_ij_fs_ji = r3(spearman(a$fs_ij, a$fs_ji)), n_unordered_pairs = nrow(a),
    mean_abs_fs_ij_minus_fs_ji = r3(mean(abs(a$fs_ij - a$fs_ji))),
    china_mean_fs_from_core = r3(mean(chn$fs)), china_min_fs_from_core = r3(min(chn$fs)),
    china_max_fs_from_core = r3(max(chn$fs)),
    china_rank_among_core_plus_china = z[partner_iso == "CHN", rk],
    china_mean_components = list(trade = r3(mean(chn$imp_trade_index)), capability = r3(mean(chn$eo_partner)),
                                 alignment = r3(mean(chn$geo_alignment)), need = r3(mean(chn$es_need, na.rm = TRUE)),
                                 chinese_parent_scored = r3(mean(chn$chinese_parent_exposure, na.rm = TRUE))),
    core_supplier_mean_capability = r3(d[, mean(eo_partner)]),
    china_mean_fs_no_alignment = r3(mean(fs_noalign[fsw[, .I[tech == cl[1] & supply_chain == cl[2] &
                                                                reporter_iso %in% core & partner_iso == "CHN"]]]))
  )
})
names(heat_stats) <- vapply(cells_heatmap, function(cl) paste(cl, collapse = "_"), "")
asym_all <- merge(fsw[reporter_iso %in% core & partner_iso %in% core, .(i = reporter_iso, j = partner_iso, tech, supply_chain, fs_ij = fs)],
                  fsw[reporter_iso %in% core & partner_iso %in% core, .(i = partner_iso, j = reporter_iso, tech, supply_chain, fs_ji = fs)],
                  by = c("i", "j", "tech", "supply_chain"))[i < j]
res$heatmap <- c(heat_stats, list(asymmetry_core_pooled = list(
  n = nrow(asym_all), spearman_fs_ij_fs_ji = r3(spearman(asym_all$fs_ij, asym_all$fs_ji)),
  mean_within_cell_spearman = r3(mean(asym_all[, .(r = spearman(fs_ij, fs_ji)), by = .(tech, supply_chain)]$r)))))
rm(asym_all)

# ---------------------------------------------------------------------------------------------
# Figure 2: Friendshore component contributions, top-10 suppliers of the USA and Japan
# ---------------------------------------------------------------------------------------------
comp_keys <- names(fs_comp_lab)
# Need and capability wear the pillar hues of the theme (security / need = Energy Security,
# capability = Economic Opportunity), as in the Strategic Index decomposition; the other
# components take non-pillar slots.
comp_cols <- c(unname(paper_cat["violet"]), unname(paper_pillar_cols["Energy Security"]),
               unname(paper_pillar_cols["Economic Opportunity"]), unname(paper_cat[c("yellow", "magenta", "green")]),
               paper_ink$muted)
names(comp_cols) <- c(unname(fs_comp_lab), "Climate penalty")
fs_rankable_partners <- function(r) {
  z <- merge(fs_top[reporter_iso == r], psi[, .(partner_iso, sovereign)], by = "partner_iso")
  z[sovereign == TRUE & fs_share <= tau + 1e-9][order(-tfs)][, rank := seq_len(.N)]
}
comp_data <- list(); comp_stats <- list()
for (r in c("USA", "JPN")) {
  tf <- fs_rankable_partners(r)
  top <- tf[1:10]
  d5 <- fsw[reporter_iso == r & partner_iso %in% top$partner_iso][order(partner_iso, -fs, tech, supply_chain)][,
           head(.SD, 5), by = partner_iso]
  chk <- d5[, .(m = mean(fs)), by = partner_iso][top, on = "partner_iso"]
  stopifnot(max(abs(chk$m - chk$tfs)) < 1e-9)
  agg <- d5[, c(lapply(.SD, mean), list(pen = -mean(pen), fsi_raw = mean(fsi_raw), n_obs_trade = sum(obs_imp),
                                        n_inv = sum(!is.na(outbound_index)), n_nodata = sum(status == "no_data"), x_geo = mean(geo_alignment), n_cpe = sum(!is.na(cpe_share)),
                                        x_trade = mean(imp_trade_index), x_cap = mean(eo_partner),
                                        cells = paste(paste(tech, supply_chain), collapse = "; "))),
            by = partner_iso, .SDcols = paste0("c_", comp_keys)]
  agg <- merge(agg, top[, .(partner_iso, tfs, rank, fs_share)], by = "partner_iso")[order(rank)]
  agg[, reporter := r]
  comp_data[[r]] <- agg
  # Ranking without the alignment component (same rule, same partners).
  tf_na_r <- merge(tf_na[reporter_iso == r], tf[, .(partner_iso)], by = "partner_iso")[order(-t)][, rank_na := seq_len(.N)]
  share <- function(k) r3(mean(agg[[paste0("c_", k)]] / agg$fsi_raw))
  comp_stats[[r]] <- list(
    ranking_basis = paste0("T_FS among partners that are sovereign and whose Friendshore dyad imputed weight share <= ", tau),
    n_partners_ranked = nrow(tf), n_partners_scored = fs_top[reporter_iso == r, .N],
    top10 = lapply(seq_len(nrow(agg)), function(k) with(agg[k], list(
      iso3 = partner_iso, name = nm(partner_iso), rank = rank, T_FS = r3(tfs), fs_dyad_imputed_share = r3(fs_share),
      composite_pre_penalty = r3(fsi_raw), contrib_trade = r3(c_imp_trade_index), contrib_need = r3(c_es_need),
      contrib_capability = r3(c_eo_partner), contrib_investment = r3(c_outbound_index),
      contrib_alignment = r3(c_geo_alignment), alignment_value = r3(x_geo),
      contrib_chinese_parent = r3(c_chinese_parent_exposure), best5_cells_with_chinese_parent = n_cpe, penalty = r3(-pen),
      best5_cells_observed_trade = n_obs_trade, best5_cells_without_trade_codes = n_nodata,
      best5_cells_with_investment = n_inv, best5_cells = cells))),
    top10_mean_share_of_composite = list(trade = share("imp_trade_index"), need = share("es_need"),
                                         capability = share("eo_partner"), investment = share("outbound_index"),
                                         alignment = share("geo_alignment"), chinese_parent = share("chinese_parent_exposure")),
    top10_bloc = as.list(part_bloc[partner_iso %in% agg$partner_iso, .N, by = bloc][, setNames(N, bloc)]),
    top10_mean_penalty = r4(mean(-agg$pen)), top10_mean_composite_pre_penalty = r3(mean(agg$fsi_raw)),
    top10_cells_observed_trade_of_50 = sum(agg$n_obs_trade), top10_cells_without_trade_codes_of_50 = sum(agg$n_nodata),
    top10_cells_with_investment_of_50 = sum(agg$n_inv),
    china_rank = tf[partner_iso == "CHN", rank], china_T_FS = r3(tf[partner_iso == "CHN", tfs]),
    china_rank_without_alignment = tf_na_r[partner_iso == "CHN", rank_na],
    top10_without_alignment = tf_na_r[1:10, partner_iso],
    unranked_partners_above_10th = {
      z <- fs_top[reporter_iso == r & tfs > top[10, tfs] & !partner_iso %in% tf$partner_iso]
      z <- merge(z, psi[, .(partner_iso, sovereign)], by = "partner_iso")
      lapply(split(z[order(-tfs)], by = "partner_iso"), function(q) list(T_FS = r3(q$tfs), share = r3(q$fs_share),
                                                                           sovereign = q$sovereign))
    },
    reporter_files_dip = r %in% dip_reporters
  )
}
res$fs_components <- comp_stats

cd <- rbindlist(comp_data)
cd[, reporter_lab := factor(nm(reporter), levels = nm(c("USA", "JPN")))]
cd[, ylab := sprintf("%s (%.3f)", nm(partner_iso), tfs)]
cd[, ykey := paste(reporter, partner_iso)]
cd_long <- melt(cd, id.vars = c("reporter_lab", "ykey", "ylab", "rank"),
                measure.vars = c(paste0("c_", comp_keys), "pen"), variable.name = "component", value.name = "contribution")
cd_long[, component := factor(c(setNames(unname(fs_comp_lab), paste0("c_", comp_keys)), pen = "Climate penalty")[as.character(component)],
                              levels = names(comp_cols))]
ylabs <- setNames(cd$ylab, cd$ykey)
cd_long[, ykey := factor(ykey, levels = cd[order(reporter_lab, -rank), ykey])]
p_comp <- ggplot(cd_long, aes(x = contribution, y = ykey, fill = component)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.25, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  facet_wrap(~reporter_lab, scales = "free_y", nrow = 1) +
  scale_y_discrete(labels = ylabs) +
  scale_x_continuous(limits = c(-0.1, 0.9), breaks = seq(0, 0.8, 0.2), expand = c(0, 0)) +
  scale_fill_manual(values = comp_cols, name = NULL, drop = FALSE) +
  labs(x = "Mean contribution to the pre-penalty composite over the dyad's five best cells", y = NULL) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
  theme_paper(base_size = 8.5, grid = "x") +
  theme(panel.spacing.x = unit(0.9, "cm"), axis.text.y = element_text(size = 7.5),
        legend.text = element_text(size = 7.5), plot.margin = margin(4, 10, 2, 2))
save_paper_fig(p_comp, "bilateral_fs_components", width = paper_width_full, height = 3.8)

# ---------------------------------------------------------------------------------------------
# CM-FDI: Chinese-parent ownership exposure of suppliers and the investment ties it supplies
# ---------------------------------------------------------------------------------------------
# The CM-FDI database (Net Zero Industrial Policy Lab, Johns Hopkins; CC BY 4.0) lists cross-border
# manufacturing projects in solar, wind, batteries and electric vehicles. scripts/15 uses it twice
# in the (Solar, Wind, Batteries, Electric Vehicles) x Midstream cells: as the investor -> destination
# tie of the outward-investment component (in place of the IMF position) and as the new component
# chinese_parent_exposure of supplier j (share of j's foreign-invested manufacturing in t owned by
# parents headquartered in China, Hong Kong or Macao; scored 1 - share; missing below min_projects).
cm_exp <- read_proc("partner_cm_fdi_exposure_tbl")
cm_cov <- read_proc("partner_cm_fdi_coverage_tbl")
cm_ties <- read_proc("partner_cm_fdi_ties_tbl")
cm_proj <- read_proc("cm_fdi_projects_tbl")[, .(project_id, tech, index_use, china_parent, destination_iso3, home_iso3)]
cm_cfg <- pcfg$cm_fdi
china_hq <- c("CHN", "HKG", "MAC")
# The stored component equals the exposure table wherever the component is defined.
cpe_fs <- unique(fsw[!is.na(cpe_share), .(partner_iso, tech, supply_chain, cpe_share)])
stopifnot(!anyDuplicated(cpe_fs[, .(partner_iso, tech, supply_chain)]))
cpe_chk <- merge(cpe_fs, cm_exp[, .(partner_iso = destination_iso, tech, supply_chain, x = chinese_parent_exposure,
                                    n_projects, exposure_basis)], by = c("partner_iso", "tech", "supply_chain"))
stopifnot(nrow(cpe_chk) == nrow(cpe_fs), max(abs(cpe_chk$cpe_share - cpe_chk$x)) < 1e-9)
cpe_fs <- merge(cpe_fs, cm_exp[, .(partner_iso = destination_iso, tech, supply_chain, n_projects, n_china, value_usd_m,
                                   share_count, share_value, exposure_basis)],
                by = c("partner_iso", "tech", "supply_chain"), all.x = TRUE)
cpe_fs[, tier := fifelse(partner_iso %in% core, "core", fifelse(partner_iso %in% partners_tier, "partner", "none"))]
cpe_ex <- cpe_fs[!partner_iso %in% china_hq]

# Counterfactual: the component left out (weight 0), every later step re-run.
w_fs_nocpe <- w_fs; w_fs_nocpe[["chinese_parent_exposure"]] <- 0
fs_nocpe <- fs_index(fsw, w = w_fs_nocpe)
tf_nc <- core_t5(fsw, fs_nocpe)
v_nc <- psi_view(tf_nc, base_tp, core)
res$counterfactual_no_chinese_parent_exposure <- cf_compare(v_nc, "chinese_parent_exposure weight set to 0")
fsw[, fs_nocpe := fs_nocpe]
cm_cells <- fsw[cmfdi_cell == TRUE]
res$counterfactual_no_chinese_parent_exposure$cell_level_spearman_mean_within_cmfdi_cell <-
  r3(mean(cm_cells[, .(r = spearman(fs, fs_nocpe)), by = .(tech, supply_chain)]$r))

# Effect on the core members' ratings of each supplier in the covered cells.
core_cm <- cm_cells[reporter_iso %in% core & !is.na(cpe_share),
                    .(fs = mean(fs), fs_nocpe = mean(fs_nocpe), share = cpe_share[1], n = .N),
                    by = .(partner_iso, tech, supply_chain)]
core_cm[, d := fs - fs_nocpe]
res$cm_fdi <- list(
  source = "CM-FDI, Net Zero Industrial Policy Lab, Johns Hopkins University (CC BY 4.0); data/processed/cm_fdi_projects_tbl.rds",
  settings = list(investment_ties = cm_cfg$investment_ties, chinese_parent_exposure = cm_cfg$chinese_parent_exposure,
                  min_projects = if (is.null(cm_cfg$min_projects)) 2L else cm_cfg$min_projects,
                  jv_parent_share = cm_cfg$jv_parent_share, stage = cm_cfg$stage,
                  weight = w_fs[["chinese_parent_exposure"]],
                  weight_share_when_all_present = r3(w_fs[["chinese_parent_exposure"]] / sum(w_fs))),
  projects_in_file = nrow(cm_proj), projects_used = cm_proj[index_use == TRUE, .N],
  projects_used_china_parent = cm_proj[index_use == TRUE & china_parent == TRUE, .N],
  projects_used_by_tech = as.list(cm_proj[index_use == TRUE, .N, by = tech][, setNames(N, tech)]),
  coverage_by_cell = lapply(split(cm_cov, by = c("tech")), function(z) list(
    dyad_cells = z$n_cells, cells_with_tie = z$n_tie, reporters_with_tie = z$n_tie_reporters,
    cells_with_exposure = z$n_exposure, partners_with_exposure = z$n_exposure_partners)),
  partners_with_exposure_any_cell = uniqueN(cpe_fs$partner_iso),
  partners_with_exposure_excl_china_hq = uniqueN(cpe_ex$partner_iso),
  coalition_members_with_exposure = sort(unique(cpe_ex[tier != "none", partner_iso])),
  n_coalition_members_with_exposure = uniqueN(cpe_ex[tier != "none", partner_iso]),
  nonmembers_with_exposure = sort(unique(cpe_ex[tier == "none", partner_iso])),
  share_distribution_excl_china_hq = qsum(cpe_ex$cpe_share),
  share_by_tech_median_excl_china_hq = as.list(cpe_ex[, .(m = r3(median(cpe_share))), by = tech][, setNames(m, tech)]),
  cells_with_share_ge_0_5 = cpe_ex[cpe_share >= 0.5, .N], cells_excl_china_hq = nrow(cpe_ex),
  cells_share_zero = cpe_ex[cpe_share == 0, .N],
  highest_exposures = lapply(seq_len(min(10, nrow(cpe_ex))), function(k) with(cpe_ex[order(-cpe_share, -n_projects)][k],
    list(iso3 = partner_iso, name = nm(partner_iso), tech = tech, share = r3(cpe_share), n_projects = n_projects,
         n_china = n_china, basis = exposure_basis, tier = tier))),
  coalition_exposure = lapply(split(cpe_ex[tier != "none"][order(partner_iso, tech)], by = "partner_iso"), function(z)
    setNames(as.list(r3(z$cpe_share)), z$tech)),
  china_hq_share = r3(unique(cpe_fs[partner_iso %in% china_hq, cpe_share])),
  mean_contribution_in_covered_cells = r3(cm_cells[!is.na(cpe_share), mean(c_chinese_parent_exposure)]),
  mean_share_of_composite_in_covered_cells = r3(cm_cells[!is.na(cpe_share), mean(c_chinese_parent_exposure / fsi_raw)]),
  share_covered_cells_with_component = r3(cm_cells[, mean(!is.na(cpe_share))]),
  core_view_cell_effect = list(
    n_supplier_cells = nrow(core_cm),
    spearman_share_vs_fs_change = r3(spearman(core_cm$share, core_cm$d)),
    mean_fs_change_share_ge_0_5 = r3(core_cm[share >= 0.5 & !partner_iso %in% china_hq, mean(d)]),
    mean_fs_change_share_lt_0_2 = r3(core_cm[share < 0.2 & !partner_iso %in% china_hq, mean(d)]),
    china_fs_change = as.list(core_cm[partner_iso == "CHN", setNames(r3(d), tech)]),
    largest_losses = lapply(seq_len(6), function(k) with(core_cm[order(d)][k],
      list(iso3 = partner_iso, name = nm(partner_iso), tech = tech, share = r3(share), fs = r3(fs), fs_without = r3(fs_nocpe))))
  ),
  investment_ties = list(
    pairs_with_tie = uniqueN(cm_ties[, .(investor_iso, destination_iso)]),
    investors = uniqueN(cm_ties$investor_iso), destinations = uniqueN(cm_ties$destination_iso),
    core_investors = sort(intersect(core, unique(cm_ties$investor_iso))),
    ties_by_tech = as.list(cm_ties[, .N, by = tech][, setNames(N, tech)])
  ),
  aib_link = "operationalises the common-mode / parent-company dependence upgrade of the AIB concept paper"
)

# Figure: Chinese-parent exposure by technology (panel a) and the core members' mean Friendshore
# rating of the supplier in the cell with and without the component (panel b).
tech_ord <- c("Batteries", "Electric Vehicles", "Solar", "Wind")
fig_rows <- cpe_ex[, .(mean_share = mean(cpe_share), n_cells = .N), by = .(partner_iso, tier)]
tier_lab <- c(core = "Core", partner = "Partner", none = "Non-member")
fig_rows[, tier_f := factor(tier_lab[tier], levels = tier_lab)]
fig_rows[, ylab := nm(partner_iso)]
fig_rows <- fig_rows[order(tier_f, -mean_share, ylab)]
fig_rows[, ykey := factor(partner_iso, levels = rev(partner_iso))]
tiles <- CJ(partner_iso = fig_rows$partner_iso, tech = tech_ord)
tiles <- merge(tiles, cpe_ex[, .(partner_iso, tech, cpe_share, n_projects)], by = c("partner_iso", "tech"), all.x = TRUE)
tiles <- merge(tiles, fig_rows[, .(partner_iso, ykey, tier_f)], by = "partner_iso")
tiles[, tech_f := factor(tech, levels = tech_ord)]
tiles[, lab := fifelse(is.na(cpe_share), "", sprintf("%.0f%%", 100 * cpe_share))]
tiles[, lab_col := fifelse(!is.na(cpe_share) & cpe_share >= 0.55, "white", paper_ink$primary)]
p_cpe_a <- ggplot(tiles, aes(x = tech_f, y = ykey)) +
  geom_tile(aes(fill = cpe_share), colour = "white", linewidth = 0.4) +
  geom_text(aes(label = lab, colour = lab_col), size = 2.2) +
  scale_colour_identity() +
  scale_fill_gradientn(colours = paper_seq_orange, limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       labels = scales::percent, na.value = paper_ink$neutral,
                       name = "Chinese-parent share of foreign-invested\nmanufacturing projects (Midstream)") +
  scale_x_discrete(position = "top", labels = c(Batteries = "Batteries", `Electric Vehicles` = "EVs",
                                                Solar = "Solar", Wind = "Wind")) +
  scale_y_discrete(labels = setNames(fig_rows$ylab, fig_rows$partner_iso)) +
  ggforce::facet_col(vars(tier_f), scales = "free_y", space = "free") +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_colourbar(title.position = "top", barwidth = unit(4.2, "cm"), barheight = unit(0.22, "cm"))) +
  theme_paper(base_size = 8, grid = "none") +
  theme(axis.text.y = element_text(size = 6.8), axis.text.x.top = element_text(size = 7.2),
        axis.line.x = element_blank(), strip.text = element_text(size = 7.5, margin = margin(b = 1, t = 3)),
        legend.position = "bottom", legend.title = element_text(size = 7), plot.margin = margin(2, 4, 2, 2))
eff <- core_cm[!partner_iso %in% china_hq, .(fs = mean(fs), fs_nocpe = mean(fs_nocpe)), by = partner_iso]
eff <- merge(eff, fig_rows[, .(partner_iso, ykey, tier_f)], by = "partner_iso")
eff_long <- melt(eff, id.vars = c("partner_iso", "ykey", "tier_f"), measure.vars = c("fs", "fs_nocpe"),
                 variable.name = "variant", value.name = "value")
eff_long[, variant := factor(variant, levels = c("fs", "fs_nocpe"),
                             labels = c("With the component (headline)", "Component left out"))]
p_cpe_b <- ggplot() +
  geom_segment(data = eff, aes(x = fs_nocpe, xend = fs, y = ykey, yend = ykey), colour = paper_ink$axis, linewidth = 0.5) +
  geom_point(data = eff_long, aes(x = value, y = ykey, shape = variant), size = 1.5, colour = paper_cat[["blue"]],
             fill = paper_cat[["blue"]], stroke = 0.6) +
  scale_shape_manual(values = c(19, 1), name = NULL) +
  scale_y_discrete(labels = NULL) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  ggforce::facet_col(vars(tier_f), scales = "free_y", space = "free") +
  labs(x = "Mean Friendshore rating by the 31 core members\n(covered cells, mean over technologies)", y = NULL) +
  guides(shape = guide_legend(nrow = 2)) +
  theme_paper(base_size = 8, grid = "x") +
  theme(strip.text = element_text(colour = "white", size = 7.5, margin = margin(b = 1, t = 3)),
        legend.position = "bottom", plot.margin = margin(14, 6, 2, 2))
stopifnot(setequal(eff$partner_iso, fig_rows$partner_iso))
get_leg <- function(p) {
  g <- cowplot::get_plot_component(p, "guide-box-bottom", return_all = TRUE)
  if (is.list(g) && !inherits(g, "grob")) g[[1]] else g
}
leg_a <- get_leg(p_cpe_a); leg_b <- get_leg(p_cpe_b)
fig_cpe <- cowplot::plot_grid(
  cowplot::plot_grid(p_cpe_a + theme(legend.position = "none"), p_cpe_b + theme(legend.position = "none"),
                     nrow = 1, rel_widths = c(1.15, 1), align = "h", axis = "tb",
                     labels = c("(a)", "(b)"), label_size = 8.5, label_fontface = "bold", hjust = 0, label_x = 0.005),
  cowplot::plot_grid(leg_a, leg_b, nrow = 1, rel_widths = c(1.15, 1)),
  ncol = 1, rel_heights = c(1, 0.09))
save_paper_fig(fig_cpe, "bilateral_cmfdi_exposure", width = paper_width_full,
               height = min(8.6, 1.6 + 0.135 * nrow(fig_rows)))
res$cm_fdi$figure_rows <- fig_rows$partner_iso
res$cm_fdi$panel_b_core_mean_fs <- setNames(lapply(seq_len(nrow(eff)), function(k)
  list(with_component = r3(eff$fs[k]), without_component = r3(eff$fs_nocpe[k]))), eff$partner_iso)
rm(cm_cells, core_cm); invisible(gc())

# ---------------------------------------------------------------------------------------------
# PSI views: core members (headline), full coalition, world
# ---------------------------------------------------------------------------------------------
view_tbl <- function(name, view) {
  read_proc(name)[, .(partner_iso, value, rank, rankable, n_rep = n_reporters_friendshore, view = view)]
}
views <- rbind(view_tbl("psi_tbl", "core"), view_tbl("psi_full_coalition_tbl", "full_coalition"),
               view_tbl("psi_world_tbl", "world"))
view_n_rep <- views[, .(n = max(n_rep)), by = view][, setNames(n, view)]
vw <- dcast(views, partner_iso ~ view, value.var = c("value", "rank", "rankable"))
view_pairs <- list(c("core", "full_coalition"), c("core", "world"), c("full_coalition", "world"))
top15_of <- function(v) views[view == v & rankable == TRUE][order(rank)][1:15, partner_iso]
res$psi_views <- list(
  reporters_averaged = as.list(view_n_rep),
  n_ranked = as.list(views[, .(n = sum(rankable)), by = view][, setNames(n, view)]),
  world_view_unranked_sovereign = views[view == "world" & rankable == FALSE & partner_iso %in% psi[sovereign == TRUE, partner_iso], sort(partner_iso)],
  spearman = lapply(setNames(view_pairs, vapply(view_pairs, paste, "", collapse = "_vs_")), function(p) {
    z <- vw[get(paste0("rankable_", p[1])) == TRUE & get(paste0("rankable_", p[2])) == TRUE]
    list(rho = r3(spearman(z[[paste0("value_", p[1])]], z[[paste0("value_", p[2])]])), n = nrow(z),
         median_abs_rank_change = median(abs(frank(-z[[paste0("value_", p[1])]]) - frank(-z[[paste0("value_", p[2])]]))))
  }),
  top10 = lapply(setNames(c("core", "full_coalition", "world"), c("core", "full_coalition", "world")), function(v)
    lapply(seq_len(10), function(k) with(views[view == v & rankable == TRUE][order(rank)][k],
                                         list(rank = rank, iso3 = partner_iso, name = nm(partner_iso), psi = r3(value))))),
  top15 = lapply(setNames(c("core", "full_coalition", "world"), c("core", "full_coalition", "world")), top15_of),
  top15_overlap = list(core_full = length(intersect(top15_of("core"), top15_of("full_coalition"))),
                       core_world = length(intersect(top15_of("core"), top15_of("world"))),
                       full_world = length(intersect(top15_of("full_coalition"), top15_of("world")))),
  top15_core_members = lapply(setNames(c("core", "full_coalition", "world"), c("core", "full_coalition", "world")),
                              function(v) sum(top15_of(v) %in% core)),
  top15_world_not_in_core = setdiff(top15_of("world"), top15_of("core")),
  top15_core_not_in_world = setdiff(top15_of("core"), top15_of("world")),
  top15_full_not_in_core = setdiff(top15_of("full_coalition"), top15_of("core")),
  key_ranks = lapply(setNames(c("CHN", "TWN", "CHE", "IND", "IDN", "VNM", "USA", "JPN"), c("CHN", "TWN", "CHE", "IND", "IDN", "VNM", "USA", "JPN")),
                     function(k) as.list(views[partner_iso == k, setNames(rank, view)]))
)

# Table: top 15 by view.
tab_rows <- vapply(seq_len(15), function(k) {
  cellv <- vapply(c("core", "full_coalition", "world"), function(v) {
    z <- views[view == v & rankable == TRUE][order(rank)][k]
    sprintf("%s & %.3f", gsub("&", "\\\\&", nm(z$partner_iso)), z$value)
  }, "")
  paste0(k, " & ", paste(cellv, collapse = " & "), "\\\\")
}, "")
sp <- res$psi_views$spearman
tab_tex <- c(
  "% Generated by scripts/paper/fig_bilateral.R; numbers in paper/results/bilateral.json (psi_views).",
  "\\begin{table}[t]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{The fifteen highest-ranked partners in the three views of the Partnership Strength Index}",
  "\\label{tab:bilateral_psi_views}",
  "\\footnotesize",
  "\\begin{tabular}{@{}r l S[table-format=1.3] l S[table-format=1.3] l S[table-format=1.3]@{}}",
  "\\toprule",
  sprintf(" & \\multicolumn{2}{c}{Core members (%d reporters)} & \\multicolumn{2}{c}{Full coalition (%d)} & \\multicolumn{2}{c}{World (%d)}\\\\",
          view_n_rep[["core"]], view_n_rep[["full_coalition"]], view_n_rep[["world"]]),
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(l){6-7}",
  "Rank & Partner & {$\\PSI_j$} & Partner & {$\\PSI_j$} & Partner & {$\\PSI_j$}\\\\",
  "\\midrule",
  tab_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item \\textit{Notes:} $\\PSI_j$ (equation~\\eqref{eq:psi}) with $C^{\\FS}_j$ and $C^{\\PO}_j$ averaged over ",
         "the reporters of each view: the ", view_n_rep[["core"]], " core coalition members (the headline), the ",
         view_n_rep[["full_coalition"]], " core and partner-tier members, and every other reporter (",
         view_n_rep[["world"]], " per partner). Ranks among the rankable partners of each view (sovereign, imputed weight ",
         "share at most ", tau, "): ", res$psi_views$n_ranked$core, ", ", res$psi_views$n_ranked$full_coalition, " and ",
         res$psi_views$n_ranked$world, " partners. Spearman correlations of $\\PSI_j$ across the partners ranked in both views: ",
         "core and full coalition ", sprintf("%.3f", sp$core_vs_full_coalition$rho), " (", sp$core_vs_full_coalition$n, "), ",
         "core and world ", sprintf("%.3f", sp$core_vs_world$rho), " (", sp$core_vs_world$n, "), ",
         "full coalition and world ", sprintf("%.3f", sp$full_coalition_vs_world$rho), " (", sp$full_coalition_vs_world$n, "). ",
         "Source: \\texttt{psi\\_tbl.rds}, \\texttt{psi\\_full\\_coalition\\_tbl.rds}, \\texttt{psi\\_world\\_tbl.rds}; ",
         "\\texttt{scripts/paper/fig\\_bilateral.R}."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(tab_tex, file.path(tab_dir, "bilateral_psi_views.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------------------------
# Headline PSI: ranks, categories, China
# ---------------------------------------------------------------------------------------------
rk[, `:=`(rk_fs = frank(-friendshore, ties.method = "min"), rk_po = frank(-opportunity, ties.method = "min"))]
rk[!is.na(development), rk_dp := frank(-development, ties.method = "min")]
cor_cats <- function(a, b) {
  z <- rk[!is.na(get(a)) & !is.na(get(b))]
  list(spearman = r3(spearman(z[[a]], z[[b]])), n = nrow(z))
}
var_share <- function(z, cats) {
  a <- w_psi[cats] / sum(w_psi[cats])
  p <- as.vector(as.matrix(z[, ..cats]) %*% a)
  stopifnot(max(abs(p - z$psi)) < 1e-9)
  s <- vapply(cats, function(k) a[[k]] * stats::cov(z[[k]], p), 0) / stats::var(p)
  c(as.list(r3(s)), list(n = nrow(z), nominal = as.list(r3(a))))
}
psi_dyad <- read_proc("psi_dyad_tbl")
top_pairs <- psi_dyad[reporter_iso %in% coalition & partner_iso %in% coalition & rankable == TRUE][
  order(-Partnership_Strength_Index)][1:10]
res$psi <- list(
  view = "core members (headline)", n_ranked = nrow(rk),
  top10 = lapply(seq_len(10), function(k) with(rk[order(rank)][k], list(
    rank = rank, iso3 = partner_iso, name = nm(partner_iso), psi = r3(psi), C_FS = r3(friendshore),
    C_PO = r3(opportunity), C_DP = r3(development), tier = tier))),
  core_in_top25 = rk[tier == "core" & rank <= 25, .N],
  first_noncore = with(rk[tier != "core"][order(rank)][1], list(iso3 = partner_iso, name = nm(partner_iso), rank = rank)),
  member_ranks = setNames(as.list(rk[is_member == TRUE][order(rank), rank]), rk[is_member == TRUE][order(rank), partner_iso]),
  core_median_rank = median(rk[tier == "core", rank]), partner_tier_median_rank = median(rk[tier == "partner", rank]),
  partner_tier_best = with(rk[tier == "partner"][order(rank)][1:3], list(iso3 = partner_iso, rank = rank)),
  partner_tier_worst = with(rk[tier == "partner"][order(-rank)][1:3], list(iso3 = partner_iso, rank = rank)),
  partner_tier_by_bloc_median_rank = as.list(merge(rk[tier == "partner"], part_bloc, by = "partner_iso")[,
    .(m = median(rank), n = .N), by = bloc][, setNames(paste0(m, " (n=", n, ")"), bloc)]),
  nonmember_top10 = lapply(seq_len(10), function(k) with(rk[is_member == FALSE][order(rank)][k], list(
    rank = rank, iso3 = partner_iso, name = nm(partner_iso), psi = r3(psi)))),
  psi_range_ranked = r3(range(rk$psi)), psi_range_members = r3(range(rk[is_member == TRUE, psi])),
  psi_range_core = r3(range(rk[tier == "core", psi])),
  mean_psi_by_income_group = as.list(rk[, .(m = r3(mean(psi)), n = .N), by = income_group][, setNames(paste0(m, " (n=", n, ")"), income_group)]),
  mean_C_FS_C_PO_by_income_group = as.list(rk[, .(m = r3(mean((friendshore + opportunity) / 2))), by = income_group][, setNames(m, income_group)]),
  sd_categories = list(C_FS = r4(sd(rk$friendshore)), C_PO = r4(sd(rk$opportunity)), C_DP = r4(sd(rk$development, na.rm = TRUE)),
                       n_DP = sum(!is.na(rk$development))),
  mean_categories = list(C_FS = r3(mean(rk$friendshore)), C_PO = r3(mean(rk$opportunity)), C_DP = r3(mean(rk$development, na.rm = TRUE))),
  spearman_C_FS_C_PO = cor_cats("friendshore", "opportunity"),
  spearman_C_FS_C_DP = cor_cats("friendshore", "development"),
  spearman_C_PO_C_DP = cor_cats("opportunity", "development"),
  variance_share_dp_eligible = var_share(rk[!is.na(development)], c("friendshore", "opportunity", "development")),
  variance_share_no_dp = var_share(rk[is.na(development)], c("friendshore", "opportunity")),
  top10_coalition_pairs_psi_ij = lapply(seq_len(10), function(k) with(top_pairs[k], list(
    reporter = reporter_iso, partner = partner_iso, psi_ij = r3(Partnership_Strength_Index),
    T_FS = r3(friendshore), T_PO = r3(opportunity)))),
  psi_dyad_rankable = psi_dyad[, sum(rankable)], psi_dyad_n = nrow(psi_dyad)
)
rm(psi_dyad)
# China and other economies as supplier (C_FS) and as market (C_PO), core view.
core_tf <- fs_top[reporter_iso %in% core]
core_tp <- po_top[reporter_iso %in% core]
chn_tx <- pow[reporter_iso %in% core & partner_iso == "CHN" & obs_exp == TRUE]
key_partner <- function(k) {
  z <- rk[partner_iso == k]
  list(name = nm(k), rank_psi = z$rank, psi = r3(z$psi), C_FS = r3(z$friendshore), rank_C_FS = z$rk_fs,
       C_PO = r3(z$opportunity), rank_C_PO = z$rk_po, C_DP = r3(z$development),
       bloc = part_bloc[partner_iso == k, bloc],
       mean_T_FS_core = r3(core_tf[partner_iso == k, mean(tfs)]), mean_T_PO_core = r3(core_tp[partner_iso == k, mean(tpo)]))
}
res$psi$key_partners <- lapply(setNames(c("CHN", "TWN", "CHE", "IND", "IDN", "VNM", "RUS", "USA"),
                                        c("CHN", "TWN", "CHE", "IND", "IDN", "VNM", "RUS", "USA")), key_partner)
res$psi$china_as_market <- list(
  core_exporters_observed_cells = nrow(chn_tx), share_from_mirror = r3(mean(chn_tx$src == "mirror")),
  mean_export_trade_component_core = r3(mean(chn_tx$trade_index)),
  rank_mean_export_trade_component_core = {
    z <- pow[reporter_iso %in% core & obs_exp == TRUE & partner_iso %in% rk$partner_iso, .(m = mean(trade_index)), by = partner_iso]
    z[, frank(-m, ties.method = "min")][z$partner_iso == "CHN"]
  },
  mean_partner_need_eta = r3(pow[partner_iso == "CHN", mean(partner_es_need)])
)
# What C_PO measures in the core view: the market's need eta against the trade term of core exporters.
eta_p <- pow[, .(eta = mean(partner_es_need)), by = partner_iso]
tx_p <- pow[reporter_iso %in% core & obs_exp == TRUE, .(tau_x = mean(trade_index)), by = partner_iso]
zz <- merge(merge(rk[, .(partner_iso, opportunity, friendshore)], eta_p, by = "partner_iso", all.x = TRUE), tx_p, by = "partner_iso", all.x = TRUE)
res$psi$market_need_and_C_PO <- list(
  spearman_C_PO_vs_mean_eta = r3(spearman(zz$opportunity, zz$eta)),
  spearman_C_PO_vs_core_observed_tau_x = r3(spearman(zz$opportunity, zz$tau_x)),
  spearman_C_FS_vs_mean_eta = r3(spearman(zz$friendshore, zz$eta)),
  n = nrow(zz), n_with_tau_x = sum(!is.na(zz$tau_x))
)
rm(zz, eta_p, tx_p)
res$psi$strong_supplier_weak_market <- lapply(seq_len(6), function(k) with(rk[order(rk_fs - rk_po)][k],
  list(iso3 = partner_iso, name = nm(partner_iso), rank_C_FS = rk_fs, rank_C_PO = rk_po)))
res$psi$strong_market_weak_supplier <- lapply(seq_len(6), function(k) with(rk[order(rk_po - rk_fs)][k],
  list(iso3 = partner_iso, name = nm(partner_iso), rank_C_FS = rk_fs, rank_C_PO = rk_po)))

# ---------------------------------------------------------------------------------------------
# Figure 3: headline PSI for members, top and selected non-members, and unranked partners
# ---------------------------------------------------------------------------------------------
sel_non <- rk[is_member == FALSE][order(rank)][1:10, partner_iso]
sel_non <- union(sel_non, intersect(c("CHN", "TWN", "CHE"), rk$partner_iso))
sel_unr <- psi[rankable == FALSE][order(-psi)][1:5, partner_iso]
grp_levels <- c(sprintf("Core members (%d)", length(core)), "Not ranked: five highest scores",
                sprintf("Partner-tier members (%d)", length(partners_tier)),
                "Non-members (selected)")
sel <- rbind(psi[tier == "core"][, grp := grp_levels[1]], psi[partner_iso %in% sel_unr][, grp := grp_levels[2]],
             psi[tier == "partner"][, grp := grp_levels[3]], psi[partner_iso %in% sel_non][, grp := grp_levels[4]])
sel[, col := fifelse(grp %in% grp_levels[1:2], "left", "right")]
sel[, ylab := fifelse(rankable, sprintf("%s (%d)", nm(partner_iso), rank), sprintf("%s (n.r.)", nm(partner_iso)))]
sel[, ykey := paste(grp, partner_iso)]
pts <- melt(sel, id.vars = c("ykey", "grp", "col", "psi"), measure.vars = c("friendshore", "opportunity", "development"),
            variable.name = "pillar", value.name = "score")[!is.na(score)]
rng <- pts[, .(lo = min(score, psi[1]), hi = max(score, psi[1])), by = .(ykey, grp, col)]
x_lim <- c(floor(min(pts$score, sel$psi) * 20) / 20, 1)
pillar_cols <- setNames(unname(paper_cat[c("blue", "orange", "aqua")]), c("friendshore", "opportunity", "development"))
psi_col_plot <- function(side) {
  s <- sel[col == side]
  s[, ykey := factor(ykey, levels = s[order(grp, psi), ykey])]
  s[, grp := factor(grp, levels = intersect(grp_levels, unique(grp)))]
  p_ <- pts[col == side][, ykey := factor(ykey, levels = levels(s$ykey))][, grp := factor(grp, levels = levels(s$grp))]
  r_ <- rng[col == side][, ykey := factor(ykey, levels = levels(s$ykey))][, grp := factor(grp, levels = levels(s$grp))]
  ggplot() +
    geom_segment(data = r_, aes(x = lo, xend = hi, y = ykey, yend = ykey), colour = paper_ink$grid, linewidth = 0.6) +
    geom_point(data = p_, aes(x = score, y = ykey, colour = pillar), size = 1.3) +
    geom_point(data = s, aes(x = psi, y = ykey, shape = ifelse(rankable, "ranked", "unranked")), size = 2,
               colour = paper_ink$primary, fill = paper_ink$primary, stroke = 0.5) +
    ggforce::facet_col(vars(grp), scales = "free_y", space = "free") +
    scale_y_discrete(labels = setNames(s$ylab, s$ykey)) +
    scale_colour_manual(values = pillar_cols, name = NULL, limits = names(pillar_cols),
                        labels = c(friendshore = expression("Friendshore " * C[j]^FS),
                                   opportunity = expression("Opportunity " * C[j]^PO),
                                   development = expression("Development " * C[j]^DP))) +
    scale_shape_manual(values = c(ranked = 18, unranked = 5), name = NULL, limits = c("ranked", "unranked"),
                       labels = c(ranked = expression(PSI[j] * " (ranked)"), unranked = expression(PSI[j] * " (not ranked)"))) +
    scale_x_continuous(limits = x_lim, breaks = seq(0.4, 1, 0.1), expand = expansion(mult = c(0.02, 0.04))) +
    labs(x = "Score (core-member view)", y = NULL) +
    guides(shape = guide_legend(order = 1), colour = guide_legend(order = 2)) +
    theme_paper(base_size = 8, grid = "x") +
    theme(strip.text = element_text(hjust = 0, size = 7.5, margin = margin(b = 2, t = 4)),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_blank(), panel.spacing.y = unit(0.3, "cm"),
          legend.position = "none", plot.margin = margin(4, 12, 2, 2))
}
p_left <- psi_col_plot("left"); p_right <- psi_col_plot("right")
leg_psi <- cowplot::get_plot_component(p_left + theme(legend.position = "bottom"), "guide-box-bottom", return_all = TRUE)
if (is.list(leg_psi) && !inherits(leg_psi, "grob")) leg_psi <- leg_psi[[1]]
n_left <- sel[col == "left", .N] + 2; n_right <- sel[col == "right", .N] + 2
fig_psi <- cowplot::plot_grid(cowplot::plot_grid(p_left, p_right, nrow = 1, align = "h"),
                              leg_psi, ncol = 1, rel_heights = c(1, 0.035))
save_paper_fig(fig_psi, "bilateral_psi_rank", width = paper_width_full, height = 6.9)
res$psi$figure_rows <- list(nonmembers = sel_non, unranked = sel_unr)

# ---------------------------------------------------------------------------------------------
# Figure 4: FS(consumer <- supplier) against PO(supplier -> consumer), coalition dyads
# ---------------------------------------------------------------------------------------------
edge <- merge(fsw[, .(c = reporter_iso, p = partner_iso, tech, supply_chain, fs, obs_imp, e_p = eo_partner,
                      tau_m = imp_trade_index, nu_c = es_need, a_cp = geo_alignment)],
              pow[, .(p = reporter_iso, c = partner_iso, tech, supply_chain, po, obs_exp, eps_p = econ_opp_index,
                      tau_x = trade_index, eta_c = partner_es_need)],
              by = c("c", "p", "tech", "supply_chain"))
edge[, both_obs := obs_imp & obs_exp]
ec <- edge[c %in% coalition & p %in% coalition]
dm <- ec[, .(fs = mean(fs), po = mean(po), n = .N), by = .(c, p)]
by_stage <- ec[, .(rho = r3(spearman(fs, po))), by = supply_chain]
by_cell <- ec[, .(rho = spearman(fs, po), trade = paste(tech[1], supply_chain[1]) %in% trade_data_cells), by = .(tech, supply_chain)]
bm <- ec[tech == "Batteries" & supply_chain == "Midstream"]
partial_given <- function(a, b, z) {
  ok <- !is.na(a) & !is.na(b) & !is.na(z)
  if (sum(ok) < 10 || stats::var(z[ok]) == 0) return(NA_real_)
  ra <- frank(a[ok]); rb <- frank(b[ok]); rz <- frank(z[ok])
  stats::cor(stats::resid(stats::lm(ra ~ rz)), stats::resid(stats::lm(rb ~ rz)))
}
overlap_cell <- ec[, .(rho = spearman(fs, po), rho_partial_e = partial_given(fs, po, e_p),
                       rho_e = spearman(e_p, eps_p)), by = .(tech, supply_chain)]
tl <- merge(fs_top[, .(c = reporter_iso, p = partner_iso, tfs)], po_top[, .(p = reporter_iso, c = partner_iso, tpo)], by = c("c", "p"))
res$fs_vs_po <- list(
  definition = "FS(c<-p): reporter c (importer) rates partner p (supplier); PO(p->c): reporter p (exporter) rates partner c (market); both rate the supply edge p->c",
  n_matched_cells_all = nrow(edge), spearman_cell_level_all = r3(spearman(edge$fs, edge$po)),
  n_matched_cells_coalition = nrow(ec), spearman_cell_level_coalition = r3(spearman(ec$fs, ec$po)),
  spearman_cell_level_core = r3(ec[c %in% core & p %in% core, spearman(fs, po)]),
  n_coalition_ordered_pairs = nrow(dm), spearman_dyad_mean_coalition = r3(spearman(dm$fs, dm$po)),
  matched_cells_per_coalition_pair = as.list(summary(dm$n)),
  spearman_by_stage_coalition = setNames(as.list(by_stage$rho), by_stage$supply_chain),
  mean_within_cell_spearman = r3(mean(by_cell$rho, na.rm = TRUE)),
  mean_within_cell_spearman_trade_cells = r3(by_cell[trade == TRUE, mean(rho, na.rm = TRUE)]),
  mean_within_cell_spearman_no_trade_cells = r3(by_cell[trade == FALSE, mean(rho, na.rm = TRUE)]),
  within_cell_spearman_range = r3(range(by_cell$rho, na.rm = TRUE)),
  shared_inputs = list(
    mean_within_cell_spearman_e_p_vs_eps_p = r3(mean(overlap_cell$rho_e, na.rm = TRUE)),
    spearman_tau_m_vs_tau_x_both_observed_coalition = r3(ec[both_obs == TRUE, spearman(tau_m, tau_x)]),
    n_both_observed_coalition = ec[both_obs == TRUE, .N],
    spearman_nu_c_vs_eta_c_coalition = r3(ec[, spearman(nu_c, eta_c)]),
    mean_within_cell_partial_spearman_given_e_p = r3(mean(overlap_cell$rho_partial_e, na.rm = TRUE)),
    alignment_enters_fs_only = TRUE),
  batteries_midstream = list(n = nrow(bm), spearman = r3(spearman(bm$fs, bm$po)),
                             spearman_both_trade_observed = r3(bm[both_obs == TRUE, spearman(fs, po)]),
                             n_both_trade_observed = bm[both_obs == TRUE, .N],
                             spearman_by_alignment = as.list(bm[, .(r = r3(spearman(fs, po)), n = .N), by = a_cp][order(a_cp), setNames(r, a_cp)]),
                             mean_fs_minus_po_by_alignment = as.list(bm[, .(d = r3(mean(fs - po))), by = a_cp][order(a_cp), setNames(d, a_cp)]),
                             share_abs_diff_gt_0_25 = r3(mean(abs(bm$fs - bm$po) > 0.25))),
  share_coalition_cells_abs_diff_gt_0_25 = r3(mean(abs(ec$fs - ec$po) > 0.25)),
  dyad_mean_fs_minus_po_by_alignment = as.list(merge(dm, unique(ec[, .(c, p, a_cp)]), by = c("c", "p"))[,
    .(d = r3(mean(fs - po)), n = .N), by = a_cp][order(a_cp), setNames(paste0(d, " (n=", n, ")"), a_cp)]),
  most_supplier_view_favoured = lapply(seq_len(5), function(k) with(dm[order(po - fs)][k],
    list(supplier = p, consumer = c, FS = r3(fs), PO = r3(po)))),
  most_market_view_favoured = lapply(seq_len(5), function(k) with(dm[order(fs - po)][k],
    list(supplier = p, consumer = c, FS = r3(fs), PO = r3(po)))),
  best5_spearman_T_FS_cp_vs_T_PO_pc = list(coalition = r3(tl[c %in% coalition & p %in% coalition, spearman(tfs, tpo)]),
                                           n_coalition = tl[c %in% coalition & p %in% coalition, .N],
                                           core = r3(tl[c %in% core & p %in% core, spearman(tfs, tpo)]),
                                           all = r3(spearman(tl$tfs, tl$tpo)), n_all = nrow(tl))
)
rm(tl)

align_lab <- c("0" = "Opposite blocs (0)", "0.5" = "One side nonaligned (0.5)", "1" = "Same bloc (1)")
align_cols <- setNames(unname(c(paper_cat["red"], paper_ink$muted, paper_cat["blue"])), align_lab)
bm[, align := factor(align_lab[as.character(a_cp)], levels = align_lab)]
dm <- merge(dm, unique(ec[, .(c, p, a_cp)]), by = c("c", "p"))
dm[, align := factor(align_lab[as.character(a_cp)], levels = align_lab)]
lab_fun <- function(d, n_each = 3) {
  d <- copy(d)[, gap := po - fs]
  rbind(d[order(gap)][1:n_each], d[order(-gap)][1:n_each])[, lab := paste0(p, " %->% ", c)]
}
sc_theme <- theme_paper(base_size = 8.5, grid = "xy") +
  theme(axis.line.y = element_line(colour = paper_ink$axis, linewidth = 0.3))
sc_panel <- function(d, xlab, ylab, sub) {
  ggplot(d, aes(fs, po)) +
    geom_abline(slope = 1, intercept = 0, colour = paper_ink$axis, linewidth = 0.35) +
    geom_point(aes(colour = align), size = 0.8, alpha = 0.55, stroke = 0) +
    geom_text_repel(data = lab_fun(d), aes(label = lab), parse = TRUE, size = 2.5,
                    colour = paper_ink$primary, segment.colour = paper_ink$muted, segment.size = 0.25,
                    min.segment.length = 0, box.padding = 0.35, seed = 7, max.overlaps = Inf,
                    bg.color = "white", bg.r = 0.12) +
    scale_colour_manual(values = align_cols, name = "UNGA bloc alignment of the pair", drop = FALSE) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    coord_equal() +
    labs(x = xlab, y = ylab, subtitle = sub) +
    guides(colour = guide_legend(override.aes = list(size = 2.2, alpha = 1))) +
    sc_theme
}
p_a <- sc_panel(bm, expression(FS[cp] ~ "(c friend-shores with p)"), expression(PO[pc] ~ "(p exports to c)"),
                "(a) Batteries Midstream, one point per ordered pair")
p_b <- sc_panel(dm, expression("Mean " * FS[cp] * " over matched cells"), expression("Mean " * PO[pc] * " over matched cells"),
                "(b) Mean over the cells rated by both pillars")
leg_sc <- cowplot::get_plot_component(p_a + theme(legend.position = "bottom"), "guide-box-bottom", return_all = TRUE)
if (is.list(leg_sc) && !inherits(leg_sc, "grob")) leg_sc <- leg_sc[[1]]
fig_sc <- cowplot::plot_grid(
  cowplot::plot_grid(p_a + theme(legend.position = "none"), p_b + theme(legend.position = "none"), nrow = 1, align = "hv"),
  leg_sc, ncol = 1, rel_heights = c(1, 0.08))
save_paper_fig(fig_sc, "bilateral_fs_po", width = paper_width_full, height = 3.75)

# ---------------------------------------------------------------------------------------------
# Outward investment counterparts and Development Potential
# ---------------------------------------------------------------------------------------------
ob <- unique(fsw[!is.na(outbound_index) & cmfdi_cell == FALSE & reporter_iso %in% c("USA", "JPN"),
                 .(reporter_iso, partner_iso, outbound_index)])
stopifnot(!anyDuplicated(ob[, .(reporter_iso, partner_iso)]))
res$outbound <- lapply(setNames(c("USA", "JPN"), c("USA", "JPN")), function(r) {
  z <- ob[reporter_iso == r][order(-outbound_index)]
  list(n_counterparts_with_position = nrow(z), top10 = z[1:10, partner_iso],
       top10_conduit_centres = sum(z[1:10, partner_iso] %in% conduits))
})
dpc <- unlist(wcfg$partnership_development_components)
dp_rk <- merge(dp_top, psi[, .(partner_iso, sovereign)], by = "partner_iso")[sovereign == TRUE & dp_share <= tau + 1e-9][order(-tdp)]
dp_rk[, rank := frank(-tdp, ties.method = "min")]
dp5 <- dpw[partner_iso %in% dp_rk[1:10, partner_iso] & !is.na(dp)][order(partner_iso, -dp, tech, supply_chain)][, head(.SD, 5), by = partner_iso]
dp_contrib <- dp5[, {
  m <- as.matrix(.SD[, names(dpc), with = FALSE])
  wm <- sweep(!is.na(m), 2, dpc, `*`)
  cm <- ifelse(is.na(m), 0, sweep(m, 2, dpc, `*`)) / rowSums(wm)
  as.list(colMeans(cm))
}, by = partner_iso]
res$development <- list(
  n_scored = nrow(dp_top), n_ranked = nrow(dp_rk),
  top10 = lapply(seq_len(10), function(k) with(dp_rk[k], list(rank = rank, iso3 = partner_iso, name = nm(partner_iso), T_DP = r3(tdp)))),
  top10_best5_stage_shares = as.list(r3(table(factor(dp5$supply_chain, c("Upstream", "Midstream", "Downstream"))) / nrow(dp5))),
  top10_best5_aid_share_of_composite = r3(dp_contrib[, sum(aid_index_supplychain + aidgdp_index_supplychain)] /
                                            sum(dp_contrib[, names(dpc), with = FALSE])),
  aid_nominal_share = r3(sum(dpc[c("aid_index_supplychain", "aidgdp_index_supplychain")]) / sum(dpc))
)

# ---------------------------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------------------------
res$meta <- list(
  script = "scripts/paper/fig_bilateral.R",
  build = "final rerun of 2026-10-12 (scripts 04 10 12 13 20 15 25, 30, 40)",
  inputs = c("data/processed/partner_friendshore_tbl.rds", "data/processed/partner_opportunity_tbl.rds",
             "data/processed/partner_development_tbl.rds", "data/processed/partner_*_top5_tbl.rds",
             "data/processed/partner_trade_coverage_tbl.rds", "data/processed/psi_tbl.rds",
             "data/processed/psi_full_coalition_tbl.rds", "data/processed/psi_world_tbl.rds",
             "data/processed/psi_dyad_tbl.rds", "data/processed/coalition_tbl.rds",
             "data/processed/geo_alignment_tbl.rds", "data/processed/partner_cm_fdi_exposure_tbl.rds",
             "data/processed/partner_cm_fdi_ties_tbl.rds", "data/processed/partner_cm_fdi_coverage_tbl.rds",
             "data/processed/cm_fdi_projects_tbl.rds", "data/raw/comtrade_energy_dyads_all.csv",
             "data/raw/energy_hs6_master.csv", "data/raw/ipcc_ghg_intensity.csv",
             "data/raw/CAT_country ratings data.csv", "data/raw/wdi_country_info.csv",
             "config/weights.yml", "config/partnership.yml", "config/rankability.yml"),
  weights = list(friendshore = as.list(w_fs), opportunity = as.list(w_po), opportunity_climate_weight = po_climate_weight,
                 friendshore_penalty_coefficient = fs_penalty_coef, psi = as.list(w_psi)),
  core_members = core, partner_tier = partners_tier,
  correlations = "Spearman rank correlations throughout",
  rankings = paste0("PSI ranks are the pipeline's (psi_*_tbl rank / rankable: sovereign and imputed weight share <= ", tau,
                    "); supplier lists rank partners that are sovereign and whose Friendshore dyad imputed weight share <= ", tau)
)

# ---------------------------------------------------------------------------------------------
# Table: coverage of the bilateral tables (numbers from res$coverage, written as LaTeX)
# ---------------------------------------------------------------------------------------------
fmt_n <- function(x) formatC(x, format = "d", big.mark = "{,}")
fmt_p <- function(x) sprintf("%.1f\\%%", 100 * x)
cvf <- res$coverage$friendshore; cvo <- res$coverage$opportunity; cvd <- res$coverage$development
dp_nonhigh <- uniqueN(dpw$partner_iso)
aid <- cvd$partners_with_stage_aid
cov_rows <- c(
  sprintf("Reporters & %s & %s & --\\\\", fmt_n(cvf$reporters), fmt_n(cvo$reporters)),
  sprintf("Partners & %s & %s & %s\\\\", fmt_n(cvf$partners), fmt_n(cvo$partners), fmt_n(dp_nonhigh)),
  sprintf("Reporter--partner pairs & %s & %s & --\\\\", fmt_n(cvf$pairs), fmt_n(cvo$pairs)),
  sprintf("Cells $(i,j,t,s)$ or $(j,t,s)$ & %s & %s & %s\\\\", fmt_n(cvf$cells), fmt_n(cvo$cells), fmt_n(cvd$cells)),
  sprintf("Technology--stage cells with an HS6 basket & %s & %s & --\\\\", cvf$tech_stage_cells_with_trade_codes,
          cvo$tech_stage_cells_with_trade_codes),
  sprintf("Reporters with observed bilateral trade & %s & -- & --\\\\", fmt_n(cvf$reporters_with_observed_trade)),
  sprintf("Share of basket cells with observed trade & %s & %s & --\\\\", fmt_p(cvf$share_trade_observed), fmt_p(cvo$share_trade_observed)),
  sprintf("\\quad of which from the partner's mirror declaration & %s & %s & --\\\\", fmt_p(cvf$share_observed_from_mirror),
          fmt_p(cvo$share_observed_from_mirror)),
  sprintf("Reporters with IMF outward positions (other cells) & %s & -- & --\\\\", fmt_n(cvf$reporters_filing_dip)),
  sprintf("Share of cells with an investment component & %s & -- & --\\\\", fmt_p(cvf$share_cells_with_investment)),
  sprintf("\\quad CM-FDI cells: share with a project tie & %s & -- & --\\\\", fmt_p(cvf$share_cmfdi_cells_with_investment_tie)),
  sprintf("\\quad CM-FDI cells: share with Chinese-parent exposure & %s & -- & --\\\\", fmt_p(cvf$share_cmfdi_cells_with_chinese_parent_exposure)),
  sprintf("Share of cells with reporter need missing & %s & -- & --\\\\", fmt_p(cvf$share_cells_need_missing)),
  sprintf("Partners with stage aid (Up/Mid/Down) & -- & -- & %s/%s/%s\\\\", aid$Upstream, aid$Midstream, aid$Downstream),
  sprintf("Partners in $\\PSI_j$ (core view); pairs in $\\PSI_{ij}$ & \\multicolumn{3}{c}{%s partners; %s pairs}\\\\",
          fmt_n(res$coverage$psi$partners), fmt_n(res$coverage$psi$dyads))
)
cov_tex <- c(
  "% Generated by scripts/paper/fig_bilateral.R; numbers in paper/results/bilateral.json (coverage).",
  "\\begin{table}[t]", "\\centering", "\\begin{threeparttable}",
  "\\caption{Coverage of the bilateral partnership tables}", "\\label{tab:bilateral_coverage}", "\\footnotesize",
  "\\begin{tabular}{@{}l r r r@{}}", "\\toprule", " & Friendshore & Opportunity & Development\\\\", "\\midrule",
  cov_rows, "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item \\textit{Notes:} Final rerun of 2026-10-12. Trade shares are over the dyad cells of the ",
         cvf$tech_stage_cells_with_trade_codes, " technology--stage cells with an HS6 basket; observed trade is the ",
         "importer's own declaration or, where the importer does not report, the exporter's mirror declaration times ",
         "the CIF/FOB factor ", cif_fob, " (all-reporter Comtrade dyads, 2020 and 2024). The investment component is the ",
         "IMF outward direct-investment position of the reporter in the partner (16 conduit financial centres excluded) ",
         "except in the four cells (Batteries, Electric Vehicles, Solar and Wind Midstream) where it is the reporter's ",
         "cross-border manufacturing projects in the partner recorded in CM-FDI. Development Potential covers the partners ",
         "that are not high-income. Source: \\texttt{partner\\_*\\_tbl.rds}, \\texttt{partner\\_trade\\_coverage\\_tbl.rds}, ",
         "\\texttt{psi\\_tbl.rds}, \\texttt{psi\\_dyad\\_tbl.rds}; \\texttt{scripts/paper/fig\\_bilateral.R}."),
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
writeLines(cov_tex, file.path(paper_dir("tables"), "bilateral_coverage.tex"), useBytes = TRUE)

write_paper_results(res, "bilateral")
message("bilateral: done")
