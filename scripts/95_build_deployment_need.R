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
        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))


# Deployment Need and national home-market growth (spec Parts V.2 and VI; design D12, D13).
#
#   Rscript scripts/95_build_deployment_need.R
#
# Runs after scripts/10 (import dependence), 17 (IEA WEO World pathways), 18 (WEO 2025 regions) and 20
# (the country universe); scripts/26 reads both outputs (deployment_need at Deployment & Use; the home
# market as the national lift of the industrial prize). Rules: R/strategic_position/deployment_need.R and
# R/strategic_position/home_market.R; settings: config/deployment_need.yml (OPSI_DEPLOYMENT_NEED overrides).
#
# Inputs (data/raw; licences in config/raw_inputs_manifest.yml):
# - ember/release_generation_yearly_global.csv  Ember yearly electricity data (CC BY 4.0): generation,
#   capacity, demand by country, year and source.
# - ember/targets_download.xlsx  Ember 2030 Global Renewable Target Tracker (CC BY 4.0): 2030 solar, wind
#   and renewable capacity targets; approved national-plan capacities (nuclear, battery, coal, gas).
# - EV data by country 2026.csv  IEA Global EV Outlook 2026: EV sales share, sales and stock (historical;
#   Current Policies and Stated Policies projections for CHN, IND, JPN, KOR, USA).
# - ei_stat_review_world_energy.csv  Energy Institute Statistical Review 2026: oil use (oil_tes_ej, EJ).
# - ei_stat_review_world_energy_wide.xlsx, sheet "Grid Scale BESS Capacity": stationary battery capacity.
# and (data/processed) import_dependence_tbl.rds (scripts/10), climate_src_weo_values_tbl.rds and
# climate_src_weo_geo_tbl.rds (scripts/18), iea_pathways_values_tbl.rds (scripts/17),
# outputs/index_outputs.rds (scripts/20; the country universe, as scripts/26 builds it).
# BNEF (subscription) is never read here (D13).
#
# Outputs:
# - data/processed/deployment_need_tbl.rds  one row per universe country x technology at Deployment:
#   deployment_need (0-1), deployment_need_pct, dn_families_observed, dn_confidence, dn_basis,
#   dn_scenarios; per family dn_<f> (family percentile), dn_<f>_score, _basis, _scenario, _measures,
#   _source, _note; dn_climate_aligned* (World, never in the score); dn_regional_proxy_* (context);
#   dn_target_* / dn_re_* (target detail); deployment_trend*; dnm_<measure>* (each measure's raw value,
#   percentile, basis, years, source, context).
# - data/processed/deployment_need_measures_tbl.rds  the long measure table (one row per country x
#   technology x measure).
# - data/processed/home_market_tbl.rds  one row per universe country x technology: io_home_market (0-1,
#   national evidence only), io_home_market_basis, home_market_pct, hm_* per kind (scenario, target,
#   observed_trend) with additions, growth, source and years.
# - diagnostics/strategic_position/deployment_need.csv, deployment_need_coverage.csv,
#   deployment_need_review.csv (spec XXI.4 countries and technologies), deployment_need_targets.csv,
#   deployment_need_home_market.csv.
# IO only.

source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "strategic_position", "sp_helpers.R"))
source(file.path(repo_root, "R", "strategic_position", "deployment_need.R"))
source(file.path(repo_root, "R", "strategic_position", "home_market.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(processed_dir, "outputs")
}
diagnostics_dir <- file.path(repo_root, "diagnostics", "strategic_position")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
cfg <- yaml::read_yaml(Sys.getenv("OPSI_DEPLOYMENT_NEED", file.path(repo_root, "config", "deployment_need.yml")))

read_required <- function(path, hint) {
  if (!file.exists(path)) stop(path, " not found; ", hint)
  readRDS(path)
}
need_file <- function(path, hint) {
  if (!file.exists(path)) stop(path, " not found; ", hint)
  path
}

# ---- Country universe (as scripts/26 builds it) ---------------------------------------------------------
index_outputs <- read_required(file.path(outputs_dir, "index_outputs.rds"), "run scripts/20_build_indices.R.")
universe <- data.frame(Country = sort(unique(c(index_outputs$economic_opportunity_index$Country,
                                               index_outputs$energy_security_index$Country))),
                       stringsAsFactors = FALSE)
rm(index_outputs)
universe$iso3 <- country_name_to_iso3(universe$Country)
universe <- universe[!is.na(universe$iso3) & !duplicated(universe$iso3), c("iso3", "Country")]
message("Universe: ", nrow(universe), " countries.")

# ---- Inputs --------------------------------------------------------------------------------------------
ember_path <- need_file(file.path(raw_data_path, "ember", "release_generation_yearly_global.csv"),
                        "stage Ember yearly electricity data (config/raw_inputs_manifest.yml, ember_yearly_electricity).")
targets_path <- need_file(file.path(raw_data_path, "ember", "targets_download.xlsx"),
                          "stage the Ember 2030 target tracker (config/raw_inputs_manifest.yml, ember_renewable_targets_2030).")
gevo_path <- need_file(file.path(raw_data_path, "EV data by country 2026.csv"), "stage IEA GEVO 2026 (iea_ev_outlook).")
ei_path <- need_file(file.path(raw_data_path, "ei_stat_review_world_energy.csv"), "stage the EI Statistical Review long file.")
ei_wide_path <- need_file(file.path(raw_data_path, "ei_stat_review_world_energy_wide.xlsx"), "stage the EI Statistical Review wide workbook.")

ember <- dn_ember_prepare(utils::read.csv(ember_path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA")))
message("Ember: ", length(unique(ember$iso3)), " economies, ", min(ember$year), "-", max(ember$year), ".")

tsheets <- readxl::excel_sheets(targets_path)
for (s in c("capacity_target_wide", "raw_data_long", "sources")) if (!s %in% tsheets) stop(targets_path, ": sheet ", s, " missing.")
targets <- dn_targets_prepare(readxl::read_excel(targets_path, sheet = "capacity_target_wide"),
                              readxl::read_excel(targets_path, sheet = "raw_data_long"),
                              readxl::read_excel(targets_path, sheet = "sources"), cfg$targets)
message("Targets: ", nrow(targets), " (", paste(names(table(targets$target_key)), table(targets$target_key), sep = " ", collapse = ", "), ").")

weo_values <- read_required(file.path(processed_dir, "climate_src_weo_values_tbl.rds"), "run scripts/18_build_climate_sources.R.")
weo_geo <- read_required(file.path(processed_dir, "climate_src_weo_geo_tbl.rds"), "run scripts/18_build_climate_sources.R.")
weo_national <- dn_weo_national(weo_values, weo_geo)
weo_regional <- dn_weo_regional(weo_values, weo_geo)
pathways <- read_required(file.path(processed_dir, "iea_pathways_values_tbl.rds"), "run scripts/17_build_iea_pathways.R.")
import_dep <- dn_import_dependence(read_required(file.path(processed_dir, "import_dependence_tbl.rds"), "run scripts/10_build_themes.R."))

gevo <- utils::read.csv(gevo_path, check.names = FALSE, stringsAsFactors = FALSE)
gevo_share <- dn_gevo_prepare(gevo, "EV sales share", to_iso3 = country_name_to_iso3)
gevo_stock <- dn_gevo_prepare(gevo, "EV stock", to_iso3 = country_name_to_iso3)
gevo_sales <- dn_gevo_prepare(gevo, "EV sales", to_iso3 = country_name_to_iso3)
rm(gevo)

ei_long <- utils::read.csv(ei_path, stringsAsFactors = FALSE)
oil <- dn_ei_series(ei_long, "oil_tes_ej")
rm(ei_long)
bess_sheet <- grep("BESS", readxl::excel_sheets(ei_wide_path), value = TRUE)
bess <- if (length(bess_sheet)) {
  dn_ei_bess_prepare(suppressMessages(readxl::read_excel(ei_wide_path, sheet = bess_sheet[1], col_names = FALSE)),
                     to_iso3 = country_name_to_iso3)
} else {
  warning("No BESS sheet in ", ei_wide_path, "; battery capacity not assessed.")
  NULL
}
message("EI BESS: ", if (is.null(bess)) 0 else length(unique(bess$iso3)), " countries.")

inp <- list(ember = ember, weo_national = weo_national, import_dep = import_dep, gevo_share = gevo_share,
            gevo_stock = gevo_stock, gevo_sales = gevo_sales, oil = oil, targets = targets, bess = bess)

# ---- Deployment Need -------------------------------------------------------------------------------------
built <- dn_build_measures(inp, cfg)
dn_tbl <- dn_assemble(built$measures, universe, cfg)
scored <- dn_score_measures(built$measures, universe$iso3, cfg)

# ---- Home market -----------------------------------------------------------------------------------------
hm_measures <- hm_build_measures(inp, built, cfg)
hm_tbl <- hm_assemble(hm_measures, universe, cfg)

# ---- Context columns (never scored) ------------------------------------------------------------------
join_by_key <- function(x, y, by) {
  y <- y[!duplicated(y[, by, drop = FALSE]), , drop = FALSE]
  i <- match(do.call(paste, x[, by, drop = FALSE]), do.call(paste, y[, by, drop = FALSE]))
  for (col in setdiff(names(y), by)) x[[col]] <- y[[col]][i]
  x
}
dn_tbl <- join_by_key(dn_tbl, dn_climate_aligned(pathways, cfg), "tech")
dn_tbl <- join_by_key(dn_tbl, dn_regional_proxy(weo_regional, cfg), "iso3")
td <- if (!is.null(built$shortfall)) dn_target_detail(built$shortfall, cfg) else list(own = NULL, renewables = NULL)
if (!is.null(td$own)) dn_tbl <- join_by_key(dn_tbl, td$own, c("iso3", "tech"))
if (!is.null(td$renewables)) dn_tbl <- join_by_key(dn_tbl, td$renewables, c("iso3", "tech"))
dn_tbl <- join_by_key(dn_tbl, dn_deployment_trend(hm_measures), c("iso3", "tech"))
dn_tbl <- join_by_key(dn_tbl, dn_measures_wide(scored, cfg), c("iso3", "tech"))

meta <- list(
  created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  scenarios = vapply(cfg$families, function(f) f$scenario, character(1)),
  ember_years = range(ember$year),
  sources = c(
    ember = "Ember, Yearly Electricity Data (CC BY 4.0)",
    ember_targets = "Ember, 2030 Global Renewable Target Tracker (CC BY 4.0)",
    weo = "IEA World Energy Outlook 2025 free datasets (CC BY-NC-SA 4.0): single-country regions only",
    gevo = "IEA Global EV Outlook 2026",
    ei = "Energy Institute Statistical Review of World Energy 2026",
    import_dependence = "EI Statistical Review / EIA (data/processed/import_dependence_tbl.rds)"
  )
)
attr(dn_tbl, "meta") <- meta
attr(hm_tbl, "meta") <- meta

saveRDS(dn_tbl, file.path(processed_dir, "deployment_need_tbl.rds"))
saveRDS(built$measures, file.path(processed_dir, "deployment_need_measures_tbl.rds"))
saveRDS(hm_tbl, file.path(processed_dir, "home_market_tbl.rds"))

# ---- Diagnostics ---------------------------------------------------------------------------------------
write_diag <- function(x, name) {
  utils::write.csv(x, file.path(diagnostics_dir, name), row.names = FALSE, na = "")
}
r3 <- function(x) if (is.numeric(x)) round(x, 3) else x
main_cols <- c("Country", "iso3", "tech", "deployment_need", "deployment_need_pct", "dn_families_observed", "dn_confidence",
               "dn_basis", unlist(lapply(DN_FAMILIES, function(f) paste0(f, c("", "_basis", "_measures")))),
               "deployment_trend")
diag_main <- dn_tbl[dn_tbl$dn_families_observed > 0, main_cols]
diag_main[] <- lapply(diag_main, r3)
write_diag(diag_main[order(diag_main$tech, -diag_main$deployment_need), ], "deployment_need.csv")
write_diag(dn_coverage(dn_tbl, scored), "deployment_need_coverage.csv")

rv <- cfg$review
review <- dn_tbl[dn_tbl$iso3 %in% unlist(rv$countries) & dn_tbl$tech %in% unlist(rv$techs),
                 c("iso3", "tech", "deployment_need", "dn_families_observed", "dn_demand", "dn_demand_basis",
                   "dn_policy_gap", "dn_policy_gap_basis", "dn_system", "dn_system_basis", "dn_demand_note",
                   "dn_policy_gap_note", "dn_system_note", "deployment_trend")]
hm_rv <- hm_tbl[, c("iso3", "tech", "io_home_market", "hm_primary_kind")]
review <- join_by_key(review, hm_rv, c("iso3", "tech"))
review$iso3 <- factor(review$iso3, levels = unlist(rv$countries))
review$tech <- factor(review$tech, levels = unlist(rv$techs))
review <- review[order(review$iso3, review$tech), ]
review[] <- lapply(review, function(x) if (is.factor(x)) as.character(x) else r3(x))
write_diag(review, "deployment_need_review.csv")

if (!is.null(built$shortfall)) {
  sf <- built$shortfall
  sf[] <- lapply(sf, r3)
  write_diag(sf[order(sf$target_key, sf$iso3), ], "deployment_need_targets.csv")
}
hm_diag <- hm_tbl[hm_tbl$hm_kinds_observed > 0, c("Country", "iso3", "tech", "io_home_market", "home_market_pct", "hm_kinds_observed",
                                                   "hm_unit", "hm_primary_kind", "hm_primary_additions", "hm_primary_growth_rate",
                                                   "hm_primary_years", "hm_scenario_additions", "hm_target_additions",
                                                   "hm_observed_trend_additions", "hm_primary_source")]
hm_diag[] <- lapply(hm_diag, r3)
write_diag(hm_diag[order(hm_diag$tech, -hm_diag$io_home_market), ], "deployment_need_home_market.csv")

# ---- Console summary --------------------------------------------------------------------------------------
cov <- stats::aggregate(cbind(dn_demand = !is.na(dn_tbl$dn_demand), dn_policy_gap = !is.na(dn_tbl$dn_policy_gap),
                              dn_system = !is.na(dn_tbl$dn_system), deployment_need = !is.na(dn_tbl$deployment_need),
                              home_market = !is.na(hm_tbl$io_home_market[match(paste(dn_tbl$iso3, dn_tbl$tech), paste(hm_tbl$iso3, hm_tbl$tech))])),
                        by = list(tech = dn_tbl$tech), FUN = sum)
message("Coverage (countries with a value, of ", nrow(universe), "):")
print(cov, row.names = FALSE)
message("Wrote deployment_need_tbl.rds (", nrow(dn_tbl), " rows), home_market_tbl.rds (", nrow(hm_tbl), " rows) and diagnostics.")
