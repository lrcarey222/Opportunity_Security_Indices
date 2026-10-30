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


# Strategic position framework, canonical decision chain Need -> Prize -> Position -> Choice -> Policy
# alignment (docs/strategic_position.md):
#   Step 1 Position and Need: Competitive Position, Strategic Need (-> supply_chain_need at the producing
#          stages) and Revealed Policy Priority per country x technology x six-stage cell, with evidence;
#   Step 2 Secure override: the Yards & Fences engine's three Secure routes (its Compete tests are kept as
#          diagnostics, engine_posture), for each cell and each sensitive layer;
#   Step 2b Prize, pathway, deployment need and Choice: industrial_opportunity (node prize + national home
#          market), the Catch Up pathway, deployment_need (scripts/95), and the canonical choice (Secure,
#          Preserve, Catch Up, Partner, Accelerate, Watch, or not assessed); the legacy need x EO quadrant
#          is kept as legacy_quadrant_* for comparison only;
#   Step 3 Policy Design: fence, gate and build for the posture and delivery mode;
#   Step 4 Policy alignment: recommended choice vs observed policy (never an input).
# Reads index_outputs.rds (scripts/20), policy_outputs.rds (scripts/10), the NIPO six-stage tables
# (scripts/11), import_exposure_tbl.rds (scripts/15), the supplier-share and alignment tables, and the
# reference tables in config/ and data/reference/. Writes strategic_position_tbl (cells),
# strategic_posture_layers_tbl (sensitive layers), their inputs, and diagnostics/strategic_position/.

library(dplyr)

for (f in c(
  "R/utils/schema.R", "R/utils/scurve.R", "R/utils/country.R", "R/utils/supply_chain_stages.R",
  "R/utils/variable_stage_evidence.R", "R/indices/build_indices.R", "R/indices/build_policy_index.R",
  "R/categories/policy/cat_policy_index.R",
  "R/strategic_position/sp_helpers.R", "R/strategic_position/competitive_position.R",
  "R/strategic_position/import_exposure.R", "R/strategic_position/strategic_need.R",
  "R/strategic_position/policy_priority.R", "R/strategic_position/alignment.R",
  "R/strategic_position/supply_structure.R", "R/strategic_position/posture_nodes.R",
  "R/strategic_position/posture_engine.R", "R/strategic_position/posture_inputs.R",
  "R/strategic_position/policy_design.R", "R/strategic_position/legacy_si_sensitivity.R",
  "R/strategic_position/sp_diagnostics.R", "R/categories/trade/trade_core.R",
  "R/strategic_position/trade_benchmark.R", "R/strategic_position/trade_momentum.R",
  "R/strategic_position/security_routes.R", "R/strategic_position/decision_quadrant.R",
  "R/strategic_position/partner_recs.R", "R/strategic_position/sp_diag_framework.R",
  "R/strategic_position/industrial_opportunity.R", "R/strategic_position/supply_chain_need.R",
  "R/strategic_position/pathway.R", "R/strategic_position/decision_engine.R", "R/strategic_position/sp_diag_choice.R",
  "R/categories/investment/investment_momentum.R"
)) source(file.path(repo_root, f))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
sp_cfg <- yaml::read_yaml(Sys.getenv("OPSI_STRATEGIC_POSITION", file.path(repo_root, "config", "strategic_position.yml")))
post_cfg <- sp_cfg$strategic_posture
dec_cfg <- sp_cfg$decision_framework
io_cfg <- dec_cfg$industrial_opportunity

processed_dir <- file.path(repo_root, config$processed_dir)
raw_data_path <- file.path(repo_root, config$raw_data_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(processed_dir, "outputs")
}
diagnostics_dir <- file.path(repo_root, "diagnostics", "strategic_position")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

read_required <- function(path, hint) {
  if (!file.exists(path)) stop(path, " not found; ", hint)
  readRDS(path)
}
read_ref <- function(path) {
  utils::read.csv(file.path(repo_root, path), colClasses = "character", check.names = FALSE, na.strings = character())
}
index_outputs <- read_required(file.path(outputs_dir, "index_outputs.rds"), "run scripts/20_build_indices.R.")
policy_outputs <- read_required(file.path(processed_dir, "policy_outputs.rds"), "run scripts/10_build_themes.R.")
nipo_cells <- read_required(file.path(processed_dir, "nipo_stage_cells_tbl.rds"), "run scripts/11_build_nipo_stage_policy.R.")
nipo_country <- read_required(file.path(processed_dir, "nipo_stage_country_tbl.rds"), "run scripts/11_build_nipo_stage_policy.R.")
import_exposure <- read_required(file.path(processed_dir, "import_exposure_tbl.rds"), "run scripts/15_build_partner_themes.R.")
geo_alignment <- read_required(file.path(processed_dir, "geo_alignment_tbl.rds"), "run scripts/12_build_geopolitical_alignment.R.")
major_supplier_shares <- read_required(file.path(processed_dir, "major_supplier_shares_tbl.rds"), "run scripts/10_build_themes.R.")
tech_ghg <- read_required(file.path(processed_dir, "tech_ghg_tbl.rds"), "run scripts/10_build_themes.R.")
evidence <- vse_read_check(utils::read.csv(
  file.path(repo_root, "data", "reference", "index_variable_stage_evidence.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = ""
))
yf_components <- read_ref("data/reference/yards_fences_assessments.csv")
node_attributes <- pn_read_check(read_ref(sp_cfg$policy_design$node_table), yf_components)
design_tables <- ppd_read_check(read_ref(sp_cfg$policy_design$design_table), read_ref(sp_cfg$policy_design$delivery_table))
cost_raw_path <- file.path(raw_data_path, "Relative_Costs_IEA.csv")

techs <- unlist(sp_cfg$headline_techs)
eo_contrib <- index_outputs$economic_opportunity_variable_contributions
es_contrib <- index_outputs$energy_security_variable_contributions
coalition <- read_required(file.path(processed_dir, "coalition_tbl.rds"), "run scripts/13_build_coalition.R.")
coalition_members <- coalition$iso3[coalition$tier %in% c("core", "partner")]

# ---- PRIZE inputs: world demand growth (IEA WEO 2025 / Critical Minerals, scripts/17), global
# investment momentum and pipeline (Clean Investment Monitor), maturity (IEA Clean Tech Guide TRL) ------
iea_map <- read_required(file.path(processed_dir, "iea_pathways_map_tbl.rds"), "run scripts/17_build_iea_pathways.R.")
iea_values <- read_required(file.path(processed_dir, "iea_pathways_values_tbl.rds"), "run scripts/17_build_iea_pathways.R.")
iea_cells <- read_required(file.path(processed_dir, "iea_pathways_cells_tbl.rds"), "run scripts/17_build_iea_pathways.R.")
iea_minerals <- read_required(file.path(processed_dir, "iea_pathways_minerals_tbl.rds"), "run scripts/17_build_iea_pathways.R.")
investment_tbl <- read_required(file.path(processed_dir, "investment_momentum_tbl.rds"), "run scripts/10_build_themes.R.")
trl_tbl <- read_required(file.path(processed_dir, "technological_readiness_tbl.rds"), "run scripts/10_build_themes.R.")
io_growth <- sp_io_node_growth(iea_map, iea_values, iea_cells, iea_minerals, techs = techs, cfg = io_cfg)
io_momentum <- sp_io_investment_momentum(
  investment_tbl[investment_tbl$variable == "Annual Investment (USD bn, 2024$)" & investment_tbl$tech %in% techs, ],
  io_cfg$investment_momentum
)
io_maturity <- trl_tbl %>%
  dplyr::filter(.data$variable == "TRL 2025", .data$tech %in% techs, !is.na(.data$value)) %>%
  dplyr::distinct(.data$tech, .data$supply_chain, .keep_all = TRUE) %>%
  dplyr::transmute(.data$tech, .data$supply_chain, io_maturity = round(.data$value, 2))
# The technology outlook the engine's catch-up test and the legacy cap read: from data, with the
# documented overrides (config decision_framework.industrial_opportunity.market_outlook_override).
post_cfg$market_outlook <- as.list(sp_io_tech_outlook(io_growth, io_cfg$market_outlook_override))
pipeline_raw <- investment_tbl %>%
  dplyr::filter(.data$variable == "Pipeline Capacity (raw stage sum)", .data$tech %in% techs) %>%
  dplyr::group_by(iso3 = .data$iso3c, .data$tech, .data$supply_chain) %>%
  dplyr::summarise(pipeline_capacity = sum(.data$value, na.rm = TRUE), .groups = "drop")
pipeline_covered <- dplyr::distinct(pipeline_raw, .data$tech, .data$supply_chain)
# GCIM capacity by product and status (the Catch Up anchors operating_capacity and committed_pipeline, and
# the announced_pipeline support): each country's share of world operating capacity, per product, the stage
# taking its most material product (investment_capacity_shares()).
gcim_path <- file.path(raw_data_path, "GCIM_Investment_Capacity_aggregated.xlsx")
capacity_shares <- NULL
if (file.exists(gcim_path)) {
  gcim_sheet <- investment_resolve_gcim_sheet(readxl::excel_sheets(gcim_path), investment_gcim_sheet_patterns$capacity, "manufacturing/industry capacity")
  gcim_capacity <- investment_read_gcim_sheet(gcim_path, gcim_sheet, c("Country", "Segment", "Technology", "Product", "Category", "Value"),
                                              label = "manufacturing/industry capacity")
  capacity_shares <- investment_capacity_shares(investment_capacity_clean(gcim_capacity)) %>%
    dplyr::mutate(iso3 = country_name_to_iso3(.data$Country)) %>%
    dplyr::filter(!is.na(.data$iso3), .data$tech %in% techs) %>%
    dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(dplyr::across(c("operating_share", "committed_share", "announced_share"), max), .groups = "drop")
  rm(gcim_capacity)
} else {
  warning("GCIM capacity workbook not found (", gcim_path, "): the operating-capacity and committed-pipeline anchors are not observed.")
}
input_cost <- eo_contrib %>%
  dplyr::filter(.data$category == "Cost Competitiveness", .data$variable == "Input Cost Index", !.data$imputed, .data$tech %in% techs) %>%
  dplyr::transmute(iso3 = country_name_to_iso3(.data$Country), .data$tech, .data$supply_chain, input_cost_index = .data$component_value) %>%
  dplyr::filter(!is.na(.data$iso3))
read_optional <- function(file, hint) {
  path <- file.path(processed_dir, file)
  if (file.exists(path)) return(readRDS(path))
  warning(path, " not found (", hint, ").")
  NULL
}
home_market <- read_optional(io_cfg$home_market_file %||% "home_market_tbl.rds",
                             "scripts/95: the prize is the node prize everywhere")
deployment_need_tbl <- read_optional(dec_cfg$deployment_need$file %||% "deployment_need_tbl.rds",
                                     "scripts/95: every Deployment & Use cell is not assessed")

# ---- Trade evidence: the structural benchmark and current momentum (scripts/94) ---------------------
tb_cfg <- sp_cfg$competitive_position$trade_benchmark
tm_cfg <- sp_cfg$competitive_position$trade_momentum
trade_codes <- trade_core_build_energy_codes(readr::read_csv(file.path(raw_data_path, "hts_codes_categories_bolstered_final.csv"), show_col_types = FALSE))
reexport_hubs <- toupper(unlist(getOption("opportunity_security.index_definition")$trade$re_export_hubs))
annual_path <- file.path(raw_data_path, tb_cfg$annual_file)
trade_bench <- NULL
bench_idx <- NULL
annual_totals <- NULL
eo_contrib_cp <- eo_contrib
trade_benchmark_period <- "Atlas 2023 / Comtrade 2024 (single year)"
if (file.exists(annual_path)) {
  annual <- data.table::fread(annual_path, colClasses = list(character = c("cmd_code", "period")))
  trade_bench <- sp_trade_benchmark(annual, trade_codes, years = unlist(tb_cfg$years), exclude_reporters = reexport_hubs,
                                    min_years = tb_cfg$min_years %||% 2L)
  atlas_share <- readRDS(file.path(processed_dir, "trade_concentration_tbl.rds")) %>%
    dplyr::filter(.data$variable == "market_share", .data$data_type == "raw") %>%
    dplyr::transmute(iso3 = .data$iso3c, .data$tech, .data$supply_chain, market_share = .data$value)
  bench_idx <- sp_trade_benchmark_index(trade_bench, economies = stats::na.omit(country_name_to_iso3(unique(eo_contrib$Country))),
                                        atlas_share = atlas_share)
  eo_contrib_cp <- sp_apply_trade_benchmark(eo_contrib, bench_idx, country_name_to_iso3)
  trade_benchmark_period <- paste(range(unlist(tb_cfg$years)), collapse = "-")
  annual_totals <- annual %>% dplyr::filter(.data$cmd_code == SP_TB_TOTAL_CODE) %>%
    dplyr::group_by(.data$reporter_iso, year = as.integer(.data$period)) %>%
    dplyr::summarise(total_exports = sum(.data$primary_value), .groups = "drop")
} else {
  warning(annual_path, " not found (scripts/94_pull_comtrade_trade_refresh.R): Competitive Position keeps the single-year trade measures.")
}
tm_universe <- sort(unique(c(coalition_members, coalition$iso3[!is.na(coalition$gdp_usd) & coalition$gdp_usd >= tm_cfg$universe_min_gdp_usd])))
monthly_path <- file.path(raw_data_path, tm_cfg$monthly_file)
tm <- NULL
tm_cut <- NULL
if (file.exists(monthly_path)) {
  monthly <- data.table::fread(monthly_path, colClasses = list(character = "cmd_code"))
  tm_cut <- sp_tm_cutoff(monthly, tm_universe, window = tm_cfg$window_months, min_share = tm_cfg$min_complete_share)
  tm <- sp_trade_momentum(monthly, trade_codes, tm_cut, bench = trade_bench, annual_totals = annual_totals, cfg = tm_cfg)
} else {
  warning(monthly_path, " not found (scripts/94_pull_comtrade_trade_refresh.R): no current trade momentum.")
}

# ---- Step 1: the three dimensions ------------------------------------------------------------------
cp <- sp_competitive_position(eo_contrib_cp, es_contrib, sp_cfg$competitive_position, techs = techs)
cp_families <- names(sp_cfg$competitive_position$families)
cp_structural <- cp$cells
if (!is.null(tm)) {
  cp$cells <- sp_cp_blend_momentum(cp$cells, tm$cells, country_name_to_iso3, cp_families, weight = tm_cfg$momentum_weight)
}
sn <- sp_strategic_need(es_contrib, import_exposure, sp_cfg$strategic_need, techs = techs, geo_alignment = geo_alignment)
pams_cfg <- sp_cfg$policy_priority$pams
pams_links <- sp_pams_links(
  policy_outputs$policy_clean, techs = techs, stock_status = unlist(pams_cfg$stock_status),
  own_jurisdictions = unlist(pams_cfg$own_jurisdictions), international_single_country = isTRUE(pams_cfg$international_single_country)
)
pp <- sp_policy_priority(pams_links, nipo_cells, sp_cfg$policy_priority, techs = techs,
                         windows = unlist(sp_cfg$policy_priority$flow_windows_sensitivity))

# Evidence behind each dimension (reported next to the scores, never folded into them).
bands <- sp_cfg$evidence$quality_bands
specific <- unlist(sp_cfg$evidence$specific_statuses)
evidence_cells <- function(inputs, suffix, by = c("Country", "tech", "supply_chain")) {
  out <- vse_cell_specificity(inputs, evidence, specific = specific, bands = bands, by = by)
  keep <- setdiff(names(out), by)
  names(out)[match(keep, names(out))] <- paste0(keep, "_", suffix)
  out
}
sn_inputs <- sn$inputs %>% dplyr::filter(.data$weight > 0)
ev_cp <- evidence_cells(cp$inputs, "cp")
ev_sn <- evidence_cells(sn_inputs, "sn")
ev_pp <- evidence_cells(pp$inputs, "pp", by = c("iso3", "tech", "supply_chain"))

countries <- tibble::tibble(Country = sort(unique(c(cp$cells$Country, sn$cells$Country)))) %>%
  dplyr::mutate(iso3 = country_name_to_iso3(.data$Country)) %>%
  dplyr::filter(!is.na(.data$iso3), !duplicated(.data$iso3))
grid <- tidyr::expand_grid(countries, tech = techs, supply_chain = SUPPLY_CHAIN_STAGES) %>%
  dplyr::mutate(legacy_stage = supply_chain_stage_rollup(.data$supply_chain))

legacy_si <- index_outputs$strategic_index %>%
  dplyr::select("Country", "tech", "supply_chain", strategic_index_legacy = "strategic_index")
cat_context <- sp_cat_context(index_outputs$policy_component_tbl, tech_ghg = tech_ghg)

drop_dup <- function(tbl, keys) dplyr::select(tbl, -dplyr::any_of(setdiff(c("Country", "iso3", "legacy_stage"), keys)))
strategic_position_tbl <- grid %>%
  dplyr::left_join(drop_dup(cp$cells, c("Country", "tech", "supply_chain")), by = c("Country", "tech", "supply_chain")) %>%
  dplyr::left_join(drop_dup(sn$cells, c("Country", "tech", "supply_chain")), by = c("Country", "tech", "supply_chain")) %>%
  dplyr::left_join(drop_dup(pp$cells, c("iso3", "tech", "supply_chain")), by = c("iso3", "tech", "supply_chain")) %>%
  dplyr::left_join(ev_cp, by = c("Country", "tech", "supply_chain")) %>%
  dplyr::left_join(ev_sn, by = c("Country", "tech", "supply_chain")) %>%
  dplyr::left_join(ev_pp, by = c("iso3", "tech", "supply_chain")) %>%
  dplyr::mutate(stage_evidence_quality = .data$stage_evidence_quality_cp) %>%
  sp_alignment(sp_cfg$alignment) %>%
  dplyr::left_join(legacy_si, by = c("Country", "tech", "supply_chain")) %>%
  dplyr::left_join(cat_context, by = "Country")

# Trade evidence fields: the benchmark period and values, and the momentum signal with its quality.
strategic_position_tbl$trade_benchmark_period <- trade_benchmark_period
if (!is.null(bench_idx)) {
  strategic_position_tbl <- strategic_position_tbl %>%
    dplyr::left_join(bench_idx %>% dplyr::transmute(.data$iso3, .data$tech, .data$supply_chain, .data$trade_benchmark_source,
                                                    trade_benchmark_exports = .data$bench_exports, trade_benchmark_share = .data$bench_share,
                                                    trade_benchmark_rca = .data$bench_rca, trade_benchmark_n_years = .data$bench_n_years),
                     by = c("iso3", "tech", "supply_chain"))
}
tm_cols <- c("trade_momentum_period", "trade_momentum_previous_period", "trade_months_observed", "trade_momentum_quality",
             "trade_momentum_coverage", "trade_momentum_raw", "trade_momentum_specialization", "trade_momentum_share_change",
             "trade_momentum_score", "trade_exports_current", "trade_exports_previous")
if (!is.null(tm)) {
  strategic_position_tbl <- strategic_position_tbl %>%
    dplyr::left_join(tm$cells[, c("iso3", "tech", "supply_chain", tm_cols)], by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::mutate(
      # a universe member's cell without a traded HS6 basket has no trade to compare
      trade_momentum_quality = dplyr::coalesce(.data$trade_momentum_quality,
                                               dplyr::if_else(.data$iso3 %in% tm_universe, "no_basket", "outside_universe")),
      trade_momentum_period = dplyr::coalesce(.data$trade_momentum_period, tm$cells$trade_momentum_period[1]),
      trade_momentum_previous_period = dplyr::coalesce(.data$trade_momentum_previous_period, tm$cells$trade_momentum_previous_period[1])
    )
} else {
  for (cl in tm_cols) strategic_position_tbl[[cl]] <- NA
  strategic_position_tbl$trade_momentum_quality <- "no_monthly_data"
}

# ---- Step 2: strategic posture -----------------------------------------------------------------------
coercive_pairs <- sp_coercive_pairs(geo_alignment, unlist(post_cfg$coercive_powers),
                                    opposite_bloc = !isFALSE(post_cfg$coercive_opposite_bloc)) %>%
  sp_nonaligned_trust(post_cfg$nonaligned_trust %||% "bloc")
measure_priority <- lapply(post_cfg$supply_measure_priority, function(v) as.character(unlist(v)))
supply <- sp_supply_structure(major_supplier_shares, coercive_pairs, post_cfg$mineral_min_demand_share,
                              measure_priority = measure_priority, techs = techs)
premium <- if (file.exists(cost_raw_path)) {
  sp_cost_premium(utils::read.csv(cost_raw_path, check.names = FALSE), sp_cost_region_map(EU27_ISO3))
} else {
  warning("Relative_Costs_IEA.csv not found: the domestic premium is unknown everywhere.")
  NULL
}
feas <- eo_contrib %>%
  dplyr::filter(.data$category == "Trade", .data$variable == "feas", !.data$imputed, .data$tech %in% techs) %>%
  dplyr::distinct(.data$Country, .data$tech, .data$supply_chain, .keep_all = TRUE) %>%
  dplyr::transmute(.data$Country, .data$tech, .data$supply_chain, feas = .data$component_value)

# Curated coercive shares (Y&F non_allied_top_share_override) on the layers they belong to.
override_ids <- yf_components$component_id[nzchar(yf_components$non_allied_top_share_override)]
coercion_override <- node_attributes %>%
  dplyr::filter(.data$yf_component_id %in% override_ids, .data$country_iso3 == "*") %>%
  dplyr::transmute(.data$tech, supply_chain = .data$stage, .data$layer_id, .data$yf_component_id) %>%
  dplyr::left_join(yf_components %>% dplyr::select(yf_component_id = "component_id", "non_allied_top_share_override", "override_subject"),
                   by = "yf_component_id") %>%
  dplyr::transmute(.data$tech, .data$supply_chain, .data$layer_id,
                   coercive_top_share = as.numeric(.data$non_allied_top_share_override),
                   coercive_top_iso3 = country_name_to_iso3(.data$override_subject)) %>%
  # only for the countries to which the named supplier is coercive (not Russia itself or its bloc)
  tidyr::crossing(iso3 = countries$iso3) %>%
  dplyr::inner_join(coercive_pairs %>% dplyr::filter(.data$coercive %in% TRUE) %>%
                      dplyr::select(iso3 = "reporter_iso", coercive_top_iso3 = "partner_iso"),
                    by = c("iso3", "coercive_top_iso3"))

nodes <- pn_resolve(node_attributes, yf_components, iso3 = countries$iso3) %>%
  dplyr::filter(.data$tech %in% techs)
posture_context <- strategic_position_tbl %>%
  dplyr::select("Country", "tech", "supply_chain", "strategic_need_percentile", "stage_evidence_quality_cp", "cp_within_country_pct", "sn_import_dependence")
posture_inputs <- sp_posture_inputs(
  nodes, supply$cells, import_exposure, cp$cells, countries, post_cfg,
  supply_minerals = supply$minerals, supply_measures = supply$measures, feas = feas, premium = premium,
  context = posture_context, exclude_iso3 = unlist(sp_cfg$strategic_need$exclude_iso3),
  coercion_override = coercion_override, tech_ghg = tech_ghg
)
# The Secure routes each node may take (config/security_route_rules.csv), checked against the nodes.
sr_rules <- sr_read_check(read_ref(post_cfg$security_routes$rules_table), pn_resolve(node_attributes, yf_components, iso3 = countries$iso3[1]))
posture_inputs <- sp_attach_security_rules(posture_inputs, sr_rules)
posture <- sp_strategic_posture(posture_inputs, post_cfg)
# imports_usd and import_hhi stay as Strategic Need reports them on the cell table; layers carry the
# posture's own (which differ at Deployment).
# home_share is the engine's (Test 3 brackets an unlisted economy's share with the listing's residual);
# home_share_exposure is the measured share, or the lower bound where the listing omits the economy: the
# contender test, the consistency counts and the bundle's share read it (sp_measured_share()).
carry <- c("layer_label", "critical_function", "home_share", "home_share_exposure", "home_status", "coercive_top_iso3", "coercive_top_share",
           "coercive_world_share", "trusted_world_share", "coercive_import_share", "complementary",
           "adjacent_established", "resource_base", "market_pct", "premium_pct", "exposed", "supply_basis",
           "attr_confidence", "attr_provenance",
           # the pathway test's feasibility leg, the prize's maturity context and the Secure-engine
           # node attributes the drawer shows under NEED
           "feas_pct", "maturity", "leap_candidate", "market_outlook", "substitutability", "reconstitution_time",
           "qualification_time", "stockpile", "systemic_essentiality", "dual_use", "digitally_active")
layer_carry <- c(carry, "import_hhi", "imports_usd")
posture_all <- posture %>%
  dplyr::left_join(posture_inputs %>% dplyr::select("iso3", "tech", "supply_chain", "layer_id", dplyr::any_of(layer_carry)),
                   by = c("iso3", "tech", "supply_chain", "layer_id"))

# Sensitive layers: their own posture, never their cell's.
layers_tbl <- posture_all %>% dplyr::filter(.data$layer_id != "stage")
posture_all <- dplyr::select(posture_all, -dplyr::any_of(setdiff(layer_carry, carry)))
secure_layers <- layers_tbl %>%
  dplyr::filter(.data$strategic_posture %in% "secure") %>%
  dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
  dplyr::summarise(sensitive_layers_secure = paste(.data$layer_id, collapse = ";"), .groups = "drop")
cell_posture <- posture_all %>%
  dplyr::filter(.data$layer_id == "stage") %>%
  dplyr::select(-"Country", -"layer_id", -"layer_type", -dplyr::any_of(c("layer_label")))

strategic_position_tbl <- strategic_position_tbl %>%
  dplyr::left_join(cell_posture, by = c("iso3", "tech", "supply_chain")) %>%
  dplyr::left_join(secure_layers, by = c("iso3", "tech", "supply_chain"))

# ---- Step 2b: Prize, pathway, deployment need and the canonical Choice ----------------------------------
# Legacy: the need x legacy-EO quadrant (the rule until 2026-10-28), on a copy, for comparison only.
dq_cfg <- post_cfg$decision_quadrant
eo_index <- index_outputs$economic_opportunity_index %>%
  dplyr::filter(.data$rankable %in% TRUE, .data$tech %in% techs) %>%
  dplyr::transmute(iso3 = country_name_to_iso3(.data$Country), .data$tech, .data$supply_chain,
                   legacy_eo_opportunity = .data$Economic_Opportunity_Index) %>%
  dplyr::filter(!is.na(.data$iso3), !duplicated(paste(.data$iso3, .data$tech, .data$supply_chain)))
opp_peers <- sort(unique(c(coalition_members, coalition$iso3[!is.na(coalition$gdp_usd) & coalition$gdp_usd >= dq_cfg$opportunity$peer_min_gdp_usd])))
opportunity <- sp_opportunity_axis(eo_index, supply$cells, opp_peers, dq_cfg$opportunity, outlook = unlist(post_cfg$market_outlook))
legacy_q <- strategic_position_tbl %>%
  dplyr::left_join(opportunity, by = c("iso3", "tech", "supply_chain")) %>%
  sp_decision_quadrant(post_cfg)
strategic_position_tbl <- strategic_position_tbl %>%
  dplyr::left_join(opportunity %>% dplyr::transmute(.data$iso3, .data$tech, .data$supply_chain, .data$legacy_eo_opportunity,
                                                    legacy_eo_opportunity_pct = .data$opportunity_percentile,
                                                    legacy_eo_opportunity_basis = .data$opportunity_basis,
                                                    legacy_eo_opportunity_capped = .data$opportunity_capped),
                   by = c("iso3", "tech", "supply_chain")) %>%
  dplyr::left_join(legacy_q %>% dplyr::transmute(.data$iso3, .data$tech, .data$supply_chain, legacy_quadrant = .data$decision_quadrant,
                                                 legacy_quadrant_posture = .data$strategic_posture, legacy_quadrant_mode = .data$compete_mode,
                                                 legacy_quadrant_flag = .data$quadrant_consistency_flag,
                                                 legacy_quadrant_borderline = .data$quadrant_borderline),
                   by = c("iso3", "tech", "supply_chain"))
rm(legacy_q)

# NEED: supply_chain_need at the producing stages (four families where trusted alternatives are
# observed widely enough); deployment_need at Deployment & Use (scripts/95).
strategic_position_tbl <- sp_supply_chain_need(strategic_position_tbl, dec_cfg)
sn_trusted_coverage <- mean(!is.na(strategic_position_tbl$sn_trusted_alternatives[
  strategic_position_tbl$supply_chain != "Deployment" & !is.na(strategic_position_tbl$strategic_need_percentile)]))
if (!is.null(deployment_need_tbl)) {
  dn <- as.data.frame(deployment_need_tbl, stringsAsFactors = FALSE)
  if (!"deployment_need" %in% names(dn)) stop("deployment_need_tbl.rds has no deployment_need column (scripts/95).")
  if ("supply_chain" %in% names(dn)) dn <- dn[dn$supply_chain %in% "Deployment", , drop = FALSE]
  dn_cols <- setdiff(intersect(names(dn), c("deployment_need", grep("^dn_|^deployment_", names(dn), value = TRUE))), "deployment_stage")
  dn <- dn[!duplicated(dn[, c("iso3", "tech")]), c("iso3", "tech", dn_cols), drop = FALSE]
  names(dn)[-(1:2)] <- paste0(".dn_", names(dn)[-(1:2)])
  strategic_position_tbl <- dplyr::left_join(strategic_position_tbl, dn, by = c("iso3", "tech"))
  for (cl in dn_cols) {
    v <- strategic_position_tbl[[paste0(".dn_", cl)]]
    v[strategic_position_tbl$supply_chain != "Deployment"] <- NA
    strategic_position_tbl[[cl]] <- v
    strategic_position_tbl[[paste0(".dn_", cl)]] <- NULL
  }
} else {
  strategic_position_tbl$deployment_need <- NA_real_
}

# PRIZE: the node prize (55 producing nodes) and, where national, the home market.
io_nodes <- sp_io_node_prize(io_growth, io_momentum, io_maturity, io_cfg)
# the curated maturity of the stage node (country-invariant context; carried from the engine inputs)
mat_class <- strategic_position_tbl %>%
  dplyr::filter(!is.na(.data$maturity)) %>%
  dplyr::count(.data$tech, .data$supply_chain, io_maturity_class = .data$maturity) %>%
  dplyr::arrange(.data$tech, .data$supply_chain, dplyr::desc(.data$n)) %>%
  dplyr::distinct(.data$tech, .data$supply_chain, .keep_all = TRUE) %>%
  dplyr::select(-"n")
if (nrow(mat_class)) io_nodes <- dplyr::left_join(io_nodes %>% dplyr::select(-dplyr::any_of("io_maturity_class")), mat_class, by = c("tech", "supply_chain"))
strategic_position_tbl <- sp_industrial_opportunity(strategic_position_tbl, io_nodes, home_market, io_cfg)

# POSITION among the node's contenders, and the Catch Up pathway.
strategic_position_tbl <- sp_cp_contender_pct(strategic_position_tbl, dec_cfg)
strategic_position_tbl <- sp_pathway(strategic_position_tbl, pipeline_raw, pipeline_covered, input_cost, dec_cfg$pathway, capacity_shares)
# the pathway under the other rules too, kept as columns for the rule comparison (the configured rule decides)
for (alt in list(list(col = "pathway_credible_legs_1", cfg = list(rule = "legs", min_legs = 1)),
                 list(col = "pathway_credible_legs_2", cfg = list(rule = "legs", min_legs = 2)),
                 list(col = "pathway_credible_anchor", cfg = list(rule = "anchor")))) {
  strategic_position_tbl[[alt$col]] <- sp_pathway(strategic_position_tbl, pipeline_raw, pipeline_covered, input_cost,
                                                  utils::modifyList(dec_cfg$pathway, alt$cfg), capacity_shares)$pathway_credible
}

# CHOICE.
strategic_position_tbl <- sp_decision_choice(strategic_position_tbl, dec_cfg, post_cfg)
strategic_position_tbl$choice_reason <- sp_choice_reason(strategic_position_tbl, dec_cfg)

# Layers: Secure is their own; otherwise they follow their cell's choice (they have no axes of their own).
follow <- c("strategic_choice", "strategic_posture", "compete_mode", "deploy_mode", "posture_variant", "compete_strategy",
            "delivery_mode", "posture_confidence", "consistency_flag", "need_type", "supply_chain_need", "deployment_need",
            "industrial_opportunity", "cp_contender_pct", "pathway_credible", "competitive_position_percentile",
            "strategic_need_percentile", "stage_evidence_quality_cp")
layer_secure <- layers_tbl$strategic_posture %in% "secure"
layers_tbl$engine_posture <- layers_tbl$strategic_posture
layers_tbl$engine_variant <- layers_tbl$posture_variant
cell_choice <- strategic_position_tbl[, c("iso3", "tech", "supply_chain", follow)]
names(cell_choice)[-(1:3)] <- paste0(".cell_", follow)
layers_tbl <- dplyr::left_join(layers_tbl, cell_choice, by = c("iso3", "tech", "supply_chain"))
for (cl in follow) {
  cv <- layers_tbl[[paste0(".cell_", cl)]]
  if (!cl %in% names(layers_tbl)) layers_tbl[[cl]] <- cv
  else if (cl %in% c("strategic_choice", "strategic_posture", "compete_mode", "deploy_mode", "posture_variant", "compete_strategy",
                     "delivery_mode", "posture_confidence", "consistency_flag")) {
    layers_tbl[[cl]] <- ifelse(layer_secure, layers_tbl[[cl]], cv)
  } else layers_tbl[[cl]] <- cv
  layers_tbl[[paste0(".cell_", cl)]] <- NULL
}
layers_tbl$strategic_choice[layer_secure] <- "secure"
layers_tbl$compete_mode[layer_secure] <- NA_character_
layers_tbl$deploy_mode[layer_secure] <- NA_character_

# ---- Step 2c: Partner nodes -> trusted partners (AIB), and the allied division of labour -------------
aib_dir <- file.path(outputs_dir, "aib")
aib_files <- file.path(aib_dir, c("aib_inputs.rds", "aib_strategic.rds", "aib_results.rds"))
aib_cells <- NULL
allied <- unique(c(coalition_members, unlist(post_cfg$treat_as_allied %||% character())))
if (all(file.exists(aib_files))) {
  aib_nodes <- readRDS(aib_files[1])$nodes
  aib_st <- readRDS(aib_files[2])
  aib_res <- readRDS(aib_files[3])
  aib_cells <- sp_aib_cell_capability(aib_nodes, aib_st$evidence, aib_res$quality, aib_res$leave_one_out, techs = techs)
  allied <- unique(c(allied, aib_st$allied))
  rm(aib_nodes, aib_res)
} else {
  warning("AIB outputs not found in ", aib_dir, " (scripts/31-33): partner capability falls back to world supply shares everywhere.")
}
capability <- sp_cell_capability(supply$cells, aib_cells)
compat_path <- file.path(processed_dir, "partner_compat_cells_tbl.rds")
fs_path <- file.path(processed_dir, "partner_friendshore_tbl.rds")
if (file.exists(fs_path) && (!file.exists(compat_path) || file.mtime(compat_path) < file.mtime(fs_path))) {
  fs <- readRDS(fs_path)
  fs <- fs[fs$variable == "Friendshore Index", c("reporter_iso", "partner_iso", "tech", "supply_chain", "value")]
  names(fs)[5] <- "friendshore"
  saveRDS(fs, compat_path)
  rm(fs)
  invisible(gc())
}
compat <- if (file.exists(compat_path)) readRDS(compat_path) else data.frame(reporter_iso = character(), partner_iso = character(),
                                                                             tech = character(), supply_chain = character(), friendshore = numeric())
dyad_path <- file.path(raw_data_path, "comtrade_energy_dyads_all.csv")
imp_path <- file.path(processed_dir, "import_partner_cells_tbl.rds")
if (file.exists(dyad_path) && (!file.exists(imp_path) || file.mtime(imp_path) < file.mtime(dyad_path))) {
  d <- data.table::fread(dyad_path, select = c("period", "reporter_iso", "flow_code", "partner_iso", "cmd_code", "primary_value"),
                         colClasses = list(character = "cmd_code"))
  d <- d[d$flow_code == "M" & d$period == max(d$period) & !is.na(d$partner_iso) & !d$partner_iso %in% c("W00", ""), ]
  d$code6 <- stringr::str_pad(d$cmd_code, 6, side = "left", pad = "0")
  codes6 <- unique(trade_codes[, c("tech", "supply_chain", "code6")])
  codes6$tech[codes6$tech == "Natural Gas"] <- "Gas"
  imp <- dplyr::inner_join(as.data.frame(d[, c("reporter_iso", "partner_iso", "code6", "primary_value")]), codes6, by = "code6",
                           relationship = "many-to-many") %>%
    dplyr::group_by(.data$reporter_iso, .data$partner_iso, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(imports_usd = sum(.data$primary_value, na.rm = TRUE), .groups = "drop")
  attr(imp, "trade_year") <- max(d$period)
  saveRDS(imp, imp_path)
  rm(d)
  invisible(gc())
}
import_partners <- if (file.exists(imp_path)) readRDS(imp_path) else NULL
# Partner recommendations for Compete . Partner cells and for Secure cells whose mitigation relies on
# trusted foreign capacity (delivery allied, mixed or diversified_global).
partner_cells <- strategic_position_tbl %>%
  dplyr::filter(.data$compete_mode %in% "partner" |
                  (.data$strategic_posture %in% "secure" & .data$delivery_mode %in% c("allied", "mixed", "diversified_global"))) %>%
  dplyr::select("iso3", "tech", "supply_chain", "coercive_top_share", "strategic_posture")
partner_recs <- sp_partner_recs(
  partner_cells, capability, coercive_pairs, compat,
  strategic_position_tbl %>% dplyr::select("iso3", "tech", "supply_chain", "competitive_position_percentile"),
  imports = import_partners, aib_cells = aib_cells$cells, members = allied, cfg = post_cfg$partner_recs
)
if (nrow(partner_recs)) {
  partner_recs$partner_rec_for <- ifelse(
    paste(partner_recs$iso3, partner_recs$tech, partner_recs$supply_chain) %in%
      paste(partner_cells$iso3, partner_cells$tech, partner_cells$supply_chain)[partner_cells$strategic_posture %in% "secure"],
    "secure", "partner")
  strategic_position_tbl <- dplyr::left_join(strategic_position_tbl, partner_recs, by = c("iso3", "tech", "supply_chain"))
}
demand <- strategic_position_tbl %>%
  dplyr::group_by(.data$tech, .data$supply_chain) %>%
  dplyr::summarise(partner_demand = sum(.data$compete_mode %in% "partner"), secure_demand = sum(.data$strategic_posture %in% "secure"),
                   compete_demand = sum(.data$strategic_posture %in% "compete"),
                   partner_gap_countries = sum(.data$partner_gap %in% TRUE & .data$compete_mode %in% "partner"),
                   partner_demand_members = sum(.data$compete_mode %in% "partner" & .data$iso3 %in% allied),
                   secure_foreign_demand = sum(.data$strategic_posture %in% "secure" & .data$delivery_mode %in% c("allied", "mixed", "diversified_global")),
                   partner_aib_gaps = sum(.data$partner_aib_status %in% "gap"), .groups = "drop")
aib_division <- sp_aib_division(capability, allied, unlist(post_cfg$coercive_powers), demand, aib_cells$cells, post_cfg$partner_recs) %>%
  dplyr::filter(.data$tech %in% techs)

# ---- Steps 3-4: policy design and policy alignment ---------------------------------------------------
has_record <- dplyr::coalesce(strategic_position_tbl$pams_stock, 0) + dplyr::coalesce(strategic_position_tbl$nipo_all_stock, 0) > 0
# A Watch deployment over-prioritized by localisation: local-content rules sit on the equipment's HS codes,
# so the Deployment row reads the technology's Assembly protection priority.
asm_pro <- strategic_position_tbl %>% dplyr::filter(.data$supply_chain == "Assembly") %>%
  dplyr::select("iso3", "tech", deployment_localisation_priority = "policy_protection_priority")
strategic_position_tbl <- strategic_position_tbl %>%
  dplyr::select(-dplyr::any_of("deployment_localisation_priority")) %>%
  dplyr::left_join(asm_pro, by = c("iso3", "tech")) %>%
  dplyr::mutate(deployment_localisation_priority = ifelse(.data$supply_chain == "Deployment", .data$deployment_localisation_priority, NA_real_))
strategic_position_tbl$policy_alignment <- sp_policy_alignment(strategic_position_tbl, sp_cfg$policy_alignment, has_record = has_record)
strategic_position_tbl$policy_alignment_flag <- sp_policy_alignment_flag(strategic_position_tbl, sp_cfg$policy_alignment, has_record = has_record)
strategic_position_tbl$policy_alignment_confidence <- sp_policy_alignment_confidence(
  strategic_position_tbl, strategic_position_tbl$policy_alignment, sp_cfg$policy_alignment, has_record = has_record)
strategic_position_tbl <- sp_policy_design(strategic_position_tbl, design_tables$design, design_tables$delivery)
layers_tbl <- sp_policy_design(layers_tbl, design_tables$design, design_tables$delivery)

stopifnot(!anyDuplicated(strategic_position_tbl[, c("Country", "tech", "supply_chain")]))
assert_supply_chain_stages(strategic_position_tbl$supply_chain, label = "strategic_position_tbl")

# Compatibility view: the three-stage roll-up of the headline columns.
strategic_position_legacy_tbl <- sp_legacy_rollup(
  strategic_position_tbl,
  mean_cols = c("competitive_position_percentile", "strategic_need_percentile", "policy_priority_percentile",
                "evidence_specificity_cp", "evidence_specificity_sn", "evidence_specificity_pp"),
  sum_cols = intersect(c("policy_support_stock", "policy_protection_stock", "pams_stock", "nipo_all_stock"), names(strategic_position_tbl))
)

saveRDS(strategic_position_tbl, file.path(outputs_dir, "strategic_position_tbl.rds"))
utils::write.csv(strategic_position_tbl, file.path(outputs_dir, "strategic_position_tbl.csv"), row.names = FALSE, na = "")
saveRDS(aib_division, file.path(outputs_dir, "strategic_aib_division_tbl.rds"))
utils::write.csv(aib_division, file.path(outputs_dir, "strategic_aib_division_tbl.csv"), row.names = FALSE, na = "")
saveRDS(list(benchmark = trade_bench, momentum_cut = tm_cut, momentum_reporters = tm$reporters, momentum_floors = tm$floors,
             capability = capability, aib_cells = aib_cells$cells, partner_recs = partner_recs, legacy_opportunity = opportunity,
             io_nodes = io_nodes, io_growth = io_growth, io_momentum = io_momentum, pipeline = pipeline_raw),
        file.path(outputs_dir, "strategic_position_trade_partner_inputs.rds"))
saveRDS(layers_tbl, file.path(outputs_dir, "strategic_posture_layers_tbl.rds"))
saveRDS(
  list(cp_inputs = cp$inputs, cp_variable_coverage = cp$variable_coverage, sn_inputs = sn$inputs, pp_inputs = pp$inputs,
       pams_links = pams_links, posture_inputs = posture_inputs, supply = supply, premium = premium,
       coercive_pairs = coercive_pairs, legacy_rollup = strategic_position_legacy_tbl),
  file.path(outputs_dir, "strategic_position_inputs.rds")
)

# ---- Diagnostics -------------------------------------------------------------------------------------
wcsv <- function(x, f) utils::write.csv(x, file.path(diagnostics_dir, f), row.names = FALSE, na = "")
review <- unlist(sp_cfg$review_countries)
tbl <- strategic_position_tbl

# Evidence coverage by index, technology, stage and country.
ev_long <- dplyr::bind_rows(
  ev_cp %>% dplyr::rename_with(~ sub("_cp$", "", .x)) %>% dplyr::mutate(index = "competitive_position"),
  ev_sn %>% dplyr::rename_with(~ sub("_sn$", "", .x)) %>% dplyr::mutate(index = "strategic_need"),
  ev_pp %>% dplyr::rename_with(~ sub("_pp$", "", .x)) %>% dplyr::mutate(index = "policy_priority") %>%
    dplyr::left_join(countries, by = "iso3")
)
for (by in list(c("index", "tech"), c("index", "supply_chain"), c("index", "Country"))) {
  wcsv(vse_coverage_summary(ev_long, by = by), paste0("evidence_coverage_by_", by[2], ".csv"))
}
wcsv(vse_coverage_summary(ev_long, by = "index"), "evidence_coverage_by_index.csv")

# Validation questions 1-12 of the redesign brief.
wcsv(sp_diag_specialists(tbl, top_n = 5), "q1_specialists_by_cell.csv")
wcsv(dplyr::bind_rows(lapply(c("cp", "sn", "pp"), function(d) dplyr::mutate(sp_diag_weak_evidence(tbl, d), dimension = d))), "q3_weak_evidence_cells.csv")
wcsv(sp_diag_legacy_priorities(tbl, top_k = 5), "q5_legacy_si_priorities.csv")
diag_cols <- c("Country", "tech", "supply_chain", "competitive_position_percentile", "cp_within_country_pct",
               "strategic_need_percentile", "policy_priority_percentile", "policy_priority_sources", "priority_gap",
               "evidence_specificity_cp", "competitive_policy_alignment", "strategic_posture", "posture_variant",
               "policy_alignment")
wcsv(sp_diag_latent_advantages(tbl) %>% dplyr::select(dplyr::any_of(diag_cols)), "q6_latent_advantages.csv")
wcsv(sp_diag_robust_low_priority(tbl) %>% dplyr::select(dplyr::any_of(diag_cols)), "q7_robust_low_priority.csv")
agree <- dplyr::bind_rows(
  sp_diag_policy_agreement(tbl, "policy_support_priority", "policy_protection_priority", "q8 NIPO support vs protection"),
  sp_diag_policy_agreement(tbl, "nipo_priority", "pams_priority", "q9 NIPO vs PAMS"),
  sp_diag_policy_agreement(tbl, "nipo_priority", "nipo_priority_iw", "q11 NIPO count vs instrument-weighted"),
  sp_diag_policy_agreement(tbl, "policy_momentum_1y", "policy_momentum_3y", "q12 momentum 1y vs 3y"),
  sp_diag_policy_agreement(tbl, "policy_momentum_3y", "policy_momentum_5y", "q12 momentum 3y vs 5y"),
  sp_diag_policy_agreement(tbl, "policy_priority_percentile", "policy_momentum", "stock vs 3y flow")
)
wcsv(agree, "q8_q9_q11_q12_policy_agreement.csv")
wcsv(sp_diag_policy_sources(tbl), "q9_policy_priority_sources.csv")
wcsv(sp_diag_gap_variants(tbl), "priority_gap_variants.csv")
wcsv(sp_diag_posture_mix(tbl), "competitive_policy_alignment_mix.csv")

# Legacy Strategic Index: the stage and technology priors (Q4) and CAT (Q10).
lsi_cfg <- sp_cfg$legacy_si_sensitivity
si_legacy <- index_outputs$strategic_index
allies <- index_outputs$coalition_members %>%
  dplyr::transmute(Country = .data$country, .data$iso3, .data$tier, .data$network_node)
lsi_long <- sp_si_stage_scenarios(si_legacy, sp_cfg$stage_scenarios, match_stored_tol = lsi_cfg$match_stored_tol)
lsi_cmp <- sp_si_scenario_comparison(lsi_long, baseline = lsi_cfg$baseline, top_n = lsi_cfg$top_n)
lsi_picks <- sp_si_pick_comparison(lsi_long, allies, baseline = lsi_cfg$baseline, near_margin = lsi_cfg$near_margin,
                                   focus_techs = unlist(lsi_cfg$focus_techs))
prov <- index_outputs$policy_index_provenance
lsi_cat <- sp_cat_rank_effect(si_legacy, index_outputs$policy_component_tbl, techs = prov$techs,
                              component_weights = prov$weights, required_components = prov$required_components)
lsi_cat_cmp <- sp_si_scenario_comparison(lsi_cat$long, baseline = "with_cat")
lsi_cat_picks <- sp_si_pick_comparison(lsi_cat$long, allies, baseline = "with_cat")
stopifnot(identical(lsi_cat$check$max_abs_pol_diff, 0))
wcsv(lsi_cmp$summary, "q4_legacy_si_scenario_summary.csv"); wcsv(lsi_cmp$country, "q4_legacy_si_scenario_country.csv")
wcsv(lsi_cmp$stage, "q4_legacy_si_scenario_top_stage.csv"); wcsv(lsi_cmp$top_cell, "q4_legacy_si_scenario_top_cell.csv")
wcsv(lsi_picks$picks, "q4_legacy_si_pick_scenarios.csv"); wcsv(lsi_picks$summary, "q4_legacy_si_pick_summary.csv")
wcsv(lsi_picks$by_tech, "q4_legacy_si_pick_by_tech.csv"); wcsv(lsi_picks$by_stage, "q4_legacy_si_pick_by_stage.csv")
wcsv(lsi_picks$focus, "q4_legacy_si_pick_focus.csv"); wcsv(lsi_picks$transitions, "q4_legacy_si_pick_transitions.csv")
wcsv(lsi_cat$country, "q10_cat_country.csv"); wcsv(lsi_cat_cmp$cell, "q10_cat_cell.csv")
wcsv(lsi_cat_picks$picks, "q10_cat_picks.csv"); wcsv(lsi_cat_picks$summary, "q10_cat_pick_summary.csv")

# Strategic posture.
wcsv(sp_diag_posture_distribution(tbl, "Country"), "posture_by_country.csv")
wcsv(sp_diag_posture_distribution(tbl, "tech"), "posture_by_tech.csv")
wcsv(sp_diag_posture_distribution(tbl, "supply_chain"), "posture_by_stage.csv")
wcsv(sp_diag_posture_distribution(tbl %>% dplyr::filter(.data$Country %in% review), c("Country", "tech")), "posture_review_country_tech.csv")
wcsv(tbl %>% dplyr::count(.data$strategic_posture, .data$posture_variant, .data$posture_confidence, name = "n_cells"), "posture_variants.csv")
wcsv(tbl %>% dplyr::count(.data$strategic_posture, .data$policy_alignment, name = "n_cells"), "posture_policy_alignment.csv")
wcsv(sp_diag_country_divergence(tbl, review), "posture_country_divergence.csv")
wcsv(sp_diag_layer_precision(tbl, layers_tbl), "posture_layer_precision.csv")
wcsv(sp_diag_policy_misalignment(tbl %>% dplyr::filter(.data$Country %in% review)) %>%
       dplyr::select("Country", "tech", "supply_chain", "strategic_posture", "posture_variant", "policy_alignment",
                     "policy_priority_percentile", "nipo_priority", "pams_priority", "policy_support_priority",
                     "policy_protection_priority", "competitive_position_percentile", "strategic_need_percentile"),
     "posture_policy_misalignment_review.csv")
wcsv(tbl %>% dplyr::filter(.data$strategic_posture %in% "secure") %>%
       dplyr::select("Country", "tech", "supply_chain", "posture_variant", "posture_confidence", "t2_path", "coercive_top_iso3",
                     "coercive_top_share", "coercive_import_share", "home_share", "trusted_world_share", "delivery_mode",
                     "policy_alignment") %>% dplyr::arrange(.data$tech, .data$supply_chain, .data$Country),
     "posture_secure_cells.csv")
wcsv(layers_tbl %>% dplyr::filter(.data$Country %in% review) %>%
       dplyr::select("Country", "tech", "supply_chain", "layer_id", "layer_type", "strategic_posture", "posture_variant",
                     "posture_confidence", "t1_consequence", "t2_vulnerability", "t2_path", "coercive_top_iso3",
                     "coercive_top_share", "coercive_import_share", "delivery_mode", "posture_possible", "pivotal_inputs"),
     "posture_layers_review.csv")
wcsv(tbl %>% dplyr::filter(.data$Country %in% review) %>%
       dplyr::select("Country", "tech", "supply_chain", "strategic_posture", "posture_variant", "posture_confidence",
                     "evidence_completeness", "posture_possible", "pivotal_inputs", "posture_primary_rationale",
                     "posture_secondary_rationale", "compete_strategy", "delivery_mode", "t1_consequence", "t2_vulnerability",
                     "t2_path", "t3_capability", "t4_strategy", "t5_proportionality", "home_share", "coercive_top_iso3",
                     "coercive_top_share", "coercive_import_share", "trusted_world_share", "competitive_position_percentile",
                     "cp_within_country_pct", "strategic_need_percentile", "policy_priority_percentile",
                     "competitive_policy_alignment", "policy_alignment", "sensitive_layers_secure", "strategic_index_legacy"),
     "posture_review_cells.csv")
wcsv(dplyr::bind_rows(lapply(review, function(cn) {
  sub <- tbl %>% dplyr::filter(.data$Country == cn, !is.na(.data$strategic_index_legacy)) %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index_legacy)) %>% utils::head(5)
  dplyr::select(sub, "Country", "tech", "supply_chain", "strategic_index_legacy", "strategic_posture", "posture_variant",
                "policy_alignment", "competitive_policy_alignment")
})), "posture_vs_legacy_si_top5.csv")

# Trade refresh: the cutoff, coverage, availability, small-base floors, movers, weights and prices.
if (!is.null(tm)) {
  wcsv(tm_cut$coverage %>% dplyr::mutate(chosen = .data$cutoff == tm_cut$cutoff, min_share = tm_cut$min_share), "trade_momentum_cutoff.csv")
  wcsv(tm$reporters, "trade_momentum_reporters.csv")
  wcsv(sp_diag_momentum_availability(tbl, c(review, "China", "Canada", "France")), "trade_momentum_availability.csv")
  wcsv(tm$floors %>% dplyr::left_join(tm$cells %>% dplyr::group_by(.data$tech, .data$supply_chain) %>%
                                        dplyr::summarise(n_small_base = sum(.data$trade_momentum_quality == "small_base"),
                                                         n_valid = sum(!is.na(.data$trade_momentum_score)), .groups = "drop"),
                                      by = c("tech", "supply_chain")), "trade_momentum_small_base.csv")
  wcsv(sp_diag_momentum_movers(cp_structural, cp$cells, tm$cells, country_name_to_iso3), "trade_momentum_movers.csv")
  # the canonical choice re-run on each weight's Competitive Position (stored need, prize, pathway, floor)
  choice_rerun <- function(bl) {
    alt <- tbl %>%
      dplyr::select(-"competitive_position_percentile", -"competitive_position_score") %>%
      dplyr::left_join(bl %>% dplyr::select("Country", "tech", "supply_chain", "competitive_position_percentile", "competitive_position_score"),
                       by = c("Country", "tech", "supply_chain")) %>%
      sp_cp_contender_pct(dec_cfg)
    th <- sp_choice_thresholds(dec_cfg)
    dep <- alt$supply_chain == "Deployment"
    hc <- sp_choice_high_cp(alt$cp_contender_pct, alt$cp_contender_basis, alt$cp_is_contender, alt$t3_capability_lo, th$cp, th$floor)
    v <- sp_choice_rule(alt$supply_chain, alt$engine_posture %in% "secure", ifelse(dep, NA_real_, alt$supply_chain_need),
                        ifelse(dep, NA_real_, alt$industrial_opportunity), hc, alt$pathway_credible, ifelse(dep, alt$deployment_need, NA_real_), th)
    v[is.na(tbl$strategic_choice)] <- NA_character_
    flag <- ifelse(v %in% "preserve" & !(alt$industrial_opportunity >= th$prize) & alt$supply_chain_need >= th$need, "review", NA_character_)
    c(sum(!mapply(identical, v, tbl$strategic_choice)),
      sum(!mapply(identical, flag, ifelse(tbl$consistency_flag %in% "review_strong_position_low_prize", "review", NA_character_))), NA)
  }
  wcsv(sp_diag_momentum_weights(cp_structural, tm$cells, country_name_to_iso3, cp_families, c(0, unlist(tm_cfg$weight_sensitivity)),
                                tm_cfg$momentum_weight, tbl = tbl, cfg = post_cfg, rerun_fn = choice_rerun), "trade_momentum_weight_sensitivity.csv")
  wcsv(sp_diag_momentum_prices(tm$cells), "trade_momentum_prices.csv")
}
# Secure routes, the canonical choice, alignment and partners; old vs new against a baseline table if given.
sec_countries <- c(review, "China", "Canada", "France")
th <- sp_choice_thresholds(dec_cfg)
wcsv(sp_diag_choice_distribution(tbl, "Country"), "choice_by_country.csv")
wcsv(sp_diag_choice_distribution(tbl, "tech"), "choice_by_tech.csv")
wcsv(sp_diag_choice_distribution(tbl, "supply_chain"), "choice_by_stage.csv")
wcsv(sp_diag_choice_distribution(tbl, character()), "choice_overall.csv")
wcsv(sp_diag_choice_examples(tbl, sec_countries), "choice_examples.csv")
wcsv(sp_choice_threshold_sensitivity(tbl, dec_cfg, dec_cfg$thresholds_sensitivity), "choice_threshold_sensitivity.csv")
wcsv(sp_diag_consistency_before_after(tbl), "consistency_before_after.csv")
wcsv(sp_diag_consistency_cells(tbl), "consistency_cells.csv")
wcsv(sp_diag_quadrant_vs_choice(tbl, dq_cfg$need_threshold, dq_cfg$opportunity_threshold), "legacy_quadrant_vs_choice.csv")
wcsv(sp_diag_pathway_summary(tbl), "pathway_by_stage.csv")
pathway_alts <- list(legs_1 = tbl$pathway_credible_legs_1, legs_2 = tbl$pathway_credible_legs_2, anchor = tbl$pathway_credible_anchor)
wcsv(sp_diag_pathway_rules(tbl, pathway_alts, dec_cfg), "catch_up_rule_comparison.csv")
wcsv(sp_diag_catch_up_capability(tbl, pathway_alts, dec_cfg), "catch_up_capability_bands.csv")
wcsv(sp_diag_anchor_types(tbl), "catch_up_anchor_types.csv")
wcsv(io_nodes, "io_node_prize.csv")
io_val <- sp_io_validation(tbl, momentum_weight = io_cfg$investment_momentum$weight %||% 0.25)
wcsv(io_val$correlations, "opportunity_validation_correlations.csv")
wcsv(io_val$weights, "opportunity_validation_weights.csv")
wcsv(io_val$movers, "opportunity_validation_movers_vs_legacy_eo.csv")
wcsv(io_val$disguise, "opportunity_validation_by_node.csv")
wcsv(data.frame(measure = c("trusted_alternatives coverage of assessed producing cells", "trusted_alternatives in the need score",
                            "home-market rows read", "home-market cells scored (national)", "deployment-need rows read",
                            "Deployment cells with deployment_need"),
                value = c(round(sn_trusted_coverage, 3), as.numeric(isTRUE(tbl$sn_trusted_in_score[1])),
                          if (is.null(home_market)) 0 else nrow(home_market), sum(tbl$io_home_market_scored %in% TRUE),
                          if (is.null(deployment_need_tbl)) 0 else nrow(deployment_need_tbl),
                          sum(tbl$supply_chain == "Deployment" & !is.na(tbl$deployment_need)))), "choice_input_coverage.csv")
sel <- sp_diag_secure_selectivity(tbl, th$need)
wcsv(sel$summary, "secure_selectivity.csv")
wcsv(sp_diag_watch_policy(tbl), "watch_high_policy_intensity.csv")
wcsv(sp_diag_deployment_review(tbl, unlist(sp_cfg$deployment_review_countries), unlist(sp_cfg$deployment_review_techs)), "deployment_review.csv")
wcsv(sp_diag_manufacturing_vs_deployment(tbl), "manufacturing_vs_deployment.csv")
wcsv(tbl %>% dplyr::count(.data$strategic_choice, .data$policy_alignment, .data$policy_alignment_confidence, name = "n_cells"),
     "choice_policy_alignment.csv")
# The legacy need x EO quadrant (the rule until 2026-10-28), described on its own terms.
legacy_view <- sp_diag_legacy_view(tbl)
wcsv(sp_diag_quadrant_distribution(legacy_view, "Country") %>% dplyr::filter(.data$Country %in% sec_countries), "legacy_quadrant_by_country_review.csv")
wcsv(sp_diag_quadrant_distribution(legacy_view, "supply_chain"), "legacy_quadrant_by_stage.csv")
wcsv(sp_diag_quadrant_thresholds(legacy_view, unlist(dq_cfg$thresholds_sensitivity), dq_cfg$need_threshold, dq_cfg$opportunity_threshold,
                                 unlist(dq_cfg$apply_stages)), "legacy_quadrant_threshold_sensitivity.csv")
rm(legacy_view)
wcsv(sp_diag_alignment_summary(tbl %>% dplyr::mutate(decision_quadrant = .data$deploy_mode)), "policy_alignment_by_mode.csv")
wcsv(sp_diag_partner_sanity(tbl, coercive_pairs, allied), "partner_recs_sanity.csv")
wcsv(tbl %>% dplyr::filter(!is.na(.data$partner_rec_for), .data$Country %in% sec_countries) %>%
       dplyr::select("Country", "tech", "supply_chain", "strategic_choice", "partner_rec_for", dplyr::starts_with("recommended_partner_"),
                     "partner_n_credible", "partner_gap", "partner_aib_status", "partner_gap_reasons", "partner_trusted_capacity",
                     "partner_capability_source", "partner_evidence_quality", "partner_reason"), "partner_recs_review.csv")
wcsv(tbl %>% dplyr::filter(!is.na(.data$partner_rec_for)) %>% dplyr::count(.data$partner_rec_for, .data$partner_aib_status, name = "n_cells"),
     "partner_aib_status.csv")
wcsv(aib_division, "aib_division_matrix.csv")
wcsv(tbl %>% dplyr::filter(.data$strategic_posture %in% "secure") %>%
       dplyr::count(.data$tech, .data$supply_chain, .data$security_rationale, .data$posture_variant, name = "n_cells"), "secure_by_rationale.csv")
baseline_path <- Sys.getenv("SP_BASELINE_TBL", "")
if (nzchar(baseline_path) && file.exists(baseline_path)) {
  old_tbl <- readRDS(baseline_path)
  wcsv(sp_diag_secure_compare(old_tbl, tbl, "Country", sec_countries), "secure_old_vs_new_by_country.csv")
  wcsv(sp_diag_secure_compare(old_tbl, tbl, "tech"), "secure_old_vs_new_by_tech.csv")
  wcsv(sp_diag_secure_compare(old_tbl, tbl, "supply_chain"), "secure_old_vs_new_by_stage.csv")
  wcsv(sp_diag_secure_compare(old_tbl, tbl, c("Country", "tech", "supply_chain"), sec_countries) %>%
         dplyr::filter(.data$secure_old > 0 | .data$secure_new > 0), "secure_old_vs_new_review_cells.csv")
  key <- c("iso3", "tech", "supply_chain")
  wcsv(dplyr::inner_join(old_tbl %>% dplyr::select(dplyr::all_of(key), old_posture = "strategic_posture", old_mode = dplyr::any_of("compete_mode")),
                         tbl %>% dplyr::select(dplyr::all_of(key), "strategic_choice"), by = key) %>%
         dplyr::count(.data$old_posture, .data$old_mode, .data$strategic_choice, name = "n_cells"), "posture_old_vs_new_transitions.csv")
}

# Threshold sensitivity of the Secure engine: the share of classified nodes whose posture moves when one
# engine rule moves (the choice thresholds are in choice_threshold_sensitivity.csv). Outside Secure, a
# node takes the canonical choice it has on the stored need, prize, position and pathway.
set_rule <- function(path, value) function(cfg) { cfg[[path[1]]][[path[2]]] <- value; cfg }
set_top <- function(name, value) function(cfg) { cfg[[name]] <- value; cfg }
perturb <- list(
  coercion_0.4 = set_rule(c("coercion", "threshold"), 0.4), coercion_0.6 = set_rule(c("coercion", "threshold"), 0.6),
  coercive_imports_0.15 = set_rule(c("coercive_import_share", "threshold"), 0.15),
  coercive_imports_0.35 = set_rule(c("coercive_import_share", "threshold"), 0.35),
  position_strong_0.05 = set_rule(c("position", "strong"), 0.05), position_strong_0.15 = set_rule(c("position", "strong"), 0.15),
  position_moderate_0.05 = set_rule(c("position", "moderate"), 0.05),
  allied_alternative_0.4 = set_top("allied_alternative_share", 0.4), allied_alternative_0.6 = set_top("allied_alternative_share", 0.6),
  acute_includes_moderate = set_top("acute_consequence", c("moderate", "high", "severe")),
  acute_severe_only = set_top("acute_consequence", "severe"),
  dual_use_imports_0.15 = set_rule(c("security_routes", "dual_use_coercive_import_share"), list(threshold = 0.15, marginal = 0.1)),
  dual_use_imports_0.35 = set_rule(c("security_routes", "dual_use_coercive_import_share"), list(threshold = 0.35, marginal = 0.25)),
  resilience_alternatives_0.4 = function(cfg) { cfg$security_routes$resilience$trusted_alternative_max <- 0.4; cfg },
  resilience_alternatives_0.6 = function(cfg) { cfg$security_routes$resilience$trusted_alternative_max <- 0.6; cfg }
)
cells_in <- posture_inputs %>% dplyr::filter(.data$layer_id == "stage")
dep_tbl <- tbl$supply_chain == "Deployment"
nonsecure_choice <- sp_choice_rule(
  tbl$supply_chain, FALSE, ifelse(dep_tbl, NA_real_, tbl$supply_chain_need), ifelse(dep_tbl, NA_real_, tbl$industrial_opportunity),
  sp_choice_high_cp(tbl$cp_contender_pct, tbl$cp_contender_basis, tbl$cp_is_contender, tbl$t3_capability_lo, th$cp, th$floor),
  tbl$pathway_credible, ifelse(dep_tbl, tbl$deployment_need, NA_real_), th)
nonsecure_posture <- stats::setNames(unname(SP_CHOICE_VERB_POSTURE[nonsecure_choice]), paste(tbl$iso3, tbl$tech, tbl$supply_chain))
strict <- function(x, cfg) {
  p <- sp_posture_strict(x, cfg)
  data.frame(strategic_posture = ifelse(is.na(p), NA_character_, ifelse(p == "secure", "secure",
                                        unname(nonsecure_posture[paste(x$iso3, x$tech, x$supply_chain)]))))
}
sens <- sp_diag_threshold_sensitivity(cells_in, post_cfg, perturb, classify = strict)
# The bloc rule: only the coercive powers count as coercive.
pairs_powers <- sp_coercive_pairs(geo_alignment, unlist(post_cfg$coercive_powers), opposite_bloc = FALSE) %>%
  sp_nonaligned_trust(post_cfg$nonaligned_trust %||% "bloc")
supply_powers <- sp_supply_structure(major_supplier_shares, pairs_powers, post_cfg$mineral_min_demand_share,
                                     measure_priority = measure_priority, techs = techs)
cells_powers <- sp_posture_inputs(
  nodes %>% dplyr::filter(.data$layer_id == "stage"), supply_powers$cells, import_exposure, cp$cells, countries, post_cfg,
  supply_minerals = supply_powers$minerals, supply_measures = supply_powers$measures, feas = feas, premium = premium,
  context = posture_context, exclude_iso3 = unlist(sp_cfg$strategic_need$exclude_iso3), tech_ghg = tech_ghg
) %>%
  sp_attach_security_rules(sr_rules)
p_base <- strict(cells_in, post_cfg)$strategic_posture
p_pow <- strict(cells_powers, post_cfg)$strategic_posture[match(paste(cells_in$iso3, cells_in$tech, cells_in$supply_chain),
                                                                paste(cells_powers$iso3, cells_powers$tech, cells_powers$supply_chain))]
ok <- !is.na(p_base) & !is.na(p_pow)
sens <- dplyr::bind_rows(sens, data.frame(
  perturbation = "coercive_powers_only (world shares; import shares unchanged)", n_nodes = sum(ok),
  share_changed = mean(p_base[ok] != p_pow[ok]), to_secure = sum(ok & p_pow != p_base & p_pow == "secure"),
  to_compete = sum(ok & p_pow != p_base & p_pow == "compete"), to_deploy = sum(ok & p_pow != p_base & p_pow == "deploy")
))
wcsv(sens, "posture_threshold_sensitivity.csv")
# Files of the retired quadrant diagnostics (now legacy_quadrant_*.csv and choice_*.csv).
unlink(file.path(diagnostics_dir, c("quadrant_by_country_review.csv", "quadrant_by_tech.csv", "quadrant_by_stage.csv",
                                    "quadrant_examples.csv", "quadrant_threshold_sensitivity.csv", "quadrant_consistency_flags.csv")))

message(
  "Strategic position: ", format(nrow(tbl), big.mark = ","), " cells, ", nrow(countries), " countries; positions ",
  sum(!is.na(tbl$competitive_position_percentile)), ", need ", sum(!is.na(tbl$strategic_need_percentile)),
  ", policy priority ", sum(!is.na(tbl$policy_priority_percentile)), ". Choices: ",
  paste(names(table(tbl$strategic_choice)), table(tbl$strategic_choice), sep = " ", collapse = ", "),
  "; ", nrow(layers_tbl), " layer rows (", sum(layers_tbl$strategic_posture %in% "secure"), " secure)."
)
