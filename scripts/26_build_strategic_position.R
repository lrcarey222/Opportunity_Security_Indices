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


# Strategic position framework and its Yards & Fences decision layer (docs/strategic_position.md):
#   Step 1 Strategic Position: Competitive Position, Strategic Need and Revealed Policy Priority per
#          country x technology x six-stage cell, with the evidence behind each;
#   Step 2 Strategic Posture: Secure / Compete / Deploy from the five decision tests (observed policy
#          never enters them), for each cell and each sensitive layer;
#   Step 3 Policy Design: fence, gate and build for the posture and delivery mode;
#   Step 4 Evaluation: recommended posture vs observed policy, benchmarks and exit conditions.
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
  "R/strategic_position/sp_diagnostics.R"
)) source(file.path(repo_root, f))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
sp_cfg <- yaml::read_yaml(Sys.getenv("OPSI_STRATEGIC_POSITION", file.path(repo_root, "config", "strategic_position.yml")))
post_cfg <- sp_cfg$strategic_posture

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

# ---- Step 1: the three dimensions ------------------------------------------------------------------
cp <- sp_competitive_position(eo_contrib, es_contrib, sp_cfg$competitive_position, techs = techs)
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
posture <- sp_strategic_posture(posture_inputs, post_cfg)
# imports_usd and import_hhi stay as Strategic Need reports them on the cell table; layers carry the
# posture's own (which differ at Deployment).
carry <- c("layer_label", "critical_function", "home_share", "home_status", "coercive_top_iso3", "coercive_top_share",
           "coercive_world_share", "trusted_world_share", "coercive_import_share", "complementary",
           "adjacent_established", "resource_base", "market_pct", "premium_pct", "exposed", "supply_basis",
           "attr_confidence", "attr_provenance")
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

# ---- Steps 3-4: policy design and policy alignment ---------------------------------------------------
has_record <- dplyr::coalesce(strategic_position_tbl$pams_stock, 0) + dplyr::coalesce(strategic_position_tbl$nipo_all_stock, 0) > 0
strategic_position_tbl$policy_alignment <- sp_policy_alignment(strategic_position_tbl, sp_cfg$policy_alignment, has_record = has_record)
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

# Threshold sensitivity: the share of classified nodes whose posture moves when one rule moves.
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
  catch_up_market_0.5 = set_top("catch_up_market_pct", 0.5)
)
cells_in <- posture_inputs %>% dplyr::filter(.data$layer_id == "stage")
strict <- function(x, cfg) data.frame(strategic_posture = sp_posture_strict(x, cfg))
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
)
p_base <- sp_posture_strict(cells_in, post_cfg)
p_pow <- sp_posture_strict(cells_powers, post_cfg)
ok <- !is.na(p_base) & !is.na(p_pow)
sens <- dplyr::bind_rows(sens, data.frame(
  perturbation = "coercive_powers_only (world shares; import shares unchanged)", n_nodes = sum(ok),
  share_changed = mean(p_base[ok] != p_pow[ok]), to_secure = sum(ok & p_pow != p_base & p_pow == "secure"),
  to_compete = sum(ok & p_pow != p_base & p_pow == "compete"), to_deploy = sum(ok & p_pow != p_base & p_pow == "deploy")
))
wcsv(sens, "posture_threshold_sensitivity.csv")

message(
  "Strategic position: ", format(nrow(tbl), big.mark = ","), " cells, ", nrow(countries), " countries; positions ",
  sum(!is.na(tbl$competitive_position_percentile)), ", need ", sum(!is.na(tbl$strategic_need_percentile)),
  ", policy priority ", sum(!is.na(tbl$policy_priority_percentile)), ". Postures: ",
  paste(names(table(tbl$strategic_posture)), table(tbl$strategic_posture), sep = " ", collapse = ", "),
  "; ", nrow(layers_tbl), " layer rows (", sum(layers_tbl$strategic_posture %in% "secure"), " secure)."
)
