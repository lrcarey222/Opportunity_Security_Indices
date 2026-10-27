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

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "categories", "policy", "iea_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))
source(file.path(repo_root, "R", "indices", "build_policy_index.R"))

# Always reload runtime configs from repository YAML so this script is not
# affected by option overrides from tests/interactive sessions.
reload_runtime_configs <- function(repo_root) {
  config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
  weights_path <- Sys.getenv("OPSI_WEIGHTS", file.path(repo_root, "config", "weights.yml"))
  missing_data_path <- Sys.getenv("OPSI_MISSING_DATA", file.path(repo_root, "config", "missing_data.yml"))
  index_definition_path <- Sys.getenv("OPSI_INDEX_DEFINITION", file.path(repo_root, "config", "index_definition.yml"))

  options(
    opportunity_security.config = yaml::read_yaml(config_path),
    opportunity_security.weights = yaml::read_yaml(weights_path),
    opportunity_security.missing_data = yaml::read_yaml(missing_data_path),
    opportunity_security.index_definition = yaml::read_yaml(index_definition_path)
  )
}

reload_runtime_configs(repo_root)

techs <- c("Electric Vehicles",
           "Nuclear","Coal","Batteries","Green Hydrogen","Wind","Oil",                       
           "Solar", "Gas", "Geothermal","Electric Grid")

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
missing_data <- getOption("opportunity_security.missing_data")
if (is.null(config) || is.null(weights) || is.null(missing_data)) {
  stop("Config, weights, or missing data config not loaded; run scripts/00_setup.R first.")
}

allow_partial_categories <- isTRUE(config$allow_partial_categories)
include_sub_sector <- isTRUE(if (!is.null(config$include_sub_sector)) {
  config$include_sub_sector
} else {
  config$energy_security_include_sub_sector
})
processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found: ", processed_dir)
}

read_processed_tbl <- function(name, processed_dir) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("Processed table not found: ", path)
  }
  readRDS(path)
}

# WDI country reference (names, ISO3, regions), read here so the script also runs in a fresh
# session; it used to rely on the country_info global left behind by scripts/10.
raw_data_path <- file.path(repo_root, config$raw_data_dir)
country_info <- standardize_country_info(
  read.csv(file.path(raw_data_path, "wdi_country_info.csv"))
)

energy_access_tbl <- read_processed_tbl("energy_access_tbl", processed_dir)
solar_pv_potential_tbl <- read_processed_tbl("solar_pv_potential_tbl", processed_dir)
wind_potential_tbl <- read_processed_tbl("wind_potential_tbl", processed_dir)
geothermal_potential_tbl <- read_processed_tbl("geothermal_potential_tbl", processed_dir)
import_dependence_tbl <- read_processed_tbl("import_dependence_tbl", processed_dir)
reserves_tbl <- read_processed_tbl("reserves_tbl", processed_dir)
foreign_dependency_tbl <- read_processed_tbl("foreign_dependency_tbl", processed_dir)
critical_minerals_processing_tbl <- read_processed_tbl(
  "critical_minerals_processing_tbl",
  processed_dir
)
critical_minerals_production_tbl <- read_processed_tbl(
  "critical_minerals_production_tbl",
  processed_dir
)
critical_minerals_trade_tbl <- read_processed_tbl("critical_minerals_trade_tbl", processed_dir)
energy_consumption_tbl <- read_processed_tbl("energy_consumption_tbl", processed_dir)
trade_concentration_tbl <- read_processed_tbl("trade_concentration_tbl", processed_dir)
energy_prices_tbl <- read_processed_tbl("energy_prices_tbl", processed_dir)
export_feasibility_tbl <- read_processed_tbl("export_feasibility_tbl", processed_dir)
future_demand_tbl <- read_processed_tbl("future_demand_tbl", processed_dir)
lcoe_competitiveness_tbl <- read_processed_tbl("lcoe_competitiveness_tbl", processed_dir)
market_share_manufacturing_tbl <- read_processed_tbl(
  "market_share_manufacturing_tbl",
  processed_dir
)
cost_competitiveness_tbl <- read_processed_tbl("cost_competitiveness_tbl", processed_dir)
production_depth_momentum_tbl <- read_processed_tbl(
  "production_depth_momentum_tbl",
  processed_dir
)
overcapacity_premium_tbl <- read_processed_tbl("overcapacity_premium_tbl", processed_dir)
technological_readiness_tbl <- read_processed_tbl(
  "technological_readiness_tbl",
  processed_dir
)
investment_momentum_tbl <- read_processed_tbl("investment_momentum_tbl", processed_dir)
policy_component_tbl <- read_processed_tbl("policy_component_tbl", processed_dir)
policy_outputs <- read_processed_tbl("policy_outputs", processed_dir)
nipo_tech_year<-read_processed_tbl("nipo_tech_year", processed_dir)

# The policy block of scripts/10 records which NIPO extract and as-of date the components
# were built from. A policy_outputs.rds without that record predates it: the component table
# shipped until Sep 2026 was the Feb 2026 build, seven months older than the pillars.
policy_provenance <- policy_outputs$provenance
if (is.null(policy_provenance)) {
  warning(
    "policy_outputs.rds has no build provenance, so policy_component_tbl.rds predates the ",
    "current policy block of scripts/10_build_themes.R. Rerun it before reporting Policy ",
    "or Strategic Index results."
  )
} else {
  message(
    "Policy components (scripts/10, built ", policy_provenance$built_at, "): IEA PAMS from ",
    if (is.null(policy_provenance$iea_pams_extract)) "an unrecorded extract" else policy_provenance$iea_pams_extract,
    ", CAT from ",
    if (is.null(policy_provenance$cat_ratings)) "an unrecorded file" else policy_provenance$cat_ratings,
    if (!is.null(policy_provenance$cat_eu_bloc_members)) {
      paste0(" (", length(policy_provenance$cat_eu_bloc_members), " EU members take the EU bloc rating)")
    },
    "; NIPO (", policy_provenance$nipo_extract, ", ", policy_provenance$nipo_scope,
    ") is built but not in the composite, see the policy block below."
  )
}

tech_ghg <- read_processed_tbl("tech_ghg_tbl", processed_dir)

energy_security_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
  wind_potential = wind_potential_tbl,
  geothermal_potential = geothermal_potential_tbl,
  import_dependence = import_dependence_tbl,
  reserves = reserves_tbl,
  foreign_dependency = foreign_dependency_tbl,
  critical_minerals_processing = critical_minerals_processing_tbl,
  critical_minerals_production = critical_minerals_production_tbl,
  critical_minerals_trade = critical_minerals_trade_tbl,
  energy_consumption = energy_consumption_tbl,
  trade_concentration = trade_concentration_tbl,
  energy_prices = energy_prices_tbl,
  investment_momentum = investment_momentum_tbl
)

# Every theme table carries the six supply-chain stages (R/utils/supply_chain_stages.R,
# docs/supply_chain_stages.md) since 2026-10-26, and the pillars are scored at six stages. A
# table still labelled Upstream / Midstream / Downstream (built by scripts/10 before the
# migration) would be scored as cells of its own and fall out of the Strategic Index, whose stage
# weights name the six stages only, so stop and name it.
assert_theme_tables_staged <- function(theme_tables) {
  for (nm in names(theme_tables)) {
    assert_supply_chain_stages(theme_tables[[nm]]$supply_chain, label = paste0(nm, " theme table supply_chain"))
  }
  invisible(theme_tables)
}
assert_theme_tables_staged(energy_security_inputs)

# Rankability rule (config/rankability.yml). The ES and EO tables keep every cell and value
# and add imputed_weight_share, rankable and rank_exclusion: a cell is rankable when the
# economy is sovereign and at most max_imputed_weight_share of its index weight is imputed.
# Cross-country rankings, top-N lists and the Strategic Index use rankable cells only. The
# option makes the rule visible to the scripts that run after this one in the same session.
rankability_path <- Sys.getenv("OPSI_RANKABILITY", file.path(repo_root, "config", "rankability.yml"))
if (!file.exists(rankability_path)) {
  stop("Rankability config not found: ", rankability_path)
}
rankability <- yaml::read_yaml(rankability_path)
options(opportunity_security.rankability = rankability)

energy_security_outputs <- build_energy_security_index_v2(
  theme_tables = energy_security_inputs,
  weights = weights$energy_security,
  missing_data = missing_data$energy_security,
  allow_partial_categories = allow_partial_categories,
  include_sub_sector = include_sub_sector,
  max_imputed_weight_share = rankability$max_imputed_weight_share
)

energy_security_category_scores <- energy_security_outputs$category_scores
energy_security_category_contributions <- energy_security_outputs$category_contributions
energy_security_variable_contributions <- energy_security_outputs$variable_contributions
energy_security_index <- energy_security_outputs$index %>%
  filter(tech %in% techs)

economic_opportunity_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
  wind_potential = wind_potential_tbl,
  geothermal_potential = geothermal_potential_tbl,
  energy_consumption = energy_consumption_tbl,
  energy_prices = energy_prices_tbl,
  export_feasibility = export_feasibility_tbl,
  future_demand = future_demand_tbl,
  lcoe_competitiveness = lcoe_competitiveness_tbl,
  market_share_manufacturing = market_share_manufacturing_tbl,
  cost_competitiveness = cost_competitiveness_tbl,
  production_depth_momentum = production_depth_momentum_tbl,
  overcapacity_premium = overcapacity_premium_tbl,
  technological_readiness = technological_readiness_tbl,
  investment_momentum = investment_momentum_tbl
)
assert_theme_tables_staged(economic_opportunity_inputs)

economic_opportunity_outputs <- build_economic_opportunity_index_v2(
  theme_tables = economic_opportunity_inputs,
  weights = weights$economic_opportunity,
  allow_partial_categories = allow_partial_categories,
  include_sub_sector = include_sub_sector,
  missing_data = missing_data$economic_opportunity,
  max_imputed_weight_share = rankability$max_imputed_weight_share
)

economic_opportunity_category_scores <- economic_opportunity_outputs$category_scores
economic_opportunity_category_contributions <- economic_opportunity_outputs$category_contributions
economic_opportunity_variable_contributions <- economic_opportunity_outputs$variable_contributions
economic_opportunity_index <- economic_opportunity_outputs$index%>%
  filter(tech %in% techs)

for (pillar_tbl in list(
  list(label = "Energy security", tbl = energy_security_index, col = "Energy_Security_Index"),
  list(label = "Economic opportunity", tbl = economic_opportunity_index, col = "Economic_Opportunity_Index")
)) {
  scored_cells <- !is.na(pillar_tbl$tbl[[pillar_tbl$col]])
  message(
    pillar_tbl$label, ": ", sum(pillar_tbl$tbl$rankable), " of ", sum(scored_cells),
    " scored headline cells are rankable (sovereign, imputed weight share <= ",
    rankability$max_imputed_weight_share, "); ",
    sum(scored_cells & grepl("not a sovereign economy", pillar_tbl$tbl$rank_exclusion)),
    " are territories and ",
    sum(scored_cells & grepl("imputed weight share above", pillar_tbl$tbl$rank_exclusion) &
          !grepl("not a sovereign economy", pillar_tbl$tbl$rank_exclusion)),
    " other cells exceed the imputed-weight threshold."
  )
}
rm(pillar_tbl, scored_cells)

technological_readiness_index <- economic_opportunity_category_scores %>%
  dplyr::filter(.data$category == "Technological Readiness") %>%
  dplyr::select(
    Country,
    tech,
    supply_chain,
    trl_index = category_score
  )

if (!exists("policy_component_tbl") || !exists("policy_outputs")) {
  stop("Policy theme outputs not found; run scripts/10_build_themes.R first.")
}

# Composite Policy index: IEA PAMS 3, CAT 2, renormalised over the components present in
# each cell (build_composite_policy_index() in R/indices/build_policy_index.R, with the weights
# and required components below).
# Eligibility (user decision, 9 Oct 2026): IEA PAMS is required, CAT is added where it rates the
# country. A country with IEA PAMS but no CAT rating is scored from IEA PAMS alone and flagged
# policy_basis "IEA PAMS only" (carried onto the policy index and Strategic Index rows); until
# then both components were required, which left 150 of 215 countries, Israel, Ukraine and nine
# coalition partners among them, without a Policy index and so without a Strategic Index.
# A country with CAT but no IEA PAMS is not scored (none in the Oct 2026 build; see
# R/indices/build_policy_index.R for why). OPSI_POLICY_REQUIRE_ALL=1 restores the
# both-required rule (robustness runs).
# Two components are left out (Oct 2026), the same for every country:
# - NIPO (GTA, weight 4 until Oct 2026) is excluded from the composite for now. scripts/10
#   still builds it and its tables stay on disk untouched (nipo_policy_index_tbl.rds,
#   nipo_tech_year.rds, the NIPO rows of policy_component_tbl.rds), so it can return by adding
#   it back to policy_component_weights. POLICY_COMPONENT_WEIGHTS, the function's default, is
#   the same IEA PAMS 3 + CAT 2 (so callers that rely on it rebuild this index); the weights
#   are passed explicitly here so the provenance records what was used.
# - The dual-use score is not a component: it is an LLM-elicited judgement identical for every
#   country, so it cannot rank countries. Its rows stay in policy_component_tbl and
#   dual_use_scores_tbl for reference.
# What was used is recorded in policy_index_provenance (saved in index_outputs.rds).
policy_component_weights <- c(
  "IEA PAMS Policy Index" = 3,
  "CAT Policy Index" = 2
)
policy_components_excluded <- c(
  "NIPO Policy Index" = paste(
    "excluded from the composite for now (Oct 2026); NIPO tables kept on disk untouched and",
    "may return"
  ),
  "Dual-use policy score" = paste(
    "not a component (Oct 2026): LLM-elicited judgement identical for every country, so it",
    "cannot rank countries"
  )
)
policy_require_all <- tolower(Sys.getenv("OPSI_POLICY_REQUIRE_ALL", "false")) %in% c("1", "true", "yes")
policy_required_components <- if (policy_require_all) {
  names(policy_component_weights)
} else {
  "IEA PAMS Policy Index"
}
policy_build <- build_composite_policy_index(
  policy_component_tbl,
  techs = techs,
  component_weights = policy_component_weights,
  required_components = policy_required_components
)
policy_index <- policy_build$index
policy_eligibility <- policy_build$eligibility
policy_full_basis <- policy_basis_label(names(policy_component_weights))
policy_n_every_component <- sum(policy_eligibility$n_components == length(policy_component_weights))
policy_scored <- policy_eligibility %>% dplyr::filter(.data$eligible)
policy_basis_counts <- table(factor(
  policy_scored$policy_basis,
  levels = unique(c(policy_full_basis, sort(unique(policy_scored$policy_basis))))
))
message(
  "Policy index: ",
  paste0(policy_component_label(names(policy_component_weights)), " (", policy_component_weights, ")",
         collapse = " + "),
  "; excluded: ", paste(policy_component_label(names(policy_components_excluded)), collapse = ", "),
  ". Required: ", paste(policy_component_label(policy_required_components), collapse = " + "),
  if (policy_require_all) {
    " (OPSI_POLICY_REQUIRE_ALL=1)"
  } else {
    " (a country without the other components is scored on what it has)"
  },
  ". ", policy_n_every_component, " of ", nrow(policy_eligibility),
  " countries have every component; ", nrow(policy_scored), " are scored (",
  paste(names(policy_basis_counts), policy_basis_counts, sep = ": ", collapse = ", "), ")."
)
policy_index_provenance <- list(
  components = names(policy_component_weights),
  weights = policy_component_weights,
  excluded_components = policy_components_excluded,
  required_components = policy_required_components,
  eligibility_rule = paste0(
    "a country is scored if ",
    if (policy_require_all) "every component" else
      paste(policy_component_label(policy_required_components), collapse = " and "),
    " is observed for at least one of its cells (over the headline technologies in `techs`)",
    if (!policy_require_all) {
      paste0(
        "; a component it lacks everywhere is left out and policy_basis records what its index ",
        "rests on (e.g. '", policy_basis_label(policy_required_components), "'; user decision ",
        "9 Oct 2026, both were required before)"
      )
    },
    "; weights are renormalised over the components present in each cell"
  ),
  techs = techs,
  n_countries_eligible = nrow(policy_scored),
  eligible_countries = sort(policy_scored$Country),
  # the both-required count (the rule until 9 Oct 2026) and the scored countries by basis
  n_countries_every_component = policy_n_every_component,
  countries_by_basis = lapply(split(policy_scored$Country, policy_scored$policy_basis), sort),
  # scripts/10's record of the component build (NIPO extract, as-of date, CAT EU members);
  # NULL for a policy_outputs.rds built before that record existed
  component_build = policy_provenance,
  built_at = Sys.time()
)

# Strategic Index (R/indices/build_indices.R):
#   0.2 * pol + 0.3 * EO + 0.3 * energy_security_need + 0.1 * sc_weight + 0.1 * tech_weight,
# where energy_security_need = 1 - Energy_Security_Index (the pillar is oriented
# higher = more secure, so the SI favours sectors where the country is least secure),
# pol is the within-country S-curve of the Policy index and sc_weight is the stage weight of
# SUPPLY_CHAIN_STAGE_SI_WEIGHTS (R/utils/supply_chain_stages.R: each of the six stages an equal
# share of its roll-up's weight, Upstream 0.50, Midstream 0.75, Downstream 0.25). Cells missing
# EO, ES or policy, and cells whose EO or ES value is not rankable (territory, or more than
# max_imputed_weight_share of its weight imputed), are left out rather than imputed;
# si_exclusion says why.
# Manufacturing sectors (Components, Assembly) that no Energy Security theme measures are not
# reported (strategic_unmeasured_sectors(): Geothermal - Components and - Assembly), the rule
# behind the three-stage build's hand-picked exclusion of Geothermal - Midstream.
# Sectors whose Economic Opportunity rests only on inputs copied onto every stage (costs, demand,
# technology-wide readiness) are not reported either (strategic_unevidenced_sectors()). A
# readiness cell rests on items of its own unless the theme says it took the technology-wide value.
strategic_unmeasured_es <- strategic_unmeasured_sectors(economic_opportunity_index, energy_security_index)
trl_stage_cells <- technological_readiness_tbl %>%
  dplyr::filter(
    .data$variable == "TRL Index",
    !grepl("technology-wide", .data$explanation, fixed = TRUE)
  ) %>%
  dplyr::distinct(.data$tech, .data$supply_chain)
strategic_unevidenced <- strategic_unevidenced_sectors(economic_opportunity_category_scores, trl_stage_cells)
strategic_unevidenced <- strategic_unevidenced[sub(" - .*$", "", strategic_unevidenced) %in% techs]
strategic_unmeasured <- sort(unique(c(strategic_unmeasured_es, strategic_unevidenced)))
strategic_excluded_sectors <- strategic_unmeasured
message(
  "Strategic index: not reporting the manufacturing sectors no Energy Security theme measures: ",
  if (length(strategic_unmeasured_es) > 0) paste(strategic_unmeasured_es, collapse = ", ") else "none",
  "; nor the sectors whose Economic Opportunity has no stage-specific input: ",
  if (length(strategic_unevidenced) > 0) paste(strategic_unevidenced, collapse = ", ") else "none",
  "."
)
strategic_index <- build_strategic_index(
  economic_opportunity_index = economic_opportunity_index,
  energy_security_index = energy_security_index,
  policy_index = policy_index,
  tech_ghg = tech_ghg,
  technological_readiness_index = technological_readiness_index,
  techs = techs,
  supply_chain_weights = SUPPLY_CHAIN_STAGE_SI_WEIGHTS,
  excluded_sectors = strategic_unmeasured
)
message(
  "Strategic index: ", sum(!is.na(strategic_index$strategic_index)), " cells for ",
  dplyr::n_distinct(strategic_index$Country[!is.na(strategic_index$strategic_index)]),
  " countries."
)

# A pillar that is constant across countries within a sector is a sector constant, not a
# measurement (ES was 0 everywhere in Nuclear - Upstream in the Sep 2026 three-stage build, which
# made it the top pick of 8 of the 18 allies scored). Name such sectors before anything is reported.
# Such sectors are excluded and the index rebuilt, so the within-country policy ranks are
# taken over the sectors that remain.
strategic_degenerate <- strategic_index_degenerate_sectors(strategic_index)
if (nrow(strategic_degenerate) > 0) {
  message(
    "Strategic index: excluding sectors where a pillar is constant across countries: ",
    paste0(
      strategic_degenerate$sector, " (",
      dplyr::case_when(
        strategic_degenerate$es_constant & strategic_degenerate$eo_constant ~ "ES and EO",
        strategic_degenerate$es_constant ~ "ES",
        TRUE ~ "EO"
      ), ")",
      collapse = ", "
    )
  )
  strategic_excluded_sectors <- unique(c(strategic_unmeasured, strategic_degenerate$sector))
  strategic_index <- build_strategic_index(
    economic_opportunity_index = economic_opportunity_index,
    energy_security_index = energy_security_index,
    policy_index = policy_index,
    tech_ghg = tech_ghg,
    technological_readiness_index = technological_readiness_index,
    techs = techs,
    supply_chain_weights = SUPPLY_CHAIN_STAGE_SI_WEIGHTS,
    excluded_sectors = strategic_excluded_sectors
  )
  message(
    "Strategic index after exclusions: ", sum(!is.na(strategic_index$strategic_index)),
    " cells for ",
    dplyr::n_distinct(strategic_index$Country[!is.na(strategic_index$strategic_index)]),
    " countries."
  )
}

# Endowment screen (strategic_resource_base() / flag_strategic_resource_base(),
# R/indices/build_indices.R): an Extraction stage is eligible for a member's allied pick only
# where the country has recorded reserves or production of that stage's resources in the
# reserves or production themes. The index is unchanged; screened-out cells carry
# resource_base = FALSE and pick_exclusion "no resource base". Without it the need reading made
# Nuclear - Upstream (three-stage build) the top pick of 20 of 45 members, most of them with no
# uranium. OPSI_SI_RESOURCE_SCREEN=0 turns the screen off (robustness runs).
si_resource_screen <- !identical(Sys.getenv("OPSI_SI_RESOURCE_SCREEN"), "0")
strategic_resource_base_tbl <- strategic_resource_base(
  reserves_tbl, critical_minerals_production_tbl,
  stage = "Extraction"
)
strategic_index <- flag_strategic_resource_base(strategic_index, strategic_resource_base_tbl)
si_screened <- !is.na(strategic_index$strategic_index) & !is.na(strategic_index$resource_base)
message(
  "Strategic index endowment screen (", if (si_resource_screen) "on" else "OFF", "): ",
  sum(si_screened & strategic_index$resource_base %in% FALSE), " of ", sum(si_screened),
  " scored Extraction cells in ", dplyr::n_distinct(strategic_index$tech[si_screened]),
  " screened technologies have no recorded resource base."
)

# Allied strategic selection over the derived coalition (coalition_members() in
# R/indices/build_indices.R; no hand-written list): the two-tier coalition of coalition_tbl.rds
# (scripts/13_build_coalition.R, tier core or partner) or, until that table exists, the
# single-tier alignment coalition of geo_coalition_tbl.rds (scripts/12, member == TRUE, all
# tier "core"; coalition_members() warns). The selection stays national: every member, EU
# members individually, gets its own pick, and allied_strategic.csv records its iso3, tier and
# network_node (the EU bundle is a node of the allied network only, scripts/30). A pick is a
# sector in any stage but Deployment (STRATEGIC_ALLIED_STAGES: Extraction, Refining, Components,
# Assembly, Recovery, the old Upstream and Midstream).
coalition_path <- file.path(processed_dir, "coalition_tbl.rds")
geo_coalition_path <- file.path(processed_dir, "geo_coalition_tbl.rds")
if (!file.exists(coalition_path) && !file.exists(geo_coalition_path)) {
  stop(
    "No coalition table in ", processed_dir, ": neither coalition_tbl.rds ",
    "(scripts/13_build_coalition.R) nor geo_coalition_tbl.rds ",
    "(scripts/12_build_geopolitical_alignment.R). The allied coalition is derived, not listed by ",
    "hand; run scripts/12_build_geopolitical_alignment.R and scripts/13_build_coalition.R first."
  )
}
coalition <- if (file.exists(coalition_path)) {
  coalition_members(readRDS(coalition_path))
} else {
  coalition_members(NULL, geo_coalition_tbl = readRDS(geo_coalition_path))
}
coalition_iso3 <- coalition$iso3
allies <- coalition %>%
  dplyr::select("iso3", "tier", "network_node") %>%
  dplyr::left_join(
    country_info %>% dplyr::distinct(iso3 = .data$iso3c, Country = .data$country),
    by = "iso3"
  )
coalition_unmatched <- allies$iso3[is.na(allies$Country)]
if (length(coalition_unmatched) > 0) {
  stop(
    "Coalition members missing from wdi_country_info.csv: ",
    paste(coalition_unmatched, collapse = ", "), "."
  )
}
if (anyDuplicated(allies$iso3)) {
  stop(
    "wdi_country_info.csv has more than one name for coalition members: ",
    paste(unique(allies$iso3[duplicated(allies$iso3)]), collapse = ", "), "."
  )
}

allied_index <- select_allied_strategic_sectors(
  strategic_index,
  ally_countries = allies,
  stages = STRATEGIC_ALLIED_STAGES,
  require_resource_base = si_resource_screen
)
allies_policy <- allies %>%
  dplyr::left_join(
    policy_eligibility %>% dplyr::select("Country", "eligible", "policy_basis", "missing_required"),
    by = "Country"
  )
allies_unpicked <- allies_policy %>%
  dplyr::filter(!.data$Country %in% allied_index$Country) %>%
  dplyr::mutate(reason = dplyr::case_when(
    is.na(.data$eligible) ~ "no policy component at all",
    !.data$eligible ~ paste0("no Policy index, lacks ", .data$missing_required),
    TRUE ~ paste0("no in-sample cell outside Deployment; Policy index ", .data$policy_basis)
  )) %>%
  dplyr::arrange(.data$tier, .data$Country)
# Members picked on a Policy index without every component (the PAMS-only fallback).
allies_partial_policy <- allies_policy %>%
  dplyr::filter(
    .data$Country %in% allied_index$Country,
    .data$eligible %in% TRUE,
    .data$policy_basis != policy_full_basis
  ) %>%
  dplyr::arrange(.data$tier, .data$Country)
message(
  "Allied strategic selection: coalition of ", nrow(coalition), " (", coalition$source[1], "; ",
  paste(names(table(coalition$tier)), table(coalition$tier), sep = " ", collapse = ", "), "); ",
  dplyr::n_distinct(allied_index$Country),
  " members have a scored sector outside Deployment",
  if (nrow(allies_partial_policy) > 0) {
    partial_by_basis <- split(allies_partial_policy$Country, allies_partial_policy$policy_basis)
    paste0(
      " (Policy index ",
      paste0(
        names(partial_by_basis), " for ", lengths(partial_by_basis), ": ",
        vapply(partial_by_basis, paste, character(1), collapse = ", "),
        collapse = "; "
      ),
      ")"
    )
  },
  "; none for ", nrow(allies_unpicked),
  if (nrow(allies_unpicked) > 0) {
    paste0(
      ": ",
      paste0(allies_unpicked$Country, " (", allies_unpicked$tier, "; ", allies_unpicked$reason, ")",
             collapse = ", ")
    )
  },
  "."
)
# Diagnostic (nothing excluded): sectors where one ES value is shared by many scored countries
# (strategic_index_es_floor_sectors(), R/indices/build_indices.R), with the allied picks there.
es_floor_sectors <- strategic_index_es_floor_sectors(strategic_index)
if (nrow(es_floor_sectors) > 0) {
  message(
    "Strategic index: sectors where one Energy_Security_Index value is shared by >= 25% of the ",
    "scored countries (a floor, read as a high sector-wide need): ",
    paste0(
      es_floor_sectors$sector, " (", es_floor_sectors$n_at_mode, " of ", es_floor_sectors$n_countries,
      " at ", signif(es_floor_sectors$es_mode, 3), "; allied picks ",
      vapply(es_floor_sectors$sector, function(s) sum(allied_index$sector == s), integer(1)), ")",
      collapse = ", "
    )
  )
}

charts_dir <- file.path(processed_dir, "charts")
if (!dir.exists(charts_dir)) {
  dir.create(charts_dir, recursive = TRUE)
}
write.csv(allied_index, file.path(charts_dir, "allied_strategic.csv"), row.names = FALSE)


outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(repo_root, config$processed_dir, "outputs")
}

if (!dir.exists(outputs_dir)) {
  dir.create(outputs_dir, recursive = TRUE)
}

utils::write.csv(
  energy_security_category_contributions,
  file = file.path(outputs_dir, "energy_security_category_contributions.csv"),
  row.names = FALSE
)

utils::write.csv(
  energy_security_variable_contributions,
  file = file.path(outputs_dir, "energy_security_variable_contributions.csv"),
  row.names = FALSE
)

outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
if (!nzchar(outputs_rds_path)) {
  outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
    file.path(repo_root, config$outputs_rds)
  } else {
    file.path(outputs_dir, "index_outputs.rds")
  }
}

outputs_rds_dir <- dirname(outputs_rds_path)
if (!dir.exists(outputs_rds_dir)) {
  dir.create(outputs_rds_dir, recursive = TRUE)
}

saveRDS(
  list(
    energy_security_outputs = energy_security_outputs,
    economic_opportunity_outputs = economic_opportunity_outputs,
    policy_outputs = policy_outputs,
    energy_security_category_scores = energy_security_category_scores,
    energy_security_category_contributions = energy_security_category_contributions,
    energy_security_variable_contributions = energy_security_variable_contributions,
    energy_security_index = energy_security_index,
    economic_opportunity_category_scores = economic_opportunity_category_scores,
    economic_opportunity_category_contributions = economic_opportunity_category_contributions,
    economic_opportunity_variable_contributions = economic_opportunity_variable_contributions,
    economic_opportunity_index = economic_opportunity_index,
    policy_component_tbl = policy_component_tbl,
    policy_index = policy_index,
    policy_eligibility = policy_eligibility,
    policy_index_provenance = policy_index_provenance,
    strategic_index = strategic_index,
    # the sectors left out of the Strategic Index (strategic_unmeasured_sectors(), plus any
    # sector where a pillar is constant across countries), for consumers that rebuild it
    strategic_excluded_sectors = strategic_excluded_sectors,
    # the coalition the allied selection used (coalition_members(): iso3, country, tier,
    # network_node, source) and its ISO3 codes
    coalition_members = coalition,
    allied_coalition_iso3 = coalition_iso3
  ),
  outputs_rds_path
)


# install.packages("openxlsx")
library(dplyr)
library(openxlsx)

# Countries to export (data may store Korea as "South Korea")
countries_export <- c("Japan", "India", "South Korea", "United States")

# Build the 4 data frames (and name sheets as requested)
sheets <- list(
  "Japan"   = strategic_index %>% filter(Country == "Japan"),
  "India"   = strategic_index %>% filter(Country == "India"),
  "Korea"   = strategic_index %>% filter(Country %in% c("Korea", "South Korea")),
  "USA"= strategic_index %>% filter(Country == "United States"),
  "UK" = strategic_index %>% filter(Country=="United Kingdom")
)

# The columns are the Strategic Index inputs as they enter the formula:
#   strategic index = 0.2 pol + 0.3 economic_opportunity + 0.3 energy_security_need
#                     + 0.1 neis_weight + 0.1 climate_weight.
# energy_security is the pillar itself (higher = more secure); energy_security_need =
# 1 - energy_security. The sheets used to show the S-curved eo and es instead, which are not
# in the formula, and flipped es a second time, so "es" held S-curved insecurity.
sheets <- lapply(sheets, function(df) {
  df %>%
    transmute(
      Country,
      sector,
      tech,
      supply_chain,
      economic_opportunity = Economic_Opportunity_Index,
      energy_security = Energy_Security_Index,
      energy_security_need,
      pol,
      neis_weight=sc_weight,
      climate_weight=tech_weight,
      trl_index,
      `strategic index` = strategic_index,
      si_exclusion
    ) %>%
    arrange(desc(`strategic index`))
})

# Write to Excel with one tab per country
out_path <- file.path(processed_dir, "strategic_index_selected_countries.xlsx")
openxlsx::write.xlsx(
  x = sheets,
  file = out_path,
  overwrite = TRUE
)

out_path

library(dplyr)
library(tidyr)
library(ggplot2)

# Decompose strategic_index into its weighted terms, which sum to it exactly:
#   0.3 * Economic_Opportunity_Index   (contrib_eo)
#   0.3 * energy_security_need         (contrib_es_need; = 1 - Energy_Security_Index)
#   0.2 * pol                          (contrib_policy)
#   0.1 * sc_weight                    (contrib_sc)
#   0.1 * tech_weight                  (contrib_ghg)
# strategic_index_contributions() takes the weights from the same constant as the index,
# so the plot cannot drift from the formula again.

# The sectors build_strategic_index() leaves out (strategic_unmeasured) are not in the table.
top10_by_country <- strategic_index_contributions(strategic_index) %>%
  filter(!is.na(strategic_index)) %>%
  group_by(Country) %>%
  slice_max(order_by = strategic_index, n = 10, with_ties = FALSE) %>%
  arrange(Country, strategic_index) %>%   # ascending so highest ends up at top after coord_flip()
  mutate(
    # unique key per facet to enforce ordering
    sector_key = paste(Country, sector, sep = "||"),
    sector_key = factor(sector_key, levels = unique(sector_key))
  ) %>%
  ungroup()

plot_df <- top10_by_country %>%
  filter(Country %in% c("Japan","South Korea","India","Viet Nam","United Kingdom")) %>%
  select(Country, sector, sector_key, strategic_index,
         contrib_eo, contrib_es_need, contrib_policy, contrib_sc,contrib_ghg) %>%
  pivot_longer(
    cols = starts_with("contrib_"),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = recode(component,
                       contrib_eo      = "Economic opportunity",
                       contrib_es_need = "Energy security need (1 - ES)",
                       contrib_policy  = "Policy",
                       contrib_ghg     = "Climate",
                       contrib_sc = "Supply chain"
    )
  )

rmi_palette <- c("#0BD0D9",
                 "#0989B1",
                 "#003A61",
                 "#FFCA08",
                 "#F8931D",
                 "#548538",
                 "#7F7F7F")

ggplot(plot_df %>% filter(Country=="Japan"), aes(x = sector_key, y = contribution, fill = component)) +
  geom_col() +
  coord_flip() +
  #facet_wrap(~ Country, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
  scale_fill_manual(values = rmi_palette) +
  labs(x = NULL, y = "Weighted contribution to strategic_index", fill = NULL) +
  theme_minimal()

strategic_asia <- strategic_index %>%
  inner_join(country_info %>%
               filter(region=="East Asia & Pacific"),by=c("Country"="country"))
