library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

read_processed_tbl <- function(name, processed_dir) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Missing processed table: ", path)
  readRDS(path)
}

derive_strategic_path <- function(eo, es_risk, cut = 0.5) {
  case_when(
    es_risk >= cut & eo <  cut ~ "friendshore",
    es_risk >= cut & eo >= cut ~ "leapfrog",
    es_risk <  cut & eo >= cut ~ "export_promotion",
    TRUE                       ~ "alternative_value_creation"
  )
}

# Seed only. You should allow manual overrides later.
derive_stage_from_trl <- function(trl_index) {
  case_when(
    is.na(trl_index)        ~ "unknown",
    trl_index < 0.20        ~ "r_and_d_or_prototype",
    trl_index < 0.40        ~ "foak_or_demonstration",
    trl_index < 0.60        ~ "early_commercialization_or_noak",
    trl_index < 0.80        ~ "scale_up_or_diffusion",
    TRUE                    ~ "mature_or_competitive"
  )
}

# Default relevance overlay for CATF risks.
# Replace with a CSV override table once you start curating.
add_catf_default_relevance <- function(tbl) {
  tbl %>%
    mutate(
      construction_cost_overrun_relevance = case_when(
        technology_stage_baseline %in% c("foak_or_demonstration", "early_commercialization_or_noak") ~ 3,
        technology_stage_baseline == "scale_up_or_diffusion" ~ 2,
        TRUE ~ 1
      ),
      technology_performance_relevance = case_when(
        technology_stage_baseline %in% c("r_and_d_or_prototype", "foak_or_demonstration", "early_commercialization_or_noak") ~ 3,
        TRUE ~ 1
      ),
      revenue_adequacy_relevance = case_when(
        supply_chain_phase == "Downstream" ~ 3,
        tech %in% c("Hydrogen", "Geothermal", "Nuclear", "CCUS", "Low Emissions Ammonia") ~ 3,
        TRUE ~ 1
      ),
      regulatory_uncertainty_relevance = case_when(
        tech %in% c("Nuclear", "Hydrogen", "CCUS", "Geothermal") ~ 3,
        TRUE ~ 1
      ),
      enabling_infrastructure_relevance = case_when(
        tech %in% c("Hydrogen", "Electric Grid", "Geothermal", "CCUS", "Low Emissions Ammonia") ~ 3,
        supply_chain_phase == "Downstream" ~ 2,
        TRUE ~ 1
      ),
      social_opposition_relevance = case_when(
        tech %in% c("Wind", "Nuclear", "Mining", "Critical Minerals", "Geothermal", "CCUS") ~ 2,
        TRUE ~ 1
      ),
      supply_chain_limitations_relevance = case_when(
        strategic_path_baseline == "friendshore" ~ 3,
        supply_chain_phase %in% c("Upstream", "Midstream") ~ 2,
        TRUE ~ 1
      ),
      commercial_insurance_relevance = case_when(
        technology_stage_baseline %in% c("foak_or_demonstration", "early_commercialization_or_noak") ~ 2,
        TRUE ~ 1
      ),
      delivery_system_relevance = case_when(
        supply_chain_phase == "Downstream" ~ 2,
        tech %in% c("Wind", "Geothermal", "Nuclear", "CCUS", "Hydrogen") ~ 2,
        TRUE ~ 1
      )
    )
}

# -----------------------------------------------------------------------------
# Main builder
# -----------------------------------------------------------------------------

build_country_sector_context <- function(
    repo_root = ".",
    outputs_rds = NULL,
    processed_dir = file.path(repo_root, "data/processed"),
    countries = c("Japan", "India", "South Korea", "Viet Nam")
) {
  # Source repo helpers
  source(file.path(repo_root, "R", "utils", "scurve.R"))
  source(file.path(repo_root, "R", "charts", "package_selection_viz.R"))
  
  outputs_rds <- outputs_rds %||% file.path(processed_dir, "outputs", "index_outputs.rds")
  index_outputs <- readRDS(outputs_rds)
  
  # ---------------------------------------------------------------------------
  # 1) Base strategic table at Country x tech x supply_chain
  # ---------------------------------------------------------------------------
  strategic_base <- map_dfr(
    countries,
    ~ build_country_strategic_tbl(index_outputs, country = .x, include_sub_sector = FALSE)
  ) %>%
    transmute(
      country = Country,
      sector = tech,
      tech = tech,
      supply_chain_phase = supply_chain,
      economic_opportunity_score = eo,
      energy_security_risk_score = es_risk,
      policy_score = pol,
      trl_index = trl_index,
      strategic_path_baseline = derive_strategic_path(eo, es_risk),
      strategic_index = strategic_index
    )
  
  # ---------------------------------------------------------------------------
  # 2) Subsector scaffold
  # Try to preserve one row per Country x tech x sub_sector x supply_chain
  # ---------------------------------------------------------------------------
  eo_tbl <- index_outputs$economic_opportunity_index
  
  subsector_scaffold <- eo_tbl %>%
    filter(Country %in% countries) %>%
    transmute(
      country = Country,
      sector = tech,
      tech = tech,
      subsector = if ("sub_sector" %in% names(.)) as.character(sub_sector) else NA_character_,
      supply_chain_phase = supply_chain
    ) %>%
    distinct() %>%
    mutate(
      subsector = if_else(is.na(subsector) | subsector == "", sector, subsector)
    )
  
  context_tbl <- subsector_scaffold %>%
    left_join(
      strategic_base,
      by = c("country", "sector", "tech", "supply_chain_phase")
    ) %>%
    mutate(
      context_id = str_c(
        str_replace_all(country, "\\s+", "_"), "__",
        str_replace_all(sector, "\\s+", "_"), "__",
        str_replace_all(subsector, "\\s+", "_"), "__",
        str_replace_all(supply_chain_phase, "\\s+", "_")
      ),
      technology_stage_baseline = derive_stage_from_trl(trl_index)
    )
  
  # ---------------------------------------------------------------------------
  # 3) Partner priority score
  # Use newer partner contributions outputs, not the stale shortlist code.
  # ---------------------------------------------------------------------------
  fs_contrib <- tryCatch(
    read_processed_tbl("partner_friendshore_contributions_tbl", processed_dir),
    error = function(e) tibble()
  )
  
  partner_priority_tbl <- tibble(
    country = character(),
    tech = character(),
    supply_chain_phase = character(),
    partner_priority_score = numeric()
  )
  
  if (nrow(fs_contrib) > 0) {
    partner_priority_tbl <- fs_contrib %>%
      transmute(
        country = reporter,
        partner = partner,
        tech = tech,
        supply_chain_phase = supply_chain,
        contribution = coalesce(contribution, value)
      ) %>%
      filter(country %in% countries) %>%
      group_by(country, tech, supply_chain_phase, partner) %>%
      summarise(friendshore_score = mean(contribution, na.rm = TRUE), .groups = "drop") %>%
      group_by(country, tech, supply_chain_phase) %>%
      summarise(
        partner_priority_score = mean(head(sort(friendshore_score, decreasing = TRUE), 3), na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  context_tbl <- context_tbl %>%
    left_join(
      partner_priority_tbl,
      by = c("country", "tech", "supply_chain_phase")
    )
  
  # ---------------------------------------------------------------------------
  # 4) Partnership strength score
  # Best practice: persist psi_tbl from scripts/25_build_partner_indices.R
  # and join it here. If absent, leave NA.
  # ---------------------------------------------------------------------------
  psi_path <- file.path(processed_dir, "psi_tbl.rds")
  
  if (file.exists(psi_path)) {
    psi_tbl <- readRDS(psi_path)
    
    psi_join <- psi_tbl %>%
      transmute(
        country = Country,
        partnership_strength_score = value
      ) %>%
      group_by(country) %>%
      summarise(
        partnership_strength_score = mean(partnership_strength_score, na.rm = TRUE),
        .groups = "drop"
      )
    
    context_tbl <- context_tbl %>%
      left_join(psi_join, by = "country")
  } else {
    context_tbl <- context_tbl %>%
      mutate(partnership_strength_score = NA_real_)
  }
  
  # ---------------------------------------------------------------------------
  # 5) CATF risk relevance defaults
  # ---------------------------------------------------------------------------
  context_tbl <- context_tbl %>%
    add_catf_default_relevance() %>%
    mutate(
      context_note = NA_character_,
      context_source_ids = NA_character_
    ) %>%
    select(
      context_id,
      country,
      sector,
      subsector,
      supply_chain_phase,
      technology_stage_baseline,
      strategic_path_baseline,
      energy_security_risk_score,
      economic_opportunity_score,
      partnership_strength_score,
      partner_priority_score,
      construction_cost_overrun_relevance,
      technology_performance_relevance,
      revenue_adequacy_relevance,
      regulatory_uncertainty_relevance,
      enabling_infrastructure_relevance,
      social_opposition_relevance,
      supply_chain_limitations_relevance,
      commercial_insurance_relevance,
      delivery_system_relevance,
      context_note,
      context_source_ids
    ) %>%
    arrange(country, sector, subsector, supply_chain_phase)
  
  context_tbl
}

# Example run:
ctx <- build_country_sector_context(repo_root = ".")
readr::write_csv(ctx, "outputs/tables/country_sector_context.csv")