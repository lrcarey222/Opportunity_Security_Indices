# Figures and numbers for the Economic Opportunity results (paper Section 6, Results subsection).
#
# Figures (paper/figures/<name>.pdf + png twin):
#   eo_distribution  EO by technology within each supply-chain stage (box plots across the
#                    rankable economies of each technology-stage pair).
#   eo_contrib       Mean category shares of the pillar's level and of its within-pair
#                    cross-country variance, by stage (rankable cells).
#   eo_heatmap       Within-pair percentile rank of the 57 coalition members (31 core, 26
#                    partners) and China in every technology-stage pair; cells that are not
#                    rankable are shown but not coloured.
#   eo_top           Ten highest-scoring rankable economies in four illustrative pairs, bars
#                    stacked by contribution (technology-level constants versus country-varying
#                    categories).
# Numbers: paper/results/eo.json (every number quoted in paper/results/text/eo.tex); the tables
# paper/tables/eo_weight_split.tex and paper/tables/eo_leaders.tex are written here from the same
# objects.
#
# Inputs (read only): data/processed/outputs/index_outputs.rds (the final rerun of
# 2026-10-12: economic_opportunity_*, energy_security_index for the ES comparison,
# coalition_members),
# data/processed/cost_competitiveness_tbl.rds (lending rates and earnings behind the Input Cost
# Index, for the cost explanations of the text) and data/raw/wdi_country_info.csv (World Bank
# income groups, for the "emerging economy" flag).
# R/utils/country.R is sourced for its pure name <-> ISO3 helpers.
#
# Conventions:
#   * Sample: the eleven headline technologies x three stages (economic_opportunity_index).
#   * Cross-country statistics (distribution, fixed effects, contributions, ranks, leaders,
#     coalition positions, top tens) use RANKABLE cells only (config/rankability.yml: sovereign
#     economy and at most half of the cell's index weight imputed), within pairs whose
#     diagnostics status (index_cells) is "ok". Scored-but-not-rankable cells keep their values;
#     their statistics are reported separately where useful (suffix _all_scored).
#   * A variable is a technology-level constant in a pair if its component value has zero range
#     across all scored economies of the pair (checked in the data, not assumed).
#   * Coalition = io$coalition_members (57 members: tier core 31, tier partner 26).
#   * "Emerging economy" = World Bank income group low, lower-middle or upper-middle income in the
#     staged WDI country file; Taiwan, which WDI omits, is classified high income as in the World
#     Bank's own list.
#   * Rank correlations are Spearman.
#
# Run from the worktree root:
#   Rscript scripts/paper/fig_eo.R

local({
  find_theme <- function() {
    d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    while (dirname(d) != d) {
      f <- file.path(d, "scripts", "paper", "_paper_theme.R")
      if (file.exists(f)) return(f)
      d <- dirname(d)
    }
    stop("Run from inside the repository.")
  }
  source(find_theme(), local = FALSE)
})

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
options(dplyr.summarise.inform = FALSE)

root <- paper_repo_root()
source(file.path(root, "R", "utils", "country.R"))
config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
raw_dir <- file.path(root, config$raw_data_dir)
processed_dir <- file.path(root, config$processed_dir)
rank_cfg <- yaml::read_yaml(file.path(root, "config", "rankability.yml"))

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------

stages <- c("Upstream", "Midstream", "Downstream")
# Category order and weights of config/weights.yml (Table tab:eo_structure).
category_order <- c(
  "Trade", "Production", "Technology Demand", "Technological Readiness", "Energy Prices",
  "Investment", "Energy Access", "Foreign Dependency", "Cost Competitiveness", "Consumption"
)
# The four illustrative pairs of eo_top: the three clean-technology pairs of the task
# (batteries and solar midstream, electric vehicles downstream) and upstream oil, the upstream
# pair of Figure es_top, which has the second-largest number of rankable economies of the
# upstream pairs (after nuclear; the counts are written to eo.json).
top_cells <- tibble::tibble(
  tech = c("Oil", "Batteries", "Solar", "Electric Vehicles"),
  supply_chain = c("Upstream", "Midstream", "Midstream", "Downstream")
)
tech_short <- c(
  "Batteries" = "Batteries", "Coal" = "Coal", "Electric Grid" = "Grid",
  "Electric Vehicles" = "EVs", "Gas" = "Gas", "Geothermal" = "Geothermal",
  "Green Hydrogen" = "Hydrogen", "Nuclear" = "Nuclear", "Oil" = "Oil", "Solar" = "Solar",
  "Wind" = "Wind"
)
# Shorter row labels for the heatmap.
name_short <- c("Democratic Republic of Congo" = "DR Congo",
                "Dominican Republic" = "Dominican Rep.")
const_tol <- 1e-12
tau <- rank_cfg$max_imputed_weight_share

# ---------------------------------------------------------------------------
# Local helpers (pure)
# ---------------------------------------------------------------------------

country_iso3 <- function(country) {
  canon <- country_canonical_names()
  out <- unname(setNames(names(canon), canon)[country])
  miss <- is.na(out)
  if (any(miss)) {
    out[miss] <- suppressWarnings(countrycode::countrycode(country[miss], "country.name", "iso3c"))
  }
  out
}

r2_of <- function(formula, data) summary(stats::lm(formula, data = data))$r.squared

stage_factor <- function(x) factor(x, levels = stages)

q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE, names = FALSE))

moments <- function(x) {
  list(
    n = length(x), mean = mean(x), sd = stats::sd(x), min = min(x), p10 = q(x, 0.10),
    p25 = q(x, 0.25), median = stats::median(x), p75 = q(x, 0.75), p90 = q(x, 0.90), max = max(x)
  )
}

# Records of a data frame as a list of named lists (JSON array of objects).
records <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

named_by <- function(df, key, value) as.list(setNames(df[[value]], df[[key]]))

spearman <- function(a, b) stats::cor(a, b, method = "spearman")

# Within-pair percentile rank, (n - average rank from the top) / (n - 1): 1 = top, 0 = bottom.
pct_rank <- function(x) (length(x) - rank(-x, ties.method = "average")) / (length(x) - 1)

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

io <- readRDS(file.path(processed_dir, "outputs", "index_outputs.rds"))
eo_idx <- io$economic_opportunity_index %>% rename(eo = Economic_Opportunity_Index)
eo_cc_all <- io$economic_opportunity_category_contributions
eo_vc_all <- io$economic_opportunity_variable_contributions
eo_diag <- io$economic_opportunity_outputs$diagnostics
eo_years <- eo_diag$year_provenance
eo_diag$year_provenance <- NULL
es_idx <- io$energy_security_index %>% rename(es = Energy_Security_Index)
es_cells <- io$energy_security_outputs$diagnostics$index_cells
coalition_tbl <- io$coalition_members %>% select(iso3, member_name = country, tier)
rm(io)
invisible(gc())

keys <- c("Country", "tech", "supply_chain")
eo_cc <- eo_cc_all %>% semi_join(eo_idx, by = keys)
eo_vc <- eo_vc_all %>% semi_join(eo_idx, by = keys)
rm(eo_cc_all, eo_vc_all)
eo_cells <- eo_diag$index_cells %>% semi_join(distinct(eo_idx, tech, supply_chain),
                                              by = c("tech", "supply_chain"))
# The index table carries the imputed weight share of the diagnostics (checked).
imp_chk <- eo_diag$index_imputation %>% inner_join(eo_idx, by = keys)
stopifnot(nrow(imp_chk) == nrow(eo_idx),
          max(abs(imp_chk$imputed_weight_share.x - imp_chk$imputed_weight_share.y)) < 1e-12,
          nrow(eo_cells) == n_distinct(eo_idx$tech, eo_idx$supply_chain),
          !anyNA(eo_idx$rankable))
rm(imp_chk)
# The rankability rule as configured: a rankable cell has at most tau of its weight imputed.
stopifnot(all(eo_idx$imputed_weight_share[eo_idx$rankable] <= tau + 1e-12))

info <- utils::read.csv(file.path(raw_dir, "wdi_country_info.csv"), stringsAsFactors = FALSE) %>%
  filter(region != "Aggregates") %>%
  select(iso3 = iso3c, income)
income_of <- function(iso3) {
  inc <- info$income[match(iso3, info$iso3)]
  inc[is.na(inc) & iso3 %in% "TWN"] <- "High income"
  inc[is.na(inc) | inc == "Not classified"] <- "Unclassified"
  inc
}
emerging_groups <- c("Low income", "Lower middle income", "Upper middle income")

ok_pairs <- eo_cells %>% filter(cell_status == "ok") %>% select(tech, supply_chain)
coalition_iso3 <- coalition_tbl$iso3
core_iso3 <- coalition_tbl$iso3[coalition_tbl$tier == "core"]
partner_iso3 <- coalition_tbl$iso3[coalition_tbl$tier == "partner"]
group_levels <- c("Core", "Partner", "China", "Other")

eo <- eo_idx %>%
  mutate(
    iso3 = country_iso3(Country),
    income = income_of(iso3),
    emerging = income %in% emerging_groups,
    coalition = iso3 %in% coalition_iso3,
    group = case_when(iso3 %in% core_iso3 ~ "Core", iso3 %in% partner_iso3 ~ "Partner",
                      iso3 == "CHN" ~ "China", TRUE ~ "Other"),
    stage = stage_factor(supply_chain),
    pair = paste(tech, supply_chain, sep = " | ")
  )
stopifnot(!anyNA(eo$iso3), all(coalition_iso3 %in% eo$iso3),
          length(core_iso3) + length(partner_iso3) == length(coalition_iso3))

# Rankable cells of ok pairs: the sample of every cross-country statistic below.
eo_r <- eo %>% filter(rankable) %>% semi_join(ok_pairs, by = c("tech", "supply_chain"))

# Within-pair ranks and percentiles over the rankable economies: rank 1 = highest EO.
eo_rank <- eo_r %>%
  group_by(tech, supply_chain) %>%
  mutate(n_pair = n(), rank = rank(-eo, ties.method = "min"), pct = pct_rank(eo)) %>%
  ungroup()
rkeys <- eo_rank %>% select(all_of(keys))

# ---------------------------------------------------------------------------
# Decomposition identities and the classification of inputs
# ---------------------------------------------------------------------------

recon_cat <- eo_cc %>% group_by(across(all_of(keys))) %>%
  summarise(s = sum(weighted_category_contribution)) %>% inner_join(eo_idx, by = keys)
recon_var <- eo_vc %>% group_by(across(all_of(keys))) %>%
  summarise(s = sum(weighted_variable_contribution),
            w = sum(category_weight * component_weight_within_overall)) %>%
  inner_join(eo_idx, by = keys)

vc <- eo_vc %>%
  mutate(eff_w = category_weight * component_weight_within_overall)
const_vars <- vc %>%
  filter(eff_w > 0) %>%
  group_by(tech, supply_chain, category, variable) %>%
  summarise(range = max(component_value) - min(component_value), n = n(),
            value = mean(component_value)) %>%
  ungroup() %>%
  mutate(constant = range <= const_tol)
vc <- vc %>%
  left_join(select(const_vars, tech, supply_chain, category, variable, constant),
            by = c("tech", "supply_chain", "category", "variable")) %>%
  mutate(
    constant = coalesce(constant, FALSE),
    input_type = case_when(
      constant ~ "Technology-level constant",
      imputed ~ "Imputed, country-varying",
      TRUE ~ "Observed, country-varying"
    )
  )

const_var_summary <- const_vars %>%
  group_by(category, variable) %>%
  summarise(n_pairs = n(), n_pairs_constant = sum(constant)) %>%
  ungroup() %>%
  arrange(match(category, category_order), variable)
constant_variables <- const_var_summary %>% filter(n_pairs_constant == n_pairs) %>% pull(variable)
mixed_variables <- const_var_summary %>%
  filter(n_pairs_constant > 0, n_pairs_constant < n_pairs) %>% pull(variable)

# TRL Index by pair (now mapped by stage from the IEA Clean Technology Guide): one value per
# pair, constant across economies.
trl_by_pair <- const_vars %>%
  filter(variable == "TRL Index") %>%
  select(tech, supply_chain, trl_component_value = value, constant) %>%
  arrange(tech, match(supply_chain, stages))
trl_stage_spread <- trl_by_pair %>% group_by(tech) %>%
  summarise(range_across_stages = max(trl_component_value) - min(trl_component_value)) %>%
  ungroup()

# Per country-cell: effective weight and level contribution by input type.
cell_split_all <- vc %>%
  group_by(across(all_of(keys)), input_type) %>%
  summarise(w = sum(eff_w), contrib = sum(weighted_variable_contribution)) %>%
  ungroup() %>%
  complete(nesting(Country, tech, supply_chain), input_type, fill = list(w = 0, contrib = 0)) %>%
  left_join(eo_idx, by = keys) %>%
  mutate(stage = stage_factor(supply_chain), level_share = contrib / eo)
cell_split <- cell_split_all %>% semi_join(rkeys, by = keys)

split_summary <- function(df, ...) {
  df %>% group_by(..., input_type) %>%
    summarise(mean_weight = mean(w), mean_level_share = mean(level_share), n_cells = n()) %>%
    ungroup()
}
weight_split_stage <- split_summary(cell_split, stage)
weight_split_all <- split_summary(cell_split)
weight_split_stage_scored <- split_summary(cell_split_all, stage)
weight_split_all_scored <- split_summary(cell_split_all)

# Mean effective weight of each constant variable, by stage (over the rankable cells).
n_cells_stage <- eo_rank %>% count(stage, name = "n_cells")
const_weight_by_var <- vc %>%
  semi_join(rkeys, by = keys) %>%
  filter(constant) %>%
  mutate(stage = stage_factor(supply_chain)) %>%
  group_by(stage, variable) %>%
  summarise(sum_w = sum(eff_w)) %>%
  ungroup() %>%
  left_join(n_cells_stage, by = "stage") %>%
  mutate(mean_weight = sum_w / n_cells) %>%
  select(stage, variable, mean_weight)

# Country-varying categories in each pair: those with at least one non-constant variable.
varying_cats <- vc %>%
  filter(!constant, eff_w > 0) %>%
  distinct(tech, supply_chain, category) %>%
  group_by(tech, supply_chain) %>%
  summarise(n_varying_categories = n(),
            varying_categories = paste(category[order(match(category, category_order))],
                                       collapse = "; ")) %>%
  ungroup()

# ---------------------------------------------------------------------------
# Coverage and rankability
# ---------------------------------------------------------------------------

cat_sets <- eo_cc %>%
  group_by(across(all_of(keys))) %>%
  summarise(k = n_distinct(category), set = paste(sort(unique(category)), collapse = "|")) %>%
  ungroup()
pairs_one_set <- cat_sets %>% group_by(tech, supply_chain) %>%
  summarise(n_sets = n_distinct(set)) %>% ungroup()
fully_imputed <- eo %>% filter(imputed_weight_share >= 1 - 1e-12)
stopifnot(all(eo_cc$category %in% category_order))
pair_category_sets <- eo_cc %>%
  group_by(tech, supply_chain) %>%
  summarise(
    n_economies = n_distinct(Country),
    categories = paste(category_order[category_order %in% category], collapse = "; "),
    zero_rule_categories = paste(
      category_order[category_order %in% category[missing_rule_applied %in% "zero"]], collapse = "; ")
  ) %>%
  ungroup() %>%
  arrange(match(supply_chain, stages), tech)

pair_rankable <- eo %>%
  group_by(tech, supply_chain) %>%
  summarise(n_scored = n(), n_rankable = sum(rankable),
            n_not_sovereign = sum(grepl("sovereign", rank_exclusion)),
            n_imputed_only = sum(rank_exclusion %in% "imputed weight share above 0.5"),
            mean_imputed_scored = mean(imputed_weight_share),
            mean_imputed_rankable = mean(imputed_weight_share[rankable])) %>%
  ungroup() %>%
  arrange(match(supply_chain, stages), n_rankable)

# Vintage mix: distinct selected years among a cell's index components (the variables of the
# variable decomposition) and among every recorded component, including the sub-indices of a
# composite such as the Input Cost Index (year_provenance of the pillar diagnostics).
cell_years <- function(df) {
  df %>% filter(!is.na(Year_selected)) %>%
    group_by(across(all_of(keys))) %>%
    summarise(n_years = n_distinct(Year_selected)) %>% ungroup()
}
years_hl <- eo_years %>% semi_join(eo_idx, by = keys)
yr_index <- cell_years(years_hl %>% semi_join(distinct(eo_vc, Country, tech, supply_chain, variable),
                                             by = c(keys, "variable")))
yr_all <- cell_years(years_hl)
year_mix <- list(
  n_cells_with_year_index_components = nrow(yr_index),
  share_cells_more_than_one_year_index_components = mean(yr_index$n_years > 1),
  max_distinct_years_index_components = max(yr_index$n_years),
  n_cells_with_year_all_components = nrow(yr_all),
  share_cells_more_than_one_year_all_components = mean(yr_all$n_years > 1),
  max_distinct_years_all_components = max(yr_all$n_years),
  note = paste("Selected year (latest recorded) per component from the pillar diagnostics",
               "year_provenance; index_components = variables of the variable decomposition;",
               "all_components adds sub-indices of composites. Every scored headline cell.")
)
rm(years_hl, yr_index, yr_all, eo_years)

excl_counts <- eo %>% mutate(reason = coalesce(rank_exclusion, "rankable")) %>%
  count(reason)
coverage <- list(
  n_cells_scored = nrow(eo),
  n_economies_scored = n_distinct(eo$Country),
  n_pairs = n_distinct(eo$pair),
  n_cells_rankable = nrow(eo_rank),
  share_cells_rankable = nrow(eo_rank) / nrow(eo),
  n_economies_rankable = n_distinct(eo_rank$Country),
  n_cells_not_rankable = sum(!eo$rankable),
  n_cells_territory = sum(grepl("sovereign", eo$rank_exclusion)),
  n_cells_imputed_only = sum(eo$rank_exclusion %in% "imputed weight share above 0.5"),
  n_economies_territory = n_distinct(eo$Country[grepl("sovereign", eo$rank_exclusion)]),
  rank_exclusion_counts = named_by(excl_counts, "reason", "n"),
  max_imputed_weight_share_tau = tau,
  n_cells_scored_by_stage = named_by(count(eo, supply_chain), "supply_chain", "n"),
  n_economies_scored_by_stage = named_by(eo %>% group_by(supply_chain) %>%
                                           summarise(n = n_distinct(Country)), "supply_chain", "n"),
  n_categories_per_cell_distribution = named_by(count(cat_sets, k) %>% mutate(k = as.character(k)),
                                                "k", "n"),
  n_cells_rankable_by_stage = named_by(count(eo_rank, supply_chain), "supply_chain", "n"),
  share_scored_cells_rankable_by_stage = named_by(
    eo %>% group_by(supply_chain) %>% summarise(s = mean(rankable)), "supply_chain", "s"),
  n_economies_rankable_by_stage = named_by(eo_rank %>% group_by(supply_chain) %>%
                                             summarise(n = n_distinct(Country)), "supply_chain", "n"),
  n_sovereign_economies_scored = n_distinct(eo$Country[!grepl("sovereign", eo$rank_exclusion)]),
  rankable_per_pair_min = min(pair_rankable$n_rankable),
  rankable_per_pair_max = max(pair_rankable$n_rankable),
  rankable_per_pair_median = stats::median(pair_rankable$n_rankable),
  pairs_fewest_rankable = records(pair_rankable %>% arrange(n_rankable) %>% slice_head(n = 5) %>%
                                    select(tech, supply_chain, n_scored, n_rankable)),
  rankable_by_pair = records(pair_rankable),
  scored_per_pair_min = min(pair_rankable$n_scored),
  scored_per_pair_max = max(pair_rankable$n_scored),
  pair_status_counts = named_by(count(eo_cells, cell_status), "cell_status", "n"),
  non_ok_pairs = with(filter(eo_cells, cell_status != "ok"), paste(tech, supply_chain)),
  n_pairs_ranked = nrow(ok_pairs),
  n_cells_fully_imputed = nrow(fully_imputed),
  fully_imputed_by_economy = named_by(count(fully_imputed, Country), "Country", "n"),
  mean_imputed_weight_share_scored = mean(eo$imputed_weight_share),
  mean_imputed_weight_share_scored_by_stage = named_by(
    eo %>% group_by(supply_chain) %>% summarise(m = mean(imputed_weight_share)), "supply_chain", "m"),
  mean_imputed_weight_share_rankable = mean(eo_rank$imputed_weight_share),
  mean_imputed_weight_share_rankable_by_stage = named_by(
    eo_rank %>% group_by(supply_chain) %>% summarise(m = mean(imputed_weight_share)),
    "supply_chain", "m"),
  share_category_scores_with_imputed_component_headline = mean(eo_cc$imputed),
  n_categories_per_cell_by_stage = records(cat_sets %>% group_by(supply_chain) %>%
                                             summarise(min = min(k), max = max(k))),
  n_categories_per_cell_by_stage_rankable = records(
    cat_sets %>% semi_join(rkeys, by = keys) %>% group_by(supply_chain) %>%
      summarise(min = min(k), max = max(k))),
  n_pairs_single_category_set = sum(pairs_one_set$n_sets == 1),
  category_sets_by_pair = records(pair_category_sets),
  trl_by_pair = records(trl_by_pair),
  trl_n_techs_varying_across_stages = sum(trl_stage_spread$range_across_stages > const_tol),
  trl_techs_constant_across_stages = trl_stage_spread$tech[trl_stage_spread$range_across_stages <= const_tol],
  trl_all_pairs_constant_across_economies = all(trl_by_pair$constant),
  trl_component_min = min(trl_by_pair$trl_component_value),
  trl_component_min_pair = with(trl_by_pair[which.min(trl_by_pair$trl_component_value), ],
                                paste(tech, supply_chain)),
  trl_component_max = max(trl_by_pair$trl_component_value),
  trl_component_max_pair = with(trl_by_pair[which.max(trl_by_pair$trl_component_value), ],
                                paste(tech, supply_chain)),
  trl_median_range_across_stages = stats::median(trl_stage_spread$range_across_stages),
  trl_stage_range_by_tech = named_by(trl_stage_spread, "tech", "range_across_stages"),
  year_mix = year_mix,
  max_abs_error_index_vs_category_contributions = max(abs(recon_cat$s - recon_cat$eo)),
  max_abs_error_index_vs_variable_contributions = max(abs(recon_var$s - recon_var$eo)),
  max_abs_error_effective_weights_sum_to_one = max(abs(recon_var$w - 1))
)

# What the rankability rule removes: where the excluded cells would sit if every scored economy
# were ranked (percentile among all scored economies of the pair).
eo_all_pct <- eo %>%
  semi_join(ok_pairs, by = c("tech", "supply_chain")) %>%
  group_by(tech, supply_chain) %>%
  mutate(pct_all = pct_rank(eo)) %>%
  ungroup() %>%
  mutate(status = case_when(rankable ~ "rankable",
                            grepl("sovereign", rank_exclusion) ~ "territory",
                            TRUE ~ "imputed share above tau"))
excluded_position <- eo_all_pct %>%
  group_by(status) %>%
  summarise(n_cells = n(), mean_eo = mean(eo), mean_pct_among_all_scored = mean(pct_all),
            share_below_pair_median = mean(pct_all < 0.5)) %>%
  ungroup()
# Top tens over all scored economies that the rule changes.
top10_all <- eo_all_pct %>% group_by(tech, supply_chain) %>%
  mutate(rank_all = rank(-eo, ties.method = "min")) %>% ungroup() %>% filter(rank_all <= 10)
rankability_effects <- list(
  position_of_cells_by_status = records(excluded_position),
  n_top10_slots_all_scored = nrow(top10_all),
  n_top10_slots_all_scored_not_rankable = sum(!top10_all$rankable),
  n_rank1_all_scored_not_rankable = sum(!top10_all$rankable & top10_all$rank_all == 1),
  note = paste("pct_all = within-pair percentile among all scored economies (ok pairs);",
               "status = rankable / territory (not sovereign) / imputed share above tau only.")
)

# Coverage of the clean-technology midstream pairs: the rule leaves the economies with
# recorded capacity or investment. Share of rankable economies with an observed (not
# zero-filled) Foreign Dependency or Investment input.
midstream_fd_pairs <- c("Batteries", "Solar", "Wind", "Green Hydrogen", "Electric Vehicles")
clean_mid_cov <- eo_cc %>%
  filter(supply_chain == "Midstream", tech %in% midstream_fd_pairs,
         category %in% c("Foreign Dependency", "Investment")) %>%
  group_by(Country, tech, supply_chain) %>%
  summarise(any_observed = any(!imputed)) %>%
  ungroup() %>%
  left_join(select(eo, all_of(keys), rankable), by = keys) %>%
  group_by(tech) %>%
  summarise(n_scored = n(), n_any_observed = sum(any_observed),
            n_rankable = sum(rankable),
            n_rankable_with_observed = sum(rankable & any_observed)) %>%
  ungroup()
rankability_effects$clean_midstream_fd_investment_coverage <- records(clean_mid_cov)

# ---------------------------------------------------------------------------
# Distribution and fixed-effects variance decomposition
# ---------------------------------------------------------------------------

by_stage <- lapply(setNames(stages, stages), function(s) moments(eo_rank$eo[eo_rank$supply_chain == s]))
by_stage_scored <- lapply(setNames(stages, stages), function(s) moments(eo$eo[eo$supply_chain == s]))
pair_stats <- eo_rank %>%
  group_by(tech, supply_chain) %>%
  summarise(n = n(), median = median(eo), p25 = q(eo, 0.25), p75 = q(eo, 0.75),
            iqr = q(eo, 0.75) - q(eo, 0.25), sd = sd(eo), min = min(eo), max = max(eo)) %>%
  ungroup() %>%
  arrange(match(supply_chain, stages), desc(median))
pair_median_range <- pair_stats %>%
  group_by(supply_chain) %>%
  summarise(lowest_pair = tech[which.min(median)], lowest_median = min(median),
            highest_pair = tech[which.max(median)], highest_median = max(median),
            mean_within_pair_sd = mean(sd), mean_iqr = mean(iqr),
            median_iqr = median(iqr)) %>%
  ungroup()

fe_decomp <- function(df, y) {
  df <- df %>% mutate(pair = factor(paste(tech, supply_chain)), tech = factor(tech),
                      stage = factor(supply_chain), Country = factor(Country))
  df$y <- df[[y]]
  rss <- function(f) sum(stats::residuals(stats::lm(f, data = df))^2)
  tss <- sum((df$y - mean(df$y))^2)
  rss_pair <- rss(y ~ pair)
  rss_two <- rss(y ~ pair + Country)
  list(
    n_cells = nrow(df), n_pairs = nlevels(df$pair), n_economies = nlevels(df$Country),
    total_variance = stats::var(df$y),
    r2_pair_fe = 1 - rss_pair / tss,
    r2_stage_fe = r2_of(y ~ stage, df),
    r2_tech_fe = r2_of(y ~ tech, df),
    r2_tech_plus_stage_fe = r2_of(y ~ tech + stage, df),
    r2_country_fe = r2_of(y ~ Country, df),
    r2_pair_plus_country_fe = 1 - rss_two / tss,
    within_pair_share_explained_by_country_fe = (rss_pair - rss_two) / rss_pair
  )
}
variance <- fe_decomp(eo_rank, "eo")
variance$all_scored <- fe_decomp(eo, "eo")
es_ok <- es_cells %>% filter(cell_status == "ok") %>% select(tech, supply_chain)
es_r <- es_idx %>% filter(rankable) %>% semi_join(es_ok, by = c("tech", "supply_chain"))
variance$es_comparison_rankable <- fe_decomp(es_r, "es")
variance$note <- paste(
  "R2 of OLS of the pillar on dummies, pooled over the rankable cells of the ok pairs of the",
  "headline technologies (all_scored: every scored cell); within_pair_share =",
  "(RSS_pair - RSS_pair+country) / RSS_pair. es_comparison_rankable: the same for ES over its",
  "rankable cells of pairs with status ok.")
rm(es_idx, es_r)

# ---------------------------------------------------------------------------
# Category contributions: share of the level and share of within-pair variance (rankable)
# ---------------------------------------------------------------------------

cc <- eo_cc %>%
  semi_join(rkeys, by = keys) %>%
  left_join(select(eo_idx, all_of(keys), eo), by = keys) %>%
  mutate(stage = stage_factor(supply_chain), level_share = weighted_category_contribution / eo)

level_share <- cc %>%
  group_by(stage, category) %>%
  summarise(sum_share = sum(level_share), sum_weight = sum(category_weight),
            n_cells_with = n()) %>%
  ungroup() %>%
  left_join(n_cells_stage, by = "stage") %>%
  mutate(mean_level_share = sum_share / n_cells, mean_effective_weight = sum_weight / n_cells) %>%
  select(stage, category, mean_level_share, mean_effective_weight, n_cells_with)

# var(EO) is taken over the rankable economies of the pair (one value each). Every economy of a
# pair carries the same category set (checked), so the covariance over the rows of a category is
# the covariance over the pair's economies and the shares sum to one within a pair.
stopifnot(all(pairs_one_set$n_sets == 1))
pair_var <- eo_rank %>%
  group_by(tech, supply_chain) %>%
  summarise(v = stats::var(eo), n_econ = n()) %>%
  ungroup()
var_share_pair <- cc %>%
  left_join(pair_var, by = c("tech", "supply_chain")) %>%
  group_by(tech, supply_chain, stage, category) %>%
  summarise(n_rows = n(), n_econ = first(n_econ),
            var_share = stats::cov(weighted_category_contribution, eo) / first(v)) %>%
  ungroup()
stopifnot(all(var_share_pair$n_rows == var_share_pair$n_econ))
stopifnot(all(abs(tapply(var_share_pair$var_share,
                         paste(var_share_pair$tech, var_share_pair$supply_chain), sum) - 1) < 1e-9))
n_pairs_stage <- eo_rank %>% distinct(stage, pair) %>% count(stage, name = "n_pairs")
var_share <- var_share_pair %>%
  group_by(stage, category) %>%
  summarise(sum_vs = sum(var_share)) %>%
  ungroup() %>%
  left_join(n_pairs_stage, by = "stage") %>%
  mutate(mean_var_share = sum_vs / n_pairs) %>%
  select(stage, category, mean_var_share)

# What widens the widest boxes of eo_distribution: per category, the IQR of EO across the
# pair's rankable economies when that category's contribution is replaced by its pair mean.
iqr_of <- function(x) q(x, 0.75) - q(x, 0.25)
widest_pairs <- pair_stats %>% arrange(desc(iqr)) %>% slice_head(n = 3) %>%
  select(tech, supply_chain)
widest_pair_iqr_sources <- cc %>%
  semi_join(widest_pairs, by = c("tech", "supply_chain")) %>%
  group_by(tech, supply_chain, category) %>%
  summarise(
    n_economies = n(),
    n_observed = sum(!imputed),
    n_zero_rule = sum(missing_rule_applied %in% "zero"),
    n_global_average_rule = sum(missing_rule_applied %in% "global_average"),
    n_nonzero_contribution = sum(weighted_category_contribution > 0),
    iqr_eo = iqr_of(eo),
    iqr_eo_with_category_at_pair_mean = iqr_of(eo - weighted_category_contribution +
                                                 mean(weighted_category_contribution)),
    var_share = stats::cov(weighted_category_contribution, eo) / stats::var(eo)
  ) %>%
  ungroup() %>%
  arrange(match(supply_chain, stages), tech, match(category, category_order))
# Production under the global-average rule: an economy without recorded production takes the
# producers' mean component values, so it sits above observed small producers. By pair: rankable
# economies with Production observed or filled, and their mean within-pair EO percentile.
production_coverage <- cc %>%
  filter(category == "Production") %>%
  left_join(select(eo_rank, all_of(keys), pct), by = keys) %>%
  group_by(tech, supply_chain) %>%
  summarise(n_economies = n(), n_observed = sum(!imputed),
            n_global_average_rule = sum(missing_rule_applied %in% "global_average"),
            mean_pct_observed = mean(pct[!imputed]),
            mean_pct_filled = if (any(imputed)) mean(pct[imputed]) else NA_real_,
            mean_category_score_observed = mean(category_score[!imputed]),
            category_score_filled = if (any(imputed)) mean(category_score[imputed]) else NA_real_,
            share_observed_below_fill = if (any(imputed)) mean(category_score[!imputed] < mean(category_score[imputed])) else NA_real_) %>%
  ungroup() %>%
  arrange(match(supply_chain, stages), tech)
investment_coverage <- cc %>%
  filter(category == "Investment") %>%
  group_by(tech, supply_chain) %>%
  summarise(n_economies = n(), n_observed = sum(!imputed),
            n_zero_rule = sum(missing_rule_applied %in% "zero")) %>%
  ungroup() %>%
  arrange(match(supply_chain, stages), tech)

contrib_tbl <- expand_grid(stage = stage_factor(stages), category = category_order) %>%
  left_join(level_share, by = c("stage", "category")) %>%
  left_join(var_share, by = c("stage", "category")) %>%
  mutate(across(c(mean_level_share, mean_effective_weight, mean_var_share, n_cells_with),
                ~ coalesce(.x, 0)))

top3 <- function(df, col) {
  df %>% group_by(stage) %>% arrange(desc(.data[[col]]), .by_group = TRUE) %>%
    slice_head(n = 3) %>% summarise(top = list(setNames(as.list(.data[[col]]), category))) %>%
    { setNames(.$top, as.character(.$stage)) }
}
cell_constant_categories <- c("Technology Demand", "Technological Readiness", "Energy Prices")
cat_const_weight <- contrib_tbl %>%
  filter(category %in% cell_constant_categories) %>%
  group_by(stage) %>%
  summarise(w = sum(mean_effective_weight), level = sum(mean_level_share),
            var = sum(mean_var_share)) %>%
  ungroup()
td_var_by_stage <- contrib_tbl %>% filter(category == "Technology Demand") %>%
  transmute(stage = as.character(stage), mean_var_share)

contributions <- list(
  by_stage_category = records(contrib_tbl %>% mutate(stage = as.character(stage))),
  top3_level_share_by_stage = top3(contrib_tbl, "mean_level_share"),
  top3_within_pair_variance_share_by_stage = top3(contrib_tbl, "mean_var_share"),
  combined_effective_weight_TD_TRL_EP_by_stage = named_by(
    cat_const_weight %>% mutate(stage = as.character(stage)), "stage", "w"),
  combined_level_share_TD_TRL_EP_by_stage = named_by(
    cat_const_weight %>% mutate(stage = as.character(stage)), "stage", "level"),
  combined_variance_share_TD_TRL_EP_by_stage = named_by(
    cat_const_weight %>% mutate(stage = as.character(stage)), "stage", "var"),
  technology_demand_variance_share_by_stage = named_by(td_var_by_stage, "stage", "mean_var_share"),
  widest_pair_iqr_sources = records(widest_pair_iqr_sources),
  investment_coverage_by_pair = records(investment_coverage),
  production_coverage_by_pair = records(production_coverage),
  investment_n_pairs = nrow(investment_coverage),
  investment_n_pairs_majority_zero_rule = sum(investment_coverage$n_zero_rule >
                                                investment_coverage$n_economies / 2),
  note = paste(
    "Rankable cells only. mean_level_share: mean over the rankable cells of the stage of",
    "weighted_category_contribution / EO (absent categories count 0).",
    "mean_effective_weight: same mean of the renormalised category weight.",
    "mean_var_share: mean over the 11 pairs of the stage of",
    "cov(category contribution, EO) / var(EO) across the rankable economies of the pair;",
    "the shares sum to 1 within a stage. widest_pair_iqr_sources: for the three pairs with the",
    "widest IQR of EO, the IQR when one category's contribution is replaced by its pair mean.",
    "investment_coverage_by_pair: rankable economies with Investment observed or zero-filled.")
)

input_weights <- list(
  constant_variables = constant_variables,
  variables_constant_in_some_pairs_only = mixed_variables,
  constancy_by_variable = records(const_var_summary),
  by_stage = records(weight_split_stage %>% mutate(stage = as.character(stage))),
  all_cells = records(weight_split_all),
  by_stage_all_scored = records(weight_split_stage_scored %>% mutate(stage = as.character(stage))),
  all_scored_cells = records(weight_split_all_scored),
  constant_weight_by_variable_and_stage = records(
    const_weight_by_var %>% mutate(stage = as.character(stage)) %>% arrange(stage, desc(mean_weight))),
  note = paste(
    "Effective weight of variable v in a country-cell = category_weight x",
    "component_weight_within_overall (sums to 1 per cell). Technology-level constant = zero range",
    "of component_value across the scored economies of the pair. Imputed = imputed flag of the",
    "variable contribution (missing-data rule applied). mean_level_share = mean of contribution /",
    "EO. by_stage / all_cells: rankable cells (the headline); *_all_scored: every scored cell.")
)

# ---------------------------------------------------------------------------
# Leaders and emerging economies (rankable cells)
# ---------------------------------------------------------------------------

leaders <- eo_rank %>%
  group_by(tech, supply_chain) %>%
  arrange(desc(eo), .by_group = TRUE) %>%
  summarise(
    leader = Country[1], leader_iso3 = iso3[1], leader_eo = eo[1],
    leader_income = income[1], leader_emerging = emerging[1], leader_coalition = coalition[1],
    leader_group = group[1],
    leader_imputed_weight_share = imputed_weight_share[1],
    runner_up = Country[2], runner_up_iso3 = iso3[2], runner_up_eo = eo[2],
    third = Country[3], third_iso3 = iso3[3], third_eo = eo[3],
    margin = eo[1] - eo[2], tied = isTRUE(all.equal(eo[1], eo[2], tolerance = 1e-12)),
    n_sharing_rank_top3 = sum(rank %in% rank[1:3]),
    best_coalition = Country[coalition][1], best_coalition_iso3 = iso3[coalition][1],
    best_coalition_tier = group[coalition][1],
    best_coalition_rank = rank[coalition][1], best_coalition_eo = eo[coalition][1],
    best_core = Country[group == "Core"][1], best_core_iso3 = iso3[group == "Core"][1],
    best_core_rank = rank[group == "Core"][1],
    usa_rank = if (any(iso3 == "USA")) rank[iso3 == "USA"][1] else NA_integer_,
    chn_rank = if (any(iso3 == "CHN")) rank[iso3 == "CHN"][1] else NA_integer_,
    usa_rank_n_tied = if (any(iso3 == "USA")) sum(rank == rank[iso3 == "USA"][1]) else NA_integer_,
    chn_rank_n_tied = if (any(iso3 == "CHN")) sum(rank == rank[iso3 == "CHN"][1]) else NA_integer_,
    best_coalition_rank_n_tied = sum(rank == rank[coalition][1]),
    n = n()
  ) %>%
  ungroup() %>%
  left_join(varying_cats, by = c("tech", "supply_chain")) %>%
  arrange(match(supply_chain, stages), tech)

# Cost-only pairs: Cost Competitiveness is the only country-varying category.
cost_only_pairs <- leaders %>% filter(varying_categories == "Cost Competitiveness")
leader_varying <- vc %>%
  semi_join(leaders %>% select(Country = leader, tech, supply_chain), by = keys) %>%
  filter(!constant) %>%
  group_by(Country, tech, supply_chain) %>%
  summarise(varying_weight = sum(eff_w), imputed_varying_weight = sum(eff_w[imputed])) %>%
  ungroup()
leaders <- leaders %>%
  left_join(leader_varying %>% rename(leader = Country), by = c("leader", "tech", "supply_chain"))

top10 <- eo_rank %>% filter(rank <= 10)
leader_summary <- list(
  n_pairs = nrow(leaders),
  n_leaders_emerging = sum(leaders$leader_emerging),
  n_leaders_china = sum(leaders$leader_iso3 == "CHN"),
  n_leaders_emerging_excluding_china = sum(leaders$leader_emerging & leaders$leader_iso3 != "CHN"),
  n_leaders_coalition = sum(leaders$leader_coalition),
  n_leaders_core = sum(leaders$leader_group == "Core"),
  n_leaders_partner = sum(leaders$leader_group == "Partner"),
  n_leaders_high_income = sum(leaders$leader_income == "High income"),
  leader_counts = named_by(count(leaders, leader, sort = TRUE), "leader", "n"),
  emerging_leaders_other_than_china = with(
    filter(leaders, leader_emerging, leader_iso3 != "CHN"),
    paste0(leader, " (", tech, " ", supply_chain, ")")),
  n_ties_at_top = sum(leaders$tied),
  cost_only_pairs = with(cost_only_pairs, paste(tech, supply_chain)),
  cost_only_pair_leaders = with(cost_only_pairs, paste0(leader, " (", tech, " ", supply_chain, ")")),
  cost_only_pair_runner_up = with(cost_only_pairs, paste0(runner_up, " (", tech, " ", supply_chain, ")")),
  cost_only_pair_margins = with(cost_only_pairs, setNames(as.list(margin), paste(tech, supply_chain))),
  share_top10_slots_emerging = mean(top10$emerging),
  share_top10_slots_emerging_excluding_china = mean(top10$emerging[top10$iso3 != "CHN"]),
  share_top10_slots_coalition = mean(top10$coalition),
  n_top10_slots = nrow(top10),
  n_pairs_china_in_top10 = sum(top10$iso3 == "CHN"),
  n_pairs_usa_in_top10 = sum(top10$iso3 == "USA"),
  n_pairs_china_rank1 = sum(leaders$leader_iso3 == "CHN"),
  median_margin_leader_over_second = median(leaders$margin),
  median_margin_when_china_leads = median(leaders$margin[leaders$leader_iso3 == "CHN"]),
  n_pairs_margin_below_0_01 = sum(leaders$margin < 0.01),
  pairs_margin_below_0_01 = with(filter(leaders, margin < 0.01), paste(tech, supply_chain)),
  largest_margins = records(leaders %>% arrange(desc(margin)) %>% slice_head(n = 5) %>%
                              select(tech, supply_chain, leader, runner_up, margin, n)),
  n_pairs_best_coalition_is_world_leader = sum(leaders$best_coalition_rank == 1),
  median_world_rank_best_coalition_member = median(leaders$best_coalition_rank),
  n_pairs_best_coalition_rank_le3 = sum(leaders$best_coalition_rank <= 3),
  pairs_best_coalition_rank_gt3 = with(filter(leaders, best_coalition_rank > 3),
                                       paste0(tech, " ", supply_chain, " (", best_coalition, ", ",
                                              best_coalition_rank, ")")),
  best_coalition_member_counts = named_by(count(leaders, best_coalition, sort = TRUE),
                                          "best_coalition", "n"),
  n_pairs_best_coalition_is_partner = sum(leaders$best_coalition_tier == "Partner"),
  median_world_rank_best_core_member = median(leaders$best_core_rank),
  emerging_definition = paste("World Bank income group (data/raw/wdi_country_info.csv) in",
                              paste(emerging_groups, collapse = ", "))
)

# Pairs in which the United States or China ranks in the bottom half of the pair's rankable
# economies: category scores and component values against their within-pair percentile among
# the rankable economies (1 = top), to explain the low ranks of Table tab:eo_leaders.
comp_pct <- vc %>%
  semi_join(rkeys, by = keys) %>%
  filter(eff_w > 0) %>%
  group_by(tech, supply_chain, category, variable) %>%
  mutate(component_pct = pct_rank(component_value)) %>%
  ungroup()
cat_pct_all <- eo_cc %>%
  semi_join(rkeys, by = keys) %>%
  group_by(tech, supply_chain, category) %>%
  mutate(category_pct = pct_rank(category_score),
         pair_median_category_score = stats::median(category_score)) %>%
  ungroup()
low_pairs_of <- function(code) {
  low <- eo_rank %>% filter(iso3 == code, rank > n_pair / 2) %>%
    select(all_of(keys), rank, n_pair, eo)
  list(
    pairs = records(low %>% arrange(match(supply_chain, stages), tech)),
    categories = records(cat_pct_all %>% semi_join(low, by = keys) %>%
                           select(tech, supply_chain, category, category_score, category_pct,
                                  pair_median_category_score, imputed) %>%
                           arrange(match(supply_chain, stages), tech,
                                   match(category, category_order))),
    components = records(comp_pct %>% semi_join(low, by = keys) %>%
                           filter(!constant) %>%
                           select(tech, supply_chain, category, variable, component_value,
                                  component_pct, imputed) %>%
                           arrange(match(supply_chain, stages), tech,
                                   match(category, category_order), variable))
  )
}
low_rank_detail <- list(
  united_states = low_pairs_of("USA"),
  china = low_pairs_of("CHN"),
  note = paste("Pairs with rank > n/2 among the rankable economies. category_pct /",
               "component_pct: within-pair percentile among the rankable economies, 1 = top.",
               "Components: country-varying variables only.")
)
rm(comp_pct, cat_pct_all)

# ---------------------------------------------------------------------------
# Coalition (rankable cells)
# ---------------------------------------------------------------------------

grp_summary <- function(df, ...) {
  df %>% group_by(...) %>%
    summarise(mean_eo = mean(eo), mean_pct = mean(pct), mean_imputed = mean(imputed_weight_share),
              n_cells = n(), n_economies = n_distinct(Country)) %>%
    ungroup()
}
eo_rank_g <- eo_rank %>% mutate(group = factor(group, levels = group_levels))
coalition_stage <- grp_summary(eo_rank_g, supply_chain, group) %>%
  arrange(match(supply_chain, stages), group)
coalition_all <- grp_summary(eo_rank_g, group) %>% arrange(group)
coalition_pooled <- grp_summary(eo_rank_g %>% mutate(member = coalition), member)
coalition_pooled_stage <- grp_summary(eo_rank_g %>% filter(coalition), supply_chain)
# Share of the members' scored cells that are rankable, by tier.
member_rankable <- eo %>% filter(coalition) %>% group_by(group) %>%
  summarise(n_cells = n(), n_rankable = sum(rankable), share_rankable = mean(rankable),
            n_excluded_not_sovereign = sum(grepl("sovereign", rank_exclusion)),
            n_excluded_imputed_share = sum(rank_exclusion %in% "imputed weight share above 0.5"),
            mean_imputed_scored = mean(imputed_weight_share)) %>% ungroup()

# Within-pair percentile of each category score (same definition as pct), for the cost link.
cat_pct <- eo_cc %>%
  semi_join(rkeys, by = keys) %>%
  group_by(tech, supply_chain, category) %>%
  mutate(cat_pct = pct_rank(category_score)) %>%
  ungroup()
cost_pct <- cat_pct %>%
  filter(category == "Cost Competitiveness") %>%
  group_by(Country) %>%
  summarise(mean_pct_cost = mean(cat_pct)) %>%
  ungroup()
econ_pct <- eo_rank %>% group_by(Country, iso3) %>% summarise(mean_pct = mean(pct), n_cells = n()) %>%
  ungroup() %>% inner_join(cost_pct, by = "Country")

# Recorded nominal interest rate behind the capital-cost leg of the Input Cost Index
# (cost_competitiveness theme); rate_index is the winsorised min-max capital-cost input R_i
# (1 = at the ceiling); wage_pct is the economy's percentile of recorded monthly earnings (USD)
# among reporting economies, mean over the three stages (1 = highest wages).
cost_theme <- readRDS(file.path(processed_dir, "cost_competitiveness_tbl.rds")) %>%
  filter(variable %in% c("nominal_rate", "rate_index", "earnings_usd")) %>%
  select(Country, tech, supply_chain, variable, value, Year)
rate_index_tbl <- cost_theme %>%
  filter(variable == "rate_index", !is.na(value)) %>%
  group_by(Country) %>% summarise(rate_index = first(value)) %>% ungroup()
wage_pct_tbl <- cost_theme %>%
  filter(variable == "earnings_usd", !is.na(value)) %>%
  distinct(Country, supply_chain, value) %>%
  group_by(supply_chain) %>%
  mutate(p = (rank(value, ties.method = "average") - 1) / (n() - 1)) %>%
  group_by(Country) %>% summarise(wage_pct = mean(p)) %>% ungroup()
rate_tbl <- cost_theme %>%
  filter(variable == "nominal_rate", !is.na(value)) %>%
  distinct(Country, value, Year) %>%
  group_by(Country) %>% arrange(desc(Year), .by_group = TRUE) %>% slice_head(n = 1) %>%
  ungroup() %>%
  rename(nominal_rate = value, rate_year = Year)
rm(cost_theme)
rate_rank <- rate_tbl %>%
  mutate(rate_rank_low_to_high = rank(nominal_rate, ties.method = "min"), n_rates = n()) %>%
  left_join(rate_index_tbl, by = "Country") %>%
  left_join(wage_pct_tbl, by = "Country")

member_tbl <- eo %>%
  filter(coalition | iso3 == "CHN") %>%
  group_by(Country, iso3, group, income) %>%
  summarise(n_scored = n(), n_rankable = sum(rankable),
            mean_imputed_scored = mean(imputed_weight_share)) %>%
  ungroup() %>%
  left_join(
    eo_rank %>%
      filter(coalition | iso3 == "CHN") %>%
      group_by(Country) %>%
      summarise(
        mean_pct = mean(pct), median_rank = median(rank), n_top10 = sum(rank <= 10),
        n_rank1 = sum(rank == 1),
        mean_pct_up = if (any(supply_chain == "Upstream")) mean(pct[supply_chain == "Upstream"]) else NA_real_,
        mean_pct_mid = if (any(supply_chain == "Midstream")) mean(pct[supply_chain == "Midstream"]) else NA_real_,
        mean_pct_down = if (any(supply_chain == "Downstream")) mean(pct[supply_chain == "Downstream"]) else NA_real_,
        best_pair = pair[which.max(pct)], best_pair_rank = min(rank),
        mean_imputed_rankable = mean(imputed_weight_share)
      ) %>% ungroup(),
    by = "Country") %>%
  left_join(cost_pct, by = "Country") %>%
  left_join(rate_rank %>% select(Country, nominal_rate, rate_year, rate_rank_low_to_high, n_rates,
                                 rate_index, wage_pct), by = "Country") %>%
  arrange(desc(mean_pct))

# Counterfactual mean percentile of the members and China when one category's contribution is
# replaced by its pair mean over the rankable economies (absent = 0), all other economies
# unchanged: how far that category moves an economy's position.
cf_member_pct <- function(category_name) {
  contrib <- cc %>%
    filter(category == category_name) %>%
    select(all_of(keys), contrib = weighted_category_contribution)
  eo_rank %>%
    select(all_of(keys), iso3, coalition, eo, pct) %>%
    left_join(contrib, by = keys) %>%
    mutate(contrib = coalesce(contrib, 0)) %>%
    group_by(tech, supply_chain) %>%
    mutate(
      is_member = coalition | iso3 == "CHN",
      x_cf = eo - contrib + mean(contrib),
      pct_cf = vapply(seq_len(n()), function(i) {
        if (!is_member[i]) return(NA_real_)
        (sum(eo[-i] < x_cf[i]) + 0.5 * sum(eo[-i] == x_cf[i])) / (n() - 1)
      }, numeric(1))
    ) %>%
    ungroup() %>%
    filter(is_member) %>%
    group_by(Country) %>%
    summarise(mean_pct = mean(pct), mean_pct_cf = mean(pct_cf)) %>%
    ungroup()
}
member_cf <- cf_member_pct("Cost Competitiveness") %>%
  rename(mean_pct_cost_at_pair_mean = mean_pct_cf) %>%
  inner_join(cf_member_pct("Technology Demand") %>%
               select(Country, mean_pct_demand_at_pair_mean = mean_pct_cf), by = "Country") %>%
  arrange(desc(mean_pct))
stopifnot(all(abs(member_cf$mean_pct - member_tbl$mean_pct[match(member_cf$Country,
                                                                  member_tbl$Country)]) < 1e-12))

# The BNEF regional composite of Technology Demand ("Overall Demand" outside electric vehicles,
# where the EV-market composite carries the same name) is observed for few economies; the
# global-average rule gives every other economy their mean. Which economies are observed, and
# in how many pairs their value lies below that fill value (rankable cells).
bnef <- vc %>%
  semi_join(rkeys, by = keys) %>%
  filter(category == "Technology Demand", variable == "Overall Demand", tech != "Electric Vehicles")
bnef_fill <- bnef %>%
  group_by(tech, supply_chain) %>%
  summarise(n_observed = sum(!imputed), n_economies = n(),
            fill_value = if (any(imputed)) mean(component_value[imputed]) else NA_real_,
            fill_range = if (any(imputed)) diff(range(component_value[imputed])) else 0) %>%
  ungroup()
stopifnot(all(bnef_fill$fill_range < 1e-12))
bnef_obs <- bnef %>%
  filter(!imputed) %>%
  left_join(select(bnef_fill, tech, supply_chain, fill_value), by = c("tech", "supply_chain")) %>%
  group_by(Country) %>%
  summarise(n_pairs_observed = n(), n_pairs_below_fill = sum(component_value < fill_value)) %>%
  ungroup() %>%
  arrange(desc(n_pairs_observed))
bnef_summary <- list(
  n_pairs_with_composite = nrow(bnef_fill),
  max_economies_observed_in_a_pair = max(bnef_fill$n_observed),
  observed_economies = records(bnef_obs)
)

# Do economies that rank well in one stage rank well in the others? Spearman across economies
# of the mean within-pair percentile by stage (economies rankable in all three stages).
stage_pct <- eo_rank %>%
  group_by(Country, supply_chain) %>% summarise(m = mean(pct)) %>% ungroup() %>%
  pivot_wider(names_from = supply_chain, values_from = m) %>% tidyr::drop_na()
stage_rank_cor <- list(
  n_economies = nrow(stage_pct),
  upstream_midstream = spearman(stage_pct$Upstream, stage_pct$Midstream),
  upstream_downstream = spearman(stage_pct$Upstream, stage_pct$Downstream),
  midstream_downstream = spearman(stage_pct$Midstream, stage_pct$Downstream)
)
# Within-pair Spearman between EO and the imputed weight share (rankable economies).
imp_cor <- eo_rank %>% group_by(tech, supply_chain) %>%
  summarise(rho = if (stats::sd(imputed_weight_share) > 0) spearman(eo, imputed_weight_share) else NA_real_) %>%
  ungroup()

core_names <- member_tbl %>% filter(group == "Core") %>% arrange(desc(mean_pct)) %>% pull(Country)
partner_names <- member_tbl %>% filter(group == "Partner") %>% arrange(desc(mean_pct)) %>% pull(Country)
coalition <- list(
  n_members = length(coalition_iso3), n_core = length(core_iso3), n_partner = length(partner_iso3),
  core_iso3 = core_iso3, partner_iso3 = partner_iso3,
  by_stage_and_group = records(coalition_stage),
  all_cells_by_group = records(coalition_all),
  members_pooled_vs_rest = records(coalition_pooled),
  members_pooled_by_stage = records(coalition_pooled_stage),
  members_rankable_cells_by_tier = records(member_rankable),
  members_ranked_by_mean_percentile = records(member_tbl),
  members_mean_percentile_with_category_at_pair_mean = records(member_cf),
  members_rate_index_at_ceiling = member_tbl %>%
    filter(group %in% c("Core", "Partner"), !is.na(rate_index), rate_index >= 1 - 1e-9) %>%
    pull(Country),
  members_cost_neutral_gain_above_0_2 = member_cf %>%
    filter(Country %in% member_tbl$Country[member_tbl$group %in% c("Core", "Partner")],
           mean_pct_cost_at_pair_mean - mean_pct > 0.2) %>%
    arrange(desc(mean_pct_cost_at_pair_mean - mean_pct)) %>% pull(Country),
  bnef_regional_demand_composite = bnef_summary,
  top5_core_by_mean_percentile = head(core_names, 5),
  bottom5_core_by_mean_percentile = tail(core_names, 5),
  top5_partner_by_mean_percentile = head(partner_names, 5),
  bottom5_partner_by_mean_percentile = tail(partner_names, 5),
  members_with_fewest_rankable_cells = records(member_tbl %>% filter(group != "China") %>%
                                                 arrange(n_rankable) %>% slice_head(n = 8) %>%
                                                 select(Country, group, n_rankable, mean_imputed_scored)),
  spearman_stage_mean_percentiles_across_economies = stage_rank_cor,
  spearman_eo_vs_imputed_share_within_pair_median = stats::median(imp_cor$rho, na.rm = TRUE),
  spearman_eo_vs_imputed_share_within_pair_range = range(imp_cor$rho, na.rm = TRUE),
  spearman_eo_vs_imputed_share_n_pairs = sum(!is.na(imp_cor$rho)),
  spearman_mean_eo_pct_vs_mean_cost_pct_across_economies = spearman(econ_pct$mean_pct,
                                                                    econ_pct$mean_pct_cost),
  spearman_mean_eo_pct_vs_mean_cost_pct_n_economies = nrow(econ_pct),
  spearman_mean_eo_pct_vs_mean_cost_pct_coalition = with(
    filter(econ_pct, iso3 %in% coalition_iso3), spearman(mean_pct, mean_pct_cost)),
  spearman_mean_eo_pct_vs_mean_cost_pct_coalition_n = sum(econ_pct$iso3 %in% coalition_iso3),
  nominal_rates_selected = records(rate_rank %>%
    filter(Country %in% c(member_tbl$Country, leaders$leader, cost_only_pairs$runner_up))),
  note = paste("Rankable cells only. pct = within-pair percentile rank among the rankable",
               "economies, (n - average rank) / (n - 1), 1 = top; means are over rankable",
               "country-cells. Groups: Core / Partner (coalition_members tier), China, Other.",
               "mean_pct_cost = same percentile of the Cost Competitiveness category score.",
               "nominal_rate = the rate recorded in cost_competitiveness_tbl.rds (per cent) and",
               "its year; rate_rank_low_to_high ranks it among all economies with a recorded rate.")
)

# ---------------------------------------------------------------------------
# Illustrative pairs (eo_top)
# ---------------------------------------------------------------------------

top_groups <- c("Technology-level constants", "Trade", "Production", "Technology Demand",
                "Investment", "Foreign Dependency", "Cost Competitiveness")
top_eo <- eo_rank %>%
  semi_join(top_cells, by = c("tech", "supply_chain")) %>%
  filter(rank <= 10)
stopifnot(all(count(top_eo, tech, supply_chain)$n == 10))
top_contrib <- vc %>%
  semi_join(top_eo, by = keys) %>%
  mutate(seg = if_else(constant, "Technology-level constants", category)) %>%
  group_by(across(all_of(keys)), seg) %>%
  summarise(contrib = sum(weighted_variable_contribution),
            contrib_imputed = sum(weighted_variable_contribution[imputed & !constant])) %>%
  ungroup()
stopifnot(all(top_contrib$seg %in% top_groups))
upstream_choice <- pair_rankable %>% filter(supply_chain == "Upstream") %>%
  arrange(desc(n_rankable)) %>% select(tech, n_rankable, mean_imputed_rankable)

top_detail <- top_eo %>%
  left_join(top_contrib %>% select(-contrib_imputed) %>%
              pivot_wider(names_from = seg, values_from = contrib, values_fill = 0), by = keys) %>%
  arrange(match(paste(tech, supply_chain), paste(top_cells$tech, top_cells$supply_chain)), rank)
top_const <- top_contrib %>% filter(seg == "Technology-level constants") %>%
  group_by(tech, supply_chain) %>%
  summarise(const_min = min(contrib), const_max = max(contrib)) %>% ungroup()
top_segment_span <- top_detail %>%
  select(tech, supply_chain, Country, any_of(top_groups)) %>%
  pivot_longer(-c(tech, supply_chain, Country), names_to = "segment", values_to = "contrib") %>%
  group_by(tech, supply_chain, segment) %>%
  summarise(min = min(contrib), max = max(contrib), span = max(contrib) - min(contrib),
            largest = Country[which.max(contrib)]) %>%
  ungroup() %>%
  arrange(match(paste(tech, supply_chain), paste(top_cells$tech, top_cells$supply_chain)),
          desc(span))
top_imputed <- top_contrib %>% group_by(tech, supply_chain) %>%
  summarise(max_imputed_contrib = max(contrib_imputed)) %>% ungroup()

illustrative <- list(
  pairs = paste(top_cells$tech, top_cells$supply_chain),
  n_rankable_in_pair = named_by(pair_rankable %>% semi_join(top_cells, by = c("tech", "supply_chain")) %>%
                                  mutate(p = paste(tech, supply_chain)), "p", "n_rankable"),
  n_scored_in_pair = named_by(pair_rankable %>% semi_join(top_cells, by = c("tech", "supply_chain")) %>%
                                mutate(p = paste(tech, supply_chain)), "p", "n_scored"),
  upstream_pairs_by_n_rankable = records(upstream_choice),
  top10 = records(top_detail %>% select(tech, supply_chain, rank, Country, iso3, eo, group,
                                        emerging, imputed_weight_share, any_of(top_groups))),
  constant_segment_range = records(top_const),
  segment_span_in_top10 = records(top_segment_span),
  imputed_country_varying_contribution_max = records(top_imputed),
  n_coalition_in_top10 = named_by(top_eo %>% mutate(p = paste(tech, supply_chain)) %>%
                                    group_by(p) %>% summarise(n = sum(coalition)), "p", "n"),
  n_core_in_top10 = named_by(top_eo %>% mutate(p = paste(tech, supply_chain)) %>%
                               group_by(p) %>% summarise(n = sum(group == "Core")), "p", "n"),
  n_emerging_in_top10 = named_by(top_eo %>% mutate(p = paste(tech, supply_chain)) %>%
                                   group_by(p) %>% summarise(n = sum(emerging)), "p", "n")
)

# ---------------------------------------------------------------------------
# Write the numbers
# ---------------------------------------------------------------------------

write_paper_results(list(
  source = paste("data/processed/outputs/index_outputs.rds (economic_opportunity_*),",
                 "final rerun of 2026-10-12"),
  sample = paste("11 headline technologies x 3 stages; cross-country statistics over rankable",
                 "cells (config/rankability.yml) of pairs with index_cells status ok"),
  coverage = coverage,
  rankability_effects = rankability_effects,
  distribution_by_stage = by_stage,
  distribution_by_stage_all_scored = by_stage_scored,
  distribution_by_pair = records(pair_stats),
  pair_median_range_by_stage = records(pair_median_range),
  variance_decomposition = variance,
  contributions = contributions,
  input_weights = input_weights,
  leaders = records(leaders),
  leader_summary = leader_summary,
  low_rank_detail_usa_china = low_rank_detail,
  coalition = coalition,
  illustrative_pairs = illustrative
), "eo")

# ---------------------------------------------------------------------------
# Tables: paper/tables/eo_leaders.tex and paper/tables/eo_weight_split.tex, written from the
# same objects as eo.json so that their numbers cannot drift from it.
# ---------------------------------------------------------------------------

tex_int <- function(n) gsub(",", "{,}", format(n, big.mark = ",", trim = TRUE), fixed = TRUE)
tables_dir <- paper_dir("tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

lead_tbl <- as.data.frame(leaders)
code_names <- bind_rows(
  tibble(code = lead_tbl$leader_iso3, name = lead_tbl$leader),
  tibble(code = lead_tbl$runner_up_iso3, name = lead_tbl$runner_up),
  tibble(code = lead_tbl$third_iso3, name = lead_tbl$third),
  tibble(code = lead_tbl$best_coalition_iso3, name = lead_tbl$best_coalition)
) %>% distinct(code, .keep_all = TRUE) %>%
  filter(!code %in% c("USA", "CHN")) %>% arrange(code) %>%
  mutate(name = sub("Democratic Republic of Congo", "DR Congo", name, fixed = TRUE),
         name = sub("Turkiye", "T\\\"urkiye", name, fixed = TRUE))
f2 <- function(v) sprintf("%.2f", v)
lead_lines <- c(
  "% Written by scripts/paper/fig_eo.R from the objects saved in paper/results/eo.json (key: leaders).",
  "\\begin{table}[tbp]", "\\centering", "\\begin{threeparttable}",
  "\\caption{Leading economies in each technology-stage pair of the Economic Opportunity pillar (rankable cells)}",
  "\\label{tab:eo_leaders}", "\\footnotesize",
  "\\begin{tabular}{@{}lrlllcrr@{}}", "\\toprule",
  "Technology & $n$ & First & Second & Third & Best member (rank) & USA & CHN \\\\", "\\midrule"
)
for (si in seq_along(stages)) {
  if (si > 1) lead_lines <- c(lead_lines, "\\addlinespace")
  lead_lines <- c(lead_lines, sprintf("\\multicolumn{8}{@{}l}{\\textit{%s}}\\\\", stages[si]))
  rows <- lead_tbl %>% filter(supply_chain == stages[si]) %>% arrange(tech)
  lead_lines <- c(lead_lines, sprintf(
    "%s & %d & %s %s & %s %s & %s %s & %s (%d) & %d & %d \\\\",
    rows$tech, rows$n, rows$leader_iso3, f2(rows$leader_eo), rows$runner_up_iso3,
    f2(rows$runner_up_eo), rows$third_iso3, f2(rows$third_eo), rows$best_coalition_iso3,
    rows$best_coalition_rank, rows$usa_rank, rows$chn_rank))
}
lead_lines <- c(
  lead_lines, "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} $\\EO_{its}$ for the eleven headline technologies. Only rankable cells enter: ",
    "a sovereign economy with at most half of the cell's index weight imputed ",
    "(\\texttt{config/rankability.yml}). Economies are identified by ISO3 code and followed by their score; ",
    sprintf("$n$ is the number of rankable economies in the pair (%d to %d). ",
            min(lead_tbl$n), max(lead_tbl$n)),
    sprintf("``Best member'' is the highest-ranked of the %d coalition members (%d core, %d partners), ",
            coalition$n_members, coalition$n_core, coalition$n_partner),
    "with its rank in parentheses; the USA and CHN columns give the ranks of the United States and China. ",
    "A rank is one plus the number of rankable economies with a strictly higher score. ",
    "All 33 pairs are scored and none is constant across economies. In geothermal upstream and midstream ",
    "and electric-grid downstream, Cost Competitiveness is the only category that varies across economies. ",
    "Codes: ", paste(sprintf("%s %s", code_names$code, code_names$name), collapse = ", "),
    ", USA United States, CHN China. ",
    "Source: \\texttt{economic\\_opportunity\\_index} in \\texttt{data/processed/outputs/index\\_outputs.rds}; ",
    "\\texttt{scripts/paper/fig\\_eo.R}."
  ),
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
)
writeLines(lead_lines, file.path(tables_dir, "eo_leaders.tex"), useBytes = TRUE)

split_row <- function(df) {
  g <- function(type, col) 100 * df[[col]][df$input_type == type]
  sprintf("%.1f & %.1f & %.1f & %.1f",
          g("Technology-level constant", "mean_weight"), g("Imputed, country-varying", "mean_weight"),
          g("Observed, country-varying", "mean_weight"),
          g("Technology-level constant", "mean_level_share"))
}
iw_stage <- as.data.frame(weight_split_stage %>% mutate(stage = as.character(stage)))
nr_stage <- coverage$n_cells_rankable_by_stage
split_lines <- c(
  "% Written by scripts/paper/fig_eo.R from the objects saved in paper/results/eo.json (keys: input_weights, coverage).",
  "\\begin{table}[tbp]", "\\centering", "\\begin{threeparttable}",
  "\\caption{Effective weight of the Economic Opportunity pillar by type of input}",
  "\\label{tab:eo_weight_split}", "\\small",
  "\\begin{tabular}{@{}l S[table-format=2.1] S[table-format=2.1] S[table-format=2.1] S[table-format=2.1]@{}}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Mean effective weight (per cent)} & {Constants' share}\\\\",
  "\\cmidrule(lr){2-4}",
  " & {Technology-level} & {Imputed,} & {Observed,} & {of the level}\\\\",
  " & {constants} & {country-varying} & {country-varying} & {(per cent)}\\\\",
  "\\midrule",
  vapply(stages, function(st) sprintf("%s & %s\\\\", st, split_row(iw_stage[iw_stage$stage == st, ])),
         character(1)),
  "\\addlinespace",
  sprintf("All rankable cells & %s\\\\", split_row(as.data.frame(weight_split_all))),
  sprintf("All scored cells & %s\\\\", split_row(as.data.frame(weight_split_all_scored))),
  "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} The effective weight of variable $v$ in cell $(i,t,s)$ is the renormalised ",
    "category weight times the variable's weight within its category, $\\omega_{kits}\\lambda_{vkits}$ ",
    "(equation~\\eqref{eq:var_decomp}); it sums to one in every cell. Technology-level constants are the ",
    "variables whose value has zero range across all scored economies of the pair: the TRL Index (one value ",
    "per technology and stage), the WEO global-demand composite, the BCG addressable market, the overcapacity ",
    "index, the world-market concentration term of Trade and commodity-price volatility. Imputed components ",
    "are those filled by a missing-data rule (global average or zero) for the economy in question. The first ",
    "three columns sum to 100 within a row, up to rounding. The last column is the mean share of $\\EO_{its}$ ",
    "contributed by the constants. Rows by stage and ``All rankable cells'': means over the ",
    sprintf("%s upstream, %s midstream and %s downstream rankable cells ",
            tex_int(nr_stage[["Upstream"]]), tex_int(nr_stage[["Midstream"]]),
            tex_int(nr_stage[["Downstream"]])),
    "of the eleven headline technologies (a sovereign economy with at most half of the cell's weight ",
    sprintf("imputed); ``All scored cells'': means over all %s scored cells. ",
            tex_int(coverage$n_cells_scored)),
    "Source: \\texttt{economic\\_opportunity\\_variable\\_contributions} in ",
    "\\texttt{data/processed/outputs/index\\_outputs.rds}; \\texttt{scripts/paper/fig\\_eo.R}."
  ),
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
)
writeLines(split_lines, file.path(tables_dir, "eo_weight_split.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------
# Figure: eo_distribution
# ---------------------------------------------------------------------------

dist_df <- eo_rank %>%
  left_join(pair_stats %>% select(tech, supply_chain, median, n_r = n), by = c("tech", "supply_chain")) %>%
  mutate(stage = stage_factor(supply_chain))
y_tbl <- dist_df %>% distinct(stage, tech, median, n_r) %>%
  arrange(stage, median) %>%
  mutate(key = paste(tech, stage, sep = "___"), lab = paste0(tech, " (", n_r, ")"))
dist_df <- dist_df %>% mutate(y = factor(paste(tech, stage, sep = "___"), levels = y_tbl$key))
y_lab <- setNames(y_tbl$lab, y_tbl$key)
# Axis range: the rankable scores span roughly 0.2 to 0.8; even tenths keep the ticks legible.
dist_lim <- c(floor(min(dist_df$eo) * 5) / 5, ceiling(max(dist_df$eo) * 5) / 5)

p_dist <- ggplot(dist_df, aes(x = eo, y = y)) +
  geom_boxplot(aes(colour = stage, fill = stage), width = 0.62, linewidth = 0.3,
               outlier.size = 0.7, outlier.stroke = 0, outlier.alpha = 0.8, staplewidth = 0) +
  facet_wrap(~stage, nrow = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) unname(y_lab[x])) +
  scale_colour_manual(values = paper_stage_cols, guide = "none") +
  scale_fill_manual(values = scales::alpha(paper_stage_cols, 0.22), guide = "none") +
  scale_x_continuous(limits = dist_lim, breaks = seq(dist_lim[1], dist_lim[2], 0.2),
                     labels = scales::label_number(accuracy = 0.1),
                     expand = expansion(mult = 0.02)) +
  labs(x = "Economic Opportunity score (rankable cells)", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(10, "pt"), axis.line.x = element_line(colour = paper_ink$axis, linewidth = 0.3))
save_paper_fig(p_dist, "eo_distribution", width = paper_width_full, height = 3.3)

# ---------------------------------------------------------------------------
# Figure: eo_contrib
# ---------------------------------------------------------------------------

wholly_constant_cats <- vc %>% group_by(category) %>% summarise(all_const = all(constant)) %>%
  filter(all_const) %>% pull(category)
cat_label <- function(x) ifelse(x %in% wholly_constant_cats, paste0(x, "*"), x)
contrib_long <- bind_rows(
  contrib_tbl %>% transmute(stage, category, value = mean_level_share,
                            measure = "Share of the pillar level"),
  contrib_tbl %>% transmute(stage, category, value = mean_var_share,
                            measure = "Share of within-pair variance")
) %>%
  mutate(
    measure = factor(measure, levels = c("Share of the pillar level",
                                         "Share of within-pair variance")),
    category = factor(cat_label(category), levels = rev(cat_label(category_order)))
  )
x_max <- max(contrib_long$value)

p_contrib <- ggplot(contrib_long, aes(x = value, y = category, fill = stage)) +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_col(position = position_dodge2(reverse = TRUE, padding = 0.12), width = 0.78) +
  facet_wrap(~measure, nrow = 1) +
  scale_fill_manual(values = paper_stage_cols, name = NULL) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0, ceiling(x_max * 10) / 10, 0.1),
                     expand = expansion(mult = c(0.02, 0.04))) +
  labs(x = "Mean share within the stage (over rankable cells for the level, over pairs for the variance)",
       y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(24, "pt"), axis.line.x = element_blank(),
        legend.position = "bottom")
save_paper_fig(p_contrib, "eo_contrib", width = paper_width_full, height = 3.9)

# ---------------------------------------------------------------------------
# Figure: eo_heatmap
# ---------------------------------------------------------------------------

panel_lab <- c(Core = paste0("Core (", length(core_iso3), ")"),
               Partner = paste0("Partners (", length(partner_iso3), ")"),
               China = "Reference")
row_order <- member_tbl %>% arrange(group, mean_pct) %>% pull(Country)
short_name <- function(x) ifelse(x %in% names(name_short), name_short[x], x)
hm <- eo %>%
  filter(coalition | iso3 == "CHN") %>%
  left_join(select(eo_rank, all_of(keys), pct), by = keys) %>%
  mutate(
    stage = stage_factor(supply_chain),
    tech_lab = factor(unname(tech_short[tech]), levels = unname(tech_short)),
    row = factor(Country, levels = row_order),
    panel = factor(unname(panel_lab[group]), levels = unname(panel_lab))
  )
stopifnot(all(is.na(hm$pct) == !hm$rankable))
p_hm <- ggplot(hm, aes(x = tech_lab, y = row)) +
  geom_tile(data = filter(hm, rankable), aes(fill = pct), colour = "white", linewidth = 0.4) +
  geom_tile(data = filter(hm, !rankable), fill = paper_ink$neutral, colour = "white",
            linewidth = 0.4) +
  geom_point(data = filter(hm, !rankable),
             aes(shape = "Not ranked: more than half of the cell's weight imputed"),
             size = 0.55, stroke = 0, colour = paper_ink$muted) +
  facet_grid(panel ~ stage, scales = "free", space = "free") +
  scale_fill_gradientn(colours = paper_seq_blue[2:7], limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1"),
                       name = "Percentile rank among the pair's rankable economies (1 = top)",
                       guide = guide_colourbar(barwidth = unit(4.2, "cm"), barheight = unit(0.25, "cm"),
                                               title.position = "top", order = 1)) +
  scale_shape_manual(values = 16, name = NULL,
                     guide = guide_legend(order = 2, override.aes = list(size = 1.4))) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), labels = short_name) +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7.5),
    axis.text.y = element_text(size = 7),
    axis.line.x = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0, face = "plain", colour = paper_ink$secondary,
                                size = 7.5),
    panel.spacing.x = unit(6, "pt"), panel.spacing.y = unit(5, "pt"),
    legend.box = "horizontal", legend.title = element_text(size = 8),
    plot.margin = margin(4, 4, 2, 4)
  )
save_paper_fig(p_hm, "eo_heatmap", width = paper_width_full, height = 7.9)

# ---------------------------------------------------------------------------
# Figure: eo_top
# ---------------------------------------------------------------------------

# Category colours avoid the three stage hues and match the ES category figure (Trade magenta,
# Production violet), so a category keeps its colour across figures.
top_fill <- c("Technology-level constants" = paper_ink$axis,
              setNames(c(unname(paper_cat[c("magenta", "violet", "yellow", "green", "red")]), paper_ink$secondary),
                       top_groups[-1]))
cell_lab <- function(tech, stage, n) {
  paste0(ifelse(tech == "Electric Vehicles", "EVs", tech), ", ", tolower(stage), " (n = ", n, ")")
}
n_rk <- pair_rankable %>% select(tech, supply_chain, n_rankable)
top_cells_lab <- top_cells %>% left_join(n_rk, by = c("tech", "supply_chain")) %>%
  mutate(facet = cell_lab(tech, supply_chain, n_rankable))
top_plot <- top_contrib %>%
  left_join(select(top_eo, all_of(keys), eo, rank, coalition), by = keys) %>%
  left_join(select(top_cells_lab, tech, supply_chain, facet), by = c("tech", "supply_chain")) %>%
  mutate(
    facet = factor(facet, levels = top_cells_lab$facet),
    seg = factor(seg, levels = top_groups),
    key = paste(Country, facet, sep = "___"),
    label = if_else(coalition, paste0("**", Country, "**"), Country)
  )
key_levels <- top_plot %>% distinct(key, facet, rank) %>% arrange(facet, desc(rank)) %>% pull(key)
label_map <- top_plot %>% distinct(key, label) %>% { setNames(.$label, .$key) }
top_plot <- top_plot %>% mutate(key = factor(key, levels = key_levels))
top_xmax <- ceiling(max(top_eo$eo) * 10) / 10

p_top <- ggplot(top_plot, aes(x = contrib, y = key, fill = seg)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.72, colour = "white",
           linewidth = 0.25) +
  facet_wrap(~facet, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = function(x) unname(label_map[x])) +
  scale_fill_manual(values = top_fill, name = NULL, drop = FALSE) +
  scale_x_continuous(limits = c(0, top_xmax), breaks = seq(0, top_xmax, 0.2),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Economic Opportunity score, stacked by contribution", y = NULL) +
  guides(fill = guide_legend(ncol = 3, byrow = FALSE)) +
  theme_paper(grid = "x") +
  theme(axis.text.y = ggtext::element_markdown(size = 8, colour = paper_ink$primary),
        panel.spacing.x = unit(14, "pt"), panel.spacing.y = unit(8, "pt"))
save_paper_fig(p_top, "eo_top", width = paper_width_full, height = 5.7)

# ---------------------------------------------------------------------------
# Console summary (for transcribing Table tab:eo_leaders; not a file)
# ---------------------------------------------------------------------------

cat("\nLeaders by pair (rankable cells):\n")
print(as.data.frame(leaders %>% select(tech, supply_chain, n, leader_iso3, leader_eo, runner_up_iso3,
                                       margin, best_coalition_iso3, best_coalition_rank,
                                       best_core_iso3, best_core_rank, usa_rank, chn_rank,
                                       varying_categories)), digits = 3)
cat("\nDone: figures eo_distribution, eo_contrib, eo_heatmap, eo_top; results eo.json\n")
