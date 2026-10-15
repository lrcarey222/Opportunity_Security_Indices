# Energy Security results (paper Section 5, subsection "Results").
#
# Figures (paper/figures/<name>.pdf + png/<name>.png):
#   es_distribution      ES by technology within each stage over the rankable cells: box plots
#                        across economies, every economy as a gray dot, coalition members in the
#                        stage colour, the United States and China marked; the common value of
#                        Nuclear upstream economies without uranium annotated.
#   es_category_contrib  Mean share of each category in ES by stage (exact category
#                        decomposition, rankable cells), with the mean effective weight marked.
#   es_heatmap           ES for the 57 coalition members (and China for reference) across the 33
#                        technology-stage sectors; rows grouped into core members outside the EU,
#                        core members inside the EU (one network node) and partners; cells that
#                        are scored but not rankable in gray; sectors without an ES score blank.
#   es_top               Dot plots of the ten highest-ranked rankable economies in three cells
#                        (Oil upstream, Batteries midstream, Solar midstream), the United States
#                        and China appended when outside the top ten, the median of the rankable
#                        economies and the cell-constant part of the score marked.
# Table (paper/tables/es_leaders.tex): leaders, best coalition member, US and China rank in
#   every technology-stage pair, rankable cells only.
# Numbers quoted in the text: paper/results/es.json.
#
# Sample: the eleven headline technologies of energy_security_index in
# data/processed/outputs/index_outputs.rds (final rerun of 2026-10-12). Rankings, top-N lists,
# distributions, fixed-effect decompositions, correlations and coalition averages use the
# rankable cells only (rankable == TRUE: a sovereign economy with at most tau = 0.5 of its index
# weight imputed, config/rankability.yml); counts over every scored cell are reported alongside.
#
# Run from the worktree root:
#   Rscript scripts/paper/fig_es.R

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
})

root <- paper_repo_root()
config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
raw_dir <- file.path(root, config$raw_data_dir)
processed_dir <- file.path(root, "data", "processed")
rank_cfg <- yaml::read_yaml(file.path(root, "config", "rankability.yml"))
tau <- rank_cfg$max_imputed_weight_share

# Pipeline helpers (pure functions) for the country roster and the investment split quoted in
# Section 5.
code_env <- new.env()
for (f in c("scripts/utils/raw_inputs.R", "R/utils/schema.R", "R/utils/scurve.R",
            "R/utils/country.R", "R/utils/iea_critical_minerals.R",
            "R/categories/reserves/reserves.R",
            "R/categories/investment/investment_momentum.R")) {
  sys.source(file.path(root, f), envir = code_env)
}

stage_levels <- c("Upstream", "Midstream", "Downstream")
tech_short <- c(
  "Batteries" = "Batteries", "Coal" = "Coal", "Electric Grid" = "Electric grid",
  "Electric Vehicles" = "EVs", "Gas" = "Gas", "Geothermal" = "Geothermal",
  "Green Hydrogen" = "Green H2", "Nuclear" = "Nuclear", "Oil" = "Oil", "Solar" = "Solar",
  "Wind" = "Wind"
)
tol <- 1e-9

# ---------------------------------------------------------------------------
# Data
io <- readRDS(file.path(processed_dir, "outputs", "index_outputs.rds"))
es_all <- io$energy_security_outputs$index
es_hl <- io$energy_security_index %>%
  rename(es = Energy_Security_Index)
diag <- io$energy_security_outputs$diagnostics
cells_diag <- diag$index_cells
cat_contrib <- io$energy_security_outputs$category_contributions %>%
  semi_join(es_hl, by = c("Country", "tech", "supply_chain"))
var_contrib <- io$energy_security_outputs$variable_contributions %>%
  semi_join(es_hl, by = c("Country", "tech", "supply_chain"))
categories_cfg <- diag$categories_configured
category_weights <- unlist(yaml::read_yaml(file.path(root, "config", "weights.yml"))$energy_security)
coalition_tbl <- io$coalition_members
si_tbl <- io$strategic_index %>%
  select(Country, tech, supply_chain, Energy_Security_Index, strategic_index, si_exclusion,
         resource_base, pick_exclusion)

# Construction diagnostics shared with Section 4 (normalization), computed the same way for
# both pillars on the eleven headline technologies.
pillar_diagnostics <- function(out, index_tbl, value_col) {
  idx <- index_tbl %>% rename(value = all_of(value_col))
  keys <- c("Country", "tech", "supply_chain")
  cc <- out$category_contributions %>% semi_join(idx, by = keys)
  vc <- out$variable_contributions %>% semi_join(idx, by = keys)
  d <- out$diagnostics
  pairs_hl <- distinct(idx, tech, supply_chain)
  status <- d$index_cells %>% semi_join(pairs_hl, by = c("tech", "supply_chain"))
  yrs <- d$year_provenance %>%
    semi_join(vc, by = c(keys, "category", "variable")) %>%
    group_by(across(all_of(keys))) %>%
    summarise(n_years = n_distinct(Year_selected), .groups = "drop")
  sets <- cc %>% group_by(across(all_of(keys))) %>%
    summarise(set = paste(sort(category), collapse = "|"), n_cat = n(), .groups = "drop")
  dec_c <- cc %>% group_by(across(all_of(keys))) %>%
    summarise(s = sum(weighted_category_contribution), .groups = "drop") %>%
    left_join(idx, by = keys)
  dec_v <- vc %>% group_by(across(all_of(keys))) %>%
    summarise(s = sum(weighted_variable_contribution), .groups = "drop") %>%
    left_join(idx, by = keys)
  list(
    n_pairs = nrow(pairs_hl),
    n_economies = n_distinct(idx$Country),
    n_cells = nrow(idx),
    n_cells_rankable = sum(idx$rankable),
    n_cells_not_sovereign = sum(grepl("not a sovereign", idx$rank_exclusion)),
    n_cells_sovereign_over_threshold = sum(idx$rank_exclusion %in% "imputed weight share above 0.5"),
    n_economies_rankable = n_distinct(idx$Country[idx$rankable]),
    n_economies_not_sovereign = n_distinct(idx$Country[grepl("not a sovereign", idx$rank_exclusion)]),
    n_economies_sovereign = n_distinct(idx$Country) -
      n_distinct(idx$Country[grepl("not a sovereign", idx$rank_exclusion)]),
    grid_economies_by_stage = idx %>% group_by(supply_chain) %>%
      summarise(n = n_distinct(Country), n_rankable = n_distinct(Country[rankable]),
                .groups = "drop"),
    pair_status = as.list(table(factor(status$cell_status, levels = c("ok", "constant", "unscored")))),
    share_cells_multi_year = mean(yrs$n_years > 1),
    max_years_in_cell = max(yrs$n_years),
    n_cells_by_years = yrs %>% count(n_years, name = "n_cells"),
    share_pairs_one_category_set = sets %>% group_by(tech, supply_chain) %>%
      summarise(one = n_distinct(set) == 1, .groups = "drop") %>% pull(one) %>% mean(),
    n_cells_by_categories = sets %>% count(n_cat, name = "n_cells"),
    max_abs_diff_category_sum = max(abs(dec_c$s - dec_c$value)),
    max_abs_diff_variable_sum = max(abs(dec_v$s - dec_v$value)),
    n_cells_fully_imputed = sum(idx$imputed_weight_share >= 1 - 1e-9),
    mean_imputed_weight_share_by_stage = idx %>% group_by(supply_chain) %>%
      summarise(mean = mean(imputed_weight_share), .groups = "drop"),
    share_category_scores_with_imputed_component = mean(cc$imputed),
    imputed_share_all_techs_builder = d$imputed_share
  )
}
joint_diagnostics <- list(
  es = pillar_diagnostics(io$energy_security_outputs, io$energy_security_index,
                          "Energy_Security_Index"),
  eo = pillar_diagnostics(io$economic_opportunity_outputs, io$economic_opportunity_index,
                          "Economic_Opportunity_Index")
)
rm(io); invisible(gc())

stopifnot(!anyDuplicated(es_hl[c("Country", "tech", "supply_chain")]))
headline_techs <- sort(unique(es_hl$tech))
stopifnot(length(headline_techs) == 11, !anyNA(es_hl$rankable))

# Country roster: the pipeline's canonical names and their ISO3 codes (WDI list, as in
# scripts/20_build_indices.R); economies outside the WDI list (Taiwan) via countrycode.
country_info <- code_env$standardize_country_info(
  read.csv(file.path(raw_dir, "wdi_country_info.csv"))
)
iso_map <- tibble(Country = sort(unique(es_hl$Country))) %>%
  left_join(country_info %>% select(Country = country, iso3c), by = "Country") %>%
  mutate(iso3c = ifelse(is.na(iso3c),
                        countrycode::countrycode(Country, "country.name", "iso3c",
                                                 warn = FALSE),
                        iso3c))
stopifnot(!anyNA(iso_map$iso3c), !anyDuplicated(iso_map$iso3c))

# Coalition: the 57 members of coalition_tbl (31 core, 26 partners), as stored with the run.
coalition_tbl <- coalition_tbl %>%
  mutate(group = case_when(tier == "core" & !eu_member ~ "Core, outside the EU",
                           tier == "core" & eu_member ~ "Core, EU members",
                           TRUE ~ "Partners"))
coalition_iso3 <- coalition_tbl$iso3
stopifnot(length(coalition_iso3) == 57, sum(coalition_tbl$tier == "core") == 31,
          all(coalition_tbl$country %in% es_hl$Country))
stopifnot(all(coalition_iso3 %in% iso_map$iso3c))
core_iso3 <- coalition_tbl$iso3[coalition_tbl$tier == "core"]
eu_iso3 <- coalition_tbl$iso3[coalition_tbl$eu_member]
usa <- iso_map$Country[iso_map$iso3c == "USA"]
chn <- iso_map$Country[iso_map$iso3c == "CHN"]

# Pairs: the 33 technology-stage sectors, of which those with an ES score.
sectors_all <- tidyr::expand_grid(tech = headline_techs, supply_chain = stage_levels)
pairs_scored <- distinct(es_hl, tech, supply_chain)
pairs_unscored <- anti_join(sectors_all, pairs_scored, by = c("tech", "supply_chain"))
pair_status <- cells_diag %>%
  semi_join(pairs_scored, by = c("tech", "supply_chain")) %>%
  mutate(pair = paste(tech, supply_chain, sep = " - "))
constant_pairs <- es_hl %>% group_by(tech, supply_chain) %>%
  summarise(range = max(es) - min(es), .groups = "drop") %>% filter(range < 1e-9)
stopifnot(nrow(constant_pairs) == 0, all(pair_status$cell_status == "ok"))

es_scored <- es_hl %>%
  left_join(iso_map, by = "Country") %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels),
         member = iso3c %in% coalition_iso3)
es <- es_scored %>%
  filter(rankable) %>%
  group_by(tech, supply_chain) %>%
  mutate(
    n_cell = n(),
    rank = min_rank(desc(round(es, 12))),
    pct = (n_cell - rank) / (n_cell - 1)
  ) %>%
  ungroup()
rankable_keys <- es %>% transmute(Country, tech, supply_chain = as.character(supply_chain))

# ---------------------------------------------------------------------------
# Coverage, rankability and construction checks
cats_per_cell <- cat_contrib %>%
  count(Country, tech, supply_chain, name = "n_cat")
cat_sets <- cat_contrib %>%
  group_by(Country, tech, supply_chain) %>%
  summarise(set = paste(sort(category), collapse = "|"), .groups = "drop") %>%
  group_by(tech, supply_chain) %>%
  summarise(n_sets = n_distinct(set), set = first(set), .groups = "drop")
decomp_cat <- cat_contrib %>%
  group_by(Country, tech, supply_chain) %>%
  summarise(s = sum(weighted_category_contribution), w = sum(category_weight), .groups = "drop") %>%
  left_join(es_hl, by = c("Country", "tech", "supply_chain"))
decomp_var <- var_contrib %>%
  group_by(Country, tech, supply_chain) %>%
  summarise(s = sum(weighted_variable_contribution), .groups = "drop") %>%
  left_join(es_hl, by = c("Country", "tech", "supply_chain"))

# Modal-value diagnostic: pairs in which one ES value is shared by at least a quarter of the
# scored economies (ties that a ranking cannot break).
mode_share <- function(v) max(table(round(v, 10))) / length(v)
es_resolution <- es_scored %>% group_by(supply_chain, tech) %>%
  summarise(n_scored = n(), n_distinct_scored = n_distinct(round(es, 10)),
            share_at_mode_scored = mode_share(es),
            modal_value = as.numeric(names(which.max(table(round(es, 10))))),
            n_rankable = sum(rankable),
            share_at_mode_rankable = if (any(rankable)) mode_share(es[rankable]) else NA_real_,
            .groups = "drop") %>%
  arrange(desc(share_at_mode_scored))

rank_by_stage <- es_scored %>% group_by(supply_chain) %>%
  summarise(n_scored = n(), n_rankable = sum(rankable),
            n_not_sovereign = sum(grepl("not a sovereign", rank_exclusion)),
            n_sovereign_over_threshold = sum(rank_exclusion %in% "imputed weight share above 0.5"),
            n_economies_scored = n_distinct(Country),
            n_economies_rankable = n_distinct(Country[rankable]),
            share_sovereign_rankable = sum(rankable) / sum(!grepl("not a sovereign", rank_exclusion)),
            .groups = "drop")
rank_by_pair <- es_scored %>% group_by(supply_chain, tech) %>%
  summarise(n_scored = n(), n_rankable = sum(rankable), .groups = "drop")

coverage <- list(
  n_cells_headline = nrow(es_hl),
  n_economies_headline = n_distinct(es_hl$Country),
  n_pairs_headline = nrow(pairs_scored),
  n_pairs_headline_by_stage = as.list(table(factor(pairs_scored$supply_chain, levels = stage_levels))),
  pairs_without_es_score = pairs_unscored %>% transmute(pair = paste(tech, supply_chain)),
  n_cells_all_techs = nrow(es_all),
  n_economies_all_techs = n_distinct(es_all$Country),
  n_pairs_all_techs = nrow(cells_diag),
  pair_status_headline = as.list(table(factor(pair_status$cell_status,
                                              levels = c("ok", "constant", "unscored")))),
  n_constant_pairs = nrow(constant_pairs),
  n_unscored_groups = nrow(diag$unscored_groups),
  rankability_tau = tau,
  n_cells_rankable = sum(es_hl$rankable),
  n_cells_not_sovereign = sum(grepl("not a sovereign", es_hl$rank_exclusion)),
  n_cells_not_sovereign_and_over_threshold = sum(es_hl$rank_exclusion %in%
    "not a sovereign economy and imputed weight share above 0.5"),
  n_cells_sovereign_over_threshold = sum(es_hl$rank_exclusion %in% "imputed weight share above 0.5"),
  n_economies_rankable = n_distinct(es_hl$Country[es_hl$rankable]),
  n_economies_not_sovereign = n_distinct(es_hl$Country[grepl("not a sovereign", es_hl$rank_exclusion)]),
  rankability_by_stage = rank_by_stage,
  rankability_by_pair = rank_by_pair,
  rankable_per_pair_range = range(rank_by_pair$n_rankable),
  rankable_per_pair_min_pair = rank_by_pair %>% slice_min(n_rankable, n = 1) %>%
    transmute(pair = paste(tech, supply_chain), n_rankable),
  economies_per_pair_range = range(rank_by_pair$n_scored),
  n_economies_by_stage = es_hl %>% group_by(supply_chain) %>%
    summarise(n_economies = n_distinct(Country), .groups = "drop"),
  n_cells_fully_imputed = sum(es_hl$imputed_weight_share >= 1 - tol),
  mean_imputed_weight_share_by_stage = es_hl %>% group_by(supply_chain) %>%
    summarise(mean = mean(imputed_weight_share),
              mean_rankable = mean(imputed_weight_share[rankable]), .groups = "drop"),
  mean_imputed_weight_share_all = mean(es_hl$imputed_weight_share),
  share_category_scores_with_imputed_component = mean(cat_contrib$imputed),
  categories_per_cell_by_stage = cats_per_cell %>% group_by(supply_chain) %>%
    summarise(min = min(n_cat), max = max(n_cat), .groups = "drop"),
  categories_per_pair = cats_per_cell %>% group_by(tech, supply_chain) %>%
    summarise(min = min(n_cat), max = max(n_cat), .groups = "drop"),
  categories_by_pair = cat_sets %>% transmute(tech, supply_chain, categories = gsub("\\|", ", ", set)),
  n_cells_by_categories = cats_per_cell %>% count(n_cat, name = "n_cells"),
  share_pairs_one_category_set = mean(cat_sets$n_sets == 1),
  max_abs_diff_index_vs_category_sum = max(abs(decomp_cat$s - decomp_cat$es)),
  max_abs_diff_index_vs_variable_sum = max(abs(decomp_var$s - decomp_var$es)),
  max_abs_dev_effective_weight_sum = max(abs(decomp_cat$w - 1)),
  n_cells_es_exactly_zero = sum(es_hl$es <= tol),
  modal_value_diagnostic = es_resolution,
  pairs_mode_share_ge_25pct_scored = es_resolution %>% filter(share_at_mode_scored >= 0.25) %>%
    transmute(pair = paste(tech, supply_chain), share_at_mode_scored, modal_value,
              share_at_mode_rankable)
)

# ---------------------------------------------------------------------------
# Distribution by stage and by pair (rankable cells; every scored cell for comparison)
q <- function(v, p) unname(stats::quantile(v, p, type = 7))
moments <- function(d) {
  d %>% summarise(
    n = n(), mean = mean(es), sd = sd(es), min = min(es), p10 = q(es, .1), p25 = q(es, .25),
    median = median(es), p75 = q(es, .75), p90 = q(es, .9), max = max(es),
    skewness = mean((es - mean(es))^3) / sd(es)^3, .groups = "drop"
  )
}
dist_by_stage <- es %>% group_by(supply_chain) %>% moments()
dist_all <- es %>% moments()
dist_by_stage_scored <- es_scored %>% group_by(supply_chain) %>% moments()
dist_by_pair <- es %>% group_by(supply_chain, tech) %>% moments() %>%
  mutate(iqr = p75 - p25, range = max - min) %>%
  arrange(supply_chain, desc(median))
dist_pair_iqr_summary <- dist_by_pair %>% group_by(supply_chain) %>%
  summarise(median_pair_iqr = median(iqr), min_pair_median = min(median),
            max_pair_median = max(median), .groups = "drop")

# Variance shares from fixed-effect regressions on the rankable cells.
r2 <- function(formula, d) summary(stats::lm(formula, data = d))$r.squared
es_fe <- es %>% mutate(pair = interaction(tech, supply_chain, drop = TRUE))
variance <- list(
  sample = "rankable cells",
  n_cells = nrow(es_fe),
  r2_pair_fe = r2(es ~ pair, es_fe),
  r2_stage_fe = r2(es ~ supply_chain, es_fe),
  r2_tech_fe = r2(es ~ tech, es_fe),
  r2_country_fe = r2(es ~ Country, es_fe),
  r2_pair_plus_country_fe = r2(es ~ pair + Country, es_fe),
  r2_country_fe_within_stage = es_fe %>% group_by(supply_chain) %>%
    group_modify(~ tibble(r2_pair = r2(es ~ pair, .x), r2_country = r2(es ~ Country, .x),
                          r2_both = r2(es ~ pair + Country, .x))) %>% ungroup()
)
variance$incremental_r2_country_given_pair <- variance$r2_pair_plus_country_fe - variance$r2_pair_fe
# Partial R^2 of country effects given pair effects, (R2_both - R2_pair) / (1 - R2_pair): the
# share of the within-pair variance that country fixed effects explain (Frisch-Waugh-Lovell).
variance$partial_r2_country_given_pair <- variance$incremental_r2_country_given_pair /
  (1 - variance$r2_pair_fe)
variance$partial_r2_country_given_pair_by_stage <- variance$r2_country_fe_within_stage %>%
  transmute(supply_chain, partial_r2 = (r2_both - r2_pair) / (1 - r2_pair))
# The same decomposition over every scored cell, for comparison with the rankable sample.
es_fe_scored <- es_scored %>% mutate(pair = interaction(tech, supply_chain, drop = TRUE))
variance$scored_cells <- list(
  n_cells = nrow(es_fe_scored),
  r2_pair_fe = r2(es ~ pair, es_fe_scored),
  r2_country_fe = r2(es ~ Country, es_fe_scored),
  r2_pair_plus_country_fe = r2(es ~ pair + Country, es_fe_scored)
)

# ---------------------------------------------------------------------------
# Leaders, coalition members, the United States and China (rankable cells)
first_or_na <- function(v) if (length(v)) v[[1]] else v[NA_integer_]
leaders <- es %>%
  arrange(supply_chain, tech, rank, Country) %>%
  group_by(supply_chain, tech) %>%
  summarise(
    n = first(n_cell),
    leader = first(Country), leader_iso3 = first(iso3c), leader_es = first(es),
    n_tied_at_top = sum(rank == 1),
    second_iso3 = iso3c[2], second_es = es[2], second_tie = sum(rank == rank[2]),
    third_iso3 = iso3c[3], third_es = es[3], third_tie = sum(rank == rank[3]),
    leader_member = first(member),
    best_member = Country[member][1], best_member_iso3 = iso3c[member][1],
    best_member_rank = rank[member][1], best_member_es = es[member][1],
    members_in_top10 = sum(member & rank <= 10),
    n_members_rankable = sum(member),
    best_member_tie = sum(rank == best_member_rank),
    usa_rank = first_or_na(rank[iso3c == "USA"]), usa_es = first_or_na(es[iso3c == "USA"]),
    usa_tie = sum(rank == usa_rank),
    chn_rank = first_or_na(rank[iso3c == "CHN"]), chn_es = first_or_na(es[iso3c == "CHN"]),
    chn_tie = sum(rank == chn_rank),
    median_es = median(es),
    .groups = "drop"
  )

leader_counts <- leaders %>% count(leader, name = "n_pairs_led") %>% arrange(desc(n_pairs_led))
leader_summary <- list(
  n_pairs = nrow(leaders),
  n_pairs_member_first = sum(leaders$best_member_rank == 1),
  n_pairs_member_top3 = sum(leaders$best_member_rank <= 3),
  n_pairs_member_top10 = sum(leaders$best_member_rank <= 10),
  median_best_member_rank = median(leaders$best_member_rank),
  pairs_led_by_member = leaders %>% filter(best_member_rank == 1) %>%
    transmute(pair = paste(tech, supply_chain), leader),
  pairs_best_member_outside_top3 = leaders %>% filter(best_member_rank > 3) %>%
    transmute(pair = paste(tech, supply_chain), best_member, best_member_rank),
  n_pairs_china_first = sum(leaders$chn_rank == 1, na.rm = TRUE),
  n_pairs_usa_first = sum(leaders$usa_rank == 1, na.rm = TRUE),
  n_pairs_usa_above_china = sum(leaders$usa_rank < leaders$chn_rank, na.rm = TRUE),
  n_pairs_with_both = sum(!is.na(leaders$usa_rank) & !is.na(leaders$chn_rank)),
  n_pairs_any_tie_at_top = sum(leaders$n_tied_at_top > 1),
  mean_members_in_top10 = mean(leaders$members_in_top10),
  median_members_in_top10 = median(leaders$members_in_top10),
  pairs_more_than_10_at_rank_10_or_better = leaders %>%
    filter(members_in_top10 > 10) %>% transmute(pair = paste(tech, supply_chain), members_in_top10),
  members_in_top10_by_stage = leaders %>% group_by(supply_chain) %>%
    summarise(mean = mean(members_in_top10), mean_rankable_members = mean(n_members_rankable),
              .groups = "drop")
)

es_grp <- es %>%
  mutate(coalition_group = case_when(
    iso3c %in% coalition_iso3 & iso3c %in% core_iso3 ~ "core",
    iso3c %in% coalition_iso3 ~ "partner",
    TRUE ~ "non-member"))
member_stage <- es %>%
  group_by(supply_chain, member) %>%
  summarise(mean_es = mean(es), median_es = median(es), mean_pct = mean(pct), n = n(),
            n_economies = n_distinct(Country), .groups = "drop")
member_tier_stage <- es_grp %>%
  group_by(supply_chain, coalition_group) %>%
  summarise(mean_es = mean(es), median_es = median(es), mean_pct = mean(pct), n = n(),
            n_economies = n_distinct(Country), .groups = "drop")
member_eu_stage <- es %>% filter(member) %>%
  mutate(eu = iso3c %in% eu_iso3) %>%
  group_by(supply_chain, eu) %>%
  summarise(mean_es = mean(es), mean_pct = mean(pct), n = n(), .groups = "drop")
member_means <- es %>% filter(member) %>%
  group_by(Country, iso3c) %>%
  summarise(mean_es = mean(es), mean_pct = mean(pct), n_pairs = n(), .groups = "drop") %>%
  left_join(coalition_tbl %>% select(iso3c = iso3, tier, eu_member, group), by = "iso3c") %>%
  arrange(desc(mean_pct))
member_rankable_cells <- es_scored %>% filter(member) %>%
  group_by(Country, iso3c) %>%
  summarise(n_scored = n(), n_rankable = sum(rankable), .groups = "drop") %>%
  arrange(n_rankable)
member_stage_country <- es %>% filter(member) %>%
  group_by(Country, iso3c, supply_chain) %>%
  summarise(mean_es = mean(es), mean_pct = mean(pct), n = n(), .groups = "drop")
member_stage_best <- member_stage_country %>% filter(n >= 5) %>% group_by(supply_chain) %>%
  slice_max(mean_pct, n = 5) %>% ungroup()
member_stage_worst <- member_stage_country %>% filter(n >= 5) %>% group_by(supply_chain) %>%
  slice_min(mean_pct, n = 5) %>% ungroup()

focus_positions <- es %>% filter(iso3c %in% c("USA", "CHN", "TWN")) %>%
  group_by(iso3c, supply_chain) %>%
  summarise(mean_es = mean(es), mean_pct = mean(pct), median_rank = median(rank),
            n_top10 = sum(rank <= 10), n_top1 = sum(rank == 1), n_pairs = n(), .groups = "drop")
focus_pairs <- es %>% filter(iso3c %in% c("USA", "CHN", "TWN")) %>%
  select(iso3c, tech, supply_chain, es, rank, n_cell, pct) %>%
  arrange(iso3c, supply_chain, rank)
focus_overall <- es %>% filter(iso3c %in% c("USA", "CHN", "TWN")) %>%
  group_by(iso3c) %>%
  summarise(mean_es = mean(es), mean_pct = mean(pct), n_top10 = sum(rank <= 10),
            n_top1 = sum(rank == 1), n_pairs = n(), .groups = "drop")
taiwan_scored <- es_scored %>% filter(iso3c == "TWN") %>%
  select(tech, supply_chain, es, imputed_weight_share, rankable, rank_exclusion)
# China in Oil downstream: its Consumption components against the other rankable economies.
china_oil_downstream <- var_contrib %>%
  semi_join(rankable_keys, by = c("Country", "tech", "supply_chain")) %>%
  filter(tech == "Oil", supply_chain == "Downstream") %>%
  group_by(category, variable) %>%
  summarise(china = component_value[Country == chn],
            n_strictly_below_china = sum(component_value < component_value[Country == chn] - 1e-12),
            n_economies = n(), n_distinct = n_distinct(round(component_value, 10)),
            category_weight_china = category_weight[Country == chn], .groups = "drop")

# ---------------------------------------------------------------------------
# Nuclear upstream: uranium resources (Red Book identified recoverable resources below
# USD 260/kgU), mine production and, since the final rerun, Trade in uranium ores and
# concentrates (HS 261210, 284410: the Upstream basket). An economy with neither resources nor
# mine output scores Energy Prices (one value for every economy) plus its Trade category; where
# its uranium trade is unobserved, Trade is imputed and the economy sits at one common value, the
# mode of the pair (the "base" value used below in place of the earlier Energy-Prices-only floor).
nu_cc <- cat_contrib %>% filter(tech == "Nuclear", supply_chain == "Upstream")
nu_wide <- nu_cc %>%
  select(Country, category, category_score, category_weight, weighted_category_contribution) %>%
  pivot_wider(names_from = category,
              values_from = c(category_score, category_weight, weighted_category_contribution))
nu_trade_obs <- var_contrib %>%
  filter(tech == "Nuclear", supply_chain == "Upstream", category == "Trade", variable != "HHI") %>%
  group_by(Country) %>%
  summarise(trade_observed = any(!imputed), .groups = "drop")
nu <- es_scored %>% filter(tech == "Nuclear", supply_chain == "Upstream") %>%
  left_join(nu_wide, by = "Country") %>%
  left_join(nu_trade_obs, by = "Country") %>%
  mutate(has_uranium = `category_score_Reserves` > tol | `category_score_Production` > tol,
         trade_observed = coalesce(trade_observed, FALSE))
nu_ep <- unique(round(nu$`weighted_category_contribution_Energy Prices`, 10))
stopifnot(length(nu_ep) == 1)
nu_nou_r <- nu %>% filter(rankable, !has_uranium)
nu_floor <- as.numeric(names(which.max(table(round(nu_nou_r$es, 10)))))
at_base <- function(x) abs(x - nu_floor) < 1e-8
nu_rank <- es %>% filter(tech == "Nuclear", supply_chain == "Upstream") %>%
  arrange(rank, Country)
si_nu <- si_tbl %>% filter(tech == "Nuclear", supply_chain == "Upstream", !is.na(strategic_index))
nuclear_upstream <- list(
  categories = sort(unique(nu_cc$category)),
  effective_weights = nu_cc %>% group_by(category) %>%
    summarise(weight = mean(category_weight), sd_weight = sd(category_weight), .groups = "drop"),
  energy_prices_score = unique(round(nu$`category_score_Energy Prices`, 10)),
  energy_prices_contribution = nu_ep,
  base_value = nu_floor,
  base_definition = "modal ES of the rankable economies without uranium resources or mine output (Energy Prices plus an imputed Trade category)",
  base_trade_score = unique(round(nu_nou_r$`category_score_Trade`[at_base(nu_nou_r$es)], 6)),
  n_scored = nrow(nu),
  n_at_base_scored = sum(at_base(nu$es)),
  n_with_uranium_scored = sum(nu$has_uranium),
  n_with_reserves_scored = sum(nu$`category_score_Reserves` > tol),
  n_with_production_scored = sum(nu$`category_score_Production` > tol),
  n_rankable = sum(nu$rankable),
  n_with_uranium_rankable = sum(nu$has_uranium[nu$rankable]),
  n_without_uranium_rankable = nrow(nu_nou_r),
  n_at_base_rankable = sum(at_base(nu$es[nu$rankable])),
  share_at_base_rankable = mean(at_base(nu$es[nu$rankable])),
  n_without_uranium_trade_observed_rankable = sum(nu_nou_r$trade_observed),
  n_without_uranium_at_base_trade_observed = sum(nu_nou_r$trade_observed & at_base(nu_nou_r$es)),
  range_without_uranium_rankable = range(nu_nou_r$es),
  without_uranium_off_base = nu_nou_r %>% filter(!at_base(es)) %>% arrange(desc(es)) %>%
    transmute(Country, iso3c, es, trade_score = `category_score_Trade`, trade_observed, member),
  n_members_with_uranium = sum(nu$has_uranium & nu$member),
  members_with_uranium = nu %>% filter(has_uranium, member) %>% arrange(desc(es)) %>%
    transmute(Country, iso3c, es, rankable),
  members_without_uranium = nu %>% filter(!has_uranium, member) %>% pull(iso3c) %>% sort(),
  top10 = nu_rank %>% filter(rank <= 10) %>% transmute(Country, iso3c, es, rank, member),
  usa = nu_rank %>% filter(iso3c == "USA") %>% transmute(es, rank, n_cell),
  chn = nu_rank %>% filter(iso3c == "CHN") %>% transmute(es, rank, n_cell),
  rank_of_base_tie = min(nu_rank$rank[at_base(nu_rank$es)]),
  si_countries = nrow(si_nu),
  si_countries_at_base = sum(at_base(si_nu$Energy_Security_Index)),
  si_countries_with_uranium = sum(si_nu$resource_base %in% TRUE)
)

# Sectors in which one ES value is shared by at least a quarter of the Strategic Index countries
# (the pipeline's strategic_index_es_floor_sectors(), recomputed on the stored SI table): a floor
# that the need reading 1 - ES turns into a high, sector-wide need.
si_floor_sectors <- si_tbl %>%
  filter(!is.na(strategic_index)) %>%
  mutate(es_r = round(Energy_Security_Index, 6), sector = paste(tech, supply_chain)) %>%
  group_by(sector) %>%
  summarise(n_countries = n_distinct(Country),
            es_mode = as.numeric(names(which.max(table(es_r)))),
            n_at_mode = max(table(es_r)), .groups = "drop") %>%
  mutate(share_at_mode = n_at_mode / n_countries) %>%
  filter(n_countries > 1, share_at_mode >= 0.25) %>%
  arrange(desc(share_at_mode))
si_sample <- list(
  n_si_cells = sum(!is.na(si_tbl$strategic_index)),
  n_si_countries = n_distinct(si_tbl$Country[!is.na(si_tbl$strategic_index)]),
  floor_sectors_share_ge_25pct = si_floor_sectors
)

# Taiwan's Trade category in the midstream pairs (rankable cells): its position on the export
# share and net-import terms now that the trade themes carry it.
taiwan_trade_mid <- cat_contrib %>%
  semi_join(rankable_keys, by = c("Country", "tech", "supply_chain")) %>%
  filter(category == "Trade", supply_chain == "Midstream") %>%
  group_by(tech) %>%
  mutate(trade_rank = min_rank(desc(round(category_score, 12))), n = n()) %>%
  ungroup() %>%
  filter(Country == "Taiwan") %>%
  select(tech, category_score, trade_rank, n)

# ---------------------------------------------------------------------------
# Category contributions and effective weights by stage (rankable cells)
cc_rank <- cat_contrib %>%
  semi_join(rankable_keys, by = c("Country", "tech", "supply_chain")) %>%
  left_join(es_hl %>% select(Country, tech, supply_chain, es),
            by = c("Country", "tech", "supply_chain"))
cell_keys <- cc_rank %>% distinct(Country, tech, supply_chain, es)
contrib_cells <- cc_rank %>%
  select(Country, tech, supply_chain, category, category_weight,
         weighted_category_contribution, es) %>%
  complete(nesting(Country, tech, supply_chain, es), category = categories_cfg,
           fill = list(category_weight = 0, weighted_category_contribution = 0))
contrib_by_stage <- contrib_cells %>%
  filter(es > tol) %>%
  mutate(share = weighted_category_contribution / es) %>%
  group_by(supply_chain, category) %>%
  summarise(mean_share = mean(share), mean_weight = mean(category_weight),
            ratio_of_sums = sum(weighted_category_contribution) / sum(es),
            n_cells = n(), .groups = "drop") %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels),
         category = factor(category, levels = categories_cfg)) %>%
  arrange(supply_chain, category)
contrib_cells_by_stage <- cell_keys %>% filter(es > tol) %>% count(supply_chain, name = "n_cells")
# Upstream without Nuclear: in the Nuclear upstream cells without uranium, Energy Prices and an
# imputed Trade category make the whole score, which raises their upstream mean shares.
contrib_upstream_excl_nuclear <- contrib_cells %>%
  filter(es > tol, supply_chain == "Upstream", tech != "Nuclear") %>%
  mutate(share = weighted_category_contribution / es) %>%
  group_by(category) %>%
  summarise(mean_share = mean(share), mean_weight = mean(category_weight), n_cells = n(),
            .groups = "drop") %>%
  filter(mean_weight > 0) %>% arrange(desc(mean_share))
contrib_top3 <- contrib_by_stage %>% group_by(supply_chain) %>%
  slice_max(mean_share, n = 3) %>% ungroup() %>% select(supply_chain, category, mean_share)
contrib_cells_dropped <- sum(cell_keys$es <= tol)

# Cell-constant inputs: variables with no cross-country variation inside a pair. Energy Prices
# and the world-concentration term of Trade are exactly constant; the Minerals Trade
# concentration term is constant except for economies outside the World Bank list.
var_spread <- var_contrib %>%
  semi_join(rankable_keys, by = c("Country", "tech", "supply_chain")) %>%
  group_by(tech, supply_chain, category, variable) %>%
  summarise(n_distinct = n_distinct(round(component_value, 10)),
            range = max(component_value) - min(component_value), .groups = "drop")
constant_vars <- tibble(
  category = c("Energy Prices", "Trade", "Minerals Trade"),
  variable = c("Overall Price Volatility Index", "HHI", "critmin_hhi")
)
critmin_hhi_exceptions <- var_contrib %>%
  filter(category == "Minerals Trade", variable == "critmin_hhi") %>%
  group_by(tech, supply_chain) %>%
  mutate(mode_val = as.numeric(names(which.max(table(round(component_value, 10)))))) %>%
  filter(abs(component_value - mode_val) > 1e-8) %>%
  ungroup() %>% distinct(Country)
const_weight_cells <- var_contrib %>%
  semi_join(rankable_keys, by = c("Country", "tech", "supply_chain")) %>%
  semi_join(constant_vars, by = c("category", "variable")) %>%
  mutate(eff_weight = category_weight * component_weight_within_overall) %>%
  group_by(Country, tech, supply_chain) %>%
  summarise(const_weight = sum(eff_weight), const_contrib = sum(weighted_variable_contribution),
            .groups = "drop") %>%
  right_join(cell_keys, by = c("Country", "tech", "supply_chain")) %>%
  mutate(const_weight = coalesce(const_weight, 0), const_contrib = coalesce(const_contrib, 0))
const_weight_by_stage <- const_weight_cells %>%
  group_by(supply_chain) %>%
  summarise(mean_const_weight = mean(const_weight),
            mean_const_contrib_share = mean(const_contrib[es > tol] / es[es > tol]),
            .groups = "drop")
const_weight_by_pair <- const_weight_cells %>%
  group_by(supply_chain, tech) %>%
  summarise(const_weight = mean(const_weight), const_contrib = mean(const_contrib),
            median_es = median(es), .groups = "drop")
net_df <- const_weight_cells %>%
  mutate(pair = interaction(tech, supply_chain, drop = TRUE), es_net = es - const_contrib)
variance$r2_pair_fe_es_net_of_constant_inputs <-
  summary(stats::lm(es_net ~ pair, data = net_df))$r.squared

# Energy Prices: one value per technology, so it moves upstream levels only.
energy_prices <- cc_rank %>% filter(category == "Energy Prices") %>%
  group_by(tech, supply_chain) %>%
  summarise(score = mean(category_score), sd_score = sd(category_score),
            eff_weight = mean(category_weight), level_shift = mean(weighted_category_contribution),
            .groups = "drop") %>%
  arrange(desc(score))

# ---------------------------------------------------------------------------
# Within-pair Spearman correlations between categories, and with the pillar (rankable cells)
spearman_or_na <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 10 || sd(a[ok]) == 0 || sd(b[ok]) == 0) return(NA_real_)
  stats::cor(a[ok], b[ok], method = "spearman")
}
cat_wide <- cc_rank %>%
  select(Country, tech, supply_chain, category, category_score, es)
pair_cats <- cat_wide %>% distinct(tech, supply_chain, category)
cat_pairs <- pair_cats %>%
  inner_join(pair_cats, by = c("tech", "supply_chain"), suffix = c("_a", "_b"),
             relationship = "many-to-many") %>%
  filter(match(category_a, categories_cfg) < match(category_b, categories_cfg))
# rho: all rankable economies of the pair. rho_positive: only economies with a strictly
# positive score on both categories, which removes the block of tied zeros that the zero rule
# and the zero-anchored S-curve create in the register-type categories.
cat_cor <- cat_pairs %>%
  rowwise() %>%
  mutate(cors = list({
    d <- cat_wide %>% filter(tech == .env$tech, supply_chain == .env$supply_chain)
    a <- d %>% filter(category == category_a) %>% select(Country, a = category_score)
    b <- d %>% filter(category == category_b) %>% select(Country, b = category_score)
    ab <- inner_join(a, b, by = "Country")
    pos <- ab$a > tol & ab$b > tol
    c(rho = spearman_or_na(ab$a, ab$b), rho_positive = spearman_or_na(ab$a[pos], ab$b[pos]),
      n = nrow(ab), n_positive = sum(pos))
  })) %>%
  ungroup() %>%
  tidyr::unnest_wider(cors)
cat_cor_by_stage <- cat_cor %>%
  filter(!is.na(rho)) %>%
  group_by(supply_chain, category_a, category_b) %>%
  summarise(median_rho = median(rho), min_rho = min(rho), max_rho = max(rho),
            median_rho_positive = median(rho_positive, na.rm = TRUE),
            median_n_positive = median(n_positive),
            n_pairs = n(), .groups = "drop") %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels)) %>%
  arrange(supply_chain, desc(median_rho))
cat_cor_stage_summary <- cat_cor %>% filter(!is.na(rho)) %>%
  group_by(supply_chain) %>%
  summarise(median_rho = median(rho), share_negative = mean(rho < 0),
            min_rho = min(rho), max_rho = max(rho), n = n(), .groups = "drop")
cat_es_cor <- cat_wide %>%
  group_by(tech, supply_chain, category) %>%
  summarise(rho = spearman_or_na(category_score, es), .groups = "drop")
cat_es_cor_by_stage <- cat_es_cor %>% filter(!is.na(rho)) %>%
  group_by(supply_chain, category) %>%
  summarise(median_rho = median(rho), min_rho = min(rho), max_rho = max(rho),
            n_pairs = n(), .groups = "drop") %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels)) %>%
  arrange(supply_chain, desc(median_rho))

# Resolution of each category inside a pair (rankable cells).
cat_resolution <- cat_wide %>%
  group_by(tech, supply_chain, category) %>%
  summarise(n = n(), n_distinct = n_distinct(round(category_score, 10)),
            share_zero = mean(category_score <= tol), .groups = "drop")
cat_resolution_by_stage <- cat_resolution %>%
  group_by(supply_chain, category) %>%
  summarise(median_n_distinct = median(n_distinct), min_n_distinct = min(n_distinct),
            max_n_distinct = max(n_distinct), median_share_zero = median(share_zero),
            n_pairs = n(), .groups = "drop") %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels),
         category = factor(category, levels = categories_cfg)) %>%
  arrange(supply_chain, category)

# ---------------------------------------------------------------------------
# Investment split quoted in Section 5 (Batteries vs EV share of upstream critical-minerals
# investment), recomputed with the pipeline's own functions.
critical_minerals_path <- code_env$resolve_versioned_raw_input(
  raw_dir,
  pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
  fallback = "IEA Critical Minerals Dataset 2026.xlsx",
  label = "IEA Critical Minerals Dataset"
)
critical <- code_env$read_iea_critical_minerals(critical_minerals_path)
mineral_demand_clean <- code_env$reserves_build_mineral_demand_clean(critical)
investment_split <- code_env$investment_critical_minerals_split(mineral_demand_clean)
mineral_demand_shares <- mineral_demand_clean %>%
  filter(tech %in% headline_techs) %>%
  transmute(Mineral, tech, share_base_year = as.numeric(share_24)) %>%
  arrange(tech, desc(share_base_year))
rm(critical); invisible(gc())

# ---------------------------------------------------------------------------
# Figure 1: es_distribution (rankable cells)
pair_order <- es %>% group_by(supply_chain, tech) %>%
  summarise(med = median(es), mean = mean(es), .groups = "drop") %>%
  arrange(supply_chain, med, mean) %>%
  mutate(y_lab = paste(supply_chain, tech_short[tech], sep = "|"))
y_levels <- pair_order$y_lab
lab_fun <- function(x) sub("^.*\\|", "", x)

dist_df <- es %>%
  mutate(y_lab = factor(paste(supply_chain, tech_short[tech], sep = "|"), levels = y_levels),
         grp = case_when(iso3c == "USA" ~ "United States",
                         iso3c == "CHN" ~ "China",
                         member ~ "Coalition member",
                         TRUE ~ "Other economy"),
         grp = factor(grp, levels = c("Other economy", "Coalition member", "United States", "China")))
jit <- position_jitter(width = 0, height = 0.2, seed = 20261009)
others <- dist_df %>% filter(grp == "Other economy")
members <- dist_df %>% filter(grp == "Coalition member")
focus <- dist_df %>% filter(grp %in% c("United States", "China"))
nu_note <- tibble(
  supply_chain = factor("Upstream", levels = stage_levels),
  y_lab = factor(paste("Upstream", tech_short[["Nuclear"]], sep = "|"), levels = y_levels),
  x = nu_floor + 0.03,
  label = sprintf("%d of %d rankable economies at %.3f: no uranium resources or mine output, trade imputed",
                  nuclear_upstream$n_at_base_rankable, nuclear_upstream$n_rankable, nu_floor)
)

p_dist <- ggplot() +
  geom_point(data = others, aes(x = es, y = y_lab, shape = grp), fill = "#b4b2a9",
             colour = "#b4b2a9", size = 0.7, alpha = 0.75, stroke = 0, position = jit) +
  geom_boxplot(data = dist_df, aes(x = es, y = y_lab, colour = supply_chain),
               fill = NA, width = 0.62, outlier.shape = NA, linewidth = 0.35,
               coef = 1.5, staplewidth = 0) +
  geom_point(data = members, aes(x = es, y = y_lab, fill = supply_chain, shape = grp),
             size = 1.15, colour = "white", stroke = 0.25, position = jit) +
  geom_point(data = focus, aes(x = es, y = y_lab, shape = grp),
             fill = paper_ink$primary, colour = "white", size = 1.9, stroke = 0.35) +
  geom_text(data = nu_note, aes(x = x, y = y_lab, label = label), hjust = 0,
            nudge_y = -0.62, size = 2.5, colour = paper_ink$secondary, fontface = "italic") +
  facet_grid(supply_chain ~ ., scales = "free_y", space = "free_y") +
  # Extra room below the first row of each panel holds the Nuclear upstream note.
  scale_y_discrete(labels = lab_fun, expand = expansion(add = c(1.05, 0.6))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = 0.01)) +
  scale_colour_manual(values = paper_stage_cols, guide = "none") +
  scale_fill_manual(values = paper_stage_cols, guide = "none") +
  scale_shape_manual(
    name = NULL,
    values = c("Other economy" = 21, "Coalition member" = 21, "United States" = 24,
               "China" = 23),
    breaks = c("Other economy", "Coalition member", "United States", "China"),
    labels = c("Other economy", "Coalition member (stage colour)", "United States", "China")
  ) +
  guides(shape = guide_legend(override.aes = list(
    fill = c("#b4b2a9", paper_ink$secondary, paper_ink$primary, paper_ink$primary),
    alpha = 1, size = c(1.2, 1.6, 1.9, 1.9)))) +
  labs(x = "Energy Security score (0-1; higher = more secure), rankable cells", y = NULL) +
  theme_paper(base_size = 9, grid = "x") +
  theme(strip.text.y = element_text(angle = -90, hjust = 0.5, face = "bold"),
        panel.spacing.y = unit(8, "pt"),
        legend.position = "bottom",
        axis.text.y = element_text(size = 8, colour = paper_ink$primary))
save_paper_fig(p_dist, "es_distribution", width = paper_width_full, height = 6.6)

# ---------------------------------------------------------------------------
# Figure 2: es_category_contrib (faceted by stage; one colour per stage)
contrib_plot <- contrib_by_stage %>%
  mutate(present = mean_weight > 0,
         category = factor(category, levels = rev(categories_cfg)),
         lab = ifelse(present & mean_share >= 0.10, sprintf("%.0f%%", 100 * mean_share), NA))
contrib_xmax <- max(0.85, ceiling(10 * max(contrib_plot$mean_share, contrib_plot$mean_weight) + 1) / 10)
p_contrib <- ggplot(contrib_plot %>% filter(present),
                    aes(y = category)) +
  geom_col(aes(x = mean_share, fill = supply_chain), width = 0.66) +
  geom_point(aes(x = mean_weight, shape = "Mean effective weight of the category"),
             size = 4.2, colour = paper_ink$primary) +
  geom_text(aes(x = pmax(mean_share, mean_weight) + 0.02, label = lab), hjust = 0, size = 2.8,
            colour = paper_ink$secondary, na.rm = TRUE) +
  facet_grid(. ~ supply_chain) +
  scale_fill_manual(values = paper_stage_cols, guide = "none") +
  scale_shape_manual(name = NULL, values = c("Mean effective weight of the category" = 124)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, contrib_xmax),
                     breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(drop = FALSE) +
  labs(x = "Mean share of the cell's Energy Security score (rankable cells)", y = NULL) +
  theme_paper(base_size = 9, grid = "x") +
  theme(strip.text.x = element_text(face = "bold", hjust = 0),
        panel.spacing.x = unit(10, "pt"),
        axis.text.y = element_text(size = 8, colour = paper_ink$primary))
save_paper_fig(p_contrib, "es_category_contrib", width = paper_width_full, height = 3.4)

# ---------------------------------------------------------------------------
# Figure 3: es_heatmap (57 coalition members x 33 sectors; China for reference)
group_levels <- c("Core, outside the EU", "Core, EU members", "Partners", "Reference")
row_order <- member_means %>%
  mutate(group = factor(group, levels = group_levels)) %>%
  arrange(group, desc(mean_pct)) %>%
  select(Country, group)
stopifnot(nrow(row_order) == length(coalition_iso3))
row_order <- bind_rows(row_order, tibble(Country = chn, group = factor("Reference", levels = group_levels)))
heat_rows <- row_order$Country
sectors_plot <- sectors_all %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels),
         scored = paste(tech, supply_chain) %in% paste(pairs_scored$tech, pairs_scored$supply_chain))
heat_df <- tidyr::expand_grid(Country = heat_rows, sectors_plot) %>%
  left_join(es_scored %>% select(Country, tech, supply_chain, es, rankable),
            by = c("Country", "tech", "supply_chain")) %>%
  left_join(row_order, by = "Country") %>%
  mutate(
    Country_lab = factor(Country, levels = rev(heat_rows)),
    tech_lab = factor(tech_short[tech], levels = tech_short[sort(names(tech_short))]),
    status = case_when(!scored ~ "Sector without an ES score",
                       is.na(es) ~ "Economy not scored in the sector",
                       !rankable ~ "Scored, not rankable",
                       TRUE ~ "Rankable"),
    es_fill = ifelse(status == "Rankable", es, NA_real_)
  )
heat_counts <- heat_df %>% filter(Country != chn) %>% count(status, name = "n_cells")
nonrank_cells <- heat_df %>% filter(status == "Scored, not rankable")
missing_cells <- heat_df %>% filter(status == "Economy not scored in the sector")
unscored_cells <- heat_df %>% filter(status == "Sector without an ES score")
unscored_label <- unscored_cells %>%
  # The middle row of the largest group (the EU members), so the vertical label stays inside it.
  filter(Country == {eu_rows <- row_order$Country[row_order$group == "Core, EU members"];
                     eu_rows[ceiling(length(eu_rows) / 2)]}) %>%
  mutate(label = "no ES score")
heat_keys <- c("Scored, not rankable", "Not scored")
heat_key_df <- heat_df %>% slice(1:2) %>% mutate(key = factor(heat_keys, levels = heat_keys))
heat_strip <- c("Core, outside the EU" = "Core, outside the EU",
                "Core, EU members" = "Core, EU members (one node)",
                "Partners" = "Partners", "Reference" = "")
heat_row_lab <- function(x) ifelse(x == chn, paste(chn, "(reference)"), x)
p_heat <- ggplot(heat_df, aes(x = tech_lab, y = Country_lab)) +
  geom_tile(aes(fill = es_fill), colour = "white", linewidth = 0.3) +
  geom_tile(data = nonrank_cells, fill = "#d6d5ce", colour = "white", linewidth = 0.3) +
  geom_tile(data = unscored_cells, fill = "white", colour = "white", linewidth = 0.3) +
  geom_point(data = missing_cells, shape = 4, size = 0.9, stroke = 0.35,
             colour = paper_ink$muted) +
  geom_point(data = heat_key_df, aes(shape = key), size = 0, alpha = 0) +
  geom_text(data = unscored_label, aes(label = label), angle = 90, size = 2.5,
            colour = paper_ink$muted, fontface = "italic") +
  facet_grid(group ~ supply_chain, scales = "free", space = "free",
             labeller = labeller(group = heat_strip)) +
  scale_y_discrete(labels = heat_row_lab) +
  scale_shape_manual(name = NULL, values = c("Scored, not rankable" = 15, "Not scored" = 4),
                     guide = guide_legend(order = 2, override.aes = list(
                       size = c(3.6, 1.6), alpha = 1, stroke = c(0, 0.5),
                       colour = c("#d6d5ce", paper_ink$muted)))) +
  scale_fill_gradientn(colours = c("#eef5fe", paper_seq_blue), limits = c(0, 1),
                       breaks = seq(0, 1, 0.25), na.value = "white",
                       name = "Energy Security score",
                       guide = guide_colourbar(barwidth = unit(3.6, "cm"),
                                               barheight = unit(0.25, "cm"),
                                               title.vjust = 0.9, order = 1)) +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) +
  theme_paper(base_size = 9, grid = "none") +
  theme(axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 7.5,
                                       colour = paper_ink$primary),
        axis.text.y = element_text(size = 7, colour = paper_ink$primary),
        axis.line.x = element_blank(),
        strip.text.x = element_text(face = "bold", hjust = 0, margin = margin(b = 3)),
        strip.text.y = element_text(angle = -90, hjust = 0.5, size = 7.5, face = "bold",
                                    colour = paper_ink$secondary),
        strip.placement = "outside",
        panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(4, "pt"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0))
save_paper_fig(p_heat, "es_heatmap", width = paper_width_full, height = 7.1)

# ---------------------------------------------------------------------------
# Figure 4: es_top (dot plots of the top ten, rankable cells)
top_cells <- tibble(tech = c("Oil", "Batteries", "Solar"),
                    supply_chain = c("Upstream", "Midstream", "Midstream")) %>%
  mutate(panel = paste(tech, tolower(supply_chain), sep = ", "))
top_df <- es %>%
  mutate(stage = as.character(supply_chain)) %>%
  inner_join(top_cells, by = c("tech", "stage" = "supply_chain")) %>%
  group_by(panel) %>%
  arrange(rank, Country, .by_group = TRUE) %>%
  mutate(pos = row_number(), in_top = pos <= 10) %>%
  filter(in_top | iso3c %in% c("USA", "CHN")) %>%
  ungroup() %>%
  mutate(label = sprintf("%d  %s", rank, Country),
         label = ifelse(member, paste0("**", label, "**"), label),
         panel = factor(panel, levels = top_cells$panel),
         grp = case_when(iso3c == "USA" ~ "United States", iso3c == "CHN" ~ "China",
                         member ~ "Coalition member", TRUE ~ "Other economy"),
         key = paste(panel, Country, sep = "|"))
top_key_levels <- top_df %>% arrange(panel, desc(pos)) %>% pull(key)
top_label_map <- setNames(top_df$label, top_df$key)
top_df <- top_df %>% mutate(key = factor(key, levels = top_key_levels))
top_median <- es %>%
  mutate(stage = as.character(supply_chain)) %>%
  inner_join(top_cells, by = c("tech", "stage" = "supply_chain")) %>%
  group_by(panel) %>% summarise(median = median(es), n = n(), .groups = "drop") %>%
  mutate(panel = factor(panel, levels = top_cells$panel))
# The cell-constant inputs (Energy Prices and the world-concentration term of Trade) give
# every economy of a cell the same contribution.
top_const <- var_contrib %>%
  inner_join(top_cells %>% select(tech, supply_chain, panel), by = c("tech", "supply_chain")) %>%
  semi_join(constant_vars, by = c("category", "variable")) %>%
  group_by(panel, Country) %>%
  summarise(contrib = sum(weighted_variable_contribution), .groups = "drop") %>%
  group_by(panel) %>%
  summarise(value = mean(contrib), spread = max(contrib) - min(contrib), .groups = "drop") %>%
  mutate(panel = factor(panel, levels = top_cells$panel))
stopifnot(all(top_const$spread < 1e-9))
# Hairline above the appended United States / China rows.
top_sep <- top_df %>% group_by(panel) %>%
  summarise(n_app = sum(!in_top), .groups = "drop") %>%
  filter(n_app > 0) %>% transmute(panel, y = n_app + 0.5)
top_med_lab <- top_median %>%
  mutate(y = Inf, label = sprintf("median of %d rankable economies", n))

# Category contributions behind the three cells: every plotted economy's weighted category
# contributions, and the gap between the top ten and all rankable economies by category.
top_contrib <- top_df %>%
  select(panel, Country, iso3c, tech, stage, es, rank, in_top) %>%
  inner_join(cat_contrib %>%
               select(Country, tech, supply_chain, category, category_score, category_weight,
                      weighted_category_contribution),
             by = c("Country", "tech", "stage" = "supply_chain")) %>%
  arrange(panel, rank, category)
top_contrib_summary <- es %>%
  mutate(stage = as.character(supply_chain)) %>%
  inner_join(top_cells, by = c("tech", "stage" = "supply_chain")) %>%
  select(panel, Country, tech, stage, rank) %>%
  inner_join(cat_contrib %>%
               select(Country, tech, supply_chain, category, category_score,
                      weighted_category_contribution),
             by = c("Country", "tech", "stage" = "supply_chain")) %>%
  group_by(panel, category) %>%
  summarise(mean_contrib_all = mean(weighted_category_contribution),
            mean_contrib_top10 = mean(weighted_category_contribution[rank <= 10]),
            gap_top10_minus_all = mean_contrib_top10 - mean_contrib_all,
            mean_score_all = mean(category_score),
            mean_score_top10 = mean(category_score[rank <= 10]),
            leader_contrib = weighted_category_contribution[rank == 1][1],
            .groups = "drop") %>%
  group_by(panel) %>%
  mutate(share_of_es_gap = gap_top10_minus_all / sum(gap_top10_minus_all)) %>%
  ungroup() %>%
  arrange(panel, desc(gap_top10_minus_all))

top_shapes <- c("Coalition member" = 21, "Other economy" = 21, "United States" = 24, "China" = 23)
const_key <- "Cell-constant part of the score (the same for every economy)"
p_top <- ggplot(top_df, aes(x = es, y = key)) +
  # Train the discrete y scale on every row first, so the factor order (top ten, then the
  # appended United States / China rows at the bottom) is kept in each panel.
  geom_blank() +
  geom_rect(data = top_const, aes(xmin = 0, xmax = value, ymin = -Inf, ymax = Inf,
                                  alpha = const_key),
            inherit.aes = FALSE, fill = paper_ink$neutral) +
  geom_vline(data = top_median, aes(xintercept = median), colour = paper_ink$muted,
             linewidth = 0.35) +
  geom_text(data = top_med_lab, aes(x = median, y = y, label = label), inherit.aes = FALSE,
            hjust = -0.04, vjust = 1.2, size = 2.5, colour = paper_ink$secondary) +
  geom_hline(data = top_sep, aes(yintercept = y), colour = paper_ink$axis, linewidth = 0.3) +
  geom_point(data = top_df %>% filter(grp %in% c("Coalition member", "Other economy")),
             aes(fill = ifelse(grp == "Coalition member", as.character(stage), "Other"),
                 shape = grp), size = 2.3, colour = "white", stroke = 0.35) +
  geom_point(data = top_df %>% filter(grp %in% c("United States", "China")),
             aes(shape = grp), fill = paper_ink$primary, colour = "white", size = 2.5,
             stroke = 0.35) +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
  # Extra room above the first row of each panel holds the median label.
  scale_y_discrete(labels = function(x) unname(top_label_map[x]),
                   expand = expansion(add = c(0.6, 1.2))) +
  scale_alpha_manual(name = NULL, values = setNames(1, const_key),
                     guide = guide_legend(order = 2, override.aes = list(colour = NA))) +
  scale_fill_manual(values = c(paper_stage_cols, Other = "#b4b2a9"), guide = "none") +
  scale_shape_manual(
    name = NULL, values = top_shapes,
    breaks = c("Coalition member", "Other economy", "United States", "China"),
    labels = c("Coalition member (stage colour, bold)", "Other economy", "United States",
               "China")) +
  guides(shape = guide_legend(order = 1, nrow = 1, override.aes = list(
    fill = c(paper_ink$secondary, "#b4b2a9", paper_ink$primary, paper_ink$primary),
    size = c(2.3, 2.3, 2.5, 2.5)))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Energy Security score (0-1); rank among the rankable economies of the cell", y = NULL) +
  theme_paper(base_size = 9, grid = "xy") +
  theme(axis.text.y = ggtext::element_markdown(size = 8, colour = paper_ink$primary),
        strip.text.y = element_text(angle = -90, hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_line(colour = "#efeee9", linewidth = 0.25),
        panel.spacing.y = unit(8, "pt"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(2, "pt"),
        legend.margin = margin(0, 0, 0, 0))
save_paper_fig(p_top, "es_top", width = paper_width_full, height = 5.6)

# ---------------------------------------------------------------------------
# Figure 5: es_nuclear_upstream (rankable cells). The highest-ranked economies of Nuclear
# upstream with their score split into the four categories, and one row for the economies at the
# base value (no uranium resources or mine output, uranium trade imputed): Energy Prices plus the
# imputed Trade category.
nu_n_top <- 15
nu_cat_levels <- c("Energy Prices", "Trade", "Reserves", "Production")
nu_cat_labels <- c(
  "Energy Prices" = "Energy Prices (same for every economy)",
  "Trade" = "Trade (uranium ores and concentrates)",
  "Reserves" = "Reserves (Red Book identified resources)",
  "Production" = "Production (uranium mine output)"
)
nu_cat_cols <- c("Energy Prices" = "#d6d5ce", "Trade" = unname(paper_cat["magenta"]),
                 "Reserves" = unname(paper_cat["yellow"]),
                 "Production" = unname(paper_cat["violet"]))
nu_rows <- nu_rank %>% filter(rank <= nu_n_top) %>% arrange(rank, Country) %>%
  transmute(Country, iso3c, rank, member, es,
            label = ifelse(member, sprintf("**%d %s**", rank, Country),
                           sprintf("%d %s", rank, Country)))
stopifnot(nrow(nu_rows) == nu_n_top, all(!at_base(nu_rows$es)))
floor_label <- sprintf("%d economies at the base value", nuclear_upstream$n_at_base_rankable)
nu_bar <- nu_cc %>%
  semi_join(nu_rows, by = "Country") %>%
  select(Country, category, weighted_category_contribution) %>%
  left_join(nu_rows %>% select(Country, label), by = "Country") %>%
  bind_rows(tibble(Country = floor_label, label = floor_label, category = nu_cat_levels,
                   weighted_category_contribution = c(nu_ep, nu_floor - nu_ep, 0, 0)))
nu_levels <- rev(c(nu_rows$label, floor_label))
nu_bar <- nu_bar %>%
  mutate(label = factor(label, levels = nu_levels),
         category = factor(category, levels = rev(nu_cat_levels)))
nu_sum_check <- nu_bar %>% filter(Country != floor_label) %>% group_by(Country) %>%
  summarise(s = sum(weighted_category_contribution), .groups = "drop") %>%
  left_join(nu_rows %>% select(Country, es), by = "Country")
stopifnot(max(abs(nu_sum_check$s - nu_sum_check$es)) < 1e-9)
nu_floor_text <- tibble(
  label = factor(floor_label, levels = nu_levels), x = nu_floor + 0.012,
  txt = sprintf("%.3f: no uranium, trade imputed. All %d without uranium: %.3f-%.3f",
                nu_floor, nuclear_upstream$n_without_uranium_rankable,
                nuclear_upstream$range_without_uranium_rankable[1],
                nuclear_upstream$range_without_uranium_rankable[2]))
p_nu <- ggplot(nu_bar, aes(x = weighted_category_contribution, y = label, fill = category)) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.35) +
  geom_hline(yintercept = 1.5, colour = paper_ink$axis, linewidth = 0.3) +
  geom_text(data = nu_floor_text, aes(x = x, y = label, label = txt), inherit.aes = FALSE,
            hjust = 0, size = 2.6, colour = paper_ink$secondary) +
  scale_fill_manual(values = nu_cat_cols, breaks = nu_cat_levels, labels = nu_cat_labels,
                    name = NULL, guide = guide_legend(ncol = 2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Energy Security score, Nuclear upstream (sum of weighted category contributions)",
       y = NULL) +
  theme_paper(base_size = 9, grid = "x") +
  theme(axis.text.y = ggtext::element_markdown(size = 8, colour = paper_ink$primary),
        legend.position = "bottom", legend.justification = "left",
        legend.margin = margin(0, 0, 0, 0), legend.text = element_text(size = 7.5))
save_paper_fig(p_nu, "es_nuclear_upstream", width = paper_width_full, height = 4.0)

nu_eff_w <- nuclear_upstream$effective_weights
nuclear_upstream$configured_weights <- as.list(category_weights[nu_cat_levels])
nuclear_upstream$weight_note <- sprintf(
  "effective weights within the pair: %s (configured weights renormalised over the four categories present)",
  paste(sprintf("%s %.4f", nu_eff_w$category, nu_eff_w$weight), collapse = ", "))
nuclear_upstream$figure_rows <- nu_bar %>%
  mutate(label = as.character(label), category = as.character(category)) %>%
  select(Country, category, weighted_category_contribution)
nuclear_upstream$n_producers_rankable <- sum(nu$`category_score_Production`[nu$rankable] > tol)
nuclear_upstream$n_reserves_rankable <- sum(nu$`category_score_Reserves`[nu$rankable] > tol)
nuclear_upstream$producers_rankable <- nu %>%
  filter(rankable, `category_score_Production` > tol) %>% arrange(desc(es)) %>% pull(iso3c)
nuclear_upstream$n_members_rankable <- sum(nu$member & nu$rankable)
nuclear_upstream$n_members_without_uranium <- sum(!nu$has_uranium & nu$member)
nuclear_upstream$members_in_top15 <- sum(nu_rows$member)
nuclear_upstream$top15 <- nu_rows %>% select(Country, iso3c, rank, member, es)
# Lowest score among economies with uranium, and the median score of the holders.
nuclear_upstream$min_es_with_uranium_rankable <- min(nu$es[nu$rankable & nu$has_uranium])
nuclear_upstream$median_es_with_uranium_rankable <- median(nu$es[nu$rankable & nu$has_uranium])
nuclear_upstream$n_with_uranium_below_max_without <- sum(
  nu$es[nu$rankable & nu$has_uranium] < max(nu_nou_r$es))
nuclear_upstream$with_uranium_below_base <- nu %>%
  filter(rankable, has_uranium, es < nu_floor) %>% transmute(Country, iso3c, es)
nu_ranked <- nu %>% filter(rankable) %>%
  left_join(nu_rank %>% select(Country, rank), by = "Country") %>%
  mutate(producer = `category_score_Production` > tol)
nuclear_upstream$worst_rank_producer <- max(nu_ranked$rank[nu_ranked$producer])
nuclear_upstream$best_rank_non_producing_holder <- min(nu_ranked$rank[!nu_ranked$producer & nu_ranked$has_uranium])
nuclear_upstream$best_non_producing_holder <- nu_ranked %>%
  filter(!producer, has_uranium) %>% slice_min(rank, n = 1, with_ties = FALSE) %>%
  transmute(Country, iso3c, es, rank)
nuclear_upstream$best_rank_without_uranium <- min(nu_ranked$rank[!nu_ranked$has_uranium])
# Economies without uranium: their zeros on Reserves and Production are filled by the zero rule
# and count as imputed, as does an imputed Trade category; they remain rankable while the imputed
# weight stays at or below tau.
nuclear_upstream$imputed_share_without_uranium_rankable <- nu_ranked %>% filter(!has_uranium) %>%
  count(imputed_weight_share = round(imputed_weight_share, 4), name = "n_cells")
nuclear_upstream$share_members_without_uranium <- mean(!nu$has_uranium[nu$member])

# ---------------------------------------------------------------------------
# Table: leaders by pair (rankable cells)
fmt <- function(v) sprintf("%.2f", v)
# A rank shared by several economies carries the size of the tie as a superscript.
fmt_rank <- function(r, tie) {
  ifelse(is.na(r), "--", paste0(r, ifelse(!is.na(tie) & tie > 1, sprintf("$^{%d}$", tie), "")))
}
tab_rows <- leaders %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels)) %>%
  arrange(supply_chain, tech) %>%
  mutate(
    row = sprintf("%s & %d & %s %s%s & %s %s%s & %s %s%s & %s (%s) & %s & %s \\\\",
                  tech, n, leader_iso3, fmt(leader_es),
                  ifelse(n_tied_at_top > 1, sprintf("$^{%d}$", n_tied_at_top), ""),
                  second_iso3, fmt(second_es),
                  ifelse(second_tie > 1, sprintf("$^{%d}$", second_tie), ""),
                  third_iso3, fmt(third_es),
                  ifelse(third_tie > 1, sprintf("$^{%d}$", third_tie), ""),
                  best_member_iso3, fmt_rank(best_member_rank, best_member_tie),
                  fmt_rank(usa_rank, usa_tie), fmt_rank(chn_rank, chn_tie))
  )
tab_lines <- c(
  "% Generated by scripts/paper/fig_es.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Leading economies in each technology-stage pair of the Energy Security pillar (rankable cells)}",
  "\\label{tab:es_leaders}",
  "\\footnotesize",
  "\\begin{tabular}{@{}lrlllcrr@{}}",
  "\\toprule",
  "Technology & $n$ & First & Second & Third & Best member (rank) & USA & CHN \\\\",
  "\\midrule"
)
for (st in stage_levels) {
  tab_lines <- c(tab_lines, sprintf("\\multicolumn{8}{@{}l}{\\textit{%s}}\\\\", st),
                 tab_rows$row[tab_rows$supply_chain == st])
  if (st != "Downstream") tab_lines <- c(tab_lines, "\\addlinespace")
}
unscored_txt <- pairs_unscored %>%
  mutate(supply_chain = factor(supply_chain, levels = stage_levels)) %>%
  arrange(supply_chain, tech) %>%
  mutate(txt = paste(tech, tolower(supply_chain))) %>% pull(txt)
tab_lines <- c(
  tab_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} $\\ES_{its}$ for the eleven headline technologies, final run. ",
    "Only rankable cells enter: a sovereign economy with at most half of the cell's index ",
    "weight imputed. Economies are identified by ISO3 code and followed by their score; $n$ is ",
    "the number of rankable economies in the pair. ``Best member'' is the highest-ranked of the ",
    sprintf("%d coalition members, with its rank in parentheses; ", length(coalition_iso3)),
    "the USA and CHN columns give the ranks of the United States and China. A rank is one ",
    "plus the number of rankable economies with a strictly higher score, so tied economies ",
    "share a rank, and a superscript gives the number of economies that share it (the economy listed is the first of them alphabetically). ",
    sprintf("%s %s no Energy Security score. ",
            if (length(unscored_txt) > 1) {
              paste(paste(head(unscored_txt, -1), collapse = ", "), "and", tail(unscored_txt, 1))
            } else unscored_txt,
            ifelse(length(unscored_txt) == 1, "has", "have")),
    "Source: \\texttt{energy\\_security\\_index} in \\texttt{index\\_outputs.rds}; ",
    "\\texttt{scripts/paper/fig\\_es.R}."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tables_dir <- paper_dir("tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
writeLines(tab_lines, file.path(tables_dir, "es_leaders.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------
# Derived values quoted in the text (each recomputed here from the objects above).
text_numbers <- list(
  nuclear_upstream_need_at_base = 1 - nu_floor,
  share_rankable_by_stage = rank_by_stage %>% transmute(supply_chain, share = n_rankable / n_scored),
  pairs_led_by_china = leaders %>% filter(chn_rank == 1) %>% transmute(pair = paste(tech, supply_chain)),
  pairs_usa_above_china = leaders %>% filter(usa_rank < chn_rank) %>%
    transmute(pair = paste(tech, supply_chain), usa_rank, chn_rank),
  n_midstream_pairs_usa_above_china = sum(leaders$usa_rank < leaders$chn_rank &
                                            leaders$supply_chain == "Midstream"),
  upstream_pairs_iqr_below_0_05 = dist_by_pair %>% filter(supply_chain == "Upstream", iqr < 0.05) %>%
    transmute(tech, iqr),
  trade_only_midstream_pairs = dist_by_pair %>% filter(supply_chain == "Midstream") %>%
    semi_join(cat_sets %>% filter(set == "Trade"), by = c("tech", "supply_chain")) %>%
    transmute(tech, median),
  taiwan_n_scored_cells = nrow(taiwan_scored),
  taiwan_n_rankable_cells = sum(taiwan_scored$rankable),
  taiwan_trade_rank_range_midstream = range(taiwan_trade_mid$trade_rank),
  members_x_scored_pairs = length(coalition_iso3) * nrow(pairs_scored),
  members_rankable_in_fewer_than_15_pairs = member_rankable_cells %>% filter(n_rankable < 15) %>%
    transmute(iso3c, n_scored, n_rankable),
  members_in_top10_batteries_mid = leaders$members_in_top10[leaders$tech == "Batteries" &
                                                             leaders$supply_chain == "Midstream"],
  members_in_top10_solar_mid = leaders$members_in_top10[leaders$tech == "Solar" &
                                                         leaders$supply_chain == "Midstream"],
  members_in_top10_oil_up = leaders$members_in_top10[leaders$tech == "Oil" &
                                                      leaders$supply_chain == "Upstream"]
)

# ---------------------------------------------------------------------------
# Results
results <- list(
  text_numbers = text_numbers,
  sample = list(
    source = "data/processed/outputs/index_outputs.rds (energy_security_index, headline techs; final rerun 2026-10-12)",
    rankable_rule = sprintf("rankable == TRUE: sovereign economy and imputed_weight_share <= %.2f (config/rankability.yml); rankings, top-N lists, distributions, FE decompositions, correlations, contribution averages and coalition averages use rankable cells only", tau),
    coalition = sprintf("io$coalition_members: %d members (%d core, %d partners); the %d EU members among them form one network node (EUU)", length(coalition_iso3), sum(coalition_tbl$tier == "core"), sum(coalition_tbl$tier == "partner"), sum(coalition_tbl$eu_member)),
    coalition_iso3 = coalition_iso3,
    coalition_core_iso3 = core_iso3,
    coalition_eu_iso3 = eu_iso3,
    rank_rule = "rank = 1 + number of rankable economies with strictly higher ES in the pair; pct = (n - rank)/(n - 1)",
    correlation_method = "Spearman, within technology-stage pair across rankable economies"
  ),
  coverage = coverage,
  joint_construction_diagnostics_headline = joint_diagnostics,
  category_weights = as.list(category_weights),
  distribution_by_stage = dist_by_stage,
  distribution_all = dist_all,
  distribution_by_stage_all_scored_cells = dist_by_stage_scored,
  distribution_by_pair = dist_by_pair,
  distribution_pair_summary_by_stage = dist_pair_iqr_summary,
  variance_decomposition = variance,
  leaders_by_pair = leaders,
  leader_counts = leader_counts,
  leader_summary = leader_summary,
  coalition_vs_others_by_stage = member_stage,
  coalition_tier_by_stage = member_tier_stage,
  coalition_eu_vs_noneu_by_stage = member_eu_stage,
  coalition_member_means = member_means,
  coalition_member_rankable_cells = member_rankable_cells,
  coalition_member_best_by_stage = member_stage_best,
  coalition_member_worst_by_stage = member_stage_worst,
  heatmap_cell_status_members = heat_counts,
  usa_china_taiwan_by_stage = focus_positions,
  usa_china_taiwan_overall = focus_overall,
  usa_china_taiwan_by_pair = focus_pairs,
  taiwan_all_scored_cells = taiwan_scored,
  taiwan_trade_category_midstream = taiwan_trade_mid,
  strategic_index_sample = si_sample,
  china_oil_downstream_components = china_oil_downstream,
  nuclear_upstream = nuclear_upstream,
  contribution_share_by_stage = contrib_by_stage,
  contribution_cells_by_stage = contrib_cells_by_stage,
  contribution_share_upstream_excluding_nuclear = contrib_upstream_excl_nuclear,
  contribution_top3_by_stage = contrib_top3,
  contribution_cells_dropped_es_zero = contrib_cells_dropped,
  cell_constant_inputs = list(
    definition = "Energy Prices (Overall Price Volatility Index), Trade HHI, Minerals Trade critmin_hhi; effective weight = category_weight x component_weight_within_overall",
    exactly_constant_variables = var_spread %>% filter(range < 1e-12) %>% select(tech, supply_chain, category, variable),
    critmin_hhi_nonmodal_economies = critmin_hhi_exceptions$Country,
    mean_effective_weight_by_stage = const_weight_by_stage,
    mean_effective_weight_by_pair = const_weight_by_pair
  ),
  energy_prices_by_tech = energy_prices,
  category_spearman_within_pair = cat_cor,
  category_spearman_by_stage = cat_cor_by_stage,
  category_spearman_stage_summary = cat_cor_stage_summary,
  category_vs_es_spearman_by_stage = cat_es_cor_by_stage,
  category_resolution_by_pair = cat_resolution,
  category_resolution_by_stage = cat_resolution_by_stage,
  investment_critical_minerals_split = as.list(investment_split),
  mineral_demand_shares_base_year = mineral_demand_shares,
  top_cells = top_df %>% transmute(panel = as.character(panel), Country, iso3c, es, rank,
                                   member, in_top, pos),
  top_cells_median = top_median,
  top_cells_constant_part = top_const %>% select(panel, value),
  top_cells_category_contributions = top_contrib,
  top_cells_category_gap_top10_vs_all = top_contrib_summary
)
write_paper_results(results, "es")
message("fig_es.R: done")
