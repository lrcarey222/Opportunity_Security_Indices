# Robustness and validation of the pillars, the Strategic Index and the PSI (Section 11).
#
# Every number is computed from the outputs of the FINAL full rerun of 2026-10-12 (scripts 04 10 12
# 13 20 15 25, then 30 at the calibrated lambda 0.78 and 40), which supersedes the run of 2026-10-10:
#   data/processed/outputs/index_outputs.rds   ES / EO (with imputed_weight_share, rankable),
#                                              Policy components (IEA PAMS 3 + CAT 2, IEA PAMS
#                                              required; the stored February 2026 NIPO rows, not
#                                              used), Strategic Index (with the endowment screen),
#                                              coalition members
#   data/processed/*_tbl.rds                   the theme tables scripts/20 feeds the builders;
#                                              tech_ghg_tbl, coalition_tbl; psi_tbl (core view),
#                                              psi_full_coalition_tbl, psi_world_tbl
#   data/processed/vintages/*.csv              2020 and 2025 vintages (scripts/40)
#   data/processed/charts/allied_strategic.csv baseline allied picks (checked, not used)
#   data/raw/{ei_stat_review_world_energy, eia_intl_fossil_production, uranium_production}.csv and
#                                              the IEA critical minerals workbook: the two fossil
#                                              themes rebuilt with zero in place of the EIA fill
#   config/*.yml                               weights, missing-data rules, rankability tau
# and three study folders outside the repository (<scratch> = OPSI_SCRATCH_DIR, by default
# ../scratch_final next to the worktree; read only):
#   <scratch>/nipo_oct/nipo_component_oct2026.rds   the October 2026 NIPO component (GTA NIPO,
#                                              as of 5 Oct 2026, debiased strength of PR #333; 77
#                                              jurisdictions; built as scripts/10 builds the NIPO
#                                              Policy Index rows; README.txt)
#   <scratch>/vertical/{eo_linkages,es_input_security}   the vertical-complementarity study of
#                                              2026-10-12 (Atlas HS92 cell panel, implied weights,
#                                              sourcing-weighted input security)
#   <scratch>/fixes7/pipeline/out              the CM-FDI integration study of 2026-10-11 (PSI
#                                              with and without the CM-FDI parts of Friendshore)
#
# Rank statistics are Spearman rank correlations. Cross-country rankings are taken within
# technology-stage pairs of the eleven headline technologies and over RANKABLE cells only
# (rankable == TRUE in the baseline: sovereign economy, imputed weight share <= tau = 0.5,
# config/rankability.yml). A variant is compared with the baseline on the baseline's rankable
# cells, so that every comparison is made on one fixed set of cells; pairs with fewer than
# three rankable cells or a constant baseline among them cannot rank and are left out.
#
# Analyses (keys of paper/results/robustness.json):
#   vintage      2020 vs 2025 vintage (scripts/40): within-pair Spearman, level changes,
#                movers, and the share of pillar weight that re-derives with the vintage year.
#   weights      Dirichlet draws of the category weights centred on config/weights.yml (CV 0.3
#                for a category of average weight), ES and EO recomputed with the available-case
#                rule; pair structure; equal weights; variance-based importance; rank bands of
#                coalition members.
#   loo          leave one category out.
#   gamma        S-curve curvature gamma in {0.25, 1, 2}: every index row of every theme table
#                re-curved exactly (logit phi_g = (g / 0.5) logit phi_0.5) and ES / EO rebuilt
#                with the pipeline's builders.
#   imputation   every 'global_average' rule removed (available case) or replaced by 'zero'.
#   rankability  tau in {0.3, 0.5, 0.7}: rankable cells, robustness of the rankable rankings to
#                the imputation rule, top-10 lists, the Strategic Index sample and the picks,
#                and the PSI's ranked partners.
#   si           Strategic Index: Dirichlet draws of the five weights, and discrete variants
#                (security ES for need 1 - ES, no sector priors, all terms ranked within
#                country, no policy term, equal weights, NIPO added back, endowment screen off),
#                with their effect on the allied picks.
#   policy       the Policy index: both components required (OPSI_POLICY_REQUIRE_ALL=1, the
#                rule until 9 Oct 2026; picks of the members with a CAT rating), IEA PAMS alone
#                for every country (CAT dropped), and the October 2026 NIPO component added
#                back with its former weight (NIPO 4, IEA PAMS 3, CAT 2; IEA PAMS still required).
#   pick_stability  margins of the picks (and how many lie within 0.005 and 0.01 SI of the
#                runner-up), the picks recorded for the interim builds, and the picks when the
#                pillars are rebuilt from the category scores with one category dropped, with the
#                Trade category measured on the Atlas basis of the 2025 vintage, or from the 2020
#                / 2025 vintage pillars.
#   data_choices the data choices of 2026-10-11/12: fossil production EI does not report filled
#                from EIA vs zero-filled (the two themes rebuilt with the pipeline's builders);
#                the new Upstream trade baskets of EV, Wind, Nuclear and Solar removed (their
#                Trade category dropped); the CM-FDI parts of Friendshore off (from the CM-FDI
#                integration study).
#   vertical     appendix sensitivity of the vertical-complementarity study: the within-country-
#                deviation EO linkage term at the evidence weights and the sourcing-weighted ES
#                input-security term at a 15% share, each next to its permutation placebo.
#   psi_views    PSI from the core (headline), full-coalition and world viewpoints.
#
# Output: paper/figures/robustness_{variants,vintage,ranks,loo,si,picks_data,psi_views}.pdf
# (+ png), paper/tables/robustness_{summary,importance,rankability,picks,data,vertical}.tex and
# paper/results/robustness.json. Run from the repository root:
#   Rscript scripts/paper/fig_robustness.R

find_repo_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "paper", "_paper_theme.R"))) {
    if (dirname(d) == d) stop("Run from inside the repository.")
    d <- dirname(d)
  }
  d
}

paper_root <- find_repo_root()
source(file.path(paper_root, "scripts", "paper", "_paper_theme.R"))
# Packages, repo_root and the runtime configs, as every pipeline script.
source(file.path(paper_root, "scripts", "utils", "bootstrap.R"))

for (f in c("R/utils/scurve.R", "R/utils/country.R", "R/utils/schema.R", "R/utils/levels.R",
            "R/indices/build_energy_security_index_v2.R",
            "R/indices/build_economic_opportunity_index_v2.R",
            "R/indices/build_policy_index.R",
            "R/indices/build_indices.R")) {
  source(file.path(repo_root, f))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
})

# Same YAML as scripts/20_build_indices.R (reload_runtime_configs() and the rankability rule).
rankability_cfg <- yaml::read_yaml(file.path(repo_root, "config", "rankability.yml"))
options(
  opportunity_security.config = yaml::read_yaml(file.path(repo_root, "config", "config.yml")),
  opportunity_security.weights = yaml::read_yaml(file.path(repo_root, "config", "weights.yml")),
  opportunity_security.missing_data = yaml::read_yaml(file.path(repo_root, "config", "missing_data.yml")),
  opportunity_security.index_definition = yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml")),
  opportunity_security.rankability = rankability_cfg
)
config <- getOption("opportunity_security.config")
weights_cfg <- getOption("opportunity_security.weights")
missing_cfg <- getOption("opportunity_security.missing_data")
allow_partial_categories <- isTRUE(config$allow_partial_categories)
include_sub_sector <- isTRUE(if (!is.null(config$include_sub_sector)) config$include_sub_sector else
  config$energy_security_include_sub_sector)
processed_dir <- file.path(repo_root, config$processed_dir)

HEADLINE_TECHS <- c("Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind",
                    "Oil", "Solar", "Gas", "Geothermal", "Electric Grid")
STAGES <- c("Upstream", "Midstream", "Downstream")
SEED <- 20261005L
N_DRAWS <- 1000L
TARGET_CV <- 0.3
GAMMA_BASE <- 0.5
GAMMAS <- c(0.25, 1, 2)
TAU_BASE <- as.numeric(rankability_cfg$max_imputed_weight_share)
TAUS <- c(0.3, TAU_BASE, 0.7)
TAU_TOL <- 1e-9   # as flag_rankable_cells()
TOL <- 1e-12
TOP_N <- 10L
PSI_TOP <- 15L
N_PLACEBO <- 200L   # permutation placebos of the vertical-complementarity terms
# Study folders outside the repository (read only): OPSI_SCRATCH_DIR, by default the scratch_final
# folder next to the worktree (as scripts/87 finds its pipeline marker).
scratch_dir <- Sys.getenv("OPSI_SCRATCH_DIR", file.path(dirname(repo_root), "scratch_final"))
if (!dir.exists(scratch_dir)) stop("Study folder not found: ", scratch_dir, " (set OPSI_SCRATCH_DIR)")
stopifnot(TAU_BASE == 0.5)

results <- list(meta = list(
  built = format(Sys.time(), "%Y-%m-%d %H:%M"),
  inputs = "data/processed (final full rerun of 2026-10-12; network at lambda 0.78 on 37 nodes)",
  rank_statistic = "Spearman rank correlation",
  pair_rule = paste("within technology-stage pairs of the 11 headline technologies, over the baseline's",
                    "rankable cells (sovereign, imputed weight share <= tau); pairs with fewer than 3",
                    "rankable cells or a constant baseline among them are left out"),
  headline_techs = HEADLINE_TECHS,
  tau = TAU_BASE,
  seed = SEED, n_draws = N_DRAWS, target_cv_at_mean_weight = TARGET_CV
))
save_results <- function() write_paper_results(results, "robustness")

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

key_of <- function(df) paste(df$Country, df$tech, df$supply_chain, sep = "\r")
pair_of <- function(df) paste(df$tech, df$supply_chain, sep = " - ")

q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE, names = FALSE))

safe_spearman <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 3) return(NA_real_)
  a <- a[ok]; b <- b[ok]
  if (stats::sd(a) <= TOL || stats::sd(b) <= TOL) return(NA_real_)
  stats::cor(a, b, method = "spearman")
}

# Within-pair Spearman of two index vectors aligned on `keys`, over the rows flagged `keep`.
pair_spearman <- function(keys, base, alt, keep) {
  tibble::tibble(pair = pair_of(keys), base = base, alt = alt) %>%
    dplyr::filter(keep) %>%
    dplyr::group_by(.data$pair) %>%
    dplyr::summarise(
      n = sum(!is.na(.data$base) & !is.na(.data$alt)),
      rho = safe_spearman(.data$base, .data$alt),
      .groups = "drop"
    )
}

summarise_rho <- function(tbl) {
  r <- tbl$rho
  i_min <- which.min(r)
  list(
    n_pairs = sum(!is.na(r)),
    n_pairs_undefined = sum(is.na(r)),
    median = stats::median(r, na.rm = TRUE),
    p25 = q(r, 0.25), p75 = q(r, 0.75),
    min = if (length(i_min)) r[i_min] else NA_real_,
    min_pair = if (length(i_min)) tbl$pair[i_min] else NA_character_,
    max = max(r, na.rm = TRUE),
    share_pairs_ge_0.9 = mean(r >= 0.9, na.rm = TRUE)
  )
}

# Dirichlet draws, K x n, by normalised gammas.
rdirichlet_cols <- function(n, alpha) {
  g <- matrix(stats::rgamma(n * length(alpha), shape = alpha), nrow = length(alpha))
  sweep(g, 2, colSums(g), "/")
}

# Concentration c of Dirichlet(c * wbar) such that a category of average weight 1/K has
# coefficient of variation TARGET_CV: CV(w) = sqrt((1 - w) / (w (c + 1))).
dirichlet_concentration <- function(K, cv = TARGET_CV) (K - 1) / cv^2 - 1
dirichlet_cv <- function(w, conc) sqrt((1 - w) / (w * (conc + 1)))

# Available-case weighted mean, as compute_index_and_contributions():
#   index = sum_k w_k c_k 1[c_k observed] / sum_k w_k 1[c_k observed], NA when nothing observed.
# C is cells x categories (NA = unobserved); W is categories x draws.
ac_index <- function(C, W) {
  W <- as.matrix(W)
  O <- !is.na(C)
  C0 <- C
  C0[!O] <- 0
  num <- C0 %*% W
  den <- (O * 1) %*% W
  out <- num / den
  out[den <= 0] <- NA_real_
  out
}

# Rankability at threshold tau, as flag_rankable_cells(): scored, sovereign and imputed weight
# share <= tau (within TAU_TOL). Sovereignty is read from the baseline's rank_exclusion, which
# names it whatever the threshold.
rankable_at <- function(value, share, rank_exclusion, tau) {
  sovereign <- !grepl("not a sovereign economy", rank_exclusion)
  !is.na(value) & sovereign & !is.na(share) & share <= tau + TAU_TOL
}
retag_rankable <- function(tbl, col, tau) {
  sovereign <- !grepl("not a sovereign economy", tbl$rank_exclusion)
  over <- !is.na(tbl$imputed_weight_share) & tbl$imputed_weight_share > tau + TAU_TOL
  tbl$rank_exclusion <- dplyr::case_when(
    is.na(tbl[[col]]) ~ "not scored",
    !sovereign & over ~ paste0("not a sovereign economy and imputed weight share above ", format(tau)),
    !sovereign ~ "not a sovereign economy",
    is.na(tbl$imputed_weight_share) ~ "imputed weight share unknown",
    over ~ paste0("imputed weight share above ", format(tau)),
    TRUE ~ NA_character_
  )
  tbl$rankable <- is.na(tbl$rank_exclusion)
  tbl
}

sector_short <- function(x) {
  x <- sub(" - Upstream$", " Up", x)
  x <- sub(" - Midstream$", " Mid", x)
  x <- sub(" - Downstream$", " Down", x)
  x <- sub("^Electric Grid", "Grid", x)
  x <- sub("^Electric Vehicles", "EV", x)
  x
}

# ---------------------------------------------------------------------------
# Final outputs and the coalition
# ---------------------------------------------------------------------------

io <- readRDS(file.path(processed_dir, "outputs", "index_outputs.rds"))

country_info <- standardize_country_info(
  utils::read.csv(file.path(repo_root, config$raw_data_dir, "wdi_country_info.csv"))
)
# Coalition as scripts/20 resolves it (coalition_members() over coalition_tbl.rds, names from
# the WDI country reference).
coalition <- coalition_members(readRDS(file.path(processed_dir, "coalition_tbl.rds")))
allies <- coalition %>%
  dplyr::select("iso3", "tier", "network_node") %>%
  dplyr::left_join(country_info %>% dplyr::distinct(iso3 = .data$iso3c, Country = .data$country), by = "iso3")
if (anyNA(allies$Country) || anyDuplicated(allies$iso3)) stop("Coalition names do not resolve uniquely.")
if (!setequal(allies$iso3, io$allied_coalition_iso3)) stop("coalition_tbl and index_outputs disagree on the coalition.")
core <- allies %>% dplyr::filter(.data$tier == "core")
iso_of <- setNames(country_info$iso3c, country_info$country)
results$meta$coalition <- list(n_members = nrow(allies), n_core = nrow(core),
                               n_partner = sum(allies$tier == "partner"),
                               core = sort(core$iso3), partners = sort(allies$iso3[allies$tier == "partner"]))

pillars <- list(
  es = list(label = "Energy Security", key = "energy_security", col = "Energy_Security_Index",
            out = io$energy_security_outputs, headline = io$energy_security_index,
            weights = weights_cfg$energy_security, missing = missing_cfg$energy_security),
  eo = list(label = "Economic Opportunity", key = "economic_opportunity",
            col = "Economic_Opportunity_Index", out = io$economic_opportunity_outputs,
            headline = io$economic_opportunity_index,
            weights = weights_cfg$economic_opportunity, missing = missing_cfg$economic_opportunity)
)

for (p in names(pillars)) {
  pl <- pillars[[p]]
  hl <- pl$headline %>% dplyr::filter(.data$tech %in% HEADLINE_TECHS)
  if (anyDuplicated(key_of(hl))) stop(p, ": duplicated headline cells")
  # The stored flag must be the rule at tau.
  rk_check <- rankable_at(hl[[pl$col]], hl$imputed_weight_share, hl$rank_exclusion, TAU_BASE)
  if (!identical(rk_check, hl$rankable %in% TRUE)) stop(p, ": rankable_at() does not reproduce the stored flag")
  hl$pair <- pair_of(hl)
  # Pairs that can rank: >= 3 rankable cells and a non-constant baseline among them.
  ps <- hl %>% dplyr::filter(.data$rankable) %>% dplyr::group_by(.data$pair) %>%
    dplyr::summarise(n = dplyr::n(), sd = stats::sd(.data[[pl$col]]), .groups = "drop")
  pl$rk_pairs <- ps$pair[ps$n >= 3 & ps$sd > TOL]
  hl$rk <- hl$rankable & hl$pair %in% pl$rk_pairs
  pl$hl <- hl
  pl$cells <- pl$out$diagnostics$index_cells %>% dplyr::filter(.data$tech %in% HEADLINE_TECHS)
  pillars[[p]] <- pl
  missing_members <- setdiff(allies$Country, hl$Country)
  if (length(missing_members) > 0) message(p, ": coalition members without a headline cell: ",
                                           paste(missing_members, collapse = ", "))
}

results$meta$cells <- lapply(pillars, function(pl) {
  hl <- pl$hl
  sc <- !is.na(hl[[pl$col]])
  terr <- grepl("not a sovereign economy", hl$rank_exclusion)
  list(
    headline_pairs_scored = length(unique(hl$pair[sc])),
    pairs_status = as.list(table(pl$cells$cell_status)),
    rankable_pairs = length(pl$rk_pairs),
    pairs_not_ranking = setdiff(unique(hl$pair[sc]), pl$rk_pairs),
    scored_cells = sum(sc), rankable_cells = sum(hl$rankable),
    territory_cells = sum(sc & terr), over_threshold_cells = sum(sc & !terr & !hl$rankable),
    rankable_cells_in_rankable_pairs = sum(hl$rk),
    rankable_countries = dplyr::n_distinct(hl$Country[hl$rankable]),
    by_stage = hl %>% dplyr::group_by(.data$supply_chain) %>%
      dplyr::summarise(scored = sum(!is.na(.data[[pl$col]])), rankable = sum(.data$rankable), .groups = "drop")
  )
})
save_results()

# ===========================================================================
# 1. Vintage comparison, 2020 vs 2025
# ===========================================================================

vintage_dir <- file.path(processed_dir, "vintages")
coverage <- utils::read.csv(file.path(vintage_dir, "index_vintage_weight_coverage.csv"))
cat_by_year <- utils::read.csv(file.path(vintage_dir, "index_category_scores_by_year.csv"))
theme_prov <- utils::read.csv(file.path(vintage_dir, "index_vintage_theme_provenance.csv"))

vintage <- list()
vintage_points <- list()
for (p in names(pillars)) {
  pl <- pillars[[p]]
  rk_keys <- key_of(pl$hl[pl$hl$rk, ])
  cmp <- utils::read.csv(file.path(vintage_dir, paste0(pl$key, "_comparison_2020_vs_2025.csv"))) %>%
    dplyr::filter(.data$tech %in% HEADLINE_TECHS) %>%
    dplyr::mutate(pair = pair_of(.))
  cmp_rk <- cmp[key_of(cmp) %in% rk_keys & !is.na(cmp$index_2020) & !is.na(cmp$index_2025), ]
  # A pair constant in either vintage (over these cells) cannot rank countries.
  pair_sd <- cmp_rk %>% dplyr::group_by(.data$pair) %>%
    dplyr::summarise(n = dplyr::n(), sd20 = stats::sd(.data$index_2020), sd25 = stats::sd(.data$index_2025),
                     .groups = "drop")
  ok <- pair_sd$pair[pair_sd$n >= 3 & pair_sd$sd20 > TOL & pair_sd$sd25 > TOL]
  d <- cmp_rk %>% dplyr::filter(.data$pair %in% ok) %>%
    dplyr::group_by(.data$pair) %>%
    dplyr::mutate(n_pair = dplyr::n(),
                  r20 = rank(-.data$index_2020, ties.method = "min"),
                  r25 = rank(-.data$index_2025, ties.method = "min"),
                  pct_rank_change = 100 * (.data$r20 - .data$r25) / (.data$n_pair - 1)) %>%
    dplyr::ungroup()
  rho <- d %>% dplyr::group_by(.data$pair, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(n = dplyr::n(), rho = safe_spearman(.data$index_2020, .data$index_2025),
                     mean_abs_change = mean(abs(.data$index_change)),
                     share_unchanged = mean(abs(.data$index_change) <= TOL), .groups = "drop")
  members <- d %>% dplyr::filter(.data$Country %in% allies$Country)
  movers_fun <- function(tbl, k = 8) {
    tbl %>% dplyr::arrange(dplyr::desc(abs(.data$index_change))) %>%
      dplyr::slice_head(n = k) %>%
      dplyr::transmute(Country = .data$Country, pair = .data$pair,
                       index_2020 = .data$index_2020, index_2025 = .data$index_2025,
                       index_change = .data$index_change,
                       rank_2020 = .data$r20, rank_2025 = .data$r25,
                       n_pair = .data$n_pair, pct_rank_change = .data$pct_rank_change)
  }
  vintage[[p]] <- list(
    cells_in_comparison_file = nrow(cmp),
    n_country_cells = nrow(d),
    pairs_used = length(unique(d$pair)),
    pairs_constant_in_a_vintage = setdiff(unique(cmp_rk$pair), ok),
    pooled_spearman = safe_spearman(d$index_2020, d$index_2025),
    within_pair_spearman = summarise_rho(rho),
    within_pair_spearman_by_pair = rho %>% dplyr::select("pair", "n", "rho", "mean_abs_change", "share_unchanged"),
    mean_abs_change = mean(abs(d$index_change)),
    median_abs_change = stats::median(abs(d$index_change)),
    p90_abs_change = q(abs(d$index_change), 0.9),
    share_country_cells_unchanged = mean(abs(d$index_change) <= TOL),
    mean_abs_change_by_stage = d %>% dplyr::group_by(.data$supply_chain) %>%
      dplyr::summarise(mean_abs_change = mean(abs(.data$index_change)), .groups = "drop"),
    # Share of the sum of squared index changes that is a common shift of the pair (the
    # squared pair-mean change): such a shift moves levels and leaves within-pair ranks intact.
    share_sq_change_common_pair_shift_by_stage = d %>% dplyr::group_by(.data$pair) %>%
      dplyr::mutate(m = mean(.data$index_change)) %>% dplyr::ungroup() %>%
      dplyr::group_by(.data$supply_chain) %>%
      dplyr::summarise(share = sum(.data$m^2) / sum(.data$index_change^2), .groups = "drop"),
    mean_abs_pct_rank_change = mean(abs(d$pct_rank_change)),
    largest_movers_all = movers_fun(d),
    largest_movers_coalition = movers_fun(members)
  )
  vintage_points[[p]] <- d %>% dplyr::mutate(pillar = pl$label)

  # Share of pillar weight that re-derives with the vintage year.
  #   nominal    index_vintage_weight_coverage.csv (a category counts as varying when any of
  #              its themes is flagged time-varying)
  #   realised   categories whose scores differ between the two vintages in >= 1% of cells
  #   effective  per country-cell, the available-case weight share on categories whose score
  #              differs in that cell, averaged over the rankable cells of the used pairs
  cov <- coverage[coverage$pillar == pl$key, ]
  cy <- cat_by_year %>% dplyr::filter(.data$pillar == pl$key, .data$tech %in% HEADLINE_TECHS) %>%
    dplyr::select("index_year", "Country", "tech", "supply_chain", "category", "category_score") %>%
    tidyr::pivot_wider(names_from = "index_year", values_from = "category_score", names_prefix = "y")
  cy$changed <- !is.na(cy$y2020) & !is.na(cy$y2025) & abs(cy$y2020 - cy$y2025) > TOL
  cy$in_one_vintage_only <- is.na(cy$y2020) != is.na(cy$y2025)
  w <- unlist(pl$weights)
  cat_changes <- cy %>% dplyr::group_by(.data$category) %>%
    dplyr::summarise(n_cells = dplyr::n(), share_cells_changed = mean(.data$changed),
                     n_in_one_vintage_only = sum(.data$in_one_vintage_only),
                     mean_abs_change = mean(abs(.data$y2020 - .data$y2025), na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(weight = unname(w[.data$category]))
  realised_cats <- cat_changes$category[cat_changes$share_cells_changed >= 0.01]
  trace_cats <- cat_changes$category[cat_changes$share_cells_changed > 0 &
                                       cat_changes$share_cells_changed < 0.01]
  used_keys <- key_of(d)
  eff <- cy %>% dplyr::filter(!is.na(.data$y2025), key_of(.) %in% used_keys) %>%
    dplyr::mutate(weight = unname(w[.data$category])) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(share = sum(.data$weight[.data$changed]) / sum(.data$weight),
                     share_ex_prices = sum(.data$weight[.data$changed & .data$category != "Energy Prices"]) /
                       sum(.data$weight), .groups = "drop")
  vintage[[p]]$weight_rederiving <- list(
    nominal_share = cov$share_varying, nominal_weight = cov$weight_varying,
    nominal_total = cov$weight_total, nominal_categories = cov$categories_varying,
    realised_categories = paste(sort(realised_cats), collapse = "; "),
    realised_rule = "score differs between vintages in >= 1% of the category's headline cells",
    realised_share = sum(w[realised_cats]) / sum(w[cat_changes$category]),
    trace_change_categories = paste(sort(trace_cats), collapse = "; "),
    flagged_varying_but_identical = paste(setdiff(strsplit(cov$categories_varying, "; ")[[1]],
                                                  realised_cats), collapse = "; "),
    effective_share_mean = mean(eff$share),
    effective_share_mean_excluding_energy_prices = mean(eff$share_ex_prices),
    by_category = cat_changes
  )

  # The 2025 vintage against the headline index (the vintage build runs its trade themes on the
  # Atlas of Economic Complexity in every year; the headline uses the Comtrade dyads).
  by_year <- utils::read.csv(file.path(vintage_dir, paste0(pl$key, "_index_by_year.csv"))) %>%
    dplyr::filter(.data$index_year == 2025, .data$tech %in% HEADLINE_TECHS)
  hv <- dplyr::inner_join(by_year %>% dplyr::select("Country", "tech", "supply_chain", v = dplyr::all_of(pl$col)),
                          pl$hl %>% dplyr::select("Country", "tech", "supply_chain", h = dplyr::all_of(pl$col), "rk"),
                          by = c("Country", "tech", "supply_chain"))
  hv_rho <- pair_spearman(hv, hv$v, hv$h, hv$rk)
  hc <- cat_by_year %>% dplyr::filter(.data$pillar == pl$key, .data$index_year == 2025,
                                      .data$tech %in% HEADLINE_TECHS) %>%
    dplyr::inner_join(pl$out$category_scores, by = c("Country", "tech", "supply_chain", "category")) %>%
    dplyr::group_by(.data$category) %>%
    dplyr::summarise(share_identical = mean(abs(.data$category_score.x - .data$category_score.y) <= TOL,
                                            na.rm = TRUE), .groups = "drop")
  vintage[[p]]$vintage2025_vs_headline <- list(
    n_country_cells = sum(hv$rk),
    pooled_spearman = safe_spearman(hv$v[hv$rk], hv$h[hv$rk]),
    within_pair_spearman = summarise_rho(hv_rho),
    within_pair_by_pair = hv_rho,
    share_country_cells_identical = mean(abs(hv$v[hv$rk] - hv$h[hv$rk]) <= TOL),
    max_abs_difference = max(abs(hv$v[hv$rk] - hv$h[hv$rk])),
    categories_not_identical = paste(hc$category[hc$share_identical < 1], collapse = "; "),
    category_share_identical = hc
  )
}
vintage$source_years <- utils::read.csv(file.path(vintage_dir, "index_vintage_source_years.csv"))
vintage$trade_basis <- unique(theme_prov$basis[theme_prov$category == "Trade"])
results$vintage <- vintage
save_results()
message("vintage done")

# ===========================================================================
# 2. Monte Carlo category weights and leave-one-category-out (ES, EO)
# ===========================================================================

category_matrix <- function(pl) {
  cc <- pl$out$category_contributions
  keys <- pl$out$index[, c("Country", "tech", "supply_chain", pl$col)]
  cats <- sort(unique(cc$category[!is.na(cc$category_score)]))
  cats <- intersect(cats, names(pl$weights))
  stopifnot(!anyDuplicated(paste(key_of(cc), cc$category)))
  C <- matrix(NA_real_, nrow(keys), length(cats), dimnames = list(NULL, cats))
  i <- match(key_of(cc), key_of(keys))
  j <- match(cc$category, cats)
  keep <- !is.na(i) & !is.na(j)
  C[cbind(i[keep], j[keep])] <- cc$category_score[keep]
  list(C = C, keys = keys, cats = cats, w = unlist(pl$weights)[cats])
}

set.seed(SEED)
weights_mc <- list()
loo <- list()
rank_band_rows <- list()
mc_pair_rows <- list()
loo_rows <- list()
equal_rows <- list()
pair_structure_all <- list()
cm_list <- list()   # category matrices, reused by the pick-stability variants (section 3)
for (p in names(pillars)) {
  pl <- pillars[[p]]
  cm <- category_matrix(pl)
  base_all <- cm$keys[[pl$col]]
  # Reproduce the stored index at the configured weights.
  rep_idx <- ac_index(cm$C, cm$w)[, 1]
  max_dev <- max(abs(rep_idx - base_all), na.rm = TRUE)
  na_match <- identical(is.na(rep_idx), is.na(base_all))
  if (!(max_dev < 1e-10 && na_match)) stop(p, ": available-case re-implementation does not reproduce the index")
  cm_list[[p]] <- cm

  # Category-set structure over ALL scored headline cells (for Section 4's statement).
  sc_rows <- which(cm$keys$tech %in% HEADLINE_TECHS & !is.na(base_all))
  sc_pair <- pair_of(cm$keys[sc_rows, ])
  n_obs_cat <- rowSums(!is.na(cm$C[sc_rows, , drop = FALSE]))
  shared_all <- vapply(split(seq_along(sc_rows), sc_pair), function(r) {
    length(unique(apply(!is.na(cm$C[sc_rows[r], , drop = FALSE]), 1, paste, collapse = ""))) == 1
  }, logical(1))
  pair_structure_all[[p]] <- list(
    n_pairs = length(shared_all), n_pairs_shared_category_set = sum(shared_all),
    pairs_not_shared = names(shared_all)[!shared_all],
    observed_categories_per_cell = as.list(table(n_obs_cat)),
    observed_categories_per_cell_median = stats::median(n_obs_cat)
  )

  key_all <- key_of(cm$keys)
  head_rows <- match(key_of(pl$hl[pl$hl$rk, ]), key_all)
  if (anyNA(head_rows)) stop(p, ": rankable headline cells missing from the index")
  keys_h <- cm$keys[head_rows, ]
  C_h <- cm$C[head_rows, , drop = FALSE]
  base_h <- base_all[head_rows]
  pair_h <- pair_of(keys_h)
  all_keep <- rep(TRUE, length(base_h))

  # Pair structure that bounds the effect of any weight change. Within a pair whose cells share
  # one observed category set, a category constant across them only shifts and rescales the
  # index, so the within-pair ranking depends only on the relative weights of the varying
  # categories; with one varying category it does not depend on the weights at all.
  varies_in <- function(Cp) vapply(seq_len(ncol(Cp)), function(k) {
    v <- Cp[!is.na(Cp[, k]), k]
    length(v) > 1 && (max(v) - min(v)) > TOL
  }, logical(1))
  structure_tbl <- dplyr::bind_rows(lapply(sort(unique(pair_h)), function(pp) {
    Cp <- C_h[pair_h == pp, , drop = FALSE]
    obs <- colSums(!is.na(Cp)) > 0
    varies <- varies_in(Cp)
    patterns <- unique(apply(!is.na(Cp), 1, paste, collapse = ""))
    tibble::tibble(pair = pp, n_countries = nrow(Cp), n_categories_observed = sum(obs),
                   n_categories_varying = sum(varies), shared_category_set = length(patterns) == 1,
                   varying = paste(colnames(Cp)[varies], collapse = "; "),
                   share_observed_weight_constant = sum(cm$w[obs & !varies]) / sum(cm$w[obs]))
  }))
  structure_tbl$weight_invariant <- structure_tbl$shared_category_set & structure_tbl$n_categories_varying <= 1
  pair_structure <- list(
    n_pairs = nrow(structure_tbl),
    n_pairs_shared_category_set = sum(structure_tbl$shared_category_set),
    n_categories_observed = list(median = stats::median(structure_tbl$n_categories_observed),
                                 min = min(structure_tbl$n_categories_observed),
                                 max = max(structure_tbl$n_categories_observed)),
    n_categories_varying = list(median = stats::median(structure_tbl$n_categories_varying),
                                min = min(structure_tbl$n_categories_varying),
                                max = max(structure_tbl$n_categories_varying),
                                table = as.list(table(structure_tbl$n_categories_varying))),
    n_pairs_weight_invariant = sum(structure_tbl$weight_invariant),
    weight_invariant_pairs = structure_tbl$pair[structure_tbl$weight_invariant],
    mean_share_of_observed_weight_on_constant_categories = mean(structure_tbl$share_observed_weight_constant),
    by_pair = structure_tbl
  )

  K <- length(cm$cats)
  wbar <- cm$w / sum(cm$w)

  # Variance-based importance of each category within pairs (the linear and exact form of the
  # effective importance of Paruolo et al. 2013): within a pair the index is the sum of the
  # category contributions a_k = w_k c_k / sum_{l observed} w_l, so its cross-country variance
  # splits into covariance shares S_k = Cov(a_k, y) / Var(y) that sum to one over k.
  importance_tbl <- dplyr::bind_rows(lapply(sort(unique(pair_h)), function(pp) {
    r <- which(pair_h == pp)
    Cp <- C_h[r, , drop = FALSE]
    O <- !is.na(Cp)
    den <- as.numeric(O %*% cm$w)
    A <- sweep(ifelse(O, Cp, 0), 2, cm$w, "*") / den
    y <- rowSums(A)
    tibble::tibble(pair = pp, category = cm$cats, observed = colSums(O) > 0,
                   renormalised_weight = colMeans(sweep(O * 1, 2, cm$w, "*") / den),
                   variance_share = as.numeric(stats::cov(A, y)) / stats::var(y),
                   share_zero = ifelse(colSums(O) > 0, colSums(O & Cp == 0, na.rm = TRUE) / colSums(O), NA_real_),
                   max_abs_dev_from_index = max(abs(y - base_h[r])))
  }))
  if (max(importance_tbl$max_abs_dev_from_index) > 1e-10) stop(p, ": contributions do not sum to the index")
  importance <- importance_tbl %>% dplyr::group_by(.data$category) %>%
    dplyr::summarise(pairs_observed = sum(.data$observed),
                     mean_renormalised_weight = mean(.data$renormalised_weight),
                     mean_variance_share = mean(.data$variance_share),
                     mean_variance_share_where_observed = if (any(.data$observed))
                       mean(.data$variance_share[.data$observed]) else NA_real_,
                     max_variance_share = max(.data$variance_share),
                     min_variance_share = min(.data$variance_share),
                     share_zero_median = if (any(.data$observed))
                       stats::median(.data$share_zero[.data$observed]) else NA_real_,
                     share_zero_min = if (any(.data$observed)) min(.data$share_zero[.data$observed]) else NA_real_,
                     share_zero_max = if (any(.data$observed)) max(.data$share_zero[.data$observed]) else NA_real_,
                     .groups = "drop") %>%
    dplyr::mutate(nominal_share = unname(wbar[.data$category]))
  conc <- dirichlet_concentration(K)
  W <- rdirichlet_cols(N_DRAWS, conc * wbar)
  rownames(W) <- cm$cats
  IDX <- ac_index(C_h, W)

  pairs <- sort(unique(pair_h))
  rho_mat <- matrix(NA_real_, length(pairs), N_DRAWS, dimnames = list(pairs, NULL))
  pooled <- as.numeric(stats::cor(base_h, IDX, method = "spearman"))
  for (pp in pairs) {
    r <- which(pair_h == pp)
    rho_mat[pp, ] <- as.numeric(stats::cor(base_h[r], IDX[r, , drop = FALSE], method = "spearman"))
    # Rank bands of coalition members (rank 1 = highest index among the pair's rankable cells).
    m <- which(keys_h$Country[r] %in% allies$Country)
    if (length(m) > 0) {
      n_r <- length(r)
      R <- apply(-IDX[r, , drop = FALSE], 2, rank, ties.method = "average")[m, , drop = FALSE]
      base_rank <- rank(-base_h[r], ties.method = "average")[m]
      rank_band_rows[[length(rank_band_rows) + 1]] <- tibble::tibble(
        pillar = pl$label, pair = pp, tech = keys_h$tech[r][m], supply_chain = keys_h$supply_chain[r][m],
        Country = keys_h$Country[r][m], n_pair = n_r, base_rank = base_rank,
        q05 = apply(R, 1, q, 0.05), q50 = apply(R, 1, q, 0.5), q95 = apply(R, 1, q, 0.95)
      )
    }
  }
  draw_median <- apply(rho_mat, 2, stats::median, na.rm = TRUE)
  pair_median <- apply(rho_mat, 1, stats::median, na.rm = TRUE)
  pair_p05 <- apply(rho_mat, 1, q, 0.05)
  mc_pair_rows[[p]] <- tibble::tibble(pillar = pl$label, pair = pairs, rho_median_over_draws = pair_median,
                                      rho_p05_over_draws = pair_p05)
  realised_cv <- apply(W, 1, function(v) stats::sd(v) / mean(v))
  sens <- structure_tbl$pair[!structure_tbl$weight_invariant]
  rm_s <- rho_mat[rownames(rho_mat) %in% sens, , drop = FALSE]
  weights_mc[[p]] <- list(
    reimplementation_max_abs_deviation_from_stored_index = max_dev,
    reimplementation_na_pattern_identical = na_match,
    categories = cm$cats, configured_weights = unname(cm$w), configured_shares = unname(wbar),
    dirichlet_concentration = conc,
    cv_theoretical = setNames(as.list(dirichlet_cv(wbar, conc)), cm$cats),
    cv_realised = setNames(as.list(realised_cv), cm$cats),
    n_country_cells = length(base_h), n_pairs = length(pairs),
    pair_structure = pair_structure,
    within_pair_rho_median_over_pairs_per_draw = list(
      median = stats::median(draw_median), p05 = q(draw_median, 0.05), p95 = q(draw_median, 0.95),
      min = min(draw_median)),
    within_pair_rho_all_pair_draws = list(
      median = stats::median(rho_mat, na.rm = TRUE), p05 = q(rho_mat, 0.05), p01 = q(rho_mat, 0.01),
      min = min(rho_mat, na.rm = TRUE),
      share_ge_0.9 = mean(rho_mat >= 0.9, na.rm = TRUE)),
    within_pair_rho_pairs_with_2plus_varying_categories = list(
      n_pairs = nrow(rm_s), median = stats::median(rm_s, na.rm = TRUE),
      p05 = q(rm_s, 0.05), min = min(rm_s, na.rm = TRUE)),
    within_pair_rho_pair_medians = summarise_rho(tibble::tibble(pair = pairs, rho = pair_median)),
    pooled_rho = list(median = stats::median(pooled), p05 = q(pooled, 0.05), min = min(pooled)),
    least_stable_pairs = mc_pair_rows[[p]] %>% dplyr::arrange(.data$rho_p05_over_draws) %>%
      dplyr::slice_head(n = 5)
  )

  # Equal category weights (each observed category 1 / |K_its|), as a fixed alternative.
  eq_alt <- ac_index(C_h, setNames(rep(1, K), cm$cats))[, 1]
  eq_rr <- pair_spearman(keys_h, base_h, eq_alt, all_keep)
  equal_rows[[p]] <- eq_rr %>% dplyr::mutate(pillar = pl$label)
  weights_mc[[p]]$equal_weights <- list(
    pooled_spearman = safe_spearman(base_h, eq_alt),
    within_pair_spearman = summarise_rho(eq_rr),
    mean_abs_level_change = mean(abs(eq_alt - base_h))
  )

  # Leave one category out: weight 0, the rest renormalised by the available-case rule, over
  # the pairs in which the category is scored for at least one rankable cell.
  loo_p <- lapply(cm$cats, function(k) {
    wk <- cm$w
    wk[k] <- 0
    alt <- ac_index(C_h, wk)[, 1]
    scored_pairs <- unique(pair_h[!is.na(C_h[, k])])
    rr <- pair_spearman(keys_h, base_h, alt, pair_h %in% scored_pairs)
    s <- summarise_rho(rr)
    tibble::tibble(
      pillar = pl$label, category = k, weight_share = unname(wbar[k]),
      pairs_scored = length(scored_pairs),
      median = s$median, p25 = s$p25, p75 = s$p75, min = s$min, min_pair = s$min_pair, max = s$max,
      share_pairs_ge_0.9 = s$share_pairs_ge_0.9,
      n_pairs = s$n_pairs, n_pairs_undefined = s$n_pairs_undefined,
      undefined_pairs = paste(rr$pair[is.na(rr$rho)], collapse = "; "),
      country_cells_unscored = sum(is.na(alt)),
      pooled = safe_spearman(base_h, alt)
    )
  })
  loo_rows[[p]] <- dplyr::bind_rows(loo_p)
  loo[[p]] <- loo_rows[[p]] %>% dplyr::arrange(.data$median)
  weights_mc[[p]]$variance_importance <- list(
    definition = paste("S_k = Cov(a_k, y) / Var(y) within each rankable headline pair over its rankable",
                       "cells, a_k the category contribution; mean over pairs with 0 where k is not",
                       "observed (sums to one); renormalised weight w_k / sum of observed weights,",
                       "averaged the same way"),
    max_abs_dev_contributions_vs_index = max(importance_tbl$max_abs_dev_from_index),
    n_pairs = length(unique(importance_tbl$pair)),
    sum_mean_variance_share = sum(importance$mean_variance_share),
    total_variation_nominal_vs_variance = 0.5 * sum(abs(importance$nominal_share - importance$mean_variance_share)),
    total_variation_renormalised_vs_variance =
      0.5 * sum(abs(importance$mean_renormalised_weight - importance$mean_variance_share)),
    total_variation_nominal_vs_renormalised =
      0.5 * sum(abs(importance$nominal_share - importance$mean_renormalised_weight)),
    by_category = importance %>%
      dplyr::left_join(loo_rows[[p]] %>% dplyr::select("category", loo_median = "median",
                                                       loo_pairs = "n_pairs"), by = "category") %>%
      dplyr::arrange(dplyr::desc(.data$mean_variance_share))
  )
  rm(IDX, W, rho_mat); invisible(gc(verbose = FALSE))
}

rank_bands <- dplyr::bind_rows(rank_band_rows) %>%
  dplyr::mutate(width_pct = 100 * (.data$q95 - .data$q05) / (.data$n_pair - 1),
                base_pct = 100 * (.data$n_pair - .data$base_rank) / (.data$n_pair - 1))
band_summary <- function(tbl) list(
  n_member_cells = nrow(tbl),
  median_width_pct_points = stats::median(tbl$width_pct),
  p90_width_pct_points = q(tbl$width_pct, 0.9),
  max_width_pct_points = max(tbl$width_pct),
  share_width_le_10_pct_points = mean(tbl$width_pct <= 10),
  median_width_ranks = stats::median(tbl$q95 - tbl$q05)
)
for (p in names(pillars)) {
  rb <- rank_bands[rank_bands$pillar == pillars[[p]]$label, ]
  weights_mc[[p]]$coalition_rank_bands <- c(
    band_summary(rb),
    list(by_stage = lapply(setNames(STAGES, STAGES), function(s) band_summary(rb[rb$supply_chain == s, ])),
         widest = rb %>% dplyr::arrange(dplyr::desc(.data$width_pct)) %>% dplyr::slice_head(n = 5) %>%
           dplyr::select("Country", "pair", "n_pair", "base_rank", "q05", "q95", "width_pct"))
  )
  # Tie structure of the categories that vary in the pairs of the three widest bands (over the
  # pair's rankable cells): wide bands arise where a member sits among large blocks of ties.
  widest_pairs <- unique(head(rb$pair[order(-rb$width_pct)], 3))
  rk_keys <- key_of(pillars[[p]]$hl[pillars[[p]]$hl$rk, ])
  weights_mc[[p]]$coalition_rank_bands$widest_pairs_tie_structure <-
    pillars[[p]]$out$category_contributions %>%
    dplyr::filter(pair_of(.) %in% widest_pairs, key_of(.) %in% rk_keys, !is.na(.data$category_score)) %>%
    dplyr::mutate(pair = pair_of(.)) %>%
    dplyr::group_by(.data$pair, .data$category) %>%
    dplyr::summarise(n_countries = dplyr::n(),
                     n_distinct_values = dplyr::n_distinct(round(.data$category_score, 10)),
                     share_at_modal_value = max(table(round(.data$category_score, 10))) / dplyr::n(),
                     share_zero = mean(.data$category_score == 0), .groups = "drop") %>%
    dplyr::filter(.data$n_distinct_values > 1)
}
results$weights <- weights_mc
results$loo <- loo
results$pair_structure_all_scored_cells <- pair_structure_all
save_results()
message("weights and loo done")

# ===========================================================================
# 3. Strategic Index: rebuild, Monte Carlo of the five weights, variants, picks
# ===========================================================================

tech_ghg <- readRDS(file.path(processed_dir, "tech_ghg_tbl.rds"))
trl_index <- io$economic_opportunity_category_scores %>%
  dplyr::filter(.data$category == "Technological Readiness") %>%
  dplyr::select("Country", "tech", "supply_chain", trl_index = "category_score")
resource_base_tbl <- strategic_resource_base(readRDS(file.path(processed_dir, "reserves_tbl.rds")),
                                             readRDS(file.path(processed_dir, "critical_minerals_production_tbl.rds")))

# scripts/20's sequence: build, exclude sectors where a pillar is constant, flag the
# endowment screen.
si_build <- function(es_idx, eo_idx, pol_idx) {
  si <- build_strategic_index(economic_opportunity_index = eo_idx, energy_security_index = es_idx,
                              policy_index = pol_idx, tech_ghg = tech_ghg,
                              technological_readiness_index = trl_index, techs = HEADLINE_TECHS)
  deg <- strategic_index_degenerate_sectors(si)
  if (nrow(deg) > 0) {
    si <- build_strategic_index(economic_opportunity_index = eo_idx, energy_security_index = es_idx,
                                policy_index = pol_idx, tech_ghg = tech_ghg,
                                technological_readiness_index = trl_index, techs = HEADLINE_TECHS,
                                excluded_sectors = unique(c(STRATEGIC_EXCLUDED_SECTORS, deg$sector)))
  }
  si <- flag_strategic_resource_base(si, resource_base_tbl)
  attr(si, "degenerate") <- deg$sector
  si
}
picks_of <- function(si, screen = TRUE) {
  select_allied_strategic_sectors(si, ally_countries = allies, require_resource_base = screen) %>%
    dplyr::select("Country", "iso3", "tier", "sector", "strategic_index")
}

si_full <- si_build(io$energy_security_index, io$economic_opportunity_index, io$policy_index)
stored <- io$strategic_index
jj <- dplyr::full_join(stored %>% dplyr::select("Country", "tech", "supply_chain", a = "strategic_index", ra = "resource_base"),
                       si_full %>% dplyr::select("Country", "tech", "supply_chain", b = "strategic_index", rb = "resource_base"),
                       by = c("Country", "tech", "supply_chain"))
si_rebuild_dev <- max(abs(jj$a - jj$b), na.rm = TRUE)
if (nrow(jj) != nrow(stored) || si_rebuild_dev > 1e-12 || any(is.na(jj$a) != is.na(jj$b)) ||
    !identical(jj$ra, jj$rb)) stop("Rebuilding the Strategic Index does not reproduce the stored one")
picks_base <- picks_of(si_full)
shipped_sel <- utils::read.csv(file.path(processed_dir, "charts", "allied_strategic.csv"))
selection_reproduced <- nrow(picks_base) == nrow(shipped_sel) &&
  all(picks_base$sector == shipped_sel$sector[match(picks_base$Country, shipped_sel$Country)])
if (!selection_reproduced || anyDuplicated(picks_base$Country)) stop("Baseline picks do not reproduce allied_strategic.csv")

si_tbl <- si_full %>% dplyr::filter(!is.na(.data$strategic_index))
X <- cbind(policy = si_tbl$pol, economic_opportunity = si_tbl$Economic_Opportunity_Index,
           energy_security_need = si_tbl$energy_security_need, supply_chain = si_tbl$sc_weight,
           technology = si_tbl$tech_weight)
w_si <- STRATEGIC_INDEX_WEIGHTS[colnames(X)]
si_rep <- as.numeric(X %*% w_si)
si_max_dev <- max(abs(si_rep - si_tbl$strategic_index))
if (si_max_dev > 1e-10) stop("SI re-computation does not reproduce strategic_index")

# Eligible rows for a pick: members, Upstream / Midstream, not Geothermal or Green Hydrogen,
# and (screen on) not flagged resource_base == FALSE.
elig_rows <- function(screen = TRUE) {
  which(si_tbl$Country %in% allies$Country & si_tbl$supply_chain %in% c("Midstream", "Upstream") &
          !si_tbl$tech %in% c("Geothermal", "Green Hydrogen") &
          (if (screen) !(si_tbl$resource_base %in% FALSE) else TRUE))
}
allied_rows <- elig_rows(TRUE)
top_sector <- function(score, screen = TRUE) {
  si_v <- si_full
  si_v$strategic_index[!is.na(si_v$strategic_index)] <- score
  picks_of(si_v, screen = screen) %>% dplyr::select("Country", "sector")
}
base_top <- picks_base %>% dplyr::select("Country", "sector")
stopifnot(identical(top_sector(si_tbl$strategic_index) %>% dplyr::arrange(.data$Country),
                    base_top %>% dplyr::arrange(.data$Country)))

country_rho <- function(alt) {
  tibble::tibble(Country = si_tbl$Country, base = si_tbl$strategic_index, alt = alt) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(n = dplyr::n(), rho = safe_spearman(.data$base, .data$alt), .groups = "drop")
}
agree_top <- function(a) {
  j <- dplyr::inner_join(base_top, a, by = "Country")
  list(n_members_with_pick = nrow(a), n_compared = nrow(j), n_same = sum(j$sector.x == j$sector.y),
       lost_pick = setdiff(base_top$Country, a$Country), new_pick = setdiff(a$Country, base_top$Country),
       changed = j %>% dplyr::filter(.data$sector.x != .data$sector.y) %>%
         dplyr::transmute(Country = .data$Country, iso3 = unname(iso_of[.data$Country]),
                          baseline = .data$sector.x, variant = .data$sector.y),
       sector_counts = as.list(table(a$sector)))
}

# --- Policy index with NIPO added back --------------------------------------------------------
# The October 2026 NIPO component (the user's run of GTA NIPO as of 5 October 2026 with the
# debiased strength of PR #333; 77 jurisdictions, EU members individually; built exactly as
# scripts/10 builds the NIPO Policy Index rows; <scratch>/nipo_oct/README.txt) replaces the NIPO
# rows stored in policy_component_tbl (the February 2026 build, 23 countries, out of the composite
# anyway) and re-enters the composite with the weight the composite gave NIPO until October 2026
# (NIPO 4, IEA PAMS 3, CAT 2), renormalised over the components present in each cell as
# build_composite_policy_index() does. IEA PAMS stays required: the eligible set is the headline
# one (the countries with IEA PAMS, CAT added where it rates the country), so NIPO changes the
# policy values of its jurisdictions only, and the within-country S-curve is re-taken over the
# same Strategic Index cells. Nothing is written to data/processed.
NIPO <- "NIPO Policy Index"
nipo_oct_path <- file.path(scratch_dir, "nipo_oct", "nipo_component_oct2026.rds")
nipo_oct <- readRDS(nipo_oct_path)
if (!all(nipo_oct$variable == NIPO)) stop("The October NIPO component carries other variables")
nipo_oct_unmatched <- setdiff(unique(nipo_oct$Country), unique(io$policy_component_tbl$Country))
pol_components <- dplyr::bind_rows(
  io$policy_component_tbl %>% dplyr::filter(.data$variable %in% names(POLICY_COMPONENT_WEIGHTS)),
  nipo_oct %>% dplyr::select("Country", "tech", "supply_chain", "variable", "value")
) %>%
  dplyr::filter(.data$tech %in% HEADLINE_TECHS) %>%
  dplyr::group_by(.data$Country, .data$tech, .data$supply_chain, .data$variable) %>%
  dplyr::summarise(value = if (all(is.na(.data$value))) NA_real_ else mean(.data$value, na.rm = TRUE),
                   .groups = "drop")
eligible_countries <- io$policy_index_provenance$eligible_countries
pol_ac <- function(wv) {
  pol_components %>% dplyr::filter(.data$variable %in% names(wv), !is.na(.data$value),
                                   .data$Country %in% eligible_countries) %>%
    dplyr::mutate(w = unname(wv[.data$variable])) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(P = sum(.data$w * .data$value) / sum(.data$w), .groups = "drop")
}
match_si <- function(tbl) tbl$P[match(key_of(si_tbl), key_of(tbl))]
pol_rep <- match_si(pol_ac(POLICY_COMPONENT_WEIGHTS))
pol_rep_max_dev <- max(abs(pol_rep - si_tbl$value))
if (anyNA(pol_rep) || pol_rep_max_dev > 1e-10) stop("Policy composite re-computation does not reproduce the stored Policy index")
pol_rerank_check <- max(abs(ave(si_tbl$value, si_tbl$Country, FUN = median_scurve) - si_tbl$pol))
if (pol_rerank_check > 1e-12) stop("Within-country S-curve does not reproduce pol")
NIPO_WEIGHTS <- c("NIPO Policy Index" = 4, POLICY_COMPONENT_WEIGHTS)
pol_nipo_raw <- match_si(pol_ac(NIPO_WEIGHTS))
stopifnot(!anyNA(pol_nipo_raw))
pol_nipo <- ave(pol_nipo_raw, si_tbl$Country, FUN = median_scurve)
nipo_obs <- pol_components %>% dplyr::filter(.data$variable == NIPO, !is.na(.data$value))
nipo_countries <- sort(unique(nipo_obs$Country))
si_has_nipo <- key_of(si_tbl) %in% key_of(nipo_obs)

eo_within <- ave(si_tbl$Economic_Opportunity_Index, si_tbl$Country, FUN = median_scurve)
need_within <- ave(si_tbl$energy_security_need, si_tbl$Country, FUN = median_scurve)

si_variants_def <- list(
  security_orientation = list(
    label = "Security ES in place of need 1 - ES",
    score = as.numeric(cbind(si_tbl$pol, si_tbl$Economic_Opportunity_Index, si_tbl$Energy_Security_Index,
                             si_tbl$sc_weight, si_tbl$tech_weight) %*% w_si)),
  no_sector_priors = list(
    label = "Without the stage and technology priors (0.2, 0.3, 0.3 renormalised)",
    score = as.numeric(X[, 1:3] %*% (w_si[1:3] / sum(w_si[1:3])))),
  all_ranked = list(
    label = "EO and 1 - ES S-curved within country, like the policy term",
    score = as.numeric(cbind(si_tbl$pol, eo_within, need_within, si_tbl$sc_weight, si_tbl$tech_weight) %*% w_si)),
  nipo_added = list(
    label = "October 2026 NIPO added back to the Policy index (NIPO 4, IEA PAMS 3, CAT 2), re-ranked within country",
    score = as.numeric(cbind(pol_nipo, X[, -1]) %*% w_si)),
  no_policy = list(
    label = "No policy term (other four weights unchanged)",
    score = as.numeric(X[, -1] %*% w_si[-1])),
  equal_weights = list(
    label = "Equal weights (0.2 on each of the five terms)",
    score = as.numeric(X %*% rep(0.2, ncol(X)))),
  screen_off = list(
    label = "Endowment screen off (OPSI_SI_RESOURCE_SCREEN=0): index unchanged, every Upstream cell eligible",
    score = si_tbl$strategic_index, screen = FALSE)
)
variant_picks <- lapply(si_variants_def, function(v) top_sector(v$score, screen = !isFALSE(v$screen)))
si_variants <- lapply(names(si_variants_def), function(nm) {
  v <- si_variants_def[[nm]]
  cr <- country_rho(v$score)
  ag <- agree_top(variant_picks[[nm]])
  list(label = v$label,
       pooled_spearman = safe_spearman(si_tbl$strategic_index, v$score),
       within_country_spearman_median = stats::median(cr$rho, na.rm = TRUE),
       within_country_spearman_min = min(cr$rho, na.rm = TRUE),
       within_country_spearman_min_country = cr$Country[which.min(cr$rho)],
       within_country_spearman_max = max(cr$rho, na.rm = TRUE),
       allied_top_sector_same = ag$n_same, allied_members_compared = ag$n_compared,
       allied_members_with_pick = ag$n_members_with_pick,
       allied_pick_sector_counts = ag$sector_counts,
       allied_top_sector_changed = ag$changed)
})
names(si_variants) <- names(si_variants_def)

# NIPO in detail: policy ranks of the NIPO countries, and the picks of the members among them.
pol_nn_country <- tibble::tibble(Country = si_tbl$Country, a = si_tbl$pol, b = pol_nipo) %>%
  dplyr::filter(.data$Country %in% nipo_countries) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(rho = safe_spearman(.data$a, .data$b), .groups = "drop")
nipo_members_picked <- intersect(base_top$Country, nipo_countries)
policy_nipo_added <- list(
  definition = paste("October 2026 NIPO component (GTA NIPO as of 5 Oct 2026, debiased strength) substituted",
                     "for the stored NIPO rows and added to the composite with weight 4 (IEA PAMS 3, CAT 2),",
                     "renormalised over the components present in each cell; IEA PAMS still required: headline",
                     "eligible set kept (", length(eligible_countries), "countries with IEA PAMS); within-country",
                     "S-curve over the same SI cells"),
  source_file = "<scratch>/nipo_oct/nipo_component_oct2026.rds",
  component_rows = nrow(nipo_oct), component_jurisdictions = dplyr::n_distinct(nipo_oct$Country),
  component_names_not_in_policy_table = nipo_oct_unmatched,
  stored_feb2026_nipo_countries = dplyr::n_distinct(io$policy_component_tbl$Country[
    io$policy_component_tbl$variable == NIPO & !is.na(io$policy_component_tbl$value)]),
  nipo_countries = nipo_countries, n_nipo_countries = length(nipo_countries),
  nipo_cells_headline = nrow(nipo_obs),
  members_with_nipo = sort(allies$iso3[allies$Country %in% nipo_countries]),
  nipo_countries_in_si = length(intersect(nipo_countries, unique(si_tbl$Country))),
  nipo_stages = sort(unique(nipo_obs$supply_chain)),
  share_si_cells_with_nipo = mean(si_has_nipo), n_si_cells_with_nipo = sum(si_has_nipo),
  pooled_spearman_policy_value_nipo_cells = safe_spearman(si_tbl$value[si_has_nipo], pol_nipo_raw[si_has_nipo]),
  within_country_spearman_policy_rank_median = stats::median(pol_nn_country$rho, na.rm = TRUE),
  within_country_spearman_policy_rank_min = min(pol_nn_country$rho, na.rm = TRUE),
  within_country_spearman_policy_rank_min_country = pol_nn_country$Country[which.min(pol_nn_country$rho)],
  within_country_spearman_policy_rank_max = max(pol_nn_country$rho, na.rm = TRUE),
  members_picked_with_nipo = length(nipo_members_picked),
  members_picked_with_nipo_iso3 = sort(unname(iso_of[nipo_members_picked])),
  members_picked_with_nipo_same = sum(!nipo_members_picked %in% si_variants$nipo_added$allied_top_sector_changed$Country),
  members_picked_with_nipo_changed = si_variants$nipo_added$allied_top_sector_changed,
  nipo_countries_policy_eligible = length(intersect(nipo_countries, eligible_countries)),
  nipo_countries_by_basis = as.list(table(io$policy_eligibility$policy_basis[
    match(nipo_countries, io$policy_eligibility$Country)], useNA = "ifany"))
)

# Endowment screen in detail.
up_scored <- !is.na(si_full$strategic_index) & si_full$supply_chain == "Upstream"
screen_detail <- list(
  scored_upstream_cells = sum(up_scored),
  scored_upstream_cells_no_resource_base = sum(up_scored & si_full$resource_base %in% FALSE),
  nuclear_upstream_picks_screen_on = sum(base_top$sector == "Nuclear - Upstream"),
  nuclear_upstream_picks_screen_off = sum(variant_picks$screen_off$sector == "Nuclear - Upstream"),
  upstream_picks_screen_on = sum(grepl("Upstream", base_top$sector)),
  upstream_picks_screen_off = sum(grepl("Upstream", variant_picks$screen_off$sector)),
  es_floor_sectors = strategic_index_es_floor_sectors(si_full),
  nuclear_upstream_es_floor_countries = {
    nu <- si_tbl[si_tbl$sector == "Nuclear - Upstream", ]
    list(n_si_countries = nrow(nu), n_at_min = sum(abs(nu$Energy_Security_Index - min(nu$Energy_Security_Index)) < 1e-9),
         es_min = min(nu$Energy_Security_Index))
  }
)

# --- Monte Carlo of the five SI weights ---------------------------------------------------------
K5 <- length(w_si)
conc5 <- dirichlet_concentration(K5)
set.seed(SEED + 1L)
W5 <- rdirichlet_cols(N_DRAWS, conc5 * w_si)
SID <- X %*% W5
countries_si <- sort(unique(si_tbl$Country))
rho_c <- matrix(NA_real_, length(countries_si), N_DRAWS, dimnames = list(countries_si, NULL))
for (cn in countries_si) {
  r <- which(si_tbl$Country == cn)
  if (length(r) >= 3) rho_c[cn, ] <- as.numeric(stats::cor(si_tbl$strategic_index[r], SID[r, , drop = FALSE], method = "spearman"))
}
draw_med_c <- apply(rho_c, 2, stats::median, na.rm = TRUE)
# Allied pick per draw (matrix form of select_allied_strategic_sectors() with the screen on;
# checked against it at the configured weights).
ally_d <- tibble::tibble(Country = si_tbl$Country[allied_rows], sector = si_tbl$sector[allied_rows])
ally_index <- split(seq_along(allied_rows), ally_d$Country)
chk <- vapply(names(ally_index), function(cn) {
  r <- ally_index[[cn]]
  ally_d$sector[r][which.max(si_tbl$strategic_index[allied_rows[r]])]
}, character(1))
stopifnot(identical(unname(chk), base_top$sector[match(names(chk), base_top$Country)]))
retain <- sapply(names(ally_index), function(cn) {
  r <- ally_index[[cn]]
  top_d <- ally_d$sector[r][apply(SID[allied_rows[r], , drop = FALSE], 2, which.max)]
  mean(top_d == base_top$sector[base_top$Country == cn])
})
margins <- tibble::tibble(Country = si_tbl$Country[allied_rows], s = si_tbl$strategic_index[allied_rows]) %>%
  dplyr::group_by(.data$Country) %>% dplyr::arrange(dplyr::desc(.data$s), .by_group = TRUE) %>%
  dplyr::summarise(n_eligible = dplyr::n(), margin = if (dplyr::n() >= 2) .data$s[1] - .data$s[2] else NA_real_,
                   .groups = "drop")
retain_tbl <- tibble::tibble(Country = names(retain), share_draws_baseline_top = unname(retain)) %>%
  dplyr::left_join(base_top, by = "Country") %>%
  dplyr::left_join(allies %>% dplyr::select("Country", "iso3", "tier"), by = "Country") %>%
  dplyr::left_join(margins, by = "Country")

results$si <- list(
  rebuild_max_abs_deviation_from_stored = si_rebuild_dev,
  reimplementation_max_abs_deviation = si_max_dev,
  policy_composite_reimplementation_max_abs_deviation = pol_rep_max_dev,
  policy_within_country_rerank_max_abs_deviation_from_pol = pol_rerank_check,
  degenerate_sectors_excluded = attr(si_full, "degenerate"),
  n_cells = nrow(si_tbl), n_countries = length(countries_si),
  configured_weights = as.list(w_si),
  baseline_selection_matches_allied_strategic_csv = selection_reproduced,
  n_members = nrow(allies), n_members_with_pick = nrow(base_top),
  members_without_pick = sort(allies$iso3[!allies$Country %in% base_top$Country]),
  baseline_pick_counts = as.list(table(base_top$sector)),
  baseline_margin = list(median = stats::median(margins$margin, na.rm = TRUE),
                         p25 = q(margins$margin, 0.25), p75 = q(margins$margin, 0.75),
                         min = min(margins$margin, na.rm = TRUE),
                         min_member = unname(iso_of[margins$Country[which.min(margins$margin)]]),
                         n_below_0.005 = sum(margins$margin < 0.005, na.rm = TRUE),
                         share_below_0.005 = mean(margins$margin < 0.005, na.rm = TRUE),
                         members_below_0.005 = sort(unname(iso_of[margins$Country[(margins$margin < 0.005) %in% TRUE]])),
                         n_below_0.01 = sum(margins$margin < 0.01, na.rm = TRUE),
                         share_below_0.01 = mean(margins$margin < 0.01, na.rm = TRUE),
                         n_below_0.02 = sum(margins$margin < 0.02, na.rm = TRUE),
                         members_below_0.01 = sort(unname(iso_of[margins$Country[(margins$margin < 0.01) %in% TRUE]])),
                         n_eligible_sectors_median = stats::median(margins$n_eligible)),
  baseline_picks_by_policy_basis = {
    pb <- io$policy_eligibility$policy_basis[match(base_top$Country, io$policy_eligibility$Country)]
    list(counts = as.list(table(pb)),
         pams_only_members = base_top %>% dplyr::filter(pb == "IEA PAMS only") %>%
           dplyr::transmute(iso3 = unname(iso_of[.data$Country]), sector = .data$sector) %>%
           dplyr::arrange(.data$iso3))
  },
  weights_mc = list(
    dirichlet_concentration = conc5,
    cv_theoretical = as.list(dirichlet_cv(w_si, conc5)),
    within_country_rho_median_over_countries_per_draw = list(
      median = stats::median(draw_med_c), p05 = q(draw_med_c, 0.05), p95 = q(draw_med_c, 0.95)),
    within_country_rho_all = list(median = stats::median(rho_c, na.rm = TRUE), p05 = q(rho_c, 0.05),
                                  min = min(rho_c, na.rm = TRUE)),
    allied_top_retention = list(
      mean_share = mean(retain), median_share = stats::median(retain),
      n_allies_ge_0.9 = sum(retain >= 0.9), n_allies_lt_0.5 = sum(retain < 0.5),
      n_allies = length(retain),
      min_share = min(retain), min_country = names(retain)[which.min(retain)],
      cor_margin_retention_spearman = safe_spearman(retain_tbl$margin, retain_tbl$share_draws_baseline_top),
      by_member = retain_tbl %>% dplyr::arrange(.data$share_draws_baseline_top))
  ),
  variants = si_variants,
  policy_nipo_added = policy_nipo_added,
  endowment_screen = screen_detail
)
save_results()
rm(SID, rho_c); invisible(gc(verbose = FALSE))
message("si done")

# Helpers shared by the rebuilt-SI variants below (policy basis, pick stability).
si_common <- function(st) {
  dplyr::inner_join(si_tbl %>% dplyr::select("Country", "tech", "supply_chain", a = "strategic_index"),
                    st %>% dplyr::filter(!is.na(.data$strategic_index)) %>%
                      dplyr::select("Country", "tech", "supply_chain", b = "strategic_index"),
                    by = c("Country", "tech", "supply_chain"))
}
rho_summary <- function(r, who = NULL) {
  ok <- !is.na(r)
  list(n = sum(ok), median = stats::median(r, na.rm = TRUE), p25 = q(r, 0.25), p75 = q(r, 0.75),
       min = if (any(ok)) min(r, na.rm = TRUE) else NA_real_,
       min_country = if (any(ok) && !is.null(who)) who[which.min(r)] else NA_character_,
       max = if (any(ok)) max(r, na.rm = TRUE) else NA_real_)
}
within_country_summary <- function(common) {
  cr <- common %>% dplyr::group_by(.data$Country) %>%
    dplyr::summarise(rho = safe_spearman(.data$a, .data$b), .groups = "drop")
  rho_summary(cr$rho, cr$Country)
}
# Picks of a subset of members under a variant, against the baseline.
picks_restricted <- function(pk, members) {
  b <- base_top %>% dplyr::filter(.data$Country %in% members)
  j <- dplyr::left_join(b, pk, by = "Country")
  list(n_members = nrow(b), n_with_pick = sum(!is.na(j$sector.y)),
       n_same = sum(j$sector.x == j$sector.y, na.rm = TRUE),
       changed = j %>% dplyr::filter(!is.na(.data$sector.y), .data$sector.x != .data$sector.y) %>%
         dplyr::transmute(iso3 = unname(iso_of[.data$Country]), baseline = .data$sector.x,
                          variant = .data$sector.y))
}
rebuilt_variant <- function(label, st, pk) {
  ag <- agree_top(pk)
  cm_ <- si_common(st)
  list(label = label,
       si_cells = sum(!is.na(st$strategic_index)),
       si_countries = dplyr::n_distinct(st$Country[!is.na(st$strategic_index)]),
       degenerate_sectors = attr(st, "degenerate"),
       common_cells_with_baseline = nrow(cm_),
       si_within_country_spearman = within_country_summary(cm_),
       members_with_pick = nrow(pk), picks_same = ag$n_same, picks_compared = ag$n_compared,
       lost_pick = sort(unname(iso_of[ag$lost_pick])), new_pick = sort(unname(iso_of[ag$new_pick])),
       picks_changed = ag$changed, pick_counts = ag$sector_counts)
}

# ===========================================================================
# 3b. The Policy index: both components required, IEA PAMS alone, NIPO added back
# ===========================================================================
# Headline rule (user decision of 9 Oct 2026): IEA PAMS is required and CAT is added where it
# rates the country. Two rebuilds of the Policy index with the pipeline's builder:
#   require_all  OPSI_POLICY_REQUIRE_ALL=1, the rule until then: both components required
#                (build_composite_policy_index(required_components = both components)).
#   pams_only    CAT dropped for every country, so the countries with a CAT rating are scored
#                on IEA PAMS alone, as the other countries are in the headline.
# Each rebuilds the Strategic Index with scripts/20's sequence (si_build()) and re-selects.
# The NIPO variant (above) changes the policy values of its jurisdictions on the fixed SI sample.

pc_tbl <- io$policy_component_tbl
pol_key_cmp <- function(a, b) {
  j <- dplyr::full_join(a %>% dplyr::select("Country", "tech", "supply_chain", a = "value"),
                        b %>% dplyr::select("Country", "tech", "supply_chain", b = "value"),
                        by = c("Country", "tech", "supply_chain"))
  list(n_cells = nrow(j), na_mismatch = sum(is.na(j$a) != is.na(j$b)),
       max_abs_deviation = max(abs(j$a - j$b), na.rm = TRUE))
}
pol_build_head <- build_composite_policy_index(pc_tbl, techs = HEADLINE_TECHS)
pol_head_check <- pol_key_cmp(io$policy_index, pol_build_head$index)
if (pol_head_check$na_mismatch > 0 || pol_head_check$max_abs_deviation > 1e-12) {
  stop("build_composite_policy_index() does not reproduce the stored Policy index")
}
pol_build_both <- build_composite_policy_index(pc_tbl, techs = HEADLINE_TECHS,
                                               required_components = names(POLICY_COMPONENT_WEIGHTS))
pol_build_pams <- build_composite_policy_index(pc_tbl %>% dplyr::filter(.data$variable != "CAT Policy Index"),
                                               techs = HEADLINE_TECHS)
both_countries <- sort(pol_build_both$eligibility$Country[pol_build_both$eligibility$eligible])
if (!setequal(both_countries, io$policy_index_provenance$countries_by_basis[["IEA PAMS + CAT"]])) {
  stop("The both-required eligible set differs from the headline's 'IEA PAMS + CAT' countries")
}
si_both <- si_build(io$energy_security_index, io$economic_opportunity_index, pol_build_both$index)
si_pams <- si_build(io$energy_security_index, io$economic_opportunity_index, pol_build_pams$index)
picks_both <- picks_of(si_both) %>% dplyr::select("Country", "sector")
picks_pams <- picks_of(si_pams) %>% dplyr::select("Country", "sector")
cat_members <- allies$Country[allies$Country %in% both_countries]

# Within-country agreement of the policy values (what the SI's within-country S-curve reads)
# between the composite and IEA PAMS alone, over the cells of the countries with both components;
# and of the policy ranks pol over their SI cells.
pv <- dplyr::inner_join(
  pol_build_head$index %>% dplyr::filter(.data$Country %in% both_countries) %>%
    dplyr::select("Country", "tech", "supply_chain", comp = "value"),
  pol_build_pams$index %>% dplyr::select("Country", "tech", "supply_chain", pams = "value"),
  by = c("Country", "tech", "supply_chain"))
pv_rho <- pv %>% dplyr::group_by(.data$Country) %>%
  dplyr::summarise(n = dplyr::n(), rho = safe_spearman(.data$comp, .data$pams), .groups = "drop")
pol_si_rho <- dplyr::inner_join(
  si_tbl %>% dplyr::filter(.data$Country %in% both_countries) %>%
    dplyr::select("Country", "tech", "supply_chain", a = "pol"),
  si_pams %>% dplyr::filter(!is.na(.data$strategic_index)) %>%
    dplyr::select("Country", "tech", "supply_chain", b = "pol"),
  by = c("Country", "tech", "supply_chain")) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(rho = safe_spearman(.data$a, .data$b), .groups = "drop")

basis_of <- function(cn) io$policy_eligibility$policy_basis[match(cn, io$policy_eligibility$Country)]
results$policy <- list(
  headline = list(
    rule = io$policy_index_provenance$eligibility_rule,
    weights = as.list(POLICY_COMPONENT_WEIGHTS),
    n_scored = length(eligible_countries),
    n_by_basis = lapply(io$policy_index_provenance$countries_by_basis, length),
    rebuild_reproduces_stored = pol_head_check,
    members_by_basis = as.list(table(basis_of(allies$Country), useNA = "ifany")),
    members_pams_only = sort(allies$iso3[basis_of(allies$Country) %in% "IEA PAMS only"]),
    si_cells_by_basis = as.list(table(si_tbl$policy_basis, useNA = "ifany")),
    si_countries_by_basis = as.list(table(si_tbl$policy_basis[!duplicated(si_tbl$Country)], useNA = "ifany"))
  ),
  require_all = c(
    rebuilt_variant("Both components required (OPSI_POLICY_REQUIRE_ALL=1; the rule until 9 Oct 2026)",
                    si_both, picks_both),
    list(n_eligible = length(both_countries),
         si_max_abs_change_common_cells = {
           cc <- si_common(si_both)
           max(abs(cc$a - cc$b))
         },
         members_with_cat = length(cat_members),
         cat_members_picks = picks_restricted(picks_both, cat_members))
  ),
  pams_only = c(
    rebuilt_variant("IEA PAMS alone for every country (CAT dropped)", si_pams, picks_pams),
    list(n_scored = sum(pol_build_pams$eligibility$eligible),
         policy_value_within_country_spearman_both_basis = rho_summary(pv_rho$rho, pv_rho$Country),
         policy_rank_within_country_spearman_si_cells = rho_summary(pol_si_rho$rho, pol_si_rho$Country),
         si_within_country_spearman_both_basis = within_country_summary(
           si_common(si_pams) %>% dplyr::filter(.data$Country %in% both_countries)),
         mean_policy_value = list(
           both_basis_composite = mean(pv$comp), both_basis_pams_alone = mean(pv$pams),
           pams_only_basis_countries = mean(pol_build_head$index$value[pol_build_head$index$policy_basis == "IEA PAMS only"])),
         cat_members_picks = picks_restricted(picks_pams, cat_members))
  ),
  nipo_added = policy_nipo_added
)
variant_picks$require_all <- picks_both
variant_picks$pams_only <- picks_pams
save_results()
message("policy variants done")

# ===========================================================================
# 3c. Pick stability: margins, category drops, trade on the Atlas basis, vintage pillars
# ===========================================================================
# The data fixes of 9-12 Oct 2026 revised the trade inputs (the Comtrade energy-trade file
# rebuilt from the all-reporter pull, which filled 60 basket codes; the export-HHI ranking over
# the headline cells; Hong Kong's re-exports out of the export measures; HS 851762 out of the
# grid basket; the new Upstream baskets) and the mineral reserves (USGS MCS 2026). The superseded
# trade file is not kept, so that revision cannot be re-run here (the EIA fill and the new
# Upstream baskets are, in section 4b); the variants below measure how far the picks rest on
# those inputs instead: each category dropped from both pillars in turn (the available-case
# mean renormalises the rest), the Trade category replaced by the Atlas-based Trade category of
# the 2025 vintage (scripts/40, Atlas of Economic Complexity HS92 2023, no Comtrade), and the
# pillars of the 2020 and 2025 vintages. The pillars' rankable flags are kept from the baseline
# in the category variants (the vintage pillars carry their own); the Policy index and the
# endowment screen are unchanged.
# The interim builds recorded their pick counts in the decision log of the integration workstream
# (pending.md); they are kept as a record and cannot be recomputed from the final inputs. 8 Oct
# 2026: endowment screen on, coalition of 56, both policy components required, before the reserves
# refresh and the trade fixes. 10 Oct 2026: coalition of 57, IEA PAMS required and CAT added, USGS
# MCS 2026 reserves, the energy-trade file rebuilt from the all-reporter pull (which filled the
# missing Electric Grid Midstream codes), before the EIA fill, the new Upstream baskets and CM-FDI.
PICK_HISTORY <- list(
  build_2026_10_08 = list(
    source = "decision log (pending.md), build of 8 Oct 2026; recorded, not recomputed",
    members = 56L, members_with_pick = 45L,
    counts = list("Electric Grid - Midstream" = 17L, "Batteries - Midstream" = 11L, "Solar - Midstream" = 6L,
                  "Nuclear - Upstream" = 6L, "Batteries - Upstream" = 3L, "Nuclear - Midstream" = 2L)),
  build_2026_10_10 = list(
    source = "decision log (pending.md), final rerun of 10 Oct 2026 (15:31); recorded, not recomputed",
    members = 57L, members_with_pick = 57L,
    counts = list("Batteries - Midstream" = 26L, "Nuclear - Upstream" = 10L, "Nuclear - Midstream" = 7L,
                  "Batteries - Upstream" = 4L, "Electric Grid - Midstream" = 4L, "Solar - Midstream" = 3L,
                  "Oil - Midstream" = 2L, "Wind - Upstream" = 1L))
)
for (h in PICK_HISTORY) stopifnot(sum(unlist(h$counts)) == h$members_with_pick)

# Pillar table (the stored headline table with its rankable flags) with values recomputed from
# the category scores under modified weights or category scores.
pillar_from_categories <- function(p, w = NULL, C = NULL) {
  cm <- cm_list[[p]]
  pl <- pillars[[p]]
  ww <- cm$w
  if (!is.null(w)) ww[names(w)] <- w
  v <- ac_index(if (is.null(C)) cm$C else C, ww)[, 1]
  tbl <- pl$headline
  tbl[[pl$col]] <- v[match(key_of(tbl), key_of(cm$keys))]
  tbl
}
for (p in names(pillars)) {
  tb <- pillar_from_categories(p)
  d <- max(abs(tb[[pillars[[p]]$col]] - pillars[[p]]$headline[[pillars[[p]]$col]]), na.rm = TRUE)
  if (d > 1e-10 || !identical(is.na(tb[[pillars[[p]]$col]]), is.na(pillars[[p]]$headline[[pillars[[p]]$col]]))) {
    stop(p, ": pillar_from_categories() does not reproduce the headline table")
  }
}
si_from <- function(es_tbl, eo_tbl) si_build(es_tbl, eo_tbl, io$policy_index)

# --- Each category dropped from both pillars ---------------------------------------------------
cats_all <- sort(union(cm_list$es$cats, cm_list$eo$cats))
loo_pick_rows <- list()
loo_pick_detail <- list()
for (k in cats_all) {
  es_t <- if (k %in% cm_list$es$cats) pillar_from_categories("es", w = setNames(0, k)) else io$energy_security_index
  eo_t <- if (k %in% cm_list$eo$cats) pillar_from_categories("eo", w = setNames(0, k)) else io$economic_opportunity_index
  st <- si_from(es_t, eo_t)
  pk <- picks_of(st) %>% dplyr::select("Country", "sector")
  rv <- rebuilt_variant(paste("Category dropped:", k), st, pk)
  loo_pick_detail[[k]] <- rv
  loo_pick_rows[[k]] <- tibble::tibble(
    category = k,
    pillars = paste(c("ES", "EO")[c(k %in% cm_list$es$cats, k %in% cm_list$eo$cats)], collapse = ", "),
    es_weight_share = if (k %in% cm_list$es$cats) unname(cm_list$es$w[k] / sum(cm_list$es$w)) else NA_real_,
    eo_weight_share = if (k %in% cm_list$eo$cats) unname(cm_list$eo$w[k] / sum(cm_list$eo$w)) else NA_real_,
    si_cells = rv$si_cells, members_with_pick = rv$members_with_pick, picks_same = rv$picks_same,
    si_within_country_median = rv$si_within_country_spearman$median,
    grid_mid_picks = sum(pk$sector == "Electric Grid - Midstream"),
    batteries_mid_picks = sum(pk$sector == "Batteries - Midstream"),
    nuclear_up_picks = sum(pk$sector == "Nuclear - Upstream"),
    nuclear_mid_picks = sum(pk$sector == "Nuclear - Midstream")
  )
  variant_picks[[paste0("drop_", k)]] <- pk
}
loo_pick_tbl <- dplyr::bind_rows(loo_pick_rows) %>% dplyr::arrange(.data$picks_same)
message("category-drop picks done")

# --- Trade category on the Atlas basis (2025 vintage) ------------------------------------------
atlas_trade <- cat_by_year %>%
  dplyr::filter(.data$index_year == 2025, .data$category == "Trade") %>%
  dplyr::select("pillar", "Country", "tech", "supply_chain", "category_score")
if (anyDuplicated(paste(atlas_trade$pillar, key_of(atlas_trade)))) stop("Atlas trade scores are not unique by cell")
trade_swap <- list()
trade_swap_stats <- list()
for (p in names(pillars)) {
  cm <- cm_list[[p]]
  a <- atlas_trade[atlas_trade$pillar == pillars[[p]]$key, ]
  C2 <- cm$C
  C2[, "Trade"] <- a$category_score[match(key_of(cm$keys), key_of(a))]
  trade_swap[[p]] <- pillar_from_categories(p, C = C2)
  # Agreement of the two Trade category scores over the rankable headline cells of the
  # rankable pairs where both are observed.
  rk_rows <- match(key_of(pillars[[p]]$hl[pillars[[p]]$hl$rk, ]), key_of(cm$keys))
  tt <- tibble::tibble(pair = pair_of(cm$keys[rk_rows, ]), comtrade = cm$C[rk_rows, "Trade"],
                       atlas = C2[rk_rows, "Trade"]) %>%
    dplyr::filter(!is.na(.data$comtrade))
  rr <- tt %>% dplyr::filter(!is.na(.data$atlas)) %>% dplyr::group_by(.data$pair) %>%
    dplyr::summarise(rho = safe_spearman(.data$comtrade, .data$atlas), .groups = "drop")
  trade_swap_stats[[p]] <- list(
    rankable_cells_with_trade = nrow(tt), share_with_atlas_trade = mean(!is.na(tt$atlas)),
    within_pair_spearman_trade_category = summarise_rho(rr),
    pillar_within_pair_spearman = summarise_rho(pair_spearman(
      pillars[[p]]$hl, pillars[[p]]$hl[[pillars[[p]]$col]],
      trade_swap[[p]][[pillars[[p]]$col]][match(key_of(pillars[[p]]$hl), key_of(trade_swap[[p]]))],
      pillars[[p]]$hl$rk)))
}
si_trade_atlas <- si_from(trade_swap$es, trade_swap$eo)
variant_picks$trade_atlas <- picks_of(si_trade_atlas) %>% dplyr::select("Country", "sector")

# --- Vintage pillars ---------------------------------------------------------------------------
# The vintage tables flag only territories (scripts/40 applies no imputed-share threshold), so
# the vintage values are placed on the headline table and its rankable flags: the same cells
# enter the SI sample, with the vintage's values (a cell the vintage does not score drops out).
vintage_pillar <- function(p, year) {
  pl <- pillars[[p]]
  v <- utils::read.csv(file.path(vintage_dir, paste0(pl$key, "_index_by_year.csv"))) %>%
    dplyr::filter(.data$index_year == year, .data$tech %in% HEADLINE_TECHS)
  if (anyDuplicated(key_of(v))) stop("vintage ", year, " ", p, ": duplicated cells")
  tbl <- pl$headline
  tbl[[pl$col]] <- v[[pl$col]][match(key_of(tbl), key_of(v))]
  tbl
}
si_vintage <- list()
for (yr in c(2025, 2020)) {
  si_vintage[[as.character(yr)]] <- si_from(vintage_pillar("es", yr), vintage_pillar("eo", yr))
  variant_picks[[paste0("vintage_", yr)]] <- picks_of(si_vintage[[as.character(yr)]]) %>%
    dplyr::select("Country", "sector")
}

data_variants <- list(
  trade_atlas = c(rebuilt_variant("Trade category on the Atlas basis of the 2025 vintage (ES and EO)",
                                  si_trade_atlas, variant_picks$trade_atlas),
                  list(trade_category = trade_swap_stats)),
  vintage_2025 = rebuilt_variant("ES and EO of the 2025 vintage (scripts/40)", si_vintage[["2025"]],
                                 variant_picks$vintage_2025),
  vintage_2020 = rebuilt_variant("ES and EO of the 2020 vintage (scripts/40)", si_vintage[["2020"]],
                                 variant_picks$vintage_2020)
)
results$pick_stability <- list(
  baseline_pick_counts = as.list(table(base_top$sector)),
  history_recorded = PICK_HISTORY,
  margins = results$si$baseline_margin,
  margin_by_pick_sector = margins %>% dplyr::left_join(base_top, by = "Country") %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::summarise(n = dplyr::n(), median_margin = stats::median(.data$margin, na.rm = TRUE),
                     n_below_0.01 = sum(.data$margin < 0.01, na.rm = TRUE), .groups = "drop"),
  category_dropped = loo_pick_tbl,
  category_dropped_detail = lapply(loo_pick_detail, function(x) x[c("picks_same", "members_with_pick",
                                                                    "lost_pick", "picks_changed",
                                                                    "pick_counts")]),
  data_variants = data_variants,
  vintage_sample_note = paste("vintage values placed on the headline tables and their rankable flags (scripts/40",
                              "flags territories only), so the SI sample is the headline one less the cells the",
                              "vintage does not score")
)
save_results()
rm(si_vintage, trade_swap); invisible(gc(verbose = FALSE))
message("pick stability done")

# ===========================================================================
# 3d. Vertical complementarities: appendix sensitivity with permutation placebos
# ===========================================================================
# The vertical-complementarity study of 2026-10-12 (<scratch>/vertical; user decision: paper only,
# no index change) recommends one appendix row per pillar, built here on the final outputs:
#   EO-VCL  a value-chain-linkage category added to the Midstream and Upstream EO cells: the
#           adjacent stage's S-curved world market share (Atlas HS92 basket, newest year of the
#           study's cell panel) minus the economy's mean of that measure over the nine basketed
#           headline technologies (the within-country deviation, the only part with predictive
#           content in the study's Mundlak split), S-curved within the cell over the economies EO
#           scores there (median_scurve). Weights: the study's evidence weights, 0.6 x
#           beta_adjacent / beta_feasibility at the five-year horizon (implied_weights.csv): about
#           0.07 in Midstream and 0.23 in Upstream EO weight points, upper bounds.
#   ES-IS   the study's sourcing-weighted input security (es_input_security, primary variant, 2024
#           sourcing; IS = domestic share x own supply security + import-share-weighted supplier
#           security, supplier security = 1/2 supplier ES in the input stage + 1/2 dyadic
#           alignment), S-curved within the cell, added to the Midstream and Downstream ES cells
#           at a 15 per cent share of the cell's observed weight: ES' = 0.85 ES + 0.15 IS.
# Each enters the cell's available-case mean; the baseline rankable flags are kept (the extra
# category is observed data, so it could only lower a cell's imputed share). The SI changes only
# through the pillar term: SI' = SI + 0.3 (EO' - EO), or SI' = SI - 0.3 (ES' - ES) through the need
# term. Placebos: the term's scores permuted within each cell (N_PLACEBO draws; information
# destroyed, distribution kept) and, for ES-IS, a constant 0.5 category (pure reweighting).
vert_dir <- file.path(scratch_dir, "vertical")
iw <- utils::read.csv(file.path(vert_dir, "eo_linkages", "tables", "implied_weights.csv"))
iw5 <- iw[iw$h == 5, ]
W_VCL <- c(Midstream = iw5$w_mid_up, Upstream = iw5$w_up)
cell_panel <- as.data.frame(readRDS(file.path(vert_dir, "eo_linkages", "data", "cell_panel.rds")))
vcl_year <- max(cell_panel$year)
cur <- cell_panel[cell_panel$year == vcl_year & cell_panel$tech != "Semiconductors",
                  c("iso3", "tech", "stage", "ms")]
rm(cell_panel); invisible(gc(verbose = FALSE))
eo_hl <- pillars$eo$hl
eo_iso <- unname(iso_of[eo_hl$Country])
miss_iso <- is.na(eo_iso)
eo_iso[miss_iso] <- suppressWarnings(countrycode::countrycode(
  eo_hl$Country[miss_iso], "country.name", "iso3c", warn = FALSE,
  custom_match = c(Kosovo = "XKX", Taiwan = "TWN", "Channel Islands" = "CHI")))
iso_eo <- tibble::tibble(Country = eo_hl$Country, iso3 = eo_iso) %>% dplyr::distinct()
if (anyDuplicated(iso_eo$Country)) stop("EO country names map to more than one ISO3 code")
cur <- cur[cur$iso3 %in% iso_eo$iso3, ]
cur <- cur %>% dplyr::group_by(.data$tech, .data$stage) %>%
  dplyr::mutate(ms_S = median_scurve(.data$ms)) %>%
  dplyr::group_by(.data$iso3, .data$stage) %>%
  dplyr::mutate(dev = .data$ms_S - mean(.data$ms_S, na.rm = TRUE)) %>%
  dplyr::ungroup()
vcl_techs <- sort(unique(cur$tech))
# The Midstream term reads the Upstream deviation and vice versa.
vcl_raw <- cur %>%
  dplyr::transmute(iso3 = .data$iso3, tech = .data$tech,
                   supply_chain = ifelse(.data$stage == "Upstream", "Midstream", "Upstream"), raw = .data$dev) %>%
  dplyr::left_join(iso_eo, by = "iso3", relationship = "many-to-many")
eo_cm <- cm_list$eo
eo_keys <- eo_cm$keys
eo_W <- as.numeric(((!is.na(eo_cm$C)) * 1) %*% eo_cm$w)
eo_val <- eo_keys[[pillars$eo$col]]
vcl <- eo_keys %>% dplyr::select("Country", "tech", "supply_chain") %>%
  dplyr::mutate(row = dplyr::row_number(), eo = eo_val) %>%
  dplyr::left_join(vcl_raw %>% dplyr::select("Country", "tech", "supply_chain", "raw"),
                   by = c("Country", "tech", "supply_chain")) %>%
  dplyr::mutate(raw = ifelse(is.na(.data$eo), NA_real_, .data$raw)) %>%
  dplyr::group_by(.data$tech, .data$supply_chain) %>%
  dplyr::mutate(score = median_scurve(.data$raw)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.data$row)
vcl_w <- unname(ifelse(vcl$supply_chain %in% names(W_VCL), W_VCL[vcl$supply_chain], 0))
eo_with_term <- function(score) {
  has <- !is.na(score) & !is.na(eo_val) & vcl_w > 0
  out <- eo_val
  out[has] <- (eo_val[has] * eo_W[has] + vcl_w[has] * score[has]) / (eo_W[has] + vcl_w[has])
  out
}
es_cm <- cm_list$es
es_keys <- es_cm$keys
es_val <- es_keys[[pillars$es$col]]
is_long <- as.data.frame(readRDS(file.path(vert_dir, "es_input_security", "out", "is_long.rds")))
is_prim <- is_long[is_long$variant == "primary" & is_long$year == 2024, c("Country", "tech", "stage", "score")]
rm(is_long); invisible(gc(verbose = FALSE))
if (anyDuplicated(paste(is_prim$Country, is_prim$tech, is_prim$stage))) stop("Input-security scores not unique by cell")
is_score <- is_prim$score[match(key_of(es_keys), paste(is_prim$Country, is_prim$tech, is_prim$stage, sep = "\r"))]
is_score[is.na(es_val)] <- NA_real_
IS_SHARE <- 0.15
es_with_term <- function(score) {
  has <- !is.na(score) & !is.na(es_val)
  out <- es_val
  out[has] <- (1 - IS_SHARE) * es_val[has] + IS_SHARE * score[has]
  out
}

# Picks from a modified pillar: the SI moves only through the pillar term.
si_rows_eo <- match(key_of(si_tbl), key_of(eo_keys))
si_rows_es <- match(key_of(si_tbl), key_of(es_keys))
stopifnot(!anyNA(si_rows_eo), !anyNA(si_rows_es),
          max(abs(eo_val[si_rows_eo] - si_tbl$Economic_Opportunity_Index)) < 1e-12,
          max(abs(1 - es_val[si_rows_es] - si_tbl$energy_security_need)) < 1e-12)
fast_picks <- function(score) {
  vapply(names(ally_index), function(cn) {
    r <- ally_index[[cn]]
    ally_d$sector[r][which.max(score[allied_rows[r]])]
  }, character(1))
}
base_fast <- fast_picks(si_tbl$strategic_index)
stopifnot(identical(unname(base_fast), base_top$sector[match(names(base_fast), base_top$Country)]))
picks_changed <- function(score) sum(fast_picks(score) != base_fast)
si_from_eo <- function(eo_new) si_tbl$strategic_index + w_si[["economic_opportunity"]] * (eo_new[si_rows_eo] - eo_val[si_rows_eo])
si_from_es <- function(es_new) si_tbl$strategic_index - w_si[["energy_security_need"]] * (es_new[si_rows_es] - es_val[si_rows_es])

# Within-pair Spearman over the baseline's rankable cells in the receiving stages.
term_rho <- function(p, new_vals, stages) {
  pl <- pillars[[p]]
  keys <- if (p == "eo") eo_keys else es_keys
  base <- if (p == "eo") eo_val else es_val
  hl_rows <- match(key_of(pl$hl), key_of(keys))
  keep <- pl$hl$rk & pl$hl$supply_chain %in% stages
  pair_spearman(pl$hl, base[hl_rows], new_vals[hl_rows], keep)
}
rho_by_stage <- function(rr) {
  rr$stage <- sub("^.* - ", "", rr$pair)
  lapply(split(rr, rr$stage), function(x) list(n_pairs = sum(!is.na(x$rho)), median = stats::median(x$rho, na.rm = TRUE),
                                               min = min(x$rho, na.rm = TRUE), min_pair = x$pair[which.min(x$rho)]))
}
permute_within <- function(score, tech, stage) {
  out <- score
  g <- split(seq_along(score), paste(tech, stage))
  for (ix in g) {
    o <- ix[!is.na(score[ix])]
    if (length(o) > 1) out[o] <- score[o][sample.int(length(o))]
  }
  out
}
placebo_summary <- function(rho_med, changes) {
  list(n_draws = length(changes),
       within_pair_rho_median = list(p05 = q(rho_med, 0.05), p50 = q(rho_med, 0.5), p95 = q(rho_med, 0.95)),
       picks_changed = list(p05 = q(changes, 0.05), p50 = q(changes, 0.5), p95 = q(changes, 0.95),
                            mean = mean(changes)))
}
changed_detail <- function(score) {
  fp <- fast_picks(score)
  d <- tibble::tibble(Country = names(fp), variant = unname(fp)) %>%
    dplyr::left_join(base_top %>% dplyr::rename(baseline = "sector"), by = "Country") %>%
    dplyr::filter(.data$variant != .data$baseline)
  d %>% dplyr::transmute(iso3 = unname(iso_of[.data$Country]), baseline = .data$baseline, variant = .data$variant)
}

# EO-VCL.
eo_vcl <- eo_with_term(vcl$score)
rr_vcl <- term_rho("eo", eo_vcl, c("Upstream", "Midstream"))
si_vcl <- si_from_eo(eo_vcl)
set.seed(SEED + 11L)
pl_vcl <- vapply(seq_len(N_PLACEBO), function(b) {
  sc <- permute_within(vcl$score, vcl$tech, vcl$supply_chain)
  e2 <- eo_with_term(sc)
  c(rho = stats::median(term_rho("eo", e2, c("Upstream", "Midstream"))$rho, na.rm = TRUE),
    changed = picks_changed(si_from_eo(e2)))
}, numeric(2))
# ES-IS.
es_is <- es_with_term(is_score)
rr_is <- term_rho("es", es_is, c("Midstream", "Downstream"))
si_is <- si_from_es(es_is)
es_const <- es_with_term(ifelse(is.na(is_score), NA_real_, 0.5))
rr_const <- term_rho("es", es_const, c("Midstream", "Downstream"))
set.seed(SEED + 12L)
pl_is <- vapply(seq_len(N_PLACEBO), function(b) {
  sc <- permute_within(is_score, es_keys$tech, es_keys$supply_chain)
  e2 <- es_with_term(sc)
  c(rho = stats::median(term_rho("es", e2, c("Midstream", "Downstream"))$rho, na.rm = TRUE),
    changed = picks_changed(si_from_es(e2)))
}, numeric(2))

# Study evidence quoted in the methods paragraph (read, not re-estimated).
z03 <- utils::read.csv(file.path(vert_dir, "synthesis", "tables", "z03_eo_attack.csv"), stringsAsFactors = FALSE)
ev_row <- function(spec_prefix, stage, outcome = "dS", h = 5) {
  r <- z03[startsWith(z03$spec, spec_prefix) & z03$stage == stage & z03$outcome == outcome & z03$h == h &
             z03$term == "adj_ms_S", ]
  if (nrow(r) == 0) return(NULL)
  r <- r[1, ]
  list(spec = r$spec, est = r$est, se = r$se, p = r$p, n = r$n, n_countries = r$n_ctry)
}
t2 <- utils::read.csv(file.path(vert_dir, "es_input_security", "out", "t2_importer_shortfall.csv"), stringsAsFactors = FALSE)
t3 <- utils::read.csv(file.path(vert_dir, "es_input_security", "out", "t3_propagation.csv"), stringsAsFactors = FALSE)
z08 <- utils::read.csv(file.path(vert_dir, "synthesis", "tables", "z08_es_alignment.csv"), stringsAsFactors = FALSE)
z06 <- utils::read.csv(file.path(vert_dir, "synthesis", "tables", "z06_colocation_lift.csv"), stringsAsFactors = FALSE)
oos <- utils::read.csv(file.path(vert_dir, "eo_linkages", "tables", "out_of_sample_auc.csv"), stringsAsFactors = FALSE)
row_of <- function(tb, model, term) {
  r <- tb[tb$model == model & tb$term == term, ]
  if (nrow(r) == 0) return(NULL)
  list(est = r$est[1], se = r$se[1], p = r$p[1], n = r$n_obs[1])
}

results$vertical <- list(
  source = "<scratch>/vertical (study of 2026-10-12); terms rebuilt here on the final outputs",
  eo_vcl = list(
    definition = paste("adjacent-stage S-curved Atlas market share minus the economy's mean over the basketed",
                       "technologies, S-curved within the cell; Midstream EO reads the Upstream deviation and Upstream",
                       "EO the Midstream one"),
    atlas_year = vcl_year, technologies = vcl_techs,
    weights = as.list(W_VCL),
    weight_share_of_mean_observed_weight = lapply(c(Midstream = "Midstream", Upstream = "Upstream"), function(s) {
      r <- eo_keys$supply_chain == s & !is.na(eo_val)
      W_VCL[[s]] / (mean(eo_W[r]) + W_VCL[[s]])
    }),
    cells_with_term = as.list(table(vcl$supply_chain[!is.na(vcl$score)])),
    within_pair_spearman = rho_by_stage(rr_vcl),
    within_pair_spearman_all = summarise_rho(rr_vcl),
    picks_changed = sum(fast_picks(si_vcl) != base_fast), picks_changed_detail = changed_detail(si_vcl),
    placebo_permuted = placebo_summary(pl_vcl["rho", ], pl_vcl["changed", ])
  ),
  es_is = list(
    definition = "sourcing-weighted input security (primary, 2024 sourcing) at a 15% share: ES' = 0.85 ES + 0.15 IS",
    share = IS_SHARE,
    cells_with_term = as.list(table(es_keys$supply_chain[!is.na(is_score)])),
    study_names_unmatched = length(setdiff(unique(is_prim$Country), unique(es_keys$Country))),
    within_pair_spearman = rho_by_stage(rr_is),
    within_pair_spearman_all = summarise_rho(rr_is),
    picks_changed = sum(fast_picks(si_is) != base_fast), picks_changed_detail = changed_detail(si_is),
    placebo_constant = list(within_pair_spearman = summarise_rho(rr_const),
                            picks_changed = sum(fast_picks(si_from_es(es_const)) != base_fast)),
    placebo_permuted = placebo_summary(pl_is["rho", ], pl_is["changed", ])
  ),
  study_evidence = list(
    eo_main_mid_dS = ev_row("S0 main", "Midstream"), eo_main_up_dS = ev_row("S0 main", "Upstream"),
    eo_main_mid_entry = ev_row("S0 main", "Midstream", "entry"),
    eo_country_tech_fe_mid_dS = ev_row("F2", "Midstream"), eo_country_tech_fe_up_dS = ev_row("F2", "Upstream"),
    eo_clean_tech_mid_dS = ev_row("P3", "Midstream"), eo_clean_tech_up_dS = ev_row("P3", "Upstream"),
    eo_fossil_nuclear_mid_dS = ev_row("P4", "Midstream"), eo_fossil_nuclear_up_dS = ev_row("P4", "Upstream"),
    eo_own_lags_mid_dS = ev_row("M1 + own S(ms)", "Midstream"), eo_own_lags_up_dS = ev_row("M1 + own S(ms)", "Upstream"),
    eo_out_of_sample_auc = oos,
    colocation_lift_2023 = z06 %>% dplyr::select("tech", "lift"),
    es_t2_shortfall_is2020 = row_of(t2, "IS2020", "IS20"),
    es_t2_shortfall_sourcing_hhi = row_of(t2, "portfolio", "HHI"),
    es_t3_midstream_growth_is2020 = row_of(t3, "IS_2022", "IS20"),
    es_t1_alignment_shift = z08 %>% dplyr::select("sample", "est", "se", "pct")
  )
)
save_results()
rm(eo_cm, es_cm); invisible(gc(verbose = FALSE))
message("vertical complementarities done")

# ===========================================================================
# 4. Rebuilds: S-curve curvature and imputation rules
# ===========================================================================

read_tbl <- function(name) readRDS(file.path(processed_dir, paste0(name, ".rds")))
# Theme inputs exactly as scripts/20_build_indices.R names and passes them.
es_inputs <- list(
  energy_access_consumption = read_tbl("energy_access_tbl"),
  solar_pv_potential = read_tbl("solar_pv_potential_tbl"),
  wind_potential = read_tbl("wind_potential_tbl"),
  geothermal_potential = read_tbl("geothermal_potential_tbl"),
  import_dependence = read_tbl("import_dependence_tbl"),
  reserves = read_tbl("reserves_tbl"),
  foreign_dependency = read_tbl("foreign_dependency_tbl"),
  critical_minerals_processing = read_tbl("critical_minerals_processing_tbl"),
  critical_minerals_production = read_tbl("critical_minerals_production_tbl"),
  critical_minerals_trade = read_tbl("critical_minerals_trade_tbl"),
  energy_consumption = read_tbl("energy_consumption_tbl"),
  trade_concentration = read_tbl("trade_concentration_tbl"),
  energy_prices = read_tbl("energy_prices_tbl"),
  investment_momentum = read_tbl("investment_momentum_tbl")
)
eo_inputs <- list(
  energy_access_consumption = es_inputs$energy_access_consumption,
  solar_pv_potential = es_inputs$solar_pv_potential,
  wind_potential = es_inputs$wind_potential,
  geothermal_potential = es_inputs$geothermal_potential,
  energy_consumption = es_inputs$energy_consumption,
  energy_prices = es_inputs$energy_prices,
  export_feasibility = read_tbl("export_feasibility_tbl"),
  future_demand = read_tbl("future_demand_tbl"),
  lcoe_competitiveness = read_tbl("lcoe_competitiveness_tbl"),
  market_share_manufacturing = read_tbl("market_share_manufacturing_tbl"),
  cost_competitiveness = read_tbl("cost_competitiveness_tbl"),
  production_depth_momentum = read_tbl("production_depth_momentum_tbl"),
  overcapacity_premium = read_tbl("overcapacity_premium_tbl"),
  technological_readiness = read_tbl("technological_readiness_tbl"),
  investment_momentum = es_inputs$investment_momentum
)

build_warnings <- character()
quietly <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    build_warnings <<- c(build_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }, message = function(m) invokeRestart("muffleMessage"))
}

build_both <- function(es_in, eo_in, md_es = pillars$es$missing, md_eo = pillars$eo$missing) {
  es <- quietly(build_energy_security_index_v2(
    theme_tables = es_in, weights = weights_cfg$energy_security, missing_data = md_es,
    allow_partial_categories = allow_partial_categories, include_sub_sector = include_sub_sector,
    max_imputed_weight_share = TAU_BASE))
  eo <- quietly(build_economic_opportunity_index_v2(
    theme_tables = eo_in, weights = weights_cfg$economic_opportunity,
    allow_partial_categories = allow_partial_categories, include_sub_sector = include_sub_sector,
    missing_data = md_eo, max_imputed_weight_share = TAU_BASE))
  list(es = es$index, eo = eo$index)
}

# Variant values aligned with each pillar's headline table (pl$hl).
variant_values <- function(variant) {
  lapply(setNames(names(pillars), names(pillars)), function(p) {
    pl <- pillars[[p]]
    v <- variant[[p]]
    v[[pl$col]][match(key_of(pl$hl), key_of(v))]
  })
}
compare_to_base <- function(vals) {
  out <- list()
  for (p in names(pillars)) {
    pl <- pillars[[p]]
    b <- pl$hl[[pl$col]]
    v <- vals[[p]]
    rr <- pair_spearman(pl$hl, b, v, pl$hl$rk)
    out[[p]] <- list(
      pooled_spearman = safe_spearman(b[pl$hl$rk], v[pl$hl$rk]),
      within_pair_spearman = summarise_rho(rr),
      by_pair = rr,
      mean_abs_level_change = mean(abs(v - b)[pl$hl$rk], na.rm = TRUE),
      country_cells_lost = sum(pl$hl$rk & is.na(v)),
      country_cells_compared = sum(pl$hl$rk)
    )
  }
  out
}

# Baseline rebuild: must reproduce the stored index exactly.
base_build <- build_both(es_inputs, eo_inputs)
rebuild_check <- lapply(names(pillars), function(p) {
  pl <- pillars[[p]]
  j <- dplyr::full_join(pl$out$index, base_build[[p]], by = c("Country", "tech", "supply_chain"))
  list(max_abs_deviation = max(abs(j[[paste0(pl$col, ".x")]] - j[[paste0(pl$col, ".y")]]), na.rm = TRUE),
       na_mismatch = sum(is.na(j[[paste0(pl$col, ".x")]]) != is.na(j[[paste0(pl$col, ".y")]])),
       rankable_mismatch = sum(j$rankable.x != j$rankable.y, na.rm = TRUE))
})
names(rebuild_check) <- names(pillars)
if (any(vapply(rebuild_check, function(r) r$max_abs_deviation > 1e-10 || r$na_mismatch > 0 ||
                 r$rankable_mismatch > 0, logical(1)))) {
  stop("Rebuilding from the theme tables does not reproduce the stored index")
}
rm(base_build); invisible(gc(verbose = FALSE))

# --- S-curve curvature ------------------------------------------------------

regamma <- function(tbl, gamma, base = GAMMA_BASE) {
  i <- which(tbl$data_type == "index" & !is.na(tbl$value))
  tbl$value[i] <- stats::plogis((gamma / base) * stats::qlogis(tbl$value[i]))
  tbl
}

# Which components are a single S-curve of a percent rank? Invert phi_0.5 and test whether the
# implied ranks of a technology-stage-year group lie on a grid k / (m - 1), k a multiple of 1/2
# (average ranks of ties), for some pool size m. Groups with one distinct value carry no
# information; for components constant within every pair the test is run on the pooled values
# of the year instead (a world-level series S-curved across technologies).
on_rank_grid <- function(v, m_max = 3000, tol = 1e-8) {
  v <- v[!is.na(v)]
  r <- unique(round(stats::plogis(2 * stats::qlogis(v)), 12))
  if (length(r) <= 1) return(NA)
  for (m in 2:m_max) {
    z <- 2 * r * (m - 1)
    if (all(abs(z - round(z)) < tol)) return(TRUE)
  }
  FALSE
}
idx_def <- getOption("opportunity_security.index_definition")
score_overalls <- unique(c(
  vapply(idx_def$pillars$energy_security$categories, function(x) x$score_variable, character(1)),
  vapply(idx_def$pillars$economic_opportunity$categories, function(x) x$score_variable, character(1))))
component_names <- unique(unlist(lapply(idx_def$overall_variables[intersect(score_overalls,
                                                                           names(idx_def$overall_variables))],
                                       function(d) unlist(d$components))))
all_inputs <- c(es_inputs, eo_inputs[setdiff(names(eo_inputs), names(es_inputs))])
classes <- dplyr::bind_rows(lapply(names(all_inputs), function(th) {
  t <- dplyr::ungroup(all_inputs[[th]])
  t <- t[t$data_type == "index" & t$variable %in% component_names, , drop = FALSE]
  if (nrow(t) == 0) return(NULL)
  if (!"sub_sector" %in% names(t)) t$sub_sector <- "All"
  t %>% dplyr::group_by(.data$category, .data$variable) %>%
    dplyr::group_modify(function(g, k) {
      within <- g %>% dplyr::group_by(.data$tech, .data$supply_chain, .data$sub_sector, .data$Year) %>%
        dplyr::summarise(ok = on_rank_grid(.data$value), .groups = "drop")
      informative <- sum(!is.na(within$ok))
      if (informative > 0) {
        exact <- all(within$ok, na.rm = TRUE)
        basis <- "within pair"
      } else {
        pooled <- g %>% dplyr::group_by(.data$Year) %>%
          dplyr::summarise(ok = on_rank_grid(.data$value), .groups = "drop")
        exact <- isTRUE(all(pooled$ok, na.rm = TRUE)) && any(!is.na(pooled$ok))
        basis <- "pooled across pairs (constant within pair)"
      }
      tibble::tibble(theme = th, n_rows = nrow(g), groups_informative = informative,
                     exact = exact, test_basis = basis)
    }) %>% dplyr::ungroup()
}))
class_cv <- classes %>% dplyr::group_by(.data$category, .data$variable) %>%
  dplyr::summarise(exact = all(.data$exact), themes = paste(sort(unique(.data$theme)), collapse = "; "),
                   test_basis = paste(sort(unique(.data$test_basis)), collapse = "; "), .groups = "drop")
exact_share <- lapply(names(pillars), function(p) {
  pl <- pillars[[p]]
  rk_keys <- key_of(pl$hl[pl$hl$rk, ])
  vc <- pl$out$variable_contributions %>%
    dplyr::filter(.data$tech %in% HEADLINE_TECHS, !is.na(.data$category_weight), key_of(.) %in% rk_keys) %>%
    dplyr::mutate(w = .data$component_weight_within_overall * .data$category_weight) %>%
    dplyr::left_join(class_cv %>% dplyr::select("category", "variable", "exact"),
                     by = c("category", "variable"))
  per_cell <- vc %>% dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(share = sum(.data$w[.data$exact %in% TRUE]) / sum(.data$w), .groups = "drop")
  list(mean_share_of_weight_on_exact_components = mean(per_cell$share, na.rm = TRUE),
       unclassified_components_with_weight = unique(vc$variable[is.na(vc$exact) & vc$w > 0]),
       components_entering = length(unique(paste(vc$category, vc$variable)[vc$w > 0])))
})
names(exact_share) <- names(pillars)
comp_in_use <- unique(dplyr::bind_rows(lapply(pillars, function(pl)
  pl$out$variable_contributions %>% dplyr::filter(.data$tech %in% HEADLINE_TECHS, !is.na(.data$category_weight)) %>%
    dplyr::distinct(.data$category, .data$variable))))
class_in_use <- class_cv %>% dplyr::semi_join(comp_in_use, by = c("category", "variable"))

gamma_res <- list()
for (g in GAMMAS) {
  v <- build_both(lapply(es_inputs, regamma, gamma = g), lapply(eo_inputs, regamma, gamma = g))
  gamma_res[[paste0("gamma_", g)]] <- compare_to_base(variant_values(v))
  rm(v); invisible(gc(verbose = FALSE))
  message("gamma ", g, " done")
}
results$gamma <- list(
  baseline_gamma = GAMMA_BASE,
  transform = "value_gamma = plogis((gamma / 0.5) * qlogis(value_0.5)) on every data_type == 'index' row",
  rebuild_reproduces_stored_index = rebuild_check,
  components = class_cv,
  components_in_use = nrow(class_in_use),
  components_in_use_exact = sum(class_in_use$exact),
  components_in_use_approximate = class_in_use$variable[!class_in_use$exact],
  components_approximate_category = paste(class_in_use$category[!class_in_use$exact],
                                          class_in_use$variable[!class_in_use$exact], sep = ": "),
  exact_weight_share = exact_share,
  variants = lapply(gamma_res, function(v) lapply(v, function(x) x[setdiff(names(x), "by_pair")]))
)
save_results()

# --- Imputation rules ---------------------------------------------------------

switch_global_average <- function(md, to) {
  md <- as.list(md)
  ga <- vapply(md, identical, logical(1), "global_average")
  if (identical(to, "none")) md[!ga] else {
    md[ga] <- to
    md
  }
}
imp_vals <- list()
imp_res <- list()
for (to in c("none", "zero")) {
  v <- build_both(es_inputs, eo_inputs,
                  md_es = switch_global_average(pillars$es$missing, to),
                  md_eo = switch_global_average(pillars$eo$missing, to))
  imp_vals[[to]] <- variant_values(v)
  imp_res[[to]] <- compare_to_base(imp_vals[[to]])
  rm(v); invisible(gc(verbose = FALSE))
  message("imputation ", to, " done")
}
# Share of each rankable headline cell's weight (category weight x component weight within the
# overall) on components filled by each rule in the baseline.
imputed_weight <- lapply(pillars, function(pl) {
  rk_keys <- key_of(pl$hl[pl$hl$rk, ])
  vc <- pl$out$variable_contributions %>%
    dplyr::filter(.data$tech %in% HEADLINE_TECHS, !is.na(.data$category_weight), key_of(.) %in% rk_keys) %>%
    dplyr::mutate(w = .data$component_weight_within_overall * .data$category_weight,
                  ga = .data$imputed %in% TRUE & grepl("global_average", .data$missing_rule_applied),
                  zr = .data$imputed %in% TRUE & !.data$ga & grepl("zero", .data$missing_rule_applied))
  per_cell <- vc %>% dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(ga = sum(.data$w[.data$ga]) / sum(.data$w), zr = sum(.data$w[.data$zr]) / sum(.data$w),
                     .groups = "drop")
  all_sc <- pl$hl[!is.na(pl$hl[[pl$col]]), ]
  list(mean_cell_weight_share_global_average = mean(per_cell$ga),
       median_cell_weight_share_global_average = stats::median(per_cell$ga),
       share_cells_with_any_global_average = mean(per_cell$ga > 0),
       mean_cell_weight_share_zero_rule = mean(per_cell$zr),
       mean_imputed_weight_share_rankable = mean(pl$hl$imputed_weight_share[pl$hl$rk]),
       mean_imputed_weight_share_all_scored = mean(all_sc$imputed_weight_share, na.rm = TRUE),
       global_average_weight_by_category = vc %>% dplyr::filter(.data$ga) %>%
         dplyr::group_by(.data$category) %>%
         dplyr::summarise(share_of_global_average_weight = sum(.data$w), .groups = "drop") %>%
         dplyr::mutate(share_of_global_average_weight = .data$share_of_global_average_weight /
                         sum(.data$share_of_global_average_weight)) %>%
         dplyr::arrange(dplyr::desc(.data$share_of_global_average_weight)))
})
results$imputation <- list(
  baseline_imputed_weight = imputed_weight,
  rules_switched = list(
    energy_security = names(Filter(function(x) identical(x, "global_average"), pillars$es$missing)),
    economic_opportunity = names(Filter(function(x) identical(x, "global_average"), pillars$eo$missing))),
  none = "global_average rules removed: uncovered countries stay NA and drop out by the available-case rule",
  zero = "global_average rules replaced by zero",
  variants = lapply(imp_res, function(v) lapply(v, function(x) x[setdiff(names(x), "by_pair")]))
)
# ===========================================================================
# 4b. Data choices of 2026-10-11/12: EIA fill, new Upstream baskets, CM-FDI
# ===========================================================================
# (a) EIA fill vs zero fill. Since 2026-10-11 the oil, gas and coal production EI does not report
#     for an economy is taken from EIA International Energy Statistics (flagged, counted as
#     observed) in the import-dependence and production themes; until then it was zero (0 per
#     cent self-sufficient). The two themes are rebuilt here with the pipeline's builders (as
#     scripts/10 calls them; the headline call must reproduce the stored tables exactly) with zero
#     in place of the EIA value: in import dependence a zero production row is added for every
#     economy and fuel with a positive EI consumption row but no production row in 2025
#     (consumption of the producers EI lists without a consumption row still comes from EIA, so
#     the roster is the headline one); the production theme is built without the EIA extract,
#     which leaves such a cell at zero. Both pillars are rebuilt with the builders, then the SI and
#     the picks. The coalition (whose supplier route reads production shares) is held fixed.
# (b) The new Upstream trade baskets (commit 0fc5b73): EV (battery-mineral ores and magnet rare
#     earths), Wind (rare earths), Nuclear (uranium ores and yellowcake, moved from Midstream) and
#     Solar (silicon metal) Upstream had no trade basket before, so no Trade category. The variant
#     drops the Trade rows of those four cells from every theme table and rebuilds both pillars
#     (their imputed shares and rankable flags re-derive). The codes moved out of Nuclear, EV and
#     Wind Midstream are not restored, so this is the Upstream half of that change.
# (c) CM-FDI off: the partnership side is not rebuilt here (the dyad tables do not fit the memory
#     budget); the PSI with and without the CM-FDI parts of Friendshore is read from the CM-FDI
#     integration study (2026-10-11 inputs, before the final rerun), and the agreement of that
#     study build with the final core-view PSI is reported, so that the reader can judge how far
#     it stands for the final one.
for (f in c("R/utils/eia_intl.R", "R/utils/iea_critical_minerals.R", "R/utils/theme_standardize.R",
            "R/categories/shared/overall_index.R", "R/categories/energy_imports/import_dependence.R",
            "R/categories/reserves/reserves.R", "R/categories/reserves/uranium.R",
            "R/categories/production/production_depth_momentum.R")) {
  source(file.path(repo_root, f))
}
source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
raw_dir <- file.path(repo_root, config$raw_data_dir)
ei_raw <- utils::read.csv(file.path(raw_dir, "ei_stat_review_world_energy.csv"))
eia_raw <- utils::read.csv(file.path(raw_dir, "eia_intl_fossil_production.csv"), stringsAsFactors = FALSE,
                           na.strings = "")
critical_path <- resolve_versioned_raw_input(
  raw_dir, pattern = "^IEA Critical Minerals Dataset [0-9]{4}[.]xlsx$|^iea_criticalminerals_[0-9]{2}[.]csv$",
  fallback = "IEA Critical Minerals Dataset 2026.xlsx", label = "IEA Critical Minerals Dataset")
critical_raw <- read_iea_critical_minerals(critical_path)
uranium_production_path <- file.path(raw_dir, "uranium_production.csv")
if (!file.exists(uranium_production_path)) {
  uranium_production_path <- file.path(raw_inputs_reference_dir(repo_root), "uranium_production.csv")
}
uranium_production_raw <- utils::read.csv(uranium_production_path, stringsAsFactors = FALSE)
build_fossil_themes <- function(ei, eia_production) {
  list(
    import_dependence = quietly(standardize_theme_types(
      import_dependence(ei, eia = eia_raw, eia_fill_consumption = TRUE), country_info = country_info)),
    production_depth_momentum = quietly(standardize_theme_types(production_depth_momentum(
      ei = ei, critical = critical_raw, mineral_demand_clean = reserves_build_mineral_demand_clean(critical_raw),
      country_info = country_info, uranium_production = uranium_production_raw,
      eia = if (eia_production) eia_raw else NULL), country_info = country_info))
  )
}
fossil_head <- build_fossil_themes(ei_raw, eia_production = TRUE)
if (!isTRUE(all.equal(as.data.frame(fossil_head$import_dependence), as.data.frame(es_inputs$import_dependence))) ||
    !isTRUE(all.equal(as.data.frame(fossil_head$production_depth_momentum),
                      as.data.frame(eo_inputs$production_depth_momentum)))) {
  stop("The import-dependence and production builders do not reproduce the stored theme tables")
}
# Zero production rows: every economy and fuel with a positive EI consumption row but no production
# row in 2025 (the cells the EIA fill reaches).
EIA_YEAR <- 2025L
fuel_pairs <- c(oilprod_kbd = "oil_tes_ej", gasprod_ej = "gas_tes_ej", coalprod_ej = "coal_tes_ej")
ei_agg <- grepl("World|Other|Total|OECD|OPEC|USSR", ei_raw$Country)
zero_rows <- dplyr::bind_rows(lapply(names(fuel_pairs), function(v) {
  has_cons <- unique(ei_raw$Country[!ei_agg & ei_raw$Year == EIA_YEAR & ei_raw$Var == fuel_pairs[[v]] &
                                      !is.na(ei_raw$Value) & ei_raw$Value > 0])
  has_prod <- unique(ei_raw$Country[ei_raw$Year == EIA_YEAR & ei_raw$Var == v & !is.na(ei_raw$Value)])
  miss <- setdiff(has_cons, has_prod)
  if (length(miss) == 0) return(NULL)
  tmpl <- ei_raw[match(miss, ei_raw$Country), , drop = FALSE]
  tmpl$Year <- EIA_YEAR
  tmpl$Var <- v
  tmpl$Value <- 0
  tmpl
}))
fossil_zero <- build_fossil_themes(rbind(ei_raw, zero_rows[, names(ei_raw)]), eia_production = FALSE)
eia_rows <- list(import_dependence = sum(grepl("EIA", fossil_head$import_dependence$source)),
                 production_depth_momentum = sum(grepl("EIA", fossil_head$production_depth_momentum$source)))
es_in_zero <- es_inputs
es_in_zero$import_dependence <- fossil_zero$import_dependence
eo_in_zero <- eo_inputs
eo_in_zero$production_depth_momentum <- fossil_zero$production_depth_momentum
build_zero <- build_both(es_in_zero, eo_in_zero)
rm(es_in_zero, eo_in_zero, ei_raw, critical_raw, fossil_head, fossil_zero); invisible(gc(verbose = FALSE))

# (b) Upstream baskets removed.
NEW_BASKET_TECHS <- c("Electric Vehicles", "Wind", "Nuclear", "Solar")
drop_new_baskets <- function(inputs) {
  dropped <- integer()
  for (nm in names(inputs)) {
    t <- inputs[[nm]]
    if (!all(c("category", "tech", "supply_chain") %in% names(t))) next
    rm_rows <- t$category %in% "Trade" & t$tech %in% NEW_BASKET_TECHS & t$supply_chain %in% "Upstream"
    if (any(rm_rows)) {
      dropped[nm] <- sum(rm_rows)
      inputs[[nm]] <- t[!rm_rows, , drop = FALSE]
    }
  }
  attr(inputs, "dropped") <- dropped
  inputs
}
es_in_nb <- drop_new_baskets(es_inputs)
eo_in_nb <- drop_new_baskets(eo_inputs)
nb_dropped <- list(es = as.list(attr(es_in_nb, "dropped")), eo = as.list(attr(eo_in_nb, "dropped")))
if (length(nb_dropped$es) == 0 || length(nb_dropped$eo) == 0) {
  stop("No Trade rows of the new Upstream baskets found in the theme tables")
}
build_nb <- build_both(es_in_nb, eo_in_nb)
rm(es_in_nb, eo_in_nb); invisible(gc(verbose = FALSE))

# Pillar comparison, rankable counts, SI and picks of a rebuilt pair of pillars.
rankable_counts <- function(tbl, col) {
  h <- tbl[tbl$tech %in% HEADLINE_TECHS, ]
  list(scored = sum(!is.na(h[[col]])), rankable = sum(h$rankable %in% TRUE))
}
data_choice_variant <- function(label, built, cells) {
  st <- si_build(built$es, built$eo, io$policy_index)
  pk <- picks_of(st) %>% dplyr::select("Country", "sector")
  cmpb <- compare_to_base(variant_values(built))
  # Rankable cells and within-pair agreement in the technology-stage cells the choice touches.
  aff <- lapply(names(pillars), function(p) {
    pl <- pillars[[p]]
    b <- pl$hl[pl$hl$pair %in% cells, ]
    v <- built[[p]][built[[p]]$tech %in% HEADLINE_TECHS & pair_of(built[[p]]) %in% cells, ]
    list(scored_baseline = sum(!is.na(b[[pl$col]])), scored_variant = sum(!is.na(v[[pl$col]])),
         rankable_baseline = sum(b$rankable), rankable_variant = sum(v$rankable %in% TRUE),
         by_pair = cmpb[[p]]$by_pair %>% dplyr::filter(.data$pair %in% cells))
  })
  names(aff) <- names(pillars)
  list(rv = c(rebuilt_variant(label, st, pk),
              list(pillars = lapply(cmpb, function(x) x[setdiff(names(x), "by_pair")]),
                   rankable = list(es = rankable_counts(built$es, pillars$es$col),
                                   eo = rankable_counts(built$eo, pillars$eo$col)),
                   affected_cells = aff)),
       picks = pk)
}
fossil_cells <- paste(c("Oil", "Gas", "Coal"), "Upstream", sep = " - ")
dc_zero <- data_choice_variant("Fossil production EI does not report set to zero instead of the EIA value",
                               build_zero, fossil_cells)
dc_nb <- data_choice_variant("New Upstream trade baskets removed (EV, Wind, Nuclear, Solar Upstream without Trade)",
                             build_nb, paste(NEW_BASKET_TECHS, "Upstream", sep = " - "))
variant_picks$eia_zero <- dc_zero$picks
variant_picks$no_new_baskets <- dc_nb$picks
rm(build_zero, build_nb); invisible(gc(verbose = FALSE))

# (c) CM-FDI off, from the CM-FDI integration study (psi_<view>_<variant>.csv: v_o / rankable_o the
# IMF-only build without chinese_parent_exposure, v_n / rankable_n the CM-FDI build; noexp = the
# exposure component off, noties = the CM-FDI investment ties off).
cmfdi_dir <- file.path(scratch_dir, "fixes7", "pipeline", "out")
psi_final_core <- readRDS(file.path(processed_dir, "psi_tbl.rds"))
cmfdi_cmp <- function(view, variant) {
  f <- file.path(cmfdi_dir, sprintf("psi_%s_%s.csv", view, variant))
  if (!file.exists(f)) return(NULL)
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  both <- d$rankable_o %in% TRUE & d$rankable_n %in% TRUE
  ro <- rank(-d$v_o[both]); rn <- rank(-d$v_n[both])
  top_o <- d$partner_iso[both][ro <= PSI_TOP]; top_n <- d$partner_iso[both][rn <= PSI_TOP]
  out <- list(file = basename(f), partners = nrow(d), ranked_without = sum(d$rankable_o %in% TRUE),
              ranked_with = sum(d$rankable_n %in% TRUE), ranked_both = sum(both),
              spearman = safe_spearman(d$v_o[both], d$v_n[both]),
              max_abs_rank_change = max(abs(ro - rn)), mean_abs_rank_change = mean(abs(ro - rn)),
              top15_overlap = length(intersect(top_o, top_n)))
  if (view == "coalition") {
    j <- dplyr::inner_join(d %>% dplyr::select("partner_iso", study = "v_n", study_rk = "rankable_n"),
                           psi_final_core %>% dplyr::select("partner_iso", final = "value", final_rk = "rankable"),
                           by = "partner_iso")
    k <- j$study_rk %in% TRUE & j$final_rk %in% TRUE
    out$study_vs_final_core_psi_spearman <- safe_spearman(j$study[k], j$final[k])
    out$study_vs_final_core_psi_n <- sum(k)
  }
  out
}
cmfdi_off <- list(
  source = "<scratch>/fixes7/pipeline/out (CM-FDI integration study, 2026-10-11 inputs; not recomputed on the final rerun)",
  note = paste("'without' = IMF outbound positions in every cell and no chinese_parent_exposure (both CM-FDI parts",
               "off); noexp / noties switch off one part; the study's coalition view averages over the core members"),
  both_off = list(coalition = cmfdi_cmp("coalition", "base"), world = cmfdi_cmp("world", "base")),
  exposure_off = list(coalition = cmfdi_cmp("coalition", "noexp")),
  ties_off = list(coalition = cmfdi_cmp("coalition", "noties"))
)
if (is.null(cmfdi_off$both_off$coalition)) stop("CM-FDI study output not found in ", cmfdi_dir)

results$data_choices <- list(
  eia_fill = c(dc_zero$rv, list(
    zero_production_rows_added = nrow(zero_rows),
    zero_production_rows_by_fuel = as.list(table(zero_rows$Var)),
    eia_sourced_rows_headline = eia_rows,
    headline_builders_reproduce_stored_tables = TRUE)),
  upstream_baskets = c(dc_nb$rv, list(trade_rows_dropped = nb_dropped,
                                      cells = paste(NEW_BASKET_TECHS, "Upstream", sep = " - "))),
  cmfdi_off = cmfdi_off
)
save_results()
message("data choices done")

results$build_warnings_unique <- unique(substr(build_warnings, 1, 200))
save_results()
rm(es_inputs, eo_inputs, all_inputs); invisible(gc(verbose = FALSE))

# ===========================================================================
# 5. Rankability threshold tau in {0.3, 0.5, 0.7}
# ===========================================================================
# The rule filters cells and never changes a value, so the order of two cells rankable under
# both thresholds is the same; what tau changes is the comparison set. We report the cells
# kept, how robust the rankings of the kept cells are to the imputation rule (the criterion
# config/rankability.yml was calibrated on), how far the top-10 lists change, the Strategic
# Index sample and picks, and the PSI's ranked partners.

tau_rows <- list()
top_overlap_rows <- list()
for (p in names(pillars)) {
  pl <- pillars[[p]]
  hl <- pl$hl
  b <- hl[[pl$col]]
  top_sets <- list()
  for (tau in TAUS) {
    rk_t <- rankable_at(b, hl$imputed_weight_share, hl$rank_exclusion, tau)
    ps <- tibble::tibble(pair = hl$pair, rk = rk_t, v = b) %>% dplyr::filter(.data$rk) %>%
      dplyr::group_by(.data$pair) %>% dplyr::summarise(n = dplyr::n(), sd = stats::sd(.data$v), .groups = "drop")
    use_pairs <- ps$pair[ps$n >= 3 & ps$sd > TOL]
    keep <- rk_t & hl$pair %in% use_pairs
    rho_none <- summarise_rho(pair_spearman(hl, b, imp_vals$none[[p]], keep))
    rho_zero <- summarise_rho(pair_spearman(hl, b, imp_vals$zero[[p]], keep))
    by_stage <- vapply(STAGES, function(s) sum(rk_t & hl$supply_chain == s), integer(1))
    sov <- !grepl("not a sovereign economy", hl$rank_exclusion) & !is.na(b)
    tau_rows[[length(tau_rows) + 1]] <- tibble::tibble(
      pillar = pl$label, tau = tau, rankable_cells = sum(rk_t),
      share_of_sovereign_scored = sum(rk_t) / sum(sov),
      upstream = by_stage[["Upstream"]], midstream = by_stage[["Midstream"]], downstream = by_stage[["Downstream"]],
      rankable_countries = dplyr::n_distinct(hl$Country[rk_t]),
      min_cells_in_a_pair = min(ps$n), pairs_ranking = length(use_pairs),
      rho_none_median = rho_none$median, rho_none_p25 = rho_none$p25, rho_none_min = rho_none$min,
      rho_zero_median = rho_zero$median, rho_zero_p25 = rho_zero$p25, rho_zero_min = rho_zero$min,
      members_rankable_cells = sum(rk_t & hl$Country %in% allies$Country)
    )
    top_sets[[as.character(tau)]] <- tibble::tibble(pair = hl$pair, Country = hl$Country, v = b, rk = rk_t) %>%
      dplyr::filter(.data$rk) %>% dplyr::group_by(.data$pair) %>%
      dplyr::slice_max(.data$v, n = TOP_N, with_ties = FALSE) %>% dplyr::ungroup()
  }
  base_set <- top_sets[[as.character(TAU_BASE)]]
  for (tau in setdiff(TAUS, TAU_BASE)) {
    alt <- top_sets[[as.character(tau)]]
    ov <- base_set %>% dplyr::group_by(.data$pair) %>%
      dplyr::summarise(overlap = sum(.data$Country %in% alt$Country[alt$pair == dplyr::cur_group()$pair]) / dplyr::n(),
                       .groups = "drop")
    top_overlap_rows[[length(top_overlap_rows) + 1]] <- tibble::tibble(
      pillar = pl$label, tau = tau, n_pairs = nrow(ov), mean_overlap = mean(ov$overlap),
      min_overlap = min(ov$overlap), pairs_identical = sum(ov$overlap == 1))
  }
}
tau_tbl <- dplyr::bind_rows(tau_rows)
top_overlap_tbl <- dplyr::bind_rows(top_overlap_rows)

# Strategic Index at each tau.
si_tau <- list()
for (tau in TAUS) {
  es_t <- retag_rankable(io$energy_security_index, "Energy_Security_Index", tau)
  eo_t <- retag_rankable(io$economic_opportunity_index, "Economic_Opportunity_Index", tau)
  if (tau == TAU_BASE) {
    stopifnot(identical(es_t$rankable, io$energy_security_index$rankable),
              identical(eo_t$rankable, io$economic_opportunity_index$rankable))
  }
  st <- si_build(es_t, eo_t, io$policy_index)
  pk <- picks_of(st)
  common <- dplyr::inner_join(si_tbl %>% dplyr::select("Country", "tech", "supply_chain", a = "strategic_index"),
                              st %>% dplyr::filter(!is.na(.data$strategic_index)) %>%
                                dplyr::select("Country", "tech", "supply_chain", b = "strategic_index"),
                              by = c("Country", "tech", "supply_chain"))
  cr <- common %>% dplyr::group_by(.data$Country) %>%
    dplyr::summarise(rho = safe_spearman(.data$a, .data$b), .groups = "drop")
  ag <- agree_top(pk %>% dplyr::select("Country", "sector"))
  si_tau[[as.character(tau)]] <- list(
    tau = tau, si_cells = sum(!is.na(st$strategic_index)),
    si_countries = dplyr::n_distinct(st$Country[!is.na(st$strategic_index)]),
    degenerate_sectors = attr(st, "degenerate"),
    common_cells_with_baseline = nrow(common),
    within_country_spearman_median = stats::median(cr$rho, na.rm = TRUE),
    within_country_spearman_min = min(cr$rho, na.rm = TRUE),
    members_with_pick = nrow(pk), picks_same = ag$n_same, picks_compared = ag$n_compared,
    picks_changed = ag$changed, lost_pick = ag$lost_pick, new_pick = ag$new_pick,
    pick_counts = ag$sector_counts
  )
}
if (si_tau[[as.character(TAU_BASE)]]$si_cells != nrow(si_tbl)) stop("SI at the baseline tau does not reproduce the sample")

# PSI views at each tau (same rule; psi_flag_rankable()).
psi_views <- list(
  core = readRDS(file.path(processed_dir, "psi_tbl.rds")),
  full_coalition = readRDS(file.path(processed_dir, "psi_full_coalition_tbl.rds")),
  world = readRDS(file.path(processed_dir, "psi_world_tbl.rds"))
)
psi_tau <- dplyr::bind_rows(lapply(names(psi_views), function(v) {
  x <- psi_views[[v]]
  chk <- rankable_at(x$value, x$imputed_weight_share, x$rank_exclusion, TAU_BASE)
  if (!identical(chk, x$rankable %in% TRUE)) stop("PSI ", v, ": rankable_at() does not reproduce the stored flag")
  dplyr::bind_rows(lapply(TAUS, function(tau) tibble::tibble(
    view = v, tau = tau, ranked = sum(rankable_at(x$value, x$imputed_weight_share, x$rank_exclusion, tau)))))
}))

results$rankability <- list(
  rule = "rankable = scored, sovereign and imputed weight share <= tau (config/rankability.yml, tau = 0.5)",
  pillars = tau_tbl,
  top10_overlap_with_baseline = top_overlap_tbl,
  strategic_index = si_tau,
  psi = psi_tau
)
save_results()
message("rankability done")

# ===========================================================================
# 6. PSI viewpoints: core (headline), full coalition, world
# ===========================================================================

psi_cmp <- psi_views$core %>%
  dplyr::select("partner_iso", "Country", core = "value", core_rk = "rankable", core_rank = "rank") %>%
  dplyr::left_join(psi_views$full_coalition %>% dplyr::select("partner_iso", full_coalition = "value",
                                                              full_rk = "rankable", full_rank = "rank"),
                   by = "partner_iso") %>%
  dplyr::left_join(psi_views$world %>% dplyr::select("partner_iso", world = "value", world_rk = "rankable",
                                                     world_rank = "rank"), by = "partner_iso")
if (anyDuplicated(psi_cmp$partner_iso)) stop("PSI tables have duplicated partners")
psi_pair <- function(alt, alt_rk, alt_rank) {
  both <- psi_cmp$core_rk & psi_cmp[[alt_rk]]
  top_core <- psi_cmp$partner_iso[psi_cmp$core_rk & psi_cmp$core_rank <= PSI_TOP]
  top_alt <- psi_cmp$partner_iso[psi_cmp[[alt_rk]] & psi_cmp[[alt_rank]] <= PSI_TOP]
  list(n_ranked_both = sum(both),
       spearman = safe_spearman(psi_cmp$core[both], psi_cmp[[alt]][both]),
       top15_overlap = length(intersect(top_core, top_alt)),
       top15_core_only = sort(setdiff(top_core, top_alt)),
       top15_alt_only = sort(setdiff(top_alt, top_core)),
       top15_alt = top_alt[order(psi_cmp[[alt_rank]][match(top_alt, psi_cmp$partner_iso)])],
       median_abs_rank_change_ranked_both = {
         rc <- rank(-psi_cmp$core[both]); ra <- rank(-psi_cmp[[alt]][both])
         stats::median(abs(rc - ra))
       },
       max_abs_rank_change = {
         rc <- rank(-psi_cmp$core[both]); ra <- rank(-psi_cmp[[alt]][both])
         i <- which.max(abs(rc - ra))
         list(partner = psi_cmp$partner_iso[both][i], core_rank = rc[i], alt_rank = ra[i])
       })
}
results$psi_views <- list(
  reporters = list(core = max(psi_views$core$n_reporters_friendshore),
                   full_coalition = max(psi_views$full_coalition$n_reporters_friendshore),
                   world = max(psi_views$world$n_reporters_friendshore)),
  ranked = list(core = sum(psi_cmp$core_rk), full_coalition = sum(psi_cmp$full_rk), world = sum(psi_cmp$world_rk)),
  partners = nrow(psi_cmp),
  core_top15 = psi_cmp$partner_iso[order(psi_cmp$core_rank)][seq_len(PSI_TOP)],
  core_vs_full_coalition = psi_pair("full_coalition", "full_rk", "full_rank"),
  core_vs_world = psi_pair("world", "world_rk", "world_rank"),
  full_coalition_vs_world = {
    both <- psi_cmp$full_rk & psi_cmp$world_rk
    list(n = sum(both), spearman = safe_spearman(psi_cmp$full_coalition[both], psi_cmp$world[both]))
  },
  mean_value = list(core = mean(psi_cmp$core[psi_cmp$core_rk]),
                    full_coalition = mean(psi_cmp$full_coalition[psi_cmp$full_rk]),
                    world = mean(psi_cmp$world[psi_cmp$world_rk]))
)
save_results()
message("psi done")

# ===========================================================================
# 7. Headline summary and placeholder fills
# ===========================================================================

med_of <- function(x) x$within_pair_spearman$median
rg <- list(
  es = range(c(vapply(gamma_res, function(v) med_of(v$es), numeric(1)),
               weights_mc$es$within_pair_rho_median_over_pairs_per_draw$median)),
  eo = range(c(vapply(gamma_res, function(v) med_of(v$eo), numeric(1)),
               weights_mc$eo$within_pair_rho_median_over_pairs_per_draw$median)))
results$summary <- list(
  es_eo_within_pair_median_spearman_range_over_gamma_and_weight_variants = rg,
  si_security_orientation_pooled_spearman = si_variants$security_orientation$pooled_spearman,
  si_security_orientation_within_country_median = si_variants$security_orientation$within_country_spearman_median
)

f2 <- function(x) sprintf("%.2f", x)
f3 <- function(x) sprintf("%.3f", x)
n_al <- function(v) si_variants[[v]]$allied_top_sector_same
n_mem <- nrow(base_top)
results$tbd_fills <- list(
  intro_scurve_weights_median_spearman = list(
    file = "paper/sections/01_introduction.tex",
    tbd = "median Spearman correlation of ES and EO ranks with the baseline across S-curve curvatures and category-weight perturbations",
    fill = sprintf(paste0("closely (median within-pair Spearman correlations, over rankable cells, between %s and %s ",
                          "for $\\ES$ and between %s and %s for $\\EO$, the lowest at $\\gamma=2$)"),
                   f3(rg$es[1]), f3(rg$es[2]), f3(rg$eo[1]), f3(rg$eo[2]))),
  intro_si_sign_spearman = list(
    file = "paper/sections/01_introduction.tex",
    tbd = "rank correlation between SI with security need $1-\\ES$ and SI with security $\\ES$",
    fill = sprintf(paste0("%s (Spearman, pooled over the %s cells of the %d countries with a Strategic Index; the ",
                          "median within-country correlation is %s), and the allied pick survives the change of ",
                          "sign for only %d of the %d coalition members"),
                   f2(si_variants$security_orientation$pooled_spearman), format(nrow(si_tbl), big.mark = "{,}"),
                   length(countries_si), f2(si_variants$security_orientation$within_country_spearman_median),
                   n_al("security_orientation"), n_mem)),
  strategic_all_ranked = list(
    file = "paper/sections/08_strategic.tex",
    tbd = "rank correlation and top-sector agreement between the baseline and the all-ranked variant",
    fill = sprintf(paste0("a Spearman correlation of %s between the baseline and the all-ranked variant ",
                          "(pooled; median within country %s) and the same allied pick for %d of the %d coalition members"),
                   f2(si_variants$all_ranked$pooled_spearman),
                   f2(si_variants$all_ranked$within_country_spearman_median), n_al("all_ranked"), n_mem)),
  strategic_top_sector_agreement = list(
    file = "paper/sections/08_strategic.tex",
    tbd = "top-sector agreement under the opposite security orientation, without the sector priors, and with all terms ranked within country",
    fill = sprintf(paste0("that the allied pick is unchanged for %d of the %d coalition members under the opposite ",
                          "security orientation, for %d without the sector priors and for %d with all terms ",
                          "ranked within country"),
                   n_al("security_orientation"), n_mem, n_al("no_sector_priors"), n_al("all_ranked"))),
  appendix_d_nipo = list(
    file = "paper/sections/appendix_d_corrections.tex",
    tbd = "sensitivity of the Policy index, the SI and the allied selection to dropping the NIPO component, or to rescoring it on the July 2026 extract if that run is made",
    fill = sprintf(paste0("the effect of adding the October 2026 NIPO component (GTA NIPO as of 5 October 2026, ",
                          "%d jurisdictions) back to the Policy index with its former weight (NIPO 4, IEA PAMS 3, ",
                          "CAT 2), IEA PAMS still required, which the final composite excludes: NIPO is observed ",
                          "for %d policy-eligible jurisdictions and in %.0f per cent of the Strategic Index cells; ",
                          "for those jurisdictions the within-country Spearman correlation between the policy ",
                          "ranks with and without it has a median of %s (range %s to %s), and the allied pick is ",
                          "unchanged for %d of the %d coalition members (%d of the %d members with a NIPO score)"),
                   policy_nipo_added$component_jurisdictions,
                   policy_nipo_added$nipo_countries_policy_eligible, 100 * policy_nipo_added$share_si_cells_with_nipo,
                   f2(policy_nipo_added$within_country_spearman_policy_rank_median),
                   f2(policy_nipo_added$within_country_spearman_policy_rank_min),
                   f2(policy_nipo_added$within_country_spearman_policy_rank_max),
                   n_al("nipo_added"), n_mem, policy_nipo_added$members_picked_with_nipo_same,
                   policy_nipo_added$members_picked_with_nipo)),
  strategic_margins = list(
    file = "paper/sections/08_strategic.tex",
    tbd = "Number of members whose margin between the first and second sector is below 0.01, and the members with no selection",
    fill = sprintf(paste0("The margin between a member's first and second eligible sectors is below 0.005 for %d of ",
                          "the %d members, below 0.01 for %d and below 0.02 for %d (median %s); every member has a selection"),
                   results$si$baseline_margin$n_below_0.005, n_mem, results$si$baseline_margin$n_below_0.01,
                   results$si$baseline_margin$n_below_0.02, f3(results$si$baseline_margin$median))),
  policy_basis_spearman = list(
    file = "paper/sections/08_strategic.tex",
    tbd = "within-country agreement of the policy ranking on IEA PAMS alone and on the composite",
    fill = sprintf(paste0("for the %d countries scored on both components, the within-country Spearman correlation ",
                          "between the policy values on IEA PAMS alone and on the composite has a median of %s ",
                          "(interquartile range %s to %s)"),
                   results$policy$pams_only$policy_value_within_country_spearman_both_basis$n,
                   f2(results$policy$pams_only$policy_value_within_country_spearman_both_basis$median),
                   f2(results$policy$pams_only$policy_value_within_country_spearman_both_basis$p25),
                   f2(results$policy$pams_only$policy_value_within_country_spearman_both_basis$p75))),
  normalization_category_sets = list(
    file = "paper/sections/04_normalization.tex",
    tbd = "share of ES and EO technology-stage pairs in which all scored countries share one category set, and the distribution of the number of observed categories per cell",
    fill = sprintf(paste0("all scored economies share one category set in %d of the %d headline $\\ES$ pairs and in %d of ",
                          "the %d $\\EO$ pairs, and the median scored cell observes %s of the ten categories in $\\ES$ ",
                          "and %s in $\\EO$"),
                   pair_structure_all$es$n_pairs_shared_category_set, pair_structure_all$es$n_pairs,
                   pair_structure_all$eo$n_pairs_shared_category_set, pair_structure_all$eo$n_pairs,
                   format(pair_structure_all$es$observed_categories_per_cell_median),
                   format(pair_structure_all$eo$observed_categories_per_cell_median)))
)
save_results()

# ===========================================================================
# Tables (paper/tables/robustness_*.tex)
# ===========================================================================

tables_dir <- paper_dir("tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
write_tab <- function(lines, name) writeLines(c(paste0("% Generated by scripts/paper/fig_robustness.R; do not edit by hand."),
                                                 lines), file.path(tables_dir, paste0(name, ".tex")), useBytes = TRUE)
fmt_n <- function(x) formatC(x, format = "d", big.mark = "{,}")
d2 <- function(x) ifelse(is.na(x), "--", sprintf("%.2f", x))
d1 <- function(x) sprintf("%.1f", x)

# --- Summary of within-pair rank stability ----------------------------------------------------
sum_row <- function(label, es, eo) {
  paste0("    ", label, " & ", d2(es$pooled), " & ", d2(es$median), " & ", d2(es$min), " & ",
         d2(eo$pooled), " & ", d2(eo$median), " & ", d2(eo$min), " \\\\")
}
cmp_cells <- function(x) list(pooled = x$pooled_spearman, median = x$within_pair_spearman$median,
                              min = x$within_pair_spearman$min)
mc_cells <- function(p) list(pooled = weights_mc[[p]]$pooled_rho$median,
                             median = weights_mc[[p]]$within_pair_rho_pair_medians$median,
                             min = weights_mc[[p]]$within_pair_rho_pair_medians$min)
eq_cells <- function(p) list(pooled = weights_mc[[p]]$equal_weights$pooled_spearman,
                             median = weights_mc[[p]]$equal_weights$within_pair_spearman$median,
                             min = weights_mc[[p]]$equal_weights$within_pair_spearman$min)
vin_cells <- function(p) list(pooled = vintage[[p]]$pooled_spearman, median = vintage[[p]]$within_pair_spearman$median,
                              min = vintage[[p]]$within_pair_spearman$min)
vh_cells <- function(p) list(pooled = vintage[[p]]$vintage2025_vs_headline$pooled_spearman,
                             median = vintage[[p]]$vintage2025_vs_headline$within_pair_spearman$median,
                             min = vintage[[p]]$vintage2025_vs_headline$within_pair_spearman$min)
es_m <- results$meta$cells$es; eo_m <- results$meta$cells$eo
write_tab(c(
  "\\begin{table}[tbp]", "  \\centering", "  \\begin{threeparttable}",
  "  \\caption{Robustness of within-pair country rankings: Spearman correlations with the baseline}",
  "  \\label{tab:robustness_summary}", "  \\small",
  "  \\begin{tabular}{lcccccc}", "    \\toprule",
  "     & \\multicolumn{3}{c}{Energy Security ($\\ES$)} & \\multicolumn{3}{c}{Economic Opportunity ($\\EO$)} \\\\",
  "    \\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  "    Variant & Pooled & Median & Minimum & Pooled & Median & Minimum \\\\", "    \\midrule",
  "    \\multicolumn{7}{l}{\\emph{Category weights}}\\\\",
  sum_row("\\quad Dirichlet draws", mc_cells("es"), mc_cells("eo")),
  sum_row("\\quad Equal weights", eq_cells("es"), eq_cells("eo")),
  "    \\multicolumn{7}{l}{\\emph{S-curve curvature} (baseline $\\gamma=0.5$)}\\\\",
  sum_row("\\quad $\\gamma=0.25$", cmp_cells(gamma_res$gamma_0.25$es), cmp_cells(gamma_res$gamma_0.25$eo)),
  sum_row("\\quad $\\gamma=1$ (percent rank)", cmp_cells(gamma_res$gamma_1$es), cmp_cells(gamma_res$gamma_1$eo)),
  sum_row("\\quad $\\gamma=2$", cmp_cells(gamma_res$gamma_2$es), cmp_cells(gamma_res$gamma_2$eo)),
  "    \\multicolumn{7}{l}{\\emph{Global-average imputation rules}}\\\\",
  sum_row("\\quad Removed (available case)", cmp_cells(imp_res$none$es), cmp_cells(imp_res$none$eo)),
  sum_row("\\quad Replaced by zero", cmp_cells(imp_res$zero$es), cmp_cells(imp_res$zero$eo)),
  "    \\multicolumn{7}{l}{\\emph{Vintage}}\\\\",
  sum_row("\\quad 2020 against 2025", vin_cells("es"), vin_cells("eo")),
  sum_row("\\quad 2025 vintage against headline", vh_cells("es"), vh_cells("eo")),
  "    \\bottomrule", "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} Spearman rank correlations between the baseline pillar and each variant, over ",
         "the baseline's rankable cells (sovereign economies with at most half of the index weight imputed). ",
         "``Pooled'' is computed over all rankable cells of the eleven headline technologies; ``Median'' and ",
         "``Minimum'' are taken across technology--stage pairs of the correlation computed within each pair. For ",
         "the Dirichlet draws the pooled entry is the median over ", fmt_n(N_DRAWS), " draws and the within-pair ",
         "entries summarise, across pairs, each pair's median over the draws. The vintage rows compare the 2020 ",
         "and 2025 builds of \\texttt{scripts/40\\_build\\_index\\_vintages.R}, in which only the time-varying sources ",
         "differ (", fmt_n(vintage$es$n_country_cells), " $\\ES$ and ", fmt_n(vintage$eo$n_country_cells),
         " $\\EO$ rankable cells observed in both vintages), and the 2025 build with the headline index. Sample: ",
         es_m$rankable_pairs, " $\\ES$ pairs (", fmt_n(es_m$rankable_cells_in_rankable_pairs), " rankable cells) and ",
         eo_m$rankable_pairs, " $\\EO$ pairs (", fmt_n(eo_m$rankable_cells_in_rankable_pairs), " rankable cells). ",
         "Entries are rounded to two decimals. Source: \\texttt{scripts/paper/fig\\_robustness.R}; ",
         "\\texttt{paper/results/robustness.json}."),
  "  \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}"), "robustness_summary")

# --- Nominal weight, effective weight, variance share --------------------------------------------
imp_rows <- function(p) {
  bc <- weights_mc[[p]]$variance_importance$by_category
  vapply(seq_len(nrow(bc)), function(i) sprintf("    \\quad %s & %s & %s & %s & %d & %s \\\\",
                                                  bc$category[i], d1(100 * bc$nominal_share[i]),
                                                  d1(100 * bc$mean_renormalised_weight[i]),
                                                  d1(100 * bc$mean_variance_share[i]), bc$pairs_observed[i],
                                                  d2(bc$loo_median[i])), character(1))
}
write_tab(c(
  "\\begin{table}[tbp]", "  \\centering", "  \\begin{threeparttable}",
  "  \\caption{Nominal weight, effective weight and variance share of each category within technology--stage pairs}",
  "  \\label{tab:robustness_importance}", "  \\small",
  "  \\begin{tabular}{lrrrrr}", "    \\toprule",
  "    & Nominal & Effective & Variance & & Leave-one- \\\\",
  "    Category & share (\\%) & weight (\\%) & share (\\%) & Pairs & out $\\rho$ \\\\", "    \\midrule",
  sprintf("    \\multicolumn{6}{l}{\\emph{Panel A. Energy Security ($\\ES$), %d pairs}}\\\\", weights_mc$es$n_pairs),
  imp_rows("es"), "    \\addlinespace",
  sprintf("    \\multicolumn{6}{l}{\\emph{Panel B. Economic Opportunity ($\\EO$), %d pairs}}\\\\", weights_mc$eo$n_pairs),
  imp_rows("eo"),
  "    \\bottomrule", "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} Nominal share: $w_k/\\sum_{k'}w_{k'}$ from \\texttt{config/weights.yml}. Effective ",
         "weight: mean over the rankable headline pairs of the weight $w_k/\\sum_{l\\ \\mathrm{observed}}w_l$ a ",
         "category receives after renormalisation over the observed categories; zero where the category is not ",
         "observed. Variance share: mean over the same pairs of $S_{kts}$ (equation~\\eqref{eq:variance_share}), the ",
         "covariance of the category's contribution with the pillar across the rankable cells of the pair divided by ",
         "the pillar's variance; zero where the category is not observed. Each of the three columns sums to 100 within ",
         "a panel up to rounding. Pairs: number of pairs in which the category is observed. Leave-one-out $\\rho$: ",
         "median, over the pairs in which the category is observed and the reduced pillar still ranks countries, of ",
         "the within-pair Spearman correlation between the baseline pillar and the pillar without the category ",
         "(Figure~\\ref{fig:robustness_loo}). Rows are ordered by the variance share. Sample: rankable cells of the ",
         "eleven headline technologies. Source: category scores in \\texttt{index\\_outputs.rds}; ",
         "\\texttt{scripts/paper/fig\\_robustness.R} (\\texttt{weights.<pillar>.variance\\_importance} in ",
         "\\texttt{paper/results/robustness.json})."),
  "  \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}"), "robustness_importance")

# --- Rankability threshold ------------------------------------------------------------------------
tau_line <- function(tau) {
  es <- tau_tbl[tau_tbl$pillar == "Energy Security" & tau_tbl$tau == tau, ]
  eo <- tau_tbl[tau_tbl$pillar == "Economic Opportunity" & tau_tbl$tau == tau, ]
  st <- si_tau[[as.character(tau)]]
  ps <- psi_tau[psi_tau$tau == tau, ]
  c(sprintf("    %s%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            format(tau), if (tau == TAU_BASE) "$^{\\ast}$" else "",
            fmt_n(es$rankable_cells), d2(es$rho_none_median), d2(es$rho_zero_median),
            fmt_n(eo$rankable_cells), d2(eo$rho_none_median), d2(eo$rho_zero_median),
            fmt_n(st$si_cells), st$si_countries,
            sprintf("%d/%d", st$picks_same, st$members_with_pick),
            sprintf("%d/%d", ps$ranked[ps$view == "core"], ps$ranked[ps$view == "world"])))
}
write_tab(c(
  "\\begin{table}[tbp]", "  \\centering", "  \\begin{threeparttable}",
  "  \\caption{The rankability threshold: cells ranked, robustness of their rankings, and the Strategic Index}",
  "  \\label{tab:robustness_rankability}", "  \\small",
  "  \\begin{tabular}{lcccccccccc}", "    \\toprule",
  "    & \\multicolumn{3}{c}{$\\ES$} & \\multicolumn{3}{c}{$\\EO$} & \\multicolumn{3}{c}{Strategic Index} & PSI \\\\",
  "    \\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}\\cmidrule(lr){11-11}",
  "    $\\tau$ & Cells & $\\rho_{\\text{none}}$ & $\\rho_{\\text{zero}}$ & Cells & $\\rho_{\\text{none}}$ & $\\rho_{\\text{zero}}$ & Cells & Countries & Picks & Ranked \\\\",
  "    \\midrule",
  unlist(lapply(TAUS, tau_line)),
  "    \\bottomrule", "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} A cell is rankable when the economy is sovereign and at most a share $\\tau$ of its ",
         "index weight rests on imputed components ($^{\\ast}$ the headline, \\texttt{config/rankability.yml}). Cells: ",
         "rankable headline cells (of ", fmt_n(es_m$scored_cells), " scored $\\ES$ and ", fmt_n(eo_m$scored_cells),
         " scored $\\EO$ cells). $\\rho_{\\text{none}}$ and $\\rho_{\\text{zero}}$: median across technology--stage ",
         "pairs of the within-pair Spearman correlation, over the cells rankable at $\\tau$, between the baseline ",
         "pillar and the pillar rebuilt with every global-average rule removed (available case) or replaced by zero. ",
         "Strategic Index: cells and countries in the sample when the pillars' rankable flags are taken at $\\tau$ ",
         "(the Policy index is unchanged), and the number of coalition members whose allied pick is the baseline one, ",
         "over the members with a pick at $\\tau$. PSI: partners ranked in the core-coalition (headline) and world ",
         "views. Within a technology--stage pair the order of two cells rankable under two thresholds is the same, ",
         "because the rule filters cells and does not change values. Source: \\texttt{scripts/paper/fig\\_robustness.R} ",
         "(\\texttt{rankability} in \\texttt{paper/results/robustness.json})."),
  "  \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}"), "robustness_rankability")

# --- Allied picks by sector under each variant ---------------------------------------------------
# Panel A: specification and policy variants; panel B: data variants (pick stability).
variant_picks$baseline <- base_top
pick_panels <- list(
  A = c(baseline = "Base-\\\\line", security_orientation = "$\\ES$ for\\\\$1-\\ES$",
        no_sector_priors = "No\\\\priors", all_ranked = "All\\\\ranked", no_policy = "No\\\\policy",
        equal_weights = "Equal\\\\weights", require_all = "CAT\\\\required", pams_only = "PAMS\\\\only",
        nipo_added = "NIPO\\\\added"),
  B = c(baseline = "Base-\\\\line", screen_off = "Screen\\\\off", eia_zero = "Zero for\\\\EIA",
        no_new_baskets = "No new\\\\baskets", trade_atlas = "Trade on\\\\Atlas",
        drop_Trade = "No\\\\Trade", drop_Reserves = "No\\\\Reserves",
        vintage_2025 = "2025\\\\vintage", vintage_2020 = "2020\\\\vintage")
)
pick_count_tbl <- function(vars) {
  pc <- dplyr::bind_rows(lapply(vars, function(v) tibble::tibble(variant = v, sector = variant_picks[[v]]$sector))) %>%
    dplyr::count(.data$variant, .data$sector) %>%
    tidyr::pivot_wider(names_from = "variant", values_from = "n", values_fill = 0)
  pc <- pc[, c("sector", vars)]
  pc$stage <- sub("^.* - ", "", pc$sector)
  pc %>% dplyr::arrange(factor(.data$stage, levels = STAGES), dplyr::desc(.data$baseline), .data$sector)
}
same_of <- function(v) if (v == "baseline") nrow(base_top) else agree_top(variant_picks[[v]])$n_same
panel_lines <- function(vars, title) {
  pc <- pick_count_tbl(names(vars))
  c(sprintf("    \\multicolumn{%d}{l}{\\emph{%s}}\\\\", length(vars) + 1, title),
    paste0("    Sector & ", paste(sprintf("\\makecell{%s}", unname(vars)), collapse = " & "), " \\\\"),
    "    \\midrule",
    vapply(seq_len(nrow(pc)), function(i) paste0("    ", sub(" - ", " ", pc$sector[i]), " & ",
                                                 paste(unlist(pc[i, names(vars)]), collapse = " & "), " \\\\"),
           character(1)),
    "    \\midrule",
    paste0("    Members with a pick & ", paste(vapply(names(vars), function(v) sprintf("%d", nrow(variant_picks[[v]])),
                                                    character(1)), collapse = " & "), " \\\\"),
    paste0("    Same pick as baseline & ", paste(vapply(names(vars), function(v) sprintf("%d", same_of(v)),
                                                      character(1)), collapse = " & "), " \\\\"))
}
results$si$pick_counts_by_variant <- lapply(pick_panels, function(v) pick_count_tbl(names(v)))
n_cols_b <- max(lengths(pick_panels))
write_tab(c(
  "\\begin{table}[tbp]", "  \\centering", "  \\begin{threeparttable}",
  "  \\caption{Allied picks by sector under the baseline and the variants of the Strategic Index}",
  "  \\label{tab:robustness_picks}", "  \\footnotesize", "  \\setlength{\\tabcolsep}{4pt}",
  paste0("  \\begin{tabular}{l", strrep("r", n_cols_b), "}"), "    \\toprule",
  panel_lines(pick_panels$A, "Panel A. Specification and Policy index"),
  "    \\addlinespace", "    \\midrule",
  panel_lines(pick_panels$B, "Panel B. Data: endowment screen, final data choices, trade and reserves inputs, vintage"),
  "    \\bottomrule", "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\scriptsize",
  paste0("    \\item \\emph{Notes:} Number of coalition members whose allied pick, the Upstream or Midstream sector ",
         "(excluding Geothermal and Green Hydrogen, and Upstream cells without a recorded resource base) with ",
         "the highest Strategic Index, falls in each sector. All ", nrow(allies), " members have a pick in the ",
         "baseline. Panel A: $\\ES$ for $1-\\ES$, security in place of need; No priors, without the stage and ",
         "technology terms; All ranked, $\\EO$ and $1-\\ES$ S-curved within country; No policy, without the policy ",
         "term; Equal weights, 0.2 on each term; CAT required, both Policy components required ",
         "(\\texttt{OPSI\\_POLICY\\_REQUIRE\\_ALL=1}), which leaves ", results$policy$require_all$n_eligible,
         " countries with a Policy index and ", results$policy$require_all$members_with_pick,
         " members with a pick; PAMS only, CAT dropped for every country; NIPO added, the October 2026 NIPO ",
         "component (GTA NIPO as of 5 October 2026) re-enters the Policy index with weight 4 (IEA PAMS 3, CAT 2), ",
         "IEA PAMS still required, for its ", policy_nipo_added$nipo_countries_policy_eligible,
         " policy-eligible jurisdictions. Panel B: Screen off, every Upstream cell eligible, ",
         "including the ", screen_detail$scored_upstream_cells_no_resource_base, " of ",
         screen_detail$scored_upstream_cells, " scored Upstream cells without recorded reserves or production; ",
         "Zero for EIA, the oil, gas and coal production that the Energy Institute does not report set to zero ",
         "instead of the EIA value (the rule until 11 October 2026); No new baskets, the Trade category removed from ",
         "Electric Vehicles, Wind, Nuclear and Solar Upstream, whose trade baskets were added on 11 October 2026; ",
         "Trade on Atlas, the Trade category of both pillars replaced by its Atlas-based counterpart of the 2025 ",
         "vintage; No Trade, No Reserves, the category dropped from the pillars that score it and ",
         "the remaining categories renormalised; 2025 and 2020 vintage, both pillars taken from the vintage builds, ",
         "in which only the time-varying sources differ. The Policy index is the headline one in Panel B. ",
         "Source: \\texttt{index\\_outputs.rds}, \\texttt{data/processed/vintages}; ",
         "\\texttt{scripts/paper/fig\\_robustness.R}."),
  "  \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}"), "robustness_picks")

# --- Data and policy choices of the final build ---------------------------------------------------
dc <- results$data_choices
med_rho <- function(bp) if (is.null(bp) || nrow(bp) == 0) NA_real_ else stats::median(bp$rho, na.rm = TRUE)
pick_cell <- function(same, n) sprintf("%d/%d", same, n)
row_dc <- function(label, es_rk, eo_rk, es_rho, eo_rho, si_cells, si_ctry, same, n) {
  sprintf("    %s & %s & %s & %s & %s & %s & %s & %s \\\\", label, es_rk, eo_rk, es_rho, eo_rho, si_cells, si_ctry,
          pick_cell(same, n))
}
es_rk0 <- fmt_n(es_m$rankable_cells); eo_rk0 <- fmt_n(eo_m$rankable_cells)
pol_row <- function(label, v, n_cells = NULL, n_ctry = NULL) {
  row_dc(label, "=", "=", "--", "--", fmt_n(if (is.null(n_cells)) v$si_cells else n_cells),
         if (is.null(n_ctry)) v$si_countries else n_ctry, v$picks_same, v$members_with_pick)
}
cf <- dc$cmfdi_off$both_off$coalition
write_tab(c(
  "\\begin{table}[tbp]", "  \\centering", "  \\begin{threeparttable}",
  "  \\caption{Data and policy choices of the final build: rankable cells, rankings and allied picks}",
  "  \\label{tab:robustness_data}", "  \\footnotesize", "  \\setlength{\\tabcolsep}{4pt}",
  "  \\begin{tabular}{lccccccc}", "    \\toprule",
  "    & \\multicolumn{2}{c}{Rankable cells} & \\multicolumn{2}{c}{Median $\\rho$, touched pairs} & \\multicolumn{3}{c}{Strategic Index} \\\\",
  "    \\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-8}",
  "    Variant & $\\ES$ & $\\EO$ & $\\ES$ & $\\EO$ & Cells & Countries & Same pick \\\\", "    \\midrule",
  row_dc("Baseline", es_rk0, eo_rk0, "--", "--", fmt_n(nrow(si_tbl)), length(countries_si), nrow(base_top), nrow(base_top)),
  "    \\multicolumn{8}{l}{\\emph{Data}}\\\\",
  row_dc("\\quad Zero in place of the EIA fill", fmt_n(dc$eia_fill$rankable$es$rankable), fmt_n(dc$eia_fill$rankable$eo$rankable),
         d2(med_rho(dc$eia_fill$affected_cells$es$by_pair)), d2(med_rho(dc$eia_fill$affected_cells$eo$by_pair)),
         fmt_n(dc$eia_fill$si_cells), dc$eia_fill$si_countries, dc$eia_fill$picks_same, dc$eia_fill$members_with_pick),
  row_dc("\\quad New Upstream baskets removed", fmt_n(dc$upstream_baskets$rankable$es$rankable),
         fmt_n(dc$upstream_baskets$rankable$eo$rankable),
         d2(med_rho(dc$upstream_baskets$affected_cells$es$by_pair)), d2(med_rho(dc$upstream_baskets$affected_cells$eo$by_pair)),
         fmt_n(dc$upstream_baskets$si_cells), dc$upstream_baskets$si_countries, dc$upstream_baskets$picks_same,
         dc$upstream_baskets$members_with_pick),
  "    \\multicolumn{8}{l}{\\emph{Policy index}}\\\\",
  pol_row("\\quad Both components required", results$policy$require_all),
  pol_row("\\quad IEA PAMS alone", results$policy$pams_only),
  row_dc("\\quad October 2026 NIPO added back", "=", "=", "--", "--", fmt_n(nrow(si_tbl)), length(countries_si),
         si_variants$nipo_added$allied_top_sector_same, si_variants$nipo_added$allied_members_with_pick),
  "    \\multicolumn{8}{l}{\\emph{Allied pick rule}}\\\\",
  row_dc("\\quad Endowment screen off", "=", "=", "--", "--", fmt_n(nrow(si_tbl)), length(countries_si),
         si_variants$screen_off$allied_top_sector_same, si_variants$screen_off$allied_members_with_pick),
  "    \\bottomrule", "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} Rankable cells: headline cells of sovereign economies with at most half of the ",
         "index weight imputed, after the variant (= unchanged). Median $\\rho$: median across the touched ",
         "technology--stage pairs (Oil, Gas and Coal Upstream for the EIA fill; Electric Vehicles, Wind, Nuclear and ",
         "Solar Upstream for the baskets) of the within-pair Spearman correlation between the baseline and the ",
         "variant over the baseline's rankable cells. Strategic Index: scored cells and countries, and the coalition ",
         "members whose allied pick is the baseline one over the members with a pick. Zero in place of the EIA fill: ",
         "the oil, gas and coal production that the Energy Institute does not report (", dc$eia_fill$eia_sourced_rows_headline$import_dependence,
         " EIA-sourced rows of the import-dependence theme and ", dc$eia_fill$eia_sourced_rows_headline$production_depth_momentum,
         " of the production theme in the headline) set to zero, the rule until 11 October 2026, both themes rebuilt ",
         "with the pipeline's builders. New Upstream baskets removed: the Trade category of the four Upstream cells ",
         "whose trade baskets were added on 11 October 2026 dropped and both pillars rebuilt; the uranium codes moved ",
         "out of Nuclear Midstream and the rare-earth codes moved out of EV and Wind Midstream are not restored. ",
         "Policy rows as in Table~\\ref{tab:robustness_picks}. The partnership side is not rebuilt: in the CM-FDI ",
         "integration study (inputs of 11 October 2026), switching off both CM-FDI parts of Friendshore gives a ",
         "core-view PSI with a Spearman correlation of ", sprintf("%.4f", cf$spearman), " with the CM-FDI build over ",
         cf$ranked_both, " ranked partners, a largest rank change of ", cf$max_abs_rank_change, " places and ",
         cf$top15_overlap, " of the top ", PSI_TOP, " in common. Source: \\texttt{scripts/paper/fig\\_robustness.R} ",
         "(\\texttt{data\\_choices} and \\texttt{policy} in \\texttt{paper/results/robustness.json})."),
  "  \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}"), "robustness_data")

# --- Vertical complementarities (appendix) ---------------------------------------------------------
vt <- results$vertical
d3 <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
pct_rng <- function(x, d = 3) sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), x$p50, x$p05, x$p95)
int_rng <- function(x) sprintf("%.0f [%.0f, %.0f]", x$p50, x$p05, x$p95)
cells_txt <- function(x) paste(sprintf("%s %s", names(x), vapply(x, fmt_n, character(1))), collapse = "; ")
write_tab(c(
  "\\begin{table}[tbp]", "  \\centering", "  \\begin{threeparttable}",
  "  \\caption{Vertical complementarities: sensitivity of the pillars and the allied picks to an adjacent-stage term}",
  "  \\label{tab:robustness_vertical}", "  \\footnotesize", "  \\setlength{\\tabcolsep}{4pt}",
  "  \\begin{tabular}{llccc}", "    \\toprule",
  "    Term & Weight & \\makecell{Median within-\\\\pair $\\rho$} & \\makecell{Minimum\\\\$\\rho$} & \\makecell{Picks\\\\changed} \\\\", "    \\midrule",
  sprintf("    $\\EO$ linkage, within-country deviation & Mid %s, Up %s & %s & %s & %d \\\\",
          sprintf("%.2f", vt$eo_vcl$weights$Midstream), sprintf("%.2f", vt$eo_vcl$weights$Upstream),
          d3(vt$eo_vcl$within_pair_spearman_all$median), d3(vt$eo_vcl$within_pair_spearman_all$min), vt$eo_vcl$picks_changed),
  sprintf("    \\quad Placebo: scores permuted within cell & same & %s & -- & %s \\\\",
          pct_rng(vt$eo_vcl$placebo_permuted$within_pair_rho_median), int_rng(vt$eo_vcl$placebo_permuted$picks_changed)),
  "    \\addlinespace",
  sprintf("    $\\ES$ input security, sourcing-weighted & %.0f\\%% share & %s & %s & %d \\\\", 100 * vt$es_is$share,
          d3(vt$es_is$within_pair_spearman_all$median), d3(vt$es_is$within_pair_spearman_all$min), vt$es_is$picks_changed),
  sprintf("    \\quad Placebo: constant 0.5 & same & %s & %s & %d \\\\",
          d3(vt$es_is$placebo_constant$within_pair_spearman$median), d3(vt$es_is$placebo_constant$within_pair_spearman$min),
          vt$es_is$placebo_constant$picks_changed),
  sprintf("    \\quad Placebo: scores permuted within cell & same & %s & -- & %s \\\\",
          pct_rng(vt$es_is$placebo_permuted$within_pair_rho_median), int_rng(vt$es_is$placebo_permuted$picks_changed)),
  "    \\bottomrule", "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} $\\EO$ linkage: a category added to the Midstream and Upstream $\\EO$ cells, the ",
         "S-curved world market share of the adjacent stage (Atlas of Economic Complexity, HS92 baskets, ", vt$eo_vcl$atlas_year,
         ") minus the economy's mean of that measure over the ", length(vt$eo_vcl$technologies), " basketed headline ",
         "technologies, S-curved within the cell (", cells_txt(vt$eo_vcl$cells_with_term), " cells). Weights in $\\EO$ ",
         "weight points (the nominal category weights are 2 to 4): the evidence weights of the vertical-complementarity study, ",
         "0.6 times the ratio of the adjacent-stage to the feasibility coefficient in the five-year market-share regression, ",
         "upper bounds because about a third of the coefficient is absorbed by lags of own capability. $\\ES$ input security: ",
         "the domestic supply share times the economy's own supply security plus the import-share-weighted security of its ",
         "suppliers (half the supplier's $\\ES$ in the input stage, half the dyadic alignment), 2024 sourcing, S-curved within ",
         "the cell and given 15 per cent of the Midstream and Downstream cells' weight (", cells_txt(vt$es_is$cells_with_term),
         " cells). Median and minimum $\\rho$: within-pair Spearman correlation with the baseline pillar over the baseline's ",
         "rankable cells of the receiving stages; for the permutation placebo, the median and the 5th and 95th percentiles ",
         "over ", N_PLACEBO, " draws of the per-draw median. Picks changed: coalition members (of ", nrow(base_top),
         ") whose allied pick differs from the baseline when the Strategic Index takes the modified pillar ",
         "(for the placebo, median and 5th--95th percentiles over the draws). Neither term enters the headline indices. ",
         "Source: \\texttt{scripts/paper/fig\\_robustness.R} (\\texttt{vertical} in \\texttt{paper/results/robustness.json}), ",
         "from the study's inputs."),
  "  \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}"), "robustness_vertical")
save_results()

# ===========================================================================
# Figures
# ===========================================================================

stage_f <- function(x) factor(x, levels = STAGES)
pillar_levels <- c("Energy Security", "Economic Opportunity")
unit_labels <- function(x) sub("\\.?0+$", "", formatC(x, format = "f", digits = 2))

# --- Figure: vintage scatter ---------------------------------------------------
set.seed(SEED + 2L)
vp <- dplyr::bind_rows(vintage_points) %>%
  dplyr::mutate(pillar = factor(.data$pillar, levels = pillar_levels),
                supply_chain = stage_f(.data$supply_chain))
vp <- vp[sample.int(nrow(vp)), ]  # no stage systematically drawn on top
fig_vintage <- ggplot(vp, aes(.data$index_2020, .data$index_2025, colour = .data$supply_chain)) +
  geom_abline(slope = 1, intercept = 0, colour = paper_ink$secondary, linewidth = 0.3) +
  geom_point(size = 0.6, alpha = 0.5, stroke = 0) +
  facet_wrap(~pillar, nrow = 1) +
  scale_colour_stage(name = NULL, guide = guide_legend(override.aes = list(size = 2.2, alpha = 1))) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = unit_labels,
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = unit_labels,
                     expand = expansion(mult = 0.02)) +
  coord_equal() +
  labs(x = "Index, 2020 vintage", y = "Index, 2025 vintage") +
  theme_paper(grid = "xy") +
  theme(panel.spacing.x = unit(2, "lines"), legend.margin = margin(t = -2))
save_paper_fig(fig_vintage, "robustness_vintage", width = paper_width_full, height = 3.45)

# --- Figure: variants dot plot ---------------------------------------------------
# One row per variant; the dot is the median over technology-stage pairs of the within-pair
# Spearman correlation with the baseline, the thick bar the interquartile range and the thin
# line the range. For the weight draws the per-pair value is the median over the draws.
variant_rows <- function(p) {
  pl <- pillars[[p]]
  det <- c(
    setNames(lapply(gamma_res, function(v) v[[p]]$by_pair), names(gamma_res)),
    list(impute_none = imp_res$none[[p]]$by_pair, impute_zero = imp_res$zero[[p]]$by_pair)
  )
  d <- dplyr::bind_rows(lapply(names(det), function(nm) det[[nm]] %>% dplyr::mutate(variant = nm)))
  mc <- mc_pair_rows[[p]] %>% dplyr::transmute(pair = .data$pair, rho = .data$rho_median_over_draws,
                                               variant = "weights_mc")
  eq <- equal_rows[[p]] %>% dplyr::transmute(pair = .data$pair, rho = .data$rho, variant = "weights_equal")
  vt <- vintage[[p]]$within_pair_spearman_by_pair %>%
    dplyr::transmute(pair = .data$pair, rho = .data$rho, variant = "vintage")
  dplyr::bind_rows(d, mc, eq, vt) %>% dplyr::mutate(pillar = pl$label)
}
variant_order <- c("weights_mc", "weights_equal", "gamma_0.25", "gamma_1", "gamma_2", "impute_none",
                   "impute_zero", "vintage")
# plotmath labels: the pdf() device cannot be relied on to carry a Unicode gamma.
variant_labels <- c(
  "Category weights, Dirichlet draws",
  "Equal category weights",
  expression("S-curve" ~ gamma == 0.25),
  expression("S-curve" ~ gamma == 1 ~ "(percent rank)"),
  expression("S-curve" ~ gamma == 2),
  "No imputation (available case)",
  "Zero in place of global average",
  "2020 vs 2025 vintage"
)
vr <- dplyr::bind_rows(variant_rows("es"), variant_rows("eo")) %>%
  dplyr::mutate(pillar = factor(.data$pillar, levels = pillar_levels))
vs <- vr %>% dplyr::group_by(.data$pillar, .data$variant) %>%
  dplyr::summarise(med = stats::median(.data$rho, na.rm = TRUE), lo = min(.data$rho, na.rm = TRUE),
                   hi = max(.data$rho, na.rm = TRUE), p25 = q(.data$rho, 0.25), p75 = q(.data$rho, 0.75),
                   n_pairs = sum(!is.na(.data$rho)), .groups = "drop") %>%
  dplyr::mutate(variant = factor(.data$variant, levels = rev(variant_order)))
results$figure_data <- list(variants = vs)
x_lo <- floor(min(vs$lo) * 4) / 4
interval_dots <- function(y) {
  list(
    geom_segment(aes(x = .data$lo, xend = .data$hi, y = .data[[y]], yend = .data[[y]]), linewidth = 0.35),
    geom_segment(aes(x = .data$p25, xend = .data$p75, y = .data[[y]], yend = .data[[y]]), linewidth = 1.8),
    geom_point(aes(x = .data$med, y = .data[[y]]), size = 2.1, shape = 21, fill = "white", stroke = 0.8)
  )
}
fig_variants <- ggplot(vs, aes(colour = .data$pillar)) +
  interval_dots("variant") +
  facet_wrap(~pillar, nrow = 1) +
  scale_colour_manual(values = paper_pillar_cols, guide = "none") +
  scale_x_continuous(limits = c(x_lo, 1), breaks = seq(x_lo, 1, 0.25), labels = unit_labels,
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_discrete(breaks = rev(variant_order), labels = rev(variant_labels)) +
  labs(x = "Spearman correlation with the baseline within technology-stage pairs", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(1.5, "lines"))
save_paper_fig(fig_variants, "robustness_variants", width = paper_width_full, height = 3.0)

# --- Figure: leave one category out ------------------------------------------
lr <- dplyr::bind_rows(loo_rows) %>%
  dplyr::filter(.data$n_pairs > 0) %>%
  dplyr::mutate(pillar = factor(.data$pillar, levels = pillar_levels),
                lab = sprintf("%s (%.0f%%, %d)", .data$category, 100 * .data$weight_share,
                              .data$pairs_scored),
                med = .data$median, lo = .data$min, hi = .data$max) %>%
  dplyr::arrange(.data$pillar, dplyr::desc(.data$median), dplyr::desc(.data$p25)) %>%
  dplyr::mutate(row = factor(paste(.data$pillar, .data$lab, sep = "|"),
                             levels = paste(.data$pillar, .data$lab, sep = "|")))
x_lo_loo <- floor(min(lr$lo, na.rm = TRUE) * 4) / 4
fig_loo <- ggplot(lr, aes(colour = .data$pillar)) +
  interval_dots("row") +
  facet_wrap(~pillar, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = paper_pillar_cols, guide = "none") +
  scale_x_continuous(limits = c(x_lo_loo, 1), breaks = seq(x_lo_loo, 1, 0.25), labels = unit_labels,
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_discrete(labels = function(x) sub("^.*\\|", "", x)) +
  labs(x = "Spearman correlation with the baseline within technology-stage pairs", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(1.5, "lines"))
save_paper_fig(fig_loo, "robustness_loo", width = paper_width_full, height = 3.0)

# --- Figure: rank bands of core members in one pair ----------------------------------
band_pair <- "Batteries - Midstream"
rbp <- rank_bands %>% dplyr::filter(.data$pair == band_pair, .data$Country %in% core$Country) %>%
  dplyr::left_join(allies %>% dplyr::select("Country", "iso3"), by = "Country") %>%
  dplyr::mutate(lo_pct = 100 * (.data$n_pair - .data$q95) / (.data$n_pair - 1),
                hi_pct = 100 * (.data$n_pair - .data$q05) / (.data$n_pair - 1),
                pillar = factor(.data$pillar, levels = pillar_levels))
ord <- rbp %>% dplyr::group_by(.data$iso3) %>%
  dplyr::summarise(o = if (any(.data$pillar == "Economic Opportunity"))
    .data$base_pct[.data$pillar == "Economic Opportunity"][1] else -1, .groups = "drop") %>%
  dplyr::arrange(.data$o)
rbp$iso3 <- factor(rbp$iso3, levels = ord$iso3)
results$figure_data$rank_bands_pair <- band_pair
results$figure_data$rank_bands <- rbp %>%
  dplyr::select("pillar", "iso3", "n_pair", "base_rank", "q05", "q95", "base_pct", "lo_pct", "hi_pct")
results$figure_data$rank_bands_core_members_shown <- rbp %>% dplyr::count(.data$pillar)
fig_ranks <- ggplot(rbp, aes(y = .data$iso3, colour = .data$pillar)) +
  geom_segment(aes(x = .data$lo_pct, xend = .data$hi_pct, yend = .data$iso3), linewidth = 1.5, alpha = 0.45) +
  geom_point(aes(x = .data$base_pct), size = 1.5) +
  facet_wrap(~pillar, nrow = 1) +
  scale_colour_manual(values = paper_pillar_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = 0.02)) +
  labs(x = "Percentile position among the pair's rankable cells (100 = highest index)", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(1.5, "lines"), axis.text.y = element_text(size = 6.5))
save_paper_fig(fig_ranks, "robustness_ranks", width = paper_width_full, height = 4.4)

# --- Figure: SI allied picks under weight draws and variants -----------------
rt <- retain_tbl %>% dplyr::arrange(.data$share_draws_baseline_top, dplyr::desc(.data$iso3)) %>%
  dplyr::mutate(stage = stage_f(sub("^.* - ", "", .data$sector)),
                lab = sprintf("%s  %s  %s", .data$iso3, sector_short(.data$sector),
                              ifelse(is.na(.data$margin), "(--)", sprintf("(%.3f)", .data$margin))),
                lab = factor(.data$lab, levels = .data$lab))
panel_si_a <- ggplot(rt, aes(x = .data$share_draws_baseline_top, y = .data$lab)) +
  geom_segment(aes(x = 0, xend = .data$share_draws_baseline_top, yend = .data$lab),
               colour = paper_ink$axis, linewidth = 0.35) +
  geom_point(aes(colour = .data$stage), size = 1.6) +
  scale_colour_stage(name = NULL, drop = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = scales::percent,
                     expand = expansion(mult = c(0, 0.06))) +
  labs(title = "(a) Weight draws; margin over runner-up in brackets",
       x = "Share of weight draws\nkeeping the baseline pick", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 6.5), plot.title = element_text(size = 8.5, face = "bold"),
        legend.position = "bottom", legend.text = element_text(size = 7), legend.title = element_text(size = 7))
variant_short <- c(security_orientation = "ES for\n1 - ES", no_sector_priors = "No\npriors",
                   all_ranked = "All\nranked", no_policy = "No\npolicy", require_all = "CAT\nrequired",
                   pams_only = "PAMS\nonly", nipo_added = "NIPO\nadded", screen_off = "Screen\noff",
                   trade_atlas = "Trade on\nAtlas")
status_levels <- c("Same pick as baseline", "Different pick", "No pick")
agree <- dplyr::bind_rows(lapply(names(variant_short), function(v) {
  dplyr::left_join(base_top, variant_picks[[v]], by = "Country") %>%
    dplyr::transmute(Country = .data$Country, variant = v,
                     status = dplyr::case_when(is.na(.data$sector.y) ~ "No pick",
                                               .data$sector.x == .data$sector.y ~ "Same pick as baseline",
                                               TRUE ~ "Different pick"))
})) %>%
  dplyr::left_join(rt %>% dplyr::select("Country", "lab"), by = "Country") %>%
  dplyr::mutate(variant = factor(.data$variant, levels = names(variant_short)),
                status = factor(.data$status, levels = status_levels))
n_same_by <- vapply(names(variant_short), function(x) sum(agree$status[agree$variant == x] == status_levels[1]),
                    integer(1))
results$figure_data$si_variant_agreement <- as.list(n_same_by)
panel_si_b <- ggplot(agree, aes(x = .data$variant, y = .data$lab)) +
  geom_point(aes(fill = .data$status, shape = .data$status), size = 1.6, colour = paper_ink$primary,
             stroke = 0.45) +
  scale_fill_manual(values = c("Same pick as baseline" = paper_ink$primary, "Different pick" = "white",
                               "No pick" = paper_ink$axis), name = NULL, drop = FALSE) +
  scale_shape_manual(values = c("Same pick as baseline" = 21, "Different pick" = 21, "No pick" = 4),
                     name = NULL, drop = FALSE) +
  scale_x_discrete(labels = function(v) sprintf("%s\n%d/%d", variant_short[v], n_same_by[v], nrow(rt))) +
  labs(title = "(b) Variants", x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.text.y = element_blank(), axis.line.x = element_blank(),
        axis.text.x = element_text(size = 6.2, lineheight = 0.9), legend.position = "bottom",
        legend.direction = "horizontal", legend.text = element_text(size = 7),
        plot.title = element_text(size = 8.5, face = "bold"))
fig_si <- cowplot::plot_grid(panel_si_a, panel_si_b, nrow = 1, align = "h", axis = "tb",
                             rel_widths = c(2.25, 2.5))
save_paper_fig(fig_si, "robustness_si", width = paper_width_full, height = 8.2)

# --- Figure: picks when the pillars' inputs change --------------------------------------------
# One row per variant: members (of the coalition) whose pick is the baseline one. The rows for the
# inputs the final data fixes revised (trade, reserves, the EIA fill, the Upstream baskets) are
# highlighted.
revised <- c("Trade", "Reserves")
pd <- dplyr::bind_rows(
  loo_pick_tbl %>% dplyr::transmute(
    group = "Category dropped from the pillars",
    lab = sprintf("%s (%s)", .data$category, .data$pillars),
    same = .data$picks_same, with_pick = .data$members_with_pick,
    hl = .data$category %in% revised),
  tibble::tibble(
    group = "Alternative inputs",
    lab = c("Zero in place of the EIA production fill", "New Upstream trade baskets removed",
            "Trade category on the Atlas basis (ES, EO)", "Pillars of the 2025 vintage", "Pillars of the 2020 vintage"),
    same = c(results$data_choices$eia_fill$picks_same, results$data_choices$upstream_baskets$picks_same,
             data_variants$trade_atlas$picks_same, data_variants$vintage_2025$picks_same,
             data_variants$vintage_2020$picks_same),
    with_pick = c(results$data_choices$eia_fill$members_with_pick, results$data_choices$upstream_baskets$members_with_pick,
                  data_variants$trade_atlas$members_with_pick, data_variants$vintage_2025$members_with_pick,
                  data_variants$vintage_2020$members_with_pick),
    hl = c(TRUE, TRUE, TRUE, FALSE, FALSE))
) %>%
  dplyr::mutate(group = factor(.data$group, levels = c("Category dropped from the pillars", "Alternative inputs")),
                hl = factor(ifelse(.data$hl, "Input revised in the final data fixes", "Other input"),
                            levels = c("Input revised in the final data fixes", "Other input"))) %>%
  dplyr::arrange(.data$group, .data$same) %>%
  dplyr::mutate(lab = factor(.data$lab, levels = rev(unique(.data$lab))))
results$figure_data$picks_data <- pd
n_all <- nrow(allies)
fig_pd <- ggplot(pd, aes(y = .data$lab)) +
  geom_vline(xintercept = n_all, colour = paper_ink$axis, linewidth = 0.3) +
  geom_segment(aes(x = 0, xend = .data$same, yend = .data$lab), colour = paper_ink$axis, linewidth = 0.35) +
  geom_point(aes(x = .data$same, colour = .data$hl), size = 2) +
  facet_grid(rows = vars(.data$group), scales = "free_y", space = "free_y", labeller = label_wrap_gen(14)) +
  scale_colour_manual(values = c("Input revised in the final data fixes" = paper_cat[["blue"]],
                                 "Other input" = paper_ink$muted),
                      name = NULL) +
  scale_x_continuous(limits = c(0, n_all), breaks = seq(0, 50, 10), expand = expansion(mult = c(0, 0.03))) +
  labs(x = sprintf("Coalition members keeping their baseline pick (of %d)", n_all), y = NULL) +
  theme_paper(grid = "x") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 7.5), axis.text.y = element_text(size = 7.5))
save_paper_fig(fig_pd, "robustness_picks_data", width = paper_width_full, height = 3.9)

# --- Figure: PSI viewpoints -------------------------------------------------------
pct_rank <- function(v, keep) {
  out <- rep(NA_real_, length(v))
  r <- rank(-v[keep], ties.method = "average")
  out[keep] <- 100 * (sum(keep) - r) / (sum(keep) - 1)
  out
}
psi_long <- dplyr::bind_rows(lapply(list(
  list(alt = "full_coalition", rk = "full_rk",
       lab = sprintf("Full coalition, %d reporters\nSpearman %s; top-%d overlap %d",
                     results$psi_views$reporters$full_coalition, f2(results$psi_views$core_vs_full_coalition$spearman),
                     PSI_TOP, results$psi_views$core_vs_full_coalition$top15_overlap)),
  list(alt = "world", rk = "world_rk",
       lab = sprintf("World, %d reporters\nSpearman %s; top-%d overlap %d",
                     results$psi_views$reporters$world, f2(results$psi_views$core_vs_world$spearman),
                     PSI_TOP, results$psi_views$core_vs_world$top15_overlap))), function(a) {
  both <- psi_cmp$core_rk & psi_cmp[[a$rk]]
  tibble::tibble(partner_iso = psi_cmp$partner_iso[both],
                 x = pct_rank(psi_cmp$core, both)[both], y = pct_rank(psi_cmp[[a$alt]], both)[both],
                 top = psi_cmp$core_rank[both] <= PSI_TOP, panel = a$lab)
})) %>% dplyr::mutate(panel = factor(.data$panel, levels = unique(.data$panel)),
                      grp = ifelse(.data$top, "Core-view top 15", "Other ranked partners"))
lab_pts <- psi_long %>% dplyr::filter(.data$top) %>% dplyr::group_by(.data$panel) %>%
  dplyr::slice_min(.data$y, n = 3, with_ties = FALSE) %>% dplyr::ungroup()
fig_psi <- ggplot(psi_long, aes(.data$x, .data$y)) +
  geom_abline(slope = 1, intercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_point(data = psi_long %>% dplyr::filter(!.data$top), aes(colour = .data$grp), size = 0.9, alpha = 0.8) +
  geom_point(data = psi_long %>% dplyr::filter(.data$top), aes(colour = .data$grp), size = 1.5) +
  ggrepel::geom_text_repel(data = lab_pts, aes(label = .data$partner_iso), size = 2.5, nudge_x = -14, nudge_y = -4,
                           colour = paper_ink$primary, min.segment.length = 0, segment.size = 0.25,
                           segment.colour = paper_ink$muted, box.padding = 0.3, seed = 1) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(values = c("Core-view top 15" = paper_cat[["blue"]],
                                 "Other ranked partners" = paper_ink$axis), name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = 0.02)) +
  coord_equal() +
  labs(x = "Percentile position, core-coalition view (headline)", y = "Percentile position, alternative view") +
  theme_paper(grid = "xy") +
  theme(panel.spacing.x = unit(2, "lines"), strip.text = element_text(size = 7.5))
save_paper_fig(fig_psi, "robustness_psi_views", width = paper_width_full, height = 3.6)
results$figure_data$psi_labelled <- lab_pts

save_results()
message("figures done")
