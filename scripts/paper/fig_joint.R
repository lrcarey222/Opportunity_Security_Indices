# Section 7, the opportunity-security space: figures and the numbers quoted in the text.
#
#   joint_scatter        ES vs EO within-pair percentile ranks, faceted by stage, coalition
#                        members highlighted by tier (core, partner), within-pair medians
#                        splitting the quadrants
#   joint_corr           average within-pair Spearman correlation among the ES and EO
#                        categories (lower triangle), diverging around zero
#   joint_quadrants      shares of cells in each quadrant of the space by technology-stage pair
#   joint_members        shares of each coalition member's sectors in each quadrant, by tier
#
# Writes paper/figures/joint_*.pdf (+ png/), paper/results/joint.json and the stage summary
# table paper/tables/joint_stage.tex (generated here so its numbers cannot drift from the JSON).
# Run from the repository root:  Rscript scripts/paper/fig_joint.R
#
# Sample. The headline technologies of the Energy Security (ES) and Economic Opportunity (EO)
# pillars, joined on Country x tech x supply_chain. Technology-stage pairs that either pillar's
# diagnostics flag as "constant" or "unscored" (index_cells) are dropped, so are pairs that one
# pillar does not score. Only RANKABLE cells enter (rankable == TRUE in both pillars: a
# sovereign economy with at most tau = 0.5 of the pillar's weight imputed, config/rankability.yml).
# Every comparison is made within technology-stage pairs (both pillars are rank-based within
# them): Spearman correlations are computed across countries within a pair, and quadrants are
# defined by the within-pair medians of ES and EO over the rankable cells. A cell is "high" on a
# pillar when its value is strictly above the within-pair median (ties at the median count as
# low).
#
# Coalition: io$coalition_members (two tiers, 31 core + 26 partners = 57 members, derived by
# scripts/13_build_coalition.R). The six EU members below the GDP floor that belong only to the
# EU network node are not coalition members here. The script reads no network output (the allied
# picks come from data/processed/charts/allied_strategic.csv, written by scripts/20).

source(file.path("scripts", "paper", "_paper_theme.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggrepel)
})
options(dplyr.summarise.inform = FALSE)

root <- paper_repo_root()
processed_path <- function(...) file.path(root, "data", "processed", ...)

keys <- c("Country", "tech", "supply_chain")
stage_levels <- c("Upstream", "Midstream", "Downstream")
quad_levels <- c(
  "Opportunity without security", "Leaders",
  "Security without opportunity", "Laggards"
)
# Within-country statistics (correlations and dispersions over a country's Strategic Index cells)
# need a minimum number of sectors to mean anything: countries with fewer are left out of them.
min_wc_cells <- 5

# ---------------------------------------------------------------------------------------------
# Local helpers (pure)
# ---------------------------------------------------------------------------------------------

pct_rank <- function(x) (rank(x, ties.method = "average") - 0.5) / length(x)

spearman <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3 || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  stats::cor(x[ok], y[ok], method = "spearman")
}

classify_quadrant <- function(es_hi, eo_hi) {
  factor(dplyr::case_when(
    eo_hi & !es_hi ~ "Opportunity without security",
    eo_hi & es_hi ~ "Leaders",
    !eo_hi & es_hi ~ "Security without opportunity",
    TRUE ~ "Laggards"
  ), levels = quad_levels)
}

# Off-diagonal quadrant share implied by a Gaussian copula with Spearman correlation rho_s
# (Greiner's relation r = 2 sin(pi rho_s / 6); P(X > m, Y < m) = 1/4 - asin(r) / (2 pi)).
implied_offdiag_share <- function(rho_s) {
  r <- 2 * sin(pi * rho_s / 6)
  0.25 - asin(r) / (2 * pi)
}

# Pillar value recomputed from a subset of its components: the weighted mean of the kept
# components, with the effective weights (category weight x within-category weight)
# renormalised to sum to one. The pillar is exactly this weighted mean over all components.
subset_index <- function(var_tbl, keep) {
  var_tbl %>%
    dplyr::filter(keep) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(value = sum(.data$contrib) / sum(.data$w), .groups = "drop")
}

short_tech <- c(
  "Batteries" = "Batteries", "Coal" = "Coal", "Electric Grid" = "Grid",
  "Electric Vehicles" = "EVs", "Gas" = "Gas", "Geothermal" = "Geothermal",
  "Green Hydrogen" = "Green H2", "Nuclear" = "Nuclear", "Oil" = "Oil",
  "Solar" = "Solar", "Wind" = "Wind"
)

r3 <- function(x) round(x, 3)

# ---------------------------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------------------------

io <- readRDS(processed_path("outputs", "index_outputs.rds"))

es_idx_full <- io$energy_security_index %>%
  dplyr::select(dplyr::all_of(keys), es = "Energy_Security_Index",
                es_rankable = "rankable", es_rank_exclusion = "rank_exclusion")
eo_idx_full <- io$economic_opportunity_index %>%
  dplyr::select(dplyr::all_of(keys), eo = "Economic_Opportunity_Index",
                eo_rankable = "rankable", eo_rank_exclusion = "rank_exclusion")
es_idx <- es_idx_full %>% dplyr::select(dplyr::all_of(keys), "es")
eo_idx <- eo_idx_full %>% dplyr::select(dplyr::all_of(keys), "eo")
headline_techs <- sort(intersect(unique(es_idx$tech), unique(eo_idx$tech)))

cells_es <- io$energy_security_outputs$diagnostics$index_cells
cells_eo <- io$economic_opportunity_outputs$diagnostics$index_cells

var_cols <- function(v) {
  v %>%
    dplyr::filter(.data$tech %in% headline_techs) %>%
    dplyr::transmute(
      Country, tech, supply_chain, category, variable,
      x = component_value,
      w = category_weight * component_weight_within_overall,
      contrib = weighted_variable_contribution,
      imputed = imputed
    )
}
var_es <- var_cols(io$energy_security_variable_contributions)
var_eo <- var_cols(io$economic_opportunity_variable_contributions)

cat_cols <- function(v) {
  v %>%
    dplyr::filter(.data$tech %in% headline_techs) %>%
    dplyr::select(dplyr::all_of(keys), "category", "category_score")
}
cat_es <- cat_cols(io$energy_security_category_scores)
cat_eo <- cat_cols(io$economic_opportunity_category_scores)

si_tbl <- io$strategic_index %>%
  dplyr::select(dplyr::all_of(keys), "strategic_index", "si_exclusion", "pick_exclusion", "tech_weight")

coalition <- io$coalition_members %>%
  dplyr::transmute(iso3c = .data$iso3, Country = .data$country,
                   tier = factor(.data$tier, levels = c("core", "partner")))
stopifnot(nrow(coalition) == 57, sum(coalition$tier == "core") == 31, !anyNA(coalition$tier),
          all(coalition$Country %in% es_idx$Country))

rm(io)
invisible(gc())

# The allied picks of the Strategic Index (scripts/20_build_indices.R): each member's top
# Upstream/Midstream sector, Geothermal and Green Hydrogen excluded, Upstream extraction stages
# only with a recorded resource base. Read from the pipeline's own output, checked below against
# a recomputation from strategic_index.
allied_picks <- utils::read.csv(processed_path("charts", "allied_strategic.csv"),
                                stringsAsFactors = FALSE) %>%
  dplyr::select(dplyr::all_of(keys), "iso3", "tier", "strategic_index")

# ---------------------------------------------------------------------------------------------
# Sample: pairs scored by both pillars, neither constant nor unscored
# ---------------------------------------------------------------------------------------------

pairs_es <- cells_es %>% dplyr::filter(.data$tech %in% headline_techs)
pairs_eo <- cells_eo %>% dplyr::filter(.data$tech %in% headline_techs)
flagged_pairs <- dplyr::bind_rows(
  pairs_es %>% dplyr::filter(.data$cell_status != "ok") %>% dplyr::mutate(pillar = "ES"),
  pairs_eo %>% dplyr::filter(.data$cell_status != "ok") %>% dplyr::mutate(pillar = "EO")
) %>% dplyr::select("pillar", "tech", "supply_chain", "cell_status")
ok_pairs <- dplyr::inner_join(
  pairs_es %>% dplyr::filter(.data$cell_status == "ok") %>% dplyr::select("tech", "supply_chain"),
  pairs_eo %>% dplyr::filter(.data$cell_status == "ok") %>% dplyr::select("tech", "supply_chain"),
  by = c("tech", "supply_chain")
)
pairs_eo_only <- dplyr::anti_join(
  pairs_eo %>% dplyr::select("tech", "supply_chain"),
  pairs_es %>% dplyr::select("tech", "supply_chain"),
  by = c("tech", "supply_chain")
)

# Cells scored by both pillars in the kept pairs, before and after the rankability rule.
joint_scored <- dplyr::inner_join(es_idx_full, eo_idx_full, by = keys) %>%
  dplyr::semi_join(ok_pairs, by = c("tech", "supply_chain")) %>%
  dplyr::filter(!is.na(.data$es), !is.na(.data$eo))
not_sovereign <- function(x) grepl("not a sovereign", x)
rankability_counts <- list(
  n_cells_scored_by_both = nrow(joint_scored),
  n_economies_scored_by_both = dplyr::n_distinct(joint_scored$Country),
  n_cells_rankable_on_both = sum(joint_scored$es_rankable & joint_scored$eo_rankable),
  n_cells_dropped = sum(!(joint_scored$es_rankable & joint_scored$eo_rankable)),
  n_dropped_not_sovereign = sum(not_sovereign(joint_scored$es_rank_exclusion) |
                                  not_sovereign(joint_scored$eo_rank_exclusion)),
  n_dropped_sovereign_es_only_over_threshold =
    sum(!not_sovereign(joint_scored$es_rank_exclusion) & !not_sovereign(joint_scored$eo_rank_exclusion) &
          !joint_scored$es_rankable & joint_scored$eo_rankable),
  n_dropped_sovereign_eo_only_over_threshold =
    sum(!not_sovereign(joint_scored$es_rank_exclusion) & !not_sovereign(joint_scored$eo_rank_exclusion) &
          joint_scored$es_rankable & !joint_scored$eo_rankable),
  n_dropped_sovereign_both_over_threshold =
    sum(!not_sovereign(joint_scored$es_rank_exclusion) & !not_sovereign(joint_scored$eo_rank_exclusion) &
          !joint_scored$es_rankable & !joint_scored$eo_rankable)
)
rankability_counts$n_dropped_sovereign_over_threshold <-
  rankability_counts$n_dropped_sovereign_es_only_over_threshold +
  rankability_counts$n_dropped_sovereign_eo_only_over_threshold +
  rankability_counts$n_dropped_sovereign_both_over_threshold
stopifnot(rankability_counts$n_cells_dropped ==
            rankability_counts$n_dropped_not_sovereign +
            rankability_counts$n_dropped_sovereign_es_only_over_threshold +
            rankability_counts$n_dropped_sovereign_eo_only_over_threshold +
            rankability_counts$n_dropped_sovereign_both_over_threshold)

joint <- joint_scored %>%
  dplyr::filter(.data$es_rankable, .data$eo_rankable) %>%
  dplyr::select(dplyr::all_of(keys), "es", "eo") %>%
  dplyr::group_by(.data$tech, .data$supply_chain) %>%
  dplyr::mutate(
    n_pair = dplyr::n(),
    es_pr = pct_rank(.data$es),
    eo_pr = pct_rank(.data$eo),
    es_hi = .data$es > stats::median(.data$es),
    eo_hi = .data$eo > stats::median(.data$eo),
    es_at_median = .data$es == stats::median(.data$es),
    eo_at_median = .data$eo == stats::median(.data$eo)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    quadrant = classify_quadrant(.data$es_hi, .data$eo_hi),
    supply_chain = factor(.data$supply_chain, levels = stage_levels),
    pair = paste(.data$tech, .data$supply_chain, sep = " - "),
    coalition = .data$Country %in% coalition$Country
  ) %>%
  dplyr::left_join(coalition, by = "Country")

stopifnot(!anyDuplicated(joint[, keys]))

# Coalition members with no rankable joint cell at all (they then have no posture profile).
coalition_absent <- setdiff(coalition$Country, joint$Country)

# ---------------------------------------------------------------------------------------------
# Shared components: same category in both pillars, identical observed values
# ---------------------------------------------------------------------------------------------

shared_map <- dplyr::inner_join(
  var_es %>% dplyr::filter(!.data$imputed) %>%
    dplyr::select(dplyr::all_of(keys), "category", var_es = "variable", x_es = "x"),
  var_eo %>% dplyr::filter(!.data$imputed) %>%
    dplyr::select(dplyr::all_of(keys), "category", var_eo = "variable", x_eo = "x"),
  by = c(keys, "category"), relationship = "many-to-many"
) %>%
  dplyr::group_by(.data$category, .data$var_es, .data$var_eo) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    share_identical = mean(abs(.data$x_es - .data$x_eo) < 1e-9),
    .groups = "drop"
  ) %>%
  dplyr::filter(.data$n_obs >= 30, .data$share_identical >= 0.99)

shared_es <- unique(paste(shared_map$category, shared_map$var_es, sep = "::"))
shared_eo <- unique(paste(shared_map$category, shared_map$var_eo, sep = "::"))

var_es <- var_es %>% dplyr::mutate(shared = paste(.data$category, .data$variable, sep = "::") %in% shared_es)
var_eo <- var_eo %>% dplyr::mutate(shared = paste(.data$category, .data$variable, sep = "::") %in% shared_eo)

# Sanity check: the pillar is the weighted mean of its components.
chk <- subset_index(var_es, rep(TRUE, nrow(var_es))) %>%
  dplyr::inner_join(es_idx, by = keys)
stopifnot(max(abs(chk$value - chk$es)) < 1e-8)
chk <- subset_index(var_eo, rep(TRUE, nrow(var_eo))) %>%
  dplyr::inner_join(eo_idx, by = keys)
stopifnot(max(abs(chk$value - chk$eo)) < 1e-8)
rm(chk)

es_ns <- subset_index(var_es, !var_es$shared) %>% dplyr::rename(es_ns = "value")
eo_ns <- subset_index(var_eo, !var_eo$shared) %>% dplyr::rename(eo_ns = "value")
# Diagnostic: EO's non-shared components without its own Trade components (RCA, export size,
# feasibility), to see how much of the residual ES-EO association runs through trade.
eo_ns_notrade <- subset_index(var_eo, !var_eo$shared & var_eo$category != "Trade") %>%
  dplyr::rename(eo_ns_notrade = "value")

joint <- joint %>%
  dplyr::mutate(supply_chain = as.character(.data$supply_chain)) %>%
  dplyr::left_join(es_ns, by = keys) %>%
  dplyr::left_join(eo_ns, by = keys) %>%
  dplyr::left_join(eo_ns_notrade, by = keys) %>%
  dplyr::mutate(supply_chain = factor(.data$supply_chain, levels = stage_levels))

# Category-level sharing: a category is "fully shared" when every component of it is shared
# in both pillars, "partly shared" when some are.
cat_sharing <- dplyr::bind_rows(
  var_es %>% dplyr::semi_join(joint, by = keys) %>% dplyr::mutate(pillar = "ES"),
  var_eo %>% dplyr::semi_join(joint, by = keys) %>% dplyr::mutate(pillar = "EO")
) %>%
  dplyr::group_by(.data$pillar, .data$category) %>%
  dplyr::summarise(
    n_components = dplyr::n_distinct(.data$variable),
    n_shared = dplyr::n_distinct(.data$variable[.data$shared]),
    shared_components = paste(sort(unique(.data$variable[.data$shared])), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(sharing = dplyr::case_when(
    n_shared == 0 ~ "not shared",
    n_shared == n_components ~ "fully shared",
    TRUE ~ "partly shared"
  ))

# Overlap: effective weight and value share of shared components, per cell in the sample.
overlap_cell <- function(v) {
  v %>%
    dplyr::semi_join(joint, by = keys) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      w_shared = sum(.data$w[.data$shared]),
      value_share_shared = ifelse(sum(.data$contrib) > 0,
                                  sum(.data$contrib[.data$shared]) / sum(.data$contrib), NA_real_),
      all_shared = all(.data$shared),
      .groups = "drop"
    )
}
overlap_es <- overlap_cell(var_es)
overlap_eo <- overlap_cell(var_eo)

overlap_summary <- function(o) {
  o <- o %>% dplyr::left_join(joint %>% dplyr::select(dplyr::all_of(keys), "pair"),
                              by = keys)
  stage <- o %>%
    dplyr::mutate(supply_chain = as.character(.data$supply_chain)) %>%
    dplyr::group_by(.data$supply_chain) %>%
    dplyr::summarise(
      mean_effective_weight_shared = r3(mean(.data$w_shared)),
      mean_value_share_shared = r3(mean(.data$value_share_shared, na.rm = TRUE)),
      .groups = "drop"
    )
  pairs_all_shared <- o %>%
    dplyr::group_by(.data$pair) %>%
    dplyr::summarise(all_shared = all(.data$all_shared), .groups = "drop") %>%
    dplyr::filter(.data$all_shared) %>%
    dplyr::pull(.data$pair)
  list(
    mean_effective_weight_shared = r3(mean(o$w_shared)),
    mean_value_share_shared = r3(mean(o$value_share_shared, na.rm = TRUE)),
    by_stage = stage,
    pairs_where_every_component_is_shared = sort(pairs_all_shared),
    n_pairs_where_every_component_is_shared = length(pairs_all_shared)
  )
}

# ---------------------------------------------------------------------------------------------
# Correlations
# ---------------------------------------------------------------------------------------------

pair_stats <- joint %>%
  dplyr::group_by(.data$tech, .data$supply_chain, .data$pair) %>%
  dplyr::summarise(
    n = dplyr::n(),
    rho = spearman(.data$es, .data$eo),
    rho_nonshared = spearman(.data$es_ns, .data$eo_ns),
    rho_es_vs_eo_nonshared = spearman(.data$es, .data$eo_ns),
    rho_nonshared_eo_without_trade = spearman(.data$es_ns, .data$eo_ns_notrade),
    es_tied_at_median = mean(.data$es_at_median),
    eo_tied_at_median = mean(.data$eo_at_median),
    share_opp_wo_sec = mean(.data$quadrant == "Opportunity without security"),
    share_leaders = mean(.data$quadrant == "Leaders"),
    share_sec_wo_opp = mean(.data$quadrant == "Security without opportunity"),
    share_laggards = mean(.data$quadrant == "Laggards"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(implied_offdiag = implied_offdiag_share(.data$rho)) %>%
  dplyr::arrange(.data$rho)

pairs_ns <- pair_stats %>% dplyr::filter(!is.na(.data$rho_nonshared))

# Pooled within-pair Spearman: Pearson correlation of within-pair percentile ranks, pooled.
pooled_within <- function(d) r3(stats::cor(d$es_pr, d$eo_pr))

# Kept unrounded: the strip labels and the stage table round these to two decimals, and rounding
# a value already rounded to three decimals can go the wrong way (0.6948 -> 0.695 -> 0.70). The
# JSON copy is rounded to four decimals when it is written.
stage_stats <- joint %>%
  dplyr::group_by(supply_chain = as.character(.data$supply_chain)) %>%
  dplyr::group_modify(~ tibble::tibble(
    n_cells = nrow(.x),
    n_pairs = dplyr::n_distinct(.x$tech),
    pooled_within_pair_spearman = stats::cor(.x$es_pr, .x$eo_pr),
    pooled_spearman_levels = spearman(.x$es, .x$eo)
  )) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    pair_stats %>%
      dplyr::group_by(supply_chain = as.character(.data$supply_chain)) %>%
      dplyr::summarise(
        median_within_pair_spearman = stats::median(.data$rho),
        min_within_pair_spearman = min(.data$rho),
        max_within_pair_spearman = max(.data$rho),
        n_pairs_nonshared_defined = sum(!is.na(.data$rho_nonshared)),
        median_within_pair_spearman_nonshared = stats::median(.data$rho_nonshared, na.rm = TRUE),
        median_within_pair_spearman_full_same_pairs = stats::median(.data$rho[!is.na(.data$rho_nonshared)]),
        .groups = "drop"
      ),
    by = "supply_chain"
  ) %>%
  dplyr::mutate(supply_chain = factor(.data$supply_chain, levels = stage_levels)) %>%
  dplyr::arrange(.data$supply_chain) %>%
  dplyr::mutate(supply_chain = as.character(.data$supply_chain))

tech_stats <- pair_stats %>%
  dplyr::group_by(.data$tech) %>%
  dplyr::summarise(
    n_pairs = dplyr::n(),
    median_within_pair_spearman = r3(stats::median(.data$rho)),
    within_pair_spearman_by_stage = paste0(
      as.character(.data$supply_chain), " ", sprintf("%.2f", .data$rho), collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    joint %>% dplyr::group_by(.data$tech) %>%
      dplyr::summarise(pooled_within_pair_spearman = pooled_within(dplyr::pick(dplyr::everything())),
                       .groups = "drop"),
    by = "tech"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$median_within_pair_spearman))

fe_r2 <- function(y, g) summary(stats::lm(y ~ factor(g)))$r.squared

# ---------------------------------------------------------------------------------------------
# Quadrants
# ---------------------------------------------------------------------------------------------

quad_share <- function(d) {
  d %>%
    dplyr::count(.data$quadrant, .drop = FALSE) %>%
    dplyr::mutate(share = r3(.data$n / sum(.data$n)))
}
quad_pooled <- quad_share(joint)
quad_stage <- joint %>%
  dplyr::group_by(supply_chain = as.character(.data$supply_chain)) %>%
  dplyr::group_modify(~ quad_share(.x)) %>%
  dplyr::ungroup()
quad_coalition <- quad_share(joint %>% dplyr::filter(.data$coalition))
quad_tech <- joint %>%
  dplyr::group_by(.data$tech) %>%
  dplyr::group_modify(~ quad_share(.x)) %>%
  dplyr::ungroup() %>%
  dplyr::select("tech", "quadrant", "share") %>%
  tidyr::pivot_wider(names_from = "quadrant", values_from = "share") %>%
  dplyr::arrange(dplyr::desc(.data$`Opportunity without security`))

# Pairs where more than half of the countries share the median value of a pillar: there the
# median split separates a minority from a tied mass, so quadrant membership is a construction
# artefact more than a measurement. Reported as a sensitivity, not dropped.
# Tie-dominated pairs: at least a quarter of the rankable economies tie at the ES or EO median (the
# convention that a tied cell counts as low then decides the region of a large block of cells).
tie_pairs <- pair_stats$pair[pair_stats$es_tied_at_median >= 0.25 | pair_stats$eo_tied_at_median >= 0.25]
joint_no_tie <- joint %>% dplyr::filter(!.data$pair %in% tie_pairs)

# Sensitivity to the tie convention: a cell tied at its pair's median counted as high instead of
# low. Region shares of pairs with a large tie mass at a median move with the convention.
joint <- joint %>%
  dplyr::group_by(.data$tech, .data$supply_chain) %>%
  dplyr::mutate(es_hi_ge = .data$es >= stats::median(.data$es),
                eo_hi_ge = .data$eo >= stats::median(.data$eo)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(quadrant_ties_high = classify_quadrant(.data$es_hi_ge, .data$eo_hi_ge))
quad_share_ties_high <- function(d) quad_share(d %>% dplyr::mutate(quadrant = .data$quadrant_ties_high))

# Cells whose Energy Security components are all imputed, apart from the price-volatility index
# (which is constant across economies within a pair).
es_all_imputed <- var_es %>%
  dplyr::filter(.data$category != "Energy Prices") %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
  dplyr::summarise(es_all_imputed_excl_prices = all(.data$imputed), .groups = "drop")

tie_by_pair <- joint %>%
  dplyr::mutate(supply_chain = as.character(.data$supply_chain)) %>%
  dplyr::left_join(es_all_imputed, by = keys) %>%
  dplyr::group_by(.data$pair) %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_tied_at_es_median = sum(.data$es_at_median),
    n_tied_at_es_median_eo_high = sum(.data$es_at_median & .data$eo_hi),
    n_tied_at_es_median_es_all_imputed = sum(.data$es_at_median & .data$es_all_imputed_excl_prices),
    n_tied_at_eo_median = sum(.data$eo_at_median),
    es_median_value = r3(stats::median(.data$es)),
    es_median_is_pair_minimum = stats::median(.data$es) == min(.data$es),
    share_tied_at_es_median = r3(mean(.data$es_at_median)),
    share_tied_at_eo_median = r3(mean(.data$eo_at_median)),
    share_opp_wo_sec = r3(mean(.data$quadrant == "Opportunity without security")),
    share_opp_wo_sec_ties_high = r3(mean(.data$quadrant_ties_high == "Opportunity without security")),
    .groups = "drop"
  ) %>%
  dplyr::filter(.data$n_tied_at_es_median + .data$n_tied_at_eo_median >= 0.05 * .data$n) %>%
  dplyr::arrange(dplyr::desc(abs(.data$share_opp_wo_sec_ties_high - .data$share_opp_wo_sec)))

# The scatter draws the region boundaries at percentile rank 0.5. A cell tied at the median is
# counted as low but plotted at the mean rank of its tie group, which can exceed 0.5: count the
# cells that are plotted on the other side of a line from the region they are counted in.
line_mis <- joint %>%
  dplyr::mutate(mis_es = (.data$es_pr > 0.5) != .data$es_hi,
                mis_eo = (.data$eo_pr > 0.5) != .data$eo_hi) %>%
  dplyr::filter(.data$mis_es | .data$mis_eo)

top_opp_pairs <- pair_stats %>%
  dplyr::arrange(dplyr::desc(.data$share_opp_wo_sec)) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::transmute(pair, n, share_opp_wo_sec = r3(share_opp_wo_sec),
                   rho = r3(rho), es_tied_at_median = r3(es_tied_at_median))

# Coalition posture profiles.
coal_profile <- joint %>%
  dplyr::filter(.data$coalition) %>%
  dplyr::group_by(.data$iso3c, .data$Country, .data$tier) %>%
  dplyr::summarise(
    n_sectors = dplyr::n(),
    n_opp_wo_sec = sum(.data$quadrant == "Opportunity without security"),
    n_leaders = sum(.data$quadrant == "Leaders"),
    n_sec_wo_opp = sum(.data$quadrant == "Security without opportunity"),
    n_laggards = sum(.data$quadrant == "Laggards"),
    # The same count outside the pairs in which most economies tie at a pillar median.
    n_opp_wo_sec_excl_tie_pairs = sum(.data$quadrant == "Opportunity without security" &
                                        !.data$pair %in% tie_pairs),
    opp_wo_sec_sectors = paste(sort(.data$pair[.data$quadrant == "Opportunity without security"]),
                               collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(share_opp_wo_sec = r3(.data$n_opp_wo_sec / .data$n_sectors),
                share_leaders = r3(.data$n_leaders / .data$n_sectors),
                share_sec_wo_opp = r3(.data$n_sec_wo_opp / .data$n_sectors),
                share_laggards = r3(.data$n_laggards / .data$n_sectors)) %>%
  dplyr::arrange(dplyr::desc(.data$n_opp_wo_sec), dplyr::desc(.data$n_leaders))

quad_by_tier <- joint %>%
  dplyr::filter(.data$coalition) %>%
  dplyr::group_by(tier = as.character(.data$tier)) %>%
  dplyr::group_modify(~ quad_share(.x)) %>%
  dplyr::ungroup()

# Technologies most often in the opportunity-without-security quadrant among coalition members.
coal_opp_tech <- joint %>%
  dplyr::filter(.data$coalition, .data$quadrant == "Opportunity without security") %>%
  dplyr::count(.data$pair, name = "n_members") %>%
  dplyr::arrange(dplyr::desc(.data$n_members))

# Where the Strategic Index concentrates: coalition members' top-ranked SI sectors, by quadrant.
si_q <- si_tbl %>%
  dplyr::filter(!is.na(.data$strategic_index)) %>%
  dplyr::inner_join(joint %>% dplyr::select(dplyr::all_of(keys), "quadrant", "coalition"),
                    by = keys)
si_top <- function(k) {
  si_q %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::slice_max(order_by = .data$strategic_index, n = k, with_ties = FALSE) %>%
    dplyr::ungroup()
}
si_share_opp <- function(d) r3(mean(d$quadrant == "Opportunity without security"))
si_mean_by_quad <- si_q %>%
  dplyr::group_by(.data$quadrant) %>%
  dplyr::summarise(n = dplyr::n(), mean_si = r3(mean(.data$strategic_index)), .groups = "drop")
# Every SI cell needs rankable EO and ES, so every SI cell of a kept pair is in the joint sample.
n_si_cells <- sum(!is.na(si_tbl$strategic_index))
n_si_cells_outside_joint <- n_si_cells - nrow(si_q)

# The allied picks: check the pipeline's file against a recomputation from strategic_index
# (top SI among Upstream/Midstream sectors, Geothermal and Green Hydrogen excluded, cells the
# endowment screen leaves eligible), then place each pick in the space.
picks_recomputed <- si_tbl %>%
  dplyr::filter(!is.na(.data$strategic_index), is.na(.data$pick_exclusion),
                .data$supply_chain %in% c("Upstream", "Midstream"),
                !.data$tech %in% c("Geothermal", "Green Hydrogen"),
                .data$Country %in% coalition$Country) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::slice_max(order_by = .data$strategic_index, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()
stopifnot(nrow(picks_recomputed) == nrow(allied_picks),
          nrow(dplyr::inner_join(picks_recomputed, allied_picks, by = keys)) == nrow(allied_picks))
picks_q <- allied_picks %>%
  dplyr::left_join(joint %>% dplyr::select(dplyr::all_of(keys), "quadrant", "es_pr", "eo_pr"),
                   by = keys)
stopifnot(!anyNA(picks_q$quadrant))
picks_quad_counts <- picks_q %>%
  dplyr::mutate(quadrant = factor(as.character(.data$quadrant), levels = quad_levels)) %>%
  dplyr::count(.data$quadrant, .drop = FALSE)
# The pick-eligible pool of the members that have a pick, by quadrant, as the benchmark.
pick_pool_q <- si_q %>%
  dplyr::filter(.data$Country %in% allied_picks$Country, is.na(.data$pick_exclusion),
                .data$supply_chain %in% c("Upstream", "Midstream"),
                !.data$tech %in% c("Geothermal", "Green Hydrogen"))
# Regions of the picks by sector, and the size of each picking member's eligible pool (the
# number of upstream and midstream sectors the pick was chosen from).
picks_by_pair_quadrant <- picks_q %>%
  dplyr::mutate(pair = paste(.data$tech, .data$supply_chain, sep = " - "),
                quadrant = as.character(.data$quadrant)) %>%
  dplyr::group_by(.data$pair) %>%
  dplyr::summarise(
    n_picks = dplyr::n(),
    n_opp_wo_sec = sum(.data$quadrant == "Opportunity without security"),
    n_leaders = sum(.data$quadrant == "Leaders"),
    n_sec_wo_opp = sum(.data$quadrant == "Security without opportunity"),
    n_laggards = sum(.data$quadrant == "Laggards"),
    members = paste(sort(.data$iso3), collapse = " "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(.data$n_picks))
pick_pool_size <- pick_pool_q %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(n_eligible = dplyr::n(),
                   eligible_sectors = paste(sort(paste(.data$tech, .data$supply_chain, sep = " - ")),
                                            collapse = "; "),
                   .groups = "drop")
picks_q <- picks_q %>% dplyr::left_join(pick_pool_size, by = "Country")
stopifnot(!anyNA(picks_q$n_eligible))

# Does the SI ranking of a member's sectors follow the quadrant logic? Within-country Spearman
# between SI and the within-pair rank gap (EO rank minus ES rank), and the level gap EO - ES.
# Within-country Spearman correlation of EO and the need term 1 - ES over each SI country's
# in-sample sectors (the Spearman correlation of EO and 1 - ES is minus that of EO and ES).
si_eo_need <- si_tbl %>%
  dplyr::filter(!is.na(.data$strategic_index)) %>%
  dplyr::inner_join(dplyr::inner_join(es_idx, eo_idx, by = keys), by = keys) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(n = dplyr::n(), rho = spearman(.data$eo, 1 - .data$es), .groups = "drop") %>%
  dplyr::filter(.data$n >= min_wc_cells)

si_gap <- si_q %>%
  dplyr::inner_join(joint %>% dplyr::select(dplyr::all_of(keys), "es", "eo", "es_pr", "eo_pr"),
                    by = keys) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(
    n = dplyr::n(),
    rho_rank_gap = spearman(.data$strategic_index, .data$eo_pr - .data$es_pr),
    rho_level_gap = spearman(.data$strategic_index, .data$eo - .data$es),
    .groups = "drop"
  ) %>%
  dplyr::filter(.data$n >= min_wc_cells)
stopifnot(!anyNA(si_eo_need$rho), !anyNA(si_gap$rho_rank_gap), !anyNA(si_gap$rho_level_gap))

# Within-country view (equation eq:joint_var of Section 7), over each country's Strategic Index
# sample: the Pearson and Spearman correlations of EO and ES across the country's cells, the
# standard deviation of Delta = EO - ES, and the share of Var(EO) + Var(ES) that the covariance
# term removes from Var(Delta), 2 Cov(EO, ES) / (Var EO + Var ES). Levels, not ranks: the
# Strategic Index uses pillar levels.
within_country <- si_tbl %>%
  dplyr::filter(!is.na(.data$strategic_index)) %>%
  dplyr::inner_join(dplyr::inner_join(es_idx, eo_idx, by = keys), by = keys) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(
    n = dplyr::n(),
    pearson = stats::cor(.data$eo, .data$es),
    spearman = spearman(.data$eo, .data$es),
    sd_eo = stats::sd(.data$eo),
    sd_es = stats::sd(.data$es),
    sd_delta = stats::sd(.data$eo - .data$es),
    var_sum = stats::var(.data$eo) + stats::var(.data$es),
    cov = stats::cov(.data$eo, .data$es),
    .groups = "drop"
  ) %>%
  dplyr::mutate(share_removed_by_cov = 2 * .data$cov / .data$var_sum)
# Correlations and dispersions over very few sectors are not informative (a country with one SI
# cell has none): the within-country view keeps countries with at least min_wc_cells SI cells.
within_country_dropped <- within_country %>%
  dplyr::filter(.data$n < min_wc_cells) %>%
  dplyr::transmute(Country, n)
within_country <- within_country %>% dplyr::filter(.data$n >= min_wc_cells)
stopifnot(max(abs(within_country$sd_delta^2 -
                    (within_country$var_sum - 2 * within_country$cov))) < 1e-10)

# How much of the within-country co-movement is common to every country: the pillars' pair means
# over all scored economies in the joint sample, their correlation across pairs, and each
# country's correlation of the pillars after subtracting those pair means.
pair_means <- joint %>%
  dplyr::group_by(.data$tech, supply_chain = as.character(.data$supply_chain)) %>%
  dplyr::summarise(es_bar = mean(.data$es), eo_bar = mean(.data$eo), .groups = "drop")
within_country_demeaned <- si_tbl %>%
  dplyr::filter(!is.na(.data$strategic_index)) %>%
  dplyr::inner_join(dplyr::inner_join(es_idx, eo_idx, by = keys), by = keys) %>%
  dplyr::inner_join(pair_means, by = c("tech", "supply_chain")) %>%
  dplyr::group_by(.data$Country) %>%
  dplyr::summarise(
    pearson_demeaned = stats::cor(.data$eo - .data$eo_bar, .data$es - .data$es_bar),
    sd_delta_demeaned = stats::sd((.data$eo - .data$eo_bar) - (.data$es - .data$es_bar)),
    .groups = "drop"
  )
within_country <- within_country %>% dplyr::left_join(within_country_demeaned, by = "Country")

wc_summary <- function(x, d = within_country) {
  list(mean = r3(mean(d[[x]])), median = r3(stats::median(d[[x]])),
       min = list(country = d$Country[which.min(d[[x]])], value = r3(min(d[[x]]))),
       max = list(country = d$Country[which.max(d[[x]])], value = r3(max(d[[x]]))))
}

# ---------------------------------------------------------------------------------------------
# Covariance decomposition within pairs (equation eq:joint_cov of Section 7), on pillar levels:
# P = A^P + B^P, with A^P the summed contributions of the shared components and B^P the rest
# (each divided by the cell's total effective weight, which is one).
# ---------------------------------------------------------------------------------------------

ab_parts <- function(v, prefix) {
  v %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(A = sum(.data$contrib[.data$shared]) / sum(.data$w),
                     B = sum(.data$contrib[!.data$shared]) / sum(.data$w),
                     .groups = "drop") %>%
    dplyr::rename_with(~ paste0(prefix, .x), c("A", "B"))
}

cov_dec <- joint %>%
  dplyr::select(dplyr::all_of(keys), "pair", "es", "eo") %>%
  dplyr::mutate(supply_chain = as.character(.data$supply_chain)) %>%
  dplyr::inner_join(ab_parts(var_es, "es_"), by = keys) %>%
  dplyr::inner_join(ab_parts(var_eo, "eo_"), by = keys)
stopifnot(nrow(cov_dec) == nrow(joint),
          max(abs(cov_dec$es_A + cov_dec$es_B - cov_dec$es)) < 1e-8,
          max(abs(cov_dec$eo_A + cov_dec$eo_B - cov_dec$eo)) < 1e-8)

cov_pair <- cov_dec %>%
  dplyr::group_by(.data$supply_chain, .data$pair) %>%
  dplyr::summarise(
    n = dplyr::n(),
    cov_total = stats::cov(.data$es, .data$eo),
    cov_AA = stats::cov(.data$es_A, .data$eo_A),
    cov_AB = stats::cov(.data$es_A, .data$eo_B),
    cov_BA = stats::cov(.data$es_B, .data$eo_A),
    cov_BB = stats::cov(.data$es_B, .data$eo_B),
    .groups = "drop"
  ) %>%
  dplyr::mutate(share_AA = .data$cov_AA / .data$cov_total,
                share_BB = .data$cov_BB / .data$cov_total)
stopifnot(max(abs(cov_pair$cov_AA + cov_pair$cov_AB + cov_pair$cov_BA + cov_pair$cov_BB -
                    cov_pair$cov_total)) < 1e-10)

# Pooled shares: the within-pair covariance terms summed over pairs with weights n - 1, i.e. the
# decomposition of the pooled within-pair covariance.
cov_pool <- function(d, rnd = r3) {
  tot <- sum((d$n - 1) * d$cov_total)
  list(
    n_pairs = nrow(d),
    share_shared_term_AA = rnd(sum((d$n - 1) * d$cov_AA) / tot),
    share_cross_terms_AB_BA = rnd(sum((d$n - 1) * (d$cov_AB + d$cov_BA)) / tot),
    share_nonshared_term_BB = rnd(sum((d$n - 1) * d$cov_BB) / tot),
    median_pair_share_AA = rnd(stats::median(d$share_AA)),
    n_pairs_cov_total_positive = sum(d$cov_total > 0)
  )
}
cov_by_stage <- lapply(stats::setNames(stage_levels, stage_levels),
                       function(s) cov_pool(cov_pair[cov_pair$supply_chain == s, ]))

# ---------------------------------------------------------------------------------------------
# Category correlation matrix: average within-pair Spearman across the pairs in which both
# categories are present and vary across countries
# ---------------------------------------------------------------------------------------------

cat_long <- dplyr::bind_rows(
  cat_es %>% dplyr::mutate(pillar = "ES"),
  cat_eo %>% dplyr::mutate(pillar = "EO")
) %>%
  dplyr::semi_join(joint, by = keys) %>%
  dplyr::group_by(.data$pillar, .data$category, .data$tech, .data$supply_chain) %>%
  dplyr::mutate(varies = stats::sd(.data$category_score) > 1e-12) %>%
  dplyr::ungroup()

cats_constant_everywhere <- cat_long %>%
  dplyr::group_by(.data$pillar, .data$category) %>%
  dplyr::summarise(any_var = any(.data$varies), .groups = "drop") %>%
  dplyr::filter(!.data$any_var) %>%
  dplyr::mutate(label = paste0(.data$category, " (", .data$pillar, ")"))

cat_long <- cat_long %>%
  dplyr::filter(.data$varies) %>%
  dplyr::mutate(var = paste0(.data$category, " (", .data$pillar, ")"))

shared_cat_order <- c("Consumption", "Energy Access", "Investment", "Trade",
                      "Foreign Dependency", "Production")
es_only <- c("Reserves", "Minerals Trade", "Energy Imports")
eo_only <- c("Technology Demand", "Cost Competitiveness")
var_order <- c(
  paste0(c(shared_cat_order, es_only), " (ES)"),
  paste0(c(shared_cat_order, eo_only), " (EO)")
)
var_order <- var_order[var_order %in% unique(cat_long$var)]
stopifnot(setequal(var_order, unique(cat_long$var)))

cat_wide <- cat_long %>%
  dplyr::select(dplyr::all_of(keys), "var", "category_score") %>%
  tidyr::pivot_wider(names_from = "var", values_from = "category_score")

min_pair_n <- 20
cat_pairs <- utils::combn(var_order, 2, simplify = FALSE)
cat_corr <- dplyr::bind_rows(lapply(cat_pairs, function(p) {
  d <- cat_wide %>%
    dplyr::select(dplyr::all_of(c("tech", "supply_chain", p))) %>%
    dplyr::filter(!is.na(.data[[p[1]]]), !is.na(.data[[p[2]]]))
  per_pair <- d %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(n = dplyr::n(), rho = spearman(.data[[p[1]]], .data[[p[2]]]),
                     .groups = "drop") %>%
    dplyr::filter(.data$n >= min_pair_n, !is.na(.data$rho))
  tibble::tibble(
    var1 = p[1], var2 = p[2],
    n_pairs = nrow(per_pair),
    mean_rho = if (nrow(per_pair) > 0) mean(per_pair$rho) else NA_real_
  )
}))

cross_same_category <- cat_corr %>%
  dplyr::mutate(
    c1 = sub(" \\((ES|EO)\\)$", "", .data$var1), c2 = sub(" \\((ES|EO)\\)$", "", .data$var2),
    p1 = sub("^.* \\((ES|EO)\\)$", "\\1", .data$var1), p2 = sub("^.* \\((ES|EO)\\)$", "\\1", .data$var2)
  ) %>%
  dplyr::filter(.data$c1 == .data$c2, .data$p1 != .data$p2) %>%
  dplyr::transmute(category = c1, n_pairs, mean_rho = r3(mean_rho))

cross_block <- cat_corr %>%
  dplyr::filter(grepl("\\(ES\\)$", .data$var1), grepl("\\(EO\\)$", .data$var2)) %>%
  dplyr::mutate(
    c1 = sub(" \\(ES\\)$", "", .data$var1), c2 = sub(" \\(EO\\)$", "", .data$var2)
  )
cross_diff_category <- cross_block %>%
  dplyr::filter(.data$c1 != .data$c2, !is.na(.data$mean_rho))

# EO's distinctive categories against every ES category.
eo_only_vs_es <- cross_block %>%
  dplyr::filter(.data$c2 %in% eo_only, !is.na(.data$mean_rho)) %>%
  dplyr::group_by(category = .data$c2) %>%
  dplyr::summarise(n_es_categories = dplyr::n(), max_abs_mean_rho = r3(max(abs(.data$mean_rho))),
                   min_mean_rho = r3(min(.data$mean_rho)), max_mean_rho = r3(max(.data$mean_rho)),
                   .groups = "drop")

# ---------------------------------------------------------------------------------------------
# Figure 1: the opportunity-security space
# ---------------------------------------------------------------------------------------------

fmt_rho <- function(x) sprintf("%.2f", x)
strip_lab <- stage_stats %>%
  dplyr::transmute(
    supply_chain,
    lab = sprintf("**%s** (%d pairs)<br>median Spearman %s<br>non-shared parts %s%s",
                  supply_chain, n_pairs,
                  fmt_rho(median_within_pair_spearman),
                  fmt_rho(median_within_pair_spearman_nonshared),
                  ifelse(n_pairs_nonshared_defined < n_pairs,
                         sprintf(" (%d pairs)", n_pairs_nonshared_defined), ""))
  )
strip_labeller <- ggplot2::as_labeller(stats::setNames(strip_lab$lab, strip_lab$supply_chain))

# Region shares are printed in bands above and below the unit square, outside the data, so that
# no label covers a point (the densest corners are exactly where the coalition sits). The upstream
# panel also names the regions.
band_y <- 0.065
# Percentages are rounded once, from the unrounded counts (quad_stage$share is already rounded to
# three decimals, and 0.36537 -> 0.365 -> "36%" would misprint the upstream laggards' 37%).
corner <- quad_stage %>%
  dplyr::group_by(.data$supply_chain) %>%
  dplyr::mutate(share = .data$n / sum(.data$n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    supply_chain = factor(.data$supply_chain, levels = stage_levels),
    x = ifelse(.data$quadrant %in% c("Leaders", "Security without opportunity"), 1, 0),
    y = ifelse(.data$quadrant %in% c("Leaders", "Opportunity without security"), 1 + band_y, -band_y),
    hjust = ifelse(.data$x > 0.5, 1, 0),
    short = dplyr::recode(as.character(.data$quadrant),
      "Opportunity without security" = "Opportunity only",
      "Leaders" = "Leaders",
      "Security without opportunity" = "Security only",
      "Laggards" = "Laggards"),
    lab = ifelse(.data$supply_chain == "Upstream",
                 paste(.data$short, scales::percent(.data$share, accuracy = 1)),
                 scales::percent(.data$share, accuracy = 1))
  )

# Direct labels: in each stage, the two coalition cells furthest into the
# opportunity-without-security corner (largest EO rank minus ES rank).
lab_pts <- joint %>%
  dplyr::filter(.data$coalition) %>%
  dplyr::mutate(gap = .data$eo_pr - .data$es_pr) %>%
  dplyr::group_by(.data$supply_chain) %>%
  dplyr::slice_max(order_by = .data$gap, n = 2, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(lab = paste(.data$iso3c, short_tech[.data$tech]))

# Panels already separate the stages, so coalition members wear two highlight colours by tier
# (violet for the core, magenta for partners; neither is a pillar or stage colour of the section)
# against gray; the blue and orange pillar colours are kept for the regions of joint_quadrants.
plot_df <- joint %>%
  dplyr::mutate(grp = factor(dplyr::case_when(
    .data$coalition & .data$tier == "core" ~ "Core",
    .data$coalition ~ "Partner",
    TRUE ~ "Other"
  ), levels = c("Core", "Partner", "Other")))

n_core <- sum(coalition$tier == "core")
n_partner <- sum(coalition$tier == "partner")
grp_cols <- c(Core = unname(paper_cat["violet"]), Partner = unname(paper_cat["magenta"]),
              Other = "#b4b3a9")
grp_labs <- c(Core = sprintf("Coalition core (%d)", n_core),
              Partner = sprintf("Coalition partner (%d)", n_partner),
              Other = "Other economy")

p_scatter <- ggplot(plot_df, aes(x = .data$es_pr, y = .data$eo_pr)) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = NA,
           colour = paper_ink$axis, linewidth = 0.3) +
  geom_point(data = ~ dplyr::filter(.x, !.data$coalition), aes(fill = .data$grp),
             shape = 21, colour = "transparent", stroke = 0, size = 0.7, alpha = 0.6) +
  geom_point(data = ~ dplyr::filter(.x, .data$coalition), aes(fill = .data$grp),
             shape = 21, colour = "white", stroke = 0.15, size = 1.1) +
  annotate("segment", x = 0, xend = 1, y = 0.5, yend = 0.5,
           colour = paper_ink$secondary, linewidth = 0.3) +
  annotate("segment", x = 0.5, xend = 0.5, y = 0, yend = 1,
           colour = paper_ink$secondary, linewidth = 0.3) +
  geom_text(data = corner, aes(x = .data$x, y = .data$y, label = .data$lab, hjust = .data$hjust),
            vjust = 0.5, size = 2.5, colour = paper_ink$primary, inherit.aes = FALSE) +
  ggrepel::geom_text_repel(
    data = lab_pts, aes(label = .data$lab), size = 2.5, colour = paper_ink$primary,
    min.segment.length = 0, segment.size = 0.25, segment.colour = paper_ink$secondary,
    box.padding = 0.3, point.padding = 0.15, nudge_x = 0.12, nudge_y = 0.04,
    # Labels stay inside the opportunity-without-security region their points belong to, clear
    # of the median lines.
    xlim = c(0.03, 0.47), ylim = c(0.56, 0.97),
    seed = 7, max.overlaps = Inf, bg.color = "white", bg.r = 0.12
  ) +
  facet_wrap(~ supply_chain, nrow = 1, labeller = strip_labeller) +
  scale_fill_manual(values = grp_cols, labels = grp_labs, drop = FALSE, name = NULL) +
  guides(fill = guide_legend(override.aes = list(size = 2.2, alpha = 1, colour = "transparent"))) +
  scale_x_continuous("Energy Security, within-pair percentile rank",
                     limits = c(0, 1), breaks = c(0, 0.5, 1),
                     labels = c("0", "0.5", "1"), expand = expansion(mult = 0.01)) +
  scale_y_continuous("Economic Opportunity, within-pair percentile rank",
                     limits = c(-2 * band_y, 1 + 2 * band_y), breaks = c(0, 0.5, 1),
                     labels = c("0", "0.5", "1"), expand = expansion(mult = 0)) +
  coord_fixed() +
  theme_paper(grid = "none") +
  theme(panel.spacing.x = unit(0.5, "cm"),
        axis.line.x = element_blank(),
        strip.text = ggtext::element_markdown(face = "plain", lineheight = 1.15, hjust = 0))

save_paper_fig(p_scatter, "joint_scatter", width = paper_width_full, height = 3.7)

# ---------------------------------------------------------------------------------------------
# Figure 2: category correlation matrix (lower triangle)
# ---------------------------------------------------------------------------------------------

n_es_vars <- sum(grepl("\\(ES\\)$", var_order))
mat <- dplyr::bind_rows(
  cat_corr,
  cat_corr %>% dplyr::rename(var1 = "var2", var2 = "var1")
) %>%
  dplyr::mutate(
    i = match(.data$var1, var_order), j = match(.data$var2, var_order)
  ) %>%
  dplyr::filter(.data$i > .data$j) %>%
  dplyr::mutate(
    row = factor(.data$var1, levels = rev(var_order)),
    col = factor(.data$var2, levels = var_order),
    same_cat = sub(" \\((ES|EO)\\)$", "", .data$var1) == sub(" \\((ES|EO)\\)$", "", .data$var2),
    show = !is.na(.data$mean_rho) & (abs(.data$mean_rho) >= 0.5 | .data$same_cat),
    lab = dplyr::case_when(
      is.na(.data$mean_rho) ~ "–",
      .data$show ~ sub("^(-?)0\\.", "\\1.", sprintf("%.2f", .data$mean_rho)),
      TRUE ~ ""
    ),
    ink = dplyr::case_when(
      is.na(.data$mean_rho) ~ paper_ink$muted,
      abs(.data$mean_rho) >= 0.6 ~ "white",
      TRUE ~ paper_ink$primary
    )
  )

# Position of the ES/EO boundary in the lower triangle.
b_col <- n_es_vars + 0.5
b_row <- length(var_order) - n_es_vars + 0.5

p_corr <- ggplot(mat, aes(x = .data$col, y = .data$row)) +
  geom_tile(aes(fill = .data$mean_rho), colour = "white", linewidth = 0.6) +
  geom_text(aes(label = .data$lab, colour = .data$ink,
                fontface = ifelse(.data$same_cat, "bold", "plain")), size = 2.5) +
  annotate("segment", x = b_col, xend = b_col, y = 0.5, yend = b_row,
           colour = paper_ink$primary, linewidth = 0.35) +
  annotate("segment", x = 0.5, xend = b_col, y = b_row, yend = b_row,
           colour = paper_ink$primary, linewidth = 0.35) +
  scale_colour_identity() +
  scale_fill_div(midpoint = 0, limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1),
                 na.value = "white",
                 name = "Average within-pair Spearman correlation") +
  # Unused levels dropped: the lower triangle has no tile in the first row (Consumption, ES) or the
  # last column (Cost Competitiveness, EO), and the remaining positions are unchanged.
  scale_x_discrete(position = "bottom", drop = TRUE) +
  scale_y_discrete(drop = TRUE) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line.x = element_blank(),
        legend.position = "inside", legend.position.inside = c(0.74, 0.86),
        legend.direction = "horizontal",
        legend.key.width = unit(1.1, "cm"), legend.key.height = unit(0.22, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

save_paper_fig(p_corr, "joint_corr", width = paper_width_full, height = 5.1)

# ---------------------------------------------------------------------------------------------
# Figures 3 and 4: regions of the space by technology-stage pair, and the coalition's posture
# ---------------------------------------------------------------------------------------------

# Regions: the hue names the pillar that is high on its own (orange = Economic Opportunity, blue =
# Energy Security, the paper's pillar colours); the two diagonal regions are neutrals, darker for
# both-high than for both-low.
quad_cols <- c(
  "Opportunity without security" = unname(paper_pillar_cols["Economic Opportunity"]),
  "Leaders" = "#8a8984",
  "Security without opportunity" = unname(paper_pillar_cols["Energy Security"]),
  "Laggards" = "#d6d5cd"
)
quad_labs <- c(
  "Opportunity without security" = "Opportunity without security",
  "Leaders" = "Leaders (both high)",
  "Security without opportunity" = "Security without opportunity",
  "Laggards" = "Laggards (both low)"
)

# Figure 3: every rankable economy, by technology-stage pair, one panel per stage; the number at
# the end of a bar is the pair's count of rankable economies.
pair_long <- pair_stats %>%
  dplyr::select("tech", "supply_chain", "n", "share_opp_wo_sec", "share_leaders",
                "share_sec_wo_opp", "share_laggards") %>%
  dplyr::mutate(supply_chain = factor(as.character(.data$supply_chain), levels = stage_levels),
                tech_lab = unname(short_tech[.data$tech])) %>%
  tidyr::pivot_longer(dplyr::starts_with("share_"), names_to = "q", values_to = "share") %>%
  dplyr::mutate(quadrant = factor(dplyr::recode(.data$q,
    share_opp_wo_sec = "Opportunity without security", share_leaders = "Leaders",
    share_sec_wo_opp = "Security without opportunity", share_laggards = "Laggards"),
    levels = quad_levels))
pair_order <- pair_stats %>%
  dplyr::arrange(.data$share_opp_wo_sec, .data$share_leaders) %>%
  dplyr::transmute(key = paste(.data$supply_chain, unname(short_tech[.data$tech])))
pair_long <- pair_long %>%
  dplyr::mutate(key = factor(paste(.data$supply_chain, .data$tech_lab), levels = pair_order$key))
pair_n_lab <- pair_long %>%
  dplyr::distinct(.data$supply_chain, .data$key, .data$n)

p_pairs <- ggplot(pair_long, aes(y = .data$key, x = .data$share, fill = .data$quadrant)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.3,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = pair_n_lab, aes(x = 1.03, y = .data$key, label = .data$n),
            inherit.aes = FALSE, hjust = 0, size = 2.3, colour = paper_ink$secondary) +
  facet_wrap(~ supply_chain, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = quad_cols, labels = quad_labs, name = NULL, drop = FALSE) +
  scale_x_continuous("Share of the pair's rankable economies",
                     labels = c("0%", "50%", "100%"), limits = c(0, 1.17),
                     breaks = c(0, 0.5, 1), expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(NULL, labels = function(k) sub("^\\S+ ", "", k)) +
  coord_cartesian(clip = "off") +
  theme_paper(grid = "x") +
  theme(axis.line.x = element_blank(), legend.position = "bottom",
        panel.spacing.x = unit(0.5, "cm"), plot.margin = margin(6, 12, 6, 6)) +
  guides(fill = guide_legend(nrow = 1))

save_paper_fig(p_pairs, "joint_quadrants", width = paper_width_full, height = 2.75)

# Figure 4: each coalition member's rankable sectors by region, core and partners side by side.
# The two panels are separate plots so that the rows have the same height in both: the partner
# panel's spare rows are left empty at the bottom.
short_country <- c(
  "Democratic Republic of Congo" = "DR Congo", "Dominican Republic" = "Dominican Rep.",
  "United States" = "United States", "United Kingdom" = "United Kingdom",
  "Slovak Republic" = "Slovakia", "Czech Republic" = "Czechia"
)
short_name <- function(x) ifelse(x %in% names(short_country), short_country[x], x)

all_econ_opp_share <- sum(joint$quadrant == "Opportunity without security") / nrow(joint)

posture <- coal_profile %>%
  dplyr::select("Country", "tier", "n_sectors", "n_opp_wo_sec", "n_leaders", "n_sec_wo_opp",
                "n_laggards") %>%
  tidyr::pivot_longer(c("n_opp_wo_sec", "n_leaders", "n_sec_wo_opp", "n_laggards"),
                      names_to = "q", values_to = "n") %>%
  dplyr::mutate(
    quadrant = factor(dplyr::recode(.data$q,
      n_opp_wo_sec = "Opportunity without security", n_leaders = "Leaders",
      n_sec_wo_opp = "Security without opportunity", n_laggards = "Laggards"),
      levels = quad_levels),
    share = .data$n / .data$n_sectors,
    name = short_name(.data$Country)
  )

member_panel <- function(tier_value, header, n_rows, show_line_label) {
  prof <- coal_profile %>% dplyr::filter(.data$tier == tier_value)
  ord <- prof %>%
    dplyr::arrange(.data$n_opp_wo_sec / .data$n_sectors, .data$n_leaders / .data$n_sectors,
                   dplyr::desc(.data$Country)) %>%
    dplyr::mutate(name = short_name(.data$Country)) %>%
    dplyr::pull(.data$name)
  d <- posture %>%
    dplyr::filter(.data$tier == tier_value) %>%
    dplyr::mutate(name = factor(.data$name, levels = ord))
  lab <- prof %>%
    dplyr::mutate(name = factor(short_name(.data$Country), levels = ord),
                  lab = sprintf("%d/%d", .data$n_opp_wo_sec, .data$n_sectors))
  spare <- n_rows - length(ord)
  p <- ggplot(d, aes(y = .data$name, x = .data$share, fill = .data$quadrant)) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.3,
             position = position_stack(reverse = TRUE)) +
    geom_text(data = lab, aes(x = 1.03, y = .data$name, label = .data$lab), inherit.aes = FALSE,
              hjust = 0, size = 2.3, colour = paper_ink$secondary) +
    geom_vline(xintercept = all_econ_opp_share, colour = paper_ink$primary, linewidth = 0.3) +
    scale_fill_manual(values = quad_cols, labels = quad_labs, name = NULL, drop = FALSE) +
    scale_x_continuous("Share of the member's rankable sectors",
                       labels = c("0%", "50%", "100%"), limits = c(0, 1.17),
                       breaks = c(0, 0.5, 1), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(NULL, expand = expansion(add = c(0.6 + spare, 1.6))) +
    labs(subtitle = header) +
    theme_paper(grid = "x") +
    theme(axis.line.x = element_blank(), legend.position = "bottom",
          plot.subtitle = element_text(face = "bold", colour = paper_ink$primary,
                                       size = 8.5, margin = margin(b = 2))) +
    guides(fill = guide_legend(nrow = 1))
  if (show_line_label) {
    p <- p + annotate("text", x = all_econ_opp_share + 0.015, y = length(ord) + 1.05,
                      label = sprintf("All rankable cells: %s",
                                      scales::percent(all_econ_opp_share, accuracy = 1)),
                      hjust = 0, vjust = 0.5, size = 2.3, colour = paper_ink$primary)
  }
  p
}

n_rows_members <- max(table(coal_profile$tier))
p_core <- member_panel("core", sprintf("Core members (%d)", sum(coal_profile$tier == "core")),
                       n_rows_members, TRUE)
p_partner <- member_panel("partner",
                          sprintf("Partners (%d)", sum(coal_profile$tier == "partner")),
                          n_rows_members, TRUE)

quad_legend <- cowplot::get_plot_component(p_core, "guide-box-bottom", return_all = TRUE)
if (is.list(quad_legend) && !inherits(quad_legend, "grob")) quad_legend <- quad_legend[[1]]
p_members <- cowplot::plot_grid(
  cowplot::plot_grid(p_core + theme(legend.position = "none"),
                     p_partner + theme(legend.position = "none"),
                     nrow = 1, rel_widths = c(1, 1), align = "h"),
  quad_legend, ncol = 1, rel_heights = c(1, 0.05)
) + theme(plot.background = element_rect(fill = "white", colour = NA))

save_paper_fig(p_members, "joint_members", width = paper_width_full, height = 5.3)

# ---------------------------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------------------------

rho_q <- stats::quantile(pair_stats$rho, c(0.25, 0.5, 0.75), names = FALSE)
rho_ns_q <- stats::quantile(pairs_ns$rho_nonshared, c(0.25, 0.5, 0.75), names = FALSE)
rho_full_same <- stats::median(pairs_ns$rho)
lo <- pair_stats[which.min(pair_stats$rho), ]
hi <- pair_stats[which.max(pair_stats$rho), ]

top1 <- si_top(1)
top3 <- si_top(3)
top5 <- si_top(5)
top1_coal <- top1 %>% dplyr::filter(.data$coalition)
top3_coal <- top3 %>% dplyr::filter(.data$coalition)
quad_counts <- function(d) {
  d %>%
    dplyr::mutate(quadrant = factor(as.character(.data$quadrant), levels = quad_levels)) %>%
    dplyr::count(.data$quadrant, .drop = FALSE)
}

results <- list(
  sample = list(
    description = paste(
      "Headline technologies; ES and EO joined on Country x tech x supply_chain; pairs flagged",
      "constant or unscored in either pillar's index_cells diagnostics dropped, as are pairs one",
      "pillar does not score; only cells rankable in both pillars (sovereign, imputed weight share",
      "<= 0.5). Percentile ranks, medians and Spearman correlations are computed within pair over",
      "the rankable cells. Quadrants use strictly-above-median within pair (ties count as low)."
    ),
    rankability = rankability_counts,
    n_cells_joint = nrow(joint),
    n_pairs_joint = nrow(pair_stats),
    n_countries = dplyr::n_distinct(joint$Country),
    n_cells_by_stage = joint %>% dplyr::count(supply_chain = as.character(.data$supply_chain)),
    n_rankable_economies_per_pair_range = range(pair_stats$n),
    n_rankable_economies_per_pair_min = pair_stats %>% dplyr::slice_min(.data$n, n = 2) %>%
      dplyr::transmute(pair, n),
    n_coalition_members_total = nrow(coalition),
    n_coalition_core = n_core,
    n_coalition_partner = n_partner,
    n_coalition_members = dplyr::n_distinct(joint$Country[joint$coalition]),
    coalition_members_without_rankable_joint_cells = coalition_absent,
    n_coalition_cells = sum(joint$coalition),
    n_coalition_cells_by_tier = joint %>% dplyr::filter(.data$coalition) %>%
      dplyr::count(tier = as.character(.data$tier)),
    headline_techs = headline_techs,
    pairs_flagged_in_index_cells = flagged_pairs,
    pairs_scored_by_eo_but_not_es = paste(pairs_eo_only$tech, pairs_eo_only$supply_chain, sep = " - "),
    n_pairs_eo_headline = nrow(pairs_eo %>% dplyr::distinct(tech, supply_chain)),
    n_pairs_es_headline = nrow(pairs_es %>% dplyr::distinct(tech, supply_chain)),
    n_pairs_with_es_value_headline = nrow(es_idx %>% dplyr::filter(!is.na(.data$es)) %>%
                                            dplyr::distinct(tech, supply_chain)),
    n_pairs_with_eo_value_headline = nrow(eo_idx %>% dplyr::filter(!is.na(.data$eo)) %>%
                                            dplyr::distinct(tech, supply_chain)),
    n_economies_with_es_value_headline = dplyr::n_distinct(es_idx$Country[!is.na(es_idx$es)]),
    n_economies_with_eo_value_headline = dplyr::n_distinct(eo_idx$Country[!is.na(eo_idx$eo)])
  ),
  covariance_decomposition = list(
    description = paste(
      "Within-pair covariance of ES and EO levels split as Cov(A_ES,A_EO) + Cov(A_ES,B_EO) +",
      "Cov(B_ES,A_EO) + Cov(B_ES,B_EO) (eq:joint_cov), A = summed contributions of shared",
      "components, B = the rest. Pooled shares weight each pair's covariances by n - 1."
    ),
    pooled_all_pairs = cov_pool(cov_pair),
    by_stage = cov_by_stage,
    by_pair = cov_pair %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with(c("cov_", "share_")), ~ signif(.x, 4)))
  ),
  within_country = list(
    description = paste(
      "Over each country's Strategic Index cells (pillar levels): Pearson and Spearman",
      "correlation of EO and ES (that of EO and 1 - ES is its negative), SD of Delta = EO - ES,",
      "and 2 Cov(EO,ES) / (Var EO + Var ES), the share of the variance sum removed from Var(Delta)."
    ),
    min_cells = min_wc_cells,
    countries_dropped_fewer_than_min_cells = within_country_dropped,
    n_countries = nrow(within_country),
    n_cells_range = range(within_country$n),
    n_cells_median = stats::median(within_country$n),
    n_countries_with_fewer_than_10_cells = sum(within_country$n < 10),
    n_countries_cov_negative = sum(within_country$cov < 0),
    countries_cov_negative = within_country$Country[within_country$cov < 0],
    coalition_members = list(
      n = sum(within_country$Country %in% coalition$Country),
      pearson_eo_es = wc_summary("pearson", within_country[within_country$Country %in% coalition$Country, ]),
      sd_delta = wc_summary("sd_delta", within_country[within_country$Country %in% coalition$Country, ]),
      share_removed_by_cov = wc_summary("share_removed_by_cov",
                                        within_country[within_country$Country %in% coalition$Country, ])
    ),
    pearson_eo_es = wc_summary("pearson"),
    spearman_eo_es = wc_summary("spearman"),
    sd_delta = wc_summary("sd_delta"),
    sd_pillar_part_0.3_delta_mean = r3(0.3 * mean(within_country$sd_delta)),
    sd_eo = wc_summary("sd_eo"),
    sd_es = wc_summary("sd_es"),
    share_removed_by_cov = wc_summary("share_removed_by_cov"),
    n_countries_cov_positive = sum(within_country$cov > 0),
    pair_means_pearson_across_pairs = r3(stats::cor(pair_means$eo_bar, pair_means$es_bar)),
    pair_means_spearman_across_pairs = r3(spearman(pair_means$eo_bar, pair_means$es_bar)),
    n_pairs_pair_means = nrow(pair_means),
    pearson_eo_es_pair_demeaned = wc_summary("pearson_demeaned"),
    sd_delta_pair_demeaned = wc_summary("sd_delta_demeaned"),
    by_country = within_country %>%
      dplyr::transmute(Country, n, pearson = r3(pearson), spearman = r3(spearman),
                       sd_delta = r3(sd_delta), share_removed_by_cov = r3(share_removed_by_cov),
                       pearson_pair_demeaned = r3(pearson_demeaned),
                       sd_delta_pair_demeaned = r3(sd_delta_demeaned))
  ),
  es_eo_spearman = list(
    pooled_levels_all_cells = r3(spearman(joint$es, joint$eo)),
    pooled_within_pair_ranks_all_cells = pooled_within(joint),
    pooled_within_pair_ranks_all_cells_4dp = round(stats::cor(joint$es_pr, joint$eo_pr), 4),
    within_pair_median = r3(rho_q[2]),
    # Four decimals, so that the two-decimal value printed in the table and the text can be
    # checked (0.5449 prints as 0.54, 0.545 would suggest 0.55).
    within_pair_median_4dp = round(rho_q[2], 4),
    within_pair_iqr = r3(c(rho_q[1], rho_q[3])),
    within_pair_min = list(pair = lo$pair, rho = r3(lo$rho)),
    within_pair_max = list(pair = hi$pair, rho = r3(hi$rho)),
    n_pairs_negative = sum(pair_stats$rho < 0),
    pairs_negative = pair_stats$pair[pair_stats$rho < 0],
    n_pairs_above_0_5 = sum(pair_stats$rho > 0.5),
    by_stage = stage_stats %>% dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ round(.x, 4))),
    by_technology = tech_stats,
    by_pair = pair_stats %>%
      dplyr::transmute(pair, n, rho = r3(rho), rho_nonshared = r3(rho_nonshared),
                       rho_es_vs_eo_nonshared = r3(rho_es_vs_eo_nonshared),
                       share_opp_wo_sec = r3(share_opp_wo_sec), share_leaders = r3(share_leaders),
                       share_sec_wo_opp = r3(share_sec_wo_opp), share_laggards = r3(share_laggards),
                       implied_offdiag_gaussian_copula = r3(implied_offdiag),
                       es_tied_at_median = r3(es_tied_at_median),
                       eo_tied_at_median = r3(eo_tied_at_median)),
    tech_stage_fixed_effects_r2 = list(
      es_levels = r3(fe_r2(joint$es, joint$pair)),
      eo_levels = r3(fe_r2(joint$eo, joint$pair))
    )
  ),
  nonshared = list(
    rule = paste(
      "A component is shared when, in the same category, the other pillar has a component whose",
      "observed (non-imputed) values are identical (|diff| < 1e-9) in >= 99% of >= 30 jointly",
      "observed cells. Non-shared pillars are the weighted means of the remaining components with",
      "effective weights renormalised; undefined where every component of a pillar is shared."
    ),
    shared_component_pairs = shared_map %>% dplyr::mutate(share_identical = r3(share_identical)),
    category_sharing = cat_sharing,
    n_pairs_nonshared_defined = nrow(pairs_ns),
    pairs_nonshared_undefined = pair_stats$pair[is.na(pair_stats$rho_nonshared)],
    within_pair_median_nonshared = r3(rho_ns_q[2]),
    within_pair_iqr_nonshared = r3(c(rho_ns_q[1], rho_ns_q[3])),
    within_pair_median_full_same_pairs = r3(rho_full_same),
    within_pair_median_nonshared_4dp = round(rho_ns_q[2], 4),
    within_pair_median_full_same_pairs_4dp = round(rho_full_same, 4),
    proportional_reduction_in_median = r3(1 - rho_ns_q[2] / rho_full_same),
    within_pair_median_es_full_vs_eo_nonshared = r3(stats::median(pair_stats$rho_es_vs_eo_nonshared)),
    trade_check_by_stage = pairs_ns %>%
      dplyr::group_by(supply_chain = as.character(.data$supply_chain)) %>%
      dplyr::summarise(
        n_pairs = dplyr::n(),
        median_nonshared = round(stats::median(.data$rho_nonshared), 4),
        n_pairs_eo_without_trade_defined = sum(!is.na(.data$rho_nonshared_eo_without_trade)),
        median_nonshared_eo_without_trade = round(stats::median(.data$rho_nonshared_eo_without_trade,
                                                                na.rm = TRUE), 4),
        .groups = "drop"
      ),
    overlap_es = overlap_summary(overlap_es),
    overlap_eo = overlap_summary(overlap_eo)
  ),
  quadrants = list(
    pooled_all_cells = quad_pooled,
    by_stage = quad_stage,
    by_technology = quad_tech,
    coalition_cells = quad_coalition,
    coalition_cells_by_tier = quad_by_tier,
    pairs_with_quarter_or_more_tied_at_median = tie_pairs,
    pooled_all_cells_excluding_tied_pairs = quad_share(joint_no_tie),
    coalition_cells_excluding_tied_pairs = quad_share(joint_no_tie %>% dplyr::filter(.data$coalition)),
    top_pairs_by_opp_wo_sec_share = top_opp_pairs,
    tie_convention_sensitivity = list(
      description = paste(
        "Region shares when a cell tied at its pair's median is counted as high (>= median)",
        "instead of low (> median, the convention used everywhere else). by_pair lists the pairs",
        "in which at least 5% of cells are tied at the ES or EO median; es_all_imputed_excl_prices",
        "counts tied cells whose ES components other than the (pair-constant) price-volatility",
        "index are all imputed."
      ),
      pooled_all_cells_ties_high = quad_share_ties_high(joint),
      coalition_cells_ties_high = quad_share_ties_high(joint %>% dplyr::filter(.data$coalition)),
      by_stage_opp_wo_sec_ties_high = joint %>%
        dplyr::group_by(supply_chain = as.character(.data$supply_chain)) %>%
        dplyr::summarise(share_opp_wo_sec = r3(mean(.data$quadrant == "Opportunity without security")),
                         share_opp_wo_sec_ties_high =
                           r3(mean(.data$quadrant_ties_high == "Opportunity without security")),
                         .groups = "drop"),
      by_pair = tie_by_pair
    ),
    gaussian_copula_check = list(
      mean_actual_offdiag_share = r3(mean((pair_stats$share_opp_wo_sec + pair_stats$share_sec_wo_opp) / 2)),
      mean_implied_offdiag_share = r3(mean(pair_stats$implied_offdiag)),
      corr_actual_vs_implied_opp_wo_sec = r3(stats::cor(pair_stats$share_opp_wo_sec, pair_stats$implied_offdiag))
    )
  ),
  coalition = list(
    profiles = coal_profile,
    max_n_opp_wo_sec = max(coal_profile$n_opp_wo_sec),
    members_with_most_opp_wo_sec = coal_profile %>%
      dplyr::filter(.data$n_opp_wo_sec >= sort(coal_profile$n_opp_wo_sec, decreasing = TRUE)[3]) %>%
      dplyr::select("iso3c", "Country", "tier", "n_sectors", "n_opp_wo_sec", "share_opp_wo_sec",
                    "opp_wo_sec_sectors"),
    members_with_highest_share_opp_wo_sec = coal_profile %>%
      dplyr::slice_max(.data$share_opp_wo_sec, n = 5) %>%
      dplyr::select("iso3c", "Country", "tier", "n_sectors", "n_opp_wo_sec", "share_opp_wo_sec"),
    members_with_most_opp_wo_sec_by_tier = coal_profile %>%
      dplyr::group_by(tier = as.character(.data$tier)) %>%
      dplyr::slice_max(.data$n_opp_wo_sec, n = 3) %>%
      dplyr::ungroup() %>%
      dplyr::select("tier", "iso3c", "Country", "n_sectors", "n_opp_wo_sec", "opp_wo_sec_sectors"),
    members_with_zero_opp_wo_sec = coal_profile$Country[coal_profile$n_opp_wo_sec == 0],
    n_members_with_zero_opp_wo_sec = sum(coal_profile$n_opp_wo_sec == 0),
    n_members_with_zero_opp_wo_sec_by_tier = coal_profile %>%
      dplyr::group_by(tier = as.character(.data$tier)) %>%
      dplyr::summarise(n_zero = sum(.data$n_opp_wo_sec == 0), n_members = dplyr::n(),
                       .groups = "drop"),
    n_members_with_green_hydrogen_midstream_in_opp_wo_sec =
      sum(grepl("Green Hydrogen - Midstream", coal_profile$opp_wo_sec_sectors)),
    n_sectors_range = range(coal_profile$n_sectors),
    n_sectors_range_by_tier = coal_profile %>%
      dplyr::group_by(tier = as.character(.data$tier)) %>%
      dplyr::summarise(min = min(.data$n_sectors), median = stats::median(.data$n_sectors),
                       max = max(.data$n_sectors), .groups = "drop"),
    members_with_fewest_sectors = coal_profile %>% dplyr::slice_min(.data$n_sectors, n = 5) %>%
      dplyr::select("Country", "tier", "n_sectors"),
    members_with_most_leaders = coal_profile %>% dplyr::slice_max(.data$n_leaders, n = 3) %>%
      dplyr::select("Country", "n_sectors", "n_leaders"),
    members_with_highest_share_leaders = coal_profile %>%
      dplyr::slice_max(.data$share_leaders, n = 3) %>%
      dplyr::select("Country", "n_sectors", "n_leaders", "share_leaders"),
    members_with_most_sec_wo_opp = coal_profile %>% dplyr::slice_max(.data$n_sec_wo_opp, n = 3) %>%
      dplyr::select("Country", "n_sectors", "n_sec_wo_opp"),
    median_member_share_opp_wo_sec = r3(stats::median(coal_profile$share_opp_wo_sec)),
    # From the unrounded member shares (coal_profile's shares are rounded to three decimals).
    median_member_share_opp_wo_sec_by_tier = coal_profile %>%
      dplyr::group_by(tier = as.character(.data$tier)) %>%
      dplyr::summarise(
        median_share_opp_wo_sec = round(stats::median(.data$n_opp_wo_sec / .data$n_sectors), 4),
        median_share_leaders = round(stats::median(.data$n_leaders / .data$n_sectors), 4),
        .groups = "drop"),
    all_rankable_cells_share_opp_wo_sec = r3(all_econ_opp_share),
    pairs_by_n_members_in_opp_wo_sec = coal_opp_tech %>% dplyr::slice_head(n = 8),
    allied_picks = list(
      description = paste(
        "Each member's allied pick (data/processed/charts/allied_strategic.csv: top Strategic Index",
        "among Upstream/Midstream sectors, Geothermal and Green Hydrogen excluded, endowment screen",
        "applied), placed in the region of the space it occupies; pool = the pick-eligible SI",
        "cells of the members that have a pick."
      ),
      n_picks = nrow(picks_q),
      n_picks_by_stage = picks_q %>% dplyr::count(supply_chain = .data$supply_chain),
      quadrant_counts = picks_quad_counts,
      share_opp_wo_sec = r3(mean(picks_q$quadrant == "Opportunity without security")),
      members_opp_wo_sec = sort(picks_q$iso3[picks_q$quadrant == "Opportunity without security"]),
      by_pair = picks_by_pair_quadrant,
      pool_n = nrow(pick_pool_q),
      pool_quadrant_shares = quad_share(pick_pool_q),
      eligible_pool_size_range = range(picks_q$n_eligible),
      eligible_pool_size_median = stats::median(picks_q$n_eligible),
      # Midstream nuclear, the pick of seven members: which categories its ES is built from and
      # how its rankable ES values are spread (quartiles), to read the picks' positions.
      nuclear_midstream_es = {
        nm <- joint %>% dplyr::filter(.data$tech == "Nuclear",
                                      as.character(.data$supply_chain) == "Midstream")
        list(es_categories = sort(unique(var_es$category[var_es$tech == "Nuclear" &
                                                           var_es$supply_chain == "Midstream"])),
             n_rankable = nrow(nm),
             es_quartiles = r3(stats::quantile(nm$es, c(0.25, 0.5, 0.75), names = FALSE)),
             es_range = r3(range(nm$es)))
      },
      picks = picks_q %>%
        dplyr::transmute(iso3, tier, pair = paste(tech, supply_chain, sep = " - "),
                         strategic_index = r3(strategic_index), es_pr = r3(es_pr),
                         eo_pr = r3(eo_pr), quadrant = as.character(quadrant), n_eligible,
                         eligible_sectors = ifelse(n_eligible <= 5, eligible_sectors, NA_character_))
    ),
    # The technology term of the Strategic Index (0.1 x ghg_index), by technology.
    si_technology_weight = si_tbl %>%
      dplyr::filter(!is.na(.data$strategic_index)) %>%
      dplyr::distinct(.data$tech, .data$tech_weight) %>%
      dplyr::mutate(tech_weight = r3(.data$tech_weight)) %>%
      dplyr::arrange(dplyr::desc(.data$tech_weight)),
    strategic_index = list(
      n_si_cells = n_si_cells,
      n_si_cells_outside_joint_sample = n_si_cells_outside_joint,
      n_si_cells_in_sample = nrow(si_q),
      n_si_countries = dplyr::n_distinct(si_q$Country),
      n_si_coalition_members = dplyr::n_distinct(si_q$Country[si_q$coalition]),
      share_si_cells_opp_wo_sec = si_share_opp(si_q),
      share_top1_si_sector_opp_wo_sec = si_share_opp(top1),
      share_top3_si_sectors_opp_wo_sec = si_share_opp(top3),
      share_top5_si_sectors_opp_wo_sec = si_share_opp(top5),
      top1_quadrant_counts = quad_counts(top1),
      top1_by_pair = top1 %>% dplyr::count(pair = paste(.data$tech, .data$supply_chain, sep = " - "),
                                           name = "n_countries") %>%
        dplyr::arrange(dplyr::desc(.data$n_countries)),
      top1_nuclear_upstream_quadrant_counts = quad_counts(
        top1 %>% dplyr::filter(.data$tech == "Nuclear", .data$supply_chain == "Upstream")),
      # The same top-ranked sector after the endowment screen (cells flagged "no resource base"
      # out), all stages: the screen is a pick-eligibility rule, applied here only to show how
      # much of the top-sector pattern runs through extraction stages without a resource base.
      top1_screened_n_countries = dplyr::n_distinct(
        si_q$Country[is.na(si_q$pick_exclusion)]),
      top1_screened_quadrant_counts = quad_counts(
        si_q %>% dplyr::filter(is.na(.data$pick_exclusion)) %>%
          dplyr::group_by(.data$Country) %>%
          dplyr::slice_max(order_by = .data$strategic_index, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup()),
      top1_nuclear_upstream_at_es_floor = sum(
        top1$tech == "Nuclear" & top1$supply_chain == "Upstream" &
          top1$Country %in% joint$Country[joint$tech == "Nuclear" &
                                            as.character(joint$supply_chain) == "Upstream" &
                                            joint$es_at_median]),
      coalition_members_only = list(
        n_members = nrow(top1_coal),
        n_si_cells = sum(si_q$coalition),
        share_si_cells_opp_wo_sec = si_share_opp(si_q %>% dplyr::filter(.data$coalition)),
        top1_quadrant_counts = quad_counts(top1_coal),
        share_top1_si_sector_opp_wo_sec = si_share_opp(top1_coal),
        share_top3_si_sectors_opp_wo_sec = si_share_opp(top3_coal),
        mean_si_by_quadrant = si_q %>% dplyr::filter(.data$coalition) %>%
          dplyr::group_by(quadrant = as.character(.data$quadrant)) %>%
          dplyr::summarise(n = dplyr::n(), mean_si = r3(mean(.data$strategic_index)),
                           .groups = "drop")
      ),
      mean_si_by_quadrant = si_mean_by_quad,
      min_cells_for_within_country_statistics = min_wc_cells,
      n_countries_within_country_statistics = nrow(si_gap),
      top1_sectors = top1 %>% dplyr::transmute(Country, pair = paste(tech, supply_chain, sep = " - "),
                                               strategic_index = r3(strategic_index),
                                               quadrant = as.character(quadrant)),
      within_country_spearman_si_vs_rank_gap_median = r3(stats::median(si_gap$rho_rank_gap)),
      within_country_spearman_si_vs_rank_gap_range = r3(range(si_gap$rho_rank_gap)),
      within_country_spearman_si_vs_level_gap_median = r3(stats::median(si_gap$rho_level_gap)),
      within_country_spearman_eo_vs_need_mean = r3(mean(si_eo_need$rho)),
      within_country_spearman_eo_vs_need_median = r3(stats::median(si_eo_need$rho)),
      within_country_spearman_eo_vs_need_range = r3(range(si_eo_need$rho)),
      within_country_spearman_eo_vs_need_extremes = si_eo_need %>%
        dplyr::filter(.data$rho %in% range(.data$rho)) %>%
        dplyr::transmute(Country, n, rho = r3(rho))
    )
  ),
  category_correlations = list(
    method = paste(
      "Spearman correlation across countries within each technology-stage pair in which both",
      "categories are present and vary (>= 20 countries), averaged with equal weights across pairs."
    ),
    categories_dropped_constant_within_every_pair = cats_constant_everywhere$label,
    categories_shown = var_order,
    n_pairs_per_tile_range = range(cat_corr$n_pairs[!is.na(cat_corr$mean_rho)]),
    cross_pillar_same_category = cross_same_category,
    eo_only_categories_vs_es = eo_only_vs_es,
    cross_pillar_different_category = list(
      n = nrow(cross_diff_category),
      median_mean_rho = r3(stats::median(cross_diff_category$mean_rho)),
      median_mean_rho_4dp = round(stats::median(cross_diff_category$mean_rho), 4),
      max = cross_diff_category %>% dplyr::slice_max(.data$mean_rho, n = 3) %>%
        dplyr::transmute(var1, var2, n_pairs, mean_rho = r3(mean_rho)),
      min = cross_diff_category %>% dplyr::slice_min(.data$mean_rho, n = 3) %>%
        dplyr::transmute(var1, var2, n_pairs, mean_rho = r3(mean_rho))
    ),
    production_vs_reserves_es_by_pair = cat_wide %>%
      dplyr::filter(!is.na(.data$`Production (ES)`), !is.na(.data$`Reserves (ES)`)) %>%
      dplyr::group_by(pair = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
      dplyr::summarise(n = dplyr::n(),
                       rho = r3(spearman(.data$`Production (ES)`, .data$`Reserves (ES)`)),
                       share_zero_on_both = r3(mean(.data$`Production (ES)` == 0 &
                                                      .data$`Reserves (ES)` == 0)),
                       .groups = "drop") %>%
      dplyr::filter(.data$n >= min_pair_n),
    # Upstream pairs: whether EO has a Trade category (an HS6 basket for the stage), and the
    # within-pair Spearman correlation of ES Minerals Trade with EO Trade where both exist,
    # next to the pair's ES-EO correlation.
    upstream_trade_link = cat_wide %>%
      dplyr::filter(.data$supply_chain == "Upstream") %>%
      dplyr::group_by(.data$tech) %>%
      dplyr::summarise(
        n = dplyr::n(),
        eo_has_trade = any(!is.na(.data$`Trade (EO)`)),
        rho_minerals_trade_es_vs_trade_eo = r3(spearman(.data$`Minerals Trade (ES)`, .data$`Trade (EO)`)),
        .groups = "drop"
      ) %>%
      dplyr::left_join(pair_stats %>% dplyr::filter(as.character(.data$supply_chain) == "Upstream") %>%
                         dplyr::transmute(tech, rho_es_eo = r3(rho)), by = "tech") %>%
      dplyr::arrange(dplyr::desc(.data$rho_es_eo)),
    # The same, summarised: the mean over the upstream pairs (the Production-Reserves tile of
    # joint_corr) and the range over the six pairs built on critical minerals (all but nuclear).
    production_vs_reserves_es_summary = {
      pr <- cat_wide %>%
        dplyr::filter(!is.na(.data$`Production (ES)`), !is.na(.data$`Reserves (ES)`)) %>%
        dplyr::group_by(.data$tech, .data$supply_chain) %>%
        dplyr::summarise(n = dplyr::n(),
                         rho = spearman(.data$`Production (ES)`, .data$`Reserves (ES)`),
                         share_zero_on_both = mean(.data$`Production (ES)` == 0 &
                                                     .data$`Reserves (ES)` == 0),
                         .groups = "drop") %>%
        dplyr::filter(.data$n >= min_pair_n, !is.na(.data$rho))
      minerals <- pr %>% dplyr::filter(.data$tech != "Nuclear")
      list(n_pairs = nrow(pr), stages = unique(pr$supply_chain), mean_rho = r3(mean(pr$rho)),
           n_mineral_pairs = nrow(minerals), mineral_pairs_rho_range = r3(range(minerals$rho)),
           mineral_pairs_mean_rho = r3(mean(minerals$rho)),
           mineral_pairs_share_zero_on_both_range = r3(range(minerals$share_zero_on_both)))
    },
    # Upstream nuclear: the economies tied at the modal value of ES (Energy Prices plus an imputed
    # Trade category) are a subset of those with neither recorded uranium resources nor production
    # (both category scores zero); the rest of these trade uranium and score near the mode.
    nuclear_upstream_mode = {
      nu <- joint %>%
        dplyr::filter(.data$tech == "Nuclear", .data$supply_chain == "Upstream") %>%
        dplyr::select(dplyr::all_of(keys), "es", "es_at_median", "eo_hi") %>%
        dplyr::mutate(supply_chain = as.character(.data$supply_chain)) %>%
        dplyr::left_join(cat_wide %>% dplyr::select(dplyr::all_of(keys), "Production (ES)",
                                                    "Reserves (ES)"), by = keys) %>%
        dplyr::mutate(zero_both = .data$`Production (ES)` %in% 0 & .data$`Reserves (ES)` %in% 0)
      es_r <- round(nu$es, 10)
      mode_val <- as.numeric(names(which.max(table(es_r))))
      at_mode <- es_r == mode_val
      list(n_rankable = nrow(nu), mode_value = r3(mode_val),
           mode_is_median = all(nu$es_at_median == at_mode),
           n_at_mode = sum(at_mode),
           n_zero_production_and_reserves = sum(nu$zero_both),
           n_at_mode_and_zero_both = sum(at_mode & nu$zero_both),
           n_at_mode_eo_above_median = sum(at_mode & nu$eo_hi),
           es_range_zero_both = r3(range(nu$es[nu$zero_both])),
           min_es = r3(min(nu$es)))
    },
    matrix = cat_corr %>% dplyr::mutate(mean_rho = r3(mean_rho))
  ),
  scatter_median_line = list(
    description = paste(
      "joint_scatter draws the region boundaries at percentile rank 0.5; cells tied at a median",
      "count as low but are plotted at the mean rank of their tie group. Cells plotted on the",
      "other side of a line from the region they are counted in."
    ),
    n_cells = nrow(line_mis),
    share_of_cells = r3(nrow(line_mis) / nrow(joint)),
    n_cells_es_side = sum(line_mis$mis_es),
    n_cells_eo_side = sum(line_mis$mis_eo),
    by_pair = line_mis %>% dplyr::count(.data$pair, name = "n_cells"),
    plotted_rank_range = r3(range(c(line_mis$es_pr[line_mis$mis_es], line_mis$eo_pr[line_mis$mis_eo])))
  ),
  scatter_labels = lab_pts %>%
    dplyr::transmute(stage = as.character(supply_chain), iso3c, pair,
                     es_pr = r3(es_pr), eo_pr = r3(eo_pr), quadrant = as.character(quadrant))
)

write_paper_results(results, "joint")

# ---------------------------------------------------------------------------------------------
# Table: the ES-EO relation by stage (written here so the table cannot drift from the JSON)
# ---------------------------------------------------------------------------------------------

# Every cell of the table is rounded once, from unrounded inputs (rounding a value that is already
# rounded to three decimals can go the wrong way).
f2 <- function(x) ifelse(is.na(x), "--", sub("^-", "$-$", sprintf("%.2f", round(x, 2) + 0)))
pc <- function(x) sprintf("%.1f", 100 * x)
w_shared <- function(o, stage = NULL) {
  if (!is.null(stage)) o <- o[o$supply_chain == stage, ]
  mean(o$w_shared)
}
quad_raw <- function(d) {
  as.numeric(table(factor(as.character(d$quadrant), levels = quad_levels))) / nrow(d)
}
col_vals <- function(stage) {
  if (stage == "All") {
    cp <- cov_pool(cov_pair, rnd = identity)
    list(
      pairs = nrow(pair_stats), cells = nrow(joint),
      med = stats::median(pair_stats$rho), mn = min(pair_stats$rho), mx = max(pair_stats$rho),
      pooled = stats::cor(joint$es_pr, joint$eo_pr),
      ns_pairs = nrow(pairs_ns), ns_med = rho_ns_q[2], ns_full = rho_full_same,
      w_es = w_shared(overlap_es), w_eo = w_shared(overlap_eo),
      cov_aa = cp$share_shared_term_AA, cov_bb = cp$share_nonshared_term_BB,
      q = quad_raw(joint)
    )
  } else {
    r <- stage_stats[stage_stats$supply_chain == stage, ]
    cp <- cov_pool(cov_pair[cov_pair$supply_chain == stage, ], rnd = identity)
    list(
      pairs = r$n_pairs, cells = r$n_cells,
      med = r$median_within_pair_spearman, mn = r$min_within_pair_spearman,
      mx = r$max_within_pair_spearman, pooled = r$pooled_within_pair_spearman,
      ns_pairs = r$n_pairs_nonshared_defined, ns_med = r$median_within_pair_spearman_nonshared,
      ns_full = r$median_within_pair_spearman_full_same_pairs,
      w_es = w_shared(overlap_es, stage), w_eo = w_shared(overlap_eo, stage),
      cov_aa = cp$share_shared_term_AA, cov_bb = cp$share_nonshared_term_BB,
      q = quad_raw(joint[as.character(joint$supply_chain) == stage, ])
    )
  }
}
tab_cols <- lapply(c(stage_levels, "All"), col_vals)
# The notes say that the pairs without non-shared pillars are all downstream, and name the pairs
# Energy Security does not score.
stopifnot(all(as.character(pair_stats$supply_chain[is.na(pair_stats$rho_nonshared)]) == "Downstream"),
          setequal(paste(pairs_eo_only$tech, pairs_eo_only$supply_chain),
                   c("Electric Grid Downstream", "Geothermal Upstream", "Geothermal Midstream")),
          nrow(flagged_pairs) == 0)
tab_row <- function(label, fun) {
  paste0(label, " & ", paste(vapply(tab_cols, fun, ""), collapse = " & "), " \\\\")
}
tab <- c(
  "% Generated by scripts/paper/fig_joint.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Energy Security and Economic Opportunity within technology-stage pairs, by stage}",
  "\\label{tab:joint_stage}",
  "\\footnotesize",
  "\\begin{tabular}{@{}lcccc@{}}",
  "\\toprule",
  " & Upstream & Midstream & Downstream & All \\\\",
  "\\midrule",
  tab_row("Technology-stage pairs", function(v) as.character(v$pairs)),
  tab_row("Country cells", function(v) format(v$cells, big.mark = ",")),
  "\\addlinespace",
  "\\multicolumn{5}{@{}l}{\\textit{Spearman correlation of $\\ES_{its}$ and $\\EO_{its}$ within pairs}}\\\\",
  tab_row("\\quad Median across pairs", function(v) f2(v$med)),
  tab_row("\\quad Lowest pair", function(v) f2(v$mn)),
  tab_row("\\quad Highest pair", function(v) f2(v$mx)),
  tab_row("\\quad Pooled within-pair ranks", function(v) f2(v$pooled)),
  "\\addlinespace",
  "\\multicolumn{5}{@{}l}{\\textit{Components shared by the two pillars}}\\\\",
  tab_row("\\quad Effective weight in $\\ES$", function(v) f2(v$w_es)),
  tab_row("\\quad Effective weight in $\\EO$", function(v) f2(v$w_eo)),
  tab_row("\\quad Pairs with both non-shared pillars defined", function(v) as.character(v$ns_pairs)),
  tab_row("\\quad Median correlation, non-shared pillars", function(v) f2(v$ns_med)),
  tab_row("\\quad Median correlation, full pillars, same pairs", function(v) f2(v$ns_full)),
  tab_row("\\quad Share of level covariance, shared term", function(v) f2(v$cov_aa)),
  tab_row("\\quad Share of level covariance, non-shared term", function(v) f2(v$cov_bb)),
  "\\addlinespace",
  "\\multicolumn{5}{@{}l}{\\textit{Share of cells by region of the space (percent)}}\\\\",
  tab_row("\\quad Opportunity without security", function(v) pc(v$q[1])),
  tab_row("\\quad Leaders (both high)", function(v) pc(v$q[2])),
  tab_row("\\quad Security without opportunity", function(v) pc(v$q[3])),
  tab_row("\\quad Laggards (both low)", function(v) pc(v$q[4])),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste(
    "\\item \\textit{Notes:} Eleven headline technologies; cells rankable on both pillars only",
    "(a sovereign economy with at most half of the pillar's weight imputed), which leaves",
    format(nrow(joint), big.mark = ","), "of the", format(nrow(joint_scored), big.mark = ","),
    "cells that both pillars score, for", dplyr::n_distinct(joint$Country), "economies.",
    "A technology-stage pair enters when both pillars score it and neither pillar's diagnostics",
    "flag it as constant or unscored, which leaves", nrow(pair_stats), "of the",
    nrow(dplyr::distinct(pairs_eo, tech, supply_chain)), "pairs: Energy Security does not score",
    "downstream grid or upstream and midstream geothermal.",
    "Correlations are Spearman correlations across economies within a pair; the pooled row is the",
    "Pearson correlation of within-pair percentile ranks over all cells of the stage. A component",
    "is shared when the other pillar contains a component of the same category with identical",
    "observed values; its effective weight is its category weight times its within-category",
    "weight, averaged over cells. Non-shared pillars are the weighted means of the remaining",
    "components with the weights renormalised; they are undefined where every component of",
    "$\\ES$ is shared, as in", sum(is.na(pair_stats$rho_nonshared)), "downstream pairs, and the",
    "last two correlation rows use only",
    "the pairs in which both are defined. The covariance shares decompose the within-pair",
    "covariance of the pillar levels as in equation~\\eqref{eq:joint_cov}, pooled over pairs with",
    "weights $n-1$: the shared term is $\\operatorname{Cov}(A^{\\ES},A^{\\EO})$, the non-shared term",
    "$\\operatorname{Cov}(B^{\\ES},B^{\\EO})$, and the two cross terms make up the remainder.",
    "Regions are defined by the within-pair medians of the",
    "two pillars, a cell at the median counting as low. Source: \\texttt{index\\_outputs.rds};",
    "\\texttt{scripts/paper/fig\\_joint.R}."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(tab, file.path(tab_dir, "joint_stage.tex"))
message("fig_joint.R: wrote paper/tables/joint_stage.tex")

message("fig_joint.R: wrote 4 figures and paper/results/joint.json")
