# Appendix D, closing subsection: reconciliation of the shipped September 2026 release with
# the final corrected rerun (the full pipeline run of 12 October 2026: scripts 04
# 10 12 13 20 15 25, then 30 at the final diversification weight lambda = 0.78 and 40).
#
# Theme key: "reconcile". Emits
#   paper/figures/reconcile_scatter.pdf   shipped against corrected ES, EO and SI, cell by cell
#   paper/figures/reconcile_cells.pdf     level shift against within-cell rank agreement, by
#                                         technology-stage cell (rankable cells)
#   paper/figures/reconcile_movers.pdf    the largest ES and EO changes among the members of the
#                                         final coalition (at most two per technology-stage cell)
#                                         and their attributed cause
#   paper/figures/reconcile_si_terms.pdf  the largest SI changes among coalition members, split
#                                         exactly into the five terms of the index
#   paper/tables/reconcile_movers.tex     the ES and EO movers with the category decomposition
#   paper/tables/reconcile_allied.tex     shipped against corrected allied pick, per member of the
#                                         shipped (hand-picked) list
#   paper/tables/reconcile_network.tex    shipped against final network diagnostics
#   paper/tables/reconcile_coverage.tex   rows, economies, cells and rankable cells before and after
#   paper/results/reconcile.json          every number quoted in the text
#
# Inputs (read only).
#   Corrected: data/processed/outputs/index_outputs.rds (pillars with imputed_weight_share,
#   rankable and rank_exclusion; strategic_index with resource_base and pick_exclusion;
#   coalition_members), data/processed/charts/allied_strategic.csv,
#   data/processed/outputs/allied_network_{diagnostics,observed_hhi}.csv, data/processed/psi_tbl.rds
#   (core view), data/processed/psi_world_tbl.rds (world view) and the reporter column of
#   data/raw/comtrade_energy_dyads_all.csv (which economies reported 2024 imports), all of this
#   worktree (config$processed_dir, config$raw_data_dir).
#   Shipped: the same index, chart and diagnostics artefacts under data/processed of the
#   repository's main checkout, plus its March 2026 partner_{friendshore,opportunity,
#   development}_top5_tbl.rds (the earlier PSI was never written to disk and is reconstructed from
#   them, item B4). The main checkout is found through git (git rev-parse --git-common-dir) or set
#   with the environment variable OSI_SHIPPED_ROOT. The script stops unless the shipped
#   index_outputs.rds was written on 3 September 2026, so an overwritten release is never
#   reconciled by mistake. Only the small shipped network diagnostics file is read, never the
#   shipped flows.
#
# Coalitions. The shipped allied selection ran over a hand-picked list of 23 economies (with the
# UAE coded "UAE"). The final coalition is derived (scripts/13_build_coalition.R): 57 members in
# two tiers (31 core, 26 partners), read from index_outputs$coalition_members. Movers are
# reported for the final coalition; the allied picks are compared on the members the two lists
# share (21), and the members of one list only are named.
#
# Matching. Shipped country names are mapped to canonical names with standardize_country_names()
# (R/utils/country.R, pure). Non-country units (aggregates, residual and historical entities,
# which resolve to no ISO3 code) are dropped. Where two shipped spellings of one economy map to the
# same (country, technology, stage) key, the record under the canonical spelling is kept, as the
# corrected standardiser does (item C1); the others are counted as merged. Cells are matched on
# (canonical country, technology, stage) in the headline tables (11 technologies), and SI cells on
# (canonical country, sector).
#
# Rankability. The final release ranks a pillar cell only if the economy is sovereign and at most
# tau = 0.5 of its index weight is imputed (config/rankability.yml; column rankable). Rank
# statistics (pooled and within-cell Spearman correlations, rank shifts) are computed over the
# matched cells that are rankable in the corrected release; the same statistics over all matched
# cells are reported alongside. The shipped release had no such rule.
#
# Constant cells. A technology-stage cell whose pillar has zero range across two or more countries
# (the corrected diagnostics' index_cells status "constant", and the same test on the shipped
# table) is excluded from every correlation and rank comparison. Movers include cells that were
# constant in the shipped release (their shipped level is a number a reader may hold) but not cells
# constant in the corrected one (there are none). Correlations are Spearman unless labelled Pearson.
#
# Attribution of a pillar change. For a cell, I = sum_k w_k c_k with w_k the normalised category
# weight of the release (shipped unobserved categories count as c_k = 0 at full weight, which is
# how the shipped index was computed, item A1). The change is split, by a two-factor Shapley
# decomposition, into a score effect of each category present in both releases, 0.5 (w_k^old +
# w_k^new)(c_k^new - c_k^old), and a composition effect, the remainder, which comes from
# categories added or removed and from the renormalisation of the weights. The largest of these
# effects names the cause; the label adds the data change behind the category where one applies
# (uranium resources and production from the Red Book in Nuclear Upstream; critical-mineral
# reserves from USGS MCS 2026; Trade from all-reporter Comtrade dyads and the rebuilt energy-trade
# file, with the reviewed HS6 basket where the review changed the stage's basket and with
# partners' mirror declarations where the economy did not report 2024 imports; TRL mapped by
# stage).
#
# Movers. The figure and table show the largest absolute changes among coalition members with at
# most two per technology-stage cell, so that one cell-wide change (Nuclear Upstream, where the
# shipped ES was zero for every economy) does not fill the list; the uncapped ranking is recorded
# in the results.
#
# Attribution of an SI change. SI = 0.2 pol + 0.3 EO + 0.3 (1 - ES) + 0.1 stage weight + 0.1
# technology weight in both releases, so a change splits exactly into the five weighted term
# changes. The shipped pol was the within-country S-curve of a four-component policy composite
# (IEA PAMS, CAT, NIPO, dual-use); the final pol is that of IEA PAMS (weight 3, required) and CAT
# (weight 2, where CAT rates the country); GTA NIPO and the dual-use score are excluded from the
# final composite (their rows remain in policy_component_tbl but do not enter the index).
#
# Allied pick. Each member's highest-SI Upstream or Midstream sector; Geothermal and Green
# Hydrogen are excluded by rule of the selection (select_allied_strategic_sectors()), and in the
# final release an Upstream cell without a recorded resource base is not eligible (endowment
# screen; the SI values are unchanged).
#
# Run from the repository root:
#   Rscript scripts/paper/fig_reconcile.R

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

root <- paper_repo_root()
source(file.path(root, "R", "utils", "country.R"))

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
processed_dir <- file.path(root, config$processed_dir)
raw_dir <- file.path(root, config$raw_data_dir %||% "data/raw")

# The hand-picked list over which the shipped release made its allied selection.
shipped_list_iso3 <- c("USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA", "ITA",
                       "BRA", "SAU", "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK", "ARG", "MAR",
                       "CHL")
shipped_list <- country_name_from_iso3(shipped_list_iso3)
stages <- c("Upstream", "Midstream", "Downstream")
const_tol <- 1e-9
# The allied pick: Upstream and Midstream sectors; Geothermal and Green Hydrogen are excluded by
# rule of the selection (select_allied_strategic_sectors(), R/indices/build_indices.R).
pick_stages <- c("Upstream", "Midstream")
pick_excluded_techs <- c("Geothermal", "Green Hydrogen")
# SI weights (build_strategic_index(), R/indices/build_indices.R), identical in both releases.
si_w <- c(pol = 0.2, eo = 0.3, need = 0.3, sc = 0.1, tw = 0.1)

# ---------------------------------------------------------------------------------------------
# Locate and check the shipped release
# ---------------------------------------------------------------------------------------------

locate_shipped_root <- function(root) {
  env <- Sys.getenv("OSI_SHIPPED_ROOT", "")
  if (nzchar(env)) return(normalizePath(env, winslash = "/", mustWork = TRUE))
  common <- tryCatch(
    system2("git", c("-C", shQuote(root), "rev-parse", "--path-format=absolute", "--git-common-dir"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(common) != 1 || !dir.exists(common)) {
    stop("Cannot locate the main checkout; set OSI_SHIPPED_ROOT to the shipped release's repository.")
  }
  shipped <- normalizePath(dirname(common), winslash = "/", mustWork = TRUE)
  if (identical(shipped, normalizePath(root, winslash = "/"))) {
    stop("This is the main checkout; set OSI_SHIPPED_ROOT to the shipped release's repository.")
  }
  shipped
}

shipped_root <- locate_shipped_root(root)
shipped_processed <- file.path(shipped_root, config$processed_dir)
shipped_index_path <- file.path(shipped_processed, "outputs", "index_outputs.rds")
shipped_diag_path <- file.path(shipped_processed, "outputs", "allied_network_diagnostics.csv")
shipped_mtime <- file.info(shipped_index_path)$mtime
if (is.na(shipped_mtime) || format(shipped_mtime, "%Y-%m-%d") != "2026-09-03") {
  stop("The shipped index_outputs.rds is missing or was not written on 2026-09-03 (found ",
       format(shipped_mtime), "); it may have been overwritten. Not reconciling.")
}
shipped_diag_mtime <- file.info(shipped_diag_path)$mtime
corrected_index_path <- file.path(processed_dir, "outputs", "index_outputs.rds")
corrected_mtime <- file.info(corrected_index_path)$mtime

# ---------------------------------------------------------------------------------------------
# Load the two releases, keeping only what the comparison needs
# ---------------------------------------------------------------------------------------------

extract_release <- function(path) {
  x <- readRDS(path)
  keep_cc <- c("Country", "tech", "supply_chain", "category", "category_score", "imputed",
               "missing_rule_applied", "weight", "category_weight", "weighted_category_contribution")
  out <- list(
    es = x$energy_security_index,
    eo = x$economic_opportunity_index,
    es_cc = x$energy_security_outputs$category_contributions[, keep_cc],
    eo_cc = x$economic_opportunity_outputs$category_contributions[, keep_cc],
    si = x$strategic_index,
    es_cells = x$energy_security_outputs$diagnostics$index_cells,
    eo_cells = x$economic_opportunity_outputs$diagnostics$index_cells,
    es_imp = x$energy_security_outputs$diagnostics$index_imputation,
    eo_imp = x$economic_opportunity_outputs$diagnostics$index_imputation,
    es_imputed_share = x$energy_security_outputs$diagnostics$imputed_share,
    eo_imputed_share = x$economic_opportunity_outputs$diagnostics$imputed_share,
    es_cs_imputed = mean(x$energy_security_outputs$category_scores$imputed %in% TRUE),
    eo_cs_imputed = mean(x$economic_opportunity_outputs$category_scores$imputed %in% TRUE),
    policy_eligibility = x$policy_eligibility,
    policy_variables = sort(unique(x$policy_component_tbl$variable)),
    # The components that enter the Policy index (the final provenance records them; the shipped
    # release has no provenance record, and its composite used every component of its table).
    policy_index_components = x$policy_index_provenance$components,
    policy_index_weights = x$policy_index_provenance$weights,
    policy_excluded_components = x$policy_index_provenance$excluded_components,
    coalition_members = x$coalition_members
  )
  rm(x)
  gc(verbose = FALSE)
  out
}

old <- extract_release(shipped_index_path)
new <- extract_release(corrected_index_path)
headline_techs <- sort(unique(new$es$tech))
stopifnot(c("rankable", "imputed_weight_share", "rank_exclusion") %in% names(new$es),
          c("rankable", "imputed_weight_share", "rank_exclusion") %in% names(new$eo),
          c("resource_base", "pick_exclusion", "si_exclusion") %in% names(new$si),
          !is.null(new$coalition_members))

# The final coalition, by canonical name.
final_coalition_tbl <- new$coalition_members %>%
  transmute(iso3 = .data$iso3, tier = .data$tier, network_node = .data$network_node,
            Country = country_name_from_iso3(.data$iso3))
stopifnot(!anyNA(final_coalition_tbl$Country),
          all(final_coalition_tbl$Country %in% new$es$Country),
          all(final_coalition_tbl$Country %in% new$eo$Country))
coalition <- final_coalition_tbl$Country

# ---------------------------------------------------------------------------------------------
# Country harmonisation of the shipped tables
# ---------------------------------------------------------------------------------------------

# One record per (canonical country, tech, stage). Returns the harmonised table (with the original
# spelling kept in `orig` and the number of shipped spellings of the key in `n_spell`), and the
# rows dropped as non-country units or as merged spellings.
harmonise_shipped <- function(tbl, value_col) {
  res <- country_resolve(tbl$Country)
  tbl$orig <- tbl$Country
  tbl$Country <- res$name
  tbl$iso3c <- res$iso3c
  noncountry <- tbl[is.na(tbl$iso3c), , drop = FALSE]
  kept <- tbl[!is.na(tbl$iso3c), , drop = FALSE] %>%
    group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    mutate(n_spell = dplyr::n()) %>%
    arrange(desc(.data$orig == .data$Country), .by_group = TRUE) %>%
    ungroup()
  first <- !duplicated(kept[, c("Country", "tech", "supply_chain")])
  list(tbl = kept[first, , drop = FALSE], noncountry = noncountry, merged = kept[!first, , drop = FALSE])
}

# Economies with more than one shipped spelling anywhere in a table.
split_economies <- function(tbl) {
  res <- country_resolve(unique(tbl$Country))
  tibble(orig = unique(tbl$Country), Country = res$name, iso3c = res$iso3c) %>%
    filter(!is.na(.data$iso3c)) %>%
    group_by(.data$Country) %>%
    filter(dplyr::n() > 1) %>%
    summarise(spellings = paste(sort(.data$orig), collapse = " / "), .groups = "drop")
}

cell_status <- function(tbl, value_col) {
  tbl %>%
    group_by(.data$tech, .data$supply_chain) %>%
    summarise(
      n = dplyr::n(),
      range = diff(range(.data[[value_col]])),
      value = .data[[value_col]][[1]],
      .groups = "drop"
    ) %>%
    mutate(constant = .data$n >= 2 & .data$range <= const_tol)
}

pillar_spec <- list(
  es = list(value = "Energy_Security_Index", label = "Energy Security"),
  eo = list(value = "Economic_Opportunity_Index", label = "Economic Opportunity")
)

# ---------------------------------------------------------------------------------------------
# Matching, coverage and agreement for one pillar
# ---------------------------------------------------------------------------------------------

spearman <- function(x, y) suppressWarnings(stats::cor(x, y, method = "spearman"))

agreement_stats <- function(m) {
  list(
    n_cells = nrow(m),
    n_economies = dplyr::n_distinct(m$Country),
    pearson = stats::cor(m$shipped, m$corrected),
    spearman = spearman(m$shipped, m$corrected),
    mean_change = mean(m$change),
    mean_abs_change = mean(abs(m$change)),
    share_abs_change_gt_0_1 = mean(abs(m$change) > 0.1),
    share_abs_change_gt_0_2 = mean(abs(m$change) > 0.2)
  )
}

compare_pillar <- function(key) {
  v <- pillar_spec[[key]]$value
  o_raw <- old[[key]] %>% filter(.data$tech %in% headline_techs)
  n_raw <- new[[key]]
  h <- harmonise_shipped(o_raw, v)
  o <- h$tbl

  o_status <- cell_status(o, v)
  n_status <- new[[paste0(key, "_cells")]] %>%
    filter(.data$tech %in% headline_techs) %>%
    select("tech", "supply_chain", n = "n_countries", range = "score_range", "cell_status")
  # The diagnostics' status is the authority; the recomputed test must agree with it.
  n_check <- cell_status(n_raw, v)
  stopifnot(identical(
    sort(paste(n_check$tech, n_check$supply_chain)[n_check$constant]),
    sort(paste(n_status$tech, n_status$supply_chain)[n_status$cell_status == "constant"])
  ))
  stopifnot(!any(n_status$cell_status == "unscored"))
  n_const_val <- n_check %>% select("tech", "supply_chain", corrected_value = "value")

  const_shipped <- with(o_status, paste(tech, supply_chain, sep = " | ")[constant])
  const_corrected <- with(n_status, paste(tech, supply_chain, sep = " | ")[cell_status != "ok"])
  excluded <- bind_rows(
    o_status %>% filter(.data$constant) %>%
      transmute(tech = .data$tech, supply_chain = .data$supply_chain, release = "shipped"),
    n_status %>% filter(.data$cell_status != "ok") %>%
      transmute(tech = .data$tech, supply_chain = .data$supply_chain, release = "corrected")
  )

  matched_all <- inner_join(
    o %>% select("Country", "orig", "n_spell", "tech", "supply_chain", shipped = all_of(v)),
    n_raw %>% select("Country", "tech", "supply_chain", corrected = all_of(v), "rankable",
                     "rank_exclusion", "imputed_weight_share"),
    by = c("Country", "tech", "supply_chain")
  ) %>%
    mutate(change = .data$corrected - .data$shipped,
           stage = factor(.data$supply_chain, levels = stages),
           key = paste(.data$tech, .data$supply_chain, sep = " | "),
           const_shipped = .data$key %in% const_shipped,
           const_corrected = .data$key %in% const_corrected,
           excluded = .data$const_shipped | .data$const_corrected)
  m_all <- matched_all %>% filter(!.data$excluded)
  # Rank statistics over the cells the final release ranks.
  m <- m_all %>% filter(.data$rankable %in% TRUE)

  # Within-cell ranks over the rankable countries present in both releases, so the population is
  # fixed and is the one the paper ranks.
  m <- m %>%
    group_by(.data$tech, .data$supply_chain) %>%
    mutate(rank_shipped = rank(-.data$shipped, ties.method = "average"),
           rank_corrected = rank(-.data$corrected, ties.method = "average"),
           rank_shift = .data$rank_corrected - .data$rank_shipped,
           n_in_cell = dplyr::n()) %>%
    ungroup()

  cells <- m %>%
    group_by(.data$tech, .data$supply_chain) %>%
    summarise(
      n = dplyr::n(),
      spearman = if (dplyr::n() >= 5) spearman(.data$shipped, .data$corrected) else NA_real_,
      mean_change = mean(.data$change),
      median_change = stats::median(.data$change),
      mean_abs_change = mean(abs(.data$change)),
      share_rank_shift_gt_10 = mean(abs(.data$rank_shift) > 10),
      .groups = "drop"
    ) %>%
    mutate(stage = factor(.data$supply_chain, levels = stages))
  cells_ok <- cells %>% filter(!is.na(.data$spearman))

  by_stage <- lapply(stages, function(s) agreement_stats(m %>% filter(.data$supply_chain == s)))
  names(by_stage) <- stages
  by_stage_all <- lapply(stages, function(s) agreement_stats(m_all %>% filter(.data$supply_chain == s)))
  names(by_stage_all) <- stages

  wmin <- cells_ok[which.min(cells_ok$spearman), ]
  wmax <- cells_ok[which.max(cells_ok$spearman), ]

  # Coverage: keys in one release only, and why.
  o_keys <- o %>% select("Country", "tech", "supply_chain")
  n_keys <- n_raw %>% select("Country", "tech", "supply_chain")
  removed <- anti_join(o_keys, n_keys, by = c("Country", "tech", "supply_chain"))
  added <- anti_join(n_keys, o_keys, by = c("Country", "tech", "supply_chain"))
  n_pairs <- n_raw %>% distinct(.data$tech, .data$supply_chain)
  o_pairs <- o %>% distinct(.data$tech, .data$supply_chain)
  n_econ <- unique(n_raw$Country)
  o_econ <- unique(o$Country)
  removed <- removed %>%
    mutate(reason = case_when(
      !.data$Country %in% n_econ ~ "economy absent from the corrected pillar",
      !paste(.data$tech, .data$supply_chain) %in% paste(n_pairs$tech, n_pairs$supply_chain) ~
        "technology-stage cell no longer scored",
      TRUE ~ "country not scored in this cell after correction"
    ))
  added <- added %>%
    left_join(n_raw %>% select("Country", "tech", "supply_chain", "rankable"),
              by = c("Country", "tech", "supply_chain")) %>%
    mutate(reason = case_when(
      !.data$Country %in% o_econ ~ "economy absent from the shipped pillar",
      !paste(.data$tech, .data$supply_chain) %in% paste(o_pairs$tech, o_pairs$supply_chain) ~
        "technology-stage cell newly scored",
      TRUE ~ "country newly scored in an existing cell"
    ))
  count_by <- function(x, col) {
    if (nrow(x) == 0) return(list())
    tb <- table(x[[col]])
    as.list(stats::setNames(as.integer(tb), names(tb)))
  }
  top_cells <- function(x, n = 6) {
    x %>% count(.data$tech, .data$supply_chain, .data$reason, sort = TRUE) %>% head(n) %>%
      mutate(cell = paste(.data$tech, .data$supply_chain)) %>%
      select("cell", "reason", "n")
  }
  key_cols <- c("Country", "tech", "supply_chain")
  added_imp <- new[[paste0(key, "_imp")]] %>% semi_join(added, by = key_cols)

  const_cells <- excluded %>%
    group_by(.data$tech, .data$supply_chain) %>%
    summarise(constant_in = paste(sort(.data$release), collapse = " and "), .groups = "drop") %>%
    left_join(o_status %>% select("tech", "supply_chain", shipped_n = "n", shipped_value = "value"),
              by = c("tech", "supply_chain")) %>%
    left_join(n_status %>% select("tech", "supply_chain", corrected_n = "n"), by = c("tech", "supply_chain")) %>%
    left_join(n_const_val, by = c("tech", "supply_chain"))

  # Shipped cells scored exactly zero: under the earlier aggregation a cell with no observed
  # category scored 0 rather than missing (item A1).
  zero_cells <- m_all %>% filter(.data$shipped == 0)

  rank_excl <- matched_all %>% filter(!(.data$rankable %in% TRUE))
  list(
    matched = m,
    matched_nonconstant = m_all,
    matched_all = matched_all,
    cells = cells,
    harm = h,
    shipped = o,
    removed = removed,
    added = added,
    summary = list(
      shipped_rows = nrow(o_raw),
      shipped_country_strings = dplyr::n_distinct(o_raw$Country),
      shipped_noncountry_rows = nrow(h$noncountry),
      shipped_noncountry_units = sort(unique(h$noncountry$orig)),
      shipped_merged_spelling_rows = nrow(h$merged),
      shipped_split_economies = split_economies(o_raw)$spellings,
      shipped_keys_after_harmonisation = nrow(o),
      shipped_economies_after_harmonisation = length(o_econ),
      shipped_tech_stage_pairs = nrow(o_pairs),
      corrected_rows = nrow(n_raw),
      corrected_rankable_rows = sum(n_raw$rankable %in% TRUE),
      corrected_rank_exclusion = count_by(n_raw %>% filter(!(.data$rankable %in% TRUE)), "rank_exclusion"),
      corrected_economies = length(n_econ),
      corrected_economies_with_a_rankable_cell = dplyr::n_distinct(n_raw$Country[n_raw$rankable %in% TRUE]),
      corrected_tech_stage_pairs = nrow(n_pairs),
      economies_only_in_shipped = sort(setdiff(o_econ, n_econ)),
      economies_only_in_corrected = sort(setdiff(n_econ, o_econ)),
      matched_cells_all = nrow(matched_all),
      matched_cells_rankable = sum(matched_all$rankable %in% TRUE),
      matched_cells_not_rankable_by_reason = count_by(rank_excl, "rank_exclusion"),
      matched_cells_nonconstant = nrow(m_all),
      matched_cells_nonconstant_rankable = nrow(m),
      removed_cells = nrow(removed),
      removed_by_reason = count_by(removed, "reason"),
      removed_top_cells = top_cells(removed),
      added_cells = nrow(added),
      added_cells_rankable = sum(added$rankable %in% TRUE),
      added_by_reason = count_by(added, "reason"),
      added_top_cells = top_cells(added),
      added_median_imputed_weight_share = if (nrow(added_imp)) stats::median(added_imp$imputed_weight_share) else NA_real_,
      excluded_constant_cells = const_cells,
      excluded_constant_country_cells_matched = sum(matched_all$excluded)
    ),
    agreement_all = c(agreement_stats(m_all), list(by_stage = by_stage_all)),
    agreement = c(
      agreement_stats(m),
      list(
        sample = "matched non-constant cells that are rankable in the corrected release",
        n_tech_stage_cells = nrow(cells_ok),
        by_stage = by_stage,
        within_cell_spearman = list(
          n_cells = nrow(cells_ok),
          n_cells_too_small = sum(is.na(cells$spearman)),
          median = stats::median(cells_ok$spearman),
          q25 = unname(stats::quantile(cells_ok$spearman, 0.25)),
          q75 = unname(stats::quantile(cells_ok$spearman, 0.75)),
          min = wmin$spearman, min_cell = paste(wmin$tech, wmin$supply_chain),
          max = wmax$spearman, max_cell = paste(wmax$tech, wmax$supply_chain),
          n_cells_below_0_8 = sum(cells_ok$spearman < 0.8),
          n_cells_below_0_5 = sum(cells_ok$spearman < 0.5),
          by_stage = lapply(stats::setNames(stages, stages), function(st) {
            x <- cells_ok$spearman[cells_ok$supply_chain == st]
            list(n_cells = length(x), median = stats::median(x), min = min(x), max = max(x))
          })
        ),
        share_rank_shift_gt_10 = mean(abs(m$rank_shift) > 10),
        share_rank_shift_gt_10pct_of_cell = mean(abs(m$rank_shift) > 0.1 * m$n_in_cell),
        median_abs_rank_shift = stats::median(abs(m$rank_shift)),
        cell_size_range = range(cells_ok$n),
        shipped_exact_zero_cells = list(
          n = nrow(zero_cells),
          by_stage = as.list(table(factor(zero_cells$supply_chain, levels = stages))),
          n_economies = dplyr::n_distinct(zero_cells$Country),
          top_cells = zero_cells %>% count(cell = paste(.data$tech, .data$supply_chain), sort = TRUE) %>% head(6),
          corrected_mean = if (nrow(zero_cells)) mean(zero_cells$corrected) else NA_real_
        ),
        mean_change_by_stage = as.list(tapply(m$change, m$supply_chain, mean)[stages])
      )
    )
  )
}

es <- compare_pillar("es")
eo <- compare_pillar("eo")

# ---------------------------------------------------------------------------------------------
# Data changes behind a category: reporting status, revised baskets, uranium
# ---------------------------------------------------------------------------------------------

# Economies that reported 2024 imports to Comtrade; the imports of the others enter the trade
# matrix only through their partners' (mirror) export declarations.
dyads_path <- file.path(raw_dir, "comtrade_energy_dyads_all.csv")
reporters <- data.table::fread(dyads_path, select = c("reporter_iso", "ref_year", "flow_code"),
                               showProgress = FALSE)
reporters <- unique(as.data.frame(reporters))
import_reporters_2024 <- sort(unique(reporters$reporter_iso[reporters$ref_year == 2024 &
                                                              reporters$flow_code == "M"]))
rm(reporters)
invisible(gc(verbose = FALSE))
coalition_nonreporters_2024 <- sort(setdiff(final_coalition_tbl$iso3, import_reporters_2024))

# Technology-stage baskets the 2026-10 basket review changed (codes excluded with a review note).
hs6_master <- utils::read.csv(file.path(root, "data", "reference", "energy_hs6_master.csv"),
                              stringsAsFactors = FALSE, colClasses = c(hs6 = "character"))
review_rows <- hs6_master[grepl("basket review", hs6_master$note, fixed = TRUE) &
                            hs6_master$excluded %in% TRUE, ]
revised_baskets <- unique(paste(review_rows$tech, review_rows$supply_chain))
added_rows <- hs6_master[grepl("^added \\(basket review", hs6_master$note) &
                           !(hs6_master$excluded %in% TRUE), ]
basket_review <- list(
  codes_excluded = nrow(review_rows),
  codes_kept_with_caveat = sum(grepl("basket review", hs6_master$note, fixed = TRUE) &
                                 !(hs6_master$excluded %in% TRUE) &
                                 !grepl("^added \\(basket review", hs6_master$note)),
  codes_added = nrow(added_rows),
  stages_with_added_codes = sort(unique(paste(added_rows$tech, added_rows$supply_chain))),
  stages_revised = sort(revised_baskets),
  electric_grid_upstream_codes_kept = sum(hs6_master$tech == "Electric Grid" &
                                            hs6_master$supply_chain == "Upstream" &
                                            !(hs6_master$excluded %in% TRUE))
)
rm(hs6_master)

# ---------------------------------------------------------------------------------------------
# Movers among coalition members and their attributed cause
# ---------------------------------------------------------------------------------------------

item_codes <- list(
  es = c("Trade" = "T1\u2013T5", "Minerals Trade" = "E6", "Foreign Dependency" = "E2\u2013E3",
         "Reserves" = "E1, E4", "Production" = "E1, E3", "Investment" = "E7", "Consumption" = "E5",
         "Energy Prices" = "A4", "Energy Access" = "C1\u2013C2", "Energy Imports" = "C1\u2013C2"),
  eo = c("Trade" = "T1\u2013T5", "Cost Competitiveness" = "T6", "Technology Demand" = "T8",
         "Investment" = "E7", "Foreign Dependency" = "E2, E8", "Consumption" = "E5",
         "Production" = "E1, E3", "Technological Readiness" = "T7", "Energy Access" = "C1\u2013C2",
         "Energy Prices" = "A4")
)

# The data change behind a category of a cell, beyond the audit item: uranium in Nuclear
# Upstream, the all-reporter trade matrix (with the reviewed basket and mirror declarations where
# they apply), TRL by stage.
data_change <- function(category, key, tech, stage, iso3) {
  if (tech == "Nuclear" && stage == "Upstream" && category %in% c("Reserves", "Production")) {
    return("uranium, Red Book")
  }
  # Critical-mineral reserves now come from USGS MCS 2026; oil, gas and coal keep EI.
  if (category == "Reserves" && !tech %in% c("Oil", "Gas", "Coal", "Nuclear")) {
    return("USGS MCS 2026")
  }
  if (category == "Trade") {
    parts <- "all-reporter dyads"
    if (paste(tech, stage) %in% revised_baskets) parts <- c(parts, "basket review")
    if (key == "es" && !iso3 %in% import_reporters_2024) parts <- c(parts, "mirror imports")
    return(paste(parts, collapse = ", "))
  }
  if (category == "Technological Readiness") return("TRL by stage")
  NA_character_
}

# Category rows of one cell in both releases, with the score effect of each common category.
decompose_cell <- function(key, country, orig, tech, stage) {
  oc <- old[[paste0(key, "_cc")]] %>%
    filter(.data$Country == orig, .data$tech == !!tech, .data$supply_chain == stage) %>%
    transmute(category = .data$category, c_old = coalesce(.data$category_score, 0),
              unobserved_old = is.na(.data$category_score), w_old = .data$category_weight,
              # The earlier imputed flag lost multi-component groups (item A3); a missing-data
              # rule recorded for the category is the reliable marker.
              imputed_old = (.data$imputed %in% TRUE) | !is.na(.data$missing_rule_applied),
              rule_old = .data$missing_rule_applied)
  nc <- new[[paste0(key, "_cc")]] %>%
    filter(.data$Country == country, .data$tech == !!tech, .data$supply_chain == stage) %>%
    transmute(category = .data$category, c_new = .data$category_score, w_new = .data$category_weight,
              imputed_new = .data$imputed %in% TRUE, rule_new = .data$missing_rule_applied)
  full_join(oc, nc, by = "category") %>%
    mutate(score_effect = if_else(!is.na(.data$w_old) & !is.na(.data$w_new),
                                  0.5 * (.data$w_old + .data$w_new) * (.data$c_new - .data$c_old),
                                  NA_real_))
}

with_item <- function(text, key, category, tech, stage, iso3) {
  item <- item_codes[[key]][category]
  extra <- data_change(category, key, tech, stage, iso3)
  tags <- c(if (!is.na(item)) item, if (!is.na(extra)) extra)
  if (length(tags) == 0) text else paste0(text, " (", paste(tags, collapse = "; "), ")")
}

composition_label <- function(dec, key, tech, stage, iso3) {
  removed <- dec %>% filter(is.na(.data$w_new))
  added <- dec %>% filter(is.na(.data$w_old))
  parts <- character(0)
  for (i in seq_len(nrow(removed))) {
    r <- removed[i, ]
    parts <- c(parts, if (r$category == "Investment" && r$c_old == 0) {
      "zero Investment cell removed (E7)"
    } else if (r$category == "Technology Demand") {
      "Technology Demand dropped (T8)"
    } else if (r$category == "Reserves" && key == "eo") {
      "Reserves weight removed (A5)"
    } else {
      paste(r$category, "removed")
    })
  }
  if (tech == "Nuclear" && stage == "Upstream" &&
      all(c("Reserves", "Production") %in% added$category)) {
    parts <- c(parts, "uranium Reserves and Production added (Red Book)")
    added <- added %>% filter(!.data$category %in% c("Reserves", "Production"))
  }
  for (i in seq_len(nrow(added))) {
    a <- added[i, ]
    parts <- c(parts, if (a$category == "Foreign Dependency") {
      if (key == "eo" && tech == "Green Hydrogen") "Foreign Dependency added (E8)" else "Foreign Dependency added (E2)"
    } else if (a$category == "Energy Prices") {
      "Energy Prices added (A4)"
    } else if (a$category == "Consumption") {
      "Consumption added (E5)"
    } else if (a$category == "Trade") {
      with_item("Trade added", key, "Trade", tech, stage, iso3)
    } else {
      with_item(paste(a$category, "added"), key, a$category, tech, stage, iso3)
    })
  }
  if (length(parts) == 0) parts <- "category weights renormalised (A4\u2013A5)"
  paste(parts, collapse = "; ")
}

score_label <- function(row, key, merged_spelling, tech, stage, iso3) {
  cat_txt <- row$category
  if (merged_spelling) {
    return(paste0("spellings merged (C1); ", cat_txt,
                  if (row$imputed_old && !row$imputed_new) " observed" else " rescored"))
  }
  if (row$unobserved_old) {
    lab <- with_item(paste0(cat_txt, " observed, was unobserved"), key, row$category, tech, stage, iso3)
    return(if (grepl(" (", lab, fixed = TRUE)) sub(" (", " (A1, ", lab, fixed = TRUE) else paste0(lab, " (A1)"))
  }
  if (row$imputed_old && !row$imputed_new) {
    return(with_item(paste0(cat_txt, " observed, was imputed"), key, row$category, tech, stage, iso3))
  }
  if (!row$imputed_old && row$imputed_new) {
    how <- if (grepl("zero", dplyr::coalesce(row$rule_new, ""))) " zero-filled" else " imputed"
    return(with_item(paste0(cat_txt, how), key, row$category, tech, stage, iso3))
  }
  with_item(paste0(cat_txt, " rebuilt"), key, row$category, tech, stage, iso3)
}

attribute_mover <- function(key, k, split_names) {
  iso3 <- country_name_to_iso3(k$Country)
  dec <- decompose_cell(key, k$Country, k$orig, k$tech, k$supply_chain)
  score_total <- sum(dec$score_effect, na.rm = TRUE)
  composition <- k$change - score_total
  # The reconstruction must close: shipped and corrected indices from the category rows.
  rec_old <- sum(dec$w_old * dec$c_old, na.rm = TRUE)
  rec_new <- sum(dec$w_new * dec$c_new, na.rm = TRUE)
  stopifnot(abs(rec_old - k$shipped) < 1e-8, abs(rec_new - k$corrected) < 1e-8)
  merged <- k$Country %in% split_names
  effects <- bind_rows(
    dec %>% filter(!is.na(.data$score_effect)) %>%
      transmute(driver = .data$category, effect = .data$score_effect),
    tibble(driver = "(composition)", effect = composition)
  ) %>% arrange(desc(abs(.data$effect)))
  label_of <- function(driver) {
    if (driver == "(composition)") composition_label(dec, key, k$tech, k$supply_chain, iso3)
    else score_label(dec %>% filter(.data$category == driver), key, merged, k$tech, k$supply_chain, iso3)
  }
  cause <- label_of(effects$driver[1])
  if (nrow(effects) > 1 && abs(effects$effect[2]) >= 0.5 * abs(effects$effect[1])) {
    second <- label_of(effects$driver[2])
    # A merged spelling is named once; the second driver then only adds its category.
    prefix <- "spellings merged (C1); "
    if (merged && startsWith(second, prefix)) second <- substring(second, nchar(prefix) + 1)
    cause <- paste0(cause, " + ", second)
  }
  substr(cause, 1, 1) <- toupper(substr(cause, 1, 1))
  top_score <- dec %>% filter(!is.na(.data$score_effect)) %>% arrange(desc(abs(.data$score_effect))) %>% head(1)
  tibble(
    Country = k$Country, iso3 = iso3, tech = k$tech, supply_chain = k$supply_chain,
    shipped = k$shipped, corrected = k$corrected, change = k$change,
    const_shipped = k$const_shipped,
    composition_effect = composition, score_effect_total = score_total,
    top_score_category = if (nrow(top_score)) top_score$category else NA_character_,
    top_score_effect = if (nrow(top_score)) top_score$score_effect else NA_real_,
    categories_removed = paste(dec$category[is.na(dec$w_new)], collapse = ", "),
    categories_added = paste(dec$category[is.na(dec$w_old)], collapse = ", "),
    spelling_merged = merged,
    primary_driver = effects$driver[1],
    cause = cause
  )
}

n_movers <- 12
# At most this many movers per technology-stage cell, so that one cell-wide change does not fill
# the list (without the cap, nine of the twelve largest ES changes are Nuclear Upstream cells).
n_movers_per_cell <- 2
# Movers: coalition members' matched cells that the final release ranks, including cells that
# were constant in the shipped release (not constant in the corrected one).
mover_pool <- function(res) {
  res$matched_all %>%
    filter(.data$Country %in% coalition, .data$rankable %in% TRUE, !.data$const_corrected)
}
# The uncapped ranking: the cells of the n largest changes.
uncapped_top <- function(res, n = n_movers) {
  top <- mover_pool(res) %>% arrange(desc(abs(.data$change))) %>% head(n)
  list(cells = as.list(sort(table(paste(top$tech, top$supply_chain)), decreasing = TRUE)),
       members = paste(country_name_to_iso3(top$Country), top$tech, top$supply_chain),
       change_range = range(top$change))
}
movers_for <- function(key, res, n = n_movers, per_cell = n_movers_per_cell) {
  split <- split_economies(old[[key]] %>% filter(.data$tech %in% headline_techs))
  split_names <- unique(standardize_country_names(sub(" / .*$", "", split$spellings)))
  top <- mover_pool(res) %>%
    arrange(desc(abs(.data$change))) %>%
    group_by(.data$tech, .data$supply_chain) %>%
    slice_head(n = per_cell) %>%
    ungroup() %>%
    arrange(desc(abs(.data$change))) %>%
    head(n)
  bind_rows(lapply(seq_len(nrow(top)), function(i) attribute_mover(key, top[i, ], split_names))) %>%
    mutate(pillar = pillar_spec[[key]]$label, rank = dplyr::row_number())
}

movers_es <- movers_for("es", es)
movers_eo <- movers_for("eo", eo)

# The category rows behind each mover (earlier and corrected score and weight, score effect), so
# that every category value quoted in the text can be traced.
mover_category_rows <- function(key, res, movers) {
  bind_rows(lapply(seq_len(nrow(movers)), function(i) {
    m <- movers[i, ]
    k <- res$matched_all %>%
      filter(.data$Country == m$Country, .data$tech == m$tech, .data$supply_chain == m$supply_chain)
    decompose_cell(key, k$Country, k$orig, k$tech, k$supply_chain) %>%
      mutate(rank = m$rank, Country = m$Country, tech = m$tech, supply_chain = m$supply_chain,
             .before = 1)
  }))
}

# The coalition's matched cells: agreement, and the cells the final release no longer ranks.
coalition_pillar_facts <- function(res) {
  pool <- res$matched_all %>% filter(.data$Country %in% coalition)
  cm <- res$matched %>% filter(.data$Country %in% coalition)
  nr <- pool %>% filter(!(.data$rankable %in% TRUE))
  list(n_members = length(coalition),
       n_matched_cells = nrow(pool),
       n_matched_cells_not_rankable = nrow(nr),
       not_rankable_members = sort(unique(nr$Country)),
       not_rankable_by_reason = as.list(table(nr$rank_exclusion)),
       n_rankable_nonconstant = nrow(cm),
       share_abs_change_gt_0_1 = mean(abs(cm$change) > 0.1),
       spearman = spearman(cm$shipped, cm$corrected),
       mover_pool = nrow(mover_pool(res)),
       mover_pool_const_shipped = sum(mover_pool(res)$const_shipped))
}

# Every rankable matched coalition cell decomposed, to summarise the causes beyond the top movers.
decompose_pool <- function(key, res) {
  pool <- mover_pool(res)
  split <- split_economies(old[[key]] %>% filter(.data$tech %in% headline_techs))
  split_names <- unique(standardize_country_names(sub(" / .*$", "", split$spellings)))
  bind_rows(lapply(seq_len(nrow(pool)), function(i) attribute_mover(key, pool[i, ], split_names)))
}
pool_es <- decompose_pool("es", es)
pool_eo <- decompose_pool("eo", eo)
pool_summary <- function(p) {
  big <- p %>% filter(abs(.data$change) > 0.1)
  list(
    n_cells = nrow(p),
    n_abs_change_gt_0_1 = nrow(big),
    primary_driver_of_changes_gt_0_1 = as.list(sort(table(big$primary_driver), decreasing = TRUE)),
    trade_driven_with_basket_review = sum(big$primary_driver == "Trade" &
                                            paste(big$tech, big$supply_chain) %in% revised_baskets),
    trade_driven_mirror_imports = sum(big$primary_driver == "Trade" & grepl("mirror imports", big$cause)),
    cause_mentions_among_changes_gt_0_1 = list(
      uranium_red_book = sum(grepl("Red Book", big$cause)),
      usgs_reserves = sum(grepl("USGS", big$cause)),
      basket_review = sum(grepl("basket review", big$cause)),
      mirror_imports = sum(grepl("mirror imports", big$cause)),
      all_reporter_dyads = sum(grepl("all-reporter dyads", big$cause)),
      zero_investment_removed = sum(grepl("zero Investment", big$cause, ignore.case = TRUE)),
      spellings_merged = sum(grepl("merged", big$cause))
    ),
    cells_of_changes_gt_0_1_top = as.list(utils::head(sort(table(paste(big$tech, big$supply_chain)),
                                                          decreasing = TRUE), 8)),
    nuclear_upstream = list(
      n = sum(p$tech == "Nuclear" & p$supply_chain == "Upstream"),
      change_range = if (any(p$tech == "Nuclear" & p$supply_chain == "Upstream")) {
        range(p$change[p$tech == "Nuclear" & p$supply_chain == "Upstream"])
      } else NA
    )
  )
}

# ---------------------------------------------------------------------------------------------
# The same decomposition for every matched non-constant cell (all countries)
# ---------------------------------------------------------------------------------------------

# The effect of each category and the composition effect, per (country, technology, stage).
# Summarised overall (share of the summed absolute effect) and per technology-stage cell (the
# driver with the largest mean effect moves the level of the cell; the driver with the largest
# cross-country standard deviation reorders it). Computed over the rankable matched cells.
decompose_all <- function(key, res) {
  keys <- c("Country", "tech", "supply_chain")
  kept <- res$shipped %>% select("Country", "orig", "tech", "supply_chain")
  oc <- old[[paste0(key, "_cc")]] %>%
    rename(orig = "Country") %>%
    inner_join(kept, by = c("orig", "tech", "supply_chain")) %>%
    transmute(Country = .data$Country, tech = .data$tech, supply_chain = .data$supply_chain,
              category = .data$category, c_old = coalesce(.data$category_score, 0),
              w_old = .data$category_weight)
  nc <- new[[paste0(key, "_cc")]] %>%
    transmute(Country = .data$Country, tech = .data$tech, supply_chain = .data$supply_chain,
              category = .data$category, c_new = .data$category_score, w_new = .data$category_weight)
  m <- res$matched %>% select(all_of(keys), "shipped", "corrected", "change")
  j <- full_join(oc, nc, by = c(keys, "category")) %>% semi_join(m, by = keys)
  recon <- j %>%
    group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    summarise(rec_old = sum(.data$w_old * .data$c_old, na.rm = TRUE),
              rec_new = sum(.data$w_new * .data$c_new, na.rm = TRUE),
              score_total = sum(if_else(!is.na(.data$w_old) & !is.na(.data$w_new),
                                        0.5 * (.data$w_old + .data$w_new) * (.data$c_new - .data$c_old), 0)),
              .groups = "drop") %>%
    inner_join(m, by = keys)
  stopifnot(nrow(recon) == nrow(m),
            max(abs(recon$rec_old - recon$shipped)) < 1e-8,
            max(abs(recon$rec_new - recon$corrected)) < 1e-8)
  effects <- bind_rows(
    j %>% filter(!is.na(.data$w_old), !is.na(.data$w_new)) %>%
      transmute(Country = .data$Country, tech = .data$tech, supply_chain = .data$supply_chain,
                driver = .data$category,
                effect = 0.5 * (.data$w_old + .data$w_new) * (.data$c_new - .data$c_old)),
    recon %>% transmute(Country = .data$Country, tech = .data$tech, supply_chain = .data$supply_chain,
                        driver = "(composition)", effect = .data$change - .data$score_total)
  )
  # Every country of a cell gets a row for every driver of the cell (zero where absent), so the
  # cross-country moments are taken over the same countries.
  effects <- effects %>%
    tidyr::complete(tidyr::nesting(Country, tech, supply_chain), driver, fill = list(effect = 0)) %>%
    semi_join(effects %>% distinct(.data$tech, .data$supply_chain, .data$driver),
              by = c("tech", "supply_chain", "driver"))
  overall <- effects %>%
    group_by(.data$driver) %>%
    summarise(abs_sum = sum(abs(.data$effect)), .groups = "drop") %>%
    mutate(share = .data$abs_sum / sum(.data$abs_sum)) %>%
    arrange(desc(.data$share))
  by_cell <- effects %>%
    group_by(.data$tech, .data$supply_chain, .data$driver) %>%
    summarise(mean_effect = mean(.data$effect), sd_effect = stats::sd(.data$effect), .groups = "drop")
  cell_drivers <- by_cell %>%
    group_by(.data$tech, .data$supply_chain) %>%
    summarise(
      level_driver = .data$driver[which.max(abs(.data$mean_effect))],
      level_driver_mean = .data$mean_effect[which.max(abs(.data$mean_effect))],
      reorder_driver = .data$driver[which.max(.data$sd_effect)],
      reorder_driver_sd = max(.data$sd_effect),
      .groups = "drop"
    )
  list(overall = overall, cell_drivers = cell_drivers)
}

dec_es <- decompose_all("es", es)
dec_eo <- decompose_all("eo", eo)
es$cells <- es$cells %>% left_join(dec_es$cell_drivers, by = c("tech", "supply_chain"))
eo$cells <- eo$cells %>% left_join(dec_eo$cell_drivers, by = c("tech", "supply_chain"))

# ---------------------------------------------------------------------------------------------
# Strategic Index
# ---------------------------------------------------------------------------------------------

si_old_raw <- old$si %>% filter(!is.na(.data$strategic_index))
si_new_raw <- new$si %>% filter(!is.na(.data$strategic_index))
si_old <- si_old_raw %>% mutate(Country = standardize_country_names(.data$Country))
stopifnot(!anyDuplicated(si_old[, c("Country", "sector")]),
          !anyDuplicated(si_new_raw[, c("Country", "sector")]))
# Both releases compute SI = 0.2 pol + 0.3 EO + 0.3 need + 0.1 sc + 0.1 tw; in the shipped table
# the column Energy_Security_Index holds the need 1 - ES (item P6).
si_terms_old <- si_old %>%
  transmute(Country = .data$Country, sector = .data$sector, tech = .data$tech,
            supply_chain = .data$supply_chain, si = .data$strategic_index,
            pol = .data$pol, eo = .data$Economic_Opportunity_Index,
            need = .data$Energy_Security_Index, sc = .data$sc_weight, tw = .data$tech_weight)
si_terms_new <- si_new_raw %>%
  transmute(Country = .data$Country, sector = .data$sector, si = .data$strategic_index,
            pol = .data$pol, eo = .data$Economic_Opportunity_Index,
            need = .data$energy_security_need, sc = .data$sc_weight, tw = .data$tech_weight)
si_close <- function(d) with(d, max(abs(si_w[["pol"]] * pol + si_w[["eo"]] * eo + si_w[["need"]] * need +
                                          si_w[["sc"]] * sc + si_w[["tw"]] * tw - si)))
stopifnot(si_close(si_terms_old) < 1e-8, si_close(si_terms_new) < 1e-8)

si_m <- inner_join(si_terms_old, si_terms_new, by = c("Country", "sector"), suffix = c("_old", "_new")) %>%
  transmute(Country = .data$Country, sector = .data$sector, tech = .data$tech,
            supply_chain = .data$supply_chain,
            shipped = .data$si_old, corrected = .data$si_new,
            change = .data$corrected - .data$shipped,
            d_pol = si_w[["pol"]] * (.data$pol_new - .data$pol_old),
            d_eo = si_w[["eo"]] * (.data$eo_new - .data$eo_old),
            d_need = si_w[["need"]] * (.data$need_new - .data$need_old),
            d_weights = si_w[["sc"]] * (.data$sc_new - .data$sc_old) + si_w[["tw"]] * (.data$tw_new - .data$tw_old),
            pol_old = .data$pol_old, pol_new = .data$pol_new,
            sc_old = .data$sc_old, sc_new = .data$sc_new, tw_old = .data$tw_old, tw_new = .data$tw_new,
            stage = factor(.data$supply_chain, levels = stages))
stopifnot(max(abs(si_m$d_pol + si_m$d_eo + si_m$d_need + si_m$d_weights - si_m$change)) < 1e-8)

# How much of the pooled SI agreement is common sector structure: the share of SI variance
# explained by sector means in each release, and the Spearman correlation of the SI after the
# sector means of each release are removed (within-sector agreement).
sector_r2 <- function(y, g) {
  fit <- stats::lm(y ~ factor(g))
  summary(fit)$r.squared
}
si_m <- si_m %>%
  group_by(.data$sector) %>%
  mutate(shipped_dm = .data$shipped - mean(.data$shipped),
         corrected_dm = .data$corrected - mean(.data$corrected)) %>%
  ungroup()
term_abs <- c(policy = sum(abs(si_m$d_pol)), eo = sum(abs(si_m$d_eo)), need = sum(abs(si_m$d_need)),
              weights = sum(abs(si_m$d_weights)))
si_structure <- list(
  sector_r2_shipped = sector_r2(si_m$shipped, si_m$sector),
  sector_r2_corrected = sector_r2(si_m$corrected, si_m$sector),
  within_sector_spearman = spearman(si_m$shipped_dm, si_m$corrected_dm),
  sc_weight_max_abs_diff = max(abs(si_m$sc_new - si_m$sc_old)),
  tech_weight_max_abs_diff = max(abs(si_m$tw_new - si_m$tw_old)),
  tech_weight_spearman = spearman(si_m$tw_old, si_m$tw_new),
  # The technology weight is the technology GHG index, rescaled across the technologies; record
  # it per technology so the change can be read.
  tech_weight_by_tech = si_m %>% distinct(.data$tech, .data$tw_old, .data$tw_new) %>% arrange(.data$tech),
  # pol: the within-country S-curve of the Policy index over the scored sectors (both releases);
  # shipped composite IEA PAMS + CAT + NIPO + dual-use, final composite IEA PAMS (required) + CAT
  # (where rated).
  pol_spearman = spearman(si_m$pol_old, si_m$pol_new),
  pol_pearson = stats::cor(si_m$pol_old, si_m$pol_new),
  matched_countries_policy_basis = as.list(table(
    new$si$policy_basis[match(sort(unique(si_m$Country)), new$si$Country)], useNA = "ifany")),
  shipped_policy_components = old$policy_variables,
  final_policy_index_components = new$policy_index_components,
  final_policy_index_weights = as.list(new$policy_index_weights),
  term_share_of_abs_change = as.list(term_abs / sum(term_abs)),
  mean_term_change = list(policy = mean(si_m$d_pol), eo = mean(si_m$d_eo), need = mean(si_m$d_need),
                          weights = mean(si_m$d_weights)),
  mean_abs_term_change = list(policy = mean(abs(si_m$d_pol)), eo = mean(abs(si_m$d_eo)),
                              need = mean(abs(si_m$d_need)), weights = mean(abs(si_m$d_weights)))
)
si_country <- si_m %>%
  group_by(.data$Country) %>%
  summarise(n = dplyr::n(), spearman = if (dplyr::n() >= 5) spearman(.data$shipped, .data$corrected) else NA_real_,
            .groups = "drop") %>%
  filter(!is.na(.data$spearman))
si_sectors_old <- sort(unique(si_old$sector))
si_sectors_new <- sort(unique(si_new_raw$sector))
# Why the shipped SI cells of shipped countries are not scored in the final release.
si_unmatched_old <- si_old %>%
  anti_join(si_new_raw, by = c("Country", "sector")) %>%
  left_join(new$si %>% select("Country", "sector", "si_exclusion"), by = c("Country", "sector")) %>%
  mutate(reason = case_when(
    is.na(.data$si_exclusion) & !.data$sector %in% new$si$sector ~ "sector not in the final table",
    is.na(.data$si_exclusion) ~ "no final row",
    grepl("not policy-eligible", .data$si_exclusion) ~ "country not policy-eligible",
    grepl("missing energy security", .data$si_exclusion) ~ "no ES value",
    grepl("not rankable", .data$si_exclusion) ~ "EO or ES not rankable",
    TRUE ~ .data$si_exclusion
  ))
si_exclusion_flags <- new$si %>%
  filter(is.na(.data$strategic_index)) %>%
  summarise(n = dplyr::n(),
            not_policy_eligible = sum(grepl("not policy-eligible", .data$si_exclusion)),
            eo_not_rankable = sum(grepl("economic opportunity not rankable", .data$si_exclusion)),
            es_not_rankable = sum(grepl("energy security not rankable", .data$si_exclusion)),
            missing_es = sum(grepl("missing energy security", .data$si_exclusion)),
            policy_eligible_but_pillar = sum(!grepl("not policy-eligible", .data$si_exclusion)))
si_summary <- c(
  agreement_stats(si_m),
  list(
    shipped_cells = nrow(si_old_raw),
    shipped_countries = dplyr::n_distinct(si_old_raw$Country),
    shipped_sectors = length(si_sectors_old),
    corrected_cells = nrow(si_new_raw),
    corrected_countries = dplyr::n_distinct(si_new_raw$Country),
    corrected_sectors = length(si_sectors_new),
    shipped_countries_not_scored_in_corrected = sort(setdiff(unique(si_old$Country), unique(si_new_raw$Country))),
    sectors_only_in_shipped = setdiff(si_sectors_old, si_sectors_new),
    sectors_only_in_corrected = setdiff(si_sectors_new, si_sectors_old),
    shipped_cells_not_in_corrected = nrow(si_unmatched_old),
    shipped_cells_not_in_corrected_by_reason = as.list(table(si_unmatched_old$reason)),
    shipped_cells_not_in_corrected_by_sector = as.list(sort(table(si_unmatched_old$sector), decreasing = TRUE)),
    matched_countries = dplyr::n_distinct(si_m$Country),
    matched_countries_list = sort(unique(si_m$Country)),
    within_country_spearman = list(
      n_countries = nrow(si_country),
      median = stats::median(si_country$spearman),
      min = min(si_country$spearman), min_country = si_country$Country[which.min(si_country$spearman)],
      max = max(si_country$spearman), max_country = si_country$Country[which.max(si_country$spearman)]
    ),
    corrected_unscored_cells = as.list(si_exclusion_flags),
    structure = si_structure
  )
)

# SI movers among coalition members, split into the five terms.
si_movers <- si_m %>%
  filter(.data$Country %in% coalition) %>%
  arrange(desc(abs(.data$change))) %>%
  head(n_movers) %>%
  mutate(rank = dplyr::row_number(),
         iso3 = country_name_to_iso3(.data$Country),
         main_term = c("policy", "eo", "need", "weights")[
           apply(abs(cbind(.data$d_pol, .data$d_eo, .data$d_need, .data$d_weights)), 1, which.max)])
si_coalition <- si_m %>% filter(.data$Country %in% coalition)

# ---------------------------------------------------------------------------------------------
# Allied pick per member
# ---------------------------------------------------------------------------------------------

# The selection rule of select_allied_strategic_sectors() (R/indices/build_indices.R), applied
# locally: each member's highest-SI Upstream or Midstream sector, Geothermal and Green Hydrogen
# excluded by rule; with the endowment screen, an Upstream cell flagged resource_base == FALSE is
# not eligible. The shipped table has no resource_base column, so it is not screened.
select_pick <- function(si, members, screen = TRUE) {
  bar <- if (screen && "resource_base" %in% names(si)) si$resource_base %in% FALSE else rep(FALSE, nrow(si))
  si[!bar, , drop = FALSE] %>%
    filter(.data$Country %in% members, .data$supply_chain %in% pick_stages,
           !is.na(.data$strategic_index), !.data$tech %in% pick_excluded_techs) %>%
    group_by(.data$Country) %>%
    slice_max(order_by = .data$strategic_index, n = 1) %>%
    ungroup()
}

pick_old <- select_pick(si_old, shipped_list) %>%
  select("Country", shipped_sector = "sector", shipped_si = "strategic_index")
pick_new <- select_pick(new$si, coalition) %>%
  select("Country", corrected_sector = "sector", corrected_si = "strategic_index")
pick_new_unscreened <- select_pick(new$si, coalition, screen = FALSE) %>%
  select("Country", unscreened_sector = "sector")
stopifnot(!anyDuplicated(pick_old$Country), !anyDuplicated(pick_new$Country),
          !anyDuplicated(pick_new_unscreened$Country))
allied_csv <- utils::read.csv(file.path(processed_dir, "charts", "allied_strategic.csv"),
                              stringsAsFactors = FALSE)
stopifnot(setequal(paste(allied_csv$Country, allied_csv$sector),
                   paste(pick_new$Country, pick_new$corrected_sector)))
shipped_allied_csv <- file.path(shipped_processed, "charts", "allied_strategic.csv")
shipped_allied_check <- if (file.exists(shipped_allied_csv)) {
  sa <- utils::read.csv(shipped_allied_csv, stringsAsFactors = FALSE)
  setequal(paste(standardize_country_names(sa$Country), sa$sector),
           paste(pick_old$Country, pick_old$shipped_sector))
} else {
  NA
}

# Why a member scored in both releases changed its pick. a = shipped pick, b = corrected pick.
# If a is not scored in the final SI, its exclusion is the cause; if a is scored but barred by the
# endowment screen, the screen is; if b was not scored in the shipped SI, the shipped release
# could not pick it. Otherwise the reversal gap, (SI_new(b) - SI_new(a)) - (SI_old(b) - SI_old(a)),
# splits exactly into the five weighted terms and the largest term names the cause.
term_label <- c(pol = "policy term", eo = "EO term", need = "security-need term", weights = "weights")
pick_cause <- function(country, a, b) {
  new_a <- new$si %>% filter(.data$Country == country, .data$sector == a)
  if (nrow(new_a) == 0 || is.na(new_a$strategic_index)) {
    ex <- if (nrow(new_a)) new_a$si_exclusion else "sector not in the final table"
    why <- c(if (grepl("missing energy security", ex)) "no ES value",
             if (grepl("economic opportunity not rankable", ex)) "EO not rankable",
             if (grepl("energy security not rankable", ex)) "ES not rankable")
    why <- if (length(why)) paste(why, collapse = ", ") else ex
    why <- sub("^EO not rankable, ES not rankable$", "EO and ES not rankable", why)
    return(list(cause = paste0("shipped pick not scored: ", why),
                kind = "exclusion", term = NA_character_, gap = NA_real_, exclusion = ex))
  }
  if (new_a$resource_base %in% FALSE) {
    return(list(cause = "shipped pick screened: no resource base", kind = "screen",
                term = NA_character_, gap = NA_real_))
  }
  ta_o <- si_terms_old %>% filter(.data$Country == country, .data$sector == a)
  tb_o <- si_terms_old %>% filter(.data$Country == country, .data$sector == b)
  if (nrow(tb_o) == 0) {
    return(list(cause = "final pick not scored in the shipped release", kind = "coverage",
                term = NA_character_, gap = NA_real_))
  }
  ta_n <- si_terms_new %>% filter(.data$Country == country, .data$sector == a)
  tb_n <- si_terms_new %>% filter(.data$Country == country, .data$sector == b)
  contrib <- c(
    pol = si_w[["pol"]] * ((tb_n$pol - ta_n$pol) - (tb_o$pol - ta_o$pol)),
    eo = si_w[["eo"]] * ((tb_n$eo - ta_n$eo) - (tb_o$eo - ta_o$eo)),
    need = si_w[["need"]] * ((tb_n$need - ta_n$need) - (tb_o$need - ta_o$need)),
    weights = si_w[["sc"]] * ((tb_n$sc - ta_n$sc) - (tb_o$sc - ta_o$sc)) +
      si_w[["tw"]] * ((tb_n$tw - ta_n$tw) - (tb_o$tw - ta_o$tw))
  )
  top <- names(which.max(contrib))
  list(cause = paste0("reordered by the ", term_label[[top]]), kind = "reordered", term = top,
       gap = sum(contrib), contrib = as.list(contrib))
}

allied <- tibble(Country = sort(shipped_list)) %>%
  mutate(in_final_coalition = .data$Country %in% coalition) %>%
  left_join(pick_old, by = "Country") %>%
  left_join(pick_new, by = "Country") %>%
  left_join(pick_new_unscreened, by = "Country") %>%
  left_join(final_coalition_tbl %>% select("Country", "tier"), by = "Country") %>%
  mutate(status = case_when(
    !.data$in_final_coalition ~ "not in final coalition",
    is.na(.data$shipped_sector) & is.na(.data$corrected_sector) ~ "scored in neither",
    is.na(.data$shipped_sector) ~ "added",
    is.na(.data$corrected_sector) ~ "no corrected pick",
    .data$shipped_sector == .data$corrected_sector ~ "unchanged",
    TRUE ~ "changed"
  ))
allied$cause <- NA_character_
allied$cause_kind <- NA_character_
allied$reversal_gap <- NA_real_
pick_cause_detail <- list()
for (i in which(allied$status == "changed")) {
  pc <- pick_cause(allied$Country[i], allied$shipped_sector[i], allied$corrected_sector[i])
  allied$cause[i] <- pc$cause
  allied$cause_kind[i] <- pc$kind
  allied$reversal_gap[i] <- pc$gap
  pick_cause_detail[[allied$Country[i]]] <- pc
}
# The final SI of each member's shipped pick (where scored) and its screen status.
allied <- allied %>%
  left_join(new$si %>% select("Country", shipped_sector = "sector", si_of_shipped_pick_now = "strategic_index",
                              resource_base_of_shipped_pick = "resource_base"),
            by = c("Country", "shipped_sector"))

pick_counts <- function(x) {
  x <- x[!is.na(x)]
  tb <- sort(table(x), decreasing = TRUE)
  as.list(stats::setNames(as.integer(tb), names(tb)))
}
overlap <- allied %>% filter(.data$in_final_coalition)
both <- overlap %>% filter(.data$status %in% c("changed", "unchanged"))
all_final <- final_coalition_tbl %>%
  left_join(pick_new, by = "Country") %>%
  left_join(pick_new_unscreened, by = "Country")
allied_summary <- list(
  shipped_list_n = length(shipped_list),
  final_coalition_n = length(coalition),
  overlap_n = sum(allied$in_final_coalition),
  shipped_list_not_in_final = allied$Country[!allied$in_final_coalition],
  shipped_list_not_in_final_shipped_picks = as.list(stats::setNames(
    allied$shipped_sector[!allied$in_final_coalition], allied$Country[!allied$in_final_coalition])),
  shipped_picks_n = sum(!is.na(allied$shipped_sector)),
  overlap_shipped_scored = sum(!is.na(overlap$shipped_sector)),
  overlap_corrected_scored = sum(!is.na(overlap$corrected_sector)),
  overlap_added = overlap$Country[overlap$status == "added"],
  overlap_scored_in_both = nrow(both),
  overlap_changed = sum(both$status == "changed"),
  overlap_unchanged = sum(both$status == "unchanged"),
  changed = both$Country[both$status == "changed"],
  unchanged = both$Country[both$status == "unchanged"],
  changed_cause_kind = as.list(table(both$cause_kind)),
  changed_causes = as.list(table(both$cause)),
  changed_by_cause = lapply(split(both$Country[both$status == "changed"], both$cause[both$status == "changed"]), sort),
  changed_reordered_by_term = as.list(table(unlist(lapply(pick_cause_detail, function(p) p$term)))),
  shipped_nuclear_upstream_picks = sum(allied$shipped_sector %in% "Nuclear - Upstream"),
  shipped_nuclear_upstream_pickers = allied$Country[allied$shipped_sector %in% "Nuclear - Upstream"],
  shipped_nuclear_upstream_pickers_in_overlap = overlap$Country[overlap$shipped_sector %in% "Nuclear - Upstream"],
  shipped_nuclear_upstream_resource_base_now = as.list(stats::setNames(
    overlap$resource_base_of_shipped_pick[overlap$shipped_sector %in% "Nuclear - Upstream"],
    overlap$Country[overlap$shipped_sector %in% "Nuclear - Upstream"])),
  shipped_pick_counts = pick_counts(allied$shipped_sector),
  overlap_shipped_pick_counts = pick_counts(overlap$shipped_sector),
  overlap_corrected_pick_counts = pick_counts(overlap$corrected_sector),
  overlap_unscreened_pick_counts = pick_counts(overlap$unscreened_sector),
  overlap_screen_changes_pick = overlap$Country[!is.na(overlap$corrected_sector) &
                                                  overlap$corrected_sector != overlap$unscreened_sector],
  final_coalition_picked = sum(!is.na(all_final$corrected_sector)),
  final_coalition_unpicked = sort(all_final$iso3[is.na(all_final$corrected_sector)]),
  final_coalition_pick_counts = pick_counts(all_final$corrected_sector),
  final_coalition_unscreened_pick_counts = pick_counts(all_final$unscreened_sector),
  final_coalition_nuclear_upstream_picks = sum(all_final$corrected_sector %in% "Nuclear - Upstream"),
  final_coalition_nuclear_upstream_picks_unscreened = sum(all_final$unscreened_sector %in% "Nuclear - Upstream"),
  corrected_selection_matches_allied_strategic_csv = TRUE,
  shipped_selection_matches_shipped_allied_strategic_csv = shipped_allied_check
)

# ---------------------------------------------------------------------------------------------
# Network diagnostics
# ---------------------------------------------------------------------------------------------

diag_old <- utils::read.csv(shipped_diag_path, stringsAsFactors = FALSE)
diag_new <- utils::read.csv(file.path(processed_dir, "outputs", "allied_network_diagnostics.csv"),
                            stringsAsFactors = FALSE)
obs_hhi <- utils::read.csv(file.path(processed_dir, "outputs", "allied_network_observed_hhi.csv"),
                           stringsAsFactors = FALSE)
stopifnot(length(unique(diag_new$diversity_lambda)) == 1, length(unique(diag_new$n_nodes)) == 1)
is_square <- function(n) { k <- round(sqrt(n)); k * k == n }
artefact <- diag_old$hhi > 0.99 & is_square(diag_old$n_producers) & diag_old$n_producers > 1
network_summary <- list(
  shipped = list(
    n_stages = nrow(diag_old),
    stages_by_supply_chain = as.list(table(diag_old$supply_chain)),
    method_counts = as.list(table(diag_old$method)),
    n_stages_hhi_one_square_producers = sum(artefact),
    square_roots_k = sort(round(sqrt(diag_old$n_producers[artefact]))),
    n_producers_artefact_range = range(diag_old$n_producers[artefact]),
    n_producers_range = range(diag_old$n_producers),
    hhi_range = range(diag_old$hhi),
    n_stages_three_producers = sum(diag_old$n_producers == 3),
    hhi_range_three_producer_stages = range(diag_old$hhi[diag_old$n_producers == 3]),
    upstream_midstream_stages = sum(diag_old$supply_chain %in% pick_stages),
    upstream_midstream_artefact_stages = sum(artefact & diag_old$supply_chain %in% pick_stages),
    portfolio_converged = sum(diag_old$portfolio_converged %in% TRUE),
    portfolio_iters = unique(diag_old$portfolio_iters),
    portfolio_violations_remaining = unique(diag_old$portfolio_violations_remaining),
    file_mtime = format(shipped_diag_mtime, "%Y-%m-%d")
  ),
  corrected = list(
    n_stages = nrow(diag_new),
    stages_by_supply_chain = as.list(table(diag_new$supply_chain)),
    method_counts = as.list(table(diag_new$method)),
    n_fallbacks = sum(!is.na(diag_new$fallback_reason) & nzchar(diag_new$fallback_reason)),
    n_nodes = unique(diag_new$n_nodes),
    diversity_lambda = unique(diag_new$diversity_lambda),
    node_policy_term = unique(diag_new$node_policy),
    n_producers_range = range(diag_new$n_producers),
    n_producers_median = stats::median(diag_new$n_producers),
    hhi_min = min(diag_new$hhi), hhi_max = max(diag_new$hhi), hhi_median = stats::median(diag_new$hhi),
    effective_producers_median = stats::median(diag_new$effective_producers),
    max_production_share = max(diag_new$max_production_share),
    hhi_tangent_max_error = max(diag_new$hhi_tangent_max_error),
    observed_export_hhi_median = stats::median(obs_hhi$hhi),
    observed_export_hhi_stages = nrow(obs_hhi),
    observed_effective_exporters_median = 1 / stats::median(obs_hhi$hhi),
    portfolio_converged = sum(diag_new$portfolio_converged %in% TRUE),
    portfolio_iters = unique(diag_new$portfolio_iters),
    portfolio_violations_remaining = unique(diag_new$portfolio_violations_remaining),
    n_policy_imputed_max = max(diag_new$n_policy_imputed)
  )
)

# ---------------------------------------------------------------------------------------------
# Partnership Strength Index
# ---------------------------------------------------------------------------------------------

# The earlier release never wrote the PSI to disk (item B4). It is reconstructed from the March
# 2026 top-five tables of the main checkout with the earlier code's logic (scripts/25 and
# build_partnership_strength_index() at c0cd1ce): the unit is the partner *name*, so partners
# without a WDI name pool into one unit with a missing name; a category score is the mean over
# all reporters and all variables of the category (the earlier builder pooled the component
# variables with the index); the weights 0.4/0.4/0.2 are divided by the sum of the weights of the
# categories present, and a NaN development score keeps its weight but contributes nothing.
reconstruct_earlier_psi <- function(fs, op, dv,
                                    w = c(friendshore = 0.4, opportunity = 0.4, development = 0.2)) {
  pick <- function(x) x %>% transmute(Country = .data$partner, category = .data$category, value = .data$value)
  bind_rows(pick(fs), pick(op), pick(dv)) %>%
    group_by(.data$Country, .data$category) %>%
    summarise(category_score = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = unname(w[.data$category])) %>%
    group_by(.data$Country) %>%
    summarise(
      psi = sum(.data$category_score * .data$weight / sum(.data$weight), na.rm = TRUE),
      dev_scored = any(.data$category == "development" & !is.nan(.data$category_score)),
      .groups = "drop"
    )
}

read_shipped_top5 <- function(name) {
  p <- file.path(shipped_processed, paste0(name, ".rds"))
  list(tbl = readRDS(p) %>% select("partner", "partner_iso", "category", "value"),
       mtime = format(file.info(p)$mtime, "%Y-%m-%d"))
}
top5_fs <- read_shipped_top5("partner_friendshore_top5_tbl")
top5_op <- read_shipped_top5("partner_opportunity_top5_tbl")
top5_dv <- read_shipped_top5("partner_development_top5_tbl")
psi_old_raw <- reconstruct_earlier_psi(top5_fs$tbl, top5_op$tbl, top5_dv$tbl)
top5_mtimes <- c(friendshore = top5_fs$mtime, opportunity = top5_op$mtime, development = top5_dv$mtime)
unnamed_pool_iso3 <- bind_rows(top5_fs$tbl, top5_op$tbl, top5_dv$tbl) %>%
  filter(is.na(.data$partner)) %>%
  distinct(.data$partner_iso) %>%
  pull("partner_iso") %>%
  sort()
rm(top5_fs, top5_op, top5_dv)
invisible(gc(verbose = FALSE))
psi_old <- psi_old_raw %>%
  filter(!is.na(.data$Country)) %>%
  mutate(iso3c = country_resolve(.data$Country)$iso3c) %>%
  filter(!is.na(.data$iso3c))
stopifnot(!anyDuplicated(psi_old$iso3c))
read_psi <- function(file) {
  readRDS(file.path(processed_dir, file)) %>%
    transmute(Country = .data$Country, iso3c = .data$partner_iso, psi = .data$value,
              rankable = .data$rankable, rank = .data$rank)
}
psi_core <- read_psi("psi_tbl.rds")
psi_world <- read_psi("psi_world_tbl.rds")
stopifnot(!anyDuplicated(psi_core$iso3c), !anyDuplicated(psi_world$iso3c))
psi_old_ranked <- psi_old_raw %>% mutate(iso3c = country_resolve(.data$Country)$iso3c)
rank_old <- function(iso) {
  r <- rank(-psi_old_ranked$psi, ties.method = "min")
  r[match(iso, psi_old_ranked$iso3c)]
}
psi_compare <- function(psi_new) {
  m <- inner_join(psi_old %>% select("iso3c", shipped = "psi", "dev_scored"),
                  psi_new %>% select("iso3c", "Country", corrected = "psi", "rankable", "rank"), by = "iso3c")
  mr <- m %>% filter(.data$rankable %in% TRUE)
  # Earlier ranks re-taken over the rankable matched partners, so both rankings span one population.
  mr <- mr %>% mutate(rank_shipped = rank(-.data$shipped, ties.method = "min"),
                      rank_corrected = rank(-.data$corrected, ties.method = "min"))
  rk <- function(iso) {
    x <- mr[mr$iso3c == iso, ]
    if (nrow(x)) list(shipped = x$rank_shipped, corrected = x$rank_corrected, corrected_published = x$rank,
                      n = nrow(mr)) else NULL
  }
  list(
    n_partners_matched = nrow(m),
    n_partners_rankable = nrow(mr),
    spearman_all = spearman(m$shipped, m$corrected),
    pearson_all = stats::cor(m$shipped, m$corrected),
    spearman = spearman(mr$shipped, mr$corrected),
    pearson = stats::cor(mr$shipped, mr$corrected),
    spearman_dev_scored = spearman(mr$shipped[mr$dev_scored], mr$corrected[mr$dev_scored]),
    n_dev_scored = sum(mr$dev_scored),
    spearman_dev_not_scored = spearman(mr$shipped[!mr$dev_scored], mr$corrected[!mr$dev_scored]),
    n_dev_not_scored = sum(!mr$dev_scored),
    coalition_n = sum(mr$iso3c %in% final_coalition_tbl$iso3),
    coalition_spearman = spearman(mr$shipped[mr$iso3c %in% final_coalition_tbl$iso3],
                                  mr$corrected[mr$iso3c %in% final_coalition_tbl$iso3]),
    usa = rk("USA"), jpn = rk("JPN"), chn = rk("CHN"), deu = rk("DEU")
  )
}
psi_summary <- list(
  note = paste("Earlier PSI reconstructed from the shipped March 2026 top-five tables with the earlier",
               "code's logic (a mean over all reporters, i.e. a world view); matched on ISO3 to the",
               "final core view (psi_tbl, headline) and world view (psi_world_tbl); rank statistics over",
               "partners rankable in the final release; Spearman unless labelled pearson."),
  shipped_top5_mtime = as.list(top5_mtimes),
  shipped_units = nrow(psi_old_raw),
  shipped_unnamed_pool = sum(is.na(psi_old_raw$Country)),
  shipped_unnamed_pool_n_partners = length(unnamed_pool_iso3),
  shipped_unnamed_pool_includes_taiwan = "TWN" %in% unnamed_pool_iso3,
  shipped_units_with_iso3 = nrow(psi_old),
  shipped_mean_dev_scored = mean(psi_old_raw$psi[psi_old_raw$dev_scored]),
  shipped_mean_dev_not_scored = mean(psi_old_raw$psi[!psi_old_raw$dev_scored]),
  shipped_rank_usa = rank_old("USA"), shipped_rank_jpn = rank_old("JPN"), shipped_rank_chn = rank_old("CHN"),
  shipped_rank_n = nrow(psi_old_raw),
  corrected_partners = nrow(psi_core),
  corrected_rankable_core = sum(psi_core$rankable %in% TRUE),
  corrected_rankable_world = sum(psi_world$rankable %in% TRUE),
  core_view = psi_compare(psi_core),
  world_view = psi_compare(psi_world)
)

# ---------------------------------------------------------------------------------------------
# Construction facts behind the largest changes
# ---------------------------------------------------------------------------------------------

# Categories present in a cell of a release, with their cross-country spread and mean.
cell_categories <- function(cc, tech, stage) {
  cc %>%
    filter(.data$tech == !!tech, .data$supply_chain == stage) %>%
    group_by(.data$category) %>%
    summarise(n = dplyr::n(), mean = mean(.data$category_score, na.rm = TRUE),
              sd = stats::sd(.data$category_score, na.rm = TRUE),
              weight = mean(.data$category_weight),
              imputed_share = mean((.data$imputed %in% TRUE) | !is.na(.data$missing_rule_applied)),
              .groups = "drop")
}
nuc_up_new <- new$es %>% filter(.data$tech == "Nuclear", .data$supply_chain == "Upstream")
nuc_up_mode <- as.numeric(names(which.max(table(round(nuc_up_new$Energy_Security_Index, 6)))))
construction_facts <- list(
  note = paste("Category composition of the cells behind the largest changes: n countries, mean and sd",
               "of the category score across countries, mean normalised weight, share imputed (imputed",
               "flag or a recorded missing-data rule)."),
  es_nuclear_upstream_shipped = cell_categories(old$es_cc, "Nuclear", "Upstream"),
  es_nuclear_upstream_corrected = cell_categories(new$es_cc, "Nuclear", "Upstream"),
  es_nuclear_upstream_corrected_n = nrow(nuc_up_new),
  es_nuclear_upstream_corrected_modal_value = nuc_up_mode,
  es_nuclear_upstream_corrected_n_at_mode = sum(abs(nuc_up_new$Energy_Security_Index - nuc_up_mode) < 1e-6),
  es_nuclear_upstream_corrected_rankable = sum(nuc_up_new$rankable %in% TRUE),
  # Coalition members in Nuclear Upstream: the shipped ES was 0 for every one of them; the final ES
  # rests on Red Book resources and mine production, and the members without either sit at the
  # floor value of the cell.
  es_nuclear_upstream_coalition = local({
    cm <- nuc_up_new %>% filter(.data$Country %in% coalition)
    rb <- new$si %>% filter(.data$sector == "Nuclear - Upstream", .data$Country %in% coalition)
    list(n = nrow(cm), at_floor = sum(abs(cm$Energy_Security_Index - nuc_up_mode) < 1e-6),
         range = range(cm$Energy_Security_Index),
         with_recorded_uranium = sum(rb$resource_base %in% TRUE),
         without_recorded_uranium = sum(rb$resource_base %in% FALSE))
  }),
  # Electric Grid Upstream: electricity (HS 271600) left the basket and a copper/aluminium input
  # basket entered (basket review), and its mineral-supply and reserves inputs were rebuilt.
  es_grid_upstream_shipped = cell_categories(old$es_cc, "Electric Grid", "Upstream"),
  es_grid_upstream_corrected = cell_categories(new$es_cc, "Electric Grid", "Upstream"),
  eo_grid_upstream_shipped = cell_categories(old$eo_cc, "Electric Grid", "Upstream"),
  eo_grid_upstream_corrected = cell_categories(new$eo_cc, "Electric Grid", "Upstream"),
  es_nuclear_midstream_shipped = cell_categories(old$es_cc, "Nuclear", "Midstream"),
  es_nuclear_midstream_corrected = cell_categories(new$es_cc, "Nuclear", "Midstream"),
  eo_green_hydrogen_midstream_shipped = cell_categories(old$eo_cc, "Green Hydrogen", "Midstream"),
  eo_green_hydrogen_midstream_corrected = cell_categories(new$eo_cc, "Green Hydrogen", "Midstream"),
  eo_nuclear_upstream_shipped = cell_categories(old$eo_cc, "Nuclear", "Upstream"),
  eo_nuclear_upstream_corrected = cell_categories(new$eo_cc, "Nuclear", "Upstream")
)

# ---------------------------------------------------------------------------------------------
# Rerun facts used to close Appendix D placeholders
# ---------------------------------------------------------------------------------------------

pe <- new$policy_eligibility
all_sectors <- paste(rep(headline_techs, each = length(stages)), stages, sep = " - ")
si_tbl_sectors <- sort(unique(new$si$sector))
coalition_ineligible <- final_coalition_tbl %>%
  filter(!.data$Country %in% pe$Country[pe$eligible]) %>%
  left_join(pe %>% select("Country", "missing_components"), by = "Country")
rerun_facts <- list(
  si_headline_sectors = length(all_sectors),
  si_sectors_in_corrected_table = length(si_tbl_sectors),
  si_sectors_not_in_corrected_table = setdiff(all_sectors, si_tbl_sectors),
  si_sectors_scored = length(si_sectors_new),
  si_sectors_in_table_without_score = setdiff(si_tbl_sectors, si_sectors_new),
  si_cells = nrow(si_new_raw),
  si_countries = dplyr::n_distinct(si_new_raw$Country),
  policy_eligible_countries = sum(pe$eligible),
  policy_eligibility_rows = nrow(pe),
  # The component table keeps the NIPO and dual-use rows on disk; only the components below enter
  # the final Policy index.
  policy_component_table_variables = new$policy_variables,
  policy_index_components = new$policy_index_components,
  policy_index_weights = as.list(new$policy_index_weights),
  policy_excluded_components = names(new$policy_excluded_components),
  policy_basis_counts = as.list(table(pe$policy_basis[pe$eligible])),
  coalition_members_by_policy_basis = as.list(table(
    pe$policy_basis[match(final_coalition_tbl$Country, pe$Country)], useNA = "ifany")),
  coalition_members_iea_pams_only = sort(final_coalition_tbl$iso3[
    pe$policy_basis[match(final_coalition_tbl$Country, pe$Country)] %in% "IEA PAMS only"]),
  shipped_list_members_by_policy_basis = as.list(table(
    pe$policy_basis[match(shipped_list, pe$Country)], useNA = "ifany")),
  coalition_members_policy_eligible = sum(coalition %in% pe$Country[pe$eligible]),
  coalition_members_not_policy_eligible = sort(coalition_ineligible$iso3),
  coalition_members_not_policy_eligible_missing = as.list(table(coalition_ineligible$missing_components, useNA = "ifany")),
  shipped_list_members_policy_eligible = sum(shipped_list %in% pe$Country[pe$eligible]),
  es_category_scores_imputed_share = new$es_cs_imputed,
  eo_category_scores_imputed_share = new$eo_cs_imputed,
  es_diagnostics_imputed_share = new$es_imputed_share,
  eo_diagnostics_imputed_share = new$eo_imputed_share,
  es_constant_cells_corrected = with(new$es_cells, paste(tech, supply_chain)[cell_status == "constant" & tech %in% headline_techs]),
  eo_constant_cells_corrected = with(new$eo_cells, paste(tech, supply_chain)[cell_status == "constant" & tech %in% headline_techs]),
  network_policy_imputed_max = max(diag_new$n_policy_imputed),
  network_node_policy_term = unique(diag_new$node_policy),
  coalition_nonreporters_2024 = coalition_nonreporters_2024,
  import_reporters_2024 = length(import_reporters_2024)
)
stopifnot(abs(new$es_cs_imputed - new$es_imputed_share) < 1e-12,
          abs(new$eo_cs_imputed - new$eo_imputed_share) < 1e-12)

# ---------------------------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------------------------

stage_scale <- scale_colour_manual(values = paper_stage_cols, breaks = stages, name = NULL)
nr_col <- "#d9d8d2"

# (1) Shipped against corrected, three panels. Pillar cells that the final release does not rank
# are drawn in light gray underneath.
scatter_df <- bind_rows(
  es$matched_nonconstant %>% transmute(panel = "Energy Security", stage = .data$stage, shipped = .data$shipped,
                                       corrected = .data$corrected, rankable = .data$rankable %in% TRUE),
  eo$matched_nonconstant %>% transmute(panel = "Economic Opportunity", stage = .data$stage, shipped = .data$shipped,
                                       corrected = .data$corrected, rankable = .data$rankable %in% TRUE),
  si_m %>% transmute(panel = "Strategic Index", stage = .data$stage, shipped = .data$shipped,
                     corrected = .data$corrected, rankable = TRUE)
) %>%
  mutate(panel = factor(.data$panel, levels = c("Energy Security", "Economic Opportunity", "Strategic Index")))
panel_lims <- scatter_df %>%
  group_by(.data$panel) %>%
  summarise(lo = floor(min(c(.data$shipped, .data$corrected)) * 10) / 10,
            hi = ceiling(max(c(.data$shipped, .data$corrected)) * 10) / 10, .groups = "drop")
blank_df <- bind_rows(
  panel_lims %>% transmute(panel = .data$panel, shipped = .data$lo, corrected = .data$lo),
  panel_lims %>% transmute(panel = .data$panel, shipped = .data$hi, corrected = .data$hi)
)
# Plot the dense Midstream/Downstream layers first so the Upstream points are not buried.
scatter_df <- scatter_df %>% arrange(desc(as.integer(.data$stage)))
p_scatter <- ggplot(scatter_df, aes(.data$shipped, .data$corrected)) +
  geom_blank(data = blank_df) +
  geom_point(data = function(d) d[!d$rankable, ], aes(shape = "Not rankable in final release"),
             colour = nr_col, size = 0.45, stroke = 0) +
  geom_abline(slope = 1, intercept = 0, colour = paper_ink$muted, linewidth = 0.35) +
  geom_point(data = function(d) d[d$rankable & d$panel != "Strategic Index", ], aes(colour = .data$stage),
             size = 0.45, alpha = 0.4, stroke = 0) +
  # The SI panel has far fewer cells, so its points can be larger and more opaque.
  geom_point(data = function(d) d[d$panel == "Strategic Index", ], aes(colour = .data$stage),
             size = 0.9, alpha = 0.65, stroke = 0) +
  facet_wrap(~panel, scales = "free") +
  stage_scale +
  scale_shape_manual(values = c("Not rankable in final release" = 16), name = NULL) +
  scale_x_continuous(breaks = scales::breaks_pretty(4), expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = scales::breaks_pretty(4), expand = expansion(mult = 0.02)) +
  labs(x = "Shipped release (September 2026)", y = "Final release") +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2.2, alpha = 1)),
         shape = guide_legend(order = 2, override.aes = list(size = 2.2, colour = "#b5b4ac"))) +
  theme_paper(grid = "xy") +
  theme(aspect.ratio = 1, panel.spacing.x = unit(0.5, "cm"))
save_paper_fig(p_scatter, "reconcile_scatter", width = paper_width_full, height = 2.85)

# (2) Cells: level shift against within-cell rank agreement (rankable cells).
cells_df <- bind_rows(
  es$cells %>% mutate(panel = "Energy Security"),
  eo$cells %>% mutate(panel = "Economic Opportunity")
) %>%
  filter(!is.na(.data$spearman)) %>%
  mutate(panel = factor(.data$panel, levels = c("Energy Security", "Economic Opportunity")),
         short = paste(.data$tech, .data$supply_chain)) %>%
  group_by(.data$panel) %>%
  mutate(label = if_else(rank(.data$spearman) <= 3 | rank(-abs(.data$mean_change)) <= 3,
                         .data$short, NA_character_)) %>%
  ungroup() %>%
  # Labels go below their point and away from the vertical zero line, where the cloud of cells sits.
  mutate(nudge_x = if_else(.data$mean_change < 0, -0.03, 0.03),
         nudge_y = case_when(.data$spearman < 0.7 ~ -0.1,
                             abs(.data$mean_change) > 0.05 & .data$spearman > 0.8 ~ 0.1,
                             abs(.data$mean_change) > 0.05 ~ -0.13,
                             TRUE ~ 0.08))
p_cells <- ggplot(cells_df, aes(.data$mean_change, .data$spearman)) +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_point(aes(colour = .data$stage), size = 1.8, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = .data$label), size = 2.65, colour = paper_ink$secondary,
                           position = ggrepel::position_nudge_repel(x = cells_df$nudge_x, y = cells_df$nudge_y),
                           min.segment.length = 0, segment.colour = paper_ink$muted,
                           segment.size = 0.25, box.padding = 0.5, point.padding = 0.4,
                           force = 4, seed = 11, na.rm = TRUE, max.overlaps = Inf) +
  facet_wrap(~panel) +
  stage_scale +
  scale_y_continuous(limits = c(NA, 1), breaks = scales::breaks_pretty(5)) +
  scale_x_continuous(breaks = scales::breaks_pretty(5)) +
  labs(x = "Mean change in the cell (final minus shipped)",
       y = "Within-cell Spearman correlation") +
  theme_paper(grid = "xy")
save_paper_fig(p_cells, "reconcile_cells", width = paper_width_full, height = 3.3)

# (3) ES and EO movers. The figure carries a compact form of the cause (the table gives it in full).
compact_cause <- function(x) {
  reps <- c(
    "uranium Reserves and Production added (Red Book)" = "uranium resources and output added (Red Book)",
    "Production added (E1, E3; uranium, Red Book)" = "uranium output added (Red Book)",
    "Zero Investment cell removed (E7)" = "zero Investment removed (E7)",
    "zero Investment cell removed (E7)" = "zero Investment removed (E7)",
    "all-reporter dyads" = "dyads",
    "Spellings merged (C1)" = "spellings merged (C1)",
    # Shorter still, so that every cause fits on two lines of the figure.
    "uranium resources and output added (Red Book)" = "Red Book uranium added",
    "uranium output added (Red Book)" = "Red Book uranium added",
    "Energy Prices added (A4)" = "prices added (A4)",
    "; dyads, mirror imports)" = ", mirror)",
    "; dyads, basket review)" = ", baskets)",
    "; dyads)" = ")",
    "Minerals Trade observed, was imputed (E6)" = "Minerals Trade observed (E6)",
    "Foreign Dependency rebuilt" = "Foreign Dep. rebuilt"
  )
  for (k in names(reps)) x <- gsub(k, reps[[k]], x, fixed = TRUE)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
movers_df <- bind_rows(movers_es, movers_eo) %>%
  mutate(panel = factor(.data$pillar, levels = c("Energy Security", "Economic Opportunity")),
         stage = factor(.data$supply_chain, levels = stages),
         row_label = paste0(.data$iso3, "  ", .data$tech, " ", .data$supply_chain),
         cause_wrapped = vapply(compact_cause(.data$cause),
                                function(x) paste(strwrap(x, width = 70), collapse = "\n"), character(1)),
         row_id = paste(.data$panel, .data$rank))
movers_df$row_id <- factor(movers_df$row_id, levels = rev(movers_df$row_id))
row_labels <- stats::setNames(movers_df$row_label, movers_df$row_id)
mv_lo <- floor(min(c(movers_df$shipped, movers_df$corrected)) * 10) / 10
mv_hi <- ceiling(max(c(movers_df$shipped, movers_df$corrected)) * 10) / 10
cause_x <- mv_hi + 0.045 * (mv_hi - mv_lo)
p_movers <- ggplot(movers_df, aes(y = .data$row_id)) +
  geom_segment(aes(x = .data$shipped, xend = .data$corrected, yend = .data$row_id),
               colour = paper_ink$axis, linewidth = 0.45) +
  geom_point(aes(x = .data$shipped, shape = "Shipped release"), colour = paper_ink$muted, fill = "white",
             size = 1.7, stroke = 0.5) +
  geom_point(aes(x = .data$corrected, colour = .data$stage, shape = "Final release"), size = 1.8) +
  geom_text(aes(x = cause_x, label = .data$cause_wrapped), hjust = 0, size = 2.5,
            lineheight = 0.85, colour = paper_ink$secondary) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_discrete(labels = row_labels) +
  scale_shape_manual(values = c("Shipped release" = 21, "Final release" = 16),
                     breaks = c("Shipped release", "Final release"), name = NULL) +
  stage_scale +
  scale_x_continuous(breaks = scales::breaks_pretty(4), expand = expansion(add = 0.01)) +
  coord_cartesian(xlim = c(mv_lo, mv_hi), clip = "off") +
  labs(x = "Index value", y = NULL) +
  guides(shape = guide_legend(order = 1, override.aes = list(colour = paper_ink$secondary)),
         colour = guide_legend(order = 2)) +
  theme_paper(grid = "x") +
  theme(plot.margin = margin(6, 236, 6, 6),
        axis.text.y = element_text(size = 7.5),
        legend.box = "horizontal")
save_paper_fig(p_movers, "reconcile_movers", width = paper_width_full, height = 6.9)

# (4) SI movers: the change split into the five weighted terms.
term_levels <- c("Policy term (0.2 pol)", "EO term (0.3 EO)", "Security-need term (0.3 (1 \u2212 ES))",
                 "Stage and technology weights")
term_cols <- stats::setNames(
  c(paper_pillar_cols[["Policy"]], paper_pillar_cols[["Economic Opportunity"]],
    paper_pillar_cols[["Energy Security"]], paper_pillar_cols[["Supply-chain weight"]]),
  term_levels)
si_mv_df <- si_movers %>%
  mutate(row_label = paste0(.data$iso3, "  ", .data$sector),
         row_id = factor(.data$rank, levels = rev(.data$rank)))
si_mv_long <- si_mv_df %>%
  select("row_id", "d_pol", "d_eo", "d_need", "d_weights") %>%
  pivot_longer(-"row_id", names_to = "term", values_to = "effect") %>%
  mutate(term = factor(c(d_pol = term_levels[1], d_eo = term_levels[2], d_need = term_levels[3],
                         d_weights = term_levels[4])[.data$term], levels = term_levels))
si_row_labels <- stats::setNames(gsub(" - ", " ", si_mv_df$row_label), as.character(si_mv_df$row_id))
p_si <- ggplot() +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_col(data = si_mv_long, aes(x = .data$effect, y = .data$row_id, fill = .data$term),
           width = 0.62, orientation = "y") +
  geom_point(data = si_mv_df, aes(x = .data$change, y = .data$row_id, shape = "Net change"),
             size = 1.7, colour = paper_ink$primary) +
  scale_fill_manual(values = term_cols, breaks = term_levels, name = NULL, drop = FALSE) +
  scale_shape_manual(values = c("Net change" = 18), name = NULL) +
  scale_y_discrete(labels = si_row_labels) +
  scale_x_continuous(breaks = scales::breaks_pretty(6)) +
  labs(x = "Change in the Strategic Index (final minus shipped), by term", y = NULL) +
  guides(fill = guide_legend(order = 1, nrow = 2), shape = guide_legend(order = 2)) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 7.5))
save_paper_fig(p_si, "reconcile_si_terms", width = paper_width_full, height = 3.6)

# ---------------------------------------------------------------------------------------------
# Tables
# ---------------------------------------------------------------------------------------------

write_paper_table <- function(lines, name) {
  out_dir <- paper_dir("tables")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  writeLines(enc2utf8(lines), file.path(out_dir, paste0(name, ".tex")), useBytes = TRUE)
}
f3 <- function(x) ifelse(is.na(x), "--", formatC(x, format = "f", digits = 3))
tex_escape <- function(x) gsub("&", "\\\\&", gsub("%", "\\\\%", x))
tex_dash <- function(x) gsub("\u2013", "--", x)
sector_tex <- function(x) ifelse(is.na(x), "--", gsub(" - ", ", ", x))

# Allied picks, per member of the shipped list.
status_tex <- c("not in final coalition" = "not in final coalition", "added" = "new",
                "unchanged" = "unchanged", "changed" = "changed", "no corrected pick" = "no final pick",
                "scored in neither" = "--")
allied_rows <- allied %>%
  mutate(line = paste0(
    .data$Country, " & ", sector_tex(.data$shipped_sector), " & ", f3(.data$shipped_si), " & ",
    ifelse(.data$in_final_coalition, sector_tex(.data$corrected_sector), "\\emph{not a member}"), " & ",
    f3(.data$corrected_si), " & ",
    ifelse(is.na(.data$cause), status_tex[.data$status], tex_escape(.data$cause)), " \\\\"
  ))
fmt_counts <- function(x) paste0(gsub(" - ", " ", names(x)), " (", unlist(x), ")", collapse = ", ")
write_paper_table(c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "  \\caption{Allied pick per member of the shipped list: shipped and final release}",
  "  \\label{tab:reconcile_allied}",
  "  \\footnotesize",
  paste0("  \\begin{tabularx}{\\textwidth}{@{}l >{\\raggedright\\arraybackslash}p{1.15in} c ",
         ">{\\raggedright\\arraybackslash}p{1.15in} c >{\\raggedright\\arraybackslash}X@{}}"),
  "    \\toprule",
  "    & \\multicolumn{2}{c}{Shipped release} & \\multicolumn{2}{c}{Final release} & \\\\",
  "    \\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "    Member & Pick & $\\SI$ & Pick & $\\SI$ & Status or cause of change \\\\",
  "    \\midrule",
  paste0("    ", allied_rows$line),
  "    \\bottomrule",
  "  \\end{tabularx}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} The pick is the member's Upstream or Midstream sector with the highest ",
         "Strategic Index; Geothermal and Green Hydrogen are excluded by rule of the selection. Shipped ",
         "release: the Strategic Index of 3~September 2026 over its hand-picked list of ",
         allied_summary$shipped_list_n, " economies, of which ", allied_summary$shipped_picks_n,
         " had a pick. Final release: \\texttt{data/processed/charts/allied\\_strategic.csv}, over the ",
         "derived coalition of ", allied_summary$final_coalition_n, " members, with the endowment screen ",
         "(an Upstream extraction stage is eligible only where the member has recorded reserves or ",
         "production of its resources). The comparison is on the ", allied_summary$overlap_n,
         " members the two lists share; ", length(allied_summary$shipped_list_not_in_final),
         " economies of the shipped list are not in the final coalition. ``new'': a pick only in the final ",
         "release. Cause of a change: the shipped pick is no longer scored (its exclusion), is barred by the ",
         "endowment screen, or is outscored, in which case the term of the index that contributes most to ",
         "the reversal of the two sectors' order is named (an exact split of the reversal into the five ",
         "weighted terms). The picks of the other ",
         allied_summary$final_coalition_n - allied_summary$overlap_n,
         " final members are reported in Section~\\ref{sec:strategic}."),
  "  \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "reconcile_allied")

# Movers with their decomposition.
tex_signed <- function(x) sub("^-", "$-$", sprintf("%+.3f", ifelse(abs(x) < 5e-4, 0, x)))
mover_rows <- bind_rows(movers_es, movers_eo) %>%
  mutate(line = paste0(
    .data$Country, " & ", .data$tech, " ", .data$supply_chain, " & ", f3(.data$shipped), " & ",
    f3(.data$corrected), " & ", tex_signed(.data$composition_effect), " & ",
    ifelse(is.na(.data$top_score_category), "--",
           paste0(.data$top_score_category, " ", tex_signed(.data$top_score_effect))), " & ",
    tex_escape(tex_dash(.data$cause)), " \\\\"
  ))
mover_block <- function(p) {
  r <- mover_rows %>% filter(.data$pillar == p)
  c(paste0("    \\multicolumn{7}{@{}l}{\\emph{", p, "}} \\\\"), paste0("    ", r$line))
}
rl <- ">{\\raggedright\\arraybackslash}"
# A longtable (not a float): the 24 rows with their attributions run past one page.
mover_head <- "    Member & Cell & Shipped & Final & \\makecell{Composition\\\\effect} & Largest score effect & Attributed cause \\\\"
write_paper_table(c(
  "\\begingroup",
  "  \\singlespacing\\scriptsize",
  "  \\setlength{\\tabcolsep}{3pt}",
  "  \\setlength{\\LTcapwidth}{\\textwidth}",
  paste0("  \\begin{longtable}{@{}", rl, "p{0.75in} ", rl, "p{0.95in} c c c ", rl,
         "p{1.05in} ", rl, "p{1.9in}@{}}"),
  "  \\caption{Largest pillar changes among coalition members and their attribution}",
  "  \\label{tab:reconcile_movers}\\\\",
  "    \\toprule", mover_head, "    \\midrule",
  "  \\endfirsthead",
  "    \\multicolumn{7}{@{}l}{\\small\\textbf{Table \\thetable:} continued}\\\\",
  "    \\toprule", mover_head, "    \\midrule",
  "  \\endhead",
  "    \\bottomrule",
  "  \\endfoot",
  mover_block("Energy Security"),
  "    \\addlinespace",
  mover_block("Economic Opportunity"),
  "  \\end{longtable}",
  "  \\vspace{-6pt}\\footnotesize\\noindent",
  paste0("    \\emph{Notes:} The ", n_movers, " largest absolute changes, final minus shipped release, of ",
         "$\\ES_{its}$ and of $\\EO_{its}$ among the ", length(coalition), " members of the final coalition, at most ",
         n_movers_per_cell, " per technology--stage cell, over ",
         "the headline cells present in both releases that the final release ranks (sovereign economy, at most ",
         "half of the index weight imputed); cells that were constant across countries in the shipped release ",
         "are included. The change is split by a two-factor Shapley decomposition ",
         "of the weighted category mean: the score effect of a category present in both releases is ",
         "$\\tfrac12(\\omega^{\\text{shipped}}_k+\\omega^{\\text{final}}_k)(c^{\\text{final}}_k-c^{\\text{shipped}}_k)$, ",
         "with $\\omega_k$ the normalised category weight and an unobserved shipped category counted as ",
         "$c_k=0$ (item A1); the composition effect is the remainder, due to categories that enter or leave ",
         "and to the renormalisation of the weights. The cause names the largest effect, and the second ",
         "when it is at least half as large; item codes refer to Table~\\ref{tab:corrections_index}. Data ",
         "changes: ``uranium, Red Book'' = identified recoverable resources and mine production of the ",
         "OECD-NEA/IAEA Red Book; ``USGS MCS 2026'' = critical-mineral reserves of the USGS Mineral Commodity ",
         "Summaries 2026; ``all-reporter dyads'' = trade rebuilt from all-reporter Comtrade dyads ",
         "(importer report first, else the exporter's declaration $\\times 1.06$) and the energy-trade file ",
         "rebuilt from the same pull; ``basket review'' = a stage ",
         "whose HS6 basket the October 2026 review changed; ``mirror imports'' = an economy that did not report ",
         "2024 imports, whose imports come from partners' declarations; ``TRL by stage'' = technology readiness ",
         "mapped by stage from the IEA Clean Technology Guide. ",
         "Source: \\texttt{category\\_contributions} of the two \\texttt{index\\_outputs.rds}.\\par"),
  "\\endgroup",
  "\\medskip"
), "reconcile_movers")

# Network diagnostics.
ns <- network_summary
write_paper_table(c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "  \\caption{Allied network diagnostics: shipped and final release}",
  "  \\label{tab:reconcile_network}",
  "  \\small",
  "  \\begin{tabular}{@{}l c c@{}}",
  "    \\toprule",
  "    & Shipped release & Final release \\\\",
  "    \\midrule",
  paste0("    Network nodes & 22 & ", ns$corrected$n_nodes, " \\\\"),
  paste0("    Stages designed & ", ns$shipped$n_stages, " (all three stages) & ", ns$corrected$n_stages,
         " (Upstream, Midstream) \\\\"),
  paste0("    Solver used (stages) & greedy (", ns$shipped$method_counts$greedy, ") & MILP (",
         ns$corrected$method_counts$milp, ") \\\\"),
  paste0("    Stages with $n_{\\text{producers}}=k^2$ and $\\HHI=1$ & ", ns$shipped$n_stages_hhi_one_square_producers,
         " & 0 \\\\"),
  paste0("    Reported producers per stage & ", ns$shipped$n_producers_range[1], "--",
         format(ns$shipped$n_producers_range[2], big.mark = "{,}"), " & ", ns$corrected$n_producers_range[1], "--",
         ns$corrected$n_producers_range[2], " (median ", ns$corrected$n_producers_median, ") \\\\"),
  paste0("    $\\HHI$ of designed shares & ", sprintf("%.3f", ns$shipped$hhi_range[1]), "--",
         sprintf("%.3f", ns$shipped$hhi_range[2]), " & ", sprintf("%.3f", ns$corrected$hhi_min), "--",
         sprintf("%.3f", ns$corrected$hhi_max), " (median ", sprintf("%.4f", ns$corrected$hhi_median), ") \\\\"),
  paste0("    Diversification weight $\\lambda$ & -- & ", sprintf("%.2f", ns$corrected$diversity_lambda), " \\\\"),
  paste0("    Policy term in node scores & yes & ", if (isTRUE(ns$corrected$node_policy_term)) "yes" else "no", " \\\\"),
  paste0("    Portfolio loop converged & ", if (ns$shipped$portfolio_converged == ns$shipped$n_stages) "yes" else "no",
         " & ", if (ns$corrected$portfolio_converged == ns$corrected$n_stages) "yes" else "no", " \\\\"),
  paste0("    Portfolio passes & ", paste(ns$shipped$portfolio_iters, collapse = ", "), " (limit) & ",
         paste(ns$corrected$portfolio_iters, collapse = ", "), " \\\\"),
  paste0("    Members above their portfolio cap at exit & ", paste(ns$shipped$portfolio_violations_remaining, collapse = ", "),
         " & ", paste(ns$corrected$portfolio_violations_remaining, collapse = ", "), " \\\\"),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} Shipped release: \\texttt{allied\\_network\\_diagnostics.csv} written on ",
         "23~February 2026, the only shipped network artefact read here; it designed every stage of the ",
         "hand-picked list's 22 effective members (the United Arab Emirates was coded ``UAE'', item N5). In ",
         ns$shipped$n_stages_hhi_one_square_producers, " shipped stages a single country was selected ",
         "with $k^2$ rows of share $1/k$, so the producer count was $k^2$ (with $k$ from ",
         min(ns$shipped$square_roots_k), " to ", max(ns$shipped$square_roots_k), ") and the Herfindahl ",
         "index summed over the rows was exactly one (item N1, equation~\\eqref{eq:corr_fanout}). Final ",
         "release: the same file of the final run of 12~October 2026, ", ns$corrected$n_nodes,
         " nodes (the EU-27 as one node), Upstream and Midstream stages only, MILP with the concave ",
         "diversification term ($\\lambda=", sprintf("%.2f", ns$corrected$diversity_lambda), "$, the smallest ",
         "value on the calibration grid whose median designed $\\HHI$ does not exceed the observed one) and ",
         "portfolio caps, and no policy term in the node scores; the median designed $\\HHI$ ",
         "compares with a median observed export $\\HHI$ of the nodes of ",
         sprintf("%.4f", ns$corrected$observed_export_hhi_median), " over the ",
         ns$corrected$observed_export_hhi_stages, " stages with an HS6 basket. The portfolio loop runs over ",
         "all stages jointly, so it has one outcome per release; the last row counts the members that, when ",
         "the loop stops, are a top producer in more stages than their cap allows."),
  "  \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "reconcile_network")

# Coverage.
cov_line <- function(lbl, a, b) paste0("    ", lbl, " & ", a, " & ", b, " \\\\")
fmt_n <- function(x) format(x, big.mark = "{,}", trim = TRUE)
cov_block <- function(res, nm) {
  s <- res$summary
  c(paste0("    \\multicolumn{3}{@{}l}{\\emph{", nm, "}} \\\\"),
    cov_line("Rows (country--technology--stage cells)", fmt_n(s$shipped_rows), fmt_n(s$corrected_rows)),
    cov_line("\\quad rankable", "--", fmt_n(s$corrected_rankable_rows)),
    cov_line("Distinct country strings", fmt_n(s$shipped_country_strings), fmt_n(s$corrected_economies)),
    cov_line("\\quad non-country units dropped (rows)", fmt_n(s$shipped_noncountry_rows), "--"),
    cov_line("\\quad merged spellings (rows)", fmt_n(s$shipped_merged_spelling_rows), "--"),
    cov_line("Economies after harmonisation", fmt_n(s$shipped_economies_after_harmonisation), fmt_n(s$corrected_economies)),
    cov_line("Technology--stage pairs scored", s$shipped_tech_stage_pairs, s$corrected_tech_stage_pairs),
    cov_line("Cells in both releases", fmt_n(s$matched_cells_all), fmt_n(s$matched_cells_all)),
    cov_line("\\quad rankable in the final release", "--", fmt_n(s$matched_cells_rankable)),
    cov_line("Cells in one release only", fmt_n(s$removed_cells), fmt_n(s$added_cells)))
}
write_paper_table(c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\begin{threeparttable}",
  "  \\caption{Coverage of the headline tables: shipped and final release}",
  "  \\label{tab:reconcile_coverage}",
  "  \\small",
  "  \\begin{tabular}{@{}l r r@{}}",
  "    \\toprule",
  "    & Shipped release & Final release \\\\",
  "    \\midrule",
  cov_block(es, "Energy Security"),
  "    \\addlinespace",
  cov_block(eo, "Economic Opportunity"),
  "    \\addlinespace",
  "    \\multicolumn{3}{@{}l}{\\emph{Strategic Index}} \\\\",
  cov_line("Scored cells", fmt_n(si_summary$shipped_cells), fmt_n(si_summary$corrected_cells)),
  cov_line("Countries", si_summary$shipped_countries, si_summary$corrected_countries),
  cov_line("Sectors", si_summary$shipped_sectors, si_summary$corrected_sectors),
  cov_line("Cells in both releases", si_summary$n_cells, si_summary$n_cells),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("    \\item \\emph{Notes:} Headline tables of the eleven technologies. Shipped release: ",
         "\\texttt{index\\_outputs.rds} of 3~September 2026; final release: the same file of the final 12~October 2026 ",
         "run. Shipped country names are mapped to canonical names with \\texttt{standardize\\_country\\_names()} ",
         "(item C1); units without an ISO3 code (``Global'', ``USSR'', ``Northern Cyprus'') are dropped (item C2) ",
         "and, where two spellings give the same cell, the record under the canonical spelling is kept. ",
         "``Rankable'': the economy is sovereign and at most half of the cell's index weight is imputed ",
         "(\\texttt{config/rankability.yml}); the shipped release had no such rule. ",
         "``Cells in one release only'' counts shipped cells absent from the final release (first column) and ",
         "final cells absent from the shipped release (second column). A Strategic Index cell of the final ",
         "release requires a Policy index (IEA PAMS, combined with CAT where CAT rates the country) and a ",
         "rankable EO and ES value; a shipped cell required the four-component Policy index of February 2026 ",
         "and imputed missing pillar values (item P7)."),
  "  \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
), "reconcile_coverage")

# ---------------------------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------------------------

mover_records <- function(x) {
  x %>% select("rank", "Country", "iso3", "tech", "supply_chain", "shipped", "corrected", "change",
               "const_shipped", "composition_effect", "score_effect_total", "top_score_category",
               "top_score_effect", "categories_removed", "categories_added", "spelling_merged",
               "primary_driver", "cause")
}
cell_records <- function(cells) {
  cells %>% arrange(.data$spearman) %>%
    transmute(cell = paste(.data$tech, .data$supply_chain), n = .data$n, spearman = .data$spearman,
              mean_change = .data$mean_change, mean_abs_change = .data$mean_abs_change,
              share_rank_shift_gt_10 = .data$share_rank_shift_gt_10,
              level_driver = .data$level_driver, level_driver_mean = .data$level_driver_mean,
              reorder_driver = .data$reorder_driver, reorder_driver_sd = .data$reorder_driver_sd)
}

results <- list(
  sources = list(
    shipped_index_outputs = "data/processed/outputs/index_outputs.rds of the main checkout",
    shipped_index_outputs_mtime = format(shipped_mtime, "%Y-%m-%d %H:%M:%S"),
    shipped_network_diagnostics_mtime = format(shipped_diag_mtime, "%Y-%m-%d %H:%M:%S"),
    corrected_index_outputs = "data/processed/outputs/index_outputs.rds",
    corrected_index_outputs_mtime = format(corrected_mtime, "%Y-%m-%d %H:%M:%S"),
    headline_techs = headline_techs,
    shipped_list_iso3 = shipped_list_iso3,
    final_coalition_iso3 = final_coalition_tbl$iso3,
    final_coalition_tiers = as.list(table(final_coalition_tbl$tier)),
    note = paste("Cells matched on canonical country x technology x stage (SI: country x sector);",
                 "constant cells (zero range across countries in either release) excluded from",
                 "correlations and rank shifts; rank statistics over cells rankable in the final",
                 "release (agreement), all matched cells in agreement_all; Spearman unless labelled pearson.")
  ),
  data_changes = list(
    basket_review = basket_review,
    import_reporters_2024 = length(import_reporters_2024),
    coalition_nonreporters_2024 = coalition_nonreporters_2024
  ),
  coverage = list(es = es$summary, eo = eo$summary),
  agreement = list(es = es$agreement, eo = eo$agreement, si = si_summary),
  agreement_all_matched = list(es = es$agreement_all, eo = eo$agreement_all),
  coalition = list(es = coalition_pillar_facts(es), eo = coalition_pillar_facts(eo),
                   si = list(n_cells = nrow(si_coalition), n_members = dplyr::n_distinct(si_coalition$Country),
                             members = sort(unique(si_coalition$Country)),
                             spearman = spearman(si_coalition$shipped, si_coalition$corrected),
                             share_abs_change_gt_0_1 = mean(abs(si_coalition$change) > 0.1))),
  within_cell_spearman_by_cell = list(es = cell_records(es$cells), eo = cell_records(eo$cells)),
  decomposition = list(
    note = paste("Two-factor Shapley split of final minus shipped over the rankable matched non-constant",
                 "cells: score effect of each common category and the composition effect (categories",
                 "entering or leaving, weight renormalisation). share = share of the summed absolute effects."),
    es_overall = dec_es$overall, eo_overall = dec_eo$overall,
    es_reorder_driver_by_stage = es$cells %>% filter(!is.na(.data$reorder_driver)) %>% count(.data$supply_chain, .data$reorder_driver),
    eo_reorder_driver_by_stage = eo$cells %>% filter(!is.na(.data$reorder_driver)) %>% count(.data$supply_chain, .data$reorder_driver),
    es_level_driver_by_stage = es$cells %>% filter(!is.na(.data$level_driver)) %>% count(.data$supply_chain, .data$level_driver),
    eo_level_driver_by_stage = eo$cells %>% filter(!is.na(.data$level_driver)) %>% count(.data$supply_chain, .data$level_driver)
  ),
  movers = list(
    n = n_movers,
    per_cell_cap = n_movers_per_cell,
    es_uncapped_top = uncapped_top(es),
    eo_uncapped_top = uncapped_top(eo),
    es = mover_records(movers_es),
    eo = mover_records(movers_eo),
    es_category_rows = mover_category_rows("es", es, movers_es),
    eo_category_rows = mover_category_rows("eo", eo, movers_eo),
    es_cells = as.list(table(paste(movers_es$tech, movers_es$supply_chain))),
    eo_cells = as.list(table(paste(movers_eo$tech, movers_eo$supply_chain))),
    es_primary_driver = as.list(table(movers_es$primary_driver)),
    eo_primary_driver = as.list(table(movers_eo$primary_driver)),
    es_pool = pool_summary(pool_es),
    eo_pool = pool_summary(pool_eo)
  ),
  si_movers = si_movers %>% select("rank", "Country", "iso3", "sector", "shipped", "corrected", "change",
                                   "d_pol", "d_eo", "d_need", "d_weights", "pol_old", "pol_new", "main_term"),
  si_movers_main_term = as.list(table(si_movers$main_term)),
  allied = c(allied_summary, list(table = allied, pick_cause_detail = pick_cause_detail)),
  network = network_summary,
  psi = psi_summary,
  construction = construction_facts,
  rerun_facts = rerun_facts
)
write_paper_results(results, "reconcile")

message("reconcile: ES matched rankable ", es$agreement$n_cells, " (Spearman ", round(es$agreement$spearman, 3),
        "), EO ", eo$agreement$n_cells, " (", round(eo$agreement$spearman, 3),
        "), SI ", si_summary$n_cells, " (", round(si_summary$spearman, 3),
        "), PSI core ", psi_summary$core_view$n_partners_rankable, " (", round(psi_summary$core_view$spearman, 3), ").")
