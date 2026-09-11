# ==============================================================================
# Attribution ladder: which fix moved the ranking, and by how much
# ------------------------------------------------------------------------------
# Rebuilds by_tech_sc through the remediation one fix at a time, so a rank change
# can be attributed to a specific task rather than to "the rewrite".
#
#   V0  legacy            everything as published
#   V1  + Task 1          bounded keyword dictionaries
#   V2  + Task 2          confidence_mode = "none"
#   V3  + Task 3          m_geo and the bare 2 out of the product
#   V5  + Task 5          scale_mode = "exposure"
#   V6  + Task 6          scope_mode = "firm_first"
#   V9  + Task 9          crosscutting_mode = "report_only"  == new default
#
# Task 4 adds reported columns only and Task 7 moves zero rows on this vintage,
# so neither appears as a rung. Task 8's singleton change touches only the
# pctile column, not the index.
#
# COST CONTROL. The expensive step is allocation, and only Task 1 changes it
# (via combo_weight). Task 3, 5 and 6 are pure multiplicative factors on
# scale_strength_pkg, so their effect is applied by swapping that one column on
# an existing allocation. Task 2 acts after allocation. That is 2 allocations
# and 3 expansions rather than 7 full pipeline runs.
#
# Writes diagnostics/attribution_ladder.csv and attribution_summary.csv
#
# Run from the repo root:
#   Rscript diagnostics/09_attribution_ladder.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

repo_root <- normalizePath(
  if (nzchar(Sys.getenv("OPSI_REPO_ROOT"))) Sys.getenv("OPSI_REPO_ROOT") else getwd(),
  winslash = "/", mustWork = TRUE
)

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))

raw_data_path <- file.path(repo_root, "data", "raw")
out_dir <- file.path(repo_root, "diagnostics")
hr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path, pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - February 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
nipo_raw <- readxl::read_excel(nipo_policy_path, sheet = 1)
subcat_raw <- readr::read_csv(
  file.path(raw_data_path, "hs6_categories_with_essential.csv"), show_col_types = FALSE
) %>% rename("Value.Chain" = "Value Chain")
country_info <- standardize_country_info(
  utils::read.csv(file.path(raw_data_path, "wdi_country_info.csv"))
)

hs6_essential_tbl <- resolve_hs6_essential_tbl(NULL)
subcat_driver <- prepare_subcat_mapping(subcat_raw, hs6_essential_tbl) %>%
  filter(.data$essential_for_tech_sc) %>%
  transmute(HS6 = .data$code, Technology = .data$Technology,
            `Value.Chain` = .data$`Value.Chain`, Sub.Sector = .data$Sub.Sector) %>%
  distinct()

nipo_country <- clean_nipo_raw(nipo_raw, subcat_raw, country_info, hs6_essential_tbl)
cpc_hs <- get_cpc_hs_map(); cpc_names <- get_cpc3_names()
nipo_country <- attach_cpc_validation(
  nipo_country, cpc_hs, build_cpc3_to_tech_sc_pairs(subcat_driver, cpc_hs)
)
tech_universe <- normalize_chr_vec(dplyr::pull(subcat_raw, "Technology"))
sc_universe <- normalize_chr_vec(dplyr::pull(subcat_raw, "Value.Chain"))
tech_sc_cpc_lu <- build_tech_sc_cpc_lookup(subcat_driver, cpc_hs, cpc_names)

# ---- policy_base under each parameter combination the ladder needs -----------
hr("building policy_base variants")

pb <- function(...) build_policy_base(nipo_country, ...)

# Hold the as-of date fixed across the whole ladder so a rung never reflects a
# change in the stock window. Legacy inference is used, matching the baseline.
base_legacy <- pb(include_geo_in_strength = TRUE, strength_constant = 2,
                  scale_mode = "max", scope_mode = "legacy")
asof_legacy <- add_asof_flags(base_legacy, clamp_future_as_of = FALSE)
AS_OF <- asof_legacy$as_of_date[1]
cat("as_of_date held at: ", as.character(AS_OF), "\n", sep = "")

variants_pb <- list(
  v0 = base_legacy,
  # Task 3: geo and constant removed.
  v3 = pb(include_geo_in_strength = FALSE, strength_constant = 1,
          scale_mode = "max", scope_mode = "legacy"),
  # Task 5: exposure-only scale.
  v5 = pb(include_geo_in_strength = FALSE, strength_constant = 1,
          scale_mode = "exposure", scope_mode = "legacy"),
  # Task 6: firm-first scope.
  v6 = pb(include_geo_in_strength = FALSE, strength_constant = 1,
          scale_mode = "exposure", scope_mode = "firm_first")
)

strength_of <- function(p) {
  add_asof_flags(p, as_of_date = AS_OF) %>%
    dplyr::select("policy_id", "scale_strength_pkg")
}
strengths <- lapply(variants_pb, strength_of)

# ---- allocations: only Task 1 changes these ---------------------------------
hr("allocating (the expensive step: 2 allocations, 3 expansions)")

alloc_raw_legacy <- allocate_policy_to_tech_sc(
  asof_legacy, tech_dict = TECH_KEYWORDS_LEGACY, sc_dict = SUPPLY_CHAIN_KEYWORDS_LEGACY
)
cat("legacy-dictionary allocation rows: ", nrow(alloc_raw_legacy), "\n", sep = "")

alloc_raw_new <- allocate_policy_to_tech_sc(
  asof_legacy, tech_dict = TECH_KEYWORDS, sc_dict = SUPPLY_CHAIN_KEYWORDS
)
cat("fixed-dictionary allocation rows : ", nrow(alloc_raw_new), "\n", sep = "")

expand_uniform <- function(a) {
  expand_cross_cutting_rows(a, tech_universe, sc_universe,
                            crosscutting_mode = "uniform") %>%
    filter(.data$tech != "Cross-cutting", .data$supply_chain != "Cross-cutting")
}
alloc_legacy_u <- expand_uniform(alloc_raw_legacy)
alloc_new_u <- expand_uniform(alloc_raw_new)
alloc_new_ro <- expand_cross_cutting_rows(alloc_raw_new, tech_universe, sc_universe,
                                          crosscutting_mode = "report_only")

# ---- assemble each rung ------------------------------------------------------
hr("assembling the ladder")

swap_strength <- function(alloc, s) {
  alloc %>%
    dplyr::select(-"scale_strength_pkg") %>%
    dplyr::left_join(s, by = "policy_id")
}

score_of <- function(alloc, s, conf_mode, label) {
  build_by_tech_sc(
    policy_asof_tbl = asof_legacy, tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe, supply_chain_universe = sc_universe,
    confidence_mode = conf_mode, alloc_long = swap_strength(alloc, s)
  )$data %>%
    dplyr::select("iso3", "country", "tech", "supply_chain", "domestic_stock_sum") %>%
    dplyr::rename("score_{label}" := "domestic_stock_sum")
}

rungs <- list(
  v0 = score_of(alloc_legacy_u, strengths$v0, "legacy", "v0"),
  v1 = score_of(alloc_new_u,    strengths$v0, "legacy", "v1"),
  v2 = score_of(alloc_new_u,    strengths$v0, "none",   "v2"),
  v3 = score_of(alloc_new_u,    strengths$v3, "none",   "v3"),
  v5 = score_of(alloc_new_u,    strengths$v5, "none",   "v5"),
  v6 = score_of(alloc_new_u,    strengths$v6, "none",   "v6"),
  v9 = score_of(alloc_new_ro,   strengths$v6, "none",   "v9")
)
for (nm in names(rungs)) cat(sprintf("  %-3s rows=%d\n", nm, nrow(rungs[[nm]])))

joined <- Reduce(
  function(a, b) dplyr::full_join(a, b, by = c("iso3", "country", "tech", "supply_chain")),
  rungs
)

# ---- rank comparisons --------------------------------------------------------
STEPS <- list(
  c("v0", "v1", "Task 1 keywords"),
  c("v1", "v2", "Task 2 confidence"),
  c("v2", "v3", "Task 3 geo + constant"),
  c("v3", "v5", "Task 5 scale exposure"),
  c("v5", "v6", "Task 6 m_scope firm_first"),
  c("v6", "v9", "Task 9 crosscutting report_only"),
  c("v0", "v9", "CUMULATIVE legacy -> new default")
)

cell_stats <- function(g, from, to) {
  a <- g[[paste0("score_", from)]]
  b <- g[[paste0("score_", to)]]
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 3) {
    return(tibble::tibble(n_countries = sum(ok), rho = NA_real_,
                          n_moved_gt3 = NA_integer_, max_move = NA_integer_))
  }
  ra <- dplyr::dense_rank(dplyr::desc(a[ok]))
  rb <- dplyr::dense_rank(dplyr::desc(b[ok]))
  tibble::tibble(
    n_countries = sum(ok),
    rho = suppressWarnings(stats::cor(a[ok], b[ok], method = "spearman")),
    n_moved_gt3 = sum(abs(ra - rb) > 3),
    max_move = max(abs(ra - rb))
  )
}

ladder <- lapply(STEPS, function(s) {
  joined %>%
    group_by(.data$tech, .data$supply_chain) %>%
    group_modify(~ cell_stats(.x, s[1], s[2])) %>%
    ungroup() %>%
    mutate(step = s[3], from = s[1], to = s[2])
}) %>% bind_rows()

utils::write.csv(ladder, file.path(out_dir, "attribution_ladder.csv"), row.names = FALSE)

summary_tbl <- ladder %>%
  group_by(.data$step) %>%
  summarise(
    n_cells = dplyr::n(),
    median_rho = round(stats::median(.data$rho, na.rm = TRUE), 4),
    min_rho = round(min(.data$rho, na.rm = TRUE), 4),
    country_cells = sum(.data$n_countries, na.rm = TRUE),
    moved_gt3 = sum(.data$n_moved_gt3, na.rm = TRUE),
    share_moved = round(sum(.data$n_moved_gt3, na.rm = TRUE) /
                          sum(.data$n_countries, na.rm = TRUE), 4),
    max_move = max(.data$max_move, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(step = factor(.data$step, levels = vapply(STEPS, `[`, character(1), 3))) %>%
  arrange(.data$step)

hr("ATTRIBUTION: which fix moved the ranking")
print(as.data.frame(summary_tbl), right = FALSE)
utils::write.csv(summary_tbl, file.path(out_dir, "attribution_summary.csv"), row.names = FALSE)

hr("ten weakest cells, cumulative legacy -> new default")
print(as.data.frame(
  ladder %>% filter(.data$step == "CUMULATIVE legacy -> new default") %>%
    arrange(.data$rho) %>% head(10) %>%
    select(tech, supply_chain, n_countries, rho, n_moved_gt3, max_move) %>%
    mutate(rho = round(.data$rho, 4))
), right = FALSE)

# ---- golden-file check against the Task 0 baseline --------------------------
hr("GOLDEN FILE: does dis_legacy_mode reproduce the Task 0 baseline?")
baseline_path <- file.path(out_dir, "baseline", "by_tech_sc.rds")
if (!file.exists(baseline_path)) {
  cat("baseline not found at ", baseline_path, "; skipping.\n", sep = "")
} else {
  baseline <- readRDS(baseline_path)
  cmp <- baseline %>%
    dplyr::select("iso3", "country", "tech", "supply_chain",
                  base_score = "domestic_stock_sum",
                  base_index = "domestic_intervention_index") %>%
    dplyr::inner_join(rungs$v0, by = c("iso3", "country", "tech", "supply_chain"))

  cat("baseline rows: ", nrow(baseline), "   v0 rows: ", nrow(rungs$v0),
      "   matched: ", nrow(cmp), "\n", sep = "")
  d <- abs(cmp$base_score - cmp$score_v0)
  cat("max abs difference in domestic_stock_sum: ", format(max(d, na.rm = TRUE)), "\n", sep = "")
  cat("rows differing by more than 1e-8        : ",
      sum(d > 1e-8, na.rm = TRUE), "\n", sep = "")
  cat("spearman(baseline, v0)                  : ",
      round(suppressWarnings(stats::cor(cmp$base_score, cmp$score_v0,
                                        method = "spearman")), 6), "\n", sep = "")
}

message("\nWrote attribution_ladder.csv and attribution_summary.csv")
