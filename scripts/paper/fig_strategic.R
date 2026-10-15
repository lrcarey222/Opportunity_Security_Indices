# Figures and numbers for the results of paper Section 8 (Policy index and Strategic Index).
#
# Theme key: "strategic". Emits
#   paper/figures/strategic_policy.pdf   what the Policy index rests on (IEA PAMS + CAT, or IEA
#                                        PAMS only) and the rank correlation of the two components
#   paper/figures/strategic_decomp.pdf   SI decomposition, top five sectors of six members
#   paper/figures/strategic_heatmap.pdf  coalition members x sectors SI, allied pick outlined,
#                                        cells barred by the endowment screen marked
#   paper/figures/strategic_sign.pdf     the security sign (need vs strength) and the other
#                                        specification variants
#   paper/figures/strategic_screen.pdf   allied picks by sector with the endowment screen off
#                                        and on, and the transition between them
#   paper/tables/strategic_allied.tex    allied pick, SI, runner-up and margin per member
#   paper/results/strategic.json         every number quoted in the text
#
# Inputs (read only): data/processed/outputs/index_outputs.rds (final rerun of 2026-10-12:
# policy index from IEA PAMS (required) and CAT (added where it rates the country); NIPO is out of
# the composite and enters one robustness variant only, from the October 2026 NIPO component in
# ../scratch_final/nipo_oct/ (OPSI_NIPO_OCT_COMPONENT)), coalition_tbl.rds,
# tech_ghg_tbl.rds, reserves_tbl.rds and critical_minerals_production_tbl.rds (the endowment
# screen), data/processed/charts/allied_strategic.csv (checked against the selection recomputed
# here) and data/raw/wdi_country_info.csv (ISO3 to country names, as scripts/20 does).
#
# The Policy index, the Strategic Index, the endowment flag and the allied selection are
# recomputed from the stored inputs with the pipeline's own pure functions
# (R/indices/build_policy_index.R, R/indices/build_indices.R); the script stops if any of them
# differs from the stored output, and if the screened selection differs from
# allied_strategic.csv. The unscreened selection (require_resource_base = FALSE) is the
# robustness reading of OPSI_SI_RESOURCE_SCREEN=0.
#
# Run from the repository root:
#   Rscript scripts/paper/fig_strategic.R
# The reconciliation numbers (results key reconciliation_with_shipped) are computed only when
# OPSI_SHIPPED_INDEX_OUTPUTS points to the shipped September 2026 index_outputs.rds (mtime
# 2026-09-03); otherwise that key records available = FALSE and everything else is unchanged.

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
  library(cowplot)
})

root <- paper_repo_root()
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "utils", "scurve.R"))
source(file.path(root, "R", "utils", "country.R"))
source(file.path(root, "R", "indices", "build_policy_index.R"))
source(file.path(root, "R", "indices", "build_indices.R"))

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
processed_dir <- file.path(root, config$processed_dir)
raw_dir <- file.path(root, config$raw_data_dir)

# The eleven headline technologies of scripts/20_build_indices.R.
techs <- c("Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen", "Wind", "Oil",
           "Solar", "Gas", "Geothermal", "Electric Grid")
stages <- c("Upstream", "Midstream", "Downstream")

# ---------------------------------------------------------------------------
# Load the final outputs (only the objects needed), then free the rest.
# ---------------------------------------------------------------------------
io <- readRDS(file.path(processed_dir, "outputs", "index_outputs.rds"))
eo_index <- io$economic_opportunity_index
es_index <- io$energy_security_index
policy_component_tbl <- io$policy_component_tbl
policy_index_stored <- io$policy_index
policy_eligibility <- io$policy_eligibility
policy_provenance <- io$policy_index_provenance
si_stored <- io$strategic_index
coalition_stored <- io$coalition_members
es_cells <- io$energy_security_outputs$diagnostics$index_cells
eo_cells <- io$economic_opportunity_outputs$diagnostics$index_cells
trl <- io$economic_opportunity_category_scores %>%
  filter(.data$category == "Technological Readiness") %>%
  select(Country, tech, supply_chain, trl_index = category_score)
# Variable contributions (for the cell-constant part of each pillar) and the Downstream Consumption
# category of the ES pillar (Canada's source gap), kept before the outputs are freed.
vc_keep <- c("Country", "tech", "supply_chain", "category", "variable", "component_value",
             "weighted_variable_contribution")
es_vc <- io$energy_security_outputs$variable_contributions %>% filter(.data$tech %in% techs) %>%
  select(all_of(vc_keep))
eo_vc <- io$economic_opportunity_outputs$variable_contributions %>% filter(.data$tech %in% techs) %>%
  select(all_of(vc_keep))
es_consumption_cc <- io$energy_security_outputs$category_contributions %>%
  filter(.data$tech %in% techs, .data$supply_chain == "Downstream", .data$category == "Consumption") %>%
  select(Country, tech, imputed, missing_rule_applied)
rm(io); invisible(gc())

tech_ghg <- readRDS(file.path(processed_dir, "tech_ghg_tbl.rds"))
reserves_tbl <- readRDS(file.path(processed_dir, "reserves_tbl.rds"))
cm_production_tbl <- readRDS(file.path(processed_dir, "critical_minerals_production_tbl.rds"))
allied_csv <- read.csv(file.path(processed_dir, "charts", "allied_strategic.csv"),
                       stringsAsFactors = FALSE)
country_info <- standardize_country_info(
  read.csv(file.path(raw_dir, "wdi_country_info.csv"), stringsAsFactors = FALSE)
)

# The coalition as scripts/20 reads it: coalition_members(coalition_tbl.rds), names from the WDI
# table by ISO3.
coalition <- coalition_members(readRDS(file.path(processed_dir, "coalition_tbl.rds")))
stopifnot(identical(sort(coalition$iso3), sort(coalition_stored$iso3)))
allies <- coalition %>%
  select(iso3, tier, network_node) %>%
  left_join(country_info %>% distinct(iso3 = .data$iso3c, Country = .data$country), by = "iso3")
stopifnot(!anyNA(allies$Country), !anyDuplicated(allies$iso3))
n_core <- sum(allies$tier == "core"); n_partner <- sum(allies$tier == "partner")

# ---------------------------------------------------------------------------
# Helpers (local to this script)
# ---------------------------------------------------------------------------
spearman <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 3 || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) return(NA_real_)
  stats::cor(a[ok], b[ok], method = "spearman")
}
r2_fe <- function(y, g) summary(stats::lm(y ~ factor(g)))$r.squared
rnd <- function(v, d = 4) round(v, d)
qs <- function(v, d = 4) {
  v <- v[!is.na(v)]
  list(n = length(v), median = rnd(stats::median(v), d), p25 = rnd(unname(stats::quantile(v, 0.25)), d),
       p75 = rnd(unname(stats::quantile(v, 0.75)), d), min = rnd(min(v), d), max = rnd(max(v), d),
       mean = rnd(mean(v), d))
}

# Cov-based variance shares: Var(SI) = sum_k Cov(c_k, SI), so share_k = Cov(c_k, SI)/Var(SI)
# sums to one (a term can have a negative share when it moves against the index).
cov_shares <- function(df, terms, total = "si") {
  v <- stats::var(df[[total]])
  vapply(terms, function(k) stats::cov(df[[k]], df[[total]]) / v, numeric(1))
}

allied_stages <- c("Upstream", "Midstream")
allied_excluded_techs <- c("Geothermal", "Green Hydrogen")

# Allied ranking under any score column (equation eq:si_allied): the member's Upstream and
# Midstream cells outside Geothermal and Green Hydrogen, less (screen = TRUE) the cells barred by
# the endowment screen; top, runner-up and margin per member.
allied_rank <- function(df, score, screen = TRUE) {
  df %>%
    filter(.data$supply_chain %in% allied_stages,
           !.data$tech %in% allied_excluded_techs,
           !is.na(.data[[score]]),
           !(screen & .data$resource_base %in% FALSE)) %>%
    group_by(.data$Country) %>%
    arrange(desc(.data[[score]]), .by_group = TRUE) %>%
    summarise(
      top = .data$sector[1], top_score = .data[[score]][1],
      second = .data$sector[2], second_score = .data[[score]][2],
      top3 = list(.data$sector[1:min(3, n())]),
      n_eligible = n(),
      n_tied_top = sum(abs(.data[[score]] - .data[[score]][1]) < 1e-12),
      .groups = "drop"
    ) %>%
    mutate(margin = .data$top_score - .data$second_score)
}

# ---------------------------------------------------------------------------
# 1. Policy index: eligibility, coverage, correlation of the two components
# ---------------------------------------------------------------------------
policy_build <- build_composite_policy_index(policy_component_tbl, techs = techs)
stopifnot(
  identical(names(POLICY_COMPONENT_WEIGHTS), names(policy_provenance$weights)),
  isTRUE(all.equal(unname(POLICY_COMPONENT_WEIGHTS), unname(policy_provenance$weights))),
  nrow(policy_build$index) == nrow(policy_index_stored),
  isTRUE(all.equal(
    arrange(policy_build$index, Country, tech, supply_chain)$value,
    arrange(policy_index_stored, Country, tech, supply_chain)$value
  ))
)
policy_index <- policy_build$index
eligible <- policy_eligibility$Country[policy_eligibility$eligible]
stopifnot(length(eligible) == policy_provenance$n_countries_eligible)

# Policy basis (user decision of 9 Oct 2026): IEA PAMS is required, CAT is added where it rates
# the country. "IEA PAMS + CAT" countries are scored on both, "IEA PAMS only" on PAMS alone.
basis_full <- "IEA PAMS + CAT"; basis_pams <- "IEA PAMS only"
basis_tbl <- policy_eligibility %>% filter(.data$eligible) %>% select(Country, policy_basis)
stopifnot(all(basis_tbl$policy_basis %in% c(basis_full, basis_pams)),
          identical(sort(basis_tbl$Country[basis_tbl$policy_basis == basis_full]),
                    sort(policy_provenance$countries_by_basis[[basis_full]])))
both_countries <- sort(basis_tbl$Country[basis_tbl$policy_basis == basis_full])
pams_only_countries <- sort(basis_tbl$Country[basis_tbl$policy_basis == basis_pams])

comp_names <- c("IEA PAMS Policy Index" = "PAMS", "CAT Policy Index" = "CAT",
                "NIPO Policy Index" = "NIPO", "Dual-use policy score" = "Dual-use")
comp_all <- policy_component_tbl %>% filter(.data$tech %in% techs)
countries_with_component <- comp_all %>%
  filter(!is.na(.data$value)) %>%
  group_by(.data$variable) %>%
  summarise(n_countries = n_distinct(.data$Country),
            n_coalition = n_distinct(.data$Country[.data$Country %in% allies$Country]),
            .groups = "drop") %>%
  mutate(component = unname(comp_names[.data$variable]))
n_comp <- setNames(countries_with_component$n_countries, countries_with_component$component)
n_comp_coal <- setNames(countries_with_component$n_coalition, countries_with_component$component)
eu_bloc <- policy_provenance$component_build$cat_eu_bloc_members
n_cat_bloc <- length(intersect(eu_bloc, both_countries))
coalition_eligible <- intersect(allies$Country, eligible)
coalition_basis <- allies %>%
  left_join(basis_tbl, by = "Country") %>%
  mutate(policy_basis = dplyr::coalesce(.data$policy_basis, "not eligible"))
coalition_pams_only <- coalition_basis %>% filter(.data$policy_basis == basis_pams) %>%
  arrange(.data$tier, .data$iso3)
coalition_not_eligible <- coalition_basis %>% filter(.data$policy_basis == "not eligible")

# One value per eligible cell and component (the composite's own de-duplication).
comp_wide <- comp_all %>%
  filter(.data$Country %in% eligible) %>%
  group_by(.data$Country, .data$tech, .data$supply_chain, .data$variable) %>%
  summarise(value = if (all(is.na(.data$value))) NA_real_ else mean(.data$value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(component = unname(comp_names[.data$variable])) %>%
  select(Country, tech, supply_chain, component, value) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  left_join(policy_index %>% select(Country, tech, supply_chain, POL = value, n_components, policy_basis),
            by = c("Country", "tech", "supply_chain"))
# IEA PAMS reaches every headline cell of most eligible economies; a few territories have PAMS
# rows for some cells only, and their other cells have no Policy index.
stopifnot(identical(is.na(comp_wide$PAMS), is.na(comp_wide$POL)))
pams_cells_per_country <- comp_wide %>% filter(!is.na(.data$PAMS)) %>% count(.data$Country)
partial_policy <- pams_cells_per_country %>% filter(.data$n < length(techs) * 3) %>% arrange(.data$Country)
comp_wide <- comp_wide %>% filter(!is.na(.data$PAMS))
is_full <- comp_wide$policy_basis == basis_full
stopifnot(nrow(comp_wide) == nrow(policy_index), nrow(pams_cells_per_country) == length(eligible),
          !anyNA(comp_wide$PAMS), !anyNA(comp_wide$POL), !anyNA(comp_wide$policy_basis),
          # CAT is observed in every cell of a rated country and in no cell of the others
          identical(!is.na(comp_wide$CAT), is_full),
          all(comp_wide$n_components == ifelse(is_full, 2L, 1L)),
          max(abs(comp_wide$POL[is_full] - (3 * comp_wide$PAMS[is_full] + 2 * comp_wide$CAT[is_full]) / 5)) < 1e-12,
          max(abs(comp_wide$POL[!is_full] - comp_wide$PAMS[!is_full])) < 1e-12)
comp_both <- comp_wide %>% filter(.data$policy_basis == basis_full)

# CAT rating kappa_i, recovered from the oil cell (g_t = 0 for oil, so x = 1 - kappa).
stopifnot(tech_ghg$ghg_index[tech_ghg$tech == "Oil"] == 0)
cat_kappa <- comp_both %>% filter(.data$tech == "Oil") %>%
  group_by(.data$Country) %>% summarise(kappa = round(1 - mean(.data$CAT), 6), .groups = "drop")
stopifnot(all(cat_kappa$kappa %in% c(0.25, 0.5, 0.75, 1)), nrow(cat_kappa) == length(both_countries))
kappa_labels <- c("0.25" = "critically insufficient", "0.5" = "highly insufficient",
                  "0.75" = "insufficient", "1" = "almost sufficient")
kappa_count <- function(cs) {
  k <- cat_kappa %>% filter(.data$Country %in% cs) %>% count(.data$kappa)
  setNames(as.list(k$n), kappa_labels[as.character(k$kappa)])
}

# Correlations over the countries scored on both components: pooled over their cells, between
# countries (country means), and within country (distribution over those countries; CAT is
# constant within a country rated highly insufficient, which then has no within-country
# correlation with CAT). For the "IEA PAMS only" countries POL equals PAMS by construction.
pairs <- list(c("PAMS", "CAT"), c("POL", "PAMS"), c("POL", "CAT"))
pair_lab <- c("PAMS-CAT" = "PAMS and CAT", "POL-PAMS" = "Policy index and PAMS",
              "POL-CAT" = "Policy index and CAT")
country_means <- comp_both %>% group_by(.data$Country) %>%
  summarise(across(c(PAMS, CAT, POL), mean), .groups = "drop")
corr_tbl <- bind_rows(lapply(pairs, function(p) {
  wc <- comp_both %>% group_by(.data$Country) %>%
    summarise(r = spearman(.data[[p[1]]], .data[[p[2]]]), .groups = "drop")
  tibble(pair = paste(p, collapse = "-"),
         pooled = spearman(comp_both[[p[1]]], comp_both[[p[2]]]),
         between = spearman(country_means[[p[1]]], country_means[[p[2]]]),
         within_median = stats::median(wc$r, na.rm = TRUE),
         within_p25 = unname(stats::quantile(wc$r, 0.25, na.rm = TRUE)),
         within_p75 = unname(stats::quantile(wc$r, 0.75, na.rm = TRUE)),
         within_min = min(wc$r, na.rm = TRUE), within_max = max(wc$r, na.rm = TRUE),
         within_n = sum(!is.na(wc$r)), within_negative = sum(wc$r < 0, na.rm = TRUE),
         within = list(wc))
}))
# Within-country PAMS vs the technology weight g_t: whether a government's recorded policy leans
# to cleaner technologies (the within-country PAMS-CAT correlation is this times sign(2kappa-1)).
pams_g_all <- comp_wide %>% left_join(tech_ghg %>% select(tech, g = ghg_index), by = "tech") %>%
  group_by(.data$Country, .data$policy_basis) %>%
  summarise(r = spearman(.data$PAMS, .data$g), .groups = "drop")
pams_g <- pams_g_all %>% filter(.data$policy_basis == basis_full) %>% left_join(cat_kappa, by = "Country")
stage_component_means <- comp_wide %>% group_by(.data$policy_basis, .data$supply_chain) %>%
  summarise(PAMS = mean(.data$PAMS), CAT = mean(.data$CAT), POL = mean(.data$POL), n = n(),
            .groups = "drop")
# Level of the policy index by basis (the two bases are not one scale: CAT adds its level and its
# technology tilt), over the eligible countries' cells and as country means.
basis_level <- comp_wide %>% group_by(.data$policy_basis, .data$Country) %>%
  summarise(POL = mean(.data$POL), PAMS = mean(.data$PAMS), .groups = "drop") %>%
  group_by(.data$policy_basis) %>%
  summarise(n_countries = n(), mean_POL = mean(.data$POL), median_POL = stats::median(.data$POL),
            mean_PAMS = mean(.data$PAMS), median_PAMS = stats::median(.data$PAMS), .groups = "drop")
# Distinct PAMS values within a country (a country with few recorded policies has few distinct
# values, so its within-country policy ranks have many ties).
pams_distinct <- comp_wide %>% group_by(.data$policy_basis, .data$Country) %>%
  summarise(k = n_distinct(round(.data$PAMS, 9)), .groups = "drop")
# NIPO for the "NIPO added back" variant: the user's October 2026 NIPO run (GTA NIPO - October
# 2026.xlsx, as of 5 October 2026, debiased strength of PR #333), built as scripts/10 builds the
# NIPO Policy Index rows (77 jurisdictions, EU members individually). The February 2026 rows still
# stored in policy_component_tbl cover 23 countries only and are not used. The file lives in the
# workflow's scratch area beside the worktree; OPSI_NIPO_OCT_COMPONENT overrides the location.
nipo_oct_path <- Sys.getenv("OPSI_NIPO_OCT_COMPONENT",
  file.path(dirname(root), "scratch_final", "nipo_oct", "nipo_component_oct2026.rds"))
stopifnot(file.exists(nipo_oct_path))
nipo_stored <- as.data.frame(readRDS(nipo_oct_path)) %>%
  filter(.data$variable == "NIPO Policy Index", !is.na(.data$value), .data$tech %in% techs) %>%
  select(Country, tech, supply_chain, value)
nipo_feb_n_countries <- dplyr::n_distinct(comp_all$Country[comp_all$variable == "NIPO Policy Index" & !is.na(comp_all$value)])
nipo_oct_unmatched <- setdiff(unique(nipo_stored$Country), unique(policy_index$Country))

# ---------------------------------------------------------------------------
# 2. Strategic Index: recompute both passes, the endowment flag, and check the stored output
# ---------------------------------------------------------------------------
si_pass1 <- build_strategic_index(eo_index, es_index, policy_index, tech_ghg, trl, techs = techs)
degenerate <- strategic_index_degenerate_sectors(si_pass1)
si <- if (nrow(degenerate) > 0) {
  build_strategic_index(eo_index, es_index, policy_index, tech_ghg, trl, techs = techs,
                        excluded_sectors = unique(c(STRATEGIC_EXCLUDED_SECTORS, degenerate$sector)))
} else si_pass1
resource_base_tbl <- strategic_resource_base(reserves_tbl, cm_production_tbl)
si <- flag_strategic_resource_base(si, resource_base_tbl)
chk_a <- si %>% arrange(Country, sector)
chk_b <- si_stored %>% arrange(Country, sector)
stopifnot(nrow(chk_a) == nrow(chk_b),
          identical(chk_a$Country, chk_b$Country), identical(chk_a$sector, chk_b$sector),
          identical(is.na(chk_a$strategic_index), is.na(chk_b$strategic_index)),
          max(abs(chk_a$strategic_index - chk_b$strategic_index), na.rm = TRUE) < 1e-12,
          identical(chk_a$si_exclusion, chk_b$si_exclusion),
          identical(chk_a$resource_base, chk_b$resource_base),
          identical(chk_a$pick_exclusion, chk_b$pick_exclusion),
          identical(chk_a$policy_basis, chk_b$policy_basis))
rm(chk_a, chk_b, si_stored); invisible(gc())

cell_status <- bind_rows(es_cells %>% mutate(pillar = "ES"), eo_cells %>% mutate(pillar = "EO")) %>%
  filter(.data$tech %in% techs)

# Exclusion reasons (they combine): counts of the combined strings and of each reason.
reason_pattern <- c(
  not_policy_eligible = "country not policy-eligible",
  missing_policy_index = "missing policy index",
  missing_eo = "missing economic opportunity",
  missing_es = "missing energy security",
  eo_not_rankable = "economic opportunity not rankable",
  es_not_rankable = "energy security not rankable"
)
reason_counts <- function(x) {
  out <- vapply(reason_pattern, function(p) sum(grepl(p, x, fixed = TRUE)), integer(1))
  c(as.list(out), scored = sum(is.na(x)), rows = length(x))
}
# Among the cells of policy-eligible countries, the single reason a cell is not scored, in
# order of precedence (missing pillar before not rankable).
elig_rows <- si %>% filter(.data$Country %in% eligible)
elig_reason <- elig_rows %>%
  mutate(reason = case_when(
    is.na(.data$si_exclusion) ~ "scored",
    grepl("missing energy security", .data$si_exclusion) ~ "ES missing",
    grepl("missing economic opportunity", .data$si_exclusion) ~ "EO missing",
    grepl("energy security not rankable", .data$si_exclusion) &
      grepl("economic opportunity not rankable", .data$si_exclusion) ~ "ES and EO not rankable",
    grepl("energy security not rankable", .data$si_exclusion) ~ "ES not rankable",
    grepl("economic opportunity not rankable", .data$si_exclusion) ~ "EO not rankable",
    TRUE ~ "other"
  )) %>% count(.data$reason)
stopifnot(!"other" %in% elig_reason$reason)

# Scored sample with the five weighted terms (all policy-eligible countries).
w <- STRATEGIC_INDEX_WEIGHTS
sc_all <- strategic_index_contributions(si) %>%
  filter(!is.na(.data$strategic_index)) %>%
  transmute(Country, tech, supply_chain, sector,
            ES = Energy_Security_Index, EO = Economic_Opportunity_Index,
            need = energy_security_need, POL = value, pol, sc_weight, tech_weight,
            policy_basis, resource_base, resource_evidence,
            c_pol = contrib_policy, c_eo = contrib_eo, c_need = contrib_es_need,
            c_stage = contrib_sc, c_tech = contrib_ghg, si = strategic_index)
stopifnot(max(abs(sc_all$c_pol + sc_all$c_eo + sc_all$c_need + sc_all$c_stage + sc_all$c_tech -
                    sc_all$si)) < 1e-12)
terms <- c("c_pol", "c_eo", "c_need", "c_stage", "c_tech")
term_labels <- c(c_pol = "Policy", c_eo = "Economic opportunity", c_need = "Security need (1-ES)",
                 c_stage = "Stage weight", c_tech = "Technology weight")
sc_all <- sc_all %>% left_join(allies %>% select(Country, iso3, tier), by = "Country")
sc <- sc_all %>% filter(!is.na(.data$tier))     # coalition members with an index
coal_si_members <- sort(unique(sc$Country))

si_dist <- function(d) list(
  n_cells = nrow(d), n_countries = n_distinct(d$Country), n_sectors = n_distinct(d$sector),
  mean = rnd(mean(d$si)), sd = rnd(stats::sd(d$si)), min = rnd(min(d$si)), max = rnd(max(d$si)),
  p25 = rnd(unname(stats::quantile(d$si, 0.25))), median = rnd(stats::median(d$si)),
  p75 = rnd(unname(stats::quantile(d$si, 0.75))),
  argmax = paste(d$Country[which.max(d$si)], d$sector[which.max(d$si)], sep = " / "),
  argmin = paste(d$Country[which.min(d$si)], d$sector[which.min(d$si)], sep = " / ")
)
fe_r2 <- function(d) list(
  sector_fe = rnd(r2_fe(d$si, d$sector)), country_fe = rnd(r2_fe(d$si, d$Country)),
  sector_and_country_fe = rnd(summary(stats::lm(si ~ factor(sector) + factor(Country), data = d))$r.squared),
  stage_fe = rnd(r2_fe(d$si, d$supply_chain)), technology_fe = rnd(r2_fe(d$si, d$tech))
)
# Within-country statistics use countries with at least min_cells_within scored cells (Ethiopia
# has one SI cell, so no within-country variance); pooled statistics use every cell.
min_cells_within <- 5
decomp_stats <- function(d) {
  level_share <- vapply(terms, function(k) mean(d[[k]]) / mean(d$si), numeric(1))
  pooled <- cov_shares(d, terms)
  n_by_country <- table(d$Country)
  d_w <- d %>% filter(.data$Country %in% names(n_by_country)[n_by_country >= min_cells_within])
  d_all <- d
  d <- d_w
  by_country <- d %>% group_by(.data$Country) %>%
    group_modify(~ as_tibble(as.list(cov_shares(.x, terms)))) %>% ungroup()
  within_mean <- colMeans(by_country[, terms])
  d_dm <- d %>% group_by(.data$Country) %>%
    mutate(across(all_of(c(terms, "si")), ~ .x - mean(.x))) %>% ungroup()
  within_pooled <- cov_shares(d_dm, terms)
  within_sd <- d %>% group_by(.data$Country) %>%
    summarise(across(all_of(c(terms, "si")), stats::sd), .groups = "drop")
  corr <- lapply(terms, function(k) {
    wc <- d %>% group_by(.data$Country) %>% summarise(r = spearman(.data[[k]], .data$si), .groups = "drop")
    list(pooled_spearman = rnd(spearman(d_all[[k]], d_all$si)),
         within_country_spearman_median = rnd(stats::median(wc$r, na.rm = TRUE)),
         within_country_spearman_min = rnd(min(wc$r, na.rm = TRUE)),
         within_country_spearman_max = rnd(max(wc$r, na.rm = TRUE)))
  })
  names(corr) <- term_labels[terms]
  eo_need <- d %>% group_by(.data$Country) %>%
    summarise(r = spearman(.data$EO, .data$need), .groups = "drop")
  nm <- function(v) as.list(setNames(rnd(unname(v), 6), term_labels[terms]))
  list(
    n_countries_within = n_distinct(d$Country),
    countries_below_min_cells = sort(names(n_by_country)[n_by_country < min_cells_within]),
    min_cells_within = min_cells_within,
    level_share = nm(level_share),
    variance_share_pooled = nm(pooled),
    variance_share_within_country_mean = nm(within_mean),
    variance_share_within_country_min = nm(vapply(terms, function(k) min(by_country[[k]]), 0)),
    variance_share_within_country_max = nm(vapply(terms, function(k) max(by_country[[k]]), 0)),
    variance_share_within_pooled_demeaned = nm(within_pooled),
    variance_share_within_stage_plus_technology = rnd(within_mean[["c_stage"]] + within_mean[["c_tech"]], 6),
    within_country_sd_mean = as.list(setNames(rnd(colMeans(within_sd[, c(terms, "si")])),
                                              c(term_labels[terms], "SI"))),
    corr_si_with_term = corr,
    eo_vs_need_within_country_spearman = c(
      qs(eo_need$r),
      list(n_negative = sum(eo_need$r < 0, na.rm = TRUE),
           min_country = eo_need$Country[which.min(eo_need$r)],
           max_country = eo_need$Country[which.max(eo_need$r)])
    )
  )
}
decomp_all <- decomp_stats(sc_all)
decomp_coal <- decomp_stats(sc)

sector_means <- sc_all %>% group_by(.data$sector, .data$tech, .data$supply_chain) %>%
  summarise(mean_si = mean(.data$si), sd_si = stats::sd(.data$si), n = n(), .groups = "drop") %>%
  arrange(desc(.data$mean_si))
stage_means <- sc_all %>% group_by(.data$supply_chain) %>%
  summarise(mean_si = mean(.data$si), mean_need = mean(.data$need), mean_eo = mean(.data$EO),
            mean_pol = mean(.data$pol), mean_POL = mean(.data$POL), n = n(), .groups = "drop")

# ES-floor diagnostic (strategic_index_es_floor_sectors()), computed on all scored cells.
es_floor <- strategic_index_es_floor_sectors(si)

# Endowment screen statistics.
up_scored <- si %>% filter(!is.na(.data$strategic_index), !is.na(.data$resource_base))
screen_stats <- list(
  screened_technologies = sort(unique(up_scored$tech)),
  n_screened_technologies = n_distinct(up_scored$tech),
  scored_upstream_cells_screened = nrow(up_scored),
  scored_upstream_cells_no_resource_base = sum(!up_scored$resource_base),
  coalition_scored_upstream_cells = sum(up_scored$Country %in% coal_si_members),
  coalition_no_resource_base = sum(!up_scored$resource_base & up_scored$Country %in% coal_si_members),
  coalition_no_resource_base_by_tech = up_scored %>%
    filter(.data$Country %in% coal_si_members) %>% group_by(.data$tech) %>%
    summarise(n = n(), barred = sum(!.data$resource_base), .groups = "drop") %>%
    { setNames(lapply(seq_len(nrow(.)), function(k) list(scored = .$n[k], no_resource_base = .$barred[k])), .$tech) },
  nuclear_upstream_es_floor_share_without_resource = {
    nu <- up_scored %>% filter(.data$sector == "Nuclear - Upstream")
    list(n_scored = nrow(nu), n_no_resource_base = sum(!nu$resource_base),
         es_values_without_resource = unique(rnd(nu$Energy_Security_Index[!nu$resource_base])))
  }
)

# ---------------------------------------------------------------------------
# 3. Allied selection with the endowment screen on (headline) and off (robustness)
# ---------------------------------------------------------------------------
sel_on <- select_allied_strategic_sectors(si, allies, require_resource_base = TRUE)
sel_off <- select_allied_strategic_sectors(si, allies, require_resource_base = FALSE)
stopifnot(!anyDuplicated(sel_on$Country), !anyDuplicated(sel_off$Country),
          setequal(sel_on$Country, sel_off$Country))
# The screened selection must be the pipeline's allied_strategic.csv.
stopifnot(nrow(allied_csv) == nrow(sel_on),
          setequal(allied_csv$Country, sel_on$Country),
          all(allied_csv$sector[match(sel_on$Country, allied_csv$Country)] == sel_on$sector),
          max(abs(allied_csv$strategic_index[match(sel_on$Country, allied_csv$Country)] -
                    sel_on$strategic_index)) < 1e-9,
          all(allied_csv$tier[match(sel_on$Country, allied_csv$Country)] == sel_on$tier))

base_sel <- allied_rank(sc, "si", screen = TRUE)
base_off <- allied_rank(sc, "si", screen = FALSE)
stopifnot(all(base_sel$top == sel_on$sector[match(base_sel$Country, sel_on$Country)]),
          all(base_off$top == sel_off$sector[match(base_off$Country, sel_off$Country)]),
          all(base_sel$n_tied_top == 1), all(base_off$n_tied_top == 1))
base_sel <- base_sel %>% left_join(allies %>% select(Country, iso3, tier), by = "Country")

members_no_pick <- allies %>% filter(!.data$Country %in% sel_on$Country) %>%
  left_join(policy_eligibility %>% select(Country, eligible, missing_required), by = "Country") %>%
  mutate(reason = case_when(
    is.na(.data$eligible) ~ "no policy component at all",
    !.data$eligible ~ paste0("no Policy index, lacks ", .data$missing_required),
    TRUE ~ "no in-sample Upstream/Midstream cell"
  )) %>% arrange(.data$tier, .data$iso3)
# Members whose policy term rests on IEA PAMS alone, and those of them with a pick.
members_pams_only_picked <- intersect(coalition_pams_only$Country, sel_on$Country)

screen_cmp <- base_sel %>% select(Country, iso3, tier, on = top, si_on = top_score) %>%
  left_join(base_off %>% select(Country, off = top, si_off = top_score), by = "Country") %>%
  mutate(changed = .data$on != .data$off, si_forgone = .data$si_off - .data$si_on)
stopifnot(all(screen_cmp$si_forgone >= -1e-12))
# Every changed pick moves away from a cell the screen bars.
off_flag <- screen_cmp %>% filter(.data$changed) %>%
  left_join(sc %>% select(Country, off = sector, rb_off = resource_base), by = c("Country", "off"))
stopifnot(all(off_flag$rb_off %in% FALSE))
pick_tab <- function(v) { t <- sort(table(v), decreasing = TRUE); setNames(as.list(as.integer(t)), names(t)) }
pick_members <- function(df, col) df %>% group_by(.data[[col]]) %>%
  summarise(m = paste(sort(.data$iso3), collapse = " "), .groups = "drop") %>%
  { setNames(as.list(.$m), .[[col]]) }
transition <- screen_cmp %>% count(off, on, name = "n")

# Which terms separate the pick from the runner-up (screen on); pick ranks within A_i.
allied_set <- sc %>% filter(.data$supply_chain %in% allied_stages, !.data$tech %in% allied_excluded_techs,
                            !(.data$resource_base %in% FALSE))
pick_terms <- base_sel %>% select(Country, top, second) %>%
  left_join(sc %>% select(Country, top = sector, all_of(terms)), by = c("Country", "top")) %>%
  left_join(sc %>% select(Country, second = sector, all_of(terms)), by = c("Country", "second"),
            suffix = c("_top", "_second"))
pick_term_gaps <- vapply(terms, function(k) mean(pick_terms[[paste0(k, "_top")]] -
                                                   pick_terms[[paste0(k, "_second")]]), numeric(1))
pick_term_rank <- allied_set %>% group_by(.data$Country) %>%
  mutate(across(all_of(terms), ~ rank(-.x, ties.method = "min"), .names = "rk_{.col}"),
         n_allied = n()) %>% ungroup() %>%
  semi_join(base_sel %>% select(Country, sector = top), by = c("Country", "sector"))
# Members whose pick is Nuclear Midstream: the pick-eligible sectors they have (scored, Upstream or
# Midstream, Geothermal and Green Hydrogen out, endowment screen applied) and the terms that separate
# the pick from the runner-up; plus the spread of ES across all SI countries in each Midstream sector
# (Nuclear Midstream ES is compressed around its median, so its need term varies little).
nuc_mid_pickers <- base_sel %>% filter(.data$top == "Nuclear - Midstream")
nuc_mid_sets <- allied_set %>% filter(.data$Country %in% nuc_mid_pickers$Country) %>%
  group_by(.data$iso3) %>%
  summarise(n_allied = n(), sectors = paste(sort(.data$sector), collapse = "; "), .groups = "drop")
nuc_mid_gaps <- pick_terms %>% filter(.data$top == "Nuclear - Midstream") %>%
  left_join(base_sel %>% select(Country, iso3), by = "Country")
es_spread_mid <- sc_all %>% filter(.data$supply_chain == "Midstream") %>% group_by(.data$sector) %>%
  summarise(n = n(), iqr_ES = stats::IQR(.data$ES), sd_ES = stats::sd(.data$ES),
            median_ES = stats::median(.data$ES), .groups = "drop") %>% arrange(.data$iqr_ES)
pick_rank_summary <- list(
  allied_set_size = list(min = min(pick_term_rank$n_allied), max = max(pick_term_rank$n_allied),
                         median = stats::median(pick_term_rank$n_allied)),
  policy_rank1 = sum(pick_term_rank$rk_c_pol == 1), policy_top3 = sum(pick_term_rank$rk_c_pol <= 3),
  need_rank1 = sum(pick_term_rank$rk_c_need == 1), need_top3 = sum(pick_term_rank$rk_c_need <= 3),
  eo_rank1 = sum(pick_term_rank$rk_c_eo == 1), eo_top3 = sum(pick_term_rank$rk_c_eo <= 3),
  median_policy_rank = stats::median(pick_term_rank$rk_c_pol),
  median_need_rank = stats::median(pick_term_rank$rk_c_need),
  median_eo_rank = stats::median(pick_term_rank$rk_c_eo)
)
# Profile of each picked sector among the members that pick it: pillar levels and the rank of the
# sector on the policy and need terms within the member's eligible set.
pick_profile <- pick_term_rank %>%
  group_by(.data$sector) %>%
  summarise(n = n(), median_ES = median(.data$ES), median_EO = median(.data$EO),
            median_pol = median(.data$pol), median_rank_policy = median(.data$rk_c_pol),
            median_rank_need = median(.data$rk_c_need), median_rank_eo = median(.data$rk_c_eo),
            .groups = "drop") %>% arrange(desc(.data$n))
coal_sector_es_median <- sc %>% group_by(.data$sector) %>% summarise(m = median(.data$ES), .groups = "drop")
# Where each pick sits in its sector's cross-country ES and EO distributions (all SI countries):
# the share of countries with an ES value at or below the pick's (1/n = the sector's lowest
# value) and whether the pick's ES is the sector's minimum or its modal floor value. With the
# need reading a low ES raises the index whether it reflects exposure of an existing industry or
# the absence of one; the endowment screen addresses the second case only in Upstream.
sector_dist <- sc_all %>% group_by(.data$sector) %>%
  mutate(es_le_share = vapply(.data$ES, function(x) mean(.data$ES <= x + 1e-9), 0),
         eo_le_share = vapply(.data$EO, function(x) mean(.data$EO <= x + 1e-9), 0),
         es_is_min = abs(.data$ES - min(.data$ES)) < 1e-9,
         n_sector = n()) %>%
  ungroup() %>%
  select(Country, sector, es_le_share, eo_le_share, es_is_min, n_sector)
pick_position <- base_sel %>% select(Country, iso3, tier, sector = top) %>%
  left_join(sc %>% select(Country, sector, ES, EO, need, pol, supply_chain), by = c("Country", "sector")) %>%
  left_join(sector_dist, by = c("Country", "sector")) %>%
  left_join(es_floor %>% select(sector, es_mode), by = "sector") %>%
  mutate(es_at_floor_mode = !is.na(.data$es_mode) & abs(round(.data$ES, 6) - .data$es_mode) < 1e-6)
pick_position_by_sector <- pick_position %>% group_by(.data$sector) %>%
  summarise(n = n(), median_ES = stats::median(.data$ES), median_es_le_share = stats::median(.data$es_le_share),
            n_es_bottom_decile = sum(.data$es_le_share <= 0.10), n_es_min = sum(.data$es_is_min),
            n_es_floor_mode = sum(.data$es_at_floor_mode),
            median_EO = stats::median(.data$EO), median_eo_le_share = stats::median(.data$eo_le_share),
            members = paste(sort(.data$iso3), collapse = " "), .groups = "drop") %>%
  arrange(desc(.data$n))
mid_picks <- pick_position %>% filter(.data$supply_chain == "Midstream")
# The screen bars an Upstream cell, not a technology: how many changed picks move to the
# Midstream stage of the same technology, and, for the Nuclear - Midstream picks, whether the
# member's Nuclear - Downstream cell (power generation) is scored at all.
screen_moves <- screen_cmp %>% filter(.data$changed) %>%
  mutate(tech_off = sub(" - .*$", "", .data$off), tech_on = sub(" - .*$", "", .data$on),
         stage_on = sub("^.* - ", "", .data$on))
same_tech_midstream <- screen_moves %>% filter(.data$tech_off == .data$tech_on, .data$stage_on == "Midstream")
nuc_mid_pickers <- base_sel %>% filter(.data$top == "Nuclear - Midstream") %>% select(Country, iso3)
nuc_mid_detail <- nuc_mid_pickers %>%
  left_join(si %>% filter(.data$sector == "Nuclear - Downstream") %>%
              transmute(Country, downstream_scored = !is.na(.data$strategic_index),
                        downstream_exclusion = .data$si_exclusion), by = "Country") %>%
  left_join(screen_cmp %>% select(Country, off, si_forgone), by = "Country") %>%
  left_join(sc %>% filter(.data$sector == "Nuclear - Midstream") %>% select(Country, ES, EO, pol, policy_basis),
            by = "Country") %>%
  mutate(downstream_scored = dplyr::coalesce(.data$downstream_scored, FALSE))
# Fixed priors 0.1 lambda_s + 0.1 g_t of the eligible (Upstream, Midstream) sectors.
priors_tbl <- tidyr::expand_grid(tech = setdiff(techs, allied_excluded_techs), supply_chain = allied_stages) %>%
  left_join(tech_ghg %>% select(tech, g = ghg_index), by = "tech") %>%
  mutate(prior = w[["supply_chain"]] * unname(STRATEGIC_SUPPLY_CHAIN_WEIGHTS[.data$supply_chain]) +
           w[["technology"]] * .data$g,
         sector = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
  arrange(desc(.data$prior))
# Within-country link between the policy term and the technology weight: CAT varies within a
# country only through g_t, so pol leans to clean technologies where kappa > 0.5.
pol_tech_wc <- sc_all %>% group_by(.data$Country) %>%
  summarise(r = spearman(.data$pol, .data$tech_weight), n = n(), .groups = "drop") %>%
  filter(.data$n >= min_cells_within) %>%
  left_join(cat_kappa, by = "Country")
pol_tech_by_rating <- pol_tech_wc %>% group_by(.data$kappa) %>%
  summarise(n = n(), median = median(.data$r, na.rm = TRUE), .groups = "drop")
# The ES floor of Nuclear - Upstream and the endowment screen pick out the same countries.
nu_up <- sc_all %>% filter(.data$sector == "Nuclear - Upstream")
nu_floor_es <- es_floor$es_mode[es_floor$sector == "Nuclear - Upstream"]
nu_floor_equals_no_resource <- identical(sort(nu_up$Country[round(nu_up$ES, 6) == round(nu_floor_es, 6)]),
                                         sort(nu_up$Country[nu_up$resource_base %in% FALSE]))

# Pick versus the network design (review of 2026-10-15): the designed production share of the
# member's network node in its pick stage (the EU node for EU members), from scripts/30 at lambda 0.78.
spec_csv <- read.csv(file.path(processed_dir, "outputs", "allied_network_specialization.csv"),
                     stringsAsFactors = FALSE)
design_share <- spec_csv %>%
  transmute(sector = paste(.data$tech, .data$supply_chain, sep = " - "), network_node = .data$network_node,
            design_share = .data$production_share)
stopifnot(!anyDuplicated(design_share[, c("sector", "network_node")]))
pick_design <- base_sel %>% select(Country, iso3, tier, top) %>%
  left_join(allies %>% select(iso3, network_node), by = "iso3") %>%
  left_join(design_share, by = c("top" = "sector", "network_node")) %>%
  mutate(design_share = dplyr::coalesce(.data$design_share, 0))
nodes_no_production <- design_share %>% group_by(.data$network_node) %>%
  summarise(n = sum(.data$design_share > 0), .groups = "drop") %>% filter(.data$n == 0) %>% pull(.data$network_node)
bm_pick <- pick_design %>% filter(.data$top == "Batteries - Midstream")
pick_vs_design <- list(
  source = "data/processed/outputs/allied_network_specialization.csv (lambda = 0.78)",
  batteries_midstream_pickers = nrow(bm_pick),
  batteries_midstream_designed_producers = sum(design_share$design_share[design_share$sector == "Batteries - Midstream"] > 0),
  batteries_midstream_designed_producer_nodes = sort(design_share$network_node[design_share$sector == "Batteries - Midstream" &
                                                                              design_share$design_share > 0]),
  batteries_midstream_pickers_with_designed_role = sum(bm_pick$design_share > 0),
  batteries_midstream_pickers_with_role_via_eu_node = sum(bm_pick$design_share > 0 & bm_pick$network_node == "EUU"),
  batteries_midstream_pickers_without_role = sort(bm_pick$iso3[bm_pick$design_share <= 0]),
  members_with_role_in_pick_stage = sum(pick_design$design_share > 0),
  members_without_role_in_pick_stage = sort(pick_design$iso3[pick_design$design_share <= 0]),
  nodes_without_production = sort(nodes_no_production),
  members_in_nodes_without_production = sort(pick_design$iso3[pick_design$network_node %in% nodes_no_production]),
  design_share_by_member = setNames(as.list(round(pick_design$design_share, 4)), pick_design$iso3)
)
u_res <- read.csv(file.path(root, "data", "reference", "uranium_resources.csv"), stringsAsFactors = FALSE) %>%
  filter(.data$variable == "identified_recoverable_resources_lt_usd260_kgu")
u_prod <- read.csv(file.path(root, "data", "reference", "uranium_production.csv"), stringsAsFactors = FALSE)
u_world <- sum(u_res$value, na.rm = TRUE)
nu_up_pick <- base_sel %>% filter(.data$top == "Nuclear - Upstream")
nu_up_res <- vapply(nu_up_pick$iso3, function(i) sum(u_res$value[u_res$iso3c == i], na.rm = TRUE), 0)
nu_up_prod <- vapply(nu_up_pick$iso3, function(i) sum(u_prod$value[u_prod$iso3c == i], na.rm = TRUE), 0)
nuclear_upstream_pick_resources <- list(
  source = "data/reference/uranium_resources.csv (Red Book 2026, identified recoverable < USD 260/kgU) and uranium_production.csv",
  world_identified_resources_tU = u_world,
  production_years = range(u_prod$year),
  identified_resources_tU = as.list(nu_up_res),
  share_of_world_pct = as.list(round(100 * nu_up_res / u_world, 3)),
  share_of_world_pct_range = round(100 * range(nu_up_res) / u_world, 3),
  recorded_mine_production_tU = as.list(nu_up_prod),
  members_with_production_rows = sort(nu_up_pick$iso3[nu_up_pick$iso3 %in% u_prod$iso3c]),
  members_with_positive_production = sort(nu_up_pick$iso3[nu_up_prod > 0])
)
# Canada's Downstream Consumption: BNEF NEO has no North American aggregate and does not model
# Canada, so the category is imputed at the global average in every Downstream cell it enters.
cons_all_imputed <- es_consumption_cc %>% group_by(.data$Country) %>%
  summarise(n = n(), n_imp = sum(.data$imputed %in% TRUE), .groups = "drop") %>% filter(.data$n_imp == .data$n)
can_es_down <- es_index %>% filter(.data$Country == "Canada", .data$supply_chain == "Downstream")
canada_downstream <- list(
  consumption_rows = sum(es_consumption_cc$Country == "Canada"),
  consumption_rows_imputed = sum(es_consumption_cc$Country == "Canada" & es_consumption_cc$imputed %in% TRUE),
  rule = unique(es_consumption_cc$missing_rule_applied[es_consumption_cc$Country == "Canada"]),
  es_downstream_cells_scored = sum(!is.na(can_es_down$Energy_Security_Index)),
  es_downstream_cells_rankable = sum(can_es_down$rankable %in% TRUE),
  es_downstream_cells_not_rankable = sum(!is.na(can_es_down$Energy_Security_Index) & !(can_es_down$rankable %in% TRUE)),
  es_downstream_unscored = sort(can_es_down$tech[is.na(can_es_down$Energy_Security_Index)]),
  economies_with_all_downstream_consumption_imputed = sort(cons_all_imputed$Country)
)

country_top_overall <- sc %>% group_by(.data$Country) %>% slice_max(.data$si, n = 1, with_ties = TRUE) %>%
  ungroup() %>% select(Country, sector, supply_chain, si)
top_overall_differs <- country_top_overall %>%
  anti_join(base_sel %>% select(Country, sector = top), by = c("Country", "sector"))

# ---------------------------------------------------------------------------
# 4. Specification variants on the coalition sample (the selection keeps the screen)
# ---------------------------------------------------------------------------
# Variants share the baseline cells and, except where the policy term changes, pol (the
# within-country policy ranks); each changes one element of equation eq:si.
sc <- sc %>%
  group_by(.data$Country) %>%
  mutate(eo_rank = median_scurve(.data$EO), need_rank = median_scurve(.data$need)) %>%
  ungroup() %>%
  mutate(
    si_strength = w[["policy"]] * pol + w[["economic_opportunity"]] * EO +
      w[["energy_security_need"]] * ES + w[["supply_chain"]] * sc_weight +
      w[["technology"]] * tech_weight,
    si_nopriors = c_pol + c_eo + c_need,
    si_strength_nopriors = w[["policy"]] * pol + w[["economic_opportunity"]] * EO +
      w[["energy_security_need"]] * ES,
    si_allranked = w[["policy"]] * pol + w[["economic_opportunity"]] * eo_rank +
      w[["energy_security_need"]] * need_rank + c_stage + c_tech,
    si_nopolicy = c_eo + c_need + c_stage + c_tech
  )
# The policy components on the SI cells: the variant "policy from IEA PAMS alone" re-ranks PAMS
# within country (identical to the baseline for the "IEA PAMS only" members, so it is reported
# for the members scored on both), and the October 2026 NIPO component is re-added where observed
# (weights NIPO 4, PAMS 3, CAT 2 as before October 2026, renormalised over the components present
# in the cell), re-ranked within country; identical to the baseline for members without NIPO.
nipo_cells <- nipo_stored %>% group_by(.data$Country, .data$tech, .data$supply_chain) %>%
  summarise(NIPO = mean(.data$value), .groups = "drop")
sc <- sc %>%
  left_join(comp_wide %>% select(Country, tech, supply_chain, PAMS, CAT), by = c("Country", "tech", "supply_chain")) %>%
  left_join(nipo_cells, by = c("Country", "tech", "supply_chain")) %>%
  mutate(POL_nipo = ifelse(is.na(.data$NIPO), .data$POL,
                           (4 * .data$NIPO + 3 * .data$PAMS + 2 * dplyr::coalesce(.data$CAT, 0)) /
                             (4 + 3 + 2 * !is.na(.data$CAT)))) %>%
  group_by(.data$Country) %>%
  mutate(pol_nipo = median_scurve(.data$POL_nipo), pol_pams = median_scurve(.data$PAMS)) %>%
  ungroup() %>%
  mutate(si_nipo = w[["policy"]] * pol_nipo + c_eo + c_need + c_stage + c_tech,
         si_pams = w[["policy"]] * pol_pams + c_eo + c_need + c_stage + c_tech)
stopifnot(!anyNA(sc$si_nipo), !anyNA(sc$PAMS), !anyNA(sc$si_pams),
          max(abs(sc$si_pams - sc$si)[sc$policy_basis == basis_pams]) < 1e-12)
nipo_members <- sort(unique(sc$Country[!is.na(sc$NIPO)]))
coal_both_members <- sort(unique(sc$Country[sc$policy_basis == basis_full]))

# Within-country agreement of the policy ranks on PAMS alone and on the composite, over the SI
# cells of every SI country scored on both components (the coalition and the rest).
pams_vs_composite_si <- sc_all %>% filter(.data$policy_basis == basis_full) %>%
  left_join(comp_wide %>% select(Country, tech, supply_chain, PAMS), by = c("Country", "tech", "supply_chain")) %>%
  group_by(.data$Country) %>%
  summarise(r = spearman(.data$POL, .data$PAMS), n = n(), .groups = "drop")

# Cell-constant inputs (review of 2026-10-15): a variable whose component value is the same for every
# economy in a technology-stage pair (world series, TRL, prices) shifts that pair's pillar level for
# all countries and so acts as an implicit sector prior in each within-country ranking. const_es and
# const_eo are the cells' summed contributions of such variables; si_noconst removes them from the
# pillar levels; si_pairrank enters EO and need as within-pair S-curved ranks over all SI countries.
const_part <- function(vc) {
  vc <- vc %>% filter(!is.na(.data$component_value), !is.na(.data$weighted_variable_contribution))
  cv <- vc %>% group_by(.data$tech, .data$supply_chain, .data$category, .data$variable) %>%
    summarise(rng = max(.data$component_value) - min(.data$component_value), .groups = "drop") %>%
    filter(.data$rng <= 1e-9)
  list(vars = cv,
       cells = vc %>% semi_join(cv, by = c("tech", "supply_chain", "category", "variable")) %>%
         group_by(.data$Country, .data$tech, .data$supply_chain) %>%
         summarise(const = sum(.data$weighted_variable_contribution), .groups = "drop"),
       recon = vc %>% group_by(.data$Country, .data$tech, .data$supply_chain) %>%
         summarise(total = sum(.data$weighted_variable_contribution), .groups = "drop"))
}
cp_es <- const_part(es_vc); cp_eo <- const_part(eo_vc)
rm(es_vc, eo_vc); invisible(gc())
recon_dev <- function(cp, tbl, col) {
  z <- tbl %>% select(Country, tech, supply_chain, v = all_of(col)) %>%
    inner_join(cp$recon, by = c("Country", "tech", "supply_chain"))
  max(abs(z$v - z$total), na.rm = TRUE)
}
const_tbl <- sc_all %>% select(Country, tech, supply_chain, sector) %>%
  left_join(cp_es$cells %>% rename(const_es = const), by = c("Country", "tech", "supply_chain")) %>%
  left_join(cp_eo$cells %>% rename(const_eo = const), by = c("Country", "tech", "supply_chain")) %>%
  mutate(const_es = dplyr::coalesce(.data$const_es, 0), const_eo = dplyr::coalesce(.data$const_eo, 0))
pair_rank_tbl <- sc_all %>% group_by(.data$sector) %>%
  mutate(eo_pr = median_scurve(.data$EO), need_pr = median_scurve(.data$need)) %>% ungroup() %>%
  select(Country, sector, eo_pr, need_pr)
sc <- sc %>%
  left_join(const_tbl %>% select(Country, sector, const_es, const_eo), by = c("Country", "sector")) %>%
  left_join(pair_rank_tbl, by = c("Country", "sector")) %>%
  mutate(si_noconst = .data$si - w[["economic_opportunity"]] * .data$const_eo +
           w[["energy_security_need"]] * .data$const_es,
         si_pairrank = c_pol + w[["economic_opportunity"]] * eo_pr + w[["energy_security_need"]] * need_pr +
           c_stage + c_tech)
stopifnot(!anyNA(sc$si_noconst), !anyNA(sc$si_pairrank))
# Share of each pick's margin over the runner-up due to the cell-constant parts.
const_margin <- base_sel %>% select(Country, iso3, top, second, margin) %>%
  left_join(sc %>% select(Country, top = sector, es_t = const_es, eo_t = const_eo), by = c("Country", "top")) %>%
  left_join(sc %>% select(Country, second = sector, es_s = const_es, eo_s = const_eo), by = c("Country", "second")) %>%
  mutate(const_gap = w[["economic_opportunity"]] * (.data$eo_t - .data$eo_s) -
           w[["energy_security_need"]] * (.data$es_t - .data$es_s),
         const_share = .data$const_gap / .data$margin)
const_term_by_sector <- const_tbl %>%
  mutate(t = -w[["economic_opportunity"]] * .data$const_eo + w[["energy_security_need"]] * .data$const_es) %>%
  group_by(.data$sector) %>% summarise(m = mean(.data$t), .groups = "drop")
cell_constant <- list(
  definition = paste("variables whose component value has zero range across all economies scored in the",
                     "technology-stage pair; their summed weighted contributions are the cell-constant part;",
                     "constant_gap = 0.3 x (EO part of pick - runner-up) - 0.3 x (ES part of pick - runner-up)"),
  es_variable_pairs_constant = nrow(cp_es$vars), eo_variable_pairs_constant = nrow(cp_eo$vars),
  es_constant_variables = sort(unique(cp_es$vars$variable)), eo_constant_variables = sort(unique(cp_eo$vars$variable)),
  es_index_vs_variable_contributions_max_abs_dev = signif(recon_dev(cp_es, es_index, "Energy_Security_Index"), 3),
  eo_index_vs_variable_contributions_max_abs_dev = signif(recon_dev(cp_eo, eo_index, "Economic_Opportunity_Index"), 3),
  si_cells_mean_const_es = rnd(mean(const_tbl$const_es)), si_cells_mean_const_eo = rnd(mean(const_tbl$const_eo)),
  sector_mean_of_constant_si_term = list(min = rnd(min(const_term_by_sector$m)), max = rnd(max(const_term_by_sector$m)),
                                         sd = rnd(stats::sd(const_term_by_sector$m))),
  pick_margin_share = qs(const_margin$const_share, 3),
  picks_where_constant_gap_favours_pick = sum(const_margin$const_gap > 0),
  picks_where_constant_gap_exceeds_margin = sum(const_margin$const_gap > const_margin$margin),
  per_pick = setNames(lapply(seq_len(nrow(const_margin)), function(k) list(
    margin = rnd(const_margin$margin[k]), constant_gap = rnd(const_margin$const_gap[k]),
    share = rnd(const_margin$const_share[k], 3))), const_margin$iso3)
)
rm(cp_es, cp_eo); invisible(gc())

# Endowment screen with a materiality rule: an Upstream extraction stage is eligible only if the
# member has recorded production (reserves alone do not qualify).
sc_prod <- sc %>% mutate(resource_base = ifelse(.data$resource_base %in% TRUE &
                                                  !grepl("Production", dplyr::coalesce(.data$resource_evidence, "")),
                                                FALSE, .data$resource_base))
sel_prod <- allied_rank(sc_prod, "si", screen = TRUE)
prod_cmp <- base_sel %>% select(Country, iso3, top) %>%
  left_join(sel_prod %>% select(Country, top_prod = top), by = "Country")
prod_changed <- !is.na(prod_cmp$top_prod) & prod_cmp$top != prod_cmp$top_prod
screen_production_required <- list(
  rule = "Upstream cells eligible only with recorded production (resource_evidence includes Production)",
  coalition_upstream_cells_barred = sum(sc_prod$resource_base %in% FALSE),
  coalition_upstream_cells_barred_headline = sum(sc$resource_base %in% FALSE),
  members_with_pick = nrow(sel_prod),
  members_without_pick = sort(setdiff(base_sel$iso3, base_sel$iso3[base_sel$Country %in% sel_prod$Country])),
  picks_changed = sum(prod_changed),
  changed = setNames(as.list(prod_cmp$top_prod[prod_changed]), prod_cmp$iso3[prod_changed]),
  pick_distribution = pick_tab(sel_prod$top)
)
rm(sc_prod)

variants <- c(
  si_strength = "Security as strength (ES)",
  si_strength_nopriors = "Strength, no stage or technology weight",
  si_nopriors = "No stage or technology weight",
  si_allranked = "EO and need ranked within country",
  si_nopolicy = "No policy term",
  si_pams = "Policy from IEA PAMS alone",
  si_nipo = "NIPO (October 2026) added back",
  si_pairrank = "EO and need ranked within technology-stage pair",
  si_noconst = "Cell-constant inputs removed from the pillar levels"
)
variant_members <- function(v) switch(v, si_nipo = nipo_members, si_pams = coal_both_members, coal_si_members)
variant_summary <- lapply(names(variants), function(v) {
  members <- variant_members(v)
  d <- sc %>% filter(.data$Country %in% members)
  wc <- d %>% group_by(.data$Country) %>% summarise(r = spearman(.data$si, .data[[v]]), .groups = "drop")
  sel <- allied_rank(d, v, screen = TRUE)
  m <- base_sel %>% filter(.data$Country %in% members) %>% select(Country, iso3, top_base = top) %>%
    left_join(sel %>% select(Country, top_var = top, top3_var = top3), by = "Country")
  b3 <- allied_rank(d, "si", screen = TRUE) %>% select(Country, top3_base = top3)
  m <- m %>% left_join(b3, by = "Country")
  same <- m$top_base == m$top_var
  in3 <- mapply(function(b, t3) b %in% t3, m$top_base, m$top3_var)
  sel_off_v <- allied_rank(d, v, screen = FALSE)
  alt_all <- switch(v,
    si_strength = with(sc_all, c_pol + c_eo + w[["energy_security_need"]] * ES + c_stage + c_tech),
    si_nopriors = with(sc_all, c_pol + c_eo + c_need),
    si_strength_nopriors = with(sc_all, c_pol + c_eo + w[["energy_security_need"]] * ES),
    si_nopolicy = with(sc_all, c_eo + c_need + c_stage + c_tech),
    NULL)
  list(
    label = unname(variants[v]),
    members_rule = switch(v, si_nipo = "coalition members with an October 2026 NIPO value",
                          si_pams = "coalition members whose Policy index is IEA PAMS + CAT",
                          "all coalition members with a Strategic Index"),
    n_members = nrow(m),
    pooled_spearman = rnd(spearman(d$si, d[[v]])),
    # over all SI countries (the variants that need no within-country re-ranking)
    pooled_spearman_all_si_countries = if (is.null(alt_all)) "not computed" else rnd(spearman(sc_all$si, alt_all)),
    within_country_spearman_median = rnd(stats::median(wc$r)),
    within_country_spearman_p25 = rnd(unname(stats::quantile(wc$r, 0.25))),
    within_country_spearman_p75 = rnd(unname(stats::quantile(wc$r, 0.75))),
    within_country_spearman_min = rnd(min(wc$r)),
    within_country_spearman_max = rnd(max(wc$r)),
    min_country = wc$Country[which.min(wc$r)], max_country = wc$Country[which.max(wc$r)],
    n_within_country_negative = sum(wc$r < 0),
    within_country_negative = sort(wc$Country[wc$r < 0]),
    top_pick_same = sum(same),
    top_pick_same_share = rnd(mean(same), 4),
    top_pick_base_in_variant_top3 = sum(in3),
    top_pick_base_in_variant_top3_share = rnd(mean(in3), 4),
    n_midstream_picks = sum(sub("^.* - ", "", m$top_var) == "Midstream"),
    pick_distribution = pick_tab(m$top_var),
    pick_distribution_without_screen = pick_tab(sel_off_v$top),
    changed = setNames(as.list(m$top_var[!same]), m$Country[!same]),
    unchanged = sort(m$iso3[same]),
    per_country_spearman = setNames(as.list(rnd(wc$r)), wc$Country),
    picks = setNames(as.list(m$top_var), m$Country)
  )
})
names(variant_summary) <- names(variants)
strength_wc <- sc %>% group_by(.data$Country, .data$iso3, .data$tier) %>%
  summarise(r = spearman(.data$si, .data$si_strength), n = n(), .groups = "drop")

# Both components required (OPSI_POLICY_REQUIRE_ALL=1, the rule before 9 Oct 2026): the Policy
# index, the SI and the screened selection rebuilt from the same inputs. A country's SI depends
# only on its own cells (pol is a within-country rank; EO and ES enter as levels), so the countries
# scored on both components keep their values and picks unless the degenerate-sector test changes.
policy_req_all <- build_composite_policy_index(policy_component_tbl, techs = techs,
                                               required_components = names(POLICY_COMPONENT_WEIGHTS))
si_ra <- build_strategic_index(eo_index, es_index, policy_req_all$index, tech_ghg, trl, techs = techs)
deg_ra <- strategic_index_degenerate_sectors(si_ra)
if (nrow(deg_ra) > 0) {
  si_ra <- build_strategic_index(eo_index, es_index, policy_req_all$index, tech_ghg, trl, techs = techs,
                                 excluded_sectors = unique(c(STRATEGIC_EXCLUDED_SECTORS, deg_ra$sector)))
}
si_ra <- flag_strategic_resource_base(si_ra, resource_base_tbl)
sel_ra <- select_allied_strategic_sectors(si_ra, allies, require_resource_base = TRUE)
ra_join <- si_ra %>% filter(!is.na(.data$strategic_index)) %>%
  select(Country, sector, si_ra = strategic_index) %>%
  inner_join(sc_all %>% select(Country, sector, si), by = c("Country", "sector"))
require_all <- list(
  eligible_countries = sum(policy_req_all$eligibility$eligible),
  si_cells = sum(!is.na(si_ra$strategic_index)),
  si_countries = n_distinct(si_ra$Country[!is.na(si_ra$strategic_index)]),
  degenerate_sectors = if (nrow(deg_ra) == 0) "none" else deg_ra$sector,
  members_with_pick = nrow(sel_ra),
  members_without_pick = sort(setdiff(allies$iso3, sel_ra$iso3)),
  picks_identical_to_baseline = all(sel_ra$sector == sel_on$sector[match(sel_ra$Country, sel_on$Country)]),
  max_abs_si_difference_common_cells = signif(max(abs(ra_join$si_ra - ra_join$si)), 3),
  common_cells = nrow(ra_join),
  pick_distribution = pick_tab(sel_ra$sector)
)
rm(si_ra, ra_join); invisible(gc())

# ---------------------------------------------------------------------------
# 5. Optional reconciliation with the shipped (September 2026) Strategic Index.
#    Set OPSI_SHIPPED_INDEX_OUTPUTS to the shipped index_outputs.rds to compute it; the file
#    must date from 2026-09-03. Nothing is written outside paper/.
# ---------------------------------------------------------------------------
reconciliation <- list(available = FALSE)
shipped_path <- Sys.getenv("OPSI_SHIPPED_INDEX_OUTPUTS", "")
if (nzchar(shipped_path) && file.exists(shipped_path) &&
    format(file.info(shipped_path)$mtime, "%Y-%m-%d") == "2026-09-03") {
  shipped_si <- readRDS(shipped_path)$strategic_index
  invisible(gc())
  shipped_si <- shipped_si %>%
    mutate(Country = standardize_country_names(.data$Country),
           sector = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
    filter(!is.na(.data$strategic_index)) %>%
    select(Country, tech, supply_chain, sector, si_shipped = strategic_index)
  both <- sc_all %>% select(Country, sector, si) %>%
    inner_join(shipped_si %>% select(Country, sector, si_shipped), by = c("Country", "sector"))
  wc_ship <- both %>% group_by(.data$Country) %>%
    summarise(r = spearman(.data$si, .data$si_shipped), n = n(), .groups = "drop")
  ship_sel <- shipped_si %>% filter(.data$Country %in% coal_si_members) %>%
    mutate(resource_base = NA) %>% allied_rank("si_shipped", screen = FALSE)
  sel_cmp <- ship_sel %>% select(Country, top_shipped = top) %>%
    inner_join(base_sel %>% select(Country, top), by = "Country")
  reconciliation <- list(
    available = TRUE,
    shipped_file_mtime = format(file.info(shipped_path)$mtime, "%Y-%m-%d %H:%M"),
    shipped_scored_cells = nrow(shipped_si),
    shipped_scored_countries = n_distinct(shipped_si$Country),
    cells_in_both = nrow(both), countries_in_both = n_distinct(both$Country),
    pooled_spearman = rnd(spearman(both$si, both$si_shipped)),
    within_country_spearman_median = rnd(stats::median(wc_ship$r, na.rm = TRUE)),
    within_country_spearman_min = rnd(min(wc_ship$r, na.rm = TRUE)),
    within_country_spearman_max = rnd(max(wc_ship$r, na.rm = TRUE)),
    members_scored_in_both = nrow(sel_cmp),
    members_top_pick_changed = sum(sel_cmp$top != sel_cmp$top_shipped),
    shipped_top_picks = setNames(as.list(sel_cmp$top_shipped), sel_cmp$Country),
    shipped_nuclear_upstream_picks = sum(sel_cmp$top_shipped == "Nuclear - Upstream"),
    note = paste("Shipped SI rows mapped to canonical country names; the shipped pick is recomputed",
                 "on the shipped index with the rule of the time (Upstream/Midstream, not Geothermal or",
                 "Green Hydrogen, no endowment screen); the current pick uses the screen.")
  )
  rm(shipped_si, both); invisible(gc())
} else {
  message("strategic: OPSI_SHIPPED_INDEX_OUTPUTS not set to the 2026-09-03 shipped file; ",
          "reconciliation_with_shipped will be written as available = FALSE.")
}

# ---------------------------------------------------------------------------
# 6. Figures
# ---------------------------------------------------------------------------
stage_abbr <- c(Upstream = "U", Midstream = "M", Downstream = "D")
tech_short <- c("Batteries" = "Batteries", "Coal" = "Coal", "Electric Grid" = "Electric grid",
                "Electric Vehicles" = "Electric vehicles", "Gas" = "Gas",
                "Geothermal" = "Geothermal", "Green Hydrogen" = "Green hydrogen",
                "Nuclear" = "Nuclear", "Oil" = "Oil", "Solar" = "Solar", "Wind" = "Wind")
tech_order <- names(tech_short)
sector_label <- function(tech, stage) paste0(tech_short[tech], " (", stage_abbr[stage], ")")
sector_label_s <- function(s) sector_label(sub(" - .*$", "", s), sub("^.* - ", "", s))
panel_tag <- function(p, tag) p + labs(tag = tag) +
  theme(plot.tag = element_text(size = 9, face = "bold"), plot.tag.position = c(0, 1))

# --- 6.1 strategic_policy ---------------------------------------------------
# Panel (a): what each group's Policy index rests on. Three groups: every economy with a Policy
# index, the economies with a Strategic Index, and the coalition members; segments: IEA PAMS +
# CAT with a national CAT rating, IEA PAMS + CAT with the EU-bloc rating, IEA PAMS only.
si_countries <- sort(unique(sc_all$Country))
basis_seg <- function(cs) {
  b <- basis_tbl %>% filter(.data$Country %in% cs)
  c(national = sum(b$policy_basis == basis_full & !b$Country %in% eu_bloc),
    bloc = sum(b$policy_basis == basis_full & b$Country %in% eu_bloc),
    pams = sum(b$policy_basis == basis_pams))
}
grp <- list(
  "Policy index" = eligible,
  "Strategic Index" = si_countries,
  "Coalition members" = intersect(allies$Country, eligible)
)
seg_lab <- c(national = "IEA PAMS + CAT (national CAT rating)",
             bloc = "IEA PAMS + CAT (EU-bloc CAT rating)",
             pams = "IEA PAMS only")
basis_counts <- bind_rows(lapply(names(grp), function(g) {
  k <- basis_seg(grp[[g]])
  tibble(group = g, seg = names(k), n = unname(k))
})) %>%
  group_by(.data$group) %>% mutate(total = sum(.data$n)) %>% ungroup() %>%
  mutate(group_lab = sprintf("%s\n(%d economies)", .data$group, .data$total),
         seg = factor(.data$seg, levels = c("national", "bloc", "pams")))
grp_levels <- rev(unique(basis_counts$group_lab))
basis_counts <- basis_counts %>% mutate(group_lab = factor(.data$group_lab, levels = grp_levels))
seg_cols <- c(national = paper_seq_blue[6], bloc = paper_seq_blue[4], pams = paper_seq_blue[2])
basis_lab_df <- basis_counts %>%
  arrange(.data$group_lab, .data$seg) %>%
  group_by(.data$group_lab) %>%
  mutate(x_end = cumsum(.data$n), x_mid = .data$x_end - .data$n / 2,
         last = dplyr::row_number() == dplyr::n()) %>% ungroup() %>%
  # a label sits inside its segment only where it fits; the last segment's label otherwise
  # moves past the bar end
  mutate(inside = .data$n >= 14,
         x_lab = ifelse(.data$inside, .data$x_mid, .data$x_end + 3),
         hj = ifelse(.data$inside, 0.5, 0),
         col_key = ifelse(.data$inside, as.character(.data$seg), "outside")) %>%
  filter(.data$inside | .data$last)
p_pol_a <- ggplot(basis_counts, aes(x = n, y = group_lab, fill = seg)) +
  geom_col(width = 0.62, position = position_stack(reverse = TRUE), colour = "white", linewidth = 0.4) +
  geom_text(data = basis_lab_df, inherit.aes = FALSE,
            mapping = aes(x = x_lab, y = group_lab, label = n, colour = col_key, hjust = hj),
            size = 2.5, show.legend = FALSE) +
  scale_colour_manual(values = c(national = "white", bloc = "white", pams = paper_ink$primary,
                                 outside = paper_ink$secondary), guide = "none") +
  scale_fill_manual(values = seg_cols, labels = seg_lab, name = NULL) +
  scale_x_continuous(limits = c(0, 222), breaks = seq(0, 200, 50), expand = c(0, 0)) +
  labs(x = "Economies", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 7.5, lineheight = 0.95), legend.text = element_text(size = 7),
        legend.margin = margin(0, 0, 0, 0), legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_legend(ncol = 1))

# Panel (b): rank correlations among the economies scored on both components.
corr_long <- corr_tbl %>%
  transmute(pair, pooled, between, within_median, within_p25, within_p75, within_min, within_max) %>%
  mutate(pair = factor(unname(pair_lab[pair]), levels = rev(unname(pair_lab))))
corr_pts <- corr_long %>%
  select(pair, pooled, between, within_median) %>%
  pivot_longer(-pair, names_to = "measure", values_to = "r") %>%
  mutate(measure = factor(measure, levels = c("within_median", "pooled", "between"),
                          labels = c("Within country (median, IQR, range)", "Pooled over cells",
                                     "Between countries (country means)")))
p_pol_b <- ggplot(corr_long, aes(y = pair)) +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_segment(aes(x = within_min, xend = within_max, yend = pair), colour = paper_seq_blue[2],
               linewidth = 0.4) +
  geom_segment(aes(x = within_p25, xend = within_p75, yend = pair), colour = paper_seq_blue[3],
               linewidth = 2.2, lineend = "butt") +
  geom_point(data = corr_pts, aes(x = r, shape = measure, fill = measure), size = 2.1,
             stroke = 0.5, colour = paper_ink$primary) +
  scale_shape_manual(values = c(21, 23, 24), name = NULL) +
  scale_fill_manual(values = c(paper_cat[["blue"]], "white", paper_cat[["orange"]]), name = NULL) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  labs(x = "Spearman rank correlation", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 7.5), legend.text = element_text(size = 7),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(shape = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))
fig_policy <- plot_grid(panel_tag(p_pol_a, "(a)"), panel_tag(p_pol_b, "(b)"), nrow = 1,
                        rel_widths = c(1, 1), align = "h", axis = "tb")
save_paper_fig(fig_policy, "strategic_policy", width = paper_width_full, height = 2.8)

# --- 6.2 strategic_decomp ---------------------------------------------------
term_cols <- c(
  "Policy" = unname(paper_pillar_cols["Policy"]),
  "Economic opportunity" = unname(paper_pillar_cols["Economic Opportunity"]),
  "Security need (1-ES)" = unname(paper_pillar_cols["Energy Security"]),
  "Stage weight" = unname(paper_pillar_cols["Supply-chain weight"]),
  "Technology weight" = unname(paper_pillar_cols["Technology weight"])
)
focal <- c("United States", "Japan", "Germany", "South Korea", "India", "Australia")
stopifnot(all(focal %in% sc$Country))
focal_top5 <- sc %>% filter(.data$Country %in% focal) %>% group_by(.data$Country) %>%
  slice_max(.data$si, n = 5, with_ties = FALSE) %>% ungroup() %>%
  left_join(base_sel %>% select(Country, pick = top), by = "Country") %>%
  mutate(is_pick = .data$sector == .data$pick, barred = .data$resource_base %in% FALSE)
focal_term_ranks <- sc %>% filter(.data$Country %in% focal) %>% group_by(.data$Country) %>%
  mutate(across(all_of(terms), ~ rank(-.x, ties.method = "min"), .names = "rk_{.col}"),
         n_cells = n()) %>% ungroup() %>%
  semi_join(focal_top5, by = c("Country", "sector"))
decomp_df <- focal_top5 %>%
  mutate(label = paste0(sector_label(tech, supply_chain),
                        ifelse(is_pick, " *", ""), ifelse(barred, " \u00b0", "")),
         key = paste(Country, label, sep = "__")) %>%
  group_by(Country) %>% arrange(si, .by_group = TRUE) %>%
  mutate(key = factor(key, levels = key)) %>% ungroup() %>%
  select(Country, key, label, si, all_of(terms)) %>%
  pivot_longer(all_of(terms), names_to = "term", values_to = "contribution") %>%
  mutate(term = factor(unname(term_labels[term]), levels = names(term_cols)),
         Country = factor(Country, levels = focal))
p_decomp <- ggplot(decomp_df, aes(x = contribution, y = key, fill = term)) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.35,
           position = position_stack(reverse = TRUE)) +
  facet_wrap(~ Country, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = function(k) sub("^.*__", "", k)) +
  scale_fill_manual(values = term_cols, name = NULL) +
  scale_x_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1), expand = c(0, 0)) +
  labs(x = "Strategic Index and its weighted terms", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(0.5, "cm"), panel.spacing.y = unit(0.25, "cm"),
        axis.text.y = element_text(size = 7.5), legend.text = element_text(size = 7.5)) +
  guides(fill = guide_legend(nrow = 1))
save_paper_fig(p_decomp, "strategic_decomp", width = paper_width_full, height = 5.0)

# --- 6.3 strategic_heatmap --------------------------------------------------
tier_lab <- c(core = sprintf("Core members"), partner = sprintf("Partners"))
heat_base <- si %>%
  filter(.data$Country %in% coal_si_members) %>%
  select(Country, tech, supply_chain, sector, strategic_index, si_exclusion, resource_base) %>%
  left_join(allies %>% select(Country, tier), by = "Country") %>%
  complete(nesting(Country, tier), tech = tech_order, supply_chain = stages) %>%
  filter(!(tech == "Geothermal" & supply_chain == "Midstream"))
stopifnot(nrow(heat_base) == length(coal_si_members) * 32)
heat_df <- heat_base %>%
  mutate(Country = factor(Country, levels = rev(sort(unique(Country)))),
         tier = factor(tier, levels = c("core", "partner"), labels = unname(tier_lab)),
         tech = factor(tech, levels = tech_order, labels = unname(tech_short)),
         supply_chain = factor(supply_chain, levels = stages))
pick_df <- heat_df %>% semi_join(base_sel %>% select(Country, sector = top), by = c("Country", "sector"))
barred_df <- heat_df %>% filter(.data$resource_base %in% FALSE, !is.na(.data$strategic_index))
si_range_coal <- range(sc$si)
stopifnot(si_range_coal[1] >= 0.25, si_range_coal[2] <= 0.80)  # inside the colour scale
p_heat <- ggplot(heat_df, aes(x = tech, y = Country)) +
  geom_tile(aes(fill = strategic_index), colour = "white", linewidth = 0.3) +
  geom_point(data = barred_df, shape = 21, size = 0.75, stroke = 0.3, fill = "white",
             colour = paper_ink$primary) +
  geom_tile(data = pick_df, fill = NA, colour = paper_ink$primary, linewidth = 0.7,
            width = 0.9, height = 0.86) +
  facet_grid(tier ~ supply_chain, scales = "free", space = "free") +
  scale_fill_gradientn(colours = paper_seq_blue, limits = c(0.25, 0.80),
                       breaks = seq(0.3, 0.8, 0.1), na.value = paper_ink$neutral,
                       name = "Strategic Index") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        axis.text.y = element_text(size = 6.2),
        strip.text.x = element_text(hjust = 0.5),
        strip.text.y = element_text(angle = 270, hjust = 0.5),
        panel.spacing.x = unit(0.15, "cm"), panel.spacing.y = unit(0.2, "cm"),
        legend.key.width = unit(1.4, "cm"), legend.key.height = unit(0.25, "cm")) +
  guides(fill = guide_colourbar(title.position = "left", title.vjust = 0.9))
# A key for the two overlays and the grey cells (drawn, so the glyphs match the plot).
key_df <- tibble(x = c(1, 4.6, 8.9), lab = c("Allied pick (with the endowment screen)",
                                             "No recorded resource base: barred from the pick",
                                             "Not scored"))
p_key <- ggplot(key_df) +
  geom_tile(aes(x = x, y = 1), width = 0.32, height = 0.7,
            fill = c(paper_seq_blue[5], paper_seq_blue[5], paper_ink$neutral), colour = "white") +
  geom_tile(data = key_df[1, ], aes(x = x, y = 1), width = 0.29, height = 0.62, fill = NA,
            colour = paper_ink$primary, linewidth = 0.7) +
  geom_point(data = key_df[2, ], aes(x = x, y = 1), shape = 21, size = 0.75, stroke = 0.3,
             fill = "white", colour = paper_ink$primary) +
  geom_text(aes(x = x + 0.25, y = 1, label = lab), hjust = 0, size = 2.5, colour = paper_ink$primary) +
  scale_x_continuous(limits = c(0.8, 11.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.5, 1.5)) +
  coord_cartesian(clip = "off") +
  theme_void()
fig_heat <- plot_grid(p_heat, p_key, ncol = 1, rel_heights = c(1, 0.035))
save_paper_fig(fig_heat, "strategic_heatmap", width = paper_width_full, height = 8.0)

# --- 6.4 strategic_sign -----------------------------------------------------
med_str <- variant_summary$si_strength$within_country_spearman_median
str_df <- strength_wc %>%
  mutate(tier = factor(tier, levels = c("partner", "core"), labels = rev(unname(tier_lab))))
set.seed(20261009)
# Label the lowest and the highest member of each tier.
ext <- bind_rows(strength_wc %>% group_by(tier) %>% slice_min(r, n = 1, with_ties = FALSE) %>% ungroup(),
                 strength_wc %>% group_by(tier) %>% slice_max(r, n = 1, with_ties = FALSE) %>% ungroup()) %>%
  mutate(tier = factor(tier, levels = c("partner", "core"), labels = rev(unname(tier_lab))))
p_sign_a <- ggplot(str_df, aes(x = r, y = tier)) +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_vline(xintercept = med_str, colour = paper_cat[["blue"]], linewidth = 0.3, alpha = 0.6) +
  geom_point(position = position_jitter(width = 0, height = 0.18, seed = 7), size = 1.6,
             shape = 21, stroke = 0.35, colour = "white", fill = paper_cat[["blue"]]) +
  geom_text(data = ext, aes(label = iso3), nudge_y = 0.32, size = 2.2, colour = paper_ink$secondary) +
  annotate("text", x = med_str + 0.03, y = 2.47, label = sprintf("median %.2f", med_str),
           hjust = 0, size = 2.3, colour = paper_ink$secondary) +
  scale_x_continuous(limits = c(-0.75, 1), breaks = seq(-0.5, 1, 0.5)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.65))) +
  labs(x = "Spearman, SI(need) vs SI(strength),\nwithin member", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 7.5))

var_order <- c("si_strength", "si_strength_nopriors", "si_nopriors", "si_allranked", "si_nopolicy", "si_pams",
               "si_nipo")
var_lab <- c(si_strength = "Security as strength (ES)",
             si_strength_nopriors = "Strength, no stage or tech. weight",
             si_nopriors = "Need, no stage or tech. weight",
             si_allranked = "EO and need ranked within country",
             si_nopolicy = "No policy term",
             si_pams = sprintf("Policy from IEA PAMS alone (%d)", variant_summary$si_pams$n_members),
             si_nipo = sprintf("NIPO (Oct. 2026) added back (%d)", variant_summary$si_nipo$n_members))
var_df <- tibble(
  variant = factor(var_lab[var_order], levels = rev(var_lab[var_order])),
  med = vapply(var_order, function(v) variant_summary[[v]]$within_country_spearman_median, 0),
  p25 = vapply(var_order, function(v) variant_summary[[v]]$within_country_spearman_p25, 0),
  p75 = vapply(var_order, function(v) variant_summary[[v]]$within_country_spearman_p75, 0),
  lo = vapply(var_order, function(v) variant_summary[[v]]$within_country_spearman_min, 0),
  hi = vapply(var_order, function(v) variant_summary[[v]]$within_country_spearman_max, 0),
  same = vapply(var_order, function(v) 100 * variant_summary[[v]]$top_pick_same / variant_summary[[v]]$n_members, 0),
  in3 = vapply(var_order, function(v) 100 * variant_summary[[v]]$top_pick_base_in_variant_top3 / variant_summary[[v]]$n_members, 0)
)
n_sel <- nrow(base_sel)
p_sign_b <- ggplot(var_df, aes(y = variant)) +
  geom_vline(xintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_segment(aes(x = lo, xend = hi, yend = variant), colour = paper_seq_blue[2], linewidth = 0.4) +
  geom_segment(aes(x = p25, xend = p75, yend = variant), colour = paper_seq_blue[3], linewidth = 2.2) +
  geom_point(aes(x = med), size = 2, shape = 21, stroke = 0.4, colour = "white",
             fill = paper_cat[["blue"]]) +
  scale_x_continuous(limits = c(-0.75, 1), breaks = seq(-0.5, 1, 0.5)) +
  labs(x = "Spearman with the baseline, within member\n(median, IQR, range)", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 7.5))
agree_long <- var_df %>%
  select(variant, same, in3) %>%
  pivot_longer(c(same, in3), names_to = "measure", values_to = "n") %>%
  mutate(measure = factor(measure, levels = c("same", "in3"),
                          labels = c("Same pick", "Baseline pick in variant's top three")))
p_sign_c <- ggplot(agree_long, aes(x = n, y = variant)) +
  geom_line(aes(group = variant), colour = paper_ink$axis, linewidth = 0.3) +
  geom_point(aes(shape = measure), size = 2, stroke = 0.6, colour = paper_cat[["blue"]], fill = "white") +
  scale_shape_manual(values = c(16, 21), name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0.03, 0)) +
  labs(x = "Members whose pick agrees (%)", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_blank(), legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 7)) +
  guides(shape = guide_legend(nrow = 2))
fig_sign <- plot_grid(
  panel_tag(p_sign_a, "(a)"),
  plot_grid(panel_tag(p_sign_b, "(b)"), panel_tag(p_sign_c, "(c)"), nrow = 1,
            rel_widths = c(1.55, 0.8), align = "h", axis = "tb"),
  ncol = 1, rel_heights = c(0.62, 1)
)
save_paper_fig(fig_sign, "strategic_sign", width = paper_width_full, height = 4.9)

# --- 6.5 strategic_screen ---------------------------------------------------
pick_counts <- bind_rows(
  screen_cmp %>% count(sector = off) %>% mutate(screen = "Screen off"),
  screen_cmp %>% count(sector = on) %>% mutate(screen = "Screen on (headline)")
) %>%
  complete(sector, screen, fill = list(n = 0L)) %>%
  mutate(stage = sub("^.* - ", "", sector), label = sector_label_s(sector))
ord <- pick_counts %>% group_by(label) %>% summarise(k = max(n), k_on = n[screen == "Screen on (headline)"],
                                                      .groups = "drop") %>%
  arrange(k_on, k) %>% pull(label)
pick_counts <- pick_counts %>% mutate(label = factor(label, levels = ord),
                                      screen = factor(screen, levels = c("Screen off", "Screen on (headline)")))
seg_df <- pick_counts %>% select(label, stage, screen, n) %>%
  pivot_wider(names_from = screen, values_from = n)
names(seg_df) <- c("label", "stage", "off", "on")
p_scr_a <- ggplot(pick_counts, aes(x = n, y = label)) +
  geom_segment(data = seg_df, aes(x = off, xend = on, y = label, yend = label),
               colour = paper_ink$axis, linewidth = 0.5) +
  geom_point(aes(shape = screen, colour = stage), fill = "white", size = 2.2, stroke = 0.8) +
  scale_shape_manual(values = c("Screen off" = 21, "Screen on (headline)" = 16), name = NULL) +
  scale_colour_manual(values = paper_stage_cols[c("Upstream", "Midstream")], name = NULL) +
  scale_x_continuous(limits = c(0, 5 * ceiling(max(pick_counts$n) / 5)), breaks = seq(0, 60, 5),
                     expand = c(0.02, 0)) +
  labs(x = sprintf("Members picking the sector (of %d)", n_sel), y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(size = 7.5), legend.text = element_text(size = 7),
        legend.margin = margin(0, 0, 0, 0), legend.box = "vertical", legend.spacing.y = unit(0, "cm")) +
  guides(shape = guide_legend(order = 1, nrow = 1), colour = guide_legend(order = 2, nrow = 1))

lev_off <- rev(levels(pick_counts$label))
tr_df <- transition %>%
  mutate(off = factor(sector_label_s(off), levels = intersect(lev_off, sector_label_s(unique(off)))),
         on = factor(sector_label_s(on), levels = intersect(lev_off, sector_label_s(unique(on)))),
         diag = as.character(off) == as.character(on))
tr_dark <- 0.45 * max(tr_df$n)
p_scr_b <- ggplot(tr_df, aes(x = on, y = off)) +
  geom_tile(aes(fill = n), colour = "white", linewidth = 0.6) +
  geom_tile(data = filter(tr_df, diag), fill = NA, colour = paper_ink$primary, linewidth = 0.4,
            width = 0.94, height = 0.92) +
  geom_text(aes(label = n, colour = n > tr_dark), size = 2.5) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = paper_ink$primary), guide = "none") +
  scale_fill_gradientn(colours = paper_seq_blue[1:6], limits = c(0, max(tr_df$n)), guide = "none") +
  scale_y_discrete(limits = rev) +
  labs(x = "Pick with the screen (headline)", y = "Pick without the screen") +
  theme_paper(grid = "none") +
  theme(axis.line.x = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))
fig_screen <- plot_grid(panel_tag(p_scr_a, "(a)"), panel_tag(p_scr_b, "(b)"), nrow = 1,
                        rel_widths = c(1, 1))
save_paper_fig(fig_screen, "strategic_screen", width = paper_width_full, height = 3.3)

# ---------------------------------------------------------------------------
# 7. Table: allied selection
# ---------------------------------------------------------------------------
mark <- function(b) ifelse(b, "$\\checkmark$", "--")
same_under <- function(v) {
  p <- unlist(variant_summary[[v]]$picks)
  unname(p[base_sel$Country] == base_sel$top)
}
tab <- base_sel %>%
  mutate(s_str = same_under("si_strength"), s_np = same_under("si_nopriors")) %>%
  left_join(pick_design %>% select(Country, design_share, network_node), by = "Country") %>%
  left_join(screen_cmp %>% select(Country, off, changed), by = "Country") %>%
  mutate(tier = factor(tier, levels = c("core", "partner"))) %>%
  arrange(tier, Country)
tex_escape <- function(x) gsub("&", "\\\\&", x)
row_tex <- function(d) sprintf(
  "%s & %s & %.3f & %s & %.4f & %s & %s & %s & %s \\\\",
  paste0(tex_escape(d$Country), ifelse(d$Country %in% coalition_pams_only$Country, "$^{\\dagger}$", "")),
  sector_label_s(d$top), d$top_score, sector_label_s(d$second), d$margin,
  ifelse(d$design_share > 0, sprintf("%.3f", d$design_share), "--"),
  ifelse(d$changed, sector_label_s(d$off), "same"), mark(d$s_str), mark(d$s_np)
)
tab_core <- tab %>% filter(tier == "core"); tab_partner <- tab %>% filter(tier == "partner")
no_pick_txt <- paste0(members_no_pick$Country, collapse = ", ")
# A longtable (not a float): the 57 member rows and the notes exceed one page.
tab_head <- c(
  "\\toprule",
  " & & & & & & & \\multicolumn{2}{c@{}}{\\makecell{Same pick\\\\under}}\\\\",
  "\\cmidrule(l){8-9}",
  "Member & Pick & {$\\SI$} & Runner-up & {Margin} & \\makecell{Design\\\\share} & \\makecell[l]{Pick without\\\\the screen} & (1) & (2)\\\\",
  "\\midrule"
)
tab_tex <- c(
  "% Generated by scripts/paper/fig_strategic.R -- do not edit by hand.",
  "\\begingroup",
  "\\singlespacing\\scriptsize",
  "\\setlength{\\tabcolsep}{2.9pt}",
  "\\setlength{\\LTcapwidth}{\\textwidth}",
  "\\begin{longtable}{@{}l l S[table-format=1.3] l S[table-format=1.4] c l c c@{}}",
  "\\caption{Allied strategic selection: pick, runner-up, margin and robustness of the pick}",
  "\\label{tab:strategic_allied}\\\\",
  tab_head,
  "\\endfirsthead",
  "\\multicolumn{9}{@{}l}{\\small\\textbf{Table \\thetable:} continued}\\\\",
  tab_head,
  "\\endhead",
  "\\midrule",
  "\\multicolumn{9}{r@{}}{\\textit{Continued on the next page}}\\\\",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot",
  sprintf("\\multicolumn{9}{@{}l}{\\textit{Core members (%d of %d with a pick)}}\\\\", nrow(tab_core), n_core),
  row_tex(tab_core),
  "\\midrule",
  sprintf("\\multicolumn{9}{@{}l}{\\textit{Partners (%d of %d with a pick)}}\\\\", nrow(tab_partner), n_partner),
  row_tex(tab_partner),
  "\\midrule",
  sprintf("\\multicolumn{6}{@{}l}{Members with the same pick (of %d)} & %d & %d & %d\\\\",
          nrow(tab), sum(!tab$changed), sum(tab$s_str), sum(tab$s_np)),
  "\\end{longtable}",
  "\\vspace{-6pt}\\noindent",
  paste0(
    "\\textit{Notes:} For each coalition member with a Strategic Index, the pick is the ",
    "Upstream or Midstream sector outside Geothermal and Green Hydrogen with the highest index ",
    "(equation~\\eqref{eq:si_allied}), after the endowment screen bars Upstream sectors in which ",
    "the member has no recorded reserves or production; the runner-up is the second-highest such ",
    "sector and the margin the difference between their indices; (U) Upstream, (M) Midstream. ",
    if (all(base_sel$n_tied_top == 1)) "No member has a tie. " else "",
    "Design share is the share of the stage's production that the network design ",
    "(Section~\\ref{sec:network}, $\\lambda=0.78$) assigns to the member's node, for EU members the EU node ",
    "(-- if none). ",
    "``Pick without the screen'' is the pick when the endowment screen is ",
    "switched off (``same'' if unchanged); the index values are the same in both cases. ",
    "Columns (1) and (2) mark whether the pick, with the screen, is unchanged when the index is ",
    "recomputed on the same cells (1) with security strength $\\ES$ in place of need $1-\\ES$ and ",
    "(2) without the stage and technology weights. ",
    if (nrow(members_no_pick) == 0) sprintf("All %d members have a pick. ", nrow(allies)) else
      sprintf("The %d members without a pick: %s. ", nrow(members_no_pick), no_pick_txt),
    sprintf(paste0("$^{\\dagger}$\\,Policy index on IEA PAMS alone (no CAT rating; %d members); ",
                   "the other members' Policy index combines IEA PAMS and CAT. "), nrow(coalition_pams_only)),
    "Source: \\texttt{index\\_outputs.rds} and \\texttt{allied\\_strategic.csv} of the final run, ",
    "which the script reproduces; \\texttt{scripts/paper/fig\\_strategic.R}.\\par"
  ),
  "\\endgroup",
  "\\medskip"
)
tables_dir <- paper_dir("tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
writeLines(tab_tex, file.path(tables_dir, "strategic_allied.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------
# 8. Numbers for the text
# ---------------------------------------------------------------------------
corr_out <- lapply(seq_len(nrow(corr_tbl)), function(k) {
  r <- corr_tbl[k, ]
  list(pooled = rnd(r$pooled), between_countries = rnd(r$between),
       within_median = rnd(r$within_median), within_p25 = rnd(r$within_p25),
       within_p75 = rnd(r$within_p75), within_min = rnd(r$within_min), within_max = rnd(r$within_max),
       within_n_countries = r$within_n, within_negative = r$within_negative)
})
names(corr_out) <- corr_tbl$pair
results <- list(
  meta = list(
    script = "scripts/paper/fig_strategic.R",
    input = "data/processed/outputs/index_outputs.rds (final rerun, 2026-10-12)",
    policy_components = "IEA PAMS (3) + CAT (2); NIPO and dual-use excluded from the composite",
    correlation_method = "Spearman (rank) throughout",
    rankability = "SI cells require rankable EO and ES cells (rankable == TRUE); all rankings here are over SI cells",
    policy_index_recomputed_equals_stored = TRUE,
    si_recomputed_equals_stored = TRUE,
    resource_flag_recomputed_equals_stored = TRUE,
    allied_pick_screen_on_equals_allied_strategic_csv = TRUE,
    reconciliation_requires = paste(
      "Set OPSI_SHIPPED_INDEX_OUTPUTS to the shipped September 2026 index_outputs.rds (mtime",
      "2026-09-03); without it reconciliation_with_shipped is written as available = FALSE.")
  ),
  policy = list(
    rule = paste("IEA PAMS (weight 3) required; CAT (weight 2) added where it rates the country;",
                 "weights renormalised over the components present; NIPO and dual-use excluded"),
    economies_with_any_component = nrow(policy_eligibility),
    economies_with_component = list(IEA_PAMS = n_comp[["PAMS"]], CAT = n_comp[["CAT"]],
                                    CAT_national = n_comp[["CAT"]] - n_cat_bloc, CAT_eu_bloc = n_cat_bloc,
                                    NIPO_stored_not_in_composite = n_comp[["NIPO"]],
                                    dual_use_not_in_composite = n_comp[["Dual-use"]]),
    economies_with_any_of_four_components = n_distinct(comp_all$Country[!is.na(comp_all$value)]),
    eligible_n = length(eligible),
    eligible_by_basis = list(pams_and_cat = length(both_countries), pams_only = length(pams_only_countries)),
    eligible_both_cat_national = length(both_countries) - n_cat_bloc,
    eligible_both_cat_eu_bloc = n_cat_bloc,
    countries_pams_and_cat = both_countries,
    require_all_variant_eligible = sum(policy_req_all$eligibility$eligible),
    coalition_members = nrow(allies),
    coalition_members_with_component = list(IEA_PAMS = n_comp_coal[["PAMS"]], CAT = n_comp_coal[["CAT"]]),
    coalition_members_eligible = length(coalition_eligible),
    coalition_by_basis = as.list(table(coalition_basis$policy_basis)),
    coalition_by_basis_and_tier = lapply(split(coalition_basis$policy_basis, coalition_basis$tier),
                                         function(v) as.list(table(v))),
    coalition_pams_only_iso3 = coalition_pams_only$iso3,
    coalition_pams_only_tier = setNames(as.list(coalition_pams_only$tier), coalition_pams_only$iso3),
    coalition_members_not_eligible = coalition_not_eligible$iso3,
    eligible_non_coalition = length(setdiff(eligible, allies$Country)),
    eligible_cells = nrow(comp_wide), cells_per_country = length(techs) * 3,
    cells_by_basis = as.list(table(comp_wide$policy_basis)),
    economies_with_policy_in_all_33_cells = sum(pams_cells_per_country$n == length(techs) * 3),
    economies_with_policy_in_some_cells_only = setNames(as.list(partial_policy$n), partial_policy$Country),
    cat_in_every_cell_of_rated_countries = TRUE,
    cat_rating_eligible = kappa_count(both_countries),
    cat_rating_coalition = kappa_count(intersect(coalition_eligible, both_countries)),
    cat_constant_within_country_n = sum(cat_kappa$kappa == 0.5),
    spearman_countries_with_both = corr_out,
    spearman_note = paste("over the economies scored on IEA PAMS + CAT and their 33 headline cells;",
                          "POL equals PAMS for the IEA PAMS only economies"),
    pams_vs_composite_within_country_policy_cells = c(
      qs(corr_tbl$within[[which(corr_tbl$pair == "POL-PAMS")]]$r),
      list(n_negative = corr_tbl$within_negative[corr_tbl$pair == "POL-PAMS"])),
    pams_vs_composite_within_country_si_cells = c(qs(pams_vs_composite_si$r),
                                                   list(n_negative = sum(pams_vs_composite_si$r < 0, na.rm = TRUE))),
    pams_vs_technology_weight_within_country_both = c(qs(pams_g$r), list(n_negative = sum(pams_g$r < 0, na.rm = TRUE))),
    pams_vs_technology_weight_within_country_by_basis = lapply(split(pams_g_all$r, pams_g_all$policy_basis), function(v)
      c(qs(v), list(n_negative = sum(v < 0, na.rm = TRUE)))),
    pol_scurve_vs_technology_weight_within_country_si_sample = c(
      qs(pol_tech_wc$r),
      list(by_cat_rating = setNames(lapply(seq_len(nrow(pol_tech_by_rating)), function(k)
        list(n = pol_tech_by_rating$n[k], median = rnd(pol_tech_by_rating$median[k]))),
        ifelse(is.na(pol_tech_by_rating$kappa), "no CAT rating (IEA PAMS only)",
               kappa_labels[as.character(pol_tech_by_rating$kappa)])))),
    level_by_basis = setNames(lapply(seq_len(nrow(basis_level)), function(k)
      lapply(basis_level[k, -1], rnd)), basis_level$policy_basis),
    distinct_pams_values_per_country = lapply(split(pams_distinct$k, pams_distinct$policy_basis), qs),
    mean_component_by_basis_and_stage = lapply(split(stage_component_means, stage_component_means$policy_basis),
      function(d) setNames(lapply(seq_len(nrow(d)), function(k) lapply(d[k, c("PAMS", "CAT", "POL", "n")], rnd)),
                           d$supply_chain)),
    mean_pol_scurve_by_stage_si_sample = setNames(as.list(rnd(stage_means$mean_pol)), stage_means$supply_chain),
    mean_pol_scurve_by_stage_and_basis_si_sample = lapply(split(sc_all, sc_all$policy_basis), function(d)
      tapply(d$pol, d$supply_chain, function(v) rnd(mean(v))) %>% as.list()),
    nipo_stored_rows_feb2026 = sum(comp_all$variable == "NIPO Policy Index"),
    nipo_stored_economies_feb2026 = nipo_feb_n_countries,
    nipo_oct2026_rows_headline_techs = nrow(nipo_stored),
    nipo_oct2026_economies = dplyr::n_distinct(nipo_stored$Country),
    nipo_oct2026_not_in_policy_index = sort(nipo_oct_unmatched)
  ),
  sample = list(
    strategic_index_rows = nrow(si),
    rows_countries = n_distinct(si$Country),
    reported_sectors = n_distinct(si$sector),
    excluded_sectors_by_rule = STRATEGIC_EXCLUDED_SECTORS,
    degenerate_sectors = if (nrow(degenerate) == 0) "none" else degenerate$sector,
    es_index_cells_status_11tech = cell_status %>% filter(pillar == "ES") %>% count(cell_status) %>%
      { setNames(as.list(.$n), .$cell_status) },
    eo_index_cells_status_11tech = cell_status %>% filter(pillar == "EO") %>% count(cell_status) %>%
      { setNames(as.list(.$n), .$cell_status) },
    reasons_all_rows = reason_counts(si$si_exclusion),
    eligible_country_rows = nrow(elig_rows),
    eligible_country_rows_by_first_reason = setNames(as.list(elig_reason$n), elig_reason$reason),
    reasons_coalition_rows = reason_counts(si$si_exclusion[si$Country %in% allies$Country]),
    coalition_rows = sum(si$Country %in% allies$Country),
    final_scored_cells = nrow(sc_all), final_countries = n_distinct(sc_all$Country),
    final_sectors_scored = n_distinct(sc_all$sector),
    coalition_scored_cells = nrow(sc), coalition_scored_members = length(coal_si_members),
    coalition_scored_members_by_tier = as.list(table(sc %>% distinct(Country, tier) %>% pull(tier))),
    coalition_sectors_never_scored = sort(setdiff(unique(si$sector), unique(sc$sector))),
    cells_per_country = qs(sc_all %>% count(Country) %>% pull(n)),
    cells_per_coalition_member = qs(sc %>% count(Country) %>% pull(n)),
    es_floor_sectors = lapply(seq_len(nrow(es_floor)), function(k) list(
      sector = es_floor$sector[k], n_countries = es_floor$n_countries[k], es_mode = rnd(es_floor$es_mode[k], 3),
      n_at_mode = es_floor$n_at_mode[k], share_at_mode = rnd(es_floor$share_at_mode[k], 3),
      picks_screen_on = sum(base_sel$top == es_floor$sector[k]),
      picks_screen_off = sum(screen_cmp$off == es_floor$sector[k]))),
    endowment_screen = screen_stats
  ),
  si = list(
    distribution_all = si_dist(sc_all),
    distribution_coalition = si_dist(sc),
    distribution_by_basis = lapply(split(sc_all, sc_all$policy_basis), si_dist),
    top_sector_any_stage_all_countries = sc_all %>% group_by(.data$Country) %>%
      slice_max(.data$si, n = 1, with_ties = FALSE) %>% ungroup() %>% count(.data$sector, sort = TRUE) %>%
      head(6) %>% { setNames(as.list(.$n), .$sector) },
    nuclear_upstream_top_any_stage = {
      tops <- sc_all %>% group_by(.data$Country) %>% slice_max(.data$si, n = 1, with_ties = FALSE) %>%
        ungroup() %>% filter(.data$sector == "Nuclear - Upstream")
      list(n_countries = nrow(tops), n_si_countries = n_distinct(sc_all$Country),
           n_at_es_floor_mode = sum(round(tops$ES, 6) == round(nu_floor_es, 6)),
           n_without_resource_base = sum(tops$resource_base %in% FALSE))
    },
    top_cells_all = sc_all %>% arrange(desc(.data$si)) %>% head(10) %>%
      { setNames(as.list(rnd(.$si)), paste(.$Country, .$sector, sep = " / ")) },
    technology_weight_g_t = tech_ghg %>% arrange(ghg_index) %>% { setNames(as.list(rnd(.$ghg_index)), .$tech) },
    stage_weight_lambda_s = as.list(STRATEGIC_SUPPLY_CHAIN_WEIGHTS),
    r2_fixed_effects_all = fe_r2(sc_all),
    r2_fixed_effects_coalition = fe_r2(sc),
    decomposition_all = decomp_all,
    decomposition_coalition = decomp_coal,
    sector_mean_si_top5 = head(sector_means, 5) %>% { setNames(as.list(rnd(.$mean_si)), .$sector) },
    sector_mean_si_bottom5 = tail(sector_means, 5) %>% { setNames(as.list(rnd(.$mean_si)), .$sector) },
    sector_mean_si_all = setNames(as.list(rnd(sector_means$mean_si)), sector_means$sector),
    sector_sd_across_countries_median = rnd(stats::median(sector_means$sd_si, na.rm = TRUE)),
    stage_means = lapply(split(stage_means, stage_means$supply_chain), function(d)
      lapply(d[c("mean_si", "mean_need", "mean_eo", "mean_pol", "mean_POL", "n")], rnd))
  ),
  allied = list(
    coalition_members = nrow(allies), core = n_core, partner = n_partner,
    n_members_with_selection = nrow(base_sel),
    n_with_selection_by_tier = as.list(table(base_sel$tier)),
    members_without_selection = setNames(as.list(members_no_pick$reason), members_no_pick$iso3),
    members_without_selection_tier = setNames(as.list(members_no_pick$tier), members_no_pick$iso3),
    picks = setNames(lapply(seq_len(nrow(base_sel)), function(k) {
      cell <- sc[sc$Country == base_sel$Country[k] & sc$sector == base_sel$top[k], ]
      sc_off <- screen_cmp[screen_cmp$Country == base_sel$Country[k], ]
      pp <- pick_position[pick_position$Country == base_sel$Country[k], ]
      list(iso3 = base_sel$iso3[k], tier = base_sel$tier[k], policy_basis = cell$policy_basis,
           top = base_sel$top[k], si = rnd(base_sel$top_score[k]),
           second = base_sel$second[k], second_si = rnd(base_sel$second_score[k]), margin = rnd(base_sel$margin[k], 6),
           pick_without_screen = sc_off$off, si_without_screen = rnd(sc_off$si_off),
           ES = rnd(cell$ES), EO = rnd(cell$EO), pol = rnd(cell$pol), resource_evidence = cell$resource_evidence,
           es_share_at_or_below_in_sector = rnd(pp$es_le_share), es_is_sector_min = pp$es_is_min,
           es_at_floor_mode = pp$es_at_floor_mode, eo_share_at_or_below_in_sector = rnd(pp$eo_le_share))
    }), base_sel$Country),
    picks_by_basis = lapply(split(base_sel$top, sc$policy_basis[match(base_sel$Country, sc$Country)]), pick_tab),
    members_pams_only_with_pick = sort(allies$iso3[allies$Country %in% members_pams_only_picked]),
    pick_position_by_sector = setNames(lapply(seq_len(nrow(pick_position_by_sector)), function(k)
      lapply(pick_position_by_sector[k, -1], function(z) if (is.numeric(z)) rnd(z) else z)),
      pick_position_by_sector$sector),
    midstream_picks_es_bottom_decile = sum(mid_picks$es_le_share <= 0.10),
    midstream_picks_es_sector_min = sum(mid_picks$es_is_min),
    midstream_picks_n = nrow(mid_picks),
    screen_changed_to_same_technology_midstream = nrow(same_tech_midstream),
    screen_changed_to_same_technology_midstream_by_tech = as.list(table(same_tech_midstream$tech_on)),
    nuclear_midstream_picks = setNames(lapply(seq_len(nrow(nuc_mid_detail)), function(k) list(
      policy_basis = nuc_mid_detail$policy_basis[k], ES = rnd(nuc_mid_detail$ES[k]), EO = rnd(nuc_mid_detail$EO[k]),
      pol = rnd(nuc_mid_detail$pol[k]), pick_without_screen = nuc_mid_detail$off[k],
      si_forgone = rnd(nuc_mid_detail$si_forgone[k]),
      nuclear_downstream_scored = nuc_mid_detail$downstream_scored[k],
      nuclear_downstream_exclusion = nuc_mid_detail$downstream_exclusion[k])), nuc_mid_detail$iso3),
    nuclear_midstream_picks_without_scored_nuclear_downstream =
      sort(nuc_mid_detail$iso3[!nuc_mid_detail$downstream_scored]),
    fixed_priors_eligible_sectors = setNames(as.list(rnd(priors_tbl$prior)), priors_tbl$sector),
    pick_distribution_screen_on = pick_tab(base_sel$top),
    pick_distribution_screen_off = pick_tab(screen_cmp$off),
    pick_members_screen_on = pick_members(screen_cmp, "on"),
    pick_members_screen_off = pick_members(screen_cmp, "off"),
    picks_by_stage_screen_on = as.list(table(sub("^.* - ", "", base_sel$top))),
    picks_by_stage_screen_off = as.list(table(sub("^.* - ", "", screen_cmp$off))),
    picks_by_tier_screen_on = lapply(split(base_sel$top, base_sel$tier), pick_tab),
    n_changed_by_screen = sum(screen_cmp$changed),
    changed_by_screen = setNames(lapply(which(screen_cmp$changed), function(k)
      list(off = screen_cmp$off[k], on = screen_cmp$on[k], si_forgone = rnd(screen_cmp$si_forgone[k]))),
      screen_cmp$iso3[screen_cmp$changed]),
    si_forgone_changed = qs(screen_cmp$si_forgone[screen_cmp$changed]),
    transition_off_to_on = lapply(seq_len(nrow(transition)), function(k)
      list(off = transition$off[k], on = transition$on[k], n = transition$n[k])),
    nuclear_upstream_screen_on_members = sort(screen_cmp$iso3[screen_cmp$on == "Nuclear - Upstream"]),
    nuclear_upstream_floor_countries_equal_no_resource_base = nu_floor_equals_no_resource,
    nuclear_upstream_screen_on_evidence = sc %>%
      semi_join(base_sel %>% filter(top == "Nuclear - Upstream") %>% select(Country, sector = top),
                by = c("Country", "sector")) %>% { setNames(as.list(.$resource_evidence), .$iso3) },
    margin = qs(base_sel$margin, 6),
    margin_smallest = base_sel %>% arrange(margin) %>% head(5) %>%
      { setNames(as.list(round(.$margin, 6)), .$iso3) },
    pick_profile = setNames(lapply(seq_len(nrow(pick_profile)), function(k) {
      list(n = pick_profile$n[k], median_ES = rnd(pick_profile$median_ES[k]),
           coalition_median_ES_in_sector = rnd(coal_sector_es_median$m[coal_sector_es_median$sector == pick_profile$sector[k]]),
           median_EO = rnd(pick_profile$median_EO[k]), median_pol = rnd(pick_profile$median_pol[k]),
           median_rank_policy = pick_profile$median_rank_policy[k],
           median_rank_need = pick_profile$median_rank_need[k], median_rank_eo = pick_profile$median_rank_eo[k])
    }), pick_profile$sector),
    margin_below_0.01 = sort(base_sel$iso3[base_sel$margin < 0.01]),
    n_margin_below_0.005 = sum(base_sel$margin < 0.005),
    share_margin_below_0.005 = rnd(mean(base_sel$margin < 0.005), 4),
    margin_below_0.005 = sort(base_sel$iso3[base_sel$margin < 0.005]),
    n_margin_below_0.01 = sum(base_sel$margin < 0.01),
    share_margin_below_0.01 = rnd(mean(base_sel$margin < 0.01), 4),
    nuclear_midstream_pickers = setNames(lapply(seq_len(nrow(nuc_mid_gaps)), function(k) {
      z <- nuc_mid_gaps[k, ]
      list(n_pick_eligible_sectors = nuc_mid_sets$n_allied[nuc_mid_sets$iso3 == z$iso3],
           pick_eligible_sectors = nuc_mid_sets$sectors[nuc_mid_sets$iso3 == z$iso3],
           runner_up = z$second,
           gap = setNames(lapply(terms, function(t) rnd(z[[paste0(t, "_top")]] - z[[paste0(t, "_second")]])),
                          term_labels[terms]))
    }), nuc_mid_gaps$iso3),
    es_spread_midstream_all_si = setNames(lapply(seq_len(nrow(es_spread_mid)), function(k)
      list(n = es_spread_mid$n[k], iqr_ES = rnd(es_spread_mid$iqr_ES[k]), sd_ES = rnd(es_spread_mid$sd_ES[k]),
           median_ES = rnd(es_spread_mid$median_ES[k]))), es_spread_mid$sector),
    n_margin_below_0.02 = sum(base_sel$margin < 0.02),
    mean_term_gap_top_minus_runnerup = as.list(setNames(rnd(pick_term_gaps), term_labels[terms])),
    pick_rank_on_terms_within_allied_set = pick_rank_summary,
    top_sector_any_stage_differs_from_pick = nrow(top_overall_differs),
    top_sector_any_stage_by_stage = as.list(table(country_top_overall$supply_chain)),
    focal_top5 = lapply(split(focal_term_ranks, focal_term_ranks$Country), function(d) {
      d <- d[order(-d$si), ]
      lapply(seq_len(nrow(d)), function(k) list(sector = d$sector[k], si = rnd(d$si[k]),
        rank_policy = d$rk_c_pol[k], rank_eo = d$rk_c_eo[k], rank_need = d$rk_c_need[k],
        n_cells = d$n_cells[k], resource_base = d$resource_base[k]))
    }),
    focal_pick = setNames(as.list(base_sel$top[match(focal, base_sel$Country)]), focal),
    pick_vs_design = pick_vs_design,
    nuclear_upstream_pick_resources = nuclear_upstream_pick_resources,
    screen_production_required = screen_production_required,
    cell_constant = cell_constant,
    canada_downstream_es = canada_downstream
  ),
  robustness = c(
    variant_summary,
    list(strength_within_member_by_tier = lapply(split(strength_wc$r, strength_wc$tier), qs),
         nipo_variant_members = nipo_members,
         policy_both_components_required = require_all)
  ),
  reconciliation_with_shipped = lapply(reconciliation, function(z) if (is.numeric(z)) rnd(z) else z),
  # Earlier intermediate runs, as recorded in the decision log (integration/pending.md); quoted in
  # the text only to show how far the picks moved during the final data revisions.
  history_from_decision_log = list(
    source = "integration/pending.md (decision log of the workflow)",
    run_2026_10_08_picks_with_screen = list(`Electric Grid - Midstream` = 17L, `Batteries - Midstream` = 11L),
    run_2026_10_08_members_with_pick = 45L, run_2026_10_08_coalition_members = 56L,
    run_2026_10_10_si_countries = 192L,
    reason_si_countries_fell = "new Upstream trade baskets add an imputed Trade category to small economies' Upstream cells"
  )
)
write_paper_results(results, "strategic")
message("strategic: figures, table and results written.")
