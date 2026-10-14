# ==============================================================================
# Task 9 - cross-cutting reclassification and EU consolidation
# ------------------------------------------------------------------------------
# Answers the two questions the review flagged as needing judgement:
#   1. How many policies and how much strength does crosscutting_mode
#      reclassify?
#   2. How many rows and how much strength sit in eu_wide / eu_member / non_eu?
#
# Writes diagnostics/crosscutting_impact.csv and diagnostics/eu_split.csv
#
# Run from the repo root:
#   Rscript diagnostics/08_crosscutting_and_eu.R
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

policy_base <- build_policy_base(nipo_country)
policy_asof <- add_asof_flags(policy_base, as_of_date = as.Date("2026-06-30"))

# ---- 1) How much is Cross-cutting to begin with? ----------------------------
hr("cross-cutting population")
alloc0 <- allocate_policy_to_tech_sc(policy_asof)
cc <- alloc0 %>% filter(.data$tech == "Cross-cutting")
cat("policies with hs6_n == 0 (allocated Cross-cutting): ",
    dplyr::n_distinct(cc$policy_id), " of ", dplyr::n_distinct(alloc0$policy_id),
    sprintf(" (%.1f%%)", 100 * dplyr::n_distinct(cc$policy_id) /
              dplyr::n_distinct(alloc0$policy_id)), "\n", sep = "")
cat("their scale_strength_pkg total: ",
    round(sum(cc$scale_strength_pkg, na.rm = TRUE), 1), "\n", sep = "")
cat("share of all policy strength  : ",
    round(sum(cc$scale_strength_pkg, na.rm = TRUE) /
            sum(alloc0$scale_strength_pkg[!duplicated(alloc0$policy_id)], na.rm = TRUE), 4),
    "\n", sep = "")
cat("active as-of only             : ",
    dplyr::n_distinct(cc$policy_id[cc$is_active_asof]), " policies\n", sep = "")

# ---- 2) Reclassification by mode --------------------------------------------
hr("crosscutting_mode: what each mode does")

rows <- lapply(c("report_only", "sector_flagged", "uniform"), function(m) {
  o <- build_by_tech_sc(
    policy_asof_tbl = policy_asof, tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe, supply_chain_universe = sc_universe,
    confidence_mode = "none", crosscutting_mode = m
  )
  d <- o$data
  real <- d %>% filter(.data$tech != "Cross-cutting")
  ccr <- d %>% filter(.data$tech == "Cross-cutting")

  # How many cross-cutting POLICIES actually got attributed under this mode.
  a <- o$policy_alloc
  attributed <- a %>% filter(.data$is_crosscutting_policy, .data$tech != "Cross-cutting")

  tibble::tibble(
    crosscutting_mode = m,
    output_rows = nrow(d),
    populated_cells = dplyr::n_distinct(paste(real$tech, real$supply_chain)),
    crosscutting_rows_kept = nrow(ccr),
    policies_reclassified = dplyr::n_distinct(attributed$policy_id),
    strength_reclassified = round(sum(attributed$scale_strength_pkg * attributed$alloc,
                                      na.rm = TRUE), 1),
    strength_in_named_cells = round(sum(real$domestic_stock_sum, na.rm = TRUE), 1),
    strength_left_crosscutting = round(sum(ccr$domestic_stock_sum, na.rm = TRUE), 1)
  )
})
cc_impact <- bind_rows(rows)
print(as.data.frame(cc_impact), right = FALSE)
utils::write.csv(cc_impact, file.path(out_dir, "crosscutting_impact.csv"), row.names = FALSE)

cat("\nCells that exist ONLY because of uniform smearing:\n")
d_uni <- build_by_tech_sc(policy_asof, tech_sc_cpc_lu, tech_universe, sc_universe,
                          confidence_mode = "none", crosscutting_mode = "uniform")$data
d_rep <- build_by_tech_sc(policy_asof, tech_sc_cpc_lu, tech_universe, sc_universe,
                          confidence_mode = "none", crosscutting_mode = "report_only")$data
cells_uni <- unique(paste(d_uni$tech, d_uni$supply_chain, sep = " / "))
cells_rep <- unique(paste(d_rep$tech[d_rep$tech != "Cross-cutting"],
                          d_rep$supply_chain[d_rep$tech != "Cross-cutting"], sep = " / "))
invented <- setdiff(cells_uni, cells_rep)
cat("  ", length(invented), " cells: ", paste(invented, collapse = "; "), "\n", sep = "")

# ---- 3) EU split -------------------------------------------------------------
hr("EU consolidation")
by_ts <- neis_consolidate_eu(d_rep, mode = "both_flagged")

eu_split <- by_ts %>%
  group_by(.data$eu_view) %>%
  summarise(
    n_rows = dplyr::n(),
    n_countries = dplyr::n_distinct(.data$country),
    strength = round(sum(.data$domestic_stock_sum, na.rm = TRUE), 1),
    n_active_policies = sum(.data$n_active_policies, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(share_strength = round(.data$strength / sum(.data$strength), 4))

print(as.data.frame(eu_split), right = FALSE)
utils::write.csv(eu_split, file.path(out_dir, "eu_split.csv"), row.names = FALSE)

cat("\nImplementing jurisdictions mapping to eu_wide:\n")
print(unique(by_ts$country[by_ts$eu_view == "eu_wide"]))
cat("\nEU member jurisdictions present:\n")
print(sort(unique(by_ts$country[by_ts$eu_view == "eu_member"])))

cat("\nDouble-count exposure: summing eu_wide + eu_member gives ",
    round(sum(eu_split$strength[eu_split$eu_view %in% c("eu_wide", "eu_member")]), 1),
    " against ", round(sum(eu_split$strength), 1), " total.\n", sep = "")

# ---- 4) The double-count that IS present -------------------------------------
hr("EU act replication - the actual double-count mechanism")
cat("There are no eu_wide rows in this export, so the eu_wide-versus-eu_member\n")
cat("double-count the review described does not arise. A DIFFERENT one does:\n")
cat("a single EU act is recorded once per member state.\n\n")

acts <- policy_asof %>%
  mutate(is_eu_member = .data$iso3 %in% EU_MEMBER_ISO3) %>%
  group_by(.data$state_act_id) %>%
  summarise(
    n_jurisdictions = dplyr::n_distinct(.data$country),
    n_eu_members = dplyr::n_distinct(.data$country[.data$is_eu_member]),
    strength = sum(.data$scale_strength_pkg, na.rm = TRUE),
    strength_eu = sum(.data$scale_strength_pkg[.data$is_eu_member], na.rm = TRUE),
    .groups = "drop"
  )

replicated <- acts %>% filter(.data$n_eu_members > 1)
cat("state acts spanning >1 EU member state : ", nrow(replicated),
    " of ", nrow(acts), "\n", sep = "")
cat("max EU members on one act             : ", max(acts$n_eu_members), "\n", sep = "")
cat("strength carried by replicated acts   : ", round(sum(replicated$strength_eu), 1), "\n", sep = "")
cat("total EU-member strength              : ",
    round(sum(acts$strength_eu), 1), "\n", sep = "")
cat("share of EU strength that is replicated: ",
    round(sum(replicated$strength_eu) / sum(acts$strength_eu), 4), "\n", sep = "")

cat("\nIf each replicated act were counted ONCE rather than per member state,\n")
cat("EU-member strength would fall from ", round(sum(acts$strength_eu), 1),
    " to roughly ",
    round(sum(acts$strength_eu) - sum(replicated$strength_eu) +
            sum(replicated$strength_eu / replicated$n_eu_members), 1), ".\n", sep = "")

eu_replication <- tibble::tibble(
  metric = c("state_acts_total", "state_acts_spanning_multiple_eu_members",
             "max_eu_members_on_one_act", "strength_replicated_acts",
             "strength_eu_total", "share_eu_strength_replicated",
             "strength_eu_if_deduplicated"),
  value = c(nrow(acts), nrow(replicated), max(acts$n_eu_members),
            round(sum(replicated$strength_eu), 1), round(sum(acts$strength_eu), 1),
            round(sum(replicated$strength_eu) / sum(acts$strength_eu), 4),
            round(sum(acts$strength_eu) - sum(replicated$strength_eu) +
                    sum(replicated$strength_eu / replicated$n_eu_members), 1))
)
utils::write.csv(eu_replication, file.path(out_dir, "eu_act_replication.csv"),
                 row.names = FALSE)

message("\nWrote crosscutting_impact.csv, eu_split.csv and eu_act_replication.csv")
