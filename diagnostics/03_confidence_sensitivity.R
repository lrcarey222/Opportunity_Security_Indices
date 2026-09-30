# ==============================================================================
# Task 2 - confidence sensitivity
# ------------------------------------------------------------------------------
# Rebuilds by_tech_sc under all four confidence_mode values and reports, per
# tech x supply_chain, the Spearman correlation of country rankings between
# "legacy" and each other mode.
#
# Writes:
#   diagnostics/confidence_sensitivity.csv          (expansion on, as published)
#   diagnostics/confidence_sensitivity_nocc.csv     (cross-cutting not expanded)
#
# The second file matters: cross-cutting rows are pinned at confidence 0.25
# while mapped rows sit in [0.75, 2.00], so with expansion on, part of the
# legacy-vs-none gap is really the cross-cutting attribution problem (Task 9)
# rather than the confidence multiplier itself.
#
# Run from the repo root:
#   Rscript diagnostics/03_confidence_sensitivity.R
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

nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - February 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
message("NIPO export: ", basename(nipo_policy_path))

nipo_raw <- readxl::read_excel(nipo_policy_path, sheet = 1)
subcat_raw <- readr::read_csv(
  file.path(raw_data_path, "hs6_categories_with_essential.csv"),
  show_col_types = FALSE
) %>%
  rename("Value.Chain" = "Value Chain")
country_info <- standardize_country_info(
  utils::read.csv(file.path(raw_data_path, "wdi_country_info.csv"))
)

# Build the shared inputs once. This mirrors nipo_policy_outputs() up to the
# point where confidence_mode starts to matter, so the four variants differ in
# nothing but that argument.
hs6_essential_tbl <- resolve_hs6_essential_tbl(NULL)
subcat_driver <- prepare_subcat_mapping(subcat_raw, hs6_essential_tbl) %>%
  filter(.data$essential_for_tech_sc) %>%
  transmute(HS6 = .data$code, Technology = .data$Technology,
            `Value.Chain` = .data$`Value.Chain`, Sub.Sector = .data$Sub.Sector) %>%
  distinct()

nipo_country <- clean_nipo_raw(nipo_raw, subcat_raw, country_info, hs6_essential_tbl)
cpc_hs <- get_cpc_hs_map()
cpc_names <- get_cpc3_names()
cpc_pairs <- build_cpc3_to_tech_sc_pairs(subcat_driver, cpc_hs)
nipo_country <- attach_cpc_validation(nipo_country, cpc_hs, cpc_pairs)

tech_universe <- normalize_chr_vec(dplyr::pull(subcat_raw, "Technology"))
sc_universe <- normalize_chr_vec(dplyr::pull(subcat_raw, "Value.Chain"))
tech_sc_cpc_lu <- build_tech_sc_cpc_lookup(subcat_driver, cpc_hs, cpc_names)

policy_base <- build_policy_base(nipo_country)
policy_asof <- add_asof_flags(policy_base)

cat("as_of_date: ", as.character(policy_asof$as_of_date[1]), "\n", sep = "")
cat("policy rows: ", nrow(policy_asof), "\n", sep = "")

hr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

run_variant_report <- function(expand_cc, path, label) {
  hr(label)
  stab <- dis_variant_stability(
    policy_asof_tbl = policy_asof,
    tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe,
    supply_chain_universe = sc_universe,
    expand_cross_cutting = expand_cc
  )
  utils::write.csv(stab, path, row.names = FALSE)

  cat("cells: ", nrow(stab), "\n", sep = "")
  cat("\nTen cells with the LOWEST rho(legacy, none) - the headline number:\n")
  show <- stab %>%
    select(tech, supply_chain, n_countries,
           rho_legacy_none, rho_legacy_downweight, rho_legacy_filter,
           starts_with("n_rank_moved_gt3_none")) %>%
    head(10) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
  print(as.data.frame(show), right = FALSE)

  cat("\nsummary of rho(legacy, none) across cells:\n")
  print(summary(stab$rho_legacy_none))
  cat("\nmedian rho(legacy, none)      : ", round(stats::median(stab$rho_legacy_none, na.rm = TRUE), 4), "\n", sep = "")
  cat("median rho(legacy, downweight): ", round(stats::median(stab$rho_legacy_downweight, na.rm = TRUE), 4), "\n", sep = "")
  cat("median rho(legacy, filter)    : ", round(stats::median(stab$rho_legacy_filter, na.rm = TRUE), 4), "\n", sep = "")

  moved_col <- grep("^n_rank_moved_gt3_none$", names(stab), value = TRUE)
  if (length(moved_col) == 1) {
    cat("total country-cells whose rank moves >3 places (none vs legacy): ",
        sum(stab[[moved_col]], na.rm = TRUE), " of ", sum(stab$n_countries), "\n", sep = "")
  }
  invisible(stab)
}

run_variant_report(TRUE,  file.path(out_dir, "confidence_sensitivity.csv"),
                   "confidence sensitivity - cross-cutting EXPANDED (as published)")
run_variant_report(FALSE, file.path(out_dir, "confidence_sensitivity_nocc.csv"),
                   "confidence sensitivity - cross-cutting NOT expanded (isolates Task 2)")

message("\nWrote confidence_sensitivity.csv and confidence_sensitivity_nocc.csv")
