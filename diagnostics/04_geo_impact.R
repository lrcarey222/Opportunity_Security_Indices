# ==============================================================================
# Task 3 - impact of removing m_geo and the bare constant from the product
# ------------------------------------------------------------------------------
# Unlike mapping_confidence, m_geo varies systematically across countries: a
# jurisdiction that names many affected partners (sanctions-heavy programmes)
# scored higher than one naming few, for the same domestic measure. So this fix
# can move rankings in a way Task 2 did not.
#
# Both terms are pure multiplicative factors on scale_strength_base, so the
# geo-included and geo-excluded strengths differ by exactly (m_geo * 2). That
# lets one allocation serve both variants: the script asserts the algebra on a
# slice first, then applies it at full scale.
#
# Writes diagnostics/geo_impact.csv
#
# Run from the repo root:
#   Rscript diagnostics/04_geo_impact.R
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

# ---- 1) Assert the algebra on a slice ---------------------------------------
hr("verifying (m_geo * 2) factorisation on a slice")
slice_raw <- nipo_raw %>% filter(.data$`Implementing Jurisdiction` %in%
                                   c("Japan", "Denmark", "Germany"))
nc_s <- clean_nipo_raw(slice_raw, subcat_raw, country_info, hs6_essential_tbl)
b_geo <- build_policy_base(nc_s, include_geo_in_strength = TRUE, strength_constant = 2)
b_new <- build_policy_base(nc_s)
ratio <- b_geo$scale_strength_pkg / b_new$scale_strength_pkg
expected <- b_geo$m_geo * 2
ok <- isTRUE(all.equal(unname(ratio[is.finite(ratio)]), unname(expected[is.finite(ratio)])))
cat("ratio equals m_geo * 2 : ", ok, "\n", sep = "")
stopifnot(ok)

# ---- 2) Full-scale comparison from one allocation ---------------------------
hr("full-scale rank impact")
nipo_country <- clean_nipo_raw(nipo_raw, subcat_raw, country_info, hs6_essential_tbl)
cpc_hs <- get_cpc_hs_map(); cpc_names <- get_cpc3_names()
nipo_country <- attach_cpc_validation(
  nipo_country, cpc_hs, build_cpc3_to_tech_sc_pairs(subcat_driver, cpc_hs)
)
tech_universe <- normalize_chr_vec(dplyr::pull(subcat_raw, "Technology"))
sc_universe <- normalize_chr_vec(dplyr::pull(subcat_raw, "Value.Chain"))
tech_sc_cpc_lu <- build_tech_sc_cpc_lookup(subcat_driver, cpc_hs, cpc_names)

# Build with geo INCLUDED, then derive the geo-excluded strength by algebra.
policy_base <- build_policy_base(nipo_country,
                                 include_geo_in_strength = TRUE,
                                 strength_constant = 2)
policy_asof <- add_asof_flags(policy_base)

cat("m_geo distribution across policies:\n")
print(summary(policy_asof$m_geo))
cat("\nshare of policies with more than one affected jurisdiction: ",
    round(mean(policy_asof$partner_n > 1, na.rm = TRUE), 4), "\n", sep = "")

alloc <- allocate_policy_to_tech_sc(policy_asof)
alloc <- expand_cross_cutting_rows(
  alloc, tech_universe = tech_universe,
  supply_chain_universe = sc_universe, split_strength = TRUE
) %>%
  filter(.data$tech != "Cross-cutting", .data$supply_chain != "Cross-cutting")

agg_for <- function(alloc_tbl, label) {
  build_by_tech_sc(
    policy_asof_tbl = policy_asof, tech_sc_cpc_lu = tech_sc_cpc_lu,
    tech_universe = tech_universe, supply_chain_universe = sc_universe,
    confidence_mode = "none", alloc_long = alloc_tbl
  )$data %>%
    select(iso3, country, tech, supply_chain, domestic_stock_sum) %>%
    rename("score_{label}" := "domestic_stock_sum")
}

with_geo <- agg_for(alloc, "geo")
no_geo <- agg_for(
  alloc %>% mutate(scale_strength_pkg = .data$scale_strength_pkg / (.data$m_geo * 2)),
  "nogeo"
)

cmp <- full_join(with_geo, no_geo, by = c("iso3", "country", "tech", "supply_chain"))

stab <- cmp %>%
  group_by(.data$tech, .data$supply_chain) %>%
  group_modify(function(g, key) {
    r_geo <- dplyr::dense_rank(dplyr::desc(g$score_geo))
    r_no <- dplyr::dense_rank(dplyr::desc(g$score_nogeo))
    tibble::tibble(
      n_countries = nrow(g),
      rho_geo_nogeo = suppressWarnings(stats::cor(g$score_geo, g$score_nogeo,
                                                  method = "spearman",
                                                  use = "pairwise.complete.obs")),
      n_rank_moved_gt3 = sum(abs(r_geo - r_no) > 3, na.rm = TRUE),
      max_rank_move = suppressWarnings(max(abs(r_geo - r_no), na.rm = TRUE)),
      level_ratio_median = stats::median(g$score_geo / g$score_nogeo, na.rm = TRUE)
    )
  }) %>%
  ungroup() %>%
  arrange(.data$rho_geo_nogeo)

utils::write.csv(stab, file.path(out_dir, "geo_impact.csv"), row.names = FALSE)

cat("\nTen cells with the LOWEST rho(with geo, without geo):\n")
print(as.data.frame(stab %>% head(10) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))), right = FALSE)

cat("\nsummary of rho across cells:\n")
print(summary(stab$rho_geo_nogeo))
cat("\nmedian rho                : ", round(stats::median(stab$rho_geo_nogeo, na.rm = TRUE), 4), "\n", sep = "")
cat("country-cells moving >3   : ", sum(stab$n_rank_moved_gt3, na.rm = TRUE),
    " of ", sum(stab$n_countries), "\n", sep = "")
cat("largest single rank move  : ", max(stab$max_rank_move, na.rm = TRUE), "\n", sep = "")
cat("median level ratio        : ", round(stats::median(stab$level_ratio_median, na.rm = TRUE), 3),
    "x (levels fall by this factor)\n", sep = "")

message("\nWrote geo_impact.csv")
