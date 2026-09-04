# ==============================================================================
# Task 4 - pending measures and real duration
# ------------------------------------------------------------------------------
# Verifies the requirement that announced-but-unimplemented measures survive
# into policy_base, policy_asof, by_policy and the allocation table, with row
# counts at each stage, and reports the duration distributions that m_duration
# was throwing away.
#
# Writes diagnostics/pending_and_duration.csv
#
# Run from the repo root:
#   Rscript diagnostics/05_pending_and_duration.R
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

policy_base <- build_policy_base(nipo_country)
# Pass as_of_date explicitly: the default would be 2028-10-01, two years past
# the data, which would inflate every exposure_days figure below.
as_of <- as.Date("2026-06-30")
policy_asof <- add_asof_flags(policy_base, as_of_date = as_of)
by_policy <- build_by_policy(policy_asof, cpc_names = cpc_names)
alloc <- allocate_policy_to_tech_sc(policy_asof)

# ---- 1) Row-count survival --------------------------------------------------
hr("row counts: do pending measures survive each stage?")

counts <- tibble::tibble(
  stage = c("raw_nipo", "nipo_country", "policy_base", "policy_asof",
            "by_policy", "allocation_table"),
  rows = c(nrow(nipo_raw), nrow(nipo_country), nrow(policy_base),
           nrow(policy_asof), nrow(by_policy), nrow(alloc)),
  distinct_policies = c(
    nrow(nipo_raw), nrow(nipo_country), dplyr::n_distinct(policy_base$policy_id),
    dplyr::n_distinct(policy_asof$policy_id), dplyr::n_distinct(by_policy$policy_id),
    dplyr::n_distinct(alloc$policy_id)
  ),
  pending_policies = c(
    sum(is.na(as.Date(nipo_raw$`Implementation Date`)) &
          !is.na(as.Date(nipo_raw$`Announcement Date`))),
    NA_integer_,
    sum(policy_base$pending_implementation),
    sum(policy_asof$pending_implementation),
    sum(by_policy$pending_implementation),
    dplyr::n_distinct(alloc$policy_id[alloc$pending_implementation])
  )
)
print(as.data.frame(counts), right = FALSE)

pending_n <- sum(policy_asof$pending_implementation)
cat("\npending measures: ", pending_n, " of ", nrow(policy_asof),
    sprintf(" (%.1f%%)", 100 * pending_n / nrow(policy_asof)), "\n", sep = "")
cat("their total scale_strength_pkg: ",
    sum(policy_asof$scale_strength_pkg[policy_asof$pending_implementation], na.rm = TRUE),
    "  (zero by design: m_duration = 0 for a stock index)\n", sep = "")
cat("pending rows counted in the active stock: ",
    sum(policy_asof$is_active_asof & policy_asof$pending_implementation), "\n", sep = "")

# ---- 2) m_duration saturation ------------------------------------------------
hr("m_duration: how much information does it carry?")
md <- policy_asof$m_duration
cat("share of ALL policies with m_duration exactly 1.0 : ",
    round(mean(md == 1, na.rm = TRUE), 4), "\n", sep = "")
cat("share of ALL policies with m_duration exactly 0.0 : ",
    round(mean(md == 0, na.rm = TRUE), 4), "\n", sep = "")
cat("share strictly between 0 and 1                    : ",
    round(mean(md > 0 & md < 1, na.rm = TRUE), 4), "\n", sep = "")
cat("\nAmong implemented measures, m_duration by removal status:\n")
print(policy_asof %>%
        filter(!is.na(.data$impl_date)) %>%
        mutate(has_removal = !is.na(.data$removal_date)) %>%
        group_by(.data$has_removal) %>%
        summarise(n = dplyr::n(), mean_m_duration = round(mean(.data$m_duration), 4),
                  share_at_1 = round(mean(.data$m_duration == 1), 4), .groups = "drop") %>%
        as.data.frame(), right = FALSE)

# ---- 3) The signal m_duration was discarding --------------------------------
hr("implementation lag and real duration")
cat("impl_lag_days (announcement -> implementation):\n")
print(summary(policy_asof$impl_lag_days))
cat("\nnegative lags suppressed to NA: ",
    sum(!is.na(policy_asof$announce_date) & !is.na(policy_asof$impl_date) &
          policy_asof$impl_date < policy_asof$announce_date), "\n", sep = "")

cat("\nobserved_duration_days (completed lifetimes only):\n")
print(summary(policy_asof$observed_duration_days))
cat("n with a completed lifetime: ", sum(!is.na(policy_asof$observed_duration_days)), "\n", sep = "")

cat("\nexposure_days (censored at as_of_date):\n")
print(summary(policy_asof$exposure_days))
cat("n censored (still in force): ", sum(policy_asof$duration_censored), "\n", sep = "")

# ---- 4) Save ----------------------------------------------------------------
summary_tbl <- tibble::tibble(
  metric = c("as_of_date", "total_policies", "pending_policies", "pending_share",
             "pending_strength", "m_duration_at_1_share", "m_duration_at_0_share",
             "impl_lag_median_days", "impl_lag_p75_days",
             "n_completed_lifetimes", "observed_duration_median_days",
             "n_censored", "exposure_median_days"),
  value = c(
    as.character(as_of), nrow(policy_asof), pending_n,
    round(pending_n / nrow(policy_asof), 4),
    sum(policy_asof$scale_strength_pkg[policy_asof$pending_implementation], na.rm = TRUE),
    round(mean(md == 1, na.rm = TRUE), 4), round(mean(md == 0, na.rm = TRUE), 4),
    stats::median(policy_asof$impl_lag_days, na.rm = TRUE),
    as.numeric(stats::quantile(policy_asof$impl_lag_days, 0.75, na.rm = TRUE, names = FALSE)),
    sum(!is.na(policy_asof$observed_duration_days)),
    stats::median(policy_asof$observed_duration_days, na.rm = TRUE),
    sum(policy_asof$duration_censored),
    stats::median(policy_asof$exposure_days, na.rm = TRUE)
  )
)
utils::write.csv(summary_tbl, file.path(out_dir, "pending_and_duration.csv"), row.names = FALSE)
utils::write.csv(counts, file.path(out_dir, "pending_row_counts.csv"), row.names = FALSE)
message("\nWrote pending_and_duration.csv and pending_row_counts.csv")
