# ==============================================================================
# Task 5 - scale field coverage by year
# ------------------------------------------------------------------------------
# Reports, per year, the share of ACTIVE policies with each scale field
# populated, so the coverage boundary is visible before choosing a scale_mode
# for publication.
#
# "Active in year Y" means implemented on or before 31 Dec Y and not removed
# before 1 Jan Y, i.e. in force at some point during Y.
#
# Writes:
#   diagnostics/scale_coverage_by_year.csv   (active policies - the Task 5 ask)
#   diagnostics/scale_coverage_by_announce_year.csv  (all policies, by announcement)
#
# Run from the repo root:
#   Rscript diagnostics/06_scale_coverage.R
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
raw <- readxl::read_excel(nipo_policy_path, sheet = 1)

d <- raw %>%
  transmute(
    announce_year = suppressWarnings(as.integer(format(as.Date(.data$`Announcement Date`), "%Y"))),
    impl_date = as.Date(.data$`Implementation Date`),
    removal_date = as.Date(.data$`Removal Date`),
    trade = suppressWarnings(as.numeric(.data$`Trade Covered (USD Million)`)),
    subsidy = suppressWarnings(as.numeric(.data$`Size of Subsidy (USD Million)`))
  )

# ---- 1) Active-policy coverage, per year ------------------------------------
hr("scale field coverage among ACTIVE policies, per year")

years <- seq(2008, as.integer(format(Sys.Date(), "%Y")))
implemented <- d %>% filter(!is.na(.data$impl_date))

active_cov <- lapply(years, function(y) {
  y_start <- as.Date(paste0(y, "-01-01"))
  y_end <- as.Date(paste0(y, "-12-31"))
  act <- implemented %>%
    filter(.data$impl_date <= y_end,
           is.na(.data$removal_date) | .data$removal_date >= y_start)
  tibble::tibble(
    year = y,
    n_active = nrow(act),
    n_trade_covered = sum(!is.na(act$trade)),
    share_trade_covered = if (nrow(act) > 0) mean(!is.na(act$trade)) else NA_real_,
    n_subsidy = sum(!is.na(act$subsidy)),
    share_subsidy = if (nrow(act) > 0) mean(!is.na(act$subsidy)) else NA_real_,
    share_either = if (nrow(act) > 0) mean(!is.na(act$trade) | !is.na(act$subsidy)) else NA_real_,
    share_neither = if (nrow(act) > 0) mean(is.na(act$trade) & is.na(act$subsidy)) else NA_real_
  )
}) %>% bind_rows()

print(as.data.frame(active_cov %>% mutate(across(starts_with("share"), ~ round(.x, 4)))),
      right = FALSE)
utils::write.csv(active_cov, file.path(out_dir, "scale_coverage_by_year.csv"),
                 row.names = FALSE)

# ---- 2) Announcement-year coverage, all policies ----------------------------
hr("scale field coverage by ANNOUNCEMENT year, all policies")

ann_cov <- d %>%
  filter(!is.na(.data$announce_year)) %>%
  group_by(year = .data$announce_year) %>%
  summarise(
    n_policies = dplyr::n(),
    n_trade_covered = sum(!is.na(.data$trade)),
    share_trade_covered = mean(!is.na(.data$trade)),
    n_subsidy = sum(!is.na(.data$subsidy)),
    share_subsidy = mean(!is.na(.data$subsidy)),
    .groups = "drop"
  ) %>%
  arrange(.data$year)

print(as.data.frame(ann_cov %>% mutate(across(starts_with("share"), ~ round(.x, 4)))),
      right = FALSE)
utils::write.csv(ann_cov, file.path(out_dir, "scale_coverage_by_announce_year.csv"),
                 row.names = FALSE)

# ---- 3) Is there a 2023 break? ----------------------------------------------
hr("is there a collection break at 2023?")
cat("The review's premise was that subsidy values are systematic only from 2023\n")
cat("and that the historical extension records none. Testing that directly.\n\n")

pre <- ann_cov %>% filter(.data$year < 2023)
post <- ann_cov %>% filter(.data$year >= 2023)
cat("subsidy coverage, announcement year < 2023 : ",
    round(sum(pre$n_subsidy) / sum(pre$n_policies), 4), "\n", sep = "")
cat("subsidy coverage, announcement year >= 2023: ",
    round(sum(post$n_subsidy) / sum(post$n_policies), 4), "\n", sep = "")
cat("\npeak subsidy coverage year : ",
    ann_cov$year[which.max(ann_cov$share_subsidy)],
    " at ", round(max(ann_cov$share_subsidy), 4), "\n", sep = "")
cat("earliest year with any subsidy value: ",
    min(ann_cov$year[ann_cov$n_subsidy > 0]), "\n", sep = "")
cat("\ntrade-covered range across years: ",
    round(min(ann_cov$share_trade_covered), 4), " to ",
    round(max(ann_cov$share_trade_covered), 4), "\n", sep = "")
cat("subsidy range across years      : ",
    round(min(ann_cov$share_subsidy), 4), " to ",
    round(max(ann_cov$share_subsidy), 4), "\n", sep = "")

message("\nWrote scale_coverage_by_year.csv and scale_coverage_by_announce_year.csv")
