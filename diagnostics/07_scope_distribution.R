# ==============================================================================
# Task 6 - m_scope distribution before and after the reordering
# ------------------------------------------------------------------------------
# Prints the m_scope distribution under both scope_mode values and reports how
# many rows changed value, plus which case_when branches are actually reachable
# on the current export.
#
# Writes diagnostics/scope_distribution.csv
#
# Run from the repo root:
#   Rscript diagnostics/07_scope_distribution.R
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

# Reproduce the three inputs to m_scope exactly as build_policy_base() derives
# them, without paying for the whole pipeline.
inputs <- tibble::tibble(
  is_horizontal = as_bool(raw$`Is Horizontal`),
  policy_level = stringr::str_to_lower(dplyr::coalesce(raw$`Levels of Policy Intervention`, "")),
  has_beneficiary = !is.na(raw$`Firm: Beneficiary`) & nzchar(as.character(raw$`Firm: Beneficiary`))
)

legacy <- dis_m_scope(inputs$is_horizontal, inputs$has_beneficiary,
                      inputs$policy_level, scope_mode = "legacy")
fixed <- dis_m_scope(inputs$is_horizontal, inputs$has_beneficiary,
                     inputs$policy_level, scope_mode = "firm_first")

# ---- 1) Which branches actually fire? ---------------------------------------
hr("branch reachability on the current export")
cat("distinct `Levels of Policy Intervention` values (lowercased):\n")
print(as.data.frame(inputs %>% count(.data$policy_level, sort = TRUE, name = "n")), right = FALSE)

cat("\nrows matched by each regex used in the case_when:\n")
for (re in c("economy|cross|horizontal", "sector|industry", "firm")) {
  n <- sum(stringr::str_detect(inputs$policy_level, re))
  cat(sprintf("  %-28s %7d  %s\n", re, n, if (n == 0) "<- DEAD BRANCH" else ""))
}
cat("\nis_horizontal TRUE : ", sum(inputs$is_horizontal), "\n", sep = "")
cat("has_beneficiary     : ", sum(inputs$has_beneficiary), "\n", sep = "")

# ---- 2) Distribution before and after ---------------------------------------
hr("m_scope distribution, before and after")
dist <- dplyr::full_join(
  tibble::tibble(m_scope = legacy) %>% count(.data$m_scope, name = "n_legacy"),
  tibble::tibble(m_scope = fixed) %>% count(.data$m_scope, name = "n_firm_first"),
  by = "m_scope"
) %>%
  mutate(
    n_legacy = dplyr::coalesce(.data$n_legacy, 0L),
    n_firm_first = dplyr::coalesce(.data$n_firm_first, 0L),
    share_legacy = round(.data$n_legacy / nrow(inputs), 4),
    share_firm_first = round(.data$n_firm_first / nrow(inputs), 4),
    change = .data$n_firm_first - .data$n_legacy
  ) %>%
  arrange(.data$m_scope)

print(as.data.frame(dist), right = FALSE)
utils::write.csv(dist, file.path(out_dir, "scope_distribution.csv"), row.names = FALSE)

# ---- 3) How many rows changed? ----------------------------------------------
hr("rows changed")
changed <- legacy != fixed
cat("rows changing m_scope value: ", sum(changed), " of ", length(changed),
    sprintf(" (%.2f%%)", 100 * mean(changed)), "\n", sep = "")

if (any(changed)) {
  cat("\nthe transition, and what those rows look like:\n")
  print(as.data.frame(
    tibble::tibble(
      from = legacy[changed], to = fixed[changed],
      policy_level = inputs$policy_level[changed],
      has_beneficiary = inputs$has_beneficiary[changed]
    ) %>%
      count(.data$from, .data$to, .data$policy_level, .data$has_beneficiary, name = "n")
  ), right = FALSE)

  cat("\nmean m_scope: legacy ", round(mean(legacy), 4),
      " -> firm_first ", round(mean(fixed), 4),
      "  (", round(100 * (mean(fixed) / mean(legacy) - 1), 2), "%)\n", sep = "")
}

message("\nWrote scope_distribution.csv")
