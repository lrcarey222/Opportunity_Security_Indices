# ==============================================================================
# Task 0 / Task 9 - NIPO export field reconnaissance
# ------------------------------------------------------------------------------
# Reads the raw NIPO export only (no pipeline), and reports the field facts the
# remediation tasks depend on:
#   - the true motive column names and their coverage        (Task 9)
#   - the values of `Levels of Policy Intervention`          (Task 6, Task 9)
#   - the values of `Initial Assessment` -> status mapping   (Task 7)
#   - scale-field coverage by year                           (Task 5)
#   - `Firm: Beneficiary` / `Firm: Targeted` population      (Task 6)
#
# Writes: diagnostics/scale_coverage_by_year.csv and a console report.
#
# Run from the repo root:
#   Rscript diagnostics/01_nipo_field_recon.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

repo_root <- normalizePath(
  if (nzchar(Sys.getenv("OPSI_REPO_ROOT"))) Sys.getenv("OPSI_REPO_ROOT") else getwd(),
  winslash = "/", mustWork = TRUE
)

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))

raw_data_path <- file.path(repo_root, "data", "raw")
out_dir <- file.path(repo_root, "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - February 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
message("NIPO export: ", basename(nipo_policy_path))

raw <- readxl::read_excel(nipo_policy_path, sheet = 1)

hr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

cat("rows: ", nrow(raw), "  cols: ", ncol(raw), "\n", sep = "")

# ---- 1) Motive fields --------------------------------------------------------
hr("MOTIVE FIELDS (true names + coverage)")
motive_cols <- grep("^Motive:|^Mentions ", names(raw), value = TRUE)
if (length(motive_cols) == 0) {
  cat("NO motive columns present in this export.\n")
} else {
  motive_report <- lapply(motive_cols, function(cn) {
    v <- raw[[cn]]
    tibble::tibble(
      column = cn,
      class = class(v)[1],
      n_non_na = sum(!is.na(v)),
      share_non_na = mean(!is.na(v)),
      distinct_values = paste(utils::head(sort(unique(as.character(v))), 6), collapse = " | ")
    )
  })
  print(as.data.frame(dplyr::bind_rows(motive_report)), right = FALSE)
}

# ---- 2) Policy-layer field ---------------------------------------------------
hr("LEVELS OF POLICY INTERVENTION (Task 6 m_scope / Task 9 nipo_layer)")
if ("Levels of Policy Intervention" %in% names(raw)) {
  lvl <- raw[["Levels of Policy Intervention"]]
  print(as.data.frame(
    tibble::tibble(value = as.character(lvl)) %>%
      count(value, sort = TRUE, name = "n") %>%
      mutate(share = round(n / nrow(raw), 4)) %>%
      head(30)
  ), right = FALSE)
} else {
  cat("column absent\n")
}

# ---- 3) Status field ---------------------------------------------------------
hr("INITIAL ASSESSMENT (Task 7 status weights)")
status_col <- "Initial Assessment (Change Relative to 1 Jan 2009)"
if (status_col %in% names(raw)) {
  print(as.data.frame(
    tibble::tibble(value = as.character(raw[[status_col]])) %>%
      count(value, sort = TRUE, name = "n") %>%
      mutate(share = round(n / nrow(raw), 4))
  ), right = FALSE)
} else {
  cat("column absent\n")
}

# ---- 4) Firm targeting -------------------------------------------------------
hr("FIRM FIELDS (Task 6 - has_beneficiary vs firm-targeting)")
for (cn in c("Firm: Beneficiary", "Firm: Targeted", "Is Horizontal")) {
  if (cn %in% names(raw)) {
    v <- as.character(raw[[cn]])
    cat(sprintf("%-20s populated: %6d  (%.1f%%)\n",
                cn, sum(!is.na(v) & nzchar(v)), 100 * mean(!is.na(v) & nzchar(v))))
  } else {
    cat(sprintf("%-20s ABSENT\n", cn))
  }
}

# ---- 5) Scale coverage by year (Task 5) --------------------------------------
hr("SCALE FIELD COVERAGE BY ANNOUNCEMENT YEAR (Task 5)")
scale_cov <- raw %>%
  transmute(
    year = suppressWarnings(as.integer(format(as.Date(.data$`Announcement Date`), "%Y"))),
    trade_covered = suppressWarnings(as.numeric(.data$`Trade Covered (USD Million)`)),
    subsidy = suppressWarnings(as.numeric(.data$`Size of Subsidy (USD Million)`))
  ) %>%
  filter(!is.na(.data$year)) %>%
  group_by(.data$year) %>%
  summarise(
    n_policies = dplyr::n(),
    n_trade_covered = sum(!is.na(.data$trade_covered)),
    share_trade_covered = mean(!is.na(.data$trade_covered)),
    n_subsidy = sum(!is.na(.data$subsidy)),
    share_subsidy = mean(!is.na(.data$subsidy)),
    .groups = "drop"
  ) %>%
  arrange(.data$year)

print(as.data.frame(scale_cov), right = FALSE)
utils::write.csv(scale_cov, file.path(out_dir, "scale_coverage_by_year.csv"),
                 row.names = FALSE)

# ---- 6) Keyword false-positive demonstration (Task 1) -----------------------
hr("TASK 1 - unanchored keyword false positives on real titles")
ai_hits <- raw %>%
  filter(grepl("ai", tolower(.data$Title))) %>%
  filter(!grepl("\\bai\\b|artificial intelligence", tolower(.data$Title)))
cat("titles containing bare substring 'ai' but no standalone 'ai'/'artificial intelligence': ",
    nrow(ai_hits), " of ", nrow(raw),
    sprintf(" (%.1f%%)", 100 * nrow(ai_hits) / nrow(raw)), "\n", sep = "")
cat("\nexamples:\n")
writeLines(paste0("  - ", utils::head(unique(ai_hits$Title), 12)))

message("\nWrote ", file.path(out_dir, "scale_coverage_by_year.csv"))
