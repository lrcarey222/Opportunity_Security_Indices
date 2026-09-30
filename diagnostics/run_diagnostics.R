# ==============================================================================
# Regenerate every diagnostic CSV in one call
# ------------------------------------------------------------------------------
# Run from the repo root:
#   Rscript diagnostics/run_diagnostics.R              # all but the baseline
#   Rscript diagnostics/run_diagnostics.R --baseline   # include the baseline
#   Rscript diagnostics/run_diagnostics.R --only 02,07 # just those scripts
#
# 00_nipo_baseline.R is EXCLUDED by default and must be asked for explicitly.
# It writes diagnostics/baseline/, which is the golden file the
# dis_legacy_mode regression test compares against; regenerating it against
# changed code would silently redefine the thing being tested.
#
# Each script is run in a SEPARATE R process. These scripts are memory-hungry
# (see the note on the NIPO pipeline's peak usage in 00_nipo_baseline.R), and a
# fresh process guarantees the previous one's allocation tables are released.
# ==============================================================================

repo_root <- normalizePath(
  if (nzchar(Sys.getenv("OPSI_REPO_ROOT"))) Sys.getenv("OPSI_REPO_ROOT") else getwd(),
  winslash = "/", mustWork = TRUE
)
diag_dir <- file.path(repo_root, "diagnostics")

args <- commandArgs(trailingOnly = TRUE)
include_baseline <- "--baseline" %in% args
only <- NULL
if ("--only" %in% args) {
  idx <- which(args == "--only")
  if (length(args) > idx) only <- trimws(strsplit(args[idx + 1L], ",")[[1]])
}

# script -> the CSVs it produces, for the report at the end.
SCRIPTS <- list(
  "00_nipo_baseline.R"        = c("baseline/by_tech_sc.csv", "baseline/by_tech_sc_year.csv"),
  "01_nipo_field_recon.R"     = c("scale_coverage_by_year.csv"),
  "02_task1_keyword_impact.R" = c("keyword_audit.csv", "keyword_impact.csv"),
  "03_confidence_sensitivity.R" = c("confidence_sensitivity.csv",
                                    "confidence_sensitivity_nocc.csv"),
  "04_geo_impact.R"           = c("geo_impact.csv"),
  "05_pending_and_duration.R" = c("pending_and_duration.csv", "pending_row_counts.csv"),
  "06_scale_coverage.R"       = c("scale_coverage_by_year.csv",
                                  "scale_coverage_by_announce_year.csv"),
  "07_scope_distribution.R"   = c("scope_distribution.csv"),
  "08_crosscutting_and_eu.R"  = c("crosscutting_impact.csv", "eu_split.csv",
                                  "eu_act_replication.csv"),
  "09_attribution_ladder.R"   = c("attribution_ladder.csv", "attribution_summary.csv")
)

to_run <- names(SCRIPTS)
if (!include_baseline) to_run <- setdiff(to_run, "00_nipo_baseline.R")
if (!is.null(only)) {
  to_run <- to_run[vapply(to_run, function(s) any(startsWith(s, only)), logical(1))]
}

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

cat(strrep("=", 78), "\n", sep = "")
cat("Running ", length(to_run), " diagnostic script(s)\n", sep = "")
if (!include_baseline && is.null(only)) {
  cat("(00_nipo_baseline.R skipped; pass --baseline to include it)\n")
}
cat(strrep("=", 78), "\n", sep = "")

results <- list()
for (s in to_run) {
  path <- file.path(diag_dir, s)
  if (!file.exists(path)) {
    cat("\n[SKIP] ", s, " - not found\n", sep = "")
    results[[s]] <- list(status = "missing", seconds = NA_real_)
    next
  }
  cat("\n[RUN ] ", s, "\n", sep = "")
  t0 <- Sys.time()
  # stdout is shown; a non-zero status is recorded and the run continues, so one
  # failure does not hide the rest.
  code <- system2(rscript, args = shQuote(path), stdout = "", stderr = "")
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  results[[s]] <- list(status = if (identical(code, 0L)) "ok" else paste0("FAILED(", code, ")"),
                       seconds = secs)
  cat("[", results[[s]]$status, "] ", s, "  ", round(secs, 1), "s\n", sep = "")
}

cat("\n", strrep("=", 78), "\n", sep = "")
cat("SUMMARY\n")
cat(strrep("=", 78), "\n", sep = "")
for (s in names(results)) {
  cat(sprintf("  %-30s %-12s %8s\n", s, results[[s]]$status,
              if (is.na(results[[s]]$seconds)) "-" else paste0(round(results[[s]]$seconds), "s")))
}

cat("\nExpected outputs:\n")
missing_any <- FALSE
for (s in to_run) {
  for (f in SCRIPTS[[s]]) {
    p <- file.path(diag_dir, f)
    ok <- file.exists(p)
    if (!ok) missing_any <- TRUE
    cat(sprintf("  [%s] %s\n", if (ok) "x" else " ", f))
  }
}

if (missing_any) {
  cat("\nSome expected outputs are missing; see the failures above.\n")
} else {
  cat("\nAll expected outputs present.\n")
}
