# QC: write the validation report (reports/AIB_VALIDATION_REPORT.md; docs/QC_SYSTEM_DESIGN.md 6.5; R/qc/qc_report.R).
# Reads whatever QC result CSVs exist (the TypeScript bundle layer's and the R pipeline layer's), the review export
# CSVs if present, the case snapshots and the case-audit JSONs, and renders the Markdown report. Reviewer names
# never reach the report: the exports are read without any name column.
#
# Usage:
#   Rscript scripts/qc/write_validation_report.R [--results qc/results] [--exports qc/private/exports]
#     [--audits qc/audits] [--cases dashboard/public/data/review/cases.<campaign>.json]
#     [--out reports/AIB_VALIDATION_REPORT.md] [--date 2026-11-20]
# Paths are relative to the repo root unless absolute. --date defaults to today (UTC); pass it for a
# reproducible report. With no --cases, the first dashboard/public/data/review/cases.*.json found is used.

local({
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  d <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]), winslash = "/")) else getwd()
  while (!file.exists(file.path(d, "R", "qc", "qc_report.R")) && dirname(d) != d) d <- dirname(d)
  assign("qc_repo_root", d, envir = globalenv())
})
source(file.path(qc_repo_root, "R", "qc", "qc_report.R"))

args <- qc_report_cli_args(commandArgs(trailingOnly = TRUE))
rel <- function(p) if (is.null(p)) NULL else if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(qc_repo_root, p)
arg <- function(k, default) if (is.null(args[[k]]) || isTRUE(args[[k]])) default else args[[k]]
results_arg <- arg("results", "qc/results")
exports_arg <- arg("exports", "qc/private/exports")
audits_arg <- arg("audits", "qc/audits")
out_arg <- arg("out", "reports/AIB_VALIDATION_REPORT.md")
date <- arg("date", format(Sys.time(), "%Y-%m-%d", tz = "UTC"))

read_csvs <- function(dir, only = NULL) {
  if (!dir.exists(dir)) return(list())
  f <- sort(list.files(dir, pattern = "\\.csv$"))
  if (!is.null(only)) f <- intersect(f, only)
  out <- lapply(f, function(x) {
    d <- utils::read.csv(file.path(dir, x), stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"), encoding = "UTF-8")
    # never load a name column into memory for the report
    d[, !grepl("(^|_)name$|organi[sz]ation|email", names(d), ignore.case = TRUE) | names(d) %in% c("case_title", "card_title"), drop = FALSE]
  })
  stats::setNames(out, f)
}
results <- read_csvs(rel(results_arg))
exports <- read_csvs(rel(exports_arg), QC_REPORT_EXPORT_FILES)

cases_path <- if (!is.null(args$cases)) rel(args$cases) else {
  cand <- sort(Sys.glob(file.path(qc_repo_root, "dashboard", "public", "data", "review", "cases.*.json")))
  if (length(cand)) cand[1] else NULL
}
cases <- if (!is.null(cases_path) && file.exists(cases_path)) jsonlite::fromJSON(cases_path, simplifyVector = FALSE) else NULL

audit_dir <- rel(audits_arg)
audits <- NULL
if (dir.exists(audit_dir)) {
  # one row per audited cell: the ISO3_Tech_Stage files (CASE_### copies are the same audits)
  af <- sort(list.files(audit_dir, pattern = "^[A-Z]{3}_.*\\.json$"))
  audits <- do.call(rbind, lapply(af, function(f) qc_report_audit_row(jsonlite::fromJSON(file.path(audit_dir, f), simplifyVector = FALSE),
                                                                     sub("\\.json$", "", f))))
}

shown <- function(p) if (is.null(p)) NULL else sub(paste0("^", qc_repo_root, "/?"), "", normalizePath(p, winslash = "/", mustWork = FALSE))
md <- qc_report_build(results, exports, cases, audits,
                      meta = list(date = date, results_dir = shown(rel(results_arg)),
                                  exports_dir = if (length(exports)) shown(rel(exports_arg)) else NULL,
                                  cases_file = if (!is.null(cases)) shown(cases_path) else NULL))
out <- rel(out_arg)
# synthetic demo submissions never reach a report outside qc/private/ (the committed report must hold real reviews only)
n_synthetic <- qc_report_synthetic_rows(exports)
if (n_synthetic > 0) {
  norm <- function(p) tolower(normalizePath(p, winslash = "/", mustWork = FALSE))
  private <- norm(file.path(qc_repo_root, "qc", "private"))
  out_dir <- norm(dirname(out))
  if (!(out_dir == private || startsWith(out_dir, paste0(private, "/")))) {
    stop("the review exports hold ", n_synthetic, " rows of SYNTHETIC demo submissions (SUB-DEMO-..., [synthetic]): ",
         "write this report under qc/private/ (--out qc/private/...), or export real submissions only", call. = FALSE)
  }
}
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
con <- file(out, open = "wb")
writeLines(enc2utf8(md), con, sep = "\n", useBytes = TRUE)
close(con)
message("Wrote ", out, " (", length(md), " lines) from ", length(results), " QC result files, ", length(exports), " review exports and ",
        if (is.null(audits)) 0L else nrow(audits), " audits")
