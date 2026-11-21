# QC: the validation report (reports/AIB_VALIDATION_REPORT.md; docs/QC_SYSTEM_DESIGN.md 6.5) and the small CLI
# helpers the scripts/qc/*.R scripts share.
#
# The report puts three things side by side and never merges them into one score: data quality (the automated
# checks on the pipeline and on the shipped bundles), analytical robustness (the per-cell sensitivity classes of
# both layers) and expert validation (the review exports). Expert results are structured elicitation, not
# representative polling. Reviewers appear only as counts: no name, organisation or reviewer code is printed, and
# any column that could carry a name is dropped before anything is rendered (qc_report_scrub()).
#
# Every input is optional: a result file that does not exist is listed as not run, and with no review exports
# every review section says "No expert reviews recorded yet" while the automated sections still render. Output
# is deterministic for the same inputs and date (rows are sorted; nothing reads the clock).
#
# Pure functions: no IO. scripts/qc/write_validation_report.R reads the QC result CSVs, the review exports, the
# case snapshots and the audit JSON summaries and passes them in as data frames / lists.

QC_REPORT_NO_REVIEWS <- "No expert reviews recorded yet."
QC_REPORT_NEIS <- c("USA", "JPN", "KOR", "AUS", "IND", "DEU")
# The result files the report knows (layer, what they are); anything else found is listed in the scope.
QC_REPORT_RESULT_FILES <- c(
  bundle_qc_checks.csv = "bundle: automated checks on the shipped bundles",
  choice_reproduction.csv = "bundle: the choice rule re-run from the shipped decision trace",
  qc_sensitivity_bundle.csv = "bundle: spec flips and the non-stable cells' robustness classes from the shipped trace",
  robustness_bundle_summary.csv = "bundle: robustness class counts",
  qc_golden_case_status.csv = "bundle: golden cases on the shipped bundles",
  qc_sensitivity_pipeline.csv = "pipeline: spec flips and the fragile / sensitive cells' classes (canonical choice rule)",
  pipeline_cell_classes.csv = "pipeline: every cell's robustness class",
  pipeline_robustness_summary.csv = "pipeline: robustness class counts, spec counts, reproduction checks",
  pipeline_data_qc_checks.csv = "pipeline: data QC checks",
  pipeline_trade_production_flags.csv = "pipeline: trade vs production plausibility flags",
  pipeline_drift_transitions.csv = "pipeline: release-to-release choice transitions",
  pipeline_drift_cells.csv = "pipeline: release-to-release cell changes",
  pipeline_golden_case_status.csv = "pipeline: golden cases on the processed tables"
)
QC_REPORT_EXPORT_FILES <- c("qc_review_responses.csv", "qc_case_results.csv", "qc_method_feedback.csv", "qc_data_issues.csv",
                            "qc_framework_answers.csv", "qc_decision_use.csv", "qc_threat_reviews.csv", "qc_priority_fixes.csv")
# Columns never rendered (identity): names, organisations, contact details and the pseudonymous codes.
QC_REPORT_PRIVATE_COLS <- "(^|_)(name|organization|organisation|email|code|id)$"
QC_REPORT_PRIVATE_KEEP <- c("case_id", "card_id", "scenario_id", "submission_id", "check_id", "group_id", "metric_id", "campaign_id")
QC_REPORT_CLOSED <- c("resolved", "rejected", "duplicate", "wont_fix")

# ---- CLI helpers ------------------------------------------------------------------------------------------------
# Parse "--name value" / "--name=value" / "--flag" arguments into a named list (a flag is TRUE).
qc_report_cli_args <- function(args) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    if (!startsWith(a, "--")) stop("unexpected argument: ", a, call. = FALSE)
    a <- sub("^--", "", a)
    if (grepl("=", a, fixed = TRUE)) {
      out[[sub("=.*$", "", a)]] <- sub("^[^=]*=", "", a)
    } else if (i < length(args) && !startsWith(args[i + 1], "--")) {
      out[[a]] <- args[i + 1]
      i <- i + 1
    } else {
      out[[a]] <- TRUE
    }
    i <- i + 1
  }
  out
}

# The processed directory a parsed config/config.yml (or config.example.yml) names, data/processed by default.
qc_report_processed_default <- function(conf) {
  p <- if (is.list(conf)) conf$processed_dir else NULL
  if (is.null(p) || !nzchar(p)) "data/processed" else p
}

# ---- small helpers ----------------------------------------------------------------------------------------------
qc_report_has <- function(df) is.data.frame(df) && nrow(df) > 0
# A column by name (NA where absent), so differently-shaped inputs never error.
qc_report_col <- function(df, nm, default = NA) {
  if (is.data.frame(df) && nm %in% names(df)) df[[nm]] else rep(default, if (is.data.frame(df)) nrow(df) else 0L)
}
# The first of several candidate columns that exists (NULL if none).
qc_report_pick <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[1] else NULL
}
qc_report_true <- function(x) tolower(as.character(x)) %in% c("true", "t", "1", "yes")
qc_report_num <- function(x) suppressWarnings(as.numeric(x))
qc_report_pct <- function(x) ifelse(is.na(x), "n/a", paste0(round(100 * x), "%"))
qc_report_or <- function(a, b) if (is.null(a)) b else a

# Drop every column that could identify a reviewer (names, organisations, e-mail, reviewer codes and ids, the
# adjudicator); the analytical ids (case_id, card_id, ...) are kept.
qc_report_scrub <- function(df) {
  if (!is.data.frame(df)) return(df)
  drop <- grepl(QC_REPORT_PRIVATE_COLS, names(df), ignore.case = TRUE) & !names(df) %in% QC_REPORT_PRIVATE_KEEP
  drop <- drop | grepl("reviewer|decided_by|adjudicated_by", names(df), ignore.case = TRUE)
  df[, !drop, drop = FALSE]
}

# "a 3, b 2" (most frequent first, ties alphabetical); "none" when empty.
qc_report_count <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return("none")
  t <- table(x)
  t <- t[order(-as.integer(t), names(t))]
  paste0(names(t), " ", as.integer(t), collapse = ", ")
}

qc_report_md_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  gsub("\\|", "\\\\|", gsub("[\r\n]+", " ", x))
}

# A Markdown table (private columns scrubbed first); "_(none)_" when empty.
qc_report_md_table <- function(df, max_rows = 30) {
  df <- qc_report_scrub(df)
  if (!qc_report_has(df)) return("_(none)_")
  more <- nrow(df) - max_rows
  df <- utils::head(df, max_rows)
  num <- function(v) vapply(v, function(z) if (is.na(z)) NA_character_ else if (z == round(z)) format(z, scientific = FALSE, trim = TRUE)
                            else format(signif(z, 4), scientific = FALSE, trim = TRUE), character(1))
  df[] <- lapply(df, function(v) if (is.numeric(v)) num(v) else v)
  h <- paste0("| ", paste(names(df), collapse = " | "), " |")
  s <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  b <- vapply(seq_len(nrow(df)), function(i) paste0("| ", paste(qc_report_md_escape(vapply(df[i, , drop = FALSE], as.character, character(1))),
                                                                 collapse = " | "), " |"), character(1))
  c(h, s, b, if (more > 0) c("", paste0("_", more, " more rows in the source file._")) else NULL)
}

qc_report_trunc <- function(x, n = 160) {
  x <- ifelse(is.na(x), "", gsub("[\r\n]+", " ", as.character(x)))
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
}

# The case list of a cases.<campaign>.json (list with $cases, or the array itself) as a data frame: case_id, kind,
# title, anchor, sector_groups (";"), country_groups (";"), stage.
qc_report_cases_df <- function(cases) {
  if (is.null(cases)) return(NULL)
  cl <- if (is.list(cases) && !is.null(cases$cases)) cases$cases else cases
  if (is.data.frame(cl)) cl <- lapply(seq_len(nrow(cl)), function(i) as.list(cl[i, , drop = FALSE]))
  if (!length(cl)) return(NULL)
  g <- function(x, k) {
    v <- x[[k]]
    if (is.null(v)) return(NA_character_)
    v <- unlist(v)
    if (!length(v)) NA_character_ else paste(as.character(v), collapse = ";")
  }
  do.call(rbind, lapply(cl, function(x) data.frame(case_id = g(x, "case_id"), kind = g(x, "kind"), title = g(x, "title"),
                                                   anchor = g(x, "anchor"), sector_groups = g(x, "sector_groups"),
                                                   country_groups = g(x, "country_groups"), stage = g(x, "stage"), stringsAsFactors = FALSE)))
}

qc_report_any_review <- function(exports) any(vapply(exports, qc_report_has, logical(1)))
qc_report_case_rows <- function(exports) {
  cr <- exports[["qc_case_results.csv"]]
  if (!qc_report_has(cr) || sum(qc_report_num(qc_report_col(cr, "n_reviews")) > 0, na.rm = TRUE) == 0) NULL else cr
}

# ---- sections ---------------------------------------------------------------------------------------------------

qc_report_scope <- function(results, exports, audits, meta) {
  rel <- sort(unique(stats::na.omit(as.character(unlist(lapply(c(results, exports), function(d) qc_report_col(d, "data_release")))))))
  rel <- rel[nzchar(rel)]
  known <- names(QC_REPORT_RESULT_FILES)
  found <- names(results)
  extra <- sort(setdiff(found, known))
  tab <- data.frame(file = c(known, extra), what = c(unname(QC_REPORT_RESULT_FILES), rep("other QC result", length(extra))),
                    stringsAsFactors = FALSE)
  tab$rows <- vapply(tab$file, function(f) if (f %in% found) as.character(nrow(results[[f]])) else "not run", character(1))
  ex <- data.frame(file = QC_REPORT_EXPORT_FILES, rows = vapply(QC_REPORT_EXPORT_FILES, function(f)
    if (f %in% names(exports)) as.character(nrow(exports[[f]])) else "absent", character(1)), stringsAsFactors = FALSE)
  c("## 1. Scope", "",
    paste0("Report date: ", qc_report_or(meta$date, "n/a"), ". Generated by scripts/qc/write_validation_report.R from ",
           qc_report_or(meta$results_dir, "qc/results"),
           if (!is.null(meta$exports_dir)) paste0(" and the review exports in ", meta$exports_dir) else "",
           if (!is.null(meta$cases_file)) paste0(", with the case snapshots in ", meta$cases_file) else "", "."),
    "",
    paste0("Data releases described by these results: ", if (length(rel)) paste0("`", rel, "`", collapse = ", ") else "none recorded", ". ",
           "`pipeline-*` identifies the processed strategic_position table the R layer read; `rel-*` the shipped-bundle release the ",
           "Explorer and the reviewers saw. The two layers are reported separately and are not one release."),
    "",
    paste0("What was checked: the shipped dashboard bundles (what users and reviewers see), the processed pipeline tables they are ",
           "exported from, end-to-end case audits, and the expert review exports. Data quality, analytical robustness and expert ",
           "validation are reported as three separate things; nothing here is a single validation score."),
    "",
    "**QC result files**", "", qc_report_md_table(tab, 50), "",
    "**Review exports**", "", qc_report_md_table(ex, 20), "",
    paste0("**Case audits**: ", if (qc_report_has(audits)) paste0(nrow(audits), " end-to-end audits in qc/audits.") else "none found."), "")
}

qc_report_reviewers <- function(exports, cases_df, codes) {
  out <- c("## 2. Reviewers", "")
  if (!qc_report_any_review(exports)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  out <- c(out, paste0(if (is.na(codes)) "An unknown number of" else codes, " reviewers contributed at least one submission. Reviewers are ",
                       "counted, never named; these are structured expert elicitations, not a representative poll, and no share below is ",
                       "an estimate of expert opinion at large."), "")
  rr <- exports[["qc_review_responses.csv"]]
  if (qc_report_has(rr)) {
    out <- c(out, paste0("- Case reviews by expertise match (reviews): ", qc_report_count(qc_report_col(rr, "expertise_match")), "."))
    if (qc_report_has(cases_df)) {
      j <- merge(data.frame(sub = as.character(qc_report_col(rr, "submission_id")), case_id = as.character(qc_report_col(rr, "case_id")),
                            match = as.character(qc_report_col(rr, "expertise_match")), stringsAsFactors = FALSE),
                 cases_df[, c("case_id", "sector_groups")], by = "case_id")
      j <- j[j$match %in% "in" & !is.na(j$sector_groups), , drop = FALSE]
      if (nrow(j)) {
        tags <- unlist(strsplit(j$sector_groups, ";", fixed = TRUE))
        out <- c(out, paste0("- In-expertise case reviews by the case's sector tag: ", qc_report_count(tags), "."))
      }
    }
  }
  for (f in c("qc_method_feedback.csv", "qc_framework_answers.csv", "qc_data_issues.csv", "qc_decision_use.csv", "qc_threat_reviews.csv")) {
    d <- exports[[f]]
    if (qc_report_has(d)) out <- c(out, paste0("- ", sub("\\.csv$", "", sub("^qc_", "", f)), ": ", nrow(d), " submissions"))
  }
  c(out, "")
}

qc_report_coverage <- function(exports, cases_df) {
  cr <- qc_report_case_rows(exports)
  out <- c("## 3. Coverage", "")
  if (is.null(cr)) {
    out <- c(out, QC_REPORT_NO_REVIEWS)
    if (qc_report_has(cases_df)) out <- c(out, "", paste0("Cases in the campaign: ", nrow(cases_df), " (", qc_report_count(cases_df$kind),
                                                          "); anchors: ", sum(qc_report_true(cases_df$anchor)), "."))
    return(c(out, ""))
  }
  n <- qc_report_num(qc_report_col(cr, "n_reviews"))
  few <- qc_report_true(qc_report_col(cr, "too_few_reviews"))
  anc <- qc_report_true(qc_report_col(cr, "anchor"))
  out <- c(out, paste0(sum(n > 0, na.rm = TRUE), " of ", nrow(cr), " cases have at least one review; ", sum(!few & n > 0, na.rm = TRUE),
                       " have enough reviews to summarise (3 or more). Anchor cases: ", sum(anc), ", of which ", sum(anc & !few & n > 0, na.rm = TRUE),
                       " have enough reviews."), "")
  if (qc_report_has(cases_df)) {
    miss <- setdiff(cases_df$case_id, as.character(cr$case_id[n > 0]))
    out <- c(out, paste0("Campaign cases with no review yet: ", if (length(miss)) paste(sort(miss), collapse = ", ") else "none", "."), "")
  }
  tab <- data.frame(case_id = qc_report_col(cr, "case_id"), kind = qc_report_col(cr, "case_kind"), anchor = qc_report_col(cr, "anchor"),
                    n_reviews = n, n_interpretable = qc_report_col(cr, "n_interpretable"), n_dont_know = qc_report_col(cr, "n_dont_know"),
                    summary = qc_report_col(cr, "summary_label"), stringsAsFactors = FALSE)
  c(out, qc_report_md_table(tab[order(tab$case_id), , drop = FALSE], 60), "")
}

# Rows of every check table (check_id + status) with one of the statuses, fail first.
qc_report_check_rows <- function(results, statuses) {
  rows <- lapply(sort(names(results)), function(f) {
    d <- results[[f]]
    if (!qc_report_has(d) || !all(c("check_id", "status") %in% names(d))) return(NULL)
    d <- d[as.character(d$status) %in% statuses, , drop = FALSE]
    if (!nrow(d)) return(NULL)
    data.frame(file = f, check_id = d$check_id, table = qc_report_col(d, "table"), status = d$status, n = qc_report_col(d, "n"),
               human = qc_report_col(d, "needs_human_explanation"), detail = qc_report_trunc(qc_report_col(d, "detail"), 220),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(NULL)
  out[order(out$status != "fail", out$file, out$check_id), , drop = FALSE]
}

# A tolerant summary of choice_reproduction.csv: the column that says whether a cell reproduced.
qc_report_reproduction <- function(d) {
  if (!qc_report_has(d)) return(NULL)
  col <- qc_report_pick(d, c("reproduced", "match", "matches", "status", "result"))
  if (is.null(col)) return(list(n = nrow(d), bad = NA_integer_, label = "no reproduction column recognised"))
  v <- tolower(as.character(d[[col]]))
  ok <- v %in% c("true", "t", "1", "yes", "reproduced", "match", "same", "pass", "ok", "info")
  list(n = nrow(d), bad = sum(!ok), label = paste0("column ", col, ": ", qc_report_count(d[[col]])))
}

# Golden-case status rows (status / result column): pass vs anything else.
qc_report_golden <- function(d) {
  if (!qc_report_has(d)) return(NULL)
  col <- qc_report_pick(d, c("status", "result", "outcome"))
  v <- if (is.null(col)) rep(NA_character_, nrow(d)) else tolower(as.character(d[[col]]))
  idc <- qc_report_pick(d, c("id", "golden_id", "gc_id", "case_id"))
  bad <- !v %in% c("pass", "passed", "ok", "info")
  # a case may be evaluated against several tables (one row each): list each id once, failures apart from cases the
  # table cannot evaluate (not_applicable / missing)
  na_like <- v %in% c("not_applicable", "missing", "skipped")
  ids <- function(i) if (!is.null(idc) && any(i)) sort(unique(as.character(d[[idc]][i]))) else character()
  list(n = nrow(d), pass = sum(!bad), counts = qc_report_count(v), fails = ids(bad & !na_like), not_evaluable = ids(na_like))
}

qc_report_findings <- function(results, exports, audits) {
  head <- c("## 4. Findings", "", "### Automated QC", "")
  out <- character()
  pr <- results[["pipeline_robustness_summary.csv"]]
  if (qc_report_has(pr)) {
    rp <- pr[pr$group_type %in% "reproduction", , drop = FALSE]
    if (nrow(rp)) {
      out <- c(out, "Pipeline reproduction (the stored table re-derived from its own columns with the canonical pure functions):", "",
               qc_report_md_table(data.frame(check = rp$group, cells = rp$n_cells, mismatches = rp$n_changed, status = rp$status,
                                             examples = qc_report_trunc(rp$note, 160), stringsAsFactors = FALSE)), "")
    }
  }
  rep <- qc_report_reproduction(results[["choice_reproduction.csv"]])
  if (!is.null(rep)) out <- c(out, paste0("Shipped-bundle choice reproduction: ", rep$n, " rows, ", rep$bad, " not reproduced (", rep$label, ")."), "")
  for (f in c("qc_golden_case_status.csv", "pipeline_golden_case_status.csv")) {
    g <- qc_report_golden(results[[f]])
    if (!is.null(g)) out <- c(out, paste0("Golden cases (", f, "): ", g$n, " evaluations, ", g$pass, " pass; statuses ", g$counts,
                                          if (length(g$fails)) paste0("; failing: ", paste(g$fails, collapse = ", ")) else "",
                                          if (length(g$not_evaluable)) paste0("; not evaluable on some table: ", paste(g$not_evaluable, collapse = ", ")) else "",
                                          "."), "")
  }
  allchk <- qc_report_check_rows(results, c("fail", "warn", "info"))
  if (!is.null(allchk)) {
    checks <- allchk[allchk$status %in% c("fail", "warn"), , drop = FALSE]
    out <- c(out, paste0("Check tables: ", nrow(allchk), " checks; status ", qc_report_count(allchk$status), "."), "")
    if (nrow(checks)) out <- c(out, "Failing and warning checks (fail first):", "", qc_report_md_table(checks, 40), "")
  }
  if (qc_report_has(audits)) {
    out <- c(out, paste0("End-to-end case audits (qc/audits): every link from source to recommendation is re-computed where the stored ",
                         "data allow and otherwise marked not reproducible."), "",
             qc_report_md_table(audits[order(audits$stem), c("stem", "choice_pipeline", "choice_bundle", "reproduced", "differs",
                                                            "not_reproducible", "provenance_gaps", "bundle_differences"), drop = FALSE], 40), "")
    d <- audits[!is.na(audits$gaps_differs) & nzchar(audits$gaps_differs), , drop = FALSE]
    if (nrow(d)) out <- c(out, "Audit links that differ:", "", paste0("- ", d$stem, ": ", qc_report_trunc(d$gaps_differs, 400)), "")
  }
  if (!length(out)) out <- c("No automated QC results found.", "")
  out <- c(head, out, "### Expert review", "")
  pf <- exports[["qc_priority_fixes.csv"]]
  if (!qc_report_has(pf)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  pf <- pf[order(-qc_report_num(qc_report_col(pf, "score")), as.character(qc_report_col(pf, "group_id"))), , drop = FALSE]
  c(out, "Priority = materiality x confidence the issue is real x breadth (design 6.6); frequency alone never ranks an issue.", "",
    qc_report_md_table(data.frame(rank = seq_len(nrow(pf)), title = qc_report_trunc(qc_report_col(pf, "title"), 120),
                                  issue_type = qc_report_col(pf, "issue_type"), severity = qc_report_col(pf, "severity"),
                                  score = qc_report_col(pf, "score"), reports = qc_report_col(pf, "n_reports"),
                                  status = qc_report_col(pf, "adjudication_status"), case = qc_report_col(pf, "case_id"),
                                  stringsAsFactors = FALSE), 15), "")
}

qc_report_validations <- function(results, exports, audits) {
  out <- c("## 5. Strongest validations", "")
  pr <- results[["pipeline_robustness_summary.csv"]]
  auto <- character()
  if (qc_report_has(pr)) {
    rp <- pr[pr$group_type %in% "reproduction" & pr$status %in% "reproduced", , drop = FALSE]
    if (nrow(rp)) auto <- c(auto, paste0("- Pipeline: ", paste0(rp$group, " (", rp$n_cells, " cells)", collapse = ", "),
                                         " reproduce exactly from the stored inputs."))
  }
  if (qc_report_has(audits)) {
    same <- sum(!is.na(audits$choice_bundle) & audits$choice_pipeline == audits$choice_bundle)
    auto <- c(auto, paste0("- Audits: the pipeline choice equals the shipped bundle's in ", same, " of ", nrow(audits), " audited cells; ",
                           sum(qc_report_num(audits$differs) == 0, na.rm = TRUE), " audits have no differing link."))
  }
  out <- c(out, "Automated (these show the outputs are reproducible, not that they are right):", "",
           if (length(auto)) auto else "- none recorded", "", "Expert review:", "")
  cr <- qc_report_case_rows(exports)
  if (is.null(cr)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  ok <- !qc_report_true(qc_report_col(cr, "too_few_reviews")) & qc_report_num(qc_report_col(cr, "share_matching_model")) >= 0.75 &
    qc_report_num(qc_report_col(cr, "share_rejecting")) %in% 0
  v <- cr[ok %in% TRUE, , drop = FALSE]
  if (!nrow(v)) return(c(out, "No case with at least 3 reviews has 75% or more blind answers matching the Explorer and no rejecting verdict.", ""))
  v <- v[order(as.character(v$case_id)), , drop = FALSE]
  c(out, "Cases with at least 3 reviews where 75% or more of the blind answers match the Explorer and no reviewer rejected it:", "",
    qc_report_md_table(data.frame(case_id = v$case_id, title = qc_report_col(v, "case_title"), model = qc_report_col(v, "model_label"),
                                  n = qc_report_col(v, "n_interpretable"), matching = qc_report_pct(qc_report_num(qc_report_col(v, "share_matching_model"))),
                                  stringsAsFactors = FALSE)), "")
}

qc_report_disagreements <- function(exports) {
  out <- c("## 6. Unresolved disagreements", "")
  cr <- qc_report_case_rows(exports)
  if (is.null(cr)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  enough <- !qc_report_true(qc_report_col(cr, "too_few_reviews"))
  rej <- qc_report_num(qc_report_col(cr, "share_rejecting"))
  mb <- as.character(qc_report_col(cr, "modal_blind"))
  modal_off <- !is.na(mb) & nzchar(mb) & mb != as.character(qc_report_col(cr, "model_choice"))
  d <- cr[enough & ((rej >= 1 / 3) %in% TRUE | modal_off), , drop = FALSE]
  out <- c(out, paste0("Cases with at least 3 reviews where a third or more of reviewers rejected the conclusion (verdicts 4-6) or the most ",
                       "common blind answer differs from the Explorer. Disagreement is evidence to investigate, not an instruction to change the model."), "")
  if (!nrow(d)) out <- c(out, "None among the cases with enough reviews.", "") else {
    d <- d[order(-qc_report_num(qc_report_col(d, "share_rejecting")), as.character(d$case_id)), , drop = FALSE]
    out <- c(out, qc_report_md_table(data.frame(case_id = d$case_id, model = qc_report_col(d, "model_label"), modal_blind = qc_report_col(d, "modal_blind"),
                                                rejecting = qc_report_pct(qc_report_num(qc_report_col(d, "share_rejecting"))),
                                                persuaded = qc_report_pct(qc_report_num(qc_report_col(d, "share_persuaded"))),
                                                issue_types = qc_report_col(d, "issue_types"), suggested = qc_report_col(d, "suggested_choices"),
                                                stringsAsFactors = FALSE)), "")
  }
  few <- cr[!enough & qc_report_num(qc_report_col(cr, "n_reviews")) > 0, , drop = FALSE]
  if (nrow(few)) out <- c(out, paste0("Too few reviews to summarise (fewer than 3): ", paste(sort(as.character(few$case_id)), collapse = ", "), "."), "")
  out
}

qc_report_data_issues <- function(results, exports) {
  out <- c("## 7. Critical data issues", "", "### Automated", "")
  fails <- qc_report_check_rows(results, "fail")
  out <- c(out, if (is.null(fails)) "No automated check failed." else qc_report_md_table(fails, 30), "")
  tpf <- results[["pipeline_trade_production_flags.csv"]]
  if (qc_report_has(tpf)) {
    sev <- qc_report_pick(tpf, c("severity", "status", "flag"))
    out <- c(out, paste0("Trade vs production flags: ", nrow(tpf), " rows", if (!is.null(sev)) paste0("; ", sev, " ", qc_report_count(tpf[[sev]])) else "", "."), "")
  }
  out <- c(out, "### Reported by reviewers", "")
  di <- exports[["qc_data_issues.csv"]]
  if (!qc_report_has(di)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  sev <- tolower(as.character(qc_report_col(di, "severity")))
  st <- tolower(as.character(qc_report_col(di, "adjudication_status")))
  st[is.na(st) | !nzchar(st)] <- "open"
  d <- di[sev %in% c("critical", "material") & !st %in% QC_REPORT_CLOSED, , drop = FALSE]
  out <- c(out, paste0(nrow(di), " data-issue and break-the-model reports; severity ", qc_report_count(sev), "; adjudication ", qc_report_count(st), "."), "")
  if (!nrow(d)) return(c(out, "No open critical or material data issue.", ""))
  d <- d[order(-qc_report_num(qc_report_col(d, "priority_score")), as.character(qc_report_col(d, "submission_id"))), , drop = FALSE]
  c(out, "Open critical and material issues (highest priority first):", "",
    qc_report_md_table(data.frame(severity = qc_report_col(d, "severity"), status = qc_report_col(d, "adjudication_status"),
                                  where = paste(qc_report_col(d, "metric_country"), qc_report_col(d, "metric_tech"), qc_report_col(d, "metric_stage")),
                                  metric = qc_report_col(d, "metric_label"), what_is_wrong = qc_report_trunc(qc_report_col(d, "what_is_wrong"), 200),
                                  priority = qc_report_col(d, "priority_score"), stringsAsFactors = FALSE), 25), "")
}

qc_report_robustness <- function(results, neis = QC_REPORT_NEIS) {
  out <- c("## 8. Sensitive and fragile conclusions", "",
           paste0("Fragile: a local spec (a decision threshold moved by the borderline band, 0.05, one at a time, or the strict reading of ",
                  "Competitive Position >= 0.75) changes the choice. Sensitive: only a structural alternative does. A fence change (yard ",
                  "change) is more consequential than a change of mode inside a fence. Robustness is not data quality and not expert agreement."), "")
  sp <- results[["qc_sensitivity_pipeline.csv"]]
  pr <- results[["pipeline_robustness_summary.csv"]]
  cols <- c("group", "n_cells", "stable", "sensitive", "fragile", "not_testable", "fence_change")
  if (qc_report_has(pr)) {
    o <- pr[pr$group_type %in% "overall", , drop = FALSE]
    if (nrow(o)) out <- c(out, paste0("**Pipeline layer** (", o$data_release[1], "): ", o$n_cells[1], " cells: ", o$stable[1], " stable, ", o$sensitive[1],
                                      " sensitive, ", o$fragile[1], " fragile, ", o$not_testable[1], " not testable; ", o$fence_change[1],
                                      " of the fragile or sensitive cells can change fence."), "")
    ch <- pr[pr$group_type %in% "choice", intersect(cols, names(pr)), drop = FALSE]
    if (nrow(ch)) out <- c(out, "By choice:", "", qc_report_md_table(stats::setNames(ch, c("choice", names(ch)[-1]))), "")
    ne <- pr[pr$group_type %in% "neis_economy", intersect(cols, names(pr)), drop = FALSE]
    if (nrow(ne)) out <- c(out, "NEIS-central economies:", "", qc_report_md_table(stats::setNames(ne, c("economy", names(ne)[-1]))), "")
    spc <- pr[pr$group_type %in% "spec", , drop = FALSE]
    if (nrow(spc)) out <- c(out, "Cells each spec changes (against its comparator):", "",
                            qc_report_md_table(data.frame(spec = spc$group, kind = spc$spec_kind, status = spc$status, changed = spc$n_changed,
                                                          fence_changed = spc$n_fence_changed, note = qc_report_trunc(spc$note, 140),
                                                          stringsAsFactors = FALSE), 40), "")
    art <- pr[pr$group_type %in% "diagnostic", , drop = FALSE]
    if (nrow(art)) out <- c(out, paste0("- ", art$group, ": ", art$n_changed, " cells. ", qc_report_trunc(art$note, 900)), "")
  }
  if (qc_report_has(sp)) {
    cc <- sp[sp$row_type %in% "cell_class", , drop = FALSE]
    f <- cc[cc$iso3 %in% neis & cc$class %in% c("fragile", "sensitive"), , drop = FALSE]
    f <- f[order(f$class != "fragile", match(f$iso3, neis), f$tech, f$stage), , drop = FALSE]
    if (nrow(f)) out <- c(out, "Fragile and sensitive cells in the NEIS-central economies (spec:alternative choice):", "",
                          qc_report_md_table(data.frame(cell = f$case_key, choice = f$baseline_choice, class = f$class,
                                                        local = qc_report_col(f, "local_flips"), structural = qc_report_col(f, "structural_flips"),
                                                        fence_change = f$fence_change, drivers = f$drivers, stringsAsFactors = FALSE), 120), "")
  } else if (!qc_report_has(pr)) out <- c(out, "Pipeline layer: not run (qc_sensitivity_pipeline.csv absent).", "")
  bs <- results[["robustness_bundle_summary.csv"]]
  sb <- results[["qc_sensitivity_bundle.csv"]]
  if (qc_report_has(sb) && "class" %in% names(sb)) {
    rows <- if ("row_type" %in% names(sb)) sb[sb$row_type %in% c("cell_class", "cell"), , drop = FALSE] else sb
    if (!nrow(rows)) rows <- sb
    out <- c(out, paste0("**Bundle layer** (qc_sensitivity_bundle.csv): class rows for ", nrow(rows), " non-stable cells and nodes; class ",
                         qc_report_count(rows$class), ". Stable cells are counted in robustness_bundle_summary.csv."), "")
  }
  if (qc_report_has(bs)) out <- c(out, "**Bundle layer** (robustness_bundle_summary.csv):", "", qc_report_md_table(bs, 25), "")
  if (!qc_report_has(sb) && !qc_report_has(bs)) out <- c(out, "Bundle layer: not run (qc_sensitivity_bundle.csv absent).", "")
  out
}

qc_report_method <- function(results, exports) {
  out <- c("## 9. Methodological issues", "", "### From the automated runs", "")
  pr <- results[["pipeline_robustness_summary.csv"]]
  auto <- character()
  if (qc_report_has(pr)) {
    note <- dplyr::coalesce(as.character(pr$note), "")
    sk <- pr[pr$group_type %in% "spec" & pr$status %in% "skipped", , drop = FALSE]
    if (nrow(sk)) auto <- c(auto, paste0("- Spec not run: ", sk$group, " (", sk$note, ")"))
    du <- pr[pr$group_type %in% "spec" & grepl("same choices as the local spec", note), , drop = FALSE]
    if (nrow(du)) auto <- c(auto, paste0("- ", du$group, ": ", du$note, "."))
    art <- pr[pr$group_type %in% "diagnostic" & qc_report_num(pr$n_changed) > 0, , drop = FALSE]
    if (nrow(art)) auto <- c(auto, paste0("- The engine's own threshold sensitivity (sp_choice_threshold_sensitivity) ignores the need-floor cap: ",
                                          art$n_changed, " cells flip at need 0.60 only because a capped need is stored as need - band - 0.01 ",
                                          "(or would be capped at 0.60). Its reported counts include these artifacts."))
    mm <- pr[pr$group_type %in% "reproduction" & pr$status %in% "mismatch", , drop = FALSE]
    if (nrow(mm)) auto <- c(auto, paste0("- Reproduction mismatch: ", mm$group, " (", mm$n_changed, " cells; ", qc_report_trunc(mm$note, 200), ")"))
  }
  out <- c(out, if (length(auto)) auto else "- none recorded", "", "### From expert review", "")
  mf <- exports[["qc_method_feedback.csv"]]
  fa <- exports[["qc_framework_answers.csv"]]
  if (!qc_report_has(mf) && !qc_report_has(fa)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  if (qc_report_has(mf)) {
    ids <- sort(unique(as.character(qc_report_col(mf, "card_id"))))
    tab <- do.call(rbind, lapply(ids, function(id) {
      d <- mf[as.character(mf$card_id) == id, , drop = FALSE]
      st <- tolower(as.character(qc_report_col(d, "stance")))
      m <- qc_report_num(qc_report_col(d, "materiality"))
      data.frame(card_id = id, title = qc_report_trunc(qc_report_col(d, "card_title")[1], 80), n = nrow(d), retain = sum(st == "retain"),
                 modify = sum(st == "modify"), reject = sum(st == "reject"), unsure = sum(st == "unsure"),
                 mean_materiality = if (all(is.na(m))) NA_real_ else round(mean(m, na.rm = TRUE), 2),
                 bias = qc_report_count(qc_report_col(d, "bias_direction")), stringsAsFactors = FALSE)
    }))
    tab <- tab[order(-(tab$modify + tab$reject) / pmax(tab$n, 1), tab$card_id), , drop = FALSE]
    out <- c(out, "Methodology cards (most contested first):", "", qc_report_md_table(tab, 40), "")
  }
  if (qc_report_has(fa)) {
    ids <- sort(unique(as.character(qc_report_col(fa, "scenario_id"))))
    tab <- do.call(rbind, lapply(ids, function(id) {
      d <- fa[as.character(fa$scenario_id) == id, , drop = FALSE]
      cor <- qc_report_col(d, "correct")
      mm <- qc_report_true(qc_report_col(d, "mismatch")) | (!is.na(cor) & nzchar(as.character(cor)) & !qc_report_true(cor))
      terms <- unlist(strsplit(as.character(stats::na.omit(qc_report_col(d, "confusing_terms"))), ";", fixed = TRUE))
      data.frame(scenario_id = id, concept = qc_report_col(d, "concept")[1], n = nrow(d), mismatches = sum(mm),
                 confusing_terms = qc_report_count(terms), stringsAsFactors = FALSE)
    }))
    tab <- tab[order(-tab$mismatches / pmax(tab$n, 1), tab$scenario_id), , drop = FALSE]
    out <- c(out, "Framework comprehension (a mismatch is framework ambiguity, not reviewer error):", "", qc_report_md_table(tab, 40), "")
  }
  out
}

qc_report_changes <- function(exports) {
  out <- c("## 10. Changes made because of peer review", "")
  if (!qc_report_any_review(exports)) return(c(out, QC_REPORT_NO_REVIEWS, ""))
  di <- exports[["qc_data_issues.csv"]]
  rc <- as.character(qc_report_col(di, "resolution_commit"))
  d <- if (qc_report_has(di)) di[!is.na(rc) & nzchar(rc), , drop = FALSE] else NULL
  if (!qc_report_has(d)) return(c(out, "No change has been recorded: no adjudicated issue carries a resolution_commit.", ""))
  d <- d[order(as.character(d$resolution_commit), as.character(qc_report_col(d, "submission_id"))), , drop = FALSE]
  c(out, "Adjudicated issues resolved by a commit (reviewer submissions are never edited; the adjudication records the change):", "",
    qc_report_md_table(data.frame(resolution_commit = d$resolution_commit, status = qc_report_col(d, "adjudication_status"),
                                  issue_type = qc_report_col(d, "adjudication_issue_type"),
                                  where = paste(qc_report_col(d, "metric_country"), qc_report_col(d, "metric_tech"), qc_report_col(d, "metric_stage")),
                                  note = qc_report_trunc(qc_report_col(d, "adjudication_note"), 160), stringsAsFactors = FALSE), 40), "")
}

qc_report_limitations <- function(results, exports, audits) {
  missing <- setdiff(names(QC_REPORT_RESULT_FILES), names(results))
  out <- c("## 11. Remaining limitations", "",
           "- Passing checks show that outputs are reproducible and internally consistent, not that the analysis is right.",
           "- Expert results are structured elicitation from a small, non-random set of reviewers; they are not representative polling and no agreement share is a release gate.",
           "- The Secure override (posture engine Tests 1-5) is not re-run by the sensitivity analysis; Secure cells are classed not testable.",
           "- Robustness is tested one spec at a time; no joint perturbation is run.")
  if (length(missing)) out <- c(out, paste0("- QC results not available for this report: ", paste(missing, collapse = ", "), "."))
  if (qc_report_has(audits)) {
    nr <- sum(qc_report_num(audits$not_reproducible), na.rm = TRUE)
    pg <- sum(qc_report_num(audits$provenance_gaps), na.rm = TRUE)
    out <- c(out, paste0("- The case audits mark ", nr, " links across ", nrow(audits), " audits as not reproducible from shipped/pipeline data ",
                         "(raw-to-cell lineage, measure-level values held only in index_outputs.rds, the supply structure, the trade benchmark ",
                         "rows and the pathway signals are not saved per cell), and record ", pg, " provenance gaps (sources without a recorded vintage or licence)."))
  }
  if (!qc_report_any_review(exports)) out <- c(out, paste0("- ", QC_REPORT_NO_REVIEWS, " Every expert-review section is empty."))
  c(out, "")
}

# The number of distinct reviewers across the exports (from the pseudonymous codes; never printed). NA when no
# export carries reviewer codes.
qc_report_n_reviewers <- function(exports) {
  codes <- unique(stats::na.omit(as.character(unlist(lapply(exports, function(d) qc_report_col(d, "reviewer_code"))))))
  codes <- codes[nzchar(codes)]
  if (length(codes)) length(codes) else NA_integer_
}

# Rows of the review exports that come from the SYNTHETIC demo submissions (dashboard/src/review/demo.ts: submission
# ids 'SUB-DEMO-...', free text starting '[synthetic]'). Synthetic data must never read as expert results: the report
# is then marked as synthetic, and write_validation_report.R refuses to write it outside qc/private/.
qc_report_synthetic_rows <- function(exports = list()) {
  sum(vapply(exports, function(d) {
    if (!is.data.frame(d) || !nrow(d)) return(0L)
    chr <- vapply(d, function(x) is.character(x) || is.factor(x), logical(1))
    hit <- rep(FALSE, nrow(d))
    for (nm in names(d)[chr]) {
      v <- as.character(d[[nm]])
      hit <- hit | (!is.na(v) & (startsWith(v, "SUB-DEMO-") | startsWith(v, "[synthetic]")))
    }
    sum(hit)
  }, integer(1)))
}

QC_REPORT_SYNTHETIC_BANNER <- paste0("**SYNTHETIC DEMO DATA: the review sections below are built from generated demo submissions, ",
                                     "not expert judgements. Never quote, count or commit them as review results.**")

# The report. results / exports: named lists of data frames keyed by file name (missing files absent); cases: the
# parsed cases.<campaign>.json (or NULL); audits: one row per audit JSON (qc_report_audit_row()) or NULL; meta:
# list(date, results_dir, exports_dir, cases_file). Returns the Markdown lines.
qc_report_build <- function(results = list(), exports = list(), cases = NULL, audits = NULL, meta = list()) {
  n_rev <- qc_report_n_reviewers(exports)
  synthetic <- qc_report_synthetic_rows(exports) > 0
  keep_codes <- lapply(exports, function(d) d)
  exports <- lapply(keep_codes, function(d) {
    if (!is.data.frame(d)) return(d)
    d[, !grepl("name|organi[sz]ation|email|decided_by|adjudicated_by", names(d), ignore.case = TRUE) | names(d) %in% c("case_title", "card_title"),
      drop = FALSE]
  })
  cases_df <- qc_report_cases_df(cases)
  c("# AIB Explorer validation report", "",
    if (synthetic) c(QC_REPORT_SYNTHETIC_BANNER, ""),
    paste0("_Generated ", qc_report_or(meta$date, "n/a"), ". Data quality, analytical robustness and expert validation are reported ",
           "separately; none is combined into a single score. Reviewers are never named._"), "",
    qc_report_scope(results, exports, audits, meta),
    qc_report_reviewers(exports, cases_df, n_rev),
    qc_report_coverage(exports, cases_df),
    qc_report_findings(results, exports, audits),
    qc_report_validations(results, exports, audits),
    qc_report_disagreements(exports),
    qc_report_data_issues(results, exports),
    qc_report_robustness(results),
    qc_report_method(results, exports),
    qc_report_changes(exports),
    qc_report_limitations(results, exports, audits))
}

# One audits row from a parsed audit JSON (build_case_audit.R output): stem, case_id, the summary counts, and the
# differing links.
qc_report_audit_row <- function(audit, stem) {
  s <- if (is.null(audit$summary)) list() else audit$summary
  g <- unlist(audit$gaps)
  v <- function(x) if (is.null(x) || !length(x)) NA else x[[1]]
  data.frame(stem = stem, case_id = v(audit$case_id), choice_pipeline = v(s$choice_pipeline), choice_bundle = v(s$choice_bundle),
             reproduced = v(s$reproduced), differs = v(s$differs), not_reproducible = v(s$not_reproducible),
             provenance_gaps = v(s$provenance_gaps), bundle_differences = v(s$bundle_differences),
             gaps_differs = paste(g[grepl(": differs", g, fixed = TRUE)], collapse = " | "), stringsAsFactors = FALSE)
}
