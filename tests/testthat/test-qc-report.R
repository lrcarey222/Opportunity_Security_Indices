# Guards the validation report (R/qc/qc_report.R; docs/QC_SYSTEM_DESIGN.md 6.5): it renders every section with
# no review data (each review section saying so) and with review exports, it is deterministic, it never prints a
# reviewer name, organisation or code, and it is tolerant of result files of unknown shape.

qr_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(qr_root(), "R", "qc", "qc_report.R"))

QR_SECTIONS <- c("Scope", "Reviewers", "Coverage", "Findings", "Strongest validations", "Unresolved disagreements",
                 "Post-reveal verdicts (taxonomy)", "Prior, validity and persuasion", "Evidence gaps", "Critical data issues",
                 "Sensitive and fragile conclusions", "Methodological issues", "Changes made because of peer review", "Remaining limitations")

qr_results <- function() {
  summ <- data.frame(layer = "pipeline", data_release = "pipeline-abc",
                     group_type = c("overall", "choice", "neis_economy", "spec", "spec", "reproduction", "diagnostic"),
                     group = c("all", "partner", "USA", "need_minus", "prize_065", "choice_rule_rerun", "need_cap_artifact_at_060"),
                     n_cells = c(10, 4, 3, NA, NA, 10, NA), stable = c(6, 2, 1, NA, NA, NA, NA), sensitive = c(1, 1, 1, NA, NA, NA, NA),
                     fragile = c(2, 1, 1, NA, NA, NA, NA), not_testable = c(1, 0, 0, NA, NA, NA, NA), fence_change = c(2, 1, 1, NA, NA, NA, NA),
                     spec_kind = c(NA, NA, NA, "local", "structural", NA, NA), status = c(NA, NA, NA, "run", "run", "reproduced", "artifact_present"),
                     n_changed = c(NA, NA, NA, 3, 1, 0, 2), n_fence_changed = c(NA, NA, NA, 3, 1, NA, NA),
                     note = c(NA, NA, NA, "Need threshold 0.62", "gives the same choices as the local spec prize_plus: it cannot make a cell sensitive on its own",
                              NA, "...: ARG|Nuclear|Refining; CAN|Nuclear|Extraction"), stringsAsFactors = FALSE)
  sens <- data.frame(layer = "pipeline", data_release = "pipeline-abc", row_type = "cell_class", case_key = c("USA|Coal|Extraction", "USA|Gas|Assembly"),
                     iso3 = "USA", tech = c("Coal", "Gas"), stage = c("Extraction", "Assembly"), baseline_choice = c("watch", "preserve"),
                     class = c("fragile", "sensitive"), local_flips = c("need_minus:preserve", NA), structural_flips = c(NA, "siv_unknown_half:watch"),
                     fence_change = TRUE, drivers = c("supply_chain_need", "siv_unknown_attributes"), stringsAsFactors = FALSE)
  checks <- data.frame(check_id = c("schema_unique", "entrepot_hubs", "bounds_need"), layer = "pipeline", area = "x", table = "t",
                       status = c("info", "warn", "fail"), n = c(0, 3, 1), detail = c("ok", "SGP re-exports", "need 1.2 out of range"),
                       examples = "", needs_human_explanation = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
  odd <- data.frame(whatever = 1:2, thing = c("a", "b"))
  list(pipeline_robustness_summary.csv = summ, qc_sensitivity_pipeline.csv = sens, pipeline_data_qc_checks.csv = checks, other_result.csv = odd)
}
qr_exports <- function() {
  rr <- data.frame(submission_id = c("S1", "S2", "S3", "S4"), case_id = c("CASE_001", "CASE_001", "CASE_001", "CASE_002"),
                   reviewer_code = c("R-AAA", "R-BBB", "R-CCC", "R-AAA"), reviewer_name = c("Ada Lovelace", "Bob Secret", "Cy Hidden", "Ada Lovelace"),
                   expertise_match = c("in", "in", "adjacent", "outside"), blind_choice = "partner", verdict = c(1, 5, 4, 7),
                   main_issue = "x", data_release = "rel-123", stringsAsFactors = FALSE)
  cr <- data.frame(case_id = c("CASE_001", "CASE_002"), case_kind = "posture_cell", case_title = c("Korea batteries", "Japan EV"),
                   anchor = c(TRUE, FALSE), model_choice = c("preserve", "secure"), model_label = c("Preserve", "Secure"), n_reviews = c(3, 1),
                   n_interpretable = c(3, 0), n_dont_know = 0, modal_blind = c("partner", NA), share_matching_model = c(0, NA),
                   share_persuaded = c(0.33, NA), share_rejecting = c(0.67, NA), issue_types = c("method:1;data:1", ""),
                   suggested_choices = c("partner:2", ""), too_few_reviews = c(FALSE, TRUE), summary_label = c("3 reviews", "too few reviews to summarise"),
                   stringsAsFactors = FALSE)
  di <- data.frame(submission_id = c("D1", "D2"), kind = "data_issue", reviewer_code = c("R-BBB", "R-CCC"), reviewer_name = c("Bob Secret", "Cy Hidden"),
                   severity = c("critical", "cosmetic"), what_is_wrong = c("BEL crude is re-exported through NLD", "typo"),
                   metric_country = "BEL", metric_tech = "Oil", metric_stage = "Extraction", metric_label = "Top supplier",
                   adjudication_status = c("confirmed", "resolved"), adjudication_issue_type = c("data", "cosmetic"), adjudication_note = c("real", "fixed"),
                   adjudicated_by = c("Admin Person", "Admin Person"), resolution_commit = c(NA, "abc1234"), priority_score = c(0.9, 0.05),
                   stringsAsFactors = FALSE)
  mf <- data.frame(submission_id = "M1", card_id = "MC_001", card_title = "Low prize + low need -> Watch", reviewer_code = "R-AAA",
                   reviewer_name = "Ada Lovelace", stance = "modify", materiality = 3, bias_direction = "understates_need", stringsAsFactors = FALSE)
  list(qc_review_responses.csv = rr, qc_case_results.csv = cr, qc_data_issues.csv = di, qc_method_feedback.csv = mf)
}
qr_cases <- function() list(cases = list(list(case_id = "CASE_001", kind = "posture_cell", title = "Korea batteries", anchor = TRUE,
                                              sector_groups = list("batteries", "electric-vehicles")),
                                         list(case_id = "CASE_002", kind = "posture_cell", title = "Japan EV", anchor = FALSE,
                                              sector_groups = list("critical-minerals")),
                                         list(case_id = "CASE_003", kind = "aib_node", title = "Node", anchor = TRUE, sector_groups = list("solar"))))
qr_audits <- function() data.frame(stem = "KOR_Batteries_Components", case_id = "CASE_001", choice_pipeline = "preserve", choice_bundle = "preserve",
                                   reproduced = 40, differs = 1, not_reproducible = 18, provenance_gaps = 1, bundle_differences = 0,
                                   gaps_differs = "2. base import HHI <- partner rows: differs", stringsAsFactors = FALSE)

qr_section <- function(md, title) {
  i <- which(grepl("^## [0-9]+\\. ", md) & sub("^## [0-9]+\\. ", "", md) == title)
  j <- which(startsWith(md, "## "))
  nxt <- j[j > i][1]
  md[(i + 1):(if (is.na(nxt)) length(md) else nxt - 1)]
}

test_that("with no review data every section renders and the review sections say so", {
  md <- qc_report_build(qr_results(), list(), NULL, qr_audits(), meta = list(date = "2026-11-20"))
  heads <- md[grepl("^## [0-9]+\\. ", md)]
  expect_equal(sub("^## [0-9]+\\. ", "", heads), QR_SECTIONS)
  expect_equal(sub("^## ([0-9]+)\\..*$", "\\1", heads), as.character(seq_along(QR_SECTIONS)))
  for (s in c("Reviewers", "Coverage", "Unresolved disagreements", "Post-reveal verdicts (taxonomy)", "Prior, validity and persuasion", "Evidence gaps",
              "Changes made because of peer review")) {
    expect_true(any(qr_section(md, s) == QC_REPORT_NO_REVIEWS), info = s)
  }
  # the automated parts still render
  rob <- qr_section(md, "Sensitive and fragile conclusions")
  expect_true(any(grepl("10 cells: 6 stable, 1 sensitive, 2 fragile, 1 not testable", rob, fixed = TRUE)))
  expect_true(any(grepl("USA\\|Coal\\|Extraction", rob, fixed = TRUE))) # the pipes are escaped in the Markdown table
  fnd <- qr_section(md, "Findings")
  expect_true(any(grepl("choice_rule_rerun", fnd)))
  expect_true(any(grepl("bounds_need", fnd)))
  crit <- qr_section(md, "Critical data issues")
  expect_true(any(grepl("bounds_need", crit)))
  expect_false(any(grepl("entrepot_hubs", crit))) # a warn, not a fail
  meth <- qr_section(md, "Methodological issues")
  expect_true(any(grepl("prize_065", meth)))
  expect_true(any(grepl("ignores the need-floor cap", meth)))
  # an unknown result file is listed, not dropped
  expect_true(any(grepl("other_result.csv", qr_section(md, "Scope"), fixed = TRUE)))
  expect_true(any(grepl("pipeline-abc", md, fixed = TRUE)))
})

test_that("with review exports it summarises them and never prints a reviewer name or code", {
  md <- qc_report_build(qr_results(), qr_exports(), qr_cases(), qr_audits(),
                        meta = list(date = "2026-11-20", exports_dir = "qc/private/exports"))
  txt <- paste(md, collapse = "\n")
  for (secret in c("Ada Lovelace", "Bob Secret", "Cy Hidden", "Admin Person", "R-AAA", "R-BBB", "R-CCC", "reviewer_name", "reviewer_code")) {
    expect_false(grepl(secret, txt, fixed = TRUE), info = secret)
  }
  rv <- qr_section(md, "Reviewers")
  expect_true(any(grepl("^3 reviewers contributed", rv)))
  expect_true(any(grepl("not a representative poll", rv)))
  expect_true(any(grepl("in 2, adjacent 1, outside 1", rv, fixed = TRUE)))
  expect_true(any(grepl("batteries 2", rv, fixed = TRUE)))
  cov <- qr_section(md, "Coverage")
  expect_true(any(grepl("CASE_003", cov)))
  dis <- qr_section(md, "Unresolved disagreements")
  expect_true(any(grepl("CASE_001", dis)))
  expect_true(any(grepl("Too few reviews to summarise.*CASE_002", dis)))
  crit <- qr_section(md, "Critical data issues")
  expect_true(any(grepl("re-exported through NLD", crit)))
  expect_false(any(grepl("typo", crit))) # cosmetic and resolved
  ch <- qr_section(md, "Changes made because of peer review")
  expect_true(any(grepl("abc1234", ch)))
  meth <- qr_section(md, "Methodological issues")
  expect_true(any(grepl("MC_001", meth)))
  expect_true(any(grepl("rel-123", qr_section(md, "Scope"), fixed = TRUE)))
})

test_that("the report is deterministic and the scrub drops identity columns", {
  a <- qc_report_build(qr_results(), qr_exports(), qr_cases(), qr_audits(), meta = list(date = "2026-11-20"))
  b <- qc_report_build(rev(qr_results()), qr_exports()[c(4, 3, 2, 1)], qr_cases(), qr_audits(), meta = list(date = "2026-11-20"))
  expect_identical(a, b)
  s <- qc_report_scrub(data.frame(case_id = 1, reviewer_code = 1, reviewer_name = 1, organization = 1, email = 1, adjudicated_by = 1,
                                  card_title = 1, submission_id = 1))
  expect_equal(names(s), c("case_id", "card_title", "submission_id"))
})

test_that("synthetic demo submissions are detected and the report says so; real exports carry no banner", {
  real <- qc_report_build(qr_results(), qr_exports(), qr_cases(), qr_audits(), meta = list(date = "2026-11-20"))
  expect_equal(qc_report_synthetic_rows(qr_exports()), 0)
  expect_false(any(grepl("SYNTHETIC", real, fixed = TRUE)))
  demo <- list(qc_review_responses.csv = data.frame(submission_id = c("SUB-DEMO-1", "SUB-DEMO-2"), case_id = "CASE_001",
                                                    blind_rationale = c("[synthetic] demo rationale", NA), stringsAsFactors = FALSE))
  expect_equal(qc_report_synthetic_rows(demo), 2)
  expect_equal(qc_report_synthetic_rows(list(x.csv = data.frame(what_is_wrong = "[synthetic] demo", stringsAsFactors = FALSE))), 1)
  md <- qc_report_build(qr_results(), demo, qr_cases(), qr_audits(), meta = list(date = "2026-11-20"))
  expect_identical(md[3], QC_REPORT_SYNTHETIC_BANNER)
})

test_that("an audit JSON becomes one audits row", {
  au <- list(case_id = "CASE_001", summary = list(choice_pipeline = "preserve", choice_bundle = "preserve", reproduced = 3, differs = 1,
                                                  not_reproducible = 2, provenance_gaps = 0, bundle_differences = 0),
             gaps = list("5. x: differs (y)", "1. z: not reproducible from shipped/pipeline data"))
  r <- qc_report_audit_row(au, "KOR_Batteries_Components")
  expect_equal(r$differs, 1)
  expect_equal(r$gaps_differs, "5. x: differs (y)")
})

test_that("CLI arguments parse flags, pairs and key=value", {
  a <- qc_report_cli_args(c("--results", "qc/results", "--date=2026-11-20", "--flag"))
  expect_equal(a$results, "qc/results")
  expect_equal(a$date, "2026-11-20")
  expect_true(a$flag)
  expect_equal(qc_report_processed_default(list()), "data/processed")
})

test_that("golden cases evaluated on several tables are listed once, failures apart from cases a table cannot evaluate", {
  g <- data.frame(id = c("GC_001", "GC_002", "GC_002", "GC_003", "GC_003"), status = c("pass", "fail", "fail", "not_applicable", "not_applicable"),
                  stringsAsFactors = FALSE)
  s <- qc_report_golden(g)
  expect_equal(s$fails, "GC_002")
  expect_equal(s$not_evaluable, "GC_003")
  md <- qc_report_build(c(qr_results(), list(pipeline_golden_case_status.csv = g)), list(), NULL, NULL, meta = list(date = "2026-11-20"))
  line <- md[startsWith(md, "Golden cases (pipeline_golden_case_status.csv)")]
  expect_match(line, "failing: GC_002; not evaluable on some table: GC_003", fixed = TRUE)
})

# UX v2 exports: the v2 columns of qc_review_responses (after the v1 ones) and qc_evidence_gaps.csv
qr_v2_exports <- function() {
  ex <- qr_exports()
  rr <- data.frame(submission_id = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
                   case_id = c("CASE_001", "CASE_001", "CASE_001", "CASE_002", "CASE_002", "CASE_003", "CASE_003", "CASE_001"),
                   reviewer_code = c("R-AAA", "R-BBB", "R-CCC", "R-AAA", "R-BBB", "R-CCC", "R-AAA", "R-AAA"),
                   expertise_match = "in",
                   blind_choice = c("partner", "secure", "watch", "insufficient_evidence", "insufficient_evidence", "secure", "outside_expertise", "partner"),
                   model_choice = c("partner", "partner", "partner", "secure", "secure", "watch", "watch", "partner"),
                   verdict = c(1, 2, 5, 3, 3, 4, 7, 1), data_release = "rel-123",
                   submitted_at = c("2027-01-01", "2027-01-02", "2027-01-03", "2027-01-04", "2027-01-05", "2027-01-06", "2027-01-07", "2026-12-01"),
                   product = c("country", "country", "country", "country", "country", "sector", "sector", "country"),
                   blind_state = c(NA, NA, NA, "insufficient_evidence", "insufficient_evidence", NA, "outside_expertise", NA),
                   verdict_code = c("same_same", "persuaded", "wrong_rule", "plausible_insufficient", "plausible_insufficient", "wrong_fact", "outside_expertise", NA),
                   validity = c("supported", "supported", "not_supported", "unsure", "partly", "not_supported", NA, NA),
                   persuasion = c("unchanged", "changed", "unchanged", "unchanged", "unchanged", "unchanged", NA, NA),
                   prior_differs = c(FALSE, TRUE, TRUE, NA, NA, TRUE, NA, NA),
                   verdict_consistency = c("consistent", "consistent", "consistent", "unknown", "unknown", "consistent", "unknown", NA),
                   persuaded_by_evidence = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA), stringsAsFactors = FALSE)
  gaps <- data.frame(submission_id = c("S4", "S4", "S5", "S3"), case_id = c("CASE_002", "CASE_002", "CASE_002", "CASE_001"),
                     reviewer_code = c("R-AAA", "R-AAA", "R-BBB", "R-CCC"), product = "country",
                     source = c("insufficient_evidence", "insufficient_evidence", "insufficient_evidence", "red_team"),
                     category = c("production_capacity", "ownership", "production_capacity", "qualification_constraints"),
                     category_label = c("Actual production capacity", "Ownership (firm-level)", "Actual production capacity", "Qualification constraints / lead times"),
                     text = c("plant list", NA, NA, "lead times"), stringsAsFactors = FALSE)
  ex[["qc_review_responses.csv"]] <- rr
  ex[["qc_evidence_gaps.csv"]] <- gaps
  ex
}

test_that("the v2 sections summarise the taxonomy, prior x validity x persuasion and evidence gaps", {
  md <- qc_report_build(qr_results(), qr_v2_exports(), qr_cases(), qr_audits(), meta = list(date = "2026-11-20"))
  heads <- md[grepl("^## [0-9]+\\. ", md)]
  expect_equal(sub("^## [0-9]+\\. ", "", heads), QR_SECTIONS)
  txt <- paste(md, collapse = "\n")
  for (secret in c("R-AAA", "R-BBB", "R-CCC", "Ada Lovelace", "reviewer_code")) expect_false(grepl(secret, txt, fixed = TRUE), info = secret)
  tx <- qr_section(md, "Post-reveal verdicts (taxonomy)")
  # the latest review per reviewer and case: S8 (an older row of R-AAA on CASE_001) is not counted
  expect_true(any(grepl("7 reviews)", tx, fixed = TRUE)))
  expect_true(any(grepl("| Disagree: the decision rule linking evidence to recommendation | wrong_rule | 1 | 14% |", tx, fixed = TRUE)))
  expect_true(any(grepl("insufficient_evidence 2, outside_expertise 1", tx, fixed = TRUE)))
  pp <- qr_section(md, "Prior, validity and persuasion")
  expect_true(any(grepl("| different | supported | changed | 1 |", pp, fixed = TRUE)))
  expect_true(any(grepl("\"changed\"): 1 review(s) on 1 case(s).", pp, fixed = TRUE)))
  expect_true(any(grepl("| CASE_001 | country | secure | partner | supported | changed |", pp, fixed = TRUE)))
  eg <- qr_section(md, "Evidence gaps")
  expect_true(any(grepl("| Actual production capacity | 2 | 0 | 2 | 1 |", eg, fixed = TRUE)))
  expect_true(any(grepl("| CASE_002 | 2 | 2 | yes |", eg, fixed = TRUE)))
  expect_true(any(grepl("1 case(s) where it happened 2 or more times", eg, fixed = TRUE)))
})

test_that("exports written before UX v2 make the v2 sections say there is no data; synthetic v2 rows are flagged", {
  md <- qc_report_build(qr_results(), qr_exports(), qr_cases(), qr_audits(), meta = list(date = "2026-11-20"))
  for (s in c("Post-reveal verdicts (taxonomy)", "Prior, validity and persuasion", "Evidence gaps")) {
    expect_true(any(qr_section(md, s) == QC_REPORT_NO_V2), info = s)
  }
  demo <- qr_v2_exports()
  demo[["qc_evidence_gaps.csv"]]$text[1] <- "[synthetic] demo gap"
  expect_equal(qc_report_synthetic_rows(demo["qc_evidence_gaps.csv"]), 1)
  expect_identical(qc_report_build(qr_results(), demo, qr_cases(), NULL, meta = list(date = "2026-11-20"))[3], QC_REPORT_SYNTHETIC_BANNER)
})
