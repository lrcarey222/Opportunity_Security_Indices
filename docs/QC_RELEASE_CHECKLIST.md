# AIB Explorer validation-release checklist (draft)

**Status: draft for NEIS.** This is the gate a release of the AIB Explorer passes before NEIS describes
it as quality-controlled and expert-reviewed. It is deliberately **not** an agreement-percentage rule:
no release passes or fails because some share of reviewers agreed with the model. What it checks is that
errors that matter have been found and dealt with, that conclusions resting on knife-edges are visible
as such, that important results have had independent expert scrutiny, and that every published number
can be traced from source to recommendation.

Data quality, analytical robustness and expert validation are checked and signed off separately
(see [QC_ADMIN_GUIDE.md](QC_ADMIN_GUIDE.md) section 16). How each check is run:
[QC_ADMIN_GUIDE.md](QC_ADMIN_GUIDE.md); design: [QC_SYSTEM_DESIGN.md](QC_SYSTEM_DESIGN.md); hosting and
release identity: [QC_DEPLOYMENT.md](QC_DEPLOYMENT.md).

Commands: Node CLIs run from `dashboard/`, R scripts from the repository root, `<processed-dir>` is the
pipeline's processed directory. Attach evidence files to the release record (a folder in NEIS's
SharePoint, not the public repository, for anything that contains reviewer answers).

---

## High-consequence outputs (definition)

The checks below are strictest for these. NEIS confirms or amends the definition before the first gate.

1. **NEIS-central economies x headline sectors.** Economies: USA, JPN, KOR, AUS, IND, DEU (the set the
   sensitivity reports by, `QC_SENS_NEIS` in `R/qc/qc_sensitivity.R`). Headline sectors (draft, NEIS to
   confirm): batteries, critical minerals (the Extraction and Refining stages of Batteries and Electric
   Vehicles), solar, electric grid and nuclear, i.e. the five anchor groups of round 1. All six stages.
2. **Every Secure cell** (high fence), in any economy.
3. **Every Sectoral Compact decision** (`SC-*`, each scenario the Explorer shows).
4. **Every AIB node classed BUILD** under the headline scenario.

---

## The checks

### 1. No unresolved critical provenance or data errors in high-consequence outputs

- **How to check:**
  - `node scripts/review/run-bundle-qc.mjs` exits 0; `qc/results/bundle_qc_checks.csv` has no `fail`.
  - `Rscript scripts/qc/run_data_qc.R --processed-dir <processed-dir>` exits 0;
    `qc/results/pipeline_data_qc_checks.csv` has no `fail`, and every `warn` with
    `needs_human_explanation` that touches a high-consequence output has a written explanation.
  - `/review/admin` Overview: "Unresolved critical issues" is 0 for high-consequence outputs; in the
    exported `qc_priority_fixes.csv`, no row with `severity` critical (or material, in a high-consequence
    output) has `unresolved` true.
  - `reports/AIB_VALIDATION_REPORT.md` section 7 (critical data issues) lists nothing open for them.
- **Sign-off:** data lead.
- **Evidence:** the two checks CSVs and `pipeline_qc_summary.md` for the release; the filtered
  `qc_priority_fixes.csv`; the explanations of the warnings.

### 2. All golden cases pass, or have documented intentional changes with re-approval

- **How to check:** `node scripts/review/run-bundle-qc.mjs` (golden rows `BQ-GOLDEN-00/01` pass;
  `qc/results/qc_golden_case_status.csv` all `pass`); the dashboard tests in CI (`npm test`) pass;
  `qc/results/pipeline_golden_case_status.csv` failures (warnings) are explained as drift or fixed. Every
  case in `config/qc/golden_cases.yml` has `approved.by` set to an approver (not "draft - pending NEIS
  approval") and `approved.release` set to a release ID; every case changed since the last release has
  its re-approval recorded in `rationale` and the commit message (admin guide section 6).
- **Sign-off:** methodology lead (for `change_requires_review` cases), QC lead (the rest).
- **Evidence:** the golden status CSVs; the diff of `golden_cases.yml` since the last release.

### 3. Important strategic cases have independent, in-expertise expert review

- **How to check:** every high-consequence case in the campaign (at least the anchor cases and the cases
  whose golden category is `neis_central`) has at least **N** completed reviews whose `expertise_match`
  is `in`, from different reviewers (`qc_review_responses.csv`; the Agreement tab). **N is a NEIS
  decision** (round 1's anchor target is 4). Reviewers must not be authors of the Explorer's methodology.
  "I don't know" and verdict 7 do not count towards N.
- **Sign-off:** QC lead.
- **Evidence:** `qc_case_results.csv` and a count of in-expertise reviews per case.

### 4. Anchor cases reach their target number of reviewers

- **How to check:** `/review/admin` Overview, "Anchor cases at target" equals the number of anchors;
  `qc_case_results.csv` `n_interpretable` at or above `target_reviews_per_anchor` for every anchor.
  Cases below 3 usable reviews show "too few reviews to summarise" and cannot be cited as validated.
- **Sign-off:** QC lead.
- **Evidence:** the Overview anchor table (screenshot or `qc_case_results.csv` rows).

### 5. Material disagreements are categorised and adjudicated

- **How to check:** every priority item of severity material or critical has an adjudication whose
  status is not `open` (`qc_priority_fixes.csv`, `adjudication_status`), with an issue type, a
  confidence and a note; the Disagreement taxonomy tab has no uncategorised items; for anchor cases with
  verdicts 4-6, the report's "unresolved disagreements" section says what NEIS concluded and why. Agreement
  is reported, never used as the test.
- **Sign-off:** methodology lead (method and framework issues), data lead (data issues).
- **Evidence:** `aib-review-adjudications.json` (private), `qc_priority_fixes.csv`, report sections 6
  and 9.

### 6. Fragile conclusions are improved or visibly flagged

- **How to check:** list the high-consequence outputs classed **fragile** (and, separately, those whose
  fence would change) in both layers: `qc_sensitivity_bundle.csv` and `qc_sensitivity_pipeline.csv`
  (`row_type`/class rows), the Robustness tab, report section 8. Each one is either changed through a
  reviewed method or data fix, or shown to users with its caveat (for example the borderline / low
  confidence marking already in the Explorer, or text in the relevant memo or page). Flags are not a
  methodology change; any threshold change goes through the methodology lead.
- **Sign-off:** methodology lead.
- **Evidence:** the list with the action taken per item.

### 7. High-priority outputs are reproducible from source to recommendation

- **How to check:**
  - Bundle layer: `choice_reproduction.csv` shows every shipped choice reproduced from its decision trace
    (exceptions listed with reasons); `qc_summary.json` `reproduction.rate`.
  - Pipeline layer: the reproduction rows of `pipeline_robustness_summary.csv` show 0 mismatches, and in
    `pipeline_data_qc_checks.csv` the shipped-bundle-to-pipeline comparison (`drift_choice_changes`, read
    with `release_alignment_posture`) shows no cell changing choice.
  - Audits: `Rscript scripts/qc/build_case_audit.R --processed-dir <processed-dir> --all-posture-cases`
    (and `--cells` for any other high-consequence cell to be cited) writes `qc/audits/*.md` with no
    difference against the shipped bundle; the "not reproducible from shipped/pipeline data" links are
    listed and accepted as known gaps.
  - AIB nodes and compacts: as built there is no end-to-end audit; the bundle layer's `BQ-AIB-RULE-01`
    (class rule re-run) must pass, and the provenance item below must be closed.
- **Sign-off:** data lead.
- **Evidence:** the two reproduction files and the audit Markdown for each cited cell.

### 8. Data and methodology limitations are disclosed

- **How to check:** report section 11 (remaining limitations) and the Explorer's methods and data pages
  state the limitations found: provenance gaps (sources without vintage or licence), licence
  conflicts, structurally missing measures (not-assessed cells), trade-versus-production cases, entrepot
  handling, knife-edges, and anything a reviewer could not check because it is not shipped (see "Why
  this result?" gaps). Methods documents agree with the shipped numbers (for example counts quoted in
  `docs/strategic_position.md` and `docs/aib_methodology.md`).
- **Sign-off:** methodology lead and the NEIS publication owner.
- **Evidence:** links to the updated pages and report section.

### 9. Release identity is complete

- **How to check:**
  - Every bundle is stamped: `bundle_qc_checks.csv` provenance rows `BQ-PROV-*` pass (no bundle or
    detail file without a `created` time and git commit), `BQ-REL-01` passes, and
    `dashboard/public/data/review/manifest.json` `data_release.complete` is true.
  - The published site serves main's bundles: compare `created` and `git_commit` of each manifest on
    the Pages site (`/data/posture/manifest.json`, `/data/aib/manifest.json`, `/data/country/manifest.json`,
    `/data/sector/manifest.json`, `/data/manifest.json`, `/data/partnerships/manifest.json`) with the same
    files in `dashboard/public/data` on main.
  - The review data were built from that release: `manifest.json` `data_release.id` equals the
    `data_release` in `cases.<campaign>.json`, and `qc_review_responses.csv` `snapshot_matches` is true
    (`qc_case_results.csv` `n_snapshot_mismatch` 0), or each mismatch is explained.
- **Sign-off:** release manager.
- **Evidence:** the manifest comparison and the release ID.

### 10. Open items already found (must each be closed or explicitly accepted)

| Item | How to check | Owner |
|---|---|---|
| **The published Pages posture bundle lags main.** At the 2026-11-20 audit, Pages served posture 2026-11-17T17:12 (`8021770d`), main 2026-11-17T20:33 (`0496e7ba`), with different choice counts | Check 9's manifest comparison; republish the Pages copy from main | Release manager |
| **The AIB bundle cannot be traced to processed outputs.** The shipped AIB bundle (created 2026-11-17T19:07Z, no git commit) matches no saved AIB pipeline output; the latest saved outputs (2026-10-30) differ in 7 node classes and most compact coalitions (`release_alignment_aib`, `drift_aib_*` in `pipeline_data_qc_checks.csv`; `BQ-PROV-AIB-01`) | Re-run the AIB pipeline, save its outputs and configuration, re-export the bundle with a git commit, and confirm `release_alignment_aib` passes | Data lead |
| **Memos are unverified.** The only memo shipped in main's data (USA_JPN) has `verified: false` and predates the 2026-11-17 bundles; the other pairs exist only as static documents on Pages (`BQ-MEMO-01`) | Verify each memo against the current release, or label it unverified wherever it is published | Publication owner |

---

## Sign-off

| # | Check | Owner (role) | Name | Date | Release ID | Result (pass / accepted with note) | Evidence location |
|---|---|---|---|---|---|---|---|
| 1 | No unresolved critical data or provenance errors (high-consequence) | Data lead | | | | | |
| 2 | Golden cases pass or re-approved | Methodology lead / QC lead | | | | | |
| 3 | Independent in-expertise review of important cases | QC lead | | | | | |
| 4 | Anchor cases at target | QC lead | | | | | |
| 5 | Material disagreements categorised and adjudicated | Methodology lead / Data lead | | | | | |
| 6 | Fragile conclusions improved or flagged | Methodology lead | | | | | |
| 7 | Reproducible from source to recommendation | Data lead | | | | | |
| 8 | Limitations disclosed | Methodology lead / Publication owner | | | | | |
| 9 | Release identity complete | Release manager | | | | | |
| 10a | Pages posture bundle equals main | Release manager | | | | | |
| 10b | AIB bundle traceable | Data lead | | | | | |
| 10c | Memos verified or labelled | Publication owner | | | | | |
| | **Release approved** | NEIS director (or delegate) | | | | | |

Role names are placeholders: NEIS assigns the people. An "accepted with note" result must say why the
release can go ahead and what follows.
