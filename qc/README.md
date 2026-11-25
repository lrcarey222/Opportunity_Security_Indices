# qc/: automated QC results and case audits

The outputs of the AIB Explorer's QC and validation system (design: `docs/QC_SYSTEM_DESIGN.md`). Everything here
is written by a script; nothing is edited by hand. Re-run the commands below to refresh it, in this order, from
the repository root unless stated.

| Folder | What it holds | In git |
|---|---|---|
| `results/` | The latest automated outputs of the two QC layers: the shipped-bundle layer (TypeScript, reads `dashboard/public/data`) and the pipeline layer (R, reads the processed pipeline tables). Each file is replaced on every run. | yes |
| `audits/` | Source-to-conclusion audits of the posture-cell review cases: one `<ISO3>_<Tech>_<Stage>.json` / `.md` per cell, and a `CASE_###` copy for each posture case of `config/review/review_cases.yml`. | yes |
| `private/` | Reviewer exports, demo exports, the untrimmed pipeline sensitivity table and anything with reviewer data. Never committed (`.gitignore`: `qc/private/`). | no |

No file in `results/` is larger than about 1 MB. Where a full table would be larger, the committed file is trimmed
as stated below and the full table is kept elsewhere.

## Release these results were computed on

- Shipped bundles: data release `rel-9c8f1877b5c6` (posture 2026-11-17T20:33:31Z @ 0496e7ba; country
  2026-10-31T15:58:15Z @ 4bb2adc; sector bundle 8f4910df5216fbdbcf73b0be919043a7; AIB 2026-11-17T19:07:29Z;
  strategic bundle c1736c4a25c7a33e73d56466eac415e6; partnership memos manifest sha256 afbccf4e..., LF-normalised).
  The review data in `dashboard/public/data/review/` carries the same ID.
- Pipeline tables: `pipeline-5882f35f044e` (`outputs/strategic_position_tbl.rds` of the stages processed outputs,
  modified 2026-11-17 23:16 UTC, md5 5882f35f044e...).
- App commit d717db1c (feat/qc-review, cut from origin/main). Run on 2026-11-20/21.

The memos part of the release ID hashes `dashboard/public/data/partnerships/manifest.json` with line endings
normalised to LF (`dashboard/src/review/release.ts` memosManifestSha256; `R/qc/qc_core.R` qc_memos_manifest_text),
so a Windows checkout (core.autocrlf) gives the ID Pages serves. Results computed before 2026-11-21 hashed the CRLF
file and carried `rel-cfef665eb878` for the same bundles.

## Which command writes which file

### Shipped-bundle layer (from `dashboard/`)

`node scripts/review/run-bundle-qc.mjs` (exit 1 when a check or a golden case fails) writes:

| File | Contents |
|---|---|
| `bundle_qc_checks.csv` | One row per automated check on the shipped bundles: check_id, area, status (pass / warn / fail / info), n, detail, examples. |
| `choice_reproduction.csv` | The posture cells whose shipped choice the choice rule does not reproduce from the shipped trace, with the reason, and a summary row. |
| `qc_sensitivity_bundle.csv` | Trimmed. A row per cell x spec that changes the choice (`flip`) or may (`possible`: the 3-digit bundle cannot decide), and a `class` row per NON-stable posture cell or AIB node. Stable cells are left out; a not-testable posture cell's class row has no note (it is not assessed or Secure, as baseline_choice says). Every cell's class is in `dashboard/public/data/review/robustness.json`; the counts are in `robustness_bundle_summary.csv`. |
| `robustness_bundle_summary.csv` | Robustness class counts overall and by stage, technology and shipped choice; flips and possible flips by spec and by driver. |
| `qc_golden_case_status.csv` | `config/qc/golden_cases.yml` evaluated on the shipped bundles. |

`node scripts/review/build-review-data.mjs --built-at <ISO time>` runs the same QC and writes the reviewer app's data
to `dashboard/public/data/review/` (manifest, cases, method cards, framework scenarios, robustness.json,
qc_summary.json and the SYNTHETIC `fixtures/demo_submissions.json`). Same inputs and `--built-at` give
byte-identical files.

### Pipeline layer (R)

`Rscript scripts/qc/run_data_qc.R --processed-dir <stages>/data/processed --bundle-dir dashboard/public/data --out qc/results`
writes:

| File | Contents |
|---|---|
| `pipeline_data_qc_checks.csv` | One row per data check (schema, missingness, numeric validity, trade consistency, classification reproduction, knife-edges, provenance, licences, release alignment). |
| `pipeline_qc_summary.md` | A readable summary of the run, with the release key it compared against. |
| `pipeline_trade_production_flags.csv` | One row per flagged cell x flag (trade_without_production, export_exceeds_production, entrepot_signature, trade_driven_position, no_production_data_stage) with the evidence values. Only flagged cells are written (about 0.4 MB). |
| `pipeline_drift_transitions.csv`, `pipeline_drift_cells.csv`, `pipeline_drift_distribution.csv`, `pipeline_drift_ranks.csv` | Shipped posture bundle vs the processed table: choice transitions, cells whose inputs moved, input distributions and rank correlations. |
| `pipeline_drift_aib.csv`, `pipeline_drift_aib_compacts.csv` | The pipeline's `outputs/aib` (2026-10-30) vs the shipped AIB bundle (2026-11-17): node classes and compact coalitions. |
| `pipeline_golden_case_status.csv` | The golden cases on the processed table, the pipeline AIB outputs, and the shipped bundles. |

`Rscript scripts/qc/run_sensitivity.R --processed-dir <stages>/data/processed` writes:

| File | Contents |
|---|---|
| `qc_sensitivity_pipeline.csv` | Trimmed. Every cell x spec flip row, and the `cell_class` rows of the fragile and sensitive cells only. |
| `pipeline_cell_classes.csv` | One short row per cell (iso3, tech, stage, baseline choice, class, fence_change): every cell's class, stable and not testable included. |
| `pipeline_robustness_summary.csv` | Class counts (overall, tech x stage, choice, NEIS economies), per-spec change counts, reproduction checks and the need-cap diagnostic. |
| `qc/private/qc_sensitivity_pipeline_full.csv` | The untrimmed long table (a class row with flips, drivers and note for every cell). Git-ignored; `build_case_audit.R` reads it when present. |

`Rscript scripts/qc/build_case_audit.R --processed-dir <stages>/data/processed --all-posture-cases --date <ISO time>`
writes `qc/audits/` (run `run_sensitivity.R` first so each audit carries its cell's robustness class).

`Rscript scripts/qc/write_validation_report.R --date <YYYY-MM-DD> --cases dashboard/public/data/review/cases.<campaign>.json`
reads `qc/results`, `qc/audits` and the reviewer exports in `qc/private/exports` (none yet) and writes
`reports/AIB_VALIDATION_REPORT.md`. The committed report contains no synthetic review data: when the exports hold
demo rows (`SUB-DEMO-...`, `[synthetic]`) the script marks the report SYNTHETIC and refuses any `--out` outside
`qc/private/`. To see the review
sections filled, export the synthetic demo submissions and render a second, git-ignored report:

    cd dashboard
    node scripts/review/export-review.mjs --submissions public/data/review/fixtures/demo_submissions.json \
      --cases public/data/review/cases.aib_qc_2027_round1.json --out ../qc/private/demo_exports
    cd ..
    Rscript scripts/qc/write_validation_report.R --date <YYYY-MM-DD> --cases dashboard/public/data/review/cases.aib_qc_2027_round1.json \
      --exports qc/private/demo_exports --out qc/private/AIB_VALIDATION_REPORT_demo.md

## Review controls on the public Explorer

The report buttons and the "Why this result?" disclosure in the cell drawer show only when the review UI is on:
a reviewer profile saved in this browser (from `/review/profile`), or a build made with `VITE_REVIEW_UI=1`
(`dashboard/src/review/appInfo.ts` reviewUiEnabled). Ordinary visitors see the Explorer as before.
