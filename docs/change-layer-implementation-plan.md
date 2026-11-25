# Change layer: implementation plan (vertical slice 1)

Branch `feat/change-layer` (worktree `osi-worktrees/change`). Do not merge to main. Commit incrementally.

## Scope of slice 1

- **Unit:** posture cell, `iso3 × tech × stage`.
- **History source:** the posture cores already committed in git, plus an optional provisional working-tree core.

## Steps

1. **Docs.** This file, the audit and the architecture.
2. **Config.** `config/change_layer.yml`: method paths, thresholds, severity, field labels, choice labels, and the
   field → source-ID map.
3. **Pure R in `R/change/`** (no IO, per AGENTS.md):
   - `posture_vintage.R`:
     - `posture_vintage_from_core(core)` reuses `qc_posture_core_to_table` and adds confidence, evidence, posture,
       variant, pathway, partner1 and deploy_case.
     - `posture_vintage_header()`, `change_method_id()`, `change_data_id()`, `change_slug()`, `assessment_id()`.
   - `posture_compare.R`:
     - `posture_comparable(a, b)`
     - `posture_compare(prev, cur, cfg, bridge = NULL)`, which returns `list(pair, changes)`
     - `posture_classify_cells()`
     - `posture_attribute()`
   - `posture_explain.R`: `posture_change_explain(change, cfg)`, a deterministic template.
   - `posture_impacts.R`:
     - `posture_impact_candidates(tags, crosswalk, cells)`
     - `validate_analytical_impacts(x)`
     - `attach_impacts(changes, impacts, window)`
   - `change_export.R`: `change_bundle(vintages, pairs, changes)`, the columnar-free row list for the dashboard.
4. **IO script.** `scripts/45_build_posture_vintages.R`:
   - Walk the git history of the posture manifest.
   - For each commit, read the core with `git show`, dedupe by content hash, and fingerprint the method with
     `git rev-parse <commit>:<path>`.
   - Write `data/vintages/posture/{index.json,<id>.csv.gz}`.
   - Compare each vintage with its previous comparable vintage.
   - Write `dashboard/public/data/changes/{manifest.json,changes.<md5-8>.json}`.
   - Flags: `--include-worktree`, `--dry-run`.
5. **Tests.** `tests/testthat/test-change-layer.R`, with constructed vintages covering:
   - no meaningful change;
   - data change only;
   - an evidence and confidence improvement;
   - a material score change (bar crossing);
   - a recommendation change;
   - a methodology-only change;
   - a mixed change;
   - bridge decomposition;
   - incomparable pairs;
   - impact attachment window;
   - crosswalk candidates;
   - template text.
6. **Dashboard:**
   - `src/changes/contract.ts`: types and a checker.
   - `src/changes/store.ts`: load the bundle; a 404 means no history.
   - `src/changes/ChangeSince.tsx`: the block in `CellDrawer`, with a "Why did this change?" disclosure.
   - `src/changes/ChangesView.tsx`: the `/changes` route.
   - A vitest for the contract and the view.
   - A budget line.
7. **Bridge files:**
   - `schemas/analytical_impact.schema.json`
   - `config/neis_intelligence_crosswalk.yml`
   - `config/review/analytical_impacts.yml`, empty and holding no fabricated events
8. **Validate.**
   - R: the testthat file.
   - Dashboard: `npm run typecheck`, `npm test`, `npm run build` and budget.
   - Screenshots of the drawer block and `/changes`.

## Commits

1. Docs.
2. R engine, config and tests.
3. Script and generated vintages and bundle.
4. Dashboard UI.
5. Bridge schema and crosswalk.

## Out of scope for slice 1

- Bridge vintages produced by the pipeline (the compare function supports them).
- Input-table snapshots.
- Automatic vintage creation inside scripts/91.
- Sector, Partnership and Compact change cards.
- The NEIS Intelligence migration.
- The landing page.

---

# Slice 1: outcome (2026-11-25)

## What the real history shows

There are nine distinct committed posture vintages, from 2026-10-28 to 2026-11-21, and eight comparable pairs.

**Every pair also changed the engine code.** That holds even for the 2026-11-17 "current trade" refresh. No
historical export recorded its resolved raw-input vintages. So no historical change can honestly be called
"new data", and none can be called "method only". Most land as `unattributed`; cells whose own share or market moved
land as `mixed`.

That is the right answer for this history, and it is also a process finding. **For a data refresh to be readable as
a real-world change, it has to be exported from an unchanged engine commit, with `raw_inputs_resolved.yml`
stamped (slice 2), or with a bridge run.**

## Validation

- **R:** `testthat::test_file("tests/testthat/test-change-layer.R")`. 67 expectations, 0 failures. They seed the
  six brief cases plus mixed, unattributed, bridge decomposition, dirty fingerprint, incomparable pair, crosswalk,
  impact validation and window, provenance IDs and bundle shape.
- **Dashboard:**
  - `npm run typecheck` is clean.
  - `npx vitest run --project ui src/changes`: 7 tests, all passing.
  - `npm test`: the full suite (see the PR notes for the pre-existing failures).
- **Script:** `Rscript scripts/45_build_posture_vintages.R` (add `--dry-run` / `--include-worktree`) takes about 2.5
  minutes and is idempotent.
- **Browser:**
  - `/countries/USA?node=Nuclear|Assembly` shows "Last changed 2026-11-17: Catch Up → Preserve, Position
    0.17 → 0.90".
  - `/changes` lists the eight pairs and the filterable records.
  - Screenshots are in `docs/change-layer-screens/`.

## Remaining technical debt

1. **Data stamps are windows only.** Raw-input vintages are captured only through `--raw-inputs` on a provisional
   vintage. scripts/91 should write them into the posture manifest.
2. **Vintages are created by hand** (scripts/45). scripts/91 does not yet call it on export, and CI does not check
   that the change bundle is current against the posture bundle.
3. **The method fingerprint is coarse and narrow.**
   - Coarse: export-only edits in `R/dashboard/posture` and `scripts/91` count as method changes.
   - Narrow: upstream index code (`R/indices`, trade pipelines) does not count.
   - The fix is a split into `engine` and `presentation` path sets, plus an upstream set.
4. **The provenance map is curated** (`config/change_layer.yml` sources). It names likely inputs; it is not computed
   lineage.
5. **Evidence is the 4-band `stage_evidence_quality_cp` only.** The VSE shares (exact/split/shared/proxy) live in
   the rds and are not in the core. Reading proxy → stage-specific needs the table-based vintage
   (`posture_vintage_from_table`) or a core column.
6. **Budget.** No dedicated `check-budget.mjs` line yet. The bundle is about 150 KB gzipped, inside `allData`.
7. **Bundle size.** The bundle repeats keys per record (4.1 MB raw). A columnar encoding like the posture core's would
   cut it by about 5×.
8. **Scope.** The Sector, Partnership and Compact cards and the Country "Bottom line" do not yet show changes.
9. **Bridge vintages** need engine input-table snapshots; they are not produced yet.

## Recommended next three slices

1. **Stamp and automate.**
   - scripts/91 writes `raw_inputs_resolved` and an engine-only `method_id` into the posture manifest, then calls
     scripts/45.
   - A CI check fails when the change bundle is stale against the posture bundle.
   - Split the method paths into engine and presentation sets.
   - After this, the next data refresh is attributable.
2. **Bridge runs and evidence specificity.**
   - Snapshot scripts/26's input tables, hash-addressed under `data/vintages/inputs/`, deduped by content.
   - Add a `--bridge` mode that reruns the current engine on the previous inputs.
   - Carry `evidence_specificity` and the dominant VSE status into the core or vintage, so "Shared → Stage-specific"
     is a real line.
3. **The Intelligence bridge, end to end.**
   - NEIS Intelligence migration `010_analytical_impacts.sql` and `metadata.iso3` on geography terms.
   - A rule-based proposer using `config/neis_intelligence_crosswalk.yml` (no model), plus a review queue mirroring
     `editorial_reviews`.
   - An export of accepted rows to OSI.
   - A change-bundle read-back so an event page can say "contributed to a material change in our assessment".

# Task 7: what an EconVitals-style NEIS landing page needs

The landing page should be built only on the change layer and on existing clean data.

| Question | Exists now | Missing |
|---|---|---|
| Which AIB recommendations changed recently? | **Yes.** The change bundle (`types` contains `recommendation`, with attribution) | Only attributable refreshes (slice 1 of the next three) |
| Where are dependencies worsening? | Need and import share per cell; T12M import exposure (scripts/37–38) | A *dated* series: Need deltas across vintages are confounded by method changes until stamps exist. Monthly import exposure is the cleaner near-term signal |
| Where is strategic capability improving? | Position (cp) and export share per cell; monthly trade momentum | Same attribution caveat. Momentum (`mom`, `mq`) is a ready per-cell trend |
| Where are policy commitments failing to translate into delivery? | Industrial Policy Strength module (lab); NEIS Intelligence `implementation_status` (announced → operational, cancelled, delayed) | A join of policies or projects to cells. Neither repo has project or policy entities; this needs the Intelligence bridge plus a project entity |
| Which partner relationships are becoming more complementary? | Partnership dyads and memos (static) | Dyad-level vintages: the same snapshot pattern applied to partnership outputs |
| What decision windows are approaching? | Nothing structured | A dated-event entity (policy deadlines, tariff dates, e.g. the Dec 4 snapshot) in NEIS Intelligence |

**Verdict:** only the "recommendations changed" panel has clean data today. Build the landing page after the
stamp/automate slice, starting with that panel plus momentum, so it does not headline unattributable history.
