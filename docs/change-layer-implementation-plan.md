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
