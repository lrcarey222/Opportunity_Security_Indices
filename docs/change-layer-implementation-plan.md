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

## Slice 2: stamp and automate (2026-11-25, done)

**What changed:**

- **Method fingerprint split** (`config/change_layer.yml` `method_paths`, layer version 2). Engine + upstream decide
  attribution; presentation is recorded only. Fingerprints are file-level git blob ids (`scripts/utils/change_stamps.R`),
  from the working tree at export or from git for older commits.
  - On history this correctly turns the two export-only re-exports (2026-10-31 and 2026-11-21) into "method same"
    pairs, both with zero changes.
  - Every pair with changes still changed the engine, so the history stays `unattributed`.
- **Engine input ledger.** scripts/26 traces every data file it reads and writes `outputs/strategic_position_inputs.json`.
- **Manifest stamps.** scripts/91 writes `stamps` into the posture manifest: method (engine, upstream, presentation),
  inputs (ledger id plus the path → md5 map) and raw_inputs (from `raw_inputs_resolved.yml`).
  - It refuses a ledger older than `strategic_position_tbl.rds`.
  - It then runs scripts/45 unless `--no-vintages` is given.
- **Attribution rule** (`change_data_relation`):
  - identical ledgers make the data verifiably the same, so a method change there is exactly `methodology`;
  - a changed ledger with a method change is `mixed`;
  - the same method is `real_world`.
- **scripts/45:**
  - reads the manifest stamps;
  - archives provisional (uncommitted) vintages and prunes orphans;
  - reuses archived vintages (about 35 s, was about 2 min).
- **CI guard.** `src/changes/__tests__/stale.test.ts` checks that the change bundle's latest vintage is the shipped
  posture core.

**Validation:**

- R tests: 82 expectations, all passing. New coverage: data relation, the ledger tracing real reads (and skipping R's
  own library files), and the method stamp ignoring presentation.
- Change-layer vitest: 9 passing. Typecheck is clean.
- All four touched scripts parse.
- A simulated stamped export (a modified core plus a manifest stamped as scripts/91 does) ran through scripts/45 as a
  provisional vintage. The stamps were read, the cell change was classified, the archive was written and the CI guard
  passed. The real bundle was then restored.

**Not verified here:** a real scripts/26 → scripts/91 run. This worktree has no pipeline data. The ledger and stamp code
is unit-tested, and both scripts parse.

**One-time effect.** Instrumenting scripts/26 changes the engine fingerprint, so the first stamped export will read
"method changed" against history. Every export after it is attributable.

## Slice 3: bridge runs and evidence basis (2026-11-25, done)

**What changed:**

- **Input store.** scripts/26 copies every ledger file into a content-addressed, git-ignored store (`data/vintages/inputs/`, configured by `input_store` in `config/change_layer.yml`).
  - A file is copied to a temp name and kept only if its md5 still matches.
  - Re-runs copy nothing new unless an input's content changed.
- **Bridge mode.** `OPSI_BRIDGE=<vintage id | manifest JSON>` makes scripts/26 run on that vintage's inputs:
  - The data directories move into a sandbox filled from the store. Writes cannot reach live outputs, and file checks, newest-file resolution and the two caches all see the old data.
  - Reads outside the data directories are redirected to the store.
  - Any other data read is reported as a leak.
  - `bridge_report.json` records `pure`, the counts and the method fingerprint.
- **Ingest.** `scripts/45 --bridge DIR` stores the bridge table and its metadata under `data/vintages/posture/bridges/`. Comparisons use it while it is pure and still matches the pair's inputs and method.
- **Evidence basis.** `evidence_status_from_shares()` takes the dominant VSE status from the `_cp` evidence shares:
  - scripts/91 writes it as `cells.es`, adding about 56 KB raw to the core;
  - vintages carry `evidence_status`;
  - a change of basis is an `evidence` change;
  - the drawer shows "Evidence basis: Proxy → Exact (stage-specific)".
  - Historical vintages have no basis, because their cores predate the column.
- **Layer version 3.** Archived vintages are rewritten with the new column.

**Validation:**

- R tests: 107 expectations, all passing. New coverage:
  - evidence basis and its explanation;
  - sandbox nesting and the bridge plan;
  - an end-to-end bridge in a temporary repository, where a fake engine runs normally (ledger plus store), the live inputs change, and the bridge re-reads the old inputs from the store (materialized and redirected), writes only in its sandbox and leaves live output untouched;
  - a read of a new file making the bridge impure;
  - error paths.
- `test-posture-export.R` passes.
- Change-layer vitest: 10 passing.
- All touched scripts parse.

**Not verified here:**

- A real bridge of scripts/26. It needs a stamped vintage, and none exists until the first real scripts/26 → scripts/91 run with this code.
- The real input store's size. It depends on the largest engine inputs, such as the Comtrade dyads CSV; the default cap is 2 GB per file.

## Slice 4: the first stamped run, end to end (2026-11-25, done)

Run on the real pipeline in `osi-worktrees/change`:

- `config/config.yml` was copied from `osi-worktrees/stages`.
- `data/raw` was a temporary junction to stages' raw data. scripts/26 only reads it. The junction was removed afterwards.
- `data/processed` was copied from stages (2 GB, git-ignored). These are the 17 Nov inputs that built the shipped bundle.

**What happened:**

1. **scripts/26 with the ledger.** `strategic_position_tbl`, `strategic_aib_division_tbl` and `strategic_posture_layers_tbl` came out **byte-identical** to the shipped run, and no diagnostics file changed. The instrumentation does not touch results.
   - The ledger lists 38 input files (142 MB), all recorded as repository-relative paths. Ledger paths are now logical rather than link-resolved, so a junctioned data directory never leaks a machine path.
   - The input store holds those 38 files (136 MB).
2. **scripts/91, stamped.** The export took about 10 minutes, then built the vintage itself.
   - Its stamps are the method fingerprint (45 engine and upstream files), the ledger and 11 raw-input vintages.
   - The core differs from 07b6f4bd only in its stamps and the new `cells.es` column. Fifteen detail files were sampled and differ only in stamps.
   - The new core is 2.69 MB raw; the posture budget reads 2.43 MB of 2.6 MB gzipped.
   - **Vintage 2026-11-26-576eadf8** (core_commit b865610c) is the first stamped vintage. Against 07b6f4bd it has 0 meaningful changes, with method "changed" (the ledger lines) and data "unknown".
   - The data relation no longer reads "changed" just because a richer stamp appears on one side; the input windows decide.
3. **A real bridge run.** `OPSI_BRIDGE=2026-11-26-576eadf8 Rscript scripts/26_build_strategic_position.R` ran with no raw-data junction in place.
   - Result: **PURE**. 32 inputs were materialized from the store and 6 redirected, with 0 reads outside the previous inputs and 0 missing.
   - Its table is byte-identical to the shipped run, and the live outputs and diagnostics were untouched.
   - `scripts/45 --bridge` recognizes a bridge with a vintage's own inputs and method as a reproducibility check: it matched all 14,124 cells, with 0 choice and 0 value differences, and is not stored.
4. **Store pruning.** `scripts/45 --prune-store` keeps the inputs of stamped vintages plus the current engine ledger. Here it kept all 38 files.

**Operational notes:**

- On this machine scripts/26 takes about 3 minutes, scripts/91 about 10 minutes, and scripts/45 about 2 minutes. A bridge sandbox takes about 260 MB under `outputs/change_layer/bridge/`, which is git-ignored and safe to delete.
- The dashboard detail window is now the last **2** comparable pairs that changed something, plus any later unchanged pairs. With 4, the zero-change re-exports pulled the 30 Oct rebuild (10,469 records, 17 MB) into the bundle.

**From now on:** every scripts/26 → scripts/91 run is attributable. The same engine on new inputs is `real_world`; a new engine on identical inputs is exactly `methodology`; both together are `mixed` until a bridge from the previous stamped vintage splits them.

## Slice 5: the Intelligence bridge, end to end (2026-11-25, done)

**Explorer:**

- scripts/45 publishes `dashboard/public/data/changes/assessments.<hash>.json` (338 KB): the latest vintage's 9,237 assessed cell ids plus the crosswalk, named in the changes manifest.
- The impact validator and JSON schema accept `dimension: unspecified` for proposals and refuse it for accepted impacts.

**NEIS Intelligence** (`neis-intelligence` worktree `C:/Users/LCarey/neis-worktrees/impacts`, branch `feat/analytical-impacts`, cut from origin/main c4b5c1e):

- migration 010;
- the `src/services/impacts` proposer, review, export and read-back;
- `scripts/impacts.ts` (`npm run impacts -- propose | export`);
- `/admin/impacts`, linked in the admin nav;
- the "In the AIB assessment" panel on item pages;
- the `AIB_DATA_SOURCE` setting;
- `docs/analytical-impacts.md`.

**Validation:**

- NEIS `tests/impacts.test.ts` passes 7 tests:
  - candidates;
  - the seeded US grid award proposed against US Electric Grid Components and Assembly, idempotently;
  - the too-broad guard;
  - accept requiring a dimension, the audit trail and an export without email addresses;
  - the database check;
  - the read-back;
  - loading the index from the Explorer's layout.
- NEIS typecheck and lint are clean.
- **Cross-repository smoke test.** The seeded demo items were proposed against the real `data/changes`, one was accepted, and the export validated clean against the Explorer's own R validator.
- Nothing from the demo database was committed to the Explorer. Its `analytical_impacts.yml` stays empty until real items are reviewed.

**Not done:**

- The production NEIS database has not been migrated.
- No real item has been reviewed.
- The read-back has not been seen in a browser, because it needs an accepted impact attached to a real change.

## Slice 6: field-level provenance and a columnar bundle (2026-11-26, done)

**Provenance:**

- **Ledger maps in headers.** Vintage headers carry the engine input ledger (path → md5) from the manifest stamps.
- **The diff.** A pair of stamped vintages gets `inputs_changed`, every input file modified, added or removed (`change_input_diff`). That diff is computed.
- **Per record.** Each change record gets `changed_inputs`: the changed files that feed a field it moved. `config/change_layer.yml` `field_inputs` maps each field to ledger path patterns; that map is curated and says where to look.
- **The why panel** lists those files, or says that the engine read identical inputs, or that none of the changed files feeds what moved.
- **The data relation** now compares the ledger maps file by file.

**Method versus data.** The three engine rule tables scripts/26 reads (`config/posture_delivery_modes.csv`, `config/posture_policy_design.csv`, `config/security_route_rules.csv`) were in the data ledger. They are now in the engine fingerprint, and any file under a method path is kept out of the ledger and out of the diff. Old ledgers still list them, so a file-by-file comparison is needed: comparing ids alone would read the first new export as "data changed".

**Trap.** R's `$` partially matches list names, so `data$inputs` silently returned `data$inputs_id`. Use `[["inputs"]]`.

**Columnar bundle (schema 2).**

- Records are now one array per field (`change_columns`), and the TS decoder accepts both schema 1 and schema 2.
- The change file went from 4.3 MB to 2.5 MB raw, 109 KB gzipped. The cut is smaller than the 5× hoped for, because the explanation text dominates what remains.
- `check-budget.mjs` has a "Change data" line covering the manifest, the changes file and the assessment index. It reads 130 KB against a 512 KB limit, and it now counts toward All data.

**Layer version 4.**

**Validation:**

- R tests pass. New coverage: columnar shape, input diff with method-path exclusion, relation from maps, and per-record changed inputs.
- Dashboard: 253 posture and change tests pass, including schema 2 decoding and the changed-inputs panel.
- Build and budget are OK.
- Browser: the drawer was checked on the columnar file.

**Not yet seen on real data.** Real provenance needs two stamped vintages. Today only 2026-11-26-576eadf8 is stamped; the 64-economy bundle merged from the base was exported without stamps.

## Remaining technical debt

1. **Pre-engine pipeline code (scripts/10–25) is not fingerprinted.** Its effect shows as changed engine inputs, so a
   `real_world` pair can still include upstream processing changes. The raw-input stamp shows whether source files
   changed.
2. **The ledger depends on the traced readers.** A reader added later (for example `arrow`, `jsonlite::read_json` or
   `haven`) would be missed; add it to `CHANGE_LEDGER_TRACES`. The `upstream` list must track scripts/26's `source()`
   list by hand.
3. **The provenance map is curated** (`config/change_layer.yml` sources). It names likely inputs; it is not computed
   lineage.
   - The ledger now holds the real input files per vintage.
   - Next step: link changed fields to the changed ledger files.
4. **The evidence basis exists from vintage 2026-11-26-576eadf8 on.** Older vintages read `n/a`.
5. **Bridges are produced by hand.** `OPSI_BRIDGE` and then `scripts/45 --bridge`. Nothing schedules one when a pair
   lands as `mixed`, and stale bridges are kept but ignored.
6. **The input store prunes only on request** (`--prune-store`), and it lives in one checkout. A shared location (for example SharePoint, or an object store) would let any checkout bridge.
7. **Budget and size.** No dedicated `check-budget.mjs` line. The bundle repeats keys per record (about 4 MB raw,
   150 KB gzipped); a columnar encoding would cut it by about 5×.
8. **Scope.** The Sector, Partnership and Compact cards and the Country "Bottom line" do not yet show changes.

## Recommended next slices

1. **(done in slice 5) The Intelligence bridge.**
   - NEIS Intelligence migration `010_analytical_impacts.sql` and `metadata.iso3` on geography terms.
   - A rule-based proposer over `config/neis_intelligence_crosswalk.yml`, plus a review queue mirroring
     `editorial_reviews`.
   - An export of accepted rows to OSI.
   - A read-back of "contributed to a material change".
2. **(done in slice 6) Field-level provenance and a columnar bundle.**
   - Join each change record's moved fields to the ledger files that changed between its two vintages.
   - Switch the bundle to the posture core's columnar encoding and add a budget line.
3. **Change cards beyond the cell drawer.**
   - Add "what changed" lines to the Sector strategy card, the Country "Bottom line", the Partnership case and the Compact answer.
   - Add the landing page's "recommendations changed" panel, fed only by attributable pairs.

# Task 7: what an EconVitals-style NEIS landing page needs

The landing page should be built only on the change layer and on existing clean data.

| Question | Exists now | Missing |
|---|---|---|
| Which AIB recommendations changed recently? | **Yes.** The change bundle (`types` contains `recommendation`, with attribution); stamped exports are attributable from now on | One stamped data refresh, so the panel can lead with attributable changes |
| Where are dependencies worsening? | Need and import share per cell; T12M import exposure (scripts/37–38) | A *dated* series: Need deltas across vintages are confounded by method changes until stamps exist. Monthly import exposure is the cleaner near-term signal |
| Where is strategic capability improving? | Position (cp) and export share per cell; monthly trade momentum | Same attribution caveat. Momentum (`mom`, `mq`) is a ready per-cell trend |
| Where are policy commitments failing to translate into delivery? | Industrial Policy Strength module (lab); NEIS Intelligence `implementation_status` (announced → operational, cancelled, delayed) | A join of policies or projects to cells. Neither repo has project or policy entities; this needs the Intelligence bridge plus a project entity |
| Which partner relationships are becoming more complementary? | Partnership dyads and memos (static) | Dyad-level vintages: the same snapshot pattern applied to partnership outputs |
| What decision windows are approaching? | Nothing structured | A dated-event entity (policy deadlines, tariff dates, e.g. the Dec 4 snapshot) in NEIS Intelligence |

**Verdict:** only the "recommendations changed" panel has clean data today. The stamp-and-automate slice is in, so
the landing page can follow the first stamped data refresh. Start with that panel plus momentum, so the page does
not headline unattributable history.
