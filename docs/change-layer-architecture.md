# Change layer: minimum viable architecture

This layer makes NEIS assessments *longitudinal* rather than overwrite-in-place. It answers four questions:

- What is the assessment now?
- What was it before?
- What changed, and is the change real-world or methodological?
- Which inputs (and, later, which Intelligence events) explain it?

Read with `docs/change-layer-audit.md`, which records the current system this design builds on.

## Names used here

The prompt's concepts map onto names that fit the codebase as follows:

| Prompt concept | Name here | What it is |
|---|---|---|
| assessment_snapshot | **posture vintage** | One dated, normalized table of posture cells (`iso3 × tech × stage`), plus a header |
| assessment_change | **posture change** | One record per cell per vintage pair, holding the cell's typed, attributed changes |
| methodology_version | **method fingerprint** (`method_id`) | A hash of the engine's code and config at the vintage's commit, plus the human label |
| input-data vintage | **data stamp** (`data_id`) | The trade windows recorded in the core, plus resolved raw-input vintages when known |
| evidence_reference | **source reference** | A changed field → `raw_inputs_manifest` source IDs and method docs. Accepted Intelligence impacts attach here later |
| potential_analytical_impact | **analytical impact** | A proposed or reviewed link from an Intelligence event to an assessment |

The analytical object ID used across both projects is

```
aib:posture:<ISO3>:<tech-slug>:<stage-slug>      e.g. aib:posture:USA:electric-grid:components
```

It is lower-case kebab apart from ISO3. It is stable across vintages because the key fields are stable.

## 1. What is versioned

**A posture vintage** is stored as `data/vintages/posture/<vintage_id>.csv.gz`. Each row is one cell.

| Column | Source field |
|---|---|
| iso3, tech, stage | key |
| choice | `strategic_choice` |
| posture | `strategic_posture` |
| variant | `posture_variant` |
| need_type, need | relevant need: supply-chain or deployment |
| prize | the prize the rule read |
| cp | `cp_contender_pct` |
| market | `industrial_opportunity` |
| share | measured export share |
| confidence | `posture_confidence` |
| evidence | `stage_evidence_quality_cp` |
| borderline | `decision_borderline` |
| pathway | `pathway_credible` |
| partner1 | `recommended_partner_1` |
| deploy_case | `deploy_case` |

Each vintage has a header row in `data/vintages/posture/index.json`:

```json
{ "vintage_id": "2026-11-21-07b6f4bd",
  "created": "2026-11-21T17:46:30Z",
  "git_commit": "acbc3805", "commit_dirty": false, "stamp_source": "bundle",
  "core_file": "dashboard/public/data/posture/posture-core.07b6f4bd.json", "core_commit": "3c40c9d6",
  "method": { "id": "m-1a2b3c4d", "engine": "m-...", "upstream": "m-...", "presentation": "m-...",
              "label": "decision framework (2026-10-30)", "unverified": false, "source": "manifest" },
  "data":   { "id": "d-5e6f7a8b", "benchmark_period": "2022-2024", "momentum_through": "Dec 2025",
              "verified": true, "inputs_id": "i-9c0d1e2f", "n_inputs": 41, "n_raw_inputs": 16 },
  "techs": [...], "stages": [...], "n_cells": 14124, "sha256": "<of the csv>" }
```

What is **not** versioned: raw data, intermediate rds files, and the detail bundles. They are referenced by commit
and by source IDs instead. The vintage is *derived analysis*. It is kept separate from source data by construction:
it holds no raw values, only engine outputs and their stamps.

## 2. When a new vintage is created

**Every time a posture bundle is exported and committed.** `scripts/45_build_posture_vintages.R` does the work:

- It walks the git history of `dashboard/public/data/posture/manifest.json`.
- For each commit, it reads the core file the manifest names.
- It dedupes by core content hash. A re-export with byte-identical output is not a new vintage.
- It records each new core as a vintage.
- `--include-worktree` adds the uncommitted working-tree core as a provisional vintage, tagged `provisional`.
  This lets an analyst preview a rerun's changes before committing.

The script is idempotent: rerunning it produces the same index.

**On export (since the stamp-and-automate slice).** After writing the posture bundle to its default location,
scripts/91 runs `scripts/45 --include-worktree`.

- The new core becomes a *provisional* vintage: archived and compared, and flagged until it is committed.
- Once committed, the next run finds it in history and rewrites its header from there.
- `--no-vintages` skips the step.
- Archived committed vintages are reused from `data/vintages/posture/` rather than re-parsed, so the step takes
  about 35 s.
- The CI guard `dashboard/src/changes/__tests__/stale.test.ts` fails when the shipped change bundle's latest vintage
  is not the shipped posture core.

## 3. What counts as a meaningful change

All rules are deterministic and live in `config/change_layer.yml`. **No LLM takes part in detection.** For each cell
present in both vintages of a comparable pair:

| Type | Rule (defaults) | Severity |
|---|---|---|
| `recommendation` | `choice` differs. `subtype`: `posture` when the family changed (e.g. compete → deploy), `mode` when it changed within a family (partner → catch_up), `coverage` when entering or leaving `not_assessed` | major |
| `score` | \|Δ\| ≥ 0.05 on need, prize or cp (the 0.05 borderline band), **or** the value crossed its decision bar (need 0.67, deployment 0.6, prize 0.6, cp 0.75; read from the vintage's own core meta) | moderate if a bar was crossed or \|Δ\| ≥ 0.10, else minor |
| `data` | An underlying measured input moved materially: share (\|Δ\| ≥ 0.002 and ≥ 20% relative) or market (\|Δ\| ≥ 0.05) | minor |
| `evidence` | The evidence band changed (none < low < medium < high). `direction`: improved or worsened | minor; moderate if it moved two or more bands |
| `confidence` | `posture_confidence` changed (low < moderate < high). `direction` | moderate |
| `methodology` | Pair-level: the method fingerprint differs. Every change record in such a pair is attributed (section 4) | — |

A cell with none of these types has **no meaningful change**. It is counted but not exported.

`magnitude` is the largest \|Δ\| among need, prize and cp, a number for sorting. `severity` is the larger of the
types' severities.

**Comparability.** A pair is comparable only if both vintages share the same tech set and stage set. A pair across
the 2026-10-28 four→six-stage rebuild is marked `comparable: false` with a reason, and its cells are not diffed.
The "previous comparable vintage" of V is the latest earlier vintage that is comparable with V.

## 4. How methodology changes are told apart from real-world changes

This is the critical distinction, so attribution is explicit and conservative.

1. **Method fingerprint** (`scripts/utils/change_stamps.R`).
   - The git blob id of every file under `method_paths`. It is taken from the working tree an export ran from (scripts/91 writes it into the posture manifest), or from git at the commit of an older, unstamped bundle.
   - Blob ids are used, not tree ids, so the two sources agree file by file. An export from uncommitted code is still fingerprinted exactly as it ran.
   - There are three sets:

     | Set | What it covers | Used for attribution? |
     |---|---|---|
     | `engine` | `R/strategic_position/`, `config/strategic_position.yml`, `R/utils/variable_stage_evidence.R`, `scripts/26` | yes |
     | `upstream` | the other R that scripts/26 sources: index builders, trade core, investment momentum, utils | yes |
     | `presentation` | `R/dashboard/posture/`, `scripts/91` | no — recorded only, because re-encoding a bundle does not change a result |

   - `method.id` is the hash over engine + upstream: the code the engine run executes.
   - An unstamped bundle whose `git_commit` is `-dirty` gets the flag `unverified`.
2. **Data stamp.** Two stamps are written per export, plus the windows:
   - **Engine input ledger.** scripts/26 traces every data file it reads (`readRDS`, `read.csv`, `fread`, `read_csv`, `read_excel`) and writes `outputs/strategic_position_inputs.json` (path → md5). scripts/91 stamps its hash as `data.inputs_id`. The posture manifest carries the full map, retrievable by commit.
   - **Raw-input vintages.** `raw_inputs_resolved.yml` from scripts/10 (id → vintage key), hashed into `data.id` with the trade windows. `data.verified` is true when this stamp is present.
3. **How the data of two vintages relate** (`change_data_relation`):
   - `same`: the engine input ledgers are identical, so the engine read byte-identical data. Failing that, both raw-input stamps are verified and agree.
   - `changed`: the ledgers differ, the raw-input stamps differ, or the windows differ.
   - `unknown`: no stamp could tell. This holds for every bundle exported before 2026-11-25.
4. **Attribution of each change record:**

| Method fingerprint (engine + upstream) | Bridge vintage | Data relation | Attribution |
|---|---|---|---|
| same | — | any | `real_world`: same code, so the change came from the engine's inputs |
| differs | given | — | **decomposed**: diff(prev → bridge) is `methodology`; diff(bridge → current) is `real_world` |
| differs | none | `same` | `methodology`: exact, because identical inputs went through different code |
| differs | none | `changed` | `mixed`: cannot be split without a bridge |
| differs | none | `unknown` | `mixed` if the cell's own share or market moved, else `unattributed`, never `methodology` |
| unverified (dirty, unstamped) | — | — | the above, with `method_unverified = true` |

**Why `unattributed` exists.**

- The trade windows (`benchmark_period`, `momentum_through`) were identical in all nine historical cores. They were identical even across the 2026-11-17 "current trade" refresh and the Atlas 2024 rebuild.
- A window-only stamp therefore cannot see a data refresh.
- Crediting a change to the method on that basis would confuse methodology with the world.

**Why the ledger and not only raw-input vintages.** `raw_inputs_resolved.yml` covers only the pattern-resolved inputs of scripts/10. The Comtrade refreshes the engine reads (scripts/94, 37–38, 99) are not in it. The ledger sits at the engine's real boundary, so it catches them.

**What the ledger cannot say.** When a `real_world` pair's inputs changed, that change may come from new source data or from pipeline code that ran *before* scripts/26 (scripts/10–25).

- That earlier code is outside `method_paths` on purpose. Including it would make almost every pair a methodology pair.
- Its effect reaches the engine as changed input files.
- The raw-input stamp shows whether any source file changed too.

A **bridge vintage** is the *new* method run on the *previous* inputs. It is the counterfactual that makes the split exact. `posture_compare()` accepts one now. Producing one requires the engine's input tables for the earlier vintage, which is the next slice.

## 5. How earlier vintages stay reproducible

- **The output** is exactly reproducible. Each vintage records `core_commit` and `core_file`, so
  `git show <core_commit>:<core_file>` returns the original bytes, and the vintage CSV is a pure function of them
  (`posture_vintage_from_core`). The index stores the CSV's `sha256`.
- **The computation** is reproducible as far as the code goes: `git_commit` pins code and config. Inputs are pinned
  only once `raw_inputs_resolved.yml` is stored with the vintage. That is captured going forward (`data.raw_inputs`)
  and is `null` for the ten historical vintages.

## 6. Storage

- One vintage is about 14k rows × 18 columns. Gzipped that is about 150–250 KB. Ten vintages come to about 2 MB in
  git. Raw inputs are never copied.
- The dashboard bundle `dashboard/public/data/changes/changes.<md5-8>.json` is separate from the posture core, so the
  core budget is untouched. It holds:
  - the vintage list;
  - every pair's summary;
  - *meaningful* change records only, for the last `bundle.detail_pairs` (4) comparable pairs.
- The first build holds 3,799 records: 4.1 MB raw, 150 KB gzipped. It is fetched lazily, only by the drawer block and
  `/changes`.
- Every record for every pair goes to the git-ignored `outputs/change_layer/posture_changes.csv` (19 MB for the
  history). It is derivable from the vintages plus config, so it is not committed.
- A wholesale rebuild changes almost every cell. The 2026-10-28 → 10-30 Yards & Fences rebuild changed all 14,124,
  which is why older pairs ship as summaries.

## 7. Backwards compatibility

- **Purely additive:** a new config, R module, script, data directory and dashboard file, plus one optional block
  in `CellDrawer`, one lazy route and one nav link.
- No existing bundle, contract or test changes shape.
- When `changes/manifest.json` is absent (older builds, the artifact copy), the UI renders exactly as before. The
  loader treats a 404 as "no change history".
- `qc_drift.R` is untouched. The change layer reuses `qc_posture_core_to_table` and adds the columns drift lacks.

## Change record (the structured object)

```json
{ "id": "aib:posture:USA:electric-grid:components",
  "iso3": "USA", "tech": "Electric Grid", "stage": "Components",
  "prev_vintage": "2026-11-17-…", "cur_vintage": "2026-11-21-…",
  "types": ["recommendation", "score", "evidence"],
  "subtype": "posture", "severity": "major", "magnitude": 0.09,
  "attribution": "real_world",  // real_world | methodology | mixed | unattributed | both
  "method_changed": false, "method_unverified": false,
  "prev": { "choice": "partner", "posture": "compete", "need": 0.71, "prize": 0.62, "cp": 0.66,
            "market": 0.40, "share": 0.031, "confidence": "moderate", "evidence": "medium" },
  "cur":  { "choice": "catch_up", ... },
  "deltas": { "need": 0.0, "prize": 0.01, "cp": 0.09 },
  "crossed": [ { "field": "cp", "from": 0.66, "to": 0.75, "bar": 0.75, "direction": "up" } ],
  "evidence_direction": "improved", "confidence_direction": null,
  "sources": [ { "field": "cp", "source_ids": ["comtrade_energy_trade", "atlas_..."], "doc": "docs/strategic_position.md" } ],
  "impacts": [],
  "explanation": "Recommendation changed from Partner to Catch Up. Position rose 0.66 → 0.75, crossing the 0.75 bar. Evidence improved from medium to high. Same method in both vintages, so this reflects new data." }
```

The `explanation` is filled from a deterministic template (`posture_change_explain`). A later LLM pass may
*rephrase* it from the structured record, but the structured fields stay the source of truth. Model prose is stored
in a separate field and never fed back into detection.

## Intelligence bridge: the analytical impact

```
event ─(taxonomy: geography, sector, value_chain_node)─► crosswalk ─► candidate assessment IDs
      ─► analytical impact (proposed) ─► human review ─► accepted ─► source reference on the next vintage's change
```

**Crosswalk** (`config/neis_intelligence_crosswalk.yml`, in OSI because AIB keys are defined here):

- geography slug → ISO3 (regions → ISO3 lists, or `null` for global);
- sector slug → AIB tech(s);
- value_chain_node slug → AIB stage(s).

`posture_impact_candidates()` turns an event's tags into candidate IDs. It is a pure Cartesian product, filtered to
cells that exist and are assessed.

**Impact record** (`schemas/analytical_impact.schema.json`; stored as human-curated YAML at
`config/review/analytical_impacts.yml` until NEIS Intelligence owns it):

| Field | Meaning |
|---|---|
| `impact_id` | `imp-<event_id[:8]>-<hash8>` |
| `event_id`, `event_slug` | NEIS Intelligence `events.id` and slug |
| `assessment_id` | `aib:posture:…` |
| `dimension` | need, prize, position, evidence, confidence or recommendation |
| `direction` | up, down or unclear |
| `reason` | Short text; may be model-drafted |
| `reason_method` | `rule`, `model` or `editorial`, mirroring `events.synthesis_method` |
| `confidence` | 0–1, the proposer's confidence that the link is relevant |
| `status` | `proposed`, `accepted` or `rejected` |
| `proposed_by`, `proposed_at` | |
| `reviewer`, `review_timestamp`, `review_note` | Required when the status is not `proposed` |

Rules:

- Only `accepted` impacts whose `review_timestamp` falls between two vintages attach to that pair's change record
  for the assessment, under `impacts`. An accepted impact *explains*; it never *causes*. Scores move only when the
  pipeline recomputes from data.
- `proposed` and `rejected` impacts never appear in the UI.

**NEIS Intelligence side.** This is a design only; it is not implemented here because that checkout carries other
uncommitted work. The table would be:

```sql
-- migrations/010_analytical_impacts.sql
CREATE TABLE analytical_impacts (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id uuid NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  assessment_id text NOT NULL CHECK (assessment_id ~ '^aib:posture:[A-Z]{3}:[a-z0-9-]+:[a-z]+$'),
  dimension text NOT NULL CHECK (dimension IN ('need','prize','position','evidence','confidence','recommendation')),
  direction text NOT NULL DEFAULT 'unclear' CHECK (direction IN ('up','down','unclear')),
  reason text NOT NULL,
  reason_method text NOT NULL CHECK (reason_method IN ('rule','model','editorial')),
  confidence real CHECK (confidence BETWEEN 0 AND 1),
  status text NOT NULL DEFAULT 'proposed' CHECK (status IN ('proposed','accepted','rejected')),
  assigned_by text NOT NULL CHECK (assigned_by IN ('ai','rule','editor')),
  reviewer text, review_timestamp timestamptz, review_note text,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (event_id, assessment_id, dimension),
  CHECK (status = 'proposed' OR (reviewer IS NOT NULL AND review_timestamp IS NOT NULL))
);
```

It mirrors the existing patterns: `assigned_by` and `confidence` as on `event_taxonomy_terms`, `editorial_reviews`
for the queue, and `audit_log` for status flips. Geography terms gain `metadata.iso3` (no schema change, since
`taxonomy_terms.metadata` is jsonb). The export to OSI is the accepted rows as YAML or JSON in the shape above.

The return path ("this event contributed to a material change in our assessment") is a read of OSI's change bundle,
joined on `event_id`. NEIS Intelligence never copies AIB scores.
