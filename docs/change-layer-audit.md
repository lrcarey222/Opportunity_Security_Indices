# Change layer: audit of the current system (2026-11-25)

The question behind this audit: can NEIS results be compared over time ("what changed, and why")?
Right now they cannot. Each export overwrites the last one.

**Branch audited:** `feat/change-layer`. It was cut from `origin/feat/explorer-usability-pass` (1135534c), which is
`main` (00afcda9) plus the conclusion-first usability pass. That makes it the latest intended architecture.
`feat/aib-explorer-redesign` (osi-worktrees/redesign) is older and is not merged anywhere. Its four ways in were
carried forward and refined by the usability pass, so this audit does not build on it directly.

**NEIS Intelligence audited:** `neis-intelligence` at `feat/month-slide` (c4b5c1e). That checkout has uncommitted
work and was read only.

## 1. The AIB analytical unit already exists: the posture cell

| | |
|---|---|
| Key | `iso3 × tech × supply_chain` (214 economies × 11 techs × 6 stages = 14,124 cells) |
| Built by | `scripts/26_build_strategic_position.R` → `R/strategic_position/decision_engine.R` (`sp_decision_choice`) with the Secure override from `posture_engine.R` |
| Stored | `outputs/strategic_position_tbl.rds` (git-ignored, overwritten each run) |
| Shipped | `dashboard/public/data/posture/posture-core.<md5-8>.json` (2.6 MB, columnar, 3-dp) plus 214 `posture-detail-<ISO3>.<md5-8>.json`, exported by `scripts/91_export_posture_data.R` and `R/dashboard/posture/posture_export.R` |
| Typed by | `dashboard/src/posture/contract.ts` (`PostureCell`, `PostureCore`, `decodeCore`) |

How the prompt's vocabulary maps onto the fields that actually exist:

| Prompt concept | Field(s) | Values |
|---|---|---|
| Need | `supply_chain_need` (producing stages), `deployment_need` (Deployment); `need_type` | 0–1 percentile; bar 0.67 (deployment 0.6) |
| Prize | `choice_prize` (= `strategic_industrial_value`, or `industrial_opportunity` when `prize_measure = market`) | 0–1; bar 0.6 |
| Position | `cp_contender_pct`; `pathway_credible` | 0–1; bar 0.75; yes/no/unknown |
| Choice / recommendation | `strategic_choice` | secure, preserve, catch_up, partner, accelerate, watch, not_assessed |
| Posture family | `strategic_posture` | secure, compete, deploy, not_assessed |
| Component scores | need, prize, cp, plus market (`industrial_opportunity`) and share (`home_share_exposure`) | |
| Evidence status | `stage_evidence_quality_cp` (shipped); `evidence_specificity` and `exact/split/shared/proxy/tech_wide` shares (rds only, `R/utils/variable_stage_evidence.R`) | high/medium/low/none; VSE statuses exact, split, shared, proxy, tech_wide, no_data |
| Confidence | `posture_confidence` | high, moderate, low |
| Borderline | `decision_borderline` | ";"-joined input names within 0.05 of a bar |

**The prompt's example verbs** (BUILD / PARTNER / SUSTAIN / OPEN / DIVERSIFY) are not the posture vocabulary.
Some of them do exist elsewhere:

- BUILD and COORDINATE are AIB *node* classes in `aib/contract.ts`. They are per HS node, not per country.
- `deploy_diversify` is a `posture_variant`.

The change layer keys on `strategic_choice`, the real recommendation. The labels come from config, so no vocabulary
is hard-coded.

## 2. Version and provenance metadata

What exists today:

- **Posture manifest, core and details** carry `created` and `git_commit`. The commit gets a `-dirty` suffix when
  `R/`, `scripts/` or `config/` had uncommitted edits.
- **Core meta** carries the trade windows `benchmark_period`, `momentum_period` and `momentum_through`, and the
  decision `thresholds`.
- **AIB manifest** has `version: 3` and `git_commit`. The Sector and root bundles carry `bundle_id` and `generator`.
- **`METHOD_VERSION`** is a hard-coded UI string in `shell/ReleaseStamp.tsx` (`'decision framework (2026-10-30)'`).
- **QC release ID** is `rel-<sha256(stamps)[:12]>` (`review/release.ts`).
- **Raw inputs:** `config/raw_inputs_manifest.yml` gives each raw input an `id`, `resolve` rule and free-text
  `vintage`. `write_resolved_vintages()` (`scripts/utils/raw_inputs.R`) writes `raw_inputs_resolved.yml` with
  file, vintage key, mtime and reporter count. **None of this reaches a shipped bundle.**

What is missing:

- No config hash.
- No `methodology_version` field in any bundle.
- No version key in `config/strategic_position.yml`.
- No per-cell provenance to raw inputs.

## 3. History and diffs that already exist

- **`R/qc/qc_drift.R`** is a deterministic two-release comparison, written as pure functions:
  - `qc_posture_core_to_table` turns a shipped core into a table.
  - `qc_drift_join` and `qc_drift_cells` find choice changes, moved inputs and threshold crossings.
  - It is wired to `scripts/qc/run_data_qc.R --previous-table`.
  - It is the right base to reuse. It does not classify change *types*, attribute methodology versus data, or ship
    anything to the UI.
- **`scripts/26` with `SP_BASELINE_TBL`** writes old-versus-new transition CSVs into `diagnostics/`.
- **Golden cases** (`config/qc/golden_cases.yml`, `R/qc/qc_golden.R`, `review/qc/golden.ts`) pin specific cells
  and warn on drift.
- **`scripts/40_build_index_vintages.R`** rebuilds the *old indices* at past years. That is not posture.
- **Git history is the only posture archive.** The posture bundle has been committed 10 times (2026-10-28 →
  2026-11-21). `scripts/91` deletes the previous files on every export. Those 10 cores are therefore real,
  recoverable vintages: the change layer can be seeded from history without rerunning anything.

## 4. The UI surfaces where a change would show

- **Per-cell conclusion:** `posture/CellDrawer.tsx`. `CellDrawer` puts the confidence line in its header and has a
  `ChoiceAnswer` block. It opens from any Country page with `?node=Tech|Stage`. This is where
  "Changed since last vintage" belongs.
- **Country "Bottom line":** `country/views/profile/BottomLine.tsx`.
- **Sector:** `sector/posture/SectorStrategyCard.tsx`.
- **Partnership:** `partnerships/StrategicCase.tsx`.
- **Compact:** `aib/derive/compactAnswer.ts`.
- **Advanced Explorer:** `/sector/explore/:theme?`.
- **Routes:** `dashboard/src/routes.tsx`. A top-level lazy route such as `how-to-use` is the pattern a `/changes`
  page should follow.
- **Confidence reading:** `posture/confidence.ts` (`confidenceSummary`, `confidenceSentence`) is already shared. It
  is the single source for "why this confidence".
- **Budgets:** the posture core is at its raw budget (2.4–3 MiB, `posture_export.R:17`) and has a gzip budget in
  `dashboard/scripts/check-budget.mjs`. Change data must ship as **its own file**, not as new columns in the core.

## 5. QC and peer review

`review/model.ts` covers:

- case kinds, including `posture_cell` keyed by iso3, tech and stage;
- a `CaseSnapshot` with `snapshot_sha256` and bundle stamps;
- an `Adjudication` with statuses open, confirmed, rejected, duplicate, resolved and wont_fix.

Submissions go to a write-only sink, so no server-side state is read back. The adjudication status set and the case
key are the pattern to mirror for reviewing proposed event impacts. Adjudication itself is tied to a
`submission_id`, so impacts need their own small record kind.

## 6. NEIS Intelligence

**Item model.** `events` (`migrations/001_init.sql:333`) holds every item: developments, reports, analysis,
commentary and data releases. Key fields:

- `factual_summary` (WHAT HAPPENED), `why_it_matters` and `what_changed`. These are deliberately separate columns
  so that fact and interpretation render differently.
- `synthesis_method`: extractive, model, editorial or NULL.
- `implementation_status`: rumor → operational, plus cancelled, delayed and superseded.

**Provenance and review patterns:**

- `assigned_by` (ai, editor, seed, rule) plus `confidence` on taxonomy, framework and relationship joins. Editor
  tags are never overwritten by model passes.
- `editorial_reviews` (open, resolved, dismissed), and `audit_log` with before/after JSON.

**Taxonomy.** Generic `taxonomies` and `taxonomy_terms` tables (slug, parent, `metadata jsonb`). Vocabularies
include geography, sector, `value_chain_node`, signal_type, policy_instrument and theme. Frameworks are versioned.

**Entities.** `entities` covers company, government_body and similar. `country` is free text.

**What NEIS Intelligence lacks:**

- No project, policy, partnership or agreement entity tables. Those exist only as signal_type terms or as events.
- No ISO codes. Geography slugs mix countries and regions (`japan`, `southeast-asia`, `european-union`).

**Database:** Postgres (PGlite in dev). Migrations are `NNN_*.sql`; the next free number is `010`.

## 7. Shared schemas and IDs between the two projects

**None.** Neither repo references the other. The nearest correspondences:

| NEIS Intelligence | AIB |
|---|---|
| `geography` slug (`japan`) | ISO3 (`JPN`) — no mapping exists; regions have no 1:1 mapping |
| `sector` slug (`batteries`, `grid`, `solar`, `wind`, `electric-vehicles`, `critical-minerals`, `nuclear-energy`, `hydrogen`, `geothermal`) | `tech` label (`Batteries`, `Electric Grid`, `Solar`, …) |
| `value_chain_node` (`extraction`, `processing`, `refining`, `materials`, `components`, `manufacturing`, `assembly`, `deployment`, `recycling`) | `supply_chain` stage (Extraction, Refining, Components, Assembly, Deployment, Recovery) |
| `risk-opportunity` framework (secure, upgrade, watch, scale) | posture families; not the same framework |

## 8. Conclusions for the design

1. **No architectural blocker.** The posture cell is a clean, keyed, typed unit. Build the vertical slice on it.
2. **Snapshots should be normalized cell tables derived from the shipped core.**
   - The core is already a compact, content-hashed artifact, retrievable from git by commit.
   - Raw inputs are hundreds of MB and must not be copied.
3. **The methodology fingerprint has to be computed.** Nothing stamps it today. Use git blob and tree ids of the
   engine paths at the bundle's commit.
4. **Reuse `qc_drift.R`.**
   - Use its core decoder.
   - Generalize its standard columns with confidence and evidence.
   - Keep drift as a QC report. The change layer adds typed, attributed, explained records and a UI bundle.
5. **The Intelligence bridge has to start with a crosswalk** (slug → ISO3, tech, stage) and a human-reviewed
   impact record. No table in either project can hold such a link today.
