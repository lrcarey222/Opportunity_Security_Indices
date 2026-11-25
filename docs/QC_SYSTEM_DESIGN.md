# AIB Explorer: Quality Control, Validation and Expert Peer Review system

Status: design, written before implementation (2026-11-20). Branch `feat/qc-review`, cut from
`origin/main` a5b22881: the published AIB Explorer (Sectors / Countries / Allied Industrial Base / About),
whose data bundles are the ones served by the public GitHub Pages site (posture bundle: see 1.3).

Note on the audit notes in `docs/qc_audit/`: they were written against the unpushed redesign branch,
which shares main's R pipeline and bundle formats but not its page layout, and whose bundles were the
2026-10-31 export. Methodology, code references and bundle *formats* carry over; page names, routes and
bundle *numbers* do not. Section 1 below states main's facts.

The purpose of this system is to find where the Explorer is wrong. It is a validation harness, not a
satisfaction survey: every piece of it is built to make disagreement specific, traceable and
attributable to data, method, missing context, framework ambiguity or actionability.

---

## 1. What exists today (audit, 2026-11-20)

### 1.1 Application

- React 19 + Vite + TypeScript + Tailwind v4 static app in `dashboard/`. Routes in `dashboard/src/routes.tsx`;
  each module has its own `routes.tsx` with lazy chunks (`view(load)` helper) and `errorElement: <RouteError/>`.
- Navigation (`shell/modules.ts`): **Sectors** (`/sector`, `/sector/:tech/:stage[/:iso3]`, minerals,
  advanced explorer), **Countries** (`/countries`, `/countries/:iso3` profile with the posture strategy map
  and cell drawer), **Allied Industrial Base** (`/aib` overview, `/aib/partnerships`, `/compacts` and
  `/compacts/s/:sid` Sectoral Compacts, `/compacts/:id` operational compacts, `/aib/roles`,
  `/aib/diversification`), **About** (`/methods`, `/data`, `/glossary`); plus the legacy Strategic views
  and Chart of the Day. Partnership and compact memos are published on Pages as static documents
  (`memos.html`, `data/partnerships/`), outside the app.
- Hosting: static. The public site is published by hand to GitHub Pages (separate repository
  `lrcarey222/aib-explorer`) from main; a Netlify config with a strict CSP (`connect-src 'self'`,
  `form-action 'none'`) is also in the repo. **There is no backend anywhere.**
- Design system: tokens in `src/styles/globals.css` (`.panel`, `.card`, `.btn*`, `.field`, `.tag`,
  `.disclosure`, `.data-table`, `.eyebrow`, `.label`); React primitives in `src/components/ui.tsx`
  (`PageHead`, `Select`, `Segmented`, `Notice`, `Tag`, `Disclosure`, `StatTile`, ...). There is no
  textarea / form-field primitive. Posture colours and names: `posture/grammar.ts` (`VERB_COLOR`, `VERB_LONG`) and
  `content/vocabulary.ts` (`CHOICE_NAME`, `AIB_CLASS_NAME`, `ALIGNMENT_NAME`, ...).
- Runtime build stamps: `shell/buildInfo.ts` publishes only the Strategic and Sector bundle stamps.
  Posture, Country, AIB and memo bundles publish none; the app has no code SHA or build date.
- Storage: one `localStorage` use (a "seen" flag). No reviewer state anywhere.
- Feedback: none, apart from the artifact-preview notice.

### 1.2 Canonical analytical vocabulary (read from code, not assumed)

- Strategic choices (`R/strategic_position/decision_engine.R` `SP_CHOICES`; display
  `content/vocabulary.ts` `CHOICE_NAME`): **Secure, Preserve, Catch Up, Partner, Accelerate, Watch**, plus
  **Not assessed**. "Keep open" is the *action/fence* wording for Watch, never a choice name. Postures:
  Secure / Compete / Deploy. Yards and fences: Small/High (Secure), Medium/medium (Preserve, Catch Up,
  Partner, Accelerate), Large/Low (Watch).
- Framework steps: Need ("Strategic Need" / supply-chain vulnerability), Prize ("Strategic Industrial
  Value"), Position ("Competitive Position"), Choice, Policy alignment (after the choice; never feeds it).
- AIB node classes (`AIB_CLASS_NAME`): COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION,
  INSUFFICIENT DATA. Scenarios: Today's network, Managed Interdependence, Major-supplier disruption.
- Compact decision (`compact/decide.ts`): Sectoral Compact / Narrower arrangement / No compact needed.
- The review system reads all labels from these constants. Nothing is re-typed.

### 1.3 Data bundles (what reviewers actually see)

| Bundle | Stamp | Provenance carried |
|---|---|---|
| Posture (`public/data/posture`) | 2026-11-17T20:33, git `0496e7ba` (manifest only) | per-cell decision trace, need families, prize parts, CP families, pathway basis, trade benchmark source; **no indicator-level source or year** |
| Country | 2026-10-31, `4bb2adc`, `as_of` 2026-10-30 | `src_map`, source editions (legacy ES/EO corners only) |
| Sector | 2026-10-31, `4bb2adc` | 75-variable dictionary with source, data year, licence, transform |
| Strategic engine | 2026-10-30, `a54a9d3` | file md5s, 70 parity checks |
| AIB | 2026-11-17, **no commit** | thresholds, per-node variants, physical-data warnings |
| Partnership / compact memos | per memo, **no commit**, all `verified:false` | evidence register summarised, not shipped |

Release drift: **the live Pages site serves an earlier posture bundle** (2026-11-17T17:12, `8021770d`:
Preserve 103 / Catch Up 353 / Partner 1,478 / Watch 6,577) than main (`0496e7ba`: 111 / 351 / 1,475 /
6,574; regenerated with the innovation family, 6e077a8a). AIB, Country, Sector and Strategic manifests
are identical on main and Pages. The bundles come from four export dates and no release ID ties them
together.

### 1.4 Existing QC

- R: `validate_schema`, `assert_unique_keys`, `assert_country_iso3_consistent` (`R/utils/schema.R`),
  `posture_check_invariants`, `aib_check_*`, `sector_check_*`, `country_check_*`; export-time validation
  tables in scripts 85/87/88 (parity checks, coverage guards against Comtrade stub vintages).
- Diagnostics: `diagnostics/strategic_position/` (threshold sensitivity *counts*, large-yard sensitivity,
  consistency cells, paper reproduction, decision cards). No per-cell robustness class.
- Tests: 129 R test files (synthetic logic, schema, parity, 22 silently skip in CI without data);
  127 vitest files; `src/regression/golden.test.ts` snapshots 6 economies' outputs from the shipped
  bundles through the app's own derive functions (runs in CI).
- Gaps: no numeric-bounds checks on the posture/AIB layers, no systematic missingness report, no
  production-vs-trade plausibility test, entrepots other than HKG/MAC unhandled, no provenance
  completeness check, no release-to-release drift tool, no curated golden cases with rationales, no
  per-cell robustness classification, no source-to-conclusion audit export.

### 1.5 Problems the audit already surfaced (to be verified by the QC suite, not assumed)

Found on the redesign branch's 2026-10-31 bundles; each must be re-verified on main's bundles.

1. Published posture bundle differs from main's; between the 2026-10-31 and 2026-11-17 exports 207
   cells changed choice and CP was volatile (e.g. USA Nuclear/Assembly cp .173 → .904).
2. Threshold-sensitivity diagnostics ignore the need-floor cap (capped need stored as 0.61), so need
   0.60 flips 13 capped Watch cells: an artifact inside the reported counts.
3. Knife-edges: 432 cells carry prize exactly 0.59 (market-only cap); 165 cells sit exactly on a
   threshold; CP lands on exactly 0.75 in NEIS cells (DEU Solar/Assembly, IND EV/Assembly, IND
   Solar/Refining → Preserve by the inclusive rule); Electric Grid Assembly prize is 0.613 for every
   economy, so whole nodes flip together.
4. Entrepots: only HKG/MAC (AIB) and HKG (+MAC, MHL, LBR, PAN for need) are handled. HKG re-enters CP via
   the Atlas fallback; SGP Grid Components CP .95; BEL Oil/Extraction is Secure with zero production.
5. Trade vs production: USA Batteries/Components share .301 vs trade benchmark share .137 (basis
   unclear). Physical data constrain only 11 AIB nodes.
6. Shared baskets: Batteries/EV Extraction identical inputs; JPN EV/Refining Secure but Batteries/Refining
   Catch Up on the same import mix.
7. Five coexisting "trusted partner" definitions (coalition tiers, global candidates, bloc host screen,
   allied set + TWN/CHE, memo coercive list).
8. Implausible AIB compact cores (USA as "Resource supplier" in SC-BATTERIES; SGP core in SC-SEMIS; THA
   in SC-MAGNETS). Zero-gain rank-1 host (PNG, balsa).
9. TS fallback `DEFAULT_THRESHOLDS.deployment = 0.67` vs 0.6 everywhere else.
10. HS 281290 (NF3/SF6) still filed in three cabling sub-sectors at Refining.
11. Licence drift: IEA WEO marked "attribution" in the sector dictionary, CC BY-NC-SA in the raw manifest.
12. Doc/data drift in the AIB methodology (57 vs 59 economies, 78 vs 77 nodes, v2 vs v3 schema,
    stale "hosts all tie" and "China rank-1 host" claims).

---

## 2. What is reused

| Need | Reused from |
|---|---|
| Choice labels, colours, fences | `content/vocabulary.ts`, `posture/grammar.ts` |
| Bundle decoding (browser and Node) | `posture/store.ts`, `posture/contract.ts` (`decodeCore`, `checkPostureDetail`), `aib/data/store.ts`, `conclusion/memos.ts`, `engine/codec.ts`; Node imports the TS sources with `--experimental-strip-types` |
| Decision trace for "Why this result?" | posture detail `trace`, `prize`, `cp_families`, need families, `pivotal`, `choice_possible`; `CellDrawer` |
| Source dictionary | `config/sector_dictionary.yml` sources, `config/raw_inputs_manifest.yml`, `config/strategic_position.yml` family recipes |
| Before/after for threat amelioration | AIB `nodes[]` (baseline vs optimized HHI, largest share, residual, N-1, diversion, frontier, variants), `diversify`, compact `metrics`, `strategic.compacts[].scenarios` |
| Canonical sensitivity engine | `sp_choice_rerun`, `sp_choice_threshold_sensitivity`, `sp_large_yard_case`, `sp_strategic_value`, `sp_supply_chain_need` (pure R) |
| Exports | `lib/csv.ts`, `lib/download.ts`, `lib/zip.ts` |
| Golden guard pattern | `src/regression/golden.test.ts` |
| R validators | `R/utils/schema.R`, `posture_check_invariants` |

---

## 3. Architecture

```
config/review/                       authored by NEIS (YAML)
  campaigns/<campaign_id>.yml        campaign definition
  review_cases.yml                   case catalogue (stable IDs)
  method_cards.yml                   methodology red-team cards
  framework_scenarios.yml            comprehension scenarios
config/qc/golden_cases.yml           golden regression cases

dashboard/scripts/review/            Node CLIs (IO only; logic imported from src/review)
  build-review-data.mjs              YAML + shipped bundles -> public/data/review/*.json (snapshots, hashes)
  export-review.mjs                  submissions dir -> qc_*.csv exports
  run-bundle-qc.mjs                  automated QC + sensitivity on the shipped bundles -> qc/results/*.csv

dashboard/public/data/review/        built, public, non-private
  manifest.json                      campaigns, data release, build stamps
  cases.<campaign_id>.json           case snapshots (exact output a reviewer saw)
  method_cards.json, framework_scenarios.json
  fixtures/demo_submissions.json     clearly synthetic demo data

dashboard/src/review/                the reviewer app and its pure logic
  model.ts manifest.ts assign.ts flow.ts storage.ts sink.ts context.ts exports.ts aggregate.ts privacy.ts
  qc/ choiceRule.ts sensitivity.ts bundleQc.ts threat.ts provenance.ts golden.ts
  components/ (IssueReportDialog, ReportIssueButton, WhyThisResult, form fields)
  views/ (ReviewHome, Profile, CaseReview, MethodCards, Framework, DecisionUse, Threat, BreakModel, Admin)

R/qc/*.R  (pure)   scripts/qc/*.R (IO)   tests/testthat/test-qc-*.R
  pipeline-level data QC, trade-vs-production, drift, golden cases on processed tables,
  source-to-conclusion audits, canonical sensitivity, validation report

qc/results/   latest automated QC outputs (CSV, committed, small)
qc/audits/    <case_id>.json / .md end-to-end audits (committed)
qc/private/   submissions and reviewer data (git-ignored; never committed)
reports/AIB_VALIDATION_REPORT.md
```

Two QC layers, kept distinct and both reported:

1. **Shipped-bundle layer (TypeScript/Node).** Checks exactly what reviewers and users see. Runs in CI
   (vitest) because `public/data` is committed. Re-runs the choice rule from the shipped decision trace
   (a reproducibility test in itself) and classifies robustness per cell.
2. **Pipeline layer (R).** Checks the processed tables the bundles are exported from (pass
   `--processed-dir`; default from `config/config.yml` `processed_dir`). Runs the canonical sensitivity
   engine, builds source-to-conclusion audits, trade-vs-production diagnostics and release drift.

Every output carries the data release it describes, so the two layers cannot be confused.

### 3.1 Data release identity

A **data release ID** is computed from the manifests of every bundle a page reads:
`rel-<sha256(posture.created|posture.git_commit|country.created|sector.bundle_id|aib.created|strategic.bundle_id|memos manifest hash)[0:12]>`.
`build-review-data.mjs` writes it into `public/data/review/manifest.json`; the app computes the same ID
at runtime from `buildInfo` (extended to publish posture, country, AIB and memo stamps). The app code
SHA and build date are injected at build time (`import.meta.env.VITE_APP_COMMIT`,
`VITE_BUILD_DATE`; `unknown` in tests). Every submission records both.

---

## 4. Data model

All IDs are stable strings. Times are ISO-8601 UTC. TypeScript types live in `src/review/model.ts`;
the YAML files are validated against them by `src/review/manifest.ts` (build fails on an invalid file).

### 4.1 Campaign (`config/review/campaigns/<id>.yml`)

```yaml
campaign_id: aib_qc_2027_round1
name: AIB Explorer Expert Validation - Round 1
status: draft | active | closed
opens: 2027-01-15
closes: 2027-03-01
review_types: [case, methodology, framework, decision_use, threat, break_model]
case_ids: [CASE_001, ...]            # cases in scope (subset of the catalogue)
anchor_cases: [CASE_001, CASE_005]
target_reviews_per_anchor: 4
assignment:
  assigned_per_reviewer: {min: 2, max: 4}
  anchors_per_reviewer: 1
  out_of_specialty: optional          # none | optional | one
  allow_self_selected: true
  min_domain_confidence: 3            # 1-5 self-rating needed for an in-expertise match
method_card_ids: [MC_...]             # empty = all
framework_scenario_ids: [FS_...]      # empty = all
reviewers:                            # optional pre-registration (codes only, never names)
  - code: R-ABC123
    groups: [batteries]
```

### 4.2 Case (`config/review/review_cases.yml` → `cases.<campaign>.json`)

Authored fields: `case_id` (`CASE_###`), `kind` (`posture_cell | aib_node | compact | partnership |
sector`), `key` (e.g. `{iso3: KOR, tech: Batteries, stage: Components}`, `{node: rare-earth-metals,
scenario: decoupling}`, `{sid: SC-BATTERIES, scenario: managed}`, `{pair: USA_JPN}`), `title`,
`question` (what the reviewer is asked), `sector_groups` (expertise tags), `country_groups`,
`stage`, `assignment_group`, `anchor` (bool), `why_selected` (golden category, threshold case, etc.),
`explorer_path` (deep link), `context_fields` (facts shown before the blind answer),
`reveal_fields` (fields shown only after it).

Built fields (never hand-written): `snapshot` (the exact values: conclusion, component scores, trace,
indicators), `snapshot_sha256`, `bundle` (`{file, sha256, created, git_commit}` of every file read),
`data_release`, `app_commit` at build, `built_at`, and `answer_options` (the canonical labels for this
case kind, read from the vocabulary constants).

### 4.3 Reviewer profile (local; never shipped)

`reviewer_id` (random, generated client-side), optional `name`, `anonymous` (bool), `organization`,
`role`, `countries[]`, `sectors[]`, `stages[]`, `methods[]` (e.g. trade data, input-output, industrial
policy, security, finance), `confidence` per tag (1-5), `familiarity_neis` (0-3),
`familiarity_explorer` (0-3), `advised_on_topic` (`governments | firms | investors | none`, multi).

### 4.4 Submissions

Every submission is an envelope:

```
{ submission_id, schema: "aib-review-submission", schema_version: 1, campaign_id, kind,
  reviewer: { reviewer_id, name?, anonymous, profile_digest },
  created_at, submitted_at, app_commit, data_release, page_context?, payload }
```

`kind` and `payload`:

- `case_review`: `case_id`, `case_snapshot_sha256`, `assignment_reason` (`assigned | anchor | outside |
  self_selected`), `expertise_match` (`in | adjacent | outside`), **`blind`** `{ choice, confidence 1-5,
  factors[1..3], rationale?, answered_at }`, **`reveal`** `{ revealed_at, verdict 1-7, main_issue
  {text, type: data|method|context|framework|actionability|insufficient_evidence, metric_ids?},
  suggested_choice?, evidence_wanted? }`, `outside_expertise` (bool), `time_spent_s`.
  Verdict codes: 1 matches prior; 2 differs but persuaded; 3 plausible, evidence insufficient; 4 wrong:
  data; 5 wrong: method/weighting/threshold; 6 wrong: missing context; 7 not enough expertise.
- `method_feedback`: `card_id`, `stance` (`retain | modify | reject | unsure`), `materiality` 1-4,
  `alternative?`, `affected_outputs?`, `bias_direction` (`overstates_need | understates_need |
  favours_incumbents | favours_new_entrants | other | none | unsure`), `comment?`.
- `framework_answer`: `scenario_id`, `answer`, `intended_answer` (copied at submit), `correct` (bool),
  `confidence`, `confusing_terms[]`, `comment?`.
- `decision_use`: `case_id`, `would_do_differently`, `actionable` 1-4, `identifies_problem` 1-4,
  `separates_diagnosis_from_prescription` 1-4, `right_geography` 1-4, `evidence_required?`,
  `improvement?`, `decision_use?`.
- `threat_review`: `case_id`, the threat metrics shown (snapshot), `reduces_threat` 1-4,
  `scale_sufficient` 1-4, `timing_plausible` 1-4, `commercially_plausible` 1-4,
  `missing_constraints[]` (qualification, infrastructure, workforce, financing, demand, permitting,
  other), `success_looks_like?`, `stop_point?`.
- `data_issue` and `break_model`: `page_context` is required; `metric?` `{ id, label, value, display,
  country, tech, stage, source, source_year, transform }`, `what_is_wrong`, `corrected_value?`,
  `citation?`, `severity` (`cosmetic | minor | material | critical`), and for `break_model`
  `break_type` (`surprising | obviously_wrong | misleading_data | right_for_wrong_reason |
  threshold_artifact | omitted_variable`).
- Adjudication (admin only, separate record so reviewer submissions are immutable):
  `{ adjudication_id, submission_id, status: open|confirmed|rejected|duplicate|resolved|wont_fix,
  issue_type, confidence_real 0-1, affected_outputs (n or ids), note, decided_by, decided_at,
  resolution_commit? }`.

`page_context` (captured automatically): `url` (router location, not `window.location`, so it is right
in the artifact build), `route`, `view`, `country`, `tech`, `stage`, `node`, `layer`, `pair`, `sid`,
`scenario`, `displayed_result`, `metric_ids[]`, `data_release`, `bundle_stamps`, `app_commit`,
`captured_at`.

### 4.5 Golden case (`config/qc/golden_cases.yml`)

```yaml
- id: GC_001
  category: obvious_secure | keep_open | strong_producer | import_dependent | entrepot | sparse |
            near_threshold | neis_central | surprising | method_sensitive
  kind: posture_cell | aib_node | compact
  key: {iso3: JPN, tech: Electric Vehicles, stage: Refining}
  expect:
    choice: secure                     # canonical key; or choice_in: [preserve, catch_up]
    fence: high                        # optional
    numeric: {need: [0.95, 1.0]}       # accepted ranges
  exact: true                          # exact match required (else the allowed set/range)
  change_requires_review: true         # any change fails until the case is re-approved
  rationale: "..."
  approved: {by: NEIS, on: 2026-11-20, release: rel-...}
```

### 4.6 Robustness record (per cell)

`{ case_key, baseline_choice, specs_run, local_flips[], structural_flips[], fence_change (bool),
class: stable | sensitive | fragile | not_testable, drivers[] }`.

- *Local* specs: each decision threshold ± the borderline band (0.05) one at a time; strict vs
  inclusive comparison at the CP threshold.
- *Structural* specs (defensible alternatives that could change a policy conclusion): large-yard
  `min_signals` 4/6, `position_first` off, market-only gate 0.33 / Unknown counted 0.5, need floor
  0.05, prize threshold 0.65, deployment requires ≥2 families, need-floor-cap-aware need 0.60.
  AIB: release semantics, conservative supply, trade-only mode, china_50, growth variants.
- **Fragile** = at least one local spec changes the choice. **Sensitive** = no local spec changes it,
  at least one structural spec does. **Stable** = none does. **Not testable** = the inputs needed are
  not in the source (e.g. not assessed). `fence_change` is reported separately because a change of
  fence is more consequential than a change of mode within a fence.

Data quality, analytical robustness and expert validation are always reported as three separate
fields. They are never merged into one confidence score.

---

## 5. Reviewer workflow

`/review` (lazy route, own layout, not in the top navigation; the Explorer is unchanged for ordinary
visitors).

1. **Start** — campaign intro (what the exercise is, ~10-15 minutes per case, nothing is published,
   how submissions travel). Storage mode is shown plainly (Demo: stays in this browser / Packet: you
   download and send a file / Live: sent to NEIS's endpoint).
2. **Profile** — the short profile of 4.3. Self-ratings per tag. "Anonymous" toggle.
3. **Assignments** — `assign.ts` produces a deterministic list from the profile, the campaign and the
   reviewer's seed: 2-4 in-expertise cases (match = sector tag AND (country tag OR stage tag) with
   confidence ≥ `min_domain_confidence`), ≥1 anchor case (anchors with the fewest planned reviews
   first), optionally one adjacent/outside case, plus one optional self-selected case. A reviewer who
   only knows Korean batteries is never assigned methodology-wide cases; methodology cards are offered
   filtered by their stated method expertise, all others marked "outside your stated expertise"
   (still answerable).
4. **Case review (blind-first)** — context step (the facts in `context_fields`: import dependence,
   supplier mix, world share, market outlook; never the choice, fence, reason, or any field that
   reveals it) → blind step (posture choice from the canonical labels or "I don't know / outside my
   expertise", confidence, 1-3 driving factors from a structured list + free text, optional rationale)
   → **only then** reveal (Explorer conclusion, component scores, the decision trace, the "Why this
   result?" chain, robustness class, data-quality notes) → verdict 1-7 → the single most important
   miss (structured type + text; required when the verdict is 3-6) → optional decision-use and threat
   questions when the case has them. The reveal step cannot render before the blind answer is stored
   (state machine in `flow.ts`, tested).
5. **Methodology red team** — one card at a time: current choice, why, concrete example (with a link),
   alternatives, then retain/modify/reject, materiality, alternative, affected outputs, bias direction.
6. **Framework comprehension** — short scenarios; the reviewer picks what the framework would imply
   before seeing the intended interpretation; mismatches are recorded as framework ambiguity.
7. **Break the model** — available from every page through the review toolbar when a reviewer session
   is active: captures `page_context` automatically; the reviewer adds the break type, what is wrong,
   severity.
8. **Progress** — drafts autosave to `localStorage` (try/catch, in-memory fallback); a completion bar
   per assignment; resume from where they stopped.

**Report a data issue** — a `ReportIssueButton` placed on substantive data points (posture cell drawer
decision card and measure rows, AIB node class chips, compact and partnership conclusion sections) and
a page-level control. Visible only when a reviewer session is active or the build sets
`VITE_REVIEW_UI=1`, so the public Explorer does not show a control that leads nowhere.

**Why this result?** — a disclosure in the posture cell drawer (and on review reveal screens) tracing
choice → decision trace steps with thresholds → component scores → family values → raw shares where
shipped → transformation (from the `strategic_position.yml` recipe) → source and vintage (from
`sector_dictionary.yml` / manifests). Where a link in the chain is not shipped, the drawer says so
explicitly ("raw value not in this release") rather than hiding the gap.

---

## 6. Automated QC workflow

### 6.1 Shipped-bundle checks (`src/review/qc/bundleQc.ts`, vitest + `run-bundle-qc.mjs`)

- Schema: required columns in posture core, allowed choice/posture/mode/yard codes, unique
  (country, tech, stage), ISO3 format, 214 × 66 completeness, stage and tech vocabulary.
- Numeric validity: need, prize, cp, dn, shares in [0,1]; no NaN/Inf/malformed strings; HHI in (0,1];
  shares summing ≤ 1 + tol; prize exactly at the 0.59 cap counted and labelled.
- Classification logic: posture invariants (Compete has a mode, no Compete at Deployment, Accelerate only
  at Deployment, ...); every assessed cell's trace ends in its choice; **re-running the choice rule from
  the trace reproduces the shipped choice** (exceptions listed, never silently tolerated).
- Provenance: every bundle has created + git_commit; release ID computable; missing stamps flagged
  (AIB, memos, posture details lack commits today).
- AIB: classes allowed, coverage path monotone (static ≤ existing ≤ optimized ≤ 1), residual = 1 −
  optimized, rank-1 hosts with zero gain flagged.
- Golden cases (`golden.ts`): every `config/qc/golden_cases.yml` case evaluated; failures fail CI unless
  the case is re-approved.

### 6.2 Pipeline checks (R; `scripts/qc/run_data_qc.R --processed-dir <dir>`)

Schema, missingness (per indicator × stage × tech, sudden increases vs previous release), numeric
validity, trade consistency (duplicate dyads, mirror vs reported, shares reconcile, units), classification
logic, provenance completeness (raw manifest fields, licence consistency, stale vintages), trade vs
production (export share high but production/capacity evidence absent or inconsistent; entrepot
candidates by re-export signature: import/export ratio, absent production, known hubs), version drift
(two releases: ranking changes, classification transitions, score distribution shifts, AIB class and
compact membership changes) → `qc/results/*.csv` with a severity per row (`info | warn | fail`) and a
`needs_human_explanation` flag. Drift never fails automatically; it produces a review list.

### 6.3 Sensitivity

- TS (shipped bundle): local + structural specs that the shipped trace supports.
- R (canonical): `sp_choice_rerun` with the full structural set, need-floor-cap aware.
- Both write `qc_sensitivity_results.csv` with a `layer` column (`bundle | pipeline`).

### 6.4 Audits

`scripts/qc/build_case_audit.R --case CASE_005 --processed-dir ...` writes `qc/audits/<case_id>.json`
and `.md`: source datasets and rows, cleaning, transformations, normalized indicators, weights,
intermediate scores, threshold decisions, final classification, recommendation, plus the shipped
bundle's values for the same cell and any difference.

### 6.5 Reports and exports

- `export-review.mjs --submissions qc/private/submissions --out qc/private/exports` →
  `qc_review_responses.csv`, `qc_case_results.csv`, `qc_method_feedback.csv`, `qc_data_issues.csv`
  (+ framework, decision-use, threat). Reviewer names are dropped unless `--include-names`.
- `qc_sensitivity_results.csv`, `qc_golden_case_status.csv` come from the QC runs.
- `scripts/qc/write_validation_report.R` → `reports/AIB_VALIDATION_REPORT.md` (scope, reviewers,
  coverage, findings, strongest validations, unresolved disagreements, critical data issues, sensitive
  conclusions, methodological issues, changes made because of review, limitations). It states that
  expert results are structured elicitation, not representative polling.

### 6.6 Internal QC dashboard (`/review/admin`)

Runs entirely in the admin's browser. The admin loads submission files (or the demo fixtures); nothing
is uploaded and nothing private is shipped with the site. Views: Overview (cases reviewed, reviewers,
sector/country coverage, anchor coverage, unresolved critical issues), Agreement (model vs expert,
expert vs expert, blind prior vs post-reveal, persuaded despite differing), Disagreement taxonomy,
Data QC (bundle checks, failures, stale data, missing provenance, open data reports), Robustness
(stable/sensitive/fragile counts and drivers), Priority fixes.

Priority score = **materiality × confidence the issue is real × breadth**, with materiality
cosmetic 0.1 / minor 0.3 / material 0.6 / critical 1.0; confidence from adjudication when available,
else the reporter's confidence × expertise match (in 1.0, adjacent 0.7, outside 0.4), raised (capped at
1) by independent corroborating reports; breadth = 1 + log2(1 + affected outputs). A single critical,
confirmed data error outranks many cosmetic complaints by construction. Frequency is shown but never
used alone.

Anchor-case agreement: distribution of blind postures, modal posture, share matching the Explorer,
share persuaded after reveal, share rejecting (verdicts 4-6), disagreement reasons. With fewer than 3
reviews a case is labelled "too few reviews to summarise". No kappa or significance claims.

---

## 7. Privacy and security

- **No submissions, reviewer names or comments are ever committed.** `qc/private/` and
  `**/review_submissions/` are git-ignored; demo fixtures are synthetic and labelled so.
- **No credentials in the frontend.** The optional live endpoint is a public, write-only URL configured
  at build time (`VITE_REVIEW_ENDPOINT`); the receiving service must do its own validation, rate
  limiting and storage access control. The CSP must be widened explicitly for it.
- The `/review` URL is not a secret and is not treated as one: the public pages contain only public
  Explorer outputs and question text. Private data exist only in the reviewer's browser, in packets they
  send, and in NEIS's backend.
- Reviewer identity: a random `reviewer_id`; the name is optional; exports pseudonymise by default.
- Admin dashboard processes files locally; it never transmits them.
- Deployment options and trade-offs: `docs/QC_DEPLOYMENT.md`. No external service is chosen in code.

---

## 8. Implementation phases

The repo makes a different order from the suggested one materially better: the case-review interface
needs case snapshots and the data-release identity first, and the robustness class shown on the reveal
screen needs the sensitivity engine. So the shared model and the QC engines are built first, in
parallel with the pipeline-level R suite; the UI comes second; documentation and full runs last.

1. **Foundations (parallel)**: review data model + manifest validation + assignment + flow + storage +
   sinks + exports + aggregation + release identity; shipped-bundle QC (choice-rule reproduction,
   sensitivity, threat derivation, provenance map, golden runner); content (campaign, cases, method
   cards, framework scenarios, golden cases, verified against the bundles); R data QC, trade vs
   production, drift, golden-on-pipeline; R audits, canonical sensitivity, validation report.
2. **Interfaces (parallel)**: reviewer app (profile, assignments, blind-first case review, decision use,
   threat); methodology/framework modules, break-the-model and data-issue reporting, Why-this-result
   drawer, admin dashboard.
3. **Run and document**: build review data, run both QC layers on current data, generate audits and the
   report, write the reviewer / admin / deployment / release-checklist docs, run every existing test.

## 9. What this system does not do

- It does not change the methodology. Expert disagreement is evidence to investigate, never an
  instruction to override the model.
- It does not produce a single "validated" score or an agreement-percentage release gate.
- It does not invent production data: where production or capacity evidence is missing the diagnostic
  says "no production evidence", it does not guess.

---

## As built (2026-11-21)

Built on `feat/qc-review` against main's bundles (posture 2026-11-17T20:33 @0496e7ba, AIB 2026-11-17,
country/sector 2026-10-31 @4bb2adc, strategic 2026-10-30 @a54a9d3; data release `rel-9c8f1877b5c6`, with the
partnerships manifest hashed LF-normalised; the CRLF checkout hash gave `rel-cfef665eb878` before 2026-11-21).
How to operate it: [QC_ADMIN_GUIDE.md](QC_ADMIN_GUIDE.md); for reviewers: [QC_REVIEWER_GUIDE.md](QC_REVIEWER_GUIDE.md);
hosting and storage: [QC_DEPLOYMENT.md](QC_DEPLOYMENT.md); release gate: [QC_RELEASE_CHECKLIST.md](QC_RELEASE_CHECKLIST.md).

### File layout

```
config/review/README.md, campaigns/aib_qc_2027_round1.yml, review_cases.yml (CASE_001-021),
  method_cards.yml (MC_001-022), framework_scenarios.yml (FS_001-012)
config/qc/golden_cases.yml (GC_001-036)
dashboard/src/review/
  model.ts tags.ts manifest.ts assign.ts flow.ts storage.ts sink.ts context.ts appInfo.ts release.ts
  exports.ts aggregate.ts privacy.ts demo.ts ReviewLayout.tsx routes.tsx
  data/store.ts                       review data loader (manifest, cases, cards, scenarios, robustness, QC summary)
  qc/ choiceRule.ts sensitivity.ts bundleQc.ts threat.ts provenance.ts golden.ts snapshot.ts answerOptions.ts
      bundles.ts run.ts reports.ts reviewData.ts node/loadBundles.ts (Node only)
  components/ form.tsx IssueReportDialog.tsx ReportIssueButton.tsx ReviewToolbar.tsx WhyThisResult.tsx
  views/ ReviewHome ProfileView AssignmentsView CaseReviewView DecisionUseStep ThreatStep MethodCardsView
         FrameworkView BreakModelView AdminDashboardView
dashboard/scripts/review/ build-review-data.mjs run-bundle-qc.mjs export-review.mjs
dashboard/public/data/review/ manifest.json cases.aib_qc_2027_round1.json method_cards.json
  framework_scenarios.json robustness.json qc_summary.json fixtures/demo_submissions.json
R/qc/ qc_core.R qc_data_checks.R qc_trade_production.R qc_drift.R qc_golden.R qc_sensitivity.R qc_audit.R qc_report.R
scripts/qc/ run_data_qc.R run_sensitivity.R build_case_audit.R write_validation_report.R
tests/testthat/test-qc-{audit,data-checks,drift,golden,report,sensitivity,trade-production}.R
qc/results/ bundle layer: bundle_qc_checks.csv choice_reproduction.csv qc_sensitivity_bundle.csv
              robustness_bundle_summary.csv qc_golden_case_status.csv
            pipeline layer: pipeline_data_qc_checks.csv pipeline_trade_production_flags.csv pipeline_drift_*.csv
              pipeline_golden_case_status.csv pipeline_qc_summary.md qc_sensitivity_pipeline.csv
              pipeline_robustness_summary.csv
qc/audits/ <ISO3>_<Tech>_<Stage>.{json,md} and CASE_###.{json,md} (posture cells only)
qc/private/ git-ignored (submissions, adjudications, exports)
reports/AIB_VALIDATION_REPORT.md
```

Shell edits outside `review/`: `shell/buildInfo.ts` publishes posture, country, AIB and memo stamps (the
stores call the setters); `vite.config.ts` defines `VITE_APP_COMMIT` and `VITE_BUILD_DATE`; the review
route is registered before `'*'` in `routes.tsx` (`modules.ts` place `review`, no top-level tab);
`check-budget.mjs` has review-view and review-data limits; report buttons sit in the posture cell drawer,
AIB node classes, the strategic compact view and the partnerships view; `ReviewToolbar` sits in
`AppRoot` above the footer.

### CLIs

- `node scripts/review/build-review-data.mjs [--config-dir] [--data-dir] [--out] [--golden | --no-golden] [--built-at ISO] [--app-commit] [--seed] [--no-demo] [--quiet]` (exit 1 invalid content, nothing written).
- `node scripts/review/run-bundle-qc.mjs [--data-dir] [--out] [--golden | --no-golden] [--quiet]` (exit 1 on any failing check, golden included).
- `node scripts/review/export-review.mjs --submissions <dir|file>... --out <dir> [--cases] [--adjudications] [--cards] [--scenarios] [--include-names] [--strict]` (exit 2 when `--out` is in a work tree but not under an ignored `qc/private/`).
- `Rscript scripts/qc/run_data_qc.R --processed-dir <dir> [--bundle-dir] [--previous-table] [--out] [--raw-dir] [--golden] [--theme-tables] [--dyads] [--as-of]`.
- `Rscript scripts/qc/run_sensitivity.R [--processed-dir] [--out] [--sp-config] [--node-attributes] [--yf-components] [--no-node-attributes]`.
- `Rscript scripts/qc/build_case_audit.R [--processed-dir] (--case | --iso3/--tech/--stage | --cells | --all-posture-cases) [--bundle-dir] [--out] [--sensitivity] [--date]`.
- `Rscript scripts/qc/write_validation_report.R [--results] [--exports] [--audits] [--cases] [--out] [--date]`.

### Deviations from the design above

1. **Sensitivity files.** Two files, `qc/results/qc_sensitivity_bundle.csv` and
   `qc/results/qc_sensitivity_pipeline.csv` (each still with a `layer` column), plus
   `robustness_bundle_summary.csv` / `pipeline_robustness_summary.csv`, instead of one
   `qc_sensitivity_results.csv`. Golden status likewise: `qc_golden_case_status.csv` (bundle) and
   `pipeline_golden_case_status.csv` (pipeline; failures there are warnings, i.e. drift evidence).
   The bundle layer cannot run the structural specs that need unshipped inputs (gate 0.33 / Unknown 0.5,
   need floor, two-family deployment, cap-aware need) and reports a "possible" flip when a 3-digit value
   sits exactly on a moved threshold. Secure cells are `not_testable` in both layers (the override is not
   re-run).
2. **Answer options** come from `review/qc/answerOptions.ts` (always ending with "I don't know"), which
   `manifest.ts` also uses for canonical keys. There is no `review/vocab.ts` and main has no
   `compact/decide.ts`.
3. **Compact decisions on main** are two families: strategic `SC-*` compacts answer `sectoral` /
   `narrower` / `none`, labelled Potential Sectoral Compact / Targeted action / No compact needed
   ("Targeted action" is quoted from `aib/views/strategic/sections.tsx`, the one label not importable);
   operational `AIB-Cxx` compacts use the `AIB_COMPACT_TYPES` strings. Keys are validated per family.
   Framework scenario FS_010 (`compact_decision`) offers the three strategic answers only.
4. **Partnership cases** are limited to the USA_JPN memo, the only memo whose data main ships in
   `public/data/partnerships`. Memo choice kinds have no Catch Up, so partnership answer options cannot
   offer it.
5. **"Why this result?"** is a collapsed disclosure in the posture cell drawer only, shown only when the
   review UI is enabled (`reviewUiEnabled()`: a reviewer profile in the browser or `VITE_REVIEW_UI=1`).
   Review reveal screens show the decision trace and link to the Explorer rather than embedding it.
6. **Release ID.** Case files carry the full release ID including the memos manifest hash. Explorer
   pages do not load the memos manifest (and each page loads only some bundles), so the ID a report
   carries can be partial; it also records its bundle stamps one by one (`page_context.bundle_stamps`),
   so match a report with a release by comparing stamps, not the composite ID. Case submissions record the case file's release.
   The memos hash is taken with LF line endings.
7. **Campaign rules** are written into `public/data/review/manifest.json` by `build-review-data.mjs`; the
   reviewer pages fall back to defaults equal to round 1's YAML, and say so, when a rule is missing.
   `status` and `review_types` are informational in the UI (no module is hidden, a closed campaign is
   announced, not locked). The optional `reviewers` pre-registration list is validated (codes only) but
   not read by the reviewer pages.
8. **Anchor targets** cannot be enforced across reviewers on a static site (each browser assigns cases
   without knowing the others); `target_reviews_per_anchor` is monitored on the admin Overview tab.
9. **Exports** add `qc_framework_answers.csv`, `qc_decision_use.csv`, `qc_threat_reviews.csv`,
   `qc_priority_fixes.csv` and `export_summary.json`, and neutralise spreadsheet formulas in reviewer text.
10. **Admin dashboard.** Adjudications are stored in the admin's browser and downloaded as JSON for
    `export-review.mjs --adjudications`; the dashboard cannot re-load that file. An adjudication is
    attached to the first submission of an issue group. The Data QC and Robustness tabs show the bundle
    layer (`qc_summary.json`, `robustness.json`); pipeline results are read from `qc/results` and the
    report.
11. **Audits** exist for posture cells only; AIB-node, compact and partnership cases have none.
12. **Hosting.** The CSP in `dashboard/netlify.toml` applies on Netlify only; the GitHub Pages copy sets
    no headers and `index.html` has no CSP meta tag (see QC_DEPLOYMENT.md). No backend is configured;
    the default sink is `local`.
13. **Reviewer-facing wording** still contains a few upstream labels that break the house vocabulary
    ("optimized", bare "HHI" on AIB node facts); they are listed in `KNOWN_WORDING` in
    `views/CaseReviewView.real.test.tsx` so that a new one, or a fix, fails the test.
