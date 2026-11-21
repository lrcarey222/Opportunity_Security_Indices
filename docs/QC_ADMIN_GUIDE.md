# AIB Explorer QC and expert review: admin guide (NEIS)

This guide is for the NEIS staff who run the QC, validation and expert peer review of the published AIB
Explorer: who configure campaigns and cases, run the automated checks, collect and export expert
submissions, adjudicate issues and write the validation report. It describes the system as built on
branch `feat/qc-review`. Where a choice is NEIS's to make, it says so.

- Design and rationale: [QC_SYSTEM_DESIGN.md](QC_SYSTEM_DESIGN.md) (including its "As built" section).
- What reviewers see: [QC_REVIEWER_GUIDE.md](QC_REVIEWER_GUIDE.md).
- Where submissions go and the security model: [QC_DEPLOYMENT.md](QC_DEPLOYMENT.md).
- The draft validation-release gate: [QC_RELEASE_CHECKLIST.md](QC_RELEASE_CHECKLIST.md).
- Authoring rules for the YAML content: [`config/review/README.md`](../config/review/README.md).
- What the committed QC outputs are: [`qc/README.md`](../qc/README.md).

---

## 0. The pieces

| Path | What it is | Who edits it |
|---|---|---|
| `config/review/campaigns/<campaign_id>.yml` | One review round | NEIS, by hand |
| `config/review/review_cases.yml` | The case catalogue (stable `CASE_###` IDs) | NEIS, by hand |
| `config/review/method_cards.yml` | Methodology red-team cards (`MC_###`) | NEIS, by hand |
| `config/review/framework_scenarios.yml` | Comprehension scenarios (`FS_###`) | NEIS, by hand |
| `config/qc/golden_cases.yml` | Golden regression cases (`GC_###`) | NEIS, by hand |
| `dashboard/public/data/review/` | Built, public review data (case snapshots, cards, scenarios, robustness map, QC summary, synthetic demo submissions) | `build-review-data.mjs` only |
| `dashboard/src/review/` | The reviewer app, its pure logic and the shared types (`model.ts`) | code |
| `dashboard/scripts/review/` | Node CLIs: `build-review-data.mjs`, `run-bundle-qc.mjs`, `export-review.mjs` | code |
| `R/qc/`, `scripts/qc/` | Pipeline-layer QC (pure functions / IO scripts) | code |
| `qc/results/` | Latest automated QC outputs (CSV, committed) | the QC runs |
| `qc/audits/` | End-to-end case audits (`.json` + `.md`, committed) | `build_case_audit.R` |
| `qc/private/` | Submissions, adjudications, exports. **Git-ignored; never committed** | NEIS |
| `reports/AIB_VALIDATION_REPORT.md` | The validation report | `write_validation_report.R` |

Tools: Node 24 (the CLIs import the TypeScript sources directly) and R 4.4 with `dplyr`, `tibble` and
`yaml` (plus the packages the strategic-position code already needs). Run the Node CLIs from
`dashboard/` and the R scripts from the repository root. Paths below are relative to those directories;
nothing in the scripts is tied to one machine.

Two QC layers run side by side and are never merged:

1. **Bundle layer** (TypeScript): checks exactly what the Explorer serves (`dashboard/public/data`).
2. **Pipeline layer** (R): checks the processed tables the bundles are exported from
   (`--processed-dir`, default `processed_dir` in `config/config.yml`).

Every output names the release it describes: `rel-<12 hex>` for a set of shipped bundles,
`pipeline-<12 hex>` for the processed `strategic_position_tbl` the R layer read.

---

## 1. Add reviewers

There are no accounts and no logins. A reviewer opens `<site>/review`, fills in a profile, and the browser
creates a random `reviewer_id`. Every export and packet file names the reviewer by a pseudonymous code
derived from it, `R-` plus 8 hex characters (for example `R-3F9A1C07`; `privacy.ts`
`pseudonymReviewerCode`).

To invite a reviewer: send them the review address, the reviewer guide, the contact for questions and,
in packet mode, the route for returning files (see [QC_DEPLOYMENT.md](QC_DEPLOYMENT.md)).

**Pre-registration (optional).** A campaign may list `reviewers: [{code: R-..., groups: [...]}]`. The
validator accepts codes only (`R-` followed by capitals and digits) and **rejects** any `name`, `email`
or `organization` field, because the YAML is committed. As built, the reviewer pages do **not** read
this list (there is no code-entry field), so it is a bookkeeping record only: for example, the codes
NEIS has received packets from, with the groups they were recruited for. If NEIS needs to know who is
behind a code, keep that key **outside the repository** (and outside any public place) together with
consent records. Whether to pre-register at all, and how, is a NEIS decision.

## 2. Expertise tags

One vocabulary is used by the profile form, case labels, assignment and the dashboard
(`dashboard/src/review/tags.ts`):

| Kind | Tags |
|---|---|
| Sector | `batteries`, `coal`, `electric-grid`, `electric-vehicles`, `gas`, `geothermal`, `green-hydrogen`, `nuclear`, `oil`, `solar`, `wind`, `critical-minerals`, `semiconductors` |
| Stage | `Extraction`, `Refining`, `Components`, `Assembly`, `Deployment`, `Recovery` |
| Country | ISO3 codes (`KOR`, `JPN`, ...) |
| Method | `trade_data`, `production_capacity_data`, `industrial_policy`, `supply_chain_security`, `investment_finance`, `development_finance`, `geopolitics_alliances`, `energy_systems`, `input_output_modelling`, `indices_statistics` |

Reviewers rate each tag they pick from 1 to 5. A reviewer is **in** expertise for a case when they rate
one of its sector tags at or above the campaign's `min_domain_confidence` **and** either one of its
country tags or its stage at or above it (a case with no sector tags, such as the partnership, is "in"
on a rated country alone). **Adjacent**: a sector or country tag of the case is listed at any rating.
Otherwise **outside**. Method cards are "in" when the reviewer rates one of the card's `method_tags` at
3 or more.

Tag cases (`sector_groups`, `country_groups`, `stage`) with these exact tags; the build rejects others.

## 3. Configure a campaign

One YAML file per round in `config/review/campaigns/`. Fields (type `Campaign` in `model.ts`):

| Field | Meaning |
|---|---|
| `campaign_id` | Stable ID; also names the built file `cases.<campaign_id>.json` |
| `name`, `description` | Shown at the top of `/review`. The description is the place for the purpose, the privacy notice and the contact |
| `status` | `draft`, `active` or `closed`. As built this changes the text on the first page only (draft: "for testing the review pages"; closed: "NEIS is no longer collecting answers"); it does not block submissions. The pages open the campaign named by `?campaign=<id>`, else the first `active` one, else the first listed |
| `opens`, `closes` | Dates, or `null`. Informational |
| `review_types` | Any of `case`, `methodology`, `framework`, `decision_use`, `threat`, `break_model`. Validated and recorded; as built the reviewer pages show every module whatever this lists |
| `case_ids` | The cases in scope (each must exist in the catalogue; no duplicates) |
| `anchor_cases` | A subset of `case_ids` that several reviewers should judge |
| `target_reviews_per_anchor` | How many reviews each anchor should get |
| `assignment.assigned_per_reviewer` | `{min, max}` cases per reviewer |
| `assignment.anchors_per_reviewer` | Anchors given to each reviewer (when they can judge one) |
| `assignment.out_of_specialty` | `none`, `optional` (one beyond the stated expertise, marked optional) or `one` (required) |
| `assignment.allow_self_selected` | Whether a reviewer may add one case from the catalogue |
| `assignment.min_domain_confidence` | Self-rating (1-5) needed for an "in" match |
| `method_card_ids`, `framework_scenario_ids` | Subsets; empty or absent means all |
| `reviewers` | Optional pre-registration, codes only (section 1) |

`build-review-data.mjs` writes each campaign's rules into `dashboard/public/data/review/manifest.json`,
and the reviewer pages apply them. If a rule is missing from the manifest, the pages fall back to
defaults equal to round 1's YAML (min 2 / max 4, 1 anchor, out-of-specialty optional, self-selection on,
min confidence 3, target 4) and the first page says that defaults are in use. After editing a campaign,
always rebuild (section 7).

## 4. Choose cases

`config/review/review_cases.yml`, `cases:` list (type `CaseDefinition`). Full authoring rules are in
[`config/review/README.md`](../config/review/README.md). The essentials:

| Field | Meaning |
|---|---|
| `case_id` | `CASE_###`, stable, **never renumbered and never reused** (submissions, exports and adjudications refer to it). Retire a case by dropping it from a campaign's `case_ids`; it stays in the catalogue. The validator rejects a duplicate ID in the file, but it cannot know about a retired one, so reuse is a human rule |
| `kind` | `posture_cell`, `aib_node`, `compact`, `partnership` or `sector` |
| `key` | posture/sector `{iso3, tech, stage}` (tech as the posture display name, e.g. `Electric Vehicles`); AIB node `{node, scenario}` with scenario `decoupling` or `managed`; compact `{sid, scenario}`; partnership `{pair}` |
| `title`, `question` | What the reviewer is asked, in plain language |
| `sector_groups`, `country_groups`, `stage`, `assignment_group` | Expertise tags used for assignment |
| `anchor` | `true` for an anchor (keep it consistent with the campaign's `anchor_cases`) |
| `why_selected` | Why NEIS chose it (golden category, threshold margin, centrality) |
| `explorer_path` | A real deep link into main's app |
| `context_fields` | Facts shown **before** the blind answer |
| `reveal_fields` | Facts shown only **after** it |
| `decision_use`, `threat` | Add the decision-use / threat questions to this case |

**Context versus reveal.** Context facts are the evidence an expert would weigh (import mix, supplier
shares, world share, export strength, market outlook; for AIB nodes the exposure and today's
suppliers). The model's deciding scores (need, prize, Competitive Position, need-family percentiles,
pathway, deployment need, coverage after coordination) belong in `reveal_fields`, so the blind answer is
the reviewer's own reading of the evidence. The build **refuses** these IDs in `context_fields`, because
they give the answer away: `choice`, `fence`, `posture`, `reason`, `confidence`, `borderline`,
`partner`, `policy_alignment`, `class`, `decision`, `missions`, `choices_summary`, `headline`. Leave out
of `reveal_fields` any fact the bundle never carries for that case (it would show as "not available in
this release").

**`why_selected` is never shown before the reveal.** It often states the conclusion (for example "Preserve
with Competitive Position 0.763 against the 0.75 bar"). The case page shows it only after the blind
answer is stored, and a test checks this for every committed case. Do not surface it on any other
pre-answer page.

**Limits as built.** Only the USA_JPN partnership memo can be a case: main's
`dashboard/public/data/partnerships/` holds only that memo's data (other pairs exist only as static
documents on the Pages site). Partnership answer options come from the memo choice kinds, which have no
Catch Up. A `sector` case needs a full `{iso3, tech, stage}` key (it reuses the posture snapshot).
Numbers quoted in `why_selected` are frozen text: re-check them whenever the bundles change.

## 5. Anchor cases and target reviews

Anchor cases are judged by several reviewers so their answers can be compared. Choose one per main
expert group that every reviewer of that group can judge (round 1: `CASE_001` batteries, `CASE_003`
critical minerals, `CASE_006` solar, `CASE_008` electric grid, `CASE_009` nuclear), list them in the
campaign's `anchor_cases` and mark them `anchor: true`.

Each browser assigns cases on its own from the profile, the campaign rules and a seed of the reviewer
code, so assignment is deterministic and repeatable. The browser cannot see other reviewers'
assignments, so **`target_reviews_per_anchor` is a target NEIS monitors, not a quota the site enforces**.
Watch "Anchor cases at target" on the admin dashboard's Overview tab and steer coverage by whom you
invite (their declared expertise decides which anchor they get) or by asking a reviewer to add an anchor
as their self-selected case. A case with fewer than 3 usable reviews is shown as "too few reviews to
summarise".

## 6. Method cards, framework scenarios and golden cases

**Method cards** (`method_cards.yml`, `cards:`): `card_id`, `title`, `area` (need, prize, position,
choice_rule, secure, deployment, aib_stress_test, aib_compact, aib_partners, data), `current_choice` in
plain language, `why` (the stated rationale), `example` `{text, explorer_path}` read from the bundle, one
to three `alternatives`, `affected_outputs`, `code_refs` (`file:line`; not shown to reviewers),
`config_keys` and `method_tags`. Only real, consequential choices of the current code.

**Framework scenarios** (`framework_scenarios.yml`, `scenarios:`): `scenario_id`, `concept`, `prompt`,
`option_set` (`posture_choice`, `aib_class`, `compact_decision` or `custom`), `options` (leave `[]` for a
canonical set; the builder fills it from `review/qc/answerOptions.ts`), `intended_answer` (what the code
does), `explanation` and `terms`. A `compact_decision` scenario offers the three strategic-compact
answers only (Potential Sectoral Compact / Targeted action / No compact needed).

**Golden cases** (`config/qc/golden_cases.yml`, `cases:`): `id` (`GC_###`, stable), `category`
(obvious_secure, keep_open, strong_producer, import_dependent, entrepot, sparse, near_threshold,
neis_central, surprising, method_sensitive), `kind` (posture_cell, aib_node, compact), `key`, `expect`
(`choice` or `choice_in`, optional `fence`, optional `numeric` ranges per fact id), `exact`,
`change_requires_review`, `rationale`, `approved {by, on, release}`. A golden case pins **current**
behaviour so that a change is deliberate; it does not endorse the output, and the rationale says so
where a result looks doubtful.

Golden cases run in three places: the dashboard unit tests (`npm test`, so CI fails on any failing
case), `run-bundle-qc.mjs` (exit code 1), and the pipeline layer (`run_data_qc.R`, where a failure on the
processed table is a **warning** with "needs human explanation": drift since the case was pinned).

**Re-approving a golden case after an intended change.** The runner does not excuse a failure because of
`change_requires_review` or `approved`: a failing case fails until its expectation is edited. When a new
release makes a case fail:

1. Establish whether the change is intended (a deliberate method or data change, with its commit) or a
   defect. If it is a defect, fix the pipeline, not the case.
2. If intended, edit `expect` to the new value, set or range, and append to `rationale` what changed,
   why, and the old expectation.
3. Set `approved` to `{by: <role or name of the approver>, on: <date>, release: <the new rel-... ID from
   dashboard/public/data/review/manifest.json data_release.id>}`. For cases with
   `change_requires_review: true` (always so for near_threshold, surprising, method_sensitive and
   neis_central), the approver is the NEIS methodology lead.
4. Re-run `node scripts/review/run-bundle-qc.mjs` and `npx vitest run --project unit src/review/qc`
   and confirm they pass; name the case IDs and the reason in the commit message.

Never retire or loosen a golden case to make a release pass without that record.

## 7. Build the review data

After any change to `config/review/` or to the shipped bundles, from `dashboard/`:

```
node scripts/review/build-review-data.mjs --built-at 2026-12-01T00:00:00Z
```

| Flag | Default | Meaning |
|---|---|---|
| `--config-dir <dir>` | `<repo>/config` | Where `review/` lives |
| `--data-dir <dir>` | `dashboard/public/data` | The shipped bundles to read |
| `--out <dir>` | `<data-dir>/review` | Where to write |
| `--golden <file>` / `--no-golden` | `config/qc/golden_cases.yml` | Golden cases for the QC summary |
| `--built-at <ISO>` | now | Timestamp written into the files; pass it for byte-identical rebuilds |
| `--app-commit <sha>` | `git rev-parse --short HEAD` | Commit recorded in the files |
| `--seed <s>` | `aib-review-demo` | Seed of the synthetic demo submissions |
| `--no-demo` | | Do not write `fixtures/demo_submissions.json` |
| `--quiet` | | Less output |

It validates every YAML file (`dashboard/src/review/manifest.ts`), runs the bundle QC, builds each
case's snapshot (the exact values a reviewer sees), its sha256, the bundle file stamps and the answer
options, and writes `manifest.json`, `cases.<campaign>.json`, `method_cards.json`,
`framework_scenarios.json`, `robustness.json`, `qc_summary.json` and the demo fixtures. Any validation or
build error exits with code 1 and writes nothing. Exit code 2 means a bad command line.

After a rebuild: run `npx vitest run --project ui src/review/views` (the real-case test checks the
blind-first guarantee on every committed case and lists known wording problems in `KNOWN_WORDING`; update
that list only when the wording genuinely changed) and `npm run budget` after a production build (the
review data has its own size limit in `scripts/check-budget.mjs`).

**Freeze the build for a campaign.** Every case submission records the case's `case_snapshot_sha256`
and the case file's `data_release`. Rebuilding against new bundles mid-campaign changes snapshots, so
reviewers would be judging different numbers. Rebuild during a campaign only to fix an error, and note it.

## 8. Run the automated QC

### Bundle layer (from `dashboard/`)

```
node scripts/review/run-bundle-qc.mjs [--data-dir public/data] [--out ../qc/results] [--golden <file> | --no-golden] [--quiet]
```

Writes `qc/results/bundle_qc_checks.csv` (one row per check: schema, codes, numeric validity,
completeness, posture invariants, trace consistency, provenance stamps, AIB coverage paths and hosts,
memos, robustness, golden), `choice_reproduction.csv` (the choice rule re-run from each cell's shipped
decision trace; every non-reproduced cell with its reason), `qc_sensitivity_bundle.csv`,
`robustness_bundle_summary.csv` and `qc_golden_case_status.csv`. Exit 1 when any check fails (golden
included), 2 on a usage or read error.

### Pipeline layer (from the repository root)

```
Rscript scripts/qc/run_data_qc.R --processed-dir <processed-dir> [--bundle-dir dashboard/public/data]
    [--previous-table <older strategic_position_tbl.rds>] [--out qc/results] [--raw-dir <dir>]
    [--golden config/qc/golden_cases.yml] [--theme-tables a,b,c] [--dyads current_trade_dyads_tbl|none]
    [--as-of YYYY-MM-DD]
```

Schema, missingness, numeric validity, trade consistency (mirror versus reported, importer shares, HS6
crosswalk), classification logic (choice reproduction, knife-edges, need-floor caps, excluded contenders),
provenance (raw-inputs manifest: URL, vintage, licence, staged files, licence conflicts), trade versus
production, release drift (with `--previous-table`), release alignment (whether each shipped bundle can be
traced to the processed outputs) and golden cases on the processed table. Writes the `pipeline_*.csv`
files and `pipeline_qc_summary.md`. Exit 1 only when a row is `fail`; drift never fails, it produces a
review list. `--raw-dir` defaults to the `raw` directory beside `--processed-dir` and is only listed.

```
Rscript scripts/qc/run_sensitivity.R [--processed-dir <dir>] [--out qc/results]
    [--sp-config config/strategic_position.yml] [--node-attributes data/reference/posture_node_attributes.csv]
    [--yf-components data/reference/yards_fences_assessments.csv] [--no-node-attributes]
```

The canonical sensitivity: re-runs the decision engine's choice rule per cell under the local and
structural specifications, need-floor-cap aware. Writes `qc_sensitivity_pipeline.csv` and
`pipeline_robustness_summary.csv` (with its reproduction checks).

```
Rscript scripts/qc/build_case_audit.R [--processed-dir <dir>]
    (--case CASE_001[,CASE_004] [--cases-file config/review/review_cases.yml]
     | --iso3 KOR --tech "Batteries" --stage Components
     | --cells "KOR/Batteries/Components;USA/Batteries/Refining"
     | --all-posture-cases)
    [--bundle-dir dashboard/public/data] [--out qc/audits] [--sensitivity qc/results/qc_sensitivity_pipeline.csv]
    [--sp-config config/strategic_position.yml] [--manifest config/raw_inputs_manifest.yml] [--date <ISO>]
```

Source-to-conclusion audits of posture cells: sources and rows, transformations, indicators, scores,
threshold decisions, the final choice, the robustness class, the shipped bundle's values for the same
cell and any difference, and every link that is "not reproducible from shipped/pipeline data". Writes
`<ISO3>_<Tech>_<Stage>.json/.md`, plus `<CASE_###>.json/.md` copies for catalogue cases. Re-run with
`--all-posture-cases` after any change to the posture cases. AIB-node, compact and partnership cases have
no R audit as built.

Run the R tests of this layer with `Rscript -e "testthat::test_file('tests/testthat/test-qc-<name>.R')"`
for `audit`, `data-checks`, `drift`, `golden`, `report`, `sensitivity` and `trade-production`.

## 9. Collect submissions

Every submission is a JSON envelope (`schema: aib-review-submission`) with a `submission_id`, the
`campaign_id`, the `kind` (case_review, method_feedback, framework_answer, decision_use, threat_review,
data_issue, break_model), the reviewer stamp (code, name only if given and not anonymous, a profile
digest, the expertise tags and ratings), the app commit, the data release and, for reports, the page
context. A **packet** (`schema: aib-review-packet`) holds one reviewer's submissions.

Keep everything received under `qc/private/` (git-ignored), for example `qc/private/submissions/`. How
submissions arrive depends on the build's storage mode (details and options in
[QC_DEPLOYMENT.md](QC_DEPLOYMENT.md)):

| Mode (`VITE_REVIEW_SINK`) | How NEIS receives them |
|---|---|
| `local` (default, "Demo") | Nothing is sent. Reviewers use "Download your answers" on `/review` and send the packet file |
| `packet` | Each submit downloads a one-submission packet; reviewers send the files (and can send the all-in-one download at the end). Duplicates are harmless: exports and the dashboard de-duplicate by `submission_id` |
| `http` ("Live") | The browser POSTs each submission to `VITE_REVIEW_ENDPOINT`; NEIS retrieves them from its backend into `qc/private/` (a local copy also stays in the reviewer's browser) |

## 10. Export responses

From `dashboard/`:

```
node scripts/review/export-review.mjs --submissions ../qc/private/submissions --out ../qc/private/exports \
    --cases public/data/review/cases.aib_qc_2027_round1.json \
    --adjudications ../qc/private/aib-review-adjudications.json \
    --cards public/data/review/method_cards.json --scenarios public/data/review/framework_scenarios.json
```

| Flag | Meaning |
|---|---|
| `--submissions <dir\|file> [<file> ...]` | Required. Every `*.json` below a directory, or the files given: a submission, a list, a packet, or `{submissions: [...]}` |
| `--out <dir>` | Required. See the privacy guard below |
| `--cases`, `--cards`, `--scenarios` | Built review files, for titles, tags and intended answers |
| `--adjudications <file>` | The adjudications JSON downloaded from the admin dashboard |
| `--include-names` | Include reviewer names (never an anonymous reviewer's). Off by default |
| `--strict` | Write nothing if any submission is invalid (otherwise invalid ones are skipped with a warning) |

Writes `qc_review_responses.csv`, `qc_case_results.csv`, `qc_method_feedback.csv`, `qc_data_issues.csv`,
`qc_framework_answers.csv`, `qc_decision_use.csv`, `qc_threat_reviews.csv`, `qc_priority_fixes.csv` and
`export_summary.json`. Reviewers appear as `R-` codes. Text that a spreadsheet would run as a formula is
prefixed with an apostrophe.

**Privacy guard.** The output directory must be outside any git work tree, or inside the work tree's
`qc/private/` **and** git-ignored there; anything else is refused with exit code 2. Exit 1 is a bad
command line or invalid input. Do not copy exports anywhere that syncs to a public place. If the checkout
itself sits in a synced folder (OneDrive, SharePoint), `qc/private/` syncs too: whether that is
acceptable is a NEIS data-protection decision.

## 11. The admin dashboard (`/review/admin`)

Open `<site>/review/admin` (not linked from the reviewer pages; the URL is not a secret and the page
holds no data until you load files). It runs entirely in your browser: files are read locally and nothing
is uploaded.

**Loading.** "Submission files or packets (JSON)" accepts several files at once (single submissions,
packets, lists). "Load the demo data (synthetic)" loads the clearly labelled demo fixtures; the page then
warns that nothing shown is a real answer, and the CSV zip is named `-SYNTHETIC`. "Add this browser's own
submissions" adds those stored locally. Names are hidden unless you tick "Show names" (never for
anonymous reviewers).

**Tabs.**

- **Overview**: submissions and reviewers, cases reviewed of those in scope, anchor cases at target,
  unresolved critical issues, open data reports, and an anchor table.
- **Agreement**: per case, anchors first: the distribution of blind answers, the modal answer, the share
  matching the Explorer, the share persuaded after the reveal, the share rejecting (verdicts 4-6) and the
  reasons; below 3 usable reviews, "too few reviews to summarise". A blind-versus-verdict table counts
  agreed and confirmed, agreed but doubtful or rejecting, differed but persuaded, differed and
  unconvinced or rejecting, outside expertise, and inconsistent answers.
- **Disagreement taxonomy**: counts by type (data, method, missing context, framework ambiguity,
  actionability, insufficient evidence) from case reviews, framework mismatches and decision-use answers.
- **Data QC**: the bundle-layer summary (`qc_summary.json`: checks failing / warning / passing, choices
  reproduced, golden status) and the open data-issue reports. Pipeline-layer results are not shown here:
  read `qc/results/pipeline_qc_summary.md` and the report.
- **Robustness**: the bundle-layer class map (`robustness.json`) as a technology x stage grid, the number
  of cells whose fence would change, and the drivers.
- **Priority fixes**: every reported issue, ranked, with its factors and an adjudication editor.

**The priority score** = **materiality x confidence the issue is real x breadth** (`aggregate.ts`
`priorityFixes`, weights from `model.ts`):

- *Materiality* (`SEVERITY_WEIGHT`): cosmetic 0.1, minor 0.3, material 0.6, critical 1.0. Data and
  break-the-model reports use the reporter's severity. A case review with verdict 3 counts as minor and
  4-6 as material. A method card answered modify or reject counts by its materiality answer 1-4
  (cosmetic, minor, material, critical); retain, unsure and outside-expertise answers are not issues. Reports
  on the same issue are grouped (same measure and cell, same case and disagreement type, or same card)
  and the group takes its highest severity.
- *Confidence the issue is real*: an adjudication's `confidence_real` when one is decided (a rejected or
  duplicate ruling without a value counts 0; any other decided ruling without a value counts 1).
  Otherwise the reports combine as independent evidence: for each reviewer
  `c = reporter confidence x expertise match weight` (`MATCH_WEIGHT`: in 1.0, adjacent 0.7, outside 0.4),
  where reporter confidence is the reviewer's own 1-5 rating of the matching tags (data reports), the
  blind confidence (case reviews) or the method-tag rating (cards), divided by 5, or 0.5 when unknown;
  the group's confidence is `1 - product(1 - c)`, capped at 1.
- *Breadth* = `1 + log2(1 + affected outputs)`, where the count comes from the adjudication, else 1 (for
  a method card, the number of outputs listed on the card).

So a single confirmed critical data error (1.0 x 1.0 x 1 = 1.0) outranks many cosmetic complaints by
construction. The number of reports is shown but never ranks an issue on its own.

## 12. Review disagreements

Disagreement is evidence to investigate, never an instruction to change the model. For each anchor or
much-disputed case:

1. Read the Agreement tab with the case's audit (`qc/audits/<CASE_###>.md`) and robustness class open.
2. Separate the causes using the reviewers' "single most important miss" types: a data problem goes to
   the data owners, a method problem to a method card or a methodology review, missing context to the
   case's evidence or the method's scope, framework ambiguity to the wording, actionability to the
   recommendation text.
3. Weigh blind answers by expertise match; treat "I don't know" and verdict 7 as abstentions, not votes.
4. Record the conclusion as an adjudication (section 13). If the Explorer stays as it is, say why.

## 13. Adjudicate issues and mark them resolved

In Priority fixes, "Adjudicate" opens an editor. An adjudication is a separate record (reviewer
submissions are never edited):

| Field | Meaning |
|---|---|
| `status` | `open`, `confirmed`, `rejected` (not an issue), `duplicate`, `resolved`, `wont_fix` |
| `issue_type` | data, method, context, framework, actionability, insufficient_evidence |
| `confidence_real` | 0-1, your confidence the issue is real (overrides the reports' combined confidence) |
| `affected_outputs` | How many outputs it affects (sets the breadth) |
| `note` | The reasoning |
| `resolution_commit` | The commit that fixed it, when `resolved` |
| `decided_by`, `decided_at` | Filled in by the editor |

`resolved`, `rejected`, `duplicate` and `wont_fix` close an issue; `open` and `confirmed` leave it
unresolved. The record is attached to the first submission of the issue group; the latest decided
ruling in a group decides its score. Mark an issue `resolved` only once the fix is merged and the
regenerated bundle is checked, and record the commit.

Adjudications are stored **in this browser only**. Download them with "Download the adjudications
(JSON)" after every session, keep the file in `qc/private/`, and pass it to `export-review.mjs
--adjudications`. As built, the dashboard cannot re-load an adjudications file, so adjudicate from one
browser (or merge files by hand) until that exists.

## 14. Generate the validation report

From the repository root, after the QC runs and the export:

```
Rscript scripts/qc/write_validation_report.R [--results qc/results] [--exports qc/private/exports]
    [--audits qc/audits] [--cases dashboard/public/data/review/cases.<campaign>.json]
    [--out reports/AIB_VALIDATION_REPORT.md] [--date YYYY-MM-DD]
```

It reads whatever result and export CSVs exist (a missing one is listed as "not run" or "absent"),
the case snapshots and the audits, and writes the report: scope and releases, reviewers, coverage,
findings, strongest validations, unresolved disagreements, critical data issues, sensitive and fragile
conclusions, methodological issues, changes made because of review and remaining limitations. Reviewer
names never reach it; without review exports every review section says "No expert reviews recorded
yet." Pass `--date` for a reproducible report. Read it before committing: it is committed, the exports
are not.

## 15. Interpret robustness classes

Each posture cell (and AIB node) is re-decided under alternative specifications:

- **Local** specs: each decision threshold (need, prize, Competitive Position, deployment need) moved by
  the borderline band of 0.05 one at a time, and a strict instead of inclusive reading of the
  Competitive Position bar.
- **Structural** specs: defensible alternative methods, such as the large-yard rule needing 4 or 6
  signals instead of 5, position-first off, a market-only gate of 0.33, Unknown attributes counted as 0.5,
  need floor 0.05, Accelerate needing two or more need families, need 0.60 computed cap-aware. AIB nodes:
  variants of the headline shock (for example China's supply halved instead of removed).

| Class | Meaning |
|---|---|
| **Stable** | No spec tested changes the conclusion |
| **Sensitive** | No local spec changes it; at least one structural spec does |
| **Fragile** | At least one small threshold move changes it |
| **Not testable** | The inputs needed are not available (not-assessed cells; Secure cells, whose override tests are not re-run; in the pipeline layer also entrepot exclusions) |

**Fence change** is reported separately: a flip that changes the fence (for example Preserve to Watch,
medium to low) matters more than one within a fence (Preserve to Catch Up).

**Bundle versus pipeline layer.** The bundle layer works from the 3-digit values the Explorer ships; it
cannot run the structural specs that need unshipped inputs, and where a value sits exactly on a moved
threshold it reports a **possible** flip and never calls the cell stable. The pipeline layer runs the
full set on the processed table. Read them side by side (`qc_sensitivity_bundle.csv`,
`qc_sensitivity_pipeline.csv`); the class a reviewer sees on a reveal screen is the bundle layer's. A
fragile class is not an error: it says the conclusion rests on a knife-edge and should be presented,
reviewed and released with that caveat.

## 16. Keep the three things separate

Report **data quality** (is the input right? automated checks, data-issue reports, audits),
**analytical robustness** (does the conclusion survive defensible alternatives? the sensitivity layers)
and **expert validation** (do domain experts, judging blind, reach the same view, and why not?) as three
separate fields. Never combine them into one score or one "validated" label, and never gate a release on
an agreement percentage. Expert results are structured elicitation from a small, chosen panel, not a
representative poll.

---

## 17. Launching round 1: checklist

- [ ] NEIS chooses the storage mode and, if not packet mode, the backend ([QC_DEPLOYMENT.md](QC_DEPLOYMENT.md)). Packet mode is the recommended default for round 1.
- [ ] NEIS adds the contact, the privacy notice and the retention period to the campaign `description` and to the placeholders in [QC_REVIEWER_GUIDE.md](QC_REVIEWER_GUIDE.md).
- [ ] Review `config/review/campaigns/aib_qc_2027_round1.yml`: dates, `case_ids`, `anchor_cases`, `target_reviews_per_anchor`, assignment rules. Keep `status: draft` while testing.
- [ ] Review the 21 cases, 22 method cards and 12 scenarios for wording and for numbers quoted in `why_selected` / `example` against the current bundles.
- [ ] NEIS approves the golden cases (`approved.by`, `approved.on`, `approved.release`), or records which stay drafts.
- [ ] The site to be used serves the same bundles as main ([QC_RELEASE_CHECKLIST.md](QC_RELEASE_CHECKLIST.md), release identity); if not, republish before building the review data.
- [ ] Run `run-bundle-qc.mjs`, `run_data_qc.R`, `run_sensitivity.R`, `build_case_audit.R --all-posture-cases`; read the warnings.
- [ ] Set `status: active`, rebuild with `build-review-data.mjs --built-at <ISO>`, run the review tests (`npx vitest run --project unit src/review` and `--project ui src/review`), typecheck, build, `npm run budget`.
- [ ] Deploy with the chosen `VITE_REVIEW_SINK` (and `VITE_REVIEW_ENDPOINT` and the CSP change for http mode). Do not set `VITE_REVIEW_UI=1` on the public build unless NEIS wants every visitor to see the report buttons.
- [ ] Dry run: one NEIS colleague completes the profile, two cases, a card, a scenario and a report on the deployed site; NEIS receives the packet, loads it in `/review/admin`, exports it with `export-review.mjs` and regenerates the report. Delete the dry-run data afterwards.
- [ ] Recruit reviewers so every anchor can reach its target (Overview tab); send the address, the reviewer guide and the return route.
- [ ] During the campaign: collect packets into `qc/private/submissions/`, check anchor coverage weekly, download adjudications after each session.
- [ ] At close: set `status: closed`, rebuild, export, adjudicate, regenerate the report, and work through [QC_RELEASE_CHECKLIST.md](QC_RELEASE_CHECKLIST.md).
