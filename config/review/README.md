# Review and golden-case content

Authored content of the QC, validation and expert peer-review system for the published AIB Explorer
(main). The design is `docs/QC_SYSTEM_DESIGN.md`; the types these files map to, one to one, are in
`dashboard/src/review/model.ts`. The files are plain YAML, edited by hand by NEIS.

| File | Type in `model.ts` | What it holds |
|---|---|---|
| `campaigns/<campaign_id>.yml` | `Campaign` | One review round: status, review types, the cases in scope, anchor cases, assignment rules. Empty `method_card_ids` / `framework_scenario_ids` mean "all". |
| `review_cases.yml` (`cases:`) | `CaseDefinition` | The case catalogue: posture cells, AIB nodes, sector chains, strategic compacts and partnership memos that reviewers judge blind, each with its authored `brief:`. |
| `method_cards.yml` (`cards:`) | `MethodCard` | Methodology red-team cards: one consequential judgment call each, with the current choice, the stated reason, a real example, alternatives and code references. |
| `framework_scenarios.yml` (`scenarios:`) | `FrameworkScenario` | Short comprehension scenarios; `intended_answer` is what the code does. |
| `../qc/golden_cases.yml` (`cases:`) | `GoldenCase` | Regression cases evaluated against the shipped bundles by `dashboard/src/review/qc/golden.ts`. |

`dashboard/scripts/review/build-review-data.mjs` validates every file with `dashboard/src/review/manifest.ts`
(`validateCampaign`, `validateCaseCatalogue`, `validateMethodCards`, `validateFrameworkScenarios`,
`validateGoldenCases`) and fails on any error. It then builds `dashboard/public/data/review/*.json` from the
shipped bundles: the exact facts each case shows (its snapshot), the snapshot hash, the bundle stamps and the
answer options. Nothing numeric in these YAML files is read by the build. Numbers quoted in `why_selected`,
`example` and `rationale` text explain a pick, and were read from main's bundles: posture
2026-11-17T20:33 (git 0496e7ba, `posture-core.f665dc19.json`), AIB 2026-11-17 (`compacts.c5643da1.json`) and
the USA_JPN memo (generated 2026-11-06). Re-check them when the bundles change.

## IDs

- `CASE_###`, `MC_###`, `FS_###` and `GC_###` are stable. Never renumber and never reuse an ID for something
  else, because submissions, exports and adjudications refer to them.
- Retire a case by removing it from a campaign's `case_ids` (it stays in the catalogue), a card or scenario by
  listing the others in the campaign's `method_card_ids` / `framework_scenario_ids`, and a golden case by
  deleting it with a note in the commit message. A retired ID is not given to a new entry.
- New entries take the next free number.

## Shared vocabularies

- Sector tags: `batteries, coal, electric-grid, electric-vehicles, gas, geothermal, green-hydrogen, nuclear,
  oil, solar, wind, critical-minerals, semiconductors`. Stage tags: `Extraction, Refining, Components,
  Assembly, Deployment, Recovery`. Country tags: ISO3. Method tags: `trade_data, production_capacity_data,
  industrial_policy, supply_chain_security, investment_finance, development_finance, geopolitics_alliances,
  energy_systems, input_output_modelling, indices_statistics`.
- Canonical conclusion keys (what the Explorer can conclude): posture `secure, preserve, catch_up, partner,
  accelerate, watch, not_assessed`; AIB node classes as the bundle writes them (`BUILD`, `COORDINATE`,
  `MANAGE SPECIALIZATION`, `LOW INTERVENTION`, `INSUFFICIENT DATA`); strategic compacts `sectoral | narrower |
  none` (the lead mission is material / targeted / absent under the case's scenario); partnership memo boxes
  `preserve | partner | accelerate | watch` (labelled Build together, Trade and standards, Institutions, Stay
  out, as R/partnerships/pm_document.R writes them); sector chains the six stage tags plus `no_stage`.
  Labels are never typed here: the builder reads them from `content/vocabulary.ts`, `posture/grammar.ts`,
  `lib/stages.ts` and `review/qc/answerOptions.ts`. Watch keeps its name and is shown as "Watch (keep open)".
- Reviewer options (UX v2): the conclusions minus the model states (`not_assessed` and `INSUFFICIENT DATA`
  describe the model, not a recommendation), each with its one-line definition, then the three reviewer states
  `none_of_these`, `insufficient_evidence`, `outside_expertise`. The v1 `dont_know` is no longer offered
  but stays valid in stored answers.
- Fact ids (snapshot keys) per case kind are listed in `review/qc/snapshot.ts` (`FACT_IDS`); brief fact ids
  (for `brief.evidence_ids`) in `review/qc/brief.ts` (`BRIEF_FACT_IDS_BY_KIND`), assumption ids (for
  `brief.assumption_ids`) in `review/qc/brief.ts` (`ASSUMPTIONS`).

## The case brief (UX v2)

Reviewers are blind to the Explorer's conclusion, not to the information needed to form one. Before the blind
answer each case shows a neutral brief, built by `dashboard/src/review/qc/brief.ts` from the bundles plus the
authored `brief:` block:

- authored: `decision_question` (one decision with its horizon, e.g. "How should South Korea manage battery
  component manufacturing through 2035?"), `decision_context` (2-3 sentences: whose decision it approximates,
  the objective, what the analysis does not determine), `objective`; optional `time_horizon`,
  `evidence_ids` (5-8) and `assumption_ids`;
- computed: the identity (country names, six-stage display names, product and chain names), the true horizon
  and scenario of the kind (posture: 2024-2035 under the IEA World Energy Outlook 2025 Stated Policies, today's
  network with no disruption; deployment: current policies to 2035 against 2030 targets; AIB, compacts and sector
  chains: the bundle's trade period, China removed one-way or concentration capped at a benchmark), the six-stage
  strip, 5-8 neutral facts (value, plain reading by generic conventions such as the US merger-guideline HHI
  bands, source, vintage, inline warnings), analytical assumptions linked to method cards, data warnings
  (trade-only position, trade versus production divergence, missing evidence families, transit hubs, no
  production data, memo unverified or older than the data), what the data cannot show, and definitions;
  tailored sections for partnerships (what each side brings, where each is exposed, the shared problem),
  compacts (problem today, what existing trade covers, the gap left: labelled estimates) and sector chains
  (facts stage by stage).

After the blind answer, `dashboard/src/review/qc/reasoning.ts` adds the reasoning reveal: the conclusion in
one sentence, each analytical step with its level, value against its bar and drivers, the rule actually applied,
and at least one neighbouring alternative (why not, what would make it right, the margin), computed by replaying
the Explorer's own rules.

The brief is linted (`dashboard/src/review/qc/briefLint.ts`): no conclusion label or key of any kind, no model
score or score name, no outcome-giving model label (the physical-capability warning, "coercive", "posture",
"fence", "large yard", "keep open"), no claim about what the Explorer concludes, no banned word. The validator
lints the authored text (titles, questions and `brief:`), and the build fails on any leak in a built brief.
Write briefs that list the possible responses evenly ("protect it, keep and strengthen it, build it up, rely on
trusted partners or diversified suppliers, or leave it to the open market") without the choice names.

## Adding a case (`review_cases.yml`)

1. Pick the subject and check it in the shipped bundle (`dashboard/public/data`): posture cells by
   `{iso3, tech, stage}` (tech as the posture display name, e.g. `Electric Vehicles`), AIB nodes by
   `{node, scenario}` (`decoupling` or `managed`), strategic compacts and sector chains by
   `{sid, scenario}` (`sid` an SC-* strategic compact), partnerships by `{pair}`.
2. Write the `brief:` block and a plain `question` (the same decision question) a domain expert can answer
   without the Explorer ("How should Japan manage grid equipment assembly through 2035?"); never a choice name,
   "posture" or a model term (the validator rejects them).
3. `context_fields`: the snapshot facts shown before the blind answer (import mix, supplier shares, world
   share, RCA, export trend, demand outlook; deployment notes at Deployment & Use; for AIB nodes exposure and
   today's suppliers; for compacts and sector chains the steps, supply at risk and existing-trade coverage).
   Never a revealing id: `choice, fence, posture, reason, confidence, borderline, partner, policy_alignment,
   class, decision, missions, choices_summary, headline, lead_stage, explorer_stages, node_classes` (the
   validator rejects them). Keep the model's deciding scores (need, prize, cp, pathway, coverage after
   coordination) and outputs that presuppose the answer (`physical_warning`, `purpose`,
   `non_allied_relied_on`, `critical_external`; the jargon `coercive_top*` and `opposite_bloc_share`) for
   `reveal_fields`.
4. `reveal_fields`: the conclusion and the rest of the evidence. Leave out facts the bundle never carries for
   that case (e.g. `partner` on a cell without a named partner).
5. `explorer_path`: a real link into main's app (see the header of `review_cases.yml` for the patterns).
6. `why_selected`: one sentence (golden category, threshold margin, centrality to NEIS).
7. Add the ID to the campaign's `case_ids`; to `anchor_cases` only if every reviewer group can judge it.

Sector chains (`kind: sector_chain`, CASE_022 and CASE_023) ask at which stages of one strategic compact's chain
allied intervention is warranted. The Explorer has no single bottleneck output; the build derives the set (the
stages holding a product classed Build capacity, Coordinate trade or Manage specialization) and the lead stage
(where the primary mission's largest lost supply sits), and shows a chokepoint view beside them because dollar
ranking favours downstream stages (`dashboard/src/review/qc/sectorChain.ts`).

The built cases file (`cases.<campaign>.json`) is written as compact JSON since UX v2 (the briefs and reasoning
more than doubled it; compact JSON halves its gzipped size).

Partnership cases need the pair's memo data in `dashboard/public/data/partnerships` (the manifest and the
memo JSON). Main ships only the USA_JPN memo there; the other pairs published on the public site as static
documents (memos.html) cannot be review cases until their memo data are added to that folder.

## Adding a method card (`method_cards.yml`)

Only real, consequential judgment calls of the current code. State `current_choice` in plain language,
quote the stated rationale in `why` where one exists, give a real `example` read from the bundle (with an
`explorer_path`), one to three defensible `alternatives`, the outputs that would move, `code_refs` as
`file:line` in this repository and `config_keys` as `<file> <dotted.key>`. No internal jargon without a gloss.

## Adding a framework scenario (`framework_scenarios.yml`)

Test comprehension, not opinion: describe a situation and ask what the framework implies. With a canonical
`option_set` (`posture_choice`, `aib_class`, `compact_decision`) leave `options: []`; the builder fills
them from the vocabulary constants. Otherwise use `option_set: custom` and list at least two options.
`intended_answer` must be what the code does (`R/strategic_position/decision_engine.R` `sp_choice_rule`,
`R/aib/aib_compacts.R` `aib_classify_nodes`, `R/aib/aib_strategic.R` `aib_strategic_missions`), and
`explanation` says why in one or two sentences.

## Adding a golden case (`../qc/golden_cases.yml`)

Golden cases pin current behaviour so that a change is deliberate; they do not endorse it. Each must pass
on the current bundle. Use `exact: true` with `choice` for central or obvious cases and `choice_in` where a
defined set is acceptable. Numeric ranges are a sensible tolerance around the current value (about +/-0.03
on need, prize and cp; tighter where the case sits on a threshold; capped values such as prize 0.59 or need
0.61 pinned exactly). `change_requires_review: true` is required for `near_threshold`, `surprising`,
`method_sensitive` and `neis_central`. Where the current output looks doubtful the `rationale` says so
plainly. `approved.release` stays null until NEIS approves the case against a data release ID.
