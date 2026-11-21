# Review and golden-case content

Authored content of the QC, validation and expert peer-review system for the published AIB Explorer
(main). The design is `docs/QC_SYSTEM_DESIGN.md`; the types these files map to, one to one, are in
`dashboard/src/review/model.ts`. The files are plain YAML, edited by hand by NEIS.

| File | Type in `model.ts` | What it holds |
|---|---|---|
| `campaigns/<campaign_id>.yml` | `Campaign` | One review round: status, review types, the cases in scope, anchor cases, assignment rules. Empty `method_card_ids` / `framework_scenario_ids` mean "all". |
| `review_cases.yml` (`cases:`) | `CaseDefinition` | The case catalogue: posture cells, AIB nodes, strategic compacts and partnership memos that reviewers judge blind. |
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
- Canonical answer keys: posture `secure, preserve, catch_up, partner, accelerate, watch, not_assessed`; AIB
  node classes as the bundle writes them (`BUILD`, `COORDINATE`, `MANAGE SPECIALIZATION`, `LOW INTERVENTION`,
  `INSUFFICIENT DATA`); strategic compacts `sectoral | narrower | none` (the lead mission is material /
  targeted / absent under the case's scenario). Labels are never typed here: the builder reads them from
  `content/vocabulary.ts`, `posture/grammar.ts` and `review/qc/answerOptions.ts`. "Keep open" is the action
  wording of Watch, never a choice.
- Fact ids (snapshot keys) per case kind are listed in `review/qc/snapshot.ts` (`FACT_IDS`).

## Adding a case (`review_cases.yml`)

1. Pick the subject and check it in the shipped bundle (`dashboard/public/data`): posture cells by
   `{iso3, tech, stage}` (tech as the posture display name, e.g. `Electric Vehicles`), AIB nodes by
   `{node, scenario}` (`decoupling` or `managed`), strategic compacts by `{sid, scenario}`, partnerships by
   `{pair}`.
2. Write a plain question a domain expert can answer without the Explorer ("Which strategic posture is
   closest to what you would recommend for ...?").
3. `context_fields`: the evidence shown before the blind answer (import mix, supplier shares, world share,
   RCA, market outlook; for AIB nodes exposure and today's suppliers). Never a revealing id: `choice, fence,
   posture, reason, confidence, borderline, partner, policy_alignment, class, decision, missions,
   choices_summary, headline` (the validator rejects them). Keep the model's deciding scores (need, prize,
   cp, pathway, coverage after coordination) for `reveal_fields`.
4. `reveal_fields`: the conclusion and the rest of the evidence. Leave out facts the bundle never carries for
   that case (e.g. `partner` on a cell without a named partner).
5. `explorer_path`: a real link into main's app (see the header of `review_cases.yml` for the patterns).
6. `why_selected`: one sentence (golden category, threshold margin, centrality to NEIS).
7. Add the ID to the campaign's `case_ids`; to `anchor_cases` only if every reviewer group can judge it.

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
