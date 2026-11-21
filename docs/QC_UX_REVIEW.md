# QC reviewer UX audit (2026-11-21)

Audit of the current `/review` journey on `feat/qc-review` against the NEIS UX brief. Paths are relative to `dashboard/src/review/` unless they start with `config/`, `docs/` or `dashboard/`. Case content comes from `dashboard/public/data/review/cases.aib_qc_2027_round1.json`.

## Current journey, as walked

| Route | What the reviewer sees | Judgment asked |
|---|---|---|
| `/review` | Campaign `description` ("posture cells, AIB stress-test nodes… structured elicitation", `config/review/campaigns/*.yml:12-17`), four bullets, a storage notice, "Start with your profile" (`views/ReviewHome.tsx:56-101`) | None |
| `/review/profile` | Sectors, stages, methods rated 1-5; countries as ISO3 codes (`views/ProfileView.tsx:254-260`). | Self-rating |
| `/review/cases` | Kind ("Allied supply-chain step"), "Anchor case: several reviewers judge it", "In your expertise" (`views/AssignmentsView.tsx:15-34`) | Choose a case |
| `/review/case/:id` | Steps "The facts → Your answer → The Explorer's conclusion → Your verdict → What it misses → Use and threat → Done" (`views/CaseReviewView.tsx:77-85`) | See below |
| `/review/methods` | 22 cards: "Should the Explorer keep this choice?" (`views/MethodCardsView.tsx:323`) | Method stance |
| `/review/framework` | 12 scenarios in model terms ("need ranks 0.85 (above the 0.67 bar)"): "What does the framework imply?" (`views/FrameworkView.tsx:222`) | Read the rule back |
| `/review/break` | A paste-an-address issue form (`views/BreakModelView.tsx:103`) | Report a break |
| `/review/admin` | Agreement, blind→verdict transitions, taxonomy, robustness, open issues (`views/AdminDashboardView.tsx`) | Internal |

**CASE_001 (KOR · Batteries · Components).** *The facts* shows the question "Which strategic posture is closest to what you would recommend for Korea's manufacturing of battery components (the Batteries / Component Manufacturing node)?" and 14 rows, such as "Largest import supplier: CHN", "Imports of the stage basket / GDP: 0.015% of GDP", "Opposite-bloc share of imports: 85.6%" and "Largest coercive supplier (world supply): CHN". Only the three trade-benchmark rows carry a source. *Your answer* offers seven options with definitions plus "I don't know", a 1-5 confidence scale ("Guess…Certain") and 1-3 driving factors. *Reveal* shows "Preserve · Medium fence · Compete", the reason "a high strategic industrial value (0.79; market 0.85, commons 0.50, option 0.00) and a top-quarter position among contenders (0.76)…", a "Fragile" badge, 17 evidence rows and a trace table ("position | cp contender pct | 0.763 | 0.75 | high"). A domain expert could make this judgment without NEIS help, but only by guessing what "opposite bloc", "coercive" and "trusted" mean, and without being told the horizon or the objective.

**CASE_012 (USA · Nuclear · Deployment; near-threshold, sparse).** The question is "Does the United States need a major push to deploy nuclear power…? Choose the posture closest to your view." The only fact shown before the answer is "Competitive Position among contenders (0-1): 0.966" (`config/review/review_cases.yml:214`): a model score, irrelevant to deployment need. With no deployment evidence the reviewer cannot judge without NEIS help. The reveal reason, "Deployment need 0.60 is below 0.60", looks contradictory (0.597 rounded).

**CASE_014 (rare-earth magnets, AIB node, decoupling).** Clear question with the scenario named; ten neutral facts ("Share of allied traded demand lost to the shock 61.8%", "Top suppliers today… CHN 61.7%; JPN 9.1%…"). But "Physical-capability warning: none" (note: "trade only; no physical data") reads as all clear, and the reveal has no trace, so the BUILD bar (10% gap) is never shown; the reason is "a 61% gap remains after optimized trade". A judgment is possible without help; the reasoning cannot be checked.

**CASE_018 (Battery compact, decoupling).** The question is "…do allied economies need a standing Sectoral Compact across the battery chain, targeted action on a few steps only, or no compact?" The options are defined in terms of "materiality floor", and the "No compact needed" definition describes UI behaviour ("the compact page shows the scenario statement instead of missions"). The facts include "Purpose: Build an allied battery chain…" and "Non-allied suppliers the coalition still relies on: MOZ; SVN". After the reveal, the ThreatStep shows coverage going from 40.7% to 42.0%, a 58% remaining gap and a dependence outcome of "unknown". The reviewer cannot tell whether "Potential Sectoral Compact" means the threat is solved.

**CASE_021 (USA_JPN partnership).** The question asks three things: where to build together, where to rely on trade, and where to stay out. The answer is a single verb (Secure…Watch). The only facts before the answer are "Partnership pair: USA_JPN", two dates and "Memo independently verified: no". The reveal shows "Preserve" with a list of build-together products. A reviewer cannot make this judgment without NEIS help.

## 1. What works well (keep)

- **The blind-first lock.** `flow.ts:116-118` (`canReveal`), `:168-174` (`canEnter`), `:182` (locks the answer), `:321` (a hand-edited draft cannot skip the lock). Revealing ids are dropped even when listed (`manifest.ts:63-65`, `views/CaseReviewView.tsx:116`).
- **Drafts autosave and resume.** `views/CaseReviewView.tsx:370-372`, `ReviewLayout.tsx` ResumeLink. Submissions carry the snapshot sha and release stamps (`:296-326`).
- **Honest gaps.** "not available in this release" (`views/CaseReviewView.tsx:73`); ThreatStep's "not modelled: the Explorer has no lead-time data" (`views/ThreatStep.tsx:73`).
- **Robustness is kept separate from data quality**, and its flips already answer "what would change it" (`views/CaseReviewView.tsx:211-259`).
- **Option definitions are canonical** (`qc/answerOptions.ts`), and "I don't know" is always offered.
- **The AIB node and compact questions are concrete** and name the scenario in plain words (`config/review/review_cases.yml:238, 286, 304`).
- **The admin aggregation:** taxonomy, transitions and priority fixes (`aggregate.ts:229, 343, 534`).

## 2. Major UX problems

1. **No case brief.** The context step is the question plus a flat fact list (`views/CaseReviewView.tsx:450-464`), with no decision-maker, objective, horizon or scenario. The stage appears only as text, e.g. "KOR · Batteries · Components" (`:99-113`).
2. **Facts have no interpretation, source or vintage.** Only the benchmark rows carry a source (`qc/snapshot.ts:230-232`). Country codes are shown raw.
3. **No split between evidence and assumptions.** The trusted, opposite-bloc, coercive and allied groupings are model definitions but are presented as facts.
4. **The reveal gives the answer, not the reasoning.** Posture cases get a raw trace table that prints code names (`:283`). AIB nodes, compacts and the partnership case get no trace at all.
5. **No "why this, not the neighbour".** The material is there, in `why_selected` and in the robustness flips, but is not framed that way.
6. **The verdict mixes prior, validity and persuasion.** Option 1 is "Matches my prior assessment." (`model.ts:233`). The scale cannot say "same conclusion, different reasons", "decision rule wrong" or "too broad".
7. **The partnership case does not work as a review task.** It asks a three-part question, takes a one-verb answer and shows no evidence first.
8. **The home page leads with internal structure.** The nav reads "Methodology / Framework / Break the model" (`ReviewLayout.tsx:131-138`); the progress strip and tiles count cards and scenarios (`:171-175`, `views/ReviewHome.tsx:109-114`).
9. **No calibration, notes or completion summary.** The done step offers only "Next case" or "All your cases" (`views/CaseReviewView.tsx:674-695`).
10. **"Insufficient evidence" leads nowhere.** "I don't know" goes straight to verdict 7 (`flow.ts:194`). `evidence_wanted` is hidden free text (`views/CaseReviewView.tsx:618-627`) that `aggregate.ts` never reads.

## 3. Where reviewer judgment is under-contextualised

- **Model scores shown before the answer.** CASE_012 shows only `cp` (0.966). The "More of the Explorer's measures" factor disclosure (`views/CaseReviewView.tsx:490-500`; `factorOptions` at `:119-127`) shows labels without values. Those labels are the rule's own inputs, and some name the outcome: "Supply-chain vulnerability (Strategic Need, 0-1 percentile)", "Large-yard (commodity) signals", "Remaining gap after coordinated trade (new capacity needed)", "Hosts of new capacity", "Steps by treatment". They confuse a newcomer and hint at the outcome.
- **Model-derived labels shown as facts.** "Largest coercive supplier", "Opposite-bloc share", "Trusted partners' share of world supply (less own)" and "Prize note" (`qc/snapshot.ts:61-79`). On compacts: "Purpose: Build…", "Coverage with existing relationships" (an optimiser output) and "…the coalition still relies on" (`:120-125`).
- **No horizon or objective.** No case states either; the prize note's "2024-35" is the only date shown.
- **Facts without a scale.** "0.015% of GDP" (`qc/snapshot.ts:226-228`) and "HHI 0.400" give no point of comparison. "Physical-capability warning: none" hides that there is no physical data.
- **Answer options that need explaining.** "Not assessed: the data cannot support a call" describes the model, not the reviewer. The AIB option "Not enough evidence" is a model class that reads like "I lack evidence". The compact options rely on "materiality floor".
- **No Sector case.** The 21 cases cover four kinds, but none is a `sector` case (`views/AssignmentsView.tsx:20`).

## 4. Where the interface over-explains

- Notes written for engineers. Before the answer: "blended with the latest 12 months of trade (weight 0.5, own_monthly, window ending 202512)". On the reveal: "postInvestment = coordinated + min(increment, gap) by construction…", "the AIB bundle carries no git commit" and "compact/decide.ts is redesign-only".
- The home page shows the storage-mode notice, the download block and "Note for NEIS…" (`views/ReviewHome.tsx:72-74, 115-128`) before any case.
- The extras step asks for a lot. Decision use is four 1-4 scales plus four text boxes (`views/DecisionUseStep.tsx:13-22`); threat is four scales plus three text fields (`views/ThreatStep.tsx:15-23`). CASE_018, which has both, runs to about 20 inputs after the reveal.

## 5. Ambiguous questions

- "Which strategic **posture** is closest…" (all 13 posture cases). The options are *choices*, while the reveal shows a separate "Posture: Compete". The two terms collide.
- "…the Batteries / Component Manufacturing **node**": "node" is internal jargon.
- "Where should the United States and Japan build… where should they rely on trade… and where should they stay out?" (`config/review/review_cases.yml:355`) cannot be answered with one option.
- "Which comes closest to your view of the Explorer's conclusion?" (`views/CaseReviewView.tsx:573`), combined with "Matches my prior assessment.": it is unclear whether this asks about agreement or about evidence.
- "What drives your answer most? Pick 1 to 3." The options are metric labels, not reasons.
- "What does the framework imply?" (`views/FrameworkView.tsx:222`) tests reading the jargon back.
- "Does it keep the diagnosis (what is wrong) apart from the prescription (what to do)?" (`views/DecisionUseStep.tsx:17`): abstract.
- "How confident are you?" on a five-point scale whose lowest point is labelled "Guess" (`components/form.tsx:192`). The brief asks for Low/Medium/High.

## 6. Recommended fixes (mapped to brief sections)

| § | Fix | Tag |
|---|---|---|
| 3 | `CaseBrief`: identity (`STAGE_DISPLAY`, country names), decision question, context, horizon, scenario (`SCENARIO_NAME`), six-stage strip; `brief` fields in `review_cases.yml` | add |
| 4 | 5-8 facts per kind with value, plain reading, source, vintage and inline warnings ("No physical production data: trade only"); replace CASE_012's `cp` with deployment evidence | change |
| 5 | "Evidence" vs "Analytical assumptions" (partner-group definitions, scenario, thresholds) via a `kind` on `CaseFact` | add |
| 6 | "Based on the evidence above, what broad strategy would you recommend?"; CHOICE_NAME + `CHOICE_ACTION` one-liners (Watch = "Keep open…"); add None / Insufficient evidence / Outside expertise; drop "Not assessed"; Low/Medium/High; concept-level drivers | change |
| 7 | Need / Prize / Position, each a level plus one sentence, then "Therefore" in prose (`traceTestWords`, `decisionPath`); AIB and compact class bars stated | change |
| 8 | "Why X, not Y?" and "What would make Y more appropriate?" from the flips and the rule's next branch | add |
| 9 | Ten-way taxonomy stored beside `verdict`, with a mapping for `aggregate.ts` | change |
| 10 | Two red-team prompts on disagreement or low confidence; admin "Evidence gaps" panel | add |
| 11 | Per-kind layouts: Country (build/preserve/partner/open), Compact causal chain from `ThreatTable` data, Partnership "A brings / B brings" with CASE_021 re-posed; Sector needs a case | add |
| 12 | Level 2 is "Why this result?", reusing `WhyThisResult`. Level 3 is "Inspect the analysis" plus "Open in the Explorer". | change |
| 13 | A mode flag in the profile: Decision (the default) or Technical, which opens Level 2 by default | add |
| 14 | One CTA, "Review your assigned cases"; Methodology/Framework/Break under "Want to go deeper?"; storage and download lower | change |
| 15 | A one-screen calibration before the first case (managed interdependence; Partner and Watch can be successes) | add |
| 16 | An autosaved notes panel per case, convertible to a data issue or comment through `IssueReportDialog` | add |
| 17 | "Insufficient evidence" opens structured gap categories and stores them | add |
| 18 | Separate prior (derived automatically from blind vs conclusion), validity and persuasion questions | change |
| 19 | A completion summary built from the submissions (matched, changed view, issues raised) | add |
| 21 | A lint for context fields: no model scores (`need`, `prize`, `market`, `cp`, percentiles) and no outcome-naming labels in the factor options | add |

## 7. Components to reuse rather than rebuild

- Flow and storage: `flow.ts` (add steps), `storage.ts`, `sink.ts`, `context.ts`. Forms: `components/form.tsx`.
- Facts: `FactList` in `views/CaseReviewView.tsx`; `qc/snapshot.ts` `fact()`, which already carries `source`, `source_year` and `note`.
- Reasoning: `components/WhyThisResult.tsx` with `qc/provenance.ts` (as used in `posture/CellDrawer.tsx:57`), `qc/choiceRule.ts` `decisionPath`, the `qc/sensitivity.ts` flips, `posture/grammar.ts:359` `traceTestWords`.
- Compact chain: `ThreatTable` and `threatFigures` in `views/ThreatStep.tsx`.
- Words: `content/vocabulary.ts` (`CHOICE_ACTION`, `SCENARIO_NAME`, `AVOID_RE`), `lib/stages.ts` `STAGE_DISPLAY`, `components/Term.tsx`.
- Issues and admin: `components/IssueReportDialog.tsx`, `aggregate.ts`, `components/ui.tsx` (`Disclosure`, `StatTile`).

## 8. Blind-first risks to watch while implementing

- **Leaky briefs.** Never paraphrase `why_selected`; write from the question and lint with `AVOID_RE`.
- **Interpretation as persuasion.** "High concentration: a security concern" pre-judges Need; describe magnitude only.
- **Level 2 too early.** Gate `WhyThisResult` on `canReveal` inside the component, not only by step.
- **Outcome-naming option labels** ("new capacity needed", "Hosts of new capacity") must not reach driver or gap options.
- **Compact "Purpose" and "coverage with existing relationships"** are model outputs: move after the lock or reword.
- **Verdict migration.** Keep `verdict` and `snapshot_sha256`; add the taxonomy beside them.
- **Notes span the reveal.** Timestamp them so pre- and post-reveal notes stay distinguishable.
- **"It matches / It differs from the Explorer"** (`views/CaseReviewView.tsx:536`) stays after the lock.

## Walkthrough of real cases (after implementation)

To be completed after implementation.
