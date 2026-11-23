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

**Method.** Each case below was rendered through the real routes (`routes.tsx`, `createMemoryRouter`) from the built
`cases.aib_qc_2027_round1.json`. A throwaway ui-project test drove the flow: it stored a profile and the calibration
flag, opened `/review/case/<id>`, answered the blind question, locked it, chose a verdict and continued to Done. It
dumped the visible text of `<main>` and the number of form controls at every step. The test and its dumps were kept
outside the repository and deleted afterwards. Quotes are verbatim and come from the final render, after the fixes
listed at the end of this section. CASE_003 ran in Technical review mode; the others ran in Decision review mode.

| Need | Case | Why this one |
|---|---|---|
| Obvious | CASE_002 JPN · Electric Vehicles · Refining (Secure) | The golden Secure case: China holds 94.4% of world supply and 47.2% of Japan's imports. |
| Genuinely ambiguous | CASE_008 JPN · Electric Grid · Assembly (Catch Up) | Need, prize and position all sit within 0.05 of their bars, and the call has low confidence. |
| Counterintuitive result | CASE_003 AUS · Batteries · Extraction (Watch) | Watch, although Australia holds 16% of world supply and 44% of world exports. |
| Sparse data | CASE_012 USA · Nuclear · Deployment (Watch) | Only two of the three deployment evidence families are measured, and the result is 0.597 against a 0.6 bar. |
| Trade and production diverge | CASE_016 polysilicon, AIB node, decoupling (Low intervention) | China is 11.7% of allied traded supply but holds 97% of operational capacity. |
| Partnership | CASE_021 USA_JPN | The only shipped memo, re-posed as "what should lead cooperation". |
| Compact | CASE_018 SC-BATTERIES, decoupling (Potential Sectoral Compact) | Central to NEIS: US$108bn at risk and 10 of 14 products classed for new capacity. |
| Sector chain | CASE_022 SC-SOLAR, decoupling (lead stage: Final Assembly & Equipment) | Dollar ranking puts the lead at modules, while the upstream chokepoint is visible only in production data. |

The journey before any case is identical for all eight: Home → profile (with the review mode) → "Before you start"
(seven points) → the case. Every case then runs **The case → Your assessment → The Explorer's reasoning → Your
verdict**. After that come Test the conclusion (only after a disagreeing verdict or a low-confidence answer), the
product step (all products except Country), Use and threat (only on cases with those questions), Your notes (only
when notes exist) and Done.

### CASE_002: obvious (Japan, rare-earth and battery-metal refining)

- **The case.**
  - Question: "How should Japan manage rare-earth and battery-metal refining for electric vehicles through 2035?"
  - Header: "Objective: Resilient and affordable supply, not maximum national self-sufficiency" · "Time horizon: Through 2035" (with "Market outlook 2024 to 2035; import shares 2024 blended with the 12 months to Aug 2026…") · "Scenario: Today's network".
  - The stage strip marks "Refining & Processing · this case".
  - Warnings come first: "Trade and production evidence disagree. Japan holds 1.8% of world supply but 9.0% of world exports…".
  - Then eight facts under neutral headings (Where supply comes from / Domestic capability / Alternative suppliers / Market outlook), each with a reading, a source and a vintage. For example: "China's share of world supply… 94.4% · Reading: a majority of world supply in one economy" and "World supply held by economies in Japan's UN-voting group (excluding Japan) 4.0%".
  - Three assumptions are shown and five more are folded ("Vulnerability is a rank, not a level…"), each with "Read the method card".
- **Your assessment.**
  - Prompt: "Based on the evidence above, what broad strategy would you recommend?"
  - Six definitions (Secure … "Watch (keep open)"), then "Or, if you cannot recommend one of these:" with the three reviewer states.
  - Choosing an option opens Low / Medium / High confidence and "What is the most important factor driving your judgment?". 24 controls.
- **Reveal.**
  - "Your answer: Secure (confidence: high). It matches the Explorer." → "Explorer conclusion: Secure".
  - The step chain: "Security test Yes" (both routes spelled out), then "Need… High · Japan ranks above 99% of economies… (Not used: an earlier test decided.)".
  - "Therefore: Secure · …The rule takes the first test that matches, in the order shown."
  - "Why Secure rather than Catch Up?" with "Decisive margin: rival suppliers 88.5% of imports vs a quarter; same-group world supply 4.0% vs half".
- **Verdict.** "You answered Secure; the Explorer concluded Secure. They match." Then the ten statements, validity and persuasion. 18 controls, and no further step: four screens in all.
- **Judgment possible.** Yes, without NEIS help. The concentration facts carry the case.
- **Conclusion revealed early?** No. The brief never names a choice. It does state the security test's routes as an assumption ("A separate security test comes first…"), so a reader who applies the rule can reconstruct Secure. That is the rule, not the answer; see the NEIS questions below.
- **Missing evidence.** Import values in dollars ("Import values in US dollars for this step are not in this release.") and whether the world-supply share is production or exports ("Measure not stated").
- **Technical access.**
  - Level 2 gives each step's value against its bar (e.g. "Need: import vulnerability: High (value 0.998, bar 0.67)") with drivers.
  - Level 3 gives the test-by-test table, every figure the Explorer holds, the provenance chain and "Open in the Explorer".
  - Level 3 still prints codes and engineering notes ("Largest import supplier CHN", "weight 0.5, own_monthly"). That is acceptable at that level.
- **Burden.** Low for an agreeing reviewer. About 1,400 words of brief, but the folded assumptions and the Definitions block can be skipped.

### CASE_008: genuinely ambiguous (Japan, grid equipment assembly)

- **The case.**
  - Question: "How should Japan manage the final assembly of grid equipment (transformers, switchgear and cables) through 2035?"
  - The context sentence warns: "Grid equipment has long lead times, which the analysis does not measure."
  - Facts: "China, 51.6% of imports · Reading: a majority from one supplier"; "59.2% (2024: 58.4%; latest 12 months: 60.1%)" from China, Russia and the other UN-voting group; Japan "2.9%" of world supply.
- **Reveal.**
  - "Explorer conclusion: Catch Up" with "Confidence in the call: low".
  - Three neighbours, each with its margin:
    - "Why Catch Up rather than Partner? … Decisive margin: Prize 0.613 vs bar 0.6 (+0.013)"
    - "…rather than Preserve? … Position 0.703 vs bar 0.75 (−0.047)"
    - "…rather than Watch (keep open)? … Commodity signals 3 of 5 needed"
  - Level 2 robustness: "Fragile · Prize threshold 0.65 (+0.05): Partner · Competitive Position bar 0.7 (-0.05): Preserve".
- **Judgment possible.** Yes. This is the case where the reveal helps most: a reviewer who answered Partner or Preserve sees exactly which bar separates them from the Explorer and can pick "7. I disagree with how the evidence is interpreted or weighted" or "8. …decision rule…", then tag "Step: Prize" in "Which facts, assumptions or steps are involved?".
- **Revealed early?** No.
- **Missing evidence.** Lead times and the order backlog, which the brief itself says are not measured. The prize is the same 0.613 for all 214 economies, so the Prize step is a property of the step rather than of Japan. The reveal shows the value but not that it is shared. See the findings.
- **Burden.** The verdict step grows to 42 controls when "Where exactly does it fail?" opens, because the fact, assumption and step checkboxes list everything. It is optional but long.

### CASE_003: counterintuitive Explorer result (Australia, battery mineral mining; Technical review mode)

- **The case.**
  - The warnings set up the surprise without stating it:
    - "Trade and production evidence disagree. Australia holds 16.0% of world supply but 44.1% of world exports"
    - "Large producer. Australia holds 16.0% of world supply, so its imports are a small part of its own supply."
  - The fact "World supply held by economies in Australia's UN-voting group… 0.0%" carries its own warning: "Implausible zero: …it may reflect the measure used for this step rather than real supply."
- **Reveal.**
  - "Explorer conclusion: Watch (keep open)" → "Australia's battery mineral mining: keep open: buy competitively, do not force localization."
  - The chain:
    - "Commodity test No · 4 commodity signals hold; the test needs 5"
    - "Need… Low · Australia ranks above 12%…; its import mix is not scored because it holds 10% or more of world supply"
    - "Prize… Held below the bar: market size alone"
    - "Position… High (top quarter) · 0.864"
  - "Therefore: … Neither vulnerable nor high value: keep it open; a strong position alone does not justify intervention."
  - "What would make Preserve more appropriate? … if the value were not capped as 'market size alone' (industrial know-how or a future option scoring 0.5 or more)."
  - Technical mode opens Level 2 by default (`<details open>`).
- **Judgment possible.** Yes. The reveal names the exact rule a mining expert would contest (the prize cap and the large-producer guard), so "8. decision rule" with "Step: Prize" is a precise objection.
- **Revealed early?** No. The assumption "What makes a step valuable… market size alone cannot make the value high" is visible before the answer: it is a rule a reviewer needs to judge the case, not the outcome.
- **Defect found and fixed.** Level 2 listed "signals that hold: mature, passive, substitutable, easy qualification, low spillover, low differentiation" (six signals) under "4 commodity signals hold". The two required signals are not counted. It now reads "required signals that hold (all are needed; not counted): mature, passive" and "counted signals that hold (4 of 5 needed): substitutable, easy qualification, low spillover, low differentiation".
- **Remaining confusion.** The reveal header prints "Low fence · Deploy". The Secure/Compete/Deploy posture word "Deploy" on a mining cell is never explained to a reviewer. Reported, not fixed.

### CASE_012: sparse data (United States, nuclear deployment)

- **The case.**
  - Question: "Does the United States need a major push to deploy nuclear power at home, faster than current policies imply, by 2030-2035?"
  - "Time horizon: Through 2035, against 2030 national targets" and "Scenario: Current policies".
  - Warning: "Part of the evidence is missing. 2 of the 3 evidence families (demand, 2030 targets, system needs) are measured for United States."
  - The target fact reads "Not measured for United States", with its own "Evidence missing" warning.
  - The audit's v1 problem, the model score `cp` 0.966 shown before the answer, is gone.
- **Your assessment.** "Insufficient evidence to judge" opens "What information is missing?" with 16 categories, then "Anything more specific? (optional)". The confidence and driver questions are not asked. 28 controls.
- **Reveal.**
  - "Your answer: Insufficient evidence to judge"
  - "Deployment need · Moderate · Deployment need is 0.597 against the bar of 0.6; not measured: gap to 2030 national targets."
  - "Decisive margin: Deployment need 0.597 vs bar 0.6 (−0.003)"
  - The Explorer's own words stay quoted: "Deployment need 0.60 is below 0.60". The step chain now shows the unrounded value beside them.
  - Level 2: "Fragile · Deployment threshold 0.55 (-0.05): Accelerate".
- **Verdict.** "There is no recommendation of yours to compare." The red-team step follows, because the blind answer was not a confident recommendation.
- **Judgment possible.** Only partly. That is the honest outcome, and the flow records it as an evidence gap rather than a guess.
- **Defects found and fixed.** "United States's nuclear power" now reads "United States' nuclear power". At Deployment the security step said "…so value and position decide"; it now says "…so deployment need decides".
- **Remaining issues.**
  - The answer list offers Secure, Preserve, Catch Up and Partner for a yes/no deployment question, although the Deployment & Use rule can only give Secure, Accelerate or Watch (keep open).
  - "Confidence in the call: moderate" sits beside a "Fragile" robustness badge.
  - "import displacement: coal 16% x 0.00; gas 40% x 0.00" is an engineering string in a pre-answer fact.

### CASE_016: trade and production evidence diverge (polysilicon, China stops supplying)

- **The case.**
  - The question names the scenario and the response types: "If China stopped supplying polysilicon to the 59 allied economies (while still buying from them), what kind of allied response would be needed…".
  - "Time horizon: Immediate re-sourcing on 2025–26 trade… Build times for new capacity are not modelled anywhere in the Explorer."
  - The first warning: "Trade and production evidence disagree. Production is more concentrated than trade: China holds 97% of operational production capacity, against 10% of exports."
  - Facts: "Largest suppliers to allied economies today: United States 41.6%; Germany 18.4%; China 11.7%…" and "Concentration (HHI)… 0.238 · Reading: highly concentrated by the US merger-guideline bands (above 0.18)".
- **Reveal.**
  - "Explorer conclusion: Low intervention" → "Polysilicon if China stops supplying: the disruption removes under 15% of allied supply, so the product needs little intervention."
  - The chain shows every bar, including the ones that did not decide: "Concentration after coordinated trade · Concentrated · The largest supplier after… (United States) holds 46.9%; at 40% or more… the reliance needs managing".
  - "Under other settings · Differs" (three other shocks give Manage specialization).
  - "Production data check · Not validated".
  - "Why Low intervention rather than Manage specialization? The exposure is 11.7%, below the 15% bar".
- **Judgment possible.** Yes, and well located: an expert who thinks traded share understates the risk can point at the exposure gate (Step: Supply exposed) or the assumption "Only traded supply counts".
- **Revealed early?** No. "PHYSICAL CAPABILITY NOT VALIDATED" appears only after the answer.
- **Defects found and fixed.**
  - The production-data fact said only "Production data cover the main producers of this product.", while the reveal said exporters holding 15% of world trade are absent from those data. The fact now adds "Reading: exporters holding 15% of world trade are not in the production data; the analysis holds them at their current shipments".
  - The summary ended with "..".
- **Remaining.** Level 2 still lists "the AIB bundle carries no git commit" under data quality. Level 3 notes keep field names ("residual x demandUsd"); that is acceptable at Level 3.

### CASE_021: partnership (United States × Japan)

- **The case.**
  - Question: "Over the next 12 to 36 months, what should lead cooperation between the United States and Japan on clean-energy supply chains: new production capacity built jointly…, recognising each other as trusted suppliers…, upgrading the agreements they already have, or no new joint commitments?"
  - Warnings: "Rule-generated memo… has not been independently verified" and "Memo older than the data… 2026-10-30… 2026-11-17".
  - "What each side brings": "United States brings / United States depends on one supplier for / Japan brings / Japan depends on one supplier for / Shared strategic problem", each a list of concrete steps with shares.
- **Your assessment.** The four memo boxes, defined: "Build together · Lead with new production capacity built jointly on specific steps…", "Trade and standards", "Institutions", "Stay out".
- **Reveal.**
  - "Explorer conclusion: Build together" → "The memo leads with build together: Accumulator parts & separators, Natural graphite, Rare-earth permanent magnets, Wind turbine generating sets, Polysilicon, Rare-earth oxides & compounds."
  - "Consistency check · 4 products appear in both the first and the second box".
  - The rule is stated honestly: "Its first box is always the joint-build list, so the headline is the same for every pair with any joint build."
  - "What coordination changes" shows "Polysilicon: … 8.2% needs new capacity (low intervention)", which sits in the joint-build list.
- **Product step.** "Does this relationship represent genuine industrial complementarity or primarily diplomatic compatibility?" plus four optional questions.
- **Judgment possible.** Yes, on the evidence. The v1 three-part question is gone.
- **Defect found and fixed.** The shared problem read "rare-earth and wind materials refining: both depend on one supplier for half or more of imports (… Japan: China 42.6%)". 42.6% is not half; Japan qualifies through the rival-group share. It now reads "…both depend on one supplier, or on China, Russia and the other UN-voting group, for half or more of imports (… Japan: China 42.6%, China, Russia and the other UN-voting group 75.9%)".
- **Missing evidence.** The "Institutions" option asks about "upgrading the agreements they already have", but the brief never lists those agreements. The memo names the Japan-US Strategic Investment Initiative only after the reveal. The reveal also shows "KPI", "MOU" and "Japan->US" verbatim from the memo.
- **Burden.** The heaviest case: 11,000 characters of brief and five post-reveal screens (verdict, red team, the relationship, use, done).

### CASE_018: compact (battery chain, China stops supplying)

- **The case.**
  - Question: "…do they need a standing multi-country arrangement across the whole chain, joint action on a few steps only, or no joint arrangement?"
  - The stage strip marks Extraction to Component Manufacturing and Recovery "in this chain". Seven warnings follow: one is the trade-only count and six are trade versus production divergences, e.g. "Cathode materials & precursors: …China holds 92% of operational production capacity, against 42% of exports". After them comes "1 product has too little trade data to test." (fixed from "1 products").
  - "The problem, step by step":
    - "1. Problem today: US$108.3bn of US$182.4bn a year (59.4%)"
    - "2. What existing trade can cover · Estimate · 42.0%"
    - "3. Remaining gap after existing links · Estimate · 58.0% of allied demand (US$105.8bn a year)"
    - "The rest of the chain… is shown after your answer."
- **Reveal.**
  - "Explorer conclusion: Potential Sectoral Compact" → "Battery Materials & Cells: the lead mission (build capacity) carries 99.8% of the supply at risk (US$108.1bn), so the Explorer proposes: Potential Sectoral Compact."
  - The rest of the chain follows:
    - "6. Countries capable of adding it · Estimate"
    - "7. Proposed coalition · Estimate · 39 economies… Non-allied suppliers the coalition still relies on: Mozambique, Slovenia."
    - "8. Expected resilience after · 49.4% of allied demand met; 50.6% still unmet · …arithmetic by construction, not a finding."
  - "Decisive margin: share 99.8% vs 25%; US$108.064bn vs US$1.000bn".
- **Post-reveal.** The red team, then "Does it help? · Does this compact materially ameliorate the stated threat?", then Use and threat (48 controls, all optional, with "Add detail (optional)" folded).
- **Judgment possible.** Yes. The reviewer can see that the materiality test is passed by a factor of 100, so the disagreement is with the rule (Targeted action vs Compact) rather than the facts.
- **Defects found and fixed.** The threat step showed field names, codes and repository paths: "time_to_capacity_years: …null, never estimated", "postInvestment = coordinated + min(increment, gap) by construction (docs/aib_methodology.md 16.3)", "new capacity at JPN, ARG, COD… (3 BUILD step(s) with tied hosts: none named)" and "relies on non-allied suppliers: MOZ, SVN". They now read in words with country names, e.g. "Time to build new capacity is not modelled anywhere in the Explorer…", "coordinated trade; new capacity in Japan; Argentina; Congo, Dem. Rep.; …", "After the intervention the coalition still relies on suppliers outside the allied group: Mozambique; Slovenia."
- **Burden.** The longest post-reveal path: five screens after the answer, about 80 controls, most of them optional.

### CASE_022: sector chain (solar manufacturing, China stops supplying)

- **The case.**
  - Question: "…at which stages is allied intervention warranted? Select every stage that needs it, then the one you would act on first."
  - "The chain stage by stage" gives, per stage, the products, allied imports, supply lost and China's share by product. Examples: "Polysilicon: China 11.7%; Wafers: China 37.9%…" and "PV modules & generators: China 68.2%".
  - Production coverage and divergence warnings are given inline per stage.
- **Your assessment.** Six stage checkboxes ("The stages are the six of the supply chain; the strip above marks the ones this chain covers"). Then "Which of them would you act on first?" with only the ticked stages, plus "No stage needs allied intervention" and the three reviewer states.
- **Reveal.**
  - "Your answer: Refining & Processing (stages: Refining & Processing, Component Manufacturing; first: Refining & Processing)… It differs from the Explorer." → "Explorer conclusion: Final Assembly & Equipment".
  - A stage strip marks "needs allied action" and "acts here first".
  - "Chokepoint view: …Polysilicon (Refining & Processing): production is more concentrated than trade."
  - "Why Final Assembly & Equipment rather than Refining & Processing? …the Explorer ranks by dollars. Polysilicon is read from trade (China 11.7% of allied supply) and classed low intervention, although China holds 97% of operational production capacity".
- **Verdict.** "Stages you selected: … The Explorer's: … Overlap 67%." Then "The bottleneck · Has the Explorer located the actual strategic bottleneck in this supply chain?".
- **Judgment possible.** Yes. This is the case type that most directly tests "where exactly it fails": the reviewer can agree on the set and disagree on the ranking.
- **Defects found and fixed.**
  - "3 stage(s) hold products…" now reads "3 stages hold products…".
  - The admin aggregate and the CSV export compared the reviewer's stage keys with the reasoning's display labels. Every sector-chain answer counted as "differs", and the stage overlap was always 0. Now fixed: `flow.ts` `stageKeyOf`.
- **Burden.** The three divergence warnings appear three times: as data warnings, inline per stage, and as an evidence fact. They are worth stating, but the repetition lengthens the brief to about 12,000 characters.

### Across the eight cases

- **Blind-first holds.** No conclusion, conclusion label, model score, robustness flip or `why_selected` text appears before the answer in any case. `CaseReviewView.real.test.tsx` checks all 23 built cases; the throwaway walk saw the same.
- **Every case can be answered from its brief.** CASE_012 is the exception by design, and the insufficient-evidence branch captures what is missing.
- **The reveal locates failure points.** Every case names the rule actually applied, the neighbouring conclusion and the decisive margin. The verdict step lets the reviewer tag the fact, assumption or step involved.
- **Burden is front-loaded.** Briefs run 6,000 to 12,000 characters. `caseMinutes` budgets 6 minutes (+2 for a partnership, decision use or threat), which looks short for a first-time reader of a 12,000-character brief.

### Fixes made during the walkthrough (each with a test)

| Defect | File | Test |
|---|---|---|
| "4 commodity signals hold" listed six signals (the two required signals are not counted) | `qc/reasoning.ts` (now uses `posture/yards.ts` `largeYardOf` / `splitSignals`) | `qc/reasoning.test.ts` "walkthrough wording" |
| "ranks above 100% of economies" (percentile 0.998 rounded) | `qc/reasoning.ts` | same |
| "United States's …" (also "position", "share", "imports") | `qc/reasoning.ts` `poss()` | same |
| At Deployment: "…so value and position decide" | `qc/reasoning.ts` | same |
| AIB summary ended "..", compact summary "so potential sectoral compact." | `qc/reasoning.ts` | same |
| "1 products", "3 stage(s)" | `qc/brief.ts`, `qc/reasoning.ts` | `qc/brief.test.ts`, `qc/reasoning.test.ts` |
| Production-data fact hid that exporters with 15% of world trade are missing | `qc/brief.ts` | `qc/brief.test.ts` (sector) |
| Demand fact's "Reading" was the investment figure | `qc/brief.ts` | `qc/brief.test.ts` (entrepot / zero share) |
| Shared dependence showed China 42.6% as "half or more" | `qc/brief.ts` | `qc/brief.test.ts` (partnership) |
| Compact threat notes printed field names, repo paths and codes | `qc/threat.ts` | `qc/reasoning.test.ts`, `qc/threat.test.ts` |
| Sector-chain agreement and overlap compared stage keys with labels | `flow.ts` `stageKeyOf` | `flow.v2.test.ts` |

The review data were rebuilt after the fixes. The rebuild is byte-identical across two runs with the same `--built-at`.

## Before vs after

| | Before (v1) | After (v2) |
|---|---|---|
| Entry | Campaign text, four bullets, storage notice, "Start with your profile"; nav led with Methodology / Framework / Break the model | "Help us break the AIB Explorer", the reviewer's own time ("~20 minutes · 3 cases"), one button "Review your assigned cases"; the deeper tasks under "Optional:" and "Want to go deeper?" |
| Calibration | None | "Before you start": seven points (managed interdependence; dependence is not automatically a problem; Partner and Watch (keep open) can be successes; challenge false alarms and missed risks) |
| Before the answer | Question + 3-14 flat facts, raw codes, one model score on CASE_012 | A brief: decision question, context, objective, true horizon, scenario, stage strip, data warnings, 5-8 facts with reading/source/vintage, assumptions apart from evidence, what the data cannot show, definitions, product-specific sections |
| The answer | Seven options + "I don't know", 1-5 confidence, 1-3 metric "drivers" | Conclusions with one-line definitions (Watch shown as "Watch (keep open)"), then None of these / Insufficient evidence (with gap categories) / Outside my expertise; Low/Medium/High; one concept-level driver; sector chains pick stages then a first stage |
| The reveal | Conclusion, reason string, trace table with code names; no trace for AIB, compact or partnership | One-sentence summary, the Explorer's own words, the step chain with levels, "Therefore", product remainder, "Why X rather than Y?" / "What would make Y more appropriate?" with margins, Level 2 "Why this result?", Level 3 "Inspect the analysis" |
| Verdict | 1-7 scale mixing prior, validity and persuasion | Prior computed, ten statements, validity and persuasion asked separately; disagreement asks where it fails and which facts/assumptions/steps |
| After the verdict | "What it misses" + decision use and threat (about 20 inputs) | Red team only on disagreement or low confidence; product questions; extras folded under "Add detail (optional)"; notes converted at the end |
| End | "Next case" / "All your cases" | Summary: cases reviewed, matched, changed view, raised a data issue; "Nothing more is required." |
| Admin | Agreement, transitions, taxonomy | Plus Verdicts, Prior/validity/persuasion, Evidence gaps, Product questions tabs; R report sections 7-9 |

## How contextual evidence is selected

1. **Per kind, from the bundles.** `qc/brief.ts` builds a fixed set of candidate facts per kind (`BRIEF_FACT_IDS_BY_KIND`). Each fact carries a value, a plain reading, a source, a vintage and its own warnings.
   - Country cells: largest supplier, China or rival-group import share, own world share, export position, export trend, the major world supplier's share, the same-group share, import scale, demand outlook, technology readiness.
   - Deployment: installed trend, demand, target gap, system need, climate context.
   - AIB nodes: allied imports, China's share or the share to shift, supply at risk, top suppliers, concentration, top-3, production data, data quality.
   - Chains: steps, imports, supply at risk, per-step suppliers, existing-trade cover, gap, untested products, production gaps, divergence.
   - Partnerships: the four complementarity counts, the shared allied problem, memo status.
2. **5-8 by default, authored when needed.** Each kind has a default selection. The case's `brief.evidence_ids` can replace it, and the validator checks that the ids exist.
3. **Readings use generic conventions, never the Explorer's bars.** These are the US merger-guideline HHI bands, supplier-share words, RCA classes and annual growth rates. A score the model computes (need, prize, Competitive Position, percentiles, coverage after coordination) is never a fact.
4. **Assumptions are separate.** Supplier groups, the vulnerability rank, the security test, the prize rule, the trade-only stress test and the one-way cut are listed as "Analytical assumptions… If one of these is wrong, that is a method problem", each linked to its method card.
5. **Warnings are computed from the data.** Trade versus production divergence, trade-only positions, two-period import shares, implausible zeros, missing evidence families, transit hubs, memo status and age.
6. **What stays for the reveal.** Model-derived labels that presuppose the outcome: the physical-capability warning, "coercive", "opposite bloc", compact purpose and coalition, capped-prize and need-floor states, the commodity test. Leaking `context_fields` were moved to `reveal_fields`.

## How blind-first independence is preserved

- **Flow.** `flow.ts` (v2) refuses any step after the answer unless a valid blind answer is stored. That covers goto, back, a restored draft and a hand-edited draft; an edited draft returns to the blind step with every reveal field cleared. A locked answer cannot change.
- **Code.** Everything shown after the answer lives in one lazy chunk (`components/case/PostReveal.tsx`), which does not load before the lock.
- **Content.** `qc/briefLint.ts` fails the build if a brief contains the case's conclusion, any canonical label of any kind, a model score id or name, an outcome-giving model label, a claim about what the Explorer concludes, or a banned word. `manifest.ts` lints authored titles, questions and briefs, and rejects revealing ids in `context_fields`.
- **Tests.** `CaseReviewView.real.test.tsx` renders all 23 built cases. Before the answer there is no conclusion, reasoning, alternative, remainder, reveal fact, robustness or `why_selected` text; after it, all of them are present.
- **Method cards.** The brief links each assumption to its method card, and several card examples name the Explorer's conclusion for a review case (MC_015: polysilicon is Low intervention; MC_013: Japan's EV refining is Secure). The examples stay hidden while any assigned case is unsubmitted, optional outside and self-selected cases included (`MethodCardsView.tsx` exampleHoldReason).
- **Admin.** The prior (did the blind answer differ?) is computed, not asked, so the reviewer is never asked to restate it after seeing the answer.

## How Country / Sector / Partnership / Compact reviews differ

| | Country (`posture_cell`) | Sector (`aib_node`, `sector_chain`) | Partnership | Compact |
|---|---|---|---|---|
| Question | How should X manage step S of technology T through 2035? (Deployment: does X need a major push…by 2030-2035?) | If China stopped supplying / under a concentration cap, what response does a product need? Chains: at which stages, and which first? | What should lead the pair's cooperation over 12-36 months? | Does the chain need a standing multi-country arrangement, targeted action, or none? |
| Horizon and scenario | 2024-2035, IEA World Energy Outlook 2025 Stated Policies, today's network (Deployment: Current Policies, 2030 targets) | Immediate re-sourcing on 2025-26 trade; one-way cut or benchmark cap | 12-36 months; memo data date | As sector |
| Tailored brief | Evidence grouped: where supply comes from, domestic capability, alternative suppliers, market outlook | Chains: the chain stage by stage | "X brings / X depends on / Y brings / Y depends on / Shared strategic problem" | The first three causal-chain links, estimates labelled |
| Options | Six choices with actions; Watch as "Watch (keep open)" | Four response types; chains: six stages + "No stage needs allied intervention" | The memo's four boxes | Three compact decisions |
| Reveal remainder | Security test, commodity test, need, prize, position, credible base | Class bars and shipped variants; chains: stage strip, lead stage, chokepoint view | Four boxes, coordination changes, consistency check | Coordinated trade, new capacity, hosts, coalition, resilience after |
| Product step | None | "Has the Explorer located the actual strategic bottleneck…?" | Complementarity (required) + incentives, asymmetry, durability, substitution | "Does this compact materially ameliorate the stated threat?" |

## Remaining context gaps

- **Country cells.**
  - No import values in dollars.
  - Whether the world-supply share is production or exports is unstated for most steps.
  - Net imports, the basis of the vulnerability rank, are not shipped.
  - No lead times, backlog or project pipeline (named in CASE_008's own context).
  - The technology-readiness edition is not stated.
- **Deployment.** Grid queues, permitting and costs are absent, and the 2030-target family is missing for the United States.
- **AIB nodes and chains.**
  - The customs codes behind each product group are not listed.
  - Production capacity enters only as a divergence warning, never as a supply figure the reviewer can weigh against trade.
  - Build times are not modelled anywhere.
- **Partnership.**
  - The existing bilateral agreements are not in the brief, although one option is "upgrading the agreements they already have".
  - Firm interest and politics are out of scope by design.
- **Compact.** Concentration is not published for a whole chain, so concentration before and after, and the dependence outcome, are "not modelled".
- **Reveal (all kinds).** The posture word beside the choice ("Low fence · Deploy", "High fence · Secure") is unexplained. The Level 2 data-quality notes still include engineering lines ("the AIB bundle carries no git commit").

## UX choices that need NEIS judgment

1. **Rules before the answer.** The analytical assumptions (the security routes, the prize cap, the large-producer guard) are shown before the answer. They are definitions, not conclusions, but a careful reader can replay the rule. Should the brief keep them, fold them all, or move the rule-like ones to the reveal?
2. **Impossible options at Deployment.** Deployment cases offer all six choices, although the rule can only give Secure, Accelerate or Watch (keep open). Should the extra options be kept, as they are informative for "None of these", or narrowed?
3. **Time per case.** The rule is 6 minutes, +2 each for a partnership, decision use and threat. Briefs of 6,000-12,000 characters suggest 10-15 minutes for a first case.
4. **Repeated warnings.** Divergence warnings appear in the warnings, per stage and as a fact on chain cases. Keep all three or show them once?
5. **Order of the closing questions.** The compact question "Does it help?" comes before the threat table that informs it.
6. **The partnership headline.** The partnership case's Explorer conclusion is always "Build together" whenever the memo has a joint build (stated in the reveal). Keep the case, re-pose it on the memo's ranking within boxes, or score agreement on the box contents rather than the headline?
7. **Capped prize and need floor.** These states appear only after the reveal; brief-data treats them as model-derived on purpose.
8. **Thresholds and counting rules.** Several were fixed in code and are still to confirm:
   - insufficient evidence hides confidence and driver;
   - the first product question is required;
   - repeated inability is flagged at 2 reviewers;
   - "somewhat" persuaded is reported beside, not inside, "prior differed, evidence persuaded";
   - the calibration counts per campaign, with any campaign accepted by the case view.
9. **Converting a note.** A note converted to a data issue is recorded on the note; sending the full data-issue report (which now opens with the note's text pre-filled, `IssueReportDialog` `initialText`) is optional. Should a data-issue note require the full report?
