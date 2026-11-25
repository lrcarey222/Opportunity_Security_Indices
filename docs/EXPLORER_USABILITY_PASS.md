# Explorer usability pass (2026-11-25)

Branch `feat/explorer-usability-pass` off main `854f4f79` (PR #371 merged, plus the partnership and compact memo
PRs #352 / #362). Low-risk presentation and copy changes only: no methodology, threshold, eligibility or data
change. Governing rule: do not make the Explorer bigger; make the conclusions already in it easier to find,
understand and audit.

## Already done in #371 (not repeated)
Country "What should [country] do?" block with Why this result / Why not; sector "Where are the real bottlenecks?";
AIB overview "Where does coordination change the outcome?"; trusted-network list on AIB overview and the Country
block; Compare two countries + pair summary; data-release stamp; Methods legacy demoted; plain labels for the main
AIB measures; no-evidence stages collapsed on technology pages; decision-brief CTA and staleness notice.

## Checklist (before coding)
| # | Item | Where | Approach |
|---|---|---|---|
| A1 | Sector: "What matters in this supply chain?" first screen with strongest holders, concentration, implication, evidence limit | `sector/views/TechChainView.tsx`, `Bottlenecks.tsx` | Lead with observed concentration and the strongest holders from the chart rows; AIB stress-test result demoted to one linked line (it is modelled, AIB analysis) |
| A2 | Stage page: "Who is strongest today?" in text before the scatter | `sector/views/profile/*` | Generated from the stage's existing ranking (largest holders); no new score |
| A3 | Country: recommendations above index charts | `country/views/profile/BottomLine.tsx` | Add confidence + evidence summary (A6) to the existing block; nothing new above it |
| A4 | Partnership: "What is the strategic case?" | `partnerships/RelationshipThesis.tsx`, `aib/views/AibPartnershipsView.tsx` | Map thesis logic to supply / complementary capabilities / market / new capacity / concentration; never alignment alone; link the pair memo when one is shipped |
| A5 | Compact: "What bottleneck, who contributes, what gap remains?" | `aib/views/StrategicCompactView.tsx` + `strategic/*` | Three-line answer from the compact's own spine, core roles and remaining gap |
| A6 | Confidence / provenance: "Why this conclusion? / Evidence quality / Data / Explore the evidence" | new `posture/confidence.ts` (done) + one small summary component | Only shipped fields: posture confidence, capability evidence, borderline inputs, policy-alignment confidence, data periods from meta |
| B | Copy pass: node -> supply-chain step, residual -> remaining gap, coalition -> trusted, no "the model says", Yards & Fences explained | vocabulary, visible labels | Display text only; definitions unchanged |
| C | Coalition / trust definitions findable | tooltips where "core coalition", "allied", "trusted" appear; glossary entry lists the countries | Reuse `trustedNetwork.ts`, `TrustedNetworkControl`, `<Term>` |
| D | Missing data never reads as low priority / zero / Watch | sector and country views, AIB tables | "Insufficient evidence" / "No data" in the neutral style |
| E | Affordability & deployment | sector tech page, country block | Surface existing cost / LCOE / deployment measures with links; no new index |
| F | Duplication on the sector first screen | `TechChainView.tsx` | Demote repeated metrics and AIB-modelled facts behind disclosures |
| G | Memo CTA | Country head, memo page, pair view | "Download decision brief" + one line on what it contains; pair memos linked |
| H | How to use the Explorer | new short route `/how-to-use`, linked from Home and About | Five workflows, a few lines each |
| I | Homepage polish | `shell/Home.tsx` | Four entry points (sector, country, partnership, compact) with short copy; AIB term not required to enter |

---

# Implementation note

## Changes made

### Homepage / navigation
- Four entry cards, each one question and one action: Explore a Sector ("Where are the real bottlenecks and
  opportunities?"), Explore a Country ("What should this country secure, build, partner for or deploy?"), Explore
  Partnership Opportunities ("Who should work with whom, and for what?", to `/aib/partnerships`), Explore a Compact
  ("Which bottlenecks need a few countries to act together?", to `/compacts`). No card requires knowing the term
  "Allied Industrial Base"; the AIB overview is one link under the cards. Long card descriptions removed.
- New short page **How to use the Explorer** (`/how-to-use`, `src/shell/HowToUseView.tsx`): five workflows (sector,
  country strategy, partnership, compact, research question) with links to the real entry routes. Linked from Home
  ("First time here?"), the About tab, the footer and the About page.

### Conclusion-first presentation
- **Sector technology page** opens with "What matters in this supply chain?" (`ChainAnswer.tsx`,
  `sector/derive/answer.ts`): where supply is concentrated (each stage's most concentrated observed row), the
  strongest positions, what stands out, the evidence limits, and affordability & deployment, all generated from the
  observed supplier-share rows already on the page. The AIB stress-test list (a modelled result) moved below as
  "What the stress test adds", marked as modelled, with its step list in a disclosure and a link to the AIB analysis.
- **Stage page**: "Who is strongest today?", one sentence naming the three largest holders of the stage's primary
  measure, with measure and year; with fewer than three holders it says the figure is not a comparison.
- **Country page**: the "What should [country] do?" block (from #371) gains a confidence sentence in each step's
  "Why this result?", an "Evidence quality" line, a collapsed "Why this conclusion?", a "Data" line and links to the
  evidence (`posture/EvidenceSummary.tsx`, `posture/evidenceSummary.ts`). Nothing new sits above it.
- **Partnership pair view**: a strategic-case block leads the thesis card (`partnerships/strategicCase.ts`): the
  shipped thesis logics mapped to securing supply, combining complementary capabilities, expanding a market,
  building new capacity and reducing concentration, ranked by the number of priority compacts behind each, with
  compact links. Pairs without a thesis say no strategic case is identified, and that alignment is not a case.
- **Strategic compact page**: Bottleneck / Who contributes / Remaining gap, three generated lines from the
  compact's own spine, core roles and coverage under the selected scenario, with an evidence-limit clause
  (`aib/derive/compactAnswer.ts`).

### Copy and definitions
- Glossary entries lead with why the term matters (coalition, trusted network, competitive position, deploy mode,
  yards and fences, sectoral compact, allied coverage); coalition, AIB, gap and tier entries link to the trusted
  network. Definitions unchanged.
- The trusted network (64 economies) is listed on `/glossary` next to its entry, on `/aib/roles`, on compact pages,
  on the AIB overview and in the Country block; "core coalition" on compact pages reads "the compact's core group".
  The list's note now says the stress tests let any economy redirect its exports.
- Visible "node" -> "supply-chain step", "the model simulates" -> "the analysis simulates", "optimized trade" ->
  "coordinated trade"; the partnership prompt adds "reducing concentration". VOCABULARY.md rule 13 (why it matters
  first) and new term rows.

### Missing data / confidence
- `posture/confidence.ts`: one reading of the shipped posture fields (posture confidence, capability evidence,
  borderline inputs, unmeasured position, unknown pathway, consistency flag, policy-alignment confidence). At
  Deployment & Use only deployment inputs are named. A not-assessed step reads "Insufficient evidence: no conclusion
  is drawn for this step". No new score.
- Sector pages: green hydrogen reads "Insufficient evidence" for every stage; geothermal states its one measure and
  the stages without evidence. The plain HHI line ("Not concentrated ...") shows only for supply measures with at
  least three holders, so a resource-potential measure no longer reads as low supply risk. A source-tagged holder
  ("China (IEA)") counts as the economy when counting who leads.
- Country map intro: "Hatched cells are not assessed (not Watch)."

### Affordability & deployment
- Sector technology page: one "Affordability & deployment" row naming the existing cost and deployment evidence with
  Advanced-explorer links. Public technology cost exists only for geothermal (enhanced geothermal levelised cost,
  Franzmann 2023); BNEF LCOE is licensed and named, not shown, for solar, wind, batteries, coal and gas; other
  technologies say there is no technology cost evidence in the Explorer. Input prices (IMF 2025) and deployment
  measures (capacity additions, consumption, EV sales, IEA outlook supply) are linked where they exist.
- Country block: an "Affordability & deployment" row (Accelerate technologies, why, where deployment policy lags),
  stating that costs and prices do not enter the choices and linking world commodity prices.

### Memo / use guide
- Country head: "Download decision brief" with one line on what it contains (recommendation, evidence, partner
  implications, deployment priorities, policy direction, next steps; PDF or Word) and its written date; same line on
  the memo page. The staleness notice stays.
- Pair view and compact pages link the shipped partnership and compact memos ("Download partnership memo"), with
  contents, date, "not independently verified" and a notice when the memo predates the data release. `/compacts`
  lists the compact memos (SC-CRITMIN has no compact page, so this is its only link).

## What I deliberately did not change
- No methodology, threshold, eligibility, coalition, posture or AIB rule; no data bundle or review release rebuilt.
- No new score: confidence and evidence quality are the engine's own fields in words.
- "Partner despite capability" cases, Catch Up without a measured position, and the AIB's global candidate supply
  were not reworked (see issues).
- No confidence sentence in the step drawer (it sits in the Country first-view chunk); no strategic case on the
  Country profile; no Quad group memo link; IEA Net Zero pathways not added to the technology page (lazy climate
  chunk); methods.md, the AIB About methods text and review-quoted terms left as they are.
- Budget: the Country profile first-view limit is raised from 900 to 910 kB (measured 902.5 kB), for the
  plain-language glossary text that the shared InfoTip chunk carries on every page.

## Issues discovered
1. **Trusted network vs stress-test supply.** The AIB runs with a global candidate universe, so supply "recovered
   through trade" can come from outside the 64-economy trusted network: 404 of 1,732 trade links in the current
   bundle (Jordan 31, Russia 29, Zimbabwe 21 ...; lithium minerals are all Zimbabwe to Malaysia, Viet Nam, Turkey,
   Thailand; Russia to Japan nickel intermediates and Russia to France uranium conversion). Roles and partners are
   trusted-only; recovered supply is not.
2. **Two meanings of "allied".** Allied demand is the 59-economy coalition (`nDemandScope`); roles, theses and
   partner naming use the 64-economy trusted network.
3. **Missing evidence read as evidence.** 20 Batteries Recovery cells are Catch Up with no competitive position or
   evidence grade (pathway "yes" alone); 78 Partner cells have no competitive position; 385 supply-stage cells are
   high confidence with weak or no capability evidence, mostly Watch with position 0 and share 0. Of 4,900 cells at
   position exactly 0, 603 have weak or no evidence: check they are real zeros.
4. **Stage mapping.** Solar Refining has no polysilicon production measure (export value makes Germany the leader,
   China 8.7%); the IEA capacity share is the same figure at Components and Assembly; iron ore is linked at solar and
   batteries Extraction and silicon at batteries Refining; the manifest says green-hydrogen Extraction has country
   data, but no measure has a world total.
5. **Doubtful roles and coverage.** Viet Nam as demand anchor in Semiconductors, the United States as resource
   supplier in Batteries, Indonesia as specialized supplier of solar cells, Japan as demand anchor in Nuclear;
   SC-SOLAR names a host for a Low-intervention step; SC-BATTERIES under Managed Interdependence has 7 of 14 steps
   untested, yet its headline coverage reads compact-wide.
6. **Theses missing for close pairs.** United States-Taiwan (5 shared compacts, including Semiconductors),
   Australia-Indonesia and others have no thesis; the big pairs hold all six logics, so logic alone cannot tell
   them apart.
7. **Provenance.** Partnership and compact memos (17 Nov) predate the data release; country briefs predate the
   posture data; running text uses World Bank names ("Korea, Rep.").
8. **Definitions.** The coalition glossary entry gives partners a $100 bn GDP floor that the trusted-network method
   text does not mention (the agreement route has the floor; the supplier route does not).

## Recommended next sprint
1. Decide whether stress-test recovered supply should be limited to the trusted network (a `config/aib.yml`
   candidate-universe change and a full AIB rerun), or state the distinction wherever supply is "recovered".
2. Regenerate country briefs and partnership and compact memos on the current data release.
3. Treat an unmeasured competitive position as missing evidence in the decision rule (Catch Up and Partner without
   a position), and audit the 603 zero-position cells with weak evidence.
4. Fix the solar refining measure (polysilicon production) and the duplicated IEA capacity rows.
5. Add theses for close pairs without one (United States-Taiwan) and review the doubtful compact roles.
