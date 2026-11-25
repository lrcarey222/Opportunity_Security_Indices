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

The implementation note follows below once the work is done.
