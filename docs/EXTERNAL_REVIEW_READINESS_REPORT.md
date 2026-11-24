# External review readiness: final report

Date: 2026-11-23. Branch `feat/external-review-readiness` (worktree `osi-worktrees/xreview`), based on
origin/main `db02539b` + `feat/qc-review`. Nothing pushed; the public Pages site is not yet republished.
Companion documents: [audit](EXTERNAL_REVIEW_READINESS_AUDIT.md) (written before implementation),
[case audit](EXTERNAL_REVIEW_CASE_AUDIT.md), [partner eligibility](PARTNER_ELIGIBILITY_AND_FRAMING.md).

The Explorer is still the same Explorer: same navigation, pages, charts and visual design. The changes are
reordering, collapsing, rewriting and linking, plus one credibility fix (partner naming).

## 1. Worktrees reviewed
`qcmain` (feat/qc-review), `stamps` (PR #370, on main), `redesign` (feat/aib-explorer-redesign),
`pagesbuild`, `thememo`, `stages` (PR #367, on main), the local Pages repo (`pages/aib-explorer`), and the
unrelated worktrees (cib, fantasy, monthly, nipo, nodata, regs, tri, tradegrowth, yards, partnerfix). Details
in the audit, section 3.

## 2. Redesign changes explicitly discarded
Four-way home and `/start`, the new nav, the `src/partnership`, `src/compact`, `src/sector/conclusion` and
`src/conclusion` route trees, separate conclusion/evidence/policy pages, the memo-data swap. The redesign
branch was not merged or cherry-picked; its "answer first" idea was reimplemented inside existing sections.

## 3. Worktree changes ported
- `feat/qc-review` merged whole (self-contained `/review` app, R QC, review content). It stays the structured
  external-review environment; the Explorer does not imitate it.
- Stamps work (PR #370) was already on main; its remaining gap (AIB commit dropped) is fixed here.
- Not ported from the redesign: golden snapshot test, share-label rule (optional, deferred).

## 4. User-facing changes
| Page | Change |
|---|---|
| Home | Intro cut to one sentence; quiet "Want to inspect the underlying data? Advanced explorer →"; the jump picker demoted below the strategic questions (kept: it is Home's only mineral jump). |
| Country | "What should [country] do?" at the top of the Strategy section: up to 3 steps per choice with a one-line reason, Keep open and Not assessed counts, a 1-2 sentence synthesis, collapsed "Why this result?" with "Why not [alternative]?" per step, one feasibility caveat, the trusted-network list. "Download decision brief" replaces the misleading "Generate memo"; stale-brief notice. Section nav reads "Secure (high fence)" etc. Default partner in the legacy panel is the first trusted-network member. |
| Sector (technology) | "Where are the real bottlenecks?" first (AIB stress-test steps with largest current supplier and a plain why), then the supplier-share chart, then the chain. Stages without comparable country evidence collapse into one line. One-line HHI significance. |
| AIB overview | "Where does coordination actually change the outcome?" first (acting together adds most / new trade links close the gap / new capacity required / Low intervention count), trusted-network list, "Compare two countries" control linking to the existing pair view; pair view gains a short "A brings / B brings / complement / what working together adds / action still needed" summary where a thesis exists. |
| Advanced explorer | Title and eyebrow now say "Advanced explorer" (matches the nav). No other change. |
| Footer / Methods / Data | "Data release YYYY-MM-DD" with method version, app commit and bundle commits; Methods page demotes the legacy index method into a collapsed "descriptive, not the decision rule" section and says where the canonical methodology lives. |

## 5. Terminology changes
Plain meaning first, technical term in brackets where review cases quote it: "Could add capacity (investment
host)", "Major buyer (demand anchor)", "Smallest group that meets the target (minimum coalition)", "Supply
exposed to the disruption", "Extra supply recovered by acting together (coordination dividend)", "Remaining
supply gap", fence titles "Secure what is critical (high fence)" / "Build, preserve, partner and deploy
(medium fence)" / "Keep open (low fence)", "First-ranked partner". New glossary entries: supply-chain step,
trusted network, supply exposed, data release; "why it matters" clauses for HHI, coordination dividend,
remaining supply gap, supply exposed. VOCABULARY.md rule 12 records the pattern.

## 6. Partner-logic changes
Client-side only (no R or data change): every partner list is split into **eligible trusted partners**
(trusted network = coalition core 31 + partners 28 + Taiwan and Switzerland per
`config/aib_strategic_layer.yml`, 61 economies) and **major suppliers outside the trusted network**, which are
still shown, never dropped. 643 steps in 72 economies had a non-member lead partner (China 468, Russia 100);
528 steps in 48 economies now say "No eligible trusted partner identified". Sri Lanka: lead partner China 10 /
Russia 4 -> Viet Nam 4 / Indonesia 1 / none 15. United States unchanged. CSV exports gain trusted-network
flags. Regression tests on the real bundle. Recommended R follow-up in the partner document.

## 7. Missing-data UX
Tech pages no longer show an empty card per no-data stage; one line names the stages and says the page
focuses on the rest. True zero prints 0; missing prints "–" with "No data" for screen readers on the risk card;
not-applicable chips say "not applicable". Not assessed is never merged into Watch on the Country block.

## 8. Memo / export
Decision briefs made prominent and honestly labelled: a notice when a brief predates the data release (8 of 9
briefs' counts differ from the live page). False "available for the 31 core members" line removed. Not done:
regenerating briefs or fixing Figure 2 (overlapping labels, legend mismatch, found on the USA brief), which
needs a `scripts/92` R/Edge rerun on the stages data. No PDF rendering test was added.

## 9. Bilateral feature
Implemented as a control plus summary, no new method: "Compare two countries" on /aib opens the existing pair
view; a summary card uses only the shipped theses (`strategic.theses` logic, compact roles, missions). Pairs
without a thesis (e.g. Argentina-Australia, U.S.-China) get no summary rather than invented content. The pair
view does not show current dependence on China (flag in the case audit).

## 10. README / docs
README rewritten (1,322 -> ~250 lines, current unit, decision chain, AIB logic, site, data, reproducibility;
old README archived verbatim in `docs/archive/`). `docs/README.md` answers "where is the current
methodology?" (strategic_position.md, aib_methodology.md, supply_chain_stages.md). Legacy banners on seven
docs; stale counts and status fixed; AIB method drift listed at the top of aib_methodology.md;
dashboard/README deploy (GitHub Pages) and budget sections corrected.

## 11. Provenance / version
Footer, Methods and Data show the data release, method version, app commit and posture / AIB / sector /
country stamps. AIB `git_commit` now kept by `buildInfo` and recorded in `/review` submissions. Review release
rebuilt: `rel-f07fe2fb9554` (bundle QC 32 pass / 7 warn / 0 fail; 14,124 of 14,124 choices reproduced; 36 of
36 golden cases pass). No bundle values changed.

## 12. Tests
- Typecheck: clean.
- Dashboard `npm test` (unit + ui + parity-synthetic, 2,271 tests): after integration fixes, all failures were
  either fixed or reproduced as load timeouts that pass when re-run alone, except one pre-existing failure:
  `src/review/exports.test.ts` fails to parse on this Windows checkout (CRLF after the shebang of
  `scripts/review/export-review.mjs`; also failed before any change; passes on LF checkouts).
- Real-data projects (country-real, sector-real, aib-real): 58 of 59 pass. The one failure is pre-existing
  (`aib-real` pins diversification counts 37/34; the shipped bundle has 36/35).
- `tsc -b` and `vite build`: pass. `npm run budget`: OK after lazy-loading the Country "What should ... do?"
  block as its own chunk (it renders only once posture data arrive). Country profile first view is now
  899.4 kB of 900 kB: almost no headroom left for further Country-page code.
- One correctness review pass (single reviewer) over the new logic: one verified defect (scatter tooltips
  claimed "no eligible trusted partner" from the first-ranked supplier alone), fixed. It also re-ran
  whyChoice over all 14,124 shipped cells (all match) and checked staleness dates, stage collapse, bottleneck
  shares, release stamp and hooks.
- New tests: trusted network (config mirror), partner eligibility (unit, view, real data), whyChoice (unit
  and all 14,124 real traces), bottom lines (country, sector, AIB), release stamp, review context AIB commit.
- R: no R code changed in this sprint; the R suite was not re-run here.

## 13. Representative cases audited
United States, Japan, Sri Lanka, New Zealand, Kenya; batteries, solar, electric grid, nuclear, green
hydrogen, geothermal; U.S.-Japan, U.S.-Korea, U.S.-Viet Nam (counterintuitive), U.S.-Indonesia (China
dominates current trade). See the case audit.

## 14. Unexpected analytical outputs (flagged, not fixed)
- Partner choices with no eligible ally (477 steps in 48 economies).
- Kenya competitive position 0.654 in Nuclear Assembly (likely re-export artefact).
- Single Secure step for the United States.
- Catch Up with no measured position (Batteries Recovery, U.S. and Japan).
- Nuclear has no step needing action in the stress test.
- Sector bottleneck lists are effectively rankings by China's share (coordination gain ~0).
- Overhead conductors: Low intervention yet top-3 for supply recovered by acting together.
- U.S. "55% of world exports or production" in Minerals & recycled feed (Battery Materials compact).
- Node stage vs cell stage mismatches (wafers, overhead conductors, Li-ion cells).
- Known QC items: 432 cells at prize 0.59, entrepot leaks (SGP, BEL), HS 281290 at Refining.

## 15. Remaining methodological limitations
No fiscal cost, price premium, policy durability or success-probability evidence (stated on the page as
"Feasibility evidence: Not assessed"). Four readings of "trusted" remain in the pipeline (coalition, AIB
strategic allied, AIB global candidates, engine per-reporter trust); the Explorer now names partners from one.
The disruption scenario is China-only. Partner identity does not enter the choice rule.

## 16. Remaining data gaps
13 of 66 technology-stage cells have no country data (most of green hydrogen and geothermal); 4,887 of 14,124
steps not assessed; bundle vintages mixed (posture/AIB 2026-11-21, country/sector 2026-10-31); briefs stale;
3 of 4 AIB files and the partnership memos carry no git commit.

## 17. Defer until after external review
R-side partner filter and rerun; brief regeneration and Figure 2 fix (do before if briefs are sent); golden
regression snapshot; degenerate-chart rule; dash titles across all tables; Pages assembly script tracked in
the repo; partnership memos beyond U.S.-Japan; any fiscal-cost or feasibility model; a geopolitically neutral
scenario toggle.

# Must resolve before sending to external reviewers
1. Merge this branch (and the QC branch inside it) to main and republish the public Pages site from it; the
   live site still lacks PRs #370, #367, #354 and this sprint.
2. Regenerate the nine decision briefs on the current posture release (`scripts/92`), fixing Figure 2 label
   overlap, or withhold the brief downloads; a reviewer will compare brief and page.
3. Check the U.S. "55% of world exports or production" Minerals & recycled feed figure and the Kenya Nuclear
   Assembly position before they are shown.
4. Decide how to present Partner choices that have no eligible trusted partner (keep with the new wording, or
   change the R rule) and say so in the reviewer guide.
5. Re-run the review data build on the final merged commit (`build-review-data.mjs`) and confirm the release
   ID reviewers will see.
6. Visual check of the Country, Sector and AIB tops on desktop and phone (not done in this session; the
   Browser pane was hidden).

# Questions we should deliberately leave for external reviewers to test
1. Is "Partner" the right call where no trusted partner has the capability, or should it become Catch Up /
   Secure / Keep open?
2. Is the trusted network (coalition + Taiwan + Switzerland) the right eligibility set, and should it be
   scenario-dependent?
3. Should Secure be broader for the U.S. (one step) and should fossil extraction for importers be Secure?
4. Is a China-only disruption the right stress test, and does it make bottleneck lists a China-share ranking?
5. Are the thresholds (need 0.67, prize 0.6, position top quarter, deployment 0.6) and the 0.59 prize cluster
   defensible?
6. Does "Catch Up" with an unmeasured position (pathway only) make sense?
7. Does nuclear really have no supply-chain step needing action?
8. Are trade-based competitive positions robust to re-export hubs (Kenya, Singapore, Belgium)?
9. Do the plain-English labels lose meaning relative to the technical terms?
10. Is the "strategic case, not an investment recommendation" framing sufficient without a cost model?
