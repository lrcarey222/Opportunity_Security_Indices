# External review: representative case audit

Date: 2026-11-23. Branch `feat/external-review-readiness`. Bundles: posture 2026-11-21 (`17f0737e`),
AIB 2026-11-24 (`acbc3805`, roles trusted-only, 64-economy trusted network), country and sector 2026-10-31 (`4bb2adc`). Review release `rel-4a271ac2b550`.

Method: outputs read from the shipped bundles with small read-only scripts (posture core and detail
documents, AIB compacts document, sector manifest), plus the components' unit and UI tests on the
synthetic and real bundles. The Browser pane was hidden during this session, so pages were **not**
inspected visually in a browser; "visible without scrolling" is judged from component order, not a
screenshot. Unexpected results are **flagged, not fixed**.

Legend: OK = meets the check; FLAG = semantic or model question for reviewers / NEIS; FIXED = changed in
this sprint (presentation only); GAP = known limitation left open.

## Countries

Counts are steps (technology x stage) by choice, from the posture core.

| Country | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | Not assessed |
|---|---|---|---|---|---|---|---|
| United States | 1 | 13 | 3 | 4 | 2 | 25 | 18 |
| Japan | 4 | 5 | 8 | 4 | 5 | 22 | 18 |
| Sri Lanka | 4 | 0 | 0 | 16 | 6 | 21 | 19 |
| New Zealand (small allied) | 1 | 0 | 0 | 8 | 0 | 39 | 18 |
| Kenya (developing) | 3 | 0 | 0 | 17 | 8 | 17 | 21 |

### United States
- Conclusion: Preserve-heavy (EV components and assembly, battery assembly, nuclear assembly, wind
  components); Secure only EV refining (rare-earth/battery-materials refining). OK as a reading of the rule.
- FLAG: a single Secure step for the U.S. will surprise reviewers used to "secure critical minerals"
  framing. Secure is an override with three rationales; most mineral steps land in Partner (Batteries and EV
  Extraction, lead partner Australia). Leave for reviewers to test.
- FLAG: Batteries Recovery is Catch Up with no measured position (`cp` null): the rule treats an unknown
  position as "not top quarter", so a credible pathway alone gives Catch Up (same for Japan). The QC case
  audit already noted this is anchored on scrap exports.
- FLAG: Oil Recovery is Partner on high need with prize 0.18 and low confidence; it is held back from the new
  bottom-line list (low confidence) and counted instead.
- Partners: Australia, Japan, Germany, all in the trusted network. Unchanged by the partner fix. OK.
- Brief: STALE. The brief (2026-11-04) says 8 Preserve / 8 Catch Up; the page says 13 / 3. Now labelled
  with a notice on the head and the brief page (FIXED: notice). Brief Figure 2 has overlapping labels in the
  top-right Preserve cluster, a label on the top-quarter line, a label over markers, and a Secure ring that
  contradicts the legend's "review cases are hollow" (GAP: needs `scripts/92` rerun with label repel).
- 19 of 66 steps are low confidence; the bottom line lists none of them as answers (counted).

### Japan
- Conclusion: Secure EV refining and the three fossil extraction steps; Preserve battery and nuclear assembly;
  Catch Up wind components/assembly, solar assembly, nuclear refining, battery recovery; Partner on battery
  and EV extraction (Australia), wind refining (Germany). Semantically coherent.
- FLAG: Secure Gas/Oil/Coal Extraction with prize near 0 and position near 0: Secure here is the
  operational-control override for an import-dependent economy, not a call to build domestic extraction.
  The "Why this result?" text says it is the override; reviewers should test whether the label reads right.
- FLAG: three of five Preserve steps are low confidence (EV components, EV assembly, nuclear components).
- Brief: STALE (brief 2 Preserve / 12 Catch Up / 3 Partner vs page 5 / 8 / 4; the brief says "No Preserve
  call clears the evidence bar"). Notice added.

### Sri Lanka (known problematic case)
- Before: lead partner China 10, Russia 4, Viet Nam 4, Indonesia 1, UAE 1; pages read "Best-placed partner:
  China" and the engine called China a "credible trusted supplier".
- After (FIXED, presentation; UAE trusted since 2026-11-23): eligible lead partner UAE 2, Viet Nam 4, Indonesia 1; 13 steps say "No eligible
  trusted partner identified; the leading suppliers ... are outside the trusted network". China and Russia
  still appear, labelled as suppliers outside the trusted network. Regression test:
  `dashboard/src/posture/__tests__/real.partnerEligibility.test.ts`.
- FLAG (method): those 13 steps remain **Partner** choices ("better solved through allies") with no eligible
  ally. The choice rule does not depend on partner identity, so this is a real tension between the rule and
  the framing. Left for reviewers.
- FLAG: Nuclear Assembly is a Partner step with need 0.95 for an economy with no nuclear programme; need is
  computed from trade exposure, not from deployment plans.
- FLAG: Secure Oil Refining / Oil Extraction name China and Russia as the leading suppliers; the Secure
  rationale is about exposure to them, which is now worded as such.
- No decision brief (not a coalition member). The page now says "No decision brief yet" instead of the
  false "31 core members" line. FIXED.

### New Zealand (smaller allied economy)
- Conclusion: one Secure (oil refining), eight Partner (battery chain with Korea and Poland, EV extraction
  with Canada), 39 Watch. "The high-security fence is narrow" reads correctly here.
- FLAG: Partner Batteries Assembly with lead partner Poland: plausible (cell plants in Poland), but reviewers
  may expect Korea or Japan; the ranking is capability x relationship x diversification.

### Kenya (developing economy)
- Same partner pattern as Sri Lanka before the fix (China 9, Russia 3); fixed the same way.
- FLAG (data): Kenya's competitive-position percentile in Nuclear Assembly is 0.654. Kenya has no nuclear
  industry; this looks like a re-export / entrepot artefact of the kind QC already logged for Singapore and
  Belgium. Do not fix silently; show reviewers.
- Accelerate on Geothermal Deployment (need 0.92, position 0.82) is the most intuitive result in the set. OK.

## Sectors

"Where are the real bottlenecks?" now leads each technology page, built from the AIB stress-test nodes
(major-supplier disruption); the supplier-share chart follows it.

| Sector | Top steps (remaining gap after coordinated trade) | Checks |
|---|---|---|
| Batteries | Li-ion cells & packs (Assembly) 69%; cathode materials (Refining) 57%; synthetic graphite (Refining) 51%; separators/accumulator parts (Components) 37%; manganese dioxide (Refining) 35% | OK; China is the largest current supplier in each |
| Solar | PV modules 67%; PV cover glass 58%; wafers 35%; inverters 33%; power electronics 30% | FLAG: wafers is a Refining node filed under the Components cell |
| Electric grid | inverters, grain-oriented electrical steel, power electronics, insulators & bushings, power semiconductors (33% to 23%) | FLAG: the same dictionary steps (inverters, power electronics, insulators) top the solar, grid and wind lists |
| Nuclear | none of 6 steps needs action (5 Low intervention, 1 Not enough evidence) | FLAG: shown as "no step needs action" + concentration-only rows; reviewers may find "nuclear has no bottleneck" counterintuitive (uranium enrichment is not separately modelled) |
| Green hydrogen | no AIB steps; 5 of 6 stages lack comparable country evidence (only Extraction kept) | FIXED: the five empty cards collapse into one "Comparable country-level evidence is not currently available for ..." line |
| Geothermal | no AIB steps; 5 of 6 stages lack evidence (only Deployment kept) | FIXED: same collapse |

Cross-cutting sector flags:
- FLAG: in every top-5 list the gain from acting together is about 0, so remaining gap ~= supply exposed ~=
  China's current share; the list is in practice a ranking by China's share. That is a property of the
  disruption scenario (China supply removed) and should be stated to reviewers, not hidden.
- FLAG: a node's own stage and the cell stage it is filed under differ in places (wafers, overhead conductors,
  Li-ion cells under EV components). The list uses the cell's stage.
- GAP: the bottleneck list cites no data year itself; the supplier-share chart beneath it does.
- Zero vs missing: a true zero prints 0 / 0.0%; missing prints "–" with "No data" for screen readers on the
  risk card; not-applicable coverage chips say "not applicable". Other tables still use a bare dash (GAP).

## Partnership / AIB

AIB overview now opens with "Where does coordination actually change the outcome?" (scenario-dependent).

| Case | What the Explorer says | Checks |
|---|---|---|
| Overview, Managed Interdependence | Acting together recovers most in nuclear special materials (+23%), reactor parts (+14%), overhead conductors (+8%); only 2 steps are Coordinate; new capacity needed most for Li-ion cells (46%, $61bn), PV cover glass, silver powder | FLAG: overhead conductors is a Low intervention step yet in the top 3 for supply recovered |
| Overview, Decoupling | Recovers most: electric rail vehicles, hydro turbines, wind towers; new capacity: Li-ion cells 69% ($91bn), PV modules 67%, rare-earth magnets 61%; 25 of 77 steps Low intervention | OK |
| Trade that closes the gap | Uses any economy, not only trusted partners (e.g. wind towers Viet Nam -> Philippines); the card says so | FLAG: by design (global candidate universe); reviewers should know |
| US-Japan | All six partnership logics; complement in grid (U.S. buyer, Japan technology), nuclear, magnets; action still needed on GOES steel, reactors and enrichment machinery, magnets | OK |
| US-Korea | All six logics; grid and wind (U.S. buyer, Korea scale manufacturer), semiconductors | OK |
| Counterintuitive: US-Viet Nam | Viet Nam the "major buyer (demand anchor)" for semiconductors; U.S. "second-sources for Viet Nam" | FLAG |
| China dominates current trade: US-Indonesia | China is Indonesia's top supplier in 24 of 66 cells (mean share 45%); the thesis gives five logics and never mentions China (theses are structural) | FLAG: the pair view does not show current dependence on China |
| Implausible number | U.S. "largest position: Minerals & recycled feed, 55% of world exports or production" (Battery Materials compact) | FLAG: check the source before review |
| No thesis (ARG-AUS, USA-CHN) | Headline only, no summary | OK (no invented content) |

## Checklist by criterion

| Criterion | Status |
|---|---|
| Conclusion makes semantic sense | Mostly; flags above (single U.S. Secure, Kenya nuclear position, Partner with no eligible ally, nuclear without bottleneck) |
| Explanation matches model logic | "Why this result?" is generated from the shipped decision trace and matches the shipped choice on 14,124 of 14,124 traced cells (`real.whyChoice.test.ts`) |
| Partner recommendations match framing | FIXED for named partners; Partner choice itself unchanged (flag) |
| Missing data not misleading | Improved on tech pages; not assessed never shown as Watch; other tables still use a bare dash |
| Source years appear | Country freshness line, AIB run line, sector chart columns; new bottleneck list has none of its own (GAP) |
| Empty / degenerate charts suppressed | Empty stage cards and an empty tech chart suppressed; no general degenerate-chart rule |
| Conclusion visible without excessive scrolling | Country: top of the first section, under the head and action row. Sector: first block. AIB: first card. Not verified on screen (pane hidden) |
| Deeper evidence accessible | Drawer, fence sections, Advanced explorer, Methods, `/review` all unchanged and linked |
