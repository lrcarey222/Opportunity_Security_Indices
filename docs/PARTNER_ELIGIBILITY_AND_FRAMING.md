# Partner eligibility and framing

External review readiness, 2026-11-23. Brief section 10. Client-side change only: no R rerun and no data change.

## 1. The problem

The Explorer is about an allied, trusted industrial base. But the posture engine's "best-placed partner" for
many non-aligned economies is China or Russia. On the bundle shipped 2026-11-21:

- Sri Lanka's lead partner by step is China 10 times, Russia 4, Viet Nam 4, Indonesia 1 and the United Arab
  Emirates 1.
- Kenya and Bangladesh look the same.
- 47 economies have China and 40 have Russia somewhere in a partner list.

Pages said "Best-placed partner: China", and the engine's reason line called China a "credible trusted
supplier".

## 2. Current logic (R)

| Step | Where | What it does |
|---|---|---|
| Coercion | `R/strategic_position/supply_structure.R:20` `SP_COERCIVE_POWERS` (CHN, HKG, MAC, RUS); `sp_coercive_pairs()` at :35 | Coercion is judged from the reporter's side. China is not coercive to itself or to Russia. |
| Trust for non-aligned reporters | `supply_structure.R:519` `sp_nonaligned_trust()` (rule `non_coercive`) | A reporter with no same-bloc partner trusts every partner that is not coercive to it. It writes `trust_basis = "nonaligned_non_coercive"`, which is not shipped. |
| Candidates | `R/strategic_position/partner_recs.R` `sp_partner_recs()` ~L134-148 | Candidates are `pairs$trusted & !coercive`. They are ranked by capability x relationship (Friendshore) x diversification, top 3. |
| Allied set | `scripts/26_build_strategic_position.R:525-531` (`allied` = coalition members + `treat_as_allied` + AIB allied), passed as `members` at :603-606 | Used only for `recommended_partner_1_member` (partner_recs.R ~L222). It does not filter candidates, and the flag is not exported. |
| Export | `R/dashboard/posture/posture_export.R:405-416` | Ships `partners: [{iso3, capability, cp, relationship, diversification}]` and core `cells.p1`. Also ships `partner_reason` (partner_recs.R ~L207-219). |

The Explorer used three different "trusted / allied" sets (until 2026-11-25; section 9 makes sets 1 and 2 one
allied set):

1. **Coalition: 59 economies.** 31 core and 28 partner, from `config/coalition.yml` via scripts/13. This is
   `countries.tier` in the posture core and `tier` in the Country fact sheets. The two agree on every row
   checked.
2. **AIB strategic "allied": 64 economies** (61 until 2026-11-23, when Slovenia, Iceland and the UAE were added by
   user decision). The coalition plus `treat_as_allied: [TWN, CHE, SVN, ISL, ARE]`
   (`config/aib_strategic_layer.yml:102`). This is the set `src/posture/trustedNetwork.ts` uses (core 31,
   partner 28, allied 2) and that the "Trusted network" control lists.
3. **AIB candidate suppliers: global.** Any exporter may add or redirect supply in coordinated trade
   (`dashboard/src/aib/derive/settings.ts:68`, "any economy may supply").

On top of these, the posture engine's per-reporter trust (above) is a fourth reading for non-aligned
reporters.

## 3. The change (client side)

`dashboard/src/posture/partnerEligibility.ts` splits every engine partner list by the trusted network (set 2)
and keeps the engine's order:

- **Eligible trusted partners.** List members that are in the trusted network. Only these are named as
  partners.
- **Major suppliers outside the trusted network.** The other list members. They are still shown, as
  suppliers, and never as recommended partners. Nothing is dropped and nobody is relabelled as trusted.

Helpers:

- `splitPartners`, `leadEligible`, `partnerLead`, `partnerLeadWords` and `noEligibleWords`.
- `networkOf`: a cached `trustedNetwork()` per country table.

Where no member is listed, the page says: "No eligible trusted partner identified; the leading suppliers,
China and Russia, are outside the trusted network."

The task wording "current supply comes mainly from..." was not used. The engine ranks partners by their share
of world (or modelled AIB) supply, not by the reporter's own imports, so "current supply" would claim
evidence the list does not carry.

| Surface | Before | After |
|---|---|---|
| Step drawer, Partner choice (`CellDrawer.tsx` -> `PartnerEligibilityList.tsx`) | "Best-placed partners" listed every engine partner with "relationship 0.94" | "Eligible trusted partners" (share of supply, position, "strong relationship" where the score is 0.6 or more), then "Major suppliers outside the trusted network (not recommended as partners)". The engine reason line shows only when every listed partner is a member. While the detail loads: "First-ranked supplier: China, outside the trusted network". |
| Fence one-liners and Country "Partner steps" lead (`fences.ts fenceReason`, `FenceSections.tsx PartnerLead`) | "Best-placed partner: China" | First member, or the no-eligible sentence |
| Strategy map tooltip (`PostureMatrix.tsx`) | core `p1` only | Same lead logic, using the detail when it is loaded |
| Allied Strategy (`AlliedStrategy.tsx`, `sections.ts alliedRows`) | Every engine partner recommended | Members only. Outside suppliers are listed apart. The opening sentence adds "N have no eligible trusted partner identified". There is a new table column and CSV fields `suppliers_outside_trusted_network(_iso3)`. |
| Strategic Choices (`StrategicChoices.tsx`) | "Recommended partners: China" | Members, or the no-eligible sentence. The diversification value uses members only. |
| Posture tables (`tables.tsx`) | "First trusted partner" | "First-ranked partner", with "(outside the trusted network)" after a non-member when the caller passes the network |
| Country Partnerships, legacy scores (`PartnersSection.tsx`) | Friendshore / Partnership Opportunity / Development Potential top lists and the partner table were unmarked | "· outside the trusted network" tag on non-members. Lists are not filtered and scores are unchanged. There is a `trusted_network` CSV field. |

No choice, threshold, count, gap flag or AIB status changed.

## 4. Cases that change (bundle of 2026-11-21, 214 economies)

Counted by a node script over `public/data/posture/posture-detail-*.json` and `posture-core`.

**Overall**

- Steps with an engine partner list: 1,656 in 195 economies. 1,444 are Partner choices; the rest are Secure
  steps delivered with allies.
> **Update 2026-11-23 (trusted network of 64, with Slovenia, Iceland and the UAE):** the named lead partner is
> outside the network in 601 steps (544 Partner choices) across 67 economies (China 468, Russia 100, Myanmar 14,
> Kazakhstan 14, Nigeria 3, Iraq 1, Algeria 1); 451 steps (438 Partner choices) in 47 economies have no eligible
> partner. Sri Lanka after: UAE 2, Viet Nam 4, Indonesia 1, none 13; Kenya: UAE 3, Viet Nam 4, Indonesia 1,
> none 12; Bangladesh: UAE 2, Viet Nam 5, Indonesia 1, none 12; United States unchanged. The figures below are
> for the first version (61 economies) and are kept as the record of that step.

- The named lead partner is outside the network in 643 steps (568 Partner choices) across 72 economies.
- Steps with no eligible partner at all: 528 (477 Partner choices) in 48 economies.

**Who the lead was** (in those 643 steps)

| Economy | Steps |
|---|---|
| China | 468 |
| Russia | 100 |
| United Arab Emirates | 42 |
| Kazakhstan | 14 |
| Myanmar | 14 |
| Nigeria | 3 |
| Iraq | 1 |
| Algeria | 1 |

**Which reporters are affected**

- 61 of the 72 economies are outside the network themselves.
- 11 are coalition partners: Oman 12, Indonesia 9, Viet Nam 8, Brazil 2, Colombia 2, South Africa 2, Costa Rica
  1, India 1, Malaysia 1, Peru 1 and Türkiye 1. Their engine trust set includes non-members because of their
  UN-vote bloc, either non-aligned or same-bloc. This was not traced per economy.

**Sri Lanka, Kenya, Bangladesh and the United States**

| Economy | Steps with a list | Lead before | Lead after | No eligible partner |
|---|---|---|---|---|
| Sri Lanka | 20 (16 Partner) | China 10, Russia 4, Viet Nam 4, Indonesia 1, United Arab Emirates 1 | Viet Nam 4, Indonesia 1, none 15 | 15 (12 Partner) |
| Kenya | 20 (17 Partner) | China 9, Russia 3, Viet Nam 4, United Arab Emirates 2, Indonesia 1, Kazakhstan 1 | Viet Nam 4, Indonesia 1, none 15 | 15 (13 Partner) |
| Bangladesh | 20 (17 Partner) | China 15, Russia 3, Indonesia 1, United Arab Emirates 1 | Viet Nam 5, Indonesia 1, none 14 | 14 (12 Partner) |
| United States | 4 (3 Partner) | Australia 2, Japan 1, Germany 1 | unchanged | 0 |

A real-data regression test pins this behaviour:
`dashboard/src/posture/__tests__/real.partnerEligibility.test.ts`. It checks that:

- Sri Lanka, Kenya and Bangladesh never have China or Russia in an eligible list.
- China and Russia still appear among Sri Lanka's outside suppliers.
- The United States' lists are unchanged.

## 5. Implications

- For about a quarter of non-aligned economies, many Partner steps now read "no eligible trusted partner".
  That is honest, but it sits next to a Partner choice ("better solved through allies") that the engine made
  with China or Russia counted as trusted. The choice itself is unchanged here. This is a methodological
  tension for the R side, not a display issue.
- AIB status ("covered" or "gap") for those steps is still the engine's. It was computed on the same
  non-aligned trust, so a step can show "Covered" with no eligible partner.

## 6. Limitations

- **The R output is unchanged.** `partners`, `p1` and `partner_reason` still carry China and Russia, and any
  CSV of the raw bundle or other consumer will show them.
- **The `partner_reason` text** ("CHN holds 37% of allied supply (AIB) ... credible trusted supplier") is
  hidden whenever a listed partner is outside the network. It is not rewritten. "Allied supply (AIB)" there is
  the global AIB candidate universe, China included.
- **AIB coordinated trade** still includes Russian, Chinese and Iranian exporters by design: 29 relationships,
  e.g. AIB-C09 RUS->SAU grain-oriented electrical steel. The candidate universe is global and the page says
  "any economy may supply".
- **The legacy Friendshore, Partnership Opportunity and Development Potential scores** are tagged, not
  filtered. China still tops Sri Lanka's and Kenya's Friendshore list. The Country profile's default selected
  partner is still `psi_order[0]`, which is China for Sri Lanka (`CountryProfileView.tsx:162`, not changed
  here).
- **Some surfaces are outside this change and still show the raw first partner:** the Sector strategy
  scatter tooltip, the posture scatter tooltip ("First partner"), and the `first_partner` field of the posture
  CSV (`derive.ts`). The posture tables need their callers to pass the network.

## 7. Recommended R follow-up

1. In `R/strategic_position/partner_recs.R` ~L140-148, filter candidates to `partner %in% members` (the
   allied set already passed in from scripts/26). Keep the non-member suppliers as a separate output (e.g.
   `outside_suppliers`) so pages can still show concentration.
2. Decide whether `sp_nonaligned_trust` should still drive the Partner *choice* for non-aligned reporters. If
   it should not, a reporter with no eligible partner would fall to a partner gap ("no credible allied
   solution yet") instead.
3. Export `trust_basis` and the member flag in the posture detail.
4. Rerun scripts/26 -> scripts/91, re-run the real-data tests, re-approve the golden QC cases (choice counts
   and partner fields will move), then simplify the client split to a check.

## 8. AIB roles: allied only (2026-11-23)

**Decision (user, 2026-11-23):** China, and every other economy outside the allied set, holds no role in the
Allied Industrial Base. The allied set is the model's demand scope (the coalition tiers) plus
`allied.treat_as_allied` (TWN, CHE; SVN, ISL and ARE added 2026-11-23).

**Rule change (R, not client side).** `config/aib_strategic_layer.yml` `roles: non_allied_roles: false`
(default `FALSE` in `aib_strategic_default_params()`; `true` restores the earlier rule):

- Strategic AIB Roles: every role, the three supply roles (RPA, STM, SMI) included, is assessed among
  allied economies only. Non-allied economies are no longer in the percentile pool that allied economies are
  ranked against.
- Compact-specific roles: none for a non-allied economy (no more "... (non-allied supplier)" roles), so
  the compact cores list no non-allied lead suppliers (`nonAllied` is empty in every compact).
- Kept as supplier and exposure information: the non-allied major suppliers' metrics and `major_supplier`
  flag in `aib_country_strategic_role_metrics.csv`; the division of labour; the workstreams' critical external
  dependencies and non-allied suppliers the coalition relies on; the stress lens's leading non-allied
  supplier; the nodes' supplier lists. The bundle's `strategic.countries` now lists allied economies only
  (an economy is listed when allied or when it holds a role). The Explorer's "Leading suppliers outside the
  alliance" now reads each compact's division of labour (largest anchor or specialist position, at least 10%
  of world supply of a node) and names suppliers by share, with no role.

**Rerun.** scripts/33 and scripts/84 on a copy of the 2026-11-21 current-trade run (stamps
`outputs/aib_current`), same recipe as commit 43e035c7. A control rerun with `non_allied_roles: true`
reproduced the committed bundle (compacts.be93214e) exactly apart from the provenance stamp.

**What changed (compacts.be93214e -> compacts.312f5bc4).** Nodes, classes, operational compacts,
coalitions, scenario files (byte-identical), investment flows, strategic compacts' spines and coverage,
allied cores, workstreams and all 1,085 theses are unchanged.

| | before | after |
|---|---|---|
| economies in `strategic.countries` | 74 (61 allied + 13 non-allied) | 61 (allied only) |
| economies with a primary or secondary role | 29 | 24 |
| compact-specific roles | 253 (225 allied + 28 non-allied) | 225 (the same 225 allied rows) |
| non-allied lead suppliers listed in compact cores | 23 across the 10 compacts | 0 |

Non-allied roles removed: CHN (primary RPA, SMI, STM; 10 compact roles), RUS (primary RPA, secondary STM;
3), KAZ (secondary RPA; 1), MMR (secondary RPA; 1), BHR (secondary STM; 1); compact roles only: LAO (2),
NGA, ZWE, MOZ, SVN, ECU, PNG, IRN, ARE, ISL, TTO (1 each).

Allied economies whose roles changed (the supply-role percentiles are now taken among allied economies):

- **ESP** gains a secondary Resource & Processing Anchor (secondary CIA -> CIA, RPA).
- **GBR** gains a secondary Resource & Processing Anchor (primary DMA; secondary DSP -> RPA, DSP).
- Order only, same set: JPN primary CIA, STM, SMI -> STM, CIA, SMI; FRA secondary SMI, CIA -> CIA, SMI;
  MEX secondary SMI, CBH -> CBH, SMI; NLD secondary DMA, STM -> STM, DMA; MYS beyond the cap DMA, RPA, SMI ->
  RPA, DMA, SMI.
- KOR keeps its roles; its headline now leads with Scale Manufacturing & Integration Platform (its first
  primary role is now 3+ points ahead of the next) instead of "Combines ... roles".
- Every other allied economy keeps its roles; the gate percentiles of the supply roles move slightly.

Zero roles for CHN, RUS and every other non-allied economy, in `strategic.countries` and `compactRoles`.

**Worth a look.** SVN (Slovenia, scale manufacturer in Wind), ISL (Iceland, scale manufacturer in Grid) and
ARE lose compact roles because they are outside the coalition tiers. If they should count as allied, add
them to `allied.treat_as_allied`.

### 8.1 Slovenia, Iceland and the UAE added to the trusted network (2026-11-23)

User decision after the roles change: `treat_as_allied: [TWN, CHE, SVN, ISL, ARE]` (64 trusted economies). AIB bundle
re-exported (scripts/33 -> 84, stamp `acbc3805`); review release `rel-4a271ac2b550`.

- `strategic.countries` 61 -> 64; compact roles 225 -> 230 (new: SC-BATTERIES Slovenia diversification partner,
  SC-WIND Slovenia scale manufacturer, SC-GRID UAE and Iceland scale manufacturers, SC-GRID Slovenia
  capacity-building host); theses 1,085 -> 1,152 (pairs with the three new members).
- The three hold no Strategic AIB Role at the bars. The larger allied pool shifts some allied secondary roles:
  Australia gains Capital & Investment Anchor; Argentina and the UK lose Development & Scale-Up Platform; Japan's
  primary roles reorder.
- Nodes, compacts, strategic compact cores and investment flows are unchanged.
- The Country roles page's operational network roles (supplier, specialist, host, investor, roles gained or lost)
  now list trusted-network economies only (`trustedOf()` in `dashboard/src/aib/derive/network.ts`). Russia and Iran,
  which supply in the scenario trade (the candidate universe is global), appear in "Who supplies whom" only.

## 9. One allied set (2026-11-25)

**Decision (user, 2026-11-25):** "Allied" is the 64-economy trusted network, and it is always the same coalition.

- **AIB demand scope = the trusted network.** `config/aib.yml` `demand_scope: include_treat_as_allied: true`: a
  coalition demand scope takes the core and partner tiers plus `config/aib_strategic_layer.yml`
  `allied.treat_as_allied`, the one file that keeps that list (`R/aib/aib_scope.R`; scripts/32 records it in
  `meta$demand_iso`, and scripts/33 takes its allied set from there). Taiwan, Switzerland, Slovenia, Iceland and the
  UAE are protected demand and can hold demand roles like any ally. Bundle `nDemandScope` 59 -> 64.
- **Explorer.** Every tier or allied grouping is decoded with `allianceTier()` / `allianceGroup()` in
  `dashboard/src/posture/trustedNetwork.ts`: the Sector groups are relabelled "Trusted core" / "Trusted partners",
  the Country profile tag reads "Treated as allied", the posture
  tier column and the Partnerships coalition lens list them, and the AIB header tile counts the trusted network.
  The coalition membership itself is unchanged.
- **Run (2026-11-25, current trade, `outputs/aib_current64`).** Node classes: 5 change (power cables, AC generators,
  switchgear and protection LOW INTERVENTION -> BUILD; graphite electrodes, wind towers COORDINATE -> BUILD), BUILD
  45 -> 50. Allied traded demand US$2.02tn -> 2.17tn; under the headline shock coverage after existing
  relationships falls 80.8% -> 76.3% and the gap left for new capacity rises 19.0% -> 23.6% (demand-weighted over 74
  nodes): exports to the five were previously free to be redirected to the 59, and now they are protected demand.
  Biggest moves in Semiconductors (SC-SEMIS Decoupling coordinated coverage 87.3% -> 80.0%). Sectoral compacts 35 ->
  37 (22 unchanged); compact roles 230 -> 235; strategic roles change for CAN, IND, KOR, MEX, PHL, TUR, USA.
- **Not yet carried through.** The posture bundle's `aib_header.members` (59) and the division table's allied set
  (61, from the 2026-10-27 AIB run) need scripts/26 and scripts/91 on a 64-set AIB run; the Explorer's header tile
  already counts the trusted network from the country tiers. The Sector data's geo groups (R `sector_dims.R`) still
  file the five under "rest": the regrouping has to come from the Sector export, because `meta.summary` is checked
  against the client's recomputation (a client-only remap broke that parity on 1,272 summary rows).
