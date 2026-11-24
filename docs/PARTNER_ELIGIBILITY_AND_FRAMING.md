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

The Explorer uses three different "trusted / allied" sets:

1. **Coalition: 59 economies.** 31 core and 28 partner, from `config/coalition.yml` via scripts/13. This is
   `countries.tier` in the posture core and `tier` in the Country fact sheets. The two agree on every row
   checked.
2. **AIB strategic "allied": 61 economies.** The coalition plus `treat_as_allied: [TWN, CHE]`
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
