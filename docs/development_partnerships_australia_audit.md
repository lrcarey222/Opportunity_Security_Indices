# Development partnerships: Australia audit (pilot, 2026-11-03; review fixes 2026-11-04)

This audit comes from `scripts/93_export_development_partnerships.R --audit AUS`. The full tables are:
- `data/processed/development_partnerships/development_partnerships_AUS.csv`: every pair considered, all fields;
- `development_partnerships_AUS_audit.csv`: the eight checks for every development pair.

No threshold was tuned to produce pairs: every bar is an existing engine setting. The 2026-11-04 rerun applies
the review fixes in `docs/development_partnerships.md`:
- constraints only where their input is observed;
- the Preserve evidence tests;
- the five capability checks;
- aid specificity by where the money is;
- CM-FDI and OCI coverage.

## Result

Australia has 116 pairs considered:

| Route | Pairs | Notes |
|---|---|---|
| Development | 49 | with 16 developing partners across 10 steps |
| Commercial | 29 | the partner already Preserves the step or hosts its AIB capacity |
| Review | 19 | insufficient evidence: 11 capability unverified, 1 thin anchor, 7 evidence, confidence or a review flag |
| Excluded | 19 | the trust screen |

Compared with 2026-11-03 (53 / 32 / 12 / 19):
- Mozambique, Colombia and Tanzania leave battery materials refining. The capacity behind their positions is
  critical-minerals mines, not refineries.
- South Africa leaves rare-earth and wind materials refining. Its anchor is the mining of other wind minerals.
- India's polysilicon refining (a Preserve) and Morocco's grid assembly (a Preserve) are low-confidence calls.
  They move from commercial to review.

## The five pairs in the brief

The brief takes the top development pair of each step, in rank order, each partner once. The partners are
unchanged by the fixes.

| # | Step | Partner | 1 Donor reason | 2 Eligible | 3 Catch Up | 4 Same capability | 5 Aid not overstated | 6 Tool fits | 7 Productive | 8 Additional |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | Wind turbines | Türkiye | Partner, high need, shortfall, BUILD | Upper middle income | supply + export share; feasibility, adjacency (evidence medium, confidence high) | wind assembly; BUILD turbines and meters | no aid recorded | finance-led (UMI); project preparation for plant below the capacity bar | Assembly | CP 67, not a host |
| 2 | EV drivetrain components | Thailand | same | UMI | supply + export share; feasibility, input costs, adjacency | EV components; BUILD packs, magnets, motors | $2.4m, at Deployment only | finance-led; plant capacity is not covered by the data, so no constraint leads | Components | CP 68, not a host |
| 3 | Battery cells and packs | Indonesia | same | UMI | supply share; feasibility, input costs, adjacency | battery assembly; BUILD li-ion cells | $16m, at Deployment only | finance-led; project preparation for "no plant in the data" | Assembly | CP 55, not a host |
| 4 | PV modules and inverters | Morocco | same | Lower middle income | supply + export share; adjacency (confidence moderate) | solar assembly; BUILD modules, inverters | no aid recorded | project preparation, investment promotion, industrial infrastructure and skills first; input costs not covered | Assembly | CP 40, not a host |
| 5 | EV and charger manufacturing | Mexico | Partner, high need, BUILD (allied supply covers) | UMI | supply + export share, operating capacity, adjacency | EV assembly; BUILD cars, buses | no aid recorded | finance-led; infrastructure for "input costs" | Assembly | CP 68, not a host |

The brief's second table gives each pair's relationship today and what would make it durable, from the pair's
own fields:
- the direct investment position (economy-wide; all five are partner-reported);
- CM-FDI factory projects (none recorded for any of the five);
- the AIB simulated investment pairings (Türkiye, Mexico);
- the recipient's role in the compact of its own step;
- deployment demand.

Mechanical results across all 49 development pairs:
- Checks 1–7 pass for all 49. Check 5 now tests each aid label against the named, shared, proxy and pooled
  amounts.
- Check 8 flags one pair: Brazil, blades, towers and nacelle parts, with a Competitive Position score of 78.
  The engine places it below the top quarter among contenders, so it stays Catch Up. On the score it sits
  above 75, so it should be read as close to commercial scale.
- Aid at the step of a development pair is recorded for five pairs, all with Viet Nam or the Philippines, and
  all under stage-wide codes.
- CM-FDI is NA for 11 of the 49 pairs (technologies outside the table), and OCI is NA for 30 (steps outside its
  crosswalk). No observed value is above 0.

Judgment notes on the five pairs:
- **Türkiye, Thailand, Indonesia and Mexico** are large upper-middle-income manufacturers. The tools lead
  with finance, guarantees and offtake, and grants are kept for public goods. That is the additionality
  answer for them.
- **Morocco** is the only lower-middle-income partner in the five.
- **Australia's aid** does not reach Türkiye, Morocco or Mexico. Its aid to Thailand and Indonesia sits at
  Deployment.

## Validation lists

**Australian Partner / Secure steps with no qualifying development partner:**

| Step | Candidates and why none qualifies |
|---|---|
| Rare-earth and battery-metal refining (Secure) | Malaysia and Thailand are commercial (AIB capacity hosts). Argentina, Indonesia, Mozambique, Congo, the Philippines, Madagascar and Tanzania are review cases: their anchors rest on battery-metal capacity data, not rare earths (Indonesia Preserves the step, but on such an anchor). Mexico is a thin anchor. China, Laos and Zimbabwe are excluded by the trust screen. |
| Battery recycling (Partner) | India and Malaysia are review cases (no evidence rating). China is excluded by the trust screen. |
| Drilling and pump components; waste-oil recovery (Partner, low confidence) | No developing economy is Catch Up or Preserve. Australia's own calls here are low confidence. |

**Steps where the capability checks act:**

| Step | Australian pairs | Effect |
|---|---|---|
| Battery materials refining | 7 development; Mozambique, Colombia and Tanzania review | the capacity data are mines |
| Rare-earth and wind materials refining | India development; South Africa review | South Africa's anchor is the mining of other wind minerals |
| Uranium conversion and enrichment | none; Australia's own step is a review case | Namibia's pairs (with other donors) move to review on the same uranium-compounds measure |
| Nuclear fuel and reactor parts; reactors and steam generators | none for Australia | every pair, across all donors, is a review case |

**High Development Potential partners that fail the Catch Up test**, among the top 15 by Development
Potential:

| Partner | Why |
|---|---|
| China | the trust screen |
| Uzbekistan | the trust screen (nuclear refining) |
| Mongolia, Georgia, Mauritius, Mauritania, Bhutan, Costa Rica | no Catch Up step that Australia needs |

Viet Nam, Serbia, India, Thailand, Malaysia, South Africa and Mexico pass in at least one step.

**Development partners with no Australian aid recorded at any supply-chain step** (2020–2024, energy,
industry and mining codes): Türkiye, Morocco, Mexico, Honduras, Serbia and Madagascar. Mexico receives small
aid in codes that map to no step. Brazil, India and South Africa receive under $0.1m, at other
steps.

**Aid relationships to redirect or deepen:**

| Partner | Aid, 2020–2024 | Reading |
|---|---|---|
| Papua New Guinea | $85m, Australia's largest | All of it falls outside the battery-materials refining where PNG has a credible pathway (an export-share anchor). Redirect part of it to refining project preparation. |
| Viet Nam | $17m | Under $0.1m of it is at the partnership steps (stage-wide codes); the rest is elsewhere. |
| Philippines | $1.9m | $1.3m at the partnership steps (stage-wide industry codes): deepen it. |
| Indonesia | $16m | At Deployment. |
| Ukraine | $13m | At Deployment; Ukraine's EV-components pathway is credible. |
| Thailand | $2.4m | At Deployment. |
| Palau, Tonga, Cambodia, Fiji, Solomon Islands | | Large recipients with no partnership in this analysis. Their aid serves other aims and is not judged. |

## Inverse view: Ukraine

Ukraine is the only non-high-income Core Coalition member, so it tests the recipient view.
- It has one Catch Up step, EV drivetrain components. The anchor is a share of world supply, corroborated by
  specialization in the adjacent step, and the step passes.
- 35 high-income economies need the step; the view lists the donors with their own need first.
- Its constraints read as know-how outside the top quarter and input costs. Plant capacity is not covered by
  the data for EV components, so "no plant" and "trade only" are not stated as constraints.
- The tools lead with finance, as for an upper-middle-income economy, with public support for skills and
  supplier development.
- The dashboard flips the question to "Where could development partnerships help Ukraine Catch Up?". It also
  lists the economy's Preserve steps as the commercial route (tested in `devpart.view.test.tsx`).

## Data limitations

- **CRS aid:** mostly stage-level, not technology-level. Components and Assembly share codes, and recycling
  has no code.
- **IMF direct investment:** Australia reports no outward IMF positions under the Friendshore measure, so the
  ties are partner-reported and economy-wide.
- **CM-FDI factory projects:** they do not separate Components from Assembly, and they cover batteries, EVs,
  solar and wind only. None links Australia to these partners.
- **OCI finance:** it covers Export Finance Australia, but no Australian transaction falls in these steps.
- **Bankability:** proxied by AIB host lists.
- **Matching:** step-level only. Layers (for example magnets inside EV components) are not matched.
- **Market tie:** Australia's imports from each partner in the step's basket are not yet a pair field.
