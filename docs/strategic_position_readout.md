# Strategic Position and Posture: readout (2026-10-27 run; 2026-10-28 extension first)

Method: [strategic_position.md](strategic_position.md). Every figure below comes from the CSVs in
`diagnostics/strategic_position/` written by `scripts/26_build_strategic_position.R`, on the 10:13
`index_outputs.rds` build (which includes the uncommitted manufacturing-pair fix), the October 2026 GTA
extract (scripts/11) and the regenerated import-exposure table. Example cards are in
`diagnostics/strategic_position/decision_cards.md`.

## 2026-10-28 run: trade refresh, three Secure routes, the quadrant and partners

Same inputs as below plus the Comtrade refresh (`scripts/94`). Old figures are the 2026-10-27 run
(commit fa3ecda), compared through `SP_BASELINE_TBL`.

**Trade.** The structural benchmark averages 2022-2024 for 143 annual reporters (69,310 rows; Atlas
2023 share kept for non-reporters). Against the single-year measures it reorders countries little
(median within-cell rank correlation of Competitive Position 0.975), but 1,189 cells move more than 10
points, mostly where the Atlas lacked basket codes (UK Wind Extraction 0.37 -> 0.82). Monthly: the
universe is 73 economies, 61 publish monthly trade. **Common cutoff: December 2025** (51 of 61
monthly reporters, 84%, have all 24 matched months; 70% of the whole universe); momentum compares Jan-Dec
2025 with Jan-Dec 2024. China has only 12 of the 24 months (it reports January and February together),
so it has no momentum. Cells: 1,137 ok, 14 caution (Morocco's monthly totals are 10% off its annual
total), 633 small base, 350 incomplete coverage, 420 no monthly data. 1,151 cells take the 15% weight;
the largest movers are Oil Refining (Bulgaria -45%, Egypt -50%: Competitive Position -18 points).
Weight sensitivity (`trade_momentum_weight_sensitivity.csv`): structural vs 15% rank correlation
0.9986, 324 cells move >5 and 75 >10 points; 10% / 20% move 148 / 428 cells >5 points. **No posture
changes at any weight** (re-run): momentum only changes consistency flags (61 between 0% and 15%).

**Secure: 18 -> 174 cells** (no cell left Secure). By route: coercive chokepoint 18 (EV Refining x 15,
Gas Extraction x 3), operational control 5 (dual-use: Nuclear Refining for Brazil, Romania, Korea; EV
Refining for Russia and Viet Nam), systemic resilience 151 (Gas 48, Oil 53, Coal 24, Electric Grid
Assembly 23, Nuclear Components 3). Review countries: US 1 (EV Refining), China 3 (Gas Extraction and
Refining, Oil Extraction), Japan 4 (+ Oil and Coal Extraction, LPG), Korea 5 (+ Nuclear Refining
dual-use), Germany 3, UK 2, France 2, India / Australia / Canada 1. Self-supplied countries (US oil and
gas, Australia, Canada) are not resilience cases. Policy alignment of Secure: 92 under-prioritized, 41
aligned, 12 mixed, 29 no data: import-dependent fuel security is rarely where NIPO and PAMS record
effort.

**Quadrant** (producing stages, thresholds 0.67): catch_up 117, preserve 266, partner 1,456, watch
2,746, insufficient data 7,011 (no measured need or opportunity, mostly small economies). Compete
215 -> 1,839 cells. Thresholds at 0.60 / 0.70 move 357 / 166 postures (`quadrant_threshold_sensitivity.csv`).
Per review country (secure / catch up / preserve / partner / watch):

| Country | S | Catch up | Preserve | Partner | Watch |
|---|---|---|---|---|---|
| United States | 1 | 6 | 15 | 4 | 12 |
| China | 3 | 7 | 21 | 0 | 5 |
| Japan | 4 | 2 | 1 | 9 | 25 |
| South Korea | 5 | 2 | 2 | 9 | 22 |
| India | 1 | 8 | 10 | 9 | 7 |
| Germany | 3 | 2 | 14 | 7 | 15 |
| France | 2 | 4 | 13 | 5 | 16 |
| United Kingdom | 2 | 0 | 1 | 6 | 23 |
| Australia | 1 | 0 | 3 | 13 | 23 |
| Canada | 1 | 6 | 6 | 3 | 25 |

Examples (`quadrant_examples.csv`): Japan catches up in Battery Refining (need 0.75, opportunity 0.81)
and partners in Battery Extraction (Australia first); Korea preserves EV Assembly (opportunity 0.84,
need 0.13; policy under-prioritized) and catches up in EV Components; India catches up in Solar
Components and Battery Assembly and partners for oil (Saudi Arabia first) and EV minerals (Australia);
the US preserves gas, oil refining, solar and wind manufacturing and catches up in battery components
and assembly.

**Consistency flags: 425** - partner_despite_capability 354 (Viet Nam 16, Thailand 12, Mexico 11,
Japan 8, India 8), catch_up_already_leading 49, preserve_without_capability 22. The partner flags are
the main weakness of the opportunity axis: EO, placed among the node's contenders, rates established
exporters lower than their demonstrated position (Japan Battery Components: 13% of world supply,
Competitive Position 0.96, opportunity 0.25 -> Partner, flagged).

**Partners.** 1,456 Partner cells, 3,941 recommendations; none is the country itself, coercive,
untrusted or below 2% of supply. 15 partner gaps (all Oil Recovery). First partners: Germany 231, the
US 200, Australia 95, Japan 81, Korea 69; China 203 and Russia 49, all for China-leaning reporters
(the bloc rule), 34 of them for coalition partner-tier members (Indonesia, Viet Nam, Oman). Policy:
74 Partner cells are autarkic (protection in the top third: `partner_autarkic`).

**AIB division** (39 producing nodes with supply): gap none 12, single point of failure 9, weak
evidence 18 (the AIB covers 25 cells); no node lacks a trusted holder and every node has at least
three allied holders at 2%, because the coalition is 62 economies. The Explorer header: 31 core
members, 82% of the 34 priority nodes with credible trusted coverage, 22 unresolved gaps (weak evidence
counts as a gap).

## Coverage

| | cells (of 14,454) |
|---|---|
| Competitive Position percentile | 9,538 |
| Strategic Need percentile | 8,989 |
| Revealed Policy Priority percentile | 10,796 (PAMS only 8,554; NIPO + PAMS 2,204; NIPO only 38) |
| Classified posture | 14,124 (330 entrepot or registry rows not classified) |

219 countries x 11 headline technologies x 6 stages; 7,665 sensitive-layer rows.

## Evidence: what the six stages really rest on

| | mean evidence specificity | exact | split | shared | proxy | tech-wide |
|---|---|---|---|---|---|---|
| Economic Opportunity (old pillar, as built) | 0.26 | | | | | |
| Energy Security (old pillar, as built) | 0.72 | | | | | |
| Competitive Position | 0.75 | 85% | 8% | 7% | 0% | 0% |
| Strategic Need | 0.72 | 80% | 0% | 1% | 19% | 0% |
| Revealed Policy Priority | 0.35 | 4% | 30% | 30% | 0% | 35% |

Only 9.8% of the old EO pillar's weight was country-specific, observed, stage-specific evidence; the
rest was world constants, regional broadcasts, copies and fills. Competitive Position drops those, so
its cells mostly rest on stage-specific data (5,506 high, 3,948 medium, 78 low, 614 none). The weak
spots: Deployment (0.60) and the manufacturing pair, where GCIM capacity and capex are shared between
Components and Assembly. Strategic Need's proxy share is all Deployment (Assembly equipment need).
Policy priority is the least stage-specific: a third of the policy signal touching a cell is
technology-wide or cross-technology (PAMS).

## Validation questions (Strategic Position)

1. **Who is specialized?** China leads most manufacturing cells (Batteries, Solar and Wind Components
   and Assembly). Resource cells look right: Batteries Extraction China, Australia, Brazil, Russia,
   Zimbabwe; Nuclear Extraction Namibia, Australia, Canada, Russia, Kazakhstan; Oil Extraction Saudi
   Arabia, Russia, US, Canada, Iraq; Nuclear Refining Kazakhstan, Russia, Canada, France, Netherlands.
2. **Plausible?** Mostly. Two exceptions: Japan and Korea sit below the US, Canada and Hungary in
   Batteries Components, because recent capex (GCIM) lifts new investors over established producers;
   and re-export hubs (Singapore, UAE) lead some equipment baskets.
3. **Shared / proxy cells**: Components and Assembly (shared GCIM), Deployment (regional inputs), and
   all of policy priority (above).
4. **Stage prior.** Removing it changes 11 of 60 allied picks (9 of them had a margin below 0.01);
   the technology prior changes 15; both 24. Under the old prior the top cell sat at Extraction for 97
   countries and Deployment for 64; without it 110 and 49. Cross-country order within a cell cannot
   move (the priors are constant in a cell). The allied network (scripts/30) is dormant and never read
   the Strategic Index, so it has no assignment to change.
5. **Old priorities that disappear.** Of the eight review countries' top-5 Strategic Index cells (40),
   37 are Deploy under the new posture: 21 are Deployment cells and 13 Extraction cells (Japan's top
   three were Batteries, Solar and Wind Extraction, where it has no resource). Three survive as
   Compete: Australia Batteries Extraction, US Solar Components and China Batteries Recovery.
6. **Latent advantages** (position well ahead of attention): Japan Electric Grid Refining and
   Components (Compete preserve, priority 14th and 25th percentile), Germany Wind Refining
   (rare-earth refining, Compete adjacent capability, 4th), Germany EV Refining (Secure, 4th), China
   Batteries Components (Compete preserve, 22nd), US Electric Grid Extraction (copper, Compete
   preserve, 29th).
7. **Robust non-priorities** (low on position, need and priority): e.g. Germany Gas Refining, Korea
   Solar Deployment.
8. **NIPO support vs protection**: median within-country Spearman 0.25; they disagree on high vs low
   in 37% of cells. India and Australia are protection-led (Solar, Grid and Wind Assembly).
9. **NIPO vs PAMS**: median within-country Spearman 0.06: the two sources measure different things
   (trade and subsidy measures vs policy documents). Both are kept visible (`nipo_priority`,
   `pams_priority`, `policy_priority_sources`).
10. **CAT.** Among the 64 CAT-rated countries, the within-country Spearman of the old composite policy
    index with and without CAT is 0.455; 52.8% of their cells change policy third and 10 of 60
    allied picks change without it. CAT varied by technology only through the GHG multiplier; it is
    now country context (`cat_context`).
11. **Count vs instrument-weighted NIPO**: Spearman 1.00. The strength class adds nothing; the count
    is the default.
12. **Flow windows**: 1-year vs 3-year momentum 0.64, 3 vs 5 years 0.78, stock vs 3-year flow 0.52.
    1-year flow is too thin (passes for 41 countries); 3 years is the default.

Priority gap: the percentile, rank and z-score versions agree (median Spearman 1.00 and 0.97, sign
agreement 91%); the percentile version is kept.

## Strategic posture

| | Secure | Compete | Deploy |
|---|---|---|---|
| All countries (cells) | 18 (0.1%) | 215 (1.5%) | 13,891 (98%) |
| United States | 1 | 19 | 46 |
| China | 0 | 31 | 35 |
| Japan | 1 | 18 | 47 |
| Germany | 1 | 18 | 47 |
| South Korea | 1 | 16 | 49 |
| United Kingdom | 1 | 6 | 59 |
| India | 1 | 6 | 59 |
| Australia | 1 | 2 | 63 |

Sensitive layers: 995 of 7,665 layer rows are Secure, almost all control layers (battery-system
firmware and communications, grid control and metering, inverter firmware, turbine control,
connected-vehicle software) plus rare-earth magnets and separation, HALEU and fuel fabrication.

Compete by strategy: preserve 88, compete_costly 96 (a strong case with a cost: premium, partners'
alternatives, or climate cost for coal, oil and gas equipment), adjacent capability 16, catch-up 15,
leap ahead 0 (no curated leap candidate has evidence beyond novelty yet). Deploy variants: deploy
9,420, deploy_diversify 4,437, deploy_floor 34.

Confidence: Secure 18 moderate; Compete 45 high, 148 moderate, 22 low; Deploy 1,928 high, 7,003
moderate, 4,960 low. 39% of classified nodes have a posture the unknowns do not settle
(`posture_possible`); the strict reading puts them in Deploy, with the pivotal inputs listed.

### Does Secure find genuine chokepoints?

Cell-level Secure is rare and specific: EV Refining (graphite and rare-earth refining, China 94% of
world supply) for 15 countries (the US, Japan, Korea, Germany, the UK, India, Australia, Canada,
Brazil, Czech Republic, Netherlands, Norway, Poland, Taiwan, Turkiye), but not China (a 71% world
supplier itself); and Gas Extraction for Congo, Egypt and Senegal on the regional rule (bloc-rule
artifacts: see below). 1,134 classified nodes have an acute consequence but no exploitable
vulnerability, and 65 a vulnerability without an acute consequence. Concentration alone never makes
Secure: China holds 80% of graphite mining and 53% of world exports of battery cells and packs, and
neither Batteries Extraction nor Batteries Assembly is Secure (losing imported cells slows deployment
rather than disabling a critical function within a year: consequence moderate). The acute battery
vulnerability sits in the battery-system firmware layer, which is Secure for every review country but
China.

### Does Compete find country capabilities rather than important technologies?

Yes, with caveats. Compete needs a position or a base plus a strategy:

- Japan and Korea: batteries (preserve), grid equipment, nuclear components and assembly;
- Germany: grid, EV and wind equipment; the US: batteries (catch-up in Assembly), EVs, grid, solar
  components and assembly;
- the UK: nuclear refining (preserve: enrichment), nuclear components, nuclear recovery (catch-up);
- Australia: lithium mining (preserve) and rare-earth refining (catch-up); India: six cells.

Solar Components is Compete for China, the US, Japan, Korea and Germany and Deploy for India, the UK
and Australia; Solar Assembly is Compete for China and the US (preserve) and Germany (costly), and
Deploy for Japan, Korea, India, the UK and Australia.

### Does Deploy hold a large share?

Yes: 98% of all cells, and 53-95% of the review countries' cells. Deploy is an affirmative conclusion
with a design (low fence, wide gate, enabling build), not a residual.

### Do countries diverge?

33 of the 66 technology x stage cells get at least two different postures across the eight review
countries (`posture_country_divergence.csv`). Batteries Assembly is Compete (preserve) for China,
Japan and Korea, Compete (catch-up) for the US, and Deploy (diversify) for Germany, India, the UK and
Australia. Coal Components is Compete-costly for China, Japan, Korea and Germany and Deploy elsewhere.

### Layer precision

Inverter firmware, grid SCADA and metering are Secure for Japan, Korea, India and Australia while
Electric Grid Assembly is Deploy or Compete; battery-system firmware is Secure for every review
country but China, while the battery cell layer is Compete or Deploy.

### Policy misalignment

| | aligned | under-prioritized | over-prioritized | mixed | insufficient data |
|---|---|---|---|---|---|
| Secure | 8 | 8 | | 2 | |
| Compete | 78 | 80 | | 57 | |
| Deploy | 5,208 | | 800 | 238 | 7,645 |

Deploy is `insufficient_data` where its strategic case is not settled (the unknowns still allow Secure
or Compete): heavy or light policy there is not evidence either way. Under-prioritized: Secure EV
Refining for India (priority 3rd percentile) and Germany (4th); Compete Germany Wind Refining (4th),
Korea Nuclear Refining (catch-up, 14th), Japan Grid Refining (14th), China Batteries Components
(22nd). Over-prioritized Deploy: China Coal Extraction (98th), India Solar Assembly (98th), Australia
Solar Assembly (92nd), US Nuclear Assembly (92nd), UK Gas Refining (90th), Germany Batteries Assembly
(87th).

### Allied solutions

Secure nodes with weak domestic position where trusted partners hold at least 30% of world supply get
allied delivery: Australia's smart-meter control layer (49% of the carrying hardware from coercive
suppliers, 52% trusted world supply), and battery-system firmware for the US, the UK, India and
Australia (mixed for Japan, Korea and Germany). Where no trusted supply exists at scale (graphite and
rare-earth refining: trusted 4-6% of world supply), delivery is mixed: new trusted capacity at home
and with partners, bridged by stockpiles.

### Threshold sensitivity (share of classified nodes whose posture changes)

| Rule moved | changed |
|---|---|
| coercion line 0.4 / 0.6 | 0% / 0% |
| coercive import share 0.15 / 0.35 | 0.03% / 0.02% |
| position strong 5% / 15% | 0.11% / 0.03% |
| position foothold 5% | 0.25% |
| allied alternative 0.4 / 0.6 | 0.02% / 0% |
| acute includes moderate consequence | 0.15% |
| acute severe only | 0.11% |
| catch-up market 0.5 | 0.03% |
| coercive powers only (no bloc rule) | 0% |

The posture is driven much more by the curated node attributes (consequence, substitution, timing,
learning) than by the numeric thresholds: those attributes are where review effort pays.

## Things that look wrong or fragile

- **The bloc rule cuts both ways.** For China-leaning UN voters, US-bloc suppliers count as coercive:
  the Gas Extraction Secure cells (Congo, Egypt, Senegal) rest on US-bloc supply. The powers-only rule
  is a configured sensitivity.
- **Pipeline transit hides origin.** Hungary and Slovakia do not come out Secure on Russian gas,
  because declared imports name the transit country.
- **The physical import leg reads the cell's HS6 basket, not the driving mineral** (Wind Extraction's
  basket is one monazite code).
- **Recent capex lifts new investors** over established producers in Competitive Position (Japan and
  Korea in Batteries Components).
- **Over-prioritization is partly mechanical**: policy priority is a within-country percentile, so
  every country has a top third, and most cells are Deploy.
- **Fossil equipment shows as Compete-costly** for machinery exporters (Japan, Korea and Germany in
  Coal and Gas Components and Assembly): a real export position whose protection has a climate cost.
- **Solar Components rests on an unknown learning case** (the Draw the Fence curation leaves it
  contested): its Compete postures are the strict reading of a position proven at least moderate.
- **Node attributes are first-pass curation** (54 of 101 rows low confidence), and some cited figures
  were recalled, not re-checked (Japan's oil cover, the inverter-disabling incidents, MOFCOM dates).
- **Leap-ahead never fires**: every leap candidate has evidence beyond novelty "partly".
