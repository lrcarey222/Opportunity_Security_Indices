# Strategic Position: readout of the canonical decision framework (2026-10-29 run)

Method: [strategic_position.md](strategic_position.md). Plan and audit: [decision_framework_plan.md](decision_framework_plan.md).
Final report draft: [decision_framework_report.md](decision_framework_report.md).

Every figure below comes from `diagnostics/strategic_position/` as written on 2026-10-29 by
`scripts/95_build_deployment_need.R` (Ember yearly data and 2030 targets, WEO 2025, GEVO 2026),
`scripts/26_build_strategic_position.R` (run with `SP_BASELINE_TBL` = the 2026-10-28 table, so the old-vs-new files
are written) and `scripts/27_review_strategic_position.R` (`decision_review.md`, `choice_cards.md`,
`legacy_osi_corners_vs_choice_*.csv`), rerun on commit 91dba6f (the review fixes: an unmeasured Competitive Position
lowers confidence, a top-quarter score that fails the Preserve test says why, and over-prioritization rests on observed
NIPO protection; the choices did not change). The Explorer's posture bundle (`dashboard/public/data/posture`, schema 2,
`git_commit` 91dba6f) was exported from the same table by `scripts/91`. The readouts of the earlier posture engine (2026-10-27 run, 2026-10-28
trade refresh and need x EO quadrant) are in git history (this file at commit 1bd7bde).

Thresholds (config `decision_framework`): supply-chain need 0.67, industrial opportunity (prize) 0.6, Competitive
Position in the top quarter of the node's contenders (0.75) with the capability floor, deployment need 0.6,
borderline band 0.05.

## 1. The choices

14,124 country x technology x six-stage cells (214 economies x 11 technologies x 6 stages):

| Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|
| 174 | 86 | 1,660 | 1,592 | 514 | 4,803 | 5,295 |

By stage (Secure / Preserve / Catch Up / Partner / Accelerate / Watch / not assessed):

| stage | S | P | CU | Pa | A | W | n.a. |
|---|---|---|---|---|---|---|---|
| Extraction | 71 | 8 | 64 | 298 | - | 778 | 1,135 |
| Refining & Processing | 77 | 12 | 345 | 415 | - | 986 | 519 |
| Component Manufacturing | 3 | 32 | 638 | 389 | - | 781 | 511 |
| Final Assembly & Equipment | 23 | 34 | 483 | 441 | - | 862 | 511 |
| Deployment & Use | - | - | - | - | 514 | 1,169 | 671 |
| Recovery & Recycling | 0 | 0 | 130 | 49 | - | 227 | 1,948 |

Review economies (of 66 nodes each):

| economy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| United States | 1 | 9 | 4 | 5 | 2 | 25 | 20 |
| Japan | 4 | 1 | 10 | 10 | 5 | 16 | 20 |
| South Korea | 5 | 6 | 10 | 8 | 6 | 11 | 20 |
| India | 1 | 6 | 11 | 7 | 9 | 12 | 20 |
| Germany | 3 | 3 | 8 | 8 | 4 | 20 | 20 |
| United Kingdom | 2 | 0 | 11 | 13 | 3 | 17 | 20 |
| France | 2 | 5 | 8 | 7 | 2 | 22 | 20 |
| Australia | 1 | 1 | 6 | 14 | 5 | 19 | 20 |
| Canada | 1 | 1 | 19 | 7 | 1 | 17 | 20 |
| China | 3 | 16 | 3 | 2 | 8 | 14 | 20 |
| Brazil | 2 | 1 | 6 | 6 | 3 | 28 | 20 |
| Italy | 2 | 1 | 11 | 9 | 6 | 17 | 20 |
| Mexico | 0 | 0 | 13 | 18 | 4 | 11 | 20 |
| Indonesia | 2 | 5 | 11 | 11 | 5 | 12 | 20 |

All 214 economies: `choice_by_country.csv`. Across economies the median has 0 Secure, 0 Preserve, 8 Catch Up, 7
Partner, 1 Accelerate, 23 Watch and 23 not assessed; 83 economies have at least one Secure node, only 25 at least one
Preserve (China 16, United States 9, India and South Korea 6), 129 at least one Accelerate.

What this shows:

- **Watch is the largest assessed answer** (4,803, 54% of assessed cells): the framework makes non-priorities
  explicit, as the spec asks. 1,169 of them are Deployment & Use nodes with deployment need below 0.6.
- **Preserve is rare** (86): it now needs a top-quarter position among the node's real contenders (economies with
  at least 0.5% of world supply) and the capability floor. Most economies have no such node.
- **Catch Up is large** (1,660) and is the weakest result (section 2): the credible-pathway test (design D3, any
  one of five legs) fires for 63% of the high-prize cells without a high position (1,660 Catch Up, 345 Partner,
  643 Watch).
- **Not assessed is structural, not country-specific**: every review economy has exactly 20, the same 20 nodes,
  because those nodes have no measured supply-chain need for any economy: the whole Geothermal and Green Hydrogen
  chains (10), Recovery of Coal, Gas, Electric Grid, EVs, Nuclear, Solar and Wind, and Nuclear, Solar and Wind
  Extraction. No producing cell lacks a prize; all 4,624 producing not-assessed cells lack need. 671 Deployment
  cells lack deployment need (mostly Oil, Green Hydrogen and EVs in small economies).

## 2. Threshold sensitivity

Cells per choice when one threshold moves (`choice_threshold_sensitivity.csv`; base = the configured thresholds):

| parameter | value | Preserve | Catch Up | Partner | Accelerate | Watch | cells changed |
|---|---|---|---|---|---|---|---|
| base | | 86 | 1,660 | 1,592 | 514 | 4,803 | 0 |
| need | 0.60 | 95 | 1,660 | 1,921 | 514 | 4,465 | 338 |
| need | 0.75 | 81 | 1,660 | 1,182 | 514 | 5,218 | 415 |
| prize | 0.50 | 111 | 2,000 | 1,437 | 514 | 4,593 | 365 |
| prize | 0.55 | 96 | 1,728 | 1,562 | 514 | 4,755 | 78 |
| prize | 0.65 | 73 | 858 | 1,842 | 514 | 5,368 | 815 |
| prize | 0.70 | 71 | 802 | 1,870 | 514 | 5,398 | 873 |
| CP | 0.67 | 114 | 1,635 | 1,589 | 514 | 4,803 | 28 |
| CP | 0.85 | 52 | 1,687 | 1,599 | 514 | 4,803 | 34 |
| deployment | 0.55 | 86 | 1,660 | 1,592 | 669 | 4,648 | 155 |
| deployment | 0.67 | 86 | 1,660 | 1,592 | 372 | 4,945 | 142 |
| deployment | 0.75 | 86 | 1,660 | 1,592 | 245 | 5,072 | 269 |

Secure (174) does not move with these thresholds (it is the engine's override; its own rule sensitivity is in
`posture_threshold_sensitivity.csv`). The prize threshold is the knife-edge: between 0.60 and 0.65 Catch Up halves
(1,660 -> 858), because the node prize is dominated by one country-invariant percentile across 55 nodes, so whole
nodes cross the line together. The need threshold mostly trades Partner against Watch. The CP bar moves few cells.
No threshold was tuned to improve these numbers; the configured values are the design's, except deployment need 0.6
instead of the design's starting 0.67, chosen for selectivity: deployment need is a mean of up to three family
percentiles, narrower than one percentile, so 0.6 takes about the top 30% of assessed Deployment nodes (514 of 1,683,
31%), the selectivity of supply-chain need at 0.67 (a percentile), where 0.67 would take 22% (372). At 0.67 the United
States would have no Accelerate node (its highest deployment need is Batteries, 0.64), and Germany and France would keep
only Green Hydrogen (0.71 and 0.84; `deployment_need.csv`).

The credible-pathway rule is the other lever (not a threshold, reported for transparency):

| pathway rule | Catch Up cells kept |
|---|---|
| any one of five legs (design D3, used) | 1,660 |
| two legs or more | 748 |
| any leg except the adjacent-stage leg | 912 |

The legs among Catch Up cells: adjacent stage 1,316, feasibility 565, learning 539, input cost 397, investment
pipeline 106. The adjacent-stage leg (adjacent world share >= 5% or the adjacent stage among the economy's stronger
cells, within-country CP percentile >= 0.6) alone carries 748 of the Catch Up cells. Tightening it is a user decision
(listed in section 9).

## 3. Legacy vs new (spec XXI.1)

**Legacy need x EO quadrant** (the live posture until 2026-10-28) against the canonical choice, all classified cells:

| legacy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| preserve | 5 | 49 | 73 | 11 | 75 | 222 | 3 |
| catch_up | 11 | 17 | 57 | 41 | 39 | 34 | 7 |
| partner | 113 | 5 | 248 | 968 | 62 | 378 | 27 |
| watch | 26 | 11 | 539 | 201 | 121 | 2,382 | 31 |
| insufficient data (shown as Deploy) | 19 | 4 | 743 | 371 | 217 | 1,787 | 5,227 |

(Rows are the legacy quadrant value on every cell, including the Deployment rows where it was computed for
information only, so the row totals exceed the legacy distribution of `decision_review.md`; `legacy_quadrant_vs_choice.csv`.) The largest transitions and their first explaining driver
(`decision_review.md`): insufficient data -> not assessed 4,624 (now shown as not assessed instead of Deploy);
insufficient data -> Watch 1,182 and -> Catch Up 647 (Competitive Position and the pathway now decide cells the
legacy EO could not rank); Deployment -> Watch 1,169, -> not assessed 671, -> Accelerate 514 (deployment need);
watch -> Catch Up 539 and partner -> Catch Up 248 (the cleaned prize); partner -> Watch 220 and watch -> Partner 174
(the cleaned need); preserve -> Watch 119 (the cleaned prize). Drivers over all changed cells: not assessed 4,624,
deployment need 2,354, Competitive Position and pathway 2,223, prize 989, need 691.

**Legacy OSI corners** (Energy Security x Economic Opportunity, the Country fact sheets' `legacy_corner`, schema 3;
6,351 placed cells) against the choice, with the conceptual mapping:

| legacy corner | maps to | cells | agree | agreement | to Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Grow | Preserve | 2,131 | 55 | 3% | 20 | 55 | 228 | 226 | 156 | 992 | 454 |
| Upgrade | Catch Up | 1,108 | 100 | 9% | 41 | 12 | 100 | 278 | 40 | 302 | 335 |
| Exposed | Partner | 2,018 | 482 | 24% | 71 | 6 | 315 | 482 | 81 | 717 | 346 |
| Stable | Watch | 1,094 | 452 | 41% | 11 | 7 | 99 | 114 | 32 | 452 | 379 |

Disagreements attributed (first that applies): prize 1,895, not assessed 1,368, deployment need 1,053, supply-chain
need 595, Competitive Position 208, Secure override 143. The corners agree poorly with the choice, and they should:

- A legacy corner is **relative to the economy's own median sector** (the 2026-10-15 rule): every economy has Grow
  sectors, by construction about a quarter of its placed sectors. Preserve is **absolute among contenders** (top
  quarter of the economies that hold the node). Grow -> Preserve agreement of 3% mostly reflects that difference.
- Legacy Energy Security mixes vulnerability with capability, reserves and deployment; legacy Economic Opportunity
  mixes market prize with current capability (export share, RCA, production, capacity), cost and investment. The new
  framework separates them (need, prize and position each answer one question), so cells move whenever the mixed
  index was carried by the "wrong" component (for example a high legacy EO driven by current export share now shows
  as a strong position with a modest prize).
- Deployment & Use was never decided by the legacy corners in a deployment sense (Energy Security there reads
  import exposure of the equipment); 1,053 disagreements are deployment-need decisions.

Named examples (review economies; legacy quadrant -> choice): France Nuclear Components and Gas Refining preserve ->
Watch (legacy EO 0.93 / 0.89 from capability and cost; world nuclear and gas equipment demand growth gives a prize of
0.42 / 0.25); Australia Batteries Components partner -> Catch Up (prize 0.85, pipeline and adjacent-stage pathway);
Canada Solar and EV Components watch -> Catch Up (prize 0.85 / 0.78, the feasibility leg with adjacency or learning); Canada Coal
Components watch -> Partner (need 0.76, prize 0.16).

## 4. Industrial Opportunity validation (spec XXI.2)

`industrial_opportunity` over the 12,045 producing cells: min 0.01, quartiles 0.35 / 0.54 / 0.67, max 0.97; 4,734 at
or above 0.6.

| Spearman with | pooled | median within node | cells |
|---|---|---|---|
| Competitive Position percentile | 0.09 | 0.36 (37 nodes where the prize varies) | 8,002 |
| supply-chain need | 0.04 | 0.13 | 7,146 |
| legacy EO opportunity percentile | 0.24 | 0.18 | 7,342 |

Component weights: world demand growth (country-invariant) 82% of the prize, global capex momentum 4%, national
home market 14% (where it lifts the prize; 3,350 cells). **Not a disguised capability score**: pooled correlation with
CP is 0.09; within a node only the home market can move the prize, and there the correlation is 0.36 (large home
markets are partly large producers). The largest movers from legacy EO are Green Hydrogen and Batteries Recovery /
Components / Assembly in economies without capability (legacy EO near 0, prize 0.94-0.97): the prize now says "the
world market for this node grows fast" and leaves "can this economy do it" to Position and the pathway. The flip side
is that the prize is mostly a property of the node, not of the country, so most country differentiation now comes from
need, position and the pathway.

## 5. Position-choice consistency (spec XXI.3)

Before = the 2026-10-28 need x legacy EO quadrant; after = the canonical choice; both counted on the same 2026-10-29
Competitive Position and measured supply shares (`consistency_before_after.csv`):

| contradiction | before | after |
|---|---|---|
| Partner with CP in the contenders' top quarter | 8 | 2 |
| Partner with cross-country CP >= 0.75 | 363 of 1,456 (25%) | 361 of 1,592 (23%) |
| Catch Up with 10% or more of world supply | 8 | 6 |
| Catch Up with CP in the contenders' top decile | 10 | 3 |
| Catch Up with cross-country CP >= 0.9 | 49 of 117 (42%) | 207 of 1,660 (12%) |
| Preserve without capability (cross-country CP < 0.25 or < 0.5% of world supply) | 66 | 0 |
| Preserve below the contenders' median | 43 | 0 |

The contender-level contradictions fall sharply and Preserve-without-capability is gone by construction. The
cross-country counts do not fall in number, because far more cells are now decided (Catch Up 117 -> 1,660); as rates
they fall (Catch Up 42% -> 12%; Partner 25% -> 23%, barely). A cross-country top-decile position is not a leading one
in a node with 30-plus contenders, so most of the 207 are mid-ranked contenders in crowded nodes; the 361 Partner cells
with cross-country CP >= 0.75 are mostly the same story in Extraction and Refining (large producers' neighbours with
high need and a low prize).

Remaining flags (kept, not silently overridden):

- `review_strong_position_low_prize` (16, all Preserve, design D2): strong position, high need, low prize. Mostly
  fossil and nuclear incumbents: Russia Nuclear Refining and Components, Russia, Canada and United States Oil
  Extraction, UAE Gas and Oil Refining, Indonesia Coal Refining, France Nuclear Refining, Japan Coal Components,
  India Solar Refining, Viet Nam Coal, Grid and Nuclear Assembly.
- `review_top_quarter_below_floor` (11: 9 Catch Up, 2 Partner): a top-quarter contender score that fails the
  capability floor. Solar Components in India, Viet Nam, Thailand and France (1-2% world shares in a node China
  dominates); EV-minerals Extraction in Australia, Russia and the Philippines (supply share not quantified); Russia Gas
  Extraction (Partner: 9.6% of world supply, just under the engine's 10% "strong" line, with no complementary
  capability recorded, so the floor reads "none"); Nigeria Gas Refining (Partner); Saudi Arabia Gas Assembly.
- Not a flag, but now named: a top-quarter score held by an economy below the contender line (under 0.5% of world
  supply) is not a strong position either. Its trace says `not_a_contender` and its reason "a top-quarter score
  without a contender share" (32 nodes: 11 Catch Up, 3 Partner, 18 Watch; e.g. Canada Batteries Assembly 0.88 and
  India Batteries Assembly 0.75, each with 0.13% of world supply). Until this run their reason said "below the top
  quarter" and their trace "capability floor not met".

## 6. Deployment validation (spec XXI.4)

`deployment_need` over 1,683 assessed Deployment cells: quartiles 0.32 / 0.49 / 0.65; Accelerate 514, Watch 1,169, not
assessed 671. Accelerate by technology: Gas 62, Batteries 58, Electric Grid 56, Geothermal 50, Wind 50, Nuclear 49,
Solar 48, Coal 41, Green Hydrogen 35, EVs 33, Oil 32.

Accelerate in the nine review economies:

| economy | Deploy · Accelerate | borderline Watch (0.55-0.60) |
|---|---|---|
| United States | Batteries (0.64), Gas | Nuclear 0.60, Grid 0.58 |
| Japan | Nuclear (0.78), Solar (0.66), Wind (0.65), Gas, Green Hydrogen | Geothermal 0.57 |
| South Korea | Nuclear (0.80), EVs (0.80), Solar (0.73), Wind (0.72), Green Hydrogen, Geothermal | Storage 0.56, Grid 0.56, Gas 0.55 |
| India | Nuclear (0.81), Storage (0.80), Solar (0.79), Wind (0.75), Grid (0.75), Coal, Gas, Oil, Geothermal | Green Hydrogen 0.55 |
| Germany | Storage (0.66), EVs (0.65), Solar (0.63), Green Hydrogen (0.71) | Wind 0.59, Grid 0.58 |
| United Kingdom | Storage (0.69), Wind (0.66), Grid (0.64) | EVs 0.59, Coal 0.58, Solar 0.55 |
| France | EVs (0.64), Green Hydrogen (0.84) | Nuclear 0.56 |
| Australia | Storage (0.83), Grid (0.71), Wind (0.66), Solar (0.61), Oil | |
| Canada | Gas | |

Against the spec's questions:

- *Does it reflect expected demand?* Yes where the data are national: India (WEO Current Policies 5.1%/yr demand
  growth) accelerates every major clean technology; Germany, the UK and France (falling or flat demand) accelerate
  only where their own targets or system need are large. Demand is WEO-national only for six economies (BRA CHN IND
  JPN RUS USA); elsewhere it is the 2020-25 Ember trend.
- *Does it identify major target gaps?* Yes for renewables: UK Wind (policy gap 0.91), Germany Solar (0.76), South
  Korea Solar and Wind (0.71 / 0.77), Australia Wind (0.79). No national nuclear, EV or grid targets are in the staged
  data, so those technologies have no policy-gap family (Japan, US, France, Germany nuclear rest on demand and system
  need only).
- *Does it recognise grid / storage needs?* Partly. Storage and grid read the renewables-target gap and the solar +
  wind share (flexibility); congestion and transmission adequacy are not assessed (no national data). Storage
  accelerates in five of nine; the grid in three (Australia, India, UK) and sits just below the line in the US,
  Germany and Korea.
- *Does it avoid classifying high deployment need as Partner?* Yes, by construction: no Deployment node is Compete
  (enforced by the R exporter and the TS contract).
- *Can a technology be low manufacturing priority but high deployment priority?* Yes, often: across all economies
  328 technology pairs accelerate deployment while Components and Assembly are Watch or Partner, and 372 the reverse
  (deployment Watch, manufacturing Preserve or Catch Up). Review examples: Australia Solar and Wind (Accelerate;
  manufacturing Watch / Partner), Japan Nuclear (Accelerate; manufacturing Watch), India and UK Grid (Accelerate;
  equipment Watch / Partner); the reverse: United States Solar, Wind and EVs (manufacturing Preserve; deployment Watch,
  need 0.47 / 0.37 / 0.09), Canada Solar, Wind, Nuclear, EVs and Storage (manufacturing Catch Up; deployment Watch).

What looks weak:

- **United States EVs 0.09** and **India EVs 0.30**: GEVO 2026 Current Policies has the US EV sales share falling 10%
  -> 8% by 2035 and India 4% -> 5%; the US is not an oil importer (system need 0.16). Correct under current policy,
  but it is a current-policy reading, not a climate-aligned one (the drawer shows the World NZE ratio as an
  explicitly labelled alternative).
- **Canada** accelerates only Gas: a hydro-dominated grid with small renewable targets relative to its fleet and no
  fuel imports to displace. Plausible, but it makes the Canada strategy look manufacturing-only.
- **Fossil Accelerate** (user decision: assessed alike): Gas 62, Coal 41, Oil 32 cells, e.g. India Coal, Gas and Oil;
  Japan Gas; US Gas; Australia Oil; Canada Gas. These follow from demand growth and firm-capacity need under current
  policy.
- Japan's storage and grid (0.55 / 0.52) and the US grid (0.58) fall just short of the line; Germany Wind (0.59)
  too. The deployment threshold of 0.55 would add 155 Accelerate cells (section 2).

## 7. Secure validation (spec XXI.5)

174 Secure cells; 163 of the 2,326 high-need producing cells (7%) are Secure, so Secure does not absorb high need;
11 Secure cells have need below the threshold (the engine's operational / chokepoint evidence carries them).

| rationale | Extraction | Refining | Components | Assembly | total |
|---|---|---|---|---|---|
| systemic resilience | 68 | 57 | 3 | 23 | 151 |
| coercive chokepoint | 3 | 15 | - | - | 18 |
| operational control | - | 5 | - | - | 5 |

By node: Gas Refining 39, Oil Extraction 35, Coal Extraction 24, Electric Grid Assembly (transformers and switchgear)
23, Oil Refining 18, EV-minerals Refining (rare earths and battery metals) 15 coercive chokepoints + 2 operational
control, Gas Extraction 9 + 3, Nuclear Refining 3 operational control, Nuclear Components 3. Delivery: allied 72,
mixed 60, diversified global 42 (Secure is not reshoring). In the review economies the EV-minerals refining chokepoint
appears for the US, Japan, Korea, India, Germany, UK, Canada and Australia; the other Secure nodes are fossil-fuel
systemic resilience (Japan, Korea, France, Germany, UK oil or gas) and nuclear fuel (Korea, Brazil operational
control). Systemic resilience is 87% of Secure and mostly fossil supply: correct for import-dependent economies, but it
means the "transformers, power electronics, battery materials" resilience examples of the spec are thin (only Grid
Assembly, 23 cells).

## 8. Watch validation (spec XXI.6) and policy alignment

994 of 4,803 Watch cells carry top-third policy intensity (priority, protection or deployment localisation;
`watch_high_policy_intensity.csv`); 375 of them are judged over-prioritized, on three bases:

| basis | over-prioritized Watch nodes | alignment confidence |
|---|---|---|
| NIPO protection in the top third (producing stages, `watch_protection`) | 103 | high 101, medium 2 |
| the equipment's local-content protection (Deployment, `watch_localisation_deployment`) | 65 | medium (PAMS-only cap) |
| total policy attention in the top third, where NIPO does not observe the node at all (producing stages, `watch_attention`, the fallback) | 207 | low, always |

So 168 rest on observed protection or localisation and 207 (55%) on the total-attention fallback, which design D5
allows only where NIPO has no protection record either way and which always carries low confidence. Where NIPO
observes a node, protection alone decides: until this run 100 Watch nodes whose NIPO protection stock is recorded (98
of them at zero, e.g. Bulgaria Electric Grid Refining) were called over-prioritized on total attention because their
economy protects too few nodes to rank; they are now aligned (73) or mixed (27). The rest of the 994: 537 aligned
(mostly Deployment Watch with heavy PAMS deployment support but no localisation: support is not over-prioritization),
33 insufficient data (an unsettled Watch), 49 mixed. Review economies: Brazil, Canada and the US 14 each, UK 13,
France 11, Australia 10. Examples, all on observed protection: India, Korea, Germany, UK and Canada Electric Grid
equipment (protection 0.78-0.99 on a low-need, low-prize node); US Nuclear Assembly and Deployment (protection and
local content 0.78, deployment need 0.60, just below the line); Australia Solar Components (protection 0.91, weak
domestic case); France Batteries and Wind Deployment (local content 0.96 / 0.99 with deployment need 0.39 / 0.34).

Policy alignment over all cells (`choice_policy_alignment.csv`): aligned 5,185; under-prioritized 947 (Catch Up 828,
Secure 92, Preserve 27); deployment gap 182; over-prioritized 375 (all Watch: 103 NIPO protection, 65 local content,
207 total-attention fallback); misaligned 68 (Partner with protection in the top third, e.g.
Australia Solar and Wind Assembly, US Nuclear Refining, India Coal Refining); mixed 234; insufficient data 7,133
(5,295 not assessed). Alignment confidence: high 1,893, medium 1,263, low 10,968: NIPO's support / protection split
covers few cells, so most judgements rest on one source (an observed zero protection stock now counts as a
measurement, not a gap, which moved 808 judgements up one level). Secure under-prioritized in the review economies: EV-minerals
refining in Germany, India and Korea; oil, gas or coal Extraction in Japan, France, Germany and the UK; Korea gas
processing and nuclear fuel.
Deployment gaps: Japan, Korea Solar; UK Wind and Grid; Australia Solar, Wind, Grid.

Allied strategy: Partner cells with an allied solution in the AIB division 858, gap 734 (insufficient allied capacity
697, single credible supplier 269, weak evidence 231, excessive concentration 66, no trusted supplier 30; 30 Partner
cells have no credible partner at all). Secure cells that rely on foreign capacity: 56 covered, 118 gap.

## 9. Things that look wrong or fragile

1. **Catch Up is too easy** (1,660; the adjacent-stage leg alone carries 748). Two legs would keep 748; dropping the
   adjacency leg 912. A user decision (design D3 says "any of"), not tuned here.
2. **The prize is mostly node context** (82% country-invariant world demand growth) and mixes a stock (WEO installed
   capacity for manufacturing nodes) with a flow (mineral demand for Extraction / Refining): Solar Refining (polysilicon)
   0.20 vs Solar Components 0.85. Next refinement: WEO capacity additions for manufacturing nodes.
3. **The prize threshold is a knife-edge** (0.60 -> 0.65 halves Catch Up).
4. **Mineral-node supply shares**: the engine brackets an unlisted economy's share at the listing's residual. Until
   this run, that residual (0.87% in Batteries Refining, 1.9% in Green Hydrogen Extraction) made every unlisted economy
   a "contender", so 28 economies (US, Japan, Korea, Germany, UK, France, Canada, India among them) were "Preserve" in
   battery-materials refining. Fixed 2026-10-29 (`sp_measured_share()`: the contender test, the consistency counts and
   the bundle's share use the measured share or its lower bound); Batteries Refining now has 8 contenders and Preserves
   only China and Indonesia. The underlying weighted-mineral share of partially listed economies (Indonesia 0.75%
   lower bound in battery refining) remains rough.
5. **Not assessed** covers 37% of cells and is the same 20 nodes everywhere (no supply-chain need measured for
   Geothermal, Green Hydrogen, most Recovery and three Extraction nodes).
6. **Deployment need** has no national grid, EV, hydrogen or nuclear-target data; nuclear restarts inflate Japan's
   WEO-based scenario additions; demand is national only for six economies.
7. **Systemic-resilience Secure is mostly fossil fuel**; the equipment-resilience route reaches only Grid Assembly.
8. **Fossil Accelerate** (135 cells) is a consequence of the user's "assess fossil alike" decision and should be
   presented with that framing.
9. **Competitive Position is not measured at Recovery & Recycling** (no supply share and no Competitive Position family
   exists for recycling). The 406 assessed Recovery nodes (Catch Up 130, Partner 49, Watch 227) are decided with the
   position read as not strong, so no Recovery node can be Preserve. Since the 91dba6f rerun an unmeasured position
   lowers confidence wherever the rule reads it (Recovery Catch Up 120 moderate and 10 low, Partner 41 moderate and 8
   low; only Watch on low need and a low prize stays high, 172) and the reason says "Competitive Position not
   measured" (237 nodes, 233 of them Recovery). Before, 207 such nodes had a reason claiming a position "below the top
   quarter", 185 of them at high confidence. A recycling capability measure (secondary output, scrap processing capacity) is
   the fix.
