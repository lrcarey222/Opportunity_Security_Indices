# Strategic Position: readout of the canonical decision framework (2026-10-30 run)

Method: [strategic_position.md](strategic_position.md). Plan and audit: [decision_framework_plan.md](decision_framework_plan.md).
Final report draft: [decision_framework_report.md](decision_framework_report.md).

Every figure below comes from `diagnostics/strategic_position/` as written by
`scripts/95_build_deployment_need.R` on 2026-10-29 (Ember yearly data and 2030 targets, WEO 2025, GEVO 2026) and by
`scripts/26_build_strategic_position.R` (run with `SP_BASELINE_TBL` = the 2026-10-28 table, so the old-vs-new files
are written) and `scripts/27_review_strategic_position.R` (`decision_review.md`, `choice_cards.md`,
`legacy_osi_corners_vs_choice_*.csv`), rerun on 2026-10-30 on commit 7d847ba: the Catch Up pathway now needs at least
two of its five evidence legs (user decision 2026-10-30; no input data changed). The run before, on commit 91dba6f
(the review fixes: an unmeasured Competitive Position lowers confidence, a top-quarter score that fails the Preserve
test says why, and over-prioritization rests on observed NIPO protection; the choices did not change), had one leg;
its figures are quoted below as "under the one-leg rule" where they help. The Explorer's posture bundle
(`dashboard/public/data/posture`, schema 2, `git_commit` 7d847ba) was exported from the same table by `scripts/91`.
The readouts of the earlier posture engine (2026-10-27 run, 2026-10-28 trade refresh and need x EO quadrant) are in
git history (this file at commit 1bd7bde).

Thresholds (config `decision_framework`): supply-chain need 0.67, industrial opportunity (prize) 0.6, Competitive
Position in the top quarter of the node's contenders (0.75) with the capability floor, deployment need 0.6,
borderline band 0.05.

## 1. The choices

14,124 country x technology x six-stage cells (214 economies x 11 technologies x 6 stages):

| Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|
| 174 | 86 | 748 | 1,850 | 514 | 5,457 | 5,295 |

By stage (Secure / Preserve / Catch Up / Partner / Accelerate / Watch / not assessed):

| stage | S | P | CU | Pa | A | W | n.a. |
|---|---|---|---|---|---|---|---|
| Extraction | 71 | 8 | 32 | 308 | - | 800 | 1,135 |
| Refining & Processing | 77 | 12 | 153 | 458 | - | 1,135 | 519 |
| Component Manufacturing | 3 | 32 | 269 | 504 | - | 1,035 | 511 |
| Final Assembly & Equipment | 23 | 34 | 243 | 511 | - | 1,032 | 511 |
| Deployment & Use | - | - | - | - | 514 | 1,169 | 671 |
| Recovery & Recycling | 0 | 0 | 51 | 69 | - | 286 | 1,948 |

Review economies (of 66 nodes each):

| economy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| United States | 1 | 9 | 4 | 5 | 2 | 25 | 20 |
| Japan | 4 | 1 | 10 | 10 | 5 | 16 | 20 |
| South Korea | 5 | 6 | 10 | 8 | 6 | 11 | 20 |
| India | 1 | 6 | 11 | 7 | 9 | 12 | 20 |
| Germany | 3 | 3 | 8 | 8 | 4 | 20 | 20 |
| United Kingdom | 2 | 0 | 10 | 13 | 3 | 18 | 20 |
| France | 2 | 5 | 8 | 7 | 2 | 22 | 20 |
| Australia | 1 | 1 | 3 | 17 | 5 | 19 | 20 |
| Canada | 1 | 1 | 19 | 7 | 1 | 17 | 20 |
| China | 3 | 16 | 3 | 2 | 8 | 14 | 20 |
| Brazil | 2 | 1 | 4 | 6 | 3 | 30 | 20 |
| Italy | 2 | 1 | 11 | 9 | 6 | 17 | 20 |
| Mexico | 0 | 0 | 11 | 19 | 4 | 12 | 20 |
| Indonesia | 2 | 5 | 10 | 12 | 5 | 12 | 20 |

All 214 economies: `choice_by_country.csv`. Across economies the median has 0 Secure, 0 Preserve, 1 Catch Up, 8
Partner, 1 Accelerate, 26.5 Watch and 23 not assessed; 83 economies have at least one Secure node, only 25 at least one
Preserve (China 16, United States 9, India and South Korea 6), 112 at least one Catch Up (204 under the one-leg rule),
129 at least one Accelerate. Of these fourteen economies the two-leg rule moved the United Kingdom (Solar Components to
Watch), Australia (Batteries Recovery, EV Components and EV Extraction to Partner), Brazil (Batteries Refining and Solar
Components to Watch), Mexico (Gas Refining to Watch, Solar Components to Partner) and Indonesia (Wind Components to
Partner): 9 of the 912 nodes that left Catch Up are in these fourteen economies.

What this shows:

- **Watch is the largest assessed answer** (5,457, 62% of assessed cells): the framework makes non-priorities
  explicit, as the spec asks. 1,169 of them are Deployment & Use nodes with deployment need below 0.6.
- **Preserve is rare** (86): it now needs a top-quarter position among the node's real contenders (economies with
  at least 0.5% of world supply) and the capability floor. Most economies have no such node.
- **Catch Up** (748) needs at least two of the five pathway legs since 2026-10-30 (section 2): 28% of the assessed
  high-prize cells without a high position (748 Catch Up, 603 Partner, 1,297 Watch); under the one-leg rule it was
  63% (1,660, 345, 643). It is still the weakest result: feasibility and learning fire together for large diversified
  economies, so their Catch Up lists did not change and lead with the same nodes in every country.
- **Not assessed is structural, not country-specific**: every review economy has exactly 20, the same 20 nodes,
  because those nodes have no measured supply-chain need for any economy: the whole Geothermal and Green Hydrogen
  chains (10), Recovery of Coal, Gas, Electric Grid, EVs, Nuclear, Solar and Wind, and Nuclear, Solar and Wind
  Extraction. No producing cell lacks a prize; all 4,624 producing not-assessed cells lack need. 671 Deployment
  cells lack deployment need (mostly Oil, Green Hydrogen and EVs in small economies).

## 2. Threshold sensitivity

Cells per choice when one threshold moves (`choice_threshold_sensitivity.csv`; base = the configured thresholds):

| parameter | value | Preserve | Catch Up | Partner | Accelerate | Watch | cells changed |
|---|---|---|---|---|---|---|---|
| base | | 86 | 748 | 1,850 | 514 | 5,457 | 0 |
| need | 0.60 | 95 | 748 | 2,237 | 514 | 5,061 | 396 |
| need | 0.75 | 81 | 748 | 1,368 | 514 | 5,944 | 487 |
| prize | 0.50 | 111 | 935 | 1,753 | 514 | 5,342 | 212 |
| prize | 0.55 | 96 | 796 | 1,831 | 514 | 5,418 | 58 |
| prize | 0.65 | 73 | 422 | 1,965 | 514 | 5,681 | 339 |
| prize | 0.70 | 71 | 380 | 1,987 | 514 | 5,703 | 383 |
| CP | 0.67 | 114 | 724 | 1,847 | 514 | 5,456 | 28 |
| CP | 0.85 | 52 | 774 | 1,857 | 514 | 5,458 | 34 |
| deployment | 0.55 | 86 | 748 | 1,850 | 669 | 5,302 | 155 |
| deployment | 0.67 | 86 | 748 | 1,850 | 372 | 5,599 | 142 |
| deployment | 0.75 | 86 | 748 | 1,850 | 245 | 5,726 | 269 |

Secure (174) does not move with these thresholds (it is the engine's override; its own rule sensitivity is in
`posture_threshold_sensitivity.csv`). The prize threshold is the knife-edge, smaller than under the one-leg rule:
between 0.60 and 0.65 Catch Up falls from 748 to 422 and 339 cells change (one leg: 1,660 -> 858, 815 changed),
because the node prize is dominated by one country-invariant percentile across 55 nodes, so whole nodes cross the line
together. The need threshold mostly trades Partner against Watch. The CP bar moves few cells.
No threshold was tuned to improve these numbers; the configured values are the design's, except deployment need 0.6
instead of the design's starting 0.67, chosen for selectivity: deployment need is a mean of up to three family
percentiles, narrower than one percentile, so 0.6 takes about the top 30% of assessed Deployment nodes (514 of 1,683,
31%), the selectivity of supply-chain need at 0.67 (a percentile), where 0.67 would take 22% (372). At 0.67 the United
States would have no Accelerate node (its highest deployment need is Batteries, 0.64), and Germany and France would keep
only Green Hydrogen (0.71 and 0.84; `deployment_need.csv`).

The credible-pathway rule was the other lever (not a threshold). It is decided: at least two of the five legs (user
decision 2026-10-30, commit 7d847ba; design D3 had "any of"):

| pathway rule | Catch Up cells kept |
|---|---|
| two legs or more (used since 2026-10-30) | 748 |
| any one of five legs (design D3, used until 2026-10-29) | 1,660 |
| one-leg era alternative, not adopted: any one leg except the adjacent-stage leg | 912 |

Under the one-leg rule the legs among Catch Up cells were adjacent stage 1,316, feasibility 565, learning 539, input
cost 397, investment pipeline 106, and the adjacent-stage leg (adjacent world share >= 5% or the adjacent stage among
the economy's stronger cells, within-country CP percentile >= 0.6) alone carried 748 of them. The 912 cells that left
Catch Up each had one leg (adjacent stage 748, input cost 152, feasibility 7, learning 4, pipeline 1); 258 went to
Partner (high need) and 654 to Watch, and no other cell changed choice. Among the 748 Catch Up cells now: adjacent
stage 568, feasibility 558, learning 535, input cost 245, investment pipeline 105; feasibility and learning fire
together in 508. A cell with one leg and other evidence observed has `pathway_basis` "<leg>;too_few_legs"; the reason
reads "only one piece of pathway evidence (adjacent stage) where two are required" (section 9, item 1).

## 3. Legacy vs new (spec XXI.1)

**Legacy need x EO quadrant** (the live posture until 2026-10-28) against the canonical choice, all classified cells:

| legacy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| preserve | 5 | 49 | 59 | 12 | 75 | 235 | 3 |
| catch_up | 11 | 17 | 53 | 45 | 39 | 34 | 7 |
| partner | 113 | 5 | 136 | 1,057 | 62 | 401 | 27 |
| watch | 26 | 11 | 329 | 215 | 121 | 2,578 | 31 |
| insufficient data (shown as Deploy) | 19 | 4 | 171 | 521 | 217 | 2,209 | 5,227 |

(Rows are the legacy quadrant value on every cell, including the Deployment rows where it was computed for
information only, so the row totals exceed the legacy distribution of `decision_review.md`; `legacy_quadrant_vs_choice.csv`.) The largest transitions and their first explaining driver
(`decision_review.md`): insufficient data -> not assessed 4,624 (now shown as not assessed instead of Deploy);
insufficient data -> Watch 1,553, -> Partner 413 and -> Catch Up 146 (Competitive Position and the pathway now decide
cells the legacy EO could not rank); Deployment -> Watch 1,169, -> not assessed 671, -> Accelerate 514 (deployment
need); watch -> Catch Up 329 and partner -> Catch Up 136 (the cleaned prize); partner -> Watch 220 and watch -> Partner
174 (the cleaned need); insufficient data -> Watch 138 (the cleaned need); preserve -> Watch 119 (the cleaned prize).
Drivers over all changed cells: not assessed 4,624, deployment need 2,354, Competitive Position and pathway 2,227,
prize 704, need 691.

**Legacy OSI corners** (Energy Security x Economic Opportunity, the Country fact sheets' `legacy_corner`, schema 3;
6,351 placed cells) against the choice, with the conceptual mapping:

| legacy corner | maps to | cells | agree | agreement | to Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Grow | Preserve | 2,131 | 55 | 3% | 20 | 55 | 155 | 243 | 156 | 1,048 | 454 |
| Upgrade | Catch Up | 1,108 | 68 | 6% | 41 | 12 | 68 | 293 | 40 | 319 | 335 |
| Exposed | Partner | 2,018 | 522 | 26% | 71 | 6 | 213 | 522 | 81 | 779 | 346 |
| Stable | Watch | 1,094 | 485 | 44% | 11 | 7 | 60 | 120 | 32 | 485 | 379 |

Disagreements attributed (first that applies): prize 1,822, not assessed 1,368, deployment need 1,053, supply-chain
need 612, Competitive Position 223, Secure override 143. The corners agree poorly with the choice, and they should:

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
| Partner with CP in the contenders' top quarter | 8 | 3 |
| Partner with cross-country CP >= 0.75 | 363 of 1,456 (25%) | 396 of 1,850 (21%) |
| Catch Up with 10% or more of world supply | 8 | 6 |
| Catch Up with CP in the contenders' top decile | 10 | 2 |
| Catch Up with cross-country CP >= 0.9 | 49 of 117 (42%) | 173 of 748 (23%) |
| Preserve without capability (cross-country CP < 0.25 or < 0.5% of world supply) | 66 | 0 |
| Preserve below the contenders' median | 43 | 0 |

The contender-level contradictions fall sharply and Preserve-without-capability is gone by construction. The
cross-country counts do not fall in number, because far more cells are now decided (Catch Up 117 -> 748); as rates
they fall (Catch Up 42% -> 23%; Partner 25% -> 21%). Under the one-leg rule the Catch Up rate was 12% (207 of 1,660)
and the Partner rate 23% (361 of 1,592): only 34 of the 912 cells that left Catch Up had cross-country CP >= 0.9, so the
remaining Catch Up set is stronger on average and its rate rises. A cross-country top-decile position is not a leading
one in a node with 30-plus contenders, so most of the 173 are mid-ranked contenders in crowded nodes; the 396 Partner
cells with cross-country CP >= 0.75 are mostly the same story in Extraction and Refining (253 of them; 322 have high
need and a low prize: large producers' neighbours).

Remaining flags (kept, not silently overridden):

- `review_strong_position_low_prize` (16, all Preserve, design D2): strong position, high need, low prize. Mostly
  fossil and nuclear incumbents: Russia Nuclear Refining and Components, Russia, Canada and United States Oil
  Extraction, UAE Gas and Oil Refining, Indonesia Coal Refining, France Nuclear Refining, Japan Coal Components,
  India Solar Refining, Viet Nam Coal, Grid and Nuclear Assembly.
- `review_top_quarter_below_floor` (8: 5 Catch Up, 3 Partner; 11 under the one-leg rule, 9 and 2): a top-quarter
  contender score that fails the capability floor. Solar Components in India, Viet Nam, Thailand and France (1-2% world
  shares in a node China dominates) and Cambodia Solar Assembly (Catch Up); Russia Gas Extraction (Partner: 9.6% of
  world supply, just under the engine's 10% "strong" line, with no complementary capability recorded, so the floor
  reads "none"); Nigeria Gas Refining (Partner); Australia EV-minerals Extraction (Partner since the two-leg rule:
  contender score 0.93, supply share not quantified, the adjacent stage its only leg). EV-minerals Extraction in Russia
  and the Philippines and Saudi Arabia Gas Assembly also had only the adjacent-stage leg and are now Watch, which does
  not carry the flag.
- Not a flag, but now named: a top-quarter score held by an economy below the contender line (under 0.5% of world
  supply) is not a strong position either. Its trace says `not_a_contender` and its reason "a top-quarter score
  without a contender share" (32 nodes: 11 Catch Up, 3 Partner, 18 Watch; e.g. Canada Batteries Assembly 0.88 and
  India Batteries Assembly 0.75, each with 0.13% of world supply). Until the 91dba6f rerun their reason said "below
  the top quarter" and their trace "capability floor not met".

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
  385 technology pairs accelerate deployment while neither Components nor Assembly is Preserve or Catch Up, and 173 the
  reverse (deployment Watch, manufacturing Preserve or Catch Up). Review examples: Australia Solar and Wind (Accelerate;
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

1,157 of 5,457 Watch cells carry top-third policy intensity (priority, protection or deployment localisation;
`watch_high_policy_intensity.csv`); 503 of them are judged over-prioritized, on three bases:

| basis | over-prioritized Watch nodes | alignment confidence |
|---|---|---|
| NIPO protection in the top third (producing stages, `watch_protection`) | 122 | high 120, medium 2 |
| the equipment's local-content protection (Deployment, `watch_localisation_deployment`) | 65 | medium (PAMS-only cap) |
| total policy attention in the top third, where NIPO does not observe the node at all (producing stages, `watch_attention`, the fallback) | 316 | low, always |

So 187 rest on observed protection or localisation and 316 (63%) on the total-attention fallback, which design D5
allows only where NIPO has no protection record either way and which always carries low confidence. Under the one-leg
rule it was 375 (103 / 65 / 207): 126 of the 654 cells that left Catch Up for Watch keep their observed policy
attention and are now over-prioritized (19 on NIPO protection, 107 on the fallback), and Libya Gas Extraction and
Refining are over-prioritized because Catch Up is no longer among their possible choices. Where NIPO
observes a node, protection alone decides: until the 91dba6f rerun 100 Watch nodes whose NIPO protection stock is
recorded (98 of them at zero, e.g. Bulgaria Electric Grid Refining) were called over-prioritized on total attention
because their economy protects too few nodes to rank; they are now aligned (73) or mixed (27). The rest of the 1,157:
563 aligned (mostly Deployment Watch with heavy PAMS deployment support but no localisation: support is not
over-prioritization), 41 insufficient data (an unsettled Watch), 50 mixed. Review economies: Brazil 15; Canada, the UK
and the US 14 each; France 11; Australia and Mexico 10. Examples, all on observed protection: India, Korea, Germany,
UK and Canada Electric Grid equipment (protection 0.78-0.99 on a low-need, low-prize node); US Nuclear Assembly and Deployment (protection and
local content 0.78, deployment need 0.60, just below the line); Australia Solar Components (protection 0.91, weak
domestic case); France Batteries and Wind Deployment (local content 0.96 / 0.99 with deployment need 0.39 / 0.34).

Policy alignment over all cells (`choice_policy_alignment.csv`): aligned 5,486; under-prioritized 516 (Catch Up 397,
Secure 92, Preserve 27); deployment gap 182; over-prioritized 503 (all Watch: 122 NIPO protection, 65 local content,
316 total-attention fallback); misaligned 76 (Partner with protection in the top third, e.g.
Australia Solar and Wind Assembly, US Nuclear Refining, India Coal Refining; 8 are cells that left Catch Up, e.g.
Australia EV Components); mixed 220; insufficient data 7,141 (5,295 not assessed). Under-prioritized fell from 947
under the one-leg rule because 431 of the cells that left Catch Up had been under-prioritized Catch Up. Alignment
confidence: high 1,928, medium 1,295, low 10,901: NIPO's support / protection split
covers few cells, so most judgements rest on one source (an observed zero protection stock now counts as a
measurement, not a gap, which moved 808 judgements up one level). Secure under-prioritized in the review economies: EV-minerals
refining in Germany, India and Korea; oil, gas or coal Extraction in Japan, France, Germany and the UK; Korea gas
processing and nuclear fuel.
Deployment gaps: Japan, Korea Solar; UK Wind and Grid; Australia Solar, Wind, Grid.

Allied strategy: Partner cells with an allied solution in the AIB division 1,003, gap 847 (insufficient allied
capacity 802, single credible supplier 321, weak evidence 233, excessive concentration 89, no trusted supplier 30; 30
Partner cells have no credible partner at all). Of the 258 new Partner cells, 145 are covered and 113 gap. Secure
cells that rely on foreign capacity: 56 covered, 118 gap.

## 9. Things that look wrong or fragile

1. **Catch Up was too easy; decided 2026-10-30.** Under design D3's "any of" rule the adjacent-stage leg alone carried
   748 of 1,660 Catch Up cells. The user chose two legs (commit 7d847ba): Catch Up 1,660 -> 748, with 258 cells to
   Partner and 654 to Watch (section 2); the alternative of any one leg except the adjacency leg (912) was not adopted.
   What remains: the large economies barely moved, because feasibility and learning fire together for big diversified
   economies, so Catch Up is now feasibility-plus-learning driven and every country's Catch Up list still leads with the
   same high-prize nodes; Batteries Refining is still the most common Catch Up node (75 economies, 171 before).
2. **The prize is mostly node context** (82% country-invariant world demand growth) and mixes a stock (WEO installed
   capacity for manufacturing nodes) with a flow (mineral demand for Extraction / Refining): Solar Refining (polysilicon)
   0.20 vs Solar Components 0.85. Next refinement: WEO capacity additions for manufacturing nodes.
3. **The prize threshold is a knife-edge** (0.60 -> 0.65 cuts Catch Up from 748 to 422 and changes 339 cells; under
   the one-leg rule it halved Catch Up and changed 815).
4. **Mineral-node supply shares**: the engine brackets an unlisted economy's share at the listing's residual. Until
   the 2026-10-29 run, that residual (0.87% in Batteries Refining, 1.9% in Green Hydrogen Extraction) made every
   unlisted economy a "contender", so 28 economies (US, Japan, Korea, Germany, UK, France, Canada, India among them) were "Preserve" in
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
   exists for recycling). The 406 assessed Recovery nodes (Catch Up 51, Partner 69, Watch 286; 130, 49 and 227 under
   the one-leg rule) are decided with the position read as not strong, so no Recovery node can be Preserve. Since the
   91dba6f rerun an unmeasured position lowers confidence wherever the rule reads it (Recovery Catch Up 50 moderate and
   1 low, Partner 57 moderate and 12 low; only Watch on low need and a low prize stays high, 172) and the reason
   says "Competitive Position not measured" (237 nodes, 233 of them Recovery). Before, 207 such nodes had a reason claiming a position "below the top
   quarter", 185 of them at high confidence. A recycling capability measure (secondary output, scrap processing capacity) is
   the fix.
