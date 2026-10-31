# Strategic Position: readout of the canonical decision framework (2026-10-30 rerun)

Method: [strategic_position.md](strategic_position.md). Plan and audit: [decision_framework_plan.md](decision_framework_plan.md).
Final report: [decision_framework_report.md](decision_framework_report.md) (sections 20-29 describe each change since
the two-leg pathway; section 28 has the paper-reproduction table).

Every figure below comes from `diagnostics/strategic_position/` and `strategic_position_tbl.rds` as written by the
2026-10-30 pipeline rerun committed as e41cb5b: the Atlas of Economic Complexity 2024 release, the basket change
(quartz and silver ores at Solar Extraction, yellowcake at Nuclear Extraction), the dyads and energy trade combined
with the electro-industrial dictionary pull, the re-pulled 2022-24 trade benchmark and monthly momentum, then
`scripts/26_build_strategic_position.R` and `scripts/27_review_strategic_position.R` on commit a54a9d3 (the
capability-anchor Catch Up rule, the 0.10 need floor, fences by yard, Strategic Industrial Value, the large-yard case,
layer-first yards). Deployment need is the `scripts/95_build_deployment_need.R` output of 2026-10-29 (Ember yearly
data and 2030 targets, WEO 2025, GEVO 2026), not rebuilt since. The Explorer's posture bundle
(`dashboard/public/data/posture`, schema 2, `git_commit` a54a9d3) was exported from the same table by `scripts/91`.

Figures from earlier runs are labelled: **two-leg run** = the 2026-10-30 rerun of 7d847ba (two pathway legs, the
market prize, no large yard, the pre-Atlas-2024 data; this file at a442484 is its full readout); **one-leg rule** =
the 2026-10-29 run of 91dba6f; **old data** = the inputs of those runs. The readouts of the earlier posture engine
(2026-10-27 run, 2026-10-28 trade refresh and need x EO quadrant) are in git history (this file at commit 1bd7bde).

Thresholds and rules (config `decision_framework`): supply-chain need 0.67, with a need score of at least 0.10;
prize = strategic industrial value 0.6, never high where capped as market-only; Competitive Position in the top
quarter of the node's contenders (0.75) with the capability floor; deployment need 0.6; borderline band 0.05; the
large-yard case = mature and passive and at least 5 of 8 commodity signals, no credible leap and no strong learning,
read after a top-quarter position; Catch Up = a capability anchor plus corroboration from a different evidence family.

## 1. The choices

14,124 country x technology x six-stage cells (214 economies x 11 technologies x 6 stages):

| | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| current run | 206 | 103 | 355 | 1,474 | 514 | 6,585 | 4,887 |
| two-leg run | 174 | 86 | 748 | 1,850 | 514 | 5,457 | 5,295 |

By stage (current run):

| stage | S | P | CU | Pa | A | W | n.a. |
|---|---|---|---|---|---|---|---|
| Extraction | 98 | 4 | 0 | 114 | - | 1,411 | 727 |
| Refining & Processing | 81 | 10 | 71 | 175 | - | 1,497 | 520 |
| Component Manufacturing | 3 | 34 | 109 | 555 | - | 1,142 | 511 |
| Final Assembly & Equipment | 24 | 55 | 154 | 554 | - | 1,056 | 511 |
| Deployment & Use | - | - | - | - | 514 | 1,169 | 671 |
| Recovery & Recycling | 0 | 0 | 21 | 76 | - | 310 | 1,947 |

Review economies (of 66 nodes each), current run, with the yard roll-up of their cells (small / medium / large /
mixed; not assessed excluded):

| economy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed | yards S / M / L / mixed |
|---|---|---|---|---|---|---|---|---|
| United States | 1 | 8 | 8 | 4 | 2 | 25 | 18 | 1 / 13 / 22 / 12 |
| Japan | 4 | 2 | 12 | 3 | 5 | 22 | 18 | 4 / 13 / 18 / 13 |
| South Korea | 5 | 9 | 8 | 5 | 6 | 15 | 18 | 5 / 17 / 12 / 14 |
| India | 1 | 8 | 9 | 5 | 9 | 16 | 18 | 1 / 22 / 15 / 10 |
| Germany | 4 | 6 | 10 | 3 | 4 | 21 | 18 | 4 / 14 / 18 / 12 |
| United Kingdom | 3 | 0 | 12 | 7 | 3 | 23 | 18 | 3 / 15 / 20 / 10 |
| France | 2 | 5 | 10 | 3 | 2 | 26 | 18 | 2 / 13 / 23 / 10 |
| Australia | 1 | 1 | 3 | 10 | 5 | 28 | 18 | 1 / 12 / 23 / 12 |
| Canada | 1 | 2 | 15 | 6 | 1 | 23 | 18 | 1 / 17 / 20 / 10 |
| China | 3 | 16 | 2 | 0 | 8 | 19 | 18 | 3 / 19 / 16 / 10 |
| Brazil | 2 | 0 | 2 | 4 | 3 | 37 | 18 | 2 / 4 / 33 / 9 |
| Italy | 2 | 4 | 10 | 3 | 6 | 23 | 18 | 2 / 16 / 19 / 11 |
| Mexico | 0 | 0 | 12 | 7 | 4 | 25 | 18 | 0 / 15 / 23 / 10 |
| Indonesia | 2 | 4 | 7 | 9 | 5 | 21 | 18 | 2 / 18 / 18 / 10 |

All 214 economies: `choice_by_country.csv`. Across economies the median has 0 Secure, 0 Preserve, 0 Catch Up, 6
Partner, 1 Accelerate, 32 Watch and 21 not assessed; 97 economies have at least one Secure node, 31 at least one
Preserve (China 16, South Korea 9, the United States and India 8), 64 at least one Catch Up (112 on the two-leg run,
204 under the one-leg rule), 129 at least one Accelerate.

What this shows:

- **Watch is the largest assessed answer** (6,585, 71% of the 9,237 assessed cells): the framework makes
  non-priorities explicit, as the spec asks. 1,169 of them are Deployment & Use nodes with deployment need below 0.6;
  2,133 are producing-stage cells decided by the large-yard case (mature, passive, commodity nodes: Extraction and
  Refining of coal, gas, oil, grid metals, solar and nuclear minerals), 1,253 of those with a capability floor.
- **Preserve is rare** (103): it needs a top-quarter position among the node's real contenders (economies with at
  least 0.5% of world supply) and the capability floor. Most economies have no such node.
- **Catch Up** (355) needs a capability anchor plus corroboration since 2026-10-30 (section 2). It shifted from
  mineral extraction and solar components toward grid and nuclear equipment, whose industrial commons lift their
  strategic industrial value: the United States has 8 Catch Up nodes (4 on the two-leg run), including Electric Grid,
  Gas, Oil and Nuclear Assembly. Battery refining is Catch Up in all nine review economies of the report and battery recycling in eight.
- **Partner fell** (1,850 -> 1,474) because the large-yard case sends commodity Partner nodes to Watch: high import
  need on a mature, diversified commodity is a diversification question, not a domestic industrial one.
- **Not assessed is structural, not country-specific**: every review economy has exactly 18, the same 18 nodes,
  because those nodes have no measured supply-chain need for any economy: the whole Geothermal and Green Hydrogen
  chains (10), Wind Extraction (monazite trades too thinly), and Recovery of Coal, Gas, Electric Grid, EVs, Nuclear,
  Solar and Wind. Solar and Nuclear Extraction are assessed since the basket change (need measured for 204 and 203
  economies; 20 such nodes before). 4,216 producing cells and 671 Deployment cells (mostly Oil, Green Hydrogen and EVs
  in small economies) are not assessed.

## 2. Catch Up: the capability anchor

The rule (user spec 2026-10-30): a high prize, no strong position, at least one **capability anchor** (direct supply
or 2022-24 export share >= 0.5%; GCIM operating capacity >= 0.5% or committed pipeline >= 1% of world operating
capacity; an adjacent stage with >= 5% of world supply; at Extraction a resource base) and at least one more signal
from a **different evidence family** (another anchor; or feasibility, input cost, relative adjacency, learning
inputs, announced capacity >= 2%). Two enabling signals without an anchor are never enough.

Rule comparison on the current data (`catch_up_rule_comparison.csv`; Secure 206, Preserve 103, Accelerate 514 and not
assessed 4,887 in every row):

| pathway rule | Catch Up | Partner | Watch | cells changed vs one leg |
|---|---|---|---|---|
| any one of five legs (design D3) | 1,952 | 988 | 5,474 | 0 |
| two legs or more (the morning of 2026-10-30) | 928 | 1,268 | 6,218 | 1,024 |
| capability anchor + corroboration (used) | 355 | 1,474 | 6,585 | 1,597 |

Capability bands of the Catch Up cells (`catch_up_capability_bands.csv`; contender CP = the Competitive Position
percentile among the node's contenders, 0 for an economy below the contender line; cross-country CP = among all
economies):

| rule | Catch Up | contender CP = 0 | < 0.10 | < 0.25 | cross-country CP < 0.10 | < 0.25 | CP not measured | without an anchor |
|---|---|---|---|---|---|---|---|---|
| one leg | 1,952 | 1,175 | 1,369 | 1,503 | 70 | 273 | 161 | 1,595 |
| two legs | 928 | 372 | 485 | 605 | 6 | 36 | 56 | 591 |
| anchor | 355 | 41 | 75 | 132 | 0 | 0 | 21 | 0 |

Every Catch Up cell now has an anchor and none sits in the bottom quarter across economies. The 41 at contender CP 0
are below the contender line (under 0.5% of the node's measured world supply), so they cannot hold the direct-supply
anchor and hold another (export share, capacity, pipeline or an adjacent stage).

Anchors and supports (`catch_up_anchor_types.csv`): among the 355 Catch Up cells, direct export share 303, direct
supply share 256, adjacent stage share 54, operating capacity 43, committed pipeline 31, resource base 0 (no
Extraction cell is Catch Up: every Extraction stage row is capped as market-only, and most are large yards); supporting signals learning 323, feasibility 302,
relative adjacency 246, input cost 73, announced capacity 10. Of the 1,474 Partner cells 90 have an anchor (no
corroboration, or no high prize), and 792 carry `greenfield_option` (a high prize without a position or an anchor);
of the 6,585 Watch cells 1,737 do. Catch Up by stage: Refining 71, Components 109, Assembly 154, Recovery 21. Most
common Catch Up nodes: Batteries Refining 34, EV Components 27, Wind Components 27, Wind Assembly 27, Electric Grid
Assembly 25, Solar Assembly 24, Batteries Recovery 21, Batteries Components 20, EV Assembly 20, Nuclear Components 19,
Nuclear Assembly 18, EV Refining 17. Of the 2,902 assessed high-prize producing cells without a high position (not
Secure, not a large yard), 12% have a credible pathway (355), 28% go Partner (800) and 60% Watch (1,747).

On the old data (scratch comparison of 2026-10-30, after the measured-share fix of d65ae03): the anchor rule took
Catch Up from 748 to 357, Partner 1,850 -> 1,996, Watch 5,457 -> 5,702; 456 cells left Catch Up (164 to Partner, 292
to Watch, all `no_anchor`; their evidence was mostly feasibility + adjacent stage + learning, adjacent stage + input
cost, or feasibility + learning) and 65 joined (one-leg cells with an anchor, e.g. Chilean and Argentine
battery-minerals Extraction, Australian EV-minerals Extraction, DR Congo battery refining). Economies with any Catch
Up: 112 -> 70; Batteries Refining 75 -> 35. Before that fix, `home_share` gave every economy that a partial listing
omits the listing's residual, 0.87% of battery refining, which made Afghanistan, Andorra, Bermuda and 150 other small
economies Catch Up there on a direct-supply anchor.

## 3. Threshold and rule sensitivity

Cells per choice when one threshold moves (`choice_threshold_sensitivity.csv`; base = the configured thresholds;
Secure 206 and not assessed 4,887 in every row):

| parameter | value | Preserve | Catch Up | Partner | Accelerate | Watch | cells changed |
|---|---|---|---|---|---|---|---|
| base | | 103 | 355 | 1,474 | 514 | 6,585 | 0 |
| need | 0.60 | 112 | 355 | 1,771 | 514 | 6,279 | 306 |
| need | 0.75 | 99 | 355 | 1,124 | 514 | 6,939 | 354 |
| prize | 0.50 | 117 | 397 | 1,469 | 514 | 6,534 | 56 |
| prize | 0.55 | 113 | 375 | 1,471 | 514 | 6,558 | 30 |
| prize | 0.65 | 89 | 300 | 1,487 | 514 | 6,641 | 69 |
| prize | 0.70 | 84 | 291 | 1,491 | 514 | 6,651 | 83 |
| CP | 0.67 | 135 | 326 | 1,472 | 514 | 6,584 | 32 |
| CP | 0.85 | 63 | 393 | 1,474 | 514 | 6,587 | 40 |
| deployment | 0.55 | 103 | 355 | 1,474 | 669 | 6,430 | 155 |
| deployment | 0.67 | 103 | 355 | 1,474 | 372 | 6,727 | 142 |
| deployment | 0.75 | 103 | 355 | 1,474 | 245 | 6,854 | 269 |

Secure does not move with these thresholds (it is the engine's override; its own rule sensitivity is in
`posture_threshold_sensitivity.csv`). The prize threshold is no longer the knife-edge: 0.60 -> 0.65 changes 69 cells
(339 on the two-leg run, 815 under the one-leg rule), because the strategic industrial value spreads nodes more than
world demand growth alone, the market-only cap holds whole nodes below the line at any threshold, and the large-yard
case removes commodity nodes before the prize is read. The need threshold mostly trades Partner against Watch. The CP
bar moves few cells. No threshold was tuned; deployment need 0.6 (instead of the design's starting 0.67) was chosen
for selectivity: deployment need is a mean of up to three family percentiles, so 0.6 takes about the top 30% of
assessed Deployment nodes (514 of 1,683, 31%), the selectivity of supply-chain need at 0.67, where 0.67 would take 22%
(372). At 0.67 the United States would have no Accelerate node (its highest deployment need is Batteries, 0.64), and
Germany and France would keep only Green Hydrogen (0.71 and 0.84; `deployment_need.csv`).

The Yards & Fences settings (`large_yard_sensitivity.csv`; Secure 206, Accelerate 514, not assessed 4,887 throughout):

| variant | Preserve | Catch Up | Partner | Watch | large-yard cases | cells changed |
|---|---|---|---|---|---|---|
| `min_signals` 4 | 103 | 331 | 1,227 | 6,856 | 3,723 | 271 |
| `min_signals` 5 (used) | 103 | 355 | 1,474 | 6,585 | 2,730 | 0 |
| `min_signals` 6 | 103 | 355 | 1,633 | 6,426 | 1,971 | 159 |
| `position_last` | 98 | 355 | 1,474 | 6,590 | 2,730 | 5 |
| no learning veto | 103 | 355 | 1,474 | 6,585 | 2,730 | 0 |

(Large-yard cases count the cell rows the case holds for over all 219 economies in the table, assessed or not: 2,666
of them among the 14,124 classified cells.) The prize measure and
the large-yard case against the market prize (`strategic_value_vs_market.csv`, producing stages, the need floor
applied throughout):

| rule | Preserve | Catch Up | Partner | Watch | cells changed |
|---|---|---|---|---|---|
| market prize, no large yard | 86 | 357 | 2,035 | 4,870 | 0 |
| strategic industrial value only | 103 | 355 | 2,050 | 4,840 | 231 |
| market prize + large yard | 86 | 329 | 1,468 | 5,465 | 595 |
| both (used) | 103 | 355 | 1,474 | 5,416 | 798 |

The 798 moves (`strategic_value_vs_market_transitions.csv`): Partner -> Watch 567 (all by the large-yard case),
Watch -> Catch Up 73, Catch Up -> Watch 69, Catch Up -> Partner 30, Watch -> Preserve 26, Partner -> Catch Up 24,
Preserve -> Watch 9.

The need floor: 40 cells are capped (Nuclear Extraction 21, Nuclear Refining 18, EV-minerals Refining 1); re-running
the choice on the stored table without it changes 13 choices under the current method (Partner -> Watch 10, Preserve
-> Watch 3) and 36 under the market prize without the large yard (Partner -> Watch 32, Preserve -> Watch 4, all
nuclear).

## 4. Yards

Each choice maps to a yard (small = Secure; medium = Preserve, Catch Up, Partner, Accelerate; large = Watch), and each
cell's stage row and sensitive layers roll up to the cell (`yard_rollup_by_cell.csv`, `yard_layers.csv`):

| roll-up | cells |
|---|---|
| small | 181 |
| medium | 1,809 |
| large | 6,051 |
| mixed high / medium | 320 |
| mixed medium / low | 350 |
| mixed high / low | 362 |
| mixed high / medium / low | 164 |
| not assessed | 4,887 |

Mixed cells are where the layers carry the decision: Electric Grid Assembly (140 mixed: control layers Small,
transformers and power conversion Medium, cables Large), Batteries Assembly (88), Solar Assembly (155), EV Components
and Assembly (103 and 111), Wind Components and Assembly (66 and 107), Gas and Oil Assembly (73 and 86), Solar
Refining (82: polysilicon Medium, the rest Large). The high fence reaches hardware the stage row does not show
(`yard_fence_scope` `extends_to_hardware`) on 439 cells. Layers (35 per economy, 7,665 rows): small 1,106, medium
1,724, large 4,022, not assessed 813. The Small layers are the control layers (solar inverter firmware 109; grid
control, metering and utility inverter firmware 91 each; connected vehicles 91; wind-turbine control 85; oil and gas
OT control 80 and 61; battery control and communications 63 each; reactor controls 18), NdFeB magnets (EVs 103, wind
66), grain-oriented steel (37), transformers (24), rare-earth separation (17), fuel fabrication (11) and enrichment
(2-3), each where the engine finds an acute vulnerability for the economy.
Large-yard layers: battery enclosures, grid cables, PV racking and conventional PV modules (the last with a
capability floor).

Against the paper's placements (`yf_paper_reproduction.csv`; full table in the report, section 28): of 19 placed
components the model's yard agrees for Japan on 17, Germany on 12 and the United States on 10. The differences are
the control layers (Small only where coercive control is measured for the country: Japan, not the United States or
Germany; a user decision), LEU and HALEU enrichment (Catch Up, medium, where the paper has small), and battery
enclosures (US) and PV modules (US, Germany), Preserve where the cell holds a top-quarter position.

## 5. Legacy vs new (spec XXI.1)

**Legacy need x EO quadrant** (the live posture until 2026-10-28, recomputed on the current data) against the
canonical choice, all classified cells (`legacy_quadrant_vs_choice.csv`):

| legacy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| secure | 206 | - | - | - | - | - | - |
| preserve | - | 65 | 53 | 9 | - | 147 | - |
| catch_up | - | 10 | 36 | 54 | - | 15 | - |
| partner | - | 4 | 56 | 753 | - | 708 | - |
| watch | - | 22 | 185 | 150 | - | 2,596 | - |
| insufficient data (shown as Deploy) | - | 2 | 25 | 508 | - | 1,950 | 4,216 |
| Deployment (not decided) | - | - | - | - | 514 | 1,169 | 671 |

Drivers over the changed cells (`decision_review.md`, first that applies): not assessed 4,216, deployment need 2,354,
Competitive Position and the pathway 1,770, the large-yard case 1,131, the strategic industrial value 574,
supply-chain need 423. Largest transitions: insufficient data -> not assessed 4,216; insufficient data -> Watch 1,271
(Competitive Position) and 549 (large yard); Deployment -> Watch 1,169, -> not assessed 671, -> Accelerate 514;
partner -> Watch 529 (large yard) and 93 (need); insufficient data -> Partner 399 (Competitive Position) and 109
(need); watch -> Catch Up 185 (the strategic industrial value). (On the two-leg run the 2026-10-28 quadrant row
counts were computed on the old data; this file at a442484 has that table.)

**Legacy OSI corners** (Energy Security x Economic Opportunity, the Country fact sheets' `legacy_corner`, schema 3;
6,351 placed cells) against the choice, with the conceptual mapping:

| legacy corner | maps to | cells | agree | agreement | to Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Grow | Preserve | 2,131 | 78 | 4% | 25 | 78 | 151 | 175 | 156 | 1,188 | 358 |
| Upgrade | Catch Up | 1,108 | 42 | 4% | 39 | 8 | 42 | 195 | 40 | 486 | 298 |
| Exposed | Partner | 2,018 | 367 | 18% | 88 | 7 | 84 | 367 | 81 | 1,103 | 288 |
| Stable | Watch | 1,094 | 601 | 55% | 13 | 8 | 22 | 97 | 32 | 601 | 321 |

Disagreements attributed (first that applies): not assessed 1,119, strategic industrial value 1,072, deployment need
1,053, large-yard case 1,031, supply-chain need 449, Competitive Position 374, Secure override 165 (two-leg run:
agreement 3% / 6% / 26% / 44%). The corners agree poorly with the choice, and they should:

- A legacy corner is **relative to the economy's own median sector** (the 2026-10-15 rule): every economy has Grow
  sectors, by construction about a quarter of its placed sectors. Preserve is **absolute among contenders** (top
  quarter of the economies that hold the node). Grow -> Preserve agreement of 4% mostly reflects that difference.
- Legacy Energy Security mixes vulnerability with capability, reserves and deployment; legacy Economic Opportunity
  mixes market prize with current capability (export share, RCA, production, capacity), cost and investment. The new
  framework separates them, so cells move whenever the mixed index was carried by the "wrong" component.
- Exposed -> Partner agreement fell (26% -> 18%) and Stable -> Watch rose (44% -> 55%) because the large-yard case
  sends commodity nodes with high import need to Watch.
- Deployment & Use was never decided by the legacy corners in a deployment sense; 1,053 disagreements are
  deployment-need decisions.

## 6. Prize validation (spec XXI.2)

**The market prize** `industrial_opportunity` over the 12,045 producing cells: min 0.01, quartiles 0.35 / 0.54 /
0.67, max 0.97; 4,734 at or above 0.6 (unchanged: the demand series did not change).

| Spearman with | pooled | median within node | cells |
|---|---|---|---|
| Competitive Position percentile | 0.095 | 0.38 (37 nodes where the prize varies) | 7,992 |
| supply-chain need | 0.04 | 0.10 | 7,554 |
| legacy EO opportunity percentile | 0.26 | 0.21 | 7,327 |

Component weights: world demand growth (country-invariant) 82% of the market prize, global capex momentum 4%,
national home market 14% (where it lifts the prize; 3,350 cells). **Not a disguised capability score**: within a node
only the home market can move it, and there the correlation is 0.38 (large home markets are partly large producers).
The largest movers from legacy EO are Green Hydrogen and Batteries Recovery / Components / Assembly in economies
without capability (legacy EO near 0, market prize 0.94-0.97).

**The prize the choice reads** `strategic_industrial_value` over the 11,770 producing cells: quartiles 0.25 / 0.50 /
0.74; 3,044 of the 7,554 assessed producing cells (40%) are high. 39 producing nodes or layers are capped as
market-only in every economy (`strategic_value_families.csv`): the Extraction stage rows of all eleven technologies;
Coal, Gas and Geothermal Refining; Oil Components; Solar Components (its stage row's learning attributes are Unknown);
Recovery of everything but Batteries and Nuclear; 10 of the 12 producing-stage control layers (not HVDC control or
reactor controls); battery enclosures, grid cables, PV racking and hydrogen PGMs. The industrial-commons family is what moves equipment nodes (grid, nuclear,
gas and oil assembly) above the line, and the cap what holds commodity nodes below it.

## 7. Position-choice consistency (spec XXI.3)

Before = the 2026-10-28 need x legacy EO quadrant recomputed on the current data; after = the canonical choice; both
counted on the same Competitive Position and measured supply shares (`consistency_before_after.csv`; its "after"
label still reads 2026-10-29, the counts are the current run's):

| contradiction | before | after | after, two-leg run |
|---|---|---|---|
| Partner with CP in the contenders' top quarter | 10 | 3 | 3 |
| Partner with cross-country CP >= 0.75 | 415 of 1,521 (27%) | 275 of 1,474 (19%) | 396 of 1,850 (21%) |
| Catch Up with 10% or more of world supply | 11 | 5 | 6 |
| Catch Up with CP in the contenders' top decile | 10 | 0 | 2 |
| Catch Up with cross-country CP >= 0.9 | 50 of 115 (43%) | 152 of 355 (43%) | 173 of 748 (23%) |
| Preserve without capability (cross-country CP < 0.25 or < 0.5% of world supply) | 63 | 0 | 0 |
| Preserve below the contenders' median | 42 | 0 | 0 |

The contender-level contradictions fall sharply and Preserve-without-capability is gone by construction. The Catch Up
rate at cross-country CP >= 0.9 went back up (23% -> 43%): the anchor keeps the economies with a real base and drops
those without one, so the remaining Catch Up set sits high in the cross-country ranking while still below the top
quarter of its node's contenders. That is the rule's intent, but it reads as "Catch Up despite a strong position" and
is the weakest consistency result.

Remaining flags (kept, not silently overridden):

- `review_strong_position_low_prize` (8, all Preserve, design D2; 16 on the two-leg run): strong position, high
  need, low prize. Australia EV-minerals Extraction, Canada and UAE Oil Extraction, Chile Solar Extraction (silver
  ores), India Solar Refining, Japan Coal Components, Nigeria Gas Refining, Oman Oil Components.
- `review_top_quarter_below_floor` (4; 8 on the two-leg run): a top-quarter contender score that fails the capability
  floor. Brazil Wind Assembly (Catch Up); India, Thailand and Viet Nam Solar Components (Partner).
- Not a flag, but named: a top-quarter score held by an economy below the contender line has trace `not_a_contender`
  and the reason "a top-quarter score without a contender share" (25 cells: 2 Catch Up, 11 Partner, 12 Watch).

## 8. Deployment validation (spec XXI.4)

Deployment need was not rebuilt, so this section holds as on the two-leg run. `deployment_need` over 1,683 assessed
Deployment cells: quartiles 0.32 / 0.49 / 0.65; Accelerate 514, Watch 1,169, not assessed 671. Accelerate by
technology: Gas 62, Batteries 58, Electric Grid 56, Geothermal 50, Wind 50, Nuclear 49, Solar 48, Coal 41, Green
Hydrogen 35, EVs 33, Oil 32.

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
  data.
- *Does it recognise grid / storage needs?* Partly. Storage and grid read the renewables-target gap and the solar +
  wind share (flexibility); congestion and transmission adequacy are not assessed.
- *Does it avoid classifying high deployment need as Partner?* Yes, by construction: no Deployment node is Compete.
- *Can a technology be low manufacturing priority but high deployment priority?* Yes, often (current run,
  `manufacturing_vs_deployment.csv`, manufacturing = Components and Assembly): 410 economy x technology pairs
  accelerate deployment while neither manufacturing stage is Preserve or Catch Up (Partner 177, Watch 135, Secure 9,
  not assessed 89), 134 the reverse, 104 both. Review examples: Australia Solar, Wind, Storage and Grid (Accelerate;
  manufacturing Watch or Partner); the reverse: United States Wind and EVs (manufacturing Preserve; deployment need
  0.37 / 0.09), Canada Nuclear, EVs and Storage (manufacturing Catch Up or Preserve; deployment Watch).

What looks weak: United States EVs 0.09 and India EVs 0.30 (GEVO 2026 Current Policies; a current-policy reading,
with the climate-aligned ratio shown as a labelled alternative); Canada accelerates only Gas; fossil Accelerate (Gas
62, Coal 41, Oil 32: the user's "assess fossil alike" decision); Japan's storage and grid (0.55 / 0.52), the US grid
(0.58) and Germany Wind (0.59) fall just short of the line.

## 9. Secure validation (spec XXI.5)

206 Secure cells (174 on the two-leg run; the engine did not change, the data did); 191 of the 2,386 high-need
producing cells (8%) are Secure, so Secure does not absorb high need; 15 Secure cells have need below the threshold
(`secure_selectivity.csv`).

| rationale | Extraction | Refining | Components | Assembly | total |
|---|---|---|---|---|---|
| systemic resilience | 86 | 62 | 3 | 24 | 175 |
| coercive chokepoint | 12 | 15 | - | - | 27 |
| operational control | - | 4 | - | - | 4 |

By node (`secure_by_rationale.csv`): Oil Refining 42, Gas Extraction 41 (29 resilience, 12 chokepoint), Oil Extraction
32, Coal Extraction 25, Electric Grid Assembly 24, Gas Refining 19, EV-minerals Refining 17 (15 chokepoint, 2 dual
use), Nuclear Components 3, Nuclear Refining 3 (2 dual use, 1 resilience). Delivery: allied 104, mixed 60, diversified
global 42 (Secure is not reshoring). Systemic resilience is 85% of Secure and mostly fossil supply: correct for
import-dependent economies, but the "transformers, power electronics, battery materials" resilience examples of the
spec are thin (only Grid Assembly, 24 cells). Every Secure variant now has a high fence; the resilience fence guards
the essential supply itself (screening of foreign control, trusted long-term supply, stockholding), never import
protection.

## 10. Watch validation (spec XXI.6) and policy alignment

1,412 of 6,585 Watch cells carry top-third policy intensity (priority, protection or deployment localisation;
`watch_high_policy_intensity.csv`); 679 of them are judged over-prioritized, on three bases:

| basis | over-prioritized Watch cells | alignment confidence | two-leg run |
|---|---|---|---|
| NIPO protection in the top third (producing stages, `watch_protection`) | 151 | high 148, medium 3 | 122 |
| the equipment's local-content protection (Deployment, `watch_localisation_deployment`) | 64 | medium | 65 |
| total policy attention in the top third, where NIPO does not observe the cell (`watch_attention`, the fallback) | 464 | low, always | 316 |

So 215 rest on observed protection or localisation and 464 (68%) on the total-attention fallback, which design D5
allows only where NIPO has no protection record either way and which always carries low confidence. The rise from
503 follows the Watch count: the large-yard case sends commodity Extraction and Refining cells to Watch, and they
keep their observed policy attention. Examples on observed protection (`decision_review.md`): Germany Solar
Components (protection 0.99, need 0.19, a market-only prize); Korea and UK Oil Refining (protection 0.99, large
yard); US Solar Refining (0.96, large yard); Brazil Electric Grid Assembly (0.99, weak domestic case); Japan Solar
Components (0.94); at Deployment & Use France Wind and Batteries (local content 0.99 / 0.94, deployment need 0.34 /
0.39) and US Nuclear (0.81, deployment need 0.60, just below the line).

Policy alignment over all cells (`choice_policy_alignment.csv`): aligned 5,826; under-prioritized 310 (Catch Up 176,
Secure 104, Preserve 30); deployment gap 182; over-prioritized 679 (all Watch); misaligned 59 (Partner with
protection in the top third, all high confidence); mixed 233; insufficient data 6,835 (4,887 not assessed). Alignment
confidence: high 2,096, medium 1,350, low 10,678. Two-leg run: aligned 5,486, under-prioritized 516, over 503,
misaligned 76, mixed 220, insufficient 7,141.

Allied strategy (`partner_aib_status.csv`): Partner cells with an allied solution 692, gap 782 (reasons, several per
cell: insufficient allied capacity 737, single credible supplier 378, weak evidence 328, excessive concentration 299,
no trusted supplier 30); Secure cells that rely on foreign capacity: 110 covered, 96 gap. 30 Partner cells have no
credible partner at all (`partner_recs_sanity.csv`).

## 11. Things that look wrong or fragile

1. **Catch Up sits high in the cross-country ranking** (43% at 0.9 or above) and leans on the direct-export anchor
   (303 of 355): 0.5% of world exports is a low bar for a large diversified exporter, so the large economies keep
   long Catch Up lists (Canada 15, Japan and the UK 12). The anchor removed the economies with no base, not the
   breadth of the large ones.
2. **The strategic industrial value rests on curated attributes, Unknown counting 0.** 39 nodes or layers are
   market-only, and some for want of evidence rather than on evidence: the Solar Components stage row (learning
   attributes Unknown) reads Watch in most economies while its wafer-and-cell and next-generation layers are Medium.
3. **`min_signals` is the new lever**: 4 or 6 change 271 or 159 choices, and conventional PV modules hold the
   large-yard case on exactly 5 of 8 signals. Three of the eight signals are facets of one product-market property.
4. **Control layers are country-specific Small** (a user decision): grid SCADA, inverter firmware and reactor controls
   are Small for Japan, Large for the United States and Germany, where the paper places them Small everywhere.
5. **Need-only Partner is a Medium yard** by the user's mapping, where the paper keeps dependence inside the Large yard
   with diversification.
6. **The market prize is mostly node context** (82% country-invariant world demand growth) and mixes a stock (WEO
   installed capacity for manufacturing nodes) with a flow (mineral demand for Extraction / Refining): Solar Refining
   0.20 vs Solar Components 0.85. Next refinement: WEO capacity additions.
7. **Mineral-node supply shares**: the engine brackets an unlisted economy's share at the listing's residual; the
   contender test and the Catch Up anchors read the measured share (lower bound). The weighted-mineral share of
   partially listed economies remains rough.
8. **Not assessed** covers 35% of cells and is the same 18 nodes everywhere (Geothermal, Green Hydrogen, Wind
   Extraction and seven Recovery nodes); the new Solar and Nuclear Extraction baskets are measured but land in the
   large yard almost everywhere.
9. **Deployment need** has no national grid, EV, hydrogen or nuclear-target data; nuclear restarts inflate Japan's
   WEO-based scenario additions; demand is national only for six economies.
10. **Systemic-resilience Secure is mostly fossil fuel**; the equipment-resilience route reaches only Grid Assembly.
11. **Competitive Position is not measured at Recovery & Recycling**: the 407 assessed Recovery cells (Catch Up 21,
    Partner 76, Watch 310) are decided with the position read as not strong, so none can be Preserve; the reason says
    "Competitive Position not measured" on 317 cells. A recycling capability measure is the fix.
12. **Policy alignment rests mostly on one source**: 464 of the 679 over-prioritized Watch cells rest on the
    low-confidence total-attention fallback.
13. **The Explorer does not yet show the new method**: at e41cb5b the drawer labels the prize "Industrial Opportunity"
    while the bundle carries the strategic industrial value, the yard fields and `meta.yf_nodes` are not read, and the
    dashboard's posture budget check (128 KiB a drawer document) is below the export's (42 of 214 documents exceed
    it). The dashboard copy-refresh work is on it.
