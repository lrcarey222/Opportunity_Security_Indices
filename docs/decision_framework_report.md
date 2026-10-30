# Canonical decision framework: final report (2026-10-29, updated 2026-10-30)

Status: final. Numbers are from the 2026-10-30 rerun of commit 7d847ba ("Require two pieces of evidence for a
credible Catch Up pathway"; scripts 26 -> 27 -> 91, no input data changed), which followed the 2026-10-29 run of
commit 91dba6f after the review fixes. Commits on `feat/supply-chain-stages`, none pushed: 784b108 (the canonical
method), 91dba6f (review fixes), e82f631 (the Country page), 7d847ba (the two-leg pathway, section 7), 3c82b84 (its
review fixes; no output change). Where a
number is quoted "under the one-leg rule" it is from the 91dba6f run. Method:
[strategic_position.md](strategic_position.md); full readout with every table:
[strategic_position_readout.md](strategic_position_readout.md); plan: [decision_framework_plan.md](decision_framework_plan.md).
Diagnostics: `diagnostics/strategic_position/` (`decision_review.md`, `choice_cards.md`, `choice_*.csv`).

The canonical chain is **Need -> Prize -> Position -> Choice -> Policy alignment**, decided per country x technology x
six-stage node (214 economies x 11 technologies x 6 stages = 14,124 nodes). The legacy Energy Security x Economic
Opportunity framework (Grow / Upgrade / Exposed / Stable corners, Strategic Index) is kept for replication and
comparison only.

## 1. Methodological changes

- **Prize** (`industrial_opportunity`) rebuilt from the node's market prize (world demand growth under current
  policy, global capex momentum) lifted by the national home market where national data exist. Every capability,
  need and factor-cost variable of the legacy EO is removed. The old column is `legacy_eo_opportunity`.
- **Need split by stage**: `supply_chain_need` at the five producing stages, a new `deployment_need` at Deployment &
  Use (Ember 2030 targets and yearly electricity data, WEO 2025, GEVO 2026). The two are never combined.
- **Position decides the response**: Need x Prize triages; Competitive Position among the node's contenders (top
  quarter + capability floor) decides Preserve; a credible-pathway test (at least two of five evidence legs since
  2026-10-30) decides Catch Up vs Partner / Watch.
- **Secure stays a rule-based override** (three routes, unchanged engine).
- **Deployment & Use** is decided on deployment need alone: Accelerate or Watch, never Partner, Preserve or Catch Up.
- **Not assessed** is a state of its own (5,295 nodes, previously shown as Deploy).
- **Policy alignment** gains misaligned (wrong treatment), deployment gap (under-response) and an alignment
  confidence; it stays downstream of the choice.
- **Legacy namespacing**: the Country fact sheets' `quadrant` (grow / upgrade / secure / watch) is now
  `legacy_corner` (legacy_grow / legacy_upgrade / legacy_exposed / legacy_stable), Country schema 3, so `secure` and
  `watch` mean only the canonical choices. The legacy need x EO quadrant of 2026-10-28 is kept as `legacy_quadrant_*`.
- **Fixed on 2026-10-29**: the contender test read the posture engine's bracketed supply share (an unlisted
  economy was given the listing's residual), so every economy was a "contender" in Batteries Refining and 28 of them
  "preserved" battery-materials refining. The measured share (or its lower bound) is used now (`sp_measured_share()`).
- **Review fixes (commit 91dba6f; no choice changed)**: an unmeasured Competitive Position (all of Recovery &
  Recycling) still counts as not strong but now lowers confidence and is named in the reason; a top-quarter score
  that fails the Preserve test says why (not a contender, or below the capability floor); over-prioritization rests
  on observed NIPO protection wherever NIPO observes the node, and the total-attention fallback, now confined to nodes
  NIPO does not observe, is always low confidence.

## 2. Old vs new framework mapping

| legacy corner (ES x EO, relative to the economy's own median sector) | conceptual new choice | agreement on 6,351 placed nodes |
|---|---|---|
| Grow (high EO, high ES) | Preserve | 3% |
| Upgrade (high EO, low ES) | Catch Up | 6% |
| Exposed (low EO, low ES; key `secure` until schema 3) | Partner | 26% |
| Stable (low EO, high ES; key `watch` until schema 3) | Watch | 44% |

Results differ because the legacy indices mix capability, vulnerability, production, trade, investment, demand,
reserves, concentration and deployment, and because the corners are relative to each economy's own median sector
(every economy has Grow sectors) while Preserve is absolute among the node's contenders. The disagreements are
attributed to the cleaned prize (1,822), not assessed (1,368), deployment need (1,053), the cleaned need (612),
Competitive Position (223) and the Secure override (143).

## 3. New Industrial Opportunity definition

Is there a plausible economic or technological prize in this node? `industrial_opportunity` (0-1, producing stages):
the node prize = percentile across the 55 producing nodes of world demand growth 2024 -> 2035 (IEA WEO 2025 Stated
Policies node series; IEA Critical Minerals Stated Policies for mineral Extraction and Refining), blended 0.75 / 0.25
with the global manufacturing capex trend (Clean Investment Monitor) where observed (ten nodes), capped at 0.33 for a
declining world market; then lifted to the mean of node prize and national home-market growth where the home market
(national basis only) is higher. Maturity (TRL) is context, never scored. Feasibility, investment pipeline, adjacency,
learning and input cost are not in the prize; they are the Catch Up pathway test.

Validation: Spearman with Competitive Position 0.09 pooled (0.36 within the 37 nodes where the home market varies the
prize), with supply-chain need 0.04 / 0.13, with legacy EO 0.24. Component weights: world demand growth 82%, capex 4%,
home market 14%. Not a disguised capability score; but mostly node context, so countries differ mainly through need,
position and pathway. High prize: >= 0.6 (4,734 of 12,045 producing nodes; median 0.54).

## 4. New Supply-chain Need definition

How costly or dangerous is dependence on external supply for this node? `supply_chain_need` (0-1, producing stages
only) is the within-node percentile across economies of the mean of four families: import dependence (net imports /
GDP; fuel self-sufficiency), own-import supplier concentration (HHI), risky-supplier exposure (China and opposite-bloc
import share), and lack of trusted alternatives (1 - trusted suppliers' world share less the economy's own; observed
for 94% of assessed cells, above the 80% line for scoring). A self-supply guard zeroes the composition families for an
economy holding 10% or more of world supply. Substitutability, reconstitution time, inventories, systemic essentiality
and operational control stay Secure-engine inputs and drawer subcomponents. Measured for 7,146 of 11,770 producing
nodes; high need (>= 0.67) 2,326.

## 5. New Deployment Need definition

How much additional deployment does the country need to meet expected system requirements or its own goals?
`deployment_need` (0-1, Deployment & Use only) = mean of the observed family percentiles, each ranked within the
technology across countries:

| family | scenario | measures |
|---|---|---|
| demand adequacy | current policy | electricity demand growth (WEO 2025 Current Policies for BRA CHN IND JPN RUS USA, else the Ember 2020-25 trend), the technology's own generation growth, EV sales-share growth (GEVO 2026), oil-use trend |
| policy / target gap | national commitments | 2030 shortfall of installed + recent build rate against the country's targets and approved plans (Ember 2030 Renewable Target Tracker; the total-renewables target also for storage and grid) |
| system need | current policy | imported fuel displaced, flexibility need (solar + wind share), firm-capacity need |

Each family carries its basis (national / observed_trend / not_assessed) and scenario label. A climate-aligned value
(World WEO NZE vs Current Policies, 2035) is shown only as a labelled alternative and never scored. Missing evidence
stays missing: 671 of 2,354 Deployment nodes are not assessed. Fossil technologies are assessed exactly like clean
ones (user decision). Accelerate threshold 0.6 (514 of 1,683 assessed nodes, 31%), chosen for selectivity: deployment
need is a mean of family percentiles, narrower than one percentile, and 0.6 matches the selectivity of supply-chain need
at 0.67; the design's starting 0.67 would take 22% (it would leave the United States without an Accelerate node, and
Germany and France with Green Hydrogen only).

## 6. Secure logic

Step 1 of the choice: if the posture engine finds an acute vulnerability (Test 1 consequence AND one of three routes:
operational / control risk, a coercive chokepoint that needs concentration plus material import dependence, poor
substitution, long reconstitution or weak trusted alternatives, or systemic resilience for long-lead essential nodes),
the node is Secure whatever its need, prize or position, with `security_rationale`, `delivery_mode` and the mitigation
text. 174 Secure nodes: systemic resilience 151, coercive chokepoint 18, operational control 5; delivery allied 72,
mixed 60, diversified global 42. Only 163 of 2,326 high-need producing nodes (7%) are Secure: Secure does not absorb
high need.

Secure by rationale and stage (`secure_by_rationale_stage.csv`): systemic resilience is Extraction 68, Refining 57,
Assembly 23 (Grid equipment), Components 3; coercive chokepoint is Refining 15, Extraction 3; operational control is
Refining 5. Systemic-resilience Secure is mostly fossil-fuel extraction and processing; the equipment-resilience route
reaches only Grid Assembly (transformers, power electronics elsewhere are not reached). Secure does not move with the
need, prize, position or deployment thresholds below (it is the engine's override; its own rule sensitivity is in
`posture_threshold_sensitivity.csv`).

## 7. Preserve / Catch Up / Partner decision logic

At the producing stages, after Secure, with high need = `supply_chain_need >= 0.67`, high prize =
`industrial_opportunity >= 0.6`, strong position = Competitive Position in the top quarter (>= 0.75) among the node's
contenders (economies with >= 0.5% of world supply, measured) AND the engine's capability floor (moderate or strong):

| | strong position | else credible pathway | else high need | else |
|---|---|---|---|---|
| high prize | Preserve | Catch Up | Partner | Watch (R&D option value) |
| low prize, high need | Preserve + `review_strong_position_low_prize` | | Partner | |
| low prize, low need | Watch | | | Watch |

Credible pathway (at least two of the five, user decision 2026-10-30; design D3 had any one): Atlas feasibility in
the top quarter; CIM pipeline capacity in the node; an established adjacent stage; curated learning inputs on a
domestic base; an input-cost advantage. Extraction also needs a resource base. One leg with other evidence observed is
not credible; unknown (nothing observed, every observed leg fires but fewer than two are observed, or two legs at
Extraction with the resource base unknown) = not credible, lower confidence. Need or prize missing -> not assessed. An unmeasured Competitive
Position (every Recovery & Recycling node, and 128 assessed nodes elsewhere, all Watch) counts as not strong, so Recovery is never
Preserve; where the rule reads the position it lowers confidence like an unknown pathway, and the reason says
"Competitive Position not measured".

Threshold sensitivity (`choice_threshold_sensitivity.csv`; one threshold moved at a time; nothing was tuned):

| parameter | value | Preserve | Catch Up | Partner | Accelerate | Watch | nodes changed |
|---|---|---|---|---|---|---|---|
| base | | 86 | 748 | 1,850 | 514 | 5,457 | 0 |
| need | 0.60 / 0.75 | 95 / 81 | 748 / 748 | 2,237 / 1,368 | 514 | 5,061 / 5,944 | 396 / 487 |
| prize | 0.55 / 0.65 | 96 / 73 | 796 / 422 | 1,831 / 1,965 | 514 | 5,418 / 5,681 | 58 / 339 |
| CP bar | 0.67 / 0.85 | 114 / 52 | 724 / 774 | 1,847 / 1,857 | 514 | 5,456 / 5,458 | 28 / 34 |
| deployment | 0.55 / 0.67 | 86 | 748 | 1,850 | 669 / 372 | 5,302 / 5,599 | 155 / 142 |

The prize threshold is still a knife-edge, but a smaller one: 0.60 -> 0.65 cuts Catch Up from 748 to 422 and changes
339 nodes (under the one-leg rule it halved Catch Up, 1,660 -> 858, and changed 815), because the prize is dominated by
one country-invariant percentile, so whole nodes cross the line together. The credible-pathway rule was the other
lever and is now decided (user decision 2026-10-30): at least two of the five legs (used) keeps 748 Catch Up nodes; any
one of five legs (design D3, used until 2026-10-29) kept 1,660, of which the adjacent-stage leg alone carried 748. A
one-leg-era alternative, any one leg except the adjacent stage, would have kept 912; it was not adopted.

### Two-leg pathway (2026-10-30)

Commit 7d847ba sets `decision_framework.pathway.min_legs: 2` (`config/strategic_position.yml`,
`R/strategic_position/pathway.R`). One leg fired with other evidence observed is now `pathway_credible` "no" with
`pathway_basis` "<leg>;too_few_legs"; every observed leg fired but fewer than two observed is "unknown" with
"<legs>;too_few_observed" (no classified node is in that state on this run). Extraction still needs a resource
base. The choice reason names the shortfall, e.g. "only one piece of pathway evidence (adjacent stage) where two are
required: Partner with trusted suppliers." Scripts 26, 27 and 91 were rerun; no input data changed.

| rule | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| one leg (2026-10-29) | 174 | 86 | 1,660 | 1,592 | 514 | 4,803 | 5,295 |
| two legs (2026-10-30, used) | 174 | 86 | 748 | 1,850 | 514 | 5,457 | 5,295 |

Compete falls from 3,338 to 2,684 and Deploy rises from 5,317 to 5,971. The only moves are 912 Catch Up nodes: 258 to
Partner (high need) and 654 to Watch (all with `low_need` and `weak_domestic_case`, 287 also
`ample_diversified_supply`). No other node changed choice. Each moved node had one leg: adjacent stage 748, input cost
152, feasibility 7, learning 4, pipeline 1. Their confidence now: high 328, moderate 515, low 69. 169 economies had a
node move.

- Catch Up by stage: Extraction 64 -> 32, Refining 345 -> 153, Components 638 -> 269, Assembly 483 -> 243, Recovery
  130 -> 51.
- Legs among the 748 Catch Up nodes: adjacent stage 568, feasibility 558, learning 535, input cost 245, pipeline 105.
  Feasibility and learning fire together in 508 of them; 350 have exactly two legs, 293 three, 93 four, 12 five.
- Economies with any Catch Up node: 204 -> 112 (92 lost all of theirs); the median economy now has 1 Catch Up node
  (8 before).
- Battery-materials refining (Batteries x Refining): 171 -> 75 Catch Up economies. The most common Catch Up nodes now:
  Batteries Refining 75, Batteries Components 71, EV Components 65, EV Assembly 62, Wind Components 62, Batteries
  Assembly 60, Solar Assembly 57, Solar Components 57, EV Refining 56, Wind Assembly 53, Batteries Recovery 51, EV
  Extraction 15.

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

Of these nine only two moved: the United Kingdom's Solar Components (feasibility its only leg) is now Watch, and
Australia's Batteries Recovery, EV Components and EV Extraction (adjacent stage only) are now Partner. Elsewhere in
section 10's table, Brazil (Batteries Refining, Solar Components to Watch), Mexico (Gas Refining to Watch, Solar
Components to Partner) and Indonesia (Wind Components to Partner) moved.

Caveats:

- **The large economies barely move.** Feasibility and learning fire together for big diversified economies, so they
  keep their Catch Up nodes; only 9 of the 912 moved nodes are in the fourteen economies of section 10, and Catch Up
  is now driven by feasibility plus learning (508 of 748) rather than by adjacency alone. The weakness of section 16
  remains: every country's Catch Up clause leads with the same nodes, because the prize is mostly country-invariant.
- **The prize knife-edge is smaller, not gone** (0.65 changes 339 nodes, 815 under the one-leg rule).
- **Over-prioritized Watch rises from 375 to 503** (by basis: NIPO protection 122, local content 65, total-attention
  fallback 316; was 103 / 65 / 207), because the moved nodes keep their observed policy attention: 126 of the 654 new
  Watch nodes are over-prioritized (19 on NIPO protection, 107 on the low-confidence fallback). Two more Watch nodes
  (Libya Gas Extraction and Refining) are over-prioritized because Catch Up is no longer among their possible choices.
  Under-prioritized falls from 947 to 516 (431 of the moved nodes were under-prioritized Catch Up) and misaligned rises
  from 68 to 76 (8 of the new Partner nodes, e.g. Australia EV Components).
- **Australia's EV Extraction now goes Partner.** Its contender score (0.93) is in the top quarter but below the
  capability floor, and the adjacent stage was its only leg, so Partner with a contender CP >= 0.75 rises from 2 to 3
  (all three flagged `review_top_quarter_below_floor`). The flag now covers 8 nodes (5 Catch Up, 3 Partner; 11 under
  the one-leg rule): Russia and Philippines EV Extraction and Saudi Arabia Gas Assembly had only the adjacent-stage leg
  and are now Watch.
- **Consistency rates move in opposite directions** (section 12): Partner with cross-country CP >= 0.75 is 396 of 1,850
  (21%; 361 of 1,592, 23%, under the one-leg rule) and Catch Up with cross-country CP >= 0.9 is 173 of 748 (23%; 207 of
  1,660, 12%). Only 34 of the 912 moved nodes had a cross-country CP >= 0.9, so the remaining Catch Up set is
  stronger on average and the rate rises.
- **An unmeasured leg counts against a node.** "Not credible" needs only one observed leg that does not fire, so 483 of
  the 912 moved nodes were decided with a leg unobserved that could have made the second (input cost unobserved in
  329, the investment pipeline in 208, feasibility in 40; 394 with one leg unobserved, 89 with two or three). Design D3
  already read "some evidence observed, none fires" as "no" rather than "unknown"; the stricter reading would mark these
  483 "unknown", which leaves the choice unchanged but lowers confidence and keeps Catch Up among the possible choices.
  Not adopted; a user decision.

## 8. Accelerate / Watch decision logic

At Deployment & Use: deployment need missing -> not assessed; >= 0.6 -> Deploy · Accelerate (the energy system needs
more of it; says nothing about where the equipment is made); below -> Deploy · Watch (no major acceleration needed).
At producing stages Watch means "domestic industrial priority low", with reasons (low need, low prize, ample
diversified supply, weak domestic case, option value).

## 9. Policy Alignment logic

Observed policy (NIPO six-stage support and protection, IEA PAMS) is compared with the choice and never feeds it:
Secure + weak response -> under-prioritized; Preserve / Catch Up + weak capability-building support ->
under-prioritized; Partner + protection in the top third -> misaligned (wrong treatment); Deploy · Accelerate + weak
deployment policy -> deployment gap; Deploy · Watch + heavy protection or local-content rules -> over-prioritized;
sources disagreeing -> mixed; no data -> insufficient data. Where NIPO observes a node (a protection stock is
recorded, zero included), protection alone decides over-prioritization. Only where NIPO does not observe the node at
all does a fallback judge a Watch on total policy attention in the top third (design D5), and that verdict is always
low confidence. Alignment confidence high / medium / low from source coverage. Results (`choice_policy_alignment.csv`):
aligned 5,486; under-prioritized 516 (Catch Up 397, Secure 92, Preserve 27); deployment gap 182; over-prioritized 503;
misaligned 76; mixed 220; insufficient data 7,141. Confidence is high for 1,928 nodes, medium for 1,295 and low for
10,901 (one source or none).

Watch nodes with high policy intensity (`watch_high_policy_intensity.csv`): 1,157 of 5,457 Watch nodes carry top-third
policy intensity (priority, protection or deployment localisation). 503 are judged over-prioritized, by basis:

| basis | over-prioritized Watch nodes | alignment confidence |
|---|---|---|
| NIPO protection in the top third (producing stages) | 122 | high 120, medium 2 |
| the equipment's local-content protection (Deployment & Use) | 65 | medium |
| total-attention fallback: total policy attention in the top third, NIPO does not observe the node | 316 | low |

So 187 rest on observed protection or local content and 316 on the attention fallback (under the one-leg rule, 168
and 207 of 375; section 7). Until the review fix, 100 Watch nodes whose NIPO protection stock is recorded (98 at
zero) were over-prioritized on total attention because their economy protects too few nodes to rank; they are now
aligned or mixed. The rest of the 1,157: 563 aligned (mostly
Deployment Watch with heavy PAMS deployment support but no localisation: support is not over-prioritization), 41
insufficient data, 50 mixed. Examples, all on observed protection: India, Korea, Germany, UK and Canada Electric Grid
equipment (protection 0.78-0.99 on a low-need, low-prize node); US Nuclear Assembly and Deployment (protection and
local content 0.78, deployment need 0.60, just below the line); Australia Solar Components (protection 0.91, weak
domestic case); France Batteries and Wind Deployment (local content 0.96 / 0.99, deployment need 0.39 / 0.34).
Misaligned Partner nodes: Australia Solar and Wind Assembly, US Nuclear Refining, India Coal Refining.

## 10. Distribution of all choices by country

All nodes: Secure 174, Preserve 86, Catch Up 748, Partner 1,850, Accelerate 514, Watch 5,457, not assessed 5,295.
Per economy (214, of 66 nodes): median 0 Secure, 0 Preserve, 1 Catch Up, 8 Partner, 1 Accelerate, 26.5 Watch, 23 not
assessed; 83 economies have a Secure node, 25 a Preserve node, 112 a Catch Up node, 129 an Accelerate node. Every
economy has at least 20 not-assessed nodes (the same 20 nodes have no measured supply-chain need anywhere). All economies:
`diagnostics/strategic_position/choice_by_country.csv`.

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

## 11. Largest changes from the old methodology

Against the 2026-10-28 need x EO quadrant (the live posture before this work): 4,624 nodes previously shown as Deploy
on insufficient data are now not assessed; Deployment & Use (2,354 nodes, all previously "Deploy" without a decision)
splits into 514 Accelerate, 1,169 Watch and 671 not assessed; 1,553, 413 and 146 insufficient-data nodes are now
Watch, Partner and Catch Up through Competitive Position and the pathway; the cleaned prize moves 329 watch -> Catch
Up, 136 partner -> Catch Up and 119 preserve -> Watch; the cleaned need moves 220 partner -> Watch and 174 watch ->
Partner. Preserve falls from 266 to 86 (top quarter among contenders), Catch Up rises from 117 to 748.

Named examples: France Nuclear Components and Gas Refining preserve -> Watch (legacy EO 0.93 / 0.89 carried by
capability and cost; world demand growth gives a prize of 0.42 / 0.25); Australia Batteries Components partner -> Catch
Up (prize 0.85; pipeline and adjacent stage); Canada Solar and EV Components watch -> Catch Up; Canada Coal Components
watch -> Partner (need 0.76, prize 0.16); battery-materials refining in the US, Japan, Korea, Germany, UK, France,
Canada and India: Catch Up, not Preserve (8 real contenders; China and Indonesia preserve).

## 12. Remaining consistency flags

| contradiction | before (2026-10-28 quadrant) | after |
|---|---|---|
| Partner with CP in the contenders' top quarter | 8 | 3 |
| Catch Up with CP in the contenders' top decile | 10 | 2 |
| Catch Up with 10%+ of world supply | 8 | 6 |
| Preserve without capability | 66 | 0 |
| Preserve below the contenders' median | 43 | 0 |
| Partner with cross-country CP >= 0.75 | 363 (25%) | 396 (21%) |
| Catch Up with cross-country CP >= 0.9 | 49 (42%) | 173 (23%) |

Flags kept for review: `review_strong_position_low_prize` 16 (Preserve; mostly fossil and nuclear incumbents, e.g.
Russia Nuclear Refining, US and Canada Oil Extraction, Japan Coal Components, France Nuclear Refining);
`review_top_quarter_below_floor` 8: 5 Catch Up (Solar Components in India, Viet Nam, Thailand, France; Cambodia Solar
Assembly) and 3 Partner (Russia Gas Extraction at 9.6% of world supply, just under the engine's 10% line with no
complementary capability recorded; Nigeria Gas Refining; Australia EV Extraction, Catch Up under the one-leg rule).
The cross-country counts fall as rates only: most are mid-ranked contenders in crowded nodes, which the contender test
treats as not leading. Under the one-leg rule the Catch Up rate was 12% (207 of 1,660); the second leg removed mostly
lower-ranked positions, so it is 23% now. Partner at 21% "despite" cross-country CP >= 0.75 and Catch Up at 23%
with cross-country CP >= 0.9 are the weakest consistency results. A top-quarter score held by an economy below the
contender line is not flagged but now named: its trace says `not_a_contender` and its reason "a top-quarter score
without a contender share" (32 nodes, e.g. Canada and India Batteries Assembly at 0.13% of world supply).

## 13. Deployment-priority examples

| economy | Accelerate (deployment need) | Watch, near the line |
|---|---|---|
| United States | Storage 0.64, Gas | Nuclear 0.60, Grid 0.58; Solar 0.47, Wind 0.37, EVs 0.09 |
| Japan | Nuclear 0.78, Solar 0.66, Wind 0.65, Gas, Green Hydrogen | Storage 0.55, Grid 0.52 |
| South Korea | Nuclear 0.80, EVs 0.80, Solar 0.73, Wind 0.72, Green Hydrogen, Geothermal | Storage 0.56, Grid 0.56 |
| India | Nuclear 0.81, Storage 0.80, Solar 0.79, Wind 0.75, Grid 0.75, Coal, Gas, Oil, Geothermal | EVs 0.30 |
| Germany | Storage 0.66, EVs 0.65, Solar 0.63, Green Hydrogen 0.71 | Wind 0.59, Grid 0.58, Nuclear 0.45 (phase-out) |
| United Kingdom | Storage 0.69, Wind 0.66, Grid 0.64 | EVs 0.59, Solar 0.55, Nuclear 0.31 |
| France | EVs 0.64, Green Hydrogen 0.84 | Nuclear 0.56, Solar 0.37, Wind 0.34 |
| Australia | Storage 0.83, Grid 0.71, Wind 0.66, Solar 0.61, Oil | Nuclear 0.47, EVs 0.46 |
| Canada | Gas | Storage 0.51, Grid 0.50, Wind 0.41 |

Deployment need tracks demand where it is national (India) and target gaps for renewables (UK wind 0.91 policy gap;
Germany solar 0.76; Korea solar / wind; Australia wind). No national nuclear, EV, hydrogen or grid targets are staged,
so those rest on demand and system need. US EVs (0.09) reflect GEVO Current Policies (sales share 10% -> 8% by 2035) and
no oil-import displacement: a current-policy reading, with the climate-aligned alternative shown separately.

## 14. Where manufacturing priority and deployment priority differ

Across all economies: 385 technology pairs accelerate deployment while manufacturing (Components and Assembly) is
neither Preserve nor Catch Up; 173 the reverse (deployment Watch, manufacturing Preserve or Catch Up); 129 both high;
996 both low.
Examples: deploy faster, manufacture elsewhere: Australia Solar, Wind and Grid; Japan Nuclear and Gas; India and UK
Grid; India Coal, Gas and Oil. Manufacture, deployment on track: United States Solar, Wind and EVs (Preserve
manufacturing; deployment need 0.47 / 0.37 / 0.09); Canada Solar, Wind, Nuclear, EVs and Storage (Catch Up
manufacturing; deployment Watch); Korea Storage; France Wind and Storage; Germany Wind.

## 15. Country-page redesign

The Country profile (`dashboard/src/country/views/profile`, components in `dashboard/src/posture`) now follows Need ->
Prize -> Position -> Choice -> Policy alignment, in seven sections with the same order in the side navigation (pill
bar on narrow screens): Strategy, Need, Prize & Position, Choices, Policy Alignment, Allied Strategy, Evidence &
Methodology. Old section anchors (`#sectors`, `#opportunities`, `#priorities` ...) still scroll to their new homes.

- **Header:** the country name, one generated strategy sentence (`posture/strategy.ts`: top Preserve nodes, Secure
  vulnerabilities with their rationale, top Catch Up nodes, the main Partner needs, the technologies with the highest
  deployment need; every clause backed by at least one node, a clause dropped when its group is empty), the data
  freshness line (strategy build, trade benchmark, momentum cut-off, country facts date), and the actions (memo, which
  says it is being revised; the strategy CSV; all data and downloads). The "at a glance" tiles are gone.
- **Strategy:** the Country Strategy Map (technology x six stages) leads the section. Three background families only
  (Secure, Compete, Deploy) plus a hatched not-assessed state; markers O / K / R for the Secure rationale, P / C / ⇄
  for Preserve / Catch Up / Partner, ↑ / • for Accelerate / Watch; policy alignment as the cell border and evidence as
  opacity. The Deployment & Use column is set apart by dividers and the annotation "Supply-chain strategy → |
  Energy-system strategy | → Circularity", and the footnote states which need each column reads. Above the grid: the
  six-verb counts ("4 Secure · 1 Preserve · 10 Catch Up · 10 Partner · 5 Accelerate · 16 Watch" for Japan) with the
  family line and the not-assessed count beneath.
- **Need:** two panels that are never added together: "Where is dependence a problem?" (producing-stage nodes,
  Secure first, then by supply-chain need, with reason tags) and "Where does the energy system need to move faster?"
  (technologies by deployment need, each family with its scenario and basis named, and the climate-aligned value
  shown only as a labelled alternative).
- **Prize & Position:** a scatter of each producing node, X = Industrial Opportunity, Y = Competitive Position, with
  the 60 and 75 thresholds and quadrant tendencies; the marker is the choice and Secure nodes are outlined at their
  true position. A stage filter and a table view. The monthly trade card follows the selected node.
- **Choices:** six groups (Secure, Preserve, Catch Up, Partner, Accelerate, Watch) plus not assessed, each card with
  the fields spec XIV asks for (Secure: rationale, main risk, delivery mode, treatment; Catch Up: opportunity,
  position gap, credible pathway and its evidence; Partner: need, domestic case, partners, diversification value, AIB
  status; Accelerate: the three deployment-need families, trend, and domestic manufacturing priority stated
  separately; Watch: why domestic industrial priority is low).
- **Policy Alignment:** one table of every assessed node: recommendation, observed policy intensity, support,
  protection, momentum, alignment with its confidence. No overall score. The PAMS / NIPO / climate-rating detail sits
  below it.
- **Allied Strategy:** one row per Partner node and per Secure node delivered with trusted foreign capacity: need ->
  recommended partners -> why -> AIB status (covered, gap, or no credible allied solution). The generic partnership
  material is under a closed "Explore all partnership analysis".
- **Evidence & Methodology:** how the strategy is decided, the stage mapping, the country's evidence coverage and
  freshness, notes, sources and downloads, and at the bottom the Legacy OSI framework (paper baseline) with the
  old-to-new mapping; the corners scatter, Strategic Index and partner tables are inside a closed disclosure, and
  the Perspective dials appear only there, as "Explore scenarios".
- **One drawer:** every entry point (map cell, either Need row, scatter point, choice card, policy row, allied node)
  sets `?node=Tech|Stage` and opens the same drawer with exactly five questions: Need, Prize, Position, Choice, Policy
  alignment. A shared link opens its node.
- **Elsewhere:** the Sector pages carry a new card, "Every economy's choice in one node" (Competitive Position
  against the node's own need, markers by choice). The AIB division card counts Partner demand from Compete · Partner
  nodes only. The glossary and the Methods page lead with the decision framework and label the old indices Legacy
  OSI. The Strategic views (/overview, /country, /space, /strategic, /partners) carry a "Legacy OSI framework" label.

## 16. Screenshots / rendered-page review

Reviewed in the browser (Vite dev server, real exports) on Japan, with spot checks of the United States, Korea,
India, Germany and the UK, at 1600 x 1000 (large desktop), 1366 x 800 (laptop), 768 x 1024 (tablet) and 375 x 812
(phone).

- **Large desktop and laptop:** all seven sections render in order with no page-level horizontal scroll and no
  console errors. The Strategy Map's stage names, Deployment & Use divider and annotation, markers and legend are
  legible. On a 1366 x 800 laptop the map's first row sat at 863 px (below the fold); the Strategy section's
  one-line intro is now dropped (the map's footnote already says to select a node), the strategy sentence runs
  wider and the action row is tighter, so the first row now sits at 762 px. The rest of the map stays below the
  fold at laptop size.
- **Tablet:** the side navigation becomes a sticky pill bar; the full map fits the width; the Need panels stack.
- **Phone:** no page-level horizontal scroll; the map keeps its own horizontal scroller (Recovery & Recycling is
  reached by swiping); the drawer opens as a bottom sheet; the counts line wraps.
- **Fixed during the review:** a Catch Up card read "at the Preserve bar" when the node's position is not measured
  (Japan battery recycling); it now reads "position not measured". The domestic manufacturing priority of a
  technology with no assessed producing stage read "low"; it now reads "not assessed" (Japan green hydrogen). The
  drawer's "Trusted alternatives" row is a need value (high = few trusted alternatives) and is now labelled "Lack
  of trusted alternatives". The drawer's policy sources showed the raw codes "nipo, pams"; they now read "NIPO
  industrial policy records, IEA Policies and Measures".
- **Fixed after the code review** (three reviewers, then a skeptic pass; 15 findings confirmed). Engine, committed as
  91dba6f, then 26 -> 27 -> 91 rerun from that commit with the choice counts unchanged:
  - An unmeasured Competitive Position (all of Recovery & Recycling) lowers confidence and is named in the reason.
  - A top-quarter score that fails the Preserve test says why (not a contender, or below the capability floor).
  - Over-prioritization rests on observed NIPO protection where it exists; the total-attention fallback is always
    low confidence.
  - The bundle names the commit that produced it.

  Page:
  - Watch reasons come from the engine. The page had read "trusted alternatives" backwards and called scarce
    supply "ample" on about 40% of Watch nodes.
  - Support, protection and policy momentum read as ranks within the country ("low, 22nd percentile"), not as
    shares or signed changes, with "none recorded" where no protection exists.
  - Catch Up and Partner text no longer calls a top-quarter score that failed the Preserve test "strong" or "at
    the Preserve bar".
  - The Country page's definition of Watch includes its second route: a high prize with low need and no domestic
    case.
- **Still worth a look:** Choices (about 3,800 px on Japan) and Allied Strategy (about 2,800 px) are long; the cards
  sit in a two-column grid, so a short group (Preserve, 1 card) leaves white space beside a long one (Secure, 4
  cards). On the Sector card, economies that score below every contender in a node all sit at position 0 and form
  one dense column (about 170 in Batteries Refining). The freshness line dates the strategy build in UTC ("30
  October 2026" for a run on the evening of 29 October, Pacific time).
- **What the numbers say on screen:** every country's Catch Up clause leads with the same nodes (battery recycling,
  PV modules & inverters, wafers & PV cells), because the prize is mostly country-invariant (sections 7 and 18). The
  two-leg pathway (2026-10-30) does not change that for the large economies, where feasibility and learning fire
  together. The Sector card showed 171 economies as Catch Up in battery-materials refining under the one-leg rule; the
  rerun bundle has 75 (not re-reviewed in the browser).

## 17. Tests and results

Final run, 2026-10-30, on the real exports in the worktree (posture bundle schema 2 from commit 7d847ba, the two-leg
pathway rerun; 214 economies, 14,124 nodes; Country bundle schema 3, 219 fact sheets), after the integration and
review fixes. Totals: **1,700 dashboard tests and 6,569 R expectations pass; the one R failure predates this
build.** The jsdom and browser checks further down were made on the 2026-10-29 bundle (commit 91dba6f); on the
2026-10-30 bundle the drawer of a moved node (Australia EV Components) was checked in the browser.

**Dashboard (TypeScript, from `dashboard/`)**

- Type check: `tsc -p tsconfig.app.json --noEmit` and `tsc -p tsconfig.node.json --noEmit`, 0 errors each.
- Vitest, all seven projects, 2026-10-30: 113 files, 1,700 tests, 0 failures (the first run caught the pathway
  glossary entry over its 35-word limit; shortened, keeping "Unknown counts as not credible").

| Project | What it covers here |
|---|---|
| unit | pure code: the posture contract v2 (vocabularies, the spec VIII invariants), the real posture bundle through the contract (`real.posture.test.ts`, 6) and its size budget (2), the strategy sentence, the node selection, the section rows, the words for every R code a reader can meet (flags, trace, pathway evidence, borderline inputs); the Country contract schema 3 (legacy corner rename, refusals of schema-2 names), derivations |
| ui | every view in jsdom; the spec XXII UI list in `country/__tests__/strategy.view.test.tsx` (11 tests): the Strategy Map renders all six stages, the Deployment & Use column is set apart, Accelerate and Watch markers render, the Need panels, the Prize & Position scatter, the six Strategic Choices groups, Policy Alignment, Allied Strategy from the canonical partner outputs, the Legacy OSI kept out of sections 1-6 (no dials, corners or Strategic Index; the engine never starts), cross-navigation from all six entry points through `?node=`, phone width |
| parity-synthetic | the in-browser Legacy OSI engine against R on the committed fixtures |
| parity-real | the same on the real Strategic bundle: the legacy indices replicate unchanged |
| aib-real | the AIB bundle, and on the real posture bundle: Partner demand = Compete · Partner nodes, Secure demand = Secure nodes, Accelerate only at Deployment & Use |
| sector-real | the real Sector bundle (manifest 176 PASS, 4 WARN, 0 FAIL) |
| country-real | the real Country bundle (schema 3, no `quadrant` key left) |

- `npm run build`: ok. `npm run budget`: OK, all 18 figures within their limits. The posture bundle is now counted
  where pages load it (2026-10-30 build): Countries landing 552 kB of 700 kB (posture manifest + core 87 kB on the
  wire), Country profile first view 809 kB of 900 kB (+ the largest drawer document, 13 kB), a new "Posture data"
  figure 1.90 MB against 2.15 MB (measured + ~15%), All data 38.75 MB of 42.5 MB with posture included. No other
  limit was changed. The Sector pages' posture card also loads the core (87 kB); counted, the Sector profile (1.22 MB)
  would read 1.31 MB against its 1.30 MB limit, so the script
  reports it as a note (an open decision: lazy-load the card or raise that limit). Raw
  file sizes (`posture/__tests__/budget.test.ts`) after the 2026-10-30 rerun: core 2,324 kB of 3 MiB, largest drawer
  document 123 kB of 128 KiB.

**R (testthat, 44 files, each run with `stop_on_failure = FALSE`)**: 6,569 expectations pass, 1 fails, 0 errors
(with 3c82b84, the hardening of the two-leg setting).

- Decision framework and strategic position (24 files, 2,523): decision-engine 273, industrial-opportunity 96,
  deployment-need 106, home-market 29, decision-quadrant 74, partner-recs 59, posture-export 72, strategic-posture 95,
  posture-nodes 86, posture-policy-design 200, security-routes 50, trade-momentum 63, supply-chain-stages 54,
  strategic-position-plots 221, competitive-position 83, strategic-need 140, strategic-resource-base 36,
  supply-structure 126, policy-priority 128, investment-momentum 69, variable-stage-evidence 86, strategic-alignment 26,
  strategic-index 259, legacy-si-sensitivity 92.
- Country (5 files, 2,259): check 1,748, memo 90, rules 342, harmonization 37, trade-monthly 42.
- Dashboard writers (5 files, 294): cmfdi 101, fixtures 24, osb 60, screen 76, ties 33.
- Sector (9 files, 823): chunks 121, dev 39, dictionary 79, facts 157, fdi 132, parity 19, summary 118, trade-sub 85,
  trade 73.
- raw-inputs-manifest: 670 pass, 1 fail. The failure ("manifest carries no entries the active pipeline no longer
  reads": `electro_industrial_hs6_dictionary`) predates this build: the entry is unchanged since the checkpoint, the file
  lives in `data/reference` and `scripts/31` reads it through `config/aib.yml`, which the reference scan does not
  follow. The fix (`retain: true` on that entry) is left to the user.

The spec XXII method tests are in `test-industrial-opportunity.R` (no capability and no need variable can enter the
prize; country-invariant factors do not differentiate countries; missing components reported, not filled; the
validation tables), `test-deployment-need.R` (Deployment & Use only; demand adequacy and target gaps computed
consistently; scenarios labelled per family, the climate-aligned value a separate alternative; missing evidence stays
NA, never zero) and `test-decision-engine.R` (Secure override first; Deployment & Use decided on deployment need, high
-> Accelerate; a Deployment node never becomes Partner, Preserve or Catch Up; high prize + strong position ->
Preserve; high prize + weak position + credible pathway -> Catch Up; high need + low prize + weak position -> Partner;
low need + low prize -> Watch; observed policy never determines the recommendation). Commit 7d847ba adds the two-leg
pathway tests to `test-industrial-opportunity.R` and `test-decision-engine.R`: the one-leg test pinned to
`min_legs: 1`, the `too_few_legs` / `too_few_observed` states and bases, the reason wording and invalid settings.

**Checks on the real exports beyond the suites**

- The Country page rendered in jsdom from the real exports for Japan, the United States, Germany, India, Australia,
  Brazil, Nigeria and Kazakhstan: the header and sections 1-6 (default view) carry no legacy wording (Energy Security,
  Economic Opportunity, Strategic Index, corners, Perspective dials, NEIS baseline) and no three-stage wording. On
  Japan, each of the six entry points (map cell, supply-chain and deployment Need rows, Prize & Position dot, choice
  card, policy row, allied node) sets `?node=` and opens the one drawer with its five questions.
- Raw R codes: every drawer of Japan, the United States, India, Australia and Nigeria (5 x 66 nodes), and the header
  and sections 1-6 of Japan, the United States, India, Nigeria and China with every "Show all" open, carry no
  snake_case code, "NaN" or "undefined" (after the fixes below).
- In the browser (Vite dev server, 1440 x 900 and 375 x 812): the Strategy Map shows the six stage names at desktop
  width and the short labels on a phone, with no horizontal page scroll and no console warning; `/sectors`, `/aib`,
  `/methods#legacy-osi` and `/space` render without an error notice.

**Fixed in the integration pass**

- The Strategy Map's stage column names were invisible at 640 px and wider: an unlayered `.sr-only` rule in
  `dashboard/src/styles/globals.css` beat Tailwind's layered `sm:not-sr-only` (it also hid the fence skip link on
  focus). Removed; Tailwind's own `sr-only` utility applies.
- The strategy sentence read ambiguously when node phrases carry their own "and" ("wafers and PV cells and PV modules
  and inverters"): in a list of two or more, those now read "&" ("battery recycling, wafers & PV cells and PV modules &
  inverters"); unit-tested (`posture/__tests__/strategy.test.ts`).
- Legacy wording left in sections 1-6: the Strategy intro no longer names the Perspective dials, the sections' basis
  tag reads "Strategic position framework · fixed thresholds", and the header's freshness line says "country facts as
  of" rather than "index baseline". The Legacy OSI test now also forbids "dials" in sections 1-6.
- The climate-aligned alternative of deployment need is a ratio to current policies (World 2035, Net Zero vs Current
  Policies; R unit `ratio_vs_current_policies`), but the drawer showed it as a percentile ("55%") and the Need panel as
  a 0-100 score. It now reads "+55% vs current policies · World 2035, Net Zero vs Current Policies"; the CSV adds the
  unit; the posture test fixture carries R's shape.
- Raw R codes in the drawer and tooltips (the decision trace's tests and results, the pathway evidence, the borderline
  inputs, the policy-alignment notes, the `review_top_quarter_below_floor` flag) are now words from one table in
  `posture/grammar.ts`, unit-tested.
- `posture/__tests__/sections.test.tsx` expected "Delivery modemixed"; the new glossary entry for delivery mode puts a
  tip trigger between label and value, so the assertion allows the space, as the Competitive Position line already did.
- `dashboard/scripts/check-budget.mjs` counts the posture bundle (above).

## 18. Known data limitations

- Not assessed: 37% of nodes, the same 20 nodes in every economy (no supply-chain need measured for Geothermal and
  Green Hydrogen chains, most Recovery nodes, Nuclear, Solar and Wind Extraction); 671 Deployment nodes without
  deployment need (Oil, Green Hydrogen, EVs in small economies).
- The prize is 82% country-invariant world demand growth and mixes a stock (installed capacity for manufacturing nodes)
  with a flow (mineral demand for Extraction / Refining): Solar Refining 0.20 vs Solar Components 0.85.
- Mineral-node supply shares are weighted over minerals with partial IEA listings; an unlisted economy's share is only
  bracketed (0 to the residual). The contender test now uses the lower bound.
- Deployment need: WEO demand is national for six economies only (else the 2020-25 trend); no national grid,
  congestion, EV, hydrogen or nuclear targets; no committed pipeline (GEM needs a contact form); Ember capacity ends in
  2024 for some economies; stationary storage installed base for 21 economies; WEO scenario capacity assumes a constant
  capacity factor (inflates Japan's nuclear restarts).
- Policy alignment rests on one source for most nodes (confidence low for 10,901); PAMS is the only deployment-policy
  source; NIPO's support / protection split is sparse, so 316 of the 503 over-prioritized Watch nodes rest on the
  low-confidence total-attention fallback.
- Competitive Position is not measured at Recovery & Recycling (no supply share or capability family for recycling):
  Recovery nodes are decided with the position read as not strong, at lower confidence.
- Licences: Ember data CC BY 4.0; WEO-derived values CC BY-NC-SA 4.0; BNEF values never ship.

## 19. Recommended next refinements

1. Done 2026-10-30 (user decision, commit 7d847ba): the Catch Up pathway now needs two legs; Catch Up 1,660 -> 748,
   the 912 moved nodes going 258 to Partner and 654 to Watch (section 7). The alternative of one leg other than the
   adjacent stage (912 Catch Up under the one-leg rule) was not adopted. Still open: Catch Up for large economies
   rests on feasibility and learning, which fire together for most of them, so their Catch Up lists did not change.
2. Use WEO capacity additions (a flow) for manufacturing nodes so manufacturing and mineral prizes are comparable.
3. Measure supply-chain need for the 20 structurally unassessed nodes (hydrogen, geothermal, recovery flows).
4. Add national grid, storage, EV and nuclear targets and a committed pipeline to deployment need.
5. Report the prize-threshold knife-edge to users (0.60 -> 0.65 cuts Catch Up from 748 to 422; it halved it under
   the one-leg rule) or smooth it with a borderline band shown on the map.
6. Revisit the engine's 10% "strong position" line for large fossil producers (Russia gas extraction) and the
   resilience route for equipment nodes (transformers, power electronics), which today only reaches Grid Assembly.
7. Improve the weighted-mineral home share for partially listed refiners (Indonesia, Japan battery-metal refining).
8. Measure recycling capability (secondary output, scrap processing capacity) so Recovery & Recycling has a
   Competitive Position and can be Preserve; today it is read as not strong, at lower confidence.
9. Extend NIPO's support / protection coverage (or add a second protection source) so fewer Watch nodes rest on the
   low-confidence total-attention fallback (316 of the 503 over-prioritized).
