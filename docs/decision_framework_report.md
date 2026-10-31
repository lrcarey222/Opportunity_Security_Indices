# Canonical decision framework: final report (2026-10-29, updated 2026-10-30)

Status: final. Current numbers are from the 2026-10-30 pipeline rerun committed as e41cb5b: the Atlas of Economic
Complexity 2024 release, the basket change (quartz and silver ores at Solar Extraction, yellowcake at Nuclear
Extraction), the dyads and energy trade combined with the electro-industrial dictionary pull, the re-pulled 2022-24
trade benchmark and monthly momentum, then scripts 26 and 27 on commit a54a9d3 (capability-anchor Catch Up, the 0.10
need floor, fences by yard, Strategic Industrial Value, the large-yard case, layer-first yards) and the exports.
**Sections 10 and 20-29 carry the current results** (the nine-country table is in section 10, its path from the
two-leg run in section 27); sections 1-14 and 17-19 have been brought up to the same run where the diagnostics allow,
and sections 15-16 describe the Country page as built on 2026-10-29. Figures from earlier runs are kept only where labelled: "two-leg run" is the 2026-10-30 rerun of 7d847ba (before
the Atlas 2024 data, the anchor rule and the Yards & Fences method), "under the one-leg rule" the 2026-10-29 run of
91dba6f, and "old data" the inputs those runs used.

Commits on `feat/supply-chain-stages`, none pushed: 784b108 (the canonical method), 91dba6f (review fixes), e82f631
(the Country page), 7d847ba (the two-leg pathway, section 7), 3c82b84 (its review fixes; no output change), a442484
(these docs); then ea6089a and d65ae03 (capability-anchor Catch Up, section 20), 56e9271 (merge of the Atlas 2024
release, 01a1be7), 42dd358 and 20a431e (basket and evidence sources, section 21), 26d0131 (the need floor, section 22),
4c804b6 (the Country page by fences, section 23), 37f8f13 (fence heights by yard, section 24), 315cff0 (merge of
`feat/yf-method`: 50b81e6, 777f6b2, 99528e6; section 25), a54a9d3 (the posture core carries the Yards & Fences
evidence once, section 26) and e41cb5b (the rebuilt bundles and diagnostics, section 27). Method:
[strategic_position.md](strategic_position.md); full readout with every table:
[strategic_position_readout.md](strategic_position_readout.md); plan: [decision_framework_plan.md](decision_framework_plan.md);
node attributes: [yf_node_attributes.md](yf_node_attributes.md).
Diagnostics: `diagnostics/strategic_position/` (`decision_review.md`, `choice_cards.md`, `choice_*.csv`,
`catch_up_*.csv`, `strategic_value_*.csv`, `large_yard_*.csv`, `yard_*.csv`, `yf_paper_reproduction.csv`).

**Current results (e41cb5b; 214 economies x 66 nodes = 14,124):** Secure 206, Preserve 103, Catch Up 355, Partner
1,474, Accelerate 514, Watch 6,585, not assessed 4,887. Yards by cell (the roll-up of the stage row and its sensitive
layers): small 181, medium 1,809, large 6,051, mixed 1,196.

The canonical chain is **Need -> Prize -> Position -> Choice -> Policy alignment**, decided per country x technology x
six-stage node (214 economies x 11 technologies x 6 stages = 14,124 nodes). The legacy Energy Security x Economic
Opportunity framework (Grow / Upgrade / Exposed / Stable corners, Strategic Index) is kept for replication and
comparison only.

## 1. Methodological changes

- **Prize** (`industrial_opportunity`) rebuilt from the node's market prize (world demand growth under current
  policy, global capex momentum) lifted by the national home market where national data exist. Every capability,
  need and factor-cost variable of the legacy EO is removed. The old column is `legacy_eo_opportunity`. Since
  2026-10-30 this market prize is one of three families of the prize the choice reads, **Strategic Industrial
  Value** (market, industrial commons, future option, with a market-only cap; section 25).
- **Need split by stage**: `supply_chain_need` at the five producing stages, a new `deployment_need` at Deployment &
  Use (Ember 2030 targets and yearly electricity data, WEO 2025, GEVO 2026). The two are never combined. High
  supply-chain need also needs a need score of at least 0.10 (section 22).
- **Position decides the response**: Need x Prize triages; Competitive Position among the node's contenders (top
  quarter + capability floor) decides Preserve; a credible-pathway test decides Catch Up vs Partner / Watch: since
  2026-10-30 a capability anchor plus corroboration from a different evidence family (section 20; at least two of
  five evidence legs earlier that day, section 7).
- **A large-yard case** (mature, passive, commodity nodes) sends a node to Watch before the prize is read, unless a
  top-quarter position comes first (section 25).
- **Secure stays a rule-based override** (three routes, unchanged engine).
- **Deployment & Use** is decided on deployment need alone: Accelerate or Watch, never Partner, Preserve or Catch Up.
- **Not assessed** is a state of its own (4,887 nodes on the current run; 5,295 on the old data, before Solar and
  Nuclear Extraction had a traded basket; previously shown as Deploy).
- **Yards and fences**: every choice maps to a yard (small = Secure; medium = Preserve, Catch Up, Partner, Accelerate;
  large = Watch), sensitive layers are yard units of their own rolled up to the cell, and the policy-design fence
  height follows the yard (sections 24-25).
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

| legacy corner (ES x EO, relative to the economy's own median sector) | conceptual new choice | agreement on 6,351 placed nodes (current run) | two-leg run |
|---|---|---|---|
| Grow (high EO, high ES) | Preserve | 4% (78 of 2,131) | 3% |
| Upgrade (high EO, low ES) | Catch Up | 4% (42 of 1,108) | 6% |
| Exposed (low EO, low ES; key `secure` until schema 3) | Partner | 18% (367 of 2,018) | 26% |
| Stable (low EO, high ES; key `watch` until schema 3) | Watch | 55% (601 of 1,094) | 44% |

Results differ because the legacy indices mix capability, vulnerability, production, trade, investment, demand,
reserves, concentration and deployment, and because the corners are relative to each economy's own median sector
(every economy has Grow sectors) while Preserve is absolute among the node's contenders. On the current run the
disagreements are attributed to not assessed (1,119), the strategic industrial value (1,072), deployment need (1,053),
the large-yard case (1,031), the cleaned need (449), Competitive Position (374) and the Secure override (165)
(`legacy_osi_corners_vs_choice_drivers.csv`; on the two-leg run: the cleaned prize 1,822, not assessed 1,368,
deployment need 1,053, need 612, Competitive Position 223, Secure 143). Exposed agrees less because the large-yard case
sends commodity Partner nodes to Watch; Stable agrees more for the same reason.

## 3. New Industrial Opportunity definition

Is there a plausible economic or technological prize in this node? `industrial_opportunity` (0-1, producing stages):
the node prize = percentile across the 55 producing nodes of world demand growth 2024 -> 2035 (IEA WEO 2025 Stated
Policies node series; IEA Critical Minerals Stated Policies for mineral Extraction and Refining), blended 0.75 / 0.25
with the global manufacturing capex trend (Clean Investment Monitor) where observed (ten nodes), capped at 0.33 for a
declining world market; then lifted to the mean of node prize and national home-market growth where the home market
(national basis only) is higher. Maturity (TRL) is context, never scored. Feasibility, investment pipeline, adjacency,
learning and input cost are not in the prize; they are the Catch Up pathway test.

Validation (current run, `opportunity_validation_*.csv`): Spearman with Competitive Position 0.095 pooled (0.38
within the 37 nodes where the home market varies the prize), with supply-chain need 0.04 / 0.10, with legacy EO 0.26
(two-leg run: 0.09 / 0.36, 0.04 / 0.13, 0.24). Component weights: world demand growth 82%, capex 4%, home market 14%.
Not a disguised capability score; but mostly node context, so countries differ mainly through need, position and
pathway. Market prize >= 0.6: 4,734 of 12,045 producing nodes (median 0.54), the same on both runs (the demand
series did not change).

Since 2026-10-30 this is the **market family** of the prize the choice reads, `strategic_industrial_value`
(section 25): on the current run 3,044 of the 7,554 assessed producing nodes (40%) have a high strategic industrial
value; 39 producing nodes or layers are capped as market-only (neither industrial commons nor future option reaches
0.5).

## 4. New Supply-chain Need definition

How costly or dangerous is dependence on external supply for this node? `supply_chain_need` (0-1, producing stages
only) is the within-node percentile across economies of the mean of four families: import dependence (net imports /
GDP; fuel self-sufficiency), own-import supplier concentration (HHI), risky-supplier exposure (China and opposite-bloc
import share), and lack of trusted alternatives (1 - trusted suppliers' world share less the economy's own; observed
for 94% of assessed cells, above the 80% line for scoring). A self-supply guard zeroes the composition families for an
economy holding 10% or more of world supply. Substitutability, reconstitution time, inventories, systemic essentiality
and operational control stay Secure-engine inputs and drawer subcomponents. High need is a percentile of at least 0.67
**and** a need score of at least 0.10 (since 2026-10-30, section 22). Measured for 7,554 of 11,770 producing nodes;
high need 2,386 (current run; 7,146 and 2,326 on the old data, before Solar and Nuclear Extraction had a basket).

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
Germany and France with Green Hydrogen only). These figures hold on the current run: deployment need was not rebuilt
(scripts/95 output of 2026-10-29), and Accelerate (514), Deployment Watch (1,169) and Deployment not assessed (671) are
unchanged. Accelerate now carries its own policy design, `deploy_accelerate` (section 24).

## 6. Secure logic

Step 1 of the choice: if the posture engine finds an acute vulnerability (Test 1 consequence AND one of three routes:
operational / control risk, a coercive chokepoint that needs concentration plus material import dependence, poor
substitution, long reconstitution or weak trusted alternatives, or systemic resilience for long-lead essential nodes),
the node is Secure whatever its need, prize or position, with `security_rationale`, `delivery_mode` and the mitigation
text. Current run: 206 Secure nodes: systemic resilience 175, coercive chokepoint 27, operational control 4; delivery
allied 104, mixed 60, diversified global 42. Only 191 of 2,386 high-need producing nodes (8%) are Secure: Secure does
not absorb high need (15 Secure nodes have need below the threshold). Two-leg run (old data): 174 Secure (151 / 18 /
5), 163 of 2,326 high-need nodes (7%). The rise to 206 comes from the data (section 21), not the method: the engine
did not change.

Secure by rationale and stage (`secure_by_rationale_stage.csv`, current run): systemic resilience is Extraction 86,
Refining 62, Assembly 24 (Grid equipment), Components 3; coercive chokepoint is Refining 15, Extraction 12 (Gas
Extraction); operational control is Refining 4 (the dual-use reading, EV-minerals and nuclear refining). By node: Oil
Refining 42, Gas Extraction 41, Oil Extraction 32, Coal Extraction 25, Electric Grid Assembly 24, Gas Refining 19,
EV-minerals Refining 17, Nuclear Components 3, Nuclear Refining 3. Systemic-resilience Secure is mostly fossil-fuel
extraction and processing; the equipment-resilience route reaches only Grid Assembly (transformers, power electronics
elsewhere are not reached). Every Secure variant now carries a high fence (section 24). Secure does not move with the
need, prize, position or deployment thresholds below (it is the engine's override; its own rule sensitivity is in
`posture_threshold_sensitivity.csv`).

## 7. Preserve / Catch Up / Partner decision logic

At the producing stages, after Secure: need or prize missing -> not assessed. Then the large-yard case (section 25),
after a top-quarter position (`position_first`): TRUE -> Watch. Otherwise, with high need = `supply_chain_need >= 0.67`
(and a need score of at least 0.10), high prize = `strategic_industrial_value >= 0.6` and not capped as market-only
(`industrial_opportunity >= 0.6` until 2026-10-30), strong position = Competitive Position in the top quarter (>= 0.75)
among the node's contenders (economies with >= 0.5% of world supply, measured) AND the engine's capability floor
(moderate or strong):

| | strong position | else credible pathway | else high need | else |
|---|---|---|---|---|
| high prize | Preserve | Catch Up | Partner | Watch (R&D option value) |
| low prize, high need | Preserve + `review_strong_position_low_prize` | | Partner | |
| low prize, low need | Watch | | | Watch |

Credible pathway, current rule (`pathway.rule: anchor`, user spec 2026-10-30, section 20): at least one capability
anchor (direct supply or 2022-24 export share >= 0.5%, GCIM operating capacity >= 0.5% or committed pipeline >= 1% of
world operating capacity, an adjacent stage with >= 5% of world supply, at Extraction a resource base) and at least one
more signal from a different evidence family (another anchor, or feasibility, input cost, relative adjacency, learning
inputs, announced capacity >= 2%). Extraction also needs a resource base. Earlier rules (design D3: any one of five
legs; the morning of 2026-10-30: at least two) are kept as `pathway.rule: legs` for the sensitivity. Need or prize
missing -> not assessed. An unmeasured Competitive Position (every Recovery & Recycling node, and a few cells
elsewhere) counts as not strong, so Recovery is never Preserve; where the rule reads the position it lowers
confidence like an unknown pathway, and the reason says "Competitive Position not measured" (317 nodes on the current
run).

Threshold sensitivity, current run (`choice_threshold_sensitivity.csv`; one threshold moved at a time; nothing was
tuned; Secure 206 and not assessed 4,887 in every row):

| parameter | value | Preserve | Catch Up | Partner | Accelerate | Watch | nodes changed |
|---|---|---|---|---|---|---|---|
| base | | 103 | 355 | 1,474 | 514 | 6,585 | 0 |
| need | 0.60 / 0.75 | 112 / 99 | 355 / 355 | 1,771 / 1,124 | 514 | 6,279 / 6,939 | 306 / 354 |
| prize | 0.55 / 0.65 | 113 / 89 | 375 / 300 | 1,471 / 1,487 | 514 | 6,558 / 6,641 | 30 / 69 |
| CP bar | 0.67 / 0.85 | 135 / 63 | 326 / 393 | 1,472 / 1,474 | 514 | 6,584 / 6,587 | 32 / 40 |
| deployment | 0.55 / 0.67 | 103 | 355 | 1,474 | 669 / 372 | 6,430 / 6,727 | 155 / 142 |

The prize threshold is no longer a knife-edge: 0.60 -> 0.65 changes 69 nodes and cuts Catch Up from 355 to 300 (on
the two-leg run it changed 339 and cut Catch Up from 748 to 422; under the one-leg rule 815, 1,660 -> 858). The
strategic industrial value spreads countries and nodes more than world demand growth alone, the market-only cap holds
whole nodes below the line at every threshold, and the large-yard case removes the commodity nodes before the prize is
read. The pathway rule is now the larger lever (`catch_up_rule_comparison.csv`, current run): one leg 1,952 Catch Up,
two legs 928, the anchor rule (used) 355.

Two-leg run (old data; superseded): base 86 / 748 / 1,850 / 514 / 5,457; need 0.60 / 0.75 changed 396 / 487; prize
0.55 / 0.65 changed 58 / 339; CP bar 28 / 34; deployment 155 / 142. The credible-pathway rule was then decided at two
legs (748 Catch Up nodes; one leg kept 1,660, of which the adjacent-stage leg alone carried 748; one leg except the
adjacent stage, not adopted, 912).

### Two-leg pathway (2026-10-30 morning; superseded by section 20, figures from the two-leg run)

Commit 7d847ba sets `decision_framework.pathway.min_legs: 2` (`config/strategic_position.yml`,
`R/strategic_position/pathway.R`). One leg fired with other evidence observed is now `pathway_credible` "no" with
`pathway_basis` "<leg>;too_few_legs"; every observed leg fired but fewer than two observed is "unknown" with
"<legs>;too_few_observed" (no classified node is in that state on this run). Extraction still needs a resource
base. The choice reason names the shortfall, e.g. "only one piece of pathway evidence (adjacent stage) where two are
required: Partner with trusted suppliers." Scripts 26, 27 and 91 were rerun; no input data changed.

| rule | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| one leg (2026-10-29) | 174 | 86 | 1,660 | 1,592 | 514 | 4,803 | 5,295 |
| two legs (2026-10-30 morning) | 174 | 86 | 748 | 1,850 | 514 | 5,457 | 5,295 |

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
At producing stages Watch means "domestic industrial priority low", with reasons (the large-yard case, low need, low
prize, ample diversified supply, weak domestic case, option value). On the current run 2,133 of the 5,416
producing-stage Watch nodes are decided by the large-yard case (1,253 of them with a capability floor, variant
`deploy_floor`), the other 3,283 by the triage. Policy-design variants: Accelerate `deploy_accelerate` (a medium,
permeable fence, section 24); Watch `deploy`, `deploy_floor` or `deploy_diversify` (low fences).

## 9. Policy Alignment logic

Observed policy (NIPO six-stage support and protection, IEA PAMS) is compared with the choice and never feeds it:
Secure + weak response -> under-prioritized; Preserve / Catch Up + weak capability-building support ->
under-prioritized; Partner + protection in the top third -> misaligned (wrong treatment); Deploy · Accelerate + weak
deployment policy -> deployment gap; Deploy · Watch + heavy protection or local-content rules -> over-prioritized;
sources disagreeing -> mixed; no data -> insufficient data. Where NIPO observes a node (a protection stock is
recorded, zero included), protection alone decides over-prioritization. Only where NIPO does not observe the node at
all does a fallback judge a Watch on total policy attention in the top third (design D5), and that verdict is always
low confidence. Alignment confidence high / medium / low from source coverage. Results, current run
(`choice_policy_alignment.csv`): aligned 5,826; under-prioritized 310 (Catch Up 176, Secure 104, Preserve 30);
deployment gap 182; over-prioritized 679; misaligned 59 (Partner, all high confidence); mixed 233; insufficient data
6,835 (4,887 of them not assessed). Confidence is high for 2,096 nodes, medium for 1,350 and low for 10,678 (one source
or none). Two-leg run: aligned 5,486, under-prioritized 516, deployment gap 182, over-prioritized 503, misaligned 76,
mixed 220, insufficient data 7,141.

Watch nodes with high policy intensity (`watch_high_policy_intensity.csv`, current run): 1,412 of 6,585 Watch nodes
carry top-third policy intensity (priority, protection or deployment localisation). 679 are judged over-prioritized,
by basis:

| basis | over-prioritized Watch nodes | alignment confidence | two-leg run |
|---|---|---|---|
| NIPO protection in the top third (producing stages) | 151 | high 148, medium 3 | 122 |
| the equipment's local-content protection (Deployment & Use) | 64 | medium | 65 |
| total-attention fallback: total policy attention in the top third, NIPO does not observe the node | 464 | low | 316 |

So 215 rest on observed protection or local content and 464 on the attention fallback. The rise from 503 follows the
Watch count (5,457 -> 6,585): the large-yard case sends commodity Extraction and Refining nodes to Watch, and they keep
their observed policy attention. Examples on observed protection (`decision_review.md`, current run): Germany Solar
Components (protection 0.99, need 0.19, a market-only prize); Korea and UK Oil Refining (protection 0.99, large
yard); US Solar Refining (protection 0.96, large yard); Brazil Electric Grid Assembly (protection 0.99, weak domestic
case); Japan Solar Components (protection 0.94); and at Deployment & Use France Wind and Batteries (local content 0.99
/ 0.94, deployment need 0.34 / 0.39) and US Nuclear (local content 0.81, deployment need 0.60, just below the line).
Until the 2026-10-29 review fix, 100 Watch nodes whose NIPO protection stock is recorded (98 at zero) were
over-prioritized on total attention because their economy protects too few nodes to rank; they are aligned or mixed
since.

## 10. Distribution of all choices by country

Current run (e41cb5b), all nodes: Secure 206, Preserve 103, Catch Up 355, Partner 1,474, Accelerate 514, Watch 6,585,
not assessed 4,887 (two-leg run: 174, 86, 748, 1,850, 514, 5,457, 5,295). By stage:

| stage | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| Extraction | 98 | 4 | 0 | 114 | - | 1,411 | 727 |
| Refining & Processing | 81 | 10 | 71 | 175 | - | 1,497 | 520 |
| Component Manufacturing | 3 | 34 | 109 | 555 | - | 1,142 | 511 |
| Final Assembly & Equipment | 24 | 55 | 154 | 554 | - | 1,056 | 511 |
| Deployment & Use | - | - | - | - | 514 | 1,169 | 671 |
| Recovery & Recycling | 0 | 0 | 21 | 76 | - | 310 | 1,947 |

Per economy (214, of 66 nodes): median 0 Secure, 0 Preserve, 0 Catch Up, 6 Partner, 1 Accelerate, 32 Watch, 21 not
assessed; 97 economies have a Secure node, 31 a Preserve node, 64 a Catch Up node, 129 an Accelerate node (two-leg run:
83, 25, 112, 129). Every economy has at least 18 not-assessed nodes: the same 18 nodes have no measured supply-chain need
anywhere (the Geothermal and Green Hydrogen chains, Wind Extraction, and Recovery of Coal, Gas, Electric Grid, EVs,
Nuclear, Solar and Wind); 20 before Solar and Nuclear Extraction had a basket. All economies:
`diagnostics/strategic_position/choice_by_country.csv`.

| economy | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| United States | 1 | 8 | 8 | 4 | 2 | 25 | 18 |
| Japan | 4 | 2 | 12 | 3 | 5 | 22 | 18 |
| South Korea | 5 | 9 | 8 | 5 | 6 | 15 | 18 |
| India | 1 | 8 | 9 | 5 | 9 | 16 | 18 |
| Germany | 4 | 6 | 10 | 3 | 4 | 21 | 18 |
| United Kingdom | 3 | 0 | 12 | 7 | 3 | 23 | 18 |
| France | 2 | 5 | 10 | 3 | 2 | 26 | 18 |
| Australia | 1 | 1 | 3 | 10 | 5 | 28 | 18 |
| Canada | 1 | 2 | 15 | 6 | 1 | 23 | 18 |
| China | 3 | 16 | 2 | 0 | 8 | 19 | 18 |
| Brazil | 2 | 0 | 2 | 4 | 3 | 37 | 18 |
| Italy | 2 | 4 | 10 | 3 | 6 | 23 | 18 |
| Mexico | 0 | 0 | 12 | 7 | 4 | 25 | 18 |
| Indonesia | 2 | 4 | 7 | 9 | 5 | 21 | 18 |

The nine review economies on the two-leg run are in section 7; the path from there to here, step by step, is in
section 27.

## 11. Largest changes from the old methodology

Current run (`decision_review.md`; the 2026-10-28 quadrant recomputed on the current data): 4,216 producing nodes
previously shown as Deploy on insufficient data are now not assessed; Deployment & Use splits into 514 Accelerate,
1,169 Watch and 671 not assessed; insufficient-data nodes become Watch 1,950 (1,271 through Competitive Position and
the pathway, 549 through the large-yard case, 130 through the need) and Partner 508; partner -> Watch 708 (529 by the
large-yard case, 93 by the need); watch -> Catch Up 185 (the strategic industrial value). Drivers over all changed
nodes: not assessed 4,216, deployment need 2,354, Competitive Position and pathway 1,770, the large-yard case 1,131,
the strategic industrial value 574, the need 423. Preserve falls from 274 to 103, Catch Up rises from 115 to 355.

Two-leg run (old data), kept for the record: 4,624 nodes previously shown as Deploy
on insufficient data are now not assessed; Deployment & Use (2,354 nodes, all previously "Deploy" without a decision)
splits into 514 Accelerate, 1,169 Watch and 671 not assessed; 1,553, 413 and 146 insufficient-data nodes are now
Watch, Partner and Catch Up through Competitive Position and the pathway; the cleaned prize moves 329 watch -> Catch
Up, 136 partner -> Catch Up and 119 preserve -> Watch; the cleaned need moves 220 partner -> Watch and 174 watch ->
Partner. Preserve falls from 266 to 86 (top quarter among contenders), Catch Up rises from 117 to 748.

Named examples (two-leg run, market prize): France Nuclear Components and Gas Refining preserve -> Watch (legacy EO 0.93 / 0.89 carried by
capability and cost; world demand growth gives a prize of 0.42 / 0.25); Australia Batteries Components partner -> Catch
Up (prize 0.85; pipeline and adjacent stage); Canada Solar and EV Components watch -> Catch Up; Canada Coal Components
watch -> Partner (need 0.76, prize 0.16); battery-materials refining in the US, Japan, Korea, Germany, UK, France,
Canada and India: Catch Up, not Preserve (8 real contenders; China and Indonesia preserve).

## 12. Remaining consistency flags

Current run (`consistency_before_after.csv`; "before" = the 2026-10-28 quadrant recomputed on the current data):

| contradiction | before (2026-10-28 quadrant) | after (current run) | after (two-leg run) |
|---|---|---|---|
| Partner with CP in the contenders' top quarter | 10 | 3 | 3 |
| Catch Up with CP in the contenders' top decile | 10 | 0 | 2 |
| Catch Up with 10%+ of world supply | 11 | 5 | 6 |
| Preserve without capability | 63 | 0 | 0 |
| Preserve below the contenders' median | 42 | 0 | 0 |
| Partner with cross-country CP >= 0.75 | 415 of 1,521 (27%) | 275 of 1,474 (19%) | 396 of 1,850 (21%) |
| Catch Up with cross-country CP >= 0.9 | 50 of 115 (43%) | 152 of 355 (43%) | 173 of 748 (23%) |

Under the anchor rule every Catch Up node has a capability anchor and none has a cross-country Competitive Position
below 0.25 (section 20), so the Catch Up set sits higher in the cross-country ranking: 43% at 0.9 or above, mostly
contenders below the top quarter of crowded nodes. That is the rule working (a real base, not yet a leading position),
but it reads as "Catch Up despite a strong position" and is the weakest consistency result. Flags on the current run:
`review_strong_position_low_prize` 8 (Preserve: Australia EV-minerals Extraction, Canada and UAE Oil Extraction,
Chile Solar Extraction, India Solar Refining, Japan Coal Components, Nigeria Gas Refining, Oman Oil Components);
`review_top_quarter_below_floor` 4 (Brazil Wind Assembly, Catch Up; India, Thailand and Viet Nam Solar Components,
Partner). A top-quarter score held by an economy below the contender line carries `not_a_contender` in its trace
(25 nodes: 2 Catch Up, 11 Partner, 12 Watch).

Two-leg run, kept for the record: flags `review_strong_position_low_prize` 16 (Preserve; mostly fossil and nuclear incumbents, e.g.
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

(Deployment need was not rebuilt after 2026-10-29, so this table holds on the current run.)

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

Current run (`manufacturing_vs_deployment.csv`; manufacturing = the Components and Assembly choices of the same
technology): 410 economy x technology pairs accelerate deployment while neither manufacturing stage is Preserve or
Catch Up (Partner 177, Watch 135, Secure 9, not assessed 89: the Geothermal and Green Hydrogen chains); 134 the
reverse (deployment Watch, manufacturing Preserve or Catch Up); 104 both. Two-leg run: 385, 173 and 129 (996 both
low). The examples below are from the two-leg run; the current Deployment rows of the review economies are in
`decision_review.md` section 4.
Examples: deploy faster, manufacture elsewhere: Australia Solar, Wind and Grid; Japan Nuclear and Gas; India and UK
Grid; India Coal, Gas and Oil. Manufacture, deployment on track: United States Solar, Wind and EVs (Preserve
manufacturing; deployment need 0.47 / 0.37 / 0.09); Canada Solar, Wind, Nuclear, EVs and Storage (Catch Up
manufacturing; deployment Watch); Korea Storage; France Wind and Storage; Germany Wind.

## 15. Country-page redesign

(The page as built on 2026-10-29. Its Need, Prize & Position and Choices sections were rebuilt around High, Medium
and Low fences on 2026-10-30, section 23; the counts quoted here are from the two-leg run.)

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

(Review of the 2026-10-29 page on the 2026-10-29 and two-leg bundles; the fence-based page of section 23 and the
current bundle are being reviewed in the dashboard copy-refresh work.)

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

**Current state (after the two-leg pathway).**

- **R**: the last full suite ran at 315cff0 (the Yards & Fences merge): **7,027 expectations pass over 47 files; the
  one failure is the known stale `electro_industrial_hs6_dictionary` manifest entry** (below), which predates this
  work. Earlier counts on the way: 6,635 over 44 files at ea6089a (the anchor rule). a54a9d3 added checks to
  `test-posture-export.R` after that run; the suite has not been rerun since, so the count at e41cb5b is not stated.
  New test files since the two-leg build: `test-strategic-value.R` (the strategic industrial value, the large-yard
  case, the pinned outcome on the curated attributes) and `test-yard-rollup.R`; the anchor rule, the need floor and
  the fence-follows-yard check have their own tests in the existing files (sections 20, 22, 24).
- **Dashboard**: being refreshed by the copy-refresh work on the Country page and drawer; results:
  {{DASHBOARD_TESTS}}. At 4c804b6 (the fence-based Country page) the commit records 1,725 tests passing over 114
  files, typecheck, build and budget OK, Country profile first view 804.9 kB of 900 kB.
- **Posture bundle** (e41cb5b, measured on disk): core 2,543,531 bytes (2,484 KiB, schema 2, `git_commit` a54a9d3),
  214 drawer documents, the largest GBR at 139,452 bytes (136 KiB), inside the R export's 160 KiB detail budget. The
  dashboard's own check (`dashboard/src/posture/__tests__/budget.test.ts` at e41cb5b) still caps a drawer document at
  128 KiB, which 42 of the 214 exceed: that limit has to follow the export's in the dashboard refresh.

The rest of this section is the **two-leg build** of 2026-10-30 (posture bundle schema 2 from commit 7d847ba; 214
economies, 14,124 nodes; Country bundle schema 3, 219 fact sheets), after the integration and review fixes. Totals then:
**1,700 dashboard tests and 6,569 R expectations pass; the one R failure predates this build.** The jsdom and browser
checks further down were made on the 2026-10-29 bundle (commit 91dba6f); on the 2026-10-30 bundle the drawer of a
moved node (Australia EV Components) was checked in the browser.

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

Current run unless stated.

- Not assessed: 35% of nodes (4,887 of 14,124), the same 18 nodes in every economy: no supply-chain need is measured
  for the Geothermal and Green Hydrogen chains, Wind Extraction (monazite trades too thinly; user decision 2026-10-30)
  or Recovery of Coal, Gas, Electric Grid, EVs, Nuclear, Solar and Wind (Solar and Wind Recovery have no HS6 code);
  671 Deployment nodes have no deployment need (Oil, Green Hydrogen, EVs in small economies). Solar and Nuclear
  Extraction are assessed since the basket change (section 21), but the large-yard case sends them to Watch almost
  everywhere.
- The market prize is 82% country-invariant world demand growth and mixes a stock (installed capacity for
  manufacturing nodes) with a flow (mineral demand for Extraction / Refining): Solar Refining 0.20 vs Solar
  Components 0.85.
- The strategic industrial value and the large-yard case rest on curated node attributes
  ([yf_node_attributes.md](yf_node_attributes.md)), and an Unknown counts 0. 39 producing nodes or layers are capped
  as market-only, among them 10 of the 12 producing-stage control layers (process knowledge is not assessed for
  software), most Recovery rows and the Solar Components stage row, whose learning attributes are Unknown: a gap in
  the curation as much as a finding about the node. A layer without its own world-supply measure reads its cell's for the diversification
  signal.
- Mineral-node supply shares are weighted over minerals with partial IEA listings; an unlisted economy's share is only
  bracketed (0 to the residual). The contender test and the Catch Up anchors read the measured share (the lower
  bound).
- Thinly traded nodes: most economies import too little to register, so a tiny need score can rank high; the 0.10
  floor (section 22) caps 40 such nodes, 39 of them nuclear.
- Deployment need: WEO demand is national for six economies only (else the 2020-25 trend); no national grid,
  congestion, EV, hydrogen or nuclear targets; no committed pipeline (GEM needs a contact form); Ember capacity ends in
  2024 for some economies; stationary storage installed base for 21 economies; WEO scenario capacity assumes a constant
  capacity factor (inflates Japan's nuclear restarts).
- Policy alignment rests on one source for most nodes (confidence low for 10,678); PAMS is the only deployment-policy
  source; NIPO's support / protection split is sparse, so 464 of the 679 over-prioritized Watch nodes rest on the
  low-confidence total-attention fallback.
- Competitive Position is not measured at Recovery & Recycling (no supply share or capability family for recycling):
  Recovery nodes are decided with the position read as not strong, at lower confidence.
- Licences: Ember data CC BY 4.0; WEO-derived values CC BY-NC-SA 4.0; BNEF values never ship.

## 19. Recommended next refinements

1. Done 2026-10-30: the Catch Up pathway needs a capability anchor plus corroboration (section 20; it replaced the
   two-leg rule of the same morning, section 7). Still open: 303 of the 355 Catch Up nodes carry the direct export
   anchor, and a 0.5% export share is a low bar for a large diversified exporter; 43% of Catch Up nodes have a
   cross-country Competitive Position of 0.9 or more (section 12).
2. Use WEO capacity additions (a flow) for manufacturing nodes so manufacturing and mineral prizes are comparable.
3. Measure supply-chain need for the 18 structurally unassessed nodes (hydrogen, geothermal, recovery flows; Wind
   Extraction).
4. Add national grid, storage, EV and nuclear targets and a committed pipeline to deployment need.
5. Settle the open calibration choices of the Yards & Fences method (section 29): `min_signals` is now the largest
   single lever (4 / 6 change 271 / 159 nodes) and PV modules sit exactly on it. The prize threshold is no longer a
   knife-edge (0.65 changes 69 nodes).
6. Revisit the engine's 10% "strong position" line for large fossil producers and the resilience route for
   equipment nodes (transformers, power electronics), which today only reaches Grid Assembly.
7. Improve the weighted-mineral home share for partially listed refiners (Indonesia, Japan battery-metal refining).
8. Measure recycling capability (secondary output, scrap processing capacity) so Recovery & Recycling has a
   Competitive Position and can be Preserve; today it is read as not strong, at lower confidence.
9. Extend NIPO's support / protection coverage (or add a second protection source) so fewer Watch nodes rest on the
   low-confidence total-attention fallback (464 of the 679 over-prioritized).
10. Fill the Unknown learning attributes that cap the Solar Components stage row and the Recovery rows as market-only.
11. Bring the dashboard to the strategic industrial value and the yards: at e41cb5b the drawer still labels the prize
    "Industrial Opportunity" (it now carries the strategic industrial value), the decoders ignore the yard fields and
    `meta.yf_nodes`, and the posture budget check caps a drawer document at 128 KiB (42 exceed it). In progress in the
    dashboard copy-refresh work.

## 20. Capability-anchor Catch Up (ea6089a, d65ae03)

**Rule** (user spec 2026-10-30, `decision_framework.pathway.rule: anchor`, `R/strategic_position/pathway.R`). Catch
Up needs a high prize, no strong position, at least one **capability anchor** and at least one more signal from a
**different evidence family**. Anchors are absolute measures, never a Competitive Position score:

| anchor | holds when | family |
|---|---|---|
| direct supply share | the measured world supply share of the node (`home_share_exposure`) >= 0.5% | direct |
| direct export share | the 2022-24 benchmark world export share of the node basket >= 0.5% | direct |
| operating capacity | GCIM operating capacity >= 0.5% of world operating capacity (the node's most material product) | capacity |
| committed pipeline | GCIM capacity under construction or built, not yet online, >= 1% of world operating capacity | pipeline |
| adjacent stage share | an adjacent stage of the same technology with >= 5% of world supply (absolute adjacency) | adjacency |
| resource base | at Extraction, recorded reserves or resource above the cell floor | resource |

Supporting signals corroborate an anchor and never qualify on their own: product-space feasibility in the top
quarter, an input-cost advantage, relative adjacency (an adjacent stage among the country's own stronger cells),
curated learning inputs (whose "base" can be feasibility alone), announced or paused GCIM capacity >= 2% of world
capacity. A signal in the anchor's own family does not corroborate it (the two direct shares are one family; relative
adjacency does not corroborate absolute adjacency). Extraction still needs a resource base. `pathway_basis` names the
anchors and supports that fired, or the state: `no_anchor` and `anchor_uncorroborated` (no), `anchor_unobserved`,
`corroboration_unobserved` and `no_evidence` (unknown, read as not credible at lower confidence). A high prize without a
strong position or an anchor, decided Partner or Watch, carries `greenfield_option` (a flag for readers, never Catch
Up). The legs rule (`rule: legs`, `min_legs` 1 or 2) is kept for the sensitivity.

**Why.** Two weak enabling signals let economies with no industrial capability qualify. Under the two-leg rule
feasibility and learning fired together for big diversified economies (508 of 748 Catch Up nodes), and adjacency plus
input cost carried others, so Catch Up said "the product space is near" rather than "there is a base to build on".

**What moved** (old data, the inputs of the two-leg run; scratch comparison of 2026-10-30): Catch Up 748 -> 357,
Partner 1,850 -> 1,996, Watch 5,457 -> 5,702; nothing else changed. 456 nodes left Catch Up (164 to Partner, 292 to
Watch), every one with basis `no_anchor`; their two-leg evidence was mostly feasibility + adjacent stage + learning
(149), adjacent stage + input cost (136) and feasibility + learning (86). 65 nodes became Catch Up (47 from Watch, 18
from Partner): one-leg nodes of the old rule that have an anchor, e.g. Chile and Argentina battery and EV-minerals
Extraction (resource base and direct supply), Australia EV-minerals Extraction, DR Congo battery refining (operating
capacity). Economies with any Catch Up node: 112 -> 70; Batteries Refining 75 -> 35. Catch Up with a cross-country
Competitive Position below 0.25: none; without an anchor: none. Of the nine review economies only India Nuclear
Refining, the UK's Batteries Assembly and Wind Refining and Australia's Batteries Components left Catch Up (all to
Partner); Australia gained EV-minerals Extraction and the UK one node from Watch. Confidence was unchanged (high 6,437
-> 6,436).

**Bug found and fixed (d65ae03).** `home_share` gives an economy that a partial listing omits the listing's residual
as an upper bound: every unlisted economy held 0.87% of battery refining, which made Afghanistan, Andorra, Bermuda and
150 other small economies Catch Up there on a "direct supply share" anchor. The direct supply and absolute-adjacency
anchors read the measured share (`home_share_exposure`), as the contender test does; the legs rule keeps its
historical input so its counts reproduce. The figures above are after the fix.

**Current run** (`catch_up_rule_comparison.csv`, `catch_up_capability_bands.csv`, `catch_up_anchor_types.csv`):

| rule | Catch Up | Partner | Watch | nodes changed vs one leg |
|---|---|---|---|---|
| one leg (design D3) | 1,952 | 988 | 5,474 | 0 |
| two legs | 928 | 1,268 | 6,218 | 1,024 |
| anchor (used) | 355 | 1,474 | 6,585 | 1,597 |

| rule | Catch Up | contender CP = 0 | contender CP < 0.10 | contender CP < 0.25 | cross-country CP < 0.25 | CP not measured | without an anchor |
|---|---|---|---|---|---|---|---|
| one leg | 1,952 | 1,175 | 1,369 | 1,503 | 273 | 161 | 1,595 |
| two legs | 928 | 372 | 485 | 605 | 36 | 56 | 591 |
| anchor | 355 | 41 | 75 | 132 | 0 | 21 | 0 |

A contender score of 0 is an economy below the contender line (under 0.5% of the node's world supply measured on its
primary measure), which therefore cannot hold the direct-supply anchor but holds another (export share, capacity,
pipeline or an adjacent stage). Anchors among the 355
Catch Up nodes: direct export share 303, direct supply share 256, adjacent stage share 54, operating capacity 43,
committed pipeline 31, resource base 0 (no Extraction node is Catch Up: every Extraction stage row is capped as market-only, and most are large yards); supports:
learning 323, feasibility 302, relative adjacency 246, input cost 73, announced capacity 10. Catch Up by stage:
Refining 71, Components 109, Assembly 154, Recovery 21. Economies with any Catch Up: 64; Batteries Refining 34.
`greenfield_option`: 2,529 nodes (Partner 792, Watch 1,737).

**Caveats.** The direct family carries most anchors, and 0.5% of world exports is a low bar for a large diversified
exporter, so the large economies keep long Catch Up lists; the anchor removes the economies with no base, not the
breadth of the large ones. Catch Up now sits higher in the cross-country ranking (43% at 0.9 or above), which reads as
"Catch Up despite a strong position" though it is the rule's intent (a base, not yet a leading position). An observed
anchor family that does not fire gives "no" even where another anchor family is unobserved.

## 21. Atlas 2024, the basket change and the trade refresh (56e9271, 42dd358, 20a431e)

**Data.** The Atlas of Economic Complexity 2024 release (Harvard Dataverse "International Trade Data (HS, 92)" V18,
data to 2024; 01a1be7 merged as 56e9271): HS6 exports and HS4 feasibility now read 2024, the Sector bundle takes the
market-share data year from the source, and the stage evidence rows cite those years (20a431e). Trade: the bilateral
dyads and energy trade were combined with the electro-industrial dictionary pull (no new API calls), and scripts/94
re-pulled the 2022-24 benchmark and monthly momentum (120 calls).

**Basket** (user decisions 2026-10-30, `data/reference/energy_hs6_master.csv`): 2844.10 (natural uranium and its
compounds, how most yellowcake ships) moves from Nuclear Refining to Nuclear Extraction (milled at the mine; its
three-stage supply chain stays Upstream); 2506.10 (quartz) and 2616.10 (silver ores and concentrates) join Solar
Extraction, which had no traded basket. Copper stays at Electric Grid only. Wind Extraction stays not assessed
(monazite trades too thinly). Solar and Wind Recovery have no codes.

**What moved** (anchor rule, market prize, old -> new data, before the need floor; scratch comparison of 2026-10-30):

| | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed |
|---|---|---|---|---|---|---|---|
| old data | 174 | 86 | 357 | 1,996 | 514 | 5,702 | 5,295 |
| new data | 206 | 90 | 357 | 2,067 | 514 | 6,003 | 4,887 |

972 nodes changed: not assessed -> Watch 286, Partner -> Watch 216, Watch -> Partner 192, not assessed -> Partner
119, Partner -> Secure 47, Secure -> Partner 21, Watch -> Secure 20, Secure -> Watch 14, and 57 smaller moves. By
node: Solar Extraction 204, Nuclear Extraction 203, Nuclear Refining 43, Oil Refining 38, Gas Refining 37, Gas
Extraction 33. Solar Extraction now has a measured need in 204 economies and Nuclear Extraction in 203 (none
before). In the allied coalition, Niger leaves the partner tier (no longer a major supplier on the new Nuclear
Extraction basket, where Kazakhstan and the United States enter on export share) and the Czech Republic and Israel
gain the supplier route.

**Caveats.** Moving yellowcake exposed the thin-cell problem in Nuclear Extraction and Refining need (section 22). On
the current run Solar Extraction is Watch in 203 economies (202 by the large-yard case) and Preserve in Chile
(flagged), and Nuclear Extraction Watch in 203 (200 by the large-yard case), so the new baskets change need, the Secure engine's inputs and the
coalition more than the choices.

## 22. The need floor (26d0131)

**Rule** (user decision 2026-10-30, `decision_framework.supply_chain_need.score_floor: 0.10`,
`supply_chain_need.R`). A need percentile at or above the threshold (less its borderline band) stands only where the
need score itself reaches 0.10; below it the percentile is capped at 0.61, under the threshold and its band, so the
node reads plainly as low need, not borderline (`supply_chain_need_cap` "score_below_floor"; the uncapped percentile in
`supply_chain_need_uncapped`). The reason says so: "a need score of 0.03, below the 0.10 floor, though it ranks at 0.77
in a thinly traded node".

**Why.** In a thinly traded node most economies import too little to register and score 0, so a tiny positive score
ranks above most of the field: after yellowcake moved, Australia and Canada read 0.77 and 0.75 need on uranium for
need scores of 0.03 and 0.02, and Germany, the UK, Australia and Canada flipped to high need in Nuclear Refining.

**What moved** (re-running the choice on the stored current table with and without the floor): 40 nodes are capped,
39 of which would otherwise be high need: Nuclear Extraction 21, Nuclear Refining 18, EV-minerals Refining 1. Under
the market prize without the large-yard case 36 change choice (Partner -> Watch 32, Preserve -> Watch 4; Nuclear
Extraction 19, Refining 17). Under the current method only 13 change (Partner -> Watch 10, Preserve -> Watch 3),
because the large-yard case already sends Nuclear Extraction to Watch.

**Caveats.** The floor is a calibration choice like the threshold. Capped nodes can still be Catch Up or Preserve on
the prize and position (Australia, Canada, Germany and the UK are Catch Up in Nuclear Refining; France Preserve).

## 23. The Country page by fences (4c804b6)

User requests 2026-10-30 (dashboard; from the commit record, not re-reviewed here). The Strategy Map is colour-coded
only, in NEIS colours, with no letters, arrows or markers, and a legend grouped by fence with counts; the Need, Prize
& Position and Choices sections become three fence sections. The user's fence mapping: **High fence = Secure;
Medium fence = Preserve, Catch Up, Partner, and Accelerate at Deployment & Use; Low fence = Watch.** Each fence section
lists only its own nodes, a top 5 (supply chain and deployment) and closed drop-downs with every node by the variables
behind its score; the Medium fence carries the Prize x Position chart and Prize, Competitive Position, Partner and
Accelerate sub-sections. The Prize x Position chart and the Sector posture scatter take the old QuadrantScatter style;
Allied Strategy lists only the compacts where the economy has a role. At that commit: 1,725 dashboard tests passed (114
files), typecheck, build and budget OK. The same mapping defines the yards (section 25). The dashboard is being
refreshed by the copy-refresh work (section 17).

## 24. Fence heights follow the yard (37f8f13)

**Rule** (user decision 2026-10-30, `config/posture_policy_design.csv`): the policy-design fence follows the yard, as
in the Yards & Fences paper. Every Secure variant is a high fence (the instruments still differ by route); every
Compete variant and Accelerate a medium, permeable fence; Watch a low fence. `policy_design.R::ppd_read_check()`
refuses a fence height that does not follow the yard.

**What changed.** `secure_input` moderate -> high (a high, targeted fence around the input, phased in as trusted supply
qualifies); `secure_resilience` none -> high (a fence that guards the essential supply itself: screening of foreign
control of essential assets, trusted long-term supply, stockholding; never import protection); `compete_leap_ahead`,
`compete_costly` and `compete_partner` low -> moderate; a new `deploy_accelerate` variant for Accelerate (open to every
trusted supplier the build-out needs, with trusted-supplier and security conditions on public deployment support and
connected equipment). Watch at Deployment keeps `deploy`, `deploy_floor` or `deploy_diversify`. No choice changed.
Current run: high fences 206 (secure_resilience 175, secure_input 27, secure_dual_use 4), medium 2,446 (compete 1,932,
deploy_accelerate 514), low 6,585 (deploy 3,512, deploy_diversify 1,789, deploy_floor 1,284).

**Caveats.** A need-only Partner gets a medium fence by the user's mapping (a departure from the paper, section 29).
The resilience fence is a guard on essential supply, not trade protection: the text has to keep saying so.

## 25. Yards & Fences fidelity (315cff0: 50b81e6, 777f6b2, 99528e6)

User spec 2026-10-30, items 1-3, without encoding the paper's placements. Method:
[strategic_position.md](strategic_position.md) sections 2 and 4.

**Node attributes (50b81e6).** Standardization, differentiation and commoditization are curated for all 66 cells and
35 layers (Unknown where the public evidence is thin), with layer learning and option fields filled where evidence
allows; every value has a reason, confidence and source ([yf_node_attributes.md](yf_node_attributes.md)). No value
encodes a yard or the paper's placement.

**Strategic Industrial Value (the prize).** `strategic_value.R::sp_strategic_value()`: market (the market prize,
unchanged), industrial commons (process knowledge, workforce and suppliers, adjacent innovation; yes 1 / partly 0.5 /
no 0 summed over three, Unknown 0) and future option (the four leap tests of a leap candidate, summed over four; 0 for
any other node), weighted 1:1:1 and ranked across the producing cells. Where neither commons nor option reaches 0.5
the value is capped at 0.59 (market-only), at every prize threshold. No Competitive Position, capability, pathway or
need column can enter (`sp_siv_check_inputs()`). Current run: 3,044 of 7,554 assessed producing nodes have a high
value; 39 nodes or layers are market-only.

**The large-yard case.** `sp_large_yard_case()`: ten signals (mature, passive, standardized, substitutable, quick to
qualify, diversified world supply, low learning, low spillover, low differentiation, commoditized; never market
size). Mature and passive are required, at least 5 of the other 8 must hold, and a credible leap or strong industrial
learning (2 of 3 learning points) vetoes it; moderate learning (1 point or more) leaves a capability floor (variant
`deploy_floor`). The rule tests it after Secure and after a top-quarter position (`position_first: true`, the paper's
gate order), before the prize: TRUE -> Watch. A case on exactly `min_signals`, or one signal short, is borderline.
Current run: the case holds for 2,338 assessed producing nodes and decides 2,133 Watch nodes (1,253 with a capability
floor); the rest are Secure (159), a top-quarter Preserve (5) or Watch on the triage (41); 408 nodes are undecided.
Large-yard nodes: Electric Grid Refining (203 economies) and Extraction (201), Solar Extraction (202) and Refining
(201), Nuclear Extraction (200), Coal Refining (198) and Extraction (175), Gas Refining (181) and Extraction (157), Oil
Extraction (165) and Refining (155), Batteries and EV-minerals Extraction (48 and 47, where world supply reads
diversified); among layers battery enclosures, grid cables, PV racking and conventional PV modules (with a capability
floor).

**Layer-first yards.** `yard_rollup.R`: each sensitive layer takes its own choice (engine Secure kept; else its own
value and large-yard case with its cell's need, position and pathway) and its own yard; `sp_yard_rollup()` gives the
cell its `yard_rollup` (small, medium, large, or mixed_high_medium / mixed_medium_low / mixed_high_low /
mixed_high_medium_low) and `yard_children`. Yards follow the user's fence mapping (section 23). Current run, by cell:
small 181, medium 1,809, large 6,051, mixed 1,196 (high/medium 320, medium/low 350, high/low 362, all three 164);
fence scope `extends_to_hardware` on 439 cells, `layer` on 412. Layers (35 per economy, 7,665 rows): small 1,106,
medium 1,724, large 4,022, not assessed 813.

**Review fixes (99528e6).** A cell without need or prize is not assessed before the large-yard case is read; the
market-only cap is a rule passed to the choice (no threshold lifts it); commons and option are scored on the strict
reading; strong learning vetoes the large yard and moderate learning gives the floor; a layer with its own supply
measure reads its own; `position_first` defaults to true; the reproduction diagnostic shows the choice behind each
yard.

**What moved** (`strategic_value_vs_market.csv`, producing stages of the current run, the need floor applied
throughout):

| rule | Secure | Preserve | Catch Up | Partner | Watch | not assessed | changed vs market |
|---|---|---|---|---|---|---|---|
| market prize, no large yard | 206 | 86 | 357 | 2,035 | 4,870 | 4,216 | 0 |
| strategic value only | 206 | 103 | 355 | 2,050 | 4,840 | 4,216 | 231 |
| market prize + large yard | 206 | 86 | 329 | 1,468 | 5,465 | 4,216 | 595 |
| both (used) | 206 | 103 | 355 | 1,474 | 5,416 | 4,216 | 798 |

The 798 moves: Partner -> Watch 567, all by the large-yard case (Electric Grid Refining 68, Coal Refining 67, Electric
Grid Extraction 67, Solar Extraction 66, Solar Refining 66, Gas Refining 46, Coal Extraction 43, Oil Refining 40, Oil
Extraction 33, Nuclear Extraction 30, Gas Extraction 23, Batteries Extraction 9); Watch -> Catch Up 73, Partner ->
Catch Up 24 and Watch -> Preserve 26, all equipment nodes with high industrial commons (Electric Grid Assembly, Nuclear
Assembly, Components and Refining, Gas and Oil Assembly, Coal and Gas Components); Catch Up -> Watch 69, Catch Up ->
Partner 30 and Preserve -> Watch 9 (Batteries and EV-minerals Extraction, Solar Components, Gas Extraction and
Refining, Nuclear Extraction). So Catch Up shifts from mineral extraction and solar components toward grid and nuclear
equipment: the United States goes from 4 to 8 Catch Up nodes (Electric Grid, Gas, Oil and Nuclear Assembly, Nuclear
Components and Refining join; Batteries and EV-minerals Extraction, now market-only, go to Partner) and its Solar
Components go from Preserve to Watch (market-only); Australia's Partner nodes fall from 16 to 10.

Sensitivity (`large_yard_sensitivity.csv`): `min_signals` 4 -> Watch 6,856, 271 nodes change (3,723 large-yard
cases); 6 -> Watch 6,426, 159 change (1,971 cases); `position_last` 5 change; no learning veto 0 change.

**Caveats.**

- The Solar Components stage row is market-only because its learning attributes are Unknown, so the cell reads Watch
  in most economies while its wafer-and-cell and next-generation layers are medium yards (21 cells roll up mixed).
- Three of the eight large-yard signals (standardized, low differentiation, commoditized) are facets of one
  product-market property; grouping the signals by family was not adopted.
- Concentration is one of the eight votes here; in the paper it only adds diversification inside the Large yard.
- Control layers are kept out of the large yard (digitally active), but their prize is low (market-only for 10 of the
  12, below the line for reactor controls), so where coercive control is not measured for the country and need is
  low they are Watch (sections 28-29).

## 26. The posture core carries the Yards & Fences evidence once (a54a9d3)

The Yards & Fences fields took 22 drawer documents over the export's 160 KiB budget (the USA at 162 KiB): every
country repeated the node-level large-yard signals, the commons and option families and each child layer's label and
reason. The core now holds `meta.yf_nodes`, one row per producing-stage node and layer (89 rows, 8.8 KiB: label, the
large-yard case with its signals as indexes into `meta.large_yard_signals`, the strategic-value families that do not
vary by country, each the modal value across countries). The detail keeps the prize the choice read and the market
prize; the large-yard case only where the country's differs from its node's (2 of 55 nodes vary); child layers as
`{layer_id, yard, choice, variant}`, the drawer composing the reason. Measured on the e41cb5b bundle: core 2,484 KiB,
largest drawer document 136 KiB (GBR). `test-posture-export.R` checks the split.

## 27. The rebuild (e41cb5b) and the path from the two-leg run

Pipeline rerun 2026-10-30 in this worktree: the Atlas 2024 release, the basket change, dyads and energy trade combined
with the electro-industrial dictionary pull (0 calls), the 2022-24 benchmark and monthly momentum re-pulled
(scripts/94, 120 calls), then scripts 04 10 11 13 20 15 25 31 32 33, 26 and 27 on a54a9d3, and the exports 91 85 87 88
84. Bundles: posture (schema 2, `git_commit` a54a9d3), strategic, sector, country and AIB; the diagnostics regenerated.

| step | Secure | Preserve | Catch Up | Partner | Accelerate | Watch | not assessed | source |
|---|---|---|---|---|---|---|---|---|
| two-leg run (old data) | 174 | 86 | 748 | 1,850 | 514 | 5,457 | 5,295 | section 7 |
| + capability anchor (old data) | 174 | 86 | 357 | 1,996 | 514 | 5,702 | 5,295 | section 20 |
| + Atlas 2024, basket, trade | 206 | 90 | 357 | 2,067 | 514 | 6,003 | 4,887 | section 21 |
| + need floor | 206 | 86 | 357 | 2,035 | 514 | 6,039 | 4,887 | section 22 |
| + Yards & Fences (current) | 206 | 103 | 355 | 1,474 | 514 | 6,585 | 4,887 | `choice_overall.csv` |

Catch Up / Partner / Watch of the nine review economies along the same path (the second and third steps in between
are in sections 20 and 21):

| economy | two-leg run | anchor, old data | anchor + new data + need floor | current |
|---|---|---|---|---|
| United States | 4 / 5 / 25 | 4 / 5 / 25 | 4 / 9 / 24 | 8 / 4 / 25 |
| Japan | 10 / 10 / 16 | 10 / 10 / 16 | 10 / 11 / 17 | 12 / 3 / 22 |
| South Korea | 10 / 8 / 11 | 10 / 8 / 11 | 10 / 10 / 11 | 8 / 5 / 15 |
| India | 11 / 7 / 12 | 10 / 8 / 12 | 10 / 8 / 13 | 9 / 5 / 16 |
| Germany | 8 / 8 / 20 | 8 / 8 / 20 | 7 / 8 / 21 | 10 / 3 / 21 |
| United Kingdom | 10 / 13 / 18 | 9 / 15 / 17 | 9 / 14 / 19 | 12 / 7 / 23 |
| France | 8 / 7 / 22 | 8 / 7 / 22 | 8 / 10 / 22 | 10 / 3 / 26 |
| Australia | 3 / 17 / 19 | 3 / 17 / 19 | 2 / 16 / 22 | 3 / 10 / 28 |
| Canada | 19 / 7 / 17 | 19 / 7 / 17 | 18 / 8 / 17 | 15 / 6 / 23 |

The full current rows are in section 10. Catch Up nodes of the nine now: the United States Batteries Refining and
Recovery, Electric Grid, Gas, Oil and Nuclear Assembly, Nuclear Components and Refining; Japan Batteries Refining,
Components, Assembly and Recovery, Electric Grid Assembly, EV Components and Assembly, Nuclear Refining and Components,
Solar Assembly, Wind Components and Assembly; Korea Batteries Refining, Components and Recovery, Electric Grid
Assembly, Gas Components, Solar Assembly, Wind Components and Assembly; India Batteries Refining, Components, Assembly
and Recovery, Coal Components, Electric Grid Assembly, Nuclear Components, Oil Assembly, Wind Refining; Germany
Batteries Refining, Components, Assembly and Recovery, Gas Components and Assembly, Nuclear Refining and Components,
Wind Refining and Components; the UK Batteries Refining, Components and Recovery, Electric Grid Assembly, EV Components
and Assembly, Nuclear Refining, Components and Assembly, Solar Assembly, Wind Components and Assembly; France
Batteries Refining, Assembly and Recovery, Electric Grid Assembly, EV Refining, Components and Assembly, Nuclear
Components and Assembly, Wind Refining; Australia Batteries, Nuclear and Wind Refining; Canada Batteries Refining,
Components and Recovery, Electric Grid Assembly, EV Components and Assembly, Gas Components and Assembly, Nuclear
Refining, Components and Assembly, Oil Assembly, Solar Assembly, Wind Components and Assembly. Battery refining and
recycling and grid equipment recur across countries: the strategic industrial value is still mostly node context.

## 28. The paper's placements against the model

`yf_paper_reproduction.csv` (`sp_yf_reproduction()`; the only reader of `paper_placement`, never an input). For each
of the 21 curated Yards & Fences components (magnets count twice, EV and Wind), the yard the model gives the linked
layer in the United States, Japan and Germany, the modal yard over all economies with its share and choice, the
large-yard signal count (of the first country's unit) with its margin from `min_signals`, and the paper's placement:

| component (layer) | USA | JPN | DEU | modal (share, choice) | large-yard signals (margin) | paper | agrees for |
|---|---|---|---|---|---|---|---|
| battery-management firmware, EMS, remote updates (bess-control) | small | small | small | large (58%, watch) | 1 (-4) | small | all three |
| communications gateway, cloud fleet management (bess-comms) | small | small | small | large (58%, watch) | 1 (-4) | small | all three |
| battery cells (bess-cells) | medium | medium | medium | large (63%, watch) | 2 (-3) | medium | all three |
| cathode materials (bess-cathode) | medium | medium | medium | large (56%, watch) | 1 (-4) | medium | all three |
| anode materials (bess-anode) | medium | medium | medium | large (56%, watch) | 0 (-5) | medium | all three |
| power conversion hardware (bess-pcs) | medium | medium | medium | large (52%, watch) | 3 (-2) | medium | all three |
| enclosures, racking, thermal, balance of system (bess-enclosure) | medium | large | large | large (99%, watch) | 6 (+1) | large | JPN, DEU |
| utility inverter firmware (grid-inverter-firmware) | large | small | large | small (44%, secure) | 2 (-3) | small | JPN |
| grid-control software, SCADA / EMS / DERMS (grid-scada) | large | small | large | small (44%, secure) | 2 (-3) | small | JPN |
| large power transformers (grid-transformers) | medium | medium | medium | large (52%, watch) | 1 (-4) | none | - |
| cables and conductors (grid-cables) | large | large | large | large (100%, watch) | 7 (+2) | large | all three |
| rare-earth separation and metal-making (ree-separation) | medium | small | medium | large (57%, watch) | 2 (-3) | small | JPN |
| NdFeB magnets, EV (ree-magnets) | small | small | small | small (50%, secure) | 0 (-5) | small | all three |
| NdFeB magnets, Wind (ree-magnets) | medium | small | small | large (43%, watch) | 0 (-5) | small | JPN, DEU |
| electric drive units and motors (ev-motors) | medium | medium | medium | large (55%, watch) | 3 (-2) | none | - |
| conventional c-Si modules (pv-modules) | medium | large | medium | large (96%, watch) | 5 (0) | large | JPN |
| solar cells and wafers (pv-cells-wafers) | medium | medium | medium | large (57%, watch) | 4 (-1) | none | - |
| next-generation PV (pv-nextgen) | medium | medium | medium | large (57%, watch) | 2 (-3) | medium | all three |
| passive racking and mounting (pv-racking) | large | large | large | large (99%, watch) | 8 (+3) | large | all three |
| reactor control and safety instrumentation (nuc-reactor-controls) | large | small | large | large (63%, watch) | 2 (-3) | small | JPN |
| HALEU enrichment (nuc-haleu) | medium | medium | medium | large (82%, watch) | 1 (-4) | small | none |
| LEU enrichment services (nuc-leu) | medium | medium | medium | large (82%, watch) | 4 (-1) | small | none |

Of the 19 components the paper places, the model agrees for Japan on 17, Germany on 12 and the United States on 10;
on the modal yard over all economies on 7, because most small economies have low need, no position and no pathway,
so most units are Watch there (the paper describes a large advanced economy). Where they differ:

- **Control layers** (grid SCADA, inverter firmware, reactor controls) are Small only where the engine measures
  coercive control of the carrying hardware for the country: Japan, not the United States or Germany. For those two
  the layer misses Secure, its prize is market-only and need is low, so it is Watch. This is the user's decision
  (section 29), a documented departure from the paper.
- **Enrichment** (LEU, HALEU) is Catch Up (medium) in all three: a high strategic industrial value, a position below
  the top quarter and an anchor. The paper places it Small; the engine finds no acute vulnerability (no Secure route
  is met) for these three, so the layer goes to the triage.
- **Battery enclosures** (USA) and **PV modules** (USA, Germany) are Preserve: the layer reads its cell's
  top-quarter Competitive Position, which comes before the large-yard case (`position_first`). PV modules hold the
  large-yard case on exactly `min_signals` (5 of 8, with a capability floor), so `min_signals` 6 would take them out
  of the large yard everywhere.
- **Magnets** are Small in all three for EVs and in Japan and Germany for wind; the United States is Preserve in wind
  magnets. **Rare-earth separation** is Small only in Japan (Catch Up in the United States and Germany).

## 29. User decisions and open calibration choices

Decisions of 2026-10-30, recorded:

1. Catch Up needs a capability anchor plus corroboration from a different evidence family (section 20).
2. Basket: quartz and silver ores at Solar Extraction; yellowcake at Nuclear Extraction; copper stays at Electric Grid
   only; Wind Extraction stays not assessed; Solar and Wind Recovery have no codes (section 21).
3. High supply-chain need needs a need score of at least 0.10 (section 22).
4. The fence mapping: High = Secure; Medium = Preserve, Catch Up, Partner, Accelerate; Low = Watch (sections 23, 25).
5. The policy-design fence height follows the yard (section 24).
6. The Yards & Fences tightening, spec items 1-3: Strategic Industrial Value, the large-yard case, layer-first yards
   (section 25).
7. **Control layers are Small yard only where coercive foreign control is measured for the country.** This is
   country-specific, so grid SCADA, inverter firmware and reactor controls are Small for Japan but Large for the United
   States and Germany: a documented departure from the paper's placements (section 28).
8. **A need-alone Partner sits in the Medium fence** by the user's mapping: a documented departure from the paper,
   where dependence and concentration only add a diversification posture inside the Large yard.

Open calibration choices (each reported, none tuned):

| setting | value | what it does | sensitivity |
|---|---|---|---|
| `large_yard.min_signals` | 5 of 8 | commodity signals needed for a large yard | 4 / 6 change 271 / 159 nodes; PV modules sit exactly on 5 |
| `strategic_value.gate` | 0.5 | commons or option needed to escape the market-only cap | 39 nodes or layers capped |
| `strategic_value.weights` | 1:1:1 | market, commons, option | a neutral prior, not an estimate |
| `large_yard.diversified_top_share_max` | 0.5 | the largest supplier's share for "diversified" | splits Batteries and EV-minerals Extraction (48 and 47 large-yard Watch nodes) |
| `large_yard.position_first` | true | a top-quarter position is read before the large-yard case | `position_last` changes 5 nodes |

The learning veto and floor (2 and 1 points of 3) change nothing on the current run (`no_learning_veto`: 0 nodes).
Still open from before: the Sector profile budget note (lazy-load the posture card or raise its 1.30 MB limit) and
`retain: true` on the `electro_industrial_hs6_dictionary` manifest entry (section 17).
