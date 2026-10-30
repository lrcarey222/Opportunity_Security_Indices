# Decision framework: implementation plan (Phase 1 audit, 2026-10-29)

The canonical framework is Need -> Prize -> Position -> Choice -> Policy alignment. The legacy OSI framework
(Energy Security Index, Economic Opportunity Index, Strategic Index, Grow / Upgrade / Exposed / Stable corners)
stays for replication and comparison only. This note records what the audit found and what changes. The
method itself is documented in [strategic_position.md](strategic_position.md); the Part XXI validation is
`diagnostics/strategic_position/decision_review.md` (scripts/27).

## Reused as is

- **Secure override:** the three routes (operational control, coercive chokepoint, systemic resilience),
  their per-node rules in `config/security_route_rules.csv`, `security_rationale`, and the mitigation and
  delivery text.
- **Competitive Position:** seven families of demonstrated capability, the 2022-24 Comtrade benchmark, and
  trade momentum blended into the export family at 0.15.
- **Supply-chain need families:** import dependence, own import HHI, China / opposite-bloc exposure;
  mid-rank percentiles; entrepots excluded.
- **Engine outputs that already match the spec:** `delivery_mode` (same vocabulary), partner
  recommendations and the AIB division of labour, the borderline machinery (`posture_possible`,
  `pivotal_inputs`) and `posture_confidence`.
- **Policy as a downstream comparator:** Revealed Policy Priority (NIPO six-stage plus IEA PAMS) never feeds
  the choice.
- **Explorer pieces:** the six-stage posture matrix, cell drawer, policy-alignment border and evidence
  opacity, and the partner and AIB data and sections.

## Refactored

- **Prize:** `industrial_opportunity` was the legacy EO re-ranked among a node's contenders. Within a node,
  EO ranks countries mainly by country input cost (Spearman 0.77) and capability (0.57), so it partly
  double-counts Competitive Position. It becomes a node market prize (world demand growth 2024-2035 under
  the WEO 2025 / Critical Minerals Stated Policies scenario, global manufacturing capex momentum at weight
  0.25; maturity as context; a declining market capped), lifted by national home-market growth only where
  real national data exist (never lowered by a small home market). The old column is renamed
  `legacy_eo_opportunity`; the hand-typed market-outlook list is replaced by data.
- **Catch Up pathway:** feasibility (Atlas product space), the CIM investment pipeline, adjacent established
  stages, curated learning inputs and input-cost advantage are moved out of the Prize. They become the
  credible-pathway test for Catch Up (`pathway_credible`, `pathway_basis`; unknown counts as not credible).
- **Need:** `supply_chain_need` at the producing stages (the three families plus trusted alternatives, with
  the engine's self-supply guard on the import-composition families); no Assembly proxy at Deployment.
- **Choice logic:** Need x Prize triages and Competitive Position decides the response: the top quarter of
  the node's contenders with the capability floor -> Preserve; a credible pathway -> Catch Up; otherwise
  Partner (high need) or Watch. Strong position with high need and a low prize is Preserve flagged for
  review, not Partner; a top-quarter score below the capability floor is flagged too.
- **Deployment & Use:** its own `deployment_need`, built from three families: demand adequacy, policy /
  target gap (Ember 2030 targets and approved plans) and system need (fuel-import displacement, flexibility,
  firm capacity). Results are Deploy . Accelerate (deployment need at or above 0.6, about the top 30%) or
  Deploy . Watch, never Partner. Fossil technologies are assessed the same way as clean ones.
- **Unassessed cells:** 60% of producing cells had no need or prize value and were silently shown as
  Deploy. They are now labelled not assessed (39% of producing cells, 4,624 of 11,770, mostly thin need cells at Recovery and
  Extraction).
- **Policy alignment:** adds misaligned (wrong treatment), a deployment gap (under-response) and an
  alignment confidence; over-prioritization is judged on protection / localisation where measured.
  Deployment cells are no longer "aligned" by rule.
- **Allied strategy:** partner recommendations also for Secure nodes that rely on trusted foreign capacity;
  an AIB status (covered / gap) with the gap reasons the data support (no trusted supplier, a single
  credible supplier, weak evidence, insufficient allied capacity, excessive concentration).
- **Legacy names:** the Country fact sheets' `quadrant` values grow / upgrade / secure / watch become
  `legacy_corner` values `legacy_*`, so "secure" and "watch" mean only the new choices; the 2026-10-28 need x
  EO quadrant survives only as `legacy_quadrant_*` columns.
- **Explorer bundle:** posture bundle schema 2 (choice, modes, need type, relevant need, prize, contender
  CP, pathway, flags, evidence words, alignment confidence, thresholds, stage display names, node phrases;
  per-country detail with the five drawer questions and a compact trace).

## Where current outputs conflicted with the spec

- **Position:** it never chose the response; it only raised flags (25% of Partner cells "despite
  capability", 42% of Catch Up cells "already leading").
- **Deployment:** it borrowed the Assembly import need, and its policy alignment was always "aligned".
- **Country page:** it presented the legacy corners, the Strategic Index priorities and the Perspective dials
  as co-equal with the posture. These move to Evidence & Methodology -> Legacy OSI.

## Order of work

1. **Methodology:** stage the Ember data; build deployment need and the national home market (scripts/95);
   build the clean Prize and the pathway test; apply the revised decision and alignment rules; namespace
   legacy outputs; rerun scripts 26 -> 27 -> 91. Commit.
2. **Country page:** strategy header and sentence, Country Strategy Map, Need, Prize & Position, Strategic
   Choices, Policy Alignment, Allied Strategy, Evidence & Methodology, the five-question drawer, and
   cross-navigation. Consistent wording on the Sector and AIB posture cards, the glossary and the Methods
   page. Commit.
3. **Validation and render review:** Part XXI checks in scripts/27 and the readout; desktop, laptop, tablet
   and phone checks; final report in `docs/decision_framework_report.md`.

## Calibration choices made during implementation (2026-10-29)

| setting | value | why |
|---|---|---|
| `prize_threshold` | 0.6 | design start value; 39% of producing cells at or above it; sensitivity 0.50-0.70 in `choice_threshold_sensitivity.csv` |
| `deployment_threshold` | 0.6 (design start 0.67) | deployment need is a mean of family percentiles, narrower than a percentile: 0.6 takes about 30% of assessed Deployment cells, the selectivity of need at 0.67; 0.67 took 22% and left the US, Germany and France with no Accelerate |
| `investment_momentum.weight` | 0.25 (first draft 0.5) | the capex trend is observed for ten nodes only; at 0.5 the post-2022 EV-manufacturing capex dip outweighed a tripling of EV demand |
| `home_market_mode` | lift | the user's "lifted by national home-market growth": a small home market does not lower a world-market prize |
| self-supply guard on need | home share >= 10% | the engine's self-supply line; a large producer is not dependent on the composition of its small imports |
