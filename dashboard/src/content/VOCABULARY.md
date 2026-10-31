# Shared vocabulary: rules for page copy

The single source is `src/content/vocabulary.ts` (NEIS copy refresh, 2026-10-30; editorial guide
`NEIS_dashboard_copy_redraft.txt`). Import the constant; do not retype the sentence. If a page needs a new
shared phrase, add it there first.

## Rules

1. **Question first.** The first screen of every page states its strategic question (`MODULE_QUESTION`) before
   any method. Say why a number matters before how it is calculated.
2. **Plain words in visible copy** (`TERMS`, table below). Technical words stay in tooltips (`InfoTip`, `Term` with
   a glossary id), drawers, Evidence & Methodology, methods pages and advanced views. Do not delete technical
   detail: move it there.
3. **The six choice names do not change**: Secure, Preserve, Catch Up, Partner, Accelerate, Watch
   (`CHOICE_NAME`; `posture/grammar.ts VERB_LABEL` reads it). Explain each as an action (`CHOICE_ACTION`,
   `CHOICE_DEFINITION`). In generated sentences use `CHOICE_PROSE` ("rely on partners for", "leave open"), and
   lead with the 2-4 most important conclusions; do not list every category. "Partner / Diversify"
   (`PARTNER_LONG_NAME`) where diversification is the point.
4. **Yards.** Explain them with `YARD_DEFINITION` (the user's wording) and `YARD_AND_FENCE`. Never imply a whole
   technology sits in one yard (`MIXED_YARD_NOTE`). Control layers are Small yard only where the data show
   coercive foreign control for that country (`CONTROL_LAYER_NOTE`). Fence section titles: `FENCE_TITLE`.
5. **Framing.** No country needs to make everything (`FRAMING`). Dependence is not bad by default; Partner and
   open, diversified supply are legitimate strategies, not failures. Deployment priorities (Accelerate) stay
   separate from manufacturing priorities.
6. **Allied Industrial Base.** Use `AIB_EXPLAINER` / `AIB_EXPLAINER_SHORT` / `AIB_OUTCOME`, and the words in
   `AIB_LANGUAGE` (division of labor, trusted capacity, best-placed producers, single points of failure,
   redundancy, partner demand, co-investment, offtake, export finance...). Make both sides explicit
   (`AIB_SIDES`): "Who has the strongest capability?" and "Which countries need a partner?". A compact always
   answers `COMPACT.questions`.
7. **Labels with the data's codes as keys**: `AIB_CLASS_NAME` / `AIB_CLASS_DEFINITION` (COORDINATE ...),
   `SCENARIO_NAME` / `SCENARIO_DEFINITION` (baseline, managed, decoupling), `SCENARIO_CHART_LABEL`,
   `COMPACT_ROLE_DEFINITION`, `COMPACT_TIER_DEFINITION`, `ALIGNMENT_NAME`, `RATIONALE_NAME`. Do not print the
   all-caps class codes as text.
8. **Missing data is uncertainty, never a call** (`STATE`): "Not assessed", "Not enough evidence"; never Watch or
   "low priority" for a gap in the data.
9. **Do not overclaim.** Every sentence must be true of the data shown. Decoupling removes China in this build
   (`SCENARIO_DEFINITION.decoupling` says so). Deployment Watch means need below the bar, not "on track".
10. **Tone.** Plain English, short sentences, active voice; US spelling (division of labor, localization). Avoid
    "the model", jargon, unexplained acronyms, and `AVOID_WORDS` (optimize, leverage, unlock...). Tests can
    assert `not.toMatch(AVOID_RE)`.
11. **Legacy OSI** stays clearly subordinate (`LEGACY_OSI_NOTE`); its corner names never appear on current
    strategy pages.

## Terms (technical -> visible copy)

| Technical | Say instead |
|---|---|
| node | supply-chain step / capability |
| node class | what this step needs |
| coverage | share of allied demand that can be met ("coverage" only as a short chart label) |
| static / existing-response / optimized coverage | supply that remains available without a new response / that existing partners can replace / that allies could replace if they coordinate |
| residual gap; residual uncovered nodes | remaining supply gap; remaining allied gaps |
| shock; shock exposure | stress test / disruption scenario; supply exposed to the disruption |
| candidate partner(s) | potential partner; ranked: best-placed partners |
| demand scope | countries whose supply needs to be replaced |
| investment host; marginal investment | country that could add capacity; where new capacity would help most |
| frontier | trade-off between resilience and concentration |
| coordination dividend | extra supply allies can recover by acting together |
| minimum coalition | smallest group of partners that meets the resilience target |
| coalition tier | role in the compact |
| candidate compact; compact unnecessary | potential Sectoral Compact; no compact needed |
| fragmented action | today's uncoordinated response |
| allied demand affected; restored by trade; left for new capacity | demand that must find a new supplier; recovered through trade; gap that requires new capacity |
| policy need | action needed |
| physical-capability warning | production evidence is weak |
| coalition coverage | where allies are strong |
| role allocation | who should build what |
| high strategic-need score | supply-chain vulnerability |
| unexplained percentile | strong competitive position (percentile in the tooltip) |
| strategic posture (classification) | strategic choice; "What should this country do?" |
| Under-prioritized / Over-prioritized / Wrong treatment / Mixed / Insufficient data | Needs more attention / Too much intervention / Wrong tool for the problem / Mixed signals / Not enough policy evidence |
| Operational control / Coercive chokepoint / Systemic resilience | Control-system risk / Chokepoint risk / System resilience |
| COORDINATE / BUILD / MANAGE SPECIALIZATION / LOW INTERVENTION / INSUFFICIENT DATA | Coordinate trade / Build capacity / Manage specialization / Low intervention / Not enough evidence |
| Baseline / Decoupling | Today's network / Major-supplier disruption |

Already wired: `posture/grammar.ts` (choice names and actions, fence names, alignment and rationale labels, and
the capability-anchor pathway codes in plain words) and `posture/yards.ts` (yard names) read the vocabulary, so the
Country, Sector and AIB pages show these words. The glossary (`content/glossary.ts`) has an entry for each choice,
fence and AIB term: link them with `Term` / `InfoTip` ids (`choice_partner`, `high_fence`, `allied_coverage` ...).
Not yet wired (page owners): AIB class chips (`aib/derive/classes.ts`), scenario labels
(`aib/derive/scenarios.ts`), chart labels, and page ledes.
