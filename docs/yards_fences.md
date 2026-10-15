# Draw the Fence: the Yards & Fences decision tool

*Draw the Fence* is a module of the Allied Industrial Base Explorer (`dashboard/`, routes under
`/yards-fences`). It turns the NEIS Center paper *Clean Energy Industrial Strategy: Three Yards for
Three Different Problems* (discussion draft, October 2026) into an interactive policy lab:

> OSI tells you what the world looks like. Yards & Fences helps you decide what to do about it.

The tool runs **Problem → Evidence → Yard → Fence → Policy → Metrics → Reassessment**. It never runs
technology → score → label: there is no aggregate score, and no headline index is mapped to a yard.

## Pages

| Path | What it does |
|---|---|
| `/yards-fences` | The framework in one screen: three yards as three problems, the sequence, the examples |
| `/yards-fences/assess` | The guided assessment: what is assessed → what the data tell us → Gate 1 (acute harm) → Gate 2 (2A control layers, 2B physical chokepoints) → Gate 3 (capability; preserve / catch up / leap ahead) → Gate 4 (cost of protection). Analyst and Quick modes share one state and engine |
| `/yards-fences/result` | The result: yard, posture, confidence, evidence completeness, why (and what did *not* drive it), the decision tree, evidence gaps, what would change the answer, the policy posture, the curated notes and every input with its provenance. For a system, the component stack first. Print / Save as PDF |
| `/yards-fences/scenario` | Test another world: change observed data or assumptions; baseline and scenario side by side; the change of yard explained by the gate that changed |
| `/yards-fences/fence` | Build your fence: 13 instruments × Off/Low/Medium/High and the scope of security measures; rule-based findings (under-fencing, over-fencing, mismatch, closed gate, no exit, …), the two fences drawn side by side, and how we will know it worked |
| `/yards-fences/diversify` | Can trusted diversification solve the problem? Current supply, home import sources, the paper's allied network design for the stage (with Partnership Strength), and three strategies compared |
| `/yards-fences/examples` | Six one-click assessments, computed live |
| `/yards-fences/methodology` | The rules (rendered from `engine/rules.ts`), the data and vintages, the build's validation, the metric map, the curated table |

The whole assessment travels in the URL (`?a=`, base64url JSON), so *Copy assessment link* reproduces
it exactly. Nothing is stored anywhere else.

## Architecture

```
config/yards_fences_metric_map.yml          which published measures become evidence; labels; engine inputs; stacks
data/reference/yards_fences_assessments.csv the curated component table (what OSI cannot measure)
R/dashboard/yards_fences/yf_build.R         pure helpers (no IO)
scripts/89_export_yards_fences_data.R       IO: reads the published bundles, writes dashboard/public/data/yards-fences/
dashboard/src/yards/
  engine/        the decision engine: pure TypeScript, no IO, unit-tested (engine.test.ts)
  data/          contract.ts (the bundle's shape), store.ts (fetch + cache), adapter.ts (evidence -> engine inputs)
  state/         assessment.ts (state + URL codec), YardsContext.tsx (provider, memoised engine runs)
  components/    kit (yard glyphs, provenance badges, EvidenceMetric), Controls, EvidencePanel,
                 ResultBlocks (decision tree, gaps, change conditions), ComponentStack, FenceDiagram
  content/       examples, policy toolkits
  views/         one lazy route chunk per page
```

### Data build

```bash
Rscript scripts/89_export_yards_fences_data.R                 # after scripts/85 and 87
Rscript scripts/89_export_yards_fences_data.R --check-only    # every check, nothing written
Rscript scripts/89_export_yards_fences_data.R --processed-dir <dir> --data-dir <dir> --country USA
```

Like the Country export, scripts/89 reads the **published** Sector and Strategic bundles rather than
recomputing from raw tables, so every number in the tool is the number Sector and Strategic Insights
show. It adds the allied network outputs (`data/processed/outputs/allied_network_*.csv`). It writes
`manifest.json` (always revalidated), `reference.<h8>.json` and `evidence-<ISO3>.<h8>.json`, and
stops on any failed check:

* the Sector bundle excludes restricted (BNEF, BCG) data;
* the Strategic bundle is the one the Sector bundle was built next to;
* every component's evidence scope has records; every stack layer is a curated component;
* every raw record carries a source and a year;
* no normalised OSI component feeds the engine; no headline index is used as evidence.

Evidence is measured at three scopes, and every record says which: the **technology-stage** HS6
basket, an **HS6 sub-component** (for example Battery Cells, HS 8507.60; Permanent Magnets, HS 8505.11)
or a **mineral** (IEA refining and mining, USGS reserves). A component with no measured scope
(firmware, software, services) gets no supplier-share data at all: technology-stage numbers are shown
as context, labelled "whole stage, not this component", and never fed to the engine. Where a source
does not list the home economy (for example IEA manufacturing capacity, where the US is "not
quantified"), the share is written as unknown, never 0, and the engine reads the home share from the
next measure that lists it.

### The engine

Inputs carry provenance: `data`, `assessment` (curated), `judgment` (the reader), `scenario`,
`unknown`. Every test is tri-state (yes / no / unknown).

* **Small Yard** = acute consequence (high or severe) AND either
  * **A**: a control layer an outsider could command, update, disable or read (harm within months), or
  * **B**: a physical chokepoint, where all three hold: a potentially coercive supplier holds at least
    50% of world supply (Matisek's screening line), substitution is hard, and trusted supply could not
    be restored (qualification time if trusted producers can cover the loss, else the longer of
    qualification and reconstitution) before harm becomes intolerable plus stockpile cover.

  Separability sets the scope of the fence (control layer only / plus integrated hardware / whole
  product / unclear). It does not decide whether there is a fence. When both paths hold, the result
  keeps both problems (`small-both`: secure the control layer *and* remediate the chokepoint).
  Values the reader or a scenario supplies are described as assumed, never as the data supplier's
  observed share.
* **Medium Yard** (if not small) = present position (home share ≥ 10%, or ≥ 3% backed by
  complementary capabilities), industrial learning (process knowledge, skills and suppliers, adjacent
  innovation) or a credible leap-ahead case (an advantage, evidence beyond novelty, and two of:
  displaceable incumbent, complementary capabilities, path to scale). A strong case (strong learning,
  or two rationales) is medium; a single moderate rationale is medium unless the domestic premium
  exceeds 30%.
* **Large Yard** otherwise; a moderate case with a high premium is *large with a specified capability
  floor*; concentrated supply adds diversification to the posture, never a high fence.

**Unknowns are never filled.** `classify()` evaluates the gates at the four corners of the unknowns
(security inputs at their open or security end × capability inputs at their weak or strong end). If
the corners agree the yard is settled; otherwise the result is *indeterminate*, with the yards still
possible and the *pivotal* gaps that would settle it. **Confidence** (low / moderate / high: marginal
answers near thresholds, low-confidence assessments, completeness) is reported separately from
**evidence completeness** (share of relevant inputs known). An input is relevant only while it can still matter: security
questions stop counting once the small yard is ruled out; capability, leap and cost questions once it
is settled.

*What would change the answer* searches single changes, then the closest pair of changes, among the
inputs that can matter for each move, and says so when a result is robust to any one or two changes.
The paper's generic reassessment conditions are always listed too.

Every threshold lives in `dashboard/src/yards/engine/rules.ts` with its reason; the Methodology page
renders that object.

### Tests

* `dashboard/src/yards/engine/engine.test.ts`: the brief's cases A–H (concentration alone is not small;
  control-layer small; physical chokepoint; medium; large; unknowns lower confidence and are never
  filled; a mixed system; over- and under-fencing), transitions, change conditions, determinism.
* `dashboard/src/yards/data/adapter.test.ts`: scope and measure selection, provenance, no cross-scope
  borrowing, curated overrides, the URL codec.
* `dashboard/src/yards/views/yards.test.tsx`: the module through the real router on a synthetic bundle.
* `dashboard/src/yards/shell.test.ts`: the module in the shared header, nav and last-location memory.
* `tests/testthat/test-yards-fences-build.R`: the curated table, the build helpers, unlisted home
  shares, the metric map's safeguards, JSON array serialisation.

The module's tests (50 in vitest, 29 R expectations) pass. In the full dashboard suite, Strategic views
in `src/app.test.tsx` (`/network`, `/overview`) fail intermittently under load and pass on re-run;
they are outside this module.

```bash
cd dashboard && npx vitest run --project unit --project ui src/yards
Rscript -e "testthat::test_file('tests/testthat/test-yards-fences-build.R')"
```

## Decisions taken without asking

* **Where it lives.** The current OSI dashboard is the React app on `fix/allied-base-paper`, so the
  tool is a third module of it (branch `feat/yards-fences`), not of the older static `dashboard/` on
  `main`. It shares the app's design tokens, header, primitives and URL conventions; module code is
  isolated under `src/yards/`, and the shared shell changes are small (`modules.ts`, `Header.tsx`,
  `lastLocation.ts`, `Home.tsx`, `routes.tsx`).
* **Inputs are the published bundles**, not a second reading of `data/processed`, for parity with
  the other modules (as scripts/88 does).
* **Coercive concentration** is measured as the largest supplier *outside the 57-economy coalition*,
  labelled as such ("outside the coalition is not the same as adversarial"). The reader can change it.
* **The primary concentration measure** per scope is set in the metric map: manufacturing capacity
  (IEA ETP) before exports for a stage; sub-component exports for an HS6 sub-component; refining
  before mining for a mineral.
* **Time to harm against time to restore** replaces a fixed "acute horizon" for physical inputs, which
  is how the paper frames the small yard ("acute harm that markets cannot remedy in time").
* **Normalised OSI components** (addressable market, input costs) are shown as ranks and never feed
  the engine. An overcapacity component was left out because its direction could not be verified.
* **Examples** set a few assumptions of their own as YOUR JUDGMENT (for example a 40% premium for
  solar modules) and say so on the page; they demonstrate the framework, they do not settle it.
* **The curated table** encodes Unknown wherever the public record does not support a value (no
  stockpile cover is asserted anywhere; HALEU's learning questions; inverter-firmware separability).
* **The domestic premium** is always the reader's input: the indices do not measure it, and no
  deployment elasticity is claimed (the premium's consequences are shown as directions only).

## Limits and next steps

* The bundle is built for the United States; the state, adapter and build take `--country` and other
  economies as soon as curated components are reviewed for them.
* Component-level data exist only where an HS6 sub-component or a mineral isolates the component;
  software and services never will.
* The curated table is a starting set of 21 components in 5 systems; extending it is the main lever on
  the tool's usefulness and should be reviewed by domain experts.
* The policy-fit rules are deliberately simple and deterministic; they flag obvious mismatches, they
  do not optimise a package.
