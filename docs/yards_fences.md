# Draw the Fence: the Yards & Fences decision tool

*Draw the Fence* is a standalone NEIS Center site, separate from the Allied Industrial Base Explorer
but built from the same package (`dashboard/`), so it shares the Explorer's look, design tokens, UI
primitives, fonts and data without copying them. It turns the NEIS Center paper *Clean Energy
Industrial Strategy: Three Yards for Three Different Problems* (discussion draft, October 2026) into an
interactive policy lab:

> OSI tells you what the world looks like. Yards & Fences helps you decide what to do about it.

The tool runs **Problem → Evidence → Yard → Fence → Policy → Metrics → Reassessment**. It never runs
technology → score → label: there is no aggregate score, and no headline index is mapped to a yard.

The framework and the decision engine lead. Data from the Opportunity & Security Indices and the
allied network appear as context: a short note of one to three plain sentences beside the gate they
inform, with the figures and sources one click away, and in collapsed sections on the result and
trusted-supply pages. They inform the reader's answers and never decide the yard.

```bash
cd dashboard
npm run dev:fence       # http://localhost:5173 (or --port)
npm run build:fence     # fence/dist/, a site of its own (fence/netlify.toml)
```

## Pages

| Path | What it does |
|---|---|
| `/` | The framework in one screen: three yards as three problems, how the engine decides (the four gates), the sequence, the examples |
| `/assess` | The guided assessment: what is assessed → Gate 1 (acute harm) → Gate 2 (2A control layers, 2B physical chokepoints) → Gate 3 (capability; preserve / catch up / leap ahead) → Gate 4 (cost of protection), each with a context note. Analyst and Quick modes share one state and engine: Quick asks the questions flagged `quick` in `engine/inputs.ts` that apply to the subject (at most 7: 2A's follow-ups drop out when nothing is digitally active and 2B for a pure control layer; choosing leap ahead adds its 4 checks), hides the context notes and the premium-justification question, and lists each skipped question with the value it keeps (curated, an earlier answer, or Unknown). Every assumption, here and across the site, has an "i" tooltip with a plain-language explanation (`INPUT_EXPLAIN` in `engine/inputs.ts`), also collected in the Methodology glossary |
| `/result` | The result: yard, posture, confidence, evidence completeness, why (and what did *not* drive it), the decision tree, evidence gaps, what would change the answer, the policy posture and the curated notes; every input with its provenance and the data behind it in collapsed sections. For a system, the component stack first. Print / Save as PDF |
| `/scenario` | Test another world: change the framework's assumptions first, then the cost of protection, then market conditions; baseline and scenario side by side; the change of yard explained by the gate that changed |
| `/fence` | Build your fence: 13 instruments × Off/Low/Medium/High and the scope of security measures; rule-based findings (under-fencing, over-fencing, mismatch, closed gate, no exit, …), the two fences drawn side by side, and how we will know it worked |
| `/supply` | Trusted supply: can trusted diversification solve the problem? The three strategies and what the diagnosis implies for them, compared for this assessment; current supply, import sources and the allied network design in a collapsed section |
| `/examples` | Six one-click assessments, computed live |
| `/methodology` | The rules (rendered from `engine/rules.ts`), the data and vintages, the build's validation, the metric map, the curated table |

The whole assessment travels in the URL (`?a=`, base64url JSON), so *Copy assessment link* reproduces
it exactly. Nothing is stored anywhere else.

## Architecture

```
config/yards_fences_metric_map.yml          which published measures become evidence; labels; engine inputs; stacks
data/reference/yards_fences_assessments.csv the curated component table (what OSI cannot measure)
R/dashboard/yards_fences/yf_build.R         pure helpers (no IO)
scripts/89_export_yards_fences_data.R       IO: reads the published bundles, writes dashboard/fence/public/data/yards-fences/
dashboard/vite.fence.config.ts              the site's build (root fence/, code src/fence/; `/src/` aliased to the package)
dashboard/fence/                            index.html, public/ (favicon, data), netlify.toml, dist/ (built)
dashboard/src/fence/
  main.tsx, routes.tsx, nav.ts, title.ts, FenceLayout.tsx, fence.css (imports styles/globals.css)
  shell/         Header (brand row, views row, Menu below 1024 px) and Footer
  engine/        the decision engine: pure TypeScript, no IO, unit-tested (engine.test.ts)
  data/          contract.ts (the bundle's shape), store.ts (fetch + cache), adapter.ts (evidence -> engine inputs)
  state/         assessment.ts (state + URL codec), YardsContext.tsx (provider, memoised engine runs)
  components/    kit (yard glyphs, provenance badges, EvidenceMetric), Controls, Context (the context
                 notes), EvidencePanel, ResultBlocks (decision tree, gaps, change conditions),
                 ComponentStack, FenceDiagram
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

Every threshold lives in `dashboard/src/fence/engine/rules.ts` with its reason; the Methodology page
renders that object.

### Tests

* `dashboard/src/fence/engine/engine.test.ts`: the brief's cases A–H (concentration alone is not small;
  control-layer small; physical chokepoint; medium; large; unknowns lower confidence and are never
  filled; a mixed system; over- and under-fencing), transitions, change conditions, determinism.
* `dashboard/src/fence/data/adapter.test.ts`: scope and measure selection, provenance, no cross-scope
  borrowing, curated overrides, the URL codec.
* `dashboard/src/fence/views/fence.test.tsx`: the site through its own router on a synthetic bundle: the landing page, a system result, the sandbox, the scenario lab, the guided flow (context notes beside the gates), trusted supply and methodology, the No data screen.
* `tests/testthat/test-yards-fences-build.R`: the curated table, the build helpers, unlisted home
  shares, the metric map's safeguards, JSON array serialisation.

The site's tests (47 in vitest, 29 R expectations) pass. In the full dashboard suite, Strategic views
in `src/app.test.tsx` (`/network`, `/overview`) fail intermittently under load and pass on re-run;
they are outside this site.

```bash
cd dashboard && npx vitest run --project unit --project ui src/fence
Rscript -e "testthat::test_file('tests/testthat/test-yards-fences-build.R')"
```

## Decisions taken without asking

* **Where it lives.** A site of its own, separate from the Allied Industrial Base Explorer (which
  neither imports nor links to it), on branch `feat/yards-fences` off `fix/allied-base-paper`. It is
  built from the Explorer's package so that both share one design system and one data pipeline: its
  own entry (`fence/index.html`), router, header, footer, title and build output (`fence/dist/`).
  The Explorer's own files are unchanged apart from three npm scripts (`dev:fence`, `build:fence`,
  `preview:fence`) and the new config in `tsconfig.node.json`.
* **Data as context.** The framework's questions lead every gate; measured facts sit beside them as
  a short context note. The separate "what the data tell us" stage of the first version was folded into
  these notes; the full evidence panel stays available, collapsed, on the result page.
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
