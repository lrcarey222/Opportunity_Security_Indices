# Opportunity Security Indices (OSI) and the Allied Industrial Base Explorer

This repository builds the **Allied Industrial Base (AIB) Explorer**, a public, static web app from the NEIS Center.
For each economy, energy technology and supply-chain stage, it recommends what a country should secure, compete
in, source from partners, deploy faster or leave open. It also shows where allies gain by coordinating supply
chains. Behind the app sits an R pipeline (`R/`, `scripts/`, `config/`) that turns public trade, production,
investment and policy data into the app's data bundles.

- **Public build:** <https://lrcarey222.github.io/aib-explorer/> (GitHub Pages, published by hand from the
  separate `lrcarey222/aib-explorer` repository).
- **Where is the current methodology?** Start at [docs/README.md](docs/README.md). The decision framework is
  [docs/strategic_position.md](docs/strategic_position.md), the AIB is [docs/aib_methodology.md](docs/aib_methodology.md)
  and the stages are [docs/supply_chain_stages.md](docs/supply_chain_stages.md).
- **Previous README:** the long version, with the legacy index formulas and three-stage pillar notes, is archived
  at [docs/archive/README_full_2026-11.md](docs/archive/README_full_2026-11.md).

## Unit of analysis

**Country x technology x supply-chain stage.** The Explorer covers 214 economies, 11 technologies and 6 stages,
which makes 66 technology x stage steps. The posture bundle holds 14,124 country x step cells.

- **Technologies:** Electric Vehicles, Nuclear, Coal, Batteries, Green Hydrogen, Wind, Oil, Solar, Gas,
  Geothermal and Electric Grid.
- **Stages** (`config/supply_chain_stages.yml`, `dashboard/src/lib/stages.ts`), with the labels shown on screen:

| On screen | Full name | Legacy roll-up |
|---|---|---|
| Extraction | Extraction | Upstream |
| Refining | Refining & Processing | Upstream |
| Components | Component Manufacturing | Midstream |
| Assembly | Final Assembly & Equipment | Midstream |
| Deployment | Deployment & Use | Downstream |
| Recovery | Recovery & Recycling | Upstream |

Upstream / Midstream / Downstream is a **legacy roll-up**. It is kept only for compatibility and for old links,
and no decision uses it. Sources are often coarser than six stages. So every input carries an evidence status
(exact, split, shared, proxy, technology-wide, no data), and each cell reports how much of its evidence is
specific to its stage.

## Decision architecture

Each cell runs one chain: **Need -> Prize -> Position -> Choice -> Policy alignment**
([docs/strategic_position.md](docs/strategic_position.md); code `R/strategic_position/`; rules
`config/strategic_position.yml`).

| Step | Question | Main output |
|---|---|---|
| Need | What problem must the country solve? | Supply-chain need at the five producing stages; deployment need at Deployment. The two are never combined. |
| Prize | Is domestic capability in this step strategically valuable? | Strategic Industrial Value: market, industrial-commons and future-option value, with a market-only cap and a countervailing large-yard test. Nothing that measures capability or need enters it. |
| Position | How strong is the country today? | Competitive Position: demonstrated export, production, capacity, investment, cost and resource evidence only. |
| Choice | So what should it do? | One of six choices (below), with a decision trace, confidence and borderline flags. |
| Policy alignment | Does current policy match? | Observed policy (Global Trade Alert's New Industrial Policy Observatory by stage, IEA Policies and Measures) compared with the choice: aligned, under-prioritized, over-prioritized, misaligned, deployment gap, mixed or insufficient data. |

**The six choices and their fences.** The fence height follows the yard (the Yards & Fences framing):

| Choice | Meaning | Fence |
|---|---|---|
| Secure | Protect a step where loss of control or a coercive chokepoint would be acute | High (small yard) |
| Preserve | Keep a strong existing position | Medium |
| Catch Up | Build a credible position from an existing capability anchor | Medium |
| Partner | Rely on trusted partners and diversified supply | Medium |
| Accelerate | Deploy faster (Deployment only) | Medium |
| Watch | Domestic industrial priority is low; keep the market open | Low (large yard) |

Shipped counts (`dashboard/public/data/posture/manifest.json`, created 2026-11-21, git `17f0737e`):
Secure 212, Preserve 111, Catch Up 351, Partner 1,475, Accelerate 514, Watch 6,574, not assessed 4,887.

Rules worth knowing before reading results:

- **Not assessed is not Watch.** A cell whose Need or Prize is not measured is "not assessed". Watch is a positive
  conclusion and is never used for missing data.
- **Observed policy never enters Need, Prize, Position or Choice.** `tests/testthat/test-decision-engine.R` checks
  that changing every policy column leaves every choice unchanged.
- **Sensitive layers** (control software, firmware, enrichment, magnets...) take their own choice and yard. A cell
  shows the roll-up, marked mixed where its layers differ.
- **Legacy indices.** The Energy Security (ES), Economic Opportunity (EO) and Strategic Index (SI) scores remain in
  the pipeline and on the legacy pages (`/overview`, `/space`, `/strategic`). They are descriptive and historical,
  and they are **not** the decision rule. Their method is archived in [docs/methodology.md](docs/methodology.md).

## Allied Industrial Base logic

The AIB treats the allied supply base as a network of bilateral trade and investment relationships, not as a
fixed alliance ([docs/aib_methodology.md](docs/aib_methodology.md); code `R/aib/`; settings `config/aib.yml`,
`config/aib_strategic_layer.yml`).

- **Managed interdependence.** Specialization creates value, so the aim is not maximum diversification. The question
  is which dependencies are worth keeping and how much backup is enough. Concentration is weighed against
  resilience at each step: a supplier cap shows what it costs to spread supply.
- **Trusted network.** 61 economies: the 31 core members and 28 partners of the two-tier coalition
  (`config/coalition.yml`, built by `scripts/13`), plus Taiwan and Switzerland, which are treated as allied
  (`config/aib_strategic_layer.yml` `treat_as_allied`). The 59 coalition members are the demand the stress tests
  protect. Any economy may supply them. The app derives the list from data in
  `dashboard/src/posture/trustedNetwork.ts`.
- **Scenarios.** *Today's network* (observed 2024 relationships, no disruption); *Managed Interdependence* (no
  supplier banned, but a cap on any one supplier's share at concentrated steps); *Major-supplier disruption* (in
  this build China stops supplying allied markets; the data id is `decoupling`, shock `china_100`).
- **Trade is enough, or new capacity is needed.** Each step is classed as *Low intervention* (the existing network
  copes), *Coordinate trade* (existing suppliers can cover the gap through new or expanded trade relationships:
  the extra supply recovered by acting together, or coordination dividend, is at least 5 points), *Build capacity*
  (even the best coordinated trade leaves a remaining supply gap of 10% or more), *Manage specialization* (resilient
  but too concentrated) or *Not enough evidence*. Observed trade is never read as spare physical capacity. Where
  production data exist, they decide who can expand supply.
- **Sectoral Compacts** group the recurring relationships at steps where joint action improves resilience. Countries
  get compact roles and Strategic AIB roles.

## The Explorer

A static React app in `dashboard/` ([dashboard/README.md](dashboard/README.md)). Navigation:

| Area | Paths | What it answers |
|---|---|---|
| Home | `/` | Entry points to the three questions |
| Sectors | `/sector`, `/sector/:tech`, `/sector/:tech/:stage`, Advanced explorer `/sector/explore`, `/sector/minerals` | Where are the bottlenecks and opportunities in a supply chain? |
| Countries | `/countries`, `/countries/:iso3`, country strategy (decision) briefs at `/countries/:iso3/memo` | What should this country do, and why? Nine briefs are published: AUS, CAN, DEU, FRA, GBR, IND, JPN, KOR, USA |
| Allied Industrial Base | `/aib`, `/aib/partnerships`, `/aib/roles`, `/aib/diversification`, `/aib/agreements`, `/compacts` | Where does coordination change the outcome, and who should work together? |
| About | `/about`, `/methods`, `/data`, `/glossary` | Methods, data and sources, glossary |
| Expert review | `/review` (no tab) | Structured blind-first review of selected cases ([docs/QC_REVIEWER_GUIDE.md](docs/QC_REVIEWER_GUIDE.md)) |

The separate *Draw the Fence* site (`dashboard/fence/`, [docs/yards_fences.md](docs/yards_fences.md)) is not part
of the Explorer.

## Data

Every raw input, with its vintage, cadence and licence, is listed in [docs/sources.md](docs/sources.md) (generated
from `config/raw_inputs_manifest.yml`). The main families:

| Family | Sources (vintage in use) | Used for |
|---|---|---|
| Trade | UN Comtrade all-reporter HS6 dyads (2020 and 2024, pulled 2026-10); Comtrade monthly dyads (latest 12 months per reporter, pulled 2026-11); Harvard Atlas of Economic Complexity HS92 (1995-2024, Dataverse V18) | Need, Position, AIB stress tests |
| Production and resources | USGS Mineral Commodity Summaries 2026; IEA Critical Minerals Dataset 2026; Energy Institute Statistical Review 2026; U.S. Energy Information Administration international statistics; OECD-NEA/IAEA Uranium Red Book 2026; Clean Investment Monitor capacity | Position, AIB physical supply limits |
| Deployment and pathways | Ember electricity data and 2030 targets; IEA Global EV Outlook 2026, World Energy Outlook 2025, World Energy Investment 2026 | Deployment need |
| Investment and finance | Cleantech Manufacturing FDI Dataset (CM-FDI, edition 2026.10.05); IMF Coordinated Direct Investment Survey; OECD Creditor Reporting System; Oil Change International energy finance | Position, partnerships, development partnerships |
| Policy | Global Trade Alert New Industrial Policy Observatory (February 2026); IEA Policies and Measures; Climate Action Tracker | Policy alignment only |
| Alignment and agreements | UN General Assembly ideal points (Dataverse V39); economic-security agreement membership; DESTA v2.3; Global Strategic Partnership Dataset | Coalition tiers, agreements |
| Development | World Bank World Development Indicators | Country context, development partnerships |

- **Trade is not production.** Trade is measured in value and production in physical volume. Position counts
  both, and the AIB uses production or capacity shares, where they cover at least 80% of world output, to decide
  who can expand supply. In the 2026-10 data that holds for 11 AIB steps; the others carry a trade-only label.
- **Missing data.** Missing is never zero. Unmeasured posture cells are "not assessed" (4,887 in the shipped
  bundle), and thin AIB steps are "Not enough evidence". 13 of the 66 technology x stage steps have no
  country-level primary measure, and the Sector pages say so. Examples are Geothermal outside Deployment and
  Green Hydrogen outside Extraction.

## Reproducibility

**Pipeline.** `Rscript run_pipeline.R` runs the core steps in order (00, 05, 07, 10, 11, 16, 17, 18, 12, 13, 20,
15, 25, 26, 27, 31-33, 80). It does not run `scripts/95_build_deployment_need.R`, so run 95 first whenever 10,
17, 18 or 20 change. Copy `config/config.example.yml` to `config/config.yml` first. Comtrade pulls need
`COMTRADE_API_KEY` in the environment (never commit it). The archived README lists every script, environment
variable and refresh step.

**Exports into `dashboard/public/data/`** (tracked in git), in this order: `scripts/91` (posture), `85`
(legacy index bundle), `86` (engine fixtures), `87` (sector), `88` (country), `84` (AIB), `93` (development
partnerships), then `scripts/92 --iso3 <ISO3>` for each brief. The agreements bundle comes from `scripts/41`
-> `42` -> `43` ([docs/partnership_agreements.md](docs/partnership_agreements.md)). Commit the R code before
exporting: each manifest records `created` and `git_commit`, and the posture and AIB stamps add `-dirty` when
`R/`, `scripts/` or `config/` have uncommitted changes.

**Release identity.** A release is named by its data release date, the bundle stamps and the review release id.
Bundles as of 2026-11-23:

| Bundle | Created (UTC) | Stamp |
|---|---|---|
| Posture | 2026-11-21 | git `17f0737e` |
| Allied Industrial Base | 2026-11-21 | git `03560172` |
| Country | 2026-10-31 | git `4bb2adc` |
| Sector | 2026-10-31 | git `4bb2adc` |
| Legacy index (strategic) | 2026-10-30 | git `a54a9d3` |

The review release id, `rel-<first 12 hex characters of a sha256 over the bundle stamps>`, is written to
`data_release.id` in `dashboard/public/data/review/manifest.json` by `node dashboard/scripts/review/build-review-data.mjs`.
Every expert-review submission carries it, together with the app commit and build date
([docs/QC_DEPLOYMENT.md](docs/QC_DEPLOYMENT.md), [docs/QC_RELEASE_CHECKLIST.md](docs/QC_RELEASE_CHECKLIST.md)).

**Tests.**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"     # R, from the repository root
cd dashboard && npm ci
npm run typecheck && npm test && npm run build && npm run budget
npm run test:country-real   # also test:sector-real, test:aib-real, test:parity (need the real bundles)
```

Size limits live in `dashboard/scripts/check-budget.mjs`. Data QC and sensitivity checks are in `scripts/qc/`
([docs/QC_SYSTEM_DESIGN.md](docs/QC_SYSTEM_DESIGN.md)).

**CI** (`.github/workflows/`): `dashboard.yml` (typecheck, test, build, budget on Node 24); `r-tests.yml`,
`r-check.yml` and `r-ci.yml` (testthat, smoke tests). `news-scoring.yml` serves an unrelated project.

## Repository layout

```
R/            pure functions, no IO (strategic_position/, aib/, dashboard/, indices/, categories/, qc/ ...)
scripts/      IO and orchestration, numbered in run order; fetchers/, qc/, paper/
config/       rules, weights, coalition, AIB settings, raw-input manifest, review campaigns
data/         reference/ (tracked crosswalks); raw/ and processed/ (local, not tracked)
docs/         methodology and module docs (index: docs/README.md); archive/
dashboard/    the Explorer (React, Vite, TypeScript); fence/ is Draw the Fence
tests/        testthat suite
```

## Known issues

Open model and data issues are logged in [docs/QC_SYSTEM_DESIGN.md](docs/QC_SYSTEM_DESIGN.md) section 1.5:
knife-edge prize values, entrepot re-exports, coexisting "trusted partner" definitions, implausible compact cores
and HS 2812.90 filed at Refining. The review-readiness audit is
[docs/EXTERNAL_REVIEW_READINESS_AUDIT.md](docs/EXTERNAL_REVIEW_READINESS_AUDIT.md).

## Data licences and attribution

Each input's licence is recorded in `config/raw_inputs_manifest.yml` (`licence`). Key terms:

- **CM-FDI** (CC BY 4.0). Cite as: Bandara, P. and the Net Zero Industrial Policy Lab. Cleantech Manufacturing FDI
  Dataset, edition 2026.10.05. Johns Hopkins University. <https://cmfdi.netzeropolicylab.com>. Counts, shares and
  indices are computed by OSI from the project records.
- **CC BY 4.0, cite the source:** Climate TRACE, OECD Creditor Reporting System, Ember, IEA Critical Minerals
  Dataset, IEA World Energy Investment.
- **Non-commercial:** IEA World Energy Outlook free dataset (CC BY-NC-SA 4.0); IEA Net Zero by 2050 (CC BY-NC-SA
  3.0 IGO); Oil Change International energy finance (CC BY-NC 4.0).
- **Public domain:** U.S. Energy Information Administration international energy statistics.
- **Licensed, never shipped in the Explorer:** BloombergNEF and BCG data. The IEA Energy and Emissions per Value
  Added database is licence-restricted and is not read.
- The Explorer republishes UN Comtrade dyads, IEA Policies and Measures titles and links, and IEA Critical Minerals
  tables with attribution (Sector > Sources).

Main sources: [UN Comtrade](https://comtradeplus.un.org/) · [Atlas of Economic Complexity](https://atlas.hks.harvard.edu/) ·
[USGS MCS 2026](https://doi.org/10.3133/mcs2026) · [IEA Critical Minerals](https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset) ·
[IEA Policies](https://www.iea.org/policies) · [Climate Action Tracker](https://climateactiontracker.org/countries/) ·
[Energy Institute Statistical Review](https://www.energyinst.org/statistical-review) ·
[EIA International](https://www.eia.gov/international/data/world) · [Global Trade Alert](https://www.globaltradealert.org/) ·
[UNGA ideal points](https://doi.org/10.7910/DVN/LEJUQZ) · [WDI](https://databank.worldbank.org/source/world-development-indicators) ·
[CM-FDI](https://cmfdi.netzeropolicylab.com) · [Ember](https://ember-energy.org/data/yearly-electricity-data/).

## Citation

Cite the software with `CITATION.cff`. When you use outputs in external work, record:

- the repository commit and the release identity above (data release date, bundle stamps, review release id);
- [docs/strategic_position.md](docs/strategic_position.md), [docs/aib_methodology.md](docs/aib_methodology.md) and
  [docs/sources.md](docs/sources.md);
- the configuration files used (`config/*.yml`), the state and date of the raw inputs, the processing date, and any
  manual overrides.

Results are model outputs for analysis, not forecasts.

## License

MIT; see `LICENSE`. For contributions and responsible disclosure, see `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and
`SECURITY.md`.
