# Allied Industrial Base Explorer

An interactive companion to the NEIS Center paper *Opportunity, Security, and the Allied Industrial
Base: A Quantitative Network Approach to Coalition Supply Chains for Energy Technologies*. Since the
October 2026 refactor it is organised around three questions. Home (`/`) has one entry card for each,
and every page shares one header with four tabs:

| Tab | Main paths | The question |
|---|---|---|
| **Sectors** | `/sector` (all sectors), `/sector/:tech` (the supply chain, Upstream -> Midstream -> Downstream), `/sector/:tech/:stage` (the Sector profile: Economic Opportunity, Energy Security, Policy and Climate at the top, the detail below), `/sector/minerals`, `/sector/explore` (advanced explorer) | What matters about this sector? Measured values in their sources' own units and years; no indices, no weights, no dials |
| **Countries** | `/countries`, `/countries/:iso3` (the Country profile: strategic position, opportunities and risks, sector position, policy and climate, partnerships, role in the Allied Industrial Base), `/countries/:iso3/memo`, `/countries/about` | What is this country's strategic position? The only place with the preference dials |
| **Allied Industrial Base** | `/aib` (overview), `/aib/partnerships` (trade, investment, development), `/compacts` and `/compacts/:id` (Sectoral Compacts), `/aib/roles` (country roles), `/compacts/about` | Where does international coordination add value, who needs to coordinate, and in what form? The AIB trade and investment model (scripts/31, 32, 84) |
| **About** (secondary) | `/about`, `/methods`, `/data` (Data & sources), `/glossary` | the methods, the data and their sources, the glossary |

In the Country profile readers open **Perspective: NEIS baseline** and move four preference dials (an
advanced panel sits behind its own disclosure); the explorer recomputes, in their browser, the Policy
and Strategic indices, the bilateral partnership indices (Friendshore, Partnership Opportunity, the PSI)
and the allied network design. The dials change priorities, rankings and partner attractiveness; they
never change observed or modelled facts (trade flows, production, concentration, the Energy Security and
Economic Opportunity values, and the whole AIB bundle: shock coverage, residual gaps, coordination
dividend, classes, compacts and roles). Pages label the two "Observed / modelled facts" and
"Preference-adjusted priorities". (Only the advanced panel's category weights move the two pillars.)

| Dial | Weighs up |
|---|---|
| Hawkishness | security: supply need, aligned partners, diversification of the network |
| Bullishness | opportunity: economic opportunity, export markets, economic size |
| Populism | policy signals and onshoring: industrial policy, midstream stages, self-supply |
| Climate (URL key `g`, engine key `greenness`) | clean technologies and climate ambition (coal, oil and gas dropped at 90+) |

Every dial runs from 0 to 100; 50 is the NEIS baseline (the paper). With every dial at 50 and the
advanced settings at their defaults, every page shows the pipeline's exported values exactly. Any other
setting is a custom perspective, recomputed in a Web Worker; it lives in the page address, so a copied
link reproduces it.

The model is the paper's final method (data build of 10 Oct 2026): a coalition of 57 economies (31
core members and 26 partners, Brazil among them through its mineral reserves); the EU-27 as one node
of the 37-node allied network, designed with diversification weight lambda = 0.94; the Policy index
from IEA Policies and Measures (required) plus Climate Action Tracker ratings where CAT rates the
economy; critical-mineral reserves from USGS Mineral Commodity Summaries 2026; and an endowment screen
on the allied picks (an Upstream stage with no recorded reserves or production is not picked; a
switch in the advanced panel). The Methods page has the details.

It is a static site: Vite + React + TypeScript, Tailwind v4 with the NEIS design tokens, Observable
Plot and d3 for the charts. There is no server; the data are files next to the app.

Old links keep working. The former Strategic Insights views keep their bare paths as advanced pages
with the dial panel (`/overview`, `/country`, `/space`, `/strategic`, `/partners`, `/network`; the
header files them under Countries or Allied Industrial Base); their one-economy links open the
matching section of the Country profile, which embeds the same views (`/country/:iso3` -> `#sectors`,
`/strategic/:iso3` -> `#opportunities`, `/partners/:iso3` -> `#partnerships`, query string kept). The
old module paths are aliases (`/strategic-insights` and `/country-insights` -> `/countries`,
`/sector-insights` and `/sectors` -> `/sector`, `/sectoral-compacts` -> `/compacts`,
`/allied-industrial-base` -> `/aib`), and a bare `/` carrying a scenario key (`/?h=80&tech=Solar`) still
redirects to `/overview?h=80&tech=Solar`. Home, Sectors, the AIB pages and a Country profile at the
NEIS baseline never start the model's Web Worker; it starts when a page needs it (a custom
perspective, AIB development partnerships, Methods and Data, a legacy Strategic page) and survives
switching tabs. The tabs remember the last page of each for the session.

## How the numbers are produced

```
R pipeline (scripts/0x-4x)                 data/processed/...
  -> scripts/85_export_dashboard_data.R    dashboard/public/data/  manifest.json + 4 data chunks
                                           (builders in R/dashboard/; checks the R engine at the
                                           paper's baseline against the stored outputs)
  -> scripts/86_export_dashboard_fixtures.R  src/engine/__fixtures__/  parity fixtures
  -> scripts/87_export_sector_data.R       dashboard/public/data/sector/  Sector Insights: manifest.json
                                           + lazy chunks (pure helpers in R/dashboard/sector/; the
                                           hand-checked config/sector_dictionary.yml)
  -> scripts/88_export_country_data.R      dashboard/public/data/country/  Country Insights: manifest.json,
                                           a fact sheet per economy, drivers files and the policy memos
                                           (pure helpers in R/dashboard/country/; config/country_insights.yml)
```

* `R/dashboard/` is the reference engine: at the baseline it reproduces the pipeline bit for bit.
* `src/engine/` is its TypeScript port (no dependencies), run in a Web Worker (`src/worker/`). It
  is parity-tested against R fixtures: pillars, Policy and the Strategic Index to 1e-9 (in practice
  bit-identical), partnership arrays exactly (the bundle is lossless: no quantisation), and the
  greedy network with identical producer sets.
* The baseline network: the paper's exact (MILP) design ships with the data when it was solved on
  the same inputs; otherwise the greedy design at the paper's settings is the baseline (the
  manifest's `flags.network_consistent`, shown on the Network and Methods pages). Scenarios are
  re-designed with the fast greedy solver; exact re-solves in the browser are out of scope for v1.

### Data files (`public/data/`)

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | at start (no-cache) | vintage, flags, notes, validation report, sources, baseline parameters, chunk list |
| `core.<md5>.osb` | at start | pillars, Policy, Strategic Index inputs and the shipped baseline tables |
| `vars.<md5>.osb` | Country view (variable drill-down) | variable-level components |
| `partner.<md5>.osb` | Partners, Network, PSI metric | Friendshore / Opportunity components, dyads, baseline best-five and PSI |
| `network.<md5>.osb` | Network view | nodes, member weights, stage inputs, the baseline design |
| `ties.<md5>.osb` | lazily, main thread: Partners view, Country partners section (never the engine) | IMF direct investment positions by partner (both directions, latest year, and the 2020-2024 mean Friendshore ranks; conduit centres flagged), CM-FDI factory-project flows by sector, Development Potential by sector, agreements, UN-vote alignment, top partners by best-five Friendshore / Partnership Opportunity (R/dashboard/dash_ties.R; scripts/85 checks the positions reproduce the investment tie bit for bit) |
| `pcell-<slug>.<md5>.osb` | lazily, one sector: Country partners section | each economy's top 10 friendshoring opportunities (Friendshore) and export markets (Partnership Opportunity) in that sector at the baseline, with the Partnership Strength rank; the Partners view ranks the sector in the worker instead (`cellPartners`, so the dials apply) |

OSB is a small container (a JSON header and typed columns, gzipped) written by
`R/dashboard/dash_osb.R` and read by `src/engine/codec.ts`. Chunk names carry their md5, so they can
be cached forever; the manifest is always revalidated.

### Sector data (`public/data/sector/`)

Written only by `scripts/87_export_sector_data.R` (independent of scripts/85: either can run first;
each has its own `bundle_id`, and the Sector manifest records the Strategic one it was built next to).
Same OSB container and codec, lossless (every value with a `data/processed` source is checked
`identical()` after the round trip). Only raw measured values ship: no index, weight or contribution
rows, and no licensed BNEF or BCG data.

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | idle prefetch on Home; Sector pages (no-cache) | vintage, headline facts for the question cards, coverage glyph, notes, validation, sources, chunk list, budget |
| `sector-meta.<h8>.osb` | every Sector page (prefetched on Home's idle) | dimensions (economies, items, links), dictionary (labels, units, years, sources, caveats), rosters, coverage, the concentration summary, the trade reconciliation |
| `sector-facts.<h8>.osb` | profiles, minerals, explorer, concentration trend | every cross-section and short series fact, with value states, and world totals |
| `sector-trade-<tech>.<h8>.osb` (10) | a profile's Trade section, the EU-bloc switch on trade cards | reconciled Comtrade dyads 2020 and 2024 per technology, declarations, the HS6 basket and its exclusions |
| `sector-policy.<h8>.osb` | Policy sections | IEA PAMS records (technology and stage masks) and Climate Action Tracker ratings |
| `sector-policy-text.<h8>.osb` | the policy list | PAMS titles and links (with attribution) |
| `sector-series.<h8>.osb` | series cards | EI 1965-2025, IMF monthly prices, IEA EV regions 2021-2024, CIM additions and manufacturing capacity |
| `sector-dev.<h8>.osb` | lazily, by the Development & finance flow layouts (Explore: aid flows, investment positions), the economy page's "Aid and investment partners" and Sources | OECD CRS official development assistance by aid sector, year, donor and recipient (USD m, constant 2023 prices; `data/raw/oecd_crs_api.csv`) and IMF Direct Investment Positions, outward debt instruments, the 2020-2024 mean per investor and destination with the conduit-centre flag (`data/raw/imf_dip.csv`), parsed with the Development Potential and Friendshore outbound builders' functions (R/dashboard/sector/sector_dev.R); the WDI and Doing Business values and the per-economy aid totals are in sector-facts. `--no-dev` leaves the chunk and its measures out |
| `sector-fdi.<h8>.osb` | lazily, by the CM-FDI views only (Solar, Wind, Batteries and EV Midstream: profile Investment section, Explore, Country in sector, Sources download) | CM-FDI cross-border manufacturing projects (Net Zero Industrial Policy Lab, edition 2026.10.05, CC BY 4.0): every project with its source text and links per field, home -> destination flows, parent-HQ group splits and announcements by year, for all projects and for those the partnership indices use (`data/raw/cm_fdi_projects.csv`, or `--cmfdi <csv>`; `--no-fdi` leaves the chunk and its measures out) |

Refresh after a pipeline rerun (scripts 10-25):

```bash
Rscript scripts/87_export_sector_data.R              # writes dashboard/public/data/sector/
Rscript scripts/87_export_sector_data.R --check-only # every guard and parity check, writes nothing
Rscript scripts/87_export_sector_data.R --synthetic dashboard/tests/fixtures/sector/synthetic
```

Without `public/data/sector/manifest.json` the Sector module says "No sector data" and points here;
Strategic Insights is unaffected.

### Country data (`public/data/country/`)

Written only by `scripts/88_export_country_data.R`, after scripts/85 and 87 (it reads their published
manifests and chunks and checks every Energy Security, Economic Opportunity, Strategic Index and
Partnership Strength value against them and `data/processed`). It also reads `nipo_policy_all.rds`,
`nipo_policy_index_tbl.rds` and the Global Trade Alert NIPO extracts in `data/raw` (vintage check),
`policy_outputs.rds`, `index_outputs.rds`, the partner tables, `charts/allied_strategic.csv`,
`outputs/allied_network_member_shares.csv`, `wdi_country_info.csv` and `economic_security_agreements.csv`.

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | Country pages (no-cache) | roster, tiers, quadrant counts, memo entries, sources, validation |
| `facts-<ISO3>.<h8>.json` | a Country profile | the economy's fact sheet (scores, drivers of the first sectors, policy, partners, network) |
| `drivers-<ISO3>.<h8>.json` | the profile's "why" panel for other sectors | the remaining driver details |
| `memo-<ISO3>.<h8>.{json,pdf,docx,md}` | the memo panel and its downloads | the verified AI-written policy memo of a core member (spec_country.md part B) |

```bash
Rscript scripts/88_export_country_data.R                    # writes dashboard/public/data/country/
Rscript scripts/88_export_country_data.R --check-only       # every check, writes nothing
Rscript scripts/88_export_country_data.R --facts-dir <dir>  # also writes the memo writers' fact sheets
Rscript scripts/88_export_country_data.R --render-memos [--iso3 USA,JPN] [--lint-only]
Rscript scripts/88_export_country_data.R --synthetic        # the test fixture (src/country/__fixtures__/synthetic/)
```

`--render-memos` lints each `dashboard/country-memos/<ISO3>/memo.src.md` against the published fact sheet,
requires a complete passing `verify.json` from the independent verifier (a later fact sheet whose claims are
unchanged keeps the sign-off), and publishes the memo JSON, PDF (headless Edge or Chrome) and Word file
(Python with python-docx and pypdf; local only). `--iso3` limits a run; `--save-ctx` / `--load-ctx` reuse
the inputs of an earlier run for debugging. Real-data checks: `npm run test:country-real`.

### Sectoral Compacts data (`public/data/aib/`)

Written only by `scripts/84_export_aib_data.R` (builder in `R/dashboard/aib/aib_export.R`) from
scripts/32's `aib_results.rds`: plain JSON, every array field wrapped in `I()`, the manifest written last.

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | every Sectoral Compacts page (no-cache) | schema `osi-aib` v1, created, universe, headline shock and scenario, the document's file name |
| `compacts.<md5_8>.json` | every Sectoral Compacts page (immutable) | meta (trade year, shocks, scenarios, classes, coverage targets, classification thresholds), country names, the compacts (nodes, coalition and its path, roles, metrics with the global optimum and the demand anchors' coverage under fragmented action and under the compact, class mix, policy needs, trade and investment relationships) and every classified sub-sector node (with its supplier-limit frontier: coverage, coverage after a second shock, specialization) |

The contract (types and the runtime checks `checkAibManifest()` / `checkCompactsDoc()`) is
`src/aib/contract.ts`. Without `public/data/aib/manifest.json` the module says "No compacts data";
the other modules are unaffected. Real-data checks: `npm run test:aib-real`; the view tests also run on
the real bundle with `AIB_DATA_DIR=public/data/aib npx vitest run --project ui src/aib`.

## Refresh the data

From the repository root, after a pipeline rerun:

```bash
Rscript scripts/85_export_dashboard_data.R      # writes dashboard/public/data/ and data/processed/dashboard/
Rscript scripts/86_export_dashboard_fixtures.R  # regenerates the engine's parity fixtures
```

`scripts/85` prints a validation table (every baseline check must PASS except those the manifest
flags as upstream-inconsistent) and stops above the budget. Options: `--out-dir`, `--r-out`,
`--network-dir` (the scripts/30 MILP outputs to ship as the exact baseline), `--skip-trade`,
`--paper` (copy the preview PDF to `public/paper/`; off by default). Then rerun the tests below.

Run the export only on a finished pipeline run: it also reads raw inputs (the Comtrade basket trade,
CAT ratings, GDP), and when the stored scripts/30 design no longer matches the nodes, stages, lambda
or edge inputs, the export ships the greedy design as the network baseline (and the manifest says
so) instead of the paper's exact design. `scripts/86 --set real` refuses a bundle other than the one
`scripts/85` last exported, so run the two in that order.

## Develop

Node 24 and npm 11.

```bash
cd dashboard
npm ci
npm run dev           # http://localhost:5173, serves public/data/
npm run dev:fixture   # the same app on the small synthetic bundles (no export needed): the
                      # Strategic one at data/, the Sector one (tests/fixtures/sector/synthetic/)
                      # at data/sector/, the Country one at data/country/ and the Sectoral Compacts
                      # one (src/aib/__fixtures__/synthetic/data/aib/) at data/aib/
```

Without `public/data/` the Strategic pages say so ("No data") and point here; Home and Sector
Insights do not need it.

Layout:

```
src/
  routes.tsx   the route table: AppRoot, Home, the lazy Strategic layout and views, /sector
  shell/       AppRoot (header with module tabs, notices, <main>, footer), Home, StrategicLayout
               (engine, scenario, runs, dials), last location per module, build stamps, prefetch
  engine/      the TypeScript engine (port of R/dashboard/) + parity tests and fixtures
  worker/      Web Worker protocol, host and the main-thread client
  state/       URL state (url.ts), engine provider, runs, navigation
  views/       Strategic: Overview, Country, Space, Strategic, Partners, Network, Methods, Data
  aib/         Sectoral Compacts: contract.ts (the data contract with scripts/84), data/ (store, hooks),
               derive/ (pure helpers: classes, coverage, nodes, flows, coalition, roles), charts/,
               views/ (index, compact, about), AibLayout, routes, nav
  sector/      Sector Insights: contract.ts (the data contract with scripts/87), data/ (store),
               derive/ (pure derivations), views/, charts/ (the Sector chart kit), chrome/
               (SectorBar, SectionRail, BottomSheet, ThemeChips), url.ts, format.ts
  charts/      PlotFigure (Observable Plot + HTML tooltips), ChartCard (table twin, CSV), maps, Sankey
  components/  Strategic chrome (status strip, dials), tables, NEIS controls, Logo
  content/     methods.md (the Methods page), questions.ts (Home's questions), sector.md
  styles/      globals.css (NEIS tokens and classes, Tailwind v4)
```

## Test

```bash
npm run typecheck     # app and test/config projects
npm test              # unit (engine numerics, worker, URL state, view derivations),
                      # ui (jsdom: every view on the synthetic bundle, dial panel, downloads),
                      # parity-synthetic (the TS engine against the committed R fixtures)
npm run test:parity   # the real data's fixtures (scripts/86 --set real; not committed)
npm run test:sector-real  # the decoded real Sector bundle: the reference values (spec Appendix B),
                          # meta.summary = the client recomputation, every chunk against the
                          # contract, headlines, validation (no FAIL; WARNs listed) and the data
                          # budgets (skipped without public/data/sector/manifest.json)
npm run test:aib-real     # the real Sectoral Compacts bundle against its contract (skipped without
                          # public/data/aib/manifest.json)
```

The Sector route smoke tests and the summary parity also run on any Sector bundle directory:
`SECTOR_BUNDLE_DIR=public/data/sector npx vitest run --project unit --project ui src/sector/views/views.synthetic.test.tsx src/sector/derive src/sector/contract.test.ts`
(CI keeps the synthetic default).

## Build

```bash
npm run build         # dist/ (tsc -b && vite build)
npm run budget        # first-visit download and data sizes against the limits
npm run preview       # serve dist/ locally
```

Budget (`npm run budget`, gzip sizes; it also fails when a manifest lists a missing chunk):

| Figure | Contents | Limit |
|---|---|---|
| Home first visit | entry JS + CSS, the Latin Roboto face, the Home chunk (no data, no worker, no map) | 400 kB |
| Strategic deep link `/overview` | entry, StrategicLayout, Overview, world map, engine worker, manifest, core chunk | 3 MB |
| Sector landing `/sector` | entry, SectorLayout, the Sector home view, sector manifest, meta | 700 kB |
| Sector profile first view | the landing, the profile view, facts | 1.3 MB |
| Each Sector chunk | meta 150 kB, facts 600 kB, trade 450 kB each, policy 250 kB, policy text 700 kB, series 900 kB | |
| Sector data / all data | `public/data/sector/` / Strategic + Sector | 9 MB / 25 MB |

On 11 Oct 2026 (Strategic export of 10 Oct, Sector export of 11 Oct on the final data): Home
181 kB, `/overview` 1.27 MB, Sector landing 395 kB (278 kB of code, 75 kB of manifest and meta),
Sector profile 703 kB (344 kB of code, 317 kB of data), Sector data 2.91 MB, all data 7.95 MB
(Strategic 5.04 MB).
`BUDGET_DIST=<dir>` checks a build made with `vite build --outDir <dir>`; `BUDGET_SECTOR_DIR=<dir>`
checks another Sector bundle (e.g. `tests/fixtures/sector/synthetic`). Nothing is quantised.

## Deploy (Netlify)

Not done by the builders; the custom domain and the publication of the data are NEIS decisions.

* **From git:** connect the repository in Netlify and set the site's *Base directory* to
  `dashboard`; the build command, publish directory, Node version, redirects and headers come from
  `netlify.toml`. `public/data/` must then be committed (about 5 MB of derived data).
* **From a local build:** `npm run build`, then `npx netlify deploy --dir=dist` (a draft URL) and,
  once checked, `npx netlify deploy --dir=dist --prod`.
* **Under a sub-path** (e.g. `neiscenter.org/explorer/`): build with `VITE_BASE=/explorer/` and
  adjust the redirects in `netlify.toml` to that prefix.

`netlify.toml` serves missing data files as 404s (not the app), caches `assets/` and the data chunks
as immutable (`/data/*.osb` and, explicitly, `/data/sector/*.osb`), revalidates both `manifest.json`
files, and sets a Content-Security-Policy with no third-party origins (fonts and the world map are
self-hosted; the IEA policy links are navigations, not requests).

Browsers: current Chrome, Edge, Firefox and Safari. Gzip is decoded natively where
`DecompressionStream` exists (Safari 16.4+, Firefox 113+), with a small fallback otherwise.

## What is generated and what is committed

| Path | Made by | Committed |
|---|---|---|
| `public/data/` | `scripts/85` | needed for Netlify builds from git (the maintainers' decision) |
| `public/data/sector/` | `scripts/87` | as `public/data/` |
| `tests/fixtures/sector/synthetic/` | `scripts/87 --synthetic` | yes (small; the Sector tests and `dev:fixture`) |
| `src/engine/__fixtures__/synthetic/` | `scripts/86` | yes (CI parity tests) |
| `public/data/aib/` | `scripts/84` | as `public/data/` |
| `src/aib/__fixtures__/synthetic/` | by hand (invented economies and numbers) | yes (small; the Sectoral Compacts tests and `dev:fixture`) |
| `src/engine/__fixtures__/real/` | `scripts/86 --set real` | no (gitignored, ~30 MB) |
| `public/paper/` | `scripts/85 --paper` | no (gitignored) |
| `dist/`, `node_modules/` | npm | no |

## License and citation

MIT (see `LICENSE` at the repository root). Cite the paper: NEIS Center, *Opportunity, Security,
and the Allied Industrial Base: A Quantitative Network Approach to Coalition Supply Chains for
Energy Technologies*, and the explorer with its data build (shown in the footer and on the Data
page). Scenario results are model outputs for exploration, not forecasts. Sector Insights republishes
UN Comtrade dyads, IEA Policies and Measures titles and links, and the IEA Critical Minerals top-6
tables with attribution (see its Sources & data page); licensed BNEF and BCG data are not part of the
public explorer.
