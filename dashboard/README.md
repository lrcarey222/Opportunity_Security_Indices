# Allied Industrial Base Explorer

The NEIS Center's public Explorer. For each economy, energy technology and supply-chain stage it shows what a
country should do (Secure, Preserve, Catch Up, Partner, Accelerate or Watch), why, and where allies gain by
coordinating supply chains. The method is documented outside this folder: start at
[../docs/README.md](../docs/README.md) (decision framework `docs/strategic_position.md`, Allied Industrial Base
`docs/aib_methodology.md`, stages `docs/supply_chain_stages.md`).

It is a static site: Vite + React + TypeScript, Tailwind v4 with the NEIS design tokens, Observable Plot and d3
for the charts. There is no server; the data are files in `public/data/`, exported by the R pipeline.

| Area | Main paths | The question |
|---|---|---|
| Home | `/` | Entry points to the three questions |
| **Sectors** | `/sector`, `/sector/:tech` (the chain, Extraction -> Refining -> Components -> Assembly -> Deployment -> Recovery), `/sector/:tech/:stage`, `/sector/minerals`, `/sector/explore` (Advanced explorer) | Where are the bottlenecks and opportunities? Measured values in their sources' own units and years, plus the recommended choices at fixed thresholds |
| **Countries** | `/countries`, `/countries/:iso3` (the Country profile, led by the country's choices), `/countries/:iso3/memo` (country strategy briefs: AUS, CAN, DEU, FRA, GBR, IND, JPN, KOR, USA) | What should this country do, and why? |
| **Allied Industrial Base** | `/aib`, `/aib/partnerships`, `/aib/roles`, `/aib/diversification`, `/aib/agreements`, `/compacts`, `/compacts/:id` | Where does coordination change the outcome, who should work together, and in what form? |
| **About** (secondary) | `/about`, `/methods`, `/data`, `/glossary` | Methods, data and sources, glossary |
| Expert review (no tab) | `/review`, `/review/admin` | Blind-first expert review of selected cases (`src/review/`; [../docs/QC_REVIEWER_GUIDE.md](../docs/QC_REVIEWER_GUIDE.md), [../docs/QC_ADMIN_GUIDE.md](../docs/QC_ADMIN_GUIDE.md)) |

The recommendations come from the posture bundle (`public/data/posture/`, `scripts/91`) and the Allied Industrial
Base bundle (`public/data/aib/`, `scripts/84`). They use fixed thresholds and are the same for every reader. Every
page reads the six stages (`src/lib/stages.ts`): 11 technologies x 6 stages = 66 steps.

**Legacy index views.** The Energy Security, Economic Opportunity and Strategic Index scores (descriptive and
historical, not the decision rule) remain on the pages without a tab (`/overview`, `/country`, `/space`,
`/strategic`, `/partners`) and behind "Perspective: NEIS baseline" in the Country profile. There, four preference
dials (Hawkishness, Bullishness, Populism, Climate) recompute those indices in a Web Worker (`src/engine/`, a
parity-tested port of `R/dashboard/`). The dials never change the recommended choices, observed facts or the
Allied Industrial Base results. A custom perspective lives in the page address.

**Old links keep working.** One-economy legacy links open the matching Country profile section (`/country/:iso3`
-> `#sectors`, `/strategic/:iso3` -> `#opportunities`, `/partners/:iso3` -> `#partnerships`); old module paths
are aliases (`/strategic-insights`, `/country-insights`, `/sector-insights`, `/sectors`, `/sectoral-compacts`,
`/allied-industrial-base`); three-stage links (`/sector/:tech/downstream`, `/upstream`, `/midstream`,
`?stage=Upstream`) open the six-stage pages with a notice (`src/shell/legacySector.tsx`); `/network` (the
retired allied network design) redirects to `/aib`.

## How the numbers are produced

```
R pipeline (scripts/0x-4x)                 data/processed/...
  -> scripts/85_export_dashboard_data.R    dashboard/public/data/  manifest.json + 4 data chunks
                                           (builders in R/dashboard/; checks the R engine at the
                                           NEIS baseline against the stored outputs)
  -> scripts/86_export_dashboard_fixtures.R  src/engine/__fixtures__/  parity fixtures
  -> scripts/87_export_sector_data.R       dashboard/public/data/sector/  Sector Insights: manifest.json
                                           + lazy chunks (pure helpers in R/dashboard/sector/; the
                                           hand-checked config/sector_dictionary.yml)
  -> scripts/88_export_country_data.R      dashboard/public/data/country/  Country Insights: manifest.json,
                                           a fact sheet per economy, drivers files and the policy memos
                                           (pure helpers in R/dashboard/country/; config/country_insights.yml)
  -> scripts/91_export_posture_data.R      dashboard/public/data/posture/  the six choices per cell (manifest,
                                           core, one drawer file per economy)
  -> scripts/84_export_aib_data.R          dashboard/public/data/aib/  Allied Industrial Base
  -> scripts/93, scripts/92, scripts/43    development partnerships, country strategy briefs, agreements
node scripts/review/build-review-data.mjs  dashboard/public/data/review/  expert-review data and release id
```

Run order and the full list of steps: the repository [README](../README.md) ("Reproducibility"). Each
manifest records `created` and `git_commit` (`src/shell/buildInfo.ts` reads them); every review submission
carries the review release id built from them.

* `R/dashboard/` is the reference engine: at the baseline it reproduces the pipeline bit for bit.
* `src/engine/` is its TypeScript port (no dependencies), run in a Web Worker (`src/worker/`). It
  is parity-tested against R fixtures: pillars, Policy and the Strategic Index to 1e-9 (in practice
  bit-identical) and partnership arrays exactly (the bundle is lossless: no quantisation). The
  greedy network solver (L5) went with the retired allied network design (2026-10-17).

### Data files (`public/data/`)

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | at start (no-cache) | vintage, flags, notes, validation report, sources, baseline parameters, chunk list |
| `core.<md5>.osb` | at start | pillars, Policy, Strategic Index inputs and the shipped baseline tables |
| `vars.<md5>.osb` | Country view (variable drill-down) | variable-level components |
| `partner.<md5>.osb` | Partners, PSI metric | Friendshore / Opportunity components, dyads, baseline best-five and PSI |
| `ties.<md5>.osb` | lazily, main thread: Partners view, Country partners section (never the engine) | IMF direct investment positions by partner (both directions, latest year, and the 2020-2024 mean Friendshore ranks; conduit centres flagged), CM-FDI factory-project flows by sector (the Components and Assembly cells carry the same flows, `meta.cmfdi_stages`: count by project), Development Potential by sector, agreements, UN-vote alignment, top partners by best-five Friendshore / Partnership Opportunity (R/dashboard/dash_ties.R; scripts/85 checks the positions reproduce the investment tie bit for bit) |
| `pcell-<slug>.<md5>.osb` | lazily, one sector (66: `pcell-batteries-refining` ...): Country partners section | each economy's top 10 friendshoring opportunities (Friendshore) and export markets (Partnership Opportunity) in that sector at the baseline, with the Partnership Strength rank; the Partners view ranks the sector in the worker instead (`cellPartners`, so the dials apply) |

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
| `sector-fdi.<h8>.osb` | lazily, by the CM-FDI views only (Solar, Wind, Batteries and EV Components and Assembly, which carry the same projects: profile Investment section, Explore, Country in sector, Sources download) | CM-FDI cross-border manufacturing projects (Net Zero Industrial Policy Lab, edition 2026.10.05, CC BY 4.0): every project with its source text and links per field, home -> destination flows, parent-HQ group splits and announcements by year, for all projects and for those the partnership indices use (`data/raw/cm_fdi_projects.csv`, or `--cmfdi <csv>`; `--no-fdi` leaves the chunk and its measures out) |

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
`wdi_country_info.csv` and `economic_security_agreements.csv`.

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | Country pages (no-cache) | roster, tiers, quadrant counts, memo entries, sources, validation |
| `facts-<ISO3>.<h8>.json` | a Country profile | the economy's fact sheet (scores, drivers of the first sectors, policy, partners, the allied pick) |
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

Memos under revision (2026-10-17): with the original allied network design retired, the 31 memos are
being rewritten. `config/country_insights.yml` `memo_revision.before` hides every memo generated before
that time: scripts/88 marks it stale in the manifest, and the memo page, the profile's memo panel and its
(disabled) GENERATE MEMO button say the memo is being revised. The memo files are kept; a memo rendered
after the cut-off is shown again. The fact sheets dropped their `network` section (the design role) and
carry the allied pick as a top-level `pick` (memo tokens `{{pick.cell}}`, `{{pick.margin}}`, ...).

### Sectoral Compacts data (`public/data/aib/`)

Written only by `scripts/84_export_aib_data.R` (builder in `R/dashboard/aib/aib_export.R`) from
scripts/32's `aib_results.rds`: plain JSON, every array field wrapped in `I()`, the manifest written last.

| File | Loaded | Content |
|---|---|---|
| `manifest.json` | every Sectoral Compacts page (no-cache) | schema `osi-aib` (version 3 in the shipped bundle), created, git commit, universe, headline shock and scenario, the document's and the scenario files' names |
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
`--paper` (copy the preview PDF to `public/paper/`; off by default). Then rerun the tests below.

Run the export only on a finished pipeline run: it also reads raw inputs (CAT ratings, GDP).
`scripts/86 --set real` refuses a bundle other than the one
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
  views/       Strategic: Overview, Country, Space, Strategic, Partners, Methods, Data
  aib/         Sectoral Compacts: contract.ts (the data contract with scripts/84), data/ (store, hooks),
               derive/ (pure helpers: classes, coverage, nodes, flows, coalition, roles), charts/,
               views/ (index, compact, about), AibLayout, routes, nav
  sector/      Sector Insights: contract.ts (the data contract with scripts/87), data/ (store),
               derive/ (pure derivations), views/, charts/ (the Sector chart kit), chrome/
               (SectorBar, SectionRail, BottomSheet, ThemeChips), url.ts, format.ts
  country/     Countries: the Country profile, briefs (memo view), contract with scripts/88
  posture/     the six choices (contract with scripts/91), the trusted network, the cell drawer
  agreements/  partnership agreements (contract with scripts/43)
  review/      the expert review pages (/review, /review/admin)
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
npm run test:country-real # the real Country bundle (skipped without public/data/country/manifest.json)
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

Budget (`npm run budget`, gzip sizes; it also fails when a manifest lists a missing chunk). The limits, with
the measured size behind each, are in `scripts/check-budget.mjs` (`LIMITS`), the single source of truth; do not
copy the numbers here. It checks the first visit to Home, each module's landing and profile, the expert review
pages and data, the posture data, each data folder and all data together, and fails any route chunk over its
limit. Raise a limit only to the measured size plus about 15%, with a dated comment.
`BUDGET_DIST=<dir>` checks a build made with `vite build --outDir <dir>`; `BUDGET_SECTOR_DIR=<dir>`
checks another Sector bundle (e.g. `tests/fixtures/sector/synthetic`). Nothing is quantised.

## Deploy (GitHub Pages)

The public build is <https://lrcarey222.github.io/aib-explorer/>, served by GitHub Pages from the separate
repository `lrcarey222/aib-explorer`, which holds build output only. It is published by hand from main; no
workflow in this repository deploys it.

```bash
VITE_BASE=/aib-explorer/ npm run build   # Git Bash on Windows: prefix with MSYS_NO_PATHCONV=1
npm run budget
```

Then assemble the site in a checkout of the Pages repository and push it. The assembly script is not yet tracked in
this repository; it empties the site (keeping `.git` and `README.md`), copies `dist/` without source maps, adds
`.nojekyll`, a `404.html` copy of the app page (so deep links boot the app) and a no-index robots tag, ships
only the current country strategy briefs (not the hidden legacy memos) and the partnership memos. Publishing is an outward-facing step: check the
bundle stamps and the review release id first ([../docs/QC_RELEASE_CHECKLIST.md](../docs/QC_RELEASE_CHECKLIST.md)).
GitHub Pages cannot set response headers, so the Pages copy runs without the Content-Security-Policy
([../docs/QC_DEPLOYMENT.md](../docs/QC_DEPLOYMENT.md)).

**Legacy: Netlify.** `netlify.toml` is kept from an earlier plan and is not used by the public build. It sets the
base directory, caching (immutable chunks, revalidated manifests), 404s for missing data files and a strict
Content-Security-Policy with no third-party origins.

Browsers: current Chrome, Edge, Firefox and Safari. Gzip is decoded natively where
`DecompressionStream` exists (Safari 16.4+, Firefox 113+), with a small fallback otherwise.

## What is generated and what is committed

| Path | Made by | Committed |
|---|---|---|
| `public/data/` | `scripts/85` | yes (the maintainers' decision; the Pages build and the real-data tests read it) |
| `public/data/posture/` | `scripts/91` | as `public/data/` |
| `public/data/review/` | `node scripts/review/build-review-data.mjs` | as `public/data/` |
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
