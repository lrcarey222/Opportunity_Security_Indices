# Dashboard application architecture and the new UX (worktree `C:/Users/LCarey/osi-worktrees/qc`, branch `feat/qc-review-system`, HEAD eabf5591)

All paths below are relative to `dashboard/` unless stated otherwise.

## 1. Entry, routers, base path

- **Entry.** `src/main.tsx:1` imports `./artifactFetch.ts` first, then fonts and `styles/globals.css`, then builds the router. `VITE_ARTIFACT === '1'` uses `createMemoryRouter(routes, { initialEntries: [artifactEntry(hash)] })`; any other build uses `createBrowserRouter(routes, { basename: basename() })` (`src/main.tsx:14-17`).
  - Artifact builds also add a document-level click handler (`src/main.tsx:21-31`). It sends plain `<a href="/...">` links through `router.navigate`. It only touches hrefs that start with `/`; external links and `download` links pass through.
- **`artifactEntry`** (`src/routes.tsx:120-123`) maps a hash such as `#countries.JPN` to `/countries/JPN`.
- **`basename()`** (`src/routes.tsx:126-129`) derives the router basename from Vite's `BASE_URL`. `vite.config.ts:58` sets `base: process.env.VITE_BASE ?? '/'`. There is no other build-time `define`: no git SHA, no build date, no app version is injected (`vite.config.ts:57-63`).
- **`artifactFetch.ts`** (lines 6-18). In artifact builds it monkey-patches `fetch` so that `*.osb` requests are served from base64 `*.osb.txt` files.
- **GitHub Pages.** There is no Pages workflow in this repo. `.github/workflows/dashboard.yml` runs `npm ci`, `typecheck`, `test`, `build` and `budget` only. The root `README.md:930` says a public beta is "published by hand on GitHub Pages from a separate repository, lrcarey222/aib-explorer".
  - `public/404.html` is a plain "Not found" page, not an SPA redirect shim. Any Pages deep-link handling therefore lives in that other repo and cannot be verified here.
- **Netlify** (`netlify.toml`):
  - `/data/*` returns 404 (line 17).
  - `/*` falls back to `/index.html` 200 (line 23).
  - Manifests are `no-cache`; content-hashed chunks are immutable.
  - CSP (line 115): `connect-src 'self'` … `form-action 'none'`.
  - **Implication for "Report a data issue":** you cannot POST to a third-party endpoint or use `<form action>`. Only same-origin fetch, `mailto:` links or GitHub-issue links (navigations) are possible.

## 2. Routing map (`src/routes.tsx:82-116`)

Root: `{ path: '/', element: <AppRoot/>, hydrateFallbackElement: <BootScreen/>, errorElement: <RootError/> }`. Its children:

| Path | Implementation |
|---|---|
| index | `HomeOrLegacy` (Home = HomeView, the four cards; legacy `/?h=` redirect) |
| `start` | former Home (`StartPage`) |
| pathless `id:'strategic'` | lazy `shell/StrategicLayout.tsx` (engine + dials) around `STRATEGIC_ROUTES`: `/overview /country /space /strategic /partners` (lines 68-74) |
| pathless `id:'about-engine'` | lazy `shell/AboutEngineLayout.tsx` around `/methods`, `/data` (lines 77-80). This starts the engine worker via `EngineScope` (`shell/AboutEngineLayout.tsx:17`) |
| `sector` | `sectorRoute` (`src/sector/routes.tsx:36-57`), lazy `SectorLayout` |
| legacy 3-stage sector links | `LEGACY_SECTOR_ROUTES` (`shell/legacySector.tsx`) |
| `countries` | `countryRoute` (`src/country/routes.tsx:18-32`), lazy `CountryLayout` |
| `aib` | `aibHubRoute` (`src/aib/routes.tsx:83-93`), lazy `AibLayout` |
| `compacts` | `aibRoute` (`src/aib/routes.tsx:96-114`), lazy `AibLayout` |
| `partnerships` | `partnershipRoute` (`src/partnership/routes.tsx:9-15`). It has **no layout component**: two lazy children |
| `about`, `glossary` | lazy shell views |
| aliases | `sectors`, `sector-insights`, `strategic-insights`, `network/:stage?`; `LEGACY_PROFILE_LINKS` (`country/:iso3`, `strategic/:iso3`, `partners/:iso3` go to `/countries/ISO#section`) |
| `*` | `ToHome` with the notice "That page does not exist; showing the home page." |

### Conclusion pages and their Explorers

- **Sector.** `/sector` (index) and `/sector/:tech` both use `sector/conclusion/SectorConclusionView.tsx` (`sector/routes.tsx:42,51`).
  - Explorer: `/sector/:tech/chain` (TechChainView), `/sector/:tech/:stage` (SectorProfileView), `/sector/:tech/:stage/:iso3`, `/sector/all`, `/sector/minerals[/:mineral]`, `/sector/explore/:theme?`, `/sector/concentration`, `/sector/sources`.
  - `/sector` redirects to the last brief tech or `batteries` (`SectorConclusionView.tsx:51-56`). Non-headline techs (e.g. Semiconductors) redirect to `/chain`.
- **Country.** `/countries/:iso3` uses `country/views/CountryProfileView.tsx`, with the conclusion sections in `country/views/profile/CountryConclusion.tsx` (memo-ordered sections, `CONCLUSION_SECTIONS`).
  - `/countries/:iso3/evidence` and `/policy` use the same component, which reads the view from the path (`country/routes.tsx:28-29`). `/countries/:iso3/memo` is MemoView.
  - The Explorer also includes the legacy score views.
- **Partnership.** `/partnerships` and `/partnerships/:pair` (e.g. `USA_JPN`) use `partnership/PartnershipView.tsx`. The Explorer is `/aib/partnerships` (and the legacy `/partners`).
- **Compact.**
  - `/compacts` (index) is `compact/CompactSelectionView.tsx`, wrapped by `landingOrWorkstreams` (`aib/routes.tsx:102`).
  - `/compacts/brief/:sid` is `compact/CompactSpecView.tsx` (`aib/routes.tsx:104`).
  - Explorer: `/compacts/explore` (StrategicCompactsView), `/compacts/s/:sid`, `/compacts/workstreams`, `/compacts/:id`, `/aib`, `/aib/roles`, `/aib/diversification`.

### Conventions a new lazy route must follow

- **Lazy chunk.** Use the `view(load)` helper, defined identically in each routes file (`src/routes.tsx:41`, `sector/routes.tsx:10`, etc.): `lazy: async () => ({ Component: (await import(...)).default })`. The view file must `export default`.
- **`errorElement: <RouteError />`** on every top-level child (`src/routes.tsx:92-105`). `RouteError` (`shell/RouteError.tsx:18`) shows a Reload notice for chunk-load failures.
- **Error boundary.** `AppRoot` already wraps the outlet in `ViewErrorBoundary resetKey={pathname}` (`shell/AppRoot.tsx:68-70`).
- **Route order.** Add the route **before** `{ path: '*' }` (`src/routes.tsx:113`).
- **Title.** Call `useTitle(view, module)` from `shell/title.ts:41`.
  - `MODULE_LABEL` (`title.ts:15-31`) has no `review` key.
  - `pageTitle` falls back to the raw module string (`title.ts:35`), so `useTitle('X','review')` would render "X | review | Allied Industrial Base Explorer | NEIS Center". Add a label.
- **Nav registration.** Tabs come from `MODULES` (`shell/modules.ts:37-43`: Sector, Country, Partnership, Compact, then Methods as `secondary`). `ModuleId` is `'sector'|'country'|'partnership'|'compact'|'about'` (`modules.ts:21`).
  - `placeOf` (`modules.ts:176`) returns `null` for unknown first segments. A `/review` path therefore gets no active tab, no tier-2 nav and a `null` place in the Footer.
  - A visible tab needs all of: the `ModuleId` union, `MODULES`, `placeOf`/`tabOf`, `Header.tsx` `VIEWS_LABEL` (lines 65-71), `tabHref` (lines 80-86), `moduleItems` (lines 26-63), and `lastLocation.ts`.
  - Tier-2 links are static lists (`SECTOR_NAV` :89, `COUNTRY_NAV` :112, `PARTNERSHIP_NAV` :223, `COMPACT_NAV` :236, `ABOUT_NAV` :164), or links a module layout registers through `useRegisterModuleNav` (`shell/ModuleNav.tsx`).
- **Bundle budget.** `scripts/check-budget.mjs` measures named route chunks. A new route should add its own figure so it cannot leak into the Home first visit (Home ≤ 400 kB, `LIMITS.home`).

## 3. Shell

- **`AppRoot`** (`shell/AppRoot.tsx:48-76`):
  - `ModuleNavProvider`, skip links, `<Header>`, then `<main id="main">` holding `PreviewNotice`, `RouteNotice` and `ViewErrorBoundary > Outlet`, then `<Footer/>`.
  - A route-change effect calls `rememberLocation` (in-memory only; `shell/lastLocation.ts:1-3` says "No storage: a reload starts from the URL").
  - `RouteNotice` reads `location.state.notice`, set by `shell/Redirect.tsx`.
  - `PreviewNotice` (lines 35-46) only appears in artifact builds. It is the **only feedback affordance in the app**: "Please leave feedback as a comment on the preview."
- **Header** (`shell/Header.tsx`):
  - Tier 1: logo, then the eyebrow "Allied Industrial Base Explorer" linking to `/`, then the tabs.
  - Tier 2 (≥1024 px): module views plus the persistent `HowToRead` (lines 150-172).
  - Below 1024 px: a Menu button and a mobile panel.
  - The prefetch hooks fire on tab hover (lines 73-77).
- **Footer** (`shell/Footer.tsx:21-87`):
  - Cite-as line, using `strategic?.paper?.title ?? PAPER_TITLE`.
  - Data vintage line, only for **Sector** and **Strategic**: "Sector data: export {date}, build {bundle_id[0:8]}." and "Strategic data: export …, build …, git {git_commit[0:10]}" (lines 44-54).
  - Links: Methods, Data and downloads, Sources & data, Country data notes, How compacts are derived, Glossary, neiscenter.org (lines 61-83).
- **Build info at runtime** (`shell/buildInfo.ts`):
  - A tiny `useSyncExternalStore` store with `{ strategic, sector }`, each a `BuildStamp { bundle_id, created, git_commit }` (lines 7-11).
  - Publishers: `setStrategicBuild` (line 33), called from `shell/EngineScope.tsx:39` only when the engine mounts; `setSectorBuild` (line 40), called from `sector/SectorLayout.tsx:143` and `shell/sectorManifest.ts:26`.
  - **No stamp exists for the Country, posture, AIB or partnership-memo bundles**, and no app-code commit or build date exists. On the Country, Partnership and Compact conclusion pages the footer usually shows **no** data release at all.

## 4. Data fetching

All data is static under `public/data/` (committed to git: `git ls-files public/data` = 1221 files). It is fetched from `${BASE_URL}data/...`.

| Bundle | Loader | Manifest stamp (current) |
|---|---|---|
| Strategic `.osb` | Web worker: `worker/client.ts:113-119` `EngineClient.create(dataBase = BASE_URL+'data/')`; `worker/host.ts`; `engine/bundle.ts`. Only under StrategicLayout and AboutEngineLayout | `manifest.json`: bundle_id `c1736c4a…`, created 2026-10-30T21:17:20Z, git_commit `a54a9d3`, osi_branch `feat/supply-chain-stages`; `validation` 70 checks (69 PASS, 1 INFO) |
| Sector `.osb` | `sector/data/store.ts:62`, `shell/sectorManifest.ts:12` | bundle_id `8f4910df…`, 2026-10-31, git `4bb2adc`; validation 180 (176 PASS, 4 WARN) |
| Country JSON | `country/data/store.ts:37` `httpCountrySource` (manifest `cache:'no-cache'`) | no bundle_id; 2026-10-31, git `4bb2adc`, `as_of` 2026-10-30; validation 47 (44 PASS, 3 WARN) |
| Posture JSON | `posture/store.ts:10-62` `PostureStore` (manifest no-cache; core plus per-country detail) | schema `osi-posture` v2, 2026-10-31, git `4bb2adc`, no bundle_id, no validation; counts: 214 countries, 14,124 cells, `not_assessed` 4,887, `watch` 6,585 |
| AIB JSON | `aib/data/store.ts:38-70` `httpAibSource` (manifest no-cache) | schema `osi-aib` v3, 2026-10-30, no git_commit, no validation; headlineShock `china_100` |
| Partnership/compact memos | `conclusion/memos.ts:88-108` (plain `fetch`, **manifest not no-cache**); `repairDeep` mojibake repair (lines 52-82) | schema `osi-partnership-memos` v1; 10 pairs and 3 compacts, **all `verified: false`**, `as_of` 2026-10-30 |

- The store pattern repeats in every module: a promise cache keyed by file, deleted on failure so retries work, plus a `setXStoreForTests` injection used by the UI tests.
- **Usable for automated QC:** the manifests' `validation` arrays.
  - The Strategic checks are shown on `/data` (`views/Data/DataView.tsx:128-140`).
  - The Sector checks are shown on `/sector/sources` (`sector/views/SourcesView.tsx:379-380`).
  - **The Country manifest's validation is not displayed anywhere.**
- **Engine self-check.** `engine/selfcheck.ts` compares the in-browser engine with the R baseline. Failures surface through `components/AppShell.tsx:130-135` `Banners`.

## 5. State and storage

- **URL state.** `src/state/url.ts` holds the Strategic scenario and view keys. `state/nav.ts` provides `useGoto`. `state/EngineProvider.tsx` and `appData.ts` hold the engine and static build data.
- **Module URL state:** `sector/url.ts`; `aib/scenarioUrl.ts` (`scn`, `scenarioCarry`); `posture/selection.ts` (`?node=Tech|Stage`, `?layer=`, lines 22-30).
- **lastLocation.** Per-module memory in `shell/lastLocation.ts` (in-memory).
- **localStorage.** Used **only once**: `sector/views/HhiExplainer.tsx:47,55` (a "seen" flag). There is no sessionStorage and no IndexedDB.

## 6. Design system to reuse for `/review`

### CSS tokens

`src/styles/globals.css`, `@theme` at line 12:

- Ink and paper: `ink-900 #111827` (page), `ink-850`, `ink-800 #1f2937` (panels and cards), `ink-700/600/500`; `paper #fff`, `paper-dim #d1d5db`, `paper-mute #9ca3af`.
- Accents: `signal #f58229`; `brand-blue #1889b0`, `brand-green #a3b63b`, `brand-teal #55c4c5`, `alert #e2574c`.
- Ramps: `seq-1..7`, `div-*`, `tri-1..9`.

### Component classes

| Purpose | Class (globals.css line) |
|---|---|
| Rule | `.rule-neis` (111) / `.rule-neis-sm` (121) |
| Labels | `.eyebrow` (135), `.label` (143), `.tabular` (153) |
| Containers | `.panel` (159), `.card` (166; orange top rule on hover) |
| Tags | `.tag` (217), `[aria-pressed]` (238), `.tag-framework`, `.tag-fixture` |
| Buttons | `.btn` (270) with `.btn-primary` (292), `-ghost` (302), `-quiet` (312), `-danger` (325), `-sm` (334) |
| Links | `.link` (341) |
| Inputs | `.field` (359) |
| Disclosure | `.disclosure` (449) |
| Tables | `.data-table` (611) |

### React primitives

- **`components/ui.tsx`:** `PageHead` (7), `FilterRow`, `Select` (35), `Segmented` (80), `Switch`, `Checkbox`, `Notice` (150; kinds info, warning, error), `Loading`, `EmptyState`, `StatTile` (206), `RankChip`, `Tag` (234; the "chip"), `Disclosure` (242), `ChangedDot`, `KeyValue`.
  - **No `Field`, textarea or form primitive exists.** The header comment lists "Field" but nothing is exported, and there is no `<textarea>` anywhere in `src`.
- **`conclusion/primitives.tsx`:** `ConclusionHead` (24), `BottomLine` (44), `DecisionCards`/`DecisionItem` (54-96), `DecisionBadge` (99), `ExplorerLink` (111), `MemoSection` (120), `ConclusionSection` (133), `EvidenceDrawer` (146; a `<details className="disclosure panel">`), `SourceMeta` (161), `EvidenceGrade` (183), `NotAssessed` (203), `ScenarioCompare` (222), `Guardrail` with `GUARDRAIL_MISSING` (257-305), `MemoProvenance` (308; "(draft: not yet independently verified)" when `verified === false`).
  - Evidence grades (`GRADE_WORDS` :176-180):
    - strong "Several independent measures agree." `#a3b63b`
    - moderate "The main measures agree, with gaps." `#f5c242`
    - weak "Thin or single-source data: treat as a lead, not a finding." `#e2574c`
    - A missing grade reads "Not graded".
- **Colours are hard-coded in the primitives** (`'#f58229'`, `'#9ca3af'`, `'#6b7280'`), not CSS variables.

### Posture and choice colours (canonical)

`posture/grammar.ts`:

- `POSTURE_COLOR` (line 44): `{ S:'#f58229', C:'#1889b0', D:'#a3b63b' }`. `POSTURE_LABEL`: Secure / Compete / Deploy / Not assessed.
- `VERB_COLOR` (lines 98-105): secure `#f58229`, preserve `#1889b0`, catch_up `#b4e7fe`, partner `#55c4c5`, accelerate `#a3b63b`, watch `#4b5563`.
- `VERB_OUTLINE`: watch `#9ca3af`.
- `VERB_LONG`, e.g. "Partner · Medium fence".
- `FENCE_OF_VERB`, `nodeFence`, `nodeFill`.
- `NOT_ASSESSED_HATCH` / `NOT_ASSESSED_BORDER`.

`theme/palette.ts` holds the entity colours (`GROUP_COLOR` china `#c96f7a`, core `#1889b0`, partner `#8b9d17`, rest `#6b7280`), `TRI`, `STAGE_NEUTRAL`, and `HATCH_STYLE`.

### Decision colours on the conclusion pages

- Sector `DECISION_COLOR` (`sector/conclusion/sectorCase.ts:80`): build `#8b9d17`, coordinate `#1889b0`, open/unchanged `#9ca3af`, evidence `#6b7280`.
- Compact `DECISION_INFO` (`compact/decide.ts:31-50`): Sectoral Compact `#f58229`, Narrower arrangement `#98c4d8`, No compact needed `#4b5563`.

### Drawers and dialogs

- **`posture/CellDrawer.tsx`** is the richest "why" surface:
  - Fixed side panel or bottom sheet, `role="dialog"`, closes on Escape (lines 803-820).
  - Sections: `DecisionCard`, then "The full reasoning", then `PartsBlock`, then `DRAWER_QUESTIONS` (lines 65-71) Need / Prize / Position / Choice / Policy alignment.
  - `PostureNodeDrawer` (909) is the connected one, driven by `?node=`.
- Others: `sector/chrome/BottomSheet.tsx`, `shell/HowToRead.tsx` (popover), `components/InfoTip.tsx` (glossary popover).

## 7. Canonical vocabulary (`src/content/vocabulary.ts`, pure data)

| Constant | Line | Value(s) |
|---|---|---|
| `CHOICE_NAME` | 243-250 | Secure, Preserve, Catch Up, Partner, Accelerate, Watch |
| `CHOICE_ACTION` / `CHOICE_DEFINITION` / `CHOICE_PROSE` | 253-283 | — |
| `WATCH_PROSE_LABEL` | 286 | 'Low domestic priority' |
| `PARTNER_LONG_NAME` | 291 | 'Partner / Diversify' |
| `YARD_NAME`, `FENCE_NAME`, `YARD_AND_FENCE`, `YARD_DEFINITION`, `YARD_CHOICES`, `FENCE_TITLE`, `FENCE_DEFINITION` | 297-332 | e.g. 'High fence · Secure' |
| `YARDS_NOT_A_SCALE` | 350 | — |
| `STRATEGY_STEPS` | 359 | need, prize, position, choice, policy |
| `ALIGNMENT_NAME` | 420-427 | Broadly aligned / Needs more attention / Too much intervention / Wrong tool for the problem / Mixed signals / Not enough policy evidence |
| `RATIONALE_NAME` | 443 | Control-system risk / Chokepoint risk / System resilience |
| `AIB_CLASS_NAME` | 452 | COORDINATE 'Coordinate trade', BUILD 'Build capacity', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION' 'Low intervention', 'INSUFFICIENT DATA' 'Not enough evidence' |
| `SCENARIO_NAME` | 470 | baseline "Today's network", managed 'Managed Interdependence', decoupling 'Major-supplier disruption' |
| `STATE` | 622 | e.g. `notAssessed`, `dataLimitation` 'Missing or thin data are shown as uncertainty, never as a low priority.' |
| `AVOID_RE` | 237 | bans optimize / leverage / unlock / synergy …; UI tests assert against it |

- Rules for page authors: `content/VOCABULARY.md`.
- **Glossary:** `content/glossary.ts` has 107 entries (`GLOSSARY_TEMPLATES` :62, `{{placeholders}}` filled through `GlossaryVarsProvider`), used by InfoTip, Term and `/glossary`.
- **Methods page:** `views/Methods/MethodsView.tsx` renders `content/methods.md?raw` with marked and KaTeX, then `fillPlaceholders` with build constants (`methodsFill.ts`).
  - Thresholds stated in `methods.md` (lines 7-9): the need-score floor `0.10`; contenders = economies with "at least 0.5% of world supply"; momentum takes "15% of the trade family"; the Catch Up anchor needs 0.5% of world supply or exports, 0.5% of operating capacity, 1% committed, or an adjacent stage with 5%.
- **Compact rule** (`compact/decide.ts:53-54`, `DECISION_RULE`): "at least 25% of the chain's lost supply, or US$1bn". This matches `config/aib_strategic_layer.yml:48-49` (`material_share: 0.25`, `material_usd: 1.0e+9`) and `docs/aib_methodology.md:1187`.

## 8. Existing feedback, review and provenance features

- **Feedback, report, contact, survey or comment UI:** none. A grep finds only the artifact preview notice (`AppRoot.tsx:42`).
- **Provenance that already exists:**
  - `SourceMeta` and `MemoProvenance` (primitives).
  - The Sector `SectorChartCard`/`MeasureCaption`, which requires provenance (`sector/charts/SectorChartCard.tsx:1-6`; caption contract `sector/charts/caption.tsx`), and the source "Known issues" column (`SourcesView.tsx:113`).
  - The AIB `PhysicalWarning` (trade not confirmed by production data, `aib/charts/ClassChip.tsx:50,89`). This is the existing trade-vs-production diagnostic.
  - The memo provenance fields on `MemoDoc` (`country/contract.ts:858-891`): `generated_at, verified_at, writer, verifier, facts_sha256, src_sha256, claims_sha256, disclosure`.
- **Memo QC fields.** `dashboard_redesign/product_redesign.md:52` confirms the only "Expert-QC" field is the `verified` flag. A threat-amelioration score is "Not implemented" (`product_redesign.md:45`).

## 9. Tests

- **`vitest.config.ts` projects:**
  - `unit`: node, `src/**/*.test.ts`, excluding `*.parity|sector|country|aib.test.ts` (lines 16-22).
  - `ui`: jsdom, `src/**/*.test.tsx`, esbuild `jsx:'automatic'` (lines 24-34).
  - `parity-synthetic`, `parity-real`, `sector-real`, `country-real` (env `COUNTRY_REAL=1`), `aib-real`.
  - `npm test` runs unit + ui + parity-synthetic (`package.json`).
- **How UI tests render routes:** `render(<RouterProvider router={createMemoryRouter(routes, { initialEntries: [path] })} />)`, using the real `routes` (`compact/compact.view.test.tsx:57`; `app.test.tsx:16-21`).
  - Stubs: `matchMedia` (where `min-width` matches), `ResizeObserver`, `scrollTo`, and `Request` (signal stripped) (`compact.view.test.tsx:19-46`).
  - Stores are injected with `setAibStoreForTests` / `setCountryStoreForTests` / `setEngineClient`.
  - Teardown resets with `resetLastLocations`, `resetBuildInfo`, `resetSectorManifestCache`, `resetPrefetch`.
- **Test helpers** (`components/testing/`):
  - `bareAcronyms(container)` (`acronyms.ts`, `ACRONYM_SCOPES`), asserted as `toEqual([])` on every route.
  - `cssHex` (`color.ts`), for asserting `VERB_COLOR` fills.
- **Golden guard** (`src/regression/golden.test.ts`, unit project):
  - Decodes the shipped `public/data` (posture, country, AIB) for USA, JPN, KOR, AUS, IND and DEU through the dashboard's own derive functions.
  - Compares with `toMatchFileSnapshot('./__snapshots__/golden-outputs.json')` (26,482 lines). Large results are stored as sha256 hashes.
  - `describe.skipIf(!have)`. Update command: `npx vitest run --project unit src/regression -u`.
  - This is the natural seed for "golden-case regression tests".

## 10. Budgets (`scripts/check-budget.mjs`)

- `LIMITS` (lines 58-95): home 400 kB, strategic 3 MB, sectorLanding 700 kB, sectorProfile 1.3 MB, countryProfile 900 kB, countryData 28 MB, aibLanding 700 kB, aibCompact 900 kB, aibData 2 MB, aibScenario 250 kB, allData 57 MB, postureData 2.4 MB.
- Per-file limits: `COUNTRY_FILE_LIMIT` (line 105) and `BRIEF_FILE_LIMIT` (line 109).

## Recommendations (hook points)

1. **`/review` route.**
   - Add `{ path: 'review', lazy: view(() => import('./review/ReviewLayout.tsx')), errorElement: <RouteError />, children: [...] }` in `src/routes.tsx` before `*` (line 113).
   - Make it a self-contained lazy layout, like `AibLayout`/`CountryLayout`, that loads the stores it needs. Do **not** put it under `AboutEngineLayout`, which would start the worker.
   - Keep it off `MODULES`: it is internal. But extend `placeOf` (`modules.ts:176`) with a `'review'` Place, so the Footer and Header know it.
   - Add `review: 'Review'` to `MODULE_LABEL` (`title.ts:15`).
   - Add a budget figure.
   - Build it from `PageHead`/`ConclusionHead`, `panel`/`card`, `DecisionCards`, `Tag`, `Notice`, `.data-table`, `Segmented`, `Select` and `.field`.
   - Colour postures only with `VERB_COLOR`/`VERB_OUTLINE`/`NOT_ASSESSED_HATCH` and label them with `VERB_LONG`/`CHOICE_NAME`.
   - Exports: reuse `lib/download.ts` (`downloadText`, `downloadBlob`), `lib/csv.ts` (`rowsToCsv`, `withBom`) and `lib/zip.ts` (`zipTexts`).
   - Reviewer state can only go in localStorage, wrapped in try/catch like HhiExplainer, plus JSON export/import. There is no backend, and the CSP blocks third-party connects.
2. **Site-wide "Report a data issue".**
   - Place it in `AppRoot` just before `<Footer/>` or as a Footer link (`Footer.tsx:61-83`).
   - Context to capture: `pathname+search` (which already encodes `?node=`, `?c=`, `scn`, `:pair`, `:sid`), the selected node, and **all** data stamps.
   - First extend `buildInfo.ts` with `country`, `posture`, `aib` and `memos` stamps. Publish them from `country/data/store.ts`, `posture/store.ts`, `aib/data/store.ts` and `conclusion/memos.ts`, since these manifests currently publish nothing.
   - Add a Vite `define` (`__APP_COMMIT__`, `__BUILD_DATE__`) in `vite.config.ts`.
   - Deliver the report as a `mailto:` link, a prefilled GitHub-issue link, or a JSON download. A form POST is blocked by `form-action 'none'`.
3. **"Why this result?" drawer.**
   - Generalise `EvidenceDrawer` + `SourceMeta` (primitives) into a provenance drawer that takes `{measure, source, date, scenario, caveat, grade, bundle stamp, memo hashes}`.
   - Anchor it in:
     - `CellDrawer`'s `DecisionCard` ("Explore full evidence") and its five `DRAWER_QUESTIONS`;
     - the Sector `StepCase.tsx` cards;
     - Compact `SPEC_ROWS` rows (`decide.ts:225`);
     - the Partnership `LogicChain.tsx` steps;
     - the AIB `ClassChip`/`PhysicalWarning`.
   - For memo-derived text, show `MemoProvenance` plus `verifier`/`verified_at`/`claims_sha256` from the `MemoDoc`.

## Problems noticed

- **Footer vintage is partial.** The footer shows only Sector and Strategic stamps (`Footer.tsx:44-54`). Country, posture, AIB and memos never publish a stamp, and the app has no code SHA or build date (`vite.config.ts` has no `define`).
- **Mixed data releases.** The Strategic bundle is git `a54a9d3` (2026-10-30); Sector, Country and posture are git `4bb2adc` (2026-10-31). AIB has no git_commit; posture and AIB have no bundle_id or validation.
- **Stale Sector `pipeline_marker`.** It reads "pipeline 04-25 finished 2026-10-12", while `processed_mtime_max` is 2026-10-30T17:28:02Z.
- **Bundles are older than the processed outputs.** `stages/data/processed` has files from 2026-11-17 (`deployment_need_tbl.rds`, `current_import_exposure_tbl.rds`, `current_trade_dyads_tbl.rds`). The posture bundle is from 2026-10-31, so the deployment-case tag and the T12M exposure are not on this dashboard.
- **NIPO still uses the old three-stage taxonomy.** The Country manifest WARN "NIPO: measures tagged with the six supply-chain stages" (n 62,561) notes "stages in the run: Cross-cutting, Midstream, Upstream". **No dashboard page displays the Country validation.**
- **Sector WARN: China share mismatch.** "trade bases: matrix and exporter-reported China shares within 3 points" fails on 42 cells (e.g. batteries-refining, nuclear-refining). This is a trade-vs-reporter diagnostic candidate.
- **Draft memos shown on conclusion pages.** All 13 partnership and compact memos are `verified: false`, but they are rendered verbatim on the conclusion pages (`documentation_issues.md` #11).
- **Partnership memo manifest is fetched without `no-cache`** (`conclusion/memos.ts:96`), unlike every other manifest.
- **Dead `/aib/agreements` mapping.** `placeOf`/`partnershipNavKeyOf` map `/aib/agreements` to Partnership (`modules.ts:183,230`), but there is no route for it, so it falls into `ToAibHome` with "does not exist".
- **Budget gaps and drift.**
  - `check-budget.mjs` has no budget for `public/data/partnerships/` (about 18 MB on disk, PDFs and DOCX included), and `allData` omits it.
  - The header comment says "All data … <= 52 MB" while `LIMITS.allData` is 57 MB.
- **Stale comments and nav names.**
  - The Header comment (lines 3-7) still names "Sectors, Countries, Allied Industrial Base and … About"; line 115 says "seven tabs" (there are five).
  - `routes.tsx:1-2` and the `title.ts` comments still use the old tab names.
  - `documentation_issues.md` #5 lists tab names ("Country Strategy, Supply Chain Evidence …") that are themselves superseded.
  - `methods.md` says allied division "is set out under Allied Industrial Base, Sectoral Compacts, About", which are retired nav names.
- **Colour collisions across contexts.** Sector `DECISION_COLOR.coordinate` `#1889b0` is the same blue as `VERB_COLOR.preserve` and `POSTURE_COLOR.C`. `build` `#8b9d17` is close to `accelerate` `#a3b63b`, and the same hue is `GROUP_COLOR.partner`. The primitives hard-code hex values rather than tokens. `globals.css` still defines `--color-stage-up/mid/down`, even though `palette.ts:26-32` says a stage is never a colour.
- **Sector title uses the raw slug.** `useTitle(\`${tech}: opportunities and risks\`, 'sector')` gives e.g. "electric-vehicles: …" (`SectorConclusionView.tsx:68`).
- **`MethodsView` imports `useTitle` from the legacy re-export** `App.tsx` rather than `shell/title.ts`, and `/methods` boots the engine worker.
- **`ui.tsx` header advertises a `Field` component that does not exist.** There is no textarea or form primitive anywhere, so the review and report forms must add one.
- **No SPA fallback for GitHub Pages in this repo.** `public/404.html` is a static "Not found" page, so the Pages beta (separate repo) depends on unverified routing there. The artifact build relies on the memory router, so URLs (and any "report" context taken from `window.location`) do not reflect the page. Use the router location instead.
