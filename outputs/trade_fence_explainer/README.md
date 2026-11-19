# US trade fences across the electro-industrial stack: chart package

Charts, data and code for the NEIS Center Substack explainer on US trade restrictions as of **4 December 2026**.
Built on branch `feat/trade-fence-explainer` (off `main` 2d0ae52d). Editorial notes: [TRADE_FENCE_EXPLAINER_CHARTS.md](TRADE_FENCE_EXPLAINER_CHARTS.md).
Known gaps: [DATA_GAPS.md](DATA_GAPS.md). Review sheet: [contact_sheet.html](contact_sheet.html).

## What is here

| File | What |
|---|---|
| `00a_electro_industrial_trade_overview`, `00b_trade_rewiring`, `00c_policy_timeline` `.{png,svg}` | Opening section: what the US trades, how sourcing changed, policy timeline |
| `01_fence_rising.{png,svg}` ... `08_trade_response.{png,svg}`, `05b_verdict_summary.{png,svg}` | The tariff-fence charts, 6.5 in wide (PNG 300 dpi = 1950 px) |
| `analysis/fence_change_vs_diversification.png` | Analytical scatter (not for publication) |
| `data/00a_*.csv`, `data/00b_*.csv`, `data/00_policy_timeline.csv`, `data/fence_change_vs_diversification*.csv` | Opening-section data: trade by technology and stage, diversification metrics, timeline, fence-change analysis |
| `data/01_*.csv` ... `data/08*.csv` | Chart-ready data, one or more files per chart |
| `data/A_B_cell_exposure_and_dependence.csv` | Supporting tables A (trusted-partner exposure) and B (coercive dependence) |
| `data/C_import_response.csv` | Supporting table C (import response by product group) |
| `data/D_domestic_response.csv` | Supporting table D (US production, capacity, prices; copy of the reference file) |
| `data/qc_checks.csv` | The quality checks, pass/fail with detail |
| `data/build_info.csv` | Data month, snapshot date, tracker commits |
| `contact_sheet.html` | All charts on one page, plus a 375 px (phone) column |

## How it is built

```
scripts/28_build_us_trade_restrictiveness.R              existing audit build (now also saves the line table)
scripts/trade_fence_explainer/fetch_comtrade.R           UN Comtrade downloads (2024 partner totals; monthly HS 854143 by partner)
scripts/trade_fence_explainer/statutory_variants.py      Trade War Tracker engine: 15 Aug and 4 Dec 2026, full and without s.232/301/338
scripts/trade_fence_explainer/01_build_data.R            all CSVs and QC checks
scripts/trade_fence_explainer/03_build_trade_overview.R  opening section data (trade overview, diversification, timeline) and QC T1-T14
scripts/trade_fence_explainer/02_draw_charts.R           PNG/SVG and the contact sheet (publication sequence first)
scripts/trade_fence_explainer/check_s338_hts.py          checks the Section 338 Canada basket against HTSUS chapter 99 note 51
R/trade_fence_explainer/explainer_data.R                 pure data functions (verdict mapping, dollar weighting, scenarios)
R/trade_fence_explainer/explainer_charts.R               pure chart functions and the visual system
R/trade_fence_explainer/trade_overview.R                 pure functions for the opening section (stage split, sourcing, HHI, quadrants)
R/trade_fence_explainer/trade_overview_charts.R          charts 00a, 00b, 00c and the analysis scatter
config/trade_fence_explainer.yml                         settings (dates, cells, partners, product groups, file paths)
data/dec4_2026_policy_snapshot.csv                       measures legally in force on 4 Dec 2026 (55 rows, sources)
data/reference/trade_fence_explainer/                    Dec 4 AD/CVD file, engine extra actions, chart 8 events, domestic data, fence-tools inputs
scripts/trade_fence_explainer/fence_tools/               the Python that built US_Fence_Tools_by_Stage.xlsx (vendored for the record)
```

## Refresh

```bash
Rscript scripts/28_build_us_trade_restrictiveness.R                  # OPSI_TWT_REFRESH=true for a new Census month
python scripts/trade_fence_explainer/statutory_variants.py <trade-war-redux-2025 clone> data/raw/trade_war_tracker/statutory_variants \
  --dates 2026-08-15 2026-12-04 --variants full no232 no301 no338 \
  --extra data/reference/trade_fence_explainer/statutory_extra_actions.csv --hs6 <basket hs6 list>
Rscript scripts/trade_fence_explainer/fetch_comtrade.R 202401 202608      # partner totals, HS 854143 by partner, basket X/M by month
python -I scripts/trade_fence_explainer/check_s338_hts.py data/raw/hts/chapter99_current.pdf <clone>/tariff-lists/canada-338-autos-2026-10-30.csv \
  data/raw/trade_war_tracker/statutory_variants/basket_hs6.txt data/reference/trade_fence_explainer/s338_hts_check.csv
Rscript scripts/trade_fence_explainer/01_build_data.R
Rscript scripts/trade_fence_explainer/03_build_trade_overview.R
Rscript scripts/trade_fence_explainer/02_draw_charts.R
```

Use the full path to Rscript on this machine (`C:\Users\LCarey\AppData\Local\Programs\R\R-4.4.1\bin\Rscript.exe`). The engine
needs Python with pandas (<3), pyarrow and nbformat (the venv in `osi-worktrees/scratch_twt/venv` has them) and a full
clone of github.com/tradewartracker/trade-war-redux-2025. For a new Census month also move `statutory_base_date` to the
middle of that month, review `data/reference/adcvd_orders.csv` and its Dec 4 copy, and re-check the policy snapshot.
Chart 4 reads frozen copies of the fence-tools workbook's derived tables (built 2026-11-08 in `osi-worktrees/scratch_twt`):
sha256 `f0064041851dbff8319bc643afa42353ae086b0c91bfc8a626f81404454dce84` (`fence_stage_tbl.csv`),
`c23c1adc8807abe5753a38ce0150512a0b23bb490cceaa193972f71f3d5094d1` (`fence_measures.csv`),
`cfc7741f8cbb3f078ce67f3c6676bfda2b2f2006cfdc0994b2cb869d216ebdd7` (`td_scope_review.csv`). Inputs read from `data/processed/`: `geo_alignment_tbl.rds`, `outputs/strategic_position_tbl.csv` and `cm_fdi_projects_tbl.rds`
(copies of the stages worktree's files, identical by md5 on 2026-11-09).

## Concepts kept apart

- **Observed tariffs**: Census calculated duty / customs value by HS10 line and partner, latest month **August 2026**
  (the Trade War Tracker's files, commit a9063f35). Census duty excludes AD/CVD.
- **Statutory measures in force on 4 December 2026**: `data/dec4_2026_policy_snapshot.csv`. Modelled as *observed August rate +
  the change in statutory added duty between 15 August and 4 December* from the tracker's tariff-action engine (commit 71ff5a8),
  extended with the Section 232 polysilicon tariff (Proclamation 11052) as an extra action, plus AD/CVD deposits in force on
  4 December (`adcvd_orders_dec4.csv`). The engine change touches 1.1% of 2024 import weight (Canada s.338 baskets, s.232
  polysilicon). Minimum import prices are **flagged, not converted** into tariff equivalents.
- **AD/CVD**: all-others / country-wide cash deposits from Federal Register notices, added once per covered line, lines capped
  at 100% (as in the audit). Never in Census duty, never in the engine, so never double counted (QC 9).
- **Trade restrictiveness**: the Waugh Trade Restrictiveness Index, sqrt(sum w t^2 / sum w) on 2024 HS10 x partner weights,
  exactly as in the audit (it reproduces the tracker's published TRI to 2e-15). Charts 3 and 8 use effective rates
  (duties / imports), labelled as such. TRI is never called an average tariff or multiplied by imports.
- **Supplier groups**: the posture engine's rule (`sp_coercive_pairs()`), unchanged: trusted = US-leaning UN-voting bloc;
  coercive = China, Hong Kong, Macao, Russia and the China-leaning bloc; nonaligned = the rest. Under this rule **Vietnam
  and Indonesia are coercive** and Mexico, India, Taiwan, Thailand, Malaysia, Singapore and Switzerland are nonaligned.
- **Yards & Fences**: the six choices and the audit's verdict rule (`tri_posture_verdict()`, open 5%, high 15%) unchanged.
  The explainer maps the audit's five verdicts to four: In line -> Aligned; Taxes trusted partners; Fence above
  recommendation; No tariff fence / Partial fence -> Fence below recommendation / missing.

## Chart definitions and sources

| Chart | Measure | Data (CSV) | Built from |
|---|---|---|---|
| 1 Fence rising | Economy-wide TRI by supplier group, 2024 vs Aug 2026, Census duty only; Dec 4 modelled in caption | `01_fence_rising.csv` | `diagnostics/us_trade_restrictiveness/tri_benchmarks.csv` (audit) + line table |
| 2 Tariff map | TRI by technology x stage, all suppliers, Aug 2026 duty + AD/CVD; Deployment = Assembly basket; thin < $0.5bn | `02_tariff_map.csv` | `tri_vs_posture.csv` (audit) |
| 3 Gate | Effective rate (duty/imports, all goods, Jun-Aug 2026) vs UN-voting position; bubble = 2024 two-way trade | `03_who_gets_through_gate.csv`, `03_group_rates.csv` | line table; `geo_alignment_tbl.rds`; Comtrade 2024 |
| 4 Multi-tool | Percentile bands per tool vs whole economy, grid/solar/batteries; dagger = one measure | `04_multi_tool_fence.csv` | `fence_stage_tbl.csv`, `fence_measures.csv` (the workbook's inputs, frozen) |
| 5 Y&F audit | Audit verdict per cell, observed; Dec 4 re-run | `05_yards_fences_audit.csv`, `05b_verdict_summary.csv` | `tri_vs_posture.csv` + Dec 4 layer |
| 6 Dollar-weighted | 2024 imports by verdict, HS6 counted once (equal split across producing-stage cells, Deployment excluded) | `06_*.csv` | line table + verdicts |
| 7 Why allies pay | Trusted-partner TRI on the Dec 4 regime and with s.232, s.301, s.338 or AD/CVD switched off | `07_why_allies_pay.csv` | line table + engine variants |
| 8 Trade response | A: monthly c-Si module imports by origin + US module capacity; B: Li-ion imports by origin, Jan-Aug 2024 vs 2026, + FDI ownership | `08a_*.csv`, `08b_*.csv`, `08_policy_events.csv`, `D_domestic_response.csv` | tracker monthly files; Comtrade monthly; CM-FDI; SEIA/WoodMac |

## The opening section (charts 00a, 00b, 00c)

**Periods.** January-August 2024 vs January-August 2026, the latest comparable months (Census data in the tracker files stop
at August 2026; the Census API needs a key, and UN Comtrade had no September 2026 data on 2026-11-10). QC T2.

**Imports and exports.** Imports: Census imports for consumption by HTS10 line and partner (tracker monthly files: 30 named
partners and the all-country total), summed to HS6. Exports: Census total exports by HS6 as reported to UN Comtrade (monthly,
partner World, fetched in batches of 125 codes). Both use the same 232 HS6 codes (producing stages of the nine technologies in
`energy_hs6_master.csv`). Comtrade's general imports for the same codes run 4-5% above Census consumption imports (QC T1).

**Overlaps.** Technology totals are attributed (a code in several technologies counts in each; exclusive totals in the CSV);
stage totals count each code once, split equally where it sits in different stages, and sum to the unique total (QC T4).
Deployment is never a traded stage (QC T3).

**Sourcing and concentration** (`00b_trade_diversification_metrics.csv`, per technology and technology x stage): import value,
China, coercive-excluding-China, trusted, nonaligned, unassigned (rest of world), North American (Canada + Mexico) and
Southeast Asian (named: Vietnam, Thailand, Malaysia, Indonesia, Singapore) shares, top supplier, top-three share, HHI,
effective number of suppliers and suppliers with at least 1%, each in both periods with changes. Shares sum to 100% (QC T5)
and reconcile to the Census total (T6); coercive change = China change + coercive-excl.-China change (T8). Concentration is
over the 30 named partners (84% of imports), so HHI is a lower bound (T7). Hong Kong and Macao are not separable.

**Chart 00b** plots technologies (and four HS6 product groups) by import growth and change in China's share, with the four
quadrant readings; oil, gas, coal and refined copper cathodes are in the CSV but off the chart. **Chart 00c** shows the key
events of `data/00_policy_timeline.csv` (25 events with sources; 18 checked against Federal Register or HTSUS text, 6 2025 IEEPA
steps from the tracker's registry). Every descriptive chart carries the note that trade changes cannot be attributed to
individual measures from these charts (QC T11).

**Fence change vs diversification** (analysis only): change in all-supplier TRI (2024 rates to August 2026, Census duty) against
change in coercive-supplier share, by producing cell; archetypes at +10 points of TRI and -5 points of share.

## Methods specific to the new analyses

**Chart 6 (dollar weighting).** 91 of the 232 HS6 codes in the nine technologies sit in two to four baskets (static
converters, switchgear and cable sit in grid, EV, solar and wind assembly). The headline basis counts each code's 2024
imports once, split equally across the assessed producing-stage cells it sits in; Deployment cells (which repeat Assembly)
are left out. The CSV also carries the gross cell sum ($1,109bn, overlaps and Deployment included), the sum without
Deployment, and a variant in which Assembly goods take their technology's Deployment verdict. A third bar drops oil, gas and
coal, because crude oil and refined products ($224bn) make up most of the aligned dollars. Dedup total = unique assessed
basket imports, $531bn (QC 5).

**Chart 7 (counterfactuals).** An additive decomposition is not defensible: Section 232 goods are exempt from the Section 301
forced-labour duty and from parts of Section 338, so removing 232 brings other duties in. Each scenario re-runs the tracker's
engine with every action of one authority removed (and, for 232, the 232 carve-out of the stacked actions emptied), takes
the change from 15 August to 4 December under that scenario, adds it to the observed August rate (floored at the lower of
the observed and 2024 rates), then adds AD/CVD. The all-measures scenario equals chart 5's Dec 4 value (QC 7b). The
engine-only version is in the CSV (`engine_only_trusted_tri`); its levels run 1-5 points above observed because the engine
assumes every listed good pays (no USMCA claims, flat auto-parts duty on switchgear and lithium-ion batteries).

**Chart 8A (origins).** The tracker's monthly files name 30 partners; in January-August 2026, 80% of crystalline-silicon module
imports came from elsewhere. The Census remainder is split each month by each untracked country's share of US imports of
HS 854143 in UN Comtrade (the same Census data, all partners, HS6 including thin film). Monthly totals reconcile to Census
exactly (QC 8b). Africa = Ethiopia, Kenya, Nigeria, Djibouti, Ghana, Egypt and others.

**Chart 8B (ownership).** CM-FDI battery manufacturing projects (all statuses) by destination, capital weighted by the
database's attribution weight; Chinese parent = the database's `china_parent` flag. Project source URLs stay in
`cm_fdi_projects_tbl.rds` (left out of `08b_battery_fdi_projects.csv`: some news-URL slugs trip the CI secret scan).

## Visual conventions

NEIS dashboard tokens and Roboto. Blocs: trusted #1889b0, nonaligned #9ca3af, coercive #f58229. Fence height: one sequential
NEIS blue ramp. Verdicts: aligned #0e8a3a, taxes trusted #7b4fbf, above #c98500, below #d03b3b, each with a glyph
(checked with the dataviz palette validator: CVD and normal-vision separation pass; the grey nonaligned colour fails the
chroma floor on purpose and is always direct-labelled). Low/Medium/High on chart 4 are the workbook's percentile bands;
chart 2 bins the TRI at the audit's 5% and 15% lines (and 30%), so the two are not the same scale.

## Quality checks (data/qc_checks.csv)

The 16 audit and package checks and the 14 opening-section checks (T1-T14, incl. the Section 338 HTSUS verification) all pass on 2026-11-10. The first 16 pass as on 2026-11-09: headline TRIs reproduce the audit (2.1->12.3, 13.6->24.8, 15.7->28.4, 6.6->15.6); 46 assessed
cells; verdicts 12/17/16/1 observed and on 4 December; blocs consistent; Deployment not double counted; HS6 overlaps handled;
observed vs statutory dates separated; AD/CVD once; chart 7 reconciles to chart 5; chart 8A reconciles to Census;
traceability to tracker commits. Policy dates were checked against Federal Register text on 2026-11-09 (corrections: IEEPA
collection ended 24 Feb 2026; the forced-labour 301 action covers 60 economies; India/Indonesia/Laos solar orders published
9 Nov 2026 with India's AD deposit 107.17% and Laos 65.36%). Mobile legibility was checked on the rendered PNGs and the
contact sheet's 375 px column.

## Datawrapper versions

`scripts/trade_fence_explainer/04_datawrapper.R` recreates the charts as Datawrapper charts in the NEIS team folder
**Trade fence explainer (Dec 2026)** (folder 460185, team tbtM5ZLH), NEIS theme, from the CSVs in `data/`
(specs in `R/trade_fence_explainer/datawrapper_specs.R`). Chart IDs are kept in `datawrapper_charts.csv`, so a rerun
updates the same charts. They are **unpublished drafts**; set `OPSI_DW_PUBLISH=true` to publish. The token is read from
`DATAWRAPPER_KEY`, or from the .Renviron named by `OPSI_RENVIRON`:

```bash
OPSI_RENVIRON="<path to .Renviron>" Rscript scripts/trade_fence_explainer/04_datawrapper.R
```

Datawrapper equivalents: multi-panel figures are split (chart 00a into four charts, chart 8 into solar imports, US module
capacity and batteries); the dumbbell is a range plot, the counterfactuals a dot plot, the tariff map a heatmap table, and
the policy-intensity and Yards & Fences grids are tables whose cells carry intensity bars (▰) and coloured verdict squares.
The API token cannot export PNGs or read charts back, so the Datawrapper rendering (colours, axis ranges, labels) has not
been checked visually; review each chart in the app before publishing.

| Key | Type | Chart | Title |
|---|---|---|---|
| `00a_trade_by_tech_clean` | d3-bars-grouped | [W6Ayh](https://app.datawrapper.de/chart/W6Ayh/visualize) | America imports far more clean-energy and grid goods than it exports |
| `00a_trade_by_tech_fuels` | d3-bars-grouped | [PgJaO](https://app.datawrapper.de/chart/PgJaO/visualize) | The US is a net exporter of fuels |
| `00a_trade_by_stage_all` | d3-bars-grouped | [ySQn4](https://app.datawrapper.de/chart/ySQn4/visualize) | Across all electro-industrial goods, the US exports raw materials and fuels and imports equipment |
| `00a_trade_by_stage_clean` | d3-bars-grouped | [Rcuaa](https://app.datawrapper.de/chart/Rcuaa/visualize) | 55% of clean-energy and grid imports are assembled goods |
| `00b_trade_rewiring` | d3-scatter-plot | [gEBcH](https://app.datawrapper.de/chart/gEBcH/visualize) | US electro-industrial trade is being rewired, not simply reduced |
| `00c_policy_timeline` | tables | [L688W](https://app.datawrapper.de/chart/L688W/visualize) | The policy environment changed fast between the two trade windows |
| `01_fence_rising` | d3-range-plot | [xNeBV](https://app.datawrapper.de/chart/xNeBV/visualize) | The US raised the fence against China, and sharply raised the floor under its allies |
| `02_tariff_map` | tables | [5A9yN](https://app.datawrapper.de/chart/5A9yN/visualize) | Where has America actually built tariff walls? |
| `04_multi_tool_fence` | tables | [MXUSS](https://app.datawrapper.de/chart/MXUSS/visualize) | America's new trade regime is not really just a tariff regime |
| `05_yards_fences_audit` | tables | [WoMRE](https://app.datawrapper.de/chart/WoMRE/visualize) | Only 12 of 46 electro-industrial tariff fences match the strategic posture Yards & Fences recommends |
| `05b_verdict_summary` | d3-bars | [qCYIS](https://app.datawrapper.de/chart/qCYIS/visualize) | 12 of 46 US electro-industrial tariff fences match the strategy |
| `06_dollar_weighted_misalignment` | d3-bars-stacked | [cNaMP](https://app.datawrapper.de/chart/cNaMP/visualize) | Outside oil, gas and coal, only 3% of the imports behind these fences meet an aligned one |
| `07_why_allies_pay` | d3-dot-plot | [RExwJ](https://app.datawrapper.de/chart/RExwJ/visualize) | Steel, aluminium and copper tariffs explain most of what trusted partners pay for clean-energy equipment |
| `08a_solar_trade_response` | stacked-column-chart | [fwgDj](https://app.datawrapper.de/chart/fwgDj/visualize) | Solar module imports collapsed and moved to new sources |
| `08a_us_module_capacity` | column-chart | [uWBIR](https://app.datawrapper.de/chart/uWBIR/visualize) | US module factories grew, but cell capacity stayed small |
| `08b_battery_trade_response` | d3-bars-grouped | [Cu0G3](https://app.datawrapper.de/chart/Cu0G3/visualize) | Battery imports diversified away from China by origin more than by ownership |
| `03_who_gets_through_gate` | d3-scatter-plot | [jEgPo](https://app.datawrapper.de/chart/jEgPo/visualize) | The US has a China gate. It does not yet have a coherent trusted-partner gate |
| `analysis_fence_change_vs_diversification` | d3-scatter-plot | [RV0gL](https://app.datawrapper.de/chart/RV0gL/visualize) | Analysis: higher fences and lower coercive dependence rarely coincide |
