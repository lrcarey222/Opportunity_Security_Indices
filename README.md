# Opportunity Security Indices (OSI)

This repository holds the **Opportunity Security Indices** pipeline. It produces two country-level pillars, the indices built on them, and a bilateral and coalition layer:

- **Energy Security (ES)**: exposure and resilience across energy supply chains (higher = more secure).
- **Economic Opportunity (EO)**: market opportunity and competitiveness in energy-related value chains.
- **Policy index**: industrial and climate policy support (IEA Policies and Measures + Climate Action Tracker).
- **Strategic Index (SI)**: where a country should build, from EO, its energy-security *need*, policy support, the supply-chain stage and the technology's climate weight.
- **Geopolitical alignment**: bloc membership on the US-China axis of UN General Assembly voting.
- **Allied coalition**: a two-tier coalition (core and partners) derived from alignment, economic-security agreements and major-supplier (or reserve-holder) status, with the EU as one network node.
- **Partnership Strength Index (PSI)**: a partner-level score from bilateral Friendshore, Opportunity and Development components, from the coalition's point of view.
- **Allied network design**: a coalition-wide division of labour per technology and stage (a MILP with a diversification term).

> **Unit of analysis:** **Country × Technology × Supply-chain stage**
> Stages are `Upstream`, `Midstream`, `Downstream`. The 11 headline technologies are Electric Vehicles, Nuclear, Coal, Batteries, Green Hydrogen, Wind, Oil, Solar, Gas, Geothermal and Electric Grid.

The code is the specification. `docs/methodology.md` states the method in one place; this README adds the category-by-category notes, how to run the pipeline, and what changed on this branch ([Changelog](#changelog-fixallied-base-paper-sep-oct-2026)).

---

## Table of contents

- [What this repo produces](#what-this-repo-produces)
- [Data model](#data-model)
- [How the indices are built](#how-the-indices-are-built)
  - [Normalization: `median_scurve`](#normalization-median_scurve)
  - [Overall variables and category scoring](#overall-variables-and-category-scoring)
  - [Pillar aggregation: available case](#pillar-aggregation-available-case)
  - [Missing data: observed-only imputation](#missing-data-observed-only-imputation)
  - [Country harmonisation](#country-harmonisation)
  - [Rankability](#rankability)
  - [Supply-chain coupling (not applied)](#supply-chain-coupling-not-applied)
- [Pillars, categories, and technical appendix](#pillars-categories-and-technical-appendix)
  - [Energy Security (ES)](#energy-security-es)
  - [Economic Opportunity (EO)](#economic-opportunity-eo)
  - [Policy index](#policy-index)
  - [Strategic Index (SI)](#strategic-index-si)
  - [Geopolitical alignment](#geopolitical-alignment)
  - [Allied coalition: two tiers and the EU node](#allied-coalition-two-tiers-and-the-eu-node)
  - [Partnership Strength Index (PSI)](#partnership-strength-index-psi)
  - [Allied network design](#allied-network-design)
- [Repository layout](#repository-layout)
- [Quick start](#quick-start)
- [Interactive explorer](#interactive-explorer)
- [Outputs](#outputs)
- [Comparing index vintages across years](#comparing-index-vintages-across-years)
- [Testing and quality checks](#testing-and-quality-checks)
- [Additional utilities and workflows](#additional-utilities-and-workflows)
- [Raw inputs and refresh](#raw-inputs-and-refresh)
  - [API fetchers](#api-fetchers)
  - [The all-reporter Comtrade dyad pull](#the-all-reporter-comtrade-dyad-pull)
  - [The HS6 crosswalk](#the-hs6-crosswalk)
  - [When ingestion fails](#when-ingestion-fails)
  - [How refresh works](#how-refresh-works)
  - [Key external sources (cited)](#key-external-sources-cited)
- [Known issues and to-dos](#known-issues-and-to-dos)
- [Changelog (fix/allied-base-paper, Sep-Oct 2026)](#changelog-fixallied-base-paper-sep-oct-2026)
- [Citation](#citation)
- [License](#license)

---

## What this repo produces

| Step | Script | Main outputs (`data/processed/`) |
| --- | --- | --- |
| Themes | `scripts/10_build_themes.R` | one tidy table per theme (`*_tbl.rds`), policy components, technology GHG weights |
| Alignment | `scripts/12_build_geopolitical_alignment.R` | `geo_ideal_points_tbl.rds`, `geo_alignment_tbl.rds`, `geo_coalition_tbl.rds` |
| Coalition | `scripts/13_build_coalition.R` | `coalition_tbl.rds`, `eu_stage_weights_tbl.rds`, `major_supplier_shares_tbl.rds` |
| Pillars, Policy, SI | `scripts/20_build_indices.R` | `outputs/index_outputs.rds` (ES, EO, contributions, diagnostics, Policy index, Strategic Index, coalition), `charts/allied_strategic.csv` |
| Partner themes | `scripts/15_build_partner_themes.R` | `partner_{friendshore,opportunity,development}_*_tbl.rds`, `partner_trade_coverage_tbl.rds`, `cm_fdi_projects_tbl.rds`, `partner_cm_fdi_{ties,exposure,coverage}_tbl.rds` |
| PSI | `scripts/25_build_partner_indices.R` | `psi_tbl.rds` (core-coalition view, headline), `psi_full_coalition_tbl.rds`, `psi_world_tbl.rds`, `psi_dyad_tbl.rds` and category contributions |
| Allied network | `scripts/30_build_allied_network_design.R` | `outputs/allied_network_*.csv` |
| Vintages | `scripts/40_build_index_vintages.R` | `vintages/*` (ES and EO at several years) |
| Dashboard: Strategic Insights | `scripts/85_export_dashboard_data.R`, `scripts/86_export_dashboard_fixtures.R` | `dashboard/validation_report.csv`; the app's bundle goes to the repo's `dashboard/public/data/` and the engine's parity fixtures to `dashboard/src/engine/__fixtures__/` |
| Dashboard: Sector Insights | `scripts/87_export_sector_data.R` | `dashboard/sector_validation.csv`; the app's Sector data go to the repo's `dashboard/public/data/sector/` (manifest + 16 OSB chunks, `sector-fdi` among them) |
| Dashboard: Country Insights | `scripts/88_export_country_data.R` | the app's Country data go to the repo's `dashboard/public/data/country/` (manifest, a fact sheet per economy, drivers files; `--render-memos` publishes the verified policy memos of the 31 core members as JSON, PDF, Word and Markdown) |

Every index keeps its decomposition (category and variable contributions reconcile to the index value), and every country-cell carries how much of its weight rests on imputed inputs.

---

## Data model

Theme outputs are stored in a tidy format with common columns:

- `Country` (one canonical name per economy; see [Country harmonisation](#country-harmonisation))
- `tech`
- `supply_chain`
- `sub_sector` (`"All"` unless `include_sub_sector` is on)
- `category`
- `variable`
- `data_type` (`raw` vs `index`; some themes also emit `weight` and `contribution` for decomposition)
- `value`
- `Year`
- `source`
- `explanation`

A schema validator and standardization helpers live under `R/utils/` (`schema.R`, `theme_standardize.R`, `country.R`). The index, coalition, partner and network tables are described in `docs/data_dictionary.md`.

---

## How the indices are built

### Normalization: `median_scurve`

Most raw metrics are converted to a 0–1 index using a median-centered S-curve:

```
idx = r^gamma / (r^gamma + (1 - r)^gamma)
```

where `r` is the percentile rank and `gamma = 0.5` by default. This compresses values near the median while preserving separation at the tails. Every country-level index is ranked **across economies within a technology × stage cell** (never across a country's own cells). A variable that is one value per cell (a world-market HHI, a technology's TRL, world price volatility) shifts the level of a cell but cannot rank countries inside it.

### Overall variables and category scoring

The pipeline is **config-driven**:

- **Overall ... Index** formulas are defined in `config/index_definition.yml` as simple compositions (means) of component index variables. A component that is missing for a country drops out of the mean; it is not counted as 0.
- Each **category** has a **score variable** (e.g. ES Trade uses `Overall Trade Risk Index`, EO Trade uses `Overall Trade Index`) defined in the same file.
- The builders warn when a configured score variable is never produced, or when a theme's own composite stands in for the configured overall, naming the category and the variables its inputs do carry.

### Pillar aggregation: available case

ES and EO are weighted means of their category scores **within each Country × Tech × Stage cell**, over the categories observed in that cell:

```
index = sum_{k observed} w_k c_k / sum_{k observed} w_k
```

A category with no score carries no weight; a cell with no observed category gets `NA`. Weights are in `config/weights.yml`. Category and variable contributions share the same denominator, so both decompositions sum to the index. Builders: `R/indices/build_energy_security_index_v2.R`, `R/indices/build_economic_opportunity_index_v2.R`, shared core `R/indices/index_builder_core.R`.

### Missing data: observed-only imputation

Rules are keyed by theme (or variable/category) in `config/missing_data.yml`:

- `zero`: absence is meaningful (reserves, mineral production and trade, manufacturing market share, investment).
- `global_average`: the mean over the countries observed in the same (tech, stage, category, variable, theme) group.

A rule fills a country **only where the source observes at least one country in the group**; a group nobody is observed in stays `NA` and drops out under the available-case rule. A theme with no rule stays `NA` (e.g. BNEF LCOE, so a country BNEF does not cover is scored on the other cost components). Every imputed value is flagged, and each index cell carries `imputed_weight_share`: the share of its weight resting on imputed components (zero and average fills alike).

### Country harmonisation

Sources spell economies differently ("Viet Nam"/"Vietnam", "Korea"/"South Korea", "Taiwan, Province of China"). `standardize_country_names()` (`R/utils/country.R`) maps every spelling to one canonical name, the WDI name for its ISO3 code (Taiwan is `TWN`), via `countrycode` plus a curated match table. `standardize_theme_types()` drops aggregates and residual rows ("World", "Other Asia Pacific", "USSR", WDI aggregate codes) from every theme and collapses keys that merged spellings now share, warning on genuine conflicts. Before this the index tables carried about 270 country strings for about 198 economies.

### Rankability

Every country-cell the sources reach is scored and keeps its value, but a cell is **rankable** only when

1. the economy is a sovereign state (a Correlates of War state-system code: the 193 UN members plus Kosovo and Taiwan; `is_sovereign_economy()`), and
2. `imputed_weight_share <= tau`, with `tau = 0.5` in `config/rankability.yml`.

Cross-country rankings, top-N lists and the Strategic Index use rankable cells only; the index tables carry `imputed_weight_share`, `rankable` and `rank_exclusion`. `tau = 0.5` is the smallest value on the calibration grid that keeps a majority of sovereign cells in every pillar and stage; it raises EO's within-cell rank correlation across imputation variants from 0.78 to 0.97 (calibration table in `config/rankability.yml`). The PSI applies the same rule to partners ([below](#partnership-strength-index-psi)).

### Supply-chain coupling (not applied)

`R/indices/couple_pillar_scores_by_hhi.R` (shrinking stage scores toward a technology-level chain score) is still in the repository and sourced by `scripts/20`, but no pipeline step calls it: the published pillars are **uncoupled** stage scores.

---

## Pillars, categories, and technical appendix

### Energy Security (ES)

**Interpretation:** ES measures exposure and resilience across upstream resources, midstream manufacturing, and downstream deployment and consumption. Higher = more secure.

**Categories and weights (`config/index_definition.yml`, `config/weights.yml`):** Foreign Dependency 6, Production 6, Consumption 5, Energy Imports 4, Reserves 4, Trade 4, Minerals Trade 4, Energy Access 4, Investment 3, Energy Prices 2.

<details>
<summary><strong>ES — category-by-category construction notes (with cited sources)</strong></summary>

#### 1) Foreign Dependency
**What it captures:** exposure to foreign control of critical upstream inputs (minerals) and midstream manufacturing capacity.

**Overall variable:** `Overall Foreign Dependency Index` = mean of `Mineral Supply`, `Market Share`, `Overall Market Share`, `production`, `sales`, `net_export`, `market_share`, `Overall Production` (those present).

**Data sources:** IEA Critical Minerals Dataset ([IEA-CM]) (mining = Upstream, refining and processing = Midstream; mineral names harmonised across the supply and demand sheets, so rare earths and graphite are no longer dropped; non-suppliers complete with zero supply); IEA Global EV Outlook ([IEA-EV]).

**Clean-tech Midstream manufacturing capacity** (IEA, `iea_cleantech_Midstream.csv`): `Market Share` raw is the entity's share of the IEA world total of 2035 (pipeline) capacity, the source's "Other" row included (since 2026-10-10; the shares used to be taken over the listed entities only, which overstated each of them, e.g. China's 2035 solar share 0.90 against the IEA's 80%). Each technology's shares shrink by one common factor, so the index did not move. The technology read as `Heat.Pumps` (read.csv's name for the header "Heat Pumps - Current") is labelled `Heat Pumps`. Electrolyser rows keep the IEA label `Electrolyzers` in this table, so they do not reach ES Green Hydrogen Midstream (see [Known issues](#known-issues-and-to-dos)).

**EV Midstream** uses the IEA Global EV Outlook extract for five regions that partition the world: China, European Union, North America, Other Asia Pacific and Rest of World. Each region's production, sales, net export ratio (`net_export_ratio` = (production − sales) / sales, positive = net exporter; the extract calls the column `import_share`, and the builders stop if it is not that ratio) and share of world production are copied to the EI economies in it; Rest of World holds every economy outside the other four regions, non-EU Europe included. The index term is `net_export` (higher = less dependent on imported EVs; named `import` until 2026-10-10, though it never measured imports). Each row's `explanation` ends with "IEA region: <region>". Until 2026-10-10 a placeholder in the region join dropped the eight non-EU European EI economies (United Kingdom, Norway, Turkiye, Ukraine, Switzerland, Iceland, Serbia, North Macedonia) from EV Midstream.

**Implementation:** `R/categories/foreign_dependency/foreign_dependency.R`, `critical_minerals_processing.R`, `mineral_supply_security.R`.

---

#### 2) Energy Imports
**What it captures:** reliance on external suppliers for fuels (oil, gas, coal), proxied by the production-consumption balance.

**Overall variable:** `Overall Energy Imports Index` = `Production surplus/deficit`. The index is the mean of the percent-ranked balance (production minus consumption, EJ; raw `Production surplus/deficit`) and the percent-ranked `Self-sufficiency` (production as a percent of consumption: 100 balanced, 0 fully imported; NA where the country consumes none of the fuel). Higher = less import dependent. **Production EI does not report comes from the EIA.** EI lists fewer producers than consumers and counts the rest in its "Other <region>" residuals, so an absent EI production row means "not reported separately" (none, or some inside the residual: New Zealand gas, German oil), not zero. Where EI has no row for the economy and fuel (2025: 43 oil, 37 gas and 48 coal cells of the 79 EI economies), production is the EIA International Energy Statistics value ([EIA-IES]; `data/raw/eia_intl_fossil_production.csv`, `scripts/fetchers/eia_intl.R`) converted to EI's units: oil = crude oil incl. lease condensate + NGPL (thousand b/d; EI's oil production also counts crude, condensate and NGLs and excludes biofuels, CTL/GTL and refinery gain), gas = dry natural gas × 0.036 EJ/bcm (EI's own factor), coal = coal (EIA gross TJ) × 0.95 to net EJ. The year is EI's (oil 2025); gas and coal, which EIA has published only to 2024, take EIA's 2024 value. An EIA value is observed data: the row's `source` adds "EIA International Energy Statistics" and its `explanation` names the year and conversion; an EIA zero is a real zero (2025: Japan oil 7.4 kb/d and gas 0.07 EJ, South Korea 0, Germany oil 32 kb/d, New Zealand gas 0.12 EJ). EI stays the source wherever it reports, a reported zero (2025: Spain coal) included. One EIA value is not used: Estonia coal, because EI counts Estonian oil shale as coal (EI's coal consumption is about 1,200 times EIA's), so EIA's zero coal production is not production of EI's fuel; the check (EIA's consumption of the fuel below 10% of EI's and more than 0.01 EJ short) catches only this cell. That cell and any EIA does not give a value for are NA (`explanation` says why), and the pillar fills the index by the `import_dependence: global_average` rule of `config/missing_data.yml` and flags it imputed. Fossil sums the fuels with a value and is NA only where none has one. Until 2026-10-11 an absent EI row was read as 0 production (0% self-sufficient), then briefly as NA. Economies EI lists with production but no consumption row (Angola, Nigeria, Libya, Guyana and 16 others) take their oil, gas and coal consumption from EIA as well (`eia_fill_consumption = TRUE` in scripts/10 and scripts/40, adopted 2026-10-11), which puts them on the theme's roster (36 more rankable ES Upstream cells). **Production of an earlier year is set against consumption of that year.** Where production is an EIA value of an earlier year than consumption (2025: the gas and coal production above, EIA 2024, against EI 2025 consumption), self-sufficiency is production over consumption of the production year (EI 2024), and the balance applies it to the consumption the cell uses: balance = consumption × (self-sufficiency − 1), i.e. production enters as EIA's 2024 production × EI's 2024 → 2025 consumption change (the 2024 self-sufficiency carried to 2025). Zero production is unaffected. The `explanation` gives both years and values. 2025: 32 cells (22 gas, 10 coal); until 2026-10-11 the two years were divided as they were, so Slovenia coal read 121% self-sufficient and a net exporter (EI's 2025 coal consumption fell 39%; paired: 73.5%), New Zealand gas 122% (paired: 109.5%). The reverse case is not paired: the producers EI lists without consumption have EI's observed 2025 production over EIA's 2024 consumption (taken as 2025's; the `explanation` names the EIA year), because their production moves far more than their consumption (2024 → 2025: Yemen oil −57%, South Sudan oil +53%, Bolivia gas −15%). `Self-sufficiency` replaced `Import share` on 2026-10-10: that variable was self-sufficiency minus 100, so it was positive for exporters despite its name, and its rounding noise split economies that produce none of a fuel instead of tying them (2025 oil: Spain ranked below 38 non-producers, Israel, the Netherlands, Poland and Singapore above them). **Sources:** Energy Institute Statistical Review ([EI-SR]); EIA International Energy Statistics ([EIA-IES]) where EI does not report production. **Implementation:** `R/categories/energy_imports/import_dependence.R`, `R/utils/eia_intl.R`.

---

#### 3) Reserves
**What it captures:** domestic reserves (fossil fuels, minerals, uranium) and demand-weighted "technology reserves" for minerals, plus renewable resource potential.

**Overall variable:** `Overall Reserves Index` = mean of the fuel and mineral reserve indices (`Oil/Gas/Coal Reserves`, `Cobalt/Lithium/Graphite/Rare Earths/Copper/Manganese/Nickel/Zinc/PGMs Reserves`), the technology reserve roll-ups (`Solar/Wind/Electric Vehicles/Batteries/Electric Grid/Green Hydrogen/Heat Pumps/Hydroelectric Power/Geothermal Reserves`), `Nuclear Reserves`, and the Solar PV, Wind and Geothermal potential indices.

**Sources by resource (user decision 2026-10-09):** oil, gas and coal: EI proved reserves at end-2020 (the last year EI published them); cobalt, lithium, natural graphite, rare earths (REO equivalent), copper, manganese, nickel, zinc and PGMs (combined): USGS *Mineral Commodity Summaries 2026* reserves at end-2025 ([USGS-MCS]; the data release's commodity table, `data/raw/MCS2026_World_Data.csv`, with the report's ver. 1.3 correction of Brazil's rare earths to 11 Mt applied); uranium: the Red Book. EI's mineral sheets are copies of older USGS figures that fold smaller holders into Rest of World. USGS value codes are read explicitly: '—' is an observed zero (index 0); 'NA', 'W' (withheld) and 's' (small; the graphite chapter footnotes these holders as included in the world total) are missing; an unknown code stops the build. 'Other countries' and 'World total' are never holders.

**Uranium (Nuclear Upstream):** `Nuclear Reserves` is identified recoverable uranium resources (RAR + inferred) at **< USD 260/kgU**, the category the OECD-NEA/IAEA *Uranium 2026* "Red Book" headlines (Table 1.2a, as of 1 January 2025), ranked like the other reserve tables. Data are transcribed with source, table and access date in `data/reference/uranium_resources.csv` (`R/categories/reserves/uranium.R`).

**Missing data:** `zero`: the EI, USGS and Red Book tables list the material holders, so a country absent from them has none recorded (EI's "Rest of World" and USGS's "Other countries" can hide a sizeable holder; see [Known issues](#known-issues-and-to-dos)). A holder USGS lists without a figure (NA, W, s) is missing in its mineral variable (scored 0, counted as imputed) and, like an unlisted mineral, counts 0 in the technology roll-ups.

**Sources:** EI Statistical Review ([EI-SR]); USGS Mineral Commodity Summaries ([USGS-MCS]); IEA Critical Minerals Dataset ([IEA-CM]) demand shares; OECD-NEA/IAEA Red Book ([REDBOOK]); Global Solar Atlas ([GSA]); Global Wind Atlas ([GWA]).

**Implementation:** `R/categories/reserves/reserves.R`, `R/categories/reserves/usgs_mcs.R`, `R/categories/reserves/uranium.R`, `R/categories/energy_access/{solar_pv,wind,geothermal}_potential.R`.

---

#### 4) Trade (risk)
**What it captures:** exposure through traded energy-technology products in the technology × stage basket.

**Overall variable:** `Overall Trade Risk Index` = mean of
- `HHI`: S(−HHI) of the world export market for the basket (one value per cell, ranked across the cells of the headline technologies);
- `market_share`: the country's share of world basket exports (its basket exports over the world's; non-exporters score 0);
- `deficit_gdp`: S(−(imports − exports) / GDP), so a smaller deficit or a surplus scores as more secure.

The ES and EO trade themes come from one builder but keep different components (`TRADE_THEME_VARIABLES`): the ES table carries the exposure terms, EO the capability terms. Market share and the deficit come from the Harvard Atlas of Economic Complexity ([AEC], HS92, reconciled, ~235 economies, newest year on disk) with WDI GDP ([WDI]); the HHI from UN Comtrade reporters' 2024 basket exports (`comtrade_energy_trade.csv`, [COMTRADE]); rankings are across the WDI economies within the cell. Since 2026-10-09 the HHI S-curve ranks the basket cells of the 11 headline technologies only (18 since the Upstream baskets of 2026-10-11; `config/coalition.yml` `headline_techs`, passed in by `scripts/10`), and the cells of an unscored technology - Semiconductors Upstream and Midstream - are placed against them (`median_scurve_ref()`), so an unscored basket cannot move a scored cell (it had moved Batteries Midstream from 0.290 to 0.217). **Re-export hubs** (`config/index_definition.yml` `trade: re_export_hubs`, Hong Kong) are left out of every Comtrade measure - exports, imports, RCA, export size and the export HHI - and so of the world aggregates of the RCA and of the HHI's shares, because their Comtrade exports are mostly goods made elsewhere that the producers' exports already count; they keep their Atlas market share, feasibility and deficit/GDP. The bilateral dyad matrix of the partner themes and the network is not filtered.

**Implementation:** `R/categories/trade/trade_core.R`, `trade_concentration.R`.

---

#### 5) Minerals Trade
**What it captures:** a country's position in the world trade of the critical minerals a technology uses.

**Overall variable:** `Overall Minerals Trade Index` = mean of
- `critmin_market_share` = sum over minerals of w_mt · S_m(X_im / Σ_j X_jm) (S-curve within mineral over exporters; non-exporters 0), and
- `critmin_hhi` = sum over minerals of w_mt · S(−HHI_m) (a world-market term, the same for every country within a technology),

with w_mt the IEA base-year demand shares renormalised over the minerals with trade data.

**Mirror exports:** an economy that does not report exports to Comtrade gets its partners' import declarations, moved from CIF to FOB with an HS-chapter CIF/FOB ratio estimated from importers that file both values. Economies that declared their exports after the pulls were staged ("late reporters": DR Congo, Jordan, Oman, ...) count as reporters with their own declarations. Without the mirror, DR Congo, Russia, Guinea, Gabon, New Caledonia, Mongolia, Lao PDR and Viet Nam had no exports and every world total and HHI was computed without them. Partner-level imports come from `scripts/fetchers/comtrade_mirror_critmin.R` (keyless Comtrade preview API, `data/raw/critmin_mirror_import.csv`).

**Sources:** UN Comtrade ([COMTRADE]) critical-minerals import, export and total-export pulls (the newest vintage with at least 50 reporters) and the mirror pull; IEA Critical Minerals Dataset ([IEA-CM]).

**Implementation:** `R/categories/minerals_trade/critical_minerals_trade.R`.

---

#### 6) Production
**What it captures:** depth and momentum of domestic production (fossil fuels, generation, minerals, uranium).

**Overall variable:** `Overall Production Index` = mean of `size`, `growth_abs`, `Overall Production`, the mineral production indices (`Cobalt ... PGMs Production`), the technology roll-ups and the fuel production indices, including `Nuclear Production`: Red Book Table 1.18 uranium mine production in the latest year (`data/reference/uranium_production.csv`, 2019–2024), completed with observed zeros over the EI and WDI economies (the table is exhaustive). ES Production has no Oil, Gas or Coal cells: scripts/20 passes `production_depth_momentum` (the EI fuel and generation cells) to EO only, so the EIA fill of fuel production (EO "2) Production") reaches ES through Energy Imports alone.

**Sources:** EI Statistical Review ([EI-SR]); IEA Critical Minerals Dataset ([IEA-CM]); Red Book ([REDBOOK]).

**Implementation:** `R/categories/production/production_depth_momentum.R`, `critical_minerals_production.R`, `R/categories/reserves/uranium.R`.

---

#### 7) Energy Access
**What it captures:** downstream enabling conditions via per-capita energy consumption.

**Overall variable:** `Overall Energy Access Index` = mean of `Energy consumption per capita` and `Energy consumption per capita growth`. **Source:** EI Statistical Review ([EI-SR]). **Implementation:** `R/categories/energy_access/energy_access_consumption.R`.

---

#### 8) Consumption
**What it captures:** installed base and growth (per-capita installed capacity and projected growth).

**Overall variable:** `Overall Consumption Index` = mean of `Energy consumption per capita`, `Energy consumption per capita growth`, `installed_cap_index`, `elec_growth_index`. BNEF onshore and offshore wind are recoded to Wind, and capacity growth is ranked within technology. **Source:** BloombergNEF New Energy Outlook ([BNEF-NEO]). **Implementation:** `R/categories/consumption/energy_consumption.R`.

---

#### 9) Energy Prices
**What it captures:** input price volatility (annualised volatility of monthly log returns of IMF PCPS commodity series), oriented so lower volatility scores higher.

**Overall variable:** `Overall Energy Prices Index` = `Overall Price Volatility Index`. It is a **world-level series**, identical for every country within a technology: it shifts levels across technologies but cannot rank countries. (The category used to be configured on a variable nothing produced, `price_volatility`, and was silently unscored.)

**PCPS coverage:** fossil fuels, uranium and the transition minerals in the snapshot (lithium, nickel, cobalt, copper, vanadium, iron ore, diammonium phosphate). Graphite and soda ash are not covered.

**Units.** The raw `latest_price` is in the commodity's own unit, stated in the row's `explanation` and held in `energy_prices_pcps_units()` (`R/categories/energy_prices/energy_prices.R`), which takes the units from the IMF PCPS series metadata (SDMX codelists), not from the IMF label text: the label is wrong for cobalt (it says per pound; the series is USD per metric tonne) and propane (it says US cents; the series is USD per gallon), and gives no unit for lithium, vanadium, chromium and rare earths (USD per metric tonne each). Oil is USD per barrel, gas and LNG USD per MMBtu, uranium USD per pound (U3O8), coal and the metals USD per metric tonne. `yoy_price_change_pct` is in percent. A series without a verified unit (urea, the optional Energy Transition Metal index) reads "unit to confirm". Units do not enter the index, which is built from returns.

**Source:** IMF Primary Commodity Prices ([IMF-COMM]). **Implementation:** `R/categories/energy_prices/energy_prices.R`.

---

#### 10) Investment
**What it captures:** clean-energy investment flows and capacity pipeline.

**Overall variable:** `Overall Investment Index` = mean of `Annual Investment Index`, `Investment Momentum Index`, `Operating Capacity Index`, `Pipeline Capacity Index`. Critical-minerals investment is split between Batteries and EV Upstream by IEA demand shares (it used to be copied in full to both); cells with no tracked investment are not emitted as zeros. Missing data rule: `zero`.

**Source:** Clean Investment Monitor investment and capacity extract (`GCIM_Investment_Capacity_aggregated.xlsx`). **Implementation:** `R/categories/investment/investment_momentum.R`.

</details>

---

### Economic Opportunity (EO)

**Interpretation:** EO measures market opportunity and competitiveness: demand, export capability, cost position, industrial capability and technology maturity.

**Categories and weights:** Technology Demand 4; Trade, Production, Energy Prices, Investment, Energy Access, Foreign Dependency, Cost Competitiveness, Consumption 3 each; Technological Readiness 2. (Reserves is not an EO category.)

<details>
<summary><strong>EO — category-by-category construction notes (with cited sources)</strong></summary>

#### 1) Trade (opportunity)
**What it captures:** revealed and latent export capability.

**Overall variable:** `Overall Trade Index` = mean of `market_share` (share of world basket exports), `rca` (Balassa RCA, world aggregates over the reporters present in both extracts), `export_size`, `feas` (Atlas feasibility from product-space distance, one row per HS4 heading) and `HHI`. RCA, export size and HHI come from UN Comtrade reporters' 2024 basket exports (`comtrade_energy_trade.csv`; RCA denominator `comtrade_total_export.csv`; Hong Kong, a re-export hub, left out of both, see ES Trade), market share and feasibility from the Atlas. Removing a hub rescales every RCA in a cell by the same factor (world aggregates), so it changes no other economy's order. The net-import term is ES-only. Export size and feasibility are ranked across economies within the cell (they used to be ranked within each country across its own cells, which put Moldova and Samoa next to China in Solar Midstream).

**Sources:** Atlas of Economic Complexity ([AEC]); UN Comtrade ([COMTRADE]); WDI ([WDI]). **Implementation:** `R/categories/trade/trade_core.R`, `export_feasibility.R`.

---

#### 2) Production
**What it captures:** productive capability and momentum.

**Overall variable:** `Overall Production Index` (as in ES; EO reads `size`, `growth_abs` and `Overall Production` from `production_depth_momentum`). Nuclear Upstream runs Red Book uranium mine production through the same depth-and-momentum indices as oil, gas and coal, over the EI and WDI economies (non-producers are observed zeros).

**Mineral Upstream cells.** For the six mineral-intensive technologies (Batteries, Electric Grid, Electric Vehicles, Green Hydrogen, Solar, Wind) the Upstream level `production_latest` is the technology's demand-weighted share of world mined supply (0–1): each mineral is the country's share of world supply in the IEA Critical Minerals Dataset base year (the named producers plus "Rest of world"), weighted by the technology's share of that mineral's demand, renormalised over the minerals with supply data (the weights of the reserves and critical-minerals production roll-ups):

```
P_it(y) = sum_m w_mt * Q_im(y) / Q_m(y0),   y0 = base year (2025), y1 = 2035
```

`size` = S[P(y0)]; `growth_abs` = S[P(y1) − P(y0)], an absolute change in quantity (the 2035 level is over base-year world supply; the change is 0 for a listed country that mines none of the minerals); `Overall Production` = S[2 size + growth_abs + h], where the demand-weighted world HHI term h is the same for every country and drops out of the ranking. The rows are sourced "IEA Critical Minerals Database". Until 2026-10-10 the level was the unweighted mean of a country's mined kilotonnes over the technology's minerals (copper dominated it), size ranked all country-mineral rows together (so a country scored on how many minerals it mines), and growth was relative and so undefined for the 92 listed non-producer cells, which the global-average rule then placed above small producers. Solar Upstream is in effect copper mining (silicon and silver have no IEA supply rows; copper carries 0.994 of the renormalised weight), so it is close to Electric Grid Upstream (copper only). A country the IEA names for some mineral is counted as 0 for the minerals it is not named for, although it may mine them inside "Rest of world"; see [Known issues](#known-issues-and-to-dos).

**Fuel cells (Oil, Gas, Coal Upstream) EI does not report.** The theme scores every economy EI lists for any of its variables (99 in 2025) on every cell. Where EI has no production row for one of them in a year (2025: 49 oil, 50 gas and 65 coal cells; EI folds them into "Other <region>" residuals), the value is the EIA International Energy Statistics value of that year, converted as for Energy Imports (ES "2) Energy Imports"); 2025 gas and coal use EIA's 2024 value, its latest. 2025: 31 oil, 29 gas and 12 coal cells get a positive value (Bahrain and Turkiye oil, Angola and Equatorial Guinea gas, Philippines and Malaysia coal, ...), the rest a real EIA zero, and Estonia coal is NA in every year (EI counts oil shale as coal, which EIA's coal series exclude), so the pillar's `global_average` rule fills it. The rows' `source` adds "EIA International Energy Statistics" and `explanation` names the years and the conversion. Until 2026-10-11 these cells were 0. **Generation (Downstream) EI does not report is still 0**: EI's electricity-by-fuel table lists 31 of the 99 economies, and EIA has positive 2024 net generation for most of the other 68 (coal 35, gas 60, oil 63; wind 8 and solar 19 of 20), so a 0 there is a known undercount, not an observation; filling it from EIA (series `INTL.<product>-12-<iso3>-BKWH.A`) is not done yet.

**Sources:** EI Statistical Review ([EI-SR]); EIA International Energy Statistics ([EIA-IES]) for fuel production EI does not report; IEA Critical Minerals Dataset ([IEA-CM]); Red Book ([REDBOOK]). **Implementation:** `R/categories/production/production_depth_momentum.R`, `R/utils/eia_intl.R`.

---

#### 3) Technology Demand
**What it captures:** forward demand, with an overcapacity penalty as a margin-risk proxy.

**Overall variable:** `Overall Technology Demand Index` = mean of one composite per source: `Overall Global Demand` (IEA WEO), `Overall Demand` (BNEF NEO regional demand, or the IEA EV Outlook index for EV Downstream), `Overall Addressable Market` (BCG) and `Overall Overcapacity` (BNEF). (The size and growth terms used to be listed beside the composites built from them, which gave WEO half the category.)

**Variables by leg.** WEO (global, copied to every country): `2035` (EJ), `global_growth` (change 2024–2035, EJ), `global_demand_size`, `global_demand_growth` (indices ranked across fuels) and `Overall Global Demand`. BNEF NEO (regional): `X2024`, `X2035`, `growth` (relative), `demand_size`, `demand_growth` and `Overall Demand`. The WEO terms carry the `global_` prefix since 2026-10-10: with shared names the twelve NEO countries had two rows per key (756 in all), and `growth` meant an EJ change in one and a ratio in the other. `future_demand()` now stops on a duplicate (Country, tech, supply_chain, variable) key. The category reads the composites only, so the index did not move.

**Sources:** BloombergNEF ([BNEF-NEO]); IEA Global EV Outlook ([IEA-EV]); IEA WEO annex and BCG market sizing extracts (see `docs/sources.md`). **Implementation:** `R/categories/technology_demand/future_demand.R`, `overcapacity_premium.R`.

---

#### 4) Technological Readiness
**What it captures:** technology maturity (TRL) by technology **and stage**, one value per cell, identical across countries.

**Overall variable:** `Overall Technological Readiness Index` = `TRL Index`.

**Method:**
- **Stage mapping.** Every item of the IEA ETP Clean Energy Technology Guide ([IEA-CTG], 2026 public dataset, TRL.2020–TRL.2025) is mapped to a technology and a stage (Upstream = extraction, primary inputs and recycling to raw materials; Midstream = processing and manufacturing, including hydrogen production; Downstream = deployment, generation, distribution and use) or classified out of scope with a reason, by ordered rules in `config/iea_clean_tech_guide_tech_map.yml` keyed on the Guide's taxonomy path, cross-cutting tags and names. Unclassified items raise a warning. A cell averages its items; a stage no item maps to takes the technology-wide value (recorded in `basis`).
- **Level:** a Goldilocks bell on the end-year TRL on the 1–11 scale (peak 6, zero at 2 and 11). The 2026 Guide stops at TRL 9 and grades TRL 9 items by a post-TRL-9 adoption stage (`Post.index.2025`), read back as Awaiting adoption = 9, Building momentum = 10, Commercial in many markets = 11.
- **Momentum:** the capped, positive TRL change over the release's window, on the scale the release reports every year.
- **TRL Index** = 0.7 × cell Level + 0.3 × cell Momentum (blend of cell means).
- TRL is broadcast to every country as an observed input (it used to be a "Global" pseudo-country, 99.5% imputed).

**Implementation:** `R/categories/technological_readiness/technological_readiness.R`.

---

#### 5) Cost Competitiveness
**What it captures:** deployment and manufacturing cost position.

**Overall variable:** `Overall Input Cost Index` = mean of `LCOE Index` (the mean of the BNEF current-year and 2050 LCOE indices, Downstream, ~20–30 markets per technology), `Input Cost Index` (ILO labour and IMF capital costs; Green Hydrogen has its own) and `IEA Cost index` (relative equipment cost). LCOE used to carry three of four votes. LCOE has no missing-data rule on purpose.

**Capital leg (IMF MFS_IR).** Each country takes the newest reading of its highest-priority rate series (lending rate; euro-area loans to non-financial corporations, new business, then outstanding; money-market; policy; deposit); a reading older than the 10-year recency floor is held out and filled like a missing one. A zero or negative reading in a lending-type series is a placeholder, not an observation: since 2026-10-10 it is dropped, with the IMF quarterly and annual averages that include it, and the series' last real reading is used (`cost_competitiveness_drop_rate_stubs()`). This affects New Zealand, whose lending series ends in January 2017 (4.76%) and is followed only by zero entries (it read 0.0, the panel's lowest), and Solomon Islands, whose 2021 annual average (5.18%) folded in zero entries against 10.3–10.4% in every reported month (now 10.35%). Zero and negative policy, money-market and deposit rates are kept: they are real.

*BNEF ships the LCOE Data Viewer as a macro workbook (`.xlsb`) from the 2026 release onward; `scripts/utils/bnef_lcoe.R` flattens the "Raw LCOE data" sheet once through `scripts/utils/convert_xlsb_sheet.ps1` (Excel automation, Windows only) and caches the CSV under `data/raw_cache/bnef_lcoe/`. A `.csv` export of the same sheet is still read directly. The current-year variable is named after the release's reference year (`lcoe_24`, `lcoe_25`).*

**Sources:** IMF Data Explorer ([IMF-DEX]); ILOSTAT ([ILO]); BNEF LCOE; IEA relative cost exports. **Implementation:** `R/categories/energy_prices/lcoe_competitiveness.R`, `R/categories/economic opportunity/cost_competitiveness.R`.

---

#### 6) Energy Prices
As in ES: `Overall Price Volatility Index`, a world-level series per technology (IMF PCPS, [IMF-COMM]).

---

#### 7) Energy Access
As in ES: `Energy consumption per capita` and its growth (EI, [EI-SR]).

---

#### 8) Foreign Dependency
**What it captures:** midstream manufacturing market share, read as positioning (`market_share_manufacturing`: IEA clean-tech manufacturing capacity, with electrolysers mapped to Green Hydrogen Midstream; missing data rule `zero`). The raw `Overall Market Share` is the 2035 share of the IEA world total, "Other" included, as in ES. EV Midstream uses the same five IEA EV regions and `net_export` term as ES Foreign Dependency (one builder each, kept identical by a parity test). **Implementation:** `R/categories/foreign_dependency/market_share_manufacturing.R`.

---

#### 9) Consumption
As in ES: installed capacity per capita and projected growth (BNEF, [BNEF-NEO]).

---

#### 10) Investment
As in ES: Clean Investment Monitor flows, momentum, operating and pipeline capacity (`zero` rule).

</details>

---

### Policy index

**What it captures:** policy support for a technology × stage in a country.

```
P_its = sum_v w_v x^v_its / sum_v w_v   over the components observed in the cell
w: IEA PAMS 3, CAT 2
```

- **IEA Policies and Measures (PAMS)** (`R/categories/policy/iea_policy_index.R`): policies matched to technology and stage by whole-word rules (substring rules had tagged "more" and "Korea" as ore, "determine" as mine), weighted by jurisdiction.
- **Climate Action Tracker (CAT)** ratings (`R/categories/policy/cat_policy_index.R`): EU members without a rating of their own take the EU bloc rating.
- **Eligibility:** IEA PAMS is required; CAT is added where it rates the country (39 countries and the EU). A country is scored if it has IEA PAMS for at least one of its headline cells. One without a CAT rating is scored from IEA PAMS alone and flagged `policy_basis = "IEA PAMS only"` (150 of 215 in the October 2026 build, Israel, Ukraine and nine coalition partners among them); one with both is `"IEA PAMS + CAT"` (65). A country with a CAT rating but no IEA PAMS is not scored (none in the build): CAT varies across a country's cells only through the technology GHG index, so on its own it would rank sectors by emissions, which the SI's climate term already carries. Until 9 October 2026 both components were required (65 countries); `OPSI_POLICY_REQUIRE_ALL=1` restores that rule for robustness runs. The two bases are different readings: for the 65 countries with both, ranking their cells on IEA PAMS alone agrees with the composite at a median within-country Spearman of 0.48, so tables and charts show the basis.
- **Excluded for now (October 2026):** GTA **NIPO** (weight 4 until October 2026). `scripts/10` still builds it and its tables stay on disk (`OPSI_SKIP_NIPO=true` reuses the stored NIPO rows instead of rescoring them), so it can return by adding it to the weights. The **dual-use** score is not a component: it is an LLM-elicited judgement per technology × stage, identical for every country, so it cannot rank countries.
- **NIPO tables on disk: the October 2026 refresh.** `data/processed/nipo_*` come from the October 2026 extract (`GTA NIPO - October 2026.xlsx`, as of 5 October 2026; `nipo_refresh_manifest.txt`), built by the NIPO pipeline of PR #333 on `main` (debiased strength), which this branch has not merged yet. They are read only as context - the strongest active measures per sector in Country Insights and the paper's "NIPO added back" robustness variant (77 jurisdictions; the stored February 2026 component in `policy_component_tbl.rds` covers 23) - and do not enter the Policy index.

`scripts/20` passes the weights and the required components explicitly and records them, the exclusions, the eligibility rule and the scored countries by basis in `index_outputs.rds$policy_index_provenance`. **Implementation:** `R/indices/build_policy_index.R` (`build_composite_policy_index()`).

---

### Strategic Index (SI)

```
SI_its = 0.2 pol_its + 0.3 EO_its + 0.3 (1 - ES_its) + 0.1 lambda_s + 0.1 g_t
```

- `pol`: within-country median S-curve of the Policy index over the country's in-sample sectors.
- `EO`: the Economic Opportunity pillar, as built.
- `1 - ES`: energy-security **need** (column `energy_security_need`). ES is oriented higher = more secure, so the SI favours sectors where the country is least secure. `Energy_Security_Index` keeps its security meaning in the output.
- `lambda_s`: stage weight, Midstream 0.75, Upstream 0.50, Downstream 0.25.
- `g_t`: technology climate weight (`ghg_index`, 1 = lowest lifecycle GHG intensity; IPCC AR5 Annex III medians, oil from SRREN Annex II at 840 gCO2eq/kWh).

**Sample (nothing is imputed):** a cell enters only if EO, ES and the Policy index are all observed and both pillar cells are rankable; others stay in the table with `strategic_index = NA` and the reason in `si_exclusion`. Geothermal Midstream is excluded, and any sector where a pillar is constant across countries is excluded and the index rebuilt (`strategic_index_degenerate_sectors()`). `scripts/20` also reports sectors where one ES value is shared by at least a quarter of the scored countries (`strategic_index_es_floor_sectors()`; see [Known issues](#known-issues-and-to-dos) on Nuclear Upstream).

**Allied selection:** each coalition member (EU members individually) gets its top Upstream/Midstream sector, excluding Geothermal and Green Hydrogen, written to `charts/allied_strategic.csv` with its tier, network node and `policy_basis`. An Upstream extraction stage can be the pick only where the country has recorded reserves or production of its resources (the endowment screen: `resource_base`, `pick_exclusion = "no resource base"`; the index is unchanged; `OPSI_SI_RESOURCE_SCREEN=0` turns it off). Every member has a Policy index, 11 on IEA PAMS alone (Israel, Ukraine, DR Congo, Dominican Republic, Gabon, Ghana, Guatemala, Malaysia, Namibia, Oman, Qatar); `scripts/20` names them and any member without a pick, with the reason.

**Implementation:** `R/indices/build_indices.R` (`build_strategic_index()`, `select_allied_strategic_sectors()`).

---

### Geopolitical alignment

`scripts/12_build_geopolitical_alignment.R`, `R/geopolitical_distance/alignment.R`, settings in `config/geopolitical_distance.yml`.

- **Input:** Bailey–Strezhnev–Voeten UN General Assembly ideal points, **all-votes** series (Dataverse V39.0, `scripts/fetchers/un_ideal_points.R` → `data/raw/un_ga_ideal_points_all.csv`), mean over the latest **5 sessions** (2019–2023; the all-votes series ends in 2023). The final-passage series (to 2025) is kept as a sensitivity.
- **Axis:** s_i = (θ_i − θ_CHN) / (θ_USA − θ_CHN), clamped to [0, 1], so a country voting further from the US than China does sits with China.
- **Blocs** (after Gopinath, Gourinchas, Presbitero and Topalova, IMF WP/24/76): U.S.-leaning = top quartile of s, China-leaning = bottom quartile, nonaligned otherwise. **Hard blocs** (`tau = 0`): p_i ∈ {1, 0.5, 0}.
- **Dyadic alignment:** A_ij = p_i p_j + (1 − p_i)(1 − p_j): 1 within a bloc, 0 between the two blocs, 0.5 when either is nonaligned.
- **Economies without a UN vote** (`data/reference/territory_sovereign_map.csv`): territories and SARs **inherit** their sovereign's position (Hong Kong and Macao: China; Puerto Rico, Guam: US; Greenland: Denmark; ...); Taiwan, Palestine and Kosovo are **neutral** (p = 0.5). `alignment_basis` records `own_votes`, `inherited`, `neutral` or `none`.
- **Alignment coalition:** the U.S.-leaning bloc with own votes and latest WDI GDP ≥ USD 100bn (`geo_coalition_tbl.rds`, 31 members). It is the core tier of the coalition below.

Limitations are documented in the `alignment.R` header: the first UNGA dimension is driven by recurring resolutions, so Gulf partners are China-leaning or nonaligned and Mexico, India, Brazil, Turkey and Switzerland are nonaligned; Japan, Ireland and New Zealand sit just above the U.S.-leaning cut.

---

### Allied coalition: two tiers and the EU node

`scripts/13_build_coalition.R`, `R/indices/coalition.R`, settings in `config/coalition.yml`. A country's tier is the highest route it satisfies; all routes are recorded in `coalition_tbl$routes`.

- **core:** the alignment coalition (U.S.-leaning bloc, own UN votes, GDP ≥ USD 100bn).
- **partner** (not core), by either route:
  - **agreement:** a member of a U.S. FTA, IPEF, the Minerals Security Partnership, the G7 or NATO (`data/reference/economic_security_agreements.csv`, each row with source and access date) with GDP ≥ USD 100bn, whatever its bloc (the MSP Forum is a sensitivity only);
  - **supplier:** not China-leaning, no GDP floor, with either a world share ≥ 5% of production, refining, manufacturing capacity or exports in at least one measure of an Upstream or Midstream stage of a headline technology (route `supplier`), or ≥ 10% of world reserves of a headline mineral, fuel or uranium (route `supplier: reserves`; user decision 2026-10-09: a stock is not yet supply, hence the higher bar). Flow measures (`major_supplier_shares_tbl.rds`): IEA mining and refining shares for copper, cobalt, lithium, nickel, graphite and magnet rare earths (linked to a technology that takes ≥ 1% of the mineral's demand); EI mine production for manganese, zinc and PGMs; EI oil, gas and coal production; Red Book uranium production; IEA clean-tech manufacturing capacity; Atlas basket export shares. Reserves measures, Upstream, from the Reserves theme's sources: USGS *Mineral Commodity Summaries 2026* reserves of cobalt, lithium, graphite, rare earths, copper, manganese, nickel, zinc and PGMs at end-2025 (`usgs_reserves`, linked to technologies as for mining; shares of the larger of USGS's rounded world total and the sum of the listed rows); EI proved reserves of oil, gas and coal at end-2020 (`ei_reserves`, each to its own technology; shares of each sheet's "Total World" row); Red Book identified uranium resources < USD 260/kgU (Table 1.2a, 1 January 2025; `uranium_resources`), as shares of the listed countries' total. Each row is tested at its own bar (`threshold`, `coalition_supplier_threshold()`).
- **none:** no route.

**EU node.** The 27 EU member states share one network node, `EUU`, with tier core, whatever their own tier (six members, CYP EST LTU LVA MLT SVN, are tier none on their own). `eu_stage_weights_tbl.rds` gives each member's weight within the node per (tech, stage): EI production or Red Book uranium production, else Atlas export shares, else GDP shares (a measure qualifies only when the members cover ≥ 50% of the EU total and the EU holds ≥ 0.1% of the world). Only the network layer is bundled: national indices, the Strategic Index selection and the PSI stay national.

On the inputs of 2026-10-09 the coalition has **31 core members, 26 partners and 37 network nodes** (57 members; 63 economies with the six node-only EU members). Brazil is a partner by the reserves leg alone (graphite 23.9%, manganese 16.7%, rare earths 14.6%, nickel 11.4% of world reserves, USGS MCS 2026; its largest flow share is 3.9%, of oil production). The next non-member holders that are not China-leaning stay below 10%: Madagascar (graphite 8.7%), Mozambique (8.1%), Kuwait (oil 5.9%), Tanzania (graphite 5.8%), Niger (uranium 5.7%), New Caledonia (nickel 5.1%). Switzerland left the partner tier with the basket review: its only route was its share of electricity exports (HS 2716.00), which is no longer in the grid basket.

**Taiwan is not in the coalition** (disclosed, user decision 2026-10-09). It has no UN vote, so it is neutral (p = 0.5) and cannot be core; it is in none of the agreement lists, and WDI has no GDP for it; its largest supplier share is 1.5% (Solar Midstream exports), below 5%, and it has no reserves in the EI, USGS or Red Book tables. Its semiconductor strength lies outside the baskets of the 11 headline technologies.

Every consumer reads the coalition through `coalition_members()` (`R/indices/build_indices.R`), which falls back to `geo_coalition_tbl.rds` (all core, each its own node) with a warning while `coalition_tbl.rds` does not exist. The hand-picked 23-member list survives only as the legacy/test default `allied_network_default_iso3c()`.

---

### Partnership Strength Index (PSI)

**Interpretation:** how valuable partner j is to the allied coalition across energy technologies, from three components rated from the reporter's side.

**Components (`config/weights.yml`):**

| Component | Level | Built from (weights) |
| --- | --- | --- |
| **Friendshore** FS(i, j) | dyad, reporter i = importer, partner j = supplier | imports of i from j (`imp_trade_index` 2), i's ES need (`es_need` 2), j's EO (`eo_partner` 2), i's outward investment in j (`outbound_index` 2: the CM-FDI manufacturing-investment tie in Solar, Wind, Batteries and EV Midstream, the IMF DIP position elsewhere), same-bloc alignment (`geo_alignment` 2), Chinese-parent ownership exposure of j (`chinese_parent_exposure` 1, scored as 1 − share; CM-FDI cells only), `econ_opp_raw` 0; less a CAT climate penalty |
| **Opportunity** OP(i, j) | dyad, reporter i = exporter, partner j = market | exports of i to j (`trade_index` 3), i's EO (`econ_opp_index` 3), j's ES need (`partner_es_need` 1); climate penalty 0.05 |
| **Development** D(j) | partner only, non-high-income partners | WDI development, Doing Business, investment environment, j's ES and EO, OECD CRS aid to the supply chain (absolute and per GDP), energy use (weights 1, 1, 1, 1, 1.5, 1.5, 2, 1) |

Each component is a weighted mean of the parts present (a missing part drops out), S-curved within its comparison set. Each dyad (or partner, for Development) is summarised by the mean of its **top-5** (tech, stage) cells over the 11 headline technologies; self-dyads are excluded.

**Bilateral trade (scripts/15, `config/partnership.yml`):**
- **Source:** the all-reporter Comtrade dyad pull (`comtrade_energy_dyads_all.csv`, 2020 and 2024, imports and exports, the HS6 basket; [below](#the-all-reporter-comtrade-dyad-pull)); the 22-reporter allied file is a fallback with a warning.
- **Importer-first / mirror rule:** per (importer, cell, year) the importer's declarations are used when it reports the cell (0 where it records nothing); otherwise the exporter's declared exports × **1.06** (IMF DOTS CIF/FOB convention; median 1.05 in the pull); otherwise unobserved. FS reads the import view and OP the export view of the same reconciled matrix.
- **Two-stage trade index:** S-curve of last-year level and growth (2:1) across trading dyads within (tech, stage), then within the reporter. Observed zeros form one tie block below every trading partner; unobserved cells take the (tech, stage) mean (`trade_status` `trade` / `zero` / `imputed`; `no_data` where no HS6 code maps to the cell). Coverage per cell is in `partner_trade_coverage_tbl.rds`.

**Outbound investment and conduits:** `outbound_index` is from IMF Direct Investment Positions (outward debt-instrument positions). Positions with a **conduit financial centre** are unobserved and the component renormalises away, on both sides of the dyad (`conduit_scope: both`): the ten pass-through economies of Damgaard, Elkjaer and Johannesen (NLD, LUX, HKG, VGB, BMU, SGP, CYM, CHE, IRL, MUS) plus the six offshore centres excluded before (BHS, BRB, TCA, IMN, GGY, CYP). `conduit_rule: legacy` reproduces the earlier rule.

**Cleantech manufacturing FDI (CM-FDI; since 2026-10-11).** `data/raw/cm_fdi_projects.csv` is the Cleantech Manufacturing FDI Dataset of the Net Zero Industrial Policy Lab, Johns Hopkins University ([CM-FDI]; edition 2026.10.05, CC BY 4.0): 1,042 cross-border greenfield and expansion projects in solar, wind, battery and EV manufacturing, each with its parent company, the parent's headquarters economy, destination, status, years announced and completed, industry activity, capital investment (US$ m; 259 undisclosed, which is not zero), joint-venture fields and a source link per field. Settings: `config/partnership.yml` `cm_fdi`; code: `R/themes/partnership_strength/cm_fdi.R` (pure; scripts/15 reads the file).

- **Projects used by the indices:** industry activity "Manufacturing" (974; R&D, design and logistics projects are descriptive only, in Sector Insights), status Announced, Under Construction, Completed or Operational (the CM-FDI working paper's exclusion of Cancelled, Paused and Closed), not marked "Ceased operations" or "Paused" in the Operational field (among them 28 completed plants that have since closed), home economy other than the destination: 905 projects. Every project maps to the technology's Midstream. **Time window: all announcement years** (`announced_from: null`): both measures read the stock of foreign-owned capacity standing or in the pipeline, as the IMF position they replace is a stock; closed plants are already dropped; a 2015 cut would drop 131 standing plants (14% of those used), 116 of them non-Chinese, and raise measured Chinese exposure mechanically. **Joint ventures** count `jv_parent_share` = 0.5 to the listed parent; the partner's half is left out, because CM-FDI does not code the partner's economy (197 of the 905 are JVs).
- **Investment ties.** In Solar, Wind, Batteries and EV Midstream, `outbound_index` of FS(i, j) is the tie of parents headquartered in i (the investor) to projects located in j (the destination), the same direction as the IMF position it replaces: the mean of the S-curves of the attributed project count and the attributed disclosed value across dyads with a project in the cell, then an S-curve within the reporter, with the partners without a project as one tie block below. A reporter with no project abroad in the cell has no tie (NA; the component drops out). No conduit rule applies (project records of operating companies, not financial positions). The column `outbound_source` (`cm_fdi` / `imf_dip`) of the dyad builder records the measure. Every other cell keeps the IMF position.
- **`chinese_parent_exposure`.** The share of supplier j's foreign-invested manufacturing in the technology whose parent is headquartered in China (CHN, HKG, MAC; Taiwan not included), the same for every reporter. It is value-weighted where values are disclosed and count-based for the rest (`exposure_weighting: blend`: disclosed projects carry their count share, split among them by value; undisclosed projects their count share; `value` and `count` are the alternatives). It is defined only where j has at least `min_projects` = 2 such projects in the cell (with one the share is 0 or 1 by construction) and is otherwise **missing, never 0**; China, Hong Kong and Macao as suppliers get 1 (`china_supplier: exposed`), since their own foreign-invested plants are non-Chinese by definition. It enters Friendshore as 1 − share with weight 1, beside `geo_alignment` at 2 (geo is 2/11 of the FS weights). It is the parent-company case of common-mode risk, the upgrade proposed in section 7.4 of the NEIS working paper *Allied Industrial Base: Conceptual overview, literature review and analytical design*: plants in two supplier countries owned by the same Chinese parents do not diversify supply away from China.
- **Rankability:** both CM-FDI parts count as observed where defined and are not part of the cell where undefined (a census has no gaps to impute); a missing IMF position still counts as unobserved in the other cells.
- **Switches** (robustness): `cm_fdi: investment_ties: false` restores the IMF measure in every cell; `chinese_parent_exposure: false` leaves the component out of every dyad (the same fsi_raw as weight 0); with both off the CSV is not read. scripts/15 writes `cm_fdi_projects_tbl.rds`, `partner_cm_fdi_ties_tbl.rds`, `partner_cm_fdi_exposure_tbl.rds` and `partner_cm_fdi_coverage_tbl.rds` ([data dictionary](docs/data_dictionary.md)).
- **Effect** (function level, on the 2026-10-10 final-run data): every exported column of the 1,352,094 dyad cells outside the four covered cells is unchanged; in the covered cells 12 to 20 investor reporters have ties and the exposure is defined for 24 to 28 partners per cell; FS Spearman old vs new 0.978–0.984 there; coalition PSI Spearman 0.9998 (max rank change 5), world view 0.9996 (max 8). Suppliers whose foreign-invested plants are mostly Chinese-owned fall in the covered cells (exposure: Thailand Batteries 0.97, Viet Nam EV 1.00, Malaysia EV 0.90, Indonesia Solar 0.85, Turkiye 0.63–1.00, Hungary Batteries 0.55; Mexico 0.00–0.35). Economic Opportunity, Opportunity and Development do not read CM-FDI (EO's Investment category already scores investment).

Cite as: Bandara, P. and the Net Zero Industrial Policy Lab. Cleantech Manufacturing FDI Dataset, edition 2026.10.05. Johns Hopkins University. https://cmfdi.netzeropolicylab.com (CC BY 4.0; counts, shares and indices computed by OSI from the project records).

**PSI composite and views (scripts/25):**

```
C^k_j  = mean over the view's reporters i != j of the dyad's top-5 score (Development: j's own)
PSI_j  = sum_k w_k C^k_j / sum over the categories j has of w_k      w: FS 0.4, OP 0.4, Dev 0.2
```

A high-income partner (no Development) gets (0.4 FS + 0.4 OP) / 0.8, not a score capped at 0.8.
- **Core-coalition view (headline, `psi_tbl.rds`):** reporters = the core tier of `coalition_tbl.rds` (31).
- **Full-coalition view (`psi_full_coalition_tbl.rds`):** core and partner tiers.
- **World view (`psi_world_tbl.rds`):** every reporter (219 economies).
- **Dyadic PSI (`psi_dyad_tbl.rds`):** the same composite per (reporter, partner), with `coalition_reporter` and `reporter_tier`.

**PSI rankability:** a partner is ranked only if it is sovereign and its `imputed_weight_share` ≤ 0.5 (`config/rankability.yml` `psi`). The share counts partner- and pair-specific inputs that are imputed or missing (trade cells imputed at the cell mean, no DIP position outside the CM-FDI cells, an assigned rather than voted bloc, missing Development indicators, and the partner's ES/EO cells at their own pillar share), averaged like the score. At 0.5 the core view ranks 192 of 219 partners (Monaco, Liechtenstein and Kosovo drop out among sovereigns).

**Implementation:** `R/themes/partnership_strength/{safer_friendshore,prosperous_opportunity,stronger_development,partnership_strength_helpers}.R`, `R/indices/build_partnership_strength_index.R`.

---

### Allied network design

**What it captures:** a coalition-wide division of labour: for each technology × stage, which nodes produce, with what share, and which producer → consumer flows serve each consumer.

**Scope and inputs (`scripts/30_build_allied_network_design.R`, `R/indices/allied_network_design.R`):**
- **Nodes:** the coalition members, with the EU-27 bundled as one node `EUU` (37 nodes on the inputs of 2026-10-09: 31 core and 26 partner members plus the 6 other EU members). A bundled node's inputs are aggregated from its members **before** any ranking: EO, ES and development are stage-weighted means over members with a value; demand size is the summed GDP and need the GDP-weighted mean member need; an edge to or from the node is the members' edge mean weighted by their 2024 basket trade on that edge (else stage weights, else equal); edges inside the node are dropped. `allied_network_member_shares.csv` splits a node's designed share back to its members by stage weight (derived, not designed).
- **Stages:** **Upstream and Midstream only** (22 stage problems). Downstream deployment is not part of the allied division of labour.
- **Producer score:** EO and ES only (`node_policy = FALSE`; weights 0.5 and 0.2 renormalised to 5/7 and 2/7). The policy term is off (decided while the Policy index required CAT and covered fewer members, whose gaps had to be filled with the stage median; every member now has one, 11 on IEA PAMS alone).
- **Demand:** `demand_mode = "mixed"`, need 0.5 and size 0.5.
- **Edges:** edge(p → c) = 0.5 FS(reporter = c, partner = p) + 0.5 OP(reporter = p, partner = c): Friendshore is rated by the importer and Opportunity by the exporter, so both halves rate the producer → consumer link. A missing component counts as 0 on the edge; a node's self-supply edge takes the stage's median edge weight.

**Objective per stage (maximised):**

```
w_node * sum_p a_p x_p + w_edge * sum_pc W_pc f_pc + w_dev * sum_p dev_p z_p - lambda * sum_p x_p^2
```

with x_p the production share, w_node = 1, w_edge = 0.5, w_dev = 0. **Diversification:** `lambda` (score units per unit of HHI) turns the bang-bang linear design (λ = 0: producers at the 0.40 cap) into interior shares. The MILP carries Σx² as perspective tangent cuts on a 0.01 share grid (error ≤ 2.5e-5 per producer).

**Calibration rule for lambda:** the smallest λ on a grid at which the **median designed HHI over the 22 stages ≤ the median observed export HHI of the network's own nodes** (2024 basket trade from the reconciled Comtrade matrix, shares within the nodes, the EU-27 as one exporter counting only extra-EU exports; median over the 18 stages with an HS6 basket; written to `allied_network_observed_hhi.csv`). On the 37-node set of the final rerun (2026-10-12: EIA-filled production, the EV, Solar, Wind and Nuclear Upstream baskets, CM-FDI in Friendshore) the target was 0.1556 (6.4 equivalent exporters, 18 basket stages) and the pick **λ = 0.78** (median designed HHI 0.152, all 22 stages solved by MILP; 0.77 gives 0.1558). The pick is knife-edge with respect to the inputs, so it must be re-derived on each full rerun; `scripts/30` warns when the designed median exceeds the observed one.

**Constraints and solver settings:** `method = "auto"` (MILP for stages with ≤ 60 nodes, greedy fallback recorded in diagnostics), GLPK time limit 300 s per stage; `min_producers = 3`, `max_share = 0.40`, `min_share = 0.05`, `min_suppliers_per_consumer = 2`, `epsilon_supplier_share = 0.10`, `allow_self = TRUE`. **Portfolio caps (headline):** each node may appear in the stage top-5 at most `cap` times over the 22 stages, `cap` from log GDP between 2 and 10 (the EU node's cap uses the summed EU GDP), enforced iteratively (up to 30 passes; top-5 membership by strict rank). The no-caps design is the robustness variant.

**Outputs:** `allied_network_specialization.csv`, `_flows.csv`, `_diagnostics.csv` (method, fallback reason, HHI, effective producers, objective terms, tangent error), `_members.csv`, `_member_shares.csv`, `_nodes.csv`, `_node_edges.csv`, `_observed_hhi.csv`, `_portfolio_caps.csv`, `_portfolio_counts.csv`, `_topk_by_stage.csv`, `_build_candidates.csv`, `_input_checks.csv`.

---

## Repository layout

```
R/                    # Pure functions (no IO)
  utils/              # Helpers: schema, theme standardization, country harmonisation, HS6 crosswalk
  categories/         # Theme builders by category
  indices/            # ES/EO builders, Policy index, Strategic Index, coalition, PSI, allied network
  geopolitical_distance/  # UNGA ideal points and alignment
  themes/             # Bilateral partnership themes (Friendshore, Opportunity, Development)
  charts/             # Chart builders (no file IO)
  dashboard/          # Pure builders of the dashboard data (Strategic bundle; sector/ and country/ helpers)

scripts/              # Orchestration + IO (reads raw, writes processed/outputs)
  fetchers/           # One file per API source; registered, contract-validated
  utils/              # Bootstrap, manifest/sync helpers, fetcher framework, Comtrade dyad helpers
  paper/              # Figure and table scripts of the working paper

data/
  reference/          # Project-authored crosswalks and transcribed tables (version controlled)
  raw/                # Local raw inputs (gitignored; populated by ingestion)
  processed/          # Theme, index, partner and network tables (gitignored)

config/               # Weights, missing data, index definitions, rankability, alignment,
                      # coalition, partnership settings, raw-input manifest
docs/                 # Methodology, data dictionary, sources
paper/                # Working paper (LaTeX)
dashboard/            # Allied Industrial Base Explorer (static React app; see dashboard/README.md)
run_pipeline.R        # Pipeline entry point
tests/                # Unit tests (testthat)
```

---

## Quick start

1) **Copy config and update for your environment**

```bash
cp config/config.example.yml config/config.yml
```

The tracked configuration lives in `config/weights.yml`, `config/missing_data.yml`, `config/index_definition.yml`, `config/rankability.yml`, `config/geopolitical_distance.yml`, `config/coalition.yml`, `config/partnership.yml` and `config/iea_clean_tech_guide_tech_map.yml`.

2) **(Optional) Run without local raw data**

Set `SKIP_DATA_DOWNLOADS=true` (or `1`) so `scripts/10_build_themes.R` can exit cleanly when inputs are missing.

3) **Run the pipeline in order**

If you are refreshing UN Comtrade outputs, set your API key first (never commit it):

```bash
export COMTRADE_API_KEY=your_key_here
# Optional overrides (defaults shown):
# export COMTRADE_TARGET_YEAR=$(($(date +%Y)-1))
# export COMTRADE_START_YEAR=$((COMTRADE_TARGET_YEAR-4))
# export COMTRADE_CHUNK_COUNT=8
# export COMTRADE_CHUNK_INDEX=1   # run 1..8 in separate executions
# export COMTRADE_REQUEST_TIMEOUT_SECONDS=120
# export COMTRADE_MAX_RETRIES=3
```

Then run (the order matters: 13 reads the trade theme of 10 and the alignment of 12; 15 reads the pillars of 20 and the alignment; 25 reads the top-5 tables of 15 and the coalition of 13; 30 reads all of them):

```bash
Rscript scripts/00_setup.R
Rscript scripts/04_build_hs6_views.R
Rscript scripts/05_ingest_sources.R
Rscript scripts/10_build_themes.R
Rscript scripts/12_build_geopolitical_alignment.R
Rscript scripts/13_build_coalition.R
Rscript scripts/20_build_indices.R
Rscript scripts/15_build_partner_themes.R
Rscript scripts/25_build_partner_indices.R
Rscript scripts/30_build_allied_network_design.R
Rscript scripts/80_write_outputs.R
```

For an end-to-end run (`00, 05, 07, 10, 12, 13, 20, 15, 25, 30, 80`; 30 unless `OPSI_RUN_ALLIED_NETWORK=false`):

```bash
Rscript run_pipeline.R
```

Run the allied network only:

```bash
Rscript scripts/build_allied_network.R
```

Optional environment variables:

- `OPSI_INSTALL_MILP_PKGS=true|false`, `OPSI_REQUIRE_MILP=true|false` — MILP solver stack (ompr, ROI, GLPK)
- `OPSI_AUTO_PREREQS=true|false`
- `OPSI_RUN_ALLIED_NETWORK=true|false`
- `OPSI_SKIP_NIPO=true|false` — reuse the stored NIPO rows of `policy_component_tbl.rds` instead of rescoring the GTA extract (4–5 GB of memory); NIPO is not in the Policy index composite
- `OPSI_NIPO_SCOPE` — NIPO country scope (`allies` keeps the legacy ally list)
- `OPSI_CONFIG`, `OPSI_WEIGHTS`, `OPSI_MISSING_DATA`, `OPSI_INDEX_DEFINITION`, `OPSI_RANKABILITY` — alternative config files (scratch runs)
- `OPSI_OUTPUTS_RDS` — path of `index_outputs.rds`
- `OPSI_COALITION_OUT_DIR` — where `scripts/13` writes (default `data/processed`)
- `OPSI_FORCE_REFRESH=true|false` — recopy every staged raw input even when the local copy looks current (see [Raw inputs and refresh](#raw-inputs-and-refresh))
- `OPSI_PREFER_FETCH=true|false` — treat the API as authoritative for every input that has a fetcher, for one run
- `OPSI_SKIP_FETCH=true|false` — disable API fetchers entirely
- `OPSI_REQUIRE_FETCH=true|false` — make a failed fetch fatal instead of falling back to the local file
- `OPSI_IMF_START_YEAR=<year>` — earliest year kept by the IMF fetchers (default 1990; on PPI and lending rates the window controls country coverage, and on commodity prices it must stay well behind the 20-year volatility window)

When `COMTRADE_CHUNK_COUNT > 1`, each run writes chunk files under `data/raw/comtrade_chunks/...` and combines them into final CSVs once all chunk indices have been run.

---

## Interactive explorer

`dashboard/` holds the Allied Industrial Base Explorer, a static React app (Vite, TypeScript, NEIS design) with a Home page and three modules:

* **Strategic Insights** (normative) recomputes the indices, partnership scores and the allied network in the browser when a reader moves the four preference dials. Its data bundle (`dashboard/public/data/`) is exported from the pipeline outputs by `scripts/85_export_dashboard_data.R` (builders in `R/dashboard/`), which also checks the dashboard engine at its baseline against the stored outputs; rerun it after a pipeline rerun, then `scripts/86_export_dashboard_fixtures.R` for the engine's parity fixtures. The TypeScript engine in `dashboard/src/engine/` is a port of `R/dashboard/` and is parity-tested against it.
* **Sector Insights** (descriptive) shows the measured data behind the indices, by technology and supply-chain stage, in each source's own units and years: trade (the reconciled Comtrade dyad matrix), investment, cross-border manufacturing projects (CM-FDI, announced 1996-2026, in Solar, Wind, Batteries and EV Midstream: home → destination flows, the parent-HQ group split - China, coalition core, partners, rest - announcements by year and the project table with status and CM-FDI's source links; project counts and disclosed US$ m only, an undisclosed value never read as 0, the CC BY attribution on every card), reserves, production, consumption, prices, policy (IEA PAMS records and CAT ratings), time series (EI 1965-2025, IMF monthly prices, CIM investment and capacity, IEA EV regions) and, under Development & finance, the raw data behind Development Potential and Friendshore's investment tie (since 2026-10-15): the World Bank WDI series and Doing Business score the builder ranks, OECD CRS official development assistance received and given by aid sector (energy, industry, mining: the stage the builder maps each to) with the donor -> recipient flows, and the IMF bilateral direct-investment positions (who invests where; conduit financial centres shown and flagged), each read with the builders' own functions and checked against them (`--no-dev` leaves them out). No index, weight or dial value appears in it. Its data (`dashboard/public/data/sector/`) are exported by `scripts/87_export_sector_data.R` (pure helpers in `R/dashboard/sector/`, captions and units from `config/sector_dictionary.yml`): every value with a `data/processed` source is checked `identical()` to it after the encode/decode round trip, and the run writes `data/processed/dashboard/sector_validation.csv`. Rerun it after a pipeline rerun (`--check-only` runs every check without writing; `--synthetic <dir>` writes the small test fixture). Restricted sources (BNEF, BCG) never ship.
* **Country Insights** (a hybrid of the two) puts one economy's index results and the data behind them on one page, on the paper baseline (with a link to the same country in Strategic Insights): its sectors on the Economic Opportunity x Energy Security plane (each axis the share of other economies it beats in that sector) with the Grow / Upgrade / Secure / Watch quadrants split at the country's own median sector, so they show its relatively strong and weak sectors, the drivers of its high-priority sectors in raw data, its sector priorities (the Strategic Index without its policy term) against its policy activity, the strongest active industrial-policy measures per sector from the latest GTA NIPO run (context only, not part of the Policy index), and partner priorities against today's trade, factory projects, agreements and UN-vote alignment. The 31 core members have an AI-written policy memo (on screen, PDF and Word) in which every number is a token filled from the country's fact sheet, checked by a linter and an independent AI verifier. Data and memos are exported by `scripts/88_export_country_data.R` (pure helpers in `R/dashboard/country/`, settings in `config/country_insights.yml`, memo sources in `dashboard/country-memos/`).

To run it: `cd dashboard && npm ci && npm run dev`; `dashboard/README.md` covers tests, the build and the Netlify deployment.

---

## Outputs

Outputs are written under the configured outputs directory (default `data/processed/outputs`) and `data/processed`:

* `index_outputs.rds`: ES and EO indices (with `imputed_weight_share`, `rankable`, `rank_exclusion`), category scores, category and variable contributions, builder diagnostics, the Policy index with its eligibility and provenance, the Strategic Index, and the coalition used
* `energy_security_category_contributions.csv`, `energy_security_variable_contributions.csv`
* `charts/allied_strategic.csv` and `strategic_index_selected_countries.xlsx`
* coalition, alignment, partner and PSI tables (`*.rds` in `data/processed`)
* allied network CSVs (`allied_network_*.csv`)

`docs/data_dictionary.md` lists the columns.

---

## Comparing index vintages across years

The main pipeline produces one snapshot: every theme builder reaches for the newest observation its source carries, so `data/processed` holds a single vintage. `scripts/40_build_index_vintages.R` builds the ES and EO pillars at a chosen set of years instead:

```bash
Rscript scripts/40_build_index_vintages.R --years=2020,2025
```

Years can also be set with `OPSI_INDEX_YEARS=2015,2020,2025`. The default is `2020,2025`. The comparison table is built between the earliest and the latest year.

It slices each **raw input** to the requested year and runs the same theme builders and v2 index builders the annual pipeline uses. `scripts/10_build_themes.R` must have been run first, because themes with no time dimension are read from `data/processed` and reused unchanged across vintages.

**What actually moves.** Only sources with a genuine time dimension can be re-cut: EI Statistical Review, the Atlas of Economic Complexity (trade, both the ES and EO trade themes with the deficit term), IMF monthly prices and rates, ILO earnings and Clean Investment Monitor investment. Resource potential, reserves, the IEA critical minerals release, BNEF demand and LCOE, overcapacity and TRL are single snapshots or projections, held fixed. Every run writes the share of each pillar's scored weight that re-derives per vintage (`index_vintage_weight_coverage.csv`) and the year each source resolved to (`index_vintage_source_years.csv`; the Atlas stops at 2023, so a 2025 vintage uses Atlas 2023).

**Reading the results.** Every component is a cross-sectional percent rank, so an index value is a country's standing against its peers in that year. Both level change and rank change are written out.

Outputs land in `data/processed/vintages/`:

| File | Contents |
| --- | --- |
| `energy_security_index_by_year.csv` | ES pillar index per country × tech × supply chain × vintage year, with `Energy_Security_Risk` as the complement |
| `economic_opportunity_index_by_year.csv` | EO pillar index, same keys |
| `index_category_scores_by_year.csv` | Category scores behind both pillars |
| `energy_security_comparison_<a>_vs_<b>.csv` | Level and rank in each end year, plus `index_change` and `rank_change` |
| `economic_opportunity_comparison_<a>_vs_<b>.csv` | Same for EO |
| `index_vintage_theme_provenance.csv` | Per theme: whether it varies with the year, and why |
| `index_vintage_weight_coverage.csv` | Share of each pillar's scored weight that re-derives per vintage |
| `index_vintage_source_years.csv` | The year each source actually resolved to, per vintage |
| `index_vintages.rds` | All of the above plus the full builder outputs per year |

See [Known issues](#known-issues-and-to-dos) for the two ways the vintages still differ from the headline build.

---

## Testing and quality checks

The `testthat` suite under `tests/testthat/` covers, among others:

* S-curve normalization, schema checks and country harmonisation
* ES/EO v2 construction: available-case aggregation, observed-only imputation, contributions, rankability
* Theme builders (energy, minerals incl. the mirror, uranium, trade, TRL stage mapping, HS6 crosswalk, GHG weights)
* Policy index and Strategic Index (weights, eligibility, exclusions, coalition wiring)
* Geopolitical alignment, the ideal-point fetchers and the territory map
* Coalition tiers and EU stage weights
* Partnership themes: trade flows (importer-first / mirror rule), conduits, PSI viewpoints and rankability, Comtrade dyad helpers
* Allied network: integrity, diversification, EU node, demand modes, portfolio constraints
* Raw-input manifest drift and sync, fetchers
* Dashboard bundle container (OSB) round trips and the engine's parity fixtures (the app's own tests run with `npm test` in `dashboard/`)
* Sector Insights export helpers: the dictionary validator, facts and value states, the trade matrix and zero rebuild, concentration summaries, chunk round trips and the parity ledger (`test-sector-*.R`, synthetic inputs only)

Run tests from the repo root with:

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

---

## Additional utilities and workflows

* `scripts/01_generate_raw_inputs_manifest.R` — regenerates the raw-input inventory by scanning the active `scripts/` and `R/` code.
* `scripts/02_render_sources_doc.R` — renders `docs/sources.md` from the manifest.
* `scripts/03_check_raw_inputs.R` — read-only preflight: which raw inputs would block a build and where each comes from.
* `scripts/04_build_hs6_views.R` and `R/utils/hs6_crosswalk.R` — regenerate the HS6 crosswalk views from the master.
* `scripts/12_build_geopolitical_alignment.R`, `scripts/13_build_coalition.R` — alignment and coalition (above).
* `scripts/85_export_dashboard_data.R` — exports the dashboard bundle to `dashboard/public/data/`.
* `scripts/86_export_dashboard_fixtures.R` — writes the TypeScript engine's parity fixtures.
* `scripts/87_export_sector_data.R` — exports the Sector Insights data to `dashboard/public/data/sector/` (`--check-only`, `--synthetic <dir>`).
* `scripts/88_export_country_data.R` — exports the Country Insights data to `dashboard/public/data/country/` (`--check-only`, `--synthetic`, `--iso3`, `--facts-dir <dir>`, `--save-ctx` / `--load-ctx`; `--render-memos [--lint-only]` lints and publishes the policy memos; rendering needs Edge or Chrome and Python with python-docx and pypdf).
* `scripts/90_build_package_selection_viz.R` — package-selection visualization outputs.
* `scripts/96_pull_trade_timeseries.R` and `R/charts/trade_timeseries.R` — trade time series.
* `scripts/98_pull_comtrade_energy_dyads.R` — the all-reporter Comtrade dyad pull (below).
* `R/charts/export_similarity.R` — Finger–Kreinin export similarity between the six major energy-technology exporters (CHN, DEU, FRA, JPN, KOR, USA) over the HS6 basket, at `ESI_LEVEL` (`hs6`/`hs4`/`hs2`/`tech`/`sub_sector`, default `hs4`); only comparable within a level.
* `docs/data_dictionary.md` and `docs/comtrade_ingestion_and_timeseries.md` — data fields and Comtrade workflows.

---

## Raw inputs and refresh

`config/raw_inputs_manifest.yml` is the single inventory of every raw input the pipeline reads. Each entry records where the file comes from and how often it changes:

| `source_type` | Meaning |
| --- | --- |
| `api` | Fetched automatically during ingestion (Comtrade, WDI, IMF, UN ideal points) |
| `manual` | Staged by hand into `sharepoint_raw_dir` before a run |
| `derived` | Authored by the project — a crosswalk, not a fetched dataset |
| `generated` | Written by the pipeline as bookkeeping |

`staged_from` says how each input reaches `data/raw/`: `sharepoint` (copied from your staging folder), `repo` (version controlled in `data/reference/`), or `pipeline` (written by an ingestion step).

The manifest is generated **from the active pipeline**, so it cannot drift away from what `scripts/` reads:

```bash
Rscript scripts/01_generate_raw_inputs_manifest.R
```

Curated fields (`source_name`, `url`, `cadence`, `owner`, `vintage`) survive regeneration; `required_by` is refreshed from code. `tests/testthat/test-raw-inputs-manifest.R` fails if the manifest and the pipeline disagree in either direction. `docs/sources.md` is rendered from the manifest — edit the manifest, not the doc:

```bash
Rscript scripts/02_render_sources_doc.R
```

### API fetchers

Inputs with a public API are fetched by `scripts/fetchers/*.R` and written in the layout the pipeline parses:

| Input | Source | Notes |
| --- | --- | --- |
| `wb_wdi.csv` | World Bank v2 API | 44 series, DataBank wide layout |
| `wb_doingbusiness.csv` | World Bank v2 API | Archived series; reproduces 190 of 192 economies |
| `imf_ppi.csv` | IMF SDMX 3.0 | Producer price index |
| `imf_lending_rates.csv` | IMF SDMX 3.0 | The six rate types the theme scores |
| `imf_commodity_prices.csv` | IMF SDMX 3.0 | PCPS world price panel (`prefer`) |
| `un_ga_ideal_points_all.csv` | Harvard Dataverse (doi:10.7910/DVN/LEJUQZ) | All-votes ideal points (alignment default; ends 2023) |
| `un_ga_ideal_points.csv` | Harvard Dataverse | Final-passage ideal points (sensitivity; to 2025) |
| `critmin_mirror_import.csv` | UN Comtrade public preview API (keyless) | Partner-level critical-mineral imports from non-reporters, plus late reporters' own exports (optional) |
| `eia_intl_fossil_production.csv` | EIA International Energy Statistics bulk file `INTL.zip` (keyless, public domain) | Oil, gas and coal production and consumption by country, 2000 on; fills the fuel production EI does not report. Staged copy kept (`fallback`; EIA revises monthly); refetch with `Rscript scripts/fetchers/eia_intl.R` |

Three properties matter:

- **A failed fetch never destroys a good file.** Every fetch is written to a temp path, validated against a declared contract (required columns, minimum rows, uniqueness key), and only then swapped into place. `OPSI_REQUIRE_FETCH=true` makes a failure fatal.
- **`fetch_policy` decides who wins.** `prefer`: the API is the authority. `fallback` (the default): a staged file wins and the fetcher only runs when no local copy exists.
- **Coverage differences are recorded, not hidden.** `imf_ppi` and `imf_lending_rates` are `fallback` because the live flows no longer publish every country in the staged extracts (see their manifest `notes`).

Not automatable, and marked `manual`: the Energy Institute download (Cloudflare challenge), BNEF, BCG, GTA NIPO and the IEA extracts (subscription or licence-restricted), and the CM-FDI project export (`cm_fdi_projects.csv`, the CSV download of https://cmfdi.netzeropolicylab.com, staged byte-identical; no API).

### The all-reporter Comtrade dyad pull

`scripts/98_pull_comtrade_energy_dyads.R` writes `data/raw/comtrade_energy_dyads_all.csv`: every reporter Comtrade has in 2020 and 2024, all partners, imports and exports, the HS6 basket (with legacy bridge codes). Comtrade's "Other Asia, nes" (S19) is recoded to Taiwan (`TWN`; `COMTRADE_DYADS_TAIWAN=drop` drops it); World, the other "nes" areas, bunkers and free zones are dropped. Calls are packed under the free tier's record cap, counted against `COMTRADE_DYADS_MAX_CALLS` per day, and every response is kept as a chunk so an interrupted run resumes; `COMTRADE_DYADS_STAGE=combine` rebuilds the CSV from the chunks without API calls. It needs `COMTRADE_API_KEY`.

**Codes added to the basket after the pull** are fetched on their own instead of re-pulling the basket: give the pull a new `COMTRADE_DYADS_CHUNK_DIR` holding copies of the main pull's `getDA_<year>.json` (so the reporters are the same), list the codes in `COMTRADE_DYADS_CODES`, fetch, then rebuild the file with `COMTRADE_DYADS_STAGE=combine` and `COMTRADE_DYADS_COMBINE_DIRS` naming the new directory (the directories must not share a code). The combine step stops when the basket has a code that none of the directories holds, so a re-combine of the main directory alone (for example to re-run the Taiwan recode) cannot silently drop a supplementary pull's codes; `COMTRADE_DYADS_ALLOW_PARTIAL=true` combines without them and the log lists them. The Electric Grid Upstream codes 7402.00, 7403.11, 7601.10 and 7601.20 were added this way on 2026-10-09 (14 calls), and the new codes of the Upstream baskets of 2026-10-11 - uranium ores (2612.10), monazite (2612.20), silicon metal (2804.69) and the HS92 bridge 8112.91 - on 2026-10-12 UTC (14 calls); the dyad file combines the main chunk directory with both supplementary ones (`config/raw_inputs_manifest.yml` records the vintages), and the `energy_trade` stage reads all three.

**The country-level trade files come from the same chunks.** Every chunk also holds each reporter's World row (its total over all partners), the record the country-level trade themes read. `COMTRADE_DYADS_STAGE=energy_trade`, run with the same `COMTRADE_DYADS_CHUNK_DIR` and `COMTRADE_DYADS_COMBINE_DIRS` as the combine (together the whole basket, or it stops), rewrites `comtrade_energy_trade.csv` from those World rows in the file's year - every basket code and every reporter of the pull, Taiwan as `TWN` - and refreshes the RCA denominator `comtrade_total_export.csv` for the same reporters with one TOTAL-exports call per `COMTRADE_DYADS_MAX_REPORTERS` reporters (the responses are kept in `COMTRADE_DYADS_TOTALS_DIR` and reused, so a rerun makes no call; `COMTRADE_DYADS_ENERGY_TRADE_DIR` writes the two files elsewhere for a dry run). This replaces the ~400 calls of a `scripts/05` energy pull. Both files come from this stage since 2026-10-09: the `scripts/05` pull staged 2026-09-30 had been made with an older product list and lacked 60 of the 256 basket codes (19 of the 20 Semiconductors Midstream codes, and 14 Electric Grid, 13 Solar and 19 EV Midstream codes) and 13 reporters, among them DR Congo, Taiwan, Iraq, Algeria and Oman. The 13 basket codes still absent (12 before the 8112.91 bridge of 2026-10-11) are legacy bridge codes with no 2024 record. A `scripts/05` run with downloads on overwrites both files with its own pull; rerun the stage afterwards.

### The HS6 crosswalk

`data/reference/energy_hs6_master.csv` is the **single source of truth** for the HS6 → technology / supply-chain / sub-sector mapping. Its columns are `tech`, `supply_chain`, `sub_sector`, `hs6`, `essential`, `legacy_hs`, `excluded`, `note`.

- **Dominant-use rule.** A code belongs in a technology's basket only if that technology is a dominant use of the product. Codes that fail stay in the master with `excluded = TRUE` and the reason in `note` (91 rows in the October 2026 review: residual chemicals 3824.99 and PE/PP resins from Batteries Midstream, aircraft engines from Coal and Gas Midstream, general machinery n.e.s. from Oil and Gas Midstream, copper ore from Batteries Upstream, electrical energy 2716.00 from Electric Grid Upstream, ...; 92 rows since 8517.62, switching and routing apparatus, left Electric Grid Midstream on 2026-10-09 as telecommunications equipment, like the coaxial and optical-fibre cables); 259 codes are in the basket since the Upstream baskets of 2026-10-11. Generic codes kept although the technology is not their only use say "kept with caveat" with their basket share.
- **Electric Grid Upstream** holds the grid's copper and aluminium inputs (since 2026-10-09; electricity, its only code before the review, is not an input to building the grid): copper ores and concentrates (2603.00), unrefined copper and anodes (7402.00), refined copper cathodes (7403.11), bauxite (2606.00), alumina (2818.20) and unwrought aluminium, unalloyed and alloyed (7601.10, 7601.20). They are kept with caveat: the grid is a major use of both metals but not shown to be the dominant one, and neither has a grid-specific code. Copper ore, bauxite and alumina stay excluded from the Batteries baskets, so each code is in one basket. Wire rod and aluminium conductors stay in Electric Grid Midstream; scrap (7404, 7602) and the other refined-copper shapes (7403.12/13/19) are left out.
- **Upstream baskets of Electric Vehicles, Solar, Wind, Nuclear and Semiconductors** (since 2026-10-11; before, these stages had no trade measure), one sub-component per code. A code in two technologies' baskets counts fully in each. Rows say "kept with caveat" where the technology is a major but not the dominant use (silicon metal, rare earths, natural uranium compounds, which include conversion's natural UF6, the minor metals, and the battery-mineral caveats of Batteries Upstream).
  - *Electric Vehicles*: the battery minerals - Batteries Upstream's ores and concentrates, shared with that cell: lithium minerals (2530.90, led by spodumene), nickel, cobalt and manganese ores (2604.00, 2605.00, 2602.00) and natural graphite (2504.10) - and the magnet rare earths of the traction motor (below). Batteries' recycling codes, the refined battery chemicals and intermediates (Batteries Midstream) and copper (Electric Grid Upstream) are not in it, so Chile's lithium carbonate, Indonesia's nickel matte and the DR Congo's cobalt hydroxide are not EV Upstream trade.
  - *Wind* and *Electric Vehicles*: rare-earth ores (2612.20 - the HS files monazite, a rare-earth ore, as a thorium ore), compounds (2846.90) and metals (2805.30). The HS has no code for bastnaesite or ion-adsorption ores, so mined concentrates and mixed carbonates (Myanmar, the United States, Laos) trade as compounds beside separated oxides; compounds and metals moved from the two Midstream baskets, which now begin at the permanent magnet. Cerium compounds (2846.10) are left out.
  - *Nuclear*: uranium ores and concentrates (2612.10) and natural uranium compounds (2844.10, the heading under which most yellowcake is shipped; moved from Midstream, which keeps enriched uranium, spent fuel and reactor and steam-plant hardware). Thorium ores are left out: there is no commercial thorium fuel cycle.
  - *Solar*: silicon metal (2804.69, metallurgical grade), the feedstock of polysilicon, which stays Midstream. Quartz, silver, silver powder, tellurium and copper are left out (reasons in the rows' notes).
  - *Semiconductors* (not a headline technology): unwrought gallium, germanium and indium (8112.92, with niobium and vanadium; moved from Midstream; HS92 bridge 8112.91).
- **Stage follows the product:** ores, concentrates, slag and scrap sit in Upstream, and a code sits in one stage of a technology (tested): where a technology's Upstream needs the first traded form of a raw material that its Midstream held, the code moved rather than being copied.
- **Legacy bridge codes** (`legacy_hs = TRUE`) carry older HS codes whose successors are all in the basket (e.g. 8541.40 for the HS2022 PV codes 8541.42/43, an upper bound since it also holds LEDs), so the HS92 Atlas and older-vintage Comtrade reporters keep the headline product.

Three legacy file names are regenerated from the in-basket rows as views during ingestion:

| Generated view | Shape | Read by |
| --- | --- | --- |
| `hts_codes_categories_bolstered_final.csv` | `Technology, Value Chain, Sub.Sector, HS6` | Comtrade code list (`05`), ES/EO trade themes (`10`), `96`, `97` |
| `consolidated_hs6_energy_tech_long.csv` | `tech, supply_chain, sub_sector, HS6` | `R/charts/35_trade_bloc_counterfactual.R` |
| `hs6_categories_with_essential.csv` | as above plus `essential` | NIPO essential-goods override (`10`), `96`, `97` |

```bash
Rscript scripts/04_build_hs6_views.R
```

**To change the mapping, edit the master.** The views are overwritten on every ingest, and `tests/testthat/test-hs6-crosswalk.R` fails if they disagree with it.

> **Other project-authored inputs** (`economic_security_agreements.csv`, `territory_sovereign_map.csv`, `uranium_resources.csv`, `uranium_production.csv`, the dual-use scores) also live in `data/reference/` and are read from there or copied into `data/raw/` during ingestion. Edit them there.

> **Tip:** `scripts/10_build_themes.R` and `scripts/15_build_partner_themes.R` remain the authoritative "what files are required to run" inventory, because they fail fast on a concrete list of paths.

### When ingestion fails

```bash
Rscript scripts/03_check_raw_inputs.R
```

Reports, without writing anything, which inputs are blocking, which are absent but covered by a fetcher, and which are absent but optional.

### How refresh works

- **Ingestion syncs rather than copies once.** `scripts/05_ingest_sources.R` compares size and mtime, so replacing a staged file with a newer vintage is enough. Force a full recopy with `OPSI_FORCE_REFRESH=true`.
- **Vintage-bearing inputs resolve to the newest match** (`GTA NIPO - <month> <year>.xlsx`, `WEO<year>_AnnexA_...`, `critmin_*_<year>.csv`, the dated BNEF exports). An input can set a reporter-count floor: the critical-minerals Comtrade pulls set `min_reporters: 50`, so a stub release with a handful of reporters is skipped for the previous full one.
- **Each run stamps what it used.** `data/raw/raw_inputs_resolved.yml` records the file and vintage behind every pattern-resolved input, alongside `data/raw/comtrade_vintage.yml`.

### Key external sources (cited)

* [IEA Critical Minerals Dataset][IEA-CM]
* [IEA ETP Clean Energy Technology Guide][IEA-CTG]
* [IEA Global EV Outlook][IEA-EV]
* [IEA Policies and Measures database][IEA-PAMS]
* [Climate Action Tracker][CAT]
* [Energy Institute Statistical Review][EI-SR]
* [U.S. EIA International Energy Statistics][EIA-IES] (bulk file https://www.eia.gov/opendata/bulk/INTL.zip; public domain)
* [USGS Mineral Commodity Summaries 2026][USGS-MCS] (data release: https://doi.org/10.5066/P1WKQ63T)
* [OECD-NEA/IAEA Uranium: Resources, Production and Demand (Red Book)][REDBOOK]
* [UN Comtrade][COMTRADE]
* [Harvard Atlas of Economic Complexity][AEC]
* [UN General Assembly ideal points (Bailey, Strezhnev and Voeten)][UNGA-IP]
* [World Development Indicators (WDI)][WDI]
* [Global Solar Atlas][GSA]
* [Global Wind Atlas][GWA]
* [BloombergNEF New Energy Outlook][BNEF-NEO]
* [IMF Commodity Prices][IMF-COMM]
* [ILOSTAT][ILO]
* [IMF Data Explorer][IMF-DEX]
* [Cleantech Manufacturing FDI Dataset (CM-FDI)][CM-FDI]: Bandara, P. and the Net Zero Industrial Policy Lab, Johns Hopkins University, edition 2026.10.05 (CC BY 4.0)

---

## Known issues and to-dos

* **Lambda must be recalibrated on every full rerun.** The rule pick (0.78 on the 37-node set of the final 2026-10-12 rerun, 18 basket stages) is knife-edge: 0.77 misses the target (0.1556) by 0.0002, and the designed median is not monotone in λ. Earlier picks were 0.94 (2026-10-10, 14 basket stages), 0.95, 0.92 (36 nodes), 0.94 (34 nodes) and 1.5 (the hand-picked coalition); any change of node set, basket or inputs moves it, and the study's trade matrix must be rebuilt on the current basket first.
* **Nuclear Upstream ES floor.** ES Nuclear Upstream is Production 1/2, Reserves 1/3 and a world energy-price constant 1/6, so every economy without uranium sits at the same floor (0.117) and has a high energy-security need. Nuclear Upstream was the top allied Strategic Index pick of 20 of 45 members in the October 2026 scratch build. Since 2026-10-09 the endowment screen bars it as a pick where the country has no recorded uranium resources or production (see [Strategic Index](#strategic-index-si)), but the floor itself stays in the index; `scripts/20` reports such floor sectors.
* **Coalition-view PSI and hard blocs.** With hard blocs and `geo_alignment` weighted like trade, every core reporter gives a U.S.-leaning partner 1 against 0.5 for a nonaligned one, so small U.S.-leaning economies (Malta, North Macedonia, Cyprus, Marshall Islands, San Marino, Iceland, ...) rank above India, China, Mexico and Singapore in the core view. The rankability rule removes only Monaco, Liechtenstein and Kosovo among sovereigns.
* **Taiwan has no WDI row.** The trade themes add it to their roster (`add_unlisted_economies()`, since 2026-10-09), so its Atlas market share, HHI and feasibility are observed and, since 2026-10-09, its Comtrade export size, RCA and share in the export HHI (the energy-trade file recodes S19 too); only deficit/GDP, which needs WDI GDP, is missing. Its bilateral Comtrade flows are observed since the S19 recode. It is outside the coalition (see [Allied coalition](#allied-coalition-two-tiers-and-the-eu-node)).
* **Electric Grid Upstream trades base metals.** Its basket (since 2026-10-09) is copper and aluminium from ore to unwrought metal, which the grid shares with building wire, equipment, transport and construction, so the cell ranks the suppliers of the grid's materials rather than of grid-specific goods. Its Comtrade-only variables (export size, RCA, HHI) include DR Congo since 2026-10-09 (the cell's second exporter, $28bn of 2024 basket exports) but miss economies that file no 2024 data with Comtrade - Guinea (bauxite), Russia and the UAE (aluminium); the Atlas market share and the bilateral matrix (mirror flows) include them. In ES the cell's Trade and Minerals Trade categories both draw on copper trade.
* **Comtrade-only trade variables cover reporters only.** Export size, RCA and the export HHI of both trade themes read `comtrade_energy_trade.csv` (2024; 142 exporting reporters, every basket code since 2026-10-09). Economies that file no 2024 data with Comtrade - Viet Nam, Russia, Iran, the UAE and Guinea among them - have no export size or RCA and are left out of the HHI; the Atlas market share and the bilateral mirror flows include them. Hong Kong's Comtrade exports are mostly re-exports: it was the largest 2024 exporter of Semiconductors Midstream (19% of the cell) and fifth in Electric Grid Midstream, so since 2026-10-09 it is left out of export size, RCA and the export HHI and their world aggregates (`trade: re_export_hubs`); it keeps its Atlas variables, and its bilateral flows stay in the partner themes and the network. Other economies with large re-exports (Singapore) stay in. HS 8517.62 (switching and routing apparatus; 28% of Electric Grid Midstream's Comtrade exports) left the grid basket the same day. Before 2026-10-09 the file lacked 19 of the 20 Semiconductors Midstream codes ($3.5bn of the cell's $1.24tn), so earlier Semiconductors trade results are not comparable. No index scores Semiconductors; since 2026-10-09 its cells (Midstream, and Upstream since 2026-10-11) take no part in the HHI S-curve's ranking (the Midstream cell had moved Batteries Midstream's HHI index from 0.290 to 0.217; 0.224 on the 2026-10-10 inputs).
* **The Upstream baskets of 2026-10-11 are thin and partly shared.** Solar Upstream is one code (silicon metal), Semiconductors Upstream one (unwrought gallium, germanium and indium), Wind Upstream rare earths only; their 2024 Comtrade basket exports are $3.5bn, $0.8bn and $1.6bn (EV $13.9bn, Nuclear $10.2bn). Silicon metal, rare earths, natural uranium compounds and the minor metals are kept with caveat, and 2846.90 and 2844.10 mix raw and processed forms. EV Upstream shares its five battery-mineral codes with Batteries Upstream and its rare earths with Wind Upstream. Large suppliers that do not report to Comtrade - Myanmar (31% of the Atlas Wind Upstream basket), Laos, Nigeria - are in the Atlas share and the mirror flows only. Australia does not publish its uranium exports in Comtrade: it reports neither 2612.10 nor 2844.10 for 2024, although importers name it the origin of 76% of 2024 2612.10 imports (5.2% of the Atlas 2023 Nuclear Upstream basket), so the Comtrade measures of Nuclear Upstream (exports, RCA, export size, HHI) miss a top-four producer and its EO Trade category is imputed; the dyad layers (partner themes, network) see it through mirror flows. The Trade category is imputed for economies with no trade in these baskets (170 of 218 in EO Nuclear Upstream), which moved 18 small economies out of the rankable set of EO Nuclear Upstream (Taiwan joined it). In ES the cells' Trade and Minerals Trade categories partly draw on the same minerals.
* **CM-FDI exposure covers foreign-invested capacity, not total capacity.** `chinese_parent_exposure` is a share of the supplier's foreign-invested plants only, so an industry that is mostly domestic can score high: Korea Batteries 0.91 (seven Chinese cathode and precursor plants, several of them JVs or announcements, against four Umicore plants; LG, Samsung and SK are domestic), Germany Batteries 0.53. A capacity-weighted version needs plant capacity by owner, which is not available here. HQ is as CM-FDI codes it (Maxeon is China in one project and Singapore in three), and status can lag events (the Vestas and Siemens Gamesa plants in Russia are still "Operational", so Russia Wind reads 0). Choices for the user to confirm, each with a setting in `config/partnership.yml` `cm_fdi`: China, Hong Kong and Macao as suppliers get 1 (`china_supplier`), `min_projects` 2, HKG and MAC parents count as Chinese (`china_hq_iso3`), "Ceased operations" and "Paused" plants dropped (`drop_operational`), the 3 "R&D & Manufacturing" projects left out (`manufacturing_activities`), JVs 0.5 to the listed parent (`jv_parent_share`); the variants measured (`china_supplier: missing`, `min_projects: 1`, `jv_parent_share: 1`, 2015 window, count or value weighting) keep the coalition PSI Spearman at 0.9998 or above (at most 7 ranks); and, without a switch, the exposure is the same for every reporter, China-bloc reporters included (world view only).
* **Memos of Belgium, Ireland and Sweden carry the first verification round only.** Their writer revised them on the fact sheets corrected on 2026-10-14 (priority and policy ranks had been offset by the out-of-sample sectors) and they lint clean, but the independent re-verification was skipped to save time; re-verify them before relying on them.
* **EU members carry the EU's regional IEA figures.** The IEA reports the 2035 announced manufacturing-capacity shares (ES `Market Share`, EO `Overall Market Share`) and the EV-region terms for the EU as one region, so every EU member carries the same value (Wind Midstream 0.15, Batteries Midstream 0.11, Green Hydrogen Midstream 0.24, Solar Midstream 0.00). Members are not ranked apart on these variables, and each scores the EU's capacity as if it were its own (Finland's Wind Midstream EO rests largely on it). The indices are unchanged; Country Insights labels the values as the EU's (`eu_region_share`) and the paper discloses it. Found by the dashboard memo verifiers, 2026-10-13.
* **Four NIPO jurisdictions lack an ISO3 code in the October 2026 index table** (`nipo_policy_index_tbl.rds`): Slovakia, Vietnam, Bosnia and Herzegovina and Chinese Taipei, whose GTA names miss the WDI name join in `R/categories/policy/nipo_policy_index.R` (on `main`). Country Insights matches measures by jurisdiction name and is unaffected; anything that joins the index table by ISO3 misses them until the fix lands.
* **Supplier-route coverage.** Chromium, molybdenum, silver and silicon have no supply data in the coalition's supplier test, and the trade-based supplier measures inherit the HS6 basket's judgement calls (re-export hubs such as Singapore, generic codes kept with caveat).
* **Mineral reserves are USGS MCS 2026 (since 2026-10-09).** The data release CSV predates the report's ver. 1.3 correction of Brazil's rare earths (21 -> 11 Mt; world >85 -> >75 Mt), which the parser applies (`usgs_mcs_errata()`; a no-op once a corrected CSV is staged); its manganese world total (1.70 Gt) disagrees with the report (1.80 Gt) and the listed rows (1.798 Gt), which the denominator rule (larger of the two) absorbs. USGS lists some holders without a figure - India, Madagascar, Myanmar, Nigeria and Thailand (rare earths), Ukraine, the United States, Austria and Germany (graphite, 's'), Côte d'Ivoire (manganese), Bolivia (zinc) - and folds others into 'Other countries' (Morocco, New Caledonia, South Africa and Zambia cobalt, which EI's older copy listed); they score like an unlisted economy (zero-filled and counted as imputed in their mineral variable, 0 in the technology roll-ups, whose cells are not flagged imputed for it). Counting such a mineral as 0 rather than dropping it from the country's roll-up weights lowers, for example, India's Wind and EV reserves and the United States' Batteries and EV reserves. Manganese reserves are gross weight (the chapter default), the other minerals metal content (REO for rare earths); only within-mineral ranks and shares are used, so bases do not mix. The coalition partner list is unchanged by the switch; 26 more scored Upstream cells pass the SI endowment screen, none fewer.
* **EO mineral Upstream production: "not named" counts as 0.** The IEA supply sheet names five to seven producers per mineral; the rest sits in "Rest of world" (33% of copper mining, 12% of cobalt, 9% of nickel, 8% of lithium, 6% of magnet rare earths, 3% of graphite). An economy the IEA names for some mineral counts 0 for the minerals it is not named for; an economy it names nowhere takes the EO global-average fill. So Electric Grid Upstream (copper only) and Solar Upstream (copper 0.994) record 0 for Australia, Indonesia, Zambia, Canada and Brazil, real copper miners, while Mexico or Kazakhstan take the average. The 2026-10-10 fix extended this observed zero from `size` to `growth_abs` and `Overall Production`, which made nine cells rankable (India and Viet Nam Batteries and EV Upstream; Lao PDR Green Hydrogen and Solar Upstream; Zimbabwe Green Hydrogen, Solar and Wind Upstream). A source covering every producer (USGS mine production) would remove the asymmetry. Open choice: growth is the absolute change P(2035) − P(2025), matching the variable's name and the fuel cells; the relative alternative P(2035)/P(2025) − 1 (0/0 read as 0) changes only the growth term.
* **EV Midstream covers the EI roster only.** The IEA's five EV regions cover every economy, but the theme copies them to the EI country list, so economies off it have no EV Midstream rows: EO scores them 0 under the `zero` rule (49 rankable EO cells, among them Malta, an EU member, and the coalition members Dominican Republic, Guatemala, Ghana, Costa Rica and Namibia) and ES gives them the global average. Mapping the non-EI economies to the regions and scoring them with `median_scurve_ref()` against the EI roster would leave every EI economy's value unchanged. Two further design points: ranks are taken over copied rows, so a region counts once per EI economy in it (Rest of World 57 copies, China 1), and the region map follows EI sub-regions (Mexico in North America; Hong Kong, Taiwan and New Caledonia in Other Asia Pacific), not verified against the IEA's own definitions.
* **ES clean-tech manufacturing keeps the IEA's "Electrolyzers" label.** `foreign_dependency_build_cleantech_midstream()` does not map electrolysers to Green Hydrogen as the EO builder does, so its 90 Electrolyzers rows (`Market Share`) never reach ES Green Hydrogen Midstream, which is scored on `Mineral Supply` alone. Pre-existing; mapping them would add an input to that ES cell.
* **`foreign_dependency_tbl` carries two theme-level overall rows per cell in places.** `apply_overall_definitions()` groups by Year, so EV Midstream has an `Overall Foreign Dependency Index` row for 2024 (EV terms) and one for 2025 (Mineral Supply), and Batteries, Solar and Wind Midstream one for 2035 and one for 2025. The pillars recompute the overall with Year dropped, so only readers of the theme table (the dashboard) see the duplicates.
* **Cost of capital: a lapsed top-priority series wins.** The rate selector takes the newest reading of the highest-priority series, so New Zealand reads its 2017 lending rate (4.76%, inside the 10-year floor) although its 2025 policy rate (3.0%) is current; Chile (2018) and Japan (2017) are similar. Where the top-priority reading is older than the floor the country is peer-imputed even if a lower-priority series is current (Croatia, Guinea, Malta, the Netherlands, Serbia, Sweden). Deposit rates standing in for borrowing costs set the panel's low end (Cambodia 1.25%, the CEMAC economies 2.45%); the WAEMU members' common lending rate (5.30%, 2017) sits below their deposit rate (6.60%); an annual average and a December month that share a date are separated by file order; and positive anomalies escape the ≤ 0 stub rule (Trinidad and Tobago 2020-M12 = 0.08, which reaches the 2020 vintage only).
* **EI edition and target year.** The EI themes label their source "EI Statistical Review of World Energy (<data year + 1> edition)" (`ei_stat_review_source()`, `R/utils/schema.R`), from the target year the builder reads. `scripts/10` calls the EI builders with their default target year (2025); when a 2027-edition file is staged the defaults must move, or the themes keep reading 2025 data under a 2026-edition label. `scripts/40` derives its year from the data; a vintage's label names the edition current in that vintage year although the values come from the newest file.
* **Small code to-dos found in the October 2026 fixes.** `energy_prices_imf_clean(include_optional_indices = TRUE)` errors in R ≥ 4.3 (`&&` with a vector operand; no caller passes TRUE). `R/charts/35_trade_bloc_counterfactual.R` reads the Energy Imports index (higher = more self-sufficient) as `fossil_import_dependence` without `inverse = TRUE`, so net exporters count as the most exposed in that standalone chart. Hardening: the EO mineral production weights are renormalised per country (equal to per-technology weights only because every roster country is completed with every mineral); the clean-tech share denominator would halve silently if the IEA extract gained a World or Total row; the EV net-export check uses an absolute tolerance of 1e-6 and the region map relies on EI's sub-region labels "North America" and "Asia Pacific".
* **The all-votes ideal points end in 2023** (two years behind the final-passage series), so recent voting shifts are not in the blocs; Japan, Ireland and New Zealand sit just above the U.S.-leaning cut.
* **The EU node's demand weight** is about 3.8% of coalition demand while the EU-27 is about a quarter of coalition GDP: demand is a rank S-curve over nodes, so the bundled EU is one point.
* **Policy index on IEA PAMS alone for countries CAT does not rate** (150 economies, 11 coalition members). Their policy term lacks CAT's technology-GHG tilt: for the countries with both, IEA PAMS alone and the composite agree at a median within-country Spearman of 0.48, and on the tables of 2026-10-09 six of the 45 members with CAT (AUS, BGR, CAN, CZE, FIN, PRT) would get another pick on IEA PAMS alone. `policy_basis` flags it; `OPSI_POLICY_REQUIRE_ALL=1` is the both-required variant. The fallback also brings thinner policy signals into the SI (192 scored countries on the tables of 2026-10-09, up from 65, micro-states among them): any cross-country SI or policy-level table should show the basis.
* **Vintages differ from the headline build in two ways:** `scripts/40` does not pass the rankability threshold (its `rankable` flags use tau = 1, sovereignty only) and does not pass the uranium production table, so EO Nuclear Upstream lacks its Production category in every vintage.
* **Supply-chain coupling** (`couple_pillar_scores_by_hhi.R`) is unused; delete it or wire it in deliberately.
* **Raw-input completeness is environment dependent.** Use the manifest workflow and `scripts/03_check_raw_inputs.R` to reconcile gaps before full builds.
* **`allies.csv` remains staged from SharePoint.** It only feeds the 22-reporter allied Comtrade file, which is now a fallback behind the all-reporter pull.
* **Remaining automation candidates**: `imf_dip.csv` (a full pull is ~6.5 MB per reporter, so it needs dimension filtering), `oecd_crs_api.csv` (the CRS dataflow is not exposed under the agency the staged file names), and the Atlas extracts via Harvard Dataverse.
* **IMF fetchers are `fallback`, not `prefer`.** The live PPI and MFS_IR flows have dropped countries the staged extracts still carry; switching would change index coverage.
* **EIA fill of EI production (since 2026-10-11): open ends.** (1) Generation EI does not report (Production Downstream, 68 of 99 economies) is still 0 although EIA has positive net generation for most of them; the same fill would apply to `electbyfuel_*`, `solar_twh`, `wind_twh` (EIA series `INTL.<product>-12-<iso3>-BKWH.A`). (2) 2025 gas and coal fills use EIA's 2024 value until EIA publishes 2025. (3) EI's coal consumption is 1.5-2.2 times EIA's for Finland, Ireland and Belarus (probably peat, which they mine), so their EIA zero coal production may understate a domestic share; only Estonia's oil shale is far enough apart to fail the comparability check. (4) The 20 producers EI lists without a consumption row (Angola, Nigeria, Libya, Guyana, ...) take EIA consumption (`eia_fill_consumption = TRUE`, adopted 2026-10-11). (5) The Sector Insights exporter (`sector_facts_production()`, `sector_facts_balance()`) still classifies by EI row presence: it ships EIA-filled balances as "not reported separately" and EIA-filled production as plain observations, with EI provenance. (The NA Estonia coal production fact ships as `nsr`, NaN.) (6) Ratio years, fixed 2026-10-11 for production: a 2025 gas or coal fill divided EIA's 2024 production by EI's 2025 consumption (Slovenia coal 121% self-sufficient and a net exporter, because EI's consumption fell 39% in 2025; New Zealand gas 122%). Energy Imports now sets that production against EI's 2024 consumption and carries the 2024 self-sufficiency to 2025 consumption (Slovenia coal 73.5%, New Zealand gas 109.5%; ES "2) Energy Imports"). What remains is the lag itself: those 2025 cells are 2024 self-sufficiencies, which assumes production followed consumption in 2025 (apt for mine-mouth lignite and a closed gas market such as New Zealand's; for an importer with a small domestic output it scales that output with imports-driven demand, e.g. Bangladesh coal 4.3% → 5.6%). Open, for a user decision: the producers of (4) keep EI's 2025 production over EIA's 2024 consumption (Guyana oil 2,675%); pairing them on 2024 as well (one condition in `import_dependence_pair_years()`) would replace their observed 2025 production with 2024's (Yemen oil 22% → 51%, South Sudan 1,116% → 728%, Bolivia gas 181% → 213%; ES up to 0.016, picks unchanged). In Production (EO) a 2025 gas or coal cell from EIA (29 gas, 12 coal with positive production) has EIA's 2024 level, so its `size` is 2024's and its 5-year change runs 2020-2024; the theme has no production-to-consumption ratio and is unchanged. EIA's calorific basis also differs from EI's by country (EI/EIA coal production: Germany 0.64, Turkiye 1.12).

---

## Changelog (fix/allied-base-paper, Sep-Oct 2026)

Corrections:

- **Test and CI paths** resolve from the test file; 35 code/test mismatches this unmasked are fixed (typed empty overall tables, ISO-date year parsing, duplicated IMF parser, grouped-tibble leak in Friendshore, `mean(a:b)` in the outbound index, Shiny filter masking).
- **Variable-contribution weighting** used an inflated denominator; contributions now reconcile to the index.
- **Available-case pillars:** a missing category no longer counts as 0; **observed-only imputation:** fills apply only where the source observes someone in the group; the imputed flag (which was FALSE for any multi-row group) and the guard for unproduced score variables now work. Energy Prices, silently unscored, is scored.
- **Country harmonisation:** one name per economy (about 270 strings → 198 economies); aggregates and residual rows dropped; Comtrade vintages need ≥ 50 reporters.
- **Theme-builder fixes:** mineral-name harmonisation (rare earths, graphite), refining mapped to Midstream (the recode mapped everything to Upstream), zero supply for non-suppliers, reserves roll-up zeros, BNEF wind recode and within-technology growth ranks, Minerals Trade scores market share and HHI, GCIM minerals investment split by demand, electrolyser capacity to Green Hydrogen Midstream.
- **Trade themes split:** the ES and EO tables were identical and the ES deficit term was never computed; ES now scores HHI, market share and deficit/GDP, EO market share, RCA, export size, feasibility and HHI; every index is ranked across economies within the cell; market share is the basket share.
- **Comtrade energy-trade file completed** (2026-10-09): `comtrade_energy_trade.csv` is rebuilt from the World rows of the all-reporter dyad pull (every basket code with 2024 trade, 142 exporting and 143 importing reporters, Taiwan included; it lacked 60 basket codes, among them 19 of 20 Semiconductors Midstream codes, and 13 reporters) and `comtrade_total_export.csv` refreshed from the same release.
- **Upstream trade baskets** (2026-10-11): Electric Vehicles, Solar, Wind, Nuclear and Semiconductors Upstream get HS6 baskets by sub-component (battery minerals and rare earths; silicon metal; rare earths; uranium ores and natural uranium compounds; gallium, germanium and indium), feeding the ES and EO Trade categories of the headline cells. Rare-earth compounds and metals, natural uranium compounds and the unwrought minor metals moved from the Midstream baskets of the same technologies (Nuclear Midstream: Comtrade 2024 exports $28.3bn -> $19.2bn). The four new codes were pulled as a supplementary dyad pull (14 calls) and added to the energy-trade file.
- **Critical-mineral reserves from USGS MCS 2026** (user decision 2026-10-09): the nine mineral reserve variables, their technology roll-ups and the coalition's mineral reserves measure (`usgs_reserves`) read the USGS data release (ver. 1.3 erratum applied); EI keeps oil, gas and coal, the Red Book uranium. Holders 76 -> 92, Brazil's manganese and nickel now recorded, copper holders 7 -> 14.
- **Config wiring:** Energy Prices variable, Minerals Trade components, EO Trade without the deficit, Technology Demand and Cost Competitiveness counting each source once, EO Reserves weight removed.
- **Policy index:** IEA PAMS was scored with NIPO's jurisdiction weights, substring stage rules tagged 37% of policies Upstream, Korea and the UAE were dropped by name mismatches, and the NIPO block had not run since February 2026.
- **Strategic Index:** ES enters as need (1 − ES) explicitly; missing pillars are excluded, not filled with country means; degenerate sectors are excluded.
- **Partnership tables:** Friendshore was ~2× duplicated; Development is partner-level (it was copied per reporter and donor); PSI is available-case (high-income partners were capped at 0.8); headline technologies only; `partner_es_need` renamed; mis-oriented WDI series fixed.
- **Allied network:** a join fan-out (606,866 rows for 744) corrupted the outputs; edges are oriented producer → consumer; caps and greedy count distinct countries; ARE code fixed.
- **GHG weight:** oil lifecycle median corrected to 840 gCO2eq/kWh (SRREN), which had been a 25th percentile.
- **Theme fixes found while cataloguing the Sector Insights data (2026-10-10; index effects measured at function level on the 2026-10-09 inputs, before the rerun):**
  - *EO mineral Upstream production* (Batteries, Electric Grid, EV, Green Hydrogen, Solar, Wind): the level is the demand-weighted share of world mined supply, growth its absolute change to 2035 against base-year world supply, and the rows are labelled IEA (they were an unweighted mean of kilotonnes, ranked per mineral row, with relative growth undefined for 92 listed non-producer cells, and labelled EI). EO moves only in those six cells: by up to 0.067 for the 29 IEA-listed economies (within-cell Spearman 0.955–0.992), by less than 0.001 elsewhere; nine cells become rankable.
  - *EV Midstream regions*: a placeholder in the region join had dropped the eight non-EU European EI economies (GBR, NOR, TUR, UKR, CHE, ISL, SRB, MKD) from EV Midstream; they now take Rest of World's values. ES EV Midstream: those eight −0.039 and six of them become rankable (99 → 105); EO: those eight +0.086 and Switzerland becomes rankable; other economies move by at most 0.005 (ES) and 0.003 (EO); the top 10 is unchanged in both.
  - *Names that inverted their meaning*: the EV term `import` is `net_export` ((production − sales) / sales; `config/index_definition.yml` updated), and the EI `Import share` is `Self-sufficiency` (production / consumption × 100). The orientation was already right in both; the self-sufficiency form also makes non-producers tie exactly, which moves ES Energy Imports for a few economies the rounding noise had split (Spain Oil +0.054 in ES, Switzerland and Luxembourg Gas +0.039, Netherlands and Saudi Arabia Coal −0.022, ...).
  - *Cost of capital*: zero lending-rate stubs and the IMF averages built on them are dropped (New Zealand 0.0 → 4.76%, Solomon Islands 5.18 → 10.35%). EO: New Zealand −0.010 and Solomon Islands −0.023 on average over their cells; 76 other economies by at most 0.00052 (the global-average fill of their cost category moves).
  - *No index effect*: clean-tech manufacturing shares over the IEA world total including "Other" (China's 2035 solar share 0.90 → 0.80); WEO demand terms renamed `global_*` (756 duplicate keys); IMF price units from the series metadata (cobalt, propane, lithium, vanadium, chromium, rare earths, gas and uranium were mislabelled or unlabelled); `Heat.Pumps` → `Heat Pumps`; EI source labels name the edition read ("(2026 edition)"; they read "(2025)", the data year).
- **EI fuel production not reported separately comes from the EIA** (user decisions 2026-10-11; measured at function level against the 2026-10-10 build): Energy Imports and Production read an absent EI production row (an economy EI counts in an "Other <region>" residual) as 0 production. Those cells now take the EIA International Energy Statistics value of the same year in EI's units (gas and coal: EIA's 2024, its latest), as observed data with EIA named in `source`; an EIA zero is a real zero; EI stays the source where it reports. Estonia coal is the one exception (EI counts oil shale as coal; EIA's series do not) and is NA. *Energy Imports*: of the 128 gap cells (43 oil, 37 gas, 48 coal), 58 get a positive value, 69 an EIA zero, Estonia coal none. ES: 605 Oil, Gas and Coal Upstream cells move, no other cell. Cells with a positive EIA value rise (coal +0.034, gas +0.010, oil +0.009 on average; Peru Coal +0.084, Slovenia Coal +0.071, New Zealand Gas +0.064, Turkiye Oil +0.018); EIA zeros fall slightly (−0.002 to −0.021: they now rank below the small producers they tied with); reporting economies move 0 to −0.009 and off-roster economies by at most 0.0011. Rankable headline cells 4,126 → 4,125 (Estonia Coal Upstream is now imputed); 140 ranks move, the Upstream top 10s do not. *Production (EO only)*: of the 2025 gap cells (49 oil, 50 gas, 65 coal) 72 get a positive value (Bahrain and Turkiye oil, Angola gas, Philippines coal, ...), 91 a zero, Estonia coal NA; earlier years likewise. EO: 632 Oil, Gas and Coal Upstream cells move (−0.040 to +0.042; Production category: Peru Coal +0.25, Slovak Republic Coal −0.24 and South Korea Gas −0.22 from production that ended after 2020), rankability unchanged (4,307). EO Upstream top 10s: Philippines enters Coal at 5 (Thailand leaves); Kuwait/UAE and India/Malaysia swap. *Strategic Index*: 3,109 → 3,108 cells (Estonia Coal Upstream leaves), 192 countries; 264 cells move by at most 0.026; all 57 allied picks unchanged. An intermediate version of the same day read the absent rows as NA (global-average fill, imputed), which made 122 of these cells unrankable (ES 4,004 rankable; SI 2,987 cells) and moved Morocco's allied pick; the EIA fill restores all but Estonia coal. Generation EI does not report (Downstream) is still 0.
- **Energy Imports sets EIA production of an earlier year against consumption of that year** (2026-10-11; measured at function level against the EIA fill as scripts/10 calls it, consumption fill included, on the 2026-10-10 inputs): where production is an EIA value of an earlier year than consumption (2025 gas and coal: EIA 2024), self-sufficiency is that year's and the balance applies it to the cell's consumption (ES "2) Energy Imports"). The two years used to be divided as they were. Theme: 32 cells pair (22 gas, 10 coal; Slovenia coal 121% → 73.5%, a net exporter → importer; New Zealand gas 122% → 109.5%; Ecuador gas 57% → 45%); 29 Fossil self-sufficiencies move; 254 of 1,584 rows change value; 2020 (scripts/40) is unchanged. Oil, and the producers EI lists without consumption (EI 2025 production over EIA 2024 consumption), are unchanged (README EIA open end (6)). ES: 179 Coal and Gas Upstream cells move: 10 paired cells, 50 re-ranked (by at most 0.0026) and 119 coal cells off the roster through the pillar's global-average fill (by at most 0.00013); largest: Slovenia Coal −0.044 (rank 37 → 51 of 96), New Zealand Gas −0.003. Rankable cells unchanged (4,161); Oil, Gas and Coal Upstream top 10s unchanged. Strategic Index: 3,144 cells, 192 countries; 59 cells move by at most 0.013 (Slovenia Coal Upstream 0.358 → 0.371); all 57 allied picks unchanged. EO is unchanged (the production theme has no production-to-consumption ratio).

Extensions:

- **Rankability rule** (sovereign and imputed weight share ≤ 0.5) for ES, EO, the Strategic Index and the PSI.
- **Uranium** (Red Book resources and mine production) for Nuclear Upstream in ES and EO.
- **TRL stage mapping** with the post-TRL-9 adoption stage read onto the 1–11 scale.
- **HS6 basket review** (dominant-use rule, `excluded` column, legacy bridge codes, caveat notes); Electric Grid Upstream's copper and aluminium basket, with a supplementary dyad pull of its new codes. Switching and routing apparatus (8517.62) left Electric Grid Midstream (telecommunications equipment, 28% of the cell's 2024 Comtrade exports).
- **Mirror critical-minerals exports** for non-reporting economies, with late reporters.
- **Policy index = IEA PAMS + CAT** (CAT where it rates the country; IEA PAMS alone otherwise, flagged `policy_basis`); NIPO excluded for now; dual-use dropped.
- **Strategic Index endowment screen** for the allied picks (Upstream only where the country has a recorded resource base).
- **Geopolitical alignment** from all-votes UNGA ideal points, quartile hard blocs, territory map; `geo_alignment` enters Friendshore.
- **Two-tier coalition** (core / partner by agreement or supplier route, the supplier route counting flows at 5% and reserves at 10%) with the EU as one network node.
- **Bilateral indices:** all-reporter Comtrade dyads with Taiwan, importer-first / mirror × 1.06 rule, observed zeros, conduit financial centres excluded from outbound FDI, core-coalition (headline), full-coalition and world PSI views, PSI rankability.
- **CM-FDI in Friendshore** (user decision 2026-10-11; measured at function level on the 2026-10-10 final-run data): the Cleantech Manufacturing FDI Dataset (Net Zero Industrial Policy Lab, edition 2026.10.05) gives the investment tie in Solar, Wind, Batteries and EV Midstream in place of the IMF position, and a new Friendshore risk component, `chinese_parent_exposure` (weight 1, scored 1 − share), the parent-company case of the common-mode risk of the AIB concept paper (section 7.4). 905 of the 1,042 projects are used (manufacturing; announced, under construction, completed or operational; all years). Outside the four cells every exported value is unchanged; coalition PSI Spearman 0.9998 (max rank change 5), world 0.9996 (max 8); Economic Opportunity, Opportunity and Development unchanged. Sector Insights shows the projects descriptively (flows, parent-HQ groups, years, the project table with source links). See [Partnership Strength Index](#partnership-strength-index-psi).
- **Allied network:** Upstream and Midstream only, diversification term with a calibration rule, perspective tangent cuts, strict top-K caps, node policy term off, EU node aggregation, MILP for every stage (≤ 60 nodes, 300 s, 30 cap passes), observed-HHI output and warning.
- **Vertical complementarities tested, not adopted** (2026-10-12): whether a stage's EO should reward capability in the adjacent stages and its ES the security of its inputs. In an Atlas panel (1995-2023, 172 economies, country-year and technology-year fixed effects) the linkages are cross-sectional co-location, concentrated in fossil fuels and nuclear, about zero for clean-tech chains and within country-technology cells, with no out-of-sample gain; sourcing-weighted input security did not predict the 2021-22 shortfalls (sourcing concentration did). Reported in the paper (methods and an appendix); no index change.

---

## Citation

If you use OSI outputs in external work, cite:

* This repository (commit hash / release tag)
* `docs/methodology.md`
* `docs/sources.md`
* The exact configuration files used (`config/*.yml`)

And record:

* raw input folder state/date
* processing date
* any manual overrides (missing-data rules, weights, rankability threshold, lambda)

---

## License

This project is licensed under the terms in `LICENSE`.

For contribution expectations and responsible disclosure guidance, see `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, and `SECURITY.md`.

---

<!-- Link reference definitions -->

[IEA-CM]: https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset
[IEA-CTG]: https://www.iea.org/data-and-statistics/data-tools/etp-clean-energy-technology-guide
[IEA-EV]: https://www.iea.org/data-and-statistics/data-product/global-ev-outlook-2026
[IEA-PAMS]: https://www.iea.org/policies
[CAT]: https://climateactiontracker.org/countries/
[EI-SR]: https://www.energyinst.org/statistical-review
[EIA-IES]: https://www.eia.gov/international/data/world
[USGS-MCS]: https://doi.org/10.3133/mcs2026
[REDBOOK]: https://www.oecd-nea.org/jcms/pl_121658/uranium-2026-resources-production-and-demand
[COMTRADE]: https://comtrade.un.org/
[AEC]: https://atlas.hks.harvard.edu/
[UNGA-IP]: https://doi.org/10.7910/DVN/LEJUQZ
[WDI]: https://databank.worldbank.org/source/world-development-indicators
[GSA]: https://globalsolaratlas.info
[GWA]: https://globalwindatlas.info/
[BNEF-NEO]: https://about.bnef.com/insights/clean-energy/new-energy-outlook/
[CM-FDI]: https://cmfdi.netzeropolicylab.com
[IMF-COMM]: https://www.imf.org/en/research/commodity-prices
[ILO]: https://ilostat.ilo.org/data/
[IMF-DEX]: https://data.imf.org/en/Data-Explorer
