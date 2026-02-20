# Opportunity Security Indices (OSI)

This repository contains the scaffold for the **Opportunity Security Indices** pipeline. It produces three related pillars:

- **Energy Security (ES)** — resilience and security across energy supply chains.
- **Economic Opportunity (EO)** — opportunity for growth and competitiveness in energy-related markets.
- **Partnership Strength Index (PSI)** — a composite partnership measure blending friendshoring, opportunity, and development potential.

> **Unit of analysis:** **Country × Technology × Supply-chain stage**  
> Stages are standardized to `Upstream`, `Midstream`, `Downstream`.

---

## Table of contents

- [What this repo produces](#what-this-repo-produces)
- [Data model](#data-model)
- [How the indices are built](#how-the-indices-are-built)
  - [Normalization: `median_scurve`](#normalization-median_scurve)
  - [Overall variables and category scoring](#overall-variables-and-category-scoring)
  - [Pillar aggregation and weights](#pillar-aggregation-and-weights)
  - [Missing data policy](#missing-data-policy)
  - [Supply-chain coupling](#supply-chain-coupling)
- [Pillars, categories, and technical appendix](#pillars-categories-and-technical-appendix)
  - [Energy Security (ES)](#energy-security-es)
  - [Economic Opportunity (EO)](#economic-opportunity-eo)
  - [Partnership Strength Index (PSI)](#partnership-strength-index-psi)
- [Repository layout](#repository-layout)
- [Quick start](#quick-start)
- [Interactive explorer (Shiny)](#interactive-explorer-shiny)
- [Outputs](#outputs)
- [Testing and quality checks](#testing-and-quality-checks)
- [Additional utilities and workflows](#additional-utilities-and-workflows)
- [Data sources](#data-sources)
- [Known issues and to-dos](#known-issues-and-to-dos)
- [Citation](#citation)
- [License](#license)

---

## What this repo produces

The pipeline produces:

1. **Theme tables** (raw metrics + normalized indices) at the Country × Tech × Supply-chain level.
2. **Category scores** and **pillar indices** for ES and EO using configured weights.
3. **PSI** components and a composite PSI index.
4. Optional **contribution tables** (category and variable contributions) for auditability.

---

## Data model

Theme outputs are stored in a tidy format with common columns, including:

- `Country`
- `tech`
- `supply_chain`
- `category`
- `variable`
- `data_type` (`raw` vs `index`; some themes also emit `weight` and `contribution` for decomposition)
- `value`
- `Year`
- `source`
- `explanation`

A schema validator and standardization helpers live under `R/utils/`.

---

## How the indices are built

### Normalization: `median_scurve`

Most raw metrics are converted to a 0–1 index using a median-centered S-curve:

```

idx = r^gamma / (r^gamma + (1 - r)^gamma)

```

where `r` is the percentile rank and `gamma = 0.5` by default.

This compresses values near the median while preserving separation at the tails.

### Overall variables and category scoring

This repo is **config-driven**:

- **Overall ... Index** formulas are defined in `config/index_definition.yml` as simple compositions (typically means) over component index variables.
- Each **category** has a **score variable** (e.g., ES Trade uses `Overall Trade Risk Index`, EO Trade uses `Overall Trade Index`) defined in the same config file.

This keeps scoring transparent and avoids hard-coded assumptions in code.

### Pillar aggregation and weights

ES and EO are computed as weighted means of their category scores **within each Country × Tech × Supply-chain group**.

Weights are defined in `config/weights.yml` (separately for ES, EO, and PSI).

### Missing data policy

Missing-data behavior is configured in `config/missing_data.yml` and applied at the theme level. Common patterns include:

- `zero` — treat missing as zero for that variable (used when absence is meaningful or conservative).
- `global_average` — impute to the global mean for that tech/supply_chain/variable/theme group (used when missingness is likely data availability rather than true absence).

### Supply-chain coupling

There is an optional “coupled” pillar score that shrinks stage-level pillar scores toward a tech-level **chain score** (geometric mean across stages), using interdependence edges data.

- Implementation: `R/indices/couple_pillar_scores_by_hhi.R`  
- Orchestration hook: `scripts/20_build_indices.R` loads `interdependence_edges_primary_secondary_tertiary.csv` and applies coupling.

**Important note:** the methodology doc describes an HHI-driven logistic mapping for coupling strength, but the current implementation uses **interdependence edge strength** (normalized) and a **linear mapping** to lambda in `[lambda_min, lambda_max]`. If you want the implementation to match the doc exactly, this should be reconciled.

---

## Pillars, categories, and technical appendix

This section is written to function as a **drop-in technical appendix inside the README**. For publication, you can move it into `docs/technical_appendix.md` and link it here.

### Energy Security (ES)

**Interpretation:** ES measures exposure and resilience across upstream resources, midstream manufacturing, and downstream deployment/consumption.

**Configured ES categories (see `config/index_definition.yml`):**
- Foreign Dependency
- Energy Imports
- Reserves
- Trade
- Minerals Trade
- Production
- Energy Access
- Consumption
- Energy Prices

<details>
<summary><strong>ES — Category-by-category construction notes (with cited sources)</strong></summary>

#### 1) Foreign Dependency
**What it captures:** exposure to foreign control of critical upstream inputs (minerals) and midstream manufacturing capacity (clean tech / EV supply).

**Overall variable construction (configured):** the category score uses `Overall Foreign Dependency Index`, calculated as the **mean** of: `Mineral Supply`, `Market Share`, `Overall Market Share`, `production`, `sales`, `import`, `market_share`, and `Overall Production`.

**Data sources:**
- **IEA Critical Minerals Dataset** ([IEA-CM]) — mineral supply series used to compute market share + concentration proxies
- **IEA Global EV Data Explorer** ([IEA-EV]) — EV market/production/import share inputs (as used in repo exports)
- **Energy Institute Statistical Review** ([EI-SR]) — country harmonization + EU mapping used for rollups/expansion (and associated energy stats)

*(Note: some midstream capacity snapshots are sourced from IEA ETP exports; see `docs/sources.md` for provenance when the public landing page differs from the internal extract.)*

**Substantive rationale:** Foreign dependency is a first-order vulnerability channel: when upstream/midstream capacity is externally concentrated, disruptions or policy restrictions transmit quickly into domestic shortages and price shocks.

**Implementation entry points:**
- Theme code: `R/categories/foreign_dependency/foreign_dependency.R`
- Midstream share scaffolding: `R/categories/foreign_dependency/market_share_manufacturing.R`

---

#### 2) Energy Imports
**What it captures:** reliance on external suppliers for fuels (oil/gas/coal), proxied by production-consumption balance.

**Overall variable construction (configured):** the category score uses `Overall Energy Imports Index`, calculated as the **mean** of: `Production surplus/deficit`.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR])

**Substantive rationale:** import dependence is the classic energy-security exposure: it increases sensitivity to foreign supply disruptions and global price volatility.

**Implementation entry point:**
- `R/categories/energy_imports/import_dependence.R`

---

#### 3) Reserves
**What it captures:** domestic reserves depth (fossil + minerals) and demand-weighted “technology reserves” for minerals.

**Overall variable construction (configured):** the category score uses `Overall Reserves Index`, calculated as the **mean** of reserve and potential components: `Oil Reserves`, `Gas Reserves`, `Coal Reserves`, `Cobalt Reserves`, `Lithium Reserves`, `Graphite Reserves`, `Rare Earths Reserves`, `Copper Reserves`, `Manganese Reserves`, `Nickel Reserves`, `Zinc Reserves`, `PGMs Reserves`, `Solar Reserves`, `Wind Reserves`, `Electric Vehicles Reserves`, `Batteries Reserves`, `Electric Grid Reserves`, `Green Hydrogen Reserves`, `Heat Pumps Reserves`, `Nuclear Reserves`, `Hydroelectric Power Reserves`, `Geothermal Reserves`, `Overall Solar PV Potential Index`, `Overall Wind Potential Index`, and `Overall Geothermal Potential Index`.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR]) — fossil and mineral reserves tables used in the repo
- **IEA Critical Minerals Dataset** ([IEA-CM]) — cleantech demand-by-tech shares used to roll mineral reserves into tech-weighted reserve indices

**Substantive rationale:** reserves proxy long-run domestic supply optionality and reduce risk of external supply squeeze.

**Implementation entry points:**
- `R/categories/reserves/reserves.R`
- `R/categories/energy_access/solar_pv_potential.R`
- `R/categories/energy_access/wind_potential.R`
- `R/categories/energy_access/geothermal_potential.R`

---

#### 4) Trade (risk)
**What it captures:** concentrated exposure and/or weak strategic positioning in relevant traded energy-tech products.

**Overall variable construction (configured):** the category score uses `Overall Trade Risk Index`, calculated as the **mean** of: `HHI`, `market_share`, and `deficit_gdp`.

**Data sources:**
- **UN Comtrade** ([COMTRADE]) — exports/imports; RCA inputs (as used in repo exports)
- **Harvard Atlas of Economic Complexity** ([AEC]) — distance→feasibility and market-share fields (as used in repo exports)
- **World Development Indicators (WDI)** ([WDI]) — GDP for deficit-to-GDP scaling
- **Project HS mapping** — maps HS6 → tech/supply_chain/sub-sector (repo file)

**Substantive rationale:** concentrated trade relationships create single points of failure; persistent deficits indicate structural dependence.

**Implementation entry points:**
- Core logic: `R/categories/trade/trade_core.R`
- Wrappers: `R/categories/trade/trade_concentration.R`, `R/categories/trade/export_feasibility.R`

---

#### 5) Minerals Trade
**What it captures:** critical minerals trade positioning and concentration, rolled up into technology exposure via demand weights.

**Overall variable construction (configured):** the category score uses `Overall Minerals Trade Index`, calculated as the **mean** of: `critmin_trade`.

**Data sources:**
- **UN Comtrade** ([COMTRADE]) — critical-minerals import/export/totals (as used in repo exports)
- **IEA Critical Minerals Dataset** ([IEA-CM]) — mineral list + demand-by-tech shares used to roll minerals into tech exposure

**Substantive rationale:** critical minerals are binding inputs in the electro-industrial stack; trade structure determines vulnerability to bottlenecks and bargaining power.

**Implementation entry point:**
- `R/categories/minerals_trade/critical_minerals_trade.R`

---

#### 6) Production
**What it captures:** depth and momentum of domestic production (energy production and generation proxies; plus critical minerals supply/production proxies).

**Overall variable construction (configured):** the category score uses `Overall Production Index`, calculated as the **mean** of: `size`, `growth_abs`, `Overall Production`, `Cobalt Production`, `Lithium Production`, `Graphite Production`, `Rare Earths Production`, `Copper Production`, `Manganese Production`, `Nickel Production`, `Zinc Production`, `PGMs Production`, `Solar Production`, `Wind Production`, `Electric Vehicles Production`, `Batteries Production`, `Electric Grid Production`, `Green Hydrogen Production`, `Heat Pumps Production`, `Coal Production`, `Nuclear Production`, `Oil Production`, `Gas Production`, `Hydroelectric Power Production`, and `Geothermal Production`.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR]) — fossil production and generation series
- **IEA Critical Minerals Dataset** ([IEA-CM]) — mineral supply series used for mineral-side production scaling and concentration proxies

**Substantive rationale:** deeper domestic production provides redundancy and shock-absorption capacity; it also affects surge capability.

**Implementation entry point:**
- `R/categories/production/production_depth_momentum.R`

---

#### 7) Energy Access
**What it captures:** downstream access/enabling conditions via per-capita consumption (renewable resource potential is grouped under Reserves).

**Overall variable construction (configured):** the category score uses `Overall Energy Access Index`, calculated as the **mean** of: `Energy consumption per capita` and `Energy consumption per capita growth`.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR]) — population + per-capita consumption metrics

**Substantive rationale:** reliable energy access and domestic renewable potential reduce dependence on imported fuels and enable industrial scaling.

**Implementation entry points:**
- `R/categories/energy_access/energy_access_consumption.R`

---

#### 8) Consumption
**What it captures:** installed base / consumption-side scale and growth (e.g., per-capita installed capacity and projected growth).

**Overall variable construction (configured):** the category score uses `Overall Consumption Index`, calculated as the **mean** of: `Energy consumption per capita`, `Energy consumption per capita growth`, `installed_cap_index`, and `elec_growth_index`.

**Data sources:**
- **BloombergNEF New Energy Outlook** ([BNEF-NEO]) — installed capacity and projections used for per-capita and growth indices (as used in repo extracts)

**Substantive rationale:** consumption indicates exposure scale (how much must be secured) and infrastructure intensity (how demanding the system is).

**Implementation entry point:**
- `R/categories/consumption/energy_consumption.R`

---

#### 9) Energy Prices
**What it captures:** commodity input price volatility (annualized volatility of monthly log returns), oriented so lower volatility scores higher.

**Overall variable construction (configured):** the category score uses `Overall Energy Prices Index`, calculated as the **mean** of: `price_volatility`.

**Data sources:**
- **IMF Commodity Prices** ([IMF-COMM]) — monthly commodity price series used to compute volatility indices (as used in repo extracts)

**PCPS coverage notes:**
- Annualized volatility is computed from monthly **log returns** of IMF PCPS commodity series.
- Coverage includes fossil fuels + uranium and energy-transition minerals available in snapshot data (e.g., lithium, nickel, cobalt, copper, vanadium, iron ore, and diammonium phosphate).
- Limitations: key inputs absent from PCPS snapshots (e.g., graphite and soda ash/sodium carbonate) are not captured unless integrated from another source.

**Substantive rationale:** volatility is a strong proxy for macro supply instability and economic exposure to shocks.

**Implementation entry point:**
- `R/categories/energy_prices/energy_prices.R`

</details>

---

### Economic Opportunity (EO)

**Interpretation:** EO measures expected market opportunity and competitiveness — demand growth, deployability, cost position, and industrial capability.

**Configured EO categories (see `config/index_definition.yml`):**
- Trade
- Production
- Technology Demand
- Technological Readiness
- Energy Prices
- Investment *(see “Known issues” — currently configured as a placeholder)*
- Energy Access
- Foreign Dependency
- Cost Competitiveness
- Consumption

<details>
<summary><strong>EO — Category-by-category construction notes (with cited sources)</strong></summary>

#### 1) Trade (opportunity)
**What it captures:** export competitiveness and feasibility (e.g., RCA, feasibility, market shares), rather than import vulnerability.

**Overall variable construction (configured):** the category score uses `Overall Trade Index`, calculated as the **mean** of: `market_share`, `rca`, `export_size`, `feas`, `deficit_gdp`, and `HHI`.

**Data sources:**
- **UN Comtrade** ([COMTRADE])
- **Harvard Atlas of Economic Complexity** ([AEC])
- **World Development Indicators (WDI)** ([WDI])

**Rationale:** trade competitiveness is a growth channel: countries positioned to export intermediate/final goods capture value and learning effects.

**Implementation entry points:**
- Core logic: `R/categories/trade/trade_core.R`
- Wrappers: `R/categories/trade/trade_concentration.R`, `R/categories/trade/export_feasibility.R`

---

#### 2) Production
**What it captures:** productive capability and momentum — similar underlying measures as ES, but interpreted as growth potential.

**Overall variable construction (configured):** the category score uses `Overall Production Index`, calculated as the **mean** of: `size`, `growth_abs`, `Overall Production`, `Cobalt Production`, `Lithium Production`, `Graphite Production`, `Rare Earths Production`, `Copper Production`, `Manganese Production`, `Nickel Production`, `Zinc Production`, `PGMs Production`, `Solar Production`, `Wind Production`, `Electric Vehicles Production`, `Batteries Production`, `Electric Grid Production`, `Green Hydrogen Production`, `Heat Pumps Production`, `Coal Production`, `Nuclear Production`, `Oil Production`, `Gas Production`, `Hydroelectric Power Production`, and `Geothermal Production`.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR])
- **IEA Critical Minerals Dataset** ([IEA-CM])

**Rationale:** deeper production ecosystems tend to scale faster and attract investment; production is both a capability and a market signal.

**Implementation entry point:**
- `R/categories/production/production_depth_momentum.R`

---

#### 3) Technology Demand
**What it captures:** forward demand and growth (global and/or country-level), plus midstream overcapacity penalties as a margin-risk proxy.

**Overall variable construction (configured):** the category score uses `Overall Technology Demand Index`, calculated as the **mean** of: `demand_size`, `demand_growth`, `Overall Global Demand`, `Overall Demand`, `sales_growth`, `sales_size`, `sales_forecast_growth`, `sales_forecast_size`, `stock_growth`, `stock_size`, `stock_forecast_growth`, `stock_forecast_size`, `share_growth`, `share_size`, `share_forecast_growth`, `share_forecast_size`, `Overall Addressable Market`, and `Overall Overcapacity`.

**Data sources (cited where applicable):**
- **BloombergNEF New Energy Outlook** ([BNEF-NEO]) — region/country demand levels and growth (as used in repo extracts)
- **IEA Global EV Data Explorer** ([IEA-EV]) — EV stock/sales/share and growth inputs (as used in repo extracts)

*(Note: the repo also uses IEA WEO annex exports and BCG market sizing sheets; these are referenced in `docs/sources.md` where public landing pages differ from the internal extracts.)*

**Rationale:** opportunity follows demand — but segments with persistent overcapacity may be structurally low-margin even when volumes rise.

**Implementation entry points:**
- `R/categories/technology_demand/future_demand.R`
- `R/categories/technology_demand/overcapacity_premium.R`

---

#### 4) Technological Readiness
**What it captures:** technology maturity proxy (TRL), normalized to 0–1.

**Overall variable construction (configured):** the category score uses `Overall Technological Readiness Index`, calculated as the **mean** of: `TRL Index`.

**Data sources:**
- **IEA ETP Clean Energy Technology Guide** ([IEA-CTG]) — TRL and technology classification inputs (as used in repo extracts)

**Rationale:** readiness is strongest in a "Goldilocks" band (not too nascent, not fully mature), and technologies with improving TRL trajectories can represent rising opportunity.

**Method notes:**
- Mapping from IEA taxonomy to target technologies is token-based and config-driven via `config/iea_clean_tech_guide_tech_map.yml` (using taxonomy + supply-chain signals rather than brittle fixed sector positions).
- The TRL theme now blends:
  - **TRL Level Index**: Goldilocks bell-curve score on end-year TRL.
  - **TRL Momentum Index**: scaled positive TRL change from 2020 to 2023.
  - **TRL Index**: weighted combination of level and momentum.
- Fossil fuel technologies (**Coal**, **Oil**, **Gas**) are explicitly supported through taxonomy rules.

**Implementation entry point:**
- `R/categories/technological_readiness/technological_readiness.R`

---

#### 5) Cost Competitiveness
**What it captures:** deployment/manufacturing cost position, including relative technology costs, LCOE competitiveness, and composite “input cost” competitiveness (labor + capital proxies).

**Overall variable construction (configured):** the category score uses `Overall Input Cost Index`, calculated as the **mean** of: `lcoe_24`, `lcoe_50`, `Overall input cost index`, `Input Cost Index`, and `IEA Cost index`.

**Data sources (cited where applicable):**
- **IMF Data Explorer** ([IMF-DEX]) — lending rates and PPI series used as capital/input-cost proxies (as used in repo extracts)
- **ILOSTAT** ([ILO]) — earnings by economic activity used as labor cost proxy (pulled via API in the pipeline)
- **BloombergNEF New Energy Outlook** ([BNEF-NEO]) — used in some downstream competitiveness scaffolding (as used in repo extracts)

*(Note: the repo also uses BNEF LCOE and IEA relative cost exports; see `docs/sources.md` for provenance and internal snapshot naming.)*

**Rationale:** opportunity depends not just on demand, but on **bankable cost competitiveness** — production + deployment happen where costs clear.

**Implementation entry points:**
- `R/categories/energy_prices/lcoe_competitiveness.R`
- `R/categories/economic opportunity/cost_competitiveness.R`

---

#### 6) Energy Prices
**What it captures:** macro input price volatility, oriented so lower volatility scores higher.

**Overall variable construction (configured):** the category score uses `Overall Energy Prices Index`, calculated as the **mean** of: `price_volatility`.

**Data sources:**
- **IMF Commodity Prices** ([IMF-COMM])

**PCPS coverage notes:**
- Annualized volatility is computed from monthly **log returns** of IMF PCPS commodity series.
- Coverage includes fossil fuels + uranium and energy-transition minerals available in snapshot data (e.g., lithium, nickel, cobalt, copper, vanadium, iron ore, and diammonium phosphate).
- Limitations: key inputs absent from PCPS snapshots (e.g., graphite and soda ash/sodium carbonate) are not captured unless integrated from another source.

**Rationale:** stable price environments improve bankability and reduce risk premiums for investment.

**Implementation entry point:**
- `R/categories/energy_prices/energy_prices.R`

---

#### 7) Energy Access
**What it captures:** enabling conditions via per-capita consumption.

**Overall variable construction (configured):** the category score uses `Overall Energy Access Index`, calculated as the **mean** of: `Energy consumption per capita` and `Energy consumption per capita growth`.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR])

**Implementation entry point:**
- `R/categories/energy_access/energy_access_consumption.R`

---

#### 8) Foreign Dependency
**What it captures:** in EO, these variables can reflect constraint *or* positioning (e.g., high midstream market share can be “opportunity” even if framed elsewhere as “dependency”).

**Overall variable construction (configured):** the category score uses `Overall Foreign Dependency Index`, calculated as the **mean** of: `Mineral Supply`, `Market Share`, `Overall Market Share`, `production`, `sales`, `import`, `market_share`, and `Overall Production`.

**Data sources:**
- **IEA Critical Minerals Dataset** ([IEA-CM])
- **IEA Global EV Data Explorer** ([IEA-EV])
- **Energy Institute Statistical Review** ([EI-SR])

**Implementation entry points:**
- `R/categories/foreign_dependency/foreign_dependency.R`
- `R/categories/foreign_dependency/market_share_manufacturing.R`

---

#### 9) Consumption
**What it captures:** market size and growth runway (installed capacity per capita and projected growth).

**Overall variable construction (configured):** the category score uses `Overall Consumption Index`, calculated as the **mean** of: `Energy consumption per capita`, `Energy consumption per capita growth`, `installed_cap_index`, and `elec_growth_index`.

**Data sources:**
- **BloombergNEF New Energy Outlook** ([BNEF-NEO])

**Implementation entry point:**
- `R/categories/consumption/energy_consumption.R`


---

#### 10) Investment *(placeholder in current config)*
**What it captures:** currently a placeholder category in the EO configuration.

**Overall variable construction (configured):** the category score is mapped to `Overall Investment Index`, whose configured component list is currently empty (`components: []`), so no sub-indicator aggregation is applied until components are added in configuration.

</details>

---

### Partnership Strength Index (PSI)

**Interpretation:** PSI measures bilateral and country-level partnership quality for energy-system cooperation by combining partner reliability (friendshoring), market upside (opportunity), and absorptive capacity (development potential).

**Configured PSI components (see `config/weights.yml`):**
- Friendshore (0.4)
- Opportunity (0.4)
- Development (0.2)

<details>
<summary><strong>PSI — component-by-component construction notes (with cited sources)</strong></summary>

#### 1) Friendshore (Safer Friendshore)
**What it captures:** importer-side partner safety/compatibility at the reporter-partner dyad level, built from import trade concentration and strategic compatibility signals.

**Data sources:**
- **UN Comtrade** ([COMTRADE]) — import dyads and product-level trade exposure
- **World Development Indicators (WDI)** ([WDI]) — GDP scaffolding for scaling and harmonization
- **IMF Data Explorer** ([IMF-DEX]) — outward investment / position proxies used in outbound connectivity
- **Climate Action Tracker country ratings** (repo extract; see `docs/sources.md`) — climate-policy compatibility inputs

**Substantive rationale:** friendshoring emphasizes dependence on partners that are economically capable, strategically aligned, and less likely to create concentrated vulnerability.

**Implementation entry point:**
- `R/themes/partnership_strength/safer_friendshore.R`

---

#### 2) Partnership Opportunity (Prosperous Opportunity)
**What it captures:** exporter/opportunity upside in each dyad using trade potential with reporter and partner fundamentals, then penalizing risk where applicable.

**Data sources:**
- **UN Comtrade** ([COMTRADE]) — dyadic export structure and concentration inputs
- **World Development Indicators (WDI)** ([WDI]) — country mapping and macro scaffolding
- **Climate Action Tracker country ratings** (repo extract; see `docs/sources.md`) — policy-related weighting/penalty inputs

**Substantive rationale:** high-opportunity partnerships pair favorable market structure with capable partners while discounting combinations that are likely to be lower quality or less resilient.

**Implementation entry point:**
- `R/themes/partnership_strength/prosperous_opportunity.R`

---

#### 3) Development Potential (Stronger Development)
**What it captures:** country-level absorptive and institutional capacity to convert partnership into durable development gains.

**Data sources:**
- **World Development Indicators (WDI)** ([WDI]) — governance, macro/financial, infrastructure, and enabling-condition indicators
- **World Bank Doing Business** (historical extract; see `docs/sources.md`) — business-environment index inputs
- **OECD CRS aid data** (repo extract; see `docs/sources.md`) — aid/sector channels used in development-related scoring

**Substantive rationale:** partnership value is partly determined by local ability to deploy capital, absorb technology, and sustain implementation.

**Implementation entry point:**
- `R/themes/partnership_strength/stronger_development.R`

---

#### 4) PSI composite
**What it captures:** a country × technology × supply-chain composite of friendshore, opportunity, and development components using configured PSI weights.

**Configured default weights:**
- Friendshore = 0.4
- Opportunity = 0.4
- Development = 0.2

Each component is normalized using the median S-curve framework before weighted aggregation.

**Implementation entry points:**
- Build and aggregate in `scripts/25_build_partner_indices.R`
- Tidy composite output helper in `R/themes/partnership_strength/psi_composite.R`

---

#### 5) Allied network design (optimization module)
**What it captures:** coalition-oriented producer-consumer flow design that uses PSI-derived edge quality (friendshore + opportunity) and node capacity/readiness signals to generate feasible alliance network scenarios.

**Substantive rationale:** PSI can be used not only as a scorecard, but also as an input into constrained network design for diversified allied sourcing.

**Implementation entry points:**
- Core optimizer + helpers: `R/indices/allied_network_design.R`
- Scripted run/output writer: `scripts/30_build_allied_network_design.R`

`allied_network_design()` now supports configurable demand composition through `demand_mode` (`"need"`, `"size"`, `"mixed"`, `"equal"`).
For backwards compatibility, the function default remains `demand_mode = "need"` (legacy need-only demand).
The production runner (`scripts/30_build_allied_network_design.R`) now uses `demand_mode = "mixed"` with 50/50 need-size weights.

Portfolio constraints can optionally cap how often a country appears in stage-level top-K producers.
Caps are GDP-derived by default (`portfolio_min_cap = 2`, `portfolio_max_cap = 10`, `portfolio_top_k = 5`, log scaling), while leaving countries available as consumers.
Portfolio enforcement is iterative and writes diagnostics for convergence and any residual violations.

Runner outputs now include three additional diagnostics files: `allied_network_portfolio_caps.csv`, `allied_network_portfolio_counts.csv`, and `allied_network_topk_by_stage.csv`.

</details>

---

## Repository layout

```

R/                    # Pure functions (no IO)
utils/              # Reusable helpers, schema checks, standardization
categories/         # Category/theme calculations
indices/            # Pillar indices + helpers (ES, EO, PSI)
charts/             # Chart builders (no file IO)
themes/             # Theme grouping (incl. partnership_strength)

scripts/              # Orchestration + IO (reads raw, writes processed/outputs)
config/               # Config files (weights, missing data, index definitions)
docs/                 # Methodology + data sources
shiny/                # Interactive Shiny app (world map explorer)
archive/              # Deprecated website assets (archived)
run_pipeline.R        # Pipeline entry point
tests/                # Unit tests (testthat)

````

---

## Quick start

1) **Copy config and update for your environment**

```bash
cp config/config.example.yml config/config.yml
````

This repo also ships with:

* `config/weights.yml`
* `config/missing_data.yml`
* `config/index_definition.yml`

2. **(Optional) Run without local raw data**

If you don’t have the raw inputs available, set:

* `SKIP_DATA_DOWNLOADS=true` (or `1`)

so `scripts/10_build_themes.R` can exit cleanly when inputs are missing.

3. **Run ingestion and theme/index scripts in order**

If you are refreshing UN Comtrade outputs (including
`allied_comtrade_energy_data.csv`, `comtrade_energy_trade.csv`, and
`comtrade_total_export.csv`), set your API key first:

```bash
export COMTRADE_API_KEY=your_key_here
# Optional overrides (defaults shown):
# export COMTRADE_TARGET_YEAR=$(($(date +%Y)-1))
# export COMTRADE_START_YEAR=$((COMTRADE_TARGET_YEAR-4))
# Optional chunked runs for long API pulls:
# export COMTRADE_CHUNK_COUNT=8
# export COMTRADE_CHUNK_INDEX=1   # run 1..8 in separate executions
# export COMTRADE_REQUEST_TIMEOUT_SECONDS=120
# export COMTRADE_MAX_RETRIES=3
```

Then run:

```bash
Rscript scripts/00_setup.R
Rscript scripts/05_ingest_sources.R
Rscript scripts/10_build_themes.R
Rscript scripts/20_build_indices.R
Rscript scripts/15_build_partner_themes.R
Rscript scripts/25_build_partner_indices.R
Rscript scripts/80_write_outputs.R
```

For an end-to-end run, use:

```bash
Rscript run_pipeline.R
```


When `COMTRADE_CHUNK_COUNT > 1`, each run writes chunk files under
`data/raw/comtrade_chunks/...` and automatically combines them into final CSVs
once all chunk indices have been run.

---

## Interactive explorer (Shiny)

Run the interactive world map from the repository root:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "scales",
  "countrycode",
  "leaflet",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata"
))
shiny::runApp("shiny", launch.browser = FALSE)
```

**Data sources:**

- The app looks for pipeline outputs in `data/processed/outputs/index_outputs.rds` (or other
  common output paths). You can override this with `OPSI_OUTPUTS_RDS=/path/to/index_outputs.rds`.
- If no outputs are found, the app falls back to a small synthetic sample dataset stored in
  `shiny/inst/extdata/sample_indices.csv`. The sidebar indicates which data source is active.

---

## Outputs

Outputs are written under your configured outputs directory (or `processed/outputs` by default). Typical outputs include:

* ES and EO pillar index tables
* Category scores
* Category contribution tables
* Variable contribution tables
* PSI tables
* Optional “coupled” pillar index variants (if interdependence edges are provided)

See `scripts/20_build_indices.R` for the exact CSV outputs written.

---

## Testing and quality checks

This repository includes a `testthat` suite under `tests/testthat/` that covers core scoring behavior and integration-sensitive paths, including:

* S-curve normalization and schema checks
* ES/EO v2 index construction
* NIPO policy index logic
* Comtrade unification behavior
* Package-selection visual outputs
* Allied network design scaling
* Shiny app load smoke test

Run tests from repo root with:

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

---

## Additional utilities and workflows

Beyond the main pipeline scripts, the repo includes supporting workflows:

* `scripts/01_generate_raw_inputs_manifest.R` — generates/refreshes a raw-input inventory from configured requirements.
* `config/raw_inputs_manifest.yml` — declarative manifest used to track expected raw datasets.
* `scripts/30_build_allied_network_design.R` — builds allied network design outputs used for partnership analyses.
* `scripts/90_build_package_selection_viz.R` — prepares package-selection visualization outputs.
* `scripts/96_pull_trade_timeseries.R` and `R/charts/trade_timeseries.R` — pull and visualize trade time series.
* `docs/data_dictionary.md` and `docs/comtrade_ingestion_and_timeseries.md` — implementation details for data fields and Comtrade workflows.

For app-specific details (UI behavior, data-loading order, and deployment notes), see `shiny/README.md`.

---

## Data sources

A curated list of sources (including links and expected provenance) is maintained in:

* `docs/sources.md`

Theme builders read raw inputs directly from your configured `raw_data_dir/...`.

> **Tip:** `scripts/10_build_themes.R` and `scripts/15_build_partner_themes.R` are the authoritative “what files are required” inventory, because they list the expected raw file names and wire them into each theme builder.

### Key external sources (cited)

* [IEA Critical Minerals Dataset][IEA-CM]
* [IEA ETP Clean Energy Technology Guide][IEA-CTG]
* [IEA Global EV Data Explorer][IEA-EV]
* [Energy Institute Statistical Review][EI-SR]
* [UN Comtrade][COMTRADE]
* [Harvard Atlas of Economic Complexity][AEC]
* [World Development Indicators (WDI)][WDI]
* [Global Solar Atlas][GSA]
* [Global Wind Atlas][GWA]
* [BloombergNEF New Energy Outlook][BNEF-NEO]
* [IMF Commodity Prices][IMF-COMM]
* [ILOSTAT][ILO]
* [IMF Data Explorer][IMF-DEX]

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
* any manual overrides (e.g., missing data policies or custom weights)

---

## Known issues and to-dos

* **EO Investment category is currently a placeholder** in the configured EO category list and may not yet represent a finalized production methodology.
* **Supply-chain coupling implementation differs from the methodology text**: current code uses normalized interdependence edge strength with linear lambda mapping (rather than HHI-driven logistic mapping).
* **Raw-input completeness is environment dependent**. If expected files are missing, use the manifest workflow and source documentation to reconcile gaps before running full builds.

---

## License

This project is licensed under the terms in `LICENSE`.

For contribution expectations and responsible disclosure guidance, see `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, and `SECURITY.md`.

---

<!-- Link reference definitions -->

[IEA-CM]: https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset
[IEA-CTG]: https://www.iea.org/data-and-statistics/data-tools/etp-clean-energy-technology-guide
[IEA-EV]: https://www.iea.org/data-and-statistics/data-tools/global-ev-data-explorer
[EI-SR]: https://www.energyinst.org/statistical-review
[COMTRADE]: https://comtrade.un.org/
[AEC]: https://atlas.hks.harvard.edu/
[WDI]: https://databank.worldbank.org/source/world-development-indicators
[GSA]: https://globalsolaratlas.info
[GWA]: https://globalwindatlas.info/
[BNEF-NEO]: https://about.bnef.com/insights/clean-energy/new-energy-outlook/
[IMF-COMM]: https://www.imf.org/en/research/commodity-prices
[ILO]: https://ilostat.ilo.org/data/
[IMF-DEX]: https://data.imf.org/en/Data-Explorer
