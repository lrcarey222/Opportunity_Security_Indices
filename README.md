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
- [Outputs](#outputs)
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

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR])

**Substantive rationale:** import dependence is the classic energy-security exposure: it increases sensitivity to foreign supply disruptions and global price volatility.

**Implementation entry point:**
- `R/categories/energy_imports/import_dependence.R`

---

#### 3) Reserves
**What it captures:** domestic reserves depth (fossil + minerals) and demand-weighted “technology reserves” for minerals.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR]) — fossil and mineral reserves tables used in the repo
- **IEA Critical Minerals Dataset** ([IEA-CM]) — cleantech demand-by-tech shares used to roll mineral reserves into tech-weighted reserve indices

**Substantive rationale:** reserves proxy long-run domestic supply optionality and reduce risk of external supply squeeze.

**Implementation entry point:**
- `R/categories/reserves/reserves.R`

---

#### 4) Trade (risk)
**What it captures:** concentrated exposure and/or weak strategic positioning in relevant traded energy-tech products.

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

**Data sources:**
- **UN Comtrade** ([COMTRADE]) — critical-minerals import/export/totals (as used in repo exports)
- **IEA Critical Minerals Dataset** ([IEA-CM]) — mineral list + demand-by-tech shares used to roll minerals into tech exposure

**Substantive rationale:** critical minerals are binding inputs in the electro-industrial stack; trade structure determines vulnerability to bottlenecks and bargaining power.

**Implementation entry point:**
- `R/categories/minerals_trade/critical_minerals_trade.R`

---

#### 6) Production
**What it captures:** depth and momentum of domestic production (energy production and generation proxies; plus critical minerals supply/production proxies).

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR]) — fossil production and generation series
- **IEA Critical Minerals Dataset** ([IEA-CM]) — mineral supply series used for mineral-side production scaling and concentration proxies

**Substantive rationale:** deeper domestic production provides redundancy and shock-absorption capacity; it also affects surge capability.

**Implementation entry point:**
- `R/categories/production/production_depth_momentum.R`

---

#### 7) Energy Access
**What it captures:** downstream access/enabling conditions, including per-capita consumption and renewable resource potential.

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR]) — population + per-capita consumption metrics
- **Global Solar Atlas** ([GSA]) — solar PV potential GIS-derived country data (as used in repo extracts)
- **Global Wind Atlas** ([GWA]) — wind power density / thresholds / totals (as used in repo extracts)

**Substantive rationale:** reliable energy access and domestic renewable potential reduce dependence on imported fuels and enable industrial scaling.

**Implementation entry points:**
- `R/categories/energy_access/energy_access_consumption.R`
- `R/categories/energy_access/solar_pv_potential.R`
- `R/categories/energy_access/wind_potential.R`

---

#### 8) Consumption
**What it captures:** installed base / consumption-side scale and growth (e.g., per-capita installed capacity and projected growth).

**Data sources:**
- **BloombergNEF New Energy Outlook** ([BNEF-NEO]) — installed capacity and projections used for per-capita and growth indices (as used in repo extracts)

**Substantive rationale:** consumption indicates exposure scale (how much must be secured) and infrastructure intensity (how demanding the system is).

**Implementation entry point:**
- `R/categories/consumption/energy_consumption.R`

---

#### 9) Energy Prices
**What it captures:** commodity input price volatility (annualized volatility of monthly log returns), oriented so lower volatility scores higher.

**Data sources:**
- **IMF Commodity Prices** ([IMF-COMM]) — monthly commodity price series used to compute volatility indices (as used in repo extracts)

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

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR])
- **IEA Critical Minerals Dataset** ([IEA-CM])

**Rationale:** deeper production ecosystems tend to scale faster and attract investment; production is both a capability and a market signal.

**Implementation entry point:**
- `R/categories/production/production_depth_momentum.R`

---

#### 3) Technology Demand
**What it captures:** forward demand and growth (global and/or country-level), plus midstream overcapacity penalties as a margin-risk proxy.

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

**Data sources:**
- **IEA ETP Clean Energy Technology Guide** ([IEA-CTG]) — TRL and technology classification inputs (as used in repo extracts)

**Rationale:** higher readiness usually implies nearer-term deployability, clearer cost curves, and lower commercialization risk.

**Implementation entry point:**
- `R/categories/technological_readiness/technological_readiness.R`

---

#### 5) Cost Competitiveness
**What it captures:** deployment/manufacturing cost position, including relative technology costs, LCOE competitiveness, and composite “input cost” competitiveness (labor + capital proxies).

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

**Data sources:**
- **IMF Commodity Prices** ([IMF-COMM])

**Rationale:** stable price environments improve bankability and reduce risk premiums for investment.

**Implementation entry point:**
- `R/categories/energy_prices/energy_prices.R`

---

#### 7) Energy Access
**What it captures:** enabling conditions (per-capita consumption and renewable resource potential).

**Data sources:**
- **Energy Institute Statistical Review** ([EI-SR])
- **Global Solar Atlas** ([GSA])
- **Global Wind Atlas** ([GWA])

**Implementation entry points:**
- `R/categories/energy_access/energy_access_consumption.R`
- `R/categories/energy_access/solar_pv_potential.R`
- `R/categories/energy_access/wind_potential.R`

---

#### 8) Foreign Dependency
**What it captures:** in EO, these variables can reflect constraint *or* positioning (e.g., high midstream market share can be “opportunity” even if framed elsewhere as “dependency”).

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

**Data sources:**
- **BloombergNEF New Energy Outlook** ([BNEF-NEO])

**Implementation entry point:**
- `R/categories/consumption/energy_consumption.R`

</details>

---

### Partnership Strength Index (PSI)

PSI is a weighted blend of three components:

- **Friendshore index** (0.4)
- **Opportunity index** (0.4)
- **Development potential index** (0.2)

Each component is normalized using the same median S-curve framework, then combined via a weighted mean.

**Cited sources most commonly used in PSI inputs:**
- **UN Comtrade** ([COMTRADE]) — dyadic trade flows (as used in repo extracts)
- **World Development Indicators (WDI)** ([WDI]) — GDP/country reference scaffolding (as used in repo extracts)
- **IMF Data Explorer** ([IMF-DEX]) — outward investment / positions series (as used in repo extracts)

*(Other PSI inputs include additional policy/aid/governance datasets; see `docs/sources.md`.)*

---

## Repository layout

```

R/                    # Pure functions (no IO)
utils/              # Reusable helpers, schema checks, standardization
categories/         # Category/theme calculations
indices/            # Pillar indices + helpers (ES, EO, PSI)
outputs/            # Output builders (no file IO)
charts/             # Chart builders (no file IO)
themes/             # Theme grouping (incl. partnership_strength)

scripts/              # Orchestration + IO (reads raw, writes processed/outputs)
config/               # Config files (weights, missing data, index definitions)
docs/                 # Methodology + data sources
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

2. **(Optional) Run without local raw snapshots**

If you don’t have the raw inputs available, set:

* `SKIP_DATA_DOWNLOADS=true` (or `1`)

so `scripts/10_build_themes.R` can exit cleanly when inputs are missing.

3. **Run the pipeline scripts in order**

```bash
Rscript scripts/00_setup.R
Rscript scripts/10_build_themes.R
Rscript scripts/20_build_indices.R
```

For an end-to-end run, use:

```bash
Rscript run_pipeline.R
```

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

## Data sources

A curated list of sources (including links and expected provenance) is maintained in:

* `docs/sources.md`

Theme builders typically read from date-stamped input snapshots under your configured `raw_data_dir/<snapshot>/...`.

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

* input snapshot dates
* processing date
* any manual overrides (e.g., missing data policies or custom weights)

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

