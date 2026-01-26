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
<summary><strong>ES — Category-by-category construction notes (with sources)</strong></summary>

#### 1) Foreign Dependency
**What it captures:** exposure to foreign control of critical upstream inputs (minerals) and midstream manufacturing capacity (clean tech / EV supply).

**Data sources:**
- **IEA Critical Minerals Database** — `iea_criticalminerals_25.csv` (mineral “Total supply” series; used to compute market share + concentration proxies)
- **IEA Energy Technology Perspectives 2024** — `iea_cleantech_Midstream.csv` (clean-tech midstream capacity/market shares; EU roll-up applied)
- **IEA EV Outlook** — `ev_Midstream_capacity.csv` (EV production/sales/import share/market share inputs)
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv` (country harmonization + EU membership mapping used for rollups/expansion)

**Substantive rationale:** Foreign dependency is a first-order vulnerability channel: when upstream/midstream capacity is externally concentrated, disruptions or policy restrictions transmit quickly into domestic shortages and price shocks.

**Implementation entry points:**
- Theme code: `R/categories/foreign_dependency/foreign_dependency.R`
- Midstream share scaffolding: `R/categories/foreign_dependency/market_share_manufacturing.R`

---

#### 2) Energy Imports
**What it captures:** reliance on external suppliers for fuels (oil/gas/coal), proxied by production-consumption balance.

**Data sources:**
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv`

**Substantive rationale:** import dependence is the classic energy-security exposure: it increases sensitivity to foreign supply disruptions and global price volatility.

**Implementation entry point:**
- `R/categories/energy_imports/import_dependence.R`

---

#### 3) Reserves
**What it captures:** domestic reserves depth (fossil + minerals) and (in some cases) demand-weighted “technology reserves” for minerals.

**Data sources:**
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy_wide.xlsx` (fossil + mineral reserves sheets)
- **IEA Critical Minerals Database** — `iea_criticalminerals_25.csv` (used to construct demand-by-tech weights for rolling mineral reserves into tech-weighted reserves)

**Substantive rationale:** reserves proxy long-run domestic supply optionality and reduce risk of external supply squeeze.

**Implementation entry point:**
- `R/categories/reserves/reserves.R`

---

#### 4) Trade (risk)
**What it captures:** concentrated exposure and/or weak strategic positioning in relevant traded energy-tech products.

**Data sources:**
- **UN Comtrade** — `comtrade_energy_trade.csv`, `comtrade_total_export.csv` (exports/imports; RCA; totals)
- **Harvard Atlas of Economic Complexity** — `hs92_country_product_year_4.csv`, `hs92_country_product_year_6.csv` (distance→feasibility; global market share)
- **World Bank WDI** — `wdi_gdp.csv` (GDP for deficit-to-GDP scaling)
- **Project HS mapping** — `consolidated_hs6_energy_tech_long.csv` (maps HS6 → tech/supply_chain/sub-sector)

**Substantive rationale:** concentrated trade relationships create single points of failure; persistent deficits indicate structural dependence.

**Implementation entry points:**
- Core logic: `R/categories/trade/trade_core.R`
- Wrappers: `R/categories/trade/trade_concentration.R`, `R/categories/trade/export_feasibility.R`

---

#### 5) Minerals Trade
**What it captures:** critical minerals trade positioning and concentration, rolled up into technology exposure via demand weights.

**Data sources:**
- **UN Comtrade** — `critmin_import_2024.csv`, `critmin_export_2024.csv`, `critmin_total_export_2024.csv`
- **IEA Critical Minerals Database** — `iea_criticalminerals_25.csv` (mineral list + demand-by-tech shares used to roll minerals into tech exposure)

**Substantive rationale:** critical minerals are binding inputs in the electro-industrial stack; trade structure determines vulnerability to bottlenecks and bargaining power.

**Implementation entry point:**
- `R/categories/minerals_trade/critical_minerals_trade.R`

---

#### 6) Production
**What it captures:** depth and momentum of domestic production (fossil production and electricity generation proxies; plus critical minerals supply/production proxies).

**Data sources:**
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv` (fossil production and generation series)
- **IEA Critical Minerals Database** — `iea_criticalminerals_25.csv` (mineral “Total supply” series used for mineral production/scaling proxies)
- **IEA demand weights (via the critical minerals file)** — used to roll minerals into tech-weighted indices

**Substantive rationale:** deeper domestic production provides redundancy and shock-absorption capacity; it also affects surge capability.

**Implementation entry point:**
- `R/categories/production/production_depth_momentum.R`

---

#### 7) Energy Access
**What it captures:** downstream access/enabling conditions, including per-capita consumption and renewable resource potential.

**Data sources:**
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv` (population + per-capita consumption metrics)
- **Global Solar Atlas (country GIS data)** — `solar_potential_clean.csv` (PV potential totals and per-area)
- **Global Wind Atlas (country data)** — `wb_wind_country.csv` (wind power density / thresholds / totals)

**Substantive rationale:** reliable energy access and domestic renewable potential reduce dependence on imported fuels and enable industrial scaling.

**Implementation entry points:**
- `R/categories/energy_access/energy_access_consumption.R`
- `R/categories/energy_access/solar_pv_potential.R`
- `R/categories/energy_access/wind_potential.R`

---

#### 8) Consumption
**What it captures:** installed base / consumption-side scale and growth (e.g., per-capita installed capacity and growth).

**Data sources:**
- **BNEF New Energy Outlook 2024** — `2024-10-29 - New Energy Outlook 2024.csv` (installed capacity + projections; population-normalized)
- *(Legacy / overlap)* **Energy Institute Statistical Review of World Energy (2024)** — per-capita consumption exists in the pipeline but is emitted under **Energy Access** rather than **Consumption**.

**Substantive rationale:** consumption indicates exposure scale (how much must be secured) and infrastructure intensity (how demanding the system is).

**Implementation entry point:**
- `R/categories/consumption/energy_consumption.R`

---

#### 9) Energy Prices
**What it captures:** commodity input price volatility (annualized volatility of monthly log returns), oriented so lower volatility scores higher.

**Data sources:**
- **IMF Commodity Prices** — `imf_commodity_prices.csv`

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
<summary><strong>EO — Category-by-category construction notes (with sources)</strong></summary>

#### 1) Trade (opportunity)
**What it captures:** export competitiveness and feasibility (e.g., RCA, feasibility, market shares), rather than import vulnerability.

**Data sources:**
- **UN Comtrade** — `comtrade_energy_trade.csv`, `comtrade_total_export.csv`
- **Harvard Atlas of Economic Complexity** — `hs92_country_product_year_4.csv`, `hs92_country_product_year_6.csv`
- **World Bank WDI** — `wdi_gdp.csv`
- **Project HS mapping** — `consolidated_hs6_energy_tech_long.csv`

**Rationale:** trade competitiveness is a growth channel: countries positioned to export intermediate/final goods capture value and learning effects.

**Implementation entry points:**
- Core logic: `R/categories/trade/trade_core.R`
- Wrappers: `R/categories/trade/trade_concentration.R`, `R/categories/trade/export_feasibility.R`

---

#### 2) Production
**What it captures:** productive capability and momentum — similar underlying measures as ES, but interpreted as growth potential.

**Data sources:**
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv`
- **IEA Critical Minerals Database** — `iea_criticalminerals_25.csv` (mineral supply series used for mineral-side production proxies)

**Rationale:** deeper production ecosystems tend to scale faster and attract investment; production is both a capability and a market signal.

**Implementation entry point:**
- `R/categories/production/production_depth_momentum.R`

---

#### 3) Technology Demand
**What it captures:** forward demand and growth (global and/or country-level), plus midstream overcapacity penalties as a margin-risk proxy.

**Data sources:**
- **IEA World Energy Outlook (WEO)** — `WEO2024_AnnexA_Free_Dataset_World.csv` (global demand levels and growth; expanded to country scaffold)
- **BNEF New Energy Outlook (NEO)** — `2024-10-29 - New Energy Outlook 2024.csv` (region/country demand levels and growth)
- **IEA EV Data Explorer / EV Outlook data** — `IEA_EVDataExplorer2025.xlsx` (EV stock/sales/share + growth)
- **BCG market sizing workbook** — `Market Size for Technology and Supply Chain.xlsx` (SAM / addressable market proxy)
- **BNEF Energy Transition Supply Chains 2025** — `BNEF_Energy Transition Supply Chains 2025.xlsx` (overcapacity ratios)

**Rationale:** opportunity follows demand — but segments with persistent overcapacity may be structurally low-margin even when volumes rise.

**Implementation entry points:**
- `R/categories/technology_demand/future_demand.R`
- `R/categories/technology_demand/overcapacity_premium.R`

---

#### 4) Technological Readiness
**What it captures:** technology maturity proxy (TRL), normalized to 0–1.

**Data sources:**
- **IEA Clean Tech Guide** — `IEA_Clean_Tech_Guide.csv`

**Rationale:** higher readiness usually implies nearer-term deployability, clearer cost curves, and lower commercialization risk.

**Implementation entry point:**
- `R/categories/technological_readiness/technological_readiness.R`

---

#### 5) Cost Competitiveness
**What it captures:** deployment/manufacturing cost position, including relative technology costs, LCOE competitiveness, and composite “input cost” competitiveness (labor + capital proxies).

**Data sources:**
- **IEA Energy Technology Perspectives 2024** — `Relative_Costs_IEA.csv` (relative costs vs a benchmark, used as midstream cost index)
- **BNEF LCOE Estimates (2025)** — `2025-03-24 - 2025 LCOE Data Viewer Tool.csv` (LCOE by tech/region; normalized so lower LCOE scores higher)
- **International Labour Organization (ILOSTAT)** — pulled via API in `scripts/10_build_themes.R` (earnings by economic activity; used as labor cost proxy)
- **International Monetary Fund (IMF)** — `imf_lending_rates.csv` (financing cost proxy) and `imf_ppi.csv` (price environment proxy)

**Rationale:** opportunity depends not just on demand, but on **bankable cost competitiveness** — production + deployment happen where costs clear.

**Implementation entry points:**
- `R/categories/energy_prices/lcoe_competitiveness.R`
- `R/categories/economic opportunity/cost_competitiveness.R`

---

#### 6) Energy Prices
**What it captures:** macro input price volatility, oriented so lower volatility scores higher.

**Data sources:**
- **IMF Commodity Prices** — `imf_commodity_prices.csv`

**Rationale:** stable price environments improve bankability and reduce risk premiums for investment.

**Implementation entry point:**
- `R/categories/energy_prices/energy_prices.R`

---

#### 7) Energy Access
**What it captures:** enabling conditions (per-capita consumption and renewable resource potential).

**Data sources:**
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv`
- **Global Solar Atlas (country GIS data)** — `solar_potential_clean.csv`
- **Global Wind Atlas (country data)** — `wb_wind_country.csv`

**Implementation entry points:**
- `R/categories/energy_access/energy_access_consumption.R`
- `R/categories/energy_access/solar_pv_potential.R`
- `R/categories/energy_access/wind_potential.R`

---

#### 8) Foreign Dependency
**What it captures:** in EO, these variables can reflect constraint *or* positioning (e.g., high midstream market share can be “opportunity” even if framed elsewhere as “dependency”).

**Data sources:**
- **IEA Critical Minerals Database** — `iea_criticalminerals_25.csv`
- **IEA Energy Technology Perspectives 2024** — `iea_cleantech_Midstream.csv`
- **IEA EV Outlook** — `ev_Midstream_capacity.csv`
- **Energy Institute Statistical Review of World Energy (2024)** — `ei_stat_review_world_energy.csv` (EU rollups/country mapping)

**Implementation entry points:**
- `R/categories/foreign_dependency/foreign_dependency.R`
- `R/categories/foreign_dependency/market_share_manufacturing.R`

---

#### 9) Consumption
**What it captures:** market size and growth runway (installed capacity per capita and projected growth).

**Data sources:**
- **BNEF New Energy Outlook 2024** — `2024-10-29 - New Energy Outlook 2024.csv`

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

**Implementation entry points:**
- PSI composition: `R/outputs/psi_composite.R`, `R/indices/build_partnership_strength_index.R`
- Component themes: `R/themes/partnership_strength/`

<details>
<summary><strong>PSI — Conceptual intent</strong></summary>

- **Friendshore**: How feasible/strategically sensible it is to deepen supply ties, given trade links, investment links, and alignment constraints.
- **Opportunity**: Where partnerships are likely to generate the largest strategic and economic payoff (EO of exporter + ES need of partner, etc.).
- **Development potential**: Where engagement can credibly unlock development and institutional capacity improvements.

</details>

---

## Repository layout

