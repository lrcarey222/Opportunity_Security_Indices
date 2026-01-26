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
<summary><strong>ES — Category-by-category construction notes</strong></summary>

#### 1) Foreign Dependency
**What it captures:** exposure to foreign control of critical upstream inputs (minerals) and midstream manufacturing capacity (clean tech / EV supply).

**Typical metrics used (by theme):**
- Market shares and concentration measures (e.g., HHI) for supply / production / manufacturing.
- Composite “Overall …” indices that combine market share + concentration / dependency proxies.

**Substantive rationale:** Foreign dependency is a first-order vulnerability channel: when upstream/midstream capacity is externally concentrated, disruptions or policy restrictions transmit quickly into domestic shortages and price shocks.

**Implementation entry points:**
- Theme code under `R/categories/foreign_dependency/`
- Midstream share scaffolding under `R/categories/market_share_manufacturing/`

#### 2) Energy Imports
**What it captures:** reliance on external suppliers for fuels (oil/gas/coal), proxied by production-consumption balance.

**Substantive rationale:** import dependence is the classic energy-security exposure: it increases sensitivity to foreign supply disruptions and global price volatility.

**Implementation entry point:**
- Theme code under `R/categories/import_dependence/`

#### 3) Reserves
**What it captures:** domestic reserves depth (fossil + minerals) and (in some cases) technology-weighted mineral reserves.

**Substantive rationale:** reserves proxy long-run domestic supply optionality and reduce risk of external supply squeeze.

**Implementation entry point:**
- Theme code under `R/categories/reserves/`

#### 4) Trade (risk)
**What it captures:** concentrated import exposure and weak strategic export position in relevant products/materials.

**Typical metrics:**
- HHI concentration (diversification proxy)
- Trade balance scaled to GDP (dependency proxy)
- Export capability measures (market share, feasibility, RCA), depending on whether ES or EO view is used

**Substantive rationale:** concentrated trade relationships create single points of failure; persistent deficits indicate structural dependence.

**Implementation entry points:**
- `R/categories/trade_concentration/`
- `R/categories/export_feasibility/`

#### 5) Minerals Trade
**What it captures:** market structure and positioning in critical minerals trade (often weighted into tech-level exposure).

**Substantive rationale:** critical minerals are binding inputs in the electro-industrial stack; trade structure determines vulnerability to bottlenecks and bargaining power.

**Implementation entry point:**
- `R/categories/critical_minerals_trade/`

#### 6) Production
**What it captures:** depth and momentum of domestic production/industrial capacity for the relevant tech/supply-chain stage.

**Substantive rationale:** deeper domestic production provides redundancy and shock-absorption capacity; it also affects surge capability.

**Implementation entry point:**
- `R/categories/production_depth_momentum/`

#### 7) Energy Access
**What it captures:** baseline consumption and domestic renewable resource potential (e.g., solar/wind potential), depending on tech and stage.

**Substantive rationale:** reliable energy access and domestic generation potential reduce dependence on imported fuels and enable industrial scaling.

**Implementation entry points:**
- `R/categories/energy_access/`
- `R/categories/solar_pv_potential/`
- `R/categories/wind_potential/`

#### 8) Consumption
**What it captures:** installed base and/or demand-side scaling (e.g., capacity per capita; electricity demand growth), depending on tech.

**Substantive rationale:** consumption indicates exposure scale (how much must be secured) and infrastructure intensity (how demanding the system is).

**Implementation entry point:**
- `R/categories/energy_consumption/`

#### 9) Energy Prices
**What it captures:** input price volatility (IMF commodity volatility proxies), normalized so lower volatility scores higher.

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
<summary><strong>EO — Category-by-category construction notes</strong></summary>

#### 1) Trade (opportunity)
**What it captures:** export competitiveness and feasibility (e.g., RCA, export feasibility, market shares), rather than import vulnerability.

**Rationale:** trade competitiveness is a growth channel: countries positioned to export intermediate/final goods capture value and learning effects.

#### 2) Production
**What it captures:** productive capability and momentum — similar underlying measures as ES, but interpreted as growth potential.

**Rationale:** deeper production ecosystems tend to scale faster and attract investment; production is both a capability and a market signal.

#### 3) Technology Demand
**What it captures:** forward demand and growth (global and/or country-level), sometimes paired with “overcapacity penalties” as a margin-risk proxy.

**Rationale:** opportunity follows demand — but segments with persistent overcapacity may be structurally low-margin even when volumes rise.

**Implementation entry points:**
- `R/categories/future_demand/`
- `R/categories/overcapacity_premium/`

#### 4) Technological Readiness
**What it captures:** technology maturity proxies such as TRL.

**Rationale:** higher readiness usually implies nearer-term deployability, clearer cost curves, and lower commercialization risk.

**Implementation entry point:**
- `R/categories/technological_readiness/`

#### 5) Cost Competitiveness
**What it captures:** cost position for deployment/manufacturing, using a mix of:
- Relative tech costs (e.g., IEA vs benchmark, BNEF LCOE)
- Input cost environment (labor and capital proxies)

**Rationale:** opportunity depends not just on demand, but on **bankable cost competitiveness** — production + deployment happen where costs clear.

**Implementation entry points:**
- `R/categories/lcoe_competitiveness/`
- `R/categories/economic opportunity/cost_competitiveness.R`

#### 6) Energy Prices
Same underlying volatility logic as ES, but interpreted as an enabling (or constraining) macro cost environment.

#### 7) Energy Access
Renewable potential and access proxies are enabling conditions for industrial scale and deployment.

#### 8) Foreign Dependency
In EO, foreign dependency variables can function as *either* constraint (risk) *or* proxy for market positioning (e.g., high market share in midstream can indicate opportunity). The category definition and variable orientation should be checked carefully for consistency.

#### 9) Consumption
Demand-side scale / growth; can represent market size and growth runway.

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

