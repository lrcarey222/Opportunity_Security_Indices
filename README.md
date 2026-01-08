# Opportunity & Security Indices (R)

This repository builds a reproducible set of composite indices that score energy-related technologies and trade partners across three pillars:

- **Energy Security (ES):** exposure to supply shocks, import dependence, and resource scarcity by technology and supply-chain stage.  
- **Economic Opportunity (EO):** where U.S. firms can most profitably and feasibly expand production, exports, or investment.  
- **Partnership Strength (PSI):** which bilateral relationships deliver the best mix of reduced risk, commercial upside, and executability.

The analytical unit is **Country × Technology × Supply-chain stage (Up/Mid/Down)** for ES and EO, and **Country** (aggregated across technologies) for PSI.

## Method overview

### Normalization (raw → 0–1 index)
Each raw metric is converted to a comparable 0–1 score using a percentile-based S-curve (“median_scurve”, γ=0.5). Metrics are tracked in two forms:
- `raw`: original units (e.g., EJ, kt, $, %)
- `index`: S-curved percentile score used for aggregation

### Aggregation
- Within a category (e.g., Reserves): simple mean of subindices
- Across categories into a pillar: weighted mean using domain-specific weights
- Separate scoring for Upstream / Midstream / Downstream segments

### Partnership Strength (PSI)
PSI combines three components (Safer/Friendshore, Prosperous/Opportunity, Stronger/Development) and blends them:
`PSI = 0.4 × Friendshore + 0.4 × Opportunity + 0.2 × (1 − Development Environment)` (see `docs/methodology.md`).

## Repository layout (high-level)
- `R/` contains **pure functions** (no file IO). Functions return tidy tables or ggplot objects.
- `scripts/` contains orchestration and **all file IO** (reading raw sources, writing outputs, saving charts).
- `run_pipeline.R` is the one entrypoint for a full run.
- `docs/` contains methodology and data documentation intended to meet academic/publication scrutiny.
- `legacy/` contains the original scripts retained for reference.

## Quick start

### 1) Prerequisites
- R (>= 4.2 recommended)
- RStudio (optional)

### 2) Configure
Copy example config files and edit paths for your environment:

- `config/config.example.yml` → `config/config.yml`
- `config/weights.example.yml` → `config/weights.yml`

If any APIs are used (e.g., Comtrade), set keys via environment variables (do not commit secrets):
- `COMTRADE_PRIMARY_KEY=...`

### 3) Run the pipeline
From the repo root:

```r
source("run_pipeline.R")
