# Opportunity Security Indices

This repository contains the scaffold for the Opportunity Security Indices pipeline, which produces three related pillars:

- **Energy Security**: measures resilience and security across energy supply chains.
- **Economic Opportunity**: measures the opportunity for growth and competitiveness in energy-related markets.
- **Partnership Strength (PSI)**: blends friendshoring, opportunity, and development potential into a single partnership index.

## Unit of analysis and data model

The pipeline is organized around **country × technology × supply chain stage** observations. Theme outputs are stored in a tidy format with common columns such as `Country`, `tech`, `supply_chain`, `Pillar`, `category`, `variable`, `data_type` (`raw` vs `index`), and `value`.

## Normalization: `median_scurve`

Raw metrics are converted to indices using a median-centered S-curve. The function converts raw values to a percentile rank, then applies:

```
idx = r^gamma / (r^gamma + (1 - r)^gamma)
```

with `gamma = 0.5` by default. This compresses values around the median while preserving separation in the tails, producing index values on a 0–1 scale. See the legacy pipeline for the reference implementation.

## Aggregation rules

**Energy Security** and **Economic Opportunity** are weighted means of category indices, calculated within each country/technology/supply-chain group.

- Energy Security uses category weights (e.g., Foreign Dependency, Energy Imports, Reserves, Production, Consumption) to compute an overall index.
- Economic Opportunity uses category weights (e.g., Trade, Production, Technology Demand, Energy Prices, Investment) to compute an overall index.

Weights are configurable and surfaced in the pipeline to keep methodology transparent.

## Partnership Strength Index (PSI)

PSI is a weighted blend of three components:

- **Friendshore index** (0.4)
- **Opportunity index** (0.4)
- **Development potential index** (0.2)

These components are normalized with the same median S-curve and combined via a weighted mean.

## Repository layout

```
R/                    # Pure functions (no IO)
  utils/              # Reusable helpers
  themes/             # Theme-level calculations
  indices/            # Pillar indices
  outputs/            # Output builders (no file IO)
  charts/             # Chart builders (no file IO)

scripts/              # Orchestration + IO
config/               # Example configs (copy to config.yml)
docs/                 # Methodology + data dictionary
run_pipeline.R        # Pipeline entry point
```

## Quick start

1. Copy the example configs and update them for your environment:

   ```bash
   cp config/config.example.yml config/config.yml
   cp config/weights.example.yml config/weights.yml
   ```

2. Provide secrets (e.g., API keys) via environment variables using `.Renviron` or `.env`.

3. Run the pipeline from any directory:

   ```bash
   Rscript /path/to/Opportunity_Security_Indices/run_pipeline.R
   ```
