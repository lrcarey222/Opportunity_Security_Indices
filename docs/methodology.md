# Methodology

This project produces three pillars for the Opportunity Security Indices:

- **Energy Security (ES)**: resilience of energy supply chains across upstream, midstream, and downstream stages.
- **Economic Opportunity (EO)**: market and competitiveness opportunity across the same stages.
- **Partnership Strength Index (PSI)**: a composite of friendshoring, opportunity, and development potential.

## Unit of analysis

Metrics are computed at the **country × technology × supply chain stage** level. Supply-chain stages are standardized to `Upstream`, `Midstream`, and `Downstream`. The technology set includes:

- Electric Vehicles
- Nuclear
- Coal
- Batteries
- Green Hydrogen
- Wind
- Oil
- Solar
- Gas
- Geothermal
- Electric Grid

## Normalization: median-centered S-curve

Raw metrics are normalized to a 0–1 index using a median-centered S-curve. The process ranks values by percentile and applies:

```
idx = r^gamma / (r^gamma + (1 - r)^gamma)
```

with `gamma = 0.5` as the default. This compresses the middle of the distribution while preserving separation at the tails.

## Aggregation

Theme-level indices are aggregated into pillar indices using weighted means across categories. Weights are defined by category and applied within each country/technology/supply-chain group. The ES and EO pillars are computed from their respective categories (e.g., Foreign Dependency, Trade, Production, Consumption, Technology Demand, Investment, etc.).

## Supply-chain coupling (HHI-weighted shrinkage)

To align stage-level pillar scores with a technology-wide supply-chain signal, each pillar stage score is shrunk toward a chain score computed as the geometric mean across upstream, midstream, and downstream stages (using an epsilon to avoid log-zero). The shrinkage strength is determined by a tech-level HHI concentration measure: upstream and midstream HHI values are averaged (latest available HHI year) and normalized to [0, 1]. A bounded logistic ramp maps normalized HHI to a lambda in [0.15, 0.65], and the coupled score is `(1 - lambda) * stage_score + lambda * chain_score`. Coupling is applied to pillar scores only, leaving category scores unchanged.

## Partnership Strength Index (PSI)

PSI blends three components, each normalized with the same S-curve, using a weighted mean:

- Friendshore index (0.4)
- Opportunity index (0.4)
- Development potential index (0.2)

## Pipeline stages

1. **Ingest sources** → raw inputs from external datasets.
2. **Build themes** → compute raw and indexed metrics at the theme level.
3. **Build indices** → aggregate themes into ES/EO and compute PSI inputs.
4. **Write outputs** → standardized tables and sheets.
5. **Build charts** → publication-ready visuals.
