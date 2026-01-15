# Energy Security Index v2

## Overview
The v2 builder (`build_energy_security_index_v2()`) is a drop-in alternative to the legacy Energy Security index builder, designed to align more strictly with the YAML-driven index definition, missing-data policy, and latest-year selection rules.

## Key differences from legacy
- **Score variables are defined in `config/index_definition.yml`.** The v2 builder reads the Energy Security pillar’s category definitions and uses the configured `score_variable` for each category (no hard-coded variable name assumptions).
- **Latest-year selection per group.** The v2 builder normalizes `Year` values and uses the latest available year per `(Country, tech, supply_chain, sub_sector, category, score_variable)`.
- **Missing-data policy is applied before scoring.** Missing values are imputed at the variable level using `config/missing_data.yml` rules (global average or zero) prior to computing category scores.
- **Decomposition outputs.** The v2 builder returns category- and variable-level contribution tables that reconcile with the final index value for each group.

## How to enable
1. Add `use_energy_security_v2: true` in `config/config.yml` (or set it in your runtime config).
2. Run the pipeline as usual (`scripts/20_build_indices.R` will choose v2 when the flag is true).

## Outputs
The v2 builder returns a list with:
- `energy_security_index`: the final index table (`Country`, `tech`, `supply_chain`, `sub_sector`, `Energy_Security_Index`, `Year_used`).
- `category_contributions`: category scores, weights, and weighted contributions.
- `variable_contributions`: variable-level contributions to each category and the overall index.
- `diagnostics`: coverage and imputation summaries.

The category and variable contribution tables sum to the overall `Energy_Security_Index` per group (within floating-point tolerance).
