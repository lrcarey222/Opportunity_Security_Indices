# Energy Security Index v2 (and its Economic Opportunity twin)

## Overview

`build_energy_security_index_v2()` (`R/indices/build_energy_security_index_v2.R`) builds the Energy Security (ES) pillar from the theme tables. It is the only ES builder the pipeline uses: `scripts/20_build_indices.R` and the vintage builder (`scripts/40_build_index_vintages.R`) call it directly, so no config flag selects it (`use_energy_security_v2` in older `config.yml` files is ignored). `build_economic_opportunity_index_v2()` (`R/indices/build_economic_opportunity_index_v2.R`) builds the Economic Opportunity (EO) pillar the same way; both share `R/indices/index_builder_core.R`.

## How a pillar is built

1. **Standardise the inputs.** Each theme table is brought onto the common schema, the world-level energy-price rows are copied to every country, and only `index` rows are scored. `sub_sector` collapses to `"All"` unless `include_sub_sector` is on.
2. **Latest year per group.** `Year` is normalised (an ISO date reads as its leading year) and the latest available year is kept per (Country, tech, supply_chain, category, variable, theme); `diagnostics$year_provenance` records it.
3. **Observed-only imputation.** `config/missing_data.yml` rules (`zero`, `global_average`; variable > category > theme) fill a country only in groups where the source observes at least one country; a group nobody is observed in stays `NA`, and a theme without a rule stays `NA`. Filled values are flagged `imputed`.
4. **Overall variables.** Each category's `score_variable` (`config/index_definition.yml`) is built as the mean of the components present. The builder warns when a configured score variable is never produced, or when a theme's own same-named composite stands in for the configured definition.
5. **Available-case aggregation.** Within each cell the index is the weighted mean of the **observed** category scores, `sum w_k s_k / sum w_k` over observed k (`config/weights.yml`); an unobserved category carries no weight, and a cell with none is `NA`.
6. **Decomposition.** Category and variable contributions share one denominator and sum to the index (within floating-point tolerance).
7. **Imputed weight share and rankability.** `imputed_weight_share` is the share of the cell's weight on imputed components. `rankable` is TRUE for a sovereign economy (Correlates of War state system) with `imputed_weight_share <= max_imputed_weight_share` (`tau`, 0.5 in `config/rankability.yml`; `scripts/20` passes it; without it the builder takes the `opportunity_security.rankability` option, else tau = 1 with a message). Values are never changed by the rule.

## Arguments

```r
build_energy_security_index_v2(
  theme_tables,                 # named list of theme tables (scripts/20: 14 ES themes)
  weights,                      # config/weights.yml energy_security
  missing_data = NULL,          # config/missing_data.yml energy_security
  allow_partial_categories = TRUE,
  include_sub_sector = FALSE,
  techs = NULL,                 # optional technology filter
  max_imputed_weight_share = NULL
)
```

`build_economic_opportunity_index_v2()` takes the same arguments (with `index_definition` in place of `techs`).

## Outputs

A list with:

- `energy_security_index` (also `index`): `Country`, `tech`, `supply_chain`, `Energy_Security_Index`, `imputed_weight_share`, `rankable`, `rank_exclusion` (`NA` when rankable; otherwise "not scored", "not a sovereign economy", "imputed weight share above <tau>", both of the last two, or "imputed weight share unknown").
- `category_scores`: one row per cell and category, with `category_score`, `imputed` and `missing_rule_applied`.
- `category_contributions`: category scores, weights and weighted contributions.
- `variable_contributions`: variable-level contributions to each category and to the index.
- `diagnostics`: `categories_configured`, `categories_in_data`, `categories_unproduced`, `categories_substituted`, `imputed_share` and `imputed_share_by_category`, `unscored_groups`, `index_cells` (unscored and constant tech × stage cells), `index_imputation` (per-cell imputed weight share), `rankability` (rankable cells by stage), `unobserved_components` (groups no country is observed in), `year_provenance`.

The EO builder returns the same list with `economic_opportunity_index` and `Economic_Opportunity_Index`.

## Reading the index

Higher ES = more secure. The Strategic Index uses the need reading `energy_security_need = 1 - Energy_Security_Index`; the ES column keeps its security meaning. Cross-country rankings and top-N lists should use rankable cells only.
