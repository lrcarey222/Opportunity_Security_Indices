# Package Selection Visualization

## What this module does

This module creates a country-specific **Package Selection Visualization** bundle from `index_outputs.rds`:

1. PNG charts for memo-ready figures
2. Datawrapper-ready CSVs in wide format
3. A `manifest.csv` mapping chart metadata to each file

The chart builders are pure functions in `R/charts/package_selection_viz.R`, and all file IO is orchestrated in `scripts/90_build_package_selection_viz.R`.

## How to run

From the repository root:

```bash
Rscript scripts/90_build_package_selection_viz.R --country="Japan"
```

With optional arguments:

If you prefer not to pass CLI args every run, you can set the default in the script:

- Edit `scripts/90_build_package_selection_viz.R` and change `DEFAULT_COUNTRY <- "Japan"` to your preferred country (for example `"India"`).
- `--country=...` still overrides that default when provided.


```bash
Rscript scripts/90_build_package_selection_viz.R \
  --country="Japan" \
  --out-dir="outputs" \
  --top-n=10 \
  --top-k-vars=5 \
  --include-raw-highlights=true \
  --selected="Solar - Midstream;Wind - Midstream;Batteries - Midstream"
```

## Output structure

Default output location:

- `file.path(repo_root, config$outputs_dir, "package_selection_viz", <country_slug>)`
- falls back to `outputs/` when `config$outputs_dir` is not set

Generated files:

- `plots/*.png`
- `datawrapper/*.csv`
- `manifest.csv`

## Included charts

Minimum chart pack generated:

1. Country strategic-index heatmap
2. Country EO vs ES-risk scatter
3. Top-N sectors decomposition stacked bar
4. ES category contributions for selected sectors
5. EO category contributions for selected sectors

Optional (if partner tables are available and schema matches expected columns):

6. Partner shortlist chart for selected sectors

## Datawrapper manual upload

1. Open Datawrapper and create a chart of the recommended type in `manifest.csv`.
2. Upload the matching CSV from `datawrapper/`.
3. Set labels and styling using the chart title from `manifest.csv`.
4. Publish/export once formatting is complete.

No Datawrapper API key is required for this workflow.

## Passing a selected package list

Pass semicolon-separated sector labels via `--selected`:

```bash
--selected="Solar - Midstream;Wind - Midstream;Batteries - Midstream"
```

Each label must match `paste(tech, supply_chain, sep = " - ")`.


## Raw indicator highlights (top EO/ES drivers)

The module now exports additional EO/ES outputs to show the **raw indicator metrics** behind top variable drivers for selected sectors.

1. **Variable selection**
   - For each pillar (EO/ES), selected sector, and country, the script ranks variables by absolute `weighted_variable_contribution` and keeps the top `--top-k-vars` entries.
   - Exports include a wide Datawrapper CSV and a long audit CSV for traceability (`component_value`, `category_weight`, `imputed`, `missing_rule_applied`).

2. **Raw value extraction**
   - For selected top variables, the script reads processed theme tables from `config$processed_dir` (`*_tbl.rds`).
   - It uses tidy-schema keys (`Country`, `tech`, `supply_chain`, `category`, `variable`, `data_type`) and filters to `data_type == "raw"`.
   - If multiple country rows exist, it normalizes Year by extracting a trailing 4-digit year and picks the latest row.

3. **Global context statistics**
   - For each variable/sector metric, global quartiles (`global_p25`, `global_p50`, `global_p75`) are computed across countries at the selected/latest year.
   - These are used for range-style plotting and Datawrapper upload.

4. **Output files**
   - `plots/eo_top_variable_contributions_selected.png`
   - `plots/es_top_variable_contributions_selected.png`
   - `plots/eo_raw_highlights_selected.png`
   - `plots/es_raw_highlights_selected.png`
   - `datawrapper/eo_top_variable_contributions_selected_wide.csv`
   - `datawrapper/es_top_variable_contributions_selected_wide.csv`
   - `datawrapper/eo_top_variable_contributions_selected_long.csv`
   - `datawrapper/es_top_variable_contributions_selected_long.csv`
   - `datawrapper/eo_raw_highlights_selected_long.csv`
   - `datawrapper/es_raw_highlights_selected_long.csv`
   - `datawrapper/<pillar>_raw_highlights_<sector_slug>.csv` (per selected sector)

If processed theme tables or variable-contribution inputs are missing, the script logs a clear message and skips raw-highlight outputs gracefully.
