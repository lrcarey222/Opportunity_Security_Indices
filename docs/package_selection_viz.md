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

```bash
Rscript scripts/90_build_package_selection_viz.R \
  --country="Japan" \
  --out-dir="outputs" \
  --top-n=10 \
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
