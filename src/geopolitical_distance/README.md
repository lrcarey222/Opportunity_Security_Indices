This folder contains the geopolitical-distance replication code.

## Quick start
From the repository root:

```bash
Rscript R/geopolitical_distance/pipeline/run_all.R
```

## Step-by-step workflow
1. Edit parameters in `config/geopolitical_distance.yml`.
2. Run the full pipeline entrypoint above.
3. Inspect outputs in `data_processed/`, `figures/geopolitical_distance/`, and `output/geopolitical_distance/validation_checks.csv`.

## Optional staged execution
Run individual stages as needed:

```bash
Rscript R/geopolitical_distance/pipeline/fetch_data.R
Rscript R/geopolitical_distance/pipeline/estimate_ideal_points.R
Rscript R/geopolitical_distance/pipeline/compute_ipd.R
Rscript R/geopolitical_distance/pipeline/compute_seg.R
Rscript R/geopolitical_distance/pipeline/assign_blocs.R
Rscript R/geopolitical_distance/pipeline/compute_exposure.R
Rscript R/geopolitical_distance/pipeline/make_figures.R
Rscript R/geopolitical_distance/pipeline/validate_and_log.R
```
