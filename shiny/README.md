# Shiny interactive explorer

This Shiny app provides an interactive world map for the Opportunity Security Indices.

## Run locally

From the repo root:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "scales",
  "countrycode",
  "leaflet",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata"
))
shiny::runApp("shiny", launch.browser = FALSE)
```

## Data sources

The app attempts to load pipeline outputs first. It looks for the `index_outputs.rds` file in:

- `data/processed/outputs/index_outputs.rds`
- `data/processed/index_outputs.rds`
- `output/index_outputs.rds`
- `outputs/index_outputs.rds`

You can override this with `OPSI_OUTPUTS_RDS=/path/to/index_outputs.rds`.

If no outputs are found, the app falls back to a small synthetic sample dataset in
`shiny/inst/extdata/sample_indices.csv`. The sidebar indicates which data source is being used.

The map joins data to country polygons using ISO3 codes. If pipeline outputs do not include
ISO3 values, the app derives them from country names with `countrycode` and a small override
dictionary defined in `shiny/R/helpers.R`.

## Deployment (optional)

You can deploy this app to hosting platforms such as Posit Connect or shinyapps.io by
pointing the deployment target at the `shiny/` directory. Ensure the same packages above
are installed on the host.
