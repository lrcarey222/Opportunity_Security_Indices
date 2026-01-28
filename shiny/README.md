# Shiny interactive explorer

This Shiny app provides an interactive world map for the Opportunity Security Indices using
an embedded Datawrapper choropleth (optional).

## Run locally

From the repo root:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "scales",
  "countrycode",
  "httr",
  "jsonlite"
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

## Datawrapper publishing (optional)

The app includes an optional Datawrapper tab that can publish a world choropleth map using
the Datawrapper API. The app still runs without a Datawrapper key; it will show a
“Datawrapper disabled” message until a key is provided.

Set environment variables before running the app:

```bash
export DATAWRAPPER_API_KEY="..."
export DATAWRAPPER_CHART_ID_WORLD="..." # optional
```

You can also set the key inside an R session:

```r
Sys.setenv(DATAWRAPPER_API_KEY = "your-key-here")
Sys.setenv(DATAWRAPPER_CHART_ID_WORLD = "your-chart-id") # optional
```

For a persistent key that is not committed, add it to your `~/.Renviron` file and restart R:

```
DATAWRAPPER_API_KEY=your-key-here
DATAWRAPPER_CHART_ID_WORLD=your-chart-id
```

The chart ID is cached locally in `~/.config/opportunity_security_indices/datawrapper_chart_world.txt`
when created.

## Security check (no secrets)

To confirm no Datawrapper keys are stored in tracked files, run:

```bash
git grep -n -E "(DATAWRAPPER_API_KEY\\s*=\\s*\\\"|api\\.datawrapper|Bearer\\s+[A-Za-z0-9]{10,}|sk-[A-Za-z0-9])"
```

## Deployment (optional)

You can deploy this app to hosting platforms such as Posit Connect or shinyapps.io by
pointing the deployment target at the `shiny/` directory. Ensure the same packages above
are installed on the host.
