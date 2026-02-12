# Comtrade ingestion and timeseries behavior

## Environment-driven year selection

- `COMTRADE_TARGET_YEAR` defaults to the previous calendar year.
- `COMTRADE_START_YEAR` defaults to `COMTRADE_TARGET_YEAR - 4`.
- Ingestion writes the same raw output CSV names used by the pipeline, while storing the actual selected vintage in `data/raw/comtrade_vintage.yml`.

## Latest-available fallback

For each Comtrade dataset, ingestion probes the requested `COMTRADE_TARGET_YEAR`. If no rows are returned, it tries each prior year (up to 5 years back) and picks the first year with data.

## Timeseries script usage

`96_pull_trade_timeseries.R` supports explicit `latest5` and refresh controls:

```bash
COMTRADE_API_KEY="<your-key>" \
Rscript scripts/96_pull_trade_timeseries.R \
  --country="USA" \
  --tech="Batteries" \
  --supply-chain="Midstream" \
  --years="latest5" \
  --refresh=true
```

If `--refresh=false` and the output already exists, the script reuses the existing file.
