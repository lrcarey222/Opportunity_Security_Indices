# Economic Opportunity v2 migration

The economic opportunity v2 builder mirrors the energy security v2 flow (latest-year selection, missing-data policy, overall-variable definitions, and weighted aggregation). To enable it:

1. Set the config flag in `config/config.yml`:

```yaml
use_economic_opportunity_v2: true
```

2. Run the pipeline as usual. The wrapper in `build_economic_opportunity_index()` will dispatch to v2 when the flag is enabled.

If you need to compare results during migration, toggle the flag to switch between legacy and v2 outputs.
