# Data dictionary

The pipeline standardizes outputs to a tidy schema with the following columns:

| Column | Description |
| --- | --- |
| `Country` | Country name for the observation. |
| `tech` | Technology (e.g., Solar, Gas, Electric Vehicles). |
| `supply_chain` | Supply-chain stage (`Upstream`, `Midstream`, `Downstream`). |
| `sub_sector` | Optional sub-sector label for themes that further split supply-chain stages. |
| `category` | Thematic category (e.g., Trade, Production, Consumption). |
| `variable` | Specific metric name within a category. |
| `data_type` | `raw` or `index`, indicating pre- or post-normalization. |
| `value` | Numeric value for the metric. |
| `Year` | Observation year (when applicable). |
| `source` | Source dataset or citation. |
| `explanation` | Brief calculation or interpretation notes. |

## Index definition metadata

`config/index_definition.yml` defines which Overall variables are used for category scores, how Overall variables are composed from component indices, and variable-level metadata used for validation. The configuration ensures category scoring is explicit and consistent across pillars.

## `sub_sector` usage

`sub_sector` is optional and controlled by the global `include_sub_sector` configuration flag. When disabled, outputs use `sub_sector = "All"` to avoid duplicate keys and ensure consistent aggregation across pipeline stages.
