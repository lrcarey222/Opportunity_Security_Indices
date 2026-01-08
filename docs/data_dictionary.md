# Data dictionary

The pipeline standardizes outputs to a tidy schema with the following columns:

| Column | Description |
| --- | --- |
| `Country` | Country name for the observation. |
| `tech` | Technology (e.g., Solar, Gas, Electric Vehicles). |
| `supply_chain` | Supply-chain stage (`Upstream`, `Midstream`, `Downstream`). |
| `category` | Thematic category (e.g., Trade, Production, Consumption). |
| `variable` | Specific metric name within a category. |
| `data_type` | `raw` or `index`, indicating pre- or post-normalization. |
| `value` | Numeric value for the metric. |
| `Year` | Observation year (when applicable). |
| `source` | Source dataset or citation. |
| `explanation` | Brief calculation or interpretation notes. |
