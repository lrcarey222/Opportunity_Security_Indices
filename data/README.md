# Data staging

This repository separates data into three stages:

- `data/raw/`: Immutable source files exactly as received. Do not edit, clean, or rename once ingested.
- `data/processed/`: Cleaned, transformed, or derived datasets ready for analysis or modeling.
- `data/examples/`: Small, non-sensitive sample datasets suitable for tests, documentation, or demos.

## Naming conventions

- Use descriptive, lowercase, hyphenated names (e.g., `county-population-2023.csv`).
- Include dates or version markers when relevant (e.g., `labor-force-2024-01.csv`).
- Keep raw and processed filenames aligned so lineage is clear.

## Security and privacy

Never commit secrets, credentials, or any PII. If a dataset contains sensitive fields, keep it out of the repository and document how to obtain it separately.
