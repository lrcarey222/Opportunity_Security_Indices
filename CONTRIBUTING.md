# Contributing

Thanks for your interest in contributing to the Opportunity Security Indices project. This repository is used to produce published index outputs; please keep changes focused and well-documented.

## Getting started

1. Install **R** (recent stable release) and the packages listed in the project documentation.
2. Open the project in RStudio (optional) using `Opportunity_Security_Indices.Rproj`.
3. Run scripts from the project root so relative paths resolve correctly.

## Tests

If tests are available, run them with:

```sh
R -q -e 'testthat::test_dir("tests/testthat")'
```

## Running the pipeline

At a high level, the pipeline is driven by `run_pipeline.R`. Review the script before running it locally and ensure required inputs are available. Do not commit raw or proprietary data to the repository.

## Methodology changes

Changes to index methodology, weighting, or analytical logic require:

- A clear issue or discussion describing the motivation.
- Documentation updates explaining the change and its impact.
- Agreement from project maintainers before implementation.

## Pull requests

Please keep pull requests focused, include a clear summary of changes, and note any validation you performed.
