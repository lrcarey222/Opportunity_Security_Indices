# Documentation index

**Where is the current methodology documented?** The decision framework is in
[strategic_position.md](strategic_position.md), the Allied Industrial Base in
[aib_methodology.md](aib_methodology.md) and the six supply-chain stages in
[supply_chain_stages.md](supply_chain_stages.md). Everything listed under "Legacy" below is kept for history only.

The repository overview is the [top-level README](../README.md). The Explorer's in-app Methods page is
`dashboard/src/content/methods.md`.

## Current methodology (canonical)

| Document | What it covers |
|---|---|
| [strategic_position.md](strategic_position.md) | Need -> Prize -> Position -> Choice -> Policy alignment; the six choices, yards and fences; policy design |
| [aib_methodology.md](aib_methodology.md) | Allied Industrial Base: stress tests, coordination, investment hosts, node classes, Sectoral Compacts, strategic roles (read its drift note first) |
| [supply_chain_stages.md](supply_chain_stages.md) | The six stages, the legacy Upstream / Midstream / Downstream roll-up, migration status |
| [sources.md](sources.md) | Every raw input with vintage, cadence and owner (generated from `config/raw_inputs_manifest.yml`, which also holds each licence) |
| [yf_node_attributes.md](yf_node_attributes.md) | Product-market and learning evidence behind the yards (`data/reference/posture_node_attributes.csv`) |
| [data_dictionary.md](data_dictionary.md) | Columns of the processed tables (partly still in three-stage terms) |

## Results and change records

| Document | What it covers |
|---|---|
| [strategic_position_readout.md](strategic_position_readout.md) | Results and validation of the 2026-10-30 decision-framework run (the shipped bundle is newer: counts in [strategic_position.md](strategic_position.md)) |
| [decision_framework_report.md](decision_framework_report.md) | Change log and report of the 2026-10-29/30 framework changes |

## Modules built on the framework

| Document | Module |
|---|---|
| [country_briefs.md](country_briefs.md) | Country strategy briefs (`scripts/92`) |
| [development_partnerships.md](development_partnerships.md) | Development partnerships (`scripts/93`) |
| [partnership_memos.md](partnership_memos.md) | Partnership Memos (`scripts/34`) |
| [partnership_agreements.md](partnership_agreements.md) | Partnership agreements layer (`scripts/41`-`43`) |
| [us_trade_restrictiveness.md](us_trade_restrictiveness.md) | US trade restrictiveness by technology x stage (`scripts/28`) |
| [comtrade_ingestion_and_timeseries.md](comtrade_ingestion_and_timeseries.md) | Comtrade year selection and time series |
| [electro_industrial_dictionary/README.md](electro_industrial_dictionary/README.md) | The electro-industrial HS6 dictionary behind the AIB nodes |

Pilot records: [country_briefs_pilot_report.md](country_briefs_pilot_report.md),
[development_partnerships_australia_audit.md](development_partnerships_australia_audit.md).

## Quality control and expert review

| Document | What it covers |
|---|---|
| [QC_SYSTEM_DESIGN.md](QC_SYSTEM_DESIGN.md) | QC and expert-review design; section 1.5 lists known model and data issues |
| [QC_REVIEWER_GUIDE.md](QC_REVIEWER_GUIDE.md) | For reviewers using `/review` |
| [QC_ADMIN_GUIDE.md](QC_ADMIN_GUIDE.md) | Running a review campaign and `/review/admin` |
| [QC_DEPLOYMENT.md](QC_DEPLOYMENT.md) | Hosting, storage modes, release identity |
| [QC_RELEASE_CHECKLIST.md](QC_RELEASE_CHECKLIST.md) | Gate a release passes before publication (draft) |
| [QC_UX_REVIEW.md](QC_UX_REVIEW.md) | Review-flow UX notes and open decisions |
| [qc_audit/](qc_audit/QC_EVIDENCE_CATALOGUE.md) | Audit notes behind the QC design |
| [EXTERNAL_REVIEW_READINESS_AUDIT.md](EXTERNAL_REVIEW_READINESS_AUDIT.md) | State of the Explorer before external review (2026-11-23) |

Review content (cases, method cards, campaigns) lives in `config/review/` ([its README](../config/review/README.md)).

## Legacy (history only; not the current method)

| Document | What it was |
|---|---|
| [methodology.md](methodology.md) | Opportunity Security Indices method: Energy Security, Economic Opportunity, Strategic Index |
| [energy_security_index_v2.md](energy_security_index_v2.md) | Energy Security and Economic Opportunity index builders |
| [migration_eo_v2.md](migration_eo_v2.md) | Economic Opportunity v2 migration note (January 2026) |
| [package_selection_viz.md](package_selection_viz.md) | Package selection visualization on the legacy indices (February 2026) |
| [replication_log.md](replication_log.md) | UN General Assembly ideal-point replication log (March 2026) |
| [decision_framework_plan.md](decision_framework_plan.md) | Implementation audit of 2026-10-29 (historical) |
| [yards_fences.md](yards_fences.md) | The separate Draw the Fence site, not the Explorer |
| [archive/README_full_2026-11.md](archive/README_full_2026-11.md) | The previous long README (three-stage pillars, index formulas, full run notes) |
