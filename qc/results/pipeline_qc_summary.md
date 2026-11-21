# Pipeline data QC summary

Generated 2026-11-21 03:08 PST by `scripts/qc/run_data_qc.R`. Layer: **pipeline** (processed tables), not the shipped bundle.

- Pipeline table: `outputs/strategic_position_tbl.rds` modified 2026-11-17 23:16 UTC, md5 `5882f35f044ee996e42c949a776807e2`, 14454 rows
- Shipped bundle release compared against: `rel-9c8f1877b5c6` (`posture:2026-11-17T20:33:31Z@0496e7ba|country:2026-10-31T15:58:15Z@4bb2adc|sector:8f4910df5216fbdbcf73b0be919043a7|aib:2026-11-17T19:07:29Z|strategic:c1736c4a25c7a33e73d56466eac415e6|memos:afbccf4e0eaaa07cad3219618b5f88b176d85d416f04f50de5fee2fd983a9b8c`)
- Checks: 0 fail, 30 warn, 146 info

Passing checks are not evidence the analysis is right; they only say these defects were not found.

## Release alignment (can each shipped bundle be traced to these processed outputs?)
- [info] posture: bundle created 2026-11-17 20:33 UTC, processed output 2026-11-17 23:16 UTC; the processed output is newer than the bundle and reproduces its results (0 choice changes, 0 cells on one side only, max input difference 0.0005)
- [warn] aib: bundle created 2026-11-17 19:07 UTC, processed output 2026-10-30 19:47 UTC; the processed output predates the shipped bundle: the bundle was exported from another run and cannot be traced to this processed dir (outputs/aib/aib_node_classification.csv vs compacts.c5643da1.json: 7 of 77 node classes differ)

## Fails
- none

## Warnings needing a human explanation
- `numeric_share_sum_trade_benchmark_share` (strategic_position_tbl, n=35): trade_benchmark_share summed over the economies of a node exceeds 1 + 0.02 (double counting, or shares on different bases stacked); per-basis sums from trade_benchmark_source show which basis adds the excess
- `trade_shares_china_share_le_top` (strategic_position_tbl, n=490): china_share exceeds the top single supplier's share (exposure_basis: (none)=173, base=154, blend=163): china_share counts CHN + HKG + MAC (SP_CHINA_ISO3), top_supplier_share one exporter; on blend rows both are T12M blends but top_supplier_iso3 is the base year's
- `trade_shares_china_top_agree` (strategic_position_tbl, n=700): the top supplier is China but china_share differs from top_supplier_share by more than 0.005 (exposure_basis: (none)=222, base=145, blend=333)
- `class_excluded_rows_in_contender_set` (strategic_position_tbl, n=9): excluded rows (posture_exclusion) counted as contenders in 9 nodes: they set other economies' cp_contender_pct
- `class_exactly_on_threshold_cp` (strategic_position_tbl, n=15): cp_contender_pct exactly at its threshold 0.75 (inclusive rule: counts as high) (choices: preserve=8, watch=7); 92 decided cells within the borderline band 0.05
- `class_node_constant_prize_near_threshold` (strategic_position_tbl, n=1): nodes where every economy carries the same prize within 0.05 of the 0.60 threshold: the whole node flips together
- `numeric_theme_inf_nan` (reserves_tbl, n=1714): Inf or NaN values (a division by zero upstream); by variable: Overall Reserves Index: 1714
- `schema_theme_stage_vocabulary` (cm_fdi_projects_tbl, n=1042): column supply_chain: 1042 rows carry a legacy stage label (Upstream / Midstream / Downstream); 0 unknown stage labels
- `trade_mirror_vs_reported` (current_trade_dyads_tbl, n=7971): 7971 of 22615 exporter-reported dyad x code flows (same 12-month window, both sides >= 100000) differ from the partner's mirror imports by more than x2.0; median mirror/reported 0.977; 30.0% of matched value
- `hs6_known_misfiling_281290` (energy_hs6_master, n=3): HS 2812.90 is NF3 / SF6 (non-metal halides), not cabling; live basket rows filing it there: 3
- `prov_missing_licence` (raw_inputs_manifest, n=46): 46 of 72 external (api/manual) inputs have no licence (13 derived/generated inputs also lack it); filled for 26 of 85 entries
- `prov_licence_conflict` (sector_dictionary_vs_raw_manifest, n=4): dictionary source and raw-manifest entry for the same dataset disagree on reuse (62 linked pairs checked)
- `prov_licence_self_conflict` (sector_dictionary, n=3): dictionary sources whose licence field is permissive while their attribution / issues text names a non-commercial or restricted licence
- `tp_trade_without_production` (strategic_position_tbl x production evidence, n=83): 83 cells flagged trade_without_production (choices: (excluded)=2, catch_up=32, partner=4, preserve=4, secure=1, watch=40)
- `tp_entrepot_signature` (strategic_position_tbl x production evidence, n=56): 56 cells flagged entrepot_signature (choices: (excluded)=2, catch_up=24, partner=4, preserve=6, watch=20)
- `tp_trade_driven_position` (strategic_position_tbl x production evidence, n=267): 267 cells flagged trade_driven_position (choices: catch_up=159, partner=15, preserve=41, watch=52)
- `drift_aib_class_changes` (drift: AIB pipeline outputs/aib 2026-10-30 -> AIB bundle 2026-11-17, n=7): 7 of 77 AIB nodes change class (headline scenario)
- `drift_compact_membership_changes` (drift: AIB pipeline outputs/aib 2026-10-30 -> AIB bundle 2026-11-17, n=23): 23 of 25 compacts with the same node set in both releases change their minimum coalition
- `drift_compact_set_changes` (drift: AIB pipeline outputs/aib 2026-10-30 -> AIB bundle 2026-11-17, n=52): 31 compact node sets only in the previous release, 21 only in the current (compacts formed, dissolved or re-scoped)
- `drift_compact_id_reused` (drift: AIB pipeline outputs/aib 2026-10-30 -> AIB bundle 2026-11-17, n=28): compact IDs (AIB-C##) that name a different node set in the two releases: an ID is not a stable reference across runs
- `release_alignment_aib` (release_alignment, n=1): aib: bundle created 2026-11-17 19:07 UTC, processed output 2026-10-30 19:47 UTC; the processed output predates the shipped bundle: the bundle was exported from another run and cannot be traced to this processed dir (outputs/aib/aib_node_classification.csv vs compacts.c5643da1.json: 7 of 77 node classes differ)
- `golden_cases_fail` (golden_cases vs pipeline outputs/aib 2026-10-30 19:47 UTC, n=3): golden cases: pass=1, fail=3, missing=0, not_applicable=1. A fail on a table other than the release the case was pinned to is drift evidence between the two (release_alignment says which is newer); a fail on the pinned release means the case is out of date or wrong

## Trade vs production flags (`pipeline_trade_production_flags.csv`)
- entrepot_signature: 56
  - DEU Solar/Assembly (preserve): re-export candidate: export share 8.4% (comtrade), stage imports 0.96x benchmark exports, production / capacity measured as zero
  - SGP Gas/Components (preserve): re-export candidate: export share 6.7% (comtrade), stage imports 0.90x benchmark exports, no production / capacity source covers this stage; a known re-export hub
  - NLD Solar/Assembly (catch_up): re-export candidate: export share 6.3% (comtrade), stage imports 0.44x benchmark exports, production / capacity measured as zero; a known re-export hub
  - JPN Wind/Components (catch_up): re-export candidate: export share 5.9% (comtrade), stage imports 0.57x benchmark exports, production / capacity measured as zero
  - JPN Solar/Assembly (catch_up): re-export candidate: export share 5.4% (comtrade), stage imports 0.53x benchmark exports, production / capacity measured as zero
- export_exceeds_production: 28
  - MYS Electric Vehicles/Refining (catch_up): export share 22.8% is 4.4x the measured production share 5.1% (iea_mineral_refining:Rare Earths)
  - USA Electric Vehicles/Refining (secure): export share 17.7% is 3.4x the measured production share 5.3% (iea_mineral_refining:Rare Earths)
  - VNM Electric Vehicles/Refining (secure): export share 7.0% is 6.4x the measured production share 1.1% (iea_mineral_refining:Rare Earths)
  - BEL Gas/Extraction (secure): export share 6.3% is 17397.7x the measured production share 0.0% (production_share_of_reporters)
  - DEU Batteries/Refining (catch_up): export share 3.8% is 4.0x the measured production share 0.9% (iea_mineral_refining:Lithium)
- no_production_data_stage: 227
  - FRA Nuclear/Refining (preserve): export share 37.9% (comtrade); no production or capacity source covers Nuclear Refining: no production evidence (not imputed)
  - NLD Nuclear/Refining (catch_up): export share 30.6% (comtrade); no production or capacity source covers Nuclear Refining: no production evidence (not imputed)
  - CHN Nuclear/Assembly (preserve): export share 26.0% (comtrade); no production or capacity source covers Nuclear Assembly: no production evidence (not imputed)
  - CHN Oil/Assembly (preserve): export share 25.7% (comtrade); no production or capacity source covers Oil Assembly: no production evidence (not imputed)
  - CHN Electric Grid/Assembly (preserve): export share 24.3% (comtrade); no production or capacity source covers Electric Grid Assembly: no production evidence (not imputed)
- trade_driven_position: 267
  - FRA Nuclear/Refining (preserve): choice preserve, CP among contenders 0.95; export_position 0.91 is 100.0% of the 1 observed CP families' sum; no production / capacity source covers this stage
  - NLD Nuclear/Refining (catch_up): Catch Up anchored only on trade (direct_supply_share;direct_export_share); export share 30.6%; no production / capacity source covers this stage
  - CHN Nuclear/Assembly (preserve): choice preserve, CP among contenders 0.98; export_position 0.89 is 50.5% of the 2 observed CP families' sum; no production / capacity source covers this stage
  - CHN Oil/Assembly (preserve): choice preserve, CP among contenders 0.99; export_position 0.88 is 100.0% of the 1 observed CP families' sum; no production / capacity source covers this stage
  - CHN Electric Grid/Assembly (preserve): choice preserve, CP among contenders 0.98; export_position 0.87 is 53.9% of the 2 observed CP families' sum; no production / capacity source covers this stage
- trade_without_production: 83
  - DEU Solar/Assembly (preserve): export share 8.4% (comtrade) but production / capacity measured as zero (214 economies measured: CM-FDI (Net Zero Industrial Policy Lab); CP family manufacturing_capacity (GCIM operating capacity index); IEA Energy Technology Perspectives 2024 (manufacturing capacity))
  - POL Wind/Refining (catch_up): export share 6.5% (comtrade) but production / capacity measured as zero (218 economies measured: CP family production_position (Production/size: EI, IEA CM, Red Book); IEA Critical Minerals Database; IEA Critical Minerals Dataset (Total supply for key minerals))
  - NLD Solar/Assembly (catch_up): export share 6.3% (comtrade) but production / capacity measured as zero (214 economies measured: CM-FDI (Net Zero Industrial Policy Lab); CP family manufacturing_capacity (GCIM operating capacity index); IEA Energy Technology Perspectives 2024 (manufacturing capacity))
  - JPN Wind/Components (catch_up): export share 5.9% (comtrade) but production / capacity measured as zero (214 economies measured: CM-FDI (Net Zero Industrial Policy Lab); CP family manufacturing_capacity (GCIM operating capacity index))
  - ESP Wind/Refining (catch_up): export share 5.8% (comtrade) but production / capacity measured as zero (218 economies measured: CP family production_position (Production/size: EI, IEA CM, Red Book); IEA Critical Minerals Database; IEA Critical Minerals Dataset (Total supply for key minerals))

## Drift (`pipeline_drift_*.csv`)
- posture bundle 2026-11-17 (0496e7ba) -> pipeline 2026-11-17: 0 cells change choice
- AIB (pipeline outputs/aib 2026-10-30 -> AIB bundle 2026-11-17): 7 of 77 node classes change; compacts matched on node set: 23 membership changes, 52 node sets on one side only; 28 compact IDs reused for a different node set

## Golden cases (`pipeline_golden_case_status.csv`)
A fail on a table other than the release a case was pinned to is drift evidence between the two (see Release alignment for which is newer); a fail on the pinned release means the case is out of date or wrong.

- pipeline strategic_position_tbl 2026-11-17 23:16 UTC: not_applicable=3, pass=28
- pipeline outputs/aib 2026-10-30 19:47 UTC: fail=3, not_applicable=1, pass=1
- posture bundle 2026-11-17 (0496e7ba): not_applicable=3, pass=28
- AIB bundle 2026-11-17: not_applicable=1, pass=4
