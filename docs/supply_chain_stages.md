# Six-stage supply-chain taxonomy (draft, 2026-10-26)

Replaces `supply_chain` = Upstream / Midstream / Downstream with six stages across the Opportunity,
Security and Partnership indices. The decisions below are settled; nothing in the pipeline reads
these files yet.

| File | What it holds |
|---|---|
| `config/supply_chain_stages.yml` | The six stages (ids, labels, definitions, roll-up, literature crosswalk) and the Strategic Index stage weights |
| `data/reference/energy_hs6_stage_map.csv` | A stage for every in-basket row of `energy_hs6_master.csv` (415 rows, 259 codes) |
| `data/reference/index_variable_stage_map.csv` | A proposed stage, fit, roll-up change and rule for every index variable group (51 rows) |
| `tests/testthat/test-supply-chain-stages.R` | Keeps the four in step with each other and with the HS6 master |

## Decisions (user, 2026-10-26)

1. **Upstream means materials, including refining.** `refining` rolls up to Upstream, so Midstream is
   manufacturing only (components and assembly).
2. **Scores are computed at the six stages.** The roll-up is for summaries, the Strategic Index
   weights, and tables that cannot be split.
3. **Strategic Index:** keep the three stage weights, and give each stage an equal share of its
   roll-up's weight (table below).
4. **Enabling equipment stays in `assembly`,** with the flag.
5. **Geothermal subsurface-drilling technologies are `deployment`** (site development).

## The stages

| # | id | Label | Rolls up to | SI weight | Literature (DOE 2022 / IEA ETP 2023 / UN BEC) |
|---|---|---|---|---:|---|
| 1 | `extraction` | Extraction | Upstream | 0.167 | Raw materials / Mining / Primary goods |
| 2 | `refining` | Refining & Processing | Upstream | 0.167 | Processed materials / Materials production / Processed intermediates |
| 3 | `components` | Component Manufacturing | Midstream | 0.375 | Subcomponents / Component manufacturing / Parts & accessories |
| 4 | `assembly` | Final Assembly & Equipment | Midstream | 0.375 | End products / Final assembly / Capital & consumption goods |
| 5 | `deployment` | Deployment & Use | Downstream | 0.25 | Deployment / Installation / (not traded) |
| 6 | `recovery` | Recovery & Recycling | Upstream | 0.167 | End of life / Recycling / Waste & scrap |

The literature labels were recalled, not re-read. Check them against the reports before citing them
in the paper.

**The SI weight changes how stages rank against each other, not just their scale.** `sc_weight`
enters each sector's Strategic Index additively (0.1 × weight, `R/indices/build_indices.R:286`,
`:329`). It is an attribute of the stage, not a share that sums to one.

- **Before:** an Upstream sector carried 0.50, twice a Downstream sector's 0.25.
- **After:** each Upstream stage carries 0.167, below `deployment`. The two Midstream stages carry 0.375 each.
- **Size of the effect:** the largest change is 0.1 × (0.50 − 0.167) = 0.033 SI points for an Upstream sector.
- **Alternative:** if the intent was for every stage to keep its parent's weight (each Upstream stage 0.50), change `split` and the stage values in the config.

## Tagging rules

- **Stage is relative to the technology.** A Li-ion pack is `assembly` for Batteries and `components`
  for EVs. Discrete semiconductors and ICs are the Semiconductors end product.
- **A traded good is staged by what its HS6 code is, not by the intent of its sub-sector.** The
  "Cathode Materials & Battery Precursors" codes are generic chemicals (MnO2, phosphates, carbonates),
  so they are `refining`.
- **`components` are parts that only work inside a finished good:** cells, wafers, magnets, blades,
  towers, gears, insulators, power semiconductors, and "parts n.e.c." headings.
- **`assembly` covers finished goods and stand-alone balance-of-system equipment:** cables and
  conductors, racking, foundations, transformers, switchgear.
- **Where a source separates cells from packs, cells are `components`.** HS 850760 does not separate
  them, so the trade map files it as `assembly`.
- **Enabling equipment is `assembly` with a flag.** Drill rigs, OCTG, liquefaction plant, enrichment
  machinery and PV/semiconductor fab tools carry `enabling_equipment = TRUE` and a `serves_stage`.
  43 basket rows carry it, nearly all in Oil and Gas.

## HS6 basket result

| Tech | extraction | refining | components | assembly | recovery |
|---|---:|---:|---:|---:|---:|
| Batteries | 5 | 31 | 1 | 2 | 7 |
| Coal | 5 | 2 | 4 | 10 | |
| Electric Grid | 2 | 14 | 10 | 28 | |
| Electric Vehicles | 6 | 2 | 18 | 23 | |
| Gas | 2 | 4 | 7 | 44 | |
| Nuclear | 1 | 2 | 5 | 11 | 1 |
| Oil | 1 | 7 | 2 | 41 | 2 |
| Semiconductors | | 6 | 6 | 9 | |
| Solar | | 5 | 13 | 29 | |
| Wind | 1 | 6 | 12 | 28 | |

**55 rows (48 codes) change roll-up, all from Midstream to Upstream:**

- Battery refining and precursor chemicals, lithium chemicals, foils, anode graphite, electrolyte and binder chemicals
- Electrical steel (GOES)
- Copper wire and SF6 inside the cable baskets
- Polysilicon and semiconductor materials
- Enriched uranium and coke
- Spent fuel, which goes to `recovery`

The refined goods already filed Upstream (alumina, refined copper, rare-earth compounds, silicon
metal, Ga/Ge/In, fuel oils, NGLs, natural uranium compounds) keep their roll-up.

**Batteries Midstream shrinks to three codes:** 850760 Li-ion accumulators, 850780 other
accumulators, 850790 parts. HS has no headings for cathode active material, separators or
electrolyte, so the cell-component step can't be measured from trade. Battery `components` will
lean on the capacity sources below (Clean Investment Monitor EAM and cells, BNEF component
overcapacity, IEA cell capacity).

## Index variables: where the finer grain already exists

Of 51 variable groups:

| Fit | Groups |
|---|---:|
| Map exactly (5 of them after the 2026-10-26 fixes below) | 25 |
| Can be split from data in hand | 11 |
| Span several stages, no way to split | 8 |
| Copies of one value | 4 |
| Inherit from the pillars | 2 |
| No stage | 1 |

The sources that already carry the detail:

| Variable group | Finer labels the source already carries |
|---|---|
| Trade (ES + EO), NIPO, PSI trade | HS6 code (the stage map above) |
| Investment: manufacturing capacity (Clean Investment Monitor) | Polysilicon / wafers / cells / modules; blades / nacelles; EAM / cells / modules; ZEVs |
| Overcapacity (BNEF) | Polysilicon, wafers, cells, modules; cathodes, anodes, separators, electrolytes, cells; nacelles; stacks |
| Technological readiness (IEA guide) | Item-level entries: chemistries, cell types, cables vs transformers, conversion vs enrichment vs fuel fabrication |
| Mineral Supply, production depth (IEA minerals) | Mining vs refining. Refining production is computed and then dropped at `production_depth_momentum.R:513`. |
| Energy prices (IMF PCPS), Minerals Trade | Ore and crude series vs refined-metal series; ore vs refined vs scrap HS6 codes |
| PSI aid (OECD CRS) | The unused 5-digit `oecd_crs_purpose_crosswalk.csv` would make aid tech- and stage-specific |

Where the source has no finer grain, the variable can only be copied across stages:

- **Labour costs:** one ILO Manufacturing series serves `refining`, `components` and `assembly`. Mining serves `extraction`, and ISIC E is the natural series for `recovery`.
- **The capital-cost leg** (PPI, lending rate) is country-level.
- **CAT** and the cross-cutting part of **IEA PAMS** have no stage.
- **WEO/BNEF future demand** is copied to every stage today. It belongs in `deployment` only.
- **BCG SAM** (US only) has only the three source columns.
- **Midstream annual investment** has no product column. It now spans Upstream (`refining`) and Midstream.
- **CM-FDI** projects have no product field. They span `refining` to `assembly`.

## Mislabels fixed in the current pipeline (2026-10-26)

These were filed in the wrong roll-up. They are fixed in the three-stage pipeline now, ahead of the
six-stage migration:

- **EV sales are Downstream rows.** They are no longer part of the EV Midstream composite or the
  Foreign Dependency overall (`foreign_dependency.R`, `market_share_manufacturing.R`,
  `config/index_definition.yml`). Demand is already scored under Technology Demand from the same
  IEA EV Outlook. The composite is now the mean of production, net export ratio and market share.
  The Sector dashboard still shows regional sales beside production on the EV Midstream cell,
  because it explains the net export ratio; its dictionary entry no longer claims sales feed
  Foreign Dependency.
- **TRL vehicle, wind turbine and reactor designs are Midstream** (final assembly), not Downstream.
  Wind Downstream now takes the technology-wide value, and Nuclear Downstream is left with
  radioactive waste disposal only.
- **TRL recovery items are Upstream.** That covers fuel reprocessing (split out from the nuclear
  back end), battery reuse and passport, and the five cable-recycling items, which are newly mapped
  to Electric Grid.
- **TRL geothermal subsurface operations are Downstream.** Geothermal Upstream now takes the
  technology-wide value.

Rule changes are in `config/iea_clean_tech_guide_tech_map.yml`. The 2026 Guide still classifies
every item (full-Guide test run against the staged file).

**Reviewed and kept: IEA Cost index ammonia.** It stays Green Hydrogen Downstream. Stage is relative
to the technology: for Green Hydrogen, making ammonia is a downstream conversion of the hydrogen,
and the TRL map files hydrogen-based fuels production the same way. Filing it as `refining` would
contradict that.

The refining-as-Upstream decision clears three earlier flags: Critical Minerals investment,
refined-metal prices and Minerals Trade are now correctly Upstream. Two problems remain inside them:

- Batteries and EV "Upstream" investment is the same money split by a constant share, so the two cells rank countries identically.
- Minerals Trade's name regex catches noise such as copper sanitary ware.

## Migration surface

The theme builders and index assembly mostly group by `supply_chain` generically. The sites that
name or count the three stages:

- **R/config (about 140 sites):**
  - `R/indices/coalition.R` (13 sites).
  - `R/indices/build_indices.R`: SI stage weights, the Geothermal exclusion, allied picks.
  - `cm_fdi.R` and `config/partnership.yml`; `stronger_development.R`; `config/coalition.yml`.
  - Network stages in `scripts/30` and `scripts/85`.
  - `config/sector_dictionary.yml`: 35 cell slugs, 24 `link_stage` values.
  - Dashboard R builders: `DASH_STAGES`, `COUNTRY_STAGES`, the 3-bit stage mask in `sector_policy.R`.
- **Dashboard TypeScript (about 300 lines in 83 files):** the `Stage` union types, `STAGE_ORDER` and the three marker symbols.
- **Dead code to delete rather than port:** `couple_pillar_scores_by_hhi.R` is sourced and never called, and `partnership_strength_build_oecd_sector()` is never called.

Scoring at six stages has knock-on effects to plan for:

- **More cells.** They go from 33 to 66, and every within-(tech, stage) S-curve gets thinner. `recovery` and the Batteries `components`/`assembly` trade cells are the thinnest.
- **PSI top-5 crowding.** Components copied across stages (aid, CM-FDI, labour cost) can fill a dyad's top-5. Its top-5 aggregation should dedupe or cap per roll-up.
- **Within-reporter S-curves shift.** `es_need` and `econ_opp_raw` are ranked over all of a reporter's cells, so their distributions change.
