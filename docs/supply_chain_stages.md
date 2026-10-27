# Six-stage supply-chain taxonomy (2026-10-26)

Replaces `supply_chain` = Upstream / Midstream / Downstream with six stages across the Opportunity,
Security and Partnership indices. Since 2026-10-26 the Energy Security and Economic Opportunity
theme tables, the Policy index and the Strategic Index are scored at six stages (scripts/10 and
scripts/20). The partner indices, the coalition and allied network, the AIB module, NIPO, the
dashboard exports and the paper figures still read three stages and are the next step (see
"Status" at the end).

| File | What it holds |
|---|---|
| `config/supply_chain_stages.yml` | The six stages (ids, labels, definitions, roll-up, literature crosswalk) and the Strategic Index stage weights |
| `data/reference/energy_hs6_master.csv` (`stage`, `enabling_equipment`, `serves_stage`) | A stage for every in-basket row (415 rows, 259 codes) |
| `R/utils/supply_chain_stages.R` | The stage names (`SUPPLY_CHAIN_STAGES`), roll-up and Strategic Index weights in R, and helpers to replicate legacy-labelled rows |
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
enters each sector's Strategic Index additively (0.1 × weight, `build_strategic_index()` in
`R/indices/build_indices.R`). It is an attribute of the stage, not a share that sums to one.

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
| Mineral Supply, production depth (IEA minerals) | Mining vs refining. Refined production, computed and then dropped in the three-stage build, is now kept as `Refining`. |
| Energy prices (IMF PCPS), Minerals Trade | Ore and crude series vs refined-metal series; ore vs refined vs scrap HS6 codes |
| PSI aid (OECD CRS) | The unused 5-digit `oecd_crs_purpose_crosswalk.csv` would make aid tech- and stage-specific |

Where the source has no finer grain, the variable can only be copied across stages:

- **Labour costs:** one ILO Manufacturing series serves `refining`, `components` and `assembly`. The ILO aggregate "Mining and quarrying; Electricity, gas and water supply" serves both `extraction` and `recovery` (ISIC E alone would be the sharper `recovery` proxy).
- **The capital-cost leg** (PPI, lending rate) is country-level.
- **CAT** and the cross-cutting part of **IEA PAMS** have no stage.
- **WEO/BNEF future demand** is copied to every stage by design: demand for a technology drives every stage of its chain.
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

## How each variable was staged (2026-10-26)

`data/reference/index_variable_stage_map.csv` holds the rule for each variable group. The choices
made in implementing it:

- **Trade (ES and EO):** each HS6 code is scored in its master `stage`, so the old baskets are split
  by their own codes, not copied. 20 tech x stage cells become 41 (Extraction 8, Refining 10,
  Components 10, Assembly 10, Recovery 3). Deployment has no basket, as Downstream had none. HS
  8541.40, the pre-2022 code for PV cells and modules together and the only one the HS92 Atlas panel
  has, is staged Assembly because modules are most of its value. `trade_concentration()` and
  `export_feasibility()` take `legacy_stages = TRUE` to rebuild the old three-stage baskets.
- **Minerals Trade:** each name-matched HS6 code is staged by what it is (2024 pull: 110 codes,
  Extraction 11, Refining 90, Recovery 9), and the HHI S-curve is taken within each stage.
- **Mineral Supply, production depth, reserves, energy imports:** mining, reserves and fuel
  production are Extraction; refining is Refining; electricity generation is Deployment.
- **Foreign Dependency, manufacturing shares:** battery cell capacity is Components; solar, wind,
  electrolyser and heat-pump capacity are Assembly. Electrolyser capacity is now named Green
  Hydrogen in both pillars (the Security side called it "Electrolyzers", which matched no index
  technology, so Green Hydrogen Assembly had no Security reading). EV production, market share and
  net export ratio are Assembly; EV sales are Deployment rows outside the score.
- **Energy prices:** each IMF series is Extraction (crude, gas hubs and LNG, coal, iron ore, U3O8)
  or Refining (refined metals, battery-grade lithium, silicon metal, rare-earth carbonate, propane,
  fertilisers). Prices remain global.
- **Investment:** capacity is staged by product (polysilicon Refining; wafers, cells, blades and
  electrode materials Components; modules, nacelles and vehicles Assembly; critical-minerals
  chemicals Refining). Manufacturing capex has no product column and is copied onto Components and
  Assembly; critical-minerals capex is Refining.
- **Overcapacity:** split by BNEF component, averaged within (tech, stage).
- **Technological readiness:** every rule of `config/iea_clean_tech_guide_tech_map.yml` carries a
  six-stage value. 37 of 66 cells take the technology-wide value.
- **Cost competitiveness:** the labour leg reads the ILO activity of each stage (above); the author
  share table and the country-level capital leg are copied onto the six stages; IEA relative costs
  are Assembly (battery cells Components, aluminium and steel Refining, ammonia Green Hydrogen
  Deployment).
- **Policy:** CAT is copied onto all six stages; IEA PAMS keyword rules assign each policy to one or
  more of the six stages, with cross-cutting policies spread over all six; dual-use scores expand
  from primary / secondary / tertiary. NIPO stays three-stage (it is built but not in the composite).
- **Strategic Index:** six-stage weights (above); the allied pick takes any stage but Deployment; the
  endowment screen applies to Extraction. Two rules replace the three-stage build's hand-picked
  exclusion of Geothermal - Midstream: manufacturing sectors no Energy Security theme measures
  (`strategic_unmeasured_sectors()`), and sectors whose Economic Opportunity rests only on inputs
  copied onto every stage (`strategic_unevidenced_sectors()`: Cost Competitiveness, Technology
  Demand and technology-wide readiness). At six stages they drop Geothermal Extraction, Refining,
  Components, Assembly and Recovery; Green Hydrogen Components and Recovery; and Coal, Gas, Solar,
  Wind and EV Recovery.

## Results against the three-stage build (2026-10-26)

Both builds were run on the same raw inputs (NIPO reused, coalition table reused). The Deployment
cells reproduce the Downstream cells exactly (Spearman 1.00 for every technology in both pillars),
which checks that nothing outside the stage logic moved.

- **Scored cells.** ES headline cells 6,252 to 11,830 (8,727 rankable); EO 7,131 to 14,188 (8,969
  rankable). Strategic Index 6,918 to 6,107 rows after the two rules and the constant-pillar check
  (Oil - Recovery, whose ES is the same for every country).
- **Rank agreement with the old cell** (median Spearman across technologies): EO Assembly vs old
  Midstream 1.00, Components 0.95, Extraction vs Upstream 0.91, Refining 0.85 vs Midstream and 0.82
  vs Upstream. ES Refining vs old Midstream 0.56 and ES Assembly vs Midstream 0.88: Security moves
  more, because refining dependence and finished-goods dependence are different exposures.
- **Allied picks (57 coalition members).** 32 keep their technology. Picks concentrate in Batteries:
  45 members (32 before), 42 of them Batteries - Assembly. Nuclear falls from 17 picks to 3. Stages:
  Assembly 42, Components 9, Extraction 6 (before: Midstream 43, Upstream 14). Batteries - Assembly
  is measured mainly by trade in finished lithium-ion batteries (HS 8507.60), which many countries
  both export and import, so it scores high on opportunity (mean within-country EO 0.77) and on
  need (0.68) at once; the old Batteries - Midstream basket was dominated by battery chemicals (EO
  0.39). Its margin is thin: across members it beats Batteries - Extraction by 0.013 SI points on
  average, less than the 0.021 the Assembly stage weight adds over an Upstream stage.

## Status

Not yet on six stages, and reading three-stage tables or labels:

- **Partner indices** (`scripts/15`, `scripts/25`, `R/themes/partnership_strength/*`): they join the
  six-stage pillar cells against three-stage partner trade cells, so they must move next. CM-FDI's
  `stage: Midstream` and the OECD CRS aid map are the two hard-coded stage assignments.
- **Coalition and allied network** (`R/indices/coalition.R`, `scripts/13`, `scripts/30`):
  `coalition_shares_trade()` filters Upstream / Midstream and finds no rows in the six-stage trade
  table. scripts/20 read the stored coalition table in the 2026-10-26 run.
- **AIB module and the Country dashboard** read `policy_outputs.rds` (`aib_pams_node()`,
  `country_pams_counts_all()`), now six-stage.
- **Dashboard exports and TypeScript** (`scripts/85`-`89`, `R/dashboard/*`, `dashboard/src`), NIPO,
  and the paper figures (`scripts/paper/*`: `fig_eo_transforms.R` asserts 33 cells,
  `fig_strategic.R` and `fig_robustness.R` call the SI with defaults that now need
  `R/utils/supply_chain_stages.R`).
- **Compatibility shims to remove when they move:** `legacy_stages` in the trade themes,
  `iea_critical_minerals_supply_stage(legacy = TRUE)` (the default, for `coalition.R`),
  `build_policy_table(stages = "legacy")`, `STRATEGIC_SUPPLY_CHAIN_WEIGHTS` and
  `STRATEGIC_EXCLUDED_SECTORS`, and the Upstream / Midstream defaults still accepted by
  `select_allied_strategic_sectors()`.

Open questions from the migration:

- Batteries Refining and EV Refining carry the same critical-minerals money split by a constant
  share, so they rank countries identically.
- Manufacturing capex is copied onto Components and Assembly (Solar Refining has capacity but no
  capex rows; EV Components takes vehicle-plant capex).
- Uranium: the U3O8 price is Extraction, while HS 2844.10 (natural uranium compounds) is Refining.
- Minerals Trade basket noise staged Refining (copper sanitary ware 7418.20, pipe fittings,
  zinc-coated steel) is left in the basket.
- The IEA PAMS keyword rules were tightened while splitting them (bare "plant" dropped,
  "processing" and "components" only in context, "refined" as a verb and vehicle "scrap" schemes
  excluded); 82.7% of policies keep their three-stage roll-up.
