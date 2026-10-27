# Electro-Industrial HS6 Dictionary

Status: **the node taxonomy of the Allied Industrial Base since 2026-10-19** (`config/aib_taxonomy_dictionary.yml`,
`scripts/31_build_aib_trade_network.R`, `docs/aib_methodology.md` section 2). The OSI indices still use the
energy basket `data/reference/energy_hs6_master.csv`; this file is copied to `data/reference/` for the pipeline.
The live trade basket is still `data/reference/energy_hs6_master.csv` (on `origin/main`: 507 rows,
306 codes, 92 rows excluded by the 2026-10 basket review).

| File | What it is |
|---|---|
| `data/reference/electro_industrial_hs6_dictionary.csv` | The dictionary: 370 rows, 318 HS2022 codes, RMI Electro-Industrial Stack (6 indices, 39 sectors) |
| `green_dictionary_comparison.csv` | Every code in NZIPL's Green Dictionary, our live basket, or the proposal (655 codes), with what happened to it and why |

Built 2026-10-18 from NZIPL's `Green Dictionary.xlsx` (591 rows) and our live master, against the UN
Comtrade HS1992–HS2022 nomenclatures and the UNSD HS2022→HS2017/HS1992 correlation tables.

---

## 1. How the two lists differ

| | NZIPL Green Dictionary | OSI live basket |
|---|---|---|
| Rows / distinct codes | 591 / 340 | 507 / 306 (259 after the review's exclusions) |
| Technologies | Batteries, Solar, Wind, Nuclear, Transmission, Magnets, **Heat Pumps, Electrolyzers, Geothermal, Biofuel** | Batteries, Solar, Wind, Nuclear, Electric Grid, **EVs, Semiconductors, Oil, Gas, Coal** |
| Row typing | Every row typed (Raw Material / Processed Material / Product Component / Process Equipment / Final Product) and staged (Upstream / Midstream / Downstream) | Upstream / Midstream only (358 Midstream rows, 57 Upstream) |
| Manufacturing equipment | 206 rows, 97 codes (cell lines, blade moulds, wafer saws…) | 11 of those 97 codes |
| HS revision | Mostly HS2017: 314 of 340 codes valid in HS2017; **23 not valid in HS2022** (21 obsolete, e.g. 854140 PV cells, 8462 machine tools; 2 that never existed: 262000, 282030) | HS2022, with 15 deliberate HS1992/HS2017 bridge codes (`legacy_hs`) plus one obsolete code (681510) |
| Repeats | 110 repeated (technology, code) pairs: one code per process step (847989 appears 7 times under Batteries) | One row per (tech, stage, sub-sector); codes shared across techs on purpose |
| Notes / provenance | Product label per row | Per-row notes with share-of-basket evidence and exclusion reasons |

**They agree surprisingly little.** For the five technologies both lists cover, the share of codes in common
(Jaccard) is: Batteries 0.28, Nuclear 0.22, Solar 0.18, Grid/Transmission 0.13, Wind 0.10.

The philosophies differ. The Green Dictionary maps a *supply chain*: every input, tool and material a
factory buys, however generic. Our basket maps *traded goods that signal a technology*, and the 2026-10
review dropped generic codes on purpose. Both are legitimate. They answer different questions, and the
Green Dictionary's approach breaks trade indicators when used naively.

## 2. Where the Green Dictionary improves our list

1. **Missing technologies.** Heat pumps, electrolysers and biofuels are absent from our basket, and the
   Green Dictionary gives a starting code set for each. Of its codes, 42 that we lacked are now in the proposal.
   Examples: refrigeration and heat-pump compressors 841430, reversible heat pumps 841581, packaged AC
   841582, heat pumps 841861, ammonia 281410, fuel ethanol 220720, PGMs 711011/711041, EVA encapsulant
   390130, silver ores and semis.
2. **Typing and staging.** Its Raw → Processed → Component → Final → Process Equipment scheme is the main
   structural idea we adopt (`item_type` and `stage` in the proposal). Our basket loses this information,
   and the 2026-10 Upstream baskets show we already need it.
3. **Process equipment as a dimension.** Most of its equipment codes are generic machinery (see §3), but
   the idea is right. The proposal keeps 12 equipment lines where the HS line is dedicated or predominant,
   or is the only line for a critical step:
   - semiconductor equipment 848610/20/40/90, wafer test 903082 and inspection 903141;
   - enrichment machinery 840120;
   - electrode drying 841939, calendering 842010 and tab welding 851580;
   - isostatic magnet presses 847983 and coil-winders 847981.

   The battery-line, magnet-press and coil-winding machines are extended scope: shared lines, kept as
   sensitivity rows.
4. **Upstream inputs we hadn't mapped**: silver for PV paste, tin (solder), tantalum, heavy water,
   depleted uranium, and uranium ore 261210 (our review has since added the ore).

## 3. Where it is weaker: 215 of its 340 codes were not adopted

| Reason (distinct codes) | n | Largest examples (2024 world exports) |
|---|---|---|
| Too generic | 189 | iron ore 260111 under Batteries/Nuclear/Transmission ($128bn); plastic articles 392690 under Wind ($80bn); steel articles 732690 ($56bn); machinery n.e.c. 847989 ($55bn); PCBs 853400 under Heat Pumps ($46bn); chemical preparations 382499 ($43bn) |
| Wrong mapping | 36 | vehicle gearboxes 870840 under Wind ($65bn); copper-ore row labelled nickel ore; tantalite under Magnets |
| Out of scope | 23 | crude petroleum 270900 under Geothermal/Transmission ($801bn); oilseeds and vegetable oils under Biofuel |
| Obsolete in HS2022 | 21 | refined petroleum 271000 under Geothermal ($536bn); 854380 under Electrolyzers ($54bn); 854140 PV cells (→ 854142/854143) |
| Never existed | 2 | 262000, 282030 (its LMO cathode line) |

The not-adopted codes carry about $2.8tn of 2024 trade, roughly the size of the whole energy basket.
Joined naively to Comtrade, they would make a Geothermal "market" larger than Solar, and make iron-ore
exporters Batteries leaders.

## 4. What the comparison exposed in our own list

Of our 175 non-fossil, non-bridge basket codes, the proposal keeps 156 and drops 19. The 2026-10 basket
review had already caught 32 of the codes the review agents would have removed. It excluded 28 of them:
router equipment 851762, carbon black 280300, commodity PE/PP resins and film, electricity 271600,
chemical preparations 382499, generic e-waste and smelter residues. It deleted the chapter-44 logs
(440321–440325) outright. The remaining issues are:

- **Codes dropped outright** (largest first): structures n.e.c. 730890 under Solar racking and Wind
  foundations ($53bn); LV connectors 853690 ($41bn); cords with connectors 854442 under every cable
  sub-sector ($40bn); LV plugs and sockets 853669 under EV charging ($22bn); aluminium structures 761090
  ($12bn); wire rope 731210 under Grid cables ($7bn); and battery "foils" that are actually PCB laminate
  (741021/741022), packaging foil (760719) or alloy foil (741012). Also 870490, a residual line since
  HS2022 moved BEV trucks to 870460.
- **Re-attributed, not dropped.** The Grid cable block is copied into Solar and Wind, and the steam-plant
  boilers into Nuclear. The proposal keeps these codes once, primary in Grid (C) or B, with secondary rows
  only where justified.
- **Obsolete code:** 681510 → 681511 (carbon fibre, now a wind-blade input).
- **No stage information**, and no heat pumps, electrolysers, process heat, data-centre power or
  manufacturing equipment.

Every decision, with its reason, is in `green_dictionary_comparison.csv` (`disposition`, `reason`, `replacement_hs6`).

## 5. The proposed dictionary

**Backbone:** RMI's Electro-Industrial Stack, the 6 indices and 39 sectors already used by `R/charts/eis_equities.R`
and the fantasy-draft game: A Materials & Inputs, B Electrotechnologies, C Grid, D Generation,
E Electrified End Uses, F Industrial Transition. Fossil fuels stay out; keep them as a comparator block in the master.

**One row per (code, sector).** Columns:

| Column | Meaning |
|---|---|
| `hs6`, `hs6_desc` | HS2022 subheading and its official text |
| `index_code`, `index_name`, `sector`, `segment` | EIS index, one of 39 sectors, and a finer label |
| `item_type`, `stage` | Raw Material / Processed Material / Component / Final Product / Process Equipment; Upstream / Midstream / Downstream (one value per code) |
| `specificity` | Relative to the row's sector. 1 dedicated (854143 PV modules), 2 predominant (850423 transformers >10 MVA), 3 shared (740811 copper wire rod). Tier-4 "generic" codes are not rows. |
| `scope` | `core` = specificity 1–2 (3 only with a written justification); `extended` = 3 |
| `primary` | Exactly one TRUE per code: the row used for non-overlapping totals |
| `source` | `both`, `osi`, `gd`, `new`, relative to our live basket |
| `osi_essential` | Carried from the live master |
| `hs1992_join`, `hs1992_relationship` | UNSD conversion for joins to the Atlas HS92 panel; join only where the relationship is 1:1 or n:1 |
| `notes` | Rationale; mandatory for specificity 3, new codes and translated codes |

**Aggregation rules this enables**, which also fix the double counting in the current crosswalk:

- Totals across sectors use `primary == TRUE` only.
- Sector "exposure" views may use all rows.
- Headline indicators use `core`; `extended` is for sensitivity runs.

**Size:** 370 rows; 318 codes (182 core, 136 extended-only). Rows by index: A 111, B 88, C 48, D 52, E 60, F 11.
Primary rows by source: 83 in both lists, 78 kept from OSI, 42 adopted from the Green Dictionary, 115 new.

| Idx | Sector | Core | Ext. | Main core codes |
|---|---|---|---|---|
| A | Critical minerals | 2 | 20 | 711041 Ir/Ru PGMs, 710610 silver powder |
| A | Battery minerals/refining | 16 | 9 | 750110 nickel matte, 253090 spodumene, 283691 Li carbonate, 282520 LiOH |
| A | Graphite | 2 | 2 | 380110 synthetic, 250410 natural flake |
| A | Rare earths | 3 | 2 | 284690 RE compounds, 280530 RE metals, 261220 monazite |
| A | Copper/aluminum | 8 | 14 | 260300 Cu ore, 740311 Cu cathode, 740811 wire rod |
| A | Electrical steel | 4 | 0 | 722511/722611 GOES, 722519/722619 NGO |
| A | Polysilicon | 1 | 3 | 280461 |
| A | Semiconductor materials | 3 | 9 | 381800 wafers, 710410 piezo quartz, 811292 Ga/Ge/In |
| A | Uranium/fuel | 13 | 0 | 284410, 284420, 840130 fuel assemblies |
| B | Logic/memory/analog chips | 11 | 1 | 8542xx ICs, 848610 wafer equipment |
| B | Power semis | 3 | 3 | 854129 IGBT/MOSFET/SiC, 854110, 854130 |
| B | Power electronics | 5 | 3 | 850440, 850490, 850450 |
| B | Controls/sensors | 3 | 11 | 903289, 903290, 854151 MEMS |
| B | Cathodes | 3 | 4 | 284190 NMC/NCA, 284290 LFP, 285390 pCAM (medium confidence) |
| B | Battery materials | 3 | 4 | 741011 Cu foil, 282690 LiPF6 |
| B | Cells/packs | 6 | 3 | 850760, 850720, 850790 |
| B | Stationary storage | 0 | 4 | not separable from 850760 in HS |
| B | Magnets | 2 | 2 | 850511 NdFeB/SmCo, 850519 |
| B | Motors | 15 | 2 | 8501xx, 850300 |
| C | Transformers | 7 | 6 | 850421–850423 |
| C | Switchgear/protection | 9 | 6 | 853620, 853720 (incl. GIS), 853590 |
| C | Cables/conductors | 6 | 8 | 854460 >1 kV, 854690, 761410 ACSR |
| C | HVDC | 0 | 3 | not separable in HS |
| C | Digital grid/GETs | 1 | 1 | 902830 meters |
| C | Microgrids/resilience | 0 | 1 | not separable in HS |
| D | Solar | 8 | 6 | 854143 modules, 854142 cells, 700719 PV glass |
| D | Onshore wind | 4 | 8 | 841290 blades/hubs, 850231, 730820 towers |
| D | Offshore wind | 0 | 2 | 890590 floating structures only |
| D | Nuclear reactor systems | 8 | 7 | 840140, 840120, 840110 |
| D | Geothermal | 0 | 2 | not separable in HS |
| D | Other firm/flexible power | 4 | 3 | 8410xx hydro turbines |
| E | EVs/drivetrains | 13 | 15 | 870380 BEV, 870360 PHEV, 870460 e-trucks, 871160 |
| E | Charging | 0 | 1 | not separable in HS |
| E | Heat pumps/cooling | 14 | 6 | 841510, 841430, 841581–83, 841861 |
| E | Industrial electrification/process heat | 9 | 0 | 8514xx furnaces, 851440 induction, 854511 EAF electrodes |
| E | Data-center power/cooling | 0 | 2 | proxy only |
| F | Electrolysers/H2 equipment | 1 | 1 | 854330 (shared with electroplating) |
| F | H2 derivatives and low-emissions fuels | 6 | 2 | 290511 methanol, 281410 ammonia, 220720 ethanol, 382600 biodiesel |
| F | Near-zero materials and carbon management | 1 | 0 | 720310 DRI/HBI |

## 6. What HS trade data cannot measure

Seven sectors have no core code because HS2022 has no line for them: Stationary storage, HVDC, Microgrids,
Offshore wind (beyond floating structures), Geothermal, Charging and Data-centre power/cooling. These
should carry a `measured / proxy / not measurable` flag in outputs rather than a thin, misleading trade
number. Electrolysers rest on one shared line (854330). Ammonia and methanol in F are today's fossil
products and serve as incumbency proxies. The cathode lines depend on national customs practice
(medium confidence).

## 7. HS2028 (in force 1 January 2028)

Checked against the WCO amendment text (NG0304Ba, Council Recommendation of 26 June 2025):

| HS2022 | HS2028 | Effect |
|---|---|---|
| 8415.10 | 8415.11 (HFC/HCFC) / 8415.19 (other) | Natural-refrigerant units become separable |
| 8415.81 | 8415.31 / 8415.39 (same refrigerant split) | Reversible heat pumps |
| 8418.61 | 8418.62 (HFC/HCFC heat pumps) / 8418.68 (other heat pumps) | |
| 8703.80 | 8703.81 (range-extender EVs) / 8703.89 (other) | Separate EREV from BEV |
| 8711.60 | 8711.61 e-bikes / 8711.62 kick-scooters / 8711.69 other | |
| 8413.70 | new 8413.71 photovoltaic pumps | New dedicated off-grid solar line |
| 8513.10 | new 8513.11 photovoltaic portable lamps | New dedicated off-grid solar line |

There are no amendments to 85.07 (accumulators), 85.41 (PV cells, semiconductors), 85.43 (electrolysers)
or 85.49 (e-waste). Those dictionary codes carry over unchanged. The WCO HS2022→HS2028 correlation
tables were not yet published at the time of writing.

## 8. Comtrade data for the new codes

Pulled on 2026-10-19 (UTC) with `scripts/98_pull_comtrade_energy_dyads.R` from `origin/main`, code
unchanged. It is a supplementary pull, the same method as the 2026-10-12 basket supplement. Full provenance
is in the pull's working folder (`NOTES.md`, outside the repository).

- **What was pulled:** 160 codes, covering imports and exports, all reporters × all partners, for 2020
  and 2024. The reporters are the main pull's (168 in 2020, 143 in 2024).
  - 155 are dictionary codes no existing pull held: 68 core, 87 extended.
  - 5 are HS2017 bridges for HS2022 splits: cadmium 8107.20/90, and zirconium 8109.20/30/90 for the
    nuclear-grade 8109.x1 lines.
- **Calls:** 33 keyed calls (one connection reset, retried) and 974,573 raw rows. No call hit the record
  cap. The combine and energy_trade stages made 0 further calls.
- **Coverage:** every code returned data. 37 codes have no 2020 rows because HS2022 created them:
  semiconductor sensors 8541.5x, electric furnaces 8514.1x/3x, HFC blends 3827.6x, hybrid trucks 8704.4x/5x,
  nuclear-grade zirconium, and others. The new codes carry $633bn of 2024 World exports ($229bn core, $404bn
  extended). The whole dictionary carries $3.3tn ($2.4tn core), counted in each code's primary sector once.
- **Outputs**, kept out of the live raw directory:
  - `comtrade_eid_dyads_all.csv` (outside the repository; `scripts/31 --dyads FILE` reads it): 2,959,925 rows. All 1,126 year × flow × code
    cells of the existing codes match the live dyad file exactly; the new codes add 911,255 rows.
  - `out/comtrade_energy_trade.csv` and `out/comtrade_total_export.csv`: country-level World rows for
    2024 and the RCA denominator. 56,003 keys are unchanged and 0 revised; 27,670 are new.
- **To use it in the dashboard/paper,** once the dictionary is adopted: add its codes to the basket the
  pipeline reads, then install these three files in `data/raw`. No further API calls are needed.

Values in `green_dictionary_comparison.csv` come from these refreshed World rows (Hong Kong re-exports
included), with Atlas HS92 for codes no pull holds.

## 9. Adoption path (decisions for the team)

1. Agree on the taxonomy (EIS 6×39) and the specificity/scope/primary rules.
2. Decide how the dictionary relates to the live master:
   (a) replace the non-fossil part of the master, or
   (b) run the two side by side as a sensitivity basket for the paper.
3. Then build the views (a pure function in `R/utils/`, IO in `scripts/`), add the validator tests, and
   re-run the trade themes.
