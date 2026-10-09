# Methodology

This document states the method of the Opportunity Security Indices as the code implements it (the code is the specification; file references point to it). The README has category-by-category notes, run instructions and the changelog of the October 2026 corrections.

Outputs:

- **Energy Security (ES)**: exposure and resilience across energy supply chains (higher = more secure).
- **Economic Opportunity (EO)**: market and competitiveness opportunity across the same chains.
- **Policy index**: IEA Policies and Measures and Climate Action Tracker support.
- **Strategic Index (SI)**: where a country should build.
- **Geopolitical alignment** and the **two-tier allied coalition** with the EU as one network node.
- **Partnership Strength Index (PSI)**: partner j's value to the allied coalition, from bilateral components.
- **Allied network design**: a coalition-wide division of labour per technology and stage.

## 1. Unit of analysis

Metrics are computed at the **country × technology × supply-chain stage** level (a "cell"). Stages are `Upstream` (extraction, primary inputs, recycling to raw materials), `Midstream` (processing and manufacturing) and `Downstream` (deployment, generation, use). The headline technologies are Electric Vehicles, Nuclear, Coal, Batteries, Green Hydrogen, Wind, Oil, Solar, Gas, Geothermal and Electric Grid.

**Economies.** Every source is put on one canonical name per economy before anything is ranked (`standardize_country_names()`, `R/utils/country.R`): the WDI name of the economy's ISO3 code (Taiwan: `TWN`), matched through `countrycode` and a curated table. Aggregates, regions, residual "Other ..." rows and historical entities are dropped from every theme (`standardize_theme_types()`), and keys that two spellings of one economy share are collapsed (a conflict between two non-missing values warns).

## 2. Normalization: median-centred S-curve

Raw metrics become 0–1 indices with

```
idx = r^gamma / (r^gamma + (1 - r)^gamma),   gamma = 0.5
```

where `r` is the percentile rank (`median_scurve()`, `R/utils/scurve.R`). Country-level indices are ranked **across economies within a technology × stage cell**. A value that is one per cell (world-market HHI, TRL, world price volatility) sets the cell's level but cannot rank countries in it. Variables where less is better (HHI, net imports, volatility) are ranked on their negation.

## 3. From themes to pillars

### 3.1 Overall variables and categories

`config/index_definition.yml` defines each category's score variable and how each **Overall ... Index** is composed (a mean of component index variables). A component a country lacks drops out of the mean. The builders warn when a configured score variable is never produced or when a theme's own composite stands in for the configured one.

For each (country, tech, stage, category, variable, theme) the latest available year is used (`latest_by_group()`), and the year chosen is recorded (`diagnostics$year_provenance`).

### 3.2 Missing data: observed-only imputation

`config/missing_data.yml` keys a rule to a theme (or a variable or category; variable > category > theme):

- `zero`: absence is meaningful (reserves, mineral production, processing and trade, manufacturing market share, investment);
- `global_average`: the mean over the observed countries of the same (tech, stage, category, variable, theme) group.

A rule fills a country only where the source observes **at least one** country in that group; a group nobody is observed in stays `NA`. A theme without a rule stays `NA` (BNEF LCOE has none on purpose). Every filled value is flagged `imputed` with the rule applied (`apply_missing_policy()`, `R/indices/index_builder_core.R`).

### 3.3 Pillar aggregation: available case

Within each cell the pillar is the weighted mean over the **observed** categories:

```
I_c = sum_{k observed in c} w_k s_kc / sum_{k observed in c} w_k
```

A category with no score carries no weight; a cell with no observed category is `NA`. Weights (`config/weights.yml`):

| Category | ES | EO |
| --- | ---: | ---: |
| Foreign Dependency | 6 | 3 |
| Production | 6 | 3 |
| Consumption | 5 | 3 |
| Energy Imports | 4 | – |
| Reserves | 4 | – |
| Trade | 4 | 3 |
| Minerals Trade | 4 | – |
| Energy Access | 4 | 3 |
| Investment | 3 | 3 |
| Energy Prices | 2 | 3 |
| Technology Demand | – | 4 |
| Technological Readiness | – | 2 |
| Cost Competitiveness | – | 3 |

Category contributions and variable contributions share one denominator and both sum to the index (`compute_index_and_contributions()`). Builders: `build_energy_security_index_v2()` and `build_economic_opportunity_index_v2()`, the only pillar builders `scripts/20_build_indices.R` calls.

No supply-chain coupling is applied: `R/indices/couple_pillar_scores_by_hhi.R` exists but is not called, so the pillars are stage scores.

### 3.4 Imputed weight share and rankability

Each cell carries `imputed_weight_share`: the share of its index weight resting on imputed components, read off the variable decomposition (zero and average fills alike; `summarize_index_imputation()`).

A cell is **rankable** when (`flag_rankable_cells()`, `config/rankability.yml`):

1. the economy is sovereign: a Correlates of War state-system code (the 193 UN members plus Kosovo and Taiwan; Serbia and Kosovo from the COW v2024 list), so territories, SARs and the West Bank and Gaza are scored but not ranked (`is_sovereign_economy()`); and
2. `imputed_weight_share <= tau`, `tau = 0.5`.

Values are never changed; `rankable` and `rank_exclusion` are added. Cross-country rankings, top-N lists and the Strategic Index use rankable cells only. Calibration (`config/rankability.yml`): 0.5 is the smallest grid value that keeps a majority of sovereign cells in every pillar and stage; it raises the median within-cell rank correlation of EO under alternative imputation rules from 0.78 to 0.97 (available case) and from 0.61 to 0.78 (zero fill). Because `zero` fills count as imputed, excluded ES cells lean towards economies absent from reserve, production and investment tables.

## 4. Category construction notes

Sources and component lists are in the README; this section records the method choices.

- **Trade (ES and EO).** One builder (`R/categories/trade/trade_core.R`) on the Atlas of Economic Complexity (HS92, reconciled, newest year on disk) with WDI GDP; each theme keeps its own components. ES `Overall Trade Risk Index` = mean of S(−HHI) of the world basket export market, the country's share of world basket exports (non-exporters 0) and S(−net basket imports / GDP). EO `Overall Trade Index` = mean of market share, Balassa RCA, export size, Atlas feasibility (one row per HS4 heading) and S(−HHI). Rankings are across WDI economies within the cell.
- **HS6 basket.** `data/reference/energy_hs6_master.csv` maps HS6 codes to (tech, stage, sub-sector). A code is in a basket only if the technology is a dominant use of the product; others stay in the master with `excluded = TRUE` and a reason. Ores, concentrates, slag and scrap are Upstream. Legacy HS codes whose successors are all in the basket bridge the HS92 Atlas and older Comtrade revisions (`legacy_hs`). Electric Grid Upstream's basket is the grid's copper and aluminium inputs (since 2026-10-09): copper ore (2603.00), blister (7402.00) and cathode (7403.11), bauxite (2606.00), alumina (2818.20) and unwrought aluminium (7601.10, 7601.20), kept with caveat because the grid is a major but not the dominant use of either metal; electrical energy (2716.00) is excluded. Each code is in this basket only (the Batteries rows of copper ore, bauxite and alumina are excluded), so no trade value is attributed to two cells.
- **Minerals Trade (ES).** For technology t, with w_mt the IEA base-year demand shares renormalised over the minerals with data: `critmin_market_share` = Σ_m w_mt S_m(X_im / Σ_j X_jm) (non-exporters 0) and `critmin_hhi` = Σ_m w_mt S(−HHI_m); the category is their mean. X_im is the exporter's own Comtrade declaration when it reports (including late reporters), else **mirror exports**: the sum of partners' reported imports from it, moved from CIF to FOB with an HS-chapter CIF/FOB ratio from importers filing both (pooled for chapters with fewer than 30 records). Aggregate reporters never count toward a mirror. The Comtrade pulls must have at least 50 reporters.
- **Uranium (Nuclear Upstream).** ES `Nuclear Reserves` = identified recoverable resources (RAR + inferred) at < USD 260/kgU (OECD-NEA/IAEA *Uranium 2026*, Table 1.2a); ES `Nuclear Production` and EO Nuclear Upstream size and growth = Red Book Table 1.18 mine production (2019–2024), completed with observed zeros over the EI and WDI economies. Tables with sources are in `data/reference/uranium_*.csv` (`R/categories/reserves/uranium.R`).
- **Reserves.** Absent from the EI/USGS reserve tables = none recorded (`zero`); an unlisted mineral counts 0 in the technology roll-ups.
- **Foreign Dependency, Production.** IEA supply: mining = Upstream, refining and processing = Midstream; mineral names harmonised across sheets; non-suppliers complete with zero supply.
- **Energy Prices.** `Overall Price Volatility Index`: annualised volatility of monthly log returns of the IMF PCPS series linked to the technology, a world-level series per technology.
- **Investment.** Clean Investment Monitor flows, momentum, operating and pipeline capacity; critical-minerals investment split between Batteries and EV Upstream by IEA demand shares.
- **Technology Demand (EO).** Mean of one composite per source: IEA WEO global demand, BNEF NEO regional demand (or the IEA EV Outlook index for EV Downstream), BCG addressable market, BNEF overcapacity.
- **Cost Competitiveness (EO).** Mean of the BNEF LCOE index (current-year and 2050), the ILO/IMF input cost index and the IEA relative equipment cost.
- **Technological Readiness (EO).** Each item of the IEA Clean Energy Technology Guide (2026 public dataset) is mapped to a (technology, stage) or classified out of scope by ordered rules (`config/iea_clean_tech_guide_tech_map.yml`). Level = bell on the end-year TRL on 1–11 (zero at ≤ 2 and ≥ 11, peak at 6), with TRL 9 items placed by the Guide's post-TRL-9 adoption stage (Awaiting adoption 9, Building momentum 10, Commercial in many markets 11); Momentum = (min(max(ΔTRL, 0), cap) / cap)^0.5 over the release's window. A cell's TRL Index = 0.7 mean Level + 0.3 mean Momentum over its items; a stage with no item takes the technology-wide value. TRL is one value per (technology, stage), broadcast to every country as an observed input.

## 5. Policy index

```
P_its = sum_v w_v x^v_its / sum_{v observed in the cell} w_v,   w: IEA PAMS 3, CAT 2
```

(`build_composite_policy_index()`, `R/indices/build_policy_index.R`.) IEA PAMS policies are matched to technology and stage by whole-word rules and weighted by jurisdiction; CAT ratings apply to all of a country's cells, and EU members without a rating take the EU bloc rating. **Eligibility:** IEA PAMS is required and CAT is added where it rates the country: a country is scored if IEA PAMS is observed for at least one of its headline cells, and one without a CAT rating is scored from IEA PAMS alone (the renormalisation above), its rows flagged `policy_basis = "IEA PAMS only"` (150 of 215 in the October 2026 build; 65 are `"IEA PAMS + CAT"`). A country rated by CAT but absent from IEA PAMS would not be scored (none is): CAT varies within a country only through the technology GHG index, which the SI already carries. Before 9 October 2026 both were required (65 countries); `OPSI_POLICY_REQUIRE_ALL=1` restores that rule. Because the SI ranks policy within a country, a country's basis does not move its values against another's, but the bases differ (median within-country Spearman 0.48 between IEA PAMS alone and the composite for the 65 with both), so a cross-country comparison of policy levels must keep them apart. **Excluded:** GTA NIPO (built and kept on disk, out of the composite for now) and the dual-use score (an LLM-elicited judgement identical for every country, so not a measurement of countries). The weights, required components, exclusions, eligibility rule and scored countries by basis are recorded in `policy_index_provenance`.

## 6. Strategic Index

```
SI_its = 0.2 pol_its + 0.3 EO_its + 0.3 (1 - ES_its) + 0.1 lambda_s + 0.1 g_t
```

- `pol`: within-country median S-curve of the Policy index over the country's in-sample sectors;
- `EO`: the EO pillar as built;
- `1 − ES`: energy-security **need** (`energy_security_need`); the SI favours sectors where the country is least secure;
- `lambda_s`: Midstream 0.75, Upstream 0.50, Downstream 0.25;
- `g_t`: technology climate weight (`ghg_index`, from IPCC AR5 Annex III lifecycle medians; oil 840 gCO2eq/kWh from SRREN Annex II).

A cell is scored only if EO, ES and Policy are observed and both pillar cells are rankable; nothing is imputed, and `si_exclusion` records why a cell is out. Geothermal Midstream is excluded, and any sector in which a pillar is constant across the scored countries is excluded before the within-country S-curves (`strategic_index_degenerate_sectors()`). The allied selection gives every coalition member (EU members individually) its top Upstream/Midstream sector, Geothermal and Green Hydrogen excluded (`select_allied_strategic_sectors()`, `R/indices/build_indices.R`). An Upstream cell is eligible for the pick only where the country has a recorded resource base: a positive technology-level reserve or production value in the Reserves or Production theme (`strategic_resource_base()`, `flag_strategic_resource_base()`; the index is unchanged, the cell carries `resource_base = FALSE` and `pick_exclusion = "no resource base"`; `OPSI_SI_RESOURCE_SCREEN=0` turns the screen off). Each SI row carries its country's `policy_basis`; every coalition member has a Policy index, 11 on IEA PAMS alone.

## 7. Geopolitical alignment

(`R/geopolitical_distance/alignment.R`, `scripts/12_build_geopolitical_alignment.R`, `config/geopolitical_distance.yml`.)

1. θ_i = mean of Bailey–Strezhnev–Voeten **all-votes** UNGA ideal points over the latest 5 sessions (2019–2023; the series ends in 2023). Final-passage estimates are a sensitivity.
2. s_i = (θ_i − θ_CHN) / (θ_USA − θ_CHN), clamped to [0, 1].
3. **Quartile blocs** (after Gopinath et al., IMF WP/24/76): U.S.-leaning if s_i ≥ the 75th percentile of s over the other countries, China-leaning if s_i ≤ the 25th percentile, nonaligned otherwise. **Hard blocs** (tau = 0): p_i = 1, 0.5 or 0.
4. A_ij = p_i p_j + (1 − p_i)(1 − p_j) (`geo_alignment`): 1 same bloc, 0 opposite blocs, 0.5 with a nonaligned or neutral economy.
5. Economies without a vote (`data/reference/territory_sovereign_map.csv`): territories and SARs inherit their sovereign's p; Taiwan, Palestine and Kosovo are neutral (0.5). Assigned after the quartile cuts, so they do not move them.

The alignment coalition (`geo_coalition_tbl.rds`) is the U.S.-leaning bloc with own votes and latest WDI GDP ≥ USD 100bn.

## 8. Allied coalition and the EU node

(`R/indices/coalition.R`, `scripts/13_build_coalition.R`, `config/coalition.yml`.) A country's tier is the highest route it satisfies; all routes are recorded.

- **core:** U.S.-leaning bloc, own UN votes, GDP ≥ USD 100bn.
- **partner:** not core and either (a) a member of a U.S. FTA, IPEF, the MSP, the G7 or NATO (`data/reference/economic_security_agreements.csv`) with GDP ≥ USD 100bn, or (b) not China-leaning, with no GDP floor, and either a world share ≥ 5% in at least one flow measure (production, refining, manufacturing capacity, exports) of an Upstream or Midstream stage of a headline technology (route `supplier`) or ≥ 10% of world reserves of a headline mineral, fuel or uranium, Upstream (route `supplier: reserves`; user decision 2026-10-09: a stock is not yet supply). Flow measures: IEA mining and refining shares (copper, cobalt, lithium, nickel, graphite, magnet rare earths; a mineral counts for a technology taking ≥ 1% of its demand), EI mine production (manganese, zinc, PGMs), EI oil, gas and coal production, Red Book uranium production, IEA clean-tech manufacturing capacity, Atlas basket export shares. Reserves measures: the EI reserve sheets of the Reserves theme (minerals at end-2025, USGS data, linked to technologies as for mining; oil, gas and coal at end-2020, each to its own technology), as shares of each sheet's "Total World" row, and Red Book identified uranium resources < USD 260/kgU, as shares of the listed total. Each measure is tested at its own bar (`coalition_supplier_threshold()`; `major_supplier_shares_tbl.rds`).
- **Not in the coalition: Taiwan** (disclosed): no UN vote (neutral, so not core), in no agreement list, no WDI GDP, largest supplier share 1.5% (Solar Midstream exports) and no listed reserves; its semiconductor strength lies outside the headline technologies' baskets.
- **EU node:** the 27 EU members form one network node `EUU` (tier core) whatever their own tiers. Member weights per (tech, stage) (`eu_stage_weights_tbl.rds`): production (EI, Red Book) if it lists the members covering ≥ 50% of the EU total and the EU holds ≥ 0.1% of the world, else Atlas export shares, else GDP shares. Only the network is bundled; indices, SI selection and PSI stay national.

On the inputs of 2026-10-09: 31 core members and 26 partners (Brazil by the reserves leg alone: rare earths 22.7%, graphite 21.8%), 37 network nodes. All consumers read the coalition through `coalition_members()` (`R/indices/build_indices.R`).

## 9. Partnership Strength Index (PSI)

### 9.1 Components

Weighted means of the parts present (a missing part drops out; Friendshore and Opportunity less a CAT-based climate-policy penalty), S-curved within (tech, stage):

- **Friendshore** FS(i, j): reporter i the importer, partner j the supplier. Parts (weights): i's imports from j (2), i's ES need (2), j's EO (2), i's outward investment position in j (2), `geo_alignment` (2), `econ_opp_raw` (0).
- **Opportunity** OP(i, j): reporter i the exporter, partner j the market. Parts: i's exports to j (3), i's EO (3), j's ES need (1).
- **Development** D(j): partner-level, non-high-income partners only: WDI development, Doing Business and investment environment indices, j's ES and EO, OECD CRS supply-chain aid (absolute and per GDP), energy use.

Each dyad (or partner) is summarised by its **top-5** (tech, stage) mean over the headline technologies.

### 9.2 Bilateral trade

One reconciled exporter → importer flow matrix per (tech, stage) and year (2020, 2024) from the all-reporter Comtrade pull (`partnership_strength_build_trade_cells()`):

1. **importer first:** if the importer reports imports in the cell that year, its declarations are used for every exporter (0 where none recorded);
2. **mirror:** otherwise the exporter's declared exports × 1.06 (IMF DOTS CIF/FOB convention);
3. otherwise unobserved.

FS reads the import view, OP the export view. Trade index: S-curve of last-year level and growth (2:1) across trading dyads within (tech, stage), then within the reporter; observed zeros form one tie block below every trading partner; unobserved cells take the (tech, stage) mean (`trade_status`: trade / zero / imputed / no_data).

### 9.3 Outbound investment and conduits

Positions (IMF Direct Investment Positions, outward debt instruments) with a conduit financial centre are treated as unobserved on either side of the dyad: the ten pass-through economies of Damgaard, Elkjaer and Johannesen (NLD, LUX, HKG, VGB, BMU, SGP, CYM, CHE, IRL, MUS) and six offshore centres excluded before (BHS, BRB, TCA, IMN, GGY, CYP) (`config/partnership.yml`).

### 9.4 Composite, views and rankability

```
C^k_j = mean over the view's reporters i != j of the dyad top-5 score (Development: j's own)
PSI_j = sum_k w_k C^k_j / sum_{k j has} w_k,   w: FS 0.4, OP 0.4, Dev 0.2
```

Views: **core coalition** (headline, `psi_tbl.rds`; reporters = core tier), **full coalition** (core + partner tiers) and **world** (all reporters). The dyadic PSI applies the same composite per (i, j).

A partner is **rankable** if it is sovereign and its PSI imputed weight share ≤ 0.5. The share is computed per cell over the partner- and pair-specific components (an imputed trade cell, a missing DIP position, an assigned rather than voted bloc and a missing Development indicator count in full; the partner's ES/EO components count at their own pillar share; reporter attributes are left out), averaged over the top-5 cells, the view's reporters and the categories exactly as the score is.

## 10. Allied network design

(`R/indices/allied_network_design.R`, `scripts/30_build_allied_network_design.R`.)

- **Nodes:** coalition members, with the EU-27 as one node. A bundled node's producer inputs are stage-weighted means of its members' raw values (over members with a value); demand need is the GDP-weighted mean member need and size the summed GDP; edges to or from it are member edges weighted by the members' 2024 basket trade on that edge (else stage weights, else equal); edges inside the node are dropped; its portfolio cap uses the summed GDP. Designed node shares are split back to members by stage weight (derived).
- **Stages:** Upstream and Midstream of the 11 technologies (22 problems).
- **Producer score** a_p: EO and ES (weights 5/7 and 2/7; the policy term is off). **Demand:** 0.5 need + 0.5 size. **Edges:** W_pc = 0.5 FS(reporter c, partner p) + 0.5 OP(reporter p, partner c), a missing component counting 0.
- **Objective per stage (max):**

  ```
  sum_p a_p x_p + 0.5 sum_pc W_pc f_pc - lambda sum_p x_p^2
  ```

  with x_p production shares (sum 1) and f_pc flows. The MILP represents Σx² by perspective tangent cuts on a 0.01 share grid (error ≤ 2.5e-5 per producer).
- **Constraints:** ≥ 3 producers, shares in {0} ∪ [0.05, 0.40], every consumer served by ≥ 2 suppliers each with ≥ 10% of its supply, self-supply allowed.
- **Portfolio caps (headline):** a node may be in a stage's top 5 at most `cap` times over the 22 stages, `cap` from log GDP in [2, 10]; enforced iteratively (≤ 30 passes) by barring over-cap nodes; top-5 by strict rank. No caps is the robustness variant.
- **Solver:** MILP (ompr/ROI/GLPK) for every stage with ≤ 60 nodes, 300 s per stage; greedy fallback (water-filled shares under the same objective) recorded in diagnostics.
- **Calibration of lambda:** the smallest λ on the grid at which the median designed HHI over the 22 stages is at most the median observed export HHI of the network's nodes over the 14 stages with an HS6 basket (2024 reconciled trade, shares within the nodes, the EU counting extra-EU exports only; `allied_network_observed_hhi.csv`). Pick on the 36-node set of 2026-10-09: λ = 0.92 (target 0.1396 over the 13 basket stages then scored; designed median 0.1382; 0.91 gives 0.1433). The rule must be re-applied on each full rerun; `scripts/30` warns when it fails. The fixes of 2026-10-09 (Brazil as a 37th node; Electric Grid Upstream back in the target, 14 stages, median 0.1374 on the 36-node set) require a recalibration.

## 11. Pipeline stages

1. **Ingest** (`scripts/05`): sync staged inputs, run API fetchers; HS6 views (`scripts/04`).
2. **Themes** (`scripts/10`): raw and indexed metrics per theme; policy components.
3. **Alignment and coalition** (`scripts/12`, `scripts/13`).
4. **Pillars, Policy, Strategic Index** (`scripts/20`).
5. **Partner themes and PSI** (`scripts/15`, `scripts/25`).
6. **Allied network** (`scripts/30`).
7. **Outputs and charts** (`scripts/80`, `scripts/paper/`, `scripts/85` for the dashboard bundle).
