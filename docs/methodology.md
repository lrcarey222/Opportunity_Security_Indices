# Methodology

This document states the method of the Opportunity Security Indices as the code implements it (the code is the specification; file references point to it). The README has category-by-category notes, run instructions and the changelog of the October 2026 corrections.

Outputs:

- **Energy Security (ES)**: exposure and resilience across energy supply chains (higher = more secure).
- **Economic Opportunity (EO)**: market and competitiveness opportunity across the same chains.
- **Policy index**: IEA Policies and Measures and Climate Action Tracker support.
- **Strategic Index (SI)**: where a country should build.
- **Geopolitical alignment** and the **two-tier allied coalition** with the EU-27 as one coalition node.
- **Partnership Strength Index (PSI)**: partner j's value to the allied coalition, from bilateral components.
- **Allied Industrial Base** (section 10a): how the coalition absorbs a supply shock with existing trade, where new investment would close the rest, the emergent network of trade and investment relationships, country roles and the Sectoral Compacts. It replaces the original **allied network design** (section 10), retired on 2026-10-17.

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

- **Trade (ES and EO).** One builder (`R/categories/trade/trade_core.R`); market share, feasibility and net imports come from the Atlas of Economic Complexity (HS92, reconciled, newest year on disk) with WDI GDP, export size, Balassa RCA and the export HHI from UN Comtrade reporters' 2024 exports of the basket (`comtrade_energy_trade.csv`: the World totals of the all-reporter pull, every basket code, 142 exporting reporters; RCA denominator `comtrade_total_export.csv`); each theme keeps its own components. The HHI index is S(−HHI) across the cells of the 11 headline technologies (`config/coalition.yml` `headline_techs`; 18 cells with a basket since 2026-10-11); a cell of an unscored technology (Semiconductors Upstream or Midstream) gets the value it would take if added alone to them (`median_scurve_ref()`), so it cannot move a scored cell. Re-export hubs (`config/index_definition.yml` `trade: re_export_hubs`: Hong Kong, whose Comtrade exports are mostly re-exports) are left out of the Comtrade measures (exports, imports, RCA, export size, HHI) and of their world aggregates; they keep their Atlas variables, and the bilateral dyad matrix is not filtered. ES `Overall Trade Risk Index` = mean of S(−HHI) of the world basket export market, the country's share of world basket exports (non-exporters 0) and S(−net basket imports / GDP). EO `Overall Trade Index` = mean of market share, Balassa RCA, export size, Atlas feasibility (one row per HS4 heading) and S(−HHI). Rankings are across WDI economies within the cell.
- **HS6 basket.** `data/reference/energy_hs6_master.csv` maps HS6 codes to (tech, stage, sub-sector). A code is in a basket only if the technology is a dominant use of the product; others stay in the master with `excluded = TRUE` and a reason. Ores, concentrates, slag and scrap are Upstream, and a code is in one stage of a technology. Legacy HS codes whose successors are all in the basket bridge the HS92 Atlas and older Comtrade revisions (`legacy_hs`). Electric Grid Upstream's basket is the grid's copper and aluminium inputs (since 2026-10-09): copper ore (2603.00), blister (7402.00) and cathode (7403.11), bauxite (2606.00), alumina (2818.20) and unwrought aluminium (7601.10, 7601.20), kept with caveat because the grid is a major but not the dominant use of either metal; electrical energy (2716.00) is excluded. Switching and routing apparatus (8517.62) left Electric Grid Midstream on 2026-10-09 (telecommunications equipment, like the coaxial and optical-fibre cables; 28% of the cell's Comtrade 2024 exports). Each code is in this basket only (the Batteries rows of copper ore, bauxite and alumina are excluded), so no trade value is attributed to two cells. Since 2026-10-11 Electric Vehicles, Solar, Wind, Nuclear and Semiconductors Upstream have baskets: EV the battery minerals of Batteries Upstream (lithium minerals 2530.90, nickel, cobalt and manganese ores, natural graphite; shared) and the magnet rare earths; Wind the rare earths - monazite (2612.20, filed as a thorium ore), compounds (2846.90) and metals (2805.30), the latter two moved from Wind and EV Midstream (the HS has no other rare-earth ore code, so mined concentrates trade as compounds); Nuclear uranium ores (2612.10) and natural uranium compounds (2844.10, the heading of most yellowcake; moved from Midstream); Solar silicon metal (2804.69); Semiconductors unwrought gallium, germanium and indium (8112.92, moved from Midstream; HS92 bridge 8112.91). Codes in two technologies count fully in each cell.
- **Minerals Trade (ES).** For technology t, with w_mt the IEA base-year demand shares renormalised over the minerals with data: `critmin_market_share` = Σ_m w_mt S_m(X_im / Σ_j X_jm) (non-exporters 0) and `critmin_hhi` = Σ_m w_mt S(−HHI_m); the category is their mean. X_im is the exporter's own Comtrade declaration when it reports (including late reporters), else **mirror exports**: the sum of partners' reported imports from it, moved from CIF to FOB with an HS-chapter CIF/FOB ratio from importers filing both (pooled for chapters with fewer than 30 records). Aggregate reporters never count toward a mirror. The Comtrade pulls must have at least 50 reporters.
- **Uranium (Nuclear Upstream).** ES `Nuclear Reserves` = identified recoverable resources (RAR + inferred) at < USD 260/kgU (OECD-NEA/IAEA *Uranium 2026*, Table 1.2a); ES `Nuclear Production` and EO Nuclear Upstream size and growth = Red Book Table 1.18 mine production (2019–2024), completed with observed zeros over the EI and WDI economies. Tables with sources are in `data/reference/uranium_*.csv` (`R/categories/reserves/uranium.R`).
- **Reserves.** Oil, gas and coal: EI proved reserves (end-2020); the nine critical minerals: USGS *Mineral Commodity Summaries 2026* (end-2025; ver. 1.3 erratum for Brazil's rare earths applied; `R/categories/reserves/usgs_mcs.R`); uranium: Red Book. Absent from these tables = none recorded (`zero`); a reported zero ('—') scores 0; a holder USGS lists without a figure (NA, W, s) is missing in its mineral variable (scored 0, counted as imputed) and, like an unlisted mineral, counts 0 in the technology roll-ups. 'Other countries' and 'World total' rows are never holders.
- **Foreign Dependency, Production.** IEA supply: mining = Upstream, refining and processing = Midstream; mineral names harmonised across sheets; non-suppliers complete with zero supply. Clean-tech manufacturing capacity (ES `Market Share`, EO `Overall Market Share`): each entity's share of the IEA world total, the source's "Other" row included (a share over the listed entities alone overstated each of them by 6–28%; the index, a rank, is unchanged). EV Midstream (ES and EO): the IEA Global EV Outlook's five regions (China, European Union, North America, Other Asia Pacific, Rest of World), which partition the world, are copied to the EI economies in them: China; the EU flag; EI "North America"; the rest of EI "Asia Pacific"; every other economy, non-EU Europe included, to Rest of World (`foreign_dependency_ev_region()`). The terms are production, sales, share of world production and the net export ratio (production − sales) / sales (`net_export_ratio` raw, `net_export` index; higher = net exporter = less import dependent), percent-ranked over the copied rows and averaged. The IEA gives the 2035 announced capacity shares and the EV-region terms for the EU as one region, so every EU member carries the EU's value: members are not ranked apart on these variables (disclosed, not corrected).
- **Energy Imports (ES).** For oil, gas and coal (Upstream): the mean of S(balance), balance = production − consumption in EJ (raw `Production surplus/deficit`), and S(self-sufficiency), self-sufficiency = 100 × production / consumption (raw `Self-sufficiency`, NA where consumption is 0). Higher = less import dependent. Where EI has no production row for the economy and fuel (it lists fewer producers than consumers and counts the rest in "Other <region>" residuals), production is the EIA International Energy Statistics value, an observation (§ EIA fill below). Only a cell with no comparable EIA value (2025: Estonia coal) is NA; its index takes the `global_average` fill and counts as imputed (§3.2, §3.4). A reported zero stays 0. Fossil sums the fuels with a value and is NA only where none has one. Until 2026-10-11 an absent row was read as 0. (Until October 2026 the second term was published as `Import share` = self-sufficiency − 100; the ranks are the same, except that the old form split non-producers by rounding noise.)
- **Production (EO), mineral Upstream cells** (Batteries, Electric Grid, Electric Vehicles, Green Hydrogen, Solar, Wind). With Q_im(y) the IEA mined supply of mineral m by country i (0 where the IEA does not name i for m), Q_m(y0) world mined supply in the base year (named producers plus "Rest of world") and w_mt the IEA base-year demand shares renormalised over the minerals with supply data: P_it(y) = Σ_m w_mt Q_im(y) / Q_m(y0), y ∈ {y0, 2035}. Raw `production_latest` = P(y0); `size` = S[P(y0)]; `growth_abs` = S[P(2035) − P(y0)], a change in quantity in base-year world-supply units; `Overall Production` = S[2 size + growth_abs + h], with h = S[−Σ_m w_mt HHI_m] common to every country. Economies the IEA names for no mineral take the `global_average` fill. Solar Upstream is in effect copper (silicon and silver have no supply rows). The fuel cells (EI production; uranium: Red Book) are historical instead: `size` = S[latest production], `growth_abs` = S[5-year absolute change] and `Overall Production` = S[mean of size and the relative and absolute 5-year growth indices]. Oil, gas and coal production EI does not report for an economy it lists elsewhere takes the EIA value of the year (EIA fill below); generation EI does not report is 0.
- **EIA fill of EI fuel production** (Energy Imports, Production; `R/utils/eia_intl.R`). Where EI has no production row for an economy and fuel, X_EI(y) = EIA value of year y in EI's units, or of the latest EIA year in [y − 1, y] when EIA has not published y (2025 gas and coal: 2024). Oil (thousand b/d) = crude oil incl. lease condensate + natural gas plant liquids, EI's definition (crude, shale oil, oil sands, condensates, NGLs; no biofuels, CTL/GTL, refinery gain). Gas (EJ) = dry natural gas (bcm) × 0.036, the factor of every EI gas row. Coal (EJ) = coal (TJ, gross calorific value) × 10⁻⁶ × 0.95 (net/gross). Checked on economies both report (2020, 2024): median ratio of EI to the converted EIA value 1.006–1.011 oil, 0.97–0.99 gas, 0.99–1.05 coal (wide spread: EI uses its own calorific values). An EIA zero is 0, and so is an economy EIA covers but gives no production series for the fuel; a value EIA does not give as a number ("--", "NA", "ie", "w") is missing. Comparability: the value is not used when EIA's consumption of the fuel covers less than 10% of EI's and falls more than 0.01 EJ short, since EI then counts under the fuel something EIA's series exclude (2015–2025: Estonia coal only, where EI counts oil shale). EIA values are observed data, not imputations. **Year pairing (Energy Imports).** Where a fuel's production P is an EIA value of an earlier year y* than its consumption C (2025 gas and coal production from EIA 2024), self-sufficiency = 100 × P(y*) / C(y*), C(y*) from the consumption's own source (EI), and balance = C × (self-sufficiency / 100 − 1), C the consumption the cell uses; production thus enters as P(y*) × C / C(y*), the 2024 production scaled by the 2024–2025 consumption change (the 2024 self-sufficiency carried to 2025). Zero production is unaffected; a cell without C(y*) keeps its two years (none in 2025). 2025: 32 cells (22 gas, 10 coal). Until 2026-10-11 the two years were divided as they were (Slovenia coal 121% self-sufficient against 73.5% paired). Consumption of an earlier year than production (the producers EI lists without consumption: EIA 2024 consumption, EI 2025 production) is not paired: their observed 2025 production is kept and consumption is carried from 2024, since production moves far more than consumption there (2024–2025: Yemen oil −57%, South Sudan oil +53%); a user decision is open (README EIA open end (6)). The comparability check compares EIA's consumption in the EIA year with EI's in the cell's year; with EI's of the EIA year it decides the same in 2010–2025. The production theme (EO) has no production-to-consumption ratio and is not paired: a 2025 gas or coal cell from EIA 2024 has a 2024 level and a 2020–2024 change.
- **Energy Prices.** `Overall Price Volatility Index`: annualised volatility of monthly log returns of the IMF PCPS series linked to the technology, a world-level series per technology. The raw `latest_price` is in each commodity's own unit, from the IMF PCPS series metadata (`energy_prices_pcps_units()`), not the IMF label text, which is wrong for cobalt and propane and silent for lithium, vanadium, chromium and rare earths; an unverified unit reads "unit to confirm".
- **Investment.** Clean Investment Monitor flows, momentum, operating and pipeline capacity; critical-minerals investment split between Batteries and EV Upstream by IEA demand shares.
- **Technology Demand (EO).** Mean of one composite per source: IEA WEO global demand, BNEF NEO regional demand (or the IEA EV Outlook index for EV Downstream), BCG addressable market, BNEF overcapacity. The WEO terms, copied to every country, are named `2035`, `global_growth` (EJ), `global_demand_size`, `global_demand_growth` and `Overall Global Demand`; the BNEF terms `X2024`, `X2035`, `growth` (relative), `demand_size`, `demand_growth` and `Overall Demand`. `future_demand()` requires one row per (country, tech, stage, variable).
- **Cost Competitiveness (EO).** Mean of the BNEF LCOE index (current-year and 2050), the ILO/IMF input cost index and the IEA relative equipment cost. The capital leg takes the newest reading of the country's highest-priority IMF MFS_IR rate (lending; euro-area loans to non-financial corporations, new business, then outstanding; money-market; policy; deposit), held out if older than the 10-year floor. A zero or negative reading in a lending-type series is a placeholder and is dropped, with the IMF quarterly and annual averages that include it (`cost_competitiveness_drop_rate_stubs()`): New Zealand's lending series carries only zero entries after January 2017 (4.76%), and Solomon Islands' 2021 annual average folded in zero entries (5.18% against 10.3–10.4% in every reported month). Zero and negative policy, money-market and deposit rates are kept.
- **Technological Readiness (EO).** Each item of the IEA Clean Energy Technology Guide (2026 public dataset) is mapped to a (technology, stage) or classified out of scope by ordered rules (`config/iea_clean_tech_guide_tech_map.yml`). Level = bell on the end-year TRL on 1–11 (zero at ≤ 2 and ≥ 11, peak at 6), with TRL 9 items placed by the Guide's post-TRL-9 adoption stage (Awaiting adoption 9, Building momentum 10, Commercial in many markets 11); Momentum = (min(max(ΔTRL, 0), cap) / cap)^0.5 over the release's window. A cell's TRL Index = 0.7 mean Level + 0.3 mean Momentum over its items; a stage with no item takes the technology-wide value. TRL is one value per (technology, stage), broadcast to every country as an observed input.

## 5. Policy index

```
P_its = sum_v w_v x^v_its / sum_{v observed in the cell} w_v,   w: IEA PAMS 3, CAT 2
```

(`build_composite_policy_index()`, `R/indices/build_policy_index.R`.) IEA PAMS policies are matched to technology and stage by whole-word rules and weighted by jurisdiction; CAT ratings apply to all of a country's cells, and EU members without a rating take the EU bloc rating. **Eligibility:** IEA PAMS is required and CAT is added where it rates the country: a country is scored if IEA PAMS is observed for at least one of its headline cells, and one without a CAT rating is scored from IEA PAMS alone (the renormalisation above), its rows flagged `policy_basis = "IEA PAMS only"` (150 of 215 in the October 2026 build; 65 are `"IEA PAMS + CAT"`). A country rated by CAT but absent from IEA PAMS would not be scored (none is): CAT varies within a country only through the technology GHG index, which the SI already carries. Before 9 October 2026 both were required (65 countries); `OPSI_POLICY_REQUIRE_ALL=1` restores that rule. Because the SI ranks policy within a country, a country's basis does not move its values against another's, but the bases differ (median within-country Spearman 0.48 between IEA PAMS alone and the composite for the 65 with both), so a cross-country comparison of policy levels must keep them apart. **Excluded:** GTA NIPO (built and kept on disk, out of the composite for now) and the dual-use score (an LLM-elicited judgement identical for every country, so not a measurement of countries). The weights, required components, exclusions, eligibility rule and scored countries by basis are recorded in `policy_index_provenance`.

GTA NIPO is not a component. The NIPO tables on disk are the October 2026 refresh (as of 5 October 2026); they feed only the strongest-measures context of Country Insights and the paper's "NIPO added back" robustness variant, which gives NIPO its former weight 4 next to IEA PAMS 3 and CAT 2.

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
- **partner:** not core and either (a) a member of a U.S. FTA, IPEF, the MSP, the G7 or NATO (`data/reference/economic_security_agreements.csv`) with GDP ≥ USD 100bn, or (b) not China-leaning, with no GDP floor, and either a world share ≥ 5% in at least one flow measure (production, refining, manufacturing capacity, exports) of an Upstream or Midstream stage of a headline technology (route `supplier`) or ≥ 10% of world reserves of a headline mineral, fuel or uranium, Upstream (route `supplier: reserves`; user decision 2026-10-09: a stock is not yet supply). Flow measures: IEA mining and refining shares (copper, cobalt, lithium, nickel, graphite, magnet rare earths; a mineral counts for a technology taking ≥ 1% of its demand), EI mine production (manganese, zinc, PGMs), EI oil, gas and coal production, Red Book uranium production, IEA clean-tech manufacturing capacity, Atlas basket export shares. Reserves measures, from the Reserves theme's sources: USGS MCS 2026 mineral reserves at end-2025 (`usgs_reserves`, linked to technologies as for mining; shares of the larger of the rounded world total and the listed rows), EI oil, gas and coal proved reserves at end-2020 (`ei_reserves`, shares of each sheet's "Total World" row), and Red Book identified uranium resources < USD 260/kgU, as shares of the listed total. Each measure is tested at its own bar (`coalition_supplier_threshold()`; `major_supplier_shares_tbl.rds`).
- **Not in the coalition: Taiwan** (disclosed): no UN vote (neutral, so not core), in no agreement list, no WDI GDP, largest supplier share 1.5% (Solar Midstream exports) and no listed reserves; its semiconductor strength lies outside the headline technologies' baskets.
- **EU node:** the 27 EU members form one network node `EUU` (tier core) whatever their own tiers. Member weights per (tech, stage) (`eu_stage_weights_tbl.rds`): production (EI, Red Book) if it lists the members covering ≥ 50% of the EU total and the EU holds ≥ 0.1% of the world, else Atlas export shares, else GDP shares. Only the network is bundled; indices, SI selection and PSI stay national.

On the inputs of 2026-10-09: 31 core members and 26 partners (Brazil by the reserves leg alone: graphite 23.9%, manganese 16.7%, rare earths 14.6%, nickel 11.4%), 37 network nodes. All consumers read the coalition through `coalition_members()` (`R/indices/build_indices.R`).

## 9. Partnership Strength Index (PSI)

### 9.1 Components

Weighted means of the parts present (a missing part drops out; Friendshore and Opportunity less a CAT-based climate-policy penalty), S-curved within (tech, stage):

- **Friendshore** FS(i, j): reporter i the importer, partner j the supplier. Parts (weights): i's imports from j (2), i's ES need (2), j's EO (2), i's outward investment in j (2: the CM-FDI tie in (Solar, Wind, Batteries, EV) × Midstream, the IMF DIP position elsewhere; 9.3, 9.4), `geo_alignment` (2), `chinese_parent_exposure` (1, scored 1 − share; 9.4), `econ_opp_raw` (0).
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

### 9.4 Cleantech manufacturing FDI (CM-FDI)

**Source:** the Cleantech Manufacturing FDI Dataset (P. Bandara and the Net Zero Industrial Policy Lab, Johns Hopkins University; edition 2026.10.05; CC BY 4.0; `data/raw/cm_fdi_projects.csv`), a project-level census of 1,042 cross-border greenfield and expansion projects in solar, wind, battery and EV manufacturing. Settings: `config/partnership.yml` `cm_fdi`; code: `R/themes/partnership_strength/cm_fdi.R`. It enters Friendshore only; Economic Opportunity, Opportunity and Development do not read it.

**Projects used** (905; `cm_fdi_flag_index_use()`, reason in `index_exclusion`):
- industry activity "Manufacturing" (R&D, design and logistics projects, and the 3 "R&D & Manufacturing" ones, are descriptive only);
- status Announced, Under Construction, Completed or Operational, the CM-FDI working paper's exclusion of Cancelled, Paused and Closed; plants whose Operational field reads "Ceased operations" or "Paused" are dropped as well;
- home economy different from the destination;
- all announcement years (`announced_from: null`). Both measures are stocks of standing and pipeline capacity, like the IMF position they replace; closed plants are already removed; a 2015 cut would drop 131 standing plants, 116 of them non-Chinese, and raise measured exposure mechanically (robustness run).

Technologies map to Batteries, Solar, Wind and Electric Vehicles, all at Midstream. A joint venture counts `jv_parent_share` = 0.5 to its listed parent in counts and values; the partner's half is not attributed, because CM-FDI does not code the partner's economy. Countries go through `country_name_to_iso3()` (EU members stay individual; HKG and TWN are their own economies).

**Investment tie** (i = the parent's home economy = reporter, j = destination = partner; the direction of i's IMF outward position in j):
- stage 1: within (tech, stage), over dyads with a project, the mean of the S-curves of the attributed project count and of the attributed disclosed value (value missing where no project of the tie discloses one);
- stage 2: S-curve within (reporter, tech, stage), with the partners without a project as one tie block below every partner with one;
- a reporter with no project abroad in the cell has no tie (NA), and the component drops out of its FS mean. No conduit rule applies.

**Chinese-parent exposure** of supplier j in technology t: E_jt = the share of j's foreign-invested manufacturing whose parent is headquartered in China, Hong Kong or Macao (Taiwan not included), the same for every reporter.
- Weighting (`exposure_weighting: blend`): with a the attributed weight of a project (1, or 0.5 for a JV), v its disclosed value, χ = 1 for a Chinese parent, A = Σ a over j's projects in the cell and D = Σ a over those with a disclosed value, E = [Σ_undisclosed a·χ + D · (Σ_disclosed a·v·χ / Σ_disclosed a·v)] / A: the disclosed projects carry their count share D/A, split among them by value, and each undisclosed project its own count share. With every value disclosed this is the value share, with none the count share; an undisclosed value is neither dropped nor imputed. `value` (the value share, else the count share) and `count` are the alternatives.
- E is defined only where j has at least `min_projects` = 2 project rows in the cell, JVs included (with one plant it is 0 or 1 by construction), and is otherwise missing, never 0. China, Hong Kong and Macao as suppliers get 1 (`china_supplier: exposed`), since their capacity is Chinese-held and their foreign-invested plants are non-Chinese by definition.
- FS scores it as 1 − E with weight 1 (geo_alignment 2; geo is 2/11 of the FS weights). It is a share, used without an S-curve, as geo_alignment is.

**Rankability:** both parts count as observed where defined, and as not part of the cell where undefined (a census has no gaps to impute); a missing IMF position still counts as unobserved in the other cells.

**Switches:** `investment_ties: false` restores the IMF measure in every cell; `chinese_parent_exposure: false` makes the component NA everywhere (the fsi_raw of weight 0); with both off the file is not read.

**Rationale:** E is the parent-company case of the common-mode risk in section 7.4 of the NEIS working paper *Allied Industrial Base: Conceptual overview, literature review and analytical design*: two suppliers in two countries do not give true diversification if they depend on the same parent company. A partner whose foreign-invested plants belong to Chinese-headquartered firms adds less independent supply than its location suggests.

**Limitation:** E describes the foreign-invested part of j's industry only, not its whole capacity (Korea Batteries 0.91 although its battery industry is mostly domestic); HQ and status are as CM-FDI codes them.

### 9.5 Composite, views and rankability

```
C^k_j = mean over the view's reporters i != j of the dyad top-5 score (Development: j's own)
PSI_j = sum_k w_k C^k_j / sum_{k j has} w_k,   w: FS 0.4, OP 0.4, Dev 0.2
```

Views: **core coalition** (headline, `psi_tbl.rds`; reporters = core tier), **full coalition** (core + partner tiers) and **world** (all reporters). The dyadic PSI applies the same composite per (i, j).

A partner is **rankable** if it is sovereign and its PSI imputed weight share ≤ 0.5. The share is computed per cell over the partner- and pair-specific components (an imputed trade cell, a missing DIP position outside the CM-FDI cells (9.4), an assigned rather than voted bloc and a missing Development indicator count in full; the partner's ES/EO components count at their own pillar share; reporter attributes are left out), averaged over the top-5 cells, the view's reporters and the categories exactly as the score is.

## 10. Allied network design (retired)

Retired on 2026-10-17 and replaced by the Allied Industrial Base analysis (section 10a,
`docs/aib_methodology.md`). The design (`R/indices/allied_network_design.R`,
`scripts/30_build_allied_network_design.R`) spread each Upstream and Midstream stage's production across
the coalition's nodes by a lambda-calibrated MILP. It no longer runs in `run_pipeline.R`, the dashboard
no longer ships or recomputes it, and the paper no longer reports it; the code and its tests are kept,
dormant. Its method as it stood is in the git history of this section (before 2026-10-17).

## 10a. Allied Industrial Base and Sectoral Compacts

A separate layer on the observed bilateral trade (sub-sector nodes, shocks, existing vs optimized
partnerships, a specialization-resilience frontier, a marginal capacity simulation, the emergent
network and Sectoral Compact formation). It changes no index, and it replaces the allied network
design of section 10 (retired 2026-10-17). Method, settings and outputs: `docs/aib_methodology.md`.

## 11. Pipeline stages

1. **Ingest** (`scripts/05`): sync staged inputs, run API fetchers; HS6 views (`scripts/04`).
2. **Themes** (`scripts/10`): raw and indexed metrics per theme; policy components.
3. **Alignment and coalition** (`scripts/12`, `scripts/13`).
4. **Pillars, Policy, Strategic Index** (`scripts/20`).
5. **Partner themes and PSI** (`scripts/15`, `scripts/25`).
6. **Allied Industrial Base** (`scripts/31`, `scripts/32`; `scripts/84` for its dashboard data). The retired allied network design (`scripts/30`) no longer runs.
7. **Outputs and charts** (`scripts/80`, `scripts/paper/`, `scripts/85` for the dashboard bundle).
