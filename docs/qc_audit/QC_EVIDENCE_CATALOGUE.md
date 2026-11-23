# Evidence catalogue: case brief, neutral evidence snapshot and reasoning reveal

For the implementers of the NEIS UX brief (C:/Users/LCarey/Downloads/review.txt) in osi-worktrees/qcmain
(feat/qc-review). Read-only inspection, 2026-11-21. Every value below was read from the shipped bundles in
dashboard/public/data with node, or from the config / code cited by file:line.

Releases inspected:

| Bundle | Stamp | Files |
|---|---|---|
| Posture | created 2026-11-17T20:33:31Z, git 0496e7ba | posture/posture-core.f665dc19.json, posture/posture-detail-ISO3.*.json |
| AIB | manifest created 2026-11-17T19:07:29Z, meta.built 2026-11-17T10:57:20, no git commit | aib/compacts.c5643da1.json, aib/scenario-managed.feae9f60.json, aib/scenario-baseline.2b9a857b.json |
| Partnerships | memo USA_JPN generated 2026-11-06T17:50:45Z, as_of 2026-10-30, verified false | partnerships/manifest.json, partnerships/memo-USA_JPN.d98e5bec.json |
| Sector | created 2026-10-31T15:58:13Z, git 4bb2adc | sector/sector-meta.a1ee8228.osb (gzip OSB1: dict.vars, dict.sources, dict.hhi_refs ...) |
| Review cases | data_release rel-9c8f1877b5c6, built 2026-11-21 | review/cases.aib_qc_2027_round1.json |

Labels used in the fact tables:

- **RAW**: observed or externally sourced (trade value or share, production share, a projection quoted from a
  source). Safe before the blind answer.
- **RAW-DEF**: raw arithmetic on observed data, but it depends on a modelling definition (who counts as "trusted" or
  "coercive", which world-supply measure is used). Safe before the blind answer only if it is relabelled neutrally
  and the definition is shown as an assumption.
- **MODEL**: a percentile, score, class, label, threshold result or simulated outcome. Show it only after the blind
  answer. The one exception is a simulated quantity that defines the scenario itself (for example the supply that
  must shift under Managed Interdependence), shown with its assumption.
- **CURATED**: an expert attribute from a reference table (for example substitutability or reconstitution time). This
  is an assumption, not evidence. Show it only in the Assumptions panel.

---

## 0. Key findings in one page

1. **The current context fields leak model judgments in three places.** (a) AIB `physical_warning` appears only on
   LOW INTERVENTION nodes: 4 of 4 in this bundle, by rule config/aib.yml:142-144 ("a LOW INTERVENTION or
   COORDINATE class carries the warning"). Showing it before the blind answer reveals the class. (b) The compact
   `purpose` text is persuasive and presupposes action ("Build an allied battery chain ...", "Secure the nuclear fuel
   cycle ..."). (c) The compact fields `non_allied_relied_on` and `critical_external` are outputs of the coalition.
   They presuppose that a compact was formed. There are also jargon and definition risks in the posture fields
   `coercive_top`, `coercive_top_share`, `trusted_share`, `opposite_bloc_share` and `outlook`, and in CASE_012's
   `cp` (section 7).
2. **None of the posture cases is shown its true time horizon or scenario today.** The Explorer's horizon is
   2024→2035. The prize uses world demand under the IEA WEO 2025 Stated Policies Scenario (minerals: IEA Critical
   Minerals, Stated Policies). Deployment need uses WEO Current Policies 2024→2035 plus 2030 national targets, not
   "2035 targets". The evidence is 2024 trade blended 50/50 with the latest 12 months. AIB runs on a 2025–26 trade
   period (meta.tradePeriod), but three places still label it "2024" (section 8).
3. **The posture bundle carries everything a reasoning reveal needs.** The detail cell ships the trace, need, prize
   and its parts, cp and its families, the pathway, `choice_possible` and `pivotal`. With review/qc/choiceRule.ts
   this gives the decisive input and the neighbouring alternative for every branch (section 2D, worked out for all
   13 posture cases). Secure is the exception: the trace says only which route fired. The consequence and timing legs
   of the route are not shipped.
4. **The detail cell ships the raw evidence, and today's facts mostly show percentiles.** Examples: deployment
   notes with GW and TWh, world demand growth %, export values for 2025 against 2024, TRL, and the current-trade
   window per country. Five to eight neutral facts per kind are listed in sections 2B to 6B.
5. **The partnership case asks an ill-posed question.** The shipped "conclusion" is always the first box of the memo
   (`items[0]`, snapshot.ts:511-515, always "Build together" = kind `preserve`). The answer options are the six
   posture choice names, not the memo's four boxes. Section 6 proposes a per-node question.
6. **The Explorer has no single "bottleneck stage" output.** The most defensible derived answer is the set of
   stages holding a BUILD, COORDINATE or MANAGE node within one strategic compact. Each node belongs to exactly one
   compact (76 of 77 nodes). The lead stage is the first step the primary mission names, and missions list steps
   by lost US$. Dollar ordering mechanically favours downstream stages, and the reviewer must be told this
   (section 4).

---

## 1. Shared vocabulary and generic interpretation conventions

### 1.1 Names to use (never re-type)

| Item | Source | Values |
|---|---|---|
| Six stages | dashboard/src/lib/stages.ts:75-82 `STAGE_DISPLAY`; identical in posture-core `meta.stage_display` | Extraction · Refining & Processing · Component Manufacturing · Final Assembly & Equipment · Deployment & Use · Recovery & Recycling |
| Node phrase (posture) | posture-core `meta.phrases["Tech|Stage"]` | e.g. "battery components", "battery mineral mining", "grid metals refining" |
| Six choices | content/vocabulary.ts:243-250 `CHOICE_NAME`, :253-260 `CHOICE_ACTION`, :263-273 `CHOICE_DEFINITION` | Secure, Preserve, Catch Up, Partner, Accelerate, Watch. The brief's "Keep Open" is `CHOICE_ACTION.watch` ("Keep open: buy competitively, do not force localization"). The header rule (vocabulary.ts:16) says the names do not change, so show "Watch (keep open)". **This needs a NEIS decision.** |
| AIB classes | vocabulary.ts:452-458 `AIB_CLASS_NAME`, :459-466 `AIB_CLASS_DEFINITION` | Build capacity, Coordinate trade, Manage specialization, Low intervention, Not enough evidence |
| Scenarios | vocabulary.ts:470-474 `SCENARIO_NAME`, :481-487 `SCENARIO_DEFINITION`; data words `strategic.scenarios[].words`, `meta.scenarios[].words` | baseline "Today's network"; managed "Managed Interdependence"; decoupling "Major-supplier disruption". The strategic layer also has `partial` (China halved; no investment or coalitions). |
| Data states | vocabulary.ts:622+ `STATE` | `notAssessed`, `weakProductionEvidence` ("Production evidence is weak"), `dataLimitation` |

### 1.2 Generic interpretation rules (independent of the model's thresholds)

These rules phrase neutral facts before the blind answer. None of them uses an Explorer bar.

| Quantity | Plain-English rule | Authority / basis |
|---|---|---|
| HHI (0-1) | below 0.10 "unconcentrated"; 0.10-0.18 "moderately concentrated"; above 0.18 "highly concentrated". Also show the 2010 line (above 0.25 "highly concentrated, 2010 guidelines"). | US DOJ/FTC Merger Guidelines 2023 (Guideline 1) and 2010 s.5.3; EC 2004 horizontal guidelines para 19. **Already shipped**: config/sector_dictionary.yml:758-765 `hhi_refs`, in the sector bundle as `dict.hhi_refs`. Reuse it. |
| One supplier's share of a buyer's imports or of a market | 50% or more: "a majority from one supplier"; 25-50%: "one large supplier"; 10-25%: "one of several suppliers"; under 10%: "minor". Optional reference line: 65%, the EU Critical Raw Materials Act benchmark (Reg. (EU) 2024/1252, Art. 5(1)(d): no more than 65% of EU consumption of a strategic raw material from one third country). | plain-language convention; CRMA as an external anchor |
| Top-3 share | 75% or more: "three suppliers hold three-quarters or more" (descriptive only) | descriptive |
| RCA (Balassa) | under 1: no revealed specialisation; 1-2 weak; 2-4 medium; over 4 strong | Balassa 1965; Hinloopen & van Marrewijk 2001 classes |
| World share (production or exports) | state the number and, where shipped, the rank. Do not use the Explorer's 10% / 3% / 0.5% lines (config strategic_position.yml:355, :360, :472) before the blind answer. | descriptive |
| Growth 2024→2035 | convert to an annual rate: +301% ≈ 13.5%/yr; +309% ≈ 13.7%/yr; +185% ≈ 10%/yr; +176% ≈ 9.7%/yr; +42% ≈ 3.2%/yr; +38% ≈ 3.0%/yr; −1% ≈ flat | arithmetic |
| Export trend | (exports_current / exports_previous − 1), with the two 12-month periods named | arithmetic |
| TRL | the IEA's 1-11 scale. 9 or more means commercially available; 10-11 means integration at scale or proven stable. | IEA ETP Clean Energy Technology Guide |
| Imports / GDP | show the number as a share of GDP. Use no word for its size. The Explorer's 0.05%-of-GDP materiality line (strategic_position.yml:327) is a model threshold. | descriptive |

---

## 2. Posture cell: the Country review

### 2A. Brief scaffolding

**Identity parts** (all shipped):

- Country name: `posture-detail-ISO3.json .country`, or `core.countries[].name`
- Technology: `cells[].tech`
- Stage: `cells[].stage`, displayed through `meta.stage_display`
- Node phrase: `meta.phrases["Tech|Stage"]`
- Group tier: `core.countries[].tier` (core 31 / partner 28 / none 155). This is a coalition label, so do not show
  it before the blind answer.

Example: **Korea × Batteries × Component Manufacturing ("battery components")**.

**Decision question** (what the model decides: one of six choices for one country at one stage of one technology,
sp_choice_rule, R/strategic_position/decision_engine.R:160-182):

- Producing stages: "Over the next decade (to 2035), should {country} protect, keep and strengthen, build up, rely
  on trusted partners for, or leave to the open market its {phrase} (the {Stage display} step of {tech})?"
- Deployment & Use: "Does {country} need a major push to deploy {tech} at home faster than current policies imply
  (to 2030-2035)? This asks only about deployment, not about where the equipment is made."

**Decision context template** (2-3 sentences):

> This approximates a call a national economic-security or industrial-strategy team makes about one step of one
> supply chain. The objective is secure, affordable supply and lasting industrial value. It is not
> self-sufficiency: relying on trusted partners or leaving supply open can be the right answer. The analysis does not
> assess individual firms, the cost of specific programmes, or which policy instrument to use.

For Deployment & Use, replace the middle sentence with: "The question is whether the domestic energy system needs
much more of this technology. It says nothing about manufacturing it."

**True time horizon and scenario**, with evidence:

| Element | What the model actually uses | Where it says so |
|---|---|---|
| Prize (market part) | World demand growth **2024→2035, IEA WEO 2025 Stated Policies (STEPS)**. For the mineral Extraction and Refining nodes: the IEA Critical Minerals dataset, Stated Policies, `minerals_base_year` 2025 → 2035. | config/strategic_position.yml:488-497; per cell `prize.note`, e.g. KOR Batteries/Components "World demand +301% 2024-35 (WEO 2025 STEPS); capex +29% 2021-22 to 2024-25 (CIM)" |
| Prize (capex momentum, weight 0.25) | world clean-tech manufacturing capex, mean of 2024-25 against mean of 2021-22 (CIM) | strategic_position.yml:507-513 |
| Need (import shares) | UN Comtrade reconciled matrix **2024**, blended **50/50 with the latest 12 months** where monthly data exist. The window end varies by country: KOR to Dec 2025 (202512), JPN, USA, IND and DEU to Aug 2026, BEL to Jul 2026, CHN none (2024 only). | strategic_position.yml:150-152; per cell `current_trade.{source, window_end, weight}`; sector_dictionary.yml:207-217 |
| Position (exports) | trade benchmark **2022-2024** (`core.meta.benchmark_period`); momentum **Jan-Dec 2025 vs Jan-Dec 2024** (`meta.momentum_period`, `meta.momentum_previous_period`) | posture-core meta |
| Deployment need | Demand family: WEO **Current Policies 2024→2035** (`scenario_base_year` 2024, `scenario_year` 2035). Policy-gap family: **2030** national targets (Ember 2030 target tracker and approved plans; the `national_commitments` scenario). System family: current (2025) system constraints. A climate-aligned value (World NZE 2035 against Current Policies) is shown only as a labelled alternative and is never used. | config/deployment_need.yml:41-43, :51/:55/:59, :100-106, :178-180, :208-211; `core.meta.family_scenarios` = {demand: current_policy, policy_gap: national_commitments, system: current_policy}; `meta.scenario_labels` |
| Secure | no calendar horizon: "restore time" against "time to intolerable harm + stockpile cover", in months | strategic_position.yml:344-350; docs/strategic_position.md:764-768 |

**Recommended brief lines:**

- **Time horizon:** "Through 2035 (market outlook 2024-2035; today's trade, 2024 plus the latest 12 months)."
  Deployment & Use: "Through 2035 (demand), against 2030 national targets."
- **Scenario:** "Current trade and stated policies; no disruption is imposed." Deployment & Use: "Current policies,
  with 2030 national commitments."
- The posture model has **no supplier-disruption scenario**. Do not label a posture case "Decoupling".

### 2B. Neutral evidence facts

JSON paths are relative to `posture-detail-ISO3.json → cells[tech==T && stage==S]`, unless they start with `core.`
(posture-core) or `meta.`. Fact ids are those of review/qc/snapshot.ts:26-31 where one exists. New ids are marked
NEW.

| id | Label (neutral) | JSON path | Unit | Interpretation rule (generic) | Source | Vintage (where readable) | Kind |
|---|---|---|---|---|---|---|---|
| top_supplier, top_supplier_share | Largest supplier of {country}'s imports, and its share | `.top_supplier`, `.top_supplier_share` | ISO3, share | 1.2 single-supplier rule | UN Comtrade reconciled matrix (comtrade_matrix) | 2024 + latest 12 months: `.current_trade.window_end`, `.source` (own_monthly = the importer's customs; ember_fill = estimate) | RAW |
| china_share | Share of imports from China (with Hong Kong and Macao) | `.china_share`; base and latest: `.current_trade.china_share_base / _latest` | share | 1.2; show base → latest when they move | comtrade_matrix | as above | RAW |
| coercive_import_share (NEW) | Share of imports from China, Russia and economies on the other side of UN votes | `.coercive_import_share` | share | 1.2 | comtrade_matrix + UNGA ideal points (un_votes V39.0, sessions to 2023) | trade as above; blocs to 2023 | RAW-DEF: the bloc definition (strategic_position.yml:297-304) |
| opposite_bloc_share | Share of imports from the other UN-voting bloc | `.opposite_bloc_share` | share | 1.2 | as above | as above | RAW-DEF; NA for nonaligned importers by rule (strategic_position.yml:204-207) |
| imports_to_gdp | Gross imports of this step's goods, as % of GDP | `.imports_to_gdp` | ratio | number only (1.2) | comtrade_matrix, World Bank WDI | 2024 trade | RAW. The need measure itself is *net* imports / GDP, which is not shipped (provenance.ts:217) |
| home_share | {country}'s share of world supply of this step | `.home_share` (core `share`) | share | number; flag the measure | Depends on the stage: mining / refining shares (IEA Critical Minerals, base 2025; EI; USGS), else Atlas export share (data to 2024) (strategic_position.yml:310-318). **The measure used for a cell is not shipped.** | varies | RAW-DEF |
| benchmark_share, benchmark_rca, benchmark_exports | Share of world exports; RCA; exports (US$) | `.trade.benchmark_share / _rca / _exports`, `.trade.benchmark_source` | share, ratio, USD | RCA classes (1.2) | UN Comtrade exporter-reported (comtrade_reported, "2024 data"); Atlas for non-reporters | `meta.benchmark_period` "2022-2024" | RAW |
| export_trend (NEW) | Exports in the latest 12 months against the previous 12 | `.trade.exports_current`, `.trade.exports_previous`, `.trade.months_observed`, `.trade.momentum_quality` | USD | growth %, rule 1.2 | Comtrade monthly | `meta.momentum_period` "Jan 2025 - Dec 2025" vs `meta.momentum_previous_period` | RAW |
| coercive_top, coercive_top_share | **Rename**: "{Name}'s share of world supply" (show only when it is CHN or RUS; otherwise drop) | `.coercive_top`, `.coercive_top_share` | ISO3, share | 1.2 | world-supply measure as for home_share | varies | RAW-DEF. "Coercive" is relative to the importer (for China the "largest coercive supplier" is JPN, 9.8%). This is the Secure physical-path leg (strategic_position.yml:337). |
| trusted_share | **Rename**: "World supply held by economies in {country}'s UN-voting group (excluding {country})" | `.trusted_share` | share | 1.2 | as above + un_votes | blocs to 2023 | RAW-DEF: trusted-partner definition (MC_022). Feeds the resilience route (<0.5) and the delivery mode. |
| demand_outlook (NEW) | World demand outlook 2024-2035 | parse `.prize.note` (the % and the scenario words) | % and %/yr | 1.2 growth | IEA WEO 2025 pathways (iea_weo_pathways) or IEA Critical Minerals (iea_cm); CIM capex | 2024→2035; capex 2021-22 → 2024-25 | RAW |
| outlook | growing / flat / declining | `.prize.outlook` | label | use demand_outlook instead | | | MODEL (the ±5% rule, strategic_position.yml:497) |
| maturity (NEW) | Technology readiness (IEA scale 1-11) | `.prize.maturity` | TRL | 1.2 | IEA Clean Energy Technology Guide (iea_ctg 2026). The source is inferred from the sector dictionary `technology.trl`. **Verify.** | 2026 | RAW |
| home_market (level 2) | Home-market growth score | `.prize.home_market`, `.prize.home_market_basis` | 0-1 | | | | MODEL |
| deployment_trend (NEW, Deployment) | Installed capacity trend | `.deployment_trend`, e.g. "96.5 -> 96.9 GW (2020-2025)" | text | as written | Ember / EI | 2020-2025 | RAW |
| dn_demand_note (NEW, Deployment) | Demand outlook and observed growth | `.dn_demand.note`, e.g. "WEO Current Policies 2024-2035: 1.68%/yr; observed 2020-2025: 2.07%/yr; 815 -> 834 TWh" | text | as written | IEA WEO 2025 Current Policies; Ember | 2024→2035 | RAW (`.dn_demand.value` is MODEL) |
| dn_policy_gap_note (NEW) | Distance to the 2030 national target | `.dn_policy_gap.note` (USA nuclear: "not assessed: no data for this country") | text | as written | Ember 2030 target tracker | 2030 | RAW (value MODEL) |
| dn_system_note (NEW) | System constraints | `.dn_system.note` (e.g. "firm_capacity_need: 962.8 GW dispatchable for 517.4 GW average load (2025)") | text | as written | Ember / EI / WEO | 2025 | RAW (value MODEL) |
| dn_climate_aligned (level 2) | How much more a net-zero path deploys worldwide by 2035 | `.dn_climate_aligned` (unit ratio_vs_current_policies) | ratio | "+27% vs current policies, world" | IEA NZE / WEO | 2035 | RAW, context only (never used in the choice) |
| policy priority, support, protection | Current policy activity | `.policy.{priority, support, protection, protection_recorded, momentum, sources, n_sources}` | within-country percentiles | Level 2 only | NIPO (GTA), IEA PAMS | | MODEL. Policy never enters the choice, so it is not a leak, but it is anchoring. |
| substitutability, reconstitution_time, systemic_essentiality, operational_control | Node attributes | `.substitutability` etc. | words | Assumptions panel | config/security_route_rules.csv; posture_node_attributes.csv | | CURATED |
| need, import_dependence, import_hhi, china_bloc, trusted_alternatives | Need and its families | `.need`, `.import_dependence`, `.import_hhi`, `.china_bloc`, `.trusted_alternatives` | 0-1 percentile within the node | reveal only | | | MODEL (the raw import HHI is **not shipped**) |
| prize, market, node_prize, investment_momentum | Prize and its parts | `.prize.{value, market, node_prize, investment_momentum}` | 0-1 | reveal only | | | MODEL |
| cp, cp_families | Competitive Position and its families | `.cp`, `.cp_families.*` | 0-1 percentile | reveal only | | | MODEL |
| pathway | Credible pathway to build | `.pathway_credible`, `.pathway_basis` | yes/no/unknown + list | reveal only | | | MODEL |
| commons, option, cap, large-yard signals | Industrial commons, future option, market-only cap, commodity signals | `core.meta.yf_nodes["Tech|Stage|stage"].{commons, option, cap, largeYard, n, signals, unknown}` (columnar in raw JSON; decoded by posture/contract.ts decodeYfNodes) | | reveal / assumptions | posture_node_attributes.csv | | CURATED → MODEL |
| choice, reason, trace, choice_possible, pivotal, borderline, watch_reasons, confidence, partners, partner_reason, policy alignment | | as named | | reveal only | | | MODEL |

**Recommended 5-8 neutral facts per posture case** (producing stages):

1. Largest supplier of imports and its share, plus China's share of imports when that differs, with the 2024 →
   latest-window movement.
2. {country}'s own share of world supply, labelled with its measure, plus its share of world exports and RCA
   (2022-24, US$).
3. Export trend over the last 12 months against the previous 12.
4. Concentration of world supply: China's (or Russia's) share of world supply. Name it; do not call it "coercive".
5. Supply held by same-group economies: `trusted_share`, with the definition one click away.
6. Import scale: imports as % of GDP.
7. Market outlook: world demand growth 2024-2035, as % and %/yr, with the capex trend.
8. Optional: technology readiness (TRL).

Deployment cells instead use: the installed-capacity trend; the demand note; the 2030-target note (or "not measured
for this country"); the system note; and the climate-aligned ratio, labelled "context, not used".

The neutral values of the 12 producing-stage cases, read from the detail cells:

| Case | Largest import supplier | China share of imports | Imports/GDP | Own world-supply share | Export share / RCA (2022-24) | Exports 2025 vs 2024 | China/Russia share of world supply (own imports from that group) | Same-group partners' world share | World demand outlook | TRL | Import window | CP families (trade only?) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| CASE_001 KOR Batteries/Components | CHN 76.6% | 76.6% | 0.015% | 6.17% | 10.7% / 3.48 ($623m) | $691m vs $474m (+46%) | CHN 26.7% (85.6%) | 63.4% | +301% 2024-35 (WEO 2025 STEPS); capex +29% | 7.15 | 2024 + 12m to Dec 2025 | export, mfg capacity, investment, innovation |
| CASE_002 JPN EV/Refining | CHN 47.2% | 47.2% | 0.012% | 1.82% | 9.0% / 2.71 ($191m) | $178m vs $165m (+8%) | CHN 94.4% (88.5%) | 4.0% | +185% (IEA minerals STEPS); capex −13% | 9.12 | to Aug 2026 | export, production, mfg, investment |
| CASE_003 AUS Batteries/Extraction | GAB 45.4% | 5.9% | 0.001% | 16.00% | 44.1% / 25.2 ($8.2bn) | $3.2bn vs $3.3bn (−2%) | CHN 80.4% (6.4%) | **0.0%** (see 8.4) | +176% (IEA minerals STEPS) | 6.5 | to Aug 2026 | export, production, resource |
| CASE_004 USA Batteries/Refining | CAN 19.3% | 10.3% | 0.020% | 0.51% | 3.4% / 0.36 ($2.8bn) | $2.5bn vs $2.7bn (−5%) | CHN 94.4% (15.2%) | 4.0% | +176%; capex −13% | 9 | to Aug 2026 | export, production, mfg, investment |
| CASE_005 IND Solar/Components | CHN 58.2% | 58.7% | 0.189% | 1.01% | 0.8% / 0.37 ($1.1bn) | $1.5bn vs $1.3bn (+23%) | CHN 25.9% (58.7%) | 70.6% | +309% (WEO STEPS); capex +27% | 8 | to Aug 2026 | export, mfg, investment, innovation |
| CASE_006 CHN Solar/Components | JPN 22.4% | n/a (own) | 0.117% | 25.90% | 24.4% / 1.5 ($37.0bn) | n/a | "JPN 9.8%" (relative label; drop) | 31.7% | +309%; capex +27% | 8 | none (2024 only) | export, mfg, investment, innovation |
| CASE_007 DEU Solar/Assembly | CHN 19.6% | 19.7% | 0.822% | 8.44% | 8.4% / 1.07 ($41.7bn) | $44.4bn vs $41.2bn (+8%) | CHN 29.0% (21.3%) | 45.0% | +309%; capex +27% | 10.5 | to Aug 2026 | export, mfg, investment, innovation |
| CASE_008 JPN Grid/Assembly | CHN 51.6% | 51.7% | 0.212% | 2.88% | 3.2% / 0.96 ($10.7bn) | $10.0bn vs $9.8bn (+2%) | CHN 23.3% (59.2%) | 51.1% | +42% (WEO STEPS) | 9.42 | to Aug 2026 | export, innovation |
| CASE_009 KOR Nuclear/Refining | RUS 44.2% | 0.0% | 0.058% | 0.00% | 0.0% / 0.0008 | $0m | RUS 21.8% (50.4%) | 71.6% | +38% (WEO STEPS) | 8.2 | to Dec 2025 | **export only (trade only)** |
| CASE_010 BEL Oil/Extraction | NLD 98.4% | 0.0% | 3.100% | 0.01% | 0.0% ($9m) | $6m vs $10m | RUS 10.2% (0.2%) | 28.8% | −1% (WEO STEPS) | 9.25 | to Jul 2026 | export, production, resource |
| CASE_011 IND Batteries/Extraction | ZAF 49.0% | 1.8% | 0.037% | 0.00% | 0.2% / 0.12 ($40m) | $43m vs $34m (+27%) | CHN 80.4% (1.8%) | 14.7% | +176% (IEA minerals STEPS) | 6.5 | to Aug 2026 | export, production, resource |
| CASE_013 DEU Grid/Components | CHN 19.2% | 19.4% | 0.096% | 9.24% | 10.4% / 1.32 ($5.5bn) | $5.5bn vs $5.3bn (+4%) | CHN 28.4% (20.8%) | 41.2% | +42% (WEO STEPS) | 9.11 | to Aug 2026 | export, innovation |

CASE_012 (USA Nuclear, Deployment & Use):

- `deployment_trend` "96.5 -> 96.9 GW (2020-2025)"
- `dn_demand.note`: WEO Current Policies 2024-2035 electricity demand +1.68%/yr; observed 2020-2025 +2.07%/yr;
  nuclear output 815 → 834 TWh (2024-2035) in a 4,564 TWh system
- `dn_policy_gap.note`: "not assessed: no data for this country"
- `dn_system.note`: 962.8 GW dispatchable for 517.4 GW average load (2025)
- `dn_climate_aligned`: +27% vs current policies (world 2035)

**Data-quality warnings that can be computed today** (show them in the brief, not in an appendix):

| Warning | Test | Field | Count in this release |
|---|---|---|---|
| Production evidence limited: position read from trade only | every `cp_families` key starts with `export_position` | `.cp_families` | 2,277 of 8,084 producing cells that have a cp (e.g. CASE_009 KOR Nuclear/Refining) |
| Trade and production evidence disagree | \|home_share − trade.benchmark_share\| ≥ 0.05 and the larger ≥ 0.05 | as named | 74 cells (CHN Solar/Refining 46.8% vs 9.3%; AUS Batteries/Extraction 16.0% vs 44.1%; USA Batteries/Components 30.1% vs 13.7%) |
| World-supply measure not stated | always, at the producing stages, until the measure is shipped | `.home_share` | all |
| Import shares mix two periods | `.current_trade` present: "2024 blended 50/50 with 12 months to {window_end} ({source})"; absent: "2024 only" | `.current_trade` | 3,216 cells blended |
| China's import share moved sharply | `.current_trade.flag` china_up / china_down | | per cell |
| Older source | blocs from UNGA votes to 2023 (un_votes V39.0); capex 2021-22 → 2024-25; benchmark 2022-24 | sector_dictionary.yml:482-491 | all bloc-based facts |
| Missing need family | `.trusted_alternatives == null`; Deployment: any `dn_*.value` null / `dn_families_observed` < 3 | | 4,623 of 11,770 producing cells lack trusted alternatives; Deployment family pattern counts (demand / policy gap / system): 101 = 794, 111 = 386, 001 = 272, 100 = 198, none = 671 |
| Not assessed | `.reason` / not-assessed reason; render as `STATE.notAssessed` | | 4,216 producing ("too few comparable importers or no trade data") + 671 Deployment |
| Large own producer: import-mix measures switched off | `home_share >= 0.10` (the self-supply guard, strategic_position.yml:355, :476; MC_008) | | e.g. CASE_003 AUS (16%) |
| Prize capped by the "market size alone" rule | `meta.yf_nodes[...].cap == "market_only"`. Use the node record, not `prize == 0.59`: IND Batteries/Extraction is capped at 0.571. | | 25 capped nodes; 432 cells at exactly 0.59 |
| Need capped by the score floor | `need == 0.61` (score_floor, strategic_position.yml:486; MC_006) | | 45 cells |
| Stage evidence quality none/low | `core.cells.ev` ∈ {none, low} | | none 813, low 737 |
| Consistency flag | `core.cells.fl` | | review_strong_position_low_prize 11; review_top_quarter_below_floor 6 (CASE_005) |
| Possible transit hub | top supplier on a curated hub list (NLD, BEL, SGP, ARE, plus config HKG/MAC) with ≥ 50% of imports. **Do not automate this from the supplier's own world share**: that heuristic flags 1,972 of 6,742 cells. | `.top_supplier` | CASE_010 BEL Oil/Extraction (NLD 98.4%) |
| Curated attributes | `meta.yf_nodes[...].unknown` non-empty (signals not observed) | | per node |

### 2C. Analytical assumptions (Assumptions panel), each linked to a method card

| Assumption, in neutral words | Config / code | Method card |
|---|---|---|
| Vulnerability ("need") is a rank among ~200 economies within this step (a percentile), not an absolute level of dependence | strategic_position.yml:169-213 | MC_007 |
| Need weighs import dependence, the concentration of the country's own suppliers and imports from China and the other UN-voting bloc equally. Trusted alternatives count only where measured for 80% of cells. | :170-185, :480 | MC_022 (trusted definition) |
| A tiny need score cannot rank as high (capped at 0.61) | :486 | MC_006 |
| An economy holding 10% or more of world supply is not scored on the mix of its imports | :355, :476 | MC_008 |
| Who counts as a risky ("coercive") or trusted supplier: the other UN-voting bloc plus China (HK, Macao) and Russia; everyone else in the same bloc is trusted | :297-304 | MC_022 |
| Only Hong Kong and Macao are treated as re-export hubs (plus four excluded registries) | :203 | MC_014 |
| The value of the industry weighs market growth, industrial know-how ("commons") and future technology option equally | :538 | MC_011 |
| Market size alone never makes the value high (capped just below the bar unless know-how or option reach 0.5) | :543-545 | MC_002 |
| The value is set per step for every country (a whole-node prize), lifted by fast home-market growth but never lowered by a small home market | :501, :522 | MC_003, MC_012 |
| "Strong position" means the top quarter of the economies holding ≥ 0.5% of world supply, with a minimum capability floor | :458, :472, :475 | MC_004 |
| A strong position alone does not justify Preserve | decision_engine.R:160-182 | MC_001 |
| Mature, standardised, diversified (commodity) steps default to open supply unless the country already holds a top-quarter position | :555-591 | MC_005 |
| Catch Up needs a capability anchor plus one corroborating signal | :596-612 | MC_009 |
| Deployment acceleration is decided at 0.6, even on one family of evidence | :463 | MC_010 |
| Security override: three curated routes (control/dual-use, coercive chokepoint, systemic resilience); concentration alone never triggers it | :398-409; docs/strategic_position.md:760-838 | MC_013 |
| Current trade weighs the latest 12 months at 50% | :150-152 | (no card: propose one) |
| Observed policy never feeds the choice; it is compared afterwards (policy alignment) | :631-649 | (no card) |
| All bars are inclusive; values within 0.05 of a bar are "borderline" | :456-468 | (no card) |

### 2D. Reasoning reveal (posture)

**Inputs available for every cell:**

- `trace[]` with steps secure_override → large_yard_case → supply_chain_need / deployment_need →
  strategic_industrial_value → cp_contender_pct → pathway_credible → choice, and `value`, `threshold`, `result`
- `need`, `prize.*`, `cp`, `cp_families`, `home_share`, `pathway_*`
- `borderline`, `choice_possible` (the engine's own neighbours, e.g. "preserve|catch_up"), `pivotal`,
  `watch_reasons`, `reason`

Extract them with review/qc/choiceRule.ts `extractChoiceInputs()` (:170-253) and replay them with `choiceRule()`
(:336-383) / `spChoiceRule()` (:87-107).

**Level words.** Post-reveal, so the model's bars may be named.

| Component | High | Moderate | Low | Other words | One-sentence "why" template and drivers |
|---|---|---|---|---|---|
| Need (producing) | trace `supply_chain_need.result == "high"` (≥ 0.67) | 0.33-0.67 ("below the 0.67 line the rule uses") | < 0.33 | "Not measured" (null); "Capped by the score floor" (0.61); "Import mix not scored: {country} supplies ≥10% of the world" (home ≥ 0.10) | "{Country} ranks above {need×100}% of economies on vulnerability for this step, chiefly because {driver}." Driver = the highest of `import_dependence`, `import_hhi`, `china_bloc`, `trusted_alternatives`, each paired with its raw fact (imports/GDP; top supplier share; China/other-bloc import share; same-group world share). |
| Deployment need | `deployment_need.result == "high"` (≥ 0.6) | 0.4-0.6 | < 0.4 | "Not measured" | "Demand {dn_demand.note}; 2030 target {dn_policy_gap.note}; system {dn_system.note}." Name the missing family. |
| Prize | `strategic_industrial_value.result == "high"` (≥ 0.6, not capped) | 0.4-0.6 | < 0.4 | "Held below the bar: market size alone" (`yf.cap == market_only`) | "World demand grows {cagr}%/yr to 2035; know-how score {commons}, future option {option}{; lifted by home-market growth}." Parts: `prize.market`, `yf.commons`, `yf.option`, `prize.home_market`. |
| Position | trace `cp_contender_pct.result == "high"` | `not_top_quarter` (a contender, below the top quarter) | `not_a_contender` (< 0.5% of world supply) or cp null | "Top-quarter rank but below the capability floor" (`capability_floor_not_met`); "Not tested" (Secure / large-yard cells: describe from `cp` and `home_share`) | "{Country} ranks {cp} among economies holding ≥0.5% of world supply; it holds {home}% of world supply and {bench}% of exports (RCA {rca})." Strongest and weakest `cp_families`. |
| Pathway (Catch Up only) | `pathway_credible == yes` | | no | unknown | `pathway_basis` in words: direct_supply_share, direct_export_share, operating_capacity, committed_pipeline, feasibility, adjacent_stage_relative, learning |

**The decision rule in plain English.** This covers every branch of sp_choice_rule (decision_engine.R:167-181;
choiceRule.ts:94-106). The rule is **first match wins**. Identify the branch with choiceRule.ts `decisionPath()`
(:459-465).

| Branch (path) | Plain-English rule shown after the reveal | Neighbour(s) to show | What would make the neighbour right |
|---|---|---|---|
| **Secure override** (`secure_override`, trace step 1 `result == "yes"`, `value` = routes) | "Before weighing value or position, the Explorer checks for an acute vulnerability. It found one through the {route} route, so the answer is Secure whatever the value or position." Route words: coercive_chokepoint = "one rival supplier holds ≥50% of world supply, ≥25% of {country}'s imports come from rival suppliers, substitution is hard and trusted supply could not be restored in time"; operational_control = "a control-layer or dual-use step (enrichment, isotope separation, rare-earth separation, magnets) with ≥25% of imports from rival suppliers"; systemic_resilience = "an essential function, import-dependent, a year or more to rebuild, and trusted partners hold under half of world supply". | Re-run the triage with `secure: false` (spChoiceRule, using `need`, `prize.value`, `cp`, `pathway`, `meta.yf_nodes` largeYard). Results: JPN EV/Refining → **Catch Up**; KOR Nuclear/Refining → **Catch Up**; BEL Oil/Extraction → **Watch** (large yard). | Name the route leg closest to failing, using shipped legs only: `coercive_top_share` vs 0.5; `coercive_import_share` vs 0.25; `trusted_share` vs 0.5; `substitutability`, `reconstitution_time` (CURATED); `systemic_essentiality`. The consequence level, qualification time and stockpiles are **not shipped**. Say so ("the Explorer's full Secure test cannot be replayed from this release"). |
| **Deployment** (`deployment`) | "At Deployment & Use the only question is whether the energy system needs much more of it: deployment need at or above 0.6 means Accelerate, below means Watch. It says nothing about where equipment is made." | Accelerate ↔ Watch | "Accelerate would follow if deployment need reached 0.6." Name the family that would move it, and any family not measured. |
| **Not assessed** | "The Explorer makes no call: {reason}." | none | "A call needs trade data for at least 5 comparable importers" (strategic_position.yml:197-200), or a deployment-need series. |
| **Large-yard Watch** (`large_yard`: Watch with no cp/need step in the trace) | "This step looks like a commodity: mature, passive, and {n} of 8 commodity signals hold ({signals}). The Explorer then prefers affordable, diversified access, unless {country} already ranks in the top quarter of producers." | the triage result with `large: false` | "If fewer than 5 commodity signals held (or know-how scored ≥ 2 learning points), the step would be judged on vulnerability and value: {triage result}." |
| **Triage: Preserve** (high value + strong position) | "High value and a top-quarter position: keep and strengthen it." | Catch Up (if position falls below the top quarter and a pathway exists); Partner (no pathway, high need); Watch (value not high and need not high) | "Catch Up would follow if {country} ranked below the top quarter (cp {cp} vs 0.75)." |
| **Triage: Catch Up** (high value, position not strong, credible pathway) | "High value, a position below the top quarter, and a credible base to build from: build it." | Preserve (position reaches the top quarter); Partner (no credible pathway and high need); Watch (no pathway and low need, or value not high) | as named |
| **Triage: Partner (high value)** (high value + high need, no strong position, no pathway) | "Valuable and vulnerable, but no credible domestic base: rely on trusted partners." | Catch Up (a pathway exists); Preserve (top-quarter position) | |
| **Triage: Watch (high value only)** | "Valuable, but neither vulnerable nor positioned to build: keep open." | Catch Up (pathway); Partner (need high); Preserve (strong position) | |
| **Triage: Preserve (high need, not high value)** | "Vulnerable and already strong, though the value is not high: keep it." | Partner (position not strong); Watch (need falls) | |
| **Triage: Partner (need only)** | "Vulnerable but without high value or a strong position: rely on trusted partners." | Preserve (strong position + floor); Catch Up (value high + pathway); Watch (need below the bar) | |
| **Triage: Watch (default)** | "Neither vulnerable nor high value: keep open; a strong position alone does not justify intervention (MC_001)." | Partner (need high); Preserve (need or value high with a strong position); Catch Up (value high + pathway) | |

**How to compute "why this, not the neighbour".**

1. If `choice_possible` is non-empty, use its other entries as the neighbours. `pivotal` names the decisive input.
2. Otherwise flip each input on its own with choiceRule (need ↔ above/below 0.67; prize ↔ high/low including the
   cap; cp_high; pathway; large) and keep the flips that change the choice.
3. Order the flips by distance to the bar: |value − threshold| for numbers; for the cap, "the cap"; for the
   large-yard case, "signals short".
4. The nearest flip is "the neighbouring alternative". Its input is "what would make it more appropriate".

This is what it gives for the 13 posture cases:

| Case | Shipped | Path | Neighbour (nearest flip) | Decisive input and distance | Plain sentence for "what would make the alternative right" |
|---|---|---|---|---|---|
| 001 KOR Batteries/Components | Preserve | triage | Catch Up (`choice_possible` preserve\|catch_up, `pivotal` cp) | cp 0.763 vs 0.75 (+0.013; one rank step ≈ 0.053) | One place lower among the node's contenders, and the same evidence gives Catch Up. |
| 002 JPN EV/Refining | Secure | secure_override (operational_control; coercive_chokepoint) | Catch Up (triage without the override) | rival supplier 94.4% of world (≥ 50%); rival share of own imports 88.5% (≥ 25%); trusted 4% | Secure would drop only if rival suppliers fell below 25% of Japan's imports, or trusted supply rose toward half of world supply. |
| 003 AUS Batteries/Extraction | Watch | triage | Preserve (need high, or prize uncapped) | need 0.115 (far; import mix not scored because the home share is 16%); prize capped (commons 0.333 < 0.5) | Preserve if mining know-how were judged strong enough to lift the cap (commons ≥ 0.5), given the top-quarter position (cp 0.864). Borderline: large-yard signals 4 of 5. |
| 004 USA Batteries/Refining | Catch Up | triage | Watch (pathway not credible, or prize low) | home share 0.51% sits on the 0.5% anchor line; prize 0.834 (far) | Watch if the US share of world supply fell below 0.5% and no other capability anchor held. |
| 005 IND Solar/Components | Partner | triage | Preserve (capability floor met) or Catch Up (prize uncapped) | cp 0.948 but `capability_floor_not_met`; prize capped (commons 0, option 0.375) | Preserve if India's capability case were at least moderate; Catch Up if the step's value were not capped as "market size alone". |
| 006 CHN Solar/Components | Watch | triage | Preserve | need 0.012 (far); prize capped | Preserve only if China were vulnerable here or the step's value were uncapped. |
| 007 DEU Solar/Assembly | Preserve | triage | Catch Up (`choice_possible`, `pivotal` cp) | cp 0.779 vs 0.75 (+0.029); large-yard 4 of 5 (no effect: position first) | Catch Up if Germany slipped below the top quarter. |
| 008 JPN Grid/Assembly | Catch Up | triage | Partner (prize low or no pathway) or Preserve (cp high) | prize 0.613 vs 0.60 (+0.013); cp 0.703 vs 0.75 (−0.047); need 0.685 (+0.015) | Partner if the value dipped below 0.6; Preserve if Japan reached the top quarter. |
| 009 KOR Nuclear/Refining | Secure | secure_override (operational_control: dual-use) | Catch Up | rival share of own imports 50.4% vs 25% | Secure would drop if under 25% of Korea's imports came from rival suppliers (Russia 44.2% today). |
| 010 BEL Oil/Extraction | Secure | secure_override (systemic_resilience) | Watch (a large yard without the override) | trusted 28.8% of world (< 50%); the import share is NLD 98.4% (transit) | Secure would drop if same-group suppliers held half of world supply, or if Belgium's import dependence did not rank in the top half. Transit via Rotterdam is a data question, not a strategic one. |
| 011 IND Batteries/Extraction | Partner | triage | Watch (`choice_possible` partner\|watch, `pivotal` large_yard_case) | large-yard signals 4 of 5 | Watch if one more commodity signal held. |
| 012 USA Nuclear/Deployment | Watch | deployment | Accelerate (`choice_possible`) | deployment need 0.597 vs 0.60; the 2030-target family is not measured | Accelerate at 0.60. The missing 2030-target family could move it either way. |
| 013 DEU Grid/Components | Watch | triage | Preserve | need 0.31 and prize 0.467 (both far) | Preserve only with high vulnerability or high value, given cp 0.948. |

### 2E. Notes on the reveal sentence

- `reason` (detail) is the Explorer's own sentence. It contains scores ("(0.79; market 0.85, commons 0.50, option
  0.00)") and code-like words. Show it at Level 2, verbatim with a "quoted" note, as snapshot.ts already does for AIB.
  Use the templates above at Level 1.
- The CASE_012 reason rounds 0.597 to "0.60 is below 0.60". Use 3 decimals in the reveal.

---

## 3. AIB node: the Sector review, one step under one stress

### 3A. Brief scaffolding

**Identity:**

- `nodes[id].label`
- AIB sector `nodes[id].techs`
- index `meta.indices[code==nodes[id].indexCode].name`
- six-stage step `nodes[id].stage` → STAGE_DISPLAY
- the components inside: `nodes[id].yfSubcomponents` (e.g. "Permanent Magnets & Magnet Parts"). The HS6 list is
  not shipped.
- the posture technologies touched: `nodes[id].yfCells`

Example: **Rare-earth permanent magnets · Magnets · Component Manufacturing · Major-supplier disruption**.

**Decision question** (what the model decides: one class per node and scenario, aib_compacts.R:15-31):

- Decoupling: "If China stopped supplying {label} to the 59 allied economies (while still buying from them), what
  kind of allied response would be needed: new capacity, coordinated trade, managed reliance on a few suppliers, or
  little intervention?"
- Managed: "If no supplier could hold more of the allied market for {label} than is normal for comparable products,
  what kind of allied response would be needed?"

**Decision context:**

> This approximates a joint decision by allied economic-security and trade officials about one product group. It
> asks whether existing trade could absorb a supply shock, or whether coordination or new capacity is needed. It
> looks at traded supply only and is immediate: it does not model how long new plants take, prices or firm-level
> constraints.

**True horizon and scenario:**

| Element | Value | Evidence |
|---|---|---|
| Trade period | **2025-26**: the latest 12 months of each economy's own monthly customs data (most windows Sep 2025-Aug 2026; 65% of the import value). Economies without monthly data keep 2024, with trade with China scaled by Ember. | compacts JSON `meta.tradePeriod`, `meta.tradeNote`; `meta.tradeYear` 2024 is the fallback (aib/contract.ts:469-476, `tradeWhen()` :1593); config/aib.yml:23 |
| Demand scope / suppliers | 59 economies (`meta.nDemandScope`; coalition core + partner tiers); any economy may supply | aib.yml:54-61 |
| Decoupling | China supply removed (100%) under **hold** semantics (China keeps buying); reallocation (suppliers may redirect exports from outside buyers); production-constrained where physical data exist; existing links may at most double | aib.yml:68-73, :110-117, :119-126, :145-149, :169-176; `meta.shockSemantics`, `meta.supplyConstraint` |
| Managed Interdependence | no supplier removed; a node more concentrated than its HS4 siblings (Atlas HS92 panel, latest year) is capped at the share that brings it to the benchmark; the excess shifts | aib.yml:94-108; `nodes[].diversify.{currentHhi, benchmarkHhi, basis, cap, capped, excess}` |
| Time | no lead time: `time_to_capacity_years` is not modelled (threat.ts notes). Say "immediate re-sourcing; build times are not modelled". | |

### 3B. Neutral evidence facts

Paths are relative to `compacts.c5643da1.json → nodes[id==X]`.

| id | Label | JSON path | Unit | Interpretation (generic) | Source / vintage | Kind |
|---|---|---|---|---|---|---|
| demand_usd | Allied economies' imports of {label} | `.demandUsd` | USD/yr | number | Comtrade monthly / 2024 (`meta.tradePeriod`) | RAW |
| exposure (decoupling) | Share of allied imports supplied by China (incl. HK/Macao re-exports) | `.exposure` (≈ `baseline.suppliers` CHN plus re-export hubs) | share | 1.2 | as above | RAW (hub attribution: aib.yml:35-36) |
| lost_usd | China's supply to the allied economies | `.lostUsd` | USD | number | as above | RAW (decoupling) |
| exposure (managed) | Share of allied demand above the concentration cap that must shift | `.diversify.excess`; cap `.diversify.cap`; who is capped `.diversify.capped` | share | state with the cap and the benchmark | Atlas HS92 benchmark | MODEL that **defines the scenario**: safe if worded as the scenario setting |
| concentration today | HHI of supply to the allied market | `.baseline.hhi` | 0-1 | DOJ/FTC 2023 (1.2) | as above | RAW |
| world-export HHI vs sibling benchmark (managed) | | `.diversify.currentHhi`, `.diversify.benchmarkHhi`, `.diversify.basis` | 0-1 | 1.2 | Atlas | RAW-DEF |
| top suppliers today | Top 5 (or top 10 with US$) suppliers to the allied market | `.baseline.suppliers` [iso3, share]; top 10 with USD: `scenario-baseline.2b9a857b.json nodes[id].suppliers` [iso3, usd, share] | share, USD | 1.2 | as above | RAW |
| largest supplier, top-3 | | `.baseline.largest`, `.baseline.largestShare`, `.baseline.top3Share` | | 1.2 | | RAW |
| physical evidence status | Production data for this step | `.physicalData` ∈ none / incomplete / complete; text `.physicalValidation` ("trade only; no physical data", "constrained by physical data ...") | | "Production evidence limited: inferred from trade" when none | GCIM, IEA (per node) | RAW |
| production vs trade divergence | | `.flags[]` containing "production more concentrated than trade ...", `.physicalDivergence` | | show verbatim | GCIM | RAW |
| data quality | | `.quality` (High confidence / Usable with caution / Insufficient for optimization), `.flags[]` | | show as a warning | | RAW. Note that "Insufficient for optimization" ⇔ class INSUFFICIENT DATA. |
| static | Coverage right after the shock | `.static` | | = 1 − exposure: redundant, drop | | MODEL |
| existing, optimized, dividend, residual, largest/largestShare/hhi after, n1Reoptimized, instantaneousN1, diversion, hosts, specialtyHosts, developmentHosts, bestGain, partners, treatment, frontier, variants, growth | post-shock solution | as named | | reveal (Level 2/3) | | MODEL |
| warning | "PHYSICAL CAPABILITY NOT VALIDATED" | `.warning` | | **reveal only**: it fires only on LOW INTERVENTION or COORDINATE (aib.yml:142-144; 4 of 4 here are LOW) | | MODEL (**leak**) |
| cls, reason, policyNeeds | | as named | | reveal | | MODEL |

**Recommended 5-8 neutral facts:**

1. Allied imports (US$, trade period).
2. China's share of allied supply (decoupling), or the cap and the share that must shift (managed), with the
   benchmark used.
3. Lost supply in US$.
4. The top 5-10 suppliers today with shares (who else supplies).
5. HHI today, in DOJ/FTC words.
6. Top-3 share.
7. Production-data status, with any production-vs-trade flag.
8. Data-quality grade and flags.

**Warnings:**

- physicalData none (54 of 77 nodes) → "Production evidence limited".
- Production-vs-trade divergence (e.g. polysilicon: "CHN holds 97% of GCIM operational capacity vs 10% of exports").
- quality "Usable with caution" (26) / "Insufficient" (3).
- "Most HS6 codes are not essential (broad headings)".
- HS2022 revision breaks.
- Managed only: "not tested" / "not stressed" status (`diversify.status`: already as diversified as its benchmark
  35; within the cap 3).
- The trade-period label mismatch (section 8).

### 3C. Analytical assumptions

| Assumption | Config | Card |
|---|---|---|
| Only traded supply counts; domestic production for home use is invisible | aib.yml:19-40 | MC_015 |
| In the shock, China keeps buying (hold); a two-way cut is a sensitivity | :110-115 | MC_016 |
| Existing trade links can at most double after a shock | :169-176 | MC_017 |
| Suppliers may redirect exports away from buyers outside the allied group | :119-126 | MC_018 |
| Round-number bars decide the class (low exposure < 15%; build gap ≥ 10%; coordination gain ≥ 5%; existing ≥ 90% adequate; largest after ≥ 40% or top-3 ≥ 75% "concentrated") | :277-297 | MC_019 |
| "Normal" concentration is judged against sibling products in the same HS4 heading | :94-108 | MC_020 |
| Who is an ally (demand scope = 59 coalition economies; hosts screened by UN-vote bloc) | :54-61, :252-255 | MC_022 |
| Where physical data cover ≥ 80% of world production, non-producers cannot expand supply | :145-149 | (no card) |
| No build times, prices or firm capacity; new capacity = +10% of demand at one host, at most doubling it | :198-217 | (no card: propose one, "immediate re-sourcing") |

### 3D. Reasoning reveal (AIB class rule, first match wins; aib_compacts.R:24-31)

1. data too thin → **Not enough evidence**
2. exposure < 0.15 → **Low intervention** ("the shock removes only {x}% of allied supply")
3. residual gap after coordinated trade ≥ 0.10 → **Build capacity**
4. concentrated after (largest ≥ 0.40, or top-3 ≥ 0.75, or capping at 40% costs ≥ 5 points of coverage) → **Manage
   specialization**
5. existing relationships restore ≥ 0.90 → **Low intervention**
6. coordination gain ≥ 0.05 → **Coordinate trade**
7. otherwise **Low intervention**

**Level words** (post-reveal):

- Exposure: High ≥ 40% (`high_severity`) / Moderate 15-40% / Low < 15%
- Gap after coordinated trade: material ≥ 10%
- Coordination gain: material ≥ 5 points
- Concentration after: the step-4 test

**Inputs and their shipped paths.** The concentration test of step 4 is reproducible from the bundle except for
top-3 under Decoupling:

- `exposure`, `residual`, `existing`, `dividend`, `largestShare`
- top-3 after: Decoupling not shipped (CSV only); Managed `.diversify.top3Share`
- coverage loss at the 40% cap = `frontier[cap=1].coverage − frontier[cap=0.4].coverage`

Managed uses the same rule on the `diversify.*` fields.

**Neighbour.** Re-run the chain with the decisive input moved to its bar. The variants already shipped
(`.variants`, `.growth`) give empirical neighbours.

| Case | Class | Decisive input | Neighbour and why |
|---|---|---|---|
| 014 RE magnets, decoupling | Build | gap 60.7% (≥ 10%) | Far from any neighbour: every shipped variant (China halved, release, conservative, trade-only, every growth bound) is Build. With the gap under 10% the rule would fall to Low (largest after JPN 24%; no coverage loss at the cap; existing 39%; gain 0). |
| 015 Graphite electrodes, decoupling | Coordinate | gain 8.1 pts (≥ 5); existing 83.4% (< 90%); gap 8.5% (< 10%) | **Build** if the gap reached 10% (the conservative scenario gives Build). **Low** if existing links restored 90% (unlimited growth gives Low) or China were only halved (Low). |
| 016 Polysilicon, decoupling | Low (exposure 11.7% < 15%) | exposure | **Manage specialization** if exposure were ≥ 15%: the gap (8.2%) is under 10% and the largest supplier after is USA at 46.9% (≥ 40%). The variants largest_subsector / stage / technology all give Manage. Pair this with the production flag (China 97% of operational capacity). |
| 017 Li-ion cells & packs, managed | Build (gap 45.6%) | gap after coordinated trade under the cap | **Manage specialization** if the gap were under 10%: China would still hold 42.3% after (≥ 40%). |

---

## 4. NEW kind: Sector bottleneck ("where in this supply chain is intervention warranted?")

### 4.1 Proposal

- **Kind:** `sector_chain`. CaseKind already has `sector`, but it reuses posture ids (snapshot.ts:46). Add a new
  value rather than overloading it.
- **Key:** `{sid: "SC-*", scenario: "decoupling" | "managed"}`, i.e. one Strategic Sectoral Compact chain under one
  stress.
- **Why this key:**
  - Strategic compacts partition the AIB nodes: 76 of 77 nodes sit in exactly one compact's `steps[].nodes`, and
    none in two.
  - The steps are curated value-chain steps with descriptions.
  - Each node carries its own six-stage `stage`.
- **Why not the alternatives:**
  - Grouping by posture technology through `yfCells` double-counts shared nodes. Semiconductor manufacturing
    equipment (US$93bn demand) lands under Solar Components and Assembly.
  - `yfCells` stages disagree with `node.stage` for 11 nodes (e.g. wafers: node Refining vs solar-components;
    li-ion cells & packs: Assembly vs electric-vehicles-components).
  - `{iso3, tech}` across the six posture stages gives a choice per stage. That is not a bottleneck: for USA
    Batteries the result is Partner / Catch Up / Preserve / Preserve / Accelerate / Catch Up, with intervention
    "warranted" nearly everywhere.
- **Question:** "Under {scenario}, at which stage(s) of the {compact name} chain is allied intervention warranted?
  Select every stage that needs it, then the one you would act on first." (In the brief's framing: "Has the
  Explorer located the actual strategic bottleneck?")
- **Answer options:** the six stages. Stages with no node in the chain are greyed "no product measured here". Add
  "No stage needs intervention", "Insufficient evidence" and "Outside my expertise".

### 4.2 How the Explorer's own answer is derived, and whether that is honest

The Explorer outputs a class per node and missions per compact. **It has no single "bottleneck stage" output.** Two
defensible derived answers:

- **Set answer** (faithful): the stages holding at least one node whose class under the scenario is BUILD,
  COORDINATE or MANAGE SPECIALIZATION. Source: `strategic.compacts[sid].scenarios[s].nodes` tuples
  `[node, status, coverage, host, ties]` joined to `nodes[id].stage`.
- **Lead stage** (faithful to the Explorer's own ordering): the stage of the first step named in the primary
  mission. aib_strategic.R `steps_of()` (:559-575) orders steps by **lost US$**. This equals the stage with the
  largest lost supply in intervention-class nodes.
- **Caveat to show at the reveal:** dollar weighting favours high-value downstream stages (modules, motors) over
  small-value upstream chokepoints (oxides, polysilicon). The lead stage is "where the largest dollar gap is", not
  "the chokepoint".
- Also show a chokepoint view: the stage whose node has the highest pre-shock China or largest-supplier share
  (`nodes[id].baseline`) and the production-vs-trade flags. That way a reviewer who picks the upstream chokepoint is
  not scored "wrong" by construction.
- **Scoring for analysis:** compare the reviewer's set with the Explorer's set (Jaccard), and the reviewer's lead
  with the Explorer's lead. Report the chokepoint view separately.

### 4.3 Brief, evidence, assumptions, reveal

- **Brief:** compact name (`strategic.compacts[].name`); steps and descriptions (`.steps[].label / .description`);
  a six-stage strip with each node placed by `nodes[id].stage`; scenario and trade period as in 3A.
- **Neutral evidence per step:** allied imports, China's or the largest supplier's share, lost US$, production-data
  status and the divergence flag for each node. These are the same paths as 3B. Do not show classes, missions,
  hosts or coverages.
- **Assumptions:** 3C, plus "steps are ranked by dollars lost" (propose a method card).
- **Reveal:** node classes per stage (3D rules), the mission text, and the chokepoint view.

### 4.4 Two concrete candidate cases (values from compacts.c5643da1.json)

**Candidate 1: SC-SOLAR × Decoupling** (counter-intuitive; trade and physical evidence diverge)

| Stage | Node | Class | Lost US$ | Gap US$ (residual × demand) | Notes |
|---|---|---|---|---|---|
| Refining & Processing | polysilicon | LOW INTERVENTION | 0.26bn | 0.18bn | China 11.7% of allied traded supply, but "CHN holds 97% of GCIM operational capacity vs 10% of exports"; physicalData complete |
| Refining & Processing | wafers | BUILD | 5.54bn | 5.11bn | node.stage Refining (yfCells says solar-components) |
| Refining & Processing | silver-powder | LOW INTERVENTION | 0.02bn | 0.00 | |
| Component Manufacturing | solar-cells | BUILD | 2.36bn | 2.27bn | |
| Component Manufacturing | pv-cover-glass | BUILD | 3.26bn | 3.09bn | China 60.4% today |
| Final Assembly & Equipment | pv-modules-and-generators | BUILD | 20.25bn | 20.01bn | China 68.2% today |

- The Explorer's set is {Refining, Components, Assembly}.
- The lead is **Final Assembly & Equipment**. The primary mission reads "build missing capacity in modules, cells &
  cover glass and wafers" (BUILD: 99% of lost supply, 4 nodes).
- The scenario: 51.2% of chain demand is lost (US$31.7bn); coverage is 48.8% after the shock and 50.4% with
  existing links.
- The test: an expert may name polysilicon as the bottleneck.

**Candidate 2: SC-GRID × Managed Interdependence** (the obvious case)

| Stage | Nodes (class) | Notes |
|---|---|---|
| Extraction | copper-ores-and-concentrates (LOW; 13.6% must shift, US$6.45bn) | the PHYSICAL CAPABILITY NOT VALIDATED warning |
| Refining & Processing | **grain-oriented-electrical-steel-goes (BUILD; 29.8% must shift, gap US$0.90bn, CHN 37.2% today, HHI 0.221 vs benchmark 0.107)**; copper-smelting-and-refining, copper-wire-and-rod and aluminium-wire-rod (NOT STRESSED) | |
| Component Manufacturing | insulators-and-bushings (LOW), overhead-conductors (LOW), power-cables (NOT STRESSED) | |
| Final Assembly & Equipment | transformers and switchgear-and-protection (NOT STRESSED), electricity-meters (LOW) | |

- The Explorer's set and lead are **Refining & Processing** (GOES only).
- The primary mission: "build missing capacity in grain-oriented electrical steel (GOES)" (13% of supply to shift;
  US$1.026bn).
- The test: whether experts see transformers rather than GOES as the constraint.

Optional third candidate: **SC-MAGNETS × Decoupling**.

- Every step is BUILD except ore (INSUFFICIENT DATA) and AC generators (LOW).
- The lead is Final Assembly & Equipment (electric motors, US$13.4bn gap), against separated oxides at US$0.52bn
  and RE metals at US$0.19bn.
- This is the purest test of the dollar-weighting caveat.

---

## 5. Compact: Strategic Sectoral Compact (SC-*)

### 5A. Brief scaffolding

**Identity:**

- `strategic.compacts[sid].name` and `.family`
- `.steps[].label` with descriptions
- the scenario
- `.workstreams` (operational AIB-C ids; Level 3)

**Decision question** (the model decides: the materiality of the primary mission → sectoral / narrower / none;
answerOptions.ts:33-62; aib_strategic.R:578):

> "Under {scenario}, do the allied economies need a standing Sectoral Compact across the {name} chain, targeted
> action on a few steps only, or no compact?"

**Decision context:**

> This approximates a G7 or allied-coalition decision on whether to set up a standing multi-country arrangement
> (coordinated investment, long-term offtake, stockpiles, trade rules) for a whole supply chain. A compact is meant
> to be warranted only where joint action materially changes resilience compared with what countries would do
> alone. The analysis does not cost the arrangement or test its politics.

**Horizon and scenario:** as for AIB (3A).

- `strategic.scenarios` words: baseline / managed / partial / decoupling. `partial` has no coalitions or investment.
- The neutral **purpose** sentence must be written fresh. Do not show the shipped `.purpose` (section 7).

### 5B. Neutral evidence facts

| id | Label | Path (`strategic.compacts[sid]`) | Kind |
|---|---|---|---|
| steps | Steps and what they contain | `.steps[].{order, label, description, nodes}` | RAW (curated grouping) |
| demand_usd | Allied imports across the chain | `.scenarios[s].demandUsd` | RAW |
| lost_usd / exposure | Supply lost (decoupling) or that must shift (managed) | `.scenarios[s].lostUsd`, `.exposure` | RAW (decoupling) / MODEL-defines-scenario (managed) |
| per-step pressure | China's or the largest supplier's share and lost US$ per node | `nodes[id].baseline`, `nodes[id].lostUsd` / `.diversify.lostUsd` | RAW |
| coverage_existing | Share of demand existing trade links could restore | `.scenarios[s].coverage.existing` | MODEL (under MC_017). Safe as "what existing trade can solve" if labelled as an estimate. |
| gap after existing (NEW) | 1 − coverage.existing | arithmetic | MODEL, same caveat |
| untested nodes | Steps the model could not test | `.scenarios[s].counts.{insufficient, notTested}` | RAW status |
| production-data status per node | | `nodes[id].physicalData` | RAW |
| purpose | | `.purpose` | **hide** (persuasive) |
| coverage_static | | `.coverage.static` | redundant (= 1 − exposure) |
| coverage_coordinated, coverage_post_investment, counts by class, missions, statement, policyMix, menu, headline, classMix, core, coalition, criticalPartners, criticalExternal, alliedOutsideCore, nonAlliedReliedOn, buildHosts, buildHostTies, division | | `.scenarios[s].*`, `.core`, `.nonAllied`, `.division` | MODEL: reveal only. `headline` contains the coordinated coverage. |

**Recommended neutral facts (6):**

1. The steps.
2. Allied imports across the chain.
3. Lost or shifted supply (US$ and %).
4. The per-step largest supplier and lost US$.
5. Coverage from existing links and the gap that remains (labelled as a model estimate).
6. Untested or thin steps and production-data gaps.

### 5C. Assumptions

- The materiality floor: 25% of the chain's lost supply OR US$1bn (aib_strategic_layer.yml:42-49;
  aib_strategic.R:578). Card MC_021.
- Everything in 3C.
- "Resilience after" is arithmetic: postInvestment = coordinated + min(increment, gap) (docs/aib_methodology.md 16.3).
  No card yet; propose one.
- The coalition is the fewest countries meeting 90/95/99% targets (aib.yml:314-317). No card.

### 5D. Reasoning reveal (the compact rule)

> "The Explorer classifies every step under the scenario (3D). It forms missions from the classes needing action
> (Build, Manage specialization, Coordinate), ordered by the supply they carry. If the lead mission carries at least
> 25% of the chain's lost supply, or at least US$1bn, a standing **Sectoral Compact** is warranted. If action is
> needed but the lead mission falls below both, the answer is **targeted action** on those steps. If no step needs
> action, **no compact** is needed."

Shipped: `missions[].{slot, cls, text, basis, targeted}`, `statement`.

| Case | Decision | Decisive arm | Neighbour |
|---|---|---|---|
| 018 SC-BATTERIES, decoupling | Sectoral | BUILD = 100% of US$108.3bn lost (10 nodes) | Far: targeted action would need the BUILD nodes under 25% and under US$1bn. |
| 019 SC-NUCLEAR, managed | Sectoral | share arm: 52% of only US$0.20bn to shift (≈ US$0.11bn) | **Targeted action** if the BUILD share were under 25%. The US$ arm is far below US$1bn. Note: under decoupling all nodes are LOW → no compact. |
| 020 SC-GRID, managed | Sectoral | US$ arm: GOES US$1.026bn ≥ US$1bn (share 12.5% < 25%) | **Targeted action** if GOES's supply to shift were US$26m (2.6%) lower. |

### 5F. The causal chain, with exact fields and pre/post-blind status

| Link | Fields | Before the blind answer? |
|---|---|---|
| 1. Problem today | `.scenarios[s].{demandUsd, lostUsd, exposure}`; per node `nodes[id].baseline.{largest, largestShare, top3Share, hhi, suppliers}` and `lostUsd`; scenario-baseline `nodes[id].suppliers` (top 10 with US$) | **Yes** |
| 2. What existing trade can solve | `.coverage.existing` (plus per node `nodes[id].existing`). Label: "if existing links could at most double (MC_017)". | **Yes** (as an estimate) |
| 3. Remaining gap | 1 − `.coverage.existing` | **Yes** |
| 4. New capacity required | `.coverage.coordinated` (coordinated trade), 1 − coordinated; BUILD node tuples `.nodes[i]` with status BUILD; `nodes[id].residual × demandUsd`; `.menu` | No: reveal |
| 5. Countries capable of adding it | `.buildHosts`, `.buildHostTies`, `.buildHostsNonAllied`; `nodes[id].{hosts, specialtyHosts, developmentHosts, bestGain}`; division flag `h` | No: reveal. **Pre-blind neutral substitute:** today's non-China suppliers per step (from link 1). |
| 6. Proposed coalition / compact | `.core[]` (curated roles), `.scenarios[s].{coalition, criticalPartners, criticalExternal, alliedOutsideCore, nonAlliedReliedOn}`; decision from `.missions` | No: reveal. Note SC-NUCLEAR managed: the coalition is **empty** although the decision is Sectoral (build host ITA). |
| 7. Expected resilience after | `.coverage.postInvestment`, 1 − postInvestment; `.nonAlliedReliedOn` | No: reveal, with "arithmetic, not a finding" (snapshot.ts:396). For SC-BATTERIES: 42.0% → 49.4%, so a 50.6% gap remains. |

**The "Does this compact materially ameliorate the threat?" test:**

- Show (postInvestment − existing) and the remaining gap.
- Time to capacity is not modelled; say so.
- Per-compact concentration before and after is not exported (threat.ts notes), so "dependence reduced?" cannot be
  answered from the bundle for SC-*.

---

## 6. Partnership (memo pairs)

### 6A. Brief scaffolding

**Identity:** `partnerships/manifest.json pairs[PAIR].{country_a, country_b, title}`.

**What the memo actually decides.** Four boxes in the summary section (memo JSON `sections[id=summary].blocks[0]`,
`type: choices`, `items[{label, kind, inlines}]`):

| Box | `kind` | USA_JPN content |
|---|---|---|
| Build together | preserve | RE oxides (joint), RE magnets (JPN→US), natural graphite (joint), separators (JPN→US), polysilicon (US→JPN), wind generating sets (US→JPN) |
| Trade and standards | partner | |
| Institutions | accelerate | upgrade USJ-SII; no new MOU |
| Stay out | watch | automatic controllers, gas components, semiconductor equipment |

Today's answer options use `CHOICE_NAME` labels (Preserve, Partner ...). The snapshot conclusion is always box 1.
**Replace this with a per-node task.**

- **Decision question:** "For {A} and {B}, in each of the following supply-chain steps, should they build new
  capacity together, rely on trade and common rules, or stay out (leave it to the market)?" Ask about 4-6 nodes:
  2-3 from the memo's lists, plus 1-2 controls the memo does not list. Add one institutional question: "Is a new
  agreement needed, or an upgrade of an existing one?"
- **Decision context:**

  > This approximates the agenda of a bilateral economic-security dialogue. The aim is joint resilience and
  > industrial value where each side's strengths meet the other's needs. It is not general goodwill: the analysis
  > does not judge diplomatic closeness, and two strong competitors are not complementary.

- **Horizon:** "12-36 months for deals" (memo section s9 "12-36 Month Deal Pipeline"). The data are as of
  `as_of` 2026-10-30. The memo states "AIB division of labour, 2024 trade" (s2), which is older than the current
  bundles.
- **Scenario:** the memo's AIB parts rest on the decoupling headline (China supply removed) and "allied short" =
  node class BUILD (docs/partnership_memos.md:63-66).

### 6B. Neutral evidence (pre-blind safe)

| id | Label | Path | Kind |
|---|---|---|---|
| bilateral trade | A's imports from B and B's from A in the energy basket | memo s1 bullet ("US imported US$41.9bn from Japan and Japan imported US$20.1bn from US (bilateral flows 2024)"). Better: compute from the bundle; that needs a dyad table, which is not shipped. | RAW |
| per-node world share, A and B | Share of world supply (exports or production) | `strategic.compacts[*].division` tuple `[iso3, node, ws, ds, rank, flags]` → `ws` | RAW |
| per-node allied demand share, A and B | Share of the allied market's imports | same tuple → `ds` | RAW |
| per-node China share of allied supply | | `nodes[id].baseline.suppliers` | RAW |
| per tech×stage import exposure, each side | Largest supplier and share; China share | posture detail (2B) | RAW |
| existing agreements | Facts only, not assessments | memo s1 bullets. Use only items not marked partial or unverified; the evidence register is not shipped. | CURATED-RAW |
| memo status | unverified; as_of; generated_at | manifest `verified`, `as_of`, `generated_at` | RAW |

USA_JPN neutral facts for the memo's nodes (current bundle; the memo may quote older numbers, e.g. "31%" vs 29.5%):

| Node | USA world share | USA share of allied demand | JPN world share | JPN share of allied demand | China share of allied supply |
|---|---|---|---|---|---|
| polysilicon | 29.5% | 7.4% | 7.5% | 32.7% | 11.7% (production: 97% of capacity) |
| rare-earth permanent magnets | 1.8% | 8.8% | 9.5% | 14.1% | 61.7% |
| rare-earth oxides & compounds | 5.7% | 17.4% | 5.7% | 13.6% | 35.3% |
| natural graphite | 2.8% | 23.5% | 2.1% | 8.8% | 33.6% |
| accumulator parts & separators | 2.3% | 34.2% | 5.9% | 4.6% | 39.4% |
| wind turbine generating sets | 7.3% | 0.2% | 0.0% | 1.7% | 46.4% |
| automatic controllers (stay out) | 15.4% | 12.2% | 11.4% | 7.3% | 14.6% |
| semiconductor manufacturing equipment (stay out) | 15.3% | 16.7% | 20.2% | 8.4% | n/a |

**Hold until the reveal:**

- the choices box and its headline
- the s2 cell codes (S/M/W position, H/L need: model-derived)
- the s3 relationship states
- the tier-1 joint bets
- the AIB strategic roles ("Demand & Market Anchor" ...)
- compact recommendations, "Do not prioritise" and the MEA assessment
- posture choices of either country (Preserve / Partner cells)
- division flags a/s/h/m

### 6C. Assumptions

The rules are in docs/partnership_memos.md:60-139:

- Two strong positions = competition, never complementarity.
- A supplier must be strong, or moderate and named; the buyer must have high need.
- A chokepoint = a rival supplier with ≥ 50% of world supply.
- Allied shortage = AIB class BUILD.
- Tier-1 joint bet = importance ≥ 0.67, evidence ≥ 0.5, additionality not low.

No method card covers these; propose one. MC_001 and MC_022 apply indirectly.

### 6D. Reveal

**Choices box:** use the box labels (Build together / Trade and standards / Institutions / Stay out), never
`CHOICE_NAME`.

**"Why this, not the neighbour" per node**, from the state rules:

- Build together ⇐ capacity-constrained trusted supply (allied supply short) or shared vulnerability.
- Trade and standards ⇐ one side supplies, the other needs, and allied supply is adequate.
- Stay out ⇐ both strong (competition) or a low strategic case.

The neighbour is the state the next rule would give:

- Build ↔ Trade if allied capacity were not short (node class not BUILD).
- Stay out ↔ Build if trusted capacity were short (MANAGE where more capacity is needed).

Per-node states are in the memo appendix tables A1/A2 (and `dyad_complementarity.csv`, which is not shipped).

### 6E. A brings / B brings / shared problem / what coordination changes

Computed from posture data with neutral rules: no choices or percentiles.

- **"A brings"** (cells): A's world share ≥ 5%, or A's export share ≥ 5% with RCA > 1.
- **"B is exposed"**: B's largest-supplier share or rival-supplier import share ≥ 50%, and B holds < 10% of world
  supply.
- **"Shared exposure"**: both exposed.
- **"Both strong"**: competition.

For USA→JPN this flags, among others:

| Cell | USA | JPN |
|---|---|---|
| Batteries/Components | 30.1% of world supply | CHN 76.6% of imports |
| Electric Grid/Assembly | 7.6% | CHN 51.6% |
| Wind/Components | 7.7% | CHN 46.3%, rival import share 58.3% |
| Wind/Assembly | 6.7% | CHN 55.6% |
| Solar/Assembly | 5.9% | CHN 45.6%, rival import share 52.1% |
| Gas/Refining | 37.5% | USA already 77.8% of its imports |

- **Shared exposure:** Batteries/Assembly (US CHN 56.3%; JPN CHN 82.7%), EV/Refining (CHN 70.1% / 47.2%; rival
  import share 70.7% / 88.5%).
- **Both strong:** Solar/Components (8.2% / 9.8%), Grid/Components, Nuclear/Components.
- At node level, use the AIB `ws` / `ds` table above: "A brings" = A's ws ≥ 5% where B's ds ≥ 5%.
- **Shared problem:** nodes where China holds ≥ 50% of allied supply and both hold ≥ 5% of allied demand (RE
  magnets: 61.7%; US 8.8%, JPN 14.1%).
- **What coordination changes:** reveal only. Use the node `existing` → `optimized` coverage, the memo's trade and
  investment agendas, and the compact roles.
- **Independence check:** these lists use the same raw facts the memo rules read, but none of the memo's
  states, tiers or labels. They do not reveal the box assignment. A reviewer can still disagree with the memo,
  e.g. wind generating sets: Japan is 1.7% of allied demand.

---

## 7. Leak and jargon list: `context_fields` in config/review/review_cases.yml

The enforcement list is snapshot.ts:53-55 `REVEALING_FACT_IDS` (choice, fence, posture, reason, confidence,
borderline, partner, policy_alignment, class, decision, missions, choices_summary, headline). The fields below pass
it but still leak or confuse.

| Field | Cases (yml line of context_fields) | Problem | Severity | Fix |
|---|---|---|---|---|
| `physical_warning` | 014 (:246), 015 (:262), 016 (:278), 017 (:294) | Fires only on LOW INTERVENTION / COORDINATE (aib.yml:142-144); 4 of 4 in this bundle are LOW, so it reveals the class (CASE_016) | **High (leak)** | Show `physicalData` + `physicalValidation` + `flags` (production vs trade) instead; move `warning` to reveal |
| `purpose` | 018 (:312), 019 (:329), 020 (:345) | Curated mission statement in imperative voice ("Build an allied battery chain ...", "Secure the nuclear fuel cycle ..."): persuasive, presupposes a compact | **High (persuasion)** | Write a neutral "what this chain makes" from `.steps[].description` |
| `non_allied_relied_on` | 018, 019, 020 | Output of the coalition solve: presupposes a coalition exists and names its dependencies; null when no coalition | **High (leak)** | Reveal only |
| `critical_external` | 018, 019, 020 | Leave-one-out result of the coalition: model-derived, presupposes a coalition | **High (leak)** | Reveal only |
| `coercive_top`, `coercive_top_share` | 001-011, 013 (:45 ... :229) | "Coercive" is a model classification relative to the importer (China's "largest coercive supplier" is JPN at 9.8%); `coercive_top_share` is the Secure physical-path leg (≥ 0.5, strategic_position.yml:337). It cues Secure in CASE_002 (94.4%). | **Medium (jargon + leak)** | Rename to "{China/Russia}'s share of world supply"; drop when the top "coercive" supplier is not CHN or RUS; put the definition in Assumptions |
| `trusted_share` | 001-011, 013 | Depends on the trusted-partner definition (MC_022); it is the resilience-route leg (< 0.5) and the delivery-mode input; AUS and USA battery mining show 0.0% (8.4) | **Medium** | Rename "world supply held by same-group economies"; show the definition; flag 0.0% values |
| `opposite_bloc_share` | 001-004, 006-010, 013 | Bloc assignment from UN votes to 2023; jargon ("opposite bloc"); NA for nonaligned by rule | **Medium (jargon)** | Merge into "share of imports from China, Russia and the other UN-voting bloc" (`coercive_import_share`), with the definition |
| `outlook` | all posture except 012 | Model label from a ±5% rule (strategic_position.yml:497) | **Low** | Replace with the demand growth % and %/yr parsed from `prize.note` |
| `prize_note` | all posture except 012 | Raw, but titled "Prize note" (model word) with abbreviations ("WEO 2025 STEPS", "CIM") | **Low (jargon)** | Retitle "World demand outlook 2024-2035"; expand the abbreviations |
| `home_share` | all posture except 012 | Measure varies by stage and is not shipped per cell; it is the contender / self-supply / position input | **Low** | Keep; label the measure; show the trade-vs-production divergence warning |
| `imports_to_gdp` | all posture except 012 | Gross, while the need measure is net; fine as a fact | **Low** | Keep, labelled "gross" |
| `cp` | 012 (:214) | A model percentile (Competitive Position at Deployment = deployment scale + investment, 0.966); jargon; the Deployment rule does not read it | **Medium (jargon)** | Replace with `deployment_trend`, the `dn_*.note` texts, and `dn_climate_aligned` as context |
| `static`, `coverage_static` | 014-017, 018-020 | = 1 − exposure: redundant; "coverage right after the shock" is model language | **Low** | Drop |
| `exposure`, `lost_usd` under Managed | 017, 019, 020 | Not observed: the excess above a modelled cap | **Low** | Keep as the scenario definition, worded so, with the cap and the benchmark |
| `coverage_existing` | 018-020 | Model output under the growth bound (MC_017) | **Low** | Keep as "what existing trade can solve (estimate)", per the brief's chain |
| `pair`, `memo_as_of`, `memo_generated_at`, `memo_verified` | 021 (:363) | No leak, but no evidence at all: the reviewer is under-informed | **Gap** | Add 6B facts |

Other pre-blind text to check:

- Titles: CASE_021 "where to build together" presupposes joint building.
- Questions: no posture question states the horizon or scenario.
- `why_selected` is shown only after the reveal (CaseReviewView.tsx:548-550). It is internal NEIS language with
  thresholds and "Surprising". Keep it for technical mode only.

---

## 8. Data and labelling issues found while cataloguing

1. **The AIB trade period is mislabelled in three places.** The bundle runs on 2025-26 trade (`meta.tradePeriod`,
   `meta.tradeNote`), but:
   - `strategic.scenarios[baseline].words` says "Trade as it is today (2024)" (aib_strategic.R:310 uses `tradeYear`)
   - review/qc/threat.ts:66 hard-codes "the observed 2024 supply"
   - the USA_JPN memo says "2024 trade"

   Use `tradeWhen(meta)` (aib/contract.ts:1593) everywhere.
2. **The minerals prize note year.** The note says "World demand +185% 2024-35 (IEA minerals STEPS)", but
   config `minerals_base_year` is 2025 (strategic_position.yml:495). Verify which base the number uses.
3. **`trusted_share` = 0.0 for battery-mineral mining** for both AUS and USA (Batteries/Extraction). This is not
   plausible when allied economies hold large lithium shares. It may be a driving-mineral measure (graphite) or the
   self-supply zeroing leaking into the raw field. Verify before showing it.
4. **USA Batteries/Components world-supply share 30.1% vs export share 13.7%**, and 73 other ≥ 5-point divergences.
   The per-cell supply measure is not shipped, so a reviewer cannot tell which is right.
5. **SC-NUCLEAR under Managed is "Sectoral" with an empty coalition** (`coalition` []) and one build host (ITA), on
   US$0.20bn to shift.
6. **The partnership "conclusion" is structurally constant** (box 1). The answer labels mismatch the memo's boxes
   (snapshot.ts:509-515; answerOptions.ts:91-92).
7. **The memo is older than the bundles**: as_of 2026-10-30 vs posture and AIB 2026-11-17. Node numbers differ
   (polysilicon "31%" vs 29.5%).
8. **The stage maps disagree for 11 AIB nodes** (`node.stage` vs `yfCells`). Any stage strip must pick one (use
   `node.stage`).
9. **The snapshot's prize-cap note** (snapshot.ts:273) tests `prize === 0.59` only. Capped nodes with a lower value
   (IND Batteries/Extraction 0.571) get no warning. Test `meta.yf_nodes[...].cap === 'market_only'` instead.
10. **BEL Oil/Extraction** reads Rotterdam transit as Dutch supply (NLD 98.4%). Only HKG and MAC are treated as hubs
    (MC_014).

## 9. Not in this release (the brief must say "not measured", never guess)

- Raw import HHI and net imports / GDP behind the need families (only percentiles and gross imports / GDP ship).
- Import values in US$ per posture cell.
- The world-supply measure used per cell.
- The contender list and n behind cp (so one rank step cannot be stated exactly).
- The capability lower bound (`t3_capability_lo`).
- The Secure consequence level, qualification time and stockpile legs.
- The HS6 list per AIB node.
- Top-3 share and supplier count after the shock (Decoupling: CSV only).
- Per-compact concentration before and after.
- Time to build capacity (nowhere in the Explorer).
- The memo evidence register, `dyad_complementarity.csv`, and bilateral flows per node.
- Firm-level capacity, qualification lead times, cost curves and project pipelines. These are good candidates for
  the "insufficient evidence" gap categories.
