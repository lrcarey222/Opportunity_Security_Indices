# Node attributes for the Yards & Fences method: product-market and learning evidence

This page records the evidence added on 2026-10-30 to `data/reference/posture_node_attributes.csv` so that
the strategic-position engine (`docs/strategic_position.md`, `R/strategic_position/`) can classify
capability layers on their own and test a large-yard (commoditization) case. It is data only. No value
here encodes a yard, a posture or the NEIS paper's placement of a component: each attribute was judged
on its own evidence, and `Unknown` is written wherever the public record is thin.

Two kinds of change were made:

1. **Three new columns** after `maturity`, filled on every `*` row (66 whole cells and 35 layers):

   | Column | Values | Question |
   |---|---|---|
   | `standardization` | low / moderate / high | How standardized and interchangeable is the product across suppliers (commodity specs, standard form factors, certification to common standards)? |
   | `differentiation` | low / moderate / high | How much do suppliers' products differ in performance, design or IP that buyers pay for? |
   | `commoditization` | no / partly / yes | Does the product compete mainly on price in a mature, liquid, multi-supplier market? |

   Whole-cell rows describe the cell's whole basket (a mixed basket gets a middle value and says why);
   layers describe the specific capability. The 41 country rows are left blank in these columns: a blank
   on a country row means "no override" and the `*` row applies (these are product facts, not country facts).

2. **Layer fills** of attributes that resolved to Unknown (the layer's own cell was `Unknown`, or blank and
   inheriting `Unknown` from its `yards_fences_assessments.csv` row): substitutability, qualification
   time, the three learning attributes and the leap test `incumbent_vulnerable`. A layer's own value now
   overrides the inherited one. Values that a layer already had, or inherited as a real value, were not
   changed. `yards_fences_assessments.csv` was not edited.

Every value carries a reason clause appended to the row's `notes` ("Product-market and learning
evidence (2026-10-30): std high [moderate]: ...") and its sources appended to the row's `source`. The
bracket is the confidence of that value. The row's own `confidence` column was not changed, so where a
new value is less certain than the row (several stage rows at `moderate` with new values at `low`) the
per-value confidence is in the notes and in the tables below.

## How the values were judged

* **Standardization** is high where a public specification makes products from different suppliers
  substitutable without re-engineering (exchange contract specs, fuel and gas quality specs, IEC 61215/61730
  modules, LME metals, UF6 specs); moderate where common standards govern grades, safety or interfaces but
  products are qualified buyer by buyer or partly design-specific (battery cells and materials, magnets,
  inverters, transformers and switchgear, control software with standard protocols); low where the
  product is made to one design (OEM spares, licensed fuel and reactor designs, engineered-to-order plant,
  proprietary firmware).
* **Differentiation** is low where buyers pay for grade or spec only; moderate where performance,
  reliability or features differ and are priced but several suppliers are close substitutes; high where
  design or IP decides the purchase (reactor and fuel designs, electrolyser stacks, vehicles, BMS and
  turbine-control algorithms, HVDC control and protection).
* **Commoditization** is yes only for products that are exchange-, index- or spot-priced among many
  suppliers in a mature market (crude, products, gas, coal, LME metals, scrap, polysilicon, PV modules,
  passive racking); partly where price leads but liquidity is thin, supply is concentrated, buyers
  qualify suppliers, or the basket mixes commodities with differentiated goods; no for engineered-to-order,
  licensed, platform-nominated or proprietary products and for early markets.
* **Deployment** cells take `Unknown` in the three new columns under the existing Deployment convention
  (use, not a traded good; no capability case is tested there). Empty cells (no defined product) and
  emerging recovery activities with no basket also take `Unknown`.
* **Control layers.** `process_knowledge` stays `Unknown` on all 13 control layers: it asks whether
  losing *manufacturing* would weaken process know-how, and public evidence does not establish a
  production-linked learning effect for software and firmware. Substitutability and adjacent innovation
  were filled where the evidence is specific (open protocols, retrofit markets, licensing records,
  research roadmaps); qualification time only where a public record gives a duration (EMS replacements,
  the BIS connected-vehicle schedule, the Diablo Canyon digital I&C licensing record). The sentence in
  unlinked control-layer notes that said physical-path and learning fields are "not used for control
  layers" was replaced to say what is now assessed and what stays Unknown (reconstitution, stockpile).
* **Sources.** IEA reports, US DOE supply-chain assessments, standards documents, exchange contract
  specifications, peer-reviewed literature, regulatory records and company or trade-press reporting
  (named in the source column). NREL is cited for the grid-forming inverter research roadmap. No
  BNEF value is used. The NEIS paper and the Yards & Fences component table are cited only for a
  component's characteristics (for example "passive steel and aluminium structures"), never for its yard.

## The industrial-commons attributes and the spec's names

The specification asks for an industrial-commons family (B) built from process knowledge, the supplier
ecosystem, production-innovation linkage and cross-sector spillovers. The table already holds three
learning attributes (Draw the Fence Gate 3, `dashboard/src/fence/engine/inputs.ts`):

| Spec name | Column | Question it answers (Draw the Fence) | Fit |
|---|---|---|---|
| Supplier ecosystem; workforce | `workforce_suppliers` | Are there specialised workforce skills or supplier relationships that would be difficult to recreate? | Direct. One column holds both the workforce and the supplier base, as in the paper's "industrial commons" (Pisano and Shih). |
| Production-innovation linkage | `process_knowledge` | Would losing production materially weaken process knowledge? | Direct for manufactured goods: it is the process-embedded innovation of Pisano and Shih's modularity-maturity argument. Not applicable to software layers (left Unknown). |
| Cross-sector spillovers | `adjacent_innovation` | Does production materially improve the ability to innovate in adjacent *or successor* technologies? | Partial. The question bundles cross-sector spillovers (family B) with successor-technology learning (example: battery lines as a base for solid-state cells), which overlaps the future-option family C (`leap_candidate`, `successor`, the four leap tests). |

The large-yard signals in the specification map as follows: technological maturity = `maturity`;
substitutability = `substitutability`; ease of qualification = `qualification_time`; low process-learning
value = `process_knowledge` no; low innovation spillover = `adjacent_innovation` no; product
standardization = `standardization`; low differentiation = `differentiation`; commoditization =
`commoditization`. Two signals are not curated attributes: **global supply diversification** is
measured (supplier shares in `major_supplier_shares_tbl`), and **passive vs digitally active** is
`digitally_active` on layers only, because whole-cell rows carry `digitally_active = no` by convention
(their control risks live in control layers), so a cell's `no` is not evidence that the cell is passive.

No new column was added for the commons family. The case for one is discussed in the report that
accompanies this change: `adjacent_innovation` mixes cross-sector spillover with successor learning, so a
separate `cross_sector_spillover` column (no / partly / yes) would let family B and family C be scored
without counting successor learning twice.

## Open points

* `config/security_route_rules.csv` repeats substitutability for each node and still says `Unknown` for
  the 13 control layers that now carry a value; its agreement check only compares known values, so it
  does not fail, but the table should be brought in line.
* `solar:components:stage` keeps `incumbent_vulnerable = Unknown` while its two layers now carry
  `partly` (filling whole-cell leap tests was outside this change).
* Left Unknown for lack of public evidence: GOES qualification time; qualification time for BMS and
  gateway, inverter-firmware, smart-meter, OT-control, wind-controller, HVDC-control and charge-point
  back-end replacements; perovskite qualification time; workforce and supplier base for most control layers;
  adjacent innovation for gas and oil OT control; differentiation and commoditization for battery parts,
  oil-equipment parts and EV recovery; standardization and differentiation for reprocessing;
  commoditization of charge-point back ends; all three new columns for geothermal components, waste
  oils and PV and wind recovery.

## Attribute tables

One line per value added. "Row" is the stage (whole cell) or `stage:layer`. Confidence "-" marks an
Unknown. Source keys are listed at the end; "row's own notes and sources" means the evidence already
cited on that row.

### Batteries

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | moderate | moderate | concentrates trade on grade specs (spodumene Li2O content, Mn and Ni ore grades) but impurity profiles differ by deposit and refiners must adapt to a new feed | [IEA-GCMO24], row's own notes and sources |
| `extraction (whole cell)` | differentiation | low | moderate | buyers pay for grade and impurity content, not for supplier design or IP | [IEA-GCMO24] |
| `extraction (whole cell)` | commoditization | partly | moderate | grade-indexed bulk ores, but sold mostly under bilateral offtakes in thin, concentrated markets (cobalt, nickel ore) | [IEA-GCMO24] |
| `refining (whole cell)` | standardization | moderate | low | lithium salts have battery-grade specs and exchange contracts, but cathode and anode materials are qualified cell by cell | [LI-FUT], [IEA-BAT24], row's own notes and sources |
| `refining (whole cell)` | differentiation | moderate | low | cathode and anode materials differ in chemistry, particle engineering and coatings that set cell performance; lithium salts do not | [IEA-BAT24] |
| `refining (whole cell)` | commoditization | partly | low | lithium chemicals are exchange-priced; cathode, anode, separator and electrolyte materials are customer-qualified specialties | [LI-FUT], [IEA-BAT24] |
| `refining:bess-anode` | standardization | moderate | low | spherical and synthetic graphite are sold to purity and particle-size specs but qualified cell by cell | [IEA-GCMO24], [YF] |
| `refining:bess-anode` | differentiation | moderate | low | natural vs synthetic grades, coatings and particle engineering set fast charging and cycle life | [IEA-BAT24] |
| `refining:bess-anode` | commoditization | partly | low | many producers compete on price, but customer qualification limits switching | [IEA-GCMO24], [YF] |
| `refining:bess-cathode` | standardization | moderate | low | chemistry families (LFP, NMC grades) are common, but materials are qualified cell by cell | [YF] |
| `refining:bess-cathode` | differentiation | moderate | low | single-crystal, doping and coating know-how differentiate NMC; LFP grades converge | [IEA-BAT24] |
| `refining:bess-cathode` | commoditization | partly | low | LFP cathode competes on price amid overcapacity; high-nickel NMC less so | [IEA-BAT24] |
| `refining:li-chemicals` | standardization | moderate | moderate | battery-grade carbonate and hydroxide specs and exchange contracts exist, but impurity profiles are qualified by each cathode maker (1-3 years) | [LI-FUT], row's own notes and sources |
| `refining:li-chemicals` | differentiation | low | moderate | spec chemicals; buyers pay for purity, not supplier design | [LI-FUT] |
| `refining:li-chemicals` | commoditization | partly | moderate | exchange-priced with many converters, but qualification and concentrated refining limit liquidity | [LI-FUT], [IEA-GCMO24] |
| `components (whole cell)` | standardization | low | low | parts of accumulators (cases, plates, separators, BMS boards) are made to each pack design | row's own notes and sources |
| `components (whole cell)` | differentiation | Unknown | - | no public evidence on how this mixed parts basket competes | row's own notes and sources |
| `components (whole cell)` | commoditization | Unknown | - | no public evidence on how this mixed parts basket competes | row's own notes and sources |
| `assembly (whole cell)` | standardization | moderate | moderate | cells come in a few common formats (cylindrical, prismatic, pouch) certified to common safety standards, but switching cell needs pack redesign and requalification | [CELL-STD], [IEA-GEVO25] |
| `assembly (whole cell)` | differentiation | moderate | moderate | energy density, cycle life, fast charging and safety differ by maker and chemistry; LFP cells have converged most | [IEA-BAT24] |
| `assembly (whole cell)` | commoditization | partly | moderate | LFP cell prices fell sharply amid overcapacity and compete increasingly on price, but chemistry, format and bankability differences persist | [IEA-BAT24], [IEA-GEVO25] |
| `assembly:bess-cells` | standardization | moderate | moderate | LFP and NMC cells in a few common formats certified to common safety standards, but a cell switch needs pack redesign and requalification | [CELL-STD], [YF] |
| `assembly:bess-cells` | differentiation | moderate | moderate | energy density, cycle life and safety differ by maker and chemistry | [IEA-BAT24] |
| `assembly:bess-cells` | commoditization | partly | moderate | LFP cells compete increasingly on price amid overcapacity; bankability and chemistry differences persist | [IEA-BAT24], [IEA-GEVO25] |
| `assembly:bess-comms` | standardization | moderate | low | gateways speak open device protocols, but cloud fleet platforms are vendor-proprietary | [BESS-PROTO], [YF] |
| `assembly:bess-comms` | differentiation | moderate | low | platforms differ in analytics and market-bidding features | [DOE-VPP] |
| `assembly:bess-comms` | commoditization | no | low | vendor platforms bought with the storage system | [YF] |
| `assembly:bess-comms` | substitutability | moderate | low | third-party gateways and fleet platforms can usually replace the vendor's without touching the cells, but integration must be verified per architecture | [YF] |
| `assembly:bess-comms` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:bess-comms` | adjacent_innovation | partly | low | fleet platforms are the base for aggregating storage into virtual power plants | [DOE-VPP] |
| `assembly:bess-control` | standardization | low | moderate | BMS firmware is integrated with proprietary cell hardware and has no interoperability standard; replacement may need redesign and recertification | [YF], [PLETT] |
| `assembly:bess-control` | differentiation | high | low | state-of-charge, state-of-health and safety algorithms are core supplier IP that sets usable capacity, life and warranty | [PLETT] |
| `assembly:bess-control` | commoditization | no | moderate | proprietary, system-specific software | [YF] |
| `assembly:bess-control` | substitutability | moderate | low | other integrators' BMS and EMS exist for new systems, but an installed BMS is tied to its cells (partial separability) | [YF] |
| `assembly:bess-control` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:bess-control` | adjacent_innovation | yes | low | BMS and EMS know-how carries into managing successor chemistries, second-life use and grid services | [PLETT], [DOE-VPP] |
| `assembly:bess-enclosure` | standardization | high | moderate | ISO-container formats and generic steel and aluminium structures | [ISO668], [YF] |
| `assembly:bess-enclosure` | differentiation | low | moderate | passive structures with little performance difference | [YF] |
| `assembly:bess-enclosure` | commoditization | partly | low | generic structures are price-competed; thermal management and fire protection are engineered per system design | [CELL-STD] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | low | low | black mass and battery scrap vary by chemistry and process, and pack designs vary, which hampers standardised dismantling | [HARPER] |
| `recovery (whole cell)` | differentiation | low | low | buyers pay for contained nickel, cobalt and lithium, not for supplier design | [HARPER] |
| `recovery (whole cell)` | commoditization | partly | low | valued on contained metals, but feed is heterogeneous and the traded market is young and thin | [IEA-GCMO24], [HARPER] |

### Coal

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | high | moderate | seaborne thermal coal trades on standard contracts with quality specs (calorific value, ash, sulphur) | [COAL] |
| `extraction (whole cell)` | differentiation | low | moderate | grades are priced by quality spec; no supplier IP | [COAL] |
| `extraction (whole cell)` | commoditization | yes | moderate | index-priced and futures-traded with many exporting countries | [COAL] |
| `refining (whole cell)` | standardization | moderate | low | coke is sold to quality specs (strength, ash, sulphur), but blast furnaces are sensitive to coke quality | [IEA-STEEL20] |
| `refining (whole cell)` | differentiation | low | low | priced on quality spec, not supplier design | [IEA-STEEL20] |
| `refining (whole cell)` | commoditization | partly | low | spec-priced, but most coke is made captive at steelworks and the traded market is thin | [IEA-STEEL20], row's own notes and sources |
| `components (whole cell)` | standardization | low | low | OEM-specific spares made to the plant's design | row's own notes and sources |
| `components (whole cell)` | differentiation | moderate | low | the OEM holds the design; non-OEM suppliers offer some replacement parts and repairs that differ in fit and life | [AFTERMKT] |
| `components (whole cell)` | commoditization | no | low | bought on fit and availability for a given machine; no liquid market | row's own notes and sources |
| `assembly (whole cell)` | standardization | low | low | boilers and steam turbines are engineered to order for each plant under pressure-equipment codes | [ASME], row's own notes and sources |
| `assembly (whole cell)` | differentiation | moderate | low | steam conditions and efficiency (supercritical, ultra-supercritical) and OEM designs differ; several OEMs compete | row's own notes and sources |
| `assembly (whole cell)` | commoditization | no | low | engineered-to-order capital equipment | row's own notes and sources |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |

### Electric Grid

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | moderate | moderate | copper concentrate and bauxite are graded on metal content with penalties for impurities | [IEA-GCMO24] |
| `extraction (whole cell)` | differentiation | low | moderate | buyers pay for contained metal and impurity levels | [IEA-GCMO24] |
| `extraction (whole cell)` | commoditization | yes | moderate | copper concentrate, the bulk of the value, is priced off LME copper less treatment and refining charges, with many mines | [LME], [IEA-GCMO24] |
| `refining (whole cell)` | standardization | high | moderate | by value the cell is LME-grade copper cathode and primary aluminium made to exchange specs (GOES is a layer) | [LME], row's own notes and sources |
| `refining (whole cell)` | differentiation | low | moderate | exchange-grade metal is interchangeable across producers | [LME] |
| `refining (whole cell)` | commoditization | yes | moderate | exchange-traded in liquid multi-supplier markets | [LME] |
| `refining:goes` | standardization | moderate | moderate | graded by core loss to common standards, but transformer makers qualify each mill's steel with trial batches and core tests | [GOES-STD], [NLMK-ABB] |
| `refining:goes` | differentiation | moderate | moderate | high-permeability and domain-refined low-loss grades come from few mills holding the process know-how; buyers pay for loss grade | [DOE-GRID22], [GOES-STD] |
| `refining:goes` | commoditization | no | moderate | few producers worldwide, sold under qualification and contracts | [DOE-GRID22] |
| `refining:goes` | qualification_time | Unknown | - | qualification runs through trial batches and core tests, but no public source gives its duration | [NLMK-ABB] |
| `components (whole cell)` | standardization | moderate | low | line hardware (poles, stranded wire, insulators) is built to common standards; power semiconductors and converter parts are device-specific | [DOE-SPE22], row's own notes and sources |
| `components (whole cell)` | differentiation | moderate | low | little among line hardware; power semiconductors (IGBT, SiC) differ in performance and IP | [DOE-SPE22] |
| `components (whole cell)` | commoditization | partly | low | commodity line hardware alongside differentiated semiconductors | [DOE-SPE22], row's own notes and sources |
| `assembly (whole cell)` | standardization | moderate | moderate | equipment is built to IEC/IEEE transformer and switchgear standards, but large units are engineered to order; cables, breakers and meters are more standard | [TX-STD], [DOE-LPT14] |
| `assembly (whole cell)` | differentiation | moderate | moderate | losses, reliability and delivery differ by maker; buyers qualify makers | [DOE-GRID22] |
| `assembly (whole cell)` | commoditization | partly | moderate | distribution transformers, cables and meters are tendered on price among qualified makers; large power transformers and HVDC are engineered to order with multi-year backlogs | [DOE-GRID22], [IEA-GRID25] |
| `assembly:bess-pcs` | standardization | moderate | low | inverters and converters are certified to common grid and safety standards in standard ratings, but integration with storage and plant controls is vendor-specific | [GRID-CODES] |
| `assembly:bess-pcs` | differentiation | moderate | low | efficiency, grid-forming capability and reliability differ by vendor | [NREL-GFM] |
| `assembly:bess-pcs` | commoditization | partly | low | many makers compete on price for string and central inverters; grid-forming utility PCS less so | [IEA-PV22], [NREL-GFM] |
| `assembly:grid-ami` | standardization | moderate | low | meters follow DLMS/COSEM and national specs written for supplier interoperability (GB SMETS2); head-end systems are proprietary | [DLMS] |
| `assembly:grid-ami` | differentiation | moderate | low | little among spec meters; head-end and data platforms differ | [DLMS] |
| `assembly:grid-ami` | commoditization | partly | low | meters are bought in large tenders to common specifications; head-end systems are not | [DLMS] |
| `assembly:grid-ami` | substitutability | moderate | low | several vendors meet the standard meter specs, but switching an installed fleet means replacing meters or the head-end (partial separability) | [DLMS], row's own notes and sources |
| `assembly:grid-ami` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:grid-ami` | adjacent_innovation | partly | low | AMI data and control underpin demand response and grid-edge analytics | [DOE-VPP] |
| `assembly:grid-cables` | standardization | high | moderate | power cables are made to common IEC standards; ordinary fittings with plentiful alternatives | [CABLE-STD], [YF] |
| `assembly:grid-cables` | differentiation | low | moderate | little for distribution cables and fittings | [YF] |
| `assembly:grid-cables` | commoditization | partly | moderate | copper-linked and made by many firms, but HV and submarine cables are capacity-constrained engineered products | [IEA-GRID25] |
| `assembly:grid-inverter-firmware` | standardization | moderate | low | grid-support functions are specified by common standards and grid codes, but the firmware is proprietary to each inverter | [GRID-CODES] |
| `assembly:grid-inverter-firmware` | differentiation | moderate | low | grid-forming and fault ride-through performance differ by vendor | [NREL-GFM] |
| `assembly:grid-inverter-firmware` | commoditization | no | low | proprietary firmware sold with the inverter | [GRID-CODES] |
| `assembly:grid-inverter-firmware` | substitutability | moderate | low | many certified utility-scale inverter vendors, but firmware cannot be swapped without the inverter | [GRID-CODES], [YF] |
| `assembly:grid-inverter-firmware` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:grid-inverter-firmware` | adjacent_innovation | yes | moderate | inverter control development is where grid-forming capability for high-renewable grids is being built | [NREL-GFM] |
| `assembly:grid-scada` | standardization | moderate | moderate | interfaces follow the Common Information Model and standard telecontrol protocols, but EMS and SCADA platforms are proprietary | [CIM] |
| `assembly:grid-scada` | differentiation | high | moderate | network applications (state estimation, contingency analysis, DER management) differ and a few vendors serve most utilities | [CIM], [EMS-REPL] |
| `assembly:grid-scada` | commoditization | no | moderate | multi-year, utility-specific system projects | [EMS-REPL] |
| `assembly:grid-scada` | substitutability | moderate | moderate | several vetted vendors exist, but migration is a major project | [EMS-REPL], [YF] |
| `assembly:grid-scada` | qualification_time | gt3y | low | full EMS/SCADA replacements typically run three to five years from procurement to cutover | [EMS-REPL] |
| `assembly:grid-scada` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:grid-scada` | adjacent_innovation | yes | moderate | DERMS and advanced EMS are the platform for DER integration and virtual power plants | [DOE-VPP] |
| `assembly:grid-transformers` | standardization | low | moderate | large power transformers are custom-designed for each application | [DOE-LPT14] |
| `assembly:grid-transformers` | differentiation | moderate | low | losses, reliability and delivery differ; utilities qualify makers | [DOE-LPT14], [DOE-GRID22] |
| `assembly:grid-transformers` | commoditization | no | moderate | engineered to order with multi-year backlogs since 2022 | [DOE-GRID22], [YF] |
| `assembly:hvdc-control` | standardization | low | moderate | control and protection are vendor-proprietary and not interoperable across vendors today | [INTEROPERA] |
| `assembly:hvdc-control` | differentiation | high | moderate | each vendor's control and protection system is its own design | [INTEROPERA] |
| `assembly:hvdc-control` | commoditization | no | moderate | supplied only with the vendor's converter station | [INTEROPERA] |
| `assembly:hvdc-control` | substitutability | hard | moderate | until multi-vendor interfaces exist, control and protection can come only from the converter vendor; the alternative is rebuilding the station | [INTEROPERA], row's own notes and sources |
| `assembly:hvdc-control` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:hvdc-control` | workforce_suppliers | yes | low | a handful of HVDC vendors hold the control and protection know-how; defining multi-vendor interfaces takes a four-vendor, eight-TSO programme | [INTEROPERA] |
| `assembly:hvdc-control` | adjacent_innovation | yes | moderate | multi-terminal, multi-vendor HVDC grids with grid-forming converters depend on control and protection development | [INTEROPERA] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | high | moderate | copper and aluminium scrap trade under published grade specifications | [REMA] |
| `recovery (whole cell)` | differentiation | low | moderate | priced on grade and metal content | [REMA] |
| `recovery (whole cell)` | commoditization | yes | moderate | priced against exchange metal prices in a liquid, multi-supplier scrap market | [REMA], [LME] |

### Electric Vehicles

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | moderate | moderate | as Batteries Extraction: concentrates graded on content, impurity profiles differ by deposit; monazite sold on rare-earth oxide content | [IEA-GCMO24] |
| `extraction (whole cell)` | differentiation | low | moderate | buyers pay for grade and impurity content | [IEA-GCMO24] |
| `extraction (whole cell)` | commoditization | partly | moderate | grade-indexed, but bilateral offtakes in thin, concentrated markets | [IEA-GCMO24] |
| `refining (whole cell)` | standardization | high | moderate | rare-earth oxides and metals are sold to purity specs; battery metals to exchange or battery-grade specs | [IEA-GCMO24] |
| `refining (whole cell)` | differentiation | low | moderate | spec-graded products; no supplier design premium | [IEA-GCMO24] |
| `refining (whole cell)` | commoditization | partly | moderate | spec-graded, but rare earths are priced in opaque, China-centred markets with no liquid exchange contract | [IEA-GCMO24] |
| `components (whole cell)` | standardization | low | low | traction packs, drive units and harnesses are engineered per vehicle platform | [YF], [IEA-GEVO25] |
| `components (whole cell)` | differentiation | moderate | low | drive-unit efficiency and power density and pack energy density differ by supplier | [YF] |
| `components (whole cell)` | commoditization | no | low | sourced under multi-year platform nominations, not in a liquid market | [YF] |
| `components:ev-motors` | standardization | low | low | drive units are engineered per vehicle platform | [YF] |
| `components:ev-motors` | differentiation | moderate | low | efficiency, power density and integration differ across suppliers and in-house OEM designs | [YF], [IEA-GEVO25] |
| `components:ev-motors` | commoditization | no | low | sourced under platform nominations, not in a liquid market | [YF] |
| `components:ree-magnets` | standardization | moderate | moderate | sintered NdFeB comes in standard grade classes, but parts are custom shapes and coatings qualified per motor | [MAGNET-STD], [YF] |
| `components:ree-magnets` | differentiation | moderate | moderate | heavy-rare-earth reduction and high-temperature grades are maker know-how | [DOE-MAG22] |
| `components:ree-magnets` | commoditization | partly | moderate | graded products from many producers, but custom parts, qualification and concentrated supply | [DOE-MAG22], [IEA-GCMO24] |
| `assembly (whole cell)` | standardization | low | moderate | vehicles are model-specific; only chargers follow common connector and protocol standards | [UNECE] |
| `assembly (whole cell)` | differentiation | high | moderate | range, software, features and brand drive purchase | [IEA-GEVO25] |
| `assembly (whole cell)` | commoditization | no | moderate | a scaling, model-differentiated market, although price competition is intense in China | [IEA-GEVO25] |
| `assembly:ev-connected-vehicle` | standardization | moderate | low | connectivity modules follow cellular standards and are treated as replaceable hardware; OTA and vehicle software are OEM-specific | [BIS-CV] |
| `assembly:ev-connected-vehicle` | differentiation | moderate | low | software features differ by OEM; connectivity hardware little | [BIS-CV] |
| `assembly:ev-connected-vehicle` | commoditization | partly | low | connectivity modules are standard components; the software is not | [BIS-CV] |
| `assembly:ev-connected-vehicle` | substitutability | moderate | moderate | the BIS rule treats connectivity hardware as replaceable and software as integrated, so OEMs must re-source both | [BIS-CV] |
| `assembly:ev-connected-vehicle` | qualification_time | y1to3 | moderate | BIS gave software re-sourcing to model year 2027 (about two years after the January 2025 rule) and hardware to model year 2030; automakers redesign platforms every four to six years | [BIS-CV] |
| `assembly:ev-connected-vehicle` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:ev-connected-vehicle` | adjacent_innovation | yes | low | connectivity and OTA platforms carry automated-driving and software-defined-vehicle features (the BIS rule covers ADS software alongside connectivity) | [BIS-CV] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment:ev-charging-mgmt` | standardization | high | moderate | OCPP (with certification) and ISO 15118 are open standards, so back ends interoperate with certified chargers | [OCA-MIG], [ISO15118] |
| `deployment:ev-charging-mgmt` | differentiation | moderate | low | back ends differ in roaming, billing and smart-charging features | [OCA-MIG] |
| `deployment:ev-charging-mgmt` | commoditization | Unknown | - | no public evidence on whether back ends compete mainly on price | row's own notes and sources |
| `deployment:ev-charging-mgmt` | substitutability | easy | moderate | chargers can be pointed to a new OCPP back end by configuration, without hardware changes | [OCA-MIG] |
| `deployment:ev-charging-mgmt` | qualification_time | Unknown | - | migrations need both back ends in parallel; a 35,000-point network move runs over a year, small ones months; no typical figure | [OCA-MIG] |
| `deployment:ev-charging-mgmt` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `deployment:ev-charging-mgmt` | adjacent_innovation | partly | low | smart charging and vehicle-to-grid run through the back end | [ISO15118], [DOE-VPP] |
| `recovery (whole cell)` | standardization | low | low | pack designs, chemistries and formats vary widely, which hampers standardised dismantling | [HARPER] |
| `recovery (whole cell)` | differentiation | Unknown | - | no traded basket and a nascent market | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | no traded basket and a nascent market | row's own notes and sources |

### Gas

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | high | high | pipeline gas and LNG are delivered to network quality specs and are fungible once import capacity exists | [GAS-HUBS], row's own notes and sources |
| `extraction (whole cell)` | differentiation | low | high | no supplier differentiation beyond delivery terms | [GAS-HUBS] |
| `extraction (whole cell)` | commoditization | yes | moderate | hub-priced with deep futures markets (Henry Hub, TTF, JKM), although pipeline supply is regionally segmented | [GAS-HUBS] |
| `refining (whole cell)` | standardization | high | moderate | propane and butane are sold to standard LPG specifications | [LPG] |
| `refining (whole cell)` | differentiation | low | moderate | spec products | [LPG] |
| `refining (whole cell)` | commoditization | yes | moderate | fungible seaborne cargoes priced off public benchmarks | [LPG], row's own notes and sources |
| `components (whole cell)` | standardization | low | low | OEM-specific spares (the Nord Stream turbine dispute showed one part can be held up) | row's own notes and sources |
| `components (whole cell)` | differentiation | moderate | low | the OEM holds hot-section design IP; non-OEM parts and repair suppliers compete for some parts | [AFTERMKT] |
| `components (whole cell)` | commoditization | no | low | bought on fit and availability for a given machine; no liquid market | row's own notes and sources |
| `assembly (whole cell)` | standardization | moderate | low | line pipe and tanks are built to API standards; gas turbines, compressors and LNG trains are engineered OEM designs | [API], row's own notes and sources |
| `assembly (whole cell)` | differentiation | moderate | low | heavy-duty gas turbines from few OEMs differ in efficiency and hydrogen capability; pipe and tanks little | row's own notes and sources |
| `assembly (whole cell)` | commoditization | partly | low | line pipe and tanks compete on price; turbines and LNG plant do not | [API] |
| `assembly:gas-ot-control` | standardization | moderate | low | protocols and security standards are common, but control platforms are vendor-proprietary | [IEC62443] |
| `assembly:gas-ot-control` | differentiation | moderate | low | platforms differ in architecture and applications | [IEC62443] |
| `assembly:gas-ot-control` | commoditization | no | low | site-specific system integration projects | [IEC62443] |
| `assembly:gas-ot-control` | substitutability | moderate | low | several vetted ICS vendors exist (separable), but migrating a control system is a major project | [IEC62443], row's own notes and sources |
| `assembly:gas-ot-control` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |

### Geothermal

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | Unknown | - | no traded product: the resource is site-bound | row's own notes and sources |
| `extraction (whole cell)` | differentiation | Unknown | - | no traded product: the resource is site-bound | row's own notes and sources |
| `extraction (whole cell)` | commoditization | Unknown | - | no traded product: the resource is site-bound | row's own notes and sources |
| `refining (whole cell)` | standardization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `refining (whole cell)` | differentiation | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `refining (whole cell)` | commoditization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `components (whole cell)` | standardization | Unknown | - | no HS6 basket or supply measure; the mix of generic tubulars and design-specific turbine and heat-exchanger parts is not quantified | row's own notes and sources |
| `components (whole cell)` | differentiation | Unknown | - | no HS6 basket or supply measure; the mix of generic tubulars and design-specific turbine and heat-exchanger parts is not quantified | row's own notes and sources |
| `components (whole cell)` | commoditization | Unknown | - | no HS6 basket or supply measure; the mix of generic tubulars and design-specific turbine and heat-exchanger parts is not quantified | row's own notes and sources |
| `assembly (whole cell)` | standardization | low | low | plants and turbines are designed to each resource's temperature, pressure and fluid chemistry | [DIPIPPO], [IEA-GEO24] |
| `assembly (whole cell)` | differentiation | moderate | low | a handful of steam-turbine and ORC makers with proprietary designs | [DIPIPPO], row's own notes and sources |
| `assembly (whole cell)` | commoditization | no | low | engineered to order | [DIPIPPO] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |

### Green Hydrogen

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | high | low | PGMs and nickel are sold as refined metal to good-delivery or exchange specs | [LPPM], [LME] |
| `extraction (whole cell)` | differentiation | low | low | spec metal | [LPPM] |
| `extraction (whole cell)` | commoditization | partly | low | platinum and nickel are exchange-traded; iridium is a thin by-product market without an exchange | [LPPM], [JM] |
| `extraction:h2-pgm` | standardization | high | moderate | platinum is sold to good-delivery specs; iridium as sponge or powder to purity specs | [LPPM], [JM] |
| `extraction:h2-pgm` | differentiation | low | moderate | spec metals | [LPPM] |
| `extraction:h2-pgm` | commoditization | partly | moderate | platinum is exchange-traded; iridium is a thin by-product market priced by refiners | [JM] |
| `refining (whole cell)` | standardization | moderate | low | refined PGMs and nickel are spec-graded; catalyst materials are formulated per customer | [JM], [IEA-GHR24] |
| `refining (whole cell)` | differentiation | moderate | low | catalyst loading and activity differ by supplier; cutting iridium loading is an active innovation front | [IEA-GHR24] |
| `refining (whole cell)` | commoditization | partly | low | metals are exchange-priced; catalysts are customer-qualified | [JM] |
| `components (whole cell)` | standardization | low | moderate | stack designs (alkaline, PEM, solid oxide, AEM) are proprietary and not interchangeable; standards cover system safety, not stack interfaces | [ISO22734], [IEA-GHR24] |
| `components (whole cell)` | differentiation | high | moderate | efficiency, current density, durability and materials use differ widely across makers and technologies | [IEA-GHR24] |
| `components (whole cell)` | commoditization | no | moderate | an early, scaling market | [IEA-GHR24] |
| `assembly (whole cell)` | standardization | low | low | systems are configured per project; balance of plant is generic but its integration is not | [ISO22734], [IEA-GHR24] |
| `assembly (whole cell)` | differentiation | moderate | low | Chinese alkaline systems sell well below Western prices while Western makers compete on efficiency, footprint and flexibility | [IEA-GHR24] |
| `assembly (whole cell)` | commoditization | no | low | an emerging market | [IEA-GHR24] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | empty cell: no defined product, HS6 basket or supply measure | row's own notes and sources |

### Nuclear

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | high | moderate | U3O8 concentrate is delivered to converter specifications; a standard commodity | [ASTM-U], row's own notes and sources |
| `extraction (whole cell)` | differentiation | low | moderate | spec product | [ASTM-U] |
| `extraction (whole cell)` | commoditization | partly | moderate | priced on spot and term indicators, but most volume moves under long-term contracts in a thin, concentrated market | [WNA-FUEL] |
| `refining (whole cell)` | standardization | high | moderate | conversion and enrichment deliver UF6 to ASTM specs; enriched uranium is fungible across enrichers (swaps are common) | [ASTM-U], [WNA-ENR] |
| `refining (whole cell)` | differentiation | low | moderate | a homogeneous product (SWU) | [WNA-ENR] |
| `refining (whole cell)` | commoditization | partly | moderate | competes on price and reliability, but with four enrichment suppliers and long-term contracts, not a liquid market | [WNA-ENR] |
| `refining:nuc-haleu` | standardization | moderate | low | specified by enrichment level, but the delivered form (UF6, metal or oxide) depends on the reactor design | [DOE-HALEU] |
| `refining:nuc-haleu` | differentiation | low | low | a homogeneous enrichment product | [DOE-HALEU] |
| `refining:nuc-haleu` | commoditization | no | moderate | one commercial-scale supplier and an emerging market | [DOE-HALEU], [YF] |
| `refining:nuc-leu` | standardization | high | moderate | enriched UF6 is delivered to ASTM specs and is fungible across enrichers | [ASTM-U], [WNA-ENR] |
| `refining:nuc-leu` | differentiation | low | moderate | a homogeneous product (SWU) | [WNA-ENR] |
| `refining:nuc-leu` | commoditization | partly | moderate | competes on price and reliability, but four suppliers and long-term contracts | [WNA-ENR] |
| `components (whole cell)` | standardization | low | moderate | fuel assemblies and reactor parts are design-specific and licensed reactor by reactor | [ESA23], row's own notes and sources |
| `components (whole cell)` | differentiation | high | moderate | fuel and component designs are vendor IP | [ESA23] |
| `components (whole cell)` | commoditization | no | moderate | design-segmented, licensed supply | [ESA23] |
| `components:nuc-fuel-fab` | standardization | low | moderate | assemblies are design-specific and licensed reactor by reactor | [ESA23], row's own notes and sources |
| `components:nuc-fuel-fab` | differentiation | high | moderate | fuel designs are vendor IP | [ESA23] |
| `components:nuc-fuel-fab` | commoditization | no | moderate | a design-segmented, licensed market | [ESA23] |
| `assembly (whole cell)` | standardization | low | moderate | each reactor design is licensed separately and major equipment is built to order | [IEA-NUC25], row's own notes and sources |
| `assembly (whole cell)` | differentiation | high | moderate | reactor designs are vendor IP | [IEA-NUC25] |
| `assembly (whole cell)` | commoditization | no | moderate | engineered to order | [IEA-NUC25] |
| `assembly:nuc-reactor-controls` | standardization | low | moderate | safety I&C platforms are licensed individually and integrated plant by plant | [NRC-ISG06] |
| `assembly:nuc-reactor-controls` | differentiation | moderate | low | several qualified digital platforms differ in architecture | [NRC-ISG06] |
| `assembly:nuc-reactor-controls` | commoditization | no | moderate | licensed, plant-specific systems | [NRC-ISG06] |
| `assembly:nuc-reactor-controls` | substitutability | moderate | low | several nuclear-qualified digital I&C platforms exist, but a replacement needs a licence amendment | [NRC-ISG06] |
| `assembly:nuc-reactor-controls` | qualification_time | gt3y | low | the Diablo Canyon digital protection-system replacement ran from its licence amendment request (Oct 2011) through NRC supplements and audits in 2013-14, before installation | [NRC-ISG06] |
| `assembly:nuc-reactor-controls` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:nuc-reactor-controls` | workforce_suppliers | yes | low | nuclear-qualified digital I&C needs suppliers with nuclear QA programmes and NRC-reviewed platforms, a small supplier base | [NRC-ISG06] |
| `assembly:nuc-reactor-controls` | adjacent_innovation | partly | low | digital I&C platforms and qualification experience carry into advanced-reactor I&C | [LWRS] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | reprocessing services are bespoke contracts at very few sites; evidence on product standardisation is thin | [WNA-REPRO] |
| `recovery (whole cell)` | differentiation | Unknown | - | evidence is thin | [WNA-REPRO] |
| `recovery (whole cell)` | commoditization | no | low | one or two commercial providers; no market | [WNA-REPRO] |

### Oil

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | high | high | crude is traded by grade and assay against benchmark crudes | [OIL-SPECS] |
| `extraction (whole cell)` | differentiation | low | high | grade differentials, not supplier IP | [OIL-SPECS] |
| `extraction (whole cell)` | commoditization | yes | high | benchmark-priced in deep, liquid, multi-supplier markets | [OIL-SPECS], [IEA-OIL24] |
| `refining (whole cell)` | standardization | high | high | products are made to standard fuel specifications | [OIL-SPECS], row's own notes and sources |
| `refining (whole cell)` | differentiation | low | high | spec products | [OIL-SPECS] |
| `refining (whole cell)` | commoditization | yes | high | benchmark-priced, liquid, multi-supplier markets | [IEA-OIL24] |
| `components (whole cell)` | standardization | moderate | low | parts of standard oilfield equipment, much of it built to API specifications | [API], row's own notes and sources |
| `components (whole cell)` | differentiation | Unknown | - | no public evidence on how these parts compete | row's own notes and sources |
| `components (whole cell)` | commoditization | Unknown | - | no public evidence on how these parts compete | row's own notes and sources |
| `assembly (whole cell)` | standardization | moderate | low | line pipe, wellheads and tanks are built to API standards; rigs, tankers and gensets are engineered products | [API] |
| `assembly (whole cell)` | differentiation | moderate | low | little among pipe and tanks, more among rigs and engineered equipment | [API] |
| `assembly (whole cell)` | commoditization | partly | low | standard pipe and tanks compete on price; engineered equipment does not | [API] |
| `assembly:oil-ot-control` | standardization | moderate | low | protocols and security standards are common, but control platforms are vendor-proprietary | [IEC62443] |
| `assembly:oil-ot-control` | differentiation | moderate | low | platforms differ in architecture and applications | [IEC62443] |
| `assembly:oil-ot-control` | commoditization | no | low | site-specific system integration projects | [IEC62443] |
| `assembly:oil-ot-control` | substitutability | moderate | low | several vetted ICS vendors exist (separable), but migrating a control system is a major project | [IEC62443], row's own notes and sources |
| `assembly:oil-ot-control` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | public evidence on the traded market for waste oils is thin (HS 2710.91/99 are absent from the Atlas panel) | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | public evidence on the traded market for waste oils is thin (HS 2710.91/99 are absent from the Atlas panel) | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | public evidence on the traded market for waste oils is thin (HS 2710.91/99 are absent from the Atlas panel) | row's own notes and sources |

### Solar

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | moderate | low | copper and zinc concentrates and silver are graded on content; quartz grades vary by deposit | [IEA-GCMO24] |
| `extraction (whole cell)` | differentiation | low | low | buyers pay for grade | [IEA-GCMO24] |
| `extraction (whole cell)` | commoditization | partly | low | copper and zinc are index-priced; quartz is sold bilaterally | [LME], [IEA-GCMO24] |
| `refining (whole cell)` | standardization | high | moderate | polysilicon is sold to solar-grade purity specs, silicon metal to standard grades, copper wire to standards | [SEMI-PV17], [IEA-PV22] |
| `refining (whole cell)` | differentiation | low | moderate | graded products with little supplier differentiation | [IEA-PV22] |
| `refining (whole cell)` | commoditization | yes | moderate | spot-priced and price-led amid overcapacity since 2023 | [PV-SPOT], [IEA-REN24] |
| `refining:polysilicon` | standardization | high | moderate | sold to solar-grade purity specs (n-type and p-type grades) | [SEMI-PV17] |
| `refining:polysilicon` | differentiation | low | moderate | graded product with little supplier differentiation | [IEA-PV22] |
| `refining:polysilicon` | commoditization | yes | moderate | spot-priced and price-led amid overcapacity since 2023 | [PV-SPOT], [IEA-REN24] |
| `components (whole cell)` | standardization | moderate | low | wafers, cells and solar glass come in converged formats, but inverter parts and power semiconductors in the basket are device-specific | [IEA-PV22], row's own notes and sources |
| `components (whole cell)` | differentiation | moderate | low | cell architectures (PERC, TOPCon, HJT, back-contact) differ in efficiency; power semiconductors differ in performance | [TOPCON-IP], [DOE-SPE22] |
| `components (whole cell)` | commoditization | partly | low | wafers and cells are spot-priced and price-led; parts and semiconductors are not | [PV-SPOT], [IEA-REN24] |
| `components:pv-cells-wafers` | standardization | high | moderate | wafers and cells come in converged formats (M10, G12) priced in weekly public spot assessments | [PV-SPOT], [IEA-PV22] |
| `components:pv-cells-wafers` | differentiation | moderate | moderate | architectures (PERC, TOPCon, HJT, back-contact) differ in efficiency and are patented | [TOPCON-IP] |
| `components:pv-cells-wafers` | commoditization | partly | moderate | price-led within an architecture amid overcapacity, but rapid architecture turnover keeps efficiency premiums | [PV-SPOT], [IEA-REN24] |
| `components:pv-cells-wafers` | process_knowledge | partly | low | cell lines are process-intensive, but much of the process comes embedded in turnkey equipment; the Y&F curators judged the question contested | [IEA-PV22], [KAVLAK], [YF] |
| `components:pv-cells-wafers` | workforce_suppliers | partly | low | the specialised equipment and materials supply base sits mainly in China and lines elsewhere import it | [IEA-PV22] |
| `components:pv-cells-wafers` | adjacent_innovation | yes | moderate | perovskite-silicon tandems are built on silicon bottom cells, and the tandem pilot lines are run by incumbent cell makers | [TANDEM] |
| `components:pv-cells-wafers` | incumbent_vulnerable | partly | low | tandems extend rather than replace silicon and incumbents lead the pilots; all-perovskite routes could bypass silicon but are unproven | [TANDEM] |
| `components:pv-nextgen` | standardization | low | moderate | no settled product format, and stability qualification protocols are still being developed | [KHENKIN] |
| `components:pv-nextgen` | differentiation | high | moderate | efficiency and durability vary widely by architecture and maker | [KHENKIN], [TANDEM] |
| `components:pv-nextgen` | commoditization | no | moderate | emerging, early-commercial product | [NEIS], [TANDEM] |
| `components:pv-nextgen` | qualification_time | Unknown | - | certification and bankability timelines for perovskite products are not yet established | [KHENKIN] |
| `components:pv-nextgen` | incumbent_vulnerable | partly | low | tandems extend rather than replace silicon and incumbent cell makers lead the pilots; all-perovskite routes could bypass silicon but are unproven | [TANDEM] |
| `assembly (whole cell)` | standardization | high | moderate | modules, the largest item, are certified to IEC 61215/61730 in converged formats; racking is generic steel | [IEC-PV], [IEA-PV22] |
| `assembly (whole cell)` | differentiation | moderate | moderate | modules differ little within a cell architecture, but inverters and trackers in the basket differ in features and firmware | [IEA-PV22] |
| `assembly (whole cell)` | commoditization | partly | moderate | modules are spot-priced commodities at record-low prices; inverters and transformers are differentiated products | [PV-SPOT], [IEA-REN24] |
| `assembly:pv-inverter-firmware-dist` | standardization | moderate | low | grid-support functions follow common standards, but firmware and vendor clouds are proprietary | [GRID-CODES] |
| `assembly:pv-inverter-firmware-dist` | differentiation | moderate | low | monitoring, cloud and control features differ by vendor | [GRID-CODES] |
| `assembly:pv-inverter-firmware-dist` | commoditization | no | low | proprietary firmware and cloud services sold with the inverter | [GRID-CODES] |
| `assembly:pv-inverter-firmware-dist` | substitutability | moderate | low | many certified inverter makers, but firmware is integrated, so switching means replacing inverters; dongles are separable | [GRID-CODES], row's own notes and sources |
| `assembly:pv-inverter-firmware-dist` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:pv-inverter-firmware-dist` | adjacent_innovation | partly | low | smart-inverter functions are standardised, so innovation sits more in aggregation software than device firmware | [GRID-CODES], [DOE-VPP] |
| `assembly:pv-modules` | standardization | high | high | IEC 61215/61730-certified modules in converged formats and connectors are interchangeable across makers | [IEC-PV], [NEIS] |
| `assembly:pv-modules` | differentiation | low | moderate | within a cell architecture, efficiency differences among leading makers are small | [IEA-PV22] |
| `assembly:pv-modules` | commoditization | yes | moderate | spot-priced, many makers, prices at record lows amid overcapacity | [PV-SPOT], [IEA-REN24] |
| `assembly:pv-racking` | standardization | high | moderate | generic steel and aluminium structures (trackers excluded) | [NEIS], [YF] |
| `assembly:pv-racking` | differentiation | low | moderate | passive structures with little performance difference | [NEIS] |
| `assembly:pv-racking` | commoditization | yes | moderate | generic steel fabrication from many suppliers | [YF] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | an emerging activity with no HS6 basket; market structure not established | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | an emerging activity with no HS6 basket; market structure not established | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | an emerging activity with no HS6 basket; market structure not established | row's own notes and sources |

### Wind

| Row | Attribute | Value | Confidence | Reason | Source |
|---|---|---|---|---|---|
| `extraction (whole cell)` | standardization | moderate | low | monazite is sold on rare-earth oxide content; zinc, nickel and copper concentrates on metal content | [IEA-GCMO24] |
| `extraction (whole cell)` | differentiation | low | low | buyers pay for grade | [IEA-GCMO24] |
| `extraction (whole cell)` | commoditization | partly | low | base-metal concentrates are index-priced; rare-earth concentrates trade in opaque, concentrated markets | [IEA-GCMO24] |
| `refining (whole cell)` | standardization | moderate | low | rare-earth oxides and copper wire are spec-graded; carbon fibre comes in maker-specific grades | [IEA-GCMO24], [DOE-WIND22] |
| `refining (whole cell)` | differentiation | moderate | low | carbon-fibre grades differ by maker; oxides and wire do not | [DOE-WIND22] |
| `refining (whole cell)` | commoditization | partly | low | mixed: spec-graded metals and wire alongside differentiated carbon fibre and an opaque rare-earth market | [IEA-GCMO24], [DOE-WIND22] |
| `refining:ree-separation` | standardization | high | moderate | separated oxides and metals are sold to purity specs | [IEA-GCMO24] |
| `refining:ree-separation` | differentiation | low | moderate | spec products | [IEA-GCMO24] |
| `refining:ree-separation` | commoditization | partly | moderate | spec-graded, but priced in opaque, concentrated markets with no liquid exchange contract | [IEA-GCMO24] |
| `components (whole cell)` | standardization | low | low | blades, gearboxes and nacelle parts are made to each OEM's design and qualified into its type certificate | [IEC61400], row's own notes and sources |
| `components (whole cell)` | differentiation | moderate | low | blade aerodynamics and drivetrain reliability are OEM and specialist-supplier IP | [DOE-WIND22] |
| `components (whole cell)` | commoditization | partly | low | towers and castings are build-to-print and price-competed; blades and gearboxes are design-specific | [DOE-WIND22] |
| `components:ree-magnets` | standardization | moderate | moderate | sintered NdFeB comes in standard grade classes, but parts are custom shapes and coatings qualified per motor | [MAGNET-STD], [YF] |
| `components:ree-magnets` | differentiation | moderate | moderate | heavy-rare-earth reduction and high-temperature grades are maker know-how | [DOE-MAG22] |
| `components:ree-magnets` | commoditization | partly | moderate | graded products from many producers, but custom parts, qualification and concentrated supply | [DOE-MAG22], [IEA-GCMO24] |
| `assembly (whole cell)` | standardization | moderate | low | turbines are certified to common IEC 61400 design classes, but each OEM's platform is proprietary | [IEC61400] |
| `assembly (whole cell)` | differentiation | moderate | low | rotor size, reliability and service terms differ; Chinese OEMs compete strongly on price | [DOE-WIND22] |
| `assembly (whole cell)` | commoditization | no | low | sold project by project with long-term service agreements | [DOE-WIND22] |
| `assembly:wind-turbine-control` | standardization | low | moderate | controllers are tied to each OEM's turbine design; only SCADA communications are standardised | [IEC61400] |
| `assembly:wind-turbine-control` | differentiation | high | moderate | control strategies (load reduction, wake steering, grid response) are core OEM know-how | [VEERS] |
| `assembly:wind-turbine-control` | commoditization | no | moderate | OEM-specific | [IEC61400] |
| `assembly:wind-turbine-control` | substitutability | moderate | low | third-party controller retrofits exist for older turbine families; for current platforms the controller is OEM-bound | [WIND-RETRO] |
| `assembly:wind-turbine-control` | process_knowledge | Unknown | - | no manufacturing process; public evidence does not establish production-linked learning for this software layer | row's own notes and sources |
| `assembly:wind-turbine-control` | adjacent_innovation | yes | moderate | wind-plant control is a grand challenge for larger rotors and plant-level optimisation | [VEERS] |
| `deployment (whole cell)` | standardization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | differentiation | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `deployment (whole cell)` | commoditization | Unknown | - | Deployment convention: Deployment is use, not a traded good, and no capability case is tested there, so the product-market attributes are not assessed | row's own notes and sources |
| `recovery (whole cell)` | standardization | Unknown | - | an emerging activity with no HS6 basket; market structure not established | row's own notes and sources |
| `recovery (whole cell)` | differentiation | Unknown | - | an emerging activity with no HS6 basket; market structure not established | row's own notes and sources |
| `recovery (whole cell)` | commoditization | Unknown | - | an emerging activity with no HS6 basket; market structure not established | row's own notes and sources |

## Sources

* **[AFTERMKT]** company literature on non-OEM turbine parts and repair (e.g. Sulzer, EthosEnergy)
* **[API]** API Spec 5L (line pipe), 5CT (casing and tubing), 6A (wellhead equipment) and API 650 (storage tanks)
* **[ASME]** ASME Boiler and Pressure Vessel Code
* **[ASTM-U]** ASTM C967 (uranium ore concentrate) and ASTM C996 (enriched uranium hexafluoride)
* **[BESS-PROTO]** SunSpec Modbus, IEEE 2030.5, IEC 61850 and DNP3 (IEEE 1815) device communication standards
* **[BIS-CV]** US Department of Commerce, BIS, connected vehicles final rule (January 2025: software from model year 2027, hardware from model year 2030); Alliance for Automotive Innovation comments on the proposed rule (2024)
* **[CABLE-STD]** IEC 60502 and IEC 60840 (power cables)
* **[CELL-STD]** UN Manual of Tests and Criteria 38.3; IEC 62619; UL 1973 and UL 9540A (battery cell and system safety standards)
* **[CIM]** IEC 61970 and IEC 61968 (Common Information Model); IEC 60870-5-104; DNP3 (IEEE 1815); IEC 61850
* **[COAL]** globalCOAL Standard Coal Trading Agreement (SCoTA); API2 and Newcastle thermal coal indices
* **[DIPIPPO]** DiPippo, Geothermal Power Plants, 4th ed. (Butterworth-Heinemann, 2016)
* **[DLMS]** IEC 62056 (DLMS/COSEM); GB SMETS2 technical specification (DESNZ)
* **[DOE-GRID22]** US DOE, Electric Grid Supply Chain Review: Large Power Transformers and High Voltage Direct Current Systems (2022)
* **[DOE-HALEU]** US DOE HALEU Availability Program
* **[DOE-LPT14]** US DOE, Large Power Transformers and the U.S. Electric Grid (2014)
* **[DOE-MAG22]** US DOE, Rare Earth Permanent Magnets Supply Chain Deep Dive Assessment (2022)
* **[DOE-SPE22]** US DOE, Semiconductor Power Electronics Supply Chain Deep Dive Assessment (2022)
* **[DOE-VPP]** US DOE, Pathways to Commercial Liftoff: Virtual Power Plants (2023)
* **[DOE-WIND22]** US DOE, Wind Energy Supply Chain Deep Dive Assessment (2022)
* **[EMS-REPL]** utility EMS and SCADA replacement records: ESO (Bulgaria) SCADA/EMS modernisation subproject, Jan 2023 to Mar 2026; Guam Power Authority SCADA/EMS project (OSTI 433827)
* **[ESA23]** Euratom Supply Agency, Annual Report 2023
* **[GAS-HUBS]** IEA Gas Market Report (hub-priced gas: Henry Hub, TTF, JKM)
* **[GOES-STD]** ASTM A876 and IEC 60404-8-7 (grain-oriented electrical steel grades)
* **[GRID-CODES]** IEEE 1547-2018, IEEE 2800-2022 and EN 50549 (grid-support requirements for inverter-based resources); UL 1741; IEC 62109
* **[HARPER]** Harper et al., Recycling lithium-ion batteries from electric vehicles, Nature 575 (2019)
* **[IEA-BAT24]** IEA, Batteries and Secure Energy Transitions (2024)
* **[IEA-GCMO24]** IEA Global Critical Minerals Outlook 2024
* **[IEA-GEO24]** IEA, The Future of Geothermal Energy (2024)
* **[IEA-GEVO25]** IEA Global EV Outlook 2025
* **[IEA-GHR24]** IEA Global Hydrogen Review 2024
* **[IEA-GRID25]** IEA, Building the Future Transmission Grid (2025)
* **[IEA-NUC25]** IEA, The Path to a New Era for Nuclear Energy (2025)
* **[IEA-OIL24]** IEA Oil 2024
* **[IEA-PV22]** IEA, Special Report on Solar PV Global Supply Chains (2022)
* **[IEA-REN24]** IEA, Renewables 2024 (PV manufacturing overcapacity and price falls)
* **[IEA-STEEL20]** IEA Iron and Steel Technology Roadmap (2020)
* **[IEC-PV]** IEC 61215 and IEC 61730 (PV module design qualification and safety)
* **[IEC61400]** IEC 61400-1 (design classes), IEC 61400-25 (SCADA communications), IECRE OD-501 (type certification)
* **[IEC62443]** IEC 62443 (industrial automation and control system security); OPC UA (IEC 62541); Modbus; DNP3 (IEEE 1815)
* **[INTEROPERA]** InterOPERA project, Enabling interoperability of multi-vendor HVDC grids (Horizon Europe, CORDIS 101095874, 2023-2027)
* **[ISO15118]** ISO 15118-20 (vehicle-to-grid communication); OCPP 2.0.1 smart charging
* **[ISO22734]** ISO 22734 (hydrogen generators using water electrolysis)
* **[ISO668]** ISO 668 (freight container dimensions)
* **[JM]** Johnson Matthey, PGM Market Report (2024)
* **[KAVLAK]** Kavlak, McNerney and Trancik, Evaluating the causes of cost reduction in photovoltaic modules, Energy Policy 123 (2018)
* **[KHENKIN]** Khenkin et al., Consensus statement for stability assessment and reporting for perovskite photovoltaics based on ISOS procedures, Nature Energy 5 (2020)
* **[LI-FUT]** Guangzhou Futures Exchange lithium carbonate futures (listed 2023); CME lithium hydroxide futures (listed 2021)
* **[LME]** London Metal Exchange contract specifications (copper Grade A cathode, primary aluminium, nickel)
* **[LPG]** ASTM D1835 (liquefied petroleum gases); Saudi Aramco contract price and Mont Belvieu LPG price benchmarks
* **[LPPM]** LPPM Good Delivery rules (platinum, palladium)
* **[LWRS]** US DOE Light Water Reactor Sustainability Program (INL), digital I&C modernisation research
* **[MAGNET-STD]** IEC 60404-8-1 (magnetically hard materials, including sintered NdFeB grades)
* **[NEIS]** NEIS, Three Yards for Three Different Problems (discussion draft, Oct 2026), component descriptions (characteristics only)
* **[NLMK-ABB]** Steel Times International, NLMK certified by ABB as qualified supplier of transformer steel (trial batches and core tests)
* **[NRC-ISG06]** US NRC, DI&C-ISG-06 Rev. 2 (2018), licensing process for digital I&C upgrades; PG&E License Amendment Request 11-07, Diablo Canyon process protection system replacement (Oct 2011), NRC acceptance (Jan 2012) and review record (2013-14)
* **[NREL-GFM]** Lin et al., Research Roadmap on Grid-Forming Inverters, NREL/TP-5D00-73476 (2020)
* **[OCA-MIG]** Open Charge Alliance, white paper on OCPP-based migration of charging stations (v5.0 2023; 2025 update); public reporting on Allego's back-end migration of about 35,000 charge points (completion due Q4 2026)
* **[OIL-SPECS]** crude benchmarks (Brent, WTI, Dubai); ASTM D975 and EN 590 (diesel); ASTM D1655 (jet fuel)
* **[PLETT]** Plett, Battery Management Systems, Vols I-II (Artech House, 2015)
* **[PV-SPOT]** InfoLink Consulting and PVInsights weekly spot price assessments for polysilicon, wafers, cells and modules (public)
* **[REMA]** ReMA (formerly ISRI), Scrap Specifications Circular
* **[SEMI-PV17]** SEMI PV17, specification for virgin silicon feedstock for photovoltaic applications
* **[TANDEM]** company and trade-press reporting on perovskite-silicon tandem pilot lines (LONGi 100 MW pilot line, pv magazine Aug 2026; JinkoSolar, Trina and Tongwei tandem pilots; GCL perovskite tandem production base, 2025-26)
* **[TOPCON-IP]** public reporting on TOPCon cell patent litigation (2024-25)
* **[TX-STD]** IEC 60076 and IEEE C57 (power transformers); IEC 62271 (switchgear)
* **[UNECE]** UNECE vehicle type-approval regulations; IEC 62196, SAE J1772 and SAE J3400 (charging connectors)
* **[VEERS]** Veers et al., Grand challenges in the science of wind energy, Science 366 (2019)
* **[WIND-RETRO]** DEIF, Mita-Teknik and Bachmann wind-turbine control retrofit offerings (company literature)
* **[WNA-ENR]** World Nuclear Association, Uranium Enrichment (2024)
* **[WNA-FUEL]** World Nuclear Association, The Nuclear Fuel Report (2023)
* **[WNA-REPRO]** World Nuclear Association, Processing of Used Nuclear Fuel (2024)
* **[YF]** yards_fences_assessments.csv (component description and notes; characteristics only)
