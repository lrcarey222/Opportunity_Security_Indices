# Content trace: South Korea and the Allied Industrial Base

Each claim of the brief with the dashboard field it comes from, the source datasets and the evidence quality it rests on.

**s1.2** It holds 36% of world exports or production of cathode materials and precursors.

- `aib.contributes.cathode-materials-and-precursors.share` = 36% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s1.3** It is the largest allied holder of battery components and of gas turbines and LNG plant, and it is the best-placed trusted partner of 65 economies worldwide for at least one step.

- `aib.division.batteries-components.name` = battery components | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.division.gas-assembly.name` = gas turbines and LNG plant | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.recommended_economies` = 65 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s1.7** It is among the most import-dependent economies for oil, gas and coal, relies on China for refined rare earths, and buys 44% of its imported uranium conversion and enrichment from Russia.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.nuclear-refining.top_supplier_share` = 44% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-refining.top_supplier` = Russia | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s1.12** Imported oil, gas and coal; the rare-earth chain from separation to magnets, where China holds 94% of world refining supply and trusted suppliers only 4%; uranium conversion and enrichment; and the control software of grids, batteries and inverters.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s1.19** Source them from trusted producers, such as the United States, Canada and Australia for minerals and Germany, Belgium and Spain for wind materials, and add trusted capacity through co-investment and offtake where allied supply falls short.

- `cells.batteries-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate
- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**s1.28** In battery cells and packs, the battery software is Secure while the cells and enclosures are Preserve; in wafers and PV cells, a Watch step, the cell layers are Preserve.

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.2** Secure is a broad call for South Korea: five whole steps and 14 layers inside other steps, which group into ten capabilities.

- `counts.secure` = five | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.secure_layers` = 14 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.high_capabilities` = ten | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.11** China holds 94% of world refining supply and provides 62% of imports; substitution is hard and rebuilding takes more than three years.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.china_share` = 62% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.substitutability` = hard | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.reconstitution` = more than three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**s2.13** Qualified supply from trusted refiners such as the United States, Australia and Japan through offtake, public finance and allied agreements; stockpiles and recycling until it qualifies.

- `cells.electric-vehicles-refining.partners` = the United States, Australia and Japan | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s2.16** Russia supplies 44% of imports; substitution is impossible and rebuilding takes more than three years.

- `cells.nuclear-refining.top_supplier` = Russia | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-refining.top_supplier_share` = 44% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-refining.substitutability` = impossible | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high
- `cells.nuclear-refining.reconstitution` = more than three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s2.18** For the national-security end uses: long-term contracts, co-investment and shared stockpiles with trusted processors such as Canada, the United Kingdom and France.

- `cells.nuclear-refining.partners` = Canada, the United Kingdom and France | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence strong

**s2.20** Fuel power, industry and transport; rebuilding supply takes one to three years.

- `cells.oil-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s2.21** Import dependence ranks 97 (oil), 97 (gas) and 96 (coal) out of a hundred among importing economies (net imports against GDP).

- `cells.oil-extraction.import_dependence` = 97 | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.gas-extraction.import_dependence` = 97 | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.coal-extraction.import_dependence` = 96 | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.22** Saudi Arabia supplies 34% of crude imports, and allied oil supply falls short.

- `cells.oil-extraction.top_supplier` = Saudi Arabia | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-extraction.top_supplier_share` = 34% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.24** Stocks sized to the rebuild time; long-term and capacity-reservation contracts with trusted producers such as Canada, Norway and the United States for oil and Norway, the United States and Canada for gas.

- `cells.oil-extraction.partners` = Canada, Norway and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak
- `cells.gas-extraction.partners` = Norway, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**s2.27** South Korea builds battery and grid hardware but also imports it: China supplies 81% of battery cell and pack imports; the data do not measure who supplies the firmware or remote-access software.

- `cells.batteries-assembly.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.china_share` = 81% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.31** Grain-oriented electrical steel reads Secure only on the measures of its step, grid metals refining, where South Korea has no measurable position (0).

- `cells.electric-grid-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.36** Battery cells and packs, EV drivetrain components and EV and charger manufacturing combine top-quarter positions (82, 90 and 85) with high value and high confidence.

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.cp` = 82 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-components.cp` = 90 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.cp` = 85 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.37** Battery components reach the top quarter too (76), on four anchors from operating capacity to committed investment, with moderate confidence.

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.cp` = 76 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.anchors` = four | posture detail cells[].pathway_basis, pathway_credible | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.confidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.38** PV modules and inverters (84) and transformers, switchgear and cables (89) also read Preserve, on moderate evidence.

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.solar-assembly.cp` = 84 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.cp` = 89 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.39** Inside the Watch step wafers and PV cells, the cell layers read Preserve, and the AIB gives South Korea a core seat in solar manufacturing.

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.40** Reactors and steam generators sit exactly on the top-quarter bar, a narrow lead on trade baskets that mix reactor lines with generic boiler and steam-turbine parts, so the position may be overstated.

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.42** The strongest case is battery materials refining.

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s2.43** It reads a Competitive Position of 0, yet South Korea holds 3% of world exports: the zero understates a real industry.

- `cells.batteries-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.export_share` = 3% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.44** Nuclear fuel and reactor parts (71) and blades, towers and nacelle parts (45) rest on shares of world supply and exports; in wind towers the AIB counts South Korea a supplier and the second-placed host for new allied capacity.

- `cells.nuclear-components.name` = Nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-components.cp` = 71 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-components.cp` = 45 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.46** Gas turbines and LNG plant (59) and drilling rigs, tankers and oil equipment (52) rest on what looks like a shared export line, and gas-turbine and drilling components partly on the gas plant step above it.

- `cells.gas-assembly.name` = Gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.gas-assembly.cp` = 59 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.oil-assembly.name` = drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.oil-assembly.cp` = 52 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.gas-components.name` = gas-turbine and drilling components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s2.48** Wind turbines take their anchor from cables, transformers and switchgear the step's trade data share with the grid, not from turbines.

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.49** Battery recycling rests on exports of battery waste and scrap, which show where scrap goes, not recycling capacity.

- `cells.batteries-recovery.name` = Battery recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**s2.52** Dependence should be deliberate and spread across trusted producers: the United States, Canada and Australia for battery and EV minerals and Germany, Belgium and Spain for rare-earth and wind materials.

- `cells.batteries-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate
- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**s2.53** Drilling and pump components can come from the United States, Canada and the Netherlands.

- `cells.oil-components.partners` = the United States, Canada and the Netherlands | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate; partner evidence weak

**s2.54** In rare-earth and wind materials refining, allied supply falls short, so a purchase-only strategy will not hold.

- `lists.partner_gap` = rare-earth and wind materials refining | derived: lists over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.55** Grid components read Preserve on a position of 88, but the call is flagged for review by the engine, so this brief does not build on it.

- `cells.electric-grid-components.name` = Grid components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low
- `cells.electric-grid-components.cp` = 88 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low
- `cells.electric-grid-components.review` = flagged for review by the engine | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**s2.57** Dashed lines mark the value bar (60) and the top quarter (75).

- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.68** Electricity meters

- `shared.electricity-meters.label` = Electricity meters | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.69** transformers, switchgear and cables (Preserve); PV modules and inverters (Preserve); and wind turbines (Catch Up)

- `shared.electricity-meters.steps` = transformers, switchgear and cables (Preserve); PV modules and inverters (Preserve); and wind turbines (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.70** The choices differ across technologies with each step's own measures (transformers, switchgear and cables: need 46, value 61, position 89; PV modules and inverters: need 23, value 82, position 84; wind turbines: need 64, value 89, position 56)

- `shared.electricity-meters.reading` = The choices differ across technologies with each step's own measures (transformers, switchgear and cables: need 46, value 61, position 89; PV modules and inverters: need 23, value 82, position 84; wind turbines: need 64, value 89, position 56) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.71** Allies need new capacity here (BUILD); no supplier role

- `shared.electricity-meters.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.72** Ferrite & other magnets

- `shared.ferrite-and-other-magnets.label` = Ferrite & other magnets | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.73** EV drivetrain components (Preserve) and blades, towers and nacelle parts (Catch Up)

- `shared.ferrite-and-other-magnets.steps` = EV drivetrain components (Preserve) and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.74** The choices differ across technologies with each step's own measures (EV drivetrain components: need 97, value 93, position 90; blades, towers and nacelle parts: need 82, value 89, position 45)

- `shared.ferrite-and-other-magnets.reading` = The choices differ across technologies with each step's own measures (EV drivetrain components: need 97, value 93, position 90; blades, towers and nacelle parts: need 82, value 89, position 45) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.75** Allies need new capacity here (BUILD); no supplier role

- `shared.ferrite-and-other-magnets.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.76** Insulators & bushings

- `shared.insulators-and-bushings.label` = Insulators & bushings | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.77** grid components (Preserve, review: flagged for review by the engine); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up)

- `shared.insulators-and-bushings.steps` = grid components (Preserve, review: flagged for review by the engine); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.78** The choices differ across technologies with each step's own measures (grid components: need 70, value 47, position 88; wafers and PV cells: need 31, value 59, position 78; blades, towers and nacelle parts: need 82, value 89, position 45)

- `shared.insulators-and-bushings.reading` = The choices differ across technologies with each step's own measures (grid components: need 70, value 47, position 88; wafers and PV cells: need 31, value 59, position 78; blades, towers and nacelle parts: need 82, value 89, position 45) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.79** Allies need new capacity here (BUILD); no supplier role

- `shared.insulators-and-bushings.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.80** Power cables

- `shared.power-cables.label` = Power cables | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.81** transformers, switchgear and cables (Preserve); EV and charger manufacturing (Preserve); PV modules and inverters (Preserve); and wind turbines (Catch Up)

- `shared.power-cables.steps` = transformers, switchgear and cables (Preserve); EV and charger manufacturing (Preserve); PV modules and inverters (Preserve); and wind turbines (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.82** The choices differ across technologies with each step's own measures (transformers, switchgear and cables: need 46, value 61, position 89; EV and charger manufacturing: need 39, value 93, position 85; PV modules and inverters: need 23, value 82, position 84; wind turbines: need 64, value 89, position 56)

- `shared.power-cables.reading` = The choices differ across technologies with each step's own measures (transformers, switchgear and cables: need 46, value 61, position 89; EV and charger manufacturing: need 39, value 93, position 85; PV modules and inverters: need 23, value 82, position 84; wind turbines: need 64, value 89, position 56) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.83** Allies need new capacity here (BUILD); no supplier role

- `shared.power-cables.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.88** Oil refining is an established export business, with 6% of world supply; it needs no fence.

- `cells.oil-refining.share` = 6% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.90** The solar-grade polysilicon layer reads Catch Up, but only on its step's export share, and South Korea's position in the step is 0.

- `cells.solar-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s3.2** In the Allied Industrial Base analysis, which protects the demand of a trusted network of 64 economies, South Korea is a core member of the trusted network.

- `aib.network.size` = 64 | AIB strategic.countries[].slots and items | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.network.tier` = a core member of the trusted network | AIB strategic.countries[].slots and items | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.3** It is a Scale Manufacturing & Integration Platform with specialized technology role, and it ranks first among allies on outward cleantech-manufacturing FDI.

- `aib.role` = Scale Manufacturing & Integration Platform with specialized technology role | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.first_on` = outward cleantech-manufacturing FDI | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.4** It holds 36% of world exports or production of cathode materials and precursors, 17% of integrated circuits and 16% of accumulator parts and separators.

- `aib.contributes.cathode-materials-and-precursors.share` = 36% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.integrated-circuits.share` = 17% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.accumulator-parts-and-separators.share` = 16% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.5** It is the largest allied holder in battery components, with Japan and Poland the next holders, and in gas turbines and LNG plant, ahead of Germany and the United States.

- `aib.division.batteries-components.name` = battery components | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.division.batteries-components.backups` = Japan and Poland | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.division.gas-assembly.name` = gas turbines and LNG plant | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.division.gas-assembly.backups` = Germany and the United States | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.6** It is the best-placed trusted partner of 29 economies for gas turbines and LNG plant, 27 for battery components and 16 for battery materials refining.

- `aib.recommended_by.gas-assembly.n` = 29 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run
- `aib.recommended_by.batteries-components.n` = 27 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run
- `aib.recommended_by.batteries-refining.n` = 16 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s3.7** It holds a role in all 10 strategic compacts and a core seat in Battery Materials & Cells, Semiconductors & Power Electronics and Solar Manufacturing.

- `aib.n_compact_roles` = 10 | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.core_in` = Battery Materials & Cells, Semiconductors & Power Electronics and Solar Manufacturing | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.10** If China's supply stopped, as in the Decoupling scenario, South Korea would lose US$38.8 billion a year of imports across the Allied Industrial Base basket.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.lost_usd` = US$38.8 billion | AIB network.countries[].lostUsd | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.11** New capacity at the best-placed hosts, led by Singapore, would restore about US$22.7 billion of it a year.

- `aib.investment_decoupling.top_host` = Singapore | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.investment_decoupling.total` = US$22.7 billion | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.12** Under Managed Interdependence, the default scenario, South Korea sits in the minimum coalition of five Sectoral Compacts (a different grouping from the strategic compacts above); under Decoupling it sits in 14 and is a demand anchor in 13 of them.

- `aib.scenarios.managed.label` = Managed Interdependence | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_managed` = five | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_decoupling` = 14 | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_demand_anchor` = 13 | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.20** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.21** Cathode materials (36%), accumulator parts (16%) and cells

- `aib.contributes.cathode-materials-and-precursors.share` = 36% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.accumulator-parts-and-separators.share` = 16% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.23** the United States, Canada and Australia for minerals

- `cells.batteries-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s3.25** Permanent Magnets & Drive Systems

- `aib.compacts.SC-MAGNETS.name` = Permanent Magnets & Drive Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.26** EV drivetrains and non-oriented electrical steel (20% of world exports or production of the steel; no supplier role in magnets)

- `aib.compacts.SC-MAGNETS.lead_share` = 20% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.28** the United States, Australia and Japan for refining; the compact's makers, Germany, Japan, the United States and Mexico

- `cells.electric-vehicles-refining.partners` = the United States, Australia and Japan | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate
- `aib.compacts.SC-MAGNETS.core_makers` = Germany, Japan, the United States and Mexico | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.30** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.31** Fuel assemblies and zirconium (10% of world exports or production) and reactors

- `aib.compacts.SC-NUCLEAR.lead_share` = 10% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.33** Canada, the United Kingdom and France

- `cells.nuclear-refining.partners` = Canada, the United Kingdom and France | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence strong

**s3.35** Equipment allies buy: Solar Manufacturing, Grid Equipment & Conductors and gas plant

- `aib.compacts.SC-SOLAR.name` = Solar Manufacturing | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.36** Solar cells (8%), transformers (9%) and gas and LNG plant; the first-placed host for new power-cable capacity

- `aib.compacts.SC-SOLAR.lead_share` = 8% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.compacts.SC-GRID.lead_share` = 9% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.38** The compacts' other makers, which share the producing role rather than buy it, such as the United States and Japan in solar

- `aib.compacts.SC-SOLAR.core_makers` = the United States and Japan | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s4.6** Of 47 pairs, 31, with 16 developing partners across six steps, rest on Korean need: rare-earth and wind materials refining; battery materials refining; EV drivetrain components; battery components; battery cells and packs; and blades, towers and nacelle parts.

- `dev.n_pairs` = 47 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_pairs` = 31 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_partners` = 16 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_steps` = six | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.own_steps` = rare-earth and wind materials refining; battery materials refining; EV drivetrain components; battery components; battery cells and packs; and blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.7** Another 16 rest on the allied gap alone.

- `dev.n_allied_pairs` = 16 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.9** Indonesia, Malaysia and Mexico are upper-middle-income economies, so finance, guarantees and offtake should lead.

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.10** Viet Nam and the Philippines, both lower middle income, need groundwork first: project preparation, industrial infrastructure and skills.

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.17** Rare-earth and wind materials refining: high need, allied supply falls short and new allied capacity needed

- `dev.rows.r1.capability` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.problem` = high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.18** Viet Nam (lower middle income)

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.19** A share of world supply and a share of world exports; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r1.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r1.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.20** Plant capacity not covered by the data and input costs not covered by the data.

- `dev.rows.r1.unobserved` = Plant capacity not covered by the data and input costs not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.21** Feasibility studies and project preparation, industrial infrastructure, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r1.tools` = Feasibility studies and project preparation, industrial infrastructure, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.22** A trusted second source and allied capacity where allied supply falls short (allies need new capacity in 2 product groups of the step)

- `dev.rows.r1.donor_gain` = A trusted second source and allied capacity where allied supply falls short | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.23** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r1.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.24** Battery materials refining: high need and new allied capacity needed

- `dev.rows.r2.capability` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.25** the Philippines (lower middle income)

- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.26** A share of world exports and operating capacity; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r2.anchors` = A share of world exports and operating capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r2.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.27** Constraint: input costs outside the cheapest quarter.

- `dev.rows.r2.constraints` = input costs outside the cheapest quarter | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.28** Industrial infrastructure, feasibility studies and project preparation, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r2.tools` = Industrial infrastructure, feasibility studies and project preparation, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.29** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 6 product groups of the step)

- `dev.rows.r2.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.n_build` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.30** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r2.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.31** EV drivetrain components: high need and new allied capacity needed

- `dev.rows.r3.capability` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.32** Indonesia (upper middle income)

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.33** A share of world supply, a share of world exports and a strong position in the next step up the chain; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r3.anchors` = A share of world supply, a share of world exports and a strong position in the next step up the chain | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r3.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.34** Plant capacity not covered by the data.

- `dev.rows.r3.unobserved` = Plant capacity not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.35** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for supplier development and technical and vocational skills

- `dev.rows.r3.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for supplier development and technical and vocational skills | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.36** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 7 product groups of the step)

- `dev.rows.r3.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.n_build` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.37** A certified supplier base and export sales of components

- `dev.rows.r3.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.38** Battery cells and packs: high need and new allied capacity needed

- `dev.rows.r4.capability` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.39** Malaysia (upper middle income)

- `dev.rows.r4.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.40** A share of world supply and a share of world exports; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r4.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r4.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.41** Constraint: no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r4.constraints` = no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.42** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r4.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.43** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 2 product groups of the step)

- `dev.rows.r4.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.44** Factory capacity, skills and export sales of finished equipment

- `dev.rows.r4.partner_gain` = Factory capacity, skills and export sales of finished equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.45** Battery components: high need and new allied capacity needed

- `dev.rows.r5.capability` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.46** Mexico (upper middle income)

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.47** A share of world supply and a share of world exports; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r5.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r5.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.48** Constraint: no operating or committed plant in the capacity data; input costs outside the cheapest quarter; and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r5.constraints` = no operating or committed plant in the capacity data; input costs outside the cheapest quarter; and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.49** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r5.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.50** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 1 product group of the step)

- `dev.rows.r5.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.n_build` = 1 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.51** A certified supplier base and export sales of components

- `dev.rows.r5.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.53** The second table shows what ties South Korea to each partner today and what would hold the relationship as the capability scales; aid is the 2020 to 2024 total.

- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.55** In Malaysia and Mexico, the capacity data record no plant while the factory-project data record Korean projects; check that constraint before sizing support.

- `dev.rows.r4.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.59** Viet Nam: rare-earth and wind materials refining

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.60** $0.4 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $36,204 million (economy-wide)

- `dev.rows.r1.ties` = $0.4 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $36,204 million (economy-wide) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.61** Long-term offtake and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Wind & Hydropower Equipment compact; South Korea's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r1.durable` = Long-term offtake and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Wind & Hydropower Equipment compact; South Korea's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.62** the Philippines: battery materials refining

- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.63** $2.0 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $3,145 million (economy-wide); and no export-credit or lender finance recorded at the step

- `dev.rows.r2.ties` = $2.0 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $3,145 million (economy-wide); and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.64** Long-term offtake and investment guarantees as the capability scales; the partner's role as Bridge / coordination partner in the Battery Materials & Cells compact; the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r2.durable` = Long-term offtake and investment guarantees as the capability scales; the partner's role as Bridge / coordination partner in the Battery Materials & Cells compact; the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.65** Indonesia: EV drivetrain components

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.66** $8.8 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $12,750 million (economy-wide); and 1 factory project by firms from South Korea in the technology

- `dev.rows.r3.ties` = $8.8 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $12,750 million (economy-wide); and 1 factory project by firms from South Korea in the technology | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.67** Long-term offtake, joint ventures and investment guarantees as the capability scales; South Korea's own deployment demand for the technology (Accelerate)

- `dev.rows.r3.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; South Korea's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.68** Malaysia: battery cells and packs

- `dev.rows.r4.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.69** $0.6 million of energy, industry and mining aid, none at this step; a direct investment position of $7,336 million (economy-wide); 2 factory projects by firms from South Korea in the technology; and no export-credit or lender finance recorded at the step

- `dev.rows.r4.ties` = $0.6 million of energy, industry and mining aid, none at this step; a direct investment position of $7,336 million (economy-wide); 2 factory projects by firms from South Korea in the technology; and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.70** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Battery Materials & Cells compact

- `dev.rows.r4.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Battery Materials & Cells compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.71** Mexico: battery components

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.capability` = battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.72** Under $0.1 million of energy, industry and mining aid, none at this step; a direct investment position of $4,987 million (economy-wide); 1 factory project by firms from South Korea in the technology; and no export-credit or lender finance recorded at the step

- `dev.rows.r5.ties` = Under $0.1 million of energy, industry and mining aid, none at this step; a direct investment position of $4,987 million (economy-wide); 1 factory project by firms from South Korea in the technology; and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.73** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Capacity-building host in the Battery Materials & Cells compact

- `dev.rows.r5.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Capacity-building host in the Battery Materials & Cells compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.75** 7 developing economies, led by the Philippines and Madagascar, have credible refining pathways; Mozambique, Colombia and Tanzania are review cases, their positions resting on mine and smelter capacity.

- `dev.by_step.batteries-refining.n_development` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.by_step.batteries-refining.development_top` = the Philippines and Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.by_step.batteries-refining.review_capability` = Mozambique, Colombia and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.77** Malaysia and Thailand host its new capacity in the AIB, a commercial route.

- `dev.by_step.electric-vehicles-refining.hosts` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.78** In all, 33 pairs are commercial: the partner already Preserves the step or hosts its capacity, so trade, offtake and investment fit better than aid.

- `dev.n_commercial` = 33 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.80** Aid coded to a partnership's technology reaches only Viet Nam and Malaysia.

- `dev.aid_aligned` = Viet Nam and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.82** Viet Nam, India, the Philippines, Thailand, South Africa, Indonesia and Serbia receive aid at a partnership's step under stage-wide codes that do not identify the technology.

- `dev.aid_stage_wide` = Viet Nam, India, the Philippines, Thailand, South Africa, Indonesia and Serbia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.83** Of the partners, 6 receive it at other steps and 3 at no supply-chain step; a partner with several partnership steps can appear in more than one group.

- `dev.aid_wrong_stage` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_none` = 3 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.84** The largest recipient, Ecuador, received $165 million over 2020 to 2024 and has no partnership here: that aid serves other aims, which this brief does not judge.

- `dev.aid_top_1` = Ecuador | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_top_1_usd` = $165 million | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s5.2** Nuclear power, EV adoption, solar and wind power clear the Accelerate bar of 60.

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s5.3** The national target of 100 GW of renewables by 2030 compares with 37 GW installed in 2025, and at the recent build rate the shortfall is about 45 GW.

- `cells.solar-deployment.target.target_gw` = 100 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.target_year` = 2030 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.installed_gw` = 37 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.installed_year` = 2025 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.shortfall_gw` = 45 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.4** Between 2020 and 2025, solar capacity grew from 14.8 to 30.4 GW and wind only from 1.6 to 2.4 GW.

- `cells.solar-deployment.deployment_years` = 2020 and 2025 | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.deployment_trend` = 14.8 to 30.4 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.deployment_trend` = 1.6 to 2.4 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.9** Geothermal power and heat also clears the bar, driven by system need with moderate confidence; it needs confirming before it becomes a priority.

- `cells.geothermal-deployment.name` = Geothermal power and heat | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**s5.10** Battery storage (56) and grid networks (56) sit just below the bar.

- `cells.batteries-deployment.dn` = 56 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high
- `cells.electric-grid-deployment.dn` = 56 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence moderate; deployment-need confidence high

**s6.7** Uranium conversion and enrichment

- `cells.nuclear-refining.name` = Uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.8** Secure

- `cells.nuclear-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.9** Needs more attention

- `cells.nuclear-refining.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.10** Vulnerability without enough policy attention

- `cells.nuclear-refining.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.11** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.12** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.13** Needs more attention

- `cells.electric-vehicles-refining.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.14** Vulnerability without enough policy attention

- `cells.electric-vehicles-refining.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.15** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.16** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.17** Needs more attention

- `cells.gas-extraction.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.18** Vulnerability without enough policy attention

- `cells.gas-extraction.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.19** Battery materials refining

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.20** Catch Up

- `cells.batteries-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.21** Needs more attention

- `cells.batteries-refining.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.22** Capability case without enough policy attention

- `cells.batteries-refining.gap` = Capability case without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.23** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.24** Accelerate

- `cells.solar-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.25** Deployment policy below the middle

- `cells.solar-deployment.policy_flag` = Deployment policy below the middle | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.26** Deployment needed faster than policy supports

- `cells.solar-deployment.gap` = Deployment needed faster than policy supports | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.27** Wind power

- `cells.wind-deployment.name` = Wind power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.28** Accelerate

- `cells.wind-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.29** Deployment policy below the middle

- `cells.wind-deployment.policy_flag` = Deployment policy below the middle | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.30** Deployment needed faster than policy supports

- `cells.wind-deployment.gap` = Deployment needed faster than policy supports | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.31** Geothermal power and heat

- `cells.geothermal-deployment.name` = Geothermal power and heat | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**s6.32** Accelerate

- `cells.geothermal-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**s6.33** Deployment policy below the middle

- `cells.geothermal-deployment.policy_flag` = Deployment policy below the middle | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**s6.34** Deployment needed faster than policy supports

- `cells.geothermal-deployment.gap` = Deployment needed faster than policy supports | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**s6.35** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s6.36** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s6.37** Heavy protection or localization

- `cells.solar-components.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**s6.38** Localizing what is not a domestic priority

- `cells.solar-components.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**s6.39** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**s6.40** Watch

- `cells.electric-grid-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**s6.41** Heavy local-content protection on the equipment of a Watch deployment

- `cells.electric-grid-deployment.policy_flag` = Heavy local-content protection on the equipment of a Watch deployment | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate; deployment-need confidence high

**s6.42** Localizing what is not a domestic priority

- `cells.electric-grid-deployment.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate; deployment-need confidence high

**s6.43** Oil refining

- `cells.oil-refining.name` = Oil refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.44** Watch

- `cells.oil-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.45** Heavy protection or localization

- `cells.oil-refining.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.46** Localizing what is not a domestic priority

- `cells.oil-refining.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.49** Uranium conversion and enrichment, rare-earth and battery-metal refining and gas production rank low among South Korea's own policy priorities (14, 30 and 22 out of a hundred).

- `cells.nuclear-refining.policy_priority` = 14 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high
- `cells.electric-vehicles-refining.policy_priority` = 30 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.policy_priority` = 22 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.50** Battery materials refining, a Catch Up step, sits below the middle (42 out of a hundred).

- `cells.batteries-refining.policy_priority` = 42 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s7.5** Build trusted capacity with partners through long-term contracts, co-investment and shared stockpiles with trusted processors such as Canada, the United Kingdom and France.

- `cells.nuclear-refining.partners` = Canada, the United Kingdom and France | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence strong

**s7.8** Build diversified, qualified supply with trusted refiners such as the United States, Australia and Japan through offtake, public finance and allied agreements, hold stockpiles until it qualifies, and invest in recycling.

- `cells.electric-vehicles-refining.partners` = the United States, Australia and Japan | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.9** No developing partner qualifies at rare-earth and battery-metal refining; Viet Nam qualifies at rare-earth and wind materials refining, the step that holds the rare-earth separation layer.

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.18** Secure minerals in return through offtake and co-investment with the United States, Canada and Australia.

- `cells.batteries-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.21** Start with the highest-ranked pairs, Viet Nam in rare-earth and wind materials refining and the Philippines in battery materials refining, where groundwork comes first.

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.22** Also start with Indonesia in EV drivetrain components, Malaysia in battery cells and packs and Mexico in battery components, where Korean firms already invest.

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.capability` = battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.2** Every number comes from the Country page data of the Allied Industrial Base Explorer (the data release of 26 November 2026) and from the Allied Industrial Base analysis.

- `meta.as_of_label` = 26 November 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.5** at deployment, Accelerate where deployment need reaches 60;

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.7** Preserve where Competitive Position is in the top quarter (75) and either Strategic Industrial Value (bar 60) or supply-chain need (bar 67) is high;

- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.need` = 67 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.18** Of South Korea's steps, three are review cases.

- `counts.review` = three | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.22** Grid components

- `cells.electric-grid-components.name` = Grid components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.23** Preserve

- `cells.electric-grid-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.24** Flagged for review by the engine

- `cells.electric-grid-components.review` = Flagged for review by the engine | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.25** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.26** Accelerate

- `cells.green-hydrogen-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.27** A single indicator of low confidence

- `cells.green-hydrogen-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.28** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.29** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.30** A single indicator of low confidence

- `cells.oil-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.32** The battery materials Catch Up has high confidence.

- `cells.batteries-refining.confidence` = high | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.33** Its Competitive Position of 0 sits beside an AIB share of 36% in cathode materials: the position measure gives South Korea nothing for plant capacity or investment, while exports and the AIB show a large industry.

- `cells.batteries-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `aib.contributes.cathode-materials-and-precursors.share` = 36% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**a.35** Gas turbines and LNG plant and drilling rigs, tankers and oil equipment show nearly the same share of world exports (11% and 11%), which suggests overlapping trade codes.

- `cells.gas-assembly.export_share` = 11% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.oil-assembly.export_share` = 11% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.36** The oil step's value of 60 only just reaches the bar.

- `cells.oil-assembly.siv` = 60 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.38** The uranium Secure call is a dual-use call, with the fence on the national-security end uses; coercive suppliers provide 50% of South Korea's imports.

- `cells.nuclear-refining.coercive_import_share` = 50% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.41** In all, 18 steps lack the data for a call, including all of geothermal and green hydrogen manufacturing and most recycling.

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.42** Recorded policy ranks clean hydrogen use and electrolyser systems among South Korea's highest priorities (98 and 95 out of a hundred).

- `cells.green-hydrogen-deployment.policy_priority` = 98 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence low
- `cells.green-hydrogen-assembly.policy_priority` = 95 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.64** Rare-earth and wind materials refining

- `dev.by_step.wind-refining.name` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.65** Partner, high need, allied supply falls short and new allied capacity needed

- `dev.by_step.wind-refining.reason` = Partner, high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.66** Viet Nam and India

- `dev.by_step.wind-refining.development` = Viet Nam and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.67** South Africa

- `dev.by_step.wind-refining.review` = South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.68** Viet Nam, India and South Africa

- `dev.by_step.wind-refining.aid` = Viet Nam, India and South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.69** Battery materials refining

- `dev.by_step.batteries-refining.name` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.70** High need and new allied capacity needed

- `dev.by_step.batteries-refining.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.71** the Philippines, Madagascar, India, Thailand, South Africa, Papua New Guinea and Turkiye

- `dev.by_step.batteries-refining.development` = the Philippines, Madagascar, India, Thailand, South Africa, Papua New Guinea and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.72** Indonesia, Argentina, the Democratic Republic of Congo and Malaysia

- `dev.by_step.batteries-refining.commercial` = Indonesia, Argentina, the Democratic Republic of Congo and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.73** Mozambique, Colombia and Tanzania

- `dev.by_step.batteries-refining.review` = Mozambique, Colombia and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.74** the Philippines, India, Thailand, South Africa, Indonesia, the Democratic Republic of Congo, Colombia and Tanzania

- `dev.by_step.batteries-refining.aid` = the Philippines, India, Thailand, South Africa, Indonesia, the Democratic Republic of Congo, Colombia and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.75** EV drivetrain components

- `dev.by_step.electric-vehicles-components.name` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.76** High need and new allied capacity needed

- `dev.by_step.electric-vehicles-components.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.77** Indonesia, Thailand, Serbia, Turkiye, Honduras, Morocco and Ukraine

- `dev.by_step.electric-vehicles-components.development` = Indonesia, Thailand, Serbia, Turkiye, Honduras, Morocco and Ukraine | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.78** India, the Philippines, Malaysia and Mexico

- `dev.by_step.electric-vehicles-components.commercial` = India, the Philippines, Malaysia and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.79** Viet Nam

- `dev.by_step.electric-vehicles-components.review` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.80** Indonesia, Thailand, Serbia, India, the Philippines and Viet Nam

- `dev.by_step.electric-vehicles-components.aid` = Indonesia, Thailand, Serbia, India, the Philippines and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.81** Battery components

- `dev.by_step.batteries-components.name` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.82** High need and new allied capacity needed

- `dev.by_step.batteries-components.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.83** Viet Nam, Indonesia, Mexico, Turkiye, India, Morocco and Malaysia

- `dev.by_step.batteries-components.development` = Viet Nam, Indonesia, Mexico, Turkiye, India, Morocco and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.84** Viet Nam, Indonesia and India

- `dev.by_step.batteries-components.aid` = Viet Nam, Indonesia and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.85** Battery cells and packs

- `dev.by_step.batteries-assembly.name` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.86** High need and new allied capacity needed

- `dev.by_step.batteries-assembly.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.87** Indonesia, Malaysia, Mexico, India and Turkiye

- `dev.by_step.batteries-assembly.development` = Indonesia, Malaysia, Mexico, India and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.88** Viet Nam

- `dev.by_step.batteries-assembly.commercial` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.89** Indonesia, India and Viet Nam

- `dev.by_step.batteries-assembly.aid` = Indonesia, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.90** Blades, towers and nacelle parts

- `dev.by_step.wind-components.name` = Blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.91** High need and new allied capacity needed

- `dev.by_step.wind-components.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.92** Thailand, Serbia and Brazil

- `dev.by_step.wind-components.development` = Thailand, Serbia and Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.93** Mexico, Viet Nam, the Philippines, India, Turkiye and Malaysia

- `dev.by_step.wind-components.commercial` = Mexico, Viet Nam, the Philippines, India, Turkiye and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.94** Thailand, Serbia, Viet Nam, the Philippines, India and Malaysia

- `dev.by_step.wind-components.aid` = Thailand, Serbia, Viet Nam, the Philippines, India and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.95** EV and charger manufacturing

- `dev.by_step.electric-vehicles-assembly.name` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.96** New allied capacity needed

- `dev.by_step.electric-vehicles-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.97** Viet Nam, Morocco, Malaysia and Indonesia

- `dev.by_step.electric-vehicles-assembly.development` = Viet Nam, Morocco, Malaysia and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.98** Mexico, Turkiye, India and Thailand

- `dev.by_step.electric-vehicles-assembly.commercial` = Mexico, Turkiye, India and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.99** Viet Nam, Indonesia, India and Thailand

- `dev.by_step.electric-vehicles-assembly.aid` = Viet Nam, Indonesia, India and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.100** Wind turbines

- `dev.by_step.wind-assembly.name` = Wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.101** New allied capacity needed

- `dev.by_step.wind-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.102** Viet Nam, Malaysia, Thailand, Indonesia and Morocco

- `dev.by_step.wind-assembly.development` = Viet Nam, Malaysia, Thailand, Indonesia and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.103** India, Turkiye and Mexico

- `dev.by_step.wind-assembly.commercial` = India, Turkiye and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.104** Brazil

- `dev.by_step.wind-assembly.review` = Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.105** Viet Nam, Malaysia, Thailand, Indonesia and India

- `dev.by_step.wind-assembly.aid` = Viet Nam, Malaysia, Thailand, Indonesia and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.106** Drilling rigs, tankers and oil equipment

- `dev.by_step.oil-assembly.name` = Drilling rigs, tankers and oil equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.107** New allied capacity needed

- `dev.by_step.oil-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.108** Mexico, Malaysia, Indonesia and Turkiye

- `dev.by_step.oil-assembly.development` = Mexico, Malaysia, Indonesia and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.109** India and Viet Nam

- `dev.by_step.oil-assembly.review` = India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.110** Malaysia, Indonesia, India and Viet Nam

- `dev.by_step.oil-assembly.aid` = Malaysia, Indonesia, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.111** Transformers, switchgear and cables

- `dev.by_step.electric-grid-assembly.name` = Transformers, switchgear and cables | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.112** New allied capacity needed

- `dev.by_step.electric-grid-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.113** Malaysia and Morocco

- `dev.by_step.electric-grid-assembly.development` = Malaysia and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.114** Thailand, Mexico and Turkiye

- `dev.by_step.electric-grid-assembly.commercial` = Thailand, Mexico and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.115** the Philippines, India and Viet Nam

- `dev.by_step.electric-grid-assembly.review` = the Philippines, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.116** Malaysia, Thailand, the Philippines, India and Viet Nam

- `dev.by_step.electric-grid-assembly.aid` = Malaysia, Thailand, the Philippines, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.117** PV modules and inverters

- `dev.by_step.solar-assembly.name` = PV modules and inverters | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.118** New allied capacity needed

- `dev.by_step.solar-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.119** Morocco

- `dev.by_step.solar-assembly.development` = Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.120** Viet Nam, India, Thailand, Malaysia, Turkiye and Mexico

- `dev.by_step.solar-assembly.commercial` = Viet Nam, India, Thailand, Malaysia, Turkiye and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.121** Indonesia

- `dev.by_step.solar-assembly.review` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.122** Viet Nam, India, Thailand, Malaysia and Indonesia

- `dev.by_step.solar-assembly.aid` = Viet Nam, India, Thailand, Malaysia and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.123** Rare-earth and battery-metal refining

- `dev.by_step.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.124** Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed

- `dev.by_step.electric-vehicles-refining.reason` = Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.125** Malaysia and Thailand

- `dev.by_step.electric-vehicles-refining.commercial` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.126** Indonesia, the Democratic Republic of Congo, Mozambique, the Philippines, Tanzania, Mexico and Madagascar

- `dev.by_step.electric-vehicles-refining.review` = Indonesia, the Democratic Republic of Congo, Mozambique, the Philippines, Tanzania, Mexico and Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.127** Thailand, Indonesia, the Democratic Republic of Congo, the Philippines and Tanzania

- `dev.by_step.electric-vehicles-refining.aid` = Thailand, Indonesia, the Democratic Republic of Congo, the Philippines and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.128** Uranium conversion and enrichment

- `dev.by_step.nuclear-refining.name` = Uranium conversion and enrichment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.129** Secure with allied delivery and high need

- `dev.by_step.nuclear-refining.reason` = Secure with allied delivery and high need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.130** Namibia

- `dev.by_step.nuclear-refining.review` = Namibia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.131** Drilling and pump components

- `dev.by_step.oil-components.name` = Drilling and pump components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.132** Partner and high need

- `dev.by_step.oil-components.reason` = Partner and high need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.133** Viet Nam

- `dev.by_step.oil-components.review` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.134** Viet Nam

- `dev.by_step.oil-components.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.135** Battery recycling

- `dev.by_step.batteries-recovery.name` = Battery recycling | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.136** High need

- `dev.by_step.batteries-recovery.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.137** India and Malaysia

- `dev.by_step.batteries-recovery.review` = India and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.138** Polysilicon refining

- `dev.by_step.solar-refining.name` = Polysilicon refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.139** High need

- `dev.by_step.solar-refining.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.140** India

- `dev.by_step.solar-refining.review` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.141** India

- `dev.by_step.solar-refining.aid` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.142** Grid components

- `dev.by_step.electric-grid-components.name` = Grid components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.143** New allied capacity needed

- `dev.by_step.electric-grid-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.144** India

- `dev.by_step.electric-grid-components.review` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.145** India

- `dev.by_step.electric-grid-components.aid` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.156** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.157** Extraction

- `cells.batteries-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.158** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.159** Medium

- `cells.batteries-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.160** 100

- `cells.batteries-extraction.need` = 100 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.161** 57

- `cells.batteries-extraction.siv` = 57 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.162** 0

- `cells.batteries-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.163** strong

- `cells.batteries-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.164** broadly aligned

- `cells.batteries-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.165** Battery materials refining

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.166** Refining

- `cells.batteries-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.167** Catch Up

- `cells.batteries-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.168** Medium

- `cells.batteries-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.169** 99

- `cells.batteries-refining.need` = 99 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.170** 77

- `cells.batteries-refining.siv` = 77 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.171** 0

- `cells.batteries-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.172** strong

- `cells.batteries-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.173** needs more attention

- `cells.batteries-refining.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.174** Battery components

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.175** Components

- `cells.batteries-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.176** Preserve

- `cells.batteries-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.177** Medium

- `cells.batteries-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.178** 73

- `cells.batteries-components.need` = 73 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.179** 79

- `cells.batteries-components.siv` = 79 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.180** 76

- `cells.batteries-components.cp` = 76 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.181** moderate

- `cells.batteries-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.182** broadly aligned

- `cells.batteries-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.183** Battery cells and packs

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.184** Assembly

- `cells.batteries-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.185** Preserve

- `cells.batteries-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.186** Mixed High/Medium

- `cells.batteries-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.187** 76

- `cells.batteries-assembly.need` = 76 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.188** 97

- `cells.batteries-assembly.siv` = 97 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.189** 82

- `cells.batteries-assembly.cp` = 82 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.190** moderate

- `cells.batteries-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.191** broadly aligned

- `cells.batteries-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.192** Battery storage

- `cells.batteries-deployment.name` = Battery storage | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.193** Deployment

- `cells.batteries-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.194** Watch

- `cells.batteries-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.195** Low

- `cells.batteries-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.196** 56

- `cells.batteries-deployment.dn` = 56 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.197** strong

- `cells.batteries-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.198** broadly aligned

- `cells.batteries-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.199** Battery recycling

- `cells.batteries-recovery.name` = Battery recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.200** Recovery

- `cells.batteries-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.201** Catch Up

- `cells.batteries-recovery.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.202** Medium

- `cells.batteries-recovery.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.203** 96

- `cells.batteries-recovery.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence moderate

**a.204** 82

- `cells.batteries-recovery.siv` = 82 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence moderate

**a.205** mixed signals

- `cells.batteries-recovery.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate

**a.206** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.207** Extraction

- `cells.coal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.208** Secure

- `cells.coal-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.209** High

- `cells.coal-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.210** 97

- `cells.coal-extraction.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.211** 4

- `cells.coal-extraction.siv` = 4 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.212** 8

- `cells.coal-extraction.cp` = 8 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.213** strong

- `cells.coal-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.214** mixed signals

- `cells.coal-extraction.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.215** Coke and coal processing

- `cells.coal-refining.name` = Coke and coal processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.216** Refining

- `cells.coal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.217** Watch

- `cells.coal-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.218** Low

- `cells.coal-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.219** 95

- `cells.coal-refining.need` = 95 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.220** 9

- `cells.coal-refining.siv` = 9 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.221** 0

- `cells.coal-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.222** strong

- `cells.coal-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.223** not enough policy evidence

- `cells.coal-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.224** Coal-plant components

- `cells.coal-components.name` = Coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.225** Components

- `cells.coal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.226** Preserve

- `cells.coal-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.227** Medium

- `cells.coal-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.228** 59

- `cells.coal-components.need` = 59 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.229** 67

- `cells.coal-components.siv` = 67 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.230** 81

- `cells.coal-components.cp` = 81 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.231** moderate

- `cells.coal-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.232** broadly aligned

- `cells.coal-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.233** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.234** Assembly

- `cells.coal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.235** Catch Up

- `cells.coal-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.236** Medium

- `cells.coal-assembly.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.237** 56

- `cells.coal-assembly.need` = 56 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.238** 67

- `cells.coal-assembly.siv` = 67 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.239** 72

- `cells.coal-assembly.cp` = 72 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.240** moderate

- `cells.coal-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.241** broadly aligned

- `cells.coal-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.242** Coal power and use

- `cells.coal-deployment.name` = Coal power and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.243** Deployment

- `cells.coal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.244** Watch

- `cells.coal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.245** Low

- `cells.coal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.246** 52

- `cells.coal-deployment.dn` = 52 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.247** strong

- `cells.coal-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.248** broadly aligned

- `cells.coal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.249** Coal recovery

- `cells.coal-recovery.name` = Coal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.250** Recovery

- `cells.coal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.251** Not assessed

- `cells.coal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.252** 1

- `cells.coal-recovery.siv` = 1 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.253** not enough policy evidence

- `cells.coal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.254** Copper and bauxite mining

- `cells.electric-grid-extraction.name` = Copper and bauxite mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.255** Extraction

- `cells.electric-grid-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.256** Watch

- `cells.electric-grid-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.257** Low

- `cells.electric-grid-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.258** 95

- `cells.electric-grid-extraction.need` = 95 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.259** 41

- `cells.electric-grid-extraction.siv` = 41 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.260** 0

- `cells.electric-grid-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.261** strong

- `cells.electric-grid-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.262** broadly aligned

- `cells.electric-grid-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.263** Grid metals refining

- `cells.electric-grid-refining.name` = Grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.264** Refining

- `cells.electric-grid-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.265** Watch

- `cells.electric-grid-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.266** Mixed High/Low

- `cells.electric-grid-refining.yard` = Mixed High/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.267** 97

- `cells.electric-grid-refining.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.268** 53

- `cells.electric-grid-refining.siv` = 53 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.269** 0

- `cells.electric-grid-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.270** strong

- `cells.electric-grid-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.271** broadly aligned

- `cells.electric-grid-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.272** Grid components

- `cells.electric-grid-components.name` = Grid components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.273** Components

- `cells.electric-grid-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.274** Preserve

- `cells.electric-grid-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.275** Medium

- `cells.electric-grid-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.276** 70

- `cells.electric-grid-components.need` = 70 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**a.277** 47

- `cells.electric-grid-components.siv` = 47 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.278** 88

- `cells.electric-grid-components.cp` = 88 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**a.279** moderate

- `cells.electric-grid-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.280** broadly aligned

- `cells.electric-grid-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence low

**a.281** Transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = Transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.282** Assembly

- `cells.electric-grid-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.283** Preserve

- `cells.electric-grid-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.284** Mixed High/Medium/Low

- `cells.electric-grid-assembly.yard` = Mixed High/Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.285** 46

- `cells.electric-grid-assembly.need` = 46 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.286** 61

- `cells.electric-grid-assembly.siv` = 61 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.287** 89

- `cells.electric-grid-assembly.cp` = 89 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.288** moderate

- `cells.electric-grid-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.289** broadly aligned

- `cells.electric-grid-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.290** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.291** Deployment

- `cells.electric-grid-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.292** Watch

- `cells.electric-grid-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.293** Low

- `cells.electric-grid-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.294** 56

- `cells.electric-grid-deployment.dn` = 56 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence moderate; deployment-need confidence high

**a.295** too much intervention

- `cells.electric-grid-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate; deployment-need confidence high

**a.296** Grid-equipment recycling

- `cells.electric-grid-recovery.name` = Grid-equipment recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.297** Recovery

- `cells.electric-grid-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.298** Not assessed

- `cells.electric-grid-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.299** 14

- `cells.electric-grid-recovery.siv` = 14 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.300** not enough policy evidence

- `cells.electric-grid-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.301** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.302** Extraction

- `cells.electric-vehicles-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.303** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.304** Medium

- `cells.electric-vehicles-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.305** 100

- `cells.electric-vehicles-extraction.need` = 100 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.306** 59

- `cells.electric-vehicles-extraction.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.307** 14

- `cells.electric-vehicles-extraction.cp` = 14 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.308** strong

- `cells.electric-vehicles-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.309** broadly aligned

- `cells.electric-vehicles-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.310** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.311** Refining

- `cells.electric-vehicles-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.312** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.313** High

- `cells.electric-vehicles-refining.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.314** 99

- `cells.electric-vehicles-refining.need` = 99 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.315** 84

- `cells.electric-vehicles-refining.siv` = 84 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.316** 0

- `cells.electric-vehicles-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.317** strong

- `cells.electric-vehicles-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.318** needs more attention

- `cells.electric-vehicles-refining.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.319** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.320** Components

- `cells.electric-vehicles-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.321** Preserve

- `cells.electric-vehicles-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.322** Mixed High/Medium

- `cells.electric-vehicles-components.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.323** 97

- `cells.electric-vehicles-components.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.324** 93

- `cells.electric-vehicles-components.siv` = 93 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.325** 90

- `cells.electric-vehicles-components.cp` = 90 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.326** moderate

- `cells.electric-vehicles-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.327** broadly aligned

- `cells.electric-vehicles-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.328** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.329** Assembly

- `cells.electric-vehicles-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.330** Preserve

- `cells.electric-vehicles-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.331** Mixed High/Medium

- `cells.electric-vehicles-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.332** 39

- `cells.electric-vehicles-assembly.need` = 39 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.333** 93

- `cells.electric-vehicles-assembly.siv` = 93 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.334** 85

- `cells.electric-vehicles-assembly.cp` = 85 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.335** moderate

- `cells.electric-vehicles-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.336** broadly aligned

- `cells.electric-vehicles-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.337** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.338** Deployment

- `cells.electric-vehicles-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.339** Accelerate

- `cells.electric-vehicles-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.340** Medium

- `cells.electric-vehicles-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.341** 80

- `cells.electric-vehicles-deployment.dn` = 80 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.342** strong

- `cells.electric-vehicles-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.343** broadly aligned

- `cells.electric-vehicles-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.344** EV recycling

- `cells.electric-vehicles-recovery.name` = EV recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.345** Recovery

- `cells.electric-vehicles-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.346** Not assessed

- `cells.electric-vehicles-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.347** 37

- `cells.electric-vehicles-recovery.siv` = 37 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.348** not enough policy evidence

- `cells.electric-vehicles-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.349** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.350** Extraction

- `cells.gas-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.351** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.352** High

- `cells.gas-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.353** 90

- `cells.gas-extraction.need` = 90 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.354** 32

- `cells.gas-extraction.siv` = 32 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.355** 4

- `cells.gas-extraction.cp` = 4 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.356** strong

- `cells.gas-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.357** needs more attention

- `cells.gas-extraction.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.358** Gas liquids processing

- `cells.gas-refining.name` = Gas liquids processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.359** Refining

- `cells.gas-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.360** Watch

- `cells.gas-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.361** Low

- `cells.gas-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.362** 84

- `cells.gas-refining.need` = 84 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.363** 32

- `cells.gas-refining.siv` = 32 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.364** 12

- `cells.gas-refining.cp` = 12 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.365** strong

- `cells.gas-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.366** too much intervention

- `cells.gas-refining.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.367** Gas-turbine and drilling components

- `cells.gas-components.name` = Gas-turbine and drilling components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.368** Components

- `cells.gas-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.369** Catch Up

- `cells.gas-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.370** Medium

- `cells.gas-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.371** 49

- `cells.gas-components.need` = 49 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.372** 73

- `cells.gas-components.siv` = 73 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.373** 16

- `cells.gas-components.cp` = 16 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.374** moderate

- `cells.gas-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.375** broadly aligned

- `cells.gas-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.376** Gas turbines and LNG plant

- `cells.gas-assembly.name` = Gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.377** Assembly

- `cells.gas-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.378** Catch Up

- `cells.gas-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.379** Mixed High/Medium

- `cells.gas-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.380** 32

- `cells.gas-assembly.need` = 32 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.381** 73

- `cells.gas-assembly.siv` = 73 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.382** 59

- `cells.gas-assembly.cp` = 59 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.383** moderate

- `cells.gas-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.384** broadly aligned

- `cells.gas-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.385** Gas networks and use

- `cells.gas-deployment.name` = Gas networks and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.386** Deployment

- `cells.gas-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.387** Watch

- `cells.gas-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.388** Low

- `cells.gas-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.389** 55

- `cells.gas-deployment.dn` = 55 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.390** strong

- `cells.gas-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.391** broadly aligned

- `cells.gas-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.392** Gas recovery

- `cells.gas-recovery.name` = Gas recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.393** Recovery

- `cells.gas-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.394** Not assessed

- `cells.gas-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.395** 25

- `cells.gas-recovery.siv` = 25 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.396** not enough policy evidence

- `cells.gas-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.397** Geothermal resource development

- `cells.geothermal-extraction.name` = Geothermal resource development | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.398** Extraction

- `cells.geothermal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.399** Not assessed

- `cells.geothermal-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.400** 20

- `cells.geothermal-extraction.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.401** not enough policy evidence

- `cells.geothermal-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.402** Geothermal refining

- `cells.geothermal-refining.name` = Geothermal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.403** Refining

- `cells.geothermal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.404** Not assessed

- `cells.geothermal-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.405** 20

- `cells.geothermal-refining.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.406** not enough policy evidence

- `cells.geothermal-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.407** Geothermal plant components

- `cells.geothermal-components.name` = Geothermal plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.408** Components

- `cells.geothermal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.409** Not assessed

- `cells.geothermal-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.410** 69

- `cells.geothermal-components.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.411** 13

- `cells.geothermal-components.cp` = 13 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.412** none

- `cells.geothermal-components.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.413** not enough policy evidence

- `cells.geothermal-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.414** Geothermal power-plant equipment

- `cells.geothermal-assembly.name` = Geothermal power-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.415** Assembly

- `cells.geothermal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.416** Not assessed

- `cells.geothermal-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.417** 69

- `cells.geothermal-assembly.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.418** 13

- `cells.geothermal-assembly.cp` = 13 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.419** none

- `cells.geothermal-assembly.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.420** not enough policy evidence

- `cells.geothermal-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.421** Geothermal power and heat

- `cells.geothermal-deployment.name` = Geothermal power and heat | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.422** Deployment

- `cells.geothermal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.423** Accelerate

- `cells.geothermal-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.424** Medium

- `cells.geothermal-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.425** 67

- `cells.geothermal-deployment.dn` = 67 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.426** strong

- `cells.geothermal-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.427** deployment policy lags

- `cells.geothermal-deployment.alignment` = deployment policy lags | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.428** Geothermal recovery

- `cells.geothermal-recovery.name` = Geothermal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.429** Recovery

- `cells.geothermal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.430** Not assessed

- `cells.geothermal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.431** 20

- `cells.geothermal-recovery.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.432** not enough policy evidence

- `cells.geothermal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.433** Electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = Electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.434** Extraction

- `cells.green-hydrogen-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.435** Not assessed

- `cells.green-hydrogen-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.436** 35

- `cells.green-hydrogen-extraction.siv` = 35 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.437** 0

- `cells.green-hydrogen-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.438** strong

- `cells.green-hydrogen-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.439** not enough policy evidence

- `cells.green-hydrogen-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.440** PGM and catalyst refining

- `cells.green-hydrogen-refining.name` = PGM and catalyst refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.441** Refining

- `cells.green-hydrogen-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.442** Not assessed

- `cells.green-hydrogen-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.443** 63

- `cells.green-hydrogen-refining.siv` = 63 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.444** 91

- `cells.green-hydrogen-refining.cp` = 91 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.445** strong

- `cells.green-hydrogen-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.446** not enough policy evidence

- `cells.green-hydrogen-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.447** Electrolyser stacks

- `cells.green-hydrogen-components.name` = Electrolyser stacks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.448** Components

- `cells.green-hydrogen-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.449** Not assessed

- `cells.green-hydrogen-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.450** 99

- `cells.green-hydrogen-components.siv` = 99 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.451** 95

- `cells.green-hydrogen-components.cp` = 95 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.452** none

- `cells.green-hydrogen-components.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.453** not enough policy evidence

- `cells.green-hydrogen-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.454** Electrolyser systems

- `cells.green-hydrogen-assembly.name` = Electrolyser systems | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.455** Assembly

- `cells.green-hydrogen-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.456** Not assessed

- `cells.green-hydrogen-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.457** 90

- `cells.green-hydrogen-assembly.siv` = 90 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.458** 95

- `cells.green-hydrogen-assembly.cp` = 95 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.459** none

- `cells.green-hydrogen-assembly.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.460** not enough policy evidence

- `cells.green-hydrogen-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.461** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.462** Deployment

- `cells.green-hydrogen-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.463** Accelerate

- `cells.green-hydrogen-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.464** Medium

- `cells.green-hydrogen-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.465** 93

- `cells.green-hydrogen-deployment.dn` = 93 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence low

**a.466** broadly aligned

- `cells.green-hydrogen-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence low

**a.467** Stack and catalyst recycling

- `cells.green-hydrogen-recovery.name` = Stack and catalyst recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.468** Recovery

- `cells.green-hydrogen-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.469** Not assessed

- `cells.green-hydrogen-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.470** 49

- `cells.green-hydrogen-recovery.siv` = 49 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.471** not enough policy evidence

- `cells.green-hydrogen-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.472** Uranium mining

- `cells.nuclear-extraction.name` = Uranium mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.473** Extraction

- `cells.nuclear-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.474** Watch

- `cells.nuclear-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.475** Low

- `cells.nuclear-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.476** 96

- `cells.nuclear-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.477** 52

- `cells.nuclear-extraction.siv` = 52 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.478** 0

- `cells.nuclear-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.479** strong

- `cells.nuclear-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.480** broadly aligned

- `cells.nuclear-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.481** Uranium conversion and enrichment

- `cells.nuclear-refining.name` = Uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.482** Refining

- `cells.nuclear-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.483** Secure

- `cells.nuclear-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.484** High

- `cells.nuclear-refining.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.485** 97

- `cells.nuclear-refining.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.486** 88

- `cells.nuclear-refining.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.487** 20

- `cells.nuclear-refining.cp` = 20 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.488** strong

- `cells.nuclear-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.489** needs more attention

- `cells.nuclear-refining.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.490** Nuclear fuel and reactor parts

- `cells.nuclear-components.name` = Nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.491** Components

- `cells.nuclear-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.492** Catch Up

- `cells.nuclear-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.493** Medium

- `cells.nuclear-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.494** 56

- `cells.nuclear-components.need` = 56 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.495** 88

- `cells.nuclear-components.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.496** 71

- `cells.nuclear-components.cp` = 71 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.497** moderate

- `cells.nuclear-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.498** broadly aligned

- `cells.nuclear-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.499** Reactors and steam generators

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.500** Assembly

- `cells.nuclear-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.501** Preserve

- `cells.nuclear-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.502** Mixed High/Medium

- `cells.nuclear-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.503** 58

- `cells.nuclear-assembly.need` = 58 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.504** 96

- `cells.nuclear-assembly.siv` = 96 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.505** 75

- `cells.nuclear-assembly.cp` = 75 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.506** moderate

- `cells.nuclear-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.507** broadly aligned

- `cells.nuclear-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.508** Nuclear power

- `cells.nuclear-deployment.name` = Nuclear power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.509** Deployment

- `cells.nuclear-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.510** Accelerate

- `cells.nuclear-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.511** Medium

- `cells.nuclear-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.512** 80

- `cells.nuclear-deployment.dn` = 80 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.513** strong

- `cells.nuclear-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.514** broadly aligned

- `cells.nuclear-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.515** Spent-fuel reprocessing

- `cells.nuclear-recovery.name` = Spent-fuel reprocessing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.516** Recovery

- `cells.nuclear-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.517** Not assessed

- `cells.nuclear-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.518** 88

- `cells.nuclear-recovery.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.519** not enough policy evidence

- `cells.nuclear-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.520** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.521** Extraction

- `cells.oil-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.522** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.523** High

- `cells.oil-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.524** 97

- `cells.oil-extraction.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.525** 25

- `cells.oil-extraction.siv` = 25 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.526** 0

- `cells.oil-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.527** strong

- `cells.oil-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.528** mixed signals

- `cells.oil-extraction.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.529** Oil refining

- `cells.oil-refining.name` = Oil refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.530** Refining

- `cells.oil-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.531** Watch

- `cells.oil-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.532** Low

- `cells.oil-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.533** 23

- `cells.oil-refining.need` = 23 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.534** 45

- `cells.oil-refining.siv` = 45 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.535** 71

- `cells.oil-refining.cp` = 71 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.536** strong

- `cells.oil-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.537** too much intervention

- `cells.oil-refining.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.538** Drilling and pump components

- `cells.oil-components.name` = Drilling and pump components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.539** Components

- `cells.oil-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.540** Partner

- `cells.oil-components.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.541** Medium

- `cells.oil-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.542** 72

- `cells.oil-components.need` = 72 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.543** 32

- `cells.oil-components.siv` = 32 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.544** 10

- `cells.oil-components.cp` = 10 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.545** moderate

- `cells.oil-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.546** broadly aligned

- `cells.oil-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.547** Drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = Drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.548** Assembly

- `cells.oil-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.549** Catch Up

- `cells.oil-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.550** Mixed High/Medium

- `cells.oil-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.551** 33

- `cells.oil-assembly.need` = 33 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.552** 60

- `cells.oil-assembly.siv` = 60 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.553** 52

- `cells.oil-assembly.cp` = 52 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.554** moderate

- `cells.oil-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.555** broadly aligned

- `cells.oil-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.556** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.557** Deployment

- `cells.oil-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.558** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.559** Low

- `cells.oil-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.560** 30

- `cells.oil-deployment.dn` = 30 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.561** strong

- `cells.oil-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.562** broadly aligned

- `cells.oil-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.563** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.564** Recovery

- `cells.oil-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.565** Watch

- `cells.oil-recovery.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.566** Low

- `cells.oil-recovery.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.567** 42

- `cells.oil-recovery.need` = 42 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence high

**a.568** 13

- `cells.oil-recovery.siv` = 13 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence high

**a.569** broadly aligned

- `cells.oil-recovery.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high

**a.570** PV mineral mining

- `cells.solar-extraction.name` = PV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.571** Extraction

- `cells.solar-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.572** Watch

- `cells.solar-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.573** Low

- `cells.solar-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.574** 96

- `cells.solar-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.575** 30

- `cells.solar-extraction.siv` = 30 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.576** 0

- `cells.solar-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.577** strong

- `cells.solar-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.578** broadly aligned

- `cells.solar-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.579** Polysilicon refining

- `cells.solar-refining.name` = Polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.580** Refining

- `cells.solar-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.581** Watch

- `cells.solar-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.582** Mixed Medium/Low

- `cells.solar-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.583** 76

- `cells.solar-refining.need` = 76 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.584** 58

- `cells.solar-refining.siv` = 58 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.585** 0

- `cells.solar-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.586** strong

- `cells.solar-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.587** broadly aligned

- `cells.solar-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.588** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.589** Components

- `cells.solar-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.590** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.591** Mixed Medium/Low

- `cells.solar-components.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.592** 31

- `cells.solar-components.need` = 31 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.593** 59

- `cells.solar-components.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.594** 78

- `cells.solar-components.cp` = 78 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.595** moderate

- `cells.solar-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.596** too much intervention

- `cells.solar-components.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.597** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.598** Assembly

- `cells.solar-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.599** Preserve

- `cells.solar-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.600** Mixed Medium/Low

- `cells.solar-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.601** 23

- `cells.solar-assembly.need` = 23 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.602** 82

- `cells.solar-assembly.siv` = 82 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.603** 84

- `cells.solar-assembly.cp` = 84 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.604** moderate

- `cells.solar-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.605** broadly aligned

- `cells.solar-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.606** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.607** Deployment

- `cells.solar-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.608** Accelerate

- `cells.solar-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.609** Medium

- `cells.solar-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.610** 73

- `cells.solar-deployment.dn` = 73 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.611** strong

- `cells.solar-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.612** deployment policy lags

- `cells.solar-deployment.alignment` = deployment policy lags | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.613** PV module recycling

- `cells.solar-recovery.name` = PV module recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.614** Recovery

- `cells.solar-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.615** Not assessed

- `cells.solar-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.616** 45

- `cells.solar-recovery.siv` = 45 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.617** not enough policy evidence

- `cells.solar-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.618** Wind mineral mining

- `cells.wind-extraction.name` = Wind mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.619** Extraction

- `cells.wind-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.620** Not assessed

- `cells.wind-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.621** 44

- `cells.wind-extraction.siv` = 44 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.622** 33

- `cells.wind-extraction.cp` = 33 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.623** strong

- `cells.wind-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.624** not enough policy evidence

- `cells.wind-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.625** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.626** Refining

- `cells.wind-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.627** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.628** Mixed High/Medium

- `cells.wind-refining.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.629** 79

- `cells.wind-refining.need` = 79 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.630** 58

- `cells.wind-refining.siv` = 58 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.631** 0

- `cells.wind-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.632** strong

- `cells.wind-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.633** broadly aligned

- `cells.wind-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.634** Blades, towers and nacelle parts

- `cells.wind-components.name` = Blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.635** Components

- `cells.wind-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.636** Catch Up

- `cells.wind-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.637** Mixed High/Medium

- `cells.wind-components.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.638** 82

- `cells.wind-components.need` = 82 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.639** 89

- `cells.wind-components.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.640** 45

- `cells.wind-components.cp` = 45 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.641** moderate

- `cells.wind-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.642** broadly aligned

- `cells.wind-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.643** Wind turbines

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.644** Assembly

- `cells.wind-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.645** Catch Up

- `cells.wind-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.646** Mixed High/Medium

- `cells.wind-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.647** 64

- `cells.wind-assembly.need` = 64 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.648** 89

- `cells.wind-assembly.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.649** 56

- `cells.wind-assembly.cp` = 56 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.650** moderate

- `cells.wind-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.651** broadly aligned

- `cells.wind-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.652** Wind power

- `cells.wind-deployment.name` = Wind power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.653** Deployment

- `cells.wind-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.654** Accelerate

- `cells.wind-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.655** Medium

- `cells.wind-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.656** 72

- `cells.wind-deployment.dn` = 72 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.657** strong

- `cells.wind-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.658** deployment policy lags

- `cells.wind-deployment.alignment` = deployment policy lags | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.659** Wind-turbine recycling

- `cells.wind-recovery.name` = Wind-turbine recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.660** Recovery

- `cells.wind-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.661** Not assessed

- `cells.wind-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.662** 30

- `cells.wind-recovery.siv` = 30 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.663** not enough policy evidence

- `cells.wind-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.669** Anode materials (natural and synthetic graphite)

- `cells.batteries-refining.layers.bess-anode.name` = Anode materials (natural and synthetic graphite) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.670** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.671** Catch Up

- `cells.batteries-refining.layers.bess-anode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.672** Medium

- `cells.batteries-refining.layers.bess-anode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.673** Cathode active materials and precursors

- `cells.batteries-refining.layers.bess-cathode.name` = Cathode active materials and precursors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.674** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.675** Catch Up

- `cells.batteries-refining.layers.bess-cathode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.676** Medium

- `cells.batteries-refining.layers.bess-cathode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.677** Battery-grade lithium hydroxide and carbonate

- `cells.batteries-refining.layers.li-chemicals.name` = Battery-grade lithium hydroxide and carbonate | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.678** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.679** Catch Up

- `cells.batteries-refining.layers.li-chemicals.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.680** Medium

- `cells.batteries-refining.layers.li-chemicals.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.681** Battery cells (LFP and NMC)

- `cells.batteries-assembly.layers.bess-cells.name` = Battery cells (LFP and NMC) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.682** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.683** Preserve

- `cells.batteries-assembly.layers.bess-cells.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.684** Medium

- `cells.batteries-assembly.layers.bess-cells.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.685** Communications gateway and cloud fleet management

- `cells.batteries-assembly.layers.bess-comms.name` = Communications gateway and cloud fleet management | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.686** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.687** Secure

- `cells.batteries-assembly.layers.bess-comms.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.688** High

- `cells.batteries-assembly.layers.bess-comms.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.689** Battery-management firmware, energy-management software and remote updates

- `cells.batteries-assembly.layers.bess-control.name` = Battery-management firmware, energy-management software and remote updates | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.690** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.691** Secure

- `cells.batteries-assembly.layers.bess-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.692** High

- `cells.batteries-assembly.layers.bess-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.693** Enclosures, racking, thermal management and balance of system

- `cells.batteries-assembly.layers.bess-enclosure.name` = Enclosures, racking, thermal management and balance of system | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.694** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.695** Preserve

- `cells.batteries-assembly.layers.bess-enclosure.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.696** Medium

- `cells.batteries-assembly.layers.bess-enclosure.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.697** Grain-oriented electrical steel

- `cells.electric-grid-refining.layers.goes.name` = Grain-oriented electrical steel | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.698** grid metals refining

- `cells.electric-grid-refining.name` = grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.699** Secure

- `cells.electric-grid-refining.layers.goes.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.700** High

- `cells.electric-grid-refining.layers.goes.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.701** Power conversion hardware (inverters and converters)

- `cells.electric-grid-assembly.layers.bess-pcs.name` = Power conversion hardware (inverters and converters) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.702** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.703** Preserve

- `cells.electric-grid-assembly.layers.bess-pcs.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.704** Medium

- `cells.electric-grid-assembly.layers.bess-pcs.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.705** Smart meters and advanced metering head-end systems

- `cells.electric-grid-assembly.layers.grid-ami.name` = Smart meters and advanced metering head-end systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.706** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.707** Secure

- `cells.electric-grid-assembly.layers.grid-ami.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.708** High

- `cells.electric-grid-assembly.layers.grid-ami.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.709** Cables and conductors

- `cells.electric-grid-assembly.layers.grid-cables.name` = Cables and conductors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.710** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.711** Watch

- `cells.electric-grid-assembly.layers.grid-cables.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.712** Low

- `cells.electric-grid-assembly.layers.grid-cables.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.713** Utility-scale inverter firmware and remote-access functions

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.name` = Utility-scale inverter firmware and remote-access functions | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.714** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.715** Secure

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.716** High

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.717** Grid-control software (SCADA, EMS, DERMS)

- `cells.electric-grid-assembly.layers.grid-scada.name` = Grid-control software (SCADA, EMS, DERMS) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.718** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.719** Secure

- `cells.electric-grid-assembly.layers.grid-scada.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.720** High

- `cells.electric-grid-assembly.layers.grid-scada.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.721** Large power transformers

- `cells.electric-grid-assembly.layers.grid-transformers.name` = Large power transformers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.722** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.723** Preserve

- `cells.electric-grid-assembly.layers.grid-transformers.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.724** Medium

- `cells.electric-grid-assembly.layers.grid-transformers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.725** HVDC converter-station control and protection

- `cells.electric-grid-assembly.layers.hvdc-control.name` = HVDC converter-station control and protection | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.726** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.727** Preserve

- `cells.electric-grid-assembly.layers.hvdc-control.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.728** Medium

- `cells.electric-grid-assembly.layers.hvdc-control.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.729** Electric drive units and motors

- `cells.electric-vehicles-components.layers.ev-motors.name` = Electric drive units and motors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.730** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.731** Preserve

- `cells.electric-vehicles-components.layers.ev-motors.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.732** Medium

- `cells.electric-vehicles-components.layers.ev-motors.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.733** Sintered NdFeB permanent magnets (traction motors)

- `cells.electric-vehicles-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets (traction motors) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.734** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.735** Secure

- `cells.electric-vehicles-components.layers.ree-magnets.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.736** High

- `cells.electric-vehicles-components.layers.ree-magnets.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.737** Connected-vehicle telematics, over-the-air software and connectivity systems

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.name` = Connected-vehicle telematics, over-the-air software and connectivity systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.738** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.739** Secure

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.740** High

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.741** Charging-network management software (charge-point back ends)

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.name` = Charging-network management software (charge-point back ends) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.742** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.743** Accelerate

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.choice` = Accelerate | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.744** Medium

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.745** Pipeline SCADA, compressor-station and LNG-terminal control systems

- `cells.gas-assembly.layers.gas-ot-control.name` = Pipeline SCADA, compressor-station and LNG-terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.746** gas turbines and LNG plant

- `cells.gas-assembly.name` = gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.747** Secure

- `cells.gas-assembly.layers.gas-ot-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.748** High

- `cells.gas-assembly.layers.gas-ot-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.749** Iridium and platinum for PEM electrolyser catalysts

- `cells.green-hydrogen-extraction.layers.h2-pgm.name` = Iridium and platinum for PEM electrolyser catalysts | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.750** electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.751** Not assessed

- `cells.green-hydrogen-extraction.layers.h2-pgm.choice` = Not assessed | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.752** HALEU enrichment

- `cells.nuclear-refining.layers.nuc-haleu.name` = HALEU enrichment | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.753** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.754** Secure

- `cells.nuclear-refining.layers.nuc-haleu.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.755** High

- `cells.nuclear-refining.layers.nuc-haleu.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.756** LEU enrichment services

- `cells.nuclear-refining.layers.nuc-leu.name` = LEU enrichment services | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.757** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.758** Secure

- `cells.nuclear-refining.layers.nuc-leu.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.759** High

- `cells.nuclear-refining.layers.nuc-leu.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.760** Fuel assemblies (design-specific fabrication)

- `cells.nuclear-components.layers.nuc-fuel-fab.name` = Fuel assemblies (design-specific fabrication) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.761** nuclear fuel and reactor parts

- `cells.nuclear-components.name` = nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.762** Catch Up

- `cells.nuclear-components.layers.nuc-fuel-fab.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.763** Medium

- `cells.nuclear-components.layers.nuc-fuel-fab.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.764** Reactor control and safety instrumentation

- `cells.nuclear-assembly.layers.nuc-reactor-controls.name` = Reactor control and safety instrumentation | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.765** reactors and steam generators

- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.766** Secure

- `cells.nuclear-assembly.layers.nuc-reactor-controls.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.767** High

- `cells.nuclear-assembly.layers.nuc-reactor-controls.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.768** Oil pipeline and terminal control systems

- `cells.oil-assembly.layers.oil-ot-control.name` = Oil pipeline and terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.769** drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.770** Secure

- `cells.oil-assembly.layers.oil-ot-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.771** High

- `cells.oil-assembly.layers.oil-ot-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.772** Solar-grade polysilicon

- `cells.solar-refining.layers.polysilicon.name` = Solar-grade polysilicon | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.773** polysilicon refining

- `cells.solar-refining.name` = polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.774** Catch Up

- `cells.solar-refining.layers.polysilicon.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.775** Medium

- `cells.solar-refining.layers.polysilicon.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.776** Solar cells and wafers

- `cells.solar-components.layers.pv-cells-wafers.name` = Solar cells and wafers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.777** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.778** Preserve

- `cells.solar-components.layers.pv-cells-wafers.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.779** Medium

- `cells.solar-components.layers.pv-cells-wafers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.780** Next-generation PV (perovskite and tandem cells)

- `cells.solar-components.layers.pv-nextgen.name` = Next-generation PV (perovskite and tandem cells) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.781** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.782** Preserve

- `cells.solar-components.layers.pv-nextgen.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.783** Medium

- `cells.solar-components.layers.pv-nextgen.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.784** Distributed PV inverter firmware, communication dongles and vendor cloud

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.name` = Distributed PV inverter firmware, communication dongles and vendor cloud | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.785** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.786** Watch

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.787** Low

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.788** Conventional crystalline-silicon modules

- `cells.solar-assembly.layers.pv-modules.name` = Conventional crystalline-silicon modules | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.789** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.790** Preserve

- `cells.solar-assembly.layers.pv-modules.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.791** Medium

- `cells.solar-assembly.layers.pv-modules.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.792** Passive racking and mounting

- `cells.solar-assembly.layers.pv-racking.name` = Passive racking and mounting | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.793** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.794** Watch

- `cells.solar-assembly.layers.pv-racking.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.795** Low

- `cells.solar-assembly.layers.pv-racking.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.796** Rare-earth separation and metal-making

- `cells.wind-refining.layers.ree-separation.name` = Rare-earth separation and metal-making | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.797** rare-earth and wind materials refining

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.798** Secure

- `cells.wind-refining.layers.ree-separation.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.799** High

- `cells.wind-refining.layers.ree-separation.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.800** Sintered NdFeB permanent magnets

- `cells.wind-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.801** blades, towers and nacelle parts

- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.802** Secure

- `cells.wind-components.layers.ree-magnets.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.803** High

- `cells.wind-components.layers.ree-magnets.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.804** Turbine controllers, OEM remote access and park SCADA

- `cells.wind-assembly.layers.wind-turbine-control.name` = Turbine controllers, OEM remote access and park SCADA | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.805** wind turbines

- `cells.wind-assembly.name` = wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.806** Secure

- `cells.wind-assembly.layers.wind-turbine-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.807** High

- `cells.wind-assembly.layers.wind-turbine-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.814** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.815** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.816** the United States, Canada and Australia

- `cells.batteries-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.817** Allied supply covers the need

- `cells.batteries-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.818** moderate

- `cells.batteries-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.819** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.820** Secure

- `cells.coal-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.821** the United States, Canada and Australia

- `cells.coal-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.822** Allied supply covers the need

- `cells.coal-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.823** weak

- `cells.coal-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.824** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.825** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.826** the United States, Canada and Australia

- `cells.electric-vehicles-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.827** Allied supply covers the need

- `cells.electric-vehicles-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.828** moderate

- `cells.electric-vehicles-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.829** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.830** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.831** the United States, Australia and Japan

- `cells.electric-vehicles-refining.partners` = the United States, Australia and Japan | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.832** Allied supply covers the need

- `cells.electric-vehicles-refining.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.833** moderate

- `cells.electric-vehicles-refining.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.834** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.835** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.836** Norway, the United States and Canada

- `cells.gas-extraction.partners` = Norway, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.837** Allied supply covers the need

- `cells.gas-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.838** weak

- `cells.gas-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.839** Uranium conversion and enrichment

- `cells.nuclear-refining.name` = Uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.840** Secure

- `cells.nuclear-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.841** Canada, the United Kingdom and France

- `cells.nuclear-refining.partners` = Canada, the United Kingdom and France | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence strong

**a.842** Allied supply covers the need

- `cells.nuclear-refining.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.843** strong

- `cells.nuclear-refining.partner_evidence` = strong | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; partner evidence strong

**a.844** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.845** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.846** Canada, Norway and the United States

- `cells.oil-extraction.partners` = Canada, Norway and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.847** Allied supply falls short

- `cells.oil-extraction.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.848** weak

- `cells.oil-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.849** Drilling and pump components

- `cells.oil-components.name` = Drilling and pump components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.850** Partner

- `cells.oil-components.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.851** the United States, Canada and the Netherlands

- `cells.oil-components.partners` = the United States, Canada and the Netherlands | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate; partner evidence weak

**a.852** Allied supply covers the need

- `cells.oil-components.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.853** weak

- `cells.oil-components.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate; partner evidence weak

**a.854** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.855** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.856** Germany, Belgium and Spain

- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**a.857** Allied supply falls short

- `cells.wind-refining.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.858** strong

- `cells.wind-refining.partner_evidence` = strong | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**a.865** Permanent Magnets & Drive Systems

- `aib.compacts.SC-MAGNETS.name` = Permanent Magnets & Drive Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.866** Specialized technology supplier

- `aib.compacts.SC-MAGNETS.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.867** Specialized technology supplier, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-MAGNETS.functions` = Specialized technology supplier, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.868** Magnets & core steel

- `aib.compacts.SC-MAGNETS.lead_step` = Magnets & core steel | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.869** Thailand, Viet Nam, Germany, Japan, the United States and Mexico

- `aib.compacts.SC-MAGNETS.core` = Thailand, Viet Nam, Germany, Japan, the United States and Mexico | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.870** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.871** Specialized technology supplier

- `aib.compacts.SC-BATTERIES.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.872** Specialized technology supplier, scale manufacturer, demand anchor, investment anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-BATTERIES.functions` = Specialized technology supplier, scale manufacturer, demand anchor, investment anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.873** Electrode materials & components

- `aib.compacts.SC-BATTERIES.lead_step` = Electrode materials & components | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.874** the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany

- `aib.compacts.SC-BATTERIES.core` = the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.875** Semiconductors & Power Electronics

- `aib.compacts.SC-SEMIS.name` = Semiconductors & Power Electronics | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.876** Processing anchor

- `aib.compacts.SC-SEMIS.role` = Processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.877** Processing anchor, specialized technology supplier, scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-SEMIS.functions` = Processing anchor, specialized technology supplier, scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.878** Semiconductor materials

- `aib.compacts.SC-SEMIS.lead_step` = Semiconductor materials | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.879** South Korea, Taiwan, Singapore, Malaysia, the United States and Viet Nam

- `aib.compacts.SC-SEMIS.core` = South Korea, Taiwan, Singapore, Malaysia, the United States and Viet Nam | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.880** Solar Manufacturing

- `aib.compacts.SC-SOLAR.name` = Solar Manufacturing | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.881** Scale manufacturer

- `aib.compacts.SC-SOLAR.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.882** Scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-SOLAR.functions` = Scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.883** Cells & cover glass

- `aib.compacts.SC-SOLAR.lead_step` = Cells & cover glass | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.884** Germany, the United States, Japan, South Korea, India and Taiwan

- `aib.compacts.SC-SOLAR.core` = Germany, the United States, Japan, South Korea, India and Taiwan | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.885** Wind & Hydropower Equipment

- `aib.compacts.SC-WIND.name` = Wind & Hydropower Equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.886** Scale manufacturer

- `aib.compacts.SC-WIND.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.887** Scale manufacturer and capacity-building host

- `aib.compacts.SC-WIND.functions` = Scale manufacturer and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.888** Blades, towers & turbine parts

- `aib.compacts.SC-WIND.lead_step` = Blades, towers & turbine parts | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.889** Spain, India, Germany, Denmark, the United States and the United Kingdom

- `aib.compacts.SC-WIND.core` = Spain, India, Germany, Denmark, the United States and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.890** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.891** Scale manufacturer

- `aib.compacts.SC-NUCLEAR.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.892** Scale manufacturer

- `aib.compacts.SC-NUCLEAR.functions` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.893** Fuel & nuclear materials

- `aib.compacts.SC-NUCLEAR.lead_step` = Fuel & nuclear materials | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.894** Australia, India, the United States, Sweden, France and the United Kingdom

- `aib.compacts.SC-NUCLEAR.core` = Australia, India, the United States, Sweden, France and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.895** Grid Equipment & Conductors

- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.896** Scale manufacturer

- `aib.compacts.SC-GRID.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.897** Scale manufacturer, demand anchor, capacity-building host and diversification partner

- `aib.compacts.SC-GRID.functions` = Scale manufacturer, demand anchor, capacity-building host and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.898** Network equipment

- `aib.compacts.SC-GRID.lead_step` = Network equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.899** Chile, Peru, the Democratic Republic of Congo, Germany, India and the United States

- `aib.compacts.SC-GRID.core` = Chile, Peru, the Democratic Republic of Congo, Germany, India and the United States | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.900** Electric Vehicles

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.901** Investment anchor

- `aib.compacts.SC-EV.role` = Investment anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.902** Investment anchor and capacity-building host

- `aib.compacts.SC-EV.functions` = Investment anchor and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.903** Germany, the United States, Japan, France, Spain and the United Kingdom

- `aib.compacts.SC-EV.core` = Germany, the United States, Japan, France, Spain and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.904** Heat Pumps & Cooling

- `aib.compacts.SC-HEAT.name` = Heat Pumps & Cooling | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.905** Scale manufacturer

- `aib.compacts.SC-HEAT.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.906** Scale manufacturer, demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-HEAT.functions` = Scale manufacturer, demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.907** Compressors

- `aib.compacts.SC-HEAT.lead_step` = Compressors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.908** the United States, Thailand, Germany, Mexico, Japan and Italy

- `aib.compacts.SC-HEAT.core` = the United States, Thailand, Germany, Mexico, Japan and Italy | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.909** Industrial Electrification & Hydrogen

- `aib.compacts.SC-INDUSTRY.name` = Industrial Electrification & Hydrogen | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.910** Capacity-building host

- `aib.compacts.SC-INDUSTRY.role` = Capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.911** Demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-INDUSTRY.functions` = Demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.912** the United States, India, South Africa, Germany, Indonesia and Saudi Arabia

- `aib.compacts.SC-INDUSTRY.core` = the United States, India, South Africa, Germany, Indonesia and Saudi Arabia | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.917** 26 November 2026

- `meta.as_of_label` = 26 November 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.919** 48

- `counts.assessed` = 48 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.921** 18

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.923** 34

- `counts.layers_assessed` = 34 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.925** 29

- `counts.ev_high` = 29 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.927** 18

- `counts.ev_partial` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.929** 19

- `counts.ev_none` = 19 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
