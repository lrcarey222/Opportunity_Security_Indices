# Content trace: Japan and the Allied Industrial Base

Each claim of the brief with the dashboard field it comes from, the source datasets and the evidence quality it rests on.

**s1.1** Japan's distinctive role in the Allied Industrial Base is as its specialist supplier and manufacturer: it holds a role in all ten strategic compacts, and 61 economies count it as their best-placed trusted partner somewhere in the chain.

- `aib.n_compact_roles` = ten | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.recommended_economies` = 61 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s1.8** The rare-earth chain from separation to magnets, where China holds 94% of world supply of rare-earth and battery-metal refining, which covers battery metals as well as rare earths, and trusted suppliers only 4%.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.name` = rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s1.9** The supply of oil, gas and coal, which takes one to three years to rebuild.

- `cells.oil-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s1.17** Battery and EV minerals from trusted producers such as Australia, the United States and Canada, and wind materials, where allied supply falls short.

- `cells.batteries-extraction.partners` = Australia, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s1.27** A corner flag marks a mixed step whose named parts sit behind a different fence: in PV modules and inverters, the inverter firmware is Secure, the step Catch Up and conventional modules Watch.

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.2** Secure covers four whole steps and 13 layers inside other steps, grouped into eight capabilities.

- `counts.secure` = four | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.secure_layers` = 13 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.high_capabilities` = eight | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.10** Japan has 9% of world exports of the refining step, and 20 economies have it as their best-placed trusted refiner.

- `cells.electric-vehicles-refining.export_share` = 9% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `aib.recommended_by.electric-vehicles-refining.n` = 20 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s2.11** China holds 94% of world refining supply and trusted suppliers 4%; it provides 47% of Japan's imports.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.china_share` = 47% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.12** Rebuilding takes more than three years.

- `cells.electric-vehicles-refining.reconstitution` = more than three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**s2.14** Trusted separation, metal and magnet capacity at home and with allies such as the United States and Australia, through co-investment and offtake; stocks until it qualifies; limits on adversary sources in critical uses, phased in.

- `cells.electric-vehicles-refining.partners` = the United States and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s2.16** Supply-chain need is 98 out of a hundred, and supply takes one to three years to rebuild.

- `cells.oil-extraction.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s2.17** The United Arab Emirates supplies 44% of imports; trusted suppliers hold 29% of world supply, and allied supply falls short.

- `cells.oil-extraction.top_supplier` = The United Arab Emirates | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-extraction.top_supplier_share` = 44% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-extraction.trusted_share` = 29% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.19** Stocks sized to the rebuild time; capacity reservations with several trusted producers, such as the United States, Canada and Norway; demand-side substitution.

- `cells.oil-extraction.partners` = the United States, Canada and Norway | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**s2.22** Australia supplies 39% of gas and 66% of coal imports; allied supply covers the need, but rebuilding takes one to three years.

- `cells.gas-extraction.top_supplier` = Australia | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.top_supplier_share` = 39% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.coal-extraction.top_supplier_share` = 66% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**s2.24** Long-term and capacity-reservation contracts with trusted producers, such as Norway, the United States and Belgium for gas; stockholding obligations; stress tests.

- `cells.gas-extraction.partners` = Norway, the United States and Belgium | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**s2.27** China supplies 77% of battery cell and pack imports, 48% of PV module and inverter imports and 51% of transformer and switchgear imports.

- `cells.batteries-assembly.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low
- `cells.batteries-assembly.china_share` = 77% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low
- `cells.solar-assembly.china_share` = 48% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.china_share` = 51% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**s2.33** China supplies 56% of imports under the wind turbines step's trade codes, which also cover cables, substations and transformers, and 47% of EV and charger imports.

- `cells.wind-assembly.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-assembly.china_share` = 56% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.china_share` = 47% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**s2.43** Reactors and steam generators reads Preserve on low confidence, with a position (79) just above the top-quarter bar (75).

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low
- `cells.nuclear-assembly.review` = low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low
- `cells.nuclear-assembly.cp` = 79 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low
- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.44** Battery cells and packs and EV and charger manufacturing sit just below the bar and read Catch Up, also on low confidence, and coal-plant components is flagged for review by the engine.

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low
- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low
- `cells.electric-vehicles-assembly.review` = low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low
- `cells.coal-components.name` = coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low
- `cells.coal-components.review` = flagged for review by the engine | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**s2.46** Battery materials refining is the clearest case for building: high value, a low position (19), three capability anchors and high confidence.

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.cp` = 19 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.anchors` = three | posture detail cells[].pathway_basis, pathway_credible | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.confidence` = high | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s2.47** China holds 94% of world supply and trusted suppliers 4%, so trusted capacity is scarce for every ally.

- `cells.batteries-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.48** Battery components is also well anchored: Japan holds 13% of world exports and has operating capacity behind them, and 8 economies have it as their best-placed trusted partner.

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-components.export_share` = 13% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `aib.recommended_by.batteries-components.n` = 8 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s2.49** Yet 84% of its own imports come from China.

- `cells.batteries-components.china_share` = 84% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-components.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.50** EV drivetrain components (60), blades, towers and nacelle parts (55) and nuclear fuel and reactor parts (61) extend positions Japan already holds, with high confidence.

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-components.cp` = 60 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-components.cp` = 55 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.nuclear-components.name` = nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-components.cp` = 61 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.52** Wind turbines: a position of 16 on a world supply share of 2%, in a step whose trade codes also cover cables, substations and transformers.

- `cells.wind-assembly.cp` = 16 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-assembly.share` = 2% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.53** PV modules and inverters: moderate confidence, conventional modules read Watch, and the trade codes include machines for making wafers and chips.

- `cells.solar-assembly.confidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.54** Uranium conversion and enrichment: an inherited anchor (a strong position in the next step up the chain) and a world supply share of <1%, so trusted allied enrichment comes first.

- `cells.nuclear-refining.anchors` = a strong position in the next step up the chain | posture detail cells[].pathway_basis, pathway_credible | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-refining.share` = <1% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.57** Japan holds 0% of world supply in battery and EV minerals and the data show no credible pathway to scale it.

- `cells.batteries-extraction.share` = 0% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.58** So dependence should be deliberate and spread across trusted producers such as Australia, the United States and Canada; allied supply covers the need.

- `cells.batteries-extraction.partners` = Australia, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s2.59** In rare-earth and wind materials refining, allied supply sits in Germany, Belgium and Spain and falls short, so co-investment and offtake must add capacity.

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate
- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**s2.62** Dashed lines mark the value bar (60) and the top quarter (75).

- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.72** Semiconductor manufacturing equipment

- `shared.semiconductor-manufacturing-equipment.label` = Semiconductor manufacturing equipment | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.73** wafers and PV cells (Watch) and PV modules and inverters (Catch Up)

- `shared.semiconductor-manufacturing-equipment.steps` = wafers and PV cells (Watch) and PV modules and inverters (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.74** The choices differ within the same technology with each step's own measures (wafers and PV cells: need 32, value 59, position 57; PV modules and inverters: need 55, value 80, position 28)

- `shared.semiconductor-manufacturing-equipment.reading` = The choices differ within the same technology with each step's own measures (wafers and PV cells: need 32, value 59, position 57; PV modules and inverters: need 55, value 80, position 28) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.75** Supplier: 21% of world exports or production of semiconductor manufacturing equipment (the AIB measure of the product, not a step's share); the largest allied holder of the step

- `shared.semiconductor-manufacturing-equipment.aib_role` = Supplier: 21% of world exports or production of semiconductor manufacturing equipment (the AIB measure of the product, not a step's share); the largest allied holder of the step | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.76** Insulators & bushings

- `shared.insulators-and-bushings.label` = Insulators & bushings | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.77** grid components (Watch); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up)

- `shared.insulators-and-bushings.steps` = grid components (Watch); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.78** The choices differ across technologies with each step's own measures (grid components: need 35, value 47, position 88; wafers and PV cells: need 32, value 59, position 57; blades, towers and nacelle parts: need 60, value 88, position 55)

- `shared.insulators-and-bushings.reading` = The choices differ across technologies with each step's own measures (grid components: need 35, value 47, position 88; wafers and PV cells: need 32, value 59, position 57; blades, towers and nacelle parts: need 60, value 88, position 55) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.79** Allies need new capacity here (BUILD); no supplier role

- `shared.insulators-and-bushings.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.80** Power electronics components

- `shared.power-electronics-components.label` = Power electronics components | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.81** grid components (Watch); EV drivetrain components (Catch Up); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up)

- `shared.power-electronics-components.steps` = grid components (Watch); EV drivetrain components (Catch Up); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.82** The choices differ across technologies with each step's own measures (grid components: need 35, value 47, position 88; EV drivetrain components: need 62, value 93, position 60; wafers and PV cells: need 32, value 59, position 57; blades, towers and nacelle parts: need 60, value 88, position 55)

- `shared.power-electronics-components.reading` = The choices differ across technologies with each step's own measures (grid components: need 35, value 47, position 88; EV drivetrain components: need 62, value 93, position 60; wafers and PV cells: need 32, value 59, position 57; blades, towers and nacelle parts: need 60, value 88, position 55) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.83** Allies need new capacity here (BUILD); no supplier role

- `shared.power-electronics-components.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.84** Rare-earth metals

- `shared.rare-earth-metals.label` = Rare-earth metals | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.85** rare-earth and battery-metal refining (Secure) and rare-earth and wind materials refining (Partner)

- `shared.rare-earth-metals.steps` = rare-earth and battery-metal refining (Secure) and rare-earth and wind materials refining (Partner) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.86** The choices differ across technologies with each step's own measures (rare-earth and battery-metal refining: need 100, value 84, position 32; rare-earth and wind materials refining: need 69, value 52, position 40)

- `shared.rare-earth-metals.reading` = The choices differ across technologies with each step's own measures (rare-earth and battery-metal refining: need 100, value 84, position 32; rare-earth and wind materials refining: need 69, value 52, position 40) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.87** Allies need new capacity here (BUILD); no supplier role

- `shared.rare-earth-metals.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.92** Supply-chain need for wafers and cells is low (32), and polysilicon refining is a large, commoditized yard worth at most a stated capability floor.

- `cells.solar-components.need` = 32 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.95** Japan holds top-quarter positions (83 and 84) on modest need and low value; its makers compete for the gas build-out without a fence.

- `cells.gas-assembly.cp` = 83 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.gas-components.cp` = 84 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.97** Need is low in components (35) but high in grid metals refining (72), a large, commoditized yard; the position in components is in the top quarter (88).

- `cells.electric-grid-components.need` = 35 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.electric-grid-refining.need` = 72 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.electric-grid-components.cp` = 88 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s3.2** In the Allied Industrial Base analysis Japan combines capital & investment, specialized technology and scale manufacturing roles, and it ranks first among allies on its measure of strength (compacts with a specialist position, workstreams it bridges).

- `aib.role` = combines capital & investment, specialized technology and scale manufacturing roles | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.first_on` = compacts with a specialist position, workstreams it bridges | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.3** It supplies 21% of world exports or production of semiconductor manufacturing equipment, 28% of wafers and 11% of automatic controllers.

- `aib.contributes.semiconductor-manufacturing-equipment.share` = 21% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.semiconductor-manufacturing-equipment.name` = semiconductor manufacturing equipment | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.contributes.wafers.share` = 28% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.wafers.name` = wafers | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.contributes.automatic-controllers.share` = 11% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.automatic-controllers.name` = automatic controllers | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.4** It holds a core seat in five compacts: Permanent Magnets & Drive Systems, Battery Materials & Cells, Solar Manufacturing, Grid Equipment & Conductors and Electric Vehicles.

- `aib.core_in` = five | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.core_in` = Permanent Magnets & Drive Systems, Battery Materials & Cells, Solar Manufacturing, Grid Equipment & Conductors and Electric Vehicles | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.5** It is the largest allied holder of wafers and PV cells (11%) and PV modules and inverters (9%), but those positions rest partly on chip equipment and wafers that share the steps' trade codes, so they overstate its PV output.

- `aib.division.solar-components.name` = wafers and PV cells | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.division.solar-components.share` = 11% | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.division.solar-assembly.name` = PV modules and inverters | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.division.solar-assembly.share` = 9% | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.8** If China's supply stopped, as in the Decoupling scenario, Japan would lose US$14.5 billion a year of imports across the Allied Industrial Base basket.

- `cells.batteries-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.lost_usd` = US$14.5 billion | AIB network.countries[].lostUsd | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.9** New capacity at the best-placed allied hosts, led by Germany, would restore only about US$2.1 billion of it a year.

- `aib.investment_decoupling.top_host` = Germany | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.investment_decoupling.total` = US$2.1 billion | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.10** Under Managed Interdependence, the default scenario, Japan sits in the minimum coalition of 7 Sectoral Compacts (the finer units beneath the strategic compacts); under Decoupling, 14, in 11 of them as a demand anchor.

- `aib.scenarios.managed.label` = Managed Interdependence | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_managed` = 7 | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_decoupling` = 14 | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_demand_anchor` = 11 | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.18** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.19** Refined battery chemicals (18% of world exports or production of its lead product); components; investment

- `aib.compacts.SC-BATTERIES.lead_share` = 18% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.21** Australia, the United States and Canada for minerals; the compact's mineral and processing seats, the United States, the Democratic Republic of Congo and Indonesia

- `cells.batteries-extraction.partners` = Australia, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate
- `aib.compacts.SC-BATTERIES.core_upstream` = the United States, the Democratic Republic of Congo and Indonesia | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.23** Permanent Magnets & Drive Systems

- `aib.compacts.SC-MAGNETS.name` = Permanent Magnets & Drive Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.24** Core steel for motors and drives (9% of world exports or production of its lead product, non-oriented electrical steel, not magnets), with a core seat

- `aib.compacts.SC-MAGNETS.lead_share` = 9% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.26** The United States and Australia for refining; the compact's makers, Germany, the United States and Mexico, as co-investors in magnet capacity

- `cells.electric-vehicles-refining.partners` = The United States and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate
- `aib.compacts.SC-MAGNETS.core_makers` = Germany, the United States and Mexico | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.28** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.29** Fuel and reactor parts (6% of world supply); hosting of new capacity

- `cells.nuclear-components.share` = 6% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s3.31** The compact's processors and makers, Canada, India, the United States and France

- `aib.compacts.SC-NUCLEAR.core_industry` = Canada, India, the United States and France | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.33** Clean-energy equipment: Wind & Hydropower Equipment, Grid Equipment & Conductors and Solar Manufacturing

- `aib.compacts.SC-WIND.name` = Wind & Hydropower Equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-SOLAR.name` = Solar Manufacturing | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.34** Transformer steel (25% of world exports or production of its lead product, grain-oriented electrical steel); solar inputs; a growing market

- `aib.compacts.SC-GRID.lead_share` = 25% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.36** Germany, Belgium and Spain for wind materials; the wind compact's makers, India and Germany, for turbines

- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong
- `aib.compacts.SC-WIND.core_makers` = India and Germany | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.41** Its capital role is real but not drawn: in Electric Vehicles its functions are scale manufacturer, investment anchor and capacity-building host.

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-EV.functions` = scale manufacturer, investment anchor and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s4.4** 49 pairs qualify, with 16 developing partners across 10 steps.

- `dev.n_pairs` = 49 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_partners` = 16 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_steps` = 10 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.5** Most, 28 pairs, rest on the allied gap alone, a reason that is the same for every donor in scope.

- `dev.n_allied_pairs` = 28 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.6** Japan's own supply chain supports 21 pairs with 12 partners across four steps, and the table leads with them: rare-earth and wind materials refining; battery materials refining; battery components; and wind turbines.

- `dev.n_own_pairs` = 21 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_partners` = 12 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_steps` = four | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.own_steps` = rare-earth and wind materials refining; battery materials refining; battery components; and wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.7** India, the Philippines and Viet Nam are lower middle income, so project preparation, infrastructure and skills come first.

- `dev.rows.r1.partner` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.8** Indonesia and Mexico are upper-middle-income economies, where finance, guarantees and offtake should lead.

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.9** India's case is the thinnest: a single export-share anchor and a Competitive Position of 0.

- `dev.rows.r1.partner` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.cp` = 0 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: derived

**s4.10** The Philippines also has a Competitive Position of 0, as do the other partners in its step, so its case rests on a share of world exports and operating capacity.

- `dev.rows.r2.partner` = The Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.cp` = 0 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: derived
- `dev.rows.r2.anchors` = a share of world exports and operating capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived

**s4.11** The last row rests on the allied gap alone, in a step where Japan's own call is a review case, as do many of the 28 allied-gap pairs, so the headline count overstates the pairs that rest on a settled Japanese call.

- `dev.n_allied_pairs` = 28 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.12** Battery materials refining is among the broadest strands, level with battery components: 7 developing economies, led by the Philippines and Madagascar, have credible pathways, and Argentina, the Democratic Republic of Congo and Malaysia already host new capacity commercially.

- `dev.by_step.batteries-refining.n_development` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.by_step.batteries-refining.development_top` = the Philippines and Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.by_step.batteries-refining.hosts` = Argentina, the Democratic Republic of Congo and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.14** Thailand and Malaysia host its new capacity commercially, and no partner qualifies in minerals or fuels.

- `dev.by_step.electric-vehicles-refining.hosts` = Thailand and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.15** Another 29 pairs are commercial, where trade, offtake and investment fit better than aid.

- `dev.n_commercial` = 29 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.22** Rare-earth and wind materials refining: high need, allied supply falls short and new allied capacity needed

- `dev.rows.r1.capability` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.problem` = high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.23** India (lower middle income)

- `dev.rows.r1.partner` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.24** A share of world exports; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r1.anchors` = A share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r1.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.25** Plant capacity not covered by the data and input costs not covered by the data.

- `dev.rows.r1.unobserved` = Plant capacity not covered by the data and input costs not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.26** Feasibility studies and project preparation, industrial infrastructure, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r1.tools` = Feasibility studies and project preparation, industrial infrastructure, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.27** A trusted second source and allied capacity where allied supply falls short (allies need new capacity in 2 product groups of the step)

- `dev.rows.r1.donor_gain` = A trusted second source and allied capacity where allied supply falls short | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.28** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r1.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.29** Battery materials refining: high need and new allied capacity needed

- `dev.rows.r2.capability` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.30** the Philippines (lower middle income)

- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.31** A share of world exports and operating capacity; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r2.anchors` = A share of world exports and operating capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r2.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.32** Constraint: input costs outside the cheapest quarter.

- `dev.rows.r2.constraints` = input costs outside the cheapest quarter | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.33** Industrial infrastructure, feasibility studies and project preparation, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r2.tools` = Industrial infrastructure, feasibility studies and project preparation, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.34** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 6 product groups of the step)

- `dev.rows.r2.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.n_build` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.35** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r2.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.36** Battery components: high need and new allied capacity needed

- `dev.rows.r3.capability` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.37** Indonesia (upper middle income)

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.38** A share of world exports; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r3.anchors` = A share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r3.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.39** Constraint: operating or committed plant below the capacity bar and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r3.constraints` = operating or committed plant below the capacity bar and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.40** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r3.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.41** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 1 product group of the step)

- `dev.rows.r3.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.n_build` = 1 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.42** A certified supplier base and export sales of components

- `dev.rows.r3.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.43** Wind turbines: high need and new allied capacity needed

- `dev.rows.r4.capability` = Wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.44** Viet Nam (lower middle income)

- `dev.rows.r4.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.45** A share of world supply and a share of world exports; corroborated by feasibility and industrial learning

- `dev.rows.r4.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r4.supports` = feasibility and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.46** Constraint: no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r4.constraints` = no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.47** Input costs not covered by the data.

- `dev.rows.r4.unobserved` = Input costs not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.48** Project preparation, investment promotion, industrial infrastructure, quality systems and certification; then, at scale, development finance, guarantees, offtake, equity and joint ventures

- `dev.rows.r4.tools` = Project preparation, investment promotion, industrial infrastructure, quality systems and certification; then, at scale, development finance, guarantees, offtake, equity and joint ventures | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.49** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 2 product groups of the step)

- `dev.rows.r4.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.50** Factory capacity, skills and export sales of finished equipment

- `dev.rows.r4.partner_gain` = Factory capacity, skills and export sales of finished equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.51** EV and charger manufacturing: new allied capacity needed

- `dev.rows.r5.capability` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.problem` = new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.52** Mexico (upper middle income)

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.53** A share of world supply, a share of world exports, operating capacity and a strong position in the next step up the chain; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r5.anchors` = A share of world supply, a share of world exports, operating capacity and a strong position in the next step up the chain | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r5.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.54** Constraint: input costs outside the cheapest quarter.

- `dev.rows.r5.constraints` = input costs outside the cheapest quarter | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.55** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for industrial infrastructure and supplier development

- `dev.rows.r5.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for industrial infrastructure and supplier development | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.56** New allied capacity where the AIB finds a gap (allies need new capacity in 4 product groups of the step)

- `dev.rows.r5.donor_gain` = New allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.n_build` = 4 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.57** Factory capacity, skills and export sales of finished equipment

- `dev.rows.r5.partner_gain` = Factory capacity, skills and export sales of finished equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.59** The second table shows what ties Japan to each partner today and what would hold the supply relationship as the capability scales; aid is the 2020 to 2024 total at the step.

- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.60** Aid at these steps is small; the wider ties are economy-wide investment positions, not tied to the step, and, in Indonesia and Mexico, factory projects by firms from Japan in the technology.

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.64** India: rare-earth and wind materials refining

- `dev.rows.r1.partner` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.65** $0.2 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $30,939 million (economy-wide)

- `dev.rows.r1.ties` = $0.2 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $30,939 million (economy-wide) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.66** Long-term offtake and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Wind & Hydropower Equipment compact; Japan's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r1.durable` = Long-term offtake and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Wind & Hydropower Equipment compact; Japan's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.67** the Philippines: battery materials refining

- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.68** Under $0.1 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $16,668 million (economy-wide); and no export-credit or lender finance recorded at the step

- `dev.rows.r2.ties` = Under $0.1 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $16,668 million (economy-wide); and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.69** Long-term offtake and investment guarantees as the capability scales; the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r2.durable` = Long-term offtake and investment guarantees as the capability scales; the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.70** Indonesia: battery components

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.71** $1.3 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $37,234 million (economy-wide); 1 factory project by firms from Japan in the technology; and no export-credit or lender finance recorded at the step

- `dev.rows.r3.ties` = $1.3 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $37,234 million (economy-wide); 1 factory project by firms from Japan in the technology; and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.72** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Processing anchor in the Battery Materials & Cells compact

- `dev.rows.r3.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Processing anchor in the Battery Materials & Cells compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.73** Viet Nam: wind turbines

- `dev.rows.r4.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.74** $0.5 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $24,719 million (economy-wide); and no factory projects by firms from Japan recorded in the technology

- `dev.rows.r4.ties` = $0.5 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $24,719 million (economy-wide); and no factory projects by firms from Japan recorded in the technology | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.75** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Demand anchor in the Wind & Hydropower Equipment compact; Japan's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r4.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Demand anchor in the Wind & Hydropower Equipment compact; Japan's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.76** Mexico: EV and charger manufacturing

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.capability` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.77** Under $0.1 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $13,676 million (economy-wide); and 1 factory project by firms from Japan in the technology

- `dev.rows.r5.ties` = Under $0.1 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $13,676 million (economy-wide); and 1 factory project by firms from Japan in the technology | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.78** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Electric Vehicles compact

- `dev.rows.r5.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Electric Vehicles compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.79** Thailand, Malaysia and the Philippines receive Japan's technology-coded aid at the step of a partnership; 8 partners receive it there under stage-wide codes.

- `dev.aid_aligned` = Thailand, Malaysia and the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_stage_wide` = 8 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.80** Mexico, the Philippines, Thailand and Malaysia receive technology-coded aid in a step where they are already commercial, so trade and investment should take over.

- `dev.aid_graduate` = Mexico, the Philippines, Thailand and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.81** Japan's largest recipient of energy, industry and mining aid, Bangladesh ($3,730 million over 2020 to 2024), has no partnership here; that aid is outside the energy-industrial objective of this analysis and is not judged here.

- `dev.aid_top_1` = Bangladesh | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_top_1_usd` = $3,730 million | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s5.1** Wherever the equipment is made, nuclear, solar and wind power and gas networks clear the Accelerate bar of 60.

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s5.2** Solar and wind rest on all three measures with high confidence, though system need drives them: demand adequacy alone (41 and 49) is below the bar.

- `cells.solar-deployment.dn_families` = three | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.dn_demand` = 41 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.dn_demand` = 49 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.3** The renewables target of 201 GW by 2030 compares with 133 GW installed in 2025, a shortfall of about 47 GW at the recent build rate.

- `cells.solar-deployment.target.target_gw` = 201 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.target_year` = 2030 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.installed_gw` = 133 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.installed_year` = 2025 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.shortfall_gw` = 47 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.4** Between 2020 and 2025, solar grew from 71.9 to 92.2 GW and wind from 4.1 to 6.2 GW; nuclear capacity was flat (33.1 to 33.1 GW).

- `cells.solar-deployment.deployment_years` = 2020 and 2025 | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.deployment_trend` = 71.9 to 92.2 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.deployment_trend` = 4.1 to 6.2 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.nuclear-deployment.deployment_trend` = 33.1 to 33.1 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**s6.6** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.7** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.8** Needs more attention

- `cells.gas-extraction.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.9** Vulnerability without enough policy attention

- `cells.gas-extraction.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.10** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.11** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.12** Needs more attention

- `cells.oil-extraction.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.13** Vulnerability without enough policy attention

- `cells.oil-extraction.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.14** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.15** Secure

- `cells.coal-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.16** Needs more attention

- `cells.coal-extraction.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.17** Vulnerability without enough policy attention

- `cells.coal-extraction.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.18** Wind turbines

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.19** Catch Up

- `cells.wind-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.20** Needs more attention

- `cells.wind-assembly.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.21** Capability case without enough policy attention

- `cells.wind-assembly.gap` = Capability case without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.22** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.23** Accelerate

- `cells.solar-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.24** Deployment policy below the middle

- `cells.solar-deployment.policy_flag` = Deployment policy below the middle | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.25** Deployment needed faster than policy supports

- `cells.solar-deployment.gap` = Deployment needed faster than policy supports | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.26** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.27** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.28** Heavy protection or localization

- `cells.solar-components.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.29** Localizing what is not a domestic priority

- `cells.solar-components.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s7.3** Build trusted separation, metal-making and magnet capacity at home, where the base exists, and with allied partners such as the United States and Australia through co-investment and offtake.

- `cells.electric-vehicles-refining.partners` = the United States and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.7** Oil, gas and coal are Secure for resilience: they are essential and imported, and their supply takes one to three years to rebuild.

- `cells.oil-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s7.8** Size strategic stocks to that time, reserve capacity with trusted producers such as the United States, Canada and Norway for oil (partner evidence is weak, so test the contract terms), and stress-test the loss of the largest supplier.

- `cells.oil-extraction.partners` = the United States, Canada and Norway | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak
- `cells.oil-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**s7.15** Buy battery and EV minerals through offtake and co-investment with producers such as Australia, the United States and Canada, with export-credit finance for their capacity and contracts spread across several suppliers.

- `cells.batteries-extraction.partners` = Australia, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.18** Start with the Philippines in battery materials refining and Indonesia in battery components.

- `dev.rows.r2.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.19** Test Viet Nam in wind turbines first, where the data record no operating or committed plant and the step's trade codes also cover cables, substations and transformers.

- `dev.rows.r4.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.20** Prepare India in rare-earth and wind materials refining over a longer horizon.

- `dev.rows.r1.partner` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.2** Every number comes from the Country page data of the Allied Industrial Base Explorer (the data release of 30 October 2026) and from the Allied Industrial Base analysis.

- `meta.as_of_label` = 30 October 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.5** at deployment, Accelerate where deployment need reaches 60;

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.7** Preserve where Competitive Position is in the top quarter (75) and either Strategic Industrial Value (bar 60) or supply-chain need (bar 67) is high;

- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.need` = 67 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.18** 8 steps are review cases.

- `counts.review` = 8 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.23** Battery cells and packs

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.24** Catch Up

- `cells.batteries-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.25** Low confidence

- `cells.batteries-assembly.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.26** Coal-plant components

- `cells.coal-components.name` = Coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.27** Preserve

- `cells.coal-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.28** Flagged for review by the engine

- `cells.coal-components.review` = Flagged for review by the engine | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.29** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.30** Watch

- `cells.coal-assembly.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.31** Low confidence

- `cells.coal-assembly.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.32** Transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = Transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.33** Catch Up

- `cells.electric-grid-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.34** Low confidence

- `cells.electric-grid-assembly.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.35** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.36** Catch Up

- `cells.electric-vehicles-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.37** Low confidence

- `cells.electric-vehicles-assembly.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.38** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.39** Accelerate

- `cells.green-hydrogen-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.40** A single indicator of low confidence

- `cells.green-hydrogen-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.41** Reactors and steam generators

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.42** Preserve

- `cells.nuclear-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.43** Low confidence

- `cells.nuclear-assembly.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.44** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.45** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.46** A single indicator of low confidence

- `cells.oil-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.48** The rare-earth chokepoint rests on measured supply concentration, and the battery-materials Catch Up has high confidence.

- `cells.batteries-refining.confidence` = high | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.52** Uranium conversion and enrichment rests on an adjacent-step anchor, with a world supply share of <1%.

- `cells.nuclear-refining.share` = <1% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.54** 18 steps lack the data for a call, including geothermal supply, green hydrogen manufacturing and most recycling.

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.55** Recorded policy ranks electrolyser stacks (98) and systems (99) near the top of Japan's own measures, but those steps cannot be assessed and deployment need reads high on a single indicator, so this brief does not settle that disagreement.

- `cells.green-hydrogen-components.policy_priority` = 98 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low
- `cells.green-hydrogen-assembly.policy_priority` = 99 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.56** Several review cases also read as needing more policy attention: reactors and steam generators, EV and charger manufacturing, and transformers, switchgear and cables.

- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low
- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low
- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.77** Rare-earth and wind materials refining

- `dev.by_step.wind-refining.name` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.78** Partner, high need, allied supply falls short and new allied capacity needed

- `dev.by_step.wind-refining.reason` = Partner, high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.79** India

- `dev.by_step.wind-refining.development` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.80** Viet Nam and South Africa

- `dev.by_step.wind-refining.review` = Viet Nam and South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.81** India and Viet Nam

- `dev.by_step.wind-refining.aid` = India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.82** Battery materials refining

- `dev.by_step.batteries-refining.name` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.83** High need and new allied capacity needed

- `dev.by_step.batteries-refining.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.84** the Philippines, Madagascar, India, Thailand, South Africa, Turkiye and Papua New Guinea

- `dev.by_step.batteries-refining.development` = the Philippines, Madagascar, India, Thailand, South Africa, Turkiye and Papua New Guinea | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.85** Indonesia, Argentina, the Democratic Republic of Congo and Malaysia

- `dev.by_step.batteries-refining.commercial` = Indonesia, Argentina, the Democratic Republic of Congo and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.86** Mozambique, Tanzania and Colombia

- `dev.by_step.batteries-refining.review` = Mozambique, Tanzania and Colombia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.87** the Philippines, India, Thailand, Indonesia, Argentina, the Democratic Republic of Congo, Mozambique, Tanzania and Colombia

- `dev.by_step.batteries-refining.aid` = the Philippines, India, Thailand, Indonesia, Argentina, the Democratic Republic of Congo, Mozambique, Tanzania and Colombia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.88** Battery components

- `dev.by_step.batteries-components.name` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.89** High need and new allied capacity needed

- `dev.by_step.batteries-components.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.90** Indonesia, Viet Nam, Mexico, Turkiye, India, Morocco and Malaysia

- `dev.by_step.batteries-components.development` = Indonesia, Viet Nam, Mexico, Turkiye, India, Morocco and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.91** Indonesia, Viet Nam, Mexico and India

- `dev.by_step.batteries-components.aid` = Indonesia, Viet Nam, Mexico and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.92** Wind turbines

- `dev.by_step.wind-assembly.name` = Wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.93** High need and new allied capacity needed

- `dev.by_step.wind-assembly.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.94** Viet Nam, Turkiye, Indonesia, Thailand, Malaysia and Morocco

- `dev.by_step.wind-assembly.development` = Viet Nam, Turkiye, Indonesia, Thailand, Malaysia and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.95** India and Mexico

- `dev.by_step.wind-assembly.commercial` = India and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.96** Brazil

- `dev.by_step.wind-assembly.review` = Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.97** Viet Nam, Indonesia, Thailand, Malaysia, India, Mexico and Brazil

- `dev.by_step.wind-assembly.aid` = Viet Nam, Indonesia, Thailand, Malaysia, India, Mexico and Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.98** EV and charger manufacturing

- `dev.by_step.electric-vehicles-assembly.name` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.99** New allied capacity needed

- `dev.by_step.electric-vehicles-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.100** Mexico, Thailand, Viet Nam, Malaysia, Morocco and Indonesia

- `dev.by_step.electric-vehicles-assembly.development` = Mexico, Thailand, Viet Nam, Malaysia, Morocco and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.101** Turkiye and India

- `dev.by_step.electric-vehicles-assembly.commercial` = Turkiye and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.102** Mexico, Thailand, Viet Nam, Indonesia and India

- `dev.by_step.electric-vehicles-assembly.aid` = Mexico, Thailand, Viet Nam, Indonesia and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.103** EV drivetrain components

- `dev.by_step.electric-vehicles-components.name` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.104** New allied capacity needed

- `dev.by_step.electric-vehicles-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.105** Thailand, Malaysia, Ukraine, Turkiye, Honduras, Morocco and Serbia

- `dev.by_step.electric-vehicles-components.development` = Thailand, Malaysia, Ukraine, Turkiye, Honduras, Morocco and Serbia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.106** Indonesia, the Philippines, Mexico and India

- `dev.by_step.electric-vehicles-components.commercial` = Indonesia, the Philippines, Mexico and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.107** Viet Nam

- `dev.by_step.electric-vehicles-components.review` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.108** Thailand, Ukraine, Indonesia, the Philippines, Mexico, India and Viet Nam

- `dev.by_step.electric-vehicles-components.aid` = Thailand, Ukraine, Indonesia, the Philippines, Mexico, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.109** Battery cells and packs

- `dev.by_step.batteries-assembly.name` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.110** New allied capacity needed

- `dev.by_step.batteries-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.111** Indonesia, Malaysia, Mexico, India and Turkiye

- `dev.by_step.batteries-assembly.development` = Indonesia, Malaysia, Mexico, India and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.112** Viet Nam

- `dev.by_step.batteries-assembly.commercial` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.113** Indonesia, Mexico, India and Viet Nam

- `dev.by_step.batteries-assembly.aid` = Indonesia, Mexico, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.114** Blades, towers and nacelle parts

- `dev.by_step.wind-components.name` = Blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.115** New allied capacity needed

- `dev.by_step.wind-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.116** Thailand, Serbia, Turkiye, Brazil and Malaysia

- `dev.by_step.wind-components.development` = Thailand, Serbia, Turkiye, Brazil and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.117** Mexico, Viet Nam, the Philippines and India

- `dev.by_step.wind-components.commercial` = Mexico, Viet Nam, the Philippines and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.118** Thailand, Brazil, Malaysia, Mexico, Viet Nam, the Philippines and India

- `dev.by_step.wind-components.aid` = Thailand, Brazil, Malaysia, Mexico, Viet Nam, the Philippines and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.119** Transformers, switchgear and cables

- `dev.by_step.electric-grid-assembly.name` = Transformers, switchgear and cables | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.120** New allied capacity needed

- `dev.by_step.electric-grid-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.121** India, Malaysia and the Philippines

- `dev.by_step.electric-grid-assembly.development` = India, Malaysia and the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.122** Mexico, Turkiye, Thailand and Viet Nam

- `dev.by_step.electric-grid-assembly.commercial` = Mexico, Turkiye, Thailand and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.123** Morocco

- `dev.by_step.electric-grid-assembly.review` = Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.124** India, Malaysia, the Philippines, Mexico, Thailand and Viet Nam

- `dev.by_step.electric-grid-assembly.aid` = India, Malaysia, the Philippines, Mexico, Thailand and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.125** PV modules and inverters

- `dev.by_step.solar-assembly.name` = PV modules and inverters | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.126** New allied capacity needed

- `dev.by_step.solar-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.127** Turkiye and Morocco

- `dev.by_step.solar-assembly.development` = Turkiye and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.128** India, Viet Nam, Thailand, Malaysia, Mexico and Indonesia

- `dev.by_step.solar-assembly.commercial` = India, Viet Nam, Thailand, Malaysia, Mexico and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.129** India, Viet Nam, Thailand, Malaysia, Mexico and Indonesia

- `dev.by_step.solar-assembly.aid` = India, Viet Nam, Thailand, Malaysia, Mexico and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.130** Rare-earth and battery-metal refining

- `dev.by_step.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.131** Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed

- `dev.by_step.electric-vehicles-refining.reason` = Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.132** Thailand and Malaysia

- `dev.by_step.electric-vehicles-refining.commercial` = Thailand and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.133** Argentina, Indonesia, Mozambique, the Democratic Republic of Congo, the Philippines, Mexico, Tanzania and Madagascar

- `dev.by_step.electric-vehicles-refining.review` = Argentina, Indonesia, Mozambique, the Democratic Republic of Congo, the Philippines, Mexico, Tanzania and Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.134** Thailand, Argentina, Indonesia, Mozambique, the Democratic Republic of Congo, the Philippines, Mexico and Tanzania

- `dev.by_step.electric-vehicles-refining.aid` = Thailand, Argentina, Indonesia, Mozambique, the Democratic Republic of Congo, the Philippines, Mexico and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.135** Uranium conversion and enrichment

- `dev.by_step.nuclear-refining.name` = Uranium conversion and enrichment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.136** High need

- `dev.by_step.nuclear-refining.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.137** Namibia

- `dev.by_step.nuclear-refining.review` = Namibia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.138** Coal-plant components

- `dev.by_step.coal-components.name` = Coal-plant components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.139** High need

- `dev.by_step.coal-components.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.140** Viet Nam, India, Turkiye and Indonesia

- `dev.by_step.coal-components.review` = Viet Nam, India, Turkiye and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.141** Viet Nam, India and Indonesia

- `dev.by_step.coal-components.aid` = Viet Nam, India and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.142** Battery recycling

- `dev.by_step.batteries-recovery.name` = Battery recycling | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.143** High need

- `dev.by_step.batteries-recovery.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.144** India and Malaysia

- `dev.by_step.batteries-recovery.review` = India and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.145** Polysilicon refining

- `dev.by_step.solar-refining.name` = Polysilicon refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.146** High need

- `dev.by_step.solar-refining.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.147** India

- `dev.by_step.solar-refining.review` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.148** India

- `dev.by_step.solar-refining.aid` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.159** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.160** Extraction

- `cells.batteries-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.161** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.162** Medium

- `cells.batteries-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.163** 98

- `cells.batteries-extraction.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.164** 57

- `cells.batteries-extraction.siv` = 57 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.165** 0

- `cells.batteries-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.166** strong

- `cells.batteries-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.167** broadly aligned

- `cells.batteries-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.168** Battery materials refining

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.169** Refining

- `cells.batteries-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.170** Catch Up

- `cells.batteries-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.171** Medium

- `cells.batteries-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.172** 94

- `cells.batteries-refining.need` = 94 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.173** 78

- `cells.batteries-refining.siv` = 78 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.174** 19

- `cells.batteries-refining.cp` = 19 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.175** strong

- `cells.batteries-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.176** broadly aligned

- `cells.batteries-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.177** Battery components

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.178** Components

- `cells.batteries-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.179** Catch Up

- `cells.batteries-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.180** Medium

- `cells.batteries-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.181** 75

- `cells.batteries-components.need` = 75 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.182** 79

- `cells.batteries-components.siv` = 79 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.183** 55

- `cells.batteries-components.cp` = 55 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.184** moderate

- `cells.batteries-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.185** broadly aligned

- `cells.batteries-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.186** Battery cells and packs

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.187** Assembly

- `cells.batteries-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.188** Catch Up

- `cells.batteries-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.189** Mixed High/Medium/Low

- `cells.batteries-assembly.yard` = Mixed High/Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.190** 71

- `cells.batteries-assembly.need` = 71 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**a.191** 97

- `cells.batteries-assembly.siv` = 97 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.192** 71

- `cells.batteries-assembly.cp` = 71 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**a.193** moderate

- `cells.batteries-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.194** broadly aligned

- `cells.batteries-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence low

**a.195** Battery storage

- `cells.batteries-deployment.name` = Battery storage | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.196** Deployment

- `cells.batteries-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.197** Watch

- `cells.batteries-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.198** Low

- `cells.batteries-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.199** 55

- `cells.batteries-deployment.dn` = 55 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.200** strong

- `cells.batteries-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.201** broadly aligned

- `cells.batteries-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.202** Battery recycling

- `cells.batteries-recovery.name` = Battery recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.203** Recovery

- `cells.batteries-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.204** Catch Up

- `cells.batteries-recovery.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.205** Medium

- `cells.batteries-recovery.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.206** 98

- `cells.batteries-recovery.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence moderate

**a.207** 82

- `cells.batteries-recovery.siv` = 82 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence moderate

**a.208** mixed signals

- `cells.batteries-recovery.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate

**a.209** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.210** Extraction

- `cells.coal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.211** Secure

- `cells.coal-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.212** High

- `cells.coal-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.213** 97

- `cells.coal-extraction.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.214** 4

- `cells.coal-extraction.siv` = 4 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.215** 8

- `cells.coal-extraction.cp` = 8 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.216** strong

- `cells.coal-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.217** needs more attention

- `cells.coal-extraction.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.218** Coke and coal processing

- `cells.coal-refining.name` = Coke and coal processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.219** Refining

- `cells.coal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.220** Watch

- `cells.coal-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.221** Low

- `cells.coal-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.222** 84

- `cells.coal-refining.need` = 84 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.223** 9

- `cells.coal-refining.siv` = 9 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.224** 19

- `cells.coal-refining.cp` = 19 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.225** strong

- `cells.coal-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.226** mixed signals

- `cells.coal-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.227** Coal-plant components

- `cells.coal-components.name` = Coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.228** Components

- `cells.coal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.229** Preserve

- `cells.coal-components.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.230** Medium

- `cells.coal-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.231** 72

- `cells.coal-components.need` = 72 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.232** 58

- `cells.coal-components.siv` = 58 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.233** 95

- `cells.coal-components.cp` = 95 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.234** strong

- `cells.coal-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.235** broadly aligned

- `cells.coal-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.236** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.237** Assembly

- `cells.coal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.238** Watch

- `cells.coal-assembly.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.239** Low

- `cells.coal-assembly.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.240** 59

- `cells.coal-assembly.need` = 59 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.241** 58

- `cells.coal-assembly.siv` = 58 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.242** 80

- `cells.coal-assembly.cp` = 80 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.243** strong

- `cells.coal-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.244** broadly aligned

- `cells.coal-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.245** Coal power and use

- `cells.coal-deployment.name` = Coal power and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.246** Deployment

- `cells.coal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.247** Watch

- `cells.coal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.248** Low

- `cells.coal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.249** 40

- `cells.coal-deployment.dn` = 40 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.250** strong

- `cells.coal-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.251** broadly aligned

- `cells.coal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.252** Coal recovery

- `cells.coal-recovery.name` = Coal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.253** Recovery

- `cells.coal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.254** Not assessed

- `cells.coal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.255** 1

- `cells.coal-recovery.siv` = 1 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.256** not enough policy evidence

- `cells.coal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.257** Copper and bauxite mining

- `cells.electric-grid-extraction.name` = Copper and bauxite mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.258** Extraction

- `cells.electric-grid-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.259** Watch

- `cells.electric-grid-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.260** Low

- `cells.electric-grid-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.261** 98

- `cells.electric-grid-extraction.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.262** 41

- `cells.electric-grid-extraction.siv` = 41 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.263** 0

- `cells.electric-grid-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.264** strong

- `cells.electric-grid-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.265** broadly aligned

- `cells.electric-grid-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.266** Grid metals refining

- `cells.electric-grid-refining.name` = Grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.267** Refining

- `cells.electric-grid-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.268** Watch

- `cells.electric-grid-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.269** Mixed Medium/Low

- `cells.electric-grid-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.270** 72

- `cells.electric-grid-refining.need` = 72 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.271** 53

- `cells.electric-grid-refining.siv` = 53 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.272** 25

- `cells.electric-grid-refining.cp` = 25 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.273** strong

- `cells.electric-grid-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.274** broadly aligned

- `cells.electric-grid-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.275** Grid components

- `cells.electric-grid-components.name` = Grid components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.276** Components

- `cells.electric-grid-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.277** Watch

- `cells.electric-grid-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.278** Low

- `cells.electric-grid-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.279** 35

- `cells.electric-grid-components.need` = 35 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.280** 47

- `cells.electric-grid-components.siv` = 47 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.281** 88

- `cells.electric-grid-components.cp` = 88 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.282** strong

- `cells.electric-grid-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.283** broadly aligned

- `cells.electric-grid-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.284** Transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = Transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.285** Assembly

- `cells.electric-grid-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.286** Catch Up

- `cells.electric-grid-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.287** Mixed High/Medium/Low

- `cells.electric-grid-assembly.yard` = Mixed High/Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.288** 67

- `cells.electric-grid-assembly.need` = 67 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.289** 61

- `cells.electric-grid-assembly.siv` = 61 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.290** 30

- `cells.electric-grid-assembly.cp` = 30 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.291** strong

- `cells.electric-grid-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.292** needs more attention

- `cells.electric-grid-assembly.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.293** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.294** Deployment

- `cells.electric-grid-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.295** Watch

- `cells.electric-grid-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.296** Low

- `cells.electric-grid-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.297** 52

- `cells.electric-grid-deployment.dn` = 52 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence high

**a.298** broadly aligned

- `cells.electric-grid-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence high

**a.299** Grid-equipment recycling

- `cells.electric-grid-recovery.name` = Grid-equipment recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.300** Recovery

- `cells.electric-grid-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.301** Not assessed

- `cells.electric-grid-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.302** 14

- `cells.electric-grid-recovery.siv` = 14 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.303** not enough policy evidence

- `cells.electric-grid-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.304** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.305** Extraction

- `cells.electric-vehicles-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.306** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.307** Medium

- `cells.electric-vehicles-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.308** 98

- `cells.electric-vehicles-extraction.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.309** 59

- `cells.electric-vehicles-extraction.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.310** 0

- `cells.electric-vehicles-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.311** strong

- `cells.electric-vehicles-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.312** broadly aligned

- `cells.electric-vehicles-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.313** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.314** Refining

- `cells.electric-vehicles-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.315** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.316** High

- `cells.electric-vehicles-refining.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.317** 100

- `cells.electric-vehicles-refining.need` = 100 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.318** 84

- `cells.electric-vehicles-refining.siv` = 84 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.319** 32

- `cells.electric-vehicles-refining.cp` = 32 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.320** strong

- `cells.electric-vehicles-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.321** mixed signals

- `cells.electric-vehicles-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.322** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.323** Components

- `cells.electric-vehicles-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.324** Catch Up

- `cells.electric-vehicles-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.325** Mixed High/Medium

- `cells.electric-vehicles-components.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.326** 62

- `cells.electric-vehicles-components.need` = 62 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.327** 93

- `cells.electric-vehicles-components.siv` = 93 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.328** 60

- `cells.electric-vehicles-components.cp` = 60 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.329** moderate

- `cells.electric-vehicles-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.330** mixed signals

- `cells.electric-vehicles-components.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.331** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.332** Assembly

- `cells.electric-vehicles-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.333** Catch Up

- `cells.electric-vehicles-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.334** Mixed High/Medium

- `cells.electric-vehicles-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.335** 65

- `cells.electric-vehicles-assembly.need` = 65 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**a.336** 93

- `cells.electric-vehicles-assembly.siv` = 93 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.337** 72

- `cells.electric-vehicles-assembly.cp` = 72 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence low

**a.338** moderate

- `cells.electric-vehicles-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.339** needs more attention

- `cells.electric-vehicles-assembly.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence low

**a.340** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.341** Deployment

- `cells.electric-vehicles-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.342** Watch

- `cells.electric-vehicles-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.343** Low

- `cells.electric-vehicles-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.344** 46

- `cells.electric-vehicles-deployment.dn` = 46 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.345** strong

- `cells.electric-vehicles-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.346** broadly aligned

- `cells.electric-vehicles-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.347** EV recycling

- `cells.electric-vehicles-recovery.name` = EV recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.348** Recovery

- `cells.electric-vehicles-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.349** Not assessed

- `cells.electric-vehicles-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.350** 37

- `cells.electric-vehicles-recovery.siv` = 37 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.351** not enough policy evidence

- `cells.electric-vehicles-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.352** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.353** Extraction

- `cells.gas-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.354** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.355** High

- `cells.gas-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.356** 94

- `cells.gas-extraction.need` = 94 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.357** 11

- `cells.gas-extraction.siv` = 11 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.358** 4

- `cells.gas-extraction.cp` = 4 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.359** strong

- `cells.gas-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.360** needs more attention

- `cells.gas-extraction.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.361** Gas liquids processing

- `cells.gas-refining.name` = Gas liquids processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.362** Refining

- `cells.gas-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.363** Watch

- `cells.gas-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.364** Low

- `cells.gas-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.365** 74

- `cells.gas-refining.need` = 74 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.366** 11

- `cells.gas-refining.siv` = 11 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.367** 4

- `cells.gas-refining.cp` = 4 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.368** strong

- `cells.gas-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.369** mixed signals

- `cells.gas-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.370** Gas-turbine and drilling components

- `cells.gas-components.name` = Gas-turbine and drilling components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.371** Components

- `cells.gas-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.372** Watch

- `cells.gas-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.373** Low

- `cells.gas-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.374** 59

- `cells.gas-components.need` = 59 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.375** 45

- `cells.gas-components.siv` = 45 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.376** 84

- `cells.gas-components.cp` = 84 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.377** strong

- `cells.gas-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.378** broadly aligned

- `cells.gas-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.379** Gas turbines and LNG plant

- `cells.gas-assembly.name` = Gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.380** Assembly

- `cells.gas-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.381** Watch

- `cells.gas-assembly.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.382** Low

- `cells.gas-assembly.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.383** 40

- `cells.gas-assembly.need` = 40 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.384** 45

- `cells.gas-assembly.siv` = 45 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.385** 83

- `cells.gas-assembly.cp` = 83 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.386** strong

- `cells.gas-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.387** broadly aligned

- `cells.gas-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.388** Gas networks and use

- `cells.gas-deployment.name` = Gas networks and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.389** Deployment

- `cells.gas-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.390** Accelerate

- `cells.gas-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.391** Medium

- `cells.gas-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.392** 63

- `cells.gas-deployment.dn` = 63 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.393** strong

- `cells.gas-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.394** broadly aligned

- `cells.gas-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.395** Gas recovery

- `cells.gas-recovery.name` = Gas recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.396** Recovery

- `cells.gas-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.397** Not assessed

- `cells.gas-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.398** 7

- `cells.gas-recovery.siv` = 7 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.399** not enough policy evidence

- `cells.gas-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.400** Geothermal resource development

- `cells.geothermal-extraction.name` = Geothermal resource development | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.401** Extraction

- `cells.geothermal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.402** Not assessed

- `cells.geothermal-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.403** 20

- `cells.geothermal-extraction.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.404** not enough policy evidence

- `cells.geothermal-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.405** Geothermal refining

- `cells.geothermal-refining.name` = Geothermal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.406** Refining

- `cells.geothermal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.407** Not assessed

- `cells.geothermal-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.408** 20

- `cells.geothermal-refining.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.409** not enough policy evidence

- `cells.geothermal-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.410** Geothermal plant components

- `cells.geothermal-components.name` = Geothermal plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.411** Components

- `cells.geothermal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.412** Not assessed

- `cells.geothermal-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.413** 69

- `cells.geothermal-components.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.414** not enough policy evidence

- `cells.geothermal-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.415** Geothermal power-plant equipment

- `cells.geothermal-assembly.name` = Geothermal power-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.416** Assembly

- `cells.geothermal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.417** Not assessed

- `cells.geothermal-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.418** 69

- `cells.geothermal-assembly.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.419** not enough policy evidence

- `cells.geothermal-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.420** Geothermal power and heat

- `cells.geothermal-deployment.name` = Geothermal power and heat | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.421** Deployment

- `cells.geothermal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.422** Watch

- `cells.geothermal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.423** Low

- `cells.geothermal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.424** 57

- `cells.geothermal-deployment.dn` = 57 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.425** strong

- `cells.geothermal-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.426** broadly aligned

- `cells.geothermal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.427** Geothermal recovery

- `cells.geothermal-recovery.name` = Geothermal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.428** Recovery

- `cells.geothermal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.429** Not assessed

- `cells.geothermal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.430** 20

- `cells.geothermal-recovery.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.431** not enough policy evidence

- `cells.geothermal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.432** Electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = Electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.433** Extraction

- `cells.green-hydrogen-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.434** Not assessed

- `cells.green-hydrogen-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.435** 35

- `cells.green-hydrogen-extraction.siv` = 35 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.436** 0

- `cells.green-hydrogen-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.437** strong

- `cells.green-hydrogen-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.438** not enough policy evidence

- `cells.green-hydrogen-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.439** PGM and catalyst refining

- `cells.green-hydrogen-refining.name` = PGM and catalyst refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.440** Refining

- `cells.green-hydrogen-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.441** Not assessed

- `cells.green-hydrogen-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.442** 63

- `cells.green-hydrogen-refining.siv` = 63 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.443** 98

- `cells.green-hydrogen-refining.cp` = 98 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.444** strong

- `cells.green-hydrogen-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.445** not enough policy evidence

- `cells.green-hydrogen-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.446** Electrolyser stacks

- `cells.green-hydrogen-components.name` = Electrolyser stacks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.447** Components

- `cells.green-hydrogen-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.448** Not assessed

- `cells.green-hydrogen-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.449** 99

- `cells.green-hydrogen-components.siv` = 99 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.450** not enough policy evidence

- `cells.green-hydrogen-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.451** Electrolyser systems

- `cells.green-hydrogen-assembly.name` = Electrolyser systems | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.452** Assembly

- `cells.green-hydrogen-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.453** Not assessed

- `cells.green-hydrogen-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.454** 90

- `cells.green-hydrogen-assembly.siv` = 90 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.455** not enough policy evidence

- `cells.green-hydrogen-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.456** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.457** Deployment

- `cells.green-hydrogen-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.458** Accelerate

- `cells.green-hydrogen-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.459** Medium

- `cells.green-hydrogen-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.460** 74

- `cells.green-hydrogen-deployment.dn` = 74 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence low

**a.461** broadly aligned

- `cells.green-hydrogen-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence low

**a.462** Stack and catalyst recycling

- `cells.green-hydrogen-recovery.name` = Stack and catalyst recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.463** Recovery

- `cells.green-hydrogen-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.464** Not assessed

- `cells.green-hydrogen-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.465** 49

- `cells.green-hydrogen-recovery.siv` = 49 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.466** not enough policy evidence

- `cells.green-hydrogen-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.467** Uranium mining

- `cells.nuclear-extraction.name` = Uranium mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.468** Extraction

- `cells.nuclear-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.469** Watch

- `cells.nuclear-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.470** Low

- `cells.nuclear-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.471** 61

- `cells.nuclear-extraction.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.472** 42

- `cells.nuclear-extraction.siv` = 42 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.473** 0

- `cells.nuclear-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.474** strong

- `cells.nuclear-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.475** mixed signals

- `cells.nuclear-extraction.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.476** Uranium conversion and enrichment

- `cells.nuclear-refining.name` = Uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.477** Refining

- `cells.nuclear-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.478** Catch Up

- `cells.nuclear-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.479** Medium

- `cells.nuclear-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.480** 97

- `cells.nuclear-refining.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.481** 80

- `cells.nuclear-refining.siv` = 80 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.482** 20

- `cells.nuclear-refining.cp` = 20 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.483** strong

- `cells.nuclear-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.484** mixed signals

- `cells.nuclear-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.485** Nuclear fuel and reactor parts

- `cells.nuclear-components.name` = Nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.486** Components

- `cells.nuclear-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.487** Catch Up

- `cells.nuclear-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.488** Medium

- `cells.nuclear-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.489** 61

- `cells.nuclear-components.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.490** 78

- `cells.nuclear-components.siv` = 78 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.491** 61

- `cells.nuclear-components.cp` = 61 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.492** strong

- `cells.nuclear-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.493** broadly aligned

- `cells.nuclear-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.494** Reactors and steam generators

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.495** Assembly

- `cells.nuclear-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.496** Preserve

- `cells.nuclear-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.497** Mixed High/Medium

- `cells.nuclear-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.498** 66

- `cells.nuclear-assembly.need` = 66 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.499** 96

- `cells.nuclear-assembly.siv` = 96 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.500** 79

- `cells.nuclear-assembly.cp` = 79 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.501** strong

- `cells.nuclear-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.502** needs more attention

- `cells.nuclear-assembly.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.503** Nuclear power

- `cells.nuclear-deployment.name` = Nuclear power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.504** Deployment

- `cells.nuclear-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.505** Accelerate

- `cells.nuclear-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.506** Medium

- `cells.nuclear-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.507** 78

- `cells.nuclear-deployment.dn` = 78 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.508** strong

- `cells.nuclear-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.509** broadly aligned

- `cells.nuclear-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.510** Spent-fuel reprocessing

- `cells.nuclear-recovery.name` = Spent-fuel reprocessing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.511** Recovery

- `cells.nuclear-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.512** Not assessed

- `cells.nuclear-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.513** 80

- `cells.nuclear-recovery.siv` = 80 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.514** not enough policy evidence

- `cells.nuclear-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.515** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.516** Extraction

- `cells.oil-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.517** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.518** High

- `cells.oil-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.519** 98

- `cells.oil-extraction.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.520** 6

- `cells.oil-extraction.siv` = 6 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.521** 0

- `cells.oil-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.522** strong

- `cells.oil-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.523** needs more attention

- `cells.oil-extraction.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.524** Oil refining

- `cells.oil-refining.name` = Oil refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.525** Refining

- `cells.oil-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.526** Watch

- `cells.oil-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.527** Low

- `cells.oil-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.528** 53

- `cells.oil-refining.need` = 53 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.529** 24

- `cells.oil-refining.siv` = 24 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.530** 29

- `cells.oil-refining.cp` = 29 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.531** strong

- `cells.oil-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.532** broadly aligned

- `cells.oil-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.533** Drilling and pump components

- `cells.oil-components.name` = Drilling and pump components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.534** Components

- `cells.oil-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.535** Watch

- `cells.oil-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.536** Low

- `cells.oil-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.537** 59

- `cells.oil-components.need` = 59 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.538** 12

- `cells.oil-components.siv` = 12 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.539** 10

- `cells.oil-components.cp` = 10 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.540** strong

- `cells.oil-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.541** mixed signals

- `cells.oil-components.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.542** Drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = Drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.543** Assembly

- `cells.oil-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.544** Watch

- `cells.oil-assembly.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.545** Mixed High/Low

- `cells.oil-assembly.yard` = Mixed High/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.546** 36

- `cells.oil-assembly.need` = 36 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.547** 31

- `cells.oil-assembly.siv` = 31 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.548** 87

- `cells.oil-assembly.cp` = 87 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.549** strong

- `cells.oil-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.550** mixed signals

- `cells.oil-assembly.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.551** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.552** Deployment

- `cells.oil-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.553** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.554** Low

- `cells.oil-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.555** 8

- `cells.oil-deployment.dn` = 8 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.556** strong

- `cells.oil-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.557** broadly aligned

- `cells.oil-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.558** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.559** Recovery

- `cells.oil-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.560** Watch

- `cells.oil-recovery.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.561** Low

- `cells.oil-recovery.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high

**a.562** 43

- `cells.oil-recovery.need` = 43 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence high

**a.563** 2

- `cells.oil-recovery.siv` = 2 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence high

**a.564** broadly aligned

- `cells.oil-recovery.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high

**a.565** PV mineral mining

- `cells.solar-extraction.name` = PV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.566** Extraction

- `cells.solar-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.567** Watch

- `cells.solar-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.568** Low

- `cells.solar-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.569** 98

- `cells.solar-extraction.need` = 98 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.570** 25

- `cells.solar-extraction.siv` = 25 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.571** 0

- `cells.solar-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.572** strong

- `cells.solar-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.573** mixed signals

- `cells.solar-extraction.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.574** Polysilicon refining

- `cells.solar-refining.name` = Polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.575** Refining

- `cells.solar-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.576** Watch

- `cells.solar-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.577** Mixed Medium/Low

- `cells.solar-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.578** 95

- `cells.solar-refining.need` = 95 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.579** 51

- `cells.solar-refining.siv` = 51 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.580** 42

- `cells.solar-refining.cp` = 42 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.581** strong

- `cells.solar-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.582** mixed signals

- `cells.solar-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.583** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.584** Components

- `cells.solar-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.585** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.586** Mixed Medium/Low

- `cells.solar-components.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.587** 32

- `cells.solar-components.need` = 32 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.588** 59

- `cells.solar-components.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.589** 57

- `cells.solar-components.cp` = 57 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.590** moderate

- `cells.solar-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.591** too much intervention

- `cells.solar-components.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.592** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.593** Assembly

- `cells.solar-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.594** Catch Up

- `cells.solar-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.595** Mixed High/Medium/Low

- `cells.solar-assembly.yard` = Mixed High/Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.596** 55

- `cells.solar-assembly.need` = 55 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.597** 80

- `cells.solar-assembly.siv` = 80 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.598** 28

- `cells.solar-assembly.cp` = 28 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.599** moderate

- `cells.solar-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.600** broadly aligned

- `cells.solar-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.601** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.602** Deployment

- `cells.solar-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.603** Accelerate

- `cells.solar-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.604** Medium

- `cells.solar-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.605** 66

- `cells.solar-deployment.dn` = 66 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.606** strong

- `cells.solar-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.607** deployment policy lags

- `cells.solar-deployment.alignment` = deployment policy lags | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.608** PV module recycling

- `cells.solar-recovery.name` = PV module recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.609** Recovery

- `cells.solar-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.610** Not assessed

- `cells.solar-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.611** 44

- `cells.solar-recovery.siv` = 44 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.612** not enough policy evidence

- `cells.solar-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.613** Wind mineral mining

- `cells.wind-extraction.name` = Wind mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.614** Extraction

- `cells.wind-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.615** Not assessed

- `cells.wind-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.616** 40

- `cells.wind-extraction.siv` = 40 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.617** 8

- `cells.wind-extraction.cp` = 8 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.618** strong

- `cells.wind-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.619** not enough policy evidence

- `cells.wind-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.620** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.621** Refining

- `cells.wind-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.622** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.623** Mixed High/Medium

- `cells.wind-refining.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.624** 69

- `cells.wind-refining.need` = 69 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.625** 52

- `cells.wind-refining.siv` = 52 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.626** 40

- `cells.wind-refining.cp` = 40 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.627** strong

- `cells.wind-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.628** broadly aligned

- `cells.wind-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.629** Blades, towers and nacelle parts

- `cells.wind-components.name` = Blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.630** Components

- `cells.wind-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.631** Catch Up

- `cells.wind-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.632** Mixed High/Medium

- `cells.wind-components.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.633** 60

- `cells.wind-components.need` = 60 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.634** 88

- `cells.wind-components.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.635** 55

- `cells.wind-components.cp` = 55 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.636** moderate

- `cells.wind-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.637** mixed signals

- `cells.wind-components.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.638** Wind turbines

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.639** Assembly

- `cells.wind-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.640** Catch Up

- `cells.wind-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.641** Mixed High/Medium

- `cells.wind-assembly.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.642** 75

- `cells.wind-assembly.need` = 75 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.643** 88

- `cells.wind-assembly.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.644** 16

- `cells.wind-assembly.cp` = 16 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.645** moderate

- `cells.wind-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.646** needs more attention

- `cells.wind-assembly.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.647** Wind power

- `cells.wind-deployment.name` = Wind power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.648** Deployment

- `cells.wind-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.649** Accelerate

- `cells.wind-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.650** Medium

- `cells.wind-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.651** 65

- `cells.wind-deployment.dn` = 65 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.652** strong

- `cells.wind-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.653** broadly aligned

- `cells.wind-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.654** Wind-turbine recycling

- `cells.wind-recovery.name` = Wind-turbine recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.655** Recovery

- `cells.wind-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.656** Not assessed

- `cells.wind-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.657** 28

- `cells.wind-recovery.siv` = 28 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.658** not enough policy evidence

- `cells.wind-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.664** Anode materials (natural and synthetic graphite)

- `cells.batteries-refining.layers.bess-anode.name` = Anode materials (natural and synthetic graphite) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.665** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.666** Catch Up

- `cells.batteries-refining.layers.bess-anode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.667** Medium

- `cells.batteries-refining.layers.bess-anode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.668** Cathode active materials and precursors

- `cells.batteries-refining.layers.bess-cathode.name` = Cathode active materials and precursors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.669** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.670** Catch Up

- `cells.batteries-refining.layers.bess-cathode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.671** Medium

- `cells.batteries-refining.layers.bess-cathode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.672** Battery-grade lithium hydroxide and carbonate

- `cells.batteries-refining.layers.li-chemicals.name` = Battery-grade lithium hydroxide and carbonate | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.673** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.674** Catch Up

- `cells.batteries-refining.layers.li-chemicals.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.675** Medium

- `cells.batteries-refining.layers.li-chemicals.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.676** Battery cells (LFP and NMC)

- `cells.batteries-assembly.layers.bess-cells.name` = Battery cells (LFP and NMC) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.677** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.678** Catch Up

- `cells.batteries-assembly.layers.bess-cells.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.679** Medium

- `cells.batteries-assembly.layers.bess-cells.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.680** Communications gateway and cloud fleet management

- `cells.batteries-assembly.layers.bess-comms.name` = Communications gateway and cloud fleet management | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.681** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.682** Secure

- `cells.batteries-assembly.layers.bess-comms.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.683** High

- `cells.batteries-assembly.layers.bess-comms.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.684** Battery-management firmware, energy-management software and remote updates

- `cells.batteries-assembly.layers.bess-control.name` = Battery-management firmware, energy-management software and remote updates | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.685** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.686** Secure

- `cells.batteries-assembly.layers.bess-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.687** High

- `cells.batteries-assembly.layers.bess-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.688** Enclosures, racking, thermal management and balance of system

- `cells.batteries-assembly.layers.bess-enclosure.name` = Enclosures, racking, thermal management and balance of system | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.689** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.690** Watch

- `cells.batteries-assembly.layers.bess-enclosure.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.691** Low

- `cells.batteries-assembly.layers.bess-enclosure.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.692** Grain-oriented electrical steel

- `cells.electric-grid-refining.layers.goes.name` = Grain-oriented electrical steel | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.693** grid metals refining

- `cells.electric-grid-refining.name` = grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.694** Catch Up

- `cells.electric-grid-refining.layers.goes.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.695** Medium

- `cells.electric-grid-refining.layers.goes.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.696** Power conversion hardware (inverters and converters)

- `cells.electric-grid-assembly.layers.bess-pcs.name` = Power conversion hardware (inverters and converters) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.697** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.698** Catch Up

- `cells.electric-grid-assembly.layers.bess-pcs.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.699** Medium

- `cells.electric-grid-assembly.layers.bess-pcs.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.700** Smart meters and advanced metering head-end systems

- `cells.electric-grid-assembly.layers.grid-ami.name` = Smart meters and advanced metering head-end systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.701** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.702** Secure

- `cells.electric-grid-assembly.layers.grid-ami.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.703** High

- `cells.electric-grid-assembly.layers.grid-ami.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.704** Cables and conductors

- `cells.electric-grid-assembly.layers.grid-cables.name` = Cables and conductors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.705** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.706** Watch

- `cells.electric-grid-assembly.layers.grid-cables.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.707** Low

- `cells.electric-grid-assembly.layers.grid-cables.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.708** Utility-scale inverter firmware and remote-access functions

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.name` = Utility-scale inverter firmware and remote-access functions | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.709** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.710** Secure

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.711** High

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.712** Grid-control software (SCADA, EMS, DERMS)

- `cells.electric-grid-assembly.layers.grid-scada.name` = Grid-control software (SCADA, EMS, DERMS) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.713** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.714** Secure

- `cells.electric-grid-assembly.layers.grid-scada.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.715** High

- `cells.electric-grid-assembly.layers.grid-scada.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.716** Large power transformers

- `cells.electric-grid-assembly.layers.grid-transformers.name` = Large power transformers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.717** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.718** Catch Up

- `cells.electric-grid-assembly.layers.grid-transformers.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.719** Medium

- `cells.electric-grid-assembly.layers.grid-transformers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.720** HVDC converter-station control and protection

- `cells.electric-grid-assembly.layers.hvdc-control.name` = HVDC converter-station control and protection | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.721** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.722** Catch Up

- `cells.electric-grid-assembly.layers.hvdc-control.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.723** Medium

- `cells.electric-grid-assembly.layers.hvdc-control.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.724** Electric drive units and motors

- `cells.electric-vehicles-components.layers.ev-motors.name` = Electric drive units and motors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.725** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.726** Catch Up

- `cells.electric-vehicles-components.layers.ev-motors.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.727** Medium

- `cells.electric-vehicles-components.layers.ev-motors.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.728** Sintered NdFeB permanent magnets (traction motors)

- `cells.electric-vehicles-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets (traction motors) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.729** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.730** Secure

- `cells.electric-vehicles-components.layers.ree-magnets.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.731** High

- `cells.electric-vehicles-components.layers.ree-magnets.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.732** Connected-vehicle telematics, over-the-air software and connectivity systems

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.name` = Connected-vehicle telematics, over-the-air software and connectivity systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence low

**a.733** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.734** Secure

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.735** High

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence low

**a.736** Charging-network management software (charge-point back ends)

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.name` = Charging-network management software (charge-point back ends) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.737** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.738** Watch

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.739** Low

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.740** Pipeline SCADA, compressor-station and LNG-terminal control systems

- `cells.gas-assembly.layers.gas-ot-control.name` = Pipeline SCADA, compressor-station and LNG-terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.741** gas turbines and LNG plant

- `cells.gas-assembly.name` = gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.742** Watch

- `cells.gas-assembly.layers.gas-ot-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.743** Low

- `cells.gas-assembly.layers.gas-ot-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.744** Iridium and platinum for PEM electrolyser catalysts

- `cells.green-hydrogen-extraction.layers.h2-pgm.name` = Iridium and platinum for PEM electrolyser catalysts | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.745** electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.746** Not assessed

- `cells.green-hydrogen-extraction.layers.h2-pgm.choice` = Not assessed | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.747** HALEU enrichment

- `cells.nuclear-refining.layers.nuc-haleu.name` = HALEU enrichment | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.748** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.749** Catch Up

- `cells.nuclear-refining.layers.nuc-haleu.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.750** Medium

- `cells.nuclear-refining.layers.nuc-haleu.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.751** LEU enrichment services

- `cells.nuclear-refining.layers.nuc-leu.name` = LEU enrichment services | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.752** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.753** Catch Up

- `cells.nuclear-refining.layers.nuc-leu.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.754** Medium

- `cells.nuclear-refining.layers.nuc-leu.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.755** Fuel assemblies (design-specific fabrication)

- `cells.nuclear-components.layers.nuc-fuel-fab.name` = Fuel assemblies (design-specific fabrication) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.756** nuclear fuel and reactor parts

- `cells.nuclear-components.name` = nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.757** Catch Up

- `cells.nuclear-components.layers.nuc-fuel-fab.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.758** Medium

- `cells.nuclear-components.layers.nuc-fuel-fab.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.759** Reactor control and safety instrumentation

- `cells.nuclear-assembly.layers.nuc-reactor-controls.name` = Reactor control and safety instrumentation | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.760** reactors and steam generators

- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.761** Secure

- `cells.nuclear-assembly.layers.nuc-reactor-controls.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.762** High

- `cells.nuclear-assembly.layers.nuc-reactor-controls.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.763** Oil pipeline and terminal control systems

- `cells.oil-assembly.layers.oil-ot-control.name` = Oil pipeline and terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.764** drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.765** Secure

- `cells.oil-assembly.layers.oil-ot-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.766** High

- `cells.oil-assembly.layers.oil-ot-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.767** Solar-grade polysilicon

- `cells.solar-refining.layers.polysilicon.name` = Solar-grade polysilicon | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.768** polysilicon refining

- `cells.solar-refining.name` = polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.769** Catch Up

- `cells.solar-refining.layers.polysilicon.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.770** Medium

- `cells.solar-refining.layers.polysilicon.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.771** Solar cells and wafers

- `cells.solar-components.layers.pv-cells-wafers.name` = Solar cells and wafers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.772** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.773** Catch Up

- `cells.solar-components.layers.pv-cells-wafers.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.774** Medium

- `cells.solar-components.layers.pv-cells-wafers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.775** Next-generation PV (perovskite and tandem cells)

- `cells.solar-components.layers.pv-nextgen.name` = Next-generation PV (perovskite and tandem cells) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.776** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.777** Catch Up

- `cells.solar-components.layers.pv-nextgen.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.778** Medium

- `cells.solar-components.layers.pv-nextgen.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.779** Distributed PV inverter firmware, communication dongles and vendor cloud

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.name` = Distributed PV inverter firmware, communication dongles and vendor cloud | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.780** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.781** Secure

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.782** High

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.783** Conventional crystalline-silicon modules

- `cells.solar-assembly.layers.pv-modules.name` = Conventional crystalline-silicon modules | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.784** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.785** Watch

- `cells.solar-assembly.layers.pv-modules.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.786** Low

- `cells.solar-assembly.layers.pv-modules.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.787** Passive racking and mounting

- `cells.solar-assembly.layers.pv-racking.name` = Passive racking and mounting | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.788** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.789** Watch

- `cells.solar-assembly.layers.pv-racking.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.790** Low

- `cells.solar-assembly.layers.pv-racking.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.791** Rare-earth separation and metal-making

- `cells.wind-refining.layers.ree-separation.name` = Rare-earth separation and metal-making | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.792** rare-earth and wind materials refining

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.793** Secure

- `cells.wind-refining.layers.ree-separation.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.794** High

- `cells.wind-refining.layers.ree-separation.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.795** Sintered NdFeB permanent magnets

- `cells.wind-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.796** blades, towers and nacelle parts

- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.797** Secure

- `cells.wind-components.layers.ree-magnets.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.798** High

- `cells.wind-components.layers.ree-magnets.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.799** Turbine controllers, OEM remote access and park SCADA

- `cells.wind-assembly.layers.wind-turbine-control.name` = Turbine controllers, OEM remote access and park SCADA | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.800** wind turbines

- `cells.wind-assembly.name` = wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.801** Secure

- `cells.wind-assembly.layers.wind-turbine-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.802** High

- `cells.wind-assembly.layers.wind-turbine-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.809** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.810** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.811** Australia, the United States and Canada

- `cells.batteries-extraction.partners` = Australia, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.812** Allied supply covers the need

- `cells.batteries-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.813** moderate

- `cells.batteries-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.814** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.815** Secure

- `cells.coal-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.816** the United States, Canada and Australia

- `cells.coal-extraction.partners` = the United States, Canada and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.817** Allied supply covers the need

- `cells.coal-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.818** weak

- `cells.coal-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.819** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.820** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.821** Australia, the United States and Canada

- `cells.electric-vehicles-extraction.partners` = Australia, the United States and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.822** Allied supply covers the need

- `cells.electric-vehicles-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.823** moderate

- `cells.electric-vehicles-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.824** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.825** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.826** the United States and Australia

- `cells.electric-vehicles-refining.partners` = the United States and Australia | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.827** Allied supply covers the need

- `cells.electric-vehicles-refining.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.828** moderate

- `cells.electric-vehicles-refining.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.829** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.830** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.831** Norway, the United States and Belgium

- `cells.gas-extraction.partners` = Norway, the United States and Belgium | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.832** Allied supply covers the need

- `cells.gas-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.833** weak

- `cells.gas-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.834** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.835** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.836** the United States, Canada and Norway

- `cells.oil-extraction.partners` = the United States, Canada and Norway | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.837** Allied supply falls short

- `cells.oil-extraction.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.838** weak

- `cells.oil-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.839** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.840** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.841** Germany, Belgium and Spain

- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**a.842** Allied supply falls short

- `cells.wind-refining.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.843** strong

- `cells.wind-refining.partner_evidence` = strong | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**a.850** Permanent Magnets & Drive Systems

- `aib.compacts.SC-MAGNETS.name` = Permanent Magnets & Drive Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.851** Scale manufacturer

- `aib.compacts.SC-MAGNETS.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.852** Scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-MAGNETS.functions` = Scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.853** Magnets & core steel

- `aib.compacts.SC-MAGNETS.lead_step` = Magnets & core steel | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.854** Thailand, Viet Nam, Germany, the United States, Japan and Mexico

- `aib.compacts.SC-MAGNETS.core` = Thailand, Viet Nam, Germany, the United States, Japan and Mexico | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.855** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.856** Processing anchor

- `aib.compacts.SC-BATTERIES.role` = Processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.857** Processing anchor, specialized technology supplier, scale manufacturer, demand anchor, investment anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-BATTERIES.functions` = Processing anchor, specialized technology supplier, scale manufacturer, demand anchor, investment anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.858** Refined chemicals

- `aib.compacts.SC-BATTERIES.lead_step` = Refined chemicals | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.859** the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany

- `aib.compacts.SC-BATTERIES.core` = the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.860** Semiconductors & Power Electronics

- `aib.compacts.SC-SEMIS.name` = Semiconductors & Power Electronics | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.861** Specialized technology supplier

- `aib.compacts.SC-SEMIS.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.862** Specialized technology supplier, scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-SEMIS.functions` = Specialized technology supplier, scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.863** Fab equipment

- `aib.compacts.SC-SEMIS.lead_step` = Fab equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.864** Brazil, Taiwan, Singapore, South Korea, the United States and Viet Nam

- `aib.compacts.SC-SEMIS.core` = Brazil, Taiwan, Singapore, South Korea, the United States and Viet Nam | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.865** Solar Manufacturing

- `aib.compacts.SC-SOLAR.name` = Solar Manufacturing | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.866** Specialized technology supplier

- `aib.compacts.SC-SOLAR.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.867** Specialized technology supplier, demand anchor and capacity-building host

- `aib.compacts.SC-SOLAR.functions` = Specialized technology supplier, demand anchor and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.868** Wafers & silver paste

- `aib.compacts.SC-SOLAR.lead_step` = Wafers & silver paste | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.869** Germany, Japan, South Korea, Viet Nam, the United States and India

- `aib.compacts.SC-SOLAR.core` = Germany, Japan, South Korea, Viet Nam, the United States and India | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.870** Wind & Hydropower Equipment

- `aib.compacts.SC-WIND.name` = Wind & Hydropower Equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.871** Bridge / coordination partner

- `aib.compacts.SC-WIND.role` = Bridge / coordination partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.872** Demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-WIND.functions` = Demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.873** Spain, Poland, India, Germany, the United States and the United Kingdom

- `aib.compacts.SC-WIND.core` = Spain, Poland, India, Germany, the United States and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.874** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.875** Capacity-building host

- `aib.compacts.SC-NUCLEAR.role` = Capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.876** Capacity-building host

- `aib.compacts.SC-NUCLEAR.functions` = Capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.877** Australia, Canada, India, the United States, France and the United Kingdom

- `aib.compacts.SC-NUCLEAR.core` = Australia, Canada, India, the United States, France and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.878** Grid Equipment & Conductors

- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.879** Specialized technology supplier

- `aib.compacts.SC-GRID.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.880** Specialized technology supplier, demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-GRID.functions` = Specialized technology supplier, demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.881** Wire rod & transformer steel

- `aib.compacts.SC-GRID.lead_step` = Wire rod & transformer steel | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.882** Chile, Peru, the Democratic Republic of Congo, Japan, Germany and the United States

- `aib.compacts.SC-GRID.core` = Chile, Peru, the Democratic Republic of Congo, Japan, Germany and the United States | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.883** Electric Vehicles

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.884** Scale manufacturer

- `aib.compacts.SC-EV.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.885** Scale manufacturer, investment anchor and capacity-building host

- `aib.compacts.SC-EV.functions` = Scale manufacturer, investment anchor and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.886** Electric vehicles

- `aib.compacts.SC-EV.lead_step` = Electric vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.887** Germany, the United States, France, Japan, South Korea and the United Kingdom

- `aib.compacts.SC-EV.core` = Germany, the United States, France, Japan, South Korea and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.888** Heat Pumps & Cooling

- `aib.compacts.SC-HEAT.name` = Heat Pumps & Cooling | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.889** Specialized technology supplier

- `aib.compacts.SC-HEAT.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.890** Specialized technology supplier, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-HEAT.functions` = Specialized technology supplier, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.891** Compressors

- `aib.compacts.SC-HEAT.lead_step` = Compressors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.892** the United States, the Netherlands, Thailand, Mexico, Germany and Italy

- `aib.compacts.SC-HEAT.core` = the United States, the Netherlands, Thailand, Mexico, Germany and Italy | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.893** Industrial Electrification & Hydrogen

- `aib.compacts.SC-INDUSTRY.name` = Industrial Electrification & Hydrogen | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.894** Scale manufacturer

- `aib.compacts.SC-INDUSTRY.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.895** Specialized technology supplier, scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-INDUSTRY.functions` = Specialized technology supplier, scale manufacturer, demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.896** Electrolysers

- `aib.compacts.SC-INDUSTRY.lead_step` = Electrolysers | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.897** India, Oman, South Africa, the United States, Saudi Arabia and Morocco

- `aib.compacts.SC-INDUSTRY.core` = India, Oman, South Africa, the United States, Saudi Arabia and Morocco | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.902** 30 October 2026

- `meta.as_of_label` = 30 October 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.904** 48

- `counts.assessed` = 48 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.906** 18

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.908** 34

- `counts.layers_assessed` = 34 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.910** 39

- `counts.ev_high` = 39 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.912** 8

- `counts.ev_partial` = 8 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.914** 19

- `counts.ev_none` = 19 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
