# Content trace: the United Kingdom and the Allied Industrial Base

Each claim of the brief with the dashboard field it comes from, the source datasets and the evidence quality it rests on.

**s1.2** Its core seats in three strategic compacts are led by demand roles, and its main supply contribution is uranium conversion and enrichment.

- `aib.core_in` = three | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s1.8** Gas and oil supply, which is essential and imported and takes one to three years to rebuild; Norway alone provides 74% of gas imports.

- `cells.gas-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.top_supplier` = Norway | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.top_supplier_share` = 74% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s1.9** The rare-earth chain, where China holds 94% of world refining supply.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s1.17** Battery cells and packs from producers such as Poland, the United States and Hungary, and rare-earth and wind materials from Germany, Spain and Canada, where allied supply falls short; minerals from Australia, Canada and the United States.

- `cells.batteries-assembly.partners` = Poland, the United States and Hungary | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high; partner evidence strong
- `cells.wind-refining.partners` = Germany, Spain and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong
- `cells.batteries-extraction.partners` = Australia, Canada and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s1.25** A corner flag marks a mixed step whose named parts sit behind a different fence: in battery cells and packs, the battery software is Secure, the cells Partner and the enclosures Watch.

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s2.2** Secure covers three whole steps and four layers inside other steps, grouped into four capabilities.

- `counts.secure` = three | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.secure_layers` = four | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.high_capabilities` = four | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.9** Supply-chain need is 92 out of a hundred; gas takes one to three years to rebuild.

- `high_fence.unit_gas-extraction.need` = 92 | derived: Secure steps and layers grouped by config high_fence_groups | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: curated Secure routes and layers (Yards & Fences node attributes)
- `cells.gas-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**s2.10** Norway supplies 74% of imports; potentially coercive suppliers only 2%.

- `cells.gas-extraction.top_supplier` = Norway | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.top_supplier_share` = 74% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.gas-extraction.coercive_import_share` = 2% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.13** Long-term and capacity-reservation contracts with trusted suppliers, such as the United States, Australia and Canada (a list that can include trading hubs); stockholding obligations sized to the rebuild time; stress tests.

- `cells.gas-extraction.partners` = the United States, Australia and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**s2.15** Supply-chain need is 87; oil takes one to three years to rebuild.

- `high_fence.unit_oil-extraction.need` = 87 | derived: Secure steps and layers grouped by config high_fence_groups | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: curated Secure routes and layers (Yards & Fences node attributes)
- `cells.oil-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s2.16** Norway supplies 40% of imports; trusted suppliers hold 29% of world supply, and allied supply falls short.

- `cells.oil-extraction.top_supplier` = Norway | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-extraction.top_supplier_share` = 40% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-extraction.trusted_share` = 29% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.18** Stocks sized to the rebuild time; capacity reservations with several trusted producers, such as Canada, the United States and Norway; demand-side substitution.

- `cells.oil-extraction.partners` = Canada, the United States and Norway | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**s2.20** Feeds the magnets of EV motors, the chain's Secure layer, and of wind generators; the United Kingdom holds 0% of world refining supply.

- `cells.electric-vehicles-refining.share` = 0% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.21** China holds 94% of world refining supply and trusted suppliers 4%.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.22** Rebuilding takes more than three years.

- `cells.electric-vehicles-refining.reconstitution` = more than three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**s2.24** New trusted capacity with allies such as Japan, Australia and the United States, through co-investment and offtake; at home only where announced projects can qualify; stockpiles until it qualifies.

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s2.27** China supplies 65% of battery cell and pack imports and 25% of PV module and inverter imports: hardware shares, not software.

- `cells.batteries-assembly.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.china_share` = 65% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.solar-assembly.china_share` = 25% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.30** Grid, turbine, vehicle, and oil and gas control software mostly reads Watch, and the wind-generator magnets read Catch Up inside blades, towers and nacelle parts, on that step's measures.

- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s2.31** The Secure software layers sit in the hardware bought most from potentially coercive suppliers: 68% of battery cell and pack imports and 29% of PV equipment, against 25% for transformers and switchgear.

- `cells.batteries-assembly.coercive_import_share` = 68% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.solar-assembly.coercive_import_share` = 29% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.coercive_import_share` = 25% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.36** The highest positions, 74 in EV drivetrain components and 71 in reactors and steam generators, sit just below the bar of 75.

- `cells.electric-vehicles-components.cp` = 74 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-assembly.cp` = 71 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.40** EV and charger manufacturing rests on three anchors, including operating capacity, with 4% of world exports, a position of 65 and high confidence.

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.anchors` = three | posture detail cells[].pathway_basis, pathway_credible | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.export_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.cp` = 65 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-assembly.confidence` = high | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s2.41** EV drivetrain components (74, which overlaps the cells node) and battery components (61, with committed investment) sit beside it.

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-vehicles-components.cp` = 74 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.name` = battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.cp` = 61 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.43** In nuclear, uranium conversion and enrichment rests on a share of world supply of 12%, close to the AIB's measure of the product.

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-refining.share` = 12% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.44** Its exports of the step's trade line are <1% of world exports, so verify the capacity before support scales.

- `cells.nuclear-refining.export_share` = <1% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.45** In reactors and steam generators, need (79) and value are high, and nuclear fuel and reactor parts also reads Catch Up (66, moderate confidence).

- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-assembly.need` = 79 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-components.name` = nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-components.cp` = 66 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.nuclear-components.confidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.48** Wind turbines: shares of 2% in a step whose trade codes are mostly cables, transformers and switchgear; need is low (25).

- `cells.wind-assembly.share` = 2% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-assembly.need` = 25 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.49** Blades, towers and nacelle parts, and PV modules and inverters: positions below the top quarter (58 and 19) and low need; the PV codes include chip-industry lines.

- `cells.wind-components.cp` = 58 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.solar-assembly.cp` = 19 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.50** Transformers, switchgear and cables: value just over the bar (61), low need (23) and a position of 58.

- `cells.electric-grid-assembly.siv` = 61 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.need` = 23 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-grid-assembly.cp` = 58 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.51** Battery materials refining: a single export-share anchor of about 1% and no measurable position.

- `cells.batteries-refining.export_share` = 1% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.52** Its China share of world supply, 94%, matches the Secure refining step's, which suggests overlapping trade codes.

- `cells.batteries-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.53** Battery recycling: an export share of 11% in a step whose trade codes are battery waste, scrap and residues.

- `cells.batteries-recovery.export_share` = 11% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence moderate

**s2.55** A major gap is in battery cells and packs: need is 83, China provides 65% of imports and allied supply falls short.

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.need` = 83 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.china_share` = 65% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.56** With <1% of world supply and no capability anchor, dependence should be deliberate and spread across producers such as Poland, the United States and Hungary.

- `cells.batteries-assembly.share` = <1% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.batteries-assembly.partners` = Poland, the United States and Hungary | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high; partner evidence strong

**s2.57** In rare-earth and wind materials refining allied supply also falls short; its rare-earth separation layer reads Partner too.

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s2.58** Coal-plant equipment reads Partner on high need alone (75); allied supply covers it, and coal capacity fell from 6.3 to 0.0 GW.

- `cells.coal-assembly.need` = 75 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.coal-deployment.deployment_trend` = 6.3 to 0.0 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**s2.65** Uranium conversion & enrichment

- `shared.uranium-conversion-and-enrichment.label` = Uranium conversion & enrichment | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.66** uranium mining (Watch) and uranium conversion and enrichment (Catch Up)

- `shared.uranium-conversion-and-enrichment.steps` = uranium mining (Watch) and uranium conversion and enrichment (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.67** The choices differ within the same technology with each step's own measures (uranium mining: need 97, value 30, position 0; uranium conversion and enrichment: need 61, value 71, position 25)

- `shared.uranium-conversion-and-enrichment.reading` = The choices differ within the same technology with each step's own measures (uranium mining: need 97, value 30, position 0; uranium conversion and enrichment: need 61, value 71, position 25) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.68** Supplier: 14% of world exports or production of uranium conversion & enrichment (the AIB measure of the product, not a step's share)

- `shared.uranium-conversion-and-enrichment.aib_role` = Supplier: 14% of world exports or production of uranium conversion & enrichment (the AIB measure of the product, not a step's share) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.69** AC generators

- `shared.ac-generators.label` = AC generators | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.70** drilling rigs, tankers and oil equipment (Watch) and blades, towers and nacelle parts (Catch Up)

- `shared.ac-generators.steps` = drilling rigs, tankers and oil equipment (Watch) and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.71** The choices differ across technologies with each step's own measures (drilling rigs, tankers and oil equipment: need 13, value 58, position 16; blades, towers and nacelle parts: need 30, value 89, position 58)

- `shared.ac-generators.reading` = The choices differ across technologies with each step's own measures (drilling rigs, tankers and oil equipment: need 13, value 58, position 16; blades, towers and nacelle parts: need 30, value 89, position 58) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.72** Allies need new capacity here (BUILD); no supplier role

- `shared.ac-generators.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.73** Insulators & bushings

- `shared.insulators-and-bushings.label` = Insulators & bushings | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.74** grid components (Watch); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up)

- `shared.insulators-and-bushings.steps` = grid components (Watch); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.75** The choices differ across technologies with each step's own measures (grid components: need 23, value 47, position 64; wafers and PV cells: need 26, value 59, position 26; blades, towers and nacelle parts: need 30, value 89, position 58)

- `shared.insulators-and-bushings.reading` = The choices differ across technologies with each step's own measures (grid components: need 23, value 47, position 64; wafers and PV cells: need 26, value 59, position 26; blades, towers and nacelle parts: need 30, value 89, position 58) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.76** Allies need new capacity here (BUILD); no supplier role

- `shared.insulators-and-bushings.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.77** Li-ion cells & packs

- `shared.li-ion-cells-and-packs.label` = Li-ion cells & packs | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.78** battery cells and packs (Partner) and EV drivetrain components (Catch Up)

- `shared.li-ion-cells-and-packs.steps` = battery cells and packs (Partner) and EV drivetrain components (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.79** The choices differ across technologies with each step's own measures (battery cells and packs: need 83, value 97, position 58; EV drivetrain components: need 50, value 95, position 74)

- `shared.li-ion-cells-and-packs.reading` = The choices differ across technologies with each step's own measures (battery cells and packs: need 83, value 97, position 58; EV drivetrain components: need 50, value 95, position 74) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.80** Allies need new capacity here (BUILD); no supplier role

- `shared.li-ion-cells-and-packs.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s2.82** Dashed lines mark the value bar (60) and the top quarter (75).

- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.89** Need is low (23 and 29), yet both rank near the top of the country's own recorded protection (94 and 99).

- `cells.electric-grid-components.need` = 23 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.oil-refining.need` = 29 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-grid-components.protection` = 94 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high
- `cells.oil-refining.protection` = 99 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s2.92** Need is low (26) and value rests on market size alone.

- `cells.solar-components.need` = 26 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.95** Positions of 29 and 38 rest on low need and value just under the bar: these makers compete without a fence.

- `cells.gas-assembly.cp` = 29 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.gas-components.cp` = 38 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.97** Need is high (97, 95 and 96), but these are large, commoditized yards: diversified trusted imports, with at most a limited, stated capability floor.

- `cells.nuclear-extraction.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-grid-extraction.need` = 95 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.solar-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s3.2** The United Kingdom is a core member of the trusted network, the 64 economies the Allied Industrial Base treats as allied, and in that analysis it is a Demand & Market Anchor.

- `aib.network.tier` = a core member of the trusted network | AIB strategic.countries[].slots and items | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.network.size` = 64 | AIB strategic.countries[].slots and items | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.role` = Demand & Market Anchor | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.3** It holds a role in seven of the ten strategic compacts and core seats in Wind & Hydropower Equipment, Nuclear Fuel & Reactor Systems and Electric Vehicles, each led as a demand anchor.

- `aib.n_compact_roles` = seven | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.n_compacts` = ten | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.core_in` = Wind & Hydropower Equipment, Nuclear Fuel & Reactor Systems and Electric Vehicles | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.4** Its supply roles are narrow, apart from host seats: 14% of world exports or production of uranium conversion and enrichment.

- `aib.contributes.uranium-conversion-and-enrichment.share` = 14% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.5** In Industrial Electrification & Hydrogen it also holds a processing anchor seat with 11% of world exports or production of platinum-group metals (iridium and ruthenium).

- `aib.compacts.SC-INDUSTRY.name` = Industrial Electrification & Hydrogen | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-INDUSTRY.role` = processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.contributes.pgms-iridium-ruthenium.share` = 11% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.8** If China's supply stopped, as in the Decoupling scenario, the United Kingdom would lose US$15.9 billion a year of imports across the Allied Industrial Base basket.

- `cells.batteries-assembly.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.lost_usd` = US$15.9 billion | AIB network.countries[].lostUsd | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.9** New capacity at the best-placed hosts, led by Germany, would restore about US$10.8 billion of it a year.

- `aib.investment_decoupling.top_host` = Germany | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.investment_decoupling.total` = US$10.8 billion | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.10** Under Managed Interdependence, the default scenario, it sits in the minimum coalition of two Sectoral Compacts, and of seven under Decoupling.

- `aib.scenarios.managed.label` = Managed Interdependence | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_managed` = two | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_decoupling` = seven | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.19** Electric Vehicles and Battery Materials & Cells

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.20** A large vehicle market and a vehicle base (4% of world exports of EVs and chargers)

- `cells.electric-vehicles-assembly.export_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s3.22** The EV compact's makers, Germany, the United States, Japan, France and Spain; for cells, the battery compact's maker, South Korea

- `aib.compacts.SC-EV.core_makers` = Germany, the United States, Japan, France and Spain | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-BATTERIES.core_makers` = South Korea | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.24** Permanent Magnets & Drive Systems

- `aib.all_compacts.SC-MAGNETS` = Permanent Magnets & Drive Systems | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.25** Demand for magnets in vehicles and wind; a host seat (4th host: rare-earth oxides & compounds)

- `aib.matrix.SC-MAGNETS.host.text` = 4th host: rare-earth oxides & compounds | derived: AIB strategic.compactRoles, investmentFlows.hosts and posture partner recommendations (Figure 3) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s3.27** Refiners such as Japan, Australia and the United States

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s3.29** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.30** Uranium conversion and enrichment (14%), with a core seat

- `aib.contributes.uranium-conversion-and-enrichment.share` = 14% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.32** The compact's makers, the United States, Sweden and France

- `aib.compacts.SC-NUCLEAR.core_makers` = the United States, Sweden and France | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.34** Wind & Hydropower Equipment and Grid Equipment & Conductors

- `aib.compacts.SC-WIND.name` = Wind & Hydropower Equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.37** Germany, Spain and Canada for materials; the makers India, Germany and Denmark (wind) and Germany and India (grid)

- `cells.wind-refining.partners` = Germany, Spain and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong
- `aib.compacts.SC-WIND.core_makers` = India, Germany and Denmark | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-GRID.core_makers` = Germany and India | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s3.44** In magnets the AIB finds that allied supply covers the need, though trusted suppliers hold only 4% of world refining supply.

- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s4.3** 47 pairs qualify, with 16 developing partners across 11 steps.

- `dev.n_pairs` = 47 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_partners` = 16 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_steps` = 11 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.4** The United Kingdom's own supply chain supports 7 pairs with 6 partners across two steps, and the table leads with them: battery cells and packs and rare-earth and wind materials refining.

- `dev.n_own_pairs` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_partners` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_steps` = two | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.own_steps` = battery cells and packs and rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.5** Another 40 rest on the allied gap alone, a reason that is the same for every donor in scope.

- `dev.n_allied_pairs` = 40 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.6** Malaysia, an upper middle income economy with a position of 55 in cells and packs, suits finance, guarantees and offtake.

- `dev.rows.r1.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `dev.rows.r1.cp` = 55 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: derived

**s4.7** Viet Nam, a lower middle income economy with a position of 10, needs groundwork first.

- `dev.rows.r2.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `dev.rows.r2.cp` = 10 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: derived

**s4.8** The pairs with the Philippines, Indonesia and Thailand rest on the allied gap alone.

- `dev.rows.r3.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.partner` = Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.15** Battery cells and packs: high need, allied supply falls short and new allied capacity needed

- `dev.rows.r1.capability` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.problem` = high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.16** Malaysia (upper middle income)

- `dev.rows.r1.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.17** A share of world supply and a share of world exports; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r1.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r1.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.18** Constraint: no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r1.constraints` = no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.19** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r1.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.20** A trusted second source and allied capacity where allied supply falls short (allies need new capacity in 2 product groups of the step)

- `dev.rows.r1.donor_gain` = A trusted second source and allied capacity where allied supply falls short | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.21** Factory capacity, skills and export sales of finished equipment

- `dev.rows.r1.partner_gain` = Factory capacity, skills and export sales of finished equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.22** Rare-earth and wind materials refining: high need, allied supply falls short and new allied capacity needed

- `dev.rows.r2.capability` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.problem` = high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.23** Viet Nam (lower middle income)

- `dev.rows.r2.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.24** A share of world supply and a share of world exports; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r2.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r2.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.25** Plant capacity not covered by the data and input costs not covered by the data.

- `dev.rows.r2.unobserved` = Plant capacity not covered by the data and input costs not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.26** Feasibility studies and project preparation, industrial infrastructure, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r2.tools` = Feasibility studies and project preparation, industrial infrastructure, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.27** A trusted second source and allied capacity where allied supply falls short (allies need new capacity in 2 product groups of the step)

- `dev.rows.r2.donor_gain` = A trusted second source and allied capacity where allied supply falls short | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.28** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r2.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.29** Battery materials refining: new allied capacity needed

- `dev.rows.r3.capability` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.problem` = new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.30** the Philippines (lower middle income)

- `dev.rows.r3.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.31** A share of world exports and operating capacity; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r3.anchors` = A share of world exports and operating capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r3.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.32** Constraint: input costs outside the cheapest quarter.

- `dev.rows.r3.constraints` = input costs outside the cheapest quarter | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.33** Industrial infrastructure, feasibility studies and project preparation, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r3.tools` = Industrial infrastructure, feasibility studies and project preparation, environmental and permitting capability, workforce development; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.34** New allied capacity where the AIB finds a gap (allies need new capacity in 6 product groups of the step)

- `dev.rows.r3.donor_gain` = New allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.n_build` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.35** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r3.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.36** EV drivetrain components: new allied capacity needed

- `dev.rows.r4.capability` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.problem` = new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.37** Indonesia (upper middle income)

- `dev.rows.r4.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.38** A share of world supply, a share of world exports and a strong position in the next step up the chain; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r4.anchors` = A share of world supply, a share of world exports and a strong position in the next step up the chain | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r4.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.39** Plant capacity not covered by the data.

- `dev.rows.r4.unobserved` = Plant capacity not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.40** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for supplier development and technical and vocational skills

- `dev.rows.r4.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for supplier development and technical and vocational skills | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.41** New allied capacity where the AIB finds a gap (allies need new capacity in 7 product groups of the step)

- `dev.rows.r4.donor_gain` = New allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.n_build` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.42** A certified supplier base and export sales of components

- `dev.rows.r4.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.43** Blades, towers and nacelle parts: new allied capacity needed

- `dev.rows.r5.capability` = Blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.problem` = new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.44** Thailand (upper middle income)

- `dev.rows.r5.partner` = Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.45** A share of world supply and a share of world exports; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r5.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r5.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.46** Constraint: no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r5.constraints` = no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.47** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r5.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.48** New allied capacity where the AIB finds a gap (allies need new capacity in 8 product groups of the step)

- `dev.rows.r5.donor_gain` = New allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.n_build` = 8 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.49** A certified supplier base and export sales of components

- `dev.rows.r5.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.52** Aid is the 2020 to 2024 total; the direct investment position is economy-wide, not tied to the step.

- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.56** Malaysia: battery cells and packs

- `dev.rows.r1.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.57** $0.2 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $4,294 million (economy-wide); no factory projects by firms from the United Kingdom recorded in the technology; and no export-credit or lender finance recorded at the step

- `dev.rows.r1.ties` = $0.2 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $4,294 million (economy-wide); no factory projects by firms from the United Kingdom recorded in the technology; and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.58** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Battery Materials & Cells compact; the United Kingdom's own deployment demand for the technology (Accelerate)

- `dev.rows.r1.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Battery Materials & Cells compact; the United Kingdom's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.59** Viet Nam: rare-earth and wind materials refining

- `dev.rows.r2.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.60** Under $0.1 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $325 million (economy-wide)

- `dev.rows.r2.ties` = Under $0.1 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $325 million (economy-wide) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.61** Long-term offtake and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Wind & Hydropower Equipment compact; the United Kingdom's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r2.durable` = Long-term offtake and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Wind & Hydropower Equipment compact; the United Kingdom's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.62** the Philippines: battery materials refining

- `dev.rows.r3.partner` = the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.63** $1.3 million of energy, industry and mining aid, none at this step; a direct investment position of $581 million (economy-wide); and no export-credit or lender finance recorded at the step

- `dev.rows.r3.ties` = $1.3 million of energy, industry and mining aid, none at this step; a direct investment position of $581 million (economy-wide); and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.64** Long-term offtake and investment guarantees as the capability scales; the partner's role as Bridge / coordination partner in the Battery Materials & Cells compact; the United Kingdom's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r3.durable` = Long-term offtake and investment guarantees as the capability scales; the partner's role as Bridge / coordination partner in the Battery Materials & Cells compact; the United Kingdom's own deployment demand for the technology (Accelerate); the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.65** Indonesia: EV drivetrain components

- `dev.rows.r4.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.66** $0.4 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $7,610 million (economy-wide); and no factory projects by firms from the United Kingdom recorded in the technology

- `dev.rows.r4.ties` = $0.4 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $7,610 million (economy-wide); and no factory projects by firms from the United Kingdom recorded in the technology | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.67** Long-term offtake, joint ventures and investment guarantees as the capability scales

- `dev.rows.r4.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.68** Thailand: blades, towers and nacelle parts

- `dev.rows.r5.partner` = Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.capability` = blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.69** Under $0.1 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $2,871 million (economy-wide); and no factory projects by firms from the United Kingdom recorded in the technology

- `dev.rows.r5.ties` = Under $0.1 million at this step, under stage-wide codes (the technology is not identified); a direct investment position of $2,871 million (economy-wide); and no factory projects by firms from the United Kingdom recorded in the technology | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: derived

**s4.70** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Resource supplier in the Permanent Magnets & Drive Systems compact; the United Kingdom's own deployment demand for the technology (Accelerate)

- `dev.rows.r5.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Resource supplier in the Permanent Magnets & Drive Systems compact; the United Kingdom's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.71** Battery materials refining is among the broadest strands, level with battery components and EV drivetrain components, with 7 developing economies each; all rest on the allied gap alone.

- `dev.by_step.batteries-refining.n_development` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.72** For battery mineral mining; coal-plant equipment; EV mineral mining; rare-earth and battery-metal refining; gas production; and oil production: No credible development-led supply solution identified.

- `dev.none_steps` = battery mineral mining; coal-plant equipment; EV mineral mining; rare-earth and battery-metal refining; gas production; and oil production | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.73** In rare-earth refining, Malaysia and Thailand host new capacity in the AIB, a commercial route; the other candidates rest on battery-metal capacity data or a thin anchor.

- `dev.by_step.electric-vehicles-refining.hosts` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.74** Another 34 pairs are commercial, where trade, offtake and investment fit better than aid.

- `dev.n_commercial` = 34 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.76** Of the partners, 7 receive British aid at the step, only under stage-wide codes; 6 receive it at other steps and 3 at no supply-chain step.

- `dev.aid_stage_wide` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_wrong_stage` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_none` = 3 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.77** The largest recipient of energy, industry and mining aid, Ukraine ($132 million over 2020 to 2024), has a Catch Up case in EV drivetrain components on a single supply-share anchor, but none of that aid sits at the step.

- `dev.aid_top_1` = Ukraine | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_top_1_usd` = $132 million | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.by_step.electric-vehicles-components.name` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s5.1** Wherever the equipment is made, battery storage, wind power and grid networks clear the Accelerate bar of 60.

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s5.2** Each rests on all three measures of deployment need with high confidence, though not on demand.

- `cells.batteries-deployment.dn_families` = three | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.batteries-deployment.dn_confidence` = high | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.3** System need drives storage and the grid (93 and 93), and the gap to national targets drives wind (91).

- `cells.batteries-deployment.dn_system` = 93 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.electric-grid-deployment.dn_system` = 93 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence moderate; deployment-need confidence high
- `cells.wind-deployment.dn_policy_gap` = 91 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.4** The renewables target of 153 GW by 2030 compares with 64 GW installed in 2025, a shortfall of about 69 GW at the recent build rate.

- `cells.wind-deployment.target.target_gw` = 153 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.target.target_year` = 2030 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.target.installed_gw` = 64 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.target.installed_year` = 2025 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.target.shortfall_gw` = 69 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.5** Between 2020 and 2025, wind grew from 24.5 to 33.4 GW and storage from 1.5 to 9.0 GW.

- `cells.wind-deployment.deployment_years` = 2020 and 2025 | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.deployment_trend` = 24.5 to 33.4 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.batteries-deployment.deployment_trend` = 1.5 to 9.0 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.9** Solar power (55), EV adoption (59) and coal power (58) sit just below the bar.

- `cells.solar-deployment.dn` = 55 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high
- `cells.electric-vehicles-deployment.dn` = 59 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate
- `cells.coal-deployment.dn` = 58 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**s6.7** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.8** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.9** Needs more attention

- `cells.oil-extraction.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.10** Vulnerability without enough policy attention

- `cells.oil-extraction.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.11** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.12** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.13** Needs more attention

- `cells.gas-extraction.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.14** Vulnerability without enough policy attention

- `cells.gas-extraction.gap` = Vulnerability without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.15** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.16** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s6.17** Protection or localization in the top third

- `cells.electric-vehicles-extraction.policy_flag` = Protection or localization in the top third | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.18** Domestic duplication where trusted specialization is the answer

- `cells.electric-vehicles-extraction.gap` = Domestic duplication where trusted specialization is the answer | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**s6.19** Battery materials refining

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.20** Catch Up

- `cells.batteries-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.21** Needs more attention

- `cells.batteries-refining.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.22** Capability case without enough policy attention

- `cells.batteries-refining.gap` = Capability case without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.23** Wind power

- `cells.wind-deployment.name` = Wind power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.24** Accelerate

- `cells.wind-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.25** Deployment policy below the middle

- `cells.wind-deployment.policy_flag` = Deployment policy below the middle | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.26** Deployment needed faster than policy supports

- `cells.wind-deployment.gap` = Deployment needed faster than policy supports | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.27** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**s6.28** Accelerate

- `cells.electric-grid-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**s6.29** Deployment policy below the middle

- `cells.electric-grid-deployment.policy_flag` = Deployment policy below the middle | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate; deployment-need confidence high

**s6.30** Deployment needed faster than policy supports

- `cells.electric-grid-deployment.gap` = Deployment needed faster than policy supports | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate; deployment-need confidence high

**s6.31** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.32** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.33** Heavy protection or localization

- `cells.solar-components.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.34** Localizing what is not a domestic priority

- `cells.solar-components.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.35** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**s6.36** Watch

- `cells.solar-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**s6.37** Heavy local-content protection on the equipment of a Watch deployment

- `cells.solar-deployment.policy_flag` = Heavy local-content protection on the equipment of a Watch deployment | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**s6.38** Localizing what is not a domestic priority

- `cells.solar-deployment.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**s6.39** Grid metals refining

- `cells.electric-grid-refining.name` = Grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.40** Watch

- `cells.electric-grid-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.41** Heavy protection or localization

- `cells.electric-grid-refining.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.42** Localizing what is not a domestic priority

- `cells.electric-grid-refining.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.44** First, gas and oil supply are Secure but rank low among the country's recorded measures (19 and 14).

- `cells.gas-extraction.policy_priority` = 19 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate
- `cells.oil-extraction.policy_priority` = 14 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.48** In grid metals refining (96), the data cannot tell whether the measures target electrical steel, a Partner layer, or the wider basket.

- `cells.electric-grid-refining.protection` = 96 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s7.3** Gas and oil are Secure for resilience: they are essential and imported, and supply takes one to three years to rebuild.

- `cells.oil-extraction.reconstitution` = one to three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**s7.4** Size stocks to that time, sign long-term and capacity-reservation contracts with trusted suppliers such as the United States, Australia and Canada for gas and Canada, the United States and Norway for oil, and stress-test the loss of the largest supplier.

- `cells.gas-extraction.partners` = the United States, Australia and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak
- `cells.oil-extraction.partners` = Canada, the United States and Norway | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**s7.5** Partner evidence is weak, so test the terms.

- `cells.gas-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**s7.8** Add trusted separation, metal and magnet capacity with allies such as Japan, Australia and the United States through co-investment and offtake, and at home only where announced projects can qualify.

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.10** No developing partner qualifies in this step; Malaysia and Thailand host new capacity on commercial terms.

- `dev.by_step.electric-vehicles-refining.hosts` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.14** Source cells from producers such as Poland, the United States and Hungary through offtake and co-investment, including with the battery compact's maker, South Korea.

- `cells.batteries-assembly.partners` = Poland, the United States and Hungary | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high; partner evidence strong
- `aib.compacts.SC-BATTERIES.core_makers` = South Korea | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s7.18** Meet that need through co-investment and joint ventures that transfer capability with the nuclear compact's makers, the United States, Sweden and France, rather than a domestic build alone.

- `aib.compacts.SC-NUCLEAR.core_makers` = the United States, Sweden and France | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**s7.22** Start with Malaysia in battery cells and packs, through development finance, guarantees and offtake.

- `dev.rows.r1.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.23** Prepare Viet Nam in rare-earth and wind materials refining over a longer horizon, with project preparation, infrastructure and skills.

- `dev.rows.r2.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.27** Deploy faster through infrastructure, permitting and procurement, buying from trusted suppliers such as the wind compact's makers, India, Germany and Denmark.

- `aib.compacts.SC-WIND.core_makers` = India, Germany and Denmark | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.2** Every number comes from the Country page data of the Allied Industrial Base Explorer (the data release of 26 November 2026) and from the Allied Industrial Base analysis.

- `meta.as_of_label` = 26 November 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.5** at deployment, Accelerate where deployment need reaches 60;

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.7** Preserve where Competitive Position is in the top quarter (75) and either Strategic Industrial Value (bar 60) or supply-chain need (bar 67) is high;

- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.need` = 67 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.17** 3 steps are review cases.

- `counts.review` = 3 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.21** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.22** Watch

- `cells.green-hydrogen-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.23** A single indicator of low confidence

- `cells.green-hydrogen-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.24** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.25** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.26** A single indicator of low confidence

- `cells.oil-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.27** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.28** Partner

- `cells.oil-recovery.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.29** Low confidence

- `cells.oil-recovery.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.31** The AIB status "allied supply covers the need" for rare-earth and battery-metal refining describes the allied analysis, not ample trusted supply: trusted suppliers hold 4% of world supply.

- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.33** 18 steps lack the data for a call, including most of geothermal, green hydrogen manufacturing and most recycling.

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.34** Recorded policy ranks electrolyser stacks (96) and systems (98) near the top of the country's own measures, a disagreement this brief does not settle.

- `cells.green-hydrogen-components.policy_priority` = 96 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low
- `cells.green-hydrogen-assembly.policy_priority` = 98 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.49** Battery cells and packs

- `dev.by_step.batteries-assembly.name` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.50** Partner, high need, allied supply falls short and new allied capacity needed

- `dev.by_step.batteries-assembly.reason` = Partner, high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.51** Malaysia, Indonesia, Mexico, India and Turkiye

- `dev.by_step.batteries-assembly.development` = Malaysia, Indonesia, Mexico, India and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.52** Viet Nam

- `dev.by_step.batteries-assembly.commercial` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.53** Malaysia, Indonesia, India, Turkiye and Viet Nam

- `dev.by_step.batteries-assembly.aid` = Malaysia, Indonesia, India, Turkiye and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.54** Rare-earth and wind materials refining

- `dev.by_step.wind-refining.name` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.55** Partner, high need, allied supply falls short and new allied capacity needed

- `dev.by_step.wind-refining.reason` = Partner, high need, allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.56** Viet Nam and India

- `dev.by_step.wind-refining.development` = Viet Nam and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.57** South Africa

- `dev.by_step.wind-refining.review` = South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.58** Viet Nam, India and South Africa

- `dev.by_step.wind-refining.aid` = Viet Nam, India and South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.59** Battery materials refining

- `dev.by_step.batteries-refining.name` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.60** New allied capacity needed

- `dev.by_step.batteries-refining.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.61** the Philippines, Madagascar, India, Thailand, South Africa, Turkiye and Papua New Guinea

- `dev.by_step.batteries-refining.development` = the Philippines, Madagascar, India, Thailand, South Africa, Turkiye and Papua New Guinea | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.62** Indonesia, Argentina, the Democratic Republic of Congo and Malaysia

- `dev.by_step.batteries-refining.commercial` = Indonesia, Argentina, the Democratic Republic of Congo and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.63** Mozambique, Tanzania and Colombia

- `dev.by_step.batteries-refining.review` = Mozambique, Tanzania and Colombia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.64** India, Thailand, South Africa, Turkiye, Indonesia, Malaysia and Tanzania

- `dev.by_step.batteries-refining.aid` = India, Thailand, South Africa, Turkiye, Indonesia, Malaysia and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.65** EV and charger manufacturing

- `dev.by_step.electric-vehicles-assembly.name` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.66** New allied capacity needed

- `dev.by_step.electric-vehicles-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.67** Viet Nam, Morocco, Malaysia and Indonesia

- `dev.by_step.electric-vehicles-assembly.development` = Viet Nam, Morocco, Malaysia and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.68** Mexico, Turkiye, India and Thailand

- `dev.by_step.electric-vehicles-assembly.commercial` = Mexico, Turkiye, India and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.69** Viet Nam, Malaysia, Indonesia, Turkiye, India and Thailand

- `dev.by_step.electric-vehicles-assembly.aid` = Viet Nam, Malaysia, Indonesia, Turkiye, India and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.70** EV drivetrain components

- `dev.by_step.electric-vehicles-components.name` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.71** New allied capacity needed

- `dev.by_step.electric-vehicles-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.72** Indonesia, Thailand, Turkiye, Serbia, Ukraine, Honduras and Morocco

- `dev.by_step.electric-vehicles-components.development` = Indonesia, Thailand, Turkiye, Serbia, Ukraine, Honduras and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.73** India, Malaysia, the Philippines and Mexico

- `dev.by_step.electric-vehicles-components.commercial` = India, Malaysia, the Philippines and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.74** Viet Nam

- `dev.by_step.electric-vehicles-components.review` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.75** Indonesia, Thailand, Turkiye, India, Malaysia and Viet Nam

- `dev.by_step.electric-vehicles-components.aid` = Indonesia, Thailand, Turkiye, India, Malaysia and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.76** Wind turbines

- `dev.by_step.wind-assembly.name` = Wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.77** New allied capacity needed

- `dev.by_step.wind-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.78** Viet Nam, Malaysia, Thailand, Indonesia and Morocco

- `dev.by_step.wind-assembly.development` = Viet Nam, Malaysia, Thailand, Indonesia and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.79** India, Turkiye and Mexico

- `dev.by_step.wind-assembly.commercial` = India, Turkiye and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.80** Brazil

- `dev.by_step.wind-assembly.review` = Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.81** Viet Nam, Malaysia, Thailand, Indonesia, India and Turkiye

- `dev.by_step.wind-assembly.aid` = Viet Nam, Malaysia, Thailand, Indonesia, India and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.82** Battery components

- `dev.by_step.batteries-components.name` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.83** New allied capacity needed

- `dev.by_step.batteries-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.84** Viet Nam, Indonesia, Turkiye, Mexico, India, Malaysia and Morocco

- `dev.by_step.batteries-components.development` = Viet Nam, Indonesia, Turkiye, Mexico, India, Malaysia and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.85** Viet Nam, Indonesia, Turkiye, India and Malaysia

- `dev.by_step.batteries-components.aid` = Viet Nam, Indonesia, Turkiye, India and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.86** Blades, towers and nacelle parts

- `dev.by_step.wind-components.name` = Blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.87** New allied capacity needed

- `dev.by_step.wind-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.88** Thailand, Serbia and Brazil

- `dev.by_step.wind-components.development` = Thailand, Serbia and Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.89** Mexico, Viet Nam, the Philippines, India, Turkiye and Malaysia

- `dev.by_step.wind-components.commercial` = Mexico, Viet Nam, the Philippines, India, Turkiye and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.90** Thailand, Viet Nam, India, Turkiye and Malaysia

- `dev.by_step.wind-components.aid` = Thailand, Viet Nam, India, Turkiye and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.91** Transformers, switchgear and cables

- `dev.by_step.electric-grid-assembly.name` = Transformers, switchgear and cables | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.92** New allied capacity needed

- `dev.by_step.electric-grid-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.93** Malaysia and Morocco

- `dev.by_step.electric-grid-assembly.development` = Malaysia and Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.94** Turkiye, Thailand and Mexico

- `dev.by_step.electric-grid-assembly.commercial` = Turkiye, Thailand and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.95** the Philippines, India and Viet Nam

- `dev.by_step.electric-grid-assembly.review` = the Philippines, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.96** Malaysia, Turkiye, Thailand, India and Viet Nam

- `dev.by_step.electric-grid-assembly.aid` = Malaysia, Turkiye, Thailand, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.97** Drilling rigs, tankers and oil equipment

- `dev.by_step.oil-assembly.name` = Drilling rigs, tankers and oil equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.98** New allied capacity needed

- `dev.by_step.oil-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.99** Turkiye, Malaysia, Indonesia and Mexico

- `dev.by_step.oil-assembly.development` = Turkiye, Malaysia, Indonesia and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.100** India and Viet Nam

- `dev.by_step.oil-assembly.review` = India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.101** Turkiye, Malaysia, Indonesia, India and Viet Nam

- `dev.by_step.oil-assembly.aid` = Turkiye, Malaysia, Indonesia, India and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.102** PV modules and inverters

- `dev.by_step.solar-assembly.name` = PV modules and inverters | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.103** New allied capacity needed

- `dev.by_step.solar-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.104** Morocco

- `dev.by_step.solar-assembly.development` = Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.105** India, Viet Nam, Thailand, Malaysia, Turkiye and Mexico

- `dev.by_step.solar-assembly.commercial` = India, Viet Nam, Thailand, Malaysia, Turkiye and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.106** Indonesia

- `dev.by_step.solar-assembly.review` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.107** India, Viet Nam, Thailand, Malaysia, Turkiye and Indonesia

- `dev.by_step.solar-assembly.aid` = India, Viet Nam, Thailand, Malaysia, Turkiye and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.108** Rare-earth and battery-metal refining

- `dev.by_step.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.109** Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed

- `dev.by_step.electric-vehicles-refining.reason` = Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.110** Malaysia and Thailand

- `dev.by_step.electric-vehicles-refining.commercial` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.111** Indonesia, the Democratic Republic of Congo, Mozambique, Tanzania, the Philippines, Mexico and Madagascar

- `dev.by_step.electric-vehicles-refining.review` = Indonesia, the Democratic Republic of Congo, Mozambique, Tanzania, the Philippines, Mexico and Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.112** Malaysia, Thailand, Indonesia and Tanzania

- `dev.by_step.electric-vehicles-refining.aid` = Malaysia, Thailand, Indonesia and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.113** Coal-plant equipment

- `dev.by_step.coal-assembly.name` = Coal-plant equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.114** Partner and high need

- `dev.by_step.coal-assembly.reason` = Partner and high need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.115** India

- `dev.by_step.coal-assembly.commercial` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.116** Malaysia, Turkiye and Viet Nam

- `dev.by_step.coal-assembly.review` = Malaysia, Turkiye and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.117** India, Malaysia, Turkiye and Viet Nam

- `dev.by_step.coal-assembly.aid` = India, Malaysia, Turkiye and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.118** Reactors and steam generators

- `dev.by_step.nuclear-assembly.name` = Reactors and steam generators | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.119** High need

- `dev.by_step.nuclear-assembly.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.120** India, Turkiye, Malaysia and Viet Nam

- `dev.by_step.nuclear-assembly.review` = India, Turkiye, Malaysia and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.121** India, Turkiye, Malaysia and Viet Nam

- `dev.by_step.nuclear-assembly.aid` = India, Turkiye, Malaysia and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.122** Battery recycling

- `dev.by_step.batteries-recovery.name` = Battery recycling | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.123** High need

- `dev.by_step.batteries-recovery.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.124** India and Malaysia

- `dev.by_step.batteries-recovery.review` = India and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.125** Polysilicon refining

- `dev.by_step.solar-refining.name` = Polysilicon refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.126** High need

- `dev.by_step.solar-refining.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.127** India

- `dev.by_step.solar-refining.review` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.128** India

- `dev.by_step.solar-refining.aid` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.129** Grid components

- `dev.by_step.electric-grid-components.name` = Grid components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.130** New allied capacity needed

- `dev.by_step.electric-grid-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.131** India

- `dev.by_step.electric-grid-components.review` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.132** India

- `dev.by_step.electric-grid-components.aid` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.143** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.144** Extraction

- `cells.batteries-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.145** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.146** Medium

- `cells.batteries-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.147** 88

- `cells.batteries-extraction.need` = 88 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.148** 57

- `cells.batteries-extraction.siv` = 57 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.149** 0

- `cells.batteries-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.150** strong

- `cells.batteries-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.151** broadly aligned

- `cells.batteries-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.152** Battery materials refining

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.153** Refining

- `cells.batteries-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.154** Catch Up

- `cells.batteries-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.155** Medium

- `cells.batteries-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.156** 61

- `cells.batteries-refining.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.157** 78

- `cells.batteries-refining.siv` = 78 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.158** 0

- `cells.batteries-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.159** strong

- `cells.batteries-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.160** needs more attention

- `cells.batteries-refining.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.161** Battery components

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.162** Components

- `cells.batteries-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.163** Catch Up

- `cells.batteries-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.164** Medium

- `cells.batteries-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.165** 67

- `cells.batteries-components.need` = 67 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.166** 79

- `cells.batteries-components.siv` = 79 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.167** 61

- `cells.batteries-components.cp` = 61 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.168** moderate

- `cells.batteries-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.169** mixed signals

- `cells.batteries-components.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.170** Battery cells and packs

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.171** Assembly

- `cells.batteries-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.172** Partner

- `cells.batteries-assembly.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.173** Mixed High/Medium/Low

- `cells.batteries-assembly.yard` = Mixed High/Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.174** 83

- `cells.batteries-assembly.need` = 83 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.175** 97

- `cells.batteries-assembly.siv` = 97 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.176** 58

- `cells.batteries-assembly.cp` = 58 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.177** moderate

- `cells.batteries-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.178** broadly aligned

- `cells.batteries-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.179** Battery storage

- `cells.batteries-deployment.name` = Battery storage | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.180** Deployment

- `cells.batteries-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.181** Accelerate

- `cells.batteries-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.182** Medium

- `cells.batteries-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.183** 69

- `cells.batteries-deployment.dn` = 69 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.184** strong

- `cells.batteries-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.185** broadly aligned

- `cells.batteries-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.186** Battery recycling

- `cells.batteries-recovery.name` = Battery recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.187** Recovery

- `cells.batteries-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.188** Catch Up

- `cells.batteries-recovery.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.189** Medium

- `cells.batteries-recovery.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.190** 96

- `cells.batteries-recovery.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence moderate

**a.191** 82

- `cells.batteries-recovery.siv` = 82 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence moderate

**a.192** mixed signals

- `cells.batteries-recovery.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate

**a.193** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.194** Extraction

- `cells.coal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.195** Watch

- `cells.coal-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.196** Low

- `cells.coal-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.197** 59

- `cells.coal-extraction.need` = 59 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.198** 4

- `cells.coal-extraction.siv` = 4 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.199** 8

- `cells.coal-extraction.cp` = 8 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.200** strong

- `cells.coal-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.201** broadly aligned

- `cells.coal-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.202** Coke and coal processing

- `cells.coal-refining.name` = Coke and coal processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.203** Refining

- `cells.coal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.204** Watch

- `cells.coal-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.205** Low

- `cells.coal-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.206** 88

- `cells.coal-refining.need` = 88 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.207** 9

- `cells.coal-refining.siv` = 9 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.208** 11

- `cells.coal-refining.cp` = 11 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.209** strong

- `cells.coal-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.210** broadly aligned

- `cells.coal-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.211** Coal-plant components

- `cells.coal-components.name` = Coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.212** Components

- `cells.coal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.213** Watch

- `cells.coal-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.214** Low

- `cells.coal-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.215** 66

- `cells.coal-components.need` = 66 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.216** 43

- `cells.coal-components.siv` = 43 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.217** 47

- `cells.coal-components.cp` = 47 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.218** moderate

- `cells.coal-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.219** mixed signals

- `cells.coal-components.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.220** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.221** Assembly

- `cells.coal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.222** Partner

- `cells.coal-assembly.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.223** Medium

- `cells.coal-assembly.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.224** 75

- `cells.coal-assembly.need` = 75 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.225** 43

- `cells.coal-assembly.siv` = 43 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.226** 32

- `cells.coal-assembly.cp` = 32 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.227** moderate

- `cells.coal-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.228** broadly aligned

- `cells.coal-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.229** Coal power and use

- `cells.coal-deployment.name` = Coal power and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.230** Deployment

- `cells.coal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.231** Watch

- `cells.coal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.232** Low

- `cells.coal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.233** 58

- `cells.coal-deployment.dn` = 58 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.234** moderate

- `cells.coal-deployment.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.235** broadly aligned

- `cells.coal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate; deployment-need confidence high

**a.236** Coal recovery

- `cells.coal-recovery.name` = Coal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.237** Recovery

- `cells.coal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.238** Not assessed

- `cells.coal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.239** 1

- `cells.coal-recovery.siv` = 1 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.240** not enough policy evidence

- `cells.coal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.241** Copper and bauxite mining

- `cells.electric-grid-extraction.name` = Copper and bauxite mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.242** Extraction

- `cells.electric-grid-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.243** Watch

- `cells.electric-grid-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.244** Low

- `cells.electric-grid-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.245** 95

- `cells.electric-grid-extraction.need` = 95 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.246** 41

- `cells.electric-grid-extraction.siv` = 41 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.247** 0

- `cells.electric-grid-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.248** strong

- `cells.electric-grid-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.249** broadly aligned

- `cells.electric-grid-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.250** Grid metals refining

- `cells.electric-grid-refining.name` = Grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.251** Refining

- `cells.electric-grid-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.252** Watch

- `cells.electric-grid-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.253** Mixed Medium/Low

- `cells.electric-grid-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.254** 73

- `cells.electric-grid-refining.need` = 73 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.255** 53

- `cells.electric-grid-refining.siv` = 53 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.256** 0

- `cells.electric-grid-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.257** strong

- `cells.electric-grid-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.258** too much intervention

- `cells.electric-grid-refining.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.259** Grid components

- `cells.electric-grid-components.name` = Grid components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.260** Components

- `cells.electric-grid-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.261** Watch

- `cells.electric-grid-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.262** Low

- `cells.electric-grid-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.263** 23

- `cells.electric-grid-components.need` = 23 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.264** 47

- `cells.electric-grid-components.siv` = 47 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.265** 64

- `cells.electric-grid-components.cp` = 64 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.266** moderate

- `cells.electric-grid-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.267** too much intervention

- `cells.electric-grid-components.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.268** Transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = Transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.269** Assembly

- `cells.electric-grid-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.270** Catch Up

- `cells.electric-grid-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.271** Mixed Medium/Low

- `cells.electric-grid-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.272** 23

- `cells.electric-grid-assembly.need` = 23 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.273** 61

- `cells.electric-grid-assembly.siv` = 61 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.274** 58

- `cells.electric-grid-assembly.cp` = 58 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.275** moderate

- `cells.electric-grid-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.276** broadly aligned

- `cells.electric-grid-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.277** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.278** Deployment

- `cells.electric-grid-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.279** Accelerate

- `cells.electric-grid-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.280** Medium

- `cells.electric-grid-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate; deployment-need confidence high

**a.281** 64

- `cells.electric-grid-deployment.dn` = 64 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence moderate; deployment-need confidence high

**a.282** deployment policy lags

- `cells.electric-grid-deployment.alignment` = deployment policy lags | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate; deployment-need confidence high

**a.283** Grid-equipment recycling

- `cells.electric-grid-recovery.name` = Grid-equipment recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.284** Recovery

- `cells.electric-grid-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.285** Not assessed

- `cells.electric-grid-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.286** 14

- `cells.electric-grid-recovery.siv` = 14 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.287** not enough policy evidence

- `cells.electric-grid-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.288** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.289** Extraction

- `cells.electric-vehicles-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.290** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.291** Medium

- `cells.electric-vehicles-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.292** 88

- `cells.electric-vehicles-extraction.need` = 88 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.293** 59

- `cells.electric-vehicles-extraction.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.294** 0

- `cells.electric-vehicles-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.295** strong

- `cells.electric-vehicles-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.296** wrong tool for the problem

- `cells.electric-vehicles-extraction.alignment` = wrong tool for the problem | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.297** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.298** Refining

- `cells.electric-vehicles-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.299** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.300** High

- `cells.electric-vehicles-refining.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.301** 87

- `cells.electric-vehicles-refining.need` = 87 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.302** 89

- `cells.electric-vehicles-refining.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.303** 0

- `cells.electric-vehicles-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.304** strong

- `cells.electric-vehicles-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.305** broadly aligned

- `cells.electric-vehicles-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.306** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.307** Components

- `cells.electric-vehicles-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.308** Catch Up

- `cells.electric-vehicles-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.309** Mixed High/Medium

- `cells.electric-vehicles-components.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.310** 50

- `cells.electric-vehicles-components.need` = 50 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.311** 95

- `cells.electric-vehicles-components.siv` = 95 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.312** 74

- `cells.electric-vehicles-components.cp` = 74 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.313** moderate

- `cells.electric-vehicles-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.314** broadly aligned

- `cells.electric-vehicles-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.315** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.316** Assembly

- `cells.electric-vehicles-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.317** Catch Up

- `cells.electric-vehicles-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.318** Mixed Medium/Low

- `cells.electric-vehicles-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.319** 54

- `cells.electric-vehicles-assembly.need` = 54 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.320** 95

- `cells.electric-vehicles-assembly.siv` = 95 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.321** 65

- `cells.electric-vehicles-assembly.cp` = 65 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.322** moderate

- `cells.electric-vehicles-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.323** broadly aligned

- `cells.electric-vehicles-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.324** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.325** Deployment

- `cells.electric-vehicles-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.326** Watch

- `cells.electric-vehicles-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.327** Low

- `cells.electric-vehicles-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.328** 59

- `cells.electric-vehicles-deployment.dn` = 59 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.329** strong

- `cells.electric-vehicles-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.330** broadly aligned

- `cells.electric-vehicles-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.331** EV recycling

- `cells.electric-vehicles-recovery.name` = EV recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.332** Recovery

- `cells.electric-vehicles-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.333** Not assessed

- `cells.electric-vehicles-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.334** 42

- `cells.electric-vehicles-recovery.siv` = 42 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.335** not enough policy evidence

- `cells.electric-vehicles-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.336** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.337** Extraction

- `cells.gas-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.338** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.339** High

- `cells.gas-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.340** 92

- `cells.gas-extraction.need` = 92 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.341** 17

- `cells.gas-extraction.siv` = 17 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.342** 32

- `cells.gas-extraction.cp` = 32 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.343** strong

- `cells.gas-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.344** needs more attention

- `cells.gas-extraction.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.345** Gas liquids processing

- `cells.gas-refining.name` = Gas liquids processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.346** Refining

- `cells.gas-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.347** Watch

- `cells.gas-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.348** Low

- `cells.gas-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.349** 47

- `cells.gas-refining.need` = 47 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.350** 17

- `cells.gas-refining.siv` = 17 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.351** 26

- `cells.gas-refining.cp` = 26 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.352** strong

- `cells.gas-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.353** broadly aligned

- `cells.gas-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.354** Gas-turbine and drilling components

- `cells.gas-components.name` = Gas-turbine and drilling components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.355** Components

- `cells.gas-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.356** Watch

- `cells.gas-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.357** Low

- `cells.gas-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.358** 36

- `cells.gas-components.need` = 36 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.359** 56

- `cells.gas-components.siv` = 56 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.360** 38

- `cells.gas-components.cp` = 38 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.361** moderate

- `cells.gas-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.362** broadly aligned

- `cells.gas-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.363** Gas turbines and LNG plant

- `cells.gas-assembly.name` = Gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.364** Assembly

- `cells.gas-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.365** Watch

- `cells.gas-assembly.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.366** Low

- `cells.gas-assembly.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.367** 14

- `cells.gas-assembly.need` = 14 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.368** 56

- `cells.gas-assembly.siv` = 56 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.369** 29

- `cells.gas-assembly.cp` = 29 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.370** moderate

- `cells.gas-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.371** not enough policy evidence

- `cells.gas-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.372** Gas networks and use

- `cells.gas-deployment.name` = Gas networks and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.373** Deployment

- `cells.gas-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.374** Watch

- `cells.gas-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.375** Low

- `cells.gas-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.376** 41

- `cells.gas-deployment.dn` = 41 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.377** strong

- `cells.gas-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.378** too much intervention

- `cells.gas-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.379** Gas recovery

- `cells.gas-recovery.name` = Gas recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.380** Recovery

- `cells.gas-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.381** Not assessed

- `cells.gas-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.382** 10

- `cells.gas-recovery.siv` = 10 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.383** not enough policy evidence

- `cells.gas-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.384** Geothermal resource development

- `cells.geothermal-extraction.name` = Geothermal resource development | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.385** Extraction

- `cells.geothermal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.386** Not assessed

- `cells.geothermal-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.387** 20

- `cells.geothermal-extraction.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.388** not enough policy evidence

- `cells.geothermal-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.389** Geothermal refining

- `cells.geothermal-refining.name` = Geothermal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.390** Refining

- `cells.geothermal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.391** Not assessed

- `cells.geothermal-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.392** 20

- `cells.geothermal-refining.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.393** not enough policy evidence

- `cells.geothermal-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.394** Geothermal plant components

- `cells.geothermal-components.name` = Geothermal plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.395** Components

- `cells.geothermal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.396** Not assessed

- `cells.geothermal-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.397** 69

- `cells.geothermal-components.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.398** 92

- `cells.geothermal-components.cp` = 92 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.399** none

- `cells.geothermal-components.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.400** not enough policy evidence

- `cells.geothermal-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.401** Geothermal power-plant equipment

- `cells.geothermal-assembly.name` = Geothermal power-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.402** Assembly

- `cells.geothermal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.403** Not assessed

- `cells.geothermal-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.404** 69

- `cells.geothermal-assembly.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.405** 92

- `cells.geothermal-assembly.cp` = 92 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.406** none

- `cells.geothermal-assembly.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.407** not enough policy evidence

- `cells.geothermal-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.408** Geothermal power and heat

- `cells.geothermal-deployment.name` = Geothermal power and heat | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.409** Deployment

- `cells.geothermal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.410** Watch

- `cells.geothermal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.411** Low

- `cells.geothermal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.412** 44

- `cells.geothermal-deployment.dn` = 44 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.413** strong

- `cells.geothermal-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.414** broadly aligned

- `cells.geothermal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.415** Geothermal recovery

- `cells.geothermal-recovery.name` = Geothermal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.416** Recovery

- `cells.geothermal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.417** Not assessed

- `cells.geothermal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.418** 20

- `cells.geothermal-recovery.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.419** not enough policy evidence

- `cells.geothermal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.420** Electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = Electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.421** Extraction

- `cells.green-hydrogen-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.422** Not assessed

- `cells.green-hydrogen-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.423** 35

- `cells.green-hydrogen-extraction.siv` = 35 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.424** 0

- `cells.green-hydrogen-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.425** strong

- `cells.green-hydrogen-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.426** not enough policy evidence

- `cells.green-hydrogen-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.427** PGM and catalyst refining

- `cells.green-hydrogen-refining.name` = PGM and catalyst refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.428** Refining

- `cells.green-hydrogen-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.429** Not assessed

- `cells.green-hydrogen-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.430** 63

- `cells.green-hydrogen-refining.siv` = 63 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.431** 43

- `cells.green-hydrogen-refining.cp` = 43 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.432** strong

- `cells.green-hydrogen-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.433** not enough policy evidence

- `cells.green-hydrogen-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.434** Electrolyser stacks

- `cells.green-hydrogen-components.name` = Electrolyser stacks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.435** Components

- `cells.green-hydrogen-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.436** Not assessed

- `cells.green-hydrogen-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.437** 99

- `cells.green-hydrogen-components.siv` = 99 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.438** 89

- `cells.green-hydrogen-components.cp` = 89 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.439** none

- `cells.green-hydrogen-components.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.440** not enough policy evidence

- `cells.green-hydrogen-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.441** Electrolyser systems

- `cells.green-hydrogen-assembly.name` = Electrolyser systems | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.442** Assembly

- `cells.green-hydrogen-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.443** Not assessed

- `cells.green-hydrogen-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.444** 90

- `cells.green-hydrogen-assembly.siv` = 90 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence none; choice confidence low

**a.445** 89

- `cells.green-hydrogen-assembly.cp` = 89 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence none; choice confidence low

**a.446** none

- `cells.green-hydrogen-assembly.evidence` = none | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence none; choice confidence low

**a.447** not enough policy evidence

- `cells.green-hydrogen-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence none; choice confidence low

**a.448** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.449** Deployment

- `cells.green-hydrogen-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.450** Watch

- `cells.green-hydrogen-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.451** Low

- `cells.green-hydrogen-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.452** 53

- `cells.green-hydrogen-deployment.dn` = 53 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence low

**a.453** broadly aligned

- `cells.green-hydrogen-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence low

**a.454** Stack and catalyst recycling

- `cells.green-hydrogen-recovery.name` = Stack and catalyst recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.455** Recovery

- `cells.green-hydrogen-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.456** Not assessed

- `cells.green-hydrogen-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.457** 49

- `cells.green-hydrogen-recovery.siv` = 49 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.458** not enough policy evidence

- `cells.green-hydrogen-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.459** Uranium mining

- `cells.nuclear-extraction.name` = Uranium mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.460** Extraction

- `cells.nuclear-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.461** Watch

- `cells.nuclear-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.462** Low

- `cells.nuclear-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.463** 97

- `cells.nuclear-extraction.need` = 97 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.464** 30

- `cells.nuclear-extraction.siv` = 30 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.465** 0

- `cells.nuclear-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.466** strong

- `cells.nuclear-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.467** not enough policy evidence

- `cells.nuclear-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.468** Uranium conversion and enrichment

- `cells.nuclear-refining.name` = Uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.469** Refining

- `cells.nuclear-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.470** Catch Up

- `cells.nuclear-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.471** Medium

- `cells.nuclear-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.472** 61

- `cells.nuclear-refining.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.473** 71

- `cells.nuclear-refining.siv` = 71 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.474** 25

- `cells.nuclear-refining.cp` = 25 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.475** strong

- `cells.nuclear-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.476** mixed signals

- `cells.nuclear-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.477** Nuclear fuel and reactor parts

- `cells.nuclear-components.name` = Nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.478** Components

- `cells.nuclear-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.479** Catch Up

- `cells.nuclear-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.480** Medium

- `cells.nuclear-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.481** 61

- `cells.nuclear-components.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.482** 65

- `cells.nuclear-components.siv` = 65 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.483** 66

- `cells.nuclear-components.cp` = 66 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.484** moderate

- `cells.nuclear-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.485** broadly aligned

- `cells.nuclear-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.486** Reactors and steam generators

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.487** Assembly

- `cells.nuclear-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.488** Catch Up

- `cells.nuclear-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.489** Medium

- `cells.nuclear-assembly.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.490** 79

- `cells.nuclear-assembly.need` = 79 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.491** 95

- `cells.nuclear-assembly.siv` = 95 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.492** 71

- `cells.nuclear-assembly.cp` = 71 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.493** moderate

- `cells.nuclear-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.494** broadly aligned

- `cells.nuclear-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.495** Nuclear power

- `cells.nuclear-deployment.name` = Nuclear power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.496** Deployment

- `cells.nuclear-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.497** Watch

- `cells.nuclear-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.498** Low

- `cells.nuclear-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.499** 31

- `cells.nuclear-deployment.dn` = 31 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.500** strong

- `cells.nuclear-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.501** broadly aligned

- `cells.nuclear-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.502** Spent-fuel reprocessing

- `cells.nuclear-recovery.name` = Spent-fuel reprocessing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.503** Recovery

- `cells.nuclear-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.504** Not assessed

- `cells.nuclear-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.505** 71

- `cells.nuclear-recovery.siv` = 71 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.506** not enough policy evidence

- `cells.nuclear-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.507** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.508** Extraction

- `cells.oil-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.509** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.510** High

- `cells.oil-extraction.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.511** 87

- `cells.oil-extraction.need` = 87 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.512** 23

- `cells.oil-extraction.siv` = 23 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.513** 25

- `cells.oil-extraction.cp` = 25 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.514** strong

- `cells.oil-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.515** needs more attention

- `cells.oil-extraction.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.516** Oil refining

- `cells.oil-refining.name` = Oil refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.517** Refining

- `cells.oil-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.518** Watch

- `cells.oil-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.519** Low

- `cells.oil-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.520** 29

- `cells.oil-refining.need` = 29 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.521** 45

- `cells.oil-refining.siv` = 45 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.522** 17

- `cells.oil-refining.cp` = 17 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.523** strong

- `cells.oil-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.524** too much intervention

- `cells.oil-refining.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.525** Drilling and pump components

- `cells.oil-components.name` = Drilling and pump components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.526** Components

- `cells.oil-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.527** Watch

- `cells.oil-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.528** Low

- `cells.oil-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.529** 47

- `cells.oil-components.need` = 47 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.530** 30

- `cells.oil-components.siv` = 30 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.531** 25

- `cells.oil-components.cp` = 25 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.532** moderate

- `cells.oil-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.533** broadly aligned

- `cells.oil-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.534** Drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = Drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.535** Assembly

- `cells.oil-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.536** Watch

- `cells.oil-assembly.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.537** Low

- `cells.oil-assembly.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.538** 13

- `cells.oil-assembly.need` = 13 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.539** 58

- `cells.oil-assembly.siv` = 58 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.540** 16

- `cells.oil-assembly.cp` = 16 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.541** moderate

- `cells.oil-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.542** not enough policy evidence

- `cells.oil-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.543** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.544** Deployment

- `cells.oil-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.545** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.546** Low

- `cells.oil-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.547** 49

- `cells.oil-deployment.dn` = 49 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.548** strong

- `cells.oil-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.549** too much intervention

- `cells.oil-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.550** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.551** Recovery

- `cells.oil-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.552** Partner

- `cells.oil-recovery.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.553** Medium

- `cells.oil-recovery.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.554** 87

- `cells.oil-recovery.need` = 87 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence low

**a.555** 12

- `cells.oil-recovery.siv` = 12 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.556** broadly aligned

- `cells.oil-recovery.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.557** PV mineral mining

- `cells.solar-extraction.name` = PV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.558** Extraction

- `cells.solar-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.559** Watch

- `cells.solar-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.560** Low

- `cells.solar-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.561** 96

- `cells.solar-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.562** 29

- `cells.solar-extraction.siv` = 29 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.563** 0

- `cells.solar-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.564** strong

- `cells.solar-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.565** broadly aligned

- `cells.solar-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.566** Polysilicon refining

- `cells.solar-refining.name` = Polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.567** Refining

- `cells.solar-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.568** Watch

- `cells.solar-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.569** Mixed Medium/Low

- `cells.solar-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.570** 89

- `cells.solar-refining.need` = 89 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.571** 56

- `cells.solar-refining.siv` = 56 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.572** 0

- `cells.solar-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.573** strong

- `cells.solar-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.574** broadly aligned

- `cells.solar-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.575** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.576** Components

- `cells.solar-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.577** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.578** Mixed Medium/Low

- `cells.solar-components.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.579** 26

- `cells.solar-components.need` = 26 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.580** 59

- `cells.solar-components.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.581** 26

- `cells.solar-components.cp` = 26 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.582** moderate

- `cells.solar-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.583** too much intervention

- `cells.solar-components.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.584** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.585** Assembly

- `cells.solar-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.586** Catch Up

- `cells.solar-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.587** Mixed High/Medium/Low

- `cells.solar-assembly.yard` = Mixed High/Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.588** 27

- `cells.solar-assembly.need` = 27 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.589** 81

- `cells.solar-assembly.siv` = 81 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.590** 19

- `cells.solar-assembly.cp` = 19 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.591** moderate

- `cells.solar-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.592** broadly aligned

- `cells.solar-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.593** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.594** Deployment

- `cells.solar-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.595** Watch

- `cells.solar-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.596** Low

- `cells.solar-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.597** 55

- `cells.solar-deployment.dn` = 55 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.598** strong

- `cells.solar-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.599** too much intervention

- `cells.solar-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence high

**a.600** PV module recycling

- `cells.solar-recovery.name` = PV module recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.601** Recovery

- `cells.solar-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.602** Not assessed

- `cells.solar-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.603** 44

- `cells.solar-recovery.siv` = 44 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.604** not enough policy evidence

- `cells.solar-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.605** Wind mineral mining

- `cells.wind-extraction.name` = Wind mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.606** Extraction

- `cells.wind-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.607** Not assessed

- `cells.wind-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.608** 51

- `cells.wind-extraction.siv` = 51 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.609** 0

- `cells.wind-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.610** strong

- `cells.wind-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.611** not enough policy evidence

- `cells.wind-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.612** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.613** Refining

- `cells.wind-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.614** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.615** Medium

- `cells.wind-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.616** 87

- `cells.wind-refining.need` = 87 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.617** 64

- `cells.wind-refining.siv` = 64 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.618** 0

- `cells.wind-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.619** strong

- `cells.wind-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.620** broadly aligned

- `cells.wind-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.621** Blades, towers and nacelle parts

- `cells.wind-components.name` = Blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.622** Components

- `cells.wind-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.623** Catch Up

- `cells.wind-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.624** Medium

- `cells.wind-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.625** 30

- `cells.wind-components.need` = 30 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.626** 89

- `cells.wind-components.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.627** 58

- `cells.wind-components.cp` = 58 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.628** moderate

- `cells.wind-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.629** broadly aligned

- `cells.wind-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.630** Wind turbines

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.631** Assembly

- `cells.wind-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.632** Catch Up

- `cells.wind-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.633** Mixed Medium/Low

- `cells.wind-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.634** 25

- `cells.wind-assembly.need` = 25 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.635** 89

- `cells.wind-assembly.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.636** 61

- `cells.wind-assembly.cp` = 61 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.637** moderate

- `cells.wind-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.638** mixed signals

- `cells.wind-assembly.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.639** Wind power

- `cells.wind-deployment.name` = Wind power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.640** Deployment

- `cells.wind-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.641** Accelerate

- `cells.wind-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.642** Medium

- `cells.wind-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.643** 66

- `cells.wind-deployment.dn` = 66 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.644** strong

- `cells.wind-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.645** deployment policy lags

- `cells.wind-deployment.alignment` = deployment policy lags | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.646** Wind-turbine recycling

- `cells.wind-recovery.name` = Wind-turbine recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.647** Recovery

- `cells.wind-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.648** Not assessed

- `cells.wind-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.649** 36

- `cells.wind-recovery.siv` = 36 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.650** not enough policy evidence

- `cells.wind-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.656** Anode materials (natural and synthetic graphite)

- `cells.batteries-refining.layers.bess-anode.name` = Anode materials (natural and synthetic graphite) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.657** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.658** Catch Up

- `cells.batteries-refining.layers.bess-anode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.659** Medium

- `cells.batteries-refining.layers.bess-anode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.660** Cathode active materials and precursors

- `cells.batteries-refining.layers.bess-cathode.name` = Cathode active materials and precursors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.661** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.662** Catch Up

- `cells.batteries-refining.layers.bess-cathode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.663** Medium

- `cells.batteries-refining.layers.bess-cathode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.664** Battery-grade lithium hydroxide and carbonate

- `cells.batteries-refining.layers.li-chemicals.name` = Battery-grade lithium hydroxide and carbonate | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.665** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.666** Catch Up

- `cells.batteries-refining.layers.li-chemicals.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.667** Medium

- `cells.batteries-refining.layers.li-chemicals.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.668** Battery cells (LFP and NMC)

- `cells.batteries-assembly.layers.bess-cells.name` = Battery cells (LFP and NMC) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.669** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.670** Partner

- `cells.batteries-assembly.layers.bess-cells.choice` = Partner | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.671** Medium

- `cells.batteries-assembly.layers.bess-cells.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.672** Communications gateway and cloud fleet management

- `cells.batteries-assembly.layers.bess-comms.name` = Communications gateway and cloud fleet management | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.673** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.674** Secure

- `cells.batteries-assembly.layers.bess-comms.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.675** High

- `cells.batteries-assembly.layers.bess-comms.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.676** Battery-management firmware, energy-management software and remote updates

- `cells.batteries-assembly.layers.bess-control.name` = Battery-management firmware, energy-management software and remote updates | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.677** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.678** Secure

- `cells.batteries-assembly.layers.bess-control.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.679** High

- `cells.batteries-assembly.layers.bess-control.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.680** Enclosures, racking, thermal management and balance of system

- `cells.batteries-assembly.layers.bess-enclosure.name` = Enclosures, racking, thermal management and balance of system | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.681** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.682** Watch

- `cells.batteries-assembly.layers.bess-enclosure.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.683** Low

- `cells.batteries-assembly.layers.bess-enclosure.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.684** Grain-oriented electrical steel

- `cells.electric-grid-refining.layers.goes.name` = Grain-oriented electrical steel | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.685** grid metals refining

- `cells.electric-grid-refining.name` = grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.686** Partner

- `cells.electric-grid-refining.layers.goes.choice` = Partner | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.687** Medium

- `cells.electric-grid-refining.layers.goes.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.688** Power conversion hardware (inverters and converters)

- `cells.electric-grid-assembly.layers.bess-pcs.name` = Power conversion hardware (inverters and converters) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.689** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.690** Catch Up

- `cells.electric-grid-assembly.layers.bess-pcs.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.691** Medium

- `cells.electric-grid-assembly.layers.bess-pcs.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.692** Smart meters and advanced metering head-end systems

- `cells.electric-grid-assembly.layers.grid-ami.name` = Smart meters and advanced metering head-end systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.693** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.694** Watch

- `cells.electric-grid-assembly.layers.grid-ami.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.695** Low

- `cells.electric-grid-assembly.layers.grid-ami.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.696** Cables and conductors

- `cells.electric-grid-assembly.layers.grid-cables.name` = Cables and conductors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.697** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.698** Watch

- `cells.electric-grid-assembly.layers.grid-cables.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.699** Low

- `cells.electric-grid-assembly.layers.grid-cables.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.700** Utility-scale inverter firmware and remote-access functions

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.name` = Utility-scale inverter firmware and remote-access functions | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.701** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.702** Watch

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.703** Low

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.704** Grid-control software (SCADA, EMS, DERMS)

- `cells.electric-grid-assembly.layers.grid-scada.name` = Grid-control software (SCADA, EMS, DERMS) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.705** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.706** Watch

- `cells.electric-grid-assembly.layers.grid-scada.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.707** Low

- `cells.electric-grid-assembly.layers.grid-scada.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.708** Large power transformers

- `cells.electric-grid-assembly.layers.grid-transformers.name` = Large power transformers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.709** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.710** Catch Up

- `cells.electric-grid-assembly.layers.grid-transformers.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.711** Medium

- `cells.electric-grid-assembly.layers.grid-transformers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.712** HVDC converter-station control and protection

- `cells.electric-grid-assembly.layers.hvdc-control.name` = HVDC converter-station control and protection | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.713** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.714** Catch Up

- `cells.electric-grid-assembly.layers.hvdc-control.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.715** Medium

- `cells.electric-grid-assembly.layers.hvdc-control.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.716** Electric drive units and motors

- `cells.electric-vehicles-components.layers.ev-motors.name` = Electric drive units and motors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.717** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.718** Catch Up

- `cells.electric-vehicles-components.layers.ev-motors.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.719** Medium

- `cells.electric-vehicles-components.layers.ev-motors.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.720** Sintered NdFeB permanent magnets (traction motors)

- `cells.electric-vehicles-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets (traction motors) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.721** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.722** Secure

- `cells.electric-vehicles-components.layers.ree-magnets.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.723** High

- `cells.electric-vehicles-components.layers.ree-magnets.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.724** Connected-vehicle telematics, over-the-air software and connectivity systems

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.name` = Connected-vehicle telematics, over-the-air software and connectivity systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.725** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.726** Watch

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.727** Low

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.728** Charging-network management software (charge-point back ends)

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.name` = Charging-network management software (charge-point back ends) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.729** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.730** Watch

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.731** Low

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.732** Pipeline SCADA, compressor-station and LNG-terminal control systems

- `cells.gas-assembly.layers.gas-ot-control.name` = Pipeline SCADA, compressor-station and LNG-terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.733** gas turbines and LNG plant

- `cells.gas-assembly.name` = gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.734** Watch

- `cells.gas-assembly.layers.gas-ot-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.735** Low

- `cells.gas-assembly.layers.gas-ot-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.736** Iridium and platinum for PEM electrolyser catalysts

- `cells.green-hydrogen-extraction.layers.h2-pgm.name` = Iridium and platinum for PEM electrolyser catalysts | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.737** electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.738** Not assessed

- `cells.green-hydrogen-extraction.layers.h2-pgm.choice` = Not assessed | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.739** HALEU enrichment

- `cells.nuclear-refining.layers.nuc-haleu.name` = HALEU enrichment | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.740** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.741** Catch Up

- `cells.nuclear-refining.layers.nuc-haleu.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.742** Medium

- `cells.nuclear-refining.layers.nuc-haleu.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.743** LEU enrichment services

- `cells.nuclear-refining.layers.nuc-leu.name` = LEU enrichment services | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.744** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.745** Catch Up

- `cells.nuclear-refining.layers.nuc-leu.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.746** Medium

- `cells.nuclear-refining.layers.nuc-leu.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.747** Fuel assemblies (design-specific fabrication)

- `cells.nuclear-components.layers.nuc-fuel-fab.name` = Fuel assemblies (design-specific fabrication) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.748** nuclear fuel and reactor parts

- `cells.nuclear-components.name` = nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.749** Catch Up

- `cells.nuclear-components.layers.nuc-fuel-fab.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.750** Medium

- `cells.nuclear-components.layers.nuc-fuel-fab.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.751** Reactor control and safety instrumentation

- `cells.nuclear-assembly.layers.nuc-reactor-controls.name` = Reactor control and safety instrumentation | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.752** reactors and steam generators

- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.753** Partner

- `cells.nuclear-assembly.layers.nuc-reactor-controls.choice` = Partner | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.754** Medium

- `cells.nuclear-assembly.layers.nuc-reactor-controls.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.755** Oil pipeline and terminal control systems

- `cells.oil-assembly.layers.oil-ot-control.name` = Oil pipeline and terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.756** drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.757** Watch

- `cells.oil-assembly.layers.oil-ot-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.758** Low

- `cells.oil-assembly.layers.oil-ot-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.759** Solar-grade polysilicon

- `cells.solar-refining.layers.polysilicon.name` = Solar-grade polysilicon | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.760** polysilicon refining

- `cells.solar-refining.name` = polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.761** Partner

- `cells.solar-refining.layers.polysilicon.choice` = Partner | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.762** Medium

- `cells.solar-refining.layers.polysilicon.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.763** Solar cells and wafers

- `cells.solar-components.layers.pv-cells-wafers.name` = Solar cells and wafers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.764** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.765** Catch Up

- `cells.solar-components.layers.pv-cells-wafers.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.766** Medium

- `cells.solar-components.layers.pv-cells-wafers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.767** Next-generation PV (perovskite and tandem cells)

- `cells.solar-components.layers.pv-nextgen.name` = Next-generation PV (perovskite and tandem cells) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.768** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.769** Catch Up

- `cells.solar-components.layers.pv-nextgen.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.770** Medium

- `cells.solar-components.layers.pv-nextgen.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.771** Distributed PV inverter firmware, communication dongles and vendor cloud

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.name` = Distributed PV inverter firmware, communication dongles and vendor cloud | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.772** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.773** Secure

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.774** High

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.775** Conventional crystalline-silicon modules

- `cells.solar-assembly.layers.pv-modules.name` = Conventional crystalline-silicon modules | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.776** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.777** Watch

- `cells.solar-assembly.layers.pv-modules.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.778** Low

- `cells.solar-assembly.layers.pv-modules.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.779** Passive racking and mounting

- `cells.solar-assembly.layers.pv-racking.name` = Passive racking and mounting | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.780** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.781** Watch

- `cells.solar-assembly.layers.pv-racking.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.782** Low

- `cells.solar-assembly.layers.pv-racking.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.783** Rare-earth separation and metal-making

- `cells.wind-refining.layers.ree-separation.name` = Rare-earth separation and metal-making | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.784** rare-earth and wind materials refining

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.785** Partner

- `cells.wind-refining.layers.ree-separation.choice` = Partner | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.786** Medium

- `cells.wind-refining.layers.ree-separation.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.787** Sintered NdFeB permanent magnets

- `cells.wind-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.788** blades, towers and nacelle parts

- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.789** Catch Up

- `cells.wind-components.layers.ree-magnets.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.790** Medium

- `cells.wind-components.layers.ree-magnets.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.791** Turbine controllers, OEM remote access and park SCADA

- `cells.wind-assembly.layers.wind-turbine-control.name` = Turbine controllers, OEM remote access and park SCADA | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.792** wind turbines

- `cells.wind-assembly.name` = wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.793** Watch

- `cells.wind-assembly.layers.wind-turbine-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.794** Low

- `cells.wind-assembly.layers.wind-turbine-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.801** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.802** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.803** Australia, Canada and the United States

- `cells.batteries-extraction.partners` = Australia, Canada and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.804** Allied supply covers the need

- `cells.batteries-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.805** moderate

- `cells.batteries-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.806** Battery cells and packs

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.807** Partner

- `cells.batteries-assembly.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.808** Poland, the United States and Hungary

- `cells.batteries-assembly.partners` = Poland, the United States and Hungary | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high; partner evidence strong

**a.809** Allied supply falls short

- `cells.batteries-assembly.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.810** strong

- `cells.batteries-assembly.partner_evidence` = strong | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; partner evidence strong

**a.811** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.812** Partner

- `cells.coal-assembly.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.813** Italy, Germany and Japan

- `cells.coal-assembly.partners` = Italy, Germany and Japan | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high; partner evidence weak

**a.814** Allied supply covers the need

- `cells.coal-assembly.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.815** weak

- `cells.coal-assembly.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; partner evidence weak

**a.816** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.817** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.818** Australia, Canada and the United States

- `cells.electric-vehicles-extraction.partners` = Australia, Canada and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.819** Allied supply covers the need

- `cells.electric-vehicles-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.820** moderate

- `cells.electric-vehicles-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.821** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.822** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.823** Japan, Australia and the United States

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.824** Allied supply covers the need

- `cells.electric-vehicles-refining.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.825** moderate

- `cells.electric-vehicles-refining.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.826** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.827** Secure

- `cells.gas-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.828** the United States, Australia and Canada

- `cells.gas-extraction.partners` = the United States, Australia and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.829** Allied supply covers the need

- `cells.gas-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.830** weak

- `cells.gas-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.831** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.832** Secure

- `cells.oil-extraction.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.833** Canada, the United States and Norway

- `cells.oil-extraction.partners` = Canada, the United States and Norway | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.834** Allied supply falls short

- `cells.oil-extraction.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.835** weak

- `cells.oil-extraction.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.836** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.837** Partner

- `cells.oil-recovery.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.838** No credible allied solution yet

- `cells.oil-recovery.partner_gap` = No credible allied solution yet | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: choice confidence low; partner evidence none

**a.839** none

- `cells.oil-recovery.partner_evidence` = none | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: choice confidence low; partner evidence none

**a.840** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.841** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.842** Germany, Spain and Canada

- `cells.wind-refining.partners` = Germany, Spain and Canada | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**a.843** Allied supply falls short

- `cells.wind-refining.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.844** strong

- `cells.wind-refining.partner_evidence` = strong | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence strong

**a.851** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.852** Bridge / coordination partner

- `aib.compacts.SC-BATTERIES.role` = Bridge / coordination partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.853** Demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-BATTERIES.functions` = Demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.854** the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany

- `aib.compacts.SC-BATTERIES.core` = the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.855** Wind & Hydropower Equipment

- `aib.compacts.SC-WIND.name` = Wind & Hydropower Equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.856** Demand anchor

- `aib.compacts.SC-WIND.role` = Demand anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.857** Demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-WIND.functions` = Demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.858** Turbines

- `aib.compacts.SC-WIND.lead_step` = Turbines | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.859** Spain, India, Germany, Denmark, the United States and the United Kingdom

- `aib.compacts.SC-WIND.core` = Spain, India, Germany, Denmark, the United States and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.860** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.861** Demand anchor

- `aib.compacts.SC-NUCLEAR.role` = Demand anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.862** Processing anchor and demand anchor

- `aib.compacts.SC-NUCLEAR.functions` = Processing anchor and demand anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.863** Reactors & enrichment machinery

- `aib.compacts.SC-NUCLEAR.lead_step` = Reactors & enrichment machinery | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.864** Australia, India, the United States, Sweden, France and the United Kingdom

- `aib.compacts.SC-NUCLEAR.core` = Australia, India, the United States, Sweden, France and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.865** Grid Equipment & Conductors

- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.866** Demand anchor

- `aib.compacts.SC-GRID.role` = Demand anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.867** Demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-GRID.functions` = Demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.868** Network equipment

- `aib.compacts.SC-GRID.lead_step` = Network equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.869** Chile, Peru, the Democratic Republic of Congo, Germany, India and the United States

- `aib.compacts.SC-GRID.core` = Chile, Peru, the Democratic Republic of Congo, Germany, India and the United States | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.870** Electric Vehicles

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.871** Demand anchor

- `aib.compacts.SC-EV.role` = Demand anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.872** Demand anchor, capacity-building host, bridge / coordination partner and diversification partner

- `aib.compacts.SC-EV.functions` = Demand anchor, capacity-building host, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.873** Electric vehicles

- `aib.compacts.SC-EV.lead_step` = Electric vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.874** Germany, the United States, Japan, France, Spain and the United Kingdom

- `aib.compacts.SC-EV.core` = Germany, the United States, Japan, France, Spain and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.875** Heat Pumps & Cooling

- `aib.compacts.SC-HEAT.name` = Heat Pumps & Cooling | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.876** Capacity-building host

- `aib.compacts.SC-HEAT.role` = Capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.877** Capacity-building host

- `aib.compacts.SC-HEAT.functions` = Capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.878** the United States, Thailand, Germany, Mexico, Japan and Italy

- `aib.compacts.SC-HEAT.core` = the United States, Thailand, Germany, Mexico, Japan and Italy | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.879** Industrial Electrification & Hydrogen

- `aib.compacts.SC-INDUSTRY.name` = Industrial Electrification & Hydrogen | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.880** Processing anchor

- `aib.compacts.SC-INDUSTRY.role` = Processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.881** Processing anchor and capacity-building host

- `aib.compacts.SC-INDUSTRY.functions` = Processing anchor and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.882** Catalyst metals

- `aib.compacts.SC-INDUSTRY.lead_step` = Catalyst metals | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.883** the United States, India, South Africa, Germany, Indonesia and Saudi Arabia

- `aib.compacts.SC-INDUSTRY.core` = the United States, India, South Africa, Germany, Indonesia and Saudi Arabia | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the trusted network (core members, partners and economies treated as allied), computed by the Explorer's authors | evidence: AIB model run

**a.888** 26 November 2026

- `meta.as_of_label` = 26 November 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.890** 48

- `counts.assessed` = 48 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.892** 18

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.894** 34

- `counts.layers_assessed` = 34 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.896** 28

- `counts.ev_high` = 28 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.898** 19

- `counts.ev_partial` = 19 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.900** 19

- `counts.ev_none` = 19 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
