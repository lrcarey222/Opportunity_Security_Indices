# Content trace: Canada and the Allied Industrial Base

Each claim of the brief with the dashboard field it comes from, the source datasets and the evidence quality it rests on.

**s1.1** Canada's distinctive role in the Allied Industrial Base is as a resource and processing anchor: it ranks first among allies on resource & processing positions, and 19 economies count it as their best-placed trusted partner somewhere in the chain.

- `aib.first_on` = resource & processing positions | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.recommended_economies` = 19 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s1.2** That role is allied before it is national: at home, uranium mining reads Watch, and battery and EV mineral mining, where Canada holds only 1% of world supply, read Partner.

- `cells.batteries-extraction.share` = 1% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s1.7** The rare-earth chain from refining to traction magnets, where China holds 94% of world supply and trusted suppliers only 4% (shares that battery materials refining repeats).

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s1.10** Preserve battery cells and packs.

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s1.14** Battery and EV minerals from trusted producers such as Australia and the United States, where the allied model finds supply covers the need; coal-plant equipment from allied makers; solar-grade polysilicon.

- `cells.batteries-extraction.partners` = Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s1.23** A corner flag marks a mixed step whose named parts sit behind a different fence: in EV drivetrain components, the traction magnets are Secure and the step Catch Up.

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s2.2** Secure is a narrow call for Canada: one whole step and one layer inside another step, which form one capability.

- `counts.secure` = one | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.secure_layers` = one | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `counts.high_capabilities` = one | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.9** The refining step also covers battery metals; Canada holds 1% of its world supply and <1% of world exports.

- `cells.electric-vehicles-refining.share` = 1% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.export_share` = <1% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.10** China holds 94% of world supply and trusted suppliers 4%; it provides 37% of Canada's imports, and rebuilding takes more than three years.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.china_share` = 37% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `cells.electric-vehicles-refining.reconstitution` = more than three years | posture detail cells[] (need components and trade) | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**s2.12** New trusted separation, metal and magnet capacity with allies such as Japan, Australia and the United States, through co-investment and offtake, and at home where a foothold can be built.

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s2.14** The magnets of blades, towers and nacelle parts read Catch Up there but Secure in EV drivetrains; this brief treats both as the same chain.

- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s2.16** The hardware shares are modest (for example, 22% of PV module and inverter imports), and the data do not measure who supplies the software.

- `cells.solar-assembly.coercive_import_share` = 22% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.19** Most of its 15 Catch Up steps rest on small shares of world supply in mixed trade codes, so this brief builds on few.

- `counts.catch_up` = 15 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.21** Battery cells and packs is the only Preserve call that clears the evidence bar: a top-quarter position (87) on operating capacity, with high value and need.

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-assembly.cp` = 87 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.22** With 1% of world supply, keeping the position means expanding it.

- `cells.batteries-assembly.share` = 1% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.23** Oil production also reads Preserve but is flagged for review by the engine, a review case, although 14 economies have Canada as their best-placed trusted partner for it.

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate
- `cells.oil-extraction.review` = flagged for review by the engine | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate
- `aib.recommended_by.oil-extraction.n` = 14 | posture core cells.p1 (best-placed trusted partner of every economy) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: AIB model run

**s2.25** Battery components rests on three anchors, including committed investment, with a position of 66 and high need (71); China provides 76% of Canada's imports.

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.anchors` = three | posture detail cells[].pathway_basis, pathway_credible | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.cp` = 66 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.need` = 71 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.china_share` = 76% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.26** Battery materials refining has high confidence but a single anchor, 6% of world exports, and a position of 0: a trade position to build from, not proven refining capacity.

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.confidence` = high | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.export_share` = 6% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.batteries-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.27** The AIB's share of nickel intermediates and chemicals (10%) corroborates the nickel side, not lithium, cathode or anode materials.

- `aib.contributes.nickel-intermediates-and-chemicals.share` = 10% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s2.28** EV and charger manufacturing adds committed investment to small trade shares, from a low position (5).

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate
- `cells.electric-vehicles-assembly.cp` = 5 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s2.29** Gas turbines and LNG plant (47) and gas-turbine and drilling components (30) extend existing positions with high confidence.

- `cells.gas-assembly.name` = Gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.gas-assembly.cp` = 47 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.gas-components.name` = gas-turbine and drilling components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high
- `cells.gas-components.cp` = 30 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.30** In transformers, switchgear and cables, whose value just clears the bar, the case sits in large power transformers, power conversion and HVDC control.

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s2.31** EV drivetrain components starts from almost no position (1), so it is a build with allied partners.

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high
- `cells.electric-vehicles-components.cp` = 1 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.33** Uranium conversion and enrichment: a supply share (3%) that more likely reflects uranium compounds, concentrate included, than conversion capacity; the enrichment layers inherit it.

- `cells.nuclear-refining.share` = 3% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.35** Wind turbines and their parts: positions of 7 and 17 in trade codes dominated by cables, transformers and switchgear, with towers that include transmission masts.

- `cells.wind-assembly.cp` = 7 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.wind-components.cp` = 17 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s2.36** PV modules and inverters: moderate confidence, Watch modules, and trade codes that include chip-making machines.

- `cells.solar-assembly.confidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**s2.37** Battery recycling: an export share (19%) of battery waste, scrap and residues.

- `cells.batteries-recovery.export_share` = 19% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence moderate

**s2.38** Drilling rigs, tankers and oil equipment: a value at the bar (60) and moderate confidence.

- `cells.oil-assembly.siv` = 60 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate
- `cells.oil-assembly.confidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**s2.40** Battery and EV mineral mining have very high need (96), but their value is capped below the bar because it rests on market size alone.

- `cells.batteries-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.41** Source them deliberately from trusted producers such as Australia and the United States; allied supply covers the need.

- `cells.batteries-extraction.partners` = Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s2.42** Coal-plant hardware reads Partner on high need and low value: diversified contracts with allied makers such as Japan, Germany and Italy should suffice, on weak partner evidence.

- `cells.coal-components.partners` = Japan, Germany and Italy | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**s2.43** Solar-grade polysilicon is a Partner layer, and rare-earth and wind materials refining reads Partner on low confidence, a review case.

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low
- `cells.wind-refining.review` = low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**s2.45** Dashed lines mark the value bar (60) and the top quarter (75).

- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.56** Uranium conversion & enrichment

- `shared.uranium-conversion-and-enrichment.label` = Uranium conversion & enrichment | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.57** uranium mining (Watch) and uranium conversion and enrichment (Catch Up)

- `shared.uranium-conversion-and-enrichment.steps` = uranium mining (Watch) and uranium conversion and enrichment (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.58** The choices differ within the same technology with each step's own measures (uranium mining: need 61, value 54, position 85; uranium conversion and enrichment: need 61, value 88, position 35)

- `shared.uranium-conversion-and-enrichment.reading` = The choices differ within the same technology with each step's own measures (uranium mining: need 61, value 54, position 85; uranium conversion and enrichment: need 61, value 88, position 35) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.59** Supplier: 16% of world exports or production of uranium conversion & enrichment (the AIB measure of the product, not a step's share); the largest allied holder of the step

- `shared.uranium-conversion-and-enrichment.aib_role` = Supplier: 16% of world exports or production of uranium conversion & enrichment (the AIB measure of the product, not a step's share); the largest allied holder of the step | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.60** Insulators & bushings

- `shared.insulators-and-bushings.label` = Insulators & bushings | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.61** grid components (Watch); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up)

- `shared.insulators-and-bushings.steps` = grid components (Watch); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.62** The choices differ across technologies with each step's own measures (grid components: need 33, value 47, position 2; wafers and PV cells: need 50, value 59, position 2; blades, towers and nacelle parts: need 45, value 89, position 17)

- `shared.insulators-and-bushings.reading` = The choices differ across technologies with each step's own measures (grid components: need 33, value 47, position 2; wafers and PV cells: need 50, value 59, position 2; blades, towers and nacelle parts: need 45, value 89, position 17) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.63** Allies need new capacity here (BUILD); no supplier role

- `shared.insulators-and-bushings.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.64** Li-ion cells & packs

- `shared.li-ion-cells-and-packs.label` = Li-ion cells & packs | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.65** battery cells and packs (Preserve) and EV drivetrain components (Catch Up)

- `shared.li-ion-cells-and-packs.steps` = battery cells and packs (Preserve) and EV drivetrain components (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.66** The choices differ across technologies with each step's own measures (battery cells and packs: need 68, value 97, position 87; EV drivetrain components: need 58, value 95, position 1)

- `shared.li-ion-cells-and-packs.reading` = The choices differ across technologies with each step's own measures (battery cells and packs: need 68, value 97, position 87; EV drivetrain components: need 58, value 95, position 1) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.67** Allies need new capacity here (BUILD); no supplier role

- `shared.li-ion-cells-and-packs.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.68** Power electronics components

- `shared.power-electronics-components.label` = Power electronics components | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.69** grid components (Watch); EV drivetrain components (Catch Up); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up)

- `shared.power-electronics-components.steps` = grid components (Watch); EV drivetrain components (Catch Up); wafers and PV cells (Watch); and blades, towers and nacelle parts (Catch Up) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.70** The choices differ across technologies with each step's own measures (grid components: need 33, value 47, position 2; EV drivetrain components: need 58, value 95, position 1; wafers and PV cells: need 50, value 59, position 2; blades, towers and nacelle parts: need 45, value 89, position 17)

- `shared.power-electronics-components.reading` = The choices differ across technologies with each step's own measures (grid components: need 33, value 47, position 2; EV drivetrain components: need 58, value 95, position 1; wafers and PV cells: need 50, value 59, position 2; blades, towers and nacelle parts: need 45, value 89, position 17) | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.71** Allies need new capacity here (BUILD); no supplier role

- `shared.power-electronics-components.aib_role` = Allies need new capacity here (BUILD); no supplier role | derived: AIB nodes[].yfCells shared by several steps, with the steps' choices (country_brief_shared_capabilities()) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s2.74** Of the 23 Watch steps, 10 are deployments below the Accelerate bar.

- `counts.watch` = 23 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `lists.watch_deploy` = 10 | derived: lists over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s2.76** A top-quarter position (85) and 23% of world supply, but need and value sit below their bars: no domestic fence, and reliable supply to allies as an allied commitment.

- `cells.nuclear-extraction.cp` = 85 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.nuclear-extraction.share` = 23% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.78** Large, commoditized yards, where Canada holds 5% and 2% of world supply.

- `cells.gas-refining.share` = 5% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high
- `cells.oil-refining.share` = 2% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**s2.80** A commoditized yard and a low-need step (33), yet both rank high in Canada's protection measures.

- `cells.electric-grid-components.need` = 33 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s2.83** Low need (50) and almost no position (2); the cell layers read Catch Up only on that thin anchor.

- `cells.solar-components.need` = 50 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high
- `cells.solar-components.cp` = 2 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**s3.2** In the Allied Industrial Base analysis Canada is a Resource & Processing Anchor, first among allies on resource & processing positions.

- `aib.role` = Resource & Processing Anchor | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.first_on` = resource & processing positions | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.3** It supplies 10% of world exports or production of nickel intermediates and chemicals and 13% of aluminium wire rod.

- `aib.contributes.nickel-intermediates-and-chemicals.share` = 10% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.contributes.aluminium-wire-rod.share` = 13% | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.4** It holds a core seat in Nuclear Fuel & Reactor Systems, with 41% of world exports or production of nuclear special materials: heavy water and enriched isotopes.

- `aib.core_in` = Nuclear Fuel & Reactor Systems | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-NUCLEAR.lead_share` = 41% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.5** The AIB also names it the largest allied holder of uranium conversion and enrichment (16%, with the United Kingdom and France next), but that rests on concentrate exports, not conversion.

- `aib.division.nuclear-refining.name` = uranium conversion and enrichment | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.division.nuclear-refining.share` = 16% | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.division.nuclear-refining.backups` = the United Kingdom and France | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.8** If China's supply stopped, as in the Decoupling scenario, Canada would lose US$4.2 billion a year of imports across the Allied Industrial Base basket.

- `cells.electric-vehicles-refining.coercive_top` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.lost_usd` = US$4.2 billion | AIB network.countries[].lostUsd | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.9** New capacity at the best-placed allied hosts, led by Germany, would restore about US$1.6 billion of it a year.

- `aib.investment_decoupling.top_host` = Germany | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.investment_decoupling.total` = US$1.6 billion | AIB investmentFlows.pairs (Decoupling; Managed Interdependence file) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.10** Under Managed Interdependence, the default scenario, Canada sits in the minimum coalition of two Sectoral Compacts; under Decoupling, three, each as a demand anchor.

- `aib.scenarios.managed.label` = Managed Interdependence | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_managed` = two | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.scenarios.decoupling.label` = Decoupling | AIB strategic.scenarios | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.ws_decoupling` = three | AIB strategic.countries[].metrics; scenario-managed compacts[].coalition | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.18** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.19** Nickel intermediates and chemicals (10%, its lead product); a home cell and pack base to expand

- `aib.compacts.SC-BATTERIES.lead_share` = 10% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.20** Battery minerals; battery components, now mostly from China

- `cells.batteries-components.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s3.21** Australia and the United States for minerals; the compact's makers, such as South Korea

- `cells.batteries-extraction.partners` = Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate
- `aib.compacts.SC-BATTERIES.core_makers` = South Korea | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.23** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.24** Nuclear special materials (41%) and uranium (23% of world mining supply), with a core seat

- `aib.compacts.SC-NUCLEAR.lead_share` = 41% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `cells.nuclear-extraction.share` = 23% | posture detail cells[].home_share | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**s3.26** The next allied holders, the United Kingdom and France

- `aib.division.nuclear-refining.backups` = the United Kingdom and France | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.28** Permanent Magnets & Drive Systems

- `aib.all_compacts.SC-MAGNETS` = Permanent Magnets & Drive Systems | AIB strategic.countries[].slots and items | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.31** Refiners such as Japan, Australia and the United States

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s3.33** Electric Vehicles and Grid Equipment & Conductors

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run
- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s3.34** Electric buses, trucks and tractors (7%); aluminium wire rod (13%)

- `aib.compacts.SC-EV.lead_share` = 7% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run
- `aib.compacts.SC-GRID.lead_share` = 13% | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: AIB model run

**s3.36** Makers with core seats in both compacts, Japan and Germany

- `aib.compacts.SC-GRID.core_makers` = Japan and Germany | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s4.4** 53 pairs qualify, with 16 developing partners across 12 steps.

- `dev.n_pairs` = 53 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_partners` = 16 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_steps` = 12 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.5** Most, 37 pairs, rest on the allied gap alone, a reason that is the same for every donor in scope.

- `dev.n_allied_pairs` = 37 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.6** Canada's own supply chain supports 16 pairs with 7 partners across four steps, and the table leads with them: coal-plant components; coal-plant equipment; battery components; and battery cells and packs.

- `dev.n_own_pairs` = 16 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_partners` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.n_own_steps` = four | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.own_steps` = coal-plant components; coal-plant equipment; battery components; and battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.7** The coal pairs rest on high need in steps Canada sources from allied makers, while its coal power fell from 8.8 to 3.4 GW between 2020 and 2025.

- `cells.coal-deployment.deployment_trend` = 8.8 to 3.4 GW | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate
- `cells.coal-deployment.deployment_years` = 2020 and 2025 | posture detail cells[].deployment_trend | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**s4.9** Malaysia and Indonesia are upper-middle-income economies, so finance, guarantees and offtake should lead.

- `dev.rows.r2.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.10** Cells and packs are a Preserve step, whose policy design sets no partner-country finance, so the Malaysia case rests on high need and supplements the home build.

- `dev.rows.r2.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.11** Madagascar (low income, with a Competitive Position of 0) and Mexico rest on the allied gap alone.

- `dev.rows.r4.partner` = Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.income` = low income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `dev.rows.r4.cp` = 0 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: derived
- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.18** Coal-plant components: high need

- `dev.rows.r1.capability` = Coal-plant components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.problem` = high need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.19** Viet Nam (lower middle income)

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.income` = lower middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.20** A share of world supply and a share of world exports; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r1.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r1.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.21** Plant capacity not covered by the data and input costs not covered by the data.

- `dev.rows.r1.unobserved` = Plant capacity not covered by the data and input costs not covered by the data | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.22** Supplier development, technical and vocational skills, quality systems and certification, common testing facilities; then, at scale, development finance, guarantees, offtake, equity and joint ventures

- `dev.rows.r1.tools` = Supplier development, technical and vocational skills, quality systems and certification, common testing facilities; then, at scale, development finance, guarantees, offtake, equity and joint ventures | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.23** A trusted second source

- `dev.rows.r1.donor_gain` = A trusted second source | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.24** A certified supplier base and export sales of components

- `dev.rows.r1.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.25** Battery cells and packs: high need and new allied capacity needed

- `dev.rows.r2.capability` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.26** Malaysia (upper middle income)

- `dev.rows.r2.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.27** A share of world supply and a share of world exports; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r2.anchors` = A share of world supply and a share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r2.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.28** Constraint: no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r2.constraints` = no operating or committed plant in the capacity data and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.29** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r2.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.30** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 2 product groups of the step)

- `dev.rows.r2.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.n_build` = 2 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.31** Factory capacity, skills and export sales of finished equipment

- `dev.rows.r2.partner_gain` = Factory capacity, skills and export sales of finished equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.32** Battery components: high need and new allied capacity needed

- `dev.rows.r3.capability` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.problem` = high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.33** Indonesia (upper middle income)

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.34** A share of world exports; corroborated by feasibility, input costs, specialization in the adjacent step and industrial learning

- `dev.rows.r3.anchors` = A share of world exports | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r3.supports` = feasibility, input costs, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.35** Constraint: operating or committed plant below the capacity bar and a position that rests on trade or output shares, not on plant capacity.

- `dev.rows.r3.constraints` = operating or committed plant below the capacity bar and a position that rests on trade or output shares, not on plant capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.36** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion

- `dev.rows.r3.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for project preparation and investment promotion | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.37** Diversified supply for a high-need step and new allied capacity where the AIB finds a gap (allies need new capacity in 1 product group of the step)

- `dev.rows.r3.donor_gain` = Diversified supply for a high-need step and new allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.n_build` = 1 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.38** A certified supplier base and export sales of components

- `dev.rows.r3.partner_gain` = A certified supplier base and export sales of components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.39** Battery materials refining: new allied capacity needed

- `dev.rows.r4.capability` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.problem` = new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.40** Madagascar (low income)

- `dev.rows.r4.partner` = Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.income` = low income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.41** A share of world exports and operating capacity; corroborated by specialization in the adjacent step

- `dev.rows.r4.anchors` = A share of world exports and operating capacity | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r4.supports` = specialization in the adjacent step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.42** Constraint: related industrial know-how outside the top quarter and input costs outside the cheapest quarter.

- `dev.rows.r4.constraints` = related industrial know-how outside the top quarter and input costs outside the cheapest quarter | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.43** Workforce development, technology transfer and joint ventures, industrial infrastructure, feasibility studies and project preparation; then, at scale, offtake, market access, investment guarantees

- `dev.rows.r4.tools` = Workforce development, technology transfer and joint ventures, industrial infrastructure, feasibility studies and project preparation; then, at scale, offtake, market access, investment guarantees | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.44** New allied capacity where the AIB finds a gap (allies need new capacity in 6 product groups of the step)

- `dev.rows.r4.donor_gain` = New allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.n_build` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.45** Processing capacity at home, with the skills, standards and value capture of a refinery

- `dev.rows.r4.partner_gain` = Processing capacity at home, with the skills, standards and value capture of a refinery | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.46** EV and charger manufacturing: new allied capacity needed

- `dev.rows.r5.capability` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.problem` = new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.47** Mexico (upper middle income)

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.income` = upper middle income | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**s4.48** A share of world supply, a share of world exports, operating capacity and a strong position in the next step up the chain; corroborated by feasibility, specialization in the adjacent step and industrial learning

- `dev.rows.r5.anchors` = A share of world supply, a share of world exports, operating capacity and a strong position in the next step up the chain | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | evidence: derived
- `dev.rows.r5.supports` = feasibility, specialization in the adjacent step and industrial learning | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.49** Constraint: input costs outside the cheapest quarter.

- `dev.rows.r5.constraints` = input costs outside the cheapest quarter | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.50** Lead with development finance, guarantees, offtake, equity and joint ventures; public support for industrial infrastructure and supplier development

- `dev.rows.r5.tools` = Lead with development finance, guarantees, offtake, equity and joint ventures; public support for industrial infrastructure and supplier development | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.51** New allied capacity where the AIB finds a gap (allies need new capacity in 4 product groups of the step)

- `dev.rows.r5.donor_gain` = New allied capacity where the AIB finds a gap | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.n_build` = 4 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.52** Factory capacity, skills and export sales of finished equipment

- `dev.rows.r5.partner_gain` = Factory capacity, skills and export sales of finished equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.54** The second table shows what ties Canada to each partner today; aid is the 2020 to 2024 total at the step, and the direct investment position is the economy-wide total, not tied to the step.

- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.55** The deepest tie is with Mexico, where Canadian firms already have a factory project in the technology.

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.59** Viet Nam: coal-plant components

- `dev.rows.r1.partner` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r1.capability` = coal-plant components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.60** $0.5 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $3,811 million (economy-wide)

- `dev.rows.r1.ties` = $0.5 million at this step, under stage-wide codes (the technology is not identified) and a direct investment position of $3,811 million (economy-wide) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.61** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's own deployment demand for the technology (Accelerate)

- `dev.rows.r1.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's own deployment demand for the technology (Accelerate) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.62** Malaysia: battery cells and packs

- `dev.rows.r2.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.63** No energy, industry or mining aid recorded for the pair; a direct investment position of $1,112 million (economy-wide); no factory projects by firms from Canada recorded in the technology; and no export-credit or lender finance recorded at the step

- `dev.rows.r2.ties` = No energy, industry or mining aid recorded for the pair; a direct investment position of $1,112 million (economy-wide); no factory projects by firms from Canada recorded in the technology; and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.64** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Battery Materials & Cells compact

- `dev.rows.r2.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Specialized technology supplier in the Battery Materials & Cells compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.65** Indonesia: battery components

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.66** $95 million of energy, industry and mining aid, none at this step; a direct investment position of $4,196 million (economy-wide); no factory projects by firms from Canada recorded in the technology; and no export-credit or lender finance recorded at the step

- `dev.rows.r3.ties` = $95 million of energy, industry and mining aid, none at this step; a direct investment position of $4,196 million (economy-wide); no factory projects by firms from Canada recorded in the technology; and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.67** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Processing anchor in the Battery Materials & Cells compact

- `dev.rows.r3.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Processing anchor in the Battery Materials & Cells compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.68** Madagascar: battery materials refining

- `dev.rows.r4.partner` = Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.69** No energy, industry or mining aid recorded for the pair; a direct investment position of $50 million (economy-wide); and no export-credit or lender finance recorded at the step

- `dev.rows.r4.ties` = No energy, industry or mining aid recorded for the pair; a direct investment position of $50 million (economy-wide); and no export-credit or lender finance recorded at the step | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.70** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Resource supplier in the Battery Materials & Cells compact

- `dev.rows.r4.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Resource supplier in the Battery Materials & Cells compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.71** Mexico: EV and charger manufacturing

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.capability` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.72** $5.1 million of energy, industry and mining aid, none at this step; a direct investment position of $24,489 million (economy-wide); 1 factory project by firms from Canada in the technology; and an AIB simulated investment pairing (a model result, not an existing tie)

- `dev.rows.r5.ties` = $5.1 million of energy, industry and mining aid, none at this step; a direct investment position of $24,489 million (economy-wide); 1 factory project by firms from Canada in the technology; and an AIB simulated investment pairing (a model result, not an existing tie) | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | International Monetary Fund, Direct Investment Positions: outward positions (reported, else derived from the counterpart's reports) | fDi Markets clean-manufacturing projects (CM-FDI): factory projects by home and destination economy (Components and Assembly not separated) | Oil Change International, Public Finance for Energy: export-credit and development-lender energy finance (not aid) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: derived

**s4.73** Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Electric Vehicles compact

- `dev.rows.r5.durable` = Long-term offtake, joint ventures and investment guarantees as the capability scales; the partner's role as Scale manufacturer in the Electric Vehicles compact | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | Development tool families by supply-chain stage and constraint, and the development-partnership tests, set by the authors (config/development_partnerships.yml) | evidence: derived

**s4.74** Battery components is among the broadest strands: 7 developing economies, led by Viet Nam and Indonesia, have credible pathways.

- `dev.by_step.batteries-components.n_development` = 7 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.by_step.batteries-components.development_top` = Viet Nam and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.75** For battery mineral mining; EV mineral mining; and rare-earth and battery-metal refining: No credible development-led supply solution identified.

- `dev.none_steps` = battery mineral mining; EV mineral mining; and rare-earth and battery-metal refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.76** In rare-earth refining, the AIB places new capacity in Malaysia and Thailand, a commercial case.

- `dev.by_step.electric-vehicles-refining.hosts` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.77** Another 30 pairs are commercial, where trade, offtake and investment fit better than aid.

- `dev.n_commercial` = 30 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.79** Only Viet Nam and Ukraine receive Canadian aid at the step of a partnership, under stage-wide codes that do not identify the technology.

- `dev.aid_stage_wide` = Viet Nam and Ukraine | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.80** Of the other partners, 8 receive it at other steps only and 6 at no supply-chain step.

- `dev.aid_wrong_stage` = 8 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_none` = 6 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s4.81** The largest recipient, South Africa, received $183 million over 2020 to 2024, none of it in battery materials refining, where it has a credible pathway.

- `dev.aid_top_1` = South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_top_1_usd` = $183 million | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.aid_years` = 2020 to 2024 | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s5.1** Only gas networks and use reach the Accelerate bar of 60, with a need of 60 on two measures, moderate confidence and demand adequacy in the lead.

- `cells.gas-deployment.name` = gas networks and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate
- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `cells.gas-deployment.dn` = 60 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate
- `cells.gas-deployment.dn_families` = two | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate
- `cells.gas-deployment.dn_confidence` = moderate | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**s5.2** Battery storage (51) and grid networks (50) come next, on all three measures with high confidence, driven by system need.

- `cells.batteries-deployment.name` = Battery storage | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high
- `cells.batteries-deployment.dn` = 51 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence moderate; choice confidence high; deployment-need confidence high
- `cells.electric-grid-deployment.name` = grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high
- `cells.electric-grid-deployment.dn` = 50 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence high
- `cells.batteries-deployment.dn_families` = three | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**s5.3** Wind reads high on demand adequacy (76) but low on system need (18).

- `cells.wind-deployment.dn_demand` = 76 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.wind-deployment.dn_system` = 18 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s5.4** The renewables target of 126 GW by 2030 compares with 111 GW installed in 2025, a projected shortfall of about 8 GW by then at the recent build rate.

- `cells.solar-deployment.target.target_gw` = 126 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.target_year` = 2030 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.installed_gw` = 111 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.installed_year` = 2025 | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high
- `cells.solar-deployment.target.shortfall_gw` = 8 GW | posture detail cells[].dn_policy_gap.note (national target) | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**s6.8** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.9** Catch Up

- `cells.electric-vehicles-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.10** Needs more attention

- `cells.electric-vehicles-components.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.11** Capability case without enough policy attention

- `cells.electric-vehicles-components.gap` = Capability case without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.12** Blades, towers and nacelle parts

- `cells.wind-components.name` = Blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.13** Catch Up

- `cells.wind-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.14** Needs more attention

- `cells.wind-components.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.15** Capability case without enough policy attention

- `cells.wind-components.gap` = Capability case without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.16** Wind turbines

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.17** Catch Up

- `cells.wind-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.18** Needs more attention

- `cells.wind-assembly.alignment` = Needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.19** Capability case without enough policy attention

- `cells.wind-assembly.gap` = Capability case without enough policy attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.20** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.21** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**s6.22** Heavy protection or localization

- `cells.solar-components.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.23** Localizing what is not a domestic priority

- `cells.solar-components.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s6.24** Grid metals refining

- `cells.electric-grid-refining.name` = Grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.25** Watch

- `cells.electric-grid-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**s6.26** Heavy protection or localization

- `cells.electric-grid-refining.policy_flag` = Heavy protection or localization | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.27** Localizing what is not a domestic priority

- `cells.electric-grid-refining.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**s6.28** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**s6.29** Watch

- `cells.electric-grid-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**s6.30** Heavy local-content protection on the equipment of a Watch deployment

- `cells.electric-grid-deployment.policy_flag` = Heavy local-content protection on the equipment of a Watch deployment | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence high

**s6.31** Localizing what is not a domestic priority

- `cells.electric-grid-deployment.gap` = Localizing what is not a domestic priority | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence high

**s6.32** First, protection ranks high where the strategy is to stay open: wafers and PV cells (73), grid metals refining (76) and the equipment of Watch deployments such as grid networks (78).

- `cells.solar-components.protection` = 73 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high
- `cells.electric-grid-refining.protection` = 76 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high
- `cells.electric-grid-deployment.protection` = 78 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence high

**s6.35** Attention belongs first with battery components, whose policy priority is only 30.

- `cells.batteries-components.policy_priority` = 30 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**s6.36** In EV drivetrains, where protection already ranks high (81), it should come as time-limited support tied to benchmarks.

- `cells.electric-vehicles-components.protection` = 81 | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**s7.3** Build trusted separation, metal and magnet capacity with allies such as Japan, Australia and the United States through co-investment and offtake, and at home where a foothold can be built.

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.5** No developing partner qualifies in this step; the AIB places new capacity in Malaysia and Thailand, a commercial case.

- `dev.by_step.electric-vehicles-refining.hosts` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.9** In battery components, where China provides 76% of imports, use time-limited production incentives and demand creation tied to benchmarks, at home and with allied partners.

- `cells.batteries-components.top_supplier` = China | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate
- `cells.batteries-components.china_share` = 76% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**s7.16** Buy battery and EV minerals through offtake and co-investment with producers such as Australia and the United States, spread across several suppliers.

- `cells.batteries-extraction.partners` = Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**s7.18** Source conversion and enrichment through co-investment and offtake with allied partners, such as the United Kingdom and France.

- `aib.division.nuclear-refining.backups` = the United Kingdom and France | posture core division (allied division of labor) | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**s7.21** Start with Indonesia in battery components, where Canadian need is high, leading with development finance, guarantees and offtake; Malaysia in battery cells and packs only supplements the home build.

- `dev.rows.r3.partner` = Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r3.capability` = battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.partner` = Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r2.capability` = battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**s7.22** Add Mexico in EV and charger manufacturing, where recorded ties are deepest, and prepare Madagascar in battery materials refining over a longer horizon.

- `dev.rows.r5.partner` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r5.capability` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.partner` = Madagascar | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived
- `dev.rows.r4.capability` = battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.2** Every number comes from the Country page data of the Allied Industrial Base Explorer (the data release of 30 October 2026) and from the Allied Industrial Base analysis.

- `meta.as_of_label` = 30 October 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.4** A step is Secure where an acute vulnerability is measured, and a deployment step Accelerate where deployment need reaches 60.

- `meta.bars.deployment` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.6** Preserve needs a top-quarter Competitive Position (75) and high Strategic Industrial Value (bar 60) or supply-chain need (bar 67).

- `meta.bars.cp` = 75 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.prize` = 60 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
- `meta.bars.need` = 67 | posture core meta.thresholds | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.12** The brief treats five steps as review cases.

- `counts.review` = five | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.16** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.17** Watch

- `cells.green-hydrogen-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.18** A single indicator of low confidence

- `cells.green-hydrogen-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.19** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.20** Preserve

- `cells.oil-extraction.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.21** Flagged for review by the engine

- `cells.oil-extraction.review` = Flagged for review by the engine | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.22** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.23** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.24** A single indicator of low confidence

- `cells.oil-deployment.review` = A single indicator of low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.25** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.26** Partner

- `cells.oil-recovery.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.27** Low confidence

- `cells.oil-recovery.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.28** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.29** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.30** Low confidence

- `cells.wind-refining.review` = Low confidence | derived: country_brief_review() over core cells.conf, fl, detail pathway_basis, dn_confidence and config review_cells | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.32** The Secure refining step and battery materials refining show the same 94% share of world supply, which suggests shared battery-metal supply data.

- `cells.batteries-refining.coercive_top_share` = 94% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.33** The status "allied supply covers the need" for rare-earth and battery-metal refining describes the allied model, not ample trusted supply (4% of world supply).

- `cells.electric-vehicles-refining.trusted_share` = 4% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.34** In battery and EV mineral mining the trusted share reads 0%, which conflicts with allied supply that covers the need; this brief relies on the partner list.

- `cells.batteries-extraction.trusted_share` = 0% | posture detail cells[] (need components and trade) | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.35** 18 steps lack the data for a call, including most of geothermal, green hydrogen manufacturing and most recycling.

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.43** In rare-earth refining, the other candidates rest on battery-metal capacity data, and Mexico on a thin anchor.

- `dev.by_step.electric-vehicles-refining.review_thin_anchor` = Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.50** Coal-plant components

- `dev.by_step.coal-components.name` = Coal-plant components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.51** Partner and high need

- `dev.by_step.coal-components.reason` = Partner and high need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.52** Viet Nam, India and Indonesia

- `dev.by_step.coal-components.development` = Viet Nam, India and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.53** Turkiye

- `dev.by_step.coal-components.review` = Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.54** Viet Nam

- `dev.by_step.coal-components.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.55** Coal-plant equipment

- `dev.by_step.coal-assembly.name` = Coal-plant equipment | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.56** Partner and high need

- `dev.by_step.coal-assembly.reason` = Partner and high need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.57** Viet Nam

- `dev.by_step.coal-assembly.development` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.58** India

- `dev.by_step.coal-assembly.commercial` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.59** Turkiye

- `dev.by_step.coal-assembly.review` = Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.60** Viet Nam

- `dev.by_step.coal-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.61** Battery components

- `dev.by_step.batteries-components.name` = Battery components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.62** High need and new allied capacity needed

- `dev.by_step.batteries-components.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.63** Viet Nam, Indonesia, Mexico, Turkiye, India, Morocco and Malaysia

- `dev.by_step.batteries-components.development` = Viet Nam, Indonesia, Mexico, Turkiye, India, Morocco and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.64** Viet Nam

- `dev.by_step.batteries-components.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.65** Battery cells and packs

- `dev.by_step.batteries-assembly.name` = Battery cells and packs | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.66** High need and new allied capacity needed

- `dev.by_step.batteries-assembly.reason` = High need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.67** Malaysia, Indonesia, Mexico, India and Turkiye

- `dev.by_step.batteries-assembly.development` = Malaysia, Indonesia, Mexico, India and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.68** Viet Nam

- `dev.by_step.batteries-assembly.commercial` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.69** Viet Nam

- `dev.by_step.batteries-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.70** Battery materials refining

- `dev.by_step.batteries-refining.name` = Battery materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.71** New allied capacity needed

- `dev.by_step.batteries-refining.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.72** Madagascar, the Philippines, India, South Africa, Papua New Guinea, Thailand and Turkiye

- `dev.by_step.batteries-refining.development` = Madagascar, the Philippines, India, South Africa, Papua New Guinea, Thailand and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.73** Indonesia, Argentina, the Democratic Republic of Congo and Malaysia

- `dev.by_step.batteries-refining.commercial` = Indonesia, Argentina, the Democratic Republic of Congo and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.74** Mozambique, Colombia and Tanzania

- `dev.by_step.batteries-refining.review` = Mozambique, Colombia and Tanzania | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.75** EV and charger manufacturing

- `dev.by_step.electric-vehicles-assembly.name` = EV and charger manufacturing | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.76** New allied capacity needed

- `dev.by_step.electric-vehicles-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.77** Mexico, Viet Nam, Thailand, Malaysia, Morocco and Indonesia

- `dev.by_step.electric-vehicles-assembly.development` = Mexico, Viet Nam, Thailand, Malaysia, Morocco and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.78** Turkiye and India

- `dev.by_step.electric-vehicles-assembly.commercial` = Turkiye and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.79** Viet Nam

- `dev.by_step.electric-vehicles-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.80** Wind turbines

- `dev.by_step.wind-assembly.name` = Wind turbines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.81** New allied capacity needed

- `dev.by_step.wind-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.82** Viet Nam, Turkiye, Malaysia, Indonesia, Morocco and Thailand

- `dev.by_step.wind-assembly.development` = Viet Nam, Turkiye, Malaysia, Indonesia, Morocco and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.83** India and Mexico

- `dev.by_step.wind-assembly.commercial` = India and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.84** Brazil

- `dev.by_step.wind-assembly.review` = Brazil | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.85** Viet Nam

- `dev.by_step.wind-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.86** EV drivetrain components

- `dev.by_step.electric-vehicles-components.name` = EV drivetrain components | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.87** New allied capacity needed

- `dev.by_step.electric-vehicles-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.88** Malaysia, Thailand, Ukraine, Honduras, Morocco, Turkiye and Serbia

- `dev.by_step.electric-vehicles-components.development` = Malaysia, Thailand, Ukraine, Honduras, Morocco, Turkiye and Serbia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.89** Indonesia, the Philippines, Mexico and India

- `dev.by_step.electric-vehicles-components.commercial` = Indonesia, the Philippines, Mexico and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.90** Viet Nam

- `dev.by_step.electric-vehicles-components.review` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.91** Ukraine and Viet Nam

- `dev.by_step.electric-vehicles-components.aid` = Ukraine and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.92** Blades, towers and nacelle parts

- `dev.by_step.wind-components.name` = Blades, towers and nacelle parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.93** New allied capacity needed

- `dev.by_step.wind-components.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.94** Thailand, Serbia, Turkiye, Brazil and Malaysia

- `dev.by_step.wind-components.development` = Thailand, Serbia, Turkiye, Brazil and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.95** Mexico, Viet Nam, the Philippines and India

- `dev.by_step.wind-components.commercial` = Mexico, Viet Nam, the Philippines and India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.96** Viet Nam

- `dev.by_step.wind-components.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.97** Transformers, switchgear and cables

- `dev.by_step.electric-grid-assembly.name` = Transformers, switchgear and cables | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.98** New allied capacity needed

- `dev.by_step.electric-grid-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.99** Malaysia, India and the Philippines

- `dev.by_step.electric-grid-assembly.development` = Malaysia, India and the Philippines | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.100** Mexico, Turkiye, Thailand and Viet Nam

- `dev.by_step.electric-grid-assembly.commercial` = Mexico, Turkiye, Thailand and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.101** Morocco

- `dev.by_step.electric-grid-assembly.review` = Morocco | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.102** Viet Nam

- `dev.by_step.electric-grid-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.103** Rare-earth and wind materials refining

- `dev.by_step.wind-refining.name` = Rare-earth and wind materials refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.104** Allied supply falls short and new allied capacity needed

- `dev.by_step.wind-refining.reason` = Allied supply falls short and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.105** India

- `dev.by_step.wind-refining.development` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.106** Viet Nam and South Africa

- `dev.by_step.wind-refining.review` = Viet Nam and South Africa | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.107** Viet Nam

- `dev.by_step.wind-refining.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.108** PV modules and inverters

- `dev.by_step.solar-assembly.name` = PV modules and inverters | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.109** New allied capacity needed

- `dev.by_step.solar-assembly.reason` = New allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.110** Morocco and Turkiye

- `dev.by_step.solar-assembly.development` = Morocco and Turkiye | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.111** Viet Nam, India, Thailand, Malaysia, Mexico and Indonesia

- `dev.by_step.solar-assembly.commercial` = Viet Nam, India, Thailand, Malaysia, Mexico and Indonesia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.112** Viet Nam

- `dev.by_step.solar-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.113** Rare-earth and battery-metal refining

- `dev.by_step.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.114** Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed

- `dev.by_step.electric-vehicles-refining.reason` = Secure with allied delivery, few trusted alternatives, high need and new allied capacity needed | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.115** Malaysia and Thailand

- `dev.by_step.electric-vehicles-refining.commercial` = Malaysia and Thailand | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.116** Argentina, Indonesia, the Democratic Republic of Congo, Mozambique, Madagascar, the Philippines, Tanzania and Mexico

- `dev.by_step.electric-vehicles-refining.review` = Argentina, Indonesia, the Democratic Republic of Congo, Mozambique, Madagascar, the Philippines, Tanzania and Mexico | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.117** Nuclear fuel and reactor parts

- `dev.by_step.nuclear-components.name` = Nuclear fuel and reactor parts | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.118** High need

- `dev.by_step.nuclear-components.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.119** India, Thailand and Viet Nam

- `dev.by_step.nuclear-components.review` = India, Thailand and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.120** Viet Nam

- `dev.by_step.nuclear-components.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.121** Reactors and steam generators

- `dev.by_step.nuclear-assembly.name` = Reactors and steam generators | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.122** High need

- `dev.by_step.nuclear-assembly.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.123** Turkiye, India, Malaysia and Viet Nam

- `dev.by_step.nuclear-assembly.review` = Turkiye, India, Malaysia and Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.124** Viet Nam

- `dev.by_step.nuclear-assembly.aid` = Viet Nam | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.125** Battery recycling

- `dev.by_step.batteries-recovery.name` = Battery recycling | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.126** High need

- `dev.by_step.batteries-recovery.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.127** India and Malaysia

- `dev.by_step.batteries-recovery.review` = India and Malaysia | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.128** Polysilicon refining

- `dev.by_step.solar-refining.name` = Polysilicon refining | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.129** High need

- `dev.by_step.solar-refining.reason` = High need | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.130** India

- `dev.by_step.solar-refining.review` = India | devpart-<ISO3> file (scripts/93; country_devpart.R): pairs, steps and aid comparison | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | OECD Creditor Reporting System: official development assistance by donor, recipient and purpose code, gross disbursements at constant prices, energy, industry and mining purpose codes only, mapped to supply-chain steps by the authors (Components and Assembly share purpose codes; no code funds recycling) | evidence: derived

**a.141** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.142** Extraction

- `cells.batteries-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.143** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.144** Medium

- `cells.batteries-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.145** 96

- `cells.batteries-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.146** 57

- `cells.batteries-extraction.siv` = 57 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.147** 41

- `cells.batteries-extraction.cp` = 41 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.148** strong

- `cells.batteries-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.149** broadly aligned

- `cells.batteries-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.150** Battery materials refining

- `cells.batteries-refining.name` = Battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.151** Refining

- `cells.batteries-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.152** Catch Up

- `cells.batteries-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.153** Medium

- `cells.batteries-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.154** 47

- `cells.batteries-refining.need` = 47 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.155** 77

- `cells.batteries-refining.siv` = 77 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.156** 0

- `cells.batteries-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.157** strong

- `cells.batteries-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.158** broadly aligned

- `cells.batteries-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.159** Battery components

- `cells.batteries-components.name` = Battery components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.160** Components

- `cells.batteries-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.161** Catch Up

- `cells.batteries-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.162** Medium

- `cells.batteries-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.163** 71

- `cells.batteries-components.need` = 71 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.164** 79

- `cells.batteries-components.siv` = 79 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.165** 66

- `cells.batteries-components.cp` = 66 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.166** moderate

- `cells.batteries-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.167** needs more attention

- `cells.batteries-components.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.168** Battery cells and packs

- `cells.batteries-assembly.name` = Battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.169** Assembly

- `cells.batteries-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.170** Preserve

- `cells.batteries-assembly.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.171** Medium

- `cells.batteries-assembly.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.172** 68

- `cells.batteries-assembly.need` = 68 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.173** 97

- `cells.batteries-assembly.siv` = 97 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.174** 87

- `cells.batteries-assembly.cp` = 87 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.175** moderate

- `cells.batteries-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.176** broadly aligned

- `cells.batteries-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.177** Battery storage

- `cells.batteries-deployment.name` = Battery storage | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.178** Deployment

- `cells.batteries-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.179** Watch

- `cells.batteries-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.180** Low

- `cells.batteries-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.181** 51

- `cells.batteries-deployment.dn` = 51 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.182** moderate

- `cells.batteries-deployment.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.183** broadly aligned

- `cells.batteries-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high; deployment-need confidence high

**a.184** Battery recycling

- `cells.batteries-recovery.name` = Battery recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.185** Recovery

- `cells.batteries-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.186** Catch Up

- `cells.batteries-recovery.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.187** Medium

- `cells.batteries-recovery.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence moderate

**a.188** 86

- `cells.batteries-recovery.need` = 86 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence moderate

**a.189** 82

- `cells.batteries-recovery.siv` = 82 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence moderate

**a.190** needs more attention

- `cells.batteries-recovery.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence moderate

**a.191** Coal mining

- `cells.coal-extraction.name` = Coal mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.192** Extraction

- `cells.coal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.193** Watch

- `cells.coal-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.194** Low

- `cells.coal-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.195** 54

- `cells.coal-extraction.need` = 54 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.196** 4

- `cells.coal-extraction.siv` = 4 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.197** 38

- `cells.coal-extraction.cp` = 38 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.198** strong

- `cells.coal-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.199** too much intervention

- `cells.coal-extraction.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.200** Coke and coal processing

- `cells.coal-refining.name` = Coke and coal processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.201** Refining

- `cells.coal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.202** Watch

- `cells.coal-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.203** Low

- `cells.coal-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.204** 95

- `cells.coal-refining.need` = 95 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.205** 9

- `cells.coal-refining.siv` = 9 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.206** 33

- `cells.coal-refining.cp` = 33 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.207** strong

- `cells.coal-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.208** mixed signals

- `cells.coal-refining.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.209** Coal-plant components

- `cells.coal-components.name` = Coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.210** Components

- `cells.coal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.211** Partner

- `cells.coal-components.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.212** Medium

- `cells.coal-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.213** 81

- `cells.coal-components.need` = 81 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.214** 39

- `cells.coal-components.siv` = 39 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.215** 12

- `cells.coal-components.cp` = 12 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.216** strong

- `cells.coal-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.217** broadly aligned

- `cells.coal-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.218** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.219** Assembly

- `cells.coal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.220** Partner

- `cells.coal-assembly.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.221** Medium

- `cells.coal-assembly.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.222** 68

- `cells.coal-assembly.need` = 68 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.223** 39

- `cells.coal-assembly.siv` = 39 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.224** 57

- `cells.coal-assembly.cp` = 57 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.225** strong

- `cells.coal-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.226** broadly aligned

- `cells.coal-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.227** Coal power and use

- `cells.coal-deployment.name` = Coal power and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.228** Deployment

- `cells.coal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.229** Watch

- `cells.coal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.230** Low

- `cells.coal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.231** 42

- `cells.coal-deployment.dn` = 42 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.232** moderate

- `cells.coal-deployment.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.233** broadly aligned

- `cells.coal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high; deployment-need confidence moderate

**a.234** Coal recovery

- `cells.coal-recovery.name` = Coal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.235** Recovery

- `cells.coal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.236** Not assessed

- `cells.coal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.237** 1

- `cells.coal-recovery.siv` = 1 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.238** not enough policy evidence

- `cells.coal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.239** Copper and bauxite mining

- `cells.electric-grid-extraction.name` = Copper and bauxite mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.240** Extraction

- `cells.electric-grid-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.241** Watch

- `cells.electric-grid-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.242** Low

- `cells.electric-grid-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.243** 80

- `cells.electric-grid-extraction.need` = 80 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.244** 41

- `cells.electric-grid-extraction.siv` = 41 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.245** 0

- `cells.electric-grid-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.246** strong

- `cells.electric-grid-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.247** broadly aligned

- `cells.electric-grid-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.248** Grid metals refining

- `cells.electric-grid-refining.name` = Grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.249** Refining

- `cells.electric-grid-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.250** Watch

- `cells.electric-grid-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.251** Mixed Medium/Low

- `cells.electric-grid-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.252** 53

- `cells.electric-grid-refining.need` = 53 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.253** 53

- `cells.electric-grid-refining.siv` = 53 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.254** 0

- `cells.electric-grid-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.255** strong

- `cells.electric-grid-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.256** too much intervention

- `cells.electric-grid-refining.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.257** Grid components

- `cells.electric-grid-components.name` = Grid components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.258** Components

- `cells.electric-grid-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.259** Watch

- `cells.electric-grid-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.260** Low

- `cells.electric-grid-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.261** 33

- `cells.electric-grid-components.need` = 33 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.262** 47

- `cells.electric-grid-components.siv` = 47 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.263** 2

- `cells.electric-grid-components.cp` = 2 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.264** strong

- `cells.electric-grid-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.265** too much intervention

- `cells.electric-grid-components.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.266** Transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = Transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.267** Assembly

- `cells.electric-grid-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.268** Catch Up

- `cells.electric-grid-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.269** Mixed Medium/Low

- `cells.electric-grid-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.270** 48

- `cells.electric-grid-assembly.need` = 48 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.271** 61

- `cells.electric-grid-assembly.siv` = 61 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.272** 11

- `cells.electric-grid-assembly.cp` = 11 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.273** strong

- `cells.electric-grid-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.274** broadly aligned

- `cells.electric-grid-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.275** Grid networks

- `cells.electric-grid-deployment.name` = Grid networks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.276** Deployment

- `cells.electric-grid-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.277** Watch

- `cells.electric-grid-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.278** Low

- `cells.electric-grid-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence high

**a.279** 50

- `cells.electric-grid-deployment.dn` = 50 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence high

**a.280** too much intervention

- `cells.electric-grid-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence high

**a.281** Grid-equipment recycling

- `cells.electric-grid-recovery.name` = Grid-equipment recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.282** Recovery

- `cells.electric-grid-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.283** Not assessed

- `cells.electric-grid-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.284** 14

- `cells.electric-grid-recovery.siv` = 14 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.285** not enough policy evidence

- `cells.electric-grid-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.286** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.287** Extraction

- `cells.electric-vehicles-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.288** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.289** Medium

- `cells.electric-vehicles-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.290** 96

- `cells.electric-vehicles-extraction.need` = 96 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.291** 59

- `cells.electric-vehicles-extraction.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.292** 64

- `cells.electric-vehicles-extraction.cp` = 64 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.293** strong

- `cells.electric-vehicles-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.294** broadly aligned

- `cells.electric-vehicles-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.295** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.296** Refining

- `cells.electric-vehicles-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.297** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.298** High

- `cells.electric-vehicles-refining.fence` = High | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.299** 94

- `cells.electric-vehicles-refining.need` = 94 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.300** 89

- `cells.electric-vehicles-refining.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.301** 5

- `cells.electric-vehicles-refining.cp` = 5 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.302** strong

- `cells.electric-vehicles-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.303** broadly aligned

- `cells.electric-vehicles-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.304** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.305** Components

- `cells.electric-vehicles-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.306** Catch Up

- `cells.electric-vehicles-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.307** Mixed High/Medium

- `cells.electric-vehicles-components.yard` = Mixed High/Medium | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.308** 58

- `cells.electric-vehicles-components.need` = 58 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.309** 95

- `cells.electric-vehicles-components.siv` = 95 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.310** 1

- `cells.electric-vehicles-components.cp` = 1 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.311** moderate

- `cells.electric-vehicles-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.312** needs more attention

- `cells.electric-vehicles-components.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.313** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.314** Assembly

- `cells.electric-vehicles-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.315** Catch Up

- `cells.electric-vehicles-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.316** Mixed Medium/Low

- `cells.electric-vehicles-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.317** 62

- `cells.electric-vehicles-assembly.need` = 62 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.318** 95

- `cells.electric-vehicles-assembly.siv` = 95 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.319** 5

- `cells.electric-vehicles-assembly.cp` = 5 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.320** moderate

- `cells.electric-vehicles-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.321** broadly aligned

- `cells.electric-vehicles-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.322** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.323** Deployment

- `cells.electric-vehicles-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.324** Watch

- `cells.electric-vehicles-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.325** Low

- `cells.electric-vehicles-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.326** 19

- `cells.electric-vehicles-deployment.dn` = 19 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.327** strong

- `cells.electric-vehicles-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.328** too much intervention

- `cells.electric-vehicles-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.329** EV recycling

- `cells.electric-vehicles-recovery.name` = EV recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.330** Recovery

- `cells.electric-vehicles-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.331** Not assessed

- `cells.electric-vehicles-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.332** 42

- `cells.electric-vehicles-recovery.siv` = 42 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.333** not enough policy evidence

- `cells.electric-vehicles-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.334** Gas production

- `cells.gas-extraction.name` = Gas production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.335** Extraction

- `cells.gas-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.336** Watch

- `cells.gas-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.337** Low

- `cells.gas-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.338** 77

- `cells.gas-extraction.need` = 77 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.339** 32

- `cells.gas-extraction.siv` = 32 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.340** 69

- `cells.gas-extraction.cp` = 69 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.341** strong

- `cells.gas-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.342** broadly aligned

- `cells.gas-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.343** Gas liquids processing

- `cells.gas-refining.name` = Gas liquids processing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.344** Refining

- `cells.gas-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.345** Watch

- `cells.gas-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.346** Low

- `cells.gas-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.347** 72

- `cells.gas-refining.need` = 72 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.348** 32

- `cells.gas-refining.siv` = 32 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.349** 70

- `cells.gas-refining.cp` = 70 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.350** strong

- `cells.gas-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.351** broadly aligned

- `cells.gas-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.352** Gas-turbine and drilling components

- `cells.gas-components.name` = Gas-turbine and drilling components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.353** Components

- `cells.gas-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.354** Catch Up

- `cells.gas-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.355** Medium

- `cells.gas-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.356** 56

- `cells.gas-components.need` = 56 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.357** 73

- `cells.gas-components.siv` = 73 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.358** 30

- `cells.gas-components.cp` = 30 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.359** strong

- `cells.gas-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.360** broadly aligned

- `cells.gas-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.361** Gas turbines and LNG plant

- `cells.gas-assembly.name` = Gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.362** Assembly

- `cells.gas-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.363** Catch Up

- `cells.gas-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.364** Mixed Medium/Low

- `cells.gas-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.365** 56

- `cells.gas-assembly.need` = 56 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.366** 73

- `cells.gas-assembly.siv` = 73 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.367** 47

- `cells.gas-assembly.cp` = 47 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.368** strong

- `cells.gas-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.369** broadly aligned

- `cells.gas-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.370** Gas networks and use

- `cells.gas-deployment.name` = Gas networks and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.371** Deployment

- `cells.gas-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.372** Accelerate

- `cells.gas-deployment.choice` = Accelerate | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.373** Medium

- `cells.gas-deployment.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.374** 60

- `cells.gas-deployment.dn` = 60 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.375** strong

- `cells.gas-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.376** broadly aligned

- `cells.gas-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate; deployment-need confidence moderate

**a.377** Gas recovery

- `cells.gas-recovery.name` = Gas recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.378** Recovery

- `cells.gas-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.379** Not assessed

- `cells.gas-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.380** 25

- `cells.gas-recovery.siv` = 25 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.381** not enough policy evidence

- `cells.gas-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.382** Geothermal resource development

- `cells.geothermal-extraction.name` = Geothermal resource development | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.383** Extraction

- `cells.geothermal-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.384** Not assessed

- `cells.geothermal-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.385** 20

- `cells.geothermal-extraction.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.386** not enough policy evidence

- `cells.geothermal-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.387** Geothermal refining

- `cells.geothermal-refining.name` = Geothermal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.388** Refining

- `cells.geothermal-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.389** Not assessed

- `cells.geothermal-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.390** 20

- `cells.geothermal-refining.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.391** not enough policy evidence

- `cells.geothermal-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.392** Geothermal plant components

- `cells.geothermal-components.name` = Geothermal plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.393** Components

- `cells.geothermal-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.394** Not assessed

- `cells.geothermal-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.395** 69

- `cells.geothermal-components.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.396** not enough policy evidence

- `cells.geothermal-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.397** Geothermal power-plant equipment

- `cells.geothermal-assembly.name` = Geothermal power-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.398** Assembly

- `cells.geothermal-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.399** Not assessed

- `cells.geothermal-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.400** 69

- `cells.geothermal-assembly.siv` = 69 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.401** not enough policy evidence

- `cells.geothermal-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.402** Geothermal power and heat

- `cells.geothermal-deployment.name` = Geothermal power and heat | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.403** Deployment

- `cells.geothermal-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.404** Watch

- `cells.geothermal-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.405** Low

- `cells.geothermal-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.406** 46

- `cells.geothermal-deployment.dn` = 46 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.407** strong

- `cells.geothermal-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.408** broadly aligned

- `cells.geothermal-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.409** Geothermal recovery

- `cells.geothermal-recovery.name` = Geothermal recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.410** Recovery

- `cells.geothermal-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.411** Not assessed

- `cells.geothermal-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.412** 20

- `cells.geothermal-recovery.siv` = 20 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.413** not enough policy evidence

- `cells.geothermal-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.414** Electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = Electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.415** Extraction

- `cells.green-hydrogen-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.416** Not assessed

- `cells.green-hydrogen-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.417** 35

- `cells.green-hydrogen-extraction.siv` = 35 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.418** 70

- `cells.green-hydrogen-extraction.cp` = 70 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.419** strong

- `cells.green-hydrogen-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.420** not enough policy evidence

- `cells.green-hydrogen-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.421** PGM and catalyst refining

- `cells.green-hydrogen-refining.name` = PGM and catalyst refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.422** Refining

- `cells.green-hydrogen-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.423** Not assessed

- `cells.green-hydrogen-refining.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.424** 63

- `cells.green-hydrogen-refining.siv` = 63 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.425** 99

- `cells.green-hydrogen-refining.cp` = 99 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.426** strong

- `cells.green-hydrogen-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.427** not enough policy evidence

- `cells.green-hydrogen-refining.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.428** Electrolyser stacks

- `cells.green-hydrogen-components.name` = Electrolyser stacks | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.429** Components

- `cells.green-hydrogen-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.430** Not assessed

- `cells.green-hydrogen-components.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.431** 99

- `cells.green-hydrogen-components.siv` = 99 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.432** not enough policy evidence

- `cells.green-hydrogen-components.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.433** Electrolyser systems

- `cells.green-hydrogen-assembly.name` = Electrolyser systems | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.434** Assembly

- `cells.green-hydrogen-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.435** Not assessed

- `cells.green-hydrogen-assembly.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.436** 90

- `cells.green-hydrogen-assembly.siv` = 90 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.437** not enough policy evidence

- `cells.green-hydrogen-assembly.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.438** Clean hydrogen production and use

- `cells.green-hydrogen-deployment.name` = Clean hydrogen production and use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.439** Deployment

- `cells.green-hydrogen-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.440** Watch

- `cells.green-hydrogen-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.441** Low

- `cells.green-hydrogen-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence high; deployment-need confidence low

**a.442** 18

- `cells.green-hydrogen-deployment.dn` = 18 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: choice confidence high; deployment-need confidence low

**a.443** broadly aligned

- `cells.green-hydrogen-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence high; deployment-need confidence low

**a.444** Stack and catalyst recycling

- `cells.green-hydrogen-recovery.name` = Stack and catalyst recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.445** Recovery

- `cells.green-hydrogen-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.446** Not assessed

- `cells.green-hydrogen-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.447** 49

- `cells.green-hydrogen-recovery.siv` = 49 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.448** not enough policy evidence

- `cells.green-hydrogen-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.449** Uranium mining

- `cells.nuclear-extraction.name` = Uranium mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.450** Extraction

- `cells.nuclear-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.451** Watch

- `cells.nuclear-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.452** Low

- `cells.nuclear-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.453** 61

- `cells.nuclear-extraction.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.454** 54

- `cells.nuclear-extraction.siv` = 54 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.455** 85

- `cells.nuclear-extraction.cp` = 85 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.456** strong

- `cells.nuclear-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.457** mixed signals

- `cells.nuclear-extraction.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.458** Uranium conversion and enrichment

- `cells.nuclear-refining.name` = Uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.459** Refining

- `cells.nuclear-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.460** Catch Up

- `cells.nuclear-refining.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.461** Medium

- `cells.nuclear-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.462** 61

- `cells.nuclear-refining.need` = 61 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.463** 88

- `cells.nuclear-refining.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.464** 35

- `cells.nuclear-refining.cp` = 35 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.465** strong

- `cells.nuclear-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.466** needs more attention

- `cells.nuclear-refining.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.467** Nuclear fuel and reactor parts

- `cells.nuclear-components.name` = Nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.468** Components

- `cells.nuclear-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.469** Catch Up

- `cells.nuclear-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.470** Medium

- `cells.nuclear-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.471** 74

- `cells.nuclear-components.need` = 74 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.472** 88

- `cells.nuclear-components.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.473** 2

- `cells.nuclear-components.cp` = 2 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.474** strong

- `cells.nuclear-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.475** mixed signals

- `cells.nuclear-components.alignment` = mixed signals | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.476** Reactors and steam generators

- `cells.nuclear-assembly.name` = Reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.477** Assembly

- `cells.nuclear-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.478** Catch Up

- `cells.nuclear-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.479** Medium

- `cells.nuclear-assembly.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.480** 67

- `cells.nuclear-assembly.need` = 67 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.481** 96

- `cells.nuclear-assembly.siv` = 96 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.482** 64

- `cells.nuclear-assembly.cp` = 64 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.483** strong

- `cells.nuclear-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.484** broadly aligned

- `cells.nuclear-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.485** Nuclear power

- `cells.nuclear-deployment.name` = Nuclear power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.486** Deployment

- `cells.nuclear-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.487** Watch

- `cells.nuclear-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.488** Low

- `cells.nuclear-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.489** 28

- `cells.nuclear-deployment.dn` = 28 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.490** strong

- `cells.nuclear-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.491** broadly aligned

- `cells.nuclear-deployment.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.492** Spent-fuel reprocessing

- `cells.nuclear-recovery.name` = Spent-fuel reprocessing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.493** Recovery

- `cells.nuclear-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.494** Not assessed

- `cells.nuclear-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.495** 88

- `cells.nuclear-recovery.siv` = 88 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.496** not enough policy evidence

- `cells.nuclear-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.497** Oil production

- `cells.oil-extraction.name` = Oil production | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.498** Extraction

- `cells.oil-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.499** Preserve

- `cells.oil-extraction.choice` = Preserve | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.500** Medium

- `cells.oil-extraction.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.501** 84

- `cells.oil-extraction.need` = 84 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.502** 25

- `cells.oil-extraction.siv` = 25 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.503** 90

- `cells.oil-extraction.cp` = 90 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.504** strong

- `cells.oil-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.505** broadly aligned

- `cells.oil-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.506** Oil refining

- `cells.oil-refining.name` = Oil refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.507** Refining

- `cells.oil-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.508** Watch

- `cells.oil-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.509** Low

- `cells.oil-refining.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.510** 56

- `cells.oil-refining.need` = 56 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.511** 46

- `cells.oil-refining.siv` = 46 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.512** 32

- `cells.oil-refining.cp` = 32 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.513** strong

- `cells.oil-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.514** broadly aligned

- `cells.oil-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.515** Drilling and pump components

- `cells.oil-components.name` = Drilling and pump components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.516** Components

- `cells.oil-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.517** Watch

- `cells.oil-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.518** Low

- `cells.oil-components.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.519** 52

- `cells.oil-components.need` = 52 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.520** 34

- `cells.oil-components.siv` = 34 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.521** 56

- `cells.oil-components.cp` = 56 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.522** strong

- `cells.oil-components.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.523** broadly aligned

- `cells.oil-components.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.524** Drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = Drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.525** Assembly

- `cells.oil-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.526** Catch Up

- `cells.oil-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.527** Mixed Medium/Low

- `cells.oil-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.528** 62

- `cells.oil-assembly.need` = 62 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.529** 60

- `cells.oil-assembly.siv` = 60 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.530** 37

- `cells.oil-assembly.cp` = 37 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate

**a.531** strong

- `cells.oil-assembly.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.532** needs more attention

- `cells.oil-assembly.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence moderate

**a.533** Oil use

- `cells.oil-deployment.name` = Oil use | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.534** Deployment

- `cells.oil-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.535** Watch

- `cells.oil-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.536** Low

- `cells.oil-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.537** 37

- `cells.oil-deployment.dn` = 37 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.538** strong

- `cells.oil-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.539** too much intervention

- `cells.oil-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence low

**a.540** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.541** Recovery

- `cells.oil-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.542** Partner

- `cells.oil-recovery.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.543** Medium

- `cells.oil-recovery.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.544** 99

- `cells.oil-recovery.need` = 99 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: choice confidence low

**a.545** 16

- `cells.oil-recovery.siv` = 16 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.546** broadly aligned

- `cells.oil-recovery.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.547** PV mineral mining

- `cells.solar-extraction.name` = PV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.548** Extraction

- `cells.solar-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.549** Watch

- `cells.solar-extraction.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.550** Low

- `cells.solar-extraction.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.551** 82

- `cells.solar-extraction.need` = 82 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.552** 17

- `cells.solar-extraction.siv` = 17 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.553** 0

- `cells.solar-extraction.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.554** strong

- `cells.solar-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.555** broadly aligned

- `cells.solar-extraction.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.556** Polysilicon refining

- `cells.solar-refining.name` = Polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.557** Refining

- `cells.solar-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.558** Watch

- `cells.solar-refining.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.559** Mixed Medium/Low

- `cells.solar-refining.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.560** 67

- `cells.solar-refining.need` = 67 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.561** 42

- `cells.solar-refining.siv` = 42 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.562** 0

- `cells.solar-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high

**a.563** strong

- `cells.solar-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.564** too much intervention

- `cells.solar-refining.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high

**a.565** Wafers and PV cells

- `cells.solar-components.name` = Wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.566** Components

- `cells.solar-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.567** Watch

- `cells.solar-components.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.568** Mixed Medium/Low

- `cells.solar-components.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.569** 50

- `cells.solar-components.need` = 50 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.570** 59

- `cells.solar-components.siv` = 59 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.571** 2

- `cells.solar-components.cp` = 2 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.572** moderate

- `cells.solar-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.573** too much intervention

- `cells.solar-components.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.574** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.575** Assembly

- `cells.solar-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.576** Catch Up

- `cells.solar-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.577** Mixed Medium/Low

- `cells.solar-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.578** 36

- `cells.solar-assembly.need` = 36 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.579** 80

- `cells.solar-assembly.siv` = 80 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.580** 54

- `cells.solar-assembly.cp` = 54 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence moderate

**a.581** moderate

- `cells.solar-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.582** broadly aligned

- `cells.solar-assembly.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence moderate

**a.583** Solar power

- `cells.solar-deployment.name` = Solar power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.584** Deployment

- `cells.solar-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.585** Watch

- `cells.solar-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.586** Low

- `cells.solar-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.587** 34

- `cells.solar-deployment.dn` = 34 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.588** strong

- `cells.solar-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.589** too much intervention

- `cells.solar-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.590** PV module recycling

- `cells.solar-recovery.name` = PV module recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.591** Recovery

- `cells.solar-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.592** Not assessed

- `cells.solar-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.593** 44

- `cells.solar-recovery.siv` = 44 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.594** not enough policy evidence

- `cells.solar-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.595** Wind mineral mining

- `cells.wind-extraction.name` = Wind mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.596** Extraction

- `cells.wind-extraction.stage_id` = Extraction | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.597** Not assessed

- `cells.wind-extraction.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.598** 45

- `cells.wind-extraction.siv` = 45 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.599** 50

- `cells.wind-extraction.cp` = 50 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.600** strong

- `cells.wind-extraction.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.601** not enough policy evidence

- `cells.wind-extraction.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.602** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.603** Refining

- `cells.wind-refining.stage_id` = Refining | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.604** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.605** Medium

- `cells.wind-refining.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.606** 69

- `cells.wind-refining.need` = 69 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.607** 58

- `cells.wind-refining.siv` = 58 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.608** 0

- `cells.wind-refining.cp` = 0 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low

**a.609** strong

- `cells.wind-refining.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.610** broadly aligned

- `cells.wind-refining.alignment` = broadly aligned | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence low

**a.611** Blades, towers and nacelle parts

- `cells.wind-components.name` = Blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.612** Components

- `cells.wind-components.stage_id` = Components | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.613** Catch Up

- `cells.wind-components.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.614** Medium

- `cells.wind-components.fence` = Medium | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.615** 45

- `cells.wind-components.need` = 45 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.616** 89

- `cells.wind-components.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.617** 17

- `cells.wind-components.cp` = 17 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.618** moderate

- `cells.wind-components.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.619** needs more attention

- `cells.wind-components.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.620** Wind turbines

- `cells.wind-assembly.name` = Wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.621** Assembly

- `cells.wind-assembly.stage_id` = Assembly | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.622** Catch Up

- `cells.wind-assembly.choice` = Catch Up | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.623** Mixed Medium/Low

- `cells.wind-assembly.yard` = Mixed Medium/Low | posture core cells.yd (yard roll-up) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.624** 50

- `cells.wind-assembly.need` = 50 | posture core cells.need (supply-chain need) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.625** 89

- `cells.wind-assembly.siv` = 89 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.626** 7

- `cells.wind-assembly.cp` = 7 | posture core cells.cp (Competitive Position among contenders) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence moderate; choice confidence high

**a.627** moderate

- `cells.wind-assembly.evidence` = moderate | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.628** needs more attention

- `cells.wind-assembly.alignment` = needs more attention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence moderate; choice confidence high

**a.629** Wind power

- `cells.wind-deployment.name` = Wind power | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.630** Deployment

- `cells.wind-deployment.stage_id` = Deployment | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.631** Watch

- `cells.wind-deployment.choice` = Watch | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.632** Low

- `cells.wind-deployment.fence` = Low | posture core cells choice -> fence (grammar.ts nodeFence) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.633** 41

- `cells.wind-deployment.dn` = 41 | posture detail cells[].need, dn_demand, dn_policy_gap, dn_system, dn_confidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Ember, electricity generation and capacity data (2020-2025) and 2030 Renewable Target Tracker | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.634** strong

- `cells.wind-deployment.evidence` = strong | posture detail cells[].security_rationale, delivery; core cells.ev, conf | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.635** too much intervention

- `cells.wind-deployment.alignment` = too much intervention | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: position evidence strong; choice confidence high; deployment-need confidence high

**a.636** Wind-turbine recycling

- `cells.wind-recovery.name` = Wind-turbine recycling | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.637** Recovery

- `cells.wind-recovery.stage_id` = Recovery | posture detail cells[] (need components and trade) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.638** Not assessed

- `cells.wind-recovery.choice` = Not assessed | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.639** 32

- `cells.wind-recovery.siv` = 32 | posture core cells.prize (Strategic Industrial Value) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | International Energy Agency, World Energy Outlook 2025 (Stated Policies Scenario) and Global Critical Minerals Outlook: demand 2024-2035 | Clean Investment Monitor: clean-technology manufacturing investment and capacity (aggregated), world capital expenditure 2021-22 to 2024-25 | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: choice confidence low

**a.640** not enough policy evidence

- `cells.wind-recovery.alignment` = not enough policy evidence | posture detail cells[].policy (alignment, priority, protection, flag) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Global Trade Alert, New Industrial Policy Observatory: industrial-policy interventions, strength scored by the authors | International Energy Agency, Policies and Measures Database | evidence: choice confidence low

**a.646** Anode materials (natural and synthetic graphite)

- `cells.batteries-refining.layers.bess-anode.name` = Anode materials (natural and synthetic graphite) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.647** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.648** Catch Up

- `cells.batteries-refining.layers.bess-anode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.649** Medium

- `cells.batteries-refining.layers.bess-anode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.650** Cathode active materials and precursors

- `cells.batteries-refining.layers.bess-cathode.name` = Cathode active materials and precursors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.651** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.652** Catch Up

- `cells.batteries-refining.layers.bess-cathode.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.653** Medium

- `cells.batteries-refining.layers.bess-cathode.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.654** Battery-grade lithium hydroxide and carbonate

- `cells.batteries-refining.layers.li-chemicals.name` = Battery-grade lithium hydroxide and carbonate | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.655** battery materials refining

- `cells.batteries-refining.name` = battery materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.656** Catch Up

- `cells.batteries-refining.layers.li-chemicals.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.657** Medium

- `cells.batteries-refining.layers.li-chemicals.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.658** Battery cells (LFP and NMC)

- `cells.batteries-assembly.layers.bess-cells.name` = Battery cells (LFP and NMC) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.659** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.660** Preserve

- `cells.batteries-assembly.layers.bess-cells.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.661** Medium

- `cells.batteries-assembly.layers.bess-cells.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.662** Communications gateway and cloud fleet management

- `cells.batteries-assembly.layers.bess-comms.name` = Communications gateway and cloud fleet management | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.663** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.664** Preserve

- `cells.batteries-assembly.layers.bess-comms.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.665** Medium

- `cells.batteries-assembly.layers.bess-comms.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.666** Battery-management firmware, energy-management software and remote updates

- `cells.batteries-assembly.layers.bess-control.name` = Battery-management firmware, energy-management software and remote updates | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.667** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.668** Preserve

- `cells.batteries-assembly.layers.bess-control.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.669** Medium

- `cells.batteries-assembly.layers.bess-control.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.670** Enclosures, racking, thermal management and balance of system

- `cells.batteries-assembly.layers.bess-enclosure.name` = Enclosures, racking, thermal management and balance of system | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.671** battery cells and packs

- `cells.batteries-assembly.name` = battery cells and packs | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.672** Preserve

- `cells.batteries-assembly.layers.bess-enclosure.choice` = Preserve | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.673** Medium

- `cells.batteries-assembly.layers.bess-enclosure.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.674** Grain-oriented electrical steel

- `cells.electric-grid-refining.layers.goes.name` = Grain-oriented electrical steel | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.675** grid metals refining

- `cells.electric-grid-refining.name` = grid metals refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.676** Catch Up

- `cells.electric-grid-refining.layers.goes.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.677** Medium

- `cells.electric-grid-refining.layers.goes.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.678** Power conversion hardware (inverters and converters)

- `cells.electric-grid-assembly.layers.bess-pcs.name` = Power conversion hardware (inverters and converters) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.679** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.680** Catch Up

- `cells.electric-grid-assembly.layers.bess-pcs.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.681** Medium

- `cells.electric-grid-assembly.layers.bess-pcs.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.682** Smart meters and advanced metering head-end systems

- `cells.electric-grid-assembly.layers.grid-ami.name` = Smart meters and advanced metering head-end systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.683** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.684** Watch

- `cells.electric-grid-assembly.layers.grid-ami.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.685** Low

- `cells.electric-grid-assembly.layers.grid-ami.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.686** Cables and conductors

- `cells.electric-grid-assembly.layers.grid-cables.name` = Cables and conductors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.687** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.688** Watch

- `cells.electric-grid-assembly.layers.grid-cables.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.689** Low

- `cells.electric-grid-assembly.layers.grid-cables.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.690** Utility-scale inverter firmware and remote-access functions

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.name` = Utility-scale inverter firmware and remote-access functions | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.691** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.692** Watch

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.693** Low

- `cells.electric-grid-assembly.layers.grid-inverter-firmware.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.694** Grid-control software (SCADA, EMS, DERMS)

- `cells.electric-grid-assembly.layers.grid-scada.name` = Grid-control software (SCADA, EMS, DERMS) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.695** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.696** Watch

- `cells.electric-grid-assembly.layers.grid-scada.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.697** Low

- `cells.electric-grid-assembly.layers.grid-scada.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.698** Large power transformers

- `cells.electric-grid-assembly.layers.grid-transformers.name` = Large power transformers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.699** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.700** Catch Up

- `cells.electric-grid-assembly.layers.grid-transformers.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.701** Medium

- `cells.electric-grid-assembly.layers.grid-transformers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.702** HVDC converter-station control and protection

- `cells.electric-grid-assembly.layers.hvdc-control.name` = HVDC converter-station control and protection | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.703** transformers, switchgear and cables

- `cells.electric-grid-assembly.name` = transformers, switchgear and cables | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.704** Catch Up

- `cells.electric-grid-assembly.layers.hvdc-control.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.705** Medium

- `cells.electric-grid-assembly.layers.hvdc-control.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.706** Electric drive units and motors

- `cells.electric-vehicles-components.layers.ev-motors.name` = Electric drive units and motors | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.707** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.708** Catch Up

- `cells.electric-vehicles-components.layers.ev-motors.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.709** Medium

- `cells.electric-vehicles-components.layers.ev-motors.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.710** Sintered NdFeB permanent magnets (traction motors)

- `cells.electric-vehicles-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets (traction motors) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.711** EV drivetrain components

- `cells.electric-vehicles-components.name` = EV drivetrain components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.712** Secure

- `cells.electric-vehicles-components.layers.ree-magnets.choice` = Secure | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.713** High

- `cells.electric-vehicles-components.layers.ree-magnets.fence` = High | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.714** Connected-vehicle telematics, over-the-air software and connectivity systems

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.name` = Connected-vehicle telematics, over-the-air software and connectivity systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.715** EV and charger manufacturing

- `cells.electric-vehicles-assembly.name` = EV and charger manufacturing | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.716** Watch

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.717** Low

- `cells.electric-vehicles-assembly.layers.ev-connected-vehicle.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.718** Charging-network management software (charge-point back ends)

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.name` = Charging-network management software (charge-point back ends) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.719** EV adoption and charging

- `cells.electric-vehicles-deployment.name` = EV adoption and charging | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.720** Watch

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.721** Low

- `cells.electric-vehicles-deployment.layers.ev-charging-mgmt.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; deployment-need confidence moderate

**a.722** Pipeline SCADA, compressor-station and LNG-terminal control systems

- `cells.gas-assembly.layers.gas-ot-control.name` = Pipeline SCADA, compressor-station and LNG-terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.723** gas turbines and LNG plant

- `cells.gas-assembly.name` = gas turbines and LNG plant | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.724** Watch

- `cells.gas-assembly.layers.gas-ot-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.725** Low

- `cells.gas-assembly.layers.gas-ot-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.726** Iridium and platinum for PEM electrolyser catalysts

- `cells.green-hydrogen-extraction.layers.h2-pgm.name` = Iridium and platinum for PEM electrolyser catalysts | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.727** electrolyser mineral mining

- `cells.green-hydrogen-extraction.name` = electrolyser mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.728** Not assessed

- `cells.green-hydrogen-extraction.layers.h2-pgm.choice` = Not assessed | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.729** HALEU enrichment

- `cells.nuclear-refining.layers.nuc-haleu.name` = HALEU enrichment | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.730** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.731** Catch Up

- `cells.nuclear-refining.layers.nuc-haleu.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.732** Medium

- `cells.nuclear-refining.layers.nuc-haleu.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.733** LEU enrichment services

- `cells.nuclear-refining.layers.nuc-leu.name` = LEU enrichment services | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.734** uranium conversion and enrichment

- `cells.nuclear-refining.name` = uranium conversion and enrichment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.735** Catch Up

- `cells.nuclear-refining.layers.nuc-leu.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.736** Medium

- `cells.nuclear-refining.layers.nuc-leu.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.737** Fuel assemblies (design-specific fabrication)

- `cells.nuclear-components.layers.nuc-fuel-fab.name` = Fuel assemblies (design-specific fabrication) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.738** nuclear fuel and reactor parts

- `cells.nuclear-components.name` = nuclear fuel and reactor parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.739** Catch Up

- `cells.nuclear-components.layers.nuc-fuel-fab.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.740** Medium

- `cells.nuclear-components.layers.nuc-fuel-fab.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.741** Reactor control and safety instrumentation

- `cells.nuclear-assembly.layers.nuc-reactor-controls.name` = Reactor control and safety instrumentation | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.742** reactors and steam generators

- `cells.nuclear-assembly.name` = reactors and steam generators | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.743** Catch Up

- `cells.nuclear-assembly.layers.nuc-reactor-controls.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.744** Medium

- `cells.nuclear-assembly.layers.nuc-reactor-controls.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.745** Oil pipeline and terminal control systems

- `cells.oil-assembly.layers.oil-ot-control.name` = Oil pipeline and terminal control systems | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence moderate

**a.746** drilling rigs, tankers and oil equipment

- `cells.oil-assembly.name` = drilling rigs, tankers and oil equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.747** Watch

- `cells.oil-assembly.layers.oil-ot-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.748** Low

- `cells.oil-assembly.layers.oil-ot-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.749** Solar-grade polysilicon

- `cells.solar-refining.layers.polysilicon.name` = Solar-grade polysilicon | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence high

**a.750** polysilicon refining

- `cells.solar-refining.name` = polysilicon refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.751** Partner

- `cells.solar-refining.layers.polysilicon.choice` = Partner | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.752** Medium

- `cells.solar-refining.layers.polysilicon.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.753** Solar cells and wafers

- `cells.solar-components.layers.pv-cells-wafers.name` = Solar cells and wafers | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.754** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.755** Catch Up

- `cells.solar-components.layers.pv-cells-wafers.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.756** Medium

- `cells.solar-components.layers.pv-cells-wafers.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.757** Next-generation PV (perovskite and tandem cells)

- `cells.solar-components.layers.pv-nextgen.name` = Next-generation PV (perovskite and tandem cells) | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.758** wafers and PV cells

- `cells.solar-components.name` = wafers and PV cells | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.759** Catch Up

- `cells.solar-components.layers.pv-nextgen.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.760** Medium

- `cells.solar-components.layers.pv-nextgen.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.761** Distributed PV inverter firmware, communication dongles and vendor cloud

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.name` = Distributed PV inverter firmware, communication dongles and vendor cloud | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.762** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.763** Watch

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.764** Low

- `cells.solar-assembly.layers.pv-inverter-firmware-dist.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.765** Conventional crystalline-silicon modules

- `cells.solar-assembly.layers.pv-modules.name` = Conventional crystalline-silicon modules | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.766** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.767** Watch

- `cells.solar-assembly.layers.pv-modules.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.768** Low

- `cells.solar-assembly.layers.pv-modules.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.769** Passive racking and mounting

- `cells.solar-assembly.layers.pv-racking.name` = Passive racking and mounting | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence moderate

**a.770** PV modules and inverters

- `cells.solar-assembly.name` = PV modules and inverters | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.771** Watch

- `cells.solar-assembly.layers.pv-racking.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.772** Low

- `cells.solar-assembly.layers.pv-racking.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence moderate

**a.773** Rare-earth separation and metal-making

- `cells.wind-refining.layers.ree-separation.name` = Rare-earth separation and metal-making | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence strong; choice confidence low

**a.774** rare-earth and wind materials refining

- `cells.wind-refining.name` = rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.775** Catch Up

- `cells.wind-refining.layers.ree-separation.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.776** Medium

- `cells.wind-refining.layers.ree-separation.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.777** Sintered NdFeB permanent magnets

- `cells.wind-components.layers.ree-magnets.name` = Sintered NdFeB permanent magnets | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.778** blades, towers and nacelle parts

- `cells.wind-components.name` = blades, towers and nacelle parts | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.779** Catch Up

- `cells.wind-components.layers.ree-magnets.choice` = Catch Up | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.780** Medium

- `cells.wind-components.layers.ree-magnets.fence` = Medium | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.781** Turbine controllers, OEM remote access and park SCADA

- `cells.wind-assembly.layers.wind-turbine-control.name` = Turbine controllers, OEM remote access and park SCADA | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Yards & Fences node attributes: standardization, learning, industrial commons and control layers, curated by the authors from public sources | evidence: position evidence moderate; choice confidence high

**a.782** wind turbines

- `cells.wind-assembly.name` = wind turbines | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.783** Watch

- `cells.wind-assembly.layers.wind-turbine-control.choice` = Watch | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.784** Low

- `cells.wind-assembly.layers.wind-turbine-control.fence` = Low | posture detail cells[].yard_children; core meta.yf_nodes labels | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence moderate; choice confidence high

**a.791** Battery mineral mining

- `cells.batteries-extraction.name` = Battery mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.792** Partner

- `cells.batteries-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.793** Australia and the United States

- `cells.batteries-extraction.partners` = Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.794** Allied supply covers the need

- `cells.batteries-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.795** moderate

- `cells.batteries-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.796** Coal-plant components

- `cells.coal-components.name` = Coal-plant components | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.797** Partner

- `cells.coal-components.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.798** Japan, Germany and Italy

- `cells.coal-components.partners` = Japan, Germany and Italy | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.799** Allied supply covers the need

- `cells.coal-components.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence high

**a.800** weak

- `cells.coal-components.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence high; partner evidence weak

**a.801** Coal-plant equipment

- `cells.coal-assembly.name` = Coal-plant equipment | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.802** Partner

- `cells.coal-assembly.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.803** France, Germany and Italy

- `cells.coal-assembly.partners` = France, Germany and Italy | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.804** Allied supply covers the need

- `cells.coal-assembly.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.805** weak

- `cells.coal-assembly.partner_evidence` = weak | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence weak

**a.806** EV mineral mining

- `cells.electric-vehicles-extraction.name` = EV mineral mining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.807** Partner

- `cells.electric-vehicles-extraction.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.808** Australia and the United States

- `cells.electric-vehicles-extraction.partners` = Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.809** Allied supply covers the need

- `cells.electric-vehicles-extraction.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.810** moderate

- `cells.electric-vehicles-extraction.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.811** Rare-earth and battery-metal refining

- `cells.electric-vehicles-refining.name` = Rare-earth and battery-metal refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.812** Secure

- `cells.electric-vehicles-refining.choice` = Secure | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.813** Japan, Australia and the United States

- `cells.electric-vehicles-refining.partners` = Japan, Australia and the United States | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.814** Allied supply covers the need

- `cells.electric-vehicles-refining.aib_status` = Allied supply covers the need | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate

**a.815** moderate

- `cells.electric-vehicles-refining.partner_evidence` = moderate | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence moderate; partner evidence moderate

**a.816** Waste-oil recovery

- `cells.oil-recovery.name` = Waste-oil recovery | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.817** Partner

- `cells.oil-recovery.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: choice confidence low

**a.818** No credible allied solution yet

- `cells.oil-recovery.partner_gap` = No credible allied solution yet | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: choice confidence low; partner evidence none

**a.819** none

- `cells.oil-recovery.partner_evidence` = none | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: choice confidence low; partner evidence none

**a.820** Rare-earth and wind materials refining

- `cells.wind-refining.name` = Rare-earth and wind materials refining | posture core meta.phrases / meta.stage_display | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.821** Partner

- `cells.wind-refining.choice` = Partner | posture core cells.p, m, dm (the dashboard's choice) | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.822** Germany, Belgium and Spain

- `cells.wind-refining.partners` = Germany, Belgium and Spain | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | United Nations, UN Comtrade Database, HS6 bilateral trade (all reporters); reconciled by the authors | evidence: position evidence strong; choice confidence low; partner evidence strong

**a.823** Allied supply falls short

- `cells.wind-refining.aib_status` = Allied supply falls short | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence low

**a.824** strong

- `cells.wind-refining.partner_evidence` = strong | posture detail cells[].partners, partner_aib_status, partner_gap, partner_evidence | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: position evidence strong; choice confidence low; partner evidence strong

**a.831** Battery Materials & Cells

- `aib.compacts.SC-BATTERIES.name` = Battery Materials & Cells | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.832** Processing anchor

- `aib.compacts.SC-BATTERIES.role` = Processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.833** Processing anchor and capacity-building host

- `aib.compacts.SC-BATTERIES.functions` = Processing anchor and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.834** Refined chemicals

- `aib.compacts.SC-BATTERIES.lead_step` = Refined chemicals | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.835** the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany

- `aib.compacts.SC-BATTERIES.core` = the United States, the Democratic Republic of Congo, Japan, Indonesia, South Korea and Germany | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.836** Wind & Hydropower Equipment

- `aib.compacts.SC-WIND.name` = Wind & Hydropower Equipment | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.837** Diversification partner

- `aib.compacts.SC-WIND.role` = Diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.838** Demand anchor and diversification partner

- `aib.compacts.SC-WIND.functions` = Demand anchor and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.839** Spain, Poland, India, Germany, the United States and the United Kingdom

- `aib.compacts.SC-WIND.core` = Spain, Poland, India, Germany, the United States and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.840** Nuclear Fuel & Reactor Systems

- `aib.compacts.SC-NUCLEAR.name` = Nuclear Fuel & Reactor Systems | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.841** Processing anchor

- `aib.compacts.SC-NUCLEAR.role` = Processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.842** Resource supplier and processing anchor

- `aib.compacts.SC-NUCLEAR.functions` = Resource supplier and processing anchor | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.843** Fuel & nuclear materials

- `aib.compacts.SC-NUCLEAR.lead_step` = Fuel & nuclear materials | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.844** Australia, Canada, India, the United States, France and the United Kingdom

- `aib.compacts.SC-NUCLEAR.core` = Australia, Canada, India, the United States, France and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.845** Grid Equipment & Conductors

- `aib.compacts.SC-GRID.name` = Grid Equipment & Conductors | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.846** Specialized technology supplier

- `aib.compacts.SC-GRID.role` = Specialized technology supplier | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.847** Specialized technology supplier, scale manufacturer, demand anchor, bridge / coordination partner and diversification partner

- `aib.compacts.SC-GRID.functions` = Specialized technology supplier, scale manufacturer, demand anchor, bridge / coordination partner and diversification partner | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.848** Wire rod & transformer steel

- `aib.compacts.SC-GRID.lead_step` = Wire rod & transformer steel | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.849** Chile, Peru, the Democratic Republic of Congo, Japan, Germany and the United States

- `aib.compacts.SC-GRID.core` = Chile, Peru, the Democratic Republic of Congo, Japan, Germany and the United States | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.850** Electric Vehicles

- `aib.compacts.SC-EV.name` = Electric Vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.851** Scale manufacturer

- `aib.compacts.SC-EV.role` = Scale manufacturer | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.852** Scale manufacturer and capacity-building host

- `aib.compacts.SC-EV.functions` = Scale manufacturer and capacity-building host | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.853** Electric vehicles

- `aib.compacts.SC-EV.lead_step` = Electric vehicles | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.854** Germany, the United States, France, Japan, South Korea and the United Kingdom

- `aib.compacts.SC-EV.core` = Germany, the United States, France, Japan, South Korea and the United Kingdom | AIB strategic.compactRoles and strategic.compacts[].core | Allied Industrial Base model: roles, compacts, trade restored and new capacity across the allied coalition, computed by the Explorer's authors | evidence: AIB model run

**a.859** 30 October 2026

- `meta.as_of_label` = 30 October 2026 | Country manifest as_of_label | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.861** 48

- `counts.assessed` = 48 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.863** 18

- `counts.not_assessed` = 18 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.865** 34

- `counts.layers_assessed` = 34 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.867** 37

- `counts.ev_high` = 37 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.869** 10

- `counts.ev_partial` = 10 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived

**a.871** 19

- `counts.ev_none` = 19 | derived: counts over the posture core and detail | Country strategy choices by technology and supply-chain step (supply-chain need, Strategic Industrial Value, Competitive Position, deployment need, policy alignment), computed by the Explorer's authors | evidence: derived
