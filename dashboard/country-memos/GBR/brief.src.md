## Bottom line — the United Kingdom's strategic role

The United Kingdom's role in the Allied Industrial Base is as a demand and market anchor: allies count on it more as a buyer of turbines, vehicles and reactor parts than as a supplier. Its core seats in {{aib.core_in|nw}} strategic compacts are led by demand roles, and its main supply contribution is uranium conversion and enrichment. Nationally, no producing step reaches the top quarter of producers, so nothing reads Preserve. The industrial task is to catch up in the vehicle chain, where the anchors are, and to source battery cells and rare-earth materials from trusted partners, with development cooperation adding cell capacity now and preparing rare-earth capacity later. Storage, wind and grid networks need to be built faster. The high fence belongs around gas and oil supply, the rare-earth chain, and the control software of batteries and rooftop inverters.

:::choices
- **Secure.** Gas and oil supply, which is essential and imported and takes {{cells.gas-extraction.reconstitution}} to rebuild; {{cells.gas-extraction.top_supplier}} alone provides {{cells.gas-extraction.top_supplier_share}} of gas imports. The rare-earth chain, where {{cells.electric-vehicles-refining.coercive_top}} holds {{cells.electric-vehicles-refining.coercive_top_share}} of world refining supply. The control software of battery systems and distributed PV inverters. The answer is stocks, trusted supply and control of command paths, not self-sufficiency.
- **Build / preserve.** No step qualifies for Preserve. Catch Up in EV and charger manufacturing, EV drivetrain components and battery components. Meet the high need for reactors mainly with allied makers, and keep conversion and enrichment reliable for allies.
- **Partner.** Battery cells and packs from producers such as {{cells.batteries-assembly.partners}}, and rare-earth and wind materials from {{cells.wind-refining.partners}}, where allied supply falls short; minerals from {{cells.batteries-extraction.partners}}. With developing partners, add cell capacity now and prepare rare-earth capacity later.
- **Deploy / stay open.** Accelerate storage, wind power and grid networks, buying from trusted suppliers with security conditions on connected equipment. Leave open grid components, oil refining, wafers and PV cells, gas-turbine equipment and the commodity mining steps.
:::

In Figure 1, Secure sits in fuel extraction and rare-earth refining, Catch Up spreads across the manufacturing columns, and Partner covers minerals and battery cells. The orange corner flags mark Secure software and magnet layers inside other steps.

@figure strategy_map
Each cell is a supply-chain step of a technology, colored by its strategic choice. A corner flag marks a mixed step whose named parts sit behind a different fence: in {{cells.batteries-assembly.name}}, the battery software is Secure, the cells Partner and the enclosures Watch. A dotted outline marks a review case, a call resting on low confidence, a review flag, a thin anchor or a known data artifact; this brief does not turn those calls into recommendations. Steps marked n.a. lack the data for a call.

## Where the United Kingdom should secure, compete and stay open

### High fence — what cannot fail

Secure covers {{counts.secure|nw}} whole steps and {{counts.secure_layers|nw}} layers inside other steps, grouped into {{counts.high_capabilities|nw}} capabilities. Gas and oil are Secure for system resilience, not coercion: they are essential, hard to substitute and slow to rebuild, though most imports come from suppliers not flagged as potentially coercive.

| Capability | Why it matters | Main vulnerability | Recommended response |
|---|---|---|---|
| Gas supply | Supply-chain need is {{high_fence.unit_gas-extraction.need}} out of a hundred; gas takes {{cells.gas-extraction.reconstitution}} to rebuild. | {{cells.gas-extraction.top_supplier|cap}} supplies {{cells.gas-extraction.top_supplier_share}} of imports; potentially coercive suppliers only {{cells.gas-extraction.coercive_import_share}}. The risk is concentration and rebuild time. | **Allied.** Long-term and capacity-reservation contracts with trusted suppliers, such as {{cells.gas-extraction.partners}} (a list that can include trading hubs); stockholding obligations sized to the rebuild time; stress tests. |
| The rare-earth chain: separation, metals and magnets | Feeds the magnets of EV motors and wind generators; the United Kingdom holds {{cells.electric-vehicles-refining.share}} of world refining supply. | {{cells.electric-vehicles-refining.coercive_top}} holds {{cells.electric-vehicles-refining.coercive_top_share}} of world refining supply and trusted suppliers {{cells.electric-vehicles-refining.trusted_share}}. Rebuilding takes {{cells.electric-vehicles-refining.reconstitution}}. | **Mixed.** New trusted capacity with allies such as {{cells.electric-vehicles-refining.partners}}, through co-investment and offtake; at home only where announced projects can qualify; stockpiles until it qualifies. |
| Oil supply | Supply-chain need is {{high_fence.unit_oil-extraction.need}}; oil takes {{cells.oil-extraction.reconstitution}} to rebuild. | {{cells.oil-extraction.top_supplier|cap}} supplies {{cells.oil-extraction.top_supplier_share}} of imports; trusted suppliers hold {{cells.oil-extraction.trusted_share}} of world supply, and allied supply falls short. | **Diversified.** Stocks sized to the rebuild time; capacity reservations with several trusted producers, such as {{cells.oil-extraction.partners}}; demand-side substitution. |
| Control software of battery systems and PV inverters | Battery-management, energy-management and inverter firmware can command or disable storage and rooftop solar from abroad. | {{cells.batteries-assembly.coercive_top}} supplies {{cells.batteries-assembly.china_share}} of battery cell and pack imports and {{cells.solar-assembly.china_share}} of PV module and inverter imports: hardware shares, not software. | **Trusted control.** Trusted-source and certification rules for firmware, communications and remote access; limits on over-the-air updates by untrusted vendors; replacement of untrusted control in the installed base. |

Grid, turbine, vehicle, and oil and gas control software mostly reads Watch. The Secure software layers sit in the hardware bought most from potentially coercive suppliers: {{cells.batteries-assembly.coercive_import_share}} of battery cell and pack imports and {{cells.solar-assembly.coercive_import_share}} of PV equipment, against {{cells.electric-grid-assembly.coercive_import_share}} for transformers and switchgear. These shares measure hardware, not software vendors, so basic security standards should still apply and the calls should be re-tested as suppliers change.

### Medium fence — where industrial capability matters

**Preserve.** There is no Preserve step, because no producing step reaches the top quarter of producers. The highest positions, {{cells.gas-components.cp}} in {{cells.gas-components.name}} and {{cells.batteries-assembly.cp}} in {{cells.batteries-assembly.name}} (a Partner step with no capability anchor), sit below the bar of {{meta.bars.cp}}. The United Kingdom competes in many steps but leads in none, so its gains must be built or partnered, not defended.

**Catch Up.** The vehicle chain is where the anchors are. {{cells.electric-vehicles-assembly.name|cap}} rests on {{cells.electric-vehicles-assembly.anchors|nw}} anchors, including operating capacity, with {{cells.electric-vehicles-assembly.export_share}} of world exports, a position of {{cells.electric-vehicles-assembly.cp}} and {{cells.electric-vehicles-assembly.confidence}} confidence. {{cells.electric-vehicles-components.name|cap}} ({{cells.electric-vehicles-components.cp}}, which overlaps the cells node) and {{cells.batteries-components.name}} ({{cells.batteries-components.cp}}, with committed investment) sit beside it. Delivery is mixed: build at home where the base is established and with allied partners for the rest.

In nuclear, {{cells.nuclear-refining.name}} rests on a share of world supply of {{cells.nuclear-refining.share}}, close to the AIB's measure of the product. Its exports of the step's trade line are {{cells.nuclear-refining.export_share}} of world exports, so verify the capacity before support scales. In {{cells.nuclear-assembly.name}}, need ({{cells.nuclear-assembly.need}}) and value are high, but the basket mixes reactor parts with generic boiler and steam-turbine parts, so the build should run mainly through allied makers. The call on {{cells.nuclear-components.name}} is a review case, on {{cells.nuclear-components.review}}.

Other Catch Up calls are weaker than they look, and this brief does not build on them:

- **Wind turbines:** shares of {{cells.wind-assembly.share}} in a step whose trade codes are mostly cables, transformers and switchgear; need is low ({{cells.wind-assembly.need}}).
- **Blades, towers and nacelle parts, and PV modules and inverters:** low positions ({{cells.wind-components.cp}} and {{cells.solar-assembly.cp}}) and low need; the PV codes include chip-industry lines.
- **Transformers, switchgear and cables:** value just over the bar ({{cells.electric-grid-assembly.siv}}), low need ({{cells.electric-grid-assembly.need}}) and a position of {{cells.electric-grid-assembly.cp}}.
- **Battery materials refining:** a single export-share anchor of about {{cells.batteries-refining.export_share}} and no measurable position. Its {{cells.batteries-refining.coercive_top}} share of world supply, {{cells.batteries-refining.coercive_top_share}}, matches the Secure refining step's, which suggests overlapping trade codes.
- **Battery recycling:** an export share of {{cells.batteries-recovery.export_share}} in a step whose trade codes are battery waste, scrap and residues.

**Partner.** A major gap is in {{cells.batteries-assembly.name}}: need is {{cells.batteries-assembly.need}}, {{cells.batteries-assembly.coercive_top}} provides {{cells.batteries-assembly.china_share}} of imports and allied supply falls short. With {{cells.batteries-assembly.share}} of world supply and no capability anchor, dependence should be deliberate and spread across producers such as {{cells.batteries-assembly.partners}}. In {{cells.wind-refining.name}} allied supply also falls short; its rare-earth separation layer reads Partner too. Coal-plant equipment and components read Partner on high need alone; allied supply covers it, and coal capacity fell from {{cells.coal-deployment.deployment_trend}}.

**Shared capabilities.** The table keeps the national reading of capabilities that sit in several chains apart from what allies rely on the United Kingdom for.

@table shared_table

@figure value_position
Producing steps with both measures (recovery has no position measure). Dashed lines mark the value bar ({{meta.bars.prize}}) and the top quarter ({{meta.bars.cp}}). No step reaches the upper right. The vehicle and battery steps cluster in the upper middle, short of the top-quarter line, and gas-turbine and LNG equipment sits at the same height just left of the value bar. Hollow markers are review cases.

### Low fence — what not to prioritize

Watch is not a failure: it means affordable, diversified access matters more than domestic production.

- **Grid components and oil refining.** Need is low ({{cells.electric-grid-components.need}} and {{cells.oil-refining.need}}), yet both rank near the top of the country's own recorded protection ({{cells.electric-grid-components.protection}} and {{cells.oil-refining.protection}}). Buy them competitively.
- **Wafers and PV cells.** Need is low ({{cells.solar-components.need}}) and value rests on market size alone. The cell and next-generation PV layers read Catch Up, but they inherit the step's anchors, whose trade codes include chip wafers.
- **Gas turbines, LNG plant and their components.** Positions of {{cells.gas-assembly.cp}} and {{cells.gas-components.cp}}, among the country's highest, rest on low need and value just under the bar: these makers compete without a fence.
- **Uranium, copper and bauxite, and PV mineral mining.** Need is high ({{cells.nuclear-extraction.need}}, {{cells.electric-grid-extraction.need}} and {{cells.solar-extraction.need}}), but these are large, commoditized yards: diversified trusted imports, with at most a limited, stated capability floor.

Grid metals refining and polysilicon refining are not Low-fence cases: their grain-oriented electrical steel and solar-grade polysilicon layers read Partner.

## The United Kingdom's allied bargain

**What the United Kingdom contributes.** In the Allied Industrial Base analysis the United Kingdom is a {{aib.role}}. It holds a role in {{aib.n_compact_roles|nw}} of the {{aib.n_compacts|nw}} strategic compacts and core seats in {{aib.core_in}}, each led as a demand anchor. Its supply roles are narrow, apart from host seats: {{aib.contributes.uranium-conversion-and-enrichment.share}} of world exports or production of uranium conversion and enrichment, and, in {{aib.compacts.SC-INDUSTRY.name}}, a {{aib.compacts.SC-INDUSTRY.role}} seat with {{aib.compacts.SC-INDUSTRY.lead_share}} of world exports or production of furnace parts.

**What the United Kingdom needs.** Battery cells, rare-earth and wind materials, and the turbines, vehicles and reactor parts its demand seats describe. If {{cells.batteries-assembly.coercive_top}}'s supply stopped, as in the {{aib.scenarios.decoupling.label}} scenario, the United Kingdom would lose {{aib.lost_usd}} a year of imports across the Allied Industrial Base basket. New capacity at the best-placed allied hosts, led by {{aib.investment_decoupling.top_host}}, would restore about {{aib.investment_decoupling.total}} of it a year. Under {{aib.scenarios.managed.label}}, the default scenario, it sits in the minimum coalition of {{aib.ws_managed|nw}} Sectoral Compacts, and of {{aib.ws_decoupling|nw}} under {{aib.scenarios.decoupling.label}}.

**The dependencies to make intentional.** The United Kingdom's bargaining strength is its market. These bargains turn that demand into trusted capacity at home and with allies.

| Capability / compact | The United Kingdom contributes | The United Kingdom needs | Natural partners | What makes the bargain work |
|---|---|---|---|---|
| {{aib.compacts.SC-EV.name}} and {{aib.compacts.SC-BATTERIES.name}} | A large vehicle market and a vehicle base ({{cells.electric-vehicles-assembly.export_share}} of world exports of EVs and chargers) | Battery cells and packs, where allied supply falls short | The EV compact's makers, {{aib.compacts.SC-EV.core_makers}}; for cells, the battery compact's maker, {{aib.compacts.SC-BATTERIES.core_makers}} | For vehicles and drivetrains, joint ventures that transfer capability; for cells, offtake and co-investment |
| {{aib.all_compacts.SC-MAGNETS}} | Demand for magnets in vehicles and wind | Trusted refined rare earths and magnets | Refiners such as {{cells.electric-vehicles-refining.partners}} | Co-investment and offtake for new trusted capacity; stockpiles until it qualifies |
| {{aib.compacts.SC-NUCLEAR.name}} | Uranium conversion and enrichment ({{aib.contributes.uranium-conversion-and-enrichment.share}}), with a core seat | Reactors and reactor parts | The compact's makers, {{aib.compacts.SC-NUCLEAR.core_makers}} | Co-investment and offtake with allied makers; reliable conversion and enrichment for allies |
| {{aib.compacts.SC-WIND.name}} and {{aib.compacts.SC-GRID.name}} | A large market for turbines and grid equipment | Wind materials, where allied supply falls short; turbines and transformers | {{cells.wind-refining.partners}} for materials; the makers {{aib.compacts.SC-WIND.core_makers}} (wind) and {{aib.compacts.SC-GRID.core_makers}} (grid) | Offtake and co-investment; trusted-supplier conditions on connected equipment |

@figure aib_roles
Each row is a strategic compact in which the United Kingdom holds a role or relies on partners. Dark cells are roles allies cannot easily replace; light cells are roles others could fill; dashed cells mark where the United Kingdom relies on trusted partners, orange where allied supply still falls short of what allies need. In the nuclear row, the share printed in the supplier cell is the AIB's share of world imports of reactor parts, a measure of demand, not of supply.

Figure 3 shows an economy that anchors demand across the clean-energy compacts and supplies in few of them. Where it relies on partners for battery materials and cells and for wind, allied supply falls short, so the bargain has to add capacity, not only redirect trade.

## Development strategy — build trusted capacity by helping partners Catch Up

Development cooperation can be the development arm of that bargain. A partnership pairs a British step that needs more trusted capacity with a partner whose same step is a credible Catch Up inside the allied trust screen. {{dev.n_pairs}} pairs qualify, with {{dev.n_partners}} developing partners across {{dev.n_steps}} steps.

The United Kingdom's own supply chain supports {{dev.n_own_pairs}} pairs with {{dev.n_own_partners}} partners across {{dev.n_own_steps|nw}} steps, and the table leads with them: {{dev.own_steps}}. Another {{dev.n_allied_pairs}} rest on the allied gap alone, a reason that is the same for every donor in scope. {{dev.rows.r1.partner}}, an {{dev.rows.r1.income}} economy with a position of {{dev.rows.r1.cp}} in cells and packs, suits finance, guarantees and offtake. {{dev.rows.r2.partner}}'s case rests on a single export-share anchor and a position of {{dev.rows.r2.cp}}, so groundwork comes first. The coal pairs rest on need that allied supply already covers, so {{dev.rows.r3.partner}} is a weak own-need case. The pairs with {{dev.rows.r4.partner}} and {{dev.rows.r5.partner}} rest on the allied gap alone.

@table development

A partnership is more likely to last when it rests on more than aid. The second table shows what ties the United Kingdom to each partner today and what would hold the relationship as the capability scales. Aid is the {{dev.aid_years}} total; the direct investment position is economy-wide, not tied to the step.

@table development_ties

Battery materials refining is among the broadest strands, level with battery components and EV drivetrain components, with {{dev.by_step.batteries-refining.n_development}} developing economies each; all rest on the allied gap alone. For {{dev.none_steps}}: No credible development-led supply solution identified. In rare-earth refining, {{dev.by_step.electric-vehicles-refining.hosts}} host new capacity in the AIB, a commercial route; the other candidates rest on battery-metal capacity data or a thin anchor. Another {{dev.n_commercial}} pairs are commercial, where trade, offtake and investment fit better than aid.

Today's aid follows this map only loosely, and none of it at a partnership step is technology-coded. Of the partners, {{dev.aid_stage_wide|n}} receive British aid at the step, only under stage-wide codes; {{dev.aid_wrong_stage|n}} receive it at other steps and {{dev.aid_none|n}} at no supply-chain step. The largest recipient of energy, industry and mining aid, {{dev.aid_top_1}} ({{dev.aid_top_1_usd}} over {{dev.aid_years}}), has a Catch Up case in {{dev.by_step.electric-vehicles-components.name}} on a single supply-share anchor, but none of that aid sits at the step.

## Energy-system priorities

Wherever the equipment is made, battery storage, wind power and grid networks clear the Accelerate bar of {{meta.bars.deployment}}. Each rests on all {{cells.batteries-deployment.dn_families|nw}} measures of deployment need with {{cells.batteries-deployment.dn_confidence}} confidence, though not on demand. System need drives storage and the grid ({{cells.batteries-deployment.dn_system}} and {{cells.electric-grid-deployment.dn_system}}), and the gap to national targets drives wind ({{cells.wind-deployment.dn_policy_gap}}). The renewables target of {{cells.wind-deployment.target.target_gw}} by {{cells.wind-deployment.target.target_year}} compares with {{cells.wind-deployment.target.installed_gw}} installed in {{cells.wind-deployment.target.installed_year}}, a shortfall of about {{cells.wind-deployment.target.shortfall_gw}} at the recent build rate. Between {{cells.wind-deployment.deployment_years}}, wind grew from {{cells.wind-deployment.deployment_trend}} and storage from {{cells.batteries-deployment.deployment_trend}}.

@figure deployment
Deployment need by technology, with the largest of its measures: demand adequacy, the gap to national targets, or system need. Review cases, which rest on a single indicator of low confidence, are shown apart in grey and are not deployment priorities.

High deployment priority does not imply high manufacturing priority: storage depends on partner cells and the turbine Catch Up on cable and switchgear trade, so equipment should come from whichever trusted supplier is ready. Solar power ({{cells.solar-deployment.dn}}), EV adoption ({{cells.electric-vehicles-deployment.dn}}) and coal power ({{cells.coal-deployment.dn}}) sit just below the bar. The coal reading rests on target-gap and system measures while coal capacity fell to zero, so it is no case for coal. Clean hydrogen and oil use are review cases, each on a single indicator of low confidence.

## Is current policy pointed in the right direction?

Policy never decides a choice here; it is compared with it afterwards, using the United Kingdom's own recorded measures. Protection and priority are ranks within the country, not levels.

@table policy_gaps

Government should change course in several places. First, gas and oil supply are Secure but rank low among the country's recorded measures ({{cells.gas-extraction.policy_priority}} and {{cells.oil-extraction.policy_priority}}). That shows little industrial-policy attention, not missing stocks, so test stocks and contracts against the rebuild time. Second, deployment policy lags for wind and grid networks. Next, protection runs ahead of the case in Watch steps such as oil refining, grid components and wafers and PV cells, and in EV mineral mining, a Partner step that needs partnership terms instead. In grid metals refining ({{cells.electric-grid-refining.protection}}), the data cannot tell whether the measures target electrical steel, a Partner layer, or the wider basket. Finally, battery materials refining reads as needing more attention, but on a thin anchor: test the announced projects before adding support.

## Strategic agenda — what should the United Kingdom do next?

### 1. Hold fuel security through stocks and trusted contracts

Gas and oil are Secure for resilience: they are essential and imported, and supply takes {{cells.oil-extraction.reconstitution}} to rebuild. Size stocks to that time, sign long-term and capacity-reservation contracts with trusted suppliers such as {{cells.gas-extraction.partners}} for gas and {{cells.oil-extraction.partners}} for oil, and stress-test the loss of the largest supplier. Partner evidence is {{cells.gas-extraction.partner_evidence}}, so test the terms.

### 2. Secure the rare-earth chain with allied partners

Add trusted separation, metal and magnet capacity with allies such as {{cells.electric-vehicles-refining.partners}} through co-investment and offtake, and at home only where announced projects can qualify. Hold stocks until trusted supply qualifies, and phase in limits on adversary sources in critical uses as it does. No developing partner qualifies in this step; {{dev.by_step.electric-vehicles-refining.hosts}} host new capacity on commercial terms.

### 3. Catch up in the vehicle chain and partner for battery cells

Tie time-limited production incentives, first-of-a-kind finance, and workforce and supplier development to benchmarks in EV and charger manufacturing, drivetrain components and battery components, with the gate open to joint ventures that transfer capability. Source cells from producers such as {{cells.batteries-assembly.partners}} through offtake and co-investment, including with the battery compact's maker, {{aib.compacts.SC-BATTERIES.core_makers}}.

### 4. Meet reactor needs with allied makers and keep enrichment reliable

Reactors combine high need with a doubtful measured base, so meet that need through co-investment and offtake with the nuclear compact's makers, {{aib.compacts.SC-NUCLEAR.core_makers}}, rather than a domestic build alone. Keep conversion and enrichment reliable for allies, and verify the capacity behind them before support scales.

### 5. Build trusted cell and rare-earth capacity with developing partners

Start with {{dev.rows.r1.partner}} in {{dev.rows.r1.capability}}, through development finance, guarantees and offtake. Prepare {{dev.rows.r2.partner}} in {{dev.rows.r2.capability}} over a longer horizon, with project preparation, infrastructure and skills. Leave the coal pairs aside, since allied supply already covers that need. The objective is both an additional trusted supplier and a durable capability in the partner economy.

### 6. Accelerate storage, wind and grid while keeping sourcing open

Deploy faster through infrastructure, permitting and procurement, buying from trusted suppliers such as the wind compact's makers, {{aib.compacts.SC-WIND.core_makers}}. Require certified, trusted firmware and remote access for battery systems and distributed PV inverters, and replace untrusted control in the installed base on a deadline. Shift protection from Watch steps, such as grid components and oil refining, to deployment.

## Appendix — evidence and method

**How this brief is built.** Every number comes from the Country page data of the Allied Industrial Base Explorer (the data release of {{meta.as_of_label}}) and from the Allied Industrial Base analysis. Each technology is split into supply-chain stages from extraction to recycling, and each step gets a strategic choice by fixed rules applied in order:

- Secure where an acute vulnerability is measured;
- at deployment, Accelerate where deployment need reaches {{meta.bars.deployment}};
- Watch where the step is a large, commoditized yard;
- Preserve where Competitive Position is in the top quarter ({{meta.bars.cp}}) and either Strategic Industrial Value (bar {{meta.bars.prize}}) or supply-chain need (bar {{meta.bars.need}}) is high;
- Catch Up where the value is high, the position is not, and a capability anchor is corroborated by other evidence;
- Partner where supply-chain need is high but there is neither a top-quarter position nor, for a high-value step, a credible pathway;
- Watch otherwise.

Named layers inside a step, such as control software or magnets, take their own choice, and a step whose parts sit behind different fences is mixed. A value that rests on market size alone is capped just under the bar. Observed policy never decides a choice.

**Review cases.** Some calls rest on low confidence, an engine review flag, a thin capability anchor, a known data artifact or, at deployment, a single indicator of low confidence. They stay on the map but never enter the Bottom line, the Agenda, the policy table or a headline mark. {{counts.review|cap}} steps are review cases.

@table review_table

**Evidence and caveats.** The AIB status "allied supply covers the need" for rare-earth and battery-metal refining describes the allied analysis, not ample trusted supply: trusted suppliers hold {{cells.electric-vehicles-refining.trusted_share}} of world supply. In the AIB, the lead shares of the United Kingdom's demand-anchor seats measure world imports, not supply. {{counts.not_assessed|cap}} steps lack the data for a call, including most of geothermal, green hydrogen manufacturing and most recycling. Recorded policy ranks electrolyser stacks ({{cells.green-hydrogen-components.policy_priority}}) and systems ({{cells.green-hydrogen-assembly.policy_priority}}) near the top of the country's own measures, a disagreement this brief does not settle. Competitive Position ranks the United Kingdom among economies that already supply a step, so a zero means no measurable position. Partner lists name where allied capacity sits, which can include trading hubs, not who owns it.

**Development partnerships.** A pair joins a British step that needs more allied capacity, for the United Kingdom's own need or for the allied gap alone, with a developing economy whose same step is a credible Catch Up inside the AIB trust screen. Where a step's data describe different products, the partner's anchor must measure the product the need is about. A partner that already hosts the step's new capacity, or Preserves it, is a commercial case. Aid is energy, industry and mining aid in the OECD Creditor Reporting System, by purpose code; many codes name a stage, not a technology. Export-credit finance is reported apart, never as aid.

@table development_table

**Every step.**

@table strategy_table

**Named layers.**

@table layer_table

**Partner recommendations.**

@table partner_table

**Compact roles.**

@table compact_table

**Coverage and freshness.**

@table coverage_table
