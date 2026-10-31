## What the Sector pages show

The Sector pages show the measured data that the country rankings start from: who produces,
holds, trades, invests in and regulates each energy technology, along six supply-chain stages:
extraction (mining and fuels), refining (processed materials), components (parts), assembly (finished
equipment), deployment (installation and use) and recovery (recycling). Eleven technologies at six
stages make 66 sectors; Semiconductors are shown as well at their three trade stages (refining,
components and assembly), outside the headline set. Every number is shown in its source's own unit and
year, with the source, the coverage and the caveats beside it. Apart from the "What each country should do"
sections, which show the country strategy's recommendation at fixed thresholds, nothing
here is an index, a weight or a score, and nothing moves with the Perspective dials of the Country
profile.

## Reading the colours

Maps, tiles and heat tables use one three-colour scale: **orange** for the lower values, **white** for
the middle one (the median of the values shown), **blue** for the higher values. Colours follow the
rank among the values on screen, so a map of a very uneven measure still shows the differences between
the middle economies instead of painting almost everything one shade. For a measure that can be below
zero (a net balance, net trade, a change), white is zero: orange below, blue above. Hatching means there
is no value to colour; hover or open the table for the reason. Groups of economies keep their own
colours everywhere: China, the coalition core, the coalition partners and the rest of the world.

The Country profiles rank and weigh; the Sector pages show the measured data those rankings start from.

## Shares, world totals and concentration

All shares, ranks and concentration statistics are computed within one measure, one source and one
year. Values from different sources or years are never added or compared on one axis.

* **Share of world.** An economy's value divided by the world total of that measure. The world total
  comes from the source when it reports one (Energy Institute world rows, USGS world totals, the IEA's
  totals, the Red Book), or is the sum of every economy when the data cover them all (the reconciled
  Comtrade matrix, the Clean Investment Monitor). When a source lists only some economies, the share is
  called "of the economies listed by" that source, never a world share.
* **Not attributed.** The part of the world total that the source does not assign to a named economy
  (its "Rest of world" or "Other"). It is drawn as a hatched band and labelled.
* **Largest holder and top three.** The share of the largest holder, and the combined share of the
  three largest.
* **Concentration index (HHI).** The Herfindahl-Hirschman index: take each holder's share of the world
  total, square it and add up the squares. If one economy held everything, the index would be 1; ten
  equal holders give 0.10 and four give 0.25, so lower means more spread out. When more than 5% of the
  total is not attributed, the residual cannot be split, so the index is a lower bound and is written
  "at least" (for example "≥ 0.21").
* **Effective number.** One divided by the concentration index: the number of equal-sized holders
  that would give the same concentration. An index of 0.25 behaves like four equal holders, however many
  small ones there are.
* **Coalition share.** The combined share of the coalition's {members_n} members: {core_n} core members
  and {partner_n} partners. China, the coalition core, the coalition partners and the rest of the world
  always keep the same colours.

Concentration is described, not graded: no thresholds are drawn on the charts and no colour means
"good" or "bad". The explainer on each Trade section quotes the thresholds competition authorities use
for mergers (for example "highly concentrated" above 0.18 in the 2023 US merger guidelines) as a
yardstick only: they were written for firms competing in one market, not for trade between whole
economies.

## The EU as one bloc

By default, the 27 EU members are shown as countries, as the sources report them. With "EU as one
bloc" switched on, the members become one row, "European Union (27)", counted in the coalition core:

* additive measures (trade, investment, production, reserves, capacity, policy counts) are summed over
  the members;
* trade first removes the flows between EU members, using the bilateral data, so EU exports are the
  members' exports to non-members;
* when a source reports its own EU figure (the Energy Institute, the IEA's manufacturing entities, the
  Climate Action Tracker, the IEA policy database), that figure is used as reported;
* ratios, prices, wages, interest rates and technology readiness are not defined for the bloc and stay
  per member.

{eu_outside_sentence}

## Trade

Every trade number on screen comes from one reconciled matrix of bilateral UN Comtrade flows: what the
importer declared (cost, insurance and freight), or, where the importer did not declare, what the
exporter reported multiplied by 1.06 (a "mirror" flow). Totals, shares, rankings, flows and the EU
bloc all sum this one matrix, so the cards on a page agree to the dollar.

* **Technology attribution.** Each technology's trade is the trade of a basket of six-digit HS codes.
  Some codes belong to more than one technology, so trade is never added across technologies.
* **The other basis.** The country indices rank the exporters' own reports (free on board), which leave
  out economies that do not report and Hong Kong's re-exports. The reconciliation table below compares
  the two bases for every technology. Revealed comparative advantage is the one exporter-reported
  measure shown here, and it is labelled so.
* **Re-export hubs.** Hong Kong and Macao stay in the matrix, tagged "re-export hub"; importers record
  the country of origin, so their share there is small.
* **Baskets.** Each six-digit code sits at the stage of what it physically is: refined battery
  chemicals, polysilicon and electrical steel are refining, cells and blades components, modules and
  turbines assembly. Electric Grid's material baskets are copper and aluminium (ores at extraction,
  refined metal at refining); HS 851762 (communication equipment) is outside its manufacturing
  baskets. Excluded codes are listed with each technology's baskets.
* **Material baskets (2026-10 review).** Electric Vehicles, Solar, Wind, Nuclear and Semiconductors
  have material baskets: the battery minerals and magnet rare earths as ores and concentrates
  (Electric Vehicles, extraction) and as compounds and metals (refining), silicon metal (Solar,
  refining), rare-earth ore (Wind, extraction) and compounds and metals (refining), uranium ores
  (Nuclear, extraction) and natural uranium compounds (refining), and gallium, germanium and indium
  (Semiconductors, refining). Each profile states what its basket holds and leaves out.
* **Sub-components.** A basket is built from groups of codes, such as cells and modules in solar.
  Every Trade section can be viewed by sub-component: the exporter, net-trade and 2020-2024 cards and
  the ranking then show one group, on the same reconciled basis, so a sector's sub-components add up to
  its total. Bilateral flows stay whole-basket. A code shared with another sector counts in both, so
  sub-components are never added across sectors.
* **Export feasibility.** From the Harvard Atlas of Economic Complexity (2023): how much of the
  know-how a sector's products need an economy already shows in what it exports well (the Atlas's
  "relatedness", 1 minus its product-space "distance", averaged over the sector's four-digit headings).
  0 means none of the related capabilities are in place, 1 means all of them. It is a different source
  and year from the Comtrade matrix, so it has its own card; it describes capabilities, not costs,
  demand or policy, and it is not a forecast.

## Cross-border manufacturing projects

The components and assembly pages of Solar, Wind, Batteries and Electric Vehicles also show the
Cleantech Manufacturing FDI Dataset (CM-FDI) of the Net Zero Industrial Policy Lab, Johns Hopkins
University (P. Bandara and the Net Zero Industrial Policy Lab, edition 2026.10.05, licensed under
CC BY 4.0): one record per cross-border greenfield or expansion project, announced 1996-2026, with its
parent company, the economy where the parent is headquartered, the destination, the status and a
source link per field. A project makes parts and finished equipment alike, so both stages show the same
projects, and every sum counts a project once.

* **What is counted.** Projects, and the capital a project announced where it is disclosed. An
  undisclosed amount reads "not disclosed"; it is never counted as 0, and every sum says how many
  projects it covers. A joint venture counts wholly under its listed parent in these counts.
* **Parent groups.** Investors are coloured by where the parent is headquartered: China (Hong Kong and
  Macao included), the coalition core, the coalition partners and the rest of the world.
* **Two bases.** "All" projects, or those "used by the indices": the manufacturing projects that are
  announced, under construction, completed or operational, without cancelled, paused or closed ones
  (the projects the partnership indices read). The share of a destination's projects
  with Chinese parents is shown as a raw share, not as the index component built from it.
* **Not a country total.** Domestic investment is not in CM-FDI.

## Sector emissions

The Climate section of each sector and the Policy & climate section of each Country profile show
Climate TRACE's estimates of direct emissions (tonnes of CO2-equivalent on a 100-year basis,
2021 to the latest complete year; Climate TRACE, climatetrace.org, CC BY 4.0).

* **Mapped, with a match quality.** Climate TRACE reports subsectors (coal mining, oil and gas
  production, aluminium, electricity generation, ...), not technologies. Each sector says how well its
  subsectors match: *direct* (the subsector is the sector's own activity, as coal mining for Coal
  extraction), *partial* (it also covers other activity, or only part of the sector's: oil and gas
  production is one subsector; coal-fired power is part of coal use), *proxy* (a related activity shown
  for scale, not the sector's own emissions) or *none*. Sectors with no match show the technology's IPCC
  lifecycle intensity (grams of CO2e per kWh over a plant's life) instead, which is a property of the
  technology, not the sector's emissions.
* **Direct emissions only.** The electricity a smelter or factory buys is counted in power, not in its
  own sector.
* **Power by fuel** sums the plants Climate TRACE tracks, grouped by the first fuel each lists, so it is
  below a country's total for power.
* **Intensity** is the tracked assets' emissions over their activity (t CO2e per MWh, per tonne of
  steel, ...), counting only the assets that report an activity.
* **Materials as context.** Steel and aluminium are shown beside the manufacturing sectors whose products
  are made of them, labelled as context and never counted as those sectors' emissions.
* **Carried-forward years.** Where Climate TRACE's latest year repeats the year before (some mining
  subsectors in 2025), the section says so: the change is not a measured change.

## Climate pathways (IEA scenarios)

The Climate section of each sector also shows where the International Energy Agency's scenarios lead
for the World, and the Policy & climate section of each Country profile one line of world context. These
are **IEA scenario projections, not observed data and not forecasts**, and every figure says so.

* **Source and licence.** IEA (2025), World Energy Outlook 2025 free dataset (World), CC BY-NC-SA 4.0
  (non-commercial use; adapted data shared under the same licence):
  the IEA's estimates for 2010, 2023 and 2024 and three scenarios for 2035, 2040 and 2050. *Stated
  Policies* reflects the policies in place and those announced with concrete detail; *Current Policies*
  only those already in force; *Net Zero Emissions by 2050* is the IEA's normative pathway to net zero
  energy-sector CO2 by 2050. The dataset has no 2030 values; lines join the published years.
* **World in the sectors, regions in the Country profiles.** The Sector pages use the World totals. The
  Country profiles use the same edition's Regions file (below); its regions have no Net Zero scenario.
* **Mapped, with a match quality.** Each sector shows the WEO series for its own activity (installed
  solar, wind, nuclear, geothermal and battery storage capacity; coal, oil and gas supply and use;
  coal-, gas- and oil-fired electricity), the demand it serves (the factories of the components and
  assembly stages and the minerals of extraction and refining: the deployment of the technology they
  supply) and emissions trajectories as context (the CO2
  intensity of electricity; CO2 from iron and steel, aluminium and chemicals beside the sectors made of
  them). *Direct*, *partial* and *proxy* mean what they mean for the emissions above. The free dataset
  has no EV stock, electrolyser capacity or grid length, so electricity used in transport stands in for
  the EV fleet, low-emissions hydrogen production (which also counts hydrogen from fossil fuels with
  carbon capture) for electrolysers, and electricity generation for the grid: each is labelled a proxy.
* **Mineral demand by scenario.** For the mineral sectors (extraction and refining), the IEA Critical Minerals Dataset 2026
  (CC BY 4.0) gives demand from the sector's technologies in the Current Policies, Stated Policies and
  High Demand scenarios, summed over the technologies named. That dataset has **no Net Zero scenario**,
  so mineral demand on the Net Zero pathway is not shown.
* **Vintage.** The pathways are checked against the previous edition (World Energy Outlook 2024) on the
  years both publish; the check is reported in the build, and only the 2025 edition is shown.

## Climate sources (fossil CO2, transition indicators, regional pathways, investment)

Added 2026-10-17. Each is cited wherever its figures are shown.

* **Fossil CO2 by economy.** Global Carbon Project, Global Carbon Budget 2025 (Friedlingstein et al.,
  2025, *Earth System Science Data*, https://doi.org/10.5194/essd-2025-659); the publisher's terms make
  use conditional on that citation. *Territorial* emissions are the fossil CO2 emitted inside the economy
  (fossil fuels, cement, flaring; international aviation and shipping excluded). *Consumption-based*
  emissions (after Peters, Davis and Andrew, 2012, *Biogeosciences* 9) add the CO2 emitted abroad to make
  what it imports and subtract the CO2 in what it exports; the GCP publishes *transfers* as territorial
  minus consumption-based, so a net importer of embodied CO2 has negative transfers. Values in tonnes of
  carbon are converted to CO2 with the publisher's factor 3.664. From 1990; consumption-based to 2023.
* **World fossil CO2 by fuel.** The same budget's fossil emissions by fuel (coal, oil, gas, cement,
  flaring), shown beside the coal, oil and gas sectors as context: every use of the fuel, never the
  sector's own emissions.
* **Transition indicators.** Energy Institute, Statistical Review of World Energy 2025, Country
  Transition Tracker: energy-related greenhouse gas emissions, fossil fuel consumption, renewables' share
  of energy consumption, the power generation mix, the carbon intensity of energy and of GDP, and energy
  per person, for 79 economies (31 for the power mix). Each is shown against the *median economy*, the
  middle of the economies the tracker lists, not a world average. The low-carbon share of power adds the
  wind, solar, hydro, other renewables and nuclear shares.
* **Regional pathways.** IEA (2025), World Energy Outlook 2025 free dataset, Regions (CC BY-NC-SA 4.0):
  the IEA's estimates for 2010, 2023 and 2024 and the Stated Policies and Current Policies scenarios for
  2035 and 2050. Each Country profile names the region it shows: the economy's own (United States, Japan,
  China, India, Brazil, Russia), the European Union for its members, Southeast Asia for its members, the
  broader region whose IEA definition names the economy, or, for economies the IEA counts as "other", the
  region of their UN sub-region. A regional pathway is the region's, not the economy's. The IEA publishes
  its Net Zero scenario for the World only; the World line stays beside it as context. IEA scenario
  projections, not observed data.
* **2030 milestones.** IEA (2021), Net Zero by 2050, Annex A (CC BY-NC-SA 3.0 IGO): the 2021 roadmap's
  World values for 2030, shown beside the 2025 pathways of the same quantity and labelled as an older
  edition (the IEA has updated the scenario since). Where the 2021 row is narrower (unabated coal and gas
  only, passenger cars without vans, combustion CO2 only), the table marks it.
* **Energy investment.** IEA (2026), World Energy Investment 2026 (CC BY 4.0): investment by sector for the
  World, Advanced economies, China, emerging market and developing economies (excluding China) and seven
  regions, 2015-2026, in billions of 2025 US dollars at market exchange rates; 2026 values are IEA
  estimates. Each sector shows the investment in its own activity (solar, wind, nuclear, battery storage,
  electricity networks, coal, oil and gas supply, fossil power) or in the deployment it supplies, with
  the match quality: geothermal sits inside "other renewables" and the IEA's end-use transport spending
  (mostly electric vehicles) stands in for EVs, each labelled a proxy.
* **Not shown: manufacturing emissions per value added.** The IEA Energy and Emissions per Value Added
  database was staged, but its terms require the IEA's permission for any external dissemination, so the
  manufacturing sectors (components and assembly) do not show it and say why.

## Money

Comtrade trade is in current US dollars. Clean Investment Monitor investment is in billions of 2024
US dollars. CM-FDI capital is in millions of US dollars as announced (nominal). Nominal and real values
are never put on one chart or in one ratio.

## Value states

| State | Meaning |
|---|---|
| Observed | the source reports a value |
| Reported zero | the source reports 0 |
| Not reported separately | the value sits inside the source's "Rest of world"; it is not a zero. One exception: where no source reports an economy's production of a fuel separately (EI's "Other", no EIA value), production is assumed to be 0, so its net balance is minus its consumption and its self-sufficiency 0%. Those values are drawn like any other, marked † "Production not reported separately; assumed 0" |
| Listed without a figure | the source lists the economy but withholds the number, or reports it as under 1%; for a CM-FDI project, capital "not disclosed" |
| Mirror | built from partners' declarations |
| Estimate | flagged by the source (the Clean Investment Monitor's 2025 includes an estimated fourth quarter) |
| Base year zero | growth is not defined |
| Not covered | the economy is outside the source's coverage |
| Regional value | the value belongs to a region or entity, not to the country |
| Old | the latest observation is older than the measure allows |

Maps hatch the states without a value; tables and tooltips say each state in words, and the "Source
note" column of every table carries the fact's own notes (an assumed-zero production, an EIA value).

## What this module does not show, and why

* **Index, weight and score values.** Rankings and weights belong to the Country profiles. Each measure's
  notes say which index it feeds, without showing the index. The one exception is what each
  country should do, a recommendation at fixed thresholds, shown in its own section.
* **Totals across technologies.** Trade baskets overlap, so a sum would count some goods twice.
* **The same figure twice.** Where a source reports one figure for manufacturing as a whole
  (manufacturing investment, the IEA's manufacturing shares, development aid), the components and
  assembly pages both show it, each with a note naming the other; headline sums count it once.
* **Regional or world values as country values.** World prices, IEA regions and entities, and
  technology readiness are shown at their own level and never ranked as if they were national.
* **Two trade bases on one page.** The matrix is shown; the exporter-reported basis appears only in the
  reconciliation table.
* **Area-integrated solar and wind potential as a ranking.** It ranks land area; it appears only in a
  scatter against resource quality.
* **Zero-fills as zeros.** Values a source does not report separately are marked as such; the one
  assumed zero (production no source reports, in the net balance) is drawn as a value and labelled.
* **Growth from zero or tiny bases.** Shown as "new" or "n/a".
* **Mixed units.** Manufacturing capacity is never summed across products; the six Energy Institute
  fuels are not shown as an energy mix.
* **Unverified units.** Where a unit could not be confirmed (the geothermal cost currency, the wind
  threshold, some commodity prices), the unit is labelled "unit to confirm".
* **Licensed data.** BNEF and BCG data are licensed and are not part of the public explorer.
* **Perspective dials.** The Country profile's dials do not apply here: measured data do not move with priorities.
