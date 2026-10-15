## What Sector Insights shows

Sector Insights shows the measured data that the Strategic Insights rankings start from: who produces,
holds, trades, invests in and regulates each energy technology, from raw materials (Upstream) through
manufacturing and processing (Midstream) to deployment and use (Downstream). Eleven technologies at
three stages make 33 sectors; Semiconductors are shown as well, outside the paper's headline set.
Every number is shown in its source's own unit and year, with the source, the coverage and the caveats
beside it. Nothing here is an index, a weight or a score, and nothing moves with the Strategic Insights
dials.

## Reading the colours

Maps, tiles and heat tables use one three-colour scale: **orange** for the lower values, **white** for
the middle one (the median of the values shown), **blue** for the higher values. Colours follow the
rank among the values on screen, so a map of a very uneven measure still shows the differences between
the middle economies instead of painting almost everything one shade. For a measure that can be below
zero (a net balance, net trade, a change), white is zero: orange below, blue above. Hatching means there
is no value to colour; hover or open the table for the reason. Groups of economies keep their own
colours everywhere: China, the coalition core, the coalition partners and the rest of the world.

Strategic Insights ranks and weighs; Sector Insights shows the measured data those rankings start from.

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
* **The other basis.** Strategic Insights ranks the exporters' own reports (free on board), which leave
  out economies that do not report and Hong Kong's re-exports. The reconciliation table below compares
  the two bases for every technology. Revealed comparative advantage is the one exporter-reported
  measure shown here, and it is labelled so.
* **Re-export hubs.** Hong Kong and Macao stay in the matrix, tagged "re-export hub"; importers record
  the country of origin, so their share there is small.
* **Baskets.** The Electric Grid Upstream basket is the copper and aluminium basket; HS 851762
  (communication equipment) is outside Electric Grid Midstream. Excluded codes are listed on each
  profile.
* **Upstream baskets (2026-10 review).** Electric Vehicles, Solar, Wind, Nuclear and Semiconductors
  Upstream now have trade baskets: the battery minerals and magnet rare earths as ores and concentrates
  (Electric Vehicles), silicon metal (Solar), rare-earth compounds, metals and ore (Wind), uranium ores
  and natural uranium compounds, moved from Nuclear Midstream (Nuclear), and gallium, germanium and
  indium (Semiconductors). Each profile states what its basket holds and leaves out. Rare-earth
  compounds and metals moved from the Electric Vehicles and Wind Midstream baskets to these Upstream
  baskets.
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

The Solar, Wind, Batteries and Electric Vehicles midstream pages also show the Cleantech Manufacturing
FDI Dataset (CM-FDI) of the Net Zero Industrial Policy Lab, Johns Hopkins University (P. Bandara and the
Net Zero Industrial Policy Lab, edition 2026.10.05, licensed under CC BY 4.0): one record per
cross-border greenfield or expansion project, announced 1996-2026, with its parent company, the economy
where the parent is headquartered, the destination, the status and a source link per field.

* **What is counted.** Projects, and the capital a project announced where it is disclosed. An
  undisclosed amount reads "not disclosed"; it is never counted as 0, and every sum says how many
  projects it covers. A joint venture counts wholly under its listed parent in these counts.
* **Parent groups.** Investors are coloured by where the parent is headquartered: China (Hong Kong and
  Macao included), the coalition core, the coalition partners and the rest of the world.
* **Two bases.** "All" projects, or those "used by the indices": the manufacturing projects that are
  announced, under construction, completed or operational, without cancelled, paused or closed ones
  (the projects the Strategic Insights partnership indices read). The share of a destination's projects
  with Chinese parents is shown as a raw share, not as the index component built from it.
* **Not a country total.** Domestic investment is not in CM-FDI.

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

* **Index, weight and score values.** Rankings and weights belong to Strategic Insights. Each measure's
  notes say which index it feeds, without showing the index.
* **Totals across technologies.** Trade baskets overlap, so a sum would count some goods twice.
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
* **Dials.** The Strategic Insights dials do not apply here: measured data do not move with priorities.
