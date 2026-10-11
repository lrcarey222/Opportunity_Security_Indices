## What Sector Insights shows

Sector Insights shows the measured data that the Strategic Insights rankings start from: who produces,
holds, trades, invests in and regulates each energy technology, from raw materials (Upstream) through
manufacturing (Midstream) to deployment (Downstream). Every number is shown in its source's own unit and
year, with the source, the coverage and the caveats beside it. Nothing here is an index, a weight or a
score, and nothing moves with the Strategic Insights dials.

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
* **Top-1 and top-3.** The share of the largest holder, and the combined share of the three largest.
* **HHI.** The Herfindahl-Hirschman index: the sum of the squared shares, from near 0 (spread across
  many holders) to 1 (one holder). When more than 5% of the total is not attributed, the residual cannot
  be split, so the HHI is a lower bound and is written "at least" (for example "≥ 0.21").
* **Effective number.** One divided by the HHI: the number of equal-sized holders that would give the
  same concentration.
* **Coalition share.** The combined share of the coalition's {members_n} members: {core_n} core members
  and {partner_n} partners. China, the coalition core, the coalition partners and the rest of the world
  always keep the same colours.

Concentration is described, not graded: no thresholds are drawn and no colour means "good" or "bad".

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

## Money

Comtrade trade is in current US dollars. Clean Investment Monitor investment is in billions of 2024
US dollars. Nominal and real values are never put on one chart or in one ratio.

## Value states

| State | Meaning |
|---|---|
| Observed | the source reports a value |
| Reported zero | the source reports 0 |
| Not reported separately | the value sits inside the source's "Rest of world"; it is not a zero |
| Listed without a figure | the source lists the economy but withholds the number, or reports it as under 1% |
| Mirror | built from partners' declarations |
| Estimate | flagged by the source (the Clean Investment Monitor's 2025 includes an estimated fourth quarter) |
| Base year zero | growth is not defined |
| Not covered | the economy is outside the source's coverage |
| Regional value | the value belongs to a region or entity, not to the country |
| Old | the latest observation is older than the measure allows |

Maps hatch the states without a value; tables and tooltips say each state in words.

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
* **Zero-fills as zeros.** Values a source does not report separately are marked as such.
* **Growth from zero or tiny bases.** Shown as "new" or "n/a".
* **Mixed units.** Manufacturing capacity is never summed across products; the six Energy Institute
  fuels are not shown as an energy mix.
* **Unverified units.** Where a unit could not be confirmed (the geothermal cost currency, the wind
  threshold, some commodity prices), the unit is labelled "unit to confirm".
* **Licensed data.** BNEF and BCG data are licensed and are not part of the public explorer.
* **Dials.** The Strategic Insights dials do not apply here: measured data do not move with priorities.
