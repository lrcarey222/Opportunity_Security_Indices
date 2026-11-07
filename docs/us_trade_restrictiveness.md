# US trade restrictiveness by technology and stage

How restrictive US tariffs are on each technology x supply-chain stage of the project, and whether that
matches the fence of the posture the decision framework recommends for the US (`docs/strategic_position.md`).
It belongs to Step 4, policy alignment: the measure is compared with the recommendation and never feeds it.

- Code: `R/strategic_position/trade_restrictiveness.R` (pure), `scripts/28_build_us_trade_restrictiveness.R`
  (IO), `scripts/fetchers/trade_war_tracker.R` (downloads), `scripts/utils/tracker_statutory_rates.py`
  (optional statutory cross-check).
- Settings: `config/us_trade_restrictiveness.yml`. Reference data: `data/reference/adcvd_orders.csv`,
  `data/reference/census_partner_codes.csv`.
- Outputs: `us_trade_restrictiveness_cells.{rds,csv}` in the outputs directory (every measure, cell and
  supplier group) and `diagnostics/us_trade_restrictiveness/`.
- Tests: `tests/testthat/test-trade-restrictiveness.R`.

## The index

The measure is the Trade Restrictiveness Index of Michael Waugh's Trade War Tracker
([tradewartracker.com](https://www.tradewartracker.com), code at
[github.com/tradewartracker/how-restrictive-us-trade](https://github.com/tradewartracker/how-restrictive-us-trade);
Waugh, "How Restrictive is U.S. Trade Policy?"). A good g is one HS10 line from one partner, its weight w its
import value in 2024 and its tariff tau the calculated duty over customs value in the data month:

    TRI  = sqrt( sum_g w_g tau_g^2 / sum_g w_g )
    mean = sum_g w_g tau_g / sum_g w_g

The TRI is the uniform tariff with the same welfare cost as the actual tariff structure, so it rises with the
dispersion of tariffs as well as their level. Fixed 2024 weights keep substitution away from tariffed goods out
of the index: every change is a change in tariffs.

Run on all goods, the build reproduces the tracker's published TRI for the data month to rounding error
(`tracker_reproduction.csv`, from the tracker's own `data/metrics/country.parquet`).

## Data

- Census HS10 imports for consumption (`CON_VAL_MO`) and calculated duty (`CAL_DUT_MO`) by month for the
  tracker's 30 largest partners and the all-country total (`data/imports-hs10/` of how-restrictive-us-trade,
  monthly from 2013; the tracker builds them from the Census International Trade API). `data_month: latest`
  takes the newest month in the total file. The Census API now needs a key, which the project does not hold, so
  the build reads the tracker's copies rather than the API.
- A rest-of-world row per line is the total less the 30 partners, so every US import is covered. The tracker's
  20 other 2024 partners (Cambodia, Turkey, Argentina and others; `data/<year>-hs10-imports<code>.parquet` of
  trade-war-redux-2025) are split out of it with their own 2024 imports and rates and the residual's rate for
  the data month, so they can carry a bloc and an AD/CVD order. What remains of the residual is "Unassigned".
- The project's basket (`data/reference/energy_hs6_master.csv`, in-basket rows of current HS) maps lines to
  cells through their HS6 code; a code in two technologies counts in both. Deployment has no traded goods of
  its own, so it is scored on the technology's Assembly basket, the goods the build-out imports. Geothermal and
  Green Hydrogen have no basket, so they have no measure.

### Choices

- **Lines with no trade in the data month.** The tracker counts them as tariff-free. Here a line with no trade
  takes its partner's HS8 rate that month, then its HS6 rate: HTS lines were re-coded after 2024 (lithium-ion
  cells 8507.60.0020, $18.5bn of 2024 imports, became 8507.60.0030 and .0090), and without the fallback
  Batteries Assembly keeps 17% of its weight. A line with nothing at HS6 drops out, and `weight_coverage`
  reports how much weight is left. The tracker's convention is kept as the `_universe` measure.
- **Supplier groups** are the posture engine's own rule, `sp_coercive_pairs()`: coercive = China, Hong Kong,
  Macao, Russia and the China-leaning UN-voting bloc; trusted = the US-leaning bloc; the rest nonaligned
  (Mexico, Taiwan and India among them).

## AD/CVD overlay

Census calculated duty is computed from HTS rates, so antidumping and countervailing duties are not in it:
PV modules from Vietnam, Thailand and Malaysia show 9.6-12.5% duty in August 2026 while their imports fell
by about 95%. The headline measure adds the deposit rates of the orders in `data/reference/adcvd_orders.csv`,
each read from its Federal Register notice:

- one row per product family x country: case numbers, status, in-force flag, the HTS numbers of the scope,
  the AD and CVD rates, the basis and the source notices;
- the rate is the one for exporters without their own rate (all others, or the country-wide rate for
  non-market economies), the cash deposit where it differs from the margin, and later court or WTO
  recalculations where they replaced the order rate;
- an order still awaiting publication after an affirmative ITC vote is in force (`pending_order` TRUE);
  investigations count only once deposits are collected;
- where two orders cover a line (China's 2012 and 2015 solar orders), the higher combined rate applies;
- a covered line with no trade in the data month is absent because of the order, so it enters at its 2024 rate
  plus the deposit rather than dropping out;
- every line is capped at 100% as prohibitive (`caps`; 250% is kept as a sensitivity), because the TRI squares
  rates and one deposit in the hundreds of percent would otherwise decide a cell. The cap also trims Chinese
  EVs, at about 128%.

As of 3 November 2026 the file holds 57 orders and investigations in force: solar cells and modules (China
2012 and 2015, Taiwan, Cambodia, Malaysia, Thailand, Vietnam, and India, Indonesia and Laos with orders
pending), silicon metal, wind towers, oil country tubular goods and line pipe. Graphite anode material from
China has no order (negative ITC vote, March 2026). Update the file when Commerce or the ITC publishes an
order, a review or a revocation.

## Verdicts

Each cell's TRI is read against the fence of its recommended choice (`fence_posture` and `gate_posture` of
the strategic position table), with two lines from the config: open (5%: ordinary MFN territory; the 2024
economy-wide TRI was 6.6%) and high (15%: about the economy-wide TRI in August 2026).

| Choice | Fence in the posture | Test | Otherwise |
|---|---|---|---|
| Secure | high, targeted at the coercive supplier, open to trusted producers | coercive >= high and trusted < open | No tariff fence (coercive < open) or Partial fence |
| Preserve, Catch Up | calibrated, temporary protection; gate open to allies and the frontier | trusted < open | Taxes trusted partners |
| Partner | partnership terms, not tariffs, decide who supplies | trusted < open | Taxes trusted partners |
| Accelerate | no barrier that slows deployment | trusted < open | Taxes trusted partners |
| Watch | low fence | all suppliers < open | Fence above recommendation |

`verdict_threshold_sensitivity.csv` repeats the verdicts at open lines of 10% and 15%.

## Statutory cross-check (optional)

The tracker's tariff-action engine (`code/make-tariff-by-country-by-time.ipynb` of trade-war-redux-2025) gives
each line's legal rate on a fixed 2024 basket on any date, so it runs ahead of the Census data month.
`scripts/utils/tracker_statutory_rates.py <clone> <out.csv> <dates>` runs it from a full clone; set
`statutory_rates` (or `OPSI_TWT_STATUTORY`) to the CSV and the build adds `statutory_<date>` measures. Two of
the engine's simplifications fall on this basket, so it is a cross-check rather than the headline: the
2024-26 Section 301 increases on Chinese EVs, batteries, graphite, magnets, polysilicon and wafers sit only in
its 2024 baseline, and every lithium-ion battery takes the 25% auto-parts duty.

## Section 232 decomposition

`tri_section232_tags()` labels each line with the Section 232 product set of the tracker's engine (autos and
parts, metals after the April 2026 restructuring with copper in annex I-A, copper, timber), matching each list
code on its own length as the engine's masks do. Each cell reports the share of its sum of w tau^2 on those
lines and its TRI without them (`s232_tri_sq_share`, `tri_excl_232`). The engine applies the auto-parts list to
every good in a listed provision, so all lithium-ion batteries carry the autos tag.

## Running and refreshing

    Rscript scripts/28_build_us_trade_restrictiveness.R

It needs `geo_alignment_tbl.rds` (scripts/12) and `strategic_position_tbl.csv` (scripts/26). On the first run
the fetcher downloads about 400 MB into `data/raw/trade_war_tracker/`. For a new Census month, run with
`OPSI_TWT_REFRESH=true` once the tracker has published it, and review `adcvd_orders.csv`.
`fetch_info.csv` in each tracker directory and `run_info.csv` record the commits and the data month used.

## Results (August 2026 data, AD/CVD in force 3 November 2026)

From `diagnostics/us_trade_restrictiveness/` (tracker commits in `run_info.csv`):

- Economy-wide (Census duty only), the TRI is 15.6%, against 6.6% in 2024; trusted partners pay 12.3%
  (2.1% in 2024), coercive suppliers 24.8%, China 28.4%.
- Of the 46 assessed cells with traded goods, 12 are in line with their posture. 17 of the 22 Preserve, Catch
  Up, Partner and Accelerate cells charge trusted partners 5% or more (7-33%; 13 at a 10% line, 8 at 15%),
  mostly through Section 232 metals duties, which carry 95-99% of the trusted-partner TRI squared in Wind, Gas,
  Oil and Solar Assembly and Batteries Refining. 16 of the 23 Watch cells sit above the 5% line, including
  every Deployment cell scored. The one Secure cell, EV Refining (rare-earth oxides and metals), has no tariff
  fence: Chinese suppliers pay 2.9%.
- AD/CVD deposits change no verdict. They take Solar Assembly from 20.5% to 39.0% (84.6% at the 250% cap),
  with coercive suppliers from 31.1% to 63.7% and trusted ones unchanged at 18.5%, and lift trusted partners in
  Gas Assembly from 30.2% to 33.2%.

## What the measure cannot see

- company-specific AD/CVD rates, which are often lower for large exporters, and circumvention rulings that
  extend orders to third countries (Chinese oil country pipe finished in Thailand, Brunei or the Philippines);
- orders on suppliers that are not separate partners in the data (Laos, Kazakhstan, Czechia, Greece, Ukraine,
  Romania) and orders on other basket products (steel strand, for example);
- bans, quotas, forced-labour detentions and export controls (the ban on Russian enriched uranium);
- Section 232 duties on semiconductors and heavy trucks, which the tracker does not model.

The import-volume ratio (the last `volume_months` months over the same months of 2024) shows where fences
like these may bite; it also moves with demand.
