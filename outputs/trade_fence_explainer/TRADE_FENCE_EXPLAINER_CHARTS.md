# Trade fence explainer: charts and findings

Observed tariffs: US Census calculated duty through **August 2026**. Statutory snapshot: measures legally in force on
**4 December 2026** (`data/dec4_2026_policy_snapshot.csv`). Code: `scripts/trade_fence_explainer/`, `R/trade_fence_explainer/`.
Method, sources and refresh: [README.md](README.md). Gaps: [DATA_GAPS.md](DATA_GAPS.md).

---

### Chart 1 — The US trade fence has risen sharply, including against allies

**Question:** How much has US trade restrictiveness increased since 2024, and against whom?

**Finding:** On all goods, the Trade Restrictiveness Index on trusted suppliers rose from 2.1% (2024) to 12.3% (August 2026),
×5.8; on China from 15.7% to 28.4% (×1.8); on all suppliers from 6.6% to 15.6%. Trusted suppliers now face a slightly higher
index than nonaligned ones (11.9%). All four audit values reproduce exactly. On the 4 December statutory regime (modelled) trusted
suppliers reach 14.0% and China stays at 28.4%.

**Why it matters for Yards & Fences:** The framework's gate is meant to stay open to trusted producers; the floor under them has
risen faster than the fence against the coercive supplier.

**Suggested article headline:** The US raised the fence on China, and the floor under its allies.

**Caveats:** TRI weighs dispersion as well as level; it is not an average tariff. Census duty excludes AD/CVD. Weights are 2024
imports, so substitution away from tariffed goods is not counted. Vietnam and Indonesia are "coercive" under the project's
UN-voting rule.

**Files:** [01_fence_rising.png](01_fence_rising.png), [.svg](01_fence_rising.svg), [data/01_fence_rising.csv](data/01_fence_rising.csv);
`scripts/trade_fence_explainer/01_build_data.R` (chart 1 section), `02_draw_charts.R`.

### Chart 2 — Where has America actually built tariff walls?

**Question:** Which technologies and supply-chain stages face high or low tariff restrictions?

**Finding:** The highest walls stand at solar assembly (39%, with AD/CVD), gas assembly (36%), oil assembly (34%) and grid refining
(33%); batteries sit at 24-29% from refining to assembly. Crude oil, gas and nuclear fuel extraction, fuel refining and battery
recovery are essentially open (0-1%).

**Why it matters for Yards & Fences:** The tariff wall is highest in midstream and assembly metal-intensive equipment, not in
the stages where the framework finds coercive control.

**Suggested article headline:** America's tariff walls stand around assembly, not extraction.

**Caveats:** Deployment repeats the Assembly basket. Thin cells (under $0.5bn) in italics. AD/CVD capped at 100% per line.
Observed August 2026; the 4 December values are in the CSV (`tri_all_dec4`).

**Files:** [02_tariff_map.png](02_tariff_map.png), [.svg](02_tariff_map.svg), [data/02_tariff_map.csv](data/02_tariff_map.csv).

### Chart 3 — Who actually gets through the gate?

**Question:** Does US tariff treatment correspond to strategic alignment?

**Finding:** China paid an effective 21.9% (duties/imports, all goods, June-August 2026). Trusted partners averaged 6.2%,
*more* than nonaligned partners (4.1%); coercive suppliers other than China (Vietnam, Indonesia, Russia) paid 7.2%. Among
trusted partners rates range from 1.7% (Ireland) and 3.0% (Canada) to 10.6% (Japan); Germany pays 9.0%, Korea 7.6%. The data support the candidate headline.

**Why it matters for Yards & Fences:** A trusted-partner gate would show trusted suppliers below nonaligned ones; the opposite holds.

**Suggested article headline:** The US has a China gate. It does not yet have a trusted-partner gate.

**Caveats:** Effective rates, not TRI; exclude AD/CVD; import-weighted over the 29 tracked partners with a UN vote (Taiwan
dropped). Trusted rates are partly composition: allies sell more metal-intensive and auto goods covered by Section 232.

**Files:** [03_who_gets_through_gate.png](03_who_gets_through_gate.png), [.svg](03_who_gets_through_gate.svg),
[data/03_who_gets_through_gate.csv](data/03_who_gets_through_gate.csv), [data/03_group_rates.csv](data/03_group_rates.csv).

### Chart 4 — The fence is much more than tariffs

**Question:** What instruments make up the US electro-industrial trade regime?

**Finding:** At the assembly stage tariffs rank Low (grid) or Medium (solar, batteries) against the whole economy, while
domestic-market rules rank High in all three chains (local-content incentives and public purchasing for grid and solar; public
purchasing and local-content rules for batteries). Capital and technology controls rank High for grid assembly and for solar
components and assembly. Ten High/Medium bands rest on a single measure (seven in batteries).

**Why it matters for Yards & Fences:** Different chains are fenced through different instruments, so tariff-only audits
miss much of the fence.

**Suggested article headline:** America's trade fence is mostly not made of tariffs.

**Caveats:** Bands count how often NIPO measures name a product, not how much trade they block; many measures are firm-,
partner- or end-use-specific. From 4 December Proclamation 11052 adds a 15% tariff and price floors on solar wafers, cells and
modules (red triangles), not reflected in the August tariff band.

**Files:** [04_multi_tool_fence.png](04_multi_tool_fence.png), [.svg](04_multi_tool_fence.svg),
[data/04_multi_tool_fence.csv](data/04_multi_tool_fence.csv); inputs `data/reference/trade_fence_explainer/fence_tools/`.

### Chart 5 — Does the actual fence match the Yards & Fences strategy?

**Question:** For each technology-stage cell, does actual tariff policy match the recommended posture?

**Finding:** Of 46 assessed cells, 12 are aligned, 17 tax trusted partners (all Preserve, Catch Up, Partner or Accelerate cells
where trusted partners pay 7-33%), 16 sit above the recommended height (Watch cells, every Deployment cell scored among them),
and 1 is below it (EV refining, Secure: China pays 2.9%). The counts are unchanged on the 4 December statutory regime.

**Why it matters for Yards & Fences:** Most misalignment is not "too little fence against China" but fences landing on trusted
partners and on stages the framework would leave open.

**Suggested article headline:** Only 12 of 46 US tariff fences match the strategy.

**Caveats:** Verdicts depend on the audit's lines (open 5%, high 15%). At a 10% open line, 13 cells still tax trusted partners;
at 15%, 8 do and 25 cells are aligned (`diagnostics/us_trade_restrictiveness/verdict_threshold_sensitivity.csv`). The rule maps a
Secure cell with a high fence on both coercive and trusted suppliers to "Partial fence" (shown as below); no cell is in that case.

**Files:** [05_yards_fences_audit.png](05_yards_fences_audit.png), [.svg](05_yards_fences_audit.svg),
[data/05_yards_fences_audit.csv](data/05_yards_fences_audit.csv); social: [05b_verdict_summary.png](05b_verdict_summary.png),
[data/05b_verdict_summary.csv](data/05b_verdict_summary.csv).

### Chart 6 — How economically important are the mismatches?

**Question:** How much US electro-industrial trade sits behind aligned and misaligned fences?

**Finding:** Counting each HS6 product once, $531bn of 2024 imports sit in assessed cells: 46% behind aligned fences, 47% in
cells that tax trusted partners, 7% above the recommended height. The aligned dollars are mostly crude oil and refined products.
In clean-energy and grid technologies ($277bn) only 3% sit behind an aligned fence and 84% ($233bn) in cells that tax trusted
partners. For trusted-partner imports alone: 53% aligned, 40% taxing trusted partners.

**Why it matters for Yards & Fences:** The misalignment is not confined to small cells: it covers most of the clean-energy and
grid import base.

**Suggested article headline:** Most clean-energy imports sit behind the wrong fence.

**Caveats:** Equal split of shared codes across cells; Deployment left out because it repeats Assembly. Giving Assembly goods the
Deployment verdict instead moves dollars from "taxes trusted partners" to "above recommendation" but leaves the aligned share
unchanged. Gross cell sum (with overlaps): $1,109bn.

**Files:** [06_dollar_weighted_misalignment.png](06_dollar_weighted_misalignment.png), [.svg](06_dollar_weighted_misalignment.svg),
[data/06_dollar_weighted_misalignment.csv](data/06_dollar_weighted_misalignment.csv), [data/06_dollar_weighted_by_tech.csv](data/06_dollar_weighted_by_tech.csv),
[data/06_dedup_coverage.csv](data/06_dedup_coverage.csv).

### Chart 7 — Why are trusted partners behind the fence?

**Question:** Which policies explain the tariff burden on trusted suppliers?

**Finding:** On the 4 December regime the trusted-partner TRI is 34% in wind assembly, 28% solar assembly, 19% grid assembly, 21%
battery refining, 36% oil and 37% gas assembly. Switching off Section 232 (metals, autos, polysilicon) roughly halves it in five of
six cells (wind 17%, solar 14%, battery refining 7%, oil and gas 18%; grid 13%). Section 338 duties on Canada add 6-12 points in
wind, solar and grid assembly since September. AD/CVD adds about 7 points in oil and gas assembly (line pipe and OCTG orders on
Japan, Germany, Korea, Austria). The Section 301 forced-labour duty adds under 1 point in these cells, since 232 goods are exempt.

**Why it matters for Yards & Fences:** The fences trusted partners face in wind, grid and solar build-out were set for steel,
aluminium, copper and a Canada dispute, not for those industries.

**Suggested article headline:** Steel tariffs are a wind-farm tax.

**Caveats:** Counterfactuals, not an additive decomposition. Section 338 coverage is the tracker's reading of annex images
(DATA_GAPS 4). The August observed values (21.8, 18.5, 12.1, 21.3, 32.5, 33.2) show how much of the 4 December level is new.

**Files:** [07_why_allies_pay.png](07_why_allies_pay.png), [.svg](07_why_allies_pay.svg), [data/07_why_allies_pay.csv](data/07_why_allies_pay.csv);
`scripts/trade_fence_explainer/statutory_variants.py`.

### Chart 8 — When the fence rises, does production reshore or trade reroute?

**Question:** Did restrictions suppress imports, substitute domestic production, or shift sourcing?

**Finding:** Solar: crystalline-silicon module imports fell 76% (January-August, $9.6bn in 2024 to $2.3bn in 2026) and moved twice:
from Vietnam, Thailand, Malaysia and Cambodia (orders June 2025) to Indonesia, Laos and India (orders November 2026), then to
Africa ($1.1bn, mainly Ethiopia, Kenya, Nigeria) and the Philippines ($0.7bn) in 2026. US module capacity rose from 14.5 GW
(end-2023) to about 66 GW, but cell capacity is about 3 GW. Batteries: China's share of lithium-ion imports fell from 72% to 39%
while Japan (6% to 18%), Korea (6% to 14%), Malaysia (2% to 7%) and Thailand (0% to 3%) gained; 14 of 16 battery-manufacturing FDI
projects in Malaysia and 9 of 12 in Thailand have Chinese parents (70% and 95% of disclosed capital).

**Why it matters for Yards & Fences:** Fences moved where products are assembled, and in batteries part of the shift is to
Chinese-owned plants; diversification by origin is not diversification by ownership or technology.

**Suggested article headline:** The same fence produced rerouting in solar and re-badging in batteries.

**Caveats:** Africa/Philippines values split the Census remainder by Comtrade HS6 shares (DATA_GAPS 2). Capacity is nameplate
(SEIA/Wood Mackenzie), not output. HTS 8507.60 mixes cells and packs and EV and storage uses. CM-FDI covers projects since about
2017, not installed capacity.

**Files:** [08_trade_response.png](08_trade_response.png), [.svg](08_trade_response.svg), [data/08a_solar_trade_response.csv](data/08a_solar_trade_response.csv),
[data/08b_battery_trade_response.csv](data/08b_battery_trade_response.csv), [data/08b_battery_ownership.csv](data/08b_battery_ownership.csv),
[data/08b_battery_fdi_projects.csv](data/08b_battery_fdi_projects.csv), [data/08_policy_events.csv](data/08_policy_events.csv), [data/D_domestic_response.csv](data/D_domestic_response.csv).

---

## Five strongest empirical findings

Ranked by the strength and clarity of the evidence, not by fit with any NEIS position.

1. **Trusted suppliers' tariff burden rose almost six-fold and now matches nonaligned suppliers'.** Trusted TRI 2.1% to 12.3%
   (nonaligned 11.9%); effective rates 6.2% vs 4.1%. Direct measurement on Census duty; reproduces the tracker's published TRI
   to 2e-15; robust to the TRI-vs-average choice. (Charts 1, 3)
2. **Section 232 metals duties, not China policy, drive the allied burden in clean-energy equipment.** Removing 232 roughly halves
   the trusted-partner index in five of six key cells; the audit's separate tag finds 232 lines carry 95-99% of the trusted TRI
   squared in wind, gas, oil and solar assembly and battery refining. Two methods agree. (Chart 7)
3. **Solar module imports fell 76% and rerouted twice, while US cell-making stayed near 3 GW.** Census trade data, reconciled to
   the total every month; the origin shift tracks each AD/CVD round. Domestic capacity is secondary data. (Chart 8A)
4. **Only 12 of 46 cells are aligned, and outside fossil fuels 84% of import dollars sit in cells that tax trusted partners.**
   Holds on the 4 December regime; depends on the posture engine and the 5% line (at 15%, 25 cells are aligned). (Charts 5, 6)
5. **Battery imports diversified away from China by origin more than by ownership.** China's share 72% to 39%, with gains in
   Malaysia and Thailand where most recent battery FDI has Chinese parents. Trade data are firm; ownership rests on project
   counts and disclosed capital, not plant capacity. (Chart 8B)

The simple story the evidence supports: (1) the US has built substantial fences; (2) they differ sharply by product, stage,
country and instrument; (3) China is treated differently, but trusted partners are increasingly caught behind the fence,
mostly by Section 232; (4) against Yards & Fences, most restrictions do not line up with the strategic objective; (5) the next
question is where each fence belongs, how high it should be, who sits inside the gate, and which instrument fits the problem.
