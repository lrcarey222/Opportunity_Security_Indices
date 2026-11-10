# Trade fence explainer: charts and findings

Observed trade and tariffs: US Census data through **August 2026**; trade comparisons use **January-August 2024 vs
January-August 2026** (September 2026 needs a Census API key; see DATA_GAPS 1). Statutory snapshot: measures legally in force
on **4 December 2026** (`data/dec4_2026_policy_snapshot.csv`). Code: `scripts/trade_fence_explainer/`, `R/trade_fence_explainer/`.
Method, sources and refresh: [README.md](README.md). Gaps: [DATA_GAPS.md](DATA_GAPS.md). Review sheet: [contact_sheet.html](contact_sheet.html).

## Thesis

US electro-industrial trade is being actively reorganized. Different policy tools are producing different forms of
adjustment: rising imports with a falling China share in most clean-energy technologies, rerouting through new third
countries in solar, diversification by origin but not clearly by ownership in batteries, growing domestic module (not
cell) capacity, and significant barriers facing trusted suppliers. The question is not whether the United States now has
an industrial trade policy. It is whether those interventions are producing the supply-chain structure the United States
actually wants. Yards & Fences provides the framework for answering that question.

What the data do **not** support: a general contraction of electro-industrial trade, or broad regionalization (North
America's share of clean-energy imports is flat or falling outside batteries).

## Recommended publication sequence

| # | Section | Figure | Data |
|---|---|---|---|
| 1 | What America trades | [00a_electro_industrial_trade_overview.png](00a_electro_industrial_trade_overview.png) | `data/00a_*.csv` |
| 2 | How US supply chains are changing | [00b_trade_rewiring.png](00b_trade_rewiring.png) with [00c_policy_timeline.png](00c_policy_timeline.png) | `data/00b_*.csv`, `data/00_policy_timeline.csv` |
| 3 | The fence has risen sharply | [01_fence_rising.png](01_fence_rising.png) | `data/01_fence_rising.csv` |
| 4 | Where the tariff wall sits | [02_tariff_map.png](02_tariff_map.png) | `data/02_tariff_map.csv` |
| 5 | The fence uses more than tariffs | [04_multi_tool_fence.png](04_multi_tool_fence.png) | `data/04_multi_tool_fence.csv` |
| 6 | Does the fence match the strategy? | [05_yards_fences_audit.png](05_yards_fences_audit.png) paired with [06_dollar_weighted_misalignment.png](06_dollar_weighted_misalignment.png) | `data/05*.csv`, `data/06*.csv` |
| 7 | Why trusted partners are getting caught | [07_why_allies_pay.png](07_why_allies_pay.png) | `data/07_why_allies_pay.csv` |
| 8 | What happens when the fence rises | [08_trade_response.png](08_trade_response.png) | `data/08*.csv` |

Supporting, sidebar and social: [03_who_gets_through_gate.png](03_who_gets_through_gate.png) (sidebar),
[05b_verdict_summary.png](05b_verdict_summary.png) (social), [analysis/fence_change_vs_diversification.png](analysis/fence_change_vs_diversification.png) (analysis only).

---

### 1 — What America trades in the electro-industrial stack (new, 00a)

**Question:** What are the scale and structure of US electro-industrial trade, and where is the US most dependent on imports?

**Finding:** In January-August 2026 the US imported $193bn of clean-energy and grid goods and exported $76bn (2.5 to 1); across
all nine technologies, including fuels, imports ($358bn) and exports ($360bn) balance because the US is a large net exporter of
oil ($207bn exports vs $149bn imports) and gas. Imports are largest in electric vehicles ($118bn), grid ($82bn) and solar ($75bn).
Relative to exports, imports are highest in batteries (3.5 to 1), nuclear (3.2 to 1) and EVs (2.9 to 1). 55% of clean-energy and
grid imports are assembled goods and 27% components; the US runs surpluses in extraction and fuel refining.

**Why it matters for Yards & Fences:** The fences act on a trade base that is concentrated at assembly and components, where
the US is weakest, while upstream raw materials are mostly a US export strength.

**Suggested article headline:** America's electro-industrial trade deficit sits in a few technologies, and mostly at assembly.

**Caveats:** Technology totals overlap (shared products such as static converters, switchgear and cable count in each); stage
totals count each product once. Several strategically important flows are economically small: nuclear ($5.2bn imports), rare-earth
compounds ($0.2bn), permanent magnets ($0.3bn), graphite ($0.4bn), EV refining (rare-earth oxides and metals, $0.2bn). A deficit
is not itself a weakness. Exports come from Census data as reported to UN Comtrade (HS6); imports from Census HS10 lines.
Geothermal and green hydrogen have no traded basket.

**Files:** [PNG](00a_electro_industrial_trade_overview.png), [SVG](00a_electro_industrial_trade_overview.svg), [data/00a_electro_industrial_trade_overview.csv](data/00a_electro_industrial_trade_overview.csv),
[data/00a_trade_by_technology.csv](data/00a_trade_by_technology.csv), [data/00a_trade_by_stage.csv](data/00a_trade_by_stage.csv); `scripts/trade_fence_explainer/03_build_trade_overview.R`.

### 2 — US electro-industrial trade is being rewired, not simply reduced (new, 00b + 00c)

**Question:** How have the scale and geography of US electro-industrial imports changed between 2024 and 2026?

**Finding:** Imports grew in five of six clean-energy and grid technologies (grid +38%, wind +22%, solar and EVs +4%) and China's
share fell in all six. Batteries are the clear case of diversification through contraction: imports fell 4% while China's share
fell from 54% to 29% (battery assembly 72% to 39%), Southeast Asia's share of battery assembly rose from 4% to 14%, and supplier
concentration halved (HHI 0.31 to 0.14; effective number of suppliers 3.3 to 7.2). Grid concentration also fell (HHI 0.11 to 0.08)
as copper cathode imports tripled. Rare-earth compounds (72% Chinese) and magnets (67%) remain concentrated. Mexico is the top
supplier in EVs, solar, wind and grid, but North America's share of clean-energy imports is flat or falling except in batteries.

**Why it matters for Yards & Fences:** Diversification away from China is real and broad, but much of it is to third countries,
and in the most dependent products (rare earths, magnets) it has barely started.

**Suggested article headline:** America is buying more clean-energy goods, and fewer of them from China.

**Caveats:** A falling China share is not a success by itself (rerouting, Chinese-owned plants abroad and smaller markets all
lower it). China = imports recorded from China (Hong Kong and Macao are not separable). Concentration is measured over the 30
named partners (84% of imports), so HHI is a lower bound. The timeline is context only: trade changes cannot be attributed to
individual measures from these descriptive charts. The 2025 IEEPA steps in the timeline come from the Trade War Tracker registry.

**Files:** [00b PNG](00b_trade_rewiring.png), [SVG](00b_trade_rewiring.svg), [data/00b_trade_rewiring.csv](data/00b_trade_rewiring.csv),
[data/00b_trade_diversification_metrics.csv](data/00b_trade_diversification_metrics.csv), [data/00b_monthly_imports.csv](data/00b_monthly_imports.csv);
timeline [00c PNG](00c_policy_timeline.png), [SVG](00c_policy_timeline.svg), [data/00_policy_timeline.csv](data/00_policy_timeline.csv).

### 3 — The fence has risen sharply, including against allies (existing chart 1)

**Question:** How much has US trade restrictiveness increased since 2024, and against whom?

**Finding:** On all goods, the Trade Restrictiveness Index on trusted suppliers rose from 2.1% (2024) to 12.3% (August 2026),
+10.2 points (×5.8); on China from 15.7% to 28.4%, +12.7 points (×1.8); on all suppliers from 6.6% to 15.6%. Trusted suppliers
now face a slightly higher index than nonaligned ones (11.9%). On the 4 December regime (modelled) trusted suppliers reach 14.0%.

**Why it matters for Yards & Fences:** The gate is meant to stay open to trusted producers; the floor under them has risen
almost as far in points as the fence on China, and much further in proportion.

**Suggested article headline:** The US raised the fence against China, and sharply raised the floor under its allies.

**Caveats:** TRI weighs dispersion as well as level; it is not an average tariff. Census duty excludes AD/CVD. Percentage-point
changes are in the CSV (`change_points`). Vietnam and Indonesia are "coercive" under the project's UN-voting rule.

**Files:** [01_fence_rising.png](01_fence_rising.png), [.svg](01_fence_rising.svg), [data/01_fence_rising.csv](data/01_fence_rising.csv).

### 4 — Where the tariff wall sits (existing chart 2)

**Question:** Which technologies and supply-chain stages face high or low tariff restrictions?

**Finding:** The highest walls stand at solar assembly (39%, with AD/CVD), gas assembly (36%), oil assembly (34%) and grid refining
(33%); batteries sit at 24-29% from refining to assembly. Crude oil, gas and nuclear fuel extraction, fuel refining and battery
recovery are essentially open (0-1%).

**Why it matters for Yards & Fences:** The tariff wall is highest where chart 1 shows US imports concentrate (assembly and
metal-intensive equipment), not where the framework finds coercive control.

**Suggested article headline:** America's tariff walls stand around assembly, not extraction.

**Caveats:** Deployment repeats the Assembly basket. Thin cells (under $0.5bn) in italics. AD/CVD capped at 100% per line.

**Files:** [02_tariff_map.png](02_tariff_map.png), [.svg](02_tariff_map.svg), [data/02_tariff_map.csv](data/02_tariff_map.csv).

### 5 — The fence uses more than tariffs (existing chart 4, now "policy intensity")

**Question:** What instruments make up the US electro-industrial trade regime?

**Finding:** Measured as the relative intensity of each tool, at the assembly stage tariffs rank Low (grid) or Medium (solar,
batteries) against the whole economy, while domestic-market rules rank High in all three chains (local-content incentives and
public purchasing for grid and solar; public purchasing and local-content rules for batteries). Capital and technology controls
rank High for grid assembly and solar components. Ten High/Medium bands rest on a single measure (seven in batteries).

**Why it matters for Yards & Fences:** Different chains are fenced through different instruments, so tariff-only audits miss
much of the fence.

**Suggested article headline:** America's trade fence is mostly not made of tariffs.

**Caveats:** Bands compare each tool with itself, not tools with each other: a High procurement cell means products unusually
often named by procurement rules, not market closure equal to a High tariff. For non-tariff tools the bands count NIPO measures,
not economic restrictiveness. Proclamation 11052 (4 December) adds a 15% tariff and price floors on solar wafers, cells and modules.

**Files:** [04_multi_tool_fence.png](04_multi_tool_fence.png), [.svg](04_multi_tool_fence.svg), [data/04_multi_tool_fence.csv](data/04_multi_tool_fence.csv).

### 6 — Does the fence match the strategy? (existing charts 5 and 6, paired)

**Question:** Does actual tariff policy match the posture Yards & Fences recommends, by cell and by dollar?

**Finding:** Of 46 assessed cells, 12 are aligned, 17 tax trusted partners, 16 sit above the recommended height and 1 is below
it; the counts are unchanged on the 4 December regime. The count depends on the open-gate line: 12 of 46 aligned at 5%, 16 at
10%, 25 at 15%. In dollars, outside oil, gas and coal only 3% of 2024 imports sit behind an aligned fence and 84% ($233bn) in cells
that tax trusted partners; for clean-energy imports from trusted partners, 80% ($98bn of $122bn) sit in such cells.

**Why it matters for Yards & Fences:** Misalignment remains substantial under every threshold tested, and in dollar terms it
covers most of the clean-energy trade with the partners the US wants inside the gate.

**Suggested article headline:** Most clean-energy imports from allies sit behind the wrong fence.

**Caveats:** Verdicts use the audit's lines (open 5%, high 15%); the alternatives are shown on chart 5. Dollar weights split shared
codes equally and leave Deployment out (gross cell sum $1,109bn in the CSV).

**Files:** [05_yards_fences_audit.png](05_yards_fences_audit.png), [.svg](05_yards_fences_audit.svg), [data/05_yards_fences_audit.csv](data/05_yards_fences_audit.csv),
[data/05_threshold_sensitivity.csv](data/05_threshold_sensitivity.csv); [06_dollar_weighted_misalignment.png](06_dollar_weighted_misalignment.png), [.svg](06_dollar_weighted_misalignment.svg),
[data/06_dollar_weighted_misalignment.csv](data/06_dollar_weighted_misalignment.csv), [data/06_dollar_weighted_by_tech.csv](data/06_dollar_weighted_by_tech.csv).

### 7 — Why trusted partners are getting caught (existing chart 7)

**Question:** Which policies explain the tariff burden on trusted suppliers?

**Finding:** On the 4 December regime the trusted-partner TRI is 34% in wind assembly, 28% solar assembly, 19% grid assembly, 21%
battery refining, 36% oil and 37% gas assembly. Switching off Section 232 (metals, autos, polysilicon) roughly halves it in five of
six cells. Section 338 duties on Canada add a further 6-12 points in wind, solar and grid assembly since September; the coverage is
now verified line by line against HTSUS Revision 23, chapter 99 U.S. note 51(b)(3), and note 51(c) confirms the duty stacks on
Section 232. AD/CVD adds about 7 points in oil and gas assembly; the Section 301 forced-labour duty under 1 point.

**Why it matters for Yards & Fences:** The fences trusted partners face in wind, grid and solar build-out were set for steel,
aluminium and copper, and for a dispute with Canada, not for those industries.

**Suggested article headline:** Steel, aluminium and copper tariffs explain most of what allies pay for clean-energy equipment.

**Caveats:** Counterfactuals, not an additive decomposition. The 338 effect is modelled (it started after the latest Census month).

**Files:** [07_why_allies_pay.png](07_why_allies_pay.png), [.svg](07_why_allies_pay.svg), [data/07_why_allies_pay.csv](data/07_why_allies_pay.csv),
[data/reference/trade_fence_explainer/s338_hts_check.csv](../../data/reference/trade_fence_explainer/s338_hts_check.csv).

### 8 — What happens when the fence rises (existing chart 8)

**Question:** Did restrictions suppress imports, substitute domestic production, or shift sourcing?

**Finding:** Solar: crystalline-silicon module imports fell 76% (January-August, $9.6bn in 2024 to $2.3bn in 2026) and moved
twice, from Vietnam, Thailand, Malaysia and Cambodia to Indonesia, Laos and India, and in 2026 (estimated split) to Africa and the
Philippines. US module capacity (not output) rose from 14.5 GW to about 66 GW; cell capacity is about 3 GW. Batteries: China's share
of lithium-ion imports fell from 72% to 39%, mostly to Japan (6% to 18%) and Korea (6% to 14%); Malaysia and Thailand also gained,
and 14 of 16 battery-manufacturing FDI projects in Malaysia and 9 of 12 in Thailand have Chinese parents.

**Why it matters for Yards & Fences:** The same family of fences produced rerouting in solar and origin diversification in
batteries; neither is the same as building the missing domestic stages (cells) or diversifying ownership.

**Suggested article headline:** Battery imports diversified away from China by origin more than by ownership.

**Caveats:** Country splits outside the tracker's 30 partners are estimates (labelled "est."; the aggregate decline is exact).
Capacity is nameplate, not production. No evidence of deliberate origin circumvention is claimed. CM-FDI covers projects since
about 2017, not installed capacity.

**Files:** [08_trade_response.png](08_trade_response.png), [.svg](08_trade_response.svg), [data/08a_solar_trade_response.csv](data/08a_solar_trade_response.csv),
[data/08b_battery_trade_response.csv](data/08b_battery_trade_response.csv), [data/08b_battery_ownership.csv](data/08b_battery_ownership.csv).

---

### Sidebar — Who gets through the gate (existing chart 3)

All goods, not the electro-industrial basket, with UN-voting blocs that need explaining (Vietnam and Indonesia coercive; India,
Mexico, Malaysia, Thailand nonaligned). China paid an effective 21.9% in June-August 2026; trusted partners 6.2%, more than
nonaligned (4.1%). Best used as a sidebar to section 3. Files: [03_who_gets_through_gate.png](03_who_gets_through_gate.png), `data/03_*.csv`.

### Social — Verdict summary (05b)

Four bars, 12 / 17 / 16 / 1. Files: [05b_verdict_summary.png](05b_verdict_summary.png), `data/05b_verdict_summary.csv`.

### Analysis — Fence change vs coercive dependence (not for publication)

Across 37 producing-stage cells the change in all-supplier TRI (2024 rates to August 2026) and the change in coercive-supplier
share are barely related (correlation -0.07 unweighted, -0.36 weighted by 2024 imports). 19 cells raised the fence by 10 points or
more; only 6 of them also cut the coercive share by 5 points or more, while 13 show little diversification. The relationship is too
weak to support an article claim. Files: [analysis/fence_change_vs_diversification.png](analysis/fence_change_vs_diversification.png),
[data/fence_change_vs_diversification.csv](data/fence_change_vs_diversification.csv).

---

## Five strongest empirical findings

Ranked by the strength and clarity of the evidence, not by fit with Yards & Fences.

1. **Trusted suppliers' tariff burden rose almost six-fold and now matches nonaligned suppliers'.** Trusted TRI 2.1% to 12.3%
   (nonaligned 11.9%); effective rates 6.2% vs 4.1%. Direct measurement on Census duty; reproduces the tracker's published TRI to
   2e-15. (Sections 3, sidebar)
2. **US clean-energy trade is growing, not contracting, while China's share falls across the board.** Clean-energy and grid
   imports +7% (January-August 2026 vs 2024), China's share down in all six technologies, most in batteries (54% to 29%). Census
   data reconciled to the total every month; robust to basket overlaps. (Section 2)
3. **Section 232 metals duties, not China policy, drive the allied burden in clean-energy equipment.** Removing 232 roughly halves
   the trusted-partner index in five of six key cells; the audit's 232 tag finds those lines carry 95-99% of the trusted TRI squared
   in wind, gas, oil and solar assembly and battery refining. (Section 7)
4. **Solar module imports fell 76% and rerouted twice, while US cell-making stayed near 3 GW.** The aggregate decline and the exit
   from the four 2025-order countries are exact Census figures; the 2026 African and Philippine shares are estimates. (Section 8)
5. **In dollars, misalignment covers most clean-energy trade with allies.** 84% of clean-energy and grid imports, and 80% of those
   from trusted partners, sit in cells that tax trusted partners; 12 of 46 cells are aligned at the 5% line (16 at 10%, 25 at 15%).
   Depends on the posture engine and the threshold, but holds under every threshold tested. (Section 6)

Also well supported but less central: the deficit sits at assembly and components (section 1); rare-earth and magnet dependence
on China is still about 70% (section 2); battery supplier concentration halved (section 2). Weakly supported and left out: any
link between higher fences and lower coercive dependence (analysis).

## What the trade data change about the story

- **The most important new fact:** US clean-energy trade is being rewired, not shrunk. Imports of clean-energy and grid goods grew
  7% between the two January-August windows, and China's share fell in every technology. The fence story is about who supplies the
  US, not whether it imports.
- **Overall direction: expanding, with one contraction.** Grid (+38%, much of it copper cathodes), wind (+22%), solar and EV baskets
  (+4%) grew; batteries (-4%) and crystalline-silicon modules (-76%) shrank. Total basket imports including fuels were flat (-0.1%).
- **China dependence is genuinely declining in batteries and nuclear:** battery imports from China fell from 54% to 29% of the
  total, nuclear-basket imports from 7% to under 1%. (Hong Kong and Macao cannot be separated in these files, so transshipment
  through them cannot be ruled out.)
- **Dependence is shifting through third countries in solar and graphite:** modules moved from Southeast Asia to Indonesia, Laos and
  India and then to Africa and the Philippines (estimated split); graphite moved from China (66%) to Indonesia (53%). Battery
  assembly imports from Southeast Asia rose from 4% to 14%, where most recent battery FDI has Chinese parents.
- **Supplier concentration fell meaningfully only in batteries and grid** (HHI 0.31 to 0.14 and 0.11 to 0.08); elsewhere it barely
  moved, and it rose in oil and gas (Canada).
- **Alongside major interventions:** the largest shifts coincide with the 2024-26 China 301 increases on batteries and the 2025-26
  solar AD/CVD rounds (see the timeline), but these descriptive charts cannot attribute them, and the fence-change analysis finds no
  consistent link between bigger fences and less coercive dependence.
- **Dependencies that remain:** rare-earth compounds and magnets (about 70% Chinese), EV refining (rare-earth oxides and metals, 71%
  Chinese), battery assembly (China still the largest supplier at 39%), and crystalline-silicon cells (about 3 GW of US capacity).
- **Effect on the audit narrative: it complicates and partly strengthens it.** It strengthens the "taxes trusted partners" point,
  because the trade base those fences sit on is growing and increasingly sourced from allies and Mexico. It complicates any claim
  that the fences are failing on China: China's share is falling almost everywhere. And it weakens a tidy reshoring story: the trade
  data show rerouting and origin diversification more than regionalization or domestic substitution.
