## In plain words

Every economy gets two scores in each of 33 sectors (11 energy technologies, each at the upstream, midstream and downstream stage of its supply chain): **Energy Security (ES)**, how well it can rely on what it needs there, and **Economic Opportunity (EO)**, how well placed it is to create value there; both run from 0 to 1 and compare it with every other economy in the same sector. The **Strategic Index (SI)** then ranks each economy's own sectors by how strongly the data point to prioritising them: high opportunity, high exposure (security need, $1-ES$), existing policy activity, the stage and the technology's low-emission value. The **Partnership Strength Index (PSI)** rates how good a partner one economy is for another, as a supplier it can rely on (**Friendshore, FS**), as a market for its exports (**Partnership Opportunity, PO**) and, for middle- and lower-income partners, as a development partner (**Development Potential, DP**). Four **dials** (hawkishness, bullishness, populism, greenness) re-weight all of this, and the allied network design, to reflect different priorities. At 50 every dial reproduces the paper exactly; any other setting is a scenario recomputed in your browser.

## What this explorer does

The paper builds its indices and its network design from a chain of choices: how much weight to put on security against opportunity, on industrial policy, on climate, on spreading production across members. The paper makes one set of those choices. This explorer lets you make your own, and recomputes everything from the same data with the same method.

Four dials express a reader's priorities. Each runs from 0 to 100, and **50 is the paper**. With every dial at 50 and the advanced settings at their defaults, every number on every page is the pipeline's output for the paper, exported exactly. Any other setting is a scenario, recomputed in your browser, and every view can show the scenario, the paper's baseline, or the change between them (the **Show** control at the top of each page). The scenario lives in the page address, so a copied link reproduces it.

## What the paper's setting rests on

**The coalition.** {{n_coalition}} economies: {{n_core}} core members (the U.S.-leaning bloc of the UN-vote ideal points, with GDP of at least $100 bn) and {{n_partners}} partners. A partner is a member of a U.S. free-trade agreement, IPEF, the Minerals Security Partnership, the G7 or NATO with GDP of at least $100 bn, or a major supplier that does not lean towards China: at least 5% of world production, refining or exports in an upstream or midstream stage, or at least 10% of world reserves of a headline mineral or fuel. The reserves route adds Brazil (rare earths and graphite). The EU-27 is one node of the allied network, its members weighted by their production or export share in each stage, so the network has {{n_nodes}} nodes. The allied selection stays national: every coalition member, EU members included, gets its own pick. The PSI's coalition view averages over the core members.

**Policy activity (the Policy index).** IEA Policies and Measures (PAMS, weight 3) and the Climate Action Tracker (CAT, weight 2), renormalised over the components present in each cell. The Tracker part depends on the technology: a strong climate rating scores high for low-emission technologies and low for coal, oil and gas. It measures how much policy a government has in a sector, not whether the policies work. IEA PAMS is required; CAT is added where it rates the country. Each scored economy's index therefore rests on **IEA PAMS + CAT** ({{n_policy_both}} economies in this build) or on **IEA PAMS only** ({{n_policy_pams_only}}); the Strategic Index and Country pages show which (the *policy basis*). The Strategic Index ranks policy within an economy, so the basis does not move one economy against another, but the two bases are different readings: compare policy levels across economies only within one basis. NIPO (Global Trade Alert) and the dual-use score are not components.

**The Strategic Index and the endowment screen.** The index keeps the need reading, $1-ES$: it favours the sectors where an economy is least secure. In an extraction stage an economy without the resource sits at the energy-security floor, so its need is high in exactly the sectors it cannot supply. The allied selection (each coalition member's top upstream or midstream sector, Geothermal and Green Hydrogen aside) therefore applies an **endowment screen**: an upstream sector qualifies only where the economy has recorded reserves or production of that sector's resources (reserves from EI for oil, gas and coal, from the USGS *Mineral Commodity Summaries 2026* for critical minerals and from the Red Book for uranium; production from the critical-minerals production theme). The index values do not change. A screened-out cell keeps its rank in the economy's own list and is flagged *no resource base*. In this build {{n_screened_out}} of the {{n_screened}} scored cells in screened upstream sectors have no recorded resource base, and {{n_picked}} members have a pick ({{n_picked_upstream}} of them upstream). The advanced panel can turn the screen off, the paper's robustness variant.

**Data choices in this build.**

* Critical-mineral reserves come from the USGS *Mineral Commodity Summaries 2026*; the coalition's reserves route reads the same source.
* Electric Grid upstream is a copper and aluminium material-input basket. Electricity (HS 271600), its only code before, is excluded.
* Electric Grid midstream excludes HS 851762 (telecom switching and routing equipment).
* Hong Kong is left out of the exporter-reported Comtrade export measures (export size, RCA, export concentration) as a re-export hub. Bilateral (dyad) trade in the partnership indices and the network is unchanged.
* The trade-concentration (HHI) index is ranked across the cells of the 11 headline technologies only.
* Friendshore reads cross-border manufacturing investment from CM-FDI (P. Bandara and the Net Zero Industrial Policy Lab, Johns Hopkins University, edition 2026.10.05, CC BY 4.0) in Solar, Wind, Batteries and Electric Vehicles midstream: the 905 manufacturing projects announced, under construction, completed or operational (closed, paused and cancelled ones dropped; all announcement years; a joint venture counts half to its listed parent). They give the investment tie and the Chinese-parent ownership exposure described below; every other cell keeps the IMF direct-investment position. Economic Opportunity does not read CM-FDI.
* Upstream trade baskets now exist for Electric Vehicles (battery-mineral ores and magnet rare earths), Wind (rare earths), Nuclear (uranium ores and yellowcake, moved from Midstream), Solar (silicon metal) and Semiconductors (gallium, germanium and indium; descriptive only, not in the indices): 259 six-digit basket codes in all. Some codes count in more than one sector, so baskets are never added up across sectors.
* Fossil-fuel production that the Energy Institute's *Statistical Review* does not report is taken from EIA International Energy Statistics, flagged as such and paired by year.
* The network's diversification weight $\lambda_0$ = {{lambda0}} is the value the network script sets, calibrated on the final inputs (12 October 2026) by the rule "the smallest $\lambda$ whose designed median concentration (HHI) does not exceed the median observed coalition export concentration": designed median HHI 0.1520 against observed 0.1556 over the 18 stages with a trade basket. The pick is knife-edge (0.77 misses by 0.0002), so small input changes can move it.
* The final runs (10-12 October 2026) corrected the theme tables found faulty while Sector Insights was built: mineral production as a demand-weighted world share, future-demand keys that collided between sources, IEA 2035 manufacturing shares over the world total including "Other", the electric-vehicle midstream placeholder region, the import-share variables (now self-sufficiency and the net-export ratio) and a tie in the energy-imports term, New Zealand's lending rate, and IMF price units taken from the indicator metadata. Every index here is built from the corrected tables.

## How a dial moves a weight

Every weight a dial owns is multiplied by

$$m(d) = 4^{(d-50)/50},$$

so $m(0)=0.25$, $m(25)=0.5$, $m(50)=1$, $m(75)=2$ and $m(100)=4$. Moving a dial 25 points halves or doubles every weight it owns, symmetrically. At 50 the multiplier is exactly one, so the baseline is reproduced bit for bit.

Only the five Strategic Index weights are renormalised explicitly, so that they keep summing to one:

$$w_k = \frac{w^0_k\, m_k}{\sum_j w^0_j\, m_j}\sum_j w^0_j .$$

Every other weight set (the Policy composite, the Friendshore and Opportunity components, the PSI, the network's producer score, edge weight and demand mix) is already a weighted mean that divides by the sum of its weights, so scaling one weight changes its share and nothing else.

Hawkishness and bullishness move the two ends of the same three ratios: Friendshore against Opportunity in the PSI, Friendshore against Opportunity in the network's edge weights, and need against size in the network's demand. With both dials at the same setting those ratios are the paper's.

## Hawkishness: security first

A hawk worries about supply security and coercion. Raising the dial puts more weight on where a country is insecure, on trusted (aligned) suppliers, on secure producers and on spreading production across more members. Inside Friendshore it scales the two security components alike: UN-vote alignment and the Chinese-parent ownership component (the share of a supplier's foreign-owned clean-tech factories whose parent is headquartered in China, Hong Kong or Macao, scored as one minus the share). The ownership component's on/off switch is a robustness setting of the pipeline, not a dial.

| What it changes | Formula | Paper | At 0 / 100 |
|---|---|---|---|
| Strategic Index weight on security need $(1-ES)$ | $0.3\,m_H$, then SI renormalisation | 0.30 | 0.097 / 0.632 |
| Friendshore weight in the PSI | $0.4\,m_H$ | 0.4 of 1.0 | share 0.14 / 0.73 |
| Friendshore share of network edge weights | $0.5\,m_H$, normalised with PO | 0.50 | 0.20 / 0.80 |
| Geopolitical alignment weight inside Friendshore | $w^0_{geo}\,m_H$ | {{geo_weight}} | (no effect when the build has no alignment component) |
| Chinese-parent ownership weight inside Friendshore | $w^0_{cpe}\,m_H$ | {{cpe_weight}} | {{cpe_lo}} / {{cpe_hi}} |
| Network diversification penalty $\lambda$ | $\lambda_0\, m_H$ | {{lambda0}} | {{lambda_lo}} / {{lambda_hi}} |
| Need share of network demand | $0.5\,m_H$, normalised with size | 0.50 | 0.20 / 0.80 |
| Resilience (own ES) weight in the producer score | $0.2\,m_H$ | 0.2 | effective 0.09 / 0.62 |

## Bullishness: opportunity first

A bull is after markets and growth. Raising the dial weights economic opportunity, export markets and the size of the economy as a source of demand.

| What it changes | Formula | Paper | At 0 / 100 |
|---|---|---|---|
| Strategic Index weight on Economic Opportunity | $0.3\,m_B$, then SI renormalisation | 0.30 | 0.097 / 0.632 |
| Partnership Opportunity weight in the PSI | $0.4\,m_B$ | 0.4 | share 0.14 / 0.73 |
| Opportunity share of network edge weights | $0.5\,m_B$ | 0.50 | 0.20 / 0.80 |
| EO weight in the producer score | $0.5\,m_B$ | 0.5 | effective 0.38 / 0.91 |
| Size share of network demand | $0.5\,m_B$ | 0.50 | 0.20 / 0.80 |

## Populism: policy signals and onshoring

A populist reads governments' industrial-policy signals, wants manufacturing (the midstream) at home and prefers self-supply.

| What it changes | Formula | Paper | At 0 / 100 |
|---|---|---|---|
| Strategic Index weight on policy $POL^*$ | $0.2\,m_P$, then SI renormalisation | 0.20 | 0.058 / 0.50 |
| IEA Policies and Measures weight in the Policy composite | $w^0_{IEA}\,m_P$ (other components unchanged) | {{iea_weight}} | |
| Stage weights (midstream emphasis) | $\lambda_s = \lambda_{max}(\lambda^0_s/\lambda_{max})^{m_P}$ | Up 0.50, Mid 0.75, Down 0.25 | Up 0.68 / 0.15; Mid 0.75; Down 0.57 / 0.01 |
| Policy term in the network producer score | weight $0.3\max(0,(P-50)/50)$; on only above 50 | 0 (off in the paper) | 0 / 0.3 |
| Home bias: weight of self-supply in each stage | $\min(1,\, m_P \times$ median edge weight of the stage$)$ | the solver's default | 0.25 / 4 times the median |

The stage transform keeps the midstream weight, keeps every value in $(0, 0.75]$ and widens (populism up) or narrows (down) the gap to the other stages. The network's policy term cannot be scaled because the paper sets it to zero, so it ramps linearly from 0 at 50 to the design's original 0.3 at 100; a member without a Policy value then takes the coalition median of the stage, as in the pipeline.

## Greenness: climate first

A green reader rewards low-emission technologies and climate-policy ambition, and penalises partnerships in emission-intensive technologies.

| What it changes | Formula | Paper | At 0 / 100 |
|---|---|---|---|
| Strategic Index technology (GHG) weight $g_t$ | $0.1\,m_G$, then SI renormalisation | 0.10 | 0.027 / 0.308 |
| Climate Action Tracker weight in the Policy composite | $w^0_{CAT}\,m_G$ (no effect where the index rests on IEA PAMS only) | {{cat_weight}} | |
| Friendshore climate penalty (each side) | $(1-g_t)\,CPI_x \times 0.10\,m_G$ | {{fs_penalty}} | 0.025 / 0.40 |
| Opportunity climate penalty | $(1-g_t)\,CPI_j \times c_0\,m_G$ | {{po_penalty}} | 0.0125 / 0.20 |
| Fossil exclusion (coal, oil, gas) | automatic at greenness 90 or above | off | off / on |

Excluding whole technologies is a change of regime, not of weight, so it sits at the extreme of the dial. When it is on, coal, oil and gas are dropped from the Strategic Index (like the paper's excluded sectors, with policy re-ranked over the remaining sectors), from the partnership best-five summaries and from the network. The pillar values themselves do not change: they are measurements, not preferences. The advanced panel can force the exclusion on or off.

## Presets

The preset menu sets the four dials at once: **Hawk** (hawkishness 90), **Bull** (bullishness 90), **Populist** (populism 90), **Green** (greenness 90, which also drops coal, oil and gas) and **Green hawk** (hawkishness and greenness 80). **Reset to paper baseline** returns every dial and advanced setting to the paper's.

## The formulas the explorer recomputes

**Pillars.** For pillar $P \in \{ES, EO\}$ and cell $(i,t,s)$, with category scores $c_{itsk}$ and weights $w^P_k$ over the categories observed for the cell,

$$P_{its} = \frac{\sum_{k \in \mathcal{C}^P_{its}} w^P_k\, c_{itsk}}{\sum_{k \in \mathcal{C}^P_{its}} w^P_k}.$$

The category weights are in the advanced panel; the explorer recomputes the pillars exactly from the stored category scores. A cell is rankable if the economy is sovereign and the imputed share of its weight is at most $\tau$.

**Policy.** $POL_{its} = \sum_v \omega_v x^v_{its} / \sum_v \omega_v$ over the components observed for the cell, with the component weights in use shown on the Strategic Index page ({{policy_weights}} in this build). An economy is scored when IEA PAMS is observed for at least one of its cells ({{n_policy}} economies; {{n_policy_none}} others have no Policy index and so no Strategic Index); its policy basis names the components its index rests on.

**Strategic Index.** With $POL^*_{its}$ the S-curve of $POL_{its}$ across country $i$'s own in-sample cells,

$$SI_{its} = w_{pol}\,POL^*_{its} + w_{eo}\,EO_{its} + w_{need}\,(1-ES_{its}) + w_{sc}\,\lambda_s + w_{tech}\,g_t ,$$

with the paper's weights $(0.2, 0.3, 0.3, 0.1, 0.1)$ at the baseline ({{n_si_cells}} scored cells for {{n_si_countries}} economies in this build). The SI ranks a country's own sectors; it is not designed to compare levels across countries. A sector constant across countries (degenerate) is excluded and the index rebuilt, as in the pipeline. The **allied selection** is each coalition member's highest-SI upstream or midstream sector (ties kept), outside Geothermal and Green Hydrogen and, with the endowment screen on, outside the upstream sectors without a recorded resource base.

**Friendshore and Partnership Opportunity.** Friendshore rates partner $j$ as a supplier to reporter $i$: a weighted mean of $i$'s imports from $j$, $i$'s opportunity, $i$'s security need, $j$'s opportunity, $i$'s outward investment in $j$ (and alignment, and in four midstream cells the Chinese-parent ownership exposure of $j$, where the build has them), less the climate penalty on both sides, then an S-curve across all world dyads of the technology and stage. Outward investment is $i$'s IMF direct-investment position in $j$, except in Solar, Wind, Batteries and Electric Vehicles midstream, where it is the tie of parents headquartered in $i$ to their manufacturing projects in $j$ in the Cleantech Manufacturing FDI Dataset (CM-FDI): project count and disclosed capital, ranked within the reporter. In the same four cells the exposure $E_{jt}$ is the share of $j$'s foreign-invested manufacturing in technology $t$ whose parent is headquartered in China, Hong Kong or Macao (by disclosed capital where disclosed, by project count otherwise; missing, not zero, below two projects; 1 for China, Hong Kong and Macao themselves). It enters as $1-E_{jt}$ with weight 1, beside alignment at 2: plants in two supplier countries owned by the same Chinese parents do not diversify supply away from China. Partnership Opportunity rates $j$ as a market for $i$'s exports, from $i$'s exports to $j$, $i$'s opportunity and $j$'s security need with weights $(3, 3, 1)$, less the market's climate penalty. Each dyad is summarised by the mean of its five best cells.

**Partnership Strength Index.**

$$PSI_j = \frac{\sum_{k \in K_j} w_k\, C^k_j}{\sum_{k \in K_j} w_k}, \qquad (w_{FS}, w_{PO}, w_{DP}) = (0.4\,m_H,\ 0.4\,m_B,\ 0.2).$$

**Network design.** For each upstream and midstream stage, the design chooses producers and flows to maximise

$$J_{ts} = \sum_{p}\sum_{c} f_{pc}\,\bigl(a_p + 0.5\,W_{pc}\bigr) - \lambda\,\mathrm{HHI},$$

subject to every consumer's demand weight being met, producer shares between {{min_share}} and the maximum share, at least the minimum number of producers, and at least two suppliers per consumer. The producer score $a_p$ mixes the within-coalition S-curves of EO, ES and (when populism is above 50) Policy; the edge weight is $W_{pc} = w_{FS}\,FS_{cp} + w_{PO}\,PO_{pc}$; demand mixes security need and economic size. An outer loop caps the number of stages in which a member is among the five largest producers (2 to 10, rising with log GDP).

## Advanced settings

| Setting | Default | Effect |
|---|---|---|
| ES and EO category weights | `config/weights.yml` | exact pillar recompute from category scores |
| Rankability threshold $\tau$ | {{tau_text}} | rankable = sovereign and imputed share $\le \tau$; the SI sample follows |
| Fossil exclusion | Auto (greenness $\ge$ 90) | Never / Always override the dial |
| Endowment screen (allied picks) | {{screen_default}} | an upstream sector is a member's pick only with recorded reserves or production of its resources; the index itself is unchanged |
| PSI viewpoint | {{psi_view}} | partner-level PSI averaged over coalition or all reporters |
| Network $\lambda$ override | from the hawkishness dial | sets $\lambda$ directly |
| Max producer share / min producers | {{max_share}} / {{min_producers}} | stage constraints of the design |
| Portfolio caps | on | the outer loop above |

Fixed throughout: minimum producer share {{min_share}}, at least two suppliers per consumer each providing at least {{eps_share}} of its demand, node weight 1 and edge weight 0.5 in the objective, development potential weight 0.

## Exactness and the solver

* At the paper's settings the explorer shows the pipeline's pillars, Policy index, Strategic Index, Friendshore, Opportunity and PSI exactly; the export script checks this every time the data are refreshed, and the result is on the Data page. The checks include each economy's policy basis, the endowment-screen flags of every Strategic Index cell and the allied selection, pick by pick, against the paper's table.
* Scenarios are recomputed in your browser by a TypeScript port of the R reference engine (`R/dashboard/`). The port is tested against the R engine on fixed scenarios: pillars, Policy and the Strategic Index agree to within $10^{-9}$, the Friendshore and Opportunity arrays bit for bit (the inputs are stored losslessly), and the greedy network's producer sets exactly. When the page loads, it also recomputes the baseline and compares it with the exported values; the result is on the Data page.
* The paper's network is solved exactly, as a mixed-integer program, which takes seconds to minutes per stage. To stay interactive, a scenario network is re-designed with a fast greedy solver for every stage; exact re-solves are not available in the browser. At the paper's settings the greedy design is close to the exact one but not identical, so by default the explorer compares a greedy scenario with the **greedy design at the paper's settings** (solver-neutral); when this build ships the paper's exact design, "Compare with" can switch to it to include the solver difference.
* Development Potential is held at the pipeline's value under every scenario.
* When you change the ES or EO category weights, the pillars and the Strategic Index are recomputed exactly; unless the build supports it, the partnership and network layers keep the paper's pillar-derived components, and the page says so.
* A partnership's rankability uses the imputed share of its best-five cells at the paper's settings; when a scenario changes which cells form the best five, this is an approximation.

## Read more

The paper sets out the framework (section 2: equations for the pillars, the Strategic Index, the PSI and the network), the opportunity-security space (section 7), the Policy and Strategic indices (section 8), the bilateral indices (section 9) and the network design (section 10).
