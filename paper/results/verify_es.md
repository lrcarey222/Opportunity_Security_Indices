# Verification log: theme "es"

Verifier started 2026-10-06. Files under review:
- scripts/paper/fig_es.R
- paper/figures/es_{distribution,contrib,heatmap,top}.{pdf,png}
- paper/results/es.json
- paper/results/text/es.tex
- paper/tables/es_leaders.tex

## Step 0: inventory
All files present (script 899 lines, text 267 lines, table 52 lines).

## Step 1: clean re-run
`Rscript --vanilla scripts/paper/fig_es.R` from the worktree root: exit 0, 35 s, regenerated
all four figures (pdf + png), es.json and tables/es_leaders.tex. The regenerated es.json and
es_leaders.tex are byte-identical to the copies present before the run (deterministic).

## Step 2: independent recomputation (scratchpad/ves/indep.R; own code, base lm/rank, countrycode ISO3)
All matched the JSON and the reported headline numbers:
- 6,228 cells, 219 economies, 30 pairs; one constant pair (Nuclear upstream, 0.704 for 217 economies), 0 unscored; 29 rankable pairs, 6,011 cells; 109-219 economies per pair. OK
- 0 fully imputed cells; mean imputed weight share 0.419 / 0.236 / 0.270 (Up/Mid/Down); 41.40% of category scores with an imputed component. OK
- Median ES 0.248 / 0.401 / 0.415; skewness 1.28 / 0.641 / 0.265. OK
- R2: pair FE 0.573, country FE 0.149, both 0.720; country FE on within-pair deviations 0.340; pair FE by stage 0.421 / 0.593 / 0.258; country FE by stage 0.346 / 0.234 / 0.215 (text: 35 and 22 per cent). OK
- China leads 11 (3 up, 5 mid, 3 down); next leaders 2 each; member first 11, top-3 27, top-10 29; outside top 3: EV midstream and Nuclear midstream (best member rank 4). OK
- Coalition mean ES 0.363/0.509/0.497 vs 0.241/0.408/0.418; member pct 0.775/0.825/0.657. OK
- Member means USA 0.621, AUS 0.536, CAN 0.532, IDN 0.511, IND 0.500 ... KEN 0.336; JPN highest midstream member mean 0.614; KOR/JPN upstream 0.253 after KEN 0.222. OK
- USA pct 0.962, top-10 in 21, first in 2; CHN pct 0.931, top-10 23, first 11, midstream pct 0.987; USA above CHN in 8 of 29. China ranks: Oil up 31, Gas up 31, Gas down 28, GH2 down 68, Oil down 213/213. USA: Nuclear mid 33, Coal mid 27, Grid mid 21, Oil down 24. OK
- Category shares (5 cells with ES=0 dropped; 1,963/1,999/2,044 cells): upstream EP 21.9, MT 20.8, Trade 20.3, FD 17.5, EI 14.5, Reserves 3.0 (w 20.6), Production 1.7 (w 16.6); midstream Trade 67.0, FD 32.0; downstream Consumption 56.1, EA 25.4, Reserves 10.1, Investment 8.3 (w 19.1). OK

## Step 2b: every number in text/es.tex checked against es.json (scratchpad/ves/jsoncheck.R)
All quoted numbers are present in es.json and match at the quoted precision, including the
leaders (table), top-ten panels (4 members in Oil up; 6 and 7 in Batteries/Solar mid; MYS, THA,
HUN), the top-ten gap shares (Reserves 0.566, EI 0.259, Trade 0.175; Investment 0.568 / 0.501),
China/US category scores, China Oil downstream components (5/9 Consumption weight; 0 and 1
economies strictly below), GH2 midstream resolution (12 distinct of 109, 89.9% at mode, USA rank
12 shared by 98), Consumption median 17 distinct values, Energy Prices 0.19-0.81 and level shift
0.025-0.068, cell-constant weights 0.20/0.20/0 and shares 43%/25%, Spearman medians and pairs.
Mineral-weight claims (copper for Grid/Solar among minerals with reserve tables; PGMs for GH2)
checked against mineral_demand_shares_base_year and R/categories/reserves/reserves.R mineral list.
Investment split tbd fill: 0.115 / 0.885 -> "11.5 / 88.5 per cent" OK.

Problems found in the text (to repair):
- [major] "Part of every upstream and midstream score does not vary across economies": false for
  Green Hydrogen midstream (only Foreign Dependency; cell-constant weight 0, see
  cell_constant_inputs$mean_effective_weight_by_pair).
- [major] "Downstream the ordering is less systematic: ... country fixed effects explain 22 per cent
  of the downstream variance against 35 per cent upstream": selective (midstream is 23 per cent,
  same as downstream) and the country-only R2 conflates pair effects. The within-pair (partial)
  share is the right metric: ~0.60 / 0.66 / 0.28. Needs a new JSON key.
- [minor] "from four to seven categories upstream" is for rankable pairs; JSON
  categories_per_cell_by_stage gives 1-7 upstream (Nuclear upstream). Add a rankable-only key.
- [minor] US/China paragraph ends with "it is the reason the pillar's leaders differ so much by
  stage": overclaim (China also leads three upstream mineral pairs; the US is top-3 in three
  midstream pairs; leaders differ by stage because the category bundles differ). Soften.
- [minor] es_distribution caption: "gray dots show every economy" - gray dots are non-members
  only; legend has no entry for the gray dots.
- [minor] es_heatmap caption: Canada and Kenya means are over 28 pairs, not 29.
- [minor] es_contrib caption: "scores above the cell's other categories" -> above the cell's
  overall score (share > weight iff category score > ES).

## Step 3: figures (PNG inspected with Read; checked against anti-patterns.md and figure rules)
- es_distribution (6.5 x 6.6 in): stage colours blue/orange/aqua, 0-1 axis, one x-axis, faceted
  by stage, rows ordered by median, constant Nuclear upstream row labelled. No overlap/clipping.
  [minor, repaired] the legend had no entry for the gray (non-member) dots although they are a
  series; added "Other economy" to the shape legend (first attempt with colour = NA silently
  dropped 5,317 points - caught by the ggplot warning and fixed with colour = fill).
- es_contrib (6.5 x 3.4 in): one colour per stage facet (no category palette needed), tick legend
  present, selective labels (>= 10%), x 0-85% shares. OK.
- es_heatmap (6.5 x 5.6 in): single-hue sequential blue on fixed 0-1 limits with colourbar,
  constant column gray and labelled, crosses for the two missing member cells, China row
  separated. OK.
- es_top (6.5 x 3.5 in): lollipops from 0 on 0-1, members in stage colour, others gray, legend,
  cell median line labelled once per panel, US/China appended below a hairline with ranks. OK.
No dual axes, no titles inside plots, <= 8 colours, text >= 7.5 pt.

## Step 4: text fragment and table
LaTeX lint (scratchpad/ves/texlint.py): environments balanced, braces balanced, $ even, labels
fig:/tab: prefixed, refs all exist (eq:cat_decomp, eq:imputed_share, eq:scurve, eq:scurve_zero,
subsec:norm_pillar, subsec:es_consumption, sec:network, tab:es_structure in sections 04/05/10),
citations iea2024etp and iea2025gcmo present in references.bib, no non-ASCII. Packages
(booktabs, threeparttable, graphicx, natbib) loaded in main.tex; graphicspath figures/. No LaTeX
engine on this machine, so not compiled.

## Repairs made
1. scripts/paper/fig_es.R: new JSON keys coverage$categories_per_cell_by_stage_rankable
   (Up 4-7, Mid 1-3, Down 1-4) and variance_decomposition$partial_r2_country_given_pair (0.344)
   and $partial_r2_country_given_pair_by_stage (0.597 / 0.657 / 0.280); legend entry for gray
   dots in es_distribution. Re-ran clean (exit 0); JSON diff vs. pre-repair copy: only the three
   added keys; es_leaders.tex unchanged.
2. text/es.tex:
   - "four to seven categories upstream" now explicitly over the rankable pairs (new key).
   - "Part of every upstream and midstream score does not vary" -> excludes Green Hydrogen
     midstream (Foreign Dependency only; cell-constant weight 0).
   - Downstream-ordering sentence now quotes the partial R2 of country effects given pair effects
     for all three stages (28 vs 60 and 66 per cent) instead of the selective 22-vs-35 comparison.
   - US/China closing sentence softened (no longer "the reason the leaders differ by stage").
   - Captions: es_distribution gray dots = non-members; es_heatmap ordering over 28 pairs for
     Canada and Kenya; es_contrib bar-vs-tick reading corrected (share > weight iff category score
     > cell score); clarity fix on "Japan and South Korea (0.25 each)".

## Label mapping for integration (sections/05_energy_security.tex placeholders -> fragment floats)
fig:es_distribution -> es_distribution.pdf; fig:es_category_contrib -> es_contrib.pdf;
fig:es_heatmap -> es_heatmap.pdf; fig:es_top -> es_top.pdf; tab:es_leaders -> tables/es_leaders.tex
(\input inside the fragment). The four placeholder floats in section 05 must be deleted when the
fragment is pasted, or the labels will be duplicated.

Verdict: approve with repairs.

---
# Second verification pass (verifier re-run, 2026-10-06 afternoon)

Starting state: fig_es.R 912 lines, text/es.tex 272 lines, es_leaders.tex 52 lines, all four
figures present (09:14). Pre-run copies saved to scratchpad/ves2/pre for diffing.

## 2.1 Clean re-run
`Rscript --vanilla scripts/paper/fig_es.R` from the worktree root: exit 0, 29 s, no warnings;
regenerated the four figures (pdf + png), es.json and tables/es_leaders.tex. es.json and
es_leaders.tex are byte-identical to the pre-run copies (deterministic; jitter seeded).

## 2.2 Independent recomputation (scratchpad/ves2/indep2.R, indep3.R; own base-R code,
## countrycode ISO3, does not read es.json)
All of the following match the JSON and the text at the quoted precision:
- Coverage: 6,228 cells, 219 economies, 30 pairs; Nuclear upstream constant (0.704316 for 217
  economies), 0 unscored; 6,011 rankable cells in 29 pairs; 109-219 economies per pair; 0 fully
  imputed cells; mean imputed weight share 0.419/0.236/0.270; 41.4% of category scores imputed;
  categories per rankable cell 4-7 / 1-3 / 1-4; one category set per pair in every pair.
- Distribution: medians 0.248/0.401/0.415, skewness 1.276/0.641/0.265; pair medians EV up 0.115,
  GH2 up 0.137, oil up 0.364; Trade-only midstream pairs (coal, gas, oil, nuclear) 0.488-0.579.
- Variance: pair FE 0.573, country FE 0.149, both 0.720, partial R2 of country | pair 0.344;
  by stage pair FE 0.421/0.593/0.258, partial 0.597/0.657/0.280.
- Leaders: no ties at the top; CHN leads 11 (3 up/5 mid/3 down), next CHL/IDN/NLD/QAT/SAU 2;
  a member first in 11, top 3 in 27, top 10 in 29; outside top 3: EV mid, Nuclear mid (rank 4).
- Coalition: member mean ES 0.363/0.509/0.497 vs 0.241/0.408/0.418; member pct 0.775/0.825/0.657;
  member means USA 0.621, AUS 0.536, CAN 0.532, IDN 0.511, IND 0.500 ... KEN 0.336; JPN highest
  midstream 0.614 (USA 0.589, FRA 0.579); lowest upstream KEN 0.222, KOR 0.2525, JPN 0.2533.
- USA pct 0.962, top-10 21, first 2; CHN pct 0.931, top-10 23, first 11, midstream pct 0.987;
  USA above CHN in 8 of 29.
- Category shares (5 ES=0 cells dropped, 1,963/1,999/2,044 cells): match to 3 d.p.
- Cell-constant inputs: mean effective weight 0.201/0.196/0; mean cell share 0.433/0.249/0;
  zero only in GH2 midstream and downstream; critmin_hhi non-modal only for Taiwan and Reunion;
  Energy Prices score 0.186 (gas) to 0.814 (grid), level shift 0.0252 (batteries) to 0.0678 (oil).
- Within-pair Spearman: median 0.421/0.459/0.135; Reserves-Production 0.9994-0.9999 in all six
  mineral pairs, 0.924 among positive scorers (median n 18); FD-MT 0.515; mid FD-Trade 0.542;
  down EA-Consumption 0.120, Reserves-Consumption -0.121; category-vs-ES FD 0.849, MT 0.807,
  mid Trade 0.953, down Consumption 0.841. Zero shares: Reserves 0.862, Production 0.917.
- Top-ten gaps: Oil up Reserves 0.566, EI 0.259, Trade 0.175, EP 0; Investment 0.568 (batteries
  mid) and 0.501 (solar mid); Investment mean all 0.065/0.045, top ten 0.629/0.580; China vs US
  oil-up scores and batteries/solar FD contributions (0.371 vs 0.210; 0.398 vs 0.275) match;
  China's lead over #2 is 94% (batteries) and 80% (solar) Foreign Dependency.
- China Oil downstream: Consumption weight 0.556 (5/9); 0 economies below on elec_growth_index,
  1 on installed_cap_index. GH2 midstream: 12 distinct of 109, mode share 0.899, USA rank 12
  shared by 98. Downstream Consumption median 17 distinct values per pair.
- Investment split (tbd fill): pipeline's investment_critical_minerals_split() is what
  scripts/10_build_themes.R passes to the investment theme; 0.11533 / 0.88467 -> "11.5 per cent
  to batteries and 88.5 per cent to electric vehicles" OK.
- Heatmap caption: allied_network_specialization.csv has 33 stages; the three without an ES score
  are Electric Grid downstream, Geothermal upstream and midstream. OK.

## 2.3 LaTeX lint (scratchpad/ves2/texlint.py)
Braces 113/113, environments balanced, $ even, labels fig:/tab:, every \ref/\eqref resolves
(sections 04, 05, 10), citations iea2024etp, iea2025gcmo in references.bib, ASCII only.

## 2.4 Figures (PNG + es_top PDF inspected; anti-patterns.md and figure rules)
- es_distribution, es_contrib, es_heatmap: no overlap or clipping, stage colours blue/orange/aqua,
  0-1 axes and fixed 0-1 colour limits, one x-axis, legends present, sensible ordering (median
  within stage; member mean), no titles, no dual axes. Annotation text was 6.8-7.4 pt and heatmap
  axis text 7.5 pt: [minor, repaired] raised to 8 pt (geom_text size 2.8; axis 8).
- es_top [major, repaired]: the figure was a lollipop chart of total ES, but the section's
  placeholder (fig:es_top, "bars stacked by weighted category contribution"), the Leaders tbd
  ("with their category contributions") and the parallel eo_top all call for a decomposition, and
  the text's top-ten paragraph quoted decomposition numbers that no figure showed. Redrawn as
  stacked bars of weighted contributions (segments sum exactly to ES, asserted in the script):
  gray "Cell-constant inputs" (Energy Prices + Trade HHI; identical within a pair, asserted),
  then the country-varying part of Trade, Reserves, Energy Imports, Foreign Dependency,
  Investment (6 classes; Trade/Investment/FD in the same colours as eo_top), members in bold
  (ggtext, as eo_top), US/China appended below a hairline with rank, and a reference bar with the
  mean of each segment over all economies of the pair. Faceted vertically (space = free_y) at
  6.5 x 5.4 in; first render clipped the legend ("Investment") -> two-row legend. Re-inspected PNG
  and PDF: legible, no clipping.
- New JSON keys: top_cells_plotted_segments, top_cells_constant_segment (Oil up 0.135, batteries
  mid 0.044, solar mid 0.040), top_cells_segment_mean_all_economies. All pre-existing keys are
  unchanged (JSON diff); es_leaders.tex unchanged.

## 2.5 Text review (text/es.tex) and repairs
- [major, repaired] "they supply 43 per cent of the average upstream score and 25 per cent of the
  average midstream score" and the caveat "more than two-fifths of the average upstream score":
  the JSON value is the mean of cell shares (0.433/0.249); the share of the average score (ratio
  of sums) is 0.364/0.270, so "more than two-fifths of the average upstream score" was false as
  worded. Now "on average 43 per cent of an upstream cell's score" (both places); likewise
  "account on average for nearly all of a cell's score" in the drivers paragraph.
- [major, repaired] es_top text: the two top-ten paragraphs now refer to the decomposition drawn
  in the figure (gray base 0.14 in Oil upstream, which accounts for none of the top-ten gap; the
  Trade gap share is its country-varying part). es_top caption rewritten for the new figure.
- [minor, repaired] Distribution paragraph lacked the interquartile ranges the section tbd asks
  for; added stage IQRs (0.15-0.32, 0.31-0.51, 0.33-0.51) and the within-pair upstream IQR range
  (0.03-0.09), which also substantiates "tight band".
- [minor, repaired] Midstream drivers omitted Investment (1 per cent of the score against a mean
  effective weight of 10 per cent; 86 per cent zeros in the median of its four pairs): added.
- [minor, repaired] "10 of the 42 nominal weight points" understated the upstream weight of
  Reserves + Production; added their mean effective weights (21 and 17 per cent).
- [minor, repaired] Coalition paragraph listed Canada among holders "across the minerals" (5th of
  members by mean mineral-pair rank, 7th and 9th in solar and wind) and Chile only in grid/solar
  (also 2nd member in batteries); reworded (Australia, US, Indonesia across most mineral pairs;
  Chile grid/solar/batteries; Canada with the oil producers).
- [minor, repaired] "among the most secure in almost every pair" overclaims for the US (outside
  the top ten in 8 of 29); now "rank near the top of most pairs".
- [minor, repaired] es_distribution caption: coloured dots are the members other than the US
  (the US is the triangle), not "the 23 coalition members".
- Number check (scratchpad/ves2/numcheck.py): every number token in the text matches a JSON
  value at the quoted precision except derived ones checked by hand (42 = sum of
  category_weights; 97 = usa_tie 98 minus the US; "42, 59" = r2 by stage as per cent; year 2026).
- Leaders table rebuilt independently (scratchpad/ves2/indep6.R): all 29 rows identical.
- LaTeX re-lint after edits: braces 115/115, environments balanced, refs and cites resolve.

## 2.6 Final state
Final clean run: `Rscript --vanilla scripts/paper/fig_es.R` exit 0 (run4.log). Verdict: approve
with repairs.

Label mapping (section 05 placeholders -> fragment floats; the integration stage must delete the
four placeholder floats in sections/05_energy_security.tex when pasting the fragment):
fig:es_distribution -> es_distribution; fig:es_category_contrib -> es_contrib;
fig:es_heatmap -> es_heatmap; fig:es_top -> es_top (now stacked by category, as specified);
tab:es_leaders -> \input{tables/es_leaders} inside the fragment.
tbd fills: investment split -> "11.5 per cent to batteries and 88.5 per cent to electric
vehicles" (JSON investment_critical_minerals_split); the four Results \tbd paragraphs
(Coverage, Distribution, Drivers, Leaders) -> replaced by the fragment, which covers each item
(coverage incl. 0 fully imputed cells, the one constant pair and categories per cell by stage;
medians, IQRs and FE variance shares; category shares and cell-constant weights; top ten with
category contributions, member heatmap with China).

---
# Third verification pass (verifier re-run after "Try again", 2026-10-06 evening)

Starting state: fig_es.R 958 lines, text/es.tex 287 lines, es_leaders.tex 52 lines, four figures
(pdf + png) dated 14:55, es.json 14:55. Pre-run copies saved to scratchpad/ves3/pre for diffing.

## 3.1 Clean re-run
`Rscript --vanilla scripts/paper/fig_es.R` from the worktree root: exit 0, 40 s, no warnings;
regenerated the four figures (pdf + png), es.json and tables/es_leaders.tex. es.json and
es_leaders.tex byte-identical to the pre-run copies (deterministic).

## 3.2 Independent recomputation (scratchpad/ves3/indep.R, indep2.R, indep3.R, split.R; base R,
## countrycode ISO3, own rank/percentile/lm/Spearman code; es.json not read)
All match the JSON, the compute agent's headline numbers and the text at the quoted precision:
- Coverage: 6,228 cells / 219 economies / 30 pairs; status ok 29, constant 1 (Nuclear upstream,
  0.7043155 for all 217 economies), 0 unscored groups; 6,011 rankable cells; 109-219 per pair;
  0 fully imputed; imputed weight share 0.419/0.236/0.270; 41.4% of category scores imputed;
  categories per rankable cell 4-7 / 1-3 / 1-4.
- Distribution: medians 0.2484/0.4008/0.4151; IQR 0.152-0.317, 0.308-0.513, 0.331-0.512;
  skewness 1.276/0.641/0.265; upstream within-pair IQR 0.033-0.090; upstream pair medians EV
  0.1154, GH2 0.137, oil 0.364; Trade-only midstream pairs (coal, gas, nuclear, oil) 0.488-0.579.
- Variance: R2 pair 0.5732, country 0.1493, both 0.7202, partial country|pair 0.3443; by stage
  pair 0.421/0.593/0.258, partial 0.597/0.657/0.280; pair-FE R2 net of constant inputs 0.602.
- Leaders: CHN 11 (3 up/5 mid/3 down), next five economies 2 each; member first 11, top-3 27,
  top-10 29; best member rank 4 in EV mid (USA) and Nuclear mid (FRA); no ties at the top.
- Coalition: member mean 0.3635/0.5088/0.4967 vs 0.2409/0.4084/0.4176; member pct
  0.775/0.825/0.657; USA 0.621, AUS 0.536, CAN 0.532, IDN 0.511, IND 0.500 ... KEN 0.336 (CAN,
  KEN 28 pairs); JPN midstream 0.614 > USA 0.589 > FRA 0.579; upstream KEN 0.222, KOR 0.2525,
  JPN 0.2533.
- USA pct 0.962, top-10 21, first 2; CHN pct 0.931, top-10 23, first 11, mid pct 0.987; USA
  above CHN in 8 of 29; CHN ranks Oil down 213/213, GH2 down 68, Oil/Gas up 31, Gas down 28;
  USA Nuclear mid 33, Coal mid 27, Oil down 24, Grid mid 21.
- Category shares (5 ES=0 cells dropped; 1,963/1,999/2,044 cells): upstream EP 0.219, MT 0.208,
  Trade 0.203, FD 0.175, EI 0.145, Reserves 0.030 (w 0.206), Production 0.017 (w 0.166);
  midstream Trade 0.670, FD 0.320, Investment 0.010 (w 0.098); downstream Consumption 0.561,
  EA 0.254, Reserves 0.101, Investment 0.083 (w 0.191).
- Cell-constant inputs: mean effective weight 0.201/0.196/0; mean cell share 0.433/0.249/0
  (ratio of sums 0.364/0.270, not quoted); EP score 0.186 (gas) to 0.814 (grid); level shift
  0.0252 (batteries) to 0.0678 (oil). Midstream category sets: GH2 = FD only, coal/gas/nuclear/oil
  = Trade only, the rest include Trade, so "every midstream score except GH2" holds.
- Spearman (within pair, all economies): stage medians 0.421/0.459/0.135; Reserves-Production
  0.9994-0.9999 (six mineral pairs), positive-only median 0.924 (median n 18); FD-MT 0.515;
  mid FD-Trade 0.542; down EA-Consumption 0.120, Reserves-Consumption -0.121; vs ES: FD 0.849,
  MT 0.807, mid Trade 0.953, down Consumption 0.841; zero shares Reserves 0.862, Production 0.917.
- GH2 midstream: 12 distinct of 109, mode share 0.899, USA rank 12 shared by 98; downstream
  Consumption median 17 distinct values per pair.
- es_top cells: Oil up SAU 0.792, 4 members in top ten, USA 11th 0.683, CHN 31st; gap shares
  Reserves 0.566, EI 0.259, Trade 0.175, EP 0; constant segment 0.1355 (identical for all 218);
  batteries mid CHN 0.782 / USA 0.610, 6 members, MYS THA HUN; solar mid CHN 0.801 / IND 0.647,
  7 members; Investment gap shares 0.568/0.501, mean score 0.065/0.045 vs top ten 0.629/0.580;
  FD contributions CHN 0.371 vs USA 0.210, CHN 0.398 vs IND 0.275; USA Investment 0.858 vs CHN
  0.802 (batteries). China oil downstream: Consumption weight 5/9; 0 economies below on
  elec_growth_index (per-capita capacity growth 2024-35), 1 on installed_cap_index (2024 level).
- Investment split (tbd fill): recomputed from the IEA 2026 workbook by my own renormalize-and-
  average code: 0.11533 / 0.88467; cross-checked against the stored corrected theme table
  data/processed/investment_momentum_tbl.rds, where upstream Batteries/(Batteries+EV) annual
  investment equals 0.1153 in all 98 country-years. "11.5 per cent to batteries and 88.5 per
  cent to electric vehicles" OK.
- Number check (scratchpad/ves3/numcheck.py): all 193 numeric tokens in text/es.tex match a JSON
  value at the quoted precision (only "2026", the run date, unmatched). Borderline roundings
  checked by path: EV upstream median 0.11535 -> 0.12; Investment mean 0.06548 -> 0.07, 0.04492
  -> 0.04.

## 3.3 Figures (PNG of all four + es_top PDF inspected; anti-patterns.md and figure rules)
- es_distribution (6.5 x 6.6 in): stage colours blue/orange/aqua, 0-1 axis, one x-axis, stage
  facets, rows ordered by median, legend covers all four point series, constant Nuclear upstream
  row labelled; no overlap or clipping; text >= 8 pt. OK.
- es_contrib (6.5 x 3.4 in): one stage colour per facet, tick legend, selective labels (>= 10%),
  shares on 0-85%; no clipping. OK.
- es_heatmap (6.5 x 5.6 in): single-hue blue on fixed 0-1 limits with colourbar; gray constant
  column labelled; crosses for CAN GH2 down and KEN GH2 mid; China separated; columns = the 30
  pairs (Grid downstream, Geothermal up/mid absent, as the caption says). OK.
- es_top (6.5 x 5.4 in): stacked contributions on 0-1, segments sum to ES (asserted in script),
  gray constant base identical within a pair, members bold (renders bold in the PDF, no literal
  markdown), US/China appended with rank, mean reference bar, two-row legend; no clipping. OK.
- [minor, not repaired] es_top's category fills reuse the blue/orange/aqua slots that elsewhere
  mean Upstream/Midstream/Downstream (Trade blue, Reserves orange, Energy Imports aqua). The
  stage colours themselves are consistent in every figure, the panels name their stage, and
  eo_top uses the same paper_cat[1:6] convention for its category segments, so I left it for the
  integration stage to decide paper-wide.
No dual axes, no titles inside plots, <= 8 colours per figure, honest 0-1 scales.

## 3.4 Text and table review; LaTeX
- Lint (scratchpad/ves3/texlint.py, specials.py): braces 81/81, environments balanced, $ even,
  labels fig:/tab: only, every \ref/\eqref resolves to sections 04, 05 or 10 or to the table,
  citations iea2024etp and iea2025gcmo exist in references.bib, ASCII only, no unescaped _ # &
  outside labels/math/texttt. No LaTeX engine on this machine, so not compiled.
- Claims checked against the figures and the recomputation: leaders and runners-up (table),
  "Saudi Arabia narrowly ahead of Australia and China" in solar downstream (0.70/0.70/0.69),
  Singapore second in coal and gas midstream, China's upstream mineral leads driven by FD, MT and
  Production (contributions above Australia's), the US top three in three midstream pairs,
  oil-downstream consumption variables (installed_cap_index = 2024 per-capita capacity,
  elec_growth_index = per-capita capacity growth 2024-35, R/categories/consumption).
- [minor, repaired] "together carry 10 of the 42 nominal weight points": 42 is the sum over all
  ten categories, including the downstream-only ones, so it understated the upstream weight of
  Reserves + Production (10 of 33 upstream points). Dropped; the sentence keeps the mean
  effective weights (21 and 17 per cent), which are in the JSON.
- [minor, repaired] es_top caption said the gray segment is "Energy Prices and the
  world-concentration term of Trade" for all three panels; Energy Prices is upstream only, so in
  the two midstream panels the gray segment is the Trade concentration term alone (checked: the
  only exactly constant variables are Trade HHI + EP in Oil upstream, Trade HHI in the two
  midstream cells). Caption made precise.
- After the edits: numcheck 191 tokens, all matched (except the year 2026); lint clean. The
  script was not changed in this pass, so no re-run was needed after the text edits.

## 3.5 Verdict: approve with repairs (two minor text repairs; no blocker or major found)
Label mapping (unchanged): fig:es_distribution -> es_distribution; fig:es_category_contrib ->
es_contrib; fig:es_heatmap -> es_heatmap; fig:es_top -> es_top; tab:es_leaders ->
\input{tables/es_leaders} inside the fragment. The four placeholder floats in
sections/05_energy_security.tex carry the same labels and must be deleted when the fragment is
pasted. tbd fills: investment split -> "11.5 per cent to batteries and 88.5 per cent to electric
vehicles" (verified); the four Results \tbd paragraphs -> replaced by the fragment.
