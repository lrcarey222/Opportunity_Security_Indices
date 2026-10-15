# Verification log: theme "eo"

Verifier pass started 2026-10-06. Files under review: scripts/paper/fig_eo.R, paper/figures/eo_{distribution,contrib,heatmap,top}.{pdf,png}, paper/results/eo.json, paper/results/text/eo.tex.

## Step 1. Clean re-run
- `Rscript --vanilla scripts/paper/fig_eo.R` from the worktree root: exit 0, 35 s, no warnings or errors in the log.
- Regenerated eo.json is byte-identical to the pre-run copy (deterministic); all four PDFs and PNGs rewritten (mtime 2026-10-06 09:11).
- Input index_outputs.rds mtime 2026-10-01 11:04 (corrected run).

## Step 3 (first look). Figures
Opened all four PNGs (eo_distribution, eo_contrib, eo_heatmap, eo_top). Detailed notes below after numeric checks.

## Step 2. Independent recomputation (own base-R code, scratchpad veo/indep.R; not reading the JSON)
Matched (to the precision quoted): 7,125 cells / 219 economies / 33 pairs / 212-219 per pair; all 33 pairs ok; 24 fully imputed (IMN 14, PRI 8, LIE 2); mean iota 0.436 (0.436/0.443/0.430); 40.0% > 0.5; 46.2% category scores imputed; stage medians, means and pooled IQRs; FE R2 (pair 0.731, tech 0.320, stage 0.035, tech+stage 0.354, country 0.117, pair+country 0.848, within 43.5%); ES 29 ok pairs / 6,011 cells, pair 0.573, country 0.149; level shares by stage; TD+TRL+EP weight 53.2/41.4/34.9; constant/imputed/observed weight split and Table tab:eo_weight_split (all 16 entries); six constant variables; leaders 23/5/3/1/1, margins (median 0.038, 0.051 China, nine < 0.01), five largest gaps, Chad margins; Table tab:eo_leaders (all 33 rows: leader, rank of best member); best member rank 1 in 7, <=3 in 28, median 2; top-ten emerging share 48.2% of 330, China in 30; stage Spearman 0.75/0.74/0.78 (216 economies); Spearman EO-pct vs Cost-pct 0.81 (219) / 0.89 (23); coalition/other/China mean pct 0.63/0.48/0.96 and EO 0.506/0.473/0.662, by stage 0.56/0.73/0.60; iota 0.26 vs 0.46, USA 0; imputation-rank Spearman median -0.25, range -0.73..0.16; constant segment 0.253/0.214/0.188/0.097; pair medians and IQRs.

Discrepancies found:
- D1 (major): within-pair variance shares. Independent values (absent category = 0, var over the n economies of the pair): Cost Competitiveness 52.9/35.6/22.8 per cent, not 53.1/35.7/22.9. Cause in fig_eo.R: `var(eo)` is taken over the long category-contribution table, where each economy is repeated k times, so the denominator is k(n-1)/(nk-1) times too small and the shares sum to about 1.004, not 1 (the JSON note and the caption claim they sum to 1).
- D2 (major): Australia's mean percentile is 0.4548, which rounds to 0.45; the text says 0.46.
- D3 (major): "Downstream electric vehicles is the one illustrative pair dominated by emerging economies": midstream solar has 7 emerging economies in its top ten (EV downstream 8), so "the one" is wrong.

- Repaired D1 in fig_eo.R: var(EO) now per pair over its economies (`pair_var` from eo_idx), with stopifnot checks that each pair has one category set, that each category has one row per economy and that the shares sum to 1 in every pair. Re-run: exit 0; mean_var_share now sums to 1.000 by stage and equals the independent values exactly (Cost 0.529382/0.356124/0.228157). Only the 15 variance-share entries changed in eo.json.
- Added to eo.json (coalition block) the numbers behind the text's claim that heavily imputed cells sit low: heavily_imputed_n_cells 2851, share below the median percentile 0.647, mean pct 0.429 vs 0.547, 4 of 330 top-ten slots, 0 leaders (independently matched).

Further text checks against the JSON/data:
- D4 (major): "In every pair some member ranks high" contradicts Table tab:eo_leaders (best member 21st in grid downstream, 12th and 9th in geothermal up/mid). Rewritten.
- Minor: "colored" -> "coloured" (British spelling elsewhere); BNEF regional composite observed for 12 economies in most pairs but 8 in nuclear -> "at most twelve".
- Added `n_economies_by_group` (China 1, Coalition 23, Other 195) to eo.json so that "195 other economies" is backed by the JSON.

## Step 3. Figures (after repairs; PNGs re-opened)
- eo_distribution: stage colours Upstream blue / Midstream orange / Downstream aqua; 0-1 axis; one series per facet (strip names the stage, so no legend needed); techs ordered by median; no clipping. Caption matches (box/whisker convention, 212-219 economies, all pairs ok).
- eo_contrib: three stage colours with legend; hairline x grid; the variance panel now sums to 100% per stage, as the caption states; * categories (TRL, Energy Prices) are zero in the right panel as drawn.
- eo_heatmap: sequential single-hue blue on a fixed 0-1 percentile scale with legend; dot legend for >0.5 imputed (93 cells); 0 fully imputed cells among the 792 shown (checked); China in a separate reference panel; no overlaps.
- eo_top: gray constants + 6 categorical slots (<= 8) with legend, white gaps between segments, bars from 0 (axis 0-0.85 is honest for bars); coalition members bold as stated. Repaired: y-axis country labels were 7.5pt (below the 8pt floor), now 8pt; re-rendered and checked, no overlap.
- Minor (not repaired): eo_top reuses blue/orange/aqua (the stage colours) for Trade/Production/Technology Demand. Only five non-stage slots exist for six categories, so this cannot be avoided without changing _paper_theme.R or merging categories.
- No dual axes, no titles inside plots, no numbers printed on every mark.

## Step 4. Text fragment
- Every number in eo.tex (217 distinct tokens, comments excluded) matches a JSON value at the displayed precision (scratchpad veo/numcheck.R; 0 unmatched). Key numbers also matched semantically in Step 2.
- Repairs: D1 variance shares (52.9/35.6/22.8; 17.1, 16.2; 29.2, 22.9; 25.9, 25.8; Investment 9.6/11.1/25.8); D2 Australia 0.45; D3 the claim that EV downstream is "the one" pair dominated by emerging economies, now 8 vs 7/4/3 from n_emerging_in_top10; D4 "in every pair some member ranks high", now "in most pairs" with the five exceptions named from tab:eo_leaders; the heavily-imputed claim now quantified; "market momentum rather than market scale" toned down to "at least as much as" and backed by the TD contributions of TUR/VNM/PHL vs CHN (0.333/0.295/0.281 vs 0.271); "colored" -> "coloured"; BNEF "at most twelve"; "Its largest segment" -> "The pair's largest segment".
- LaTeX: environments balanced (figure 4, table 2, threeparttable 2, tabular 2, tablenotes 2), braces 171/171, even $; labels fig:/tab: prefixed; no \cite; no non-ASCII; every \ref/\eqref target exists in paper/sections; tabular column counts match (5 and 10); siunitx/booktabs/threeparttable loaded in main.tex. No LaTeX compiler on this machine, so the fragment was not compiled.
- Label mapping (unchanged): eo_distribution -> fig:eo_distribution; eo_contrib -> fig:eo_category_contrib; eo_heatmap -> fig:eo_heatmap; eo_top -> fig:eo_top; new tab:eo_weight_split, tab:eo_leaders. No duplicates elsewhere apart from the Section 6 placeholders that the fragment replaces.

## Final state
fig_eo.R re-run clean (exit 0) after all edits; eo.json and the four figures were regenerated by that run. Verdict: approve with repairs.

---

# Second verifier pass (2026-10-06, afternoon)

Independent re-verification of the state left at 09:22 (fig_eo.R 09:21, eo.json 09:21, eo.tex 09:22). Prior pass above is treated as unverified.

## Step 1. Clean re-run
- `Rscript --vanilla scripts/paper/fig_eo.R` from the worktree root at 14:43: exit 0, 29 s, no warnings or errors.
- eo.json byte-identical to the 09:21 copy; all four PNGs byte-identical; PDFs rewritten. The script is deterministic and current outputs are what it produces.
- Input index_outputs.rds mtime 2026-10-01 11:04 (corrected run).

## Step 2. Independent recomputation (second pass; own base-R code in scratchpad veo2/indep*.R, no dplyr, JSON not read)
All of the following match the text and eo.json at the displayed precision:
- Coverage: 7,125 cells, 219 economies, 33 pairs (all status ok), 212-219 per pair; 24 fully imputed (Isle of Man 14, Puerto Rico 8, Liechtenstein 2); mean iota 0.4364 (Up 0.4361, Mid 0.4427, Down 0.4305); 40.0% of cells iota > 0.5; 46.2% of category scores imputed; categories per cell 3-7 / 3-6 / 2-7; 0 pairs with more than one category set; reconstruction errors 0 and 1.1e-16.
- Moments: medians 0.515/0.466/0.475, means 0.495/0.454/0.484, pooled IQR 0.428-0.551 / 0.336-0.558 / 0.422-0.548; pair medians coal 0.387 to grid 0.611 (Up), wind 0.300 to gas 0.579 (Mid), geothermal 0.388 to grid 0.569 (Down); mean within-pair IQR 0.023/0.048/0.053; clean-tech midstream medians 0.300-0.345 vs 0.456-0.579; widest IQRs EV mid 0.119, EV down 0.116.
- FE R2: pair 0.731, tech 0.320, stage 0.035, tech+stage 0.354, country 0.117, pair+country 0.848, within-pair share 0.435; ES (29 ok pairs, 6,011 cells) pair 0.573, country 0.149.
- Weight split (own constancy test, zero range among w > 0): constants 38.6 / imputed 30.7 / observed 30.7 (Up 49.3/28.9/21.7, Mid 39.4/30.2/30.4, Down 27.1/33.0/40.0); constants' level share 41.8 (48.7/46.3/30.6); exactly six constant variables (TRL, Addressable Market, Global Demand, HHI, Price Volatility, Overcapacity), no variable constant in only some pairs; upstream price volatility 17.5, TRL 13.6, addressable market 11.1; cells per stage 2,392/2,344/2,389.
- Category shares (absent = 0, var over the pair's economies): level Up Cost 23.8/TD 18.7/EP 17.6, Mid Cost 27.8/TD 27.0/Trade 21.3, Down Cost 26.4/TD 20.4/TRL 18.3; variance Up Cost 52.9/Prod 17.1/Trade 16.2/Inv 9.6/TD 4.2, Mid Cost 35.6/Trade 29.2/FD 22.9/Inv 11.1/TD 1.2, Down Cons 25.9/Inv 25.8/Cost 22.8/TD 3.4; shares sum to 1 per stage; TD+TRL+EP effective weight 53.2/41.4/34.9. BNEF composite observed for 12 economies (8 in nuclear).
- Leaders: all 33 rows of tab:eo_leaders (leader, EO to 2 dp, best member and its rank) match; CHN 23, USA 5, TCD 3, IDN 1, SAU 1; median margin 0.038 (0.051 China), 9 below 0.01; five largest gaps as stated; Chad margins 0.0021/0.0006/0.0002 over Cameroon; cost-only pairs are exactly geothermal Up/Mid and grid Down, each with the Input Cost Index as the only cost variable; best member rank 1 in 7, <= 3 in 28, median 2; 27 emerging leaders; 48.2% of 330 top-ten places emerging; China in 30 top tens.
- Coalition: mean pct 0.628/0.482/0.963 (coalition/other 195/China), mean EO 0.506/0.473/0.662, by stage 0.56/0.73/0.60; member order and values (USA 0.84 ... AUS 0.4548, ZAF 0.49, KEN 0.50, BRA 0.25, ARG 0.22); stage Spearman 0.75/0.74/0.78 over 216; EO-vs-cost Spearman 0.81 (219), 0.89 (23); iota 0.26 vs 0.46, USA 0; within-pair Spearman(EO, iota) median -0.25, range -0.73..0.16; heavily imputed 2,851 cells, 64.7% below median, pct 0.43 vs 0.55, 4 top-ten slots, 0 leaders.
- Cost theme: 144 rates; Chad 2.45 (2017), rank 6, R_i = 0 (floor); Brazil 46.0 and Argentina 59.67 (2025), ranks 142/143, R_i = 1 (ceiling); wage percentile Chad 0.12, Australia 0.93; Cost percentile BRA 0.01, ARG 0.02, AUS 0.18.
- Illustrative pairs: Oil Up top five and five members; constant segments 0.253/0.214/0.188/0.097; Oil segment spans Production 0.050, Cost 0.042, Trade 0.040, USA largest Production 0.167 and Trade 0.114, China largest Cost 0.122; Batteries CHN 0.783 vs USA 0.648 (FD 0.167 vs 0.106, Cost 0.142 vs 0.073, Inv 0.134 vs 0.143); Solar CHN 0.793, IND 0.646, USA 0.638; EV Down CHN 0.764, VNM 0.740, THA 0.714, TUR 0.711, TD 0.207-0.333, TUR/VNM/PHL 0.333/0.295/0.281 vs CHN 0.271; emerging in top ten 3/4/7/8; upstream oil has the lowest upstream mean iota (0.365).
- EV composite: 3 parameters x 4 S-curves = 12 indices, 6 of them growth terms (appendix_a_variables.tex, eq:eo_ev), as the text says.
No discrepancies found in this pass.

## Step 4 (part). Text-number and LaTeX checks
- Every numeric token of eo.tex (217 distinct, comments stripped) matches a JSON value at its displayed precision, apart from 1.1 (from 1.1e-16, JSON 1.11e-16), 0.5 (the iota > 0.5 threshold) and 1.5 (box-plot whisker rule).
- Environments balanced (figure 4, table 2, threeparttable 2, tabular 2, tablenotes 2); braces 171/171; 72 dollar signs; no unescaped _ & # ^ outside math/refs/tabulars; no non-ASCII; no \cite; every \ref/\eqref target exists in paper/sections; the four fig: labels exist only in the Section 6 placeholders that this fragment replaces; tab:eo_weight_split and tab:eo_leaders are unique; siunitx, booktabs, threeparttable, graphicx loaded in main.tex. No TeX engine on this machine, so not compiled.

## Step 3. Figures (second pass; all four PNGs opened)
- eo_distribution: three stage facets in the stage colours (blue/orange/aqua), shared 0-1 axis, technologies ordered by median within each facet, strip names the stage so no legend is needed; no clipping or overlap; 8 pt axis text. Caption matches; whisker wording made exact (see repairs).
- eo_contrib: stage colours with legend; hairline x grid; level panel and variance panel each sum to 100% per stage (verified independently); TRL* and Energy Prices* zero in the variance panel as the caption says; the Upstream Cost bar (52.9%) ends inside the panel; category order is that of tab:eo_structure as the caption states.
- eo_heatmap: single-hue sequential blue on a fixed 0-1 percentile scale with a colourbar legend; dot legend for iota > 0.5; 23 member rows in mean-percentile order plus a China reference panel; no fully imputed cell is shown (the 24 such cells belong to IMN, PRI, LIE). Labels legible.
- eo_top: bars from 0 (axis 0-0.85, honest for bars; max EO 0.793); gray constants plus six categorical slots (<= 8) with legend; bold coalition members match the data (5/4/6/4); white gaps between segments; no numbers on marks.
- No dual axes, no in-plot titles, no chart junk.
- Minor, not repaired: eo_top colours Trade/Production/Technology Demand with the blue/orange/aqua slots that elsewhere mean Upstream/Midstream/Downstream. Six categories need six hues and only five non-stage slots exist; the facets are labelled by stage in text, not colour, so nothing in the figure is misread, but a reader moving from eo_contrib could be momentarily confused. A fix needs a seventh non-stage hue in _paper_theme.R (not owned).
- Minor, not repaired: the Section 6 placeholder asked for the heatmap rows "in the order of fig:es_heatmap"; the figure orders rows by mean EO percentile and the caption says so. A shared row order with the ES heatmap would ease comparison; integration may wish to align them.

## Repairs this pass (all minor)
1. Text (Distribution paragraph): "the other midstream pairs are scored on Trade, Technology Demand, Technological Readiness and Cost Competitiveness" was loose: electric-grid midstream has no Technology Demand and geothermal midstream no Trade. Now "scored only on ... (electric grid lacks Technology Demand and geothermal lacks Trade)". "the large majority of economies ... score zero" overstated EV midstream, where the zero rule fills Foreign Dependency for 116 of 216 economies; now "most economies".
2. Script: added coverage$category_sets_by_pair to eo.json (each pair's category set, the categories filled by the zero rule and how many economies each fills) so the claim above is backed by the JSON. Re-run: exit 0, no warnings; every other JSON value and all four PNGs are byte-identical to before.
3. Table tab:eo_weight_split note: the first three columns sum to 100 "up to rounding" (Upstream 99.9, Downstream 100.1).
4. Caption fig:eo_distribution: whiskers extend "to the most extreme economy within 1.5 times the interquartile range of the box" (the ggplot rule), not "at 1.5 times the IQR".
After the edits: environments, braces and dollars still balanced, no unescaped specials; number check unchanged (only 1.1e-16, 0.5 and 1.5 unmatched, all explained).

## Final state (second pass)
fig_eo.R last run clean (exit 0) after the script edit; eo.json and the four figures are as that run wrote them. Cross-check with the ES fragment: es.tex reports pair FE 57% and country FE 15% over 6,011 cells, consistent with the EO fragment's 57.3/14.9.
Label mapping unchanged: eo_distribution -> fig:eo_distribution; eo_contrib -> fig:eo_category_contrib; eo_heatmap -> fig:eo_heatmap; eo_top -> fig:eo_top; new tab:eo_weight_split, tab:eo_leaders.
Verdict: approve with repairs (minor only; no blocker or major found in this pass).

---

# Third verifier pass (2026-10-06, evening; user asked to "try again")

State under review: fig_eo.R 14:51, eo.json 14:51, four figures 14:51, eo.tex 14:53 (left by the second pass). Earlier passes treated as unverified. Pre-run copies saved to scratchpad veo3/pre.

## Step 1 (third pass). Clean re-run
- `Rscript --vanilla scripts/paper/fig_eo.R` from the worktree root: exit 0, 41 s; the log has no warning or error lines.
- eo.json and all four PNGs are byte-identical to the pre-run copies; PDFs rewritten. Input index_outputs.rds mtime 2026-10-01 11:04 (corrected run).
- Every \ref/\eqref target used by eo.tex exists (16 labels checked in paper/sections); fig:eo_* labels are defined only in the Section 6 placeholders that the fragment replaces.

## Step 2 (third pass). Independent recomputation
Own base-R code (scratchpad veo3/i1.R ... i6.R; no dplyr, JSON not read; constancy, ranks, percentiles, FE regressions, variance shares and income groups coded afresh, names mapped to ISO3 with countrycode rather than the script's helper). Matches at the displayed precision:
- Coverage: 7,125 cells, 219 economies, 33 pairs all ok, 212-219 per pair; 24 fully imputed (IMN 14, PRI 8, LIE 2); iota 0.436 (0.436/0.443/0.430); 40.0% > 0.5; 46.2% of category scores imputed.
- Moments, pair-median ranges, mean within-pair IQR (0.023/0.048/0.053), widest IQRs (EV mid 0.119, EV down 0.116), clean-tech midstream 0.300-0.345 vs 0.456-0.579.
- FE R2: pair 0.731, tech 0.320, stage 0.035, tech+stage 0.354, country 0.117, pair+country 0.848, within 0.435; ES 29 ok pairs / 6,011 cells, 0.573 / 0.149.
- Exactly six constant variables, none constant in only some pairs; weight split 38.6/30.7/30.7 and every entry of tab:eo_weight_split; upstream volatility 17.5, TRL 13.6, BCG 11.1; cells 2,392/2,344/2,389.
- Level shares, TD+TRL+EP weight 53.2/41.4/34.9, variance shares (Cost 52.9/35.6/22.8 etc.; sum 1 per stage); Investment level share < 3% at every stage (0.3/0.6/2.6).
- All 33 rows of tab:eo_leaders (leader, EO at 2 dp with the borderline values checked to 6 dp, best member and rank); 23/5/3/1/1; 27 emerging leaders; no ties; median margin 0.038 (China 0.051); 9 below 0.01; five largest gaps; best member rank 1 in 7, <= 3 in 28, median 2; 48.2% of 330 top-ten places emerging; China in 30.
- Coalition 0.63/0.48/0.96 and 0.506/0.473/0.662; by stage 0.56/0.73/0.60; member list and values; stage Spearman 0.75/0.74/0.78 (216); EO-vs-cost Spearman 0.81 (219) / 0.89 (23); iota 0.26 vs 0.46, USA 0; within-pair rho median -0.25, range -0.73..0.16; heavily imputed 2,851, 64.7%, 0.43 vs 0.55, 4 top-ten slots, 0 leaders.
- Illustrative pairs: every segment value quoted for oil, batteries, solar and EV downstream; emerging 3/4/7/8, coalition 5/4/6/4; oil lowest upstream iota 0.3651 (gas 0.3660 next).
- Cost theme: 144 rates; Chad 2.45 (2017) rank 6, R_i = 0; BRA 46.0 / ARG 59.67 (2025) ranks 142/143, R_i = 1; wage pct Chad 0.12, AUS 0.93.

Discrepancies / new findings:
- F1 (major, text): "The widest boxes, electric vehicles midstream and downstream ..., reflect the regional EV production indices and the EV-market composite, which vary across a broad set of economies." Wrong for downstream. The EV-market composite is observed for 60 of 216 economies and filled at the global average for the other 156, so it adds nothing to the IQR (IQR with Technology Demand held at its pair mean: 0.127, i.e. wider). The downstream width comes from Investment (zero rule; 123 of 216 economies non-zero): with Investment held at its mean the IQR falls from 0.116 to 0.025, and Investment carries 73% of the pair's variance. Midstream is right in substance: Foreign Dependency (the EV production position, non-zero for 100 of 216) takes the IQR from 0.119 to 0.035 when held at its mean.
- F2 (minor, text): "The upstream boxes are the narrowest because the upstream pairs carry the largest weight on inputs that do not vary" overstates the cause: by eq:affine_constant constants scale dispersion by (1 - weight), 0.51 upstream vs 0.61 midstream, while the mean IQR ratio is 0.47; the zero-rule categories that widen midstream/downstream boxes do the rest.
- F3 (minor, text): Chad's 2.45 per cent rate ties with Cameroon's (2018), so it is the joint sixth lowest; Cameroon is the runner-up in all three of Chad's pairs, which is why the margins are 0.0002-0.0021.
- F4 (minor, text): the coalition paragraph attributed Australia's low position (mean percentile 0.45) to cost alone. Counterfactual (own code, scratchpad veo3/i8.R: replace one category's contribution by its pair mean, re-rank): Cost at the pair mean lifts Australia to 0.571, Technology Demand at the pair mean to 0.527. Cause of the second: the BNEF regional composite is observed for 12 economies and the global-average rule gives the other ~200 the mean (0.5); Australia's value is below 0.5 in all 18 pairs where it is observed, so it ranks above at most 4 economies on that component. Brazil and Argentina are cost-driven (0.25 -> 0.578 and 0.22 -> 0.519 with Cost at the pair mean; Technology Demand barely matters).
- F5 (minor, text): "under the zero rule a few economies with recorded investment stand far above the many with none" is false for solar and EV downstream (160 and 123 economies recorded); the zero-rule economies are the majority in 10 of the 12 pairs that carry Investment.

## Step 3 (third pass). Figures (all four PNGs opened)
- eo_distribution: stage colours blue/orange/aqua, one facet per stage (strip names the stage, so no legend needed), shared 0-1 axis, technologies ordered by median, whiskers without staples, no clipping. Caption matches. Outlier dots are small (size 0.35) but visible.
- eo_contrib: stage colours with legend; hairline x grid; both panels on one 0-53% axis (honest common scale); the 52.9% Upstream Cost bar ends inside the panel; * categories zero in the variance panel; category order is that of tab:eo_structure (checked against Section 6). Caption matches.
- eo_heatmap: single-hue sequential blue on a fixed 0-1 scale with colourbar; dot legend for iota > 0.5; rows in mean-percentile order as captioned; China in a reference panel; labels legible, no overlap.
- eo_top: bars from 0 on a 0-0.85 axis; gray constants of equal length in each panel; bold members match the data (5/4/6/4); legend present; white gaps between segments.
- Minor, not repaired (as in the earlier passes): eo_top uses the stage hues blue/orange/aqua for Trade/Production/Technology Demand, and the thin aqua Technology Demand segment in the oil panel sits next to the green Cost segment (adjacent cool greens). Six categories need six hues and only five non-stage slots exist; a fix needs a palette decision in _paper_theme.R (not owned).
- Minor, not repaired: the Section 6 placeholder asked for the heatmap rows "in the order of fig:es_heatmap"; the figure orders by mean EO percentile (es_heatmap orders by mean ES). Ordering by the plotted quantity reads better and the caption says so; integration may align them if a common order is wanted.

## Repairs (third pass)
1. fig_eo.R: added contributions$widest_pair_iqr_sources (per category of the two widest pairs: observed / zero-rule / global-average counts, non-zero count, IQR with the category at its pair mean, variance share), contributions$investment_coverage_by_pair with investment_n_pairs (12) and investment_n_pairs_majority_zero_rule (10), coalition$members_mean_percentile_with_category_at_pair_mean (Cost and Technology Demand counterfactuals for the 23 members and China, with a stopifnot that the base equals member_tbl), coalition$australia_bnef_regional_composite (21 pairs, 18 observed, 18 below the fill value, at most 4 economies below), and Cameroon (runner-up in the cost-only pairs) in nominal_rates_selected. All new values equal the independent ones above.
2. eo.tex Distribution: F1 rewritten (EV midstream width from Foreign Dependency, non-zero for 100 of 216, IQR 0.035 without it; EV downstream from Investment, recorded for 123, IQR 0.025 without it; the EV-market composite observed for 60 and filled at the average for 156, so it shapes the top, not the body); F2 softened to "in part because ... and the least on observed country-specific inputs".
3. eo.tex What drives the pillar: F5 ("the majority in 10 of the 12 pairs that carry the category").
4. eo.tex Leaders: F3 (joint sixth lowest; Cameroon records the same rate, is the runner-up in all three pairs, so the two differ only in wages and producer-price inflation; verified that the cost-only pairs carry the Input Cost Index as their only cost variable and identical TRL/BCG values).
5. eo.tex Coalition: F4 (Brazil/Argentina counterfactuals 0.58/0.52; Australia 0.57 with cost neutralised, 0.53 with Technology Demand neutralised, BNEF composite mechanism, and the two-way effect on France/Germany/UK (0.82/0.74/0.66) and India/Viet Nam (0.70/0.57)); the closing caveat now also covers the BNEF construction.
Re-run after the script edits: exit 0, no warnings; all four PNGs byte-identical to the pre-pass copies (figures unchanged); only new JSON keys added.
- Palette check for eo_top (dataviz validate_palette.js on the actual adjacent segment sequences of the four panels: oil blue/orange/aqua/green, batteries blue/yellow/magenta/green, solar blue/aqua/yellow/magenta/green, EV aqua/yellow/green): all pass; worst adjacent aqua-green normal-vision dE 15.6 (>= 15), worst CVD dE 9.1 (>= 8). The aqua-green adjacency is therefore acceptable; only the stage-hue reuse remains (minor, palette decision outside owned files).

## Step 4 (third pass). Text fragment after repairs
- Number check (own script veo3/nc.R): 230 distinct numeric tokens; all match eo.json at the displayed precision except 1.1 x 10^-16 (JSON 1.11e-16, displayed rounded). The new numbers (100, 216, 123, 0.035, 0.025, 60, 156; 10 of 12; Cameroon 2018; 0.58, 0.52, 0.57, 0.53, 0.82, 0.74, 0.66, 0.70, 0.57; 18 pairs, four economies) were matched semantically to their keys.
- LaTeX (veo3/tx.R): figure 4/4, table 2/2, threeparttable 2/2, tabular 2/2, tablenotes 2/2; braces 172/172; 72 dollar signs; no non-ASCII; no \cite; labels fig:/tab: prefixed; no unescaped specials outside tabulars/math/filenames; tabular rows have 5 and 10 cells as declared; \EO/\ES used only in math mode (they are \mathrm macros). No TeX engine on this machine, so not compiled.
- Claims re-read against the figures: Distribution, Variance, Drivers, Leaders, Coalition and Illustrative paragraphs follow from the figures and JSON after the repairs above; construction-driven patterns (pair effects, constants, zero/global-average rules, cost-only pairs, BNEF composite) are flagged as such.
- Label mapping unchanged: eo_distribution -> fig:eo_distribution; eo_contrib -> fig:eo_category_contrib; eo_heatmap -> fig:eo_heatmap; eo_top -> fig:eo_top; new tab:eo_weight_split, tab:eo_leaders.

## Final state (third pass)
fig_eo.R last run clean (exit 0, no warnings) after all script edits; figures byte-identical to before this pass; eo.json = previous content plus the new keys listed under Repairs. Verdict: approve with repairs (one major text error, F1, repaired; four minor repaired; two minor figure points left for integration/theme owners).
