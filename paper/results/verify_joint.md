# Verification log: theme "joint" (Section 7, opportunity-security space)

Verifier run started 2026-10-06. Files under review: scripts/paper/fig_joint.R,
paper/figures/joint_{scatter,corr,quadrants}.{pdf,png}, paper/results/joint.json,
paper/tables/joint_stage.tex, paper/results/text/joint.tex.

## Step 1. Clean re-run
- Running `Rscript --vanilla scripts/paper/fig_joint.R` from the worktree root (in progress).
- Result: exit 0 in 29 s; wrote the 3 figures (pdf + png), joint.json and tables/joint_stage.tex.
  joint.json content identical to the pre-run copy (python json diff empty): the script is
  deterministic and the committed outputs are what it produces.

## Step 2. Independent recomputation (base R, own code, from index_outputs.rds)
Independent code: scratchpad vj/rc.R, rc2.R, rc3.R (base R merge/tapply/split, own shared-component
detection, own covariance decomposition). Every value below reproduced from index_outputs.rds:
- Sample 6,009 cells / 219 economies / 29 pairs; only upstream nuclear flagged (ES constant);
  coalition 665 cells = SI non-NA rows (665, 23 countries, all inside the joint sample). MATCH.
- FE R2: ES 0.5732, EO 0.7172 (between-SS / total-SS). MATCH (57%, 72%).
- Within-pair Spearman: median 0.5321, IQR 0.2626-0.6859, min upstream solar -0.1989, max
  downstream GH2 0.9424, 17 > 0.5, 2 < 0 (upstream solar, wind); pooled levels 0.3720. MATCH.
- Stage medians: 0.1997 / 0.5334 / 0.6948. Text 0.20/0.53/0.69 MATCH; **table prints 0.70 for
  downstream (double rounding 0.6948 -> 0.695 -> 0.70). MAJOR (numeric error in a table).**
- Regions: all 15.29/32.22/15.09/37.39; stage and coalition (8.27/60.60/18.95/12.18) MATCH;
  by tech 12.70 (batteries) - 19.41 (coal) MATCH; GH2 midstream 98 of 109 tied, 89.9% <= median MATCH.
- Coalition profiles (Kenya 9/28, Mexico 5, ITA/FRA/CHL 4, zero for CAN/AUS/BRA/ARG, US 26 leaders,
  BRA 17 / ARG 15 / AUS 14 sec-w/o-opp, 9 members with GH2-mid, 7.15% excl. GH2-mid). MATCH.
- Gaussian copula implied 16.42% vs observed 15.24% off-diagonal. MATCH.
- SI by region 0.5542/0.5324/0.5246/0.5148; top-1 = 2 opp-w/o-sec (Kenya upstream wind, US
  midstream GH2), 16 leaders; top-5 share 12.17%; SI vs level gap 0.597, vs rank gap 0.049. MATCH.
- Within-country: Pearson mean 0.564 (Kenya 0.213 - India 0.760), Spearman 0.566 (Kenya 0.2655 -
  Chile 0.7887), SD EO 0.090 / ES 0.157, share removed 0.484 (Kenya 0.194 - Australia 0.681), SD
  Delta 0.130 (Australia 0.091 - Japan 0.164), pair-means Pearson 0.314, demeaned 0.584 (US 0.0652 -
  Saudi Arabia 0.818), demeaned SD 0.0945. MATCH.
- Shared components (own detection): identical set; weights sum to 1 per cell; ES shared weight
  0.581 (21.3/60.3/91.2% by stage), EO 0.298; 7 downstream all-shared pairs, same list. MATCH.
- Non-shared: 22 pairs, median 0.1478 (IQR 0.0434-0.2982) vs full 0.4849, reduction 69.5%. Stage:
  0.0563 / **0.3349** / -0.0377; midstream without EO trade 0.1121. Text, strip label and table print
  **0.34** for midstream (0.3349 -> r3 0.335 -> "0.34"). **MAJOR (wrong number in text, figure, table).**
- Covariance decomposition: pooled 63.6/26.0/10.4%; stage AA 11.5/43.9/88.8%, downstream BB -0.14%;
  median pair AA 27.1%. MATCH.
- Category correlations: Consumption/EA/Investment 1.00, Trade 0.691 (14 pairs), FD 0.225, Production
  0.104; ES Production-Reserves 0.9997 in the 6 upstream mineral pairs; Tech Demand max |rho| 0.086,
  Cost Comp 0.124. MATCH.
- Text composition claims checked: 7 downstream ES sets are only Consumption/Energy Access/Investment
  (deployment/demand signals) - TRUE; downstream GH2 ES = Consumption only - TRUE; FD imputation rules
  differ (ES global_average, EO zero) - TRUE; Trade 2 of 3 / 2 of 5 shared, FD 5 of 6 - TRUE.

## Step 4 (part). LaTeX static check (no TeX engine on this machine)
- Fragment and table: environments balanced, braces balanced, $ balanced, every \ES/\EO/\operatorname
  in math mode, no unescaped specials outside labels/tabular, ASCII only. All 14 refs resolve in
  paper/sections (sec:network, subsec:eo_shared, tab:shared_signals, subsec:es_production,
  subsec:es_reserves, subsec:norm_pillar, eq:si, eq:joint_cov, eq:joint_var, sec:strategic + own);
  both cite keys (ravallion2012mashup, oecd2008handbook) exist in references.bib.

## Resumed verifier session (2026-10-06, second pass)
- Found fig_joint.R edited at 09:28 (stage_stats kept unrounded; table cells rounded once from
  unrounded inputs) but joint.json / joint_stage.tex / figures still dated 09:20, i.e. the
  double-rounding repair had not been re-run. Re-running now.
- Re-run (exit 0, 32 s). Table now prints downstream median 0.69 and midstream non-shared 0.33
  (both were double-rounding errors); scatter strip label now reads 0.33. Text still said 0.34
  (line 94) -> repaired below.

## Step 2 (second pass, own base-R code in scratchpad vj2/rc*.R; not reading the JSON)
All reproduced: 6,009/219/29, coalition 665 = SI rows; FE R2 0.573/0.717; within-pair median
0.532, IQR 0.263-0.686, min upstream solar -0.199, max downstream GH2 0.942, 17 > 0.5, 2 < 0, pooled
levels 0.372; stage medians 0.200/0.533/0.695; pooled within-pair ranks 0.184/0.549/0.694; regions
15.29/32.22/15.09/37.39 (coalition 8.27/60.60/18.95/12.18); by-stage and by-tech shares; GH2 mid 98 of
109 tied; Kenya 9/28 ... Argentina 0; copula 16.4 vs 15.2; SI by region; top-1 2/16/4/1; top-5 12.2%;
SI-gap rho 0.597/0.049; within-country 0.564 (Kenya 0.213-India 0.760) etc.; shared weights
0.581/0.298 (0.213/0.603/0.912); non-shared 22 pairs, 0.148 vs 0.485, midstream 0.3349, w/o trade
0.112; cov decomposition 63.6/26.0/10.4, stage AA 11.5/43.9/88.8, median pair 27.1; category corr
Consumption/EA/Investment 1.00, Trade 0.691, FD 0.225, Production 0.104, 47 cross pairs median 0.077,
TD 0.086 / CC 0.124, ES Production-Reserves 0.9997; dash cells = never co-occur (verified).

## New findings (second pass)
- MAJOR (overclaim, construction-driven pattern not flagged): upstream coal's 31% opportunity-
  without-security share is mostly a tie artefact. 38 of 218 economies (17%) share one ES value
  exactly at the median (small economies whose ES Trade/Energy-Imports components are imputed at
  the global average and Reserves at zero), 35 of them with EO above the median. Counting ties as
  high halves the share to 15.1%. The text explained it economically ("security rests on reserves
  and the fuel balance, which many economies lack"). Upstream gas (20%) has no ties -> the
  explanation holds there.
- MAJOR (caption does not match drawing): joint_scatter caption says the solid lines are the
  within-pair medians dividing the regions, but the lines are at rank 0.5 and 144 cells (2.4%; upstream
  coal 38, grid 45, EVs 20, wind 22, downstream nuclear 19) tied at a median are plotted at ranks
  0.51-0.58 while counted low.
- MINOR: region shares are sensitive to the tie convention (ties high: pooled 16.7/36.7/16.5/30.1,
  coalition opp-w/o-sec 5.9%; upstream wind 18% -> 37%, grid 13% -> 30% because 31%/21% of economies
  tie at the EO median); caveat paragraph only mentioned near-median differences.
- MINOR: "98 of 109" not in JSON (only the 0.899 share) -> added.
Repairs: script adds quadrants$tie_convention_sensitivity and scatter_median_line to the JSON; text
and captions edited accordingly.

## Step 3. Figures (PNG inspection, second pass)
- joint_scatter: MAJOR overlap found and repaired. The corner region labels (white 85% boxes inside
  the unit square) covered data in the densest corners, and the labelled point FRA Gas (upstream,
  ES rank 0.19, EO rank 0.94) was hidden under the "Opportunity, no security" box. Repair: region
  shares moved to bands above/below the unit square (outside the data; frame and median lines
  drawn only over [0,1]^2; repel labels confined to the square; upstream panel names the regions
  "Opportunity only / Leaders / Laggards / Security only"); height 3.5 -> 3.7in. Caption updated.
  Strip label now reads midstream non-shared 0.33. Minor (not fixed): strip prints "-0.04" with a
  hyphen-minus (the pdf device's Latin-1 encoding has no U+2212).
- joint_corr: minor repaired: the empty first row (Consumption ES) and last column (Cost
  Competitiveness EO) of the lower triangle dropped (positions of other tiles unchanged).
  Diverging blue-gray-red centred at 0 on [-1, 1]; selective labels; legend in the empty triangle.
  Dash semantics verified ("never co-occur": no NA tile corresponds to categories that co-occur).
- joint_quadrants: no defects. 0-100% axes, pillar colours for the off-diagonal regions, neutral grays
  for leaders/laggards, legend present, counts legible in the orange segments, reference line labelled.
- No dual axes; at most 4 categorical colours; violet highlight vs gray in the scatter.

## Step 4. Text (second pass)
- Repaired: 0.34 -> 0.33 (midstream non-shared); upstream-coal explanation rewritten as a tie /
  imputation artefact (38 of 218 tied at the ES median, 37 all-imputed, 35 EO-high; 15% with ties
  high), economic explanation kept for upstream gas only (no ties; 20% vs copula-implied 21%);
  tie-convention sensitivity added to the regions paragraph (16.7/36.7/16.5/30.1) and to the first
  caveat (upstream 17.4 -> 26.8; coalition 8.3 -> 5.9; ordering of stages unchanged); scatter caption
  now states the 0.5 lines and the 144 tied cells plotted across them; quadrants caption mentions the
  upstream-coal ties.
- Every number in joint.tex and joint_stage.tex found in joint.json (script check numchk.py);
  "98 of 109" now in quadrants$tie_convention_sensitivity$by_pair.
- LaTeX static check passes (envs/braces/$ balanced, ASCII, labels fig:/tab:, 14 refs resolve,
  cites ravallion2012mashup and oecd2008handbook present in references.bib).

## Final
- Final clean re-run exit 0; a second run reproduces joint.json and joint_stage.tex byte for byte.
- Label mapping unchanged: fig:joint_scatter and fig:joint_quadrants replace the Section 7
  placeholders; fig:joint_corr and tab:joint_stage are new.
- Verdict: approve with repairs.

## Third verifier pass (2026-10-06, "try again" run)
### Step 1. Clean re-run
- `Rscript --vanilla scripts/paper/fig_joint.R` from the worktree root: exit 0 in 33 s; wrote the
  3 figures (pdf + png), joint.json and tables/joint_stage.tex. joint.json and joint_stage.tex
  are byte-identical to the copies present before the run (deterministic; on-disk outputs are
  what the script produces).

### Step 2. Independent recomputation (third pass; fresh base-R code in scratchpad vj3/ind2-5.R,
own merge/rank/median/copula/covariance code, no dplyr, JSON not read)
All reproduced from index_outputs.rds and allied_network_specialization.csv:
- Sample: 6,009 cells, 219 economies, 29 pairs; dropped pairs exactly downstream grid, upstream and
  midstream geothermal (ES missing) and upstream nuclear (ES index_cells "constant"); coalition
  665 cells / 23 members; all 665 SI rows inside the sample.
- FE R2 (between-pair SS share): ES 0.5732, EO 0.7172 -> 57%, 72%.
- Within-pair Spearman: median 0.5321, IQR 0.2626-0.6859, min upstream solar -0.1989, max downstream
  GH2 0.9424, 17 > 0.5, 2 < 0 (upstream solar, upstream wind -0.1162); pooled-levels Spearman 0.3720;
  stage medians 0.1997 / 0.5334 / 0.6948; pooled within-pair ranks 0.1836 / 0.5492 / 0.6940;
  technology profiles (solar -0.20/0.66/0.69, wind -0.12/0.70/0.68, GH2 0.09/0.01/0.94).
- Regions (strictly above median = high): 15.29 / 32.22 / 15.09 / 37.39; by stage opp-w/o-sec
  17.43 / 17.02 / 11.57, sec-w/o-opp 20.44 / 14.86 / 10.20; by tech 12.70 (batteries) - 19.41 (coal);
  pairs GH2-mid 44.0, coal-up 31.2, gas-up 20.2. Ties high: 16.67 / 36.73 / 16.51 / 30.09; upstream
  26.76; coalition 5.86. GH2-mid 98 of 109 tied at the ES median, 89.9% at or below; coal-up 38 of
  218 tied, 35 of them EO-high; gas-up 0 ties, rho 0.249, copula-implied 20.8%. Upstream EO ties at
  median 31.2-31.5% (solar, wind, GH2, EVs). Copula average off-diagonal 16.42% vs observed 15.24%.
- Coalition: 60.60 / 18.95 / 12.18 / 8.27; excluding GH2-mid 7.15%; member counts (Kenya 9 of 28 with
  the nine listed pairs; Mexico 5 with the five listed pairs; Italy, France, Chile 4; Canada,
  Australia, Brazil, Argentina 0; Brazil 17, Argentina 15, Australia 14 sec-w/o-opp; US 26 leaders;
  median member 2 = 6.90%); GH2-mid counted for 9 members.
- SI: mean by region 0.5542 / 0.5324 / 0.5246 / 0.5148; top-1 sector regions 2 / 16 / 4 / 1 (Kenya
  upstream wind, US midstream GH2); top-5 share 12.17%; median member Spearman(SI, level gap) 0.5966,
  (SI, rank gap) 0.0488.
- Within-country: Pearson mean 0.5644 (Kenya 0.2131 - India 0.7600, all positive), Spearman 0.5657
  (Kenya 0.2655 - Chile 0.7887), SD ES 0.157 / EO 0.090, covariance share removed 0.484 (Kenya 0.194 -
  Australia 0.681), SD gap 0.130 (Australia 0.091 - Japan 0.164); pair-means Pearson 0.3142;
  demeaned 0.5835 (US 0.0652 - Saudi Arabia 0.818), demeaned SD gap 0.0945.
- Scatter misplacement: 144 cells (2.40%) plotted across a 0.5 line, ranks 0.514-0.578, in upstream
  coal 38, grid 45, EVs 20, wind 22 and downstream nuclear 19.
- Categories (own wide table, >= 20 economies, varying): Consumption / Energy Access / Investment
  1.00, Trade 0.6912 (14 pairs), FD 0.2253 (5), Production 0.1044 (6); 47 cross-pillar
  differently named pairs, median 0.0767; TD max |rho| 0.0856, CC 0.1241; ES Production-Reserves
  0.9994-0.9999 in the six upstream mineral pairs.
- Shared components (own detection, same rule): 16 + 16 components, identical set; mean effective
  shared weight ES 0.5811 (0.2130 / 0.6034 / 0.9118), EO 0.2978; 7 all-shared downstream pairs (the
  seven named in the text). Non-shared: 22 pairs, median 0.1478 (IQR 0.0434-0.2982) vs 0.4849 full,
  reduction 69.5%; stage 0.0563 / 0.3349 / -0.0377 (9/10/3 pairs); midstream without EO trade 0.1121.
- Covariance decomposition (n-1 weighted pooled): AA 63.6%, cross 26.0%, BB 10.4%; AA by stage
  11.5 / 43.9 / 88.8%, downstream BB -0.14%; median pair AA 27.1%.
Verdict step 2: every headline number checked matches (no discrepancies).

### Step 3. Figures (third pass; PNGs opened, panels cropped and zoomed 2x; PDF fonts inspected)
- joint_scatter: MAJOR (number printed wrong in the figure), REPAIRED. The upstream laggards'
  corner label read "36%", but the share is 36.54% (it should round to 37%). Cause: double rounding.
  quad_stage$share is r3-rounded (0.365) and scales::percent() then rounds 36.5 half-to-even to 36.
  The fix in fig_joint.R: corner labels are now computed from the unrounded counts (n / sum(n) per
  stage), so each is rounded only once. After the re-run the label reads "Laggards 37%", and the
  other 11 corner labels are unchanged and correct (17/26/20, 17/33/35/15, 12/38/40/10). JSON is
  unchanged.
- joint_scatter, otherwise: strip labels 0.20/0.06, 0.53/0.33, 0.69/-0.04 (3 pairs) are correct; the
  PDF uses WinAnsi with /Differences 45/minus, so "-0.04" prints with a true minus glyph (the earlier
  minor about hyphen-minus is moot). There is no clipping. Minor, not fixed: the "DEU Green H2"
  label halo covers a few gray points of the tied GH2 column, and the in-plot annotations are 7.1 pt
  against 8 pt axis text.
- joint_corr: diverging blue-gray-red centred at 0 on [-1, 1]; the cross-pillar block is outlined;
  selective labels follow the caption's rule (|rho| >= 0.5 on unrounded values, plus same-name
  categories in bold); the en dash for "never co-occur" is encoded correctly in the PDF (CP1252 0x96);
  the legend sits in the empty triangle; no overlaps.
- joint_quadrants: 0-100% axes; orange and blue are paper_pillar_cols EO/ES (consistent with the
  caption); leaders/laggards use neutral grays; ordering matches the caption; the white counts fit
  inside the orange segments (narrowest: the US, UK and India at 1 of 29); the reference line sits at
  15% and is labelled. No dual axes; at most four categorical colours; no stage colours are used
  (stage appears only as facet strips), so there is no clash.

### Step 4. Text (third pass)
- Every number in joint.tex and joint_stage.tex matches a JSON value at the printed precision
  (vj3/numchk3.py: 0 unmatched in either file). Targeted checks of the numbers behind the claims
  also pass: within_country$sd_pillar_part_0.3_delta_mean 0.039 ("about 0.04"); upstream EO ties
  0.312/0.315; by_technology strings; tie_convention_sensitivity.
- Composition claims re-verified from the variable contributions (vj3/ind6.R): ES of midstream GH2 is
  the single FD component "Mineral Supply"; the downstream ES of geothermal, solar and wind adds
  Reserves (resource potential); ES Trade = HHI, deficit_gdp (net imports / GDP), market_share; EO
  Trade = HHI, market_share, rca, export_size, feas; FD is 6 components per pillar with 5 shared; the
  seven all-shared downstream ES sets contain only Consumption, Energy Access and Investment.
- MAJOR (overclaim), REPAIRED: "The gradient holds within technologies." This is false for
  batteries (0.62 upstream, 0.66 midstream, 0.44 downstream), and wind and green hydrogen are not
  monotone either. The sentence is rewritten to say "within most technologies", and the battery
  exception is stated with its numbers (from by_technology).
- MINOR, REPAIRED: Foreign Dependency's shared components were described as "observed for few
  economies", but about 47% of cells are observed. Now reads "missing for many economies".
- MINOR, REPAIRED: "This is an economic rather than a definitional link" (the residual midstream
  association through trade) was softened. It now says the link is not definitional, since the
  components are different variables, but reflects the weight both pillars give to export
  performance.
- LaTeX static check (vj3/texchk.py) passes. Environments, braces and $ are balanced; the files are
  ASCII only; there are no unescaped &/_/# outside math and label arguments; every macro is
  standard or defined in main.tex (\ES, \EO; amsmath for \operatorname; booktabs and threeparttable
  are loaded); labels are fig:joint_scatter, fig:joint_corr, fig:joint_quadrants and tab:joint_stage.
  All 10 external refs resolve in paper/sections (sec:network -> 10, subsec:eo_shared and
  tab:shared_signals -> 06, subsec:es_production and subsec:es_reserves -> 05, subsec:norm_pillar ->
  04, eq:si and sec:strategic -> 08, eq:joint_cov and eq:joint_var -> 07). ravallion2012mashup and
  oecd2008handbook are in references.bib. No duplicate labels: only the two Section 7 placeholders
  share labels with the fragment, and they are to be replaced.
- tbd coverage: all five \tbd paragraphs of Section 7 Results (sample, within-pair co-movement,
  mechanical overlap, regions, within-country view) are answered by the fragment and the table.

### Final (third pass)
- After the repairs, a clean re-run exits 0. joint.json and joint_stage.tex are byte-identical to the
  pre-pass copies (the script repair touches only the figure labels).
- Label mapping is unchanged. fig:joint_scatter and fig:joint_quadrants replace the Section 7
  placeholders; fig:joint_corr and tab:joint_stage are new.
- Verdict: approve with repairs.
