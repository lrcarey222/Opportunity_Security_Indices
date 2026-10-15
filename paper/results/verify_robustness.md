# Verification log: theme "robustness"

Verifier session started 2026-10-06. Worktree C:/Users/LCarey/osi-worktrees/paper (branch fix/allied-base-paper).

## Step 0: inventory
- Script: scripts/paper/fig_robustness.R
- Figures: robustness_{variants,vintage,ranks,loo,si}.pdf (+png)
- JSON: paper/results/robustness.json
- Text: paper/results/text/robustness.tex

## Step 1: clean re-run (session 2, 2026-10-06 late)
- `Rscript --vanilla scripts/paper/fig_robustness.R` from the worktree root, started in background (log in scratchpad vr/run1.log). Previous JSON backed up to scratchpad vr/robustness_prev.json for a diff.

## Step 2: independent recomputation (own base-R code from corrected outputs, scratchpad vr/indep.R)
All of the following match the text/JSON:
- Sample: ES 30 headline pairs, 29 ok (Nuclear Upstream 'constant'), 6,011 cells; EO 33/33, 7,125 cells.
- Available-case re-implementation reproduces stored ES/EO to 2.2e-16.
- Equal weights: within-pair median ES 0.998 (min 0.987, pooled 0.971), EO 1.000 (min 0.995, pooled 0.993).
- LOO medians: ES Consumption 0.646 (8 defined of 9 observed, min 0.395), Trade 0.833 (10 of 14), EI 0.872, MT 0.885, EA 0.898, FD 0.912, Production 0.9999; EO Trade 0.724 (14), CC 0.735 (30 of 33), Consumption 0.824, TD 0.996.
- Pair structure: shared category set in 29/29 ES and 33/33 EO pairs; median 3 varying categories; weight-invariant 7 ES / 3 EO.
- Variance shares and effective weights: every entry of tab:robustness_importance reproduced to the printed decimal; TV nominal-vs-variance 0.314 / 0.378, effective-vs-variance 0.141 / 0.403.
- Dirichlet with my own seed (4242, 300 draws): median over draws of median-over-pairs ES 0.9974 (0.993-0.999), EO 0.9918 (0.962-0.998); 1st pct over pair-draws 0.929 / 0.893. Consistent with the reported 0.997 (0.992-0.999) / 0.991 (0.964-0.999), 0.93 / 0.89.
- South Korea ES Batteries downstream 90% rank band 15-210 of 210 (own draws).
- Global-average cell weight share 16.4% ES / 36.4% EO.
- Vintage: ES 5,987 cells, within-pair median 0.993 (IQR 0.977-0.999), min 0.704 Oil Downstream, mean |change| 0.0131, 27.9% unchanged, pooled 0.983; EO 7,123 cells, median 0.820 (0.716-0.940), min 0.539 Electric Grid Downstream, mean |change| 0.0255, 0% unchanged, pooled 0.930. Nominal varying weight 52.4% / 70.0%.
- SI: 665 cells / 23 members; SI formula reproduces stored index exactly; baseline top sector matches allied_strategic.csv for 23/23. ES-for-need: pooled 0.247, within-member median 0.259, 4/23 picks kept. No priors 0.833 / 0.831 / 17. No policy 0.839 / 0.819 / 3. Equal weights 0.928 / 0.912 / 16. NIPO observed in 56.4% of SI cells. Saudi margin 0.0030.

## Step 1 result
- Clean re-run exit 0; figures and JSON regenerated. JSON identical to the pre-run version except `meta.built` (fixed seeds 20261005 / 20261006). Baseline rebuild from theme tables reproduces stored ES/EO exactly (max dev 0, no NA mismatch); SI and Policy composite re-implementations reproduce stored values (<1e-10).

## Step 3: figures (PNG inspection, 6.5in)
- robustness_variants: dot-interval, 0-1 axis, pillar facets, plotmath gamma labels legible; no overlap. OK.
- robustness_vintage: 0-1 on both axes with coord_equal, 45-degree line, stage colours = paper_stage_cols, legend present. OK.
- robustness_ranks: Batteries midstream, 23 ISO3 rows, ES/EO facets, 0-100 axis; ordering by baseline EO (stated in caption). OK.
- robustness_loo: labels "(nominal share, pairs observed)" explained in caption; rows ordered by median; 0-1 axis. OK.
- robustness_si: (a) lollipop 0-100%, (b) dot matrix with counts 4/17/13/7/3 of 23 matching JSON; legend present; rows aligned across panels. OK.
- No dual axes, no titles inside plots (panel tags only), <= 3 categorical colours, no chart junk. Captions match what is drawn.

## Step 4: text checks
- Every number in the fragment traced to robustness.json (see jsoncheck output in scratchpad vr/); table rows of tab:robustness_summary and tab:robustness_importance all match to the printed decimal.
- Citations: all 6 keys present in paper/references.bib. Referenced labels all exist (04_normalization, 08_strategic, 10_network, results/text/network.tex) except sec:robustness, which the integrating 11_robustness.tex must define. Environments and braces balanced; underscores only in math/labels.
- Overlap with strategic.tex (SI variants): numbers identical (0.25/0.26/4; 0.83/17; 0.91/13; 0.90/7; 3).

## Repairs made
1. (major) Sample statement: the text implied 33 - 1 = 29 ES pairs; ES is in fact undefined for three headline pairs (Geothermal upstream/midstream, Electric Grid downstream; 30 headline ES pairs). Script now records `meta.pairs.<p>.headline_pairs_without_index`; text, table notes and figure caption now say "of the 30 headline pairs with an ES score".
2. (minor) "Where it [Cost Competitiveness] is alone, the vintage correlation is lowest (0.54 ...)" overstated: the other two CC-only pairs (Geothermal upstream/midstream) have 0.73/0.77, not the lowest. Reworded with the three values (vintage.eo.within_pair_spearman_by_pair).
3. (minor) South Korea: "rank ranges from 15th to 210th across the draws" described the 5th-95th percentile band as the range; reworded to the 90 per cent band, with the baseline rank 74th (weights.es.coalition_rank_bands.widest).
4. (minor) Vintage-build list of fixed themes omitted the mineral production sheets and critical-mineral trade extract (index_vintage_theme_provenance.csv); added.
5. (minor) "Zero-filling places every uncovered economy below every covered one" -> "at or below" (anchored zeros of covered economies).
6. (completeness) Script now writes `tbd_fills` (5 entries) computed from the results, so every placeholder fill is reproducible; header comment of the fragment lists them and the sec:robustness requirement.

## Step 5: final re-run after repairs
- `Rscript --vanilla scripts/paper/fig_robustness.R` exit 0 (scratchpad vr/run3.log). JSON diff against the original: only the new keys `meta.pairs.<p>.headline_pairs_without_index[_rows]` (ES: Geothermal Upstream, Geothermal Midstream, Electric Grid Downstream; 0 index rows) and `tbd_fills`; every other value is unchanged. Figures regenerated from unchanged code paths.

## tbd fills (robustness.json -> tbd_fills.*), checked against the JSON
- 01_introduction.tex:318 `\tbd{median Spearman correlation of ES and EO ranks ...}` -> "closely (median within-pair Spearman correlations between 0.946 and 0.997 for $\ES$ and between 0.955 and 0.995 for $\EO$, the lowest at $\gamma=2$)" [summary.es_eo_within_pair_median_spearman_range_over_gamma_and_weight_variants].
- 01_introduction.tex:323 `\tbd{rank correlation between SI with security need ...}` -> "0.25 (Spearman, pooled over the 665 cells of the 23 coalition members; the median within-member correlation is 0.26), and the allied selection survives the change of sign for only 4 of the 23 members" [si.variants.security_orientation].
- 08_strategic.tex:843 (footnote) `\tbd{rank correlation and top-sector agreement ... all-ranked variant}` -> "a Spearman correlation of 0.89 between the baseline and the all-ranked variant (pooled; median within member 0.91) and the same allied top sector for 13 of the 23 members" [si.variants.all_ranked].
- 08_strategic.tex:1034 `\tbd{top-sector agreement under the opposite security orientation, ...}` -> "that the allied top sector is unchanged for 4 of the 23 members under the opposite security orientation, for 17 without the sector priors and for 13 with all terms ranked within country".
- appendix_d_corrections.tex:841 `\tbd{sensitivity of the Policy index, the SI and the allied selection to dropping the NIPO component ...}` -> "the effect of dropping the NIPO component, which is observed in 56 per cent of the Strategic Index cells (the component was not rescored on the July 2026 extract): ... median of 0.73 (range 0.49 to 0.86), ... Strategic Index ... median of 0.90, and the allied top sector is unchanged for only 7 of the 23 members" [si.policy_without_nipo, si.variants.no_nipo].

## Label mapping
- No placeholder floats exist for Section 11 (sections/11_robustness.tex does not exist yet); the fragment defines fig:robustness_{variants,vintage,ranks,loo,si}, tab:robustness_summary, tab:robustness_importance, eq:variance_share; none duplicates another label. The integrating file must define \label{sec:robustness}.

## Verdict
approve_with_repairs.
