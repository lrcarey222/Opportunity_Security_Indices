# Verification log: theme "reconcile"

Verifier (second pass) started 2026-10-06 22:10. Worktree C:/Users/LCarey/osi-worktrees/paper
(branch fix/allied-base-paper). The first verifier pass (14:53) stopped after step 1; the script was
changed since (22:08), so every step is redone here.

## Step 1: clean re-run
- `Rscript --vanilla scripts/paper/fig_reconcile.R` from the worktree root: exit 0 in 27 s
  (22:11-22:12). Message: ES matched 5305 (Spearman 0.86), EO 6598 (0.72), SI 519 (0.889),
  PSI 217 (0.047).
- Regenerated 3 PDFs + 3 PNGs; reconcile.json and the 4 tables are byte-identical (md5) to the
  pre-run versions (deterministic and current).
- Shipped index_outputs.rds mtime verified independently: 2026-09-03 13:58:35 (the script also
  stops otherwise). Corrected index_outputs.rds: 2026-10-01 11:04:50.

## Step 2: independent recomputation
Own code (scratchpad vr1.R, vr2.R): ISO3 matching with countrycode (custom codes for Kosovo,
Channel Islands, Micronesia), canonical spelling = the corrected release's name for the ISO3,
own constant-cell test, own PSI reconstruction from the March 2026 top-5 tables. No helper from
fig_reconcile.R or R/ used. All match the JSON:
- ES: 5,746 rows / 270 strings -> 5,567 keys / 237 economies; corrected 6,228 / 219; 5,522 in
  both, 45 removed, 706 added; constant cells Geothermal Up/Mid (shipped) and Nuclear Upstream;
  5,305 matched cells of 219 economies; Spearman 0.8599 / Pearson 0.8612 (JSON identical);
  stages 0.8993 / 0.5614 / 0.8091; share |change| > 0.1 = 0.2777 (> 0.2: 0.0388); within-cell
  median 0.7767 (JSON 0.7766, the 1e-4 gap is the choice of record among merged spellings),
  IQR 0.716-0.857, 17 of 28 below 0.8; rank shift > 10 places 0.603, > 10% of cell 0.4245,
  median 16.5. Coalition 633 cells, Spearman 0.838, 24.6% > 0.1. Top-15 movers identical
  (14 Nuclear Midstream + NOR Batteries Downstream).
- EO: 6,598 matched, Spearman 0.7202 / Pearson 0.7349; stages 0.4564 / 0.8089 / 0.8995;
  within-cell median 0.7814, IQR 0.666-0.820, 21 of 33 below 0.8, min Green Hydrogen
  Midstream -0.402; > 0.1: 16.85%; rank shift > 10 places 0.624, median 15.5; 35 removed,
  527 added. Coalition 754, 0.744, 18.4%. Top-15 movers identical.
- SI: 571/18/32 -> 665/23/29; 519 matched; Spearman 0.8891, Pearson 0.9101, 0.58% > 0.1, mean
  abs 0.0238; within-country median 0.869 (USA 0.728 to NOR 0.968); sector R2 0.630 / 0.708;
  within-sector Spearman 0.844; added KOR ARE FRA ITA DNK.
- Allied: 18 scored in both, 11 changed (8 from Nuclear Upstream), 7 unchanged; corrected
  counts Batteries Up 8, Grid Mid 8, EV Mid 4, Wind Up 2, Batteries Mid 1. All as in text.
- Rerun facts: imputed category-score share 0.4112 (ES) and 0.4611 (EO), identical under the
  imputed flag and under missing_rule_applied; 23 policy-eligible, set-equal to the coalition;
  corrected ES Nuclear Upstream = 0.7043 for all 217 economies (shipped: 0 for 218 raw rows).
- Network: shipped greedy 33/33, 14 stages with square producer count and HHI 1, k 16-440,
  max 193,600, 10 iterations, not converged, 12 remaining; corrected MILP 33/33, producers
  3 (28) / 4 (4) / 5 (1), HHI 0.329-0.360, 7 iterations, converged, 0 remaining, 0 imputed.
- PSI: own reconstruction gives 218 units; USA 185 -> 14 (of 219), JPN 156 -> 49, CHN 22 -> 1;
  means 0.598 / 0.502 (= audit notes/bilateral_psi.md); 217 matched, Spearman 0.0473,
  Pearson -0.0806. The 86 partners without a development score are almost all high-income
  economies, which supports the text's "mostly high-income".

Finding (major, text + table): the earlier portfolio loop is one loop over all stages and
`portfolio_violations_remaining` counts the members above their portfolio cap
(R/indices/allied_network_design.R, both checkouts). The text said the loop "stopped ... in every
stage with twelve violations outstanding" and the network table labelled the row "violations
remaining per stage". Repaired (see step 5).

Further checks of text claims (scratchpad vr4.R, vr5.R):
- EO Nuclear Upstream: composition effect is 0.176079 for all 218 economies of the cell, as the
  text's "every economy ... 0.25 x 0.704 = 0.176" says.
- The four European EO Green Hydrogen Midstream values of 0.278: Cost Competitiveness 0 (observed)
  and the global average in Technological Readiness and Technology Demand. The text called the
  stripes "typically an imputed group mean"; tied earlier values (groups >= 4) have an imputed
  category in 89% (ES) / 100% (EO) of rows, so "imputed" is right but "group mean" is not for
  the example. Reworded (minor).
- "The cells at the bottom of Figure ... are those in which the ranking rests mainly on imputed
  values": true for the lowest EO cells (max imputed weight share 0.79-0.98) but not for ES
  Nuclear Midstream (0.37) or Gas Midstream (0.01). Overclaim softened to "several of the
  cells ..., in EO in particular" (minor/major).

## Step 3: figures (PNG + 300/600 dpi crops of the PDFs)
- reconcile_scatter: three square panels, 45-degree line, stage colours = paper_stage_cols,
  legend present, no title, no dual axes, legible at 6.5in. Axes are zoomed per panel (EO 0.1-0.8,
  SI 0.2-0.8) although the caption said "all three are on the unit interval"; caption now states
  that each panel spans its data range, the same on both axes (minor).
- reconcile_cells: labels correct (ES: Solar Mid, Oil Up, Nuclear Mid + Wind Mid, Solar Up;
  EO: GH Mid, GH Up, EV Up + Nuclear Up, Geothermal Mid, Wind Up); the Wind Midstream leader
  points at the correct point (checked at 600 dpi). Shared y scale -0.5..1 is honest. OK.
- reconcile_movers: ordering by |change|, ISO3 labels, hollow = earlier, filled = corrected
  coloured by stage, causes on one line, no clipping. x axis 0.2-0.8: caption now says so (minor).

## Step 4: text fragment and tables
- Every number in reconcile.tex checked against reconcile.json (and most recomputed in step 2);
  all match at the quoted rounding (e.g. 0.675 -> 0.68, 0.835 -> 0.84, 0.955 -> 0.96,
  0.9447 -> 94%, 0.9450 -> 94.5%).
- Structural LaTeX check (scratchpad vtex.R; no TeX engine on this machine): environments and
  braces balanced, no odd $ per paragraph, no bare notation macros outside math, no unescaped
  & / % / _ in prose, no non-ASCII, labels fig:/tab: prefixed and not clashing with sections,
  all refs resolve (tab:corrections_index, eq:corr_fanout, subsec:corr_nipo, sec:strategic),
  cite keys saisana2005uncertainty and saltelli2007composite exist in references.bib, all
  \input/\includegraphics targets exist. Packages used (threeparttable, tabularx, booktabs,
  amssymb for \checkmark) are loaded in main.tex.
- No label mapping needed: the fragment's floats are new (fig:reconcile_*, tab:reconcile_*);
  none corresponds to the fig:es_* / fig:eo_* / fig:joint_* placeholders.

## Step 5: repairs
- MAJOR, fixed: portfolio loop description. The loop is global and
  portfolio_violations_remaining counts members above their portfolio cap
  (R/indices/allied_network_design.R, allied_network_count_topk()). Text now reads "The portfolio
  loop, which runs over all stages jointly, stopped at its limit of ten passes with twelve members
  still a top producer in more stages than their portfolio cap allows" and "... converges after
  seven passes with no member above its cap". Network table rows (script) now read "Portfolio loop
  converged: no / yes", "Portfolio passes: 10 (limit) / 7", "Members above their portfolio cap at
  exit: 12 / 0", with a note sentence explaining it.
- minor, fixed: stripe sentence, bottom-cells overclaim, "uninformative" -> "carry little or no
  information ... (0.31) and ($-0.40$)", scatter and movers captions (axis ranges).
- Script re-run after the edits (Rscript --vanilla, exit 0): reconcile.json and the three other
  tables byte-identical, reconcile_network.tex regenerated; figures unchanged in content.

## tbd fills (checked against reconcile.json)
- A3 imputed shares: "41.1% (ES) and 46.1% (EO)" = rerun_facts es/eo_category_scores_imputed_share
  0.411197 / 0.461094 (identical under the missing_rule_applied definition). OK.
- P3 eligibility: "23 ... exactly the 23 allies, Viet Nam among them" = policy_eligible_countries 23,
  policy_eligible_equals_coalition TRUE. OK.
- P8 constant sectors: Nuclear Upstream only (es_constant_cells_corrected), ES 0.704 for 217
  economies, no EO constant cell; Geothermal Midstream is dropped by the fixed filter in
  scripts/20_build_indices.R (sector != "Geothermal - Midstream"); 31 sectors in the table, 29
  scored (Electric Grid Downstream and Geothermal Upstream have no member ES). OK as far as the
  relayed (truncated) text goes.
- N7 imputed policy: 0 members in any stage (network$corrected$n_policy_imputed_max = 0).
- tab:corrections_reconciliation: ES 5,305 / 0.86 / 0.78; EO 6,598 / 0.72 / 0.78;
  SI 519 / 0.89 / 0.87 (within country); PSI 217 / 0.05 / --.
- "After the rerun": pooled ES 0.86, EO 0.72, SI 0.89; median within-cell 0.78 / 0.78 (SI within
  country 0.87); within-cell rank moves > 10 places: 60% (ES), 62% (EO); allied top sector changes
  for 11 of the 18 members scored in both releases.

Verdict: approve with repairs.
