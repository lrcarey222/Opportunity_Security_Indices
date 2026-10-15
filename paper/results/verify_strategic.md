# Verification log: theme "strategic"

Verifier run 2026-10-06. Worktree C:/Users/LCarey/osi-worktrees/paper (branch fix/allied-base-paper).
Backups of the pre-verification JSON and text fragment kept in the session scratchpad (vs/).

## 1. Clean re-run of scripts/paper/fig_strategic.R

- Run 1: `Rscript --vanilla scripts/paper/fig_strategic.R` from the worktree root, no env vars: exit 0, 25 s,
  only warning "cowplot built under R 4.4.2". All 5 figures (pdf + png), tables/strategic_allied.tex and
  strategic.json regenerated. JSON identical to the pre-verification copy EXCEPT the key
  `reconciliation_with_shipped`, which becomes `{available: false}` (the reconciliation runs only when
  OPSI_SHIPPED_INDEX_OUTPUTS points at the shipped file).
- Shipped file checked: .../Opportunity_Security_Indices/data/processed/outputs/index_outputs.rds,
  mtime 2026-09-03 13:58:35 (as required). Read only.
- Run 2: same with OPSI_SHIPPED_INDEX_OUTPUTS set to the shipped file: exit 0; JSON equivalent
  (sorted-key diff empty) to the pre-verification JSON, including the reconciliation block.
- Minor (reproducibility): the text quotes reconciliation numbers that exist in the JSON only when the
  env var is set. Now recorded in JSON meta.reconciliation_requires, and the script prints a message when
  the variable is absent, so a re-run without it is not mistaken for a change in the results.
- Script hygiene: reads data/processed (index_outputs.rds, tech_ghg_tbl.rds, charts/allied_strategic.csv)
  and data/raw/wdi_country_info.csv, plus config/config.yml for the directories; sources pure functions from
  R/; writes only via save_paper_fig(), write_paper_results() and paper/tables/strategic_allied.tex. No
  hard-coded paths (the shipped file comes from the environment). It stops if its recomputed SI differs from
  the stored one or its allied pick differs from allied_strategic.csv.

## 2. Independent recomputation (own code: scratchpad vs/indep.R, vs/recon.R; reads index_outputs.rds,
##    allied_strategic.csv and the shipped rds directly, not the JSON and not fig_strategic.R)

All of the following MATCH the compute agent's headline numbers:
- Policy: 230 economies with any component; DU 217, PAMS 215, CAT 65 (39 national + 26 EU bloc), NIPO 23;
  23 eligible == the 23 coalition members; DNK/FRA/ITA eligible via EU-bloc CAT.
- NIPO: 399 stored rows for eligible, 397 observed, 2 NA (Australia Gas-M, Oil-M); 414 U/M cells of 9 techs,
  15 with no row (NOR 8, MAR 4, KEN 3). 397/759 = 52.3% all four.
- Spearman pooled: PAMS-DU 0.028, NIPO-DU 0.22, NIPO-PAMS 0.15 (397), POL-NIPO 0.847, POL-PAMS 0.514,
  POL-CAT 0.545, POL-DU 0.292; NIPO-PAMS within-member median 0.471.
- POL by stage 0.575/0.649/0.694; without NIPO 0.648/0.670/0.694.
- Stored final SI (coalition): 713 rows in 31 sectors (Nuclear-U removed), 48 missing ES (23 Grid-D,
  23 Geothermal-U, CAN GH2-D, KEN GH2-M), 665 scored, 23 members, 29 sectors (21 x 29 cells, 2 x 28).
  SI = 0.2 pol + 0.3 EO + 0.3 (1-ES) + 0.1 lambda + 0.1 g exactly (max abs diff 0).
- Nuclear-U: ES = 0.7043 for all 23, EO sd 0.0388; ES diagnostics: constant, 217 economies (the only
  non-ok ES cell in the 11 techs). ES has no index cell at all for Grid-D, Geothermal-U, Geothermal-M.
- SI mean 0.5299, sd 0.0789, IQR 0.4768-0.5869, min 0.2774 USA Coal-U, max 0.7277 DNK Batteries-U.
- R2: sector 0.7386, country 0.029, both 0.7683, tech 0.6488, stage 0.018.
- Level shares 18.87/28.65/30.60/9.44/12.44%; within-country cov shares mean 31.03/-0.46/29.91/4.45/35.07%;
  pooled 31.99/-0.09/30.55/3.42/34.14%; median within Spearman SI~term 0.573/-0.059/0.540/0.134/0.668.
- EO vs need within member: 23/23 negative, mean -0.566, min -0.789 Chile, max -0.265 Kenya.
- Allied: 23 picks, 0 ties, 13 M / 10 U; BatU 8, GridM 8, EVM 4, WindU 2, BatM 1; margin median 0.036,
  min 0.0028 MAR, max 0.0994 DEU; <0.01: AUS BRA KEN MAR SAU; <0.02: 7 (+KOR, ARE). Agrees with
  allied_strategic.csv. Pick rank on policy: 1st for 10, top-3 for 20, median 2; need never 1st, top-3 10,
  median 6; EO median 7, top-3 1.
- Strength: pooled 0.247, median 0.259 (-0.119 SAU to 0.591 MAR), 4 negative, same 4, in-top-3 10;
  Nuclear-M picked by 6. No weights: median 0.831, same 17, in3 22. Strength no weights: median -0.035,
  14 negative, same 5, Gas-M 9. No NIPO: pooled 0.92, median 0.903, same 7, Batteries-M 12.
  No policy: same 3.
- Reconciliation (shipped rds mtime 2026-09-03): 571 shipped scored cells / 18 countries; 519 common,
  pooled 0.8891, within median 0.869 (0.728-0.968); 18 allies, 11 picks changed, 8 shipped Nuclear-U.

## 3. Figures (PNG inspected with the Read tool; anti-patterns.md and the figure rules)

- strategic_policy: three panels, no dual axes; grey bars + one blue highlight; coverage tiles on the
  sequential blue ramp with selective numbers (only cells < 23); correlation matrices on the
  blue-gray-red diverging scale for a signed quantity, legend present. Minor: the CAT bar label sat far
  from its bar (hjust proportional to string width) -> REPAIRED (hjust 0 + nudge). Caption said the
  within-country medians are over 23 economies; pairs with CAT are over 21 (CAT is constant within India
  and Korea) -> caption REPAIRED and the counts added to JSON (policy.spearman_within_country_n_members).
- strategic_heatmap: 23 x 32 tiles faceted by stage, sequential blue 0.25-0.75 (data range 0.277-0.728;
  the restricted range is stated in the caption, acceptable), excluded cells neutral grey and
  distinguishable from the lightest blue, allied pick outlined, overall maximum dotted, x marks for the two
  isolated missing-ES cells. Matches caption. No overlaps. Text 7-7.5pt (dense grid; acceptable).
- strategic_decomp: stacked bars with white gaps, pillar colours from paper_pillar_cols, one axis from 0,
  legend present, sectors sorted within member. Matches caption.
- strategic_allied: dumbbell ordered by margin, stage colours (blue U / orange M) consistent with the
  theme, legend present. Minor: runner-up labels were 6.2pt light grey -> REPAIRED (6.8pt, secondary ink).
- strategic_sign: lollipop + range bars + agreement dot pairs; one axis per panel, legend for the two
  markers. Minor: variant label "ES strength, no sector weights" inconsistent with "No stage or technology
  weight" -> REPAIRED ("Strength, no stage or technology weight"; also the JSON label).
- Table tables/strategic_allied.tex: booktabs + threeparttable, siunitx S columns (siunitx loaded; \SI is
  renewed in main.tex), \checkmark from amssymb (loaded). Totals row 4/17/13/7 matches robustness keys.

## 4. Text fragment paper/results/text/strategic.tex

- Every number (224 tokens) matches a JSON value (scripted check vs/numcheck.py; only "2026" unmatched).
  Semantic spot checks of about 40 claims against the specific keys all hold (component within medians
  -0.07..0.24; POL-NIPO 0.88 within; CAT constant for India/Korea; DU sd 0 across countries; NIPO/PAMS
  stage means; POL* by stage 0.38/0.51/0.60; focal ranks: DEU BatU policy 1 need 4, IND EVM / KOR BatM
  policy 1/2 need 18/16, USA GH2M need 1, NucD in top five of AUS/DEU/JPN/USA with policy or need rank
  <= 2; leading sector never better than 11th on EO; BatU ES 0.16-0.23 for 7 of 8 pickers, median 0.26,
  Chile 0.52, EO 0.56 vs 0.47; term gaps 0.023/0.024 and -0.004/-0.002/-0.003; all variant member lists).
- LaTeX: environments balanced (5 figure, 1 table), $ balanced, macros \ES \EO \POL \SI only in math
  mode, labels fig:/tab: prefixed, every \ref/\eqref target exists in sections/ or tables/, citations
  paruolo2013ratings and becker2017weights exist in references.bib. No LaTeX engine on this machine, so
  the fragment was not compiled.
- Wording repairs (minor, precision/overclaiming): "EO segments are of similar length" -> "vary less";
  "more than either measured pillar" -> "more than any of the three measured terms"; "import-dependent
  industrial economies" -> "the ES pillar scores these economies as exposed in battery minerals".
- Script repairs (major, reproducibility): sample.cells_excluded_missing_eo / _policy were hard-coded 0L
  and sample.sectors_without_es_pillar was a hard-coded list -> now computed from the exclusion reasons
  and from the ES diagnostics index_cells (values unchanged: 0, 0, Grid-D + Geothermal-U). Added
  policy.cells_pams_cat_du_only_breakdown (253/92/17) behind the text's breakdown of the 362 cells, and
  meta.reconciliation_requires plus a run-time message when the env var is absent.
- Final re-runs: without env var exit 0 (reconciliation available = FALSE, message printed); with
  OPSI_SHIPPED_INDEX_OUTPUTS exit 0, reconciliation restored (final state on disk). Diff against the
  pre-verification JSON: only the added keys and the one renamed label; no number changed.

## 5. Label mapping for integration (section 08 placeholders -> fragment)

- fig:policy_components (placeholder \includegraphics{fig_policy_components}) -> strategic_policy
- fig:si_heatmap (fig_si_heatmap) -> strategic_heatmap
- fig:si_decomp (fig_si_decomp; the placeholder says ten sectors of JPN KOR IND GBR USA) -> strategic_decomp
  (five sectors of USA, JPN, DEU, IND, KOR, AUS); the placeholder prose must be replaced too
- fig:allied_top (fig_allied_top; the placeholder mentions members without a selection: there are none)
  -> strategic_allied
- new: fig:strategic_sign (strategic_sign), tab:strategic_allied (\input{tables/strategic_allied})
- The whole placeholder Results subsection of 08_strategic.tex (paragraphs "Coverage of the Policy index"
  through "Allied selection" and their four floats) should be replaced by the fragment; keep
  \subsection{Results}\label{subsec:strategic_results}.

## 6. Verified \tbd fills for paper/sections/08_strategic.tex (outside the Results subsection)

1. l.110 \tbd{number of countries with each component and number of policy-eligible countries}:
   "of the 230 economies with at least one component in the eleven technologies, 217 have the dual-use
   score, 215 IEA PAMS, 65 CAT (39 national ratings and 26 EU members with the bloc rating) and 23 NIPO;
   23 economies, exactly the coalition members, are policy-eligible"
   [policy.economies_with_any_component, policy.economies_with_component.*, policy.eligible_n]
2. l.367 \tbd{number and share of tagged PAMS policies}: NOT computable from this theme's outputs
   (needs the raw PAMS tagging); leave to the policy-method owner.
3. l.616 \tbd{distribution of eligible cells over availability patterns}: "397 of the 759 eligible cells
   (52.3 percent: 194 Upstream, 203 Midstream) have all four components and 362 (47.7 percent) PAMS, CAT
   and the dual-use score only (253 Downstream, 92 Geothermal and Green Hydrogen Upstream or Midstream, 17
   other Upstream or Midstream cells without NIPO); the two patterns without PAMS do not occur"
   [policy.cells_all_four, share_cells_all_four, pattern_by_stage, cells_pams_cat_du_only_breakdown]
4. l.639 \tbd{number of policy-eligible countries and any coalition member that fails...}: "23, all of
   the coalition members; none fails, and Denmark, France and Italy qualify for CAT through the EU-bloc
   rating" [policy.eligible_n, eligible_all_coalition, eligible_with_eu_bloc_cat]
5. l.808 \tbd{mean and range across countries of the within-country correlation of EO and 1-ES}:
   "negative in every one of the 23 members, with a mean Spearman correlation of $-0.57$ and a range from
   $-0.79$ (Chile) to $-0.27$ (Kenya)" [si.eo_vs_need_within_country]
6. l.836 \tbd{share of the within-country variance of SI attributable to each term...}: "31.0 percent
   for policy, $-0.5$ percent for EO, 29.9 percent for need, 4.5 percent for the stage weight and 35.1
   percent for the technology weight (covariance shares, averaged over the 23 members)"
   [si.variance_share_within_country_cov_mean]
7. l.843 \tbd{rank correlation and top-sector agreement between the baseline and the all-ranked
   variant}: "a median within-member Spearman correlation of 0.91 (range 0.79 to 0.95; 0.89 pooled) and
   the same selected sector for 13 of the 23 members, with the baseline selection among the variant's
   three highest for 22" [robustness.si_allranked]
8. l.852 \tbd{$R^2$ of sector and of country fixed effects in SI}: "73.9 percent, against 2.9 percent
   for country fixed effects (76.8 percent for both)" [si.r2_fixed_effects]
9. l.905 \tbd{list of degenerate sectors...}: "one sector, Nuclear--Upstream, in which $\ES_{its}$ takes
   the value 0.704 for all 23 members (the ES diagnostics flag the cell as constant across all 217
   economies; $\EO_{its}$ is not constant)" [sample.degenerate_sectors, sample.es_constant_cells]
10. l.906 \tbd{number of cells and countries in the Strategic Index; cells excluded by each reason}:
   "665 cells of 23 countries in 29 sectors: of the 736 coalition cells in the 32 reported sectors, 48 lack
   the ES pillar (all 23 members in Electric Grid--Downstream and Geothermal--Upstream, where it is not
   defined, and Canada in Green Hydrogen--Downstream and Kenya in Green Hydrogen--Midstream), 23 are
   removed with Nuclear--Upstream, and none lacks EO or the Policy index; all cells of other economies
   are excluded as not policy-eligible" [sample.*]
- The tbd markers inside the Results subsection (l.968-1034) are superseded by the fragment.
- The compute agent's fill for l.110 (the visible part) matches item 1. Its remaining fills were truncated
  in the verifier's brief and could not be compared line by line; the list above is the verified set.

## Verdict: approve_with_repairs

---

# Second verification pass (2026-10-06, afternoon; "Try again")

The first pass above finished its log but did not return. This pass re-checks everything on disk.
Backups of the JSON, text and table before this pass: scratchpad vs2/.

## P2.1 Clean re-runs
- `Rscript --vanilla scripts/paper/fig_strategic.R` (no env var): exit 0, 25 s; message about the missing
  shipped file printed as designed.
- Same with OPSI_SHIPPED_INDEX_OUTPUTS = shipped rds (mtime 2026-09-03 13:58:35): exit 0, 35 s.
  JSON deep-diff against the pre-pass copy: empty. Table identical. Five pdf + png regenerated.

## P2.2 Independent recomputation (new code vs2/indep2.R, vs2/recon2.R: base R on the stored
##      tables, no pipeline function, no JSON, no fig_strategic.R)
All MATCH: 230 / DU 217 / PAMS 215 / CAT 65 (26 EU bloc, 39 national) / NIPO 23; 23 eligible = coalition
(DNK, FRA, ITA via EU bloc); 397 of 759 cells with NIPO (52.3%); Spearman POL-NIPO 0.847; POL by stage
0.575/0.649/0.694; stored coalition SI 713 rows / 31 sectors, 48 missing ES, 665 scored, 23 x 29; SI =
formula exactly; mean 0.5299 sd 0.0789 IQR 0.4768-0.5869, min USA Coal-U 0.2774, max DNK Batteries-U
0.7277; Nuclear-U ES 0.7043 for all 23; R2 sector 0.7386 / country 0.029 / tech 0.6488 / stage 0.018 /
both 0.7683; level shares 18.87/28.65/30.60/9.44/12.44; within cov shares 31.03/-0.46/29.91/4.45/35.07;
EO-need within 23/23 negative, mean -0.566, Chile -0.789, Kenya -0.266; allied 23 picks, 8/8/4/2/1,
13 M / 10 U, margin median 0.036, MAR 0.0028, DEU 0.0994, <0.01 AUS BRA KEN MAR SAU, <0.02 7, equals
allied_strategic.csv; strength pooled 0.247, median 0.259 (-0.119..0.591), 4 neg, same 4, in3 10, NucM 6;
no weights median 0.83, same 17, in3 22; strength no weights median -0.035, same 5, GasM 9; no policy same 3;
pick rank: policy 1st 10, top-3 20, median 2; need 1st 0, median 6; EO median 7.
Reconciliation (own alias table): 571 shipped scored / 18; 519 common / 18; pooled 0.889; within median
0.869 (0.728-0.968); 11 of 18 picks changed; 8 shipped picks Nuclear-U.

## P2.3 Figures (all five PNGs re-opened after the re-run)
No defect found. policy: grey bars + one blue highlight, structurally absent cells neutral grey and
distinct from the lightest ramp step, diverging scale only for the signed correlations, legends present.
heatmap: sequential blue 0.25-0.75 (data 0.277-0.728; restriction stated in the caption), whole grey
columns labelled, x marks, outline = allied pick, dot = overall max; matches the caption. decomp: one axis
from 0, legend, sorted within member, white gaps. allied: dumbbell ordered by margin, stage colours from
paper_stage_cols, legend, no label collisions (Morocco's 0.003 margin still legible). sign: one axis per
panel, legend for filled/open markers. Note (not a defect of this theme): paper_pillar_cols reuses the
stage slots (ES blue = Upstream, EO orange = Midstream) by the theme's design; no strategic figure encodes
both at once.

## P2.4 Text fragment: errors the first pass missed
The first pass's number check matched each number against ANY JSON value, so a wrong number that equals an
unrelated key passed. Semantic re-check of every number against its own key found:
- MAJOR (wrong number): no-NIPO variant within-member range "0.83 to 0.97"; the JSON min is 0.8246, so
  0.82 (0.83 had matched robustness.si_nopriors.pooled_spearman). REPAIRED -> "0.82 to 0.97".
- MAJOR (wrong number): EO level share "28.7 percent"; unrounded 28.6457% (the JSON stored 0.2865 at 4 dp
  and was double-rounded). REPAIRED -> "28.6"; script now stores all level/variance shares at 6 dp
  (0.286457), so the JSON backs the one-decimal percentages unambiguously. The compute agent's headline
  "EO 28.7%" is likewise wrong.
- Every other boundary case checked against unrounded values: 0.541 (0.54053), 0.637 (0.63650), 0.024
  (0.023525), 4.5 (4.4545), 0.458 (0.45848), 0.119, 0.046, 0.55 (POL-CAT 0.5454): all correct.
- MINOR (precision): "offsets their lower policy rank and stage weight" compared Upstream with Midstream
  implicitly (Upstream's stage weight exceeds Downstream's). REPAIRED: "Relative to Midstream cells, ...".
- MINOR (consistency with Appendix D): "zero-filled ES pillar" contradicts Appendix D, which says the zeros
  came from investment cells manufactured with zero values, "not imputation". REPAIRED wording.
- Verified against the shipped file (read only): shipped SI = 0.2 pol + 0.3 EO + 0.3 c + 0.1 lambda + 0.1 g
  exactly, where its column c = 1 - (shipped ES pillar) exactly; shipped ES pillar Nuclear-U = 0 for all 218
  economies, so the need term was 1. The text's "constant was zero, so that the need term equalled one" is
  correct. With ES 0.704 now, Nuclear-U's need (0.296) lies below 83-100% of each member's cells, so
  "would have pushed the sector down in every country" also holds.
- Construction claims checked: NIPO stored values within-country ranks and earlier HS6 map (sec. 08
  l.301, l.325); PAMS S-curved over all cells jointly (sec. 08 table l.97); subsec:eo_shared lists the ES/EO
  shared signals; policy term mean 0.5 in every member (exactly); "reported sectors" excludes
  Geothermal-Midstream by STRATEGIC_EXCLUDED_SECTORS, defined in sec. 08 l.878.
- LaTeX (own checker vs2/texcheck2.py): 5 figure envs balanced, $ even, braces balanced, 19 refs all
  resolve, cites paruolo2013ratings / becker2017weights in references.bib, \ES \EO \POL \SI only in math,
  amsmath/amssymb/siunitx/booktabs/threeparttable/natbib loaded, graphicspath {figures/}. After repairs
  222 numbers, all match their keys (only "2026" unmatched, a year).

## P2.5 Final re-run after repairs
OPSI_SHIPPED_INDEX_OUTPUTS set: exit 0; JSON differs from the pre-pass copy only in the added digits of the
share keys; table unchanged; figures regenerated identically.

## P2.6 tbd fills (re-verified; the ten items of section 6 above stand) plus three in Appendix D
- appendix_d l.720-721 (P3) \tbd{number of policy-eligible countries in the rerun, and whether all 23 allies
  are among them}: "23, exactly the 23 allies (Denmark, France and Italy through the EU-bloc CAT rating)"
  [policy.eligible_n, eligible_all_coalition, eligible_with_eu_bloc_cat]
- appendix_d l.804 (P8) \tbd{sectors flagged as constant and excluded in the rerun, if any}: "one,
  Nuclear--Upstream, where ES equals 0.704 for every coalition member (constant across all 217 economies in
  the ES diagnostics); no EO cell is constant" [sample.degenerate_sectors, es_constant_cells,
  eo_index_cells_status_11tech]
- appendix_d l.1047 (N7) \tbd{number of coalition members with imputed policy after ... P3}: "none: the
  Policy index covers all 23 members in all 759 of their cells" [policy.eligible_n, eligible_cells; the
  network's allied_network_input_checks.csv reads 759 policy_index rows]. Network owner to confirm wording.

## Verdict (pass 2): approve_with_repairs

---

# Third verification pass (2026-10-06 22:00; "Try again")

Pass 2 finished its log at 14:55 but did not return. This pass re-checks the files on disk (unchanged
since 14:54) against the CURRENT sections (other agents edited paper/sections/ after 14:55).

## P3.1 Clean re-runs
- Sections, main.tex and references.bib unchanged since 2026-10-05 (before pass 2), so the label mapping
  and tbd line numbers of pass 2 still apply (re-checked below).
- `Rscript --vanilla scripts/paper/fig_strategic.R` without env var: exit 0, 31 s (message as designed).
- With OPSI_SHIPPED_INDEX_OUTPUTS = shipped rds (mtime 2026-09-03 13:58:35, read only): exit 0, 42 s.
  JSON deep-diff against the on-disk pre-pass copy: empty. Table byte-identical. 5 pdf + 5 png regenerated.

## P3.2 Independent recomputation (new code vs3/indep3.R: base R on the stored tables, coalition taken
##      from allied_strategic.csv, own S-curve; no JSON, no fig_strategic.R, no pipeline function)
All MATCH the headline numbers and the JSON: 230 any / DU 217 / PAMS 215 / CAT 65 (26 EU bloc, 39
national) / NIPO 23; eligible 23 = coalition (DNK, FRA, ITA via EU bloc); 759 cells, 397 with NIPO; POL
without NIPO by stage 0.648/0.670/0.694; final stored SI 713 coalition rows / 31 sectors, 48 missing ES,
665 scored, 23 members, 29 sectors, formula exact (max diff 0), no Nuclear-U; mean 0.5299 sd 0.0789 IQR
0.4768-0.5869, min USA Coal-U 0.2774, max DNK Batteries-U 0.7277; R2 sector 0.7386, country 0.0290, tech
0.6488, stage 0.0180, both 0.7683; level shares 18.871/28.646/30.604/9.435/12.444 % (EO 28.6, confirming
the pass-2 repair of "28.7"); within-country cov shares 31.03/-0.46/29.91/4.46/35.07 %; EO-need 23/23
negative, mean -0.566, Chile -0.789, Kenya -0.266; allied 23 picks BatU 8, GridM 8, EVM 4, WindU 2, BatM 1,
13 M / 10 U, margin median 0.036, MAR 0.0028, DEU 0.0994, < 0.01 AUS BRA KEN MAR SAU, < 0.02 n = 7, equal to
allied_strategic.csv; strength pooled 0.247, median 0.259 (SAU -0.119 .. MAR 0.591), 4 negative, same 4,
in top-3 10, Nuclear-M 6; no NIPO pooled 0.920, median 0.903 (0.825 .. 0.969), same 7, Batteries-M 12; no
weights median 0.83 (0.41 .. 0.93), same 17, in top-3 22. Baseline pol reproduces exactly from POL ranked
within member over the scored cells (so the no-NIPO variant re-ranks over the same cells). ES diagnostics:
the only non-ok cell in the 11 techs is Nuclear-Upstream, constant, 217 economies.

## P3.3 Figures (all five PNGs re-opened after the re-run; anti-patterns.md and the figure rules)
No blocker or major. policy: grey bars + one blue highlight, coverage tiles sequential blue with
selective numbers and neutral grey for structural absences, correlation matrices on the blue-gray-red
diverging scale (signed quantity) with a legend. heatmap: sequential blue 0.25-0.75 (data 0.277-0.728,
restriction stated in the caption), grey columns labelled, x marks for the two isolated gaps, black
outline = allied pick, white dot = overall max; matches caption. decomp: one axis from 0, five term
colours with white gaps, legend, sorted within member. allied: dot-plot dumbbell (non-zero axis is
appropriate for position marks), stage colours from paper_stage_cols, legend, no collisions. sign: one
axis per panel, legend for filled/open markers. Small text: tile numbers in policy (b) about 6 pt and
runner-up labels in allied 6.8 pt, legible at 6.5 in (minor, left as is).

## P3.4 Text fragment
- Semantic spot checks of about 60 claims against their own keys (policy patterns and NIPO gaps; CAT
  constant for India/Korea because c = 0.5 makes c*g + (1-c)(1-g) = 0.5, R/categories/policy/
  cat_policy_index.R; within-member component medians -0.07..0.24; stage weights 0.5/0.75/0.25 behind the
  stage-offset sentences; focal ranks DEU/IND/KOR/USA; Nuclear-D in top five of AUS/DEU/JPN/USA with a
  policy or need rank <= 2; leading sectors' EO ranks 11/20/11/19/20/28; BatU ES range; Chile pol = 1; term
  gaps; every variant's member lists; no-policy picks 20 of 23 Upstream renewable/grid): all hold.
- Scripted check vs3/numcheck3.py: 224 numbers, every one equals a JSON value at its printed precision.
- Cross-theme consistency: the reconciliation numbers (519 / 0.889 / 0.869, 0.728-0.968 / 11 changed / 8
  Nuclear-U) equal reconcile.json agreement.SI and allied; the no-NIPO numbers (0.920 / 0.903 / 0.825-0.969
  / 7 same) equal robustness.json si.variants.no_nipo.
- LaTeX (vs3/refcheck.py): 5 figure + 1 table environments balanced, braces and $ balanced, every
  \ref/\eqref target exists (sections 06, 08, appendix D, the table), cites in references.bib, macros only
  in math, no label collides with another theme's fragment or table.
- MINOR repair: heatmap caption now says Geothermal--Midstream is not a reported sector (the Midstream
  facet has ten columns, which the caption's "32 reported sectors" otherwise leaves unexplained).

## P3.5 tbd fills
The ten section-08 fills and three appendix-D fills of pass 2 stand (line numbers unchanged). One more
appendix-D marker can be filled from this theme:
- appendix_d l.841-842 \tbd{sensitivity of the Policy index, the SI and the allied selection to dropping
  the NIPO component, or to rescoring it on the July 2026 extract if that run is made}: "dropping NIPO
  (a Policy index from IEA PAMS, CAT and the dual-use score with weights 3, 2 and 1, re-ranked within
  country) narrows the Downstream--Upstream gap in the mean Policy index from 0.119 to 0.046, leaves the
  within-member ordering of the Strategic Index largely intact (median within-member Spearman correlation
  0.90, range 0.82 to 0.97; 0.92 pooled), but keeps the allied selection of only 7 of the 23 members, with
  Batteries--Midstream selected by 12; the July 2026 rescoring has not been run"
  [policy.pol_gap_downstream_minus_upstream(_without_nipo), robustness.si_nonipo.*; equals
  robustness.json si.variants.no_nipo]. Note the marker says "Section~\ref{sec:robustness} reports"; the
  numbers are also reported in Section 8's Results (fragment, last robustness paragraph).

## Verdict (pass 3): approve_with_repairs (one minor caption repair this pass; all earlier repairs intact)
