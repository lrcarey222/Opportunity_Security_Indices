# Verification log: theme "method"

Verifier session 2026-10-06. Worktree C:/Users/LCarey/osi-worktrees/paper (branch fix/allied-base-paper).

## Step 0: inventory
- Script: scripts/paper/fig_method.R (1054 lines). Figures: method_weights, method_category_sets,
  method_imputation, method_coverage (pdf + png). JSON: paper/results/method.json. Text:
  paper/results/text/method.tex. Table: paper/tables/method_imputation.tex.

## Step 1: clean re-run
- `Rscript --vanilla scripts/paper/fig_method.R` from the worktree root: exit 0, 28 s. All four
  PDFs/PNGs, method.json and tables/method_imputation.tex regenerated (new mtimes). method.json is
  byte-identical in content to the pre-run copy (JSON diff empty).
- Input mtime: data/processed/outputs/index_outputs.rds 2026-10-01 11:04 (corrected rerun).

## Step 2: independent recomputation (own base-R code, scratchpad vm_check*.R; not reading JSON)
All match the JSON and the text:
- Headline cells ES 6,228 / EO 7,125; all-tech 7,578 / 7,508; both pillars 6,226.
- 219 economies in each pillar; grid by stage ES 219/215/219, EO 219/216/219.
- ES headline pairs 30; only constant pair Nuclear-Upstream (range 0, from raw index values).
- iota: ES mean 0.3112 median 0.2727, by stage Up 0.4186 Mid 0.2360 Down 0.2702; EO 0.4364 /
  0.4583, Up 0.4361 Mid 0.4427 Down 0.4305. Fully imputed: ES 0; EO 24 (Isle of Man 14, Puerto
  Rico 8, Liechtenstein 2; 19 midstream). No imputation: ES 2,178, EO 382. iota rebuilt from
  variable_contributions: max error 0 (JSON says 1.1e-16 via the script's route; both < 1e-15).
- Decomposition: max |index - sum cat| = 0, max |index - sum var| = 1.11e-16, n = 7,578 / 7,508.
- Effective weights: ES Trade-Mid 0.588, Consumption-Down 0.492, Energy Prices-Up 0.192; EO
  Technology Demand 0.198-0.252, Cost Competitiveness 0.205-0.243, TRL 0.136-0.162.
- Category sets: no pair with >1 set; 17 distinct ES sets, 19 EO; 1,493 of 6,228 ES cells
  (0.2397) single-category; pair counts 1-7 (median 3) ES, 2-7 (median 5) EO; 8 single-cat ES pairs.
- Years: ES 0.5602 of cells >1 year (max 3); EO 0.8964 (max 6), 0.8648 counting years <= 2026
  (max 4); oldest EO component Input Cost Index 1980, Papua New Guinea, 33 cells, category Cost
  Competitiveness.
- SI: 665 in-sample cells, 23 economies, 29 sectors; policy_index 23 economies. Nuclear-Upstream and
  Geothermal-Midstream absent from the SI table; R/indices/build_indices.R
  strategic_index_degenerate_sectors() + scripts/20 confirm the degenerate-sector exclusion.
- Comtrade critmin raw files: 2024 import 130, export 128, total export 130 reporters; 2025 files 3 each.
- Within-pair component weights lambda: ES constant everywhere; EO varies only in downstream Cost
  Competitiveness (LCOE vs Input Cost Index, LCOE theme has no fill rule); no constant component has
  varying lambda, so the "constant inputs do not reorder economies within a pair" claim holds.
- JSON encoding: names such as Curacao/Reunion are valid UTF-8 bytes (C3 A7); an apparent mojibake
  was a Python cp1252 read artefact, not a defect.

## Step 3: figures (PNG inspection, 6.5in at 150 dpi)
- method_weights: stage colours correct (blue/orange/aqua), nominal tick legend present, x from 0,
  one axis, no overlaps. OK.
- method_category_sets: 33 columns x 2x10 rows; counts and light-blue constant tiles agree with
  every claim in the text (8 single-category ES pairs; Energy Prices constant in 10 upstream pairs
  per pillar; TRL in all 33 EO pairs; Technology Demand constant in 5 EO pairs). Annotations
  ("pair not scored", counts) at ggplot size 2.3-2.5 (6.5-7pt) are below the 8pt rule -> minor.
- method_imputation: sequential single-hue blue on fixed 0-1 limits, dashes for unobserved, pillar
  row labelled, companion table. Labels at 2.6 (7.4pt) -> minor.
- method_coverage: dumbbell, legend for pillar colour and mark shape, ring at 0 for Nuclear-Up
  fully visible; annotations at size 2.3 (6.5pt) -> minor. Pillar colours (blue ES / orange EO)
  coincide with the Upstream/Midstream slots; acceptable because the panels are the stages and the
  theme fixes the pillar colours; legend present.

## Step 4: text fragment findings
- LaTeX: all 17 \ref labels and both \citet keys exist (paper/sections, references.bib); macros
  \ES \EO \1 defined in main.tex; siunitx/threeparttable loaded; four figure environments balanced.
- Numbers: every number checked against the JSON; all match.
- Issues found:
  (m1) "Upstream the gap is large wherever the grid is set by the commodity-price series" overstates:
       every upstream pair has that grid, but the gap ranges from 8 (Oil) to 60 economies (217 in
       Nuclear). Rewrite with the range (new JSON key coverage.gap_by_stage).
  (m2) "followed by ... the geothermal pairs": only the upstream and midstream geothermal pairs
       (144/212 each); geothermal downstream is 216/217.
  (m3) Technology Demand imputation explained by "two features"; the global-demand term
       (world-level, recorded for 108 economies per pair and filled with the identical value for the
       rest) is imputed in about half the cells and contributes ~15% of the category's imputed
       weight. Add it (new JSON key imputation.technology_demand_components).
- tbd fills: the compute agent returned none. Sections 02, 03 and 04 contain 13 \tbd placeholders
  that this theme's numbers answer; added as method.json$tbd_fills (generated by the script).

## Step 5: repairs (all in owned files), then re-run
- scripts/paper/fig_method.R
  - In-plot annotation sizes raised from 2.3-2.6 to 2.8 (about 8pt): category-set counts and
    "pair not scored", imputation pillar labels and dashes, coverage "no ES score"/"ES constant".
    PNGs re-inspected: no overlap or clipping.
  - New JSON keys: coverage.gap_by_stage (scored minus country-specific economies, min/max by
    pillar-stage), imputation.technology_demand_components (share imputed, contribution to the
    category's imputed share, observed economies per pair, by stage and component), and tbd_fills
    (13 LaTeX fills for the \tbd placeholders of Sections 2-4, each computed from the same objects).
  - Re-run: `Rscript --vanilla scripts/paper/fig_method.R` exit 0; figures, table and JSON regenerated.
- paper/results/text/method.tex
  - (m1) upstream coverage gap sentence now gives the range (8 in Oil to 60 in Solar, Wind and
    Green Hydrogen) instead of "large wherever the grid is set by the price series".
  - (m2) "the geothermal pairs" -> "the upstream and midstream geothermal pairs (144 of 212 each)".
  - (m3) Technology Demand imputation now attributes the category's imputed share to its three
    partial-coverage terms: addressable market 0.37-0.48 (observed for one market per pair),
    regional demand outlook 0.22-0.33 (median 12 observed economies), global demand 0.11-0.13
    (world series recorded for 108 economies, identical everywhere, imputed in about half the cells).
  - "Because most categories are absent at most stages" (false for EO, which has 6-7 categories at
    every stage) -> "Because the median pair observes only three of the ten categories in ES and five
    in EO"; "almost as wide" -> "span almost the whole universe".
- Number audit after repairs: all 153 numeric tokens in the fragment occur in method.json (script
  numaudit.py), and each headline one was checked semantically against the key it comes from.
- LaTeX structure: braces balanced, 4 figure environments balanced, $ balanced per paragraph, labels
  fig:method_* only, refs/cites all resolve. No LaTeX engine on this machine, so not compiled.

## tbd fills (method.json$tbd_fills; integration replaces the whole \tbd{...})
- 02_framework.tex: ES pairs -> 30; EO pairs -> 33; economies with ES -> 219; with EO -> 219;
  coalition members with Policy index -> "all 23"; SI exclusions -> Nuclear--Upstream (ES constant
  across 217 economies), none in EO.
- 03_data.tex: ES universe -> 219 economies, 195 sovereign (193 UN + Kosovo + Taiwan), 24 territories;
  EO -> the same 219 (195 + 24).
- 04_normalization.tex: year mixing (56% ES, 90% EO; max 3 and 6, 4 excluding projections); grids by
  stage (219/215/219, 219/216/219; 4 upstream economies without country-specific data); one category
  set in all 30 ES and 33 EO pairs, categories per cell 1-7 (median 3, 24% single) and 2-7 (median 5);
  decomposition errors (0 and 1.1e-16; 7,578 and 7,508 cells); fully imputed cells (0 ES, 24 EO) and
  mean iota by stage; pair status (29 ok + 1 constant of 30 ES, 3 without ES; 33 EO ok; 43/44 and 39/39).

## Not repaired (minor, outside scope or by design)
- The script also reads config/weights.yml, config/raw_inputs_manifest.yml and
  data/raw/raw_inputs_resolved.yml + critmin CSVs (config is the specification of the nominal weights;
  reads are read-only).
- sections/03_data.tex references figures/fig_data_coverage.pdf, which does not exist in
  paper/figures (not this theme's figure; flagged for integration).
- Label mapping: no section placeholder corresponds to the method figures; all labels are new
  (fig:method_category_sets, fig:method_weights, fig:method_imputation, fig:method_coverage,
  tab:method_imputation).

## Verdict: approve with repairs.

---

# Verification session 2 (2026-10-06 14:42 PDT, workflow re-run "Try again")

Assume-errors pass over the state left by session 1 (script 09:15, figures/JSON 09:16, text 09:18).

## 2.1 clean re-run
- `Rscript --vanilla scripts/paper/fig_method.R` from the worktree root: exit 0, 27.8 s, no
  warnings printed. Four PDFs + PNGs, method.json and tables/method_imputation.tex regenerated
  (mtimes 14:43). method.json and the table are byte-identical to the pre-run copies (cmp), so the
  script is deterministic and the session-1 outputs are what the script produces.

## 2.2 independent recomputation (own data.table code, scratchpad vm2/indep*.R; JSON not read)
All of the following match method.json and the text:
- Cells: ES 6,228 headline / 7,578 all; EO 7,125 / 7,508; both pillars 6,226; 219 economies in
  each pillar, identical sets; grids by stage ES 219/215/219, EO 219/216/219.
- Pair status from raw index ranges: 30 ES pairs, the only zero-range pair is Nuclear-Upstream
  (217 economies); 33 EO pairs, none zero-range.
- iota rebuilt as sum(omega*lambda*imputed) from variable_contributions: equals the builder's
  diagnostic exactly (max error 0); ES mean 0.3112 / median 0.2727, by stage 0.4186/0.2360/0.2702;
  EO 0.4364 / 0.4583, 0.4361/0.4427/0.4305. Fully imputed: ES 0, EO 24 (Isle of Man 14, Puerto
  Rico 8, Liechtenstein 2; 19 midstream). No imputation: ES 2,178, EO 382.
- Effective weights (from category_contributions, nominal from config/weights.yml): mean |gap|
  ES 0.1060, EO 0.0778; max gap ES 0.4932 at Trade-Midstream (0.5884 vs 0.0952); ES
  Consumption-Down 0.4924, Energy Prices-Up 0.1923 of which Nuclear 0.0995 (about half), other
  upstream cells 0.1030; EO Technology Demand 0.198-0.252, Cost Competitiveness 0.205-0.243,
  TRL 0.136-0.162.
- Category sets: 0 pairs with more than one set; 17 ES and 19 EO distinct sets; per-cell counts
  1-7 (median 3) and 2-7 (median 5); 1,493 single-category ES cells (0.2397); 8 single-category
  ES pairs (same list as the text); median ES categories per pair by stage 5 / 1.5 / 2; Reserves
  downstream only in Geothermal, Solar, Wind; constant categories: Energy Prices 10 pairs (both
  pillars), TRL 33, Technology Demand 5 (Batteries Up/Mid, Geothermal x3).
- Weight on constant categories: EO 0.3683/0.2223/0.1752, ES 0.1923/0/0.
- Category imputed shares (Reserves-Up 0.872 contributing 0.159; Energy Imports 0.638; FD 0.500,
  Production 0.505 (mineral-supply / mineral-production variables); TD 0.732-0.855 contributing
  0.163-0.192; Investment 0.635-0.830; Energy Prices 0.005; TRL 0.019-0.039). Reserves upstream
  fill is the zero rule (1,712 rows), as the text says.
- Coverage (own country-specific classification): ES mid/down gap 0 in all 20 pairs; ES upstream
  gap 8 (Oil) to 60 (Solar, Wind, Green Hydrogen: 158 of 218), Nuclear 0 of 217; EO Nuclear-Up
  146/218, Electric Grid-Down 143/212, Geothermal Up/Mid 144/212, Solar/Wind/GH-Up 150/218; four
  upstream economies without country-specific data (same four, both pillars).
- Territories (hand list of the 24): mean iota 0.4734 vs 0.2936 (ES), 0.6711 vs 0.4101 (EO).
  Channel Islands = CHI and Kosovo = XKX, the only codes outside ISO 3166.
- Years: ES 0.5602 of cells mix years (max 3); EO 0.8964 (max 6), 0.8648 up to 2026 (max 4);
  oldest EO component PNG Input Cost Index 1980 in 33 cells.
- SI: 665 cells, 23 economies, 29 sectors; Geothermal-Midstream and Nuclear-Upstream absent.
- Comtrade raw files: 2024 import 130 / export 128 / total export 130 reporters; 2025 files 3 each.
- Decomposition: with data.table GForce sums the max errors are 1.1e-16 (category) and 2.2e-16
  (variable); with base sum in reverse order the category error is 0. The script's "0" and
  "1.1e-16" are therefore summation-order dependent; the index is literally
  sum(weighted_category_contribution) in R/indices/index_builder_core.R, so zero is tautological.

## 2.3 tbd fills
- Sections 02-04 were rewritten after session 1 (mtimes 14:40-14:51). All 13 anchors in
  method.json$tbd_fills still match a \tbd{} in the current files exactly (whitespace-normalised;
  scratchpad vm2/tbd.R), and every fill reads grammatically in its sentence. Fill values agree
  with the JSON and with the independent recomputation above.

## 2.4 figures (PNG re-inspection at 975 px = 6.5 in)
- method_weights, method_category_sets, method_imputation, method_coverage: no overlap or
  clipping, annotation text about 8 pt, stage colours correct, single axis, legends present,
  0-1 sequential blue with fixed limits for the heatmap, x from 0 for the weights and coverage
  charts. Category order matches Table tab:category_weights (checked against 04_normalization).
  No anti-pattern from references/anti-patterns.md applies. Nothing to repair.

## 2.5 text fragment
- All 22 \ref/\eqref labels resolve in the current sections (the equations still say what the
  text attributes to them: affine_constant, effective_weight, imputed_share, latest_year,
  cat_decomp, var_decomp, data_resolve); both \citet keys exist in references.bib; environments
  and braces balanced; $ balanced per paragraph; no unescaped % or &; ASCII only; no duplicate
  of the five labels anywhere else in sections/ or results/text/. All 108 numeric tokens occur in
  method.json (vm2/texcheck.py), and the semantically important ones were matched above.
- Repairs (minor):
  (r1) "every pair has a gap. It ranges from 8 ... to 60" silently excluded Nuclear (gap 217).
       Now "Leaving aside Nuclear, it ranges from 8 ... to 60 ...; In Nuclear upstream ... so the
       gap is the whole pair" (JSON coverage.gap_by_stage max_gap_excl_constant_pairs = 60,
       max_gap = 217).
  (r2) "the largest absolute difference ... category contributions is exactly zero" presented a
       tautology as a finding and is summation-order dependent. Now "is zero, as it must be, since
       the builders compute the index as that sum; the check confirms that the stored
       contributions are the ones that produced the index ... 1.1e-16, a rounding error".
- Checks re-run after the edits: refs, cites, balance, number audit all clean.

## 2.6 not repaired / for integration
- sections/03_data.tex includes figures/fig_data_coverage (label fig:data_coverage, theme x stage
  source coverage before imputation); no script produces it and it is not the same as
  method_coverage (pair-level economies scored vs country-specific). Needs an owner.
- Script reads config/*.yml and data/raw critmin CSVs (read-only; config is the specification of
  the nominal weights) and writes paper/tables/method_imputation.tex directly (owned file).

## Verdict (session 2): approve with repairs (two minor text repairs; no blocker or major found).

---

# Verification session 3 (2026-10-06 22:00 PDT, workflow re-run "Try again")

State on entry: script 09:15, figures/JSON/table 14:43, text 14:51 (session-2 repairs). Sections
unchanged since 2026-10-05. Pre-run copies saved to scratchpad vm3/pre.

## 3.1 clean re-run
- `Rscript --vanilla scripts/paper/fig_method.R` from the worktree root: exit 0, 37 s. All four
  PDFs + PNGs, method.json and tables/method_imputation.tex regenerated (mtimes 22:00). JSON,
  table and all four PNGs byte-identical to the pre-run copies (cmp): deterministic.

## 3.2 independent recomputation (fresh base-R code, scratchpad vm3/indep*.R; JSON not read)
All of the following reproduce method.json and the text:
- Cells: ES 6,228 headline / 7,578 all; EO 7,125 / 7,508; both 6,226; 219 economies, same set
  in both pillars; grids ES 219/215/219, EO 219/216/219.
- Pair status (index_cells): ES 44 pairs, 43 ok + Nuclear-Upstream constant (217 economies,
  single value 0.704316); headline 29 ok + 1 constant; EO 39/39 ok, 33/33 headline; no unscored.
- iota: ES mean 0.3112 / median 0.2727, by stage 0.4186/0.2360/0.2702; EO 0.4364 / 0.4583,
  0.4361/0.4427/0.4305; fully imputed ES 0, EO 24 (IMN 14, PRI 8, LIE 2; 19 midstream); none
  imputed ES 2,178, EO 382. Rebuilt from variable_contributions: max error 0.
- Decomposition: category max error 0, variable 1.11e-16, over 7,578 / 7,508 cells.
- Effective weights (category_weight / cells, nominal from config/weights.yml): ES Trade-Mid
  0.5884 (nominal 0.0952), Consumption-Down 0.4924, Energy Prices-Up 0.1923; mean |gap| 0.1060 /
  0.0778, max 0.4932; cells-vs-pairs max difference 0.032 (ES), 0.0009 (EO). EO TD 0.198-0.252,
  CC 0.205-0.243, TRL 0.136-0.162. Columns sum to 1.
- Category sets: 0 pairs with >1 set; 17 / 19 distinct sets; cells 1-7 (median 3), 2-7 (median 5);
  1,493 single-category ES cells (0.2397); 8 single-category ES pairs (same list as text); median
  ES categories per pair by stage 5 / 1.5 / 2; EO Electric Grid-Down = {CC, TRL}; ES Reserves
  downstream only Geothermal, Solar, Wind; Reserves-Up imputed rows all via the zero rule (1,712).
- Constant weight: categories EO 0.3683/0.2223/0.1752, ES 0.1923/0/0; categories + components
  EO 0.4933/0.3941/0.2708, ES 0.2306/0.1961/0.
- Category imputed shares and contributions to stage-mean iota: ES Reserves-Up 0.872 (0.159),
  Energy Imports 0.638, FD-Up 0.500, Production 0.505, Investment 0.635-0.830; EO TD 0.732-0.855
  (0.163-0.192), Investment 0.664-0.830, TRL 0.019-0.039, Energy Prices 0.005. TD terms:
  addressable market observed 1 economy/pair (0.369-0.478 of the category's imputed share),
  regional demand median 12 observed (0.219-0.326), global demand 108 observed, 50.1% imputed
  (0.114-0.131), overcapacity 209 observed.
- Coverage (own country-specific classification): ES mid/down gap 0 in 20 pairs; ES upstream gap
  8 (Oil) to 60 (Solar/Wind/GH, 158 of 218), Nuclear 0 of 217; EO Nuclear-Up 146/218, Grid-Down
  143/212, Geothermal Up/Mid 144/212, Solar/Wind/GH-Up 150/218; min EO gap 1; the same four
  upstream economies without country-specific data in both pillars.
- Sovereignty (own hand list of non-sovereign ISO3 codes, not COW): 195 sovereign / 24
  territories, identical list; TWN and XKX present, VAT absent; mean iota territory vs sovereign
  ES 0.4734 / 0.2936, EO 0.6711 / 0.4101.
- Years (positively weighted components, Year_selected > 0): ES 0.5602 (max 3); EO 0.8964 (max 6),
  0.8648 up to 2026 (max 4), median span 3; oldest PNG Input Cost Index 1980, 33 cells = all 33
  PNG EO headline cells, 11 per stage, category Cost Competitiveness. (Note: counting every
  year_provenance row, including zero-weight components, gives EO 0.915; the script's restriction
  to components that enter the index is the right one and is documented in the JSON definitions.)
- SI: 665 cells, 23 economies, 29 sectors; Nuclear-Upstream and Geothermal-Midstream rows absent
  from the SI table; Electric Grid-Down and Geothermal-Up present but "missing energy security".

## 3.3 tbd fills
- All 14 entries of method.json$tbd_fills match a \tbd{} in the current sections 02-04 exactly
  (whitespace-normalised; vm3/tbd.py); each fill read in its sentence context: grammatical, and
  values agree with 3.2. The remaining \tbd in 02 (ES-EO Spearman) belongs to another theme.

## 3.4 figures (PNG inspection at 975 px = 6.5 in; PDFs 468 pt wide)
- method_weights (6.5x3.3 in): stage colours blue/orange/aqua from paper_stage_cols, black
  nominal tick, legend for all four series, one x-axis from 0 to 0.6, rows in
  tab:category_weights order; no overlap. Dots agree with 3.2 (e.g. ES Trade-Mid 0.59,
  Consumption-Down 0.49). OK.
- method_category_sets (6.5x5.2 in): per-column counts checked against 3.2 (ES Up 7,4,6,6,4,-,5,1,
  4,5,5; Mid median 1.5; EO Grid-Down 2, Geothermal 3/3/5); light-blue tiles = Energy Prices in 10
  upstream pairs, TRL in 33, TD in Batteries Up/Mid and Geothermal x3; three "pair not scored".
  Annotations about 8 pt; no clipping. OK.
- method_imputation (6.5x3.6 in): single-hue sequential blue on fixed 0-1 limits with legend,
  dashes for unobserved, pillar row labelled 0.42/0.24/0.27 and 0.44/0.44/0.43. OK.
- method_coverage (6.5x3.9 in): x from 0, pillar colours from paper_pillar_cols (blue ES / orange
  EO, which coincide with the Upstream/Midstream hues; theme-defined identity, panels are the
  stages, legend present), fill vs ring legend, "ES constant" and "no ES score" labels legible
  and clear of marks. Counts agree with 3.2. OK.
- No dual axes, no per-point labels beyond the pillar row of the heatmap, no chart junk; nothing
  in references/anti-patterns.md applies.

## 3.5 text fragment and table
- LaTeX: 30 \ref/\eqref targets all resolve (sections + own labels); both \citet keys in
  references.bib; 4 figure + 1 table environments balanced; braces balanced; $ even per paragraph;
  \ES/\EO/\1 only inside math; ASCII only; the five labels are defined once across sections/,
  results/text/ and tables/. Table uses booktabs + threeparttable + siunitx S columns (packages
  loaded in main.tex).
- Numbers: all 108 numeric tokens occur in method.json (vm3/numaudit.py); the substantive ones
  were recomputed in 3.2.
- Every config-level claim checked: missing_data.yml gives every theme a rule except
  lcoe_competitiveness; Comtrade critmin raw files 130/128/130 (2024) and 3/3/3 (2025),
  min_reporters 50, resolved 2026-10-01 17:57:02 UTC.

## 3.6 repairs (session 3)
- (major) Constant-component sentence in "Effective weights" listed only the world export
  concentration (Trade) and the global-demand and overcapacity terms of Technology Demand as the
  constant components behind 0.49/0.39/0.27. The script's own definition (values observed or
  imputed identical across the pair) also includes the addressable-market term, which is constant
  in 21 pairs inside a varying Technology Demand category and carries as much weight as global
  demand (0.055/0.058/0.048 by stage). Text now names it. New JSON key
  effective_weights.constant_components_in_varying_categories (pillar, stage, category,
  variable, n_pairs, mean weight over stage cells) documents the decomposition; it sums exactly
  to the reported component-level shares (EO 0.3683+0.0547+0.0547+0.0156 = 0.4933, etc.).
- (minor) "the burden ... is driven by Technology Demand" -> "is led by" (TD contributes
  0.16-0.19 of 0.43-0.44, the largest single category but well under half).
- (minor) "Geothermal midstream, which is excluded by the fixed rule" -> "which the project does
  not report as a sector (it also has no ES value)", matching 08_strategic subsec:si_sample.
- (minor, script comment) "Imputed components carry no year of their own" was inaccurate: 1,191
  positively weighted imputed EO components carry a selected year and are counted. Comment
  corrected; computation unchanged.
- Re-run after edits: exit 0, 20 s; method.json identical to the pre-session copy apart from the
  new key; table and all four PNGs byte-identical. Number audit and LaTeX checks re-run clean.

## 3.7 not repaired / for integration
- sections/03_data.tex includes figures/fig_data_coverage (fig:data_coverage, theme x stage source
  coverage before imputation); no script produces it and method_coverage is a different object
  (pair-level scored vs country-specific). Needs an owner.
- The fragment classifies sovereignty with the Correlates of War state-system list but
  references.bib has no COW entry, so it is uncited; add a key at integration if wanted.
- Label mapping: no placeholder in sections 05-07 (or 02-04) corresponds to these figures; all
  labels are new (fig:method_category_sets, fig:method_weights, fig:method_imputation,
  fig:method_coverage, tab:method_imputation).

## Verdict (session 3): approve with repairs (one major text inaccuracy repaired, three minor).
