# Verification log: theme "bilateral"

Verifier session started 2026-10-06. Files under review:
- scripts/paper/fig_bilateral.R
- paper/figures/bilateral_{fs_heatmap,fs_components,psi_rank,fs_po}.{pdf,png}
- paper/results/bilateral.json
- paper/results/text/bilateral.tex

## Step 0: inventory
All four figures, JSON and text fragment exist (mtimes 2026-10-06 22:14-22:20). No tables in paper/tables/.

## Step 2: independent recomputation (verifier's own code, scratchpad/vb/indep_check.R)
Written from scratch on data/processed/*.rds (no theme helpers, JSON not read). All match the JSON/text:
| Quantity | Independent | Text/JSON |
|---|---|---|
| FS long rows / duplicate keys | 9,191,202 / 0 | same |
| FS cells, reporters, partners, pairs, tech-stage cells | 1,531,867; 219; 219; 47,742; 33 | same |
| PO cells / tech-stage cells | 1,341,448 / 30 | same |
| DP partners x cells | 132 x 33 = 4,356 | same |
| Excluded cells (ES/EO index_cells not ok or absent) | EG Down, Geo Mid, Geo Up, Nuc Up | same |
| FS cells with observed trade (by a different method: values differing from the cell's modal imputed value) | 19,379 (1.27%); coalition pairs 30.06%; 22 reporters | 19,379; 30.1% |
| Need missing / investment present / DIP reporters / DIP pairs | 12.43% / 21.70% / 68 / 10,172 | same |
| Batteries Mid supplier means (USA .924, DEU .915, FRA .893 ... KEN .493, ARG .452), importer IND .863 .. MAR .599, CHN .967 | identical | same |
| FS_ij vs FS_ji Spearman, BM / SM (253 pairs); mean abs diff | 0.228 / -0.185; 0.162 / 0.279 | same |
| BM observed vs imputed mean FS; imputed 46 of which 22 in ARE row | 0.763 / 0.612; 46 / 22 | same |
| Retained-cell top-10 suppliers USA (CAN .980 ... MYS .942) and JPN (NLD .964 ... PRT .907); 212 ranked; as-built lists incl. TCD/CMR | identical | same |
| FS(c<-p) vs PO(p->c) Spearman: all 1,341,448 cells; coalition 15,136; dyad means 506; share abs diff > .25; retained | 0.885; 0.810; 0.719; 0.12; 0.814 | same |
| DP best-five top 10 (GEO, MNG, MUS 1.000 ... MRT .872) | identical | same |
| PSI: 204 covered; top 10 (CHN .812 ... SAU .760); coalition ranks and median 29; MYS 12, THA 18, TUR 22, SGP 32 | identical | same |
| SD C_FS/C_PO/C_DP; Spearman C_FS-C_PO, C_FS-C_DP; CHN C_PO rank | .114/.024/.190; -0.481; 0.106; 201 | same |
| Mean PSI high-income (77) vs other (127) | 0.706 / 0.666 | same |
| Coalition PSI_ij top 10 (CAN-USA .989 ... DEU-FRA .975); Spearman PSI_j vs coalition-avg PSI_ij (204) | identical; 0.952 | same |
Verdict: PASS (no discrepancies).

## Step 1: clean re-run
`Rscript --vanilla scripts/paper/fig_bilateral.R` from the worktree root: exit 0, 219 s, only a cowplot
build-version warning. All four PDFs and PNGs regenerated; paper/results/bilateral.json byte-identical
to the pre-run copy. Internal reproduction checks pass (stored FS rebuilt: mean abs dev 6.5e-8, max
0.0058 on 3,687 rows; PO, trade components, T^FS, PSI_j, C_DP, PSI_ij reproduced to <= 6e-16).

## Step 3: figures (PNG inspection)
- bilateral_fs_heatmap: legible at 6.5in (7pt ticks), sequential blue 0-1 with legend, axes ordered by
  supplier mean, ARE* row, white dots visible. ISSUE (minor): the white-dot overlay had no legend key
  (caption only). Repaired: shape key "Import trade component imputed" added beside the colour bar.
- bilateral_fs_components: stacked contributions with 2px white gaps, penalty as negative gray
  segment, legend present, ordered by rank, no clipping. OK.
- bilateral_psi_rank: dot plot, legend, sorted by PSI; axis starts at 0.48 (justified in caption:
  best-five means sit in the upper part of [0,1]; dot plot needs no zero baseline). OK.
  Cosmetic: figure prints "Turkiye"/"South Korea" (psi_tbl names) while the text writes
  T\"urkiye/Korea - left as is.
- bilateral_fs_po: 0-1 on both axes, coord_equal, 45-degree line, 3+3 labels per panel as captioned.
  ISSUE (minor/major for reading): the trade-status legend sat centred under BOTH panels, while panel
  (b)'s dark-gray points are dyad means without trade status - a reader could read them as
  "imputed" (light gray). Repaired: legend moved inside panel (a)'s empty lower-right corner.

## Step 4: text fragment - findings
- All refs resolve (eq:, subsec:, sec:, tab:ghg_penalty, eq:net_edge in 10_network.tex). No \cite.
  Braces/environments/dollars balanced; no tbd. Labels reuse placeholders in 09_bilateral.tex.
- Every number checked against the JSON (all sections); all match.
- ISSUE (major, factual): "the composite reduces to a monotone function of the partner's EO rank
  in a thinly populated cell" - false: the excluded cells have 212 partners/212 reporters (44,732
  dyads), like the other cells; EO index_cells has n_countries 212, n_unscored 0. What is true:
  Chad holds the top EO rank (1.000) in all three cells without an ES score. Repaired (new JSON key
  capability_in_excluded_cells).
- ISSUE (minor, wording): "cells that the ES diagnostics report as unscored" - ES diagnostics'
  unscored_groups is empty; the three cells are simply absent from ES index_cells. Reworded to
  "cells that the ES pillar leaves unscored ... or scores as constant".
- ISSUE (minor): "climate penalty ... about a twentieth of the composite" - true for the USA (5.5%)
  but not Japan (7.4%). Repaired with new JSON keys top10_penalty_share_of_composite.
- ISSUE (major, overclaim): coalition-view paragraph asserted "The trade components drive the
  difference" without any test; coalition reporters also differ in DIP filing (16 of 23), and the
  hubs that gain (CHE, HKG) are investment hubs. Repaired: softened to what is shown, plus new keys
  spearman of C_FS / C_PO with their coalition averages.
- ISSUE (minor): caption of fig:fs_components said the rebuilt FS reproduces the stored one "up to
  numerical precision" while quoting a max deviation of 0.006. Verified the cause (scratchpad
  tiecheck.R): deviating rows lie in blocks of exactly tied rebuilt composites (median block ~950
  rows) that the pipeline's floating-point rounding splits differently. Caption made precise; new
  check keys record it.
- ISSUE (minor): heatmap caption gave only two reasons for an imputed dot; added the ARE row case.

---

# Verification pass 2 (2026-10-07, fresh verifier session, "Try again")

State found: script mtime 22:39 is later than the JSON/figures (22:37), text fragment 22:39. The
compute agent's report says it extended the script (retained-cell PSI_ij, key non-member ranks,
DP top-10 robustness) and the fragment (bracketed retained-cell ranks). Pass-1 repairs found in the
current script: shape key for white dots in the heatmap (yes), trade-status legend under panel (a)
only in fs_po (yes, now below panel (a) instead of inside it). Backups of the pre-run JSON/tex/script
are in scratchpad/vb2/.

## Step 1 (pass 2): clean re-run
`Rscript --vanilla scripts/paper/fig_bilateral.R` from the worktree root (current script, 22:39 version,
never run before this pass): exit 0, "bilateral: done", 1,785 s wall (machine at 77-100% CPU from
other agents; ~220-320 s when idle). Only warning: cowplot built under R 4.4.2. All four PNGs are
byte-identical to the 22:37 versions (md5). JSON identical to the 22:37 file except three keys the
22:39 edit added and the text already quotes: opportunity_coverage.share_fewer_cells_than_fs = 0.124,
psi.psi_spread_coalition_members_ranked_13_to_19 = 0.002, psi.dp_term_change_in_mean_psi_other_wb_income
= -0.014. Before this run those three text numbers had no JSON backing; now they do. PASS.

## Step 2 (pass 2): independent recomputation (scratchpad/vb2/indep2.R, own code, JSON not read)
Built from partner_*_tbl.rds, psi_*.rds, index_outputs.rds diagnostics, wdi_country_info.csv.
All match the text/JSON (first run of indep2.R had an R precedence bug of mine, `0.4 * !is.na(a) + ...`;
fixed and re-run, results below are from the corrected run, scratchpad/vb2/indep2b.log):
| Quantity | Independent | Text/JSON |
|---|---|---|
| FS long rows / dup keys; FS cells, reporters, partners, pairs, tech-stage cells | 9,191,202 / 0; 1,531,867; 219; 219; 47,742; 33 | same |
| PO cells / cells; DP partners x cells | 1,341,448 / 30; 132 x 33 = 4,356 | same |
| Excluded cells from ES/EO index_cells (ES: 43 ok + 1 constant; EO: 39 ok) | EG Down, Geo Mid, Geo Up, Nuc Up | same |
| As-built PSI_j rebuilt from cell tables vs psi_tbl; PSI_ij vs psi_dyad | max dev 5.6e-16; 0 (47,742) | same as script checks |
| Covered partners (>=27 cells FS and PO) | 204 | 204 |
| Retained-cell PSI: Spearman vs as built; median / max rank move | 0.9954; 2; 30 | 0.995; 2; 30 |
| Retained top 10 | CHN VNM IDN IND FRA DEU PRT ESP AUT SAU | same (VNM/IDN, ESP/AUT swap) |
| Coalition ranks built [retained]: median; ARG; BRA; members in top 20; Spearman | 29 [28]; 123 [119]; 137 [135]; 10 [10]; 0.9951 | same |
| Key non-members built [retained]: CHN, MYS, THA, TUR, SGP; TWN rank-if-added, FS/PO cells | 1[1], 12[12], 18[19], 22[20], 32[31]; 45, 27/23 | same |
| Coalition members ranked 13-19 PSI spread | 0.0020 | 0.002 |
| USA/JPN supplier lists, built and retained; TCD 10->43 (USA), 7->22 (JPN); CMR 9->16; MYS 11->10; IRN 12->9; PRT 11->10; Spearman 0.9921 / 0.9952 | identical | same |
| Coalition PSI_ij top 10 (CAN-USA .989 ... DEU-FRA .975); retained Spearman 0.99964, overlap 10/10 | identical | "1.000 to three decimals" |
| DP top 10 built = retained (GEO, MNG, MUS 1.000 ... MRT .872); Spearman 0.9995 (r3 0.999) | identical | same |
| FS(c<-p) vs PO(p->c): all 1,341,448 0.885; coalition 15,136 0.810; retained 0.814; share abs diff > .25 0.12; dyad means 506 0.719; BM 0.677; extreme dyads ARG/BRA->MEX, CAN/ARE->MAR/JPN | identical | same |
| Mean PSI high income (77) / other (127) | 0.7063 / 0.6662 | 0.706 / 0.666 |
| CHN C_FS 0.949 rank 1, C_PO 0.622 rank 201; SDs .114/.024/.190; Spearman C_FS-C_PO -0.481; C_PO vs mean eta 0.931; CHN lowest eta 0.162 | identical | same |
| ARG/BRA/MAR without DP term: ranks 83 / 110 / 106 | identical | same |
Extra checks (scratchpad/vb2/misc.R): Taiwan has 0 rows as partner or reporter in the Comtrade extract
(text: "Taiwan's trade is invisible to the extract" - correct); PO cells AFG 22, BTN 23, XKX 23, MDV 21,
NPL 23, TWN 23 (< 27, as the filter paragraph says); g_t Geothermal 0.967, Electric Grid 0.983.
Verdict: PASS.

## Step 3 (pass 2): figures
Inspected all four PNGs (unchanged by the re-run).
- bilateral_fs_heatmap: 7 pt ticks legible at 6.5 in, sequential blue 0-1 with colour bar and shape
  key, empty diagonal, identical order on both axes, ARE* row. White dots: ARE row plus the KEN, ARG
  and MAR columns in (a) - consistent with "Emirates' row and the columns of the weakest suppliers".
  OK. Cosmetic: subscript of the colour-bar title FS_ijts is small.
- bilateral_fs_components: OK (legend, 2 px white gaps, penalty left of zero, ordered by rank,
  names + T^FS at 2 dp as captioned).
- bilateral_psi_rank: OK (legend, sorted, labels "(as built; retained)"; axis from 0.48 justified in
  caption for a dot plot). Cosmetic: "Turkiye"/"South Korea" (psi_tbl names) vs text "T\"urkiye".
- bilateral_fs_po: OK (0-1 both axes, coord_equal, 45-degree line, 3+3 labels per panel, legend
  under panel (a) only, matching caption "key below the panel").
No dual axes, no more than 5 categorical colours, no number-on-every-point. PASS.

## Step 4 (pass 2): text fragment
Structure: environments balanced, braces balanced, 306 `$` (even), labels tab:bilateral_coverage,
fig:fs_heatmap, fig:fs_components, fig:psi_rank, fig:fs_po_scatter (all match the placeholders in
09_bilateral.tex lines 818-944); no \cite; every macro used (\ES \EO \FS \PO \DP \PSI \Scurve \dyad
\mean \Coalition) is defined in main.tex; every \ref/\eqref target exists (09_bilateral.tex, and
sec:network / eq:net_edge in 10_network.tex; eq:net_edge does average FS_cp and PO_pc with 0.5/0.5).
Numbers: scratchpad/vb2/numcheck2.py matched every number in the prose to the JSON; the 3 that had
no backing in the 22:37 JSON (12 per cent, 0.002, 0.014) are backed after the re-run. Each claim
was also read against its JSON block (fs_components, psi, development, fs_vs_po, robustness,
heatmap, outbound): all values, ranks and orderings agree.

Issues found and repaired in paper/results/text/bilateral.tex:
- MAJOR (wrong mechanism): "The difference in level between the categories is itself a by-product
  of construction: C^FS and C^PO average a partner's summaries over all of its reporters, while
  C^DP is a single summary." Averaging over reporters changes spread, not level (mean C_j = mean
  T_ij = 0.691 for FS). Tested (scratchpad/vb2/levelcheck.R): all pillars have within-cell mean
  0.50; the best-five uplift over a unit's own cross-cell mean is 0.192 (FS dyads), 0.182 (PO
  dyads) but 0.114 (DP partners), whose within-partner cross-cell SD is only 0.089. Repaired:
  sentence rewritten on that mechanism; script now writes psi.best5_uplift_by_pillar (and the DP
  components that are constant within a partner) to the JSON.
- MINOR: coverage paragraph attributed the coalition pairs' higher LEVEL partly to the mechanical
  spread of trade-file reporters' composites; spread widens the distribution, it does not raise
  it. Reworded: level largely economic, wider spread partly mechanical.
- MINOR: "As for every cross-country ranking in this paper, ... left out" contradicts Figure
  fig:psi_rank, which ranks PSI as built (retained-cell ranks in brackets). Reworded to "Following
  the rule this paper applies to cross-country rankings".
- MINOR: "composite ... reduces to a monotone function of the partner's EO rank" ignored the climate
  penalty (g_t = 0.983 grid, 0.967 geothermal, so the penalty is <= 0.007 but not zero). Qualified.
- MINOR: "They are North American and European neighbours and India's Gulf energy suppliers" -
  India-USA and Korea-USA are neither. Now "Most are ...".
- MINOR: "the reporter's need affecting only which cells the summaries select" - with weight
  renormalization (investment missing for some partners) need also shifts within-cell ordering.
  Now "mainly affects".
- MINOR: "Averaged over these [197] reporters, C^PO_j is ..." - C^PO averages over all 219. Reworded.
- MINOR: "gains ... for trade and financial hubs: Switzerland, Hong Kong, Oman, Panama, Bahrain"
  - Oman is not obviously either. Now "mostly for".
- MINOR (softened): "These positions record where holding and financing entities are located" ->
  "partly record".

Script change (scripts/paper/fig_bilateral.R): new block after variance_share_no_dp writing
psi.best5_uplift_by_pillar = {FS_dyads, PO_dyads, DP_partners: n_units, mean_cell_value,
mean_best5, mean_best5_minus_unit_mean, mean_within_unit_sd_across_cells;
dp_components_constant_within_partner}. Re-run (clean `Rscript --vanilla`, worktree root): exit 0,
233 s; JSON identical to run 1 except the new block (FS .192, PO .182, DP .114 uplift; DP within-
partner SD .089; constant DP components d1, d2, d3, d8) - these equal my independent values from
levelcheck.R. All four PNGs byte-identical again. Post-edit checks of the fragment: every number
matched to the JSON (only "9.19/1.53/1.34 million" are rounded forms of JSON counts), environments
and braces balanced, 312 `$`, 5 labels, no \cite.

## Remaining (not repaired, for the integrator / user)
- fig:psi_rank ranks the pipeline's PSI as built (all 33 cells) and gives the retained-cell rank in
  each label and in brackets in the text. The task rule asks to exclude constant/unscored cells from
  cross-country rankings; the two rankings have Spearman 0.995 and the same top ten, and the text
  says so, but strictly the primary ranking could be the retained-cell one. Left as a judgment call.
- Integration: the fragment replaces the whole placeholder Results subsection of 09_bilateral.tex
  (lines ~818-944, after \subsection{Results}\label{subsec:bilateral_results}); labels map 1:1
  (tab:bilateral_coverage; fig:fs_heatmap -> bilateral_fs_heatmap; fig:fs_components ->
  bilateral_fs_components; fig:psi_rank -> bilateral_psi_rank; fig:fs_po_scatter -> bilateral_fs_po).
  fig:fs_po_scatter now shows the edge-level FS(c<-p) vs PO(p->c) comparison, not the placeholder's
  partner-level C^FS vs C^PO scatter; fig:psi_rank is a dot plot.
- Cosmetic: psi_rank prints psi_tbl names ("Turkiye", "South Korea") while the text writes
  T\"urkiye / Korea; the heatmap colour-bar title subscript is small.

## Verdict (pass 2): approve_with_repairs
