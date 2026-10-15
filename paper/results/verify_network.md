# Verification log: theme "network"

Verifier started 2026-10-07. Scope: scripts/paper/fig_network.R, fig_network_robustness.R,
fig_network_frontier.R; figures network_*; JSON network*.json; text paper/results/text/network.tex;
tables paper/tables/network_*.tex.

## Step 0: inventory
- Text fragment exists (387 lines). Tables network_producers.tex, network_frontier.tex exist.
- No previous verify_network.md.

## Step 1: re-runs (clean Rscript sessions, worktree root)
- `fig_network.R`: ran without error in 31 s; network.json byte-identical to the pre-run copy,
  tables/network_producers.tex identical. Figures regenerated.
- `fig_network_robustness.R` and `fig_network_frontier.R`: the working copy of
  R/indices/allied_network_design.R carries another task's uncommitted edits (diversity_lambda,
  Upstream/Midstream-only design; md5 204d9a5c...). Both scripts were re-run with
  OPSI_NETWORK_DESIGN_SRC pointing at `git show HEAD:R/indices/allied_network_design.R`
  (md5 04fb041b..., identical to the md5 recorded in network_robustness.json). Running in background.

## Step 2: independent recomputation (scratchpad vn/indep.R, base R, from the CSVs)
Matches: 33 stages all MILP; 23 nodes/stage; no imputed policy; 105 roles; producers 3/4/5 = 28/4/1;
26 stages exactly (0.4,0.4,0.2); HHI_D 0.3295-0.360 median 0.360; roles USA 10, DEU/FRA/GBR/IND 7,
AUS/CAN/ITA/JPN/KOR/MEX 6, ARE/ARG/BRA 1; 17 at cap; caps sum 122; Spearman roles~cap 0.8228,
roles~GDP 0.7745; non-high-income shares 0.3137/0.2169/0.2386; 754 consumers with 2 suppliers + 1
with 3; epsilon binds in 33/33; self share mean 0.454%, max 5.03% (Batteries Upstream), 10 stages;
flow-weighted W (off-diagonal) median 0.873, range 0.359-0.943; battery shares (AUS/CHL .40, ZAF .131,
ARG .069; FRA/USA .40, KOR .093, JPN .057, GBR .050; DEU/USA .40, ITA .20); 14 basket stages
(5 Up, 9 Mid); HHI_W median 0.0951 (0.0461-0.2676); HHI_C median 0.1513 (0.0962-0.3268); HD > HW
and HD > HC in 14/14; max designed share > world leader share in 13/14; world leader share median
0.228; leader outside coalition 8 (CHN 7, KAZ 1); any top-3 outside 11; coalition share of world
median 0.5265 (0.377-0.901); share outside top-3 coalition exporters median 0.4537 (0.20-1.00),
coincide 0, none in top-3 in Oil and Solar Midstream; equivalent producers 2.78/10.51/6.61; up to
130 reporters, 21 coalition reporters; build top-10 identical (all selected, 9 at 0.40).

MISMATCH (major): `n_shares_at_005 = 3` in network.json. Only TWO shares equal the 0.05 floor
exactly (GBR Batteries Midstream, BRA Electric Vehicles Downstream); the third counted share is
JPN Oil Midstream = 0.050453, counted because fig_network.R rounded shares to three decimals before
comparing with the bounds. Correct split: 63 at 0.40, 26 at 0.20, 2 at 0.05, 14 interior (all in
the 7 stages whose pattern is not (0.40,0.40,0.20)). Repaired in fig_network.R (unrounded shares)
and in the text ("three equal the floor" -> "two"; "The shares are corner solutions" qualified).

## Step 3: text repairs so far
- network.tex "Who produces what": "three equal the floor" -> "two equal the floor ... (two more, in
  Geothermal Upstream and Oil Midstream, lie within 0.001 of it)"; "The shares are corner solutions"
  -> "Most shares are corner solutions"; added the 14 interior shares / 7 other-pattern stages.
  New JSON keys in network.json: producers.n_shares_at_005 = 2, n_shares_interior = 14,
  stages_other_pattern (7), n_stages_with_interior_share = 7, shares_near_floor_not_at_floor (MAR
  0.050742, JPN 0.050453). These reconcile the text with Table network_producers, which prints four
  ".05" cells at two decimals.
- fig:network_map caption: "width proportional to the flow" -> "width increasing in the flow"
  (scale_linewidth_continuous maps the flow range linearly onto 0.12-1.1 mm, not proportionally).

## Step 4: number-by-number check of network.tex against JSON (pre-rerun robustness/frontier JSON)
All other quoted numbers checked against network.json / network_robustness.json /
network_frontier.json and, where marked in Step 2, against my own recomputation: coverage and input
checks (759/688/759/297/16,698/15,136), partial-score stages (5; KEN/CAN), KEN zero demand in 4
stages (smallest GDP); role counts by stage (AUS/CAN 4 upstream; CHL/IDN/MEX/NOR 3; DEU/FRA/ITA/DNK 4
midstream; IND/JPN/KOR/GBR/USA 3; USA 6 of 11 downstream); Coal 5 distinct members, IDN in all
three; Batteries 12 roles / 11 members; EO trade weight 3 of 30 (config/weights.yml); edge coverage
15,136 both (90.6%) / 1,562 FS-only (9.4%) / 0 / 0; GH partial stages 4.3% = 22/506; flow-weighted W
0.36-0.40 in the 3 FS-only stages vs median 0.875 -> "0.88" (0.658-0.943) in the other 30;
unweighted mean 0.583; portfolio (caps 2..10, sum 122, 7 passes, 89 exclusions in 27 stages, USA 18 /
IDN 12 / DEU 10, 8 members over cap uncapped, USA 24/DEU 14/FRA 12/IDN 12, ARG/BRA/MAR/ZAF none,
JPN/MEX 1->6, KOR 3->6, ITA/AUS 4->6, GBR 5->7, DNK 2->4; loss 2.385% total, 13.03% max);
build top-10 priorities (0.748, 0.746, 0.715, 0.687, 0.677, 0.676, 0.657, 0.656, 0.636, 0.632;
9 at 0.40); robustness (means/mins/identical for six variants; role retention 70.5% vs 81-96%;
midstream no-caps mean 0.491 with 1 identical; stable core 4/10/2; solver no-caps 18 identical,
mean 0.877, min 0.60, gap median 1.95%, max 3.11%; greedy HHI 0.333-0.341, min suppliers 3);
frontier (-2.24% [-3.94,-0.47], -3.75%, +1.62%, m_S=3 -0.28%, eps 0.20 -0.12% with 33 identical,
relaxations +0.115% / +0.058% < 0.12%; median HHI 0.345 -> 0.280; 7x33 direction checks;
require_all 30 solved, 3 skipped, 21 identical, mean 0.868, min 0.40, 9 changed stages none in GH).
All match.

## Step 5: unit-test block of fig_network.R (feeds Appendix C tbd, tab:alg_tests)
MAJOR (repaired): the block ran the four committed allied-network test files against the WORKING
COPY of R/indices/allied_network_design.R (md5 204d9a5c..., another task's uncommitted
diversity_lambda edits), not against the committed code that produced the stored outputs, and
recorded the working-copy md5. fig_network.R now runs the tests in an environment whose `source`
redirects that one path to OPSI_NETWORK_DESIGN_SRC when set (n_files_sourcing_design_code = 4
confirms the redirect); design_code / design_code_md5 record the code used. Result against the
committed code (md5 04fb041b...): 4 files, 21 tests, 103 expectations passed, 0 failed, 0 errors,
0 skipped, 2 warnings ("Producer forbiddance reduced feasible producers for stage; using eligible
maximum.") - the same counts as against the working copy. Run instructions added to the header.

## Step 6: figures (PNG twins inspected with the Read tool; rules + anti-patterns.md)
(dataviz/SKILL.md is absent from the bundled-skills directory; references/anti-patterns.md was read.)
- network_specialization (6.5 x 4.6 in): legible (7.5-8.5 pt), sequential single-hue blue on the
  0-0.40 share range (bounded at the cap by construction, so the 0-0.4 scale is honest), stage bands in
  the three stage colours, colourbar legend, rows ordered by roles then summed share. Caption matches.
- network_map (6.5 x 3.0 in): three battery panels, stage-coloured producers, grey consumers, legends
  for role, node size and flow width; no clipped labels; share labels readable on white backing.
  Caption fixed (flow width not proportional). Self-share 5.0% / 0 / 0 matches JSON.
- network_concentration (6.5 x 3.6 in): one x-axis per panel, both from 0; stage colours for the
  design; open/grey/coloured series with legend; reference lines 0.36 / 0.40; world-leader codes bold
  when outside the coalition. Caption matches.
- network_portfolio (4.65 x 4.4 in, included at 0.75\textwidth): legend present, GDP order,
  arrows only for changes > 1. Caption matches.
- network_robustness (6.5 x 5.6 in): 2 x 3 panels on a common 0-1 Jaccard axis, stage colours,
  legend, strip labels with mean/min/identical counts that match the JSON; the dashed line is a mean
  reference line, not a grid line. Caption matches.
No dual axes, no > 8 categorical colours, no number-on-every-point, no titles inside plots.

## Step 7: fig_network_frontier.R re-run (committed design code)
Ran without error (EXIT 0; 9 runs, all 33 stages MILP in every frontier run). tables/network_frontier.tex
byte-identical; network_frontier.json identical except the runtime_sec fields. The reference run's
summed objective (39.766) equals the uncapped run of fig_network_robustness.R (cross-script check).

## Step 8: fig_network_robustness.R re-run (committed design code)
Ran without error (EXIT 0, ~29 min with the frontier running concurrently). Reproduction check passed
(max |share diff| 4.44e-16, 0 unmatched rows; capped counts equal the stored ones). Every
deterministic number in network_robustness.json is identical to the pre-run copy (variants, Jaccard,
stable core, role retention, solver comparison, portfolio, edges); only wall-clock/CPU timings and
the design_code file name changed. Figures network_robustness and network_portfolio regenerated.
- Runtime numbers in the text were stale after the re-run (297 s / 273 s CPU / 3.3 s median, 2.2-4.4 s).
  Updated to the regenerated JSON: 309.68 s -> "310", CPU 290.13 -> "290", first-pass median 3.21 s
  (2.07-4.55), 97 solves (64 re-solves), slowest 4.97 s. The text already says these are orders of
  magnitude only; they will change on every re-run.

## Step 9: wording (overclaim) repairs
- "the share constraints, not the indices, determine how much each produces" -> "in most stages the
  share constraints ..." (14 of 105 shares are interior).
- LaTeX: 5 figure environments balanced; braces balanced; all underscores in math or labels; every
  \ref/\eqref target exists in sections/10_network.tex except fig:network_robustness (defined here),
  tab:network_producers and tab:network_frontier (defined in the \input tables); bib keys
  makhorin2020glpk, schumacher2023ompr, uncomtrade, worldbank2026wdi all in references.bib.
  No LaTeX engine is installed on this machine, so the fragment was not compiled.

## Verdict
approve_with_repairs. Final state: all three scripts re-run cleanly from the worktree root
(robustness and frontier with OPSI_NETWORK_DESIGN_SRC = committed design code); every quoted number
traces to network.json / network_robustness.json / network_frontier.json.
