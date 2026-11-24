# External review readiness: audit (before implementation)

Date: 2026-11-23. Branch `feat/external-review-readiness`, worktree `osi-worktrees/xreview`.
Base: origin/main `db02539b` + `feat/qc-review` (the QC / expert-review system, 10 commits, previously unpushed).

This audit was written before any product change. It records the current state, what other worktrees hold,
what to port, what to abandon and the order of work. The sprint scope is set by the brief "prepare the
existing AIB Explorer for external expert review": simplify, conclusions first, clearer terms, clean
evidence, fix credibility issues, make the repository review-ready. It is not a redesign.

## 1. Current architecture

| Layer | Where | Notes |
|---|---|---|
| R pipeline | `R/` (pure functions), `scripts/` (IO), `config/*.yml` | Builds processed tables, then the Explorer bundles (scripts/85, 87, 88, 91 posture, 84 AIB, 92 country briefs, 93 development partnerships, 43 agreements). |
| Explorer app | `dashboard/` (React 19 + Vite + TS + Tailwind v4) | Static site; data bundles in `dashboard/public/data/` (tracked). A web worker runs the legacy index engine. |
| Expert review | `dashboard/src/review` (route `/review`), `R/qc`, `scripts/qc`, `config/review`, `config/qc` | From `feat/qc-review`: neutral case briefs, blind-first judgment, reveal, verdict codes, data-issue reports, release identity. |
| Public site | separate repo `lrcarey222/aib-explorer` (GitHub Pages) | Built from a dashboard `dist` with `VITE_BASE=/aib-explorer/` and `scratch_artifact/assemble_pages.py`. |

### Navigation (unchanged by this sprint)
Home · Sectors (`/sector`, `/sector/:tech`, `/sector/:tech/:stage`, Advanced explorer at `/sector/explore`) ·
Countries (`/countries`, `/countries/:iso3`, memo `/countries/:iso3/memo`) · Allied Industrial Base (`/aib`,
`/aib/partnerships`, `/aib/roles`, `/aib/diversification`, `/aib/agreements`, `/compacts`) · About (`/about`,
`/methods`, `/data`, `/glossary`). Legacy index views (`/overview`, `/space`, `/strategic`, `/partners`,
`/country`) have no tab. `/review` has no tab.

### Unit of analysis and decision architecture
- Unit: country x technology (11) x supply-chain stage (6): Extraction, Refining, Components, Assembly,
  Deployment, Recovery (`dashboard/src/lib/stages.ts:32`; `docs/supply_chain_stages.md`).
- Decision chain: Need -> Prize -> Position -> Choice -> Policy alignment (`docs/strategic_position.md`).
- Six choices: Secure (high fence); Preserve, Catch Up, Partner, Accelerate (medium fence); Watch (low fence,
  action wording "Keep open"). Unmeasured steps are "not assessed", never Watch.
- AIB classes per supply-chain step: Build capacity, Coordinate, Manage specialization, Low intervention,
  Not enough evidence; stress test = major-supplier (China) disruption.

## 2. Deployment state

| What | State |
|---|---|
| origin/main | `db02539b` (2026-11-23): PR #367 deployment case + PR #354 agreements merged on top of PR #370 bundle stamps. |
| Published Pages | Pages commit `2ab38f4`, built from main `a5b22881` (2026-11-21). **Lags main by PRs #370, #367, #354.** |
| Local Pages repo | `osi-worktrees/pages/aib-explorer` holds ~690 uncommitted entries (a pre-assembled rebuild from the stamps branch), never pushed. Superseded by this branch. |
| Bundles on this branch | posture 2026-11-21T17:46 `17f0737e`; AIB 2026-11-21T14:34 `03560172`; country and sector 2026-10-31 `4bb2adc`; strategic 2026-10-30 `a54a9d3`. |
| Review release | `public/data/review/manifest.json` release `rel-9c8f1877b5c6` stamps posture 2026-11-17 / AIB 2026-11-17: **stale**; every submission from a posture or AIB page would carry a "page stamps differ" note. Must be rebuilt. |

## 3. Worktrees reviewed

| Worktree / branch | State | Verdict |
|---|---|---|
| `qcmain` / `feat/qc-review` | 10 commits on main a5b22881, unpushed | **Merged here** (clean). It is the structured external-review environment the brief asks to keep. |
| `stamps` / `fix/bundle-stamps` | merged to main as PR #370 | Already on main. One gap left: `shell/buildInfo.ts` and `review/context.ts` drop the AIB `git_commit`. Fix here. |
| `redesign` / `feat/aib-explorer-redesign` | 4 commits on 2d0ae52d, unpushed, + uncommitted Chart-of-the-Day copies | **Do not merge.** See section 5. |
| `pagesbuild` | detached at a5b22881 | Reference for "what is live" only. |
| `thememo` | uncommitted standalone battery memo | Not relevant to the Explorer. |
| `stages` / `feat/deploy-case` | merged to main (PR #367) | Already on main. |
| Others (`cib`, `fantasy`, `monthly`, `nipo`, `nodata`, `regs`, `tri`, `tradegrowth`, `yards`, `partnerfix`) | unrelated to the live Explorer UI | Not ported. |

## 4. Changes worth porting

From `feat/qc-review` (whole branch merged; it is self-contained under `src/review`, `R/qc`, `scripts/qc`):
neutral briefs, blind-first flow, reasoning reveal, verdict taxonomy, evidence source/vintage, data-issue
reporting, decision vs technical modes, campaign infrastructure. The main Explorer does not imitate it.

From `feat/aib-explorer-redesign`, reimplemented by hand (not cherry-picked), only if cheap:
- the golden regression snapshot idea (`dashboard/src/regression/golden.test.ts`) - regenerate on this
  branch's data, never copy the old snapshot;
- the share-label rule (`fmtSplitShare`: no decimals at 10% and above) - optional;
- the "Advanced explorer" title consistency fix;
- `dashboard_redesign/documentation_issues.md` as a checklist (scenario naming, Methods page index-first,
  memos say "OSI", every memo `verified:false`).

## 5. Redesign changes explicitly abandoned
- The four-card home ("Sector, Country, Partnership, Compact") and Home moved to `/start`.
- New nav (Home | Sector | Country | Partnership | Compact | Methods) and route trees `src/partnership`,
  `src/compact`, `src/sector/conclusion`, `src/conclusion`.
- Separate conclusion pages and split evidence/policy pages (`/countries/:iso3/evidence|policy`).
- The memo data swap from `feat/partnership-memos-compacts` (budget risk; separate memo work).
- `CountryConclusion.tsx` as a new page; its idea (answer first) is done here inside the existing section.

## 6. Credibility issues found (to fix or flag)

1. **China / Russia named as "best-placed trusted partners".** For non-aligned reporters,
   `R/strategic_position/supply_structure.R:519 sp_nonaligned_trust` trusts every non-coercive supplier and
   coercion is judged from the reporter's side, so CHN and RUS become "trusted". Live data: LKA lead partner
   CHN 10, RUS 4; KEN CHN 9, RUS 3; BGD CHN 15, RUS 3; 47 economies have a CHN and 40 a RUS recommendation.
   `partner_recs.R` receives the allied set (`members`) but never filters on it. The AIB division matrix does
   use the allied set, so /aib and the Country page disagree about who is "trusted". The legacy Friendshore
   cards also rank CHN first for LKA/KEN.
2. **Country briefs are stale against the live page.** Written 2026-11-04/05; posture rebuilt 2026-11-21.
   Eight of nine briefs' Figure 1 counts differ from the page (e.g. USA brief 8 Preserve / 8 Catch Up, page
   13 / 3), yet all show `stale:false`.
3. **Review release stale** (section 2).
4. **Three different "trusted / allied" sets:** coalition 59 (31 core + 28 partner; `config/coalition.yml`),
   AIB strategic "allied" 61 (+TWN, CHE; `config/aib_strategic_layer.yml`), AIB candidate suppliers global.
   The list is visible only deep in /aib/partnerships and the Country profile.
5. **Missing data:** 13 tech x stage cells have no country data; the tech page shows each as a card saying
   "No country-level primary measure for this stage"; formatters print "–" for zero and missing alike.
6. **Version identity:** the footer shows only the sector and strategic (legacy index) stamps; app commit
   is injected but never shown; the AIB commit is dropped.
7. **Docs:** README line 30 points to `docs/methodology.md`, which is the legacy ES/EO index method;
   README choice counts are stale; `dashboard/README.md` describes dials-first and a Netlify deploy;
   Methods page (`src/content/methods.md`) is mostly legacy index content after its first 30 lines.
8. Known model issues already logged by QC (`docs/QC_SYSTEM_DESIGN.md` 1.5): 432 cells at prize 0.59,
   entrepot leaks (SGP, BEL), implausible compact cores, HS 281290 at Refining. Flag, do not fix.

## 7. Implementation sequence

1. Merge origin/main + `feat/qc-review` (done; one comment-only conflict in `check-budget.mjs`).
2. Partner eligibility (client side, no data change): split posture partner lists into eligible trusted
   partners vs current suppliers outside the trusted network, using the shipped tiers; regression tests
   (LKA, KEN, BGD, USA). Document in `docs/PARTNER_ELIGIBILITY_AND_FRAMING.md`. R-side filter left as a
   documented follow-up because it needs a scripts/26 -> 91 rerun and golden re-approval.
3. Country page: "What should [COUNTRY] do?" block at the top of the existing Strategy section, built from
   `choiceGroups` + `fenceReason`; "Why this result?" disclosure generated from the shipped decision trace
   (why this choice, why not the nearest alternative); feasibility caveat for medium-fence choices;
   "Download decision brief" made prominent, with a notice when the brief predates the data.
4. Sector tech page: "Where are the real bottlenecks?" from the AIB nodes already loaded there; lead with the
   supplier-share chart; collapse stages without comparable country evidence.
5. AIB overview: "Where does coordination change the outcome?" short lists from the shipped nodes; a
   trusted-network control ("Trusted network: N economies · View list"); a "Compare two countries" control
   that opens the existing pair view (`/aib/partnerships?country=A&partner=B`).
6. Shell: Home light touch (Advanced explorer route, trim duplication), footer data-release stamp with app
   commit, AIB commit fix, Methods page legacy content demoted; jargon pass on the most visible labels.
7. Rebuild the review release data; run the full test suite, typecheck, build and budget.
8. README rewrite (concise, current), docs index with one canonical methodology, legacy banners.
9. Case audit (`docs/EXTERNAL_REVIEW_CASE_AUDIT.md`) and final report.

## 8. Risks
- Text-pinning tests: Home (exact h1, exactly 6 h3s), Country section ids, posture copy, AIB labels; the
  review cases quote "Coordination dividend", "Demand anchor", "Investment host". Rename display labels only
  where the review cases stay intelligible; keep the term in brackets.
- Size budgets (`dashboard/scripts/check-budget.mjs`): Home 400 kB, Country profile 900 kB, sector-meta
  150 kB, posture data 2.6 MB. New blocks must reuse data already loaded on the page.
- Anything that changes a classification is out of scope: no classification is changed to pass a test.
- The briefs cannot be regenerated without the stages processed data and an R/Edge run; a staleness notice
  is the honest interim fix.
- The Pages republish is a separate, outward-facing step that needs the user's go-ahead.
