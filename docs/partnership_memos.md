# Partnership Memos

A Partnership Memo answers one decision question for a pair of countries:

> Given the capabilities, needs, vulnerabilities and policy priorities of two countries, where would deeper
> industrial integration create the most strategic value; through what trade, investment, finance, technology or
> development-cooperation mechanisms; and where is intervention unnecessary or counterproductive?

It is not a relationship summary and not an ally ranking. The chain is:
**A capability/need + B capability/need → dyadic relationship → supply-chain problem → treatment → instrument →
transaction → milestone.**

Pilot: United States–Japan (`output/partnerships/USA_JPN/`).

## Where it sits in the architecture

Branch `feat/partnership-memos` (worktree `osi-worktrees/pmemo`), cut from `feat/country-memo-aib` (country briefs
and development partnerships), which is `main` (the six-stage canonical framework, 8723f885) plus the briefs.

| Layer | What the memo reads | Produced by |
|---|---|---|
| Six-stage strategic position (canonical) | per country x technology x stage: `cp_contender_pct`, `cp_high`, `supply_chain_need`, `deployment_need`, `strategic_industrial_value`, `strategic_choice`, the NIPO / IEA PAMS `policy_*_priority` and `policy_momentum`, `stage_evidence_quality_*`, `evidence_specificity_cp`, world supply context (`coercive_world_share`, `trusted_world_share`), the engine's `recommended_partner_*` | `scripts/26` (`strategic_position_tbl.rds`) |
| Allied Industrial Base strategic layer | 76 electro-industrial nodes: world export / production share, RCA, share of allied demand and net imports, host rank, anchor / specialist flags (`aib_compact_division_of_labour.csv`); node class BUILD / COORDINATE / LOW INTERVENTION (`aib_node_classification.csv`); compact, step and workstream (`aib_strategic_spine.csv`); compact and country roles | `scripts/31-33` |
| Bilateral trade | importer x exporter x technology x stage imports, 2024 | `scripts/15` (`import_partner_cells_tbl.rds`) |
| Third countries | donor x recipient x technology x step pairs | `scripts/93` (`development_partnerships.rds`) |
| External evidence | agreements, institutions, projects, financing, disputes | curated, `data/reference/partnership_evidence/` |

Nothing upstream is recomputed. No single "Strategic Index" is rebuilt: Competitive Position, need, prize,
policy priority, momentum and evidence quality stay separate fields. The six stages are the unit; the three-stage
roll-up is carried as `rollup` only (`supply_chain_stage_rollup()`).

## Two grains

- **Cell**: technology x six-stage (11 technologies x 6 stages = 66 per country). Every engine field is available.
- **Node**: an AIB electro-industrial node (rare-earth magnets, GOES, separators, power semiconductors, ...). This is
  the grain of the Strategic Sectoral Compacts and the one the memo's compacts use. Position is the node's own
  trade/production share. **Need is inherited**: the engine scores need per cell only, so a node takes the highest
  supply-chain need among its cells (`score_cells`), and its evidence quality is capped at `proxy_cap` (0.67, medium).

Code: `R/partnerships/` (pure), `scripts/34_build_partnership_memo.R` (IO), `config/partnership_memos.yml`,
`config/partnership_instruments.csv`, `tests/testthat/test-partnership-memos.R`.

## Profiles (`country_profiles.csv`)

Per country and unit:

- **Position band.** Cell: strong = `cp_high` (top quarter of the node's contenders and above the capability floor);
  moderate = a contender (>= 0.5% of world supply); weak = not a contender; at Deployment the market-deployment
  percentile (>= 0.75 strong, >= 0.5 moderate). Node: strong = AIB anchor or specialist, >= 10% of world supply, or the
  largest trusted supplier once China is removed (with >= 2%); moderate = >= 2% (AIB presence) or a capable host;
  weak = otherwise, including **unlisted** (under 1% of world supply and allied demand).
- **Need band.** Cell: high when `supply_chain_need >= 0.67` (Deployment: `deployment_need >= 0.60`), the engine's
  thresholds. Node: high when the inherited need is high **and** the country is a material buyer of the node
  (>= 2% of allied net imports, or >= 5% of allied demand with positive net imports); **demand** when it is a material
  buyer without a high security need; low otherwise.
- `competitive_position` (0-1): cell = contender percentile; node = world share / 10% (the anchor line), capped at 1.

## Decision rules (`dyad_complementarity.csv`)

### Unit context

- `chokepoint`: a coercive supplier holds >= 50% of world supply (cell: the engine's `coercive_world_share`; node: the
  summed position of the configured coercive suppliers). `severe_chokepoint` >= 70%.
- `allied_short`: node AIB class BUILD (allied supply short of demand under the China shock); cell trusted world share
  below 30% (the engine's `partner_trusted_capacity` line).
- `capable supplier`: strong; or moderate **and** named (cell: the engine lists it as the other's recommended trusted
  partner; node: largest trusted supplier or ranked host, with >= 2% of world supply).

### Relationship states, first match wins

1. **not_assessed**: neither need nor position measured (or the engine did not assess the cell for either country).
2. **deployment cells**: Deployment is a market, not a supplied product. High deployment need in either country is
   `deployment_demand` (demand context for the compacts upstream); otherwise low strategic case. Two large deployers are
   never "competitors".
3. **direct_competition**: both strong. Treatment MANAGE SPECIALIZATION where more trusted capacity is needed
   (chokepoint or allied short), else LOW INTERVENTION (managed competition). **Two high Competitive Positions never make
   complementarity** (`bilateral_complementarity = 0`; tested).
4. **shared_vulnerability**: both have high need, neither is a capable supplier, and a chokepoint exists (or both import
   >= 40% from coercive suppliers).
5. **capacity_constrained_trusted_supply**: a capable supplier on one side, a buyer with high need (or material demand
   where a chokepoint or allied shortage exists) on the other, and allied capacity is short.
6. **a_supplies_b_needs / b_supplies_a_needs**: as 5 without the allied shortage.
7. **complementary_specialization**: one country is strong here and the other is strong at another step of the same
   chain (the compact for nodes, the technology's producing stages for cells), and the chain is under stress.
8. **low_strategic_case**: everything else, with reasons (`no_high_need`, `adequate_trusted_supply`, `large_yard`,
   `neither_capable`, `need_without_partner_capability`, `supplier_without_buyer_need`).

### Treatment, constraint type, instrument

| state | treatment |
|---|---|
| capacity-constrained, shared vulnerability | BUILD |
| supplier/needer, complementary specialisation | COORDINATE |
| direct competition | MANAGE SPECIALIZATION (more capacity needed) or LOW INTERVENTION |
| low strategic case | LOW INTERVENTION |
| deployment demand | DEMAND CONTEXT |

Constraint type (CTPI), first match: no material intervention (low case, competition without a capacity need, or a
supplier/needer pair at a non-manufacturing stage with no chokepoint, shortage or emerging bottleneck, e.g. LNG);
geopolitical chokepoint; trusted-partner capacity constraint; emerging bottleneck (deployment need high in either
country and the largest supplier >= 40%); otherwise strategic manufacturing gap. Each carries a one-line market
failure (`market_failure`), the answer to "what prevents this from happening without policy?".

**Instruments** (`config/partnership_instruments.csv`, 20 instruments, intensity 1 light to 5 heavy). An instrument is
eligible when the state and constraint are in its lists and its condition holds. The **main instrument is the lightest
eligible instrument whose family can resolve the state** (`effective_families`: trade/standards/demand for a supplier
and needer; demand/finance/investment for a capacity constraint; demand/finance/development/security/investment for a
shared vulnerability; technology/investment/standards for chain specialisation). Lighter instruments of other families
are listed as complements, never as the answer. Price floors and strategic equity require a severe chokepoint, an
allied shortage and at least medium evidence.

### Fields (all 0-1 unless labelled; none is a weighted index)

| field | rule |
|---|---|
| `competitive_position_a/b`, `strategic_need_a/b` | the profiles |
| `policy_alignment_a/b` | the engine's revealed policy priority percentile (NIPO / IEA PAMS); never enters the state |
| `policy_momentum_a/b` | the engine's 3-year policy momentum |
| `bilateral_complementarity` | supplier/needer states: min(supplier CP − buyer CP, buyer need); specialisation: min(CP, 0.75); otherwise 0 |
| `shared_vulnerability` | min(need A, need B) where a chokepoint or shared coercive dependence exists |
| `trade_fit` | min(supplier CP, buyer need): a lane is as strong as its weaker leg |
| `investment_fit` | min(supplier CP, host signal): 1 if the AIB model selects the investor-host pair at the node, 0.67 if the host is a material buyer, 0.33 otherwise |
| `third_country_fit` | shared development-partnership recipient steps / 5, capped |
| `compact_fit` | 1 both countries hold an AIB compact role in the unit's compact, 0.5 one, 0 none (active states only) |
| `strategic_importance` | max(need A, need B); a buyer's demand without a security need is capped at 0.60 unless a chokepoint exists; for chain specialisation, the need of the side that is weak at this unit |
| `urgency` | max(coercive share if a chokepoint, AIB residual gap, 0.8 if an in-force export control is in the evidence layer, deployment need if high). Not importance. |
| `tractability` | mean(policy priority A, policy priority B, existing-mechanism score: 1 an in-force/signed mechanism with finance covers the compact, 0.5 one without) |
| `additionality` | mean(market-failure score, 1 − existing commitment, 1 − market already working). Label **low** if no failure or the investment is already committed (evidence layer), **high** if failure >= 0.75, nothing committed and the market is not working |
| `execution_speed` | by main instrument family (trade/standards 1 ... investment/development 0.33), capped for greenfield mines and refineries |
| `interdependency` | upstream position in the chain (first step 1, last step 0) |
| `evidence_quality` | the weaker of the two countries' unit evidence (engine stage evidence quality; node: AIB data quality capped at the proxy cap); `low_evidence` below 0.5 |

### Priority (lexicographic)

Tier 1 **joint bet**: an active state, `strategic_importance >= 0.67`, evidence >= 0.5, additionality not low.
Tier 2 **coordinate**: other active states. Tier 3 **manage competition**. Tier 4 **leave open**. Tier 5 demand context.
Within a tier: urgency, additionality, importance, tractability, evidence. The executive summary bundles tier-1 units
**inside the electro-industrial compacts**, one bundle per compact (nodes before cells), ordered by urgency band
then importance; fuel-trade lanes are reported in the trade agenda.

Deal pipeline: tier 1 and 2 units with a market failure and importance >= 0.4 (or an AIB BUILD node); ranked on
importance, additionality, urgency, tractability, speed, interdependency, evidence; at most two deals per compact.

### Compacts (`compact_candidates.csv`, `compact_architecture.csv`)

Per AIB compact (config `aib_strategic_compacts.yml`, read through the scripts/33 outputs, never hard-coded):

- **bilateral priority**: >= 1 joint bet, >= 2 active nodes and both countries hold an AIB compact role;
- **evidence-led candidate**: not a bilateral priority, OSI measures a need in the compact, and either a bilateral
  mechanism with a pipeline covers it or the evidence layer has >= 2 bilateral project/financing/corporate items. These
  are labelled external in the memo: trade data cannot see the partner's capability (ownership abroad, domestic sales);
- **secondary** / **not a bilateral priority** otherwise. Up to five compacts are recommended, OSI-led first.

The architecture table gives each of the six stages the best node (active state first, then nodes with evidence),
or the technology cell where no node sits there; stages are not forced to carry an intervention.

### Master Energy Agreement assessment

`bilateral_mechanisms.csv` records each mechanism's scope, cross-sector reach, pipeline, finance, KPIs, review,
reciprocity and sunset. Rule: **existing** if every recommended compact has a mechanism with pipeline and finance and
a cross-sector mechanism has KPIs and a review cycle; **upgrade** if at least half the compacts are covered by finance
or a pipeline, or a cross-sector mechanism with a pipeline exists; **new MEA** otherwise. The memo then specifies
scope, reciprocal commitments, portfolio, financing lanes, governance, milestones, KPI dashboard, review cycle and
sunset.

## Evidence hierarchy

1. **OSI engine data, stage-specific** (`stage_evidence_quality` high, `evidence_specificity` high): exact.
2. **OSI node trade data** (AIB division of labour): exact for position; need is a **proxy** inherited from the cell.
3. **OSI data with low or no stage-specific evidence**: flagged `low_evidence`; never a joint bet.
4. **External evidence, verified** (the source document was read), then **partial** (secondary source), then
   **unverified** (not found or single weak source). External items never change a relationship state; they enter
   additionality (existing commitments), urgency (in-force export controls), tractability (mechanisms), compact
   selection (evidence-led) and the narrative.

Every bullet in the memo is tagged [Data], [External id, verification], [Inference] or [Recommendation].
`evidence_register.csv` has one row per recommendation per OSI metric or external item; a test checks that no
recommendation lacks one.

## External evidence layer (`data/reference/partnership_evidence/`)

- `policy_evidence.csv`: evidence_id, item_type, name, institution, countries (ISO3 joined by `|`), status,
  publication_date, access_date, technology, compact, stage, aib_node, instrument, amount_usd, source, url, claim,
  verification, research_pass, basis.
- `bilateral_mechanisms.csv`: the MEA inputs, one row per mechanism and pair.
- `institutions.csv`: public institutions by country and instrument family (responsible institutions in the deal pipeline).
- `data/reference/economic_security_agreements.csv` (existing) supplies plurilateral memberships.

The memo is rendered from these files; nothing is scraped at render time. The vintage (access date, verification
counts) is printed under the title. The 2026-11-06 vintage was compiled by two research passes (agreements and
finance; sector projects); 16 items verified against the primary source, 56 partial, 13 unverified.

## Print document (PDF, Word) in the country-brief format

`scripts/34` renders the memo with the country strategy briefs' renderer, so the PDF and Word files match the
briefs: same print stylesheet (`country_brief_html()`: orange rule, eyebrow, title, subtitle, italic provenance
line, choice boxes, ruled tables, superscript sources, appendix on a new page, closing disclosure), same Letter page
and 0.75 in margins, same headless-Edge PDF step (`country_memo_pdf()`), same Word builder
(`scripts/utils/country_memo_docx.py`, format `brief`) and the same render checks (Roboto embedded, no external
reference, Word read-back equals the document, page range, title and label in the PDF text, size limits).

- `R/partnerships/pm_document.R` converts `partnership_memo.md` (the single source of the text) into the brief
  document AST. The Executive Decision Summary is section 0 (printed without a number) and opens with four boxes
  (Build together, Trade and standards, Institutions, Stay out). `[External Exx, verification]` tags become
  superscript source numbers; the Sources list gives each cited item's name, publisher, date, URL, access date and
  verification. `[Data]` cites source 1 (the OSI tables). `[Inference]` and `[Recommendation]` stay as an italic lead-in.
- Tables longer than `document.max_table_rows` (14) move to the appendix with a pointer. Tables wider than
  `document.max_table_cols` (9) drop the columns listed in `document.print_drop_columns` in print only, with a note;
  an over-wide table or an unbreakable string makes Edge shrink every page, and that is what the trim prevents.
- Researcher process notes in the evidence claims ("I could not confirm ...") are dropped from the memo text and
  unverified items are left out of Section 1; both stay in `evidence_register.csv`.
- Two backward-compatible changes to the shared renderer: a section numbered 0 prints without a number
  (`country_brief_html()`, `country_brief_markdown()`, the docx builder's `section_title()`), and the Word subject and
  keywords read the document's `doc_kind` and `keywords` when present. Briefs number sections from 1 and carry neither field.

Outputs in `output/partnerships/<PAIR>/`: `partnership_memo.pdf`, `.docx`, `.ast.json`, `.print.html` and
`render_checks.csv`. With `--publish-dir <dir>` (for the dashboard: `dashboard/public/data/partnerships`) the run also
writes `memo-<PAIR>.<h8>.{json,pdf,docx,md}` and the pair's entry in `<dir>/manifest.json` (`pairs.<PAIR>`: title,
file names, words, generated_at, as_of, evidence_access, format `brief`, `verified: false`), the country briefs'
publication convention, replacing the pair's older hashed files. The dashboard does not read this manifest yet. A
download needs a Partnerships entry point and the size budget checked (`npm run budget`; the US–Japan set is about
1.7 MB, mostly the PDF with its embedded font). Needs Edge or Chrome and python with python-docx and pypdf
(`--edge`, `--python`); `--no-pdf` skips the print step. The Roboto font is read from `dashboard/node_modules`, else
from the processed-data worktree's `dashboard/node_modules`, else `--font`.

Not yet done: the briefs' independent-verifier step. The disclosure says so, and the manifest entry carries
`verified: false`.

## How to run another pair

1. Add the pair to `config/partnership_memos.yml`:
   ```yaml
   pairs:
     USA_KOR: {a: USA, b: KOR, a_name: "the United States", a_short: "US", a_ref: "the US",
               b_name: "Korea", b_short: "Korea", b_ref: "Korea", title: "United States–Korea Strategic Energy & Industrial Partnership"}
   ```
2. Add the pair's rows to `policy_evidence.csv` (countries `USA|KOR`), `bilateral_mechanisms.csv` and, if new,
   `institutions.csv`. Without them the memo still runs: MEA becomes "new MEA", additionality ignores commitments, and
   no compact can be evidence-led.
3. Run (with the stages data and the development-partnership table):
   ```
   Rscript scripts/34_build_partnership_memo.R --pair USA_KOR \
     --processed-dir <stages>/data/processed \
     --devpart <memos>/data/processed/development_partnerships/development_partnerships.rds
   ```
   Outputs land in `output/partnerships/USA_KOR/` (gitignored, reproducible).
4. Read `partnership_memo.md` Section 11 ("Where the findings are weak or counterintuitive") before circulating.

Any pair of the 219 economies in the strategic-position table works. Node-level profiles need both countries in the
AIB assessed set (the demand scope plus major non-allied suppliers); otherwise their nodes read "unlisted".

## Limitations

- **Trade cannot see ownership.** Japanese capability held abroad (Hitachi Energy's transformer plants in Europe and
  the US) or sold at home (heavy nuclear forgings, domestic transformers) is invisible at node level. Nuclear and grid
  therefore rely on external evidence.
- **Node need is a proxy** (inherited from the technology x stage cell).
- **Annual trade misses lead times.** A shortage that shows up as order backlogs (transformers, GOES, forgings) is not
  in the AIB classification; the memo flags nodes where the evidence layer records one.
- **Semiconductors and heat pumps have no OSI cell**: their nodes have demand but no measured security need.
- **Bilateral trade at node level** uses the node's primary cell as a proxy.
- **The external layer is a dated snapshot** with many partial items; recheck before citation, especially tariffs
  (the IEEPA ruling and the Section 122 / 301 replacements) and China's rare-earth controls (suspension to about
  2026-11-10).
- Thresholds are the engine's where one exists; the others (presence share, demand cap, urgency components) are
  calibration choices in `config/partnership_memos.yml`.

## Data that would make the analysis more credible

1. Firm-level capacity by owner and location (transformers, GOES, magnets, separators, forgings), to see
   capability held abroad.
2. Order backlogs and lead times by node (transformers, switchgear, GOES, large forgings).
3. Bilateral FDI stock and flows by sector (BEA / IMF DIP bilateral, not only totals), and the CM-FDI
   project list joined by investor-host pair.
4. Node-level need: import dependence per node, not only per cell.
5. A machine-readable Strategic Investment Initiative project list with status and sponsor.
6. Verified primary texts for the partial items in the evidence layer.
