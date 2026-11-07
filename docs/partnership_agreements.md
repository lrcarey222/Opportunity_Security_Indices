# Partnership agreements layer

Status: provisional (built 2026-11-06 on `feat/partnership-agreements`). Config:
`config/partnership_agreements.yml`. Code: `R/agreements/` (pure), `scripts/35`-`37` (IO),
`dashboard/src/agreements/`.

The layer answers *who cooperates with whom, on what, with which instruments, how deeply, and where
strategically important cooperation is missing*. It does so with individual agreements from several
datasets, linked into canonical agreements, rolled up to country pairs, and joined to the
capability, need and policy data the rest of the dashboard already uses.

> **There is no comprehensive global registry of non-binding MOUs, joint statements and joint
> initiatives.** Every source here misses some of them. An empty cell means *no agreement was found
> in these sources*, not *no cooperation exists*. The dashboard says so wherever absence is shown.

## 1. What existed before (2026-11-06)

- **Data model.** Agreements were club memberships only:
  `data/reference/economic_security_agreements.csv`, 104 rows `iso3 x agreement` for six codes
  (U.S. FTA, IPEF, MSP, MSP Forum, G7, NATO), with no date beyond `since`, no type, text, sector or
  instrument. They feed the coalition tiers (`R/indices/coalition.R`, route `agreement`), the Country
  fact sheet (`R/dashboard/country/country_ties.R`) and the Strategic `ties` chunk
  (`R/dashboard/dash_ties.R`, scripts/85).
- **Country pairs** were never stored for agreements. The dashboard intersected two membership sets
  on the fly (`sharedAgreements()` in `dashboard/src/views/Partners/ties.ts`). Two defects follow
  from that: the `U.S. FTA` rows do not list the United States, so a US pair never shows its own FTA
  in the ties chunk; and two U.S. FTA partners (Korea and Australia) appear to share an FTA with each
  other. The Country fact sheet's `country_agreements_shared()` handles U.S. FTA correctly; the
  `ties` chunk path does not. EU rows (`EUU`) are not propagated to member states.
- **Other partner measures** are directed dyad scores per technology x stage (Friendshore,
  Partnership Opportunity; scripts/15, 25), a partner-only Development Potential, the Partnership
  Strength Index (`psi_dyad_tbl`), UN-vote alignment (`geo_alignment_tbl`), CM-FDI investment ties,
  AIB relationship theses (1,093 undirected pairs) and development partnerships (scripts/93). None
  is an agreement.
- **Dashboard.** `/aib/partnerships` (Trade / Investment / Development / Geopolitical, each through
  Today / partner scores / AIB lenses) and the Country profile's partnerships section. The
  Geopolitical "Today" lens lists the six clubs. No page shows individual agreements.
- **Filters and scores.** Rankings are by the partner scores at default weights; agreements are
  ordered by count, then alignment. There was no depth class.

## 2. Conflicts with the existing model, and how they are resolved

| Conflict | Resolution |
|---|---|
| Memberships are per country, not per agreement or pair. | The memberships adapter turns each club into one plurilateral agreement record and `U.S. FTA` into 20 bilateral USA-X records. The reference CSV and its consumers (coalition tiers, fact sheet, ties chunk) are unchanged. |
| The `ties` chunk's U.S. FTA defect. | Left in place in the Strategic bundle (scripts/85 is not rerun here); the new Agreements page shows the correct pairs. Fix at source when scripts/85 is next run. |
| The glossary already defines `partner_gap` ("a Partner choice without a credible allied answer"). | The new analysis is called **agreement gaps** (`agreement_gap`) to avoid a clash. |
| The EU is a party in GSP-free sources (IISD, curated) but DESTA expands EC agreements to member states. | DESTA's EU member lists are collapsed back to `EUU`; every EU-level agreement is attributed to current member states' pairs with `via_eu` (config `pairs.propagate_eu`). |
| Partner scores are directed; agreements are symmetric. | Pair profiles are undirected (`a < b`). The gap join keeps direction (who needs, who supplies). |
| Technologies: the brief's sector list is wider than the dashboard's 11 technologies. | The brief's sectors are kept as `sector`; each maps to dashboard technologies and stages where one exists (`sectors.*.techs/stages`). Sectors without a technology (CCS, cement, steel, aluminium, data centres, semiconductors) are shown but not joined to capability data. An `Oil, gas and coal` sector is added so LNG and crude deals are not filed as clean energy. |
| Six stages vs legacy Upstream/Midstream/Downstream. | `supply_chain_segment` uses the six stage ids plus `cross_stage`; legacy values are never stored. |
| Country page first-view budget (33 kB headroom). | The new views are a separate lazy route under the Allied Industrial Base hub with their own data folder; the Country profile and AIB Partnerships page only link to it. |

## 3. Architecture

```
data/raw/ (gitignored)                 data/reference/
  gsp_partnership_documents.csv          economic_security_agreements.csv (existing)
  desta_*.csv (5 files)                  partnership_agreements/curated_agreements.csv
  iisd_trade_climate_tracker.csv (opt.)  partnership_evidence/ (pmemo, when merged)
        |                                       |
        v                                       v
scripts/35_ingest_agreements.R --source <gsp|desta|memberships|curated|pmemo|iisd|all>
   R/agreements/adapter_<source>.R  ->  data/processed/agreements/source_<source>.rds
   (records + codes + extras, validated by agr_validate())
        |
        v
scripts/36_build_agreement_partnerships.R
   R/agreements/agr_link.R    canonical agreements + links (dedup, provenance kept)
   R/agreements/agr_pairs.R   pair profiles + depth class
   R/agreements/agr_gaps.R    agreement gaps (joins strategic_position_tbl, partner tables)
   -> data/processed/agreements/{canonical_agreements,agreement_links,agreement_candidates,
      canonical_codes,pair_profiles,agreement_gaps,execution_gaps,agreement_delivery}.{rds,csv},
      build_meta.rds, build_report.md
   (--processed-dir: outputs/strategic_position_tbl.rds, partner_compat_cells_tbl.rds,
    outputs/aib/aib_relationship_theses.csv; partnership_index_outputs.rds is never read)
        |
        v
scripts/37_export_agreement_data.R  ->  dashboard/public/data/agreements/
   R/agreements/agr_export.R  (pure shaping; manifest.json, agreements-global.<h8>.json,
                               agreements-<ISO3>.<h8>.json)
```

Run order: `Rscript scripts/35_ingest_agreements.R --source all [--pmemo-dir <dir>]`, then
`scripts/36_build_agreement_partnerships.R [--processed-dir <dir>]`, then
`scripts/37_export_agreement_data.R`. `canonical_agreements` carries the per-agreement flags of
section 7 as additive columns (`active`, `recent`, `eligible`, `ei`, `trade`, `in_effect`,
`trade_depth_index`).

Fetchers (`scripts/fetchers/gsp_partnerships.R`, `scripts/fetchers/desta.R`) download the public
datasets into `data/raw` through the existing fetcher framework (validated, atomic, fail-soft), and
each raw file has a `config/raw_inputs_manifest.yml` entry. Each source is ingested and refreshed on
its own; reruns are idempotent (deterministic ids, files overwritten whole).

Adding a source (an EU MoU list, a foreign-ministry database, an export-credit agency): write
`R/agreements/adapter_<id>.R` returning `list(records, codes, extras)` in the common schema, add the
source to `sources:` in the config and to `AGR_SOURCES`, and a branch in scripts/35. A source that
is a hand-maintained table can instead add rows to the curated CSV.

## 4. Common schema

Records (`AGR_RECORD_COLUMNS`, `R/agreements/agr_schema.R`): `source_dataset, source_record_id,
title, parties, country_a, country_b, additional_parties, n_parties, bilateral_or_plurilateral,
agreement_type, agreement_type_raw, partnership_label, legal_status, legal_status_basis,
announcement_date, signature_date, effective_date, expiry_date, primary_date, date_precision,
relationship_category, sector, technology, supply_chain_segment, instrument_types,
implementation_status, implementation_status_raw, source_url, primary_document_url,
primary_source_available, last_verified, source_last_updated, description, notes`.

- Parties are ISO3 (`EUU` for the EU), sorted; `country_a`/`country_b` are the first two.
- Dates are `YYYY-MM-DD`, `YYYY-MM` or `YYYY`; `date_precision` says which.
- Multi-valued fields are `|`-joined and are always the collapse of the **codes** table
  (`source_record_id, dimension, code, basis, n_hits, snippet`), which keeps the basis of every code
  (`source_coded`, `curated`, `type_implied`, `text_title`, `text_body`).
- `extras` holds source-specific tables keyed by `source_record_id` (DESTA design and depth,
  implementation evidence).

Vocabularies are in the config: `agreement_types`, `partnership_labels`, `legal_statuses` with
`legal_status_bases` (never read off a title), `implementation_statuses`, `sectors`, `instruments`
(with `group` and `operational`), `coding_bases`, `confidence_levels`.

## 5. Linkage (deduplication)

`agr_link_records()` (R/agreements/agr_link.R):

1. **Within GSP**, documents on the same unordered pair within `gsp_event_window_days` are one event
   (both governments often publish), unless one is a treaty and the other is not (a treaty signed
   during a visit stays apart from the leaders' statement).
2. **Across sources**, candidates share the party set (Jaccard >= `party_jaccard_min`) and a date
   within `date_window_days` (or `year_window` when either date is a year). They link when the
   normalised-title token Jaccard is >= `title_jaccard_min` (rule `title`), when the shorter title's
   tokens lie inside the longer one (share >= `title_containment_min`, at least
   `title_containment_min_tokens` tokens; rule `title_containment`), or, for two trade agreements on
   the same bilateral pair, on the pair and year alone (`trade_pair_year`). A bilateral trade agreement
   that one source records as a leg of a small plurilateral trade agreement another source records
   (U.S. FTA "NAFTA / USMCA" with Canada vs DESTA's NAFTA; the CAFTA-DR legs) links to the single best
   such plurilateral: it must contain the pair, have at most `trade_leg_max_parties` parties, share
   title tokens (containment >= `trade_leg_min_containment`) and enter into force within
   `trade_leg_year_window` years (rule `trade_leg`); several legs from one source may join it. Titles are compared
   after removing country names (countrycode), demonyms, months, administrative words and
   `generic_title_words`, with light stemming. Score = (title similarity + party Jaccard + date
   closeness) / 3 (a containment counts 0.75 x its share; the trade rule a full title match). Links
   are accepted from the highest score down; one below `min_link_score` is not made and both records
   stay canonical. Records of one source never link with each other except GSP documents of one
   event and curated rows (`within_source_sources`: two research passes can record one agreement
   from both sides), and a canonical agreement never holds two records of an `exclusive_sources`
   source (DESTA, memberships, pmemo, IISD). `agreement_candidates` lists every candidate and why it
   was or was not linked.
3. Clusters are connected components of accepted links. Each gets `canonical_agreement_id` (`agr-`
   + 10 hex characters of a hash of its sorted source_record_ids), `source_records` (all members),
   `source_datasets`, `source_count` (distinct datasets), `link_rules` and `link_score_min`.
4. Field precedence for the canonical record: curated > pmemo > iisd > desta > memberships > gsp for
   type, status and dates; codes are the union over members, keeping each basis; titles prefer the
   curated or structured source; the partnership label is the highest tier any member names.

No source record is discarded; `agreement_links.csv` lists every member and the rule that linked it.

**Confidence** (canonical): `high` when a member links to primary government or agreement text
(`primary_source_available`, official host list in the config) or `source_count >= 2`; `medium` for
one structured dataset; `low` for a curated row whose verification is `unverified` or a title or
date match with score below 0.75 (the trade rules, which match on the pair or membership and an
entry-into-force year, do not lower confidence).

## 6. Pair profiles

One row per unordered pair (`a < b`) with at least one agreement (`pair_profiles`). An agreement
counts for every pair of its parties, plus member-state pairs through `EUU` (`via_eu`), except the
EU's seat in a club (`no_propagate_categories`: the G7 and the Minerals Security Partnership do not
make every member state a member). An accession
record (DESTA lists the whole membership after the accession) counts only for pairs of an acceding
country with a member, so existing members are not credited with a new agreement each time a country
joins (`agr_canonical_acceding()`). Fields:

| Field | Meaning |
|---|---|
| `pair`, `a`, `b` | pair key `AAA|BBB` |
| `n_agreements`, `n_bilateral`, `n_plurilateral`, `n_via_eu` | canonical agreements of the pair |
| `n_active`, `n_recent` | active (section 7) / dated within `recent_years` of `as_of` |
| `oldest_active`, `newest` | dates (normalised strings) |
| `has_strategic_partnership` | a strategic-partnership type or label |
| `has_trade_agreement` | an `fta`/`epa`, or a treaty coded with the trade sector, in force or signed |
| `has_sectoral` | an electro-industrial or industrial sector covered at a specific grade |
| `sectors`, `ei_sectors` | `code:grade` pairs, `|`-joined; grade `specific` or `text` |
| `n_sectors`, `n_ei_sectors` | counts of the above |
| `technologies`, `segments` | `|`-joined, specific grade only |
| `inst_<group>` for trade, finance, investment, offtake, supply, manufacturing, rd, standards, procurement, dialogue | 0 none, 1 text mention, 2 specific |
| `n_announced`, `n_signed`, `n_in_force`, `n_delivery` | by implementation status (delivery = implementation underway or operational) |
| `share_delivery` | `n_delivery / n_agreements` |
| `trade_depth_index` | the highest DESTA `depth_index` (0-7) of the pair's trade agreements in force |
| `depth_all`, `depth_all_rule` | depth class over all agreements and the rule that set it |
| `depth_ei`, `depth_ei_rule` | the same over electro-industrial agreements |
| `sources`, `max_confidence` | datasets behind the pair; best confidence |
| `agreement_ids` | canonical ids, newest first |
| `types`, `n_ei` | agreement types of the pair (`|`-joined); its electro-industrial agreements |
| `tech_cover` | `tech:grade` pairs: technologies an active, depth-eligible agreement covers through a mapped sector (best grade); the gap join recomputes coverage stage by stage from the agreements (section 8) |

`inst_other` is also written (instruments of group `other`).

## 7. Partnership depth (provisional)

Rules are tested from 5 down; the first that holds sets the class. Only agreements that are not
`cancelled` or `inactive` and that are active count. Active means in force (binding) or, for a
signed or announced agreement, dated within `active_window_years` (10) of `as_of`. Plurilaterals with
more than `max_parties_for_depth` parties count only toward class 1.

| Class | Rule (all agreements; the electro-industrial variant uses only electro-industrial agreements) |
|---|---|
| 5 Integrated | class 4 holds, and specific-grade operational instruments in the **trade** group and in the **finance, investment or manufacturing** groups, and class-3 sectoral cooperation, and something in effect: a binding agreement in force or delivery evidence (`integrated_needs`) |
| 4 Operational | an operational instrument (config `operational: true`) coded at a specific grade |
| 3 Sectoral cooperation | a sector outside `sectoral_exclude` covered at a specific grade, or at text grade (`min_body_sentences` sentences in one document) |
| 2 Framework | an agreement of a `framework_types` type, or a strategic-partnership label |
| 1 Dialogue | any other agreement (joint statements, dialogues, club co-membership) |
| 0 None | no agreement found |

Every FTA or EPA in force carries type-implied trade instruments, so any pair with one is at
least class 4 in `depth_all` (4,630 of 9,990 pairs at the 2026-11-06 build); `depth_ei` is the
discriminating scope. Keyword mentions in a broad joint statement never make class 4: a text-grade instrument is shown as
"mentioned" but only a source-coded, curated or type-implied instrument counts as established.
Quantity never raises the class.

## 8. Agreement gaps

`agreement_gaps` (`agr_gaps()`, R/agreements/agr_gaps.R) is keyed `need_iso3, supplier_iso3, tech`
(directed: the first needs, the second can supply). Complementarity and coverage are evaluated per
**technology x stage cell**, then reported per technology:

- A cell is **complementary** when the supplier is a `recommended_partner_1..3` of the need side in
  it, or the need side's need percentile is >= `need_min` while the supplier's competitive position
  percentile is >= `capability_min` in it. It counts only when the two economies' UN-vote alignment
  (`geo_alignment_tbl`: 1 same bloc, 0.5 one side non-aligned, 0 opposite blocs) is at least
  `min_alignment` (0.5): a match across opposite blocs keeps its components but is flagged
  `alignment_blocked` and is not complementarity. Unknown alignment does not block.
- An agreement **covers** the cells of the technologies and stages its sectors map to
  (`sectors.*.techs` and `sectors.*.stages`; all six stages when a sector lists none), at its grade
  for the sector. A critical-minerals framework therefore covers Batteries Extraction, Refining and
  Recovery, not battery assembly; a grid-equipment agreement covers Electric Grid Components and
  Assembly. Only active, depth-eligible agreements that are not cancelled or inactive cover cells;
  EU-level agreements cover member-state pairs.

Only supply stages count (`supply_stages`): Deployment is demand context, so a large deployer is not
a "supplier" of another's deployment, and no execution gap is read at Deployment.

Rows are every (need, supplier, tech) with complementary cells, plus every one an agreement of the
pair covers at any grade (then both directions), where both sides are in the scored universe of 219
economies (the EU itself has no need or capability, so EU-level agreements reach the gaps through
member states). Where the need side has no need or the supplier no capability measured for the
technology, complementarity is `not_assessed` and the quadrant `none`: missing data is never read
as low complementarity.

| Field | Meaning |
|---|---|
| `need` | max over the technology's supply stages (`supply_stages`: every stage but Deployment, which is demand context) of `supply_chain_need` of `need_iso3` |
| `capability` | max `competitive_position_percentile` of `supplier_iso3` over the same stages |
| `recommended` | cells where `supplier_iso3` is a recommended partner of `need_iso3` (raw, before the alignment gate) |
| `n_cells_high` | cells where need >= `need_min` and capability >= `capability_min` (raw) |
| `alignment`, `alignment_blocked` | the pair's UN-vote alignment; TRUE when the raw match exists but the pair is below `min_alignment` |
| `stages_high` | the stages with aligned complementarity (supply-chain order) |
| `friendshore` | max Friendshore Index of the dyad over the technology's cells (`partner_compat_cells_tbl`, `reporter_iso` = need side) |
| `aib_shared` | the pair's `priority_compacts` in the AIB relationship theses (pair level) |
| `complementarity` | `high` when `stages_high` is not empty; `not_assessed` when need or capability is missing; else `low` |
| `covered_grade` | best grade of the pair's coverage at `stages_high` (at any stage when complementarity is low): `specific`, `text` or `none` |
| `stages_covered` | the stages of the technology covered at a specific grade |
| `architecture` | `strong` when `covered_grade == "specific"`; else `weak` (`strong_depth`, null by default, can add a pair-depth clause) |
| `quadrant` | `established` (high, strong), `opportunity` (high, weak), `legacy` (low, strong), `none` (low and weak, or not assessed) |
| `covering_agreements`, `n_agreements`, `depth_all`, `depth_ei` | the covering agreements at the relevant stages and the pair roll-up (not exported) |

`execution_gaps` (keyed `iso3, tech`): the **priority stages** of a technology (policy priority
percentile >= `priority_min` within the economy, or a cell whose strategic choice is in
`priority_choices`) where the economy has aligned high-complementarity suppliers and no agreement
with any of them covers the stage at a specific grade. A row lists those `stages`, the suppliers
without coverage (recommended first, then by capability), their count, the ones covered at text
grade only, `priority` (the economy's highest policy priority percentile in the technology) and
`priority_basis` (`policy_priority`, `choice_secure`, `choice_partner`, `|`-joined).

All components are exported; the quadrant is a labelled rule over them, not a score, and the
dashboard shows the components next to it. The quadrant labels follow the brief: "legacy" means
strong architecture where the measures find no complementarity (politically driven, or older
cooperation), and the dashboard says which of the two reasons applies (opposite blocs, or no
recommendation and no matching need and capability at one stage).

## 9. Dashboard data contract (`dashboard/public/data/agreements/`)

Written by scripts/37; checked by `dashboard/src/agreements/contract.ts`. Arrays that R exports are
wrapped in `I()` so a one-item array stays an array.

`manifest.json`
```
{ version: 1, created: string, as_of: string, git_commit: string,
  files: { global: "agreements-global.<h8>.json", countries: { [iso3]: "agreements-<ISO3>.<h8>.json" } },
  vocab: {
    agreement_types: {[id]: label}, partnership_labels: {[id]: label},
    legal_statuses: {[id]: label}, implementation_statuses: {[id]: label},
    delivery_statuses: string[], coding_bases: {[id]: label}, confidence_levels: {[id]: label},
    depth_classes: {"0".."5": label}, instrument_groups: {[id]: label},
    sectors: [{id, label, ei: boolean, techs: string[]}],
    instruments: [{id, label, group, operational: boolean}],
    sources: [{id, label, short, citation, url, licence, n_records, last_updated}]
  },
  names: {[iso3]: name}, eu_members: string[],
  (vocab also carries specific_bases, established_instrument_bases and compacts: {[id]: name})
  counts: { records, canonical, pairs, countries, by_source: {[id]: n} },
  caveats: string[] }
```

`agreements-global.<h8>.json` (columnar, one entry per pair with an agreement)
```
{ pairs: { a: string[], b: string[], n: number[], n_recent: number[], newest: string[],
           depth_all: number[], depth_ei: number[], has_sp: boolean[], has_trade: boolean[],
           has_sectoral: boolean[], n_delivery: number[], ei_sectors: string[] ("|"-joined ids),
           types: string[] ("|"-joined agreement types), sources: string[] ("|"-joined),
           years, years_ei, legal, status, igroups, stages: string[] ("|"-joined, for the
           all-economies filters: a pair passes when any of its agreements has the value) },
  countries: { iso3: string[], n_partners: number[], n_partners_d3: number[],
               n_agreements: number[], n_ei_agreements: number[], newest: string[] } }
```

`agreements-<ISO3>.<h8>.json`
```
{ iso3,
  agreements: [{ id, title, parties: string[], type, label, legal, legal_basis,
                 date, date_precision, signature_date, effective_date, status, status_raw,
                 category, sectors: [code, basis][], instruments: [code, basis][],
                 techs: string[], segments: string[], ei: boolean, via_eu: boolean,
                 sources: [{dataset, record_id, url}], primary_url, primary_available,
                 confidence, source_count, delivery: [{text, status, url}],
                 trade_depth_index: number|null }],
  partners: [{ partner, n, n_bilateral, n_plurilateral, n_via_eu, n_active, n_recent,
               oldest_active, newest, has_sp, has_trade, has_sectoral,
               sectors: [code, grade][], ei_sectors: string[], techs: string[], segments: string[],
               inst: {[group]: 0|1|2}, n_announced, n_signed, n_in_force, n_delivery,
               share_delivery, trade_depth_index: number|null,
               depth_all, depth_all_rule, depth_ei, depth_ei_rule, sources: string[],
               max_confidence, agreement_ids: string[] }],
  gaps: [{ partner, tech, side: "needs"|"supplies", need: number|null, capability: number|null,
           recommended: number, friendshore: number|null, aib_shared: string[],
           complementarity: "high"|"low", covered_grade: "specific"|"text"|"none",
           architecture: "strong"|"weak", quadrant, alignment: number|null,
           alignment_blocked: boolean, stages_high: string[], stages_covered: string[] }],
  execution_gaps: [{ tech, stages: string[], priority_basis: string, priority: number|null,
                     suppliers_without_coverage: string[] }] }
```
Additive: `vocab.specific_bases` (config `specific_bases`). An agreement's `sectors` and
`instruments` keep one entry per code, with its most specific basis (config `coding_bases` order);
`techs` and `segments` list every code. Agreements are newest first; partners by `depth_all`, then
`n`. Empty strings are exported as `null`. An economy's file lists the agreements it is party to
and, for EU member states, the EU's own agreements (`via_eu: true`); the files are written for every
economy with at least one (206 at the 2026-11-06 build, the largest CHN at about 58 kB gzip).
`side` is the focal country's side: `needs` rows have the focal country as `need_iso3`.
Per-country gap rows keep, per technology and side, the `top_candidates` partners by
complementarity (recommended, then need x capability) plus every partner with coverage.

## 10. Sources

Records per source at the 2026-11-06 build: GSP 1,946, DESTA 1,028, memberships 25, curated 66,
pmemo 9, IISD 0 (not staged). Each source is ingested on its own by scripts/35
(`--source <id>`), validated by `agr_validate()` and written to
`data/processed/agreements/source_<id>.rds`; raw files have entries in
`config/raw_inputs_manifest.yml` (rendered in `docs/sources.md`).

### Global Strategic Partnership Dataset (GSP)

Chang-Liao and Fu (2025), Harvard Dataverse doi:10.7910/DVN/UJD7LW, V1.0 (2025-09-23), CC0.
Fetcher `scripts/fetchers/gsp_partnerships.R` -> `data/raw/gsp_partnership_documents.csv`;
adapter `R/agreements/adapter_gsp.R` (`agr_adapt_gsp()`).

**Captures.** 1,948 documents (1992-2023) that governments published about strategic and other
named partnerships: joint statements (1,370), news items (237), fact sheets (103), treaties (98),
joint press conferences (91) and action plans (44), with full text (English, Chinese, Japanese,
Korean, Spanish, French, Russian and others) and the source URL. Each row is one government's
publication, so a summit often appears twice (once per side); linkage merges the two into one
event (section 5, rule `gsp_event`: 360 documents joined another at this build).

**Misses.** Only documents the authors collected for partnership research: most sectoral MOUs,
financing agreements, implementation records and post-2023 documents are absent. GSP codes no
content, so every sector and instrument code is a keyword code.

**Normalisation.** One record per document (`gsp:<doc_id>`; repeated ids get `-2`, `-3` in a fixed
order); parties are the sorted ISO3 of publisher and partner (Kosovo -> XKX; Abkhazia `GE-AB` rows,
non-ISO codes and self-pairs dropped and listed in `extras$gsp_dropped`). Type: Treaty -> treaty
(binding, signed, signature date); Joint Statement -> joint_statement, or joint_declaration when the
title says so; Action Plan -> framework; the rest -> other. Joint statements, press conferences and
action plans are non-binding; news, fact sheets and untyped rows are unknown. A blank or invalid
date falls back to year-month-day, then month, then year. The partnership label comes from the
title, else from body sentences that name the pair's partnership (most named tier; regional
groupings excluded); `notes` says which.

**Headlines.** In news items and fact sheets (type `other`) a title keyword is a mention, not the
document's subject: it is recorded as a body code with one hit, so it never reaches the specific
grade.

**Caveats.** "Treaty" includes domestic acts and decrees publishing a treaty. Body codes are
text-grade only and need a cooperation word in the same sentence, yet list-of-areas sentences
dominate. Instrument patterns are English (plus a few CJK instrument patterns), so CJK documents
get few instrument codes, and Cyrillic and other scripts get none. 24% of sources are news sites
or unlisted official hosts (`primary_source_available` FALSE).

### Design of Trade Agreements (DESTA)

Dür, Baccini and Elsig (2014), DESTA v2.3 (September 2025), designoftradeagreements.org. No
licence is published; the authors ask for the citation, and only derived facts are
redistributed. Fetcher `scripts/fetchers/desta.R` -> `data/raw/desta_list_of_treaties.csv`,
`desta_provisions.csv`, `desta_indices.csv`, `desta_withdrawals.csv` (read) and
`desta_dyads.csv` (fetched for reference only, manifest `retain: true`: the dyadic list omits
dyads by membership role, so parties never come from it); adapter `R/agreements/adapter_desta.R`
(`agr_adapt_desta()`; its header states every rule).

**Captures.** Every preferential trade agreement since 1948: 1,410 entries in v2.3, of which 1,028
become records (fta 990, epa 28, framework 10; in force 812, signed 208, inactive 8). 382 entries
are not records (78 negotiations, consolidated versions, protocols that keep their parent's
parties, accessions of new EU member states to an EU agreement; `extras$desta_skipped`).

**Normalisation.** One record per base treaty and per accession; a protocol is a record only when
its party set differs from its parent's (the UK's 2019-2021 continuity agreements, the ALADI
bilaterals). Parties come from c1..c91 of the list file (numeric ISO -> ISO3; 900 -> XKX).
Withdrawals remove the withdrawing country; a record left with fewer than two parties is
`inactive`. EC/EU agreements are collapsed back to `EUU` (105 records) and propagated to
current member states by the pair roll-up. Type `epa` for Economic Partnership Agreements and
Japan's agreements, `framework` for DESTA framework agreements, `fta` otherwise. Status `in_force`
when the entry-into-force year is reached, else `signed`; DESTA codes no supersession. Provisions
of the newest consolidated entry become sector and instrument codes with basis `source_coded`;
FTAs imply `trade_preference` and `tariff_reduction` (`type_implied`). `depth_index` (0-7) comes
from the indices file (748 records), is recomputed from the provisions with the published rule
where no index row exists (67; the recomputation reproduces all 692 published values), or is
missing (213); `extras$desta_design` keeps it with the seven components, flexibility and
enforcement, and the pair roll-up reports the highest index of the pair's trade agreements in
force (`trade_depth_index`).

**Misses and caveats.** Trade agreements only: no MOUs, sectoral partnerships or financing.
Signature years only (`date_precision` year for most records), so cross-source links rely on
the pair and year (`trade_pair_year`).

### Memberships (project reference, `memberships`)

**Captures.** The six codes of `data/reference/economic_security_agreements.csv` as agreements:
20 bilateral USA-X records for `U.S. FTA` (the USA has no row; every U.S. FTA is with it), and one
plurilateral record each for IPEF (14), MSP (15), MSP Forum (30), G7 (8) and NATO (32). Type,
legal status and scope are recorded by hand per club (`MEMBERSHIP_SPECS` in
`R/agreements/adapter_memberships.R`): U.S. FTA `fta` / binding / in force, with the `trade` sector
and the `trade_preference` and `tariff_reduction` instruments implied by the type; IPEF `framework`
/ mixed, with the pillars (trade, supply chains, clean energy) and the Supply Chain Agreement's
Council and Crisis Response Network checked on official pages; MSP and MSP Forum non-binding,
`critical_minerals`, and inactive since FORGE succeeded the MSP (February 2026, row notes); G7 a
non-binding forum; NATO a binding treaty.

**Normalisation.** U.S. FTA dates are entry into force (`effective_date`); a club's date is its
earliest membership date (launch; NATO's is the treaty signature). The MSP Forum rows list
Forum-only members and say MSP partners are automatically members, so the 15 MSP partners are
added (`extras$members$added_from_note`). Every U.S. FTA record links to its DESTA record: 12 bilateral
ones by `trade_pair_year`, and the NAFTA/USMCA and CAFTA-DR legs by `trade_leg` (into DESTA's NAFTA
and CAFTA-DR records; USMCA stays a separate agreement). DESTA lists CAFTA and CAFTA-DR as two base
treaties, so CAFTA pairs show both.

**Misses and caveats.** Plurilateral FTAs (CAFTA-DR, NAFTA/USMCA) are kept as USA-X legs only, so
the pair Costa Rica-Guatemala has no CAFTA-DR record from this source (DESTA has it). CAN and MEX
carry NAFTA's 1994 date with USMCA (2020-07-01) in the notes. Membership dates of later joiners
are in `extras$members`, not in the record. The reference CSV and its consumers are unchanged.

### Curated government agreements (`curated`)

**Captures.** Agreements recorded by hand in
`data/reference/partnership_agreements/curated_agreements.csv`, one row each, from an official
source: critical-mineral MOUs, sectoral partnerships, investment frameworks and joint initiatives
that GSP, DESTA and IISD miss or record too coarsely. Rows map 1:1 to records
(`curated:<curated_id>`). The 2026-11-06 seed has 66 rows from two research passes: 29 EU and
EU-member agreements (`EU-*` ids) and 37 US agreements (`US-*` ids), mostly 2023-2026.

**Normalisation.** Sectors, instruments and supply-chain segments become codes with basis
`curated`; technologies follow from the sectors, and stages from the sectors only when the row
gives none. `legal_status` is `unknown` unless the official text says binding or non-binding (its
wording is required in `legal_status_note`); `legal_status_basis` is then `curated`. Delivery
statuses need written implementation evidence with a URL (`extras$implementation_evidence`).
`verification` (verified / partial / unverified) and the publisher go to `extras$curated_meta`
for the confidence rule. The two passes can record one agreement from both sides (the EU-US
critical minerals MoU is `EU-CRM-USA-2026` and `US-CRM-EUU-2026`): curated is the one source whose
records may link with each other (config `linkage.within_source_sources`).

**Rules and caveats.** Sectors only where the text names them as areas of cooperation;
instruments only where it establishes or commits to them. The adapter stops on any malformed row
and names its `curated_id` (bad ISO3, vocabulary breach, bad date, missing source). Coverage is
whatever has been added: an absent agreement means *not yet curated*. The README in that folder
has the column definitions and the steps to add a row.

### Partnership-memo evidence (`pmemo`)

**Captures.** The US-Japan Partnership Memo evidence (`data/reference/partnership_evidence/`, read
from `--pmemo-dir` until it is merged): the 8 mechanisms of `bilateral_mechanisms.csv`
(`pmemo:<mechanism_id>`; the Quad framework has four parties) and agreement-like evidence rows
that no mechanism references (agreements, frameworks, plurilaterals, dialogues and two-country
government policies: at the 2026-11-06 vintage the US-Australia Critical Minerals Framework
UJ-044). Disputes, market facts, export controls and single-country rows are never records.

**Exclusions.** `PMEMO_EXCLUDE` in the adapter lists items that are never records, with the reason
(`extras$excluded`). Only plurilateral items whose party list in the evidence is incomplete belong
there: FORGE (E37, the Forum on Resource Geostrategic Engagement) is a club of about 50
participants with no published roster, and the evidence names only the USA and Japan, so a record
would read as a US-Japan bilateral.

**Normalisation.** Explicit mapping tables in `R/agreements/adapter_pmemo.R`: compacts and
technologies to sectors (SC-SEMIS on a critical-minerals mechanism means its mineral inputs;
power generation by its fuel), evidence instruments to instrument codes (a JBIC/NEXI/EXIM loan is
`export_credit`), stages to segments; all `curated`. Duplicates across the two research passes
keep the mechanism-linked item (`extras$duplicates`).

**Delivery evidence.** Project, financing, corporate and procurement rows linked through
`evidence_ids` become delivery evidence (`extras$implementation_evidence`, exported as each
agreement's `delivery` list). A mechanism becomes `implementation_underway` when a linked item is
under construction or operating, **or when a verified financing is signed** (adapter argument
`financing_signed_counts = TRUE`): a signed loan commits money to a named project, which is
delivery, not announcement. At this vintage that rule alone moves the Strategic Investment
Initiative (`USJ-SII`) to `implementation_underway`, on three verified signed JBIC loans
(E23-E25). Set the argument to FALSE to require construction or operation.

**Caveats.** One pair (plus the Quad) only; many items are partial or unverified
(`extras$curated_meta`). Legal status is set only where the evidence says non-binding. Several
pmemo records describe the same agreement as a curated row; linkage merges them (section 5).

### IISD Trade & Climate Tracker (`iisd`, optional, not staged)

**Captures.** When IISD shares an export: economic agreements signed by two or more governments
from January 2025 (FTAs, ECAs, MoUs / MoCs, agreements on reciprocal trade, CRM frameworks and
strategic partnerships), with IISD's expert coding of operative provisions on energy, climate,
critical minerals, heavy industry and notable tariff or market-access treatment. IISD reported
211 agreements to September 2026 (139 linked to critical raw materials).

**Normalisation.** Columns are matched loosely (`IISD_COLUMN_ALIASES`); parties as ISO3 or names
(EU -> `EUU`; EFTA, MERCOSUR and GCC expand to members). Subtopics become sector codes with basis
`source_coded` (CRMs -> critical minerals; both Energy subtopics -> general clean energy; Heavy
Industry -> general industrial; Tariffs and Market Access -> trade with `tariff_reduction` and
`market_access`); the coded summaries are keyword-coded (`text_body`) for specific sectors and
instruments. Types: FTA and ART -> `fta`, ECA and CRM frameworks -> `framework`, MoU / MoC ->
`mou`, strategic partnership -> `strategic_partnership`.

**Misses and caveats.** The site is behind a bot challenge and all rights reserved: nothing is
fetched, and without a shared export the source is empty (scripts/35 skips it with a message).
IISD codes text, not implementation, so every record is `signed`; legal status stays unknown
unless the export codes it. Climate subtopics have no sector of their own (kept in
`extras$topics`). The window starts in 2025.

### Assessed, not ingested

- **UN Treaty Series (UNTS, treaties.un.org).** Over 250,000 treaties and treaty actions, with
  deterministic keys (registration number, `objid`) and coarse subject terms (Trade, Energy,
  Mineral resources, Investments, ...). No API or bulk export: search is ASP.NET postbacks, the
  title-search Excel export lists no parties, and the advanced-search export failed in testing.
  It registers binding treaties only (not MOUs or joint statements), with registration lags of
  years (Australia-Ukraine nuclear, concluded 2016, registered 2022). The UN terms of use forbid
  redistribution and derivative compilations without written permission. Not ingested; a
  candidate for a historical binding-treaty layer if the UN grants permission.
- **ECOLEX (FAO, IUCN, UNEP).** Environmental law records, some soft-law MoCs filed as treaties.
  The user agreement prohibits redistribution and internet posting, robots.txt disallows search
  results with a 30-second crawl delay, and coverage of energy, trade and minerals cooperation is
  thin and stale (Japan AND Australia, bilateral: 2 records, neither energy). Not ingested; manual
  lookup only.

## 11. Known limitations

**Coverage**
- **No complete register of non-binding agreements exists.** MOUs, joint statements and initiatives are
  missed by every source. An empty cell, a class 0 or an "opportunity" gap means *no agreement was
  found here*, not *no cooperation exists*; the dashboard says so wherever absence is shown.
- **The 2025-26 wave of critical-minerals and clean-industry deals is covered selectively.** The IISD
  Trade & Climate Tracker (211 agreements signed 2025-26) could not be ingested: its site sits behind a
  bot challenge and its content is all rights reserved. The adapter is ready for an export IISD shares.
  Until then, recent deals come from the curated seed (29 EU raw-materials, hydrogen and green
  partnerships; 37 US critical-minerals and trade instruments, all from official pages) and the US-Japan
  memo evidence. Recent deals of China, Japan, Korea, India, the Gulf states and others with third
  countries are largely missing.
- **GSP** runs 1992-2023 (most before 2021) and is uneven by publisher: 104 governments publish, Germany
  only 3 documents, the EU none. It has no partnership tier or content coding: tiers are read from
  titles and texts, sectors and instruments are keyword-coded (about 80% of body codes were genuine
  cooperation mentions in a 30-code check). Texts in languages other than English, Chinese, Japanese,
  Korean, French and Spanish are not coded; 51 documents have no text. Its "Treaty" type includes a few
  domestic acts.
- **DESTA** dates are years; content coding ends around 2023 and the published indices around 2020
  (213 records have no depth index; 67 are recomputed from provisions with the published rule). DESTA
  codes no supersession, so a signed agreement that never entered into force (TPP) stays "signed".
  No licence is published; only derived facts are redistributed, with the citation.
- **UNTS and ECOLEX are not ingested** (section 10): UNTS has no API and its terms forbid compiling or
  redistributing derivative works; ECOLEX prohibits automated harvesting and redistribution and is
  thin on bilateral energy and industrial cooperation. Binding bilateral trade agreements are covered
  by DESTA; binding bilateral energy, nuclear and investment treaties outside trade agreements are a
  known gap (UNTS would be the source, with permission).
- **Delivery evidence is rare.** Only curated rows and the memo evidence record projects, financing or
  operating bodies. "No delivery evidence recorded" means the delivery was not researched, not that the
  agreement is undelivered. The memo adapter counts a verified signed financing under a mechanism (the
  JBIC loans under the US-Japan Strategic Investment Initiative) as delivery evidence.
- **EU member states** cooperate mostly through EU law and programmes, which no source records as an
  agreement between members; intra-EU pairs look thin in the electro-industrial scope. EU-level
  agreements are attributed to the current 27 members (DESTA applies historical membership).
- The memo evidence (`data/reference/partnership_evidence/`, merged to main with the partnership
  memos on 2026-11-06; scripts/35 reads it by default) covers the US-Japan relationship only. The
  2026-11-06 build read the same files from the memo worktree, before that merge reached this branch.

**Method**
- **Depth classes are provisional rules.** In the all-agreements scope every free-trade or economic
  partnership agreement in force carries type-implied trade instruments, so any pair with one is at
  least class 4; the electro-industrial scope is the one that discriminates. Class 5 needs trade,
  finance or investment, sectoral cooperation and something in effect inside electro-industrial
  agreements, which the sources rarely show together.
- **Legal status** is set only from a source field, the source's own document type or official wording;
  most curated and memo rows are "unknown" because the text was not available or does not say.
- **Linkage is conservative.** Records link on party set, dates and title similarity, two trade
  agreements on one pair and year, or a trade leg of a small plurilateral; a weak match keeps both
  records. A joint statement announcing an agreement recorded elsewhere under another name and year
  stays a separate record.
- **The sector-to-technology map is coarse.** Critical minerals map to the upstream stages of five
  technologies; semiconductors, CCS, steel, aluminium, cement and data centres have no capability data
  in the dashboard and are not in the gap analysis. Keyword sectors are attributed to all of a
  document's parties.
- **Gaps inherit the capability data's blind spots**: complementarity uses the dashboard's need,
  competitive-position and recommended-partner measures and UN-vote alignment. Trade-based measures
  cannot see capability held abroad or sold domestically, and alignment is a coarse three-level bloc
  measure.
- **The legacy Strategic `ties` chunk** still carries the U.S. FTA pairing defect (section 1); it is
  fixed at source the next time scripts/85 runs, and the Agreements page does not read it.
- **Size.** Each economy's file repeats the agreements it is party to (about 40 MB raw, 2.4 MB gzipped
  across 206 files).
